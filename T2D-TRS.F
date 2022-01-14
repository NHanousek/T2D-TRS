!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     TRS_T2D - With Parallel basics...
!     Contains the variables and functions
!     that are used to define and run a tidal
!     range energy scheme within the BUSE subroutine
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     INFO
!     Head difference is Inside (aka upstream) - Outside (downstream)
!     Flow from in to out = positive? (Check with the previous works)
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     MODES:
!      N : Description,         Start condition,  End condition,
!      0 : Initial sluice mode, Time = 0,         Time = FLX_TIMES(1)
!      1 : High water holding,                    HD >= H_START
!      2 : Ebb generation,                        HD <= H_END
!      3 : Ebb sluicing,                          HD ~= 0.0
!     -1 : Ebb pumping,                           WL_IN <= PUMP_TARG
!      4 : Low water holding,                     HD >= H_START
!      5 : Flood generation,                      HD <= H_END
!      6 : Flood sluicing,                        HD ~= 0.0
!     -2 : FLood pumping,                         WL_IN <= PUMP_TARG
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Modal operation:
!     Suice flows:  Always match the sign of the head difference
!     Turbine flows: Always operate in assigned direction
!     Turbine power: +ve when generating, -ve when pumping
!     Ramp up during the start of generation/sluicing/pumping
!     Ramp down during start of holding from previous mode
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      MODULE TRS_T2D

      IMPLICIT NONE

!     MODULE CONTROL
      LOGICAL,SAVE :: FIRST = .TRUE.      !Is this the first call of the module
      LOGICAL,SAVE :: WRITE_TRS = .FALSE. !Print the TRS data to the file this timestep
      LOGICAL,SAVE :: G_PRNT_TRS = .TRUE. !true => print with graphical printout period
!      INTEGER,SAVE :: NF_TRS              !Unit code for tidal range scheme file... may be pointless
      INTEGER,SAVE :: NUM_TRS = 1         !How many independant tidal range schemes...
      REAL,SAVE :: TRS_TIME
                                          !false => print with 'listing period'

!     TEMPORAL CONTROLS - All in hours
!     One for each TRS
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: RAMPTIME      !How long to ramp between modes
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: PREV_SWITCH   !Time when lagoon last switched modes
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: MAX_GEN       !Maxmimum time before generation is auto ended
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: MAX_HOLD      !Maxmimum time before holding is auto ended
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: MAX_SLUICE    !Maxmimum time before sluicing is auto ended
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: MAX_PUMP      !Maxmimum time before pumping is auto ended
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: ORIG_DIAM_T   ! (m) Original diameter of Turbines used for the hill charts

!     OPERATION CONFIGURATION
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: IS2WAY           !Is the scheme in two way operation
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: ISFLEXIBLE       !Is the scheme in flexible operation
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: ISPUMPING        !Is the scheme using pumping
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: PUMPTOLEVEL      !Pump to level or head difference
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: ISPARALLELSLUICE !Is parallel sluicing being used
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: MODE             !What operational mode is the scheme in
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: MODE_PREV        !What mode was the scheme in prevously

!
!     WATER LEVELS - Meters
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: WL_IN,WL_OUT    !Water levels inside and outside, from control points
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: HEADDIFF        !Head difference = Inside - Outside
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: Q_TURB,Q_SLUICE !Flow rate in m3/s
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: POWER           !Power out(+ve)or in(-ve)


!     DIMENSIONLESS
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: RAMP           !Ramp Coefficient when changing modes

!     HILL CHART PARAMETERS - Note that shared head levels are required for powers/flows
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: N_HILLS_G,N_HILLS_P     !Number of hill charts for (_G)eneration and (_P)umping
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: LEN_HILLS_G,LEN_HILLS_P !Length of hillcharts for (_G)eneration and (_P)umping
      REAL,DIMENSION(:,:),ALLOCATABLE,SAVE :: HEAD_G,HEAD_P     !Head levels for the hill charts
      REAL,DIMENSION(:,:,:),ALLOCATABLE,SAVE :: POWER_G,FLOW_G  !Power/Flow for Generating
      REAL,DIMENSION(:,:,:),ALLOCATABLE,SAVE :: POWER_P,FLOW_P  !Power/Flow for Pumping


!     Arrays and values for flexible operation
!     Fixed operation is treated as flexible operation with one long period
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE  :: N_FLEX   !Number of flex periods
      REAL,DIMENSION(:,:),ALLOCATABLE,SAVE :: FLX_TIMES  !Start time of each flex period
      REAL,DIMENSION(:,:),ALLOCATABLE,SAVE :: H_STARTS,H_ENDS !Start and end piooints in those periods
      REAL,DIMENSION(:,:),ALLOCATABLE,SAVE :: PUMP_TARGS      !and the _CHARTS indicates the chart to use
      INTEGER,DIMENSION(:,:),ALLOCATABLE,SAVE :: TURB_CHARTS
      INTEGER,DIMENSION(:,:),ALLOCATABLE,SAVE :: PUMP_CHARTS
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: H_START, H_END          !Head to start/end generation
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: TURB_CHART, PUMP_CHART  !Which turbine/pump chart to use
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: PUMP_TARG               !Target to pump to

!     TEMPORARY VARIABLES
      REAL :: TMP_REAL = 0.0
      INTEGER :: SCHEME = 1   !Which scheme are we operating with right now.
      INTEGER :: TMP_INT = 0
      INTEGER :: i,j,k,l,m,status = 0
      LOGICAL :: TMP_BOOL = .FALSE.

      CONTAINS

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   TRS_INTERP
!   Simple linear interpolation, to be used in this module
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      REAL FUNCTION TRS_INTERP(X_NEW,X1,X2,Y1,Y2) RESULT(INTERP)
        IMPLICIT NONE
        REAL :: X_NEW,X1,X2,Y1,Y2
          INTERP = Y1 + (X_NEW - X1) * (Y2 - Y1) / (X2 - X1)
        RETURN
      END FUNCTION TRS_INTERP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!   Uses linear interpolation to find the value of the flow or power
!   at a given head difference.
!   Given the head levels (x's) and the values at those levels
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      REAL FUNCTION TRS_CALC(HEAD,HEADS,VALUES) RESULT(CALC)
        IMPLICIT NONE
        REAL :: HEAD
        REAL, DIMENSION(:) :: HEADS,VALUES
        INTEGER :: I,HSIZE,VSIZE,SIGN
        REAL :: HD

        HD = ABS(HEAD)
        HSIZE = SIZE(HEADS)
        VSIZE = SIZE(VALUES)

        IF (HEAD.LE.0.0) THEN
          SIGN = -1
        ELSE
          SIGN = 1
        ENDIF

        IF (HSIZE.EQ.VSIZE) THEN
          IF (HD.LE.HEADS(10)) THEN
            CALC = SIGN*VALUES(1)
            RETURN
          ELSEIF (HD.GE.HEADS(HSIZE)) THEN
            CALC = SIGN*VALUES(VSIZE)
            RETURN
          ELSE
            DO I = 2,VSIZE
              IF ((HD.GE.HEADS(I-1))
     &          .AND.(HD.LE.HEADS(I))) THEN
                CALC = SIGN*TRS_INTERP(HD,HEADS(I-1),
     &                  HEADS(I),VALUES(I-1),VALUES(I))
                RETURN
              ENDIF
            ENDDO
          ENDIF
        ELSE
          WRITE(*,*)'UNEQUAL ARRAYS USED IN TRS_CALC FUNCTION'
          CALC = 0.0
        ENDIF
        RETURN
      END FUNCTION TRS_CALC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   TRS_RAMP
!   Applies a sinusoidal ramp function from the phase and ramp times
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      REAL FUNCTION TRS_RAMP(RAMPTIME,PHASETIME) RESULT(RMP)
        IMPLICIT NONE
        REAL :: RAMPTIME,PHASETIME
        IF (PHASETIME.GE.RAMPTIME) THEN
          RMP = 1
        ELSE
          RMP = (1 - COS(3.1415926*PHASETIME/RAMPTIME))/2
        ENDIF
        RETURN
      END FUNCTION TRS_RAMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   Orifice equation - returned with direction
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      REAL FUNCTION TRS_ORIFICE(CD,AREA,HDIFF) RESULT(FLOW)
        REAL :: CD,AREA,HDIFF
        IF (HDIFF.GT.0.0) THEN
          FLOW = CD*AREA*SQRT(2*9.807*HDIFF)
        ELSE
          FLOW = CD*AREA*SQRT(2*9.807*ABS(HDIFF))
        ENDIF
        RETURN
      END FUNCTION TRS_ORIFICE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   'Borrowed' function to convert character strings to upper case.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      function to_upper(strIn) result(strOut)
! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
! Original author: Clive Page

      implicit none

      character(len=*), intent(in) :: strIn
      character(len=len(strIn)) :: strOut
      integer :: i,j

      do i = 1, len(strIn)
      j = iachar(strIn(i:i))
      if (j>= iachar("a") .and. j<=iachar("z") ) then
          strOut(i:i) = achar(iachar(strIn(i:i))-32)
      else
          strOut(i:i) = strIn(i:i)
      end if
      end do

      end function to_upper
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   TRS_STATUS
!   Print an error message if the stat variable is showing a bad status
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_STAT(STAT,MSG)
        INTEGER,INTENT(IN) :: STAT
        CHARACTER(LEN=*),INTENT(IN) :: MSG
        IF (STAT.NE.0) THEN
          WRITE(*,*) 'STATUS = ',STAT
          WRITE(*,*) 'MEMORY ALLOCATION ERROR [',MSG,']'
        ENDIF
      END SUBROUTINE TRS_STAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   TRS_FLEXVALUES
!   Sets the control parameters based on the time and the flex control
!   definition
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_FLEX_VALUES(CT)
      REAL,INTENT(IN) :: CT
      INTEGER :: I,TRS

      DO TRS =1,NUM_TRS
      IF (CT.LT.FLX_TIMES(TRS,1)) THEN
        !WARMUP PERIOD
        H_START(TRS) = 1.0E16 !Do not enter generation
        H_END(TRS) = 0.0
        TURB_CHART(TRS) = 1
        PUMP_TARG(TRS) = 0.0
        PUMP_CHART(TRS) = 1
        RETURN
      ELSEIF (CT.GE.FLX_TIMES(TRS,N_FLEX(TRS))) THEN
        H_START(TRS) = H_STARTS(TRS,N_FLEX(TRS))
        H_END(TRS) = H_ENDS(TRS,N_FLEX(TRS))
        TURB_CHART(TRS) = TURB_CHARTS(TRS,N_FLEX(TRS))
        PUMP_TARG(TRS) = PUMP_TARGS(TRS,N_FLEX(TRS))
        PUMP_CHART(TRS) = PUMP_CHARTS(TRS,N_FLEX(TRS))
        RETURN
      ELSE
        DO I = 2,N_FLEX(TRS)
          IF ((CT.GE.FLX_TIMES(TRS,I-1))
     &      .AND.(CT.LT.FLX_TIMES(TRS,I))) THEN
            H_START(TRS) = H_STARTS(TRS,I-1)
            H_END(TRS) = H_ENDS(TRS,I-1)
            TURB_CHART(TRS) = TURB_CHARTS(TRS,I-1)
            PUMP_TARG(TRS) = PUMP_TARGS(TRS,I-1)
            PUMP_CHART(TRS) = PUMP_CHARTS(TRS,I-1)
            RETURN
          ENDIF
        ENDDO
      ENDIF
      ENDDO
      END SUBROUTINE TRS_FLEX_VALUES

      SUBROUTINE TRS_NEW_MODE(CT)
      REAL,INTENT(IN) :: CT!, TRS_RAMP
      REAL :: PHASE_T, HD
      INTEGER :: TRS

      DO TRS = 1,NUM_TRS
      HD = ABS(HEADDIFF(TRS))
      PHASE_T = CT - PREV_SWITCH(TRS) ! Initialise with prior phase time

      IF (PHASE_T.GE.RAMPTIME(TRS)) THEN !If we have completed starting operation of this phase
        SELECT CASE(MODE(TRS))
          CASE (0)  !      0 : Initial sluicing warmup
            IF (CT.GE.FLX_TIMES(TRS,1)) THEN
              MODE(TRS) = 1  !      1 : High water holding
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (1)  !      1 : High water holding
            IF ((HD.GE.H_START(TRS))
     &      .OR.(PHASE_T.GT.MAX_HOLD(TRS))) THEN
              MODE(TRS) = 2  !      2 : Ebb generation
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (2)  !      2 : Ebb generation
            IF ((HD.LE.H_END(TRS))
     &      .OR.(PHASE_T.GT.MAX_GEN(TRS))) THEN
              IF (IS2WAY(TRS)) THEN
                MODE(TRS) = 3  !      3 : Ebb sluicing
              ELSE
                MODE(TRS) = 4  !      4 : Low water holding
              ENDIF
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (3)  !      3 : Ebb sluicing
            IF ((HD.LE.0.1)
     &      .OR.(PHASE_T.GT.MAX_SLUICE(TRS))) THEN
              IF (ISPUMPING(TRS).AND.IS2WAY(TRS)) THEN
                MODE(TRS) = -1 !     -1 : Ebb pumping
              ELSE
                MODE(TRS) = 4  !      4 : Low water holding
              ENDIF
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (-1) !     -1 : Ebb pumping
            IF ((PUMPTOLEVEL(TRS)
     &          .AND.(WL_IN(TRS).LE.PUMP_TARG(TRS)))
     &      .OR.(.NOT.(PUMPTOLEVEL(TRS))
     &          .AND.(HD.GE.PUMP_TARG(TRS)))
     &      .OR.(PHASE_T.GE.MAX_PUMP(TRS))) THEN
              MODE(TRS) = 4  !      4 : Low water holding
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (4)  !      4 : Low water holding
            IF (IS2WAY(TRS)) THEN
              IF ((HD.GE.H_START(TRS))
     &      .OR.(PHASE_T.GT.MAX_HOLD(TRS))) THEN
                MODE(TRS) = 5 !      5 : Flood generation
                PREV_SWITCH(TRS) = CT
              ENDIF
            ELSE !      5 : Flood generation
              IF((HEADDIFF(TRS).GT.0.1)
     &      .OR.(PHASE_T.GT.MAX_HOLD(TRS))) THEN
                MODE(TRS)= 6  !      6 : Flood sluicing
                PREV_SWITCH(TRS) = CT
              ENDIF
            ENDIF
          CASE (5)  !      5 : Flood generation
            IF ((HD.LE.H_END(TRS))
     &      .OR.(PHASE_T.GT.MAX_HOLD(TRS))) THEN
              MODE(TRS) = 2  !      2 : Ebb generation
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (6)  !      6 : Flood sluicing
            IF ((HD.LE.0.1)
     &      .OR.(PHASE_T.GT.MAX_SLUICE(TRS))) THEN
              IF (ISPUMPING(TRS)) THEN
                MODE(TRS) = -2 !     -2 : FLood pumping
              ELSE
                MODE(TRS) = 1  !      1 : High water holding
              ENDIF
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (-2) !     -2 : FLood pumping
          IF ((PUMPTOLEVEL(TRS).AND.(WL_IN(TRS).GE.PUMP_TARG(TRS)))
     &    .OR.(.NOT.(PUMPTOLEVEL(TRS)).AND.(HD.GE.PUMP_TARG(TRS)))
     &    .OR.(PHASE_T.GE.MAX_PUMP(TRS))) THEN
              MODE(TRS) = 1  !      1 : High water holding
              PREV_SWITCH(TRS) = CT
            ENDIF
        END SELECT
!       UPDATE PHASE TIMING?
!       PHASE_T = CT - PREV_SWITCH(TRS)
      ENDIF
      RAMP(TRS) = TRS_RAMP(RAMPTIME(TRS),PHASE_T)
      ENDDO
      RETURN
      END SUBROUTINE TRS_NEW_MODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Read the control data file
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      ! RUDIMENTARY INPUT KEY VARIABLE PARSER.
      SUBROUTINE TRS_PARSE(LN,KWRD,VRBL)
      CHARACTER(LEN=*),INTENT(IN) :: LN
      CHARACTER(LEN=100),INTENT(OUT) :: KWRD,VRBL
      INTEGER :: K,LENLN
      ! Default values, for cases where only the keyword is to be passed etc.
      KWRD = TRIM(ADJUSTL(LN))
      VRBL = 'NaN'

      LENLN = LEN(LN)
      IF ((INDEX(LN,'=').GT.1).OR.(INDEX(LN,':').GT.1)) THEN
        K = MAX(INDEX(LN,'='),INDEX(LN,':'))
        KWRD = TRIM(LN(1:k-1))
        VRBL = TRIM(LN(k+1:LENLN))
      ELSE
        DO K = 2,LENLN-1
          IF ((LN(K:K).EQ.' ').AND.(LN(K+1:K+1).NE.' ')) THEN
            KWRD = TRIM(LN(1:k))
            VRBL = TRIM(LN(K+1:LENLN))
            EXIT
          ENDIF
        END DO
      ENDIF
      END SUBROUTINE TRS_PARSE

      SUBROUTINE TRS_KV_WARN(K,V)
      CHARACTER(LEN=*),INTENT(IN) :: K,V
        WRITE(*,*) '+-+-+-+-+-+-+-WARNING-+-+-+-+-+-+-+-+-+-+-+'
        WRITE(*,*) 'Unextpected key - variable pair:'
        WRITE(*,*) 'Key: ',TRIM(K)
        WRITE(*,*) 'Variable: ',TRIM(V)
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
      END SUBROUTINE TRS_KV_WARN

!     READ A Hill Chart from the file point
      SUBROUTINE TRS_LOAD_HILLCHART(HEADS,ARRAY,NF_TRS)
      REAL,DIMENSION(:),INTENT(INOUT) :: HEADS    ! Head difference values
      REAL,DIMENSION(:,:),INTENT(INOUT) :: ARRAY  ! Array of variable
      CHARACTER(LEN=100) :: LINE
      INTEGER,INTENT(IN) :: NF_TRS             ! file key
      INTEGER :: SZA(2),SZH(1)    ! shapes of arrays
      INTEGER :: ROW,COL            ! Iterators
      SZA = SHAPE(ARRAY)
      SZH = SHAPE(HEADS)

      IF (SZA(1).NE.SZH(1)) THEN
        WRITE(*,*) 'ATTEMPTING TO READ MISMATCHED HEAD-POWER/FLOW'
      ELSE
        DO ROW = 1,SZA(1)
          READ(NF_TRS,'(A100)') LINE
          READ(LINE(1:INDEX(LINE,' ')),*) HEADS(ROW)
          READ(LINE(INDEX(LINE,' '):LEN(LINE)),*)
     &        (ARRAY(ROW,COL),COL=1,SZA(2))
        ENDDO
      ENDIF
      END SUBROUTINE TRS_LOAD_HILLCHART

!     Function to print out the hill charts to the user as a simple check
      SUBROUTINE TRS_PRINT_HILLCHART(HEADS,ARRAY,KWRD,VRBL)
      REAL,DIMENSION(:),INTENT(IN) :: HEADS    ! Head difference values
      REAL,DIMENSION(:,:),INTENT(IN) :: ARRAY  ! Array of variable
      INTEGER,DIMENSION(2) :: SZ
      INTEGER :: ROW,COL
      CHARACTER(LEN=*),INTENT(IN) :: KWRD,VRBL
      LOGICAL :: TITLEROW
      SZ = SHAPE(ARRAY)
      TITLEROW = .TRUE.
      WRITE(*,*) 'HILLCHART: ',KWRD,' - ',VRBL
      DO ROW = 1,SZ(1)
        IF (TITLEROW) THEN
          WRITE(*,'(2A10)',ADVANCE='NO') 'N','HEAD(m)'
          DO COL = 1,SZ(2)
            WRITE(*,'(A8,A1,I2,A1)',ADVANCE='NO') VRBL,'(',COL,') '
          ENDDO
          WRITE(*,*) ' '
        ENDIF

        WRITE(*,'(I10,A1,F9.3,A1)',ADVANCE='NO')ROW,' ',HEADS(ROW),' '
        DO COL = 1,SZ(2)
            WRITE(*,'(F9.3,A1)',ADVANCE='NO') ARRAY(ROW,COL),' '
        ENDDO
        WRITE(*,*) ' '

        TITLEROW = .FALSE.
      ENDDO
      END SUBROUTINE TRS_PRINT_HILLCHART

      SUBROUTINE TRS_READ_DATAFILES(NF_TRS)
      !File with the tidal range scheme control parameters
!      NFO1=T2D_FILES(T2DFO1)%LU
      INTEGER,INTENT(IN) :: NF_TRS
      INTEGER :: TRS
      CHARACTER(LEN=100) :: LINE,KEYWORD,VARIABLE
      CHARACTER(LEN=9) :: TRS_FNAME
      LOGICAL :: ISOPEN = .FALSE.,PRNTKV

      IF (.NOT.ALLOCATED(WL_IN)) THEN
        ALLOCATE(WL_IN(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'WL_IN')
      ENDIF
      IF (.NOT.ALLOCATED(WL_OUT)) THEN
        ALLOCATE(WL_OUT(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'WL_OUT')
      ENDIF
      IF (.NOT.ALLOCATED(HEADDIFF)) THEN
        ALLOCATE(HEADDIFF(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'HEADDIFF')
      ENDIF
      IF (.NOT.ALLOCATED(RAMP)) THEN
        ALLOCATE(RAMP(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'RAMP')
      ENDIF
      IF (.NOT.ALLOCATED(H_START)) THEN
        ALLOCATE(H_START(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'H_START')
      ENDIF
      IF (.NOT.ALLOCATED(H_END)) THEN
        ALLOCATE(H_END(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'H_END')
      ENDIF
      IF (.NOT.ALLOCATED(TURB_CHART)) THEN
        ALLOCATE(TURB_CHART(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'TURB_CHART')
      ENDIF
      IF (.NOT.ALLOCATED(PUMP_CHART)) THEN
        ALLOCATE(PUMP_CHART(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PUMP_CHART')
      ENDIF
      IF (.NOT.ALLOCATED(PUMP_TARG)) THEN
        ALLOCATE(PUMP_TARG(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PUMP_TARG')
      ENDIF
      IF (.NOT.ALLOCATED(Q_TURB)) THEN
        ALLOCATE(Q_TURB(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'Q_TURB')
      ENDIF
      IF (.NOT.ALLOCATED(Q_SLUICE)) THEN
        ALLOCATE(Q_SLUICE(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'Q_SLUICE')
      ENDIF
      IF (.NOT.ALLOCATED(POWER)) THEN
        ALLOCATE(POWER(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'POWER')
      ENDIF
      TRS_TIME = 0.0

      DO TRS = 1,NUM_TRS
!        WRITE(TRS_FNAME,'(A7,I1,A4)') '../TRS-',TRS,'.txt'
        TRS_FNAME = 'TRS-1.txt'
        OPEN(UNIT=NF_TRS,FILE=TRS_FNAME,STATUS='OLD')
        INQUIRE(UNIT=NF_TRS,OPENED=ISOPEN)

      IF (ISOPEN) THEN
        REWIND(NF_TRS)
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
        WRITE(*,*) '              File [',TRS_FNAME,'] Opened'
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
      IF (FIRST) THEN
        READ(NF_TRS,*) LINE !Header line
        WRITE(*,*) LINE
        READ(NF_TRS,'(A100)') LINE
        DO WHILE((LINE(1:3).NE.'END').AND.
     &           (LINE(1:3).NE.'End').AND.
     &           (LINE(1:3).NE.'end'))
!          WRITE(*,*) LINE
          CALL TRS_PARSE(LINE,KEYWORD,VARIABLE)

          SELECT CASE(TO_UPPER(TRIM(ADJUSTL(KEYWORD))))  !Quite rough'n'ready...
!           SIMPLE KEY -> VARIABLE PAIRS
            CASE('RAMPTIME')  ! Hours
            IF(.NOT.ALLOCATED(RAMPTIME)) THEN
              ALLOCATE(RAMPTIME(NUM_TRS))
              RAMPTIME(:) = 0.25 !Default will be 15 minutes
            ENDIF
              READ(VARIABLE,*) RAMPTIME(TRS)

            CASE('PREV_SWITCH') ! Hours in model
            IF(.NOT.ALLOCATED(PREV_SWITCH)) THEN
              ALLOCATE(PREV_SWITCH(NUM_TRS))
              PREV_SWITCH(:) = 0.0 ! Default will be a fresh initialisation
              !Only needs modifiaction for alternate warmup functions
            ENDIF
              READ(VARIABLE,*) PREV_SWITCH(TRS)

            CASE('MAX_GEN') ! Hours
            IF(.NOT.ALLOCATED(MAX_GEN)) THEN
              ALLOCATE(MAX_GEN(NUM_TRS))
              MAX_GEN(:) = 12.0 ! Default value
            ENDIF
              READ(VARIABLE,*) MAX_GEN(TRS)

            CASE('MAX_HOLD') !HOURS
            IF(.NOT.ALLOCATED(MAX_HOLD)) THEN
              ALLOCATE(MAX_HOLD(NUM_TRS))
              MAX_HOLD(:) = 12.0 ! Default value
            ENDIF
              READ(VARIABLE,*) MAX_HOLD(TRS)

            CASE('MAX_SLUICE') ! Hours
            IF(.NOT.ALLOCATED(MAX_SLUICE)) THEN
              ALLOCATE(MAX_SLUICE(NUM_TRS))
              MAX_SLUICE(:) = 12.0 ! Default value
            ENDIF
              READ(VARIABLE,*) MAX_SLUICE(TRS)

            CASE('MAX_PUMP') ! Hours
            IF(.NOT.ALLOCATED(MAX_PUMP)) THEN
              ALLOCATE(MAX_PUMP(NUM_TRS))
              MAX_PUMP(:) = 12.0 ! Default value
            ENDIF
              READ(VARIABLE,*) MAX_PUMP(TRS)

            CASE('IS2WAY') ! Logical (1 or 0, I think)
            IF(.NOT.ALLOCATED(IS2WAY)) THEN
              ALLOCATE(IS2WAY(NUM_TRS))
              IS2WAY(:) = .TRUE. ! Default value
            ENDIF
              READ(VARIABLE,*) IS2WAY(TRS)

            CASE('ISFLEXIBLE') ! Logical (1 or 0, I think)
            IF(.NOT.ALLOCATED(ISFLEXIBLE)) THEN
              ALLOCATE(ISFLEXIBLE(NUM_TRS))
              ISFLEXIBLE(:) = .FALSE.
            ENDIF
              READ(VARIABLE,*) ISFLEXIBLE(TRS)

            CASE('ISPUMPING') ! Logical (1 or 0, I think)
            IF(.NOT.ALLOCATED(ISPUMPING)) THEN
              ALLOCATE(ISPUMPING(NUM_TRS))
              ISPUMPING(:) = .FALSE.
            ENDIF
              READ(VARIABLE,*) ISPUMPING(TRS)

            CASE('ISPARALLELSLUICE') ! Logical (1 or 0, I think)
            IF(.NOT.ALLOCATED(ISPARALLELSLUICE)) THEN
              ALLOCATE(ISPARALLELSLUICE(NUM_TRS))
              ISPARALLELSLUICE(:) = .FALSE.
            ENDIF
              READ(VARIABLE,*) ISPARALLELSLUICE(TRS)

            CASE('MODE')  ! Integer mode code
            IF(.NOT.ALLOCATED(MODE)) THEN
              ALLOCATE(MODE(NUM_TRS))
              MODE(:) = 0 !Only modify default in test or continued operation
            ENDIF
              READ(VARIABLE,*) MODE(TRS)

            CASE('N_HILLS_G')  ! Integer
            IF(.NOT.ALLOCATED(N_HILLS_G)) THEN
              ALLOCATE(N_HILLS_G(NUM_TRS))
              N_HILLS_G(:) = 1
            ENDIF
              READ(VARIABLE,*) N_HILLS_G(TRS)

            CASE('N_HILLS_P')  ! Integer
            IF(.NOT.ALLOCATED(N_HILLS_P)) THEN
              ALLOCATE(N_HILLS_P(NUM_TRS))
              N_HILLS_P(:) = 1
            ENDIF
              READ(VARIABLE,*) N_HILLS_P(TRS)

            CASE('LEN_HILLS_G')  ! Integer
            IF(.NOT.ALLOCATED(LEN_HILLS_G)) THEN
              ALLOCATE(LEN_HILLS_G(NUM_TRS))
              LEN_HILLS_G(:) = 1
            ENDIF
              READ(VARIABLE,*) LEN_HILLS_G(TRS)

            CASE('LEN_HILLS_P')  ! Integer
            IF(.NOT.ALLOCATED(LEN_HILLS_P)) THEN
              ALLOCATE(LEN_HILLS_P(NUM_TRS))
              LEN_HILLS_P(:) = 1
            ENDIF
              READ(VARIABLE,*) LEN_HILLS_P(TRS)

            CASE('N_FLEX')  ! Integer
            IF(.NOT.ALLOCATED(N_FLEX)) THEN
              ALLOCATE(N_FLEX(NUM_TRS))
              N_FLEX(:) = 1 !Effectively fixed operation
            ENDIF
              READ(VARIABLE,*) N_FLEX(TRS) ! Allocate the flex arrays here, as they only need this?
              IF (.NOT.ALLOCATED(FLX_TIMES)) THEN
                ALLOCATE(FLX_TIMES(TRS,N_FLEX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'FLX_TIMES')
                FLX_TIMES(:,:) = 0.0

                ALLOCATE(H_STARTS(TRS,N_FLEX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'H_STARTS')
                H_STARTS(:,:) = 0.0

                ALLOCATE(H_ENDS(TRS,N_FLEX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'H_ENDS')
                H_ENDS(:,:) = 0.0

                ALLOCATE(TURB_CHARTS(TRS,N_FLEX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'TURB_CHARTS')
                TURB_CHARTS(:,:) = 0

                ALLOCATE(PUMP_TARGS(TRS,N_FLEX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'PUMP_TARGS')
                PUMP_TARGS(:,:) = 0.0

                ALLOCATE(PUMP_CHARTS(TRS,N_FLEX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'PUMP_CHARTS')
                PUMP_CHARTS(:,:) = 0

              ENDIF
            CASE('PUMPTOLEVEL')  ! Integer
            IF(.NOT.ALLOCATED(PUMPTOLEVEL)) THEN
              ALLOCATE(PUMPTOLEVEL(NUM_TRS))
              PUMPTOLEVEL(:) = 0
            ENDIF
              READ(VARIABLE,*) PUMPTOLEVEL(TRS)

            CASE('MODE_PREV') ! Integer (Don't think it does anything)
            IF(.NOT.ALLOCATED(MODE_PREV)) THEN
              ALLOCATE(MODE_PREV(NUM_TRS))
              MODE_PREV(:) = 0
            ENDIF
              READ(VARIABLE,*) MODE_PREV(TRS)

            CASE('ORIG_DIAM_T') !Diameter(meter) of original turbine used in hill charts
            IF(.NOT.ALLOCATED(ORIG_DIAM_T)) THEN
              ALLOCATE(ORIG_DIAM_T(NUM_TRS))
              ORIIG_DIAM_T(:) = 0.0
            ENDIF
              READ(VARIABLE,*) ORIG_DIAM_T(TRS)

!           MORE COMPLEX ARRAY LOADERS
            CASE('GENERATOR')
              PRNTKV = .FALSE.
              !WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)
              IF (.NOT.ALLOCATED(HEAD_G)) THEN
                ALLOCATE(HEAD_G(NUM_TRS,LEN_HILLS_G(TRS)),STAT=STATUS)
              ENDIF
              CALL TRS_STAT(STATUS,'HILL CHART: Generator heads')
              ! Power indicator words will contain the letter p
              IF ((INDEX(VARIABLE,'P').NE.0).OR.
     &             INDEX(VARIABLE,'p').NE.0) THEN
              IF (.NOT.ALLOCATED(POWER_G)) THEN
                ALLOCATE(POWER_G(NUM_TRS,LEN_HILLS_G(TRS),
     &            N_HILLS_G(TRS)),STAT=STATUS)
              ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Generator Powers')
                !There may be a better way to do this to be honest...
                CALL TRS_LOAD_HILLCHART(HEAD_G(TRS,:),
     &            POWER_G(TRS,:,:),NF_TRS)
                CALL TRS_PRINT_HILLCHART(HEAD_G(TRS,:),
     &            POWER_G(TRS,:,:),'GENERATOR','POWER')

              ! Flow will have an F
              ELSEIF ((INDEX(VARIABLE,'F').NE.0).OR.
     &             INDEX(VARIABLE,'f').NE.0) THEN
                IF (.NOT.ALLOCATED(FLOW_G)) THEN
                  ALLOCATE(FLOW_G(NUM_TRS,LEN_HILLS_G(TRS),
     &              N_HILLS_G(TRS)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Generator Flow')
                CALL TRS_LOAD_HILLCHART(HEAD_G(TRS,:),
     &            FLOW_G(TRS,:,:),NF_TRS)
                CALL TRS_PRINT_HILLCHART(HEAD_G(TRS,:),
     &            FLOW_G(TRS,:,:),'GENERATOR','FLOW')
              ELSE !Error condition of some kind
                CALL TRS_KV_WARN(KEYWORD,VARIABLE)
              ENDIF

            CASE('PUMP')
              PRNTKV = .FALSE.
              !WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)
              IF (.NOT.ALLOCATED(HEAD_P)) THEN
                ALLOCATE(HEAD_P(TRS,LEN_HILLS_P(TRS)),STAT=STATUS)
              ENDIF
              CALL TRS_STAT(STATUS,'HILL CHART: Pump heads')
              ! Power indicator words will contain the letter p
              IF ((INDEX(VARIABLE,'P').NE.0).OR.
     &             INDEX(VARIABLE,'p').NE.0) THEN
                IF (.NOT.ALLOCATED(POWER_P)) THEN
                  ALLOCATE(POWER_P(TRS,LEN_HILLS_P(TRS),
     &              N_HILLS_P(TRS)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Pump Powers')
                CALL TRS_LOAD_HILLCHART(HEAD_P(TRS,:),
     &            POWER_P(TRS,:,:),NF_TRS)
                CALL TRS_PRINT_HILLCHART(HEAD_P(TRS,:),
     &            POWER_P(TRS,:,:),'PUMP','POWER')
              ! Flow will have an F
              ELSEIF ((INDEX(VARIABLE,'F').NE.0).OR.
     &             INDEX(VARIABLE,'f').NE.0) THEN
                IF (.NOT.ALLOCATED(FLOW_P)) THEN
                  ALLOCATE(FLOW_P(TRS,LEN_HILLS_P(TRS),
     &              N_HILLS_P(TRS)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Pump Flow')
                CALL TRS_LOAD_HILLCHART(HEAD_P(TRS,:),
     &            FLOW_P(TRS,:,:),NF_TRS)
                CALL TRS_PRINT_HILLCHART(HEAD_P(TRS,:),
     &            FLOW_P(TRS,:,:),'PUMP','FLOW')
              ELSE !Error condition of some kind
                CALL TRS_KV_WARN(KEYWORD,VARIABLE)
              ENDIF

            CASE('FLEX')
              PRNTKV = .FALSE.
              !WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)
!              ALLOCATE(ARRAY(LENGTH))
!FLEX CONTOL (START_TIME,H_START,H_END,TURBINE_CHART,PUMP_TARGET,PUMP_CHART)
              WRITE(*,*)  'FLEX, T_START, H_START, H_END, T_CHART,',
     &                   ' P_TARG, P_CHART'
              DO K = 1,N_FLEX(TRS)
                READ(NF_TRS,*) FLX_TIMES(TRS,K),
     &            H_STARTS(TRS,K),
     &            H_ENDS(TRS,K),
     &            TURB_CHARTS(TRS,K),
     &            PUMP_TARGS(TRS,K),
     &            PUMP_CHARTS(TRS,K)
                WRITE(*,*) K,
     &            FLX_TIMES(TRS,K),
     &            H_STARTS(TRS,K),
     &            H_ENDS(TRS,K),
     &            TURB_CHARTS(TRS,K),
     &            PUMP_TARGS(TRS,K),
     &            PUMP_CHARTS(TRS,K)
              ENDDO

            CASE DEFAULT ! Oh no, something must have gone wrong
            IF (KEYWORD(1:1).NE.'!') THEN
              CALL TRS_KV_WARN(KEYWORD,VARIABLE)
            ENDIF
          END SELECT

          IF ((KEYWORD(1:1).NE.'!').AND.(PRNTKV)) THEN
            WRITE(*,*) TRIM(ADJUSTL(KEYWORD)),' = ',TRIM(VARIABLE)
          ENDIF
          PRNTKV = .TRUE.
          READ(NF_TRS,'(A100)',END=702) LINE
!         END error checker stops it from reading to infitity?

        END DO

702     CONTINUE

        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
        WRITE(*,*) 'Done reading [',TRS_FNAME,'] Control File.'
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
        CLOSE(NF_TRS)

        ELSE
          WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
          WRITE(*,*) 'UNABLE TO OPEN [',TRS_FNAME,'] CONTROL FILE.'
          WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
        ENDIF
      ENDIF
      END DO
      FIRST = .FALSE.
      END SUBROUTINE TRS_READ_DATAFILES
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Write the headers and initial row of the output CSV
!     Then write the updated data to the respective files.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_WRITE_RESULTS(OF_TRS)
      INTEGER,INTENT(IN) :: OF_TRS
      INTEGER :: TRS,STAT
      CHARACTER(LEN=20) :: TRS_FNAME
      CHARACTER :: A = ','
      LOGICAL,SAVE :: RESULTS_FIRST = .TRUE.

      IF (RESULTS_FIRST) THEN
        TRS_TIME = 0.0
        DO TRS = 1,NUM_TRS
          WRITE(TRS_FNAME,'(A7,I1,A12)')'../TRS-',TRS,'-Results.csv'
          OPEN(OF_TRS,FILE=TRS_FNAME,STATUS='REPLACE',IOSTAT=STAT)
          WRITE(OF_TRS,*)'TIME(hr),Z_OUT(mD),Z_IN(mD),HEADDIFF(m),',
     &      'MODE(-),Q_TURB(m3_s),Q_SLUICE(m3_s),POWER(MW),',
     &      'T_CHART(-),P_CHART(-),H_START(m),H_END(m),P_TARG(-)'
          CLOSE(OF_TRS)
        ENDDO
        RESULTS_FIRST = .FALSE.
      ENDIF
      DO TRS = 1,NUM_TRS
        WRITE(TRS_FNAME,'(A7,I1,A12)')'../TRS-',TRS,'-Results.csv'
        OPEN(OF_TRS,FILE=TRS_FNAME,STATUS='OLD',
     &         ACCESS='APPEND',IOSTAT=STAT)
        WRITE(OF_TRS,*)TRS_TIME,A,
     &    WL_OUT(TRS),A,WL_IN(TRS),A,HEADDIFF(TRS),A,MODE(TRS),A,
     &    Q_TURB(TRS),A,Q_SLUICE(TRS),A,POWER(TRS),A,TURB_CHART(TRS),A,
     &    PUMP_CHART(TRS),A,H_START(TRS),A,H_END(TRS),A,PUMP_TARG(TRS)
        CLOSE(OF_TRS)
      ENDDO
      END SUBROUTINE TRS_WRITE_RESULTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Write the next row of the output CSV
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_WRITE_STATUS()
      INTEGER :: TRS

      DO TRS = 1,NUM_TRS
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
      WRITE(*,*) 'TRS:          ',TRS
      WRITE(*,*) 'TIME(HRS):    ',TRS_TIME,
     &'MODE(-):  ',MODE(TRS)
      WRITE(*,*) 'WL(mD) OUT:   ',WL_OUT(TRS),
     &'IN:       ',WL_IN(TRS)
      WRITE(*,*) 'Q(m3/s) TURB: ',Q_TURB(TRS),
     &'SLUICE:   ',Q_SLUICE(TRS)
      WRITE(*,*) 'POWER(MW):    ',POWER(TRS),
     &'PTARG(-): ',PUMP_TARG(TRS)
      WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
      ENDDO

      END SUBROUTINE TRS_WRITE_STATUS

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     MODES:
!      N : Description,         Start condition,  End condition,

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      SUBROUTINE TRS_FLOWS()
      INTEGER :: TRS,CULV
      !Calculate the base flows, and then scale, ramp, and apply them.
      REAL,DIMENSION(NUM_TRS),ALLOCATABLE :: QG_BASE,QP_BASE
      REAL :: T_AREA,S_AREA,T_SF,PHASE_TIME
      ! Sluice flows calculated with orifice equation.
      DO TRS = 1,NUM_TRS
        QG_BASE(TRS) = TRS_CALC(
     &    HEADDIFF(TRS),
     &    HEAD_G(TRS,:),
     &    FLOW_G(TRS,TURB_CHART(TRS),:))
        QP_BASE(TRS) = TRS_CALC(
     &    HEADDIFF(TRS),
     &    HEAD_P(TRS,:),
     &    FLOW_P(TRS,PUMP_CHART(TRS),:))
      END DO
      !This is the trickiest sub so far... probably need to have the double
      !select case to do all the types of culvert and modes individually...

      DO CULV = 1,NBUSE
        TRS = CTRASH(CULV)
        PHASE_TIME = TRS_TIME - PREV_SWITCH(TRS)

        SELECT (CLPBUS(CULV))
          CASE(4) ! Control point
            DBUS(CULV) = 0.0

          CASE(5) ! Turbine
            T_AREA = 0.25*PI*LRG(CULV)*LRG(CULV)        ! Turbine area (m2)
            T_SF = (LRG(CULV)**2)/(ORIG_DIAM_T(TRS)**2) ! Turbine scale factor
            ! Ramp * Scale Factor * Flow
            SELECT (MODE(TRS))
              CASE (0)  ! Initial warmup
              !REAL FUNCTION TRS_ORIFICE(CD,AREA,HDIFF) RESULT(FLOW)
                DBUS(CULV) = RAMP(TRS)*
     &            ORIFICE(CE1(CULV),T_AREA,HEADDIFF(TRS))

              CASE (1)  ! High water hold
                ! Ramp down turbine flow if appropriate
                IF (PHASE_TIME).LT.RAMPTIME(TRS)) THEN
                  IF (ISPUMPING(TRS)) THEN
                    DBUS(CULV) = (1.0 - RAMP(TRS))*QP_BASE*T_SF
                  ELSE
                    IF(ISPARALLELSLUICE(TRS)) THEN
                      DBUS(CULV) = (1.0 - RAMP(TRS))*QG_BASE*T_SF
                    ELSE
                      DBUS(CULV) = 0.0
                    ENDIF
                  ENDIF
                ELSE
                  DBUS(CULV) = 0.0
                ENDIF

              CASE (2)  ! Ebb generation
                DBUS(CULV) = RAMP(TRS)*QG_BASE*T_SF

              CASE (3)  ! Ebb sluicing
                IF (ISPARALLELSLUICE(TRS)) THEN
                  DBUS(CULV) = RAMP(TRS)*QG_BASE*T_SF
                ELSE
                  DBUS(CULV) = 0.0
                ENDIF

              CASE (-1) ! Ebb pumping
                DBUS(CULV) = RAMP(TRS)*QP_BASE*T_SF
              CASE (4)  ! Low water hold
              ! Ramp down turbine flow if appropriate
              IF (PHASE_TIME).LT.RAMPTIME(TRS)) THEN
                IF (ISPUMPING(TRS)) THEN
                  DBUS(CULV) = (1.0 - RAMP(TRS))*QP_BASE*T_SF
                ELSE
                  IF(ISPARALLELSLUICE(TRS)) THEN
                    DBUS(CULV) = (1.0 - RAMP(TRS))*QG_BASE*T_SF
                  ELSE
                    DBUS(CULV) = 0.0
                  ENDIF
                ENDIF
              ELSE
                DBUS(CULV) = 0.0
              ENDIF

              CASE (5)  ! Flood generation
                DBUS(CULV) = RAMP(TRS)*QG_BASE*T_SF

              CASE (6)  ! Flood sluicing

              CASE (-2) ! Flood pumping

              CASE DEFAULT ! Error
            END SELECT
          CASE(6) ! Sluice
            S_AREA =
            ! Orifice equation correctly constructed.
            SELECT (MODE(TRS))
              CASE (0)  ! Initial warmup
                DBUS(CULV) = RAMP(TRS)*
     &            ORIFICE(CE1(CULV),AREA,HEADDIFF(TRS))

              CASE (1)  ! High water hold
                IF ((TRS_TIME(TRS)-PREV_SWITCH(TRS))
     &                            .LT.RAMPTIME(TRS)) THEN
                  IF (ISPUMPING(TRS)) THEN
                    DBUS(CULV) = 0.0
                  ELSE
                    DBUS(CULV) = (1.0 - RAMP(TRS))*
     &                ORIFICE(CE1(CULV),S_AREA,HEADDIFF(TRS))*
                  ENDIF
                ELSE
                  DBUS(CULV) = 0.0
                ENDIF
              CASE (2)  ! Ebb generation
              CASE (3)  ! Ebb sluicing
              CASE (-1) ! Ebb pumping
              CASE (4)  ! Low water hold
              CASE (5)  ! Flood generation
              CASE (6)  ! Flood sluicing
              CASE (-2) ! Flood pumping
              CASE DEFAULT ! Error
            END SELECT
          CASE DEFAULT
            IF ((CLPBUS(CULV).GT.6).OR.(CLPBUS(CULV).LE.0)) THEN
              ! Some kind of error
              WRITE(*,*) 'INVALID CULVERT (CLP) VALUE USED'
            ELSE
              ! Culvert operates as usual
            ENDIF
        END SELECT
      END DO
      END SUBROUTINE TRS_FLOWS
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Initialise the arrays for the number of trs schemes when passed the
!     number of schemes.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
C      SUBROUTINE TRS_INIT_NUM(NUMBER_OF_SCHEMES)
C      INTEGER :: NUMBER_OF_SCHEMES
C      NUM_TRS = NUMBER_OF_SCHEMES
C
C
C      END SUBROUTINE TRS_INIT_NUM


      END MODULE TRS_T2D

!     End of TRS_T2D

      PROGRAM TEST_TRS_T2D
      USE TRS_T2D
      INTEGER :: NFO1

C      print *, 'linear interpolate 5,0,10,5,7 = ',
C     &TRS_INTERP(5.0,0.0,10.0,5.0,7.0)
C
      WRITE(*,*) 'Program started...'
      NFO1 = 101

      CALL TRS_READ_DATAFILES(NFO1)
      CALL TRS_FLEX_VALUES(30.0)
      CALL TRS_NEW_MODE(31.0)

      DO I=1,NUM_TRS
        WRITE(*,*) 'TRS[',I,'] - Mode = ',MODE(I)
      ENDDO
      CALL TRS_WRITE_RESULTS(102)
      TRS_TIME = TRS_TIME + 1
      CALL TRS_WRITE_RESULTS(102)
      TRS_TIME = TRS_TIME + 1
      CALL TRS_WRITE_RESULTS(102)
      CALL TRS_WRITE_STATUS()
      !CALL
      !DO I=1,NUM_TRS
      !  WRITE(*,*)
      !ENDDO
      END PROGRAM TEST_TRS_T2D
