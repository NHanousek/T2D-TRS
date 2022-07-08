!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     TRS_T2D - With Parallel basics...
!     Contains the variables and functions
!     that are used to define and run a tidal
!     range energy scheme within the BUSE subroutine
!     and should be able to run more than one at a time.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     INFO
!     Head difference is Inside (aka upstream) - Outside (downstream)
!     Flow from in to out = positive
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
      LOGICAL,SAVE :: TRS_FIRST = .TRUE.      !Is this the first call of the module
      LOGICAL,SAVE :: WRITE_TRS = .FALSE. !Print the TRS data to the file this timestep
      LOGICAL,SAVE :: G_PRNT_TRS = .TRUE. !true => print with graphical printout period
      INTEGER,SAVE :: NUM_TRS = 0         !How many independant tidal range schemes...
      DOUBLE PRECISION,SAVE    :: TRS_TIME
                                          !false => print with 'listing period'

!     TEMPORAL CONTROLS - All in hours
!     One for each Scheme
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: RAMPTIME      !How long to ramp between modes
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: PREV_SWITCH   !Time when scheme last switched modes
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: MAX_GEN       !Maxmimum time before generation is auto ended
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: MAX_HOLD      !Maxmimum time before holding is auto ended
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: MAX_SLUICE    !Maxmimum time before sluicing is auto ended
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: MAX_PUMP      !Maxmimum time before pumping is auto ended
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: ORIG_DIAM_T   ! (m) Original diameter of Turbines used for the hill charts

!     OPERATION CONFIGURATION
!     One for each Scheme
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: IS2WAY           !Is the scheme in two way operation
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: ISFLEXIBLE       !Is the scheme in flexible operation
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: ISPUMPING        !Is the scheme using pumping
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: PUMPTOLEVEL      !Pump to level or head difference
      ! LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: ISPARALLELSLUICE !Is parallel sluicing being used (DEPRECATED AS THIS IS NOW DECLARED AT CULVERT)
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: MODE             !What operational mode is the scheme in
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: MODE_PREV        !What mode was the scheme in prevously

!     WATER LEVELS - Meters
!     One for each Scheme
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: WL_IN,WL_OUT    !Water levels inside and outside, from control points
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: HEADDIFF        !Head difference = Inside - Outside
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: Q_TURB          !Flow rate in m3/s
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: Q_SLUICE        !Flow rate in m3/s
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: POWER           !Power out(+ve)or in(-ve)

!     HILL CHART PARAMETERS - Note that shared head levels are required for powers/flows
!     One for each Hill chart
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE  :: N_HILLS_G,N_HILLS_P     !Number of hill charts for (_G)eneration and (_P)umping
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE  :: LEN_HILLS_G,LEN_HILLS_P !Length of hillcharts for (_G)eneration and (_P)umping
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,SAVE :: HEAD_G,HEAD_P     !Head levels for the hill charts
      DOUBLE PRECISION,DIMENSION(:,:,:),ALLOCATABLE,SAVE :: POWER_G
      DOUBLE PRECISION,DIMENSION(:,:,:),ALLOCATABLE,SAVE :: FLOW_G  !Power/Flow for Generating
      DOUBLE PRECISION,DIMENSION(:,:,:),ALLOCATABLE,SAVE :: POWER_P
      DOUBLE PRECISION,DIMENSION(:,:,:),ALLOCATABLE,SAVE :: FLOW_P  !Power/Flow for Pumping
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE  :: PREV_I_POWER_G  !Save the position of the iterator (can lead to large speedups)
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE  :: PREV_I_FLOW_G
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE  :: PREV_I_POWER_P !Save the position of the iterator (can lead to large speedups)
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE  :: PREV_I_FLOW_P

!     Arrays and values for flexible operation
!     Fixed operation is treated as flexible operation with one long period
!     One for each Scheme
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE  :: N_FLEX   !Number of flex periods for each scheme
!     The flex arrays
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,SAVE :: FLX_TIMES  ! Start time of each flex period (no overlap)
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,SAVE :: H_STARTS
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,SAVE :: H_ENDS ! Start and end piooints in those periods
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,SAVE :: PUMP_TARGS      ! and the _CHARTS indicates the chart to use
      INTEGER,DIMENSION(:,:),ALLOCATABLE,SAVE :: TURB_CHARTS ! turbines (generating)
      INTEGER,DIMENSION(:,:),ALLOCATABLE,SAVE :: PUMP_CHARTS ! and pumping
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE   :: PREV_I_FLX_TIMES !Save the position of the iterator (can lead to large speedups)
!     These variables move as time passes based on when we are and which value to use
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: H_START, H_END          !Head to start/end generation
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: TURB_CHART
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: PUMP_CHART  !Which turbine/pump chart to use
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: PUMP_TARG               !Target to pump to


!     DIMENSIONLESS
!     One for each Scheme
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: RAMP           !Ramp Coefficient when changing modes

!     TEMPORARY VARIABLES
      DOUBLE PRECISION    :: TMP_DOUBLE PRECISION = 0.0
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
      DOUBLE PRECISION FUNCTION TRS_INTERP
     & (X_NEW,X1,X2,Y1,Y2) RESULT(INTERP)
        IMPLICIT NONE
        DOUBLE PRECISION :: X_NEW,X1,X2,Y1,Y2
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
      DOUBLE PRECISION FUNCTION TRS_CALC
     & (HEAD,HEADS,VALUES) RESULT(CALC)
        IMPLICIT NONE
        DOUBLE PRECISION :: HEAD
        DOUBLE PRECISION,DIMENSION(:) :: HEADS,VALUES
        INTEGER :: I,HSIZE,VSIZE,SIGN
        DOUBLE PRECISION :: HD

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
      DOUBLE PRECISION FUNCTION TRS_RAMP
     & (RMPTM,PHSTM) RESULT(RMP)
        IMPLICIT NONE
        DOUBLE PRECISION :: RMPTM,PHSTM
        IF (PHSTM.GE.RMPTM) THEN
          RMP = 1
        ELSE
          RMP = (1 - COS(3.1415926*PHSTM/RMPTM))/2
        ENDIF
        RETURN
      END FUNCTION TRS_RAMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   Orifice equation - returned with direction
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION FUNCTION TRS_ORIFICE
     & (CD,AREA,HDIFF) RESULT(FLOW)
        DOUBLE PRECISION :: CD,AREA,HDIFF
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
      SUBROUTINE TRS_FLEX_VALUES
     &(CT,NUM_TRS,
     & N_FLEX,FLX_TIMES,H_STARTS,H_ENDS,PUMP_TARGS,TURB_CHARTS,
     & PUMP_CHARTS,PREV_I_FLX_TIMES,H_START,H_END,TURB_CHART,
     & PUMP_CHART,PUMP_TARG)

      DOUBLE PRECISION,INTENT(IN) :: CT

!     FLEXIBLE OPERATION
      INTEGER,DIMENSION(:),INTENT(IN)  :: N_FLEX   !Number of flex periods for each scheme
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)    :: FLX_TIMES  ! Start time of each flex period (no overlap)
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)    :: H_STARTS,H_ENDS ! Start and end piooints in those periods
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)    :: PUMP_TARGS      ! and the _CHARTS indicates the chart to use
      INTEGER,DIMENSION(:,:),INTENT(IN) :: TURB_CHARTS ! turbines (generating)
      INTEGER,DIMENSION(:,:),INTENT(IN) :: PUMP_CHARTS ! and pumping
      INTEGER,DIMENSION(:),INTENT(INOUT)   :: PREV_I_FLX_TIMES !Save the position of the iterator (can lead to large speedups)
      DOUBLE PRECISION,DIMENSION(:),INTENT(OUT) :: H_START, H_END          !Head to start/end generation
      DOUBLE PRECISION,DIMENSION(:),INTENT(OUT) :: TURB_CHART,PUMP_CHART  !Which turbine/pump chart to use
      DOUBLE PRECISION,DIMENSION(:),INTENT(OUT) :: PUMP_TARG               !Target to pump to

      INTEGER :: I,TRS

      DO TRS =1,NUM_TRS
      IF (CT.LT.FLX_TIMES(TRS,1)) THEN
        !WARMUP PERIOD
        H_START(TRS) = 1000 !Do not enter generation
        H_END(TRS) = 0.0
        TURB_CHART(TRS) = 1
        PUMP_TARG(TRS) = 0.0
        PUMP_CHART(TRS) = 1
        PREV_I_FLX_TIMES(TRS) = 1
        RETURN

      ELSEIF (CT.GE.FLX_TIMES(TRS,N_FLEX(TRS))) THEN
        !FINAL FLEX PERIOD ONWARDS
        H_START(TRS) = H_STARTS(TRS,N_FLEX(TRS))
        H_END(TRS) = H_ENDS(TRS,N_FLEX(TRS))
        TURB_CHART(TRS) = TURB_CHARTS(TRS,N_FLEX(TRS))
        PUMP_TARG(TRS) = PUMP_TARGS(TRS,N_FLEX(TRS))
        PUMP_CHART(TRS) = PUMP_CHARTS(TRS,N_FLEX(TRS))
        PREV_I_FLX_TIMES(TRS) = N_FLEX(TRS)
        RETURN
        ! SAME FLEX PERIOD AS LAST TIME
      ELSEIF ((CT.GE.FLX_TIMES(TRS,PREV_I_FLX_TIMES(TRS)-1))
     &  .AND.(CT.LT.FLX_TIMES(TRS,PREV_I_FLX_TIMES(TRS)))) THEN
        H_START(TRS) = H_STARTS(TRS,PREV_I_FLX_TIMES(TRS)-1)
        H_END(TRS) = H_ENDS(TRS,PREV_I_FLX_TIMES(TRS)-1)
        TURB_CHART(TRS) = TURB_CHARTS(TRS,PREV_I_FLX_TIMES(TRS)-1)
        PUMP_TARG(TRS) = PUMP_TARGS(TRS,PREV_I_FLX_TIMES(TRS)-1)
        PUMP_CHART(TRS) = PUMP_CHARTS(TRS,PREV_I_FLX_TIMES(TRS)-1)
        PREV_I_FLX_TIMES(TRS) = PREV_I_FLX_TIMES(TRS)
        RETURN

      ELSE
        !IN THE DYNAMIC FLEX PERIOD
        DO I = 2,N_FLEX(TRS)
          IF ((CT.GE.FLX_TIMES(TRS,I-1))
     &      .AND.(CT.LT.FLX_TIMES(TRS,I))) THEN
            H_START(TRS) = H_STARTS(TRS,I-1)
            H_END(TRS) = H_ENDS(TRS,I-1)
            TURB_CHART(TRS) = TURB_CHARTS(TRS,I-1)
            PUMP_TARG(TRS) = PUMP_TARGS(TRS,I-1)
            PUMP_CHART(TRS) = PUMP_CHARTS(TRS,I-1)
            PREV_I_FLX_TIMES(TRS) = I
            RETURN
          ENDIF
        ENDDO
      ENDIF
      ENDDO
      END SUBROUTINE TRS_FLEX_VALUES
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   TRS_NEW_MODE
!   Updates the operating mode of the tidal range schemes based on the
!   decision tree and the current mode, CT only used to check phase
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_NEW_MODE
     &(CT,NUM_TRS,
!    TEMPORAL CONTROLS - All in hours
     & RAMPTIME,PREV_SWITCH,MAX_GEN,MAX_HOLD,
     & MAX_SLUICE,MAX_PUMP,
!    OPERATION CONFIGURATION
     & IS2WAY,ISFLEXIBLE,ISPUMPING,PUMPTOLEVEL,MODE,MODE_PREV,
!    WATER LEVELS - Meters
     & WL_IN,WL_OUT,HEADDIFF,
!    FLEX CONTROLS
     & H_START,H_END,PUMP_TARG)

      DOUBLE PRECISION,INTENT(IN) :: CT!, TRS_RAMP
      INTEGER,INTENT(IN) :: NUM_TRS    !How many independant tidal range schemes...
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: RAMPTIME      !How long to ramp between modes
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: PREV_SWITCH   !Time when scheme last switched modes
      DOUBLE PRECISION,DIMENSION(:),INTENT(IN) :: MAX_GEN       !Maxmimum time before generation is auto ended
      DOUBLE PRECISION,DIMENSION(:),INTENT(IN) :: MAX_HOLD      !Maxmimum time before holding is auto ended
      DOUBLE PRECISION,DIMENSION(:),INTENT(IN) :: MAX_SLUICE    !Maxmimum time before sluicing is auto ended
      DOUBLE PRECISION,DIMENSION(:),INTENT(IN) :: MAX_PUMP      !Maxmimum time before pumping is auto ended
      LOGICAL,DIMENSION(:),INTENT(IN) :: IS2WAY           !Is the scheme in two way operation
      LOGICAL,DIMENSION(:),INTENT(IN) :: ISFLEXIBLE       !Is the scheme in flexible operation
      LOGICAL,DIMENSION(:),INTENT(IN) :: ISPUMPING        !Is the scheme using pumping
      LOGICAL,DIMENSION(:),INTENT(IN) :: PUMPTOLEVEL      !Pump to level or head difference
      INTEGER,DIMENSION(:),INTENT(INOUT) :: MODE             !What operational mode is the scheme in
      INTEGER,DIMENSION(:),INTENT(INOUT) :: MODE_PREV        !What mode was the scheme in prevously
      DOUBLE PRECISION,DIMENSION(:),INTENT(IN) :: WL_IN,WL_OUT    !Water levels inside and outside, from control points
      DOUBLE PRECISION,DIMENSION(:),INTENT(IN) :: HEADDIFF        !Head difference = Inside - Outside
      DOUBLE PRECISION,DIMENSION(:),INTENT(IN) :: H_START,H_END          !Head to start/end generation
      DOUBLE PRECISION,DIMENSION(:),INTENT(IN) :: PUMP_TARG               !Target to pump to

      DOUBLE PRECISION :: PHASE_T, HD
      INTEGER :: TRS

      DO TRS = 1,NUM_TRS
      HD = ABS(HEADDIFF(TRS))
      PHASE_T = CT - PREV_SWITCH(TRS) ! Initialise with prior phase time

      IF (PHASE_T.GE.RAMPTIME(TRS)) THEN !If we have completed starting operation of this phase
        SELECT CASE(MODE(TRS))
          CASE (0)  !      0 : Initial sluicing warmup
!           Ends when the start of flexing begins
            IF (CT.GE.FLX_TIMES(TRS,1)) THEN
              MODE(TRS) = 1  !      1 : High water holding
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (1)  !      1 : High water holding
            IF ((HD.GE.H_START(TRS))              ! Enough head difference
     &      .OR.(PHASE_T.GT.MAX_HOLD(TRS))) THEN  ! Time limit reached
              MODE(TRS) = 2                       ! 2 : Ebb generation
              PREV_SWITCH(TRS) = CT               ! Update previous switch
            ENDIF
          CASE (2)  !      2 : Ebb generation
            IF ((HD.LE.H_END(TRS))                ! Head diff end triggered
     &      .OR.(PHASE_T.GT.MAX_GEN(TRS))) THEN   ! Time limit reached
              IF (IS2WAY(TRS)) THEN
                MODE(TRS) = 3  !      3 : Ebb sluicing
              ELSE
                MODE(TRS) = 4  !      4 : Low water holding
              ENDIF
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (3)  !      3 : Ebb sluicing
!           HERE WE TAKE A FAIRLY BIG ASSUMPTION ABOUT HOW CLOSE TWO WATER LEVELS NEED
!           TO BE TO COUNT AS BEING EQUAL, 0.1m MAY NEED SENSITIVITY TESTING
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
!         This is a big nasty fortran logical statement.
            IF ((PUMPTOLEVEL(TRS) ! Pump to a physical level above the datum
     &          .AND.(WL_IN(TRS).LE.PUMP_TARG(TRS)))
     &      .OR.(.NOT.(PUMPTOLEVEL(TRS)) ! Pump to a given head difference
     &          .AND.(HD.GE.PUMP_TARG(TRS)))
     &      .OR.(PHASE_T.GE.MAX_PUMP(TRS))) THEN ! Time limit reached
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
          IF ((PUMPTOLEVEL(TRS)
     &        .AND.(WL_IN(TRS).GE.PUMP_TARG(TRS)))
     &    .OR.(.NOT.(PUMPTOLEVEL(TRS))
     &        .AND.(HD.GE.PUMP_TARG(TRS)))
     &    .OR.(PHASE_T.GE.MAX_PUMP(TRS))) THEN
              MODE(TRS) = 1  !      1 : High water holding
              PREV_SWITCH(TRS) = CT
            ENDIF
        END SELECT
!       UPDATE PHASE TIMING?
!       PHASE_T = CT - PREV_SWITCH(TRS)
      ENDIF
      PHASE_T = CT - PREV_SWITCH(TRS)
      RAMP(TRS) = TRS_RAMP(RAMPTIME(TRS),PHASE_T)
      ENDDO
      RETURN
      END SUBROUTINE TRS_NEW_MODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Read the control data file
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      ! RUDIMENTARY INPUT KEY VARIABLE PARSER.
      ! CAN HANDLE
      !     : and = as pair separators
      !     / and ! as comment starters
      SUBROUTINE TRS_PARSE(LN,KWRD,VRBL)
      CHARACTER(LEN=*),INTENT(IN) :: LN ! A line from the file
      CHARACTER(LEN=100),INTENT(OUT) :: KWRD,VRBL ! Keyword and Variable
      INTEGER :: K,LENLN,CMT  !iterator, length of line
      ! Default values, for cases where only the keyword is to be passed etc.
      KWRD = TRIM(ADJUSTL(LN))  !Trim any preceeding or following blanks
      VRBL = 'NaN'  !initial value

!     Strip any comments from the back of the line
      IF ((INDEX(LN,'!').GE.1).OR.(INDEX(LN,'/').GE.1)) THEN
        CMT=MIN(INDEX(LN,'!'),INDEX(LN,'/'))
        IF (CMT.EQ.0) THEN
            CMT=MAX(INDEX(LN,'!'),INDEX(LN,'/'))
        ENDIF
      ELSE
        CMT = LEN(LN) + 1 ! set length of line
      ENDIF

!     Find the two separate chunks of text in the line

      IF ((INDEX(LN,'=').GT.1).OR.(INDEX(LN,':').GT.1)) THEN
        K = MAX(INDEX(LN,'='),INDEX(LN,':'))
        KWRD = TRIM(LN(1:k-1))
        VRBL = TRIM(LN(k+1:CMT-1))
      ELSE
        DO K = 2,CMT-1
          IF ((LN(K:K).EQ.' ').AND.(LN(K+1:K+1).NE.' ')) THEN
!           Trimming and adjustl-ing them may be a bit belt + braces
            KWRD = TRIM(ADJUSTL(LN(1:k)))
            VRBL = TRIM(ADJUSTL(LN(K+1:CMT-1)))
            EXIT
          ENDIF
        END DO
      ENDIF
      END SUBROUTINE TRS_PARSE

      SUBROUTINE TRS_KV_WARN(K,V)
      CHARACTER(LEN=*),INTENT(IN) :: K,V
        WRITE(*,*) '+-+-+-+-+-+-+-WARNING-+-+-+-+-+-+-+-+-+-+-+'
        WRITE(*,*) 'Unextpected key - variable pair:'
        WRITE(*,*) 'Key: ',TRIM(ADJUSTL(K))
        WRITE(*,*) 'Variable: ',TRIM(ADJUSTL(V))
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
      END SUBROUTINE TRS_KV_WARN

!     READ A Hill Chart from the file point
      SUBROUTINE TRS_LOAD_HILLCHART(HEADS,ARRAY,NF_TRS)
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: HEADS    ! Head difference values
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT) :: ARRAY  ! Array of variable
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
      DOUBLE PRECISION,DIMENSION(:),INTENT(IN) :: HEADS    ! Head difference values
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN) :: ARRAY  ! Array of variable
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
!       Print it back to the console for user happiness
        WRITE(*,'(I10,A1,F9.3,A1)',ADVANCE='NO')ROW,' ',HEADS(ROW),' '
        DO COL = 1,SZ(2)
            WRITE(*,'(F9.3,A1)',ADVANCE='NO') ARRAY(ROW,COL),' '
        ENDDO
        WRITE(*,*) ' '

        TITLEROW = .FALSE.
      ENDDO
      END SUBROUTINE TRS_PRINT_HILLCHART

      SUBROUTINE TRS_READ_DATAFILES
     &(TRS_FRST,NF_TRS,NM_TRS,
     & RMPTM,PRV_SWTCH,MX_GN,MX_HLD,
     & MX_SLC,MX_PMP,ORG_DM_T,
!     OPERATION CONFIGURATION
     & IS2WY,ISFLXBL,ISPMPNG,PMPTLVL,MD,MD_PRV,
!     WATER LEVELS - Meters
     & W_IN,W_OUT,HDDFF,Q_TRB,Q_SLC,PWR,
!     HILL CHART PARAMETERS - Note that shared head levels are required for powers/flows
     & N_HLLS_G,N_HLLS_P,LN_HLLS_G,LN_HLLS_P,HD_G,HD_P,
     & PWR_G,FLW_G,PWR_P,FLW_P,PRV_I_PWR_G,PRV_I_FLW_G,
     & PRV_I_PWR_P,PRV_I_FLW_P,
!     FLEXIBLE OPERATION
     & N_FLX,FLX_TMS,H_STRTS,H_NDS,PMP_TRGS,TRB_CHRTS,
     & PMP_CHRTS,PRV_I_FLX_TMS,H_STRT,H_ND,TRB_CHRT,
     & PMP_CHRT,PMP_TRG,
!     DIMENSIONLESS
     & RMP)
      LOGICAL,                      INTENT(INOUT) :: TRS_FRST
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: RMPTM      !How long to ramp between modes
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: PRV_SWTCH   !Time when scheme last switched modes
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: MX_GN       !Maxmimum time before generation is auto ended
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: MX_HLD      !Maxmimum time before holding is auto ended
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: MX_SLC    !Maxmimum time before sluicing is auto ended
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: MX_PMP      !Maxmimum time before pumping is auto ended
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: ORG_DM_T   ! (m) Original diameter of Turbines used for the hill charts
!     OPERATION CONFIGURATION
      LOGICAL,DIMENSION(:),INTENT(INOUT) :: IS2WY           !Is the scheme in two way operation
      LOGICAL,DIMENSION(:),INTENT(INOUT) :: ISFLXBL       !Is the scheme in flexible operation
      LOGICAL,DIMENSION(:),INTENT(INOUT) :: ISPMPNG        !Is the scheme using pumping
      LOGICAL,DIMENSION(:),INTENT(INOUT) :: PMPTLVL      !Pump to level or head difference
      INTEGER,DIMENSION(:),INTENT(INOUT) :: MD             !What operational mode is the scheme in
      INTEGER,DIMENSION(:),INTENT(INOUT) :: MD_PRV        !What mode was the scheme in prevously
!     WATER LEVELS - Meters
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: W_IN,W_OUT    !Water levels inside and outside, from control points
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: HDDFF        !Head difference = Inside - Outside
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: Q_TRB,Q_SLC !Flow rate in m3/s
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: PWR           !Power out(+ve)or in(-ve)
!     HILL CHART PARAMETERS - Note that shared head levels are required for powers/flows
      INTEGER,DIMENSION(:),INTENT(INOUT)  :: N_HLLS_G,N_HLLS_P     !Number of hill charts for (_G)eneration and (_P)umping
      INTEGER,DIMENSION(:),INTENT(INOUT)  :: LN_HLLS_G,LN_HLLS_P !Length of hillcharts for (_G)eneration and (_P)umping
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)   :: HD_G,HD_P     !Head levels for the hill charts
      DOUBLE PRECISION,DIMENSION(:,:,:),INTENT(INOUT) :: PWR_G,FLW_G  !Power/Flow for Generating
      DOUBLE PRECISION,DIMENSION(:,:,:),INTENT(INOUT) :: PWR_P,FLW_P  !Power/Flow for Pumping
      INTEGER,DIMENSION(:),INTENT(INOUT)  :: PRV_I_PWR_G  !Save the position of the iterator (can lead to large speedups)
      INTEGER,DIMENSION(:),INTENT(INOUT)  :: PRV_I_FLW_G
      INTEGER,DIMENSION(:),INTENT(INOUT)  :: PRV_I_PWR_P !Save the position of the iterator (can lead to large speedups)
      INTEGER,DIMENSION(:),INTENT(INOUT)  :: PRV_I_FLW_P
!     FLEXIBLE OPERATION
      INTEGER,DIMENSION(:),INTENT(INOUT)  :: N_FLX   !Number of flex periods for each scheme
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)    :: FLX_TMS  ! Start time of each flex period (no overlap)
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)    :: H_STRTS,H_NDS ! Start and end piooints in those periods
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)    :: PMP_TRGS      ! and the _CHARTS indicates the chart to use
      INTEGER,DIMENSION(:,:),INTENT(INOUT) :: TRB_CHRTS ! turbines (generating)
      INTEGER,DIMENSION(:,:),INTENT(INOUT) :: PMP_CHRTS ! and pumping
      INTEGER,DIMENSION(:),INTENT(INOUT)   :: PRV_I_FLX_TMS !Save the position of the iterator (can lead to large speedups)
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: H_STRT, H_ND          !Head to start/end generation
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: TRB_CHRT, PMP_CHRT  !Which turbine/pump chart to use
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: PMP_TRG               !Target to pump to
!     DIMENSIONLESS
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: RMP           !Ramp Coefficient when changing modes

!       !File with the tidal range scheme control parameters
!       !as key variable pairs, with comments if needed
! !      NFO1=T2D_FILES(T2DFO1)%LU
!       INTEGER,INTENT(IN) :: NF_TRS,NM_TRS

      INTEGER :: TRS
      CHARACTER(LEN=100) :: LINE,KEYWORD,VARIABLE
      CHARACTER(LEN=20) :: TRS_FNAME
      LOGICAL :: ISOPEN = .FALSE.,PRNTKV

      IF (.NOT.ALLOCATED(W_IN)) THEN
        ALLOCATE(W_IN(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'WL_IN')
      ENDIF
      IF (.NOT.ALLOCATED(W_OUT)) THEN
        ALLOCATE(W_OUT(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'WL_OUT')
      ENDIF
      IF (.NOT.ALLOCATED(HDDFF)) THEN
        ALLOCATE(HDDFF(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'HEADDIFF')
      ENDIF
      IF (.NOT.ALLOCATED(RMP)) THEN
        ALLOCATE(RMP(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'RAMP')
      ENDIF
      IF (.NOT.ALLOCATED(H_STRT)) THEN
        ALLOCATE(H_STRT(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'H_START')
      ENDIF
      IF (.NOT.ALLOCATED(H_ND)) THEN
        ALLOCATE(H_ND(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'H_END')
      ENDIF
      IF (.NOT.ALLOCATED(TRB_CHRT)) THEN
        ALLOCATE(TRB_CHRT(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'TURB_CHART')
      ENDIF
      IF (.NOT.ALLOCATED(PMP_CHRT)) THEN
        ALLOCATE(PMP_CHRT(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PUMP_CHART')
      ENDIF
      IF (.NOT.ALLOCATED(PMP_TRG)) THEN
        ALLOCATE(PMP_TRG(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PUMP_TARG')
      ENDIF
      IF (.NOT.ALLOCATED(Q_TRB)) THEN
        ALLOCATE(Q_TRB(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'Q_TURB')
      ENDIF
      IF (.NOT.ALLOCATED(Q_SLC)) THEN
        ALLOCATE(Q_SLC(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'Q_SLUICE')
      ENDIF
      IF (.NOT.ALLOCATED(PWR)) THEN
        ALLOCATE(PWR(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'POWER')
      ENDIF
      IF (.NOT.ALLOCATED(PRV_I_FLX_TMS)) THEN
        ALLOCATE(PRV_I_FLX_TMS(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_FLX_TIMES')
      ENDIF
      IF (.NOT.ALLOCATED(PRV_I_FLW_G)) THEN
        ALLOCATE(PRV_I_FLW_G(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_FLOW_G')
      ENDIF
      IF (.NOT.ALLOCATED(PRV_I_FLW_P)) THEN
        ALLOCATE(PRV_I_FLW_P(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_FLOW_P')
      ENDIF
      IF (.NOT.ALLOCATED(PRV_I_PWR_G)) THEN
        ALLOCATE(PRV_I_PWR_G(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_POWER_G')
      ENDIF
      IF (.NOT.ALLOCATED(PRV_I_PWR_P)) THEN
        ALLOCATE(PRV_I_PWR_P(NM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_POWER_P')
      ENDIF

      TRS_TIME = 0.0

      DO TRS = 1,NM_TRS
        IF (TRS.LE.9) THEN
          WRITE(TRS_FNAME,'(A7,I1,A4)') '../TRS-',TRS,'.txt'
        ELSEIF (TRS.LE.99) THEN
          WRITE(TRS_FNAME,'(A7,I2,A4)') '../TRS-',TRS,'.txt'
        ELSE ! if you want over 1000 TRS you have bigger problems.
          WRITE(TRS_FNAME,'(A7,I3,A4)') '../TRS-',TRS,'.txt'
        ENDIF
        ! TRS_FNAME = 'TRS-1.txt'
        OPEN(UNIT=NF_TRS,FILE=TRS_FNAME,STATUS='OLD')
        INQUIRE(UNIT=NF_TRS,OPENED=ISOPEN)

      IF (ISOPEN) THEN
        REWIND(NF_TRS)
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
        WRITE(*,*) '              File [',TRS_FNAME,'] Opened'
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
      IF (TRS_FIRST) THEN
        READ(NF_TRS,*) LINE !Header line
        WRITE(*,*) LINE
        READ(NF_TRS,'(A100)') LINE
        ! YOU CANNOT HAVE A VARIABLE WHOSE NAME STARTS WITH 'END' in some form
        DO WHILE(TO_UPPER((LINE(1:3)).NE.'END'))
!          WRITE(*,*) LINE
          CALL TRS_PARSE(LINE,KEYWORD,VARIABLE)

          SELECT CASE(TO_UPPER(TRIM(ADJUSTL(KEYWORD))))  !Quite rough'n'ready...
!           SIMPLE KEY -> VARIABLE PAIRS
            CASE('RAMPTIME')  ! Hours
            IF(.NOT.ALLOCATED(RMPTM)) THEN
              ALLOCATE(RMPTM(NM_TRS))
              RMPTM(:) = 0.25 !Default will be 15 minutes
            ENDIF
              READ(VARIABLE,*) RMPTM(TRS)

            CASE('PREV_SWITCH') ! Hours in model
            IF(.NOT.ALLOCATED(PRV_SWTCH)) THEN
              ALLOCATE(PRV_SWTCH(NM_TRS))
              PRV_SWTCH(:) = 0.0 ! Default will be a fresh initialisation
              !Only needs modifiaction for alternate warmup functions
            ENDIF
              READ(VARIABLE,*) PRV_SWTCH(TRS)

            CASE('MAX_GEN') ! Hours
            IF(.NOT.ALLOCATED(MX_GN)) THEN
              ALLOCATE(MX_GN(NM_TRS))
              MX_GN(:) = 12.0 ! Default value
            ENDIF
              READ(VARIABLE,*) MX_GN(TRS)

            CASE('MAX_HOLD') !HOURS
            IF(.NOT.ALLOCATED(MX_HLD)) THEN
              ALLOCATE(MX_HLD(NM_TRS))
              MX_HLD(:) = 12.0 ! Default value
            ENDIF
              READ(VARIABLE,*) MX_HLD(TRS)

            CASE('MAX_SLUICE') ! Hours
            IF(.NOT.ALLOCATED(MX_SLC)) THEN
              ALLOCATE(MX_SLC(NM_TRS))
              MX_SLC(:) = 12.0 ! Default value
            ENDIF
              READ(VARIABLE,*) MX_SLC(TRS)

            CASE('MAX_PUMP') ! Hours
            IF(.NOT.ALLOCATED(MX_PMP)) THEN
              ALLOCATE(MX_PMP(NM_TRS))
              MX_PMP(:) = 12.0 ! Default value
            ENDIF
              READ(VARIABLE,*) MX_PMP(TRS)

            CASE('IS2WAY') ! Logical (1 or 0, I think)
            IF(.NOT.ALLOCATED(IS2WY)) THEN
              ALLOCATE(IS2WY(NM_TRS))
              IS2WY(:) = .TRUE. ! Default value
            ENDIF
              READ(VARIABLE,*) IS2WY(TRS)

            CASE('ISFLEXIBLE') ! Logical (1 or 0, I think)
            IF(.NOT.ALLOCATED(ISFLXBL)) THEN
              ALLOCATE(ISFLXBL(NM_TRS))
              ISFLXBL(:) = .FALSE.
            ENDIF
              READ(VARIABLE,*) ISFLXBL(TRS)

            CASE('ISPUMPING') ! Logical (1 or 0, I think)
            IF(.NOT.ALLOCATED(ISPMPNG)) THEN
              ALLOCATE(ISPMPNG(NM_TRS))
              ISPMPNG(:) = .FALSE.
            ENDIF
              READ(VARIABLE,*) ISPMPNG(TRS)

            ! CASE('ISPARALLELSLUICE') ! Logical (1 or 0, I think)
            ! IF(.NOT.ALLOCATED(ISPARALLELSLUICE)) THEN
            !   ALLOCATE(ISPARALLELSLUICE(NUM_TRS))
            !   ISPARALLELSLUICE(:) = .FALSE.
            ! ENDIF
            !   READ(VARIABLE,*) ISPARALLELSLUICE(TRS)

            CASE('MODE')  ! Integer mode code
            IF(.NOT.ALLOCATED(MD)) THEN
              ALLOCATE(MD(NM_TRS))
              MD(:) = 0 !Only modify default in test or continued operation
            ENDIF
              READ(VARIABLE,*) MD(TRS)

            CASE('N_HILLS_G')  ! Integer
            IF(.NOT.ALLOCATED(N_HLLS_G)) THEN
              ALLOCATE(N_HLLS_G(NM_TRS))
              N_HLLS_G(:) = 1
            ENDIF
              READ(VARIABLE,*) N_HLLS_G(TRS)

            CASE('N_HILLS_P')  ! Integer
            IF(.NOT.ALLOCATED(N_HLLS_P)) THEN
              ALLOCATE(N_HLLS_P(NM_TRS))
              N_HLLS_P(:) = 1
            ENDIF
              READ(VARIABLE,*) N_HLLS_P(TRS)

            CASE('LEN_HILLS_G')  ! Integer
            IF(.NOT.ALLOCATED(LN_HLLS_G)) THEN
              ALLOCATE(LN_HLLS_G(NM_TRS))
              LN_HLLS_G(:) = 1
            ENDIF
              READ(VARIABLE,*) LN_HLLS_G(TRS)

            CASE('LEN_HILLS_P')  ! Integer
            IF(.NOT.ALLOCATED(LN_HLLS_P)) THEN
              ALLOCATE(LN_HLLS_P(NM_TRS))
              LN_HLLS_P(:) = 1
            ENDIF
              READ(VARIABLE,*) LN_HLLS_P(TRS)

            CASE('N_FLEX')  ! Integer
            IF(.NOT.ALLOCATED(N_FLX)) THEN
              ALLOCATE(N_FLX(NM_TRS))
              N_FLX(:) = 1 !Effectively fixed operation
            ENDIF
              READ(VARIABLE,*) N_FLX(TRS) ! Allocate the flex arrays here, as they only need this?
              IF (.NOT.ALLOCATED(FLX_TMES)) THEN
                ALLOCATE(FLX_TMS(TRS,N_FLX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'FLX_TIMES')
                FLX_TMS(:,:) = 0.0

                ALLOCATE(H_STRTS(TRS,N_FLX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'H_STARTS')
                H_STRTS(:,:) = 0.0

                ALLOCATE(H_NDS(TRS,N_FLX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'H_ENDS')
                H_NDS(:,:) = 0.0

                ALLOCATE(TRB_CHRTS(TRS,N_FLX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'TURB_CHARTS')
                TRB_CHRTS(:,:) = 0

                ALLOCATE(PMP_TRGS(TRS,N_FLX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'PUMP_TARGS')
                PMP_TRGS(:,:) = 0.0

                ALLOCATE(PMP_CHRTS(TRS,N_FLX(TRS)),STAT=STATUS)
                CALL TRS_STAT(STATUS,'PUMP_CHARTS')
                PMP_CHRTS(:,:) = 0

              ENDIF
            CASE('PUMPTOLEVEL')  ! Integer
            IF(.NOT.ALLOCATED(PMPTLVL)) THEN
              ALLOCATE(PMPTLVL(NM_TRS))
              PMPTLVL(:) = 0
            ENDIF
              READ(VARIABLE,*) PMPTLVL(TRS)

            CASE('MODE_PREV') ! Integer (Don't think it does anything)
            IF(.NOT.ALLOCATED(MD_PRV)) THEN
              ALLOCATE(MD_PRV(NM_TRS))
              MD_PRV(:) = 0
            ENDIF
              READ(VARIABLE,*) MD_PRV(TRS)

            CASE('ORIG_DIAM_T') !Diameter(meter) of original turbine used in hill charts
            IF(.NOT.ALLOCATED(ORG_DM_T)) THEN
              ALLOCATE(ORG_DM_T(NUM_TRS))
              ORG_DM_T(:) = 0.0
            ENDIF
              READ(VARIABLE,*) ORG_DM_T(TRS)

!           MORE COMPLEX ARRAY LOADERS
            CASE('GENERATOR')
              PRNTKV = .FALSE.
              !WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)
              IF (.NOT.ALLOCATED(HD_G)) THEN
                ALLOCATE(HD_G(NM_TRS,LN_HLLS_G(TRS)),STAT=STATUS)
              ENDIF
              CALL TRS_STAT(STATUS,'HILL CHART: Generator heads')
              ! Power indicator words will contain the letter p
              IF ((INDEX(VARIABLE,'P').NE.0).OR.
     &             INDEX(VARIABLE,'p').NE.0) THEN
              IF (.NOT.ALLOCATED(PWR_G)) THEN
                ALLOCATE(PWR_G(UM_TRS,MAXVAL(LN_HLLS_G),
     &            MAXVAL(N_HLLS_G)),STAT=STATUS)
              ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Generator Powers')
                !There may be a better way to do this to be honest...
                CALL TRS_LOAD_HILLCHART(HD_G(TRS,:),
     &            PWR_G(TRS,:,:),NF_TRS)
                CALL TRS_PRINT_HILLCHART(HD_G(TRS,:),
     &            PWR_G(TRS,:,:),'GENERATOR','POWER')

              ! Flow will have an F
              ELSEIF ((INDEX(VARIABLE,'F').NE.0).OR.
     &             INDEX(VARIABLE,'f').NE.0) THEN
                IF (.NOT.ALLOCATED(FLW_G)) THEN
                  ALLOCATE(FLW_G(NM_TRS,MAXVAL(LN_HLLS_G),
     &              MAXVAL(N_HLLS_G)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Generator Flow')
                CALL TRS_LOAD_HILLCHART(HD_G(TRS,:),
     &            FLW_G(TRS,:,:),NF_TRS)
                CALL TRS_PRINT_HILLCHART(HD_G(TRS,:),
     &            FLW_G(TRS,:,:),'GENERATOR','FLOW')
              ELSE !Error condition of some kind
                CALL TRS_KV_WARN(KEYWORD,VARIABLE)
              ENDIF

            CASE('PUMP')
              PRNTKV = .FALSE.
              !WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)
              IF (.NOT.ALLOCATED(HD_P)) THEN
                ALLOCATE(HD_P(TRS,LN_HLLS_P(TRS)),STAT=STATUS)
              ENDIF
              CALL TRS_STAT(STATUS,'HILL CHART: Pump heads')
              ! Power indicator words will contain the letter p
              IF ((INDEX(VARIABLE,'P').NE.0).OR.
     &             INDEX(VARIABLE,'p').NE.0) THEN
                IF (.NOT.ALLOCATED(PWR_P)) THEN
                  ALLOCATE(PWR_P(TRS,MAXVAL(LN_HLLS_P),
     &              MAXVAL(N_HLLS_P)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Pump Powers')
                CALL TRS_LOAD_HILLCHART(HD_P(TRS,:),
     &            PWR_P(TRS,:,:),NF_TRS)
                CALL TRS_PRINT_HILLCHART(HEAD_P(TRS,:),
     &            PWR_P(TRS,:,:),'PUMP','POWER')
              ! Flow will have an F
              ELSEIF ((INDEX(VARIABLE,'F').NE.0).OR.
     &             INDEX(VARIABLE,'f').NE.0) THEN
                IF (.NOT.ALLOCATED(FLW_P)) THEN
                  ALLOCATE(FLW_P(TRS,MAXVAL(LN_HLLS_P),
     &              MAXVAL(N_HLLS_P)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Pump Flow')
                CALL TRS_LOAD_HILLCHART(HD_P(TRS,:),
     &            FLW_P(TRS,:,:),NF_TRS)
                CALL TRS_PRINT_HILLCHART(HD_P(TRS,:),
     &            FLW_P(TRS,:,:),'PUMP','FLOW')
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
              DO K = 1,N_FLX(TRS)
                READ(NF_TRS,*) FLX_TMS(TRS,K),
     &            H_STRTS(TRS,K),
     &            H_NDS(TRS,K),
     &            TRB_CHRTS(TRS,K),
     &            PMP_TRGS(TRS,K),
     &            PMP_CHRTS(TRS,K)
                WRITE(*,*) K,
     &            FLX_TMS(TRS,K),
     &            H_STRTS(TRS,K),
     &            H_NDS(TRS,K),
     &            TRB_CHRTS(TRS,K),
     &            PMP_TRGS(TRS,K),
     &            PMP_CHRTS(TRS,K)
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
      TRS_FRST = .FALSE.
      END SUBROUTINE TRS_READ_DATAFILES
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Write the headers and initial row of the output CSV
!     Then write the updated data to the respective files.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_WRITE_RESULTS
     &(NUM_TRS, TRS_TIME, MODE, WL_OUT, WL_IN, Q_TURB, Q_SLUICE,
     & POWER, PUMP_TARG, OF_TRS)

      INTEGER,  INTENT(IN) :: NUM_TRS
      DOUBLE PRECISION,     INTENT(IN) :: TRS_TIME
      INTEGER,  INTENT(IN) :: MODE(NUM_TRS)
      DOUBLE PRECISION,INTENT(IN) :: WL_OUT(NUM_TRS),WL_IN(NUM_TRS)
      DOUBLE PRECISION,INTENT(IN) :: Q_TURB(NUM_TRS),Q_SLUICE(NUM_TRS)
      DOUBLE PRECISION,INTENT(IN) :: POWER(NUM_TRS),PUMP_TARG(NUM_TRS)
      INTEGER,INTENT(IN) :: OF_TRS

      INTEGER :: TRS,STAT
      CHARACTER(LEN=30) :: TRS_FNAME
      CHARACTER :: A = ','
      LOGICAL,SAVE :: RESULTS_FIRST = .TRUE.

      IF (RESULTS_FIRST) THEN
        TRS_TIME = 0.0
        DO TRS = 1,NUM_TRS
          ! There may be a better way to do this but this should work so...
          IF (TRS.LE.9) THEN
            WRITE(TRS_FNAME,'(A7,I1,A12)') '../TRS-',TRS,'-Results.csv'
          ELSEIF (TRS.LE.99) THEN
            WRITE(TRS_FNAME,'(A7,I2,A12)') '../TRS-',TRS,'-Results.csv'
          ELSE ! if you want over 1000 TRS you have bigger problems.
            WRITE(TRS_FNAME,'(A7,I3,A12)') '../TRS-',TRS,'-Results.csv'
          ENDIF

          OPEN(OF_TRS,FILE=TRS_FNAME,STATUS='REPLACE',IOSTAT=STAT)
          WRITE(OF_TRS,*)'TIME(hr),Z_OUT(mD),Z_IN(mD),HEADDIFF(m),',
     &      'MODE(-),Q_TURB(m3_s),Q_SLUICE(m3_s),POWER(MW),',
     &      'T_CHART(-),P_CHART(-),H_START(m),H_END(m),P_TARG(-)'
          CLOSE(OF_TRS)
        ENDDO
        RESULTS_FIRST = .FALSE.
      ENDIF
      DO TRS = 1,NUM_TRS
        ! There may be a better way to do this but this should work so...
        IF (TRS.LE.9) THEN
          WRITE(TRS_FNAME,'(A7,I1,A12)') '../TRS-',TRS,'-Results.csv'
        ELSEIF (TRS.LE.99) THEN
          WRITE(TRS_FNAME,'(A7,I2,A12)') '../TRS-',TRS,'-Results.csv'
        ELSE ! if you want over 1000 TRS you have bigger problems.
          WRITE(TRS_FNAME,'(A7,I3,A12)') '../TRS-',TRS,'-Results.csv'
        ENDIF

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
      SUBROUTINE TRS_WRITE_STATUS
     &(NUM_TRS, TRS_TIME, MODE, WL_OUT, WL_IN, Q_TURB, Q_SLUICE,
     & POWER, PUMP_TARG)

      INTEGER,  INTENT(IN) :: NUM_TRS
      DOUBLE PRECISION,INTENT(IN) :: TRS_TIME
      INTEGER,  INTENT(IN) :: MODE(NUM_TRS)
      DOUBLE PRECISION,INTENT(IN) :: WL_OUT(NUM_TRS),WL_IN(NUM_TRS)
      DOUBLE PRECISION,INTENT(IN) :: Q_TURB(NUM_TRS),Q_SLUICE(NUM_TRS)
      DOUBLE PRECISION,INTENT(IN) :: POWER(NUM_TRS),PUMP_TARG(NUM_TRS)
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
!     TRS_FLOWS()
!     This is a big boy, I'm pretty sure it should go through each
!     culvert and identify:
!       the correct TRS
!         Mode of that TRS
!           Type of culvert
!             the flow through the culvert
!     based on that.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     A Reminder on the culvert data and what it does now...
! Key     Old use
! I1      First culvert point           Upstream (or internal) point
! I2      Second culvert point          Downstream (or external) point
! CE1     Head loss coeff as inlet p1   Ebb head loss coeff generate/sluice
! CE2     Head loss coedd as inlet p2   Flood head loss coeff generate/sluice
! CS1     Head loss coeff outlet p1     Ebb head loss coeff pumping
! CS2     Head loss coeff outlet p2     Flood head loss coeff pumping
! LRG     Width of the culvert          Width of sluice gate, turbine diameter
! HAUT1   Height of construction p1     Minimum water height for safe operation at p1
! CLP     Flow control type             Culvert type: 4 = control, 5 = Turbine, 6 = Sluice, [0,1,2,3] as before
! LBUS    Linear head loss in culvert   Turbine Cd when sluicing
! Z1      Level of inlet/outlet at p1   Turbine draft tube area at p1
! Z2      Level of inlet/outlet at p2   Turbine draft tube area at p2
! CV      Loss coeff due to valve       Weighting for control points
! C56     Constant to determ flow type  Parrallel sluicing for turbine: 0 = FALSE, 1 = TRUE
! CV5     Correction for type 5 flow    Turbine type: 1 = Turb+Pump, 2 = Turb Only, 3 = Pump Only
! C5      Correction for type 5 flow    unused
! CT      Loss coeff for trash screen   Tidal Range Scheme Number!!!!!!!!!!!!!!!!!!!!!
! HAUT2   Height of construction p2     Minimum water height for safe operation at p2
! FRIC    Manning Strickler coeff       Friction of the sluice gate sides?
! LENGTH  Length of the culvert         Length of sluice gate
! CIRC    Shape: 1=circle, 0=Rect       Circular or rectangular sluice gate
! D1      Angle of pipe-bottom p1       SAME/Unused
! D2      Angle of pipe-bottom p2       SAME/Unused
! A1      Angle of pipe-x_axis p1       SAME/Unused
! A2      Angle of pipe-x_axis p2       SAME/Unused
! AA      1: Auto angles, 2: User       1: Auto individual, 2: User, 3: Mean-Bank
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ! It os possible that this sub needs to call all the variables used...
      ! NH FORTRAN CHECK 01/07/2022 [NOT DONE YET]
      SUBROUTINE TRS_FLOWS
     &(NUM_TRS,NBUSE,HEADDIFF,MODE, TURB_CHART,
     & PUMP_CHART,WL_OUT,WL_IN,Q_TURB,Q_SLUICE,
     & POWER,PUMP_TARG,HEAD_G,FLOW_G,HEAD_P,FLOW_P,
     & DBUS,H,ZF)

      ! Externals
      INTEGER,  INTENT(IN) :: NUM_TRS,NBUSE
      DOUBLE PRECISION,     INTENT(IN) :: HEADDIFF(NUM_TRS)
      INTEGER,  INTENT(IN) :: MODE(NUM_TRS), TURB_CHART(NUM_TRS)
      INTEGER,  INTENT(IN) :: PUMP_CHART(NUM_TRS)
      DOUBLE PRECISION,INTENT(IN) :: WL_OUT(NUM_TRS),WL_IN(NUM_TRS)
      DOUBLE PRECISION,INTENT(OUT) :: Q_TURB(NUM_TRS),Q_SLUICE(NUM_TRS)
      DOUBLE PRECISION,INTENT(IN) :: POWER(NUM_TRS),PUMP_TARG(NUM_TRS)
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN) :: HEAD_G
      DOUBLE PRECISION,DIMENSION(:,:,:),INTENT(IN) :: FLOW_G
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN) :: HEAD_P
      DOUBLE PRECISION,DIMENSION(:,:,:),INTENT(IN) :: FLOW_P

      DOUBLE PRECISION,INTENT(INOUT) :: DBUS(NBUSE)
      DOUBLE PRECISION, INTENT(IN) :: H(*),ZF(*)

      ! Internals
      INTEGER :: TRS,CULV !iterators
      !Calculate the base flows, and then scale, ramp, and apply them.
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: QG_BASE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: QP_BASE
      DOUBLE PRECISION :: T_AREA,S_AREA ! Turbine and sluice areas
      DOUBLE PRECISION :: T_SF,PHASE_TIME ! Turbine scale factor (-), Phase time
      DOUBLE PRECISION :: IS_PUMP, IS_TURB ! Remove flows if not a pump or turbine at point
      DOUBLE PRECISION :: Q,Q_OLD ! Flow before relaxation
      ! DOUBLE PRECISION :: I_RAMP  ! The ramp of the current TRS

      IF (.NOT.ALLOCATED(QG_BASE)) THEN
        ALLOCATE(QG_BASE(NUM_TRS))
      ENDIF

      IF (.NOT.ALLOCATED(QP_BASE)) THEN
        ALLOCATE(QP_BASE(NUM_TRS))
      ENDIF

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

!     This is the trickiest sub so far... probably need to have the double
!     select case to do all the types of culvert and modes individually...

!     Iterate through the culverts given
      DO CULV = 1,NBUSE
        Q = 0.0
!       TRS is stored in the CTRASH variable
        TRS = CTRASH(CULV)
!       Load the phase time of the current TRS
        PHASE_TIME = TRS_TIME - PREV_SWITCH(TRS)
!       CLP     Flow control type. Culvert type: 4 = control, 5 = Turbine, 6 = Sluice, [0,1,2,3] as before
!       CV5     Turbine type: 1 = Turb+Pump, 2 = Turb Only, 3 = Pump Only
        IF (CV5(CULV).EQ.1) THEN ! Turbine that pumps
          IS_PUMP = 1.0
          IS_TURB = 1.0
        ELSEIF (CV5(CULV).EQ.2) THEN ! Turbine only, no pumping here
          IS_PUMP = 0.0
          IS_TURB = 1.0
        ELSEIF (CV5(CULV).EQ.3) THEN ! Pump only
          IS_PUMP = 1.0
          IS_TURB = 0.0
        ELSE ! Assume both...
          IS_PUMP = 1.0
          IS_TURB = 1.0
        ENDIF

        SELECT CASE (CLPBUS(CULV))
          CASE(4) ! Control point
            Q = 0.0 ! therefore no flow

          CASE(5) ! Turbine
            T_AREA = 0.25*3.1415926*LRG(CULV)*LRG(CULV)        ! Turbine area (m2)
            T_SF = (LRG(CULV)**2)/(ORIG_DIAM_T(TRS)**2) ! Turbine scale factor
            ! Ramp * Scale Factor * Flow
            SELECT CASE (MODE(TRS))
              CASE (0)  ! Initial warmup
              !DOUBLE PRECISION FUNCTION TRS_ORIFICE(CD,AREA,HDIFF) RESULT(FLOW)
                Q = RAMP(TRS)*
     &            ORIFICE(CE1(CULV),T_AREA,HEADDIFF(TRS))

              CASE (1)  ! High water hold
                ! Ramp down turbine flow if appropriate
                IF ((PHASE_TIME).LT.RAMPTIME(TRS)) THEN
                  IF (ISPUMPING(TRS)) THEN
                    Q = (1.0 - RAMP(TRS))*QP_BASE(TRS)
     &                *T_SF*CS2(CULV)*IS_PUMP
                  ELSE
                    IF(C56(CULV).EQ.1) THEN
                      Q = (1.0 - RAMP(TRS))*QG_BASE(TRS)
     &                  *T_SF*CE2(CULV)*IS_TURB
                    ELSE
                      Q = 0.0
                    ENDIF
                  ENDIF
                ELSE
                  Q = 0.0
                ENDIF

              CASE (2)  ! Ebb generation
                Q = RAMP(TRS)*QG_BASE(TRS)
     &            *T_SF*CE1(CULV)*IS_TURB ! Normal flow

              CASE (3)  ! Ebb sluicing
                IF (C56(CULV).EQ.1) THEN
                  Q = RAMP(TRS)*QG_BASE(TRS)
     &              *T_SF*CE1(CULV)*IS_TURB
                ELSE
                  ! ramp down into 0.0
                  Q = (1.0 - RAMP(TRS))*QG_BASE(TRS)
     &              *T_SF*CE1(CULV)*IS_TURB
                ENDIF

              CASE (-1) ! Ebb pumping
                Q = RAMP(TRS)*QP_BASE(TRS)
     &            *T_SF*CS1(CULV)*IS_PUMP
              CASE (4)  ! Low water hold
              ! Ramp down turbine flow if appropriate
              IF ((PHASE_TIME).LT.RAMPTIME(TRS)) THEN
                IF (ISPUMPING(TRS)) THEN
                  Q = (1.0 - RAMP(TRS))*QP_BASE(TRS)
     &              *T_SF*CS1(CULV)*IS_PUMP
                ELSE
                  IF(C56(CULV).EQ.1) THEN
                    Q = (1.0 - RAMP(TRS))*QG_BASE(TRS)
     &                  *T_SF*CE1(CULV)*IS_TURB
                  ELSE
                    Q = 0.0
                  ENDIF
                ENDIF
              ELSE
                Q = 0.0
              ENDIF

              CASE (5)  ! Flood generation
                Q = RAMP(TRS)*QG_BASE(TRS)
     &              *T_SF*CE2(CULV)*IS_TURB

              CASE (6)  ! Flood sluicing
                IF (C56(CULV).EQ.1) THEN
                  Q = RAMP(TRS)*QG_BASE(TRS)
     &              *T_SF*CE2(CULV)*IS_TURB
                ELSE
                  ! ramp down into 0.0
                  Q = (1.0 - RAMP(TRS))*QG_BASE(TRS)
     &              *T_SF*CE2(CULV)*IS_TURB
                ENDIF

              CASE (-2) ! Flood pumping
                Q = RAMP(TRS)*QG_BASE(TRS)
     &              *T_SF*CS2(CULV)*IS_PUMP

              CASE DEFAULT ! Error
              WRITE(*,*)'SOME KIND OF TRS MODE ERROR IN TURB FLOW CALC'

            END SELECT

          CASE(6) ! Sluice
          ! Determine sluice area based on shape
            IF (CIRC(CULV).EQ.1) THEN
              S_AREA = 0.25*3.1415926*LRG(CULV)*LRG(CULV)        ! Sluice area (m2)
            ELSEIF (CIRC(CULV).EQ.0) THEN
!             ZF = ELEVATION OF BOTTOM
!             Assume the area in the sluice flow is the Base * mean WL
              S_AREA = LRG(CULV)*
     &          (WL_IN(TRS) + WL_OUT(TRS)/2 -
     &          (P_MIN(ZF) + P_MAX(ZF)))
            ELSE
              ! You can add more area calc options here if desired
              WRITE(*,*) 'SLUICE AREA ERROR AT CULV:',CULV
              S_AREA = 0.0
            ENDIF
            ! Orifice equation correctly constructed.
            SELECT CASE (MODE(TRS))
              CASE (0)  ! Initial warmup, slow ramp up to get motion etc.
                Q = RAMP(TRS)*
     &            ORIFICE(2.0,T_AREA,HEADDIFF(TRS))

              CASE (1)  ! High water hold
                ! Ramp down as the sluices close
                IF ((TRS_TIME-PREV_SWITCH(TRS))
     &                            .LT.RAMPTIME(TRS)) THEN
                  IF (ISPUMPING(TRS)) THEN
                    ! for simplicity no sluice flow when pumping
                    Q = 0.0
                  ELSE
                    ! ramp down
                    Q = (1.0 - RAMP(TRS))*
     &                ORIFICE(CEBUS(2,CULV),S_AREA,HEADDIFF(TRS))
                  ENDIF
                ELSE
                  ! No flow, normal hold
                  Q = 0.0
                ENDIF

              CASE (2)  ! Ebb generation, no sluice flow by default
                Q = 0.0

              CASE (3)  ! Ebb sluicing, ramp up into normal
                Q = RAMP(TRS)*
     &              ORIFICE(CEBUS(1,CULV),S_AREA,HEADDIFF(TRS))

              CASE (-1) ! Ebb pumping
                Q = 0.0

              CASE (4)  ! Low water hold, same as high water hold
                IF ((TRS_TIME-PREV_SWITCH(TRS))
     &                            .LT.RAMPTIME(TRS)) THEN
                  IF (ISPUMPING(TRS)) THEN
                    Q = 0.0
                  ELSE
                    Q = (1.0 - RAMP(TRS))*
     &                ORIFICE(CEBUS(1,CULV),S_AREA,HEADDIFF(TRS))
                  ENDIF
                ELSE
                  Q = 0.0
                ENDIF

              CASE (5)  ! Flood generation, no sluice flow by default
                  Q = 0.0

              CASE (6)  ! Flood sluicing, ramp up into normal
                Q = RAMP(TRS)*
     &              ORIFICE(CEBUS(2,CULV),S_AREA,HEADDIFF(TRS))

              CASE (-2) ! Flood pumping, no flow.
                Q = 0.0

              CASE DEFAULT ! you added a new mode somewhere and did not add it here..?
                WRITE(*,*) 'MODE ERROR', MODE(TRS),
     &                     ' IN SLUICE FLOW CALC CULV [',CULV,']'
            END SELECT

          CASE DEFAULT
            IF ((CLPBUS(CULV).GT.6).OR.(CLPBUS(CULV).LE.0)) THEN
              ! Some kind of error
              WRITE(*,*)'INVALID CULVERT (CLP) VALUE [',
     &                   CLPBUS(CULV),']'
            ELSE
              ! Culvert operates as usual, copy the normal buse functionality into here?
            ENDIF
        END SELECT
        ! Apply the calculated flow to the culvert with relaxation
        Q_OLD = DBUS(CULV)
        DBUS(CULV)= (1.D0-RELAXB)*Q_OLD + RELAXB*Q
      END DO
      END SUBROUTINE TRS_FLOWS

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Initialise the arrays for the number of trs schemes when passed the
!     number of schemes.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! !      SUBROUTINE TRS_INIT_NUM(NUMBER_OF_SCHEMES)
! !      INTEGER :: NUMBER_OF_SCHEMES
! !      NUM_TRS = NUMBER_OF_SCHEMES
! !
! !
! !      END SUBROUTINE TRS_INIT_NUM
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Calculate the power generated or consumed at each culvert
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_POWER
     &(NM_TRS,NBS,
     & HDDFF,
     & MD, TRB_CHRT,
     & PMP_CHRT,
     & W_OUT,W_IN,
     & Q_TRB,Q_SLC,
     & PWR,PMP_TRG,
     & HD_G,FLW_G,
     & HD_P,FLW_P,
     & DBS,CTRSH,
     & HH,ZZF
     &)
      
      INTEGER :: TRS,CULV
      DOUBLE PRECISION :: IS_PUMP,IS_TURB
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: P_BASE_P,P_BASE_G

      IF (.NOT.ALLOCATED(P_BASE_G)) THEN
        ALLOCATE(P_BASE_G(NUM_TRS))
      ENDIF
      IF (.NOT.ALLOCATED(P_BASE_P)) THEN
        ALLOCATE(P_BASE_P(NUM_TRS))
      ENDIF

      ! BASE POWERS
      DO TRS = 1,NUM_TRS
        ! Clear the variable
        POWER(TRS) = 0.0
        ! Base generator powers
        P_BASE_G(TRS) = TRS_CALC(
     &    HEADDIFF(TRS),
     &    HEAD_G(TRS,:),
     &    POWER_G(TRS,TURB_CHART(TRS),:))
        ! Base Pumping powers
        P_BASE_P(TRS) = TRS_CALC(
     &    HEADDIFF(TRS),
     &    HEAD_P(TRS,:),
     &    POWER_P(TRS,PUMP_CHART(TRS),:))
      END DO

!     Iterate through the culverts given
      DO CULV = 1,NBUSE
!       TRS is stored in the CTRASH variable
        TRS = CTRASH(CULV)
!       Load the phase time of the current TRS
        PHASE_TIME = TRS_TIME - PREV_SWITCH(TRS)
!       CLP     Flow control type. Culvert type: 4 = control, 5 = Turbine, 6 = Sluice, [0,1,2,3] as before
!       CV5     Turbine type: 1 = Turb+Pump, 2 = Turb Only, 3 = Pump Only
        IF (CLPBUS(CULV).EQ.5) THEN
        SELECT CASE (MODE(TRS))
          CASE (0)  ! Warmup
            SELECT CASE (CV5(CULV))
              CASE (1)
                POWER(TRS) = POWER(TRS) + 0.0
              CASE (2)
                POWER(TRS) = POWER(TRS) + 0.0
              CASE (3)
                POWER(TRS) = POWER(TRS) + 0.0
              CASE DEFAULT
              WRITE(*,*)'INVALID CV5 PUSHED TO POWER CALC',
     &                  ' CULV [',CULV,']'
            END SELECT

          CASE (1)  ! High water Hold
            SELECT CASE (CV5(CULV))
              CASE (1)
                ! Ramp down from pumping power
                IF (ISPUMPING(TRS)) THEN
                  POWER(TRS) = POWER(TRS) - (1 - RAMP(TRS))*
     &               P_BASE_P(TRS)*CSBUS(2,CULV)
                ELSE
                  ! IF Parallel sluice -> so close to level that we call it 0
                  POWER(TRS) = POWER(TRS) + 0.0
                ENDIF

              CASE (2)
                ! IF Parallel sluice -> so close to level that we call it 0
                POWER(TRS) = POWER(TRS) + 0.0
              CASE (3)
              ! Ramp down from pumping power
                IF (ISPUMPING(TRS)) THEN
                  POWER(TRS) = POWER(TRS) - (1 - RAMP(TRS))*
     &               P_BASE_P(TRS)*CSBUS(2,CULV)
                ENDIF

              CASE DEFAULT
              WRITE(*,*)'INVALID CV5 PUSHED TO POWER CALC',
     &                  ' CULV [',CULV,']'
            END SELECT

          CASE (2)  ! Ebb Generation
            SELECT CASE (CV5(CULV))
              CASE (1)
                POWER(TRS) = POWER(TRS) + RAMP(TRS)*
     &            P_BASE_G(TRS)*CEBUS(1,CULV)
              CASE (2)
                POWER(TRS) = POWER(TRS) + RAMP(TRS)*
     &            P_BASE_G(TRS)*CEBUS(1,CULV)
              CASE (3)
                POWER(TRS) = POWER(TRS) + 0.0
              CASE DEFAULT
              WRITE(*,*)'INVALID CV5 PUSHED TO POWER CALC',
     &                  ' CULV [',CULV,']'
            END SELECT

          CASE (3)  ! Ebb sluicing
            SELECT CASE (CV5(CULV))
              CASE (1)
                IF(C56(CULV).EQ.1) THEN
                  POWER(TRS) = POWER(TRS) + RAMP(TRS)*
     &              P_BASE_G(TRS)*CEBUS(1,CULV)
                ENDIF
              CASE (2)
                IF(C56(CULV).EQ.1) THEN
                  POWER(TRS) = POWER(TRS) + RAMP(TRS)*
     &              P_BASE_G(TRS)*CEBUS(1,CULV)
                ENDIF
              CASE (3)
                POWER(TRS) = POWER(TRS) + 0.0
              CASE DEFAULT
              WRITE(*,*)'INVALID CV5 PUSHED TO POWER CALC',
     &                  ' CULV [',CULV,']'
            END SELECT

          CASE (-1) ! Ebb pumping
            SELECT CASE (CV5(CULV))
              CASE (1)
                IF (ISPUMPING(TRS)) THEN
                  POWER(TRS) = POWER(TRS) - RAMP(TRS)*
     &               P_BASE_P(TRS)*CSBUS(1,CULV)
                ENDIF
              CASE (2)
                POWER(TRS) = POWER(TRS) + 0.0
              CASE (3)
                IF (ISPUMPING(TRS)) THEN
                  POWER(TRS) = POWER(TRS) - RAMP(TRS)*
     &               P_BASE_P(TRS)*CSBUS(1,CULV)
                ENDIF
              CASE DEFAULT
              WRITE(*,*)'INVALID CV5 PUSHED TO POWER CALC',
     &                  ' CULV [',CULV,']'
            END SELECT

          CASE (4) ! Low water hold
            SELECT CASE (CV5(CULV))
              CASE (1)
                ! Ramp down from pumping power
                IF (ISPUMPING(TRS)) THEN
                  POWER(TRS) = POWER(TRS) - (1 - RAMP(TRS))*
     &               P_BASE_P(TRS)*CSBUS(1,CULV)
                ELSE
                  ! IF Parallel sluice -> so close to level that we call it 0
                  POWER(TRS) = POWER(TRS) + 0.0
                ENDIF

              CASE (2)
                ! IF Parallel sluice -> so close to level that we call it 0
                POWER(TRS) = POWER(TRS) + 0.0
              CASE (3)
              ! Ramp down from pumping power
                IF (ISPUMPING(TRS)) THEN
                  POWER(TRS) = POWER(TRS) - (1 - RAMP(TRS))*
     &               P_BASE_P(TRS)*CSBUS(1,CULV)
                ENDIF
              CASE DEFAULT
              WRITE(*,*)'INVALID CV5 PUSHED TO POWER CALC',
     &                  ' CULV [',CULV,']'
            END SELECT

          CASE (5)  ! Flood Generation
            SELECT CASE (CV5(CULV))
              CASE (1)
                POWER(TRS) = POWER(TRS) + RAMP(TRS)*
     &            P_BASE_G(TRS)*CEBUS(2,CULV)
              CASE (2)
                POWER(TRS) = POWER(TRS) + RAMP(TRS)*
     &            P_BASE_G(TRS)*CEBUS(2,CULV)
              CASE (3)
                POWER(TRS) = POWER(TRS) + 0.0
              CASE DEFAULT
              WRITE(*,*)'INVALID CV5 PUSHED TO POWER CALC',
     &                  ' CULV [',CULV,']'
            END SELECT

          CASE (6)  ! Flood sluicing
            SELECT CASE (CV5(CULV))
              CASE (1)
                IF(C56(CULV).EQ.1) THEN
                  POWER(TRS) = POWER(TRS) + RAMP(TRS)*
     &              P_BASE_G(TRS)*CEBUS(2,CULV)
                ENDIF
              CASE (2)
                IF(C56(CULV).EQ.1) THEN
                  POWER(TRS) = POWER(TRS) + RAMP(TRS)*
     &              P_BASE_G(TRS)*CEBUS(2,CULV)
                ENDIF
              CASE (3)
                POWER(TRS) = POWER(TRS) + 0.0
              CASE DEFAULT
              WRITE(*,*)'INVALID CV5 PUSHED TO POWER CALC',
     &                  ' CULV [',CULV,']'
            END SELECT

          CASE (-2) ! Flood pumping
            SELECT CASE (CV5(CULV))
              CASE (1)
                IF (ISPUMPING(TRS)) THEN
                  POWER(TRS) = POWER(TRS) - RAMP(TRS)*
     &               P_BASE_P(TRS)*CSBUS(2,CULV)
                ENDIF
              CASE (2)
                POWER(TRS) = POWER(TRS) + 0.0
              CASE (3)
                IF (ISPUMPING(TRS)) THEN
                  POWER(TRS) = POWER(TRS) - RAMP(TRS)*
     &               P_BASE_P(TRS)*CSBUS(2,CULV)
                ENDIF
              CASE DEFAULT
              WRITE(*,*)'INVALID CV5 PUSHED TO POWER CALC',
     &                  ' CULV [',CULV,']'
            END SELECT

          CASE DEFAULT
            WRITE(*,*) 'INVALID MODE [',MODE(TRS),
     &                 '] IN POWER CALC.'
        END SELECT
      ENDIF
      ENDDO

      END SUBROUTINE TRS_POWER

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Go through the schemes and use the weighted control points to
!     calculate an up and downstream level for the scheme
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_W_LEVELS
!      TRS
     &(NUM_TRS,
     & WL_IN,WL_OUT,HEADDIFF,
!      BUSE
     & NBUSE,ENTBUS,SORBUS,
     & H,ZF)

!     TRS
      INTEGER,           INTENT(IN) :: NUM_TRS
      DOUBLE PRECISION,DIMENSION(:),INTENT(OUT) :: WL_IN,WL_OUT
      DOUBLE PRECISION,DIMENSION(:),INTENT(OUT) :: HEADDIFF
!     BUSE
      INTEGER          , INTENT(IN)    :: NBUSE
      INTEGER          , INTENT(IN)    :: ENTBUS(NBUSE)
      INTEGER          , INTENT(IN)    :: SORBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: H(*),ZF(*)
      DOUBLE PRECISION , INTENT(IN)    :: CTRASH(NBUSE)

      INTEGER :: CULV, TRS, I1, I2
      DOUBLE PRECISION :: TMP_IN, TMP_OUT, TMP_W_IN, TMP_W_OUT
      DOUBLE PRECISION :: SUM_IN,SUM_OUT,SUM_W

      ! FOR EVERY SCHEME
      DO TRS  = 1,NUM_TRS
        WL_IN(TRS) = 0.0
        WL_OUT(TRS) = 0.0
        SUM_W = 0.0

        ! FOR EVERY CULVERT
        DO CULV = 1,NBUSE
          ! IF THIS CULVERT IS FOR THIS SCHEME
          IF (CTRASH(CULV).EQ.TRS) THEN
            TMP_IN = H(I1)+ZF(I2)
            TMP_OUT = H(I2)+ZF(I2)
            ! IF IN PARALLEL USE THE P_MIN/P_MAX METHOD
            IF (NCSIZE.GT.1) THEN
              TMP_IN = P_MIN(TMP_IN) + P_MAX(TMP_IN)
              TMP_IN = P_MIN(TMP_OUT) + P_MAX(TMP_OUT)
            ENDIF
            ! CUMULATIVE TOTALS
            SUM_IN = SUM_IN + TMP_IN*CV(CULV)
            SUM_OUT = SUM_OUT + TMP_OUT*CV(CULV)
            SUM_W = SUM_W + CV(CULV)
          ENDIF
        ENDDO
        ! WEIGHTED AVERAGE
        WL_IN(TRS) = SUM_IN/SUM_W
        WL_OUT(TRS) = SUM_OUT/SUM_W
        HEADDIFF(TRS) = WL_IN(TRS) - WL_OUT(TRS)
      ENDDO
      END SUBROUTINE TRS_W_LEVELS
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     COLLATE THE FLOW RATES THROUGH THE TRS
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_COLLATE_Q()
      INTEGER :: CULV, TRS

      DO TRS = 1,NUM_TRS
        Q_TURB(TRS) = 0.0
        Q_SLUICE(TRS) = 0.0
        DO CULV= 1,NBUSE
          IF (CTRASH(CULV).EQ.TRS) THEN
            SELECT CASE (CLPBUS(CULV))
              CASE (5) ! TURBINE or PUMP
                Q_TURB(TRS) = Q_TURB(TRS) + DBUS(CULV)
              CASE (6) ! SLUICE
                Q_SLUICE(TRS) = Q_SLUICE(TRS) + DBUS(CULV)
              CASE DEFAULT ! NORMAL or CONTROL
            END SELECT
          ENDIF
        ENDDO
      ENDDO
      END SUBROUTINE TRS_COLLATE_Q
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     TRS wrapping function, can be called externally and should run
!     all the parts.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
     !  SUBROUTINE TRS
     ! &(RELAXB,NBUSE,ENTBUS,SORBUS,GRAV,
     ! & H,ZF,DBUS,LRGBUS,HAUBUS,CLPBUS,
     ! & ALTBUS,CSBUS,CEBUS,ANGBUS,LBUS,
     ! & NTRAC,T,TBUS,UBUS,VBUS,U,V,ENTET,
     ! & CV,C56,CV5,C5,CTRASH,FRICBUS,LONGBUS,
     ! & CIRC,DELBUS,OPTBUSE,V2DPAR,DT,SECBUS,MAXSOURCE,
     ! & NPTSCE,NPOIN2,KSCE)
     !
     !
     !  END SUBROUTINE TRS

      END MODULE TRS_T2D

!     End of TRS_T2D

      PROGRAM TEST_TRS_T2D
      USE TRS_T2D
      INTEGER :: NFO1

!      print *, 'linear interpolate 5,0,10,5,7 = ',
!     &TRS_INTERP(5.0,0.0,10.0,5.0,7.0)
!
      WRITE(*,*) 'Program started...'
      ! NFO1 = 101
      !
      ! CALL TRS_READ_DATAFILES(NFO1)
      ! CALL TRS_FLEX_VALUES(30.0)
      ! CALL TRS_NEW_MODE(31.0)
      !
      ! DO I=1,NUM_TRS ! This absolute madlad used an implicit (or did it just pull from TRS..?)
      !   WRITE(*,*) 'TRS[',I,'] - Mode = ',MODE(I)
      ! ENDDO
      !
      ! CALL TRS_WRITE_RESULTS(102)
      ! TRS_TIME = TRS_TIME + 1
      ! CALL TRS_WRITE_RESULTS(102)
      ! TRS_TIME = TRS_TIME + 1
      ! CALL TRS_WRITE_RESULTS(102)
      ! CALL TRS_WRITE_STATUS()
      !CALL
      !DO I=1,NUM_TRS
      !  WRITE(*,*)
      !ENDDO
      END PROGRAM TEST_TRS_T2D
