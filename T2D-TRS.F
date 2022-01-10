!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     TRS_T2D
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
                                          !false => print with 'listing period'

!     TEMPORAL CONTROLS - All in hours
      REAL,SAVE :: RAMPTIME    = 0.0      !How long to ramp between modes
      REAL,SAVE :: PREV_SWITCH = 0.0      !Time when lagoon last switched modes
      REAL,SAVE :: MAX_GEN     = 24.0     !Maxmimum time before generation is auto ended
      REAL,SAVE :: MAX_HOLD    = 24.0     !Maxmimum time before holding is auto ended
      REAL,SAVE :: MAX_SLUICE  = 24.0     !Maxmimum time before sluicing is auto ended
      REAL,SAVE :: MAX_PUMP    = 24.0     !Maxmimum time before pumping is auto ended

!     OPERATION CONFIGURATION
      LOGICAL,SAVE :: IS2WAY = .FALSE.            !Is the scheme in two way operation
      LOGICAL,SAVE :: ISFLEXIBLE = .FALSE.        !Is the scheme in flexible operation
      LOGICAL,SAVE :: ISPUMPING = .FALSE.         !Is the scheme using pumping
      LOGICAL,SAVE :: PUMPTOLEVEL = .FALSE.       !Pump to level or head difference
      LOGICAL,SAVE :: ISPARALLELSLUICE = .TRUE.   !Is parallel sluicing being used
      INTEGER,SAVE :: MODE = 0                    !What operational mode is the scheme in
      INTEGER,SAVE :: MODE_PREV = 0               !What mode was the scheme in prevously

!
!     WATER LEVELS - Meters
      REAL,SAVE :: WL_IN,WL_OUT   !Water levels inside and outside, from control points
      REAL,SAVE :: HEADDIFF       !Head difference = Inside - Outside

!     DIMENSIONLESS
      REAL,SAVE :: RAMP           !Ramp Coefficient when changing modes

!     HILL CHART PARAMETERS - Note that shared head levels are required for powers/flows
      INTEGER,SAVE :: N_HILLS_G = 1,N_HILLS_P = 1     !Number of hill charts for (_G)eneration and (_P)umping
      INTEGER,SAVE :: LEN_HILLS_G = 1,LEN_HILLS_P = 1 !Length of hillcharts for (_G)eneration and (_P)umping
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: HEAD_G,HEAD_P     !Head levels for the hill charts
      REAL,DIMENSION(:,:),ALLOCATABLE,SAVE :: POWER_G,FLOW_G  !Power/Flow for Generating
      REAL,DIMENSION(:,:),ALLOCATABLE,SAVE :: POWER_P,FLOW_P  !Power/Flow for Pumping

!     Arrays and values for flexible operation
!     Fixed operation is treated as flexible operation with one long period
      INTEGER,SAVE  :: N_FLEX = 1
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: FLX_TIMES
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: H_STARTS,H_ENDS,TURB_CHARTS
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: PUMP_TARGS,PUMP_CHARTS
      REAL,SAVE :: H_START = 1.0, H_END = 0.5     !Head to start/end generation
      REAL,SAVE :: TURB_CHART = 1, PUMP_CHART = 1 !Which turbine/pump chart to use
      REAL,SAVE :: PUMP_TARG = 0.0                !Target to pump to

!     TEMPORARY VARIABLES
      REAL :: TMP_REAL = 0
      INTEGER :: TMP_INT = 0.0
      INTEGER :: i,j,k
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
!   TRS_CALC
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
!   TRS_FLEXVALUES
!   Sets the control parameters based on the time and the flex control
!   definition
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_FLEX_VALUES(CT)
      REAL,INTENT(IN) :: CT
      INTEGER :: I

      IF (CT.LT.FLX_TIMES(1)) THEN
        !WARMUP PERIOD
        H_START = 1.0E16 !Do not enter generation
        H_END = 0.0
        TURB_CHART = 1
        PUMP_TARG = 0.0
        PUMP_CHART = 1
        RETURN
      ELSEIF (CT.GE.FLX_TIMES(N_FLEX)) THEN
        H_START = H_STARTS(N_FLEX)
        H_END = H_ENDS(N_FLEX)
        TURB_CHART = TURB_CHARTS(N_FLEX)
        PUMP_TARG = PUMP_TARGS(N_FLEX)
        PUMP_CHART = PUMP_CHARTS(N_FLEX)
        RETURN
      ELSE
        DO I = 2,N_FLEX
          IF ((CT.GE.FLX_TIMES(I-1))
     &      .AND.(CT.LT.FLX_TIMES(I))) THEN
            H_START = H_STARTS(I-1)
            H_END = H_ENDS(I-1)
            TURB_CHART = TURB_CHARTS(I-1)
            PUMP_TARG = PUMP_TARGS(I-1)
            PUMP_CHART = PUMP_CHARTS(I-1)
            RETURN
          ENDIF
        ENDDO
      ENDIF

      END SUBROUTINE TRS_FLEX_VALUES

      SUBROUTINE TRS_NEW_MODE(CT)

      REAL,INTENT(IN) :: CT!, TRS_RAMP
      REAL :: PHASE_T, HD

      HD = ABS(HEADDIFF)
      PHASE_T = CT - PREV_SWITCH ! Initialise with prior phase time

      IF (PHASE_T.GE.RAMPTIME) THEN !If we have completed starting operation of this phase
        SELECT CASE(MODE)
          CASE (0)  !      0 : Initial sluicing warmup
            IF (CT.GE.FLX_TIMES(1)) THEN
              MODE = 1  !      1 : High water holding
              PREV_SWITCH = CT
            ENDIF
          CASE (1)  !      1 : High water holding
            IF ((HD.GE.H_START)
     &      .OR.(PHASE_T.GT.MAX_HOLD)) THEN
              MODE = 2  !      2 : Ebb generation
              PREV_SWITCH = CT
            ENDIF
          CASE (2)  !      2 : Ebb generation
            IF ((HD.LE.H_END)
     &      .OR.(PHASE_T.GT.MAX_GEN)) THEN
              IF (IS2WAY) THEN
                MODE = 3  !      3 : Ebb sluicing
              ELSE
                MODE = 4  !      4 : Low water holding
              ENDIF
              PREV_SWITCH = CT
            ENDIF
          CASE (3)  !      3 : Ebb sluicing
            IF ((HD.LE.0.1)
     &      .OR.(PHASE_T.GT.MAX_SLUICE)) THEN
              IF (ISPUMPING.AND.IS2WAY) THEN
                MODE = -1 !     -1 : Ebb pumping
              ELSE
                MODE = 4  !      4 : Low water holding
              ENDIF
              PREV_SWITCH = CT
            ENDIF
          CASE (-1) !     -1 : Ebb pumping
            IF ((PUMPTOLEVEL.AND.(WL_IN.LE.PUMP_TARG))
     &      .OR.(.NOT.(PUMPTOLEVEL).AND.(HD.GE.PUMP_TARG))
     &      .OR.(PHASE_T.GE.MAX_PUMP)) THEN
              MODE = 4  !      4 : Low water holding
              PREV_SWITCH = CT
            ENDIF
          CASE (4)  !      4 : Low water holding
            IF (IS2WAY) THEN
              IF ((HD.GE.H_START)
     &      .OR.(PHASE_T.GT.MAX_HOLD)) THEN
                MODE = 5 !      5 : Flood generation
                PREV_SWITCH = CT
              ENDIF
            ELSE !      5 : Flood generation
              IF((HEADDIFF.GT.0.1)
     &      .OR.(PHASE_T.GT.MAX_HOLD)) THEN
                MODE = 6  !      6 : Flood sluicing
                PREV_SWITCH = CT
              ENDIF
            ENDIF
          CASE (5)  !      5 : Flood generation
            IF ((HD.LE.H_END)
     &      .OR.(PHASE_T.GT.MAX_HOLD)) THEN
              MODE = 2  !      2 : Ebb generation
              PREV_SWITCH = CT
            ENDIF
          CASE (6)  !      6 : Flood sluicing
            IF ((HD.LE.0.1)
     &      .OR.(PHASE_T.GT.MAX_SLUICE)) THEN
              IF (ISPUMPING) THEN
                MODE = -2 !     -2 : FLood pumping
              ELSE
                MODE = 1  !      1 : High water holding
              ENDIF
              PREV_SWITCH = CT
            ENDIF
          CASE (-2) !     -2 : FLood pumping
          IF ((PUMPTOLEVEL.AND.(WL_IN.GE.PUMP_TARG))
     &      .OR.(.NOT.(PUMPTOLEVEL).AND.(HD.GE.PUMP_TARG))
     &      .OR.(PHASE_T.GE.MAX_PUMP)) THEN
              MODE = 1  !      1 : High water holding
              PREV_SWITCH = CT
            ENDIF
        END SELECT
!       UPDATE PHASE TIMING
        PHASE_T = CT - PREV_SWITCH
      ENDIF
      RAMP = TRS_RAMP(RAMPTIME,PHASE_T)
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
!
      SUBROUTINE TRS_READ_DATAFILE(NF_TRS)
      !File with the tidal range scheme control parameters
!      NFO1=T2D_FILES(T2DFO1)%LU
      INTEGER,INTENT(IN) :: NF_TRS
      CHARACTER(LEN=100) :: LINE,KEYWORD,VARIABLE

      OPEN(UNIT = NF_TRS,FILE="TRS-Control.txt")
C      REWIND NF_TRS

      IF (FIRST) THEN
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
        WRITE(*,*) "               TRS-Control Opened"
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
        READ(NF_TRS,*) LINE !Header line

        READ(NF_TRS,'(A100)') LINE
        DO WHILE((LINE(1:3).NE.'END').AND.
     &           (LINE(1:3).NE.'End').AND.
     &           (LINE(1:3).NE.'end'))
!          WRITE(*,*) LINE
          CALL TRS_PARSE(LINE,KEYWORD,VARIABLE)

          SELECT CASE(TO_UPPER(TRIM(ADJUSTL(KEYWORD))))  ! CASE SENSEITIVE (May add lower case options later)
            CASE('RAMPTIME')  ! Hours
              READ(VARIABLE,*) RAMPTIME
            CASE('PREV_SWITCH') ! Hours in model
              READ(VARIABLE,*) PREV_SWITCH
            CASE('MAX_GEN') ! Hours
              READ(VARIABLE,*) MAX_GEN
            CASE('MAX_HOLD') !HOURS
              READ(VARIABLE,*) MAX_HOLD
            CASE('MAX_SLUICE') ! Hours
              READ(VARIABLE,*) MAX_SLUICE
            CASE('MAX_PUMP') ! Hours
              READ(VARIABLE,*) MAX_PUMP
            CASE('IS2WAY') ! Logical (1 or 0, I think)
              READ(VARIABLE,*) IS2WAY
            CASE('ISFLEXIBLE') ! Logical (1 or 0, I think)
              READ(VARIABLE,*) ISFLEXIBLE
            CASE('ISPUMPING') ! Logical (1 or 0, I think)
              READ(VARIABLE,*) ISPUMPING
            CASE('ISPARALLELSLUICE') ! Logical (1 or 0, I think)
              READ(VARIABLE,*) ISPARALLELSLUICE
            CASE('MODE')  ! Integer mode code
              READ(VARIABLE,*) MODE
            CASE('N_HILLS_G')  ! Integer
              READ(VARIABLE,*) N_HILLS_G
            CASE('N_HILLS_P')  ! Integer
              READ(VARIABLE,*) N_HILLS_P
            CASE('LEN_HILLS_G')  ! Integer
              READ(VARIABLE,*) LEN_HILLS_G
            CASE('LEN_HILLS_P')  ! Integer
              READ(VARIABLE,*) LEN_HILLS_P
            CASE('N_FLEX')  ! Integer
              READ(VARIABLE,*) N_FLEX
            CASE('PUMPTOLEVEL')  ! Integer
              READ(VARIABLE,*) PUMPTOLEVEL
            CASE('MODE_PREV') ! Integer (Don't think it does anything)
              READ(VARIABLE,*) MODE_PREV
            CASE('GENERATOR')
              WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)

            CASE('PUMP')
              WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)

            CASE('FLEX')
              WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)

            CASE DEFAULT ! Oh no, something must have gone wrong
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
        WRITE(*,*) ' Invalid Parameter [',TRIM(KEYWORD),']'
        WRITE(*,*) ' Value possibly of [',TRIM(VARIABLE),']'
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
          END SELECT

          WRITE(*,*) TRIM(ADJUSTL(KEYWORD)),' = ',TRIM(VARIABLE)
          READ(NF_TRS,'(A100)') LINE
        END DO

        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
        WRITE(*,*) "  Done reading TRS Control File."
        WRITE(*,*) '+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
      ENDIF

      CLOSE(NF_TRS)
      END SUBROUTINE TRS_READ_DATAFILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Write the headers and initial row of the output CSV
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_INIT_RESULTS()

      END SUBROUTINE TRS_INIT_RESULTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Write the next row of the output CSV
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_WRITE_STATUS()

      END SUBROUTINE TRS_WRITE_STATUS

      END MODULE TRS_T2D

!     End of TRS_T2D

      PROGRAM TEST_TRS_T2D
      USE TRS_T2D
      INTEGER NFO1

C      print *, 'linear interpolate 5,0,10,5,7 = ',
C     &TRS_INTERP(5.0,0.0,10.0,5.0,7.0)
C
      NFO1 = 101
      CALL TRS_READ_DATAFILE(NFO1)


      END PROGRAM TEST_TRS_T2D
