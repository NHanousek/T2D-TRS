!!!
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
!     TEMPORAL CONTROLS
      REAL,SAVE :: RAMPTIME    = 0.0      !How long to ramp between modes
      REAL,SAVE :: PREV_SWITCH = 0.0      !Time when lagoon last switched modes
      REAL,SAVE :: MAX_GEN     = 24.0     !Maxmimum time before generation is auto ended
      REAL,SAVE :: MAX_HOLD    = 24.0     !Maxmimum time before holding is auto ended
      REAL,SAVE :: MAX_SLUICE  = 24.0     !Maxmimum time before sluicing is auto ended
      REAL,SAVE :: MAX_PUMP    = 24.0     !Maxmimum time before pumping is auto ended

!     OPERATION CONFIGURATION
      LOGICAL,SAVE :: IS2WAY = .FALSE.      !Is the scheme in two way operation
      LOGICAL,SAVE :: ISFLEXIBLE = .FALSE.  !Is the scheme in flexible operation
      LOGICAL,SAVE :: ISPUMPING = .FALSE.   !Is the scheme using pumping
      LOGICAL,SAVE :: ISPARRALLELSLUICE = .FALSE. !Is parrallel sluicing being used
      INTEGER,SAVE :: MODE = 0              !What operational mode is the scheme in
!
!     PHYSICAL PARAMETERS
      REAL,SAVE :: WL_IN,WL_OUT   !Water levels inside and outside, from control points
      REAL,SAVE :: HEADDIFF,RAMP  !Head difference across the scheme = WL_in - WL_out

!     HILL CHART PARAMETERS
      INTEGER,SAVE :: N_HILLS_G,N_HILLS_P     !Number of hill charts for (_G)eneration and (_P)umping
      INTEGER,SAVE :: LEN_HILLS_G,LEN_HILLS_P !Length of hillcharts for (_G)eneration and (_P)umping
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: HEAD_G,HEAD_P !Head levels for the hill charts
      REAL,DIMENSION(:,:),ALLOCATABLE,SAVE :: POWER_G,FLOW_G,POWER_P,FLOW_P !Flow and power arrays

!     Arrays and values for flexible operation
      INTEGER,SAVE  :: N_FLEX
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: FLX_TIMES
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: H_STARTS,H_ENDS,TURB_CHARTS
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: PUMP_TARGS,PUMP_CHARTS
      REAL,SAVE :: H_START,H_END,TURB_CHART,PUMP_TARG,PUMP_CHART

!     TEMPORARY VARIABLES
      REAL :: TMP_REAL = 0
      INTEGER :: TMP_INT = 0.0

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
              IF ((HD.GE.HEADS(I-1)).AND.(HD.LE.HEADS(I))) THEN
                CALC = SIGN*TRS_INTERP(HD,HEADS(I-1),
     &HEADS(I),VALUES(I-1),VALUES(I))
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
!   TRS_FLEXV
!   Sets the control parameters based on the time and the flex control
!   definition
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_FLEXV(CT)
      REAL,INTENT(IN) :: CT
      INTEGER :: I

      IF (CT.LE.FLX_TIMES(1)) THEN
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
          IF ((CT.GE.FLX_TIMES(I-1)).AND.(CT.LT.FLX_TIMES(I))) THEN
            H_START = H_STARTS(I-1)
            H_END = H_ENDS(I-1)
            TURB_CHART = TURB_CHARTS(I-1)
            PUMP_TARG = PUMP_TARGS(I-1)
            PUMP_CHART = PUMP_CHARTS(I-1)
            RETURN
          ENDIF
        ENDDO
      ENDIF

      END SUBROUTINE TRS_FLEXV

      SUBROUTINE TRS_NEW_MODE(CT)
      REAL,INTENT(IN) :: CT!, TRS_RAMP
      REAL :: PHASE_T, HD

      HD = ABS(HEADDIFF)
      PHASE_T = CT - PREV_SWITCH ! Initialise with prior phase time

      IF (PHASE_T.LE.RAMPTIME) THEN
        SELECT CASE(MODE)
          CASE (0)  !      0 : Initial sluicing warmup
            IF (CT.GE.FLX_TIMES(1)) THEN
              MODE = 1
              PREV_SWITCH = CT
            ENDIF
          CASE (1)  !      1 : High water holding
            IF ((HD.GE.H_START).OR.(PHASE_T.GT.MAX_HOLD)) THEN
              MODE = 2
              PREV_SWITCH = CT
            ENDIF
          CASE (2)  !      2 : Ebb generation
            IF ((HD.LE.H_END).OR.(PHASE_T.GT.MAX_GEN)) THEN
              IF (IS2WAY) THEN
                MODE = 3
              ELSE
                MODE = 4
              ENDIF
              PREV_SWITCH = CT
            ENDIF
          CASE (3)  !      3 : Ebb sluicing
            IF ((HD.LE.0.1).OR.(PHASE_T.GT.MAX_SLUICE)) THEN
              IF (ISPUMPING.AND.IS2WAY) THEN
                MODE = -1
              ELSE
                MODE = 4
              ENDIF
              PREV_SWITCH = CT
            ENDIF
          CASE (-1) !     -1 : Ebb pumping
            IF ((WL_IN.LE.PUMP_TARG).OR.(PHASE_T.GE.MAX_PUMP)) THEN
              MODE = 4
              PREV_SWITCH = CT
            ENDIF
          CASE (4)  !      4 : Low water holding
            IF (IS2WAY) THEN
              IF ((HD.GE.H_START).OR.(PHASE_T.GT.MAX_HOLD)) THEN
                MODE = 5
                PREV_SWITCH = CT
              ENDIF
            ELSE !Ebb generation
              IF((HEADDIFF.LT.0.1).OR.PHASE_T.GT.MAX_HOLD) THEN
                MODE = 6
                PREV_SWITCH = CT
              ENDIF
            ENDIF
          CASE (5)  !      5 : Flood generation
            IF ((HD.LE.H_END).OR.(PHASE_T.GT.MAX_HOLD)) THEN
              MODE = 2
              PREV_SWITCH = CT
            ENDIF
          CASE (6)  !      6 : Flood sluicing
            IF ((HD.LE.0.1).OR.(PHASE_T.GT.MAX_SLUICE)) THEN
              IF (ISPUMPING) THEN
                MODE = -2
              ELSE
                MODE = 1
              ENDIF
              PREV_SWITCH = CT
            ENDIF
          CASE (-2) !     -2 : FLood pumping
            IF ((WL_IN.GE.PUMP_TARG).OR.(PHASE_T.GT.MAX_PUMP)) THEN
              MODE = 1
              PREV_SWITCH = CT
            ENDIF
        END SELECT
!       UPDATE PHASE TIMING
        PHASE_T = CT - PREV_SWITCH
      ENDIF
      RAMP = TRS_RAMP(RAMPTIME,PHASE_T)
      RETURN
      END SUBROUTINE TRS_NEW_MODE

      END MODULE TRS_T2D
      END
!     End of TRS_T2D
