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
!     Sluice flows:  Always match the sign of the head difference
!     Turbine flows: Always operate in assigned direction
!     Turbine power: +ve when generating, -ve when pumping
!     Ramp up during the start of generation/sluicing/pumping
!     Ramp down during start of holding from previous mode
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      MODULE TRS_T2D
      ! USE BIEF
      USE BIEF
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
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: PHASETIME
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: WARMUP

!     OPERATION CONFIGURATION
!     One for each Scheme
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: IS2WAY           !Is the scheme in two way operation
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: ISFLEXIBLE       !Is the scheme in flexible operation
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: ISPUMPING        !Is the scheme using pumping
      LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: PUMPTOLEVEL      !Pump to level or head difference
      ! LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: ISPARALLELSLUICE !Is parallel sluicing being used (DEPRECATED AS THIS IS NOW DECLARED AT CULVERT)
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: MODE             !What operational mode is the scheme in
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: MODE_FIRST       !The first mode to change to from warmup (default is 1 -> hold high)
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
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,SAVE :: FLEX_TIMES  ! Start time of each flex period (no overlap)
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,SAVE :: H_STARTS
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,SAVE :: H_ENDS ! Start and end piooints in those periods
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,SAVE :: PUMP_TARGS      ! and the _CHARTS indicates the chart to use
      INTEGER,DIMENSION(:,:),ALLOCATABLE,SAVE :: TURB_CHARTS ! turbines (generating)
      INTEGER,DIMENSION(:,:),ALLOCATABLE,SAVE :: PUMP_CHARTS ! and pumping
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE   :: PREV_I_FLEX_TIMES !Save the position of the iterator (can lead to large speedups)
!     These variables move as time passes based on when we are and which value to use
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: H_START, H_END          !Head to start/end generation
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: TURB_CHART
      INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: PUMP_CHART  !Which turbine/pump chart to use
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: PUMP_TARG               !Target to pump to
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: QP_BASE, QG_BASE

!     DIMENSIONLESS
!     One for each Scheme
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,SAVE :: RAMP           !Ramp Coefficient when changing modes

!     TEMPORARY VARIABLES
      DOUBLE PRECISION    :: TMP_DOUBLE = 0.0
      DOUBLE PRECISION    :: TMP_D1, TMP_D2
      INTEGER :: SCHEME = 1   !Which scheme are we operating with right now.
      INTEGER :: TMP_INT = 0
      INTEGER :: i,j,k,m,status = 0
      LOGICAL :: TMP_BOOL = .FALSE.,VERBOSE

      CONTAINS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   TRS_UNIT
!   Finds a free unit to open TRS file on
!   from: https://fortranwiki.org/fortran/show/newunit
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER FUNCTION NEWUNIT(UNIT)
      INTEGER, INTENT(OUT), OPTIONAL :: UNIT
      INTEGER, PARAMETER  :: LUN_MIN=10, LUN_MAX=1000
      LOGICAL             :: OPENED
      INTEGER             :: LUN
      NEWUNIT = -1
      DO LUN=LUN_MIN,LUN_MAX
        INQUIRE(UNIT=LUN,OPENED=OPENED)
        IF (.NOT. OPENED) THEN
          NEWUNIT=LUN
          EXIT
        ENDIF
      ENDDO
      IF (PRESENT(UNIT)) THEN
        UNIT = NEWUNIT
      ENDIF
      END FUNCTION NEWUNIT
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
        INTEGER :: I,SIGN,HSIZE,VSIZE
        DOUBLE PRECISION :: HD

        ! WRITE(*,*)'TRS_CALC CALLED'
        HD = ABS(HEAD)
        HSIZE = SIZE(HEADS)
        VSIZE = SIZE(VALUES)

        IF (HEAD.LE.0.0) THEN
          SIGN = -1
        ELSE
          SIGN = 1
        ENDIF

        IF (HSIZE.EQ.VSIZE) THEN
          IF (HD.LE.HEADS(1)) THEN
            CALC = SIGN*VALUES(1)
          ELSEIF (HD.GE.HEADS(HSIZE)) THEN
            CALC = SIGN*VALUES(VSIZE)
          ELSE
            DO I = 2,VSIZE
              IF ((HD.GE.HEADS(I-1))
     &          .AND.(HD.LE.HEADS(I))) THEN
                CALC = SIGN*TRS_INTERP(HD,HEADS(I-1),
     &                  HEADS(I),VALUES(I-1),VALUES(I))
                ENDIF
              ENDDO
            ENDIF
          ELSE
            WRITE(*,*)'UNEQUAL ARRAYS USED IN TRS_CALC FUNCTION'
            CALC = 0.0
          ENDIF
          ! WRITE(*,*)'CALC = ',CALC
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
          FLOW = -CD*AREA*SQRT(2*9.807*ABS(HDIFF))
        ENDIF
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

      SUBROUTINE  TRS_CALC_GEN(H,P,TRS)
      DOUBLE PRECISION,INTENT(IN)  :: H
      INTEGER,INTENT(IN)           :: TRS
      DOUBLE PRECISION,INTENT(OUT) :: P
      P = TRS_CALC(H, ! USE FLOW IN CASE IT IS THROTTLED OR REDUCED
     &          HEAD_G(TRS,:),
     &          POWER_G(TRS,:,TURB_CHART(TRS)))
      END SUBROUTINE TRS_CALC_GEN

      SUBROUTINE  TRS_CALC_GEN_Q(Q,P,TRS)
      DOUBLE PRECISION,INTENT(IN)  :: Q
      INTEGER,INTENT(IN)           :: TRS
      DOUBLE PRECISION,INTENT(OUT) :: P
      P = TRS_CALC(Q, ! USE FLOW IN CASE IT IS THROTTLED OR REDUCED
     &          FLOW_G(TRS,:,TURB_CHART(TRS)),
     &          POWER_G(TRS,:,TURB_CHART(TRS)))
      END SUBROUTINE TRS_CALC_GEN_Q

      SUBROUTINE  TRS_CALC_PMP(H,P,TRS)
      DOUBLE PRECISION,INTENT(IN)  :: H
      INTEGER,INTENT(IN)           :: TRS
      DOUBLE PRECISION,INTENT(OUT) :: P
      P = TRS_CALC(H, ! USE FLOW IN CASE IT IS THROTTLED OR REDUCED
     &          HEAD_P(TRS,:),
     &          POWER_P(TRS,:,PUMP_CHART(TRS)))
      END SUBROUTINE TRS_CALC_PMP

      SUBROUTINE  TRS_CALC_PMP_Q(Q,P,TRS)
      DOUBLE PRECISION,INTENT(IN)  :: Q
      INTEGER,INTENT(IN)           :: TRS
      DOUBLE PRECISION,INTENT(OUT) :: P
      P = TRS_CALC(Q, ! USE FLOW IN CASE IT IS THROTTLED OR REDUCED
     &          FLOW_P(TRS,:,PUMP_CHART(TRS)),
     &          POWER_P(TRS,:,PUMP_CHART(TRS)))
      END SUBROUTINE TRS_CALC_PMP_Q
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   TRS_POWER
!   Calculate the power generated across a culvert
!   I moved this here as it's a big old pain, due to the flexibility
!   that I regret attempting to include. NH
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
       ! 524 IN BUSE
      SUBROUTINE TRS_POWER(H,SF,TRS,CLPB,CORR56,CORRV5)
      DOUBLE PRECISION,INTENT(IN) :: H,SF      ! ACTUAL FLOW THROUGH CULVERT
      INTEGER,INTENT(IN)          :: TRS    ! TIDAL RANGE SCHEME ID
      INTEGER,INTENT(IN)          :: CLPB   ! 4 = control, 5 = Turbine, 6 = Sluice, [0,1,2,3] as before
      DOUBLE PRECISION,INTENT(IN) :: CORR56 ! Parrallel sluicing for turbine: 0 = FALSE, 1 = TRUE
      DOUBLE PRECISION,INTENT(IN) :: CORRV5 ! Turbine type: 1 = Turb+Pump, 2 = Turb Only, 3 = Pump Only
      DOUBLE PRECISION            :: PWR = 0.D0! TEMPORARY POWER VARIABLE

      ! IF TURBINE
      IF (CLPB.EQ.5) THEN
        ! NORMAL GENERATING
        IF ((MODE(TRS).EQ.2).OR.(MODE(TRS).EQ.5)) THEN
          IF ((CORRV5.EQ.1).OR.(CORRV5.EQ.2)) THEN
            CALL TRS_CALC_GEN(ABS(H),PWR,TRS)
            POWER(TRS) = POWER(TRS) + RAMP(TRS)*PWR*SF
          ENDIF
            ! HOLDING (WHERE APPROPRIATE)
        ELSEIF (((MODE(TRS).EQ.1).OR.(MODE(TRS).EQ.4)).AND.
     &              (PHASETIME(TRS).LT.RAMPTIME(TRS))) THEN
          IF (ISPUMPING(TRS)) THEN
            IF ((CORRV5.EQ.1).OR.(CORRV5.EQ.3)) THEN
              CALL TRS_CALC_PMP(ABS(H),PWR,TRS)
              POWER(TRS) = POWER(TRS) - (1 - RAMP(TRS))*PWR*SF
            ENDIF
          ELSE
            IF ((CORR56.EQ.1).AND.
     &              ((CORRV5.EQ.1).OR.(CORRV5.EQ.2))) THEN
              CALL TRS_CALC_GEN(ABS(H),PWR,TRS)
              POWER(TRS) = POWER(TRS) + (1 - RAMP(TRS))*PWR*SF
            ENDIF
          ENDIF
          ! PARALLEL SLUICING
          ELSEIF (((MODE(TRS).EQ.3).OR.
     &               (MODE(TRS).EQ.6)).AND.
     &               (CORR56.EQ.1)) THEN
            IF ((CORRV5.EQ.1).OR.(CORRV5.EQ.2)) THEN
              CALL TRS_CALC_GEN(ABS(H),PWR,TRS)
              POWER(TRS) = POWER(TRS) + PWR*SF
            ENDIF

          ! NORMAL PUMPING
          ELSEIF ((MODE(TRS).EQ.-1).OR.
     &          (MODE(TRS).EQ.-2)) THEN
            IF ((CORRV5.EQ.1).OR.(CORRV5.EQ.3)) THEN
              CALL TRS_CALC_PMP(ABS(H),PWR,TRS)
              POWER(TRS) = POWER(TRS) - RAMP(TRS)*PWR*SF
            ENDIF
          ENDIF
        ENDIF
      END SUBROUTINE TRS_POWER


      SUBROUTINE TRS_POWER_Q(Q_IN,SF,TRS,CLPB,CORR56,CORRV5)
      DOUBLE PRECISION,INTENT(IN) :: Q_IN,SF      ! ACTUAL FLOW THROUGH CULVERT
      INTEGER,INTENT(IN)          :: TRS    ! TIDAL RANGE SCHEME ID
      INTEGER,INTENT(IN)          :: CLPB   ! 4 = control, 5 = Turbine, 6 = Sluice, [0,1,2,3] as before
      DOUBLE PRECISION,INTENT(IN) :: CORR56 ! Parrallel sluicing for turbine: 0 = FALSE, 1 = TRUE
      DOUBLE PRECISION,INTENT(IN) :: CORRV5 ! Turbine type: 1 = Turb+Pump, 2 = Turb Only, 3 = Pump Only
      DOUBLE PRECISION            :: Q,PWR    ! TEMPORARY POWER VARIABLE
      Q = Q_IN/SF
      ! IF TURBINE
      IF (CLPB.EQ.5) THEN
        ! NORMAL GENERATING
        IF ((MODE(TRS).EQ.2).OR.(MODE(TRS).EQ.5)) THEN
          IF ((CORRV5.EQ.1).OR.(CORRV5.EQ.2)) THEN
            CALL TRS_CALC_GEN_Q(ABS(Q),PWR,TRS)
            POWER(TRS) = POWER(TRS) + RAMP(TRS)*PWR*SF
          ENDIF
            ! HOLDING (WHERE APPROPRIATE)
        ELSEIF (((MODE(TRS).EQ.1).OR.(MODE(TRS).EQ.4)).AND.
     &              (PHASETIME(TRS).LT.RAMPTIME(TRS))) THEN
          IF (ISPUMPING(TRS)) THEN
            IF ((CORRV5.EQ.1).OR.(CORRV5.EQ.3)) THEN
              CALL TRS_CALC_PMP_Q(ABS(Q),PWR,TRS)
              POWER(TRS) = POWER(TRS) - (1 - RAMP(TRS))*PWR*SF
            ENDIF
          ELSE
            IF ((CORR56.EQ.1).AND.
     &              ((CORRV5.EQ.1).OR.(CORRV5.EQ.2))) THEN
              CALL TRS_CALC_GEN_Q(ABS(Q),PWR,TRS)
              POWER(TRS) = POWER(TRS) + (1 - RAMP(TRS))*PWR*SF
            ENDIF
          ENDIF
          ! PARALLEL SLUICING
          ELSEIF (((MODE(TRS).EQ.3).OR.
     &               (MODE(TRS).EQ.6)).AND.
     &               (CORR56.EQ.1)) THEN
            IF ((CORRV5.EQ.1).OR.(CORRV5.EQ.2)) THEN
              CALL TRS_CALC_GEN_Q(ABS(Q),PWR,TRS)
              POWER(TRS) = POWER(TRS) + PWR*SF
            ENDIF

          ! NORMAL PUMPING
          ELSEIF ((MODE(TRS).EQ.-1).OR.
     &          (MODE(TRS).EQ.-2)) THEN
            IF ((CORRV5.EQ.1).OR.(CORRV5.EQ.3)) THEN
              CALL TRS_CALC_PMP_Q(ABS(Q),PWR,TRS)
              POWER(TRS) = POWER(TRS) - RAMP(TRS)*PWR*SF
            ENDIF
          ENDIF
        ENDIF
        END SUBROUTINE TRS_POWER_Q

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!       Pushes the actual flows in and out of the schemes to the variables
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
        SUBROUTINE TRS_FLOW(Q,TRS,CLPB)
        DOUBLE PRECISION,INTENT(IN) :: Q      ! ACTUAL FLOW THROUGH CULVERT
        INTEGER,INTENT(IN)          :: TRS    ! TIDAL RANGE SCHEME ID
        INTEGER,INTENT(IN)          :: CLPB   ! 4 = control, 5 = Turbine, 6 = Sluice, [0,1,2,3] as before

        IF (CLPB.EQ.5) THEN
          Q_TURB(TRS) = Q_TURB(TRS) + Q
        ELSEIF (CLPB.EQ.6) THEN
          Q_SLUICE(TRS) = Q_SLUICE(TRS) + Q
        ENDIF
        END SUBROUTINE TRS_FLOW
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
        RETURN
      END SUBROUTINE TRS_STAT
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   TRS_NEW_MODE
!   Updates the operating mode of the tidal range schemes based on the
!   decision tree and the current mode, CT only used to check phase
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_NEW_MODE(CT)

      DOUBLE PRECISION,INTENT(IN) :: CT!, TRS_RAMP

      DOUBLE PRECISION :: PHASE_T, HD
      INTEGER :: TRS

      DO TRS = 1,NUM_TRS
      HD = ABS(HEADDIFF(TRS))
      PHASETIME(TRS) = CT - PREV_SWITCH(TRS) ! Initialise with prior phase time

      ! If we have completed starting operation of this phase
      IF (PHASETIME(TRS).GE.RAMPTIME(TRS)) THEN
        SELECT CASE(MODE(TRS))
          CASE (0)  !      0 : Initial sluicing warmup
!           Ends when the start of flexing begins
            IF (CT.GE.FLEX_TIMES(TRS,1)) THEN
              MODE(TRS) = MODE_FIRST(TRS)  !     Default is 1 -> High water holding
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (1)  !      1 : High water holding
            IF ((HD.GE.H_START(TRS))              ! Enough head difference
     &      .OR.(PHASETIME(TRS).GT.MAX_HOLD(TRS))) THEN  ! Time limit reached
              MODE(TRS) = 2                       ! 2 : Ebb generation
              PREV_SWITCH(TRS) = CT               ! Update previous switch
            ENDIF
          CASE (2)  !      2 : Ebb generation
            IF ((HD.LE.H_END(TRS))                ! Head diff end triggered
     &      .OR.(PHASETIME(TRS).GT.MAX_GEN(TRS))) THEN   ! Time limit reached
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
     &      .OR.(PHASETIME(TRS).GT.MAX_SLUICE(TRS))) THEN
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
     &      .OR.(PHASETIME(TRS).GE.MAX_PUMP(TRS))) THEN ! Time limit reached
              MODE(TRS) = 4  !      4 : Low water holding
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (4)  !      4 : Low water holding
            IF (IS2WAY(TRS)) THEN
              IF ((HD.GE.H_START(TRS))
     &      .OR.(PHASETIME(TRS).GT.MAX_HOLD(TRS))) THEN
                MODE(TRS) = 5 !      5 : Flood generation
                PREV_SWITCH(TRS) = CT
              ENDIF
            ELSE !      5 : Flood generation
              IF((HEADDIFF(TRS).GT.0.1)
     &      .OR.(PHASETIME(TRS).GT.MAX_HOLD(TRS))) THEN
                MODE(TRS)= 6  !      6 : Flood sluicing
                PREV_SWITCH(TRS) = CT
              ENDIF
            ENDIF
          CASE (5)  !      5 : Flood generation
            IF ((HD.LE.H_END(TRS))
     &      .OR.(PHASETIME(TRS).GT.MAX_HOLD(TRS))) THEN
              MODE(TRS) = 2  !      2 : Ebb generation
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE (6)  !      6 : Flood sluicing
            IF ((HD.LE.0.1)
     &      .OR.(PHASETIME(TRS).GT.MAX_SLUICE(TRS))) THEN
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
     &    .OR.(PHASETIME(TRS).GE.MAX_PUMP(TRS))) THEN
              MODE(TRS) = 1  !      1 : High water holding
              PREV_SWITCH(TRS) = CT
            ENDIF
          CASE DEFAULT
            WRITE(*,*)'ERROR IN TRS_NEW_MODE'
        END SELECT
!       UPDATE PHASE TIMING?
!       PHASE_T = CT - PREV_SWITCH(TRS)
      ENDIF
      PHASETIME(TRS) = CT - PREV_SWITCH(TRS)
      RAMP(TRS) = TRS_RAMP(RAMPTIME(TRS),PHASETIME(TRS))
      ENDDO
      RETURN
      END SUBROUTINE TRS_NEW_MODE
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   TRS_FLEX_VALUES
!   Sets the control parameters based on the time and the flex control
!   definition
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SUBROUTINE TRS_FLEX_VALUES(CT)
      DOUBLE PRECISION,INTENT(IN) :: CT

      INTEGER :: I,TRS
      ! IMPLICIT NONE

      DO TRS =1,NUM_TRS
        IF (CT.LT.FLEX_TIMES(1,TRS)) THEN
          !WARMUP PERIOD
          H_START(TRS) = 1000 !Do not enter generation
          H_END(TRS) = 0.0
          TURB_CHART(TRS) = 1
          PUMP_TARG(TRS) = 0.0
          PUMP_CHART(TRS) = 1
          PREV_I_FLEX_TIMES(TRS) = 1

      ELSEIF (CT.GE.FLEX_TIMES(N_FLEX(TRS),TRS)) THEN
        !FINAL FLEX PERIOD ONWARDS
        H_START(TRS) = H_STARTS(N_FLEX(TRS),TRS)
        H_END(TRS) = H_ENDS(N_FLEX(TRS),TRS)
        TURB_CHART(TRS) = TURB_CHARTS(N_FLEX(TRS),TRS)
        PUMP_TARG(TRS) = PUMP_TARGS(N_FLEX(TRS),TRS)
        PUMP_CHART(TRS) = PUMP_CHARTS(N_FLEX(TRS),TRS)
        PREV_I_FLEX_TIMES(TRS) = N_FLEX(TRS)
        ! SAME FLEX PERIOD AS LAST TIME
      ELSEIF ((CT.GE.FLEX_TIMES(PREV_I_FLEX_TIMES(TRS)-1,TRS))
     &  .AND.(CT.LT.FLEX_TIMES(PREV_I_FLEX_TIMES(TRS),TRS))) THEN
        H_START(TRS) = H_STARTS(PREV_I_FLEX_TIMES(TRS)-1,TRS)
        H_END(TRS) = H_ENDS(PREV_I_FLEX_TIMES(TRS)-1,TRS)
        TURB_CHART(TRS) = TURB_CHARTS(PREV_I_FLEX_TIMES(TRS)-1,TRS)
        PUMP_TARG(TRS) = PUMP_TARGS(PREV_I_FLEX_TIMES(TRS)-1,TRS)
        PUMP_CHART(TRS) = PUMP_CHARTS(PREV_I_FLEX_TIMES(TRS)-1,TRS)
        PREV_I_FLEX_TIMES(TRS) = PREV_I_FLEX_TIMES(TRS)

      ELSE
        !IN THE DYNAMIC FLEX PERIOD
        DO I = 2,N_FLEX(TRS)
          IF ((CT.GE.FLEX_TIMES(I-1,TRS))
     &      .AND.(CT.LT.FLEX_TIMES(I,TRS))) THEN
            H_START(TRS) = H_STARTS(I-1,TRS)
            H_END(TRS) = H_ENDS(I-1,TRS)
            TURB_CHART(TRS) = TURB_CHARTS(I-1,TRS)
            PUMP_TARG(TRS) = PUMP_TARGS(I-1,TRS)
            PUMP_CHART(TRS) = PUMP_CHARTS(I-1,TRS)
            PREV_I_FLEX_TIMES(TRS) = I
          ENDIF
        ENDDO
      ENDIF
      ENDDO
      RETURN
      END SUBROUTINE TRS_FLEX_VALUES

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
        WRITE(*,*)'++++++++++++++++++++++++++++++++++++',
     &    'WARNING+++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) 'Unexpected key - variable pair:'
        WRITE(*,*) 'Key:      ',TRIM(ADJUSTL(K))
        WRITE(*,*) 'Variable: ',TRIM(ADJUSTL(V))
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++',
     & '++++++++++++++++++++++++++++++++++++++++++++'
      END SUBROUTINE TRS_KV_WARN

!     READ A Hill Chart from the file point
      SUBROUTINE TRS_LOAD_HILLCHART(HEADS,ARRAY,NF_TRS)
      DOUBLE PRECISION,DIMENSION(:),INTENT(INOUT) :: HEADS ! Head difference values
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT) :: ARRAY ! Array of variable
      CHARACTER(LEN=100) :: LINE
      INTEGER,INTENT(IN) :: NF_TRS             ! file key

      INTEGER :: SZA(2),SZH(1)    ! shapes of arrays
      INTEGER :: ROW,COL            ! Iterators
      INTEGER :: NF
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

!! UPDATE THIS SUB BECAUSE IT'S A BIT OF BOTH
! Change what the input is, as it should be able to access the module components..?
      SUBROUTINE TRS_READ_DATAFILES(NF_TRS)

      INTEGER,             INTENT(INOUT) :: NF_TRS !,NUM_TRS

!       !File with the tidal range scheme control parameters
!       !as key variable pairs, with comments if needed
! !      NFO1=T2D_FILES(T2DFO1)%LU
!       INTEGER,INTENT(IN) :: NF_TRS,NM_TRS

      INTEGER :: TRS,NF
      CHARACTER(LEN=100) :: LINE,KEYWORD,VARIABLE
      CHARACTER(LEN=20) :: TRS_FNAME
      LOGICAL :: ISOPEN = .FALSE.,PRINTKV

      ! This little bit stops the compiler throwing an error but is
      ! ! probably very very bad.
      ! NF = NF_TRS
      ! NF_TRS = NF

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
      IF (.NOT.ALLOCATED(PREV_I_FLEX_TIMES)) THEN
        ALLOCATE(PREV_I_FLEX_TIMES(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_FLX_TIMES')
      ENDIF
      IF (.NOT.ALLOCATED(PREV_I_FLOW_G)) THEN
        ALLOCATE(PREV_I_FLOW_G(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_FLOW_G')
      ENDIF
      IF (.NOT.ALLOCATED(PREV_I_FLOW_P)) THEN
        ALLOCATE(PREV_I_FLOW_P(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_FLOW_P')
      ENDIF
      IF (.NOT.ALLOCATED(PREV_I_POWER_G)) THEN
        ALLOCATE(PREV_I_POWER_G(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_POWER_G')
      ENDIF
      IF (.NOT.ALLOCATED(PREV_I_POWER_P)) THEN
        ALLOCATE(PREV_I_POWER_P(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PREV_I_POWER_P')
      ENDIF
      IF (.NOT.ALLOCATED(QP_BASE)) THEN
        ALLOCATE(QP_BASE(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'QP_BASE')
      ENDIF
      IF (.NOT.ALLOCATED(QG_BASE)) THEN
        ALLOCATE(QG_BASE(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'QG_BASE')
      ENDIF
      IF (.NOT.ALLOCATED(PHASETIME)) THEN
        ALLOCATE(PHASETIME(NUM_TRS),STAT=STATUS)
        CALL TRS_STAT(STATUS,'PHASETIME')
      ENDIF

      TRS_TIME = 0.0

      DO TRS = 1,NUM_TRS
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
        IF (IPID.EQ.0) THEN
          WRITE(*,*) '++++++++++++++++++++++++++++++++++++',
     & '++++++++++++++++++++++++++++++++++++++++++++'
          WRITE(*,*) 'File [',TRS_FNAME,'] Opened'
          WRITE(*,*) '++++++++++++++++++++++++++++++++++++',
     & '++++++++++++++++++++++++++++++++++++++++++++'
        ENDIF
      IF (TRS_FIRST) THEN
        READ(NF_TRS,*) LINE !Header line
        ! WRITE(*,*) LINE
        READ(NF_TRS,'(A100)') LINE
        ! YOU CANNOT HAVE A VARIABLE WHOSE NAME STARTS WITH 'END' in some form
        DO WHILE(TO_UPPER((LINE(1:3))).NE.'END')
         ! WRITE(*,*) LINE
          CALL TRS_PARSE(LINE,KEYWORD,VARIABLE)
          ! WRITE(*,*)'KEY [',KEYWORD,'] VAR [',VARIABLE,']'
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

            CASE('MODE')  ! Integer mode code
            IF(.NOT.ALLOCATED(MODE)) THEN
              ALLOCATE(MODE(NUM_TRS))
              MODE(:) = 0 !Only modify default in test or continued operation
            ENDIF
              READ(VARIABLE,*) MODE(TRS)

            CASE('MODE_PREV') ! Integer (Don't think it does anything)
            IF(.NOT.ALLOCATED(MODE_PREV)) THEN
              ALLOCATE(MODE_PREV(NUM_TRS))
              MODE_PREV(:) = 0
            ENDIF
            READ(VARIABLE,*) MODE_PREV(TRS)

            CASE('MODE_FIRST')  ! Integer mode code for end of warmup
            IF(.NOT.ALLOCATED(MODE_FIRST)) THEN
              ALLOCATE(MODE_FIRST(NUM_TRS))
              MODE_FIRST(:) = 1 !Only modify default in test or continued operation
            ENDIF
              READ(VARIABLE,*) MODE_FIRST(TRS)

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
              ! Allocate the flex arrays here, as they only need this?
              READ(VARIABLE,*) N_FLEX(TRS)
              IF (.NOT.ALLOCATED(FLEX_TIMES)) THEN
                ALLOCATE(FLEX_TIMES(N_FLEX(TRS),NUM_TRS),STAT=STATUS)
                CALL TRS_STAT(STATUS,'FLX_TIMES')
                FLEX_TIMES(:,:) = 0.0

                ALLOCATE(H_STARTS(N_FLEX(TRS),NUM_TRS),STAT=STATUS)
                CALL TRS_STAT(STATUS,'H_STARTS')
                H_STARTS(:,:) = 0.0

                ALLOCATE(H_ENDS(N_FLEX(TRS),NUM_TRS),STAT=STATUS)
                CALL TRS_STAT(STATUS,'H_ENDS')
                H_ENDS(:,:) = 0.0

                ALLOCATE(TURB_CHARTS(N_FLEX(TRS),NUM_TRS),STAT=STATUS)
                CALL TRS_STAT(STATUS,'TURB_CHARTS')
                TURB_CHARTS(:,:) = 0

                ALLOCATE(PUMP_TARGS(N_FLEX(TRS),NUM_TRS),STAT=STATUS)
                CALL TRS_STAT(STATUS,'PUMP_TARGS')
                PUMP_TARGS(:,:) = 0.0

                ALLOCATE(PUMP_CHARTS(N_FLEX(TRS),NUM_TRS),STAT=STATUS)
                CALL TRS_STAT(STATUS,'PUMP_CHARTS')
                PUMP_CHARTS(:,:) = 0

              ENDIF
            CASE('PUMPTOLEVEL')  ! Integer
            IF(.NOT.ALLOCATED(PUMPTOLEVEL)) THEN
              ALLOCATE(PUMPTOLEVEL(NUM_TRS))
              PUMPTOLEVEL(:) = .FALSE.
            ENDIF
              READ(VARIABLE,*) PUMPTOLEVEL(TRS)

            CASE('WARMUP')  ! Sluice coeff for warmup
            IF(.NOT.ALLOCATED(WARMUP)) THEN
              ALLOCATE(WARMUP(NUM_TRS))
              WARMUP(:) = 1 ! initially set to 1
            ENDIF
              READ(VARIABLE,*) WARMUP(TRS)

            !Diameter(meter) of original turbine used in hill charts
            CASE('ORIG_DIAM_T')
            IF(.NOT.ALLOCATED(ORIG_DIAM_T)) THEN
              ALLOCATE(ORIG_DIAM_T(NUM_TRS))
              ORIG_DIAM_T(:) = 0.0
            ENDIF
              READ(VARIABLE,*) ORIG_DIAM_T(TRS)

!           MORE COMPLEX ARRAY LOADERS
            CASE('GENERATOR')
              PRINTKV = .FALSE.
              !WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)
              IF (.NOT.ALLOCATED(HEAD_G)) THEN
                ALLOCATE(HEAD_G(NUM_TRS,LEN_HILLS_G(TRS)),STAT=STATUS)
              ENDIF
              CALL TRS_STAT(STATUS,'HILL CHART: Generator heads')
              ! Power indicator words will contain the letter p
              IF ((INDEX(VARIABLE,'P').NE.0).OR.
     &             (INDEX(VARIABLE,'p').NE.0)) THEN
                IF (.NOT.ALLOCATED(POWER_G)) THEN
                  ALLOCATE(POWER_G(NUM_TRS,MAXVAL(LEN_HILLS_G),
     &            MAXVAL(N_HILLS_G)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Generator Powers')
                !There may be a better way to do this to be honest...
                CALL TRS_LOAD_HILLCHART(HEAD_G(TRS,:),
     &              POWER_G(TRS,:,:),NF_TRS)
                IF (IPID.EQ.0) THEN
                  CALL TRS_PRINT_HILLCHART(HEAD_G(TRS,:),
     &              POWER_G(TRS,:,:),'GENERATOR','POWER')
                ENDIF

              ! Flow will have an F
              ELSEIF ((INDEX(VARIABLE,'F').NE.0).OR.
     &             (INDEX(VARIABLE,'f').NE.0)) THEN
                IF (.NOT.ALLOCATED(FLOW_G)) THEN
                  ALLOCATE(FLOW_G(NUM_TRS,MAXVAL(LEN_HILLS_G),
     &              MAXVAL(N_HILLS_G)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Generator Flow')
                CALL TRS_LOAD_HILLCHART(HEAD_G(TRS,:),
     &            FLOW_G(TRS,:,:),NF_TRS)
                IF (IPID.EQ.0) THEN
                  CALL TRS_PRINT_HILLCHART(HEAD_G(TRS,:),
     &              FLOW_G(TRS,:,:),'GENERATOR','FLOW')
                ENDIF
              ELSE !Error condition of some kind
                  CALL TRS_KV_WARN(KEYWORD,VARIABLE)
              ENDIF

            CASE('PUMP')
              PRINTKV = .FALSE.
              !WRITE(*,*) TRIM(KEYWORD),' ',TRIM(VARIABLE)
              IF (.NOT.ALLOCATED(HEAD_P)) THEN
                ALLOCATE(HEAD_P(NUM_TRS,LEN_HILLS_P(TRS)),STAT=STATUS)
              ENDIF
              CALL TRS_STAT(STATUS,'HILL CHART: Pump heads')
              ! Power indicator words will contain the letter p
              IF ((INDEX(VARIABLE,'P').NE.0).OR.
     &             (INDEX(VARIABLE,'p').NE.0)) THEN
                IF (.NOT.ALLOCATED(POWER_P)) THEN
                  ALLOCATE(POWER_P(NUM_TRS,MAXVAL(LEN_HILLS_P),
     &            MAXVAL(N_HILLS_P)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Pump Powers')
                !There may be a better way to do this to be honest...
                CALL TRS_LOAD_HILLCHART(HEAD_P(TRS,:),
     &              POWER_P(TRS,:,:),NF_TRS)
                IF (IPID.EQ.0) THEN
                  CALL TRS_PRINT_HILLCHART(HEAD_P(TRS,:),
     &              POWER_P(TRS,:,:),'PUMP','POWER')
                ENDIF

              ! Flow will have an F
              ELSEIF ((INDEX(VARIABLE,'F').NE.0).OR.
     &             (INDEX(VARIABLE,'f').NE.0)) THEN
                IF (.NOT.ALLOCATED(FLOW_P)) THEN
                  ALLOCATE(FLOW_P(NUM_TRS,MAXVAL(LEN_HILLS_P),
     &              MAXVAL(N_HILLS_P)),STAT=STATUS)
                ENDIF
                CALL TRS_STAT(STATUS,'HILL CHART: Generator Flow')
                CALL TRS_LOAD_HILLCHART(HEAD_P(TRS,:),
     &            FLOW_P(TRS,:,:),NF_TRS)
                IF (IPID.EQ.0) THEN
                  CALL TRS_PRINT_HILLCHART(HEAD_G(TRS,:),
     &              FLOW_P(TRS,:,:),'GENERATOR','FLOW')
                ENDIF
              ELSE !Error condition of some kind
                  CALL TRS_KV_WARN(KEYWORD,VARIABLE)
              ENDIF

            CASE('FLEX')
              PRINTKV = .FALSE.

              IF (IPID.EQ.0) THEN
                WRITE(*,*)  'FLEX, T_START, H_START, H_END, ',
     &                   'T_CHART, P_TARG, P_CHART'
              ENDIF
              DO K = 1,N_FLEX(TRS)
                READ(NF_TRS,*) FLEX_TIMES(K,TRS),
     &            H_STARTS(K,TRS),
     &            H_ENDS(K,TRS),
     &            TURB_CHARTS(K,TRS),
     &            PUMP_TARGS(K,TRS),
     &            PUMP_CHARTS(K,TRS)
                ! H_END(TRS) = H_ENDS(I-1,TRS)
              ENDDO
              IF (IPID.EQ.0) THEN
                DO K=1,N_FLEX(TRS)
                  WRITE(*,'(I5 F8.2 F8.2 F8.2 I8 F8.2 I5)')
     &              K,
     &              FLEX_TIMES(K,TRS),
     &              H_STARTS(K,TRS),
     &              H_ENDS(K,TRS),
     &              TURB_CHARTS(K,TRS),
     &              PUMP_TARGS(K,TRS),
     &              PUMP_CHARTS(K,TRS)
                ENDDO
              ENDIF

            CASE DEFAULT ! Oh no, something must have gone wrong
              IF (KEYWORD(1:1).NE.'!') THEN
                  CALL TRS_KV_WARN(KEYWORD,VARIABLE)
              ENDIF
          END SELECT

          IF ((KEYWORD(1:1).NE.'!').AND.(PRINTKV)) THEN
            IF (IPID.EQ.0) THEN
              WRITE(*,*) TRIM(ADJUSTL(KEYWORD)),' = ',TRIM(VARIABLE)
            ENDIF
          ENDIF
          PRINTKV = .TRUE.
          READ(NF_TRS,'(A100)',END=702) LINE
          ! WRITE(*,*) LINE
!         END error checker stops it from reading to infinity?
        END DO

702     CONTINUE
        IF (IPID.EQ.0) THEN
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++',
     &    '++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) 'Done reading [',TRIM(TRS_FNAME),'] Control File.'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++',
     &    '++++++++++++++++++++++++++++++++++++++++++++'
        ENDIF
        CLOSE(NF_TRS)

        ELSE
          WRITE(*,*) '++++++++++++++++++++++++++++++++++++',
     & '++++++++++++++++++++++++++++++++++++++++++++'
          WRITE(*,*) 'UNABLE TO OPEN [',TRS_FNAME,'] CONTROL FILE.'
          WRITE(*,*) '++++++++++++++++++++++++++++++++++++',
     & '++++++++++++++++++++++++++++++++++++++++++++'
        ENDIF
      ENDIF
      END DO
      ! TRS_FIRST = .FALSE.
      END SUBROUTINE TRS_READ_DATAFILES
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     Write the headers and initial row of the output CSV
!     Then write the updated data to the respective files.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! REWRITE AS IT SHOULD HAVE ACCESS TO THE MODULE CONTENTS
      SUBROUTINE TRS_WRITE_RESULTS(TRS_TIME, OF_TRS)

      INTEGER,  INTENT(IN) :: OF_TRS ! NUM_TRS,
      DOUBLE PRECISION,     INTENT(IN) :: TRS_TIME

      INTEGER :: TRS,STAT
      CHARACTER(LEN=30) :: TRS_FNAME
      CHARACTER :: A = ','
      LOGICAL,SAVE :: RESULTS_FIRST = .TRUE.

      IF (RESULTS_FIRST) THEN
        ! TRS_TIME = 0.0
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
! REWRITE AS IT SHOULD BE ABLE TO CALL THE MODULE CONTENTS
      SUBROUTINE TRS_WRITE_STATUS(TRS_TIME)
      DOUBLE PRECISION,     INTENT(IN) :: TRS_TIME
      INTEGER :: TRS
      CHARACTER(len = 40) :: FF,II,F,FI

      FF = '(A20 F10.3 A20 F10.3)' ! float float
      II = '(A20 I10 A20 I8)'     ! int int
      FI = '(A20 F10.3 A20 I10)'
      F  = '(A20 F10.3)'
      DO TRS = 1,NUM_TRS
      IF (TRS.EQ.1) THEN
      WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++',
     & '++++++++++++++++++++++++++++'
      ENDIF
      WRITE(*,FI) 'TIME(HRS):',TRS_TIME,'TRS:',TRS
      WRITE(*,FI) 'HEADDIFF(M):',HEADDIFF(TRS),'MODE(-):',MODE(TRS)
      WRITE(*,FF) 'WL(mD) OUT:',WL_OUT(TRS),'IN:',WL_IN(TRS)
      WRITE(*,FF) 'PHASE(HRS):',PHASETIME(TRS),'RAMP:',RAMP(TRS)
      WRITE(*,FF) 'Q(m3/s)TURB:',Q_TURB(TRS),'SLUICE:',Q_SLUICE(TRS)
      WRITE(*,FF) 'POWER(MW):',POWER(TRS),'PTARG(-):',PUMP_TARG(TRS)
      WRITE(*,FF) 'H START (M):',H_START(TRS),'H END:',H_END(TRS)
      WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++',
     & '++++++++++++++++++++++++++++'
      ENDDO

      END SUBROUTINE TRS_WRITE_STATUS

      SUBROUTINE TRS_BASE_FLOWS()
      INTEGER :: TRS
        ! Sluice flows calculated with orifice equation.
        IF(.NOT.ALLOCATED(QG_BASE)) THEN
          ALLOCATE(QG_BASE(NUM_TRS))
          WRITE(*,*)'ALLOCATION WARNING',NUM_TRS
        ENDIF

      DO TRS = 1,NUM_TRS

        QG_BASE(TRS) = TRS_CALC(
     &    HEADDIFF(TRS),
     &    HEAD_G(TRS,:),
     &    FLOW_G(TRS,:,TURB_CHART(TRS)))

        QP_BASE(TRS) = TRS_CALC(
     &    HEADDIFF(TRS),
     &    HEAD_P(TRS,:),
     &    FLOW_P(TRS,:,PUMP_CHART(TRS)))

      ENDDO
      END SUBROUTINE TRS_BASE_FLOWS

      ! SUBROUTINE TRS_COLLATE_FLOWS(DBUS,CTRASH,CLPBUS)
      ! DO I,
      ! END SUBROUTINE TRS_COLLATE_FLOWS

      END MODULE TRS_T2D
!     End of TRS_T2D
