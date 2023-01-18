!                   ***************
                    SUBROUTINE BUSE
!                   ***************
!
     &(RELAXB,NBUSE,ENTBUS,SORBUS,GRAV,
     & H,ZF,DBUS,LRGBUS,HAUBUS,CLPBUS,
     & ALTBUS,CSBUS,CEBUS,ANGBUS,LBUS,
     & NTRAC,T,TBUS,UBUS,VBUS,U,V,ENTET,
     & CV,C56,CV5,C5,CTRASH,FRICBUS,LONGBUS,
     & CIRC,DELBUS,OPTBUSE,V2DPAR,DT,SECBUS,MAXSOURCE,
     & NPTSCE,NPOIN2,KSCE)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!Brief    TREATS CULVERTS/TUBES (OR BRIDGES) IN CHARGE
!         OR WITH FREE SURFACE
!
!History  C. COULET (ARTELIA)
!+        23/05/2012
!+     First version.
!+
!
!History  U.H.MERKEL
!+        17/07/2012
!+        V6P2
!+     Adaptation to NAG.
!
!History  J-M HERVOUET (LNHE)
!+        27/07/2012
!+        V6P2
!+     Correction in parallel.
!
!History  C. COULET (ARTELIA)
!+        23/04/2013
!+        V6P3
!+     Correction of a bug.
!+     Sometimes SECT=0 and DBUS<>0 due to relaxation
!
!History A. LEROY (LNHE)
!+       M.J. TELES (ANTEA GROUP)
!+       S. SMOLDERS (FLANDERS HYDRAULICS RESEARCH)
!+        17/11/2015
!+        V7P2
!+     Adding more variables and more equations, more types of flows
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!History A. LEROY (LNHE)
!+        11/09/2017
!+        V7P3
!+     Correct the formulation 1 (Carlier) so that it falls back to a
!+     siphon formula when necessary
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| MODEL CURRENT TIME IN SECONDS
!| ALTBUS         |-->| ELEVATIONS OF CULVERTS
!| ANGBUS         |-->| ANGLE OF CULVERTS WITH AXIS OX.
!| C5             |-->| CORRECTION COEFFICIENT FOR FLOW TYPE 5
!| C56            |-->| COEFFICIENT TO DIFFERENTIATE BETWEEN FLOW TYPE 5
!|                |   | AND 6
!| CEBUS          |-->| HEAD LOSS COEFFICIENT WHEN WORKING AS AN INFLOW
!| CIRC           |-->| CULVERT ROUND (=1) OR RECTANGULAR (=0)
!| CLPBUS         |-->| INTEGER FLAG FOR FLOW DIRECTION (VALVE)
!|                |   | 0 - BOTH DIRECTIONS
!|                |   | 1 - ONLY FROM ENTRY TO EXIT
!|                |   | 2 - ONLY FROM EXIT TO ENTRY
!|                |   | 3 - NO FLOW
!| CSBUS          |-->| HEAD LOSS COEFFICIENT WHEN WORKING AS AN OUTFLOW
!| CTRASH         |-->| HEAD LOSS COEFFICIENT FOR TRASH SCREEN
!| CV             |-->| HEAD LOSS COEFFICIENT OF VALVE
!| CV5            |-->| CORRECTION COEFFICIENT FOR FLOW TYPE 5
!| DBUS           |<->| DISCHARGE OF CULVERTS
!| DELBUS         |-->| ANGLE OF THE PIPES WITH THE VERTICAL-0:HOR/90:VER
!| DT             |-->| TIME STEP (SECONDS)
!| ENTET          |-->| IF YES, PRINTING INFORMATION ON LISTING
!| ENTBUS         |-->| INDICES OF ENTRY OF CULVERTS IN GLOBAL NUMBERING
!| FRICBUS        |-->| MANNING COEFFICIENT FOR WATER FLOWING
!|                |   | OVER CULVERT MATERIAL
!| GRAV           |-->| GRAVITY
!| H              |-->| DEPTH
!| LBUS           |-->| LINEAR HEAD LOSS OF CULVERTS
!| LONGBUS        |-->| LENGTH OF CULVERTS
!| NBUSE          |-->| NUMBER OF CULVERTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| OPTBUSE        |-->| OPTION FOR THE TREATMENT OF CULVERTS
!| RELAXB         |-->| RELAXATION COEFFICIENT
!| SECBUS         |<->| SECTION OF THE CULVERT
!| SORBUS         |-->| INDICES OF CULVERTS EXITS IN GLOBAL NUMBERING
!| T              |-->| BLOCK OF TRACERS
!| TBUS           |<->| VALUES OF TRACERS AT CULVERTS EXTREMITY
!| U              |-->| X-COMPONENT OF VELOCITY
!| UBUS           |<->| VELOCITY U AT CULVERTS EXTREMITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| V2DPAR         |-->| INTEGRAL OF BASES AFTER ASSEMBLING IN PARALLEL
!| VBUS           |<->| VELOCITY V AT CULVERTS EXTREMITY
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN
      ! TO HAVE ACCESS TO LOGICAL UNITS OF FILES !culverts, TRS, Output
      USE DECLARATIONS_TELEMAC2D, ONLY:
     &  T2D_FILES,T2DFO1,T2DFO2,T2DRFO,AT,AABUS
      USE TRS_T2D
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: NBUSE,NTRAC
      INTEGER          , INTENT(IN)    :: ENTBUS(NBUSE),SORBUS(NBUSE)
      LOGICAL          , INTENT(IN)    :: ENTET
      DOUBLE PRECISION , INTENT(IN)    :: RELAXB,GRAV,DT
      DOUBLE PRECISION , INTENT(INOUT) :: UBUS(2,NBUSE),VBUS(2,NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: DBUS(NBUSE),SECBUS(NBUSE)
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TBUS
      DOUBLE PRECISION , INTENT(IN)    :: ANGBUS(NBUSE,2),LBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: CEBUS(NBUSE,2),CSBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(IN)    :: ALTBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(IN)    :: LRGBUS(NBUSE),HAUBUS(NBUSE,2)
      INTEGER          , INTENT(IN)    :: CLPBUS(NBUSE),CIRC(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: DELBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(IN)    :: H(*),ZF(*),U(*),V(*)
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: T
      DOUBLE PRECISION , INTENT(IN)    :: CV(NBUSE),C56(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: CV5(NBUSE),C5(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: CTRASH(NBUSE),FRICBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: LONGBUS(NBUSE)
      INTEGER          , INTENT(IN)    :: OPTBUSE
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: V2DPAR
      INTEGER          , INTENT(IN)    :: MAXSOURCE
      INTEGER, INTENT(IN), OPTIONAL    :: NPTSCE,NPOIN2
      INTEGER, INTENT(IN), OPTIONAL    :: KSCE(MAXSOURCE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I1,I2,ITRAC,FTYP
      INTEGER VOFFSET
!
      DOUBLE PRECISION L, CTH
      DOUBLE PRECISION LARG,HAUT1,HAUT2,HAUT,TETA
      DOUBLE PRECISION S1,S2,CE1,CE2,CS1,CS2,Q,QMAX1,QMAX2
      DOUBLE PRECISION RD1,RD2,RD
      DOUBLE PRECISION FRIC,LONG,HAST,RAYON,TRASH,RADI1,RADI2
      DOUBLE PRECISION CORR56,CORRV5,VALVE,CORR5,CIR
!
      DOUBLE PRECISION PI
      DOUBLE PRECISION TWOTHIRDS,FOURTHIRDS
      DOUBLE PRECISION D1,D2,H1,H2
!
      INTRINSIC SQRT,COS,SIN,MIN,MAX,ABS,ACOS

!     TRS water level calculators
      INTEGER :: TRS,IN,OUT,CLPB
      DOUBLE PRECISION :: TMP_IN,TMP_OUT,TMP_W_IN,TMP_W_OUT
      DOUBLE PRECISION :: SUM_IN,SUM_OUT,SUM_W,T_AREA,S_AREA
      DOUBLE PRECISION :: AREA1,AREA2
      DOUBLE PRECISION :: T_SF

!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
      TWOTHIRDS  = 2.D0/3.D0
      FOURTHIRDS = 4.D0/3.D0
      CTH = AT/3600.0         ! Current time in hours, starts from 0
!
!-----------------------------------------------------------------------
!
! Initialise the tidal range schemes if they are included


!-----------------------------------------------------------------------
! LOOP OVER THE CULVERTS
!
! DEFAULT OPERATION
      ! INITIALISE TRS
      IF (MAXVAL(CLPBUS).GT.3) THEN
        ! Set the flex values
        CALL TRS_FLEX_VALUES(CTH)
        ! Get the up and downstream water levels of the schemes
        ! This has to be done here as we call many buse and telemac variables
        DO TRS=1,NUM_TRS
          WL_IN(TRS) = 0.0
          WL_OUT(TRS) = 0.0
          SUM_IN = 0.0
          SUM_OUT = 0.0
          SUM_W = 0.0
          Q_TURB(TRS) = 0.0
          Q_SLUICE(TRS) = 0.0
          POWER(TRS) = 0.0
          DO K=1,NBUSE
            IF(CTRASH(K).EQ.TRS) THEN
              IN=ENTBUS(K)
              OUT=SORBUS(K)
              ! IF(NCSIZE.GT.1) THEN
              !   IN = P_MAX(IN) + P_MIN(IN)
              !   OUT = P_MAX(OUT) + P_MIN(OUT)
              ! ENDIF
              IF (IN.GT.0) THEN
                TMP_IN = H(IN) + ZF(IN)
              ELSE
                TMP_IN = 0.0
              ENDIF
              IF (OUT.GT.0) THEN
                TMP_OUT = H(OUT) + ZF(OUT)
              ELSE
                TMP_OUT = 0.0
              ENDIF

              IF (NCSIZE.GT.1) THEN
                TMP_IN = P_MIN(TMP_IN) + P_MAX(TMP_IN)
                TMP_OUT = P_MIN(TMP_OUT) + P_MAX(TMP_OUT)
              ENDIF

              SUM_IN = SUM_IN + TMP_IN*CV(K)
              SUM_OUT = SUM_OUT + TMP_OUT*CV(K)
              SUM_W = SUM_W + CV(K)
            ENDIF
          ENDDO
          WL_IN(TRS) = SUM_IN/SUM_W
          WL_OUT(TRS)= SUM_OUT/SUM_W
          IF (SUM_W.EQ.0) THEN
            WRITE(*,*)'!!!!!! WARNING SUM_W = 0 !!!!!!!!!!!!!!!'
          ENDIF
          HEADDIFF(TRS) = WL_IN(TRS) - WL_OUT(TRS)
    !       WRITE(*,*)'TRS: ',TRS,' SUM_W: '
    !  &    ,SUM_W,' HEADDIFF: ',HEADDIFF(TRS)
        ENDDO
        ! Set a new mode
        CALL TRS_NEW_MODE(CTH)
        CALL TRS_BASE_FLOWS()
        ! WRITE(*,*)
      ENDIF

      DO N=1,NBUSE
!       IDENTIFIES ENTRY / EXIT NODES
!
!       NUMBER OF THE POINTS
        I1=ENTBUS(N)
        I2=SORBUS(N)
        ! IF(NCSIZE.GT.1) THEN
        !   I1 = P_MAX(I1) + P_MIN(I1)
        !   I2 = P_MAX(I2) + P_MIN(I2)
        ! ENDIF
!       LOADS, TAKEN AS FREE SURFACE ELEVATION
!
        IF(I1.GT.0) THEN
          S1=H(I1)+ZF(I1)
          QMAX1=0.9D0*H(I1)*V2DPAR%R(I1)/DT
        ELSE
          S1=0.D0
          QMAX1=0.D0
        ENDIF

        IF(I2.GT.0) THEN
          S2=H(I2)+ZF(I2)
          QMAX2=0.9D0*H(I2)*V2DPAR%R(I2)/DT
        ELSE
          S2=0.D0
          QMAX2=0.D0
        ENDIF
!       CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN
        IF(NCSIZE.GT.1) THEN
          S1=P_MAX(S1)+P_MIN(S1)  ! WATER LEVEL @ INSIDE NODE
          S2=P_MAX(S2)+P_MIN(S2)  ! WATER LEVEL @ OUTSIDE NODE
          QMAX1=P_MAX(QMAX1)+P_MIN(QMAX1)
          QMAX2=P_MAX(QMAX2)+P_MIN(QMAX2)
        ENDIF
!
!       COEFFICIENTS FOR COMPUTATION OF PRESSURE LOSS
!
        CE1=CEBUS(N,1)    ! TRS -> Ebb head loss coeff when generating/suicing
        CE2=CEBUS(N,2)    ! TRS -> Flood head loss coeff when generating/suicing
        CS1=CSBUS(N,1)    ! TRS -> Ebb head loss coeff when pumping
        CS2=CSBUS(N,2)    ! TRS -> Flood head loss coeff when pumping
        L=LBUS(N)         ! TRS -> 
        RD1=ALTBUS(N,1)   ! TRS -> Turbine draft tube area at p1
        RD2=ALTBUS(N,2)   ! TRS -> Turbine draft tube area at p2
        RD=0.5D0*(RD1+RD2)! TRS -> 
        LARG=LRGBUS(N)    ! TRS -> Width of sluice gate/turbine diameter
        HAUT1=HAUBUS(N,1) ! TRS -> 
        HAUT2=HAUBUS(N,2) ! TRS -> 
        RADI1=0.5D0*HAUT1 ! TRS -> 
        RADI2=0.5D0*HAUT2 ! TRS -> 
        FRIC=FRICBUS(N)   ! TRS -> 
        LONG=LONGBUS(N)   ! TRS -> 
        TRASH=CTRASH(N)   ! TRS -> 
        CORR56=C56(N)     ! TRS -> 
        VALVE=CV(N)       ! TRS -> 
        CORRV5=CV5(N)     ! TRS -> Turbine type: 1 = Turb+Pump, 2 = Turb Only, 3 = Pump Only
        CORR5=C5(N)       ! TRS -> Number of turbines/pumps at given node.
        CIR=CIRC(N)       ! TRS -> 
        HAUT=MIN(HAUT1,HAUT2)
        D1=DELBUS(N,1)    ! TRS -> 
        D2=DELBUS(N,2)    ! TRS -> 
        H1=HAUT1*COS(D1)  ! TRS -> 
        H2=HAUT2*COS(D2)  ! TRS -> 
        CLPB=CLPBUS(N)    ! TRS -> Flow control type. Culvert type: 4 = control, 5 = Turbine, 6 = Sluice, [0,1,2,3] as before

!       COMPUTES FLOWS BASED ON THE TIDAL RANGE SCHEME ASSUMPTIONS
        IF (CLPB.GE.5) THEN
          TRS = INT(TRASH)
          Q = 0.0 ! FLOW THROUGH THIS CULVERT
          !       NEED TO CHECK IF IN/OUT WORKS WITH THE REST OF THE CODE
          ! IN=ENTBUS(N)
          ! OUT=SORBUS(N)
          I1=ENTBUS(N)
          I2=SORBUS(N)

          ! IF(NCSIZE.GT.1) THEN
          !   I1 = P_MAX(I1) + P_MIN(I1)
          !   I2 = P_MAX(I2) + P_MIN(I2)
          ! ENDIF
    !       WRITE(*,*)'TRS: ',TRS,' NODE: ',N,' I1: ',
    !  &  I1,' I2: ',I2,' HD: ',HEADDIFF(TRS)
!       CLP     Flow control type. Culvert type: 4 = control, 5 = Turbine, 6 = Sluice, [0,1,2,3] as before
!       CV5     Turbine type: 1 = Turb+Pump, 2 = Turb Only, 3 = Pump Only
!       FLOW CALCS
!       MAX VOLUME CHANGE CHECKS
!       USE THE LOCAL COEFFS WHERE POSSIBLE
          TRS = TRASH ! Haha.
          SELECT CASE (CLPB)
          CASE(4) ! Control point
            Q = 0.0 ! no flow, ever

          CASE(5) ! Turbine
            T_AREA = 0.25*PI*LARG*LARG        ! Turbine area (m2)
            T_SF = (LARG**2)/(ORIG_DIAM_T(TRS)**2)*CORR5 ! Turbine scale factor
            ! Ramp * Scale Factor * Flow
            SELECT CASE (MODE(TRS))
              CASE (0)  ! Initial warmup
              !DOUBLE PRECISION FUNCTION TRS_ORIFICE(CD,AREA,HDIFF) RESULT(FLOW)
                Q = TRS_RAMP(FLEX_TIMES(1,TRS),PHASETIME(TRS))*
     &            TRS_ORIFICE(CE1,T_AREA,HEADDIFF(TRS))*T_SF

              CASE (1)  ! High water hold
                ! Ramp down turbine flow if appropriate
                IF (PHASETIME(TRS).LT.RAMPTIME(TRS)) THEN
                  IF ((ISPUMPING(TRS)).AND.
     &               ((CORRV5.EQ.1).OR.(CORRV5.EQ.3))) THEN
                    ! ramp down pump flow
                    Q = (1.0 - RAMP(TRS))*QP_BASE(TRS)*T_SF*CS2
                  ELSE
                    ! If parallel sluice
                    IF(CORR56.EQ.1) THEN
                      Q = (1.0 - RAMP(TRS))*QG_BASE(TRS)*T_SF*CE2
                    ELSE
                      Q = (1.0 - RAMP(TRS))*
     &                  TRS_ORIFICE(CE1,T_AREA,HEADDIFF(TRS))
                    ENDIF
                  ENDIF
                ELSE
                  Q = 0.0
                ENDIF

              CASE (2)  ! Ebb generation
                Q = RAMP(TRS)*QG_BASE(TRS)*T_SF*CE1 ! Normal flow

              CASE (3)  ! Ebb sluicing
                IF (CORR56.EQ.1) THEN
                  Q = RAMP(TRS)*QG_BASE(TRS)*T_SF*CE1
                ELSE
                  ! ramp down into 0.0
                  Q = TRS_ORIFICE(CE1,T_AREA,HEADDIFF(TRS))
                ENDIF

              CASE (-1) ! Ebb pumping
                IF ((CORRV5.EQ.1).OR.(CORRV5.EQ.3)) THEN
                  Q = RAMP(TRS)*QP_BASE(TRS)*T_SF*CS1
                ENDIF

              CASE (4)  ! Low water hold
              ! Ramp down turbine flow if appropriate
              IF (PHASETIME(TRS).LT.RAMPTIME(TRS)) THEN
                IF ((ISPUMPING(TRS)).AND.
     &              ((CORRV5.EQ.1).OR.(CORRV5.EQ.3))) THEN
                  Q = (1.0 - RAMP(TRS))*QP_BASE(TRS)*T_SF*CS1
                ELSE
                  IF(CORR56.EQ.1) THEN
                    Q = (1.0 - RAMP(TRS))*QG_BASE(TRS)*T_SF*CE1
                  ELSE
                    Q = (1.0 - RAMP(TRS))*
     &                TRS_ORIFICE(CE2,T_AREA,HEADDIFF(TRS))
                  ENDIF
                ENDIF
              ELSE
                Q = 0.0
              ENDIF

              CASE (5)  ! Flood generation
                Q = RAMP(TRS)*QG_BASE(TRS)*T_SF*CE2

              CASE (6)  ! Flood sluicing
                ! If parallel sluicing (and thus generating)
                IF (CORR56.EQ.1) THEN
                  Q = QG_BASE(TRS)*T_SF*CE2
                ELSE
                  ! Turbine operates as sluice
                  Q = TRS_ORIFICE(CE2,T_AREA,HEADDIFF(TRS))
                ENDIF



              CASE (-2) ! Flood pumping
                Q = RAMP(TRS)*QG_BASE(TRS)*T_SF*CS2

              CASE DEFAULT ! Error
              WRITE(*,*)'SOME KIND OF TRS MODE ERROR IN TURB FLOW CALC'

            END SELECT

          CASE(6) ! Sluice

          ! Determine sluice area based on shape
            IF (CIR.EQ.1) THEN
              S_AREA = 0.25*PI*LARG*LARG        ! Sluice area (m2)
            ELSEIF (CIR.EQ.0) THEN
!             ZF = ELEVATION OF BOTTOM
!             Assume the area in the sluice flow is the Base * mean WL
              TMP_D1 = 0.0
              TMP_D2 = 0.0
              IF (I1.GT.0) TMP_D1 = ZF(I1)
              IF (I2.GT.0) TMP_D2 = ZF(I2)

              IF (NCSIZE.GT.1) THEN
                TMP_D1 = P_MIN(TMP_D1) + P_MAX(TMP_D1)
                TMP_D2 = P_MIN(TMP_D2) + P_MAX(TMP_D2)
              ENDIF
            ! CALC SLUICE AREA AS MEAN SWL - MEAN BED LEVEL
              S_AREA = LARG*
     &          (WL_IN(TRS) + WL_OUT(TRS)/2 -
     &          (TMP_D1 + TMP_D2)/2)
            ELSE
              ! You can add more area calc options here if desired
              ! ASSUME SQUARE
              S_AREA = LARG*LARG
            ENDIF

            ! Orifice equation correctly constructed.
            SELECT CASE (MODE(TRS))
              CASE (0)  ! Initial warmup, slow ramp up to get motion etc.
                Q = RAMP(TRS)*
     &            TRS_ORIFICE(WARMUP(TRS),S_AREA,HEADDIFF(TRS))

              CASE (1)  ! High water hold
                ! Ramp down as the sluices close
                IF (PHASETIME(TRS).LT.RAMPTIME(TRS)) THEN
                  IF (ISPUMPING(TRS)) THEN
                    ! for simplicity no sluice flow when pumping
                    Q = 0.0
                  ELSE
                    ! ramp down
                    Q = (1.0 - RAMP(TRS))*
     &                TRS_ORIFICE(CE2,S_AREA,HEADDIFF(TRS))
                  ENDIF
                ELSE
                  ! No flow, normal hold
                  Q = 0.0
                ENDIF

              CASE (2)  ! Ebb generation, no sluice flow by default
                Q = 0.0

              CASE (3)  ! Ebb sluicing, ramp up into normal
                Q = RAMP(TRS)*TRS_ORIFICE(CE1,S_AREA,HEADDIFF(TRS))

              CASE (-1) ! Ebb pumping
                Q = 0.0

              CASE (4)  ! Low water hold, same as high water hold
                IF (PHASETIME(TRS).LT.RAMPTIME(TRS)) THEN
                  IF (ISPUMPING(TRS)) THEN
                    Q = 0.0
                  ELSE
                    Q = (1.0 - RAMP(TRS))*
     &                TRS_ORIFICE(CE1,S_AREA,HEADDIFF(TRS))
                  ENDIF
                ELSE
                  Q = 0.0
                ENDIF

              CASE (5)  ! Flood generation, no sluice flow by default
                  Q = 0.0

              CASE (6)  ! Flood sluicing, ramp up into normal
                Q = RAMP(TRS)*TRS_ORIFICE(CE2,S_AREA,HEADDIFF(TRS))

              CASE (-2) ! Flood pumping, no flow.
                Q = 0.0

              CASE DEFAULT ! you added a new mode somewhere and did not add it here..?
                WRITE(*,*) 'MODE ERROR', MODE(TRS),
     &                     ' IN SLUICE FLOW CALC CULV [',N,']'
            END SELECT

          CASE DEFAULT
            IF ((CLPB.GT.6).OR.(CLPB.LE.0)) THEN
              ! Some kind of error
              WRITE(*,*)'INVALID CULVERT (CLP) VALUE [',
     &                   CLPB,']'
            ELSE
              ! Culvert operates as usual, copy the normal buse functionality into here?
            ENDIF
          END SELECT
        ! Apply the calculated flow to the culvert with relaxation
        DBUS(N)= (1.D0-RELAXB)*DBUS(N) + RELAXB*Q

!       LIMITATION ON AVAILABLE WATER
!       Same as prior
        IF(DBUS(N).GT.0.D0) THEN
          DBUS(N)=MIN(QMAX1,DBUS(N))
        ELSE
          DBUS(N)=MAX(-QMAX2,DBUS(N))
        ENDIF
!       LIMITATION ON WATER LEVEL SHUTOFF
!       USES NODAL WATER LEVEL
        IF ((HAUT1.GE.S1).OR.(HAUT2.GE.S2)) THEN
          DBUS(N) = 0.0
        ENDIF
!       CALCULATE POWER IF NEEDED
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
        IF (ENTET) THEN
          CALL TRS_POWER(HEADDIFF(TRS),T_SF,TRS,CLPB,CORR56,CORRV5)
          ! CALL TRS_POWER(DBUS(N),T_SF,TRS,CLPB,CORR56,CORRV5)
          CALL TRS_FLOW(DBUS(N),TRS,CLPB)
        ENDIF

!       FLOW VELOCITY CALCS
        IF(DBUS(N).GT.0.D0) THEN

          UBUS(2,N) = (COS(D2)*DBUS(N)/RD2)*COS(ANGBUS(N,2))
          VBUS(2,N) = (COS(D2)*DBUS(N)/RD2)*SIN(ANGBUS(N,2))

          IF(NCSIZE.GT.1) THEN
            UBUS(1,N) = P_MAX(UBUS(1,N))+P_MIN(UBUS(1,N))
            VBUS(1,N) = P_MAX(VBUS(1,N))+P_MIN(VBUS(1,N))
          ENDIF

        ELSEIF(DBUS(N).LT.0.D0) THEN
          UBUS(1,N) = (COS(D1)*DBUS(N)/RD1)*COS(ANGBUS(N,1))
          VBUS(1,N) = (COS(D1)*DBUS(N)/RD1)*SIN(ANGBUS(N,1))
          IF(I2.GT.0) THEN
            ! NO CLUE WHAT KSCE IS OR DOES...
            IF (PRESENT(KSCE)) THEN
              VOFFSET = (KSCE(NPTSCE+NBUSE+N)-1)*NPOIN2
            ELSE
              VOFFSET = 0
            ENDIF
            UBUS(2,N) = U(I2+VOFFSET)
            VBUS(2,N) = V(I2+VOFFSET)
          ELSE
            UBUS(2,N) = 0.D0
            VBUS(2,N) = 0.D0
          ENDIF
          IF(NCSIZE.GT.1) THEN
            UBUS(2,N) = P_MAX(UBUS(2,N))+P_MIN(UBUS(2,N))
            VBUS(2,N) = P_MAX(VBUS(2,N))+P_MIN(VBUS(2,N))
          ENDIF

        ELSEIF(DBUS(N).EQ.0.D0) THEN
          UBUS(1,N) = 0.D0
          VBUS(1,N) = 0.D0
          UBUS(2,N) = 0.D0
          VBUS(2,N) = 0.D0
          IF(NCSIZE.GT.1) THEN
            UBUS(1,N) = P_MAX(UBUS(1,N))+P_MIN(UBUS(1,N))
            VBUS(1,N) = P_MAX(VBUS(1,N))+P_MIN(VBUS(1,N))
            UBUS(2,N) = P_MAX(UBUS(2,N))+P_MIN(UBUS(2,N))
            VBUS(2,N) = P_MAX(VBUS(2,N))+P_MIN(VBUS(2,N))
          ENDIF
        ENDIF

        ELSE
!       ORGINAL BUSE FORMULATION
!
!       COMPUTES THE FLOW ACCORDING TO DELTAH
!       IF THE LINEAR PRESSURE LOSS IS NEGLIGIBLE, COULD HAVE DIFFERENT
!       ENTRY / EXIT SECTIONS
!
!       FIRST OPTION FOR CULVERTS: WORKS ONLY WITH CIRCULAR ONES
        IF(OPTBUSE.EQ.1) THEN
!
          IF(S1.GE.S2) THEN
!
            IF(S1.GT.RD1.AND.S1.GT.RD2) THEN
!
              IF(S1.LT.(RD1+H1).AND.S1.LT.(RD2+H2)) THEN
!             FREE SURFACE FLOW WHICH FOLLOWS A WEIR LAW
                IF(S2.GT.(TWOTHIRDS*(S1-RD))+RD) THEN
                  IF(CIR.GT.0.D0) THEN
                    SECBUS(N) = PI*0.5D0*RADI2*(S2-RD2)
                  ELSE
                    SECBUS(N) = LARG*(S2-RD2)
                  ENDIF
                  Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)/(CE1+L+CS2))
                ELSE
                  IF(CIR.GT.0.D0) THEN
                    SECBUS(N) = PI*0.5D0*RADI1*(S1-RD1)
                  ELSE
                    SECBUS(N) = LARG*(S1-RD1)
                  ENDIF
                  Q = SECBUS(N)*SQRT(2.D0*GRAV)*SQRT((S1-RD1))*0.385D0
                ENDIF
              ELSE
!               PRESSURE FLOW --> ORIFICE LAW
                IF(CIR.GT.0.D0) THEN
                  SECBUS(N) = PI*RADI1**2.D0
                ELSE
                  SECBUS(N) = LARG*HAUT1
                ENDIF
                IF(S1.GE.(RD1+H1).AND.S2.LT.(RD2+H2)) THEN
                  Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-(RD2+H2))
     &              /(1.D0+CE1+L))
                ELSE
                  Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)/(L+CS2+CE1))
                ENDIF
              ENDIF
            ELSE
              Q = 0.D0
            ENDIF
!
          ELSE
!
            IF(S2.GT.RD1.AND.S2.GT.RD2) THEN

              IF(S2.LT.(RD1+H1).AND.S2.LT.(RD2+H2)) THEN
!               FREE SURFACE FLOW WHICH FOLLOWS A WEIR LAW
                IF(S1.GT.(TWOTHIRDS*(S2-RD1)+RD1)) THEN
                  IF(CIR.GT.0.D0) THEN
                    SECBUS(N) = PI*0.5D0*RADI1*(S1-RD1)
                  ELSE
                    SECBUS(N) = LARG*(S1-RD1)
                  ENDIF
                  Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)/(CE2+L+CS1))
                ELSE
                  IF(CIR.GT.0.D0) THEN
                    SECBUS(N) = PI*0.5D0*RADI1*(S2-RD2)
                  ELSE
                    SECBUS(N) = LARG*(S2-RD2)
                  ENDIF
                  Q = -SECBUS(N)*SQRT(2.D0*GRAV)*SQRT((S2-RD2))*0.385D0
                ENDIF
              ELSE
!               PRESSURE FLOW --> ORIFICE LAW
                IF(CIR.GT.0.D0) THEN
                  SECBUS(N) = PI*RADI1**2.D0
                ELSE
                  SECBUS(N) = LARG*HAUT1
                ENDIF
                IF(S2.GE.(RD2+H2).AND.S1.LT.(RD1+H1)) THEN
                  Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-(RD1+H1))
     &              /(1.D0+CE2+L))
                ELSE
                  Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)/(L+CS1+CE2))
                ENDIF
              ENDIF
            ELSE
              Q=0.D0
            ENDIF
          ENDIF
!
!       Q IS CALCULATED ACCORDING TO 5 TYPES OF FLOW
!       CALCULATION OF DISCHARGES BASED ON WATER LEVELS S1 AND S2
!       EQUATIONS BASED ON BODHAINE (1968) + CARLIER (1976)
!       IN CASE THE ENTRANCE AND EXIT SURFACE OF THE CULVERT ARE NOT EQUAL
!       WE ASSUME THAT THE SMALLEST SURFACE WILL LIMIT THE FLOW THROUGH
!       THE CULVERT. THIS SURFACE IS THUS TAKEN TO CALCULATE THE DISCHARGE
        ELSEIF(OPTBUSE.EQ.2) THEN
!
!         WL CHANNEL HIGHER THAN WL FCA; ONLY INFLOW POSSIBLE
          IF(S1.GE.S2) THEN
!           IF WL CHANNEL IS GREATER THAN BOTTOM OF CULVERT THEN ...
            IF(S1.GT.RD1.AND.S1.GT.RD2) THEN
!
!             FREE SURFACE FLOW
              IF((S1-RD1).LT.1.5D0*H1.AND.S2.LE.(RD2+H2)) THEN
!
!               SUBMERGED WEIR - FLOW TYPE 3
                IF(S2.GT.(TWOTHIRDS*(S1-RD1)+RD2)) THEN
                  FTYP=3
                  HAST = 0.5D0*(S1-RD)+0.5D0*(S2-RD)
                  RAYON = HAST*LARG/(2.D0*HAST+LARG)
                  L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                  IF(CIR.GT.0.D0) THEN
                  ! FORMULA FOR SECTION OF CIRCLE; HEIGHT IS S2-RD2;
                  ! BUT HERE SUBMERGED THUS CALCULATED WITH
                  ! S2-RD2 AND EQUIVALENT WIDTH
                    SECBUS(N) = PI*0.5D0*RADI2*(S2-RD)
                  ELSE
                  ! FOR THIS FORMULA THE CROSS SECTION CAN BE
                  ! LARGER THAN THE ACTUAL CROSS SECTION
                    SECBUS(N) = LARG*(S2-RD)
                  ENDIF
                  Q=SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)/(CE1+L+CS2+TRASH))
!
!               UNSUBMERGED WEIR - FLOW TYPE 2
                ELSE !IF(S2.LE.(TWOTHIRDS*(S1-RD)+RD)) THEN
                  FTYP=2
                  HAST = 0.5D0*TWOTHIRDS*(S1-RD)+0.5D0*(S1-RD)
                  RAYON = HAST*LARG/(2.D0*HAST+LARG)
                  L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                  IF(CIR.GT.0.D0) THEN
                    ! FORMULA FOR SECTION OF CIRCLE; HEIGHT IS S2-RD2
                    IF((S1-RD1).LT.RADI1) THEN
                      TETA = 2.D0*ACOS((RADI1-(S1-RD1))/RADI1)
                      SECBUS(N) = ACOS((RADI1-(S1-RD1))/RADI1)
     &                          *(RADI1**2)- 0.5D0*RADI1*SIN(TETA/2)
     &                          *(RADI1-(S1-RD1))
                    ELSEIF((S1-RD1).EQ.RADI1) THEN
                      SECBUS(N) = PI*(RADI1**2)*0.5D0
                    ELSEIF((S1-RD1).GT.RADI1
     &                .AND.(S1-RD1).LT.H1) THEN
                      TETA = 2.D0*ACOS(((S1-RD1)-RADI1)/RADI1)
                      SECBUS(N) = (PI*(RADI1**2))-(ACOS(((S1-RD1)-RADI1)
     &                          / RADI1)*(RADI1**2)
     &                          - 0.5D0*RADI1*SIN(TETA/2)
     &                          *((S1-RD1)-RADI1))
                    ELSEIF((S1-RD1).GE.H1) THEN
                      SECBUS(N) = PI*(RADI1**2)
                    ENDIF
                  ELSE
                    SECBUS(N) = LARG*(TWOTHIRDS*(S1-RD))
                  ENDIF
                  Q = SECBUS(N)
     &               *SQRT(2.D0*GRAV*(S1-(RD+TWOTHIRDS*(S1-RD)))
     &                     /(CE1+L+CS2+TRASH))
                ENDIF
!
!             PRESSURE FLOW --> ORIFICE LAW
              ELSEIF((S1-RD1).GE.1.5D0*H1.AND.S2.LE.(RD2+H2)) THEN
!               FLOW TYPE 6
                IF(LONG.GE.CORR56*HAUT1) THEN
                  FTYP=6
                  HAST = HAUT1
                  RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                  L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                  IF(CIR.GT.0.D0) THEN
                    SECBUS(N) = PI*RADI1**2
                  ELSE
                    SECBUS(N) = LARG*HAUT1
                  ENDIF
                  Q = SECBUS(N)
     &               *SQRT(2.D0*GRAV*(S1-(RD2+HAUT))/(CE1+L+CS2+TRASH))
!             FLOW TYPE 5
                ELSEIF(LONG.LT.CORR56*HAUT1) THEN
                  FTYP=5
                  HAST = HAUT1
                  RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                  L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                  IF(CIR.GT.0.D0) THEN
                    SECBUS(N) = PI*RADI1**2
                  ELSE
                    SECBUS(N) = LARG*HAUT1
                  ENDIF
                  Q=SECBUS(N)*SQRT(2.D0*GRAV*(S1-RD)/(CORR5*CE1+TRASH))
                ENDIF
!           FLOW TYPE 4 SUBMERGED OUTLET
              ELSEIF(S1.GT.(RD1+H1).AND.S2.GT.(RD2+H2)) THEN
                FTYP=4
                HAST = HAUT1
                RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                IF(CIR.GT.0.D0) THEN
                  SECBUS(N) = PI*RADI1**2
                ELSE
                  SECBUS(N) = LARG*HAUT1
                ENDIF
                Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)/(CE1+L+CS2+TRASH))
              ENDIF
!           IF WL ON BOTH SIDES IS LOWER THAN THE CULVERT
            ELSE
              FTYP=0
              Q=0.D0
            ENDIF
!
!         IF S1 IS SMALLER THAN S2; SO ONLY OUTLET FLOW
          ELSE
            IF(S2.GT.RD2.AND.S2.GT.RD1) THEN
!             FREE SURFACE FLOW
              IF((S2-RD2).LT.1.5D0*H2.AND.S1.LE.(RD1+H1)) THEN
!               SUBMERGED WEIR - FLOW TYPE 3
                IF(S1.GT.(TWOTHIRDS*(S2-RD2)+RD1)) THEN
                  FTYP=-3
                  HAST = 0.5D0*(S2-RD)+0.5D0*(S1-RD)
                  RAYON = HAST*LARG/(2.D0*HAST+LARG)
                  L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                  IF(CIR.GT.0.D0) THEN
                    ! FORMULA FOR SECTION OF CIRCLE; HEIGHT IS S2-RD2
                    SECBUS(N) = PI*0.5D0*RADI1*(S1-RD)
                  ELSE
                    SECBUS(N) = LARG*(S1-RD)
                  ENDIF
                  Q = -SECBUS(N)
     &                *SQRT(2.D0*GRAV*(S2-S1)/(CE2+L+VALVE+CS1+TRASH))
!               UNSUBMERGED WEIR - FLOW TYPE 2
                ELSE !IF(S1.LE.(TWOTHIRDS*(S2-RD)+RD)) THEN
                  FTYP=-2
                  HAST = 0.5D0*TWOTHIRDS*(S2-RD)+0.5D0*(S2-RD)
                  RAYON = HAST*LARG/(2.D0*HAST+LARG)
                  L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                  IF(CIR.GT.0.D0) THEN
                    ! FORMULA FOR SECTION OF CIRCLE; HEIGHT IS S2-RD2
                    IF((S2-RD2).LT.RADI2) THEN
                      TETA = 2.D0*ACOS((RADI2-(S2-RD2))/RADI2)
                      SECBUS(N) = ACOS((RADI2-(S2-RD2))/RADI2)
     &                          *(RADI2**2)- (0.5D0*RADI2*SIN(TETA/2)
     &                          *(RADI2-(S2-RD2)))
                    ELSEIF((S2-RD2).EQ.RADI2) THEN
                      SECBUS(N) = PI*(RADI2**2)*0.5D0
                    ELSEIF((S2-RD2).GT.RADI2
     &                .AND.(S2-RD2).LT.H2) THEN
                      TETA = 2.D0*ACOS(((S2-RD2)-RADI2)/RADI2)
                      SECBUS(N) = (PI*(RADI2**2))-(ACOS(((S2-RD2)-RADI2)
     &                          / RADI2)*(RADI2**2)- 0.5D0*RADI2
     &                          *SIN(TETA/2)*((S2-RD2)-RADI2))
                    ELSEIF((S2-RD2).GE.H2) THEN
                      SECBUS(N) = PI*(RADI2**2)
                    ENDIF
                  ELSE
                    SECBUS(N) = LARG*TWOTHIRDS*(S2-RD)
                  ENDIF
                  Q = -SECBUS(N)
     &                *SQRT(2.D0*GRAV*(S2-(RD+TWOTHIRDS*(S2-RD)))
     &                               /(CE2+VALVE+L+CS1+TRASH))
                ENDIF
!             PRESSURE FLOW --> ORIFICE LAW
              ELSEIF((S2-RD2).GE.1.5D0*H2.AND.S1.LE.(RD1+H1)) THEN
!               FLOW TYPE 6
                IF(LONG.GE.CORR56*HAUT2) THEN
                  FTYP=-6
                  HAST = HAUT2
                  RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                  L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                  IF(CIR.GT.0.D0) THEN
                    SECBUS(N) = PI*RADI2**2
                  ELSE
                    SECBUS(N) = LARG*HAUT2
                  ENDIF
                  Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-(RD1+HAUT))
     &                                         /(CE2+VALVE+L+CS1+TRASH))
!                 FLOW TYPE 5
                ELSEIF(LONG.LT.CORR56*HAUT2) THEN
                  FTYP=-5
                  HAST = HAUT2
                  RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                  L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                  IF(CIR.GT.0.D0) THEN
                    SECBUS(N) = PI*RADI2**2
                  ELSE
                    SECBUS(N) = LARG*HAUT2
                  ENDIF
                  Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-RD)
     &                                  /(CORR5*CE2+CORRV5*VALVE+TRASH))
                ENDIF
!             FLOW TYPE 4 SUBMERGED OUTLET
              ELSEIF(S2.GT.(RD2+H2).AND.S1.GT.(RD1+H1)) THEN
                FTYP=-4
                HAST = HAUT2
                RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                IF(CIR.GT.0.D0) THEN
                  SECBUS(N) = PI*RADI2**2
                ELSE
                  SECBUS(N) = LARG*HAUT2
                ENDIF
                Q = -SECBUS(N)
     &              *SQRT(2.D0*GRAV*(S2-S1)/(CE2+VALVE+L+CS1+TRASH))
              ENDIF
!           IF THE WATER DOES NOT REACH HIGH ENOUGH TO ENTER THE CULVERT
            ELSE
              FTYP=0
              Q = 0.D0
            ENDIF
          ENDIF
!       WRONG CHOICE OF OPTBUSE VALUE
        ELSE
          WRITE(LU,*) 'WRONG CHOICE OF OPTBUSE VALUE'
          CALL PLANTE(1)
          STOP
        ENDIF
        ENDIF
!
!       NOTHING HAPPENS IF THE LOADS AT THE 2 ENDS ARE LOWER THAN
!       THE ELEVATION OF THE NOZZLES
!
        IF(S1.LT.RD1.AND.S2.LT.RD2) Q=0.D0
        IF(HAUT1.EQ.0.D0.OR.HAUT2.EQ.0.D0) Q=0.D0
!
!       FILLS OUT DBUS(N) USING RELAXATION
!
        DBUS(N)= RELAXB*Q + (1.D0-RELAXB)*DBUS(N)
!
!       LIMITATION WITH AVAILABLE WATER
!
        IF(DBUS(N).GT.0.D0) THEN
          DBUS(N)=MIN(QMAX1,DBUS(N))
        ELSE
          DBUS(N)=MAX(-QMAX2,DBUS(N))
        ENDIF
!
!       SLUICE VALVE TREATMENT (DIRECTIONALITY)
!
        IF((CLPBUS(N).EQ.1.AND.S2.GT.S1).OR.
     &     (CLPBUS(N).EQ.2.AND.S1.GT.S2).OR.
     &     (CLPBUS(N).EQ.3).OR.
     &     (CLPBUS(N).EQ.4))
     &     DBUS(N) = 0.D0
!
!       PRINT FLOW RATES VALUES FOR EACH ACTIVE CULVERT
! AT is the telemac model time in seconds.
        IF((ENTET.AND.ABS(DBUS(N)).GT.1.D-4).AND.
     &    (MAXVAL(CLPBUS).LE.3)) THEN
          WRITE(LU,*)'CULV: ',N,', Q = ',DBUS(N),' M³/S'
        ENDIF
!
!  TREATS THE VELOCITIES AT THE SOURCES
!  SAME APPROACH FOR VELOCITY AND TRACER
!
        IF(DBUS(N).GT.0.D0) THEN
          UBUS(2,N) = (COS(D2)*DBUS(N)/SECBUS(N)) * COS(ANGBUS(N,2))
          VBUS(2,N) = (COS(D2)*DBUS(N)/SECBUS(N)) * SIN(ANGBUS(N,2))
          IF(I1.GT.0) THEN
            IF (PRESENT(KSCE)) THEN
              VOFFSET = (KSCE(NPTSCE+N)-1)*NPOIN2
            ELSE
              VOFFSET = 0
            ENDIF
            UBUS(1,N) = U(I1+VOFFSET)
            VBUS(1,N) = V(I1+VOFFSET)
          ELSE
            UBUS(1,N) = 0.D0
            VBUS(1,N) = 0.D0
          ENDIF
          IF(NCSIZE.GT.1) THEN
            UBUS(1,N) = P_MAX(UBUS(1,N))+P_MIN(UBUS(1,N))
            VBUS(1,N) = P_MAX(VBUS(1,N))+P_MIN(VBUS(1,N))
          ENDIF
        ELSEIF(DBUS(N).LT.0.D0) THEN
          UBUS(1,N) = (COS(D1)*DBUS(N)/SECBUS(N)) * COS(ANGBUS(N,1))
          VBUS(1,N) = (COS(D1)*DBUS(N)/SECBUS(N)) * SIN(ANGBUS(N,1))
          IF(I2.GT.0) THEN
            IF (PRESENT(KSCE)) THEN
              VOFFSET = (KSCE(NPTSCE+NBUSE+N)-1)*NPOIN2
            ELSE
              VOFFSET = 0
            ENDIF
            UBUS(2,N) = U(I2+VOFFSET)
            VBUS(2,N) = V(I2+VOFFSET)
          ELSE
            UBUS(2,N) = 0.D0
            VBUS(2,N) = 0.D0
          ENDIF
          IF(NCSIZE.GT.1) THEN
            UBUS(2,N) = P_MAX(UBUS(2,N))+P_MIN(UBUS(2,N))
            VBUS(2,N) = P_MAX(VBUS(2,N))+P_MIN(VBUS(2,N))
          ENDIF
        ELSEIF(DBUS(N).EQ.0.D0) THEN
          UBUS(1,N) = 0.D0
          VBUS(1,N) = 0.D0
          UBUS(2,N) = 0.D0
          VBUS(2,N) = 0.D0
          IF(NCSIZE.GT.1) THEN
            UBUS(1,N) = P_MAX(UBUS(1,N))+P_MIN(UBUS(1,N))
            VBUS(1,N) = P_MAX(VBUS(1,N))+P_MIN(VBUS(1,N))
            UBUS(2,N) = P_MAX(UBUS(2,N))+P_MIN(UBUS(2,N))
            VBUS(2,N) = P_MAX(VBUS(2,N))+P_MIN(VBUS(2,N))
          ENDIF
        ENDIF
!
!       TREATS THE TRACER :
!       NOTA : NBUSE + N <==> N,2
!                      N <==> N,1
!
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            IF(DBUS(N).GE.0.D0) THEN ! I1 --> I2
!             CASE DBUS(N)=0.D0 NOT CLEAR, BUT A VALUE HAS TO BE
!             GIVEN HERE, LEST IT IS USED AFTER
              IF(I1.GT.0) THEN
                IF (PRESENT(KSCE)) THEN
                  VOFFSET = (KSCE(NPTSCE+N)-1)*NPOIN2
                ELSE
                  VOFFSET = 0
                ENDIF
                TBUS%ADR(ITRAC)%P%R(NBUSE+N) = T%ADR(ITRAC)%P%R(I1
     &                                       + VOFFSET)
                TBUS%ADR(ITRAC)%P%R(N)       = T%ADR(ITRAC)%P%R(I1
     &                                       + VOFFSET)
              ELSE
                TBUS%ADR(ITRAC)%P%R(NBUSE+N) = 0.D0
                TBUS%ADR(ITRAC)%P%R(N)       = 0.D0
              ENDIF
            ELSE ! I2 --> I1
              IF(I2.GT.0) THEN
                IF (PRESENT(KSCE)) THEN
                  VOFFSET = (KSCE(NPTSCE+NBUSE+N)-1)*NPOIN2
                ELSE
                  VOFFSET = 0
                ENDIF
                TBUS%ADR(ITRAC)%P%R(N)       = T%ADR(ITRAC)%P%R(I2
     &                                       + VOFFSET)
                TBUS%ADR(ITRAC)%P%R(NBUSE+N) = T%ADR(ITRAC)%P%R(I2
     &                                       + VOFFSET)
              ELSE
                TBUS%ADR(ITRAC)%P%R(N)       = 0.D0
                TBUS%ADR(ITRAC)%P%R(NBUSE+N) = 0.D0
              ENDIF
            ENDIF
            IF(NCSIZE.GT.1) THEN
              TBUS%ADR(ITRAC)%P%R(NBUSE+N)=
     &          P_MAX(TBUS%ADR(ITRAC)%P%R(NBUSE+N))
     &         +P_MIN(TBUS%ADR(ITRAC)%P%R(NBUSE+N))
              TBUS%ADR(ITRAC)%P%R(N)      =
     &          P_MAX(TBUS%ADR(ITRAC)%P%R(N))
     &         +P_MIN(TBUS%ADR(ITRAC)%P%R(N))
            ENDIF
          ENDDO
        ENDIF ! End TRS vs normal culvert loop
!
!  END OF THE LOOP OVER THE CULVERTS
!
      ENDDO ! N
      IF (IPID.EQ.0) THEN
        IF(ENTET) THEN
          CALL TRS_WRITE_STATUS(CTH)
          K = NEWUNIT()
          CALL TRS_WRITE_RESULTS(CTH, K)
        ENDIF
      ENDIF
      END SUBROUTINE BUSE
