!                   *****************
                    SUBROUTINE LECBUS
!                   *****************
!
     &(RELAXB,NBUSE,ENTBUS,SORBUS,LRGBUS,HAUBUS,CLPBUS,
     & ALTBUS,CSBUS,CEBUS,ANGBUS,LBUS,IFIC,MESH,
     & CV,C56,CV5,C5,CTRASH,FRICBUS,LONGBUS,CIRC,DELBUS,
     & AABUS)
!
!***********************************************************************
! TELEMAC2D   V7P2                                   20/11/2015
!***********************************************************************
!
!brief    READS THE DATA FOR CULVERTS/TUBES/BRIDGES.
!
!history  C.COULET (ARTELIA)
!+        23/05/2012
!+        V6P2
!+   Creation
!
!history  J-M HERVOUET (LNHE)
!+        30/07/2012
!+        V6P2
!+   Parallelism
!
!history S SMOLDERS
!+       20/11/15
!+       V7P1
!+   Adding global variables
!+   CV,C56,CV5,C5,CTRASH,FRICBUS,LONGBUS
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AABUS          |<--| HORIZONTAL ANGLE OF THE PIPE USER DEFINED OR
!|                |   | CALCULATED BASED ON THE MESH
!| ALTBUS         |<--| ELEVATION OF ENTRY AND EXIT OF CULVERTS
!| ANGBUS         |<--| ANGLE OF CULVERTS WITH AXIS OX
!|                |   |   AUTOMATICALLY COMPUTED BY DEFAULT
!| C5             |<--| CORRECTION COEFFICIENT FOR FLOW TYPE 5
!| C56            |<--| COEFFICIENT TO DIFFERENTIATE BETWEEN FLOW TYPE 5
!|                |   | AND 6
!| CEBUS          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN INFLOW
!| CIRC           |<--| CULVERT ROUND (=1) OR RECTANGULAR (=0)
!| CLPBUS         |<--| INTEGER FLAG FOR FLOW DIRECTION (VALVE)
!|                |   |   0 - BOTH DIRECTIONS
!|                |   |   1 - ONLY FROM ENTRY TO EXIT
!|                |   |   2 - ONLY FROM EXIT TO ENTRY
!|                |   |   3 - NO FLOW
!| CSBUS          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN OUTFLOW
!| CTRASH         |<--| HEAD LOSS COEFFICIENT FOR TRASH SCREEN
!| CV             |<--| HEAD LOSS COEFFICIENT OF VALVE
!| CV5            |<--| CORRECTION COEFFICIENT FOR FLOW TYPE 5
!| ENTBUS         |<--| INDICES OF ENTRY OF CULVERTS IN GLOBAL NUMBERING
!| FRICBUS        |<--| MANNING COEFFICIENT FOR WATER FLOWING
!|                |   | OVER CULVERT MATERIAL
!| HAUBUS         |<--| HEIGHT OF CULVERTS
!| IFIC           |-->| LOGICAL UNIT OF CULVERTS DATA FILE
!| LBUS           |<--| LINEAR HEAD LOSS OF CULVERTS
!| LONGBUS        |<--| LENGTH OF CULVERTS
!| LRGBUS         |<--| WIDTH OF CULVERTS
!| MESH           |-->| MESH STRUCTURE
!| NBUSE          |-->| NUMBER OF CULVERTS
!| RELAXB         |<--| RELAXATION COEFFICIENT.
!| SORBUS         |<--| INDICES OF CULVERTS EXITS IN GLOBAL MESH NUMBERING
!| DELBUS         |<--| ANGLE OF THE PIPES WITH THE VERTICAL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN
      USE TRS_T2D
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: IFIC,NBUSE
      INTEGER          , INTENT(INOUT) :: ENTBUS(NBUSE),SORBUS(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: RELAXB
      DOUBLE PRECISION , INTENT(INOUT) :: HAUBUS(NBUSE,2),LRGBUS(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: ALTBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(INOUT) :: ANGBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(INOUT) :: CEBUS(NBUSE,2),CSBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(INOUT) :: LBUS(NBUSE)
      INTEGER          , INTENT(INOUT) :: CLPBUS(NBUSE),CIRC(NBUSE)
      TYPE(BIEF_MESH)  , INTENT(IN)    :: MESH
      DOUBLE PRECISION , INTENT(INOUT) :: CV(NBUSE),C56(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: CV5(NBUSE),C5(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: CTRASH(NBUSE),FRICBUS(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: LONGBUS(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: DELBUS(NBUSE,2)
      INTEGER          , INTENT(INOUT) :: AABUS(NBUSE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NUMBUSE,N_GROUP,N
!
      DOUBLE PRECISION XSOR,YSOR,XENT,YENT
      DOUBLE PRECISION ANG1,ANG2
      DOUBLE PRECISION DX,DY,ANG,CE1,CE2,CS1,CS2
      DOUBLE PRECISION HAU1,HAU2,ALT1,ALT2
      DOUBLE PRECISION DELTA1,DELTA2
!
      DOUBLE PRECISION PI
      DOUBLE PRECISION, PARAMETER:: EPSL=1.D-12
      INTRINSIC     SIGN
      DOUBLE PRECISION XSOR_G(NBUSE),YSOR_G(NBUSE)
      DOUBLE PRECISION XENT_G(NBUSE),YENT_G(NBUSE)
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
!
!-----------------------------------------------------------------------
!
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=998) RELAXB,NUMBUSE
      READ(IFIC,*,END=900)
!
      IF (NUMBUSE.NE.NBUSE) THEN
        WRITE(LU,*) 'LECBUS: NUMBER OF CULVERTS:',NUMBUSE
        WRITE(LU,*) '        DIFFERENT FROM THE ONE GIVEN IN THE'
        WRITE(LU,*) '        STEERING FILE: ',NBUSE
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO N=1,NBUSE
        READ(IFIC,*,ERR=997) ENTBUS(N),SORBUS(N),
     &                       CE1,CE2,CS1,CS2,
     &                       LRGBUS(N),HAU1,
     &                       CLPBUS(N),LBUS(N),
     &                       ALT1,ALT2,
     &                       CV(N),C56(N),CV5(N),
     &                       C5(N),CTRASH(N),HAU2,
     &                       FRICBUS(N),LONGBUS(N),CIRC(N),
     &                       DELTA1,DELTA2,ANG1,ANG2,AABUS(N)
        CEBUS(N,1)=CE1
        CEBUS(N,2)=CE2
        CSBUS(N,1)=CS1
        CSBUS(N,2)=CS2
        HAUBUS(N,1)=HAU1
        HAUBUS(N,2)=HAU2
        ALTBUS(N,1)=ALT1
        ALTBUS(N,2)=ALT2
        DELBUS(N,1) = DELTA1*PI/180.D0
        DELBUS(N,2) = DELTA2*PI/180.D0
!
!       IN // GLOBAL VALUES REPLACED BY THE LOCAL VALUES FOR FURTHER USE
        ENTBUS(N)=GLOBAL_TO_LOCAL_POINT(ENTBUS(N),MESH)
        SORBUS(N)=GLOBAL_TO_LOCAL_POINT(SORBUS(N),MESH)
!
!       DIRECTION OF FLOW SET BASED ON THE MESH
        IF(ENTBUS(N).GT.0) THEN
          XENT=MESH%X%R(ENTBUS(N))
          YENT=MESH%Y%R(ENTBUS(N))
        ELSE
          XENT=0.D0
          YENT=0.D0
        ENDIF
        IF(SORBUS(N).GT.0) THEN
          XSOR=MESH%X%R(SORBUS(N))
          YSOR=MESH%Y%R(SORBUS(N))
        ELSE
          XSOR=0.D0
          YSOR=0.D0
        ENDIF
        IF(NCSIZE.GT.1) THEN
          XENT=P_MAX(XENT)+P_MIN(XENT)
          YENT=P_MAX(YENT)+P_MIN(YENT)
          XSOR=P_MAX(XSOR)+P_MIN(XSOR)
          YSOR=P_MAX(YSOR)+P_MIN(YSOR)
        ENDIF
        IF (AABUS(N).EQ.1) THEN
          DX  = XSOR-XENT
          DY  = YSOR-YENT
          ANG = ATAN(DY/SIGN(MAX(ABS(DX),EPSL),DX))
          ANGBUS(N,1) = ANG
          ANGBUS(N,2) = ANG
!       ANGLE BASED ON GROUP (NH 25/08/2022)
        ELSEIF (AABUS(N).EQ.3) THEN
          ! RECORD ALL THE POSITIONS OF THE POINTS
          XENT_G(N) = XENT
          YENT_G(N) = YENT
          XSOR_G(N) = XSOR
          YSOR_G(N) = YSOR
!       DIRECTION OF FLOW SET BASED ON THE DATA FILE
        ELSE
          ANGBUS(N,1) = ANG1*PI/180.D0
          ANGBUS(N,2) = ANG2*PI/180.D0
        ENDIF
      ENDDO !  N
      IF (MAXVAL(AABUS).GE.3) THEN
        ! FOR EVERY CULVERT
        DO N=1,NBUSE
          ! IF ITS A MEAN-TYPE CULVERT
          IF (AABUS(N).EQ.3) THEN
            ! INITIALISE SUMMARISERS
            XENT = 0.0
            YENT = 0.0
            XSOR = 0.0
            YSOR = 0.0
            N_GROUP = 0
            ! FOR EVERY CULVERT (INCLUDING ITSELF)
            DO K=1,NBUSE
              ! IF THAT OTHER CULVERT IS AN AUTO CULVERT
              IF (AABUS(K).EQ.3) THEN
                ! AND IF THAT CULVERT IS OF THE SAME TYPE
                IF (CTRASH(N).EQ.CTRASH(K)) THEN
                  IF (CLPBUS(N).EQ.CLPBUS(K)) THEN
                    IF (CV5(N).EQ.(CV5(K))) THEN
                      ! ADD THE CONTRIBUTION OF SAID CULVERT
                      XENT = XENT + XENT_G(K)
                      YENT = YENT + YENT_G(K)
                      XSOR = XSOR + XSOR_G(K)
                      YSOR = YSOR + YSOR_G(K)
                      N_GROUP = N_GROUP + 1
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDDO

            DX  = XSOR/N_GROUP-XENT/N_GROUP
            DY  = YSOR/N_GROUP-YENT/N_GROUP
            ANG = ATAN(DY/SIGN(MAX(ABS(DX),EPSL),DX))
            ANGBUS(N,1) = ANG
            ANGBUS(N,2) = ANG
          ENDIF
        ENDDO
      ENDIF

      IF (MAXVAL(CLPBUS).GT.3) THEN

        IF (TRS_FIRST) THEN
          ! IF ((NCSIZE.EQ.0).OR.((NCSIZE.GT.1).AND.(IPID.EQ.0))) THEN
            ! As CTRASH is a real, round it down to int, assumes that
            ! the proper values are less than 2...
            NUM_TRS = INT(MAXVAL(CTRASH))
            IF (IPID.EQ.0) THEN
              WRITE(*,*) "+----------------------------------------+"
              WRITE(*,*) "    ",NUM_TRS,"    SCHEMES"
              WRITE(*,*) "+----------------------------------------+"
              WRITE(*,*) "|:          TIDAL      .::.              |"
              WRITE(*,*) "| :  ____   RANGE     .:  :.             |"
              WRITE(*,*) "| ':/    \  MODEL     :    :             |"
              WRITE(*,*) "|  /.     \          :     '.   _____    |"
              WRITE(*,*) "| / :      \G       .'      :  /     \   |"
              WRITE(*,*) "|/  ':      \E      :       ':/       \  |"
              WRITE(*,*) "|    :       \N    :'        :         \ |"
              WRITE(*,*) "|     :       \   .:        / :         \|"
              WRITE(*,*) "|'''''':'''''''\'':''''''''/''':'''''''''|"
              WRITE(*,*) "|      :        \.:       /    :         |"
              WRITE(*,*) "|       :        \       /     ':       :|"
              WRITE(*,*) "|       '.      :'\     /       :.     .:|"
              WRITE(*,*) "|        :.    .:  \___/         :.   .: |"
              WRITE(*,*) "|         :.  .:    HOLD          :...:  |"
              WRITE(*,*) "|          '::'                          |"
              WRITE(*,*) "+----------------------------------------+"
            ENDIF
            ! Read all the TRS-X.txt files...
            ! First find a free unit
            TMP_INT = NEWUNIT()
            ! Read the files
            CALL TRS_READ_DATAFILES(TMP_INT)
            ! IF (IPID.EQ.0) WRITE(*,*) 'ALL TRS FILES READ'
            CALL TRS_FLEX_VALUES(0.D0)
            TRS_FIRST = .FALSE.
          ENDIF
      ENDIF
      GO TO 1000
!
!-----------------------------------------------------------------------
!     ERROR MESSAGES
!-----------------------------------------------------------------------
!
998   CONTINUE
      WRITE(LU,*) 'LECBUS: READ ERROR ON THE'
      WRITE(LU,*) '        CULVERTS DATA FILE'
      WRITE(LU,*) '        AT LINE 2'
      CALL PLANTE(1)
      STOP
!
997   CONTINUE
      WRITE(LU,*) 'LECBUS: READ ERROR ON THE'
      WRITE(LU,*) '        CULVERTS DATA FILE'
      WRITE(LU,*) '        FOR CULVERT NUMBER ',N
      WRITE(LU,*) '        THE DATA CANNOT BE READ'
      CALL PLANTE(1)
      STOP
!
900   CONTINUE
      WRITE(LU,*) 'LECBUS: READ ERROR ON THE'
      WRITE(LU,*) '        CULVERTS DATA FILE'
      WRITE(LU,*) '        UNEXPECTED END OF FILE'
      CALL PLANTE(1)
      STOP
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
