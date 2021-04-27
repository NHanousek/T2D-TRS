
!
      MODULE INPOUT_VARIABLES

      LOGICAL, SAVE    :: first = .true.
      LOGICAL, SAVE    :: first1 = .true.
      LOGICAL, SAVE    :: first2 = .true.
      INTEGER,SAVE     ::     NPAIRS_T   ! For reading table_turbine.txt file
      REAL, dimension(:), allocatable, save   :: dzw_T, WL_Tur_power   !
      INTEGER         :: status
      INTEGER          k, row, col  !iterators
      INTEGER, SAVE   :: tphase_old   ! OLD value (used in previous timestep)
      INTEGER         :: tphase_new   ! NEW value (to be used in this timestep)
      REAL            :: H_ref
      REAL            :: WL_in_NEW      ! Water Level Downstream
      REAL, SAVE      :: WL_in_old      ! Save WL_down to be used in the next step
      REAL            :: WL_out_new      ! Same as above but for upstream
      REAL, SAVE      :: WL_out_old      ! Same as above but for upstream
      INTEGER,SAVE    :: N_level
      REAL            :: H_ebb_Pumping,H_EBB_SLUICING
      REAL            :: H_FLOOD_PUMPING,H_FLOOD_SLUICING
      INTEGER, DIMENSION(:), ALLOCATABLE, SAVE   :: upWL_represent
      INTEGER, DIMENSION(:), ALLOCATABLE, SAVE   :: downWL_represent
      INTEGER, DIMENSION(:), ALLOCATABLE, SAVE   :: P_out,P_in,N_loops
      REAL            :: BBB
      INTEGER         :: NFO1,kk
      INTEGER, SAVE   :: N_turbines
      REAL,SAVE       :: cd_turbine
      REAL,SAVE       :: Area_turbine
      REAL            :: differ_out_wl
      REAL            :: differ_in_wl
      REAL,SAVE       :: Area_Sluice
      REAL,SAVE       :: Cd_Sluice,cd_pumping
      INTEGER,SAVE    :: lines_read
      INTEGER, SAVE   :: N_Sluice
      REAL,SAVE       :: R_turbines
!      REAL,SAVE       :: H_start
!      REAL,SAVE       :: H_end
      INTEGER,SAVE       :: P_out_tur, P_out_slu
      INTEGER,SAVE       :: P_in_tur,P_in_slu,N_power
      REAL,SAVE       :: ramp_time
      REAL,SAVE       :: timsec_1  !!timsec_1 means the start time of a new phase
      REAL            :: phase_time
      REAL,SAVE       :: opnRmp,clsRmp !reduction factors for the ramps
      REAL,SAVE       :: dz, CT
      REAL            :: AAA,CCC    ! Intermediate value for calculation of discharge from 1 to 2
      REAL            :: P    ! Power
      REAL,SAVE       :: P_accumulate,Efficency_Pumping
      REAL,SAVE       :: H_ebb_start,H_ebb_end
      REAL,SAVE       :: H_flood_start, H_tur, H_slu
      REAL,SAVE       :: H_flood_end, dz_tur, dz_slu
      INTEGER,SAVE    :: Mode, Lines_buse
      INTEGER,SAVE    :: N_Halftidalloop, N_loop,data_record
      REAL, dimension(:), allocatable, save   ::Start_times, end_times
      REAL, dimension(:), allocatable, save   ::DZmins, DZmaxs
      REAL            :: Start_time,end_time,DZmin,DZmax,pump_target
!
      INTEGER, pointer,SAVE :: P_turbines_in(:)
      INTEGER, pointer,SAVE :: P_turbines_out(:)
      INTEGER, pointer,SAVE :: P_sluice_in(:)
      INTEGER, pointer,SAVE :: P_sluice_out(:)
      INTEGER          :: J,GLOBALN,LOCALN,N_Line_buse
!      INTEGER          :: I_Down,I_up
      DOUBLE PRECISION, SAVE :: Area_Cul,AIRE,S_out,S_in,dz_n,Z_IN,Z_OUT
      DOUBLE PRECISION, SAVE :: TMP_REAL1,TMP_REAL2
      LOGICAL, SAVE  :: first3 = .true.

!     modified/new variables for multiple hill charts NH Nov/2020
      INTEGER, SAVE :: N_HillCharts_Q,N_HillCharts_P !Number of hill charts
      INTEGER, dimension(:), allocatable, save :: hillCharts !Which one to use
      INTEGER :: hill_chart    !current hill chart
      INTEGER :: TRSF !Tidal range scheme data file
      REAL :: Outside, Inside, HeadDiff, QTurb, QSluice, PowerMW !for file
      REAL, dimension(:,:), allocatable, save   ::disch_T, Tur_power !2D matrices
      REAL, SAVE :: HT1, HT2, MULT, MLT !for warmup, high tide 1, high tide 2, multiplier
      REAL, SAVE :: MAX_HOLD = 6*3600 !Hours
      REAL, SAVE :: MAX_PUMP = 3.0*3600
      REAL, SAVE :: MAX_GEN  = 12.0*3600
      LOGICAL :: TMP_LOG
!     Variables for pumping (mostly)
      REAL, dimension(:,:), allocatable, save   :: Q_Pump,P_Pump  !
      REAL, dimension(:), allocatable, SAVE ::pump_targets,pumps,H_pump
      INTEGER, SAVE    :: N_PUMPS,N_Pumping,pump
      LOGICAL, Save    :: is_pumping = .FALSE.
      INTEGER, Save       :: prnt_time
      DOUBLE PRECISION, SAVE :: CT_H,DT_H
      END MODULE



!                    ***************
                     SUBROUTINE BUSE
!                    ***************
!
     &(RELAXB,NBUSE,ENTBUS,SORBUS,GRAV,
     & H,ZF,DBUS,LRGBUS,HAUBUS,CLPBUS,
     & ALTBUS,CSBUS,CEBUS,ANGBUS,LBUS,
     & NTRAC,T,TBUS,UBUS,VBUS,U,V,ENTET,
     & CV,C56,CV5,C5,CTRASH,FRICBUS,LONGBUS,
     & CIRC,OPTBUSE,V2DPAR,DT,SECBUS,MAXSOURCE,
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
!| DT             |-->| TIME STEP
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
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DFO1,T2DRFO
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DMAX,P_DMIN

      USE INPOUT_VARIABLES
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
      DOUBLE PRECISION L,LARG,HAUT1,HAUT2,HAUT,TETA
      DOUBLE PRECISION S1,S2,CE1,CE2,CS1,CS2,Q,QMAX1,QMAX2
      DOUBLE PRECISION RD1,RD2,RD
      DOUBLE PRECISION FRIC,LONG,HAST,RAYON,TRASH,RADI1,RADI2
      DOUBLE PRECISION CORR56,CORRV5,VALVE,CORR5,CIR
!
      DOUBLE PRECISION PI
      DOUBLE PRECISION TWOTHIRDS,FOURTHIRDS
!
      INTRINSIC SQRT,COS,SIN,MIN,MAX,ABS,ACOS
!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
      TWOTHIRDS  = 2.D0/3.D0
      FOURTHIRDS = 4.D0/3.D0
!
!-----------------------------------------------------------------------
!
! LOOP OVER THE CULVERTS
!

      NFO1=T2D_FILES(T2DFO1)%LU  !FORMATTED DATA FILE: T2DFO1 = 26
      REWIND NFO1   ! statement positions the file associated with the specified unit to its initial point.

      IF(first) then
      READ(NFO1,*) Mode   !The operation mode, 1. Ebb;  2;ebb with pumping; 3,two-way; 4, two way with pumping;  5, optimised head
      WRITE(*,*)  'Mode',Mode
      READ(NFO1,*) Lines_buse   !the number of turbines,16
      WRITE(*,*)  'Lines_buse',Lines_buse
      READ(NFO1,*) N_turbines   !the number of turbines,16
      WRITE(*,*)  'N_turbines',N_turbines
      READ(NFO1,*) R_turbines   !The Diameter of turbine
      WRITE(*,*)  'R_turbines',R_turbines
      READ(NFO1,*) Area_turbine !area of single Turbine
      WRITE(*,*)  'Area_turbine',Area_turbine
      READ(NFO1,*) cd_turbine   ! Discharge coefficient for Turbine 1
      WRITE(*,*)  'cd_turbine',cd_turbine
      READ(NFO1,*) N_Sluice     !the number of sluicegate, 8
      WRITE(*,*)  'N_Sluice',N_Sluice
      READ(NFO1,*) Area_Sluice  !area of single Sluice gate
      WRITE(*,*)  'Area_Sluice',Area_Sluice
      READ(NFO1,*) Cd_Sluice    !Discharge coefficient for Sluice gate
      WRITE(*,*)  'Cd_Sluice',Cd_Sluice
      READ(NFO1,*) H_ebb_start     !The head of tidal when start ebb generating
      WRITE(*,*)  'H_ebb_start ',H_ebb_start
      READ(NFO1,*) H_ebb_end       !the head of tidal when end the ebb generation phase
      WRITE(*,*)  'H_ebb_end',H_ebb_end
      READ(NFO1,*) H_ebb_Sluicing
      WRITE(*,*)  'H_ebb_Sluicing',H_ebb_Sluicing
      READ(NFO1,*) H_ebb_Pumping
      WRITE(*,*)  'H_ebb_Pumping',H_ebb_Pumping
      READ(NFO1,*) H_flood_start     !The head of tidal when satart flooding generating
      WRITE(*,*)  'H_flood_start ',H_flood_start
      READ(NFO1,*)  H_flood_end       !the head of tidal when end the flooding generation phase
      WRITE(*,*)  'H_flood_end', H_flood_end
      READ(NFO1,*) H_flood_Sluicing
      WRITE(*,*)  'H_flood_Sluicing',H_flood_Sluicing
      READ(NFO1,*) H_flood_Pumping
      WRITE(*,*)  'H_flood_Pumping',H_flood_Pumping
      READ(NFO1,*) N_level      !the number of points at each side of lagoon that used to calculate the water level
      WRITE(*,*)  'N_level',N_level
      READ(NFO1,*) lines_read    !the start line of water represent points in <Culvertdata>
      WRITE(*,*)  'lines_read',lines_read
      READ(NFO1,*) ramp_time
      ramp_time = ramp_time*60.D0 ! Change to seconds
      WRITE(*,*) 'Ramp Time (s):',ramp_time
      READ(NFO1,*) tphase_old
      WRITE(*,*)  'tphase_old',tphase_old
      READ(NFO1,*) AIRE
      WRITE(*,*) 'AIRE',AIRE
      READ(NFO1,*) HT1
      WRITE(*,*)  'HT1',HT1
      READ(NFO1,*) HT2
      WRITE(*,*)  'HT2',HT2
      READ(NFO1,*) MULT
      WRITE(*,*)  'MULT',MULT
      READ(NFO1,*)            ! Empty line for reading easier
!--------------------------READ HQ-----------------------------------
      READ(NFO1,*) NPAIRS_T   ! For reading table_turbine.txt file
      IF (status==0) THEN
            WRITE(*,*) 'NPAIRS_T (HQ):',NPAIRS_T
            ALLOCATE(dzw_T(NPAIRS_T), stat=status)! The dimension of difference of water level in hill chart
      ELSE
      IF (status/=0) THEN
            WRITE(*,*) 'Status=',status
            WRITE(*,*) 'Memory allocation error (Turbine Hillchart)'
      CALL PLANTE(1)
      ENDIF
      ENDIF
      DO row=1,NPAIRS_T
            READ(NFO1,*, iostat=status)dzw_T(row)
      ENDDO
      READ(NFO1,*)            ! Empty line for reading easier
      READ(NFO1,*) N_HillCharts_Q
      IF (status==0) Then
            ALLOCATE(disch_T(NPAIRS_T,N_HillCharts_Q), stat=status)  ! the  Discharge in hill-chart
      ELSE
            IF (status/=0) THEN
            WRITE(*,*) 'Memory allocation error (Turbine_Hillchart.txt)'
            CALL PLANTE(1)
            ENDIF
      ENDIF
      DO row=1,NPairs_T
            READ(NFO1,*) (disch_T(row,col),col=1,N_HillCharts_Q)
      ENDDO
      WRITE(*,*) 'Row DZ Q1 Q2 ... Qn-1 Qn'
      DO row=1,NPAIRS_T
            WRITE(*,'(I5,F10.2)',advance='no') row,dzw_T(row)
            DO col = 1,N_HillCharts_Q
            !READ(NFO1,'(F5.2)', iostat=status,advance='no') disch_T(row,col)
                  WRITE(*,'(F10.2)',advance='no') disch_T(row,col)
            ENDDO
            !READ(NFO1,*)
            WRITE(*,*) ' '
      ENDDO
      READ(NFO1,*)            ! Empty line for reading easier
!------------------------------READ HP-------------------------------
      READ(NFO1,*) N_power   ! For reading table_turbine.txt file
      IF (status==0) THEN
            WRITE(*,*)  'N_power (HP):',N_power
            ALLOCATE(WL_Tur_power(N_power), stat=status) ! The dimension of difference of water level in hill chart
      ELSE
            IF (status/=0) THEN
            WRITE(*,*) 'Status=',status
            WRITE(*,*) 'Memory allocation error (Turbine Hillchart)'
            CALL PLANTE(1)
            ENDIF
      ENDIF
      DO row=1,N_power
            READ(NFO1,*, iostat=status) WL_Tur_power(row)
      ENDDO
      READ(NFO1,*)            ! Empty line for reading easier
      READ(NFO1,*) N_HillCharts_P
      IF (status==0) Then
            ALLOCATE(Tur_power(N_power, N_HillCharts_P), stat=status)  ! the  Discharge in hill-chart
      ELSE
            IF (status/=0) THEN
            WRITE(*,*) 'Memory allocation error (Turbine_Hillchart.txt)'
            CALL PLANTE(1)
            ENDIF
      ENDIF
      DO row=1,N_Power
            READ(NFO1,*) (Tur_Power(row,col),col=1,N_HillCharts_P)
      ENDDO
      WRITE(*,*) 'Row DZ P1 P2 ... Pn-1 Pn'
      DO row = 1,N_power
            WRITE(*,'(I2,F10.2)', advance="no") row,WL_Tur_power(row)
            DO col = 1,N_HillCharts_P
            !READ(NFO1,'(F7.2)', iostat=status,advance='no') Tur_power(row,col)
                  WRITE(*,'(F10.2)',advance="no") Tur_power(row,col)
            ENDDO
            WRITE(*,*) ' '
      ENDDO
      !READ(NFO1,*)
      READ(NFO1,*)          ! Empty line for reading easier
      READ(NFO1,*) N_PUMPS
      READ(NFO1,*) N_Pumping  !The number of pump curve points
      IF (status==0) Then
            ALLOCATE(H_Pump(N_Pumping), stat=status)
            ALLOCATE(Q_Pump(N_PUMPS,N_PUMPING), stat=status)
            ALLOCATE(P_Pump(N_PUMPS,N_PUMPING), stat=status)
      ELSE
            WRITE(*,*) 'Memory allocation error (Turbine_Hillchart.txt)'
            CALL PLANTE(1)
      ENDIF
      DO ROW = 1,N_PUMPING
            READ(NFO1,*)H_PUMP(ROW)
      ENDDO
      READ(NFO1,*)            !Blank or commented line Pump flows
      DO ROW = 1,N_PUMPING
            READ(NFO1,*) (Q_PUMP(ROW,COL),COL=1,N_PUMPS)
      ENDDO
      WRITE(*,*)'Pump Flows (m3/s)'
      DO ROW = 1,N_PUMPING
            WRITE(*,'(I2,F10.2)',advance="no")ROW,H_PUMP(ROW)
            DO col = 1,N_PUMPS
            !READ(NFO1,'(F7.2)', iostat=status,advance='no') Tur_power(row,col)
                  WRITE(*,'(F10.2)',advance="no") Q_PUMP(row,col)
            ENDDO
            WRITE(*,*) ' '
      ENDDO
      READ(NFO1,*)            !Blank or commented line Pump flows
      DO ROW = 1,N_PUMPING
            READ(NFO1,*) (P_PUMP(ROW,COL),COL=1,N_PUMPS)
      ENDDO
      WRITE(*,*)'Pump Powers (MW)'
      DO ROW = 1,N_PUMPING
            WRITE(*,'(I2,F10.2)',advance="no")ROW,H_PUMP(ROW)
            DO col = 1,N_PUMPS
            !READ(NFO1,'(F7.2)', iostat=status,advance='no') Tur_power(row,col)
                  WRITE(*,'(F10.2)',advance="no") P_PUMP(row,col)
            ENDDO
            WRITE(*,*) ' '
      ENDDO
      READ(NFO1,*)            ! Empty line for reading easier
      READ(NFO1,*) N_Halftidalloop   ! For reading table_turbine.txt file
      WRITE(*,*)  'N_Halftidalloop',N_Halftidalloop
      ALLOCATE(N_loops(N_Halftidalloop), stat=status)
      ALLOCATE(Start_times(N_Halftidalloop), stat=status)
      ALLOCATE(end_times(N_Halftidalloop), stat=status)
      ALLOCATE(DZmins(N_Halftidalloop), stat=status)
      ALLOCATE(DZmaxs(N_Halftidalloop), stat=status)
      ALLOCATE(HillCharts(N_Halftidalloop), stat=status)
      ALLOCATE(pump_targets(N_Halftidalloop), stat=status)
      ALLOCATE(pumps(N_Halftidalloop), stat=status)
      IF (status/=0) THEN
            WRITE(*,*) 'Memory allocation error (Turbine_Hillchart.txt)'
            CALL PLANTE(1)
      ENDIF
      DO k = 1,N_Halftidalloop
            READ(NFO1,*, iostat=status) N_loops(k), Start_times(k),
     &      end_times(k),DZmins(k),DZmaxs(k),HillCharts(k),
     &      pump_targets(k),pumps(k)
      ENDDO

      DO k = 1,N_Halftidalloop
            Write(*,*) N_loops(k), Start_times(k),
     &      end_times(k),DZmins(k),DZmaxs(k),HillCharts(k),
     &      pump_targets(k),pumps(k)
      ENDDO
      CLOSE(NFO1)

      !TRSF, this should find an unused file between 10 and 1000 to use
      DO TRSF=10,1000
      	inquire(unit=TRSF, opened=TMP_LOG)
      	IF ( .not. TMP_LOG) THEN
      	    EXIT
      	ENDIF
      ENDDO

      OPEN(unit=TRSF,file='../TRS-data.csv', status='unknown')
      WRITE(TRSF,*)'Time(s),S_Out(mD),S_In(mD),',
     &      'HeadDiff(m),Mode(-),QTurb(m3_s),QSluice(m3_s),',
     &      'Power(MW),CHART(-)'
      CLOSE(TRSF)
!
!      P_out_tur=ENTBUS(1+lines_read)
!      P_in_tur=SORBUS(1+lines_read)
!      P_out_slu=ENTBUS(2+lines_read)
!      P_in_slu=SORBUS(2+lines_read)
      WRITE(*,*) 'n ENTBUS, SORBUS (from culvert data file)'
      DO row=1,NBUSE
            WRITE(*,*)row,ENTBUS(row),SORBUS(row)
      ENDDO
      ALLOCATE(P_out(N_level), stat=status)
      ALLOCATE(P_in(N_level), stat=status)
      WRITE(*,*) 'Link Point P_Out  P_In'
       Do k=1,N_level         !Just choose 6 Points, Change This part if need it in Future
          P_out(k)=ENTBUS(k+lines_read)
          P_in(k)=SORBUS(k+lines_read)
       Write(*,*) k,P_out(k), P_in(k)
      ENDDO

      CT=0
      CT_H=0
      DT_H=DT/3600.0
      prnt_time=0
      P_accumulate=0
      first = .false.
      ENDIF
      CT=CT+DT
      CT_H=CT_H+DT_H
      phase_time = CT - timsec_1   !timsec_1 means the start time of a new phase

       IF(Mode==5) THEN

       IF ((CT_H).LE.Start_times(1)) THEN
         H_ebb_end = DZmins(1)
         H_ebb_start=DZmaxs(1)
         hill_chart = HillCharts(1)
         pump_target = pump_targets(1)
         pump = pumps(1)
      ELSEIF (((CT_H).GT.Start_times(1))
     & .and. ((CT_H).LE.end_times(N_Halftidalloop))) THEN
         Do k=1, N_Halftidalloop
         IF (((CT_H).GT.Start_times(k))
     &       .and. ((CT_H).LE.end_times(k))) THEN
          N_loop=N_loops(k)
          Start_time=Start_times(k)
          end_time=end_times(k)
          H_ebb_end=DZmins(k)
          H_ebb_start=DZmaxs(k)
          hill_chart=HillCharts(k)
          pump_target=pump_targets(k)
          pump = pumps(k)
        ENDIF
       ENDDO
       ENDIF
       ENDIF
!       H_tur=(H(P_out_tur)+ZF(P_out_tur))-((H(P_in_tur)+ZF(P_in_tur)))
!       H_slu=(H(P_out_slu)+ZF(P_out_slu))-((H(P_in_slu)+ZF(P_in_slu)))
!       dz_tur=abs(H_tur)
!       dz_slu=abs(H_slu)

!         S_out: points at upstream(outside Lagoon)
!         S_in: Points at downstream(inside Lagoon)

            S_OUT = 0
            S_IN  = 0

            !NH - Red-did this calculation because I think the previous was not working
            DO k=1, N_LEVEL
                TMP_REAL1 = H(P_out(k))+ZF(P_out(k))
                TMP_REAL2 = H(P_in(k))+ZF(P_in(k))
                IF(NCSIZE.GT.1) THEN
                      TMP_REAL1 = P_DMIN(TMP_REAL1) + P_DMAX(TMP_REAL1)
                      TMP_REAL2 = P_DMIN(TMP_REAL2) + P_DMAX(TMP_REAL2)
                ENDIF
                S_OUT = S_OUT + TMP_REAL1
                S_IN  = S_IN  + TMP_REAL2
            ENDDO
            S_OUT = S_OUT/N_LEVEL !the average water level outside lagoon(downstream)
            S_IN = S_IN/N_LEVEL

           WL_out_new=S_out     ! water level outside
           WL_in_NEW=S_in       ! water level inside
           H_ref=S_in-S_out     ! Head difference (upstream - downstream) (inside - outside)
           dz=abs(H_ref)        ! absolute head difference
           IF(first1) then
                 WL_out_old=S_out
                 WL_in_old=S_in
                 first1 = .false.
                 IF (Mode == 51) THEN !Flexible pumping
                       is_pumping = .TRUE.
                       Mode = 5
                 ENDIF
           ENDIF

           differ_out_wl=WL_out_new-WL_out_old   !Head difference at two adjecent Time step of outside
           differ_in_wl=WL_in_NEW-WL_in_old      !Head difference at two adjecent Time step of inside

      IF (Mode==1) THEN  ! Ebb generation

      ELSEIF (Mode==2) THEN  ! Ebb Generation with pumping

      ELSEIF(Mode==3) THEN !Two-way generation

      ELSEIF(Mode==4) THEN !Two-way with pumping

      ELSEIF(Mode==5)THEN !two-way with optimized operation head
! I COPIED THIS WHOLE CASE FROM (MODE == 3)
! AND THEN I ADJUSTED SOME OF IT SO IT MAY BE BUSTED - NH
      SELECT CASE (tphase_old)
      !!!!!!!
      !!!!!!! Carefull about the CASE(0), Different initial water level might have different CASE(0)
      CASE(0)    ! Initial phase
       IF (((CT_H).GE.HT1).and.(dz.LE.1.D-2)) THEN
             tphase_new = 7
         ELSE
             tphase_new = 0
         ENDIF
      CASE(1) ! Holding (HW),
      IF (((H_REF.GE.H_ebb_start).OR.(phase_time.GE.MAX_HOLD))
     &      .AND.(PHASE_TIME.GE.RAMP_TIME)) THEN
             tphase_new = 2
         ELSE
             tphase_new = 1
         ENDIF
      CASE (2) ! Generating (EBB),
         IF ((DZ.LT.H_ebb_end.AND.differ_out_wl.LT.1.D-1)
     &      .OR.(PHASE_TIME.GE.MAX_GEN)) THEN
             tphase_new = 3
         ELSE
             tphase_new = 2
         ENDIF
      CASE (3) !  Emptying,
         IF ((differ_out_wl.GT.0.D0) .AND. (H_ref.LT.0.D0)) THEN
             IF (is_pumping) THEN
                   tphase_new = -1 !Ebb pumping to target low water
             ELSE
                   tphase_new = 4
             ENDIF
         ELSE
             tphase_new = 3
         ENDIF
      CASE (-1) !Ebb pump - to low water
            IF ((S_IN.LE.pump_target).OR.((PHASE_TIME.GE.MAX_PUMP)
     & 	.OR.(H_REF.GE.H_EBB_START))) THEN !Internal water level is below low water target
                  tphase_new = 4
            ELSE
                  tphase_new = -1
            ENDIF
      CASE (4) ! Holding (LW),
         IF (((PHASE_TIME.GE.MAX_HOLD).OR.
     &   (DZ.GT.H_EBB_START)).AND.(PHASE_TIME.GE.RAMP_TIME)) THEN !NH NH
             tphase_new = 5
         ELSE
             tphase_new = 4
         ENDIF
      CASE (5) ! Generating (FLOOD),
         IF (((differ_out_wl.LT.0.D0) .and. (differ_in_wl.GT.0.D0)
     &     .and.(DZ.LT.H_EBB_END)).OR.(PHASE_TIME.GE.MAX_GEN)) then
             tphase_new = 6
         ELSE
             tphase_new = 5
         ENDIF
      CASE (6) ! Filling
         IF ( (differ_out_wl.LT.0.D0) .AND.
     &        (differ_in_wl.GT.0.D0) .AND. (H_ref.GE.0.D0) ) then ! took out .and. (WL_out_new>0.5)
             IF (is_pumping) THEN
                   TPHASE_NEW = -2
             ELSE
                   tphase_new = 1
             ENDIF
         ELSE
             tphase_new = 6
         ENDIF
      CASE (-2) ! Pumping to increase High water
            IF ((S_IN.GE.pump_target).OR.((PHASE_TIME.GE.MAX_PUMP)
     & 		.OR.(H_REF.GE.H_EBB_START)))THEN !Internal water level is below low water target
                  tphase_new = 1
            ELSE
                  tphase_new = -2
            ENDIF
      CASE (7) !Exagerated Filling, activates sluices at increased capacity to
         IF ((CT_H).GT.HT2) THEN
               tphase_new = 1
         ELSE
            tphase_new = 7
         ENDIF
      END SELECT
      ENDIF


!      RAMP FUNCTION

       IF (tphase_old .NE. tphase_new) THEN
           timsec_1 = CT                       !timsec_1 means the start time of a new phase
       ENDIF

       phase_time = (CT-timsec_1)          ! Give the initial Phase_time

       IF (phase_time .LT. ramp_time) THEN
        !phase_time = phase_time + 1
        opnRmp = (1-cos(3.1415926*phase_time/ramp_time))/2
        clsrmp = 1-opnRmp
       ELSE
        opnRmp = 1
        clsRmp = 0
       ENDIF

!       LOOP OVER THE CULVERTS
        DO N=1,N_turbines+N_Sluice      !The number of turbine+Sluicegate, the number of turbine will be N_turbines

!-----------------------------------------------------------------------
!       IDENTIFIES ENTRY / EXIT NODES
!
!       NUMBER OF THE POINTS
        I1=ENTBUS(N)
        I2=SORBUS(N)

!         S_out: points at upstream(inside Lagoon)
!         S_in: Points at downstream(Outside Lagoon)
        IF(I1.GT.0) THEN
          QMAX1=0.9D0*H(I1)*V2DPAR%R(I1)/DT
        ELSE
          QMAX1=0.D0
        ENDIF
        IF(I2.GT.0) THEN
          QMAX2=0.9D0*H(I2)*V2DPAR%R(I2)/DT
        ELSE
          QMAX2=0.D0
        ENDIF

!       CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN
!         REMOVED BY NH 02/12/2020
        IF(NCSIZE.GT.1) THEN
          QMAX1=P_DMAX(QMAX1)+P_DMIN(QMAX1)
          QMAX2=P_DMAX(QMAX2)+P_DMIN(QMAX2)
        ENDIF
!
        CE1=CEBUS(N,1)
        CE2=CEBUS(N,2)
        CS1=CSBUS(N,1)
        CS2=CSBUS(N,2)
        L=LBUS(N)
        RD1=ALTBUS(N,1)
        RD2=ALTBUS(N,2)
        RD=0.5D0*(RD1+RD2)
        LARG=LRGBUS(N)
        HAUT1=HAUBUS(N,1)
        HAUT2=HAUBUS(N,2)
        RADI1=0.5D0*HAUT1
        RADI2=0.5D0*HAUT2
        FRIC=FRICBUS(N)
        LONG=LONGBUS(N)
        TRASH=CTRASH(N)
        CORR56=C56(N)
        VALVE=CV(N)
        CORRV5=CV5(N)
        CORR5=C5(N)
        CIR=CIRC(N)
        HAUT=MIN(HAUT1,HAUT2)


          IF (Mode==1) THEN  !Ebb Generation


          ELSEIF (Mode==2) THEN !Ebb with Pumping


          ELSEIF((Mode==3).OR.(Mode==5)) THEN  !Two-way Generation
          SELECT CASE (tphase_new)

         CASE(0)   ! Inital condition,Both Q = 0.
              Q=0.D0

         CASE(1)          !Holding(HW) phase:Q=0,,+ at the close start still filing (RAMP)
 !          IF (dz<=1.D0-2) THEN  !I don't know what's the meaning of this code,
!              Q=0.D0
            IF (N.LE.N_TURBINES) THEN
            IF (DZ.LE.H_Pump(1)) THEN
             Q = Q_PUMP(1,pump)
            ELSEIF (DZ.GE.H_PUMP(N_PUMPING)) THEN
             Q = Q_PUMP(N_PUMPING,PUMP)
            ELSE
             DO k = 2,N_PUMPING
                IF (DZ.GE.H_PUMP(k-1).AND.DZ.LE.H_PUMP(k)) THEN
                  AAA = (DZ-H_PUMP(k-1))/(H_PUMP(k) - H_PUMP(k-1))
                  Q=(1.D0-AAA)*Q_PUMP(k-1,PUMP)+AAA*Q_PUMP(k,PUMP)
                ENDIF
             ENDDO
            ENDIF
            Q = -Q*clsRmp
            ELSE
                Q = 0
            ENDIF

          CASE(2)           !Ebb Generating phase,
             IF (N.LE.N_turbines)   THEN    ! This N means Turbines, Q follow the Hill-Chart
               IF (dz.LT.dzw_T(1)) THEN
                   Q= 0.D0
               ELSEIF (dz.GT.dzw_T(NPAIRS_T)) THEN
                   Q = -disch_T(NPAIRS_T,hill_chart)
               ELSE
               IF (dz.GE.dzw_T(1).AND.dz.LE.dzw_T(NPAIRS_T)) THEN
                    DO k = 2,NPAIRS_T
                     IF (dz.GE.dzw_T(k-1).AND.dz.LE.dzw_T(k)) THEN
                        AAA = (dz-dzw_T(k-1))/(dzw_T(k)-dzw_T(k-1))
                        Q = -((1.D0-AAA)*disch_T(k-1,hill_chart)
     &                        + AAA*disch_T(k,hill_chart))
                     ENDIF
                   ENDDO
               ENDIF
               ENDIF
             ELSE
              IF  (N.LE.(N_turbines+N_Sluice))  THEN
                    Q = 0.D0
              ENDIF
            ENDIF       ! This N is sluice gate, Q = 0
             Q = Q*opnRmp        !RAMP function for opening

          CASE(3)  ! Emptying (EBB), Turbines < 0 [H-Q], Sluice < 0 [H-Q]
             IF (N.LE.N_turbines)   THEN
               IF (dz.LT.dzw_T(1)) THEN
                   Q= 0.D0
               ELSEIF (dz.GT.dzw_T(NPAIRS_T)) THEN
                   Q = -disch_T(NPAIRS_T,hill_chart)
               ELSE
               IF (dz.GE.dzw_T(1).AND.dz.LE.dzw_T(NPAIRS_T)) THEN
                    DO k = 2,NPAIRS_T
                     IF (dz.GE.dzw_T(k-1).AND.dz.LE.dzw_T(k)) THEN
                        AAA = (dz-dzw_T(k-1))/(dzw_T(k)-dzw_T(k-1))
                        Q = -((1.D0-AAA)*disch_T(k-1,hill_chart)
     &                       + AAA*disch_T(k,hill_chart))
                     ENDIF
                   ENDDO
               ENDIF
               ENDIF
             ELSE
               IF  (N.LE.(N_turbines+N_Sluice))   THEN
                    Q = -Cd_Sluice*Area_Sluice*sqrt(2*9.806*dz)
                    Q = Q*opnRmp
               ENDIF
             ENDIF

          CASE(4)        !Holding for Low water
          IF (N.LE.N_TURBINES) THEN
           IF (DZ.LE.H_Pump(1)) THEN
             Q = Q_PUMP(1,pump)
           ELSEIF (DZ.GE.H_PUMP(N_PUMPING)) THEN
             Q = Q_PUMP(N_PUMPING,PUMP)
           ELSE
             DO k = 2,N_PUMPING
               IF (DZ.GE.H_PUMP(k-1).AND.DZ.LE.H_PUMP(k)) THEN
                 AAA = (DZ-H_PUMP(k-1))/(H_PUMP(k) - H_PUMP(k-1))
                 Q=(1.D0-AAA)*Q_PUMP(k-1,PUMP)+AAA*Q_PUMP(k,PUMP)
               ENDIF
             ENDDO
           ENDIF
           Q = -Q*clsRmp
         ELSE
               Q = 0
         ENDIF

           CASE(5)    ! Generating for Flooding water
             IF (N.LE.N_turbines)   THEN    ! This N means Turbines, Q follow the Hill-Chart
               IF (dz.LT.dzw_T(1)) THEN
                   Q= 0.D0
               ELSEIF (dz.GT.dzw_T(NPAIRS_T)) THEN
                   Q = disch_T(NPAIRS_T,hill_chart)
               ELSE
               IF (dz.GE.dzw_T(1).AND.dz.LE.dzw_T(NPAIRS_T)) THEN
                    DO k = 2,NPAIRS_T
                     IF (dz.GE.dzw_T(k-1).AND.dz.LE.dzw_T(k)) THEN
                          AAA = (dz-dzw_T(k-1))/(dzw_T(k)-dzw_T(k-1))
                          Q = (1.D0-AAA)*disch_T(k-1,hill_chart)
     &                     + AAA*disch_T(k,hill_chart)
                     ENDIF
                   ENDDO
               ENDIF
               ENDIF
             ELSE
              IF  (N.LE.(N_turbines+N_Sluice))  THEN
                    Q = 0.D0
              ENDIF
            ENDIF       ! This N is sluice gate, Q = 0
             Q = Q*opnRmp        !RAMP function for opening

          CASE(6)     !  Filling()
             IF (N.LE.N_turbines)   THEN!
               IF (dz.LT.dzw_T(1)) THEN
                   Q= 0.D0
               ELSEIF (dz.GT.dzw_T(NPAIRS_T)) THEN
                   Q = disch_T(NPAIRS_T,hill_chart)
               ELSE
               IF (dz.GE.dzw_T(1).AND.dz.LE.dzw_T(NPAIRS_T)) THEN
                    DO k = 2,NPAIRS_T
                     IF (dz.GE.dzw_T(k-1).AND.dz.LE.dzw_T(k)) THEN
                        AAA = (dz-dzw_T(k-1))/(dzw_T(k)-dzw_T(k-1))
                        Q = ((1.D0-AAA)*disch_T(k-1,hill_chart)
     &                       + AAA*disch_T(k,hill_chart))
                     ENDIF
                   ENDDO
               ENDIF
               ENDIF
             ELSEIF (N.LE.(N_turbines+N_Sluice))   THEN  !the Discharge Through Sluice gate
                Q = Cd_Sluice*Area_Sluice*sqrt(2*9.806*dz)*opnRmp
             ENDIF

            CASE(7)     !  Exagerated Filling()

            IF(DZ.LT.1D0) THEN
	    	MLT = DZ*MULT
	    ELSE
	        MLT = MULT
	    ENDIF
	    IF(CT_H.LT.(HT1*0.5+HT2*0.5)) THEN
	        MLT = MLT*(CT_H - HT1)/(HT2 - HT1)
	    ENDIF
	    Q = cd_Sluice*Area_Sluice*sqrt(2*9.806*dz)*MLT*OPNRMP

	    IF(H_REF.GT.0.0) THEN
	    	Q = -Q
	    ENDIF

            CASE(-1) !Ebb pumping -> LW
            IF (N.LE.N_TURBINES) THEN
              IF (DZ.LE.H_Pump(1)) THEN
                Q = Q_PUMP(1,pump)
              ELSEIF (DZ.GE.H_PUMP(N_PUMPING)) THEN
                Q = Q_PUMP(N_PUMPING,PUMP)
              ELSE
                DO k = 2,N_PUMPING
                  IF (DZ.GE.H_PUMP(k-1).AND.DZ.LE.H_PUMP(k)) THEN
                    AAA = (DZ-H_PUMP(k-1))/(H_PUMP(k) - H_PUMP(k-1))
                    Q=(1.D0-AAA)*Q_PUMP(k-1,PUMP)+AAA*Q_PUMP(k,PUMP)
                  ENDIF
                ENDDO
              ENDIF
              Q = -Q*opnRmp
            ELSE
                  Q = 0
            ENDIF

            CASE(-2) !Fill pumping -> HW
            IF (N.LE.N_TURBINES) THEN
              IF (DZ.LE.H_Pump(1)) THEN
                Q = Q_PUMP(1,pump)
              ELSEIF (DZ.GE.H_PUMP(N_PUMPING)) THEN
                Q = Q_PUMP(N_PUMPING,PUMP)
              ELSE
                DO k = 2,N_PUMPING
                  IF (DZ.GE.H_PUMP(k-1).AND.DZ.LE.H_PUMP(k)) THEN
                    AAA = (DZ-H_PUMP(k-1))/(H_PUMP(k) - H_PUMP(k-1))
                    Q=(1.D0-AAA)*Q_PUMP(k-1,PUMP)+AAA*Q_PUMP(k,PUMP)
                  ENDIF
                ENDDO
              ENDIF
              Q = Q*opnRmp
            ELSE
                  Q = 0
            ENDIF
            END SELECT


          ELSEIF(Mode==4) THEN   !Two-way Generation with pumping

          !ELSEIF(Mode==5)THEN   !two-way with optimized operation head

          ENDIF

!
!       FILLS OUT DBUS(N) USING RELAXATION, and add with RAMP Function
!
	    Q = ABS(Q)
          IF ((TPHASE_NEW.EQ.-1).OR.(TPHASE_NEW.EQ.4)) THEN
                Q = -Q
          ELSEIF ((TPHASE_NEW.EQ.-2).OR.(TPHASE_NEW.EQ.1)) THEN
                Q = Q
          ELSE
                IF (H_REF.GT.0.D0) THEN
      	          Q = -Q
                ENDIF
	    ENDIF


        DBUS(N)= RELAXB*Q + (1.D0-RELAXB)*DBUS(N)


        IF(N.LE.N_turbines)   THEN
            SECBUS(N) = AIRE
        ELSEIF (N.LE.(N_turbines+N_Sluice)) THEN
            SECBUS(N) = Area_Sluice
        ENDIF
!        SECBUS(N) = LARG * HAUT1
!
!       LIMITATION WITH AVAILABLE WATER
!
        IF(DBUS(N).GT.0.D0) THEN
          DBUS(N)=MIN(QMAX1,DBUS(N))
        ELSE
          DBUS(N)=MAX(-QMAX2,DBUS(N))
        ENDIF
!
!
!  TREATS THE VELOCITIES AT THE SOURCES
!  SAME APPROACH FOR VELOCITY AND TRACER
!
        IF(DBUS(N).GT.0.D0) THEN
          UBUS(2,N) = (DBUS(N)/SECBUS(N)) * COS(ANGBUS(N,2))
          VBUS(2,N) = (DBUS(N)/SECBUS(N)) * SIN(ANGBUS(N,2))
          IF(I1.GT.0) THEN
            UBUS(1,N) = U(I1)
            VBUS(1,N) = V(I1)
          ELSE
            UBUS(1,N) = 0.D0
            VBUS(1,N) = 0.D0
          ENDIF
          IF(NCSIZE.GT.1) THEN
            UBUS(1,N) = P_DMAX(UBUS(1,N))+P_DMIN(UBUS(1,N))
            VBUS(1,N) = P_DMAX(VBUS(1,N))+P_DMIN(VBUS(1,N))
          ENDIF
        ELSEIF(DBUS(N).LT.0.D0) THEN
          UBUS(1,N) = (DBUS(N)/SECBUS(N)) * COS(ANGBUS(N,1))
          VBUS(1,N) = (DBUS(N)/SECBUS(N)) * SIN(ANGBUS(N,1))
          IF(I2.GT.0) THEN
            UBUS(2,N) = U(I2)
            VBUS(2,N) = V(I2)
          ELSE
            UBUS(2,N) = 0.D0
            VBUS(2,N) = 0.D0
          ENDIF
          IF(NCSIZE.GT.1) THEN
            UBUS(2,N) = P_DMAX(UBUS(2,N))+P_DMIN(UBUS(2,N))
            VBUS(2,N) = P_DMAX(VBUS(2,N))+P_DMIN(VBUS(2,N))
          ENDIF
        ELSEIF(DBUS(N).EQ.0.D0) THEN
          UBUS(1,N) = 0.D0
          VBUS(1,N) = 0.D0
          UBUS(2,N) = 0.D0
          VBUS(2,N) = 0.D0
          IF(NCSIZE.GT.1) THEN
            UBUS(1,N) = P_DMAX(UBUS(1,N))+P_DMIN(UBUS(1,N))
            VBUS(1,N) = P_DMAX(VBUS(1,N))+P_DMIN(VBUS(1,N))
            UBUS(2,N) = P_DMAX(UBUS(2,N))+P_DMIN(UBUS(2,N))
            VBUS(2,N) = P_DMAX(VBUS(2,N))+P_DMIN(VBUS(2,N))
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
     &          P_DMAX(TBUS%ADR(ITRAC)%P%R(NBUSE+N))
     &         +P_DMIN(TBUS%ADR(ITRAC)%P%R(NBUSE+N))
              TBUS%ADR(ITRAC)%P%R(N)      =
     &          P_DMAX(TBUS%ADR(ITRAC)%P%R(N))
     &         +P_DMIN(TBUS%ADR(ITRAC)%P%R(N))
            ENDIF
          ENDDO
            ENDIF

!   Calculate the Power Efficiecy and output

      IF((Mode==3).OR.(Mode==5)) THEN !Two-way generation
        IF(N.LE.N_turbines) THEN
          IF((tphase_new==2).or.(tphase_new==5)
     & .OR.(tphase_new==3).OR.(tphase_new==6)) THEN
            IF(dz.LT.WL_Tur_power(1))THEN
              P=Tur_power(1,hill_chart)
              IF (dz.LT.0.1D0) THEN
                    P = 0
              ENDIF
            ELSEIF (dz.GT.WL_Tur_power(N_power)) THEN
              p = Tur_power(N_power,hill_chart)
            ELSEIF (dz.GE.WL_Tur_power(1).AND.dz.
     &      LE.WL_Tur_power(N_power))  THEN
              Do k=2,N_power
       IF (dz.GE.WL_Tur_power(k-1).AND.dz.LE.WL_Tur_power(k)) THEN
       AAA=(dz-WL_Tur_power(k-1))/(WL_Tur_power(k)-WL_Tur_power(k-1))
                  P= (1.D0-AAA)*Tur_power(k-1,hill_chart)
     &            +AAA*Tur_power(k,hill_chart)*opnRmp
                ENDIF
              ENDDO
            ENDIF
          ELSEIF((tphase_new==-1).or.(tphase_new==-2)) THEN
            IF (DZ.LE.H_PUMP(1)) THEN
              P = P_PUMP(1,PUMP)
            ELSEIF (DZ.GE.H_PUMP(N_PUMPING)) THEN
              P = P_PUMP(N_PUMPING,PUMP)
            ELSE
              DO k=2,N_PUMPING
                IF (DZ.GE.H_PUMP(k-1).AND.DZ.LE.H_PUMP(k)) THEN
                   AAA = (DZ-H_PUMP(k-1))/(H_PUMP(k) - H_PUMP(k-1))
                   P=(1.D0-AAA)*P_PUMP(k-1,PUMP)+AAA*P_PUMP(k,PUMP)
                ENDIF
              ENDDO
            ENDIF
            P = -P
          ELSE
            P=0
          ENDIF
          P_accumulate=P_accumulate+P*DT
        ENDIF
      ENDIF
!
!     Record concern information

       !IF(AMOD(CT,300.0)==0) THEN   !Note that 00 is the printout time

       IF (PRNT_TIME.LT.2399) THEN
       		IF(IPID.EQ.0) THEN
       			PRNT_TIME = PRNT_TIME + 1
       		ENDIF
       ELSE

        IF(IPID.EQ.0) THEN
        PRNT_TIME = 0
           !
        IF (N == 1) THEN
         WRITE(*,*)'CT :',CT,'TPHASE_NEW : ',tphase_new
         WRITE(*,*)'S_OUT :',S_out,'S_IN :',S_in,'H_REF : ',H_REF
         WRITE(*,*)'OPNRMP : ',opnRmp,'HILLCHART : ',HILL_CHART
         !WRITE(*,*)'N=  z_out=   z_in=   dz_n=  Q=   QMAX1= QMAX2='
        ENDIF
         WRITE(*,*)'N: ',N,'Q: ',Q,'P: ',P
!        WRITE(*,*)N,CT,S_OUT, S_IN,H_REF,Q,
!     &        TPHASE_NEW,OPNRMP,P_ACCUMULATE,HILL_CHART
!    WRITE to output file TRSF
      IF ((N.GE.1).AND.(N.LE.N_turbines)) THEN !Turbine
            IF (N == 1) THEN
                  Outside = S_out
                  Inside = S_In
            ENDIF
            QTurb = QTurb + Q
            PowerMW = PowerMW + P
      ELSEIF ((N.GT.N_turbines).AND.(N.LE.(N_turbines+N_Sluice))) THEN
            QSluice = QSluice + Q
      ENDIF
      IF (N == (N_turbines+N_Sluice)) THEN
      OPEN(TRSF,file='../TRS-data.csv',status='old',access='append')
      WRITE(TRSF,*)CT,',',S_Out,',',S_In,',',H_ref,',',tphase_new,
     &      ',',QTurb,',',QSluice,',',PowerMW,',',hill_chart
      CLOSE(TRSF)
      QTurb = 0.0
      QSluice = 0.0
      PowerMW = 0.0
      ENDIF

!        WRITE(10086,*)  N,',',CT,',',S_out,',',S_in,',',H_ref,',',
 !    &   Q,',',tphase_new,',', opnRmp,',',P,',',P_accumulate
        ENDIF
        ENDIF


      ENDDO ! N

         tphase_old = tphase_new
         WL_out_old = WL_out_new
        WL_in_old = WL_in_NEW
!
!-----------------------------------------------------------------------
!
      RETURN
      END



!                    *****************
                     SUBROUTINE CORFON
!                    *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE
!
!history  J-M HERVOUET (LNHE)
!+        01/03/1990
!+        V5P2
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      DOUBLE PRECISION UNORM,AIRE,XSOM(5,4),YSOM(5,4),X4,Y4
!      DOUBLE PRECISION XSOM2(4),XSOM2(4),XSOM3(4),XSOM3(4)
!      DOUBLE PRECISION XSOM4(4),XSOM4(4),XSOM5(4),XSOM5(4)
      INTEGER I, NSOM,K


      NSOM=4
      XSOM(1,1) = 435406.556115        ! this is for -19.5
      XSOM(1,2) = 435775.085811
      XSOM(1,3) = 435612.648479
      XSOM(1,4) = 435242.088317
      YSOM(1,1) = 5715346.803488
      YSOM(1,2) = 5714980.304259
      YSOM(1,3) = 5714813.805995
      YSOM(1,4) = 5715174.213824

      XSOM(2,1) = 435199.448518      ! this is for --17
      XSOM(2,2) = 435402.495182
      XSOM(2,3) = 435820.771310
      XSOM(2,4) = 435617.724646
      YSOM(2,1) = 5715178.274757
      YSOM(2,2) = 5715388.428055
      YSOM(2,3) = 5714980.304259
      YSOM(2,4) = 5714764.059562

      XSOM(3,1) = 435405.540882      !  this is for -14.5
      XSOM(3,2) = 435865.441576
      XSOM(3,3) = 435614.678946
      XSOM(3,4) = 435152.747785
      YSOM(3,1) = 5715431.067854
      YSOM(3,2) = 5714980.304259
      YSOM(3,3) = 5714721.419763
      YSOM(3,4) = 5715177.259524

      XSOM(4,1) = 435101.986119      !  this is for -12
      XSOM(4,2) = 435618.739879
      XSOM(4,3) = 435909.096609
      XSOM(4,4) = 435400.464715
      YSOM(4,1) = 5715175.229057
      YSOM(4,2) = 5714675.734263
      YSOM(4,3) = 5714977.258559
      YSOM(4,4) = 5715467.616254

      XSOM(5,1) = 435948.690709           ! this is for -10
      XSOM(5,2) = 435620.770346
      XSOM(5,3) = 435038.026420
      XSOM(5,4) = 435396.403782
      YSOM(5,1) = 5714979.289026
      YSOM(5,2) = 5714636.140164
      YSOM(5,3) = 5715168.122424
      YSOM(5,4) = 5715524.469320

      DO I = 1,NPOIN
          IF(INPOLY(X(I),Y(I),XSOM(5,1:4),YSOM(5,1:4),NSOM)) THEN
           ZF%R(I) = -10.D0
           ENDIF
      ENDDO

      DO I = 1,NPOIN
          IF(INPOLY(X(I),Y(I),XSOM(4,1:4),YSOM(4,1:4),NSOM)) THEN
           ZF%R(I) = -12.D0
           ENDIF
      ENDDO

      DO I = 1,NPOIN
          IF(INPOLY(X(I),Y(I),XSOM(3,1:4),YSOM(3,1:4),NSOM)) THEN
           ZF%R(I) = -145.D-1
           ENDIF
      ENDDO

      DO I = 1,NPOIN
          IF(INPOLY(X(I),Y(I),XSOM(2,1:4),YSOM(2,1:4),NSOM)) THEN
           ZF%R(I) = -17.D0
           ENDIF
      ENDDO

      DO I = 1,NPOIN
          IF(INPOLY(X(I),Y(I),XSOM(1,1:4),YSOM(1,1:4),NSOM)) THEN
           ZF%R(I) = -195.D-1
           ENDIF
      ENDDO


!-----------------------------------------------------------------------
!
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : PAS DE MODIFICATION DU FOND'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : ',LISFON,' LISSAGES DU FOND'
          WRITE(LU,*)
        ENDIF
      ENDIF
      IF(LNG.EQ.2) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
          WRITE(LU,*)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END


!                    ********************
                     SUBROUTINE TELEMAC2D
!                    ********************
!
     &(PASS,ATDEP,NITER,CODE,DTDEP,NEWTIME,DOPRINT,NITERORI,
     & CONVERGENCE_LEVEL)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    SOLVES THE SAINT-VENANT EQUATIONS FOR U,V,H.
!+
!+            ADJO = .TRUE.  : DIRECT MODE
!+
!+            ADJO = .FALSE. : ADJOINT MODE
!
!history
!+        06/06/2008
!+
!+   OPTIONAL ARGUMENT BOUNDARY_COLOUR ADDED TO LECLIM
!
!history
!+        16/06/2008
!+
!+   SECOND CALL TO PROPIN FOLLOWING CALL TO BORD
!
!history
!+        25/06/2008
!+
!+   DIFFIN2 RENAMED DIFFIN + ARGUMENT MESH
!
!history
!+        27/06/2008
!+
!+   ARGUMENTS OF PROPIN_TELEMAC2D : MESH ADDED TO THE END
!
!history
!+        29/07/2008
!+
!+   ADDED CALL TO FLUSEC BEFORE THE 1ST CALL PRERES
!
!history
!+        13/08/2008
!+
!+   CHANGED CALL AND CALL CONDITIONS TO CHARAC
!
!history
!+        20/08/2008
!+
!+   LIST_PTS MODIFIED IN PARALLEL
!
!history
!+        02/09/2008
!+
!+   CALL TO MODIFIED TEL4DEL (ADDED VELOCITY AND DIFFUSION)
!
!history
!+        25/09/2008
!+
!+   CALL TO MODIFIED TEL4DEL (FLUXES SENT THRU MESH%W%R)
!
!history
!+        21/10/2008
!+
!+   CALL TO MODIFIED MASKTO (PARALLEL VERSION OF MASKTO)
!
!history
!+        09/02/2009
!+
!+   IF H CLIPPED, USES HMIN INSTEAD OF 0.D0
!
!history
!+        16/02/2009
!+
!+   CALL TO POSITIVE_DEPTHS
!
!history
!+        19/02/2009
!+
!+   H CLIPPED IN CASE OF COMPUTATION CONTINUED
!
!history
!+        02/04/2009
!+
!+   NEW FILE STRUCTURE T2D_FILES AND MED FORMAT
!
!history
!+        09/07/2009
!+
!+   ARGUMENT NPTFR2 ADDED TO LECLIM
!
!history
!+        20/07/2009
!+
!+   1 OUT OF 3 CALLS TO TEL4DEL REMOVED (THANKS TO A
!
!history
!+        22/07/2009
!+
!+   3 NEW ARGUMENTS IN PROPAG
!
!history  J-M HERVOUET (LNHE)
!+        25/11/2009
!+        V6P0
!+   VERSION WITH MULTIPLE TRACERS
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (LNHE)
!+        19/04/2011
!+        V6P1
!+   SECOND CALL TO SISYPHE MOVED AT THE END OF THE TIME LOOP SO THAT
!+   A CORRECT CONTINUITY EQUATION CAN BE SENT EVEN AT THE FIRST TIME
!+   STEP (H, HN, USIS, VSIS, DM1, ZCONV COMPATIBLE)
!
!history  J-M HERVOUET (LNHE)
!+        19/05/2011
!+        V6P1
!+   NEW THOMPSON THEORY, THAT WORKS ALSO IN PARALLEL
!
!history  J-M HERVOUET (LNHE)
!+        09/08/2011
!+        V6P1
!+   Call to lecsng changed
!
!history  C.COULET (ARTELIA)
!+        23/05/2012
!+        V6P2
!+   Modification for adding "bridge" file and separation of weirs and
!+   culvert files.
!
!history  J-M HERVOUET (LNHE)
!+        16/07/2012
!+        V6P1
!+   Call to TEL4DEL modified.
!
!history  P. CHASSE (CETMEF) / C.COULET (ARTELIA)
!+        03/08/2012
!+        V6P2
!+   Modification for adding breaches management during simulation
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        12/02/2013
!+        V6P1
!+   Call to FLOT and DERIVE modified, call to SORFLO removed.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        11/03/2013
!+        V6P3
!+   Call to METEO modified.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        22/03/2013
!+        V6P3
!+   Call to WAC and SISYPHE modified.
!
!history  R. KOPMANN (EDF R&D, LNHE)
!+        16/04/2013
!+        V6P3
!+   Adding the file format in calls to FIND_IN_SEL.
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        12/06/2013
!+        V6P3
!+   Modification for new treatment of weirs
!
!history  A. JOLY (EDF R&D, LNHE)
!+        15/07/2013
!+        V6P3
!+   Allocating algae variables and initialising them for the next
!+   time step
!
!history R.ATA (EDF R&D, LNHE)
!+        10/10/2013
!+        V6P3
!+   FORCING LISTING AND GRAPHIC OUTPUTS FOR LAST TIME STEP, FOR FV
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        30/12/2013
!+        V7P0
!+   Initialisation of YAFLODEL added (overlooked bug?).
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        02/01/2014
!+        V7P0
!+   Removing a use of KNOGL. KNOGL suppressed in call to
!+   flusec_telemac2d.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        31/03/2014
!+        V7P0
!+   1) Now written to enable different numbering of boundary points and
!+      boundary segments.
!+   2) Incident wave suppressed.
!+   3) Different advection schemes for different tracers allowed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/04/2014
!+        V7P0
!+   Now 2 calls to charac
!+   one for strong and one for weak characteristics.
!+   Second call to DIFFIN: U and V replaced by UCONV and VCONV.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        11/06/2014
!+        V7P0
!+   LIMTRA replaced by a copy in the call to cvdftr (some schemes may
!+   change it and it may cause problems for the next tracers)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        05/08/2014
!+        V7P0
!+   Initialisation of PRIVE move before call to FONSTR (it could be
!+   used in FONSTR and wrongly cancelled by initialisation).
!
!history  C VILLARET (HRW+EDF) & J-M HERVOUET (EDF - LNHE)
!+        18/09/2014
!+        V7P0
!+   Calls to sisyphe and wac changed.
!
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Secondary flow correction:
!+   add the calculation of \Omega
!
!history  R. ATA (EDF LAB, LNHE)
!+        05/11/2014
!+        V7P0
!+   add optional variables to meteo in a sake of harmonization
!+   with telemac-3d
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/05/2015
!+        V7P1
!+   ALIRE variable was wrong for tracers, they now begin at rank 34,
!+   so ALIRE(33+ITRAC)=1. After a remark by Noemie Durand.
!
!history Y. AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P1
!+   Modification to comply with the hermes module.
!
!history R. ATA (LNHE)
!+       27/05/2015
!+       V7P1
!+   UDEL and VDEL built for Delwaq in finite element options.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/05/2015
!+        V7P1
!+   Call to CVDFTR modified. 3 new arguments.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        18/09/2015
!+        V7P1
!+   Call to TEL4DEL modified. Printouts in Debug mode added. Argument
!+   NREJTR changed into NREJET in the call to difsou.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        14/03/2016
!+        V7P2
!+   Adding the argument HPROP in the call to SISYPHE. TETAHC removed
!+   and replaced everywhere by TETAC. HTILD replaced with HPROP in the
!+   call to CVDFTR.
!+   Call to COSAKE moved at the beginning.
!+   Call to FRICTION_CHOICE now without KARMAN.
!+   Call to KEPSIL modified.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/05/2016
!+        V7P2
!+   FLBOR initialised before the first call to bilan.f, for the sake of
!+   computations continued with stage-discharge curves.
!
!history  C.COULET (ARTELIA)
!+        01/09/2016
!+        V7P2
!+   Call lecsng splitted in 2 according to typseuil
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        243/09/2016
!+        V7P2
!+   MASS_RAIN set to 0.D0 before first call to BILANT. Local variables
!+   CHARR and SUSP removed.
!
!history R. ATA (LNHE)
!+       27/09/2016
!+       V7P2
!+   add new turbulence model of Spalart-Allmaras
!
!history S.E.BOURBAN (HRW))
!+       01/01/2017
!+       V7P2
!+   Adding differentiated variables
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!param atdep     [in] starting time when called for coupling
!param code      [in] calling program (if coupling)
!param doprint   [in] for overwriting the keyword on listing
!param dtdep     [in] time step to use when coupling with estel-3d
!param newtime   [in] are we starting a new time step or just iterating?
!+                    this is for coupling with estel-3d
!param niter     [in] number of iterations when called for coupling
!param pass      [in] -1 : all steps
!+                     0 : only initialisation
!+                     1 : only time-steps steps
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_TELEMAC2D => TELEMAC2D
      USE INTERFACE_SISYPHE, ONLY: SISYPHE
      USE INTERFACE_TOMAWAC, ONLY: WAC
      USE GRACESTOP
      USE FRICTION_DEF
!     MODULE SPECIFIC TO COUPLING WITH ESTEL-3D
      USE M_COUPLING_ESTEL3D
!     OIL SPILL MODEL
      USE OILSPILL
!     ALGAE MODEL
      USE ALGAE_TRANSP
!     DELWAQ
      USE TEL4DEL, ONLY: TEL4DELWAQ
      USE INTERFACE_PARALLEL, ONLY : P_IMAX,P_IMIN,P_DMIN
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: PASS,NITER
      DOUBLE PRECISION, INTENT(IN) :: ATDEP
      CHARACTER(LEN=*), INTENT(IN) :: CODE
!     TIME STEP TO USE WHEN COUPLING WITH ESTEL-3D
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: DTDEP
!     ARE WE STARTING A NEW TIME STEP OR JUST ITERATING?
      LOGICAL,          INTENT(IN), OPTIONAL :: NEWTIME
!     DO WE WANT TELEMAC2D TO OUTPUT IN THE LISTING OR NOT?
      LOGICAL,          INTENT(IN), OPTIONAL :: DOPRINT
      INTEGER,          INTENT(IN), OPTIONAL :: NITERORI
      INTEGER,          INTENT(IN), OPTIONAL :: CONVERGENCE_LEVEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      ! Both init and time loop

!
! INTEGERS
!
      INTEGER I,ISOUSI,STOP2,DISCLIN
      INTEGER IELMX
!
! REAL SCALARS
!
      DOUBLE PRECISION KMAX
      DOUBLE PRECISION CFLMAX,RELAX
      DOUBLE PRECISION EMAX,SIGMAE,SIGMAK,C1
      DOUBLE PRECISION C
!
! LOGICALS
!
      LOGICAL INFOGS,INFOGT,ARRET1,ARRET2,YASMH,ARRET3
      LOGICAL CHARR_TEL,SUSP1,YAFLODEL,YAFLULIM,YAFLULIMEBE
      LOGICAL YASMO,YASIS
!
!
!-----------------------------------------------------------------------
!
      INTEGER IMAX,ITRAC,IVAR
!
!-----------------------------------------------------------------------
!
! ADDED FOR KINETIC SCHEMES
!
      DOUBLE PRECISION DTN,FLUSORTN,FLUENTN,DTT,SVOID(1)
!
!-----------------------------------------------------------------------
!
!     FOR SISYPHE : GRAIN FEEDING AND CONSTANT FLOW DISCHARGE
      INTEGER :: ISIS_CFD
!
      INTEGER :: OLD_LEOPRD
!
!-----------------------------------------------------------------------
!
      INTRINSIC MAX

      IELMX = MAX(IELMH,IELMU,IELMT,IELMK,IELME)
!
!-----------------------------------------------------------------------
!
!  VARIABLES TO READ IN THE EVENT OF A CONTINUATION:
!  0 : DISCARD    1 : READ  (SEE SS-PG NOMVAR)
!
!                                 0: OLD PLACE FOR THE TRACER
!
!-----------------------------------------------------------------------
!
#if defined(COMPAD)
      CALL AD_TELEMAC2D_BEGIN
#endif
!
!-----------------------------------------------------------------------
!
      IF(PRESENT(NITERORI)) THEN
        TOTAL_ITER = NITERORI
      ELSE
        TOTAL_ITER = NIT
      ENDIF
      CHARR_TEL=.FALSE.
      SIS_CPL%CHARR=.FALSE.
      SIS_CPL%SUSP=.FALSE.
      YAFLODEL=.FALSE.
      DISCLIN=11
!!      NFLOT=0
!
!-----------------------------------------------------------------------
!
!     SET REF_T2D VALUE FOR CONVERGENCE STUDIES
      REF_T2D = 0
      IF (PRESENT(CONVERGENCE_LEVEL)) THEN
        REF_T2D = CONVERGENCE_LEVEL
      ENDIF
!
!-----------------------------------------------------------------------
!     LOGICAL FOR LISTING AND GRAPHICAL OUTPUT
      IMP=.FALSE.
      LEO=.FALSE.
      IF(PASS.LE.0) COMPLEO=0
!
!-----------------------------------------------------------------------
!
!     INITIALISATION OF CONSTANTS FOR K-EPSILON, + KARMAN+ SA
!
      CALL COSAKE(KARMAN,CMU,C1,C2,SIGMAK,SIGMAE,
     &            ESTAR,SCHMIT,KMIN,KMAX,EMIN,EMAX)
!
      IF(ITURB.EQ.3) THEN
!       WILL HAVE TO INITIALISE K AND EPSILON
        AKEP = .TRUE.
      ELSEIF(ITURB.EQ.6) THEN
!       WILL HAVE TO INTIALISE SPALART-ALLMARAS
        SA=.TRUE.
        CALL COSASA(SIGMANU,NUMIN,NUMAX)
      ELSE
!       SHOULD NOT INITIALISE K NOR EPSILON NEITHER SA
        AKEP = .FALSE.
        SA   = .FALSE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FOR TAKING INTO ACCOUNT FLUX LIMITATION OF ARRAY FLULIM IN ADVECTION
!     SCHEMES (SO FAR ONLY FOR TRACERS IN CASE SOLSYS=2 AND OPT_HNEG=2).
!
      YAFLULIM=.FALSE.
      YAFLULIMEBE=.FALSE.
!
!     FOR READING TRACERS IN SELAFIN FILES
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
!         SEE NOMVAR_TELEMAC2D
          ALIRE(34+ITRAC) = 1
        ENDDO
      ENDIF
!
!     FOR AVOIDING READING K, EPSILON AND DIFFUSION WHEN NOT RELEVANT
!
      IF(ITURB.NE.3) ALIRE(10) = 0
      IF(ITURB.NE.3) ALIRE(11) = 0
      IF(ITURB.EQ.1) ALIRE(12) = 0
!
!     BO @ HRW: DIFFERENTIATED VARIABLES
!     FOR READING GRADIENTS IN SELAFIN FILES
!
      IF(NADVAR.GT.0) THEN
        DO IVAR=1,NADVAR
!         SEE NOMVAR_TELEMAC2D
          ALIRE(34+NTRAC+2*NPERIAF+IVAR) = 1
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     USE DOPRINT TO LIMIT TELEMAC-2D OUTPUTS IN THE LISTING
!
      IF(PRESENT(DOPRINT)) THEN
        LISTIN =  DOPRINT
        ENTET  =  DOPRINT
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(PASS.LE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'INITIALISATION DE TELEMAC2D POUR ',CODE
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INITIALISING TELEMAC2D FOR ',CODE
        ENDIF
        CALL TELEMAC2D_INIT(CODE)
        IF(LNG.EQ.1) WRITE(LU,*) 'FIN D''INITIALISATION DE TELEMAC2D'
        IF(LNG.EQ.2) WRITE(LU,*) 'TELEMAC2D INITIALISED'
        IF(PASS.EQ.0) THEN
          RETURN
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_TIMESTEP_BEGIN
#endif
700   CONTINUE
!
!-----------------------------------------------------------------------
!
!     INITIALIZATION OF LEO
      LEO=.FALSE.
      IMP=.FALSE.
!
!-----------------------------------------------------------------------
!
      IF(PASS.EQ.1) THEN
        IF(CODE(1:7).EQ.'ESTEL3D') THEN
          AT=ATDEP
          NIT=NITER
!         USE THE TIME STEP SPECIFIED BY ESTEL-3D
          IF(PRESENT(DTDEP)) THEN
            DT = DTDEP
            DTCAS = DTDEP
          ! TO DO: CHECK WHAT HAPPENS WITH ADAPTIVE TIME STEP
          ENDIF
        ELSE
!         IF(LNG.EQ.1) WRITE(LU,*) 'PROGRAM APPELANT INCONNU'
!         IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN CALLING PROGRAM'
!         CALL PLANTE(1)
!         STOP
        ENDIF
      ENDIF
!
      LT = LT + 1
!
      IF(BRECHE) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BREACH'
        CALL BREACH
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BREACH'
      ENDIF
!
      IF(DTVARI.AND.EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!       COURANT NUMBER FOR PSI SCHEME IN P1
        CALL CFLPSI(T1,U,V,DT,IELM,MESH,MSK,MASKEL)
        CALL MAXI(CFLMAX,IMAX,T1%R,NPOIN)
!       LIMITS VARIATIONS IN THE RANGE (1/2, 2)
        DT = DT * MAX(MIN(CFLWTD/MAX(CFLMAX,1.D-6),2.D0),0.5D0)
!       LIMITS DT TO THAT OF THE STEERING FILE
        DT=MIN(DT,DTCAS)
!       for final time step
        IF(DUREE+AT0.GT.AT)DT= MIN(DT,DUREE+AT0-AT)
        IF(NCSIZE.GT.1) DT=P_DMIN(DT)
        IF(ENTET) THEN
          IF (LNG.EQ.1) WRITE(LU,78) CFLMAX,DT
          IF (LNG.EQ.2) WRITE(LU,79) CFLMAX,DT
78        FORMAT(1X,'    NOMBRE DE COURANT MAXIMUM :',G16.7,/,1X,
     &              '    PAS DE TEMPS              :',G16.7)
79        FORMAT(1X,'    MAXIMUM COURANT NUMBER: ',G16.7,/,1X,
     &              '    TIME-STEP                 :',G16.7)
        ENDIF
      ENDIF
!
!=======================================================================
!
!     COUPLING WITH TOMAWAC
!
      IF(INCLUS(COUPLING,'TOMAWAC').AND.
     &   PERCOU_WAC*((LT-1)/PERCOU_WAC).EQ.LT-1) THEN
!
        CALL CONFIG_CODE(3)
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING TOMAWAC'
!       CALL WAC(1,U,V,H,FXWAVE,FYWAVE,WINDX,WINDY,CODE1,AT,
!    &           DT,NIT,PERCOU_WAC,DIRMOY,HM0,TPR5)
        CALL WAC(1,U,V,H,FXWAVE,FYWAVE,T1   ,T2   ,CODE1,AT,
     &           DT,NIT,PERCOU_WAC,DIRMOY,HM0,TPR5,ORBVEL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM TOMAWAC'
        CALL CONFIG_CODE(1)
!
      ENDIF
!
!=======================================================================
!
      IF(ADJO) THEN
        AT = AT - DT
      ELSE
!       DT IS NOT YET KNOWN IN FINITE VOLUMES
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') AT = AT + DT
      ENDIF
!
      IF(DTVARI) THEN
        IF(ABS(AT-(DUREE+AT0)).LT.1.E-14) THEN
!         LAST TIME STEP
          NIT   = LT
          ENTET = LISTIN
          TOTAL_ITER= NIT
        ELSE
!         DUMMY VALUE GREATER THAN LT
          NIT = LT + 10
          TOTAL_ITER = NIT
        ENDIF
      ENDIF
!
      IF((LISPRD*(LT/LISPRD).EQ.LT.AND.LT.GE.PTINIL).OR.LT.EQ.NIT) THEN
        ENTET=LISTIN
      ELSE
        ENTET=.FALSE.
      ENDIF
!
!     CONSTRAINS TELEMAC-2D OUTPUT IN THE LISTING
!
      IF (PRESENT(DOPRINT)) ENTET = ENTET .AND. DOPRINT
!
      IF(ENTET) CALL ENTETE(2,AT,LT)
!
!=======================================================================
!
! BACKUP OF UN, VN, HN, TN, AKN AND EPN (THEY ARE IN FN)
!
! THIS IS NOT DONE WHEN ITERATING FOR THE COUPLING WITH ESTEL-3D
      IF(CODE(1:7).EQ.'ESTEL3D'.AND.PRESENT(NEWTIME)) THEN
        IF(NEWTIME) CALL OS('X=Y     ',X=FN,Y=F)
      ELSE
        CALL OS('X=Y     ',X=FN,Y=F)
      ENDIF
!     CALL OS( 'X=Y     ' , FN , F , F , C )
!
!=======================================================================
!
! NEW COUPLING WITH SISYPHE FOR CONSTANT FLOW DISCHARGE
!
      IF(SIS_CPL%SISYPHE_CFD.AND.SIS_CPL%CONSTFLOW) GOTO 999
!
      DO ISIS_CFD=1,SIS_CPL%NSIS_CFD
!
!=======================================================================
!
!  MASKING OF THE WETTING/DRYING ELEMENTS
!
      IF(MSK) CALL OS( 'X=C     ' , MASKEL , S , S , 1.D0 )
      IF (OPTBAN.EQ.2) THEN
        CALL MASKBD(MASKEL%R,ZFE%R,ZF%R,H%R,
     &              HMIN,MESH%IKLE%I,MESH%IFABOR%I,IT1%I,NELEM,NPOIN)
      ENDIF
!
!  MASKING SPECIFIED BY USER
!
      IF(MSKUSE) THEN
        CALL MASKOB(MASKEL%R,MESH%X%R,MESH%Y%R,
     &              MESH%IKLE%I,NELEM,NELMAX,NPOIN,AT,LT)
      ENDIF
!
! CREATES THE MASK OF THE POINTS FROM THE MASK OF THE ELEMENTS
! AND CHANGES OF IFAMAS (IFABOR WITH MASKING)
!
      IF(MSK) THEN
        CALL MASKTO(MASKEL%R,MASKPT,IFAMAS%I,MESH%IKLE%I,
     &              MESH%IFABOR%I,MESH%ELTSEG%I,MESH%NSEG,
     &              NELEM,NPOIN,IELMT,MESH)
        IF(IELMX.NE.IELM1) CALL CHGDIS(MASKPT,IELM1,IELMX,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!  COMPUTATION OF THE INTEGRAL OF THE BASES
!-----------------------------------------------------------------------
!
!     IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING MASBAS2D'
!     IF(MSK) THEN
!       CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,IELM1,MESH,MSK,MASKEL,T2,T2)
!     ENDIF
!     IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM MASBAS2D'
!
!-----------------------------------------------------------------------
!
! UPDATES POROSITY : NEW VALUE IN TE5
!                    OLD - NEW IN TE4
!
      IF(OPTBAN.EQ.3) THEN
!
        CALL OS('X=Y     ',TE4,TE5,TE5,0.D0)
        CALL POROS(TE5,ZF,HN,MESH)
        IF(MSK) CALL OS('X=XY    ',X=TE5,Y=MASKEL)
!       TEST OF UNDER-RELAXATION
        RELAX = 0.05D0
        CALL OS('X=CX    ',X=TE5,C=RELAX)
        CALL OS('X=X+CY  ',X=TE5,Y=TE4,C=1.D0-RELAX)
!       TE4 = OLD POROS - NEW POROS
        CALL OS('X=X-Y   ',X=TE4,Y=TE5)
!
      ENDIF
!
!=======================================================================
!
! NEW ADVECTION AND PROPAGATION FIELDS
! NOTE THAT U = UN, V = VN AND H = HN AT THIS STAGE
!
      IF(CONV) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CHPCONV'
        CALL CHPCONV(UCONV,VCONV,U,V,UN,VN,TETAU)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CHPCONV'
      ENDIF
!
!     COMPUTATION OF THE NEW PROPAGATION TERM
!
      IF(PROPA) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING HPROPA'
        CALL HPROPA(HPROP ,HN,H,PROLIN,HAULIN,TETAC,NSOUSI)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM HPROPA'
      ENDIF
!
!=======================================================================
!
! PREPARES BOUNDARY CONDITIONS FOR WEIRS.
!
      IF(NWEIRS.GT.0) THEN
!
        IF(TYPSEUIL.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CLSING'
          CALL CLSING(NWEIRS,NPSING,NDGA1,NDGB1,
     &                MESH%X%R,MESH%Y%R,ZF%R,CHESTR%R,NKFROT%I,
     &                KARMAN,ZDIG,PHIDIG,MESH%NBOR%I,
     &                H%R,T,NTRAC,IOPTAN_T2D,T1%R,UBOR%R,VBOR%R,TBOR,
     &                LIHBOR%I,LIUBOR%I,LIVBOR%I,LITBOR,GRAV)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CLSING'
        ELSEIF(TYPSEUIL.EQ.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CALCUL_Q_WEIR'
          CALL CALCUL_Q_WEIR(IOPTAN_T2D)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCUL_Q_WEIR'
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'LECSNG : TYPE DE SEUIL NON PROGRAMME '
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*)'LECSNG : TYPE OF WEIRS NOT IMPLEMENTED'
          ENDIF
        ENDIF
!
      ENDIF
!
! IT IS ASSUMED THAT THE TYPES OF BOUNDARY CONDITIONS DO NOT CHANGE
! DURING THE SUB-ITERATIONS. IF NOT IT IS NECESSARY TO MOVE THE CALLS
! TO KEPSIN, DIFFIN, PROPIN
!
! TYPES OF CONDITIONS FOR THE K-EPSILON MODEL
!
      IF(ITURB.EQ.3) CALL KEPSIN(LIMKEP%I,LIUBOR%I,NPTFR,
     &                           KENT,KENTU,KSORT,KADH,KLOG,
     &                           KINC,KNEU,KDIR)
!
! TYPES OF CONDITIONS FOR THE SPALART-ALLMARAS MODEL
!
      IF(ITURB.EQ.6) CALL SPALALLIN(LIMSA%I, LIUBOR%I, NPTFR,
     &                              KENT, KENTU, KSORT, KADH, KLOG,
     &                              KINC, KNEU, KDIR)
!
! TYPES OF CONDITIONS FOR THE DIFFUSION OF THE TRACER:
!
      IF(NTRAC.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DIFFIN'
        DO ITRAC=1,NTRAC
        CALL DIFFIN(MASKTR,LIMTRA%I,LITBOR%ADR(ITRAC)%P%I,
     &              IT1%I,UCONV%R,VCONV%R,
     &              MESH%XNEBOR%R,MESH%YNEBOR%R,
     &              MESH%NBOR%I,NPTFR,
     &              KENT,KSORT,KLOG,KNEU,KDIR,KDDL,
     &              ICONVFT(ITRAC),
     &              MESH%NELBOR%I,NPOIN,NELMAX,MSK,MASKEL%R,
     &              NFRLIQ,THOMFR,FRTYPE,
     &              TN%ADR(ITRAC)%P,TBOR%ADR(ITRAC)%P,MESH,NUMLIQ%I,
     &              MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DIFFIN'
      ENDIF
!
! TYPES OF CONDITIONS FOR THE PROPAGATION:
! REQUIRED FOR THOMFR ?? (OTHERWISE DONE AFTER BORD !)
!
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROPIN'
      CALL PROPIN_TELEMAC2D
     &            (LIMPRO%I,LIMPRO%DIM1,MASK,LIUBOR%I,LIVBOR%I,
     &             LIHBOR%I,MESH%NBOR%I,NPTFR,
     &             KENT,KENTU,KSORT,KADH,KLOG,
     &             KNEU,KDIR,KDDL,CLH%I,CLU%I,CLV%I,
     &             U%ELM,U%R,V%R,GRAV,H%R,LT,NPOIN,
     &             MESH%NELBOR%I,NELMAX,MSK,MASKEL%R,
     &             NFRLIQ,THOMFR,NUMLIQ%I,FRTYPE,
     &             MESH%XNEBOR%R,MESH%YNEBOR%R,MESH%IKLBOR%I,.FALSE.,
     &             MESH%NELEBX,MESH%NELEB)
!    *             MESH%XNEBOR%R,MESH%YNEBOR%R,MESH%IKLBOR%I,ENTET )
!       WARNINGS WILL BE GIVEN AT THE SECOND CALL AFTER BORD
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROPIN'
!
!=======================================================================
!                 COMPUTES THE FRICTION COEFFICIENTS
!                         VARIABLE IN TIME
!=======================================================================
! CORSTR DOES NOT DO ANYTHING UNLESS MODIFIED BY THE USER.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CORSTR'
      CALL CORSTR
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CORSTR'
!
      IF(IELMU.EQ.12.OR.IELMU.EQ.13) THEN
        CALL CHGDIS(CHESTR,DISCLIN,IELMU,MESH)
      ENDIF
!
      CALL FRICTION_CHOICE(1)
!
!=======================================================================
!                 COMPUTES VISCOSITY COEFFICIENTS
!=======================================================================
!
!  COMPUTES DYNAMIC VISCOSITY VISC
!
      IF(ITURB.EQ.1) THEN
!
        CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
!
      ELSEIF(ITURB.EQ.2) THEN
!
        CALL DISPER( VISC , U%R , V%R , H%R , CF%R , ELDER , PROPNU )
!
      ELSEIF(ITURB.EQ.3) THEN
!
        CALL VISTUR(VISC,AK,EP,NPOIN,CMU,PROPNU)
!
      ELSEIF(ITURB.EQ.4) THEN
!
        CALL SMAGOR(VISC,CF,U,V,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU)
!
      ELSEIF(ITURB.EQ.5) THEN
!
        CALL MIXLENGTH(VISC,CF,U,V,H,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU,
     &                 UNSV2D,IELMU,NPTFR)
!
      ELSEIF(ITURB.EQ.6) THEN
!
        CALL VISTURSA(VISC,VISCSA,NPOIN,PROPNU)
!
      ELSE
!
        IF(LISTIN) THEN
          IF(LNG.EQ.1) WRITE(LU,15) ITURB
          IF(LNG.EQ.2) WRITE(LU,16) ITURB
15        FORMAT(1X,'ITURB=',1I6,'MODELE DE TURBULENCE NON PREVU')
16        FORMAT(1X,'ITURB=',1I6,'UNKNOWN TURBULENCE MODEL')
        ENDIF
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!  COEFFICIENT FOR THERMAL DIFFUSION (PRANDTL = 1 FOR NOW)
!  AND THE SAME FOR ALL THE TRACERS
!
      IF(NTRAC.GT.0.AND.DIFT) THEN
        DO ITRAC=1,NTRAC
          CALL OS('X=Y+C   ',X=VISCT%ADR(ITRAC)%P,Y=VISC,C=DIFNU-PROPNU)
        ENDDO
      ENDIF
!
!  IT IS POSSIBLE TO CORRECT THE VISCOSITY COEFFICIENTS.
!
      CALL CORVIS
!
!=======================================================================
!  SOURCES : COMPUTATION OF INPUTS WHEN VARYING IN TIME
!            IF NO VARIATION IN TIME DSCE2=DSCE AND TSCE2=TSCE
!=======================================================================
!
      IF(NREJET.GT.0) THEN
        DO I=1,NREJET
          DSCE2(I)=DEBSCE(AT,I,DSCE)
        ENDDO
        IF(NTRAC.GT.0) THEN
          DO I=1,NREJET
            DO ITRAC=1,NTRAC
              TSCE2(I,ITRAC)=TRSCE(AT,I,ITRAC)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! BOUNDARY CONDITIONS
!=======================================================================
!
      IF(THOMFR) THEN
!
      CALL CPSTVC(H,T9)
      CALL PREBOR(HBOR%R,UBOR%R,VBOR%R,TBOR,U%R,V%R,H%R,
     &            T9%R,T,MESH%NBOR%I,
     &            NPOIN,NPTFR,NTRAC,NFRLIQ,FRTYPE,NUMLIQ%I)
!
      ENDIF
!
! CALLS THE USER-SUBROUTINE DETERMINING THE BOUNDARY CONDITIONS.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BORD'
      CALL BORD(HBOR%R,UBOR%R,VBOR%R,TBOR,
     &          U,V,H,ZF%R,MESH%NBOR%I,W1DEB,T8,
     &          LIHBOR%I,LIUBOR%I,LITBOR,
     &          MESH%XNEBOR%R,MESH%YNEBOR%R,NPOIN,NPTFR,
     &          NPTFR2,AT,
     &          NDEBIT,NCOTE,NVITES,NTRAC,NTRACE,NFRLIQ,NUMLIQ%I,
     &          KENT,KENTU,PROVEL,MASK,MESH,EQUA,T2D_FILES(T2DIMP)%NAME)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BORD'
!
! COMPUTES LIMPRO, CLU,CLV, CLH AND MASK
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROPIN'
      CALL PROPIN_TELEMAC2D
     &            (LIMPRO%I,LIMPRO%DIM1,MASK,LIUBOR%I,LIVBOR%I,
     &             LIHBOR%I,MESH%NBOR%I,NPTFR,
     &             KENT,KENTU,KSORT,KADH,KLOG,
     &             KNEU,KDIR,KDDL,CLH%I,CLU%I,CLV%I,
     &             U%ELM,U%R,V%R,GRAV,H%R,LT,NPOIN,
     &             MESH%NELBOR%I,NELMAX,MSK,MASKEL%R,
     &             NFRLIQ,THOMFR,NUMLIQ%I,FRTYPE,
     &             MESH%XNEBOR%R,MESH%YNEBOR%R,MESH%IKLBOR%I,ENTET,
     &             MESH%NELEBX,MESH%NELEB)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROPIN'
!
! PREPARING THE FRICTION ON THE LATERAL BOUNDARIES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING WALL_FRICTION'
      CALL WALL_FRICTION(UETUTA%R,AUBOR%R,CFBOR%R,
     &                   MESH%DISBOR%R,UN%R,VN%R,LIMPRO%I,
     &                   MESH%NBOR%I,NPTFR,KARMAN,PROPNU,
     &                   LISRUG,KNEU,KDIR,KENT,KENTU,KADH,KLOG,
     &                   IELMU,MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM WALL_FRICTION'
!
! K-EPSILON BOUNDARY CONDITIONS: KBOR,EBOR AND AUBOR
!
      IF(ITURB.EQ.3) THEN
        CALL KEPSCL(KBOR%R,EBOR%R,AUBOR%R,CF%R,CFBOR%R,
     &              MESH%DISBOR%R,
     &              UN%R,VN%R,HN%R,LIMKEP%I,LIUBOR%I,LIMPRO%I,
     &              MESH%NBOR%I,NPTFR,KARMAN,CMU,C2,ESTAR,
     &              SCHMIT,LISRUG,PROPNU,KMIN,EMIN,KNEU,KDIR,
     &              KENT,KENTU,KADH,KLOG,UETUTA%R)
      ENDIF
!
! SA BOUNDARY CONDITIONS : NUBOR
!
      IF(ITURB.EQ.6) THEN
        CALL SPALALLCL(NUBOR%R, LIMSA%I, LIUBOR%I,NPTFR,NUMIN,PROPNU,
     &                 KNEU,KDIR,KENT,KENTU, KADH, KLOG,KSORT)
      ENDIF
!
! CALLS THE SYSTEM OF RESOLUTION FOR BOUNDARIES BY THE CHARACTERISTICS
! METHOD (THOMPSON)
!
      IF(THOMFR) THEN
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING THOMPS'
      CALL THOMPS(HBOR%R,UBOR%R,VBOR%R,TBOR,U,V,T9,
     &            T,ZF,MESH%X%R,MESH%Y%R,MESH%NBOR%I,
     &            FRTYPE,T2,T3,T4,T7,T10,T11,
     &            LIHBOR%I,LIUBOR%I,LIVBOR%I,LITBOR,IT1%I,
     &            IT2%I,CV2%R,CV3%R,TE1%R,HTILD,UTILD,VTILD,
     &            TTILD,T15,MESH%SURDET%R,MESH%IKLE%I,
     &            MESH%IFABOR%I,NELEM,MESH,
     &            MESH%XNEBOR%R,MESH%YNEBOR%R,
     &            NPOIN,NPTFR,DT,GRAV,NTRAC,
     &            NFRLIQ,KSORT,KINC,KENT,KENTU,MESH%LV,MSK,MASKEL,
     &            NELMAX,IELM,T5%R,NUMLIQ%I,BM1%X%R,
     &            T12%R,T13%R,T14%R,IT3,IT4,
     &            T17,T18,T19,T20,T21,T22,W1)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM THOMPS'
!
      ENDIF
!
!     CHECKS HBOR BECAUSE THE USER CAN MODIFY BORD AND MAKE A MISTAKE
      CALL CLIP(HBOR,0.D0,.TRUE.,1.D6,.FALSE.,0)
!
!=======================================================================
!
! LOOP OVER THE SUB-ITERATIONS WHERE ADVECTION AND PROPAGATION ARE UPDATED
!
!=======================================================================
!
      DO ISOUSI = 1 , NSOUSI
      IF(DEBUG.GT.0) WRITE(LU,*) 'BOUCLE 701 ISOUSI=',ISOUSI
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_SUBITERATION_BEGIN
#endif
!
!=======================================================================
!
! : 4                     ADVECTION
!
!=======================================================================
!
      IF(CONV.AND.(FTILD%N.GT.0.OR.FTILD2%N.GT.0)) THEN
!
        IF(ENTET) CALL ENTETE(3,AT,LT)
!
        IF(SPHERI) THEN
          CALL OS('X=Y/Z   ',UCONV,UCONV,MESH%COSLAT,C)
          CALL OS('X=Y/Z   ',VCONV,VCONV,MESH%COSLAT,C)
        ENDIF
!
!       COMPUTATION OF STRONG CHARACTERISTICS AND INTERPOLATION
!
        IF(FTILD%N.GT.0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CHARAC OPTION STRONG'
          CALL CHARAC(FNCAR , FTILD  , FTILD%N  , UCONV , VCONV,S,S,S,S,
     &                DT    , IFAMAS , IELM     , NPOIN , 1,1,1,
     &                MSK   , MASKEL , BM1%X    , BM1%D , BM1%D , TB   ,
     &                IT1%I , IT2%I  , IT2%I    ,IT3%I  , IT4%I , IT2%I,
     &                MESH  , MESH%NELEM        ,MESH%NELMAX    ,
     &                MESH%IKLE,MESH%SURDET,
!                     FOR WEAK FORM OF ADVECTION                OPTCHA
     &                AM1,CV1,SLVPRO,AGGLOW,ENTET,NGAUSS,UNSV2D,1)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CHARAC OPTION STRONG'
        ENDIF
!
!       COMPUTATION OF WEAK CHARACTERISTICS AND INTERPOLATION
!
        IF(FTILD2%N.GT.0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CHARAC OPTION WEAK'
          CALL CHARAC(FNCAR2, FTILD2 , FTILD2%N , UCONV , VCONV,S,S,S,S,
     &                DT    , IFAMAS , IELM     , NPOIN , 1,1,1,
     &                MSK   , MASKEL , BM1%X    , BM1%D , BM1%D , TB   ,
     &                IT1%I , IT2%I  , IT2%I    ,IT3%I  , IT4%I , IT2%I,
     &                MESH  , MESH%NELEM        ,MESH%NELMAX    ,
     &                MESH%IKLE,MESH%SURDET,
!                     FOR WEAK FORM OF ADVECTION                OPTCHA
     &                AM1,CV1,SLVPRO,AGGLOW,ENTET,NGAUSS,UNSV2D,2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CHARAC OPTION WEAK'
        ENDIF
!
        IF(SPHERI) THEN
          CALL OS('X=XY    ',UCONV,MESH%COSLAT,S,C)
          CALL OS('X=XY    ',VCONV,MESH%COSLAT,S,C)
        ENDIF
!
      ENDIF
!
! MANAGEMENT OF THE ARRAYS.
!
      CALL GESTIO(UN   ,VN   ,HN   ,TN   ,AKN   ,EPN   ,NUN   ,
     &            UTILD,VTILD,HTILD,TTILD,AKTILD,EPTILD,NUTILD,
     &            NTRAC.GT.0,PROPA,CONVV,ITURB,3)
!
!=======================================================================
!                       END OF ADVECTION
!=======================================================================
!=======================================================================
!
! : 6                DIFFUSION - PROPAGATION
!
!=======================================================================
!
      IF(PROPA) THEN
      IF(ENTET) CALL ENTETE(6,AT,LT)
!     INFORMATION ON THE METHOD OF RESOLUTION IS GIVEN ONLY
!     IF LISTING IS REQUESTED
      INFOGS=.FALSE.
      IF(INFOGR.AND.ENTET) INFOGS=.TRUE.
!
!  WEATHER CONDITIONS.
!
      IF(VENT.OR.ATMOS.OR.INCLUS(COUPLING,'WAQTEL')) THEN
        CALL METEO(PATMOS%R,WINDX%R,WINDY%R,
     &             FUAIR,FVAIR,MESH%X%R,MESH%Y%R,AT,LT,NPOIN,VENT,ATMOS,
     &             H%R,GRAV,ROEAU,PRIVE,
     &             T2ATMA,T2ATMB,T2D_FILES,LISTIN,PATMOS_VALUE,
     &             INCLUS(COUPLING,'WAQTEL'),PLUIE,OPTWIND,WIND_SPD)
      ENDIF
!
!  COMPUTES THE DENSITY WHEN IT IS VARIABLE
!
      IF(ROVAR) THEN
!       BEWARE, SALINITY MUST BE HERE THE FIRST TRACER
        CALL VALRO(RO,T,ROEAU)
      ENDIF
!
!  SOURCE TERMS DUE TO NOZZLES AND SIPHONS.
!
      IF(NSIPH.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING SIPHON'
        CALL SIPHON(RELAXS,NSIPH,ENTSIP%I,SORSIP%I,GRAV,
     &              H%R,ZF%R,DSIP%R,SECSIP%R,ALTSIP%R,CSSIP%R,CESIP%R,
     &              DELSIP%R,ANGSIP%R,LSIP%R,
     &              NTRAC,T,TSIP,USIP%R,VSIP%R,U%R,V%R,ENTET)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SIPHON'
      ENDIF
!
!  SOURCE TERMS DUE TO TUBES OR BRIDGES.

      IF(NBUSE.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BUSE'
        CALL BUSE(RELAXB,NBUSE,ENTBUS%I,SORBUS%I,GRAV,
     &            H%R,ZF%R,DBUS%R,LRGBUS%R,HAUBUS%R,CLPBUS%I,
     &            ALTBUS%R,CSBUS%R,CEBUS%R,ANGBUS%R,LBUS%R,
     &            NTRAC,T,TBUS,UBUS%R,VBUS%R,U%R,V%R,ENTET,
     &            CV%R,C56%R,CV5%R,C5%R,CTRASH%R,FRICBUS%R,
     &            LONGBUS%R,CIRC%I,OPTBUSE,V2DPAR,DT,SECBUS%R,
     &            MAXSCE)

        CALL OS('X=0     ',X=PRIVE)
        DO I=1,NBUSE
           PRIVE%ADR(1)%P%R(ENTBUS%I(I))=-DBUS%R(I)
           PRIVE%ADR(1)%P%R(SORBUS%I(I))= DBUS%R(I)
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BUSE'
      ENDIF
!


!  SOURCE TERMS FOR PROPAGATION.
!
      IF(EQUA(1:15).NE.'SAINT-VENANT VF')THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROSOU'
        CALL PROSOU(FU,FV,SMH,UN,VN,HN,GRAV,NORD,
     &              FAIR,WINDX,WINDY,VENT,HWIND,
     &              CORIOL,FCOR,SPHERI,YASMH,
     &              MESH%COSLAT,MESH%SINLAT,AT,LT,DT,
     &              NREJET,NREJEU,DSCE2,ISCE,T1,MESH,MSK,MASKEL,
     &              MAREE,MARDAT,MARTIM,PHI0,OPTSOU,COUROU,NPTH,
     &              VARCL,NVARCL,VARCLA,UNSV2D,FXWAVE,FYWAVE,
     &              RAIN,RAIN_MMPD,PLUIE,T2D_FILES,T2DBI1,
     &              BANDEC,OPTBAN,
     &              NSIPH,ENTSIP%I,SORSIP%I,DSIP%R,USIP%R,VSIP%R,
     &              NBUSE,ENTBUS%I,SORBUS%I,DBUS%R,UBUS%R,VBUS%R,
     &              TYPSEUIL,NWEIRS,N_NGHB_W_NODES,
     &              NDGA1,NDGB1,MESH%NBOR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROSOU'
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROSOU_FV'
        CALL PROSOU_FV(FU,FV,SMH,UN,VN,HN,GRAV,NORD,
     &              FAIR,WINDX,WINDY,VENT,HWIND,
     &              CORIOL,FCOR,SPHERI,YASMH,YASMO,
     &              MESH%COSLAT,MESH%SINLAT,AT,LT,DT,
     &              NREJET,NREJEU,DSCE2,ISCE,T1,MESH,MSK,MASKEL,
     &              MAREE,MARDAT,MARTIM,PHI0,OPTSOU,COUROU,NPTH,
     &              VARCL,NVARCL,VARCLA,UNSV2D,FXWAVE,FYWAVE,
     &              RAIN,RAIN_MMPD,PLUIE,T2D_FILES,T2DBI1,
     &              BANDEC,OPTBAN,
     &              NSIPH,ENTSIP%I,SORSIP%I,DSIP%R,USIP%R,VSIP%R,
     &              NBUSE,ENTBUS%I,SORBUS%I,DBUS%R,UBUS%R,VBUS%R,
     &              TYPSEUIL,NWEIRS,N_NGHB_W_NODES,
     &              NDGA1,NDGB1,MESH%NBOR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROSOU_FV'
      ENDIF
!
!  PROPAGATION.
!
      HN%DIM1=NPOIN
      ZF%DIM1=NPOIN
      IF(EQUA(1:15).EQ.'SAINT-VENANT EF'.OR.
     &   EQUA(1:10).EQ.'BOUSSINESQ') THEN
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROPAG'
      CALL PROPAG
     & (U,V,H,UCONV,VCONV,CONVV,H0,PATMOS,ATMOS,
     & HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,
     & FU,FV,
     & SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,MBOR,
     & CV1,CV2,CV3,W1,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     & TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T7,T8,
     & LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,AT,DT,
     & TETAC,TETAC,TETAU,TETAD,
     & AGGLOC,AGGLOU,KDIR,INFOGS,KFROT,ICONVF,
     & PRIVE,ISOUSI,BILMAS,MASSES,MASS_RAIN,YASMH,OPTBAN,CORCON,
     & OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     & MAT,RHS,UNK,TB,S,TB,PRECCU,SOLSYS,CFLMAX,OPDVIT,OPTSOU,
!                       TB HERE TO REPLACE BD SUPPRESSED, NOT USED
     & NFRLIQ,SLVPRO,EQUA,VERTIC,ADJO,ZFLATS,TETAZCOMP,
     & UDEL,VDEL,DM1,ZCONV,COUPLING,FLBOR,BM1S,BM2S,CV1S,
     & VOLU2D,V2DPAR,UNSV2D,NDGA1,NDGB1,NWEIRS,NPSING,HFROT,
     & FLULIM,YAFLULIM,FLULIMEBE,YAFLULIMEBE,
     & RAIN,PLUIE,MAXADV,OPTADV_VI,MESH%IKLE%I,MESH%NELMAX) !NH uncomment 2 args on WIN
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROPAG'
!
      IF(ADJO) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROPAG_ADJ'
        CALL PROPAG_ADJ
     &( UCONV,VCONV,CONVV,H0,PATMOS,ATMOS,
     &  HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,
     &  FU,FV,SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,
     &  MBOR,CV1,CV2,CV3,W1,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     &  TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,
     &  LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,AT,DT,
     &  TETAC,TETAC,TETAU,TETAD,
     &  AGGLOC,AGGLOU,KDIR,INFOGS,KFROT,ICONVF,
     &  PRIVE,ISOUSI,BILMAS,MASSES,YASMH,OPTBAN,CORCON,
     &  OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     &  MAT,RHS,UNK,TB,S,TB,PRECCU,SOLSYS,CFLMAX,OPDVIT,
     &  OPTSOU,NFRLIQ,SLVPRO,EQUA,VERTIC,
     &  ADJO,UD,VD,HD,U,V,H,UU,VV,HH,UIT1,VIT1,HIT1,PP,QQ,RR,
     &  TAM1,TAM2,TAM3,TBM1,TBM2,TCM1,TCM2,MATADJ,UNKADJ,
     &  ALPHA1,ALPHA2,ALPHA3,ADJDIR,ESTIME,OPTCOST,NIT,NVARRES,
     &  VARSOR,T2D_FILES(T2DRES)%LU,T2D_FILES(T2DREF)%LU,
     &  ALIRE,TROUVE,MAXVAR,VARCL,VARCLA,TEXTE,
     &  TEXREF,TEXRES,W,CHESTR,KARMAN,NDEF,ITURB,LISRUG,
     &  LINDNER,SB,DP,SP,CHBORD,CFBOR,HFROT,UNSV2D)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROPAG_ADJ'
!
       ENDIF
!

!       WRITE(*,*) 'V2DPAR%R(81088)',V2DPAR%R(81088)
!       WRITE(*,*) 'V2DPAR%R(39190)',V2DPAR%R(39190)
!       WRITE(*,*) 'V2DPAR%R(39112)',V2DPAR%R(39112)
!       WRITE(*,*) 'V2DPAR%R(39038)',V2DPAR%R(39038)
!       WRITE(*,*) 'V2DPAR%R(39345)',V2DPAR%R(39345)
!       WRITE(*,*) 'V2DPAR%R(39274)',V2DPAR%R(39274)
!       WRITE(*,*) 'V2DPAR%R(39196)',V2DPAR%R(39196)
!       WRITE(*,*) 'V2DPAR%R(39118)',V2DPAR%R(39118)
!       WRITE(*,*) 'V2DPAR%R(39430)',V2DPAR%R(39430)
!       WRITE(*,*) 'V2DPAR%R(39349)',V2DPAR%R(39349)
!       WRITE(*,*) 'V2DPAR%R(39278)',V2DPAR%R(39278)
!       WRITE(*,*) 'V2DPAR%R(39201)',V2DPAR%R(39201)
!       WRITE(*,*) 'V2DPAR%R(39512)',V2DPAR%R(39512)
!       WRITE(*,*) 'V2DPAR%R(39439)',V2DPAR%R(39439)
!       WRITE(*,*) 'V2DPAR%R(39354)',V2DPAR%R(39354)
!       WRITE(*,*) 'V2DPAR%R(39284)',V2DPAR%R(39284)
!       WRITE(*,*) 'V2DPAR%R(39598)',V2DPAR%R(39598)
!       WRITE(*,*) 'V2DPAR%R(39518)',V2DPAR%R(39518)
!       WRITE(*,*) 'V2DPAR%R(39447)',V2DPAR%R(39447)
!       WRITE(*,*) 'V2DPAR%R(39360)',V2DPAR%R(39360)
!       WRITE(*,*) 'V2DPAR%R(81086)',V2DPAR%R(81086)
!       WRITE(*,*) 'V2DPAR%R(39602)',V2DPAR%R(39602)
!       WRITE(*,*) 'V2DPAR%R(39519)',V2DPAR%R(39519)
!       WRITE(*,*) 'V2DPAR%R(39451)',V2DPAR%R(39451)
!
!!       PAUSE



      ELSEIF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
!
!       VOLFIN MAY CHANGE DT
!
!       CM1%D%R : HT
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING VOLFIN'
        CALL VOLFIN(W1%R,AT,DT,LT,NELEM,NPTFR,MESH%NSEG,
     &       TB,ZF%R,CHESTR%R,NPOIN,HN%R,H%R,U%R,V%R,QU%R,QV%R,
     &       GRAV,ENTET,MESH,LIMPRO%I,
     &       MESH%NBOR%I,KDIR,KNEU,KDDL,HBOR%R,UBOR%R,VBOR%R,
     &       MASSES,FLUENT,FLUSOR,CFLWTD,DTVARI,KFROT,
     &       NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,YASMH,SMH%R,
     &       NTRAC,T%ADR(1)%P%DIM1,T,HT,TN,
     &       TBOR,MASSOU,FLUTENT,FLUTSOR,MESH%DTHAUT%R,
     &       MESH%DPX%R,MESH%DPY%R,CM1%X%R,CM2%X%R,
     &       MESH%CMI%R,MESH%JMI%I,TE1%R,TE2%R,
     &       DIFVIT,ITURB,PROPNU,DIFT,DIFNU,
     &       BM1%X%R,BM2%X%R,OPTVF,
     &       HSTOK%R,HCSTOK%R,LOGFR%I,DSZ%R,FLUXT,FLUHBOR,
     &       FLBOR,DTN,FLUSORTN,FLUENTN,
     &       LT2,FLUXTEMP,FLUHBTEMP,HC%R,SMTR,MESH%AIRST%R,
     &       TMAX,DTT,GAMMA,FLUX_OLD,MXPTVS,NEISEG%I,V2DPAR,
     &       UDEL,VDEL,HROPT,RAIN,PLUIE,MASS_RAIN,BILMAS,
     &       FU,FV,YASMO,CORIOL,FCOR,SPHERI,MESH%COSLAT,MESH%SINLAT,LEO)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM VOLFIN'
!
        AT = AT + DT
        IF (LT.GE.NIT.OR.AT.GT.TMAX.OR.
     &      ((TMAX-AT)/TMAX).LT.1E-15) THEN !LAST TIME STEP
           ! Set lt as the last timestep
           NIT = LT
           ENTET = .TRUE.
           CALL ENTETE(1,AT,LT)
           TOTAL_ITER=LT
        ENDIF
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,*) 'EQUATIONS INCONNUES : ',EQUA
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN EQUATIONS: ',EQUA
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
! IF NO PROPAGATION :
!
      ELSE
!
! MANAGEMENT OF THE ARRAYS .
!
        CALL GESTIO(U    ,V    ,H    ,T,AK  ,EP , NUN   ,
     &              UTILD,VTILD,HTILD,T,AK  ,EP , NUTILD,
     &              NTRAC.GT.0,PROPA,CONVV,ITURB ,6)
!
!       SMH USED BY THE TRACER
!       TO SIMULATE SUBIEF TAKING OFF PROPAGATION
!       AND ADVECTION, PROSOU IS NOT CALLED AND DISCRETE
!       SOURCES ARE NOT TAKEN INTO ACCOUNT.
!       STRICTLY 'CALL PROSOU' SHOULD BE HERE.
        IF(NTRAC.GT.0) CALL OS('X=0     ',X=SMH)
!
      ENDIF
!
!     TREATMENT OF NEGATIVE DEPTHS
!
      CALL CORRECTION_DEPTH_2D(MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &                         YAFLODEL,YASMH,YAFLULIM,YAFLULIMEBE)
!                              A ENLEVER
!
!=======================================================================
!                          END OF PROPAGATION
!=======================================================================
!
!  COMPUTES THE NEW ADVECTION FIELDS IF THERE REMAIN
!  SUB-ITERATIONS.
!
!  THE TEST ON ISOUSI IS MADE ONLY FOR HPROP AND NOT FOR UCONV
!  FOR REASONS OF TRACER MASS CONSERVATION (IT IS NECESSARY TO KEEP
!  THE SAME HPROP FOR THE TRACER AS THAT FOR H AND U)
!
      IF(ISOUSI.NE.NSOUSI) THEN
!       COMPUTES THE NEW PROPAGATION FIELD IF PROPAGATION
        IF(PROPA) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING HPROPA'
          CALL HPROPA(HPROP ,HN,H,PROLIN,HAULIN,TETAC,NSOUSI)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM HPROPA'
        ENDIF
      ENDIF
!
!     COMPUTES THE NEW ADVECTION FIELD (IF ADVECTION)
      IF(CONV) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CHPCONV'
        CALL CHPCONV(UCONV,VCONV,U,V,UN,VN,TETAU)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CHPCONV'
      ENDIF
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_SUBITERATION_END
#endif
!
!=======================================================================
! END OF THE LOOP OF THE SUB-ITERATIONS
!
      ENDDO ! ISOUSI
!
!=======================================================================
!
! : 5                 DIFFUSION OF THE TRACER
!
!=======================================================================
!
      IF(NTRAC.GT.0.AND.EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!
      IF(ENTET) CALL ENTETE(5,AT,LT)
!
      DO ITRAC=1,NTRAC
!
!       BOUNDARY CONDITIONS FOR THE DIFFUSION OF THE TRACER.
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DIFFCL POUR ITRAC=',ITRAC
        CALL DIFFCL(LITBOR%ADR(ITRAC)%P%I,
     &            TTILD%ADR(ITRAC)%P%R,TBOR%ADR(ITRAC)%P%R,
     &            MESH%NBOR%I,ICONVFT(ITRAC),NPOIN,NPTFR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DIFFCL'
!
      ENDDO
!
!  SOURCE TERMS FOR DIFFUSION - SOURCE TERMS OF THE TRACER
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DIFSOU'
      CALL DIFSOU(TEXP,TIMP,YASMI,TSCEXP,HPROP,TN,TETAT,NREJET,
     &            ISCE,DSCE2,TSCE2,MAXSCE,MAXTRA,AT,DT,MASSOU,NTRAC,
     &            MESH%IFAC%I,NSIPH,ENTSIP%I,SORSIP%I,DSIP%R,TSIP,
     &            NBUSE,ENTBUS%I,SORBUS%I,DBUS%R,TBUS,NWEIRS,TYPSEUIL,
     &            N_NGHB_W_NODES,NDGA1,NDGB1,TWEIRA,TWEIRB)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DIFSOU'
!
!=======================================================================
!     OIL SPILL MODEL (UNDER DEVELOPMENT IN MYGRHYCAR PROJECT)
!=======================================================================
!
      IF(SPILL_MODEL) THEN
!
        CALL OIL_SPILL_2D
!
      ENDIF
!
!=======================================================================
!     ADVECTION-DIFFUSION OF TRACERS
!=======================================================================
!
      DO ITRAC=1,NTRAC
!
!  CALLS THE STANDARD DIFFUSER. (CV1 IS THE SECOND MEMBER)
!
      INFOGT=INFOGR.AND.ENTET
!     HTILD: WORKING ARRAY WHERE HPROP IS RE-COMPUTED
!             (SAME ARRAY STRUCTURE)
!
!     LIMTRA REPLACED BY A COPY (IT MAY BE CHANGED BY THE ADVECTION SCHEME)
!
      DO I=1,NPTFR
        IT1%I(I)=LIMTRA%I(I)
      ENDDO
!
      TTILD%ADR(ITRAC)%P%DIM1=NPOIN
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CVDFTR SOLSYS=',SOLSYS
      IF(SOLSYS.EQ.1) THEN
      CALL CVDFTR(T%ADR(ITRAC)%P,TTILD%ADR(ITRAC)%P,TN%ADR(ITRAC)%P,
     &            TSCEXP%ADR(ITRAC)%P,
     &            DIFT,ICONVFT(ITRAC),CONVV(3),H,HN,HPROP,TETAC,
     &            UCONV,VCONV,DM1,ZCONV,SOLSYS,
     &            VISCT%ADR(ITRAC)%P,VISC_S,TEXP%ADR(ITRAC)%P,SMH,YASMH,
     &            TIMP%ADR(ITRAC)%P,YASMI(ITRAC),AM1,AM2,ZF,
     &            TBOR%ADR(ITRAC)%P,ATBOR%ADR(ITRAC)%P,
     &            BTBOR%ADR(ITRAC)%P,IT1,MASKTR,MESH,W1,TB,
     &            T1,T2,T3,T4,T5,T6,T7,T10,TE1,TE2,TE3,
     &            KDIR,KDDL,KENT,
     &            DT,ENTET,TETAT,AGGLOT,INFOGT,BILMAS,OPTADV_TR(ITRAC),
     &            ISOUSI,LT,NIT,OPDTRA,OPTBAN,
     &            MSK,MASKEL,MASKPT,MBOR,S,MASSOU(ITRAC),
     &            OPTSOU,SLVTRA(ITRAC),FLBOR,VOLU2D,V2DPAR,UNSV2D,
     &            2,FLBORTRA,FLULIM,YAFLULIM,FLULIMEBE,YAFLULIMEBE,
     &            DIRFLU,RAIN,PLUIE,TRAIN(ITRAC),
     &            FLODEL,.FALSE.,MAXADV,TB2,NCO_DIST,NSP_DIST)
!
      ELSE
      CALL CVDFTR(T%ADR(ITRAC)%P,TTILD%ADR(ITRAC)%P,TN%ADR(ITRAC)%P,
     &            TSCEXP%ADR(ITRAC)%P,
     &            DIFT,ICONVFT(ITRAC),CONVV(3),H,HN,HPROP,TETAC,
     &            UDEL,VDEL,DM1,ZCONV,SOLSYS,
     &            VISCT%ADR(ITRAC)%P,VISC_S,TEXP%ADR(ITRAC)%P,SMH,YASMH,
     &            TIMP%ADR(ITRAC)%P,YASMI(ITRAC),AM1,AM2,ZF,
     &            TBOR%ADR(ITRAC)%P,ATBOR%ADR(ITRAC)%P,
     &            BTBOR%ADR(ITRAC)%P,IT1,MASKTR,MESH,W1,TB,
     &            T1,T2,T3,T4,T5,T6,T7,T10,TE1,TE2,TE3,
     &            KDIR,KDDL,KENT,
     &            DT,ENTET,TETAT,AGGLOT,INFOGT,BILMAS,OPTADV_TR(ITRAC),
     &            ISOUSI,LT,NIT,OPDTRA,OPTBAN,
     &            MSK,MASKEL,MASKPT,MBOR,S,MASSOU(ITRAC),
     &            OPTSOU,SLVTRA(ITRAC),FLBOR,VOLU2D,V2DPAR,UNSV2D,
     &            2,FLBORTRA,FLULIM,YAFLULIM,FLULIMEBE,YAFLULIMEBE,
     &            DIRFLU,RAIN,PLUIE,TRAIN(ITRAC),
     &            FLODEL,YAFLODEL,MAXADV,TB2,NCO_DIST,NSP_DIST)
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CVDFTR'
!
      IF(BILMAS) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BILANT'
      CALL BILANT(H,T2,T3,DT,LT,TOTAL_ITER,ENTET,
     &            T%ADR(ITRAC)%P,AGGLOT,MASSOU(ITRAC),MASTR0(ITRAC),
     &            MASTR2(ITRAC),MASTEN(ITRAC),
     &            MASTOU(ITRAC),MSK,MASKEL,MESH,FLBOR,NUMLIQ%I,
     &            NFRLIQ,NPTFR,NAMETRAC(ITRAC),FLBORTRA,MASS_RAIN,
     &            TRAIN(ITRAC),MASTRAIN(ITRAC))
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BILANT'
!
      ENDDO
!
      ENDIF
!
!=======================================================================
!                    END OF DIFFUSION OF THE TRACER
!=======================================================================
!
!
!
!
!=======================================================================
!           DIFFUSION AND SOURCE TERMS FOR K-EPSILON MODEL OR SA MODELS
!=======================================================================
!
      IF(ITURB.EQ.3.AND..NOT.ADJO) THEN
!
        IF (ENTET) CALL ENTETE(4,AT,LT)
!
! BEWARE THE MATRIX STRUCTURE (SYMMETRICAL OR NOT)
! WHEN CONSIDERING THE COUPLED SYSTEM K-E
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING KEPSIL'
        CALL KEPSIL(AK,EP,AKTILD,EPTILD,AKN,EPN,VISC,CF,U,V,H,
     &              UCONV,VCONV,KBOR,EBOR,LIMKEP%I,IELMK,IELME,
     &              CV1,CV2,TM1,BM1,BM2,CM2,TE1,TE2,NPTFR,DT,MESH,
     &              T1,T2,T3,TB,CMU,C1,C2,SIGMAK,SIGMAE,ESTAR,SCHMIT,
     &              KMIN,KMAX,EMIN,EMAX,INFOKE.AND.ENTET,MSK,MASKEL,
     &              MASKPT,S,SLVK,SLVEP,ICONVF(4),YASMH,
     &              YAFLULIM,FLULIM,YAFLULIMEBE,FLULIMEBE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM KEPSIL'
!
      ELSEIF(ITURB.EQ.6.AND..NOT.ADJO) THEN
!
        IF (ENTET) CALL ENTETE(14,AT,LT)
!
        IF (DEBUG.GT.0) WRITE(LU,*) 'CALLING SPALART_ALLMARAS'
        CALL SPALART_ALLMARAS(U, V, VISCSA, DT, NUN, NUTILD, PROPNU,
     &                        VISC,IELMNU,SLVNU,MESH%DISBOR,
     &                        INFONU.AND.ENTET,
     &                        MSK,MASKEL,MASKPT,NPTFR,LIMSA%I,
     &                        NUBOR,S,UCONV,VCONV,ICONVF(4),
     &                        BM1,TM1,CV1,CM2,T3,T1,T2,MESH,TB,
     &                        T4,T5,WDIST,NUMIN,NUMAX,
     &                        YAFLULIM,FLULIM,YAFLULIMEBE,FLULIMEBE,
     &                        TE1,TE2,YASMH)
        IF(DEBUG.GT.0) WRITE(LU, *) 'BACK FROM SPALART_ALLMARAS'
      ENDIF
!
!=======================================================================
!  1)                 CHECKS MASS BALANCE
!=======================================================================
!
! CONTROL SECTIONS
!
      IF(NCP.NE.0.AND.(ENTET.OR.CUMFLO)) THEN
        CALL FLUSEC_TELEMAC2D(U,V,H,MESH%IKLE%I,MESH%XEL%R,MESH%YEL%R,
     &                        MESH%NELMAX,MESH%NELEM,
     &                        MESH%X%R,MESH%Y%R,DT,NCP,
     &                        CTRLSC,ENTET,AT,
     &                        MSKSEC,BM1,BM2,T1,HPROP,MESH,S,CV1,
     &                        MESH%IFABOR%I,COMFLU,CUMFLO)
      ENDIF
!
! MASS BALANCE
!
      IF(BILMAS) THEN
!
        CALL BILAN(MESH,H,T1,MASK,AT,DT,LT,TOTAL_ITER,ENTET,
     &             MASSES,MSK,MASKEL,EQUA,TE5,OPTBAN,
     &             MESH%NPTFR,FLBOR,
     &             FLUX_BOUNDARIES,NUMLIQ%I,NFRLIQ,GAMMA)
!
!       ADDED FOR THE KINETIC SCHEMES (TO BE CHECKED)
!
        IF(NTRAC.GT.0) THEN
          IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
!
            DO ITRAC=1,NTRAC
            CALL BILANT1(HSTOK,UCONV,VCONV,HPROP,T2,T3,T4,T5,T6,
     &                   DT,LT,TOTAL_ITER,ENTET,MASKTR,
     &                   T%ADR(1)%P,TN%ADR(1)%P,TETAT,
     &                   MASSOU(ITRAC),MSK,MASKEL,MESH,
     &                   FLUTSOR(ITRAC),FLUTENT(ITRAC),EQUA,LT2,ITRAC)
            ENDDO
!
          ENDIF
        ENDIF
!
      ENDIF
!
!=======================================================================
!                           DROGUE(S)
!=======================================================================
!
      IF(NFLOT_MAX.NE.0.AND..NOT.SPILL_MODEL) THEN
!
        IF(ENTET) CALL ENTETE(12,AT,LT)
!
        IF(SPHERI) THEN
          CALL OS('X=Y/Z   ',UCONV,UCONV,MESH%COSLAT,C)
          CALL OS('X=Y/Z   ',VCONV,VCONV,MESH%COSLAT,C)
        ENDIF
!
!       ADDING AND REMOVING DROGUES
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FLOT'
        CALL FLOT(XFLOT%R,YFLOT%R,NFLOT,NFLOT_MAX,MESH%X%R,MESH%Y%R,
     &            MESH%IKLE%I,NELEM,NELMAX,NPOIN,TAGFLO%I,
     &            SHPFLO%R,ELTFLO%I,MESH,LT,NIT,AT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FLOT'
!
!       MOVING THEM
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DERIVE'
        CALL DERIVE(UCONV%R,VCONV%R,VCONV%R,DT,AT,
     &              MESH%X%R,MESH%Y%R,MESH%Y%R,
     &              MESH%IKLE%I,MESH%IFABOR%I,LT,IELM,UCONV%ELM,3,NPOIN,
     &              NPOIN,
     &              NELEM,NELMAX,MESH%SURDET%R,XFLOT%R,YFLOT%R,YFLOT%R,
     &              SHPFLO%R,SHPFLO%R,TAGFLO%I,ELTFLO%I,ELTFLO%I,
     &              NFLOT,NFLOT_MAX,FLOPRD,MESH,T2D_FILES(T2DFLO)%LU,
     &              IT1%I,T1%R,T2%R,T2%R,IT2%I,W1%R,W1%R,NPOIN,STOCHA,
     &              VISC,1,SVOID,0,
     &              AALGAE=ALGAE,DALGAE=DALGAE,RALGAE=RALGAE,
     &              EALGAE=EALGAE,ALGTYP=ALGTYP,AK=AK%R,EP=EP%R,H=H%R)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DERIVE'
!
        IF(SPHERI) THEN
          CALL OS('X=XY    ',UCONV,MESH%COSLAT,S,C)
          CALL OS('X=XY    ',VCONV,MESH%COSLAT,S,C)
        ENDIF
!
      ENDIF
!
!=======================================================================
!                        LAGRANGIAN DRIFT(S)
!=======================================================================
!
      IF(NLAG.NE.0) THEN
!
        IF (ENTET) CALL ENTETE(13,AT,LT)
!
          CALL DERLAG(UCONV%R,VCONV%R,DT,MESH%X%R,MESH%Y%R,
     &                LT,IELM,UCONV%ELM,3,NPOIN,NELEM,NELMAX,
     &                XLAG%R,YLAG%R,T1%R,T2%R,IT1%I,SHPLAG%R,
     &                DEBLAG%I,FINLAG%I,ELTLAG%I,NLAG,
     &                T7%R,T8%R,IT2%I,MESH)
!
      ENDIF
!
!=======================================================================
!                     CREDIBILITY CHECKS
!                   LOOKS FOR A STEADY STATE
!=======================================================================
!
      ARRET1=.FALSE.
      IF(VERLIM) THEN
        CALL ISITOK(H%R,H%DIM1,U%R,U%DIM1,V%R,V%DIM1,NTRAC,
     &              T,T%ADR(1)%P%DIM1,
     &              MESH%X%R,MESH%Y%R,BORNES,ARRET1)
!       CORRECTION SUGGESTED BY NOEMIE DURAND (CHC-NRC) 04/01/2006
        IF(NCSIZE.GT.1) THEN
          STOP2=0
          IF(ARRET1) STOP2=1
          STOP2=P_IMAX(STOP2)
          IF(STOP2.EQ.1) ARRET1=.TRUE.
        ENDIF
      ENDIF
      ARRET2=.FALSE.
      IF(STOPER) THEN
        CALL STEADY(H%R,HN%R,H%DIM1,U%R,UN%R,U%DIM1,V%R,VN%R,
     &              V%DIM1,NTRAC,T,TN,T%ADR(1)%P%DIM1,
     &              CRIPER,ARRET2)
!       CORRECTION BY NOEMIE DURAND (CHC-NRC) 04/01/2006
        IF(NCSIZE.GT.1) THEN
          STOP2=0
          IF(ARRET2) STOP2=1
          STOP2=P_IMIN(STOP2)
          ARRET2=.NOT.(STOP2.EQ.0)
        ENDIF
      ENDIF
      IF(ARRET1.OR.ARRET2) THEN
        LEOPRD=1
        LISPRD=1
      ENDIF
!
      ARRET3=.FALSE.
      CALL TRAPSIG()
      IF(BREAKER) ARRET3=.TRUE.
!
      IF(ARRET1.OR.ARRET2.OR.ARRET3) THEN
        LEOPRD=1
        LISPRD=1
      ENDIF
!
! FH-BMD
!=============================================
!     FOR NEW COUPLING
      ENDDO ! ISIS_CFD
      IF (SIS_CPL%SISYPHE_CFD) SIS_CPL%CONSTFLOW = .TRUE.
999   CONTINUE
!
!=======================================================================
!
!     COUPLING WITH SISYPHE
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
        CALL CONFIG_CODE(2)
!
        SUSP1=SIS_CPL%SUSP.AND.PERCOU.EQ.1
!
!=======================================================================
!       LEO IS COMMON FOR THE OUTPUT OF SISYPHE AND T2D
        CALL PRERES_TELEMAC2D()
!       KEEP THE SAME GRAPHIC COUNTER FOR T2D ANS SIS
        IF(LEO.AND.EQUA(1:15).NE.'SAINT-VENANT VF')COMPLEO=COMPLEO-1
!
!       MANAGE COUPLING PERIOD CALLS
!
        YASIS = .FALSE.
        YASIS = PERCOU*(LT/PERCOU).EQ.LT
!
        IF(SUSP1.OR.(SIS_CPL%CHARR.AND.YASIS))THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING SISYPHE, CHARRIAGE'
          CALL SISYPHE(1,LT,LEOPRD_CHARR,LISPRD,NIT,U,V,H,HN,HPROP,ZF,
     &                 CF,CF,CHESTR,SIS_CPL%CONSTFLOW,SIS_CPL%NSIS_CFD,
     &                 SIS_CPL%SISYPHE_CFD,CODE1,
     &                 PERCOU,U,V,AT,VISC,DT*PERCOU,SIS_CPL%CHARR,SUSP1,
     &                 FLBOR,SOLSYS,DM1,USIS,VSIS,ZCONV,
     &                 DIRMOY,HM0,TPR5,ORBVEL,LEO,GRCOMP=COMPLEO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'FIN APPEL SISYPHE, CHARRIAGE'
!
        ENDIF
!
        IF(SIS_CPL%SUSP.AND.PERCOU.NE.1) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING SISYPHE, SUSPENSION'
          CALL SISYPHE(1,LT,LEOPRD,LISPRD,NIT,U,V,H,HN,HPROP,ZF,
     &                 CF,CF,CHESTR,SIS_CPL%CONSTFLOW,SIS_CPL%NSIS_CFD,
     &                 SIS_CPL%SISYPHE_CFD,
     &                 CODE1,1,U,V,AT,VISC,DT,CHARR_TEL,SIS_CPL%SUSP,
     &                 FLBOR,SOLSYS,DM1,USIS,VSIS,ZCONV,
     &                 DIRMOY,HM0,TPR5,ORBVEL,LEO,GRCOMP=COMPLEO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'FIN APPEL DE SISYPHE, SUSPENSION'
!
        ENDIF
!
        CALL CONFIG_CODE(1)
!
        ENDIF
!
!=======================================================================
!                      WRITES OUT THE RESULTS
!=======================================================================

      IF(ADJO) THEN
!
        IF(T2D_FILES(T2DRBI)%NAME.NE.' '.AND.
     &     INCLU2(ESTIME,'DEBUG')) THEN
          CALL BIEF_DESIMP('SERAFIN?',VARSORA,
     &                     NPOIN,T2D_FILES(T2DRBI)%LU,
     &                     'STD',-AT,LT,LISPRD,1,
     &                     SORLEOA,SORIMPA,MAXVAR,TEXTE,PTINIG,PTINIL)
        ENDIF
!
      ELSE
!
        IF(CODE(1:7).EQ.'ESTEL3D') THEN
!
!         SAVES THE DEPTH FOR ESTEL3D
          CALL DEPTH_FILL(H%R)
!
! (NOTE THAT OUTPUTS ARE DONE WITHIN ESTEL3D IN COUPLED MODE)
!
        ELSE
          ! Keeping in memory the value of leoprd as it will be
          ! set to 1 by preres_telemac2d on the last time step
          OLD_LEOPRD = LEOPRD
!
          CALL PRERES_TELEMAC2D()
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_DESIMP',LEO
          CALL BIEF_DESIMP(T2D_FILES(T2DRES)%FMT,VARSOR,
     &            NPOIN,T2D_FILES(T2DRES)%LU,'STD',AT,LT,
     &            LISPRD,OLD_LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,PTINIG,PTINIL,
     &            IIMP=IMP,ILEO=LEO,COMPGRAPH=COMPLEO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BIEF_DESIMP',LEO
        ENDIF
!
!
        IF(INCLUS(COUPLING,'DELWAQ')) THEN
!
!         T3 : MODIFIED DEPTH TO TAKE INTO ACCOUNT MASS-LUMPING
!              IN THE CONTINUITY EQUATION
          IF(ABS(1.D0-AGGLOC).GT.1.D-8) THEN
            CALL VECTOR(T3,'=','MASVEC          ',IELMH,
     &                  1.D0-AGGLOC,H ,S,S,S,S,S,MESH,MSK,MASKEL)
            IF(NCSIZE.GT.1) CALL PARCOM(T3,2,MESH)
            CALL OS('X=XY    ',X=T3 ,Y=UNSV2D)
            CALL OS('X=X+CY  ',X=T3 ,Y=H ,C=AGGLOC)
          ELSE
            CALL OS('X=Y     ',X=T3 ,Y=H )
          ENDIF
!
!         NOTE: FLODEL IS DONE IN CORRECTION_DEPTH_2D
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING TEL4DELWAQ'
          CALL TEL4DELWAQ(MESH%NPOIN,MESH%NPOIN,MESH%NELEM,MESH%NSEG,
     &                 MESH%IKLE%I,MESH%ELTSEG%I,MESH%GLOSEG%I,
     &                 MESH%ORISEG%I,MESH%GLOSEG%DIM1,
     &                 MESH%X%R,MESH%Y%R,MESH%NPTFR,LIHBOR%I,
     &                 MESH%NBOR%I,1,AT,DT,LT,NIT,T3%R,HPROP%R,T3%R,
     &                 UDEL%R,VDEL%R,T%ADR(MAX(IND_S,1))%P%R,
     &                 T%ADR(MAX(IND_T,1))%P%R,
     &                 VISC%R,TITCAS,T2D_FILES(T2DGEO)%NAME,
     &                 T2D_FILES(T2DCLI)%NAME,WAQPRD,
     &                 T2DDL1,T2DDL2,T2DDL3,T2DDL5,T2DDL6,T2DDL7,
     &                 T2DL11,T2DDL4,T2DDL8,T2DDL9,T2DL10,
     &                 ENTET,NELEM,SALI_DEL,
     &                 TEMP_DEL,VELO_DEL,DIFF_DEL,
     &                 MARDAT,MARTIM,FLODEL%R,
     &                 V2DPAR%R,MESH%KNOLG%I,T2D_FILES)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM TEL4DELWAQ'
!
        ENDIF
!
      ENDIF  !(ADJO)
!
!     OPTIONAL USER OUTPUT (COURTESY JACEK JANKOWSKI, BAW)
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING UTIMP_TELEMAC2D'
      CALL UTIMP_TELEMAC2D(LT,AT,PTINIG,LEOPRD,PTINIL,LISPRD)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM UTIMP_TELEMAC2D'
!
!=======================================================================
!              COMPARISON AGAINST A REFERENCE FILE
!=======================================================================
!
!     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
!     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
!     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
!
      IF(VALID) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_VALIDA'
        CALL BIEF_VALIDA(TB,TEXTPR,
     &                   T2D_FILES(T2DREF)%LU,T2D_FILES(T2DREF)%FMT,
     &                   VARSOR,TEXTE,
     &                   T2D_FILES(T2DRES)%LU,T2D_FILES(T2DRES)%FMT,
     &                   MAXVAR,NPOIN,LT,TOTAL_ITER,ALIRE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BIEF_VALIDA'
      ENDIF


!=======================================================================
!              REDEFINING THE ALGAE VARIABLES FOR THE NEXT
!                            TIME STEP
!=======================================================================
!
! DAJ
! UPDATE THE ALGAE VARIABLES AT T_0 FOR THE CALCULATIONS OF THE NEXT TIME STEP
      IF(ALGAE) THEN
        CALL OS('X=Y     ',X=U_X_AV_0,Y=U_X_AV)
        CALL OS('X=Y     ',X=U_Y_AV_0,Y=U_Y_AV)
        CALL OS('X=Y     ',X=U_Z_AV_0,Y=U_Z_AV)
        CALL OS('X=Y     ',X=K_AV_0,Y=K_AV)
        CALL OS('X=Y     ',X=EPS_AV_0,Y=EPS_AV)
        CALL OS('X=Y     ',X=U_X_0,Y=U_X)
        CALL OS('X=Y     ',X=U_Y_0,Y=U_Y)
        CALL OS('X=Y     ',X=U_Z_0,Y=U_Z)
        CALL OS('X=Y     ',X=V_X_0,Y=V_X)
        CALL OS('X=Y     ',X=V_Y_0,Y=V_Y)
        CALL OS('X=Y     ',X=V_Z_0,Y=V_Z)
      ENDIF
! FAJ
!
!=======================================================================
!
!  NEAT (PROGRAMMED) STOP OF THE MODEL:
!
      IF(ARRET1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'VALEURS LIMITES DEPASSEES, ARRET DE TELEMAC-2D'
          WRITE(LU,*)
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'LIMIT VALUES TRESPASSED, TELEMAC-2D IS STOPPED'
          WRITE(LU,*)
        ENDIF
        RETURN
      ENDIF
      IF(ARRET2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'ETAT PERMANENT ATTEINT, ARRET DE TELEMAC-2D'
          WRITE(LU,*)
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'STEADY STATE REACHED, TELEMAC-2D IS STOPPED'
          WRITE(LU,*)
        ENDIF
        RETURN
      ENDIF
      IF(ARRET3) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          CALL ENTETE(1,AT,LT)
          WRITE(LU,*) 'TELEMAC-2D ARRETE PAR L''UTILISATEUR'
          WRITE(LU,*) 'AVEC SIGNAL ',SIGUSR1
          WRITE(LU,*)
        ENDIF
        IF(LNG.EQ.2) THEN
          CALL ENTETE(1,AT,LT)
          WRITE(LU,*)
          WRITE(LU,*) 'TELEMAC-2D CHECKPOINTED BY THE USER'
          WRITE(LU,*) 'USING SIGNAL ',SIGUSR1
          WRITE(LU,*)
        ENDIF
        RETURN
      ENDIF
!
!     NOW ADVECTION SCHEME WILL BE CHANGED AND FLULIM
!     WILL NOT CORRESPOND TO IT.
!
      YAFLULIM=.FALSE.
      YAFLULIMEBE=.FALSE.
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_TIMESTEP_END
#endif
!
! 700: TIME LOOP
!
      IF(LT.LT.NIT) GO TO 700
!
!=======================================================================
!
! :                 /* END OF THE LOOP IN TIME */
!
!=======================================================================
!
      IF(PASS.NE.1) THEN
        IF(LNG.EQ.1.AND.LISTIN) WRITE(LU,18)
        IF(LNG.EQ.2.AND.LISTIN) WRITE(LU,19)
18      FORMAT(/,1X,'FIN DE LA BOUCLE EN TEMPS',////)
19      FORMAT(/,1X,'END OF TIME LOOP',////)
      ENDIF
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_END
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END


!                    ***************************
                     SUBROUTINE PRERES_TELEMAC2D
!                    ***************************
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  C. GOEURY (EDF R&D LNHE)
!+        25/07/2013
!+        V6P3
!+   Sum of HAP in oilspills has been added.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        02/01/2014
!+        V7P0
!+   Securing bound checking in parallelism.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        28/10/2014
!+        V7P0
!+   Initialising Lagrangian drifts for iteration 0 in case they are
!+   in outputs.
!
!history  R. ATA & J-M HERVOUET (EDF LAB, LNHE)
!+        10/06/2015
!+        V7P1
!+   Now all the variables asked for graphic printouts are written for
!+   remarkable points.
!
!history  R. ATA (EDF LAB, LNHE)
!+        11/01/2016
!+        V7P2
!+   Now preres gives instruction to bief_desimp to write graphical
!+   results (through leo and imp)
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+        20/07/2016
!+        V7P2
!+   When Elder model of turbulence is asked, the longitudinal
!+   dispersion is retrieved from NUXX and NUYY, and KL+PROPNU put in
!+   T10.
!
!history S.E.BOURBAN (HRW)
!+        11/11/2016
!+        V7P2
!+   Adding the DIFFERENTIATORS to the list of updated variables for
!+   for printing purposes.
!+   Updating function calls within WRITE statements for NAG compliancy.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DMAX,P_DMIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LTT,N,IMAX,I,J
!
      DOUBLE PRECISION HHH,HIH,HAH,XMAX
      DOUBLE PRECISION, PARAMETER :: EPSS=1.E-10
      DOUBLE PRECISION GPRDTIME,LPRDTIME,RESTE
!
      INTRINSIC MAX,SQRT,CEILING
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN BIEF_DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
!      LEO=.FALSE.
!     THIS WILL TRIGGER THE OUTPUT OF LAST TIMESTEP
!     BUT NOT WITH PARAMETER ESTIMATION (LISPRD WOULD STAY AT 1
!     FOR FURTHER COMPUTATIONS)
!      IF(LT.EQ.NIT.AND.ESTIME(1:1).EQ.' ') THEN
!        IMP=.FALSE.
!        LEO=.FALSE.
!      ENDIF
!     Always write the initial conditions
      IF(LT.EQ.0) THEN
        IMP=.TRUE.
        LEO=.TRUE.
        COMPLEO=0
        COMPLIS=0
      ELSE
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!         FEM
          LTT=(LT/LISPRD)*LISPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
          LTT=(LT/LEOPRD)*LEOPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
          IF(ABS(AT-(DUREE+AT0)).LT.1.E-14)LEO=.TRUE.
!         FOR GRAPHICAL OUTPUTS
          IF(LEO)COMPLEO=COMPLEO+1
        ELSE
!         FVM
          IF(LT.GT.PTINIL)THEN
!           LISTING OUTPUT
            LPRDTIME=LISPRD*DTINI
            LTT=CEILING(AT/LPRDTIME)
            RESTE=(LTT*LPRDTIME-AT)/LPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              IMP=.TRUE.
              COMPLIS=COMPLIS+1
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
! 1)  PART WHICH MUST BE DONE EVEN IF THERE IS NO OUTPUT FOR THIS TIMESTEP
!     BUT ONLY AFTER FIRST TIMESTEP FOR GRAPHIC PRINTOUTS
!
!-----------------------------------------------------------------------
!
      IF(LT.GE.PTINIG) THEN
!
!=======================================================================
! COMPUTES THE MAXIMUM ELEVATION AND ASSOCIATED TIME
!=======================================================================
!
      IF(SORLEO(27).OR.SORIMP(27)) THEN
        IF(.NOT.DEJA1_PRERES) THEN
          CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
          CALL OS('X=C     ',X=TMAXZ,C=AT)
          DEJA1_PRERES=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=H%R(N)+ZF%R(N)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXZ%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXZ%R(N)=XMAX
              IF(SORLEO(28).OR.SORIMP(28)) TMAXZ%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! COMPUTES THE MAXIMUM SPEED AND ASSOCIATED TIME
!=======================================================================
!
      IF(SORLEO(29).OR.SORIMP(29)) THEN
        IF(.NOT.DEJA2_PRERES) THEN
          CALL OS('X=C     ',X=MAXV ,C=0.D0)
          CALL OS('X=C     ',X=TMAXV,C=AT)
          DEJA2_PRERES=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=SQRT(U%R(N)**2+V%R(N)**2)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXV%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXV%R(N)=XMAX
              IF(SORLEO(30).OR.SORIMP(30)) TMAXV%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!     CASE WHERE OUTINI=.TRUE. : PRIORITY ON PTINIG, VALUES FOR LT=0
!     OTHERWISE THEY WOULD NOT BE INITIALISED
        IF(SORLEO(27).OR.SORIMP(27)) CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
        IF(SORLEO(28).OR.SORIMP(28)) CALL OS('X=C     ',X=TMAXZ,C=AT)
        IF(SORLEO(29).OR.SORIMP(29)) CALL OS('X=C     ',X=MAXV ,C=0.D0)
        IF(SORLEO(30).OR.SORIMP(30)) CALL OS('X=C     ',X=TMAXV,C=AT)
!
!     ENDIF FOR : IF(LT.GE.PTINIG) THEN
      ENDIF
!
!-----------------------------------------------------------------------
!
! 2)  PART WHICH MUST BE DONE ONLY IF THERE IS AN OUTPUT FOR THIS TIMESTEP
!
!-----------------------------------------------------------------------
!
!     NO PRINTOUT REQUIRED (LISTING OR RESULT FILE): EXITS
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
!
!
!=======================================================================
! COMPUTES CELERITY (IN FU, SEE BLOCK: VARSOR)
!=======================================================================
!
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        CALL CPSTVC(ZF,FU)
        DO N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FREE SURFACE ELEVATION (= H + ZF, IN FV)
!=======================================================================
!
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL CPSTVC(ZF,FV)
        DO N=1,NPOIN
          FV%R(N) = H%R(N)+ZF%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FROUDE NUMBER
!=======================================================================
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL CPSTVC(ZF,T2)
        DO N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE
!=======================================================================
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        CALL CPSTVC(ZF,T3)
        DO N=1,NPOIN
          T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! RETRIEVING LONGITUDINAL DISPERSION
!=======================================================================
!
      IF(((LEO.AND.SORLEO(12)).OR.(IMP.AND.SORIMP(12)))
     &   .AND.ITURB.EQ.2) THEN
        DO N=1,NPOIN
!         RETRIEVING KL (SEE SUBROUTINE DISPER) AND ADDING PROPNU.
          T10%R(N) = (VISC%R(N)+VISC%R(N+NPOIN)-2*PROPNU)*ELDER(1)/
     &              (ELDER(1)+ELDER(2)) + PROPNU
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE ALONG X
!=======================================================================
!
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL CPSTVC(ZF,T4)
        DO N=1,NPOIN
          T4%R(N)=H%R(N)*U%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE ALONG Y
!=======================================================================
!
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL CPSTVC(ZF,T5)
        DO N=1,NPOIN
          T5%R(N)=H%R(N)*V%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES SPEED
!=======================================================================
!
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , X=T6 , Y=U , Z=V )
      ENDIF
!
!=======================================================================
! LAGRANGIAN DRIFTS
!=======================================================================
!
      IF((LEO.AND.SORLEO(20)).OR.(IMP.AND.SORIMP(20))) THEN
        IF(LT.EQ.0) CALL OS('X=0     ',X=T7)
      ENDIF
      IF((LEO.AND.SORLEO(21)).OR.(IMP.AND.SORIMP(21))) THEN
        IF(LT.EQ.0) CALL OS('X=0     ',X=T8)
      ENDIF
!
!=======================================================================
! COMPUTES COURANT NUMBER
!=======================================================================
!
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
!                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF(NCSIZE.GT.1) XMAX = P_DMAX(XMAX)
        IF(LNG.EQ.1) WRITE(LU,78) XMAX
        IF(LNG.EQ.2) WRITE(LU,79) XMAX
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
!
!=======================================================================
! COMPUTES FRICTION SPEED
!=======================================================================
!
      IF((LEO.AND.SORLEO(31)).OR.(IMP.AND.SORIMP(31))) THEN
        CALL CPSTVC(CF,T7)
        DO N=1,NPOIN
          T7%R(N) = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2))
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES THE Q go through Culverts
!=======================================================================
!
      IF((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23)))  THEN
        DO I=1,NBUSE
           PRIVE%ADR(1)%P%R(ENTBUS%I(I))=-DBUS%R(I)
           PRIVE%ADR(1)%P%R(SORBUS%I(I))= DBUS%R(I)
        ENDDO
      ENDIF
!
!=======================================================================
! UPDATE THE POINTERS TO THE DIFFERENTIATED VARIABLES
!=======================================================================
!
!     TODO: TRY NOT USING THE HARDCODED NUMBER 34
!
      J = 34+1+NTRAC+2*NPERIAF+VARCL%N
      DO I = 1,NADVAR
        IF((LEO.AND.SORLEO(J)).OR.(IMP.AND.SORIMP(J))) THEN
          CALL AD_GET_TELEMAC2D(I,ADVAR%ADR(I)%P)
          J = J + 1
        ENDIF
      ENDDO
!
!=======================================================================
!
1000  CONTINUE
!
!=======================================================================
! HARMONIC ANALYSIS USING LEAST MEAN ERROR SQUARE METHOD
!=======================================================================
!
      IF(NPERIAF.GT.0) CALL SPECTRE
!
!=======================================================================
! PRINTOUTS FOR THE REMARKABLE POINTS
!=======================================================================
!
      IF(LT.EQ.NIT.AND.NPTS.GT.0) THEN
        DO I=1,MAXVAR
!         BEWARE : HERE SORLEO IS USED INSTEAD OF SORIMP
          IF(SORLEO(I)) THEN
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) TEXTE(I)(1:16)
            WRITE(LU,*) ' '
            DO N=1,NPTS
!             IN PARALLEL POINT DOES NOT ALWAYS EXIST, MAYBE ELSEWHERE
              IF(NCSIZE.GT.1) THEN
                HHH = 0.D0
                IF(LIST_PTS(N).GT.0) HHH=VARSOR%ADR(I)%P%R(LIST_PTS(N))
                HIH = HHH
                HIH = P_DMIN(HIH)
                HAH = HHH
                HAH = P_DMAX(HAH)
                WRITE(LU,*) NAME_PTS(N),' : ',HIH+HAH
              ELSE
                WRITE(LU,*) NAME_PTS(N),' : ',
     &                                    VARSOR%ADR(I)%P%R(LIST_PTS(N))
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END


!                    ***************************
                     SUBROUTINE NOMVAR_TELEMAC2D
!                    ***************************
!
     &(TEXTE,TEXTPR,MNEMO,NPERIAF,NTRAC,NAMETRAC,N_NAMES_PRIV,
     & NAMES_PRIVE,SECCURRENTS,NADVAR,NAMES_ADVAR)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!+                FILES (IN TEXTE) AND FOR THE PREVIOUS COMPUTATION
!+                RESULTS FILE (IN TEXTPR).
!+
!+                TEXTE AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!+                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.
!
!history  J-M HERVOUET (LNHE)
!+        31/08/2007
!+        V5P8
!+   First version.
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Secondary flow correction: add variables
!+   tau_s, Omega/h and r^{-1} for visualization
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/07/2015
!+        V7P1
!+   Now taking into account names of private arrays given by user.
!
!history  S.E. BOURBAN (HRW)
!+        20/06/2016
!+        V7P2
!+   Now taking into account names of differentiators given by user.
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+        20/07/2016
!+        V7P2
!+   Elder model of turbulence is asked, the name of viscosity is
!+   replaced by longitudinal dispersion.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MNEMO          |<--| MNEMONIC FOR 'VARIABLES FOR GRAPHIC OUTPUTS'
!| N_NAMES_PRIV   |-->| NUMBER OF NAMES OF PRIVATE VARIABLES GIVEN
!| NAMES_PRIVE    |-->| NAME OF PRIVATE VARIABLES GIVEN BY USER
!| NAMETRAC       |-->| NAME OF TRACERS (GIVEN BY KEYWORDS)
!| NPERIAF        |-->| NUMBER OF PERIODS FOR FOURRIER ANALYSIS
!| NTRAC          |-->| NUMBER OF TRACERS
!| TEXTE          |<--| SEE ABOVE
!| TEXTPR         |<--| SEE ABOVE
!| SECCURRENTS    |-->| IF YES SECONDARY CURRENTS COMPUTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY : IND_SEC,ITURB
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPERIAF,NTRAC,N_NAMES_PRIV
      INTEGER, INTENT(IN)              :: NADVAR
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMETRAC(*),NAMES_PRIVE(4)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMES_ADVAR(*)
      LOGICAL, INTENT(IN)              :: SECCURRENTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=2) CHAR2
      INTEGER I,ILAST,INEXT
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF(LNG.EQ.2) THEN
!
      TEXTE (1 ) = 'VELOCITY U      M/S             '
      TEXTE (2 ) = 'VELOCITY V      M/S             '
      TEXTE (3 ) = 'CELERITY        M/S             '
      TEXTE (4 ) = 'WATER DEPTH     M               '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'FROUDE NUMBER                   '
      TEXTE (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTE (9 ) = 'EX TRACER                       '
      TEXTE (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTE (12) = 'LONG. DISPERSIONM2/S            '
      ELSE
        TEXTE (12) = 'VISCOSITY       M2/S            '
      ENDIF
      TEXTE (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTE (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTE (15) = 'SCALAR VELOCITY M/S             '
      TEXTE (16) = 'WIND ALONG X    M/S             '
      TEXTE (17) = 'WIND ALONG Y    M/S             '
      TEXTE (18) = 'AIR PRESSURE    PASCAL          '
      TEXTE (19) = 'BOTTOM FRICTION                 '
      TEXTE (20) = 'DRIFT ALONG X   M               '
      TEXTE (21) = 'DRIFT ALONG Y   M               '
      TEXTE (22) = 'COURANT NUMBER                  '
      TEXTE (23) = 'CULVERTS        M3/S            '
      TEXTE (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTE (25) = 'VARIABLE 25     UNIT   ??       '
      TEXTE (26) = 'VARIABLE 26     UNIT   ??       '
      TEXTE (27) = 'HIGH WATER MARK M               '
      TEXTE (28) = 'HIGH WATER TIME S               '
      TEXTE (29) = 'HIGHEST VELOCITYM/S             '
      TEXTE (30) = 'TIME OF HIGH VELS               '
      TEXTE (31) = 'FRICTION VEL.   M/S             '
      TEXTE (32) = 'TAU_S           NA              '
      TEXTE (33) = '1/R             1/M             '
      TEXTE (34) = 'WALLDIST        M               '
!
! TEXTPR IS USED TO READ PREVIOUS COMPUTATION FILES.
! IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
! FROM ANOTHER CODE WITH DIFFERENT VARIABLE NAMES, WHICH MUST
! BE GIVEN HERE:
!
      TEXTPR (1 ) = 'VELOCITY U      M/S             '
      TEXTPR (2 ) = 'VELOCITY V      M/S             '
      TEXTPR (3 ) = 'CELERITY        M/S             '
      TEXTPR (4 ) = 'WATER DEPTH     M               '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'FROUDE NUMBER                   '
      TEXTPR (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTPR (9 ) = 'EX TRACER                       '
      TEXTPR (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTPR(12) = 'LONG. DISPERSIONM2/S            '
      ELSE
        TEXTPR(12) = 'VISCOSITY       M2/S            '
      ENDIF
      TEXTPR (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTPR (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTPR (15) = 'SCALAR VELOCITY M/S             '
      TEXTPR (16) = 'WIND ALONG X    M/S             '
      TEXTPR (17) = 'WIND ALONG Y    M/S             '
      TEXTPR (18) = 'AIR PRESSURE    PASCAL          '
      TEXTPR (19) = 'BOTTOM FRICTION                 '
      TEXTPR (20) = 'DRIFT ALONG X   M               '
      TEXTPR (21) = 'DRIFT ALONG Y   M               '
      TEXTPR (22) = 'COURANT NUMBER                  '
      TEXTPR (23) = 'CULVERTS        M3/S            '
      TEXTPR (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTPR (25) = 'VARIABLE 25     UNIT   ??       '
      TEXTPR (26) = 'VARIABLE 26     UNIT   ??       '
      TEXTPR (27) = 'HIGH WATER MARK M               '
      TEXTPR (28) = 'HIGH WATER TIME S               '
      TEXTPR (29) = 'HIGHEST VELOCITYM/S             '
      TEXTPR (30) = 'TIME OF HIGH VELS               '
      TEXTPR (31) = 'FRICTION VEL.   M/S             '
      TEXTPR (32) = 'TAU_S           NA              '
      TEXTPR (33) = '1/R             1/M             '
      TEXTPR (34) = 'WALLDIST        M               '
!
!-----------------------------------------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
!
      TEXTE (1 ) = 'VITESSE U       M/S             '
      TEXTE (2 ) = 'VITESSE V       M/S             '
      TEXTE (3 ) = 'CELERITE        M/S             '
      TEXTE (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'FROUDE                          '
      TEXTE (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTE (9 ) = 'EX TRACEUR                      '
      TEXTE (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTE (12) = 'DISPERSION LONG.M2/S            '
      ELSE
        TEXTE (12) = 'VISCOSITE TURB. M2/S            '
      ENDIF
      TEXTE (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTE (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTE (15) = 'VITESSE SCALAIREM/S             '
      TEXTE (16) = 'VENT X          M/S             '
      TEXTE (17) = 'VENT Y          M/S             '
      TEXTE (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTE (19) = 'FROTTEMENT                      '
      TEXTE (20) = 'DERIVE EN X     M               '
      TEXTE (21) = 'DERIVE EN Y     M               '
      TEXTE (22) = 'NBRE DE COURANT                 '
      TEXTE (23) = 'CULVERTS        M3/S            '
      TEXTE (24) = 'VARIABLE 24     UNITES ??       '
      TEXTE (25) = 'VARIABLE 25     UNITES ??       '
      TEXTE (26) = 'VARIABLE 26     UNITES ??       '
      TEXTE (27) = 'COTE MAXIMUM    M               '
      TEXTE (28) = 'TEMPS COTE MAXI S               '
      TEXTE (29) = 'VITESSE MAXIMUM M/S             '
      TEXTE (30) = 'T VITESSE MAXI  S               '
      TEXTE (31) = 'VITESSE DE FROT.M/S             '
      TEXTE (32) = 'TAU_S           NA              '
      TEXTE (33) = '1/R             1/M             '
      TEXTE (34) = 'DIST PAROI      M               '
!
! TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
! A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
! DE CALCUL A PARTIR D'UN AUTRE CODE.
!
      TEXTPR (1 ) = 'VITESSE U       M/S             '
      TEXTPR (2 ) = 'VITESSE V       M/S             '
      TEXTPR (3 ) = 'CELERITE        M/S             '
      TEXTPR (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'FROUDE                          '
      TEXTPR (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTPR (9 ) = 'EX TRACEUR                      '
      TEXTPR (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTPR(12) = 'DISPERSION LONG.M2/S            '
      ELSE
        TEXTPR(12) = 'VISCOSITE TURB. M2/S            '
      ENDIF
      TEXTPR (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTPR (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTPR (15) = 'VITESSE SCALAIREM/S             '
      TEXTPR (16) = 'VENT X          M/S             '
      TEXTPR (17) = 'VENT Y          M/S             '
      TEXTPR (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTPR (19) = 'FROTTEMENT                      '
      TEXTPR (20) = 'DERIVE EN X     M               '
      TEXTPR (21) = 'DERIVE EN Y     M               '
      TEXTPR (22) = 'NBRE DE COURANT                 '
      TEXTPR (23) = 'CULVERTS        M3/S            '
      TEXTPR (24) = 'VARIABLE 24     UNITES ??       '
      TEXTPR (25) = 'VARIABLE 25     UNITES ??       '
      TEXTPR (26) = 'VARIABLE 26     UNITES ??       '
      TEXTPR (27) = 'COTE MAXIMUM    M               '
      TEXTPR (28) = 'TEMPS COTE MAXI S               '
      TEXTPR (29) = 'VITESSE MAXIMUM M/S             '
      TEXTPR (30) = 'T VITESSE MAXI  S               '
      TEXTPR (31) = 'VITESSE DE FROT.M/S             '
      TEXTPR (32) = 'TAU_S           NA              '
      TEXTPR (33) = '1/R             1/M             '
      TEXTPR (34) = 'DIST PAROI      M               '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ALIASES FOR THE VARIABLES IN THE STEERING FILE
!
!     UVCHSBFQTKEDIJMXYPWAGLNORZ
!     VELOCITY COMPONENT U
      MNEMO(1)   = 'U       '
!     VELOCITY COMPONENT V
      MNEMO(2)   = 'V       '
!     CELERITY
      MNEMO(3)   = 'C       '
!     WATER DEPTH
      MNEMO(4)   = 'H       '
!     FREE SURFACE ELEVATION
      MNEMO(5)   = 'S       '
!     BOTTOM ELEVATION
      MNEMO(6)   = 'B       '
!     FROUDE
      MNEMO(7)   = 'F       '
!     FLOW RATE
      MNEMO(8)   = 'Q       '
!     EX TRACER
      MNEMO(9)   = '?       '
!     TURBULENT ENERGY
      MNEMO(10)   = 'K       '
!     DISSIPATION
      MNEMO(11)   = 'E       '
!     TURBULENT VISCOSITY
      MNEMO(12)   = 'D       '
!     FLOWRATE ALONG X
      MNEMO(13)   = 'I       '
!     FLOWRATE ALONG Y
      MNEMO(14)   = 'J       '
!     SPEED
      MNEMO(15)   = 'M       '
!     WIND COMPONENT X
      MNEMO(16)   = 'X       '
!     WIND COMPONENT Y
      MNEMO(17)   = 'Y       '
!     ATMOSPHERIC PRESSURE
      MNEMO(18)   = 'P       '
!     FRICTION
      MNEMO(19)   = 'W       '
!     DRIFT IN X
      MNEMO(20)   = 'A       '
!     DRIFT IN Y
      MNEMO(21)   = 'G       '
!     COURANT NUMBER
      MNEMO(22)   = 'L       '
!     VARIABLE 23
      MNEMO(23)   = 'N       '
!     VARIABLE 24
      MNEMO(24)   = 'O       '
!     VARIABLE 25
      MNEMO(25)   = 'R       '
!     VARIABLE 26
      MNEMO(26)   = 'Z       '
!     VARIABLE 27
      MNEMO(27)   = 'MAXZ    '
!     VARIABLE 28
      MNEMO(28)   = 'TMXZ    '
!     VARIABLE 29
      MNEMO(29)   = 'MAXV    '
!     VARIABLE 30
      MNEMO(30)   = 'TMXV    '
!     VARIABLE 31
      MNEMO(31)   = 'US      '
!
      MNEMO(32)   = 'TAU_S   '
!
      MNEMO(33)   = '1/R     '
!     WALL DISTANCE
      MNEMO(34)   = 'WDIST   '
!
!     THE LAST RANK 34
!
      ILAST = 34
      INEXT = ILAST+1
!
!-----------------------------------------------------------------------
!
!     FOURIER ANALYSES
!
      IF(NPERIAF.GT.0) THEN
        DO I=1,NPERIAF
          WRITE(CHAR2,'(I2)') I
          IF(LNG.EQ.1) THEN
            TEXTE(INEXT+NTRAC+2*(I-1))    =  'AMPLI PERIODE '
     &                         //ADJUSTL(CHAR2)
     &                         //'M               '
            TEXTE(INEXT+1+NTRAC+2*(I-1))  =  'PHASE PERIODE '
     &                         //ADJUSTL(CHAR2)
     &                         //'DEGRES          '
            TEXTPR(INEXT+NTRAC+2*(I-1))   =  'AMPLI PERIODE '
     &                         //ADJUSTL(CHAR2)
     &                         //'M               '
            TEXTPR(INEXT+1+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //ADJUSTL(CHAR2)
     &                         //'DEGRES          '
          ELSE
            TEXTE(INEXT+NTRAC+2*(I-1))    =  'AMPLI PERIOD  '
     &                         //ADJUSTL(CHAR2)
     &                         //'M               '
            TEXTE(INEXT+1+NTRAC+2*(I-1))  =  'PHASE PERIOD  '
     &                         //ADJUSTL(CHAR2)
     &                         //'DEGRES          '
            TEXTPR(INEXT+NTRAC+2*(I-1))   =  'AMPLI PERIOD  '
     &                         //ADJUSTL(CHAR2)
     &                         //'M               '
            TEXTPR(INEXT+1+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //ADJUSTL(CHAR2)
     &                         //'DEGRES          '
          ENDIF
          MNEMO(INEXT+NTRAC+2*(I-1))   = 'AMPL'//ADJUSTL(CHAR2)//'  '
          MNEMO(INEXT+1+NTRAC+2*(I-1)) = 'PHAS'//ADJUSTL(CHAR2)//'  '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          TEXTE(ILAST+I)  = NAMETRAC(I)
          TEXTPR(ILAST+I) = NAMETRAC(I)
          WRITE(CHAR2,'(I2)') I
          MNEMO(ILAST+I)  = 'T'//ADJUSTL(CHAR2)//'   '
        ENDDO
!       OMEGA FOR SECONDARY CURRENTS
        IF(SECCURRENTS) THEN
          TEXTE(ILAST+IND_SEC) = NAMETRAC(IND_SEC)
          TEXTPR(ILAST+IND_SEC)= NAMETRAC(IND_SEC)
          MNEMO(ILAST+IND_SEC) = 'OMEGA   '
        ENDIF
      ENDIF
      IF(N_NAMES_PRIV.GT.0) THEN
        DO I=1,N_NAMES_PRIV
          TEXTE(22+I)  = NAMES_PRIVE(I)
          TEXTPR(22+I) = NAMES_PRIVE(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     DIFFERENTIATORS
!
      IF(NADVAR.GT.0) THEN
        DO I=1,NADVAR
          TEXTE(ILAST+NTRAC+2*NPERIAF+I)  = NAMES_ADVAR(I)
          TEXTPR(ILAST+NTRAC+2*NPERIAF+I) = NAMES_ADVAR(I)
          WRITE(CHAR2,'(I2)') I
          MNEMO(ILAST+NTRAC+2*NPERIAF+I) = 'AD'//ADJUSTL(CHAR2)//'    '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
