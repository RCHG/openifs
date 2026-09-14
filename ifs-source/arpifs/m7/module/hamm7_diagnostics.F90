! ╭────────────────────────────────────────────────────────────────────────────╮
! │                                                      (updated 10-Sep-2026) │
! │ Purpose :                                                                  │
! │ -------                                                                    │
! │  *hamm7_diagnostics* - YAEROUT store of M7 diagnostics                     │
! │                                                                            │
! │                                                                            │
! │ Interface :                                                                │
! │ ---------                                                                  │
! │   *HAMM7_DIAGNOSTICS" is called from HAMM7_INTERFACE                       │
! │                                                                            │
! │                                                                            │
! │ Input :                                                                    │
! │ -----                                                                      │
! │   The specific inputs depends on the subroutine used for storing diags.    │
! │                                                                            │
! │ Output :                                                                   │
! │ ------                                                                     │
! │   The subroutines fill PGFL <-> YAEROUT                                    │
! │                                                                            │
! │ Externals :                                                                │
! │ ---------                                                                  │
! │                                                                            │
! │ Method :                                                                   │
! │ ------                                                                     │
! │                                                                            │
! │ Reference :                                                                │
! │ ---------                                                                  │
! │                                                                            │
! │ Author :                                                                   │
! │ -------                                                                    │
! │     Original version are lines in hamm7_interface that were added here     │
! │     other than aod diagnostics. This can involve:                          │
! │     Vincent Huijen (KNMI), Tommi Bergman (FMI), Lianghai Wu (KNMI),        │
! │     Philippe Le Sager (KNMI)                                               │
! │                                                                            │
! │     The previous code in hamm7_interface has been moved here, refactored   │
! │     add the AOD diagnostics and commented by R.Checa-Garcia (KNMI)         │
! │ Modifications :                                                            │
! │ -------------                                                              │
! │     Sep.  2026 - R. Checa-Garcia: moved diagnostics to external and added  │
! │     May.  2024 - R. Checa-Garcia: revision for CY48r1 and refactoring      │
! │                                   a AOD per species/modes/tracers.         │
! ╰────────────────────────────────────────────────────────────────────────────╯


MODULE HAMM7_DIAGNOSTICS
  ! Explicit-argument diagnostics subrou. extracted from HAMM7_INTERFACE
  ! (see hamm7_interface_struc.F90). 
  !
  ! hamm7_interface.F90 is a huge subroutine so this part that is just 
  ! writting outputs has been moved out and created a set of subroutines 
  ! than can be called directly from hamm7_interface.F90 
  !
  ! Each declares its own local scratch (JN,JO,JH,JY,JK,ZTMP,...). It is 
  ! explicitly avoided to share/recycle variables (temporal between blocks
  ! for safety-defensive approach).
  !
  ! Still passed via module: symbols that were genuine Fortran module-level
  ! in the original using `USE` are here also replicated:
  !   MO_HAM        : nclass, naerocomp, aerocomp, subm_ngasspec
  !   OIFS_TO_HAM   : ind_oifs_ham
  !   MO_TRACDEF    : ntrac
  !   MO_TIME_CONTROL : time_step_len
  !   YOMLUN        : NULOUT
  ! Everything else (KIDIA/KFDIA/KLON/KLEV, KTRAC, YDMODEL, and every
  ! physical field) is a per-call dummy argument.
  !
  ! IMPORTANT => FURTHER DOCS AT THE END OF THIS MODULE 
  !
  ! -----------------------------------------------------------------------------
  ! YAEROUT index map (HAMM7_INTERFACE section 7, "STORE IN AEROUTs") 
  !
  !   YAEROUT(1)     -> WRITE_TOTALAOD_DIAGNOSTICS   (AOD/SSA/ASY, 33 fixed internal wavelengths)
  !   YAEROUT(2)     -> WRITE_DEPOSITION_DIAGNOSTICS (dry deposition)
  !   YAEROUT(3)     -> WRITE_DEPOSITION_DIAGNOSTICS (column-integrated wet deposition)
  !   YAEROUT(4)     -> WRITE_DEPOSITION_DIAGNOSTICS (sedimentation)
  !   YAEROUT(5)     -> WRITE_NETFLUX_DIAGNOSTICS    (net emission-surfaceflux + BL height/index)
  !   YAEROUT(6:10)  -> WRITE_TOTALAOD_DIAGNOSTICS   (AOD/AODabs/AODfm/SSA/ASYM, diagnostic wavelengths)
  !   YAEROUT(11)    -> WRITE_COLUMN_DIAGNOSTICS     (column mass+number, current)
  !   YAEROUT(12)    -> WRITE_COLUMN_DIAGNOSTICS     (mass+number tendency)
  !   YAEROUT(13)    -> WRITE_SURFACE_DIAGNOSTICS    (surface flux of tracers)
  !   YAEROUT(14)    -> WRITE_COLUMN_DIAGNOSTICS     (column PREVIOUS, before M7)
  !   YAEROUT(15)    -> WRITE_COLUMN_DIAGNOSTICS     (column of updated tendencies)
  !   YAEROUT(16)    -> WRITE_SURFACE_DIAGNOSTICS    (mass+number mixing ratio at surface)
  !   YAEROUT(17-18) -> WRITE_DEPOSITION_DIAGNOSTICS (wet deposition, in-cloud / below-cloud)
  !   YAEROUT(19-20) -> WRITE_MISC_DIAGNOSTICS       (hardcoded SS-CS tendency before/after surface)
  !   YAEROUT(21)    -> WRITE_MISC_DIAGNOSTICS       (height of each level top)
  !   YAEROUT(22-26) -> WRITE_MISC_DIAGNOSTICS       (nucleation diagnostics)
  !   YAEROUT(27)    -> WRITE_SURFACE_DIAGNOSTICS    (gas mixing ratios at surface)
  !   YAEROUT(28)    -> WRITE_EMISSION_DIAGNOSTICS   (emissions)
  !   YAEROUT(29)    -> WRITE_EMISSION_DIAGNOSTICS   (commented "29" but actually writes slot 39 -- see note there)
  !   YAEROUT(30)    -> WRITE_OPTICAL_DIAGNOSTICS    (AOD per M7 mode @550nm)
  !   YAEROUT(31)    -> WRITE_OPTICAL_DIAGNOSTICS    (AOD per tracer @550nm)
  !   YAEROUT(32)    -> WRITE_OPTICAL_DIAGNOSTICS    (AOD per chemical species @550nm)
  !   YAEROUT(33-38) -> empty ("--" placeholder comments only)
  !   YAEROUT(39)    -> WRITE_EMISSION_DIAGNOSTICS   (actual target of YAEROUT(29)'s write, see above)
  !   YAEROUT(40-45) -> commented-out (SimChem: ZFSO2/ZFSO4/ZFSO4_AQ/ZTSO4/ZTSO4_AQ/ZTSO2)
  !   YAEROUT(46)    -> comment-only section header ("DUST FIELDS DIAGNOSTICS"), no code
  !   YAEROUT(47-50) -> commented-out (dust DU_AI/DU_CI mass and number)
  !   (28+JGAS)/(29+JGAS)/(30+IMODE) -> commented-out ("Commented because of overlap" in the source)
  ! -----------------------------------------------------------------------------

  USE PARKIND1,   ONLY: JPIM, JPRB

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: WRITE_OPTICAL_DIAGNOSTICS
  PUBLIC :: WRITE_TOTALAOD_DIAGNOSTICS
  PUBLIC :: WRITE_COLUMN_DIAGNOSTICS
  PUBLIC :: WRITE_SURFACE_DIAGNOSTICS
  PUBLIC :: WRITE_DEPOSITION_DIAGNOSTICS
  PUBLIC :: WRITE_NETFLUX_DIAGNOSTICS
  PUBLIC :: WRITE_EMISSION_DIAGNOSTICS
  PUBLIC :: WRITE_MISC_DIAGNOSTICS

CONTAINS

!-----------------------------------------------------------------------------
  SUBROUTINE WRITE_OPTICAL_DIAGNOSTICS(KIDIA, KFDIA, KLON, KLEV, YDMODEL, &
       & NAEROOPT, NSTEP, NRADFR,                                         &
       & PAOD_DIAG_MODE, PAOD_DIAG_TRACER, PAOD_DIAG,                     &
       & PGFL)

    ! YAEROUT(30)/(31)/(32): AOD per M7 mode/tracer/chemical species at
    ! 550nm, plus the runtime consistency check (sum of any of the three
    ! must reproduce the total AOD). Moved from HAMM7_INTERFACE's original
    ! hamm7_interface_struc.F90 describes physics derivation of the inputs.

    USE TYPE_MODEL,  ONLY: MODEL
    USE MO_HAM,      ONLY: nclass, naerocomp, aerocomp
    USE OIFS_TO_HAM, ONLY: ind_oifs_ham
    USE YOMLUN,      ONLY: NULOUT

    IMPLICIT NONE

    INTEGER(KIND=JPIM), INTENT(IN)    :: KIDIA, KFDIA, KLON, KLEV
    TYPE(MODEL),        INTENT(IN)    :: YDMODEL
    INTEGER(KIND=JPIM), INTENT(IN)    :: NAEROOPT, NSTEP, NRADFR
    REAL(KIND=JPRB),    INTENT(IN)    :: PAOD_DIAG_MODE(KLON,YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG,nclass)
    REAL(KIND=JPRB),    INTENT(IN)    :: PAOD_DIAG_TRACER(KLON,YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG,naerocomp)
    REAL(KIND=JPRB),    INTENT(IN)    :: PAOD_DIAG(KLON,YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG)
    REAL(KIND=JPRB),    INTENT(INOUT) :: PGFL(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NDIM)

    INTEGER(KIND=JPIM) :: JCLASS, JN, JO, ISPID, IW, IW550
    INTEGER(KIND=JPIM), PARAMETER :: NSPECMAX=20 ! safe upper bound for aerocomp(:)%spid -- same
                                                  ! value as HAMM7_INTERFACE's own NSPECMAX
    REAL(KIND=JPRB) :: ZAOD_SUM_MODE(KLON), ZAOD_SUM_TRACER(KLON), ZAOD_SUM_SPECIES(KLON)
    REAL(KIND=JPRB), PARAMETER :: ZAODCHK_TOL=1.0E-6_JPRB
    LOGICAL :: LLAOD_NEG

    ASSOCIATE(YAEROUT => YDMODEL%YRML_GCONF%YGFL%YAEROUT, &
            & NAERO_WVL_DIAG => YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG)

    IF (MOD(NSTEP,NRADFR) == 0 .AND. NAEROOPT == 2) THEN

      !** YAEROUT(30) : AOD per M7 mode at 550nm
      ! Normally IW550 is the 1st index, but it is added a checker to be sure 
      ! in case of silent changes that can break the logic. 
      IW550 = 1
      DO IW=1,NAERO_WVL_DIAG
        IF (YDMODEL%YRML_GCONF%YGFL%YAERO_WVL_DIAG_NL(IW)%IWVL == 550) THEN
          IW550 = IW
          EXIT
        ENDIF
      ENDDO

      DO JCLASS=1,nclass
        PGFL(KIDIA:KFDIA, JCLASS, YAEROUT(30)%MP) = PAOD_DIAG_MODE(KIDIA:KFDIA,IW550,JCLASS)
      END DO

      !** YAEROUT(31) : AOD per tracer (naerocomp) at 550nm
      DO JN=1,naerocomp
        JO=ind_oifs_ham%ind_mass_OIFS(JN)
        PGFL(KIDIA:KFDIA, JO, YAEROUT(31)%MP) = PAOD_DIAG_TRACER(KIDIA:KFDIA,IW550,JN)
      END DO

      !** YAEROUT(32) : AOD per chemical species at 550nm
      PGFL(KIDIA:KFDIA, 1:NSPECMAX, YAEROUT(32)%MP) = 0._JPRB
      DO JN=1,naerocomp
        ISPID=aerocomp(JN)%spid
        PGFL(KIDIA:KFDIA, ISPID, YAEROUT(32)%MP) = PGFL(KIDIA:KFDIA, ISPID, YAEROUT(32)%MP) &
                                               & + PAOD_DIAG_TRACER(KIDIA:KFDIA,IW550,JN)
      END DO

      !** Consistency check
      ZAOD_SUM_MODE(KIDIA:KFDIA)    = SUM(PAOD_DIAG_MODE(KIDIA:KFDIA,IW550,1:nclass),      DIM=2)
      ZAOD_SUM_TRACER(KIDIA:KFDIA)  = SUM(PAOD_DIAG_TRACER(KIDIA:KFDIA,IW550,1:naerocomp), DIM=2)
      ZAOD_SUM_SPECIES(KIDIA:KFDIA) = SUM(PGFL(KIDIA:KFDIA,1:NSPECMAX,YAEROUT(32)%MP),     DIM=2)

      LLAOD_NEG = ANY(PAOD_DIAG_MODE(KIDIA:KFDIA,IW550,1:nclass)      < 0._JPRB) .OR. &
                & ANY(PAOD_DIAG_TRACER(KIDIA:KFDIA,IW550,1:naerocomp) < 0._JPRB) .OR. &
                & ANY(PGFL(KIDIA:KFDIA,1:NSPECMAX,YAEROUT(32)%MP)     < 0._JPRB)

      IF (MAXVAL(ABS(ZAOD_SUM_MODE(KIDIA:KFDIA)   -PAOD_DIAG(KIDIA:KFDIA,IW550))) <= ZAODCHK_TOL .AND. &
        & MAXVAL(ABS(ZAOD_SUM_SPECIES(KIDIA:KFDIA)-PAOD_DIAG(KIDIA:KFDIA,IW550))) <= ZAODCHK_TOL) THEN
        WRITE(NULOUT,*) 'HAMM7_INTERFACE: comprobada consistencia en AOD sum per modes, and sum per species is consistent with total AOD at 550nm'
      ELSE
        WRITE(NULOUT,*) 'HAMM7_INTERFACE WARNING: AOD sum per modes or sum per species NOT consistent with total AOD at 550nm', &
             & ' max|sum_modes-total|=',    MAXVAL(ABS(ZAOD_SUM_MODE(KIDIA:KFDIA)   -PAOD_DIAG(KIDIA:KFDIA,IW550))), &
             & ' max|sum_tracers-total|=',  MAXVAL(ABS(ZAOD_SUM_TRACER(KIDIA:KFDIA) -PAOD_DIAG(KIDIA:KFDIA,IW550))), &
             & ' max|sum_species-total|=',  MAXVAL(ABS(ZAOD_SUM_SPECIES(KIDIA:KFDIA)-PAOD_DIAG(KIDIA:KFDIA,IW550)))
      ENDIF

      IF (LLAOD_NEG) THEN
        WRITE(NULOUT,*) 'HAMM7_INTERFACE WARNING: negative AOD found in per-mode/tracer/species 550nm diagnostics (YAEROUT(30)/(31)/(32))'
      ENDIF

    ENDIF

    END ASSOCIATE

  END SUBROUTINE WRITE_OPTICAL_DIAGNOSTICS

!-----------------------------------------------------------------------------
  SUBROUTINE WRITE_TOTALAOD_DIAGNOSTICS(KIDIA, KFDIA, KLON, KLEV, YDMODEL, &
       & NSTEP, NRADFR,                                                    &
       & PAOD, PSSA, PASY, PAOD_LW, PAERO_WVL_DIAG,                        &
       & PGFL)

    ! YAEROUT(1): total AOD/SSA/ASY at 33 fixed internal wavelengths (one
    ! short "533nm" AOD/SSA/ASY, 14 short-wave AOD, 16 long-wave AOD) --
    ! unconditional, no NSTEP/NRADFR/NAEROOPT guard, unlike everything else
    ! in this module.
    ! YAEROUT(6:10): total AOD/AODabs/AODfm/SSA/ASYM at the flexible
    ! diagnostic-wavelength list (PAERO_WVL_DIAG, NAERO_WVL_DIAG entries),
    ! guarded on NSTEP/NRADFR only (no NAEROOPT check, unlike
    ! WRITE_OPTICAL_DIAGNOSTICS's guard).
    ! Both are TOTAL optical properties (not broken down by mode/tracer/
    ! species like WRITE_OPTICAL_DIAGNOSTICS) -- moved verbatim from
    ! HAMM7_INTERFACE's own YAEROUT(1)/(6:10) blocks.

    USE TYPE_MODEL,        ONLY: MODEL
    USE TM5M7_OPTICS_DATA, ONLY: NASWBAND
    USE YOE_AERODIAG,      ONLY: JPAERO_WVL_AOD, JPAERO_WVL_AODABS, JPAERO_WVL_AODFM, &
                                & JPAERO_WVL_SSA, JPAERO_WVL_ASSIMETRY

    IMPLICIT NONE

    INTEGER(KIND=JPIM), INTENT(IN)    :: KIDIA, KFDIA, KLON, KLEV
    TYPE(MODEL),         INTENT(IN)    :: YDMODEL
    INTEGER(KIND=JPIM), INTENT(IN)    :: NSTEP, NRADFR
    REAL(KIND=JPRB),    INTENT(IN)    :: PAOD(KLON,NASWBAND), PSSA(KLON,NASWBAND), PASY(KLON,NASWBAND)
    REAL(KIND=JPRB),    INTENT(IN)    :: PAOD_LW(KLON,16)
    REAL(KIND=JPRB),    INTENT(IN)    :: PAERO_WVL_DIAG(KLON,YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG,YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG_TYPES)
    REAL(KIND=JPRB),    INTENT(INOUT) :: PGFL(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NDIM)

    ASSOCIATE(YAEROUT => YDMODEL%YRML_GCONF%YGFL%YAEROUT,                             &
            & NAERO_WVL_DIAG => YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG,               &
            & NAERO_WVL_DIAG_TYPES => YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG_TYPES)

    !** YAEROUT(1) : RADIATIVE PROPERTIES

    ! AOD/SSA/ASY of one internal (short) wavelengths - 533 nm
    PGFL(KIDIA:KFDIA,1,YAEROUT(1)%MP)=PAOD(KIDIA:KFDIA,10)
    PGFL(KIDIA:KFDIA,2,YAEROUT(1)%MP)=PSSA(KIDIA:KFDIA,10)
    PGFL(KIDIA:KFDIA,3,YAEROUT(1)%MP)=PASY(KIDIA:KFDIA,10)

    ! AOD of 14 internal (short) wavelengths
    PGFL(KIDIA:KFDIA,4:17,YAEROUT(1)%MP)=PAOD(KIDIA:KFDIA,1:14)

    ! AOD of 16 internal (long) wavelengths
    PGFL(KIDIA:KFDIA,18:33,YAEROUT(1)%MP)= PAOD_LW(KIDIA:KFDIA,1:16)

    !** YAEROUT(6:10) : Store all requested AOP at selected (diagnostic) wavelengths

    IF(MOD(NSTEP,NRADFR) == 0) THEN
      IF (NAERO_WVL_DIAG_TYPES >= JPAERO_WVL_AOD) THEN
        PGFL(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, YAEROUT(6)%MP) = PAERO_WVL_DIAG(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, JPAERO_WVL_AOD)
      ENDIF
      IF (NAERO_WVL_DIAG_TYPES >= JPAERO_WVL_AODABS) THEN
        PGFL(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, YAEROUT(7)%MP) = PAERO_WVL_DIAG(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, JPAERO_WVL_AODABS)
      ENDIF
      IF (NAERO_WVL_DIAG_TYPES >= JPAERO_WVL_AODFM) THEN
        PGFL(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, YAEROUT(8)%MP) = PAERO_WVL_DIAG(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, JPAERO_WVL_AODFM)
      ENDIF
      IF (NAERO_WVL_DIAG_TYPES >= JPAERO_WVL_SSA) THEN
        PGFL(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, YAEROUT(9)%MP) = PAERO_WVL_DIAG(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, JPAERO_WVL_SSA)
      ENDIF
      IF (NAERO_WVL_DIAG_TYPES >= JPAERO_WVL_ASSIMETRY) THEN
        PGFL(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, YAEROUT(10)%MP) = PAERO_WVL_DIAG(KIDIA:KFDIA, 1:NAERO_WVL_DIAG, JPAERO_WVL_ASSIMETRY)
      ENDIF
    ENDIF

    END ASSOCIATE

  END SUBROUTINE WRITE_TOTALAOD_DIAGNOSTICS

!-----------------------------------------------------------------------------
  SUBROUTINE WRITE_COLUMN_DIAGNOSTICS(KIDIA, KFDIA, KLON, KLEV, KTRAC, YDMODEL, &
       & KAERO, PXTM1, PXTTE, PDPG, PCEN_NOTEND, PTENC,                                &
       & PGFL)

    ! YAEROUT(11)/(12): column mass+number concentration and its tendency,
    !                   indexed via ind_oifs_ham (HAM-internal tracer numbering)
    !                   using PXTM1/PXTTE.
    !
    ! YAEROUT(14)/(15): column-integrated but on the OIFS tracer numbering 
    !                  (KAERO-indexed) with PCEN_NOTEND/PTENC => "previous" conc.
    !                  and "updated tendencies" column respectively. 

    USE TYPE_MODEL,     ONLY: MODEL
    USE MO_HAM,         ONLY: nclass, naerocomp
    USE OIFS_TO_HAM,    ONLY: ind_oifs_ham
    USE MO_TRACDEF,     ONLY: ntrac
    USE MO_TIME_CONTROL, ONLY: time_step_len

    IMPLICIT NONE

    INTEGER(KIND=JPIM), INTENT(IN)    :: KIDIA, KFDIA, KLON, KLEV, KTRAC
    TYPE(MODEL),        INTENT(IN)    :: YDMODEL
    INTEGER(KIND=JPIM), INTENT(IN)    :: KAERO(YDMODEL%YRML_GCONF%YGFL%NAERO)
    REAL(KIND=JPRB),    INTENT(IN)    :: PXTM1(KLON,KLEV,ntrac), PXTTE(KLON,KLEV,ntrac)
    REAL(KIND=JPRB),    INTENT(IN)    :: PDPG(KLON,KLEV)
    REAL(KIND=JPRB),    INTENT(IN)    :: PCEN_NOTEND(KLON,KLEV,KTRAC), PTENC(KLON,KLEV,KTRAC)
    REAL(KIND=JPRB),    INTENT(INOUT) :: PGFL(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NDIM)

    INTEGER(KIND=JPIM) :: JN, JO, JH, JY, JK
    REAL(KIND=JPRB)    :: ZTMP(KLON)

    ASSOCIATE(YAEROUT => YDMODEL%YRML_GCONF%YGFL%YAEROUT, &
            & NACTAERO => YDMODEL%YRML_GCONF%YGFL%NACTAERO)

    !** YAEROUT(11) : Total column mass and number concentration

    DO JN=1,naerocomp
      JO=ind_oifs_ham%ind_mass_OIFS(JN)
      JH=ind_oifs_ham%IND_mass_HAM(JN)
      JY=YAEROUT(11)%MP
      ZTMP(KIDIA:KFDIA)=0.0_JPRB
      DO JK=1,KLEV
        ZTMP(KIDIA:KFDIA)= ZTMP(KIDIA:KFDIA) + (PXTM1(KIDIA:KFDIA,JK,JH)+(PXTTE(KIDIA:KFDIA,JK,JH)*time_step_len)) * PDPG(KIDIA:KFDIA,JK)
      END DO
      PGFL(KIDIA:KFDIA,JO,JY) = ZTMP(KIDIA:KFDIA)
    END DO

    DO JN=1,nclass
      JO=ind_oifs_ham%ind_class_OIFS(JN)
      JH=ind_oifs_ham%IND_class_HAM(JN)
      JY=YAEROUT(11)%MP
      ZTMP(KIDIA:KFDIA)=0.0_JPRB
      DO JK=1,KLEV
        ZTMP(KIDIA:KFDIA) = ZTMP(KIDIA:KFDIA) + (PXTM1(KIDIA:KFDIA,JK,JH)+(PXTTE(KIDIA:KFDIA,JK,JH)*time_step_len)) * PDPG(KIDIA:KFDIA,JK)
      END DO
      PGFL(KIDIA:KFDIA,JO,JY) = ZTMP(KIDIA:KFDIA)
    END DO

    !** YAEROUT(12) : mass and number tendency (kg/kg -> kg/m2, N/kg -> N/m2)

    DO JN=1,naerocomp
      JO=ind_oifs_ham%ind_mass_OIFS(JN)
      JH=ind_oifs_ham%IND_mass_HAM(JN)
      JY=YAEROUT(12)%MP
      ZTMP(KIDIA:KFDIA)=0.0_JPRB
      DO JK=1,KLEV
        ZTMP(KIDIA:KFDIA) = ZTMP(KIDIA:KFDIA) + PXTTE(KIDIA:KFDIA,JK,JH)
      END DO
      PGFL(KIDIA:KFDIA,JO,JY) = ZTMP(KIDIA:KFDIA)
    END DO

    DO JN=1,nclass
      JO=ind_oifs_ham%ind_class_OIFS(JN)
      JH=ind_oifs_ham%IND_class_HAM(JN)
      JY=YAEROUT(12)%MP
      ZTMP(KIDIA:KFDIA)=0.0_JPRB
      DO JK=1,KLEV
        ZTMP(KIDIA:KFDIA) = ZTMP(KIDIA:KFDIA) + PXTTE(KIDIA:KFDIA,JK,JH)
      END DO
      PGFL(KIDIA:KFDIA,JO,JY) = ZTMP(KIDIA:KFDIA)
    END DO

    !** YAEROUT(14) : Total column tracer/number PREVIOUS (before call to M7) concentration

    DO JN=1,NACTAERO
      ZTMP(KIDIA:KFDIA)=0.0_JPRB
      DO JK=1,KLEV
        ZTMP(KIDIA:KFDIA)=ZTMP(KIDIA:KFDIA)+PCEN_NOTEND(KIDIA:KFDIA,JK,KAERO(JN))
      END DO
      PGFL(KIDIA:KFDIA,KAERO(JN),YAEROUT(14)%MP)  = ZTMP(KIDIA:KFDIA)
    END DO

    !** YAEROUT(15) : Total column UPDATED TENDENCIES tracer

    DO JN=1,NACTAERO
      ZTMP(KIDIA:KFDIA)=0.0_JPRB
      DO JK=1,KLEV
        ZTMP(KIDIA:KFDIA)=ZTMP(KIDIA:KFDIA)+PTENC(KIDIA:KFDIA,JK,KAERO(JN))
      END DO
      PGFL(KIDIA:KFDIA,KAERO(JN),YAEROUT(15)%MP)  = ZTMP(KIDIA:KFDIA)
    END DO

    END ASSOCIATE

  END SUBROUTINE WRITE_COLUMN_DIAGNOSTICS

!-----------------------------------------------------------------------------
  SUBROUTINE WRITE_SURFACE_DIAGNOSTICS(KIDIA, KFDIA, KLON, KLEV, KTRAC, YDMODEL, &
       & KAERO, PCFLX, PDPG, PXTM1, PXTTE,                                       &
       & PGFL)

    ! YAEROUT(13): surface flux of tracers. OIFS numbering.
    !              probably it is not emissions but emissions with a sort of 
    !              deposition flux.
    ! YAEROUT(16): M7 mass and number mixing ratio AT SURFACE (last level),
    !              HAM-internal numbering using ind_oifs_ham.
    ! YAEROUT(27): M7 gas mixing ratios at surface, 
    !              using ind_oifs_ham%ind_gas_HAM.

    USE TYPE_MODEL,      ONLY: MODEL
    USE MO_HAM,          ONLY: nclass, naerocomp, subm_ngasspec
    USE OIFS_TO_HAM,     ONLY: ind_oifs_ham
    USE MO_TRACDEF,      ONLY: ntrac
    USE MO_TIME_CONTROL, ONLY: time_step_len

    IMPLICIT NONE

    INTEGER(KIND=JPIM), INTENT(IN)    :: KIDIA, KFDIA, KLON, KLEV, KTRAC
    TYPE(MODEL),        INTENT(IN)    :: YDMODEL
    INTEGER(KIND=JPIM), INTENT(IN)    :: KAERO(YDMODEL%YRML_GCONF%YGFL%NAERO)
    REAL(KIND=JPRB),    INTENT(IN)    :: PCFLX(KLON,KTRAC)
    REAL(KIND=JPRB),    INTENT(IN)    :: PDPG(KLON,KLEV)
    REAL(KIND=JPRB),    INTENT(IN)    :: PXTM1(KLON,KLEV,ntrac), PXTTE(KLON,KLEV,ntrac)
    REAL(KIND=JPRB),    INTENT(INOUT) :: PGFL(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NDIM)

    INTEGER(KIND=JPIM) :: JN, JO, JH, JY

    ASSOCIATE(YAEROUT => YDMODEL%YRML_GCONF%YGFL%YAEROUT, &
            & NACTAERO => YDMODEL%YRML_GCONF%YGFL%NACTAERO)

    !** YAEROUT(13) : Surface fluxes of tracers (not emissions, or emissions + some deposition)

    DO JN=1,NACTAERO
      PGFL(KIDIA:KFDIA,KAERO(JN),YAEROUT(13)%MP) = - PCFLX(KIDIA:KFDIA,KAERO(JN)) * PDPG(KIDIA:KFDIA,KLEV)
    END DO

    !** YAEROUT(16) : M7 mass and number mixing ratio at surface

    DO JN=1,naerocomp
      JO=ind_oifs_ham%ind_mass_OIFS(JN)
      JH=ind_oifs_ham%IND_mass_HAM(JN)
      JY=YAEROUT(16)%MP
      PGFL(KIDIA:KFDIA,JO,JY) = PXTM1(KIDIA:KFDIA,KLEV,JH)+(PXTTE(KIDIA:KFDIA,KLEV,JH)*time_step_len)
    END DO

    DO JN=1,nclass
      JO=ind_oifs_ham%ind_class_OIFS(JN)
      JH=ind_oifs_ham%IND_class_HAM(JN)
      JY=YAEROUT(16)%MP
      PGFL(KIDIA:KFDIA,JO,JY) = PXTM1(KIDIA:KFDIA,KLEV,JH)+(PXTTE(KIDIA:KFDIA,KLEV,JH)*time_step_len)
    END DO

    !** YAEROUT(27) : M7 gas mixing ratios at SURFACE

    DO JN=1,subm_ngasspec
      PGFL(KIDIA:KFDIA,JN,YAEROUT(27)%MP)=PXTM1(KIDIA:KFDIA,KLEV,ind_oifs_ham%ind_gas_HAM(JN))
    END DO

    END ASSOCIATE

  END SUBROUTINE WRITE_SURFACE_DIAGNOSTICS

!-----------------------------------------------------------------------------
  SUBROUTINE WRITE_DEPOSITION_DIAGNOSTICS(KIDIA, KFDIA, KLON, KLEV, KTRAC, YDMODEL,        &
       & KAERO, PDDEPFLUX, WDEPOUT_2D, PSEDIFLUXSURF, WDEPOUT_IC_2D, WDEPOUT_BC_2D,        &
       & PGFL)

    ! All aerosol-loss processes: YAEROUT(2) dry deposition, YAEROUT(3)
    ! column-integrated wet deposition, YAEROUT(4) sedimentation, and
    ! YAEROUT(17-18) in-cloud/below-cloud wet deposition. (2) and (4) use
    ! ind_oifs_ham (HAM-internal numbering, same pattern as
    ! WRITE_COLUMN_DIAGNOSTICS's YAEROUT(11)/(12)); (3) and (17-18) use
    ! KAERO/NACTAERO (OIFS numbering).

    USE TYPE_MODEL,  ONLY: MODEL
    USE MO_HAM,      ONLY: nclass, naerocomp
    USE OIFS_TO_HAM, ONLY: ind_oifs_ham
    USE MO_TRACDEF,  ONLY: ntrac

    IMPLICIT NONE

    INTEGER(KIND=JPIM), INTENT(IN)    :: KIDIA, KFDIA, KLON, KLEV, KTRAC
    TYPE(MODEL),         INTENT(IN)    :: YDMODEL
    INTEGER(KIND=JPIM), INTENT(IN)    :: KAERO(YDMODEL%YRML_GCONF%YGFL%NAERO)
    REAL(KIND=JPRB),    INTENT(IN)    :: PDDEPFLUX(KLON,ntrac)
    REAL(KIND=JPRB),    INTENT(IN)    :: WDEPOUT_2D(KLON,KTRAC)
    REAL(KIND=JPRB),    INTENT(IN)    :: PSEDIFLUXSURF(KLON,ntrac)
    REAL(KIND=JPRB),    INTENT(IN)    :: WDEPOUT_IC_2D(KLON,KTRAC), WDEPOUT_BC_2D(KLON,KTRAC)
    REAL(KIND=JPRB),    INTENT(INOUT) :: PGFL(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NDIM)

    INTEGER(KIND=JPIM) :: JN

    ASSOCIATE(YAEROUT => YDMODEL%YRML_GCONF%YGFL%YAEROUT, &
            & NACTAERO => YDMODEL%YRML_GCONF%YGFL%NACTAERO)

    !** YAEROUT(2) : DRY DEPOSITION

    DO JN=1,naerocomp
      PGFL(KIDIA:KFDIA,ind_oifs_ham%ind_mass_OIFS(JN),YAEROUT(2)%MP)=PDDEPFLUX(KIDIA:KFDIA,ind_oifs_ham%IND_mass_HAM(JN))
    END DO
    DO JN=1,nclass
      PGFL(KIDIA:KFDIA,ind_oifs_ham%ind_class_OIFS(JN),YAEROUT(2)%MP)=PDDEPFLUX(KIDIA:KFDIA,ind_oifs_ham%IND_class_HAM(JN))
    END DO

    !** YAEROUT(3) : COLUMN INTEGRATED WET-DEPOSITION

    DO JN=1,NACTAERO
      PGFL(KIDIA:KFDIA,KAERO(JN),YAEROUT(3)%MP)  = WDEPOUT_2D(KIDIA:KFDIA,KAERO(JN))
    END DO

    !** YAEROUT(4) : SEDIMENTATION

    DO JN=1,naerocomp
      PGFL(KIDIA:KFDIA,ind_oifs_ham%ind_mass_OIFS(JN),YAEROUT(4)%MP)  = PSEDIFLUXSURF(KIDIA:KFDIA,ind_oifs_ham%IND_mass_HAM(JN))
    END DO
    DO JN=1,nclass
      PGFL(KIDIA:KFDIA,ind_oifs_ham%ind_class_OIFS(JN),YAEROUT(4)%MP) = PSEDIFLUXSURF(KIDIA:KFDIA,ind_oifs_ham%IND_class_HAM(JN))
    END DO

    !** YAEROUT(17-18) : IN-CLOUD & BELOW CLOUD WET DEPOSITION

    DO JN=1,NACTAERO
      PGFL(KIDIA:KFDIA,KAERO(JN),YAEROUT(17)%MP) = WDEPOUT_IC_2D(KIDIA:KFDIA,KAERO(JN))
      PGFL(KIDIA:KFDIA,KAERO(JN),YAEROUT(18)%MP) = WDEPOUT_BC_2D(KIDIA:KFDIA,KAERO(JN))
    END DO

    END ASSOCIATE

  END SUBROUTINE WRITE_DEPOSITION_DIAGNOSTICS

!-----------------------------------------------------------------------------
  SUBROUTINE WRITE_NETFLUX_DIAGNOSTICS(KIDIA, KFDIA, KLON, KLEV, KTRAC, YDMODEL, &
       & KAERO, PAERSRC, PCFLX, PDPG, PBLHIDX, PBLH,                             &
       & PGFL)

    ! YAEROUT(5): net aerosol flux (emissions - surface flux) per OIFS
    ! tracer, plus two boundary-layer scalars (top-of-BL level index,
    ! BL height) packed into the same output slot -- not physically
    ! related to the flux, only co-located because the original file
    ! reuses this YAEROUT entry's spare index range for them. Kept
    ! together here for the same reason; moved verbatim, not "fixed".

    USE TYPE_MODEL, ONLY: MODEL

    IMPLICIT NONE

    INTEGER(KIND=JPIM), INTENT(IN)    :: KIDIA, KFDIA, KLON, KLEV, KTRAC
    TYPE(MODEL),         INTENT(IN)    :: YDMODEL
    INTEGER(KIND=JPIM), INTENT(IN)    :: KAERO(YDMODEL%YRML_GCONF%YGFL%NAERO)
    REAL(KIND=JPRB),    INTENT(IN)    :: PAERSRC(KLON,YDMODEL%YRML_GCONF%YGFL%NACTAERO)
    REAL(KIND=JPRB),    INTENT(IN)    :: PCFLX(KLON,KTRAC)
    REAL(KIND=JPRB),    INTENT(IN)    :: PDPG(KLON,KLEV)
    REAL(KIND=JPRB),    INTENT(IN)    :: PBLHIDX(KLON)
    REAL(KIND=JPRB),    INTENT(IN)    :: PBLH(KLON)
    REAL(KIND=JPRB),    INTENT(INOUT) :: PGFL(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NDIM)

    INTEGER(KIND=JPIM) :: JN

    ASSOCIATE(YAEROUT => YDMODEL%YRML_GCONF%YGFL%YAEROUT, &
            & NACTAERO => YDMODEL%YRML_GCONF%YGFL%NACTAERO)

    DO JN=1,NACTAERO
      PGFL(KIDIA:KFDIA,KAERO(JN),YAEROUT(5)%MP)  = PAERSRC(KIDIA:KFDIA,KAERO(JN)) - PCFLX(KIDIA:KFDIA,KAERO(JN))* PDPG(KIDIA:KFDIA,KLEV)
    END DO
    PGFL(KIDIA:KFDIA,NACTAERO+2,YAEROUT(5)%MP)  = PBLHIDX(KIDIA:KFDIA)
    PGFL(KIDIA:KFDIA,NACTAERO+3,YAEROUT(5)%MP)  = PBLH(KIDIA:KFDIA)

    END ASSOCIATE

  END SUBROUTINE WRITE_NETFLUX_DIAGNOSTICS

!-----------------------------------------------------------------------------
  SUBROUTINE WRITE_EMISSION_DIAGNOSTICS(KIDIA, KFDIA, KLON, KLEV, YDMODEL, &
       & KAERO, PAERSRC, PXTEMS,                                           &
       & PGFL)

    ! IMPORTANT: (RChG) I copied hamm7_interface, it said 29 but stored in 39.
    !            unsure if this was intentional. I kept as it was. 

    ! YAEROUT(28): emissions, OIFS numbering (PAERSRC, indexed via KAERO).
    !              currently it is only including "online emissions" not those 
    !              from emission-files.
    ! YAEROUT(29): surface emissions modified by dry deposition (PXTEMS) (?)
    ! "YAEROUT(29)" but actually writes to YAEROUT(39)%MP, not YAEROUT(29)%MP.

    USE TYPE_MODEL,  ONLY: MODEL
    USE MO_TRACDEF,  ONLY: ntrac

    IMPLICIT NONE

    INTEGER(KIND=JPIM), INTENT(IN)    :: KIDIA, KFDIA, KLON, KLEV
    TYPE(MODEL),         INTENT(IN)    :: YDMODEL
    INTEGER(KIND=JPIM), INTENT(IN)    :: KAERO(YDMODEL%YRML_GCONF%YGFL%NAERO)
    REAL(KIND=JPRB),    INTENT(IN)    :: PAERSRC(KLON,YDMODEL%YRML_GCONF%YGFL%NACTAERO)
    REAL(KIND=JPRB),    INTENT(IN)    :: PXTEMS(KLON,ntrac)
    REAL(KIND=JPRB),    INTENT(INOUT) :: PGFL(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NDIM)

    INTEGER(KIND=JPIM) :: JN

    ASSOCIATE(YAEROUT => YDMODEL%YRML_GCONF%YGFL%YAEROUT, &
            & NACTAERO => YDMODEL%YRML_GCONF%YGFL%NACTAERO)

    !** YAEROUT(28) : EMISSIONS (probably only "online emissions")

    DO JN=1,NACTAERO
      PGFL(KIDIA:KFDIA, JN, YAEROUT(28)%MP) = PAERSRC(KIDIA:KFDIA,KAERO(JN))
    END DO

    !** YAEROUT(29) : SURFACE EMISSIONS MODIFIED BY DRY DEPOSITION
    !                 (original writes to YAEROUT(39)%MP, be carefull!!!)

    DO JN=1,ntrac
      PGFL(KIDIA:KFDIA,JN,YAEROUT(39)%MP)=PXTEMS(KIDIA:KFDIA,JN)
    END DO

    END ASSOCIATE

  END SUBROUTINE WRITE_EMISSION_DIAGNOSTICS

!-----------------------------------------------------------------------------
  SUBROUTINE WRITE_MISC_DIAGNOSTICS(KIDIA, KFDIA, KLON, KLEV, YDMODEL, &
       & PXTTE, PTENCIH, PGEOH, PRG, POUT_DNUC,                        &
       & PGFL)

    ! RChG => I created a group misc. for these sets of diagnostics that 
    !         can be of different nature. 

    ! YAEROUT(19-20): hardcoded SS-CS tendency before/after surface update.
    ! YAEROUT(21):    height of each level top.
    ! YAEROUT(22-26): nucleation diagnostics (POUT_DNUC, N_NUC_DIAG=5).


    USE TYPE_MODEL, ONLY: MODEL
    USE MO_TRACDEF, ONLY: ntrac

    IMPLICIT NONE

    INTEGER(KIND=JPIM), INTENT(IN)    :: KIDIA, KFDIA, KLON, KLEV
    TYPE(MODEL),        INTENT(IN)    :: YDMODEL
    REAL(KIND=JPRB),    INTENT(IN)    :: PXTTE(KLON,KLEV,ntrac)
    REAL(KIND=JPRB),    INTENT(IN)    :: PTENCIH(KLON,KLEV,ntrac)
    REAL(KIND=JPRB),    INTENT(IN)    :: PGEOH(KLON,0:KLEV)
    REAL(KIND=JPRB),    INTENT(IN)    :: PRG
    INTEGER(KIND=JPIM), PARAMETER     :: N_NUC_DIAG=5
    REAL(KIND=JPRB),    INTENT(IN)    :: POUT_DNUC(KLON,KLEV,N_NUC_DIAG)
    REAL(KIND=JPRB),    INTENT(INOUT) :: PGFL(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NDIM)

    INTEGER(KIND=JPIM) :: JK

    ASSOCIATE(YAEROUT => YDMODEL%YRML_GCONF%YGFL%YAEROUT)

    !** YAEROUT(19-20) :

    PGFL(KIDIA:KFDIA,KLEV,YAEROUT(19)%MP)   = PXTTE(KIDIA:KFDIA,KLEV,3)      ! tendency SS CS ham after update surface
    PGFL(KIDIA:KFDIA,KLEV-1,YAEROUT(20)%MP) = PTENCIH(KIDIA:KFDIA,KLEV,17)   ! tendency SS CS ham before update surface

    !** YAEROUT(21) : height of each level top

    DO JK=1,KLEV
      PGFL(KIDIA:KFDIA,JK,YAEROUT(21)%MP) = (PGEOH(KIDIA:KFDIA,JK-1)-PGEOH(KIDIA:KFDIA,KLEV))*PRG
    END DO

    !** YAEROUT(22-26) : Nucleation diagnostics

    PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(22)%MP) = POUT_DNUC(KIDIA:KFDIA,1:KLEV,1)  ! NS-mass production kg/s from nucleation (limited by amount of SO4)
    PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(23)%MP) = POUT_DNUC(KIDIA:KFDIA,1:KLEV,2)  ! NS-number production rate #/s from nucleation (limited by amount of SO4 vs original #/s)
    PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(24)%MP) = POUT_DNUC(KIDIA:KFDIA,1:KLEV,3)  ! original #/s from H2SO4/H2O nucleation
    PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(25)%MP) = POUT_DNUC(KIDIA:KFDIA,1:KLEV,4)  ! original #/s from organic nucleation
    PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(26)%MP) = POUT_DNUC(KIDIA:KFDIA,1:KLEV,5)  ! original #/s sum from organic and vehkamaki nucleation schemes

    END ASSOCIATE

  END SUBROUTINE WRITE_MISC_DIAGNOSTICS

END MODULE HAMM7_DIAGNOSTICS

! =============================================================================
! Reference documentation
! =============================================================================
!
! YAEROUT(30) : AOD per M7 mode (nclass) at 550nm only and NAEROOPT=2 (HAM optics) only. 
!               PGFL(:,JCLASS,YAEROUT(30)%MP), JCLASS=1:nclass. 
!               (1) Summing over JCLASS should reproduce PGFL(:,IW550,YAEROUT(6)%MP)
!               (AOD total at 550nm). 
!               Requires NAEROUT raised to >= 30 in the namelist.
!               IW550 picks out whichever YAERO_WVL_DIAG_NL entry is 550nm 
!
! YAEROUT(31) : AOD per tracer (naerocomp, mode x species but linear) at 550nm.
!               NAEROOPT=2 only. 
!               PGFL(:,JO,YAEROUT(31)%MP), JO=ind_oifs_ham%ind_mass_OIFS(JN) 
!               This is the  same OIFS tracer identity numbers that YAEROUT(11)/(12).
!               (the position of this tracer in the full YAERO metadata table).
!
!               IMPORTANT: This only ensure a valid "level" position for values 
!               of the index of ind_oifs_ham%ind_mass. So we expect a non-continuous 
!               list of numbers as only mass-tracers are considered. 
!
!               This is a possible list but needs more validation
!               5  = "SO4_AS"
!               6  = "BC_AS"
!               7  = "POM_AS"
!               8  = "SS_AS"
!               9  = "DU_AS"
!               17 = "BC_KI"
!               18 = "POM_KI"
!               20 = "DU_AI"
!               22 = "SO4_KS"
!               23 = "BC_KS"
!               24 = "POM_KS"
!               26 = "DU_CI"
!               28 = "SO4_CS"
!               29 = "BC_CS"
!               30 = "POM_CS"
!               31 = "SS_CS"
!               32 = "DU_CS"
!               34 = "SO4_NS"
!
!               Summing over the JN whose aerocomp(JN)%iclass is a given mode must reproduce
!               that mode's value in YAEROUT(30); summing over the JN of one chemical species
!               (across modes) gives the per-species AOD (YAEROUT(32)).
!               Uses IW550 computed for YAEROUT(30).
!
! YAEROUT(32) : AOD per chemical species at 550nm (e.g. all DU_* modes are added into
!               "dust" value). NAEROOPT=2 only. 
!               PGFL(:,ISPID,YAEROUT(32)%MP), ISPID=aerocomp(JN)%spid identifies 
!               the species (same spid for every mode a species appears in, 
!               e.g. DU_AS/DU_AI/DU_CI/DU_CS all share one spid).
!               Reuses ZAOD_DIAG_TRACER and IW550 computed before (YAEROUT(31)) 
!
!               ISPID comes from mo_ham_species.F90's SUBROUTINE ham_species
!                 1=DMS 
!                 2=SO2 
!                 3=OH 
!                 4=H2O2 
!                 5=O3 
!                 6=NO2 
!                 7=NO3  
!                 8=H2SO4 
!                 (Until here all gas-phase, not in aerocomp(:)%spid which has aerosol-phase)
!                 9=SO4  
!                 10=BC
!                 11=OC (namelist tracers call this "POM_xx")
!                 12=SS
!                 13=DU
!                 14=WAT (aerowater(:), not in aerocomp)
!               So only ISPID 9-13 are ever actually written here.
!
!               Relation matrix between modes and species-aerosols:
!                 SO4: NS, KS, AS, CS         (4 modes)
!                 BC:  KS, AS, CS, KI         (4 modes)
!                 OC:  KS, AS, CS, KI         (4 modes)  ("POM_xx" in YAERO_NL)
!                 SS:  AS, CS                 (2 modes)
!                 DU:  AS, CS, AI, CI         (4 modes)
!                                   naerocomp = 4+4+4+2+4 = 18 tracers total
!
!               So: 
!               YAEROUT(30) -> 7 values (modes)
!               YAEROUT(31) -> 18 tracers, one per (species,mode)
!               YAEROUT(32) -> 5 species, ISPID 9-13.
!
!               The SOA_NS/KS/AS/CS/KI, ELVOC/ISVOC/MSA tracers seen in some YAERO_NL
!               namelists do NOT appear in aerocomp at all: their species (mo_ham_soa.F90,
!               23 more new_species calls) are only registered via CALL soa_species, which is
!               under #ifdef HAMMOZ so not used (yet) in OpenIFS-M7 
!
! Consistency check for the AOD-by-mode/tracer/species diagnostics above: they are all
!               re-groupings of the same additive quantity, so summing YAEROUT(30) over its
!               7 modes, or YAEROUT(31)'s underlying 18 tracers, or YAEROUT(32) over its 5
!               species, must all reproduce the total AOD at 550nm (ZAOD_DIAG(:,IW550), same
!               value as YAEROUT(6) at IW550) to within floating-point summation-order error
!               and AOD, an optical thickness, must never be negative. 
!               Any of these check failing may means a bug in the volume-fraction 
!               split implementation maybe in mo_ham_rad.F90 or here, not in M7's own physics
!               necessarely. 
! =============================================================================
