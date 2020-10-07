      SUBROUTINE BHWID1(MODE,XPAR,NPAR)
*     *********************************
!----------------------------------------------------------------------!
!          **************************************************          !
!          *       **********************************       *          !
!          *       *      *******************       *       *          !
!          *       *      *                 *       *       *          !
!          *       *      *   B H W I D 1   *       *       *          !
!          *       *      *                 *       *       *          !
!          *       *      *******************       *       *          !
!          *       **********************************       *          !
!          **************************************************          !
!======================================================================!
!=======================     AUTHORS      =============================!
!============   S. Jadach, W. Placzek and B.F.L. Ward  ================!
!==================  VERSION 1.01  September 1996  ====================!
!======================================================================!
!                                                                      !
C   modifications for Linux compiler : b.Bloch Oct 2000
C   eeeevs: replace REAL by DREAL otherwise result is REAL*4 not *8
C   xmatvs: replace REAL by DREAL otherwise result is REAL*4 not *8
C   infra : declare CDABS as REAL *8 not complex*16
C
! Main subprogram in MC multiphoton generator for Bhabha scattering.   !
! It is multiphoton generator with Yennie-Frautschi-Suura first        !
! order exponentiation based on refs. [1-3].                           !
! Electroweak virtual and soft photon correction are taken from        !
! the program BABAMC [4,5] or the program ALIBABA [6] (the latter      !
! is recommended in this version).                                     !
! [1] S. Jadach, W. Placzek and B.F.L. Ward, UTHEP-95-1001 (Oct. 1995) !
!     hep-ph/9608412; submitted to Phys. Lett. B.                      !
! [2] S. Jadach and B.F.L. Ward,                                       !
!     Phys. Rev. D40 (1989) 3582.                                      !
! [3] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,              !
!     Comput. Phys. Commun. 70 (1992) 305; TH-6230, Sept. 1991.        !
! [4] M. Bohm, A. Denner and W. Hollik,                                !
!     Nucl. Phys. B304 (1988) 687.                                     !
! [5] F.A. Berends, R. Kleiss and W. Hollik,                           !
!     Nucl. Phys. B304 (1988) 712.                                     !
! [6] W. Beenakker, F.A. Berends and S.C. van der Marck,               !
!     Nucl. Phys. B349 (1991) 323.                                     !
!                                                                      !
!----------------------------------------------------------------------!
!                 INPUT and OUTPUT of BHWID1                           !
!----------------------------------------------------------------------!
! All input and output goes through parameters in                      !
!                 CALL BHWID1(MODE,XPAR,NPAR)                          !
! and through /MOMSET/ and /WGTALL/ common blocks.                     !
! In the following we shall  briefly indicate the meaning of the       !
! above parameters/variables.                                          !
!                                                                      !
! IF( MODE =-1 ) THEN                                                  !
! ===================                                                  !
! Initialization is performed, all input parameters are transfered     !
! through XPAR and NPAR.                                               !
! In the following table we indicate the meaning of NPAR, XPAR.        !
!                                                                      !
!      Table,           Input parameters of BHWID1                     !
!----------------------------------------------------------------------!
!  Entry    Variable   Meaning                                         !
!----------------------------------------------------------------------!
!  NPAR( 1) =KeyOpt =10*KeyWgt +KeyRnd                                 !
!                    General option switch:                            !
!            KeyWgt - switch for constant, variable weight WTMOD:      !
!                   =0 WTMOD=1 useful for apparatus Monte Carlo,       !
!                      To make it more efficient the user may need     !
!                      to adjust a value of WTMAX in routine BHWID1    !
!                   =1 WTMOD varying, faster/safer, RECOMMENDED        !
!            KeyRnd =1,2 type of random number generator RANMAR,RANECU !
!  NPAR( 2) =KeyRad =1000*KeyEWC +100*KeyLib +10*KeyMod +KeyPia        !
!                    is option switch for ElectroWeak Radiative Corr.  !
!            KeyEWC - switching ON/OFF weak corrections:               !
!                   =0 only QED corrections included                   !
!                      (here both KeyLib =1,2 should be equivalent)    !
!                   =1 all ElectroWeak Corrections included            !
!            KeyLib - option for ElectroWeak Corrections Library:      !
!                   =1 ElectroWeak Corr. from BABAMC (obsolete)        !
!                   =2 ElectroWeak Corr. from ALIBABA, RECOMMENDED     !
!            KeyMod - type of MODEL subprogram and QED matrix element  !
!                     for hard bremsstrahlung:                         !
!                   =1 obtained by the authors (helicity amplitudes)   !
!                   =2 from CALKUL, Nucl. Phys. B206 (1982) 61.        !
!                      Checked to be in a very good agreement!         !
!            KeyPia - photon vacuum polarization switch:               !
!                   =0 OFF,                                            !
!                   =1 ON, Burkhardt et.al. 1989, as in BHLUMI 2.0x    !
!                   =2 ON, S. Eidelman, F. Jegerlehner, Z.Phys.C(1995) !
!                   =3 ON, Burkhardt and Pietrzyk 1995 (Moriond).      !
!                   NOTE: Now it works for both ALIBABA and BABAMC     !
!                         rutines as well as for KeyEWC=0!             !
!  XPAR( 1) =CMSENE Total center mass energy [GeV]                     !
!  XPAR( 2) =THMINP Minimum scattering angle [deg] for positron        !
!  XPAR( 3) =THMAXP Maximum scattering angle [deg] for positron        !
!  XPAR( 4) =THMINE Minimum scattering angle [deg] for electron        !
!  XPAR( 5) =THMAXE Maximum scattering angle [deg] for electron        !
!  XPAR( 6) =ENMINP Minimum energy [GeV] for final state positron      !
!  XPAR( 7) =ENMINE Minimum energy [GeV] for final state electron      !
!  XPAR( 8) =ACOLLI Maximum acollinearity [deg] of final state e+e-    !
!  XPAR( 9) =EPSCMS Dimensionless infrared cut on CMS energy of soft   !
!                   photons, ( E_phot > CMSENE*EPSCMS/2 )              !
! Note: All the above angular limits are given with respect to the     !
!       corresponding incoming particle (positron or electron, resp.). !
!                                                                      !
! Some other parameters (like masses, coupling constants, etc.) are    !
! set up in the subroutine FILBHW (see below).                         !
! Note: In some cases the user may also need to adjust transfer limits !
!       (trmin and/or trmid) in routine FILBHW.                        !
!----------------------------------------------------------------------!
!                                                                      !
! ELSE IF( MODE = 0 ) THEN                                             !
! ========================                                             !
! Generation of the single Monte Carlo event.                          !
! The four momenta of the final state electron, positron and photon    !
! and of real photons are encoded in                                   !
!      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT     !
! where P1 and Q1 are four-momenta of positron and elecron beams.      !
! P2 and Q2 are four-momenta of outgoing positron and electron.        !
! The list PHOT(100,4) four-momenta contains                           !
! NPHOT four-momenta of real the photons, all in GeV.                  !
! NOTE: +z axis points along incoming e+.                              !
!                                                                      !
! The principal weight WTM of the event is placed in                   !
!      COMMON / WGTALL / WTMOD,WTCRUD,WTTRIG,WTSET(300)                !
! It is often of interest to use "paralel weights" from WTSET.         !
! The event weight is constructed then as WT= WTCRUD*WTTRIG*WTSET(J).  !
! Which J is alowed and what version of the QED matrix element         !
! it represents is summarized in the table below.                      !
! (Note that using "parallel weights" makes only sense for calculation !
! with variable weights, KEYWGT=1.)                                    !
! WTTRIG is a "trigger" weight (0 or 1) calculated in routine trigMC   !
! (see below) for acceptances defined by the input parameters. It is   !
! evaluated before the model weights WTSET(J) are calculated           !
! (the latter is done only for WTTRIG=1 which increases the efficiency !
! of the program).                                                     !
! To avoid unnecessary crashes of the program while using the parallel !
! weights the kinematical calculations should be protected by an       !
! appriopriate conditional statement:                                  !
! IF (WTCRUD*WTTRIG.NE.0D0) THEN ... ENDIF                             !
!                                                                      !
!              Table of WTSETS entries for BHWID1                      !
!----------------------------------------------------------------------!
!  Entry      Type of QED calculation in MODEL1                        !
!----------------------------------------------------------------------!
!  WTSET(10)    O(alpha^0)exp (YFS exponentiated)                      !
!  WTSET(11)    O(alhpa^1)exp (YFS exponentiated)                      !
!  WTSET(20)    O(alpha^0)    (non-exponentiated)                      !
!  WTSET(21)    O(alhpa^1)    (non-exponentiated)                      !
!----------------------------------------------------------------------!
! Principal weight from MODEL1 (used to calculate WTMOD) is WTSET( 1). !
! The best and default set-up for MODEL1 is:                           !
!              WTSET( 1) = WTSET(11)                                   !
!----------------------------------------------------------------------!
!  Entry      Type of QED calculation in MODEL2                        !
!----------------------------------------------------------------------!
!  WTSET(110)    O(alpha^0)exp (YFS exponentiated)                     !
!  WTSET(111)    O(alhpa^1)exp (YFS exponentiated)                     !
!  WTSET(120)    O(alpha^0)    (non-exponentiated)                     !
!  WTSET(121)    O(alhpa^1)    (non-exponentiated)                     !
!----------------------------------------------------------------------!
! Principal weight from MODEL2 (used to calculate WTMOD) is WTSET(101).!
! The best and default set-up for MODEL2 is:                           !
!              WTSET(101) = WTSET(111)                                 !
!                                                                      !
! ELSE IF( MODE = 1 ) THEN                                             !
! ========================                                             !
! The total cross section corresponding to generated series of event,  !
! i.e. resulting from MC integrartion is calculated and stored in XPAR !
! and NPAR, see table below.                                           !
!----------------------------------------------------------------------!
!  Entry    Variable   Meaning                                         !
!----------------------------------------------------------------------!
!  NPAR(10)  NEVGEN  Number of generated MC events                     !
!  NPAR(20)  NEVGEN  Number of generated MC events                     !
!  XPAR(10)   XMCNB  Total x-section [nb]                              !
!  XPAR(11)    EREL  The relative error of XPAR(10)                    !
!  XPAR(12)     XMC  Total x-section [GEV**-2]                         !
!  XPAR(20)  SIG0NB  Crude total MC x-section [nb] which is necessary  !
!                    for rescaling histograms in run with              !
!                    weighted events.                                  !
!  XPAR(21)          =0, error of XPAR(20) is zero                     !
!  XPAR(20)    SIG0  Crude x-sectio as XPAR(20) but in [GeV**-2]       !
!----------------------------------------------------------------------!
! For constant weight option KEYWGT=0 (convevience in rescaling histos)!
! we put XPAR(20,21,22)=XPAR(10,11,12)                                 !
! For MODE=1 program is called upon many times in the process of       !
! rescaling histograms, therefore, there is no output printed          !
! in this mode.                                                        !
!                                                                      !
! ELSE IF( MODE = 2 ) THEN                                             !
! ========================                                             !
! Only in this MODE=2 in addition to filling XPAR and NPAR as for      !
! MODE=1 the values of various x-sections are printed on standard      !
! output file.                                                         !
!                                                                      !
! ENDIF                                                                !
! ====                                                                 !
!----------------------------------------------------------------------!
! Last update: Oct. 04  1996                by: W. Placzek             !
!======================================================================!
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      PARAMETER( ALFPI=  1D0/PI/ALFINV ,ALFA=1D0/ALFINV)
      PARAMETER( GNANOB=389.385D-30*1.D33 )
      CHARACTER*80   BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      DIMENSION  XPAR(*), NPAR(*)
      COMMON / BHPAR1 / DEL,EPSCMS,THMIN,XMIVIS
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      COMMON / BHCRUD / trmid,crufla,Zprof,sg01,sg02,sg03,sig0
* CMONIT COMMUNICATES WITH GMONIT
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / MOMSET / PX1(4),QX1(4),PX2(4),QX2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRUD,WTTRIG,WTSET(300)
      COMMON / WGTSUP / WKP,WKQ,WTT1,WTT2,FPHS,FYFSU,FYFSD,WT3
      COMMON / INOUT  / NINP,NOUT
      SAVE   / BHPAR1 /, / BHPAR2 /, / BHPAR3 /, / CMONIT/, / TRANSR /
      SAVE   / MOMS1  /, / MOMS2  /, / MOMZ1  /, / MOMZ2 /, / MOMSET /
      SAVE   / WGTALL /, / WGTSUP /, / INOUT  /, / BHPARZ/, / BHCRUD /
      SAVE   SVAR, WTMAX, TRMX2, EMIN, IDA,IDC, KEYWGT, KEYREM, KEYUPD
      SAVE   IDGEN, NEVGEN, IEVENT, SIG0NB
      REAL*8 DRVEC(100)
!
      IF(MODE.EQ.-1) THEN
*     ===================
! Set input parameters
      CALL FILBHW(XPAR,NPAR)
      SVAR  = CMSENE**2
! Calculate total crude x-section
      Zprof = (GV**2+GA**2)**2 *SVAR**2
     $       /( (SVAR-AMAZ**2)**2 + (AMAZ*GAMMZ)**2 )
      crufla= Zprof + (SVAR/trmid)**2
      sg01  = 4*PI*ALFA**2 *Zprof*(TRMAX-trmid)/SVAR**2
      sg02  = 4*PI*ALFA**2 *(1/trmid - 1/TRMAX)
      sg03  = 4*PI*ALFA**2 *crufla*(trmid-TRMIN)/SVAR**2
      sig0  = sg01 + sg02 + sg03
      SIG0NB= sig0*GNANOB
! Maximum weight (can be adjusted by the user)
      WTMAX = 10.0
! Important histo which remembers total x-section
      IDGEN = 1
      CALL GMONIT(  -1, IDGEN,0D0,SIG0NB*WTMAX,1D0)
!-- maximum transfer for photon angular distributions
!-- TRMX2=svar is a safe choice, for low  angles
!-- (thmin,thmax << 10mrad)
!-- it can be lowered to speed up generation (but with great care).
!--      TRMX2 = TRMAX
! (over)conservative and safe choice is TRMX2=SVAR
      TRMX2 = SVAR
!--      IF(TRMX2.GT.SVAR) TRMX2=SVAR
      EMIN = CMSENE/2D0*EPSCMS
! WEIGHT MONITORING INITIALIZATION
      IDA=50
      DO 11 I=IDA+1,IDA+20
  11  CALL GMONIT(  -1, I,0D0,1D0,1D0)
      IDC = 90
      DO 12 I=IDC+1,IDC+9
  12  CALL GMONIT(  -1, I,0D0,1D0,1D0)
      CALL GBOOK1(9001,' Total weight distribution $',60,-1D0,11D0)
      KEYWGT = MOD(KEYOPT,100)/10
      KEYREM = MOD(KEYOPT,1000)/100
      KEYUPD = MOD(KEYRAD,1000)/100
      IEVENT=0
      NEVGEN=0
      ELSEIF(MODE.EQ.0) THEN
*     ======================
      NEVGEN = NEVGEN+1
  200 CONTINUE
      IEVENT=IEVENT+1
      WT1    =0
      WT2    =0
      WT3    =0
      WT4    =0
      WT5    =0
      WT6    =0
      WTKIN  =0
      WTCRUD =0
      WTTRIG =0
!--------- Generate t-channel transfer (true one) ---------
      CALL gentra(TRMIN,TRMX2,TRAN)
!--------------------  Photon generation ------------------
      CALL MLTIBR(TRAN,TRMX2,AMEL,DEL,
     $            NPHOT1,PHOT1,PHSU1,AL1,BE1,TRANP,AMSP,MK1,WKP,WTM1)
      CALL MLTIBR(TRAN,TRMX2,AMEL,DEL,
     $            NPHOT2,PHOT2,PHSU2,AL2,BE2,TRANQ,AMSQ,MK2,WKQ,WTM2)
      IF(WKP*WKQ.EQ.0D0) GOTO 140
!-- Construct fermions, transform photons and fermions to CMS frame
      CALL KINO4(SVAR,TRAN,AMEL,AMSP,AMSQ,WT3)
      CALL GMONIT(   0,IDA+3,  WT3, 1D0,5D-4)
      IF(WT3.EQ.0D0) GOTO 140
!-- Beyond this point only events conserving four-momentum !!!
      WTKIN=1D0
!-- Manipulations on mass weights, removal of soft photons
      CALL PIATEK(CMSENE,TRMX2,AMEL,EMIN,DEL,
     $      NPHOT1,P1,P2,PHOT1,PHSU1,WTM1,WTT1,WTMR1,WCTA1,WCTB1)
      CALL PIATEK(CMSENE,TRMX2,AMEL,EMIN,DEL,
     $      NPHOT2,Q1,Q2,PHOT2,PHSU2,WTM2,WTT2,WTMR2,WCTA2,WCTB2)
!-- Removing photons < EPSCMS from the record
!-- Mass weight WTMR1,2 is product of mass weights for ENE>EminCM times
!-- Average weight for photons with  ENE<EminCM.
      CALL REMPHO(EMIN,NPHOT1,PHOT1,P2,AL1,BE1,WTM1,MK1)
      CALL REMPHO(EMIN,NPHOT2,PHOT2,Q2,AL2,BE2,WTM2,MK2)
!---------- monitoring control weights
      CALL GMONIT(   0,IDC+1,       WCTA1, 1D0,5D-4)
      CALL GMONIT(   0,IDC+2,       WCTA2, 1D0,5D-4)
      CALL GMONIT(   0,IDC+3, WCTA1*WCTA2, 1D0,5D-4)
      CALL GMONIT(   0,IDC+4,       WCTB1, 1D0,5D-4)
      CALL GMONIT(   0,IDC+5,       WCTB2, 1D0,5D-4)
      CALL GMONIT(   0,IDC+6, WCTB1*WCTB2, 1D0,5D-4)
      WTM1T2 = WTMR1*WTMR2
      CALL GMONIT(   0,IDA+1,      WTM1T2,  2D0,5D-4)
      WT1 = WTMR1*WKP
      WT2 = WTMR2*WKQ
!-- Merge photons/fermion into one common block
      CALL MERGIK
!-- Crude weight before calculating a trigger weight
      WTCRUD = WT1*WT2*WT3
!-- M.C. trigger weight
      CALL trigMC(WTTRIG)
      IF (WTTRIG.EQ.0D0) GOTO 140
!----------------- YFS FORMFACTOR ----------------------
! Crude
      fPHS  = EXP( 4*ALFPI*LOG(TRMX2/AMEL**2)*LOG(1/DEL) )
! Exact
      pdel = DEL*BCUD(P1,P2,PHSU1)
      qdel = DEL*BCUD(Q1,Q2,PHSU2)
      Blogp = LOG(TRANP/AMEL**2)
      Blogq = LOG(TRANQ/AMEL**2)
      fYFSu = EXP( ALFPI*( -2*(Blogp -1)*LOG(1/pdel) +0.5*Blogp -1 ) )
      fYFSd = EXP( ALFPI*( -2*(Blogq -1)*LOG(1/qdel) +0.5*Blogq -1 ) )
      fYFSr = YFSfsu(EPSCMS)
      fYFS  = fYFSu*fYFSd*fYFSr
! Weight
      WT4 = fYFS*fPHS
      CALL GMONIT(   0,IDA+4,WT4,  1D0,5D-4)
!-- Restoring up-down interference
      CALL WTinte(WT5)
      CALL GMONIT(   0,IDA+5,WT5,  1D0,5D-4)
!-- Crude weight before including a model weight
      WTCRUD = WTCRUD*WT4*WT5
!---------------------- MODEL ----------------------------------
      CALL MODEL(1,WT6)
 140  CONTINUE
!-- Total weight
      WT  = WTCRUD*WTTRIG*WT6
!-- Monitoring model weight
      CALL GMONIT(   0,IDA+20,WT,WTMAX,RN)
      WTOVR = MAX(0D0,WT-WTMAX)
      CALL GMONIT(   0,IDA+18,  WTOVR,0D0,0D0)
      WTNEG = MIN(WT,0D0)
      CALL GMONIT(   0,IDA+19,  WTNEG,0D0,0D0)
      CALL GF1(9001,WT,1D0)
! ...Rejection according to principal weight
      IF(KEYWGT.EQ.0) THEN
! ...unweihgted events with WT=1
        CALL VARRAN(DRVEC,1)
        RN = DRVEC(1)
        IF(WT.LT.RN*WTMAX) GOTO 200
        WTMOD = 1
! ...WTCRUD, WTTRIG  weights are RESET to zero for control
        WTCRUD = 0
        WTTRIG = 0
      ELSE
! ...weighted events
        WTMOD  = WT
        CALL GMONIT(  0, IDGEN, SIG0NB, WTMAX,1D0)
      ENDIF
      ELSE
*     ====
      NPAR(10)= NEVGEN
      NPAR(20)= NEVGEN
      CALL GMONIT(   1,IDA+20,AWTOT,DWTOT,WWMX )
      XSMC   = SIG0  *AVERWT
      XSMCNB = SIG0NB*AVERWT
      EREL   = ERRELA
      ERMC   = XSMCNB*EREL
      XPAR(10) = XSMCNB
      XPAR(11) = EREL
      XPAR(12) = XSMC
      IF(KEYWGT.EQ.0) THEN
! ...WT=1  events, normal option...
         XPAR(20)=XSMCNB
         XPAR(21)=EREL
         XPAR(22)=XSMC
      ELSE
! ...Weighted events, additional information on x-sections
         XPAR(20)= SIG0NB
         XPAR(21)= 0D0
         XPAR(22)= SIG0
      ENDIF
! Printout only for MODE=2
      IF(MODE.EQ.1) RETURN
!=====(((
!#     CALL GPRINT(9001)
!=====)))
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHWID1:        WINDOW A        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL1I) NEVGEN,     ' Accepted total    ','NEVGEN','A1'
      WRITE(NOUT,BXL1I) IEVENT,     ' Raw prior reject. ','IEVENT','A2'
      WRITE(NOUT,BXL2G) XSMCNB,ERMC,' Xsec M.C. [nb]    ','XSECMC','A3'
      WRITE(NOUT,BXL1F) EREL,       ' relat. error      ','ERELMC','A4'
      WRITE(NOUT,BXL2F) AWTOT,DWTOT,' weight  M.C.      ','AWT   ','A5'
      WRITE(NOUT,BXL1I) NEVNEG,     ' WT<0              ','NEVNEG','A6'
      WRITE(NOUT,BXL1I) NEVOVE,     ' WT>WTMAX          ','NEVOVE','A7'
      WRITE(NOUT,BXL1F) WTMAX ,     ' Maximum WT        ','WWMX  ','A8'
      WRITE(NOUT,BXCLO)
! PRINT ADDITIONAL INFOS
!------------------------------------------------------------
      CALL GMONIT(   1,IDA+1, AWT1 ,DWT1 ,DUMM3)
      CALL GMONIT(   1,IDA+2, AWT2 ,DWT2 ,DUMM3)
      CALL GMONIT(   1,IDA+3, AWT3 ,DWT3 ,DUMM3)
      CALL GMONIT(   1,IDA+4, AWT4 ,DWT4 ,DUMM3)
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHWID1:        WINDOW B        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL2F) AWT1,DWT1  ,'WT1*WT2*T/TP*T/TQ  ','      ','B1'
      WRITE(NOUT,BXL2F) AWT3,DWT3  ,'WT3 from KINO4     ','      ','B2'
      WRITE(NOUT,BXL2F) AWT4,DWT4  ,'YFS formfac        ','WT    ','B4'
      WRITE(NOUT,BXL2F) AWTOT,DWTOT,'TOTAL              ','      ','B5'
      CALL GMONIT(   1,IDA+18, AWT18 ,RWT18 ,DUMM3)
      XWT18 = AWT18/AWTOT
      DWT18 = XWT18*RWT18
      WRITE(NOUT,BXL2F) XWT18,DWT18,'xsec/xtot: WT>WTMAX','WT    ','B6'
      CALL GMONIT(   1,IDA+19, AWT19 ,RWT19 ,DUMM3)
      XWT19 = AWT19/AWTOT
      DWT19 = XWT19*RWT19
      WRITE(NOUT,BXL2F) XWT19,DWT19,'xsec/xtot: WT<0    ','WT    ','B7'
      WRITE(NOUT,BXCLO)
! ---------------------------------------------------------------
      CALL GMONIT( 1,IDC+1,AWT1,DWT1,DUMM3)
      CALL GMONIT( 1,IDC+2,AWT2,DWT2,DUMM3)
      CALL GMONIT( 1,IDC+3,AWT3,DWT3,DUMM3)
      CALL GMONIT( 1,IDC+4,AWT4,DWT4,DUMM3)
      CALL GMONIT( 1,IDC+5,AWT5,DWT5,DUMM3)
      CALL GMONIT( 1,IDC+6,AWT6,DWT6,DUMM3)
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '            WINDOW C             '
      WRITE(NOUT,BXTXT) 'Built-in average control weights.'
      WRITE(NOUT,BXTXT) 'Should equal one +- statist. err.'
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL2F) AWT1,DWT1  ,'<WCTA1>            ','      ','C1'
      WRITE(NOUT,BXL2F) AWT2,DWT2  ,'<WCTA2>            ','      ','C2'
      WRITE(NOUT,BXL2F) AWT3,DWT3  ,'<WCTA1*WCTA2>      ','      ','C3'
      WRITE(NOUT,BXL2F) AWT4,DWT4  ,'<WCTB1>            ','      ','C4'
      WRITE(NOUT,BXL2F) AWT5,DWT5  ,'<WCTB2>            ','      ','C5'
      WRITE(NOUT,BXL2F) AWT6,DWT6  ,'<WCTB1*WCTB2>      ','      ','C6'
      WRITE(NOUT,BXCLO)
      ENDIF
*     =====
      END

      SUBROUTINE FILBHW(XPAR,NPAR)
*     ****************************
!----------------------------------------------------------------------!
! In this routine input parameters are set up and stored in            !
! appriopriate COMMON blocks.                                          !
!----------------------------------------------------------------------!
! Last update: Oct. 03  1996                by: W. Placzek             !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  XPAR(*), NPAR(*)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      CHARACTER*80   BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / BHPAR1 / DEL,EPSCMS,THMIN,XMIVIS
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPAR4 / THMIRP,THMARP,THMIRE,THMARE,ENMINP,ENMINE,ACOLLR
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      COMMON / BHCRUD / trmid,crufla,Zprof,sg01,sg02,sg03,sig0
! Communicates with VARRAN
      COMMON / RANPAR / KEYRND
      COMMON / INOUT  / NINP,NOUT
! Commons from BABAMC
      COMMON /INPUT1/EB
      COMMON /INPUT2/XMZ,S2W,XMH,XMT
      COMMON /INPUT3/TMIND,TMAXD,XKMAX
      COMMON /NEVCOM/ NEVT
      COMMON /XK0COM/ XK0
      COMMON /WMXCOM/ WMAX
      COMMON /REJCOM/ IREJEC
      COMMON /UNICOM/IIN,IUT
      COMMON /DBCOM/ IFLDB
! Commons from ALIBABA
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
!--
      SAVE   / INOUT  /, / RANPAR /, / TRANSR /, / BHCRUD /
      SAVE   / BHPAR1 /, / BHPAR2 /, / BHPAR3 /, / BHPAR4 /, / BHPARZ /
      SAVE
      EXTERNAL dsigbo
!--
      CMSENE = XPAR(1)
      THMINP = XPAR(2)
      THMAXP = XPAR(3)
      THMINE = XPAR(4)
      THMAXE = XPAR(5)
      ENMINP = XPAR(6)
      ENMINE = XPAR(7)
      ACOLLI = XPAR(8)
      EPSCMS = XPAR(9)
      KeyOpt = NPAR(1)
      KeyRad = NPAR(2)
      KeyRnd = MOD(KeyOpt,10)
      KeyWgt = MOD(KeyOpt,100)/10
      KeyPia = MOD(KeyRad,10)
      KeyMod = MOD(KeyRad,100)/10
      KeyLib = MOD(KeyRad,1000)/100
      KeyEWC = MOD(KeyRad,10000)/1000
      IF (KeyEWC.GT.0 .AND. KeyPia.EQ.0) THEN
        WRITE(6   ,*)'>> FILBHW: Wrong KeyPia !!!, KeyPia=',KeyPia
        WRITE(6   ,*)'>> For KeyEWC >0, KeyPia must be >0 too!'
        WRITE(NOUT,*)'>> FILBHW: Wrong KeyPia !!!, KeyPia=',KeyPia
        WRITE(NOUT,*)'>> For KeyEWC >0, KeyPia must be >0 too!'
        STOP
      ENDIF
!--------------------------------------------------
! Electron mass and s-variable
      AMEL = 0.5111D-3
      SVAR = CMSENE**2
! Angles in radians
      DEGRAD = PI/180D0
      THMIRP = THMINP *DEGRAD
      THMARP = THMAXP *DEGRAD
      THMIRE = THMINE *DEGRAD
      THMARE = THMAXE *DEGRAD
      ACOLLR = ACOLLI *DEGRAD
! Find the minimum transfer for M.C. generation
      THMIN  = DMIN1(THMIRP,THMIRE)
      Ene    = CMSENE/2D0
      XEMINP = ENMINP/Ene
      XEMINE = ENMINE/Ene
      xmivis = DMIN1(XEMINP,XEMINE)
      AMER   = AMEL/Ene
      beteli = DSQRT(1 - AMER**2)
      betelf = DSQRT(1 - (AMER/xmivis)**2)
! Minimum transfer in LL approx.
      TRMILL = SVAR*xmivis*(1 -beteli*betelf*COS(THMIN))/2 -2*AMEL**2
! Safe value of minimum transfer for MC generation
      trmin  = 0.5*TRMILL
! Find some intermediate transfer (to increase efficiency of MC generation).
! For trmin < tran < trmid, transfer is generated from a flat distribution.
! The choice of trmid and trmin may depend on a given case (CMS energy,
! selection criteria, etc.).
      thmid = DMAX1(THMIRP,THMIRE)
cc    trmid = trmin
cc    trmid = SVAR*xmivis*(1 -beteli*betelf*COS(thmid))/2 -2*AMEL**2
      trmid = SVAR*       (1 -beteli*betelf*COS(thmid))/2 -2*AMEL**2
! Maximum transfer (safe value: TRMAX=SVAR)
      TRMAX = SVAR
      XIMIN = TRMIN/SVAR
      XIMAX = TRMAX/SVAR
! Internal soft photon cut-off (the same as in BHLUMI)
      DEL = EPSCMS*0.01
!---------------------------------------------------------------------
! Masses of top quark and Higgs boson
      AMT = 174.0
      AMH = 300.0
! Z parameters
      AMAZ  = 91.1887
      GAMMZ =  2.50299336
! Weak mixing angle
      SINW2 =  0.22235915
! Weak couplings
      GA = -1/(4*DSQRT(SINW2*(1-SINW2)))
      GV = GA*(1-4*SINW2)
!---------------------------------------------------------------------
! Initialization of ElectroWeak Libraries (virtual+soft corrections)
      IF (KeyLib.EQ.2) THEN
!... ALIBABA routines
! NOTE: The above values of weak parameters are replaced by ones
!       calculated by ALIBABA!
        CALL INITBH(AMAZ,AMH,AMT)
        IWEAK = KeyEWC
! Weak parameters as calculated by ALIBABA
        GAMMZ = ZWID
        SINW2 = SIN2TH
        GA = -1/(4*DSQRT(SINW2*(1-SINW2)))
        GV = GA*(1-4*SINW2)
      ELSEIF (KeyLib.EQ.1) THEN
!... BABAMC routines
        EB  = CMSENE/2
        XMZ = AMAZ
        XGZ = GAMMZ
        XMT = AMT
        XMH = AMH
        S2W = SINW2
        TMIND = DMAX1(THMINP,THMINE)
        TMAXD = DMIN1(THMAXP,THMAXE)
        XKMAX = 1.0
        IIN = 5
        IUT = 6
        IREJEC = 0
        WMAX = 0.01
        IFLDB = 0
        CALL BAREAD
        XK0 = EPSCMS
        GVB = GV*DSQRT(4*PI/ALFINV)
        GAB = GA*DSQRT(4*PI/ALFINV)
        CALL SETUPS(EB,XMZ,XGZ,S2W,XMH,XMT,XK0)
      ELSE
        WRITE(6   ,*)'>> FILBHW: Wrong KeyLib !!!, Keylib=',KeyLib
        WRITE(NOUT,*)'>> FILBHW: Wrong KeyLib !!!, Keylib=',KeyLib
        STOP
      ENDIF
!---------------------------------------------------------------------
! Total Born cross section
      thminb = DMAX1(THMIRE,THMIRP)
      thmaxb = DMIN1(THMARE,THMARP)
      trminb = SVAR/2 *(1 -beteli**2*COS(thminb)) -2*AMEL**2
      trmaxb = SVAR/2 *(1 -beteli**2*COS(thmaxb)) -2*AMEL**2
      eeps = -1d-8
      KeyTem = KeyRad
      KeyRad = 100*KeyLib
      CALL GAUSJD(dsigbo,trminb,trmaxb,eeps,BORNXS)
      KeyRad = KeyTem
!---------------------------------------------------------------------
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHWIDE: INPUT PARAMETRES       '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL1I) KeyOpt,     ' OPTIONS   switch  ','KeyOpt','N1'
      WRITE(NOUT,BXL1I) KeyWgt,     ' weighting switch  ','KeyWgt','  '
      WRITE(NOUT,BXL1I) KeyRnd,     ' rand. numb. switch','KeyRnd','  '
      WRITE(NOUT,BXL1I) KeyRad,     ' RADIATION switch  ','KeyRad','N2'
      WRITE(NOUT,BXL1I) KeyEWC,     ' e-weak cor. switch','KeyEWC','  '
      WRITE(NOUT,BXL1I) KeyLib,     ' EWRC Libary choice','KeyLib','  '
      WRITE(NOUT,BXL1I) KeyMod,     ' QED mat. elm. type','KeyMod','  '
      WRITE(NOUT,BXL1I) KeyPia,     ' vac_pol   switch  ','KeyPia','  '
      WRITE(NOUT,BXL1F) CMSENE,     ' CMS energy   [GeV]','CMSENE','X1'
      WRITE(NOUT,BXL1G) SVAR  ,     ' CMSENE^2   [GeV^2]','SVAR  ','  '
      WRITE(NOUT,BXL1F) THMINP,     ' theta_min_e+ [deg]','THMINP','X2'
      WRITE(NOUT,BXL1F) THMAXP,     ' theta_max_e+ [deg]','THMAXP','X3'
      WRITE(NOUT,BXL1F) THMINE,     ' theta_min_e- [deg]','THMINE','X4'
      WRITE(NOUT,BXL1F) THMAXE,     ' theta_max_e- [deg]','THMAXE','X5'
      WRITE(NOUT,BXL1F) THMIRP,     ' theta_min_e+ [rad]','THMIRP','  '
      WRITE(NOUT,BXL1F) THMARP,     ' theta_max_e+ [rad]','THMARP','  '
      WRITE(NOUT,BXL1F) THMIRE,     ' theta_min_e- [rad]','THMIRE','  '
      WRITE(NOUT,BXL1F) THMARE,     ' theta_max_e- [rad]','THMARE','  '
      WRITE(NOUT,BXL1F) ENMINP,     ' Energy_min_e+[GeV]','ENMINP','X6'
      WRITE(NOUT,BXL1F) ENMINE,     ' Energy_min_e-[GeV]','ENMINE','X7'
      WRITE(NOUT,BXL1F) XEMINP,     ' E_min_e+/E_beam   ','XEMINP','  '
      WRITE(NOUT,BXL1F) XEMINE,     ' E_min_e-/E_beam   ','XEMINE','  '
      WRITE(NOUT,BXL1F) ACOLLI,     ' Acollinearity[deg]','ACOLLI','X8'
      WRITE(NOUT,BXL1F) ACOLLR,     ' Acollinearity[rad]','ACOLLR','  '
      WRITE(NOUT,BXL1G) TRMIN ,     ' trasf_min [GeV^2] ','TRMIN ','  '
      WRITE(NOUT,BXL1G) TRMAX ,     ' trasf_max [GeV^2] ','TRMAX ','  '
      WRITE(NOUT,BXL1G) XIMIN ,     ' xi_min=TRMIN/SVAR ','XIMIN ','  '
      WRITE(NOUT,BXL1G) XIMAX ,     ' xi_max=TRMAX/SVAR ','XIMAX ','  '
      WRITE(NOUT,BXL1G) EPSCMS,     ' eps_CM infr. cut  ','EPSCMS','X9'
      WRITE(NOUT,BXL1G) DEL   ,     ' delta  infr. cut  ','DEL   ','  '
      WRITE(NOUT,BXL1F) AMAZ  ,     ' Z-mass GeV        ','AMAZ  ','  '
      WRITE(NOUT,BXL1F) GAMMZ ,     ' Z-width GeV       ','GAMMZ ','  '
      WRITE(NOUT,BXL1F) SINW2 ,     ' weak mixing angle ','SINW2 ','  '
      WRITE(NOUT,BXL1G) BORNXS,     ' Born xsecion [nb] ','BORNXS','  '
      WRITE(NOUT,BXCLO)
      END

      FUNCTION dsigbo(tran)
*     *********************
! Differential Born cross section dsigma/dt in nb.
      IMPLICIT REAL*8 (a-h,o-z)
      PARAMETER( pi = 3.1415926535897932d0, alfinv=137.03604d0)
      PARAMETER( alfpi=  1/pi/alfinv, alfa= 1d0/alfinv)
      PARAMETER( Gnanob=389.385D3 )
      COMMON / BHPAR2 / CMSENE,AMEL
      SAVE   / BHPAR2 /
!
      s = CMSENE**2
      t = -tran
      u = 4*AMEL**2 -s -t
      dsig0 = xmate0(s,t,u)/(4*pi*alfa)**2
      dsigbo = Gnanob *pi*alfa**2/s**2 *dsig0
      END

      SUBROUTINE gentra(TRMIN,TRMAX,TRAN)
*     ***********************************
!--------------------------------------------------------------!
! Generation of momentum transfer squared TRAN=|Q**2| for      !
! Bhabha scattering according to "crude" distribution.         !
! INPUT:  TRMIN,TRMAX  - min. and max. transfer                !
! OUTPUT: TRAN         - generated transfer                    !
!--------------------------------------------------------------!
! Written by: Wieslaw Placzek              Knoxville, May 1995 !
! Last update: 25.05.1995         by: W.P.                     !
!--------------------------------------------------------------!
      implicit REAL*8 (a-h,o-z)
      COMMON / BHCRUD / trmid,crufla,Zprof,sg01,sg02,sg03,sig0
      SAVE   / BHCRUD /
      REAL*8 drvec(100)
!
      cu01 = sg01/sig0
      cu02 = cu01 +sg02/sig0
      call VARRAN(drvec,2)
      rn1 = drvec(1)
      rn2 = drvec(2)
      IF (rn1.LE.cu01) THEN
         TRAN = (1-rn2)*trmid + rn2*TRMAX
      ELSEIF (rn1.LE.cu02) THEN
         TRAN = 1/ ( (1-rn2)/trmid + rn2/TRMAX )
      ELSE
         TRAN = (1-rn2)*TRMIN + rn2*trmid
      ENDIF
      END

      SUBROUTINE trigMC(wt)
*     *********************
!--------------------------------------------------------------!
! Trigger for wide angle Bhabha scattering: cuts imposed on    !
! positron and electron energies, angles and acollinearity.    !
! OUTPUT: wt  - weight corresponding to the above trigger      !
!--------------------------------------------------------------!
! Written by: Wieslaw Placzek              Knoxville, May 1995 !
! Last update: 19.07.1995         by: W.P.                     !
!--------------------------------------------------------------!
      implicit REAL*8 (a-h,o-z)
      PARAMETER( PI = 3.1415926535897932d0 )
      COMMON / BHPAR4 / THMINP,THMAXP,THMINE,THMAXE,ENMINP,ENMINE,ACOLLI
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      SAVE   / BHPAR4 /, / MOMSET /
      LOGICAL lanp,lane,lenp,lene,laco
!
      wt = 0
! Energy cuts
      lenp = P2(4).gt.ENMINP
      lene = Q2(4).gt.ENMINE
      IF (lenp.AND.lene) THEN
        pmod = SQRT(p2(1)**2+p2(2)**2+p2(3)**2)
        qmod = SQRT(q2(1)**2+q2(2)**2+q2(3)**2)
        costhp = p2(3)/pmod
        costhe =-q2(3)/qmod
! Angular cuts
        lanp = costhp.LE.COS(THMINP) .AND. costhp.GE.COS(THMAXP)
        lane = costhe.LE.COS(THMINE) .AND. costhe.GE.COS(THMAXE)
        IF (lanp.AND.lane) THEN
          pq = p2(1)*q2(1) + p2(2)*q2(2) + p2(3)*q2(3)
          costpe =-pq/(pmod*qmod)
! Acollinearity cut
          laco = costpe.ge.COS(ACOLLI)
          IF (laco) wt = 1
        ENDIF
      ENDIF
      END

      SUBROUTINE REMPHO(EMIN,NPHOT,PHOT,PX,ALF,BET,WTM,MK)
*     ****************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PX(4),PHOT(100,4),ALF(50),BET(50),WTM(50),MK(50)
!
      IF (NPHOT.EQ.0) RETURN
      NPH=NPHOT
      DO 100 J=NPHOT,1,-1
      IF(PHOT(J,4).LT.EMIN) THEN
         DO 60 I=J+1,NPH
         ALF(I-1)=ALF(I)
         BET(I-1)=BET(I)
         WTM(I-1)=WTM(I)
         MK( I-1)=MK( I)
         DO 60 K=1,4
   60    PHOT(I-1,K)=PHOT(I,K)
         NPH=NPH-1
      ENDIF
  100 CONTINUE
!.....................................................
! Correction by Alex Read and Tiziano Camporesi DELPHI
! Date: Fri, 25 Nov 94 17:50:37 WET
!      Code added by ALR 22.11.94 to fix problem with
!      photon handling. Have to erase the discarded
!      photons or they cause occasional problems in
!      MERGIK when merging the PHOT1 and PHOT2 arrays
!      (REMPHO operates on these here).
!
      DO J=NPH+1,NPHOT
         DO K=1,4
           PHOT(J,K) = 0.D0
         ENDDO
      ENDDO
!.....................................................
      NPHOT=NPH
      END

      SUBROUTINE PIATEK(CMSENE,TRMAX,AMEL,EMIN,DELTA,
     $         NPHOT,P1,P2,PHOT,PHSU,WMAT,WTAL,WTMRE,WCTR1,WCTR2)
C     ***************************************************************
c Input:
c        CMSENE         CMS energy
c        TRMAX          maximum transfer
c        AMEL           electron mass (GeV)
c        EMIN           CMS minimum photon energy (GeV)
c        DELTA          MC minimum photon energy (dimensionless)
c        NPHOT          photon number
c        P1,P2(4)       fermion momenta
c        PHOT(100,4)    photon four-momenta
c        PHSU(50)       sum of photon four-momenta
c        WMAT(50)       mass weights from MLTIBR
c Output:
c        WTAL       mass weight for all photons
c        WTMRE      In the case of removal the new mass weight
c        WCTR1      Control weight for delta-->epsilon rejection
c        WCTR2      control weight for photons below EMIN removal
*     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932D0,ALFINV=137.03604D0)
      PARAMETER(ALF1=1/ALFINV/PI )
      DIMENSION P1(4),P2(4),WMAT(50),PHOT(100,4),PHSU(4)
      DATA ICONT /0/
      ICONT=ICONT+1
C Calculate mass weight for all photons and separately for
C photons with energy below/above EMIN
      EPSCM = 2*EMIN/CMSENE
      WTAL = 1D0
      WTM1 = 1D0
      WTM2 = 1D0
      WTEPSP=1D0
      DO 100 I=1,NPHOT
      WTAL = WTAL*WMAT(I)
      IF(WTAL.LT.1D-15) WTAL =0D0
      IF(PHOT(I,4).LT.EMIN) THEN
        WTM1 = WTM1*WMAT(I)
        IF(WTM1.LT.1D-15) WTM1=0D0
        WTEPSP = 0D0
      ELSE
        WTM2 = WTM2*WMAT(I)
        IF(WTM2.LT.1D-15) WTM2=0D0
      ENDIF
  100 CONTINUE
C Control weight for delta-->epsilon  "REJECTION"
      DELT1 = DELTA*BCUD(P1,P2,PHSU)
      CALL WFORM(TRMAX,P1,P2,AMEL,DELT1,EMIN,PDYFS)
      WCTR1 = WTEPSP*PDYFS
C control weight for photons ENE<EMIN  "REMOVAL"
      TRANP = 2D0*(P1(4)*P2(4)-P1(3)*P2(3)-P1(2)*P2(2)-P1(1)*P2(1))
      EPS1  =  SQRT(EMIN**2/P1(4)/P2(4))
      DELB2 = -2*ALF1*(DLOG(TRMAX/TRANP)+1) *DLOG(EPS1/DELT1)
      WCTR2 = WTM1*EXP(-DELB2)
C In the case of removal the new mass weight is this
      WTMRE = WTM2*EXP(DELB2)
      END

      FUNCTION YFSfsu(epsCMS)
*     ***********************
!-------------------------------------------------------------------!
! Yennie-Frautschi-Suura form-factor for Bhabha scattering in CMS   !
! summed over s and u channels (t-channels is already included      !
! in earlier steps of MC algorithm).                                !
! INPUT: epsCMS - soft photon limit in CMS as a fraction            !
!                 of the beam energy                                !
!-------------------------------------------------------------------!
! Written by: Wieslaw Placzek                Knoxville, Oct. 1995   !
! Last update: 04.10.1995           by: W.P.                        !
!-------------------------------------------------------------------!
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER ( pi = 3.1415926535897932D0, alfinv = 137.03604D0 )
      PARAMETER ( alfpi = 1/alfinv/pi )
      COMMON / BHPAR2 / CMSEne,amel
      COMMON / MOMSET / p1(4),q1(4),p2(4),q2(4),phot(100,4),nphot
! Save value of YFSfsu for some further use
      COMMON / sufyfs / fofasu
      SAVE   / BHPAR2 /, / MOMSET /, / sufyfs /
!
      s = CMSEne**2
      Emin = epsCMS*CMSENE/2
      Bslog = LOG(s/amel**2)
! s-channel: initial state
      Rs  = alfpi*( 2*(Bslog -1)*LOG(epsCMS) +0.5*Bslog -1 +pi**2/3 )
! s-channel: final state
      Rs1 = finfra(p2,q2,Emin,amel) +alfpi*pi**2/2
! u-channel
      Ru  = finfra(p1,q2,Emin,amel)
      Ru1 = finfra(q1,p2,Emin,amel)
! form-factor
      fofasu = EXP(Rs + Rs1 - Ru - Ru1)
      YFSfsu = fofasu
      END

      FUNCTION YFSfmf(epsCMS)
*     ***********************
!-------------------------------------------------------------------!
! Total Yennie-Frautschi-Suura form-factor for Bhabha scattering    !
! in CMS.                                                           !
! INPUT: epsCMS - soft photon limit in CMS as a fraction            !
!                 of the beam energy                                !
! NOTE: Function YFSfsu should be called prior this function is     !
!       used. This is done to increase efficiency of MC generation, !
!       as YFSfsu is normally called in the earlier step of MC      !
!       algorithm and it provides a major part of the result.       !
!-------------------------------------------------------------------!
! Written by: Wieslaw Placzek                Knoxville, Oct. 1995   !
! Last update: 05.10.1995           by: W.P.                        !
!-------------------------------------------------------------------!
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER ( pi = 3.1415926535897932D0, alfinv = 137.03604D0 )
      PARAMETER ( alfpi = 1/alfinv/pi )
      COMMON / BHPAR2 / CMSEne,amel
      COMMON / MOMSET / p1(4),q1(4),p2(4),q2(4),phot(100,4),nphot
! Value of YFS formfactor summed over s and u channels from YFSfsu
      COMMON / sufyfs / fofasu
      SAVE   / BHPAR2 /, / MOMSET /, / sufyfs /
!
      Emin = epsCMS*CMSENE/2
! t-channels form-factors
      finftp = finfra(p1,p2,Emin,amel)
      finftq = finfra(q1,q2,Emin,amel)
      fofatp = EXP(finftp)
      fofatq = EXP(finftq)
! Total YFS form-factor
      YFSfmf = fofasu*fofatp*fofatq
      END

      FUNCTION YFSirf(p1,q1,p2,q2,epsCMS,amel)
*     ****************************************
!-------------------------------------------------------------------!
! Yennie-Frautschi-Suura infrared factor in CMS summed over s, t    !
! and u channels (to be subtructed from O(alpha^1) virtual+soft     !
! correction in order to get function beta_0 in YFS formula).       !
! INPUT: p1,q1,p2,q2 - initial and final lepton 4-momenta           !
!        epsCMS      - soft photon limit in CMS as a fraction       !
!                      of the beam energy                           !
!        amel        - electron mass [in GeV]                       !
!-------------------------------------------------------------------!
! Written by: Wieslaw Placzek                Knoxville, Oct. 1995   !
! Last update: 04.10.1995           by: W.P.                        !
!-------------------------------------------------------------------!
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER ( pi = 3.1415926535897932D0, alfinv = 137.03604D0 )
      PARAMETER ( alfpi = 1/alfinv/pi )
      REAL*8 p1(4),q1(4),p2(4),q2(4)
!
      Emin = epsCMS*p1(4)
      s = 2*( p1(4)*q1(4) -p1(3)*q1(3) -p1(2)*q1(2) -p1(1)*q1(1) )
      Bslog = LOG(s/amel**2)
! infrared factors for s, t, u channels
      BBs = alfpi*( 2*(Bslog -1)*LOG(epsCMS) +0.5*Bslog -1 +pi**2/3 )
      BBt = finfra(p1,p2,Emin,amel)
      BBu = finfra(p1,q2,Emin,amel)
! total infrared factor
      YFSirf = 2*(BBs + BBt - BBu)
      END

      FUNCTION finfra(p1,p2,Emin,amel)
*     ********************************
!-------------------------------------------------------------------!
! Yennie-Frautschi-Suura infrared factor (virtual + real soft)      !
! in CMS for t and u channels, for any pair of electrons            !
! and/or positrons.                                                 !
! To get the result for the s-channel one has to add to the result  !
! of this function only the constant term: (alpha/pi)*(pi**2/2).    !
! INPUT: p1,p2 - leptons four-momenta                               !
!        Emin  - minimum energy of hard photons in CMS [in GeV]     !
!        amel  - electron mass [in GeV]                             !
!-------------------------------------------------------------------!
! Written by: Wieslaw Placzek                Knoxville, Oct. 1995   !
! Last update: 04.10.1995           by: W.P.                        !
!-------------------------------------------------------------------!
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER ( pi = 3.1415926535897932D0, alfinv = 137.03604D0 )
      PARAMETER ( alfpi = 1/alfinv/pi )
      REAL*8 p1(4),p2(4)
!
      E1 = p1(4)
      E2 = p2(4)
      QQ = 2*( p1(4)*p2(4) -p1(3)*p2(3) -p1(2)*p2(2) -p1(1)*p2(1) )
      Biglog = LOG(QQ/amel**2)
      eps2 = Emin**2/E1/E2
      om = E1 + E2
      de = E1 - E2
      Dt = SQRT(QQ + de**2)
      Root = (Biglog -1)*LOG(eps2) +0.5*Biglog -1
! At low angles Remn should be approximately equal to:
!           -pi**2/6,  for s, u channels;
!            0,        for t channel.
      Remn = pi**2/3 -0.5*LOG(E2/E1)**2
     &      -0.25*LOG( (Dt+de)**2/(4*E1*E2) )**2
     &      -0.25*LOG( (Dt-de)**2/(4*E1*E2) )**2
     &      -DILOGY( (Dt+om)/(Dt+de) ) -DILOGY( (Dt+om)/(Dt-de) )
     &      -DILOGY( (Dt-om)/(Dt+de) ) -DILOGY( (Dt-om)/(Dt-de) )
      finfra = alfpi*(Root + Remn)
      END

      SUBROUTINE WFORM(TRMAX,Q1,Q2,AMF,DELTA,EMIN,DYFS)
C     *************************************************
C For tests only.
C Yennie-Frautschi-Suura Formfactors for the single fermion pair
C This is for crude distribition before mass weights
C The triangle effect included (pi**2/6)
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(PI=3.1415926535897932D0,ALFINV=137.03604D0)
      PARAMETER(ALF1=1/ALFINV/PI )
      DIMENSION Q1(4),Q2(4)
C ...Momenta q1,q2 should be in CMS
      Q1Q2  = Q1(4)*Q2(4)-Q1(3)*Q2(3)-Q1(2)*Q2(2)-Q1(1)*Q2(1)
      E1 = Q1(4)
      E2 = Q2(4)
      BETF2 = 2*ALF1* DLOG(TRMAX /AMF**2)
      DELB  = BETF2*DLOG(EMIN/SQRT(E1*E2)/DELTA)
      EP    = E1+E2
      EM    = E1-E2
      DL    = SQRT( 2*Q1Q2 +EM**2 )
C Note that approximately REMN= +(1./6.)*PI**2 for t-channel
      REMN  = PI**2/2
     $        -0.50*DLOG(E1/E2)**2
     $        -0.25*DLOG((DL+EM)**2/(4*E1*E2))**2
     $        -0.25*DLOG((DL-EM)**2/(4*E1*E2))**2
     $        - DILOGY((DL+EP)/(DL+EM)) -DILOGY((DL-EP)/(DL-EM))
     $        - DILOGY((DL-EP)/(DL+EM)) -DILOGY((DL+EP)/(DL-EM))
C This (alf/pi)*pi**2/6 is related to replacing (y+z)>epsilon
C by max(y,z)>epsilon.   (Rejection delta=> epsilon over-estimated)
      TRIANG = -PI**2/6D0
      DYFS   = EXP( DELB +ALF1*REMN +ALF1*TRIANG)
      END

      FUNCTION BCUD(P1,P2,SF)
*     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P1(4),P2(4),SF(4)
      XPP  = P2(4)*P1(4)-P2(3)*P1(3)-P2(2)*P1(2)-P2(1)*P1(1)
      XPR  = P1(4)*(P2(4)+SF(4)) - P1(3)*(P2(3)+SF(3))
     $     - P1(2)*(P2(2)+SF(2)) - P1(1)*(P2(1)+SF(1))
      BCUD= XPR/XPP
      END

      SUBROUTINE MLTIBR(TRAN,TRMAX,AMEL,DEL,
     $      NPH,PHOT,PHSU,ALF1,BET1,TRANP,AMSP,MK,WT1,WTM)
*     ****************************************************
* This provides momenta of photons in a fermion proper frame
C Input : TRAN    = principal t-channel transfer     (GEV**2)
C         TRMAX   = max. transf. (>0) for angular phot. dist. [GEV**2]
C         AMEL    = electron energy         (GEV)
C         DEL     = low energy photon limit   (dimensionless)
C Output: NPH     = photon multiplicity
C         PHOT    = list of photon four-momenta
C         PHSU    = sum of photon momenta
C         ALF1,BET1   = Sudakov variables
C         TRANP   = (P2-P1)**2
C         AMSP    = (P2+PHSU)**2
C         MK      = marked photons
C         WT1     = TRANP/TRAN is Jacobian, =0 outside ph.sp.
C         WTM     = list of mass weights
*     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (PI=3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER (NMAX=20)
      DIMENSION PHOT(100,4),PHSU(4),PH(4)
      DIMENSION MK(50),WTM(50)
      DIMENSION ALF(50),BET(50),ALF1(50),BET1(50),Y(50),Z(50)
      DIMENSION RR(100),P2(4)
      DOUBLE PRECISION DRVEC(100)
      DATA ICONT /0/
      ICONT=ICONT+1

      DELS  = AMEL**2/TRMAX
      BILGS = LOG(TRMAX/AMEL**2)
      DELL  = LOG(DEL)
      WT1    = 1D0
      DO  11 I=1,50
      DO  11 K=1,4
   11 PHOT(I,K)=0D0
      AVERG=2D0/(PI*ALFINV)*BILGS*LOG(1D0/DEL)
      CALL POISSG(AVERG,NMAX,NPH,RR)
      IF(NPH.GT.NMAX) GOTO 900
* No photon
      DO 45 K=1,4
   45 PHSU(K)=0D0
      IF(NPH.EQ.0) THEN
        TRANP=TRAN
      ELSE
* One or more photons
   50   CALL VARRAN(DRVEC,NPH)
        BSUM=0D0
        DO 80 I=1,NPH
* We define R=LOG(MAX(YGR,ZET))
        R=DELL*RR(I)
* Photons close to lower infrared boundry are marked for tests
        MK(I)=0
        IF(EXP(R).LT.DEL*3D0) MK(I)=1
        T= 2D0*DRVEC(I)
        IF(T.GT.1D0) THEN
           YGR=R
           ZET=R-(T-1D0)*BILGS
        ELSE
           ZET=R
           YGR=R-T*BILGS
        ENDIF
        YGR=EXP(YGR)
        ZET=EXP(ZET)
        Y(I)=YGR
        Z(I)=ZET
* Define ALPHA and BETA (prim)
        ALF1(I)=YGR-ZET*DELS
        BET1(I)=ZET-YGR*DELS
        IF(ALF1(I).LE.0D0.OR.BET1(I).LE.0D0) GOTO 50
   80   BSUM=BSUM+BET1(I)
        IF(BSUM.GT.1D0) GOTO 800
* Rescale ALPHA and BETA
        CALL VARRAN(DRVEC,NPH)
        DO 90 I=1,NPH
        ALF(I)=ALF1(I)/(1D0-BSUM)
   90   BET(I)=BET1(I)/(1D0-BSUM)
* Define photon four momenta in SQRT(TRANP)/2 units
        DO 100 I=1,NPH
        PHOT(I,4)= ALF(I)+BET(I)
        PHOT(I,3)=-ALF(I)+BET(I)
        R1 = DRVEC(I)
        PHI=2D0*PI*R1
        PTRAN=2D0*DSQRT(DABS(ALF(I)*BET(I)))
        PHOT(I,1)=PTRAN*COS(PHI)
        PHOT(I,2)=PTRAN*SIN(PHI)
        DO 100 K=1,4
  100   PHSU(K)=PHSU(K)+PHOT(I,K)
* Define factor for rescaling photon momenta
        XMK2=PHSU(4)**2-PHSU(3)**2-PHSU(2)**2-PHSU(1)**2
        YY2=1D0/(1D0+PHSU(3)-.25D0*XMK2)
* YY2 negative when outside phase space (abs(t)>abs(s))
        IF(YY2.LE.0D0) GOTO 900
        TRANP=TRAN*YY2
        ENER =SQRT(TRANP)/2D0
* RESCALE ALL PHOTON MOMENTA
        DO 120 K=1,4
        PHSU(K)=PHSU(K)*ENER
        DO 120 I=1,NPH
  120   PHOT(I,K)=PHOT(I,K)*ENER
* This rotation makes PHSU(2)=0
* (we get rid her of dummy angle, see "poprawka" in the notes)
        PSIT=ANGFI(PHSU(1),PHSU(2))
        CALL ROTOD3(-PSIT, PHSU,PHSU)
        DO 140 I=1,NPH
        DO 135 K=1,4
  135   PH(K)=PHOT(I,K)
        CALL ROTOD3(-PSIT, PH,PH)
        DO 140 K=1,4
  140   PHOT(I,K)=PH(K)
      ENDIF
c+++      IF(TRANP.EQ.0D0) GO TO 900
      IF(TRANP.LE. 4*AMEL**2) GO TO 900
      BETEL=SQRT(1D0-4D0*AMEL**2/(TRANP+4D0*AMEL**2))
      P2(3)=SQRT(TRANP)/2D0
      P2(4)=SQRT(P2(3)*P2(3)+AMEL**2)
      P2(2)=0D0
      P2(1)=0D0
      AMSP=(P2(4)+PHSU(4))**2-(P2(3)+PHSU(3))**2
     $    -(P2(2)+PHSU(2))**2-(P2(1)+PHSU(1))**2
* And the weight finally
      WT1 = TRANP/TRAN
      DELT=AMEL**2/TRANP
      DO 200 I=1,NPH
* Generated distribution
C here some numerical regularization
      DIST0 = 1D0/((ALF1(I)+DELS*BET1(I))*(BET1(I)+DELS*ALF1(I)))
      YGR=ALF1(I)+DELT*BET1(I)
      ZET=BET1(I)+DELT*ALF1(I)
* Desired distribution = soft factor
      DIST1 = ALF1(I)*BET1(I)/(YGR*ZET)**2
      WTM(I)= DIST1/DIST0
  200 CONTINUE
      RETURN
C Event outside phase space
C Note that distinction is made (TRANP=-2,-1) to facilitate tests
c event dropped due to: sum(beta) > 1
 800  CONTINUE
      TRANP = -1D0
      WT1   =  0D0
      RETURN
c event dropped due to: tranp < m^2, or earlier because YY2 < 0
 900  CONTINUE
      TRANP = -2D0
      WT1   =  0D0
      END

      SUBROUTINE POISSG(AVERG,NMAX,MULT ,RR)
C     **************************************
* DIFFERS FROM THAT IN EXPAND DEC. 87
* THIS GENERATES PHOTON MULTIPLICITY ACCORDING TO POISSON DISTRIBUTION
* INPUT:  AVERG = AVERAGE MULTIPLICITY
*         NMAX  = MAXIMUM MULTIPLICITY
* OUTPUT: MULT  = GENERATED MULTIPLICITY
*         RR(1:100) LIST OF ORDERED UNIFORM RANDOM NUMBERS,
*         A BYPRODUCT RESULT, TO BE EVENTUALLY USED FOR SOME FURTHER
*         PURPOSE (I.E.  GENERATION OF PHOTON ENERGIES).
*     **************************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 RR(100)
      DOUBLE PRECISION DRVEC(100)
      SAVE NFAIL
      DATA NFAIL/0/
   50 NN=0
      IF(NMAX.GT.100) GOTO 900
      CALL VARRAN(DRVEC,NMAX)
      SUM=0D0
      DO 100 IT=1,NMAX
      RN = DRVEC(IT)
      Y= LOG(RN)
      SUM=SUM+Y
      NN=NN+1
      IF(SUM.LT.-AVERG) GOTO 130
      RR(NN)=SUM/(-AVERG)
  100 CONTINUE
      NFAIL=NFAIL+1
      IF(NFAIL.GT.100) GOTO 900
      GOTO 50
  130 MULT =NN-1
      RETURN
  900 WRITE(6,*) ' POISSG: TO SMALL OR TO BIG NMAX',NMAX
      STOP
      END

      SUBROUTINE KINO4(SVAR,TRAN,AMEL,AMSP,AMSQ,WTKK)
*     ************************************************
* Kinematics, construction of momenta in CMS
*     ************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI =3.1415926535897932D0)
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / PSIPHI / TH1,EXT1,EXB1,PSI1,EXW1,
     $                  TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI
      SAVE   / MOMS1  /, / MOMS2  /, / MOMZ1  /, / MOMZ2  /, /PSIPHI/
      DOUBLE PRECISION DRVEC(100)
      REAL*8 PH(4)
      DIMENSION QCM(4)

      BTEL=DSQRT(1D0-4D0*AMEL**2/SVAR)
      WTKK=1D0
* Three azimuthal angles
      CALL VARRAN(DRVEC,3)
      PSI1= 2D0*PI*DRVEC(1)
      PSI2= 2D0*PI*DRVEC(2)
      PHI = 2D0*PI*DRVEC(3)
* Upper vertex: transf. from P2-P1 proper frame
      CALL KLIPER(TRANP,AMEL,PHSU1,P2,TH1,EXT1,EXB1)
* Lower vertex: transf. from q2-q1 proper frame
      CALL KLIPER(TRANQ,AMEL,PHSU2,Q2,TH2,EXT2,EXB2)
* Define P1, Q1  in central QMS
      P1(3)= -(TRAN+AMSP-AMEL**2)/SQRT(TRAN)/2D0
      Q1(3)=  (TRAN+AMSQ-AMEL**2)/SQRT(TRAN)/2D0
      RPQK=(Q1(3)+P1(3))/DSQRT(SVAR)
* Correcting for electron mass
C     PX2=SVAR*(SVAR+4D0*P1(3)*Q1(3))/((Q1(3)+P1(3))**2+SVAR)/4D0
C     PX2=PX2-AMEL**2
      GPQK= P1(3)-Q1(3)
      PX2=(BTEL**2*SVAR*(1D0+RPQK*RPQK)-GPQK*GPQK)/(1D0+RPQK*RPQK)/4D0
      IF(PX2.LE.0D0) GOTO 900
      PX=SQRT(PX2)
      P1(2)=  0D0
      Q1(2)=  0D0
      P1(1)=  -PX
      Q1(1)=   PX
      P1(4)=  SQRT(P1(1)**2+P1(2)**2+P1(3)**2+AMEL**2)
      Q1(4)=  SQRT(Q1(1)**2+Q1(2)**2+Q1(3)**2+AMEL**2)
* Correcting for electron mass
C     BETP = SQRT(1D0-(AMEL/P1(4))**2)
C     BETQ = SQRT(1D0-(AMEL/Q1(4))**2)
C     DO 7 K=1,3
C     P1(K)=BETP* P1(K)
C   7 Q1(K)=BETQ* Q1(K)
      EXW1=SQRT((P1(4)+P1(1))/(P1(4)-P1(1)))
      EXW2=SQRT((Q1(4)+Q1(1))/(Q1(4)-Q1(1)))
* Construct momentum transfer Q in CMS
      QCM(4)=(AMSP-AMSQ)/SQRT(SVAR)/2D0
      QMOD=SQRT(TRAN+QCM(4)**2)
      QCM(3)=(-TRAN-AMSP/2D0-AMSQ/2D0+AMEL**2)/SQRT(SVAR-4D0*AMEL**2)
      QCM(2)=0D0
      QCM(1)=SQRT(QMOD**2-QCM(3)**2)
      FIF =ANGFI(QCM(3),QCM(1))
      EXE2=SQRT((QMOD+QCM(4))/(QMOD-QCM(4)))

* Final set of transformations from QMSP and QMSQ to CMS
* First branch, tranformed are P2, PHSU1, PHOT1
      CALL  PTRAL(TH1,EXT1,EXB1,PSI1,EXW1,EXE2,FIF,PHI,P2)
      IF(NPHOT1.NE.0) THEN
       CALL PTRAL(TH1,EXT1,EXB1,PSI1,EXW1,EXE2,FIF,PHI,PHSU1)
       DO 20 I=1,NPHOT1
       DO 15 K=1,4
   15  PH(K)=PHOT1(I,K)
       CALL PTRAL(TH1,EXT1,EXB1,PSI1,EXW1,EXE2,FIF,PHI,PH)
       DO 16 K=1,4
   16  PHOT1(I,K)=PH(K)
   20  CONTINUE
      ENDIF
* Second branch, tranformed are Q2, PHSU2, PHOT2
      CALL  QTRAL(TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI,Q2)
      IF(NPHOT2.NE.0) THEN
       CALL QTRAL(TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI,PHSU2)
       DO 30 I=1,NPHOT2
       DO 25 K=1,4
   25  PH(K)=PHOT2(I,K)
       CALL QTRAL(TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI,PH)
       DO 26 K=1,4
   26  PHOT2(I,K)=PH(K)
   30  CONTINUE
      ENDIF
* Finally, beams P1 and Q1
      CALL BOSTD3(EXE2,P1,P1)
      CALL ROTOD2( FIF,P1,P1)
      CALL BOSTD3(EXE2,Q1,Q1)
      CALL ROTOD2( FIF,Q1,Q1)
      RETURN
* Event outside phase space
  900 WTKK=0D0
      END

      SUBROUTINE PTRAL(TH,EXT,EXB,PSI,EXW,EXE,FIF,PHI,P)
*     **************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 P(4)
      CALL ROTOD2( -TH, P, P)
      CALL BOSTD3( EXT, P, P)
      CALL BOSTD1( EXB, P, P)
      CALL ROTOD3( PSI, P, P)
      CALL BOSTD1( EXW, P, P)
      CALL BOSTD3( EXE, P, P)
      CALL ROTOD2( FIF, P, P)
      CALL ROTOD3( PHI, P, P)
      END

      SUBROUTINE QTRAL(TH,EXT,EXB,PSI,EXW,EXE,FIF,PHI,P)
*     **************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI =3.1415926535897932D0)
      REAL*8 P(4)
      CALL ROTOD2( -TH, P, P)
      CALL BOSTD3( EXT, P, P)
      CALL BOSTD1( EXB, P, P)
      CALL ROTOD3( PSI, P, P)
      CALL ROTOD2(  PI, P, P)
      CALL BOSTD1( EXW, P, P)
      CALL BOSTD3( EXE, P, P)
      CALL ROTOD2( FIF, P, P)
      CALL ROTOD3( PHI, P, P)
      END

      SUBROUTINE KLIPER(TRANP,AMEL,PHSUM,P2,TH,EXT,EXB)
*     **************************************************
* Deals with Lorentz transf. from QQ1 to QQ frame
* where QQ1=P2-P1, QQ=P2+PHSUM-P1, TRANP=QQ1**2, P1**2=P2**2=AMEL**2
* Input: TRANP,AMEL,PHSUM
* Output: P2,TH,EXT,EXB,PHSUM
* Here, TH, EXT, EXB are transformation params.
*     **************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PHSUM(4),P2(4)
      REAL*8 P1(4),QQ1(4)

      BETEL=SQRT(1D0-4D0*AMEL**2/(TRANP+4D0*AMEL**2))
* No photon
      IF(PHSUM(4).EQ.0D0) THEN
        P2(3)= SQRT(TRANP)/2D0
        P2(4)= SQRT(P2(3)*P2(3)+AMEL**2)
        P2(2)=0D0
        P2(1)=0D0
        TH =0D0
        EXT=1D0
        EXB=1D0
      ELSE
* One photon or more
        ENER1=SQRT(TRANP)/2D0

        P1(1)=0D0
        P1(2)=0D0
        P1(3)=-ENER1
        P1(4)= SQRT(P1(3)*P1(3)+AMEL**2)

        P2(1)=0D0
        P2(2)=0D0
        P2(3)= ENER1
        P2(4)= SQRT(P2(3)*P2(3)+AMEL**2)

        DO 33 I=1,4
  33    QQ1(I)=P2(I)+PHSUM(I)-P1(I)

* Rotation 2 puts QQ1 paralel to axis 3
* Note that  PHSUM(2)=0 is already assured in MLTIBR!
        TH  =ANGFI(QQ1(3),QQ1(1))
        CALL ROTOD2(-TH ,QQ1,QQ1)
        CALL ROTOD2(-TH ,P1,P1)
* Boost 3 puts QQ1(4)=0
        EXT = SQRT((QQ1(3)-QQ1(4))/(QQ1(3)+QQ1(4)))
        CALL BOSTD3( EXT ,QQ1,QQ1)
        CALL BOSTD3( EXT , P1, P1)
        EXB = SQRT((P1(4)-P1(1))/(P1(4)+P1(1)))
CC Testing obsolete appendix
CC Boost 1 puts P1 antiparallel to axis 3
CC      CALL ROTOD2( -TH , P2, P2)
CC      CALL BOSTD3( EXT , P2, P2)
CC      CALL BOSTD1( EXB , P2, P2)
      ENDIF
      END

      SUBROUTINE MERGIK
*     *****************
* Transfer momenta and mark into proper commons
* photons ordered according to cms energy
* (the hardest in the first position)
!----------------------------------------------
!--- Modified by W. Placzek, Sept. 1995
!----------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / BHPAR2 / CMSENE,AMEL
      SAVE   / BHPAR2 /
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / MOMSET / PX1(4),QX1(4),PX2(4),QX2(4),PHOT(100,4),NPHOT
      COMMON / MARPKP / MARKP(100)
      SAVE   / MOMS1/,/ MOMS2/,/ MOMZ1/,/ MOMZ2/,/ MOMSET/,/ MARPKP /
!WP: photon radiation tags
      Common / RadTag / idrad(100)
      SAVE   / RadTag /
      REAL*8 phx(4),pk(4)
!
      NPHOT=NPHOT1+NPHOT2
      I1=1
      I2=1
      DO 207 I=1,NPHOT
      IF(PHOT1(I1,4).GT.PHOT2(I2,4)) THEN
         DO 205 K=1,4
  205    PHOT( I,K)=PHOT1(I1,K)
         MARKP(I)  =  MK1(I1)
!WP: initial state radiation (positron line)
         IF (al1(I1)-be1(I1).gt.0) THEN
           idrad(I) = 1
!WP: final state radiation (positron line)
         ELSE
           idrad(I) = 3
         ENDIF
         I1=I1+1
      ELSE
         DO 206 K=1,4
  206    PHOT( I,K)=PHOT2(I2,K)
         MARKP(I)  =  MK2(I2)
!WP: initial state radiation (electron line)
         IF (al2(I2)-be2(I2).gt.0) THEN
           idrad(I) = 2
!WP: final state radiation (electron line)
         ELSE
           idrad(I) = 4
         ENDIF
         I2=I2+1
      ENDIF
  207 CONTINUE
      DO 300 K=1,4
      PX1(K)=P1(K)
      PX2(K)=P2(K)
      QX1(K)=Q1(K)
      QX2(K)=Q2(K)
  300 CONTINUE
!WP: balance initial-final energy-momentum when some photons removed
      DO k = 1,4
        phx(k) = PX2(k) + QX2(k)
        DO i = 1,NPHOT
          phx(k) = phx(k) + PHOT(i,k)
        ENDDO
      ENDDO
      ECMSf = DSQRT( phx(4)**2 -phx(3)**2 -phx(2)**2 -phx(1)**2 )
      IF (CMSENE-ECMSf.gt.1d-6) THEN
        CALL BOSTDQ( 1,phx,PX2,PX2)
        CALL BOSTDQ( 1,phx,QX2,QX2)
        refa = CMSENE/ECMSf
        PX2(4) = PX2(4)*refa
        QX2(4) = QX2(4)*refa
        p2m  = DSQRT( PX2(1)**2 + PX2(2)**2 + PX2(3)**2 )
        q2m  = DSQRT( QX2(1)**2 + QX2(2)**2 + QX2(3)**2 )
        p2mr = DSQRT( PX2(4)**2 - AMEL**2 )
        q2mr = DSQRT( QX2(4)**2 - AMEL**2 )
        DO k = 1,3
          PX2(k) = PX2(k)*p2mr/p2m
          QX2(k) = QX2(k)*q2mr/q2m
        ENDDO
        DO i = 1,NPHOT
          DO k = 1,4
            pk(k) = PHOT(i,k)
          ENDDO
          CALL BOSTDQ( 1,phx,pk,pk)
          DO k = 1,4
            PHOT(i,k) = pk(k)*refa
          ENDDO
        ENDDO
      ENDIF
      END

      SUBROUTINE WTinte(wt)
*     *********************
!---------------------------------------------------------------!
! This routine provides a weight for restoring up-down          !
! interference in YFS forrmula for Bhabha scattering.           !
! OUTPUT: wt  - the up-down interference weight                 !
!---------------------------------------------------------------!
! Written by: Wieslaw Placzek              Knoxville, May 1995  !
! Last update: 23.05.1995       by: W.P.                        !
!---------------------------------------------------------------!
      implicit REAL*8 (a-h,o-z)
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / MOMSET / p1(4),q1(4),p2(4),q2(4),phot(100,4),nphot
      SAVE  / BHPAR2 /, / MOMSET /
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      save   / MOMZ1  /, / MOMZ2 /
!
      wt = 1
      if (nphot.EQ.0) RETURN
      ame = AMEL
      p1q1 = p1(4)*q1(4)-p1(3)*q1(3)-p1(2)*q1(2)-p1(1)*q1(1)
      p1p2 = p1(4)*p2(4)-p1(3)*p2(3)-p1(2)*p2(2)-p1(1)*p2(1)
      p1q2 = p1(4)*q2(4)-p1(3)*q2(3)-p1(2)*q2(2)-p1(1)*q2(1)
      q1p2 = q1(4)*p2(4)-q1(3)*p2(3)-q1(2)*p2(2)-q1(1)*p2(1)
      q1q2 = q1(4)*q2(4)-q1(3)*q2(3)-q1(2)*q2(2)-q1(1)*q2(1)
      p2q2 = p2(4)*q2(4)-p2(3)*q2(3)-p2(2)*q2(2)-p2(1)*q2(1)
      DO i = 1,nphot
! Scalar products of photon 4-momentum with other 4-momenta
        p1k = p1(4)*phot(i,4) -p1(3)*phot(i,3)
     &       -p1(2)*phot(i,2) -p1(1)*phot(i,1)
        q1k = q1(4)*phot(i,4) -q1(3)*phot(i,3)
     &       -q1(2)*phot(i,2) -q1(1)*phot(i,1)
        p2k = p2(4)*phot(i,4) -p2(3)*phot(i,3)
     &       -p2(2)*phot(i,2) -p2(1)*phot(i,1)
        q2k = q2(4)*phot(i,4) -q2(3)*phot(i,3)
     &       -q2(2)*phot(i,2) -q2(1)*phot(i,1)
! Soft factors S-tilde (factor alpha/4pi^2 omitted!)
! Upper-line only
        sfu = 2*p1p2/(p1k*p2k) -(ame/p1k)**2 -(ame/p2k)**2
! Lower-line only
        sfl = 2*q1q2/(q1k*q2k) -(ame/q1k)**2 -(ame/q2k)**2
! Interference terms
        sfi = 2*( p1q1/(p1k*q1k) -p1q2/(p1k*q2k)
     &           -q1p2/(q1k*p2k) +p2q2/(p2k*q2k) )
! Weight
        wt = wt *( 1 + sfi/(sfu+sfl) )
      ENDDO
      END

C==================================================================
C======================== MODEL====================================
C==================================================================
      SUBROUTINE MODEL(MODE,WTM)
*     ****************************
! Interface to various models for hard bremsstrhlung matrix element
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / INOUT  / NINP,NOUT
      SAVE   / INOUT  /, / BHPAR3 /

      KEYMOD = MOD(KEYRAD,100)/10
      IF (KEYMOD.EQ.1) THEN
        CALL MODEL1(MODE)
        WTM = WTSET(  1)
      ELSEIF (KEYMOD.EQ.2) THEN
        CALL MODEL2(MODE)
        WTM = WTSET(101)
      ELSE
CC JVW
C        WRITE(NOUT,*) " +++++ MODEL: wrong keymod=",KEYMOD
        WRITE(NOUT,*)' +++++ MODEL: wrong keymod=',KEYMOD
CC JVW
        STOP
      ENDIF
      END

      SUBROUTINE DUMPS(NOUT)
*     **********************
* THIS PRINTS OUT FOUR MOMENTA OF PHOTONS
* ON OUTPUT UNIT NOUT
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      SAVE   / MOMSET /
      REAL*8 SUM(4)
      WRITE(NOUT,*) '=====================DUMPS===================='
      WRITE(NOUT,3100) ' P2',(P2(K),K=1,4)
      WRITE(NOUT,3100) ' Q2',(Q2(K),K=1,4)
      DO 100 I=1,NPHOT
  100 WRITE(NOUT,3100) 'PHO',(PHOT(I,K),K=1,4)
      DO 200 K=1,4
  200 SUM(K)=P2(K)+Q2(K)
      DO 210 I=1,NPHOT
      DO 210 K=1,4
  210 SUM(K)=SUM(K)+PHOT(I,K)
      WRITE(NOUT,3100) 'SUM',(SUM(K),K=1,4)
 3100 FORMAT(1X,A3,1X,5F18.13)
      END

      SUBROUTINE DUMPR(NUNIT,WORD,PP,QQ)
C     **********************************
C 15 Jan 90 (SJ)
C prints twice dot-products of two four momentum PP and QQ
C more precisely:   2*PP.QQ  and  (PP+QQ).(PP+QQ)
C     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8 WORD
      REAL*8 PP(4),QQ(4)
      DOT1=2*(PP(4)*QQ(4)-PP(3)*QQ(3)-PP(2)*QQ(2)-PP(1)*QQ(1))
      DOT2=(PP(4)+QQ(4))**2-(PP(3)+QQ(3))**2
     $    -(PP(2)+QQ(2))**2-(PP(1)+QQ(1))**2
      WRITE(NUNIT,'(1X,A8,5(1X,F20.10))') WORD,DOT1,DOT2
      END

      SUBROUTINE GIBEA(CMSENE,AMEL,P1,P2)
C     ***********************************
C 15 Jan 90 (SJ)
C this originates from yfs302
C GIVEN CMS ENERGY (CMSENE) DEFINES BEAM MOMENTA IN CMS
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P1(*),P2(*)
      BETEL=SQRT(1D0-4D0*AMEL**2/CMSENE**2)
      P1(1)=  0D0
      P1(2)=  0D0
      P1(3)=  CMSENE/2D0*BETEL
      P1(4)=  CMSENE/2D0
      P2(1)=  0D0
      P2(2)=  0D0
      P2(3)= -CMSENE/2D0*BETEL
      P2(4)=  CMSENE/2D0
C----------------------------------------------------------------C
C                      The end of BHWID1                         C
C----------------------------------------------------------------C
      END


      SUBROUTINE BHWIDE(MODE,XPAR,NPAR)
*     *********************************
!                                                                      !
!   BBBBBBB   BBB   BBB  BBB       BBB  BBB  BBBBBBB    BBBBBBB        !
!   BBB  BBB  BBB   BBB  BBB       BBB  BBB  BBB  BBB   BBB            !
!   BBB  BBB  BBB   BBB  BBB       BBB  BBB  BBB   BBB  BBB            !
!   BBBBBBB   BBBBBBBBB  BBB       BBB  BBB  BBB   BBB  BBBBBB         !
!   BBBBBBB   BBBBBBBBB  BBB   B   BBB  BBB  BBB   BBB  BBBBBB         !
!   BBB  BBB  BBB   BBB  BBB  BBB  BBB  BBB  BBB   BBB  BBB            !
!   BBBBBBBB  BBB   BBB  BBBBBB BBBBBB  BBB  BBB  BBB   BBB            !
!   BBBBBBB   BBB   BBB  BBBBB    BBBB  BBB  BBBBBBB    BBBBBBB        !
!                                                                      !
!======================================================================!
!======================================================================!
!======================================================================!
!===============             B H W I D E            ===================!
!======================================================================!
!======================================================================!
!=============== MONTE CARLO FOR WIDE-ANGLE BHABHA ====================!
!===============            VERSION 1.01            ===================!
!======================================================================!
!====================      September 1996     =========================!
!======================================================================!
!======================================================================!
!=======================     AUTHORS      =============================!
!============  S. Jadach, W. Placzek, B.F.L. Ward  ====================!
!======================================================================!
!======================================================================!
!                                                                      !
! BHWIDE is based on the following papers:                             !
! [1] S. Jadach, W. Placzek and B.F.L. Ward, UTHEP-95-1001 (Oct. 1995) !
!     hep-ph/9608412; submitted to Phys. Lett. B.                      !
! [2] S. Jadach and B.F.L. Ward,                                       !
!     Phys. Rev. D40 (1989) 3582.                                      !
! [3] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,              !
!     Comput. Phys. Commun. 70 (1992) 305; TH-6230, Sept. 1991.        !
! [4] M. Bohm, A. Denner and W. Hollik,                                !
!     Nucl. Phys. B304 (1988) 687.                                     !
! [5] F.A. Berends, R. Kleiss and W. Hollik,                           !
!     Nucl. Phys. B304 (1988) 712.                                     !
! [6] W. Beenakker, F.A. Berends and S.C. van der Marck,               !
!     Nucl. Phys. B349 (1991) 323.                                     !
!                                                                      !
!                  IMPORTANT NOTE                                      !
!                  --------------                                      !
! The user is kindly requested to cite at least ref. [1].              !
!----------------------------------------------------------------------!
!                        INPUT and OUTPUT                              !
!----------------------------------------------------------------------!
! All input and output goes through parameters in                      !
!                 CALL BHWIDE(MODE,XPAR,NPAR)                          !
! and through /MOMSET/ and /WGTALL/ common blocks.                     !
! In the following we shall  briefly indicate the meaning of the       !
! above parameters/variables.                                          !
!                                                                      !
! IF( MODE =-1 ) THEN                                                  !
! ===================                                                  !
! Initialization is performed, all input parameters are transfered     !
! through XPAR and NPAR.                                               !
!----------------------------------------------------------------------!
!  Entry    Variable   Meaning                                         !
!----------------------------------------------------------------------!
!  NPAR( 1)  KeyOpt =10*KeyWgt +KeyRnd                                 !
!                    General option switch                             !
!            KeyRnd =1,2 type of random number generator RANMAR,RANECU !
!            KeyWgt =0,1 for constant, variable weight WTM             !
!  NPAR( 2)  KeyRad =1000*KeyEWC +100*KeyLib +10*KeyMod + KeyPia       !
!                    is option switch for ElectroWeak Radiative Corr.; !
!                    see tables in BHWID1 for more details.            !
!  XPAR( 1)  CMSENE Total center mass energy [GeV]                     !
!  XPAR( 2)         see tables in BHWID1,                              !
!  XPAR( 3)         see tables in BHWID1,                              !
!  XPAR( 4)         see tables in BHWID1,                              !
!  XPAR( 5)         see tables in BHWID1,                              !
!  XPAR( 6)         see tables in BHWID1,                              !
!  XPAR( 7)         see tables in BHWID1,                              !
!  XPAR( 8)         see tables in BHWID1,                              !
!  XPAR( 9)         see tables in BHWID1,                              !
!----------------------------------------------------------------------!
!                                                                      !
! ELSE IF( MODE = 0 ) THEN                                             !
! ========================                                             !
! Generation of the single Monte Carlo event                           !
! The four momenta of the final state electron positron and photon     !
! are encoded in                                                       !
!      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT     !
! where P1 and Q1 are four-momenta of positron and elecron beams,      !
! P2 and Q2 are four-momenta of outgoing positron and electron,        !
! PHOT(100,4) contains list of photon four-momenta                     !
! and NPHOT is the number of real photons in PHOT.                     !
! NOTE: +z axis points along incoming e+.                              !
!                                                                      !
! For weighted events it may be profitable to use "paralel weights"    !
! from                                                                 !
!      COMMON / WGTALL / WTMOD,WTCRUD,WTTRIG,WTSET(300)                !
! where WTMOD is the principal model weight and another useful weights !
! representing some interesting version of the matrix element          !
! can be constructed as WT= WTCRU1*WTCRU2*WTSET(J).                    !
!                                                                      !
! ELSE IF( MODE = 1 ) THEN                                             !
! ========================                                             !
! The total cross section corresponding to generated series of event,  !
! i.e. resulting from MC integrartion is calculated and stored in XPAR !
! and NPAR.                                                            !
! In the table below we describe their most essential entries.         !
!----------------------------------------------------------------------!
!  Entry    Variable   Meaning                                         !
!----------------------------------------------------------------------!
!  NPAR(10)  NEVGEN  Number of generated MC events                     !
!  NPAR(20)  NEVGEN  Number of generated MC events                     !
!  XPAR(10)    XSEC  Total x-section [nb]                              !
!  XPAR(11)   RXSEC  The relative (statistical) error of XSEC          !
!  XPAR(20)          Crude total MC x-section [nb] which is necessary  !
!                    for rescaling histograms in run                   !
!                    with weighted events.                             !
!  XPAR(21)          =0, error of XPAR(20) is zero                     !
!----------------------------------------------------------------------!
! For constant weight option KEYWGT=0 (convevience in rescaling histos)!
! we put XPAR(20,21)=XPAR(10,11)                                       !
! For MODE=1 program is called upon many times in the process of       !
! rescaling histograms and therefore no output is printed.             !
!                                                                      !
! ELSE IF( MODE = 2 ) THEN                                             !
! ========================                                             !
! Only in this MODE=2 in addition to filling XPAR and NPAR             !
! (as for MODE=1)                                                      !
! the values of various x-sections are printed on the standard         !
! output file.                                                         !
!                                                                      !
! ENDIF                                                                !
! ====                                                                 !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*80    BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      DIMENSION  XPAR(*),NPAR(*)
      COMMON / INOUT  / NINP,NOUT
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      SAVE   / INOUT  /, / BHPAR3 /
      SAVE   NEVG

      IF (MODE.EQ.-1) THEN
CB        NINP = 15
CB        NOUT = 16
        DO i = 1,2
          IF (i.EQ.1) THEN
            IOUT = NOUT
          ELSE
            IOUT = 6
            if ( nout.eq.6) go to 10
          ENDIF
          WRITE(IOUT,BXOPE)
          WRITE(IOUT,BXTXT) '   '
          WRITE(IOUT,BXTXT) 'BBB   B  B  B    B  B  BBB   BBBB'
          WRITE(IOUT,BXTXT) 'B  B  B  B  B    B  B  B  B  B   '
          WRITE(IOUT,BXTXT) 'BBB   BBBB  B    B  B  B  B  BBB '
          WRITE(IOUT,BXTXT) 'B  B  B  B  B BB B  B  B  B  B   '
          WRITE(IOUT,BXTXT) 'BBB   B  B  BB  BB  B  BBB   BBBB'
          WRITE(IOUT,BXTXT) '   '
          WRITE(IOUT,BXTXT) '*******************************'
          WRITE(IOUT,BXTXT) '*      BHWIDE version 1.01    *'
          WRITE(IOUT,BXTXT) '* M.C. for wide-angle Bhabha  *'
          WRITE(IOUT,BXTXT) '*   September 1995   (1.00)   *'
          WRITE(IOUT,BXTXT) '*   September 1996   (1.01)   *'
          WRITE(IOUT,BXTXT) '*           AUTHORS           *'
          WRITE(IOUT,BXTXT) '*    S. Jadach, W. Placzek,   *'
          WRITE(IOUT,BXTXT) '*          B.F.L. Ward        *'
          WRITE(IOUT,BXTXT) '*******************************'
          WRITE(IOUT,BXCLO)
!
          WRITE(IOUT,BXOPE)
          WRITE(IOUT,BXTXT) 'This program is based on papers '
          WRITE(IOUT,BXTXT) '--------------------------------'
          WRITE(IOUT,BXTXT) 'UTHEP-95-1001; hep-ph/9608412.  '
          WRITE(IOUT,BXTXT) 'Phys. Rev. D40 (1989) 3582.     '
          WRITE(IOUT,BXTXT) 'Comp. Phys. Comm. 70 (1992) 305.'
          WRITE(IOUT,BXCLO)
        ENDDO
 10   continue 
!
        NEVG = 0
        CALL BHWID1(-1,XPAR,NPAR)
      ELSEIF (MODE.EQ.0) THEN
        NEVG = NEVG + 1
        CALL BHWID1( 0,XPAR,NPAR)
! Clean final state common blocks if necessary (safety reason)
        CALL BHCLEN
      ELSE
        CALL BHWID1(MODE,XPAR,NPAR)
      ENDIF
      END

      SUBROUTINE BHCLEN
*     *****************
! This routine prevents user from using zero weight events
! and parallel weights when they should not be used!
*     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRUD,WTTRIG,WTSET(300)
      SAVE   / MOMSET /, / WGTALL /

! Parallel weights should not be used for constant weight events.
      IF(WTMOD.EQ.1D0) THEN
        DO I = 1,200
          WTSET(I) = 0D0
        ENDDO
! Clean final state momenta for events outside phase space
      ELSEIF(WTCRUD*WTTRIG.EQ.0D0)  THEN
        DO K = 1,4
          P2(K)=0D0
          Q2(K)=0D0
        ENDDO
        NPHOT=0
        DO J = 1,100
          DO K = 1,4
            PHOT(J,K) = 0D0
          ENDDO
        ENDDO
      ENDIF
      END

      FUNCTION xmatel(p1,q1,p2,q2,pk)
*     *******************************
!----------------------------------------------------------------------!
! This function provides a value of the matrix element squared for     !
! Bhabha scattering including single hard photon radiation.            !
!                                                                      !
!         e+(p1) + e-(q1) ---> e+(p2) + e-(q2) + gamma(pk)             !
!                                                                      !
! where p1, q1, p2, q2, pk  are 4-momenta of corresponding particles.  !
!                                                                      !
! INPUT:  p1(4),q1(4),p2(4),q2(4),pk(4) - 4-momenta of the particles,  !
!                                       all FERMIONS must be MASSIVE!  !
!                                                                      !
! Note: Vacuum polarization corrections are included in the above      !
!       matrix element as Dyson-type factors at the level of the       !
!       helicity amplitudes calculation.                               !
!----------------------------------------------------------------------!
! Written by: Wieslaw Placzek                 Knoxville, May 1995      !
! Last update: 07.02.1996      by: W. P.                               !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      REAL*8 p1(4) ,q1(4) ,p2(4) ,q2(4) ,pk(4)
      REAL*8 p1r(4),q1r(4),p2r(4),q2r(4),pkr(4)
      COMPLEX*16 amp(12)
!
! Rescaling 4-momenta of all particles for the massless limit
      pmod = SQRT(p2(1)**2 +p2(2)**2 +p2(3)**2)
      qmod = SQRT(q2(1)**2 +q2(2)**2 +q2(3)**2)
      refa = ( pmod + qmod + pk(4) )/( p1(4) + q1(4) )
      DO i=1,4
         p1r(i) = p1(i)
         q1r(i) = q1(i)
         p2r(i) = p2(i)/refa
         q2r(i) = q2(i)/refa
         pkr(i) = pk(i)/refa
      ENDDO
      p1r(3) = p1r(4)
      q1r(3) =-q1r(4)
      p2r(4) = pmod/refa
      q2r(4) = qmod/refa
! Matrix element squared for massless spinors
      CALL amphel(p1r,q1r,p2r,q2r,pkr,amp)
      suma = 0
      DO i=1,12
         suma = suma + amp(i)*DCONJG(amp(i))
      ENDDO
      xmem0 = suma/4
! Mass correction terms (here fermions must be massive!)
      xmemc = xmelmc(p1,q1,p2,q2,pk)
! Total matrix element squared
      xmatel = xmem0 + xmemc
      END

      SUBROUTINE amphel(p1,q1,p2,q2,pk,amp)
*     *************************************
!----------------------------------------------------------------------!
! This routine calculates helicity amplitude for the Bhabha scattering !
! process with sigle photon radiation:                                 !
!                                                                      !
!    e+(p1,l1) + e-(q1,l2) ---> e+(p2,l3) + e-(q2,l4) + gamma(pk,l5)   !
!                                                                      !
! where p1, q1, p2, q2, pk  are 4-momenta of corresponding particles,  !
! while l1, l2, l3, l4, l5 are their helicities.                       !
! Note: Both gamma and Z exchange diagrams are included.               !
!                                                                      !
! INPUT:  p1(4),q1(4),p2(4),q2(4),pk(4) - 4-momenta of the particles,  !
!                                      all PARTICLES must be MASSLESS! !
! OUTPUT: amp(12) - complex-type array of 12 helicity amplitudes,      !
!                   amp(i) = M(l1,l2,l3,l4,l5);                        !
!                                                                      !
!          amp( 1) = M(+++++),     amp( 2) = M(++++-)                  !
!          amp( 3) = M(----+),     amp( 4) = M(-----)                  !
!          amp( 5) = M(-++-+),     amp( 6) = M(-++--)                  !
!          amp( 7) = M(+--++),     amp( 8) = M(+--+-)                  !
!          amp( 9) = M(+-+-+),     amp(10) = M(+-+--)                  !
!          amp(11) = M(-+-++),     amp(12) = M(-+-+-)                  !
!                                                                      !
! Note: Vacuum polarization corrections are included in the above      !
!       helicity aplitudes as Dyson-type factors.                      !
!----------------------------------------------------------------------!
! Written by: Wieslaw Placzek                 Knoxville, May 1995      !
! Last update: 08.02.1996      by: W. P.                               !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      PARAMETER( ALFPI=  1D0/PI/ALFINV ,ALFA=1D0/ALFINV)
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      SAVE   / BHPAR3 /, / BHPARZ /
      REAL*8 p1(4),q1(4),p2(4),q2(4),pk(4)
      COMPLEX*16 amp(12)
      COMPLEX*16 Fspinp,Gspinp
      COMPLEX*16 F1 ,F2 ,F3 ,F4 ,F5 ,F6 ,G1 ,G2 ,G3 ,G4
      COMPLEX*16 F1s,F2s,F3s,F4s,F5s,F6s,G1s,G2s,G3s,G4s
      COMPLEX*16 c,Rs,gs,zs,gs1,zs1
      SAVE e,c,ale,ari,alar,ale2,ari2,ifset
      DATA ifset /0/
! Statement functions for propagator factors
      Rs(a,gs,zs) = gs + a*zs
      Rt(a,gt,zt) = gt + a*zt
!
! Set up some constants
      IF (ifset.EQ.0) THEN
        ifset = 1
! Electric charge
        e = SQRT(4*PI*ALFA)
        c = DCMPLX(0D0,2D0*DSQRT(2d0)*DBLE(e)**3)
! Left and right coupling constant for Z exchange
        ale  = GV + GA
        ari  = GV - GA
        alar = ale*ari
        ale2 = ale**2
        ari2 = ari**2
      ENDIF
! Mandelstam variables
      s  = 2*( p1(4)*q1(4)-p1(3)*q1(3)-p1(2)*q1(2)-p1(1)*q1(1) )
      s1 = 2*( p2(4)*q2(4)-p2(3)*q2(3)-p2(2)*q2(2)-p2(1)*q2(1) )
      tp =-2*( p1(4)*p2(4)-p1(3)*p2(3)-p1(2)*p2(2)-p1(1)*p2(1) )
      tq =-2*( q1(4)*q2(4)-q1(3)*q2(3)-q1(2)*q2(2)-q1(1)*q2(1) )
      u  =-2*( p1(4)*q2(4)-p1(3)*q2(3)-p1(2)*q2(2)-p1(1)*q2(1) )
      u1 =-2*( q1(4)*p2(4)-q1(3)*p2(3)-q1(2)*p2(2)-q1(1)*p2(1) )
! gamma and Z propagator factors
      CALL profas(s ,gs ,zs )
      CALL profas(s1,gs1,zs1)
      CALL profat(tp,gtp,ztp)
      CALL profat(tq,gtq,ztq)
! Functions of spinor products
      F1 = Fspinp(p1,q1)
      F2 = Fspinp(p2,q2)
      F3 = Fspinp(p1,p2)
      F4 = Fspinp(q1,q2)
      F5 = Fspinp(p1,q2)
      F6 = Fspinp(q1,p2)
      G1 = Gspinp(p1,p2,q1,q2,pk)/tp
      G2 = Gspinp(q2,q1,p1,p2,pk)/tq
      G3 = Gspinp(q2,p2,p1,q1,pk)/s1
      G4 = Gspinp(p1,q1,p2,q2,pk)/s
      F1s= DCONJG(F1)
      F2s= DCONJG(F2)
      F3s= DCONJG(F3)
      F4s= DCONJG(F4)
      F5s= DCONJG(F5)
      F6s= DCONJG(F6)
      G1s= DCONJG(G1)
      G2s= DCONJG(G2)
      G3s= DCONJG(G3)
      G4s= DCONJG(G4)
! Helicity amplitudes
      amp( 1) = -c*s *F1 *( Rt(alar,gtp,ztp)*G1  +Rt(alar,gtq,ztq)*G2  )
      amp( 2) = -c*s1*F2s*( Rt(alar,gtp,ztp)*G1s +Rt(alar,gtq,ztq)*G2s )
      amp( 3) = -c*s1*F2 *( Rt(alar,gtp,ztp)*G1  +Rt(alar,gtq,ztq)*G2  )
      amp( 4) = -c*s *F1s*( Rt(alar,gtp,ztp)*G1s +Rt(alar,gtq,ztq)*G2s )
      amp( 5) = -c*tq*F4 *( Rs(alar,gs1,zs1)*G3  +Rs(alar,gs ,zs )*G4  )
      amp( 6) = -c*tp*F3s*( Rs(alar,gs1,zs1)*G3s +Rs(alar,gs ,zs )*G4s )
      amp( 7) = -c*tp*F3 *( Rs(alar,gs1,zs1)*G3  +Rs(alar,gs ,zs )*G4  )
      amp( 8) = -c*tq*F4s*( Rs(alar,gs1,zs1)*G3s +Rs(alar,gs ,zs )*G4s )
      amp( 9) =  c*u *F5 *( Rt(ale2,gtp,ztp)*G1  +Rt(ale2,gtq,ztq)*G2
     $                     +Rs(ale2,gs1,zs1)*G3  +Rs(ale2,gs ,zs )*G4  )
      amp(10) =  c*u1*F6s*( Rt(ale2,gtp,ztp)*G1s +Rt(ale2,gtq,ztq)*G2s
     $                     +Rs(ale2,gs1,zs1)*G3s +Rs(ale2,gs ,zs )*G4s )
      amp(11) =  c*u1*F6 *( Rt(ari2,gtp,ztp)*G1  +Rt(ari2,gtq,ztq)*G2
     $                     +Rs(ari2,gs1,zs1)*G3  +Rs(ari2,gs ,zs )*G4  )
      amp(12) =  c*u *F5s*( Rt(ari2,gtp,ztp)*G1s +Rt(ari2,gtq,ztq)*G2s
     $                     +Rs(ari2,gs1,zs1)*G3s +Rs(ari2,gs ,zs )*G4s )
      END

      SUBROUTINE profas(s,gs,zs)
*     **************************
!----------------------------------------------------------------------!
! This subroutine privides values of gamma and Z propagator factors    !
! (i.e. s*propagator) for the s-channel exchange.                      !
! Various forms depending on the input parameters KeyEWC and KeyLib.   !
!----------------------------------------------------------------------!
! Written by: Wielaw Placzek                    Knoxville, Sep. 1995   !
! Last update: 25.09.1996         by: W. P.                            !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      COMMON / INOUT  / NINP,NOUT
      SAVE   / BHPAR3 /, / BHPARZ /, / INOUT  /
      COMPLEX*16 gs,zs,ags,azs
      REAL*8 IMSIGG
!
      KeyEWC = MOD(KeyRad,10000)/1000
      KeyLib = MOD(KeyRad,1000)/100
      KeyPia = MOD(KeyRad,10)
! Vacuum polarization
      CALL vacpol(KeyPia,s,SINW2,RePis,dRePis)
      IF (KeyEWC.EQ.0) THEN
        gs = 1/(1+RePis)
! Constant Z width
        zs = s/DCMPLX(s-AMAZ**2,GAMMZ*AMAZ) /(1+RePis)
      ELSEIF (KeyEWC.EQ.1) THEN
        IF (KeyLib.eq.2) THEN
! Propagators from ALIBABA
          CALL sprogz(s,ags,azs)
!WP          gs = s*ags
! Photon propagator: real part from vacpol, imaginary part from ALIBABA
          gsRe = 1 + RePis
          gsIm = IMSIGG(s)/s
          gs = 1/DCMPLX(gsRe,gsIm)
! Z propagator (from ALIBABA)
          zs = s*azs
        ELSEIF (KeyLib.EQ.1) THEN
          gs = 1/(1+RePis)
! Running Z width
          zs = s/DCMPLX(s-AMAZ**2,s*GAMMZ/AMAZ) /(1+RePis)
        ELSE
          WRITE(6   ,*)'>> profas: Wrong KeyLib !!!, KeyLib=',KeyLib
          WRITE(NOUT,*)'>> profas: Wrong KeyLib !!!, KeyLib=',KeyLib
          STOP
        ENDIF
      ELSE
        WRITE(6   ,*)'>> profas: Wrong KeyEWC !!!, KeyEWC=',KeyEWC
        WRITE(NOUT,*)'>> profas: Wrong KeyEWC !!!, KeyEWC=',KeyEWC
        STOP
      ENDIF
      END

      SUBROUTINE profat(t,gt,zt)
*     **************************
!----------------------------------------------------------------------!
! This subroutine privides values of gamma and Z propagator factors    !
! (i.e. t*propagator) for the t-channel exchange.                      !
! Various forms depending on the input parameters KeyEWC and KeyLib.   !
!----------------------------------------------------------------------!
! Written by: Wielaw Placzek                    Knoxville, Sep. 1995   !
! Last update: 25.09.1996         by: W. P.                            !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      COMMON / INOUT  / NINP,NOUT
      SAVE   / BHPAR3 /, / BHPARZ /, / INOUT  /
!
      KeyEWC = MOD(KeyRad,10000)/1000
      KeyLib = MOD(KeyRad,1000)/100
      KeyPia = MOD(KeyRad,10)
! Vacuum polarization
      CALL vacpol(KeyPia,t,SINW2,RePit,dRePit)
      IF (KeyEWC.EQ.0) THEN
        gt = 1/(1+RePit)
        zt = t/(t-AMAZ**2) /(1+RePit)
      ELSEIF (KeyEWC.EQ.1) THEN
        IF (KeyLib.EQ.2) THEN
! Propagators from ALIBABA
          CALL tprogz(t,agt,azt)
!WP          gt  = t*agt
          gt  = 1/(1+RePit)
          zt  = t*azt
        ELSEIF (KeyLib.EQ.1) THEN
          gt = 1/(1+RePit)
          zt = t/(t-AMAZ**2) /(1+RePit)
        ELSE
          WRITE(6   ,*)'>> profat: Wrong KeyLib !!!, KeyLib=',KeyLib
          WRITE(NOUT,*)'>> profat: Wrong KeyLib !!!, KeyLib=',KeyLib
          STOP
        ENDIF
      ELSE
        WRITE(6   ,*)'>> profat: Wrong KeyEWC !!!, KeyEWC=',KeyEWC
        WRITE(NOUT,*)'>> profat: Wrong KeyEWC !!!, KeyEWC=',KeyEWC
        STOP
      ENDIF
      END

      COMPLEX*16 FUNCTION Fspinp(p,q)
*     *******************************
!----------------------------------------------------------------------!
! This functions gives the result of the following expression          !
! of spinor products:                                                  !
!                                <pq>                                  !
!                       F(p,q) = -----                                 !
!                                <pq>*                                 !
! where the spinor product <pq> is defined in Ref. Zhan Xu et al.,     !
! Nucl. Phys. B291 (1987) 392; <pq>* is a complex conjugate of <pq>.   !
!                                                                      !
! INPUT: p(4), q(4) - two 4-vectors                                    !
!----------------------------------------------------------------------!
! Written by: Wieslaw Placzek                     Knoxville, May 1995  !
! Last update: 03.05.1995       by: W. P.                              !
!----------------------------------------------------------------------!
      REAL*8 p(4),q(4)
      COMPLEX*16 spipro,pxq
! Spinor product
      pxq = spipro(p,q)
      Fspinp = pxq/DCONJG(pxq)
      END

      COMPLEX*16 FUNCTION Gspinp(p,q,r,s,k)
*     *************************************
!----------------------------------------------------------------------!
! This functions gives the result of the following expression          !
! of spinor products:                                                  !
!                                  <pq>*                               !
!                       F(p,q) = --------                              !
!                                <rk><ks>                              !
! where the spinor products <..> are defined in Ref. Zhan Xu et al.,   !
! Nucl. Phys. B291 (1987) 392; <pq>* is a complex conjugate of <pq>.   !
!                                                                      !
! INPUT: p(4), q(4), r(4), s(4), k(4) - five 4-vectors                 !
!----------------------------------------------------------------------!
! Written by: Wieslaw Placzek                     Knoxville, May 1995  !
! Last update: 03.05.1995       by: W. P.                              !
!----------------------------------------------------------------------!
      REAL*8 p(4),q(4),r(4),s(4),k(4)
      COMPLEX*16 spipro,pxq,rxk,kxs
! Spinor products
      pxq = spipro(p,q)
      rxk = spipro(r,k)
      kxs = spipro(k,s)
      Gspinp = DCONJG(pxq)/(rxk*kxs)
      END

      COMPLEX*16 FUNCTION spipro(p,q)
*     *******************************
!----------------------------------------------------------------------!
! This functions gives the value of the spinor product <pq> = <p-|q+>  !
! as defined in Ref. Zhan Xu et al., Nucl. Phys. B291 (1987) 392.      !
! INPUT: p(4), q(4) - two 4-vectors                                    !
!----------------------------------------------------------------------!
! Written by: Wieslaw Placzek                     Knoxville, May 1995  !
! Last update: 08.05.1995       by: W. P.                              !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      REAL*8 p(4),q(4)
      COMPLEX*16 pt,qt,eip,eiq
!
      pp = p(4)+p(3)
      pm = p(4)-p(3)
      pta= SQRT(pp*pm)
      IF (pta.gt.0) THEN
         pt = DCMPLX(p(1),p(2))
         eip = pt/pta
      ELSE
         eip = 1
      ENDIF
      qp = q(4)+q(3)
      qm = q(4)-q(3)
      qta= SQRT(qp*qm)
      IF (qta.gt.0) THEN
         qt = DCMPLX(q(1),q(2))
         eiq = qt/qta
      ELSE
         eiq = 1
      ENDIF
! Spinor product
      spipro = SQRT(pm*qp)*eip - SQRT(pp*qm)*eiq
      END

      FUNCTION xmelmc(p1,q1,p2,q2,pk)
*     *******************************
!----------------------------------------------------------------------!
! This function provides a value of the finite fermion mass correction !
! to the matrix element for Bhabha scattering including hard photon    !
! radiation:                                                           !
!                                                                      !
!         e+(p1) + e-(q1) ---> e+(p2) + e-(q2) + gamma(pk)             !
!                                                                      !
! where p1, q1, p2, q2, pk  are 4-momenta of corresponding particles.  !
!                                                                      !
! INPUT:  p1(4),q1(4),p2(4),q2(4),pk(4) - 4-momenta of the particles,  !
!                                       all FERMIONS must be MASSIVE!  !
!                                                                      !
! Note: Vacuum polarization corrections are included in the above      !
!       matrix element as Dyson-type factors.                          !
!----------------------------------------------------------------------!
! Written by: Wieslaw Placzek                 Knoxville, May 1995      !
! Last update: 08.05.1995      by: W. P.                               !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      PARAMETER( ALFA=1D0/ALFINV)
      COMMON / BHPAR2 / CMSENE,AMEL
      SAVE   / BHPAR2 /
      REAL*8 p1(4),q1(4),p2(4),q2(4),pk(4)
!
      ame = AMEL
! Electric charge squared
      e2 = 4*PI*ALFA
! Mandelstam variables
      s  = (p1(4)+q1(4))**2 - (p1(3)+q1(3))**2
     $    -(p1(2)+q1(2))**2 - (p1(1)+q1(1))**2
      s1 = (p2(4)+q2(4))**2 - (p2(3)+q2(3))**2
     $    -(p2(2)+q2(2))**2 - (p2(1)+q2(1))**2
      tp = (p1(4)-p2(4))**2 - (p1(3)-p2(3))**2
     $    -(p1(2)-p2(2))**2 - (p1(1)-p2(1))**2
      tq = (q1(4)-q2(4))**2 - (q1(3)-q2(3))**2
     $    -(q1(2)-q2(2))**2 - (q1(1)-q2(1))**2
      u  = (p1(4)-q2(4))**2 - (p1(3)-q2(3))**2
     $    -(p1(2)-q2(2))**2 - (p1(1)-q2(1))**2
      u1 = (q1(4)-p2(4))**2 - (q1(3)-p2(3))**2
     $    -(q1(2)-p2(2))**2 - (q1(1)-p2(1))**2
! Scalar products of photon 4-momentum with other 4-momenta
      p1k = p1(4)*pk(4)-p1(3)*pk(3)-p1(2)*pk(2)-p1(1)*pk(1)
      q1k = q1(4)*pk(4)-q1(3)*pk(3)-q1(2)*pk(2)-q1(1)*pk(1)
      p2k = p2(4)*pk(4)-p2(3)*pk(3)-p2(2)*pk(2)-p2(1)*pk(1)
      q2k = q2(4)*pk(4)-q2(3)*pk(3)-q2(2)*pk(2)-q2(1)*pk(1)
! Mass correction terms
      xmelmc = -e2*( (ame/p1k)**2 *xmate0(s1,tq,u1)
     $              +(ame/q1k)**2 *xmate0(s1,tp,u )
     $              +(ame/p2k)**2 *xmate0(s ,tq,u )
     $              +(ame/q2k)**2 *xmate0(s ,tp,u1) )
      END

      FUNCTION xmate0(s,t,u)
*     **********************
!----------------------------------------------------------------------!
! This function provides a value of the lowest order matrix element    !
! squared for Bhabha scattering.                                       !
! INPUT:  s, t, u - Mandelstam varriables                              !
! Note: Vacuum polarization corrections are included in the above      !
!       matrix element as Dyson-type factors.                          !
!----------------------------------------------------------------------!
! Written by: Wieslaw Placzek                 Knoxville, May 1995      !
! Last update: 08.02.1996      by: W. P.                               !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      PARAMETER( ALFA=1D0/ALFINV)
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      SAVE   / BHPAR3 /, / BHPARZ /
      COMPLEX*16 Rs,Rsl2,Rsl2s,Rsr2,Rsr2s,Rslr,Rslrs,gs,zs
      SAVE e4,ale,ari,alar,ale2,ari2,ifset
      DATA ifset /0/
! Statement functions for propagator factors
      Rs(a,gs,zs) = gs + a*zs
      Rt(a,gt,zt) = gt + a*zt
!
! Set up some constants
      IF (ifset.EQ.0) THEN
        ifset = 1
! Electric charge: e**4
        e4= (4*PI*ALFA)**2
! left and right coupling constant for Z exchange
        ale  = GV + GA
        ari  = GV - GA
        alar = ale*ari
        ale2 = ale**2
        ari2 = ari**2
      ENDIF
! gamma and Z propagator factors
      CALL profas(s,gs,zs)
      CALL profat(t,gt,zt)
! Combinations of the above factors
      Rsl2 = Rs(ale2,gs,zs)
      Rsl2s= DCONJG(Rsl2)
      Rsr2 = Rs(ari2,gs,zs)
      Rsr2s= DCONJG(Rsr2)
      Rslr = Rs(alar,gs,zs)
      Rslrs= DCONJG(Rslr)
      Rtl2 = Rt(ale2,gt,zt)
      Rtr2 = Rt(ari2,gt,zt)
      Rtlr = Rt(alar,gt,zt)
! Various (real) combinations of the above factors
      Asl2 = Rsl2*Rsl2s
      Asr2 = Rsr2*Rsr2s
      Aslr = Rslr*Rslrs
      Atl2 = Rtl2*Rtl2
      Atr2 = Rtr2*Rtr2
      Atlr = Rtlr*Rtlr
      Astl = DBLE(Rsl2s)*Rtl2
      Astr = DBLE(Rsr2s)*Rtr2
! Matrix element
      xmate0 = e4*( ( (Asl2+Asr2)*u**2 + 2*Aslr*t**2 )/s**2
     $            + ( (Atl2+Atr2)*u**2 + 2*Atlr*s**2 )/t**2
     $            + 2*(Astl+Astr)*u**2/s/t )
      END

      FUNCTION xmecal(p1,q1,p2,q2,pk)
*     *******************************
!----------------------------------------------------------------------!
! This function provides a value of the matrix element squared for     !
! Bhabha scattering including single hard photon radiation as given    !
! in Ref. (CALKUL) F.A. Berends et al., Nucl. Phys. B206 (1982) 61.    !
!                                                                      !
!         e+(p1) + e-(q1) ---> e+(p2) + e-(q2) + gamma(pk)             !
!                                                                      !
! where p1, q1, p2, q2, pk  are 4-momenta of corresponding particles.  !
!                                                                      !
! INPUT:  p1(4),q1(4),p2(4),q2(4),pk(4) - 4-momenta of the particles,  !
!                                       all FERMIONS must be MASSIVE!  !
!                                                                      !
! Note: Vacuum polarization corrections are included in the above      !
!       matrix element as Dyson-type factors.                          !
!----------------------------------------------------------------------!
! Written by: Wieslaw Placzek                 Knoxville, May 1995      !
! Last update: 08.02.1996      by: W. P.                               !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      PARAMETER(ALFA=1D0/ALFINV)
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      SAVE   / BHPAR3 /, / BHPARZ /
      REAL*8 p1(4) ,q1(4) ,p2(4) ,q2(4) ,pk(4)
      REAL*8 p1r(4),q1r(4),p2r(4),q2r(4),pkr(4)
      COMPLEX*16 Asa2,Asa2c,Asb2,Asb2c,Asab,Asabc
      COMPLEX*16 As1a2,As1a2c,As1b2,As1b2c,As1ab,As1abc
      COMPLEX*16 As,gs,zs,gs1,zs1
      SAVE e6,a,b,a2,b2,ab,ifset
      DATA ifset /0/
! Statement function for propagator factors
      As(a,gs,zs) = gs + 4*a*zs
      At(a,gt,zt) = gt + 4*a*zt
!
! Set up some constants
      IF (ifset.eq.0) THEN
        ifset = 1
! Electric charge: e**6
        e6 = (4*PI*ALFA)**3
! Z coupling constans
        a = (GV+GA)/2
        b = (GV-GA)/2
        a2= a*a
        b2= b*b
        ab= a*b
      ENDIF
! Rescaling 4-momenta of all particles for the massless limit (tests)
cc      pmod = DSQRT(p2(1)**2+p2(2)**2+p2(3)**2)
cc      qmod = DSQRT(q2(1)**2+q2(2)**2+q2(3)**2)
cc      refa = ( pmod + qmod + pk(4) )/( p1(4) + q1(4) )
      refa = 1
      DO i=1,4
         p1r(i) = p1(i)
         q1r(i) = q1(i)
         p2r(i) = p2(i)/refa
         q2r(i) = q2(i)/refa
         pkr(i) = pk(i)/refa
      ENDDO
cc      p1r(3) = p1r(4)
cc      q1r(3) =-q1r(4)
cc      p2r(4) = pmod/refa
cc      q2r(4) = qmod/refa
! Mandelstam variables
      s  = 2*( p1r(4)*q1r(4)-p1r(3)*q1r(3)-p1r(2)*q1r(2)-p1r(1)*q1r(1) )
      s1 = 2*( p2r(4)*q2r(4)-p2r(3)*q2r(3)-p2r(2)*q2r(2)-p2r(1)*q2r(1) )
      tp =-2*( p1r(4)*p2r(4)-p1r(3)*p2r(3)-p1r(2)*p2r(2)-p1r(1)*p2r(1) )
      tq =-2*( q1r(4)*q2r(4)-q1r(3)*q2r(3)-q1r(2)*q2r(2)-q1r(1)*q2r(1) )
      u  =-2*( p1r(4)*q2r(4)-p1r(3)*q2r(3)-p1r(2)*q2r(2)-p1r(1)*q2r(1) )
      u1 =-2*( q1r(4)*p2r(4)-q1r(3)*p2r(3)-q1r(2)*p2r(2)-q1r(1)*p2r(1) )
! Scalar products of photon 4-momentum with other 4-momenta
      p1k = p1r(4)*pkr(4)-p1r(3)*pkr(3)-p1r(2)*pkr(2)-p1r(1)*pkr(1)
      q1k = q1r(4)*pkr(4)-q1r(3)*pkr(3)-q1r(2)*pkr(2)-q1r(1)*pkr(1)
      p2k = p2r(4)*pkr(4)-p2r(3)*pkr(3)-p2r(2)*pkr(2)-p2r(1)*pkr(1)
      q2k = q2r(4)*pkr(4)-q2r(3)*pkr(3)-q2r(2)*pkr(2)-q2r(1)*pkr(1)
! gamma and Z propagator factors
      CALL profas(s ,gs ,zs )
      CALL profas(s1,gs1,zs1)
      CALL profat(tp,gtp,ztp)
      CALL profat(tq,gtq,ztq)
! Propagator factors
      Asa2  = As(a2,gs,zs)
      Asa2c = DCONJG(Asa2)
      Asb2  = As(b2,gs,zs)
      Asb2c = DCONJG(Asb2)
      Asab  = As(ab,gs,zs)
      Asabc = DCONJG(Asab)
      As1a2 = As(a2,gs1,zs1)
      As1a2c= DCONJG(As1a2)
      As1b2 = As(b2,gs1,zs1)
      As1b2c= DCONJG(As1b2)
      As1ab = As(ab,gs1,zs1)
      As1abc= DCONJG(As1ab)
      Atpa2 = At(a2,gtp,ztp)
      Atpb2 = At(b2,gtp,ztp)
      Atpab = At(ab,gtp,ztp)
      Atqa2 = At(a2,gtq,ztq)
      Atqb2 = At(b2,gtq,ztq)
      Atqab = At(ab,gtq,ztq)
! Radiation factors
      rf1 = s /(p1k*q1k)
      rf2 = s1/(p2k*q2k)
      rf3 =-tp/(p1k*p2k)
      rf4 =-tq/(q1k*q2k)
      rf5 = u /(p1k*q2k)
      rf6 = u1/(q1k*p2k)
      W1 = rf1
      W2 = rf2
      W3 = rf3 + rf4 + rf5 + rf6
      W4 = rf3
      W5 = rf4
      W6 = rf1 + rf2 + rf5 + rf6
! Some traces
      Tr1 = 2*(s *p2k +u1*p1k -tp*q1k)
      Tr2 = 2*(s *q2k +u *q1k -tq*p1k)
      Tr3 = 2*(s1*p1k +u *p2k -tp*q2k)
      Tr4 = 2*(s1*q1k +u1*q2k -tq*p2k)
      epspq = s/2 *( p2(2)*q2(1) - p2(1)*q2(2) )
! Matrix element squared for massless spinors
      xm1 = ( W1*As1ab*As1abc +W2*Asab *Asabc +W3*DBLE(Asab *As1abc) )
     $     *(tp**2 + tq**2)/(s *s1)
      xm2 = ( W4*Atqab*Atqab  +W5*Atpab*Atpab +W6*     Atpab*Atqab   )
     $     *(s**2  + s1**2)/(tp*tq)
      xm3 = ( W1*(As1a2*As1a2c + As1b2*As1b2c)
     $       +W2*(Asa2 *Asa2c  + Asb2 *Asb2c )
     $       +W3*DBLE(Asa2 *As1a2c + Asb2 *As1b2c) )
     $     *(u**2 + u1**2)/(2*s *s1)
      xm4 = ( W4*(Atqa2*Atqa2  + Atqb2*Atqb2 )
     $       +W5*(Atpa2*Atpa2  + Atpb2*Atpb2 )
     $       +W6*    (Atpa2*Atqa2  + Atpb2*Atqb2 ) )
     $     *(u**2 + u1**2)/(2*tp*tq)
      xm5 = ( p1k/(s *tp)*Tr1*DBLE(Asa2 *Atpa2 + Asb2 *Atpb2)
     $       +q1k/(s *tq)*Tr2*DBLE(Asa2 *Atqa2 + Asb2 *Atqb2)
     $       +p2k/(s1*tp)*Tr3*DBLE(As1a2*Atpa2 + As1b2*Atpb2)
     $       +q2k/(s1*tq)*Tr4*DBLE(As1a2*Atqa2 + As1b2*Atqb2) )
     $     *(u**2 + u1**2)/(4*p1k*q1k*p2k*q2k)
      xm6 =-( (s-s1)/(s*s1)*DIMAG(Asa2*As1a2c - Asb2*As1b2c)
     $       +2*p1k/(s *tp)*DIMAG(Asa2 *Atpa2 - Asb2 *Atpb2)
     $       +2*q1k/(s *tq)*DIMAG(Asa2 *Atqa2 - Asb2 *Atqb2)
     $       -2*p2k/(s1*tp)*DIMAG(As1a2*Atpa2 - As1b2*Atpb2)
     $       -2*q2k/(s1*tq)*DIMAG(As1a2*Atqa2 - As1b2*Atqb2) )
     $     *(u**2 - u1**2)*epspq/(2*p1k*q1k*p2k*q2k)
      xmem0 = e6 *(xm1+xm2+xm3+xm4+xm5+xm6)
! Mass correction terms (here fermions must be massive!)
      xmemc = xmelmc(p1,q1,p2,q2,pk)
! Total matrix element squared
      xmecal = xmem0 + xmemc
      END
      SUBROUTINE MODEL1(MODE)
!     ***********************
!======================================================================!
!                                                                      !
!    Matrix element for Bhabha scattering inluding O(alpha)            !
!    radiative corrections as defined in Yennie-Frautschi-Suura        !
!    formula by functions beta0 and beta1.                             !
!                                                                      !
!    1. Electroweak virtual and real soft photon corrections           !
!       are taken from the program BABAMC or the program               !
!       ALIBABA depending on the swicht parameter KeyLib.              !
!    2. Hard photon radiation contribution is calculated according     !
!       to formulae obtained by the authors (see routine xmatel).      !
!                                                                      !
!======================================================================!
!    Written by: Wieslaw Placzek                  Knoxville, May 1995  !
!    Last update: 04.10.1996       by: W.P.                            !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( pi = 3.1415926535897932d0, alfinv=137.03604d0)
      PARAMETER( alfpi=  1/pi/alfinv, alfa= 1d0/alfinv)
      PARAMETER( Gnanob=389.385D3 )
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHCRUD / trmid,crufla,Zprof,sg01,sg02,sg03,sig0
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRUD,WTTRIG,WTSET(300)
!WP: photon radiation tags
      Common / RadTag / idrad(100)
      SAVE   / BHPAR1 /, / BHPAR2 /, / BHPAR3 /, / TRANSR /, / BHCRUD /
      SAVE   / MOMSET /, / WGTALL /, / RadTag /
      REAL*8 p1r0(4),q1r0(4),p2r0(4),q2r0(4)
      REAL*8 p1r(4),q1r(4),p2r(4),q2r(4),pk(4),pkr(4)
      REAL*8 p1e(4),q1e(4),qq(4),pp(4)
! Switch for some diagnostic printouts: 0/1 (OFF/ON)
      DATA KeyDia /0/
! Count bad weights if appear
      DATA ibadw0 /0/, ibadw1 /0/
      DATA icont /0/
      icont=icont+1
!
      eps = EPSCM
      ame = AMEL
      s   = CMSENE**2
      t   =-TRAN
CC JVW (variable ame mispelled as eme)
C      u   = 4*eme**2 -s -t
      u   = 4*ame**2 -s -t
CC JVW
      zeta = TRAN/s
! Crude MC distribution
      IF (TRAN.ge.trmid) THEN
        crude =  Zprof + (s/t)**2
      ELSE
! Flat distribution for very small values of TRAN
        crude = crufla
      ENDIF
! Calculate fermion effective 4-momenta at the interation point
      DO k = 1,4
        p1e(k) = p1(k)
        q1e(k) = q1(k)
      ENDDO
      DO i = 1,NPHOT
        ira = idrad(i)
        IF (ira.eq.1) THEN
          DO k =1,4
            p1e(k) = p1e(k) -PHOT(i,k)
          ENDDO
        ELSEIF (ira.eq.2) THEN
          DO k =1,4
            q1e(k) = q1e(k) -PHOT(i,k)
          ENDDO
        ENDIF
      ENDDO
      DO k = 1,4
        qq(k) = p1e(k) + q1e(k)
      ENDDO
      IF (NPHOT.eq.0) THEN
        DO k = 1,4
          p1r0(k) = p1(k)
          q1r0(k) = q1(k)
          p2r0(k) = p2(k)
          q2r0(k) = q2(k)
        ENDDO
      ELSE
! Reduction for beta0 (Born-like process)
        CALL REDUZ0(qq,p1,q1,p1r0,q1r0)
        CALL REDUZ0(qq,p2,q2,p2r0,q2r0)
      ENDIF
! Mandelstam variables
      s1 = qq(4)**2 -qq(3)**2 -qq(2)**2 -qq(1)**2
      t1 = (p1r0(4)-p2r0(4))**2 -(p1r0(3)-p2r0(3))**2
     $    -(p1r0(2)-p2r0(2))**2 -(p1r0(1)-p2r0(1))**2
      u1 = 4*ame**2 -s1 -t1
! Born matrix element
      xmebo = xmate0(s1,t1,u1)
      dis00 = xmebo /(4*pi*alfa)**2/4
! Electroweak O(alpha) virtual and real soft photon corrections:
      KeyEWC = MOD(KeyRad,10000)/1000
      KeyLib = MOD(KeyRad,1000)/100
      IF (KeyLib.EQ.2) THEN
! ... From ALIBABA
        xmqed = xmatvs(s1,t1,eps)
        alivs = xmebo + xmqed
        IF (KeyEWC.EQ.1) THEN
          xweak = xmatwc(s1,t1)
          alivs = xweak + xmqed
        ENDIF
        dis01 = alivs /(4*pi*alfa)**2/4
      ELSEIF (KeyLib.EQ.1) THEN
! ... From BABAMC
!    dsig0 - Born cross section, dsig1 - O(alpha) cross section.
        CALL babewc(s1,t1,eps,dsig0,dsig1)
        babvs = dsig1/dsig0
        dis01 = dis00*babvs
      ENDIF
! Infrared factor
      finf = YFSirf(p1r0,q1r0,p2r0,q2r0,eps,ame)
!===================================================================
!          ########################################
!          #               beta0                  #
!          ########################################
      beta00 = dis00
      beta01 = dis01  -finf*beta00
!          ########################################
!          #               beta1                  #
!          ########################################
      beta10 = 0
      DO i = 1,NPHOT
        ira = idrad(i)
        DO k = 1,4
          pk(k) = PHOT(i,k)
        ENDDO
        IF (NPHOT.eq.1) THEN
          DO k = 1,4
            p1r(k) = p1(k)
            q1r(k) = q1(k)
            p2r(k) = p2(k)
            q2r(k) = q2(k)
            pkr(k) = pk(k)
          ENDDO
        ELSE
! Initial state radiation
          IF (ira.eq.1 .or. ira.eq.2) THEN
! Reduction procedure (e+e- ---> e+e- + gamma like process)
            call REDUZ1(qq,p1,q1,pk,p1r,q1r,pkr)
            DO k = 1,4
              p2r(k) = p2r0(k)
              q2r(k) = q2r0(k)
              pp(k)  = p1r(k) + q1r(k)
            ENDDO
            CALL boost5(pp ,p1r,q1r,p2r,q2r,pkr)
            CALL rotat5(p1r,q1r,p2r,q2r,pkr)
! Final state radiation
          ELSE
! Reduction procedure (e+e- ---> e+e- + gamma like process)
            DO k = 1,4
               pk(k) = -pk(k)
            ENDDO
            CALL REDUZ1(qq,p2,q2,pk,p2r,q2r,pkr)
            DO k = 1,4
              p1r(k) = p1r0(k)
              q1r(k) = q1r0(k)
              pkr(k) =-pkr(k)
            ENDDO
          ENDIF
        ENDIF
        thp1k = angvec(p1r,pkr)
        thq1k = angvec(q1r,pkr)
        thp2k = angvec(p2r,pkr)
        thq2k = angvec(q2r,pkr)
        delth = 1d-7
! Collinear approx. if angle between lepton and photon less than delth
        IF (thp1k.LT.delth) THEN
          dis10 = 0
          z = pkr(4)/p1r(4)
!WP          beta1i = beta00 *z*(z-2)/2
          beta1i = beta00 *z**2/2/(1-z)
cc          print*,' iphot,theta_p1k=',i,thp1k
cc          print*,' beta01,beta1i=',beta01,beta1i
cc          CALL dumps(6)
        ELSEIF (thq1k.LT.delth) THEN
          dis10 = 0
          z = pkr(4)/q1r(4)
!WP          beta1i = beta00 *z*(z-2)/2
          beta1i = beta00 *z**2/2/(1-z)
cc          print*,' iphot,theta_q1k=',i,thq1k
cc          print*,' beta01,beta1i=',beta01,beta1i
cc          CALL dumps(6)
        ELSEIF (thp2k.LT.delth) THEN
          dis10 = 0
          z = pkr(4)/(p2r(4)+pkr(4))
!WP          beta1i = beta00 *z*(z-2)/2
          beta1i = beta00 *z**2/2/(1-z)
cc          print*,' iphot,theta_p2k=',i,thp2k
cc          print*,' beta01,beta1i=',beta01,beta1i
cc          call dumps(6)
        ELSEIF (thq2k.LT.delth) THEN
          dis10 = 0
          z = pkr(4)/(q2r(4)+pkr(4))
!WP          beta1i = beta00 *z*(z-2)/2
          beta1i = beta00 *z**2/2/(1-z)
cc          print*,' iphot,theta_q2k=',i,thq2k
cc          print*,' beta01,beta1i=',beta01,beta1i
cc          CALL dumps(6)
        ELSE
! Soft factor
          sfr = SoftFa(p1r,q1r,p2r,q2r,pkr)
! Single hard photon bremss. matrix element
          dis10 = xmatel(p1r,q1r,p2r,q2r,pkr) /( 4*(4*pi*alfa)**3 )
! beta1 O(alpha1)
          beta1i = dis10/sfr - beta00
        ENDIF
! Sum of beta1 for all photons
        beta10 = beta10 + beta1i
      ENDDO
!
!          **************************************
!          **     Definitions of MC weights    **
!          **************************************
! All beta's:  O(alf0),O(alf1)
      wtset(10) =  beta00           /crude
      wtset(11) = (beta01 + beta10) /crude
!==================================================================
!==================================================================
!                  Non-exponentiated version                      !
!==================================================================
!==================================================================
! Entire 0,1-photon distributions
      dsne0 = 0
      dsne1 = 0
      fYFS  = 1
      IF(NPHOT.eq.0) THEN
! [0] No hard photons
! O(alpha^0,^1) entire distributions
        dsne0 = dis00
        dsne1 = dis01
! YFS formfactor
        fYFS = YFSfmf(eps)
      ELSEIF(NPHOT.eq.1) THEN
! [1] One hard photon
! O(alpha^1) entire distribution
        dsne1 = dis10 /sfr
! YFS formfactor
        fYFS = YFSfmf(eps)
      ENDIF
!
!          **************************************
!          ****   Definitions of MC weights  ****
!          ****    Non-exponentiated case    ****
!          **************************************
! TOTAL O(alpha^0),O(alpha^1)
      wtset(20) = dsne0 /fYFS/crude
      wtset(21) = dsne1 /fYFS/crude
!
!---------------------------------------------------------------!
! Setting some "bad" weights to zero!
      wtx1 =-10.0
      wtx2 = 50.0
      wt0 = wtcrud*wttrig*wtset(10)
      IF (wt0.lt.wtx1 .or. wt0.gt.wtx2) THEN
        ibadw0 = ibadw0 + 1
        IF (KeyDia.eq.1) THEN
          IObad = 6
          WRITE(IObad,*)'iwt0,wt0=',ibadw0,wt0
          WRITE(IObad,*)'s,s1=',s,s1
          WRITE(IObad,*)'t,t1=',t,t1
          WRITE(IObad,*)'idrad=',(idrad(i),i=1,nphot)
          CALL dumps(IObad)
        ENDIF
        wtset(10) = 0
      ENDIF
      wt1 = wtcrud*wttrig*wtset(11)
      IF (wt1.lt.wtx1 .or. wt1.gt.wtx2) THEN
        ibadw1 = ibadw1 + 1
        IF (KeyDia.eq.1) THEN
          IObad = 6
          WRITE(IObad,*)'iwt1,wt1=',ibadw1,wt1
          WRITE(IObad,*)'s,s1=',s,s1
          WRITE(IObad,*)'t,t1=',t,t1
          WRITE(IObad,*)'idrad=',(idrad(i),i=1,nphot)
          CALL dumps(IObad)
        ENDIF
        wtset(11) = 0
      ENDIF
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!+++++++++++++++++++ PRINCIPAL WEIGHT ++++++++++++++++++++++++++!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
      wtset( 1) = wtset(11)
      END

      FUNCTION angvec(p,q)
*     ********************
!---------------------------------------------------------------!
! This function provides a value of an agle between 2 vectors.  !
! INPUT:  p(4),q(4) - two 4-vectors                             !
!---------------------------------------------------------------!
! Written by: Wieslaw Placzek              Knoxville, May 1995  !
! Last update: 19.07.1995       by: W.P.                        !
!---------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      REAL*8 p(4),q(4)
!
      pm = SQRT( p(1)**2 + p(2)**2 + p(3)**2 )
      qm = SQRT( q(1)**2 + q(2)**2 + q(3)**2 )
      pq = p(1)*q(1) + p(2)*q(2) + p(3)*q(3)
      costhe = pq/(pm*qm)
      IF (costhe.LT.-1d0) costhe =-1d0
      IF (costhe.GT. 1d0) costhe = 1d0
      angvec = ACOS(costhe)
      END

      function SoftFa(p1,q1,p2,q2,pk)
*     *******************************
!---------------------------------------------------------------!
! This function provides a value of the soft factor S-tilde     !
! for massless fermions (factor (alpha/4pi^2) omitted!).        !
! INPUT:  p1(4),q1(4),p2(4),q2(4) - fermion 4-momenta           !
!                           pk(4) - photon  4-momentum          !
!---------------------------------------------------------------!
! Written by: Wieslaw Placzek              Knoxville, May 1995  !
! Last update: 23.05.1995       by: W.P.                        !
!---------------------------------------------------------------!
      implicit REAL*8 (a-h,o-z)
      COMMON / BHPAR2 / CMSENE,AMEL
      SAVE   / BHPAR2 /
      REAL*8 p1(4) ,q1(4) ,p2(4) ,q2(4) ,pk(4)
      REAL*8 p1r(4),q1r(4),p2r(4),q2r(4),pkr(4)
!
      ame = AMEL
! Scalar products of massive fermion 4-momenta
      p1km = p1(4)*pk(4)-p1(3)*pk(3)-p1(2)*pk(2)-p1(1)*pk(1)
      q1km = q1(4)*pk(4)-q1(3)*pk(3)-q1(2)*pk(2)-q1(1)*pk(1)
      p2km = p2(4)*pk(4)-p2(3)*pk(3)-p2(2)*pk(2)-p2(1)*pk(1)
      q2km = q2(4)*pk(4)-q2(3)*pk(3)-q2(2)*pk(2)-q2(1)*pk(1)
! Rescaling 4-momenta of all particles for the massless limit
      pmod = SQRT(p2(1)**2 +p2(2)**2 +p2(3)**2)
      qmod = SQRT(q2(1)**2 +q2(2)**2 +q2(3)**2)
      refa = ( pmod + qmod + pk(4) )/( p1(4) + q1(4) )
      DO i = 1,4
         p1r(i) = p1(i)
         q1r(i) = q1(i)
         p2r(i) = p2(i)/refa
         q2r(i) = q2(i)/refa
         pkr(i) = pk(i)/refa
      ENDDO
      p1r(3) = p1r(4)
      q1r(3) =-q1r(4)
      p2r(4) = pmod/refa
      q2r(4) = qmod/refa
! Scalar products of massless fermion 4-momenta
      p1k  = p1r(4)*pkr(4)-p1r(3)*pkr(3)-p1r(2)*pkr(2)-p1r(1)*pkr(1)
      q1k  = q1r(4)*pkr(4)-q1r(3)*pkr(3)-q1r(2)*pkr(2)-q1r(1)*pkr(1)
      p2k  = p2r(4)*pkr(4)-p2r(3)*pkr(3)-p2r(2)*pkr(2)-p2r(1)*pkr(1)
      q2k  = q2r(4)*pkr(4)-q2r(3)*pkr(3)-q2r(2)*pkr(2)-q2r(1)*pkr(1)
      p1q1 = p1r(4)*q1r(4)-p1r(3)*q1r(3)-p1r(2)*q1r(2)-p1r(1)*q1r(1)
      p1p2 = p1r(4)*p2r(4)-p1r(3)*p2r(3)-p1r(2)*p2r(2)-p1r(1)*p2r(1)
      p1q2 = p1r(4)*q2r(4)-p1r(3)*q2r(3)-p1r(2)*q2r(2)-p1r(1)*q2r(1)
      q1p2 = q1r(4)*p2r(4)-q1r(3)*p2r(3)-q1r(2)*p2r(2)-q1r(1)*p2r(1)
      q1q2 = q1r(4)*q2r(4)-q1r(3)*q2r(3)-q1r(2)*q2r(2)-q1r(1)*q2r(1)
      p2q2 = p2r(4)*q2r(4)-p2r(3)*q2r(3)-p2r(2)*q2r(2)-p2r(1)*q2r(1)
! Massless terms
      sfml = 2*( p1p2/(p1k*p2k) +p1q1/(p1k*q1k) -p1q2/(p1k*q2k)
     &          -q1p2/(q1k*p2k) +p2q2/(p2k*q2k) +q1q2/(q1k*q2k) )
! Mass terms
      sfmt = ame**2 *(1/p1km**2 +1/q1km**2 +1/p2km**2 +1/q2km**2)
      SoftFa = sfml - sfmt
      END

      SUBROUTINE REDUZ0(QQ,P1,P2,PR1,PR2)
*     ***********************************
!---------------------------------------------------------------!
! This routine comes origilally from the program YFS3.          !
! Modified by: W. Placzek        Knoxville, May 1995            !
!---------------------------------------------------------------!
C reduction of momenta for beta0, second one
C I.E. WE MAPP:   P1,P2 ==> PR1,PR2
C such that  PR1+PR2 = QQ
C Resulting PRi QRi are in QQ rest frame.
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( EPS1 =1D-15)
      DIMENSION QQ(4),P1(4),P2(4),PR1(4),PR2(4)
      DIMENSION PP(4),PX1(4),PX2(4),PPX(4)
C
      DO 20 K=1,4
 20   PP(K)=P1(K)+P2(K)
      IF((PP(1)**2+PP(2)**2+PP(3)**2)/PP(4)**2 .GT. EPS1) THEN
C transform all momenta to QQ rest-frame
         CALL BOSTDQ( 1,QQ,P1 ,PX1)
         CALL BOSTDQ( 1,QQ,P2 ,PX2)
         CALL BOSTDQ( 1,QQ,PP ,PPX)
C transform all momenta to PP rest-frame
         CALL BOSTDQ( 1,PPX,PX1,PX1)
         CALL BOSTDQ( 1,PPX,PX2,PX2)
!WP      CALL BOSTDQ( 1,PP,P1,PX1)
!WP      CALL BOSTDQ( 1,PP,P2,PX2)
      ELSE
C do nothing if we are already in PP rest-frame
         DO 23 K=1,4
            PX1(K)=P1(K)
   23       PX2(K)=P2(K)
      ENDIF
C construct reduced beam momenta PR1,PR2
C note: they are understood to be in QQ rest-frame
      SVAR1 = QQ(4)**2-QQ(3)**2-QQ(2)**2-QQ(1)**2
      SVAR  = PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2
      VV    = 1D0 -SVAR1/SVAR
      IF(ABS(VV).GT. EPS1) THEN
         AMEL2=  P1(4)**2-P1(3)**2-P1(2)**2-P1(1)**2
         PR1(4)= SQRT(SVAR1)/2D0
         PR2(4)= PR1(4)
         PXMOD = SQRT(PX1(1)**2+PX1(2)**2+PX1(3)**2)
         PRMOD = SQRT(PR1(4)**2-AMEL2)
         DO 30 K=1,3
         PR1(K)= PX1(K)/PXMOD*PRMOD
 30      PR2(K)= PX2(K)/PXMOD*PRMOD
      ELSE
         DO 40 K=1,4
         PR1(K)= PX1(K)
 40      PR2(K)= PX2(K)
      ENDIF
      END

      SUBROUTINE REDUZ1(QQ,P1,P2,PH,PR1,PR2,PHR)
*     ******************************************
!---------------------------------------------------------------!
! This routine comes origilally from the program YFS3.          !
! Modified by: W. Placzek        Knoxville, May 1995            !
!---------------------------------------------------------------!
C reduction of 4-momenta for beta1
C           P1,P2,PH ==--> PR1,PR2,PHR
C such that  PR1+PR2 = QQ+PHR
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( EPS1 =1D-15)
      COMMON / INOUT  / NINP,NOUT
      SAVE   / INOUT  /
      DIMENSION QQ(4), P1(4), P2(4), PH(4), PR1(4),PR2(4),PHR(4)
      DIMENSION PP(4),QQK(4),PPX(4), PPK(4)
      DIMENSION PX1(4),PX2(4),PHX(4)
C
      DO 20 K=1,4
      PP(K)   = P1(K)+P2(K)
      PPK(K)  = P1(K)+P2(K)-PH(K)
 20   QQK(K)  = QQ(K)+PH(K)
      SVAR  =  PP(4)**2 -PP(3)**2 -PP(2)**2 -PP(1)**2
      SVAR1 =  QQ(4)**2 -QQ(3)**2 -QQ(2)**2 -QQ(1)**2
      SS1   = PPK(4)**2-PPK(3)**2-PPK(2)**2-PPK(1)**2
      SS2   = QQK(4)**2-QQK(3)**2-QQK(2)**2-QQK(1)**2
      IF((PP(1)**2+PP(2)**2+PP(3)**2)/PP(4)**2 .GT. EPS1) THEN
C transform all momenta to QQ rest-frame
         CALL BOSTDQ( 1,QQ,P1 ,PX1)
         CALL BOSTDQ( 1,QQ,P2 ,PX2)
         CALL BOSTDQ( 1,QQ,PH ,PHX)
         CALL BOSTDQ( 1,QQ,PP ,PPX)
C transform all momenta to PP rest-frame
         CALL BOSTDQ( 1,PPX,PX1,PX1)
         CALL BOSTDQ( 1,PPX,PX2,PX2)
         CALL BOSTDQ( 1,PPX,PHX,PHX)
!WP      CALL BOSTDQ( 1,PP,P1,PX1)
!WP       CALL BOSTDQ( 1,PP,P2,PX2)
!WP       CALL BOSTDQ( 1,PP,PH,PHX)
      ELSE
C do nothing if we are already in PP rest-frame
         DO 23 K=1,4
            PHX(K)=PH(K)
            PX1(K)=P1(K)
   23       PX2(K)=P2(K)
      ENDIF
C construct reduced beam momenta PR1,PR2
C note: they are understood to be in QQ rest-frame
      VV2   = 1D0 - SS2/SVAR
      IF(ABS(VV2).GT. EPS1) THEN
CCCCC    PK    =  (PX1(4)+PX2(4))*PHX(4)
CCCCC    XLAM= SQRT(SVAR1/SVAR+(PK/SVAR)**2)+PK/SVAR
         XLAM= SQRT(SVAR1/SS1)
         AMEL2=  P1(4)**2-P1(3)**2-P1(2)**2-P1(1)**2
         PXMOD = SQRT(PX1(1)**2+PX1(2)**2+PX1(3)**2)
         PX1(4)= PX1(4)*XLAM
         PX2(4)= PX2(4)*XLAM
CCC      PRMOD = SQRT(PX1(4)**2-AMEL2)
         PRMOD =      PX1(4)**2-AMEL2
         IF(PRMOD.LE.0D0) WRITE(NOUT,*) ' REDUZ1: PRMOD=', PRMOD
         IF(PRMOD.LE.0D0) WRITE(   6,*) ' REDUZ1: PRMOD=', PRMOD
         PRMOD = SQRT(ABS(PRMOD))
         DO 30 K=1,3
         PX1(K)= PX1(K)/PXMOD*PRMOD
 30      PX2(K)= PX2(K)/PXMOD*PRMOD
         DO 31 K=1,4
 31      PHX(K)= PHX(K)*XLAM
      ENDIF
C then, boost away the three-vector part of P1+P2-PH
C that is transform to QQ rest frame
      DO 35 K=1,4
 35   PP(K)= PX1(K)+PX2(K)-PHX(K)
      CALL BOSTDQ( 1,PP,PX1,PR1)
      CALL BOSTDQ( 1,PP,PX2,PR2)
      CALL BOSTDQ( 1,PP,PHX,PHR)
      END

      SUBROUTINE boost5(qq,p1,p2,p3,p4,p5)
*     ************************************
!---------------------------------------------------------------!
! Boost of the 4-vectors p1,p2,p3,p4,p5 to the rest frame of qq.!
!---------------------------------------------------------------!
! Written by: Wieslaw Placzek              Knoxville, May 1995  !
!---------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      REAL*8 qq(4),p1(4),p2(4),p3(4),p4(4),p5(4)
      CALL bostdq( 1,qq,p1,p1)
      CALL bostdq( 1,qq,p2,p2)
      CALL bostdq( 1,qq,p3,p3)
      CALL bostdq( 1,qq,p4,p4)
      CALL bostdq( 1,qq,p5,p5)
      END

      SUBROUTINE rotat5(p1,q1,p2,q2,pk)
*     *********************************
!---------------------------------------------------------------!
! Rotation of the 4-vectors p1,q1,p2,q2,pk given in the rest    !
! frame of p1+q1 such that the resulting vector p1 points along !
! the +z axis.                                                  !
!---------------------------------------------------------------!
! Written by: Wieslaw Placzek              Knoxville, May 1995  !
!---------------------------------------------------------------!
      IMPLICIT REAL*8 (a-h,o-z)
      PARAMETER ( pi = 3.1415926535897932d0 )
      REAL*8 p1(4),q1(4),p2(4),q2(4),pk(4)
!
      p1m = SQRT( p1(1)**2 + p1(2)**2 + p1(3)**2 )
      p1t = p1(1)**2 + p1(2)**2
      IF (p1t.GT.1d-20) THEN
        phi = angfi(p1(1),p1(2))
        CALL rotod3(-phi,p2,p2)
        CALL rotod3(-phi,q2,q2)
        CALL rotod3(-phi,pk,pk)
      ENDIF
      the = ACOS(p1(3)/p1m)
      CALL rotod2(-the,p2,p2)
      CALL rotod2(-the,q2,q2)
      CALL rotod2(-the,pk,pk)
      p1(3) = p1m
      p1(2) = 0
      p1(1) = 0
      q1(3) =-p1m
      q1(2) = 0
      q1(1) = 0
      END
      SUBROUTINE MODEL2(MODE)
!     ***********************
!======================================================================!
!                                                                      !
!    Matrix element for Bhabha scattering inluding O(alpha)            !
!    radiative corrections as defined in Yennie-Frautschi-Suura        !
!    formula by functions beta0 and beta1.                             !
!                                                                      !
!    1. Electroweak virtual and real soft photon corrections           !
!       are taken from the program BABAMC or the program               !
!       ALIBABA depending on the swicht parameter KeyLib.              !
!    2. Hard photon radiation contribution is calculated according     !
!       to the matrix element given by CALKUL, F.A. Berends et al.,    !
!       Nucl. Phys. B206 (1982) 61.                                    !
!                                                                      !
!======================================================================!
!    Written by: Wieslaw Placzek                  Knoxville, July 1995 !
!    Last update: 04.10.1996       by: W.P.                            !
!----------------------------------------------------------------------!
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( pi = 3.1415926535897932d0, alfinv=137.03604d0)
      PARAMETER( alfpi=  1/pi/alfinv, alfa= 1d0/alfinv)
      PARAMETER( Gnanob=389.385D3 )
      COMMON / BHPAR1 / DEL,EPSCMS,THMIN,XMIVIS
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHCRUD / trmid,crufla,Zprof,sg01,sg02,sg03,sig0
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRUD,WTTRIG,WTSET(300)
! Photon radiation tags (needed for reduction procedure)
      Common / RadTag / idrad(100)
      SAVE   / BHPAR1 /, / BHPAR2 /, /BHPAR3 /, / TRANSR /, / BHCRUD /
      SAVE   / MOMSET /, / WGTALL /, / RadTag /
      REAL*8 p1r0(4),q1r0(4),p2r0(4),q2r0(4)
      REAL*8 p1r(4),q1r(4),p2r(4),q2r(4),pk(4),pkr(4)
      REAL*8 p1e(4),q1e(4),qq(4),pp(4)
! Switch for some diagnostic printouts: 0/1 (OFF/ON)
      DATA KeyDia /0/
! Count bad weights if appear
      DATA ibadw0 /0/, ibadw1 /0/
      DATA icont /0/
      icont=icont+1
!
      eps = EPSCMS
      ame = AMEL
      s   = CMSENE**2
      t   =-TRAN
      u   = 4*ame**2 -s -t
! Crude MC distribution
      IF (TRAN.ge.trmid) THEN
        crude =  Zprof + (s/t)**2
      ELSE
! Flat distribution for very small values of TRAN
        crude = crufla
      ENDIF
! Calculate fermion effective 4-momenta at the interation point
      DO k = 1,4
        p1e(k) = p1(k)
        q1e(k) = q1(k)
      ENDDO
      DO i = 1,NPHOT
        ira = idrad(i)
        IF (ira.eq.1) THEN
          DO k =1,4
            p1e(k) = p1e(k) -PHOT(i,k)
          ENDDO
        ELSEIF (ira.eq.2) THEN
          DO k =1,4
            q1e(k) = q1e(k) -PHOT(i,k)
          ENDDO
        ENDIF
      ENDDO
      DO k = 1,4
        qq(k) = p1e(k) + q1e(k)
      ENDDO
      IF (NPHOT.eq.0) THEN
        DO k = 1,4
          p1r0(k) = p1(k)
          q1r0(k) = q1(k)
          p2r0(k) = p2(k)
          q2r0(k) = q2(k)
        ENDDO
      ELSE
! Reduction for beta0 (Born-like process)
        CALL REDUZ0(qq,p1,q1,p1r0,q1r0)
        CALL REDUZ0(qq,p2,q2,p2r0,q2r0)
      ENDIF
! Mandelstam variables
      s1 = qq(4)**2 -qq(3)**2 -qq(2)**2 -qq(1)**2
      t1 = (p1r0(4)-p2r0(4))**2 -(p1r0(3)-p2r0(3))**2
     $    -(p1r0(2)-p2r0(2))**2 -(p1r0(1)-p2r0(1))**2
      u1 = 4*ame**2 -s1 -t1
! Born matrix element
      xmebo = xmate0(s1,t1,u1)
      dis00 = xmebo /(4*pi*alfa)**2/4
! Electroweak O(alpha) virtual and real soft photon corrections:
      KeyEWC = MOD(KeyRad,10000)/1000
      KeyLib = MOD(KeyRad,1000)/100
      IF (KeyLib.EQ.2) THEN
! ... From ALIBABA
        xmqed = xmatvs(s1,t1,eps)
        alivs = xmebo + xmqed
        IF (KeyEWC.EQ.1) THEN
          xweak = xmatwc(s1,t1)
          alivs = xweak + xmqed
        ENDIF
        dis01 = alivs /(4*pi*alfa)**2/4
      ELSEIF (KeyLib.EQ.1) THEN
! ... From BABAMC
!    dsig0 - Born cross section, dsig1 - O(alpha) cross section.
        CALL babewc(s1,t1,eps,dsig0,dsig1)
        babvs = dsig1/dsig0
        dis01 = dis00*babvs
      ENDIF
! Infrared factor
      finf = YFSirf(p1r0,q1r0,p2r0,q2r0,eps,ame)
!===================================================================
!          ########################################
!          #               beta0                  #
!          ########################################
      beta00 = dis00
      beta01 = dis01  -finf*beta00
!          ########################################
!          #               beta1                  #
!          ########################################
      beta10 = 0
      DO i = 1,NPHOT
        ira = idrad(i)
        DO k = 1,4
          pk(k) = PHOT(i,k)
        ENDDO
        IF (NPHOT.eq.1) THEN
          DO k = 1,4
            p1r(k) = p1(k)
            q1r(k) = q1(k)
            p2r(k) = p2(k)
            q2r(k) = q2(k)
            pkr(k) = pk(k)
          ENDDO
        ELSE
! Initial state radiation
          IF (ira.eq.1 .or. ira.eq.2) THEN
! Reduction procedure (e+e- ---> e+e- + gamma like process)
            CALL REDUZ1(qq,p1,q1,pk,p1r,q1r,pkr)
            DO k = 1,4
              p2r(k) = p2r0(k)
              q2r(k) = q2r0(k)
              pp(k)  = p1r(k) + q1r(k)
            ENDDO
            CALL boost5(pp ,p1r,q1r,p2r,q2r,pkr)
            CALL rotat5(p1r,q1r,p2r,q2r,pkr)
! Final state radiation
          ELSE
! Reduction procedure (e+e- ---> e+e- + gamma like process)
            DO k = 1,4
               pk(k) = -pk(k)
            ENDDO
            CALL REDUZ1(qq,p2,q2,pk,p2r,q2r,pkr)
            DO k = 1,4
              p1r(k) = p1r0(k)
              q1r(k) = q1r0(k)
              pkr(k) =-pkr(k)
            ENDDO
          ENDIF
        ENDIF
! Soft factor (for massive fermion 4-momenta)
        sfr = SoftFm(p1r,q1r,p2r,q2r,pkr)
! Soft factor (for massless fermion 4-momenta)
cc        sfr = SoftFa(p1r,q1r,p2r,q2r,pkr)
! Single hard photon bremss. matrix element
        dis10 = xmecal(p1r,q1r,p2r,q2r,pkr) /( 4*(4*pi*alfa)**3 )
! beta1 O(alpha1)
        beta1i = dis10/sfr - beta00
! Sum of beta1 for all photons
        beta10 = beta10 + beta1i
      ENDDO
!
!          **************************************
!          **     Definitions of MC weights    **
!          **************************************
! All beta's:  O(alf0),O(alf1)
      wtset(110) =  beta00           /crude
      wtset(111) = (beta01 + beta10) /crude
!==================================================================
!==================================================================
!                  Non-exponentiated version                      !
!==================================================================
!==================================================================
! Entire 0,1-photon distributions
      dsne0 = 0
      dsne1 = 0
      fYFS  = 1
      IF(NPHOT.eq.0) THEN
! [0] No hard photons
! O(alpha^0,^1) entire distributions
        dsne0 = dis00
        dsne1 = dis01
! YFS formfactor
        fYFS = YFSfmf(eps)
      ELSEIF(NPHOT.eq.1) THEN
! [1] One hard photon
! O(alpha^1) entire distribution
        dsne1 =  dis10 /sfr
! YFS formfactor
        fYFS = YFSfmf(eps)
      ENDIF
!
!          **************************************
!          ****   Definitions of MC weights  ****
!          ****    Non-exponentiated case    ****
!          **************************************
! TOTAL O(alpha^0),O(alpha^1)
      wtset(120) = dsne0 /fYFS/crude
      wtset(121) = dsne1 /fYFS/crude
!---------------------------------------------------------------!
! Setting some "bad" weights to zero!
      wtx1 =-10.0
      wtx2 = 50.0
      wt0 = wtcrud*wttrig*wtset(110)
      IF (wt0.lt.wtx1 .or. wt0.gt.wtx2) THEN
        ibadw0 = ibadw0 + 1
        IF (KeyDia.eq.1) THEN
          IObad = 6
          WRITE(IObad,*)'iwt0,wt0=',ibadw0,wt0
          WRITE(IObad,*)'s,s1=',s,s1
          WRITE(IObad,*)'t,t1=',t,t1
          WRITE(IObad,*)'idrad=',(idrad(i),i=1,nphot)
          CALL dumps(IObad)
        ENDIF
        wtset(110) = 0
      ENDIF
      wt1 = wtcrud*wttrig*wtset(111)
      IF (wt1.lt.wtx1 .or. wt1.gt.wtx2) THEN
        ibadw1 = ibadw1 + 1
        IF (KeyDia.eq.1) THEN
          IObad = 6
          WRITE(IObad,*)'iwt1,wt1=',ibadw1,wt1
          WRITE(IObad,*)'s,s1=',s,s1
          WRITE(IObad,*)'t,t1=',t,t1
          WRITE(IObad,*)'idrad=',(idrad(i),i=1,nphot)
          CALL dumps(IObad)
        ENDIF
        wtset(111) = 0
      ENDIF
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!+++++++++++++++++++ PRINCIPAL WEIGHT ++++++++++++++++++++++++++!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
      wtset(101) = wtset(111)
      END

      FUNCTION SoftFm(p1,q1,p2,q2,pk)
*     *******************************
!---------------------------------------------------------------!
! This function provides a value of the soft factor S-tilde     !
! for massless fermions (factor (alpha/4pi^2) omitted!).        !
! INPUT:  p1(4),q1(4),p2(4),q2(4) - fermion 4-momenta           !
!                           pk(4) - photon  4-momentum          !
!---------------------------------------------------------------!
! Written by: Wieslaw Placzek              Knoxville, May 1995  !
! Last update: 14.07.1995       by: W.P.                        !
!---------------------------------------------------------------!
      implicit REAL*8 (a-h,o-z)
      COMMON / BHPAR2 / CMSENE,AMEL
      SAVE   / BHPAR2 /
      REAL*8 p1(4) ,q1(4) ,p2(4) ,q2(4) ,pk(4)
!
      ame = AMEL
! Scalar products of various 4-momenta
      p1k = p1(4)*pk(4)-p1(3)*pk(3)-p1(2)*pk(2)-p1(1)*pk(1)
      q1k = q1(4)*pk(4)-q1(3)*pk(3)-q1(2)*pk(2)-q1(1)*pk(1)
      p2k = p2(4)*pk(4)-p2(3)*pk(3)-p2(2)*pk(2)-p2(1)*pk(1)
      q2k = q2(4)*pk(4)-q2(3)*pk(3)-q2(2)*pk(2)-q2(1)*pk(1)
      p1q1 = p1(4)*q1(4)-p1(3)*q1(3)-p1(2)*q1(2)-p1(1)*q1(1)
      p1p2 = p1(4)*p2(4)-p1(3)*p2(3)-p1(2)*p2(2)-p1(1)*p2(1)
      p1q2 = p1(4)*q2(4)-p1(3)*q2(3)-p1(2)*q2(2)-p1(1)*q2(1)
      q1p2 = q1(4)*p2(4)-q1(3)*p2(3)-q1(2)*p2(2)-q1(1)*p2(1)
      q1q2 = q1(4)*q2(4)-q1(3)*q2(3)-q1(2)*q2(2)-q1(1)*q2(1)
      p2q2 = p2(4)*q2(4)-p2(3)*q2(3)-p2(2)*q2(2)-p2(1)*q2(1)
! Soft factor
      sfml = 2*( p1p2/(p1k*p2k) +p1q1/(p1k*q1k) -p1q2/(p1k*q2k)
     &          -q1p2/(q1k*p2k) +p2q2/(p2k*q2k) +q1q2/(q1k*q2k) )
      sfmt = ame**2 *(1/p1k**2 +1/q1k**2 +1/p2k**2 +1/q2k**2)
      SoftFm = sfml - sfmt
      END
      SUBROUTINE  vacpol(KeyPia,Q2,SINW2,RePiE,dRePiE)
!     ************************************************
!  Interface to various calculations of real part RePi
!  of cacuum polarization on the photon line.
!  Input:
!     Q2 = transfer GeV**2, negative for spacelike
!  Output:
!     RePiE  = Real Part Vacuum polarization on photon line
!              Leptonic + hadronic.
!     dRePiE = absolute error of RePiE
!     ***************************
      IMPLICIT REAL*8(A-H,O-Z)
      SAVE
      REAL *4 enq,st2,der,errder,deg,errdeg

      QQ=Q2
      IF(KeyPia .EQ. 0) THEN
        RePiE  = 0d0
        dRePiE = 0d0
      ELSEIF(KeyPia .EQ. 1) THEN
! From Burkhardt 1981,1989
        RePiE  = REPI(QQ)
        dRePiE = 0d0
      ELSEIF(KeyPia .EQ. 2) THEN
! From S. Eidelman, F. Jegerlehner, Z. Phys. C (1995)
        enq= QQ/sqrt(abs(QQ))
        st2= SINW2
        CALL hadr5(enq,st2,der,errder,deg,errdeg)
! Leptonic part taken from Burkhardt
        RePiE  = -der + REPIL(QQ)  ! total
        dRePiE = errder
      ELSEIF(KeyPia .EQ. 3) THEN
! From Burkhardt and Pietrzyk 1995 (Moriond)
        RePiE  = REPI95(QQ)
        dRePiE = 0d0
      ELSE
        WRITE(6,*) 'STOP in VACPOL ++++ WRONG KeyPia=', KeyPia
        STOP
      ENDIF
      END

      FUNCTION REPIL(S)
!-------------------------------------------- REMARKS ---------------
! VACUUM POLARIZATION IN QED. THE LEPTONIC CONTRIBUTION IS AN ANALY
! EXPRESSION INVOLVING THE LEPTON MASS; THE HADRONIC CONTRIBUTION IS
! A DISPERSION INTEGRAL OVER THE KNOWN HADRONIC CROSS SECTION. THE
! RESULT USED HERE IS A PARAMETRIZATION GIVEN BY
! H.BURKHARDT, TASSO NOTE 192(1981).
! updated see H.Burkhardt et al. Pol. at Lep CERN 88-06 VOL I
! lepton masses now fully taken into account, H.Burkhardt June 89
!--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 BETA
      REAL*8 M(3)
!
      DATA A1,B1,C1/   0.0   ,   0.00835,  1.0   /
      DATA A2,B2,C2/   0.0   ,   0.00238,  3.927 /
      DATA A3,B3,C3/ 0.00165 ,   0.00300,  1.0   /
      DATA A4,B4,C4/ 0.00221 ,   0.00293,  1.0   /
!
      DATA M/0.51099906D-3,0.10565839D0,1.7841D0/

      SAVE A1,B1,C1,A2,B2,C2,A3,B3,C3,A4,B4,C4,M
      SAVE I, AL3PI
!
!     for leptons use F,P functions see Burgers, Hollik etc.
!     F(s,m1,m2) for m1=m2 depends only on beta = sqrt((1-4m**2)/s)
      FSYM(BETA)=2.D0+BETA*LOG( (BETA-1.D0)/(BETA+1.D0) )
      P(S,XM,BETA)=1.D0/3.D0-(1.D0+2.D0*XM**2/S) * FSYM(BETA)
!     asymptotic formula for high energies (real part)
      PASYM(S,XM)=-5.D0/3.D0 - LOG (ABS(XM**2/S))
!
!---------------------------------- init and  CHECK FOR S VALUES ----
      DATA I/0/
      IF(I.EQ.0) THEN
        I=1
        AL3PI=1./ (3.D0 * 137.0359895D0 * 3.141592653589793D0)
        IF(S.GT.0.D0.AND.S.LT.100.D0)
     .  WRITE(6,'(3H0S=,F6.3,7H GEV**2,/,
     .    46H VACUUM POLARIZATION MAY BE BADLY APPROXIMATED)')
      ENDIF
!-------------------------------------------- LEPTONIC PART ---------
      REPIL=0.D0
!     loop over leptons
      DO 1 I=1,3
        IF(ABS(S).GT.1.D3*M(I)**2) THEN
!         asymptotic formula for s,t >> m**2
          REPIL=REPIL-PASYM(S,M(I))
        ELSE
          BETA=1.D0-4.D0*M(I)**2/S
          BETA=SQRT(BETA)
          REPIL=REPIL-P(S,M(I),BETA)
        ENDIF
    1 CONTINUE
      REPIL=AL3PI*REPIL
      END

      FUNCTION REPI95(S)
C-------------------------------------------- REMARKS ---------------
C Vacuum polarization in QED. The leptonic contribution is an analytic
C expression involving the lepton mass; the hadronic contribution is fro
C a dispersion integral over the known hadronic cross section. The
C result used here is a Parametrization given by
C H.Burkhardt, TASSO note 192(1981).
C updated by H.Burkhardt, B.Pietrzyk; see H.Burkhardt in proceedings
C of electroweak Moriond 1995 and   Burkhardt, Pietrzyk to be published
C--------+---------+---------+---------+---------+---------+---------+--
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 BETA
      REAL*8 M(3)
      PARAMETER (NREG=5)
      DIMENSION WREG(0:NREG),AREG(NREG),BREG(NREG),CREG(NREG)
C
      DATA WREG/-0.4,-2.,-4.,-10.,91.2,100 000./
      DATA AREG/0.,0.,0.,0.00122270,0.00164178/
      DATA BREG/0.00228770,0.00251507,0.00279328,0.00296694,0.00292051/
      DATA CREG/4.08041425,3.09624477,2.07463133,1.0,1.0/
C
      DATA M/0.51099906D-3,0.10565839D0,1.7841D0/
C
C     for leptons use F,P functions see Burgers, Hollik etc.
C     F(s,m1,m2) for m1=m2 depends only on beta = sqrt((1-4m**2)/s)
      FSYM(BETA)=2.D0+BETA*LOG( (BETA-1.D0)/(BETA+1.D0) )
      P(S,XM,BETA)=1.D0/3.D0-(1.D0+2.D0*XM**2/S) * FSYM(BETA)
C     asymptotic formula for high energies (real part)
      PASYM(S,XM)=-5.D0/3.D0 - LOG (ABS(XM**2/S))
C
C---------------------------------- init and  CHECK FOR S VALUES ----
      DATA I/0/
      IF(I.EQ.0) THEN
        I=1
        AL3PI=1./ (3.D0 * 137.0359895D0 * 3.141592653589793D0)
        IF(S.GT.0.D0.AND.S.LT.100.D0)
     .  WRITE(6,'(3H0S=,F6.3,7H GeV**2,/,
     .    46H vacuum polarization may be badly approximated)') S
      ENDIF
C-------------------------------------------- LEPTONIC PART ---------
      REPI95=0.D0
C     loop over leptons
      DO I=1,3
        IF(ABS(S).GT.1.D3*M(I)**2) THEN
C         asymptotic formula for s,t >> m**2
          REPI95=REPI95-PASYM(S,M(I))
        ELSE
          BETA=1.D0-4.D0*M(I)**2/S
          BETA=SQRT(BETA)
          REPI95=REPI95-P(S,M(I),BETA)
        ENDIF
      ENDDO
      REPI95=AL3PI*REPI95
C-------------------------------------------- hadronic part ---------
      T=ABS(S)
      DO I=1,NREG
        IREG=I
        IF(T.LT.WREG(I)**2) GOTO 1
      ENDDO
    1 CONTINUE
      HADPI=AREG(IREG)+BREG(IREG)*LOG(1.+CREG(IREG)*T)
C     WRITE(6,*) ' SQRTS=',SQRT(T),' leptonic part=',REPI95,
C    .  ' hadronic part=',HADPI,' total=',REPI95-HADPI
      REPI95=REPI95-HADPI
      END

      FUNCTION REPI(S)
!-------------------------------------------- REMARKS ---------------
! VACUUM POLARIZATION IN QED. THE LEPTONIC CONTRIBUTION IS AN ANALY
! EXPRESSION INVOLVING THE LEPTON MASS; THE HADRONIC CONTRIBUTION IS
! A DISPERSION INTEGRAL OVER THE KNOWN HADRONIC CROSS SECTION. THE
! RESULT USED HERE IS A PARAMETRIZATION GIVEN BY
! H.BURKHARDT, TASSO NOTE 192(1981).
! updated see H.Burkhardt et al. Pol. at Lep CERN 88-06 VOL I
! lepton masses now fully taken into account, H.Burkhardt June 89
!--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 BETA
      REAL*8 M(3)
!
      DATA A1,B1,C1/   0.0   ,   0.00835,  1.0   /
      DATA A2,B2,C2/   0.0   ,   0.00238,  3.927 /
      DATA A3,B3,C3/ 0.00165 ,   0.00300,  1.0   /
      DATA A4,B4,C4/ 0.00221 ,   0.00293,  1.0   /
!
      DATA M/0.51099906D-3,0.10565839D0,1.7841D0/

      SAVE A1,B1,C1,A2,B2,C2,A3,B3,C3,A4,B4,C4,M
      SAVE I, AL3PI
!
!     for leptons use F,P functions see Burgers, Hollik etc.
!     F(s,m1,m2) for m1=m2 depends only on beta = sqrt((1-4m**2)/s)
      FSYM(BETA)=2.D0+BETA*LOG( (BETA-1.D0)/(BETA+1.D0) )
      P(S,XM,BETA)=1.D0/3.D0-(1.D0+2.D0*XM**2/S) * FSYM(BETA)
!     asymptotic formula for high energies (real part)
      PASYM(S,XM)=-5.D0/3.D0 - LOG (ABS(XM**2/S))
!
!---------------------------------- init and  CHECK FOR S VALUES ----
      DATA I/0/
      IF(I.EQ.0) THEN
        I=1
        AL3PI=1./ (3.D0 * 137.0359895D0 * 3.141592653589793D0)
        IF(S.GT.0.D0.AND.S.LT.100.D0)
     .  WRITE(6,'(3H0S=,F6.3,7H GEV**2,/,
     .    46H VACUUM POLARIZATION MAY BE BADLY APPROXIMATED)')
      ENDIF
!-------------------------------------------- LEPTONIC PART ---------
      REPI=0.D0
!     loop over leptons
      DO 1 I=1,3
        IF(ABS(S).GT.1.D3*M(I)**2) THEN
!         asymptotic formula for s,t >> m**2
          REPI=REPI-PASYM(S,M(I))
        ELSE
          BETA=1.D0-4.D0*M(I)**2/S
          BETA=SQRT(BETA)
          REPI=REPI-P(S,M(I),BETA)
        ENDIF
    1 CONTINUE
      REPI=AL3PI*REPI
!-------------------------------------------- HADRONIC PART ---------
      X=DABS(S)
      IF(X.LT.0.3**2) THEN
        REPI=REPI- (A1+B1*LOG(1.+C1*X))
      ELSEIF(X.LT.3.**2) THEN
        REPI=REPI- (A2+B2*LOG(1.+C2*X))
      ELSEIF(X.LT.100.**2) THEN
        REPI=REPI- (A3+B3*LOG(1.+C3*X))
      ELSE
        REPI=REPI- (A4+B4*LOG(1.+C4*X))
      ENDIF
      END

!Return-Path: <jegerlehner@PSICLU.CERN.CH>
!Date: Mon, 20 Mar 1995 10:30:49 +0100
!From: jegerlehner@psiclu.cern.ch (Fred Jegerlehner)
!X-Vms-To: DXMINT::jadach@cernvm.cern.ch
!X-Vms-Cc: JEGERLEHNER
!Subject: Re: hadronic vacuum polarization
!X-Mail11-Ostype: VAX/VMS
!Apparently-To: <jadach@cernvm.cern.ch>
!Dear Staszek,
!the uncertainty of course depends on the energy and goes to zero as E -> 0.
!The routine I have sent you gives you the uncertainty for a given energy.
!At 1 GeV spacelike I get
!     E        delta alpha   uncertainty
! -1.0000E+00   3.7362E-03   9.2977E-05
!A copy of our paper you may get by anonymous ftp as follows:
!ftp 129.129.40.58
!anonymous
!username
!cd pub
!cd preprints
!get vapogm2.ps.gz

       subroutine hadr5(e,st2,der,errder,deg,errdeg)
c ******************************************************************
c *                                                                *
c *      subroutine for the evaluation of the light hadron         *
c *           contributions to Delta_r  and  Delta_g               *
c *                    using fits to the                           *
c *          QED vacuum polarization from e^+ e^- data             *
c *                                                                *
c *    F. Jegerlehner, Paul Scherrer Institute, CH-5232 Villigen   *
c *                                                                *
c *    E-mail:jegerlehner@cvax.psi.ch                              *
c *    Phone :   +41-56-993662                                     *
c *                                                                *
c *    Reference: F. Jegerlehner, Z. Phys. C32 (1986) 195          *
c *               H. Burkhardt et al., Z. Phys. C42 (1989) 497     *
c *               S. Eidelman, F. Jegerlehner, Z. Phys. C (1995)   *
c *                                                                *
c ******************************************************************
c       VERSION: 24/02/1995
c
C  Notation: E energy ( momentum transfer ): E>0 timelike , E<0 spacelike
C            st2 is sin^2(Theta); st2=0.2322 is the reference value
C  the routine returns the hadronic contribution of 5 flavors (u,d,s,c,b)
C                 to   DER=Delta_r with hadronic error ERRDER
C                and   DEG=Delta_g with hadronic error ERRDEG
C  The effective value of the fine structure constant alphaQED at energy
C  E is alphaQED(E)=alphaQED(0)/(1-Delta_r) ,similarly for the SU(2)
C  coupling alphaSU2(E)=alphaSU2(0)/(1-Delta_g), where Delta_r(g) is the
C  sum of leptonic, hadronic contributions (top to be added).
C
C  This program does not yet know how to compute Delta r and Delta g for
C  energies in the ranges  |E|>1TeV and 2m_pi < E < 40(13) GeV !!!!!!!!!
C
       implicit none
       integer *2 nf,ns,i,j
       parameter(nf=9,ns=4)
       real *4 e,st2,st20,der,deg,errder,errdeg,s,s0,x1,xi,x2,xlog,xlar
       real *4 m1(nf),c1(nf,ns),c2(nf,ns),c3(nf,ns),c4(nf,ns),ae(nf,ns)
       real *4 eu(nf),eo(nf),res(ns),l1(nf,ns),fx,gx,hx,xx,u,Se
       do i=1,nf
         do j=1,ns
           ae(i,j)=0.0
         enddo
       enddo
c #1# Delta_r
c Fit parameters spacelike  -1000 to  -200 GeV
      eu(1)  =-1000.
      eo(1)  = -200.
      m1(1)=  -1000.000
      c1(1,1)=  4.2069394e-02
      c2(1,1)=  2.9253566e-03
      c3(1,1)= -6.7782454e-04
      c4(1,1)=  9.3214130e-06
c   chi2=  2.5763808e-05
c Fit parameters spacelike  -200 to  -20 GeV
      eu(2)  = -200.
      eo(2)  =  -20.
      m1(2)  =  -100.0000
      c1(2,1)=  2.8526291e-02
      c2(2,1)=  2.9520725e-03
      c3(2,1)= -2.7906310e-03
      c4(2,1)=  6.4174528e-05
c   chi2=  6.6264300e-04
c Fit parameters spacelike   -20 to   -2 GeV
      eu(3)  =  -20.
      eo(3)  =   -2.
      m1(3)  =   -20.0000
      l1(3,1)=  9.3055e-3
      c1(3,1)=  2.8668314e-03
      c2(3,1)=  0.3514608
      c3(3,1)=  0.5496359
      c4(3,1)=  1.9892334e-04
c   chi2=  4.2017717e-03
      ae(3,1)=  3.0
c Fit parameters spacelike    -2 to    0.25 GeV
      eu(4)  =   -2.
      eo(4)  =    0.25
      m1(4)  =    -2.0000
      l1(4,1)=  9.3055e-3
      c1(4,1)=  2.2694240e-03
      c2(4,1)=   8.073429
      c3(4,1)=  0.1636393
      c4(4,1)= -3.3545541e-05
c   chi2=  0.1239052
      ae(4,1)=  2.0
c Fit parameters timelike   0.25 to    2 GeV
      eu(5)  =    0.25
      eo(5)  =    2.
c Fit parameters timelike   2    to   40 GeV
      eu(6)  =    2.
      eo(6)  =   40.
c Fit parameters timelike     40 to   80 GeV
      eu(7)  =   40.
      eo(7)  =   80.
      m1(7)  =   80.00000
      c1(7,1)=  2.7266588e-02
      c2(7,1)=  2.9285045e-03
      c3(7,1)= -4.7720564e-03
      c4(7,1)=  7.7295507e-04
c   chi2=  7.7148885e-05
c Fit parameters timelike     80 to  250 GeV
      eu(8)  =   80.
      eo(8)  =  250.
      m1(8)  =   91.18880
      c1(8,1)=  2.8039809e-02
      c2(8,1)=  2.9373798e-03
      c3(8,1)= -2.8432352e-03
      c4(8,1)= -5.2537734e-04
c   chi2=  4.2241514e-05
c Fit parameters timelike    250 to 1000 GeV
      eu(9)  =  250.
      eo(9)  = 1000.
      m1(9)  = 1000.00000
      c1(9,1)=  4.2092260e-02
      c2(9,1)=  2.9233438e-03
      c3(9,1)= -3.2966913e-04
      c4(9,1)=  3.4324117e-07
c   chi2=  6.0426464e-05
c #2# Delta_g
c Fit parameters spacelike  -1000 to  -200 GeV
c     eu(1)  =-1000.
c     eo(1)  = -200.
c     m1(1)=  -1000.000
      c1(1,2)=  8.6415343e-02
      c2(1,2)=  6.0127582e-03
      c3(1,2)= -6.7379221e-04
      c4(1,2)=  9.0877611e-06
c   chi2=  9.6284139e-06
c Fit parameters spacelike  -200 to  -20 GeV
c     eu(2)  = -200.
c     eo(2)  =  -20.
c     m1(2)  =  -100.0000
      c1(2,2)=  5.8580618e-02
      c2(2,2)=  6.0678599e-03
      c3(2,2)= -2.4153464e-03
      c4(2,2)=  6.1934326e-05
c   chi2=  6.3297758e-04
c Fit parameters spacelike   -20 to   -2 GeV
c     eu(3)  =  -20.
c     eo(3)  =   -2.
c     m1(3)  =   -20.0000
      l1(3,2)=  1.9954e-2
      c1(3,2)=  5.7231588e-03
      c2(3,2)=  0.3588257
      c3(3,2)=  0.5532265
      c4(3,2)=  6.0730567e-04
c   chi2=  7.9884287e-03
      ae(3,2)=  3.0
c   chi2=  4.2017717e-03
c Fit parameters spacelike    -2 to    0.25 GeV
c     eu(4)  =   -2.
c     eo(4)  =    0.25
c     m1(4)  =    -2.0000
      l1(4,2)=  1.9954e-2
      c1(4,2)=  4.8065037e-03
      c2(4,2)=   8.255167
      c3(4,2)=  0.1599882
      c4(4,2)= -1.8624817e-04
c   chi2=  0.1900761
      ae(3,2)=  2.0
c Fit parameters timelike     40 to   80 GeV
c     eu(7)  =   40.
c     eo(7)  =   80.
c     m1(7)  =   80.00000
      c1(7,2)=  5.5985276e-02
      c2(7,2)=  6.0203830e-03
      c3(7,2)= -5.0066952e-03
      c4(7,2)=  7.1363564e-04
c   chi2=  7.6000040e-05
c Fit parameters timelike     80 to  250 GeV
c     eu(8)  =   80.
c     eo(8)  =  250.
c     m1(8)  =   91.18880
      c1(8,2)=  5.7575710e-02
      c2(8,2)=  6.0372148e-03
      c3(8,2)= -3.4556778e-03
      c4(8,2)= -4.9574347e-04
c   chi2=  3.3244669e-05
c Fit parameters timelike    250 to 1000 GeV
c     eu(9)  =  250.
c     eo(9)  = 1000.
c     m1(9)  = 1000.00000
      c1(9,2)=  8.6462371e-02
      c2(9,2)=  6.0088057e-03
      c3(9,2)= -3.3235471e-04
      c4(9,2)=  5.9021050e-07
c   chi2=  2.9821187e-05
c #3# error Delta_r
c Fit parameters spacelike  -1000 to  -200 GeV
c     eu(1)  =-1000.
c     eo(1)  = -200.
c     m1(1)=  -1000.000
      c1(1,3)=  6.3289929e-04
      c2(1,3)=  3.3592437e-06
      c3(1,3)=  0.0
      c4(1,3)=  0.0
c   chi2=  2.3007713E-05
c Fit parameters spacelike  -200 to  -20 GeV
c     eu(2)  = -200.
c     eo(2)  =  -20.
c     m1(2)  =  -100.0000
      c1(2,3)=  6.2759849e-04
      c2(2,3)= -1.0816625e-06
      c3(2,3)=   5.050189
      c4(2,3)= -9.6505374e-02
c   chi2=  3.4677869e-04
      ae(2,3)=  1.0
c Fit parameters spacelike   -20 to   -2 GeV
c     eu(3)  =  -20.
c     eo(3)  =   -2.
c     m1(3)  =   -20.0000
      l1(3,3)=  2.0243e-4
      c1(3,3)=  1.0147886e-04
      c2(3,3)=   1.819327
      c3(3,3)= -0.1174904
      c4(3,3)= -1.2404939e-04
c   chi2=  7.1917898e-03
      ae(3,3)=  3.0
c Fit parameters spacelike    -2 to    0.25 GeV
c     eu(4)  =   -2.
c     eo(4)  =    0.25
c     m1(4)  =    -2.0000
      l1(4,3)=  2.0243e-4
      c1(4,3)= -7.1368617e-05
      c2(4,3)=  9.980347e-04
      c3(4,3)=   1.669151
      c4(4,3)=  3.5645600e-05
c   chi2=  0.1939734
      ae(4,3)=  2.0
c Fit parameters timelike     40 to   80 GeV
c     eu(7)  =   40.
c     eo(7)  =   80.
c     m1(7)  =   80.00000
      c1(7,3)=  6.4947648e-04
      c2(7,3)=  4.9386853e-07
      c3(7,3)=  -55.22332
      c4(7,3)=   26.13011
c   chi2=  7.2068366e-04
c Fit parameters timelike     80 to  250 GeV
c     eu(8)  =   80.
c     eo(8)  =  250.
c     m1(8)  =   91.18880
      c1(8,3)=  6.4265809e-04
      c2(8,3)= -2.8453374e-07
      c3(8,3)=  -23.38172
      c4(8,3)=  -6.251794
c   chi2=  1.1478480e-07
c Fit parameters timelike    250 to 1000 GeV
c     eu(9)  =  250.
c     eo(9)  = 1000.
c     m1(9)  = 1000.00000
      c1(9,3)=  6.3369947e-04
      c2(9,3)= -2.0898329e-07
      c3(9,3)=  0.0
      c4(9,3)=  0.0
c   chi2=  2.9124376E-06
c #4# error Delta_g
c Fit parameters spacelike  -1000 to  -200 GeV
c     eu(1)  =-1000.
c     eo(1)  = -200.
c     m1(1)=  -1000.000
      c1(1,4)=  1.2999176e-03
      c2(1,4)=  7.4505529e-06
      c3(1,4)=  0.0
      c4(1,4)=  0.0
c   chi2=  2.5312527E-05
c Fit parameters spacelike  -200 to  -20 GeV
c     eu(2)  = -200.
c     eo(2)  =  -20.
c     m1(2)  =  -100.0000
      c1(2,4)=  1.2883141e-03
      c2(2,4)= -1.3790827e-06
      c3(2,4)=   8.056159
      c4(2,4)= -0.1536313
c   chi2=  2.9774895e-04
      ae(2,4)=  1.0
c Fit parameters spacelike   -20 to   -2 GeV
c     eu(3)  =  -20.
c     eo(3)  =   -2.
c     m1(3)  =   -20.0000
      l1(3,4)=  4.3408e-4
      c1(3,4)=  2.0489733e-04
      c2(3,4)=   2.065011
      c3(3,4)= -0.6172962
      c4(3,4)= -2.5603661e-04
c   chi2=  7.5258738e-03
      ae(3,4)=  3.0
c Fit parameters spacelike    -2 to    0.25 GeV
c     eu(4)  =   -2.
c     eo(4)  =    0.25
c     m1(4)  =    -2.0000
      l1(4,4)=  4.3408e-4
      c1(4,4)= -1.5095409e-04
      c2(4,4)=  9.9847501e-04
      c3(4,4)=   1.636659
      c4(4,4)=  7.5892596e-05
c   chi2=  0.1959371
      ae(4,4)=  2.0
c Fit parameters timelike     40 to   80 GeV
c     eu(7)  =   40.
c     eo(7)  =   80.
c     m1(7)  =   80.00000
      c1(7,4)=  1.3335156e-03
      c2(7,4)=  2.2939612e-07
      c3(7,4)=  -246.4966
      c4(7,4)=   114.9956
c   chi2=  7.2293193e-04
c Fit parameters timelike     80 to  250 GeV
c     eu(8)  =   80.
c     eo(8)  =  250.
c     m1(8)  =   91.18880
      c1(8,4)=  1.3196438e-03
      c2(8,4)=  2.8937683e-09
      c3(8,4)=   5449.778
      c4(8,4)=   930.3875
c   chi2=  4.2109136e-08
c Fit parameters timelike    250 to 1000 GeV
c     eu(9)  =  250.
c     eo(9)  = 1000.
c     m1(9)  = 1000.00000
      c1(9,4)=  1.3016918e-03
      c2(9,4)= -3.6027674e-07
      c3(9,4)=  0.0
      c4(9,4)=  0.0
c   chi2=  2.8220852E-06
C ######################################################################
       Se=654./643.      ! rescaling error to published version 1995
       st20=0.2322
       s=e**2
       der=0.0
       deg=0.0
       errder=0.0
       errdeg=0.0
       if ((e.gt.1.e3).or.(e.lt.-1.e3)) goto 100
       if ((e.lt.4.e1).and.(e.gt.0.25)) goto 100
       i=1
       do while (e.ge.eo(i))
         i=i+1
       enddo
       if (e.eq.1.e3) i=9
       if (e.eq.0.0 ) goto 100
       s0=sign(1.0,m1(i))*m1(i)**2
       s =sign(1.0,e)*e**2
       x1=s0/s
       xi=1.0/x1
       x2=x1**2
       if (ae(i,1).le.0.0) then
         do j=1,4
           xlar=xi+ae(i,j)*exp(-xi)
           xlog=log(xlar)
           res(j)=c1(i,j)
     .           +c2(i,j)*(xlog+c3(i,j)*(x1-1.0)+c4(i,j)*(x2-1.0))
         enddo
       else if (ae(i,1).eq.2.0) then
         hx     =xi**2
         do j=1,2
           fx     =1.0-c2(i,j)*s
           gx     = c3(i,j)*s/(c3(i,j)-s)
           xx     =log(abs(fx))+c2(i,j)*gx
           res(j)=c1(i,j)*xx-l1(i,j)*gx+c4(i,j)*hx
         enddo
         do j=3,4
           u      =abs(s)
           gx     =-c3(i,j)*u/(c3(i,j)+u)
           xx     =xi**3/(sqrt(abs(xi))**5+c2(i,j))
           res(j)=c1(i,j)*xx-l1(i,j)*gx+c4(i,j)*hx
         enddo
       else if (ae(i,1).eq.3.0) then
         hx     =xi
         do j=1,4
           fx     =1.0-c2(i,j)*s
           gx     = c3(i,j)*s/(c3(i,j)-s)
           xx     =log(abs(fx))+c2(i,j)*gx
           res(j)=c1(i,j)*xx-l1(i,j)*gx+c4(i,j)*hx
         enddo
       endif
       der=res(1)
       deg=res(2)*st20/st2
       errder=res(3)*Se
       errdeg=res(4)*Se
       goto 100
 99    write(*,*) ' out of range! '
100    return
       end

!======================================================================
!======================= G L I B K  ===================================
!==================General Library of utilities========================
!===========It is similar but not identical to HBOOK and HPLOT=========
!======================================================================
!
!                      Version:    1.20
!              Last correction:    September 1996
!
!
!  Installation remarks:
!  (1) printing backslash character depends on F77 compilator,
!      user may need to modify definition of BS variable in HPLCAP
!
!  Usage of the program:
!  (1) In most cases names and meanings of programs and their
!      parameters is the same as in original CERN libraries HBOOK
!  (2) Unlike to original HBOOK and HPLOT, all floating parameters
!      of the programs are in double precision!
!  (3) GLIBK stores histograms in double precision and always with
!      errors. REAL*8 storage is essential for 10**7 events statistics!
!  (4) Output from GLIBK is a picture recorded as regular a LaTeX file
!      with frame and curves/histograms, it is easy to change fonts
!      add captions, merge plots, etc. by normal editing. Finally,
!      picture may be inserted in any place into LaTeX source of the
!      article.
!  (5) WARNING: two-dimensional histograms are not active!!!
!
!  ********************************************************************
!  *  History of the program:                                         *
!  *  MINI-HBOOK writen by S. Jadach, Rutherford Lab. 1976            *
!  *  Rewritten December 1989 (S.J.)                                  *
!  *  Version with DOUBLE PRECISION ARGUMENTS ONLY!  and SAVE         *
!  *  Subprogram names start with G instead of H letter!               *
!  *  Entries:   Obligatory:  GLIMIT                                  *
!  *             Optional: see table below                            *
!  *  non-user subprograms in brackets                                *
!  ********************************************************************
!    SUBR/FUNC  1 PAR. 2 PAR. 3 PAR. 4 PAR. 5 PAR. 6 PAR.
!  ====================================================================
*     (G_INIT)  ----   ----    ----   ----   ----   ----
*      GI       INT    INT     ----   ----   ----   ----
*      GIE      INT    INT     ----   ----   ----   ----
*      GF1      INT    DBL     DBL    ----   ----   ----
*      GFILL    INT    DBL     DBL    DBL    ----   ----
*      GBOOK1   INT    CHR*80  INT    DBL    DBL    ----
*     (GOPTOU)  INT    INT     INT    INT    INT     INT
* (L.F. GEXIST) INT    -----  ------  ----   ----   ----
*      GIDOPT   INT    CHR*4   -----  ----   ----   ----
*      GBFUN1   INT    CHR*80   INT   DBL    DBL  DP-FUNC
*      GIDOPT   INT    CHR*4   -----  ----   ----   ----
*      GBOOK2   INT    CHR*80   INT   DBL    DBL     INT   DBL   DBL
*      GISTDO     ---   ----   ----   ----   ----   ----
*      GOUTPU   INT     ----   ----   ----   ----   ----
*      GPRINT   INT     ----   ----   ----   ----   ----
*      GOPERA   INT    CHR*1   INT    INT    DBL    DBL
*      GINBO1   INT    CHR*8   INT    DBL    DBL    ----
*      GUNPAK   INT    DBL(*) CHR*(*) INT    ---    ----
*      GPAK     INT    DBL(*)  ----   ----   ---    ----
*      GPAKE    INT    DBL(*)  ----   ----   ---    ----
*      GRANG1   INT    DBL     DBL    ----   ---    ----
*      GINBO2   INT    INT     DBL    DBL    INT    DBL   DBL
*      GMAXIM   INT    DBL     ----   ----   ---    ----
*      GMINIM   INT    DBL     ----   ----   ---    ----
*      GRESET   INT   CHR*(*)  ----   ----   ---    ----
*      GDELET   INT     ----   ----   ----   ----   ----
*      GLIMIT   INT     ----   ----   ----   ----   ----
*     (COPCH)   CHR*80 CHR*80  ----   ----   ----   ----
* (F. JADRES)   INT     ----   ----   ----   ----   ----
*      GRFILE   INT   CHR*(*) CHR*(*) ----   ----   ----
*      GROUT    INT    INT    CHR*8   ----   ----   ----
*      GRIN     INT    INT     INT    ----   ----   ----
*      GREND   CHR*(*) ----    ----   ----   ----   ----
!  *******************  HPLOT entries ******************
*      GPLINT   INT    ----    ----   ----   ----   ----
*      GPLCAP   INT    ----    ----   ----   ----   ----
*      GPLEND   ----   ----    ----   ----   ----   ----
*      GPLOT    INT    CHR*1   CHR*1   INT   ----   ----
*     (LFRAM1)  INT      INT     INT  ----   ----   ----
*     (SAXIX)   INT      DBL     DBL   INT    DBL   ----
*     (SAXIY)   INT      DBL     DBL   INT    DBL   ----
*     (PLHIST)  INT      INT     DBL   DBL    INT    INT
*     (PLHIS2)  INT      INT     DBL   DBL    INT    INT
*     (PLCIRC)  INT      INT     INT   DBL    DBL    DBL
*     (APROF)   DBL      INT     DBL  ----   ----   ----
*      GPLSET   INT      DBL    ----  ----   ----   ----
*      GPLTIT   INT    CHR*80   ----  ----   ----   ----
!  *******************  WMONIT entries ******************
*      GMONIT   INT ???
!  *******************************************************************
!                         END OF TABLE
!  *******************************************************************
*          Map of memory for single histogram
*          ----------------------------------
*  (1-7) Header
*  ist +1   mark      9999999999999
*  ist +2   mark      9d12 + id*10 + 9
*  ist +3   iflag1    9d12 + iflag1*10 +9
*  ist +4   iflag2    9d12 + iflag2*10 +9
*  ist +5   scamin    minimum y-scale
*  ist +6   scamax    maximum y-scale
*  ist +7   jdlast    address of the next histogram
*                     from previous history of calls (see jadres)
*          ----------------------------------
*              Binning size informations
*          ----------------------------------
*  One dimensional histogram            Two dimensional histog.
*  -------------------------            ----------------------
*  (8-11) Binning information           (8-15) Binning information
*  ist2 +1    NCHX                          ist2 +5   NCHY
*  ist2 +2      XL                          ist2 +6     YL
*  ist2 +3      XU                          ist2 +7     YU
*  ist2 +4   FACTX                          ist2 +8  FACTY
*
*          ----------------------------------
*           All kind of sums except of maxwt
*          ----------------------------------
*  (12-24) Under/over-flow average x    (16-24)
*  ist3 +1   Underflow                     All nine combinations
*  ist3 +2   Normal                        (U,N,O) x (U,N,O)
*  ist3 +3   Overerflow                    sum wt only (no errors)
*  ist3 +4   U  sum w**2
*  ist3 +5   N  sum w**2
*  ist3 +6   O  sum w**2
*  ist3 +7   Sum 1
*  ist3 +8   Sum wt*x
*  ist3 +9   Sum wt*x*x
*  ist3 +10  nevzer    (gmonit)
*  ist3 +11  nevove    (gmonit)
*  ist3 +12  nevacc    (gmonit)
*  ist3 +13  maxwt     (gmonit)
*          ----------------------------------
*           Content of bins including errors
*          ----------------------------------
*  (25 to 24+2*nchx)                     (25 to 24 +nchx*nchy)
*     sum wt and sum wt**2            sum wt only (no errors)
*  ----------------------------------------------------------------

      SUBROUTINE g_init
*     ****************
! First Initialization called from may routines
*     *************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      SAVE   / cglib /
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DATA init /0/
      SAVE init
*
      IF(init .NE. 0) RETURN
      init=1
! this is version version number
      nvrs=111
! default output unit
      nout=16
      lenmax=0
      length=0
      DO i=1,idmx
         DO k=1,3
            index(i,k)=0
         ENDDO
         DO k=1,80
            titlc(i)(k:k)=' '
         ENDDO
      ENDDO
      DO k=1,50000
         b(k)=0d0
      ENDDO
      END

      SUBROUTINE gflush
*     ****************
! FLUSH memory, all histos erased!
*     *************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      SAVE   / cglib /
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/

      CALL g_init
      length=0
      DO i=1,idmx
         DO k=1,3
            index(i,k)=0
         ENDDO
         DO k=1,80
            titlc(i)(k:k)=' '
         ENDDO
      ENDDO
      DO k=1,50000
         b(k)=0d0
      ENDDO
      END

      LOGICAL FUNCTION gexist(id)
!     ***************************
! this function is true when id  exists !!!!
!     ***************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      gexist = lact .NE. 0
!###  IF(gexist)      WRITE(6,*) 'gexist: does   ID,lact= ',id,lact
!###  IF(.not.gexist) write(6,*) 'gexist: doesnt ID,lact= ',id,lact
      END

      FUNCTION gi(id,ib)
!     ******************
! getting out bin content
! S.J. 18-Nov. 90
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      SAVE idmem,nch,lact,ist,ist2,ist3
      DATA idmem / -1256765/
!
      IF(id .EQ. idmem) goto 100
      idmem=id
! some checks, not repeated if id the same as previously
      lact=jadres(id)
      IF(lact .EQ. 0) THEN
        WRITE(nout,*) ' gi: nonexisting histo id=',id
        WRITE(   6,*) ' gi: nonexisting histo id=',id
        gi= 0d0
        STOP
      ENDIF
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
! checking if histo is of proper type
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) THEN
        WRITE(nout,*) ' gi: 1-dim histos only !!! id=',id
        WRITE(   6,*) ' gi: 1-dim histos only !!! id=',id
        gi= 0d0
        STOP
      ENDIF
  100 continue
      nch  = nint(b(ist2+1))
      IF(ib .EQ. 0) THEN
! underflow
         gi=   b(ist3 +1)
      ELSEIF(ib .GE. 1.and.ib .LE. nch) THEN
! normal bin
         gi=   b(ist +nbuf+ib)
      ELSEIF(ib .EQ. nch+1) THEN
! overflow
         gi=   b(ist3 +3)
      ELSE
! abnormal exit
         WRITE(nout,*) ' gi: wrong binning id,ib=',id,ib
         WRITE(   6,*) ' gi: wrong binning id,ib=',id,ib
         gi=0d0
         STOP
      ENDIF
      END

      FUNCTION  gie(id,ib)
!     ********************
! getting out error of the bin
! s.j. 18-nov. 90
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      SAVE idmem,nch,lact,ist,ist2,ist3
      DATA idmem / -1256765/
!
      IF(id .EQ. idmem) goto 100
      idmem=id
! some checks, not repeated if id the same as previously
      lact=jadres(id)
      IF(lact .EQ. 0) THEN
        WRITE(nout,*) ' gie: nonexisting histo id=',id
        WRITE(   6,*) ' gie: nonexisting histo id=',id
        gie= 0d0
        STOP
      ENDIF
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
! checking if histo is of proper type
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) THEN
        WRITE(nout,*) ' gie: 1-dim histos only !!! id=',id
        WRITE(   6,*) ' gie: 1-dim histos only !!! id=',id
        gie= 0d0
        STOP
      ENDIF
  100 continue
      nch  = b(ist2+1)
      IF(ib .EQ. 0) THEN
! underflow
         gie=   dsqrt( dabs(b(ist3 +4)))
      ELSEIF(ib .GE. 1.and.ib .LE. nch) THEN
!...normal bin, error content
         gie=   dsqrt( dabs(b(ist+nbuf+nch+ib)) )
      ELSEIF(ib .EQ. nch+1) THEN
! overflow
         gie=   dsqrt( dabs(b(ist3 +6)))
      ELSE
! abnormal exit
         WRITE(nout,*) ' gie: wrong binning id, ib=',id,ib
         WRITE(   6,*) ' gie: wrong binning id, ib=',id,ib
         gie=0d0
         STOP
      ENDIF
      END

      SUBROUTINE gf1(id,x,wtw)
!     ************************
! recommended fast filling 1-dim. histogram
! s.j. 18 nov. 90
!!> overflow/underflow corrected by Maciek and Zbyszek
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
! exit for non-existig histo
      IF(lact .EQ. 0)  RETURN
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
! one-dim. histo only
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) RETURN
      xx= x
      wt= wtw
      index(lact,3)=index(lact,3)+1
! all entries
      b(ist3 +7)  =b(ist3 +7)   +1
! for average x
      b(ist3 +8)  =b(ist3 +8)  +wt*xx
      b(ist3 +9)  =b(ist3 +9)  +wt*xx*xx
! filling bins
      nchx  =b(ist2 +1)
      xl    =b(ist2 +2)
      xu    =b(ist2 +3)
      factx =b(ist2 +4)
!!>      kx = (xx-xl)*factx+1d0
!!>      IF(kx .LT. 1) THEN
      IF(xx .LT. xl) THEN
! underflow
         b(ist3 +1)    = b(ist3 +1)         +wt
         b(ist3 +4)    = b(ist3 +4)         +wt*wt
!!>      ELSEIF(kx .GT. nchx) THEN
      ELSEIF(xx .GT. xu) THEN
! overflow
         b(ist3 +3)    = b(ist3 +3)         +wt
         b(ist3 +6)    = b(ist3 +6)         +wt*wt
      ELSE
! normal bin
         kx = (xx-xl)*factx+1d0
         kx = max(kx,1)    !!!
         kx = min(kx,nchx) !!!
         b(ist3 +2)      = b(ist3 +2)       +wt
         b(ist +nbuf+kx) = b(ist+nbuf+kx)   +wt
! normal bin error
         b(ist3 +5)           = b(ist3 +5)          +wt*wt
         b(ist +nbuf+nchx+kx) = b(ist+nbuf+nchx+kx) +wt*wt
      ENDIF
      END   !gf1

      SUBROUTINE gfill(id,x,y,wtw)
*     ****************************
! this routine not finished, 1-dim only!
*     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      IF(lact .EQ. 0)  RETURN
      ist  = index(lact,2)
! one-dim. histo
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 1) THEN
!...one-dim. histogram
        CALL gf1(id,x,wtw)
        RETURN
      ENDIF
!...two-dim. scattergram, no errors!
      ist2 = ist+7
      ist3 = ist+15
      xx= x
      yy= y
      wt= wtw
      index(lact,3)=index(lact,3)+1
! x-axis
      nchx  =b(ist2 +1)
      xl    =b(ist2 +2)
      factx =b(ist2 +4)
      kx=(xx-xl)*factx+1d0
      lx=2
      IF(kx .LT. 1)     lx=1
      IF(kx .GT. nchx)  lx=3
      l     = ist+34  +lx
      b(l)  = b(l)    +wt
      k     = ist+nbuf2  +kx
      IF(lx .EQ. 2) b(k)  =b(k)  +wt
      k2    = ist+nbuf2  +nchx+kx
      IF(lx .EQ. 2) b(k2) =b(k2) +wt**2
! y-axix
      nchy  =b(ist2 +5)
      yl    =b(ist2 +6)
      facty =b(ist2 +8)
      ky=(yy-yl)*facty+1d0
      ly=2
      IF(ky .LT. 1)    ly=1
      IF(ky .GT. nchy) ly=3
! under/over-flow
      l = ist3  +lx +3*(ly-1)
      b(l) =b(l)+wt
! regular bin
      k = ist+nbuf2 +kx +nchx*(ky-1)
      IF(lx .EQ. 2.and.ly .EQ. 2) b(k)=b(k)+wt
      END

      SUBROUTINE gbook1(id,title,nnchx,xxl,xxu)
*     *****************************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      CHARACTER*80 title
      LOGICAL gexist
!
      CALL g_init
      IF(gexist(id)) goto 900
      ist=length
      lact=jadres(0)
! the case of no free entry in the index
      IF(lact .EQ. 0) goto 901
      index(lact,1)=id
      index(lact,2)=length
      index(lact,3)=0
*----
!cc      WRITE(6,*) 'GBOOK1: ID= ',ID
! -------
      CALL copch(title,titlc(lact))
      nchx =nnchx
      xl   =xxl
      xu   =xxu
! ---------- title and bin content ----------
      lengt2 = length +2*nchx +nbuf+1
      IF(lengt2 .GE. lenmax) goto 902
      do 10 j=length+1,lengt2+1
  10  b(j) = 0d0
      length=lengt2
!... default flags
      ioplog   = 1
      iopsla   = 1
      ioperb   = 1
      iopsc1   = 1
      iopsc2   = 1
      iflag1   =
     $ ioplog+10*iopsla+100*ioperb+1000*iopsc1+10000*iopsc2
      ityphi   = 1
      iflag2   = ityphi
! examples of decoding flags
!      id       = nint(b(ist+2)-9d0-9d12)/10
!      iflag1   = nint(b(ist+3)-9d0-9d12)/10
!      ioplog = mod(iflag1,10)
!      iopsla = mod(iflag1,100)/10
!      ioperb = mod(iflag1,1000)/100
!      iopsc1 = mod(iflag1,10000)/1000
!      iopsc2 = mod(iflag1,100000)/10000
!      iflag2   = nint(b(ist+4)-9d0-9d12)/10
!      ityphi = mod(iflag2,10)
!--------- buffer -----------------
! header
      b(ist +1)  = 9999999999999d0
      b(ist +2)  = 9d12 +     id*10 +9d0
      b(ist +3)  = 9d12 + iflag1*10 +9d0
      b(ist +4)  = 9d12 + iflag2*10 +9d0
! dummy vertical scale
      b(ist +5)  =  -100d0
      b(ist +6)  =   100d0
! pointer used to speed up search of histogram address
      b(ist +7)  =   0d0
! information on binning
      ist2       = ist+7
      b(ist2 +1) = nchx
      b(ist2 +2) = xl
      b(ist2 +3) = xu
      ddx = xu-xl
      IF(ddx .EQ. 0d0) goto 903
      b(ist2 +4) = float(nchx)/ddx
! under/over-flow etc.
      ist3       = ist+11
      do 100  j=1,13
 100  b(ist3 +j)=0d0
!
      RETURN
 900  continue
      WRITE(6   ,*) ' WARNING gbook1: already exists id=  ', id
      WRITE(NOUT,*) ' WARNING gbook1: already exists id=  ', id
      RETURN
 901  continue
      CALL gstop1(' gbook1: to many histos !!!!!,     id=  ',id)
 902  continue
      CALL gstop1(' gbook1: to litle storage!!!!,  lenmax= ',lenmax)
 903  continue
      CALL gstop1('  gbook1:    xl=xu,               id=   ',id)
      END

      SUBROUTINE gstop1(mesage,id)
*     *******************************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE   /gind/
      CHARACTER*40 mesage

      WRITE(nout,'(a)')
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(nout,'(a,a,i10,a)')
     $                          '+ ', mesage, id, ' +'
      WRITE(nout,'(a)')
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(6   ,'(a)')
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(6   ,'(a,a,i10,a)')
     $                          '+ ', mesage, id, ' +'
      WRITE(6   ,'(a)')
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      STOP
      END


      SUBROUTINE goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
!     ********************************************************
! decoding option flags
!     **********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/

      lact=jadres(id)
      IF(lact .EQ. 0) RETURN
      ist=index(lact,2)
! decoding flags
      iflag1   = nint(b(ist+3)-9d0-9d12)/10
      ioplog = mod(iflag1,10)
      iopsla = mod(iflag1,100)/10
      ioperb = mod(iflag1,1000)/100
      iopsc1 = mod(iflag1,10000)/1000
      iopsc2 = mod(iflag1,100000)/10000
      END

      SUBROUTINE gidopt(id,ch)
!     ************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      CHARACTER*4 ch
!
      lact=jadres(id)
      IF(lact .EQ. 0) RETURN
      ist=index(lact,2)
! decoding flags
      CALL goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      IF(ch .EQ.       'LOGY'  ) THEN
! log scale for print
        ioplog = 2
      ELSEIF(ch .EQ.   'ERRO'  ) THEN
! errors in printing/plotting
       ioperb  = 2
      ELSEIF(ch .EQ.   'SLAN'  ) THEN
! slanted line in plotting
       iopsla  = 2
      ELSEIF(ch .EQ.   'YMIN'  ) THEN
       iopsc1  = 2
      ELSEIF(ch .EQ.   'YMAX'  ) THEN
       iopsc2  = 2
      ENDIF
! encoding back
      iflag1   =
     $ ioplog+10*iopsla+100*ioperb+1000*iopsc1+10000*iopsc2
      b(ist+3) = 9d12 + iflag1*10 +9d0
      END


      SUBROUTINE gbfun1(id,title,nchx,xmin,xmax,func)
!     ***********************************************
! ...fills histogram with function func(x)
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DIMENSION yy(200)
      EXTERNAL func
      CHARACTER*80 title
      LOGICAL gexist
!
      CALL g_init
      IF(gexist(id)) GOTO 900
 15   xl=xmin
      xu=xmax
      CALL gbook1(id,title,nchx,xl,xu)
!...slanted line in plotting
      CALL gidopt(id,'SLAN')
      IF(nchx .GT. 200) goto 901
      DO 20 ib=1,nchx
      x= xmin +(xmax-xmin)/nchx*(ib-0.5d0)
      yy(ib) = func(x)
   20 CONTINUE
      CALL gpak(id,yy)
      RETURN
 900  WRITE(nout,*) ' +++gbfun1: already exists id=',id
      WRITE(6   ,*) ' +++gbfun1: already exists id=',id
      CALL gdelet(id)
      GO to 15
 901  WRITE(nout,*) ' +++gbfun1: to many bins'
      END

      SUBROUTINE gbfun2(id,title,nchx,xmin,xmax,func)
!     ***********************************************
! ...fills histogram with function func(x)
!.. three point fit used
!     ***********************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DIMENSION yy(200),yy1(0:200)
      EXTERNAL func
      CHARACTER*80 title
      LOGICAL gexist
!
      CALL g_init
      IF( gexist(id) ) GOTO 900
 15   xl=xmin
      xu=xmax
      CALL gbook1(id,title,nchx,xl,xu)

!...slanted line in plotting
      CALL gidopt(id,'SLAN')
      IF(nchx.gt.200) GOTO 901

      yy1(0) = func(xmin)
      dx=(xmax-xmin)/nchx

      DO ib=1,nchx
         x2= xmin +dx*(ib-0.5d0)
         x3= x2 +dx*0.5d0
         yy(ib)  = func(x2)
         yy1(ib) = func(x3)
!..  simpson
         yy(ib) = ( yy1(ib-1) +4*yy (ib) +yy1(ib))/6d0
      ENDDO

      CALL gpak(id,yy)
      RETURN
 900  WRITE(nout,*) ' +++gbfun2: already exists id=',id
      WRITE(6   ,*) ' +++gbfun2: already exists id=',id
      CALL gdelet(id)
      GO TO 15
 901  WRITE(nout,*) ' +++gbfun2: to many bins'
      END



      SUBROUTINE GBOOK2(ID,TITLE,NCHX,XL,XU,NCHY,YL,YU)
*     *************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( IDMX=400,NBUF=24,NBUF2=24)
      COMMON / Cglib / B(50000)
      COMMON /GIND/ NVRS,NOUT,LENMAX,LENGTH,INDEX(IDMX,3),TITLC(IDMX)
      CHARACTER*80 TITLC
      SAVE /cglib/,/gind/
      CHARACTER*80 TITLE
      LOGICAL GEXIST
!
      CALL g_init
      IF(GEXIST(ID)) GOTO 900
      ist=length
      LACT=JADRES(0)
      IF(LACT .EQ. 0) GOTO 901
      index(LACT,1)=ID
      index(LACT,2)=length
      CALL COPCH(TITLE,TITLC(LACT))
      nnchx=NCHX
      nnchy=NCHY
      LENGT2 = LENGTH  +44+nnchx*nnchy
      IF(LENGT2 .GE. LENMAX) GOTO 902
      DO 10 J=LENGTH+1,LENGT2+1
   10 B(J) = 0D0
      LENGTH=LENGT2
      B(ist+1)=nnchx
      B(ist+2)=XL
      B(ist+3)=XU
      B(ist+4)=float(nnchx)/(b(ist+3)-b(ist+2))
      B(ist+5)=nnchy
      B(ist+6)=YL
      B(ist+7)=YU
      B(ist+8)=float(nnchy)/(b(ist+7)-b(ist+6))
      RETURN
  900 WRITE(NOUT,*) ' GBOOK2: HISTO ALREADY EXISTS!!!! ID=',ID
      RETURN
  901 WRITE(NOUT,*) ' GBOOK2: TO MANY HISTOS !!!!!',LACT
      STOP
  902 WRITE(NOUT,*) ' GBOOK2: TO LITLE STORAGE!!!!',LENMAX
      STOP
      END

      SUBROUTINE gistdo
*     *****************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      do 10 i=1,idmx
      id=index(i,1)
      IF(id .GT. 0) call gprint(id)
   10 continue
      END

      SUBROUTINE goutpu(ilun)
*     ***********************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      CALL g_init
      nout=ilun
      END


      SUBROUTINE gprint(id)
*     *********************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      CHARACTER*1 line(0:105),lchr(22),lb,lx,li,l0
      LOGICAL llg
      SAVE lb,lx,li,l0,lchr
      DATA lb,lx,li,l0 /' ','X','I','0'/
      DATA lchr/' ','1','2','3','4','5','6','7','8','9',
     $      'A','B','C','D','E','F','G','H','I','J','K','*'/

      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
      idec    = nint(b(ist+2)-9d0-9d12)/10
      IF(idec .NE. id) write(6,*) '++++GPRINT: PANIC! ID,IDEC= ',ID,IDEC

      CALL goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      ker    =  ioperb-1
      lmx = 67
      IF(ker .EQ. 1) lmx=54
      nent=index(lact,3)
      IF(nent  .EQ.  0)                          GOTO 901
      WRITE(nout,1000) id,titlc(lact)
 1000 FORMAT('1',/,1X,I6,10X,A)
!
! one-dim. histo
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) goto 200
      nchx =   b(ist2 +1)
      xl   =   b(ist2 +2)
      dx   =  (  b(ist2 +3)-b(ist2 +2)  )/float(nchx)
! fixing vertical scale
      istr=ist+nbuf+1
      bmin = b(istr)
      bmax = b(istr)+1d-5*abs(b(istr))  ! problems for single bin case
      do 15 ibn=istr,istr+nchx-1
      bmax = max(bmax,b(ibn))
      bmin = min(bmin,b(ibn))
  15  continue
      IF(bmin  .EQ.  bmax)                       GOTO 903
      IF(iopsc1 .EQ. 2) bmin=b(ist +5)
      IF(iopsc2 .EQ. 2) bmax=b(ist +6)
!
      llg=ioplog .EQ. 2
      IF(llg.and.bmin .LE. 0d0) bmin=bmax/10000.d0
!
      deltb = bmax-bmin
      IF(deltb  .EQ.  0d0)                       GOTO 902
      fact  = (lmx-1)/deltb
      kzer  = -bmin*fact+1.00001d0
      IF(llg) fact=(lmx-1)/(log(bmax)-log(bmin))
      IF(llg) kzer=-log(bmin)*fact+1.00001d0
!
      undf = b(ist3 +1)
      ovef = b(ist3 +3)
      avex = 0d0
      sum  = b(ist3 +8)
      IF(nent .NE. 0) avex = sum/nent
      WRITE(nout,'(4a15      )')  'nent','sum','bmin','bmax'
      WRITE(nout,'(i15,3e15.5)')   nent,  sum,  bmin,  bmax
      WRITE(nout,'(4a15  )')      'undf','ovef','avex'
      WRITE(nout,'(4e15.5)')       undf,  ovef,  avex
!
      IF(llg) write(nout,1105)
 1105 format(35x,17hlogarithmic scale)
!
      kzer=max0(kzer,0)
      kzer=min0(kzer,lmx)
      xlow=xl
      do 100 k=1,nchx
! first fill with blanks
      do  45 j=1,105
   45 line(j)  =lb
! THEN fill upper and lower boundry
      line(1)  =li
      line(lmx)=li
      ind=istr+k-1
      bind=b(ind)
      bind= max(bind,bmin)
      bind= min(bind,bmax)
      kros=(bind-bmin)*fact+1.0001d0
      IF(llg) kros=log(bind/bmin)*fact+1.0001d0
      k2=max0(kros,kzer)
      k2=min0(lmx,max0(1,k2))
      k1=min0(kros,kzer)
      k1=min0(lmx,max0(1,k1))
      do 50 j=k1,k2
   50 line(j)=lx
      line(kzer)=l0
      z=b(ind)
      IF(ker .NE. 1) THEN
        WRITE(nout,'(a, f7.4,  a, d14.6,  132a1)')
     $             ' ', xlow,' ',     z,' ',(line(i),i=1,lmx)
      ELSE
        er=dsqrt(dabs(b(ind+nchx)))
        WRITE(nout,'(a,f7.4,  a,d14.6,  a,d14.6, 132a1 )')
     $             ' ',xlow,' ',    z,' ',   er,' ',(line(i),i=1,lmx)
      ENDIF
      xlow=xlow+dx
  100 continue
      RETURN
!------------- two dimensional requires complete restoration!!!----------------
  200 continue
      nchx=B(ist+1)
      nchy=B(ist+5)
      WRITE(nout,2000) (lx,i=1,nchy)
 2000 format(1h ,10x,2hxx,100a1)
      do 300 kx=1,nchx
      do 250 ky=1,nchy
      k=ist +NBUF2 +kx+nchx*(ky-1)
      N=B(K)+1.99999D0
      n=max0(n,1)
      n=min0(n,22)
      IF(DABS(b(k)) .LT. 1D-20) n=1
      line(ky)=lchr(n)
  250 continue
      line(nchy+1)=lx
      i1=nchy+1
      WRITE(nout,2100) (line(i),i=1,i1)
 2100 format(1h ,10x,1hx,100a1)
  300 continue
      WRITE(nout,2000) (lx,i=1,nchy)
      RETURN
  900 WRITE(NOUT,*) ' +++GPRINT: NONEXISTING HISTO',ID
      WRITE(6   ,*) ' +++GPRINT: NONEXISTING HISTO',ID
      RETURN
 901  WRITE(NOUT,*) ' +++GPRINT: nent.eq.0',ID
      WRITE(   6,*) ' +++GPRINT: nent.eq.0',ID
      RETURN
 902  WRITE(NOUT,*) ' +++GPRINT: wrong plotting limits',ID,bmin,bmax
      WRITE(   6,*) ' +++GPRINT: wrong plotting limits',ID,bmin,bmax
      RETURN
 903  WRITE(NOUT,*) ' +++GPRINT: bmin.eq.bmax',ID,bmin
      WRITE(   6,*) ' +++GPRINT: bmin.eq.bmax',ID,bmin
      END

      SUBROUTINE gopera(ida,chr,idb,idc,coef1,coef2)
*     **********************************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      CHARACTER*80 title
      CHARACTER*1  chr
!
      lacta=jadres(ida)
      IF(lacta .EQ. 0) RETURN
      ista  = index(lacta,2)
      ista2 = ista+7
      ncha  = b(ista2+1)
!
      lactb =jadres(idb)
      IF(lactb .EQ. 0) RETURN
      istb  = index(lactb,2)
      istb2 = istb+7
      nchb  = b(istb2+1)
      IF(nchb .NE. ncha) goto 900
!
      lactc=jadres(idc)
      IF(lactc .EQ. 0) THEN
! ...if nonexistent, histo idc is here defined
        CALL ginbo1(ida,title,nchx,xl,xu)
        CALL gbook1(idc,title,nchx,xl,xu)
        lactc = jadres(idc)
        istc  = index(lactc,2)
!...option copied from ida
        b(istc+ 3)= b(ista +3)
      ENDIF
!...one nominal entry recorded
      index(lactc,3) = 1
!
      istc  =  index(lactc,2)
      istc2 =  istc+7
      nchc  =  b(istc2+1)
!
      IF(nchc .NE. ncha) goto 900
      IF(ncha .NE. nchb.or.nchb .NE. nchc) goto 900
      do 30 k=1,ncha
      i1 = ista+nbuf+k
      i2 = istb+nbuf+k
      i3 = istc+nbuf+k
      j1 = ista+nbuf+ncha+k
      j2 = istb+nbuf+ncha+k
      j3 = istc+nbuf+ncha+k
      if    (chr .EQ. '+')   THEN
        b(i3) =    coef1*b(i1) +    coef2*b(i2)
        b(j3) = coef1**2*b(j1) + coef2**2*b(j2)
      ELSEIF(chr .EQ. '-')   THEN
        b(i3) = coef1*b(i1) - coef2*b(i2)
        b(j3) = coef1**2*b(j1) + coef2**2*b(j2)
      ELSEIF(chr .EQ. '*')   THEN
        b(j3) = (coef1*coef2)**2
     $          *(b(j1)*b(i2)**2 + b(j2)*b(i1)**2)
        b(i3) = coef1*b(i1) * coef2*b(i2)
      ELSEIF(chr .EQ. '/')   THEN
        IF(b(i2) .EQ. 0d0) THEN
          b(i3) = 0d0
          b(j3) = 0d0
        ELSE
          b(j3) = (coef1/coef2)**2/b(i2)**4
     $          *(b(j1)*b(i2)**2 + b(j2)*b(i1)**2)
          b(i3) = (coef1*b(i1) )/( coef2*b(i2))
        ENDIF
      ELSE
        goto 901
      ENDIF
   30 continue
      RETURN
  900 write(nout,*) '+++++ gopera: non-equal no. bins ',ida,idb,idc
      WRITE(   6,*) '+++++ gopera: non-equal no. bins ',ida,idb,idc
      RETURN
  901 write(nout,*) '+++++ gopera: wrong chr=',chr
      END

      SUBROUTINE ginbo1(id,title,nchx,xl,xu)
!     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      CHARACTER*80 title
!
      lact=jadres(id)
      IF(lact .EQ. 0) THEN
         WRITE(6,*) '+++++ STOP in ginbo1: wrong id=',id
         STOP
      ENDIF
      ist=index(lact,2)
      ist2   = ist+7
      nchx   = b(ist2 +1)
      xl     = b(ist2 +2)
      xu     = b(ist2 +3)
      title  = titlc(lact)
      END

      SUBROUTINE gunpak(id,a,chd1,idum)
*     *********************************
! getting out histogram content (and error)
! chd1= 'ERRO' is nonstandard option (unpack errors)
*     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      CHARACTER*(*) chd1
      dimension a(*)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist   = index(lact,2)
      ist2  = ist+7
      nch   = b(ist2 +1)
      local = ist +nbuf
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 2) THEN
        nchy  = b(ist2+5)
        nch   = nch*nchy
        local = ist+ nbuf2
      ENDIF
      do 10 ib=1,nch
      IF(chd1 .NE. 'ERRO') THEN
! normal bin
        a(ib) = b(local+ib)
      ELSE
! error content
        IF(ityphi .EQ. 2) goto 901
        a(ib) = dsqrt( dabs(b(local+nch+ib) ))
      ENDIF
   10 continue
      RETURN
 900  write(nout,*) '+++gunpak: nonexisting id=',id
      WRITE(6   ,*) '+++gunpak: nonexisting id=',id
      RETURN
 901  write(nout,*) '+++gunpak: no errors, two-dim, id=',id
      END

      SUBROUTINE gpak(id,a)
!     *********************
! Loading in histogram content
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      DIMENSION  a(*)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist  = index(lact,2)
      ist2 = ist+7
      nch=b(ist2 +1)
      local = ist+nbuf
! 2-dimens histo alowed
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 2) THEN
        nchy  = b(ist2+5)
        nch   = nch*nchy
        local = ist+nbuf2
      ENDIF
      do 10 ib=1,nch
   10 b(local +ib) = a(ib)
! one nominal entry recorded
      index(lact,3)  = 1
      RETURN
  900 write(nout,*) '+++gpak: nonexisting id=',id
      WRITE(6   ,*) '+++gpak: nonexisting id=',id
      END

      SUBROUTINE gpake(id,a)
!     **********************
! Loading in error content
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      DIMENSION  a(*)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      IF(lact .EQ. 0) goto 901
      ist  = index(lact,2)
      ist2 = ist+7
      nch=b(ist2+1)
! 2-dimens histo NOT alowed
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 2) goto 900
      do 10 ib=1,nch
   10 b(ist+nbuf+nch+ib) = a(ib)**2
      RETURN
  900 write(nout,*) ' +++++ gpake: only for one-dim histos'
      RETURN
  901 write(nout,*) '+++ gpake: nonexisting id=',id
      WRITE(6   ,*) '+++ gpake: nonexisting id=',id
      END


      SUBROUTINE grang1(id,ylr,yur)
*     *****************************
! provides y-scale for 1-dim plots
*     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      IF(lact .EQ. 0) RETURN
      ist  = index(lact,2)
      ist2 = ist+7
      nch  = b(ist2 +1)
      yl   = b(ist+nbuf+1)
      yu   = b(ist+nbuf+1)
      do 10 ib=1,nch
      yl = min(yl,b(ist+nbuf+ib))
      yu = max(yu,b(ist+nbuf+ib))
   10 continue
      CALL goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      IF(iopsc1 .EQ. 2) yl= b( ist +5)
      IF(iopsc2 .EQ. 2) yu= b( ist +6)
      ylr = yl
      yur = yu
      END

      SUBROUTINE ginbo2(id,nchx,xl,xu,nchy,yl,yu)
*     *******************************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist  = index(lact,2)
      ist2 = ist+7
      nchx = b(ist2 +1)
      xl   = b(ist2 +2)
      xu   = b(ist2 +3)
      nchy = b(ist2 +5)
      yl   = b(ist2 +6)
      yu   = b(ist2 +7)
      RETURN
  900 write(nout,*) ' +++ginbo2: nonexisting histo id= ',id
      WRITE(   6,*) ' +++ginbo2: nonexisting histo id= ',id
      END


      SUBROUTINE gmaxim(id,wmax)
*     **************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      IF(id .NE. 0) THEN
        lact=jadres(id)
        IF(lact .EQ. 0) RETURN
        ist= index(lact,2)
        b(ist+6) =wmax
        CALL gidopt(id,'YMAX')
      ELSE
        do 20 k=1,idmx
        IF(index(k,1) .EQ. 0) goto 20
        ist=index(k,2)
        jd =index(k,1)
        b(ist+6) =wmax
        CALL gidopt(jd,'YMAX')
   20   continue
      ENDIF
      END

      SUBROUTINE gminim(id,wmin)
*     **************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      IF(id .NE. 0) THEN
        lact=jadres(id)
        IF(lact .EQ. 0) RETURN
        ist =index(lact,2)
        b(ist+5) =wmin
        CALL gidopt(id,'YMIN')
      ELSE
        do 20 k=1,idmx
        IF(index(k,1) .EQ. 0) goto 20
        ist=index(k,2)
        jd =index(k,1)
        b(ist+5) =wmin
        CALL gidopt(jd,'YMIN')
   20   continue
      ENDIF
      END

      SUBROUTINE gmimax(id,wmin,wmax)
*     ******************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      CALL gminim(id,wmin)
      CALL gmaxim(id,wmax)
      END


      SUBROUTINE greset(id,chd1)
*     **************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      CHARACTER*(*) chd1
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      IF(lact .LE. 0) RETURN
      ist  =index(lact,2)
      ist2 = ist+7
!
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 1) THEN
! one-dim.
        ist3  = ist+11
        nchx  = b(ist2 +1)
        nch   = 2*nchx
        local = ist + nbuf
      ELSEIF(ityphi .EQ. 2) THEN
! two-dim.
        ist3  = ist+15
        nchx  = b(ist2 +1)
        nchy  = b(ist2 +5)
        nch   = nchx*nchy
        local = ist +nbuf2
      ELSE
         WRITE(nout,*) '+++greset: wrong type id=',id
         WRITE(6   ,*) '+++greset: wrong type id=',id
        RETURN
      ENDIF
! reset miscaelaneous entries and bins
      do 10 j=ist3+1,local +nch
  10  b(j)    = 0d0
! and no. of entries in index
      index(lact,3) = 0
      END

      SUBROUTINE GDELET(ID1)
*     *********************
! Now it should work (stj Nov. 91) but watch out!
! should works for 2-dim histos, please check this!
*     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      LOGICAL gexist
!
      ID=ID1
      IF(id .EQ. 0) GOTO 300
      IF(.not.gexist(id)) GOTO 900
      lact = jadres(id)
      ist  = index(lact,2)
      ist2 = ist+7
*----
![[[      WRITE(6,*) 'GDELET-ing ID= ',ID
      idec    = nint(b(ist+2)-9d0-9d12)/10
      IF(idec .NE. id) WRITE(6,*) '++++GDELET: ALARM! ID,IDEC= ',ID,IDEC
*----
      nch  = b(ist2 +1)
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 1) THEN
! one-dim.
        nchx  = b(ist2 +1)
        nch   = 2*nchx
! lenght of local histo to be removed
        local = nch+nbuf+1
      ELSEIF(ityphi .EQ. 2) THEN
! two-dim.
        nchx  = b(ist2 +1)
        nchy  = b(ist2 +5)
        nch   = nchx*nchy
! lenght of local histo to be removed
        local = nch+nbuf2+1
      ELSE
         WRITE(nout,*) '+++gdelet: wrong type id=',id
         WRITE(6   ,*) '+++gdelet: wrong type id=',id
        RETURN
      ENDIF
! starting position of next histo in storage b
      next = ist+1 +local
! move down all histos above this one
      DO 15 k =next,length
      b(k-local)=b(k)
   15 CONTINUE
! define new end of storage
      length=length-local
! clean free space at the end of storage b
      DO 20 k=length+1, length+local
   20 b(k)=0d0
! shift adresses of all displaced histos
      DO 25 l=lact+1,idmx
      IF(index(l,1) .NE. 0) index(l,2)=index(l,2)-local
   25 CONTINUE
! move entries in index down by one and remove id=lact entry
      DO 30 l=lact+1,idmx
      index(l-1,1)=index(l,1)
      index(l-1,2)=index(l,2)
      index(l-1,3)=index(l,3)
      titlc(l-1)=titlc(l)
   30 CONTINUE
! last entry should be always empty
      index(idmx,1)=0
      index(idmx,2)=0
      index(idmx,3)=0
      do 50 k=1,80
   50 titlc(idmx)(k:k)=' '
      RETURN
! -----------------------------------
! Deleting all histos at once!!!
  300 length=0
      DO 400 i=1,idmx
      DO 340 k=1,3
  340 index(i,k)=0
      DO 350 k=1,80
  350 titlc(i)(k:k)=' '
 400  CONTINUE
      RETURN
! -----------------------------------
 900  CONTINUE
      WRITE(nout,*) ' +++GDELET: nonexisting histo id= ',id
      WRITE(   6,*) ' +++GDELET: nonexisting histo id= ',id
      END


      SUBROUTINE glimit(lenmx)
*     ************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      CALL g_init
      IF(lenmx .GE. lenmax) THEN
         lenmax=lenmx
      ELSE
         CALL gstop1('glimit: cant decrease storage lenmx  =',lenmx)
      ENDIF
      END

      SUBROUTINE copch(ch1,ch2)
*     *************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
* copies CHARACTER*80 ch1 into ch2 up to a first $ sign
      CHARACTER*80 ch1,ch2
      LOGICAL met
      met = .false.
      do 10 i=1,80
      IF( ch1(i:i) .EQ. '$' .or. met )   THEN
        ch2(i:i)=' '
        met=.true.
      ELSE
        ch2(i:i)=ch1(i:i)
      ENDIF
  10  continue
      END

      FUNCTION jadre2(id)
*     *********************
*------------------------------------------------
* Good old version -- but it is very very slow!!!
* In the case of 100 histograms or more.
*------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      jadre2=0
      DO 1 i=1,idmx
      IF(index(i,1) .EQ. id) goto 2
    1 CONTINUE
* Nothing found.
      RETURN
* Found: id=0 is also legitimate find!!!
    2 jadre2=i
      END

      FUNCTION jadres(id1)
*     *********************
*--------------------------------------------------------------------
* Educated guess based on past history is used to find quickly
* location of the histogram in the matrix index.
* This is based on observation that subsequent histogram calls
* are linked into loops (so one can predict easily which histo will
* be called next time).
*--------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DATA iguess,jdlast,idlast /-2141593,-3141593,-3141593/
      SAVE iguess,jdlast,idlast

      id=id1
! --- The case of ID=0 treated separately, it is used to find out
! --- last entry in the index (it is marked with zero)
      IF(id .EQ. 0) THEN
         DO i=1,idmx
            IF(index(i,1) .EQ. 0) goto 4
         ENDDO
         WRITE(6,*) '+++++jadres: STOP index to short'
         STOP
 4       CONTINUE
         jadres = i
         RETURN
      ENDIF

! --- Omit sophistications if lack of initialization
      IF(jdlast .EQ. -3141593) GOTO 10
      IF(iguess .EQ. -2141593) GOTO 10
      IF(iguess .EQ. 0) GOTO 10
      IF(jdlast .EQ. 0) GOTO 10

! --- Try first previous histo (for repeated calls)
      IF(jdlast .LT. 1 .OR. jdlast .GT. idmx) THEN
         WRITE(6,*) '+++++ jadres: jdlast=',jdlast
      ENDIF
      IF(index(jdlast,1) .EQ. id) THEN
         jadres = jdlast
!##   write(6,*)
!##   $   'found, guess based on previous call to jadres ',jdlast
         GOTO 20
      ENDIF

! --- Try current guess based on previous call
      IF(iguess .LT. 1 .OR. iguess .GT. idmx)  THEN
         WRITE(6,*)'+++++ jadres: iguess=',iguess
      ENDIF
      IF(index(iguess,1) .EQ. id) THEN
         jadres = iguess
!##   write(6,*)
!##   $   'found, guess on previous calls recorded in b(ist+7)',jdlast
         GOTO 20
      ENDIF

! ================================================
!    Do it HARD WAY, Search all matrix index
! ================================================
 10   CONTINUE
!##   write(6,*) 'trying HARD WAY'
      DO i=1,idmx
         jadres=i
         IF(index(i,1) .EQ. id) GOTO 20
      ENDDO
! -------------------------------------
!     Nothing found: jadres=0
! -------------------------------------
      jadres=0
      RETURN
! =====================================
!     Found: Set new guess for next call
! =====================================
 20   CONTINUE
! --- and store result as a new guess in previous histo
! --- but only if it existed!!!!
      DO i=1,idmx
         IF(index(i,1) .EQ. 0) GOTO 40
         IF(index(i,1) .EQ. idlast) THEN
            ist=index(i,2)
            IF(ist .GT. 0 .AND. ist .LT. 50000) b(ist +7) = jadres
!##   write(6,*) 'STORED     id=',id
            GOTO 40
         ENDIF
      ENDDO
 40   CONTINUE
!##   write(6,*)  'found, hard way searching all of index)', jdlast
      iguess = b( index(jadres,2) +7)
      jdlast = jadres
      idlast = id
      END


!--------------------------------------------------------------
! ----------- storing histograms in the disk file -------------
!--------------------------------------------------------------
      SUBROUTINE grfile(nhruni,dname,chd2)
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      CHARACTER*(*) chd2, dname
      COMMON / hruni / nhist
      SAVE /hruni/
      nhist=nhruni
      END

      SUBROUTINE grout(idum1,idum2,chdum)
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      CHARACTER*8 chdum
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      COMMON / hruni / nhist
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/, /hruni/
!
      CALL g_init
      nouth=nhist
      WRITE(nouth,'(6i10)')   nvrs,nout,lenmax,length
      WRITE(nouth,'(6i10)')   ((index(i,k),k=1,3),i=1,idmx)
      WRITE(nouth,'(a80)')    titlc
      WRITE(nouth,'(3d24.16)') (b(i),i=1,length)
      END


      SUBROUTINE GRIN(IDUM1,IDUM2,IDUM3)
!     **********************************
! New version which has a possibility to
!            MERGE histograms
! If given ID already exists then it is modified by adding 1000000 !!!!
! Mergigng is done simply by appending new histograms at the
! very end of the index and bin matrices.
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      COMMON / hruni / nhist
      SAVE /cglib/,/gind/, /hruni/
! Copy of the new index from the disk
      DIMENSION lndex(idmx,3),titld(idmx)
      CHARACTER*80 titld
      LOGICAL gexist

      CALL g_init
      nouth=nhist
! Read basic params
      READ(nouth,'(6i10)')   nvrs3,nout3,lenma3,lengt3
      IF(length+lengt3 .GE. lenmax) GOTO 900
! Check version
      IF(nvrs .NE. nvrs3) WRITE(nout,*)
     $ '  +++++ warning (grin): histos produced by older version',nvrs3
      IF(nvrs .NE. nvrs3) WRITE(6,*)
     $ '  +++++ warning (grin): histos produced by older version',nvrs3
! Read new index  from the disk
      READ(nouth,'(6i10)')  ((lndex(i,k),k=1,3),i=1,idmx)
      READ(nouth,'(a80)')    titld

      lenold=length
! Append content of new histos AT ONCE  at the end of storage b
      length=length+lengt3
      READ(nouth,'(3d24.16)') (b(i),i=lenold+1,length)

! Append index and titlc with new histos one by one
      lact = jadres(0)
      DO 100 l=1,idmx
      IF(lact .EQ. 0) GOTO 901
      idn= lndex(l,1)
      IF(idn .EQ. 0) GOTO 100
! Identical id's are changed by adding big number = 1000000
 10   CONTINUE
      IF( gexist(idn) ) THEN
         idn = idn +1000000*(idn/iabs(idn))
         GOTO 10
      ENDIF
      index(lact,1)=idn
      index(lact,2)=lndex(l,2)+lenold
      index(lact,3)=lndex(l,3)
      titlc(lact)  =titld(l)
!
! Still one small correction in the newly appended histo
      istn  = index(lact,2)
      b(istn +2)  = 9d12 +     idn*10 +9d0
!
      lact=lact+1
  100 CONTINUE

!
      RETURN

 900  CONTINUE
      CALL gstop1('++++ grin: to litle space, lenmax=  ',lenmax)
 901  CONTINUE
      CALL gstop1('++++ grin: to many histos, idmx=    ',idmx)
      END




      SUBROUTINE GRIN2(IDUM1,IDUM2,IDUM3)
!     **********************************
! New version which has a possibility to
!            ADD histograms
! If ID is not existing already then no action is taken
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      COMMON / hruni / nhist
      SAVE /cglib/,/gind/, /hruni/
! Copy of the histos from the disk
      DIMENSION bz(50000)
      DIMENSION indez(idmx,3),titlz(idmx)
      CHARACTER*80 titlz
      LOGICAL gexist

      CALL g_init
      nouth=nhist
! Read basic params
      READ(nouth,'(6i10)')   nvrsz,noutz,lenmaz,lengtz
! Check version
      IF(nvrs .NE. nvrsz) WRITE(nout,*)
     $ '  +++++ warning (grin2): histos produced by older version',nvrsz
      IF(nvrs .NE. nvrsz) WRITE(6,*)
     $ '  +++++ warning (grin2): histos produced by older version',nvrsz
! Read new index, title and bins from the disk
      READ(nouth,'(6i10)')    ((indez(i,k),k=1,3),i=1,idmx)
      READ(nouth,'(a80)')     titlz
      READ(nouth,'(3d24.16)') (bz(i),i=1,lengtz)

! Add new histos from disk to existing ones one by one
      DO 100 lz=1,idmx
      id= indez(lz,1)
      IF(id .EQ. 0) GOTO 200
      IF(.not.gexist(id)) THEN
        WRITE(6,*) ' Grin2: unmached histo ID=', id, '  Skipped'
        goto 100
      ENDIF
! parameters of existing histo
      lact = jadres(id)
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
      nchx = b(ist2 +1)
! parameters of the histo from the disk
      istz   = indez(lz,2)
      ist2z  = istz+7
      ist3z  = istz+11
      nchxz  = bz(ist2z +1)
      IF(nchx .NE. nchxz) THEN
        WRITE(6,*) ' Grin2: non-equal binning ID=', id, '  Skipped'
        goto 100
      ENDIF
! Add/Merge all additive entries of the two histos
! No of entries in index
      index(lact,3) = index(lact,3)+indez(lact,3)
! Overflows, underflows etc.
      DO i=1,12
        b(ist3+i)=b(ist3+i) +bz(ist3z+i)
      ENDDO
! Except of this one non-additive entry
      b(ist3+13)=max(b(ist3+13),b(ist3z+13))
! Regular bin content added now!
      DO i= 1, 2*nchx
        b(ist+nbuf+i)=b(ist+nbuf+i) +bz(istz+nbuf+i)
      ENDDO
  100 CONTINUE
  200 CONTINUE

      END

      SUBROUTINE grend(chdum)
!     ***********************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / hruni / nhist
      SAVE   /hruni/
      CHARACTER*(*) chdum
      CLOSE(nhist)
!======================================================================
!======================end of gbook====================================
!======================================================================
      END

!======================================================================
!======================Mini-GPLOT======================================
!======================================================================
!... Plotting using LaTeX
      SUBROUTINE gplint(ICOD)
!     ***********************
      SAVE
!---------------------------------------------------
! This COMMON connects gplint, gplcap and gplend
      COMMON / clint / lint
!---------------------------------------------------
!
      lint = icod
!
      END

      SUBROUTINE gplcap(IFILE)
!     ***********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
!---------------------------------------------------
! This COMMON connects gplint, gplcap and gplend
      COMMON / clint / lint
!---------------------------------------------------
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
!----------------------------------
! Titles and captions for plot
      COMMON / lpltit / titch(50),keytit
      CHARACTER*64 titch
!----------------
! Note that backslash definition is varying from one
! instalation/compiler to another, you have to figure out by yourself
! how to fill backslash code into BS
      COMMON / BSLASH / BS
      CHARACTER*1 BS,BBS
!     DATA BBS / 1H\ /
      DATA BBS / '\\' /
      BS = BBS
!c      BS = '\\'
!---------
      KEYTIT= 0
      DO i=1,50
         DO k=1,64
            titch(i)(k:k)=' '
         ENDDO
      ENDDO
!---------
      ILINE = 1
      NOUH1=IABS(IFILE)
      NOUH2=NOUH1+1
      IF( ABS(lint) .EQ. 0) THEN
! Normal mode
         WRITE(NOUH1,'(A,A)') BS,'documentstyle[12pt,html]{article}'
         WRITE(NOUH1,'(A,A)') BS,'textwidth  = 16cm'
         WRITE(NOUH1,'(A,A)') BS,'textheight = 18cm'
         WRITE(NOUH1,'(A,A)') BS,'begin{document}'
         WRITE(NOUH1,'(A)') '  '
      ELSEIF( ABS(lint) .EQ. 1) THEN
! For TeX file is used in \input
         WRITE(NOUH1,'(A)') '  '
      ELSEIF( ABS(lint) .EQ. 2) THEN
! For one-page plot being input for postrscript
         WRITE(NOUH1,'(A,A)') BS,'documentclass[12pt]{article}'
         WRITE(NOUH1,'(A,A)') BS,'usepackage{amstex}'
         WRITE(NOUH1,'(A,A)') BS,'usepackage{amssymb}'
         WRITE(NOUH1,'(A,A)') BS,'usepackage{html}'
         WRITE(NOUH1,'(A,A)') BS,'usepackage{epsfig}'
         WRITE(NOUH1,'(A,A)') BS,'usepackage{epic}'
         WRITE(NOUH1,'(A,A)') BS,'usepackage{eepic}'
!!!         WRITE(NOUH1,'(A,A)') BS,'hoffset    = -1in'
!!!         WRITE(NOUH1,'(A,A)') BS,'voffset    = -1in'
!!!         WRITE(NOUH1,'(A,A)') BS,'textwidth  = 16cm'
!!!         WRITE(NOUH1,'(A,A)') BS,'textheight = 16cm'
!!!         WRITE(NOUH1,'(A,A)') BS,'oddsidemargin = 0cm'
!!!         WRITE(NOUH1,'(A,A)') BS,'topmargin     = 0cm'
!!!         WRITE(NOUH1,'(A,A)') BS,'headheight    = 0cm'
!!!         WRITE(NOUH1,'(A,A)') BS,'headsep       = 0cm'
         WRITE(NOUH1,'(A,A)') BS,'begin{document}'
         WRITE(NOUH1,'(A,A)') BS,'pagestyle{empty}'
         WRITE(NOUH1,'(A)') '  '
      ELSE
         WRITE(6,*) ' +++++++ STOP in gplint, wrong lint=',lint
      ENDIF

      END

      SUBROUTINE gplend
!     *****************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
!---------------------------------------------------
! This COMMON connects gplint, gplcap and gplend
      COMMON / clint / lint
!---------------------------------------------------
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS

! Note that TeX file is used in \input then you may not want
! to have header and \end{document}
      IF( ABS(lint) .NE. 1) THEN
         WRITE(NOUH1,'(2A)') BS,'end{document}'
      ENDIF

      CLOSE(NOUH1)
      END

      SUBROUTINE GPLOT(ID,CH1,CH2,KDUM)
!     *********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DIMENSION YY(200),YER(200)
      CHARACTER CH1,CH2,CHR
      CHARACTER*80 TITLE
      LOGICAL GEXIST
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      DATA CHR /' '/
! RETURN if histo non-existing
      IF(.NOT.GEXIST(ID)) GOTO 900
! ...unpack histogram
      CALL GUNPAK(ID,YY ,'    ',IDUM)
      CALL GUNPAK(ID,YER,'ERRO',IDUM)
      CALL GINBO1(ID,TITLE,NCHX,DXL,DXU)
      XL = DXL
      XU = DXU
      CALL GRANG1(ID,YL,YU)
      KAX=1200
      KAY=1200
      IF(CH1 .EQ. 'S') THEN
! ...superimpose plot
        BACKSPACE(NOUH1)
        BACKSPACE(NOUH1)
      ELSE
! ...new frame only
        CHR=CH1
        CALL LFRAM1(ID,KAX,KAY)
      ENDIF
      WRITE(NOUH1,'(A)')    '%========== next plot (line) =========='
      WRITE(NOUH1,'(A,I10)') '%==== HISTOGRAM ID=',ID
      WRITE(NOUH1,'(A,A70 )') '% ',TITLE
!...cont. line for functions
      CALL goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      ker = ioperb-1
      IF (iopsla .EQ. 2)  CHR='C'
!...suppress GPLOT assignments
      IF (CH2 .EQ. 'B')   CHR=' '
      IF (CH2 .EQ. '*')   CHR='*'
      IF (CH2 .EQ. 'C')   CHR='C'
!...various types of lines
      IF     (CHR .EQ. ' ') THEN
!...contour line used for histogram
          CALL PLHIST(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
      ELSE IF(CHR .EQ. '*') THEN
!...marks in the midle of the bin
          CALL PLHIS2(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
      ELSE IF(CHR .EQ. 'C') THEN
!...slanted (dotted) line in plotting non-MC functions
          CALL PLCIRC(KAX,KAY,NCHX,YL,YU,YY)
      ENDIF
!------------------------------!
! Ending
!------------------------------!
      WRITE(NOUH1,'(2A)') BS,'end{picture} % close entire picture '
      WRITE(NOUH1,'(2A)') BS,'end{figure}'

      RETURN
  900 WRITE(*,*) ' ++++ GPLOT: NONEXISTIG HISTO ' ,ID
      END

      SUBROUTINE LFRAM1(ID,KAX,KAY)
!     *****************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
!----------------------------------
! Titles ans captions for plot
      COMMON / lpltit / titch(50),keytit
      CHARACTER*64 TITCH
!----------------
      DIMENSION TIPSY(20),TIPSX(20)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      DOUBLE PRECISION DXL,DXU
      COMMON / BSLASH / BS
      CHARACTER*1 BS
CC JVW (variable TITLE must be defined as character!)
      CHARACTER*80 TITLE
CC JVW
      DATA ICONT/0/

      ICONT=ICONT+1
      CALL GINBO1(ID,TITLE,NCHX,DXL,DXU)
      XL = DXL
      XU = DXU
      CALL GRANG1(ID,YL,YU)
      IF(ICONT .GT. 1) WRITE(NOUH1,'(2A)') BS,'newpage'
!------------------------------!
!           Header
!------------------------------!
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     $%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE(NOUH1,'(A)') '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     $%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE(NOUH1,'(2A)') BS,'begin{figure}[!ht]'
      WRITE(NOUH1,'(2A)') BS,'centering'
!------------------------------!
! General Caption
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'caption{',BS,'small'
      IF(KEYTIT.EQ.0) THEN
        WRITE(NOUH1,'(A)')     TITLE
      ELSE
        WRITE(NOUH1,'(A)')     TITCH(1)
      ENDIF
      WRITE(NOUH1,'(A)') '}'
!------------------------------!
! Frames and labels
!------------------------------!
      WRITE(NOUH1,'(A)') '% =========== big frame, title etc. ======='
      WRITE(NOUH1,'(4A)') BS,'setlength{',BS,'unitlength}{0.1mm}'
      WRITE(NOUH1,'(2A)') BS,'begin{picture}(1600,1500)'
      WRITE(NOUH1,'(4A)') BS,'put(0,0){',BS,'framebox(1600,1500){ }}'
      WRITE(NOUH1,'(A)') '% =========== small frame, labeled axis ==='
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $    BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $    BS,'put(0,0){',BS,'framebox( ',KAX,',',KAY,'){ }}'
      WRITE(NOUH1,'(A)') '% =========== x and y axis ================'
      CALL SAXIX(KAX,XL,XU,NTIPX,TIPSX)
      CALL SAXIY(KAY,YL,YU,NTIPY,TIPSY)
      WRITE(NOUH1,'(3A)') BS,'end{picture}}'
     $                ,'% end of plotting labeled axis'
      END

      SUBROUTINE SAXIX(KAY,YL,YU,NLT,TIPSY)
!     ***************************************
! plotting x-axis with long and short tips
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TIPSY(20)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/

      DY= ABS(YU-YL)
      LY = NINT( LOG10(DY) -0.4999999d0 )
      JY = NINT(DY/10d0**LY)
      DDYL = DY*10d0**(-LY)
      IF( JY .EQ. 1)             DDYL = 10d0**LY*0.25d0
      IF( JY .GE. 2.AND.JY .LE. 3) DDYL = 10d0**LY*0.5d0
      IF( JY .GE. 4.AND.JY .LE. 6) DDYL = 10d0**LY*1.0d0
      IF( JY .GE. 7)             DDYL = 10d0**LY*2.0d0
      WRITE(NOUH1,'(A)') '% .......SAXIX........ '
      WRITE(NOUH1,'(A,I4)') '%  JY= ',JY
!-------
      NLT = INT(DY/DDYL)
      NLT = MAX0(MIN0(NLT,20),1)+1
      YY0L = NINT(YL/DDYL+0.5d0)*DDYL
      DDYS = DDYL/10d0
      YY0S = NINT(YL/DDYS+0.4999999d0)*DDYS
      P0L = KAY*(YY0L-YL)/(YU-YL)
      PDL = KAY*DDYL/(YU-YL)
      P0S = KAY*(YY0S-YL)/(YU-YL)
      PDS = KAY*DDYS/(YU-YL)
      NLT = INT(ABS(YU-YY0L)/DDYL+0.0000001d0)+1
      NTS = INT(ABS(YU-YY0S)/DDYS+0.0000001d0)+1
      DO 41 N=1,NLT
      TIPSY(N) =YY0L+ DDYL*(N-1)
  41  CONTINUE
      WRITE(NOUH1,1000)
     $ BS,'multiput('  ,P0L,  ',0)('  ,PDL,  ',0){'  ,NLT,  '}{',
     $ BS,'line(0,1){25}}',
     $ BS,'multiput('  ,P0S,  ',0)('  ,PDS,  ',0){'  ,NTS,  '}{',
     $ BS,'line(0,1){10}}'
      WRITE(NOUH1,1001)
     $ BS,'multiput('  ,P0L,  ','  ,KAY,  ')('  ,PDL,  ',0){'  ,NLT,
     $ '}{'  ,BS,  'line(0,-1){25}}',
     $ BS,'multiput('  ,P0S,  ','  ,KAY,  ')('  ,PDS,  ',0){'  ,NTS,
     $ '}{'  ,BS,  'line(0,-1){10}}'
 1000 FORMAT(2A,F8.2,A,F8.2,A,I4,3A)
 1001 FORMAT(2A,F8.2,A,I4,A,F8.2,A,I4,3A)
! ...labeling of axis
      SCMX = DMAX1(DABS(YL),DABS(YU))
      LEX  = NINT( LOG10(SCMX) -0.50001)
      DO 45 N=1,NLT
      K = NINT(KAY*(TIPSY(N)-YL)/(YU-YL))
      IF(LEX .LT. 2.AND.LEX .GT. -1) THEN
! ...without exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,A)')
     $ BS,'put(',K,',-25){',BS,'makebox(0,0)[t]{',BS,'large $ ',
     $ TIPSY(N), ' $}}'
      ELSE
! ...with exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,2A,I4,A)')
     $ BS,'put('  ,K,  ',-25){',BS,'makebox(0,0)[t]{',BS,'large $ ',
     $ TIPSY(N)/(10d0**LEX),BS,'cdot 10^{',LEX,'} $}}'
      ENDIF
  45  CONTINUE
      END

      SUBROUTINE SAXIY(KAY,YL,YU,NLT,TIPSY)
!     ***************************************
! plotting y-axis with long and short tips
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TIPSY(20)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/

      DY= ABS(YU-YL)
      LY = NINT( LOG10(DY) -0.49999999d0 )
      JY = NINT(DY/10d0**LY)
      DDYL = DY*10d0**(-LY)
      IF( JY .EQ. 1)             DDYL = 10d0**LY*0.25d0
      IF( JY .GE. 2.AND.JY .LE. 3) DDYL = 10d0**LY*0.5d0
      IF( JY .GE. 4.AND.JY .LE. 6) DDYL = 10d0**LY*1.0d0
      IF( JY .GE. 7)             DDYL = 10d0**LY*2.0d0
      WRITE(NOUH1,'(A)') '% .......SAXIY........ '
      WRITE(NOUH1,'(A,I4)') '%  JY= ',JY
!-------
      NLT = INT(DY/DDYL)
      NLT = MAX0(MIN0(NLT,20),1)+1
      YY0L = NINT(YL/DDYL+0.4999999d0)*DDYL
      DDYS = DDYL/10d0
      YY0S = NINT(YL/DDYS+0.5d0)*DDYS
      P0L = KAY*(YY0L-YL)/(YU-YL)
      PDL = KAY*DDYL/(YU-YL)
      P0S = KAY*(YY0S-YL)/(YU-YL)
      PDS = KAY*DDYS/(YU-YL)
      NLT= INT(ABS(YU-YY0L)/DDYL+0.0000001d0) +1
      NTS= INT(ABS(YU-YY0S)/DDYS+0.0000001d0) +1
      DO 41 N=1,NLT
      TIPSY(N) =YY0L+ DDYL*(N-1)
  41  CONTINUE
! plotting tics on vertical axis
      WRITE(NOUH1,1000)
     $ BS,'multiput(0,'  ,P0L,  ')(0,'  ,PDL  ,'){'  ,NLT,  '}{',
     $ BS,'line(1,0){25}}',
     $ BS,'multiput(0,'  ,P0S,  ')(0,'  ,PDS,  '){'  ,NTS,  '}{',
     $ BS,'line(1,0){10}}'
      WRITE(NOUH1,1001)
     $ BS,'multiput('  ,KAY,  ','  ,P0L,  ')(0,'  ,PDL,  '){'  ,NLT,
     $ '}{',BS,'line(-1,0){25}}',
     $ BS,'multiput('  ,KAY,  ','  ,P0S,  ')(0,'  ,PDS,  '){'  ,NTS,
     $ '}{',BS,'line(-1,0){10}}'
 1000 FORMAT(2A,F8.2,A,F8.2,A,I4,3A)
 1001 FORMAT(2A,I4,A,F8.2,A,F8.2,A,I4,3A)
! ...Zero line if necessary
      Z0L = KAY*(-YL)/(YU-YL)
      IF(Z0L .GT. 0D0.AND.Z0L .LT. FLOAT(KAY))
     $      WRITE(NOUH1,'(2A,F8.2,3A,I4,A)')
     $       BS,'put(0,'  ,Z0L,  '){',BS,'line(1,0){'  ,KAY,  '}}'
! ...labeling of axis
      SCMX = DMAX1(DABS(YL),DABS(YU))
      LEX  = NINT( LOG10(SCMX) -0.50001d0)
      DO 45 N=1,NLT
      K = NINT(KAY*(TIPSY(N)-YL)/(YU-YL))
      IF(LEX .LT. 2.AND.LEX .GT. -1) THEN
! ...without exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,A)')
     $  BS,'put(-25,'  ,K,  '){',BS,'makebox(0,0)[r]{',
     $  BS,'large $ '  ,TIPSY(N),  ' $}}'
      ELSE
! ...with exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,2A,I4,A)')
     $ BS,'put(-25,'  ,K,  '){',BS,'makebox(0,0)[r]{',
     $ BS,'large $ '
     $ ,TIPSY(N)/(10d0**LEX),  BS,'cdot 10^{'  ,LEX,  '} $}}'
      ENDIF
  45  CONTINUE
      END
      SUBROUTINE PLHIST(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
!     ************************************************
! plotting contour line for histogram
!     ***********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),YER(*)
      CHARACTER*80 FMT1
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $  BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
!...various types of line
      IF(ILINE .EQ. 1) THEN
         WRITE(NOUH1,'(2A)') BS,'thicklines '
      ELSE
         WRITE(NOUH1,'(2A)') BS,'thinlines '
      ENDIF
!...short macros for vertical/horizontal straight lines
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'x}[3]{',BS,'put(#1,#2){',
     $ BS,'line(1,0){#3}}}'
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'y}[3]{',BS,'put(#1,#2){',
     $ BS,'line(0,1){#3}}}'
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'z}[3]{',BS,'put(#1,#2){',
     $ BS,'line(0,-1){#3}}}'
!   error bars
      WRITE(NOUH1,'(8A)')
     $   BS,'newcommand{',BS,'e}[3]{',
     $   BS,'put(#1,#2){',BS,'line(0,1){#3}}}'
      IX0=0
      IY0=0
      DO 100 IB=1,NCHX
      IX1 = NINT(KAX*(IB-0.00001)/NCHX)
      IY1 = NINT(KAY*(YY(IB)-YL)/(YU-YL))
      IDY = IY1-IY0
      IDX = IX1-IX0
      FMT1 = '(2(2A,I4,A,I4,A,I4,A))'
      IF( IDY .GE. 0) THEN
         IF(IY1 .GE. 0.AND.IY1 .LE. KAY)
     $   WRITE(NOUH1,FMT1) BS,'y{',IX0,'}{',IY0,'}{',IDY,'}',
     $                     BS,'x{',IX0,'}{',IY1,'}{',IDX,'}'
      ELSE
         IF(IY1 .GE. 0.AND.IY1 .LE. KAY)
     $   WRITE(NOUH1,FMT1) BS,'z{',IX0,'}{',IY0,'}{',-IDY,'}',
     $                     BS,'x{',IX0,'}{',IY1,'}{',IDX,'}'
      ENDIF
      IX0=IX1
      IY0=IY1
      IF(KER .EQ. 1) THEN
        IX2  = NINT(KAX*(IB-0.5000d0)/NCHX)
        IERR = NINT(KAY*((YY(IB)-YER(IB))-YL)/(YU-YL))
        IE = NINT(KAY*YER(IB)/(YU-YL))
        IF(IY1 .GE. 0.AND.IY1 .LE. KAY.and.abs(ierr) .LE. 9999
     $     .and.2*ie .LE. 9999) WRITE(NOUH1,8000) BS,IX2,IERR,IE*2
      ENDIF
 100  CONTINUE
8000  FORMAT(4(A1,2He{,I4,2H}{,I5,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(3A)') BS,'end{picture}}',
     $       ' % end of plotting histogram'
! change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 2) ILINE=1
      END
      SUBROUTINE PLHIS2(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
!     ************************************************
! marks in the midle of the bin
!     **********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),YER(*)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/

      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $ BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
!...various types of mark
      IRAD1= 6
      IRAD2=10
      IF(ILINE .EQ. 1) THEN
!   small filled circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 2) THEN
!   small open circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 3) THEN
!   big filled circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD2,'}}}'
      ELSEIF(ILINE .EQ. 4) THEN
!   big open circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD2,'}}}'
! Other symbols
      ELSEIF(ILINE .EQ. 5) THEN
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'diamond$}}}'
      ELSE
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'star$}}}'
      ENDIF
!   error bars
      WRITE(NOUH1,'(8A)')
     $   BS,'newcommand{',BS,'E}[3]{',
     $   BS,'put(#1,#2){',BS,'line(0,1){#3}}}'
      DO 100 IB=1,NCHX
      IX1 = NINT(KAX*(IB-0.5000d0)/NCHX)
      IY1 = NINT(KAY*(YY(IB)-YL)/(YU-YL))
      IF(IY1 .GE. 0.AND.IY1 .LE. KAY) WRITE(NOUH1,7000) BS,IX1,IY1
      IF(KER .EQ. 1) THEN
        IERR = NINT(KAY*((YY(IB)-YER(IB))-YL)/(YU-YL))
        IE   = NINT(KAY*YER(IB)/(YU-YL))
        IF(IY1 .GE. 0.AND.IY1 .LE. KAY.and.abs(ierr) .LE. 9999
     $       .and.2*ie .LE. 9999) WRITE(NOUH1,8000) BS,IX1,IERR,IE*2
      ENDIF
 100  CONTINUE
7000  FORMAT(4(A1,2HR{,I4,2H}{,I4,1H}:1X ))
8000  FORMAT(4(A1,2HE{,I4,2H}{,I5,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(3A)') BS,'end{picture}}',
     $    ' % end of plotting histogram'
! change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 6) ILINE=1
      END
      SUBROUTINE PLCIRC(KAX,KAY,NCHX,YL,YU,YY)
!     ****************************************
! plots equidistant points, four-point interpolation,
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),IX(3000),IY(3000)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/
      SAVE DS

! ...various types of line
! ...distance between points is DS, radius of a point is IRAD
      IRAD2=6
      IRAD1=3
! .............
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $  BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
      IF(ILINE .EQ. 1) THEN
!   small filled circle
       DS = 10
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 2) THEN
!   small open circle
       DS = 10
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 3) THEN
!   big filled circle
       DS = 20
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD2,'}}}'
      ELSEIF(ILINE .EQ. 4) THEN
!   big open circle
       DS = 20
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD2,'}}}'
! Other symbols
      ELSEIF(ILINE .EQ. 5) THEN
       DS = 20
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'diamond$}}}'
      ELSE
       DS = 20
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'star$}}}'
      ENDIF
      FACY = KAY/(YU-YL)
! plot first point
      AI  = 0.
      AJ  = (APROF( (AI/KAX)*NCHX+0.5d0, NCHX, YY) -YL)*FACY
      IPNT =1
      IX(IPNT) = INT(AI)
      IY(IPNT) = INT(AJ)
      DX =  DS
      AI0 = AI
      AJ0 = AJ
! plot next points
      DO 100 IPOIN=2,3000
! iteration to get (approximately) equal distance among ploted points
      DO  50 ITER=1,3
      AI  = AI0+DX
      AJ  = (APROF( (AI/KAX)*NCHX+0.5d0, NCHX, YY) -YL)*FACY
      DX  = DX *DS/SQRT(DX**2 + (AJ-AJ0)**2)
  50  CONTINUE
      IF(INT(AJ) .GE. 0.AND.INT(AJ) .LE. KAY.AND.INT(AI) .LE. KAX) THEN
         IPNT = IPNT+1
         IX(IPNT) = INT(AI)
         IY(IPNT) = INT(AJ)
      ENDIF
      AI0 = AI
      AJ0 = AJ
      IF(INT(AI) .GT. KAX) GOTO 101
 100  CONTINUE
 101  CONTINUE
      WRITE(NOUH1,7000) (BS,IX(I),IY(I), I=1,IPNT)
7000  FORMAT(4(A1,2HR{,I4,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(2A)') BS,'end{picture}} % end of plotting line'
! change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 2) ILINE=1
      END
      FUNCTION APROF(PX,NCH,YY)
!     *************************
! PX is a continuous extension of the index in array YY
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*)
      X=PX
      IF(X .LT. 0.0.OR.X .GT. FLOAT(NCH+1)) THEN
        APROF= -1E-20
        RETURN
      ENDIF
      IP=INT(X)
      IF(IP .LT. 2)     IP=2
      IF(IP .GT. NCH-2) IP=NCH-2
      P=X-IP
      APROF = -(1./6.)*P*(P-1)*(P-2)  *YY(IP-1)
     $        +(1./2.)*(P*P-1)*(P-2)  *YY(IP  )
     $        -(1./2.)*P*(P+1)*(P-2)  *YY(IP+1)
     $        +(1./6.)*P*(P*P-1)      *YY(IP+2)
      END
      SUBROUTINE GPLSET(CH,XX)
*     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      SAVE / LPLDAT /
      CHARACTER*4 CH
      KTY=NINT(XX)
      IF(CH .EQ. 'DMOD') THEN
        ILINE=KTY
      ENDIF
      END

      SUBROUTINE gpltit(title)
*     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*64 title
!----------------------------------
! Titles ans captions for plot
      COMMON / lpltit / titch(50),keytit
      CHARACTER*64 titch
!----------------
      keytit=1
      CALL copch(title,titch(1))
      END

      SUBROUTINE gplcapt(lines)
!     ************************
! This routine defines caption and should be called
! before CALL gplot2, gpltab or bpltab2
! The matrix CHARACTER*64 lines containes text of the caption ended
! with the last line '% end-of-caption'
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*64 lines(*)
!----------------------------------
! Titles ans captions for plot
      COMMON / lpltit / titch(50),keytit
      CHARACTER*64 titch
!----------------
      keytit=0
      DO i=1,50
         titch(i)=lines(i)
         keytit= keytit+1
         IF(lines(i) .EQ. '% end-of-caption' ) GOTO 100
      ENDDO
 100  CONTINUE
      END

      SUBROUTINE gplabel(lines)
*     ************************
! This should be envoked after "CALL gplot2"
! to add lines of TeX to a given plot
*     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*64 lines(*)
!---------------------------------------------------
! This COMMON connects gplint, gplcap and gplend
      COMMON / clint / lint
!---------------------------------------------------
!----------------------------------
! Titles ans captions for plot
      COMMON / lpltit / titch(50),keytit
      CHARACTER*64 titch
!----------------
      COMMON / lpldat / nouh1,nouh2,iline
      COMMON / bslash / bs
      CHARACTER*1 BS
!
      keytit=0
      DO i=1,50
         titch(i)=lines(i)
         keytit= keytit+1
         IF(lines(i) .EQ. '% end-of-label' ) GOTO 100
      ENDDO
 100  CONTINUE
!------------------------------!
!   erase Ending               !
!------------------------------!
      BACKSPACE(NOUH1)
      BACKSPACE(NOUH1)
!
      DO i=1,keytit
        WRITE(NOUH1,'(A)')     TITCH(i)
      ENDDO
!------------------------------!
!   restore Ending             !
!------------------------------!
      WRITE(NOUH1,'(2A)') BS,'end{picture} % close entire picture '
      IF(ABS(lint) .EQ. 2) THEN
         WRITE(NOUH1,'(A)') '%====== end of gplabel =========='
      ELSE
         WRITE(NOUH1,'(2A)') BS,'end{figure}'
      ENDIF
      END


      SUBROUTINE gplot2(id,ch1,ch2,chmark,chxfmt,chyfmt)
!     **************************************************
! New version, more user-friendly of gplot
! INPUT:
!    ID          histogram identifier
!    ch1 = ' '   normal new plot
!        = 'S'   impose new plot on previous one
!    ch2 = ' '   ploting line default, contour
!        = '*'   error bars in midle of the bin
!        = 'R'   error bars at Right edge of the bin
!        = 'L'   error bars at Left  edge of the bin
!        = 'C'   slanted continuous smooth line
!    chmark =    TeX symbol for ploting points
!    chxfmt =    format (string) for labeling x-axis
!    chyfmt =    format (string) for labeling y-axis
! Furthermore:
! Captions are defined by means of
!    CALL gplcapt(capt) before CALL gplot2
!    where CHARACTER*64 capt(50) is content of
!    caption, line by line, see also comments in gplcapt routine.
! Additional text as a TeX source text can be appended by means of
!    CALL gplabel(lines) after CALL gplot2
!    where CHARACTER*64 lines(50) is the TeX add-on.
!    this is used to decorate plot with
!    any kind marks, special labels and text on the plot.
!
!     ************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER ch1,ch2,chmark*(*)
      CHARACTER*8 chxfmt,chyfmt
      SAVE
      DIMENSION yy(200),yer(200)
      CHARACTER*80 title
!---------------------------------------------------
! This COMMON connects gplint, gplcap and gplend
      COMMON / clint / lint
!---------------------------------------------------
      LOGICAL gexist
      COMMON / lpldat / nouh1,nouh2,iline
      COMMON / bslash / bs
      CHARACTER*1 BS
      CHARACTER chr
      DATA CHR /' '/
! TeX Names of the error-bar command and of the point-mark command
      CHARACTER*1 chre, chrp1
      PARAMETER ( chre = 'E', chrp1= 'R' )
      CHARACTER*2 chrp
! TeX Name of the point-mark command
      CHARACTER*1 chrx(12)
      DATA  chrx /'a','b','c','d','f','g','h','i','j','k','l','m'/
!
! RETURN if histo non-existing
      IF(.NOT.gexist(id)) GOTO 900
! ...unpack histogram
      CALL gunpak(id,yy ,'    ',idum)
      CALL gunpak(id,yer,'ERRO',idum)
      CALL ginbo1(id,title,nchx,dxl,dxu)
      xl = dxl
      xu = dxu
      CALL grang1(id,yl,yu)
      KAX=1200
      KAY=1200
      IF(CH1 .EQ. 'S') THEN
! ...superimpose plot
        incr=incr+1
        BACKSPACE(NOUH1)
        BACKSPACE(NOUH1)
      ELSE
! ...new frame only
        incr=1
        CHR=CH1
        CALL lframe(id,kax,kay,chxfmt,chyfmt)
      ENDIF
      chrp= chrp1//chrx(incr)
      WRITE(NOUH1,'(A)')    '%====gplot2:  next plot (line) =========='
      WRITE(NOUH1,'(A,I10)')'%====HISTOGRAM ID=',ID
      WRITE(NOUH1,'(A,A70 )') '% ',TITLE
      CALL goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      ker = ioperb-1
! Default line type
      IF (iopsla .EQ. 2) THEN
         CHR='C'
      ELSE
         CHR=' '
      ENDIF
! User defined line-type
      IF (CH2 .EQ. 'B')   CHR=' '
!...marks in the midle of the bin
      IF (CH2 .EQ. '*')   CHR='*'
!...marks on the right edge of the bin
      IF (CH2 .EQ. 'R')   CHR='R'
!...marks on the left edge of the bin
      IF (CH2 .EQ. 'L')   CHR='L'
      IF (CH2 .EQ. 'C')   CHR='C'
!...various types of lines
      IF     (CHR .EQ. ' ') THEN
!...contour line used for histogram
          CALL plkont(kax,kay,nchx,yl,yu,yy,ker,yer)
      ELSE IF(CHR .EQ. '*' .OR. CHR .EQ. 'R'.OR. CHR .EQ. 'L') THEN
!...marks on the right/left/midle of the bin
         CALL plmark(kax,kay,nchx,yl,yu,yy,ker,yer,chmark,chr,chrp,chre)
      ELSE IF(CHR .EQ. 'C') THEN
!...slanted (dotted) line in plotting non-MC functions
          CALL plcirc(kax,kay,nchx,yl,yu,yy)
      ENDIF
!------------------------------!
!        ENDing                !
!------------------------------!
      WRITE(NOUH1,'(2A)') BS,'end{picture} % close entire picture '
      IF(ABS(lint) .EQ. 2) THEN
         WRITE(NOUH1,'(A)') '%======= gplot2:  end of plot  =========='
      ELSE
         WRITE(NOUH1,'(2A)') BS,'end{figure}'
      ENDIF
      RETURN
  900 WRITE(*,*) ' ++++ GPLOT: NONEXISTIG HISTO ' ,ID
      END

      SUBROUTINE lframe(id,kax,kay,chxfmt,chyfmt)
!     *******************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER chxfmt*(*),chyfmt*(*)
      SAVE
!---------------------------------------------------
! This COMMON connects gplint, gplcap and gplend
      COMMON / clint / lint
!---------------------------------------------------
!----------------------------------
! Titles ans captions for plot
      COMMON / lpltit / titch(50),keytit
      CHARACTER*64 TITCH
!----------------
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      DOUBLE PRECISION DXL,DXU
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      DATA ICONT/0/

      ICONT=ICONT+1
      CALL ginbo1(id,title,nchx,dxl,dxu)
      xl = dxl
      xu = dxu
      CALL grang1(id,yl,yu)

      IF(ICONT .GT. 1) WRITE(NOUH1,'(2A)') BS,'newpage'
!------------------------------!
!           Header
!------------------------------!
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)')
     $'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE(NOUH1,'(A)')
     $'%%%%%%%%%%%%%%%%%%%%%%%%%%lframe%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      IF(ABS(lint) .EQ. 2) THEN
         WRITE(NOUH1,'(2A)') BS,'noindent'
      ELSE
         WRITE(NOUH1,'(2A)') BS,'begin{figure}[!ht]'
         WRITE(NOUH1,'(2A)') BS,'centering'
         WRITE(NOUH1,'(2A)') BS,'htmlimage{scale=1.4}'
      ENDIF
!------------------------------!
! General Caption
!------------------------------!
      IF(ABS(lint) .NE. 2) THEN
         WRITE(NOUH1,'(6A)') BS,'caption{',BS,'footnotesize',BS,'sf'
         DO i=1,keytit
            WRITE(NOUH1,'(A)')     TITCH(i)
         ENDDO
         WRITE(NOUH1,'(A)') '}'
      ENDIF
!------------------------------!
! Frames and labels
!------------------------------!
      WRITE(NOUH1,'(A)') '% =========== big frame, title etc. ======='
      WRITE(NOUH1,'(4A)') BS,'setlength{',BS,'unitlength}{0.1mm}'
      WRITE(NOUH1,'(2A)') BS,'begin{picture}(1600,1500)'
      IF( lint .LT. 0) THEN
! Big frame usefull for debuging
         WRITE(NOUH1,'(4A)') BS,'put(0,0){',BS,'framebox(1600,1500){ }}'
      ENDIF
      WRITE(NOUH1,'(A)') '% =========== small frame, labeled axis ==='
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $    BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $    BS,'put(0,0){',BS,'framebox( ',KAX,',',KAY,'){ }}'
      WRITE(NOUH1,'(A)') '% =========== x and y axis ================'
      CALL axisx(kax,xl,xu,chxfmt)
      CALL axisy(kay,yl,yu,chyfmt)
      WRITE(NOUH1,'(3A)') BS,'end{picture}}'
     $                ,'% end of plotting labeled axis'
      END

      SUBROUTINE axisx(kay,yl,yu,chxfmt)
!     ***************************************
! plotting x-axis with long and short tips
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER chxfmt*(*)
      DIMENSION tipsy(20)
      COMMON / lpldat / nouh1,nouh2,iline
      COMMON / bslash / bs
      CHARACTER*1 bs
      SAVE /lpldat/, /bslash/
!
      CHARACTER*64 fmt1,fmt2
      PARAMETER (fmt1 = '(2A,F8.2,A,F8.2,A,I4,3A)')
      PARAMETER (fmt2 = '(2A,F8.2,A,I4,A,F8.2,A,I4,3A)')

      DY= ABS(YU-YL)
      LY = NINT( LOG10(DY) -0.4999999d0 )
      JY = NINT(DY/10d0**LY)
      DDYL = DY*10d0**(-LY)
      IF( JY .EQ. 1)               DDYL = 10d0**LY*0.25d0
      IF( JY .GE. 2.AND.JY .LE. 3) DDYL = 10d0**LY*0.5d0
      IF( JY .GE. 4.AND.JY .LE. 6) DDYL = 10d0**LY*1.0d0
      IF( JY .GE. 7)               DDYL = 10d0**LY*2.0d0
      WRITE(NOUH1,'(A)') '% -------axisX---- '
      WRITE(NOUH1,'(A,I4)') '%  JY= ',JY
!-------
      NLT = INT(DY/DDYL)
      NLT = MAX0(MIN0(NLT,20),1)+1
      YY0L = NINT(YL/DDYL+0.5d0)*DDYL
      DDYS = DDYL/10d0
      YY0S = NINT(YL/DDYS+0.4999999d0)*DDYS
      P0L = KAY*(YY0L-YL)/(YU-YL)
      PDL = KAY*DDYL/(YU-YL)
      P0S = KAY*(YY0S-YL)/(YU-YL)
      PDS = KAY*DDYS/(YU-YL)
      NLT = INT(ABS(YU-YY0L)/DDYL+0.0000001d0)+1
      NTS = INT(ABS(YU-YY0S)/DDYS+0.0000001d0)+1
      DO n=1,nlt
         tipsy(n) =yy0l+ ddyl*(n-1)
      ENDDO
      WRITE(NOUH1,fmt1)
     $ BS,'multiput('  ,P0L,  ',0)('  ,PDL,  ',0){'  ,NLT,  '}{',
     $ BS,'line(0,1){25}}',
     $ BS,'multiput('  ,P0S,  ',0)('  ,PDS,  ',0){'  ,NTS,  '}{',
     $ BS,'line(0,1){10}}'
      WRITE(NOUH1,fmt2)
     $ BS,'multiput('  ,P0L,  ','  ,KAY,  ')('  ,PDL,  ',0){'  ,NLT,
     $ '}{'  ,BS,  'line(0,-1){25}}',
     $ BS,'multiput('  ,P0S,  ','  ,KAY,  ')('  ,PDS,  ',0){'  ,NTS,
     $ '}{'  ,BS,  'line(0,-1){10}}'
! ...labeling of axis
      scmx = DMAX1(DABS(yl),DABS(YU))
      lex  = NINT( LOG10(scmx) -0.50001)
      DO n=1,nlt
         k = nint(kay*(tipsy(n)-yl)/(yu-yl))
         IF(lex .LE. 3 .AND. lex .GE. -3) THEN
! ...without exponent
           WRITE(NOUH1,'(2A,I4,5A,'//chxfmt//',A)')
     $     BS,'put(',K,',-25){',BS,'makebox(0,0)[t]{',BS,'Large $ ',
     $     TIPSY(N), ' $}}'
         ELSE
! ...with exponent
           WRITE(NOUH1,'(2A,I4,5A,'//chxfmt//',2A,I4,A)')
     $     BS,'put('  ,K,  ',-25){',BS,'makebox(0,0)[t]{',BS,'Large $ ',
     $     TIPSY(N)/(10d0**LEX),BS,'cdot 10^{',LEX,'} $}}'
         ENDIF
      ENDDO
      END

      SUBROUTINE axisy(kay,yl,yu,chyfmt)
!     ***************************************
! plotting y-axis with long and short tips
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER chyfmt*(*)
      DIMENSION tipsy(20)
      COMMON / lpldat / nouh1,nouh2,iline
      COMMON / bslash / BS
      CHARACTER*1 bs
      SAVE /lpldat/, /bslash/
!
      CHARACTER*64 fmt1,fmt2
      PARAMETER (fmt1 = '(2A,F8.2,A,F8.2,A,I4,3A)')
      PARAMETER (fmt2 = '(2A,I4,A,F8.2,A,F8.2,A,I4,3A)')

      DY= ABS(YU-YL)
      LY = NINT( LOG10(DY) -0.49999999d0 )
      JY = NINT(DY/10d0**LY)
      DDYL = DY*10d0**(-LY)
      IF( JY .EQ. 1)               DDYL = 10d0**LY*0.25d0
      IF( JY .GE. 2.AND.JY .LE. 3) DDYL = 10d0**LY*0.5d0
      IF( JY .GE. 4.AND.JY .LE. 6) DDYL = 10d0**LY*1.0d0
      IF( JY .GE. 7)               DDYL = 10d0**LY*2.0d0
      WRITE(NOUH1,'(A)') '% --------saxiY------- '
      WRITE(NOUH1,'(A,I4)') '%  JY= ',JY
!-------
      NLT = INT(DY/DDYL)
      NLT = MAX0(MIN0(NLT,20),1)+1
      YY0L = NINT(YL/DDYL+0.4999999d0)*DDYL
      DDYS = DDYL/10d0
      YY0S = NINT(YL/DDYS+0.5d0)*DDYS
      P0L = KAY*(YY0L-YL)/(YU-YL)
      PDL = KAY*DDYL/(YU-YL)
      P0S = KAY*(YY0S-YL)/(YU-YL)
      PDS = KAY*DDYS/(YU-YL)
      NLT= INT(ABS(YU-YY0L)/DDYL+0.0000001d0) +1
      NTS= INT(ABS(YU-YY0S)/DDYS+0.0000001d0) +1
      DO N=1,NLT
         tipsy(n) =yy0l+ ddyl*(n-1)
      ENDDO
! plotting tics on vertical axis
      WRITE(NOUH1,fmt1)
     $ BS,'multiput(0,'  ,P0L,  ')(0,'  ,PDL  ,'){'  ,NLT,  '}{',
     $ BS,'line(1,0){25}}',
     $ BS,'multiput(0,'  ,P0S,  ')(0,'  ,PDS,  '){'  ,NTS,  '}{',
     $ BS,'line(1,0){10}}'
      WRITE(NOUH1,fmt2)
     $ BS,'multiput('  ,KAY,  ','  ,P0L,  ')(0,'  ,PDL,  '){'  ,NLT,
     $ '}{',BS,'line(-1,0){25}}',
     $ BS,'multiput('  ,KAY,  ','  ,P0S,  ')(0,'  ,PDS,  '){'  ,NTS,
     $ '}{',BS,'line(-1,0){10}}'
! ...Zero line if necessary
      Z0L = KAY*(-YL)/(YU-YL)
      IF(Z0L .GT. 0D0.AND.Z0L .LT. FLOAT(KAY))
     $      WRITE(NOUH1,'(2A,F8.2,3A,I4,A)')
     $       BS,'put(0,'  ,Z0L,  '){',BS,'line(1,0){'  ,KAY,  '}}'
! ...labeling of axis
      SCMX = DMAX1(DABS(YL),DABS(YU))
      LEX  = NINT( LOG10(SCMX) -0.50001d0)
      DO n=1,nlt
         k = nint(kay*(tipsy(n)-yl)/(yu-yl))
         IF(lex .LE. 3 .AND. lex .GE. -3) THEN
! ...without exponent
            WRITE(NOUH1,'(2A,I4,5A,'//chyfmt//',A)')
     $           BS,'put(-25,'  ,K,  '){',BS,'makebox(0,0)[r]{',
     $           BS,'Large $ '  ,TIPSY(N),  ' $}}'
         ELSE
! ...with exponent
            WRITE(NOUH1,'(2A,I4,5A,'//chyfmt//',2A,I4,A)')
     $           BS,'put(-25,'  ,K,  '){',BS,'makebox(0,0)[r]{',
     $           BS,'Large $ '
     $           ,TIPSY(N)/(10d0**LEX),  BS,'cdot 10^{'  ,LEX,  '} $}}'
      ENDIF
      ENDDO
      END

      SUBROUTINE plkont(kax,kay,nchx,yl,yu,yy,ker,yer)
!     ************************************************
! For the moment unchanged
!     ************************************************
! plotting contour line for histogram
!     ***********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),YER(*)
      CHARACTER*80 FMT1
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $  BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
!...various types of line
      IF(ILINE .EQ. 1) THEN
         WRITE(NOUH1,'(2A)') BS,'thicklines '
      ELSE
         WRITE(NOUH1,'(2A)') BS,'thinlines '
      ENDIF
!...short macros for vertical/horizontal straight lines
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'x}[3]{',BS,'put(#1,#2){',
     $ BS,'line(1,0){#3}}}'
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'y}[3]{',BS,'put(#1,#2){',
     $ BS,'line(0,1){#3}}}'
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'z}[3]{',BS,'put(#1,#2){',
     $ BS,'line(0,-1){#3}}}'
!   error bars
      WRITE(NOUH1,'(8A)')
     $   BS,'newcommand{',BS,'e}[3]{',
     $   BS,'put(#1,#2){',BS,'line(0,1){#3}}}'
      IX0=0
      IY0=0
      DO 100 IB=1,NCHX
      IX1 = NINT(KAX*(IB-0.00001)/NCHX)
      IY1 = NINT(KAY*(YY(IB)-YL)/(YU-YL))
      IDY = IY1-IY0
      IDX = IX1-IX0
      FMT1 = '(2(2A,I4,A,I4,A,I4,A))'
      IF( IDY .GE. 0) THEN
         IF(IY1 .GE. 0.AND.IY1 .LE. KAY)
     $   WRITE(NOUH1,FMT1) BS,'y{',IX0,'}{',IY0,'}{',IDY,'}',
     $                     BS,'x{',IX0,'}{',IY1,'}{',IDX,'}'
      ELSE
         IF(IY1 .GE. 0.AND.IY1 .LE. KAY)
     $   WRITE(NOUH1,FMT1) BS,'z{',IX0,'}{',IY0,'}{',-IDY,'}',
     $                     BS,'x{',IX0,'}{',IY1,'}{',IDX,'}'
      ENDIF
      IX0=IX1
      IY0=IY1
      IF(KER .EQ. 1) THEN
        IX2  = NINT(KAX*(IB-0.5000d0)/NCHX)
        IERR = NINT(KAY*((YY(IB)-YER(IB))-YL)/(YU-YL))
        IE = NINT(KAY*YER(IB)/(YU-YL))
        IF(IY1 .GE. 0.AND.IY1 .LE. KAY.and.abs(ierr) .LE. 9999
     $     .and.2*ie .LE. 9999) WRITE(NOUH1,8000) BS,IX2,IERR,IE*2
      ENDIF
 100  CONTINUE
8000  FORMAT(4(A1,2He{,I4,2H}{,I5,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(3A)') BS,'end{picture}}',
     $       ' % end of plotting histogram'
! change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 2) ILINE=1
      END

      SUBROUTINE plmark(kax,kay,nchx,yl,yu,yy,ker,yer,chmark,chr,chr2,
     &                                                           chr3)
!     ***************************************************************
! marks in the midle of the bin
!     **********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*1 chr
      DIMENSION yy(*),yer(*)
      CHARACTER chmark*(*),chr2*(*),chr3*(*)
      COMMON / lpldat / nouh1,nouh2,iline
      COMMON / bslash / bs
      CHARACTER*1 bs

      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $ BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ======= plmark: plotting primitives ======='

! Plotting symbol
      WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,chr2  , '}[2]{',
     $   BS,'put(#1,#2){',chmark,'}}'
! Error bar symbol
      WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,chr3  , '}[3]{',
     $   BS,'put(#1,#2){',BS,'line(0,1){#3}}}'

      DO ib=1,nchx
         IF(chr .EQ. '*') THEN
            IX1 = NINT(KAX*(IB-0.5000d0)/NCHX) ! Midle of bin
         ELSEIF(chr .EQ. 'R') THEN
            IX1 = NINT(KAX*(IB*1d0)/NCHX)      ! Right edge of bin
         ELSEIF(chr .EQ. 'L') THEN
            IX1 = NINT(KAX*(IB-1D0)/NCHX)      ! Left edge of bin
         ELSE
            WRITE(6,*) '+++++ plamark: wrong line type:',chr
            RETURN
         ENDIF
         IY1 = NINT(KAY*(YY(IB)-YL)/(YU-YL))
         IF(IY1 .GE. 0.AND.IY1 .LE. KAY)
     $   WRITE(NOUH1,'(A,A,A,I4,A,I4,A)')
     $               BS,chr2, '{' ,IX1, '}{' ,IY1, '}'
         IF(KER .EQ. 1) THEN
            IERR = NINT(KAY*((YY(IB)-YER(IB))-YL)/(YU-YL))
            IE   = NINT(KAY*YER(IB)/(YU-YL))
            IF(iy1 .GE. 0 .AND. iy1 .LE. kay
     $         .AND. ABS(ierr) .LE. 9999 .AND. 2*ie .LE. 9999)
     $      WRITE(NOUH1,'(A,A,A,I4,A,I5,A,I4,A)')
     $          BS, chr3,  '{'  ,IX1, '}{'  ,IERR, '}{'  ,IE*2,   '}'
         ENDIF
      ENDDO
      WRITE(NOUH1,'(3A)') BS,'end{picture}}',
     $    ' % end of plotting histogram'
      END


      SUBROUTINE gpltab(Npl,idl,capt,fmt,nch1,incr,npag)
!     ******************************************************
! Tables in TeX, up to 9 columns
! Npl           = numbers of columns/histograms
! idl(1:Npl)    = list of histo id's
! capt(1:Npl+1) = list of captions above each column
! fmt(1:1)      = format to print x(i) in first columb,
!                 h(i) and error he(i) in further columns
! nch1,incr     = raws are printet in the sequence
!                 (h(i),he(i),i=nch1,nbin,incr), nbin is no. of bins.
! npag          = 0 no page eject, =1 with page eject
!     ******************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
!------- parameters
      DIMENSION    idl(*)
      CHARACTER*16 capt(*)
      CHARACTER*8  fmt(3)
!----------------------------------
! Titles and captions for plot
      COMMON / lpltit / titch(50),keytit
      CHARACTER*64 TITCH
!----------------
      COMMON / LPLDAT / nouh1,nouh2,iline
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/
!-----------------
      LOGICAL gexist
      DIMENSION yyy(200),yer(200),bi(200,9),er(200,9)
      CHARACTER*80 title
      CHARACTER*1 Cn(9)
      DATA Cn /'1','2','3','4','5','6','7','8','9'/
!----------

! RETURN if histo non-existing or to many columns
      IF(.NOT.GEXIST(ID)) GOTO 900
      IF(Npl .GT. 9 )     GOTO 901
!
! npack histograms
      CALL ginbo1( idl(1),title,nchx,dxl,dxu)
      xl = dxl
      xu = dxu
      DO n=1,Npl
        CALL gunpak( idl(n),yyy ,'    ',idum)
        CALL gunpak( idl(n),yer ,'ERRO',idum)
        DO k=1,nchx
           bi(k,n)=yyy(k)
           er(k,n)=yer(k)
        ENDDO
      ENDDO
!------------------------------!
!           Header
!------------------------------!
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') '% ========================================='
      WRITE(NOUH1,'(A)') '% ============= begin table ==============='
      WRITE(NOUH1,'(2A)') BS,'begin{table}[!ht]'
      WRITE(NOUH1,'(2A)') BS,'centering'
!------------------------------!
! Central Caption
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'caption{',BS,'small'
      DO i=1,keytit
        WRITE(NOUH1,'(A)')     TITCH(i)
      ENDDO
      WRITE(NOUH1,'(A)') '}'
!------------------------------!
! Tabular header
!------------------------------!
      WRITE(NOUH1,'(20A)') BS,'begin{tabular}
     $ {|',  ('|c',j=1,Npl+1),  '||}'
!
      WRITE(NOUH1,'(4A)') BS,'hline',BS,'hline'
!------------------------------!
! Captions in columns
!------------------------------!
      WRITE(NOUH1,'(2A)') capt(1),('&',capt(j+1),j=1,Npl)
!
      WRITE(NOUH1,'(2A)') BS,BS
      WRITE(NOUH1,'(2A)') BS,'hline'
!----------------------------------------!
! Table content
! Note that by default RIGHT EDGE of bin is printed, as necessary for
! cumulative distributions, this can be changed with SLAN option
!----------------------------------------!
      CALL goptou(idl(1),ioplog,iopsla,ioperb,iopsc1,iopsc2)
      DO k=nch1,nchx,incr
        xi= dxl + (dxu-dxl)*k/(1d0*nchx)
        IF(iopsla.eq.2) xi= dxl + (dxu-dxl)*(k-0.5d0)/(1d0*nchx)
        IF(ioperb.eq.2) THEN
        WRITE(NOUH1,
     $  '(A,'//fmt(1)//'
     $     ,      '//Cn(Npl)//'(A,'//fmt(2)//',A,A,'//fmt(3)//'),  A)')
     $   '$', xi, ('$ & $', bi(k,j), BS, 'pm', er(k,j), j=1,Npl), '$'
        WRITE(NOUH1,'(2A)') BS,BS
        ELSE
        WRITE(NOUH1,
     $  '(A,'//fmt(1)//'
     $     ,      '//Cn(Npl)//'(A,'//fmt(2)//'),  A)')
     $   '$', xi, ('$ & $', bi(k,j), j=1,Npl), '$'
        WRITE(NOUH1,'(2A)') BS,BS
        ENDIF
      ENDDO
!------------------------------!
! Ending
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'hline',BS,'hline'
      WRITE(NOUH1,'(2A)') BS,'end{tabular}'
      WRITE(NOUH1,'(2A)') BS,'end{table}'
      WRITE(NOUH1,'(A)') '% ============= end   table ==============='
      WRITE(NOUH1,'(A)') '% ========================================='
      IF(npag .NE. 0) WRITE(NOUH1,'(2A)') BS,'newpage'

      RETURN
  900 WRITE(*,*) ' ++++ gpltab: NONEXISTIG HISTO ' ,ID
      RETURN
 901  WRITE(*,*) ' ++++ gpltab: TO MANY COLUMNS  ' ,Nplt
      END

      SUBROUTINE gpltab2(Npl,idl,ccapt,mcapt,fmt,chr1,chr2,chr3)
!     **********************************************************
! Tables in TeX, up to 9 columns
! Npl           = numbers of columns/histograms
! idl(1:Npl)    = list of histo id's
! ccapt(1:Npl+1)= list of column-captions above each column
! mcapt         = multicolumn header, none if mcapt=' ',
! fmt(1:1)      = format to print x(i) in first columb,
!                 h(i) and error he(i) in further columns
! chr1          = ' ' normal default, ='S' the Same table continued
! chr2          = ' ' midle of the bin for x(i) in the first column
!               = 'R' right edge,     = 'L' left edge of the bin
! chr3          = ' ' no page eject,  ='E' with page eject at the end.
! Furthermore:
! Captions are defined by means of
!    CALL gplcapt(capt) before CALL gpltab2
!    where CHARACTER*64 capt(50) is content of
!    caption, line by line, see also comments in gplcapt routine.
!
!     ******************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
!------- parameters
      DIMENSION    idl(*)
      CHARACTER*16 ccapt(*)
      CHARACTER*8  fmt(3)
      CHARACTER*1  chr1,chr2,chr3
      CHARACTER*64 mcapt
!---------------------------------------------------
! This COMMON connects gplint, gplcap and gplend
      COMMON / clint / lint
!---------------------------------------------------
!----------------------------------
! Titles and captions for plot
      COMMON / lpltit / titch(50),keytit
      CHARACTER*64 TITCH
!----------------
      COMMON / LPLDAT / nouh1,nouh2,iline
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/
!-----------------
      LOGICAL gexist
      DIMENSION yyy(200),yer(200),bi(200,9),er(200,9)
      CHARACTER*80 title
      CHARACTER*1 Cn(9)
      DATA Cn /'1','2','3','4','5','6','7','8','9'/
!----------

! RETURN if histo non-existing or to many columns
      IF(.NOT.GEXIST(ID)) GOTO 900
      IF(Npl .GT. 9 )     GOTO 901
!
! npack histograms
      CALL ginbo1( idl(1),title,nchx,dxl,dxu)
      xl = dxl
      xu = dxu
      DO n=1,Npl
        CALL gunpak( idl(n),yyy ,'    ',idum)
        CALL gunpak( idl(n),yer ,'ERRO',idum)
        DO k=1,nchx
           bi(k,n)=yyy(k)
           er(k,n)=yer(k)
        ENDDO
      ENDDO

      IF(chr1 .EQ. ' ' ) THEN
!------------------------------!
!           Header
!------------------------------!
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') '% ========================================'
      WRITE(NOUH1,'(A)') '% ============ begin table ==============='
      IF(ABS(lint) .EQ. 2 ) THEN
         WRITE(NOUH1,'(2A)') BS,'noindent'
      ELSE
        WRITE(NOUH1,'(2A)') BS,'begin{table}[!ht]'
        WRITE(NOUH1,'(2A)') BS,'centering'
      ENDIF
!------------------------------!
! Central Caption
!------------------------------!
      IF(ABS(lint) .NE. 2 ) THEN
         WRITE(NOUH1,'(6A)') BS,'caption{',BS,'footnotesize',BS,'sf'
         DO i=1,keytit
            WRITE(NOUH1,'(A)')     TITCH(i)
         ENDDO
         WRITE(NOUH1,'(A)') '}'
      ENDIF
!------------------------------!
! Tabular header
!------------------------------!
      WRITE(NOUH1,'(20A)') BS,'begin{tabular}
     $ {|',  ('|c',j=1,Npl+1),  '||}'
!
      WRITE(NOUH1,'(4A)') BS,'hline',BS,'hline'
!------------------------------!
! Captions in columns
!------------------------------!
      WRITE(NOUH1,'(2A)') ccapt(1),('&',ccapt(j+1),j=1,Npl)
!
      ELSEIF(chr1 .EQ. 'S' ) THEN
         DO i=1,6
            BACKSPACE(NOUH1)
         ENDDO
      ELSE
         WRITE(*,*) ' ++++ gpltab2: WRONG chr1 ' ,chr1
      ENDIF

      WRITE(NOUH1,'(2A)') BS,BS
      WRITE(NOUH1,'(2A)') BS,'hline'

!------------------------------!
! Optional multicolumn caption
!------------------------------!
      IF(mcapt .NE. ' ') THEN
         WRITE(NOUH1,'(3A,I2,A)') '& ',BS,'multicolumn{',Npl,'}{c||}{'
         WRITE(NOUH1,'(3A)') '     ',mcapt, ' }'
         WRITE(NOUH1,'(2A)') BS,BS
         WRITE(NOUH1,'(2A)') BS,'hline'
      ENDIF

!----------------------------------------!
! Table content
! Note that by default RIGHT EDGE of bin is printed, as necessary for
! cumulative distributions, this can be changed with SLAN option
!----------------------------------------!
      CALL goptou(idl(1),ioplog,iopsla,ioperb,iopsc1,iopsc2)
      DO k=1,nchx
         IF(chr2 .EQ. 'R') THEN
! right
            xi= dxl + (dxu-dxl)*k/(1d0*nchx)
         ELSEIF(chr2 .EQ. 'L') THEN
! left
            xi= dxl + (dxu-dxl)*(k-1d0)/(1d0*nchx)
         ELSE
! midle
            xi= dxl + (dxu-dxl)*(k-0.5d0)/(1d0*nchx)
         ENDIF
         IF(ioperb.eq.2) THEN
          WRITE(NOUH1,
     $    '(A,'//fmt(1)//'
     $     ,      '//Cn(Npl)//'(A,'//fmt(2)//',A,A,'//fmt(3)//'),  A)')
     $    '$', xi, ('$ & $', bi(k,j), BS, 'pm', er(k,j), j=1,Npl), '$'
          WRITE(NOUH1,'(2A)') BS,BS
         ELSE
          WRITE(NOUH1,
     $    '(A,'//fmt(1)//'
     $     ,      '//Cn(Npl)//'(A,'//fmt(2)//'),  A)')
     $    '$', xi, ('$ & $', bi(k,j), j=1,Npl), '$'
          WRITE(NOUH1,'(2A)') BS,BS
         ENDIF
      ENDDO
!------------------------------!
! Ending
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'hline',BS,'hline'
      WRITE(NOUH1,'(2A)') BS,'end{tabular}'
      IF(ABS(lint) .EQ. 2 ) THEN
         WRITE(NOUH1,'(A)') '% ========================================'
      ELSE
         WRITE(NOUH1,'(2A)') BS,'end{table}'
      ENDIF
      WRITE(NOUH1,'(A)') '% ============= end   table =============='
      WRITE(NOUH1,'(A)') '% ========================================'
      IF(chr3 .EQ. 'E') WRITE(NOUH1,'(2A)') BS,'newpage'

      RETURN
  900 WRITE(*,*) ' ++++ gpltab2: NONEXISTIG HISTO ' ,ID
      RETURN
 901  WRITE(*,*) ' ++++ gpltab2: TO MANY COLUMNS  ' ,Nplt
      END


      SUBROUTINE gmonit(mode,id,wt,wtmax,rn)
!     **************************************
! Utility program for monitoring m.c. rejection weights.
! ---------------------------------------------------------
! It is backward compatible with WMONIT except:
!  (1) for id=-1 one  should call as follows:
!      gmonit(-1,id,0d0,1d0,1d0) or skip initialisation completely!
!  (2) maximum absolute weight is looked for,
!  (3) gprint(-id) prints weight distribution, net profit!
!  (4) no restriction id<100 any more!
! ---------------------------------------------------------
! wt is weight, wtmax is maximum weight and rn is random number.
! IF(mode .EQ. -1) then
!          initalization if entry id,
!        - wtmax is maximum weight used for couting overweighted
!          other arguments are ignored
! ELSEIF(mode .EQ. 0) then
!          summing up weights etc. for a given event for entry id
!        - wt is current weight.
!        - wtmax is maximum weight used for couting overweighted
!          events with wt>wtmax.
!        - rn is random number used in rejection, it is used to
!          count no. of accepted (rn < wt/wtmax) and rejected
!          (wt > wt/wtmax) events,
!          if ro rejection then put rn=0d0.
! ELSEIF(mode .EQ. 1) THEN
!          in this mode wmonit repports on accumulated statistics
!          and the information is stored in COMMON /cmonit/
!        - averwt= average weight wt counting all event
!        - errela= relative error of averwt
!        - nevtot= total number of accounted events
!        - nevacc= no. of accepted events (rn < wt/wtmax)
!        - nevneg= no. of events with negative weight (wt < 0)
!        - nevzer= no. of events with zero weight (wt = 0d0)
!        - nevove= no. of overweghted events (wt > wtmax)
!          and if you do not want to use cmonit then the value
!          the value of averwt is assigned to wt,
!          the value of errela is assigned to wtmax and
!          the value of wtmax  is assigned to rn in this mode.
! ELSEIF(mode .EQ. 2) THEN
!          all information defined for entry id defined above
!          for mode=2 is just printed of unit nout
! ENDIF
! note that output repport (mode=1,2) is done dynamically just for a
! given entry id only and it may be repeated many times for one id and
! for various id's as well.
!     ************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
! special gmonit COMMON
      COMMON / cmonit/ averwt,errela,nevtot,nevacc,nevneg,nevove,nevzer
      SAVE / cglib /,/gind/, /cmonit/
!
      idg = -id
      IF(id .LE. 0) THEN
           WRITE(nout,*) ' =====> Gmonit: wrong id= ',id
           WRITE(   6,*) ' =====> Gmonit: wrong id= ',id
           STOP
      ENDIF
      IF(mode .EQ. -1) THEN
!     *******************
           nbin = nint(dabs(rn))
           IF(nbin .GT. 100) nbin =100
           IF(nbin .EQ. 0)   nbin =1
           xl   =  wt
           xu   =  wtmax
           IF(xu .LE. xl) THEN
             xl = 0d0
             xu = 1d0
           ENDIF
           lact=jadres(idg)
           IF(lact .EQ. 0) THEN
              CALL gbook1(idg,' gmonit $',nbin,xl,xu)
           ELSE
              WRITE(nout,*) ' WARNING gmonit: exists, id= ',id
              WRITE(   6,*) ' WARNING gmonit: exists, id= ',id
           ENDIF
      ELSEIF(mode .EQ. 0) THEN
!     **********************
           lact=jadres(idg)
           IF(lact .EQ. 0) THEN
              WRITE(nout,*) ' *****> Gmonit: uninitialized, id= ',id
              WRITE(   6,*) ' *****> Gmonit: uninitialized, id= ',id
              CALL gbook1(idg,' gmonit $',1,0d0,1d0)
              lact=jadres(idg)
           ENDIF
!     standard entries
           CALL gf1(idg,wt,1d0)
!     additional goodies
           ist  = index(lact,2)
           ist2 = ist+7
           ist3 = ist+11
!    maximum weight -- maximum by absolute value but keeping sign
           b(ist3+13)    = max( dabs(b(ist3+13)) ,dabs(wt))
           IF(wt .NE. 0d0) b(ist3+13)=b(ist3+13) *wt/dabs(wt)
!    nevzer,nevove,nevacc
           IF(wt .EQ. 0d0)        b(ist3+10) =b(ist3+10) +1d0
           IF(wt .GT. wtmax)      b(ist3+11) =b(ist3+11) +1d0
           IF(rn*wtmax .LE. wt)   b(ist3+12) =b(ist3+12) +1d0
      ELSEIF(mode .GE. 1.or.mode .LE. 3) THEN
!     ***********************************
           lact=jadres(idg)
           IF(lact .EQ. 0) THEN
              WRITE(nout,*) ' +++++++++ STOP in  wmonit ++++++++++++++'
              WRITE(   6,*) ' +++++++++ STOP in  wmonit ++++++++++++++'
              WRITE(nout,*) ' lack of initialization, id=',id
              WRITE(   6,*) ' lack of initialization, id=',id
              STOP
           ENDIF
           ist    = index(lact,2)
           ist2   = ist+7
           ist3   = ist+11
           ntot   = nint(b(ist3 +7))
           swt    =      b(ist3 +8)
           sswt   =      b(ist3 +9)
           IF(ntot .LE. 0 .or. swt  .EQ.  0d0 )  THEN
              averwt=0d0
              errela=0d0
           ELSE
              averwt=swt/float(ntot)
              errela=sqrt(abs(sswt/swt**2-1d0/float(ntot)))
           ENDIF
! output through COMMONs
           nevtot = ntot
           nevacc = b(ist3 +12)
           nevneg = b(ist3  +1)
           nevzer = b(ist3 +10)
           nevove = b(ist3 +11)
           wwmax  = b(ist3 +13)
!  output through parameters
           wt     = averwt
           wtmax  = errela
           rn     = wwmax
!  no printout for mode > 1
!  ************************
           IF(mode .EQ. 1) RETURN
           WRITE(nout,1003) id, averwt, errela, wwmax
           WRITE(nout,1004) nevtot,nevacc,nevneg,nevove,nevzer
           IF(mode .EQ. 2) RETURN
           CALL gprint(idg)
      ELSE
!     ****
           WRITE(nout,*) ' =====wmonit: wrong mode',mode
           WRITE(   6,*) ' =====wmonit: wrong mode',mode
           STOP
      ENDIF
!     *****
 1003 format(
     $  ' =======================gmonit========================'
     $/,'   id           averwt         errela            wwmax'
     $/,    i5,           e17.7,         f15.9,           e17.7)
 1004 format(
     $  ' -----------------------------------------------------------'
     $/,'      nevtot      nevacc      nevneg      nevove      nevzer'
     $/,   5i12)
      END
!!!! LOGBOOK of corrections since 24 Nov 91 !!!!!
!
! * line in MARRAN to long ( in printout of ijkl)
! * CHBIN2 replaced by CHBIN1
!  !!!!!!!!!!!!!!

C Library of utilities for YFS and BHLUMI programs
C version 1.0 November 91
      SUBROUTINE CHBIN3(R,ALF,BET,X,XPRIM,DJAC)
C     *****************************************
C Written: Dec. 1991
C This routine mapps variable R into X, XPRIM=1-X.
C To be employed in the integration (either ordinary or Monte Carlo)
C of any distributions resambling the binomial distribution
C             x**(alf-1)*(1-x)**(bet-1).
C with 1> alf,bet > 0. Variables R and X are  in (0,1) range.
C Djac is the Jacobian factor d(x)/d(r).
C Mapping is such that 1/djac is very close to
C binomial distribution x**(alf-1)*(1-x)**(bet-1).
C WARNING:
C Mapping may fail very close to R=0 and R=1. User is recommended
C to assure that: fleps**alf < R < 1-fleps**bet,
C where fleps = 1.d-30.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / INOUT  / NINP,NOUT
      SAVE   / INOUT  /
C
      IF( ALF.LE.1D-10 .OR. ALF .GT. 3D0 ) GOTO 900
      IF( BET.LE.1D-10 .OR. BET .GT. 3D0 ) GOTO 900
      X0=(1D0-ALF)/(2D0-ALF-BET)
      X0= MIN( MAX(X0, 0.001D0), 0.999D0)
      Q1=       X0**ALF            *BET*(1D0-X0)**(BET-1D0)
      Q2=       ALF*X0**(ALF-1D0)  *((1D0-X0)**BET)
      P1= Q1/(Q1+Q2)
      IF( R.LE.P1 ) THEN
         X    =  X0*(R/P1)**(1D0/ALF)
         XPRIM=  1D0-X
         DIST =  ALF* X**(ALF-1D0)  *BET*(1D0-X0)**(BET-1D0)
ccc      write(6,*) '3A:x,x1=',x,xprim
      ELSE
         XPRIM=  (1-X0)*((1D0-R)/(1D0-P1))**(1D0/BET)
         X    =  1D0- XPRIM
         DIST =  ALF*X0**(ALF-1D0)  *BET*XPRIM**(BET-1D0)
ccc      write(6,*) '3B:x,x1=',x,xprim
      ENDIF
      DJAC    =  (Q1+Q2)/DIST
      RETURN
  900 WRITE(NOUT,*) ' ++++ STOP IN CHBIN3: wrong parameters'
      WRITE(   6,*) ' ++++ STOP IN CHBIN3: wrong parameters'
      STOP
      END

      SUBROUTINE CHBIN1(R,ALF,BET,XMAX,X,DJAC)
C     ****************************************
C     last correction Dec. 91
c this mapps variable r into x.
c to be employed in the integration (either ordinary or monte carlo)
c of distributions resambling
c the binomial distribution x**(alf-1)*(1-x)**(bet-1)
c with alf > 0 and  bet arbitrary.
c variable r is in (0,1) range and x is within (0,xmax) range.
c djac is jacobian factor d(x)/d(r).
c mapping is such that 1/djac is very close to
c binomial distribution x**(alf-1)*(1-x)**(bet-1).
c WARNING: mapping may fail very close to R=0. Practically, one is
c recommended to obey: fleps**alf < r, where fleps = 1.d-30.
c Problems may also arise for very small xmax ( below 1.d-12 ).
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / INOUT  / NINP,NOUT
      save   / INOUT  /
C
      IF( ALF.LE.0D0 ) GOTO 900
      X0=(ALF-1D0)/(ALF+BET-2D0)
      IF(X0.GT.XMAX) X0=XMAX
      X0= MAX(X0, 0D0)
      Q1= 1D0/ALF*X0**ALF  *(1D0-X0)**(BET-1D0)
      Q2= X0**(ALF-1D0) /BET*((1D0-X0)**BET-(1D0-XMAX)**BET)
      P1= Q1/(Q1+Q2)
      IF( R.LE.P1 ) THEN
         X=X0*(R/P1)**(1D0/ALF)
         DIST= X**(ALF-1D0)*(1D0-X0)**(BET-1D0)
      ELSE
         R1= (1D0-R)/(1D0-P1)
         X = (1D0-XMAX)**BET + ((1D0-X0)**BET-(1D0-XMAX)**BET)*R1
         X = 1D0 - X**(1D0/BET)
         DIST= X0**(ALF-1D0)*(1D0-X)**(BET-1D0)
      ENDIF
      DJAC=(Q1+Q2)/DIST
      RETURN
  900 WRITE(NOUT,*) ' ========= STOP IN CHBIN1: WRONG PARAMS'
      STOP
      END


      SUBROUTINE VESK1W(MMODE,FUNSKO,PAR1,PAR2,PAR3)
C     **********************************************
C======================================================================
C======================================================================
C===================== V E S K 1 W ====================================
C==================S. JADACH  SEPTEMBER 1985===========================
C==================S. JADACH  November  1991===========================
C======================================================================
C ONE DIMENSIONAL MONTE CARLO  SAMPLER.
C Vesrion with weighted events!
C DOUBLE PRECISION  FUNCTION FUNSKO IS THE DISTRIBUTION TO BE GENERATED.
C JLIM1 IS THE NUMBER OF ENTRIES IN THE EQUIDISTANT LATICE WHICH
C IS FORMED IN THE FIRST STAGE AND JLIM2 IS THE TOTAL MAXIMUM
C NUMBER OF ENTRIES IN THE LATICE, NOTE THAT DIMENSIONS OF
C MATRICES IN /CESK8A/ SHOULD BE AT LEAST JLIM2+1 .
C FOR MILD FUNSKO JLIM2=128 IS ENOUGH.
C TO CREATE AN INDEPENDENT VERSION REPLACE /ESK8A/=>/ESK8B/.
C     **********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save
      COMMON / CESK1W / XX(1025),YY(1025),ZINT(1025),ZSUM,JMAX
      COMMON / INOUT  / NINP,NOUT
      EXTERNAL FUNSKO
      DATA JLIM1,JLIM2/16,257/
      DATA IWARM/0/
C
      MODE=MMODE
      IF(MODE.EQ.-1) THEN
C     ===================
C INITIALISATION PART, SEE VINSKO FOR MORE COMMENTS
      INIRAN=1
      IWARM=1
      WT=0.
      SWT=0.
      SSWT=0.
      NEVS=0
C INITIALISATION PART, SAMPLING DISTRIBUTION FUNSKO
C AND FILLING MATRICES XX,YY,ZINT ETC.
      JMAX=1
      XX(1)=0.
      XX(2)=1.
      YY(1)=FUNSKO(XX(1))
      YY(2)=FUNSKO(XX(2))
      IF(YY(1).LT.0.0.OR.YY(2).LT.0.0) GO TO 999
      ZINT(1)=.5D0*(YY(2)+YY(1))*(XX(2)-XX(1))
C
      JDIV=1
      DO 200 K=1,JLIM2-1
      IF(JMAX.LT.JLIM1) THEN
C...    NOTE THAT DESK1W INCREMENTS JMAX=JMAX+1 IN EVERY CALL
        CALL DESK1W(FUNSKO,JDIV)
        JDIV=JDIV+2
        IF(JDIV.GT.JMAX) JDIV=1
      ELSE
        JDIV=1
        ZMX=ZINT(1)
        DO 180 J=1,JMAX
        IF(ZMX.LT.ZINT(J)) THEN
          ZMX=ZINT(J)
          JDIV=J
        ENDIF
  180   CONTINUE
        CALL DESK1W(FUNSKO,JDIV)
      ENDIF
  200 CONTINUE
C
C...  FINAL ADMINISTRATION, NORMALIZING ZINT ETC.
      ZSUM1=0.
      ZSUM =0.
      DO 220 J=1,JMAX
      ZSUM1=ZSUM1+ZINT(J)
      YMAX= MAX( YY(J+1),YY(J))
      ZINT(J)=YMAX*(XX(J+1)-XX(J))
  220 ZSUM=ZSUM+ZINT(J)
      SUM=0.
      DO 240 J=1,JMAX
      SUM=SUM+ZINT(J)
  240 ZINT(J)=SUM/ZSUM
C====>>>
C Crude x-section estimate
ccc      CINTEG=ZSUM
ccc      ERRINT=0D0
      PAR1=  ZSUM
      PAR2=  ZSUM
      PAR3=  ZSUM
C===<<<
      ELSE IF(MODE.EQ.0) THEN
C     =======================
C GENERATION PART
      IF(IWARM.EQ.0) GOTO 901
ccc  222 CONTINUE
ccc      IF( (WT-1D0).GT.1D-10) THEN
ccc        WT=WT-1.D0
ccc      ELSE
        CALL VARRAN(RNUMB,1)
        DO 215 J=1,JMAX
        JSTOP=J
  215   IF(ZINT(J).GT.RNUMB) GOTO 216
  216   CONTINUE
        IF(JSTOP.EQ.1) THEN
          D=RNUMB/ZINT(1)
        ELSE
          D =(RNUMB-ZINT(JSTOP-1))/(ZINT(JSTOP)-ZINT(JSTOP-1))
        ENDIF
        X=XX(JSTOP)*(1.D0 -D )+XX(JSTOP+1)*D
        FN=FUNSKO(X)
        IF(FN.LT.0.D0) GOTO 999
        YYMAX=MAX(YY(JSTOP+1),YY(JSTOP))
        WT=FN/YYMAX
        NEVS=NEVS+1
        SWT=SWT+WT
        SSWT=SSWT+WT*WT
ccc      ENDIF
ccc      CALL VARRAN(RNUMB,1)
ccc      IF(RNUMB.GT.WT) GOTO 222
      PAR1=  X
      PAR2=  FN
      PAR3=  WT
C
      ELSE IF(MODE.EQ.1) THEN
C     =======================
C FINAL STATISTICS
C STJ 24.OCT.89
      CINTEG=0D0
      ERRINT=0D0
      IF(NEVS.GT.0) CINTEG=ZSUM*SWT/FLOAT(NEVS)
      IF(NEVS.GT.0) ERRINT=SQRT(SSWT/SWT**2-1.D0/FLOAT(NEVS))
      PAR1=  CINTEG
      PAR2=  ERRINT
      PAR3=  ZSUM
C--
      ELSE
C     ====
      GOTO  902
      ENDIF
C     =====
C
      RETURN
 901  WRITE(NOUT,9010)
 9010 FORMAT(' **** STOP IN VESK8A, LACK OF INITIALISATION')
      STOP
 902  WRITE(NOUT,9020)
 9020 FORMAT(' **** STOP IN VESK8A, WRONG MODE ')
      STOP
 999  WRITE(NOUT,9990)
 9990 FORMAT(' **** STOP IN VESK8A, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END
      SUBROUTINE DESK1W(FUNSKO,JDIV)
C     ******************************
C THIS ROUTINE BELONGS TO VESK8A PACKAGE
C IT SUDIVIDES INTO TWO EQUAL PARTS THE INTERVAL
C (XX(JDIV),XX(JDIV+1))  IN THE LATICE
C     ***********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save
      COMMON / CESK1W / XX(1025),YY(1025),ZINT(1025),ZSUM,JMAX
      COMMON / INOUT  / NINP,NOUT
      EXTERNAL FUNSKO
C
      XNEW=.5D0*(XX(JDIV) +XX(JDIV+1))
      DO 100 J=JMAX,JDIV,-1
      XX(J+2)  =XX(J+1)
      YY(J+2)  =YY(J+1)
  100 ZINT(J+1)=ZINT(J)
      XX(JDIV+1)= XNEW
      YY(JDIV+1)= FUNSKO(XNEW)
      IF(YY(JDIV+1).LT.0.) GOTO 999
      ZINT(JDIV)  =.5D0*(YY(JDIV+1)+YY(JDIV)  )*(XX(JDIV+1)-XX(JDIV)  )
      ZINT(JDIV+1)=.5D0*(YY(JDIV+2)+YY(JDIV+1))*(XX(JDIV+2)-XX(JDIV+1))
      JMAX=JMAX+1
      RETURN
  999 WRITE(NOUT,9000)
 9000 FORMAT(' **** STOP IN DESK1W, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END


      SUBROUTINE VESK2W(MODE,FUNSKO,X,Y,WT)
C     *************************************
C=======================================================================
C=======================================================================
C=======================================================================
C===============TWO DIMENSIONAL SAMPLER VESK2W==========================
C=======================================================================
C=======================================================================
C=======================================================================
C                         VESK2W                                       C
C  GENERAL PURPOSE ROUTINE TO GENERATE AN ARBITRARY TWO DIMENSIONAL    C
C  DISTRIBUTION SUPPLIED BY USER IN A FORM OF FUNCTION FUNSKO(X,Y)     C
C                 WRITTEN NOVEMBER 1985                                C
C                    BY S. JADACH                                      C
C                 LAST UPDATE:  07.NOV.1990                            C
C                 version with weighted event....                      C
C======================================================================C
C VESKO2 GENERATES TWO DIMENSIONAL DISTRIBUTION DEFINED BY ARBITRARY
C FUNCTION FUNSKO(X,Y) WHERE X,Y BELONG  TO (0,1) RANGE.
C THE METHOD CONSISTS IN DIVIDING UNIT PLAQUET INTO CELLS USING
C SORT OF 'LIFE-GAME' METHOD IN WHICH THE DIVISION OF A CELLS IS MADE
C (DURING INITIALISATION) ALWAYS FOR THIS CELL WHICH CONTAINS
C A MAXIMUM VALUE OF THE INTEGRAL OVER FUNSKO IN THE CELL.
C RESULTING CELLS CONTAIN (USUALLY UP TO FACTOR TWO) EQUAL INTERGRAL
C VALUE. THE GENERATION CONSISTS IN CHOOSING RANDOMLY  A CELL
C ACCORDING TO ITS CONTENT AND THEN IN GENERATING X,Y WITHIN THE CELL.
C REJECTION METHOD IS APPLIED AT THE END OF THE PROCEDURE IN ORDER TO
C ASSURE THAT X,Y ARE DISTRIBUTED PRECISELY ACCORDING TO FUNSKO(X,Y)
C                    PARAMETERS
C -/ MODE = -1 INITIALISATION, NO (X,Y) GENERATED, CALL VESKO2(-1,D1,D2)
C    HAS TO BE MADE PRIOR  TO GENERATING FIRST (X,Y) PAIR
C -/ MODE =  0 GENERATION OF (X,Y) PAIR BY CALL VESKO2(0,X,Y)
C -/ MODE =  1 CALL VESKO2(1,VALINT,ERRINT) MAY BE DONE AFTER LAST
C    (X,Y) WAS GENERATED IN ORDER TO OBTAIN THE VALUE OF THE INTEGRAL
C    VALINT AND ITS ERROR ERRINT, INTEGRAL IS CALCULATED USING AVERAGE
C    WEIGHTS ENCOUTERED DURING GENERATION PHASE
C -/ X,Y  IF MODE=-1 THE THEY ARE DUMMY
C         IF MODE= 0 THE RESULT OF RANDOM GENERATION ACCORDING TO
C                    FUNCTION FUNSKO, X AND Y BELONG TO (0,1)
C         IF MODE= 1 X= VALUE OF INTEGRAL AND Y=ERROR (RELATIVE)
C                    WT = crude x-section
C ------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      save
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      DOUBLE PRECISION DRVEC(100)
      EXTERNAL FUNSKO
      DATA IWARM/77/

      IF(MODE) 100,200,300
C...  INITIALISATION PART, SEE VINSKO FOR MORE COMMENTS
  100 CALL VINSKW(FUNSKO)
      IWARM=0
      WT=0D0
      WTMAX = 1D0
      WTMXX = WTMAX
      NEVOV=0
      SWT=0D0
      SSWT=0D0
      NEVS=0
C(((((((((((((
C     CALL HBOOK1(1, 16H WT-VESKO2     $,75,0.0D0,1.5D0)
C     CALL HMINIM(1,0)
C     CALL HBOOK2(2,16H X-Y VESKO2    $, 64,0,1, 32,0,1,0)
C     CALL HSCALE(2)
C))))))))))))
      RETURN
C...
  200 CONTINUE
C...  GENERATION PART
      IF(IWARM.EQ.77) GO TO 980
cc    IF(WT.GT.WTMAX) THEN
cc      write(6,*) ' vesko2: ev. overweighted, dont worry, wt=',wt
cc      WT=WT-WTMAX
cc      NEVOV=NEVOV+1
cc    ELSE
        CALL VARRAN(DRVEC,3)
        R = DRVEC(1)
        DO 215 J=1,JMAX
        JSTOP=J
  215   IF(ZINT(J).GT.R) GOTO 216
  216   CONTINUE
        XR=XX(JSTOP,1)+DX(JSTOP,1)*DRVEC(2)
        YR=XX(JSTOP,2)+DX(JSTOP,2)*DRVEC(3)
        FN=FUNSKO(XR,YR)
        IF(FN.LT.0.) GOTO 999
        YYMAX=YYMX(JSTOP)
        WT=FN/YYMAX
        WTMXX = MAX(WTMXX,WT)
cc      IF(NEVS.LE.(4*JLIM2).AND.WT.GT.WTMAX) THEN
cc         WTMAX=WT*1.1D0
cc         WRITE(6,*) ' VESKO2: NEVS, new WTMAX= ',NEVS,WTMAX
cc      ENDIF
        NEVS=NEVS+1
        SWT=SWT+WT
        SSWT=SSWT+WT*WT
C((((((((((
C       CALL HFILL(1,WT,0D0,1D0)
C))))))))))
ccc   ENDIF
CCC    CALL VARRAN(DRVEC,1)
ccc    RN=DRVEC(1)
ccc   IF(WTMAX*RN.GT.WT) GOTO 200
      X=XR
      Y=YR
C((((((((((
C     CALL HFILL(2,XR,YR)
C))))))))))
      RETURN
C...
  300 CONTINUE
C THIS IS THE VALUE OF THE INTEGRAL
      CINTEG=ZSUM*SWT/NEVS
C AND ITS ERROR
      ERRINT=SQRT(SSWT/SWT**2-1D0/NEVS)
      X=CINTEG
      Y=ERRINT
      WT=ZSUM
C((((((((((
C     CALL HPRINT(1)
C     CALL HDELET(1)
C     CALL HPRINT(2)
C     CALL HDELET(2)
      PRINT 7000,NEVS,NEVOV,WTMAX,WTMXX
 7000 FORMAT(' VESK2W: NEVS,NEVOV,WTMAX,WTMXX= ',2I7,2F7.3)
C))))))))))
      RETURN
  980 WRITE(NOUT,9002)
 9002 FORMAT(' **** STOP IN VESK2W, LACK OF INITIALISATION   ')
      STOP
  999 WRITE(NOUT,9004)
 9004 FORMAT(' **** STOP IN VESK2W, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END

      SUBROUTINE VINSKW(FUNSKO)
C     *************************
C THIS ROUTINE BELONGS TO VESKO2 PACKAGE
C JLIM1 IS THE NUMBER OF CELLS, DIVISION OF THE UNIT PLAQUE INTO CELLS
C IS MADE IN THE FIRST STAGE.    JLIM2 IS THE TOTAL MAXIMUM
C NUMBER OF CELLS, NOTE THAT DIMENSIONS OF
C MATRICES IN /VESKOA/ SHOULD BE AT LEAST JLIM2
C     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      save
C ------------------------------------------------------------
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      EXTERNAL FUNSKO

C...  INITIALISATION PART, SAMPLING DISTRIBUTION FUNSKO
C...  AND FILLING MATRICES XX,YY,ZINT ETC.
      JMAX=1
      XX(1,1)=0D0
      XX(1,2)=0D0
      DX(1,1)=1D0
      DX(1,2)=1D0
      LEV(1)=1
      SUM=0D0
      DO 150 I=1,2
      DO 150 K=1,2
C... THIS IS NOT ELEGANT BUT SIMPLE
      YY(1,I,K)=FUNSKO(XX(1,1)+(I-1.)*DX(1,1),XX(1,2)+(K-1.)*DX(1,2))
      IF(YY(1,I,K).LT.0.0) GO TO 999
  150 SUM=SUM+YY(1,I,K)
      ZINT(1)=SUM*DX(1,1)*DX(1,2)/4D0

      JDIV=1
      DO 200 KK=1,JLIM2-1
      IF(JMAX.LT.JLIM1) THEN
C...    NOTE THAT DIVSKW INCREMENTS JMAX=JMAX+1 IN EVERY CALL
        CALL DIVSKW(JDIV,FUNSKO)
C(((((((((((
c      IF(JMAX.EQ.JLIM1) THEN
c      PRINT 9900,JMAX,(LEV(I),I=1,JMAX)
c 9900 FORMAT(///,' JMAX...  LEV LEV LEV LEV LEV',I10,/(24I5))
c      PRINT 9901,((XX(JD,I),I=1,2),JD=1,JMAX)
c 9901 FORMAT('  XX XX XX XX XX XX XX  ',/(10E12.5))
c      PRINT 9902,((DX(JD,I),I=1,2),JD=1,JMAX)
c 9902 FORMAT('  DX  DX DX DX DX DX ',/(10E12.5))
c      PRINT 9903,(((YY(JD,I,K),I=1,2),K=1,2),JD=1,JMAX)
c 9903 FORMAT('  YY  YY YY YY YY YY ',/(8E15.5))
c      PRINT 9904,(ZINT(I),I=1,JMAX)
c 9904 FORMAT('   ZINT ZINT ZINT ZINT ',/(10E12.5))
c      ENDIF
C))))))))))))
        JDIV=JDIV+2
        IF(JDIV.GT.JMAX) JDIV=1
      ELSE
        JDIV=1
        ZMX=ZINT(1)
        DO 180 J=1,JMAX
        IF(ZMX.LT.ZINT(J)) THEN
          ZMX=ZINT(J)
          JDIV=J
        ENDIF
  180   CONTINUE
        CALL DIVSKW(JDIV,FUNSKO)
      ENDIF
  200 CONTINUE

C(((((((((((
c      JPRN=64
c      PRINT 9910,JMAX,(LEV(I),I=1,JMAX)
c 9910 FORMAT(/,' JMAX...  LEV LEV LEV LEV LEV',I10,/(24I5))
c      IF(JMAX.LE.JPRN) PRINT 9911,((XX(JD,I),I=1,2),JD=1,JMAX)
c 9911 FORMAT('  XX XX XX XX XX XX XX  ',/(10E12.5))
c      IF(JMAX.LE.JPRN) PRINT 9912,((DX(JD,I),I=1,2),JD=1,JMAX)
c 9912 FORMAT('  DX  DX DX DX DX DX ',/(10E12.5))
c      IF(JMAX.LE.JPRN) PRINT 9913,(((YY(JD,I,K),I=1,2),K=1,2),JD=1,JMAX)
c 9913 FORMAT('  YY  YY YY YY YY YY ',/(8E15.5))
c      IF(JMAX.LE.JPRN) PRINT 9914,(ZINT(I),I=1,JMAX)
c 9914 FORMAT('   ZINT ZINT ZINT ZINT ',/(10E12.5))
C     DO 902 J=1,JMAX
C     Z=1D0*J-.5D0
C 902 CALL HFILL(202,Z,ZINT(J))
C))))))))))))
C...  FINAL ADMINISTRATION, NORMALIZING ZINT ETC.
      ZSUM1=0D0
      ZSUM =0D0
      DO 260 J=1,JMAX
      ZSUM1=ZSUM1+ZINT(J)
      YMAX= 0D0
      DO 250 I=1,2
      DO 250 K=1,2
  250 YMAX= MAX(YMAX,YY(J,I,K))
      YYMX(J)=YMAX
      ZINT(J)=YMAX*DX(J,1)*DX(J,2)
  260 ZSUM=ZSUM+ZINT(J)
C((((((((
      ZR=ZSUM1/ZSUM
      PRINT 7000,ZR
 7000 FORMAT(' /////// ZSUM1/ZSUM= ',F20.8)
C)))))))))
      SUM=0D0
      DO 240 J=1,JMAX
      SUM=SUM+ZINT(J)
  240 ZINT(J)=SUM/ZSUM
C(((((((((((
c     JPRN=64
c     PRINT 9932,JMAX
c9932 FORMAT(/'=====JMAX ZINT ZINT ZINT  ',I10)
c     IF(JMAX.LE.JPRN) PRINT 9935,(ZINT(I),I=1,JMAX)
c9935            FORMAT(10E12.5)
C     DO 901 J=2,JMAX
C 901 CALL HFILL(201,(ZINT(J)-ZINT(J-1))*JMAX)
C     CALL HFILL(201,ZINT(1)*JMAX)
C))))))))))))
      RETURN
  999 WRITE(NOUT,9000)
 9000 FORMAT(' **** STOP IN VINSKW, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END

      SUBROUTINE DIVSKW(JD,FUNSKO)
C     ****************************
C THIS ROUTINE BELONGS TO VESKO2 PACKAGE
C IT SUBDIVIDES ONE CELL (NO. JD) INTO TWO EQUAL SIZE CELLS
C     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      save
C ------------------------------------------------------------
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      EXTERNAL FUNSKO

C...  MOOVE TO MAKE A HOLE FOR A NEW ENTRY (ONE ADDITIONAL CELL)
      DO 100 J=JMAX,JD,-1
      ZINT(J+1)=ZINT(J)
      LEV(J+1)=LEV(J)
      DO 100 I=1,2
      XX(J+1,I)  =XX(J,I)
      DX(J+1,I)  =DX(J,I)
      DO 100 K=1,2
  100 YY(J+1,I,K)  =YY(J,I,K)
C...  CREATE TWO NEW CELLS AND STORE THEM
      LL= MOD(LEV(JD),2)+1
      DX(JD,LL)=DX(JD,LL)/2D0
      DX(JD+1,LL)=DX(JD+1,LL)/2D0
      XX(JD+1,LL)=XX(JD,LL)+DX(JD,LL)
      IF(LL.EQ.1) THEN
        DO 150 I=1,2
C... THIS IS NOT ELEGANT, PROBABLY COULD BE DONE BETTER
        YY(JD,2,I)=FUNSKO(XX(JD,1)+DX(JD,1),XX(JD,2)+(I-1.)*DX(JD,2))
  150   YY(JD+1,1,I)=YY(JD,2,I)
      ELSE
        DO 152 I=1,2
        YY(JD,I,2)=FUNSKO(XX(JD,1)+(I-1.)*DX(JD,1),XX(JD,2)+DX(JD,2))
  152   YY(JD+1,I,1)=YY(JD,I,2)
      ENDIF
C...  ESTIMATE THE INTEGRALS OVER NEW CELLS RESULTING FROM DIVISION
      DO 220 JDV=JD,JD+1
      LEV(JDV)=LEV(JDV)+1
      SUM=0D0
      DO 210 I=1,2
      DO 210 K=1,2
      IF(YY(JDV,I,K).LT.0.D0) GO TO 999
  210 SUM=SUM+YY(JDV,I,K)
  220 ZINT(JDV) =SUM*DX(JDV,1)*DX(JDV,2)/4D0
      JMAX=JMAX+1
      RETURN
  999 WRITE(NOUT,9000)
 9000 FORMAT(' **** STOP IN DIVSKW, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END


      SUBROUTINE GAUSJD(FUN,AA,BB,EEPS,RESULT)
C     ****************************************
C Gauss integration by S. Jadach, Oct. 90.
C This is NON-ADAPTIVE (!!!!) UNOPTIMIZED (!!!) integration subprogram.
C     *************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION WG(12),XX(12)
      COMMON / INOUT  / NINP,NOUT
      EXTERNAL FUN
      save /inout/,wg,xx,ITERMX
      DATA WG
     $/0.101228536290376D0, 0.222381034453374D0, 0.313706645877887D0,
     $ 0.362683783378362D0, 0.027152459411754D0, 0.062253523938648D0,
     $ 0.095158511682493D0, 0.124628971255534D0, 0.149595988816577D0,
     $ 0.169156519395003D0, 0.182603415044924D0, 0.189450610455069D0/
      DATA XX
     $/0.960289856497536D0, 0.796666477413627D0, 0.525532409916329D0,
     $ 0.183434642495650D0, 0.989400934991650D0, 0.944575023073233D0,
     $ 0.865631202387832D0, 0.755404408355003D0, 0.617876244402644D0,
     $ 0.458016777657227D0, 0.281603550779259D0, 0.095012509837637D0/
      DATA ITERMX / 15/
      EPS=ABS(EEPS)
      A=AA
      B=BB
      NDIVI=1
C iteration over subdivisions terminated by precision requirement
      DO 400 ITER=1,ITERMX
      CALK8  =0D0
      CALK16 =0D0
C sum over DELTA subintegrals
      DO 200 K = 1,NDIVI
      DELTA = (B-A)/NDIVI
      X1    =  A + (K-1)*DELTA
      X2    =  X1+ DELTA
      XMIDLE= 0.5D0*(X2+X1)
      RANGE = 0.5D0*(X2-X1)
      SUM8 =0D0
      SUM16=0D0
C 8- and 12-point   Gauss integration over single DELTA subinterval
      DO 100 I=1,12
      XPLUS= XMIDLE+RANGE*XX(I)
      XMINU= XMIDLE-RANGE*XX(I)
      FPLUS=FUN(XPLUS)
      FMINU=FUN(XMINU)
      IF(I.LE.4) THEN
          SUM8 =SUM8  +(FPLUS+FMINU)*WG(I)/2D0
      ELSE
          SUM16=SUM16 +(FPLUS+FMINU)*WG(I)/2D0
      ENDIF
  100 CONTINUE
      CALK8 = CALK8 + SUM8 *(X2-X1)
      CALK16= CALK16+ SUM16*(X2-X1)
  200 CONTINUE
      ERABS = ABS(CALK16-CALK8)
      ERELA = 0D0
      IF(CALK16.NE.0D0) ERELA= ERABS/ABS(CALK16)
c     write(6,*) 'gausjd: CALK8,CALK16=',ITER,CALK8,CALK16,ERELA
C precision check to terminate integration
      IF(EEPS.GT.0D0) THEN
        IF(ERABS.LT. EPS) GOTO 800
      ELSE
        IF(ERELA.LT. EPS) GOTO 800
      ENDIF
  400 NDIVI=NDIVI*2
      WRITE(NOUT,*) ' +++++ GAUSJD:  REQUIRED PRECISION TO HIGH!'
      WRITE(NOUT,*) ' +++++ GAUSJD:  ITER,ERELA=',ITER,ERELA
  800 RESULT= CALK16
      END


      SUBROUTINE WMONIT(MODE,ID,WT,WTMAX,RN)
C     **************************************
C last correction 19 sept. 89
C Utility program for monitoring M.C. rejection weights.
C ID is weight idendifier, maximum IDMX (defined below).
C WT IS WEIGHT, WTMAX IS MAXIMUM WEIGHT AND RN IS RANDOM NUMBER.
C IF(MODE.EQ.-1) THEN
C          INITALIZATION IF ENTRY ID, OTHER ARGUMENTS ARE IGNORED
C ELSEIF(MODE.EQ.0) THEN
C          SUMMING UP WEIGHTS ETC. FOR A GIVEN EVENT FOR ENTRY ID
C        - WT IS CURRENT WEIGHT.
C        - WTMAX IS MAXIMUM WEIGHT USED FOR COUTING OVERWEIGHTED
C          EVENTS WITH WT>WTMAX.
C        - RN IS RANDOM NUMBER USED IN REJECTION, IT IS USED TO
C          COUNT NO. OF ACCEPTED (RN<WT/WTMAX) AND REJECTED
C          (WT>WT/WTMAX) EVENTS,
C          IF RO REJECTION THEN PUT RN=0D0.
C ELSEIF(MODE.EQ.1) THEN
C          IN THIS MODE WMONIT REPPORTS ON ACCUMULATED STATISTICS
C          AND THE INFORMATION IS STORED IN COMMON /CMONIT/
C        - AVERWT= AVERAGE WEIGHT WT COUNTING ALL EVENT
C        - ERRELA= RELATIVE ERROR OF AVERWT
C        - NEVTOT= TOTAL NIMBER OF ACCOUNTED EVENTS
C        - NEVACC= NO. OF ACCEPTED EVENTS (RN<WT\WTMAX)
C        - NEVNEG= NO. OF EVENTS WITH NEGATIVE WEIGHT (WT<0)
C        - NEVZER= NO. OF EVENTS WITH ZERO WEIGHT (WT.EQ.0D0)
C        - NEVOVE= NO. OF OVERWEGHTED EVENTS (WT>WTMAX)
C          AND IF YOU DO NOT WANT TO USE CMONIT THEN THE VALUE
C          The value of AVERWT is assigned to WT,
C          the value of ERRELA is assigned to WTMAX and
C          the value of WTMAX  is assigned to RN in this mode.
C ELSEIF(MODEE.EQ.2) THEN
C          ALL INFORMATION DEFINED FOR ENTRY ID DEFINED ABOVE
C          FOR MODE=2 IS JUST PRINTED OF UNIT NOUT
C ENDIF
C NOTE THAT OUTPUT REPPORT (MODE=1,2) IS DONE DYNAMICALLY JUST FOR A
C GIVEN ENTRY ID ONLY AND IT MAY BE REPEATED MANY TIMES FOR ONE ID AND
C FOR VARIOUS ID'S AS WELL.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save
      PARAMETER(IDMX=100)
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / INOUT  / NINP,NOUT
      INTEGER NTOT(IDMX),NACC(IDMX),NNEG(IDMX),NOVE(IDMX),NZER(IDMX)
      DIMENSION SWT(IDMX),SSWT(IDMX),WWMX(IDMX)
      DATA NTOT /IDMX* -1/  SWT /IDMX*   0D0/
      DATA SSWT /IDMX*0D0/ WWMX /IDMX*-1D-20/
C
      IF(ID.LE.0.OR.ID.GT.IDMX) THEN
           WRITE(NOUT,*) ' =====WMONIT: WRONG ID',ID
           STOP
      ENDIF
      IF(MODE.EQ.-1) THEN
           NTOT(ID)=0
           NACC(ID)=0
           NNEG(ID)=0
           NZER(ID)=0
           NOVE(ID)=0
           SWT(ID)   =0D0
           SSWT(ID)  =0D0
           WWMX(ID)  = -1D-20
      ELSEIF(MODE.EQ.0) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           NTOT(ID)=NTOT(ID)+1
           SWT(ID)=SWT(ID)+WT
           SSWT(ID)=SSWT(ID)+WT**2
           WWMX(ID)= MAX(WWMX(ID),WT)
           IF(WT.EQ.0D0)   NZER(ID)=NZER(ID)+1
           IF(WT.LT.0D0)   NNEG(ID)=NNEG(ID)+1
           IF(WT.GT.WTMAX)      NOVE(ID)=NOVE(ID)+1
           IF(RN*WTMAX.LE.WT)   NACC(ID)=NACC(ID)+1
      ELSEIF(MODE.EQ.1) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
           ENDIF
           NEVTOT=NTOT(ID)
           NEVACC=NACC(ID)
           NEVNEG=NNEG(ID)
           NEVZER=NZER(ID)
           NEVOVE=NOVE(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSEIF(MODE.EQ.2) THEN
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
              WWMAX=WWMX(ID)
           ENDIF
           WRITE(NOUT,1003) ID, AVERWT, ERRELA, WWMAX
           WRITE(NOUT,1004) NTOT(ID),NACC(ID),NNEG(ID),NOVE(ID),NZER(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSE
           WRITE(NOUT,*) ' =====WMONIT: WRONG MODE',MODE
           STOP
      ENDIF
 1003 FORMAT(
     $  ' =======================WMONIT========================'
     $/,'   ID           AVERWT         ERRELA            WWMAX'
     $/,    I5,           E17.7,         F15.9,           E17.7)
 1004 FORMAT(
     $  ' -----------------------------------------------------------'
     $/,'      NEVTOT      NEVACC      NEVNEG      NEVOVE      NEVZER'
     $/,   5I12)
      END

      SUBROUTINE WMONI2(MODE,ID,WT,WTMAX,RN)
C     **************************************
C -------------- SECOND COPY OF WMONIT ----------------
C last correction 19 sept. 89
C Utility program for monitoring M.C. rejection weights.
C ID is weight idendifier, maximum IDMX (defined below).
C WT IS WEIGHT, WTMAX IS MAXIMUM WEIGHT AND RN IS RANDOM NUMBER.
C IF(MODE.EQ.-1) THEN
C          INITALIZATION IF ENTRY ID, OTHER ARGUMENTS ARE IGNORED
C ELSEIF(MODE.EQ.0) THEN
C          SUMMING UP WEIGHTS ETC. FOR A GIVEN EVENT FOR ENTRY ID
C        - WT IS CURRENT WEIGHT.
C        - WTMAX IS MAXIMUM WEIGHT USED FOR COUTING OVERWEIGHTED
C          EVENTS WITH WT>WTMAX.
C        - RN IS RANDOM NUMBER USED IN REJECTION, IT IS USED TO
C          COUNT NO. OF ACCEPTED (RN<WT/WTMAX) AND REJECTED
C          (WT>WT/WTMAX) EVENTS,
C          IF RO REJECTION THEN PUT RN=0D0.
C ELSEIF(MODE.EQ.1) THEN
C          IN THIS MODE WMONIT REPPORTS ON ACCUMULATED STATISTICS
C          AND THE INFORMATION IS STORED IN COMMON /CMONIT/
C        - AVERWT= AVERAGE WEIGHT WT COUNTING ALL EVENT
C        - ERRELA= RELATIVE ERROR OF AVERWT
C        - NEVTOT= TOTAL NIMBER OF ACCOUNTED EVENTS
C        - NEVACC= NO. OF ACCEPTED EVENTS (RN<WT\WTMAX)
C        - NEVNEG= NO. OF EVENTS WITH NEGATIVE WEIGHT (WT<0)
C        - NEVZER= NO. OF EVENTS WITH ZERO WEIGHT (WT.EQ.0D0)
C        - NEVOVE= NO. OF OVERWEGHTED EVENTS (WT>WTMAX)
C          AND IF YOU DO NOT WANT TO USE CMONIT THEN THE VALUE
C          The value of AVERWT is assigned to WT,
C          the value of ERRELA is assigned to WTMAX and
C          the value of WTMAX  is assigned to RN in this mode.
C ELSEIF(MODEE.EQ.2) THEN
C          ALL INFORMATION DEFINED FOR ENTRY ID DEFINED ABOVE
C          FOR MODE=2 IS JUST PRINTED OF UNIT NOUT
C ENDIF
C NOTE THAT OUTPUT REPPORT (MODE=1,2) IS DONE DYNAMICALLY JUST FOR A
C GIVEN ENTRY ID ONLY AND IT MAY BE REPEATED MANY TIMES FOR ONE ID AND
C FOR VARIOUS ID'S AS WELL.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save
      PARAMETER(IDMX=100)
      COMMON / CMONI2/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / INOUT  / NINP,NOUT
      INTEGER NTOT(IDMX),NACC(IDMX),NNEG(IDMX),NOVE(IDMX),NZER(IDMX)
      DIMENSION SWT(IDMX),SSWT(IDMX),WWMX(IDMX)
      DATA NTOT /IDMX* -1/  SWT /IDMX*   0D0/
      DATA SSWT /IDMX*0D0/ WWMX /IDMX*-1D-20/
C
      IF(ID.LE.0.OR.ID.GT.IDMX) THEN
           WRITE(NOUT,*) ' =====WMONI2: WRONG ID',ID
           STOP
      ENDIF
      IF(MODE.EQ.-1) THEN
           NTOT(ID)=0
           NACC(ID)=0
           NNEG(ID)=0
           NZER(ID)=0
           NOVE(ID)=0
           SWT(ID)   =0D0
           SSWT(ID)  =0D0
           WWMX(ID)  = -1D-20
      ELSEIF(MODE.EQ.0) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           NTOT(ID)=NTOT(ID)+1
           SWT(ID)=SWT(ID)+WT
           SSWT(ID)=SSWT(ID)+WT**2
           WWMX(ID)= MAX(WWMX(ID),WT)
           IF(WT.EQ.0D0)   NZER(ID)=NZER(ID)+1
           IF(WT.LT.0D0)   NNEG(ID)=NNEG(ID)+1
           IF(WT.GT.WTMAX)      NOVE(ID)=NOVE(ID)+1
           IF(RN*WTMAX.LE.WT)   NACC(ID)=NACC(ID)+1
      ELSEIF(MODE.EQ.1) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONI2: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
           ENDIF
           NEVTOT=NTOT(ID)
           NEVACC=NACC(ID)
           NEVNEG=NNEG(ID)
           NEVZER=NZER(ID)
           NEVOVE=NOVE(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSEIF(MODE.EQ.2) THEN
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
              WWMAX=WWMX(ID)
           ENDIF
           WRITE(NOUT,1003) ID, AVERWT, ERRELA, WWMAX
           WRITE(NOUT,1004) NTOT(ID),NACC(ID),NNEG(ID),NOVE(ID),NZER(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSE
           WRITE(NOUT,*) ' =====WMONI2: WRONG MODE',MODE
           STOP
      ENDIF
 1003 FORMAT(
     $  ' =======================WMONI2========================'
     $/,'   ID           AVERWT         ERRELA            WWMAX'
     $/,    I5,           E17.7,         F15.9,           E17.7)
 1004 FORMAT(
     $  ' -----------------------------------------------------------'
     $/,'      NEVTOT      NEVACC      NEVNEG      NEVOVE      NEVZER'
     $/,   5I12)
      END

      FUNCTION GAUS(F,A,B,EEPS)
C     *************************
C THIS IS ITERATIVE INTEGRATION PROCEDURE
C ORIGINATES  PROBABLY FROM CERN LIBRARY
C IT SUBDIVIDES INEGRATION RANGE UNTIL REQUIRED PRECISION IS REACHED
C PRECISION IS A DIFFERENCE FROM 8 AND 16 POINT GAUSS ITEGR. RESULT
C EEPS POSITIVE TREATED AS ABSOLUTE PRECISION
C EEPS NEGATIVE TREATED AS RELATIVE PRECISION
C     *************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION W(12),X(12)
      COMMON / INOUT  / NINP,NOUT
      EXTERNAL F
      DATA CONST /1.0D-19/
      save     / INOUT/, CONST, W, X
      DATA W
     1/0.10122 85362 90376, 0.22238 10344 53374, 0.31370 66458 77887,
     2 0.36268 37833 78362, 0.02715 24594 11754, 0.06225 35239 38648,
     3 0.09515 85116 82493, 0.12462 89712 55534, 0.14959 59888 16577,
     4 0.16915 65193 95003, 0.18260 34150 44924, 0.18945 06104 55069/
      DATA X
     1/0.96028 98564 97536, 0.79666 64774 13627, 0.52553 24099 16329,
     2 0.18343 46424 95650, 0.98940 09349 91650, 0.94457 50230 73233,
     3 0.86563 12023 87832, 0.75540 44083 55003, 0.61787 62444 02644,
     4 0.45801 67776 57227, 0.28160 35507 79259, 0.09501 25098 37637/
      EPS=ABS(EEPS)
      DELTA=CONST*ABS(A-B)
      GAUS=0D0
      AA=A
    5 Y=B-AA
      IF(ABS(Y) .LE. DELTA) RETURN
    2 BB=AA+Y
      C1=0.5D0*(AA+BB)
      C2=C1-AA
      S8=0D0
      S16=0D0
      DO 1 I=1,4
      U=X(I)*C2
    1 S8=S8+W(I)*(F(C1+U)+F(C1-U))
      DO 3 I=5,12
      U=X(I)*C2
    3 S16=S16+W(I)*(F(C1+U)+F(C1-U))
      S8=S8*C2
      S16=S16*C2
      IF(EEPS.LT.0D0) THEN
        IF(ABS(S16-S8) .GT. EPS*ABS(S16)) GO TO 4
      ELSE
        IF(ABS(S16-S8) .GT. EPS) GO TO 4
      ENDIF
      GAUS=GAUS+S16
      AA=BB
      GO TO 5
    4 Y=0.5D0*Y
      IF(ABS(Y) .GT. DELTA) GOTO 2
      WRITE(NOUT,7)
      GAUS=0D0
      RETURN
    7 FORMAT(1X,36HGAUS  ... TOO HIGH ACCURACY REQUIRED)
      END


      DOUBLE PRECISION FUNCTION DILOGY(X)
C-------------------------------------------- REMARKS ---------------
C DILOGARITHM FUNCTION: DILOG(X)=INT( -LN(1-Z)/Z ) , 0 < Z < X .
C THIS IS THE CERNLIB VERSION.
C--------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Z=-1.644934066848226D0
      IF(X .LT.-1.D0) GO TO 1
      IF(X .LE. 0.5D0) GO TO 2
      IF(X .EQ. 1.D0) GO TO 3
      IF(X .LE. 2.D0) GO TO 4
      Z=3.289868133696453D0
    1 T=1.D0/X
      S=-0.5D0
      Z=Z-0.5D0*DLOG(DABS(X))**2
      GO TO 5
    2 T=X
      S=0.5D0
      Z=0.D0
      GO TO 5
    3 DILOGY=1.644934066848226D0
      RETURN
    4 T=1.D0-X
      S=-0.5D0
      Z=1.644934066848226D0-DLOG(X)*DLOG(DABS(T))
    5 Y=2.666666666666667D0*T+0.666666666666667D0
      B=      0.000000000000001D0
      A=Y*B  +0.000000000000004D0
      B=Y*A-B+0.000000000000011D0
      A=Y*B-A+0.000000000000037D0
      B=Y*A-B+0.000000000000121D0
      A=Y*B-A+0.000000000000398D0
      B=Y*A-B+0.000000000001312D0
      A=Y*B-A+0.000000000004342D0
      B=Y*A-B+0.000000000014437D0
      A=Y*B-A+0.000000000048274D0
      B=Y*A-B+0.000000000162421D0
      A=Y*B-A+0.000000000550291D0
      B=Y*A-B+0.000000001879117D0
      A=Y*B-A+0.000000006474338D0
      B=Y*A-B+0.000000022536705D0
      A=Y*B-A+0.000000079387055D0
      B=Y*A-B+0.000000283575385D0
      A=Y*B-A+0.000001029904264D0
      B=Y*A-B+0.000003816329463D0
      A=Y*B-A+0.000014496300557D0
      B=Y*A-B+0.000056817822718D0
      A=Y*B-A+0.000232002196094D0
      B=Y*A-B+0.001001627496164D0
      A=Y*B-A+0.004686361959447D0
      B=Y*A-B+0.024879322924228D0
      A=Y*B-A+0.166073032927855D0
      A=Y*A-B+1.935064300869969D0
      DILOGY=S*T*(A-B)+Z
      END


      DOUBLE PRECISION FUNCTION DPGAMM(Z)
C     **********************************
C Double precision Gamma function
      DOUBLE PRECISION Z,Z1,X,X1,X2,D1,D2,S1,S2,S3,PI,C(20),CONST
      save C,PI,CONST
      DATA C( 1) / 8.3333333333333333333333333332D-02/
      DATA C( 2) /-2.7777777777777777777777777777D-03/
      DATA C( 3) / 7.9365079365079365079365079364D-04/
      DATA C( 4) /-5.9523809523809523809523809523D-04/
      DATA C( 5) / 8.4175084175084175084175084175D-04/
      DATA C( 6) /-1.9175269175269175269175269175D-03/
      DATA C( 7) / 6.4102564102564102564102564102D-03/
      DATA C( 8) /-2.9550653594771241830065359477D-02/
      DATA C( 9) / 1.7964437236883057316493849001D-01/
      DATA C(10) /-1.3924322169059011164274322169D+00/
      DATA C(11) / 1.3402864044168391994478951001D+01/
      DATA C(12) /-1.5684828462600201730636513245D+02/
      DATA C(13) / 2.1931033333333333333333333333D+03/
      DATA C(14) /-3.6108771253724989357173265219D+04/
      DATA C(15) / 6.9147226885131306710839525077D+05/
      DATA C(16) /-1.5238221539407416192283364959D+07/
      DATA C(17) / 3.8290075139141414141414141414D+08/
      DATA C(18) /-1.0882266035784391089015149165D+10/
      DATA C(19) / 3.4732028376500225225225225224D+11/
      DATA C(20) /-1.2369602142269274454251710349D+13/
      DATA PI    / 3.1415926535897932384626433832D+00/
      DATA CONST / 9.1893853320467274178032973641D-01/
      IF(Z.GT.5.75D 1)                                     GOTO  6666
      NN = Z
      IF (Z  -  DBLE(FLOAT(NN)))                 3,1,3
    1 IF (Z    .LE.    0.D 0)                    GOTO 6667
      DPGAMM = 1.D 0
      IF (Z    .LE.    2.D 0)                    RETURN
      Z1 = Z
    2 Z1 = Z1  -  1.D 0
      DPGAMM = DPGAMM * Z1
      IF (Z1  -  2.D 0)                          61,61,2
    3 IF (DABS(Z)    .LT.    1.D-29)             GOTO 60
      IF (Z    .LT.    0.D 0)                    GOTO 4
      X  = Z
      KK = 1
      GOTO 10
    4 X  = 1.D 0  -  Z
      KK = 2
   10 X1 = X
      IF (X    .GT.    19.D 0)                   GOTO 13
      D1 = X
   11 X1 = X1  +  1.D 0
      IF (X1    .GE.    19.D 0)                  GOTO 12
      D1 = D1 * X1
      GOTO 11
   12 S3 = -DLOG(D1)
      GOTO 14
   13 S3 = 0.D 0
   14 D1 = X1 * X1
      S1 = (X1  -  5.D-1) * DLOG(X1)  -  X1  +  CONST
      DO 20                  K=1,20
      S2 = S1  +  C(K)/X1
      IF (DABS(S2  -  S1)    .LT.    1.D-28)     GOTO 21
      X1 = X1 * D1
   20 S1 = S2
   21 S3 = S3  +  S2
      GOTO (50,22),    KK
   22 D2 = DABS(Z  -  NN)
      D1 = D2 * PI
      IF (D1    .LT.    1.D-15)                  GOTO 31
   30 X2 =  DLOG(PI/DSIN(D1))  -  S3
      GOTO 40
   31 X2 = -DLOG(D2)
   40 MM = DABS(Z)
      IF(X2      .GT.      1.74D2)                         GO TO 6666
      DPGAMM = DEXP(X2)
      IF (MM    .NE.    (MM/2) * 2)              RETURN
      DPGAMM = -DPGAMM
      RETURN
   50 IF(S3      .GT.      1.74D2)                         GO TO 6666
      DPGAMM = DEXP(S3)
      RETURN
      DPGAMM = 0.D 0
 6666 PRINT *, 2000
      RETURN
 6667 PRINT *, 2001
      RETURN
   60 DPGAMM = 0.D 0
      IF(DABS(Z)   .LT.   1.D-77)   RETURN
      DPGAMM = 1.D 0/Z
   61 RETURN
 2000 FORMAT (/////, 2X, 32HDPGAMM ..... ARGUMENT TOO LARGE., /////)
 2001 FORMAT (/////, 2X, 32HDPGAMM ..... ARGUMENT IS A POLE., /////)
      END




C=======================================================================
C=======================================================================
C=======================================================================
C==Received: by dxmint.cern.ch (cernvax) (5.57/3.14)
C== id AA13405; Wed, 23 Jan 91 17:19:06 +0100
C==Message-Id: <9101231619.AA13405@dxmint.cern.ch>
C==Received: by cernapo; Wed, 23 Jan 91 17:23:40 +0100
C==Received: by apojames.cern.ch; Wed, 23 Jan 91 17:05:23 CET
C==Date: Wed, 23 Jan 91 17:05:23 CET
C==From: james@cernapo.cern.ch (Frederick James)
C==To: jadach@cernvm
C==Subject: Random generators
C==
C==      PROGRAM PSEUDORAN
C==C  CPC # ABTK                                           CPC # ABTK
C==C         Pseudorandom generator demonstration (test case)
C==      DIMENSION RVEC(1000)
C==      DIMENSION VERI(5), ISD25(25)
C==C
C==C
C==C   ................................................
C==      WRITE(6,'(20X,A)') 'DEMONSTRATION OF PSEUDORANDOM GENERATORS'
C==      WRITE(6,'(20X,A)') 'MACHINE/SYSTEM: date:'
C==      WRITE(6,'(/20X,A/)') 'INITIALIZATION AND TEST OF PORTABILITY'
C==C   ................................................
C==C
C==C                   initialization and verification  RANMAR
C==        DO 40 I9= 1, 20
C==   40   CALL RANMAR(RVEC,1000)
C==      CALL RANMAR(RVEC,5)
C==      DO 41 I= 1 ,5
C==   41 VERI(I) = (4096.*RVEC(I))*(4096.)
C==      WRITE(6,'(A,5F12.1/)') '  RANMAR 20001  ',VERI
C==C
C==C                   initialization and verification  RANECU
C==      CALL RANECU(RVEC,1000)
C==      CALL RANECU(VERI,5)
C==      DO 52 I= 1 ,5
C==   52 VERI(I) = 4096.*(4096.*VERI(I))
C==      WRITE(6,'(A,5F12.1/)') '  RANECU 1001   ',VERI
C==C
C==C                   initialization and verification  RCARRY
C==      CALL RCARRY(RVEC,1000)
C==      CALL RCARRY(VERI,5)
C==      DO 62 I= 1 ,5
C==   62 VERI(I) = 4096.*(4096.*VERI(I))
C==      WRITE(6,'(A,5F12.1/)') '  RCARRY 1001   ',VERI
C==C
C==      WRITE(6,'(//20X,A/)') 'TEST OF REPEATABILITY'
C==C  .................................................
C==C                  verify restarting      RANMAR
C==      WRITE(6,'(/A)') '   THE NEXT LINE SHOULD BE REPEATED:'
C==      CALL RMARUT(IMAR1,IMAR2,IMAR3)
C==      CALL RANMAR(RVEC,777)
C==      CALL RANMAR(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANMAR 1 ',VERI
C==      CALL RMARIN(IMAR1,IMAR2,IMAR3)
C==      CALL RANMAR(RVEC,777)
C==      CALL RANMAR(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANMAR 2 ',VERI
C==C
C==C                  verify restarting      RANECU
C==      WRITE(6,'(/A)') '   THE NEXT LINE SHOULD BE REPEATED:'
C==      CALL RECUUT(IS1,IS2)
C==      CALL RANECU(RVEC,777)
C==      CALL RANECU(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANECU 1 ',VERI
C==      CALL RECUIN(IS1,IS2)
C==      CALL RANECU(RVEC,777)
C==      CALL RANECU(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANECU 2 ',VERI
C==C
C==C                  verify restarting      RCARRY
C==      WRITE(6,'(/A)') '   THE NEXT LINE SHOULD BE REPEATED:'
C==      CALL RCARUT(ISD25)
C==      CALL RCARRY(RVEC,777)
C==      CALL RCARRY(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RCARRY 1 ',VERI
C==      CALL RCARIN(ISD25)
C==      CALL RCARRY(RVEC,777)
C==      CALL RCARRY(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RCARRY 2 ',VERI
C==C
C==      STOP
C==      END
C=======================================================================
      SUBROUTINE CARRAN(RVEC,LENV)
C         Add-and-carry random number generator proposed by
C         Marsaglia and Zaman in SIAM J. Scientific and Statistical
C             Computing, to appear probably 1990.
C         modified with enhanced initialization by F. James, 1990
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for CARRAN:                                  ++
C!!!      CALL CARRAN (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL CARINI(INT)     initializes the generator from one    ++
C!!!                   32-bit integer INT                            ++
C!!!      CALL CARRES(IVEC)    restarts the generator from vector    ++
C!!!                   IVEC of 25 32-bit integers (see CAROUT)       ++
C!!!      CALL CAROUT(IVEC)    outputs the current values of the 25  ++
C!!!                 32-bit integer seeds, to be used for restarting ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(LENV)
      DIMENSION SEEDS(24), ISEEDS(24), ISDEXT(25)
      PARAMETER (TWOP12=4096.)
      PARAMETER (ITWO24=2**24, ICONS=2147483563)
      SAVE NOTYET, I24, J24, CARRY, SEEDS, TWOM24
      LOGICAL NOTYET
      DATA NOTYET/.TRUE./
      DATA I24,J24,CARRY/24,10,0./
C
C              Default Initialization by Multiplicative Congruential
      IF (NOTYET) THEN
         NOTYET = .FALSE.
         JSEED = 314159265
         WRITE(6,'(A,I12)') ' CARRAN DEFAULT INITIALIZATION: ',JSEED
            TWOM24 = 1.
         DO 25 I= 1, 24
            TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
   25    CONTINUE
         DO 50 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
   50    CONTINUE
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .LT. SEEDS(14)) CARRY = TWOM24
      ENDIF
C
C          The Generator proper: 'Subtract-with-borrow',
C          as proposed by Marsaglia and Zaman,
C          Florida State University, March, 1989
C
      DO 100 IVEC= 1, LENV
      UNI = SEEDS(I24) - SEEDS(J24) - CARRY
      IF (UNI .LT. 0.)  THEN
         UNI = UNI + 1.0
         CARRY = TWOM24
      ELSE
         CARRY = 0.
      ENDIF
      SEEDS(I24) = UNI
      I24 = I24 - 1
      IF (I24 .EQ. 0)  I24 = 24
      J24 = J24 - 1
      IF (J24 .EQ. 0)  J24 = 24
      RVEC(IVEC) = UNI
  100 CONTINUE
      RETURN
C           Entry to input and float integer seeds from previous run
      ENTRY CARRES(ISDEXT)
         TWOM24 = 1.
         DO 195 I= 1, 24
  195    TWOM24 = TWOM24 * 0.5
      WRITE(6,'(A)') ' FULL INITIALIZATION OF CARRAN WITH 25 INTEGERS:'
      WRITE(6,'(5X,5I12)') ISDEXT
      DO 200 I= 1, 24
      SEEDS(I) = REAL(ISDEXT(I))*TWOM24
  200 CONTINUE
      CARRY = REAL(MOD(ISDEXT(25),10))*TWOM24
      ISD = ISDEXT(25)/10
      I24 = MOD(ISD,100)
      ISD = ISD/100
      J24 = ISD
      RETURN
C                    Entry to ouput seeds as integers
      ENTRY CAROUT(ISDEXT)
      DO 300 I= 1, 24
         ISDEXT(I) = INT(SEEDS(I)*TWOP12*TWOP12)
  300 CONTINUE
      ICARRY = 0
      IF (CARRY .GT. 0.)  ICARRY = 1
      ISDEXT(25) = 1000*J24 + 10*I24 + ICARRY
      RETURN
C                    Entry to initialize from one integer
      ENTRY CARINI(INSEED)
      JSEED = INSEED
      WRITE(6,'(A,I12)') ' CARRAN INITIALIZED FROM SEED ',INSEED
C      TWOM24 = 1.
         DO 325 I= 1, 24
           TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
  325    CONTINUE
         DO 350 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
  350    CONTINUE
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .LT. SEEDS(14)) CARRY = TWOM24
      RETURN
      END

      SUBROUTINE ECURAN(RVEC,LEN)
C         Random number generator given by L'Ecuyer in
C            Comm. ACM Vol 31, p.742, 1988
C            modified by F. James to return a vector of numbers
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for ECURAN:                                  ++
C!!!      CALL ECURAN (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL ECUINI(I1,I2)    initializes the generator from two   ++
C!!!                   32-bit integers I1 and I2                     ++
C!!!      CALL ECUOUT(I1,I2)    outputs the current values of the    ++
C!!!                   two integer seeds, to be used for restarting  ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(*)
      SAVE ISEED1,ISEED2
      DATA ISEED1,ISEED2 /12345,67890/
C
      DO 100 I= 1, LEN
      K = ISEED1/53668
      ISEED1 = 40014*(ISEED1 - K*53668) - K*12211
      IF (ISEED1 .LT. 0) ISEED1=ISEED1+2147483563
C
      K = ISEED2/52774
      ISEED2 = 40692*(ISEED2 - K*52774) - K* 3791
      IF (ISEED2 .LT. 0) ISEED2=ISEED2+2147483399
C
      IZ = ISEED1 - ISEED2
      IF (IZ .LT. 1)  IZ = IZ + 2147483562
C
      RVEC(I) = REAL(IZ) * 4.656613E-10
  100 CONTINUE
      RETURN
C
      ENTRY ECUINI(IS1,IS2)
      ISEED1 = IS1
      ISEED2 = IS2
      RETURN
C
      ENTRY ECUOUT(IS1,IS2)
      IS1 = ISEED1
      IS2 = ISEED2
      RETURN
      END

      SUBROUTINE VARRAN(DRVEC,LEN)
C     ***************************
C Switchable random number generator
C Translation to double precision
C     ***************************
      COMMON / RANPAR / KEYRND
      save   / RANPAR /
      DOUBLE PRECISION DRVEC(*)
      DIMENSION RVEC(1000)
      IF(LEN.LT.1.OR.LEN.GT.1000) GOTO 901
   10 CONTINUE
      IF(KEYRND.EQ.1) THEN
         CALL RANMAR(RVEC,LEN)
      ELSEIF(KEYRND.EQ.2) THEN
         CALL ECURAN(RVEC,LEN)
      ELSEIF(KEYRND.EQ.3) THEN
         CALL CARRAN(RVEC,LEN)
      ELSE
         GOTO 902
      ENDIF
C random numbers 0 and 1 not accepted
      DO 30 I=1,LEN
      IF(RVEC(I).LE.0E0.OR.RVEC(I).GE.1E0) THEN
        WRITE(6,*) ' +++++ VARRAN: RVEC=',RVEC(I)
        GOTO 10
      ENDIF
      DRVEC(I)=RVEC(I)
   30 CONTINUE
      RETURN
  901 WRITE(6,*) ' +++++ STOP IN VARRAN: LEN=',LEN
      STOP
  902 WRITE(6,*) ' +++++ STOP IN VARRAN: WRONG KEYRND',KEYRND
      STOP
      END

      SUBROUTINE BOSTDQ(MODE,QQ,PP,R)
C     *******************************
C BOOST ALONG ARBITRARY AXIS (BY RONALD KLEISS).
C P BOOSTED INTO R  FROM ACTUAL FRAME TO REST FRAME OF Q
C FORTH (MODE = 1) OR BACK (MODE = -1).
C Q MUST BE A TIMELIKE, P MAY BE ARBITRARY.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER ( NOUT =6 )
      DIMENSION QQ(*),PP(*),R(*)
      DIMENSION Q(4),P(4)

      DO 10 K=1,4
      P(K)=PP(K)
   10 Q(K)=QQ(K)
      AMQ =DSQRT(Q(4)**2-Q(1)**2-Q(2)**2-Q(3)**2)
      IF    (MODE.EQ.-1) THEN
         R(4) = (P(1)*Q(1)+P(2)*Q(2)+P(3)*Q(3)+P(4)*Q(4))/AMQ
         FAC  = (R(4)+P(4))/(Q(4)+AMQ)
      ELSEIF(MODE.EQ. 1) THEN
         R(4) =(-P(1)*Q(1)-P(2)*Q(2)-P(3)*Q(3)+P(4)*Q(4))/AMQ
         FAC  =-(R(4)+P(4))/(Q(4)+AMQ)
      ELSE
         WRITE(NOUT,*) ' ++++++++ WRONG MODE IN BOOST3 '
         STOP
      ENDIF
      R(1)=P(1)+FAC*Q(1)
      R(2)=P(2)+FAC*Q(2)
      R(3)=P(3)+FAC*Q(3)
      END


C BOOST ALONG X AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
      SUBROUTINE BOSTD1(EXE,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(1)
      RMI=RVEC(4)-RVEC(1)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(2)=RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(1)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      END

C BOOST ALONG Z AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
      SUBROUTINE BOSTD3(EXE,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(3)
      RMI=RVEC(4)-RVEC(3)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(1)=RVEC(1)
      QVEC(2)=RVEC(2)
      QVEC(3)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      END

      SUBROUTINE ROTOD1(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)=RVEC(1)
      QVEC(2)= CS*RVEC(2)-SN*RVEC(3)
      QVEC(3)= SN*RVEC(2)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END

      SUBROUTINE ROTOD2(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)+SN*RVEC(3)
      QVEC(2)=RVEC(2)
      QVEC(3)=-SN*RVEC(1)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END

      SUBROUTINE ROTOD3(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)-SN*RVEC(2)
      QVEC(2)= SN*RVEC(1)+CS*RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(4)=RVEC(4)
      END

      FUNCTION ANGFI(X,Y)
C     *******************
* CALCULATES ANGLE IN (0,2*PI) RANGE OUT OF X-Y
*     ***********************
      IMPLICIT REAL*8(A-H,O-Z)
      DATA PI /3.1415926535897932D0/

      IF(ABS(Y).LT.ABS(X)) THEN
        THE=ATAN(ABS(Y/X))
        IF(X.LE.0D0) THE=PI-THE
      ELSE
        THE=ACOS(X/SQRT(X**2+Y**2))
      ENDIF
      IF(Y.LT.0D0) THE=2D0*PI-THE
      ANGFI=THE
      END

      SUBROUTINE DUMPT(NUNIT,WORD,PP)
C     *******************************
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8 WORD
      REAL*8 PP(4)
      AMS=PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2
      IF(AMS.GT.0.0) AMS=SQRT(AMS)
      WRITE(NUNIT,'(1X,A8,5(1X,F13.8))') WORD,(PP(I),I=1,4),AMS
C======================================================================
C================END OF YFSLIB=========================================
C======================================================================
      END
      SUBROUTINE sprogz(s,PROPG,PROPZ)
!---------------------------------------------------------------!
! This routine comes originally from ALIBABA (see below).       !
! Propagators factor in the s-channel.                          !
! Modified by: Wieslaw Placzek           Knoxville, Oct. 1995   !
! Last update: 09.02.1996     by: W.P.                          !
!---------------------------------------------------------------!
!WP   SUBROUTINE GZPROP(QSQR,PROPG,PROPZ,MIXING)
*     -----------------
* The gamma-Z propagators and their mixing, up to one loop corrections,
* but for the imaginary part of the Z propagator, which includes
* second order corrections.
* QSQR is input: the momentum transfer squared through the progagators.
* PROPG, PROPZ and MIXING are complex*16 output.
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 IMSIGG,IMSGGZ,IMSIGZ,IMZ2
      COMPLEX*16 Z1,Z2,Z3, PROPG,PROPZ,MIXING
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
!WP
      SAVE  / BOSONS /, / OPTION /
      QSQR = s
      IF(IWEAK .EQ. 1) THEN
        Z1 = DCMPLX( SIGG (QSQR) , IMSIGG(QSQR) )
        Z2 = DCMPLX( SIGZ (QSQR) , IMSIGZ(QSQR) + IMZ2(QSQR) )
!WP     Z3 = DCMPLX( SIGGZ(QSQR) , IMSGGZ(QSQR) )
        PROPG = 1D0/( QSQR + Z1 )
        PROPZ = 1D0/( QSQR - RMZ**2 + Z2 )
!WP     MIXING= - Z3/( QSQR*(QSQR-RMZ**2+Z2) )
      ELSE
        PROPG  = 1D0/QSQR
!WP     IF(QSQR .GT. 0D0) THEN
          PROPZ  = 1D0/DCMPLX( QSQR-RMZ**2 , RMZ*ZWID )
!WP     ELSE
!WP       PROPZ  = 1D0/DCMPLX( QSQR-RMZ**2 , 0D0 )
!WP     ENDIF
!WP      MIXING = DCMPLX( 0D0 , 0D0 )
      ENDIF
      END

      SUBROUTINE tprogz(t,gprof,zprof)
*     ********************************
!---------------------------------------------------------------!
! This routine comes originally from ALIBABA (see below).       !
! Propagators factor in  t-channel (no imaginary parts!).       !
! Modified by: Wieslaw Placzek           Knoxville, Oct. 1995   !
! Last update: 09.02.1996     by: W.P.                          !
!---------------------------------------------------------------!
!WP   SUBROUTINE GZPROP(QSQR,PROPG,PROPZ,MIXING)
*     -----------------
* The gamma-Z propagators and their mixing, up to one loop corrections,
* but for the imaginary part of the Z propagator, which includes
* second order corrections.
* QSQR is input: the momentum transfer squared through the progagators.
* PROPG, PROPZ and MIXING are complex*16 output.
      IMPLICIT REAL*8(A-H,O-Z)
!WP   REAL*8 IMSIGG,IMSGGZ,IMSIGZ,IMZ2
!WP   COMPLEX*16 Z1,Z2,Z3, PROPG,PROPZ,MIXING
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
!WP
      SAVE  / BOSONS /, / OPTION /
      COMPLEX*16 HADRQQ
      IF(IWEAK .EQ. 1) THEN
!WP     Z1 = DCMPLX( SIGG (QSQR) , IMSIGG(QSQR) )
!WP     Z2 = DCMPLX( SIGZ (QSQR) , IMSIGZ(QSQR) + IMZ2(QSQR) )
!WP     Z3 = DCMPLX( SIGGZ(QSQR) , IMSGGZ(QSQR) )
!WP     PROPG = 1D0/( QSQR + Z1 )
!WP     PROPZ = 1D0/( QSQR - RMZ**2 + Z2 )
!WP     MIXING= - Z3/( QSQR*(QSQR-RMZ**2+Z2) )
!WP: No imaginary parts in t-channel
        hadcor = HADRQQ(t)
        gprof = 1/( t + SIGG(t) )/( 1 - hadcor - PHADPI(t) )
        zprof = 1/( t - RMZ**2 + SIGZ(t) )
      ELSE
!WP     PROPG  = 1D0/QSQR
!WP     IF(QSQR .GT. 0D0) THEN
!WP       PROPZ  = 1D0/DCMPLX( QSQR-RMZ**2 , RMZ*ZWID )
!WP     ELSE
!WP       PROPZ  = 1D0/DCMPLX( QSQR-RMZ**2 , 0D0 )
!WP     ENDIF
!WP     MIXING = DCMPLX( 0D0 , 0D0 )
!WP: No imaginary parts in t-channel
        gprof = 1/t
        zprof = 1/(t - RMZ**2)
      ENDIF
      END

      SUBROUTINE FprogZ(QSQR,PROPG,PROPZ,MIXING)
!---------------------------------------------------------------!
! This routine comes originally from ALIBABA (see below).       !
! Gamma and Z propagators and their mixing.                     !
! New vacuum polarization added!                                !
! Modified by: Wieslaw Placzek           Knoxville, Sept. 1996  !
! Last update: 25.09.1996     by: W.P.                          !
!---------------------------------------------------------------!
!WP   SUBROUTINE GZPROP(QSQR,PROPG,PROPZ,MIXING)
*     -----------------
* The gamma-Z propagators and their mixing, up to one loop corrections,
* but for the imaginary part of the Z propagator, which includes
* second order corrections.
* QSQR is input: the momentum transfer squared through the progagators.
* PROPG, PROPZ and MIXING are complex*16 output.
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 IMSIGG,IMSGGZ,IMSIGZ,IMZ2
      COMPLEX*16 Z1,Z2,Z3, PROPG,PROPZ,MIXING
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
!WP
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      SAVE  / BOSONS /, / OPTION /, / BHPAR3 /, / BHPARZ /
!
      KeyPia = MOD(KeyRad,10)
! Vacuum polarization
      CALL vacpol(KeyPia,QSQR,SINW2,RePiq,dRePiq)
      IF(IWEAK .EQ. 1) THEN
!WP        Z1 = DCMPLX( SIGG (QSQR) , IMSIGG(QSQR) )
        Z1 = DCMPLX( QSQR*RePiq , IMSIGG(QSQR) )
        Z2 = DCMPLX( SIGZ (QSQR) , IMSIGZ(QSQR) + IMZ2(QSQR) )
        Z3 = DCMPLX( SIGGZ(QSQR) , IMSGGZ(QSQR) )
        PROPG = 1D0/( QSQR + Z1 )
        PROPZ = 1D0/( QSQR - RMZ**2 + Z2 )
        MIXING= - Z3/( QSQR*(QSQR-RMZ**2+Z2) )
      ELSE
!WP        PROPG  = 1D0/QSQR
        PROPG  = 1D0/QSQR/(1+RePiq)
        IF(QSQR .GT. 0D0) THEN
          PROPZ  = 1D0/DCMPLX( QSQR-RMZ**2 , RMZ*ZWID ) /(1+RePiq)
        ELSE
          PROPZ  = 1D0/DCMPLX( QSQR-RMZ**2 , 0D0 ) /(1+RePiq)
        ENDIF
        MIXING = DCMPLX( 0D0 , 0D0 )
      ENDIF
      END

      FUNCTION xmatvs(s,t,epsCMS)
*     ***************************
!---------------------------------------------------------------!
! This routine comes originally from ALIBABA (see below).       !
! Modified by: Wieslaw Placzek           Knoxville, Oct. 1995   !
! Last update: 25.09.1996     by: W.P.                          !
!---------------------------------------------------------------!
!WP   FUNCTION EEEEVS(COSTH)
*     ---------------
* Calculation of the non-log terms of the virtual and soft corrections
* on the Born Bhabha cross section. Included are the corrections due
* to final state and initial state photons and their interference,
* and hence also box diagrams. COSTH is input and is to be integrated
* over.
* W. Beenakker and S.C. van der Marck, April 1990.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( OFFSET = 1D-10 )
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      REAL*8 MATRIX(1:6), RESEFF(1:4)
      COMPLEX*16 LABDAS,LABDAT,LABDFS,LABDFT,LE,LL,G,A, SPENCE,MZ2
      COMPLEX*16 GS,GT,ZS,ZT,MIX,GZS(-1:1,-1:1),GZT(-1:1,-1:1)
      COMPLEX*16 AGZS,VGZS,AGGS,VGGS,AGZT,VGZT,AGGT,VGGT,GDINT,HULP
      SAVE
* Statement functions for the box corrections
      G(SL,TL) = SL/2D0/(SL+TL)*LOG(TL/CMPLX(SL,OFFSET)) -
     +           SL*(SL+2D0*TL)/4D0/(SL+TL)**2*(
     +             LOG(TL/CMPLX(SL,OFFSET))**2 + PI*PI )
      A(SL,TL) = (SL-MZ2)/(SL+TL)*( LOG(TL/(SL-MZ2)) +
     +             MZ2/SL*LOG(1D0-SL/MZ2) + (SL+2D0*TL+MZ2)/(SL+TL)*(
     +               LOG(-TL/MZ2)*LOG((MZ2-SL)/(MZ2+TL)) +
     +               SPENCE(SL/MZ2) - SPENCE(-TL/MZ2) ) )
      IEVS=IEVS+1
!
!WP: COSTH from BHWIDE
      COSTH = 1 +2*t/s
!WP: Soft photon cut-off from BHWIDE
      EPS2  = epsCMS
!WP   S = SCM
      PPQP = .25D0*S*( 1D0 - COSTH )
      PPQM = .25D0*S*( 1D0 + COSTH )
      PPPM =  .5D0*S
!WP   T = - 2D0*PPQP
* Define propagators.
      E2   = EE**2
      QF2  = QF(1)*QF(IFERM)
      QF3  = QF(1)**2
!WP   CALL GZPROP(S ,GS ,ZS ,MIX)
!WP   CALL GZPROP(T ,GT ,ZT ,MIX)
      CALL FprogZ(S ,GS ,ZS ,MIX)
      CALL FprogZ(T ,GT ,ZT ,MIX)
      IF(ICHANN .EQ. 1) THEN
        ZT = (0D0,0D0)
        GT = (0D0,0D0)
      ELSEIF(ICHANN .EQ. 2) THEN
        ZS = (0D0,0D0)
        GS = (0D0,0D0)
      ENDIF
      I=IFERM
      DO 20 L1 = - 1 , 1 , 2
        DO 10 L2 = - 1 , 1 , 2
          GZS(L1,L2)=E2*(QF2*GS+(VF(1)-L1*AF(1))*(VF(I)-L2*AF(I))*ZS)
          GZT(L1,L2)=E2*(QF3*GT+(VF(1)-L1*AF(1))*(VF(1)-L2*AF(1))*ZT)
   10   CONTINUE
   20 CONTINUE
* Start calculating corrections
      SK = S
      U  = - S - T
      MZ2 = CMPLX( RMZ**2 , - RMZ*ZWID )
* The photonic vertex correction: initial state -------------------
      LE =   LOG( -CMPLX(S,OFFSET)/RMASS2(1) )
      LL = - LOG( -CMPLX(S,OFFSET)/SK )
      LABDAS= ALFA/2D0/PI*QF(1)**2*( 2D0*LL*( LE-1D0 ) + LE*(LE-1D0)
     +                           + 2D0*LE + 4D0*( PI**2/12D0 - 1D0 ) )
      LE =   LOG( -CMPLX(T,OFFSET)/RMASS2(1) )
      LL = - LOG( -CMPLX(T,OFFSET)/SK )
      LABDAT= ALFA/2D0/PI*QF(1)**2*( 2D0*LL*( LE-1D0 ) + LE*(LE-1D0)
     +                           + 2D0*LE + 4D0*( PI**2/12D0 - 1D0 ) )
* and final state (more precise: the other fermion current)
      LE =   LOG( -CMPLX(S,OFFSET)/RMASS2(IFERM) )
      LL = - LOG( -CMPLX(S,OFFSET)/SK )
      LABDFS= ALFA/2D0/PI*QF(IFERM)**2*( 2D0*LL*(LE-1D0) + LE*(LE-1D0)
     +                           + 2D0*LE + 4D0*( PI**2/12D0 - 1D0 ) )
      LE =   LOG( -CMPLX(T,OFFSET)/RMASS2(IFERM) )
      LL = - LOG( -CMPLX(T,OFFSET)/SK )
      LABDFT= ALFA/2D0/PI*QF(IFERM)**2*( 2D0*LL*(LE-1D0) + LE*(LE-1D0)
     +                           + 2D0*LE + 4D0*( PI**2/12D0 - 1D0 ) )
* Subtract the leading log terms, to end up with non-log terms only
      BETAL1 = BETALL + 2D0*ALFA/PI
      BETAF1 = BETALF + 2D0*ALFA/PI*QF(IFERM)**2
!WP   LABDAS = LABDAS + LABDFS - .75D0*( BETAL1 + BETALF )
!WP   LABDAT = LABDAT + LABDFT - .75D0*( BETAL1 + BETALF )
      LABDAS = LABDAS + LABDFS
      LABDAT = LABDAT + LABDFT
* The soft photon corrections ----------------------------
      XF    = - RMASS2(IFERM)/S
      XFLOG = LOG(-XF)
      XFLI2 = PI*PI/6D0
      BE    = LOG(S/RMASS2(1)) - 1D0
      BFIN  = BE
      BINT  = 0D0
      IF(ABS(ABS(COSTH)-1D0).GT.0D0) BINT=2D0*LOG(T/U)
      IF (IFERM .GT. 1) BFIN = LOG(S/RMASS2(IFERM)) - 1D0
      IF (IFERM .EQ. 0) BFIN = 0D0
      GIR = - ALFA/PI*LOG(SK/S)*( QF(1)**2*BE + QF(IFERM)**2*BFIN
     +                          + QF(1)*QF(IFERM)*BINT )
      GD = ALFA/PI*2D0*LOG(EPS2)*( QF(1)**2*BE + QF(IFERM)**2*BFIN
     +         + QF(1)*QF(IFERM)*BINT )
      GFIN =-ALFA/PI*( QF(1)**2*( PI**2/3D0 - .5D0 + .5D0*BE**2 )
     +        + QF(IFERM)**2*( XFLOG + 2D0*XFLI2 + .5D0*XFLOG**2 ) )
      IF(ABS(ABS(COSTH)-1D0).GT.0D0)
     +  GFIN = GFIN - ALFA/PI*( + 2D0*QF(1)*QF(IFERM)*(
     +              + DILOG(1D0+S/T) - DILOG(1D0+S/U) ) )
      DELSOF = GIR + GD + GFIN
* Subtract the leading log terms, to end ...
!WP   DELSOF = DELSOF - LOG(EPS2)*( BETALL + BETALF )
      DELSOF = DELSOF
* Resonance effects when soft photons are not all that soft ...
      HULP = (S-MZ2)/(S*(1D0-EPS)-MZ2)
      GDINT = ALFA/PI*2D0*( QF(1)**2*BE*LOG(HULP) +
     +        .5D0*QF(1)*QF(IFERM)*BINT*LOG(HULP) )
      GDRES = ALFA/PI*2D0*( QF(1)**2*BE*LOG(ABS(HULP)) +
     +             QF(1)*QF(IFERM)*BINT*LOG(ABS(HULP)) +
     +             QF(1)**2*BE*(S-RMZ**2)/RMZ/ZWID*(
     +          ATAN((RMZ**2-S*(1D0-EPS))/(RMZ*ZWID)) -
     +          ATAN((RMZ**2-S          )/(RMZ*ZWID)) )  )
      K = 0
      E4 = E2 * E2
      DO 40 L1 = - 1 , 1 , 2
        DO 30 LF = - 1 , 1 , 2
          K = K + 1
          RESEFF(K) = E4*
     +             DREAL((VF(1)-L1*AF(1))*(VF(IFERM)-LF*AF(IFERM))*ZS*
     +      CONJG((VF(1)-L1*AF(1))*(VF(IFERM)-LF*AF(IFERM))*ZS))*GDRES
     +           + 2D0*DREAL( QF(1)*QF(IFERM)*GS*CONJG(
     +            (VF(1)-L1*AF(1))*(VF(IFERM)-LF*AF(IFERM))*ZS))*GDINT
   30   CONTINUE
   40 CONTINUE
      RESEFF(1) = RESEFF(1) + E4*
     +           2D0*DREAL( (QF(1)*QF(1)*GT+(VF(1)+AF(1))**2*ZT)*
     +           CONJG( (VF(1)+AF(1))*(VF(1)+AF(1))*ZS ) )*GDINT
      RESEFF(4) = RESEFF(4) + E4*
     +           2D0*DREAL( (QF(1)*QF(1)*GT+(VF(1)-AF(1))**2*ZT)*
     +           CONJG( (VF(1)-AF(1))*(VF(1)-AF(1))*ZS ) )*GDINT
      ALPI = ALFA/PI * EE**2
      IF ( ICHANN .EQ. 1 ) THEN
        IF( IFINAL+IORDER .EQ. 0 ) THEN
          LABDAS = (0D0,0D0)
          DELSOF = 0D0
          ALPI   = 0D0
        ELSEIF ( IFINAL.EQ.0 ) THEN
          LABDAT = (0D0,0D0)
          LABDAS = LABDAS - ( LABDFS - .75D0*BETAF1 )
          DELSOF =-ALFA/PI*QF(1)**2*( PI**2/3D0 -.5D0 +.5D0*BE**2 )
          ALPI   = 0D0
        ELSEIF ( IORDER.EQ.0 .AND. ICHANN.EQ.1 ) THEN
          LABDAT = (0D0,0D0)
          LABDAS = LABDFS - .75D0*BETAF1
          DELSOF = - ALFA/PI*QF(IFERM)**2*
     +                  (XFLOG+2D0*XFLI2+.5D0*XFLOG**2)
          ALPI   = 0D0
        ENDIF
      ENDIF
*
* And finally the box corrections ------------------------
      AGGS = ALPI*( G(S,T) + G(S,U) )
      VGGS = ALPI*( G(S,T) - G(S,U) + 2D0*LOG(SK/CMPLX(-S,-OFFSET))*
     +                                    LOG(T/U) )
      AGZS = ALPI*( A(S,T) + A(S,U) )
      VGZS = ALPI*( A(S,T) - A(S,U) + 2D0*SPENCE(1D0+MZ2/T) -
     +                                2D0*SPENCE(1D0+MZ2/U) +
     +                        4D0*LOG(SQRT(MZ2*SK)/(MZ2-S))*LOG(T/U) )
      AGGT = ALPI*( G(T,S) + G(T,U) )
      VGGT = ALPI*( G(T,S) - G(T,U) + 2D0*LOG(SK/CMPLX(-T,-OFFSET))*
     +                                    LOG(CMPLX(S,OFFSET)/U) )
      AGZT = ALPI*( A(T,S) + A(T,U) )
      VGZT = ALPI*( A(T,S) - A(T,U) + 2D0*SPENCE(1D0+MZ2/S) -
     +                                2D0*SPENCE(1D0+MZ2/U) +
     +          4D0*LOG(SQRT(MZ2*SK)/(MZ2-T))*LOG(CMPLX(S,OFFSET)/U) )
*
* Combine the corrections with right helicity combinations with the
* different matrix element structures.
* The six helicity combinations are (p+ p- q+ q-):
* + + + +, - - - -, + + - -, - - + +, + - + -, - + - +
      MATRIX(1) = 16D0*PPQM**2*( (GZS( 1, 1)+GZT( 1, 1))*
     1                    DCONJG( GZS( 1, 1)*(LABDAS+DELSOF) +
     2                            GZT( 1, 1)*(LABDAT+DELSOF) +
     3             QF(1)**2*QF(IFERM)**2*GS*( VGGS + AGGS ) +
     4                 QF(1)**2*QF(1)**2*GT*( VGGT + AGGT ) +
     5     QF(1)*QF(IFERM)*(VF(1)-AF(1))*(VF(IFERM)-AF(IFERM))*
     6                                   ZS*( VGZS + AGZS ) +
     7     QF(1)*QF(1)*(VF(1)-AF(1))*(VF(1)-AF(1))*
     8                                   ZT*( VGZT + AGZT ) )
     +     + RESEFF(4) )
      MATRIX(2) = 16D0*PPQM**2*( (GZS(-1,-1)+GZT(-1,-1))*
     1                    DCONJG( GZS(-1,-1)*(LABDAS+DELSOF) +
     2                            GZT(-1,-1)*(LABDAT+DELSOF) +
     3             QF(1)**2*QF(IFERM)**2*GS*( VGGS + AGGS ) +
     4                 QF(1)**2*QF(1)**2*GT*( VGGT + AGGT ) +
     5     QF(1)*QF(IFERM)*(VF(1)+AF(1))*(VF(IFERM)+AF(IFERM))*
     6                                   ZS*( VGZS + AGZS ) +
     7     QF(1)*QF(1)*(VF(1)+AF(1))*(VF(1)+AF(1))*
     8                                   ZT*( VGZT + AGZT ) )
     +     + RESEFF(1) )
      MATRIX(3) = 16D0*PPQP**2*( GZS( 1,-1)*
     1                   DCONJG( GZS( 1,-1)*(LABDAS+DELSOF) +
     2              QF(1)**2*QF(IFERM)**2*GS*( VGGS - AGGS ) +
     3     QF(1)*QF(IFERM)*(VF(1)-AF(1))*(VF(IFERM)+AF(IFERM))*
     4                                    ZS*( VGZS - AGZS ) )
     +     + RESEFF(3) )
      MATRIX(4) = 16D0*PPQP**2*( GZS(-1, 1)*
     1                   DCONJG( GZS(-1, 1)*(LABDAS+DELSOF) +
     2              QF(1)**2*QF(IFERM)**2*GS*( VGGS - AGGS ) +
     3     QF(1)*QF(IFERM)*(VF(1)+AF(1))*(VF(IFERM)-AF(IFERM))*
     4                                    ZS*( VGZS - AGZS ) )
     +     + RESEFF(2) )
      MATRIX(5) = 16D0*PPPM**2* GZT( 1,-1)*
     1                  DCONJG( GZT( 1,-1)*(LABDAT+DELSOF) +
     2            QF(1)**2*QF(IFERM)**2*GT*( VGGT - AGGT ) +
     3     QF(1)*QF(1)*(VF(1)-AF(1))*(VF(1)+AF(1))*
     4                                  ZT*( VGZT - AGZT ) )
      MATRIX(6) = 16D0*PPPM**2* GZT(-1, 1)*
     1                  DCONJG( GZT(-1, 1)*(LABDAT+DELSOF) +
     2             QF(1)**2*QF(IFERM)**2*GT*( VGGT - AGGT ) +
     3     QF(1)*QF(1)*(VF(1)+AF(1))*(VF(1)-AF(1))*
     4                                   ZT*( VGZT - AGZT ) )
*
      SUM = MATRIX(1) + MATRIX(2) + MATRIX(3) +
     +      MATRIX(4) + MATRIX(5) + MATRIX(6)
* conversion to picobarn, 2pi from azimuthal angle, 1/8/(2pi)**2 from
* phase space, 1/(2s) from flux factor, 1/4 from spin averaging.
!WP   EEEEVS = HBARC2/8D0/2D0/PI/2D0/S/4D0 * SUM
      xmatvs = SUM/4
      END

      FUNCTION xmatwc(s,t)
*     ********************
!---------------------------------------------------------------!
! This routine comes originally from ALIBABA (see below).       !
! Modified by: Wieslaw Placzek           Knoxville, Oct. 1995   !
! Last update: 25.09.1996     by: W.P.                          !
!---------------------------------------------------------------!
!WP   FUNCTION EEEEW(COSTH)
*     ---------------
* The Born e+e- --> e+e- matrix element squared, including both gamma
* and Z in both s and t channel, and including WEAK corrections.
* Summing/averaging over spins is performed.
* COSTH is input and is to integrated over.
* W. Beenakker and S.C. van der Marck, April 1990.
* Heavy boxes (ZZ and WW) added: July 1990.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      REAL*8 MATRIX(1:6),ZCOP(-1:1,-1:1)
      COMPLEX*16 GS,GT,ZS,ZT,MIXS,MIXT,GZS(-1:1,-1:1), GZT(-1:1,-1:1)
      COMPLEX*16 HADRQQ,VZZS,AZZS,VWWS,AWWS,VZZT,AZZT,VWWT,AWWT
      SAVE
      IEEW=IEEW+1
!
!WP: COSTH from BHWIDE
      COSTH = 1 +2*t/s
!WP   S = SCM
      PPQP = .25D0*S*( 1D0 - COSTH )
      PPQM = .25D0*S*( 1D0 + COSTH )
      PPPM =  .5D0*S
!WP   T    = - 2D0*PPQP
* Define propagators, and include vertex form factors.
      E2   = 4D0*PI*ALFA
!WP   CALL GZPROP(S,GS,ZS,MIXS)
      CALL FprogZ(S,GS,ZS,MIXS)
      CALL FORMFS(S,IFERM)
      IF(ICHANN .EQ. 2) THEN
        GS = (0D0,0D0)
        ZS = (0D0,0D0)
        MIXS=(0D0,0D0)
      ENDIF
      I=IFERM
      DO 20 L1 = - 1 , 1 , 2
        DO 10 L2 = - 1 , 1 , 2
          GZS(L1,L2)=E2*( ( -QF(1)-FGV(1)-L1*(      -FGA(1) ) )*
     +                    ( -QF(I)-FGV(I)-L2*(      -FGA(I) ) )*GS +
     +                    (  VF(1)+FZV(1)-L1*( AF(1)+FZA(1) ) )*
     +                    (  VF(I)+FZV(I)-L2*( AF(I)+FZA(I) ) )*ZS -
     +      ( QF(1)*(VF(I)-L2*AF(I)) + QF(I)*(VF(1)-L1*AF(1)) )*MIXS )
          ZCOP(L1,L2) = ((VF(1)-L1*AF(1))*(VF(IFERM)-L2*AF(IFERM)))**2
   10   CONTINUE
   20 CONTINUE
*     Heavy boxes !
      IF(ICHANN .EQ. 2) THEN
        VZZS = (0D0,0D0)
        AZZS = (0D0,0D0)
        VWWS = (0D0,0D0)
        AWWS = (0D0,0D0)
      ELSE
        CALL HEAVYB(S,T,VZZS,AZZS,VWWS,AWWS)
      ENDIF
*     Now everything for the t channel
!WP   CALL GZPROP(T,GT ,ZT ,MIXT)
      CALL FprogZ(T,GT ,ZT ,MIXT)
      CALL FORMFS(T,1)
*     Incorporate the Burkhardt fit for the light quark loops.
!WP   GT = GT/( 1D0 - HADRQQ(T) - PHADPI(T) )
      IF(ICHANN .EQ. 1) THEN
        GT = (0D0,0D0)
        ZT = (0D0,0D0)
        MIXT=(0D0,0D0)
      ENDIF
      DO 40 L1 = - 1 , 1 , 2
        DO 30 L2 = - 1 , 1 , 2
          GZT(L1,L2)=E2*(
     +         ( -QF(1)-FGV(1)-L1*(      -FGA(1) ) )*
     +         ( -QF(1)-FGV(1)-L2*(      -FGA(1) ) )*GT +
     +         (  VF(1)+FZV(1)-L1*( AF(1)+FZA(1) ) )*
     +         (  VF(1)+FZV(1)-L2*( AF(1)+FZA(1) ) )*ZT -
     +       (QF(1)*(VF(1)-L2*AF(1))+QF(1)*(VF(1)-L1*AF(1)))*MIXT )
   30   CONTINUE
   40 CONTINUE
*     Heavy boxes !
      IF(ICHANN .EQ. 1) THEN
        VZZT = (0D0,0D0)
        AZZT = (0D0,0D0)
        VWWT = (0D0,0D0)
        AWWT = (0D0,0D0)
      ELSE
        CALL HEAVYB(T,S,VZZT,AZZT,VWWT,AWWT)
      ENDIF
* There are 6 different helicity combinations (see EEEEVS).
      IF ( ICHANN .NE. 3 ) THEN
        MATRIX(1) = 16D0*PPQM**2*(GZS( 1, 1)+GZT( 1, 1))*
     +                      CONJG(GZS( 1, 1)+GZT( 1, 1))
        MATRIX(2) = 16D0*PPQM**2*(GZS(-1,-1)+GZT(-1,-1))*
     +                      CONJG(GZS(-1,-1)+GZT(-1,-1))
        MATRIX(3) = 16D0*PPQP**2* GZS( 1,-1)*CONJG(GZS( 1,-1))
        MATRIX(4) = 16D0*PPQP**2* GZS(-1, 1)*CONJG(GZS(-1, 1))
        MATRIX(5) = 16D0*PPPM**2* GZT( 1,-1)*CONJG(GZT( 1,-1))
        MATRIX(6) = 16D0*PPPM**2* GZT(-1, 1)*CONJG(GZT(-1, 1))
*       Heavy boxes (factor 2 from 2*M0*M1)
        MATRIX(1) = MATRIX(1)+32D0*PPQM**2*(GZS( 1, 1)+GZT( 1, 1))*
     +                        CONJG((VZZS+AZZS+VZZT+AZZT)*ZCOP( 1, 1))
        MATRIX(2) = MATRIX(2)+32D0*PPQM**2*(GZS(-1,-1)+GZT(-1,-1))*
     +    CONJG((VZZS+AZZS+VZZT+AZZT)*ZCOP(-1,-1)+VWWS+AWWS+VWWT+AWWT)
        MATRIX(3) = MATRIX(3) +     32D0*PPQP**2*GZS( 1,-1)*
     +                         CONJG(VZZS-AZZS)*ZCOP( 1,-1)
        MATRIX(4) = MATRIX(4) +     32D0*PPQP**2*GZS(-1, 1)*
     +                         CONJG(VZZS-AZZS)*ZCOP(-1, 1)
        MATRIX(5) = MATRIX(5) +     32D0*PPPM**2*GZT( 1,-1)*
     +                         CONJG(VZZT-AZZT)*ZCOP( 1,-1)
        MATRIX(6) = MATRIX(6) +     32D0*PPPM**2*GZT(-1, 1)*
     +                         CONJG(VZZT-AZZT)*ZCOP(-1, 1)
      ELSE
        MATRIX(1) = 16D0*PPQM**2*2D0*GZS( 1, 1)*CONJG(GZT( 1, 1))
        MATRIX(2) = 16D0*PPQM**2*2D0*GZS(-1,-1)*CONJG(GZT(-1,-1))
*       Heavy boxes
        MATRIX(1) = MATRIX(1)+32D0*PPQM**2*GZS( 1, 1)*
     +                             CONJG((VZZT+AZZT)*ZCOP( 1, 1))
     +                       +32D0*PPQM**2*GZT( 1, 1)*
     +                             CONJG((VZZS+AZZS)*ZCOP( 1, 1))
        MATRIX(2) = MATRIX(2)+32D0*PPQM**2*GZS(-1,-1)*
     +                  CONJG((VZZT+AZZT)*ZCOP(-1,-1)+VWWT+AWWT)
     +                       +32D0*PPQM**2*GZT(-1,-1)*
     +                  CONJG((VZZS+AZZS)*ZCOP(-1,-1)+VWWS+AWWS)
        MATRIX(3) = 0D0
        MATRIX(4) = 0D0
        MATRIX(5) = 0D0
        MATRIX(6) = 0D0
      ENDIF
      SUM = MATRIX(1) + MATRIX(2) + MATRIX(3) +
     +      MATRIX(4) + MATRIX(5) + MATRIX(6)
!WP   EEEEW = HBARC2/8D0/2D0/PI/2D0/S/4D0 * SUM
      xmatwc = SUM/4
      END
      SUBROUTINE babewc(s,t,epsCMS,dsig0,dsig)
*     ****************************************
!---------------------------------------------------------------!
! This routine comes originally from BABAMC (see below).        !
! Modified by: Wieslaw Placzek           Knoxville, Oct. 1995   !
! Last update: 08.02.1996     by: W.P.                          !
!---------------------------------------------------------------!
!WP   SUBROUTINE BABCOR(S,T,DEL, DSIG0,DSIG)
C
C THE 'WORKING HORSE' THAT CALCULATES THE CORRECTED D(SIGMA)/D(OMEGA)
C
C   S,T ARE THE MANDELSTAM VARIABLES
C   DEL IS MAXIMUM PHOTON ENERGY / BEAM ENERGY
C   DSIG0: DIFFERENTIAL BORN CROSS SECTION (IN PBARN)
C   DSIG : DIFFERENTIAL CROSS SECTION WITH RAD. CORRECTIONS (PBARN)
C
C  MZ, MW, MH ARE THE BOSON MASSES, ME,...MT THE FERMION MASSES IN GEV.
C  SW = SIN**2 THETA-W, CW = COS**2 THETA-W, THETA-W = WEINBERG ANGLE.
C  V,A ARE THE LEPTONIC VECTOR AND AXIALVECTOR COUPLING CONSTANTS;
C  VU,AU THOSE OF I=1/2 QUARKS; VD,AD THOSE OF I=-1/2 QUARKS
C  (ALL NORMALIZED TO E=SQRT(4*PI*ALFA)).
C  GZ IS THE WIDTH OF THE Z IN GEV.
C  MF(I,J) IS THE ARRAY OF FERMION MASSES, WHERE I =1,..6 AND J=1,2;
C  LEPTONS(I=1,2,3) AND QUARKS(I=4,5,6);  J=1: UP,  J=2: DOWN MEMBERS.
C  VF IS THE CORRESPONDING ARRAY OF THE FERMION VECTOR COUPLINGS,
C  VF2, AF2 ARE THE CORRESPONDING ARRAYS OF THE VECTOR AND AXIALVECTOR
C  COUPLINGS SQUARED.
C  M IS THE ARRAY OF THE FERMION MASSES SQUARED.
C  ALFA = 1/137.036, ALPHA = ALFA/4*PI, AL = ALFA/PI
C*****************************************************************
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16   CIR1,PIGS,PIGT,PIG,X1C,X2C,SP1,SP2,SPENCE,CHIS,
     1             TGZS,TGZTS,TZZTS,TZBZS,TZBZTS,TWZS,TWZTS,TGVZS,
     2             TGZ5S,TZZZ5S,TGGZS,TGGZTS,TGZZST,TGZZTS,TGGVT,
     3             TGGVS,TGVZST,TGVZTS,TGVZT,TGZVS,TGZVST,TGZVTS,
     4             TGZVT,TZVZS,TZVZST,TZVZTS,TZVZT,TGGVST,TGGVTS,
     5             FZVS,FZAS,FZVT,FZAT,FGVS,FGAS,FGVT,FGAT,
     6             CFZVS,CFZAS,CFGVS,CFGAS,FS1,FS2,FT1,FT2,
     7             ZS,PROP,PIZS,CHITC,TGZST,TGZT
      DIMENSION MF(6,2),VF(6,2),M(6,2),VF2(6,2),AF2(6,2)
      COMMON /BOSEW/MZ,MW,MH/LEPT/ME,MMU,MTAU
     1       /HAD/MU,MD,MS,MC,MB,MT
     2       /COUP/SW,CW,V,A,VU,AU,VD,AD
     3       /WIDTH/GZ
     4       /ALF/AL,ALPHA,ALFA
     5       /FERMI/MF,VF,M,VF2,AF2
     6       /CONV/CST
!WP: Common block from BHWIDE
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      INTEGER           KEYRAD,KEYOPT,KeyEWC
      SAVE
C
!WP: Switch for weak corrections
      KeyEWC = MOD(KeyRad,10000)/1000
!WP: Soft photon cut-off
      DEL = epsCMS
C  LEPTONIC COUPLING CONSTANTS
      SW1=DSQRT(SW)
      CW1=DSQRT(CW)
      A2=A*A
      V2=V*V
      W=V2+A2
      U=2.D0*V*A
C  Z PROPAGATOR
      MZ2=MZ**2
      G2=GZ**2
      SM=S-MZ2
      TM=T-MZ2
      DS=SM**2+MZ2*G2
!WP   DT=TM**2+MZ2*G2
      DT=TM**2
      CHISR=S*SM/DS
      CHITR=T*TM/DT
      CHISI=-MZ*GZ/DS*S
      CHITI=0.D0
      CHITC=DCMPLX(CHITR,CHITI)
      CHIT=T/TM
      CHIS=DCMPLX(CHISR,CHISI)
      CHIS2=S*S/DS
      CHIT2=(T/TM)**2
C   COMBINATION OF ANGLES
      C=1.D0+2.D0*T/S
      C2=1.D0+C*C
      C1=1.D0-C
      C0=1.D0+C
      C3=C0**2/C1
      C4=(C0**2+4.D0)/C1**2
      C5=(C0**2-4.D0)/C1**2
C
      PI=3.1415926536D0
C
C  NOW THE TIJ... ARE SPECIFIED        ********************
C
C  1) BORN TERMS:
      TGZS=(V2*C2+2.D0*A2*C)*CHIS
      TGZST=-W*C3*CHITC
      TGZTS=-W*C3*CHIS
      TGZT=(V2*C4+A2*C5)*2.D0*CHITC
C
      TZZS=(W*W*C2+U*U*2.D0*C)*CHIS2
      TZZTS=-(W*W+U*U)*C3*CHIS*CHITC
      TZZT=(W*W*C4+U*U*C5)*2.D0*CHIT2
C
C  2) TERMS APPEARING IN THE PHOTON-Z-MIXING PART
C
      TGGZS=2.D0*V*C2*CHIS
      TGGZST=-2.D0*V*C3*CHIT
      TGGZTS=-2.D0*V*C3*CHIS
      TGGZT=4.D0*V*C4*CHIT
C
      TGZZS=2.D0*(V*W*C2+A*U*2.D0*C)*CHIS2
      TGZZST=-2.D0*(V*W+A*U)*DCONJG(CHIS)*CHIT*C3
      TGZZTS=-2.D0*(V*W+A*U)*CHIS*CHIT*C3
      TGZZT=4.D0*(V*W*C4+A*U*C5)*CHIT2
C
C  3) TERMS WITH WEAK VERTEX CORRECTIONS
C
!WP: Including weak corrections
      IF (KeyEWC.EQ.1) THEN
        CALL FGAM(S,FGVS,FGAS)
        CALL FGAM(T,FGVT,FGAT)
        CALL FZ(S,FZVS,FZAS)
        CALL FZ(T,FZVT,FZAT)
      ELSE
        FGVS = 0
        FGAS = 0
        FGVT = 0
        FGAT = 0
        FZVS = 0
        FZAS = 0
        FZVT = 0
        FZAT = 0
      ENDIF
      CFGVS=DCONJG(FGVS)
      CFGAS=DCONJG(FGAS)
      CFZVS=DCONJG(FZVS)
      CFZAS=DCONJG(FZAS)
C
      TGGVS=2.D0*FGVS*C2
      TGGVST=-2.D0*FGVT*C3
      TGGVTS=-2.D0*FGVS*C3
      TGGVT=4.D0*FGVT*C4
C
      TGVZS=2.D0*(V*(V*CFGVS+A*CFGAS)*C2+A*(V*CFGAS+A*CFGVS)*2.D0*C)
     1      *CHIS
      TGVZST=-2.D0*(W*CFGVS+U*CFGAS)*C3*CHIT
      TGVZTS=-2.D0*(W*FGVT+U*FGAT)*C3*CHIS
      TGVZT=4.D0*(V*(V*FGVT+A*FGAT)*C4+A*(V*FGAT+A*FGVT)*C5)*CHIT
C
      TGZVS=2.D0*(V*FZVS*C2+A*FZAS*2.D0*C)*CHIS
      TGZVST=-2.D0*(V*FZVT+A*FZAT)*C3*CHIT
      TGZVTS=-2.D0*(V*FZVS+A*FZAS)*C3*CHIS
      TGZVT=  (V*FZVT*C4+   A*FZAT*C5)*4.D0*CHIT
C
      FS1=V*CFZVS+A*CFZAS
      FS2=V*CFZAS+A*CFZVS
      FT1=V*FZVT+A*FZAT
      FT2=V*FZAT+A*FZVT
C
      TZVZS=2.D0*(W*FS1*C2+U*FS2*2.D0*C)*CHIS2
      TZVZST=-2.D0*(W*FS1+U*FS2)*C3*CHIT*DCONJG(CHIS)
      TZVZTS=-2.D0*(W*FT1+U*FT2)*C3*CHIT*CHIS
      TZVZT=4.D0*(W*FT1*C4+U*FT2*C5)*CHIT2
C
C
C  4) TERMS WHICH APPEAR WITH BOX DIAGRAMS
C
      TGZB=W*W*C2+U*U*2.D0*C
      TGZBST=-(W**2+U**2)*C3
      TGZBT=2.D0*(W*W*C4+U*U*C5)
C
      S16=16.D0*SW**2
      TGWS=C0*C0/S16
      TGWST=-2.D0*C3/S16
      TGWT=4.D0*(C0/C1)**2/S16
C
      W3=V2+3.D0*A2
      U3=3.D0*V2+A2
      TZBZS=(V2*W3*W3*C2+A2*U3*U3*2.D0*C)*CHIS
      TZBZST=-(V2*W3*W3+A2*U3*U3)*CHIT
      TZBZTS=TZBZST/CHIT*CHIS
      TZBZT=2.D0*(V2*W3*W3*C4+A2*U3*U3*C5)*CHIT
C
      VA2=(V+A)**2/S16
      TWZS=VA2*C0*C0*CHIS
      TWZST=-2.D0*VA2*C3*CHIT
      TWZTS=TWZST/CHIT*CHIS
      TWZT=4.D0*VA2*(C0/C1)**2*CHIT
C
      TGZ5S=(A2*C2+V2*2.D0*C)*CHIS
      TGZ5T =2.D0 *(A2*C4+V2*C5)*CHIT
      TZZ5S=(U*U*C2+W*W*2.D0*C)*CHIS2
      TZZ5T=2.D0*(U*U*C4+W*W*C5)*CHIT2
C
      TGZZ5S=U*U*C2+W*W*C*2.
      TGZZ5T=2.D0*(U*U*C4+W*W*C5)
      TZZZ5S=(A2*U3*U3*C2+V2*W3*W3*2.D0*C)*CHIS
      TZZZ5T=2.D0*(A2*U3*U3*C4+V2*W3*W3*C5)*CHIT
C
C END OF DEFINITION OF THE TIJ... TERMS      **************
C
C  NOW THE INFRARED CORRECTIONS ARE CALLED:
C  CIR: NON RESONANT
C  CIR1: INTERFERENCE RESONANT - NON RESONANT
C  CIR2: RESONANT
      CALL INFRA(DEL,S,T,CIR,CIR1,CIR2)
C  DEL: MAX. PHOTONENERGY/BEAM ENERGY
C  CIR1 COMPLEX, OTHERS REAL
C
C  SPECIFICATION OF THE FINITE QED CORRECTIONS:
      ME2=ME*ME
      BE=   DLOG(S/ME2)-1.D0
      X1=C1/2.D0
      X2=C0/2.D0
      DX1=DLOG(X1)
      DX2=DLOG(X2)
      X1C=DCMPLX(X1,0.D0)
      X2C=DCMPLX(X2,0.D0)
      SP1=SPENCE(X1C)
      SP2=SPENCE(X2C)
      X=DX1**2-DX2**2-2.D0*DREAL(SP1)+2.D0*DREAL(SP2)
      Z= 3.D0*BE-1.D0+2.D0*PI**2/3.D0
      Y=1.5D0*DX1-.5D0*DX1**2-PI**2/2.D0
C  TWO PHOTON BOXES
      CALL GBOX(S,T,V1S,V2S,A1S,A2S,V1T,V2T,A1T,A2T)
C  PHOTON-Z BOXES
      CALL GZBOX(S,T,V1ZS,V2ZS,A1ZS,A2ZS,V1ZT,V2ZT,A1ZT,A2ZT)
C  PHOTON VACUUM POLARIZATION
!WP   PIGS=PIG(S)
!WP   PIGT=PIG(T)
!WP   RPIGS=DREAL(PIGS)
!WP   IPIGS=DIMAG(PIGS)
!WP   RPIGT=DREAL(PIGT)
!WP: Vacuum polarization included in BHWIDE routines
      IF (KeyEWC.EQ.1) THEN
        PIGS  = PIG(S)
      ELSE
        PIGS = 0
      ENDIF
      PIGT  = 0
      RPIGS = 0
      IPIGS = DIMAG(PIGS)
      RPIGT = 0
C  SPECIFICATION OF THE WEAK CORRECTIONS:
C  Z BOSON SELF ENERGY
!WP: Obsolete!
!WP   RZS=RESZ(S)
!WP   IZS=IMSZ(S)
!WP   RZT=RESZ(T)
      RZS = 0
      IZS = 0
      RZT = 0
      ZS=DCMPLX(RZS,IZS)
      GM= MZ*GZ
      PROP= DCMPLX(SM,GM)
!WP   PIZS= PROP/(SM+ZS)-1.D0
!WP   PIZT= TM/(TM+RZT)-1.D0
      PIZS = 0
      PIZT = 0
      RPIZT=PIZT
      RPIZS=DREAL(PIZS)
      IPIZS=DIMAG(PIZS)
!WP: Including weak corrections
      IF (KeyEWC.EQ.1) THEN
C  PHOTON-Z MIXING ENERGY
        RPIGZS=-RESGZ(S)/S
        IPIGZS=-IMSGZ(S)/S
        RPIGZT=-RESGZ(T)/T
C  HEAVY BOX DIAGRAMS
        CALL BOX(S,T,V1ZZS,V2ZZS,A1ZZS,A2ZZS,
     1             V1ZZT,V2ZZT,A1ZZT,A2ZZT,
     2             V1WS,V2WS,V1WT,V2WT)
      ELSE
        RPIGZS = 0
        IPIGZS = 0
        RPIGZT = 0
        V1ZZS = 0
        V2ZZS = 0
        A1ZZS = 0
        A2ZZS = 0
        V1ZZT = 0
        V2ZZT = 0
        A1ZZT = 0
        A2ZZT = 0
        V1WS  = 0
        V2WS  = 0
        V1WT  = 0
        V2WT  = 0
      ENDIF
C  COMPOSITION OF THE "REDUCED CROSS SECTIONS"     ***********
C  PHOTON-PHOTON
      DEL1=CIR+2.D0*RPIGS+AL*(X+Z+V1S+A1S*2.D0*C/C2)
      DEL2=CIR+RPIGS+RPIGT+AL*(X+Y+Z+.5D0*(V1S+V1T+A1S+A1T))
      DEL3=CIR+2.D0*RPIGT+AL*(X+2.D0*Y+Z+V1T+A1T*C5/C4)
      SGGS=C2*(1.D0+DEL1)+2.D0*DREAL(TGGVS)
     1    +AL*(TGZB*V1ZZS+TGZZ5S*A1ZZS+TGWS*V1WS)
      SGGST=-2.D0*C3*(1.D0+DEL2)
     1    +2.D0*DREAL(TGGVTS+TGGVST)
     2     +AL*(TGZBST*(V1ZZS+A1ZZS+V1ZZT+A1ZZT)
     3          +TGWST*(V1WS+V1WT))
      SGGT=2.D0*C4*(1.D0+DEL3)
     1     +2.D0*DREAL(TGGVT)
     2     +AL*(TGZBT*V1ZZT+TGZZ5T*A1ZZT+TGWT*V1WT)
C  PHOTON-Z-INTERFERENCE
      RCIR=DREAL(CIR1)
      ICIR=DIMAG(CIR1)
      DEL11=RCIR+RPIGS+AL*(X+Z+.5D0*(V1S+V1ZS))+RPIZS
      DEL12=ICIR-IPIGS+ALFA*(V2ZS-V2S)+IPIZS
      SGZS=2.D0*DREAL(TGZS)*(1.+DEL11)-DIMAG(TGZS)*DEL12*2.D0
     1    +AL*DREAL(TGZ5S)*(A1S+A1ZS)-2.D0*ALFA*DIMAG(TGZ5S)
     &                              *(A2ZS-A2S)
     2    +2.D0*DREAL(TGGZS)*RPIGZS -2.D0*DIMAG(TGGZS)*IPIGZS
     3    +2.D0*DREAL(TGVZS+TGZVS)
     4    +AL*(DREAL(TZBZS)*V1ZZS+DREAL(TZZZ5S)*A1ZZS
     5         +DREAL(TWZS)*V1WS)
     6    +2.D0*ALFA*(DIMAG(TZBZS)*V2ZZS+DIMAG(TZZZ5S)*A2ZZS
     7              +DIMAG(TWZS)*V2WS)
C
      DEL21=CIR+RPIGS+AL*(X+Y+Z+.5D0*(V1S+A1S+V1ZT+A1ZT))+RPIZT
      DEL22=IPIGS-ALFA*(1.5D0-V2S-A2S+V2ZT+A2ZT)
      SGZST=  DREAL(TGZST)*(1.D0+DEL21)*2.D0
     1      +2.D0*TGGZST*RPIGZT +2.D0*DREAL(TGVZST+TGZVST)
     2      +AL*(TZBZST*(V1ZZS+A1ZZS)+TWZST*V1WS)
      DEL31=RCIR+RPIGT+AL*(X+Y+Z+.5D0*(V1T+A1T+V1ZS+A1ZS))+RPIZS
      DEL32=ICIR-ALFA *(1.5D0 +A2T-V2ZS-A2ZS+V2T)+IPIZS
      SGZTS=2.D0*DREAL(TGZTS)*(1.D0+DEL31)-2.D0*DIMAG(TGZTS)*DEL32
     1     +2.D0*DREAL(TGGZTS)*RPIGZS-2.D0*DIMAG(TGGZTS)*IPIGZS
     2     +2.D0 *DREAL(TGVZTS+TGZVTS)
     3     +AL*(DREAL(TZBZTS)*(V1ZZT+A1ZZT)+DREAL(TWZTS)*V1WT)
     4     +2.D0*ALFA*(DIMAG(TZBZTS)*(V2ZZT+A2ZZT)
     5               +DIMAG(TWZTS)*V2WT)
      DEL41=CIR+RPIGT+AL*(X+2.D0*Y+Z+.5D0*(V1T+V1ZT))+RPIZT
      SGZT=2.D0*DREAL(TGZT)*(1.D0+DEL41)+    TGZ5T *AL*(A1T+A1ZT)
     1     +2.D0*TGGZT*RPIGZT+2.D0*DREAL(TGZVT+TGVZT)
     2     +AL*(TZBZT*V1ZZT+TZZZ5T*A1ZZT+TWZT*V1WT)
C  Z-Z TERMS
      DEL51=CIR2+AL*(X+Z+V1ZS)+2.D0*RPIZS
      SZZS=TZZS*(1.D0+DEL51)+TZZ5S*AL*A1ZS
     1    +2.D0*TGZZS*RPIGZS+2.D0*DREAL(TZVZS)
      DEL61=RCIR+AL*(X+Y+Z+.5D0*(V1ZS+V1ZT+A1ZS+A1ZT))+RPIZS+PIZT
      DEL62=ICIR-ALFA*(1.5D0+V2ZT-V2ZS+A2ZT-A2ZS)+IPIZS
      SZZST=2.D0*DREAL(TZZTS)*(1.+DEL61)-2.*DIMAG(TZZTS)*DEL62
     1     +2.D0*DREAL(TGZZTS)*(RPIGZS+RPIGZT)
     2     +2.D0*DREAL(TZVZTS+TZVZST)-2.D0*DIMAG(TGZZTS) *IPIGZS
      DEL71=CIR+AL*(X+2.D0*Y+Z +V1ZT )+2.D0*PIZT
      SZZT=TZZT*(1.D0+DEL71)+TZZ5T*AL*A1ZT
     1    +2.D0*TGZZT*RPIGZT+2.D0*DREAL(TZVZT)
C  RADIATIVELY CORRECTED CROSS SECTION
      DSIG=SGGS+SGZS+SZZS
     1    +SGGST+SGZST+SGZTS+SZZST
     3    +SGGT+SGZT+SZZT
      DSIG=DSIG*CST
C  CROSS SECTION IN LOWEST ORDER IN NBARN
      DSIG0=C2-2.D0*C3+2.D0*C4
     1     +2.D0*DREAL(TGZS+TGZTS)+(TGZT+TGZST)*2.D0
     2     +TZZS+2.D0*DREAL(TZZTS)+TZZT
      DSIG0=DSIG0*CST
      END
!---------------------------------------------------------------!
!     This is the original ALIBABA program with a few minor     !
!     modifications by W. Placzek (last update: 08.02.1996).    !
!---------------------------------------------------------------!
      SUBROUTINE BHABHA(S,THETA1,THETA2,THETA3,THETA4,
     +                    BIGEP0,BIGEM0,ACOL,RESULT,ERROR)
*     -----------------
* Calculates the cross section for large angle Bhabha scattering at
* a total CoM energy squared of S, where the positron comes out
* at angles between THETA1 and THETA2 (defined with respect to its
* original direction) whereas the electron is required to come out
* at angles between THETA3 and THETA4 (defined with respect to its
* original direction). The positron is required to have an energy
* larger than BIGEP0 (GeV), the electron to have more that BIGEM0.
* Furthermore the acollinearity is restricted to be less than ACOL.
* S, THETA1,2,3,4, BIGEP0, BIGEM0 and ACOL are input variables.
* The result is returned as the output variable RESULT (picobarn). The
* estimated numerical error is returned as the output variable ERROR.
* (ERROR is only of practical importance if the flag NONLOG=1)
* !!! Before the first call to BHABHA, initialization has to be done
* by calling INITBH !!!
* For flag settings etc., see the comments in INITBH.
*
* THETA2 should be larger than THETA1.
* THETA1 should be larger than 0 for t channel processes.
*        It is recommended that this program is not used below 10 degr.
*        for t channel scattering.
* THETA4 should be larger than THETA3.
* THETA3 should be larger than 0 for t channel processes.
*        The program has the following built-in assumptions:
*        1) that either: theta1 .le. theta3  .and.  theta2 .ge. theta4
*                    or: theta1 .ge. theta3  .and.  theta2 .le. theta4
*           If the input does not satisfy this, it is reset to
*           theta1=theta3 and theta2=theta4.
*        2) that theta1.le.90.le.theta2  .and.  theta3.le.90.le.theta4
*           If the input does not satisfy this, it is reset to
*           theta2=180-theta1 and theta4=180-theta3.
*
* BIGEP0 and BIGEM0 should be less than .5*sqrt(s) (=kinematical upper
*                      bound on the energy of the outcoming particles)
* ACOL  should be between 0 and 180 degrees (180 means: no cut)
*
* This subroutine is not meant to be changed by the user, except
* possibly for the number of points that is required in the
* integrations by VEGAS. This number is NCALL and is set several times
* for the integration of several contributions.
*
* After this subroutine two subroutines have been added that
* calculate the forward-backward asymmetry (according two two
* different definitions), using the subroutine BHABHA. These
* subroutines AFBASY and AFBSYM are meant as examples; one can
* write other subroutines calculating the asymmetry according to
* ones own favourite definition.
*
* W. Beenakker, F.A. Berends and S.C. van der Marck, August 1990.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BOOKKP / IBH1,IBH2,IBH3,IBH4,IBH5,IBH6
      PARAMETER( NX = 10 )
      COMMON/BVEG1/XL(NX),XU(NX),ACC,NDIM,NCALL,ITMX,NPRN
      COMMON/ EVENT  / PP(0:3),PM(0:3),QP(0:3),QM(0:3),SP,QK(0:3)
      EXTERNAL SIGFLX,TWOHRD,RNONLG,EEEEVS,EEEEW
* First set parameters, use input to set integration boundaries.
* Technicalities and such.
      IF( S .LT. 4D0*RMASS2(IFERM) ) THEN
        WRITE(*,'(/,A,/)')' BHABHA: Below production threshold !'
        RESULT = 0D0
        ERROR  = 0D0
      ENDIF
      SCM = S
      C1  =   COS( THETA1 *PI /180D0 )
      C2  =   COS( THETA2 *PI /180D0 )
      C3  = - COS( THETA3 *PI /180D0 )
      C4  = - COS( THETA4 *PI /180D0 )
      TH1 = THETA1
      TH2 = THETA2
      IF( ( THETA3.LE.THETA1 .AND. THETA4.GE.THETA2 ) .OR.
     +    ( THETA3.GE.THETA1 .AND. THETA4.LE.THETA2 ) ) THEN
        IRESET = 0
        TH3 = THETA3
        TH4 = THETA4
      ELSE
        IRESET = 1
        C3  = - C1
        C4  = - C2
        TH3 = THETA1
        TH4 = THETA2
      ENDIF
      IF( TH1 .GT. 90D0 .OR. TH2 .LT. 90D0 ) THEN
        IRESET = 1
        TH2 = 180D0 - TH1
        C2  = - C1
      ENDIF
      IF( TH3 .GT. 90D0 .OR. TH4 .LT. 90D0 ) THEN
        IRESET = 1
        TH4 = 180D0 - TH3
        C4  = - C3
      ENDIF
      EBEAM = .5D0*SQRT(S)
      EP0   = BIGEP0 / EBEAM
      EM0   = BIGEM0 / EBEAM
      EP0   = MAX( EP0 , RMASS(IFERM)/EBEAM )
      EM0   = MAX( EM0 , RMASS(IFERM)/EBEAM )
      IF ( ABS(1D0-C2).GT.1D-10 .AND. ABS(1D0-C3).GT.1D-10 ) THEN
        BOUND1 = SQRT( (1D0+C2)/(1D0-C2)*(1D0+C3)/(1D0-C3) )
      ELSE
        BOUND1 = 1D0
      ENDIF
      IF ( ABS(1D0+C1).GT.1D-10 .AND. ABS(1D0+C4).GT.1D-10 ) THEN
        BOUND2 = SQRT( (1D0-C1)/(1D0+C1)*(1D0-C4)/(1D0+C4) )
      ELSE
        BOUND2 = 1D0
      ENDIF
      X0    = MIN( BOUND1 , BOUND2 )
      X0    = MAX( X0 , EP0+EM0 - 1D0 )
      ACOLMX= ACOL *PI/180D0
      IF ( ACOL.LE.0D0 .OR. ACOL.GT.180D0 ) ACOLMX = PI
      BETALL= 2D0*ALFA/PI*( LOG(SCM/RMASS2(1)) - 1D0 )
      IF(IFERM.NE.1) THEN
        IF ( IFERM .NE. 0 ) THEN
          BETALF = QF(IFERM)**2*2D0*ALFA/PI*(LOG(S/RMASS2(IFERM))-1D0)
        ELSE
          BETALF = 0D0
        ENDIF
        ISAV3  = ICHANN
        ICHANN = 1
      ELSE
        BETALF = BETALL
      ENDIF
      X0 = MAX( X0 , 4D0*RMASS2(IFERM)/SCM )
      X1MAX = 1D0 - X0
*
*     See to it that the user that asks for nonsense doesn't get it.
      IF((THETA1.LT.10D0.OR.THETA3.LT.10D0) .AND. ICHANN.NE.1) THEN
        WRITE(*,'(A,A)') ' BHABHA: This program is not fit for',
     +                   ' low angle t channel scattering !'
        RESULT = 0D0
        ERROR  = 0D0
        RETURN
      ENDIF
      IF(BIGEP0 .GE. EBEAM .OR. BIGEM0 .GE. EBEAM) THEN
        WRITE(*,'(A,A)') ' BHABHA: Nonsensical energy cut entered.',
     +                   ' RESULT = 0 !'
        RESULT = 0D0
        ERROR  = 0D0
        RETURN
      ENDIF
      IF(IFERM .LT. 0 .OR. IFERM .GT. NRMASS) THEN
        WRITE(*,'(A,I3,A)') ' BHABHA: IFERM =',IFERM,
     +                      ', which is not valid. RESULT = 0 !'
        RESULT = 0D0
        ERROR  = 0D0
        RETURN
      ENDIF
      IF ( IFERM .EQ. 0 ) THEN
        WRITE(*,'(/,A,A,/)')' Neutrino production is calculated',
     +    ' without the t-channel (1 species only) !'
      ENDIF
      IF (IFERM.EQ.NRMASS+1.OR.IFERM.EQ.8) THEN
        WRITE(*,'(/,A,A,I3)')' Terribly sorry, but this program is',
     +      ' not adapted for the choice IFERM =',IFERM
        WRITE(*,'(A,/)')' RESULT = 0 !'
        RESULT = 0D0
        ERROR  = 0D0
        RETURN
      ENDIF
      IF ( THETA1 .GE. THETA2 ) THEN
        WRITE(*,'(/,A,/)')' Empty angular region! RESULT = 0 !'
        RESULT = 0D0
        ERROR  = 0D0
        RETURN
      ENDIF
      IF ( ACOLMX .LE. 1D-3 ) THEN
        WRITE(*,'(/,A,/)')' Acollinearity cut too tight! RESULT = 0 !'
        RESULT = 0D0
        ERROR  = 0D0
        RETURN
      ENDIF
      IWRONG = 0
      IF(IWEAK .LT.0 .OR. IWEAK .GT.1) THEN
        IWEAK  = 1
        IWRONG = 1
      ENDIF
      IF(IORDER.LT.0 .OR. IORDER.GT.4) THEN
        IORDER = 4
        IWRONG = 1
      ENDIF
      IF(IFINAL.LT.0 .OR. IFINAL.GT.2) THEN
        IFINAL = 2
        IWRONG = 1
      ENDIF
      IF(NONLOG.LT.0 .OR. NONLOG.GT.1) THEN
        NONLOG = 0
        IWRONG = 1
      ENDIF
      IF(IWRONG.EQ.1) WRITE(IOUT,20)
   20 FORMAT(/,' =======> The flag setting has been changed, for',
     + ' non-valid values occurred.',/,10X,'Please check the new',
     + ' setting.',/)
      IF(IRESET.EQ.1) WRITE(IOUT,25)
   25 FORMAT(/,' =======> The angular range has been altered, in ',
     + 'order to satisfy ',/,10X,'a built-in assumption.',/,10X,
     + 'Please check the new setting and see the comment in ',
     + 'routine BHABHA.',/)
*
*     Write options, if first call since INITBH or if options changed.
      IWRITE=ABS(IBH1-IWEAK )+ABS(IBH2-ICHANN)+ABS(IBH3-IORDER)+
     +       ABS(IBH4-IFINAL)+ABS(IBH5-NONLOG)+ABS(IBH6-IFERM )+IWRONG
      IF (IWRITE.GT.0) THEN
        IBH1 = IWEAK
        IBH2 = ICHANN
        IBH3 = IORDER
        IBH4 = IFINAL
        IBH5 = NONLOG
        IBH6 = IFERM
        WRITE(IOUT,30)IWEAK,IORDER,IFINAL,NONLOG,ICHANN,IFERM
   30   FORMAT(/,' The following options have been chosen:',/,
     +         ' iweak =',I2,',   iorder =',I2,',   ifinal =',I2,
     +         ',   nonlog =',I2,',   ichann =',I2,',   iferm =',I2)
        IF(IFERM.EQ.9) WRITE(IOUT,'(/,1X,A,/,1X,A,/)')
     +  ' Bottom pair production has been chosen. Please be aware',
     +  ' that masses have been neglected.'
      ENDIF
      EPS   = 1D-5
      RLOG  = LOG( EPS )
      IF(IORDER .LT. 3) THEN
        X1MIN = RLOG
        X1MAX = LOG(X1MAX)
      ELSE
        X1MIN = 0D0
        X1MAX = X1MAX**(BETALL)
      ENDIF
*
*     Let's be nice and confirm the input to the user.
      WRITE(IOUT,40) EBEAM,2D0*EBEAM,BETALL,ACOLMX*180D0/PI,
     + EP0*EBEAM,EP0,EM0*EBEAM,EM0,TH1,C1,TH2,C2,TH3,-C3,TH4,-C4,X0
   40 FORMAT(/,'   Beam energy =',F9.4,' GeV,   <-->  sqrt(s) =',
     1 F11.6,' GeV',/,'          beta = ',F10.6,/,' Acollinearity <',
     2 F9.4,' deg.',/,'  Energy f-bar >',F9.4,' GeV  -->   cut/Ebeam ='
     3 ,F11.6,/,'  Energy   f   >',F9.4,' GeV  -->   cut/Ebeam ='
     4 ,F11.6,/,'   Angle f-bar >',F9.4,' deg. --> cos(theta1) = ',
     5  F10.6,/,'   Angle f-bar <',F9.4,' deg. --> cos(theta2) = ',
     6  F10.6,/,'   Angle   f   >',F9.4,' deg. --> cos(theta3) = ',
     7  F10.6,/,'   Angle   f   <',F9.4,' deg. --> cos(theta4) = ',
     8  F10.6,/,9X,'--> a priori cut on the x integration = ',F10.6,/)
*
* Now the actual calculation starts ...
* There are a lot of branches, different contributions, ...
* First Born and the most 'elementary' weak and QED corrections.
      BETAL1 = BETALL + 2D0*ALFA/PI
      BORN   = SIGHAT(0D0,0D0)
      ACC1   = 1D-5
      ISAVFI = IFINAL
*     If no higher than 1st order is asked for, avoid ini*fin corr.
      IF(IORDER.EQ.1.AND.IFINAL.EQ.1)  THEN
        IFINAL = 0
        FINEXT = BORN - SIGHAT(0D0,0D0)
      ENDIF
      IF(IORDER .GT. 0) HARD = DGAUSS(SIGFLX,X1MIN,X1MAX,ACC1)
      IF(IORDER .LT. 3 .AND. IORDER.GT.0) THEN
*       Non-exponentiated; hard has been done; need to do V/S part.
        FLUX = 1D0 + .5D0*BETALL*( 2D0*RLOG ) + .5D0*BETAL1*( 1.5D0 )
        IF(IORDER.EQ.2) FLUX = FLUX + .25D0*BETALL**2*( 2D0*RLOG*RLOG
     +          + 3D0*RLOG ) + .25D0*BETAL1**2*( 9D0/8D0 - PI*PI/3D0 )
        SOFT = FLUX*BORN
      ELSEIF(IORDER .GE. 3) THEN
*       Exponentiated; we did the 'soft distribution' so far, so now
*       we need to do the pure hard part.
        X1MIN  = RLOG
        X1MAX  = LOG( 1D0 - X0 )
        IORDER = IORDER + 2
        SOFT   = HARD
        HARD   = DGAUSS(SIGFLX,X1MIN,X1MAX,ACC1)
        IORDER = IORDER - 2
      ELSE
        SOFT = BORN
        HARD = 0D0
      ENDIF
      IF(IFINAL.NE.ISAVFI) THEN
        IFINAL = ISAVFI
        SOFT = SOFT + FINEXT
      ENDIF
*
      WEAKT = 0D0
      IF(IWEAK.EQ.1) THEN
*       The t dependence of the weak corrections to Born that we
*       didn't calculate exactly so far - do it now.
        IHELP  = IFINAL
        IFINAL = 0
        HELP   = SIGHAT(0D0,0D0)
        COS1   = C1
        COS2   = C2
        IF(-C3.LT.C1) COS1 = - C3
        IF(-C4.GT.C2) COS2 = - C4
        WEAKT  = DGAUSS(EEEEW,COS2,COS1,ACC1) - HELP
        IFINAL = IHELP
      ENDIF
*
      FIN2 = 0D0
      IF(IFINAL.EQ.2.AND.(EP0.GT.1D-3.OR.EM0.GT.1D-3)) THEN
*       Second order LL final state corrections.
        IHELP  = IFINAL
        IFINAL = 0
        HELP   = SIGHAT(0D0,0D0)
        FIN2   = HELP * FINAL2()
        IFINAL = IHELP
      ENDIF
*
      IF ( IORDER .EQ. 2 .OR. IORDER .EQ. 4 ) THEN
*       The part that does not exponentiate, and where both x1 and x2
*       are different from 1. Only non zero at the second and higher
*       order QED. This part is rather small, therefore somewhat less
*       fractional accuracy is needed.
*       VEGAS is used to do the 2-dimensional integration.
        ISAV1 = IWEAK
        ISAV2 = IFINAL
        IWEAK = 0
        IFINAL= 0
        ACC   = 1D-3
        XL(1) = EPS
        X0    = RMASS(IFERM)/EBEAM
        XU(1) = 1D0 - X0
        XL(2) = XL(1)
        XU(2) = XU(1)
        NDIM  = 2
        ITMX  = 5
        NCALL = 5000*4
        NPRN  = 0
        CALL VEGAS(TWOHRD,RES,ERRMC,CHI1)
        IF(ERRMC.GT.ACC*ABS(RES)) THEN
          ITMX  = 15
          NCALL = 10000
          CALL VEGAS1(TWOHRD,RES,ERRMC,CHI1)
        ENDIF
        HARD2 = RES
        IWEAK = ISAV1
        IFINAL= ISAV2
      ELSE
        HARD2 = 0D0
        ERRMC = 0D0
        CHI1  = 0D0
      ENDIF
*
      IF( NONLOG .EQ. 1 ) THEN
*       Now take care of the first order non-log terms by explicitly
*       integrating over the exact matrix element minus the collinear
*       approximation. Integration by VEGAS again, this time 5 dim.
        ISAV1 = IWEAK
        IWEAK = 0
        ACC   = 1D-3
        EPS2  = 1D-3
*       First however calculate the V/S non log contrs. (1 dim integr)
        COS1  = C1
        COS2  = C2
        IF(-C3.LT.C1) COS1 = - C3
        IF(-C4.GT.C2) COS2 = - C4
        RNLLVS= DGAUSS(EEEEVS,COS2,COS1,ACC)
*       Now the hard part (5 dimensional integral):
*       This part is a 5 dim. Monte Carlo integral (but negative
*       weights are possible, so events with unity weight can not be
*       constructed). As this is a Monte Carlo calculation, do expect
*       this to be very slow indeed.
*       Initialize incoming beam momenta:
        PP(0) = EBEAM
        PP(1) = 0D0
        PP(2) = 0D0
        PP(3) = SQRT( PP(0)**2 - RMASS2(1) )
        PM(0) =   PP(0)
        PM(1) = - PP(1)
        PM(2) = - PP(2)
        PM(3) = - PP(3)
*       Upper and lower boundaries, the variables are k0,
*       Omega(k w.r.t. p+) and Omega(q+ w.r.t. k).
        XL(1) = LOG( EPS2 * EBEAM )
        XU(1) = LOG(        EBEAM*( 1D0 - RMASS2(IFERM)/EBEAM**2 ) )
        XL(2) = - 1D0
        XU(2) =   1D0
        XL(3) =   0D0
        XU(3) =   2D0*PI
        XL(4) = - 1D0
        XU(4) =   1D0
        XL(5) =   0D0
        XU(5) =   2D0*PI
*       And now for the integration parameters and the actual integral.
        NPRN  = 0
        NDIM  = 5
        ITMX  = 5
        NCALL = 5000*4
        RES1  = 0D0
        ERR21 = 0D0
        CHI2  = 0D0
        CALL HISTO(1,0,0D0,0D0,0D0,0D0,1,' ',IOUT,1)
        IF ( ICHANN.NE.1 .OR. (IFINAL+IORDER).NE.0 ) THEN
          CALL VEGAS(RNONLG,RES1,ERR21,CHI2)
          IF(ERR21.GT.ACC*ABS(RES1)) THEN
            NPRN  = 0
            ITMX  = 5
            NCALL = 30000*4
            CALL HISTO(1,0,0D0,0D0,0D0,0D0,1,' ',IOUT,1)
            CALL VEGAS1(RNONLG,RES1,ERR21,CHI2)
          ENDIF
        ENDIF
*
*       Printing of histograms to see a few distributions for the
*       non log contributions. Can be turned on/off by the user,
*       but only together with the calls to HISTO in RNONLG.
*        X = FLOAT(NCALL)
*        CALL HISTO( 1,7, X ,0D0,0D0, 0D0,1,' ',6,1)
*        CALL HISTO( 1,2,0D0,0D0,0D0,10D0,1,' k(0)       ',IOUT,10)
*        CALL HISTO( 2,2,0D0,0D0,0D0,10D0,1,' cos q+     ',IOUT,10)
*        CALL HISTO( 3,2,0D0,0D0,0D0,10D0,1,' cos q-     ',IOUT,10)
*        CALL HISTO( 4,2,0D0,0D0,0D0,10D0,1,' cos k      ',IOUT,10)
*        CALL HISTO( 5,2,0D0,0D0,0D0,10D0,1,' cos(q+,k)  ',IOUT,10)
*        CALL HISTO( 6,2,0D0,0D0,0D0,10D0,1,' cos(q-,k)  ',IOUT,10)
*        CALL HISTO( 7,2,0D0,0D0,0D0,10D0,1,' cos(q+,q-) ',IOUT,10)
*        CALL HISTO( 8,2,0D0,0D0,0D0,10D0,1,' q+(0)      ',IOUT,10)
*        CALL HISTO( 9,2,0D0,0D0,0D0,10D0,1,' q-(0)      ',IOUT,10)
*        CALL HISTO(10,2,0D0,0D0,0D0,10D0,1,' event check',IOUT,10)
*
        RNLL  = RES1
        ERR2  = ERR21
        IWEAK = ISAV1
      ELSE
        RNLL = 0D0
        ERR2 = 0D0
        CHI2 = 0D0
        EPS2 = 1D-3
        RNLLVS = 0D0
      ENDIF
*
      IF(IFERM.NE.1) ICHANN = ISAV3
      IF(IFERM.GT.3) THEN
*       For quarks: multiply with the number of colours and with a
*       QCD correction factor due to final state QCD corr.
        COLOUR = 3D0*( 1D0 + FACQCD )
        BORN   = COLOUR * BORN
        SOFT   = COLOUR * SOFT
        HARD   = COLOUR * HARD
        HARD2  = COLOUR * HARD2
        RNLL   = COLOUR * RNLL
        RNLLVS = COLOUR * RNLLVS
        WEAKT  = COLOUR * WEAKT
        FIN2   = COLOUR * FIN2
        ERRMC  = COLOUR * ERRMC
        ERR2   = COLOUR * ERR2
      ENDIF
*
* The result!
      RESULT = SOFT + HARD + HARD2 + RNLL + RNLLVS + WEAKT + FIN2
      ERROR  = ACC1*RESULT + ERRMC + ERR2
*
* Print the results. Can also be turned off if boring.
      RESLL = SOFT + HARD + HARD2 + WEAKT + FIN2
      WRITE(IOUT,100) BORN,SOFT-BORN,HARD,RNLLVS,RNLL,ERR2,CHI2,
     + HARD2,ERRMC,CHI1,WEAKT,FIN2,RESLL,RNLLVS+RNLL,ERR2,RESULT
  100 FORMAT(' The ''Born'' contribution:      ',F15.3,/,
     1 ' The  V+S LL contribution:     ',F15.3,/,
     2 ' The hard LL contribution:     ',F15.3,/,
     3 ' The  V+S non log contribution:',F15.3,/,' The hard non log',
     4 ' contribution:',F15.3,' +- ',F15.3,' (',G11.4,')',/,
     5 ' The x1,x2 part:',15X,F15.3,' +- ',F15.3,' (',G11.4,')',/,
     6 ' The t dependence of weak corr:',F15.3,/,
     7 ' The 2nd order final st. corr.:',F15.3,/,
     8 ' The total cross section in LLA',F15.3,/,
     9 ' The total non log contribution',F15.3,' +- ',F15.3,//,
     1 ' The total cross section =     ',F15.3,' picobarn')
*
      IF(ABS(RNLLVS+RNLL).GT.2D-1*ABS(BORN)) WRITE(IOUT,110)
  110 FORMAT(' The non-log terms are quite large !',/,' Be careful',
     +       ' with the interpretation of this result.',/,
     +       ' Contact the authors ?')
      END

      SUBROUTINE AFBASY(S,THETA,BIGEP0,BIGEM0,ACOL,RESULT,ERROR)
*     -----------------
* Example 1 of a subroutine to calculate the forward backward
* asymmetry: the integral from THETA to 90 minus the integral
* from 90 to 180-THETA.
* NOTE: in the integral from THETA to 90  ONLY  the outgoing
*       positron is required to scatter between THETA and 90 off
*       its original direction. Analogously for 90 -> 180-THETA.
* For the meaning of the other parameters in the argument list, see
* the comments in subroutine BHABHA. For the meaning of the
* flags that govern which corrections are taken into account,
* see the comments in subroutine INITBH.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      CALL BHABHA(S,THETA,   90D0    ,0D0,180D0,BIGEP0,BIGEM0,ACOL,
     +                                                   RESF,ERRF)
      CALL BHABHA(S, 90D0,180D0-THETA,0D0,180D0,BIGEP0,BIGEM0,ACOL,
     +                                                   RESB,ERRB)
      RESULT = 0D0
      ERROR  = 0D0
      TOTAL = RESF + RESB
      IF( ABS(TOTAL) .GT. 0D0 ) THEN
        RESULT = ( RESF - RESB )/TOTAL
        ERROR  = ( ERRF + ERRB )/TOTAL
      ENDIF
*     QCD correction factor for quarks:
      IF(IFERM.GT.3) RESULT = RESULT*( 1D0 - FACQCD )
      WRITE(IOUT,100) RESULT,ERROR
  100 FORMAT(/,' Afb (definition 1) = ',F15.5,' +- ',F15.5,/)
      END

      SUBROUTINE AFBSYM(S,THETA,BIGEP0,BIGEM0,ACOL,RESULT,ERROR)
*     -----------------
* Example 2 of a subroutine to calculate the forward backward
* asymmetry: the integral from THETA to 90 minus the integral
* from 90 to 180-THETA.
* NOTE: in the integral from THETA to 90  BOTH  outgoing fermions
*       are required to scatter between THETA and 90 degrees
*       off their original direction. Analogously for 90 -> 180-THETA.
* For the meaning of the other parameters in the argument list, see
* the comments in subroutine BHABHA. For the meaning of the
* flags that govern which corrections are taken into account,
* see the comments in subroutine INITBH.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      CALL BHABHA(S,THETA,90D0,THETA,90D0,BIGEP0,BIGEM0,ACOL,
     +                                             RESF,ERRF)
      CALL BHABHA(S,90D0,180D0-THETA,90D0,180D0-THETA,BIGEP0,BIGEM0,
     +                                               ACOL,RESB,ERRB)
      RESULT = 0D0
      ERROR  = 0D0
      TOTAL = RESF + RESB
      IF( ABS(TOTAL) .GT. 0D0 ) THEN
        RESULT = ( RESF - RESB )/TOTAL
        ERROR  = ( ERRF + ERRB )/TOTAL
      ENDIF
*     QCD correction factor for quarks:
      IF(IFERM.GT.3) RESULT = RESULT*( 1D0 - FACQCD )
      WRITE(IOUT,100) RESULT,ERROR
  100 FORMAT(/,' Afb (definition 2) = ',F15.5,' +- ',F15.5,/)
      END

      SUBROUTINE INITBH(XMZ,XMH,XMT)
*     -----------------
* Initialization routine, needed when using the routine BHABHA.
* The parameters are the masses of the Z, Higgs and top in GeV.
* All variables are input.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ FORMMZ /FZVMZ(0:NRMASS),FZAMZ(0:NRMASS),
     +                FGVMZ(0:NRMASS),FGAMZ(0:NRMASS)
      COMPLEX*16 FZVMZ,FZAMZ,FGVMZ,FGAMZ
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BOOKKP / IBH1,IBH2,IBH3,IBH4,IBH5,IBH6
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      REAL*8 IMSIGZ,IMZ2
      CHARACTER*13 NAMES(0:NRMASS+1)
      DATA NAMES /'     neutrino','     electron','         muon',
     +            '          tau','     up quark','   down quark',
     +            '  charm quark','strange quark',
     +            '    top quark',' bottom quark','      hadrons'/
      RMZ    = XMZ
      RMH    = XMH
      RMT    = XMT
*
* A few options that can be set by the user. These determine what is
* calculated and what is not.
* IWEAK  : 0/1 switches weak (non-QED) corrections off/on.
* IORDER : the order of the initial state LL QED corrections. Valid are
*          0: no initial state LL corrections
*          1: first  order initial state LL corrections
*          2: second order initial state LL corrections
*          3: first  order initial state LL corrections+exponentiation
*          4: second order initial state LL corrections+exponentiation
*             For s-channel processes IORDER=0 also switches off the
*             non-log terms from the initial state (when t-channel is
*             included 'initial state radiation' is not a gauge
*             invariant subset any more).
* IFINAL : the order of the final state LL QED corrections. Valid are
*          0: no final state LL corrections
*          1: first  order final state LL corrections
*          2: second order final state LL corrections
*              For s-channel processes IFINAL=0 also switches off the
*              non-log terms from the final state (when t-channel is
*              included 'final state radiation' is not a gauge
*              invariant subset any more).
* NONLOG : 0/1 switches off/on: the calculation of the first order
*              non-log correction terms (SLOW! [ = 5 dim. int. ! ]).
*              It is NOT possible to switch off parts of the
*              non log terms (like final state e.g.) in the case of
*              Bhabha scattering.
* ( "Best     choice": iweak=1, iorder=4, ifinal=2, nonlog=1 (SLOW!),
*   "Best LLA choice": iweak=1, iorder=4, ifinal=2, nonlog=0        )
*
* These options can be altered after calling INITBH, by picking
* up the common block OPTION and change them elsewhere; another call
* to INITBH is NOT needed then. Upon changing Z, Higgs or top mass,
* however, INITBH has to be called again!
*
      IWEAK  = 1
      IORDER = 4
      IFINAL = 2
      NONLOG = 1
*
* IOUT  : the unit nr where the output goes to (6=screen).
* ICHANN: enables calculating only certain channels:
*         =0 calculates s + t channels plus interference,
*         =1 calculates s channel only,
*         =2 calculates t channel only,
*         The option =3 calculates only s-t interference for the
*         LL part but the s+t total for the non log part. To put
*         it simply: this option is not implemented very well.
* IFERM : final state fermion label. IFERM=1 means Bhabha scattering,
*         for the other ones see the data statement. The choices
*         8 (top) and 10 (hadrons) are not allowed. Neutrino
*         production can be calculated in s channel only, for one
*         species only.
*
      IOUT   = 6
      ICHANN = 0
      IFERM  = 1
*
* From here on: NOT user settable
*
!WP   ALFAS  = .12D0
      ALFAS = 0.124
* ALFA and HBARC2 from Particle Data Group Publ. 1990.
      ALFA   = 1D0 / 137.0359895D0
      HBARC2 = 3.8937966D8
      PI = 4D0*DATAN(1D0)
      EE = SQRT( 4D0*PI*ALFA )
* QCD and QED correction factors
      FACQCD = ALFAS/PI + (ALFAS/PI)**2*( 1.98D0 - 5D0*.115D0 )
      FACQCB = 0.045D0
      IF (ALFAS .LE. 0D0) THEN
        FACQCB = 0D0
      END IF
      FACQED = 3D0*ALFA/4D0/PI
* Starting value for sin**2(theta-w)
      SIN2TH = .2310D0
      RMW = RMZ*DSQRT( 1D0 - SIN2TH )
* Iterate to find the value for sin**2(theta-w) and Mw
* After this all couplings and renormalization constants are defined.
      NITER = 20
      CALL COUPLS(SIN2TH,RMT)
      DO 110 I = 1 , NITER
        RMWOLD = RMW
        CALL RNORM()
        CALL COUPLS(SIN2TH,RMT)
        IF(DABS(RMWOLD-RMW)/RMW .LT. 1D-6) GOTO 130
  110 CONTINUE
      WRITE(*,120) NITER
  120 FORMAT(' The calculation of MW does not converge in',I4,' steps')
      STOP' We stop right here !'
  130 CONTINUE
*
* Echo the given input and write all calculated parameters.
      WRITE(IOUT,135)
  135 FORMAT(/,' Thank you for flying .... choosing A L I B A B A',/,
     + ' ',35X,'=============',//,' A (semi) Analytical Leading log',
     + ' Improved BhABhA scattering calculation.',/,' This program is',
     + ' meant for large angle Bhabha scattering [and for other',/,
     + ' fermion pair production (but then only in s channel)].')
      WRITE(IOUT,140)
  140 FORMAT(/,' ',67('*'),/,
     1 ' * Authors: W.J.P. Beenakker, F.A. Berends and S.C.',
     2 ' van der Marck. *',/,' * Address: Instituut-Lorentz, ',
     3 'University of Leiden',16X,'*',/,' * ',9X,
     4 'P.o.b. 9506, 2300 RA Leiden, The Netherlands',11X,'*',/,
     5 ' * Bitnet addresses: BEENAKKER @ HLERUL59',26X,'*',/,
     6 ' * ',18X,'BERENDS @ HLERUL5 or BERENDS @ HLERUL59',7X,'*',/,
     7 ' * ',18X,'VANDERMARCK @ HLERUL59',24X,'*',/,
     8 ' *',19X,'joint address:',' LORENTZ @ HLERUL5',14X,'*',/,
     9 ' ',67('*'))
      WRITE(IOUT,145)
  145 FORMAT(' * References:',53X,'*',/,' * [1] W. Beenakker, F.A.',
     1 ' Berends and S.C. van der Marck,',10X,'*',/,
     2 ' *',5X,'"Large angle Bhabha scattering" and "Higher order',
     3 11X,'*',/,' *',5X,'corrections to the forward-backward',
     4 ' asymmetry,"',13X,'*',/
     5 ' *',5X,'Leiden preprints 1990, for the treatment of the',
     6 ' purely',6X,'*',/,' *',5X,'QED corrections and the',
     7 ' incorporation of cuts on energy and *',/,' *',5X,'angle',
     8 ' of both outgoing particles and their acollinearity.   *',/,
     9 ' * [2] W. Beenakker and W. Hollik, ECFA workshop on LEP 200,',
     1 7X,'*',/,' *',8X,'CERN 87-08 p.185, ed. by A. Boehm and W.',
     2 ' Hoogland;',7X,'*',/,' *',5X,'W. Hollik,',
     3 ' DESY preprint 88-188, both for the treatment',5X,'*',/,
     4 ' *',5X,'of the weak (non-QED) corrections.',26X,'*',/,
     5 ' ',67('*'),/,' Version 2.0, August 1990')
*
* Differences with version 1.0:
*  1) Integration from theta1 to theta2 (and theta3->theta4)
*     instead of theta->180-theta. Therefore Afb calculation
*     is possible with version 2.0.
*  2) Different energy cuts for e+ and e- available.
*  3) ZZ and WW boxes added.
*  4) Technicalities: Labda2 'stable' for x<1d-6.
*                     EEEEW s-t separation fixed.
*                     Erroneous check on acol cut left out.
*
      WRITE(IOUT,150) (I,NAMES(I),RMASS(I),PWIDTH(I),I=0,NRMASS)
  150 FORMAT(/,' The properties of the fermions:',/,3X,
     +       ' label',7X,'name',4X,' mass (GeV)',
     +       '  partial width of the Z (GeV)',/,
     +       (' ',I6,1X,A13,1X,F12.7,8X,F12.7))
      WRITE(IOUT,'(1X,I6,1X,A13,21X,F12.7)')NRMASS+1,'      hadrons',
     +              PWIDTH(4)+PWIDTH(5)+PWIDTH(6)+PWIDTH(7)+PWIDTH(9)
      Z = RMZ**2
      CALL FORMFS(Z,9)
      DO 160 I = 0 , NRMASS
         FZVMZ(I) = FZV(I)
         FZAMZ(I) = FZA(I)
         FGVMZ(I) = FGV(I)
         FGAMZ(I) = FGA(I)
  160 CONTINUE
      ZWID = (IMSIGZ(Z)+IMZ2(Z))/RMZ/(1D0+BIGPIZ(Z))
      WRITE(IOUT,170) RMZ,ZWID,RMW,SIN2TH,RMH
  170 FORMAT(/,' For the bosons we have (everything in GeV):',/,
     + '  mass of the   Z   =',F10.4,
     + '    total width of the Z = ',F10.7,/,
     + '  mass of the   W   =',F10.4,
     + '    <==> sin**2(theta-w) = ',F10.7,/,
     + '  mass of the Higgs =',F10.4,/)
      WRITE(IOUT,180) 1D0/ALFA,1D0+FACQED,ALFAS,1D0+FACQCD
  180 FORMAT(' Some coupling strengths:',/,
     +       '                    1/alfa = ',F10.3,/,
     +       ' the QED correction factor = ',F14.7,/,
     +       '               alfa-strong = ',F10.3,/,
     +       ' the QCD correction factor = ',F14.7,/)
* Don't forget to initialize the random number generator.
!WP   CALL RMARIN(2125,3106,IOUT)
* Bookkeeping
      IBH1 = IWEAK+10
      IBH2 = ICHANN
      IBH3 = IORDER
      IBH4 = IFINAL
      IBH5 = NONLOG
      IBH6 = IFERM
      ISIGH = 0
      ISIGF = 0
      ITWOH = 0
      IZBR  = 0
      IRNON = 0
      IEVS  = 0
      IEEW  = 0
      END

      SUBROUTINE ENDBH()
*     ----------------
* Print a few statistics. May give a clue on bottlenecks in the program
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      WRITE(IOUT,10)   ISIGH,IEVS,ISIGF,IEEW,ITWOH,IZBR,IRNON
   10 FORMAT(/,' The following subroutines have been called ',
     +  'the following number of times:',/,'      SIGMAH  ',I15,
     +  10X,' EEEEVS  ',I15,/,'      SIGFLX  ',I15,
     +  10X,' EEEEW   ',I15,/,'      TWOHRD  ',I15,
     +  10X,' ZBRENT  ',I15,/,'      RNONLG  ',I15)
      END

      FUNCTION SIGHAT(X11,X21)
*     ---------------
* The part of SIGMAH that is symmetric in x1,x2. Under the condition
* that the same cuts are applied for both particles, SIGMAH already is
* symmetric.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      IF ( ABS(C1+C3).LT.1D-6 .AND. ABS(C2+C4).LT.1D-6 .AND.
     +     ABS(EP0-EM0).LT.1D-3 ) THEN
        SIGHAT = SIGMAH(X11,X21)
      ELSE
        SIGHAT = .5D0*( SIGMAH(X11,X21) + SIGMAH(X21,X11) )
      ENDIF
      END

      FUNCTION SIGMAH(X11,X21)
*     ---------------
* The Born cross section for Bhabha scattering at reduced beam
* energies: p+ --> (1-x11)p+  ;  p- --> (1-x21)p- .
* This function includes weak corrections (but not the t dependence
* of them) if asked for. It also includes final state corrections
* if asked for.
* W. Beenakker and S.C. van der Marck, June, 1990.
* Adapted for version 2.0 (four angular cuts): July 1990.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ SAVEX  / X1SAVE, X2SAVE
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMPLEX*16 PROPG,PROPZ,MIXING,PROPGT,PROPZT,MIXINT,HADRQQ
      COMPLEX*16 PPS,PMS,ZPS,ZMS,PPSC,PMSC,ZPSC,ZMSC
      COMPLEX*16 PPT,PMT,ZPT,ZMT,PPTC,PMTC,ZPTC,ZMTC
      COMPLEX*16 PPS2,PMS2,ZPS2,ZMS2
      COMPLEX*16 PPS1,PMS1,ZPS1,ZMS1,PPSC1,PMSC1,ZPSC1,ZMSC1,ZFERM
      REAL*8 TMIN(1:2),TMAX(1:2),TINT(0:8),IMSGGZ
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      EXTERNAL ACOLLI
      INTEGER TLOOP
      ISIGH=ISIGH+1
* preliminaries ...
      X1 = 1D0 - X11
      X2 = 1D0 - X21
      X1SAVE = X1
      X2SAVE = X2
      Z = X1 * X2
      ETA1 = 0D0
      ETA2 = 0D0
      ETA3 = 0D0
      ETA4 = 0D0
      IF(ABS(1D0-C1).GT.0D0) ETA1 = ( 1D0 + C1 )/( 1D0 - C1 )
      IF(ABS(1D0-C2).GT.0D0) ETA2 = ( 1D0 + C2 )/( 1D0 - C2 )
      IF(ABS(1D0+C3).GT.0D0) ETA3 = ( 1D0 - C3 )/( 1D0 + C3 )
      IF(ABS(1D0+C4).GT.0D0) ETA4 = ( 1D0 - C4 )/( 1D0 + C4 )
      SHAT = Z*SCM
      RMZ2 = RMZ**2
* The minimum and maximum value allowed for t, using E and c cuts.
      IF(C1*C2.GT.1D-6)C2=C1
      IF(C3*C4.GT.1D-6)C4=C3
      IF(ABS(1D0-C2).GT.0D0) THEN
        TMIN(1) = - X1*SHAT/( X1 + X2*ETA2 )
      ELSE
        TMIN(1) = 0D0
      ENDIF
      IF(ABS(1D0-C1).GT.0D0) THEN
        TMAX(1) = - X1*SHAT/( X1 + X2*ETA1 )
      ELSE
        TMAX(1) = 0D0
      ENDIF
      IF(ABS(1D0+C4).GT.0D0) THEN
        TMIN(1) = MAX( TMIN(1) , - X2*SHAT/( X2 + X1*ETA4 ) )
      ELSE
        TMIN(1) = 0D0
      ENDIF
      IF(ABS(1D0+C3).GT.0D0) THEN
        TMAX(1) = MIN( TMAX(1) , - X2*SHAT/( X2 + X1*ETA3 ) )
      ELSE
        TMAX(1) = MIN( TMAX(1) , 0D0 )
      ENDIF
      IF(X1 .GT. X2) THEN
        TMIN(1) = MAX( TMIN(1) ,  SHAT*(X1-EP0)/(X2-X1) )
        TMAX(1) = MIN( TMAX(1) , -SHAT*(X2-EM0)/(X2-X1) )
      ELSEIF(X2 .GT. X1) THEN
        TMIN(1) = MAX( TMIN(1) , -SHAT*(X2-EM0)/(X2-X1) )
        TMAX(1) = MIN( TMAX(1) ,  SHAT*(X1-EP0)/(X2-X1) )
      ENDIF
      TMIN(2) = 0D0
      TMAX(2) = 0D0
*
*     Now implement the acollinearity cut. Possibly get two t regimes.
      IT = 1
      IF(TMIN(1).LT.TMAX(1).AND.ABS(X1-X2).GT.0D0.AND.
     +   ABS(ACOLMX-PI).GT.1D-10) THEN
        TMID = - .5D0*X1*X2*SCM
        ACCUCY = 1D-5
        IF ( TMID.GT.TMIN(1) .AND. TMID.LT.TMAX(1) ) THEN
*         The symmetric point lies in the interval. Therefore
*         the acollinearity has its maximum there.
          F2 = ACOLLI(TMID)
*         If f2 is negative, the acollinearity cut is always satisfied.
          IF ( F2 .GT. 0D0 ) THEN
            F1 = ACOLLI(TMIN(1))
            F3 = ACOLLI(TMAX(1))
            IF ( F1 .GT. 0D0 .AND. F3 .GT. 0D0 ) THEN
*             No possibility to satisfy the acollinearity cut.
              IT = - 1
            ELSE
*             Two distinct t regions to be integrated over.
              IT = 2
              TMIN(2) = TMIN(1)
              TMAX(2) = TMAX(1)
              IF ( F1 .GT. 0D0 ) THEN
*               The acollinearity cut cannot be satisfied.
                TMIN(1) = TMAX(1) + 1D0
              ELSE
*               Find t value with the max. allowed acollinearity.
                IZBR    = IZBR+1
                TMAX(1) = ZBRENT(ACOLLI,TMIN(1),TMID,ACCUCY)
              ENDIF
              IF ( F3 .GT. 0D0 ) THEN
*               The acollinearity cut cannot be satisfied.
                TMIN(2) = TMAX(2) + 1D0
              ELSE
*               Find t value with the max. allowed acollinearity.
                IZBR    = IZBR+1
                TMIN(2) = ZBRENT(ACOLLI,TMID,TMAX(2),ACCUCY)
              ENDIF
            ENDIF
          ENDIF
        ELSEIF ( TMID.LT.TMIN(1) ) THEN
*         acollinearity decreases from TMIN to TMAX
          F1 = ACOLLI(TMIN(1))
          F2 = ACOLLI(TMAX(1))
          IF( F2 .LT. 0D0 ) THEN
            IF ( F1 .GT. 0D0 ) THEN
              IZBR    = IZBR+1
              TMIN(1) = ZBRENT(ACOLLI,TMIN(1),TMAX(1),ACCUCY)
            ENDIF
          ELSE
            TMIN(1) = TMAX(1) + 1D0
          ENDIF
        ELSE
*         acollinearity increases from TMIN to TMAX
          F1 = ACOLLI(TMIN(1))
          F2 = ACOLLI(TMAX(1))
          IF( F1 .LT. 0D0 ) THEN
            IF ( F2 .GT. 0D0 ) THEN
              IZBR    = IZBR+1
              TMAX(1) = ZBRENT(ACOLLI,TMIN(1),TMAX(1),ACCUCY)
            ENDIF
          ELSE
            TMIN(1) = TMAX(1) + 1D0
          ENDIF
        ENDIF
      ENDIF
*
      TOTAL = 0D0
      DO 100 TLOOP = 1 , IT
*       If the minimum is larger than the maximum, the integral
*       over t is defined as 0 !
        IF ( TMIN(TLOOP) .GT. TMAX(TLOOP) ) GOTO 100
*       Define coupling constants that incorporate vertex corrections
*       and propagator corrections. First the s channel consts.
        I=IFERM
        IF(ICHANN .NE. 2) THEN
*         First calculate propagators and vertex corrections.
          CALL GZPROP(SHAT,PROPG ,PROPZ ,MIXING)
          IF(IWEAK .NE. 0) THEN
            CALL FORMFS(SHAT,1)
            MIXING = CMPLX( SIGGZ(SHAT) , IMSGGZ(SHAT) ) / SHAT
            FZV(1) = FZV(1) + QF(1)*MIXING
            IF(I.NE.1) FZV(I) = FZV(I) + QF(I)*MIXING
            PPS = - QF(1) - FGV(1) - (       - FGA(1) )
            PMS = - QF(1) - FGV(1) + (       - FGA(1) )
            ZPS =   VF(1) + FZV(1) - ( AF(1) + FZA(1) )
            ZMS =   VF(1) + FZV(1) + ( AF(1) + FZA(1) )
            PPS2= - QF(I) - FGV(I) - (       - FGA(I) )
            PMS2= - QF(I) - FGV(I) + (       - FGA(I) )
            ZPS2=   VF(I) + FZV(I) - ( AF(I) + FZA(I) )
            ZMS2=   VF(I) + FZV(I) + ( AF(I) + FZA(I) )
          ELSE
            PPS = - QF(1)
            PMS = - QF(1)
            ZPS =   VF(1) - AF(1)
            ZMS =   VF(1) + AF(1)
            PPS2= - QF(I)
            PMS2= - QF(I)
            ZPS2=   VF(I) - AF(I)
            ZMS2=   VF(I) + AF(I)
          ENDIF
          ZINT=.5D0*(PPS*PMS2*SHAT*PROPG*CONJG( ZPS*ZMS2*SHAT*PROPZ )
     +            +  PMS*PPS2*SHAT*PROPG*CONJG( ZMS*ZPS2*SHAT*PROPZ ))
          IF(IFERM .NE. 1) THEN
            PPS1 = PPS * PMS2 * SHAT*PROPG
            PMS1 = PMS * PPS2 * SHAT*PROPG
            ZPS1 = ZPS * ZMS2 * SHAT*PROPZ
            ZMS1 = ZMS * ZPS2 * SHAT*PROPZ
            PPSC1= CONJG( PPS1 )
            PMSC1= CONJG( PMS1 )
            ZPSC1= CONJG( ZPS1 )
            ZMSC1= CONJG( ZMS1 )
            ZFERM= PPS*PPS2*SHAT*PROPG * CONJG( ZPS*ZPS2*SHAT*PROPZ )
     +           + PMS*PMS2*SHAT*PROPG * CONJG( ZMS*ZMS2*SHAT*PROPZ )
          ENDIF
          PPS = PPS * PPS2 * SHAT*PROPG
          PMS = PMS * PMS2 * SHAT*PROPG
          ZPS = ZPS * ZPS2 * SHAT*PROPZ
          ZMS = ZMS * ZMS2 * SHAT*PROPZ
        ELSE
*         Only t channel wanted.
          PPS = (0D0,0D0)
          PMS = (0D0,0D0)
          ZPS = (0D0,0D0)
          ZMS = (0D0,0D0)
          ZINT= (0D0,0D0)
        ENDIF
        PPSC = CONJG( PPS )
        PMSC = CONJG( PMS )
        ZPSC = CONJG( ZPS )
        ZMSC = CONJG( ZMS )
*       Now the t channel consts
*       Start by choosing a value for T to calculate these things at.
        T = - 2D0*SHAT*( 1D0 - C1 )/( 1D0 + Z + C1*( Z - 1D0 ) )
        IF( T.LT.TMIN(TLOOP) .OR. T.GT.TMAX(TLOOP) )
     +      T = ( TMIN(TLOOP) + TMAX(TLOOP) )/2D0
        IF(ICHANN .NE. 1) THEN
*         The t channel propagators and vertex corrections ...
          CALL GZPROP(  T ,PROPGT,PROPZT,MIXINT)
*         Incorporate the Burkhardt fit to the dispersion integral to
*         have the correct t channel photon propagator corrections
*         (Watch the minus sign: difference in definition of PI-gamma.)
          IF(IWEAK.EQ.1) PROPGT=PROPGT/( 1D0 - HADRQQ(T) - PHADPI(T) )
          IF(ABS(IWEAK) .EQ. 1) THEN
            CALL FORMFS(T,1)
            MIXINT = DCMPLX( SIGGZ(T) , IMSGGZ(T) ) /T
            FZV(1) = FZV(1) + QF(1)*MIXINT/T
            PPT = - QF(1) - FGV(1) - (       - FGA(1) )
            PMT = - QF(1) - FGV(1) + (       - FGA(1) )
            ZPT =   VF(1) + FZV(1) - ( AF(1) + FZA(1) )
            ZMT =   VF(1) + FZV(1) + ( AF(1) + FZA(1) )
          ELSE
            PPT = - QF(1)
            PMT = - QF(1)
            ZPT =   VF(1) - AF(1)
            ZMT =   VF(1) + AF(1)
          ENDIF
          PROPGT =   T  * PROPGT
          PROPZT = ( T - RMZ**2 ) * PROPZT
          Z100 =(PPT*ZPT+PMT*ZMT)*PROPGT*CONJG((PPT*ZPT+PMT*ZMT)*PROPZT)
          PPT = PPT * PPT * PROPGT
          PMT = PMT * PMT * PROPGT
          ZPT = ZPT * ZPT * PROPZT
          ZMT = ZMT * ZMT * PROPZT
        ELSE
*         Only s channel wanted.
          PPT = (0D0,0D0)
          PMT = (0D0,0D0)
          ZPT = (0D0,0D0)
          ZMT = (0D0,0D0)
          Z100= (0D0,0D0)
        ENDIF
        PPTC = CONJG( PPT )
        PMTC = CONJG( PMT )
        ZPTC = CONJG( ZPT )
        ZMTC = CONJG( ZMT )
*
*       Calculate the structures that multiply the distinct t-integrals
        IF(ICHANN .NE. 3) THEN
          IF(IFERM .EQ. 1) THEN
            W1 = ( PPS+PMS )*( PPSC+PMSC ) + ( ZPS+ZMS )*( ZPSC+ZMSC )
     +         + 2D0*( PPS*ZPSC + PMS*ZMSC + 2D0*ZINT )
          ELSE
            W1 = ZPS1*ZPSC1 + ZMS1*ZMSC1 + ZPS*ZPSC + ZMS*ZMSC +
     +           PPS1*PPSC1 + PMS1*PMSC1 + PPS*PPSC + PMS*PMSC +
     +           2D0*( ZFERM + 2D0*ZINT )
          ENDIF
          W2 = 2D0*( PPS*PPSC + PMS*PMSC ) +2D0*( ZPS*ZPSC + ZMS*ZMSC )
     +       + 4D0*( PPS*ZPSC + PMS*ZMSC )
          WA = .5D0 * W2
          W3 = 2D0*( PPT*ZPSC + PMT*ZMSC ) +2D0*( ZPS*ZPTC + ZMS*ZMTC )
     +       + 2D0*( PPS*PPTC + PMS*PMTC ) +2D0*( PPS*ZPTC + PMS*ZMTC )
          W4 = PPT*PPTC + PMT*PMTC + 2D0*( PPT*ZPTC + PMT*ZMTC )
     1       + ZPT*ZPTC + ZMT*ZMTC + 4D0*( PPS*PPTC + PMS*PMTC ) +
     2       2D0*( 2D0 + RMZ2/SHAT )*( PPS*ZPTC + PMS*ZMTC ) +
     3     ( 4D0*( PPT*ZPSC + PMT*ZMSC ) +
     4       2D0*( 2D0 + RMZ2/SHAT )*( ZPS*ZPTC + ZMS*ZMTC ) )
          W5 = - 2D0*SHAT/RMZ2*Z100        +2D0*( PPS*PPTC + PMS*PMTC )
     +       + 2D0*( PPT*PPTC + PMT*PMTC ) +2D0*( PPT*ZPSC + PMT*ZMSC )
          W6 = 2D0*SHAT/RMZ2*Z100 + 2D0*( PPS*ZPTC + PMS*ZMTC ) +
     1          ( 2D0 + RMZ2/SHAT )*2D0*( PPT*ZPTC + PMT*ZMTC ) +
     2          ( 1D0 + RMZ2/SHAT )*2D0*( ZPT*ZPTC + ZMT*ZMTC ) +
     3       (2D0+RMZ2/SHAT)*2D0*( PPS*ZPTC + PMS*ZMTC )*RMZ2/SHAT +
     4       2D0*( ZPS*ZPTC + ZMS*ZMTC )*( 1D0 + RMZ2/SHAT )**2
          W7 = ( PPT + PMT )*( PPTC + PMTC )
          W8 = ( ZPT + ZMT )*( ZPTC + ZMTC ) +
     +         ( ZPT*ZPTC + ZMT*ZMTC )*RMZ2/SHAT*( 2D0 + RMZ2/SHAT )
        ELSE
*         Only s-t interference wanted.
          W1 = 0D0
          W2 = 0D0
          WA = 0D0
          W3 = 2D0*( PPT*ZPSC + PMT*ZMSC ) +2D0*( ZPS*ZPTC + ZMS*ZMTC )
     +       + 2D0*( PPS*PPTC + PMS*PMTC ) +2D0*( PPS*ZPTC + PMS*ZMTC )
          W4 = 4D0*( PPS*PPTC + PMS*PMTC ) +
     +       2D0*( 2D0 + RMZ2/SHAT )*( PPS*ZPTC + PMS*ZMTC ) +
     +     ( 4D0*( PPT*ZPSC + PMT*ZMSC ) +
     +       2D0*( 2D0 + RMZ2/SHAT )*( ZPS*ZPTC + ZMS*ZMTC ) )
          W5 = 2D0*( PPS*PPTC + PMS*PMTC ) +2D0*( PPT*ZPSC + PMT*ZMSC )
          W6 = 2D0*( PPS*ZPTC + PMS*ZMTC ) +
     +      (2D0+RMZ2/SHAT)*2D0*( PPS*ZPTC + PMS*ZMTC )*RMZ2/SHAT +
     +       2D0*( ZPS*ZPTC + ZMS*ZMTC )*( 1D0 + RMZ2/SHAT )**2
          W7 = 0D0
          W8 = 0D0
        ENDIF
*       Calculate the T integrals,
        CALL CALINT(TINT,SHAT,TMIN(TLOOP),TMAX(TLOOP),X1,X2)
*       and add all the contributions:
        SUM = W1*TINT(0) + W2*TINT(1) + WA*TINT(2) + W3*TINT(3) +
     +        W4*TINT(4) + W5*TINT(5) + W6*TINT(6) + W7*TINT(7) +
     +        W8*TINT(8)
        SUM = 1D0/SHAT/SHAT * PI*ALFA*ALFA * SUM
        TOTAL = TOTAL + SUM
  100 CONTINUE
      SIGMAH = HBARC2 * TOTAL
      END

      FUNCTION ACOLLI(T)
*     ---------------
* Calculates the acollinearity corresponding to a " X1,X2,T event ",
* minus the maximum acollinearity ACOLMX that has been asked for.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ SAVEX  / X1SAVE, X2SAVE
      IF(ABS(T) .LE. 0D0) THEN
        ACOLLI = - ACOLMX
      ELSE
        COS3 =   1D0 - 2D0/( 1D0-X1SAVE/X2SAVE - X1SAVE*X1SAVE*SCM/T )
        COS4 = - 1D0 + 2D0/( 1D0-X2SAVE/X1SAVE - X2SAVE*X2SAVE*SCM/T )
        IF(COS3 .LT. -1D0) COS3 = -1D0
        IF(COS3 .GT.  1D0) COS3 =  1D0
        IF(COS4 .LT. -1D0) COS4 = -1D0
        IF(COS4 .GT.  1D0) COS4 =  1D0
        THETA3 = ACOS( COS3 )
        THETA4 = ACOS( COS4 )
        ACOLLI = ABS( PI - THETA3 - THETA4 ) - ACOLMX
      ENDIF
      END

      SUBROUTINE CALINT(TINT,SHAT,TMIN,TMAX,X1,X2)
*     -----------------
* The result of the integration over T from tmin to tmax is put
* into the array TINT. The integration incorporates first order final
* state QED corrections, when the flag IFINAL is set accordingly.
* W. Beenakker and S.C. van der Marck, February, 1990.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      REAL*8 TINT(0:8),LOG1,LOG2,LOG3,LOG4,LOG5
* Statement functions for the no final state corrections case
      R1(T) = 1D0/3D0*T**3/SHAT**2
      R2(T) =    .5D0*T**2/SHAT
      RA(T) = T
      R3(T) = R2(T)
      R4(T) = RA(T)
      R5(T) = SHAT*LOG( - T )
      R6(T) = SHAT*LOG( - T + RMZ2 )
      R7(T) = - SHAT**2/T
      R8(T) = - SHAT**2/( T - RMZ2 )
*
      RMZ2 = RMZ**2
      TINT(0) = R1(TMAX) - R1(TMIN)
      TINT(1) = R2(TMAX) - R2(TMIN)
      TINT(2) = RA(TMAX) - RA(TMIN)
      IF(ICHANN .NE. 1) THEN
        TINT(3) = R3(TMAX) - R3(TMIN)
        TINT(4) = R4(TMAX) - R4(TMIN)
        TINT(5) = R5(TMAX) - R5(TMIN)
        TINT(6) = R6(TMAX) - R6(TMIN)
        TINT(7) = R7(TMAX) - R7(TMIN)
        TINT(8) = R8(TMAX) - R8(TMIN)
      ELSE
*       No t channel wanted; by not calculating these integrals
*       we open up the possibility to have the angular cut go to zero.
        TINT(3) = 0D0
        TINT(4) = 0D0
        TINT(5) = 0D0
        TINT(6) = 0D0
        TINT(7) = 0D0
        TINT(8) = 0D0
      ENDIF
*
      EBEAM = .5D0*SQRT(SCM)
      IF(IFINAL.GE.1.AND.((EP0.GT.1D-3.AND.EP0.GT.RMASS(IFERM)/EBEAM)
     +  .OR.(EM0.GT.1D-3.AND.EM0.GT.RMASS(IFERM)/EBEAM))) THEN
*       Final state corrections (=0 if energy cuts EP0 = EM0 = 0)
*       i=1,2: x3 part, where (i=1 <--> tmax) and (i=2 <--> tmin)
*       i=3,4: x4 part, where (i=3 <--> tmax) and (i=4 <--> tmin)
        BETA = BETALF + 2D0*ALFA/PI*QF(IFERM)**2
        DO 10 I = 1 , 4
          IF(I .LE. 2) THEN
            IF(EP0.LE.RMASS(IFERM)/EBEAM.OR.EP0.LT.1D-3) GOTO 10
            A = X1/EP0
            B = - 1D0/EP0/SCM * (1D0-X1/X2) /X1
          ELSE
            IF(EM0.LE.RMASS(IFERM)/EBEAM.OR.EM0.LT.1D-3) GOTO 10
            A = X2/EM0
            B =   1D0/EM0/SCM * (1D0-X1/X2) /X1
          ENDIF
          IF(I .EQ. 1 .OR. I .EQ. 3) THEN
            T    = TMAX
            SIGN =   1D0
          ELSE
            T    = TMIN
            SIGN = - 1D0
          ENDIF
          S3 = 0D0
          S4 = 0D0
          S5 = 0D0
          S6 = 0D0
          S7 = 0D0
          S8 = 0D0
          T2 = T**2
          T3 = T*T2
          LOG1 = LOG( CMPLX( A + B*T           ,0D0) )
          LOG4 = (0D0,0D0)
          IF(ICHANN.NE.1)  LOG4 = LOG( CMPLX( T,0D0) )
          LOG5 = LOG( CMPLX( T - RMZ2        ,0D0) )
*         If b too small we have numerical problems, so in that case
*         calculate a form where we have expanded around b=0.
          IF(ABS(B*TMIN).GT.1D-2.OR.ABS(B*TMAX).GT.1D-2 .OR.
     +       ( ABS(A-1D0) .LT. 1D-10 .AND. ABS(B).GT.1D-10 ) ) THEN
            S1 = BETALF*( - T2/6D0/B - T/3D0/B/B*(-2D0*A+1D0) ) +
     +           BETA  *( .5D0/B*( .5D0*
     +            T2 - A/B*T + A*A/B/B*LOG1 ) + .25D0/B/B*( T -
     +            2D0*A/B*LOG1 - A*A/B/(A+B*T) ) )
            S2 = BETALF*( -T/2D0/B ) + BETA*( .5D0/B*( T - A/B*LOG1 )
     +                   +.25D0/B/B*( LOG1 + A/(A+B*T) ) )
            SA = BETA*( .5D0/B*LOG1 - .25D0/B/(A+B*T) )
            S3 = S2
            S4 = SA
            IF(ICHANN .NE. 1) THEN
              IF( ABS(A-1D0) .LT. 1D-10 ) THEN
                S5 = BETALF*(.5D0*LOG(CMPLX(B*T,0D0))**2+DILOG(-B*T))+
     +               BETA*(-.75D0*(LOG1-LOG4) + .25D0/(1D0+B*T) )
                S7 = - (BETALF+BETA)*B*LOG(CMPLX(T/(1D0/B+T),0D0)) +
     +                BETA*( -.75D0/T - .25D0*B/(1D0+B*T) )
              ELSEIF( ABS(A) .LT. .05D0 ) THEN
                S5 = BETALF*( -DILOG(B*T)-.5D0*LOG(ABS(B*T))**2 ) +
     +               BETA*( -.5D0/B/T - 1D0/8D0/B/B/T/T )
                S7 = BETA*( 1D0/T - .25D0/B/T2 - 1D0/12D0/B/B/T3 )
              ELSE
                S5 = BETALF*( LOG(CMPLX(1D0-1D0/A,0D0))*LOG4 -
     +                        DILOG(-B*T/(A-1D0)) + DILOG(-B*T/A) ) +
     +               BETA*( - .5D0/A*(1D0+.5D0/A)*( LOG1-LOG4 ) +
     +                     .25D0/A/(A+B*T) )
                S7 = ( BETA*B/A/A/2D0*(1D0+1D0/A)-BETALF*B/A/(A-1D0) )*
     +               (LOG1-LOG4) + BETA*( - .5D0/A*(1D0+.5D0/A)/T
     +                                    - B/4D0/A/A/(A+B*T) )
              ENDIF
              IF( ABS(A+B*RMZ2) .GT. 1D-2) THEN
                S6 = BETALF*(LOG(CMPLX(1D0-1D0/(A+B*RMZ2),0D0))*LOG5 -
     +              DILOG(-(T-RMZ2)/((A-1D0)/B+RMZ2)) +
     +              DILOG(-(T-RMZ2)/( A     /B+RMZ2)) ) +
     +             BETA*( .5D0/(A+B*RMZ2)*(1D0+.5D0/(A+B*RMZ2))*
     +                   (LOG5-LOG1) + .25D0/(A+B*RMZ2)/(A+B*T) )
                S8 = ( BETA*B/2D0/(A+B*RMZ2)**2*(1D0+1D0/(A+B*RMZ2))
     +               -BETALF*B/(A+B*RMZ2)/(A-1D0+B*RMZ2) )*
     +               ( LOG1 - LOG5 ) + BETA*(
     +             -.5D0/(A+B*RMZ2)*(1D0+.5D0/(A+B*RMZ2))/(T-RMZ2)-
     +              B/4D0/(A+B*RMZ2)**2/(A+B*T) )
              ELSE
                S6 = -DILOG(B*(T-RMZ2))-.5D0*LOG(ABS(B*(T-RMZ2)))**2-
     +               .5D0/B/(T-RMZ2) - 1D0/8D0/B/B/(T-RMZ2)**2
                S8 = -(1D0/(T-RMZ2)-B)*LOG1 -1D0/(T-RMZ2)
     +                -.25D0/B/(T-RMZ2)**2-1D0/12D0/B/B/(T-RMZ2)**3
              ENDIF
            ENDIF
            IF( ABS( (A+B*T)-1D0 ) .GE. 1D-6 ) THEN
*             Parts that shouldn't be evaluated at a+b*t=1 (0*log(0))
              LOG2 = LOG(CMPLX( 1D0 - 1D0/(A+B*T) ,0D0) )
              LOG3 = LOG(CMPLX( (A-1D0) + B*T     ,0D0) )
              S1 = S1 + BETALF*( 1D0/3D0*( T3 + (A/B)**3 )*LOG2 +
     +                   1D0/3D0/B**3*(-3D0*A*A+3D0*A-1D0)*LOG3 )
              S2E = BETALF*( + .5D0*( T2 - A*A/B/B )*LOG2 -
     +                    .5D0/B/B*( - 2D0*A + 1D0 )*LOG3 )
              SAE = BETALF*( + ( A/B + T )*LOG2 - 1D0/B*LOG3 )
              IF(ICHANN .NE. 1) THEN
                IF( ABS(A-1D0) .GT. 1D-10 ) THEN
                  S7 = S7 - BETALF*( ( 1D0/T + B/(A-1D0) )*LOG2 )
                ELSE
                  S7 = S7 - BETALF/T*(1D0+LOG4-LOG(CMPLX(1D0/B+T,0D0)))
                ENDIF
                IF( ABS(A+B*RMZ2) .GT. 1D-2)
     +            S8 = S8 - BETALF*(1D0/(T-RMZ2)+B/(A-1D0+B*RMZ2))*LOG2
              ENDIF
            ENDIF
            S2 = S2 + S2E
            SA = SA + SAE
            S3 = S3 + S2E
            S4 = S4 + SAE
            IF(ABS(A-1D0) .GT. 1D-10) THEN
              IF(ABS(B*TMIN/(A-1D0)) .LT. 1D-1 .AND.
     +           ABS(B*TMAX/(A-1D0)) .LT. 1D-1      ) THEN
                S1=T3/3D0*(BETALF*LOG(ABS((A-1D0)/A)) + BETA*
     +              .5D0/A*(1D0+.5D0/A)) + BETA*(
     +            - B*T2*T2*( (1D0+1D0/A)/8D0/A/A-1D0/4D0/A/(A-1D0) ) )
              ENDIF
            ELSE
              LOG1 = LOG(CMPLX(1D0/B+T,0D0))
              S1=BETALF*(T3/3D0*( LOG4 - LOG1 ) - 1D0/3D0/B**3*LOG1) +
     +           BETA*( ( T2 + T/B )/12D0/B - .25D0/B**3/(1D0+B*T) )
            ENDIF
          ELSE
            IF( ABS(A-1D0)        .GT. 1D-10 .AND.
     +          ABS(A+B*TMIN-1D0) .GT. 1D-10 .AND.
     +          ABS(A+B*TMAX-1D0) .GT. 1D-10      ) THEN
*             For S1,S2,SA we have expanded around b=0, the others
*             do not need that.
              S1 = T3/3D0*(BETALF*LOG(ABS((A-1D0)/A))+BETA*.5D0/A*
     +             (1D0+.5D0/A)) + BETA*(
     +         - B*T**4*( (1D0+1D0/A)/8D0/A/A - 1D0/4D0/A/(A-1D0) ) )
              S2 = T2/2D0*(BETALF*LOG(ABS((A-1D0)/A))+BETA*.5D0/A*
     +             (1D0+.5D0/A)) + BETA*(
     +           - B*T3*( (1D0+1D0/A)/6D0/A/A - 1D0/3D0/A/(A-1D0) ) )
              SA = T     *(BETALF*LOG(ABS((A-1D0)/A))+BETA*.5D0/A*
     +             (1D0+.5D0/A)) + BETA*(
     +           - B*T2*( (1D0+1D0/A)/4D0/A/A - 1D0/2D0/A/(A-1D0) ) )
              S3 = S2
              S4 = SA
              IF(ICHANN .NE. 1) THEN
                S5 = BETALF*( LOG(CMPLX(1D0-1D0/A,0D0))*LOG4-
     +                        DILOG(-B*T/(A-1D0)) + DILOG(-B*T/A) ) +
     +               BETA*( -.5D0/A*(1D0+.5D0/A)*( LOG1 - LOG4 ) +
     +                     .25D0/A/(A+B*T) )
                S6 = BETALF*( LOG(CMPLX(1D0-1D0/(A+B*RMZ2),0D0))*LOG5 -
     +              DILOG(-B*(T-RMZ2)/((A-1D0)+B*RMZ2)) +
     +              DILOG(-B*(T-RMZ2)/( A     +B*RMZ2)) ) + BETA*(
     +             .5D0/(A+B*RMZ2)*(1D0+.5D0/(A+B*RMZ2))*(LOG5-LOG1)
     +            + .25D0/(A+B*RMZ2)/(A+B*T) )
                S7 = ( BETA*B/A/A/2D0*(1D0+1D0/A)-BETALF*B/A/(A-1D0) )*
     +               (LOG1-LOG4) + BETA*(
     +           - .5D0/A*(1D0+.5D0/A)/T - B/4D0/A/A/(A+B*T) )
                S8 = ( BETA*B/2D0/(A+B*RMZ2)**2*(1D0+1D0/(A+B*RMZ2))
     +             -BETALF*B/(A+B*RMZ2)/(A-1D0+B*RMZ2) )*( LOG1-LOG5 )
     +             + BETA*( -.5D0/(A+B*RMZ2)*(1D0+.5D0/(A+B*RMZ2))/
     +                   (T-RMZ2)-B/4D0/(A+B*RMZ2)**2/(A+B*T) )
                LOG2 = LOG(CMPLX(1D0-1D0/(A+B*T),0D0) )
                S7 = S7 - BETALF*( 1D0/T + B/(A-1D0) )*LOG2
                S8 = S8 - BETALF*(1D0/(T-RMZ2)+B/(A-1D0+B*RMZ2))*LOG2
              ENDIF
            ELSE
              S1 = 0D0
              S2 = 0D0
              SA = 0D0
              S3 = 0D0
              S4 = 0D0
              S5 = 0D0
              S6 = 0D0
              S7 = 0D0
              S8 = 0D0
            ENDIF
          ENDIF
          S1 = S1/SHAT/SHAT
          S2 = S2/SHAT
          SA = SA
          S3 = S3/SHAT
          S4 = S4
          S5 = S5*SHAT
          S6 = S6*SHAT
          S7 = S7*SHAT*SHAT
          S8 = S8*SHAT*SHAT
          FAC = .5D0 * SIGN
          TINT(0) = TINT(0) + FAC * S1
          TINT(1) = TINT(1) + FAC * S2
          TINT(2) = TINT(2) + FAC * SA
          TINT(3) = TINT(3) + FAC * S3
          TINT(4) = TINT(4) + FAC * S4
          TINT(5) = TINT(5) + FAC * S5
          TINT(6) = TINT(6) + FAC * S6
          TINT(7) = TINT(7) + FAC * S7
          TINT(8) = TINT(8) + FAC * S8
   10   CONTINUE
      ENDIF
      END

      FUNCTION SIGFLX(Y1)
*     ---------------
* The hard part of the structure functions times the reduced cross
* section. Of the latter only the part with x1=z,x2=1 is taken.
* Here we calculate the structure function part, the reduced cross
* section is given by SIGHAT. The form of the structure functions
* depends on the required QED order at which one wants to calculate
* the corrections. This is governed by IORDER.
* This function is to be integrated over numerically over Y1
* (Caution: Y1 is not simply the energy fraction x. It differs
* due to a mapping to make the integral more smooth.)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      ISIGF=ISIGF+1
*
      BETAL1 = BETALL + 2D0*ALFA/PI
      IF(IORDER .LT. 3) THEN
        Z1  = EXP( Y1 )
        Z   = 1D0 - Z1
        PEE = BETALL*2D0 - BETAL1*( 1D0 - Z*Z )
        IF(IORDER .EQ. 2) THEN
          PEEPEE = ( 1D0 + Z*Z )*( 2D0*LOG(Z1) - LOG(Z) + 3D0/2D0 )
     +                + Z1*( + .5D0*(1D0+Z)*LOG(Z) - (1D0-Z) )
        ELSE
          PEEPEE = 0D0
        ENDIF
        FLUX = .5D0*PEE + .25D0*BETALL**2*PEEPEE
      ELSEIF(IORDER .EQ. 3 .OR. IORDER .EQ. 4) THEN
        Z1 = Y1**(1D0/BETALL)
        FLUX = 1D0 +.75D0*BETAL1
        IF(IORDER.EQ.4) FLUX=FLUX+.25D0*(9D0/8D0-PI**2/3D0)*BETAL1**2
      ELSE
        Z1  = EXP( Y1 )
        Z   = 1D0 - Z1
        PEE = Z1*( - ( 1D0 + Z ) )
        PEEPEE = Z1*( -(1D0+Z**2)/Z1*LOG(Z) + (1D0+Z)*(
     +               - 2D0*LOG(Z1) + .5D0*LOG(Z) ) - Z/2D0 - 2.5D0 )
        FLUX = .5D0*BETAL1*PEE
        IF(IORDER.EQ.6) FLUX = FLUX + .25D0*BETAL1**2*PEEPEE
      ENDIF
*
      SIGFLX = FLUX * SIGHAT(Z1,0D0)
      END

      FUNCTION TWOHRD(X,WEIGHT)
*     -------------------------
* The hard part of the structure functions times the reduced cross
* section, the part where both x1 and x2 are not equal to 1.
* Two dimensional integral needed (in this form to be done by VEGAS).
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      REAL*8 X(1:2)
      ITWOH=ITWOH+1
      Z1 = X(1)
      Y1 = X(2)
      Z  = 1D0 - Z1
      Y  = 1D0 - Y1
      PEE1 = ( 1D0 + Z*Z )/Z1
      PEE2 = ( 1D0 + Y*Y )/Y1
      FLUX = 1D0/16D0 * (BETALL+2D0*ALFA/PI)**2 * PEE1*PEE2
      TWOHRD = FLUX*( SIGHAT(Z1,Y1) - SIGHAT(Z1+Y1-Z1*Y1,0D0) )
      END

      FUNCTION FINAL2()
*     ---------------
* Second order LL final state corrections as a factor times Born.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
* This was the formula for EP0=EM0
*      RLOG1 = LOG( 1D0 - E0 )
*      RLOG2 = LOG( E0 )
*      DILO  = DILOG( 1D0 - E0 )
*      FAC = ( 2D0*RLOG1 + E0 + .5D0*E0**2 )**2 + 2D0*(
*     +      - PI**2/3D0 + 2D0*DILO + 2D0*RLOG1**2 - 4D0*(1D0-E0)*RLOG1
*     +      + 4D0*(1D0-E0) + (1D0-E0)**2*( RLOG1 - .5D0 )
*     +      - 1.5D0*E0*RLOG2 + 1.5D0*E0 - .75D0*E0**2*( RLOG2-.5D0 )
*     +      + 3D0*RLOG1 + 2.5D0*E0 + .25D0*E0**2 - 3.5D0   )
* Now for the new one:
      RLOGP1 = LOG( 1D0 - EP0 )
      RLOGM1 = LOG( 1D0 - EM0 )
      RLOGP2 = 0D0
      RLOGM2 = 0D0
      IF(EP0.GT.0D0) RLOGP2 = LOG( EP0 )
      IF(EM0.GT.0D0) RLOGM2 = LOG( EM0 )
      FAC = 2D0*DILOG(1D0-EP0) + 2D0*DILOG(1D0-EM0) - PI*PI*2D0/3D0 +
     1      2D0*( RLOGP1 + RLOGM1 )**2 + 2D0*( RLOGP1 + RLOGM1 )*
     2      ( EP0*(1D0+.5D0*EP0) + EM0*(1D0+.5D0*EM0) ) + EP0 + EM0 +
     3      1D0/8D0*( EP0**2 + EM0**2 ) + EP0*EM0*(1D0+.5D0*EP0)*
     4      (1D0+.5D0*EM0) - 1.5D0*EP0*(1D0+.5D0*EP0)*RLOGP2 -
     5                       1.5D0*EM0*(1D0+.5D0*EM0)*RLOGM2
      FINAL2 = FAC * (BETALF/4D0)**2
      END

      SUBROUTINE GZPROP(QSQR,PROPG,PROPZ,MIXING)
*     -----------------
* The gamma-Z propagators and their mixing, up to one loop corrections,
* but for the imaginary part of the Z propagator, which includes
* second order corrections.
* QSQR is input: the momentum transfer squared through the progagators.
* PROPG, PROPZ and MIXING are complex*16 output.
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 IMSIGG,IMSGGZ,IMSIGZ,IMZ2
      COMPLEX*16 Z1,Z2,Z3, PROPG,PROPZ,MIXING
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      IF(IWEAK .EQ. 1) THEN
        Z1 = DCMPLX( SIGG (QSQR) , IMSIGG(QSQR) )
        Z2 = DCMPLX( SIGZ (QSQR) , IMSIGZ(QSQR) + IMZ2(QSQR) )
        Z3 = DCMPLX( SIGGZ(QSQR) , IMSGGZ(QSQR) )
        PROPG = 1D0/( QSQR + Z1 )
        PROPZ = 1D0/( QSQR - RMZ**2 + Z2 )
        MIXING= - Z3/( QSQR*(QSQR-RMZ**2+Z2) )
      ELSE
        PROPG  = 1D0/QSQR
        IF(QSQR .GT. 0D0) THEN
          PROPZ  = 1D0/DCMPLX( QSQR-RMZ**2 , RMZ*ZWID )
        ELSE
          PROPZ  = 1D0/DCMPLX( QSQR-RMZ**2 , 0D0 )
        ENDIF
        MIXING = DCMPLX( 0D0 , 0D0 )
      ENDIF
      END

      FUNCTION EEEEVS(COSTH)
*     ---------------
* Calculation of the non-log terms of the virtual and soft corrections
* on the Born Bhabha cross section. Included are the corrections due
* to final state and initial state photons and their interference,
* and hence also box diagrams. COSTH is input and is to be integrated
* over.
* W. Beenakker and S.C. van der Marck, April 1990.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( OFFSET = 1D-10 )
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      REAL*8 MATRIX(1:6), RESEFF(1:4)
      COMPLEX*16 LABDAS,LABDAT,LABDFS,LABDFT,LE,LL,G,A, SPENCF,MZ2
      COMPLEX*16 GS,GT,ZS,ZT,MIX,GZS(-1:1,-1:1),GZT(-1:1,-1:1)
      COMPLEX*16 AGZS,VGZS,AGGS,VGGS,AGZT,VGZT,AGGT,VGGT,GDINT,HULP
* Statement functions for the box corrections
      G(SL,TL) = SL/2D0/(SL+TL)*LOG(TL/CMPLX(SL,OFFSET)) -
     +           SL*(SL+2D0*TL)/4D0/(SL+TL)**2*(
     +             LOG(TL/CMPLX(SL,OFFSET))**2 + PI*PI )
      A(SL,TL) = (SL-MZ2)/(SL+TL)*( LOG(TL/(SL-MZ2)) +
     +             MZ2/SL*LOG(1D0-SL/MZ2) + (SL+2D0*TL+MZ2)/(SL+TL)*(
     +               LOG(-TL/MZ2)*LOG((MZ2-SL)/(MZ2+TL)) +
     +               SPENCF(SL/MZ2) - SPENCF(-TL/MZ2) ) )
      IEVS=IEVS+1
*
      S = SCM
      PPQP = .25D0*S*( 1D0 - COSTH )
      PPQM = .25D0*S*( 1D0 + COSTH )
      PPPM =  .5D0*S
      T = - 2D0*PPQP
* Define propagators.
      E2   = EE**2
      QF2  = QF(1)*QF(IFERM)
      QF3  = QF(1)**2
      CALL GZPROP(S ,GS ,ZS ,MIX)
      CALL GZPROP(T ,GT ,ZT ,MIX)
      IF(ICHANN .EQ. 1) THEN
        ZT = (0D0,0D0)
        GT = (0D0,0D0)
      ELSEIF(ICHANN .EQ. 2) THEN
        ZS = (0D0,0D0)
        GS = (0D0,0D0)
      ENDIF
      I=IFERM
      DO 20 L1 = - 1 , 1 , 2
        DO 10 L2 = - 1 , 1 , 2
          GZS(L1,L2)=E2*(QF2*GS+(VF(1)-L1*AF(1))*(VF(I)-L2*AF(I))*ZS)
          GZT(L1,L2)=E2*(QF3*GT+(VF(1)-L1*AF(1))*(VF(1)-L2*AF(1))*ZT)
   10   CONTINUE
   20 CONTINUE
* Start calculating corrections
      SK = S
      U  = - S - T
      MZ2 = CMPLX( RMZ**2 , - RMZ*ZWID )
* The photonic vertex correction: initial state -------------------
      LE =   LOG( -CMPLX(S,OFFSET)/RMASS2(1) )
      LL = - LOG( -CMPLX(S,OFFSET)/SK )
      LABDAS= ALFA/2D0/PI*QF(1)**2*( 2D0*LL*( LE-1D0 ) + LE*(LE-1D0)
     +                           + 2D0*LE + 4D0*( PI**2/12D0 - 1D0 ) )
      LE =   LOG( -CMPLX(T,OFFSET)/RMASS2(1) )
      LL = - LOG( -CMPLX(T,OFFSET)/SK )
      LABDAT= ALFA/2D0/PI*QF(1)**2*( 2D0*LL*( LE-1D0 ) + LE*(LE-1D0)
     +                           + 2D0*LE + 4D0*( PI**2/12D0 - 1D0 ) )
* and final state (more precise: the other fermion current)
      LE =   LOG( -CMPLX(S,OFFSET)/RMASS2(IFERM) )
      LL = - LOG( -CMPLX(S,OFFSET)/SK )
      LABDFS= ALFA/2D0/PI*QF(IFERM)**2*( 2D0*LL*(LE-1D0) + LE*(LE-1D0)
     +                           + 2D0*LE + 4D0*( PI**2/12D0 - 1D0 ) )
      LE =   LOG( -CMPLX(T,OFFSET)/RMASS2(IFERM) )
      LL = - LOG( -CMPLX(T,OFFSET)/SK )
      LABDFT= ALFA/2D0/PI*QF(IFERM)**2*( 2D0*LL*(LE-1D0) + LE*(LE-1D0)
     +                           + 2D0*LE + 4D0*( PI**2/12D0 - 1D0 ) )
* Subtract the leading log terms, to end up with non-log terms only
      BETAL1 = BETALL + 2D0*ALFA/PI
      BETAF1 = BETALF + 2D0*ALFA/PI*QF(IFERM)**2
      LABDAS = LABDAS + LABDFS - .75D0*( BETAL1 + BETALF )
      LABDAT = LABDAT + LABDFT - .75D0*( BETAL1 + BETALF )
* The soft photon corrections ----------------------------
      XF    = - RMASS2(IFERM)/S
      XFLOG = LOG(-XF)
      XFLI2 = PI*PI/6D0
      BE    = LOG(S/RMASS2(1)) - 1D0
      BFIN  = BE
      BINT  = 0D0
      IF(ABS(ABS(COSTH)-1D0).GT.0D0) BINT=2D0*LOG(T/U)
      IF (IFERM .GT. 1) BFIN = LOG(S/RMASS2(IFERM)) - 1D0
      IF (IFERM .EQ. 0) BFIN = 0D0
      GIR = - ALFA/PI*LOG(SK/S)*( QF(1)**2*BE + QF(IFERM)**2*BFIN
     +                          + QF(1)*QF(IFERM)*BINT )
      GD = ALFA/PI*2D0*LOG(EPS2)*( QF(1)**2*BE + QF(IFERM)**2*BFIN
     +         + QF(1)*QF(IFERM)*BINT )
      GFIN =-ALFA/PI*( QF(1)**2*( PI**2/3D0 - .5D0 + .5D0*BE**2 )
     +        + QF(IFERM)**2*( XFLOG + 2D0*XFLI2 + .5D0*XFLOG**2 ) )
      IF(ABS(ABS(COSTH)-1D0).GT.0D0)
     +  GFIN = GFIN - ALFA/PI*( + 2D0*QF(1)*QF(IFERM)*(
     +              + DILOG(1D0+S/T) - DILOG(1D0+S/U) ) )
      DELSOF = GIR + GD + GFIN
* Subtract the leading log terms, to end ...
      DELSOF = DELSOF - LOG(EPS2)*( BETALL + BETALF )
* Resonance effects when soft photons are not all that soft ...
      HULP = (S-MZ2)/(S*(1D0-EPS)-MZ2)
      GDINT = ALFA/PI*2D0*( QF(1)**2*BE*LOG(HULP) +
     +        .5D0*QF(1)*QF(IFERM)*BINT*LOG(HULP) )
      GDRES = ALFA/PI*2D0*( QF(1)**2*BE*LOG(ABS(HULP)) +
     +             QF(1)*QF(IFERM)*BINT*LOG(ABS(HULP)) +
     +             QF(1)**2*BE*(S-RMZ**2)/RMZ/ZWID*(
     +          ATAN((RMZ**2-S*(1D0-EPS))/(RMZ*ZWID)) -
     +          ATAN((RMZ**2-S          )/(RMZ*ZWID)) )  )
      K = 0
      E4 = E2 * E2
      DO 40 L1 = - 1 , 1 , 2
        DO 30 LF = - 1 , 1 , 2
          K = K + 1
          RESEFF(K) = E4*
     +             DREAL((VF(1)-L1*AF(1))*(VF(IFERM)-LF*AF(IFERM))*ZS*
     +      CONJG((VF(1)-L1*AF(1))*(VF(IFERM)-LF*AF(IFERM))*ZS))*GDRES
     +           + 2D0*DREAL( QF(1)*QF(IFERM)*GS*CONJG(
     +            (VF(1)-L1*AF(1))*(VF(IFERM)-LF*AF(IFERM))*ZS))*GDINT
   30   CONTINUE
   40 CONTINUE
      RESEFF(1) = RESEFF(1) + E4*
     +           2D0*DREAL( (QF(1)*QF(1)*GT+(VF(1)+AF(1))**2*ZT)*
     +           CONJG( (VF(1)+AF(1))*(VF(1)+AF(1))*ZS ) )*GDINT
      RESEFF(4) = RESEFF(4) + E4*
     +           2D0*DREAL( (QF(1)*QF(1)*GT+(VF(1)-AF(1))**2*ZT)*
     +           CONJG( (VF(1)-AF(1))*(VF(1)-AF(1))*ZS ) )*GDINT
      ALPI = ALFA/PI * EE**2
      IF ( ICHANN .EQ. 1 ) THEN
        IF( IFINAL+IORDER .EQ. 0 ) THEN
          LABDAS = (0D0,0D0)
          DELSOF = 0D0
          ALPI   = 0D0
        ELSEIF ( IFINAL.EQ.0 ) THEN
          LABDAT = (0D0,0D0)
          LABDAS = LABDAS - ( LABDFS - .75D0*BETAF1 )
          DELSOF =-ALFA/PI*QF(1)**2*( PI**2/3D0 -.5D0 +.5D0*BE**2 )
          ALPI   = 0D0
        ELSEIF ( IORDER.EQ.0 .AND. ICHANN.EQ.1 ) THEN
          LABDAT = (0D0,0D0)
          LABDAS = LABDFS - .75D0*BETAF1
          DELSOF = - ALFA/PI*QF(IFERM)**2*
     +                  (XFLOG+2D0*XFLI2+.5D0*XFLOG**2)
          ALPI   = 0D0
        ENDIF
      ENDIF
*
* And finally the box corrections ------------------------
      AGGS = ALPI*( G(S,T) + G(S,U) )
      VGGS = ALPI*( G(S,T) - G(S,U) + 2D0*LOG(SK/CMPLX(-S,-OFFSET))*
     +                                    LOG(T/U) )
      AGZS = ALPI*( A(S,T) + A(S,U) )
      VGZS = ALPI*( A(S,T) - A(S,U) + 2D0*SPENCF(1D0+MZ2/T) -
     +                                2D0*SPENCF(1D0+MZ2/U) +
     +                        4D0*LOG(SQRT(MZ2*SK)/(MZ2-S))*LOG(T/U) )
      AGGT = ALPI*( G(T,S) + G(T,U) )
      VGGT = ALPI*( G(T,S) - G(T,U) + 2D0*LOG(SK/CMPLX(-T,-OFFSET))*
     +                                    LOG(CMPLX(S,OFFSET)/U) )
      AGZT = ALPI*( A(T,S) + A(T,U) )
      VGZT = ALPI*( A(T,S) - A(T,U) + 2D0*SPENCF(1D0+MZ2/S) -
     +                                2D0*SPENCF(1D0+MZ2/U) +
     +          4D0*LOG(SQRT(MZ2*SK)/(MZ2-T))*LOG(CMPLX(S,OFFSET)/U) )
*
* Combine the corrections with right helicity combinations with the
* different matrix element structures.
* The six helicity combinations are (p+ p- q+ q-):
* + + + +, - - - -, + + - -, - - + +, + - + -, - + - +
      MATRIX(1) = 16D0*PPQM**2*( (GZS( 1, 1)+GZT( 1, 1))*
     1                    DCONJG( GZS( 1, 1)*(LABDAS+DELSOF) +
     2                            GZT( 1, 1)*(LABDAT+DELSOF) +
     3             QF(1)**2*QF(IFERM)**2*GS*( VGGS + AGGS ) +
     4                 QF(1)**2*QF(1)**2*GT*( VGGT + AGGT ) +
     5     QF(1)*QF(IFERM)*(VF(1)-AF(1))*(VF(IFERM)-AF(IFERM))*
     6                                   ZS*( VGZS + AGZS ) +
     7     QF(1)*QF(1)*(VF(1)-AF(1))*(VF(1)-AF(1))*
     8                                   ZT*( VGZT + AGZT ) )
     +     + RESEFF(4) )
      MATRIX(2) = 16D0*PPQM**2*( (GZS(-1,-1)+GZT(-1,-1))*
     1                    DCONJG( GZS(-1,-1)*(LABDAS+DELSOF) +
     2                            GZT(-1,-1)*(LABDAT+DELSOF) +
     3             QF(1)**2*QF(IFERM)**2*GS*( VGGS + AGGS ) +
     4                 QF(1)**2*QF(1)**2*GT*( VGGT + AGGT ) +
     5     QF(1)*QF(IFERM)*(VF(1)+AF(1))*(VF(IFERM)+AF(IFERM))*
     6                                   ZS*( VGZS + AGZS ) +
     7     QF(1)*QF(1)*(VF(1)+AF(1))*(VF(1)+AF(1))*
     8                                   ZT*( VGZT + AGZT ) )
     +     + RESEFF(1) )
      MATRIX(3) = 16D0*PPQP**2*( GZS( 1,-1)*
     1                   DCONJG( GZS( 1,-1)*(LABDAS+DELSOF) +
     2              QF(1)**2*QF(IFERM)**2*GS*( VGGS - AGGS ) +
     3     QF(1)*QF(IFERM)*(VF(1)-AF(1))*(VF(IFERM)+AF(IFERM))*
     4                                    ZS*( VGZS - AGZS ) )
     +     + RESEFF(3) )
      MATRIX(4) = 16D0*PPQP**2*( GZS(-1, 1)*
     1                   DCONJG( GZS(-1, 1)*(LABDAS+DELSOF) +
     2              QF(1)**2*QF(IFERM)**2*GS*( VGGS - AGGS ) +
     3     QF(1)*QF(IFERM)*(VF(1)+AF(1))*(VF(IFERM)-AF(IFERM))*
     4                                    ZS*( VGZS - AGZS ) )
     +     + RESEFF(2) )
      MATRIX(5) = 16D0*PPPM**2* GZT( 1,-1)*
     1                  DCONJG( GZT( 1,-1)*(LABDAT+DELSOF) +
     2            QF(1)**2*QF(IFERM)**2*GT*( VGGT - AGGT ) +
     3     QF(1)*QF(1)*(VF(1)-AF(1))*(VF(1)+AF(1))*
     4                                  ZT*( VGZT - AGZT ) )
      MATRIX(6) = 16D0*PPPM**2* GZT(-1, 1)*
     1                  DCONJG( GZT(-1, 1)*(LABDAT+DELSOF) +
     2             QF(1)**2*QF(IFERM)**2*GT*( VGGT - AGGT ) +
     3     QF(1)*QF(1)*(VF(1)+AF(1))*(VF(1)-AF(1))*
     4                                   ZT*( VGZT - AGZT ) )
*
      SUM = MATRIX(1) + MATRIX(2) + MATRIX(3) +
     +      MATRIX(4) + MATRIX(5) + MATRIX(6)
* conversion to picobarn, 2pi from azimuthal angle, 1/8/(2pi)**2 from
* phase space, 1/(2s) from flux factor, 1/4 from spin averaging.
      EEEEVS = HBARC2/8D0/2D0/PI/2D0/S/4D0 * SUM
      END

      FUNCTION RNONLG(X,WEIGHT)
*     ---------------
* Calculate the single photon bremsstrahlung matrix element minus
* the 4 collinear approximations of it.
* First we have to set up the momenta of all the particles involved,
* then check whether the resulting 'event' lies within our cuts, and
* if so, the matrix element can be computed. Next we call COLLIN
* four times to calculate the 4 collinear approximations.
* The integral over this combination should give us the hard photon
* non log corrections.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ EVENT  / PP(0:3),PM(0:3),QP(0:3),QM(0:3),SP,QK(0:3)
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      REAL*8 X(1:5)
      IRNON=IRNON+1
*     Construct the event.
      QK(0) = EXP( X(1) )
      COSK  = X(2)
      FIK   = X(3)
      COSQP = X(4)
      FIQP  = X(5)
      SINQP = SQRT(ABS( 1D0 - COSQP**2 ))
      SINK  = SQRT(ABS( 1D0 - COSK **2 ))
      SINFK = SIN( FIK  )
      COSFK = COS( FIK  )
      SINFP = SIN( FIQP )
      COSFP = COS( FIQP )
      SP    = 4D0*PP(0)*( PP(0) - QK(0) )
      QK(1) = QK(0)*SINK *COSFK
      QK(2) = QK(0)*SINK *SINFK
      QK(3) = QK(0)*COSK
      CQK   = COSQP
      I=1
      QP(0) = SP/( (4D0*PP(0)-2D0*QK(0))**2 - 4D0*QK(0)**2*CQK**2 )*
     +    ( 4D0*PP(0)-2D0*QK(0)*( 1D0+CQK*SQRT(ABS(1D0-RMASS2(I)/SP**2*
     +        ( (4D0*PP(0)-2D0*QK(0))**2 - 4D0*QK(0)**2*CQK**2 ) )) ) )
      IF(QP(0) .LT. RMASS(IFERM) .OR. QP(0).GT.PP(0)) THEN
        RNONLG = 0D0
        RETURN
      ENDIF
      QV    = SQRT(ABS( QP(0)*QP(0) - RMASS2(I) ))
      QP(1) = QV*SINQP*COSFP
      QP(2) = QV*SINQP*SINFP
      QP(3) = QV*COSQP
*     Have to rotate q+, for the angles were defined w.r.t. k
      THETA = ACOS( COSK )
      CALL ROTATE(QP,FIK,THETA)
*     q- from momentum conservation
      DO 10 J = 0 , 3
        QM(J) = PP(J) + PM(J) - QP(J) - QK(J)
   10 CONTINUE
*     so the mass of q- is a genuine check
      CHK = ABS(QM(0)**2-QM(1)**2-QM(2)**2-QM(3)**2-RMASS2(I))/PP(0)**2
*      CALL HISTO(10,1,CHK,1D-20,1D-10,WEIGHT,2,' ',6,10)
      IF(CHK.GT.1D-10.OR.QM(0).LT.RMASS(IFERM).OR.QM(0).GT.PP(0)) THEN
        RNONLG = 0D0
        RETURN
      ENDIF
*
*     We actually have an event now. Dot products:
      PPK = ABS( DOT(PP,QK) )
      PMK = ABS( DOT(PM,QK) )
      QPK = ABS( DOT(QP,QK) )
      QMK = ABS( DOT(QM,QK) )
*
*     Logs to recognise collinear situations.
      IF( PPK .LE. 0D0) THEN
        RLOG1 = 100D0
      ELSEIF( PMK .LE. 0D0) THEN
        RLOG1 = - 100D0
      ELSE
        RLOG1 = - LOG( PPK/PMK )
      ENDIF
      IF( QPK .LE. 0D0) THEN
        RLOG2 = 100D0
      ELSEIF( QMK .LE. 0D0) THEN
        RLOG2 = - 100D0
      ELSE
        RLOG2 = - LOG( QPK/QMK )
      ENDIF
      RMAX1 = 15D0
      RMAX2 = RMAX1
      COSP = QP(3)/QP(0)
      COSM = QM(3)/QM(0)
      COSPM= ( QP(1)*QM(1) + QP(2)*QM(2) + QP(3)*QM(3) )/QP(0)/QM(0)
      IF(COSPM .GT. 1D0) COSPM = 1D0
      IF(COSPM .LT.-1D0) COSPM =-1D0
      ACOL = ABS( PI - ACOS(COSPM) )
*
*     The actual calls to calculate matrix elements.
*     Check on cuts to calculate the matrix element. For the collinear
*     approximations the check on cuts is done separately, as the
*     event will be modified! In a collinear situation, leave out the
*     matrix element and the appropriate approximation, as they should
*     cancel exactly.
      RNON = 0D0
      IF(COSP.LT.C1.AND.COSP.GT.C2 .AND. QP(0).GT.EP0*PP(0) .AND.
     +   COSM.LT.C4.AND.COSM.GT.C3 .AND. QM(0).GT.EM0*PP(0) .AND.
     +   ACOL.LT.ACOLMX .AND.
     +   ABS(RLOG1).LT.RMAX1 .AND. ABS(RLOG2).LT.RMAX2 ) THEN
         RJACOB = QP(0)/ABS(4D0*PP(0)-2D0*QK(0)*(1D0-CQK))*QK(0)**2
         RNON = RNON + EEEEG(PPK,PMK,QPK,QMK) * RJACOB
      ENDIF
      IF( IORDER.NE.0 .OR. ICHANN.NE.1 ) THEN
        IF(RLOG1.LT. RMAX1) RNON = RNON - COLLIN(PPK,1)
        IF(RLOG1.GT.-RMAX1) RNON = RNON - COLLIN(PMK,2)
      ENDIF
      IF( IFINAL.NE.0 .OR. ICHANN.NE.1 ) THEN
        IF(RLOG2.LT. RMAX2) RNON = RNON - COLLIN(QPK,3)
        IF(RLOG2.GT.-RMAX2) RNON = RNON - COLLIN(QMK,4)
      ENDIF
*
*     Include flux and phase space factors and conversion to picobarn
      RNON = HBARC2 * RNON /4D0/( 2D0*SCM*(2D0*PI)**5 )
*
*     Fill histo's. Can be left out.
*      COSQPK = 1D0 - QPK/QP(0)/QK(0)
*      COSQMK = 1D0 - QMK/QM(0)/QK(0)
*      COSQPM = 1D0 - DOT(QP,QM)/QM(0)/QP(0)
*      W = WEIGHT*RNON
*      CALL HISTO(1,1,QK(0)/PP(0),EPS2,1D0,W,2,' ',6,12)
*      CALL HISTO(2,1,QP(3)/QP(0),  C2, C1,W,1,' ',6,10)
*      CALL HISTO(3,1,QM(3)/QM(0), -C1,-C2,W,1,' ',6,10)
*      CALL HISTO(4,1,QK(3)/QK(0),-1D0,1D0,W,1,' ',6,10)
*      CALL HISTO(5,1,  COSQPK   ,-1D0,1D0,W,1,' ',6,10)
*      CALL HISTO(6,1,  COSQMK   ,-1D0,1D0,W,1,' ',6,10)
*      CALL HISTO(7,1,  COSQPM   ,-1D0,1D0,W,1,' ',6,10)
*      CALL HISTO(8,1,QP(0),RMASS(1),PP(0),W,1,' ',6,10)
*      CALL HISTO(9,1,QM(0),RMASS(1),PP(0),W,1,' ',6,10)
*
      RNONLG = RNON
      END

      FUNCTION DOT(P,Q)
*     ------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 P(0:3),Q(0:3)
      DOT = P(0)*Q(0) - P(1)*Q(1) - P(2)*Q(2) - P(3)*Q(3)
      END

      FUNCTION EEEE(P1,P2,Q1,Q2)
*     -------------
* The Born e+(p1) e-(p2) --> e+(q1) e-(q2) matrix element squared,
* including both gamma and Z in both s and t channel. Summing/averaging
* over spins is performed. The four momenta p1,...,q2 are input.
* W. Beenakker and S.C. van der Marck, April 1990.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      REAL*8 P1(0:3),P2(0:3),Q1(0:3),Q2(0:3),MATRIX(1:6)
      COMPLEX*16 GS,GT,ZS,ZT,MIX,GZS(-1:1,-1:1),GZT(-1:1,-1:1)
      PPQP = DOT(P1,Q1)
      PPQM = DOT(P1,Q2)
      PPPM = DOT(P1,P2)
      S    =   2D0*PPPM
      T    = - 2D0*PPQP
      E2   = EE**2
      QF2  = QF(1)*QF(IFERM)
      QF3  = QF(1)**2
      CALL GZPROP(S ,GS ,ZS ,MIX)
      CALL GZPROP(T ,GT ,ZT ,MIX)
      IF(ICHANN .EQ. 1) THEN
        ZT = (0D0,0D0)
        GT = (0D0,0D0)
      ELSEIF(ICHANN .EQ. 2) THEN
        ZS = (0D0,0D0)
        GS = (0D0,0D0)
      ENDIF
      I=IFERM
      DO 50 LE = - 1 , 1 , 2
        DO 40 LF = - 1 , 1 , 2
          GZS(LE,LF)=E2*(QF2*GS+(VF(1)-LE*AF(1))*(VF(I)-LF*AF(I))*ZS)
          GZT(LE,LF)=E2*(QF3*GT+(VF(1)-LE*AF(1))*(VF(1)-LF*AF(1))*ZT)
   40   CONTINUE
   50 CONTINUE
      MATRIX(1) = 16D0*ABS(GZS( 1, 1)+GZT( 1, 1))**2*PPQM**2
      MATRIX(2) = 16D0*ABS(GZS(-1,-1)+GZT(-1,-1))**2*PPQM**2
      MATRIX(3) = 16D0*ABS(GZS( 1,-1))**2*PPQP**2
      MATRIX(4) = 16D0*ABS(GZS(-1, 1))**2*PPQP**2
      MATRIX(5) = 16D0*ABS(GZT( 1,-1))**2*PPPM**2
      MATRIX(6) = 16D0*ABS(GZT(-1, 1))**2*PPPM**2
      EEEE = .25D0*( MATRIX(1) + MATRIX(2) + MATRIX(3) +
     +               MATRIX(4) + MATRIX(5) + MATRIX(6) )
      END

      FUNCTION EEEEW(COSTH)
*     ---------------
* The Born e+e- --> e+e- matrix element squared, including both gamma
* and Z in both s and t channel, and including WEAK corrections.
* Summing/averaging over spins is performed.
* COSTH is input and is to integrated over.
* W. Beenakker and S.C. van der Marck, April 1990.
* Heavy boxes (ZZ and WW) added: July 1990.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ BHSTAT / ISIGH,ISIGF,ITWOH,IZBR,IRNON,IEVS,IEEW
      REAL*8 MATRIX(1:6),ZCOP(-1:1,-1:1)
      COMPLEX*16 GS,GT,ZS,ZT,MIXS,MIXT,GZS(-1:1,-1:1), GZT(-1:1,-1:1)
      COMPLEX*16 HADRQQ,VZZS,AZZS,VWWS,AWWS,VZZT,AZZT,VWWT,AWWT
      IEEW=IEEW+1
      S = SCM
      PPQP = .25D0*S*( 1D0 - COSTH )
      PPQM = .25D0*S*( 1D0 + COSTH )
      PPPM =  .5D0*S
      T    = - 2D0*PPQP
* Define propagators, and include vertex form factors.
      E2   = 4D0*PI*ALFA
      CALL GZPROP(S,GS,ZS,MIXS)
      CALL FORMFS(S,IFERM)
      IF(ICHANN .EQ. 2) THEN
        GS = (0D0,0D0)
        ZS = (0D0,0D0)
        MIXS=(0D0,0D0)
      ENDIF
      I=IFERM
      DO 20 L1 = - 1 , 1 , 2
        DO 10 L2 = - 1 , 1 , 2
          GZS(L1,L2)=E2*( ( -QF(1)-FGV(1)-L1*(      -FGA(1) ) )*
     +                    ( -QF(I)-FGV(I)-L2*(      -FGA(I) ) )*GS +
     +                    (  VF(1)+FZV(1)-L1*( AF(1)+FZA(1) ) )*
     +                    (  VF(I)+FZV(I)-L2*( AF(I)+FZA(I) ) )*ZS -
     +      ( QF(1)*(VF(I)-L2*AF(I)) + QF(I)*(VF(1)-L1*AF(1)) )*MIXS )
          ZCOP(L1,L2) = ((VF(1)-L1*AF(1))*(VF(IFERM)-L2*AF(IFERM)))**2
   10   CONTINUE
   20 CONTINUE
*     Heavy boxes !
      IF(ICHANN .EQ. 2) THEN
        VZZS = (0D0,0D0)
        AZZS = (0D0,0D0)
        VWWS = (0D0,0D0)
        AWWS = (0D0,0D0)
      ELSE
        CALL HEAVYB(S,T,VZZS,AZZS,VWWS,AWWS)
      ENDIF
*     Now everything for the t channel
      CALL GZPROP(T,GT ,ZT ,MIXT)
      CALL FORMFS(T,1)
*     Incorporate the Burkhardt fit for the light quark loops.
      GT = GT/( 1D0 - HADRQQ(T) - PHADPI(T) )
      IF(ICHANN .EQ. 1) THEN
        GT = (0D0,0D0)
        ZT = (0D0,0D0)
        MIXT=(0D0,0D0)
      ENDIF
      DO 40 L1 = - 1 , 1 , 2
        DO 30 L2 = - 1 , 1 , 2
          GZT(L1,L2)=E2*(
     +         ( -QF(1)-FGV(1)-L1*(      -FGA(1) ) )*
     +         ( -QF(1)-FGV(1)-L2*(      -FGA(1) ) )*GT +
     +         (  VF(1)+FZV(1)-L1*( AF(1)+FZA(1) ) )*
     +         (  VF(1)+FZV(1)-L2*( AF(1)+FZA(1) ) )*ZT -
     +       (QF(1)*(VF(1)-L2*AF(1))+QF(1)*(VF(1)-L1*AF(1)))*MIXT )
   30   CONTINUE
   40 CONTINUE
*     Heavy boxes !
      IF(ICHANN .EQ. 1) THEN
        VZZT = (0D0,0D0)
        AZZT = (0D0,0D0)
        VWWT = (0D0,0D0)
        AWWT = (0D0,0D0)
      ELSE
        CALL HEAVYB(T,S,VZZT,AZZT,VWWT,AWWT)
      ENDIF
* There are 6 different helicity combinations (see EEEEVS).
      IF ( ICHANN .NE. 3 ) THEN
        MATRIX(1) = 16D0*PPQM**2*(GZS( 1, 1)+GZT( 1, 1))*
     +                      CONJG(GZS( 1, 1)+GZT( 1, 1))
        MATRIX(2) = 16D0*PPQM**2*(GZS(-1,-1)+GZT(-1,-1))*
     +                      CONJG(GZS(-1,-1)+GZT(-1,-1))
        MATRIX(3) = 16D0*PPQP**2* GZS( 1,-1)*CONJG(GZS( 1,-1))
        MATRIX(4) = 16D0*PPQP**2* GZS(-1, 1)*CONJG(GZS(-1, 1))
        MATRIX(5) = 16D0*PPPM**2* GZT( 1,-1)*CONJG(GZT( 1,-1))
        MATRIX(6) = 16D0*PPPM**2* GZT(-1, 1)*CONJG(GZT(-1, 1))
*       Heavy boxes (factor 2 from 2*M0*M1)
        MATRIX(1) = MATRIX(1)+32D0*PPQM**2*(GZS( 1, 1)+GZT( 1, 1))*
     +                        CONJG((VZZS+AZZS+VZZT+AZZT)*ZCOP( 1, 1))
        MATRIX(2) = MATRIX(2)+32D0*PPQM**2*(GZS(-1,-1)+GZT(-1,-1))*
     +    CONJG((VZZS+AZZS+VZZT+AZZT)*ZCOP(-1,-1)+VWWS+AWWS+VWWT+AWWT)
        MATRIX(3) = MATRIX(3) +     32D0*PPQP**2*GZS( 1,-1)*
     +                         CONJG(VZZS-AZZS)*ZCOP( 1,-1)
        MATRIX(4) = MATRIX(4) +     32D0*PPQP**2*GZS(-1, 1)*
     +                         CONJG(VZZS-AZZS)*ZCOP(-1, 1)
        MATRIX(5) = MATRIX(5) +     32D0*PPPM**2*GZT( 1,-1)*
     +                         CONJG(VZZT-AZZT)*ZCOP( 1,-1)
        MATRIX(6) = MATRIX(6) +     32D0*PPPM**2*GZT(-1, 1)*
     +                         CONJG(VZZT-AZZT)*ZCOP(-1, 1)
      ELSE
        MATRIX(1) = 16D0*PPQM**2*2D0*GZS( 1, 1)*CONJG(GZT( 1, 1))
        MATRIX(2) = 16D0*PPQM**2*2D0*GZS(-1,-1)*CONJG(GZT(-1,-1))
*       Heavy boxes
        MATRIX(1) = MATRIX(1)+32D0*PPQM**2*GZS( 1, 1)*
     +                             CONJG((VZZT+AZZT)*ZCOP( 1, 1))
     +                       +32D0*PPQM**2*GZT( 1, 1)*
     +                             CONJG((VZZS+AZZS)*ZCOP( 1, 1))
        MATRIX(2) = MATRIX(2)+32D0*PPQM**2*GZS(-1,-1)*
     +                  CONJG((VZZT+AZZT)*ZCOP(-1,-1)+VWWT+AWWT)
     +                       +32D0*PPQM**2*GZT(-1,-1)*
     +                  CONJG((VZZS+AZZS)*ZCOP(-1,-1)+VWWS+AWWS)
        MATRIX(3) = 0D0
        MATRIX(4) = 0D0
        MATRIX(5) = 0D0
        MATRIX(6) = 0D0
      ENDIF
      SUM = MATRIX(1) + MATRIX(2) + MATRIX(3) +
     +      MATRIX(4) + MATRIX(5) + MATRIX(6)
      EEEEW = HBARC2/8D0/2D0/PI/2D0/S/4D0 * SUM
      END

      SUBROUTINE HEAVYB(S,T,VZZ,AZZ,VWW,AWW)
*     -----------------
* Subroutine giving the 'couplings' with which to contract the
* ZZ and WW boxes with the Born matrix element.
* S,T are input and VZZ,AZZ,VWW,AWW are complex*16 output.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMPLEX*16 VZZ,AZZ,VWW,AWW,RI5ST,RIST,RI5SU,RISU
      U = - S - T
      CALL HEAVYI(S,T,RMZ,RIST,RI5ST)
      CALL HEAVYI(S,U,RMZ,RISU,RI5SU)
      VZZ = ALFA/2D0/PI*( RIST  - RISU  )
      AZZ = ALFA/2D0/PI*( RI5ST + RI5SU )
*     WW boxes depend strongly on the isospin of the produced fermion
      IF(IFERM.EQ.0.OR.IFERM.EQ.4.OR.IFERM.EQ.6.OR.IFERM.EQ.8) THEN
*       isospin = + 1/2
        CALL HEAVYI(S,U,RMW,RISU,RI5SU)
        VWW = ALFA/2D0/PI*( - RISU  )
        AWW = ALFA/2D0/PI*( + RI5SU )
      ELSE
*       isospin = - 1/2
        CALL HEAVYI(S,T,RMW,RIST,RI5ST)
        VWW = ALFA/2D0/PI*( + RIST  )
        AWW = ALFA/2D0/PI*( + RI5ST )
      ENDIF
* To get the normalization right
      E2 = EE**2
      VZZ = VZZ * E2/S
      AZZ = AZZ * E2/S
      VWW = VWW * E2/S/4D0/SIN2TH**2
      AWW = AWW * E2/S/4D0/SIN2TH**2
      END

      SUBROUTINE HEAVYI(S,T,RM,RI,RI5)
*     -----------------
* Function needed to calculate ZZ or WW boxes.
* S,T,RM are input, RI,RI5 are complex*16 output.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( EPS = 1D-10 )
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMPLEX*16 RI,RI5,SPENCF,ROOT1,ROOT2,X1,X2,Y1,Y2,FOURSP,RLOG12
      COMPLEX*16 SHELP,THELP,I,X1X2
      IF ( S.GT.0D0 . AND. T.GT.0D0 ) THEN
        WRITE(*,'(A)')' HEAVYI: both S and T > 0.  This is not valid!'
        RI  = (0D0,0D0)
        RI5 = (0D0,0D0)
        RETURN
      ENDIF
      RM2 = RM**2
      IF( S .GT. 0D0 ) THEN
        SHELP = 4D0*RM2/CMPLX(S,EPS)
      ELSE
        SHELP = 4D0*RM2/S
      ENDIF
      IF( T .GT. 0D0 ) THEN
        THELP = RM2/CMPLX(T,EPS)
      ELSE
        THELP = RM2/T
      ENDIF
      ROOT1 = SQRT( (1D0,0D0)-SHELP )
      IF(S.LT.0D0.AND.T.LT.0D0.AND.4D0*RM2/S*(1D0+RM2/T).GT.1D0) THEN
        I = (0D0,1D0)
        ROOT2 = I*SQRT( -( (1D0,0D0)-SHELP*( (1D0,0D0) + THELP ) ) )
      ELSE
        ROOT2 =   SQRT(    (1D0,0D0)-SHELP*( (1D0,0D0) + THELP )   )
      ENDIF
      Y1 = .5D0*( 1D0 + ROOT1 )
      Y2 = .5D0*( 1D0 - ROOT1 )
      X1 = .5D0*( 1D0 + ROOT2 )
      X2 = .5D0*( 1D0 - ROOT2 )
      X1X2 = ROOT2
      FOURSP = SPENCF(X1/(X1-Y1)) + SPENCF(X1/(X1-Y2)) -
     +         SPENCF(X2/(X2-Y2)) - SPENCF(X2/(X2-Y1))
      RLOG12 = LOG(-Y1/Y2)
      IF( ABS(X1X2) .LT. 10D0*EPS ) THEN
        X1X2 = (1D0,0D0)
        FOURSP = 1D0/(Y1-Y2)*( - 4D0*Y1*LOG(2D0*Y1/(Y1-Y2))
     +                         + 4D0*Y2*LOG(2D0*Y2/(Y2-Y1)) )
      ENDIF
      RI5 = (2D0*T+S+2D0*RM2)/(2D0*(S+T))*(
     +      SPENCF( 1D0+CMPLX(T,EPS)/RM2 ) - PI*PI/6D0 - RLOG12**2 ) +
     +      .5D0*LOG(-CMPLX(T,EPS)/RM2) + (Y2-Y1)/2D0*RLOG12 +
     +      ( S+2D0*T - 4D0*T*RM2/S + 2D0*RM2**2/T - 2D0*RM2**2/S )/
     +      ( 2D0*( S + T )*(-X1X2) ) * FOURSP
      RI5 = S/( S + T ) * RI5
      RI  = RI5 + 2D0*RLOG12**2 + 2D0/X1X2*FOURSP
      END

      FUNCTION COLLIN(DOTPK,I1)
*     ---------------
* Compute the collinear approximation to the process
* e+e- --> e+e-gamma. The matrix element squared for e+e- --> e+e-
* is supposed to be given by EEEE. I1 indicates which particle the
* photon is supposed to be 'collinear' with: 1=p+, 2=p-, 3=q+, 4=q-.
* DOTPK is the dot product of this particle and the photon.
* I1 is input, DOTPK is input, but gets changed.     March 1990.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      COMMON/ CONTRL / C1,C2,C3,C4,EP0,EM0,ACOLMX,SCM,BETALL,BETALF,
     +                 EPS,EPS2
      COMMON/ EVENT  / PP(0:3),PM(0:3),QP(0:3),QM(0:3),SP,QK(0:3)
      REAL*8 PMINK(0:3),Q1(0:3),Q2(0:3)
* Project the photon onto the particle momentum it is supposed to
* be collinear with. Adjust the rest of the event to balance
* energy and momentum.
      ICUT = 0
      IF(I1 .LE. 2) THEN
        X = QK(0)/PP(0)
        PDOTK = DOTPK
        QV2   = SQRT(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
        CQK   = QP(3)/QV2
        IF(I1 .EQ. 2) CQK = - CQK
        Q1(0) = SP/( 4D0*PP(0)-2D0*QK(0)*( 1D0 - CQK ) )
        QV1   = Q1(0)
        Q1(1) = QV1/QV2*QP(1)
        Q1(2) = QV1/QV2*QP(2)
        Q1(3) = QV1/QV2*QP(3)
        IF (I1 .EQ. 1) THEN
          DO 20 I = 0 , 3
            PMINK(I) = ( 1D0 - X )*PP(I)
            Q2(I) = PMINK(I) + PM(I) - Q1(I)
   20     CONTINUE
        ELSE
          DO 30 I = 0 , 3
            PMINK(I) = ( 1D0 - X )*PM(I)
            Q2(I) = PP(I) + PMINK(I) - Q1(I)
   30     CONTINUE
        ENDIF
        COSQP = Q1(3)/Q1(0)
        COSQM = Q2(3)/Q2(0)
        COSPM = ( Q1(1)*Q2(1) + Q1(2)*Q2(2) + Q1(3)*Q2(3) )/Q1(0)/Q2(0)
        IF(COSPM .GT. 1D0) COSPM = 1D0
        IF(COSPM .LT.-1D0) COSPM =-1D0
        ACOL = ABS( PI - ACOS(COSPM) )
        IF(COSQP.LT.C1.AND.COSQP.GT.C2 .AND. Q1(0).GT.EP0*PP(0) .AND.
     +     COSQM.LT.C4.AND.COSQM.GT.C3 .AND. Q2(0).GT.EM0*PP(0) .AND.
     +     ACOL .LT. ACOLMX ) ICUT = 1
        ROOT = Q1(3)/Q1(0)
        IF(I1.EQ.2) ROOT = - ROOT
        RJACOB = ABS(Q1(0)/(4D0*PP(0)-2D0*QK(0)*(1D0-ROOT))) *QK(0)**2
      ELSEIF(I1 .EQ. 3) THEN
        X = QK(0)/(QP(0)+QK(0))
        IF( (1D0-X)*PP(0) .LT. RMASS(IFERM) .OR.X.LT.EPS2) THEN
          COLLIN = 0D0
          RETURN
        ENDIF
        PMINK(0) = PP(0)
        FAC = PP(3)/PP(0)
        QV2 = SQRT(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
        Q2(0) = PP(0)
        DO 40 I = 1 , 3
          PMINK (I) = PP(0)/QV2*QP(I)
          Q2    (I) = - PMINK(I)
   40   CONTINUE
        COSTH = ( QP(1)*QK(1) + QP(2)*QK(2) + QP(3)*QK(3) )/QK(0)/QV2
        QV2 = SQRT(QP(0)**2-RMASS2(IFERM))
        PDOTK = X*(1D0-X)*PP(0)**2*( 1D0 - COSTH*FAC )
        DOTPK = X*(1D0-X)*PP(0)**2*( 1D0 - COSTH*QV2/QP(0) )
        COSQP = QP(3)/QP(0)
        COSQM = - COSQP
        IF( COSQP.LT.C1.AND.COSQP.GT.C2 .AND.
     +      COSQM.LT.C4.AND.COSQM.GT.C3 .AND. 1D0-X.GT.EP0 ) ICUT = 1
        RJACOB = (1D0-X)/4D0 * X**2*PP(0)**2
      ELSEIF(I1 .EQ. 4) THEN
        X = QK(0)/(QM(0)+QK(0))
        IF( (1D0-X)*PP(0) .LT. RMASS(IFERM) .OR.X.LT.EPS2) THEN
          COLLIN = 0D0
          RETURN
        ENDIF
        PMINK(0) = PP(0)
        FAC = PP(3)/PP(0)
        QV1 = SQRT(QM(1)*QM(1)+QM(2)*QM(2)+QM(3)*QM(3))
        QV2 = SQRT(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
        Q1(0) = PP(0)
        DO 50 I = 1 , 3
          Q1   (I) = PP(0)/QV2*QP(I)
          PMINK(I) = - Q1(I)
   50   CONTINUE
        COSTH = ( QP(1)*QK(1) + QP(2)*QK(2) + QP(3)*QK(3) )/QK(0)/QV2
        COST1 = ( QM(1)*QK(1) + QM(2)*QK(2) + QM(3)*QK(3) )/QK(0)/QV1
        QV2 = SQRT(QM(0)**2-RMASS2(IFERM))
        PDOTK = X*(1D0-X)*PP(0)**2*( 1D0 + COSTH*FAC )
        DOTPK = X*(1D0-X)*PP(0)**2*( 1D0 - COST1*QV2/QM(0) )
        COSQP = QP(3)/QP(0)
        COSQM = - COSQP
        IF( COSQP.LT.C1.AND.COSQP.GT.C2 .AND.
     +      COSQM.LT.C4.AND.COSQM.GT.C3 .AND. 1D0-X.GT.EM0 ) ICUT = 1
        RJACOB = (1D0-X)/4D0 * X**2*PP(0)**2
      ELSE
        STOP' COLLIN: I1 error! Fatal!'
      ENDIF
*
      IF(ICUT .EQ. 1) THEN
*       Compute the matrix element e+e- ---> e+e-
        IF(I1 .EQ. 1) THEN
          RM4 = EEEE(PMINK,PM,Q1,Q2)
        ELSEIF(I1 .EQ. 2) THEN
          RM4 = EEEE(PP,PMINK,Q1,Q2)
        ELSEIF(I1 .EQ. 3) THEN
          RM4 = EEEE(PP,PM,PMINK,Q2) *(1D0-X)*QF(IFERM)**2
        ELSE
          RM4 = EEEE(PP,PM,Q1,PMINK) *(1D0-X)*QF(IFERM)**2
        ENDIF
        RM4 = RM4*RJACOB
      ELSE
*       The modified momenta didn't pass the cuts.
        RM4 = 0D0
      ENDIF
*     Now relate this to the result for e+e- ---> e+e-gamma
      COLFAC = EE**2*( ( 1D0 + (1D0-X)**2 )/X/(1D0-X)/PDOTK )
      IF(I1.LT.3.AND.IFERM.NE.1) THEN
        COLFAC = COLFAC + EE**2*( RMASS2(1)*X/DOTPK**2 )
      ELSEIF(IFERM.NE.1) THEN
        COLFAC = COLFAC + EE**2*(RMASS2(IFERM)*X/DOTPK**2)
      ENDIF
      COLLIN = COLFAC*RM4
      END

      FUNCTION EEEEG(PPK,PMK,QPK,QMK)
*     --------------
* Matrix element squared for e+e- --> e+e-gamma, including
* initial and final state bremsstrahlung on the s and t channel.
* As input is needed the common EVENT and the dotproducts PPK,etc.
* Output is EEEEG = spin averaged and summed matrix element squared.
      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ EVENT  / PP(0:3),PM(0:3),QP(0:3),QM(0:3),SP,QK(0:3)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ OPTION / IWEAK,ICHANN,IOUT,IORDER,IFINAL,NONLOG,IFERM
      PARAMETER( IPP=1, IPM=2, IQ1=3, IP=4, IM=5, IBIGQ1=5, IBIGQ2=1 )
      REAL*8 QQ(0:3,5), DD(5,5)
      COMPLEX*16 Z(-1:1,5,5),GS,GSP,GT,GTP,ZS,ZSP,ZT,ZTP,MIX
      COMPLEX*16 GZS(-1:1,-1:1),GZSP(-1:1,-1:1)
      COMPLEX*16 GZT(-1:1,-1:1),GZTP(-1:1,-1:1)
* Statement functions
      ZINI(IQP,IQM,IK,IQ,L)=- EE*ROOT2/DFLOAT(L)/Z(-L,IQ1,IBIGQ1)*(
     1  Z(1,IPP,IQM)*(  Z(-1,IQP,IPM)*Z(1,IPM, IK)
     2                - Z(-1,IQP,IQ1)*Z(1,IQ1, IK) )*Z(-1, IQ,IPM)/PMK
     3 +Z(1,IPP, IK)*(- Z(-1, IQ,IPP)*Z(1,IPP,IQM)
     4                + Z(-1, IQ,IQ1)*Z(1,IQ1,IQM) )*Z(-1,IQP,IPM)/PPK)
      ZFIN(IEP,IEM,IK,IQ,L)=- QF(IFERM)*EE*ROOT2/DFLOAT(L)/
     1  Z(-L,IQ1,IBIGQ2)*(Z(1, IM,IEP)*(- Z(-1,IEM, IP)*Z(1, IP, IK)
     2                - Z(-1,IEM,IQ1)*Z(1,IQ1, IK) )*Z(-1, IQ, IP)/QPK
     3 +Z(1, IM, IK)*(  Z(-1, IQ, IM)*Z(1, IM,IEP)
     4                + Z(-1, IQ,IQ1)*Z(1,IQ1,IEP) )*Z(-1,IEM, IP)/QMK)
      ZUPP(IFP,IFM,IK,IQ,L)= EE*ROOT2/DFLOAT(L)/Z(-L,IQ1,IBIGQ1)*(
     1  Z(1,IPP,IFM)*(  Z(-1,IFP, IP)*Z(1, IP, IK)
     2                + Z(-1,IFP,IQ1)*Z(1,IQ1, IK) )*Z(-1, IQ, IP)/QPK
     3 +Z(1,IPP, IK)*(- Z(-1, IQ,IPP)*Z(1,IPP,IFM)
     4                + Z(-1, IQ,IQ1)*Z(1,IQ1,IFM) )*Z(-1,IFP, IP)/PPK)
      ZLOW(IEP,IEM,IK,IQ,L)=- EE*ROOT2/DFLOAT(L)/Z(-L,IQ1,IBIGQ2)*(
     1  Z(1, IM,IEP)*(- Z(-1,IEM,IPM)*Z(1,IPM, IK)
     2                + Z(-1,IEM,IQ1)*Z(1,IQ1, IK) )*Z(-1, IQ,IPM)/PMK
     3 +Z(1, IM, IK)*(  Z(-1, IQ, IM)*Z(1, IM,IEP)
     4                + Z(-1, IQ,IQ1)*Z(1,IQ1,IEP) )*Z(-1,IEM,IPM)/QMK)
*
* Define spinor products and dot products
      DO 10 I=0,3
        QQ(I,1)=PP(I)
        QQ(I,2)=PM(I)
        QQ(I,3)=QK(I)
        QQ(I,4)=QP(I)
        QQ(I,5)=QM(I)
   10 CONTINUE
      DO 30   I=  1,5
        DO 20 J=I+1,5
          ROOT = SQRT( ( QQ(0,J)-QQ(1,J) )/( QQ(0,I)-QQ(1,I) ) )
          Z( 1,I,J) = CMPLX( QQ(2,I) , QQ(3,I) )*ROOT
     +              - CMPLX( QQ(2,J) , QQ(3,J) )/ROOT
          Z(-1,I,J) = - CONJG( Z(1,I,J) )
          Z( 1,J,I) = - Z( 1,I,J)
          Z(-1,J,I) = - Z(-1,I,J)
          DD(  I,J) = - Z( 1,I,J)*Z(-1,I,J)/2D0
          DD(  J,I) =   DD(I,J)
   20   CONTINUE
        Z( 1,I,I) = CMPLX(0D0,0D0)
        Z(-1,I,I) = CMPLX(0D0,0D0)
        DD(  I,I) = 0D0
   30 CONTINUE
      ROOT2 = SQRT( 2D0 )
* Calculate gamma,Z propagators and appropriate couplings.
      S = 4D0*PP(0)**2
      T = - 2D0*DD(1,4)
      TP= - 2D0*DD(2,5)
      E2= EE**2
      QF2 = QF(1)*QF(IFERM)
      QF3 = QF(1)**2
      CALL GZPROP(S ,GS ,ZS ,MIX)
      CALL GZPROP(SP,GSP,ZSP,MIX)
      CALL GZPROP(T ,GT ,ZT ,MIX)
      CALL GZPROP(TP,GTP,ZTP,MIX)
      IF(ICHANN .EQ. 1) THEN
        GT  = (0D0,0D0)
        GTP = (0D0,0D0)
        ZT  = (0D0,0D0)
        ZTP = (0D0,0D0)
      ELSEIF(ICHANN .EQ. 2) THEN
        GS  = (0D0,0D0)
        GSP = (0D0,0D0)
        ZS  = (0D0,0D0)
        ZSP = (0D0,0D0)
      ENDIF
      I=IFERM
      DO 50 LE = - 1 , 1 , 2
       DO 40 LF = - 1 , 1 , 2
         GZS (LE,LF)=E2*(QF2*GS +(VF(1)-LE*AF(1))*(VF(I)-LF*AF(I))*ZS )
         GZSP(LE,LF)=E2*(QF2*GSP+(VF(1)-LE*AF(1))*(VF(I)-LF*AF(I))*ZSP)
         GZT (LE,LF)=E2*(QF3*GT +(VF(1)-LE*AF(1))*(VF(1)-LF*AF(1))*ZT )
         GZTP(LE,LF)=E2*(QF3*GTP+(VF(1)-LE*AF(1))*(VF(1)-LF*AF(1))*ZTP)
   40  CONTINUE
   50 CONTINUE
* Calculate the fermion currents we need
      IF ( IORDER.EQ.0 .AND. ICHANN.EQ.1 ) THEN
        ZINI1 = (0D0,0D0)
        ZINI2 = (0D0,0D0)
        ZINI3 = (0D0,0D0)
        ZINI4 = (0D0,0D0)
      ELSE
        ZINI1 = ZINI( IP, IM,IQ1,IBIGQ1, 1)
        ZINI2 = ZINI( IP, IM,IBIGQ1,IQ1,-1)
        ZINI3 = ZINI( IM, IP,IQ1,IBIGQ1, 1)
        ZINI4 = ZINI( IM, IP,IBIGQ1,IQ1,-1)
      ENDIF
      IF ( IFINAL.EQ.0 .AND. ICHANN.EQ.1 ) THEN
        ZFIN1 = (0D0,0D0)
        ZFIN2 = (0D0,0D0)
        ZFIN3 = (0D0,0D0)
        ZFIN4 = (0D0,0D0)
      ELSE
        ZFIN1 = ZFIN(IPP,IPM,IQ1,IBIGQ2, 1)
        ZFIN2 = ZFIN(IPP,IPM,IBIGQ2,IQ1,-1)
        ZFIN3 = ZFIN(IPM,IPP,IBIGQ2,IQ1,-1)
        ZFIN4 = ZFIN(IPM,IPP,IQ1,IBIGQ2, 1)
      ENDIF
      IF ( ICHANN .NE. 1 ) THEN
        ZUPP1 = ZUPP(IPM, IM,IQ1,IBIGQ1, 1)
        ZUPP2 = ZUPP(IPM, IM,IBIGQ1,IQ1,-1)
        ZUPP3 = ZUPP( IM,IPM,IQ1,IBIGQ1, 1)
        ZUPP4 = ZUPP( IM,IPM,IBIGQ1,IQ1,-1)
        ZLOW1 = ZLOW(IPP, IP,IQ1,IBIGQ2, 1)
        ZLOW2 = ZLOW(IPP, IP,IBIGQ2,IQ1,-1)
        ZLOW3 = ZLOW( IP,IPP,IBIGQ2,IQ1,-1)
        ZLOW4 = ZLOW( IP,IPP,IQ1,IBIGQ2, 1)
      ELSE
        ZUPP1 = (0D0,0D0)
        ZUPP2 = (0D0,0D0)
        ZUPP3 = (0D0,0D0)
        ZUPP4 = (0D0,0D0)
        ZLOW1 = (0D0,0D0)
        ZLOW2 = (0D0,0D0)
        ZLOW3 = (0D0,0D0)
        ZLOW4 = (0D0,0D0)
      ENDIF
* and construct helicity amplitudes
* First s channel contributions
      ZPPP =       ZINI1 *GZSP( 1, 1)+      ZFIN1 *GZS( 1, 1)
      ZPPM =       ZINI2 *GZSP( 1, 1)+      ZFIN2 *GZS( 1, 1)
      ZPMP =       ZINI3 *GZSP( 1,-1)+CONJG(ZFIN3)*GZS( 1,-1)
      ZPMM =       ZINI4 *GZSP( 1,-1)+CONJG(ZFIN4)*GZS( 1,-1)
      ZMMM = CONJG(ZINI1)*GZSP(-1,-1)+CONJG(ZFIN1)*GZS(-1,-1)
      ZMMP = CONJG(ZINI2)*GZSP(-1,-1)+CONJG(ZFIN2)*GZS(-1,-1)
      ZMPM = CONJG(ZINI3)*GZSP(-1, 1)+      ZFIN3 *GZS(-1, 1)
      ZMPP = CONJG(ZINI4)*GZSP(-1, 1)+      ZFIN4 *GZS(-1, 1)
* and then t channel ones
      ZTPPP=       ZUPP1 *GZTP( 1, 1)+      ZLOW1 *GZT( 1, 1)
      ZTPPM=       ZUPP2 *GZTP( 1, 1)+      ZLOW2 *GZT( 1, 1)
      ZTPMP=       ZUPP3 *GZTP( 1,-1)+CONJG(ZLOW3)*GZT( 1,-1)
      ZTPMM=       ZUPP4 *GZTP( 1,-1)+CONJG(ZLOW4)*GZT( 1,-1)
      ZTMMM= CONJG(ZUPP1)*GZTP(-1,-1)+CONJG(ZLOW1)*GZT(-1,-1)
      ZTMMP= CONJG(ZUPP2)*GZTP(-1,-1)+CONJG(ZLOW2)*GZT(-1,-1)
      ZTMPM= CONJG(ZUPP3)*GZTP(-1, 1)+      ZLOW3 *GZT(-1, 1)
      ZTMPP= CONJG(ZUPP4)*GZTP(-1, 1)+      ZLOW4 *GZT(-1, 1)
* Some of these have the same helicity combination: those where all
* fermions have equal helicity.
      ZPPP = ZPPP + ZTPPP
      ZPPM = ZPPM + ZTPPM
      ZMMP = ZMMP + ZTMMP
      ZMMM = ZMMM + ZTMMM
* Summing and averaging over helicities.
      EEEEG = .25D0*(  ZPPP*CONJG( ZPPP) +  ZPPM*CONJG( ZPPM)
     +              +  ZPMP*CONJG( ZPMP) +  ZPMM*CONJG( ZPMM)
     +              +  ZMMP*CONJG( ZMMP) +  ZMMM*CONJG( ZMMM)
     +              +  ZMPP*CONJG( ZMPP) +  ZMPM*CONJG( ZMPM)
     +              + ZTPMP*CONJG(ZTPMP) + ZTMPM*CONJG(ZTMPM)
     +              + ZTPMM*CONJG(ZTPMM) + ZTMPP*CONJG(ZTMPP) )
      IF(EEEEG .LT. 0D0) STOP'EEEEG less than zero !! Alarm !!'
      END

      SUBROUTINE ROTATE(VECTOR,ANGLE1,ANGLE2)
*     -----------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 VECTOR(0:3),NEW(0:3)
*      Rotate VECTOR around the y-axis with ANGLE2
* Then rotate VECTOR around the z-axis with ANGLE1
      C1 = COS(ANGLE1)
      S1 = SIN(ANGLE1)
      C2 = COS(ANGLE2)
      S2 = SIN(ANGLE2)
      NEW(1) = C1*C2*VECTOR(1) - S1*VECTOR(2) + C1*S2*VECTOR(3)
      NEW(2) = S1*C2*VECTOR(1) + C1*VECTOR(2) + S1*S2*VECTOR(3)
      NEW(3) =-   S2*VECTOR(1)                +    C2*VECTOR(3)
      DO 1 I = 1 , 3
         VECTOR(I) = NEW(I)
    1 CONTINUE
      END

      SUBROUTINE HISTO(I,ISTAT,X,X0,X1,WEIGHT,LINLOG,TITLE,IUNIT,NX)
*     ----------------
* Steven van der Marck, April 2nd, 1990.
* I      = number of this particular histogram (must be >0, <N(=50))
* ISTAT  = 0   : clear the arrays for all  histo's
*          1   : fill  the arrays for histo nr. I
*          2   : print histogram nr. I to unit nr. IUNIT
*          3   : output the data to HISTO.GRA - to be used for a
*                'home made' graphics program.
*          4   : same as 3, but the whole histogram is divided by
*                the number of points.
*          5   : save all relevant information to a file HISTO.DAT
*          6   : read all relevant information from file HISTO.DAT
*          ELSE: rescale all  histo's by a factor X
* X      = x-value to be placed in a bin of histogram nr. I
*          If ISTAT=2 and LINLOG<>1, x = the number of decades (def=3).
* X0     = the minimum for x in histogram nr. I
* X1     = the maximum for x in histogram nr. I
* WEIGHT = If ISTAT=1: the weight assigned to this value of x.
*          If ISTAT=2: the number of divisions on the y-axis (def=20),
*                     should not be <5 or >80 (because of screenwidth).
* LINLOG = determines the scale of both axes of the histogram.
*          If ISTAT=1 it goes for the x axis, for ISTAT=2 the y axis.
*          For LINLOG=1 the axis is linear, ELSE logarithmic.
*          If a linear histo has only one peak that is too sharp,
*          this routine will automatically switch to a log. y axis.
* TITLE  = title of this particular histogram ( character*(*) )
* IUNIT  = unit number to write this histogram to
* NX     = the number of divisions on the x-axis for this histogram
*          NX should not be greater than NXMAX (=50 in this version).
*
* When ISTAT = 0   : IUNIT is used.
*            = 1   : I, X, X0, X1, WEIGHT, LINLOG and NX are used.
*            = 2   : I, X, WEIGHT, LINLOG, TITLE and IUNIT are used.
*            = 3,4 : I, LINLOG and TITLE are used. The user should not
*             be using unit nr 11 when invoking HISTO with this option!
*            = 5,6 : no other variables are used.
*            = ELSE: only X is used.
      IMPLICIT LOGICAL(A-Z)
      INTEGER N,NX,NXMAX,I,ISTAT,LINLOG,IUNIT
* N = the maximum number of histo's allowed; can be changed on its own.
* NXMAX = the maximum allowed for NX(= nr. of divisions on the x-axis)
      PARAMETER( N = 50 , NXMAX = 50 )
      INTEGER J,J1,J2,IX,IY,JUNIT,NYDIV, LINLOX(N), IWARN
      REAL*8 X,X0,X1,WEIGHT, Z,WEISUM,WEISU2,FACTOR
      REAL*8 Y(N,NXMAX), YMAX(N), BOUND0(N), BOUND1(N),
     +                 XMIN(N), XMAX(N), YSQUAR(N,NXMAX), YOUT(N)
      INTEGER IUNDER(N), IIN(N), IOVER(N), NRBINS(N), IBIN(N,NXMAX)
      CHARACTER TITLE*(*),LINE(132),BLANK,STAR,TEXT*12
      CHARACTER FORM1*80,F*80,STRP*1,FH*30,F2(1:3)*12
      SAVE
      DATA STRP/'-'/F2/'(1X,4I10)','(1X,10I10)','(1X,10D12.5)'/
      DATA BLANK/' '/STAR/'*'/FH/'(A),''I'',3X,G11.4,2X,G11.4,I12)'/
*     Is this a valid histo nr.? (Not necessary if ISTAT=0.)
      IF( (I.LT.1.OR.I.GT.N) .AND. (ISTAT.NE.0) ) GOTO 910
*     ISTAT decides on several MUTUALLY EXCLUSIVE branches.
      IF(ISTAT .EQ. 0) THEN
*       Zero everything you've got.
        DO 20 J1 = 1 , N
          BOUND0(J1) = 0D0
          BOUND1(J1) = 0D0
          XMIN  (J1) = 0D0
          XMAX  (J1) = 0D0
          YMAX  (J1) = 0D0
          YOUT  (J1) = 0D0
          IUNDER(J1) = 0
          IIN   (J1) = 0
          IOVER (J1) = 0
          NRBINS(J1) = 0
          DO 10 J2 = 1 , NXMAX
            Y     (J1,J2) = 0D0
            YSQUAR(J1,J2) = 0D0
            IBIN  (J1,J2) = 0
   10     CONTINUE
   20   CONTINUE
        IWARN = 0
*       Finished ... - exit !
      ELSEIF(ISTAT.EQ.1) THEN
*       Fill arrays with this x value
        IF(NRBINS(I) .EQ. 0) THEN
*         First time around: remember boundaries, lin or log x-scale,
*         and the number of divisions on the x axis.
          BOUND0(I) = X0
          BOUND1(I) = X1
          LINLOX(I) = LINLOG
          IF(LINLOX(I) .NE. 1) THEN
            IF(BOUND0(I)*BOUND1(I) .GT. 0D0) THEN
              BOUND0(I) = LOG(ABS( BOUND0(I) ))
              BOUND1(I) = LOG(ABS( BOUND1(I) ))
            ELSE
              LINLOX(I) = 1
            ENDIF
          ENDIF
          NRBINS(I) = NXMAX
          IF(NX.GT.0 .AND. NX.LE.NXMAX) NRBINS(I) = NX
        ENDIF
        IF(LINLOX(I) .NE. 1) THEN
          IF(ABS(X) .GT. 0D0) THEN
            Z = LOG(ABS(X))
          ELSE
            Z = BOUND0(I) - 1D-10
          ENDIF
        ELSE
          Z = X
        ENDIF
*       Does this x value lie within the boundaries? Update statistics!
        IF(Z.LT.BOUND0(I))THEN
          IUNDER(I) = IUNDER(I) + 1
          YOUT  (I) = YOUT  (I) + WEIGHT
          IF(Z.LT.XMIN(I).OR.IUNDER(I).EQ.1) XMIN(I) = Z
        ELSEIF(Z.GT.BOUND1(I))THEN
          IOVER(I) = IOVER(I) + 1
          YOUT (I) = YOUT (I) + WEIGHT
          IF(Z.GT.XMAX(I).OR. IOVER(I).EQ.1) XMAX(I) = Z
        ELSE
          IIN(I) = IIN(I) + 1
          IX = INT((Z-BOUND0(I))/(BOUND1(I)-BOUND0(I))*NRBINS(I))+1
          IF(IX.EQ.NRBINS(I)+1) IX = NRBINS(I)
          IBIN  (I,IX) = IBIN  (I,IX) + 1
          Y     (I,IX) = Y     (I,IX) + WEIGHT
          YSQUAR(I,IX) = YSQUAR(I,IX) + WEIGHT**2
          IF(Y(I,IX).GT.YMAX(I)) YMAX(I) = Y(I,IX)
        ENDIF
*       Finished ... - exit !
      ELSEIF(ISTAT .EQ. 2) THEN
*       Print histogram. First a header.
        WRITE(IUNIT,'(//,A,I2,A,I10,A,I8,A,I8)')' Histogram nr.',I,
     +  '  Points in:',IIN(I),'  under:',IUNDER(I),'  over:',IOVER(I)
        WRITE(IUNIT,*)' ',TITLE
*       Leave if all entries have equal 0 weight.
        IF(ABS(YMAX(I)).LE.0D0) GOTO 920
*       Determine the number of divisions on the y axis.
        NYDIV = INT(WEIGHT)
        IF(NYDIV .LT. 5 .OR. NYDIV .GT. 80) NYDIV = 20
*       Determine lin/log scale y axis.
        IF(LINLOG .EQ. 1) THEN
*         Count the number of entries that will show up in a lin scale,
*         for if they are too few, make it a log one.
          IX = 0
          DO 30 J1 = 1 , NRBINS(I)
            IF(Y(I,J1)/YMAX(I)*NYDIV .GT. 1D0) IX = IX + 1
   30     CONTINUE
        ENDIF
        IF(IX .LE. 2 .OR. LINLOG .NE. 1) THEN
          IX = 2
          FACTOR = 1D3/YMAX(I)
          IF(X.GE.1D0 .AND. X.LE.10D0) FACTOR = 10D0**X/YMAX(I)
        ELSE
          IX = 1
        ENDIF
*       Prepare the formats (they depend on #divisions in y)
        WRITE(FORM1,'(A,I3,A)') '('' '',G11.4,1X,',NYDIV,'(A),A)'
        WRITE(F,'(A,I3,A)') '('' '',A12,',NYDIV,FH
        Z = BOUND0(I)
        IF(LINLOX(I) .NE. 1) Z = EXP(Z)
        WRITE(IUNIT,FORM1) Z,(STRP,J1=1,NYDIV),
     +    '   bin boundary   bin ''area''    # points'
        WEISUM = 0D0
        WEISU2 = 0D0
*       Loop over the divisions on the x axis. Print a line per div.
        DO 50 J1 = 1 , NRBINS(I)
*         First determine the height of this entry on the y axis.
          IY=1
          IF(IX.EQ.1) THEN
            IF(Y(I,J1).GT.0D0) IY=INT(Y(I,J1)/YMAX(I)*FLOAT(NYDIV))+1
          ELSE
            IF(FACTOR*Y(I,J1).GT.1D0) IY=INT(LOG(FACTOR*Y(I,J1))/
     +                            LOG(FACTOR*YMAX(I))*FLOAT(NYDIV))+1
          ENDIF
*         Fill the character array LINE that will be printed.
          IF(IY .EQ. NYDIV+1) IY = NYDIV
          DO 40 J2 = 1 , NYDIV
            LINE(J2)=BLANK
            IF(J2.EQ.IY) LINE(J2)=STAR
   40     CONTINUE
*         Prepare surrounding text and numbers
          Z = BOUND0(I) + J1/FLOAT(NRBINS(I))*(BOUND1(I)-BOUND0(I))
          IF(LINLOX(I) .NE. 1) Z = EXP(Z)
          WEISUM = WEISUM + Y(I,J1)
          WEISU2 = WEISU2 + YSQUAR(I,J1)
          IF(J1.EQ.INT(FLOAT(NRBINS(I))/2D0))THEN
            TEXT = ' (x,y) =   I'
          ELSEIF(J1.EQ.INT(FLOAT(NRBINS(I))/2D0)+1)THEN
            TEXT = ' (lin,lin) I'
            IF(IX.NE.1.AND.LINLOX(I).EQ.1) TEXT = ' (lin,log) I'
            IF(IX.EQ.1.AND.LINLOX(I).NE.1) TEXT = ' (log,lin) I'
            IF(IX.NE.1.AND.LINLOX(I).NE.1) TEXT = ' (log,log) I'
          ELSE
            TEXT = '           I'
          ENDIF
*         The actual WRITE !
          WRITE(IUNIT,F)TEXT,(LINE(J2),J2=1,NYDIV),Z,Y(I,J1),IBIN(I,J1)
   50   CONTINUE
        Z = BOUND1(I)
        IF(LINLOX(I) .NE. 1) THEN
          Z       = EXP(Z)
          XMIN(I) = EXP(XMIN(I))
          XMAX(I) = EXP(XMAX(I))
        ENDIF
*       End with information. Then we're through.
        WRITE(IUNIT,FORM1) Z,(STRP,J1=1,NYDIV),' '
        Z=SQRT(ABS(WEISU2-WEISUM**2/FLOAT(IIN(I))))/FLOAT(IIN(I))
        WRITE(IUNIT,'(12X,''The average of the entries amounts to '',
     +    G11.4,'' +- '',G11.4,/,12X,
     +    ''The fraction inside the histo bounds: '',G11.4)')WEISUM/
     +    FLOAT(IIN(I)+IUNDER(I)+IOVER(I)),Z,WEISUM/(WEISUM+YOUT(I))
        IF(IUNDER(I).GE.1) WRITE(IUNIT,60)'minimum',XMIN(I)
        IF( IOVER(I).GE.1) WRITE(IUNIT,60)'maximum',XMAX(I)
   60   FORMAT(12X,'The ',A,' value that occurred was   ',G11.4)
*       Finished ... - exit !
      ELSEIF(ISTAT .EQ. 3 .OR. ISTAT .EQ. 4) THEN
        IF(YMAX(I) .LE. 0D0) GOTO 930
        JUNIT = 11
        OPEN(UNIT=JUNIT,FILE='HISTO',STATUS='NEW')
        FACTOR = NRBINS(I)/(BOUND1(I)-BOUND0(I))
        IF(ISTAT .EQ. 4) FACTOR=FACTOR/FLOAT(IIN(I)+IUNDER(I)+IOVER(I))
        IF(LINLOG .EQ. 1) THEN
          IF(LINLOX(I) .EQ. 1) THEN
            WRITE(JUNIT,110) BOUND0(I),BOUND1(I),1.1D0*YMAX(I)*FACTOR
  110       FORMAT('*B',/,'VH 3.0',/,'LX 14.0',/,'LY 14.0',/,
     +       'XM ',D12.4,2X,D12.4,' 10',/,'YM 0. ',D12.4,' 10',/,'//')
          ELSE
            WRITE(JUNIT,120) 1.1D0*YMAX(I)*FACTOR
  120       FORMAT('*B',/,'VH 3.0',/,'LX 14.0',/,'LY 14.0',/,
     +       'XL',/,'YM 0. ',D12.4,' 10',/,'//')
          ENDIF
        ELSE
          Z = YMAX(I)*FACTOR
          DO 130 J1 = 2 , NRBINS(I)
            IF(FACTOR*Y(I,J1).LT.Z.AND.Y(I,J1).GT.0D0) Z=Y(I,J1)*FACTOR
  130     CONTINUE
          WEISUM = .8D0*Z
          IF(LINLOX(I) .EQ. 1) THEN
            WRITE(JUNIT,140) BOUND0(I),BOUND1(I)
          ELSE
            WRITE(JUNIT,150)
          ENDIF
  140     FORMAT('*B',/,'VH 3.0',/,'LX 14.0',/,'LY 14.0',/,'XM ',D12.4,
     +      2X,D12.4,' 10',/,'YL ',/,'//')
  150     FORMAT('*B',/,'VH 3.0',/,'LX 14.0',/,'LY 14.0',/,'XL',
     +      /,'YL',/,'//')
        ENDIF
        WRITE(JUNIT,*)' ',TITLE
        WRITE(JUNIT,'(///,''//'',/,''*P'',/,''SN -1'',/,''CL'')')
        Z = BOUND0(I)
        IF(LINLOX(I) .NE. 1) Z = EXP(Z)
        IF(Y(I,1).GT.0D0 .OR. LINLOG.EQ.1)
     +    WRITE(JUNIT,170) Z,Y(I,1)*FACTOR
        DO 160 J1 = 1 , NRBINS(I)-1
          Z = BOUND0(I) + J1/FLOAT(NRBINS(I))*(BOUND1(I)-BOUND0(I))
          IF(LINLOX(I) .NE. 1) Z = EXP(Z)
          IF((Y(I,J1).GT.0D0.AND.Y(I,J1+1).GT.0D0).OR.LINLOG.EQ.1)THEN
            WRITE(JUNIT,170) Z,Y(I,J1  )*FACTOR
            WRITE(JUNIT,170) Z,Y(I,J1+1)*FACTOR
          ELSEIF(Y(I,J1).GT.0D0) THEN
            WRITE(JUNIT,170) Z,Y(I,J1)*FACTOR
            WRITE(JUNIT,170) Z, WEISUM
            WRITE(JUNIT,'(''/'')')
          ELSEIF(Y(I,J1+1).GT.0D0) THEN
            WRITE(JUNIT,170) Z, WEISUM
            WRITE(JUNIT,170) Z,Y(I,J1+1)*FACTOR
          ENDIF
  160   CONTINUE
        J1 = NRBINS(I)
        Z  = BOUND1(I)
        IF(LINLOX(I) .NE. 1) Z = EXP(Z)
        IF(Y(I,J1).GT.0D0 .OR. LINLOG.EQ.1)
     +    WRITE(JUNIT,170) Z,Y(I,J1)*FACTOR
  170   FORMAT(' ',2D15.7)
        WRITE(JUNIT,'(''//'',/,''*E'')')
        CLOSE(UNIT=JUNIT)
      ELSEIF(ISTAT .EQ. 5) THEN
        JUNIT = 11
        OPEN(UNIT=JUNIT,FILE='HISTO',STATUS='NEW')
        WRITE(JUNIT,F2(1))(IUNDER(J),IIN(J),IOVER(J),NRBINS(J),J=1,N)
        WRITE(JUNIT,F2(2)) ((IBIN(J1,J2),J2=1,NXMAX),J1=1,N)
        WRITE(JUNIT,F2(3)) (YMAX(J1),BOUND0(J1),BOUND1(J1),
     +                     XMIN(J1),XMAX  (J1),YOUT  (J1),J1=1,N)
        WRITE(JUNIT,F2(3))((Y(J1,J2),YSQUAR(J1,J2),J2=1,NXMAX),J1=1,N)
        CLOSE(UNIT=JUNIT)
      ELSEIF(ISTAT .EQ. 6) THEN
        JUNIT = 11
        OPEN(UNIT=JUNIT,FILE='HISTO',STATUS='OLD')
        READ(JUNIT,F2(1))(IUNDER(J),IIN(J),IOVER(J),NRBINS(J),J=1,N)
        READ(JUNIT,F2(2)) ((IBIN(J1,J2),J2=1,NXMAX),J1=1,N)
        READ(JUNIT,F2(3)) (YMAX(J1),BOUND0(J1),BOUND1(J1),
     +                    XMIN(J1),XMAX  (J1),YOUT  (J1),J1=1,N)
        READ(JUNIT,F2(3))((Y(J1,J2),YSQUAR(J1,J2),J2=1,NXMAX),J1=1,N)
        CLOSE(UNIT=JUNIT)
      ELSE
*       Scaling of the y axis with factor X. Useful for normalization.
        DO 200 J1 = 1 , N
          YMAX(J1) = X * YMAX(J1)
          DO 190 J2 = 1 , NXMAX
            Y     (J1,J2) = X    * Y     (J1,J2)
            YSQUAR(J1,J2) = X**2 * YSQUAR(J1,J2)
  190     CONTINUE
  200   CONTINUE
      ENDIF
      RETURN
*     A few error returns.
  910 IWARN = IWARN + 1
      IF(IWARN .LE. 5) WRITE(*,'(A,I10,A,I2)')' HISTO called with I =',
     +                       I,',     Warning nr.',IWARN
      RETURN
  920 WRITE(IUNIT,'(A)')' All bins have weights <=0. No histo printed.'
      RETURN
  930 WRITE(  *  ,'(A)')' All bins have weights <=0. No histo printed.'
      RETURN
      END

      SUBROUTINE COUPLS(SIN2TH,RMT)
*     -----------------
* Define the fermion masses and their couplings to the bosons.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      RMASS(0) = 0D0
* Charged lepton masses from Particle Data Group Publ. 1990.
      RMASS(1) = .51099906D-3
      RMASS(2) = .105658387D0
      RMASS(3) = 1.7841D0
      RMASS(4) = .04145D0
      RMASS(5) = .04146D0
      RMASS(6) = 1.5D0
      RMASS(7) = .15D0
      RMASS(8) = RMT
      RMASS(9) = 4.5D0
      SW    = DSQRT( SIN2TH )
      CW    = DSQRT( 1D0 - SIN2TH )
      DO 10 I = 0 , NRMASS
         RMASS2(I) = RMASS(I)**2
         IF(I .EQ. 0) THEN
            Q  =  0D0
            T3 = .5D0
         ELSEIF(I .LE. 3) THEN
            Q  = -  1D0
            T3 = - .5D0
         ELSEIF(I.EQ.4 .OR. I.EQ.6 .OR. I.EQ.8) THEN
            Q  =   2D0/3D0
            T3 =  .5D0
         ELSE
            Q  = -  1D0/3D0
            T3 = - .5D0
         ENDIF
         VF(I) = ( T3 - 2D0*Q*SIN2TH ) /2D0/CW/SW
         AF(I) = T3 /2D0/SW/CW
         QF(I) =   Q
   10 CONTINUE
      END

      SUBROUTINE RNORM()
*     ----------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Calculate all quantities that have to do with weak corrections on
* boson propagators.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     +                 SUMQ1,SUMQ2
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      SW  = DSQRT( SIN2TH )
      CW2 =  1D0 - SIN2TH
      CW  = DSQRT( CW2 )
      Z   = RMZ**2
      W   = RMW**2
      SIGGZ0 = USIGGZ( 0D0 )
      PIGAM0 = DUSIGG( 0D0 )
*
* Renormalization constants eq. (3.16) and (3.17) of ref.2b
      DELMZ  =   USIGZ( Z )
      DELMW  =   USIGW( W )
      DELZ2G = - PIGAM0
      DELZ2Z = - PIGAM0 - 2D0*(CW2-SIN2TH)/SW/CW*SIGGZ0/Z +
     +                    (CW2-SIN2TH)/SIN2TH*( DELMZ/Z - DELMW/W )
      DELZ2W = - PIGAM0 - 2D0*CW/SW*SIGGZ0/Z +
     +                    CW2/SIN2TH*( DELMZ/Z - DELMW/W )
*
* Contributions from the DELTA-i terms
      SUMQ1 = ALFA/4D0/PI/2D0/SIN2TH/W*(
     +      + ( RMASS2(4)-RMASS2(5) )*DLOG(RMASS(4)/RMASS(5))
     +      + ( RMASS2(6)-RMASS2(7) )*DLOG(RMASS(6)/RMASS(7))
     +      + ( RMASS2(8)-RMASS2(9) )*DLOG(RMASS(8)/RMASS(9)) )
      SUMQ2 = ALFA/2D0/PI*( + DLOG(RMASS(4)/RMASS(5))
     +                      + DLOG(RMASS(6)/RMASS(7))
     +                      + DLOG(RMASS(8)/RMASS(9)) )
*
* Calculate delta-r and update the values for sin(theta-w) and MW.
      DR = DELTAR()
      BIGA0  = 37.281D0**2
      SIN2TH = .5D0*(  1D0 - DSQRT( 1D0-4D0*BIGA0/Z/(1D0-DR) )  )
      RMW = DSQRT( Z*( 1D0 - SIN2TH ) )
      END

      SUBROUTINE FORMFS(QSQR,IFERM)
*     -----------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Calculate the vector and axial vector formfactors for the Z-ff and
* the gamma-ff couplings.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      COMPLEX*16 FZV,FZA,FGV,FGA,FL1,FL2,FL3,FL4,FL5
      COMPLEX*16 RL2Z,RL2W,RL3W,RL3Z
*
      CALL LABDAS(QSQR,RMZ,RL2Z,RL3Z)
      CALL LABDAS(QSQR,RMW,RL2W,RL3W)
      CW2  = 1D0 - SIN2TH
      SW   = DSQRT( SIN2TH )
      CW   = DSQRT( CW2 )
      ALF4PI = ALFA/4D0/PI
* eq. (C.4) ref 2b.
      FL1 = RL2W/8D0/SW**3/CW - 3D0*CW/4D0/SW**3*RL3W
      FL2 = -(1D0-2D0/3D0*SIN2TH)/8D0/SW**3/CW*RL2W +
     +         3D0*CW/4D0/SW**3*RL3W
      FL3 =  (1D0-4D0/3D0*SIN2TH)/8D0/SW**3/CW*RL2W -
     +         3D0*CW/4D0/SW**3*RL3W
      IF(IFERM .EQ. 9) THEN
         CALL FLBOT(QSQR,FL4,FL5)
      ELSE
         FL4 = FL3
         FL5 = 1D0/6D0/SIN2TH*RL2W - 3D0/4D0/SIN2TH*RL3W
      ENDIF
* eq. (C.3) of ref 2b.
      FZV(0) = ALFA/4D0/PI/4D0/SW/CW*( RL2Z/4D0/CW2/SIN2TH +
     +           (1D0-1D0/2D0/SIN2TH)*RL2W + 3D0*CW2/SIN2TH*RL3W )
      FZA(0) = FZV(0)
      FZV(1) = ALF4PI*( VF(1)*(VF(1)**2+3D0*AF(1)**2)*RL2Z + FL1 )
      FZA(1) = ALF4PI*( AF(1)*(3D0*VF(1)**2+AF(1)**2)*RL2Z + FL1 )
      FZV(4) = ALF4PI*( VF(4)*(VF(4)**2+3D0*AF(4)**2)*RL2Z + FL2 )
      FZA(4) = ALF4PI*( AF(4)*(3D0*VF(4)**2+AF(4)**2)*RL2Z + FL2 )
      FZV(5) = ALF4PI*( VF(5)*(VF(5)**2+3D0*AF(5)**2)*RL2Z + FL3 )
      FZA(5) = ALF4PI*( AF(5)*(3D0*VF(5)**2+AF(5)**2)*RL2Z + FL3 )
      FZV(9) = ALF4PI*( VF(9)*(VF(9)**2+3D0*AF(9)**2)*RL2Z + FL4 )
      FZA(9) = ALF4PI*( AF(9)*(3D0*VF(9)**2+AF(9)**2)*RL2Z + FL4 )
* eq. (C.12) ref 2b.
      FL1 = -3D0/ 4D0/SIN2TH*RL3W
      FL2 = -1D0/12D0/SIN2TH*RL2W + 3D0/4D0/SIN2TH*RL3W
      FL3 =  1D0/ 6D0/SIN2TH*RL2W - 3D0/4D0/SIN2TH*RL3W
      FGV(0) = CMPLX(0D0,0D0)
      FGA(0) = FGV(0)
* eq. (C.11) ref 2b.
      FGV(1) = ALF4PI*( QF(1)*(VF(1)**2+AF(1)**2)*RL2Z + FL1 )
      FGA(1) = ALF4PI*( QF(1)*( 2D0*VF(1)*AF(1) )*RL2Z + FL1 )
      FGV(4) = ALF4PI*( QF(4)*(VF(4)**2+AF(4)**2)*RL2Z + FL2 )
      FGA(4) = ALF4PI*( QF(4)*( 2D0*VF(4)*AF(4) )*RL2Z + FL2 )
      FGV(5) = ALF4PI*( QF(5)*(VF(5)**2+AF(5)**2)*RL2Z + FL3 )
      FGA(5) = ALF4PI*( QF(5)*( 2D0*VF(5)*AF(5) )*RL2Z + FL3 )
      FGV(9) = ALF4PI*( QF(9)*(VF(9)**2+AF(9)**2)*RL2Z + FL5 )
      FGA(9) = ALF4PI*( QF(9)*( 2D0*VF(9)*AF(9) )*RL2Z + FL5 )
* all others are related to the previous ones.
      DO 10 I = 0 , NRMASS
         IF(I.EQ.2 .OR. I.EQ.3) THEN
            FZV(I) = FZV(1)
            FZA(I) = FZA(1)
            FGV(I) = FGV(1)
            FGA(I) = FGA(1)
         ELSEIF(I.EQ.6 .OR. I.EQ.8) THEN
            FZV(I) = FZV(4)
            FZA(I) = FZA(4)
            FGV(I) = FGV(4)
            FGA(I) = FGA(4)
         ELSEIF(I.EQ.7) THEN
            FZV(I) = FZV(5)
            FZA(I) = FZA(5)
            FGV(I) = FGV(5)
            FGA(I) = FGA(5)
         ENDIF
   10 CONTINUE
      END

      SUBROUTINE LABDAS(QSQR,RM,LABDA2,LABDA3)
*     -----------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 LABDA2,LABDA3,W,X,BIGK,SPENCF
      PARAMETER( EPS = 1D-10 )
      W = RM**2/DCMPLX( QSQR , EPS )
      X = LOG( - BIGK(QSQR,RM) )
* This way of writing was not very stable for qsqr << rm**2. The
* second way is somewhat better, but still has to be cut off at some
* low qsqr value, in which case it should yield zero.
*      LABDA2 = -3.5D0 - 2D0*W - (2D0*W+3D0)*LOG(-W)+
*     +   2D0*(1D0+W)**2*( SPENCF(1D0+1D0/W) - PI**2/6D0 )
      LABDA2 = -3.5D0 - 2D0*W - (2D0*W+3D0)*LOG(-W)+
     +   2D0*(1D0+W)**2*( -SPENCF(-1D0/W)+LOG(-W)*LOG(1D0+1D0/W) )
      IF(DREAL(W).GT.1D6) LABDA2 = (0D0,0D0)
      LABDA3 =  5D0/6D0 - 2D0*W/3D0 - 1D0/3D0*(2D0*W+1)*
     +   CDSQRT(1D0-4D0*W)*X + 2D0/3D0*W*(W+2D0)*X**2
      END

      FUNCTION BIGK(QSQR,RM)
*     -------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 BIGK,W
      W = RM**2/DCMPLX( QSQR , 1D-10 )
      BIGK = - ( CDSQRT(1D0-4D0*W) - 1D0 )/( CDSQRT(1D0-4D0*W) + 1D0 )
      END

      FUNCTION SIGG(QSQR)
*     -------------
* Real part of the renormalized weakly corrected photon propagator
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     +                 SUMQ1,SUMQ2
* eq. (3.23) ref 2b.
      SIGG = USIGG(QSQR) - PIGAM0 * QSQR
      END

      FUNCTION SIGGZ(QSQR)
*     --------------
* Real part of the renormalized weakly corrected photon-Z mixing
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     +                 SUMQ1,SUMQ2
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      CW2 = 1D0 - SIN2TH
      CW  = DSQRT( CW2 )
      SW  = DSQRT( SIN2TH )
* eq. (3.23) ref 2b.
      SIGGZ = USIGGZ(QSQR) - SIGGZ0 - QSQR* CW*SW/(CW2-SIN2TH)*
     +        ( DELZ2Z - DELZ2G ) +
     +        QSQR*( - CW/SW*SUMQ1 - SUMQ2/6D0/CW/SW )
      END

      FUNCTION SIGZ(QSQR)
*     -------------
* Real part of the renormalized weakly corrected Z propagator
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     +                 SUMQ1,SUMQ2
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      CW2 = 1D0 - SIN2TH
* eq. (3.23) ref 2b.
      SIGZ = USIGZ(QSQR) - DELMZ + DELZ2Z*( QSQR - RMZ**2 ) +
     +       (QSQR-RMZ**2)*((CW2-SIN2TH)/SIN2TH*SUMQ1+SUMQ2/3D0/SIN2TH)
      END

      FUNCTION SIGW(QSQR)
*     -------------
* Real part of the renormalized weakly corrected W propagator
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     +                 SUMQ1,SUMQ2
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      CW2 = 1D0 - SIN2TH
* eq. (3.23) ref 2b.
      SIGW = USIGW(QSQR) - DELMW + DELZ2W*( QSQR - RMW**2 ) +
     +       (QSQR-RMW**2)*( CW2/SIN2TH*SUMQ1 + SUMQ2/3D0/SIN2TH )
      END

      FUNCTION DELTAR()
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* The weak correction factor delta-r
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
* eq. (4.18) ref 2b.
      DELTAR = SIGW(0D0)/RMW**2 + ALFA/4D0/PI/SIN2TH*
     +        ( 6D0 + (7D0-4D0*SIN2TH)/2D0/SIN2TH*DLOG(1D0-SIN2TH) )
      END

      FUNCTION USIGG(QSQR)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized weakly corrected photon prop
* eq. (B.2) ref 2b with errata, a minus sign and a bracket.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      S   = QSQR
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         RM = RMASS(I)
         TOT = TOT + 4D0/3D0*QF(I)**2*(
     +         ( S+2D0*RMASS2(I) )*FREAL(S,RM,RM) - S/3D0 )  *  FAC
   10 CONTINUE
      TOT = TOT - ( 3D0*S + 4D0*RMW**2 )*FREAL(S,RMW,RMW)
      USIGG = ALFA/4D0/PI * TOT
      END

      FUNCTION DUSIGG(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Derivative of the real part of the unrenormalized
* weakly corrected photon propagator
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      S   = QSQR
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         RM = RMASS(I)
         TOT = TOT + 4D0/3D0*QF(I)**2*( + FREAL(S,RM,RM) +
     +         ( S+2D0*RMASS2(I) )*DFREAL(S,RM,RM) - 1D0/3D0 )  *  FAC
   10 CONTINUE
      TOT = TOT - 3D0*FREAL(S,RMW,RMW) -
     +            ( 3D0*S + 4D0*RMW**2 )*DFREAL(S,RMW,RMW)
      DUSIGG = ALFA/4D0/PI * TOT
      END

      FUNCTION PHADPI(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the hadronic part of sigma-g(qsqr) / qsqr = pi-hadronic,
* calculated perturbatively.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      S   = QSQR
      TOT = 0D0
      FAC = 3D0
      DO 10 I = 4 , NRMASS
         RM = RMASS(I)
         IF(I.NE.8) TOT = TOT + 4D0/3D0*QF(I)**2*(
     +         ( S+2D0*RMASS2(I) )*FREAL(S,RM,RM) - S/3D0 ) * FAC/S
   10 CONTINUE
      PHADPI = ALFA/4D0/PI * TOT
      END

      FUNCTION USIGGZ(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized photon-Z mixing propagator
* eq. (B.3) ref 2b.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      S   = QSQR
      SW  = DSQRT(       SIN2TH )
      CW  = DSQRT( 1D0 - SIN2TH )
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         RM = RMASS(I)
         TOT = TOT - 4D0/3D0*QF(I)*VF(I)*(
     +         ( S+2D0*RMASS2(I) )*FREAL(S,RM,RM) - S/3D0 )  *  FAC
   10 CONTINUE
      TOT = TOT + 1D0/CW/SW*( ( 3D0*CW**2 + 1D0/6D0 )*S
     +                      + ( 4D0*CW**2 + 4D0/3D0 )*RMW**2 )*
     +                       FREAL(S,RMW,RMW)  +  S/9D0/CW/SW
      USIGGZ = ALFA/4D0/PI * TOT
      END

      FUNCTION USIGZ(QSQR)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized weakly corrected Z propagator,
* for QSQR > 0.
* eq. (B.4) ref 2b. 1 erratum in the pole part, not apparent here.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      S   = QSQR
      CW2 =  1D0 - SIN2TH
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(I .LE. 3) TOT = TOT + 4D0/3D0* 2D0*AF(I)**2*S*(
     +                    + 5D0/3D0 - DLOG(ABS( S/RMASS2(I) )) ) * FAC
         RM = RMASS(I)
         F = FREAL(S,RM,RM)
         TOT = TOT + 4D0/3D0*(  ( VF(I)**2+AF(I)**2 )*(
     +                         + ( S+2D0*RMASS2(I) )*F - S/3D0 )
     +             - 3D0/8D0/SIN2TH/CW2*RMASS2(I)*F ) * FAC
   10 CONTINUE
      W = RMW**2
      Z = RMZ**2
      H = RMH**2
      TOT = TOT + ( ( -CW2**2*(40D0*S+80D0*W) + 12D0*W +
     1              (CW2-SIN2TH)**2*( 8D0*W+S ) )*FREAL(S,RMW,RMW) +
     2            ( 10D0*Z - 2D0*H + S + (H-Z)**2/S )*FREAL(S,RMH,RMZ)-
     3            2D0*H*DLOG(H/W) - 2D0*Z*DLOG(Z/W) +
     4            ( 10D0*Z - 2D0*H + S )*( 1D0 - (H+Z)/(H-Z)*
     5              DLOG(RMH/RMZ) - DLOG(RMH*RMZ/W) ) +
     6            2D0/3D0*S*( 1D0 + (CW2-SIN2TH)**2 - 4D0*CW2**2 )
     7  )/12D0/CW2/SIN2TH
      USIGZ = ALFA/4D0/PI * TOT
      END

      FUNCTION USIGW(QSQR)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized weakly corrected W propagator,
* for QSQR >= 0.
* eq. (B.5) ref 2b with errata: a factor 3 for the last 7 lines,
*                               one factor s, one factor 1/s and a sign
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      REAL*8 MP,MM
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      S   = QSQR
      CW2 =  1D0 - SIN2TH
      W   = RMW**2
      Z   = RMZ**2
      H   = RMH**2
      TOT = 0D0
      IF(DABS(S) .GT. 1D-10) THEN
         DO 10 I = 1 , NRMASS
            RM  = RMASS (I)
            RM2 = RMASS2(I)
            IF(I .LE. 3) THEN
              TOT = TOT + ( S-RM2/2D0-RM2**2/2D0/S )*FREAL(S,0D0,RM) +
     +                     2D0/3D0*S - RM2/2D0
            ELSE
               FAC = 3D0
               IF(MOD(I,2) .EQ. 0) THEN
                  MP  = RMASS(I)
                  MM  = RMASS(I+1)
                  SUM = RMASS2(I) + RMASS2(I+1)
                  DIF = RMASS2(I) - RMASS2(I+1)
                  TOT = TOT + (
     +               (S-SUM/2D0-DIF**2/2D0/S)*FREAL(S,MP,MM)+
     +               (S-SUM/2D0)*(1D0-SUM/DIF*DLOG(MP/MM))-S/3D0) * FAC
               ENDIF
            ENDIF
   10    CONTINUE
         TOT = TOT + 3D0*(  ( SIN2TH**2*Z - CW2/3D0*( 7D0*Z + 7D0*W +
     1                  10D0*S - 2D0*(Z-W)**2/S ) - 1D0/6D0*(W+Z-S/2D0-
     2              (Z-W)**2/2D0/S ) )*FREAL(S,RMZ,RMW) +
     3       SIN2TH/3D0*( -4D0*W-10D0*S+2D0*W**2/S )*FREAL(S,0D0,RMW) +
     4       1D0/6D0*( 5D0*W-H+S/2D0+(H-W)**2/2D0/S )*FREAL(S,RMH,RMW)+
     5       ( CW2/3D0*( 7D0*Z+7D0*W+10D0*S-4D0*(Z-W) ) - SIN2TH**2*Z +
     6         1D0/6D0*( 2D0*W - S/2D0 ) ) * Z/(Z-W)*DLOG(Z/W) -
     7       ( 2D0/3D0*W + S/12D0 ) * H/(H-W)*DLOG(H/W) -
     8       CW2/3D0*( 7D0*Z + 7D0*W + 32D0/3D0*S ) + SIN2TH**2*Z +
     9       1D0/6D0*( 5D0/3D0*S + 4D0*W - Z - H ) -
     1       SIN2TH/3D0*( 4D0*W + 32D0/3D0*S )  )
      ELSE
         DO 20 I = 1 , NRMASS
            RM  = RMASS (I)
            RM2 = RMASS2(I)
            IF(I .LE. 3) THEN
              TOT = TOT - 3D0/4D0 * RM2
            ELSE
               FAC = 3D0
               IF(MOD(I,2) .EQ. 0) THEN
                  MP  = RMASS(I)
                  MM  = RMASS(I+1)
                  SUM = RMASS2(I) + RMASS2(I+1)
                  DIF = RMASS2(I) - RMASS2(I+1)
                  TOT = TOT -.5D0*(
     +                      3D0/2D0*SUM - DLOG(MP/MM)/DIF*
     +                      (SUM**2+2D0*RMASS2(I)*RMASS2(I+1)) ) * FAC
               ENDIF
            ENDIF
   20    CONTINUE
         TOT = TOT +3D0*( (2D0/3D0*CW2+1D0/12D0)*(.5D0*(W+Z)-W*Z/(W-Z)*
     1               DLOG(W/Z) ) + W/3D0*SIN2TH + 1D0/12D0*( .5D0*
     2               (H+W)-H*W/(W-H)*DLOG(W/H) ) + ( CW2/3D0*( 3D0*Z+
     3               11D0*W ) - SIN2TH**2*Z + 1D0/3D0*W )*Z/(Z-W)*
     4               DLOG(Z/W) - 2D0/3D0*W*H/(H-W)*DLOG(H/W) -
     5               1D0/3D0*CW2*(7D0*Z+7D0*W) + SIN2TH**2*Z + 1D0/6D0*
     6              (4D0*W-Z-H) - SIN2TH/3D0*4D0*W  )
      ENDIF
      USIGW = ALFA/4D0/PI /3D0/SIN2TH  * TOT
      END

      FUNCTION BIGPIZ(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Derivative of the real part of the renormalized
* weakly corrected Z propagator, for QSQR > 0.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     +                 SUMQ1,SUMQ2
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      S   = QSQR
      CW2 = 1D0 - SIN2TH
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(I .LE. 3) TOT = TOT + 4D0/3D0* 2D0*AF(I)**2*(
     +                    + 2D0/3D0 - DLOG(ABS( S/RMASS2(I) )) ) * FAC
         RM = RMASS(I)
         F  =  FREAL(S,RM,RM)
         DF = DFREAL(S,RM,RM)
         TOT = TOT + 4D0/3D0*(  ( VF(I)**2+AF(I)**2 )*(
     +                   F  + ( S+2D0*RMASS2(I) )*DF - 1D0/3D0 )
     +             - 3D0/8D0/SIN2TH/CW2*RMASS2(I)*DF ) * FAC
   10 CONTINUE
      W = RMW**2
      Z = RMZ**2
      H = RMH**2
      TOT = TOT + ( ( -CW2**2*(40D0*S+80D0*W) + 12D0*W +
     1                (CW2-SIN2TH)**2*( 8D0*W+S ) )*DFREAL(S,RMW,RMW) +
     2               (-40D0*CW2**2+(CW2-SIN2TH)**2 )*FREAL(S,RMW,RMW) +
     3            ( 10D0*Z-2D0*H+S+(H-Z)**2/S )*DFREAL(S,RMH,RMZ) +
     4            (        1D0 - (H-Z)**2/S**2 )*FREAL(S,RMH,RMZ) +
     5            ( 1D0-(H+Z)/(H-Z)*DLOG(RMH/RMZ)-DLOG(RMH*RMZ/W) ) +
     6            2D0/3D0*( 1D0 + (CW2-SIN2TH)**2 - 4D0*CW2**2 )
     7  )/12D0/CW2/SIN2TH
      BIGPIZ = TOT * ALFA/4D0/PI + DELZ2Z +
     +                    (CW2-SIN2TH)/SIN2TH*SUMQ1 + SUMQ2/3D0/SIN2TH
      END

      FUNCTION FREAL(S,RM1,RM2)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the function F(s,ma,mb), eq. (B.6) ref 2b.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( EPS = 1D-10 )
      IF(RM1 .LT. RM2) THEN
        RMA = RM1
        RMB = RM2
      ELSE
        RMA = RM2
        RMB = RM1
      ENDIF
      RMA2 = RMA**2
      RMB2 = RMB**2
      IF(DABS(S) .LT. EPS) THEN
        F = 0D0
      ELSEIF(S .GT. 0D0) THEN
        IF(RMA2 .LT. EPS .AND. RMB2 .LT. EPS) THEN
          WRITE(*,*)'FREAL: ma = mb = 0 cannot be handled !'
          F = 0D0
        ELSEIF(RMA2 .LT. EPS) THEN
          F = - 1D0 + DLOG( RMB2 )
        ELSEIF(DABS(RMA2-RMB2) .LT. EPS) THEN
          F =         DLOG( RMB2 )
        ELSE
          F = - 1D0 + ( RMA2*DLOG(RMA2)-RMB2*DLOG(RMB2) )/(RMA2-RMB2)
        ENDIF
        F = - F
        IF(RMA2 .LT. EPS) THEN
          IF(DABS(S-RMB2) .LT. EPS) THEN
            F = F + DLOG(S) - 2D0
          ELSE
            F = F + DLOG(S) - 1D0 + RMB2/S*DLOG(RMB2/S) - RMB2/S -
     +        ( (RMB2/S-1D0)*DLOG(DABS(RMB2/S-1D0)) - (RMB2/S-1D0) )
          ENDIF
        ELSE
          S0   = - .5D0*( 1D0 + RMA2/S - RMB2/S )
          S1   =   .5D0*( 1D0 - RMA2/S + RMB2/S )
          DISCR= ( (S+RMA2-RMB2)**2 - 4D0*RMA2*S ) /4D0/S**2
          ROOTD= DSQRT( DABS( DISCR ) )
          F = F + DLOG(S) + S1*DLOG( RMB2/S ) - 2D0*S1 -
     +                      S0*DLOG( RMA2/S ) + 2D0*S0
          IF(DISCR .GE. 0D0) THEN
            IF(S.LT.RMA2 .OR. S.LT.RMB2) THEN
              F = F + ROOTD*( DLOG( (S1+ROOTD)**2*S/RMB2 )
     +                      - DLOG( (S0+ROOTD)**2*S/RMA2 ) )
            ELSE
              F = F + ROOTD*( DLOG( (S1+ROOTD)**2*S/RMB2 )
     +                      - DLOG( RMA2/S/(S0-ROOTD)**2 ) )
            ENDIF
          ELSE
            F = F + 2D0*ROOTD*( DATAN(S1/ROOTD) - DATAN(S0/ROOTD) )
          ENDIF
        ENDIF
      ELSE
        IF(RMA2 .LT. EPS .AND. RMB2 .LT. EPS) THEN
          WRITE(*,*)'FREAL: ma = mb = 0 cannot be handled !'
          F = 0D0
        ELSEIF(RMA2 .LT. EPS) THEN
          F = - 1D0 - ( 1D0-RMB2/S )*LOG( RMB2/(RMB2-S) )
        ELSE
          IF(ABS(RMA2-RMB2) .LT. EPS) THEN
            F = - 2D0
          ELSE
            F = - 1D0 - ( (RMA2-RMB2)/S - (RMA2+RMB2)/(RMA2-RMB2) )*
     +                  .5D0*LOG(RMB2/RMA2)
          ENDIF
          ROOTA = SQRT( (RMA+RMB)**2 - S )
          ROOTB = SQRT( (RMA-RMB)**2 - S )
          F = F - ROOTA*ROOTB/S*LOG( (ROOTA+ROOTB)**2/4D0/RMA/RMB )
        ENDIF
      ENDIF
      FREAL = - F
      END

      FUNCTION FIMAG(S,RMA,RMB)
*     --------
* Imaginary part of the function F(s,ma,mb)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (PI=3.1415926535897932D0)
      FIMAG = 0D0
      IF(S.GT.(RMA+RMB)**2) FIMAG=PI*DSQRT((S-(RMA+RMB)**2)*
     +                                     (S-(RMA-RMB)**2))/S
      END

      FUNCTION DFREAL(S,RM1,RM2)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Derivative of the real part of the function F(s,ma,mb).
      IMPLICIT REAL*8(A-H,O-Z)
      EPS = 1D-10
      IF(RM1 .LT. RM2) THEN
         RMA = RM1
         RMB = RM2
      ELSE
         RMA = RM2
         RMB = RM1
      ENDIF
      RMA2 = RMA**2
      RMB2 = RMB**2
      DIFF = RMB2 - RMA2
      IF(S.LT.EPS .AND. RMA2.LT.EPS .AND. RMB2.LT.EPS) THEN
         WRITE(*,*)'DFREAL: S = Ma = Mb = 0 cannot be handled !'
         F = 0D0
      ELSEIF(RMA2.LT.EPS .AND. RMB2.LT.EPS) THEN
         F = 1D0/S
      ELSEIF(ABS(S).LT.EPS .AND. DABS(RMA2-RMB2).LT.EPS) THEN
         F = - 1D0/6D0/RMA2
      ELSEIF(RMA2 .LT. EPS) THEN
         F = 1D0/S*( 1D0 - RMB2/S*DLOG( RMB2/DABS(RMB2-S) ) )
      ELSEIF(ABS(S) .LT. EPS) THEN
         F = 1D0/DIFF*( .5D0 - RMB2/DIFF -
     +                  RMA2*RMB2/DIFF**2*DLOG(RMB2/RMA2) )
      ELSEIF(S .LT. 0D0) THEN
         A = (RMA+RMB)**2 - S
         B = (RMA-RMB)**2 - S
         ROOTA = SQRT( A )
         ROOTB = SQRT( B )
         F = .5D0*(RMA2-RMB2)/S**2*LOG(RMB2/RMA2) + ROOTA*ROOTB/S*( (
     +       1D0/2D0/A + 1D0/2D0/B + 1D0/S )*
     +       LOG( (ROOTA+ROOTB)**2/4D0/RMA/RMB ) + 1D0/ROOTA/ROOTB )
      ELSE
         DISCR = - ( ( S + RMA2 - RMB2 )**2 - 4D0*RMA2*S )/4D0/S**2
         ROOTD = DSQRT( DABS( DISCR ) )
         SP = (   S - RMA2 + RMB2 )/2D0/S
         SM = ( - S - RMA2 + RMB2 )/2D0/S
         IF(ROOTD .LT. EPS) THEN
            F = - ( 1D0/SP - 1D0/SM ) / S
         ELSEIF(DISCR .LT. 0D0) THEN
            IF(S.LT.RMA2 .OR. S.LT.RMB2) THEN
               F =-.5D0/S/ROOTD*DLOG( (ROOTD+SP)**2/RMB2
     +                               /(ROOTD+SM)**2*RMA2 )
            ELSE
               F =-.5D0/S/ROOTD*( DLOG( (ROOTD+SP)**2*S/RMB2 )
     +                          - DLOG( RMA2/S/(SM-ROOTD)**2 ) )
            ENDIF
         ELSE
            F = 1D0/S/ROOTD*( DATAN(SP/ROOTD) - DATAN(SM/ROOTD) )
         ENDIF
         F = F * ( - S*(RMA2+RMB2) + DIFF**2 )/2D0/S**2 +
     +         1D0/S - DIFF/2D0/S**2*DLOG(RMB2/RMA2)
      ENDIF
      DFREAL = - F
      END

      FUNCTION PWIDTH(I)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* The partial width of the Z due to fermion i.
* Fermionic 2 loop effects have been taken into account using
* eq. (5.18) ref 2b and simple QED and QCD correction factors.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      Z = RMZ**2
      GZMIX  = SIGGZ(Z) / Z
      CALL FORMFS(Z,I)
      TOT = 0D0
      IF(I .EQ. 0) THEN
         FAC = 1D0
      ELSEIF(I .LE. 3) THEN
         FAC = 1D0*( 1D0 + FACQED*QF(I)**2 )
      ELSEIF(I .EQ. 9) THEN
         FAC = 3D0*( 1D0 + FACQED*QF(I)**2 )*( 1D0 + FACQCB )
      ELSE
         FAC = 3D0*( 1D0 + FACQED*QF(I)**2 )*( 1D0 + FACQCD )
      ENDIF
      IF(Z .GT. 4.D0*RMASS2(I)) THEN
         TOT = ALFA*DSQRT(1D0-4D0*RMASS2(I)/Z)*
     +      ( ( VF(I)**2 + AF(I)**2 )*( Z + 2D0*RMASS2(I) ) -
     +       6D0*RMASS2(I)*AF(I)**2 )/3.D0 * FAC
*
         TOT = TOT + FAC * 2D0/3D0*ALFA*Z*(
     +         VF(I)*( DREAL(FZV(I)) + QF(I)*GZMIX )
     +       + AF(I)*  DREAL(FZA(I)) )
      ENDIF
      PWIDTH = TOT/( 1D0 + BIGPIZ(Z) )/ RMZ
      END

      FUNCTION IMSIGZ(S)
*     ---------------
* Imaginary part of the 1-loop Z self-energy
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      REAL*8 IMSIGZ
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      CW2 = 1D0 - SIN2TH
      TOT = 0D0
      DO 10 I = 0 , NRMASS
         IF(I .EQ. 0) FAC = 3D0
         IF(I .EQ. 1) FAC = 1D0
         IF(I .EQ. 4) FAC = 3D0
         IF(S .GT. 4.D0*RMASS2(I)) THEN
            TOT = TOT + DSQRT(1D0-4D0*RMASS2(I)/S)*
     +         ( ( VF(I)**2 + AF(I)**2 )*( S + 2D0*RMASS2(I) ) -
     +          6D0*RMASS2(I)*AF(I)**2 )/3.D0 * FAC
         ENDIF
   10 CONTINUE
      IF(S .GT. 4D0*RMW**2) TOT = TOT + DSQRT(1D0-4D0*RMW**2/S)*
     +       ((-10D0*S-20D0*RMW**2)*CW2**2+(2D0*RMW**2+S/4D0)*
     +             (CW2-SIN2TH)**2+3D0*RMW**2)*4.D0*AF(1)**2/3D0
      IF(S .GT. (RMH+RMZ)**2) TOT = TOT +
     +        (10D0*RMZ**2-2D0*RMH**2+S+(RMH**2-RMZ**2)**2/S)*AF(1)**2*
     +        DSQRT((1D0-(RMZ-RMH)**2/S)*(1D0-(RMZ+RMH)**2/S))/3D0
      IMSIGZ = TOT * ALFA
      END

      FUNCTION IMZ2(S)
*     -------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Imaginary part of the fermionic 2-loop Z self-energy.
* eq. (5.18) ref 2b and simple QED and QCD correction factors.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      REAL*8 IMZ2
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ FORMMZ /FZVMZ(0:NRMASS),FZAMZ(0:NRMASS),
     +                FGVMZ(0:NRMASS),FGAMZ(0:NRMASS)
      COMPLEX*16 FZVMZ,FZAMZ,FGVMZ,FGAMZ
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      Z = RMZ**2
      GZMIX  = SIGGZ(Z) / Z
      TOT = 0D0
      DO 10 I = 0 , NRMASS
         IF(S .GT. 4D0*RMASS2(I)) THEN
            IF(I .EQ. 0) THEN
               FAC1 = 3D0*FACQED*QF(I)**2
               FAC2 = 3D0 + FAC1
            ELSEIF(I .LE. 3) THEN
               FAC1 = 1D0*FACQED*QF(I)**2
               FAC2 = 1D0 + FAC1
            ELSEIF(I .EQ. 9) THEN
               FAC1 = 3D0*( FACQED*QF(I)**2*(1D0+FACQCB) + FACQCB )
               FAC2 = 3D0 + FAC1
            ELSE
               FAC1 = 3D0*( FACQED*QF(I)**2*(1D0+FACQCD) + FACQCD )
               FAC2 = 3D0 + FAC1
            ENDIF
            TOT = TOT +
     +           RMZ**2*ALFA*DSQRT(1D0-4D0*RMASS2(I)/Z)*
     +         ( ( VF(I)**2 + AF(I)**2 )*( 1D0 + 2D0*RMASS2(I)/Z ) -
     +          6D0*RMASS2(I)/Z*AF(I)**2 )/3.D0 * FAC1
            TOT = TOT + FAC2 * 2D0/3D0*ALFA*RMZ**2*(
     +               VF(I)*( DREAL(FZVMZ(I)) + QF(I)*GZMIX )
     +             + AF(I)*  DREAL(FZAMZ(I)) )
         ENDIF
   10 CONTINUE
      TOT  = TOT * S/RMZ**2
      IMZ2 = TOT
      END

      FUNCTION IMSIGG(S)
*     ---------------
* Imaginary part of the 1-loop QED vacuumpolarization
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      REAL*8 IMSIGG
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(S .GT. 4D0*RMASS2(I)) TOT = TOT + FAC * QF(I)**2*
     +        DSQRT(1D0-4D0*RMASS2(I)/S)*(1D0+2D0*RMASS2(I)/S)/3D0
   10 CONTINUE
      IF(S .GT. 4D0*RMW**2) TOT = TOT -
     +         DSQRT(1D0-4D0*RMW**2/S)*(3D0/4D0+RMW**2/S)
      IMSIGG = TOT * ALFA * S
      END

      FUNCTION IMSGGZ(S)
*     ---------------
* Imaginary part of the 1-loop Z-gamma mixing
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      REAL*8 IMSGGZ
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ ADHOC  / ALFA, PI, FACQCB, FACQCD,FACQED,HBARC2,EE
      CW2 = 1D0 - SIN2TH
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(S .GT. 4D0*RMASS2(I)) TOT = TOT - S*QF(I)*VF(I)/3D0* FAC*
     +             DSQRT(1D0-4D0*RMASS2(I)/S)*(1D0+2D0*RMASS2(I)/S)
   10 CONTINUE
      IF(S .GT. 4D0*RMW**2) TOT = TOT-S*AF(1)*DSQRT(1D0-4D0*RMW**2/S)*
     +                ( (3D0*CW2+1D0/6D0)+RMW**2/S*(4D0*CW2+4D0/3D0) )
      IMSGGZ = TOT * ALFA
      END

      SUBROUTINE FLBOT(QSQR,FZL9,FGL9)
*     ----------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* The left handed Z-bb and gamma-bb form factors
* see eqs (C.8),(C.9), (C.14) ff. of ref 2b.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( NRMASS = 9 )
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     +            VF(0:NRMASS),AF(0:NRMASS),QF(0:NRMASS)
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMPLEX*16 FI(1:7),GI(1:7),ZLFIN,TOT1,TOT2,FZL9,FGL9,HELP
      COMPLEX*16 C1,C2,C3,C4,C5,C6,C7,C8,C9,C10
      COMPLEX*16 B1BAR,C0SCAL,C1PLUS,C2ZERO,C2MIN,C2PLUS
      SW = DSQRT( SIN2TH )
      CW2 = 1D0 - SIN2TH
      CW  = DSQRT( CW2 )
      RT = RMASS(8)
      RB = RMASS(9)
      C1 = C0SCAL(QSQR,RT,RMW,RB)
      C2 = C1PLUS(QSQR,RT,RMW,RB)
      C3 = C2MIN (QSQR,RT,RMW,RB)
      C4 = C2ZERO(QSQR,RT,RMW,RB)
      C5 = C2PLUS(QSQR,RT,RMW,RB)
      C6 = C0SCAL(QSQR,RMW,RT,RB)
      C7 = C1PLUS(QSQR,RMW,RT,RB)
      C8 = C2MIN (QSQR,RMW,RT,RB)
      C9 = C2ZERO(QSQR,RMW,RT,RB)
      C10= C2PLUS(QSQR,RMW,RT,RB)
      ZLFIN = 1D0/2D0/SIN2TH*( 2D0 + RT**2/RMW**2 )*(
     +         B1BAR(RB**2,RT,RMW) )
      FI(1) = (2D0/3D0*SIN2TH-1D0)/4D0/CW/SW * ZLFIN
      GI(1) = -1D0/6D0*ZLFIN
      HELP  = -1.5D0 + 2D0*DLOG(RMW/RT) + 4D0*C4 - 2D0*QSQR*(C5 - C3)
     +        + 4D0*QSQR*(C2-.5D0*C1)
      FI(2) = (VF(8)+AF(8))/4D0/SIN2TH*( HELP )
     +      - (VF(8)-AF(8))/4D0/SIN2TH*2D0*RT**2*C1
      GI(2) =  1D0/6D0/SIN2TH*( HELP - 2D0*RT**2*C1 )
      HELP = -1.5D0 + 12D0*C9 - 2D0*QSQR*( C10 - C8 ) + 4D0*QSQR*C7
      FI(3) = -  CW/4D0/SIN2TH/SW * HELP
      GI(3) = - 1D0/4D0/SIN2TH    * HELP
      HELP = RT**2/RMW**2*( -.75D0 + DLOG(RMW/RT) + 2D0*C4 -
     +       QSQR*( C5 - C3 ) )
      FI(4) = (VF(8)-AF(8))/4D0/SIN2TH*HELP -
     +        (VF(8)+AF(8))/4D0/SIN2TH*RT**4/RMW**2*C1
      GI(4) = 1D0/6D0/SIN2TH*( HELP - RT**4/RMW**2*C1 )
      HELP  = RT**2/RMW**2*( - .25D0 + 2D0*C9 )
      FI(5) = (SIN2TH-CW2)/8D0/SIN2TH/SW/CW*HELP
      GI(5) = - 1D0/4D0/SIN2TH*HELP
      FI(6) = - RT**2/4D0/SW/CW *C6
      GI(6) =   RT**2/4D0/SIN2TH*C6
      FI(7) = FI(6)
      GI(7) = GI(6)
      TOT1 = 0D0
      TOT2 = 0D0
      DO 10 I = 1 , 7
         TOT1 = TOT1 + FI(I)
         TOT2 = TOT2 + GI(I)
   10 CONTINUE
      FZL9 = TOT1
      FGL9 = TOT2
      END

      FUNCTION B0BAR(QSQR,RM1,RM2)
*     --------------
* eq. (C.15)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 DCMPLX,B0BAR
      IF(DABS(RM1-RM2) .GT. 1D-10) THEN
         B0BAR = 1D0 - (RM1**2+RM2**2)/(RM1**2-RM2**2)*DLOG(RM1/RM2) +
     +           DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ELSE
         B0BAR = DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ENDIF
      END

      FUNCTION B1BAR(QSQR,RM1,RM2)
*     --------------
* eq. (C.16)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 DCMPLX,B1BAR
      IF(DABS(RM1-RM2) .GT. 1D-10) THEN
         B1BAR = -.25D0 + RM1**2/(RM1**2-RM2**2)*DLOG(RM1/RM2) +
     +           ( RM2**2-RM1**2-QSQR )/2D0/QSQR*
     +            DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ELSE
         B1BAR = -.25D0 + .5D0 +
     +           ( RM2**2-RM1**2-QSQR )/2D0/QSQR*
     +            DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ENDIF
      END

      FUNCTION C0SCAL(QSQR,RM1,RM2,RMF)
*     ---------------
* The scalar 3 point function with equal external masses.
* eq. (5.10), (C.17)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 X(1:3),Y(1:3,1:2),HELP,AA,BB,CC,DD,FF,TOT,C0SCAL
      COMPLEX*16 SPENCF,DCMPLX
      AA = DCMPLX(RMF**2,0D0)
      BB = DCMPLX(QSQR,0D0)
      CC = - BB
      DD = DCMPLX(RM1**2 - RM2**2 - RMF**2,0D0)
      FF = DCMPLX(RM2**2,-1D-15)
      ALPHA = 2D0*RMF**2/QSQR/( 1D0 + DSQRT(1D0-4D0*RMF**2/QSQR) )
      X(1) = - ( DD + 2D0*AA + CC*ALPHA )/(CC+2D0*ALPHA*BB)
      X(2) = - DD/( (1D0-ALPHA)*(CC+2D0*ALPHA*BB) )
      X(3) = DD/ALPHA/(CC+2D0*ALPHA*BB)
      HELP = CDSQRT( CC**2 - 4D0*BB*( AA + DD + FF ) )
      IF(DREAL(CC) .GE. 0D0) THEN
         Y(1,1) = ( - CC - HELP )/2D0/BB
         Y(1,2) = 4D0*BB*( AA + DD + FF )/(-CC-HELP)/2D0/BB
      ELSE
         Y(1,1) = 4D0*BB*( AA + DD + FF )/(-CC+HELP)/2D0/BB
         Y(1,2) = ( - CC + HELP )/2D0/BB
      ENDIF
      HELP = CDSQRT( DD**2 - 4D0*FF*( AA + BB + CC ) )
      IF(DREAL(DD) .GE. 0D0) THEN
         Y(2,1) = ( - DD - HELP )/2D0/AA
         Y(2,2) = 4D0*FF*( AA + BB + CC )/(-DD-HELP)/2D0/AA
      ELSE
         Y(2,1) = 4D0*FF*( AA + BB + CC )/(-DD+HELP)/2D0/AA
         Y(2,2) = ( - DD + HELP )/2D0/AA
      ENDIF
      Y(3,1) = Y(2,1)
      Y(3,2) = Y(2,2)
      TOT = 0D0
      DO 20 J = 1 , 2
         DO 10 L = 1 , 3
            TOT = TOT + (-1D0)**L*(SPENCF(  X(L)     /(X(L)-Y(L,J)) )
     +                            -SPENCF( (X(L)-1D0)/(X(L)-Y(L,J)) ) )
   10    CONTINUE
   20 CONTINUE
      C0SCAL = TOT / ( CC + 2D0*ALPHA*BB )
      END

      FUNCTION C1PLUS(QSQR,RM1,RM2,RMF)
*     ---------------
* eq. (C.9)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 C0SCAL,HELP,B0BAR,C1PLUS
      HELP = DLOG(RM2/RM1) + B0BAR(QSQR,RM1,RM1) -
     +       B0BAR(RMF**2,RM1,RM2) + (RM2**2-RM1**2+RMF**2)*
     +       C0SCAL(QSQR,RM1,RM2,RMF)
      C1PLUS = HELP / ( 4D0*RMF**2 - QSQR )
      END

      FUNCTION C2ZERO(QSQR,RM1,RM2,RMF)
*     ---------------
* eq. (C.9)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 B0BAR,C0SCAL,C1PLUS,C2ZERO
      C2ZERO = .25D0*( B0BAR(QSQR,RM1,RM1) + 1D0 ) +
     +         .5D0*( RM1**2 - RM2**2 - RMF**2 )*
     +         C1PLUS(QSQR,RM1,RM2,RMF) + .5D0*RM2**2*
     +         C0SCAL(QSQR,RM1,RM2,RMF)
      END

      FUNCTION C2PLUS(QSQR,RM1,RM2,RMF)
*     ---------------
* eq. (C.9)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 B0BAR,B1BAR,C1PLUS,C2ZERO,HELP,C2PLUS
      HELP = .5D0*B0BAR(QSQR,RM1,RM1) + .5D0*( B1BAR(RMF**2,RM2,RM1)
     +       - .25D0 ) + ( RM2**2-RM1**2+RMF**2 )*
     +       C1PLUS(QSQR,RM1,RM2,RMF) - C2ZERO(QSQR,RM1,RM2,RMF)
      C2PLUS = HELP / ( 4D0*RMF**2 - QSQR )
      END

      FUNCTION C2MIN(QSQR,RM1,RM2,RMF)
*     --------------
* eq. (C.9)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 HELP,B1BAR,C2ZERO,C2MIN
      HELP = -.5D0*( B1BAR(RMF**2,RM2,RM1) - .25D0 ) -
     +         C2ZERO(QSQR,RM1,RM2,RMF)
      C2MIN = HELP / QSQR
      END

      FUNCTION SPENCF(X)
*     ---------------
* Hans Kuijf, 1988
* SPENCF(X) calcs the complex spence-function, through mapping on
* the area where there is a quickly convergent series.
      REAL*8 PI
      COMPLEX*16 X, SPENC, SPENCF
      PI=4D0*DATAN(1D0)
* Map the x on the unit circle.
* But so that x is not in the neighbourhood of (1,0)
* ABS(Z)=-CDLOG(1D0-X) is always smaller than 1.10
* But (1.10)^19/(19!)*bernoulli(19)=2.7D-15
      IF (CDABS(1D0-X).LT.1D-13) THEN
        SPENCF=PI*PI/6D0
      ELSE IF (CDABS(1D0-X).LT.0.5D0) THEN
        SPENCF=PI*PI/6D0-CDLOG(1D0-X)*CDLOG(X)-SPENC(1D0-X)
      ELSE IF (CDABS(X).GT.1D0) THEN
        SPENCF=-PI*PI/6D0-0.5D0*CDLOG(-X)*CDLOG(-X)-SPENC(1D0/X)
      ELSE
        SPENCF = SPENC(X)
      END IF
      END

      FUNCTION SPENC(X)
      COMPLEX*16 X,SUM,Z,Z2,SPENC
      Z=-CDLOG(1D0-X)
      Z2=Z*Z
* Horner's rule for the powers z^3 through z^19
      SUM=43867D0/798D0
      SUM=SUM*Z2/342D0-3617D0/510D0
      SUM=SUM*Z2/272D0+7D0/6D0
      SUM=SUM*Z2/210D0-691D0/2730D0
      SUM=SUM*Z2/156D0+5D0/66D0
      SUM=SUM*Z2/110D0-1D0/30D0
      SUM=SUM*Z2/ 72D0+1D0/42D0
      SUM=SUM*Z2/ 42D0-1D0/30D0
      SUM=SUM*Z2/ 20D0+1D0/6D0
* The first three terms of the power series
      SUM=Z2*Z*SUM/6D0-0.25D0*Z2+Z
      SPENC=SUM
      END

* --- From here on: general purpose routines from other authors.

      FUNCTION DILOG(X)
*     --------------
      IMPLICIT REAL*8(A-H,O-Z)
      Z=-1.644934066848226D0
      IF(DABS(X-1.D0) .LE. 1.D-17) THEN
         DILOG=1.644934066848226D0
      ELSE
         IF(X.LE.-1.D0 .OR. X.GT.2.D0)THEN
            IF(X.GT.2.D0) Z=3.289868133696453D0
            T=1.D0/X
            S=-0.5D0
            Z=Z-0.5D0*DLOG(DABS(X))**2
         ELSEIF(X .LE. 0.5D0)THEN
            T=X
            S=0.5D0
            Z=0.D0
         ELSEIF(X .LE. 2.D0)THEN
            T=1.D0-X
            S=-0.5D0
            Z=1.644934066848226D0-DLOG(X)*DLOG(DABS(T))
         ENDIF
         Y=2.666666666666667D0*T+0.666666666666667D0
         B=      0.000000000000001D0
         A=Y*B  +0.000000000000004D0
         B=Y*A-B+0.000000000000011D0
         A=Y*B-A+0.000000000000037D0
         B=Y*A-B+0.000000000000121D0
         A=Y*B-A+0.000000000000398D0
         B=Y*A-B+0.000000000001312D0
         A=Y*B-A+0.000000000004342D0
         B=Y*A-B+0.000000000014437D0
         A=Y*B-A+0.000000000048274D0
         B=Y*A-B+0.000000000162421D0
         A=Y*B-A+0.000000000550291D0
         B=Y*A-B+0.000000001879117D0
         A=Y*B-A+0.000000006474338D0
         B=Y*A-B+0.000000022536705D0
         A=Y*B-A+0.000000079387055D0
         B=Y*A-B+0.000000283575385D0
         A=Y*B-A+0.000001029904264D0
         B=Y*A-B+0.000003816329463D0
         A=Y*B-A+0.000014496300557D0
         B=Y*A-B+0.000056817822718D0
         A=Y*B-A+0.000232002196094D0
         B=Y*A-B+0.001001627496164D0
         A=Y*B-A+0.004686361959447D0
         B=Y*A-B+0.024879322924228D0
         A=Y*B-A+0.166073032927855D0
         A=Y*A-B+1.935064300869969D0
         DILOG=S*T*(A-B)+Z
      ENDIF
      END

      FUNCTION HADRQQ(S)
C  HADRONIC IRREDUCIBLE QQ SELF-ENERGY: TRANSVERSE
C     parametrize the real part of the photon self energy function
C     by  a + b ln(1+C*|S|) , as in my 1981 TASSO note but using
C     updated values, extended using RQCD up to 100 TeV
C     for details see:
C     H.Burkhardt, F.Jegerlehner, G.Penso and C.Verzegnassi
C     in CERN Yellow Report on "Polarization at LEP" 1988
C               H.BURKHARDT, CERN/ALEPH, AUGUST 1988
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 HADRQQ
C
      DATA A1,B1,C1/   0.0   ,   0.00835,  1.0   /
      DATA A2,B2,C2/   0.0   ,   0.00238,  3.927 /
      DATA A3,B3,C3/ 0.00165 ,   0.00300,  1.0   /
      DATA A4,B4,C4/ 0.00221 ,   0.00293,  1.0   /
C
      DATA PI/3.141592653589793/,ALFAIN/137.0359895D0/,INIT/0/
C
      IF(INIT.EQ.0) THEN
        INIT=1
        ALFA=1./ALFAIN
      ENDIF
      T=ABS(S)
      IF(T.LT.0.3**2) THEN
        REPIAA=A1+B1*LOG(1.+C1*T)
      ELSEIF(T.LT.3.**2) THEN
        REPIAA=A2+B2*LOG(1.+C2*T)
      ELSEIF(T.LT.100.**2) THEN
        REPIAA=A3+B3*LOG(1.+C3*T)
      ELSE
        REPIAA=A4+B4*LOG(1.+C4*T)
      ENDIF
C     as imaginary part take -i alfa/3 Rexp
      HADRQQ=REPIAA-(0.,1.)*ALFA/3.*REXP(S)
      END

      FUNCTION REXP(S)
C  HADRONIC IRREDUCIBLE QQ SELF-ENERGY: IMAGINARY
      IMPLICIT REAL*8(A-H,O-Z)
C     continuum R = Ai+Bi W ,  this + resonances was used to calculate
C     the dispersion integral. Used in the imag part of HADRQQ
      PARAMETER (NDIM=18)
      DIMENSION WW(NDIM),RR(NDIM),AA(NDIM),BB(NDIM)
      DATA WW/1.,1.5,2.0,2.3,3.73,4.0,4.5,5.0,7.0,8.0,9.,10.55,
     .  12.,50.,100.,1000.,10 000.,100 000./
      DATA RR/0.,2.3,1.5,2.7,2.7,3.6,3.6,4.0,4.0,3.66,3.66,3.66,
     .   4.,3.87,3.84, 3.79, 3.76,    3.75/
      DATA INIT/0/
      IF(INIT.EQ.0) THEN
        INIT=1
C       calculate A,B from straight lines between R measurements
        BB(NDIM)=0.
        DO 4 I=1,NDIM
          IF(I.LT.NDIM) BB(I)=(RR(I)-RR(I+1))/(WW(I)-WW(I+1))
          AA(I)=RR(I)-BB(I)*WW(I)
    4   CONTINUE
      ENDIF
      REXP=0.D0
      IF(S.GT.0.D0) THEN
        W=REAL(SQRT(S))
        IF(W.GT.WW(1)) THEN
          DO 2 I=1,NDIM
C           find out between which points of the RR array W is
            K=I
            IF(I.LT.NDIM) THEN
              IF(W.LT.WW(I+1)) GOTO 3
            ENDIF
    2     CONTINUE
    3     CONTINUE
          REXP=AA(K)+BB(K)*W
        ENDIF
      ENDIF
      END

      FUNCTION ZBRENT(FUNC,X1,X2,TOL)
*     ---------------
* Ref.: William H. Press et.al., Numerical Recipes,
*       Cambridge Univ. Press, 1987.
* Using Brent's method,  find the root of a function FUNC known to
* lie between X1 and X2. The root, returned as ZBRENT, will be refined
* untill its accuracy is TOL.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER ( ITMAX=100 , EPS=3D-8 )
      EXTERNAL FUNC
      A  = X1
      B  = X2
      FA = FUNC(A)
      FB = FUNC(B)
      IF(FB*FA.GT.0D0) THEN
*       Root is not bracketed. Return the value closest to 0.
        WRITE(*,'(A)')' ZBRENT: the root is not bracketed.'
        ZBRENT = A
        IF ( ABS(FB) .LT. ABS(FA) ) ZBRENT = B
        RETURN
      ENDIF
      FC = FB
      DO 10 ITER=1,ITMAX
        IF(FB*FC.GT.0D0) THEN
*         Rename A,B,C and adjust bounding interval D.
          C = A
          FC= FA
          D = B-A
          E = D
        ENDIF
        IF(ABS(FC).LT.ABS(FB)) THEN
          A =B
          B =C
          C =A
          FA=FB
          FB=FC
          FC=FA
        ENDIF
        TOL1=2D0*EPS*ABS(B)+0.5D0*TOL
        XM=.5D0*(C-B)
*       Convergence check:
        IF(ABS(XM).LE.TOL1 .OR. DABS(FB).LT.1D-20) GOTO 999
        IF(ABS( E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
*         Attempt inverse quadratic interpolation
          S=FB/FA
          IF(DABS(A-C).LT.1D-20) THEN
            P=2.*XM*S
            Q=1D0-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.*XM*Q*(Q-R)-(B-A)*(R-1D0))
            Q=(Q-1D0)*(R-1D0)*(S-1D0)
          ENDIF
          IF(P.GT.0D0) Q=-Q
*         Check whether in bounds
          P=ABS(P)
          IF(2D0*P .LT. MIN(3D0*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
*           Accept interpolation
            E=D
            D=P/Q
          ELSE
*           Interpolation failed, use bisection.
            D=XM
            E=D
          ENDIF
        ELSE
*         Bounds decreasing too slowly, use bisection.
          D=XM
          E=D
        ENDIF
*       Move last guess to A
        A=B
        FA=FB
        IF(ABS(D) .GT. TOL1) THEN
*         Evaluate new trial root
          B=B+D
        ELSE
          B=B+SIGN(TOL1,XM)
        ENDIF
        FB=FUNC(B)
   10 CONTINUE
      WRITE(*,'(A,2G18.8)')' ZBRENT exceeding maximum iterations.',A,B
  999 ZBRENT=B
      END

      FUNCTION DGAUSS(F,A,B,EPS)
C.----------------------------------------------------------------------
C.
C.    GAUSS INTEGRAL OF THE FUNCTION F IN INTERVAL A,B
C.    LAST UPDATE: 12/03/87
C.
C.----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION W(12),X(12)
      EXTERNAL F
      DATA CONST/1.E-12/
      DATA W
     &/0.101228536290376, 0.222381034453374, 0.313706645877887,
     & 0.362683783378362, 0.027152459411754, 0.062253523938648,
     & 0.095158511682493, 0.124628971255534, 0.149595988816577,
     & 0.169156519395003, 0.182603415044924, 0.189450610455069/
      DATA X
     &/0.960289856497536, 0.796666477413627, 0.525532409916329,
     & 0.183434642495650, 0.989400934991650, 0.944575023073233,
     & 0.865631202387832, 0.755404408355003, 0.617876244402644,
     & 0.458016777657227, 0.281603550779259, 0.095012509837637/
C--
C--   INITIALISE
      DELTA=CONST*ABS(A-B)
      DGAUSS=0.
      AA=A
C--
C--   ITERATION LOOP
   10 Y=B-AA
C--
C--   EPSILON REACHED ??
      IF (ABS(Y).LE.DELTA) RETURN
   20 BB=AA+Y
      C1=0.5*(AA+BB)
      C2=C1-AA
      S8=0.
      S16=0.
      DO 30 I=1,4
         U=X(I)*C2
   30 S8=S8+W(I)*(F(C1+U)+F(C1-U))
      DO 40 I=5,12
         U=X(I)*C2
   40 S16=S16+W(I)*(F(C1+U)+F(C1-U))
      S8=S8*C2
      S16=S16*C2
      IF (ABS(S16-S8).GT.EPS*(1.0+ABS(S16))) GOTO 50
      DGAUSS=DGAUSS+S16
      AA=BB
      GOTO 10
   50 Y=0.5*Y
      IF (ABS(Y).GT.DELTA) GOTO 20
      WRITE (6,9000)
      DGAUSS=0.
      RETURN
 9000 FORMAT(1H ,'****** DGAUSS... TOO HIGH ACCURACY REQUIRED ******')
      END

      SUBROUTINE VEGAS(FXN,AVGI,SD,CHI2A)
C
C   SUBROUTINE PERFORMS N-DIMENSIONAL MONTE CARLO INTEG'N
C      - BY G.P. LEPAGE   SEPT 1976/(REV)APR 1978
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/BVEG1/XL(10),XU(10),ACC,NDIM,NCALL,ITMX,NPRN
      COMMON/BVEG2/XI(50,10),SI,SI2,SWGT,SCHI,NDO,IT
      DIMENSION D(50,10),DI(50,10),XIN(50),R(50),DX(10),DT(10),X(10)
     1   ,KG(10),IA(10)
      REAL*8 QRAN(10)
      DATA NDMX/50/,ALPH/1.5D0/,ONE/1D0/,MDS/1/
C
      NDO=1
      DO 1 J=1,NDIM
1     XI(1,J)=ONE
C
      ENTRY VEGAS1(FXN,AVGI,SD,CHI2A)
C         - INITIALIZES CUMMULATIVE VARIABLES, BUT NOT GRID
      IT=0
      SI=0.
      SI2=SI
      SWGT=SI
      SCHI=SI
C
      ENTRY VEGAS2(FXN,AVGI,SD,CHI2A)
C         - NO INITIALIZATION
      ND=NDMX
      NG=1
      IF(MDS.EQ.0) GO TO 2
      NG=(NCALL/2.)**(1./NDIM)
      MDS=1
      IF((2*NG-NDMX).LT.0) GO TO 2
      MDS=-1
      NPG=NG/NDMX+1
      ND=NG/NPG
      NG=NPG*ND
2     K=NG**NDIM
      NPG=NCALL/K
      IF(NPG.LT.2) NPG=2
      CALLS=NPG*K
      DXG=ONE/NG
      DV2G=(CALLS*DXG**NDIM)**2/NPG/NPG/(NPG-ONE)
      XND=ND
      NDM=ND-1
      DXG=DXG*XND
      XJAC=ONE/CALLS
      DO 3 J=1,NDIM
      DX(J)=XU(J)-XL(J)
3     XJAC=XJAC*DX(J)
C
C   REBIN PRESERVING BIN DENSITY
C
      IF(ND.EQ.NDO) GO TO 8
      RC=NDO/XND
      DO 7 J=1,NDIM
      K=0
      XN=0.
      DR=XN
      I=K
4     K=K+1
      DR=DR+ONE
      XO=XN
      XN=XI(K,J)
5     IF(RC.GT.DR) GO TO 4
      I=I+1
      DR=DR-RC
      XIN(I)=XN-(XN-XO)*DR
      IF(I.LT.NDM) GO TO 5
      DO 6 I=1,NDM
6     XI(I,J)=XIN(I)
7     XI(ND,J)=ONE
      NDO=ND
C
8     IF(NPRN.NE.0) WRITE(6,200) NDIM,CALLS,IT,ITMX,ACC,MDS,ND
     1                           ,(XL(J),XU(J),J=1,NDIM)
C
      ENTRY VEGAS3(FXN,AVGI,SD,CHI2A)
C         - MAIN INTEGRATION LOOP
9     IT=IT+1
      TI=0.
      TSI=TI
      DO 10 J=1,NDIM
      KG(J)=1
      DO 10 I=1,ND
      D(I,J)=TI
10    DI(I,J)=TI
C
11    FB=0.
      F2B=FB
      K=0
12    K=K+1
      CALL ARAN9(QRAN,NDIM)
      WGT=XJAC
      DO 15 J=1,NDIM
      XN=(KG(J)-QRAN(J))*DXG+ONE
      IA(J)=XN
      IF(IA(J).GT.1) GO TO 13
      XO=XI(IA(J),J)
      RC=(XN-IA(J))*XO
      GO TO 14
13    XO=XI(IA(J),J)-XI(IA(J)-1,J)
      RC=XI(IA(J)-1,J)+(XN-IA(J))*XO
14    X(J)=XL(J)+RC*DX(J)
15    WGT=WGT*XO*XND
C
      F=WGT
      F=F*FXN(X,WGT)
      F2=F*F
      FB=FB+F
      F2B=F2B+F2
      DO 16 J=1,NDIM
      DI(IA(J),J)=DI(IA(J),J)+F
16    IF(MDS.GE.0) D(IA(J),J)=D(IA(J),J)+F2
      IF(K.LT.NPG) GO TO 12
C
      F2B=DSQRT(F2B*NPG)
      F2B=(F2B-FB)*(F2B+FB)
      TI=TI+FB
      TSI=TSI+F2B
      IF(MDS.GE.0) GO TO 18
      DO 17 J=1,NDIM
17    D(IA(J),J)=D(IA(J),J)+F2B
18    KNEW=NDIM
19    KG(KNEW)=MOD(KG(KNEW),NG)+1
      IF(KG(KNEW).NE.1) GO TO 11
      KNEW=KNEW-1
      IF(KNEW.GT.0) GO TO 19
C
C   FINAL RESULTS FOR THIS ITERATION
C
      TSI=TSI*DV2G
      TI2=TI*TI
      WGT=TI2/TSI
      SI=SI+TI*WGT
      SI2=SI2+TI2
      SWGT=SWGT+WGT
      SCHI=SCHI+TI2*WGT
      AVGI=SI/SWGT
      SD=SWGT*IT/SI2
      CHI2A=SD*(SCHI/SWGT-AVGI*AVGI)/(IT-.999)
      SD=DSQRT(ONE/SD)
C
      IF(NPRN.EQ.0) GO TO 21
      TSI=DSQRT(TSI)
      WRITE(6,201) IT,TI,TSI,AVGI,SD,CHI2A
      IF(NPRN.GE.0) GO TO 21
      DO 20 J=1,NDIM
20    WRITE(6,202) J,(XI(I,J),DI(I,J),D(I,J),I=1,ND)
C
C   REFINE GRID
C
21    DO 23 J=1,NDIM
      XO=D(1,J)
      XN=D(2,J)
      D(1,J)=(XO+XN)/2.
      DT(J)=D(1,J)
      DO 22 I=2,NDM
      D(I,J)=XO+XN
      XO=XN
      XN=D(I+1,J)
      D(I,J)=(D(I,J)+XN)/3.
22    DT(J)=DT(J)+D(I,J)
      D(ND,J)=(XN+XO)/2.
23    DT(J)=DT(J)+D(ND,J)
C
      DO 28 J=1,NDIM
      RC=0.
      DO 24 I=1,ND
      R(I)=0.
      IF(D(I,J).LE.0.) GO TO 24
      XO=DT(J)/D(I,J)
      R(I)=((XO-ONE)/XO/DLOG(XO))**ALPH
24    RC=RC+R(I)
      RC=RC/XND
      K=0
      XN=0.
      DR=XN
      I=K
25    K=K+1
      DR=DR+R(K)
      XO=XN
      XN=XI(K,J)
26    IF(RC.GT.DR) GO TO 25
      I=I+1
      DR=DR-RC
      XIN(I)=XN-(XN-XO)*DR/R(K)
      IF(I.LT.NDM) GO TO 26
      DO 27 I=1,NDM
27    XI(I,J)=XIN(I)
28    XI(ND,J)=ONE
C
      IF(IT.LT.ITMX.AND.ACC*DABS(AVGI).LT.SD) GO TO 9
200   FORMAT('0INPUT PARAMETERS FOR VEGAS:  NDIM=',I3,'  NCALL=',F8.0
     1    /28X,'  IT=',I5,'  ITMX=',I5/28X,'  ACC=',G9.3
     2    /28X,'  MDS=',I3,'   ND=',I4/28X,'  (XL,XU)=',
     3    (T40,'( ',G12.6,' , ',G12.6,' )'))
201   FORMAT(///' INTEGRATION BY VEGAS' / '0ITERATION NO.',I3,
     1    ':   INTEGRAL =',G14.8/21X,'STD DEV  =',G10.4 /
     2    ' ACCUMULATED RESULTS:   INTEGRAL =',G14.8 /
     3    24X,'STD DEV  =',G10.4 / 24X,'CHI**2 PER IT''N =',G10.4)
202   FORMAT('0DATA FOR AXIS',I2 / ' ',6X,'X',7X,'  DELT I  ',
     1    2X,' CONV''CE  ',11X,'X',7X,'  DELT I  ',2X,' CONV''CE  '
     2   ,11X,'X',7X,'  DELT I  ',2X,' CONV''CE  ' /
     2    (' ',3G12.4,5X,3G12.4,5X,3G12.4))
      RETURN
      END
      SUBROUTINE SAVE(NDIM)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/BVEG2/XI(50,10),SI,SI2,SWGT,SCHI,NDO,IT
C
C   STORES VEGAS DATA (UNIT 7) FOR LATER RE-INITIALIZATION
C
      WRITE(7,200) NDO,IT,SI,SI2,SWGT,SCHI,
     1             ((XI(I,J),I=1,NDO),J=1,NDIM)
      RETURN
      ENTRY RESTR(NDIM)
C
C   ENTERS INITIALIZATION DATA FOR VEGAS
C
      READ(7,200) NDO,IT,SI,SI2,SWGT,SCHI,
     1            ((XI(I,J),I=1,NDO),J=1,NDIM)
200   FORMAT(2I8,4Z16/(5Z16))
      RETURN
      END

      SUBROUTINE ARAN9(QRAN,NDIM)
      REAL*8 QRAN(10)
      DO 1 I=1,NDIM
         CALL RANMAR(QRAN(I))
    1 CONTINUE
      RETURN
      END

      BLOCK DATA VEGASS
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( IXDIM = 10 )
      COMMON/BVEG1/XL(10),XU(10),ACC,NDIM,NCALL,ITMX,NPRN
      COMMON/BVEG2/XI(50,10),SI,SI2,SWGT,SCHI,NDO,IT
      DATA NCALL/5000/ITMX/5/NPRN/5/ACC/-1D0/XL/IXDIM*0D0/XU/IXDIM*1D0/
      END
!---------------------------------------------------------------!
!     This is the original BABAMC  program with a few minor     !
!     modifications by W. Placzek (last update: 08.02.1996).    !
!---------------------------------------------------------------!
*BERENDS-HOLLIK-KLEISS BHABHA GENERATOR
*PATCH,TITLE.
************************************************************************
*  Berends, Hollik, Kleiss Bhabha Monte Carlo
*
*  26.04.88 14:00 DAM - PAM file created from FORTRAN source code reciev
*                       from W. Hollik earlier the same month.
*                     - Subroutine INT has been renamed XINT to avoid
*                       confusion with intrinsic INT.
*  27.04.88 16:45 DAM - Changed routine SETBAB, so that all inputs are s
*                       in main program. This required intruduction of a
*                       new COMMONs.
*                     - Seperated main program BAMAIN in order to put
*                       generator in library.
*                       To extract only generator: select *NORMAL
*                       To extract also main program: select
*                                                        additional BADE
*  29.04.88 13:05 DAM - Put some of the "experts" printout under control
*                       of the flag IFLDB ( .EQ.0 THEN no printout ).
*
************************************************************************
*PATCH,*NORMAL.                             TO GET ORIGINAL VERSION.
*USE,P=BHKBAB.
*PATCH,BADELPHI.
*DECK,BAMAIN.
*     PROGRAM BHKBAB
**********************************************************************
C THIS IS THE MAIN PROGRAM, CONSISTING OF:
C 1) INITIALIZATION OF THE GENERATOR;
C 2) GENERATION OF AN EVENT SAMPLE,
C    AND SUBSEQUENT ANALYSIS OF THE EVENTS;
C 3) EVALUATION OF THE TOTAL GENERATED CROSS SECTION
C
*  27.04.88 DAM
*    The main program have been changed slightly and seperated from
*    the rest to allow FFREAD'ing of input parameters and interfacing
*    to other programs.
*
C     IMPLICIT REAL*8(A-H,O-Z)
C     COMMON / INPUT1 / EB
C     COMMON / INPUT2 / XMZ,S2W,XMH,XMT
C     COMMON / INPUT3 / THMIN,THMAX,XKMAX
*............................................................. DAM
C     COMMON / NEVCOM / NEVENT
C     COMMON / XK0COM / XK0
C     COMMON / WMXCOM / WMAX
C     COMMON / REJCOM / IREJEC
*............................................................. DAM
C     COMMON / UNICOM / IIN,IUT
C     COMMON / DBCOM / IFLDB
C     DIMENSION PP(4),PM(4),QP(4),QM(4),QK(4)
C
C THE SETUP PHASE: ASK FOR THE INPUT PARAMETERS
C     IIN=5
CC    IUT=6
*
*
C     EB=46.
C     XMZ=920.D0
C     XMT=60.
C     XMH=100.
C     THMIN=.5729577D0
C     THMAX=5.729577D0
C     XKMAX=1.
C     NEVENT=80000
*
*..... Moved up from routine SETBAB ............................. DAM
C
C THE BOUNDARY BETWEEN SOFT AND HARD  BREMSSTRAHLUNG
CC    XK0=.01D0
C
C DEFINE THE W.R.P. PROCEDURE DEFAULTS
C     IREJEC=0
C     WMAX=0.01D0
*
*.... Control of debug printout
C     IFLDB = 1
*
*.... Set input parameters by FFREAD
*                      (values set above will be overwritten)
C     CALL BAREAD
*................................................................ DAM
C
C THE INITIALIZATION STEP OF THE PROGRAM
C     CALL SETBAB(EB,XMZ,XMH,XMT,THMIN,THMAX,XKMAX)
*
*.... Book histograms here ...................................... DAM
C     CALL HISTBK
*................................................................ DAM
C
C THE EVENT LOOP
C     CALL OUTCRY('GENBAB')
C     DO 900 K=1,NEVENT
C       CALL TELLER(K,1000,'EVENT LOOP')
C       CALL GENBAB(PP,PM,QP,QM,QK,W)
C       CALL CANCUT(QP,QM,QK,W)
C
*.... Fill histograms here ...................................... DAM
C       CALL HISTFL(PP,PM,QP,QM,QK,W)
*................................................................ DAM
*
C 900 CONTINUE
C
C EVALUATION OF THE GENERATED CROSS SECTION
C     CALL ENDBAB(SIGTOT,ERRTOT)
C     CALL EFFCIT
C     CALL ENDCUT(SIGTOT)
*
*.... Output histograms here .................................... DAM
C     CALL HISTUT
*................................................................ DAM
*
C     STOP
C     END
*DECK,HISTBK.                    HISTOGRAM BOOKING ROUTINE
      SUBROUTINE HISTBK
*
*.... Book histograms here
      RETURN
      END
*DECK,HISTFL.                    HISTOGRAM FILLING ROUTINE
      SUBROUTINE HISTFL(PP,PM,QP,QM,QK,W)
*
*.... Fill histograms here
      RETURN
      END
*DECK,HISTUT.                    OUTPUT OF HISTOGRAMS
      SUBROUTINE HISTUT
*
*.... Output histograms here
      RETURN
      END
*DECK,BAREAD.
      SUBROUTINE BAREAD
*
*     Routine to set input values by FFREAD.         27/04/88   DAM
*
      REAL*8 EB,XMZ,S2W,XMH,XMT,THMIN,THMAX,XKMAX,WMAX,XK0
      REAL*4 X(10)
      COMMON / INPUT1 / EB
      COMMON / INPUT2 / XMZ,S2W,XMH,XMT
      COMMON / INPUT3 / THMIN,THMAX,XKMAX
      COMMON / NEVCOM / NEVENT
      COMMON / WMXCOM / WMAX
      COMMON / XK0COM / XK0
      COMMON / REJCOM / IREJEC
      COMMON / DBCOM  / IFLDB
*
      COMMON / CFREAD / SPACE(1000)
*     CALL FFINIT(1000)
*     CALL FFSET('SIZE',6)
*     CALL FFSET('LOUT',6)
*     CALL FFSET('LINP',3)
*
*.... Conversion to REAL*4 for FFREAD
      X(1) = EB
      X(2) = XMZ
      X(3) = XMT
      X(4) = XMH
      X(5) = THMIN
      X(6) = THMAX
      X(7) = XK0
      X(8) = XKMAX
      X(9) = WMAX
*
*     CALL FFKEY( 'EB'     , X(1) ,1,'REAL')
*     CALL FFKEY( 'XMZ'    , X(2) ,1,'REAL')
*     CALL FFKEY( 'XMT'    , X(3) ,1,'REAL')
*     CALL FFKEY( 'XMH'    , X(4) ,1,'REAL')
*     CALL FFKEY( 'THMIN'  , X(5) ,1,'REAL')
*     CALL FFKEY( 'THMAX'  , X(6) ,1,'REAL')
*     CALL FFKEY( 'XK0'    , X(7) ,1,'REAL')
*     CALL FFKEY( 'XKMAX'  , X(8) ,1,'REAL')
*     CALL FFKEY( 'NEVENT' , NEVENT ,1,'INTEGER ')
C
*     CALL FFKEY( 'IREJEC'  , IREJEC ,1,'INTEGER ')
*     CALL FFKEY( 'WMAX'    , X(9)   ,1,'REAL')
*     CALL FFKEY( 'IFLDB'   , IFLDB  ,1,'INTEGER ')
*
*     CALL FFGO
*
*.... Then back to REAL*8
      EB    = X(1)
      XMZ   = X(2)
      XMT   = X(3)
      XMH   = X(4)
      THMIN = X(5)
      THMAX = X(6)
      XK0   = X(7)
      XKMAX = X(8)
      WMAX  = X(9)
*
      RETURN
      END
      SUBROUTINE SETUP1(E,XK0,XK1,SIGMA)
C
C SUBGENERATOR 1:
C INITIAL-STATE RADIATION IN QED S-CHANNEL GRAPHS
C E = BEAM ENERGY IN GEV
C XK0 = MINIMUM PHOTON ENERGY (FRACTION OF E)
C XK1 = MAXIMUM PHOTON ENERGY (FRACTION OF E)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C COMMON BLOCK FOR TRANSFER TO 'GENER1'
      COMMON / G1COM / EBEAM,   EPS,
     .                 YK1,  YK2,  YK3,  YZ1,  YZ2,  YF1,
     .                 TRYK,  PASK
      COMMON / UNICOM / IIN,IUT
      COMMON / DBCOM / IFLDB
C
      CALL OUTCRY('SETUP1')

C
C CHECK ON PHOTON ENERGIES
      IF(XK1.LE.XK0) THEN
        SIGMA=0.D0
        RETURN
      ENDIF
C
C DEFINE CONSTANTS
      EBEAM=E
      XME=0.000511D0
      PI=3.1415926536D0
      EEL=DSQRT(4.*PI/137.036D0)
      FAC=4970.392/E**2*2.*EEL**6*PI**2
      EPS=(XME/E)**2*.5D0
      XKMAX=1.-DEXP(5.D0/3.)/2.*EPS
C
C TOTAL APPROXIMATE CROSS SECTION
      E0=E*XK0
      IF(XK1.LE.XKMAX) GOTO 2
      WRITE(IUT,1) XKMAX
    1 FORMAT(' SETUP1 : THE MAXIMUM PHOTON ENERGY IS TOO HIGH;'/,
     .       ' SETUP1 : I CHANGE ITS VALUE TO',D30.20)
      XK1=XKMAX
    2 CONTINUE
      EMAX=XK1*E
      SIGMA=FAC*DLOG(4.*E**2/XME**2)*
     . (2.*DLOG(EMAX/E0) + DLOG((E-E0)/(E-EMAX))
     . -(EMAX-E0)/E )
C
C INITIALIZE CONSTANTS FOR EVENT GENERATION
      YK1=DLOG(E0/(E-E0))
      YK2=DLOG(EMAX/E0*(E-E0)/(E-EMAX))
      YK3=E**2+(E-E0)**2
      YZ1=DLOG(EPS)
      YZ2=DLOG((2.+EPS)/EPS)
      YF1=2.*PI
C
C THE EFFICIENCY OF THE PHOTON ENERGY LOOP
      EFFK=(2.*DLOG(EMAX/E0)+DLOG((E-E0)/(E-EMAX))
     . -(EMAX-E0)/E )/(1.+(1.-E0/E)**2)
     . /DLOG(EMAX*(E-E0)/E0/(E-EMAX))
      TRYK=0.
      PASK=0.
C
C PRINT THE RESULTS SO FAR
*                          Under control of the flag IFLDB
      IF(IFLDB.EQ.0) GO TO 777
      WRITE(IUT,3)   E,   XK0,   XK1,   XME,   PI,
     .           EEL,   FAC,   EPS, XKMAX,   E0,
     .          EMAX, SIGMA,   YK1,   YK2,  YK3,
     .           YZ1,   YZ2,   YF1,  EFFK, TRYK,
     .          PASK
    3 FORMAT(' SETUP1 :',4D14.6)
  777 CONTINUE
      RETURN
      END
*DECK,GENER1
      SUBROUTINE GENER1(QP,QM,QK,WM)
C
C GENERATION OF THE FINAL-STATE MOMENTA
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION QP(4),QM(4),QK(4),H(4)
      COMMON / EFFIC1 / EFFK
      COMMON / G1COM / E,  EPS,
     .                 YK1,  YK2,  YK3,  YZ1,  YZ2,  YF1,
     .                 TRYK,  PASK
C
C GENERATE THE PHOTON ENERGY BY MAPPING AND W.R.P.
    4 TRYK=TRYK+1.
      QK(4)=E/(1.+1./DEXP(YK1+RN(1.)*YK2))
      W=(E**2+(E-QK(4))**2)/YK3
      IF(W.LT.RN(2.)) GOTO 4
      PASK=PASK+1.
      EFFK=PASK/TRYK
C
C GENERATE THE PHOTON POLAR ANGLE (N.B. STABILITY)
      V=DEXP(YZ1+RN(3.)*YZ2)-EPS
      ZG=1.-V
      SG=DSQRT(V*(2.-V))
      IF(RN(4.).GT..5D0) ZG=-ZG
C
C GENERATE THE OTHER TRIVIAL VARIABLES
      FG=YF1*RN(5.)
      FR=YF1*RN(6.)
      CR=-1.+2.*RN(7.)
      SR=DSQRT(1.-CR*CR)
C
C CALCULATE THE MASS EFFECT FACTOR
      WM=1.-EPS/(EPS+V)*2.*E*(E-QK(4))/(E**2+(E-QK(4))**2)
C
C CONSTRUCT THE PHOTON MOMENTUM IN THE LAB
      QK(1)=QK(4)*SG*DSIN(FG)
      QK(2)=QK(4)*SG*DCOS(FG)
      QK(3)=QK(4)*ZG
C
C CONSTRUCT THE E+ MOMENTUM IN THE E+E- REST FRAME
      H(4)=E*DSQRT(1.-QK(4)/E)
      H(1)=H(4)*SR*DSIN(FR)
      H(2)=H(4)*SR*DCOS(FR)
      H(3)=H(4)*CR
      XH=H(4)**2-H(3)**2-H(2)**2-H(1)**2
C
C BOOST TO OBTAIN THE LAB FRAME VALUE
      QP(4)=((2.*E-QK(4))*H(4)
     .       -H(3)*QK(3)-H(2)*QK(2)-H(1)*QK(1))/(2.*H(4))
      R=(QP(4)+H(4))/(2.*E-QK(4)+2.*H(4))
      DO 5 I=1,3
    5 QP(I)=H(I)-QK(I)*R
C
C CONSTRUCT THE E- MOMENTUM FROM CONSERVATION
      QM(4)=2.*E-QP(4)-QK(4)
      DO 6 I=1,3
    6 QM(I)=-QP(I)-QK(I)
C
      RETURN
      END
*DECK,SETUP2
      SUBROUTINE SETUP2(E,XK0,XK1,XMZ,XGZ,GV,GA,SIGMA)
C
C SUBGENERATOR 2:
C FINAL-STATE RADIATION IN Z0 ANNIHILATION GRAPHS
C E = BEAM ENERGY IN GEV
C XK0 = MINIMUM PHOTON ENERGY (FRACTION OF E)
C XK1 = MAXIMUM PHOTON ENERGY (FRACTION OF E)
C XMZ = Z0 MASS IN GEV
C XGZ = Z0 WIDTH IN GEV
C GV = VECTOR COUPLING BETWEEN E AND Z0
C GA = AXIAL VECTOR COUPLING BETWEEN E AND Z0
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / G2COM / EBEAM,  XME,  E0,  EMAX,
     .                 YK1,  YK2,  YV1,  YF1,
     .                 TRYK,  PASK
      COMMON / UNICOM / IIN,IUT
      COMMON / DBCOM / IFLDB
C
C THE INTEGRATED PHOTON SPECTRUM
      H(X)=XL*(2.*DLOG(X)-2.*X+X**2/2.)-2.*DILOGY(X)
     . +(1.-X)*(3.-X)*DLOG(1.-X)/2.
     . -(1.-X)*(5.-X)/4.
C
      CALL OUTCRY('SETUP2')
C
C CHECK ON PHOTON ENERGIES
      IF(XK1.LE.XK0) THEN
        SIGMA=0.D0
        RETURN
      ENDIF
C
C DEFINE CONSTANTS
      EBEAM=E
      XME=0.511D-03
      PI=3.1415926536D0
      EEL=DSQRT(4.*PI/137.036)
      XL=DLOG(4.*E**2/XME**2)
C
C TOTAL APPROXIMATE CROSS SECTION
      E0=XK0*E
      EMAX=XK1*E
      HXK1=H(XK1)
      HXK0=H(XK0)
      SIGMA=4970.392/E**2
     . *8.*PI**2*E**2*(HXK1-HXK0)
     . *    (( EEL**2*(GV**4+6.*GV**2*GA**2+GA**4)*4.*E**2
     .        +EEL**4*(GV**2+GA**2)*(8.*E**2-2.*XMZ**2)   )/
     .                  ((4.*E**2-XMZ**2)**2+XMZ**2*XGZ**2)
     .                                    +EEL**6/(4.*E**2)   )
C
C INITIALIZE CONSTANTS FOR EVENT GENERATION
      YK1=XL
      YK2=(E**2+(E-E0)**2)*DLOG(4.*E*(E-E0)/XME**2)
      YV1=XME**2/4./E
      YF1=2.*PI
C
C THE EFFICIENCY OF THE PHOTON ENERGY LOOP
      TRYK=0.
      PASK=0.
C
C PRINT THE RESULTS SO FAR
*                          Under control of the flag IFLDB
      IF(IFLDB.EQ.0) GO TO 777
      WRITE(IUT,1) XME,    PI,   EEL,    XL,
     .            E0,  EMAX, SIGMA,   YK1,
     .           YK2,   YV1,   YF1,  TRYK,
     .          PASK
    1 FORMAT(' SETUP2 :',4D14.6)
  777 CONTINUE
      RETURN
      END
*DECK,GENER2
      SUBROUTINE GENER2(QP,QM,QK,WM)
C
C GENERATION OF THE FINAL-STATE MOMENTA
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION QP(4),QM(4),QK(4)
      COMMON / EFFIC2 / EFFK
      COMMON / G2COM / E,  XME,  E0,  EMAX,
     .                 YK1,  YK2,  YV1,  YF1,
     .                 TRYK,  PASK
C
C GENERATE THE PHOTON ENERGY BY MAPPING AND W.R.P.
    2 TRYK=TRYK+1.
      QK(4)=(EMAX/E0)**RN(1.)*E0
      W=(E**2+(E-QK(4))**2)*(YK1+DLOG(1.-QK(4)/E))/YK2
      IF(W.LT.RN(2.)) GOTO 2
      PASK=PASK+1.
      EFFK=PASK/TRYK
C
C GENERATE THE E+ ENERGY (N.B. NUMERICAL STABILITY)
      D=YV1*QK(4)/(E-QK(4))
      V=DEXP(DLOG(D)+RN(3.)*DLOG(1.+QK(4)/D))-D
      QP(4)=E-V
C
C GENERATE THE OTHER TRIVIAL VARIABLES
      FG=YF1*RN(4.)
      C=-1.+2.*RN(5.)
      FC=YF1*RN(6.)
C
C CALCULATE THE MASS EFFECT FACTOR
      WM=1.-XME**2/2./(V+D)*QK(4)/(E**2+(E-QK(4))**2)
C
C CONSTRUCT THE E+ MOMENTUM
      SC=DSQRT(1.-C*C)
      C1=DCOS(FC)
      S1=DSIN(FC)
      QP(1)=QP(4)*SC*S1
      QP(2)=QP(4)*SC*C1
      QP(3)=QP(4)*C
C
C CONSTRUCT THE PHOTON MOMENTUM
      VG=2.*(E-QK(4))*V/(QK(4)*(E-V))
      CG=VG-1.
      SG=DSQRT(VG*(2.-VG))
      HX=QK(4)*SG*DSIN(FG)
      HY=QK(4)*SG*DCOS(FG)
      HZ=QK(4)*CG
      XH=QK(4)**2-HX**2-HY**2-HZ**2
      QK(1)=  C1*HX + S1*C*HY + S1*SC*HZ
      QK(2)= -S1*HX + C1*C*HY + C1*SC*HZ
      QK(3)=           -SC*HY      +C*HZ
C
C CONSTRUCT E- MOMENTUM FROM CONSERVATION
      QM(4)=2.*E-QP(4)-QK(4)
      DO 3 I=1,3
    3 QM(I)=-QP(I)-QK(I)
C
C SYMMETRIZE THE E+ AND E- MOMENTA
      IF(RN(8.).LT..5D0) RETURN
      DO 4 I=1,4
        X=QP(I)
        QP(I)=QM(I)
    4 QM(I)=X
      RETURN
      END
*DECK,SETUP3
      SUBROUTINE SETUP3(E,XK0,XK1,XMZ,XGZ,GV,GA,SIGMA)
C
C SUBGENERATOR 3:
C INITIAL-STATE RADIATION IN Z0 ANNIHILATION GRAPH
C E = BEAM ENERGY IN GEV
C XK0 = MINIMUM PHOTON ENERGY (FRACTION OF E)
C XK1 = MAXIMUM PHOTON ENERGY (FRACTION OF E)
C XMZ = Z0 MASS IN GEV
C XGZ = Z0 WIDTH IN GEV
C GV = VECTOR COUPLING BETWEEN EN AND Z0
C GA = AXIAL VECTOR COUPLING BETWEEN E AND Z0
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / PARAM3 / Z,G2
      COMMON / G3COM / EBEAM,  EPS,
     .                 YZ1,  YZ2,  YF1
      COMMON / UNICOM / IIN,IUT
      COMMON / DBCOM / IFLDB
      EXTERNAL FOT
C
C THE INTEGRATED PHOTON SPECTRUM
      HFUN(X)=H1*DLOG(X)+H2*DLOG((X-Z)**2+G2)/2.
     .    +H3*DATAN((X-Z)/G)+H4*X
C
      CALL OUTCRY('SETUP3')
C
C CHECK PHOTON ENERGIES
      IF(XK1.LE.XK0) THEN
        SIGMA=0.D0
        RETURN
      ENDIF
C
C DEFINE CONSTANTS
      EBEAM=E
      XME=0.511D-03
      PI=3.1415926536D0
      EEL=DSQRT(4.*PI/137.036D0)
      XL=DLOG(4.*E**2/XME**2)
C
C DEFINE SPECTRUM PARAMETERS; TOTAL APPROXIMATE CROSS SECTION
      Z=1.-XMZ**2/4./E**2
      G=XMZ*XGZ/4./E**2
      G2=G**2
      H1=2./(Z**2+G2)
      H2=-H1+3.-2.*Z
      H3=Z/G*H1+(-4.+3.*Z-Z**2+G2)/G
      H4=-1.
      HFAC=HFUN(XK1)-HFUN(XK0)
      SIGMA=4970.392/E**2
     . *EEL**2/4.*(GV**4+6.*GV**2*GA**2+GA**4)*8.D0*PI*PI
     . *XL*HFAC
C
C SET UP THE PHOTON SPECTRUM WITH ITERATED INTERVAL STRETCHING
      CALL IIS(1000,10,XK0,XK1,FOT)
C
C INITIALZIE CONSTANTS FOR THE EVENT GENERATION
      EPS=XME**2/2./E**2
      YZ1=DLOG(EPS)
      YZ2=DLOG((2.+EPS)/EPS)
      YF1=2.*PI
C
C PRINT THE RESULTS SO FAR
*                          Under control of the flag IFLDB
      IF(IFLDB.EQ.0) GO TO 777
      WRITE(IUT,1) XME,    PI,   EEL,    XL,
     .             Z,     G,    G2,    H1,
     .            H2,    H3,    H4,  HFAC,
     .         SIGMA,   EPS,   YZ1,   YZ2,
     .           YF1
    1 FORMAT(' SETUP3 :',4D14.6)
  777 CONTINUE
      RETURN
      END
*DECK,GENER3
      SUBROUTINE GENER3(QP,QM,QK,WM)
C
C GENERATION OF THE FINAL-STATE MOMENTA
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / G3COM / E,  EPS,
     .                 YZ1,  YZ2,  YF1
      DIMENSION QP(4),QM(4),QK(4),H(4)
C
C GENERATE THE PHOTON ENERGY BY USING THE SET-UP HISTOGRAM
      CALL PAK(XK)
      QK(4)=E*XK
C
C GENERATE THE PHOTON POLAR ANGLE (N.B. STABILITY)
      V=DEXP(YZ1+RN(3.)*YZ2)-EPS
      ZG=1.-V
      SG=DSQRT(V*(2.-V))
      IF(RN(4.).GT..5D0) ZG=-ZG
C
C GENERATE THE OTHER TRIVIAL VARIABLES
      FG=YF1*RN(5.)
      FR=YF1*RN(6.)
      CR=-1.+2.*RN(7.)
      SR=DSQRT(1.-CR*CR)
C
C CALCULATE THE MASS EFFECT FACTOR
      WM=1.-EPS/(EPS+V)*2.*E*(E-QK(4))/(E**2+(E-QK(4))**2)
C
C CONSTRUCT THE PHOTON MOMENTUM IN THE LAB
      QK(1)=QK(4)*SG*DSIN(FG)
      QK(2)=QK(4)*SG*DCOS(FG)
      QK(3)=QK(4)*ZG
C
C CONSTRUCT THE E+ MOMENTUM IN THE E+E- REST FRAME
      H(4)=E*DSQRT(1.-QK(4)/E)
      H(1)=H(4)*SR*DSIN(FR)
      H(2)=H(4)*SR*DCOS(FR)
      H(3)=H(4)*CR
C
C BOOST TO OBTAIN THE LAB FRAME VALUE
      QP(4)=((2.*E-QK(4))*H(4)
     .       -H(3)*QK(3)-H(2)*QK(2)-H(1)*QK(1))/(2.*H(4))
      R=(QP(4)+H(4))/(2.*E-QK(4)+2.*H(4))
      DO 4 I=1,3
    4 QP(I)=H(I)-QK(I)*R
C
C CONSTRUCT THE E- MOMENTUM FROM CONSERVATION
      QM(4)=2.*E-QP(4)-QK(4)
      DO 5 I=1,3
    5 QM(I)=-QP(I)-QK(I)
C
      RETURN
      END
*DECK,SETUP4
      SUBROUTINE SETUP4(E,XK0,XK1,THMIN,THMAX,SIGMA)
C
C SUBGENERATOR 4:
C RADIATION IN QED T-CHANNEL GRAPHS
C E=BEAM ENERGY IN GEV
C XK0 = MINIMUM PHOTON ENERGY (FRACTION OF E)
C XK1 = MAXIMUM PHOTON ENERGY (FRACTION OF E)
C THMIN = MINIMUM SCATTERING ANGLE OF E+,E- (DEGREES)
C THMAX = MAXIMUM SCATTERING ANGLE OF E+,E- (DEGREES)
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / G4COM / EBEAM, E0,  XL,  EPS,
     .                 YK1,  YK2,  YK3,  YC1,  YC2,  YC3,
     .                 YF1,  YV1,  TRYK,  PASK,  TRYC,  PASC
      COMMON / EFFIC4 / EFKPR,EFKPO,EFCPR,EFCPO
      COMMON / UNICOM / IIN,IUT
      COMMON / DBCOM / IFLDB
C
C INTEGRATED PHOTON SPECTRUM
      H4(X)=2.*DLOG(X)-2.*X+X**2/2.
C
C INTEGRATED ANGULAR DISTRIBUTION
      C4(X)=(DLOG((1.-X)/2.)+XL+1.)/(1.-X)
C
      CALL OUTCRY('SETUP4')
C
C CHECK PHOTON ENERGIES
      IF(XK1.LE.XK0) THEN
        SIGMA=0.D0
        RETURN
      ENDIF
C
C INITIALIZE CONSTANTS
      EBEAM=E
      XME=0.511D-03
      PI=3.1415926536D0
      EEL=DSQRT(4.*PI/137.036D0)
      XL=DLOG(4.*E**2/XME**2)
C
C TOTAL APPROXIMATE CROSS SECTION
      CMAX=DCOS(THMAX*PI/180.D0)
      CMIN=DCOS(THMIN*PI/180.D0)
      CFAC=C4(CMIN)-C4(CMAX)
      HFAC=H4(XK1)-H4(XK0)
      SIGMA=4970.392/E**2
     . *16.*PI**2*EEL**6*HFAC*CFAC
C
C INITIALIZE CONSTANTS FOR EVENT GENERATION
      E0=E*XK0
      EMAX=E*XK1
      YK1=EMAX/E0
      YK2=E**2
      YK3=E**2+(E-E0)**2
      YC1=1./(1.-CMAX)
      YC2=1./(1.-CMIN)-1./(1.-CMAX)
      YC3=XL+DLOG((1.-CMAX)/2.)
      YF1=2.*PI
      EPS=XME**2/2./E**2
      YV1=1.+XME**2/E**2
C
C THE EFFICIENCY OF THE PHOTON ENERGY LOOP
      EFKPR=HFAC/(1.+(1.-E0/E)**2)/DLOG(EMAX/E0)
      TRYK=0.
      PASK=0.
C
C THE EFFICIENCY OF THE SCATTERING ANGLE LOOP
      EFCPR=CFAC/(YC3*(1./(1.-CMIN)-1./(1.-CMAX)))
      TRYC=0.
      PASC=0.
C
C PRINT THE RESULTS SO FAR
*                          Under control of the flag IFLDB
      IF(IFLDB.EQ.0) GO TO 777
      WRITE(IUT,1) XME,    PI,   EEL,    XL,
     .          CMAX,  CMIN,  HFAC,  CFAC,
     .         SIGMA,    E0,  EMAX,   YK1,
     .           YK2,   YK3,   YC1,   YC2,
     .           YC3,   YF1,   EPS,   YV1,
     .          EFKPR, TRYK,  PASK, EFCPR,
     .          TRYC,  PASC
    1 FORMAT(' SETUP4 :',4D14.6)
  777 CONTINUE
      RETURN
      END
*DECK,GENER4
      SUBROUTINE GENER4(QP,QM,QK,WM)
C
C GENERATION OF THE FINAL-STATE MOMENTA
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION QP(4),QM(4),QK(4)
      COMMON / EFFIC4 / EFKPR,EFKPO,EFCPR,EFCPO
      COMMON / G4COM / E, E0,  XL,  EPS,
     .                 YK1,  YK2,  YK3,  YC1,  YC2,  YC3,
     .                 YF1,  YV1,  TRYK,  PASK,  TRYC,  PASC
C
C GENERATE THE PHOTON SPECTRUM BY MAPPING AND W.R.P.
    2 CONTINUE
      TRYK=TRYK+1.
      QK(4)=E0*YK1**RN(1.)
      W=(YK2+(E-QK(4))**2)/YK3
      IF(W.LT.RN(2.)) GOTO 2
      PASK=PASK+1.
      EFKPO=PASK/TRYK
C
C GENERATE THE SCATTERING ANGLE BY MAPPING AND W.R.P.
    3 CONTINUE
      TRYC=TRYC+1.
      C=1.-1./(YC1+RN(3.)*YC2)
      W=(DLOG((1.-C)/2.)+XL)/YC3
      IF(W.LT.RN(4.)) GOTO 3
      PASC=PASC+1.
      EFCPO=PASC/TRYC
      SC=DSQRT(1.-C*C)
C
C GENERATE THE U VALUE FOR THE FEYNMAN TRICK
      U0=EPS/(1.-C)
      U1=1.+U0
      VU=(U1/U0)**RN(5.)*U0-U0
      U=VU
      IF(RN(6.).GT.0.5D0) U=1.-VU
C
C GENERATE THE ANGLE OF THE PHOTON WRT THE VECTOR E(U)
      EU2=2.*(1.-C)*(VU+U0)*(U1-VU)
      EUV=DSQRT(YV1-EU2)
      R=RN(7.)
      V1=2.*EU2*(1.-R)/(EU2+2.*R*EUV*(1.+EPS+EUV))
      C1=1.-V1
      SC1=DSQRT(V1*(2.-V1))
      F1=YF1*RN(8.)
      CF1=DCOS(F1)
      SF1=DSIN(F1)
C
C CALCULATE THE MASS EFFECT FACTOR
      R=VU*(SC*SC1*CF1+(1.-C)*C1)+V1
      R=(R+(2.*EPS-EU2)/(EUV+1.))/EUV
      WM=1.-2.*E*(E-QK(4))/(E**2+(E-QK(4))**2)*EPS/(EPS+R)
C
C ROTATE THE PHOTON MOMENTUM TO ACCOUNT FOR THE E(U) DIRECTION
      UU=U-1.-U*C
      XK1X=(SC1*CF1*UU-U*C1*SC)/EUV
      XK1Y=SC1*SF1
      XK1Z=(U*SC*SC1*CF1+UU*C1)/EUV
      XKX=XK1X**2+XK1Y**2+XK1Z**2-1.D0
C
C CONSTRUCT THE E+ MOMENTUM
      CG=SC*XK1X+C*XK1Z
      QP(4)=2.*E*(E-QK(4))/(2.*E-QK(4)*(1.-CG))
      F=YF1*RN(9.)
      CF=DCOS(F)
      SF=DSIN(F)
      QP(1)=QP(4)*SC*SF
      QP(2)=QP(4)*SC*CF
      QP(3)=QP(4)*C
C
C CONSTRUCT THE PHOTON MOMENTUM
      QK(1)=QK(4)*(XK1X*SF-XK1Y*CF)
      QK(2)=QK(4)*(XK1X*CF+XK1Y*SF)
      QK(3)=QK(4)*(XK1Z           )
C
C CONSTRUCT THE E- MOMENTUM
      QM(4)=2.*E-QP(4)-QK(4)
      DO 4 I=1,3
    4 QM(I)=-QP(I)-QK(I)
C
C SYMMETRIZE BETWEEN "E+" AND "E-" IN 50% OF THE CASES
      IF(RN(10.).LT.0.5D0) RETURN
      DO 5 I=1,3
        QK(I)=-QK(I)
        R=QP(I)
        QP(I)=-QM(I)
    5 QM(I)=-R
      R=QP(4)
      QP(4)=QM(4)
      QM(4)=R
C
      RETURN
      END
*DECK,SETUP5
      SUBROUTINE SETUP5(E,XMZ,XGZ,THMIN,THMAX,SIGMA)
C
C SUBGENERATOR 5:
C SOFT BREMSSTRAHLUNG AND VIRTUAL CORRECTIONS
C E = BEAM ENERGY IN GEV
C THMIN = MINIMUM E+E- POLAR SCATTERING ANGLE
C THMAX = MAXIMUM E+E- POLAR SCATTERING ANGLE
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / G5COM / EBEAM,XMIN,XRAN,HM,TRY,PAS,H(1000),AZ
      COMMON / EFFIC5 / EFPRIO,EFPOST
      COMMON / UNICOM / IIN,IUT
      COMMON / DBCOM / IFLDB
      DATA PI/3.1415926536D0/
C
      CALL OUTCRY('SETUP5')
C
      EBEAM=E
C
C PARAMETERS OF THE NUMERICAL EXAMINATION STEP
      S=4.D0*E**2
      AEMP=1.D-02
      AZ=AEMP*S**2/((S-XMZ**2)**2+XMZ**2*XGZ**2)
      CMIN=DCOS(PI*THMAX/180.D0)
      CMAX=DCOS(PI*THMIN/180.D0)
      XMIN=AZ*CMIN+1.D0/(1.D0-CMIN)
      XMAX=AZ*CMAX+1.D0/(1.D0-CMAX)
      NP=1000
      NP1=NP-1
      XP1=1.D0*NP1
      XRAN=XMAX-XMIN
C
C PREPARE THE SPLINE INTERPOLATIONS
      THJUMP=20.D0
      IF(THMIN.GE.THJUMP.OR.THMAX.LE.THJUMP)
     &                            THJUMP=(THMIN+THMAX)/2.D0
      CALL STUDYF(THMIN,THMAX,THJUMP,40,20)
C
C COMPUTE THE SOFT CROSS SECTION USING THE TRAPEZOIDAL RULE
      HM=0.D0
      DO 1 J=1,NP
*
*.... Debug information if wanted. Under control of the flag IFLDB
        IF(IFLDB.EQ.0) GO TO 776
        CALL TELLER(J,100,'SOFT INT. ')
  776   CONTINUE
        H(J)=SOFT(  XMIN+XRAN*(1.D0*J-1.D0)/(1.D0*NP-1.D0)  )
    1 HM=DMAX1(HM,H(J))
      HM=HM*1.01D0
      WRITE(IUT,9)
    9 FORMAT(' SETUP5: END OF THE SOFT CROSS SECTION PART')
C
C COMPUTE THE TOTAL SOFT CROSS SECTION
      SIGMA=0.
      DO 2 J=1,NP
    2 SIGMA=SIGMA+H(J)
      SIGMA=SIGMA-(H(1)+H(NP))/2.
      SIGMA=SIGMA*XRAN/(1.D0*NP-1.D0)
      TRY=0.
      PAS=0.
C
C ESTIMATED EFFICIENCY OF THE ANGLE GENERATION LOOP
      EFPRIO=SIGMA/(HM*XRAN)
C
C PRINT THE RESULTS SO FAR
*                          Under control of the flag IFLDB
      IF(IFLDB.EQ.0) GO TO 777
      WRITE(IUT,3)   E,     THMIN, THMAX,  XMIN,
     &               XMAX,  XRAN,  HM,     SIGMA,
     &               TRY,   PAS,   EFPRIO, AZ
    3 FORMAT(' SETUP5 :',4D14.6)
  777 CONTINUE
      RETURN
      END
*DECK,GENER5
      SUBROUTINE GENER5(QP,QM,QK)
C
C GENERATION OF THE FINAL-STATE MOMENTA
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / EFFIC5 / EFPRIO,EFPOST
      COMMON / G5COM / E,XMIN,XRAN,HM,TRY,PAS,H(1000),AZ
      DIMENSION QP(4),QM(4),QK(4)
C
C GENERATE THE SCATTERING ANGLE BY W.R.P. AND MAPPING
    4 TRY=TRY+1.
      X=XMIN+XRAN*RN(1.)
      HX=SOFT(X)
      WX=HX/HM
      IF(WX.LT.RN(2.)) GOTO 4
      C=2.D0*(X-1.D0)/(AZ+X+DSQRT((AZ-X)**2+4.D0*AZ))
      PAS=PAS+1.
      EFPOST=PAS/TRY
C
C GENERATE THE AZIMUTHAL ANGLE
      FI=2.*3.1415926536D0*RN(3.)
      S=DSQRT(1.-C**2)
C
C CONSTRUCT THE E+ MOMENTUM
      QP(4)=E
      QP(3)=E*C
      QP(2)=E*S*DCOS(FI)
      QP(1)=E*S*DSIN(FI)
C
C CONSTRUCT THE E- MOMENTUM
      DO 5 J=1,3
    5 QM(J)=-QP(J)
      QM(4)=E
C
C THE PHOTON MOMENTUM IS SET TO ZERO IN THE SOFT CASE
      DO 6 J=1,4
    6 QK(J)=0.
      RETURN
      END
*DECK,DILOG
C     FUNCTION DILOG(X)
C     IMPLICIT REAL*8(A-H,O-Z)
C     Z=-1.644934066848226D0
C     IF(X .LT.-1.D0) GO TO 1
C     IF(X .LE. 0.5D0) GO TO 2
C     IF(X .EQ. 1.D0) GO TO 3
C     IF(X .LE. 2.D0) GO TO 4
C
C     Z=3.289868133696453D0
C   1 T=1.D0/X
C     S=-0.5D0
C     Z=Z-0.5D0*DLOG(DABS(X))**2
C     GO TO 5
C
C   2 T=X
C     S=0.5D0
C     Z=0.D0
C     GO TO 5
C
C   3 DILOG=1.644934066848226D0
C     RETURN
C
C   4 T=1.D0-X
C     S=-0.5D0
C     Z=1.644934066848226D0-DLOG(X)*DLOG(DABS(T))
C
C   5 Y=2.666666666666667D0*T+0.666666666666667D0
C     B=      0.000000000000001D0
C     A=Y*B  +0.000000000000004D0
C     B=Y*A-B+0.000000000000011D0
C     A=Y*B-A+0.000000000000037D0
C     B=Y*A-B+0.000000000000121D0
C     A=Y*B-A+0.000000000000398D0
C     B=Y*A-B+0.000000000001312D0
C     A=Y*B-A+0.000000000004342D0
C     B=Y*A-B+0.000000000014437D0
C     A=Y*B-A+0.000000000048274D0
C     B=Y*A-B+0.000000000162421D0
C     A=Y*B-A+0.000000000550291D0
C     B=Y*A-B+0.000000001879117D0
C     A=Y*B-A+0.000000006474338D0
C     B=Y*A-B+0.000000022536705D0
C     A=Y*B-A+0.000000079387055D0
C     B=Y*A-B+0.000000283575385D0
C     A=Y*B-A+0.000001029904264D0
C     B=Y*A-B+0.000003816329463D0
C     A=Y*B-A+0.000014496300557D0
C     B=Y*A-B+0.000056817822718D0
C     A=Y*B-A+0.000232002196094D0
C     B=Y*A-B+0.001001627496164D0
C     A=Y*B-A+0.004686361959447D0
C     B=Y*A-B+0.024879322924228D0
C     A=Y*B-A+0.166073032927855D0
C     A=Y*A-B+1.935064300869969D0
C     DILOG=S*T*(A-B)+Z
C     RETURN
C
C     END
*DECK,IIS
      SUBROUTINE IIS(N,ITER,X1,XN,FUNC)
C
C ITERATED INTERVAL STRETCHING ROUTINE.
C IT TRIES TO PUT THE FUNCTION F ON THE INTERVAL :X0,X1|
C INTO A HISTOGRAM WITH N BINS. USINF ITER ITERATIONS
C IT TRIES TO MAKE ALL BINS HAVE THE SAME AREA.
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION F(1000),A(1000),Y(1000),Z(1000),XNEW(1000)
      COMMON / IISCOM / X(1000),M
      COMMON / UNICOM / IIN,IUT
      COMMON / DBCOM / IFLDB
      EXTERNAL FUNC
C
      CALL OUTCRY('I.I.S.')
C
C PRINT OUT THE PARAMETERS
      WRITE(IUT,1) X1,XN,N,ITER
    1 FORMAT(' I.I.S. : PARAMETERS OF INTERVAL STRETCHING:',/,
     &       ' I.I.S. : LOWEST  VARIABLE VALUE   : ',D10.3,/,
     &       ' I.I.S. : HIGHEST VARIABLE VALUE   : ',D10.3,/,
     &       ' I.I.S. : NO. OF POINTS            : ',I5,/,
     &       ' I.I.S. : NO. OF ITERATIONS        : ',I3)
C
C INITIALIZE BY CHOOSING EQUIDISTANT X VALUES
      IT=0
      M=N-1
      DX=(XN-X1)/(1.D0*M)
      X(1)=X1
      DO 101 I=2,N
  101 X(I)=X(I-1)+DX
C
C STARTING POINT FOR ITERATIONS
  100 CONTINUE
C
C CALCULATE FUNCTION VALUES
      DO 102 I=1,N
  102 F(I)=FUNC(X(I))
C
C CALCULATE BIN AREAS
      DO 103 I=1,M
  103 A(I)=(X(I+1)-X(I))*(F(I+1)+F(I))/2.
C
C CALCULATE CUMULATIVE SPECTRUM Y VALUES
      Y(1)=0.D0
      DO 104 I=2,N
  104 Y(I)=Y(I-1)+A(I-1)
C
C PUT EQUIDISTANT POINTS ON Y SCALE
      DZ=Y(N)/(1.D0*M)
      Z(1)=0.D0
      DO 105 I=2,N
  105 Z(I)=Z(I-1)+DZ
C
C DETERMINE SPACING OF Z POINTS IN BETWEEN Y POINTS
C FROM THIS, DETERMINE NEW X VALUES AND FINALLY REPLACE OLD VALUES
      XNEW(1)=X(1)
      XNEW(N)=X(N)
      K=1
      DO 108 I=2,M
  106   IF( Y(K+1) .GT. Z(I) ) GOTO 107
        K=K+1
        GOTO 106
  107   R= ( Z(I) - Y(K) ) / ( Y(K+1) - Y(K) )
  108 XNEW(I) = X(K) + ( X(K+1)-X(K) )*R
      DO 109 I=1,N
  109 X(I)=XNEW(I)
C
C CHECK ON END OF ITERATIONS AND RETURN
      IT=IT+1
*
*.... Debug information if wanted. Under control of the flag IFLDB
      IF(IFLDB.EQ.0) GO TO 776
      CALL TELLER(IT,1,'I.I.S.LOOP')
      WRITE(IUT,3) IT,Y(M)
    3 FORMAT(' I.I.S. : ITERATION ',I3,'  RESULT =',D15.6)
  776 CONTINUE
      IF(IT.LT.ITER) GOTO 100
      RETURN
      END
*DECK,PAK
      SUBROUTINE PAK(VALUE)
C
C GENERATOR OF NUMBER 'VALUE' DISTRIBUTED ACCORDING TO THE
C HISTOGRAM. IT IS OBTAINED ASSUMING THAT ALL BINS HAVE THE
C SAME AREA.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / IISCOM / X(1000),M
C
C GENERATE VALUE FROM CUMULATIVE SPECTRUM BINS
      R=M*RN(1.D0)
      I=IDINT(R)
      S=R-I
      VALUE = X(I+1) + S*( X(I+2)-X(I+1) )
C
      RETURN
      END
*DECK,FOT
      FUNCTION FOT(X)
C
C THE PHOTON SPECTRUM
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / PARAM3 / Z,G2
      FOT=(1.-X)*(1.+(1.-X)**2)/X/((X-Z)**2+G2)
      RETURN
      END
*DECK,STUDYF
      SUBROUTINE STUDYF(THMIN,THMAX,THJUMP,NOP1,NOP2)
C SETS UP TWO UNIFORM GRIDS OF C VALUES AND OBTAINS
C THE VALUES OF THE FUNCTIONS D(SIGMA)/D(OMEGA) WITH THE
C CONTRIBUTION OF THE RUTHERFORD POLE DIVIDED OUT
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / NTPLCM / C(2,100),F(2,100),DEL(2),DEN(2),N(2)
      COMMON / DBCOM / IFLDB
*
      CALL OUTCRY('STUDYF')
      PI=4.D0*DATAN(1.D0)
      CMIN=DCOS(PI*THMAX/180.D0)
      CMAX=DCOS(PI*THMIN/180.D0)
      CJUMP=DCOS(PI*THJUMP/180.D0)
      N(1)=NOP1-1
      N(2)=NOP2-1
      DEL(1)=(CJUMP-CMIN)/(1.D0*N(1))
      DEL(2)=(CMAX-CJUMP)/(1.D0*N(2))
      DEN(1)=1.D0/(6.D0*DEL(1)**3)
      DEN(2)=1.D0/(6.D0*DEL(2)**3)
      DO 1 K=1,NOP1
*
*.... Debug information if wanted. Under control of the flag IFLDB
        IF(IFLDB.EQ.0) GO TO 775
        CALL TELLER(K,1,'SPLINELOOP')
  775   CONTINUE
        C(1,K)=CMIN+(K-1)*DEL(1)
    1 F(1,K)=SIGSOF(C(1,K))*(1.D0-C(1,K))**2
      DO 2 K=1,NOP2
*
*.... Debug information if wanted. Under control of the flag IFLDB
        IF(IFLDB.EQ.0) GO TO 776
        CALL TELLER(K,1,'SPLINELOOP')
  776   CONTINUE
        C(2,K)=CJUMP+(K-1)*DEL(2)
    2 F(2,K)=SIGSOF(C(2,K))*(1.D0-C(2,K))**2
      RETURN
      END
*DECK,OBTANF
      FUNCTION OBTANF(X)
C THE INTERPOLATION FORMULA FOR THE FUNCTION D(SIGMA)/D(OMEGA)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / NTPLCM / C(2,100),F(2,100),DEL(2),DEN(2),N(2)
      J=1
      IF(X.GE.C(2,1)) J=2
      K=IDINT((X-C(J,1))/DEL(J))+1
      IF(K.EQ.1) K=2
      IF(K.EQ.N(J)) K=N(J)-1
      X1=X-C(J,K-1)
      X2=X-C(J,K)
      X3=X-C(J,K+1)
      X4=X-C(J,K+2)
      OBTANF=(X1*X2*X3*F(J,K+2)-
     .        3.D0*X1*X2*X4*F(J,K+1)+
     .        3.D0*X1*X3*X4*F(J,K)-
     .        X2*X3*X4*F(J,K-1))*DEN(J)/(1.D0-X)**2
      RETURN
      END
*DECK,SOFT
      FUNCTION SOFT(U)
C PREPARES THE VALUES D(SIGMA)/D(OMEGA) GIVEN BY
C THE FUNCTION 'SIGSOF' FOR USE IN ROUTINE 'SETUP5'
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / G5COM / EBEAM,XMIN,XRAN,HM,TRY,PAS,H(1000),AZ
      COMMON / UNICOM / IIN,IUT
      DATA PI/3.1415926536D0/
      C=2.D0*(U-1.D0)/(AZ+U+DSQRT((AZ-U)**2+4.D0*AZ))
      F=OBTANF(C)
      SOFT=2.*PI*F/(AZ+1.D0/(1.D0-C)**2)
      RETURN
      END
*DECK,SETBAB
      SUBROUTINE SETBAB(E,XMZ,XMH,XMT,THMIN,THMAX,XKMAX)
C***************************************************************
C
C     MONTE CARLO EVENT GENERATOR FOR THE PROCESSES
C
C         E+(PP) E-(PM)   ---->  E+(QP) E-(QM)
C
C     AND
C
C         E+(PP) E-(PM)   ----> E+(QP) E-(QM) PHOTON(QK)
C
C
C  AUTHORS: R. KLEISS, CERN
C           F.A. BERENDS, LEIDEN UNIVERSITY, LEIDEN, HOLLAND
C           W. HOLLIK, HAMBURG UNIVERSITY, HAMBURG, GERMANY
C
C
C  THE INPUT QUANTITIES ARE
C  E     = BEAM ENERGY, IN GEV;
C  XMZ   = MASS OF THE Z0 BOSON, IN GEV;
C  XMH   = MASS OF THE HIGGS BOSON, IN GEV;
C  XMT   = MASS OF THE TOP QUARK, IN GEV;
C  THMIN = MINIMUM POLAR SCATTERING ANGLE OF THE E+,E-, IN
C          DEGREES: IT MUST BE LARGER THAN 0 DEGREES BUT
C          SMALLER THAN THMAX;
C  THMAX = MAXIMUM POLAR SCATTERING ANGLE OF THE E+,E-, IN
C          DEGREES: IT MUST BE SMALLER THAN 180 DEGREES BUT
C          LARGER THAN THMIN;
C  XKMAX = MAXIMUM ENERGY OF THE BREMSSTRAHLUNG PHOTON, AS A
C          FRACTION OF THE BEAM ENERGY E: IT MAY BE SET
C          TO 1, IN WHICH CASE THE PROGRAM CHANGES ITS VALUE
C          TO THE ACTUAL KINEMATIC LIMIT.
C
C
C  IN THE PRESENT PROGRAM THE W MASS AND THE ELECTROWEAK MIXING ANGLE
C  ARE CALCULATED FROM THE (AS YET) MORE ACCURATELY KNOWN VALUE
C  OF THE MUON LIFETIME. THIS IS DONE BY ROUTINE FINDMW
C
C***********************************************************************
*     27.04.88 DAM
*     Routine changed slightly to move all setting of input parameters
*     to main program
************************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SIGMA(5),DEEL(5)
      COMMON / SB1COM / EBEAM,  CMIN,  CMAX
      COMMON / SB2COM / PRIORI(5),CUMUL(5)
      COMMON / SB3COM / WMEAN(5),WERRO(5),WMAXI(5),WEFFI(5)
      COMMON / SB4COM / WALL(3),WCON(3,5)
      COMMON / SB5COM / SIGAPP,  WMAXT,  NEV
      COMMON / WMXCOM / WMAX
      COMMON / REJCOM / IREJEC
      COMMON / UNICOM / IIN,IUT
      COMMON / FINCOM / XMW,S2W
      COMMON / WDISCO / WEIDIS(6,21)
*............................................................... DAM
      COMMON / XK0COM / XK0
      COMMON /NORM/ CRBRN0,XBHKVS
      COMMON /OUTCRB/ICRBN
      COMMON / DISTR /TRMAX
C
*............................................................... DAM
C
      WRITE(IUT,901)
  901 FORMAT(' ',50('-')/,' WELCOME TO BHAHBA SCATTERING!'/,' ',50('-'))
C
C SIGNAL FOR START
      CALL OUTCRY('SETBAB')
C
C REPRODUCE THE INPUT PARAMETERS
      WRITE(IUT,1) E,XMZ,XMH,XMT,THMIN,THMAX,XKMAX
    1 FORMAT(
     .' YOU HAVE SUPPLIED THE FOLLOWING PARAMETERS :'/,
     .' BEAM ENERGY                     =',F10.4,' GEV'/,
     .' MASS OF THE Z0 BOSON            =',F10.4,' GEV'/,
     .' MASS OF THE HIGGS BOSON         =',F10.4,' GEV'/,
     .' MASS OF THE TOP QUARK           =',F10.4,' GEV'/,
     .' MINIMUM E+,E- SCATTERING ANGLE  =',F10.4,' DEGREES'/,
     .' MAXIMUM E+,E- SCATTERING ANGLE  =',F10.4,' DEGREES'/,
     .' MAXIMUM PHOTON ENERGY           =',F10.4,
     .' OF THE BEAM ENERGY'/,
     .' I AM NOW READY TO START THE INITIALIZATION.'/,' ',50(1H-))
C
C THE W MASS AND THE MIXING ANGLE
      CALL FINDMW(XMZ,XMT,XMH)
C
C THE LIMITS ON THE COSINES OF THE E+,- SCATTERING ANGLES
      EBEAM=E
      CMIN=DCOS(THMAX*3.1415926536D0/180.D0)
      CMAX=DCOS(THMIN*3.1415926536D0/180.D0)
C
      AMEL=.511D-3
      TRMAX=2D0*E**2*(1D0-DSQRT(1D0-( AMEL  /E)**2) *CMIN)-2D0*AMEL**2
C THE COUPLINGS IN THE STANDARD MODEL
      GA= - DSQRT(3.1415926536D0/137.036D0/4.D0/S2W/(1.-S2W))
      GV=GA*(1.-4.*S2W)
      SINK=S2W
*
*..... This has been moved to main program .................... DAM
C
C THE BOUNDARY BETWEEN SOFT AND HARD  BREMSSTRAHLUNG
*     XK0=.01D0
*.............................................................. DAM
C
C INITIALIZE THE SUBPROGRAMS
      CAL= SETUPS(E,XMZ,XGZ,SINK,XMH,XMT,XK0)
      CALL SETUP1(E,XK0,XKMAX,SIGMA(1))
      CALL SETUP2(E,XK0,XKMAX,XMZ,XGZ,GV,GA,SIGMA(2))
      CALL SETUP3(E,XK0,XKMAX,XMZ,XGZ,GV,GA,SIGMA(3))
      WRITE(6,*)' ',XK0
      CALL SETUP4(E,XK0,XKMAX,THMIN,THMAX,SIGMA(4))
      WRITE(6,*)' ',XK0
      CALL SETUP5(E,XMZ,XGZ,THMIN,THMAX,SIGMA(5))
      CALL SETUPW(E,XMZ,XGZ,GV,GA)
      CALL SETUPM(XMZ,XGZ,GV,GA)
C
C THE BORN LEVEL CROSS SECTION
      AMER=AMEL/EBEAM
      BETEL=DSQRT(1D0-AMER**2)
      BETELP=DSQRT(1D0-AMER**2/XBHKVS**2)
      TRMIN=2D0*EBEAM**2*XBHKVS*(1D0-BETEL*BETELP*CMAX       )-2D0*
     $                                                   AMEL**2
      THMNE=DACOS(1D0-2D0*TRMIN/(4D0*EBEAM**2))
      THMNE=(THMNE/DACOS(-1D0))*180D0
      CALL BORN(E,XMZ,XGZ,GV,GA,THMIN,THMAX,CRBORN)
      ICRBN=1
      CALL BORN(E,XMZ,XGZ,GV,GA,THMNE,THMAX,CRBRN0)
C
C THE TOTAL APPROXIMATE CROSS SECTION
      PRIORI(1)=1.
      PRIORI(2)=1.
      PRIORI(3)=1.
      PRIORI(4)=1.
      PRIORI(5)=1.
      SIGAPP=0.
      DO 2 I=1,5
    2 SIGAPP=SIGAPP+SIGMA(I)*PRIORI(I)
      DO 3 I=1,5
    3 DEEL(I)=SIGMA(I)*PRIORI(I)/SIGAPP
      CUMUL(1)=DEEL(1)
      DO 4 I=2,5
    4 CUMUL(I)=CUMUL(I-1)+DEEL(I)
      WRITE(IUT,5) (I,SIGMA(I),PRIORI(I),DEEL(I),CUMUL(I),I=1,5)
    5 FORMAT(' ',50(1H-)/,
     .' I HAVE NOW FINISHED THE INITIALIZATION STEP.'/,
     . ' THE CONTRIBUTIONS ARE THE FOLLOWING:'/,
     . ' 1: INITIAL-STATE RADIATION IN THE PHOTON S-CHANNEL,'/,
     . ' 2: FINAL-STATE RADIATION IN THE Z0 S-CHANNEL,'/,
     . ' 3: INITIAL-STATE RADIATION IN THE Z0 S-CHANNEL,'/,
     . ' 4: BREMSSTRAHLUNG IN THE PHOTON T-CHANNEL,'/,
     . ' 5: THE CONTRIBUTIONS FROM LOOPS AND SOFT PHOTONS.'/,
     . ' MY RESULTS SO FAR ARE:'/,
     . ' CONTR.NO.   APPR.XSEC.   WEIGHT    FRACTION    CUMUL.FR.'/,
     .  5(I7,D15.6,G13.3,G10.3,G12.3/ ),
     . ' WHERE:'/,
     . ' CONTR.NO. = THE NUMBER OF A PARTICULAR CONTRIBUTION,'/,
     . ' APPR.XSEC.= IS THE INTEGRAL OF THAT CONTRIBUTION,'/,
     . ' WEIGHT    = ITS ASSIGNED A-PRIORI WEIGHT,'/,
     . ' FRACTION  = ITS FRACTION OF THE TOTAL INTEGRAL, AND'/,
     . ' CUMUL.FR. = IS THE CUMULATIVE FRACTION.'/,' ',50(1H-))
C
C INITIALIZE THE BOOKKEEPING QUANTITIES
      NEV=0
      WMAXT=0.
      DO 7 I=1,3
        WALL(I)=0.
        DO 6 J=1,5
          WMEAN(J)=0.
          WERRO(J)=0.
          WMAXI(J)=0.
          WEFFI(J)=0.
          WCON(I,J)=0.
    6   CONTINUE
    7 CONTINUE
      DO 8 K=1,6
        DO 8 L=1,21
    8 WEIDIS(K,L)=0.D0
*
*..... This has been moved to main program .................... DAM
C
C DEFINE THE W.R.P. PROCEDURE DEFAULTS
*     IREJEC=0
*     WMAX=0.01D0
*.............................................................. DAM
C
C END OF THE INITIALIZATION PROCEDURE
      WRITE(IUT,9)
    9 FORMAT(' READY TO START GENERATION   - - - -')
      RETURN
      END
*DECK,GENBAB
C
      SUBROUTINE GENBAB(PP,PM,QP,QM,QK,WEV)
C
C GENERATION OF THE PARTICLE MOMENTA.
C WEV = THE WEIGHT OF THE EVENT.
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PP(4),PM(4),QP(4),QM(4),QK(4)
      COMMON / WMXCOM / WMAX
      COMMON / REJCOM / IREJEC
      COMMON / SB1COM / E,  CMIN,  CMAX
      COMMON / SB2COM / PRIORI(5),CUMUL(5)
      COMMON / SB3COM / WMEAN(5),WERRO(5),WMAXI(5),WEFFI(5)
      COMMON / SB4COM / WALL(3),WCON(3,5)
      COMMON / SB5COM / SIGAPP,  WMAXT,  NEV
      COMMON / UNICOM / IIN,IUT
      COMMON / WDISCO / WEIDIS(6,21)
C
C DEFINE THE BEAM MOMENTA
      PP(4)=E
      PP(3)=E
      PP(2)=0.
      PP(1)=0.
      PM(4)=E
      PM(3)=-E
      PM(2)=0.
      PM(1)=0.
C
C DECIDE ON THE CONTRIBUTION THAT WILL SUPPLY THIS EVENT
  101 CONTINUE
      CHOOSE=RN(1.)
      ICON=1
      IF(CHOOSE.GT.CUMUL(1)) ICON=2
      IF(CHOOSE.GT.CUMUL(2)) ICON=3
      IF(CHOOSE.GT.CUMUL(3)) ICON=4
      IF(CHOOSE.GT.CUMUL(4)) ICON=5
C
C GENERATE THE TRIAL EVENT
      IF(ICON.EQ.1) CALL GENER1(QP,QM,QK,WMASS)
      IF(ICON.EQ.2) CALL GENER2(QP,QM,QK,WMASS)
      IF(ICON.EQ.3) CALL GENER3(QP,QM,QK,WMASS)
      IF(ICON.EQ.4) CALL GENER4(QP,QM,QK,WMASS)
      IF(ICON.EQ.5) CALL GENER5(QP,QM,QK)
C
C DEFINE THE EVENT WEIGHT. THERE ARE THREE POSSIBILITIES:
C A) EVENT OUTSIDE ACCEPTANCE FOR THE E+,- ANGLES: WEIGHT=0
C B) SOFT EVENT, I.E. FROM GENER5: WEIGHT=1
C C) HARD EVENT, I.E. FROM GENER1,2,3,4: NOW THE WEIGHT IS
C    THE RATIO OF THE EXACT MATRIX ELEMENT SQUARED OVER THE
C    SUM OF THE APPROXIMATE CONTRIBUTIONS (TAKING THEIR
C    A-PRIORI WEIGHTS INTO ACCOUNT)
C
C CHECK ON ACCEPTABILITY
      CP=QP(3)/QP(4)
      CM=-QM(3)/QM(4)
      IF(     CP.GT.CMIN. AND. CP.LT.CMAX
     .  .AND. CM.GT.CMIN. AND. CM.LT.CMAX ) GOTO 102
C
C CASE A: EVENT OUTSIDE THE DEFINED PHASE SPACE
      W=0.
      GOTO 105
C
C EVENT INSIDE ACCEPTED PHASE SPACE
  102 CONTINUE
      IF(ICON.NE.5) GOTO 103
C
C CASE B: SOFT PHOTON EVENT
      W=1./PRIORI(5)
      GOTO 105
C
C CASE C: HARD PHOTON EVENT
  103 CONTINUE
C
C THE EXACT MATRIX ELEMENT SQUARED
      CALL AMPLIT(PP,PM,QP,QM,QK,EXACT)
C
C THE SUM OF THE APPROXIMATIONS
      CALL APROXS(QP,QM,QK,APPR1,APPR2,APPR3,APPR4)
      APPROX= APPR1*PRIORI(1) + APPR2*PRIORI(2)
     .      + APPR3*PRIORI(3) + APPR4*PRIORI(4)
C
C THE EVENT WEIGHT: N.B. ALSO FINITE-MASS EFFECTS INCLUDED.
      W=EXACT*WMASS/APPROX
C THE EVENTY WEIGHT IS NOW DEFINED IN EVERY CASE
  105 WEV=W
C
C CHECK ON ANOMALOUS WEIGHT
      IF(WEV.GT.10.D0)
     . CALL PRNVEC(QP,QM,QK,APPR1,APPR2,APPR3,APPR4,WMASS,EXACT,ICON)
C
C DO THE BOOKKEEPING TO FIND THE EXACT CROSS SECTION,
C AND SOME HISTOGRAMMING OF THE EVENT WEIGHTS.
      WMAXT=DMAX1(WMAXT,WEV)
      WMAXI(ICON)=DMAX1(WMAXI(ICON),WEV)
      WALL(1)=WALL(1)+1.D0
      WALL(2)=WALL(2)+WEV
      WALL(3)=WALL(3)+WEV**2
      WCON(1,ICON)=WCON(1,ICON)+1.D0
      WCON(2,ICON)=WCON(2,ICON)+WEV
      WCON(3,ICON)=WCON(3,ICON)+WEV**2
C
C ADD TO THE COUNTER OF SUCCESFUL EVENTS
      NEV=NEV+1
C
C IF NO REJECTION IS ASKED FOR, RETURN; ELSE, APPLY REJECTION.
      IF(IREJEC.EQ.0) GOTO 107
      WRP=WEV/WMAX
      IF(WRP.GT.1.D0) WRITE(IUT,106) WEV,WMAX
  106 FORMAT(' GENBAB : WARNING ! AN EVENT OCCURS WITH WEIGHT',
     .       G10.3/,
     .       ' GENBAB :           WHICH IS MORE THAN THE ESTIMATED',
     .       ' MAXIMUM ',G10.3)
      IF(WRP.LT.RN(2.)) GOTO 101
      WEV=1.D0
  107 KWEIDI=IDINT(10.D0*WEV)+1
      IF(KWEIDI.GT.20) KWEIDI=21
      WEIDIS(ICON,KWEIDI)=WEIDIS(ICON,KWEIDI)+1.D0
      WEIDIS( 6  ,KWEIDI)=WEIDIS( 6  ,KWEIDI)+1.D0
      RETURN
      END
*DECK,ENDBAB
      SUBROUTINE ENDBAB(SIGTOT,ERRTOT)
C
C ANALYSIS OF THE GENERATED EVENT WEIGHTS IN ORDER TO FIND THE
C EXACT TOTAL CROSS SECTION.
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL HISTOS,REJECT
      CHARACTER*3 KSTR(22)
      COMMON / FLGCOM / HISTOS,REJECT
      COMMON / SB1COM / E,  CMIN,  CMAX
      COMMON / SB2COM / PRIORI(5),CUMUL(5)
      COMMON / SB3COM / WMEAN(5),WERRO(5),WMAXI(5),WEFFI(5)
      COMMON / SB4COM / WALL(3),WCON(3,5)
      COMMON / SB5COM / SIGAPP,  WMAXT,  NEV
      COMMON / UNICOM / IIN,IUT
      COMMON / WDISCO / WEIDIS(6,21)
      DATA KSTR/'0.0','0.1','0.2','0.3','0.4','0.5','0.6',
     .          '0.7','0.8','0.9','1.0','1.1','1.2','1.3',
     .          '1.4','1.5','1.6','1.7','1.8','1.9','2.0','   '/
C
      CALL OUTCRY('ENDBAB')
C
      WRITE(IUT,201) WALL(1),
     . (KSTR(K),KSTR(K+1),(WEIDIS(L,K),L=1,6),K=1,21)
  201 FORMAT(' ',50('-')/,' NUMBER OF EVENTS GENERATED SO FAR:',F10.1/,
     . ' DISTRIBUTION OF THE EVENT WEIGHTS IN WEIGHT INTERVALS:'/,
     . ' INTERVAL                    SUBGENERATORS'/,
     . ' FROM-TO         1',11X,'2',11X,'3',11X,'4',11X,'5',11X,'ALL'/,
     . (' ',A3,'-',A3,' ',6D12.4))
C
      WRITE(IUT,202)
  202 FORMAT('0STATISTICS OF THE EVENT WEIGHTS :'/,
     .' CONTR.NO   SUM(W**0)  SUM(W**1) SUM(W**2)   MEAN',
     .'    ERROR      MAX.W.    EFFIC.')
      DO 205 I=1,5
        IF(WCON(1,I).EQ.0.D0) THEN
          WRITE(IUT,203) I
  203     FORMAT(I7,
     .       '         0          0         0         --',
     .'     --          --        --')
          GOTO 205
        ENDIF
        WMEAN(I)=WCON(2,I)/WCON(1,I)
        WERRO(I)=DSQRT(WCON(3,I)-WCON(2,I)**2/WCON(1,I))/WCON(1,I)
        IF(WMAXI(I).NE.0.D0) WEFFI(I)=WMEAN(I)/WMAXI(I)
        WRITE(IUT,204) I,(WCON(J,I),J=1,3),WMEAN(I),WERRO(I)
     .  ,WMAXI(I),WEFFI(I)
  204   FORMAT(I7,G14.4,2G11.4,4G10.3)
  205 CONTINUE
      WMEANT=WALL(2)/WALL(1)
      WERROT=DSQRT(WALL(3)-WALL(2)**2/WALL(1))/WALL(1)
      WEFFT=WMEANT/WMAXT
      WRITE(IUT,206) (WALL(K),K=1,3),WMEANT,WERROT,WMAXT,WEFFT
  206 FORMAT(
     .'  TOTAL',G14.4,2G11.4,4G10.3/,
     .' WHERE:'/,
     .' CONTR.NO = THE CONTRIBUTION NUMBER AS BEFORE,'/,
     .' SUM(W**K)= THE K-TH MOMENT OF THE WEIGHT DISTRIBUTION,'/,
     .' MEAN     = THE MEAN WEIGHT FOR THIS CONTRIBUTION,'/,
     .' ERROR    = THE ESTIMATED ERROR ON THIS MEAN,'/,
     .' MAX.W.   = THE MAXIMUM WEIGHT THAT OCCURRED,'/,
     .' EFFIC.   = THE ALGORITHM EFFICIENCY = MEAN/MAX.W.')
C
C THE FINAL RESULT: THE TOTAL CROSS SECTION (IN PICOBARN)
      SIGTOT=WMEANT*SIGAPP
      ERRTOT=WERROT*SIGAPP
      WRITE(IUT,207) SIGTOT,ERRTOT
  207 FORMAT(' ',50(1H-)/,
     . ' THE CROSS SECTION CORRESPONDING TO THE GENERATED'/,
     . ' EVENT SAMPLE IS',D15.6,' +/-',D15.6,
     . ' PICOBARN'/,' ',50(1H-))
C
      RETURN
      END
*DECK,SETUPS
      FUNCTION SETUPS(E,XMZ,XGZ,SW2,XMH,XMT,XK0)
C
C   VIRTUAL AND SOFT PHOTON CORRECTIONS TO
C   BHABHA SCATTERING , QED CORRECTIONS + WEAK CORRECTIONS
C
C     (CORRECTED VERSION, 15 NOV 85)
C
      IMPLICIT REAL*8(A-Z)
      INTEGER I,J,IIN,IUT
      DIMENSION MF(6,2),VF(6,2),M(6,2),VF2(6,2),AF2(6,2)
      COMMON /BOSEW/MZ,MW,MH/LEPT/ME,MMU,MTAU
     .       /HAD/MU,MD,MS,MC,MB,MT
     .       /COUP/SW,CW,V,A,VU,AU,VD,AD
     .       /WIDTH/GZ
     .       /ALF/AL,ALPHA,ALFA
     .       /FERMI/MF,VF,M,VF2,AF2
     .       /CONV/CST
      COMMON /SVLCOM/ SVALUE,DEL
      COMMON / UNICOM / IIN,IUT
C
      CALL OUTCRY('SETUPS')
C
C START OF THE INPUT DATA AND ARGUMENT TRANSFER
C
C THE TOTAL INVARIANT MASS SQUARED
      SVALUE=4.*E*E
C
C MASS OF THE Z0
      MZ=XMZ
C
C SIN**2 OF THE ELECTROWEAK MIXING ANGLE
      SW=SW2
C
C MASS OF THE W BOSON
      CW=1.-SW
      MW=MZ*DSQRT(CW)
C
C MASS OF THE HIGGS BOSON
      MH=XMH
C
C MASSES OF THE FERMIONS: LEPTONS...
      ME=.511D-3
      MMU=.106D0
      MTAU=1.785D0
C
C ...AND QUARKS (THE TOP QUARK MASS IS STILL A FREE PARAMETER)
      MU=.032D0
      MD=.0321D0
      MS=.15D0
      MC=1.5D0
      MB=4.5D0
      MT=XMT
C
C PUT THE MASSES IN ARRAY FORM
      MF(1,1)=0.D0
      MF(2,1)=0.D0
      MF(3,1)=0.D0
      MF(1,2)=ME
      MF(2,2)=MMU
      MF(3,2)=MTAU
      MF(4,1)=MU
      MF(4,2)=MD
      MF(5,1)=MC
      MF(5,2)=MS
      MF(6,1)=MT
      MF(6,2)=MB
C
C THE COUPLING CONSTANTS
      SW1=DSQRT(SW)
      CW1=DSQRT(CW)
      A=-1.D0/(4.D0*SW1*CW1)
      V=(1.D0-4.D0*SW)*A
      VU=(8.D0/3.D0*SW-1.D0)*A
      VD=(1.D0-4.D0/3.D0*SW)*A
      AU=-A
      AD=A
C
C PUT THE COUPLING CONSTANTS IN ARRAY FORM
      VF(1,1)=-A
      VF(2,1)=-A
      VF(3,1)=-A
      VF(1,2)=V
      VF(2,2)=V
      VF(3,2)=V
      VF(4,1)=VU
      VF(5,1)=VU
      VF(6,1)=VU
      VF(4,2)=VD
      VF(5,2)=VD
      VF(6,2)=VD
C
C SQUARES OF COUPLINGS AMD MASSES
      DO 1 I=1,6
        DO 1 J=1,2
          M(I,J)=MF(I,J)**2
          VF2(I,J)=VF(I,J)**2
          AF2(I,J)=A*A
    1 CONTINUE
C
C KINEMATICAL VARIABLES : E IS THE BEAM ENERGY IN GEV
      W=2.D0*E
      S=W*W
      E=W/2.D0
C
C MAXIMUM ENERGY OF THE SOFT PHOTON, AS FRACTION OF THE BEAM ENERGY
      DEL=XK0
C
C NUMERICAL CONSTANTS
      ALFA=1./137.036D0
      PI=3.1415926536D0
      AL=ALFA/PI
      ALPHA=AL/4.
      CST=ALFA*ALFA/4.D0/S*389385.7D03
C
C END OF THE INPUT STEP
C
C CALCULATE THE Z0 WIDTH:
    3 A2=A*A
      MTZ=(MT/MZ)**2
!WP   GZ=IMSZ(MZ**2)/MZ
!WP   XGZ=GZ
!WP: Z-width from BHWIDE
      GZ=XGZ
      WRITE(IUT,1001) XGZ
 1001 FORMAT(' SETUPS : I FOUND A TOTAL Z0 WIDTH OF ',F10.4,' GEV'/,
     . ' ',50('-'))
      SETUPS=0.
      RETURN
      END
*DECK,SIGSOF
      FUNCTION SIGSOF(C)
C GIVES THE CORRECTED FORM FOR D(SIGMA)/D(OMEGA)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /SVLCOM/ S,DEL
      COMMON / UNICOM / IIN,IUT
C
      T=-.5D0*(1.D0-C)*S
      CALL BABCOR(S,T,DEL,DSIG0,DSIG)
      SIGSOF=DSIG
      IF(SIGSOF.LT.0.D0) WRITE(IUT,4) SIGSOF,C
    4 FORMAT(' SIGSOF : VALUE =',D15.6,'    AT C = ',F8.5)
      RETURN
      END
*DECK,BABCOR
      SUBROUTINE BABCOR(S,T,DEL, DSIG0,DSIG)
C
C THE 'WORKING HORSE' THAT CALCULATES THE CORRECTED D(SIGMA)/D(OMEGA)
C
C   S,T ARE THE MANDELSTAM VARIABLES
C   DEL IS MAXIMUM PHOTON ENERGY / BEAM ENERGY
C   DSIG0: DIFFERENTIAL BORN CROSS SECTION (IN PBARN)
C   DSIG : DIFFERENTIAL CROSS SECTION WITH RAD. CORRECTIONS (PBARN)
C
C  MZ, MW, MH ARE THE BOSON MASSES, ME,...MT THE FERMION MASSES IN GEV.
C  SW = SIN**2 THETA-W, CW = COS**2 THETA-W, THETA-W = WEINBERG ANGLE.
C  V,A ARE THE LEPTONIC VECTOR AND AXIALVECTOR COUPLING CONSTANTS;
C  VU,AU THOSE OF I=1/2 QUARKS; VD,AD THOSE OF I=-1/2 QUARKS
C  (ALL NORMALIZED TO E=SQRT(4*PI*ALFA)).
C  GZ IS THE WIDTH OF THE Z IN GEV.
C  MF(I,J) IS THE ARRAY OF FERMION MASSES, WHERE I =1,..6 AND J=1,2;
C  LEPTONS(I=1,2,3) AND QUARKS(I=4,5,6);  J=1: UP,  J=2: DOWN MEMBERS.
C  VF IS THE CORRESPONDING ARRAY OF THE FERMION VECTOR COUPLINGS,
C  VF2, AF2 ARE THE CORRESPONDING ARRAYS OF THE VECTOR AND AXIALVECTOR
C  COUPLINGS SQUARED.
C  M IS THE ARRAY OF THE FERMION MASSES SQUARED.
C  ALFA = 1/137.036, ALPHA = ALFA/4*PI, AL = ALFA/PI
C*****************************************************************
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16   CIR1,PIGS,PIGT,PIG,X1C,X2C,SP1,SP2,SPENCE,CHIS,
     1             TGZS,TGZTS,TZZTS,TZBZS,TZBZTS,TWZS,TWZTS,TGVZS,
     2             TGZ5S,TZZZ5S,TGGZS,TGGZTS,TGZZST,TGZZTS,TGGVT,
     3             TGGVS,TGVZST,TGVZTS,TGVZT,TGZVS,TGZVST,TGZVTS,
     4             TGZVT,TZVZS,TZVZST,TZVZTS,TZVZT,TGGVST,TGGVTS,
     5             FZVS,FZAS,FZVT,FZAT,FGVS,FGAS,FGVT,FGAT,
     6             CFZVS,CFZAS,CFGVS,CFGAS,FS1,FS2,FT1,FT2,
     7             ZS,PROP,PIZS,CHITC,TGZST,TGZT,PIZSW,WPAS
      DIMENSION MF(6,2),VF(6,2),M(6,2),VF2(6,2),AF2(6,2)
      COMMON /BOSEW/MZ,MW,MH/LEPT/ME,MMU,MTAU
     1       /HAD/MU,MD,MS,MC,MB,MT
     2       /COUP/SW,CW,V,A,VU,AU,VD,AD
     3       /WIDTH/GZ
     4       /ALF/AL,ALPHA,ALFA
     5       /FERMI/MF,VF,M,VF2,AF2
     6       /CONV/CST
     7       /BMCB/BSIG0
      DATA ICHK/0/
C
C  LEPTONIC COUPLING CONSTANTS
      SW1=DSQRT(SW)
      CW1=DSQRT(CW)
      A2=A*A
      V2=V*V
      W=V2+A2
      U=2.D0*V*A
C  Z PROPAGATOR
      MZ2=MZ**2
      G2=GZ**2
      SM=S-MZ2
      TM=T-MZ2
      DS=SM**2+MZ2*G2
      DT=TM**2+MZ2*G2
      CHISR=S*SM/DS
      CHITR=T*TM/DT
      CHISI=-MZ*GZ/DS*S
      CHITI=0.D0
      CHITC=DCMPLX(CHITR,CHITI)
      CHIT=T/TM
      CHIS=DCMPLX(CHISR,CHISI)
      CHIS2=S*S/DS
      CHIT2=(T/TM)**2
C   COMBINATION OF ANGLES
      C=1.D0+2.D0*T/S
      C2=1.D0+C*C
      C1=1.D0-C
      C0=1.D0+C
      C3=C0**2/C1
      C4=(C0**2+4.D0)/C1**2
      C5=(C0**2-4.D0)/C1**2
C
      PI=3.1415926536D0
C
C  NOW THE TIJ... ARE SPECIFIED        ********************
C
C  1) BORN TERMS:
      TGZS=(V2*C2+2.D0*A2*C)*CHIS
      TGZST=-W*C3*CHITC
      TGZTS=-W*C3*CHIS
      TGZT=(V2*C4+A2*C5)*2.D0*CHITC
C
      TZZS=(W*W*C2+U*U*2.D0*C)*CHIS2
      TZZTS=-(W*W+U*U)*C3*CHIS*CHITC
      TZZT=(W*W*C4+U*U*C5)*2.D0*CHIT2
C
C  2) TERMS APPEARING IN THE PHOTON-Z-MIXING PART
C
      TGGZS=2.D0*V*C2*CHIS
      TGGZST=-2.D0*V*C3*CHIT
      TGGZTS=-2.D0*V*C3*CHIS
      TGGZT=4.D0*V*C4*CHIT
C
      TGZZS=2.D0*(V*W*C2+A*U*2.D0*C)*CHIS2
      TGZZST=-2.D0*(V*W+A*U)*DCONJG(CHIS)*CHIT*C3
      TGZZTS=-2.D0*(V*W+A*U)*CHIS*CHIT*C3
      TGZZT=4.D0*(V*W*C4+A*U*C5)*CHIT2
C
C  3) TERMS WITH WEAK VERTEX CORRECTIONS
C
      CALL FGAM(S,FGVS,FGAS)
      CALL FGAM(T,FGVT,FGAT)
      CALL FZ(S,FZVS,FZAS)
      CALL FZ(T,FZVT,FZAT)
      CFGVS=DCONJG(FGVS)
      CFGAS=DCONJG(FGAS)
      CFZVS=DCONJG(FZVS)
      CFZAS=DCONJG(FZAS)
C
      TGGVS=2.D0*FGVS*C2
      TGGVST=-2.D0*FGVT*C3
      TGGVTS=-2.D0*FGVS*C3
      TGGVT=4.D0*FGVT*C4
C
      TGVZS= 2.D0*(V*(V*CFGVS+A*CFGAS)*C2+A*(V*CFGAS+A*CFGVS)*2.D0*C)
     1      *CHIS
      TGVZST=-2.D0*(W*CFGVS+U*CFGAS)*C3*CHIT
      TGVZTS=-2.D0*(W*FGVT+U*FGAT)*C3*CHIS
      TGVZT=4.D0*(V*(V*FGVT+A*FGAT)*C4+A*(V*FGAT+A*FGVT)*C5)*CHIT
C
      TGZVS=2.D0*(V*FZVS*C2+A*FZAS*2.D0*C)*CHIS
      TGZVST=-2.D0*(V*FZVT+A*FZAT)*C3*CHIT
      TGZVTS=-2.D0*(V*FZVS+A*FZAS)*C3*CHIS
      TGZVT=  (V*FZVT*C4+   A*FZAT*C5)*4.D0*CHIT
C
      FS1=V*CFZVS+A*CFZAS
      FS2=V*CFZAS+A*CFZVS
      FT1=V*FZVT+A*FZAT
      FT2=V*FZAT+A*FZVT
C
      TZVZS=2.D0*(W*FS1*C2+U*FS2*2.D0*C)*CHIS2
      TZVZST=-2.D0*(W*FS1+U*FS2)*C3*CHIT*DCONJG(CHIS)
      TZVZTS=-2.D0*(W*FT1+U*FT2)*C3*CHIT*CHIS
      TZVZT=4.D0*(W*FT1*C4+U*FT2*C5)*CHIT2
C
C
C  4) TERMS WHICH APPEAR WITH BOX DIAGRAMS
C
      TGZB=W*W*C2+U*U*2.D0*C
      TGZBST=-(W**2+U**2)*C3
      TGZBT=2.D0*(W*W*C4+U*U*C5)
C
      S16=16.D0*SW**2
      TGWS=C0*C0/S16
      TGWST=-2.D0*C3/S16
      TGWT=4.D0*(C0/C1)**2/S16
C
      W3=V2+3.D0*A2
      U3=3.D0*V2+A2
      TZBZS=(V2*W3*W3*C2+A2*U3*U3*2.D0*C)*CHIS
      TZBZST=-(V2*W3*W3+A2*U3*U3)*CHIT
      TZBZTS=TZBZST/CHIT*CHIS
      TZBZT=2.D0*(V2*W3*W3*C4+A2*U3*U3*C5)*CHIT
C
      VA2=(V+A)**2/S16
      TWZS=VA2*C0*C0*CHIS
      TWZST=-2.D0*VA2*C3*CHIT
      TWZTS=TWZST/CHIT*CHIS
      TWZT=4.D0*VA2*(C0/C1)**2*CHIT
C
      TGZ5S=(A2*C2+V2*2.D0*C)*CHIS
      TGZ5T =2.D0 *(A2*C4+V2*C5)*CHIT
      TZZ5S=(U*U*C2+W*W*2.D0*C)*CHIS2
      TZZ5T=2.D0*(U*U*C4+W*W*C5)*CHIT2
C
      TGZZ5S=U*U*C2+W*W*C*2.
      TGZZ5T=2.D0*(U*U*C4+W*W*C5)
      TZZZ5S=(A2*U3*U3*C2+V2*W3*W3*2.D0*C)*CHIS
      TZZZ5T=2.D0*(A2*U3*U3*C4+V2*W3*W3*C5)*CHIT
C
C END OF DEFINITION OF THE TIJ... TERMS      **************
C
C  NOW THE INFRARED CORRECTIONS ARE CALLED:
C  CIR: NON RESONANT
C  CIR1: INTERFERENCE RESONANT - NON RESONANT
C  CIR2: RESONANT
      CALL INFRA(DEL,S,T,CIR,CIR1,CIR2)
C  DEL: MAX. PHOTONENERGY/BEAM ENERGY
C  CIR1 COMPLEX, OTHERS REAL
C
C  SPECIFICATION OF THE FINITE QED CORRECTIONS:
      ME2=ME*ME
      BE=   DLOG(S/ME2)-1.D0
      X1=C1/2.D0
      X2=C0/2.D0
      DX1=DLOG(X1)
      DX2=DLOG(X2)
      X1C=DCMPLX(X1,0.D0)
      X2C=DCMPLX(X2,0.D0)
      SP1=SPENCE(X1C)
      SP2=SPENCE(X2C)
      X=DX1**2-DX2**2-2.D0*DREAL(SP1)+2.D0*DREAL(SP2)
      Z= 3.D0*BE-1.D0+2.D0*PI**2/3.D0
      Y=1.5D0*DX1-.5D0*DX1**2-PI**2/2.D0
C  TWO PHOTON BOXES
      CALL GBOX(S,T,V1S,V2S,A1S,A2S,V1T,V2T,A1T,A2T)
C  PHOTON-Z BOXES
      CALL GZBOX(S,T,V1ZS,V2ZS,A1ZS,A2ZS,V1ZT,V2ZT,A1ZT,A2ZT)
C  PHOTON VACUUM POLARIZATION
      PIGS=PIG(S)
      PIGT=PIG(T)
C     RPIGS=DREAL(PIGS)
      RPIGS=-REPI(S)
      IPIGS=DIMAG(PIGS)
C     RPIGT=DREAL(PIGT)
      RPIGT=-REPI(T)
CDYSON SUMMATION
      WPAS=DCMPLX(1D0,0D0)/DCMPLX(1D0-RPIGS,-IPIGS)
      WPAT=1D0/(1D0-RPIGT)
C  SPECIFICATION OF THE WEAK CORRECTIONS:
C  Z BOSON SELF ENERGY
      RZS=RESZ(S)
      IZS=IMSZ(S)
      IF(SM.NE.0.AND.ICHK.EQ.0)THEN
       DRPI=RZS/SM
       ICHK=ICHK+1
       WRITE(6,*) ' DERIV PI=',DRPI,' S-M^2=',SM
      ENDIF
      RZT=RESZ(T)
      ZS=DCMPLX(RZS,IZS)
      GM= MZ*GZ
      PROP= DCMPLX(SM,GM)
      PIZS=PROP/(DCMPLX(SM,0D0)+ZS)-DCMPLX(1.D0,0D0)
C DYSON SUMMATION
      PIZSW=PIZS+DCMPLX(1D0,0D0)
      PIZT=TM/(TM+RZT)-1.D0
      PIZTW=PIZT+1D0
      RPIZT=PIZT
      RPIZS=DREAL(PIZS)
      IPIZS=DIMAG(PIZS)
C  PHOTON-Z MIXING ENERGY
      RPIGZS=-RESGZ(S)/S
      IPIGZS=-IMSGZ(S)/S
      RPIGZT=-RESGZ(T)/T
C  HEAVY BOX DIAGRAMS
      CALL BOX(S,T,V1ZZS,V2ZZS,A1ZZS,A2ZZS,
     1             V1ZZT,V2ZZT,A1ZZT,A2ZZT,
     2             V1WS,V2WS,V1WT,V2WT)
C  COMPOSITION OF THE "REDUCED CROSS SECTIONS"     ***********
C  PHOTON-PHOTON
CW    DEL1=CIR+2.D0*RPIGS+AL*(X+Z+V1S+A1S*2.D0*C/C2)
CW    DEL2=CIR+RPIGS+RPIGT+AL*(X+Y+Z+.5D0*(V1S+V1T+A1S+A1T))
CW    DEL3=CIR+2.D0*RPIGT+AL*(X+2.D0*Y+Z+V1T+A1T*C5/C4)
CW    SGGS=C2*(1.D0+DEL1)+2.D0*DREAL(TGGVS)
CW    SGGST=-2.D0*C3*(1.D0+DEL2)
CW    SGGT= 2.D0*C4*(1.D0+DEL3)
      DEL1=CIR           +AL*(X+Z+V1S+A1S*2.D0*C/C2)
      DEL2=CIR            +AL*(X+Y+Z+.5D0*(V1S+V1T+A1S+A1T))
      DEL3=CIR           +AL*(X+2.D0*Y+Z+V1T+A1T*C5/C4)
      SGGS=C2*CDABS(WPAS)**2*(1.D0+DEL1)+2.D0*DREAL(TGGVS)
     1    +AL*(TGZB*V1ZZS+TGZZ5S*A1ZZS+TGWS*V1WS)
      SGGST=-2.D0*C3*WPAT*DREAL(WPAS)*(1.D0+DEL2)
     1      +2.D0*DREAL(TGGVTS+TGGVST)
     2      +AL*(TGZBST*(V1ZZS+A1ZZS+V1ZZT+A1ZZT)
     3      +TGWST*(V1WS+V1WT))
      SGGT= 2.D0*C4*WPAT**2*(1.D0+DEL3)
     1      +2.D0*DREAL(TGGVT)
     2      +AL*(TGZBT*V1ZZT+TGZZ5T*A1ZZT+TGWT*V1WT)
C  PHOTON-Z-INTERFERENCE
      RCIR=DREAL(CIR1)
      ICIR=DIMAG(CIR1)
CW    DEL11=RCIR+RPIGS+AL*(X+Z+.5D0*(V1S+V1ZS))+RPIZS
CW    DEL12=ICIR-IPIGS+ALFA*(V2ZS-V2S)+IPIZS
CW    SGZS=2.D0*DREAL(TGZS)*(1.+DEL11)-DIMAG(TGZS)*DEL12*2.D0
CW   2     +2.D0*DREAL(TGGZS)*RPIGZS -2.D0*DIMAG(TGGZS)*IPIGZS
      DEL11=RCIR      +AL*(X+Z+.5D0*(V1S+V1ZS))
      DEL12=ICIR      +ALFA*(V2ZS-V2S)
      SGZS=2.D0*DREAL(TGZS*PIZSW*DCONJG(WPAS))*(1.+DEL11)
     1               -DIMAG(TGZS*PIZSW*DCONJG(WPAS))*DEL12*2.D0
     1     +AL*DREAL(TGZ5S)*(A1S+A1ZS)-2.D0*ALFA*DIMAG(TGZ5S)
     &                              *(A2ZS-A2S)
     2     +2.D0*DREAL(TGGZS*PIZSW*DCONJG(WPAS))*RPIGZS
     2             -2.D0*DIMAG(TGGZS*PIZSW*DCONJG(WPAS))*IPIGZS
     3     +2.D0*DREAL(TGVZS+TGZVS)
     4     +AL*(DREAL(TZBZS)*V1ZZS+DREAL(TZZZ5S)*A1ZZS
     5         +DREAL(TWZS)*V1WS)
     6     +2.D0*ALFA*(DIMAG(TZBZS)*V2ZZS+DIMAG(TZZZ5S)*A2ZZS
     7              +DIMAG(TWZS)*V2WS)
C
      DEL21=CIR+RPIGS+AL*(X+Y+Z+.5D0*(V1S+A1S+V1ZT+A1ZT))+RPIZT
      DEL22=IPIGS-ALFA*(1.5D0-V2S-A2S+V2ZT+A2ZT)
      SGZST=DREAL(TGZST)*(1.D0+DEL21)*2.D0
     1      +2.D0*TGGZST*RPIGZT +2.D0*DREAL(TGVZST+TGZVST)
     2      +AL*(TZBZST*(V1ZZS+A1ZZS)+TWZST*V1WS)
CW    DEL31=RCIR+RPIGT+AL*(X+Y+Z+.5D0*(V1T+A1T+V1ZS+A1ZS))+RPIZS
CW    DEL32=ICIR-ALFA *(1.5D0 +A2T-V2ZS-A2ZS+V2T)+IPIZS
CW    SGZTS=2.D0*DREAL(TGZTS)*(1.D0+DEL31)-2.D0*DIMAG(TGZTS)*DEL32
CW   1      +2.D0*DREAL(TGGZTS)*RPIGZS-2.D0*DIMAG(TGGZTS)*IPIGZS
      DEL31=RCIR      +AL*(X+Y+Z+.5D0*(V1T+A1T+V1ZS+A1ZS))
      DEL32=ICIR-ALFA *(1.5D0 +A2T-V2ZS-A2ZS+V2T)
      SGZTS=2.D0*DREAL(TGZTS*PIZSW*WPAT)*(1.D0+DEL31)
     1            -2.D0*DIMAG(TGZTS*PIZSW*WPAT)*DEL32
     1      +2.D0*DREAL(TGGZTS*PIZSW*WPAT)*RPIGZS
     1              -2.D0*DIMAG(TGGZTS*PIZSW*WPAT)*IPIGZS
     2      +2.D0 *DREAL(TGVZTS+TGZVTS)
     3      +AL*(DREAL(TZBZTS)*(V1ZZT+A1ZZT)+DREAL(TWZTS)*V1WT)
     4      +2.D0*ALFA*(DIMAG(TZBZTS)*(V2ZZT+A2ZZT)
     5      +DIMAG(TWZTS)*V2WT)
      DEL41=CIR+RPIGT+AL*(X+2.D0*Y+Z+.5D0*(V1T+V1ZT))+RPIZT
      SGZT=2.D0*DREAL(TGZT)*(1.D0+DEL41)+    TGZ5T *AL*(A1T+A1ZT)
     1     +2.D0*TGGZT*RPIGZT+2.D0*DREAL(TGZVT+TGVZT)
     2     +AL*(TZBZT*V1ZZT+TZZZ5T*A1ZZT+TWZT*V1WT)
C  Z-Z TERMS
CW    DEL51=CIR2+AL*(X+Z+V1ZS)+2.D0*RPIZS
      DEL51=CIR2+AL*(X+Z+V1ZS)
CW    SZZS=TZZS*(1.D0+DEL51)+TZZ5S*AL*A1ZS
      SZZS=TZZS*CDABS(PIZSW)**2*(1.D0+DEL51)+TZZ5S*AL*A1ZS
CW   1     +2.D0*TGZZS*RPIGZS+2.D0*DREAL(TZVZS)
     1     +2.D0*TGZZS*CDABS(PIZSW)**2*RPIGZS+2.D0*DREAL(TZVZS)
      DEL61=RCIR+AL*(X+Y+Z+.5D0*(V1ZS+V1ZT+A1ZS+A1ZT))+RPIZS+PIZT
      DEL62=ICIR-ALFA*(1.5D0+V2ZT-V2ZS+A2ZT-A2ZS)+IPIZS
      SZZST=2.D0*DREAL(TZZTS)*(1.+DEL61)-2.*DIMAG(TZZTS)*DEL62
     1      +2.D0*DREAL(TGZZTS)*(RPIGZS+RPIGZT)
     2      +2.D0*DREAL(TZVZTS+TZVZST)-2.D0*DIMAG(TGZZTS) *IPIGZS
      DEL71=CIR+AL*(X+2.D0*Y+Z +V1ZT )+2.D0*PIZT
      SZZT=TZZT*(1.D0+DEL71)+TZZ5T*AL*A1ZT
     1     +2.D0*TGZZT*RPIGZT+2.D0*DREAL(TZVZT)
C  RADIATIVELY CORRECTED CROSS SECTION
      DSIG=SGGS+SGZS+SZZS
     1     +SGGST+SGZST+SGZTS+SZZST
     3     +SGGT+SGZT+SZZT
C     DSIG=DSIG*CST
C  CROSS SECTION IN LOWEST ORDER IN NBARN
      DSIG0=C2-2.D0*C3+2.D0*C4
     1      +2.D0*DREAL(TGZS+TGZTS)+(TGZT+TGZST)*2.D0
     2      +TZZS+2.D0*DREAL(TZZTS)+TZZT
C     DSIG0=DSIG0*CST
      BSIG0=C2-2.D0*C3+2.D0*C4 +TZZS
      RETURN
      END
*DECK,FINDMW
      SUBROUTINE FINDMW(MZ,MT,MH)
C
C   DETERMINE SW**2 AND MW FOR GIVEN MZ VIA MU-LIFETIME
C   W. HOLLIK, HAMBURG UNIV.,  DATE APRIL 29, 1987
C   IMPLEMENTED AS SUBROUTINE BY RONALD 29-4
C   MZ = Z0 MASS IN GEV;
C   MT = TOP MASS IN GEV
C   MH = HIGGS MASS IN GEV
C
      IMPLICIT REAL*8(A-Z)
      INTEGER I,IIN,IUT
      DIMENSION SINW(20)
      COMMON /COUPA/AL
      COMMON / TOPCOM / XMT
      COMMON / UNICOM / IIN,IUT
      COMMON / FINCOM / MW,SW
      CALL OUTCRY('FINDMW')
      WRITE(IUT,1) MZ,MT,MH
    1 FORMAT(' FINDMW : MZ,MT,MH =',3F13.4)
      XMZ=MZ*MZ
      XMT=MT
      PI=3.14159265D0
      ALFA=1.D0/137.036D0
      AL=ALFA/4.D0/PI
C     A0=37.281D0**2*100D0
      A0=37.281D0**2
      I=1
C  SW = SIN**2 THETA-W, START WITH APPROXIMATE VALUE
      SQ=1.D0-4.D0*A0/XMZ/(1.D0-.07D0)
      IF(SQ.LE.0.D0) THEN
        WRITE(IUT,2) MZ
    2   FORMAT(' FINDMW : Z0 MASS ',F9.4,'  IS TOO LOW')
        STOP
      ENDIF
      SQ=DSQRT(SQ)
      SW=(1.D0-SQ)/2.D0
      SINW(1)=SW
      CW=1.D0-SW
      SW1=DSQRT(SW)
      CW1=DSQRT(CW)
      MW=MZ*CW1
C  CORR MEANS THE QUANTITY 'DELTA R' IN MU LIFETIME FORMULA
   51 STD=PSE0(MZ,MW,MH)
      CORR=-STD
      SQ=DSQRT(1.D0-4.D0*A0/XMZ/(1.D0+STD))
C  THE CORRECTED VALUE FOR SIN**2 THETA-W
      SW=(1.D0-SQ)/2.D0
      SINW(I+1)=SW
      DSIN=DABS(SINW(I+1)-SINW(I))
      CW1=DSQRT(1.D0-SW)
C  THE CORRECTED VALUE FOR THE W MASS
      MW=MZ*CW1
      IF(DSIN.LT.1D-5) GOTO 100
      I=I+1
      IF(I.GT.10) GOTO 101
      GOTO 51
  100 CONTINUE
CCCCCCCCCCCCC
      MW=80.215D0
      SW=.2259D0
      WRITE(IUT,25) I,MW,SW
   25 FORMAT(' FINDMW : CONVERGED TO 4 DIGITS AFTER',I3,' STEPS :'/,
     .       ' FINDMW : RESULT FOR THE W MASS =',F10.4,' GEV'/,
     .       ' FINDMW : AND FOR SIN**2(TH_W)  =',F10.4/,' ',50('-'))
      RETURN
  101 WRITE(IUT,26)
   26 FORMAT(' FINDMW : NO SUFFICIENT CONVERGENCE IN 10 STEPS')
CCCCCCCCCCCCCCCCC
      MW=80.215D0
      SW=.2259D0
      RETURN
      END
*DECK,FFUNCT
      DOUBLE PRECISION FUNCTION FFUNCT(Y,A,B)
      IMPLICIT REAL*8(A-Z)
      IF(A.LT.1.0D-05) GO TO 50
      F1=2.D0
      IF(A.EQ.B) GO TO 10
      F1=1.D0+((A*A-B*B)/Y-(A*A+B*B)/(A*A-B*B))*DLOG(B/A)
   10 CONTINUE
      Q=(A+B)*(A+B)
      P=(A-B)*(A-B)
      IF(Y.LT.P) GO TO 20
      IF(Y.GE.Q) GO TO 30
      F2=DSQRT((Q-Y)*(Y-P))*(-2.D0)*DATAN(DSQRT((Y-P)/(Q-Y)))
      GO TO 40
   20 CONTINUE
      F2=DSQRT((Q-Y)*(P-Y))*DLOG((DSQRT(Q-Y)+DSQRT(P-Y))**2/
     &                                               (4.D0*A*B))
      GO TO 40
   30 CONTINUE
      F2=DSQRT((Y-Q)*(Y-P))*(-1.D0)*DLOG((DSQRT(Y-P)+DSQRT(Y-Q))**2/
     &                                               (4.D0*A*B))
   40 CONTINUE
      F=F1+F2/Y
      GO TO 70
   50 CONTINUE
      IF(Y.EQ.(B*B)) GO TO 65
      IF(Y.LT.(B*B)) GO TO 60
      F=1.D0+(1.D0-B*B/Y)*DLOG(B*B/(Y-B*B))
      GO TO 70
   60 CONTINUE
      F=1.D0+(1.D0-B*B/Y)*DLOG(B*B/(B*B-Y))
      GO TO 70
   65 CONTINUE
      F=1.D0
   70 FFUNCT=F
      RETURN
      END
*DECK,PSE0
      DOUBLE PRECISION FUNCTION PSE0(MZ,M0,MH)
C  STANDARD W-SELF ENERGY AT K**2 = 0 , VERTEX AND BOX DIAGRAMS.
C  MZ = Z-MASS, M0 = W-MASS, MH = HIGGS-MASS
      IMPLICIT REAL*8(A-Z)
C     INTEGER I,J,L,RS
      INTEGER I,J,L
      DIMENSION M(6,2),MF(6,2),V(6,2),A(6,2),VF(6,2),T(25)
      COMMON /COUPA/ALPHA
      COMMON / TOPCOM / MT
      PI=3.14159265D0
      Z=MZ*MZ
      W=M0*M0
      H=MH*MH
      S=1D0-W/Z
      C=1D0-S
C  FERMION MASSES
      MF(1,1)=0.D0
      MF(1,2)=.5110034D-3
      MF(2,1)=0.D0
      MF(2,2)=.10565943D0
      MF(3,1)=0.D0
      MF(3,2)=1.7842D0
      MF(4,1)=.032D0
      MF(4,2)=.0321D0
      MF(5,1)=1.5D0
      MF(5,2)=.15D0
      MF(6,1)=MT
      MF(6,2)=4.5D0
      M(1,1)=0.D0
      M(2,1)=0.D0
      M(3,1)=0.D0
      M(1,2)=MF(1,2)**2
      M(2,2)=MF(2,2)**2
      M(3,2)=MF(3,2)**2
      M(4,1)=MF(4,1)**2
      M(4,2)=MF(4,2)**2
      M(5,1)=MF(5,1)**2
      M(5,2)=MF(5,2)**2
      M(6,1)=MF(6,1)**2
      M(6,2)=MF(6,2)**2
C  FERMION COUPLING CONSTANTS
      A1=DSQRT(1.D0/(16.D0*C*S))
      VL=(4.D0*S-1.D0)*A1
      AL=-A1
      VU=-(8.D0/3.D0*S-1.D0)*A1
      VD=(4.D0/3.D0*S-1.D0)*A1
      VF(1,1)=A1
      VF(2,1)=A1
      VF(3,1)=A1
      VF(1,2)=VL
      VF(2,2)=VL
      VF(3,2)=VL
      VF(4,1)=VU
      VF(5,1)=VU
      VF(6,1)=VU
      VF(4,2)=VD
      VF(5,2)=VD
      VF(6,2)=VD
      DO 2 I=1,6
        DO 1 J=1,2
          V(I,J)=VF(I,J)**2
          A(I,J)=A1*A1
    1   CONTINUE
    2 CONTINUE
C
C ON SHELL SELFENERGY (START)
      T(1)=0D0
      DO 71 J=1,3,1
        T(1)=T(1)-M(J,2)/2D0-M(J,2)/4D0
   71 CONTINUE
      T(1)=T(1)/(3D0*S)
      T(2)=0D0
      DO 72 L=4,6,1
        T(2)=T(2)-(M(L,1)+M(L,2))/2D0*(1D0-(M(L,1)+M(L,2))/
     &       (M(L,1)-M(L,2))*DLOG(MF(L,1)/MF(L,2)))-
     &       (M(L,1)+M(L,2))/4D0 - M(L,1)*M(L,2)/(M(L,1)-M(L,2))*
     &       DLOG(M(L,2)/M(L,1))/2D0
   72 CONTINUE
      T(2)=T(2)/S
      T(3)=(7D0*Z+7D0*W)*(1D0-Z/(Z-W)*DLOG(Z/W))-
     &      Z-W - Z*W/(Z-W)*DLOG(W/Z)*2D0
      T(3)=T(3)*C/(-3D0*S)
      T(4)=W/3D0
      T(5)=(Z+W)/24D0 + Z*W/(Z-W)*DLOG(W/Z)/12D0+
     &     (H+W)/24D0 + H*W/(H-W)*DLOG(W/H)/12D0
      T(5)=T(5)/S
C SUBSTITUTIONS X>W , FACTOR (-1)
      T(6)=0D0
      DO 76 J=1,3,1
        T(6)=T(6)+(W-M(J,2)/2D0)*(1D0+FFUNCT(W,0D0,MF(J,2)))-
     &       W/3.D0 - M(J,2)*M(J,2)/(2D0*W)*FFUNCT(W,0D0,MF(J,2))
   76 CONTINUE
      T(6)=T(6)/(3D0*S)*(-1D0)
      T(7)=0D0
      DO 77 L=4,6,1
        T(7)=T(7)+(W-(M(L,1)+M(L,2))/2D0)*(1D0-(M(L,1)+M(L,2))/
     &       (M(L,1)-M(L,2))*DLOG(MF(L,1)/MF(L,2))+
     &       FFUNCT(W,MF(L,1),MF(L,2))) -
     &       W/3D0-(M(L,1)-M(L,2))*(M(L,1)-M(L,2))/(2D0*W)*
     &       FFUNCT(W,MF(L,1),MF(L,2))
   77 CONTINUE
      T(7)=T(7)/S*(-1D0)
      T(8)=(7D0*Z+7D0*W+10D0*W)*(1D0-Z/(Z-W)*DLOG(Z/W)+
     &     FFUNCT(W,MZ,M0)) +
     &     2D0*W/3D0-2D0*(Z-W)*(Z-W)/W*FFUNCT(W,MZ,M0)
      T(8)=T(8)*C/(-3D0*S)*(-1D0)
      T(9)=(-32D0)*W/9D0-(4D0*W+10D0*W)/3D0*FFUNCT(W,0D0,M0)+
     &     2D0/3D0*W*FFUNCT(W,0D0,M0)+S/C*W*FFUNCT(W,MZ,M0)+
     &     W/S*FFUNCT(W,MH,M0)
      T(9)=T(9)*(-1D0)
      T(10)=5D0/18D0*W-W/12D0*(Z/(Z-W)*DLOG(Z/W)+H/(H-W)*DLOG(H/W))+
     &      ((Z-W)*(Z-W)/(2D0*W)-(W+Z-W/2D0))/6D0*FFUNCT(W,MZ,M0)+
     &      ((H-W)*(H-W)/(2D0*W)-(W+H-W/2D0))/6D0*FFUNCT(W,MH,M0)
      T(10)=T(10)/S*(-1D0)
C
      DO 7 I=1,10
        T(I)=T(I)/(-W)
    7 CONTINUE
C
      T(11)=0D0
      DO 711 J=1,3,1
        T(11)=T(11)+(FFUNCT(Z,MF(J,2),MF(J,2))-1D0/3D0)*
     &        (V(J,1)+V(J,2)+A(J,1)+A(J,2)) +
     &        (2D0*(V(J,2)+A(J,2))-3D0/(8D0*C*S))*M(J,2)/Z*
     &        FFUNCT(Z,MF(J,2),MF(J,2))
  711 CONTINUE
      T(11)=T(11)*C/S*4D0/3D0
      T(12)=0D0
      DO 712 L=4,6,1
        DO 172 I=1,2,1
          FZLI=FFUNCT(Z,MF(L,I),MF(L,I))
          T(12)=T(12)+(FFUNCT(Z,MF(L,I),MF(L,I))-1D0/3D0)*
     &          (V(L,I)+A(L,I)) +
     &          (2D0*(V(L,I)+A(L,I))-3D0/(8D0*C*S))*M(L,I)/Z*FZLI
  172   CONTINUE
  712 CONTINUE
      T(12)=T(12)*4D0*C/S
      T(13)=(C/(3D0*S)*(10D0+20D0*C)-1D0/S-(C-S)*(C-S)/(12D0*S*C)*
     &      (1D0+8D0*C))*FFUNCT(Z,M0,M0)
      T(13)=T(13)*C/S*(-1D0)
      T(14)=(1D0/(12D0*S*C)*(2D0*H/Z-11D0) - 1D0/(12D0*S*C)*(H-Z)*
     &      (H-Z)/(Z*Z))*FFUNCT(Z,MH,MZ)
      T(14)=T(14)*C/S*(-1D0)
      T(15)=(11D0-2D0*H/Z)*(1D0-(H+Z)/(2D0*(H-Z))*
     &      DLOG(H/Z)-DLOG(MH*MZ/W))
      T(15)=T(15)/(12D0*S*C)*C/S
      T(16)=(-1D0)/(6D0*S*C)*(H/Z*DLOG(H/W) + DLOG(Z/W)) - 2D0*C/
     &      (9D0*S) + 1D0/(18D0*S*C) + (C-S)*(C-S)/(18D0*S*C)
      T(16)=T(16)*C/S
C END OF COMPUTATION OF RE(SIGMA(Z))
C
      T(17)=0D0
      DO 717 J=1,3,1
        T(17)=T(17)+(1D0-M(J,2)/(2D0*W))*(1D0+FFUNCT(W,0D0,MF(J,2)))-
     &        1D0/3D0 - 0.5D0*M(J,2)*M(J,2)/(W*W)*FFUNCT(W,0D0,MF(J,2))
  717 CONTINUE
      T(17)=T(17)/(3D0*S)*C/S*(-1D0)
      T(18)=0D0
      DO 718 L=4,6,1
        T(18)=T(18)+(1D0-(M(L,1)+M(L,2))/(2D0*W))*(1D0-(M(L,1)+M(L,2))/
     &        (M(L,1)-M(L,2))*DLOG(MF(L,1)/MF(L,2)) +
     &        FFUNCT(W,MF(L,1),MF(L,2))) -
     &        1D0/3D0 - 0.5D0*(M(L,1)-M(L,2))*(M(L,1)-M(L,2))/(W*W)*
     &        FFUNCT(W,MF(L,1),MF(L,2))
  718 CONTINUE
      T(18)=T(18)/S*C/S*(-1D0)
      T(19)=(5D0*S/(3D0*C)-7D0/3D0-8D0*C/S+1D0/(12D0*S)*Z/W*(Z/W-4D0))*
     &      FFUNCT(W,MZ,M0)
      T(19)=T(19)*C/S*(-1D0)
      T(20)=(1D0/S+1D0/(12D0*S)*H/W*(H/W-4D0))*FFUNCT(W,MH,M0) -
     &      4D0*FFUNCT(W,0D0,M0)
      T(20)=T(20)*C/S*(-1D0)
      T(21)=(1D0+8D0*C/S-S/C+1D0/(4D0*S))*Z/(Z-W)*DLOG(Z/W)
      T(21)=T(21)*C/S*(-1D0)
      T(22)=(-3D0)/(4D0*S)*H/(H-W)*DLOG(H/W) +
     &      (17D0/18D0-(Z+H)/(6D0*W))/S + S/C - 2D0*C/(9D0*S) -
     &      8D0*C/S - 44D0/9D0 - 7D0/3D0
      T(22)=T(22)*C/S*(-1D0)
C END OF COMPUTATION OF -RE(SIGMA(W))
C
      T(23)=0D0
      DO 723 L=4,6,1
        T(23)=T(23)+(4D0/3D0 + (M(L,1)-M(L,2))/(Z-W))*DLOG(M(L,1)/
     &        M(L,2))
  723 CONTINUE
      T(23)=(T(23)/(4D0*S) + 2D0/3D0)
C
      SIG0=0D0
      DO 700 I=1,23,1
        SIG0=SIG0+T(I)
  700 CONTINUE
      SIG0=SIG0*ALPHA
      PSE0=SIG0-ALPHA/S*(6.D0+(7.D0-4.D0*S)/2.D0/S *DLOG(C))
C
      RETURN
      END
*DECK,GBOX
      SUBROUTINE GBOX(S,T,V1S,V2S,A1S,A2S,V1T,V2T,A1T,A2T)
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16 VS,VT,AS,AT,CPI,MST,MSU,MTS,MTU
      PI=3.1415926536D0
      CPI=DCMPLX(0.D0,PI)
      LST=DLOG(-T/S)
      U=-S-T
      LSU=DLOG(-U/S)
      MST=.5D0*S/(S+T)*(LST+CPI)
     1  -.25D0*S*(S+2.D0*T)/(S+T)**2*(LST**2+2.D0*CPI*LST)
      MSU=.5D0*S/(S+U)*(LSU+CPI)
     1  -.25D0*S*(S+2.D0*U)/(S+U)**2*(LSU**2+2.D0*CPI*LSU)
      LTS=-LST
      LTU=DLOG(T/U)
      MTS=.5D0*T/(S+T)*(LTS-CPI)
     1  -.25D0* T*(T+2.D0*S)/(S+T)**2*(LTS**2-2.D0*CPI*LTS)
      MTU=-.5D0*T/(T+U)*LTU
     1  -.25D0*T*(T+2.D0*U)/(T+U)**2*(LTU**2+PI**2)
      VS=MST-MSU+2.D0*CPI*LTU
      VT=MTS-MTU+2.D0*LST*(LSU+CPI)
      AS=MST+MSU
      AT=MTS+MTU
      PI2=2.D0*PI
C
      V1S=DREAL(VS)
      V2S=DIMAG(VS)/PI2
      A1S=DREAL(AS)
      A2S=DIMAG(AS)/PI2
      V1T=DREAL(VT)
      V2T=DIMAG(VT)/PI2
      A1T=DREAL(AT)
      A2T=DIMAG(AT)/PI2
      RETURN
      END
*DECK,GZBOX
       SUBROUTINE GZBOX(S,T,V1ZS,V2ZS,A1ZS,A2ZS,V1ZT,V2ZT,A1ZT,A2ZT)
       IMPLICIT REAL*8(A-Z)
       COMPLEX*16 VS,VT,AS,AT,M,ABOX,     CPI,SPENCE,
     1            LMSS,LMSM,CDLOG,VST,VSU,AST,ASU,VTS,VTU,
     2            ATS,ATU,LMT,LMTM
       COMMON /BOSEW/MZ,MW,MH/WIDTH/GZ
       U=-S-T
       XM2=MZ*MZ
       YM2=-MZ*GZ
       M=DCMPLX(XM2,YM2)
       PI=3.1415926536D0
       CPI=DCMPLX(0.D0,PI)
       PI2=2.D0*PI
       AST=ABOX(S,T)
       ASU=ABOX(S,U)
       ATS=ABOX(T,S)
       ATU=ABOX(T,U)
       LMSS=CDLOG((M-S)/S)
       LMSM=CDLOG(M/(M-S))
       LMTM=CDLOG(M/(M-T))
       LMT=CDLOG((M-T)/S)
       VST=AST+  (SPENCE((M+T)/T)+   LMSM*DLOG(-T/S))*2.D0
       VSU=ASU+  (SPENCE((M+U)/U)+   LMSM*DLOG(-U/S))*2.D0
       VTS=ATS+  (SPENCE((M+S)/S)+   LMTM*DLOG(-S/T))*2.D0
       VTU=ATU+  (SPENCE((M+U)/U)+   LMTM*(DLOG(U/T)+CPI))*2.D0
C
       VS=VST-VSU-   DLOG(U/T)*LMSS*2.D0
       VT=VTS-VTU+   (DLOG(-U/S)+CPI)*LMT*2.D0
       AS=AST+ASU
       AT=ATS+ATU
C
       V1ZS=DREAL(VS)
       A1ZS=DREAL(AS)
       V1ZT=DREAL(VT)
       A1ZT=DREAL(AT)
       V2ZS=DIMAG(VS)/PI2
       A2ZS=DIMAG(AS)/PI2
       V2ZT=DIMAG(VT)/PI2
       A2ZT=DIMAG(AT)/PI2
       RETURN
       END
*DECK,ABOX
      FUNCTION ABOX(S,T)
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16 ABOX,SPENCE,CDLOG,M,KLAM1,KLAM2,A
      COMMON /BOSEW/MZ,MW,MH/WIDTH/GZ
      XMZ=MZ*MZ
      YMZ=-MZ*GZ
      M=DCMPLX(XMZ,YMZ)
      KLAM1=SPENCE(S/M)-SPENCE(-T/M)
     1     +CDLOG(-T/M)*CDLOG((M-S)/(M+T))
      KLAM2=CDLOG(T/(S-M))+M/S*CDLOG((M-S)/M)
      A=(S+2.D0*T+M)/(S+T)*KLAM1+KLAM2
      ABOX=A*(S-M)/(S+T)
      RETURN
      END
*DECK,PIG
      FUNCTION PIG(S)
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16 PIG,CPI,IPIL,IPIH,IPIT,PII
      COMMON /LEPT/ME,MMU,MTAU/HAD/MU,MD,MS,MC,MB,MT
     1       /ALF/AL,ALPHA,ALFA
     2       /BOSEW/MZ,MW,MH
      PI=3.1415926536D0
      CPI=DCMPLX(0.D0,PI)
      QU=4.D0/3.D0
      QD=1.D0/3.D0
C  SQUARE OF QUARK CHARGES INCLUDING COLOUR FAKTOR 3
      X=DABS(S)
      W=DSQRT(X)
      L1=   DLOG(W/ME)*2.D0
      L2= -(1.D0+2.D0*MMU**2/S) *F(S,MMU,MMU)+2.D0
      L3= -(1.D0+2.D0*MTAU**2/S)*F(S,MTAU,MTAU)+2.D0
      H1= -(1.D0+2.D0*MU**2/S)*F(S,MU,MU)+2.D0
      H2= -(1.D0+2.D0*MD**2/S)*F(S,MD,MD)+2.D0
      H3= -(1.D0+2.D0*MS**2/S)*F(S,MS,MS)+2.D0
      H4= -(1.D0+2.D0*MC**2/S)*F(S,MC,MC)+2.D0
      H5= -(1.D0+2.D0*MB**2/S)*F(S,MB,MB)+1.D0/3.D0
      H6= -(1.D0+2.D0*MT**2/S)*F(S,MT,MT)+1.D0/3.D0
      PIL=(L1+L2+L3-5.D0)*AL/3.D0
      PIH=(QU*(H1+H4+H6-10.D0/3.D0)+QD*(H2+H3+H5-10.D0/3.D0))*AL/3.D0
C   GAUGE AND HIGGS PART
      PIW=(3.D0+4.D0*MW*MW/S)*F(S,MW,MW)-2.D0/3.D0
      PIW= ALPHA*PIW
      PIR=PIL+PIH+PIW
      IPIL=-CPI*AL
      IPIH=-CPI*(2.D0*QU+3.D0*QD)
      MTH=4.D0*MT*MT
      IF(S.LT.MTH) GOTO 5
      SQ=DSQRT(1.D0-MTH/S)
      IPIT=-CPI*SQ*(1.D0+MTH/2.D0/S)*QU
      IPIH=(IPIH+IPIT)*AL/3.D0
      PII=IPIL+IPIH
      PIG=DCMPLX(PIR,0.D0)+PII
      GOTO 7
    5 IF(S.LT.0.) GOTO 6
      PII= IPIL+IPIH *AL/3.D0
      PIG=DCMPLX(PIR,0.D0)+PII
      GOTO 7
    6 PIG=DCMPLX(PIR,0.D0)
    7 RETURN
      END
*DECK,F
      DOUBLE PRECISION FUNCTION F(Y,A,B)
      IMPLICIT REAL*8(A-Z)
      IF(A.LT.1.0D-05) GO TO 50
      F1=2.D0
      IF(A.EQ.B) GO TO 10
      F1=1.D0+((A*A-B*B)/Y-(A*A+B*B)/(A*A-B*B))*DLOG(B/A)
   10 CONTINUE
      Q=(A+B)*(A+B)
      P=(A-B)*(A-B)
      IF(Y.LT.P) GO TO 20
      IF(Y.GE.Q) GO TO 30
      F2=DSQRT((Q-Y)*(Y-P))*(-2.D0)*DATAN(DSQRT((Y-P)/(Q-Y)))
      GO TO 40
   20 CONTINUE
      F2=DSQRT((Q-Y)*(P-Y))*DLOG((DSQRT(Q-Y)+DSQRT(P-Y))**2/
     &                                               (4.D0*A*B))
      GO TO 40
   30 CONTINUE
      F2=DSQRT((Y-Q)*(Y-P))*(-1.D0)*DLOG((DSQRT(Y-P)+DSQRT(Y-Q))**2/
     &                                               (4.D0*A*B))
   40 CONTINUE
      F=F1+F2/Y
      GO TO 70
   50 CONTINUE
      IF(Y.EQ.(B*B)) GO TO 65
      IF(Y.LT.(B*B)) GO TO 60
      F=1.D0+(1.D0-B*B/Y)*DLOG(B*B/(Y-B*B))
      GO TO 70
   60 CONTINUE
      F=1.D0+(1.D0-B*B/Y)*DLOG(B*B/(B*B-Y))
      GO TO 70
   65 CONTINUE
      F=1.D0
   70 CONTINUE
      RETURN
      END
*DECK,XINT
      SUBROUTINE XINT(S,T,M1,IST,I5ST)
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16 IST,I5ST,SPENCE,CDLOG,X1,X2,Y1,Y2,LN2,SP4,P1,P2,K1,
     1           LN,SQX,SQY,Z1,Z2,Z3,Z4,M
      COMMON /WIDTH/GZ
      XM2=M1*M1
      YM2=-M1*GZ
      M=DCMPLX(XM2,YM2)
      SQY=CDSQRT(1.D0-4.D0*M/S)
      SQX=CDSQRT(1.D0-4.D0*M/S*(1.D0+M/T))
      Y1=(1.D0+SQY)*.5D0
      Y2=(1.D0-SQY)*.5D0
      X1=(1.D0+SQX)*.5D0
      X2=(1.D0-SQX)*.5D0
      Z1=X1/(X1-Y1)
      Z2=X1/(X1-Y2)
      Z3=X2/(X2-Y1)
      Z4=X2/(X2-Y2)
      SP4=SPENCE(Z1)+SPENCE(Z2)-SPENCE(Z3)-SPENCE(Z4)
      LN=CDLOG(-Y1/Y2)
      LN2=LN*LN
      SP4=SP4/(X1-X2)
      K1=SPENCE(1.D0+T/M)-LN2-3.1415926536D0**2/6.
      P1=(2.D0*T+S+2.D0*M)/(S+T)/2.D0
      P2=(S+2.D0*T-4.D0*T*M/S+2.D0*M*M/T-2.D0*M*M/S)/(S+T)/2.D0
      I5ST=S/(S+T)*(P1*K1+.5D0*CDLOG(-T/M)+(Y2-Y1)/2.D0*LN-P2*SP4)
      IST =I5ST+2.D0*LN2+2.D0*SP4
      RETURN
      END
*DECK,RESZ
      FUNCTION RESZ(X)
C   REAL PART OF THE Z SELF ENERGY
      IMPLICIT REAL*8(A-Z)
      INTEGER I,J,L
      DIMENSION M(6,2),V(6,2),A(6,2),VF(6,2),MF(6,2),T(25)
      COMMON /BOSEW/MZ,M0,MH/FERMI/MF,VF,M,V,A
     1       /ALF/AL,ALPHA,ALFA
     2       /COUP/SW,CW,VL,A1,VU,AU,VD,AD
C  MF IS THE ARRAY OF FERMION MASSES, M THAT OF FERMION MASS SQUARED
C  I = FAMILY INDEX (1...6); J = 1,2 DENOTES UP AND DOWN MEMBERS
C  VF IS THE ARRAY OF CORRESPONDING VECTOR COUPLINGS
C  V,A ARE THE ARRAYS OF THE SQUARES OF V AND A COUPLINGS
      PI=3.1415926536D0
      GF=1.6632D-5
      Z=MZ*MZ
      W=M0*M0
      H=MH*MH
      C=CW
      S=SW
      DO 111 L=1,25,1
        T(L)=0.D0
  111 CONTINUE
      T(1)=0.D0
      C13=1.D0/3.D0
      C38=3.D0/8.D0
      DO 71 I=1,3,1
        T(1)=T(1) + (V(I,1)+A(I,1))*(X*(F(X,MF(I,2),MF(I,2))-C13)-
     &       Z*(F(Z,MF(I,2),MF(I,2))-C13))
   71 CONTINUE
      T(1)=T(1)*4.D0/3.D0
      T(2)=0.D0
      DO 72 I=1,3,1
        T(2)=T(2) + (V(I,2)+A(I,2))*(X*(F(X,MF(I,2),MF(I,2))-C13)-
     &       Z*(F(Z,MF(I,2),MF(I,2))-C13)  +
     &       2.D0 *M(I,2)*(F(X,MF(I,2),MF(I,2))-F(Z,MF(I,2),MF(I,2))))-
     &       C38*M(I,2)/(C*S)*(F(X,MF(I,2),MF(I,2))-
     &       F(Z,MF(I,2),MF(I,2)))
   72 CONTINUE
      T(2)=T(2)*4.D0/3.D0
      T(3)=0.D0
      DO 73 L=4,6,1
        DO 173 I=1,2,1
          T(3)=T(3) + (V(L,I)+A(L,I))*(X*(F(X,MF(L,I),MF(L,I))-C13)-
     &         Z*(F(Z,MF(L,I),MF(L,I))-C13)+
     &         2.D0*M(L,I)*(F(X,MF(L,I),MF(L,I))-
     &         F(Z,MF(L,I),MF(L,I))))-C38*M(L,I)*(F(X,MF(L,I),MF(L,I))-
     &         F(Z,MF(L,I),MF(L,I)))/(    C*S)
  173   CONTINUE
   73 CONTINUE
      T(3)=T(3)*4.D0
      T(4)=2.D0*(X-Z)/3.D0+(10.D0*X+20.D0*W)*F(X,M0,M0)-
     &     (10.D0*Z+20.D0*W)*F(Z,M0,M0)
      T(4)=T(4)*(-C)/(3.D0*S)
      T(5)=3.D0*W*(F(X,M0,M0)-F(Z,M0,M0))+(X-Z)/4D0*(1D0-DLOG(MH*MZ/W)-
     &     (H+Z)/(H-Z)*DLOG(MH/MZ))+(10.D0*Z-2.D0*H+X)/4.D0*F(X,MH,MZ)-
     &     (11.D0*Z-2D0*H)/4.D0*F(Z,MH,MZ)
      T(5)=T(5)/(3.D0*S*C)
      T(6)=(X-Z)/6.D0+(H-Z)*(H-Z)/4.D0*(F(X,MZ,MH)/X - F(Z,MZ,MH)/Z)
      T(6)=T(6)/(3.D0*S*C)
      T(7)=(X-Z)/6.D0+(2.0*W+X/4.D0)*F(X,M0,M0)-(2.D0*W+Z/4.D0)*
     &     F(Z,M0,M0)
      T(7)=T(7)*(C-S)*(C-S)/(3.D0*S*C)
      T(8)=0.D0
      DO 78 J=1,3,1
        T(8)=T(8)+(F(Z,MF(J,2),MF(J,2))-    C13)*(V(J,1)+V(J,2)+A(J,1)+
     &       A(J,2)) +(2.D0*(V(J,2)+A(J,2))- C38/(   C*S))*M(J,2)/Z*
     &       F(Z,MF(J,2),MF(J,2))
   78 CONTINUE
      T(8)=T(8)*(X-Z)*(C-S)/S*4.D0/3.D0
      T(9)=0.D0
      DO 79 L=4,6,1
        DO 719 I=1,2,1
          FZLI=F(Z,MF(L,I),MF(L,I))
          T(9)=T(9)+(FZLI -C13)                   *(V(L,I)+A(L,I)) +
     &         (2.D0*(V(L,I)+A(L,I))-C38/(    C*S))*M(L,I)/Z*FZLI
  719   CONTINUE
   79 CONTINUE
      T(9)=T(9)*4.D0*(X-Z)*(C-S)/S
      T(10)=(C/(3.D0*S)*(10.D0+20.D0*C)-1.D0/S-(C-S)*(C-S)/(12.D0*S*C)*
     &      (1.D0+8.D0*C))*F(Z,M0,M0)
      T(10)=T(10)*(X-Z)*(C-S)/S*(-1.D0)
      T(11)=(1.D0/(12.0*S*C)*(2.D0*H/Z-11.D0)-1.D0/(12.D0*S*C)*(H-Z)*
     &      (H-Z)/(Z*Z))*F(Z,MH,MZ)
      T(11)=T(11)*(X-Z)*(C-S)/S*(-1.D0)
      T(12)=(-1.D0)/(12.D0*S*C)*(2.D0*H/Z-11.D0)*(1.D0-(H+Z)/(H-Z)*
     &      DLOG(MH/MZ) - DLOG(MH*MZ/W))
      T(12)=T(12)*(X-Z)*(C-S)/S
      T(13)=(-1.D0)/(6.D0*S*C)*(H/Z*DLOG(H/W) + DLOG(Z/W)) -2.D0*C/
     &      (9.D0*S)+1.D0/(18.D0*S*C) + (C-S)*(C-S)/(18.D0*S*C)
      T(13)=T(13)*(X-Z)*(C-S)/S
      T(14)=0.D0
      DO 714 J=1,3,1
        T(14)=T(14)+(1.D0-M(J,2)/(2.D0*W))*(1.D0+F(W,0.D0,MF(J,2)))-C13-
     &        0.5D0*M(J,2)*M(J,2)/(W*W)*F(W,0.D0,MF(J,2))
  714 CONTINUE
      T(14)=T(14)/(3.D0*S)*(X-Z)*(C-S)/S*(-1.D0)
      T(15)=0.D0
      DO 715 L=4,6,1
        T(15)=T(15)+
     &        (1.D0-(M(L,1)+M(L,2))/(2.D0*W))*(1.D0-(M(L,1)+M(L,2))/
     &        (M(L,1)-M(L,2))*DLOG(MF(L,1)/MF(L,2))+
     &        F(W,MF(L,1),MF(L,2)))-
     &        C13 -.5D0*(M(L,1)-M(L,2))*(M(L,1)-M(L,2))/(W*W)*
     &        F(W,MF(L,1),MF(L,2))
  715 CONTINUE
      T(15)=T(15)/S*(X-Z)*(C-S)/S*(-1.D0)
      T(16)=(5.D0*S/(3.D0*C)-7.D0/3.D0-8.D0*C/S+1.D0/(12.D0*S)*Z/W*
     &      (Z/W-4.D0))*F(W,MZ,M0)
      T(16)=T(16)*(X-Z)*(C-S)/S*(-1.D0)
      T(17)=(1.D0/S+1.D0/(12.D0*S)*H/W*(H/W-4.D0))*F(W,MH,M0)-
     &      4.D0*F(W,0.D0,M0)
      T(17)=T(17)*(X-Z)*(C-S)/S*(-1.D0)
      T(18)=(1.D0+8.D0*C/S-S/C+1.D0/(4.D0*S))*Z/(Z-W)*DLOG(Z/W)
      T(18)=T(18)*(X-Z)*(C-S)/S*(-1.D0)
      T(19)=(-3.D0)/(4.D0*S)*H/(H-W)*DLOG(H/W) +
     &      (17.D0/18.D0-(Z+H)/(6.0*W))/S+S/C-2.D0*C/(9.D0*S)-8.D0*C/S-
     &      44.D0/9.D0-7.D0/3.D0
      T(19)=T(19)*(X-Z)*(C-S)/S*(-1.D0)
      T(20)=0.D0
      DO 720 L=4,6,1
        T(20)=T(20)+(4.D0/3.D0+(C-S)/C*(M(L,1)-M(L,2))/(Z-W))*
     &        DLOG(M(L,1)/M(L,2))
  720 CONTINUE
      T(20)=(T(20)/(4.D0*S)+2.D0/3.D0)*(X-Z)
      RESZ=0.D0
      DO 700 I=1,20,1
        RESZ=RESZ+T(I)
  700 CONTINUE
      RESZ=RESZ*ALPHA
      RETURN
      END
*DECK,IMSZ
      FUNCTION IMSZ(X)
C  IMAGINARY PART OF THE Z SELF ENERGY
      IMPLICIT REAL*8(A-Z)
      INTEGER I,L
      DIMENSION MF(6,2),VF(6,2),M(6,2),V(6,2),A(6,2),T(25)
      COMMON /BOSEW/MZ,M0,MH/FERMI/MF,VF,M,V,A
     1       /ALF/AL,ALPHA,ALFA
     2       /COUP/SW,CW,VL,A1,VU,AU,VD,AD
      PI=3.1415926536D0
      Z=MZ*MZ
      W=M0*M0
      H=MH*MH
      S=SW
      C=CW
      DO 111 L=1,20,1
        T(L)=0.D0
  111 CONTINUE
      T(1)=0.D0
      DO 1 I=1,3,1
        T(1)=T(1) + (V(I,1)+A(I,1))*X*G(X,MF(I,2),MF(I,2))
    1 CONTINUE
      T(1)=T(1)*4.D0/3.D0
      T(2)=0.D0
      DO 2 I=1,3,1
        T(2)=T(2)+(V(I,2)+A(I,2))*(X+2.D0*M(I,2))*G(X,MF(I,2),MF(I,2))-
     &       3.D0*M(I,2)/(8.D0*C*S)*G(X,MF(I,2),MF(I,2))
    2 CONTINUE
      T(2)=T(2)*4.D0/3.D0
      T(3)=0.D0
      DO 311 L=4,6,1
        DO 113 I=1,2,1
          T(3)=T(3) +
     &         (V(L,I)+A(L,I))*(X+2.D0*M(L,I))*G(X,MF(L,I),MF(L,I))-
     &         3.D0*M(L,I)/(8.D0*C*S)*G(X,MF(L,I),MF(L,I))
  113   CONTINUE
  311 CONTINUE
      T(3)=T(3)*4.D0
      T(4)=(10.D0*X+20.D0*W)*(-C)*C+3.D0*W+(2.D0*W+X/4.D0)*(C-S)*(C-S)
      T(4)=T(4)*G(X,M0,M0)/(3.D0*S*C)
      T(5)=10.D0*Z-2.D0*H+X + (H-Z)*(H-Z)/X
      T(5)=T(5)*G(X,MH,MZ)/(12.D0*S*C)
      IMSZ=0.D0
      DO 100 I=1,5,1
        IMSZ=IMSZ+T(I)
  100 CONTINUE
      IMSZ=IMSZ*ALPHA
      RETURN
      END
*DECK,G
      FUNCTION G(Y,A,B)
C  IMAGINARY PART OF THE COMPLEX FUNCTION F
      IMPLICIT REAL*8(A-Z)
      PI=3.1415927
C     G(Y,M1,M2) = IM F(Y,M1,M2)
      P=(A+B)*(A+B)
      Q=(A-B)*(A-B)
      IF ( Y .LE. P ) GO TO 10
      G=DSQRT(Y-P)*DSQRT(Y-Q)*PI/Y
      GO TO 20
   10 CONTINUE
      G=0.0
   20 CONTINUE
      RETURN
      END
*DECK,RESGZ
      FUNCTION RESGZ(X)
C  REAL PART OF THE PHOTON-Z MIXING ENERGY
      IMPLICIT REAL*8(A-Z)
      INTEGER I,J,L
      DIMENSION M(6,2),V(6,2),A(6,2),VF(6,2),MF(6,2),T(20)
      COMMON /BOSEW/MZ,M0,MH/FERMI/MF,VF,M,V,A
     1       /ALF/AL,ALPHA,ALFA
     2       /COUP/SW,CW,VL,A1,VU,AU,VD,AD
C  MF IS THE ARRAY OF FERMION MASSES, M THAT OF F MASS SQUARED
C  I = FAMILY INDEX (1...6); J = 1,2 DENOTES UP AND DOWN MEMBERS
C  VF IS THE ARRAY OF CORRESPONDING VECTOR COUPLINGS
C  V,A ARE THE ARRAYS OF THE SQUARES OF V AND A COUPLINGS
      PI=3.1415926536D0
      GF=1.16634D-5
      Z=MZ*MZ
      W=M0*M0
      H=MH*MH
      C=CW
      S=SW
      CS=DSQRT(C*S)
      C13=1.D0/3.D0
      C38=3.D0/8.D0
      DO 333 J=1,20,1
        T(J)=0.D0
  333 CONTINUE
      T(1)=0.D0
      DO 71 I=1,3,1
        T(1)=T(1) + VF(I,2)*(X*(F(X,MF(I,2),MF(I,2))-C13)+2.D0*M(I,2)*
     &       F(X,MF(I,2),MF(I,2)))
   71 CONTINUE
      T(1)=T(1)*4.*C13
      T(2)=0.D0
      DO 72 I=4,6,1
        T(2)=T(2)+VF(I,1)*(X*(F(X,MF(I,1),MF(I,1))-C13)+2.D0*M(I,1)*
     &       F(X,MF(I,1),MF(I,1)))
   72 CONTINUE
      T(2)=T(2)*(-8.D0)/3.D0
      T(3)=0.D0
      DO 73 I=4,6,1
        T(3)=T(3)+VF(I,2)*(X*(F(X,MF(I,2),MF(I,2))-C13)+2.D0*M(I,2)*
     &       F(X,MF(I,2),MF(I,2)))
   73 CONTINUE
      T(3)=T(3)*4.D0*C13
      T(4)=(X*(3.D0*C+1.D0/6.D0)+W*(4.D0*C+4.D0*C13))*F(X,M0,M0)/CS
      T(5)=X/(9.D0*CS)
      T(8)=0.D0
      DO 78 J=1,3,1
        T(8)=T(8)+(F(Z,MF(J,2),MF(J,2))-    C13)*(V(J,1)+V(J,2)+A(J,1)+
     &       A(J,2)) +(2.D0*(V(J,2)+A(J,2))-3.D0/(8.D0*C*S))*M(J,2)/Z*
     &       F(Z,MF(J,2),MF(J,2))
   78 CONTINUE
      T(8)=T(8)*(-X)*CS/S*4.D0*C13
      T(9)=0.D0
      DO 79 L=4,6,1
        DO 719 I=1,2,1
          FZLI=F(Z,MF(L,I),MF(L,I))
          T(9)=T(9)+( FZLI -C13)                  *(V(L,I)+A(L,I)) +
     &         (2.D0*(V(L,I)+A(L,I))-C38/(    C*S))*M(L,I)/Z*FZLI
  719   CONTINUE
   79 CONTINUE
      T(9)=T(9)*4.D0*(-X)*CS/S
      T(10)=(C/(3.D0*S)*(10.D0+20.D0*C)-1.D0/S-(C-S)**2/(12.D0*S*C)*
     &      (1.D0+8.D0*C))*F(Z,M0,M0)
      T(10)=T(10)*X*CS/S
      T(11)=(1.D0/(12.D0*S*C)*(2.D0*H/Z-11.D0)-1.D0/(12.D0*S*C)*(H-Z)*
     &      (H-Z)/(Z*Z))*F(Z,MH,MZ)
      T(11)=T(11)*X*CS/S
      T(12)=(-1.D0)/(12.D0*S*C)*(2.D0*H/Z-11.D0)*(1.D0-(H+Z)/(H-Z)*
     &      DLOG(MH/MZ) - DLOG(MH*MZ/W))
      T(12)=T(12)*(-X)*CS/S
      T(13)=(-1.D0)/(6.D0*S*C)*(H/Z*DLOG(H/W) + DLOG(Z/W)) -2.D0*C/
     &      (9.D0*S)+1.D0/(18.D0*S*C) + (C-S)*(C-S)/(18.D0*S*C)
      T(13)=T(13)*(-X)*CS/S
      T(14)=0.D0
      DO 714 J=1,3,1
        T(14)=T(14)+(1.D0-M(J,2)/(2.D0*W))*(1.D0+F(W,0.D0,MF(J,2)))-C13-
     &        0.5D0*M(J,2)*M(J,2)/(W*W)*F(W,0.D0,MF(J,2))
  714 CONTINUE
      T(14)=T(14)/(3.D0*S)*(-X)*CS/S*(-1.D0)
      T(15)=0.D0
      DO 715 L=4,6,1
        T(15)=T(15)+
     &        (1.D0-(M(L,1)+M(L,2))/(2.D0*W))*(1.D0-(M(L,1)+M(L,2))/
     &        (M(L,1)-M(L,2))*DLOG(MF(L,1)/MF(L,2))+
     &        F(W,MF(L,1),MF(L,2)))-
     &        C13   -.5D0*(M(L,1)-M(L,2))*(M(L,1)-M(L,2))/(W*W)*
     &        F(W,MF(L,1),MF(L,2))
  715 CONTINUE
      T(15)=T(15)/S*(-X)*CS/S*(-1.D0)
      T(16)=(5.D0*S/(3.D0*C)-7.D0*C13-8.D0*C/S+1.D0/(12.D0*S)*Z/W*
     &      (Z/W-4.D0))*F(W,MZ,M0)
      T(16)=T(16)*(-X)*CS/S*(-1.D0)
      T(17)=(1.D0/S+1.D0/(12.D0*S)*H/W*(H/W-4.D0))*F(W,MH,M0)-
     &      4.D0*F(W,0.D0,M0)
      T(17)=T(17)*(-X)*CS/S*(-1.D0)
      T(18)=(1.D0+8.D0*C/S-S/C+1.D0/(4.D0*S))*Z/(Z-W)*DLOG(Z/W)
      T(18)=T(18)*(-X)*CS/S*(-1.D0)
      T(19)=(-3.D0)/(4.D0*S)*H/(H-W)*DLOG(H/W) +
     &      (17.D0/18.D0-(Z+H)/(6.D0*W))/S+S/C-2.D0*C/(9.D0*S)-8.D0*C/S-
     &      44.D0/9.D0-7.D0/3.D0
      T(19)=T(19)*(-X)*CS/S*(-1.D0)
      T(20)=0.D0
      DO 720 L=4,6,1
        T(20)=T(20)+(2.D0/3.D0+(M(L,1)-M(L,2))/(Z-W))*
     &        DLOG(M(L,1)/M(L,2))
CCC   T(20)=T(20)+2.D0/3.D0*DLOG(M(L,1)/M(L,2))
  720 CONTINUE
      T(20)=T(20)*(-X)/(4.D0*CS)
      RESGZ=0.D0
      DO 70 I=1,20,1
        RESGZ=RESGZ + T(I)
   70 CONTINUE
      RESGZ=RESGZ*ALPHA
      RETURN
      END
*DECK,IMSGZ
      FUNCTION IMSGZ(X)
C  IMAGINARY PART OF PHOTON-Z MIXING ENERGY
      IMPLICIT REAL*8(A-Z)
      INTEGER I
      DIMENSION  T(25), VF(6,2),M(6,2),V(6,2),A(6,2),MF(6,2)
      COMMON /BOSEW/MZ,M0,MH/FERMI/MF,VF,M,V,A
     1       /ALF/AL,ALPHA,ALFA
     2       /COUP/SW,CW,VL,A1,VU,AU,VD,AD
      C=CW
      S=SW
      Z=MZ*MZ
      W=M0*M0
      H=MH*MH
      PI=3.1415926536D0
      CS=DSQRT(C*S)
      C13=1.D0/3.D0
      T(1)=0.D0
      DO 1 I=1,3,1
        T(1)=T(1) + VF(I,2)*(X*G(X,MF(I,2),MF(I,2))+2.D0*M(I,2)*
     &       G(X,MF(I,2),MF(I,2)))
    1 CONTINUE
      T(1)=T(1)*4.D0*C13
      T(2)=0.D0
      DO 2 I=4,6,1
        T(2)=T(2) + VF(I,1)*(X*G(X,MF(I,1),MF(I,1))+2.D0*M(I,1)*
     &       G(X,MF(I,1),MF(I,1)))
    2 CONTINUE
      T(2)=T(2)*(-8.D0)/3.D0
      T(3)=0.D0
      DO 3 I=4,6,1
        T(3)=T(3) + VF(I,2)*(X*G(X,MF(I,2),MF(I,2))+2.D0*M(I,2)*
     &       G(X,MF(I,2),MF(I,2)))
    3 CONTINUE
      T(3)=T(3)*4.D0*C13
      T(4)=(X*(3.D0*C+1.D0/6.D0)+W*(4.D0*C+4.D0*C13))*G(X,M0,M0)/CS
      IMSGZ=0.D0
      DO 30 I=1,4,1
        IMSGZ=IMSGZ + T(I)
   30 CONTINUE
      IMSGZ=IMSGZ*ALPHA
      RETURN
      END
*DECK,INFRA
      SUBROUTINE INFRA(DEL,S,T,CIR,CIR1,CIR2)
      IMPLICIT REAL*8(A-Z)
CBB      COMPLEX*16 CIR1,M2,CDABS,CDLOG,D1,D2,DEL2,DEL3  ! CDABS is REAL*8
      COMPLEX*16 CIR1,M2,CDLOG,D1,D2,DEL2,DEL3
      COMMON /BOSEW/MZ,MW,MH/LEPT/ME,MMU,MTAU/WIDTH/GZ
     1       /ALF/AL,ALPHA,ALFA
      XM2=MZ*MZ
      YM2=-MZ*GZ
      M2=DCMPLX(XM2,YM2)
      W=DSQRT(S)
      E=W/2.D0
      U=-S-T
      BE=DLOG(W/ME)*2-1.D0
      BINT=DLOG(T/U)
      DEL1=DLOG(DEL)
      D1=(S-M2)/(S-M2-S*DEL)
      D2=S/(M2-S+S*DEL)
      AD1=CDABS(D1)
      AD2=CDABS(D2)
      CIR=4.D0*AL*(BE+BINT)*DEL1
      DEL2=CDLOG(DEL*DEL*D1)
      DEL3=CDLOG(DEL*DEL*D2)
      CIR1=2.D0*AL*(BE*DEL2+BINT*DEL3)
      DEL4=DLOG(DEL*DEL*AD1)
      DEL5=DLOG(DEL*AD2)
      PHI0=DATAN((MZ**2-S)/MZ/GZ)
      PHI= DATAN((MZ**2-S+DEL*S)/MZ/GZ)
      CIR2=2.D0*AL*(BE*DEL4+2.D0*BINT*DEL5+BE*(S-MZ**2)/MZ/GZ
     1                                        *(PHI-PHI0))
      RETURN
      END
*DECK,SPENCE
      FUNCTION SPENCE(XX)
      IMPLICIT REAL*8(A-Z)
      INTEGER N
      COMPLEX*16 XX,X,Z,D,P,SPENCE,CDLOG
      DIMENSION A(19)
      PI=3.1415926536D0
      X=XX
      XR=DREAL(X)
      XI=DIMAG(X)
      IF(XR.NE.1.) GOTO 111
      IF(XI.EQ.0.) GOTO 20
  111 CONTINUE
C    PROJEKTION IN DEN KONVERGENZKREIS
      VOR=1.D0
      P=DCMPLX(0.D0,0.D0)
      R=DREAL(X)
      IF (R .LE. 0.5D0) GOTO 1
      P=PI*PI/6.D0- CDLOG(X)*CDLOG(1.D0-X)
      VOR=-1.D0
      X=1.D0-X
    1 CONTINUE
      B=CDABS(X)
      IF (B .LT. 1.D0) GOTO 2
      P=P - (PI*PI/6.D0+ CDLOG(-X)*CDLOG(-X)/2.D0)*VOR
      VOR=VOR*(-1.D0)
      X=1.D0/X
    2 CONTINUE
C    BERECHNUNG DER SPENCE-FUNCTION
      A(1)=1.D0
      A(2)=-0.5D0
      A(3)=1.D0/6.D0
      A(5)=-1.D0/30.D0
      A(7)=1.D0/42.D0
      A(9)=-1.D0/30.D0
      A(11)=5.D0/66.D0
      A(13)=-691.D0/2730.D0
      A(15)=7.D0/6.D0
      A(17)=-3617.D0/510.D0
      A(19)=43867.D0/798.D0
      DO 5 N=2,9,1
        A(2*N)=0.D0
    5 CONTINUE
      Z=(-1.D0)*CDLOG(1.D0-X)
      D=DCMPLX(A(19),0.D0)
      DO 10 N=1,18,1
        D=D*Z/(20.D0-N) + A(19-N)
   10 CONTINUE
      D=D*Z
      SPENCE=D*VOR + P
      GOTO 30
   20 CONTINUE
      SPENCE=PI*PI/6.D0
   30 CONTINUE
      RETURN
      END
*DECK,FGAM
      SUBROUTINE FGAM(S,FGVS,FGAS)
C  FORM FACTOR OF THE PHOTON
C  FGVS: VECTOR,  FGAS: AXIALVECTOR,  S = Q**2
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16 FGVS,FGAS,LAM2,L2,LAM3,G1
      COMMON /BOSEW/MZ,MW,MH/COUP/SW,CW,V,A,VU,AU,VD,AD
     1       /ALF/AL,ALPHA,ALFA
C  WEAK CORRECTIONS TO PHOTON-LEPTON VERTEX
      W=V*V+A*A
      U=2.D0*V*A
      G1=.75D0/SW*LAM3(S,MW)
      L2=LAM2(S,MZ)
      FGVS=ALPHA*(W*L2+G1)
      FGAS=ALPHA*(U*L2+G1)
      RETURN
      END
*DECK,FZ
      SUBROUTINE FZ(S,FZVS,FZAS)
C   FORMFACTOR OF THE Z BOSON
C   FZVS: VECTOR,  FZAS: AXIALVECTOR,  S = Q**2
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16 FZVS,FZAS,LAM2,L2Z,L2W,LAM3,L3W
      COMMON /BOSEW/MZ,MW,MH/COUP/SW,CW,V,A,VU,AU,VD,AD
     1       /ALF/AL,ALPHA,ALFA
C  WEAK CORRECTIONS TO Z-LEPTON VERTEX
      L2Z=LAM2(S,MZ)
      SW1=DSQRT(SW)
      CW1=DSQRT(CW)
      L2W=LAM2(S,MW)/(8.D0*SW1*CW1*SW)
      L3W=LAM3(S,MW)*.75D0*CW1/(SW1*SW)
      V2=V*V
      A2=A*A
      W3=V2+3.D0*A2
      U3=3.D0*V2+A2
      FZVS=ALPHA*(V*W3*L2Z+L2W-L3W)
      FZAS=ALPHA*(A*U3*L2Z+L2W-L3W)
      RETURN
      END
*DECK,LAM2
      FUNCTION LAM2(S,M)
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16 LAM2,SPENCE,      X1,X2,R3
      X=M*M/S
      R1=-3.5D0-2.D0*X-(2.D0*X+3.D0)*DLOG(DABS(X))
      PI=3.1415926536D0
      IF(X.LT.0.D0) GOTO 10
      L1=DLOG(1.D0+1.D0/X)
      W1=(1.D0+X)**2
      X1=DCMPLX(-1.D0/X,0.D0)
      R3=2.D0*W1*(DLOG(X)*L1-SPENCE(X1))
      R2=DREAL(R3)
      IM=3.D0+2.D0*X-2.D0*W1*L1
      RLAM=R1+R2
      ILAM=-IM*PI
      LAM2=DCMPLX(RLAM,ILAM)
      GOTO 20
   10 W1=(1.D0+X)**2
      RX2=1.D0+1.D0/X
      X2=DCMPLX(RX2,0.D0)
      PI2=PI*PI
      R3=2.D0*W1*(SPENCE(X2)-PI2/6.D0)
      R2=DREAL(R3)
      LAM2=DCMPLX(R1+R2,0.D0)
   20 RETURN
      END
*DECK,LAM3
      FUNCTION LAM3(S,M)
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16 CPI,LAM3
      X=M*M/S
C  VALID ALSO FOR S GREATER THAN 4*M**2
      IF(X.GT.0.D0) GOTO 10
      SQ=DSQRT(1.D0-4.D0*X)
      LN=DLOG((SQ+1.D0)/(SQ-1.D0))
      LN2=LN*LN
      LA3 =5.D0/6.D0-2.D0/3.D0*X+(2.D0*X+1.D0)/3.D0*SQ*LN
     1    +2.D0/3.D0*X*(X+2.D0)*LN2
      LAM3=DCMPLX(LA3,0.D0)
      GOTO 20
   10 DISC=4.D0*X-1.D0
      IF (DISC.LT.0.D0) GOTO 11
      SQ=DSQRT(DISC)
      A=DATAN(1.D0/SQ)
      A2=A*A
      LA3 =5.D0/6.D0-2.D0/3.D0*X+2.D0/3.D0*(2.D0*X+1.D0)*SQ*A
     1     -8.D0/3.D0*X*(X+2.D0)*A2
      LAM3=DCMPLX(LA3,0.D0)
      GOTO 20
   11 DISC=-DISC
      SQ=DSQRT(DISC)
      PI=3.1415926536D0
      CPI=DCMPLX(0.D0,PI)
      LN=DLOG((1.D0+SQ)/(1.D0-SQ))
      LAM3=5.D0/6.D0-2.D0*X/3.D0+(2.D0*X+1.D0)/3.D0*SQ*LN
     1    +2.D0/3.D0*X*(X+2.D0)*(LN*LN-PI*PI)
     2    -CPI*((2.D0*X+1.D0)/3.D0*SQ+2.D0*LN)
   20 RETURN
      END
*DECK,BOX
      SUBROUTINE   BOX(S,T,V1ZZS,V2ZZS,A1ZZS,A2ZZS,
     1                     V1ZZT,V2ZZT,A1ZZT,A2ZZT,
     2                     V1WS,V2WS,V1WT,V2WT)
C  HEAVY BOX DIAGRAMS
C  1:REAL PARTS,  2: IMAGINARY PARTS
      IMPLICIT REAL*8(A-Z)
      COMPLEX*16      IST,ISU,ITS,ITU,I5ST,I5SU,I5TS,I5TU,CPI,
     1           VS,AS,VT,AT,VWS,VWT
      COMMON /BOSEW/MZ,MW,MH/WIDTH/GZ
      PI2=2.*3.1415926536D0
      CPI=DCMPLX(0.D0,PI2)
      CALL XINT(S,T,MZ,IST,I5ST)
      U=-S-T
      CALL XINT(S,U,MZ,ISU,I5SU)
      CALL XINT(T,S,MZ,ITS,I5TS)
      CALL XINT(T,U,MZ,ITU,I5TU)
C
      VS=IST-ISU
      AS=I5ST+I5SU
      VT=ITS-ITU
      AT=I5TS+I5TU
C
      CALL XINT(S,T,MW,IST,I5ST)
      VWS=IST+I5ST
      CALL XINT(T,S,MW,ITS,I5TS)
      VWT=ITS+I5TS
C
      V1ZZS=DREAL(VS)
      A1ZZS=DREAL(AS)
      V1ZZT=DREAL(VT)
      A1ZZT=DREAL(AT)
      V2ZZS=DIMAG(VS)/PI2
      A2ZZS=DIMAG(AS)/PI2
      V2ZZT=DIMAG(VT)/PI2
      A2ZZT=DIMAG(AT)/PI2
      V1WS=DREAL(VWS)
      V1WT=DREAL(VWT)
      V2WS=DIMAG(VWS)/PI2
      V2WT=DIMAG(VWT)/PI2
      RETURN
      END
*DECK,SETUPM
      SUBROUTINE SETUPM(AMZ,AGZ,GV,GA)
C
C SETUP FOR CALCULATION OF THE RADIATIVE BHABHA AMPLITUDE
C SIN2W = SIN**2 OF THE WEAK MIXING ANGLE
C AMZ = MASS OF THE Z0 IN GEV
C AGZ = WIDTH OF THE Z0 IN GEV
      IMPLICIT REAL*8(A-H,O-U),COMPLEX*16(V-Z)
      COMMON / AMPCOM / E3,  EG1,  EG2,  EG3,
     .                  AM2Z,  AMGZ
      COMMON / UNICOM / IIN,IUT
      COMMON / DBCOM / IFLDB
C
      CALL OUTCRY('SETUPM')
C
C CALCULATE THE VARIOUS CONSTANTS
      ROOT8=DSQRT(8.D0)
      E=DSQRT(4.*3.1415926536D0/137.036D0)
      AM2Z=AMZ**2
      AMGZ=AMZ*AGZ
      E3=E**3*ROOT8
      EG1=E*(GV-GA)**2*ROOT8
      EG2=E*(GV**2-GA**2)*ROOT8
      EG3=E*(GV+GA)**2*ROOT8
C
C PRINT THE RESULTS SO FAR
*                          Under control of the flag IFLDB
      IF(IFLDB.EQ.0) GO TO 777
      WRITE(IUT,1) AMZ,   AGZ,     E,    GA,
     .            GV,  AM2Z,  AMGZ,    E3,
     .           EG1,   EG2,   EG3
    1 FORMAT(' SETUPM :',4D14.6)
  777 CONTINUE
      RETURN
      END
*DECK,AMPLIT
      SUBROUTINE AMPLIT(PP,PM,QP,QM,QK,RESULT)
C
C CALCULATION OF THE AMPLITUDES FOR MASSLESS MOMENTA
C PP = INCOMING POSITRON
C PM = INCOMING ELECTRON
C QP = OUTGOING POSITRON
C QM = OUTGOING ELECTRON
C QK = BREMSSTRAHLUNG PHOTON
      IMPLICIT REAL*8(A-H,O-U),COMPLEX*16(V-Z)
      DIMENSION PP(4),PM(4),QP(4),QM(4),QK(4)
      DIMENSION A(5),ZC(5),ZM(12)
      COMMON / AMPCOM / E3,  EG1,  EG2,  EG3,
     .                  AM2Z,  AMGZ
      COMMON / AMPCM2 / PPK,  PMK,  QPK,  QMK
      COMMON / RWMASS / X51,X25,X54,X35
C
C THE PHOTON-CUM-Z0 PROPAGATORS
      Z1(S)=E3/S+EG1/DCMPLX(S-AM2Z,AMGZ)
      Z2(S)=E3/S+EG2/DCMPLX(S-AM2Z,AMGZ)
      Z3(S)=E3/S+EG3/DCMPLX(S-AM2Z,AMGZ)
C
C THE DEFINITION OF THE SPINOR PRODUCT
      ZS(I,J)=ZC(I)*A(J)-ZC(J)*A(I)
C
      A(1)=DSQRT(PP(4)-PP(1))
      A(2)=DSQRT(PM(4)-PM(1))
      A(3)=DSQRT(QP(4)-QP(1))
      A(4)=DSQRT(QM(4)-QM(1))
      A(5)=DSQRT(QK(4)-QK(1))
      ZC(1)=DCMPLX(PP(2),PP(3))/A(1)
      ZC(2)=DCMPLX(PM(2),PM(3))/A(2)
      ZC(3)=DCMPLX(QP(2),QP(3))/A(3)
      ZC(4)=DCMPLX(QM(2),QM(3))/A(4)
      ZC(5)=DCMPLX(QK(2),QK(3))/A(5)
C
C DEFINE THE NECESSARY SPINOR PRODUCTS
      X51=ZS(5,1)
      X25=ZS(2,5)
      X54=ZS(5,4)
      X35=ZS(3,5)
      X13=ZS(1,3)
      X12=ZS(1,2)
      X43=ZS(4,3)
      X42=ZS(4,2)
      X14=ZS(1,4)
      X32=ZS(3,2)
C
C COMPUTE THE FERMION PROPAGATORS IN THE MASSLESS LIMIT
      PPK=.5D0*CDABS(X51)**2
      PMK=.5D0*CDABS(X25)**2
      QPK=.5D0*CDABS(X35)**2
      QMK=.5D0*CDABS(X54)**2
C
C DEFINE THE COMPONENTS OF THE INFRARED FACTORS
      V1=-DCONJG(X42)/(X51*X35)
      V2=-DCONJG(X13)/(X54*X25)
      V3=-DCONJG(X43)/(X51*X25)
      V4=-DCONJG(X12)/(X54*X35)
      V5=-DCONJG(V1)
      V6=-DCONJG(V2)
      V7=-DCONJG(V3)
      V8=-DCONJG(V4)
C
C DEFINE THE WELL-KNOWN INVARIANT MASSES
      S0= CDABS(X12)**2
      S1= CDABS(X43)**2
      T0=-CDABS(X13)**2
      T1=-CDABS(X42)**2
      U0=-CDABS(X14)**2
      U1=-CDABS(X32)**2
C
C DEFINE THE BOSON PROPAGATORS
      Z1T1=Z1(T1)
      Z2T1=Z2(T1)
      Z3T1=Z3(T1)
      Z1T0=Z1(T0)
      Z2T0=Z2(T0)
      Z3T0=Z3(T0)
      Z1S1=Z1(S1)
      Z2S1=Z2(S1)
      Z3S1=Z3(S1)
      Z1S0=Z1(S0)
      Z2S0=Z2(S0)
      Z3S0=Z3(S0)
C
C DEFINE THE TWELVE HELICITY AMPLITUDES
      ZM( 1)=U0*( Z1T1*V1 + Z1T0*V2 - Z1S1*V3 - Z1S0*V4 )
      ZM( 2)=T0*(                   - Z2S1*V3 - Z2S0*V4 )
      ZM( 3)=S0*( Z2T1*V1 + Z2T0*V2                     )
      ZM( 4)=S1*ZM(3)/S0
      ZM( 5)=T1*ZM(2)/T0
      ZM( 6)=U1*( Z3T1*V1 + Z3T0*V2 - Z3S1*V3 - Z3S0*V4 )
      ZM( 7)=U0*( Z3T1*V5 + Z3T0*V6 - Z3S1*V7 - Z3S0*V8 )
      ZM( 8)=T0*(                   - Z2S1*V7 - Z2S0*V8 )
      ZM( 9)=S0*( Z2T1*V5 + Z2T0*V6                     )
      ZM(10)=S1*ZM(9)/S0
      ZM(11)=T1*ZM(8)/T0
      ZM(12)=U1*( Z1T1*V5 + Z1T0*V6 - Z1S1*V7 - Z1S0*V8 )
C
C DEFINE THE RESULTING CROSS SECTION
      RESULT=0.
      DO 102 I=1,12
        RESULT=RESULT+CDABS(ZM(I))**2
  102 CONTINUE
      RESULT=RESULT/4.
C
      RETURN
      END
*DECK,SETUPW
      SUBROUTINE SETUPW(E,XMZ,XGZ,GV,GA)
C
C SETUP FOR CALCULATING THE APPROXIMATIONS TO THE RADIATIVE
C CROSS SECTION.
C E = BEAM ENERGY IN GEV;
C XMZ = Z0 MASS IN GEV;
C XGZ = Z0 TOTAL WIDTH IN GEV;
C GV(GA) = (AXIAL-) VECTOR COUPLING BETWEEN ELECTRON AND Z0.
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / WGTCOM / EBEAM,S,S2,F1,F2,F3,F4,XM2,XMG2
      COMMON / UNICOM / IIN,IUT
      COMMON / DBCOM / IFLDB
C
C THE BREIT-WIGNER PROPAGATOR
      B(X)=(X-XM2)**2+XMG2
C
      CALL OUTCRY('SETUPW')
C
C CONSTANTS
      EBEAM=E
      S=4.*E**2
      S2=S**2
      E2=4.*3.1415926536D0/137.036D0
      G=GV**4+6.*GV**2*GA**2+GA**4
      XM2=XMZ**2
      XMG2=XMZ**2*XGZ**2
C
C THE CONSTANT FACTORS OF THE FOUR APPROXIMANTS
      F1=E2**3
      F2=E2*G*S/B(S)+2.*E2**2*(GV**2+GA**2)*(S-XM2)/B(S)+E2**3/S
      F3=E2*G
      F4=E2**3*2.
C
C PRINT THE RESULTS SO FAR
*                          Under control of the flag IFLDB
      IF(IFLDB.EQ.0) GO TO 777
      WRITE(IUT,1)   E,   XMZ,   XGZ,    GV,
     .            GA,     S,    S2,    E2,
     .             G,   XM2,  XMG2,    F1,
     .            F2,    F3,    F4
    1 FORMAT(' SETUPW :',4D14.6)
  777 CONTINUE
      RETURN
      END
*DECK,APROXS
      SUBROUTINE APROXS(QP,QM,QK,A1,A2,A3,A4)
C
C CALCULATE THE FOUR APPROXIMATE CROSS SECTIONS
C QP = MOMENTUM OF THE OUTGOING POSITRON
C QM = MOMENTUM OF THE OUTGOING ELECTRON
C QK = MOMENTUM OF THE BREMSSTRAHLUNG PHOTON
C THE INCOMING POSITRON IS ALONG THE POSITIVE Z-AXIS.
C A1 : INITIAL-STATE RADIATION IN THE PHOTON S-CHANNEL
C A2 : FINAL-STATE RADIATION IN THE Z0 S-CHANNEL
C A3 : INITIAL-STATE RADIATION IN THE Z0 S-CHANNEL
C A4 : RADIATION IN THE PHOTON T-CHANNEL
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION QP(4),QM(4),QK(4)
      COMMON / WGTCOM / E,S,S2,F1,F2,F3,F4,XM2,XMG2
      COMMON / AMPCM2 / PPK,  PMK,  QPK,  QMK
C
C THE BREIT-WIGNER PROPAGATOR
      B(X)=(X-XM2)**2+XMG2
C
      S1=S*(1.-QK(4)/E)
      T =-2.*E*(QP(4)-QP(3))
      T1=-2.*E*(QM(4)+QM(3))
      SFAC=S2+S1**2
      A1=F1*SFAC/(S1*PPK*PMK)
      A2=F2*SFAC/(QPK*QMK)
      A3=F3*SFAC*S1/(B(S1)*PPK*PMK)
      A4=F4*SFAC*(-1./(T1*PPK*QPK)-1./(T*PMK*QMK))
      RETURN
      END
*DECK,BORN
      SUBROUTINE BORN(EB,XMZ,XGZ,GV,GA,THMIN,THMAX,CROSS)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 ZS,SP,SM,SX
      COMMON / UNICOM / IIN,IUT
      COMMON / DBCOM / IFLDB
      COMMON /OUTCRB/ ICRBN
C
      F1(V)=-4.D0/V-4.D0*DLOG(V)+V
      F2(C)=S**2/4.D0*(C+C**2+C**3/3.D0)
      F3(W)=-(2.D0+U)**2/W-2.D0*(2.D0+U)*DLOG(W)+W
      F4(V)=-S/2.D0*(4.D0*DLOG(V)-4.D0*V+V**2/2.D0)
      F5(W)=-S/2.D0*((2.D0+U)**2*DLOG(W)-2.D0*(2.D0+U)*W+W**2/2.D0)
C F6=(F5-F4)/MZ**2
      F7(C)=S**2/4.D0*(C-C**2+C**3/3.D0)
      F8(V)=4.D0/S**2*(-1.D0/V)
      F9(W)=4.D0/S**2*(-1.D0/W)
      F10(C)=4.D0/U/S**2*DLOG((1.D0+U-C)/(1.D0-C))
C
      PI=4.D0*DATAN(1.D0)
      C0=DCOS(PI*THMAX/180.D0)
      C1=DCOS(PI*THMIN/180.D0)
      V0=1.D0-C1
      V1=1.D0-C0
      S=4.D0*EB**2
      U=XMZ**2/(S/2.D0)
      W0=V0+U
      W1=V1+U
C
      H1=F1(V1)-F1(V0)
      H2=F2(C1)-F2(C0)
      H3=F3(W1)-F3(W0)
      H4=F4(V1)-F4(V0)
      H5=F5(W1)-F5(W0)
      H6=1.D0/XMZ**2*(H5-H4)
      H7=F7(C1)-F7(C0)
      H8=F8(V1)-F8(V0)
      H9=F9(W1)-F9(W0)
      H10=F10(C1)-F10(C0)
C
      ZS=DCMPLX(S-XMZ**2,XMZ*XGZ)
C
      E=DSQRT(4.D0*PI/137.036D0)
      SP=E**2/S+(GV+GA)**2/ZS
      SM=E**2/S+(GV-GA)**2/ZS
      SX=E**2/S+(GV**2-GA**2)/ZS
C
      B1=2.D0*E**4
      B2=CDABS(SP)**2+CDABS(SM)**2
      B3=(GV+GA)**4+(GV-GA)**4
      B4=DREAL(2.D0*E**2*(SP+SM))
      B5=DREAL(2.D0*(GV+GA)**2*SP+2.D0*(GV-GA)**2*SM)
      B6=2.D0*E**2*((GV+GA)**2+(GV-GA)**2)
      B7=2.D0*CDABS(SX)**2
      B8=2.D0*S**2*E**4
      B9=2.D0*S**2*(GV**2-GA**2)**2
      B10=4.D0*S**2*E**2*(GV**2-GA**2)
C
      CROSS= B1*H1 + B2*H2 + B3*H3 + B4*H4 + B5*H5 +
     .       B6*H6 + B7*H7 + B8*H8 + B9*H9 + B10*H10
      CROSS= CROSS/(32.D0*PI*S)*3.8937D8
C
*                          Under control of the flag IFLDB
      IF(IFLDB.EQ.0) GO TO 777
      IF(ICRBN.EQ.1)GO TO 7777
        WRITE(IUT,1) EB,XMZ,XGZ,GV,GA,THMIN,THMAX
    1   FORMAT(' BORN   : ',4D15.6)
  777 CONTINUE
      IF(ICRBN.EQ.1)GO TO 7777
      WRITE(IUT,2) CROSS
    2 FORMAT(' BORN   : THE LOWEST-ORDER CROSS SECTION IS'/,
     .       '          ',D15.6,'   PICOBARN')
 7777 RETURN
      END
*DECK,CANCUT
      SUBROUTINE CANCUT(QP,QM,QK,W)
*
* CANCONICAL CUTS: ACOLINEARITY AND THRESHOLD ENERGY
* THE THRESHOLD ENERGY IS ALWAYS 1/2 OF THE BEAM ENERGY
*
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION CCUT(31),QP(4),QM(4),QK(4)
      COMMON / CUTCOM / W1(31),W2(31),WA0,WA1
      COMMON / ACOLCO / ACOL(31)
      DATA INIT/0/
*
      IF(INIT.EQ.0) THEN
        CALL OUTCRY('CANCUT')
        INIT=1
        ECUT=(QP(4)+QM(4)+QK(4))/2.D0*( 0.5D0)
        ACOL(1)=1.D0
        ACOL(2)=2.D0
        ACOL(3)=3.D0
        ACOL(4)=4.D0
        ACOL(5)=5.D0
        ACOL(6)=6.D0
        ACOL(7)=7.D0
        ACOL(8)=8.D0
        ACOL(9)=9.D0
        ACOL(10)=10.D0
        ACOL(11)=12.D0
        ACOL(12)=14.D0
        ACOL(13)=16.D0
        ACOL(14)=18.D0
        ACOL(15)=20.D0
        ACOL(16)=25.D0
        ACOL(17)=30.D0
        ACOL(18)=35.D0
        ACOL(19)=40.D0
        ACOL(20)=45.D0
        ACOL(21)=50.D0
        ACOL(22)=55.D0
        ACOL(23)=60.D0
        ACOL(24)=70.D0
        ACOL(25)=80.D0
        ACOL(26)=90.D0
        ACOL(27)=100.D0
        ACOL(28)=120.D0
        ACOL(29)=140.D0
        ACOL(30)=160.D0
        ACOL(31)=180.D0
        PI=4.D0*DATAN(1.D0)
        DO 1 K=1,31
          CCUT(K)=DCOS(ACOL(K)*PI/180.D0)
          W1(K)=0.D0
    1   W2(K)=0.D0
        WA0=0.D0
        WA1=0.D0
      ENDIF
*
* DETERMINE ACOLLINEARITY ANGLE BETWEEN QP AND QM
      C=-(QP(1)*QM(1)+QP(2)*QM(2)+QP(3)*QM(3))/(QP(4)*QM(4))
*
* ADD TO COUNTERS FOR ALL EVENTS
      WA0=WA0+1.D0
      WA1=WA1+W
*
* CHECK ON THRESHOLD ENERGIES
      IF(QP(4).LT.ECUT) RETURN
      IF(QM(4).LT.ECUT) RETURN
      WSQ=W*W
*
* CHECK ON THE VARIOUS ACOLLINEARITY CUTS
      DO 2 K=1,31
        IF(C.GT.CCUT(K)) THEN
          W1(K)=W1(K)+W
          W2(K)=W2(K)+WSQ
        ENDIF
    2 CONTINUE
      RETURN
      END
*DECK,ENDCUT
      SUBROUTINE ENDCUT(SIGTOT)
*
* EVALUATE THE CROSS SECTION AFTER CUTS: SIGTOT IS THE
* GENERATED TOTAL CROSS SECTION, PUT EQUAL TO WA1/WA0
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SIG(31),ERR(31),AV(31),DE(31)
      COMMON / CUTCOM / W1(31),W2(31),WA0,WA1
      COMMON / ACOLCO / ACOL(31)
      COMMON / UNICOM / IIN,IUT
      CALL OUTCRY('ENDCUT')
*
*     COMPUTE THE MEAN WEIGHTS AND THEIR DEVIATION
      CONVER=SIGTOT/(WA1/WA0)
      DO 2 K=1,31
        IF(W1(K).EQ.0.D0) THEN
          SIG(K)=0.D0
          ERR(K)=0.D0
          GOTO 2
        ENDIF
        SIG(K)=CONVER*W1(K)/WA0
        ERR(K)=CONVER*DSQRT(W2(K)-W1(K)**2/WA0)/WA0
    2 CONTINUE
      WRITE(IUT,11)
     .   (ACOL(K),W1(K),W2(K),SIG(K),ERR(K),K=1,31)
   11 FORMAT(' RESULTS FROM THE CANONICAL CUTS FOR THE'/,
     . ' GENERATED SAMPLE INSIDE THE 50% ENERGY THRESHOLDS:'/,
     . '  ACOLL.   SUM(W**1)   SUM(W**2)       XSECTION        ERROR'/,
     . (' ',F6.1,2D12.4,2D15.6))
      RETURN
      END
*DECK,TELLER
      SUBROUTINE TELLER(K,NTEL,STRING)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / UNICOM / IIN,IUT
      CHARACTER*10 STRING
      X1=(1.D0*K)/(1.D0*NTEL)
      X2=1.D0*(K/NTEL)
      IF(X1.EQ.X2) WRITE(IUT,1) K,STRING
    1 FORMAT(' EVENT COUNTER AT',I10,' AT LOCATION  ',A10)
      RETURN
      END
*DECK,HISTO1
      SUBROUTINE HISTO1(IH,IB,X0,X1,X,W)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / HISTOC /H(100,10),HX(100),IO(100),IU(100),II(100),
     .                 Y0(100),Y1(100),IC(100)
      DATA ISTART/0/
      IF(ISTART.EQ.0) THEN
        DO 102 NH=1,100
          DO 101 NB=1,10
  101     H(NH,NB)=0.D0
          HX(NH)=0.D0
          IO(NH)=0
          IU(NH)=0
          II(NH)=0
          Y0(NH)=1.D0
          Y1(NH)=0.D0
  102   IC(NH)=0
        ISTART=1
      ENDIF
      Y0(IH)=X0
      Y1(IH)=X1
      IC(IH)=IB
      IF(X.LT.X0) GOTO 11
      IF(X.GT.X1) GOTO 12
      IX=IDINT((X-X0)/(X1-X0)*1.D0*(IB))+1
      H(IH,IX)=H(IH,IX)+W
      IF(H(IH,IX).GT.HX(IH)) HX(IH)=H(IH,IX)
      II(IH)=II(IH)+1
      RETURN
   11 IU(IH)=IU(IH)+1
      RETURN
   12 IO(IH)=IO(IH)+1
      RETURN
      END
*DECK,HISTO2
      SUBROUTINE HISTO2(IH,IL)
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*1 REGEL(30),BLANK,STAR
      COMMON / HISTOC /H(100,10),HX(100),IO(100),IU(100),II(100),
     .                 Y0(100),Y1(100),IC(100)
      COMMON / UNICOM / IIN,IUT
      DATA REGEL /30*' '/,BLANK /' '/,STAR /'*'/
      X0=Y0(IH)
      X1=Y1(IH)
      IB=IC(IH)
      HX(IH)=HX(IH)*(1.D0+1.D-06)
      IF(IL.EQ.0) WRITE(IUT,21) IH,II(IH),IU(IH),IO(IH)
      IF(IL.EQ.1) WRITE(IUT,22) IH,II(IH),IU(IH),IO(IH)
   21 FORMAT(' NO.',I3,' LIN : INSIDE,UNDER,OVER ',3I6)
   22 FORMAT(' NO.',I3,' LOG : INSIDE,UNDER,OVER ',3I6)
      IF(II(IH).EQ.0) GOTO 28
      WRITE(IUT,23)
   23 FORMAT(35(1H ),3(10H----+----I))
      DO 27 IV=1,IB
        Z=1.D0*(IV)/(1.D0*(IB))*(X1-X0)+X0
        IF(IL.EQ.1) GOTO 24
        IZ=IDINT(H(IH,IV)/HX(IH)*30.)+1
        GOTO 25
   24   IZ=-1
        IF(H(IH,IV).GT.0.D0)
     .IZ=IDINT(DLOG(H(IH,IV))/DLOG(HX(IH))*30.)+1
   25   IF(IZ.GT.0.AND.IZ.LE.30) REGEL(IZ)=STAR
        WRITE(IUT,26) Z,H(IH,IV),(REGEL(I),I=1,30)
   26   FORMAT(1H ,2G15.6,4H   I,30A1,1HI)
        IF(IZ.GT.0.AND.IZ.LE.30) REGEL(IZ)=BLANK
   27 CONTINUE
      WRITE(IUT,23)
      IF(IUT.EQ.0) PAUSE
      RETURN
   28 WRITE(IUT,29)
   29 FORMAT('0 NO ENTRIES INSIDE HISTOGRAM')
      RETURN
      END
*DECK,HISTO3
      SUBROUTINE HISTO3(IH)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / HISTOC /H(100,10),HX(100),IO(100),IU(100),II(100),
     .                 Y0(100),Y1(100),IC(100)
      DO 31 I=1,10
   31 H(IH,I)=0.D0
      HX(IH)=0.D0
      II(IH)=0
      IU(IH)=0
      IO(IH)=0
      RETURN
      END
*DECK,RN
      FUNCTION RN(DUMMY)
      IMPLICIT REAL*8(A-H,O-Z)
      DATA P/390625D+00/
      DATA Q/9179D+00/
      DATA U/2.147483648D+09/
      DATA S/1234567.D0/
      S=DMOD(P*S + Q  , U)
      RN=(S/U)
      RETURN
      END
*DECK,PRNVEC
      SUBROUTINE PRNVEC(QP,QM,QK,A1,A2,A3,A4,WM,EX,I)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION QP(4),QM(4),QK(4)
      COMMON / UNICOM / IIN,IUT
      IF(QK(4).EQ.0.D0) RETURN
      WRITE(IUT,1) (QP(K),QM(K),QK(K),K=1,4)
    1 FORMAT('0ANOMALOUS EVENT PRINTOUT',4(/3D15.6))
      CP=QP(3)/QP(4)
      CM=QM(3)/QM(4)
      CK=QK(3)/QK(4)
      WRITE(IUT,2) CP,CM,CK
    2 FORMAT(' COSINES OF THE SCATTERING ANGLES:'/,3D15.6)
      CPM=(QP(1)*QM(1)+QP(2)*QM(2)+QP(3)*QM(3))/QP(4)/QM(4)
      CPK=(QP(1)*QK(1)+QP(2)*QK(2)+QP(3)*QK(3))/QP(4)/QK(4)
      CMK=(QM(1)*QK(1)+QM(2)*QK(2)+QM(3)*QK(3))/QM(4)/QK(4)
      WRITE(IUT,3) CPM,CPK,CMK
    3 FORMAT(' COSINES BETWEEN P-M, P-K, M-K :'/,3D15.6)
      XPM=2.D0*(QP(4)*QM(4)-QP(3)*QM(3)-QP(2)*QM(2)-QP(1)*QM(1))
      XPK=2.D0*(QP(4)*QK(4)-QP(3)*QK(3)-QP(2)*QK(2)-QP(1)*QK(1))
      XMK=2.D0*(QK(4)*QM(4)-QK(3)*QM(3)-QK(2)*QM(2)-QK(1)*QM(1))
      WRITE(IUT,4) XPM,XPK,XMK
    4 FORMAT(' INVARIANT MASSES P-M, P-K, M-K :'/,3D15.6)
      WTOT=EX*WM/(A1+A2+A3+A4)
      WRITE(IUT,5) A1,A2,A3,A4,WM,EX,WTOT,I
    5 FORMAT(' THE APPROXIMANTS:',4D15.6/,
     . ' THE MASS EFFECT FACTOR:',D15.6/,
     . ' THE EXACT CROSS SECTION:',D15.6/,
     . ' THE RESULTING WEIGHT:',D15.6/,
     . ' OBTAINED IN CHANNEL NO.',I2)
      RETURN
      END
*DECK,EFFCIT
      SUBROUTINE EFFCIT
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / EFFIC1 / EFK1
      COMMON / EFFIC2 / EFK2
      COMMON / EFFIC4 / EFK4PR,EFK4PO,EFC4PR,EFC4PO
      COMMON / EFFIC5 / EFPR5,EFPO5
      COMMON / UNICOM / IIN,IUT
      WRITE(IUT,1)
    1 FORMAT(' COMPARISON OF EXPECTED AND OBTAINED EFFICIENCIES')
      WRITE(IUT,11) EFK1
   11 FORMAT(' GENER1 : K LOOP : ',D15.6)
      WRITE(IUT,21) EFK2
   21 FORMAT(' GENER2 : K LOOP : ',D15.6)
      WRITE(IUT,41) EFK4PR,EFK4PO,EFC4PR,EFC4PO
   41 FORMAT(' GENER4 : K LOOP : ',D15.6,' EXPECTED'/,
     .       '                   ',D15.6,' OBSERVED'/,
     .       '          C LOOP : ',D15.6,' EXPECTED'/,
     .       '                   ',D15.6,' OBSERVED')
      WRITE(IUT,51) EFPR5,EFPO5
   51 FORMAT(' GENER5 : C LOOP : ',D15.6,' EXPECTED'/,
     .       '                   ',D15.6,' OBSERVED')
      RETURN
      END
*DECK,OUTCRY
      SUBROUTINE OUTCRY(STRING)
      CHARACTER*6 STRING
      COMMON / UNICOM / IIN,IUT
      WRITE(IUT,1) STRING
    1 FORMAT(' ROUTINE "',A6,'" STARTING NOW ... GO!')
      RETURN
      END
*DECK,POL
      SUBROUTINE POL(S,T,ALSS,ALTT,ALTS,AUSS,AUTT,AUTS,BUSS,BUTT)
      IMPLICIT REAL*8(A-H,P-Z)
      REAL*8 MZ,MH,MW
      COMPLEX*16 CHIS,CHIT
      COMMON /COUP/SW,CW,V,A,VU,AU,VD,AD/BOSEW/MZ,MW,MH/WIDTH/GZ
      GPL=DSQRT(SW/CW)
      GMI=(2D0*SW-1D0)/DSQRT(4D0*CW*SW)
      CHIS=DCMPLX(S  ,0D0)/DCMPLX(S-MZ**2,MZ*GZ)
      CHIT=DCMPLX(T,0D0)/DCMPLX(T-MZ**2,0D0)
      ALSS=.5D0*(GMI**2-GPL**2)*DREAL(DCONJG(CHIS)+CHIS)+.5D0*(GMI**4-
     $      GPL**4)*DREAL(DCONJG(CHIS)*CHIS)
      ALTT=.5D0*(GMI**2-GPL**2)*DREAL(DCONJG(CHIT)+CHIT)+.5D0*(GMI**4-
     $      GPL**4)*DREAL(DCONJG(CHIT)*CHIT)
      ALTS=.5D0*(GMI**2-GPL**2)*DREAL(DCONJG(CHIT)+CHIS)+.5D0*(GMI**4-
     $      GPL**4)*DREAL(DCONJG(CHIT)*CHIS)
      AUSS=1D0+
     $     .5D0*(GMI**2+GPL**2)*DREAL(DCONJG(CHIS)+CHIS)+.5D0*(GMI**4+
     $      GPL**4)*DREAL(DCONJG(CHIS)*CHIS)
      AUTT=1D0+
     $     .5D0*(GMI**2+GPL**2)*DREAL(DCONJG(CHIT)+CHIT)+.5D0*(GMI**4+
     $      GPL**4)*DREAL(DCONJG(CHIT)*CHIT)
      AUTS=1D0+
     $     .5D0*(GMI**2+GPL**2)*DREAL(DCONJG(CHIT)+CHIS)+.5D0*(GMI**4+
     $      GPL**4)*DREAL(DCONJG(CHIT)*CHIS)
      BUSS=1D0+
     $           GMI*GPL       *DREAL(DCONJG(CHIS)+CHIS)+      GMI**2*
     $      GPL**2 *DREAL(DCONJG(CHIS)*CHIS)
      BUTT=1D0+
     $           GMI*GPL       *DREAL(DCONJG(CHIT)+CHIT)+      GMI**2*
     $      GPL**2 *DREAL(DCONJG(CHIT)*CHIT)
      RETURN
      END
      FUNCTION SPOL1(S,T,POLR1)
      IMPLICIT REAL*8(A-H,P-Z)
      CALL POL(S,T,ALSS,ALTT,ALTS,AUSS,AUTT,AUTS,BUSS,BUTT)
      U=-S-T
      SPOL1=-POLR1*(U*U*ALSS/S**2+U*U*ALTT/(T*T)+2D0*U*U*ALTS/(S*T))/
     $      (U*U*AUSS/S**2+U*U*AUTT/(T*T)+2D0*U*U*AUTS/(S*T)
     $       +T*T*BUSS/S**2+S*S*BUTT/(T*T))
      RETURN
      END
