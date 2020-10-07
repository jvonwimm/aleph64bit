!
      SUBROUTINE BHLUM4(MODE,XPAR,NPAR)  
!     *********************************  
! 
!           ************************************************** 
!           *       **********************************       *
!           *       *      *******************       *       *
!           *       *      *                 *       *       *
!           *       *      *   B H L U M 4   *       *       *
!           *       *      *                 *       *       *
!           *       *      *******************       *       *
!           *       **********************************       *
!           **************************************************
!
! =======================     AUTHORS      =============================
! ==  S. Jadach, W. Placzek, E. Richter-Was, B.F.L. Ward  and Z. Was  ==
! =================  vers. 4.04 September 1996 =========================
! 
! Main subprogram of the  Monte Carlo multiphoton t-channel generator 
! BHLUMI version 4.04.
! It is multiphoton generator with Yennie-Frautschi-Suura second 
! order exponentiation based on refs. [1,2,3,4,5,6] and other.
! According to ref. [6] it features an OVERALL PRECISION is 0.11%, 
! see there for the validity range of the above statement.
! Z-contribution now added according to ref. [5].
! [1] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
!     Comp. Phys. Commun. 70 (1992) 305 (TH-6230, sept. 1991).
! [2] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
!     Phys.Lett. B268 (1991), 253 (TH-6118, June 1991). 
! [3] S. Jadach and B.F.L. Ward,
!     Phys. Rev. D40 (1989) 3582.
! [4] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
!     TH-95-38 (February 1995); Phys. Lett., B353 (1995) 362.
! [5] S. Jadach, W. Placzek and B.F.L. Ward
!     TH-95-74 (March 1995); Phys. Lett. B353 (1995) 349.
! [6] S. Jadach and O. Nicrosini (conveners of Bhabha Working Group),  
!     in Physics at LEP2, CERN Yellow Report 96-01, edited by 
!     G. Altarelli, T. Sjostrand, and F. Zwirner (CERN, Geneva, 1996), 
!     Vol. 2, p. 229, (hep-ph/9602393);
!     Summary paper of LEP2 Bhabha working group, 
!     (Convenors S. Jadach and O. Nicrosini)
!     A. Arbuzov et.al., Phys. Lett. B383 (1996) 238 (hep-ph/9605239)
! Postscript files for all the above papers are
! available from http://hpjmiady.ifj.edu.pl/programs/programs.html
! 
!----------------------------------------------------------------------
!                 INPUT and OUTPUT of BHLUM4
!----------------------------------------------------------------------
! All input and output goes through parameters in 
!                 CALL BHLUM4(MODE,XPAR,NPAR)
! and through /MOMSET/ and /WGTALL/ common blocks.
! In the following we shall  briefly indicate the meaning of the
! above parameters/variables.
! 
! IF( MODE =-1 ) THEN
! ===================
! Initialization is performed, all input parameters are transfered
! through XPAR and NPAR.
! In the following table we indicate the meaning of NPAR, XPAR 
! entries for LUMLOG subgenerator in the initialization mode. 
!      Table,           Input parameters of BHLUM4
!----------------------------------------------------------------------
!  Entry    Variable   Meaning
!----------------------------------------------------------------------
!  NPAR( 1)  KeyOpt =1000*KeyGen +100*KeyREM +10*KeyWGT +KeyRND 
!                    General Option switch in wchich:
!            KeyGen =3 for this sub-generator
!            KeyRem =0,1 removal/no-removal switch, both are OK,
!                   =1 no-removal probably safer, RECOMMENDED
!            KeyRnd =1,2 type of random number generator RANMAR,RANECU
!                   =1 better for parallel production on computer farm.
!            KeyWgt =0,1,2 for constant/variable weight WTMOD,
!                   =0, WTMOD =1 useful for apparatus Monte Carlo.
!                   =1, WTMOD variable, option faster/safer, RECOMMENDED
!                   =2, WTMOD variable, events below trmin generated
!  NPAR( 2)  KeyRad =1000*KeyZet+100*KeyUpd+10*KeyMod +KeyPia 
!                   Switch defining type of matrix element:
!            KeyZet =0,1 test switch,
!                   =0   Z contribution OFF 
!                   =1   Z contribution ON, DEFAULT!!!
!            KeyUpd =0,1,2 test switch, 
!                   =0 normal position DEFAULT!!!
!                   =1 only upper line bremss., =2 only lower line
!            KeyMod =1,2 type of MODEL subrogram and QED matrix element
!                   =1 version compatible with Comp. Phys. Comm. 70 (1992) 305
!                   =2 version 4.x which is now DEFAULT!
!            KeyPia =0,1,2,3 photon vacuum polarization and s-chanel photon
!                   =0 OFF, it used in semianalytical tests,
!                   =1 ON,  Burkhardt et.al. 1989, as in BHLUMI 2.0x
!                   =2 ON,  S. Eidelman, F. Jegerlehner, Z. Phys. C (1995)
!                   =3 ON,  Burkhardt and Pietrzyk 1995 (Moriond).
!  XPAR( 1)  CMSENE Total center mass energy [GeV]
!  XPAR( 2)   TRMIN Minimum transfer (positive) [GeV**2] 
!  XPAR( 3)   TRMAX Maximum transfer (positive) [GeV**2] 
!  XPAR( 4)   EPSCM Dimensionless infrared cut on CMS energy of soft
!                   photons, ( E_phot > CMSENE*EPSCM/2 )
!----------------------------------------------------------------------
! 
! ELSE IF( MODE = 0 ) THEN
! ========================
! Generation of the single Monte Carlo event. 
! The four momenta of the final state electron, positron and photon
! and of real photons are encoded in 
!      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
! where P1 and Q1 are four-momenta of positron and electron beams.
! P2 and Q2 are four-momenta of outgoing positron and electron.
! The list PHOT(100,4) four-momenta contains 
! NPHOT four-momenta of real the photons, all in GeV units.
! The principal weight WTM of the event is placed in
!      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)   
! It is often of interest to use 'parallel weights' from WTSET.
! The event weight is constructed then as WT= WTCRU1*WTCRU2*WTSET(J).
! Which J is allowed and what version of the QED matrix element 
! it represents is summarized in the tables below.
!
! One of the weights in WTSET (the best) is used for WTMOD.
! This is the DEFAULT weight used for rejection for KeyWgt =0.
! WARNING: using 'parallel weights' makes only sense for calculation
!          with variable weights, KeyWgt=1,2 !!!
!======================================================================!
!======================================================================!
! The list of actual weights corresponding to  KeyMod = 2              !
! (the obsolete weights for KeyMod = 1 listed later below              !
!======================================================================!
!  WtMod = wtset(1) = wtset(142)+wtset(12)  for Z on, Vac.Pol. on
!  WtMod = wtset(1) = wtset(142)+wtset(11)  for Z on, Vac.Pol. off
!        where wtset(142) is calculated in modl2b.f
!        and   wtset(11,12) are from m2agzi.f
!  The s-chan contribution and VacPol are introduced 
!  multiplicatively wtset(i)=wtset(i)*(1 +dels)*vpfac in model.f
!----------------------------------------------------------------------!
! (A) List of auxiliary weights from modl2a
!    WTSET( 30) =   O(alf0)exp      total    upper line only
!    WTSET( 31) =     O(alf1)exp    total    upper line only
!    WTSET( 32) =       O(alf2)exp  total    upper line only
!    WTSET( 35) =     O(alf1)exp    beta0    upper line only
!    WTSET( 36) =     O(alf1)exp    beta1    upper line only
!    WTSET( 37) =       O(alf2)exp  beta0    upper line only
!    WTSET( 38) =       O(alf2)exp  beta1    upper line only
!    WTSET( 39) =       O(alf2)exp  beta2    upper line only
!    WTSET( 40) =   O(alf0)exp       total    upper + lower
!    WTSET( 41) =     O(alf1)exp     total    upper + lower
!    WTSET( 42) =       O(alf2)exp   total    upper + lower
!    WTSET( 43) =     O(alf1)exp     beta0    upper + lower
!    WTSET( 44) =     O(alf1)exp     beta1    upper + lower
!    WTSET( 45) =     O(alf1)exp     beta1    upper component
!    WTSET( 46) =     O(alf1)exp     beta1    lower component
!    WTSET( 47) =       O(alf2)exp   beta0    upper + lower
!    WTSET( 48) =       O(alf2)exp   beta1    upper + lower
!    WTSET( 49) =       O(alf2)exp   beta2    upper+lower+upper*lower
!    WTSET( 50) =       O(alf2)exp   beta1    upper component
!    WTSET( 51) =       O(alf2)exp   beta1    lower component
!    WTSET( 52) =       O(alf2)exp   beta2    upper*lower component
!    WTSET( 53) =       O(alf2)exp   beta2    upper component
!    WTSET( 54) =       O(alf2)exp   beta2    lower component
!    WTSET( 60) =    O(alf0)       total    upper line only
!    WTSET( 61) =      O(alf1)     total    upper line only
!    WTSET( 62) =        O(alf2)   total    upper line only
!    WTSET( 70) =    O(alf0)       total    upper + lower
!    WTSET( 71) =      O(alf1)     total    upper + lower
!    WTSET( 72) =        O(alf2)   total    upper + lower
!----------------------------------------------------------------------!
! (B) List of auxiliary weights from modl2b
!    WTSET(130) =   O(alf0)exp      total    upper line only
!    WTSET(131) =     O(alf1)exp    total    upper line only
!    WTSET(132) =       O(alf2)exp  total    upper line only
!    WTSET(135) =     O(alf1)exp    beta0    upper line only
!    WTSET(136) =     O(alf1)exp    beta1    upper line only
!    WTSET(137) =       O(alf2)exp  beta0    upper line only
!    WTSET(138) =       O(alf2)exp  beta1    upper line only
!    WTSET(139) =       O(alf2)exp  beta2    upper line only
!    WTSET(140) =   O(alf0)exp       total    upper + lower
!    WTSET(141) =     O(alf1)exp     total    upper + lower
!    WTSET(142) =       O(alf2)exp   total    upper + lower
!    WTSET(143) =     O(alf1)exp     beta0    upper + lower
!    WTSET(144) =     O(alf1)exp     beta1    upper + lower
!    WTSET(145) =     O(alf1)exp     beta1    upper component
!    WTSET(146) =     O(alf1)exp     beta1    lower component
!    WTSET(147) =       O(alf2)exp   beta0    upper + lower
!    WTSET(148) =       O(alf2)exp   beta1    upper + lower
!    WTSET(149) =       O(alf2)exp   beta2    upper+lower+upper*lower
!    WTSET(150) =       O(alf2)exp   beta1    upper component
!    WTSET(151) =       O(alf2)exp   beta1    lower component
!    WTSET(152) =       O(alf2)exp   beta2    upper*lower component
!    WTSET(153) =       O(alf2)exp   beta2    upper component
!    WTSET(154) =       O(alf2)exp   beta2    lower component
!    WTSET(160) =    O(alf0)       total    upper line only
!    WTSET(161) =      O(alf1)     total    upper line only
!    WTSET(162) =        O(alf2)   total    upper line only
!    WTSET(170) =    O(alf0)       total    upper + lower
!    WTSET(171) =      O(alf1)     total    upper + lower
!    WTSET(172) =        O(alf2)   total    upper + lower
!----------------------------------------------------------------------!
!
!======================================================================!
!======================================================================!
! These weights are OBSOLETE and are still accessible for KeyMod = 1   !
! seeabove  for the actual weights for KeyMod = 2                      !
!======================================================================!
!    The table explains weights as published in BHLUMI Version 2.01    !
!    in  Comp. Phys. Commun. {\bf 70} (1992) 305, table 4 there.       !
!----------------------------------------------------------------------!
!  Entry      Type of QED calculation                                  !
!----------------------------------------------------------------------!
!  WTSET( 1)  = WTSET(11)                                              !
!  WTSET( 2)  = WTSET(12)           (principal weight)                 !
!             ---------------------------------------------------------!
!             QED order   Vacuum pol.   Z-exchange    s-chan.gamma exch!
!             ---------------------------------------------------------!
!  WTSET(11)  Zero-th      Yes             Yes            Yes          !
!  WTSET(12)  First        Yes             Yes            Yes          !
!  WTSET(51)  Zero          No              No             No          !
!  WTSET(52)  First         No              No             No          !
!                           ----- Miscelanous ----          !
!  WTSET(20)  First        Yes              No             No          !
!  WTSET(21)  = WTSET(20)-WTSET(52)                                    !
!  WTSET(22)  First        Yes             Yes             No          !
!  WTSET(23)  = WTSET(22)-WTSET(20)                                    !
!  WTSET(24)  First        Yes             Yes            Yes          !
!  WTSET(25)  = WTSET(24)-WTSET(22)                                    !
!             ---------------------------------------------------------!
!  WTSET(26)  Beta_0 component in WTSET(20)                            !
!  WTSET(27)  Beta_1 component in WTSET(20),                           !
!             i.e. WTSET(20)=WTSET(26)+WTSET(27)                       !
!             ---------------------------------------------------------!
!  WTSET(61)  Kind of  LL  component in    WTSET(51)                   !
!  WTSET(62)  Kind of  NLL component in    WTSET(52)                   !
!======================================================================!
!
! 
! ELSE IF( MODE = 1 ) THEN
! ========================
! The total cross section corresponding to generated series of event,
! i.e. resulting from MC integration is calculated and stored in XPAR
! and NPAR, see table below.
!----------------------------------------------------------------------
!  Entry    Variable   Meaning
!----------------------------------------------------------------------
!  NPAR(10)  NEVGEN  Number of generated MC events
!  NPAR(20)  NEVGEN  Number of generated MC events
!  XPAR(10)   XMCNB  Total x-section [nb]
!  XPAR(11)    EREL  The relative error of XPAR(10)
!  XPAR(12)     XMC  Total x-section [GEV**-2]
!  XPAR(20)  SIG0NB  Crude total MC x-section [nb] which is necessary
!                    for rescaling histograms in run with 
!                    weighted events.
!  XPAR(21)          =0, error of XPAR(20) is zero
!  XPAR(20)    SIG0  Crude x-sectio as XPAR(20) but in [GeV**-2]
!----------------------------------------------------------------------
! For constant weight option KeyWgt=0 (convenience in rescaling histos)
! we put XPAR(20,21,22)=XPAR(10,11,12) !
! For MODE=1 program is called upon many times in the process of 
! rescaling histograms, therefore, there is no output printed 
! in this mode.
! 
! ELSE IF( MODE = 2 ) THEN
! ========================                     
! Only in this MODE=2 in addition to filling XPAR and NPAR as for 
! MODE=1 the values of various x-sections are printed on standard 
! output file.
!                
! ENDIF
! ====
!*******************************************************************
!      For hidded dip-switches adjustable by hand in the code see:
!&&&&  definition of del in terms of epscm
!%%%%  TRMX2 = TRMAX !!! only for tests with fixed TRAN 
!*******************************************************************
!     *********************************   
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
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS        
      COMMON / BHPAR2 / CMSENE,AMEL       
      COMMON / BHPAR3 / KeyRad,KeyOpt
! CMONIT COMMUNICATES WITH Gmonit         
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER 
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / MOMSET / PX1(4),QX1(4),PX2(4),QX2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)        
      COMMON / WGTSUP / WKP,WKQ,WTT1,WTT2,FPHS,FYFSU,FYFSD,WT3
      COMMON / INOUT  / NINP,NOUT 
      SAVE   / BHPAR1 /, / BHPAR2 /, / BHPAR3 /, / CMONIT/, / TRANSR / 
      SAVE   / MOMS1  /, / MOMS2  /, / MOMZ1  /, / MOMZ2 /, / MOMSET /
      SAVE   / WGTALL /, / WGTSUP /, / INOUT  /
      SAVE   SVAR, WTMAX, TRMX2, EMIN, IDA,IDC
      SAVE   IDGEN, NEVGEN, IEVENT, SIG0, SIG0NB
      SAVE   KeyWgt, KeyRem, KeyUpd
      DOUBLE PRECISION DRVEC(100)
   
      IF(MODE.EQ.-1) THEN        
!     ===================
      CALL FILBH2(XPAR,NPAR)       
! This is Generator Identificator
      IDGEN = 3        
      SVAR=CMSENE**2 
!       
      KeyWgt = MOD(KeyOpt,100)/10   
      KeyRem = MOD(KeyOpt,1000)/100   
      KeyUpd = MOD(KeyRad,1000)/100   
      IF( KeyWgt .EQ. 2 .AND. abs(trmax/trmin-1).GT.0.1d0 ) THEN
!-- NEW feature: Generation of transfer below trmin
         sig0= 4d0*pi*alfa**2 *( 2d0/trmin-1d0/trmax)
      ELSE
!-- OLD method: no generation of transfer below trmin
         sig0= 4d0*pi*alfa**2 *( 1d0/trmin-1d0/trmax)
      ENDIF
!
      SIG0NB=SIG0*GNANOB       
      WTMAX= 2.8D0
      WTMAX= 3.0D0
! Important histo which remembers total x-section 
      CALL Gmonit(  -1, IDGEN,0D0,SIG0NB*WTMAX,1D0)          
!-- Set maximum transfer for photon angular distributions TRMX2 :
!-- In general TRMX2=svar is a safe choice. 
!-- However, for low  angles (i.e. thmin,thmax << 100mrad)
!-- It can be lowered, in order to speed up generation,
!-- but with great care!. 
      TRMX2 = TRMAX
! (over)conservative and safe choice is TRMX2=SVAR 
!     TRMX2 = SVAR   !%%%% For standard  low angles can be comented out!
      IF(TRMX2.GT.SVAR) TRMX2=SVAR
      EMIN = CMSENE/2D0*EPSCM
! Weight monitoring initialization
      IDA=50
      CALL Gmonit(  -1, IDA+1 ,0D0,1D0,1D0)  ! Obsolete        
      CALL Gmonit(  -1, IDA+2 ,0D0,1D0,1D0)  ! Obsolete           
      CALL Gmonit(  -1, IDA+3 ,0D0,1D0,1D0)  ! Obsolete         
      CALL Gmonit(  -1, IDA+4 ,0D0,1D0,1D0)  ! Obsolete          
      CALL Gmonit(  -1, IDA+18,0D0,1D0,1D0)          
      CALL Gmonit(  -1, IDA+19,0D0,1D0,1D0)          
      CALL Gmonit(  -1, IDA+20,0D0,1D0,1D0)          
      IDC = 90
      DO 12 I=IDC+1,IDC+6
  12  CALL Gmonit(  -1, I,0D0,1D0,1D0)  
      CALL GBOOK1(9001,' bhlum4, weight distribution $', 40, -1D0,7D0)
      ievent=0  
      nevgen=0
!     ======================     
      ELSEIF(MODE.EQ.0) THEN     
!     ======================     
      nevgen = nevgen+1
  200 CONTINUE
      ievent=ievent+1
      WT0=0D0
      WT1=0D0
      WT2=0D0
      WT3=0D0
      CALL VarRan(drvec,2)
!--------------------------------------------------------------------
! Generate t-channel transfer (the true one)
      rn1 = drvec(1)
      IF( KeyWgt .EQ. 2 .AND. abs(trmax/trmin-1).GT.0.1d0 ) THEN
!-- NEW feature: Generation of transfer below trmin
        ynorm= 2/trmin -1/trmax
        prob1= (1/trmin) /ynorm
        prob2= (1/trmin -1/trmax)/ynorm
        IF(rn1.LT.prob1) THEN
!-- flat distribution below trmin
          tran= trmin*rn1/prob1
          WT0 = trmin**2/tran**2
!-- to prevent crash of kinematics  (1d-8 effect)
          IF(tran.lt.(10*Amel)**2) GOTO 200
        ELSE
!-- normal 1/tra**2 distribution above trmin
          rnx  = (rn1-prob1)/prob2
          tran = 1d0 / (  rnx/trmin   +(1d0-rnx)/trmax )
          WT0  = 1d0
        ENDIF
      ELSE
!-- OLD method: no generation of transfer below trmin
        tran   = 1d0 / (  rn1/trmin   +(1d0-rn1)/trmax )
        WT0=1d0
      ENDIF
!--------------------------------------------------------------------
! Generate Photon multiplicity and momenta
      IF( KeyUpd .EQ. 0 ) THEN
        CALL mltibr(TRAN,TRMX2,AMEL,DEL,
     $      NPHOT1,PHOT1,PHSU1,AL1,BE1,TRANP,AMSP,MK1,WKP,WTM1)
        CALL mltibr(TRAN,TRMX2,AMEL,DEL,
     $      NPHOT2,PHOT2,PHSU2,AL2,BE2,TRANQ,AMSQ,MK2,WKQ,WTM2)
      ELSEIF( KeyUpd .EQ. 1) THEN
! Upper line bremss. only -- for special tests only!!
        CALL mltibr(TRAN,TRMX2,AMEL,DEL,
     $      NPHOT1,PHOT1,PHSU1,AL1,BE1,TRANP,AMSP,MK1,WKP,WTM1)
        NPHOT2 = 0
        TRANQ  = TRAN
        AMSQ   = AMEL**2
        WKQ    = 1D0
      ELSEIF( KeyUpd .eq. 2) THEN
! Lower line bremss. only -- for special tests only!!
        CALL mltibr(TRAN,TRMX2,AMEL,DEL,
     $      NPHOT2,PHOT2,PHSU2,AL2,BE2,TRANQ,AMSQ,MK2,WKQ,WTM2)
        NPHOT1 = 0
        TRANP  = TRAN
        AMSP   = AMEL**2
        WKP    = 1D0
      ELSE
        WRITE(nout,*) '+++++BHLUM4:  wrong KeyUpd=',KeyUpd
        STOP
      ENDIF
!
      IF(WKP*WKQ.EQ.0D0) GOTO 140
!--------------------------------------------------------------------
!-- Construct fermions, transform photons and fermions to CMS frame
      CALL kino4(SVAR,TRAN,AMEL,AMSP,AMSQ,WT3)
      CALL Gmonit(   0,IDA+3,  WT3, 1D0,5D-4) 
      IF(WT3.EQ.0D0) GOTO 140      
!-- Beyond this point events DO CONSERVE total four-momentum !!!
      WTKIN=1D0      
!-- Manipulations on mass weights, removal of soft photons
      CALL PIATEK(CMSENE,TRMX2,AMEL,EMIN,DEL,
     $      NPHOT1,P1,P2,PHOT1,PHSU1,WTM1,WTT1,WTMR1,WCTA1,WCTB1)
      CALL PIATEK(CMSENE,TRMX2,AMEL,EMIN,DEL,
     $      NPHOT2,Q1,Q2,PHOT2,PHSU2,WTM2,WTT2,WTMR2,WCTA2,WCTB2)
      IF( KeyRem .EQ. 0 ) THEN
!-- Removing photons < epsCM from the record 
!-- Mass weight WTMR1,2 is product of mass weights for ENE>EminCM times
!-- average weight for photons with  ENE<EminCM.
        CALL REMPHO(EMIN,NPHOT1,PHOT1,AL1,BE1,WTM1,MK1)
        CALL REMPHO(EMIN,NPHOT2,PHOT2,AL2,BE2,WTM2,MK2)
! In future the KeyUpd -logics below has to be mooved into routine Piatek
        IF( KeyUpd .EQ. 0 ) THEN
          WT1= WTMR1*WKP
          WT2= WTMR2*WKQ
        ELSEIF( KeyUpd .EQ. 1) THEN
          WT1= WTMR1*WKP
          WT2= WKQ
          WCTA2 =1D0
          WCTB2 =1D0
        ELSEIF( KeyUpd .EQ. 2) THEN
          WT1= WKP
          WT2= WTMR2*WKQ
          WCTA1 =1D0
          WCTB1 =1D0
        ENDIF
      ELSEIF( KeyRem .EQ. 1) THEN
!-- No removal, valid but non-default option. 
        WT1= WTT1*WKP
        WT2= WTT2*WKQ 
      ELSE
        WRITE(nout,*) '+++++BHLUM4:  wrong KeyRem=',KeyRem
        STOP
      ENDIF
!--------------------------------------------------------------------
!  Monitoring control weights
      CALL Gmonit(   0,IDC+1,       WCTA1, 1D0,5D-4)   
      CALL Gmonit(   0,IDC+2,       WCTA2, 1D0,5D-4)   
      CALL Gmonit(   0,IDC+3, WCTA1*WCTA2, 1D0,5D-4)        
      CALL Gmonit(   0,IDC+4,       WCTB1, 1D0,5D-4)   
      CALL Gmonit(   0,IDC+5,       WCTB2, 1D0,5D-4)   
      CALL Gmonit(   0,IDC+6, WCTB1*WCTB2, 1D0,5D-4)        
      WTM1T2 = WTMR1*WTMR2
      CALL Gmonit(   0,IDA+1,      WTM1T2,  2D0,5D-4) 
!--------------------------------------------------------------------
!  Calculate Formfactor  
      PDEL = DEL*BCUD(P1,P2,PHSU1)
      QDEL = DEL*BCUD(Q1,Q2,PHSU2) 
      FYFSU= EXP( ALFPI*(
     $  -2.D0*(DLOG(TRANP/AMEL**2)-1)*DLOG(1D0/PDEL)
     $  +0.5D0*DLOG(TRANP/AMEL**2)  -1D0
     $    ))
      FYFSD= EXP( ALFPI*(
     $  -2.D0*(DLOG(TRANQ/AMEL**2)-1)*DLOG(1D0/QDEL)
     $  +0.5D0*DLOG(TRANQ/AMEL**2)  -1D0
     $    ))
      FPHS  = EXP( 2D0*ALFPI* LOG(TRMX2/AMEL**2)* LOG(1D0/DEL ) )
      IF(     KeyUpd .EQ. 0 ) THEN
        WT4 = FPHS**2  *FYFSU *FYFSD
      ELSEIF( KeyUpd .EQ. 1) THEN
        WT4 = FPHS  *FYFSU 
      ELSEIF( KeyUpd .EQ. 2) THEN
        WT4 = FPHS  *FYFSD
      ENDIF
      CALL Gmonit(   0,IDA+4,WT4,  1D0,5D-4)  
!--------------------------------------------------------------------
!  Calculate QED Matrix element
      CALL Model(1,WT5)
!--------------------------------------------------------------------
!  Define Total Principal Weight
  140 CONTINUE      
      WT  = WT0*WT1*WT2*WT3*WT4   *WT5
!-- Monitoring model weight       
      CALL Gmonit(   0,IDA+20,WT,WTmax,RN)
      WTovr = MAX(0D0,WT-WTMAX)
      CALL Gmonit(   0,IDA+18,  WTovr,0D0,0D0)
      WTneg = MIN(WT,0D0)
      CALL Gmonit(   0,IDA+19,  WTneg,0D0,0D0)
      CALL GF1( 9001, WT,1D0)
!--------------------------------------------------------------------
!  Optional Rejection according to principal weight  
      IF(KeyWgt .EQ. 0) THEN  
!-- Constant weihgt events WT=1 
            RN = DRVEC(2)
            CALL Gmonit(  0, IDGEN, SIG0NB*WTMAX, WTMAX,1D0)
            IF(WT.LT.RN*WTmax) GOTO 200
            WTMOD=1.D0 
            WTCRU1=1D0  
            WTCRU2=1D0
!-- Precaution measure
            DO i=1,300
               WTSET(i)=0
            ENDDO
         ELSE 
!-- Weighted events  
            WTCRU1= WT0*WT1*WT2
            WTCRU2=     WT3*WT4
            WTMOD  = WTCRU1*WTCRU2*WT5
            CALL Gmonit(  0, IDGEN, SIG0NB, WTMAX,1D0)
         ENDIF
!
!--------------------------------------------------------------------
!  Merge photons/fermions into single common block
      CALL Mergik
!
!     ===========
      ELSE 
!     ===========
      npar(10)= nevgen  
      npar(20)= nevgen   
      CALL Gmonit(   1,ida+20,awtot,dwtot,wwmx )
      xsmc   = sig0  *averwt 
      xsmcnb = sig0nb*averwt 
      erel   = errela 
      ermc   = xsmcnb*erel 
      xpar(10) = xsmcnb
      xpar(11) = erel
      xpar(12) = xsmc 
      IF(KeyWgt .EQ. 0) THEN 
!-- WT=1  events, normal option... 
         xpar(20)=xsmcnb 
         xpar(21)=erel 
         xpar(22)=xsmc
      ELSE
!-- Weighted events, additional information on x-sections 
         xpar(20)= sig0nb
         xpar(21)= 0d0  
         xpar(22)= sig0
      ENDIF    

! printout only for MODE=2
      IF(MODE.EQ.1) RETURN    
!
      CALL Gprint(9001) 
!
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHLUM4:        WINDOW A        '
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
! Print additional infos                  
!------------------------------------------------------------       
      CALL Gmonit(   1,IDA+1, AWT1 ,DWT1 ,DUMM3)                  
      CALL Gmonit(   1,IDA+2, AWT2 ,DWT2 ,DUMM3)         
      CALL Gmonit(   1,IDA+3, AWT3 ,DWT3 ,DUMM3)         
      CALL Gmonit(   1,IDA+4, AWT4 ,DWT4 ,DUMM3) 
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHLUM4:        WINDOW B        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL2F) AWT1,DWT1  ,'WT1*WT2*T/TP*T/TQ  ','      ','B1'
      WRITE(NOUT,BXL2F) AWT3,DWT3  ,'WT3 from KINO4     ','      ','B2'
      WRITE(NOUT,BXL2F) AWT4,DWT4  ,'YFS formfac        ','WT    ','B4'
      WRITE(NOUT,BXL2F) AWTOT,DWTOT,'TOTAL              ','      ','B5'
      CALL Gmonit(   1,IDA+18, AWT18 ,RWT18 ,DUMM3)     
      XWT18 = AWT18/AWTOT
      DWT18 = XWT18*RWT18
      WRITE(NOUT,BXL2F) XWT18,DWT18,'xsec/xtot: WT>WTMAX','WT    ','B6'
      CALL Gmonit(   1,IDA+19, AWT19 ,RWT19 ,DUMM3)     
      XWT19 = AWT19/AWTOT
      DWT19 = XWT19*RWT19
      WRITE(NOUT,BXL2F) XWT19,DWT19,'xsec/xtot: WT<0    ','WT    ','B7'
      WRITE(NOUT,BXCLO)  
! ---------------------------------------------------------------
      CALL Gmonit( 1,IDC+1,AWT1,DWT1,DUMM3)                            
      CALL Gmonit( 1,IDC+2,AWT2,DWT2,DUMM3)          
      CALL Gmonit( 1,IDC+3,AWT3,DWT3,DUMM3)          
      CALL Gmonit( 1,IDC+4,AWT4,DWT4,DUMM3)          
      CALL Gmonit( 1,IDC+5,AWT5,DWT5,DUMM3)          
      CALL Gmonit( 1,IDC+6,AWT6,DWT6,DUMM3)          
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
!     =====
      END  

      SUBROUTINE FILBH2(XPAR,NPAR) 
!     **************************** 
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
      COMMON / BHPAR3 / KeyRad,KeyOpt
      COMMON / BHPAR2 / CMSENE,AMEL  
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS 
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
! Communicates with VarRan
      COMMON / RANPAR / KeyRnd
      COMMON / INOUT  / NINP,NOUT
      real *4 enq,st2,der,errder,deg,errdeg
      SAVE   / INOUT  /, / RANPAR /
      SAVE   / TRANSR /, / BHPAR3 /, / BHPAR2 / , / BHPAR1 /, / BHPARZ /
!--
      CMSENE=XPAR(1)
      TRMIN =XPAR(2)
      TRMAX =XPAR(3)
      EPSCM =XPAR(4)
      KeyOpt=NPAR(1)
      KeyRnd = MOD(KeyOpt,10)   
      KeyWgt = MOD(KeyOpt,100)/10   
      KeyRem = MOD(KeyOpt,1000)/100   
      KeyRad=NPAR(2)
      KeyPia = MOD(KeyRad,10)
      KeyMod = MOD(KeyRad,100)/10 
      KeyUpd = MOD(KeyRad,1000)/100 
      KeyZet = MOD(KeyRad,10000)/1000 
!--
      AMEL  =  0.5111D-3
      SVAR  = CMSENE**2
      XIMIN = TRMIN/SVAR
      XIMAX = TRMAX/SVAR
!&&&&&&& varius choices adjusted manualy
      DEL   = EPSCM*0.01D0  ! This one seems to be optimal
!&    DEL   = EPSCM*0.1D0
!&    DEL   = EPSCM*XIMIN
!&&&&&&&
      IF(TRMAX.GT.SVAR) TRMAX=SVAR
! Inputs for Z correction   
      AMAZ  = 91.187d0
      GAMMZ =  2.490d0
      SINW2 = 0.2319d0
      GA = -1/(4*DSQRT(SINW2*(1-SINW2)))
      GV = GA*(1-4*SINW2)
! Test of vacuum correction 
      CALL vacpol(KeyPia,-trmin,SINW2,RePi1,dRePi1)
      CALL vacpol(KeyPia,-trmax,SINW2,RePi2,dRePi2)

!--
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHLUM4: INPUT PARAMETRES       '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL1I) KeyOpt,     ' OPTIONS   switch  ','KeyOpt','  '
      WRITE(NOUT,BXL1I) KeyRnd,     ' rand. numb. switch','KeyRnd','  '
      WRITE(NOUT,BXL1I) KeyWgt,     ' weighting switch  ','KeyWgt','  '
      WRITE(NOUT,BXL1I) KeyRem,     ' photon removal  sw','KeyRem','  '
      WRITE(NOUT,BXL1I) KeyRad,     ' RADIATION switch  ','KeyRad','  '
      WRITE(NOUT,BXL1I) KeyPia,     ' vac_pol   switch  ','KeyPia','  '
      WRITE(NOUT,BXL1I) KeyMod,     ' QED mat. elm. type','KeyMod','  '
      WRITE(NOUT,BXL1I) KeyUpd,     ' Test switch, def=0','KeyUpd','  '
      WRITE(NOUT,BXL1I) KeyZet,     ' Z contribution    ','KeyZet','  '
      WRITE(NOUT,BXL1F) CMSENE,     ' CMS energy  [GeV] ','CMSENE','  '
      WRITE(NOUT,BXL1G) SVAR  ,     ' CMSENE^2  [GeV^2] ','SVAR  ','  '
      WRITE(NOUT,BXL1G) TRMIN ,     ' trasf_min [GeV^2] ','TRMIN ','  '
      WRITE(NOUT,BXL1G) TRMAX ,     ' trasf_max [GeV^2] ','TRMAX ','  '
      WRITE(NOUT,BXL1G) XIMIN ,     ' xi_min=TRMIN/SVAR ','XIMIN ','  '
      WRITE(NOUT,BXL1G) XIMAX ,     ' xi_max=TRMAX/SVAR ','XIMAX ','  '
      THMIN  = ACOS(1-2*XIMIN)*1000D0
      THMAX  = ACOS(1-2*XIMAX)*1000D0
      WRITE(NOUT,BXL1F) THMIN ,     ' theta_min  [mrad] ','THMIN ','  '
      WRITE(NOUT,BXL1F) THMAX ,     ' theta_max  [mrad] ','THMAX ','  '
      THMIN  = ACOS(1-2*XIMIN)*180/PI
      THMAX  = ACOS(1-2*XIMAX)*180/PI
      WRITE(NOUT,BXL1F) THMIN ,     ' theta_min   [deg] ','THMIN ','  '
      WRITE(NOUT,BXL1F) THMAX ,     ' theta_max   [deg] ','THMAX ','  '
      WRITE(NOUT,BXL1G) EPSCM ,     ' eps_CM infr. cut  ','EPSCM ','  '
      WRITE(NOUT,BXL1G) DEL   ,     ' delta  infr. cut  ','DEL   ','  '
      WRITE(NOUT,BXL1F) REPI1 ,     ' RePi(transf_min)  ','REPI1 ','  '
      WRITE(NOUT,BXL1F) dREPI1,     ' error             ','dREPI1','  '
      WRITE(NOUT,BXL1F) REPI2 ,     ' RePi(transf_max)  ','REPI2 ','  '
      WRITE(NOUT,BXL1F) dREPI2 ,     'error             ','dREPI2','  '
      WRITE(NOUT,BXL1F) AMAZ  ,     ' Z-mass GeV        ','AMAZ  ','  '
      WRITE(NOUT,BXL1F) GAMMZ ,     ' Z-width GeV       ','GAMMZ ','  '
      WRITE(NOUT,BXL1F) SINW2 ,     ' weak mixing angle ','SINW2 ','  '
      WRITE(NOUT,BXCLO)  
      END 

      SUBROUTINE REMPHO(EMIN,NPHOT,PHOT,ALF,BET,WTM,MK)
!     *************************************************
      IMPLICIT REAL*8(A-H,O-Z)       
      DIMENSION PHOT(100,4),ALF(50),BET(50),WTM(50),MK(50)
!
      if(nphot.eq.0) return
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
!     ***************************************************************
! Input:
!        CMSENE         CMS energy
!        TRMAX          maximum transfer (dummy)
!        AMEL           electron mass (GeV)
!        EMIN           CMS minimum photon energy (GeV)
!        DELTA          MC minimum photon energy (dimensionless)
!        NPHOT          photon number
!        P1,P2(4)       fermion momenta
!        PHOT(100,4)    photon four-momenta
!        PHSU(50)       sum of photon four-momenta
!        WMAT(50)       mass weights from MLTIBR
! Output:
!        WTAL       mass weight for all photons
!        WTMRE      In the case of removal the new mass weight
!        WCTR1      Control weight for delta-->epsilon rejection
!        WCTR2      control weight for photons below EMIN removal
!     ************************
      IMPLICIT REAL*8(A-H,O-Z)  
      PARAMETER(PI=3.1415926535897932D0,ALFINV=137.03604D0)
      PARAMETER(ALF1=1/ALFINV/PI )
      DIMENSION P1(4),P2(4),WMAT(50),PHOT(100,4),PHSU(4)
      DATA ICONT /0/
      ICONT=ICONT+1
! Calculate mass weight for all photos and separately for
! photons with energy below/above EMIN
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
        WTM2=WTM2*WMAT(I)
        IF(WTM2.LT.1D-15) WTM2=0D0
      ENDIF
  100 CONTINUE   
! Control weight for delta-->epsilon  'REJECTION'
      DELT1 = DELTA*BCUD(P1,P2,PHSU)
      CALL WFORM(TRMAX,P1,P2,AMEL,DELT1,EMIN,PDYFS)
      WCTR1 = WTEPSP*PDYFS   
! control weight for photons ENE<EMIN  'REMOVAL'
      TRANP = 2D0*(P1(4)*P2(4)-P1(3)*P2(3)-P1(2)*P2(2)-P1(1)*P2(1))
      EPS1  =  SQRT(EMIN**2/P1(4)/P2(4))     
      DELB2 = -2*ALF1*(DLOG(TRMAX/TRANP)+1) *DLOG(EPS1/DELT1)
      WCTR2 = WTM1*EXP(-DELB2)   
! In the case of removal the new mass weight is this
      WTMRE = WTM2*EXP(DELB2)
      END

      SUBROUTINE WFORM(TRMAX,Q1,Q2,AMF,DELTA,EMIN,DYFS)
!     *************************************************  
! For tests only.     
! Yennie-Frautschi-Suura Formfactors for the single fermion pair 
! This is for crude distribition before mass wights
! The triangle effect included (pi**2/6)
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(PI=3.1415926535897932D0,ALFINV=137.03604D0)
      PARAMETER(ALF1=1/ALFINV/PI )
      DIMENSION Q1(4),Q2(4)       
! ...Momenta q1,q2 should be in CMS
      Q1Q2  = Q1(4)*Q2(4)-Q1(3)*Q2(3)-Q1(2)*Q2(2)-Q1(1)*Q2(1) 
      E1 = Q1(4) 
      E2 = Q2(4)
      BETF2 = 2*ALF1* DLOG(TRMAX /AMF**2) 
      DELB  = BETF2*DLOG(EMIN/SQRT(E1*E2)/DELTA)
      EP    = E1+E2
      EM    = E1-E2  
      DL    = SQRT( 2*Q1Q2 +EM**2 )     
! Note that approximately REMN= +(1./6.)*PI**2 for t-channel
      REMN  = PI**2/2 
     $        -0.50*DLOG(E1/E2)**2  
     $        -0.25*DLOG((DL+EM)**2/(4*E1*E2))**2 
     $        -0.25*DLOG((DL-EM)**2/(4*E1*E2))**2    
     $        - DILOGY((DL+EP)/(DL+EM)) -DILOGY((DL-EP)/(DL-EM))
     $        - DILOGY((DL-EP)/(DL+EM)) -DILOGY((DL+EP)/(DL-EM)) 
! This (alf/pi)*pi**2/6 is related to replacing (y+z)>epsilon
! by max(y,z)>epsilon.   (Rejection delta=> epsilon over-estimated)
      TRIANG = -PI**2/6D0 
      DYFS   = EXP( DELB +ALF1*REMN +ALF1*TRIANG) 
      END

      FUNCTION BCUD(P1,P2,SF)
!     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P1(4),P2(4),SF(4)
      XPP  = P2(4)*P1(4)-P2(3)*P1(3)-P2(2)*P1(2)-P2(1)*P1(1)
      XPR  = P1(4)*(P2(4)+SF(4)) - P1(3)*(P2(3)+SF(3))
     $     - P1(2)*(P2(2)+SF(2)) - P1(1)*(P2(1)+SF(1))
      BCUD= XPR/XPP
      END

      SUBROUTINE MLTIBR(TRAN,TRMAX,AMEL,DEL,
     $      NPH,PHOT,PHSU,ALF1,BET1,TRANP,AMSP,MK,WT1,WTM)   
!     ****************************************************   
! This provides momenta of photons in a fermion proper frame 
! Input : TRAN    = principal t-channel transfer     (GEV**2)
!         TRMAX   = max. transf. (>0) for angular phot. dist. [GEV**2]
!         AMEL    = electron energy         (GEV)
!         DEL     = low energy photon limit   (dimensionless)
! Output: NPH     = photon multiplicity
!         PHOT    = list of photon four-momenta
!         PHSU    = sum of photon momenta
!         ALF1,BET1   = Sudakov variables
!         TRANP   = (P2-P1)**2
!         AMSP    = (P2+PHSU)**2  
!         MK      = marked photons
!         WT1     = TRANP/TRAN is Jacobian, =0 outside ph.sp.
!         WTM     = list of mass weights
!     ************************
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
! No photon        
      DO 45 K=1,4
   45 PHSU(K)=0D0
      IF(NPH.EQ.0) THEN 
        TRANP=TRAN  
      ELSE  
! One or more photons 
   50   CALL VarRan(DRVEC,NPH) 
        BSUM=0D0 
        DO 80 I=1,NPH  
! We define R=LOG(MAX(YGR,ZET)) 
        R=DELL*RR(I) 
! Photons close to lower infrared boundry are marked for tests 
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
! Define ALPHA and BETA (prim)
        ALF1(I)=YGR-ZET*DELS
        BET1(I)=ZET-YGR*DELS 
        IF(ALF1(I).LE.0D0.OR.BET1(I).LE.0D0) GOTO 50
   80   BSUM=BSUM+BET1(I)   
        IF(BSUM.GT.1D0) GOTO 800
! Rescale ALPHA and BETA        
        CALL VarRan(DRVEC,NPH) 
        DO 90 I=1,NPH 
        ALF(I)=ALF1(I)/(1D0-BSUM) 
   90   BET(I)=BET1(I)/(1D0-BSUM)  
! Define photon four momenta in SQRT(TRANP)/2 units 
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
! Define factor for rescaling photon momenta    
        XMK2=PHSU(4)**2-PHSU(3)**2-PHSU(2)**2-PHSU(1)**2 
        YY2=1D0/(1D0+PHSU(3)-.25D0*XMK2)      
! YY2 negative when outside phase space (abs(t)>abs(s))
        IF(YY2.LE.0D0) GOTO 900 
        TRANP=TRAN*YY2      
        ENER =SQRT(TRANP)/2D0        
! RESCALE ALL PHOTON MOMENTA         
        DO 120 K=1,4        
        PHSU(K)=PHSU(K)*ENER         
        DO 120 I=1,NPH      
  120   PHOT(I,K)=PHOT(I,K)*ENER     
! This rotation makes PHSU(2)=0 
! (we get rid her of dummy angle, see 'poprawka' in the notes)
        PSIT=ANGFI(PHSU(1),PHSU(2))  
        CALL ROTOD3(-PSIT, PHSU,PHSU)         
        DO 140 I=1,NPH      
        DO 135 K=1,4        
  135   PH(K)=PHOT(I,K)     
        CALL ROTOD3(-PSIT, PH,PH)    
        DO 140 K=1,4        
  140   PHOT(I,K)=PH(K)     
      ENDIF         
!+++      IF(TRANP.EQ.0D0) GO TO 900      
      IF(TRANP.LE. 4*AMEL**2) GO TO 900      
      BETEL=SQRT(1D0-4D0*AMEL**2/(TRANP+4D0*AMEL**2))          
      P2(3)=SQRT(TRANP)/2D0          
      P2(4)=SQRT(P2(3)*P2(3)+AMEL**2)         
      P2(2)=0D0     
      P2(1)=0D0     
      AMSP=(P2(4)+PHSU(4))**2-(P2(3)+PHSU(3))**2      
     $    -(P2(2)+PHSU(2))**2-(P2(1)+PHSU(1))**2      
! And the weight finally    
      WT1 = TRANP/TRAN  
      DELT=AMEL**2/TRANP    
      DO 200 I=1,NPH        
! Generated distribution         
! here some numerical regularization  
      DIST0 = 1D0/((ALF1(I)+DELS*BET1(I))*(BET1(I)+DELS*ALF1(I)))
      YGR=ALF1(I)+DELT*BET1(I)
      ZET=BET1(I)+DELT*ALF1(I)       
! Desired distribution = soft factor
      DIST1 = ALF1(I)*BET1(I)/(YGR*ZET)**2
      WTM(I)= DIST1/DIST0     
  200 CONTINUE
      RETURN 
! Event outside phase space
! Note that distinction is made (TRANP=-2,-1) to facilitate tests 
! event dropped due to: sum(beta) > 1
 800  CONTINUE
      TRANP = -1D0
      WT1   =  0D0
      RETURN
! event dropped due to: tranp < m^2, or earlier because YY2 < 0
 900  CONTINUE
      TRANP = -2D0
      WT1   =  0D0     
      END  
  
      SUBROUTINE POISSG(AVERG,NMAX,MULT ,RR)      
!     **************************************
! DIFFERS FROM THAT IN EXPAND DEC. 87         
! THIS GENERATES PHOTON MULTIPLICITY ACCORDING TO POISSON DISTRIBUTION
! INPUT:  AVERG = AVERAGE MULTIPLICITY        
!         NMAX  = MAXIMUM MULTIPLICITY        
! OUTPUT: MULT  = GENERATED MULTIPLICITY      
!         RR(1:100) LIST OF ORDERED UNIFORM RANDOM NUMBERS,    
!         A BYPRODUCT RESULT, TO BE EVENTUALLY USED FOR SOME FURTHER  
!         PURPOSE (I.E.  GENERATION OF PHOTON ENERGIES).       
!     **************************************      
      IMPLICIT REAL*8(A-H,O-Z)       
      REAL*8 RR(100)        
      DOUBLE PRECISION DRVEC(100)
      SAVE NFAIL
      DATA NFAIL/0/         
   50 NN=0    
      IF(NMAX.GT.100) GOTO 900       
      CALL VarRan(DRVEC,NMAX)
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
!     ************************************************
! Kinematics, cnstruction of momenta in CMS   
!     ************************************************
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
! Three azimuthal angles        
      CALL VarRan(DRVEC,3)      
      PSI1= 2D0*PI*DRVEC(1)
      PSI2= 2D0*PI*DRVEC(2)
      PHI = 2D0*PI*DRVEC(3)
! Upper vertex: transf. from P2-P1 proper frame       
      CALL KLIPER(TRANP,AMEL,PHSU1,P2,TH1,EXT1,EXB1)  
! Lower vertex: trascf. from q2-q1 proper frame       
      CALL KLIPER(TRANQ,AMEL,PHSU2,Q2,TH2,EXT2,EXB2)  
! Define P1, Q1  in central QMS      
      P1(3)= -(TRAN+AMSP-AMEL**2)/SQRT(TRAN)/2D0      
      Q1(3)=  (TRAN+AMSQ-AMEL**2)/SQRT(TRAN)/2D0      
      RPQK=(Q1(3)+P1(3))/DSQRT(SVAR) 
! Correcting for electron mass       
!     PX2=SVAR*(SVAR+4D0*P1(3)*Q1(3))/((Q1(3)+P1(3))**2+SVAR)/4D0
!     PX2=PX2-AMEL**2       
      GPQK= P1(3)-Q1(3)     
      PX2=(BTEL**2*SVAR*(1D0+RPQK*RPQK)-GPQK*GPQK)/(1D0+RPQK*RPQK)/4D0
      IF(PX2.LE.0D0)  GOTO 900       
      PX=SQRT(PX2)  
      P1(2)=  0D0   
      Q1(2)=  0D0   
      P1(1)=  -PX   
      Q1(1)=   PX   
      P1(4)=  SQRT(P1(1)**2+P1(2)**2+P1(3)**2+AMEL**2)         
      Q1(4)=  SQRT(Q1(1)**2+Q1(2)**2+Q1(3)**2+AMEL**2)         
! Correcting for electron mass       
!     BETP = SQRT(1D0-(AMEL/P1(4))**2)        
!     BETQ = SQRT(1D0-(AMEL/Q1(4))**2)        
!     DO 7 K=1,3    
!     P1(K)=BETP* P1(K)     
!   7 Q1(K)=BETQ* Q1(K)     
      EXW1=SQRT((P1(4)+P1(1))/(P1(4)-P1(1)))  
      EXW2=SQRT((Q1(4)+Q1(1))/(Q1(4)-Q1(1)))  
! Construct momentum transfer Q in CMS        
      QCM(4)=(AMSP-AMSQ)/SQRT(SVAR)/2D0       
      QMOD=SQRT(TRAN+QCM(4)**2)      
      QCM(3)=(-TRAN-AMSP/2D0-AMSQ/2D0+AMEL**2)/SQRT(SVAR-4D0*AMEL**2)
      QCM(2)=0D0    
      QCM(1)=SQRT(QMOD**2-QCM(3)**2) 
      FIF =ANGFI(QCM(3),QCM(1))      
      EXE2=SQRT((QMOD+QCM(4))/(QMOD-QCM(4)))  
  
! Final set of transformations from QMSP and QMSQ to CMS       
! First branch, tranformed are P2, PHSU1, PHOT1
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
! Second branch, tranformed are Q2, PHSU2, PHOT2
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
! Finally, beams P1 and Q1   
      CALL BOSTD3(EXE2,P1,P1)        
      CALL ROTOD2( FIF,P1,P1)        
      CALL BOSTD3(EXE2,Q1,Q1)        
      CALL ROTOD2( FIF,Q1,Q1)        
      RETURN        
! Event outside phase space          
  900 WTKK=0D0      
      END  
  
      SUBROUTINE PTRAL(TH,EXT,EXB,PSI,EXW,EXE,FIF,PHI,P)       
!     **************************************************       
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
!     **************************************************       
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
!     **************************************************
! Deals with Lorentz transf. from QQ1 to QQ frame
! where QQ1=P2-P1, QQ=P2+PHSUM-P1, TRANP=QQ1**2, P1**2=P2**2=AMEL**2 
! Input: TRANP,AMEL,PHSUM
! Output: P2,TH,EXT,EXB,PHSUM
! Here, TH, EXT, EXB are transformation params.
!     **************************************************
      IMPLICIT REAL*8(A-H,O-Z)       
      REAL*8 PHSUM(4),P2(4)          
      REAL*8 P1(4),QQ1(4)   
  
      BETEL=SQRT(1D0-4D0*AMEL**2/(TRANP+4D0*AMEL**2))          
! No photon         
      IF(PHSUM(4).EQ.0D0) THEN       
        P2(3)= SQRT(TRANP)/2D0       
        P2(4)= SQRT(P2(3)*P2(3)+AMEL**2)      
        P2(2)=0D0   
        P2(1)=0D0           
        TH =0D0         
        EXT=1D0         
        EXB=1D0         
      ELSE  
! One photon or more    
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
   
! Rotation 2 puts QQ1 paralel to axis 3   
! Note that  PHSUM(2)=0 is already assured in MLTIBR!
        TH  =ANGFI(QQ1(3),QQ1(1))         
        CALL ROTOD2(-TH ,QQ1,QQ1)         
        CALL ROTOD2(-TH ,P1,P1)  
! Boost 3 puts QQ1(4)=0          
        EXT = SQRT((QQ1(3)-QQ1(4))/(QQ1(3)+QQ1(4)))   
        CALL BOSTD3( EXT ,QQ1,QQ1)        
        CALL BOSTD3( EXT , P1, P1)        
        EXB = SQRT((P1(4)-P1(1))/(P1(4)+P1(1)))       
!  Testing obsolete appendix
!  Boost 1 puts P1 antiparallel to axis 3  
!       CALL ROTOD2( -TH , P2, P2)
!       CALL BOSTD3( EXT , P2, P2)
!       CALL BOSTD1( EXB , P2, P2)
      ENDIF    
      END   

      SUBROUTINE MERGIK
!     *****************
! Transfer momenta and mark into proper commons        
! photons ordered according to cms energy 
! (the hardest in the first position)
!     ************************
      IMPLICIT REAL*8(A-H,O-Z)       
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / MOMSET / PX1(4),QX1(4),PX2(4),QX2(4),PHOT(100,4),NPHOT
      COMMON / MARPKP / MARKP(100) 
      SAVE   / MOMS1/,/ MOMS2/,/ MOMZ1/,/ MOMZ2/,/ MOMSET/,/ MARPKP /
      NPHOT=NPHOT1+NPHOT2   
      I1=1 
      I2=1 
      DO 207 I=1,NPHOT      
      IF(PHOT1(I1,4).GT.PHOT2(I2,4)) THEN     
         DO 205 K=1,4       
  205    PHOT( I,K)=PHOT1(I1,K)      
         MARKP(I)  =  MK1(I1)        
         I1=I1+1    
      ELSE 
         DO 206 K=1,4       
  206    PHOT( I,K)=PHOT2(I2,K)      
         MARKP(I)  =  MK2(I2)        
         I2=I2+1    
      ENDIF         
  207 CONTINUE
      DO 300 K=1,4
      PX1(K)=P1(K)
      PX2(K)=P2(K)
      QX1(K)=Q1(K)
      QX2(K)=Q2(K)
  300 CONTINUE
      END      


      SUBROUTINE DUMPR(NUNIT,WORD,PP,QQ)    
!     **********************************
! 15 Jan 90 (SJ)
! prints twice dot-products of two four momentum PP and QQ
! more precisely:   2*PP.QQ  and  (PP+QQ).(PP+QQ)
!     ************************   
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8 WORD          
      REAL*8 PP(4),QQ(4)  
      DOT1=2*(PP(4)*QQ(4)-PP(3)*QQ(3)-PP(2)*QQ(2)-PP(1)*QQ(1))
      DOT2=(PP(4)+QQ(4))**2-(PP(3)+QQ(3))**2
     $    -(PP(2)+QQ(2))**2-(PP(1)+QQ(1))**2
      WRITE(NUNIT,'(1X,A8,5(1X,F20.10))') WORD,DOT1,DOT2        
      END  


      SUBROUTINE DUMPS(NOUT)     
!     **********************     
! THIS PRINTS OUT FOUR MOMENTA OF PHOTONS 
! ON OUTPUT UNIT NOUT
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


      SUBROUTINE BHLUMI(MODE,XPAR,NPAR)        
*     *********************************
!
!
!   BBBBBBB    BBB   BBB  BBB      BBB  BBB  BBB     BBB   BBB
!   BBB  BBB   BBB   BBB  BBB      BBB  BBB  BBBB   BBBB   BBB
!   BBB  BBB   BBB   BBB  BBB      BBB  BBB  BBBBB BBBBB   BBB
!   BBBBBB     BBBBBBBBB  BBB      BBB  BBB  BBB BBB BBB   BBB
!   BBBBBBBBB  BBBBBBBBB  BBB      BBB  BBB  BBB  B  BBB   BBB
!   BBB  BBBB  BBB   BBB  BBB  BB  BBB  BBB  BBB     BBB   BBB
!   BBBBBBBBB  BBB   BBB  BBBBBBB  BBB  BBB  BBB     BBB   BBB
!   BBBBBBBB   BBB   BBB  BBBBBBB   BBBBBB   BBB     BBB   BBB
!
!
!======================================================================
!======================================================================
!======================================================================
!===============             B H L U M I            ===================
!======================================================================
!======================================================================
!=============== MONTE CARLO FOR SMALL ANGLE BHABHA ===================
!===============            VERSION 4.04            ===================
!======================================================================
!===============    July 1995 - Sept. 1996          ===================
!======================================================================
!======================================================================
!=======================     AUTHORS      =============================
!== S. Jadach, W. Placzek, E. Richter-Was, B.F.L. Ward  and Z. Was   ==
!======================================================================
!======================================================================
!           
! The description of the usage of the program can be found
! in refs. [1] and [3].
!
! The program contains three subgenerators:
!   (1)  BHLUM4 
!    Multiphoton generator with Yennie-Frautschi-Suura SECOND order 
!    exponentiation based on refs. [1-7]
!   (2)  LUMLOG
!    Leading-Logarithmic generator with collinear emission of photons,
!    QED matrix element up to third order described in refs. [1-2],[3],[8]
!   (3)  OLDBIS
!    Modified version of the first order clasical generator OLDBAB,
!    see refs. [9-10]
!
! [1] S. Jadach, W. Placzek, E. Richter-Was, B.F.L. Ward  and Z. Was,
!     CERN preprint TH-96-158, June. 1996, submitted to Comp. Phys. Commun.
! [2] S. Jadach and B.F.L. Ward,
!     CERN preprint TH-96-156, June. 1996,  Phys. Lett. in print.
! [3] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
!     Comp. Phys. Commun. 70 (1992) 305 (TH-6230, sept. 1991).
! [4] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
!     Phys.Lett. B268 (1991), 253 (TH-6118, June 1991). 
! [5] S. Jadach and B.F.L. Ward,
!     Phys. Rev. D40 (1989) 3582.
! [6] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
!     TH-95-38; Phys. Lett., B353 (1995) 362.
! [7] S. Jadach, W. Placzek and B.F.L. Ward
!     TH-95-74; Phys. Lett. B353 (1995) 349.
! [8] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was
!     Phys. Lett. B260 (1991) 438, TH-5995.
! [9] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was
!     Phys. Lett. B253 (1991) 469, TH-5888.
! [10] F. Berends and R. Kleiss Nucl. Phys. B228 (1983) 537.  
!
! Postscript files for the above and other papers 
! available from URL http://hpjmiady.ifj.edu.pl/programs/programs.html
!
!                  IMPORTANT NOTE
!                  --------------
! The user is kindly requested to cite at least refs. [1-7]
! and any other ones from the above list if his study depends strongly 
! on the particular subgenerator.
!  
!----------------------------------------------------------------------
!                        INPUT and OUTPUT
!----------------------------------------------------------------------
! All input and output goes through parameters in 
!                 CALL BHLUMI(MODE,XPAR,NPAR)
! and through /MOMSET/ and /WGTALL/ common blocks.
! In the following we shall  briefly indicate the meaning of the
! above parameters/variables.
!
! IF( MODE =-1 ) THEN
! ===================
! Initialisation is performed, all input parameters are transfered
! through XPAR and NPAR.
! The meaning of the entries in XPAR and NPAR depends on the type of 
! the subgenerator: 
!          see tables in the source of BHLUM4.f
!          ------------------------------------ 
! and also in OLDBIS, LUMLOG and the tables in the Long-write-up. 
! In the following table we indicate very briefly
! parameters which have the same meaning for all three subgenerators
!----------------------------------------------------------------------
!  Entry    Variable   Meaning
!----------------------------------------------------------------------
!  NPAR( 1)  KEYOPT =1000*KEYGEN +10*KEYWGT +KEYRND 
!                   General option switch 
!            KEYGEN =1,2,3 for OLDBIS,LUMLOG,BHLUM4 sub-generators
!            KEYRND =1,2 type of random number generator RANMAR,RANECU
!            KEYWGT =0,1,2 for constant/variable weight WTM,
!                    see more details in tables in BHLUM4 and LUMLOG
!  NPAR( 2)  KEYRAD is QED option switch defining WTMOD weight
!                   see tables in BHLUM4, LUMLOG and OLDBIS
!  XPAR( 1)  CMSENE Total center mass energy [GeV]
!  XPAR( 2)         see tables in BHLUM4, LUMLOG and OLDBIS 
!  XPAR( 3)         see tables in BHLUM4, LUMLOG and OLDBIS 
!  XPAR( 4)         see tables in BHLUM4, LUMLOG and OLDBIS 
!  XPAR( 5)         see tables in BHLUM4, LUMLOG and OLDBIS 
!  XPAR( 6)         see tables in BHLUM4, LUMLOG and OLDBIS 
!----------------------------------------------------------------------
!
! ELSE IF( MODE = 0 ) THEN
! ========================
! Generation of the single Monte Carlo event
! The four momenta of the final state electron positron and photon
! are encoded in 
!      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
! where P1 and Q1 are four-momenta of positron and electron beams, 
! P2 and Q2 are four-momenta of outgoing positron and electron [GeV],  
! PHOT(100,4) contains list of photon four-momenta
! and NPHOT is the number of real photons in PHOT.  
! For weighted events it may be profitable to use 'paralel weights' 
! from
!      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)   
! where WTMOD is the principal model weight and another useful weights
! representing some interesting version of the QED matrix element
! can be constructed as WT= WTCRU1*WTCRU2*WTSET(J). 
! Which J is alowed and what version of the matrix element it 
! represents depends on the type of subgenerator (BHLUM4,OLDBIS,LUMLOG)
! and may be found in corresponding Tables of this source 
! code  and in Long-write-up.
! 
! ELSE IF( MODE = 1 ) THEN
! ========================
! The total cross section corresponding to generated series of event,
! i.e. resulting from MC integrartion is calculated and stored in XPAR
! and NPAR. 
! In the table below we describe their most essential entries.
! For describtion of the other entries see tables in the source code
! of the subgenerators BHLUM4, LUMLOG and OLDBIS and in Long-write-up.
!----------------------------------------------------------------------
!  Entry    Variable   Meaning
!----------------------------------------------------------------------
!  NPAR(10)  NEVGEN  Number of generated MC events
!  NPAR(20)  NEVGEN  Number of generated MC events
!  XPAR(10)    XSEC  Total x-section [nb]
!  XPAR(11)   RXSEC  The relative (statistical) error of XSEC
!  XPAR(20)          Crude total MC x-section [nb] which is necessary
!                    for rescaling histograms in run 
!                    with weighted events.
!  XPAR(21)          =0, error of XPAR(20) is zero
! ----------------------------------------------------------------------
! For constant weight option KEYWGT=0 (convevience in rescaling histos)
! we put XPAR(20,21)=XPAR(10,11).
! For MODE=1 program is typicaly called upon many times in the process of 
! rescaling histograms and therefore no output is printed.
! 
! ELSE IF( MODE = 2 ) THEN
! ========================   
! Only in this MODE=2 in addition to filling XPAR and NPAR 
! (as for MODE=1)
! the values of various x-sections are printed on the standard 
! output file.
!       
! ENDIF
! ====
!     ****************************************
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
      SAVE   NEVG, KEYGEN
   
      IF(MODE.EQ.-1) THEN        
*     ===================        
      NINP= 15   
CBB      NOUT= 16   
      nout = 6
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '   '
      WRITE(NOUT,BXTXT) 'BBB   B  B  B    B   B  B    B  B'
      WRITE(NOUT,BXTXT) 'B  B  B  B  B    B   B  BB  BB  B'
      WRITE(NOUT,BXTXT) 'BBB   BBBB  B    B   B  B BB B  B'
      WRITE(NOUT,BXTXT) 'B  B  B  B  B    B   B  B    B  B'
      WRITE(NOUT,BXTXT) 'BBB   B  B  BBBB  BBB   B    B  B'
      WRITE(NOUT,BXTXT) '   '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '*   BHLUMI version 4.04         *'
      WRITE(NOUT,BXTXT) '*   June           1991  (2.01) *'
      WRITE(NOUT,BXTXT) '*   Sept           1992  (2.02) *'
      WRITE(NOUT,BXTXT) '*   January        1995  (4.00) *'
      WRITE(NOUT,BXTXT) '*   Febuary        1995  (4.01) *'
      WRITE(NOUT,BXTXT) '*   May            1995  (4.02) *'
      WRITE(NOUT,BXTXT) '*   July           1995  (4.02a)*'
      WRITE(NOUT,BXTXT) '*   June           1996  (4.03) *'
      WRITE(NOUT,BXTXT) '*   September      1996  (4.04) *'
      WRITE(NOUT,BXTXT) '*         AUTHORS               *'
      WRITE(NOUT,BXTXT) '* S. Jadach,      W. Placzek,   *'
      WRITE(NOUT,BXTXT) '* E. Richter-Was, B.F.L. Ward,  *'
      WRITE(NOUT,BXTXT) '*        and Z. Was             *'
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXCLO)  
!
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) 'This program is based on papers '
      WRITE(NOUT,BXTXT) '--------------------------------'
      WRITE(NOUT,BXTXT) 'Phys. Lett. B353 (1995) 362     '
      WRITE(NOUT,BXTXT) 'Phys. Lett. B353 (1995) 349     '
      WRITE(NOUT,BXTXT) 'Comp. Phys. Comm. 70 (1992) 305 '
      WRITE(NOUT,BXTXT) 'Phys. Lett. B268 (1991) 253     '
      WRITE(NOUT,BXTXT) 'Phys. Rev.  D40  (1989) 3582.   '
      WRITE(NOUT,BXTXT) 'Phys. Lett. B260 (1991) 438,    '
      WRITE(NOUT,BXTXT) 'Phys. Lett. B253 (1991) 469,    '
      WRITE(NOUT,BXTXT) 'Nucl. Phys. B228 (1983) 537.    '
      WRITE(NOUT,BXCLO)  
!
      NEVG=0 
      KEYOPT=NPAR(1)
      KEYGEN = MOD(KEYOPT,10000)/1000
      IF(KEYGEN.EQ.0) CALL BHLUM4(  -1,XPAR,NPAR)   
      IF(KEYGEN.EQ.1) CALL OLDBIS(  -1,XPAR,NPAR)   
      IF(KEYGEN.EQ.2) CALL LUMLOG(  -1,XPAR,NPAR)   
      IF(KEYGEN.EQ.3) CALL BHLUM4(  -1,XPAR,NPAR)   
      IF(KEYGEN.EQ.4) CALL BHLOG4(  -1,XPAR,NPAR)   
      IF(KEYGEN.EQ.5) CALL BHLOG5(  -1,XPAR,NPAR)   

      ELSEIF(MODE.EQ.0) THEN
*     ======================
      NEVG=NEVG+1  
      IF(KEYGEN.EQ.0) CALL DUMGEN
      IF(KEYGEN.EQ.1) CALL OLDBIS(   0,XPAR,NPAR)   
      IF(KEYGEN.EQ.2) CALL LUMLOG(   0,XPAR,NPAR)   
      IF(KEYGEN.EQ.3) CALL BHLUM4(   0,XPAR,NPAR)   
      IF(KEYGEN.EQ.4) CALL BHLOG4(   0,XPAR,NPAR)   
      IF(KEYGEN.EQ.5) CALL BHLOG5(   0,XPAR,NPAR)   
! clean final state common blocks if necessary (safety reason)
      CALL BHCLEN

      ELSE 
!     ==== 
      IF(KEYGEN.EQ.0) CALL DUMGEN
      IF(KEYGEN.EQ.1) CALL OLDBIS(MODE,XPAR,NPAR)   
      IF(KEYGEN.EQ.2) CALL LUMLOG(MODE,XPAR,NPAR)   
      IF(KEYGEN.EQ.3) CALL BHLUM4(MODE,XPAR,NPAR)   
      IF(KEYGEN.EQ.4) CALL BHLOG4(MODE,XPAR,NPAR)   
      IF(KEYGEN.EQ.5) CALL BHLOG5(MODE,XPAR,NPAR)   
      ENDIF 
!     =====
      END

      SUBROUTINE BHCLEN
!     *****************
! This routine prevents user from using zero weight events 
! and parellel weights when they should not be used!
!     ************************
      IMPLICIT REAL*8(A-H,O-Z)   
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300) 
      SAVE   / MOMSET /, / WGTALL /

! Parallel weights should not be used for constant weight events.
      IF(WTMOD.EQ.1D0) THEN
       DO I=1,300
         WTSET(I)=0D0
       ENDDO
! Clean final state momenta for events outside phase space 
      ELSEIF(WTCRU1*WTCRU2 .EQ.0D0 )  THEN
       DO K=1,4
         P2(K)=0D0
         Q2(K)=0D0    
       ENDDO
       NPHOT=0
       DO J=1,100
         DO K=1,4
           PHOT(J,K)=0D0
         ENDDO
       ENDDO
      ENDIF
      END

      SUBROUTINE DUMGEN
!     *****************
! This routine is dummy generator for keygen=0.
! It may be usefull if generation is for some tests done directly
! in booking routine but input is the same as in BHLUM4
      END
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
CBB      nout=16
      nout=6
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
      SUBROUTINE LUMLOG(MODE,XPAR,NPAR)
!     *********************************  
! =================================================================== 
! ===================================================================
! ===BBB=======BBB==BBB===BBB===BBB===BBB=======BBBBBB=====BBBBBBB===
! ===BBB=======BBB==BBB===BBBB=BBBB===BBB======BBBBBBBB===BBBBBBBBB==
! ===BBB=======BBB==BBB===BBBBBBBBB===BBB======BBB==BBB===BBB========
! ===BBB=======BBB==BBB===BBBBBBBBB===BBB======BBB==BBB===BBB==BBBB==
! ===BBBBBBBB==BBBBBBBB===BBB=B=BBB===BBBBBBB==BBB==BBB===BBB===BBB==
! ===BBBBBBBB===BBBBBB====BBB===BBB===BBBBBBB===BBBBBB=====BBBBBB====
! ===================================================================
!
!           ************************************************** 
!           *       **********************************       *
!           *       *      *******************       *       *
!           *       *      *                 *       *       *
!           *       *      *   L U M L O G   *       *       *
!           *       *      *                 *       *       *
!           *       *      *******************       *       *
!           *       **********************************       *
!           **************************************************    
!
! ----------------------------------------------------------------C
!                                                                C
!                       LUMLOG                                   C
!                                                                C
!          COLLLINEAR LEADING-LOG MONTE CARLO FOR                C
!               LOW-ANGLE BHABHA SCATTERING                      C
!                     NOVEMBER 1990                              C
!                 last update  5 feb. 91 (pairs)                 C
!                 last update 14 feb. 91 born in robol6          C
!                                     bug in modelu              C
!                 last update  8 apr. 91 cosmetics (sj)          C
!                 last update 26 aug. 91 cosmetics (sj)          C
!                 last update    May. 94 Final state rad. (sj)   C
!                 last update   June. 95 LL emulation     (sj)   C
!         AUTHORS:                                               C
!         S. Jadach, E. Richter-Was, Z. Was, B.F.L. Ward         C
!                                                                C
! The user is kindly requested to cite preprint TH.5995 (1991)   C
! Phys. Lett. B260 (1991) 438 of the same authors.               C
! (Note that TH.5888 is now Phys. Lett. B253 (1991) 469).        C
! ----------------------------------------------------------------C
!
! Note that LUMLOG contains originally two MC program: BHALOG which
! is present here and MULTILOG which is not inluded here.
! The series of comparisons between these two programs (TH-5995)
! has leaded the 0.02% technical precision estimate for LUMLOG.
!
! ----------------------------------------------------------------------
!                 INPUT and OUTPUT of LUMLOG
! ----------------------------------------------------------------------
! All input and output goes through parameters in 
!                 CALL LUMLOG(MODE,XPAR,NPAR)
! and through /MOMSET/ and /WGTALL/ common blocks.
! In the following we shall  briefly indicate the meaning of the
! above parameters/variables.
!
! IF( MODE =-1 ) THEN
! ===================
! Initialisation is performed, all input parameters are transfered
! through XPAR and NPAR.
! In the following table we indicate the meaning of NPAR, XPAR 
! entries for LUMLOG subgenerator in the initialization mode.
!         Table        Input parameters of LUMLOG
! ----------------------------------------------------------------------
!  Entry    Variable   Meaning
! ----------------------------------------------------------------------
!  NPAR( 1)  KEYOPT =1000*KEYGEN +10*KEYWGT +KEYRND 
!                   General option switch 
!            KEYGEN =2 for this sub-generator
!            KEYRND =1,2 type of random number generator RANMAR,RANECU
!            KEYWGT =1 for variable weight WTM
!            KEYWGT =2 generation down to zero angle
!  NPAR( 2)  KEYRAD =100*KEYFIN +10*KEYTES+ KEYBLO , QED option switch 
!            KEYFIN =  0    No explicit final state emission
!            KEYFIN =  1    Final state emission included
!            KEYTES =  0    normal str. function DEFAUT!!!
!            KEYTES =  1    test str. functions (1-z)**(-1/2)
!            KEYBLO =  3    LLog  ln(s'*xi_dot/me**2)  -1)  DEFAUT!!!
!            KEYBLO =  4    LLog  ln(s*xiA/me**2)   
!  XPAR( 1)  CMSENE Total center mass energy [GeV]
!  XPAR( 2)   TMINL Theta_minimum [degr]
!  XPAR( 3)   TMAXL Theta_maximum [degr]
!  XPAR( 4)     XK0 Dimensionless infrared cut-off (on real soft 
!                   photons) relevant for un-exponentiated case.
!                   Range 0.000001<XKO<0.0001 recommeded.
!  XPAR( 5)   XKMAX Determines minimum effective mass s' of the
!                   final state electron-positron, s'>s*(1-XKMAX).
!                   XKMAX=1 is alowed and recommended.
! ----------------------------------------------------------------------
!
! ELSE IF( MODE = 0 ) THEN
! ========================
! Generation of the single Monte Carlo event. 
! Only variable weight events are produced (!)
! (the user may turn them, if he wishes, into WT=1 events by himself).
! The four momenta of the final state electron, positron and photon
! are encoded in 
!      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
! where P1 and Q1 are four-momenta of positron and elecron beams.
! P2 and Q2 are four-momenta of outgoing positron and electron.
!                       IMPORTANT 
!                       ---------
! Remember that P2 and Q2 from LUMLOG do not represent normal 'bare'
! electron and positron but 'dressed' ones, i.e. they sum four-momenta
! of electron and ALL photons collinear with them 
! (as calorimeter does). 
! (N.B. Collinearity relation extends to photons with transverse 
! momentum up to characteristic LL scale pT_max = sqrt(Q**2)).
!                       ---------
! As a result, the list PHOT(100,4) of photon four-momenta is empty 
! and NPHOT=0 (the number of real photons in PHOT).  
! The principal weight WTM of the event is placed in
!      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)   
! It is usually of interest to use 'paralel weights' from WTSET.
! The event weight is constructed then as WT= WTCRU1*WTCRU2*WTSET(J).
! Which J is alowed and what version of the QED matrix element 
! it represents is summarized in the table below.
! N.B. principal weight WTM= WTCRU1*WTCRU2*WTSET(4).
! All calculation in LUMLOG are Leading-Log initial state bremss. type.
!       Table of WTSET entries for LUMLOG
! ----------------------------------------------------------------------
!  Entry      Type of the used electron (non-singl) structure function
! ----------------------------------------------------------------------
!             QED order   Exponentiation       pairs 
!             ---------------------------------------------------------
!  WTSET( 1)  Zero-th     exponentiated        No
!  WTSET( 2)  First       exponentiated        No
!  WTSET( 3)  Second      exponentiated        No
!  WTSET( 4)  Third       exponentiated        No <<== PRINCIPAL WEIGHT
!  WTSET( 5)  Third       exponentiated       Yes
!  WTSET(11)  Zero-th     not exponentiated    No
!  WTSET(12)  First       not exponentiated    No
!  WTSET(13)  Second      not exponentiated    No
!  --------------------------------------------------------------------
!     LL emulation of the full matrix element (June 95)
!  --------------------------------------------------------------------
! All beta's:  O(alf0),O(alf1),O(alf2)
!      WTSET( 40) =   Total O(alf0)
!      WTSET( 41) =   Total O(alf1)
!      WTSET( 42) =   Total O(alf2) <<== PRINCIPAL emulation of multiph.
! Individual beta's in various orders.
! O(alf1)
!      WTSET( 43) =     beta0
!      WTSET( 44) =     beta1
!      WTSET( 45) =     beta1 upper  line component
!      WTSET( 46) =     beta1 lower  line component
! O(alf2)
!      WTSET( 47) =     beta0
!      WTSET( 48) =     beta1
!      WTSET( 49) =     beta2
!      WTSET( 50) =     bt11u =  beta1 upper line component
!      WTSET( 51) =     bt11l =  beta1 lower line component
!      WTSET( 52) =     bt2ul =  beta2 upper*lower component
!      WTSET( 53) =     bt20u =  beta2 upper component
!      WTSET( 54) =     bt20l =  beta2 lower component
!  --------------------------------------------------------------------
! 
! ELSE IF( MODE = 1 ) THEN
! ========================
! The total cross section corresponding to generated series of event,
! i.e. resulting from MC integrartion is calculated and stored in XPAR
! and NPAR, see table below.
!  --------------------------------------------------------------------
!  Entry    Variable   Meaning
!  --------------------------------------------------------------------
!  NPAR(20)  NEVGEN  Number of generated MC events
!  XPAR(20)    XCRU  Crude total MC x-section [nb] which is necessary
!                    for rescaling histograms 
!                    in run with weighted events.
!  XPAR(21)          =0, error of XPAR(20), it is identicaly zero
!  XPAR(22)    BORN  Born x-cextion [nb]
!  XPAR(25)    SIG0  Miscelanous
!  --------------------------------------------------------------------
! For MODE=1 program is called upon many times in the process of 
! rescaling histograms, therefore, there is no output printed in 
! this mode.
!
! ELSE IF( MODE = 2 ) THEN
! ========================                     
! 
! Only in this MODE=2 in addition to filling XPAR and NPAR as 
! for MODE=1 the values of various x-sections are printed on 
! the standard output file.
!                
! ENDIF
! ====
!
!     ***********************************  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      DIMENSION XPAR(*),NPAR(*)
! 
      CALL BHALOG(MODE,XPAR,NPAR)
      END

      SUBROUTINE BHALOG(MODE,XPAR,NPAR)
!     ***********************************  
! ----------------------------------------------------------------C
!            BHALOG is part of LUMLOG library                    C
! ----------------------------------------------------------------C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(GNANOB =  389385D0) 
      PARAMETER(AMEL   =  0.511D-3)       
      PARAMETER(X0     =  1D-5)       
      DIMENSION XPAR(*),NPAR(*)
      COMMON / INOUT  / NINP,NOUT 
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD      
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,TT3,TT4,KEYTES
      COMMON / UTIBLO / X1,X2,X3,X4,XI1,XI2,XI
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      COMMON / RANPAR / KEYRND
      LOGICAL LCONE1,LCONE2,LCONE,LENERG     
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      DOUBLE PRECISION DRVEC(100)
      EXTERNAL FUNSKI

      IF( MODE.EQ.-1) THEN
!     ********************    
! ...BX-formats for nice and flexible outputs
      BXOPE =  '(//1X,15(5H*****)    )'
      BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'
      BXL1I =  '(1X,1H*,I17,                 16X, A20,A12,A7, 1X,1H*)'
      BXL1F =  '(1X,1H*,F17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2F =  '(1X,1H*,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXL1G =  '(1X,1H*,G17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2G =  '(1X,1H*,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXCLO =  '(1X,15(5H*****)/   )'    
! .........
      CMSENE = XPAR(1)
      TMINL  = XPAR(2)
      TMAXL  = XPAR(3)  
      XK0    = XPAR(4)
      XKMAX  = XPAR(5)
      KEYOPT = NPAR(1)
      KEYRAD = NPAR(2)
!
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '*     *****************         *'
      WRITE(NOUT,BXTXT) '*     ***   LUMLOG  ***         *'
      WRITE(NOUT,BXTXT) '*     *****************         *'
      WRITE(NOUT,BXTXT) '*(P.L. B260 (1991) 438, TH-5995)*'
      WRITE(NOUT,BXTXT) '* This program is now part of   *'
      WRITE(NOUT,BXTXT) '*   BHLUMI version 4.04         *'
      WRITE(NOUT,BXTXT) '*   September      1996         *'
      WRITE(NOUT,BXTXT) '*         AUTHORS               *'
      WRITE(NOUT,BXTXT) '* S. Jadach, E. Richter-Was     *'
      WRITE(NOUT,BXTXT) '*    B.F.L. Ward, Z. Was        *'
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXCLO)
!
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '   ===== LUMLOG/BHALOG  ======   '
      WRITE(NOUT,BXTXT) '   initialization starts....     '
! .........        
      KEYBLO = MOD(KEYRAD,10) 
      KEYTES = MOD(KEYRAD,100)/10    
      KEYFIN = MOD(KEYRAD,1000)/100    
      KEYRND = MOD(KEYOPT,10)   
      KEYWGT = MOD(KEYOPT,100)/10   
      IF(TMAXL.GE. 180D0) TMAXL=179.9999999D0
      XIA    = (1-COS(TMINL*PI/180))/2  
      XIB    = (1-COS(TMAXL*PI/180))/2  
      RAXI   = XIB/XIA
      DSIG0 = 4D0*ALFA**2*PI/CMSENE**2*GNANOB    
!  ETA is a dummy parameter present only in crude str. funct.
      ETA  = ALF1*DLOG((CMSENE/AMEL)**2*XIA)
! for callibration test
      IF(KEYTES.EQ.1)  ETA  = 0.5D0    
      ZMIN = AMEL/CMSENE   
      IF(XKMAX.LT.1D0) ZMIN = 1D0-XKMAX
!-----------------------------------------------------------------
      IF(KeyWgt .EQ. 1) THEN
!-- OLD method: no generation of transfer below Xia
        bornc = dsig0*(1/xia-1/xib)      
      ELSEIF( KeyWgt .EQ. 2 .AND. abs(Xib/Xia-1).GT.0.1d0 ) THEN
!-- NEW feature: Generation of transfer below Xia
        bornc = dsig0*(2/xia-1/xib)      
      ELSE
        write(6,*) '+++++LUMLOG: wrong KeyWgt= ',KeyWgt
      ENDIF
!-----------------------------------------------------------------
      BORN  = DSIG0*FOBOR(XIA,XIB)
!... Z correction   
      AMAZ  = 91.187d0
      GAMMZ =  2.490d0
      SINW2 = 0.2319d0
      GA = -1/(4*DSQRT(SINW2*(1-SINW2)))
      GV = GA*(1-4*SINW2)

      WRITE(NOUT,BXL1I) KEYOPT,     ' options   switch  ','KEYOPT','N1'
      WRITE(NOUT,BXL1I) KEYRND,     ' VARRAN    switch  ','KEYRND','  '
      WRITE(NOUT,BXL1I) KEYWGT,     ' weight    switch  ','KEYWGT','  '
      WRITE(NOUT,BXL1I) KEYRAD,     ' radiation switch  ','KEYRAD','N2'
      WRITE(NOUT,BXL1I) KEYFIN,     ' KEYFIN            ','KEYFIN','  '
      WRITE(NOUT,BXL1I) KEYTES,     ' KEYTES            ','KEYTES','  '
      WRITE(NOUT,BXL1I) KEYBLO,     ' KEYBLO            ','KEYBLO','  '
      WRITE(NOUT,BXL1I) KEYRND,     ' KEYRND            ','KEYRND','  '
      WRITE(NOUT,BXL1F) CMSENE,     ' CMSENE [GeV]      ','CMSENE','X1'
      WRITE(NOUT,BXL1F) TMINL ,     ' TMIN  [degr.]     ','TMIN  ','X2'
      WRITE(NOUT,BXL1F) TMAXL ,     ' TMAX  [degr.]     ','TMAX  ','X3'
      WRITE(NOUT,BXL1F) XK0   ,     ' xk0 cut (no exp.) ','XK0   ','X4'
      WRITE(NOUT,BXL1F) XKMAX ,     ' minimum sprim/s   ','XKMAX ','X5'
      WRITE(NOUT,BXL1F) RAXI  ,     ' RAXI              ','RAXI  ','  '
      WRITE(NOUT,BXL1F) XIA   ,     ' XIA=(1-cosTMINL)/2','XIA   ','  '
      WRITE(NOUT,BXL1F) XIB   ,     ' XIB=(1-cosTMAXL)/2','XIB   ','  '
      WRITE(NOUT,BXL1F) ETA   ,     ' ETA               ','      ','  '
      WRITE(NOUT,BXL1F) ZMIN  ,     ' ZMIN              ','      ','  '
      WRITE(NOUT,BXL1F) BORNC ,     ' Born crude        ','BORNC ','  '
      WRITE(NOUT,BXL1F) BORN  ,     ' Born exact        ','BORNX ','  '
      WRITE(NOUT,BXL1F) AMAZ  ,     ' Z-mass GeV        ','AMAZ  ','  '
      WRITE(NOUT,BXL1F) GAMMZ ,     ' Z-width GeV       ','GAMMZ ','  '
      WRITE(NOUT,BXL1F) SINW2 ,     ' weak mixing angle ','SINW2 ','  '
! .......... 
      CALL VESK2W(-1,FUNSKI,DUM1,DUM2,WT) 
      IDA=100
      DO 12 K=1,5   
      CALL GMONIT(-1,IDA+   K,0D0,1D0,1D0)  
      CALL GMONIT(-1,IDA+10+K,0D0,1D0,1D0)  
   12 CONTINUE

      WRITE(NOUT,BXTXT) '  end of initialization          '
      WRITE(NOUT,BXCLO)  
!-------------------------------------------------------------
! This is Generator Identificator
      IDGEN =  2        
! Important histo which remembers total x-section
      CALL VESK2W(0,FUNSKI,DUM1,DUM2,WTVES) ! to prevent 1/zero in next call
      CALL VESK2W(1,FUNSKI,AWT,EREL,ZCRUD)
      SIGTZ  = ZCRUD *BORNC
      CALL GMONIT(  -1, IDGEN,0D0,2*SIGTZ,1D0)
!-------------------------------------------------------------
      nevgen=0            
          
!     *********************** 
      ELSEIF( MODE.EQ.0) THEN
!     ***********************     
      nevgen=nevgen+1   
      CALL GMONIT(  0, IDGEN, SIGTZ, 2*SIGTZ,1D0)
!==============================================================
!   Crude distributions, generation of internal MC variables
!==============================================================
  200 CONTINUE
! generation of (x1,x2)      
      CALL VESK2W(0,FUNSKI,DUM1,DUM2,WTVES)       
      XMAX=  1-ZMIN   
      XX1=0D0
      XX2=0D0
      IF(TT1.GT.X0**ETA) XX1 = XMAX*DEXP(1/ETA*DLOG(TT1))
      IF(TT2.GT.X0**ETA) XX2 = XMAX*DEXP(1/ETA*DLOG(TT2))  
      X1 = XX1
      X2 = XX2
      Z1 = 1D0-XX1
      Z2 = 1D0-XX2 
!--------------------------------------------------------------------
! Generate t-channel transfer (the true one)
!--------------------------------------------------------------------
      CALL VARRAN(DRVEC,1)
      rn1=DRVEC(1)
      IF(KeyWgt .EQ. 1) THEN
!--------------------------------------------------------------------
!-- OLD method: no generation of transfer below Xia
        Xicom =  xia*xib/(xib*(1d0-rn1)+XIA*rn1)
        WTxi=1d0
      ELSEIF( KeyWgt .EQ. 2 .AND. abs(Xib/Xia-1).GT.0.1d0 ) THEN
!--------------------------------------------------------------------
!-- NEW feature: Generation of angle below theta minimum
        ynorm= 2/Xia -1/Xib
        prob1= (1/Xia) /ynorm
        prob2= (1/Xia -1/Xib)/ynorm
        IF(rn1.LT.prob1) THEN
!-- flat distribution below Xia
          Xicom= Xia*rn1/prob1
          WTxi = Xia**2/Xicom**2
!-- to prevent crash of kinematics  (1d-8 effect)
          IF(Xicom*CMSene**2.lt.(10*Amel)**2) GOTO 200
        ELSE
!-- normal 1/Xi**2 distribution above Xia
          rnx  = (rn1-prob1)/prob2
          Xicom = 1d0 / (  rnx/Xia   +(1d0-rnx)/Xib )
          WTxi  = 1d0
        ENDIF
      ELSE
        write(6,*) '+++++LUMLOG: wrong KeyWgt= ',KeyWgt
      ENDIF
!--------------------------------------------------------------------
! Translate Xicom into one of Xi1 or Xi2 
      IF(Z1.LE.Z2) THEN 
! generation of xi2 according to 1/xi**2 distribution  
         XI2 =  Xicom
! calculate the other xi1
         XI  =  XI2*Z2/(Z1+XI2*(Z2-Z1))
         XI1 =  XI *Z2/(Z1*(1-XI) +Z2*XI) 
! for callibration test
         IF(KEYTES.EQ.1) XI1 = XI2*(Z2/Z1)**2
      ELSE            
! generation of xi1 according to 1/xi**2 distribution   
         XI1 =  Xicom
! calculate the other xi2
         XI  =  XI1*Z1/(Z2+XI1*(Z1-Z2))
         XI2 =  XI *Z1/(Z2*(1-XI) +Z1*XI)   
! for callibration test
         IF(KEYTES.EQ.1) XI2 = XI1*(Z1/Z2)**2
      ENDIF         
!------------------------------------------------------------------
! Final state bremsstrahlung, crude distributions
!------------------------------------------------------------------
      x3=0
      x4=0
      wtfin3=1
      wtfin4=1
      IF(keyfin.eq.1) THEN
        CALL Brelos(eta,tt3,x3,wtfin3)
        CALL Brelos(eta,tt4,x4,wtfin4)
      ENDIF
!------------------------------------------------------------------
!  Angular Trigger
!------------------------------------------------------------------
! symmetric angular trigger   
      LCONE1 = XI1.GT.XIA.AND.XI1.LT.XIB
      LCONE2 = XI2.GT.XIA.AND.XI2.LT.XIB
      LCONE  = LCONE1.AND.LCONE2       
      LENERG = Z1*Z2 .GT. ZMIN  
!CC No cut on z1*z2 = 1-v any more (stj sept. 91)
!      WTRIG=0D0
!      IF(LCONE) WTRIG=1D0
!CC No angular cut any more (stj may 94)
!      WTRIG=1d0
!
      WTCRU1 = WTVES
      WTCRU2 = WTxi
      WTCRUD = WTCRU1*WTCRU2
!------------------------------------------------------------------
! calculate four-momenta for accepted events
!------------------------------------------------------------------
!  No angular cut any more (stj may 94)
!*******IF( LCONE ) CALL KINOLT
        CALL KINOLT
!==================================================================
!==================    MODEL Weights    ===========================
!==================================================================

      IF(keyfin.eq.0) THEN
! Initial state only
        CALL mdline
        CALL mdlinu
      ELSE
! Initial+FINAL state 
        CALL mdlife(wtfin3,wtfin4)
        CALL mdlifu(wtfin3,wtfin4)
        CALL mdlbhe(wtfin3,wtfin4) ! LL emulation of O(alf2)exp
        CALL mdlbhU(wtfin3,wtfin4) ! Upper line emission, for DEBUG
      ENDIF

      CALL GMONIT( 0,IDA+ 1,WTCRUD*WTSET( 1),1D0,0D0)
      CALL GMONIT( 0,IDA+ 2,WTCRUD*WTSET( 2),1D0,0D0)
      CALL GMONIT( 0,IDA+12,WTCRUD*(WTSET(2)-WTSET(1)),1D0,0D0)
      CALL GMONIT( 0,IDA+ 3,WTCRUD*WTSET( 3),1D0,0D0)
      CALL GMONIT( 0,IDA+13,WTCRUD*(WTSET(3)-WTSET(2)),1D0,0D0)
      CALL GMONIT( 0,IDA+ 4,WTCRUD*WTSET( 4),1D0,0D0)
      CALL GMONIT( 0,IDA+14,WTCRUD*(WTSET(4)-WTSET(3)),1D0,0D0)
      CALL GMONIT( 0,IDA+ 5,WTCRUD*WTSET( 5),1D0,0D0)
      CALL GMONIT( 0,IDA+15,WTCRUD*(WTSET(5)-WTSET(4)),1D0,0D0)

! This is principal weight (third order no pairs)
      WTMOD     =      WTCRUD*WTSET( 4)

!============================================================= 
! for calibration test
      IF(KEYTES.EQ.1)  WTMOD=WTCRUD 
!     ********
      ELSE
!     ********  
! final printout  
      NPAR(10)= NEVGEN
      NPAR(20)= NEVGEN
      CALL VESK2W(1,FUNSKI,AWT,EREL,ZCRUD)
      DWT  = AWT*EREL       
      XCRU = ZCRUD *BORNC
! ....... xsections in concecutive orders......
      CALL GMONIT( 1,IDA+ 1,AWT01,DWT01,DUMM)    
      XS01 = XCRU*AWT01
      DS01 = XCRU*AWT01*DWT01 
      CALL GMONIT( 1,IDA+ 2,AWT02,DWT02,DUMM)    
      XS02 = XCRU*AWT02
      DS02 = XCRU*AWT02*DWT02
      CALL GMONIT( 1,IDA+ 3,AWT03,DWT03,DUMM)    
      XS03 = XCRU*AWT03
      DS03 = XCRU*AWT03*DWT03
      CALL GMONIT( 1,IDA+ 4,AWT04,DWT04,DUMM)    
      XS04 = XCRU*AWT04
      DS04 = XCRU*AWT04*DWT04 
! ....... the differences between orders......
      CALL GMONIT( 1,IDA+12,AWT12,DWT12,DUMM)    
      RXS12 = XCRU*AWT12        /BORN
      RDS12 = XCRU*AWT12*DWT12  /BORN
      CALL GMONIT( 1,IDA+13,AWT13,DWT13,DUMM)    
      RXS13 = XCRU*AWT13        /BORN
      RDS13 = XCRU*AWT13*DWT13  /BORN
      CALL GMONIT( 1,IDA+14,AWT14,DWT14,DUMM)    
      RXS14 = XCRU*AWT14        /BORN
      RDS14 = XCRU*AWT14*DWT14  /BORN
! ... and pairs: third order + pairs, pairs only
      CALL GMONIT( 1,IDA+ 5,AWT05,DWT05,DUMM)    
      XS05 = XCRU*AWT05
      DS05 = XCRU*AWT05*DWT05 
      CALL GMONIT( 1,IDA+15,AWT15,DWT15,DUMM)    
      RXS15 = XCRU*AWT15        /BORN
      RDS15 = XCRU*AWT15*DWT15  /BORN
! .......
      XPAR(10)= XS04
      XPAR(11)= DS04/XS04    
! for WEIGHTED events
      XPAR(20)= XCRU
      XPAR(21)= 0D0     
      XPAR(22)= BORN 
! auxiliary information
      XPAR(25)= DSIG0      
      IF(MODE.EQ.1) RETURN      
!     ====================
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '        OUTPUT FROM              '
      WRITE(NOUT,BXTXT) '  LUMLOG/BHALOG: WINDOW A        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '   X.sect. in [nb] units         '
      WRITE(NOUT,BXTXT) '   for total generated sample    '
      WRITE(NOUT,BXL1I) NEVGEN,     'generated events   ','NEVGEN','A1'
      WRITE(NOUT,BXL2F) AWT ,DWT   ,'vesko2 int. estim. ','AWT   ','A2'
      WRITE(NOUT,BXL1F) XCRU  ,     'crude xsec (vesko2)','XCRU  ','A3'
      WRITE(NOUT,BXL1F) BORN  ,     'Born xsection      ','BORN  ','A4'
      WRITE(NOUT,BXTXT) '        ---  O(alf0)exp ---      '
      WRITE(NOUT,BXL2F) XS01,DS01,  'xsec. total        ','XS01  ','A5'
      WRITE(NOUT,BXL1F) DS01/XS01,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS01/BORN-1,'O(alf0)/Born-1     ','      ','  '
      WRITE(NOUT,BXTXT) '        ---  O(alf1)exp ---      '
      WRITE(NOUT,BXL2F) XS02,DS02,  'xsec. total        ','XS02  ','A6'
      WRITE(NOUT,BXL1F) DS02/XS02,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS02/BORN-1,'O(alf1)/Born-1     ','      ','  '
      WRITE(NOUT,BXL2F) RXS12,RDS12,'O(alf1-alf0)/Born  ','RXS12 ','  '
      WRITE(NOUT,BXTXT) '        ---  O(alf2)exp ---      '
      WRITE(NOUT,BXL2F) XS03,DS03,  'xsec. total        ','XS03  ','A7'
      WRITE(NOUT,BXL1F) DS03/XS03,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS03/BORN-1,'O(alf2)/Born-1     ','      ','  '
      WRITE(NOUT,BXL2F) RXS13,RDS13,'O(alf2-alf1)/Born  ','RXS13 ','  '
      WRITE(NOUT,BXTXT) '        ---  O(alf3)exp ---      '
      WRITE(NOUT,BXL2F) XS04,DS04,  'xsec. total        ','XS04  ','A8'
      WRITE(NOUT,BXL1F) DS04/XS04,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS04/BORN-1,'O(alf3)/Born-1     ','      ','  '
      WRITE(NOUT,BXL2F) RXS14,RDS14,'O(alf3-alf2)/Born  ','RXS14 ','  '
      WRITE(NOUT,BXTXT) '        ---  O(alf3)exp ---      '
      WRITE(NOUT,BXTXT) '        ---  plus pairs ---      '
      WRITE(NOUT,BXL2F) XS05,DS05,  'xsec. total        ','XS05  ','A9'
      WRITE(NOUT,BXL1F) DS05/XS05,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS05/BORN-1,'O(alf3+prs)/Born-1 ','      ','  '
      WRITE(NOUT,BXL2F) RXS15,RDS15,'pairs/Born         ','RXS15 ','  '
      WRITE(NOUT,BXCLO)  
      ENDIF
!     ********
      END       


      SUBROUTINE KINOLT        
!     *****************        
! construction of four-momenta 
! They are stored in /MOMSET/ which replaces /UTILUS/
!     ***********************************  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      COMMON / INOUT  / NINP,NOUT 
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD      
      COMMON / UTIBLO / X1,X2,X3,X4,XI1,XI2,XI  
! final state fermions in BHLUMI output format
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / TRANSR / TRAN,TRANX,TRANY
!
      DOUBLE PRECISION DRVEC(100)
      DATA ICONT /0/
!
      ICONT = ICONT+1 
      ENE  = CMSENE/2D0 
!...beams
      P1(4)=  ENE
      P1(3)=  ENE
      P1(2)=  0D0
      P1(1)=  0D0
      Q1(4)=  ENE
      Q1(3)= -ENE
      Q1(2)=  0D0
      Q1(1)=  0D0 
!...Explicit collinear photons 
!   in the initial state (parallel to beams) and in the final state
      NPHOT= 2
!...outgoing dressed (calorimetric) electrons  
      Z1   = 1D0-X1
      Z2   = 1D0-X2
      z3   = 1 -x3
      z4   = 1 -x4
      TRAN = CMSENE**2*Z1*Z2*XI
      CDOT = 1D0-2D0*XI 
      SDOT = DSQRT(DABS(4D0*XI*(1D0-XI))) 
      XPT  = 2D0*DSQRT(Z1*Z2)*SDOT
      CALL VARRAN(DRVEC,1)
      PHI  = 2D0*PI*DRVEC(1)
!...first electron
      P2(4) = 0.5D0*ENE*Z3 *( Z1+Z2   +CDOT*(Z1-Z2))
      P2(3) = 0.5D0*ENE*Z3 *( CDOT*(Z1+Z2) +Z1-Z2) 
      P2(2) = 0.5D0*ENE*Z3 * XPT*DSIN(PHI) 
      P2(1) = 0.5D0*ENE*Z3 * XPT*DCOS(PHI)
!...second electron
      Q2(4) = 0.5D0*ENE*Z4 *( Z1+Z2   -CDOT*(Z1-Z2))
      Q2(3) = 0.5D0*ENE*Z4 *( -CDOT*(Z1+Z2) +Z1-Z2) 
      Q2(2) =-0.5D0*ENE*Z4 * XPT*DSIN(PHI) 
      Q2(1) =-0.5D0*ENE*Z4 * XPT*DCOS(PHI)
!...Photon(s) parallel to first beam
      PHOT(1,4)=  ENE*(1-Z1)
      PHOT(1,3)=  ENE*(1-Z1)
      PHOT(1,2)=  0
      PHOT(1,1)=  0
!...Photon(s) parallel to second beam
      PHOT(2,4)=  ENE*(1-Z2)
      PHOT(2,3)= -ENE*(1-Z2)
      PHOT(2,2)=  0
      PHOT(2,1)=  0
!...Photons parallel to outgoing electron/positron
      keyfin = mod(keyrad,1000)/100    
      IF(keyfin.eq.1) THEN
        nphot= 4
        DO k=1,4
           phot(3,k)= (1-z3)/z3 *p2(k)
           phot(4,k)= (1-z4)/z4 *q2(k)
        ENDDO
      ENDIF
      END

      FUNCTION FUNSKI(T1,T2)
!     ************************************  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
! FLEPS is the smallest floating on a given instalation  
      PARAMETER(X0=1D-10)
!c===      PARAMETER(FLEPS= 1D-300)
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,TT3,TT4,KEYTES

      TT1=T1
      TT2=T2
      XMAX = 1D0-ZMIN  
!c===      SMALL = FLEPS**ETA
!c===      IF(SMALL.GT.1D-4) 
!c===     $   WRITE(6,*) ' ++++ FUNSKO: warning FLEPS**ETA=',SMALL
!c===      IF(T1.LT.SMALL.OR.T2.LT.SMALL) GOTO 900 
      XX1=0D0
      XX2=0D0
      IF(T1.GT.X0**ETA) XX1 = XMAX*DEXP(1/ETA*DLOG(T1))
      IF(T2.GT.X0**ETA) XX2 = XMAX*DEXP(1/ETA*DLOG(T2))  
! Jacobian factor due to change of variables t=>x
! eta*x**(eta-1) multiplied  (numerical stability)  see DD1*DD2
      RJAC = XMAX**(2D0*ETA)
!=== $     /(ETA*XX1**(ETA-1D0))/(ETA*XX2**(ETA-1D0))
! anticipated angular trigger
      Z1 = 1-XX1
      Z2 = 1-XX2   
      SLOPE = Z1/Z2 
      SRAXI = SQRT(XIB/XIA)
      IF(SLOPE.GT.  SRAXI) GOTO 900
      IF(SLOPE.LT.1/SRAXI) GOTO 900  
! Ordinary flux-factor      
      FLUX = 1D0/(Z1*Z2)
! Jacobian due to xi=>xi1 or xi=>xi2 change
      IF( Z1.LT.Z2) THEN
        DJAC= Z1/Z2
      ELSE
        DJAC= Z2/Z1
      ENDIF  
! Crude structure functions
! eta*x**(eta-1) divided out (numerical stability)  see RJAC
!===  DD1 = ETA*XX1**(ETA-1D0)*(1D0+(1D0-XX1)**2)/2D0
!===  DD2 = ETA*XX2**(ETA-1D0)*(1D0+(1D0-XX2)**2)/2D0
      DD1 =                    (1D0+(1D0-XX1)**2)/2D0
      DD2 =                    (1D0+(1D0-XX2)**2)/2D0
! Test distributions
      IF(KEYTES.EQ.1) THEN
!===    DD1 = ETA*XX1**(ETA-1D0)
!===    DD2 = ETA*XX2**(ETA-1D0)
        DD1 = 1D0       
        DD2 = 1D0   
      ENDIF
! below no trace of eta*x**(eta-1) any more!
      FUNSKI = FLUX*RJAC*DJAC*DD1*DD2
      RETURN
  900 FUNSKI = 0D0
      END
  

      FUNCTION BORNB(CMSENE,THA,THB)
!     *******************************
! BORN XSECTION pure t-channel     
! THA,THB are in radians, CMSENE in GEV units
! result in nanobarns
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      PARAMETER( ALFPI=  1D0/PI/ALFINV ,ALFA=1D0/ALFINV)
      PARAMETER( GNANOB=389.385D-30*1.D33 )
      EXTERNAL BORXI
      XIA= (1D0-DCOS(THA))/2D0
      XIB= (1D0-DCOS(THB))/2D0
      DSIG0 = 4D0*ALFA**2*PI/CMSENE**2*GNANOB    
      CALL GAUSJD(BORXI,XIA,XIB,-1D-6,RESULT)
      BORNB= RESULT *DSIG0
      END
      FUNCTION BORXI(XI)
!     ******************
! Integrand of BORNB
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      BORXI=1D0/XI**2*(1D0+(1D0-XI)**2)/2D0
      END

      FUNCTION FOBOR(XIA,XIB)
!     ***********************
! BORN XSECTION for pure t-channel     
! XI=(1-cos(theta))/2
! result in DSIG0 units where
! DSIG0 = 4D0*ALFA**2*PI/CMSENE**2
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      EXTERNAL BORNXF
      CALL GAUSJD(BORNXF,XIA,XIB,-1D-6,RESULT)
      FOBOR= RESULT
      END
      FUNCTION BORNXF(XI)
!     ******************
! Integrand for FOBOR
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      BORNXF=1D0/XI**2*(1D0+(1D0-XI)**2)/2D0
!----------------------------------------------------------------C
!                  The end of LUMLOG                             C
!----------------------------------------------------------------C
      END

      SUBROUTINE Brelos(eta,tau,x,wt)
!     *******************************
! Generates single x=1-z according to crude distribution
! D(gam,x) =gam*x**(eta-1)*chi(x)
! Note: wt = <wtint> = \int_0^1 D(gam,x) dx
!       Up to first order wt=exp(-3/4*eta)=1-3/4*eta
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      PARAMETER(x0=1d-5)
      DOUBLE PRECISION drvec(10)
! Inline function
      chi(x)= (1+(1-x)**2)/2
!
 100  CONTINUE
      CALL varran(drvec,2)
      tau = drvec(1)
      x = 0
      IF(tau.GT.x0**eta) x = exp( 1/eta*log(tau) )
      rn =  drvec(2)
      wtint =chi(x)
      IF(rn.gt.wtint) goto 100
      wt = 1 -eta/(1+eta) +eta/2/(2+eta)
      END



      SUBROUTINE mdline
!     *****************
! This is set of weights for the exponentiated case up to O(gam**3)exp.
! Explicit emission INITIAL STATE ONLY (final electron dressed)
!---------------------------------------------------------------------
!===  In order to assure numerical stability
!===  factors x**(beta-1) =[xmax*t**(1/eta)]**(beta-1)
!===  are divided off by hand, here and in in STRUFU.
!===  (Similar division of x**(eta-1) in FUNSKI is independent.)
!---------------------------------------------------------------------
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(AMEL   =  0.511D-3)       
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,TT3,TT4,KEYTES
      COMMON / UTIBLO / X1,X2,X3,X4,XI1,XI2,XI
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD      
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / INOUT  / NINP,NOUT 
!
      KEYBLO = MOD(KEYRAD,10) 
      XMAX=  1-ZMIN
      xx1 =  x1
      xx2 =  x2
      z1  =1-x1
      z2  =1-x2
!---------------------------------------------------------------------
! Two types of the big-log definition
      IF(KEYBLO.EQ.3) THEN 
        SPRIM = CMSENE**2*Z1*Z2
        BETA  = ALF1*(DLOG(SPRIM*XI/AMEL**2) -1)
      ELSEIF(KEYBLO.EQ.4) THEN
        BETA  = ALF1* DLOG((CMSENE/AMEL)**2*XIA) 
![[[[[        BETA  = ALF1* DLOG((CMSENE/AMEL)**2*XIB) 
      ELSE
        WRITE(6,*) ' WRONG KEYBLO=', KEYBLO
      ENDIF
!---------------------------------------------------------------------
! Born Factor
      WTBOR = (1+(1-XI)**2)/2D0      
!---------------------------------------------------------------------
!===  DDCRU =  ETA*XX1**(ETA-1D0)*(1D0+(1D0-XX1)**2)/2D0
!=== $        *ETA*XX2**(ETA-1D0)*(1D0+(1D0-XX2)**2)/2D0 
      DDCRU =  ETA*               (1D0+(1D0-XX1)**2)/2D0
     $        *ETA*               (1D0+(1D0-XX2)**2)/2D0 
     $        *XMAX**( ETA-BETA)*TT1**(1-BETA/ETA)
     $        *XMAX**( ETA-BETA)*TT2**(1-BETA/ETA)
! zero order
      DDMZR     =  STRUFU(310,BETA,XX1)*STRUFU(310,BETA,XX2)
      WTSET( 1) =  DDMZR/DDCRU*WTBOR 
! first order
      DDMO1     =  STRUFU(311,BETA,XX1)*STRUFU(311,BETA,XX2)
      WTSET( 2) =  DDMO1/DDCRU*WTBOR 
! second order
      DDMO2     =  STRUFU(312,BETA,XX1)*STRUFU(312,BETA,XX2)
      WTSET( 3) =  DDMO2/DDCRU*WTBOR 
! third order
      DDMO3     =  STRUFU(313,BETA,XX1)*STRUFU(313,BETA,XX2)
      WTSET( 4) =  DDMO3/DDCRU*WTBOR 
! third order + pairs       
      BETR = -3D0*DLOG(1-BETA/3D0)
      DDCRR =  ETA*               (1D0+(1D0-XX1)**2)/2D0
     $        *ETA*               (1D0+(1D0-XX2)**2)/2D0 
     $        *XMAX**( ETA-BETR)*TT1**(1-BETR/ETA)
     $        *XMAX**( ETA-BETR)*TT2**(1-BETR/ETA)
      DDMO4     =  STRUFU(313,BETR,XX1)*STRUFU(313,BETR,XX2)
      WTSET( 5) =  DDMO4/DDCRR*WTBOR 
      END

      SUBROUTINE mdlife(wtfin3,wtfin4)
!     ********************************
! This is set of weights for the exponentiated case up to O(gam**3)exp.
! Explicit emission INITIAL+FINAL STATE
!---------------------------------------------------------------------
!===  In order to assure numerical stability
!===  factors x**(beta-1) =[xmax*t**(1/eta)]**(beta-1)
!===  are divided off by hand, here and in in STRUFU.
!===  (Similar division of x**(eta-1) in FUNSKI is independent.)
!---------------------------------------------------------------------
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(AMEL   =  0.511D-3)       
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,TT3,TT4,KEYTES
      COMMON / UTIBLO / X1,X2,X3,X4,XI1,XI2,XI
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD      
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / INOUT  / NINP,NOUT 
! Inline function
      chi(x)= (1+(1-x)**2)/2
!
      KEYBLO = MOD(KEYRAD,10) 
      XMAX=  1-ZMIN
      z1  =1-x1
      z2  =1-x2
!---------------------------------------------------------------------
! Two types of the big-log definition
      IF(KEYBLO.EQ.3) THEN 
        sprim = cmsene**2*z1*z2
        beta  = alf1*(dlog(sprim*xi/amel**2) -1)
      ELSEIF(KEYBLO.EQ.4) THEN
        beta  = alf1* dlog((cmsene/amel)**2*xia) 
![[[[[        beta  = alf1* dlog((cmsene/amel)**2*xib) 
      ELSE
        WRITE(6,*) ' WRONG KEYBLO=', KEYBLO
      ENDIF
!---------------------------------------------------------------------
! Born Factor
      WTBOR = chi(xi)
      WTfin = wtfin3*wtfin4
!---------------------------------------------------------------------
!===  DDCRU =  eta*x1**(eta-1)*chi(x1)*eta*x2**(eta-1)*chi(x2)
!=== $        *eta*x3**(eta-1)*chi(x3)*eta*x4**(eta-1)*chi(x4)
      DDCRU =  eta*            chi(x1)*eta*            chi(x2)
     $        *eta*            chi(x3)*eta*            chi(x4)
     $        *xmax**( eta-beta)*tt1**(1-beta/eta)
     $        *xmax**( eta-beta)*tt2**(1-beta/eta)
     $                          *tt3**(1-beta/eta)
     $                          *tt4**(1-beta/eta)
! zero order
      DDMZR     =  strufu(310,beta,x1)*strufu(310,beta,x2)
     $            *strufu(310,beta,x3)*strufu(310,beta,x4)
      WTSET( 1) =  DDMZR/DDCRU*WTBOR *WTfin
! first order
      DDMO1     =  strufu(311,beta,x1)*strufu(311,beta,x2)
     $            *strufu(311,beta,x3)*strufu(311,beta,x4)
      WTSET( 2) =  DDMO1/DDCRU*WTBOR *WTfin
! second order
      DDMO2     =  strufu(312,beta,x1)*strufu(312,beta,x2)
     $            *strufu(312,beta,x3)*strufu(312,beta,x4)
      WTSET( 3) =  DDMO2/DDCRU*WTBOR *WTfin
! third order
      DDMO3     =  strufu(313,beta,x1)*strufu(313,beta,x2)
     $            *strufu(313,beta,x3)*strufu(313,beta,x4)
      WTSET( 4) =  DDMO3/DDCRU*WTBOR *WTfin
! third order + pairs       
      BETR = -3D0*DLOG(1-BETA/3D0)
      DDCRR =  eta*           chi(x1)*eta*           chi(x2)
     $        *eta*           chi(x3)*eta*           chi(x4)
     $        *xmax**( eta-betr)*tt1**(1-betr/eta)
     $        *xmax**( eta-betr)*tt2**(1-betr/eta)
     $                          *tt3**(1-betr/eta)
     $                          *tt4**(1-betr/eta)
      DDMO4     =  strufu(313,betr,x1)*strufu(313,betr,x2)
     $            *strufu(313,betr,x3)*strufu(313,betr,x4)
      WTSET( 5) =  DDMO4/DDCRR*WTBOR *WTfin
!---------------------------------------------------------------------
!          Upper line only
!---------------------------------------------------------------------
!===  DDCRU =  eta*x1**(eta-1)*chi(x1) *eta*x2**(eta-1)*chi(x2)
!=== $        *eta*x3**(eta-1)*chi(x3) *eta*x4**(eta-1)*chi(x4)
      DDCRU =  eta*            chi(x1) *eta*            chi(x2)
     $        *eta*            chi(x3) *eta*            chi(x4)
     $        *xmax**( eta-beta)*tt1**(1-beta/eta)
     $        *xmax**( eta-beta)*tt2**(1-beta/eta)
     $                          *tt3**(1-beta/eta)
     $                          *tt4**(1-beta/eta)
! third order
!===  DDMO6     =  strufu(313,beta,x1) *beta*x2**(beta-1)
!=== $            *strufu(313,beta,x3) *beta*x4**(beta-1)
      DDMO6     =  strufu(313,beta,x1) *beta
     $            *strufu(313,beta,x3) *beta
      WTSET( 6) =  DDMO6/DDCRU*WTBOR *WTfin
!---------------------------------------------------------------------
      END

      SUBROUTINE mdlinu      
!     ***********************
!  Model weight for second order LL unexponentiated
!  note infrared cut k0 is present as usual
!  (stj) 14 febr 91, 
!  correction of bug xk0**eta instead of (xk0/xmax)**eta
!
! correction 5.05.92     E.Richter-Was
! soft region x1<k0, x2<k0  is not corrected to true crude
! str. function i.e. should be divided by 1/((1+(1-x1)**2)/2) *
! 1/((1+(1-x2)**2)/2)   --- effect completelly negligible numerically
! futher corrections included in fortran code
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(GNANOB =  389385D0) 
      PARAMETER(AMEL   =  0.511D-3)       
      PARAMETER(X0     =  1D-5)       
      COMMON / INOUT  / NINP,NOUT 
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD      
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,TT3,TT4,KEYTES
      COMMON / UTIBLO / X1,X2,X3,X4,XI1,XI2,XI
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)

      KEYBLO = MOD(KEYRAD,10) 

      XMAX=  1-ZMIN   
      T0 = (XK0/XMAX)**ETA
      XX1=0D0
      IF(TT1.GT.T0) XX1 = XMAX*DEXP(1/ETA*DLOG(TT1))
      XX2=0D0
      IF(TT2.GT.T0) XX2 = XMAX*DEXP(1/ETA*DLOG(TT2))  
      Z1 = 1D0-XX1
      Z2 = 1D0-XX2 
! Born Factor
      WTBOR = (1+(1-XI)**2)/2D0    
! Two types of the big-log definition
      IF(KEYBLO.EQ.3) THEN 
        SPRIM = CMSENE**2*Z1*Z2
        BETA  = ALF1*(DLOG(SPRIM*XI/AMEL**2) -1)
      ELSEIF(KEYBLO.EQ.4) THEN
        BETA  = ALF1* DLOG((CMSENE/AMEL)**2*XIA) 
![[[[[        BETA  = ALF1* DLOG((CMSENE/AMEL)**2*XIB) 
      ELSE
        WRITE(6,*) ' WRONG KEYBLO=', KEYBLO
      ENDIF  
!   WTSET(11)   Born
!   WTSET(12)   O(alf1)
! 
      WTSET(11) =0D0
      WTSET(12) =0D0 
      WTSET(13) =0D0 
! soft region x1<k0, x2<k0
!CCCCCCCC      IF(TT1.LT.XK0**ETA.AND.TT2.LT.XK0**ETA) THEN
      IF(TT1.LT.T0.AND.TT2.LT.T0) THEN
         WTSET(11) = 1D0/XK0**(2*ETA) * WTBOR
         WTSET(12) = (1D0+ 2D0*BETA*DLOG(XK0)+ 3D0/2D0*BETA)
     $        /XK0**(2*ETA) * WTBOR
         WTSET(13) = 
     $  ( 1D0+4D0*BETA/4D0*(3/2D0+2D0*DLOG(XK0))
     $    +    (BETA/2D0)**2 *(3/2D0+2D0*DLOG(XK0))**2
     $    +2D0*(BETA/2D0)**2 *(9/8D0+2D0*(DLOG(XK0))**2
     $           +3D0*DLOG(XK0)    -PI**2/3D0))      
     $        /XK0**(2*ETA) * WTBOR
      ENDIF       
! single bremss. one soft photon (virt.) one hard  
!CCC      IF(TT1.LT.XK0**ETA.AND.TT2.GT.XK0**ETA) THEN
      IF(TT1.LT.T0.AND.TT2.GT.T0) THEN
         WTSET(12) = 
     $     BETA/( XK0**ETA *ETA *XX2**ETA)  * WTBOR  
         WTSET(13) =
     $      BETA *(1D0+BETA/2D0*( 3/2D0+2D0*DLOG(XK0)
     $                 +2D0*DLOG(XX2)-DLOG(1D0-XX2)+3/2D0)
!c5.05$    +BETA/2D0*((2D0-XX2)/2D0*DLOG(1D0-XX2)-XX2)*XX2 )
     $     +BETA/2D0*((2D0-XX2)/2D0*DLOG(1D0-XX2)-XX2)*XX2
     $                                      /(1D0+(1D0-XX2)**2) )
     $    /( XK0**ETA *ETA *XX2**ETA)  * WTBOR
      ENDIF                 
!CCC      IF(TT1.GT.XK0**ETA.AND.TT2.LT.XK0**ETA) THEN
      IF(TT1.GT.T0.AND.TT2.LT.T0) THEN
         WTSET(12) = 
     $     BETA/( XK0**ETA *ETA *XX1**ETA)  * WTBOR
         WTSET(13) =
     $      BETA *(1D0+BETA/2D0*( 3/2D0+2D0*DLOG(XK0)
     $                 +2D0*DLOG(XX1)-DLOG(1D0-XX1)+3/2D0)
!C5.05$     +BETA/2D0*((2D0-XX1)/2D0*DLOG(1D0-XX1)-XX1)*XX1 )
     $     +BETA/2D0*((2D0-XX1)/2D0*DLOG(1D0-XX1)-XX1)*XX1 
     $                                      /(1D0+(1D0-XX1)**2) )
     $    /( XK0**ETA *ETA *XX1**ETA)  * WTBOR
      ENDIF             
! two hardreal  photons above k0    
!CCCC      IF(TT1.GT.XK0**ETA.AND.TT2.GT.XK0**ETA) THEN
      IF(TT1.GT.T0.AND.TT2.GT.T0) THEN
         WTSET(13) = 
     $     BETA**2/( ETA**2 *XX1**ETA*XX2**ETA)  * WTBOR
      ENDIF
      END

      SUBROUTINE mdlifu(wtfin3,wtfin4) 
!     ********************************
!  Model weight for second order LL un-exponentiated INITIAL+FINAL
!  note infrared cut k0 is present as usual
!  WTSET(11)   Born
!  WTSET(12)   O(alf1) without exponentiation
!  WTSET(13)   O(alf2) without exponentiation
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(GNANOB =  389385D0) 
      PARAMETER(AMEL   =  0.511D-3)       
      PARAMETER(X0     =  1D-5)       
      COMMON / INOUT  / NINP,NOUT 
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD      
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,TT3,TT4,KEYTES
      COMMON / UTIBLO / X1,X2,X3,X4,XI1,XI2,XI
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      LOGICAL sof1,sof2,sof3,sof4,har1,har2,har3,har4
! Inline function
      chi(x)= (1+(1-x)**2)/2

      KEYBLO = MOD(KEYRAD,10) 
! Here xk0 is infrared dummy parameter,
      xmax=  1-zmin   
      t0I = (xk0/xmax)**eta
      t0F =  xk0**eta
      xx1=0
      xx2=0
      xx3=0
      xx4=0
      IF(tt1.gt.t0I) xx1 = xmax*exp(1/eta*log(tt1))
      IF(tt2.gt.t0I) xx2 = xmax*exp(1/eta*log(tt2))  
      IF(tt3.gt.t0F) xx3 =      exp(1/eta*log(tt3))
      IF(tt4.gt.t0F) xx4 =      exp(1/eta*log(tt4))  
      z1 = 1-xx1
      z2 = 1-xx2
      z3 = 1-xx3
      z4 = 1-xx4
! Born Factor
      WTBOR = chi(xi) *wtfin3*wtfin4   
! Two types of the big-log definition
      IF(KEYBLO.EQ.3) THEN 
        sprim = cmsene**2*z1*z2
        beta  = alf1*(dlog(sprim*xi/amel**2) -1)
      ELSEIF(KEYBLO.EQ.4) THEN
        beta  = alf1* dlog((cmsene/amel)**2*xia) 
![[[[[        beta  = alf1* dlog((cmsene/amel)**2*xib) 
      ELSE
        WRITE(6,*) ' WRONG KEYBLO=', KEYBLO
      ENDIF  
      WTSET(11) =0
      WTSET(12) =0 
      WTSET(13) =0 
      sof1 = tt1.lt.t0I
      sof2 = tt2.lt.t0I
      sof3 = tt3.lt.t0F
      sof4 = tt4.lt.t0F
      har1 = tt1.ge.t0I
      har2 = tt2.ge.t0I
      har3 = tt3.ge.t0F
      har4 = tt4.ge.t0F
! all four soft
      IF(sof1.AND.sof2.AND.sof3.AND.sof4) THEN
        WTSET(11) =  (1/xk0**eta)**4 * wtbor
        WTSET(12) = (1 + 4*beta*(3./4+log(xk0))  )
     $              *(1/xk0**eta)**4 * wtbor 
        WTSET(13) = 
     $  (1 +4*beta*(3./4+log(xk0)) -4*beta**2*(1./12*pi**2)
     $     +1./2*(4*beta)**2*(3./4+log(xk0))**2 )
     $              *(1/xk0**eta)**4 * wtbor 
      ENDIF
!-----------------------------------------------------
! one hard other three soft
      IF(har1.AND.sof2.AND.sof3.AND.sof4) THEN
        WTSET(12) =  beta
     $    *1/(eta*xx1**eta) *(1/xk0**eta)**3   * wtbor
        WTSET(13) = (beta *chi(xx1) -beta**2 *(1./4)*xx1**2
     $    -beta**2 *log(1-xx1) *(1./8)*(4-6*xx1+3*xx1**2)
     $    +beta**2 *chi(xx1)   *(3./4+log(xx1)) 
     $  +3*beta**2 *chi(xx1)   *(3./4+log(xk0)) 
     $  )  *1/(eta*xx1**eta*chi(xx1)) *(1/xk0**eta)**3 * wtbor
      ENDIF
      IF(sof1.AND.har2.AND.sof3.AND.sof4) THEN
        WTSET(12) =  beta
     $    *1/(eta*xx2**eta) *(1/xk0**eta)**3   * wtbor
        WTSET(13) = (beta *chi(xx2) -beta**2 *(1./4)*xx2**2
     $    -beta**2 *log(1-xx2) *(1./8)*(4-6*xx2+3*xx2**2)
     $    +beta**2 *chi(xx2)   *(3./4+log(xx2)) 
     $  +3*beta**2 *chi(xx2)   *(3./4+log(xk0)) 
     $  )  *1/(eta*xx2**eta*chi(xx2)) *(1/xk0**eta)**3 * wtbor
      ENDIF
      IF(sof1.AND.sof2.AND.har3.AND.sof4) THEN
        WTSET(12) =  beta
     $    *1/(eta*xx3**eta) *(1/xk0**eta)**3   * wtbor
        WTSET(13) = (beta *chi(xx3) -beta**2 *(1./4)*xx3**2
     $    -beta**2 *log(1-xx3) *(1./8)*(4-6*xx3+3*xx3**2)
     $    +beta**2 *chi(xx3)   *(3./4+log(xx3)) 
     $  +3*beta**2 *chi(xx3)   *(3./4+log(xk0)) 
     $  )  *1/(eta*xx3**eta*chi(xx3)) *(1/xk0**eta)**3 * wtbor
      ENDIF
      IF(sof1.AND.sof2.AND.sof3.AND.har4) THEN
        WTSET(12) =  beta
     $    *1/(eta*xx4**eta) *(1/xk0**eta)**3   * wtbor
        WTSET(13) = (beta *chi(xx4) -beta**2 *(1./4)*xx4**2
     $    -beta**2 *log(1-xx4) *(1./8)*(4-6*xx4+3*xx4**2)
     $    +beta**2 *chi(xx4)   *(3./4+log(xx4)) 
     $  +3*beta**2 *chi(xx4)   *(3./4+log(xk0)) 
     $  )  *1/(eta*xx4**eta*chi(xx4)) *(1/xk0**eta)**3 * wtbor
      ENDIF
!-----------------------------------------------------
! two hard real  photons above k0, other two are soft  
      IF(har1.AND.har2.AND.sof3.AND.sof4) THEN
        WTSET(13) = beta**2 
     $   /(eta*xx1**eta) /(eta*xx2**eta) *(1/xk0**eta)**2 * wtbor
      ENDIF
      IF(har1.AND.sof2.AND.har3.AND.sof4) THEN
        WTSET(13) = beta**2 
     $   /(eta*xx1**eta) /(eta*xx3**eta) *(1/xk0**eta)**2 * wtbor
      ENDIF
      IF(har1.AND.sof2.AND.sof3.AND.har4) THEN
        WTSET(13) = beta**2 
     $   /(eta*xx1**eta) /(eta*xx4**eta) *(1/xk0**eta)**2 * wtbor
      ENDIF
      IF(sof1.AND.har2.AND.har3.AND.sof4) THEN
        WTSET(13) = beta**2 
     $   /(eta*xx2**eta) /(eta*xx3**eta) *(1/xk0**eta)**2 * wtbor
      ENDIF
      IF(sof1.AND.har2.AND.sof3.AND.har4) THEN
        WTSET(13) = beta**2 
     $   /(eta*xx2**eta) /(eta*xx4**eta) *(1/xk0**eta)**2 * wtbor
      ENDIF
      IF(sof1.AND.sof2.AND.har3.AND.har4) THEN
        WTSET(13) = beta**2 
     $   /(eta*xx3**eta) /(eta*xx4**eta) *(1/xk0**eta)**2 * wtbor
      ENDIF
      END        



      SUBROUTINE modlu 
!     *****************
!  This routine is not used, it is completely equivalent to mdlinu
!  but has nicer representation of all formulas
!-----------------------------------------------------------------
!  Model weight for second order LL un-exponentiated INITIAL+FINAL
!  note infrared cut k0 is present as usual
!  WTSET(11)   Born
!  WTSET(12)   O(alf1)
!  WTSET(13)   O(alf2)
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(GNANOB =  389385D0) 
      PARAMETER(AMEL   =  0.511D-3)       
      PARAMETER(X0     =  1D-5)       
      COMMON / INOUT  / NINP,NOUT 
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD      
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,TT3,TT4,KEYTES
      COMMON / UTIBLO / X1,X2,X3,X4,XI1,XI2,XI
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      LOGICAL sof1,sof2,har1,har2
! Inline function
      chi(x)= (1+(1-x)**2)/2

      KEYBLO = MOD(KEYRAD,10) 

      XMAX=  1-ZMIN   
      T0 = (XK0/XMAX)**ETA
      xx1=0
      IF(tt1.gt.t0) xx1 = xmax*dexp(1/eta*dlog(tt1))
      xx2=0
      IF(tt2.gt.t0) xx2 = xmax*dexp(1/eta*dlog(tt2))  
      z1 = 1-xx1
      z2 = 1-xx2 
! Born Factor
      WTBOR = chi(xi)    
! Two types of the big-log definition
      IF(KEYBLO.EQ.3) THEN 
        sprim = cmsene**2*z1*z2
        beta  = alf1*(dlog(sprim*xi/amel**2) -1)
      ELSEIF(KEYBLO.EQ.4) THEN
        beta  = alf1* dlog((cmsene/amel)**2*xia) 
![[[[[        beta  = alf1* dlog((cmsene/amel)**2*xib) 
      ELSE
        WRITE(6,*) ' WRONG KEYBLO=', KEYBLO
      ENDIF  
      WTSET(11) =0
      WTSET(12) =0 
      WTSET(13) =0 
      sof1 = tt1.lt.t0
      sof2 = tt2.lt.t0
      har1 = tt1.ge.t0
      har2 = tt2.ge.t0
! soft region x1<k0, x2<k0
      IF(sof1.AND.sof2) THEN
        WTSET(11) =  1/xk0**(2*eta) * wtbor
        WTSET(12) = (1 + 2*beta*(3./4+log(xk0))  )
     $                /xk0**(2*eta) * wtbor
        WTSET(13) = 
     $  (1 +2*beta*(3./4+log(xk0)) -2*beta**2*(1./12*pi**2)
     $     +1./2*(2*beta)**2*(3./4+log(xk0))**2 )
     $  /xk0**eta /xk0**eta * wtbor 
      ENDIF       
! single bremss. one soft photon (virt.) one hard  
      IF(sof1.AND.har2) THEN
        WTSET(12) =  beta /xk0**eta /(eta*xx2**eta)  * wtbor  
        WTSET(13) =
     $  (   beta *chi(xx2)
     $  +beta**2 *chi(xx2)   *(3./4+log(xk0) +3./4+log(xx2)) 
     $  -beta**2 *log(1-xx2) *(1./8)*(4-6*xx2+3*xx2**2)
     $  -beta**2 *(1./4)*xx2**2
     $  ) /xk0**eta  /(eta*xx2**eta*chi(xx2))  * wtbor
      ENDIF                 
      IF(har1.AND.sof2) THEN
        WTSET(12) =  beta /xk0**eta /(eta*xx1**eta)  * wtbor
        WTSET(13) =
     $  (   beta *chi(xx1)
     $  +beta**2 *chi(xx1)   *(3./4+log(xk0) +3./4+log(xx1)) 
     $  -beta**2 *log(1-xx1) *(1./8)*(4-6*xx1+3*xx1**2)
     $  -beta**2 *(1./4)*xx1**2
     $  ) /xk0**eta  /(eta*xx1**eta*chi(xx1))  * wtbor
      ENDIF             
! two hardreal  photons above k0    
      IF(har1.AND.har2) THEN
        WTSET(13) = beta**2 
     $     /(eta*xx1**eta) /(eta*xx2**eta)  * wtbor
      ENDIF
      END        

      FUNCTION STRUFU(KEYD,BETA,VV)
!-------------------------------------------------------------------C
! This originates from XSTFIG 
! non-singlet structure functions, factor x**(beta-1) removed!!!
!-------------------------------------------------------------------C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      parameter( nout=6 )
      PARAMETER (DZ2=1.6449340668482264D0,DZ3=1.2020569031595943D0)
      PARAMETER(PI= 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER(CEULER =0.57721566D0)
   
      BETI=BETA
      X=VV
      Z=1D0-VV
! ....Zero Order without subleading terms
      IF(KEYD  .EQ.310)  THEN
!===     DISTR=BETI*X**(BETI-1D0)
         DISTR=BETI
     @      *EXP(BETI*(.75D0-CEULER))/DPGAMM(1D0+BETI)
! ....First Order without subleading terms
      ELSEIF(KEYD  .EQ.311)  THEN
!===     DISTR=BETI*X**(BETI-1D0)
         DISTR=BETI
     @      *EXP(BETI*(.75D0-CEULER))/DPGAMM(1D0+BETI)
     @      *(1D0 -.5D0*(1D0-(1D0-X)**2) )
! ....Second Order without subleading terms
      ELSEIF(KEYD  .EQ.312)  THEN
!===     DISTR=BETI*X**(BETI-1D0)
         DISTR=BETI
     @      *EXP(BETI*(.75D0-CEULER))/DPGAMM(1D0+BETI)
     @      *(1D0 -.5D0*(1D0-(1D0-X)**2)
     @       +BETI*( -(1D0+3D0*(1D0-X)**2)/8D0*LOG(1D0-X) -.25D0*X**2)
     @       )
! ....Third Order without subleading terms
      ELSEIF(KEYD  .EQ.313)  THEN
! redefine beta in the case of fermion pairs, 
! (this is: LL nonsinglet, electron pair only)
!===     DISTR=BETI*X**(BETI-1D0)        
         DISTR=BETI
     @      *EXP(BETI*(.75D0-CEULER))/DPGAMM(1D0+BETI)
     @      *(1D0 -.5D0*(1D0-(1D0-X)**2)
     @       +BETI*( -(1D0+3D0*(1D0-X)**2)/8D0*LOG(1D0-X) -.25D0*X**2)
     @          +BETI**2*(
     @                +(3D0*X-2D0)*X/16*LOG(1D0-X)
     @                 +(8D0-14D0*X+7D0*X**2)/96*LOG(1D0-X)**2
     @                 +X**2/8D0
     @                  +(2D0-X)*X/8*DILOGY(X)
     @                    )  )
      ELSE
            GOTO 900
      ENDIF        
      STRUFU=DISTR      
      RETURN
 900  WRITE(NOUT,*) ' ===--->  WRONG KEYDIS IN STRUFU'
      STOP
      END 




      SUBROUTINE mdlbhU(wtfin3,wtfin4)
!     ********************************
! LL emulation of BHLUMI amplitude up to O(gam**3)exp.
! Explicit emission UPPER LINE INITIAL+FINAL 
! ---------------------------------------------------------------------
! ===  In order to assure numerical stability
! ===  factors x**(beta-1) =[xmax*t**(1/eta)]**(beta-1) removed.
! ===  Similar division of x**(eta-1) is done in FUNSKI.
! ---------------------------------------------------------------------
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(AMEL   =  0.511D-3)       
      PARAMETER(CEULER =0.57721566D0)
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,TT3,TT4,KEYTES
      COMMON / UTIBLO / X1,X2,X3,X4,XI1,XI2,XI
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD      
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / INOUT  / NINP,NOUT 
! ---------------------------------------------------------------------
! Inline functions
! ---------------------------------------------------------------------
      chi(x)   = (1+(1-x)**2)/2
      FF(g)    = EXP( -g*ceuler )/dpgamm( 1d0 + g )
      STR(g,v) = g* v**(g)
      YFS(g,v) = exp( -g*log(1d0-v) +0.25d0*g)
      YFSd(g) = exp(0.25d0*g)
! ---------------------------------------------------------------------
      DATA fleps / 1d-6 /
! ---------------------------------------------------------------------
!
      XMAX=  1-ZMIN
      z1  =1-x1
      z2  =1-x2
      z3  =1-x3
      z4  =1-x4
!-----------------------------------------------------------------------
! Only one type of the big-log definition alowed
      sprim = cmsene**2*z1*z2
      beta  = alf1*(dlog(sprim*xi/amel**2) -1)
      gam   = 2d0*beta
!-----------------------------------------------------------------------
! Born Factor
      WTBOR = chi(xi)
      WTfin = wtfin3*wtfin4
!-----------------------------------------------------------------------
      vI  = x1*z3
      vF  = x3
      uI  = x2*z4
      uF  = x4
      v   = vI+vF
      u   = uI+uF
      FFac = FF(beta)
!-----------------------------------------------------------------------
      bt00  =0
      bt01  =0
      bt02  =0
      bt10u =0
      bt10l =0
      bt11u =0
      bt11l =0
      bt20u =0
      bt20l =0
      bt2ul =0
!=======================================================================
!               UPPER LINE   only   INITIAL+FINAL 
!=======================================================================
!-----------------------------------------------------------------------
! Crude distribution, 
! x**(beta1-1) divided out for the sake of numerical stability!
!-----------------------------------------------------------------------
!===> CRU1=   chi(x1) *eta *x1**(eta-1)
!===> CRU3=   chi(x3) *eta *x3**(eta-1)
!===> CRU2=   chi(x2) *eta *x2**(eta-1)
!===> CRU4=   chi(x4) *eta *x4**(eta-1)
      XRU1=   chi(x1) *eta *xmax**( eta-beta)*tt1**(1-beta/eta)
      XRU3=   chi(x3) *eta                   *tt3**(1-beta/eta)
      XRU2=   chi(x2) *eta *xmax**( eta-beta)*tt2**(1-beta/eta)
      XRU4=   chi(x4) *eta                   *tt4**(1-beta/eta)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                         beta zero
!-----------------------------------------------------------------------
      DDCRU =  XRU1*XRU3*XRU2*XRU4
      bt00 =    YFS(2*beta,vF)
!===>$     *FFac *beta *(x1*z3)**(beta-1) *z3   ! Upper Initial
!===>$     *FFac *beta *   (x3)**(beta-1)       ! Upper Final
!===>$           *beta *   (x2)**(beta-1)    ! Lower Initial
!===>$           *beta *   (x4)**(beta-1)    ! Lower Final
     $     *FFac *beta *(   z3)**(beta-1) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $           *beta                       ! Lower Initial
     $           *beta                       ! Lower Final
     $     /DDCRU
      bt01  = bt00*(1 + gam/2            )
      bt02  = bt00*(1 + gam/2  +1/8d0*gam**2)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                         beta one
!-----------------------------------------------------------------------
! Contributions from beta1 UPPER INITIAL line
!-----------------------------------------------------------------------
      bt11uI = 0
      IF(vI .GT. fleps ) THEN
         CRU1   =  chi(x1) *eta *x1**(eta-1)
         DDCRU  =  CRU1*XRU3*XRU2*XRU4
         bt11uI =  YFS(2*beta,vF)
     $     *FFac *vI**beta  *RRI2u(gam,vI) *z3   ! Upper Initial
     $     *FFac *beta                           ! Upper Final
     $           *beta                       ! Lower Initial
     $           *beta                       ! Lower Final
     $        /DDCRU
      ENDIF
!-----------------------------------------------------------------------
! Contributions from beta1 UPPER FINAL line
!-----------------------------------------------------------------------
      bt11uF = 0
      IF(vF .GT. fleps ) THEN
         CRU3   =  chi(x3) *eta *x3**(eta-1)
         DDCRU  =  XRU1*CRU3*XRU2*XRU4
         bt11uF =  YFS(2*beta,vF)
     $     *FFac *beta *(   z3)**(beta-1) *z3  ! Upper Initial
     $     *FFac *vF**beta  *RRF2u(gam,vF)     ! Upper Final
     $           *beta                       ! Lower Initial
     $           *beta                       ! Lower Final
     $        /DDCRU
      ENDIF
      bt11u = bt11uI +bt11uF
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                       beta two Upper Upper
!-----------------------------------------------------------------------
      bt2uuIF = 0
!-- Upper Initial * Upper Final
      IF(vI .GT. fleps .AND. vF .GT. fleps ) THEN
         CRU1   =  chi(x1) *eta *x1**(eta-1)
         CRU3   =  chi(x3) *eta *x3**(eta-1)
         DDCRU  =  CRU1*CRU3*XRU2*XRU4
         bt2uuIF =   YFS(2*beta,vF)
     $        *FFac *vI**beta                *z3 ! Upper Initial
     $        *FFac *vF**beta                    ! Upper Final
     $   *1d0/4*gam**2*( xkIF(vI,vF) +gam/2*wIF(vI,vF) )
     $           *beta                       ! Lower Initial
     $           *beta                       ! Lower Final
     $        /DDCRU
      ENDIF
      bt2uuII = 0
!-- Upper Initial * Upper Initial
      IF(vI .GT. fleps ) THEN
         CRU1   =  chi(x1) *eta *x1**(eta-1)
         DDCRU  =  CRU1*XRU3*XRU2*XRU4
         bt2uuII  =  YFS(2*beta,vF)
     $     *FFac *vI**beta  *b2II(gam,vI) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $           *beta                       ! Lower Initial
     $           *beta                       ! Lower Final
     $        /DDCRU
      ENDIF
      bt2uuFF = 0
!-- Upper Final * Upper Final
      IF(vF .GT. fleps ) THEN
         CRU3   =  chi(x3) *eta *x3**(eta-1)
         DDCRU  =  XRU1*CRU3*XRU2*XRU4
         bt2uuFF =  YFS(2*beta,vF)
     $     *FFac *beta *(   z3)**(beta-1) *z3 ! Upper Initial
     $     *FFac *vF**beta  *b2II(gam,vF)     ! Upper Final
     $           *beta                       ! Lower Initial
     $           *beta                       ! Lower Final
     $        /DDCRU
      ENDIF
      bt20u=bt2uuIF+ bt2uuII + bt2uuFF


!=======================================================================
!-----------------------------------------------------------------------
!                   Define MC weights
!-----------------------------------------------------------------------
!=======================================================================
      Facton =  WTBOR *WTfin
!           ---------------------------------
!           ! EXPONentated  UPPER line only !
!           ---------------------------------
! All beta's:  O(alf0),O(alf1),O(alf2)
      WTSET(130) =    bt00  *Facton
      WTSET(131) =   (bt01+bt10u)*Facton 
      WTSET(132) =   (bt02+bt11u+bt20u)*Facton
! Individual beta's in various orders.
! O(alf1)
      WTSET(135) =    bt01*Facton                   ! beta0
      WTSET(136) =    bt10u*Facton                  ! beta1,fix bbl230996
! O(alf2)
      WTSET(147) =    bt02*Facton                   ! beta0
      WTSET(148) =    bt11u*Facton                  ! beta1
      WTSET(149) =    bt20u*Facton                  ! beta2
!-----------------------------------------------------------------------
      END

      FUNCTION RRF2u(gam,V)
!     ***********************************
! Version for upper line only, adequate one line virtual in r_1 !!!
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(CEULER =0.57721566D0)
!-----------------------------------------------------------------------
!   [R_F^2(V)] =
!       + [N]*[1]^3*gam^3*[ln(1-V)]^2 * (  - 1/32 + 1/64*V + 1/32*V^-1 )
!       + [N]*[1]^3*gam^3*[ln(1-V)] * ( 1/32 - 1/32*V )
!       + [N]*[1]^3*gam^3*[Li2(V)] * (  - 1/16 + 1/32*V )
!       + [N]*[1]^3*gam^3 * ( 1/32*V )
!       + [N]*[1]^2*gam^2*[ln(1-V)] * (  - 1/8 + 1/16*V + 1/8*V^-1 )
!       + [N]*[1]^2*gam^2 * (  - 1/4 )
!       + [N]*[1]*gam * (  - 1/2 + 1/4*V );
!-----------------------------------------------------------------------
      RRF2u =
     $ +gam**3*log(1-V)**2   * (  - 1d0/32 + 1d0/64*V + 1d0/32/V )
     $ +gam**3*log(1-V)      * ( 1d0/32    - 1d0/32*V )
     $ +gam**3*dilogy(V)     * (  - 1d0/16 + 1d0/32*V )
     $ +gam**3               * (             1d0/32*V )
     $ +gam**2*log(1-V)      * (  - 1d0/8 + 1d0/16*V + 1d0/8/V )
     $ +gam**2               * (  - 1d0/4 )
     $ +gam                  * (  - 1d0/2 + 1d0/4*V )
      END
      FUNCTION RRI2u(gam,V)
!     ***********************************
! Version for upper line only, adequate one line virtual in r_2 !!!
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(CEULER =0.57721566D0)
!-----------------------------------------------------------------------
!   [R_I^2(V)] =
!       + [N]*[1]^3*gam^3*[ln(1-V)]^2 * ( 1/32 - 1/64*V - 1/32*V^-1 )
!       + [N]*[1]^3*gam^3*[ln(1-V)] * (  - 1/32 + 1/32*V )
!       + [N]*[1]^3*gam^3*[Li2(V)] * ( 1/16 - 1/32*V )
!       + [N]*[1]^3*gam^3 * (  - 1/32*V )
!       + [N]*[1]^2*gam^2*[ln(1-V)] * ( 1/8 - 1/16*V - 1/8*V^-1 )
!       + [N]*[1]^2*gam^2 * (  - 1/4 )
!       + [N]*[1]*gam * (  - 1/2 + 1/4*V );
!-----------------------------------------------------------------------
      RRI2u =
     $ +gam**3*log(1-V)**2   * ( 1d0/32 - 1d0/64*V - 1d0/32/V )
     $ +gam**3*log(1-V)      * (  - 1d0/32 + 1d0/32*V )
     $ +gam**3*dilogy(V)     * ( 1d0/16 - 1d0/32*V )
     $ +gam**3               * (  - 1d0/32*V )
     $ +gam**2*log(1-V)      * ( 1d0/8 - 1d0/16*V - 1d0/8/V )
     $ +gam**2               * (  - 1d0/4 )
     $ +gam                  * (  - 1d0/2 + 1d0/4*V )
      END


      SUBROUTINE mdlbhe(wtfin3,wtfin4)
!     ********************************
! LL emulation of BHLUMI amplitude up to O(gam**3)exp.
! Explicit emission INITIAL+FINAL STATE
! ---------------------------------------------------------------------
! ===  In order to assure numerical stability
! ===  factors x**(beta-1) =[xmax*t**(1/eta)]**(beta-1) removed.
! ===  Similar division of x**(eta-1) is done in FUNSKI.
! ---------------------------------------------------------------------
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(AMEL   =  0.511D-3)       
      PARAMETER(CEULER =0.57721566D0)
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,TT3,TT4,KEYTES
      COMMON / UTIBLO / X1,X2,X3,X4,XI1,XI2,XI
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD      
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / INOUT  / NINP,NOUT 
! ---------------------------------------------------------------------
! Inline functions
! ---------------------------------------------------------------------
      chi(x)   = (1+(1-x)**2)/2
      FF(g)    = EXP( -g*ceuler )/dpgamm( 1d0 + g )
      STR(g,v) = g* v**(g)
      YFS(g,v) = exp( -g*log(1d0-v) +0.25d0*g)
! ---------------------------------------------------------------------
      DATA fleps / 1d-6 /
! ---------------------------------------------------------------------
!
      XMAX=  1-ZMIN
      z1  =1-x1
      z2  =1-x2
      z3  =1-x3
      z4  =1-x4
!-----------------------------------------------------------------------
! Only one type of the big-log definition alowed
      sprim = cmsene**2*z1*z2
      beta  = alf1*(dlog(sprim*xi/amel**2) -1)
      gam   = 2d0*beta
!-----------------------------------------------------------------------
! Born Factor
      WTBOR = chi(xi)
      WTfin = wtfin3*wtfin4
!-----------------------------------------------------------------------
      vI  = x1*z3
      vF  = x3
      uI  = x2*z4
      uF  = x4
      v   = vI+vF
      u   = uI+uF
      FFac = FF(beta)
!-----------------------------------------------------------------------
      bt00  =0
      bt01  =0
      bt02  =0
      bt10u =0
      bt10l =0
      bt11u =0
      bt11l =0
      bt20u =0
      bt20l =0
      bt2ul =0
!-----------------------------------------------------------------------
! Crude distribution, 
! x**(beta1-1) divided out for the sake of numerical stability!
!-----------------------------------------------------------------------
!===> CRU1=   chi(x1) *eta *x1**(eta-1)
!===> CRU3=   chi(x3) *eta *x3**(eta-1)
!===> CRU2=   chi(x2) *eta *x2**(eta-1)
!===> CRU4=   chi(x4) *eta *x4**(eta-1)
      XRU1=   chi(x1) *eta *xmax**( eta-beta)*tt1**(1-beta/eta)
      XRU3=   chi(x3) *eta                   *tt3**(1-beta/eta)
      XRU2=   chi(x2) *eta *xmax**( eta-beta)*tt2**(1-beta/eta)
      XRU4=   chi(x4) *eta                   *tt4**(1-beta/eta)
!=======================================================================
!                         beta zero
!=======================================================================
!-----------------------------------------------------------------------
      DDCRU =  XRU1*XRU2*XRU3*XRU4
      bt00 =    YFS(2*beta,vF) *YFS(2*beta,uF)
!===>$     *FFac *beta *(x1*z3)**(beta-1) *z3   ! Upper Initial
!===>$     *FFac *beta *   (x3)**(beta-1)       ! Upper Final
!===>$     *FFac *beta *(x2*z4)**(beta-1) *z4   ! Lower Initial
!===>$     *FFac *beta *   (x4)**(beta-1)       ! Lower Final
     $     *FFac *beta *(   z3)**(beta-1) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $     *FFac *beta *(   z4)**(beta-1) *z4   ! Lower Initial
     $     *FFac *beta                          ! Lower Final
     $     /DDCRU
      bt01  = bt00*(1 + gam            )
      bt02  = bt00*(1 + gam +0.5*gam**2)
!=======================================================================
!                         beta one
!=======================================================================
!-----------------------------------------------------------------------
! Contributions from beta1 UPPER INITIAL line
!-----------------------------------------------------------------------
      bt11uI = 0
      IF(vI .GT. fleps ) THEN
         CRU1   =  chi(x1) *eta *x1**(eta-1)
         DDCRU  =  CRU1*XRU2*XRU3*XRU4
         bt11uI =  YFS(2*beta,vF) *YFS(2*beta,uF)
     $     *FFac *vI**beta  *RRI2(gam,vI) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $     *FFac *beta *(   z4)**(beta-1) *z4   ! Lower Initial
     $     *FFac *beta                          ! Lower Final
     $        /DDCRU
      ENDIF
!-----------------------------------------------------------------------
! Contributions from beta1 UPPER FINAL line
!-----------------------------------------------------------------------
      bt11uF = 0
      IF(vF .GT. fleps ) THEN
         CRU3   =  chi(x3) *eta *x3**(eta-1)
         DDCRU  =  XRU1*XRU2*CRU3*XRU4
         bt11uF =  YFS(2*beta,vF) *YFS(2*beta,uF)
     $     *FFac *beta *(   z3)**(beta-1) *z3 ! Upper Initial
     $     *FFac *vF**beta  *RRF2(gam,vF)     ! Upper Final
     $     *FFac *beta *(   z4)**(beta-1) *z4 ! Lower Initial
     $     *FFac *beta                        ! Lower Final
     $        /DDCRU
      ENDIF
      bt11u = bt11uI +bt11uF
!-----------------------------------------------------------------------
! Contributions from beta1 LOWER INITIAL line
!-----------------------------------------------------------------------
      bt11lI = 0
      IF(uI .GT. fleps ) THEN
         CRU2   =  chi(x2) *eta *x2**(eta-1)
         DDCRU  =  XRU1*CRU2*XRU3*XRU4
         bt11lI =    YFS(2*beta,vF) *YFS(2*beta,uF)
     $     *FFac *beta *(   z3)**(beta-1) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $     *FFac *uI**beta *RRI2(gam,uI)  *z4   ! Lower Initial
     $     *FFac *beta                          ! Lower Final
     $        /DDCRU
      ENDIF
!-----------------------------------------------------------------------
! Contributions from beta1 LOWER FINAL line
!-----------------------------------------------------------------------
      bt11lF = 0
      IF(uF .GT. fleps ) THEN
         CRU4   =  chi(x4) *eta *x4**(eta-1)
         DDCRU  =  XRU1*XRU2*XRU3*CRU4
         bt11lF =    YFS(2*beta,vF) *YFS(2*beta,uF)
     $     *FFac *beta *(   z3)**(beta-1) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $     *FFac *beta *(   z4)**(beta-1) *z4   ! Lower Initial
     $     *FFac *uF**beta     *RRF2(gam,uF)    ! Lower Final
     $        /DDCRU
      ENDIF
      bt11l = bt11lI +bt11lF
!=======================================================================
!                   beta two   Upper Lower
!=======================================================================
!-- Upper Initial * Lower Initial 
      bt2ulII = 0
      IF(vI .GT. fleps .AND. uI .GT. fleps) THEN
         CRU1   =  chi(x1) *eta *x1**(eta-1)
         CRU2   =  chi(x2) *eta *x2**(eta-1)
         DDCRU  =  CRU1*CRU2*XRU3*XRU4
         bt2ulII = YFS(2*beta,vF) *YFS(2*beta,uF)
     $        *FFac *vI**beta *RRI1(gam,vI)  *z3 ! Upper Initial
     $        *FFac *beta                        ! Upper Final
     $        *FFac *uI**beta *RRI1(gam,uI)  *z4 ! Lower Initial
     $        *FFac *beta                        ! Lower Final
     $        /DDCRU
      ENDIF
!-- Upper Final * Lower Final
      bt2ulFF = 0
      IF(vF .GT. fleps .AND. uF .GT. fleps) THEN
         CRU3   =  chi(x3) *eta *x3**(eta-1)
         CRU4   =  chi(x4) *eta *x4**(eta-1)
         DDCRU  =  XRU1*XRU2*CRU3*CRU4
         bt2ulFF =  YFS(2*beta,vF) *YFS(2*beta,uF)
     $        *FFac *beta *(   z3)**(beta-1) *z3 ! Upper Initial
     $        *FFac *vF**beta  *RRF1(gam,vF)     ! Upper Final
     $        *FFac *beta *(   z4)**(beta-1) *z4 ! Lower Initial
     $        *FFac *uF**beta  *RRF1(gam,uF)     ! Lower Final
     $        /DDCRU
      ENDIF
!-- Upper Initial * Lower Final
      bt2ulIF = 0
      IF(vI .GT. fleps .AND. uF .GT. fleps) THEN
         CRU1   =  chi(x1) *eta *x1**(eta-1)
         CRU4   =  chi(x4) *eta *x4**(eta-1)
         DDCRU  =  CRU1*XRU2*XRU3*CRU4
         bt2ulIF =  YFS(2*beta,vF) *YFS(2*beta,uF)
     $        *FFac *vI**beta  *RRI1(gam,vI) *z3 ! Upper Initial
     $        *FFac *beta                        ! Upper Final
     $        *FFac *beta *(   z4)**(beta-1) *z4 ! Lower Initial
     $        *FFac *uF**beta  *RRF1(gam,uF)     ! Lower Final
     $        /DDCRU
      ENDIF
      bt2ulFI = 0
!-- Upper Final * Lower Initial 
      IF(vF .GT. fleps .AND. uI .GT. fleps ) THEN
         CRU3   =  chi(x3) *eta *x3**(eta-1)
         CRU2   =  chi(x2) *eta *x2**(eta-1)
         DDCRU  =  XRU1*CRU2*CRU3*XRU4
         bt2ulFI =   YFS(2*beta,vF) *YFS(2*beta,uF)
     $        *FFac *beta *(   z3)**(beta-1) *z3 ! Upper Initial
     $        *FFac *vF**beta *RRF1(gam,vF)      ! Upper Final
     $        *FFac *uI**beta *RRI1(gam,uI)  *z4 ! Lower Initial
     $        *FFac *beta                        ! Lower Final
     $        /DDCRU
      ENDIF
      bt2ul = bt2ulII+bt2ulFF+bt2ulIF+bt2ulFI
!=======================================================================
!                   beta two   Upper Upper
!=======================================================================
      bt2uuIF = 0
!-- Upper Initial * Upper Final
      IF(vI .GT. fleps .AND. vF .GT. fleps ) THEN
         CRU1   =  chi(x1) *eta *x1**(eta-1)
         CRU3   =  chi(x3) *eta *x3**(eta-1)
         DDCRU  =  CRU1*XRU2*CRU3*XRU4
         bt2uuIF =   YFS(2*beta,vF) *YFS(2*beta,uF)
     $        *FFac *vI**beta                *z3 ! Upper Initial
     $        *FFac *vF**beta                    ! Upper Final
     $   *1d0/4*gam**2*( xkIF(vI,vF) +gam/2*wIF(vI,vF) )
     $        *FFac *beta *(   z4)**(beta-1) *z4 ! Lower Initial
     $        *FFac *beta                        ! Lower Final
     $        /DDCRU
      ENDIF
      bt2uuII = 0
!-- Upper Initial * Upper Initial
      IF(vI .GT. fleps ) THEN
         CRU1   =  chi(x1) *eta *x1**(eta-1)
         DDCRU  =  CRU1*XRU2*XRU3*XRU4
         bt2uuII  =  YFS(2*beta,vF) *YFS(2*beta,uF)
     $     *FFac *vI**beta  *b2II(gam,vI) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $     *FFac *beta *(   z4)**(beta-1) *z4   ! Lower Initial
     $     *FFac *beta                          ! Lower Final
     $        /DDCRU
      ENDIF
      bt2uuFF = 0
!-- Upper Final * Upper Final
      IF(vF .GT. fleps ) THEN
         CRU3   =  chi(x3) *eta *x3**(eta-1)
         DDCRU  =  XRU1*XRU2*CRU3*XRU4
         bt2uuFF =  YFS(2*beta,vF) *YFS(2*beta,uF)
     $     *FFac *beta *(   z3)**(beta-1) *z3 ! Upper Initial
     $     *FFac *vF**beta  *b2II(gam,vF)     ! Upper Final
     $     *FFac *beta *(   z4)**(beta-1) *z4 ! Lower Initial
     $     *FFac *beta                        ! Lower Final
     $        /DDCRU
      ENDIF
      bt20u=bt2uuIF+ bt2uuII + bt2uuFF
!=======================================================================
!                   beta two   Lower Lower
!=======================================================================
      bt2llIF = 0
!-- Lower Initial * Lower Final
      IF(uI .GT. fleps .AND. uF .GT. fleps ) THEN
         CRU2   =  chi(x2) *eta *x2**(eta-1)
         CRU4   =  chi(x4) *eta *x4**(eta-1)
         DDCRU  =  XRU1*CRU2*XRU3*CRU4
         bt2llIF =   YFS(2*beta,vF) *YFS(2*beta,uF)
     $     *FFac *beta *(   z3)**(beta-1) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $     *FFac *uI**beta                *z4   ! Lower Initial
     $     *FFac *uI**beta                      ! Lower Final
     $   *1d0/4*gam**2*( xkIF(uI,uF) +gam/2*wIF(uI,uF) )
     $        /DDCRU
      ENDIF
      bt2llII = 0
!-- Lower Initial * Lower Initial
      IF(uI .GT. fleps ) THEN
         CRU2   =  chi(x2) *eta *x2**(eta-1)
         DDCRU  =  XRU1*CRU2*XRU3*XRU4
         bt2llII  =  YFS(2*beta,vF) *YFS(2*beta,uF)
     $     *FFac *beta *(   z3)**(beta-1) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $     *FFac *uI**beta *b2II(gam,uI)  *z4   ! Lower Initial
     $     *FFac *beta                          ! Lower Final
     $        /DDCRU
      ENDIF
      bt2llFF = 0
!-- Lower Final * Lower Final
      IF(uF .GT. fleps ) THEN
         CRU4   =  chi(x4) *eta *x4**(eta-1)
         DDCRU  =  XRU1*XRU2*XRU3*CRU4
         bt2llFF =  YFS(2*beta,vF) *YFS(2*beta,uF)
     $     *FFac *beta *(   z3)**(beta-1) *z3   ! Upper Initial
     $     *FFac *beta                          ! Upper Final
     $     *FFac *beta *(   z4)**(beta-1) *z4   ! Lower Initial
     $     *FFac *uF**beta     *b2II(gam,uF)    ! Lower Final
     $        /DDCRU
      ENDIF
      bt20l=bt2llIF+ bt2llII + bt2llFF


!=======================================================================
!-----------------------------------------------------------------------
!                   Define MC weights
!-----------------------------------------------------------------------
!=======================================================================
      Facton =  WTBOR *WTfin
!           ---------------------------------
!           ! EXPON  UPPER + LOWER line     !
!           ---------------------------------
! All beta's:  O(alf0),O(alf1),O(alf2)
      WTSET( 40) =    bt00  *Facton
      WTSET( 41) =   (bt01+bt10u+bt10l)*Facton 
      WTSET( 42) =   (bt02+bt11u+bt11l+bt20u+bt20l+bt2ul)*Facton 
! Individual beta's in various orders.
! O(alf1)
      WTSET( 43) =    bt01*Facton                   ! beta0
      WTSET( 44) =   (bt10u+bt10l)*Facton           ! beta1
      WTSET( 45) =    bt10u*Facton                  ! beta1 components
      WTSET( 46) =    bt10l*Facton  
! O(alf2)
      WTSET( 47) =    bt02*Facton                   ! beta0
      WTSET( 48) =   (bt11u+bt11l)*Facton           ! beta1
      WTSET( 49) =   (bt20u+bt20l+bt2ul)*Facton     ! beta2
      WTSET( 50) =    bt11u*Facton                  ! beta1 components
      WTSET( 51) =    bt11l*Facton  
      WTSET( 52) =    bt2ul*Facton  
      WTSET( 53) =    bt20u*Facton                  ! beta2 components
      WTSET( 54) =    bt20l*Facton 
! Debug weights 
      WTSET( 55) =    bt2ulII*Facton
      WTSET( 56) =    bt2ulFF*Facton
      WTSET( 57) =    bt2llIF*Facton
      WTSET( 58) =    bt2llII*Facton
      WTSET( 59) =    bt2llFF*Facton
      WTSET( 60) =    bt2ulIF*Facton
      WTSET( 61) =    bt2ulFI*Facton
      WTSET( 62) =    bt11uI*Facton
      WTSET( 63) =    bt11uF*Facton
!-----------------------------------------------------------------------
      END

      FUNCTION RRI2(gam,V)
!     ***********************************
! O(alf2) Result of convolution of beta-1 contribution with near-by
! bunch of soft photons, on the same INITIAL state fermion leg.
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(CEULER =0.57721566D0)
!-----------------------------------------------------------------------
!   2*[N] = exp(DYFS(gamb,v)) *F(gamb) *[v^gamb]*b_0
!   [R_I^2(V)] =
!       + [N]*[1]^3*gam^3*[ln(1-V)]^2 * ( 1/32 - 1/64*V - 1/32*V^-1 )
!       + [N]*[1]^3*gam^3*[ln(1-V)] * (  - 1/32 + 1/32*V )
!       + [N]*[1]^3*gam^3*[Li2(V)] * ( 1/16 - 1/32*V )
!       + [N]*[1]^3*gam^3 * (  - 3/32*V )
!       + [N]*[1]^2*gam^2*[ln(1-V)] * ( 1/8 - 1/16*V - 1/8*V^-1 )
!       + [N]*[1]^2*gam^2 * (  - 1/2 + 1/8*V )
!       + [N]*[1]*gam * (  - 1/2 + 1/4*V );
!-----------------------------------------------------------------------
      RRI2 =
     $  +gam**3*log(1-V)**2 * ( 1d0/32 - 1d0/64*V - 1d0/32/V )
     $  +gam**3*log(1-V)    * (  - 1d0/32 + 1d0/32*V )
     $  +gam**3*dilogy(V)   * ( 1d0/16 - 1d0/32*V )
     $  +gam**3             * (  - 3d0/32*V )
     $  +gam**2*log(1-V)    * ( 1d0/8 - 1d0/16*V - 1d0/8/V )
     $  +gam**2             * (  - 1d0/2 + 1d0/8*V )
     $  +gam                * (  - 1d0/2 + 1d0/4*V )
      END
      FUNCTION RRI1(gam,V)
!     ***********************************
! O(alf1) Result of convolution...
!     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
!-----------------------------------------------------------------------
!   [R_I^1(V)] =
!       + [N]*[1]^3*gam^3 * (         1/16*V )
!       + [N]*[1]^2*gam^2 * (        - 1/8*V )
!       + [N]*[1]*gam     * (  - 1/2 + 1/4*V )
!-----------------------------------------------------------------------
      RRI1 =
     $     +gam**3     *(        +1d0/16*V )
     $     +gam**2     *(        - 1d0/8*V )
     $     +gam        *( -1d0/2 + 1d0/4*V )
      END


      FUNCTION RRF2(gam,V)
!     ***********************************
! O(alf2) Result of convolution of beta-1 contribution with near-by
! bunch of soft photons, on the same FINAL state fermion leg.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      SAVE
      PARAMETER(PI     =  3.1415926535897932D0) 
      PARAMETER(CEULER =0.57721566D0)
!-----------------------------------------------------------------------
!   2*[N] = exp(DYFS(gamb,v)) *F(gamb) *[v^gamb]*b_0
!   [R_F^2(V)] =
!       + [N]*[1]^3*gam^3*[ln(1-V)]^2 * (  - 1/32 + 1/64*V + 1/32*V^-1 )
!       + [N]*[1]^3*gam^3*[ln(1-V)] * ( 1/32 - 1/32*V )
!       + [N]*[1]^3*gam^3*[Li2(V)] * (  - 1/16 + 1/32*V )
!       + [N]*[1]^3*gam^3 * (  - 1/32*V )
!       + [N]*[1]^2*gam^2*[ln(1-V)] * (  - 1/8 + 1/16*V + 1/8*V^-1 )
!       + [N]*[1]^2*gam^2 * (  - 1/2 + 1/8*V )
!       + [N]*[1]*gam * (  - 1/2 + 1/4*V );
!-----------------------------------------------------------------------
      RRF2 =
     $  +gam**3*log(1-V)**2 * (  - 1d0/32 + 1d0/64*V + 1d0/32/V )
     $  +gam**3*log(1-V)    * ( 1d0/32 - 1d0/32*V )
     $  +gam**3*dilogy(V)   * (  - 1d0/16 + 1d0/32*V )
     $  +gam**3             * (  - 1d0/32*V )
     $  +gam**2*log(1-V)    * (  - 1d0/8 + 1d0/16*V + 1d0/8/V )
     $  +gam**2             * (  - 1d0/2 + 1d0/8*V )
     $  +gam                * (  - 1d0/2 + 1d0/4*V )
      END
      FUNCTION RRF1(gam,V)
!     ***********************************
! O(alf1) Result of convolution...
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
!-----------------------------------------------------------------------
!   [R_F^1(V)] =
!       + [N]*[1]^3*gam^3 * ( 1/16*V )
!       + [N]*[1]^2*gam^2 * (  - 1/8*V )
!       + [N]*[1]*gam * (  - 1/2 + 1/4*V );
!-----------------------------------------------------------------------
      RRF1 =
     $     +gam**3     *(        +1d0/16*V )
     $     +gam**2     *(        - 1d0/8*V )
     $     +gam        *( -1d0/2 + 1d0/4*V )
      END


      FUNCTION wIF(v1,v2)
!     ***********************************
! Element of beta2UU calculation
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
**!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
**!%        [wIF(v1,v2)] =
**!%       + v1*[1-v2]^-1 * (  - 1/8 )
**!%       + v1*[1-v2]^-2 * (  - 1/8 )
**!%       + v2*[1-v1]^-1 * ( 1/8 )
**!%       + v2*[1-v1]^-2 * (  - 1/8 )
**!%       + ln(1 - v1)*v2*[1-v1]^-1 * ( 1/8 )
**!%       + ln(1 - v1)*v2*[1-v1]^-2 * ( 1/8 )
**!%       + ln(1 - v1)*[1-v1]^-1 * (  - 1/4 );
**!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
**      wIF=
**     $ + v1/(1-v2)    * (  - 1d0/8 )
**     $ + v1/(1-v2)**2 * (  - 1d0/8 )
**     $ + v2/(1-v1)    * ( 1d0/8 )
**     $ + v2/(1-v1)**2 * (  - 1d0/8 )
**     $ + log(1 - v1)*v2/(1-v1)    * ( 1d0/8 )
**     $ + log(1 - v1)*v2/(1-v1)**2 * ( 1d0/8 )
**     $ + log(1 - v1)   /(1-v1)    * (-1d0/4 )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%   [wIF(v1,v2)] =
!%       +            v1*[1-v2]^-2 * (-1/2 )
!%       + ln(1 - v2)   *[1-v2]^-1 * (-1/2 );
!%       + ln(1 - v2)*v1*[1-v2]^-1 * ( 1/4 )
!%       + ln(1 - v2)*v1*[1-v2]^-2 * ( 1/4 )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      wIF=
     $ +             v1/(1-v2)**2 * (-1d0/2 )
     $ + log(1 - v2)   /(1-v2)    * (-1d0/2 )
     $ + log(1 - v2)*v1/(1-v2)    * ( 1d0/4 )
     $ + log(1 - v2)*v1/(1-v2)**2 * ( 1d0/4 )
      END

      FUNCTION xkIF(v1,v2)
!     ***********************************
! Element of beta2UU calculation
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
**!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
**!%   [k(v1,v2)] =
**!%       + v1*[1-v2]^-1 * ( 1/8 )
**!%       + v1*[1-v2]^-2 * ( 1/8 )
**!%       + v2*[1-v1]^-1 * ( 1/8 )
**!%       + v2*[1-v1]^-2 * ( 1/8 )
**!%       + [1-v1]^-1 * (  - 1/4 )
**!%       + [1-v2]^-1 * (  - 1/4 )
**!%       + 1/2;
**!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
**      xkIF=
**     $ + v1/(1-v2)    * ( 1d0/8 )
**     $ + v1/(1-v2)**2 * ( 1d0/8 )
**     $ + v2/(1-v1)    * ( 1d0/8 )
**     $ + v2/(1-v1)**2 * ( 1d0/8 )
**     $ + 1/(1-v1)     * (-1d0/4 )
**     $ + 1/(1-v2)     * (-1d0/4 )
**     $ + 1d0/2
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%   [kIF(v1,v2)] =
!%       +                  1/2;
!%       +    [1-v2]^-1 * (-1/2 )
!%       + v1*[1-v2]^-1 * ( 1/4 )
!%       + v1*[1-v2]^-2 * ( 1/4 )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      xkIF=
     $ +                  1d0/2
     $ +  1/(1-v2)    * (-1d0/2 )
     $ + v1/(1-v2)    * ( 1d0/4 )
     $ + v1/(1-v2)**2 * ( 1d0/4 )
      END

      FUNCTION b2II(gam,v)
!     ***********************************
! Element of beta2UU calculation
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%   [d_0^*(v)] =
!%       + ln(1 - v) * ( 1/2 - 1/4*v )
!%       + 1/2*v;
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      dd0 =  log(1 - v) * ( 1d0/2 - 1d0/4*v )+ 1d0/2*v
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%   [d_1^*(v)] =
!%       + ln(1 - v) * (  - 1/4 + 1/4*v )
!%       + Li2( - 1/(1 - v)*v) * (  - 1/2 + 1/4*v )
!%       - 3/4*v;
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      dd1 =
     $     + log(1 - v)               * (  - 1d0/4 + 1d0/4*v )
     $     + dilogy( - 1d0/(1 - v)*v) * (  - 1d0/2 + 1d0/4*v )
     $     - 3d0/4*v
!------
      b2II = 1d0/8 *gam**2 *(dd0 +gam/2*dd1)
      END
      SUBROUTINE m2agzi(MODE)
!     ***********************
!======================================================================!
!======================================================================!
!    Z boson exchange contribution to low angle Bhabha scattering,     !
!    both exponentiated and non-exponentiated up to O(alpha^1).        !
!======================================================================!
!======================================================================!
! IMPORTANT: The non-exponentiated version should be run with a photon !
!            removal option switched ON, i.e. KEYREM=0 !!!             !
!======================================================================!
! The following weights are implemented (Z-contributions only!):       !
! NOTE: For practical calculations of the Z exchange contribution      !
!       in the luminosity measurement we RECOMMEND to use the weight:  !
!       wtset(12)                                                      !
!       ^^^^^^^^^                                                      !
!======================================================================!
!    This routine fills wtset(10-29) and wtset(80-99)                  !
!----------------------------------------------------------------------!
!    1) STANDARD version with EXPONENTIATION                           !
!      wtset(10)  -  O(alpha^0)exp                                     !
!      wtset(11)  -  O(alpha^1)exp, no vacuum polarization             !
! ==>  wtset(12)  -  O(alpha^1)exp, with vacuum polarization    <===   !
!----------------------------------------------------------------------!
!    2) SPECIAL version with EXPONENTIATION                            !
!    with explicit Z self energy correction                            !
!    (for some tests, mainly comparisons with ALIBABA):                !
!      wtset(20)  -  O(alpha^0)exp                                     !
!      wtset(21)  -  O(alpha^1)exp, no vac. pol., no Z self en.        !
!      wtset(22)  -  O(alpha^1)exp, with vac. pol. and Z self en.      !
! Note: Version 2 gives generally the same results as version 1,       !
!       but it is kept for the sake of some tests.                     !
!======================================================================!
!    Non-exponentiated case:                                           !
!    3) STANDARD version                                               !
!      wtset(80)  -  O(alpha^0)                                        !
!      wtset(81)  -  O(alpha^1), no vacuum polarization                !
! -->  wtset(82)  -  O(alpha^1), with vacuum polarization       <---   !
!----------------------------------------------------------------------!
!    4) SPECIAL version                                                !
!    with O(alpha^1) ansatz by W.Beenakker     !
!    & B.Pietrzyk (for some tests, mainly comparisons with BABAMC):    !
!      wtset(90)  -  O(alpha^0)                                        !
!      wtset(91)  -  O(alpha^1), no vac. pol., no Z self en.           !
!      wtset(92)  -  O(alpha^1), with vac. pol. and Z self en.         !
!======================================================================!
!    In order to make the program more efficient we introduced         !
!    a number of keys which allow the user to choose a desired         !
!    version of the Z- contribution calculation from the list          !
!    above (any combination of them can be chosen as well).            !
!    Those keys are provided trough the integer array: KeyZex(4)       !
!    and have the meaning as follows:                                  !
!      KeyZex(1) =0/1   version no 1 (see above) switched OFF/ON       !
!      KeyZex(2) =0/1   version no 2 (see above) switched OFF/ON       !
!      KeyZex(3) =0/1   version no 3 (see above) switched OFF/ON       !
!      KeyZex(4) =0/1   version no 4 (see above) switched OFF/ON       !
!    The default set up is: DATA KeyZex /1,0,0,0/                      !
!======================================================================!
!    Written by:  Wieslaw Placzek          Knoxville, March 1995       !
!    Last update: 12.10.1995               by: W. Placzek              !
!**********************************************************************!
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER( ALFA= 1D0/ALFINV, ALF1 = 1D0/PI/ALFINV)
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / PSIPHI / TH1,EXT1,EXB1,PSI1,EXW1,
     $                  TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      SAVE   / BHPAR2 /, / BHPAR3 /, / TRANSR /
      SAVE   / MOMS1  /, / MOMS2  /, / MOMZ1  /, / MOMZ2  /
      SAVE   / WGTALL /, / BHPARZ /, / PSIPHI /
      DIMENSION RP(4),RQ(4)
      DIMENSION KeyZex(4)
! Array of keys for Z-contributions switch on/off and a default set-up
      DATA KeyZex /1,0,1,0/
      SAVE KeyZex
! --------------------------------------------------------------------
      DATA icont /0/
      SAVE icont
! ------------------ Inline functions ------------------
! Dot products for some Lorentz invariants
      sdot(x1,y1,z1,e1,x2,y2,z2,e2)
     $            = (e1+e2)**2-(x1+x2)**2-(y1+y2)**2-(z1+z2)**2
      tdot(x1,y1,z1,e1,x2,y2,z2,e2)
     $            = (e1-e2)**2-(x1-x2)**2-(y1-y2)**2-(z1-z2)**2
! --------------------------------------------------------------------
      icont=icont+1
! .........................................
      DO k=1,4
        RP(k)= P2(K)+PHSU1(K)
        RQ(k)= Q2(K)+PHSU2(K)
      ENDDO
! Some invariants
      tr = -TRAN
      t  = -TRANP
      t1 = -TRANQ
      s  = CMSEne**2
      s1 = sdot(P2(1),P2(2),P2(3),P2(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      u  = tdot(P1(1),P1(2),P1(3),P1(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      u1 = tdot(P2(1),P2(2),P2(3),P2(4) ,Q1(1),Q1(2),Q1(3),Q1(4))
      xpp  = P2(4)*P1(4)-P2(3)*P1(3)-P2(2)*P1(2)-P2(1)*P1(1)
      xpr  = P1(4)*RP(4)-P1(3)*RP(3)-P1(2)*RP(2)-P1(1)*RP(1)
      xqq  = Q2(4)*Q1(4)-Q2(3)*Q1(3)-Q2(2)*Q1(2)-Q2(1)*Q1(1)
      xqr  = Q1(4)*RQ(4)-Q1(3)*RQ(3)-Q1(2)*RQ(2)-Q1(1)*RQ(1)
!.....Variables for O(alpha1) virtual+soft corrections
      wdz = 0.5*dlog(TRAN/AMEL**2)
      vdz = 0.5*dlog(s/AMEL**2)
      vdz1= 0.5*dlog(s1/AMEL**2)
      udz = 0.5*dlog(abs(4*AMEL**2-s -tr)/AMEL**2)
      udz1= 0.5*dlog(abs(4*AMEL**2-s1-tr)/AMEL**2)
!*****************************************************
!  Crude MC distribution (S-tilde factors omitted)
      crude  =  s**2/tran**2
      deltp  =  AMEL**2/TRANP
      deltq  =  AMEL**2/TRANQ
      gamp   =  2*alf1 *( dlog(1/deltp)-1 )
      gamq   =  2*alf1 *( dlog(1/deltq)-1 )
!==================================================================!
      b10uza =0
      b10uzv =0
      b10lza =0
      b10lzv =0
      s10uza =0
      s10uzv =0
      s10lza =0
      s10lzv =0
! Vacuum polarization factors
      vpft  = 1/(1+RePi(-TRAN))       ! <===
      vpfac = 1/(1+RePi(  s )) *vpft
      vpfac1= 1/(1+RePi(  s1)) *vpft

!        *************************************************
!        *************************************************
!        **              EXPONENTIATION                 **
!        *************************************************
!        *************************************************
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!>>> If exp. version is chosen
      IF (KeyZex(1) .EQ. 1 .OR. KeyZex(2) .EQ. 1) THEN
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!... Z mass
      amZ  = AMAZ
      amZ2 = amZ**2
!... s-shift of the Z-width
      GamZ = GAMMZ *s /amZ2
      GamZ1= GAMMZ *s1/amZ2
!... Z coupling const.
      Zcc = gv**2 + ga**2
!... kinematical factors for Z contribution
      fasc  = Zcc *(t+t1)*(u**2+u1**2)/(4*t*t1)
      facZZ = Zcc**2 *(u**2+u1**2+t**2+t1**2)/4
*--------------------------------------------------------------------
*--------------------------------------------------------------------
!>>> If chosen version no 1
      IF (KeyZex(1) .EQ. 1) THEN
*--------------------------------------------------------------------
*--------------------------------------------------------------------
      prki = fasc/( (s -amZ2)**2 + (amZ*gamZ )**2 )
      prki1= fasc/( (s1-amZ2)**2 + (amZ*gamZ1)**2 )
!... Real and imaginary contributions at the lowest order
      delZR = (s -amZ2) *prki
      delZI =-gamZ *amZ *prki
!... with s-shift (s --> s1)
      delZR1= (s1-amz2) *prki1
      delZI1=-gamZ1*amZ *prki1
!... with vacuum polar. corr.
      devZR = delZR *vpfac   ! <===
      devZI = delZI *vpfac   ! <===
      devZR1= delZR1*vpfac1  ! <===
      devZI1= delZI1*vpfac1  ! <===
!... Z-gamma interference, soft + virt. corrections to beta_0
      psis = dacos( (amZ2-s )/sqrt((s -amZ2)**2+(amZ*GamZ )**2))
      psis1= dacos( (amZ2-s1)/sqrt((s1-amZ2)**2+(amZ*GamZ1)**2))
      delRe =2*(wdz-udz)*dlog(s/(4*(sqrt(s)-amZ)**2+GamZ**2))
     $      +2*(vdz**2-udz**2 ) +4*wdz*(udz -vdz) +vdz +wdz
     $      +2*udz  +dilogy(1-TRAN/s) -dilogy(TRAN/s) -5*pi**2/6 -2
      delIm = pi*( 2*(wdz-vdz) + 4*(udz -wdz)*psis/pi + 1.5 )
      delRe1=2*(wdz-udz1)*dlog(s1/(4*(sqrt(s1)-amZ)**2+gamZ1**2))
     $      +2*(vdz1**2-udz1**2) +4*wdz*(udz1-vdz1)+ vdz1 +wdz
     $      +2*udz1 +dilogy(1-TRAN/s1) -dilogy(TRAN/s1) -5*pi**2/6 -2
      delIm1=pi*( 2*(wdz-vdz1) + 4*(udz1-wdz)*psis1/pi + 1.5 )
      gZRe = 1 +alf1*delRe
      gZIm = alf1*delIm
      gZRe1= 1 +alf1*delRe1
      gZIm1= alf1*delIm1
!=================================================================
!=================================================================
!                           beta0
!=================================================================
!=================================================================
!... no vacuum pol. cor.
      delZa = delZR *gZRe  + delZI *gZIm
      delZa1= delZR1*gZRe1 + delZI1*gZIm1
!... with vacuum pol. cor.
      delZv = devZR *gZRe  + devZI *gZIm   ! <===
      delZv1= devZR1*gZRe1 + devZI1*gZIm1  ! <===
!... Born Z(s)-Z(s) contribution, no vac. pol. corr.
      deZZa = facZZ/( (s -amZ2)**2 + (amZ*gamZ )**2 )
      deZZa1= facZZ/( (s1-amZ2)**2 + (amZ*gamZ1)**2 )
!... with vac. pol. corr.
      deZZv = deZZa *(vpfac /vpft)**2  ! <===
      deZZv1= deZZa1*(vpfac1/vpft)**2  ! <===
!-----
      bt01Za = (delZa+delZa1)/2 +(deZZa+deZZa1)/2
      bt01Zv = (delZv+delZv1)/2 +(deZZv+deZZv1)/2  ! <===
!-----
!... some factors for calculation of hard photon contrib.
      Zpro = (s -amZ2)**2 +(amZ*GamZ )**2
      Zpro1= (s1-amZ2)**2 +(amZ*GamZ1)**2
      faRe = Zcc*(s -amZ2)*(u**2+u1**2)
      faRe1= zcc*(s1-amz2)*(u**2+u1**2)
      faIm = 2*gv*ga *amZ*gamZ *(u**2-u1**2)*s
      faIm1= 2*gv*ga *amZ*gamZ1*(u**2-u1**2)*s
      ENDIF
*--------------------------------------------------------------------
*--------------------------------------------------------------------
!>>> If chosen version no 2
      IF (KeyZex(2) .EQ. 1) THEN
*--------------------------------------------------------------------
*--------------------------------------------------------------------
!... the same as above but with the explicit Z self energy correction
      seki = fasc/( (s -amZ2)**2 + (amZ*GAMMZ)**2 )
      seki1= fasc/( (s1-amZ2)**2 + (amZ*GAMMZ)**2 )
!... Real and imaginary contributions at the lowest order
      selZR = (s -amZ2) *seki
      selZI =-GAMMZ*amZ *seki
      selZR1= (s1-amZ2) *seki1
      selZI1=-GAMMZ*amZ *seki1
!... Z self energy corr.; from W. Beenakker & B. Pietrzyk, CERN-TH.6649/92
      Zg = GAMMZ/amZ
      Zy = (s -amZ2)/(amZ*GAMMZ )
      Zy1= (s1-amZ2)/(amZ*GAMMZ)
      PiZ= RePi(amZ2)
      Zsfac = ( 1 - 2*Zy *Zg/(1+Zy**2 ) )/(1+PiZ)
      Zsfac1= ( 1 - 2*Zy1*Zg/(1+Zy1**2) )/(1+PiZ)
!... with vac. pol. and Z self en. corr.
      sevZR = selZR *vpft*Zsfac
      sevZI = selZI *vpft*Zsfac
      sevZR1= selZR1*vpft*Zsfac1
      sevZI1= selZI1*vpft*Zsfac1
!... Z-gamma interference, Born + virt. corrections to beta_0
      psise = dacos( (amZ2-s )/sqrt((s -amZ2)**2+(amZ*GAMMZ)**2))
      psise1= dacos( (amZ2-s1)/sqrt((s1-amZ2)**2+(amZ*GAMMZ)**2))
      selRe =2*(wdz-udz)*dlog(s/(4*(sqrt(s)-amZ)**2+GAMMZ**2))
     $      +2*(vdz**2-udz**2 ) +4*wdz*(udz -vdz) +vdz +wdz
     $      +2*udz  +dilogy(1-TRAN/s) -dilogy(TRAN/s) -5*pi**2/6 -2
      selIm = pi*( 2*(wdz-vdz) + 4*(udz -wdz)*psise/pi + 1.5 )
      selRe1=2*(wdz-udz1)*dlog(s1/(4*(sqrt(s1)-amZ)**2+GAMMZ**2))
     $      +2*(vdz1**2-udz1**2) +4*wdz*(udz1-vdz1)+ vdz1 +wdz
     $      +2*udz1 +dilogy(1-TRAN/s1) -dilogy(TRAN/s1) -5*pi**2/6 -2
      selIm1=pi*( 2*(wdz-vdz1) + 4*(udz1-wdz)*psise1/pi + 1.5 )
      sZRe = 1 +alf1*selRe
      sZIm = alf1*selIm
      sZRe1= 1 +alf1*selRe1
      sZIm1= alf1*selIm1
!=================================================================
!=================================================================
!                           beta0
!=================================================================
!=================================================================
!... no vacuum pol., no Z self en.
      selZa = selZR *sZRe  + selZI *sZIm
      selZa1= selZR1*sZRe1 + selZI1*sZIm1
C... with vacuum pol. and Z self en.
      selZv = sevZR *sZRe  + sevZI *sZIm
      selZv1= sevZR1*sZRe1 + sevZI1*sZIm1
C... Born Z(s)-Z(s) contribution, no Z-self energy corr.
      seZZa = facZZ/( (s -amZ2)**2 + (amZ*GAMMZ)**2 )
      seZZa1= facZZ/( (s1-amZ2)**2 + (amZ*GAMMZ)**2 )
C... with Z-self energy corr.
      seZZv = seZZa *Zsfac**2
      seZZv1= seZZa1*Zsfac1**2
!-----
      st01Za = (selZa+selZa1)/2 +(seZZa+seZZa1)/2
      st01Zv = (selZv+selZv1)/2 +(seZZv+seZZv1)/2
!-----
!... some factors for calculation of hard photon contrib.
      spro = (s -amZ2)**2 +(amZ*GAMMZ)**2
      spro1= (s1-amZ2)**2 +(amZ*GAMMZ)**2
      saRe = Zcc*(s -amZ2)*(u**2+u1**2)
      saRe1= Zcc*(s1-amZ2)*(u**2+u1**2)
      saIm = 2*gv*ga *amZ*GAMMZ*(u**2-u1**2)*s
      ENDIF

!=================================================================
!=================================================================
!              Contributions from beta1 UPPER line
!=================================================================
!=================================================================
      DO i=1,nphot1
! numerically safe variables
        a = al1(i)
        b = be1(i)
        y = a +b*deltp
        z = b +a*deltp
! one photon bremss. distribution
        p1k = xpr*z
        p2k = xpr*y
        q1k=Q1(4)*PHOT1(i,4)
        q2k=Q2(4)*PHOT1(i,4)
        DO ii=1,3
          q1k=q1k -Q1(ii)*PHOT1(i,ii)
          q2k=q2k -Q2(ii)*PHOT1(i,ii)
        ENDDO
! soft factor
        sfcc = 2*xpp/(p1k*p2k)
        epsp2k=P2(1)*PHOT1(i,2)-P2(2)*PHOT1(i,1)
*--------------------------------------------------------------------
*--------------------------------------------------------------------
!>>> If chosen version no 1
      IF (KeyZex(1) .EQ. 1) THEN
*--------------------------------------------------------------------
*--------------------------------------------------------------------
! no s-shift
        au0=( faRe *(sfcc +s1/(p2k*q2k) +u/(p1k*q2k) +4/p2k)
     $       +faIm *epsp2k/(p1k*p2k*q2k) )/Zpro /t1/4
! with s-shift
        au1=( faRe1*(sfcc +s/(p1k*q1k) +u1/(p2k*q1k) -4/p1k)
     $       -faIm1*epsp2k/(p1k*p2k*q1k) )/Zpro1/t1/4
! no vac. pol.
        au1a=au0 +au1
! with vac. pol.
        au1v= au0*vpfac + au1*vpfac1          ! <===
! beta1 O(alf1)
        bu10Za = au1a/sfcc -(delZR+delZR1)/2
        bu10Zv = au1v/sfcc -(devZR+devZR1)/2  ! <===
        b10uZa = b10uZa +bu10Za
        b10UZv = b10uZv +bu10Zv  ! <===
      ENDIF
*--------------------------------------------------------------------
*--------------------------------------------------------------------
!>>> If chosen version no 2
      IF (KeyZex(2) .EQ. 1) THEN
*--------------------------------------------------------------------
*--------------------------------------------------------------------
!... as above but for Z self energy case
! no s-shift
        su0=( saRe *(sfcc +s1/(p2k*q2k) +u/(p1k*q2k) +4/p2k)
     $       +saIm *epsp2k/(p1k*p2k*q2k) )/spro /t1/4
! with s-shift
        su1=( saRe1*(sfcc +s/(p1k*q1k) +u1/(p2k*q1k) -4/p1k)
     $       -saIm *epsp2k/(p1k*p2k*q1k) )/spro1/t1/4
! no vac. pol., no Z self en.
        su1a=su0 +su1
! with vac. pol. and Z self en.
        su1v=(su0*Zsfac + su1*Zsfac1) *vpft
! beta1 O(alf1)
        su10Za = su1a/sfcc -(selZR+selZR1)/2
        su10Zv = su1v/sfcc -(sevZR+sevZR1)/2
        s10uZa = s10uZa  +su10Za
        s10uZv = s10uZv  +su10Zv
      ENDIF
*--------------------------------------------------------------------
*--------------------------------------------------------------------
      ENDDO
!=================================================================
!=================================================================
!             Contributions from beta1 LOWER line
!=================================================================
!=================================================================
      DO i=1,nphot2
! numerically safe variables
        a = al2(i)
        b = be2(i)
        y = a+b*deltq
        z = b+a*deltq
! one photon bremss. distribution
        q1k = xqr*z
        q2k = xqr*y
        p1k=P1(4)*PHOT2(i,4)
        p2k=P2(4)*PHOT2(i,4)
        DO ii=1,3
          p1k=p1k-P1(ii)*PHOT2(i,ii)
          p2k=p2k-P2(ii)*PHOT2(i,ii)
        ENDDO
! soft factor
        sfcc = 2*xqq/(q1k*q2k)
        epsp2k=P2(1)*PHOT2(i,2)-P2(2)*PHOT2(i,1)
*--------------------------------------------------------------------
*--------------------------------------------------------------------
!>>> If chosen version no 1
      IF (KeyZex(1) .EQ. 1) THEN
*--------------------------------------------------------------------
*--------------------------------------------------------------------
! no s-shift
        ad0=( faRe *(sfcc +s1/(p2k*q2k) +u1/(p2k*q1k) +4/q2k)
     $       +faIm *epsp2k/(p2k*q1k*q2k) )/Zpro /t/4
! with s-shift
        ad1=( faRe1*(sfcc +s/(p1k*q1k)  +u /(p1k*q2k) -4/q1k)
     $       -faIm1*epsp2k/(p1k*q1k*q2k) )/Zpro1/t/4
! no vac. pol.
        ad1a=ad0 +ad1
! with vac. pol.
        ad1v=ad0*vpfac + ad1*vpfac1           ! <===
! beta1 O(alf1)
        bl10Za = ad1a/sfcc -(delZR+delZR1)/2
        bl10Zv = ad1v/sfcc -(devZR+devZR1)/2  ! <===
        b10lZa = b10lZa  +bl10Za
        b10lZv = b10lZv  +bl10Zv  ! <===
      ENDIF
*--------------------------------------------------------------------
*--------------------------------------------------------------------
!>>> If chosen version no 2
      IF (KeyZex(2) .EQ. 1) THEN
*--------------------------------------------------------------------
*--------------------------------------------------------------------
!... as above but for Z self energy case
! no s-shift
        sd0=( saRe *(sfcc +s1/(p2k*q2k) +u1/(p2k*q1k) +4/q2k)
     $       +saIm *epsp2k/(p2k*q1k*q2k) )/spro /t/4
! with s-shift
        sd1=( saRe1*(sfcc +s/(p1k*q1k)  +u /(p1k*q2k) -4/q1k)
     $       -saIm *epsp2k/(p1k*q1k*q2k) )/spro1/t/4
! no vac. pol., no Z self en.
        sd1a=sd0 +sd1
! with vac. pol.
        sd1v=(sd0*Zsfac + sd1*Zsfac1) *vpft
! beta1 O(alf1)
        sl10Za = sd1a/sfcc -(selZR+selZR1)/2
        sl10Zv = sd1v/sfcc -(sevZR+sevZR1)/2
        s10lZa = s10lZa  +sl10Za
        s10lZv = s10lZv  +sl10Zv
      ENDIF
*--------------------------------------------------------------------
*--------------------------------------------------------------------
      ENDDO

!        *************************************************
!        *************************************************
!        **              EXPONENTIATION                 **
!        **          Definitions of MC weights          **
!        *************************************************
!        *************************************************
!              ---------------------------------
!              !  UPPER + LOWER line,    (A)   !
!              ---------------------------------
! zero order
      wtset( 10) = ( (delZR+delZR1)/2 +(deZZa+deZZa1)/2 )/crude
! first order - no vac. pol.
      wtset( 11) = (bt01Za + b10uZa + b10lZa) /crude
! first order - with vac. pol.                        ! <=== RECOMMENDED !!!
      wtset( 12) = (bt01Zv + b10uZv + b10lZv) /crude  ! <=== RECOMMENDED !!!
!... as above but with explicit Z self energy corr.
!    (for some tests, e.g. comparisons with ALIBABA)
! zero order
      wtset( 20) = ( (selZR+selZR1)/2 +(seZZa+seZZa1)/2 )/crude
! first order - no vac. pol., no Z self en.
      wtset( 21) = (st01Za + s10uZa + s10lZa) /crude
! first order - with vac. pol. and Z self energy
      wtset( 22) = (st01Zv + s10uZv + s10lZv) /crude
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      ENDIF
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||



!==================================================================
!==================================================================
!                  NO-EXPONENTIATION version                      !
!==================================================================
!==================================================================

!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!>>> If non-exp. version is chosen
      IF (KeyZex(3) .EQ. 1 .OR. KeyZex(4) .EQ. 1) THEN
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      delZB  =0
      delZ1a =0
      delZ1v =0
      selZ0  =0
      selZ1a =0
      selZ1v =0
      sevac  =0
      seZse  =0
!... Z mass
      amZ  = AMAZ
      amZ2 = amZ**2
!... s-shift of the Z-width
      GamZ = GAMMZ *s /amZ2
      GamZ1= GAMMZ *s1/amZ2
!... Z-width without shift
      GZex = GAMMZ
!... Z-width as in original BABAMC
      GZba= 2.3098
!... sin^2(theta_W) the same as in BABAMC
      sw2 = 0.2273
      ga1 = -1/(4*sqrt(sw2*(1-sw2)))
      gv1 = ga1*(1-4*sw2)
      Zcc1 = gv1**2 + ga1**2
!
      IF(nphot1 .EQ. 0.and.nphot2 .EQ. 0) THEN
!===================================================================
!===================================================================
!                        [00] No photons
!===================================================================
!===================================================================
!------
! O(alf0,1) distribution
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 3
        IF (KeyZex(3) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
        fki = Zcc *u**2/t
        fkZ = Zcc**2 *(u**2+t**2)/2
        Zpro =(s-amZ2)**2 + (GamZ*amZ)**2
! Born gamma(t)-Z(s) contribution
        dgZts = fki*(s-amZ2)/Zpro
! Born Z(s)-Z(s) contribution
        dZZss =fkZ/Zpro
! Lowest order contrib. with Z-width shift
        delZ0 = dgZts + dZZss
! Total Born Z contribution
        delZB = delZ0*Zpro/( (s-amZ2)**2 + (GAMMZ*amZ)**2 )
        ENDIF
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 4
        IF (KeyZex(4) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!... for comparisons with BABAMC
        ski = Zcc1 *u**2/t
        skZ = Zcc1**2 *(u**2+t**2)/2
        sproe =(s-amZ2)**2 + (GZex*amZ)**2
        sprob =(s-amZ2)**2 + (GZba*amZ)**2
! Born gamma(t)-Z(s) contribution
        sgZtse = ski*(s-amZ2)/sproe
        sgZtsb = ski*(s-amZ2)/sprob
! Born Z(s)-Z(s) contribution
        sZZsse=skZ/sproe
        sZZssb=skZ/sprob
! Total Born Z contribution
        selZ0 = sgZtse + sZZsse
! for the original BABAMC Z-width
        selZ0b= sgZtsb+ sZZssb
        ENDIF
*-------------------------------------------------
	Eb  = CMSEne/2
	xkm = epsCM*Eb             ! soft foton cut-off in CMS
!####################################################################
! case for CMSEne=amZ
	IF (abs(CMSEne-amZ).lt.1d-10) THEN
!####################################################################
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 3
          IF (KeyZex(3) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
          gZRe = 2*datan(2*xkm/GamZ)*(1+2*udz-2*vdz-2*wdz)
          gZIm = -pi*(2*(udz-vdz)+1.5)
! virtual+soft corretion, no vac. pol. corr.
          delZa = delZ0 + alf1*fki/amZ/GamZ*(gZRe+gZIm)
! with vac. pol. corr.
          delZv =( delZa + dZZss*(vpfac/vpft**2 -1) )*vpfac ! <---
          delZ1a =delZa
          delZ1v =delZv ! <---
          ENDIF
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 4
          IF (KeyZex(4) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
C... for comparisons with BABAMC
          sZRe = 2*datan(2*xkm/GZex)*(1+2*udz-2*vdz-2*wdz)
          sZIm = -pi*(2*(udz-vdz)+1.5)
! virtual+soft corretion, no Z self en. corr.
          selZa = selZ0 + alf1*ski/amZ/GZex*(sZRe+sZIm)
! Z self en. corr
          Zsefa = RePi(amZ2)
! virtual+soft corretion, with Z self en. corr.
          selZv = selZa + 2*sZZssb*Zsefa + selZ0b-selZ0
          selZ1a =selZa
          selZ1v =selZv
          ENDIF
!####################################################################
! otherwise (i.e. away from the Z peak)
        ELSE
!####################################################################
          hit = -t/s
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 3
          IF (KeyZex(3) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
          R0  = (CMSEne    -amZ)**2 +GamZ**2/4
          Rkm = (CMSEne-xkm-amZ)**2 +GamZ**2/4
          fiZ = datan( (-2*(CMSEne-amZ)+2*xkm)/GamZ )
     $         -datan(  -2*(CMSEne-amZ)       /GamZ )
          psis= dacos( (amZ2-s)/sqrt((s-amZ2)**2+(amZ*GamZ)**2) )
          gZRi = -2*( 1+2*(udz-vdz-wdz) )*( dlog(xkm/Eb)
     $                                     -gamZ/2/(CMSEne-amZ)*fiZ )
     $           -(1-2*vdz)*dlog((xkm/Eb)**2*R0/Rkm)
     $           -2*(udz-wdz)*dlog(xkm**2/Rkm)
     $           -2*udz**2 +2*vdz**2 +4*wdz*(udz-vdz) +3*(vdz+wdz)
     $           +dilogy(1-hit) -dilogy(hit) +pi**2/6 -4
     $           -GamZ*amZ/(s-amz2)*pi*(2*(wdz-vdz)+1.5
     $                                +4*(udz-wdz)*psis/pi)
! virtual+soft corretion, bo vac. pol. corr.
          delZa = delZ0 + alf1*dgZts*gZRi
!  with vac. pol. corr.
          delZv =( delZa +dZZss*(vpfac/vpft**2 -1) )*vpfac  ! <---
          delZ1a =delZa
          delZ1v =delZv    ! <---
          ENDIF
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 4
          IF (KeyZex(4) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!... for comparisons with BABAMC
          sR0  = (CMSene    -amZ)**2 +gZex**2/4
          sRkm = (CMSEne-xkm-amZ)**2 +gZex**2/4
          sfis = datan( (-2*(CMSEne-amZ)+2*xkm)/gZex )
     $          -datan(  -2*(CMSEne-amZ)       /gZex )
          spsis= dacos( (amZ2-s)/sqrt((s-amZ2)**2+(amZ*gZex)**2) )
          sZRi = -2*( 1+2*(udz-vdz-wdz) )*( dlog(xkm/Eb)
     $                                     -gZex/2/(CMSEne-amZ)*sfis )
     $           -(1-2*vdz)*dlog((xkm/Eb)**2*sR0/sRkm)
     $           -2*(udz-wdz)*dlog(xkm**2/sRkm)
     $           -2*udz**2 +2*vdz**2 +4*wdz*(udz-vdz) +3*(vdz+wdz)
     $           +dilogy(1-hit) -dilogy(hit) +pi**2/6 -4
     $           -gZex*amZ/(s-amz2)*pi*(2*(wdz-vdz)+1.5
     $                                +4*(udz-wdz)*spsis/pi)
! virtual+soft corretion
          selZa = selZ0 + alf1*sgZtse*sZRi
! vacuum polarization correction
          sevac =-sgZtse *RePi(-TRAN)
! Z self energy corr.; taken from W. Beenakker & B. Pietrzyk, CERN-TH.6649/92
          Zy = (s-amZ2)/(amz*gZba)
          Zg = gZba/amZ
          Zsefa = ( RePi(amZ2)*(1-Zy**2)-2*Zy*Zg )/(1+Zy**2)
          seZse = sgZtsb*Zsefa + 2*sZZssb*Zsefa + selZ0b-selZ0
          selZv = selZa + sevac + seZse
          selZ1a =selZa
          selZ1v =selZv
!--------------------------------------------------------------------
!--------------------------------------------------------------------
          ENDIF
!####################################################################
        ENDIF
!===================================================================
!===================================================================
!              [10] One upper line photon
!===================================================================
!===================================================================
      ELSEIF(nphot1 .EQ. 1.and.nphot2 .EQ. 0) THEN
! numerically safe variables
        a = al1(1)
        b = be1(1)
        y = a +b*deltp
        z = b +a*deltp
! one photon bremss. distribution
        p1k = xpr*z
        p2k = xpr*y
        q1k=Q1(4)*PHOT1(1,4)
        q2k=Q2(4)*PHOT1(1,4)
        DO ii=1,3
          q1k=Q1K-Q1(ii)*PHOT1(1,ii)
          q2k=Q2K-Q2(ii)*PHOT1(1,ii)
        ENDDO
! soft factor
        sfcc = 2*xpp/(p1k*p2k)
        sfc  = sfcc *a*b/(y*z)
        epsp2k=P2(1)*PHOT1(1,2)-P2(2)*PHOT1(1,1)
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 3
        IF (KeyZex(3) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
        Zpro = (s -amZ2)**2 +(amZ*GamZ )**2
        Zpro1= (s1-amZ2)**2 +(amZ*GamZ1)**2
        faRe = Zcc*(s -amZ2)*(u**2+u1**2)
        faRe1= Zcc*(s1-amZ2)*(u**2+u1**2)
        faIm = 2*gv1*ga1*amZ*GamZ *(u**2-u1**2)*s
        faIm1= 2*gv1*ga1*amZ*GamZ1*(u**2-u1**2)*s
        fau  = 4*u**2 /(u**2+u1**2)
        fau1 = 4*u1**2/(u**2+u1**2)
! no s-shift
        au0=( faRe *(sfcc +s1/(p2k*q2k) +u/(p1k*q2k) +4/p2k
     $       -fau *(amel/p2k)**2)
     $       +faIm *epsp2k/(p1k*p2k*q2k) )/Zpro /t1/4
! with s-shift
        au1=( faRe1*(sfcc +s/(p1k*q1k) +u1/(p2k*q1k) -4/p1k
     $       -fau1*(amel/p1k)**2)
     $       -faIm1*epsp2k/(p1k*p2k*q1k) )/Zpro1/t1/4
        au1a = au0 +au1
! with vacuum polarization
        au1v=au0*vpfac +au1*vpfac1  ! <---
        delZa = au1a/sfc
        delZv = au1v/sfc
        delZ1a= delZa
        delZ1v= delZv               ! <---
        ENDIF
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 4
        IF (KeyZex(4) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
C... for comparisons with BABAMC
        spro = (s -amZ2)**2 +(amZ*GZex)**2
        spro1= (s1-amZ2)**2 +(amZ*GZex)**2
        saRe = Zcc1*(s -amZ2)*(u**2+u1**2)
        saRe1= Zcc1*(s1-amZ2)*(u**2+u1**2)
        saIm = 2*gv1*ga1*amZ*GZex *(u**2-u1**2)*s
        sau  = 4*u**2 /(u**2+u1**2)
        sau1 = 4*u1**2/(u**2+u1**2)
! no s-shift
        su0=( saRe *(sfcc +s1/(p2k*q2k) +u/(p1k*q2k) +4/p2k
     $       -sau *(amel/p2k)**2)
     $       +saIm*epsp2k/(p1k*p2k*q2k) )/spro /t1/4
! with s-shift
        su1=( saRe1*(sfcc +s/(p1k*q1k) +u1/(p2k*q1k) -4/p1k
     $       -sau1*(amel/p1k)**2)
     $       -saIm *epsp2k/(p1k*p2k*q1k) )/spro1/t1/4
        su1a = su0 +su1
        su1v = su1a
        selZa = su1a/sfc
        selZv = su1v/sfc
        selZ1a= selZa
        selZ1v= selZv
        ENDIF
!--------------------------------------------------------------------
!--------------------------------------------------------------------
      ELSEIF(nphot1 .EQ. 0.and.nphot2 .EQ. 1) THEN
!===================================================================
!===================================================================
!                [01] One lower line photon
!===================================================================
!===================================================================
! numerically safe variables
        a = al2(1)
        b = be2(1)
        y = a+b*deltq
        z = b+a*deltq
! one photon bremss. distribution
        q1k = xqr*z
        q2k = xqr*y
        p1k=P1(4)*PHOT2(1,4)
        p2k=P2(4)*PHOT2(1,4)
        DO ii=1,3
          p1k=p1k-P1(ii)*PHOT2(1,ii)
          p2k=p2k-P2(ii)*PHOT2(1,ii)
        ENDDO
! soft factor
        sfcc = 2*xqq/(q1k*q2k)
        sfc  = sfcc *a*b/(y*z)
        epsp2k=P2(1)*PHOT2(1,2)-P2(2)*PHOT2(1,1)
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 3
        IF (KeyZex(3) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
        Zpro = (s -amZ2)**2 +(amZ*GamZ )**2
        Zpro1= (s1-amZ2)**2 +(amZ*GamZ1)**2
        faRe = Zcc*(s -amZ2)*(u**2+u1**2)
        faRe1= Zcc*(s1-amZ2)*(u**2+u1**2)
        faIm = 2*gv1*ga1*amZ*GamZ *(u**2-u1**2)*s
        faIm1= 2*gv1*ga1*amZ*GamZ1*(u**2-u1**2)*s
        fau  = 4*u**2 /(u**2+u1**2)
        fau1 = 4*u1**2/(u**2+u1**2)
! no s-shift
        ad0=( faRe *(sfcc +s1/(p2k*q2k) +u1/(p2k*q1k) +4/q2k
     $       -fau1*(amel/q2k)**2)
     $       +faIm *epsp2k/(p2k*q1k*q2k) )/Zpro /t/4
! with s-shift
        ad1=( faRe1*(sfcc +s/(p1k*q1k) +u /(p1k*q2k) -4/q1k
     $       -fau *(amel/q1k)**2)
     $       -faIm1*epsp2k/(p1k*q1k*q2k) )/Zpro1/t/4
        ad1a = ad0 +ad1
! with vacuum polarization
        ad1v=ad0*vpfac +ad1*vpfac1   ! <---
        delZa = ad1a/sfc
        delZv = ad1v/sfc
        delZ1a= delZa
        delZ1v= delZv                ! <---
        ENDIF
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!>>> If chosen version no 4
        IF (KeyZex(4) .EQ. 1) THEN
!--------------------------------------------------------------------
!--------------------------------------------------------------------
C... for comparisons with BABAMC
        spro = (s -amZ2)**2 +(amZ*GZex )**2
        spro1= (s1-amZ2)**2 +(amZ*GZex)**2
        saRe = Zcc1*(s -amZ2)*(u**2+u1**2)
        saRe1= Zcc1*(s1-amZ2)*(u**2+u1**2)
        saIm = 2*gv1*ga1*amZ*GZex*(u**2-u1**2)*s
        sau  = 4*u**2 /(u**2+u1**2)
        sau1 = 4*u1**2/(u**2+u1**2)
! no s-shift
        sd0=( saRe *(sfcc +s1/(p2k*q2k) +u1/(p2k*q1k) +4/q2k
     $       -sau1*(amel/q2k)**2)
     $       +saIm *epsp2k/(p2k*q1k*q2k) )/spro /t/4
! with s-shift
        sd1=( saRe1*(sfcc +s/(p1k*q1k) +u /(p1k*q2k) -4/q1k
     $       -sau *(amel/q1k)**2)
     $       -saIm *epsp2k/(p1k*q1k*q2k) )/spro1/t/4
        sd1a = sd0 +sd1
        sd1v = sd1a
        selZa = sd1a/sfc
        selZv = sd1v/sfc
        selZ1a= selZa
        selZ1v= selZv
        ENDIF
!--------------------------------------------------------------------
!--------------------------------------------------------------------
      ENDIF
!===================================================================
!===================================================================
!
!        ***************************************************
!        ****             NO EXPONENTIATON              ****
!        ****         Definitions of MC weights         ****
!        ***************************************************
!          ---------------------------------
!          /////  UPPER + LOWER line   /////
!          ---------------------------------
! YFS form-factor (to be divided off)
      Emin = EPSCM*CMSENE/2
      epsp = SQRT( Emin**2/P1(4)/P2(4) )
      epsq = SQRT( Emin**2/Q1(4)/Q2(4) )
      fYFSui = EXP( -gamp*log(1/epsp)  +gamp/4 -alf1/2 )
      fYFSli = EXP( -gamq*log(1/epsq)  +gamq/4 -alf1/2 )
      fYFSi  = fYFSui*fYFSli
! O(alf0)
      wtset(80) = delZB  /fYFSi/crude
! O(alf1) - no vacuum pol.
      wtset(81) = delZ1a /fYFSi/crude
! O(alf1) - with vac. pol.
      wtset(82) = delZ1v /fYFSi/crude    ! <---
*-----------------------------------------------------
C... weights below are mainly for comparisons with BABAMC
! O(alf0)
      wtset(90) = selZ0  /fYFSi/crude
! O(alf1) - no vacuum pol.
      wtset(91) = selZ1a /fYFSi/crude
! O(alf1) - with vac. pol.
      wtset(92) = selZ1v /fYFSi/crude
! Vacuum polarization correction
      wtset(97) = sevac  /fYFSi/crude
! Z self energy correction
      wtset(98) = seZse  /fYFSi/crude
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      ENDIF
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      END


! ==================================================================
! ===================== Matrix Element =============================
! ==================================================================
      SUBROUTINE MODEL(MODE,WTM)
! ***********************************************
! Interface to various models for matrix element
! ***********************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / INOUT  / NINP,NOUT
      SAVE   / INOUT  /, / BHPAR3 /

      KeyMod = MOD(KeyRad,100)/10
      IF (KeyMod .EQ. 1) THEN
! Version as in CPC 2.01 (1991,92)
! It is kept for backward compatibility
        CALL MODEL1(MODE)
        WTM = WTSET(2)
      ELSEIF(KeyMod .EQ. 2) THEN
! New version (1993, 1994, 1995)
        CALL MODEL2(MODE)
        WTM = WTSET(1)
      ELSE
        WRITE(NOUT,*)   ' +++++ MODEL: wrong keyMod=',KeyMod
        STOP
      ENDIF
      END


      SUBROUTINE model2(mode)
!     ***********************
!=================================================================!
!                                                                 !
!    New version  of O(alf2) matrix element started May 92        !
!    Now, (Jan. 94) with two examples of O(alf2) matrix element   !
!    Version Jan 95 features also matrix elements without expon.  !
!                                                                 !
!=================================================================!
!     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / INOUT  / NINP,NOUT
      SAVE   / BHPAR2 /, / BHPAR3 /, / BHPARZ /, / TRANSR /
      SAVE   / WGTALL /, / INOUT  /

      KeyZet = MOD(KeyRad,10000)/1000
      s= cmsene**2

!------------------------------------------------------------------
!     Older type (A) with and without exponentiation
!------------------------------------------------------------------
      CALL modl2a(mode)

!------------------------------------------------------------------
!     New  type  (B) with and without exponentiation
!------------------------------------------------------------------
      CALL modl2b(mode)
!
!------------------------------------------------------------------
!     Calculation od Z contribution as in TH.95-74
!------------------------------------------------------------------
      CALL m2agzi(MODE)

!****************************************************************!
!****************************************************************!
!  Model weight, normaly the best... <=====
!****************************************************************!
!  The simple multiplicative ansatz is used to implement         !
!  Vacuum polarization in the LL approximation                   !
!  and s-chanel gamma in the Born approximation                  !
!*****************************************************************
      KeyPia = MOD(KeyRad,10)
      CALL vacpol(KeyPia,-tran,SINW2,RePiE,dRePiE)
!----
      vpfac  = 1/(1 + RePiE)**2
      dels   =  -tran/s +1.5d0*(-tran/s)**2
      sgfac  = 1 + dels
!----
      IF(KeyPia .NE. 0) THEN
!     VacPol and s-chan ON
         IF(KeyZet .EQ. 0) THEN
            WtSet(1) = WtSet(142)*vpfac*sgfac           ! Z off
         ELSEIF(KeyZet .EQ. 1) THEN
            WtSet(1) = WtSet(142)*vpfac*sgfac+WtSet(12) ! Z on
         ELSE
            WRITE(NOUT,*) ' +++++ MODEL: wrong keyZet=',KeyZet
            STOP
         ENDIF
      ELSE
!     VacPol and s-chan OFF
         IF(KeyZet .EQ. 0) THEN
            WtSet(1) = WtSet(142)           ! Z off
         ELSEIF(KeyZet .EQ. 1) THEN
            WtSet(1) = WtSet(142)+WtSet(11) ! Z on
         ELSE
            WRITE(NOUT,*) ' +++++ MODEL: wrong keyZet=',KeyZet
            STOP
         ENDIF
      ENDIF
! This is for special tests with parallel weights
      KeyPia2 = 3
      CALL vacpol(KeyPia2,-tran,SINW2,RePiE2,dRePiE)
      WtSet(2) = 1/(1 + RePiE2)**2
      WtSet(3) = sgfac
!*****************************************************************
      END


      SUBROUTINE MODEL1(MODE)
!     ***********************
!--------------------------------------------------!
!     Version as in CPC 2.01 (1991,92)             !
!     It is kept for backward compatibility tests  !
!     Set KEYMOD=1 if you want to use it           !
!--------------------------------------------------!
! written:     21 jan. 91 (S.Jadach)
! last update:  5 MAY. 91 (S.J.)
! Almost identical to 2.01 version but in addition to
! original one there are two additional delta_Z
! New model weight without the up-down interference
! WT0,1,2 are zero,first,second order exponentiation weights
! The normalisation of distributions is without (1/s)**2 factor
! and without phase space jacobians (like tp*tq/t**2)
! i.e. just matrix element squared
! in beta's S-tilde*d3k/k0 divided out as usual.
!     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER( ALFA= 1D0/ALFINV, ALF1 = 1D0/PI/ALFINV)
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      SAVE   / BHPAR2 /, / BHPAR3 /, / TRANSR /
      SAVE   / MOMS1  /, / MOMS2  /, / MOMZ1  /, / MOMZ2  /
      SAVE   / WGTALL /, / BHPARZ /
      DIMENSION RP(4),RQ(4)
      SAVE ICONT
      DATA ICONT/0/
      SDOT(X1,Y1,Z1,E1,X2,Y2,Z2,E2)
     $            = (E1+E2)**2-(X1+X2)**2-(Y1+Y2)**2-(Z1+Z2)**2
      TDOT(X1,Y1,Z1,E1,X2,Y2,Z2,E2)
     $            = (E1-E2)**2-(X1-X2)**2-(Y1-Y2)**2-(Z1-Z2)**2
! ........................................
      DO 10 K=1,4
      RP(K)= P2(K)+PHSU1(K)
   10 RQ(K)= Q2(K)+PHSU2(K)
      TR = TDOT(P1(1),P1(2),P1(3),P1(4) ,RP(1),RP(2),RP(3),RP(4))
      T  = TDOT(P1(1),P1(2),P1(3),P1(4) ,P2(1),P2(2),P2(3),P2(4))
      T1 = TDOT(Q1(1),Q1(2),Q1(3),Q1(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      S  = SDOT(P1(1),P1(2),P1(3),P1(4) ,Q1(1),Q1(2),Q1(3),Q1(4))
      S1 = SDOT(P2(1),P2(2),P2(3),P2(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      U  = TDOT(P1(1),P1(2),P1(3),P1(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      U1 = TDOT(P2(1),P2(2),P2(3),P2(4) ,Q1(1),Q1(2),Q1(3),Q1(4))
! ........................................
! ..  Crude MC distribution (S-tilde factors omitted)
      CRUDE  = S**2/TR**2
! ........................................
! ..  Contribution from beta0
      BILG   = DLOG(ABS(TR)/AMEL**2)
      BET    = 2D0*ALF1 *( BILG-1D0 )
! .. regular photonic corr. to beta0
      DEL0   = BET
! .. vacuum polarization factor
      VPFAC  = 1D0/(1D0+REPI(TR))**2
      VPFACS = 1D0/(1D0+REPI( S))**2
! .. Z correction
! .. old version, no s-shift
      DELZ =
     $  (GV**2+GA**2)
     $ *(1+TR/S)**3 *2*S**2/(S**2+(S+TR)**2)
     $ *TR*(S-AMAZ**2)/((S-AMAZ**2)**2+(S/AMAZ*GAMMZ)**2)
! .. Z-gamma intrf. has one power of photon and one of Z coup_cons.
     $ *VPFACS/VPFAC
! .. full s-shift (alex read)
      DELZ1 =
     $  (GV**2+GA**2)
     $ *(1+TR/S)**3 *2*S**2/(S**2+(S+TR)**2)
     $ *TR*(S1-AMAZ**2)/((S1-AMAZ**2)**2+(S1/AMAZ*GAMMZ)**2)
     $ *VPFACS/VPFAC
!  It was painfully checked that
!  Chips=0 for initial state and chips=1 for final state!! (s.j.)
!  There are a bit over/under-flows (chips>0, chips<0) at 2% level only.
      CHIPS= 0.5D0
      IF(NPHOT1+NPHOT2.NE.0)
     $  CHIPS = (S**2 *TR**2 - T*T1*S*S1)/(S**2-S1**2)/TR**2
! the cutting below is not really necessary nor relevant numerically
!    CHIPS = MAX(0D0,MIN(1D0,CHIPS))
! half-shift: in the initial state only
      DELZIN = DELZ*CHIPS +DELZ1*(1D0-CHIPS)
! .. s-channel gamma correction
      DELS   =  TR/S +1.5D0*(TR/S)**2
! .......................
      DIS0   = (S**2+U**2+S1**2+U1**2)/4D0 /(T*T1)
      BT00   =  DIS0
      SUM00  =  BT00    /CRUDE
! ..  Leading-Log version ........
      ZETA   = DABS(TRAN)/S
      DISLL0 = 0.5D0*(1 + (1-ZETA)**2)*S**2 /(T*T1)
      BTLL00 =  DISLL0
      SULL00 =  BTLL00    /CRUDE
! ........................................
! ..  Contributions from beta1 upper line
      SULL11  =  0D0
      XPP  = P2(4)*P1(4)-P2(3)*P1(3)-P2(2)*P1(2)-P2(1)*P1(1)
      XPR  = P1(4)*RP(4)-P1(3)*RP(3)-P1(2)*RP(2)-P1(1)*RP(1)
      DELT = AMEL**2/(2*XPP)
      SUM11U=0D0
      DO 150 I=1,NPHOT1
! ..  Numerically safe variables
      A   = AL1(I)
      B   = BE1(I)
      Y   = A +B*DELT
      Z   = B +A*DELT
      P1K = XPR*Z
      P2K = XPR*Y
! ..  soft factor
      WMS0 = A*B/(Y*Z)
      SFC  = 2D0*XPP/(P1K*P2K)*WMS0
! ..  one photon bremss. distribution
      WM = A*B/(Y*Z) +DELT*(Y**2+Z**2)/((1-Y)**2+(1-Z)**2)*(Y/Z+Z/Y)
      DIS1 = (S**2 +U**2 +S1**2+U1**2)
     $       /4D0/(-T1*P1K*P2K)   *WM
! ..  beta1
      BT11U   = DIS1/SFC-BT00
      SUM11U  = SUM11U  + BT11U /CRUDE
! ..  Leading-Log version ........
! Note that in DIS1/SFC we get 1/(T*T1) which after multiplication
! by jacobians (T/TRAN)(T1/TRAN) results in 1/TRAN**2, LL is OK!
      ZETA = DABS(TRAN)/(S*(1-A))
      XX  = MAX(A,B)
      DISLL1 = S**2 *(1 + (1-ZETA)**2) * (1 +(1-XX)**2 )
     $       /4D0/(-T1*P1K*P2K)  *WMS0
      BTLL11   = DISLL1/SFC-BTLL00
      SULL11  = SULL11  + BTLL11 /CRUDE
  150 CONTINUE
! ........................................
! ..  Contributions from beta1 lower line
      XQQ  = Q2(4)*Q1(4)-Q2(3)*Q1(3)-Q2(2)*Q1(2)-Q2(1)*Q1(1)
      XQR  = Q1(4)*RQ(4)-Q1(3)*RQ(3)-Q1(2)*RQ(2)-Q1(1)*RQ(1)
      DELT = AMEL**2/(2*XQQ)
      SUM11L=0D0
      DO 250 I=1,NPHOT2
! ..  numerically safe variables
      A   = AL2(I)
      B   = BE2(I)
      Y   = A+B*DELT
      Z   = B+A*DELT
      Q1K = XQR*Z
      Q2K = XQR*Y
! ..  soft factor
      WMS0 = A*B/(Y*Z)
      SFC  = 2D0*XQQ/(Q1K*Q2K)*WMS0
! ..  one photon bremss. distribution
      WM = A*B/(Y*Z) +DELT*(Y**2+Z**2)/((1-Y)**2+(1-Z)**2)*(Y/Z+Z/Y)
      DIS1 = (S**2 +U**2 +S1**2+U1**2)
     $       /4D0/(-T *Q1K*Q2K)   *WM
! ..  beta1 contrib.
      BT11L   = DIS1/SFC-BT00
      SUM11L  = SUM11L  + BT11L /CRUDE
! ..  Leading-Log version ........
      ZETA = DABS(TRAN)/(S*(1-A))
      XX  = MAX(A,B)
      DISLL1 = S**2 *(1 + (1-ZETA)**2) * (1 +(1-XX)**2 )
     $       /4D0/(-T*Q1K*Q2K)  *WMS0
      BTLL11   = DISLL1/SFC-BTLL00
      SULL11   = SULL11  + BTLL11 /CRUDE
  250 CONTINUE
!-----------------------
! the best and most complete
!-----------------------
! zero order
      WTSET( 11) = SUM00*(1+DELZ+DELS)
! first order
      SUM01      = SUM00*(1D0 + DEL0+DELZ+DELS)
      WTSET( 12) = (SUM01 + SUM11U + SUM11L)*VPFAC
! second order not implemented
      WTSET( 13) = 0D0
!-----------------------
! model weight PURE BREMSS. only, no vac.pol, no Z
! zero order
      WTSET( 51) = SUM00
! first order
      SUM01 = SUM00*(1D0 + DEL0)
      WTSET( 52) = (SUM01 + SUM11U + SUM11L)
! second order not implemented
      WTSET( 53) = 0D0
!-----------------------
! Leading-Log version, pure bremss. only
      WTSET( 61) = SULL00
      SULL01 = SULL00*(1D0 + DEL0)
      WTSET( 62) = (SULL01 + SULL11)
      WTSET( 63) = (0D0            )
!------------------------------------------
! Miscelanous, for tests in TH-6118
! Vacuum polarization effect ONN/OFF
      SUM01      = SUM00*(1D0 + DEL0 )
      WTSET( 20) = (SUM01 + SUM11U + SUM11L)* VPFAC
      WTSET( 21) = (SUM01 + SUM11U + SUM11L)* (VPFAC-1)
! Z-exchange  ON/OFF
      SUM01      = SUM00*(1D0 +DEL0 +DELZ  )
      WTSET( 22) = (SUM01 + SUM11U + SUM11L)*VPFAC
      WTSET( 23) = WTSET(22)-WTSET(20)
! s-schannel exchange
      SUM01      = SUM00*(1D0 +DEL0 +DELZ +DELS )
      WTSET( 24) = (SUM01 + SUM11U + SUM11L)*VPFAC
      WTSET( 25) = WTSET(24)-WTSET(22)
! beta_0,1 contribs.
      WTSET( 26) = (SUM01                  )*VPFAC
      WTSET( 27) = (      + SUM11U + SUM11L)*VPFAC
! new exercises on Z-exchange
      WTSET( 30) = SUM00* DELZ1  *VPFAC
      WTSET( 31) = SUM00* DELZIN *VPFAC
!---------------------------------------------------
! Model weight, normaly the best...
!-----------------------
      WTSET(1) = WTSET(11)
      WTSET(2) = WTSET(12)
      WTSET(3) = WTSET(13)
      KEYPIA = MOD(KEYRAD,10)
! pure bremsstr. as an option:
      IF(KEYPIA.EQ.0) THEN
      WTSET(1) = WTSET(51)
      WTSET(2) = WTSET(52)
      WTSET(3) = WTSET(53)
      ENDIF
      END
      SUBROUTINE MODL2A(MODE)
!     ***********************
!======================================================================!
!                                                                      !
!    OLD type Matrix element type (A)                                  !
!    similar as in BHLUMI version. 2.01  (Identical in first order)    !
!    but now also in the second order     (January. 94).               !
!    Exponentiated and not exponentiated  (January. 95)                !
!                                                                      !
!======================================================================!
!     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER( ALFA= 1D0/ALFINV, ALF1 = 1D0/PI/ALFINV)
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / PSIPHI / TH1,EXT1,EXB1,PSI1,EXW1,
     $                  TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      SAVE   / BHPAR2 /, / BHPAR3 /, / TRANSR /
      SAVE   / MOMS1  /, / MOMS2  /, / MOMZ1  /, / MOMZ2  /
      SAVE   / WGTALL /, / BHPARZ /, / PSIPHI /
      DIMENSION ph1(4),ph2(4)
      DIMENSION sfu(100),sfmu(100),bu10(100),bu11(100),bu11g(100)
      DIMENSION sfl(100),sfml(100),bl10(100),bl11(100),bl11g(100)
! ------------------ Inline functions ------------------
! Elements of single bremss. distribution
      CHI(x)= (1+(1-x)**2)/2
      XDELS(a,b,delt) =
     $ delt*(a**2+b**2)*(a**2+b**2)/((1-a)**2+(1-b)**2)/(a*b)
!==================================================================!
      DATA icont /0/
      icont=icont+1

      s = cmsene**2
      zeta = abs(tran)/s
!*****************************************************
!  Crude MC distribution (S-tilde factors omitted)
      CRUDE  =  s**2/tran**2
      facpq  =  s**2/(tranp*tranq)
      delta  =  amel**2/tran
      deltp  =  amel**2/tranp
      deltq  =  amel**2/tranq
      gam    =  2*alf1 *( dlog(1/delta)-1 )
      gamp   =  2*alf1 *( dlog(1/deltp)-1 )
      gamq   =  2*alf1 *( dlog(1/deltq)-1 )
      dis0   =  facpq *dix1(p1,p2,q1,q2)
!==================================================================!
      bt10u =0
      bt10l =0
      bt11u =0
      bt11l =0
      bt20u =0
      bt20l =0
      bt2ul =0
! ------------------
! beta0
! ------------------
      bt00   = dis0
      bt01   = bt00*(1 + gam )
      bt02   = bt00*(1 + gam +0.5*gam**2)
! ------------------------------------
! Contributions from beta1 UPPER line
! ------------------------------------
      DO i=1,nphot1
! numerically safe variables
         a   = al1(i)
         b   = be1(i)
         v   = a+b-a*b
         y   = a +b*deltp
         z   = b +a*deltp
! soft factor
         sfc  = a*b/(y*z)**2
         sfu(i) = sfc
! one photon bremss. distribution
         wm = a*b/(y*z) +deltp*(y**2+z**2)/((1-y)**2+(1-z)**2)*(y/z+z/y)
         sfmu(i) = wm/(y*z)
         dis10   = sfmu(i)*dis0
         dis11g  = dis10 *(1+gam + gam/2*log((1-b)/(1-a)))
         dis11   = dis10 *(1+gam + gam/4*log((1-b)/(1-a)))
! beta1 O(alf1),O(alf2)
         bu10(i) =  dis10   -bt00*sfc
         bu11(i) =  dis11   -bt01*sfc
         bu11g(i)=  dis11g  -bt01*sfc ! with LL bug
         bt10u   =  bt10u   +bu10(i)/sfc
         bt11u   =  bt11u   +bu11(i)/sfc
      ENDDO
! -------------------------------------
!  Contributions from beta1 LOWER line
! -------------------------------------
      DO i=1,nphot2
         a   = al2(i)
         b   = be2(i)
         v   = a+b-a*b
         y   = a+b*deltq
         z   = b+a*deltq
! soft factor
         sfc  = a*b/(y*z)**2
         sfl(i) = sfc
! one photon bremss. distribution
         wm = a*b/(y*z) +deltq*(y**2+z**2)/((1-y)**2+(1-z)**2)*(y/z+z/y)
         sfml(i) = wm/(y*z)
         dis10   = sfml(i)*dis0
         dis11g  = dis10 *(1+gam + gam/2*log((1-b)/(1-a)))
         dis11   = dis10 *(1+gam + gam/4*log((1-b)/(1-a)))
! beta1 O(alf1),O(alf2)
         bl10(i) =  dis10   -bt00*sfc
         bl11(i) =  dis11   -bt01*sfc
         bl11g(i)=  dis11g  -bt01*sfc ! with LL bug
         bt10l   =  bt10l   +bl10(i)/sfc
         bt11l   =  bt11l   +bl11(i)/sfc
      ENDDO

!------------------
! beta2 upper line
!------------------
      DO i=1,nphot1
      DO j=i+1,nphot1
! Define photon four-vectors for further local use
      DO  k=1,4
         ph1(k) = phot1(i,k)
         ph2(k) = phot1(j,k)
      ENDDO
! Basic variables first photon
      a1  = al1(i)
      b1  = be1(i)
      y1  = a1 +b1*deltp
      z1  = b1 +a1*deltp
      v1  = a1+b1-a1*b1
! Basic variables second photon
      a2  = al1(j)
      b2  = be1(j)
      y2  = a2 +b2*deltp
      z2  = b2 +a2*deltp
      v2  = a2+b2-a2*b2
! Soft factors, starred variables
      sf1  =  sfu(i)
      sf2  =  sfu(j)
      a1st = a1/(1-v2)
      b1st = b1/(1-v2)
      v1st = v1/(1-v2)
      a2st = a2/(1-v1)
      b2st = b2/(1-v1)
      v2st = v2/(1-v1)
      a1st = min(a1st,1-zeta)
      b1st = min(b1st,1-zeta)
      v1st = min(v1st,1-zeta)
      a2st = min(a2st,1-zeta)
      b2st = min(b2st,1-zeta)
      v2st = min(v2st,1-zeta)
! H-functions exact and Leading Log
      gs1    = sf1*chi(v1)
      gs2    = sf2*chi(v2)
      gs1st  = sf1*chi(v1st)
      gs2st  = sf2*chi(v2st)
! The actual two photon distributions
      IF ( (a1-b1)*(a2-b2).le.0d0 ) THEN
!       OPPOSITE directions two photons
        IF ((a1-b1).gt.0d0) THEN
!          FIRST photon in INITIAL state, second in the final
           hs1f   = DIX2(2,ph2,p1,p2,q1,q2) *sfmu(i)
           hs2i   = DIX2(1,ph1,p1,p2,q1,q2) *sfmu(j)
           IF(v1.gt.v2) THEN
               d2uu  = facpq  *hs1f  *gs2
           ELSE
               d2uu  = facpq  *gs1st *hs2i
           ENDIF
        ELSE
!          SECOND photon in INITIAL state, first in the final
           hs1i   = DIX2(1,ph2,p1,p2,q1,q2) *sfmu(i)
           hs2f   = DIX2(2,ph1,p1,p2,q1,q2) *sfmu(j)
           IF(v1.gt.v2) THEN
               d2uu  = facpq  *hs1i *gs2st
           ELSE
               d2uu  = facpq  *gs1  *hs2f
           ENDIF
        ENDIF
      ELSE
!       SAME directions two photons
!         Both in INITIAL  state or Both in FINAL  state
          IF(v1.gt.v2) THEN
            hs1i  = DIX2(1,ph2,p1,p2,q1,q2) *sfmu(i)
            hs1f  = DIX2(2,ph2,p1,p2,q1,q2) *sfmu(i)
            d2uu  = facpq/2 *( hs1f*gs2st + hs1i*gs2)
          ELSE
            hs2i  = DIX2(1,ph1,p1,p2,q1,q2) *sfmu(j)
            hs2f  = DIX2(2,ph1,p1,p2,q1,q2) *sfmu(j)
            d2uu  = facpq/2 *( hs2f*gs1st + hs2i*gs1)
          ENDIF
      ENDIF
      bu20  = d2uu-bu10(i)*sfu(j)-bu10(j)*sfu(i)-bt00*sfu(j)*sfu(i)
      bt20u = bt20u + bu20/sfu(i)/sfu(j)
      ENDDO
      ENDDO
! ------------------
! beta2 lower line
! ------------------
      DO i=1,nphot2
      DO j=i+1,nphot2
! Define photon four-vectors for further local use
      DO  k=1,4
         ph1(k) = phot2(i,k)
         ph2(k) = phot2(j,k)
      ENDDO
! Basic variables first photon
      a1  = al2(i)
      b1  = be2(i)
      y1  = a1 +b1*deltq
      z1  = b1 +a1*deltq
      v1  = a1+b1-a1*b1
! Basic variables second photon
      a2  = al2(j)
      b2  = be2(j)
      y2  = a2 +b2*deltq
      z2  = b2 +a2*deltq
      v2  = a2+b2-a2*b2
! soft factors, starred variables
      sf1  =  sfl(i)
      sf2  =  sfl(j)
      a1st = a1/(1-v2)
      b1st = b1/(1-v2)
      v1st = v1/(1-v2)
      a2st = a2/(1-v1)
      b2st = b2/(1-v1)
      v2st = v2/(1-v1)
      a1st = min(a1st,1-zeta)
      b1st = min(b1st,1-zeta)
      v1st = min(v1st,1-zeta)
      a2st = min(a2st,1-zeta)
      b2st = min(b2st,1-zeta)
      v2st = min(v2st,1-zeta)
! H-functions exact and Leading Log
      gs1    = sf1*chi(v1)
      gs2    = sf2*chi(v2)
      gs1st  = sf1*chi(v1st)
      gs2st  = sf2*chi(v2st)
! The actual two photon distributions
      IF ( (a1-b1)*(a2-b2).le.0d0 ) THEN
!       OPPOSITE directions two photons
        IF ((a1-b1).gt.0d0) THEN
!          FIRST photon in INITIAL state, second in the final
           hs1f   = DIX2(4,ph2,p1,p2,q1,q2) *sfml(i)
           hs2i   = DIX2(3,ph1,p1,p2,q1,q2) *sfml(j)
           IF(v1.gt.v2) THEN
               d2ll  = facpq  *hs1f  *gs2
           ELSE
               d2ll  = facpq  *gs1st*hs2i
           ENDIF
        ELSE
!          SECOND photon in INITIAL state, first in the final
           hs1i   = DIX2(3,ph2,p1,p2,q1,q2) *sfml(i)
           hs2f   = DIX2(4,ph1,p1,p2,q1,q2) *sfml(j)
           IF(v1.gt.v2) THEN
               d2ll  = facpq  *hs1i *gs2st
           ELSE
               d2ll  = facpq  *gs1 *hs2f
           ENDIF
        ENDIF
      ELSE
!       SAME directions two photons
!         Both in INITIAL  state or Both in FINAL  state
          IF(v1.gt.v2) THEN
            hs1i  = DIX2(3,ph2,p1,p2,q1,q2) *sfml(i)
            hs1f  = DIX2(4,ph2,p1,p2,q1,q2) *sfml(i)
            d2ll  = facpq/2 *( hs1f*gs2st + hs1i*gs2)
          ELSE
            hs2i  = DIX2(3,ph1,p1,p2,q1,q2) *sfml(j)
            hs2f  = DIX2(4,ph1,p1,p2,q1,q2) *sfml(j)
            d2ll  = facpq/2 *( hs2f*gs1st + hs2i*gs1)
          ENDIF
      ENDIF
      bl20  = d2ll-bl10(i)*sfl(j)-bl10(j)*sfl(i)-bt00*sfl(j)*sfl(i)
      bt20l = bt20l + bl20/sfl(i)/sfl(j)
      ENDDO
      ENDDO
!=================================================
! [11] One upper and one lower line photons
! Note that bt2ul=0 in the LL.
      DO i=1,nphot1
      DO j=1,nphot2
! Basic variables first photon
        a1  = al1(i)
        b1  = be1(i)
        v1  = a1 +b1-a1*b1
! Basic variables second photon
        a2  = al2(j)
        b2  = be2(j)
        v2  = a2+b2-a2*b2
! soft factors
        sf1  =  sfu(i)
        sf2  =  sfl(j)
        IF(v1.gt.v2) THEN
           d2ul= dis0 *sfmu(i) *sfl(j)
         ELSE
           d2ul= dis0 *sfu(i)  *sfml(j)
        ENDIF
        b2ul  = d2ul-bu10(i)*sfl(j)-sfu(i)*bl10(j)-bt00*sfu(i)*sfl(j)
        bt2ul = bt2ul + b2ul/sfu(i)/sfl(j)
      ENDDO
      ENDDO
     

!     **************************************
!     **  MC weights, with exponentiation **
!     **************************************
!          ---------------------------------
!          !    UPPER line ONLY,    (A)    !
!          ---------------------------------
! Case of upper line only has separate entries in WTSET because it is 
! analysed in the separate run with separate plotting programs 
! with different entries in vvrho, differerent histogram names etc.
!
! All beta's:  TOTAL O(alf0),O(alf1),O(alf2)
      WTSET( 30) =    bt00                /crude
      WTSET( 31) =   (bt01+bt10u)         /crude
      WTSET( 32) =   (bt02+bt11u +bt20u)  /crude
! Special bet0-type distr. (without chi0) for tests only 
      WTSET( 34) =   ( s**2/(tranp*tranq) )/crude
! Individual beta's in various orders.
! O(alf1)
      WTSET( 35) =   bt01  /crude                  ! beta0
      WTSET( 36) =   bt10u /crude                  ! beta1
! O(alf2)
      WTSET( 37) =   bt02  /crude                  ! beta0
      WTSET( 38) =   bt11u /crude                  ! beta1
      WTSET( 39) =   bt20u /crude                  ! beta2
!          ---------------------------------
!          !  UPPER + LOWER line,    (A)   !
!          ---------------------------------
! All beta's:  O(alf0),O(alf1),O(alf2)
      WTSET( 40) =    bt00  /crude
      WTSET( 41) =   (bt01+bt10u+bt10l) /crude
      WTSET( 42) =   (bt02+bt11u+bt11l+bt20u+bt20l+bt2ul) /crude
! Individual beta's in various orders.
! O(alf1)
      WTSET( 43) =    bt01   /crude                ! beta0
      WTSET( 44) =   (bt10u+bt10l) /crude          ! beta1
      WTSET( 45) =    bt10u  /crude                ! beta1 components
      WTSET( 46) =    bt10l  /crude
! O(alf2)
      WTSET( 47) =    bt02   /crude                ! beta0
      WTSET( 48) =   (bt11u+bt11l)/crude           ! beta1
      WTSET( 49) =   (bt20u+bt20l+bt2ul)/crude     ! beta2
      WTSET( 50) =    bt11u  /crude                ! beta1 components
      WTSET( 51) =    bt11l  /crude
      WTSET( 52) =    bt2ul  /crude
      WTSET( 53) =    bt20u  /crude                ! beta2 components
      WTSET( 54) =    bt20l  /crude

!==================================================================
!==================================================================
!                  Non-exponentiated version                      !
!==================================================================
!==================================================================
      pdel  = del*bcud(p1,p2,phsu1)
      qdel  = del*bcud(q1,q2,phsu2)
      fyfsu = exp(-gamp*dlog(1/pdel)  +gamp/4 -alf1/2)
      fyfsl = exp(-gamq*dlog(1/qdel)  +gamq/4 -alf1/2)
!
      bb0u    =0
      bb1u    =0
      bb2u    =0
      dd0u    =0
      dd1u    =0
      dd2u    =0
      IF(nphot1.eq.0) THEN
        bb0u   = dis0
        bb1u   = bb0u*(1 -gam*dlog(1/pdel))
        bb2u   = bb0u*(1 -gam*dlog(1/pdel) +1./2*(gam*dlog(1/pdel))**2)
        virt1  = 3./4*gam -1./2*alf1
        bb0u   = dis0
        dd1u   = dis0 *(1 -gam*dlog(1/pdel) +virt1)
      ELSEIF(nphot1.eq.1) THEN
        a   = al1(1)
        b   = be1(1)
        y   = a +b*deltp
        z   = b +a*deltp
        sfc = a*b/(y*z)**2
        bb1u = dis0
        wm  = a*b/(y*z) +deltp*(y**2+z**2)/((1-y)**2+(1-z)**2)*(y/z+z/y)
        sfm = wm/(y*z)
        dist1  = dis0 *sfm
        dd1u    = dist1/sfc
      ENDIF
!
      bb1  =0
      bb2  =0
!        **************************************
!        ****   Definitions of MC weights  ****
!        **************************************
! Case of upper-line-only has separate entries in WTSET because it is 
! analysed in the separate run with separate plotting programs 
! with different entries in vvrho, differerent histogram names etc.
!
! YFS formfactor fyfsu has to be removed in the unexponentiated case!
! Baseline
      WTSET(56) =    bb1u               /fyfsu/crude ! 1-photon
      WTSET(57) =    bb2u               /fyfsu/crude ! 2-photons
      WTSET(58) =    bb1                /fyfsu/crude ! 1-photon
      WTSET(59) =    bb2                /fyfsu/crude ! 2-photons
!          ---------------------------------
!          /////    UPPER line ONLY    /////
!          ---------------------------------
! TOTAL O(alf0),O(alf1),O(alf2)
      WTSET(60) =    dd0u                /fyfsu/crude
      WTSET(61) =    dd1u                /fyfsu/crude
      WTSET(62) =    dd2u                /fyfsu/crude
! Baseline
      WTSET(63) =    0
      WTSET(64) =    0
!************************************************************************

!    ////////////////////////////////////////////
!    /////  UNEXP   UPPER + LOWER line      /////
!    ////////////////////////////////////////////

! Entire 0,1,2-photon distributions
      dis0   =0
      dis1   =0
      dis2   =0
!
      virt1y = 6./4*gam -alf1
      virt2y = 0.5*(6./4*gam)**2 -3/2d0*alf1*gam
      h0     =  dix1(p1,p2,q1,q2)
!=================================================
      IF(nphot1.eq.0.and.nphot2.eq.0) THEN
! [00] No photons
!------
! O(alf0,1,2) Entire distribution 
        dis0= facpq*h0
        dis1= facpq*h0*(1+2*gam*dlog(del))
     $       +facpq*h0*virt1y
        dis2= facpq*h0*(1+2*gam*dlog(del)+0.5*(2*gam*dlog(del))**2)
     $       +facpq*h0*(1+2*gam*dlog(del))*virt1y
     $       +facpq*h0* virt2y
!=================================================
      ELSEIF(nphot1.eq.1.and.nphot2.eq.0) THEN
! [10] One upper line photon 
!  O(alf1,2) Entire distribution
!  Note that 1/2 in the log(1-v) term = {1/2 from two tree diagrams} 
        dis1   = facpq*h0*(1+xdels(al1(1),be1(1),deltp))
        dis2g  = dis1*(1 +(gamp+gamq)*dlog(del) +virt1y
     $        -1/2d0*gam*log(1-al1(1)) -1/2d0*gam*log(1-be1(1)) )
        dis2   = dis1*(1 +(gamp+gamq)*dlog(del) +virt1y
     $        -1/4d0*gam*log(1-al1(1)) -3/4d0*gam*log(1-be1(1))
     $       +1/4d0*(gamp-gam) -(gamp-gam)*log(1-be1(1))  ! emulate 4.02a.exp
     $    )
!=================================================
      ELSEIF(nphot1.eq.0.and.nphot2.eq.1) THEN
! [01] One lower line photon 
!------
! O(alf1,2) Entire distribution
! Note that 1/2 in the log(1-v) term = {1/2 from two tree diagrams} 
        dis1= facpq*h0*(1+xdels(al2(1),be2(1),deltq))
        dis2g= dis1*(1 +(gamp+gamq)*dlog(del) +virt1y
     $        -1/2d0*gam*log(1-al2(1)) -1/2d0*gam*log(1-be2(1)) )
        dis2 = dis1*(1 +(gamp+gamq)*dlog(del) +virt1y
     $        -1/4d0*gam*log(1-al2(1)) -3/4d0*gam*log(1-be2(1))
     $       +1/4d0*(gamq-gam) -(gamq-gam)*log(1-be2(1))   ! emulate 4.02a.exp
     $       )
!=================================================
      ELSEIF(nphot1.eq.2.and.nphot2.eq.0) THEN
! [20] Two upper line photons 
! Basic distribution
        a1  = al1(1)
        b1  = be1(1)
        a2  = al1(2)
        b2  = be1(2)
        v1  = a1+b1-a1*b1
        v2  = a2+b2-a2*b2
! Define photon four-vectors for further local use
        DO k=1,4
          ph1(k) = phot1(1,k)
          ph2(k) = phot1(2,k)
        ENDDO
        a1st = min(a1/(1-v2),1-zeta)
        b1st = min(b1/(1-v2),1-zeta)
        a2st = min(a2/(1-v1),1-zeta)
        b2st = min(b2/(1-v1),1-zeta)
        v1st = min(v1/(1-v2),1-zeta)
        v2st = min(v2/(1-v1),1-zeta)
!       H-functions exact
        hs1i = DIX2(1,ph2,p1,p2,q1,q2)*(1+xdels(a1,b1,deltp))
        hs1f = DIX2(2,ph2,p1,p2,q1,q2)*(1+xdels(a1,b1,deltp))
        hs2i = DIX2(1,ph1,p1,p2,q1,q2)*(1+xdels(a2,b2,deltp))
        hs2f = DIX2(2,ph1,p1,p2,q1,q2)*(1+xdels(a2,b2,deltp))
!       The actual two photon distributions
        IF ( (a1-b1)*(a2-b2).le.0d0 ) THEN
!       OPPOSITE directions two photons
          IF (a1.gt.b1) THEN
!          FIRST photon in INITIAL state, second in the final
             IF(v1.gt.v2) dis2  = facpq* hs1f      *chi(v2)
             IF(v2.ge.v1) dis2  = facpq* chi(v1st) *hs2i
          ELSE
!          SECOND photon in INITIAL state, first in the final
             IF(v1.gt.v2) dis2  = facpq* hs1i    *chi(v2st)
             IF(v2.ge.v1) dis2  = facpq* chi(v1) *hs2f
          ENDIF
        ELSE
!       SAME directions two photons
          IF(v1.gt.v2) dis2 = facpq/2 *(hs1f*chi(v2st) + hs1i*chi(v2))
          IF(v2.ge.v1) dis2 = facpq/2 *(hs2f*chi(v1st) + hs2i*chi(v1))
        ENDIF
!=================================================
      ELSEIF(nphot1.eq.0.and.nphot2.eq.2) THEN
! [02] Two lower line photons 
! Basic distribution
        a1  = al2(1)
        b1  = be2(1)
        a2  = al2(2)
        b2  = be2(2)
        v1  = a1+b1-a1*b1
        v2  = a2+b2-a2*b2
        a1st = min(a1/(1-v2),1-zeta)
        b1st = min(b1/(1-v2),1-zeta)
        a2st = min(a2/(1-v1),1-zeta)
        b2st = min(b2/(1-v1),1-zeta)
        v1st = min(v1/(1-v2),1-zeta)
        v2st = min(v2/(1-v1),1-zeta)
! Define photon four-vectors for further local use
        DO k=1,4
          ph1(k) = phot2(1,k)
          ph2(k) = phot2(2,k)
        ENDDO
!       H-functions exact
        hs1i = DIX2(3,ph2,p1,p2,q1,q2)*(1+xdels(a1,b1,deltq))
        hs1f = DIX2(4,ph2,p1,p2,q1,q2)*(1+xdels(a1,b1,deltq))
        hs2i = DIX2(3,ph1,p1,p2,q1,q2)*(1+xdels(a2,b2,deltq))
        hs2f = DIX2(4,ph1,p1,p2,q1,q2)*(1+xdels(a2,b2,deltq))
!       The actual two photon distributions
        IF ( (a1-b1)*(a2-b2).le.0d0 ) THEN
!       OPPOSITE directions two photons
          IF (a1.gt.b1) THEN
!          FIRST photon in INITIAL state, second in the final
             IF(v1.gt.v2) dis2  = facpq* hs1f      *chi(v2)
             IF(v2.ge.v1) dis2  = facpq* chi(v1st) *hs2i
          ELSE
!          SECOND photon in INITIAL state, first in the final
             IF(v1.gt.v2) dis2  = facpq* hs1i    *chi(v2st)
             IF(v2.ge.v1) dis2  = facpq* chi(v1) *hs2f
          ENDIF
        ELSE
!       SAME directions two photons
          IF(v1.gt.v2) dis2 = facpq/2 *(hs1f*chi(v2st) + hs1i*chi(v2))
          IF(v2.ge.v1) dis2 = facpq/2 *(hs2f*chi(v1st) + hs2i*chi(v1))
        ENDIF
!=================================================
      ELSEIF(nphot1.eq.1.and.nphot2.eq.1) THEN
! [11] One upper and lower line photons 
        a1  = al1(1)
        b1  = be1(1)
        a2  = al2(1)
        b2  = be2(1)
        v1  = a1+b1-a1*b1
        v2  = a2+b2-a2*b2
        IF(v1.gt.v2) THEN
          dis2 = facpq *dix1(p1,p2,q1,q2)*(1+xdels(a1,b1,deltp))
        ELSE
          dis2 = facpq *dix1(p1,p2,q1,q2)*(1+xdels(a2,b2,deltq))
        ENDIF
      ENDIF
!     =====

!        **************************************
!        ****   Definitions of MC weights  ****
!        ****    Non-exponentiated case    ****
!        **************************************
!          ---------------------------------
!          /////  UPPER + LOWER line   /////
!          ---------------------------------
 
      fyfs = fyfsu*fyfsl
! TOTAL O(alf0),O(alf1),O(alf2)
      WTSET(70) =    dis0                /fyfs/crude
      WTSET(71) =    dis1                /fyfs/crude
      WTSET(72) =    dis2                /fyfs/crude
!
      END

      FUNCTION DIX1(p1,p2,q1,q2)
!     ***************************
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION p1(4),p2(4),q1(4),q2(4)
! ------------------ Inline functions ------------------
      pdot(x1,y1,z1,e1,x2,y2,z2,e2) = 2*(e1*e2-x1*x2-y1*y2-z1*z2)
!
      s  = pdot(p1(1),p1(2),p1(3),p1(4) ,q1(1),q1(2),q1(3),q1(4))
      s1 = pdot(p2(1),p2(2),p2(3),p2(4) ,q2(1),q2(2),q2(3),q2(4))
      u  = pdot(p1(1),p1(2),p1(3),p1(4) ,q2(1),q2(2),q2(3),q2(4))
      u1 = pdot(p2(1),p2(2),p2(3),p2(4) ,q1(1),q1(2),q1(3),q1(4))
      dix1   = (s**2+u**2+s1**2+u1**2)/(4*s**2)
      END

      FUNCTION DIX2(noga,ph,p1,p2,q1,q2)
!     **********************************
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ph(4),p1(4),p2(4),q1(4),q2(4)
      DIMENSION w1(4),w2(4),v1(4),v2(4)
! ------------------ Inline functions ------------------
      pdot(x1,y1,z1,e1,x2,y2,z2,e2) = 2*(e1*e2-x1*x2-y1*y2-z1*z2)
!
      IF(noga.eq.1) THEN
        DO 1 k=1,4
 1      w1(k) = p1(k) - ph(k)
        s  = pdot(w1(1),w1(2),w1(3),w1(4) ,q1(1),q1(2),q1(3),q1(4))
        s1 = pdot(p2(1),p2(2),p2(3),p2(4) ,q2(1),q2(2),q2(3),q2(4))
        u  = pdot(w1(1),w1(2),w1(3),w1(4) ,q2(1),q2(2),q2(3),q2(4))
        u1 = pdot(p2(1),p2(2),p2(3),p2(4) ,q1(1),q1(2),q1(3),q1(4))
      ELSEIF(noga.eq.2) THEN
        DO 2 k=1,4
 2      w2(k) = p2(k) + ph(k)
        s  = pdot(p1(1),p1(2),p1(3),p1(4) ,q1(1),q1(2),q1(3),q1(4))
        s1 = pdot(w2(1),w2(2),w2(3),w2(4) ,q2(1),q2(2),q2(3),q2(4))
        u  = pdot(p1(1),p1(2),p1(3),p1(4) ,q2(1),q2(2),q2(3),q2(4))
        u1 = pdot(w2(1),w2(2),w2(3),w2(4) ,q1(1),q1(2),q1(3),q1(4))
      ELSEIF(noga.eq.3) THEN
        DO 3 k=1,4
 3      v1(k) = q1(k) - ph(k)
        s  = pdot(p1(1),p1(2),p1(3),p1(4) ,v1(1),v1(2),v1(3),v1(4))
        s1 = pdot(p2(1),p2(2),p2(3),p2(4) ,q2(1),q2(2),q2(3),q2(4))
        u  = pdot(p1(1),p1(2),p1(3),p1(4) ,q2(1),q2(2),q2(3),q2(4))
        u1 = pdot(p2(1),p2(2),p2(3),p2(4) ,v1(1),v1(2),v1(3),v1(4))
      ELSEIF(noga.eq.4) THEN
        DO 4 k=1,4
 4      v2(k) = q2(k) + ph(k)
        s  = pdot(p1(1),p1(2),p1(3),p1(4) ,q1(1),q1(2),q1(3),q1(4))
        s1 = pdot(p2(1),p2(2),p2(3),p2(4) ,v2(1),v2(2),v2(3),v2(4))
        u  = pdot(p1(1),p1(2),p1(3),p1(4) ,v2(1),v2(2),v2(3),v2(4))
        u1 = pdot(p2(1),p2(2),p2(3),p2(4) ,q1(1),q1(2),q1(3),q1(4))
      ELSE
        write(6,*) 'DIX2: Wrong noga = ',noga
      ENDIF
      dix2   = (s**2+u**2+s1**2+u1**2)/(4*s**2)
      END

      SUBROUTINE MODL2B(MODE)
!     ***********************
!======================================================================!
!                                                                      !
!    NEW version  of the EXPONENTIATED matrix element, type (B)        !
!    Started May 92.                                                   !
!    O(alf2) part of the matrix element implemented Oct. 93            !
!    and fully debugged July 1995                                      !
!======================================================================!
!     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER( ALFA= 1D0/ALFINV, ALF1 = 1D0/PI/ALFINV)
      SAVE
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPARZ / AMAZ,GAMMZ,SINW2,GV,GA
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / PSIPHI / TH1,EXT1,EXB1,PSI1,EXW1,
     $                  TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      SAVE   / BHPAR2 /, / BHPAR3 /, / TRANSR /
      SAVE   / MOMS1  /, / MOMS2  /, / MOMZ1  /, / MOMZ2  /
      SAVE   / WGTALL /, / BHPARZ /, / PSIPHI /
      DIMENSION sfu(100),bu10(100),bu11(100),hsu(100)
      DIMENSION sfl(100),bl10(100),bl11(100),hsl(100)
      DIMENSION bu11g(100)
      DIMENSION bl11g(100)
      DIMENSION ph1(4)
! ------------------ Inline functions ------------------
! Dot products
! Elements of single bremss. distribution
      CHI(x)= (1+(1-x)**2)/2
      XDELS(a,b,delt) =
     $ delt*(a**2+b**2)*(a**2+b**2)/((1-a)**2+(1-b)**2)/(a*b)
      XRHOR(a,b,psi) =(1-a)*(1-b)+a*b
     $  +2*sqrt(a*b*max(1-a,0d0)*max(1-b,0d0))*cos(psi)
      XCHIR(a,b,rho,zeta) = ( 1      +(1-zeta/max(1-a,zeta))**2
     $                       +rho**2 +(rho -(1-b)*zeta)**2 )/4
!********** alternative ***********
!*     XSFCM(a,b,aa,bb,y,z,delt) = a*b/(y*z)**2
!*    $ +delt/(y*z)**2 *(y**2+z**2)*(a**2+b**2)/((1-aa)**2+(1-bb)**2)
!*     dis1   = facpq* xchir(a,b,xrhor(a,b,psi1),zeta)
!*    $              * xsfcm(a,b,a,b,y,z,deltp)
!**********************************
      DATA icont /0/

!==================================================================!
      s = cmsene**2
      zeta = abs(tran)/s
!*****************************************************
!  Crude MC distribution (S-tilde factors omitted)
      CRUDE  =  s**2/tran**2
      facpq  =  s**2/(tranp*tranq)
      zeta   =  abs(tran)/s
      chi0   =  ( 1+(1-zeta)**2 )/2
      delta  =  amel**2/tran
      deltp  =  amel**2/tranp
      deltq  =  amel**2/tranq
      gam    =  2*alf1 *( dlog(1/delta)-1 )
      gamp   =  2*alf1 *( dlog(1/deltp)-1 )
      gamq   =  2*alf1 *( dlog(1/deltq)-1 )
      psip   = psi1
      psiq   = psi2
!==================================================================!
!    ///////////////////////////////////////
!    /////     UPPER + LOWER line      /////
!    ///////////////////////////////////////
! ------------------
      bt10u =0
      bt10l =0
      bt11u =0
      bt11l =0
      bt20u =0
      bt20l =0
      bt2ul =0
! DEBUG
      bt11ug =0
      bt11lg =0
! ------------------
! beta0
! ------------------
      bt00   =  chi0* s**2 /(tranp*tranq)
      bt01   =  bt00*(1+gam)
      bt02   =  bt00*(1+gam+0.5*gam**2)
! ------------------
! beta1 upper line
! ------------------
      DO i=1,nphot1
         a   = al1(i)
         b   = be1(i)
         y   = a +b*deltp
         z   = b +a*deltp
         v   = a +b -a*b
! soft factor precisely as in mass weight from the generator
         sfc  = a*b/(y*z)**2
         sfu(i) = sfc
! one photon bremss. distribution
         chir    = xchir(a,b,xrhor(a,b,psip),zeta)
         dis10   = facpq * chir *(1+xdels(a,b,deltp)) * sfc
         dis11g  = dis10 *(1+gam + gam/2*log((1-b)/(1-a))) ! with LL bug
         dis11   = dis10 *(1+gam + gam/4*log((1-b)/(1-a)))
         hsu(i) = dis10 / facpq
! beta1 O(alf1),O(alf2)
         bu10(i) =  dis10   -bt00*sfc
         bu11(i) =  dis11   -bt01*sfc
         bt10u   =  bt10u   +bu10(i)/sfc
         bt11u   =  bt11u   +bu11(i)/sfc
! DEBUG
         bu11g(i) =  dis11g   -bt01*sfc ! with LL bug
         bt11ug   =  bt11ug   +bu11g(i)/sfc
      ENDDO
! ------------------
! beta1 lower line
! ------------------
      DO i=1,nphot2
         a   = al2(i)
         b   = be2(i)
         y   = a +b*deltq
         z   = b +a*deltq
         v   = a +b -a*b
! soft factor precisely as in mass weight from the generator
         sfc  = a*b/(y*z)**2
         sfl(i) = sfc
! one photon bremss. distribution
         chir    = xchir(a,b,xrhor(a,b,psiq),zeta)
         dis10   = facpq * chir *(1+xdels(a,b,deltq)) * sfc
         dis11g  = dis10 *(1+gam + gam/2*log((1-b)/(1-a)))
         dis11   = dis10 *(1+gam + gam/4*log((1-b)/(1-a)))
         hsl(i) = dis10 / facpq
! beta1 O(alf1),O(alf2)
         bl10(i) =  dis10   -bt00*sfc
         bl11(i) =  dis11   -bt01*sfc
         bt10l   =  bt10l   +bl10(i)/sfc
         bt11l   =  bt11l   +bl11(i)/sfc
! DEBUG
         bl11g(i) =  dis11g   -bt01*sfc ! with LL bug
         bt11lg   =  bt11lg   +bl11g(i)/sfc
      ENDDO
!------------------
! beta2 upper line
!------------------
      DO i=1,nphot1
      DO j=i+1,nphot1
! Basic variables first photon
      a1  = al1(i)
      b1  = be1(i)
      y1  = a1 +b1*deltp
      z1  = b1 +a1*deltp
      v1  = a1+b1-a1*b1
! Basic variables second photon
      a2  = al1(j)
      b2  = be1(j)
      y2  = a2 +b2*deltp
      z2  = b2 +a2*deltp
      v2  = a2+b2-a2*b2
! Soft factors, starred variables
      sf1  =  sfu(i)
      sf2  =  sfu(j)
      a1st = a1/(1-v2)
      b1st = b1/(1-v2)
      v1st = v1/(1-v2)
      a2st = a2/(1-v1)
      b2st = b2/(1-v1)
      v2st = v2/(1-v1)
      a1st = min(a1st,1-zeta)
      b1st = min(b1st,1-zeta)
      v1st = min(v1st,1-zeta)
      a2st = min(a2st,1-zeta)
      b2st = min(b2st,1-zeta)
      v2st = min(v2st,1-zeta)
! H-functions exact and Leading Log
      gs1    = sf1*chi(v1)
      gs2    = sf2*chi(v2)
      hs1    = hsu(i)
      hs2    = hsu(j)
      gs1st  = sf1*chi(v1st)
      gs2st  = sf2*chi(v2st)
      hs1st  = xchir(a1st,b1st,xrhor(a1st,b1st,psip),zeta)
     $            *(1+xdels(a1st,b1st,deltp))* sf1
      hs2st  = xchir(a2st,b2st,xrhor(a2st,b2st,psip),zeta)
     $            *(1+xdels(a2st,b2st,deltp))* sf2
! The actual two photon distributions
      IF ( (a1-b1)*(a2-b2).le.0d0 ) THEN
!       OPPOSITE directions two photons
        IF (a1.gt.b1) THEN
!          FIRST photon in INITIAL state, second in the final
           IF(v1.gt.v2) THEN
               d2uu  = facpq  *hs1st*gs2
           ELSE
               d2uu  = facpq  *gs1st*hs2
           ENDIF
        ELSE
!          SECOND photon in INITIAL state, first in the final
           IF(v1.gt.v2) THEN
               d2uu  = facpq  *hs1*gs2st
           ELSE
               d2uu  = facpq  *gs1*hs2st
           ENDIF
        ENDIF
      ELSE
!       SAME directions two photons
        IF(v1.gt.v2) THEN
            d2uu  = facpq/2 *( hs1*gs2st + hs1st*gs2)
        ELSE
            d2uu  = facpq/2 *( hs2*gs1st + hs2st*gs1)
        ENDIF
      ENDIF
      bu20  = d2uu-bu10(i)*sfu(j)-bu10(j)*sfu(i)-bt00*sfu(j)*sfu(i)
      bt20u = bt20u + bu20/sfu(i)/sfu(j)
      ENDDO
      ENDDO
! ------------------
! beta2 lower line
! ------------------
      DO i=1,nphot2
      DO j=i+1,nphot2
! Basic variables first photon
      a1  = al2(i)
      b1  = be2(i)
      y1  = a1 +b1*deltq
      z1  = b1 +a1*deltq
      v1  = a1+b1-a1*b1
! Basic variables second photon
      a2  = al2(j)
      b2  = be2(j)
      y2  = a2 +b2*deltq
      z2  = b2 +a2*deltq
      v2  = a2+b2-a2*b2
! soft factors, starred variables
      sf1  =  sfl(i)
      sf2  =  sfl(j)
      a1st = a1/(1-v2)
      b1st = b1/(1-v2)
      v1st = v1/(1-v2)
      a2st = a2/(1-v1)
      b2st = b2/(1-v1)
      v2st = v2/(1-v1)
      a1st = min(a1st,1-zeta)
      b1st = min(b1st,1-zeta)
      v1st = min(v1st,1-zeta)
      a2st = min(a2st,1-zeta)
      b2st = min(b2st,1-zeta)
      v2st = min(v2st,1-zeta)
! H-functions exact and Leading Log
      gs1    = sf1*chi(v1)
      gs2    = sf2*chi(v2)
      hs1    = hsl(i)
      hs2    = hsl(j)
      gs1st  = sf1*chi(v1st)
      gs2st  = sf2*chi(v2st)
      hs1st  = xchir(a1st,b1st,xrhor(a1st,b1st,psiq),zeta)
     $            *(1+xdels(a1st,b1st,deltq))* sf1
      hs2st  = xchir(a2st,b2st,xrhor(a2st,b2st,psiq),zeta)
     $            *(1+xdels(a2st,b2st,deltq))* sf2
! The actual two photon distributions
      IF ( (a1-b1)*(a2-b2).le.0d0 ) THEN
!       OPPOSITE directions two photons
        IF (a1.gt.b1) THEN
!          FIRST photon in INITIAL state, second in the final
           IF(v1.gt.v2) THEN
               d2ll  = facpq  *hs1st*gs2
           ELSE
               d2ll  = facpq  *gs1st*hs2
           ENDIF
        ELSE
!          SECOND photon in INITIAL state, first in the final
           IF(v1.gt.v2) THEN
               d2ll  = facpq  *hs1*gs2st
           ELSE
               d2ll  = facpq  *gs1*hs2st
           ENDIF
        ENDIF
      ELSE
!       SAME directions two photons
        IF(v1.gt.v2) THEN
           d2ll= facpq/2 *( hs1*gs2st  + hs1st*gs2)
       ELSE
           d2ll= facpq/2 *( hs2*gs1st  + hs2st*gs1)
        ENDIF
      ENDIF
      bl20  = d2ll-bl10(i)*sfl(j) -bl10(j)*sfl(i) -bt00*sfl(j)*sfl(i)
      bt20l = bt20l + bl20/sfl(i)/sfl(j)
      ENDDO
      ENDDO
!----------------------------
! beta2 upper and lower line
!----------------------------
      DO i=1,nphot1
      DO j=1,nphot2
! Basic variables first photon
      a1  = al1(i)
      b1  = be1(i)
      y1  = a1 +b1*deltp
      z1  = b1 +a1*deltp
      v1  = a1 +b1-a1*b1
! Basic variables second photon
      a2  = al2(j)
      b2  = be2(j)
      y2  = a2 +b2*deltq
      z2  = b2 +a2*deltq
      v2  = a2+b2-a2*b2
! soft factors
      sf1  =  sfu(i)
      sf2  =  sfl(j)
      IF(v1.gt.v2) THEN
         d2ul= facpq  *hsu(i)       *sf2*chi(v2)
       ELSE
         d2ul= facpq  *sf1*chi(v1)  *hsl(j)
      ENDIF
      b2ul  = d2ul-bu10(i)*sfl(j)-sfu(i)*bl10(j)-bt00*sfu(i)*sfl(j)
      bt2ul = bt2ul + b2ul/sfu(i)/sfl(j)
      ENDDO
      ENDDO

!        **************************************
!        ****   Definitions of MC weights  ****
!        **************************************
!          ---------------------------------
!          !    UPPER line ONLY,  New      !
!          ---------------------------------
! Case of upper line only has separate entries in WTSET because it is 
! analysed in the separate run with separate plotting programs 
! with different entries in vvrho, differerent histogram names etc.
!
! All beta's:  TOTAL O(alf0),O(alf1),O(alf2)
      WTSET(130) =    bt00                /crude
      WTSET(131) =   (bt01+bt10u)         /crude
      WTSET(132) =   (bt02+bt11u +bt20u)  /crude
! Individual beta's in various orders.
! Special O(alf0) bet0 for tests only (without chi0) called WTZERO
      WTSET(134) =   ( s**2/(tranp*tranq) )/crude
! O(alf1)
      WTSET(135) =   bt01  /crude                  ! beta0
      WTSET(136) =   bt10u /crude                  ! beta1
! O(alf2)
      WTSET(137) =   bt02  /crude                  ! beta0
      WTSET(138) =   bt11u /crude                  ! beta1
      WTSET(139) =   bt20u /crude                  ! beta2
! DEBUG
      WTSET(232) =   (bt02+bt11ug +bt20u)  /crude  ! total (LL bug)
      WTSET(238) =   bt11u /crude                  ! beta1 (LL bug)
!          ---------------------------------
!          !  UPPER + LOWER line, New      !
!          ---------------------------------
! All beta's:  O(alf0),O(alf1),O(alf2)
      WTSET(140) =    bt00  /crude
      WTSET(141) =   (bt01+bt10u+bt10l) /crude
      WTSET(142) =   (bt02+bt11u+bt11l+bt20u+bt20l+bt2ul) /crude
! Individual beta's in various orders.
! O(alf1)
      WTSET(143) =    bt01   /crude                ! beta0
      WTSET(144) =   (bt10u+bt10l) /crude          ! beta1
      WTSET(145) =    bt10u  /crude                ! beta1 components
      WTSET(146) =    bt10l  /crude
! O(alf2)
      WTSET(147) =    bt02   /crude                ! beta0
      WTSET(148) =   (bt11u+bt11l)/crude           ! beta1
      WTSET(149) =   (bt20u+bt20l+bt2ul)/crude     ! beta2
      WTSET(150) =    bt11u  /crude                ! beta1 components
      WTSET(151) =    bt11l  /crude
      WTSET(152) =    bt2ul  /crude
      WTSET(153) =    bt20u  /crude                ! beta2 components
      WTSET(154) =    bt20l  /crude
! DEBUG
      WTSET(242) = (bt02+bt11ug+bt11lg+bt20u+bt20l+bt2ul) /crude
      WTSET(250) =    bt11ug  /crude  ! (LL bug)
      WTSET(251) =    bt11l   /crude  ! (LL bug)
!==================================================================
!==================================================================
!                  Non-exponentiated version                      !
!==================================================================
!==================================================================
      pdel  = del*bcud(p1,p2,phsu1)
      qdel  = del*bcud(q1,q2,phsu2)
      fyfsu = exp(-gamp*dlog(1/pdel)  +gamp/4 -alf1/2)
      fyfsl = exp(-gamq*dlog(1/qdel)  +gamq/4 -alf1/2)
! temporary test CONV distr.
      bcon0u    =0
      bcon1u    =0
      bcon2u    =0
! entire distributions
      dis1uC   =0
      dis0u    =0
      dis1u    =0
      dis2u    =0
! beta-like components
      bt00u    =0
      bt01u    =0
      bt02u    =0
      bt10u    =0
      bt11u    =0
      bt20u    =0
! DEBUG part
      dis2ug   = 0
      bt11ug   = 0
!------------------------------------
! Normal one-line virtual correction
      virt1   =  3d0/4*gam -1d0/2*alf1 
! Tweo lines with one YFS formfactor subtraction
! that is: 5/4gam = 2(3/4)gam -1/4gam
      virt1x = 5d0/4*gam-1d0/2*alf1
      virt2x = 0.5d0*(5d0/4*gam)**2
! Note that in order to shorten our formulas we omitt S-factors,
! They always cancel with S-factors from crude distributions
!    ///////////////////////////////////////
!    /////  UNEXP  UPPER line only     /////
!    ///////////////////////////////////////
      IF(nphot1.eq.0) THEN
!     ====================
! [00] No photons, note here gamp=gam
!------
! Clasical O(alf0,1)
        dis0uC  = facpq*chi0 
        dis1uC  = facpq*chi0 *(1 +virt1 +gamp*dlog(del))
! NCONV  
        bcon0u= facpq*chi0 
        bcon1u= facpq*chi0*(1 +gamp*dlog(del))
        bcon2u= facpq*chi0*(1 +gamp*dlog(del) +1./2*(gamp*dlog(del))**2)
! O(alf0,1,2) Entire distribution 
        dis0u = facpq*chi0
        dis1u = facpq*chi0*(1 +gamp*dlog(del))
     $         +facpq*chi0*virt1x
        dis2u = facpq*chi0*(1+gamp*dlog(del) +1./2*(gamp*dlog(del))**2)
     $         +facpq*chi0*(1+gamp*dlog(del))*virt1x
     $         +facpq*chi0*virt2x
!------
! Beta's for controll
        bt00u= dis0u
        bt01u= dis1u
        bt02u= dis2u
! DEBUG part
        dis2ug = dis2u
      ELSEIF(nphot1.eq.1) THEN
!     ========================
! [10] One upper line photon, note here gamp.ne.gam !!!
!------
! Basic distribution
        a   = al1(1)
        b   = be1(1)
        v   = a+b-a*b
! O(alf1) Entire distributions 
        hs1  = xchir(a,b,xrhor(a,b,psip),zeta)*(1+xdels(a,b,deltp))
!------
! Clasical O(alf1)
        dis1uC = facpq *hs1
! NCONV 
        bcon1u   = facpq*chi0
        bcon2u   = facpq*chi0 *(1 +gamp*dlog(del))
!------
! O(alf1,2) Entire distribution
        dis1u  = facpq*hs1
        dis2ug = facpq*hs1*(1 +gamp*dlog(del) +virt1x 
     $       -1/2d0*gam*log(1-a) -1/2d0*gam*log(1-b) )
        dis2u  = facpq*hs1*(1 +gamp*dlog(del) +virt1x 
     $       -1/4d0*gam*log(1-a) -3/4d0*gam*log(1-b) )
! Beta's for control
        bt01u  = facpq*chi0
        bt02u  = facpq*chi0 *(1 +gamp*dlog(del) +virt1x)
        bt10u  = dis1u  -bt01u
        bt11u  = dis2u  -bt02u
        bt11ug = dis2ug -bt02u ! with LL bug

      ELSEIF(nphot1.eq.2) THEN
!     ========================
! [20] Two upper line photon 
!------
! NCONV 
        bcon2u   = facpq*chi0
!------
! Basic distribution
        a1  = al1(1)
        b1  = be1(1)
        a2  = al1(2)
        b2  = be1(2)
        v1  = a1+b1-a1*b1
        v2  = a2+b2-a2*b2
        a1st = min(a1/(1-v2),1-zeta)
        b1st = min(b1/(1-v2),1-zeta)
        a2st = min(a2/(1-v1),1-zeta)
        b2st = min(b2/(1-v1),1-zeta)
        v1st = min(v1/(1-v2),1-zeta)
        v2st = min(v2/(1-v1),1-zeta)
!       H-functions exact and Leading Log
        hs1 =xchir(a1,b1,xrhor(a1,b1,psip),zeta)*(1+xdels(a1,b1,deltp))
        hs2 =xchir(a2,b2,xrhor(a2,b2,psip),zeta)*(1+xdels(a2,b2,deltp))
        hs1st  = xchir(a1st,b1st,xrhor(a1st,b1st,psip),zeta)
     $            *(1+xdels(a1st,b1st,deltp))
        hs2st  = xchir(a2st,b2st,xrhor(a2st,b2st,psip),zeta)
     $            *(1+xdels(a2st,b2st,deltp))
!       The actual two photon distributions
        IF ( (a1-b1)*(a2-b2).le.0d0 ) THEN
!       OPPOSITE directions two photons
          IF (a1.gt.b1) THEN
!          FIRST photon in INITIAL state, second in the final
             IF(v1.gt.v2) d2u  = hs1st*chi(v2)
             IF(v2.ge.v1) d2u  = chi(v1st)*hs2
          ELSE
!          SECOND photon in INITIAL state, first in the final
             IF(v1.gt.v2) d2u  = hs1*chi(v2st)
             IF(v2.ge.v1) d2u  = chi(v1)*hs2st
          ENDIF
        ELSE
!       SAME directions two photons
          IF(v1.gt.v2) d2u  = 1./2 *( hs1*chi(v2st) + hs1st*chi(v2))
          IF(v2.ge.v1) d2u  = 1./2 *( hs2*chi(v1st) + hs2st*chi(v1))
        ENDIF
!------
! O(alf2) Entire distribution dis2u 
        dis2u = facpq*d2u
!------
! Beta's for control
        bt02u = facpq*chi0
        bt11u = facpq*( (hs1-chi0) +(hs2-chi0) )
        bt20u = dis2u -bt11u  -bt02u
! DEBUG part
        dis2ug = dis2u
        bt11ug = bt11u ! with LL bug
      ENDIF
!     =====
!        **************************************
!        ****   Definitions of MC weights  ****
!        ****    Non-exponentiated case    ****
!        **************************************
! Case of upper-line-only has separate entries in WTSET because it is 
! analysed in the separate run with separate plotting programs 
! with different entries in vvrho, differerent histogram names etc.
!
! YFS formfactor fyfsu has to be removed in the unexponentiated case!
!-----
! NCONV for relative comparisons
      WTSET(156) =   bcon1u      /fyfsu/crude ! NCONV O(alf1)
      WTSET(157) =   bcon2u      /fyfsu/crude ! NCONV O(alf2)
!          ---------------------------------
!          /////    UPPER line ONLY    /////
!          ---------------------------------
! TOTAL O(alf0),O(alf1),O(alf2)
      WTSET(160) =   bt00u       /fyfsu/crude
      WTSET(161) =   dis1u       /fyfsu/crude
      WTSET(162) =   dis2u       /fyfsu/crude
      WTSET(164) =   dis1uC      /fyfsu/crude
! Individual beta's in various orders.
! O(alf1)
      WTSET(165) =   bt01u  /fyfsu/crude            ! bt0
      WTSET(166) =   bt10u  /fyfsu/crude            ! bt1
! O(alf2)
      WTSET(167) =   bt02u  /fyfsu/crude            ! bt0
      WTSET(168) =   bt11u  /fyfsu/crude            ! bt1
      WTSET(169) =   bt20u  /fyfsu/crude            ! bt2
! Debug part
      WTSET(262) =   dis2ug   /fyfsu/crude    ! O(alf2) tot BUG
      WTSET(290) =   bt11ug   /fyfsu/crude    ! O(alf2) bt1 BUG

!    ////////////////////////////////////////////
!    /////  UNEXP   UPPER + LOWER line      /////
!    ////////////////////////////////////////////
! CONV for primary tests
      bcon0  =0
      bcon1  =0
      bcon2  =0
! Entire 0,1,2-photon distributions
      dis0   =0
      dis1   =0
      dis2   =0
! beta-like components
      bt00    =0
      bt01    =0
      bt02    =0
      bt10    =0
      bt11    =0
      bt20    =0
! ---------- salami on second order beta1
      bt11rg =0
      bt11uu =0
      bt11ll =0
      bt11ul =0
! ----------salami on second order beta2
      bt20u =0
      bt20l =0
      bt2ul =0
! DEBUG
      dis2g = 0
!
      virt1y = 3/2d0*gam -alf1
      virt2y = 1/2d0*(3/2d0*gam)**2 -3/2d0*alf1*gam
!=================================================
      IF(nphot1.eq.0.and.nphot2.eq.0) THEN
! [00] No photons
!------CONV 
        bcon0=facpq*chi0
        bcon1=bcon0*(1 -2*gam*dlog(1/del))
        bcon2=bcon0*(1 -2*gam*dlog(1/del)+0.5*(2*gam*dlog(1/del))**2 )
!------
! O(alf0,1,2) Entire distribution 
        dis0= facpq*chi0
        dis1= facpq*chi0*(1+2*gam*dlog(del))
     $       +facpq*chi0*virt1y
        dis2= facpq*chi0*(1+2*gam*dlog(del)+0.5*(2*gam*dlog(del))**2)
     $       +facpq*chi0*(1+2*gam*dlog(del))*virt1y
     $       +facpq*chi0* virt2y
!------
! Beta's for controll
        bt00= dis0
        bt01= dis1
        bt02= dis2
! DEBUG
        dis2g = dis2
!=================================================
      ELSEIF(nphot1.eq.1.and.nphot2.eq.0) THEN
! [10] One upper line photon 
!------CONV
        bcon1  = facpq*chi0
        bcon2  = bcon1  *(1 -(gamp+gamq)*dlog(1/del))
!------Basic distribution
        hs1 = (1+xdels(al1(1),be1(1),deltp))
     $        *xchir(al1(1),be1(1),xrhor(al1(1),be1(1),psip),zeta)
!------
! O(alf1,2) Entire distribution
! Note that 1/2 in the log(1-v) term = {1/2 from two tree diagrams} 
        dis1 = facpq*hs1
        dis2g= facpq*hs1*(1 +(gamp+gamq)*dlog(del) +virt1y
     $       -1/2d0*gam*log(1-al1(1)) -1/2d0*gam*log(1-be1(1)) )
        dis2 = facpq*hs1*(1 +(gamp+gamq)*dlog(del) +virt1y
     $       -1/4d0*gam*log(1-al1(1)) -3/4d0*gam*log(1-be1(1))
     $       +1/4d0*(gamp-gam) -(gamp-gam)*log(1-be1(1))  ! emulate 4.02a.exp
     $       )
! Beta's for control
        bt01 = facpq*chi0
        bt02 = facpq*chi0 *(1 +(gamp+gamq)*dlog(del) +virt1y)
        bt10 = dis1  -bt01
        bt11 = dis2  -bt02
! DEBUG part
        bt11g= dis2g -bt02 ! with LL bug
!/////////  SALAMI on bet11 ///////// 
! bet11 rewritten as bt10 +bt11rg +bt11uu +bt11ll +bt11ul
!        bt11 = facpq*(
!     $  (hs1-chi0)                            ! first order
!     $ +(hs1-chi0)*virt1y -hs1*gam/2*log(1-v) ! regular part + bug corr.
!     $ +(hs1-chi0)*gamp*dlog(del)             ! re_uper-im_uper
!     $ +(hs1-chi0)*gamq*dlog(del)             ! re_uper-im_lower
!     $)
        bt11rg = facpq*((hs1-chi0)*virt1y 
     $  +hs1*( -1/4d0*gam*log(1-al1(1)) -3/4d0*gam*log(1-be1(1)) )
     $       +1/4d0*(gamp-gam) -(gamp-gam)*log(1-be1(1))  ! emulate 4.02a.exp
     $       )
        bt11uu = facpq*(hs1-chi0)*gamp*dlog(del)
        bt11ul = facpq*(hs1-chi0)*gamq*dlog(del)

!=================================================
      ELSEIF(nphot1.eq.0.and.nphot2.eq.1) THEN
! [01] One lower line photon 
!------CONV 
        bcon1  = facpq*chi0
        bcon2  = bcon1  *(1 -(gamp+gamq)*dlog(1/del))
!------Basic distribution
        hs1 = (1+xdels(al2(1),be2(1),deltq))
     $        *xchir(al2(1),be2(1),xrhor(al2(1),be2(1),psiq),zeta)
!------
! O(alf1,2) Entire distribution
! Note that 1/2 in the log(1-v) term = {1/2 from two tree diagrams} 
        dis1 = facpq*hs1
        dis2g= facpq*hs1*(1 +(gamp+gamq)*dlog(del) +virt1y
     $       -1/2d0*gam*log(1-al2(1)) -1/2d0*gam*log(1-be2(1)) )
        dis2 = facpq*hs1*(1 +(gamp+gamq)*dlog(del) +virt1y
     $       -1/4d0*gam*log(1-al2(1)) -3/4d0*gam*log(1-be2(1)) 
     $       +1/4d0*(gamq-gam) -(gamq-gam)*log(1-be2(1))   ! emulate 4.02a.exp
     $       )
! Beta's for control
        bt01 = facpq*chi0
        bt02 = facpq*chi0 *(1 +(gamp+gamq)*dlog(del) +virt1y)
        bt10 = dis1  -bt01
        bt11g= dis2g -bt02 ! with LL bug
        bt11 = dis2  -bt02
!/////////  SALAMI on bet11 ///////// 
        bt11rg = facpq*((hs1-chi0)*virt1y 
     $   +hs1*(-1/4d0*gam*log(1-al2(1)) -3/4d0*gam*log(1-be2(1)) )
     $       +1/4d0*(gamq-gam) -(gamq-gam)*log(1-be2(1))   ! emulate 4.02a.exp
     $       )

        bt11ll = facpq*(hs1-chi0)*gamq*dlog(del)
        bt11ul = facpq*(hs1-chi0)*gamp*dlog(del)
!=================================================
      ELSEIF(nphot1.eq.2.and.nphot2.eq.0) THEN
! [20] Two upper line photons 
!------CONV
        bcon2  = facpq*chi0
!------
! Basic distribution
        a1  = al1(1)
        b1  = be1(1)
        a2  = al1(2)
        b2  = be1(2)
        v1  = a1+b1-a1*b1
        v2  = a2+b2-a2*b2
        a1st = min(a1/(1-v2),1-zeta)
        b1st = min(b1/(1-v2),1-zeta)
        a2st = min(a2/(1-v1),1-zeta)
        b2st = min(b2/(1-v1),1-zeta)
        v1st = min(v1/(1-v2),1-zeta)
        v2st = min(v2/(1-v1),1-zeta)
!       H-functions exact and Leading Log
        hs1 =xchir(a1,b1,xrhor(a1,b1,psip),zeta)*(1+xdels(a1,b1,deltp))
        hs2 =xchir(a2,b2,xrhor(a2,b2,psip),zeta)*(1+xdels(a2,b2,deltp))
        hs1st  = xchir(a1st,b1st,xrhor(a1st,b1st,psip),zeta)
     $            *(1+xdels(a1st,b1st,deltp))
        hs2st  = xchir(a2st,b2st,xrhor(a2st,b2st,psip),zeta)
     $            *(1+xdels(a2st,b2st,deltp))
!       The actual two photon distributions
        IF ( (a1-b1)*(a2-b2).le.0d0 ) THEN
!       OPPOSITE directions two photons
          IF (a1.gt.b1) THEN
!          FIRST photon in INITIAL state, second in the final
             IF(v1.gt.v2) d2u  = hs1st*chi(v2)
             IF(v2.ge.v1) d2u  = chi(v1st)*hs2
          ELSE
!          SECOND photon in INITIAL state, first in the final
             IF(v1.gt.v2) d2u  = hs1*chi(v2st)
             IF(v2.ge.v1) d2u  = chi(v1)*hs2st
          ENDIF
        ELSE
!       SAME directions two photons
          IF(v1.gt.v2) d2u  = 1./2 *( hs1*chi(v2st) + hs1st*chi(v2))
          IF(v2.ge.v1) d2u  = 1./2 *( hs2*chi(v1st) + hs2st*chi(v1))
        ENDIF
!------
! O(alf2) Entire distribution dis2u 
        dis2 = facpq*d2u
!------
! Beta's for control
        bt02 = facpq*chi0
        bt11 = facpq*( (hs1-chi0) +(hs2-chi0) )
        bt20 = dis2 -bt11  -bt02
        bt20u= bt20
!/////////  SALAMI on bet11 ///////// 
        bt11uu = bt11
! DEBUG
        dis2g = dis2
!=================================================
      ELSEIF(nphot1.eq.0.and.nphot2.eq.2) THEN
! [02] Two lower line photons 
!------CONV
        bcon2  = facpq*chi0
!------
! Basic distribution
        a1  = al2(1)
        b1  = be2(1)
        a2  = al2(2)
        b2  = be2(2)
        v1  = a1+b1-a1*b1
        v2  = a2+b2-a2*b2
        a1st = min(a1/(1-v2),1-zeta)
        b1st = min(b1/(1-v2),1-zeta)
        a2st = min(a2/(1-v1),1-zeta)
        b2st = min(b2/(1-v1),1-zeta)
        v1st = min(v1/(1-v2),1-zeta)
        v2st = min(v2/(1-v1),1-zeta)
!       H-functions exact and Leading Log
        hs1 =xchir(a1,b1,xrhor(a1,b1,psiq),zeta)*(1+xdels(a1,b1,deltq))
        hs2 =xchir(a2,b2,xrhor(a2,b2,psiq),zeta)*(1+xdels(a2,b2,deltq))
        hs1st  = xchir(a1st,b1st,xrhor(a1st,b1st,psiq),zeta)
     $            *(1+xdels(a1st,b1st,deltq))
        hs2st  = xchir(a2st,b2st,xrhor(a2st,b2st,psiq),zeta)
     $            *(1+xdels(a2st,b2st,deltq))
!       The actual two photon distributions
        IF ( (a1-b1)*(a2-b2).le.0d0 ) THEN
!       OPPOSITE directions two photons
          IF (a1.gt.b1) THEN
!          FIRST photon in INITIAL state, second in the final
             IF(v1.gt.v2) d2u  = hs1st*chi(v2)
             IF(v2.ge.v1) d2u  = chi(v1st)*hs2
          ELSE
!          SECOND photon in INITIAL state, first in the final
             IF(v1.gt.v2) d2u  = hs1*chi(v2st)
             IF(v2.ge.v1) d2u  = chi(v1)*hs2st
          ENDIF
        ELSE
!       SAME directions two photons
          IF(v1.gt.v2) d2u  = 1./2 *( hs1*chi(v2st) + hs1st*chi(v2))
          IF(v2.ge.v1) d2u  = 1./2 *( hs2*chi(v1st) + hs2st*chi(v1))
        ENDIF
!------
! O(alf2) Entire distribution dis2u 
        dis2 = facpq*d2u
!------
! Beta's for control
        bt02 = facpq*chi0
        bt11 = facpq*( (hs1-chi0) +(hs2-chi0) )
        bt20 = dis2 -bt11  -bt02
        bt20l= bt20
c/////////  SALAMI on bet11 ///////// 
        bt11ll = bt11
! DEBUG
        dis2g = dis2
!=================================================
      ELSEIF(nphot1.eq.1.and.nphot2.eq.1) THEN
! [11] One upper and one lower line photons 
!------CONV
        bcon2  = facpq*chi0
!------
! O(alf2) Entire distribution dis2u 
      a1  = al1(1)
      b1  = be1(1)
      a2  = al2(1)
      b2  = be2(1)
      v1  = a1+b1-a1*b1
      v2  = a2+b2-a2*b2
      hs1 = xchir(a1,b1,xrhor(a1,b1,psip),zeta)*(1+xdels(a1,b1,deltp))
      hs2 = xchir(a2,b2,xrhor(a2,b2,psiq),zeta)*(1+xdels(a2,b2,deltq))
      IF(v1.gt.v2) THEN
        d2ul=  hs1     *chi(v2)
      ELSE
        d2ul=  chi(v1) * hs2
      ENDIF
        dis2 = facpq*d2ul
!------
! Beta's for control
        bt02  = facpq*chi0
        bt11  = facpq*( (hs1-chi0) +(hs2-chi0) )
        bt20  = dis2 -bt11 -bt02
        bt2ul = bt20
!/////////  SALAMI on bet11 ///////// 
        bt11ul = bt11
! DEBUG
        dis2g = dis2
      ENDIF
!     =====

!        **************************************
!        ****   Definitions of MC weights  ****
!        ****    Non-exponentiated case    ****
!        **************************************
!          ---------------------------------
!          /////  UPPER + LOWER line   /////
!          ---------------------------------

      fyfs = fyfsu*fyfsl
! TOTAL O(alf0),O(alf1),O(alf2)
      WTSET(170) =    dis0                /fyfs/crude
      WTSET(171) =    dis1                /fyfs/crude
      WTSET(172) =    dis2                /fyfs/crude
!***************************************************************
! Individual beta's in various orders.
! O(alf1)
      WTSET(173) =    bt01         /fyfs/crude       ! beta0
      WTSET(174) =    bt10         /fyfs/crude       ! beta1
! O(alf2)
      WTSET(175) =    bt11uu       /fyfs/crude       ! beta11 up-up
      WTSET(176) =    bt11ll       /fyfs/crude       ! beta11 low-low
      WTSET(177) =    bt02         /fyfs/crude       ! beta0
      WTSET(178) =    bt11         /fyfs/crude       ! beta1
      WTSET(179) =    bt20         /fyfs/crude       ! beta2
      WTSET(180) =    bt11rg       /fyfs/crude       ! beta11 regular 
      WTSET(181) =    bt11ul       /fyfs/crude       ! beta11 up-low
      WTSET(182) =    bt2ul        /fyfs/crude
      WTSET(183) =    bt20u        /fyfs/crude       ! beta2 components
      WTSET(184) =    bt20l        /fyfs/crude
! CONV distribution for tests
      WTSET(185) =   bcon0         /fyfs/crude ! NCONV O(alf0)
      WTSET(186) =   bcon1         /fyfs/crude ! NCONV O(alf1)
      WTSET(187) =   bcon2         /fyfs/crude ! NCONV O(alf2)
! DEBUG part
      WTSET(272) =    dis2g        /fyfs/crude ! Total with LL bug
!***************************************************************
      END
      SUBROUTINE OLDBIS(MODE,XPAR,NPAR)
C     *********************************
C
C           **************************************************    
C           *       **********************************       *
C           *       *      *******************       *       *
C           *       *      *                 *       *       *
C           *       *      *   O L D B I S   *       *       *
C           *       *      *                 *       *       *
C           *       *      *******************       *       *
C           *       **********************************       *
C           **************************************************    
C
C======================================================================
C
C OLDBIS is an improved version of OLDBAB Monte Carlo of
C F. Berends and R. Kleiss Nucl. Phys. B228 (1983) 537.    
C
C OLDBIS was extensively used and partly described in 
C Phys. Lett. B253 (1991) 469, TH-5888.
C
C OLDBIS represents the true QED first order calculation 
C for small angle Bhabha ( < 10degr.) to within  0.02% TECHNICAL 
C precision, see Phys. Lett. B253 (1991) 469  for more details.
C
C For those who are too young to rememeber: 
C OLDBAB was the standard and unique Monte Carlo for luminosity 
C calculations in all PETRA and PEP experiments!
C
C In the following we anwer or commentent on the following questions:
C   (1) What are essential advantages of OLDBIS with respect to OLDBAB
C   (2) What corrections were done with respect to the original source
C   (3) How to use the program?
C======================================================================
C
C Answer to question (1) :
C ======================== 
C
C The most important advantage of OLDBIS with respect to OLDBAB is that
C OLDBIS has well established TECHMICAL precision of 0.02% in a very
C wide range of cut-off parameters, 
C see Phys. Lett. B253(1991)469,TH-5888.
C
C OLDBAB calculates first order QED correction to small angle Bhabha.
C The basic and burning question in the beginning of 1990 was: 
C to what TECHNICAL precision this
C first order correction from OLDBAB (and other programs like BABAMC)
C represent the true and unique QED answer!
C One could only guess that it is about 1%  but no firm statement with 
C solid justification could be found at the time
C in any published papers on the above guess!
C (N.B. 1% precision was enough for PETRA/PEP.)
C In Phys. Lett. B253(1991)469, we have proved that, after some 
C modifications (transforming OLDBAB into OLDBIS), the fantastic
C TECHNICAL precision 0.02% was reached! To this precision OLDBIS
C represents the TRUE first order QED calculation in the 
C low angle Bhabha.
C 
C Answer to question (2) :
C ========================        
C 
C We have started with source code of OLDBAB taken from RADCOR package 
C installed on CERN public disk by R. Kleiss.
C The algorithm and most of source code in present OLDBIS is identical 
C to that of OLDBAB.
C New corrections are of two types: 
C (a) cosmetic ones which concern mainly output/input formats, choice 
C of random number generator etc.
C (b) essential ones which modify
C algorithm, implementation of QED matrix element and usage of program.
C 
C Full list of corrections/modifications:
C 
C (a) Midifications of input/output and other of the cosmetic type:
C -----------------------------------------------------------------
C
C (==( 000 )==) 
C The histograming routines are removed.
C
C (==( 001 )==) 
C The input/output structure was aligned with other programs like
C BHLUMI, KORALZ, KORALB, LESKOF etc.
C In the CALL OLDBIS(MODE,XPAR,NPAR) initialisation, generation
C and postgeneration phases are determined by MODE= -1,0,1 parameter.
C
C (==( 002 )==) 
C All input is read through XPAR and NPAR in the obligatory
C initialization for MODE=-1 and immetiately printed out.
C
C (==( 003 )==) 
C Each generated event (for MODE=0) is encoded in /MOMBAB/.
C
C (==( 004 )==) 
C Calling OLDBIS with MODE=1 or 2 provides 
C cross-section resulting from Monte Carlo integration (necessary
C for histogram normalization).  
C For MODE=1 no output is printed.
C
C (==( 005 )==)
C For MODE=2 in addition to information encoded in XPAR and NPAR 
C certain output is printed. 
C Note that routine BABINF printing output in the same format 
C as in original OLDBAB is kept active.
C
C (==( 006 )==)
C All weight accounting is not done 'by hand' but rather using 
C special routine GMONIT from KORALZ.
C
C (==( 007 )==)
C The modern random number generator VARRAN replaces the original one.
C
C (==( 008 )==)
C Output four-momenta QP,QM,QK are now in GeV units.
C
C (==( 009 )==)
C The other side photon may go outside angular range, user has
C to reject it by himself if he wants.
C
C (b) Modifications of the algorithm and QED matrix element: 
C ----------------------------------------------------------
C
C (==( 100 )==)  
C Detailed insight into MC algorithm of OLDBAB reveals that variable 
C SIGS is a dummy variable i.e. none of the calculated x-sections 
C and distributions depends (within stat. err.) on SIGS. 
C For this to be true SIGS has to be positive, however!
C For very small k0 SIGS becomes negative and the results from 
C the OLDBAB do not represent true first order QED anymore.
C We have corrected this i.e. defined SIGS which is always positive. 
C
C (==( 101 )==)  
C The above corr. (100) is still not enough to avoid problems at the 
C very small k0=XK0 limit which has to be taken in order to check that 
C the program represent first order QED at the 0.02% technical 
C precision level. One has to alow negative weights i.e. introduction
C of weighted events is necessary. This option is implemented
C and the program provides weighted events for switch KEYWGT=1.
C The weight WTM is located in /MOMBAB/ and later transfered 
C to /WGTALL/.  We recommend to use OLDBIS with weighted events.
C
C (==( 102 )==)  
C The essential observation made in Phys. Lett. B253 (1991) 469,TH-5888
C and expoited also in other papers (TH-5995,TH-6118) is that certain
C class of anoying QED corrections, so called up-down interferences,
C is completely unimportant at low angles ( <10 degr.).
C To obtain this result we had to rewrite completely the QED soft and
C hard matrix element in OLDBAB. 
C New routine VIRSEL calculating soft and virtual corrections is added.
C The user has at his disposal four types of matrix element with
C various components switched on/off. 
C Each of them is represented by the separate model weight XWT(10:13). 
C Only one of them 
C                      XWT(10+KEYSIN) 
C is choosen as a model weight 
C for eventual rejection, where KEYSIN=0,1,2,3 is the input parameter:
C               Table 1, WTSET entries for OLDBIS
C----------------------------------------------------------------------
C  Entry        Corrections present/absent in matrix element
C----------------------------------------------------------------------
C               up-down int.   vac.pol.   Z-exch.  s-chan.phot.  
C               -------------------------------------------------------
C  XWT(10)        yes          yes         yes        yes     
C  XWT(11)         no           no          no         no       
C  XWT(12)        yes           no          no         no   principal)
C  XWT(13)        yes           no          no        yes   
C  -------------------------------------------------------------------
C The backward compatibility is kept, XWT(10) represents original
C OLDBAB matrix element (with new vacuum polarization and Z-width).
C The difference XWT(12)-XWT(11) accounts for pure up-down interferece.
C
C (==( 103 )==)  
C The archaic vacuum polarization subprogram REPI (for KEYSIN=0) 
C is replaced by the modern version of the same name.
C
C (==( 104 )==)
C The Z-gamma inerference correction includes now Z width 
C (for KEYSIN=0).
C
C (==( 105 )==) 
C No immediate rejection for events in which one fermion is in 
C theta-trigger but another one (due to photon emission) is out.
C Such an event goes through but has zero weight.
C
C (==( 106 )==) 
C Symmetrisation QP <=> QM can be supressed by setting KEYMIR switch
C KEYMIR = 0. This option was usefull in TH-5888 comparisons
C with semianalytical calculations.
C In the following common block TRAN is transfer to fermion without
C photon emission BEFORE symmetrization
C     COMMON / TRANSR / TRAN,TRANP,TRANQ
C
C Answer to question (3) :
C ======================== 
C
C The complete description of the usage of the program can be found
C in Long-write-up of BHLUMI 2.01, CERN preprint TH-6230.
C Here we only summarize on Input/Output parameters in
C                 CALL BHLUMI(MODE,XPAR,NPAR)
C The user may use directly OLDBIS as well
C                 CALL OLDBIS(MODE,XPAR,NPAR)
C
C IF( MODE =-1 ) THEN
C ===================
C   
C Initialisation is performed, all input parameters are transferred
C through XPAR and NPAR, see table below:
C     Table 2, Input parameters of OLDBIS
C----------------------------------------------------------------------
C  Entry    Variable   Meaning
C----------------------------------------------------------------------
C  NPAR( 1)  KEYOPT =1000*KEYGEN +10*KEYWGT +KEYRND 
C                   General option switch 
C            KEYGEN =1 for this sub-generator
C            KEYRND =1,2 type of random number generator RANMAR,RANECU
C            KEYWGT =0,1 for constant, variable weight WTM
C  NPAR( 2)  KEYRAD =10*KEYMIR +KEYSIN, QED switch defining WTM weight
C                   The meaning of KEYSIN is summarized in Table 1,
C            KEYMIR =0 photon emitted only from both fermions 
C            KEYMIR =1 photon emitted only from QM fermion line (tests)
C  XPAR( 1)  CMSENE Total center mass energy [GeV]
C  XPAR( 2)   THMIN Minimum theta angle for electron [degr]
C  XPAR( 3)   THMAX Maximum theta angle for electron [degr]
C  XPAR( 4)     XK0 k0 parameter, infrared cut on real photon energy
C                   is k0*CMSENE/2, recomended range 0.000001<k0<0.001
C  XPAR( 5)   XKMAX maximum real photon energy is XKMAX*CMSENE/2
C  XPAR( 6)   XKMIN minimum real photon energy is XKMIN*CMSENE/2,
C                   one should normaly set XKMAX=1, XKMIN=0.
C----------------------------------------------------------------------
C
C ELSE IF( MODE = 0 ) THEN
C ========================
C         
C Generation of the single Monte Carlo event
C The four momenta of the final state electron positron and photon
C are primarily encoded in 
C      COMMON / MOMBAB / QP(4),QM(4),QK(4),WTM,XWT(20)
C where QP and QM are four-momenta of positron and electron and QK
C is four-momentum of the photon (from time to time QK is zero).
C They are in GeV units and the z-axis points in the direction of
C the positron beam.
C WTM is the main model weight selected according to KEYSIN switch.
C All four possible model weights with various contributions on/off
C (see table 1) are encoded in XWT. For KEYWGT=0 we have WTM=1
C and XWT should not be used!
C Note that content of /MOMBAB/ is copied to standard multiphoton
C common blocks /MOMSET/ and /WGTALL/ of BHLUMI as well.
C
C
C ELSE IF( MODE = 1 ) THEN
C ========================
C       
C The total cross section corresponding to generated series of event,
C i.e. resulting from MC integrartion is calculated and stored in XPAR,
C together with a lot of auxiliary information, see table 3 below.
C This impressive set of output was nesessary for TH-5888 calculations.
C                         Table 3
C----------------------------------------------------------------------
C  Entry    Variable   Meaning
C----------------------------------------------------------------------
C  NPAR(10)  NEVGEN  Number of generated MC events
C  NPAR(20)  NEVGEN  Number of generated MC events
C  XPAR( 9)    SIG0  Born x-section  [nb]
C  XPAR(10)    XST2  Total x-section [nb]
C  XPAR(11)   RXST2  The relative error of XST2
C  XPAR(12)     XSS  Soft (k<k0)  x-section [nb]
C  XPAR(13)    RXSS  The relative error of XSS
C  XPAR(14)     XSH  Hard (k<k0)  x-section [nb]
C  XPAR(15)    RXSH  The relative error of XSH
C  XPAR(20)    SIGS+SIGH  Crude total MC x-section [nb] which 
C                    is necessary for rescaling histograms in 
C                    run with weighted events.
C  XPAR(21)          =0, error of XPAR(20) is zero
C  XPAR(22)    SIGS  Crude soft MC x-section [nb]
C  XPAR(23)          =0, error of XPAR(22) is zero
C  XPAR(24)    SIGH  Crude hard MC x-section [nb]
C  XPAR(25)          =0, error of XPAR(24) is zero
C----------------------------------------------------------------------
C For constant weight option KEYWGT=0 (convevience in rescaling histos)
C we put XPAR(20,21,22)=XPAR(10,11,12) !
C For MODE=1 program is called upon many times in the process of 
C rescaling histograms there is no output printed.
C
C ELSE IF( MODE = 2 ) THEN
C ========================                     
C 
C Only in this MODE=2 in addition to filling XPAR and NPAR as 
C for MODE=1 the values of various x-sections are printed on 
C standard output file.
C                         
C ENDIF
C ====
C
C
C
C======================================================================
C======================================================================
C History of corrections for the record (stj) 
C you may skip reading this.
C======================================================================
C===== Series of corrections from Jan. 91 to June 91 ==================
C======================================================================
C===== VERSION OF OLDBAB USED IN TH.5888/90 BENCHMARK =================
C===== DILOG and GMONIT moved to library ==============================
C===== REPI replaced with modern version ==============================
C===== Adjusted KEYRAD and KEYPOT/KEYWGT ==============================
C===== Outout XPAR(11) is relative error ==============================
C===== KINBIS translates MOMBAB into MOMSET ===========================
C======================================================================
C===== Series of corrections from Jan. 90 to Dec. 90 ==================
C======================================================================
C  (0) IMODE=-1,0,1,2   initialization, production, output
C  (1) new random number generator
C  (2) QP,QM,PK moved to /MOMBAB/
C  (3) QP,QM,PK in GeV units
C  (4) negative weights in soft part treated properly
C  (5) possibility of weighted events, including negative
C  (6) reorganized QED matrix element with up-down interf. isolated 
C======================================================================
C======================================================================
C
C        Here starts 'original' source code
C
C------------------------------------------- REMARKS ---------------
C
C
C SIMULATION OF RADIATIVE BHABHA SCATTERING IN 2ND & 3RD ORDER Q.E.D.
C WRITTEN BY R.KLEISS (LEIDEN & DESY) OCTOBER '82
C
C
C********************************************************************
C
C A DETAILED WRITEUP, TOGETHER WITH PHYSICS INTERPRETATION
C CAN BE FOUND IN  :
C         F.A. BERENDS AND R. KLEISS , NUCL. PHYS. B228(1983)537 .
C IF YOU USE THIS PROGRAM, PLEASE MAKE REFERENCE TO THE ABOVE PAPER !
C
C********************************************************************
C
C
C BEFORE CALL OF FIRST EVENT, COMMON 'PARM01' MUST BE FILLED WITH :
C EBEAM = BEAM ENERGY (IN GEV) ;
C THMIN = MINIMUM SCATTERING ANGLE OF ELECTRON/POSITRON (IN DEGREES)
C THMAX = MAXIMUM SCATTERING ANGLE OF ELECTRON/POSITRON (IN DEGREES)
C XKMIN = MINIMUM BREMSSTRAHLUNG ENERGY (IN UNITS OF EBEAM) ;
C XKMAX = MAXIMUM BREMSSTRAHLUNG ENERGY (IN UNITS OF EBEAM) .
C XKMIN=0,XKMAX=1 ARE ALLOWED. THMIN=0,THMAX=180 ARE NOT ALLOWED.
C IF XKMIN < 0.01 IT IS ASSUMED TO BE 0.
C QP(I) = FOUR MOMENTUM OF OUTGOING POSITRON (I=1,2,3,4) ;
C QM(I) = FOUR MOMENTUM OF OUTGOING ELECTRON (I=1,2,3,4) ;
C QK(I) = FOUR MOMENTUM OF PHOTON            (I=1,2,3,4) .
C FOUR MOMENTA ARE IN UNITS OF EBEAM. THE INCOMING MOMENTA ARE:
C POSITRON (0,0,1,1) , ELECTRON (0,0,-1,1) .
C--------------------------------------------------------------------
C LATEST UPDATE: APRIL 5,1983.
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      SAVE
      PARAMETER(PI= 3.1415926535897932D0, ALFINV=137.03604D0)   
      PARAMETER(GNANOB =  389385D0) 
CCCCCCCCC                       (==( 002 )==)
      DIMENSION XPAR(*),NPAR(*)
CCCCCCCCC                       (==( 003 )==)
      COMMON / MOMBAB / QP(4),QM(4),QK(4),WTM,XWT(20)
      COMMON / PARM01 / EBEAM,THMIN,THMAX,XKMIN,XKMAX,CMIN,CMAX
      COMMON / PARM02 / SIG0,SIGS,SIGH,SIGT,WT
      COMMON / INOUT  / NINP,NOUT 
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
C Communicates with VARRAN      (==( 007 )==)
      COMMON / RANPAR / KEYRND
C True transfer (==( 106 )==) 
      COMMON / TRANSR / TRAN,TRANP,TRANQ
      DOUBLE PRECISION DRVEC(100)
      DIMENSION WT(18)
CCCCCCCCC                       (==( 001 )==)
C     ===================
      IF(MODE.EQ.-1) THEN
C     ===================
C-------------------------------------------- INITIALIZATION --------
C ...BX-formats for nice and flexible outbuts
      BXOPE =  '(//1X,15(5H*****)    )'
      BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'
      BXL1I =  '(1X,1H*,I17,                 16X, A20,A12,A7, 1X,1H*)'
      BXL1F =  '(1X,1H*,F17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2F =  '(1X,1H*,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXL1G =  '(1X,1H*,G17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2G =  '(1X,1H*,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXCLO =  '(1X,15(5H*****)/   )'
      CALL GMONIT(-1,70,0D0,1D0,1D0)
      CALL GMONIT(-1,71,0D0,1D0,1D0)
      CALL GMONIT(-1,72,0D0,1D0,1D0)
      CALL GMONIT(-1,41,0D0,1D0,1D0)
      CALL GMONIT(-1,42,0D0,1D0,1D0)
      CALL GMONIT(-1,51,0D0,1D0,1D0)
      CALL GMONIT(-1,52,0D0,1D0,1D0)
CCCCCCCCC                       (==( 002 )==)
      KEYOPT=NPAR(1)
      KEYRAD=NPAR(2) 
      CMSENE=XPAR(1)
      THMIN =XPAR(2)
      THMAX =XPAR(3)
      XK0   =XPAR(4)
      XKMAX =XPAR(5)
      XKMIN =XPAR(6)
      EBEAM =CMSENE/2
      KEYRND = MOD(KEYOPT,10)   
      KEYWGT = MOD(KEYOPT,100)/10   
      KEYSIN = MOD(KEYRAD,10)
      KEYMIR = MOD(KEYRAD,100)/10   
CCCCCCCCC                       (==( 002 )==)
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '     **********************      '
      WRITE(NOUT,BXTXT) '     **    O L D B I S   **      '
      WRITE(NOUT,BXTXT) '     **********************      '
      WRITE(NOUT,BXTXT) ' O(alpha) Monte Carlo Program    '
      WRITE(NOUT,BXTXT) ' for the small-angle Bhabha scat.'
      WRITE(NOUT,BXTXT) '         --------------          '
      WRITE(NOUT,BXTXT) '         Important Note          '
      WRITE(NOUT,BXTXT) '         --------------          '
      WRITE(NOUT,BXTXT) ' This is an improved version     '
      WRITE(NOUT,BXTXT) ' of the OLDBAB M.C. program of   '
      WRITE(NOUT,BXTXT) ' F.A. BERENDS AND R. KLEISS      '
      WRITE(NOUT,BXTXT) ' [1] NUCL. PHYS. B228 (1983) 537 '
      WRITE(NOUT,BXTXT) ' Changes were done by S. Jadach, '
      WRITE(NOUT,BXTXT) ' E. Richter-Was and other        '
      WRITE(NOUT,BXTXT) ' authors of the paper            '
      WRITE(NOUT,BXTXT) ' [2] Phys. Lett. B253 (1991) 469 '
      WRITE(NOUT,BXTXT) ' All modifications are desribed  '
      WRITE(NOUT,BXTXT) ' in detail in the source code.   '
      WRITE(NOUT,BXTXT) ' PLEASE CITE references [1,2] !  '
      WRITE(NOUT,BXCLO)
C
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '* This program is part of       *'
      WRITE(NOUT,BXTXT) '*   BHLUMI version 2.01         *'
      WRITE(NOUT,BXTXT) '*   September      1991         *'
      WRITE(NOUT,BXTXT) '*         AUTHORS               *'
      WRITE(NOUT,BXTXT) '* S. Jadach, E. Richter-Was     *'
      WRITE(NOUT,BXTXT) '*    B.F.L. Ward, Z. Was        *'
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXCLO)
C
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '         OLDBIS input            '
      WRITE(NOUT,BXL1I) KEYOPT,     ' option    switch  ','KEYOPT','N1'
      WRITE(NOUT,BXL1I) KEYRND,     ' VARRAN    switch  ','KEYRND','  '
      WRITE(NOUT,BXL1I) KEYWGT,     ' weighting switch  ','KEYWGT','  '
      WRITE(NOUT,BXL1I) KEYRAD,     ' radiation switch  ','KEYRAD','N2'
      WRITE(NOUT,BXL1I) KEYSIN,     ' interf. s-chan.   ','KEYSIN','  '
      WRITE(NOUT,BXL1I) KEYMIR,     ' QM emiss.only/test','KEYMIR','  '
      WRITE(NOUT,BXL1F) CMSENE,     ' CMSENE            ','CMSENE','X1'
      WRITE(NOUT,BXL1F) THMIN ,     ' THMIN             ','THMIN ','X2'
      WRITE(NOUT,BXL1F) THMAX ,     ' THMAX             ','THMAX ','X3'
      WRITE(NOUT,BXL1F) XK0   ,     ' XK0               ','XK0   ','X4'
      WRITE(NOUT,BXL1F) XKMAX ,     ' XKMAX             ','XKMAX ','X5'
      WRITE(NOUT,BXL1F) XKMIN ,     ' XKMIN             ','XKMIN ','X6'
      WRITE(NOUT,BXCLO)
      NEVGEN=0
      WMAX=2.D0
      DO 1 I=1,18
    1 WT(I)=0.
      WT(13)=WMAX
      WT(17)=WMAX 
      AMEL  = 0.511D-3
      XM2=(AMEL/EBEAM)**2
      EM2=1.D0+.5D0*XM2
      XL2=DLOG(2.D0/XM2)
      TWOPI= 2D0*PI
      CMIN=DCOS(THMAX*TWOPI/360.D0)
      CMAX=DCOS(THMIN*TWOPI/360.D0)
      DMIN=1.D0/(1.D0-CMIN)
      DMAX=1.D0/(1.D0-CMAX)
      XLCMIN=XL2+DLOG(1.D0-CMIN)
      XSOFT=1.D0
      IF(XKMIN.LT.XK0) GOTO 2
      XSOFT=0.D0
      XK0=XKMIN
    2 XKRAT=XKMAX/XK0
      ALFA  =  1D0/ALFINV       
      SIGBS =  8D0*PI*ALFA**2/(2D0*EBEAM)**2 *GNANOB
      SIG0  =  SIGBS/16D0  *(
     $ 16*(DMAX-DMIN) + 16*DLOG(DMIN/DMAX) + 9*(CMAX-CMIN)
     $ + (CMAX**2-CMIN**2) + 1.D0/3.D0*(CMAX**3-CMIN**3)         )
      SIGH =  SIGBS * (ALFA/PI)  *4D0 *DLOG(XKRAT)*
     $ ((XL2+1.D0+DLOG(1.D0-CMAX))*DMAX - (1.D0+XLCMIN)*DMIN )  
CC[[[[[[[[[[[[   (==( 100 )==)
C     SIGS=0.D0
C     DO 3 I=1,100
C     R= FLOAT(I-1)/99.D0
C     C=1.-1./(R*DMAX+(1.-R)*DMIN)
C     IF(KEYSIN.EQ.0) THEN
C       CALL VIRSOF(EBEAM,XK0,C,BORN,CORR)
C     ELSE
C       CALL VIRSEL(KEYSIN,EBEAM,XK0,C,BORN,CORR)
C     ENDIF
C&&&3 SIGS=SIGS+BORN*(1.+CORR)*(1.-C)**2
C   3 SIGS=SIGS+BORN*(1.D0     )*(1.D0-C)**2
C the above assures full indep. of xsect. on k0, stj, sept. 89.
C     WRITE(NOUT,*) ' /////CORR IN SIGS EXCLUDED/////'
C     SIGS=SIGS*TWOPI*(DMAX-DMIN)/100.D0*XSOFT
CC]]]]]]]]]]]]
C SIGS is a dummy parameter
C convention: sigma_sof_crude = sigBS/(1-c)**2 (stj)
      SIGS = SIGBS *(DMAX-DMIN)
      SIGT=SIGS+SIGH
      YSOFT=SIGS/SIGT
      ZSOFT=0.D0
      IF(SIGS.NE.0.D0)  ZSOFT=(DMAX-DMIN)/SIGS*TWOPI
      DTOT=SIGT/SIG0-1.
      WRITE(NOUT, 4) XM2   ,EM2   ,XL2   ,TWOPI ,CMIN  ,CMAX
     .       ,DMIN  ,DMAX  ,XLCMIN,XK0   ,XSOFT ,XKRAT
     .       ,WMAX  ,SIG0  ,SIGH  ,SIGS  ,SIGT  ,YSOFT
     .       ,ZSOFT ,DTOT
    4 FORMAT('0',50('=')/,
     . ' INITIALIZATION FOR BHABHA SCATTERING'/,6(' ',4D15.6/))
      WRITE(NOUT,5)EBEAM,THMIN,THMAX,XK0,XKMAX,SIG0,SIGS,SIGH,SIGT,DTOT
    5 FORMAT(
     . '                        BEAM ENERGY =',F15.6,' GEV'/,
     . '           MINIMUM SCATTERING ANGLE =',F15.6,' DEGREES'/,
     . '           MAXIMUM SCATTERING ANGLE =',F15.6,' DEGREES'/,
     . ' MINIMUM HARD BREMSSTRAHLUNG ENERGY =',F15.6/,
     . ' MAXIMUM HARD BREMSSTRAHLUNG ENERGY =',F15.6/,
     . '         LOWEST ORDER CROSS SECTION =',D15.6,' NB'/,
     . ' APPROX. CROSS SECTION IN SOFT PART =',D15.6,' NB'/,
     . ' APPROX. CROSS SECTION IN HARD PART =',D15.6,' NB'/,
     . '        APPROX. CROSS SECTION TOTAL =',D15.6,' NB'/,
     . '           APPROX. TOTAL CORRECTION =',F15.6)
!-------------------------------------------------------------
! This is Generator Identificator
      IDGEN =  1        
! Important histo which remembers total x-section 
      CALL GMONIT(  -1, IDGEN,0D0,2*SIGT,1D0)          
C     ======================
      ELSEIF(MODE.EQ.0) THEN
C     ======================
      NEVGEN=NEVGEN+1
      CALL GMONIT(  0, IDGEN, SIGT, 2*SIGT,1D0)
    6 CONTINUE
      WTM=1D0
      WTK=1D0
C-------------------------------------------- CHOOSE HARD OR SOFT ---
CCCCCCCCC                       (==( 007 )==)
      CALL VARRAN(DRVEC,1)
      IF(DRVEC(1).LT.YSOFT) GOTO 11
C-------------------------------------------- HARD PHOTON PART ------
      WT(1)=WT(1)+1.D0
C-------------------------------------------- GENERATE K VALUE ------
      CALL VARRAN(DRVEC,1)
      XK=XK0*XKRAT**DRVEC(1)
C-------------------------------------------- GENERATE C VALUE ------
    7 CONTINUE
      CALL VARRAN(DRVEC,2)
      C=1.D0-1.D0/(DMIN+DRVEC(1)*(DMAX-DMIN))
      R=DRVEC(2)*XLCMIN/(XL2+DLOG(1.D0-C))
      WT(2)=WT(2)+1.D0
      IF(R.GT.1.D0) GOTO 7
      CM=2.D0*(1.D0-C)
      SC=DSQRT(1.D0-C*C)
C-------------------------------------------- GENERATE FI VALUE -----
      FI=R*TWOPI
C-------------------------------------------- GENERATE U VALUE ------
      D=XM2/CM
      CALL VARRAN(DRVEC,1)
      R=-1.D0+2.D0*DRVEC(1)
      V=(D/(1.D0+D))**DABS(R)
      U=((1.D0+D)*V-D)/(1.D0+V)
      E2=U*(1.D0-U)*CM
      EV=DSQRT(1.D0-E2)
      E2=XM2+E2
      IF(R.LT.0.D0) U=1.D0-U
C-------------------------------------------- GENERATE C1 VALUE -----
      CALL VARRAN(DRVEC,2)
      R=DRVEC(1)
      VC=2.D0*E2*(1.D0-R)/(E2+2.D0*EV*(EM2+EV)*R)
      C1=1.D0-VC
      SC1=DSQRT(VC*(2.D0-VC))
C-------------------------------------------- GENERATE F1 VALUE -----
      F1=TWOPI*DRVEC(2)
      CF1=DCOS(F1)
      SF1=DSIN(F1)
C-------------------------------------------- CONSTRUCT QK DIRECTION
      UC=-1.D0+U-U*C
      QK1=(UC*SC1*CF1-U*SC*C1)/EV
      QK2=SC1*SF1
      QK3=(U*SC*SC1*CF1+UC*C1)/EV
      CG=C*QK3+SC*QK1
C-------------------------------------------- REJECT CT VALUES ------
      XKM=1.D0-XK
      X=2.D0*XKM/(2.D0-XK+XK*CG)
      XT=2.D0-X-XK
      CT=(X*C+XK*QK3)/XT           
CCCCC               (==( 009 )=)
* This correction is important for calorimetric trigger!!!!
****      IF(CT.LT.CMIN.OR.CT.GT.CMAX) WTK=0D0
      WT(3)=WT(3)+ WTK
C-------------------------------------------- CALCULATE WEIGHT ------
      S =4.D0
      S1=4.D0*XKM
      T =-2.D0*X *(1.D0-C )
      T1=-2.D0*XT*(1.D0-CT)
      U =-2.D0*XT*(1.D0+CT)
      U1=-2.D0*X *(1.D0+C )
      X1=XK*(EM2-QK3)
      X2=XK*(EM2+QK3)
      DY=.5D0*XM2*XK/XKM
      Y1=2.D0*(1.D0-XT)+DY
      Y2=2.D0*(1.D0-X )+DY
C Define true transfer (==( 106 )==) 
      tran= ebeam**2*(-T)
CC[[[[[[[[[[[[         (==(102)==)
*.....KEYSIN=0   original OLDBAB 
*.....KEYSIN=1   T NONINTERFERENCE CHANNEL
*.....KEYSIN=2   T CHANNEL WITH INTERF.
*.....KEYSIN=3 S+T CHANNEL
      WTH30=(S*S1*(S*S+S1*S1)+T*T1*(T*T+T1*T1)+U*U1*(U*U+U1*U1))
     .  /(4.*S**3*S1)
     .  *(1.D0-(S*Y1*Y2+S1*X1*X2+U*X2*Y1+U1*X1*Y2)
     .         /(T*X2*Y2+T1*X1*Y1))
     .  *(1.D0-XM2*XK/(1.D0+XKM*XKM)
     .        *(XKM/X1+XKM/X2+1.D0/Y1+1.D0/Y2))
      XWT(10)=  WTK*WTH30
      XWT(13)=  WTK*WTH30
C Here t-channel only and no interferences
      XTCH= (S**2 +U**2 )/(-X1*Y1*T1)*(1+2*XM2*X1/Y1/T1)
     $     +(S1**2+U1**2)/(-X1*Y1*T1)*(1+2*XM2*Y1/X1/T1)
     $     +(S**2 +U1**2)/(-X2*Y2*T )*(1+2*XM2*X2/Y2/T )
     $     +(S1**2+U**2 )/(-X2*Y2*T )*(1+2*XM2*Y2/X2/T )
      XES2= 4*S**2*(-1/T1/X1/Y1 -1/T/X2/Y2)
      WTH1 = XTCH/XES2
      XWT(11)=  WTK*WTH1
C t-chanel only PLUS interferences
      XTCH= (S**2 +U**2 )/(-X1*Y1*T1)*(1+2*XM2*X1/Y1/T1)
     $     +(S1**2+U1**2)/(-X1*Y1*T1)*(1+2*XM2*Y1/X1/T1)
     $     +(S**2 +U1**2)/(-X2*Y2*T )*(1+2*XM2*X2/Y2/T )
     $     +(S1**2+U**2 )/(-X2*Y2*T )*(1+2*XM2*Y2/X2/T )
      XTCHI= XTCH
     $  +1/(T*T1)*(S**2+U**2+S1**2+U1**2)
     $     *( S/X1/X2 +S1/Y1/Y2 +U/X1/Y2 +U1/X2/Y1)
      XES2= 4*S**2*(-1/T1/X1/Y1 -1/T/X2/Y2)  
      WTH2 = XTCHI/XES2
      XWT(12)=  WTK*WTH2
      IF(KEYSIN.GT.3.OR.KEYSIN.LT.0) THEN
       WRITE(NOUT,*) ' ++++ WRONG KEYSIN ',KEYSIN
       STOP
      ENDIF
      WTM   =  XWT(10+KEYSIN)
      XWT(2)=  WTM
CC]]]]]]]]]]]]
C-------------------------------------------- CONSTRUCT MOMENTA -----
      CFI=DCOS(FI)
      SFI=DSIN(FI)
      QK(4)=XK
      QK(3)=XK*QK3
      QK(2)=XK*(QK2*CFI-QK1*SFI)
      QK(1)=XK*(QK2*SFI+QK1*CFI)
      QP(4)=X
      QP(3)=X*C
      QP(2)=-X*SC*SFI
      QP(1)=X*SC*CFI
      DO 8 I=1,4
    8 QM(I)=-QP(I)-QK(I)
      QM(4)=2.D0+QM(4)
C-------------------------------------------- REJECT W VALUES -------
      WT(4)=WT(4)+WTM
      WT(5)=WT(5)+WTM*WTM
      IF(WTM.LT.0.D0)   WT(11)=WT(11)+1.D0
      IF(WTM.GT.WMAX)   WT(12)=WT(12)+1.D0
      IF(WTM.LT.WT(13)) WT(13)=WTM
      IF(WTM.GT.WT(14)) WT(14)=WTM  
      CALL VARRAN(DRVEC,1)
      RN=DRVEC(1)
C principal weight
      CALL GMONIT(0,72,WTM,WMAX,RN)
      CALL GMONIT(0,70,WTM,WMAX,RN)
C auxiliary weights
      CALL GMONIT(0,51,WTH1*WTK,WMAX,RN)
      CALL GMONIT(0,52,WTH2*WTK,WMAX,RN)
C ...
      IF(KEYWGT.EQ.0.AND.RN*WMAX.GT.WTM) GOTO 6
      IF(KEYWGT.EQ.0) WTM=1D0
      WT(6)=WT(6)+1.D0
C-------------------------------------------- REFLECTION POINT ------
      CALL VARRAN(DRVEC,1)
      RN=DRVEC(1)
CCCCCCCCCC         (==( 106 )==)
C Reflection QP <=> QM can be optionally suspended
      IF(KEYMIR.EQ.1) RN=1
      IF(RN.GT.0.5D0) GOTO 10
      DO 9 I=1,3
      QK(I)=-QK(I)
      QPI=QP(I)
      QP(I)=-QM(I)
    9 QM(I)=-QPI
      QPI=QP(4)
      QP(4)=QM(4)
      QM(4)=QPI
   10 CONTINUE
CCCCCCCCCC      (==( 008 )==)
      DO 50 I=1,4
      QP(I)=EBEAM*QP(I)
      QM(I)=EBEAM*QM(I)
   50 QK(I)=EBEAM*QK(I)
      CALL KINBIS
      RETURN
C-------------------------------------------- END OF HARD PART ------
C-------------------------------------------- SOFT PHOTON PART ------
   11 CONTINUE
      WT(7)=WT(7)+1.D0
C-------------------------------------------- GENERATE C VALUE ------
      CALL VARRAN(DRVEC,1)
      C=1.D0-1.D0/(DMIN+DRVEC(1)*(DMAX-DMIN))
C-------------------------------------------- CALCULATE WEIGHT ------
C Define true transfer (==( 106 )==) 
      tran= 2*ebeam**2*(1-C)
CC[[[[[[[[[       (==(102)==)
      CALL VIRSOF(EBEAM,XK0,C,BORN,CORR) 
      XWT(10)=BORN*(1.D0+CORR)*(1.D0-C)**2*ZSOFT
      DO 52 K=1,3
      CALL VIRSEL(K,EBEAM,XK0,C,WBORN,CORR)
      XWT(10+K) =WBORN*(1.D0+CORR)*(1.D0-C)**2
   52 CONTINUE
      WTM   =  XWT(10+KEYSIN)
      XWT(2)=  WTM
C]]]]]]]]]]
      WT(8)=WT(8)+WTM
      WT(9)=WT(9)+WTM*WTM
      IF(WTM.LT.0.D0)   WT(15)=WT(15)+1.D0
      IF(WTM.GT.WMAX)   WT(16)=WT(16)+1.D0
      IF(WTM.LT.WT(17)) WT(17)=WTM
      IF(WTM.GT.WT(18)) WT(18)=WTM
C-------------------------------------------- REJECT W VALUES -------
      CALL VARRAN(DRVEC,1)
      RN=DRVEC(1)  
C principal weight
      CALL GMONIT(0,71,WTM,WMAX,RN)
      CALL GMONIT(0,70,WTM,WMAX,RN)
C auxiliary weights
      CALL VIRSEL(1,EBEAM,XK0,C,WBORN,CORR)
      WTM1=WBORN*(1.D0+CORR)*(1.D0-C)**2
      CALL VIRSEL(KEYSIN,EBEAM,XK0,C,WBORN,CORR)
      WTM2=WBORN*(1.D0+CORR)*(1.D0-C)**2
      CALL GMONIT(0,41,WTM1,WMAX,RN)
      CALL GMONIT(0,42,WTM2,WMAX,RN)
C  (==( 101 )==)
      IF(KEYWGT.EQ.0.AND.RN*WMAX.GT.WTM) GOTO 6
      IF(KEYWGT.EQ.0) WTM=1D0
      WT(10)=WT(10)+1.D0
C-------------------------------------------- GENERATE FI VALUE -----
      FI=R*TWOPI
C-------------------------------------------- CONSTRUCT MOMENTA -----
      CFI=DCOS(FI)
      SFI=DSIN(FI)
      SC=DSQRT(1.D0-C*C)
      QK(4)=0.D0
      QK(3)=0.D0
      QK(2)=0.D0
      QK(1)=0.D0
      QP(4)=1.D0
      QP(3)=C
      QP(2)=SC*SFI
      QP(1)=SC*CFI
      DO 12 I=1,3
   12 QM(I)=-QP(I)
      QM(4)=1.D0
      DO 15 I=1,4
      QP(I)=EBEAM*QP(I)
   15 QM(I)=EBEAM*QM(I)
      CALL KINBIS
C     ===================================
      ELSEIF(MODE.EQ.1.OR.MODE.EQ.2) THEN
C     ===================================
CCCCCCCCC                       (==( 004 )==)
      NPAR(10)= NEVGEN
      NPAR(20)= NEVGEN
      CALL GMONIT(1,70,AWT70,DWT70,DUMM3)
      CALL GMONIT(1,71,AWT71,DWT71,DUMM3)
      CALL GMONIT(1,72,AWT72,DWT72,DUMM3)
      XST    =  SIGT*AWT70
      DXST   =  XST *DWT70
      XSS    =  SIGS*AWT71
      DXSS   =  XSS *DWT71
      XSH    =  SIGH*AWT72
      DXSH   =  XSH *DWT72
      XST2   =  XSS+XSH
      DXST2  =  SQRT(DXSS**2 +DXSH**2)
      XPAR( 9)= SIG0
C for unweighted events, WTM=1D0
      XPAR(10)=  XST2
      XPAR(11)= DXST2/XST2
      XPAR(12)=  XSS
      XPAR(13)= DXSS    
      XPAR(14)=  XSH
      XPAR(15)= DXSH    
C for WEIGHTED events
      XPAR(20)= SIGS+SIGH
      XPAR(21)= 0D0
      XPAR(22)= SIGS
      XPAR(23)= 0D0
      XPAR(24)= SIGH
      XPAR(25)= 0D0
C for unweighted events, WTM=1D0
      IF(KEYWGT.EQ.0) THEN
        DO 313 I=20,25
  313   XPAR(I)=XPAR(I-10)
      ENDIF      
C ...
      CALL GMONIT(1,41,AWT41,DWT41,DUMM3)
      CALL GMONIT(1,51,AWT51,DWT51,DUMM3)
      XSA    =  SIGS*AWT41
      DXSA   =  XSA *DWT41
      XAH    =  SIGH*AWT51
      DXAH   =  XSH *DWT51
      XSA1   =  XSA+XAH
      DXSA1  =  SQRT(DXSA**2 +DXAH**2) 
      XPAR(41)=  XSA1
      XPAR(51)= DXSA1    
      XPAR(60)=  XSA
      XPAR(70)= DXSA
      XPAR(61)=  XAH
      XPAR(71)= DXAH
      CALL GMONIT(1,42,AWT42,DWT42,DUMM3)
      CALL GMONIT(1,52,AWT52,DWT52,DUMM3)
      XSA    =  SIGS*AWT42
      DXSA   =  XSA *DWT42
      XAH    =  SIGH*AWT52
      DXAH   =  XAH *DWT52
      XSA2   =  XSA+XAH
      DXSA2  =  SQRT(DXSA**2 +DXAH**2)  
      XPAR(42)=  XSA2
      XPAR(52)= DXSA2
      IF(MODE.EQ.1) RETURN      
C     ====================
CCCCCCC        (==( 005 )==)
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '        OUTPUT FROM              '
      WRITE(NOUT,BXTXT) '  OLDBIS:        WINDOW A        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '   X.sect. in [nb] units         '
      WRITE(NOUT,BXL1I) NEVGEN,     ' generated events  ','NEVGEN','A0'
      WRITE(NOUT,BXL2F) XSS ,DXSS  ,'Xsec.      soft    ','      ','A1'
      WRITE(NOUT,BXL2F) XSH ,DXSH  ,'Xsec.      hard    ','      ','A2'
      WRITE(NOUT,BXL2F) XST ,DXST  ,'Xsec. straight     ',' total','A3'
      WRITE(NOUT,BXL2F) XST2,DXST2 ,'Xsec. clever       ',' total','A4'
      WRITE(NOUT,BXTXT) '   More on weights etc...        '
      WRITE(NOUT,BXL1F) SIGS ,      'crude Xs. soft     ','      ','A5'
      WRITE(NOUT,BXL1F) SIGH ,      'crude Xs. hard     ','      ','A6'
      DWT71=DWT71*AWT71
      DWT72=DWT72*AWT72
      WRITE(NOUT,BXL2F) AWT71,DWT71,'aver. wt. soft     ','      ','A7'
      WRITE(NOUT,BXL2F) AWT72,DWT72,'aver. wt. hard     ','      ','A8'
      WRITE(NOUT,BXL1F) SIG0 ,      'Born  Xs.          ','      ','A9'
      WRITE(NOUT,BXCLO)  
C ...        
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '  OLDBIS:        WINDOW B        '
      WRITE(NOUT,BXTXT) '     auxiliary information       '
      WRITE(NOUT,BXL2F) XSA1,DXSA1 ,'KSIN=1, no interf. ',' total','B1'
      WRITE(NOUT,BXL2F) XSA2,DXSA2 ,'KSIN=2, with interf',' total','B2'
      WRITE(NOUT,BXCLO)

C old output routine---------
      CALL BABINF
      ENDIF
      END
      SUBROUTINE BABINF
C-------------------------------------------- REMARKS ---------------
C INFO ROUTINE TO BE CALLED AFTER 'BHABHA' HAS RUN EVENTS. IT CALCULA
C THE EXACT CROSS SECTION CORRESPONDING TO THE GENERATED EVENT SAMPLE
C AND SOME STATISTICS ON THE GENERATION.
C WT( 1) = NO.OF STARTS IN HARD-PHOTON PART;
C WT( 2) = NO. OF TRIALS INSIDE C-GENERATION W.R.P. LOOP;
C WT( 3) = NO. OF HARD PHOTON TRIALS SURVIVING C & CT CUTS;
C WT( 4) = SUM OF HARD PHOTON WEIGHTS;
C WT( 5) = SUM OF SQUARED HARD PHOTON WEIGHTS;
C WT( 6) = NO. OF ACCEPTED HARD PHOTON EVENTS;
C WT( 7) = NO. OF STARTS OF SOFT-PHOTON PART;
C WT( 8) = SUM OF SOFT PHOTON WEIGHTS;
C WT( 9) = SUM OF SQUARED SOFT PHOTON WEIGHTS;
C WT(10) = NO. OF ACCEPTED SOFT PHOTON EVENTS;
C WT(11) = NO. OF HARD EVENTS WITH W < 0;
C WT(12) = NO. OF HARD EVENTS WITH W > WMAX;
C WT(13) = MINIMUM GENERATED WEIGHT IN HARD PART;
C WT(14) = MAXIMUM GENERATED WEIGHT IN HARD PART;
C WT(15) = NO. OF SOFT EVENTS WITH W < 0;
C WT(16) = NO. OF SOFT EVENTS WITH W > WMAX;
C WT(17) = MINIMUM GENERATED WEIGHT IN SOFT PART;
C WT(18) = MAXIMUM GENERATED WEIGHT IN HARD PART.
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      SAVE
      COMMON / PARM02 / SIG0,SIGS,SIGH,SIGT,WT
      DIMENSION WT(18)
      COMMON / INOUT  / NINP,NOUT 
      DATA DH/0.D0/,DHE/0.D0/,DDHE/0.D0/,EC/0.D0/
      DATA ECT/0.D0/,EWH/0.D0/,DS/0.D0/,DSE/0.D0/
      DATA DDSE/0.D0/,EWS/0.D0/,DT/0.D0/,DTE/0.D0/,DDTE/0.D0/
      IF(WT(1).EQ.0.D0) GOTO 1
      SIGHE=SIGH*WT(4)/WT(1)
      DSIGHE=SIGH/WT(1)*DSQRT(WT(5)-WT(4)**2/WT(1))
      DH  =SIGH/SIG0
      DHE =SIGHE/SIG0
      DDHE=DSIGHE/SIG0
      EC=WT(1)/WT(2)
      ECT=WT(3)/WT(1)
      EWH=WT(6)/WT(3)
    1 CONTINUE
      IF(WT(7).EQ.0.D0) GOTO 2
      SIGSE=SIGS*WT(8)/WT(7)
      DSIGSE=SIGS/WT(7)*DSQRT(WT(9)-WT(8)**2/WT(7))
      DS  =SIGS/SIG0
      DSE =SIGSE/SIG0
      DDSE=DSIGSE/SIG0
      EWS=WT(10)/WT(7)
    2 CONTINUE
      DT  =SIGT/SIG0
      DTE =DHE+DSE
C&&&& DDTE=DDHE+DDSE
      DDTE=SQRT(DDHE**2+DDSE**2)
      SIGTOT=DTE*SIG0
      SIGSFT=DSE*SIG0
      SIGHRD=DHE*SIG0
C-----
      WRITE(NOUT,3) SIG0,SIGH,SIGS,SIGT,(WT(I),I=1,18)
    3 FORMAT(1H0,90(1H-)/,' BHABHA SAMPLE STATISTICS'/,(4D15.6))
      WRITE(NOUT,1004) SIG0,DH,DHE,DDHE,EC,ECT,EWH,DS,DSE,DDSE
      WRITE(NOUT,1005) EWS,DT,DTE,DDTE,SIGTOT,SIGSFT,SIGHRD
 1004 FORMAT(
     . '          LOWEST ORDER CROSS SECTION =',D15.6,' NB = UNIT'/,
     . '   APPROXIMATED HARD PHOTON XSECTION =',F15.6/,
     . '          EXACT HARD PHOTON XSECTION =',F15.6/,
     . '                         UNCERTAINTY =',F15.6/,
     . ' W.R.P EFFICIENCY IN INTERNAL C LOOP =',F15.6/,
     . '   '      '      OF C/CT RESTRICTION =',F15.6/,
     . '   '      '         FOR HARD WEIGHTS =',F15.6/,
     . '   APPROXIMATED SOFT PHOTON XSECTION =',F15.6/,
     . '          EXACT SOFT PHOTON XSECTION =',F15.6/,
     . '                         UNCERTAINTY =',F15.6)
 1005 FORMAT(
     . '  W.R.P. EFFICIENCY FOR SOFT WEIGHTS =',F15.6/,
     . '    APPROXIMATED TOTAL CROSS SECTION =',F15.6/,
     . '           EXACT TOTAL CROSS SECTION =',F15.6/,
     . '                         UNCERTAINTY =',F15.6/,
     . ' ----------> TOTAL CROSS SECTION =====',D15.6,' NB'/,
     . ' ---------->  SOFT CROSS SECTION =====',D15.6,' NB'/,
     . ' ---------->  HARD CROSS SECTION =====',D15.6,' NB')
      WRITE(NOUT,5) (WT(I),I=11,18)
    5 FORMAT('0 GENERATED WEIGHTS:'/,
     . '          < 0 IN HARD PART =',F15.6/,
     . '       > WMAX IN HARD PART =',F15.6/,
     . '      MINIMUM IN HARD PART =',F15.6/,
     . '      MAXIMUM IN HARD PART =',F15.6/,
     . '          < 0 IN SOFT PART =',F15.6/,
     . '       > WMAX IN SOFT PART =',F15.6/,
     . '      MINIMUM IN SOFT PART =',F15.6/,
     . '      MAXIMUM IN SOFT PART =',F15.6)
      END
      SUBROUTINE VIRSEL (KEY,EBEAM,XK0,COST,WBORN,CORR)
C     ************************************************
C new routine, not present in OLDBAB     (==(102)==)
C......COMPACT FORMULA FOR CROSS SECTION IN BHABHA PROCESS
C......BORN + SOFT +VIRTUAL CORRECTIONS... ONLY QED VERSION
C......MOST OF THE FORMULAS FROM
C......BOHM,DENNIER,HOLLIK,NUCL.PHYS.B304(1988),687
C......COMPLETED E. RICHTER-WAS. APRIL 1990.........
*.....AMGAM IS DUMMY PARAMETR
*.....KEY=1   T NONINTERFERENCE CHANNEL
*.....KEY=2   T CHANNEL
*.....KEY=3 S+T CHANNEL
*     ********************
      IMPLICIT REAL*8(A-H,O-Z)
      SAVE
      DATA PI,ALFINV /3.1415926535897932D0, 137.03604D0/
      DATA AMGAM,AMEL /1D0, 0.511D-3/
 
      ALFA=1D0/ALFINV
      ALFPI=ALFA/PI
      AKMAX=XK0*EBEAM
      S=4D0*EBEAM**2
      T=-1/2D0*S*(1D0-COST)
      U=-1/2D0*S*(1D0+COST)
      DLT=DLOG(-T/AMEL**2)
      DLS=DLOG( S/AMEL**2)
      DLU=DLOG(-U/AMEL**2)
      DLTG=DLOG(-T/AMGAM**2)
      DLSG=DLOG( S/AMGAM**2)
      DLKG=DLOG(2D0*AKMAX/AMGAM)
      DLKGM=DLOG(AMEL**2*AKMAX**2/AMGAM**2/EBEAM**2)
      DLUS=DLOG(-U/S)
      DLTS=DLOG(-T/S)
      DLTU=DLOG(T/U)
C........S + T CHANNEL...............
      IF(KEY.EQ.3) THEN
C....BORN LEVEL
      XT1=U/T
      XT3=S/T
      XS1=U/S
      XS2=T/S
      XT=XT1**2+XT3**2
      XS=XS1**2+XS2**2
      XST=2D0*XS1*XT1
      BORN=XT+XS+XST
C....VIRTUAL CORRECTIONS
      VIRT =ALFPI*(
     $      -1D0+PI*PI/12D0+1/4D0*DLT+1/4D0*DLT**2
     $      +1/2D0*DLTG*(1D0-DLT))
      DVIRT=4D0*XT*VIRT
      VIRS =ALFPI*(
     $      -1D0+PI*PI/3D0+1/4D0*DLS+1/4D0*DLS**2
     $      +1/2D0*DLSG*(1D0-DLS))
      DVIRS=4D0*XS*VIRS
      DVIRST  =2D0*(VIRT+VIRS)*XST
C.....BOX CORRECTIONS
      BOXS1= 2D0*ALFPI*(-DLTU*DLSG
     $      +S/2D0/(S+T)*DLTS-S*(S+2D0*T)/4D0/(S+T)**2*DLTS**2)
      BOXS2= 2D0*ALFPI*(-DLTU*DLSG
     $      -S/2D0/(S+U)*DLUS+S*(S+2D0*U)/4D0/(S+U)**2*DLUS**2)
      BOXT1= 2D0*ALFPI*(+DLUS*DLTG
     $      -T/2D0/(T+S)*DLTS-T*(T+2D0*S)/4D0/(T+S)**2*DLTS**2)
      BOXT3= 2D0*ALFPI*(+DLUS*DLTG
     $ +T/2D0/(T+U)*DLTU+T*(T+2D0*U)/4D0/(T+U)**2*(DLTU**2+PI*PI))
      DBOXT=BOXT1*XT1**2+BOXT3*XT3**2
      DBOXS=BOXS1*XS1**2+BOXS2*XS2**2
      DBOXST=1/2D0*(BOXS1+BOXT1)*XST
C.....SOFT CORRECTIONS
      XS=2D0*ALFPI*(
     $      (DLS-1D0)*DLKGM+1/2D0*DLS**2-PI*PI/3D0       )
      XT=2D0*ALFPI*(
     $      (DLT-1D0)*DLKGM+1/2D0*DLT**2
     $      -1/2D0*DLTS**2+PI*PI/6D0
     $      -2D0*DILOGY(1D0+2D0*EBEAM/DSQRT(-T))
     $      -2D0*DILOGY(1D0-2D0*EBEAM/DSQRT(-T))   )
      XU=2D0*ALFPI*(
     $      (DLU-1D0)*DLKGM+1/2D0*DLU**2
     $      -1/2D0*DLUS**2+PI*PI/6D0
     $      -2D0*DILOGY(1D0+2D0*EBEAM/DSQRT(-U))
     $      -2D0*DILOGY(1D0-2D0*EBEAM/DSQRT(-U))   )
      SOFT=XS+XT-XU
C.....ADDED TOGETHER
      CORS=DVIRS+DBOXS
      CORT=DVIRT+DBOXT
      CORST=DVIRST+DBOXST
      CORSOFT=BORN*SOFT
      BORNCOR=BORN+CORSOFT+CORS+CORT+CORST
      CORR=(BORNCOR-BORN)/BORN
      ENDIF
C.....end of...S + T CHANNEL...end of............
C........ T CHANNEL...............
      IF(KEY.EQ.2) THEN
C....BORN LEVEL
      XT1=U/T
      XT3=S/T
      XT=XT1**2+XT3**2
      BORN=XT
C....VIRTUAL CORRECTIONS
      VIRT =ALFPI*(
     $      -1D0+PI*PI/12D0+1/4D0*DLT+1/4D0*DLT**2
     $      +1/2D0*DLTG*(1D0-DLT))
      DVIRT=4D0*XT*VIRT
C.....BOX CORRECTIONS
      BOXT1= 2D0*ALFPI*(+DLUS*DLTG
     $      -T/2D0/(T+S)*DLTS-T*(T+2D0*S)/4D0/(T+S)**2*DLTS**2)
      BOXT3= 2D0*ALFPI*(+DLUS*DLTG
     $ +T/2D0/(T+U)*DLTU+T*(T+2D0*U)/4D0/(T+U)**2*(DLTU**2+PI*PI))
      DBOXT=BOXT1*XT1**2+BOXT3*XT3**2
C.....SOFT CORRECTIONS
      XS=2D0*ALFPI*(
     $      (DLS-1D0)*DLKGM+1/2D0*DLS**2-PI*PI/3D0       )
      XT=2D0*ALFPI*(
     $      (DLT-1D0)*DLKGM+1/2D0*DLT**2
     $      -1/2D0*DLTS**2+PI*PI/6D0
     $      -2D0*DILOGY(1D0+2D0*EBEAM/DSQRT(-T))
     $      -2D0*DILOGY(1D0-2D0*EBEAM/DSQRT(-T))   )
      XU=2D0*ALFPI*(
     $      (DLU-1D0)*DLKGM+1/2D0*DLU**2
     $      -1/2D0*DLUS**2+PI*PI/6D0
     $      -2D0*DILOGY(1D0+2D0*EBEAM/DSQRT(-U))
     $      -2D0*DILOGY(1D0-2D0*EBEAM/DSQRT(-U))   )
      SOFT=XS+XT-XU
C.....ADDED TOGETHER
      CORT=DVIRT+DBOXT
      CORSOFT=BORN*SOFT
      BORNCOR=BORN+CORSOFT+CORT
      CORR=(BORNCOR-BORN)/BORN
      ENDIF
C....end of....T  CHANNEL........end of........
C........ T non interference CHANNEL...............
      IF(KEY.EQ.1) THEN
C....BORN LEVEL
      XT1=U/T
      XT3=S/T
      XT=XT1**2+XT3**2
      BORN=XT
C....VIRTUAL CORRECTIONS
      VIRT =ALFPI*(
     $      -1D0+PI*PI/12D0+1/4D0*DLT+1/4D0*DLT**2
     $      +1/2D0*DLTG*(1D0-DLT))
C.....SOFT CORRECTIONS
      SOFT=2D0*ALFPI*(
     $      (DLT-1D0)*DLKGM+1/2D0*DLT**2
     $      -1/2D0*DLTS**2+PI*PI/6D0
     $      -2D0*DILOGY(1D0+2D0*EBEAM/DSQRT(-T))
     $      -2D0*DILOGY(1D0-2D0*EBEAM/DSQRT(-T))   )
C.....ADDED TOGETHER
      CORR=SOFT+4D0*VIRT
      ENDIF
C....end of....T non interference  CHANNEL........end of........   
      WBORN = BORN/8D0
      END
 
      SUBROUTINE VIRSOF(EB,XK0,X,BORN,CORR)
C-------------------------------------------- REMARKS ---------------
C BHABHA SCATTERING DIFFERENTIAL CROSS SECTION WITH CORRECTIONS:
C 1) SELF-ENERGY DIAGRAMS FOR IN- AND OUTGOING LEPTONS;
C 2) VERTEX CORRECTION DIAGRAMS;
C 3) BOX DIAGRAMS (TWO-PHOTON EXCHANGE);
C 4) SOFT BREMSSTRAHLUNG (PHOTON ENERGY < XK0*EBEAM );
C 5) VACUUM POLARIZATION (PHOTON SELF-ENERGY DIAGRAMS);
C 6) INTERFERENCE BETWEEN PHOTON AND Z0 EXCHANGE GRAPHS.
C FORMULA AND CONVENTIONS TAKEN FROM:
C F.A.BERENDS ET AL, NUCL.PHYS.B68(1974)541.
C EB   = BEAM ENERGY IN GEV;
C XK0  = CUTOFF ON SOFT BREMSSTRAHLUNG ENERGY;
C X    = COSINE OF POLAR SCATTERING ANGLE OF POSITRON;
C BORN = LOWEST-ORDER DIFFERENTIAL CROSS SECTION IN NANOBARN;
C CORR = TOTAL OF CORRECTIONS GIVEN ABOVE.
C ASSUMED VALUES: 90 GEV FOR THE Z0 MASS, .23 FOR SIN**2(TH).
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      SAVE
      COMMON / INOUT  / NINP,NOUT 
C-------------------------------------------- LOWEST ORDER ----------
      X2=X*X
      X3=X2*X
      X4=X3*X
      XNUM=9.D0+6.D0*X2+X4
      BORN=1.295958D0/EB**2*XNUM/(1.-X)**2
C-------------------------------------------- CORRECTIONS (NO VAC.POL
      U=2.D0*DLOG(3.9139D03*EB)
      V=U+DLOG(1.+X)-0.6931472D0
      W=U+DLOG(1.-X)-0.6931472D0
      CORR=2.3228D-03*(
     .-4.D0*(1.D0-U+V-W)*DLOG(XK0) - 6.5797D0 + U*U - V*V + W*W
     .+ 2.*DILOGY((1.D0+X)/2.D0) - 2.*DILOGY((1.D0-X)/2.D0)
     .+ ( ( 1.-12.*X+12.*X2 -4.*X3 +3.*X4)*U
     .   -( 5. -7.*X +3.*X2    -X3       )*V
     .   +(31. +5.*X +9.*X2 +3.*X3       )*W
     .   +( 3. +7.*X -5.*X2 -3.*X3 -2.*X4)*U*U*.5
     .   +( 3. -3.*X    +X2    -X3       )*V*V
     .   -( 9. +7.*X+11.*X2 +5.*X3       )*W*W*.5
     .   -( 2.    -X           -X3       )*U*V*X*2.
     .   -(21. +3.*X +9.*X2 -3.*X3 +2.*X4)*U*W
     .   +( 6. +5.*X +4.*X2    +X3       )*V*W*2.
     .   -(36.      +24.*X2        +4.*X4)
     .   +(18.-15.*X+12.*X2 -3.*X3 +4.*X4)*3.2899)/XNUM)
C<<<<<--------------------------------------- VACUUM POLARIZATION ---
C[[[[[       (==( 103 )=)
      CORR=CORR+
     .(6.*X -6.*X2 +2.*X3 -2.*X4)*REPI(4.*EB*EB)/XNUM+
     .(-18.-6.*X-6.*X2-2.*X3)*REPI(-2.*EB*EB*(1.-X))/XNUM
C<<<<---------------------------------------- INTERFERENCE WITH Z0 --
C&&&  CORR=CORR + XSWEAK(EB,9.D01,.23D0,X)/BORN
CCCC  CORR=CORR + XSWEAK(EB,92D0 ,0.2288D0,X)/BORN
CCCCCCCCCCC (==( 104 )==)
CCCC  Note outdated Z mass, width and SINW2 here !!!!
      CORR=CORR + YYWEAK(EB,92D0,2.4533D0,0.2288D0,X)
      Y=BORN*(1.+CORR)*1000.D0
CCC   IF(CORR.LT.-1.D0) WRITE(NOUT,1) CORR,EB,XK0,X
CCC 1 FORMAT(' ***VIRSOF WARNING : ',4D15.6)
      END    
CCCCCCCCCCCCCCCC (==( 104 )==)
C slightly improved version of XSWEAK with finite width of Z resonance
CCCCCCCCCCCCCCCC
C BHABHA CROSS SECTION DUE TO INTERFERENCE OF THE PHOTON AND Z0 GRAPH
C EB  = BEAM ENERGY (GEV)
C XMZ0= Z0 MASS (GEV)
C GAMZ0= Z0 WIDTH (GEV)
C SIN2= SIN(WEAK MIXING ANGLE)**2
C X   = COSINE OF POLAR SCATTERING ANGLE OF POSITRON
      FUNCTION YYWEAK(EB,XMZ0,GAMZ0,SIN2,X)
*     *************************************
      IMPLICIT REAL*8(A-H,O-Z)
      SAVE
      XM=1.-X
      A2=1./(16.*SIN2*(1.-SIN2))
      VP=((1.-4.*SIN2)**2+1.)*A2
      VM=VP-2.*A2
      SS=4*EB**2
      TT= SS*(1-X)/2D0
C (==( 104 )==)
CC    XS=SS/(SS-XMZ0**2 )
      XS=SS*(SS-XMZ0**2)/((SS-XMZ0**2)**2+(XMZ0*GAMZ0)**2)
      XT=TT/(TT+XMZ0**2 )
      XP=(1.+X)**2
CC    FACT=5.183833D0/EB**2
CC    BORN=1.295958D0/EB**2*XNUM/(1.-X)**2
      XNUM=9.D0+6.D0*X**2+X**4
      FACT= 4D0/XNUM*XM**2
      YYWEAK=FACT*( XS*( VP*XP + VM*XM**2 )/4.
     $            + XT*( VP*XP + VM*4.    )/(XM*XM)
     $        -(XS+XT)*( VP*XP            )/(2.*XM) )
      END
C==================== not used anymore...
      FUNCTION XSWEAK(EB,XMZ0,SIN2,X)
C-------------------------------------------- REMARKS ---------------
C BHABHA CROSS SECTION DUE TO INTERFERENCE OF THE PHOTON AND Z0 GRAPH
C EB  = BEAM ENERGY (GEV)
C XMZ0= Z0 MASS (GEV)
C SIN2= SIN(WEAK MIXING ANGLE)**2
C X   = COSINE OF POLAR SCATTERING ANGLE OF POSITRON
C THE COUPLING CONSTANTS OF THE ELECTRONS TO THE Z0 ARE CALCULATED
C ACCORDING TO THE STANDARD SU(2)*U(1) MODEL, USING SIN2. THE MASS
C XMZ0 IS TREATED AS AN ADDITIONAL, INDEPENDENT PARAMETER.
C NEITHER THE PURE Z0 CHANNEL, NOR THE EFFECTS OF A NONZERO Z0 WIDTH
C ARE TAKEN INTO ACCOUNT ---> THIS ROUTINE IS NOT GOOD FOR LEP/SLC.
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      SAVE
      DATA INIT /0/
      IF(INIT.NE.0) GOTO 1
      INIT=1
C-------------------------------------------- INITIALIZATION --------
      A2=1./(16.*SIN2*(1.-SIN2))
      VP=((1.-4.*SIN2)**2+1.)*A2
      VM=VP-2.*A2
      CHIQ=(XMZ0/EB)**2/2.
      XS=1./(1.-CHIQ/2.)
      FACT=5.183833D0/EB**2
C--------------------------------------------------------------------
    1 XM=1.-X
      XP=(1.+X)**2
      XT=1./(1.+CHIQ/XM)
      XSWEAK=FACT*( XS*( VP*XP + VM*XM**2 )/4.
     .            + XT*( VP*XP + VM*4.    )/(XM*XM)
     .        -(XS+XT)*( VP*XP            )/(2.*XM) )
      RETURN
      END

      SUBROUTINE KINBIS       
C     *****************
C TRANSTATES /MOMBAB/ INTO /MOMSET/     
C     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      SAVE
C from OLDBIS
      COMMON / PARM01 / EBEAM,THMIN,THMAX,XKMIN,XKMAX,CMIN,CMAX
      COMMON / MOMBAB / QP(4),QM(4),QK(4),WTM,XWT(20)     
C to BHLUMI
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300) 
      COMMON / TRANSR / TRAN,TRANP,TRANQ
      AMEL  =  0.5111D-3
C beams
      P1(4)= EBEAM
      P1(3)= DSQRT(EBEAM**2-AMEL**2)
      P1(2)= 0D0
      P1(1)= 0D0
C...
      Q1(4)= EBEAM
      Q1(3)=-DSQRT(EBEAM**2-AMEL**2)
      Q1(2)= 0D0
      Q1(1)= 0D0
C final electrons
      DO 10 K=1,4
      P2(K)=QP(K)
      Q2(K)=QM(K)
   10 CONTINUE      
C photon, if present  
      NPHOT=0
      IF(QK(4).NE.0D0) THEN
       NPHOT=1
       DO 20 K=1,4
   20  PHOT(1,K)=QK(K)
      ENDIF
C Transfers    
      tranp= 2*(p1(4)*p2(4)-p1(3)*p2(3)-p1(2)*p2(2)-p1(1)*p2(1))
      tranq= 2*(q1(4)*q2(4)-q1(3)*q2(3)-q1(2)*q2(2)-q1(1)*q2(1))
C weights
      WTCRU1=1D0
      WTCRU2=1D0
      WTMOD    = WTM
      DO 70 K=1,20
   70 WTSET(K) = XWT(K)                   
C----------------------------------------------------------------C
C                      The end of OLDBIS                         C
C----------------------------------------------------------------C
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
! ****** Pure Leptonic  ******
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
      SAVE
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
      DOUBLE PRECISION DRVEC(100)
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
ccc     CALL VARRAN(RNUMB,1)
        CALL VARRAN(DRVEC,1)
        RNUMB=DRVEC(1)
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
!###   write(6,*) 'gausjd: CALK8,CALK16=',ITER,CALK8,CALK16,ERELA
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
C=======================================================================
C=======================================================================
      SUBROUTINE MARRAN(RVEC,LENV)
C =======================S. JADACH===================================
C == This commes from F. James, The name of RANMAR is changed to   ==
C == MARRAN in order to avoid interference with the version        ==
C == already in use and the public library version (if present).   ==
C ==      THIS IS THE ONLY MODIFICATION !!!!                       ==
C ========================S. JADACH==================================
C Universal random number generator proposed by Marsaglia and Zaman
C in report FSU-SCRI-87-50
C        modified by F. James, 1988 and 1989, to generate a vector
C        of pseudorandom numbers RVEC of length LENV, and to put in
C        the COMMON block everything needed to specify currrent state,
C        and to add input and output entry points MARINI, MAROUT.
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for RANMAR:                                  ++
C!!!      CALL RANMAR (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL MARINI(I1,N1,N2)   initializes the generator from one ++
C!!!                   32-bit integer I1, and number counts N1,N2    ++
C!!!                  (for initializing, set N1=N2=0, but to restart ++
C!!!                    a previously generated sequence, use values  ++
C!!!                    output by MAROUT)                            ++
C!!!      CALL MAROUT(I1,N1,N2)   outputs the value of the original  ++
C!!!                  seed and the two number counts, to be used     ++
C!!!                  for restarting by initializing to I1 and       ++
C!!!                  skipping N2*100000000+N1 numbers.              ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(*)
      COMMON/RASET1/U(97),C,I97,J97
      PARAMETER (MODCNS=1000000000)
      SAVE CD, CM, TWOM24, NTOT, NTOT2, IJKL
      DATA NTOT,NTOT2,IJKL/-1,0,0/
C
      IF (NTOT .GE. 0)  GO TO 50
C
C        Default initialization. User has called RANMAR without MARINI.
      IJKL = 54217137
      NTOT = 0
      NTOT2 = 0
      KALLED = 0
      GO TO 1
C
      ENTRY      MARINI(IJKLIN, NTOTIN,NTOT2N)
C         Initializing routine for RANMAR, may be called before
C         generating pseudorandom numbers with RANMAR. The input
C         values should be in the ranges:  0<=IJKLIN<=900 OOO OOO
C                                          0<=NTOTIN<=999 999 999
C                                          0<=NTOT2N<<999 999 999!
C To get the standard values in Marsaglia's paper, IJKLIN=54217137
C                                            NTOTIN,NTOT2N=0
      IJKL = IJKLIN
      NTOT = MAX(NTOTIN,0)
      NTOT2= MAX(NTOT2N,0)
      KALLED = 1
C          always come here to initialize
    1 CONTINUE
      IJ = IJKL/30082
      KL = IJKL - 30082*IJ
      I = MOD(IJ/177, 177) + 2
      J = MOD(IJ, 177)     + 2
      K = MOD(KL/169, 178) + 1
      L = MOD(KL, 169)
      WRITE(6,'(A,5I10)')
     $'MARran INITIALIZED: IJ,KL,IJKL,NTOT,NTOT2=',IJ,KL,IJKL,NTOT,NTOT2
      DO 2 II= 1, 97
      S = 0.
      T = .5
      DO 3 JJ= 1, 24
         M = MOD(MOD(I*J,179)*K, 179)
         I = J
         J = K
         K = M
         L = MOD(53*L+1, 169)
         IF (MOD(L*M,64) .GE. 32)  S = S+T
    3    T = 0.5*T
    2 U(II) = S
      TWOM24 = 1.0
      DO 4 I24= 1, 24
    4 TWOM24 = 0.5*TWOM24
      C  =   362436.*TWOM24
      CD =  7654321.*TWOM24
      CM = 16777213.*TWOM24
      I97 = 97
      J97 = 33
C       Complete initialization by skipping
C            (NTOT2*MODCNS + NTOT) random numbers
      DO 45 LOOP2= 1, NTOT2+1
      NOW = MODCNS
      IF (LOOP2 .EQ. NTOT2+1)  NOW=NTOT
      IF (NOW .GT. 0)  THEN
        WRITE(6,'(A,I15)') ' MARINI SKIPPING OVER ',NOW
       DO 40 IDUM = 1, NTOT
       UNI = U(I97)-U(J97)
       IF (UNI .LT. 0.)  UNI=UNI+1.
       U(I97) = UNI
       I97 = I97-1
       IF (I97 .EQ. 0)  I97=97
       J97 = J97-1
       IF (J97 .EQ. 0)  J97=97
       C = C - CD
       IF (C .LT. 0.)  C=C+CM
   40  CONTINUE
      ENDIF
   45 CONTINUE
      IF (KALLED .EQ. 1)  RETURN
C
C          Normal entry to generate LENV random numbers
   50 CONTINUE
      DO 100 IVEC= 1, LENV
      UNI = U(I97)-U(J97)
      IF (UNI .LT. 0.)  UNI=UNI+1.
      U(I97) = UNI
      I97 = I97-1
      IF (I97 .EQ. 0)  I97=97
      J97 = J97-1
      IF (J97 .EQ. 0)  J97=97
      C = C - CD
      IF (C .LT. 0.)  C=C+CM
      UNI = UNI-C
      IF (UNI .LT. 0.) UNI=UNI+1.
      RVEC(IVEC) = UNI
C             Replace exact zeros by uniform distr. *2**-24
         IF (UNI .EQ. 0.)  THEN
         ZUNI = TWOM24*U(2)
C             An exact zero here is very unlikely, but let's be safe.
         IF (ZUNI .EQ. 0.) ZUNI= TWOM24*TWOM24
         RVEC(IVEC) = ZUNI
         ENDIF
  100 CONTINUE
      NTOT = NTOT + LENV
         IF (NTOT .GE. MODCNS)  THEN
         NTOT2 = NTOT2 + 1
         NTOT = NTOT - MODCNS
         ENDIF
      RETURN
C           Entry to output current status
      ENTRY MAROUT(IJKLUT,NTOTUT,NTOT2T)
      IJKLUT = IJKL
      NTOTUT = NTOT
      NTOT2T = NTOT2
      RETURN
      END
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
CBBL         CALL MARRAN(RVEC,LEN)
          call RANMAR(RVEC,LEN)
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
      CHARACTER*(*) WORD                       
      REAL*8 PP(4)                           
      AMS=PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2  
      IF(AMS.GT.0.0) AMS=SQRT(AMS)           
      WRITE(NUNIT,'(A4,4(1X,F15.11),1X,F10.8)') WORD,(PP(I),I=1,4),AMS 
C====================================================================== 
C================END OF YFSLIB========================================= 
C====================================================================== 
      END 
      SUBROUTINE BHLOG4(MODE,XPAR,NPAR)
C     *********************************  
C=================================================================== 
C===================================================================
C===BBBBBB=====BBB==BBB===BBB=======BBB=======BBBBBB=====BBBBBBB====
C===BBB==BB====BBB==BBB===BBB=======BBB======BBBBBBBB===BBBBBBBBB===
C===BBB==BB====BBB==BBB===BBB=======BBB======BBB==BBB===BBB=========
C===BBBBBB=====BBBBBBBB===BBB=======BBB======BBB==BBB===BBB=========
C===BBBBBBBB===BBBBBBBB===BBB=======BBB======BBB==BBB===BBB==BBBB===
C===BBB==BBB===BBB==BBB===BBBBBBB===BBBBBBB==BBB==BBB===BBB===BBB===
C===BBBBBBBB===BBB==BBB===BBBBBBB===BBBBBBB===BBBBBB=====BBBBBB=====
C===================================================================
C===================================================================
C
****************************************************
c This is simple LL generator with one-line emission
c Its purpose is to test analytical v-integration in O(alf4)prag.
c see BOKER4 and VVRHO routines in SUPERFIG
C     ***********************************  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      DIMENSION XPAR(*),NPAR(*)
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
      COMMON / INOUT  / NINP,NOUT
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPAR2 / CMSENE,AMEL  
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS 
C Communicates with VARRAN
      COMMON / RANPAR / KEYRND
      COMMON / BHPCOL / PVI,PVF,QVI,QVF
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      SAVE / INOUT  /,/ TRANSR /,/ BHPAR3 /,/ BHPAR2 /,/ BHPAR1 /
      SAVE / RANPAR /,/ BHPCOL /,/ WGTALL /
C
      SAVE NEVGEN
C...
      IF(MODE.EQ.-1) THEN  
*     *******************
      KEYOPT = NPAR(1)
      KEYRAD = NPAR(2)
      CMSENE = XPAR(1)
      TRMIN  = XPAR(2)
      TRMAX  = XPAR(3)
      EPSCM  = XPAR(4)
      KEYRND = MOD(KEYOPT,10)   
C--
      AMEL  =  0.5111D-3
      TRAN  = (TRMIN+TRMAX)/2

      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHLOG4: INPUT PARAMETRES       '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL1I) KEYOPT,     ' options   switch  ','KEYOPT','N1'
      WRITE(NOUT,BXL1I) KEYRND,     ' VARRAN    switch  ','KEYRND','  '
      WRITE(NOUT,BXL1I) KEYRAD,     ' radiation switch  ','KEYRAD','N2'
      WRITE(NOUT,BXL1F) CMSENE,     ' CMS enegry  [GeV] ','CMSENE','X1'
      WRITE(NOUT,BXL1G) TRMIN ,     ' trasf_min [GeV^2] ','TRMIN ','X2'
      WRITE(NOUT,BXL1G) TRMAX ,     ' trasf_min [GeV^2] ','TRMAX ','X3'
      WRITE(NOUT,BXL1G) TRAN  ,     ' trasfer   [GeV^2] ','TRMAS ','X3'
      WRITE(NOUT,BXL1F) EPSCM ,     ' eps_CM infr. cut  ','EPSCM ','X4'
      WRITE(NOUT,BXCLO)  

      NEVGEN=0

      ELSEIF(MODE.EQ.0) THEN
*     **********************
      NEVGEN=NEVGEN+1 

      KEYGEN = MOD(KEYOPT,10000)/1000

      CALL GENIEK(TRAN,VI,VF,VV,WTBASE,WTPDEL,WTCONV,WTCON2,WTCON3,WTD)
      WTSET(10)=WTBASE
      WTSET(12)=WTCONV
      WTSET(13)=WTCON2
      WTSET(14)=WTCON3
      WTSET(15)=WTPDEL
      WTSET(16)=WTD
      PVI=VI
      PVF=VF
      QVI=0
      QFV=0

      ELSEIF(MODE.EQ.1.OR.MODE.EQ.2) THEN 
*     ***********************************
      NPAR(10)= NEVGEN
      NPAR(20)= NEVGEN
c x-section equas one
      XPAR(10)= 1D0
      XPAR(11)= 0D0    
C for WEIGHTED events
      XPAR(20)= 1D0
      XPAR(21)= 0D0    

      ENDIF
      END

      SUBROUTINE GENIEK(TRAN,VI,VF,VV,
     $                 WTBASE,WTPDEL,WTCONV,WTCON2,WTCON3,WTDIF)
*     **********************************************************
c This is simple LL generator with two-line emission
c Its purpose is to test analytical v-integration in O(alf4)prag.
c see BOKER4 and VVRHO routines in SUPERFIG
*     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(PI     = 3.1415926535897932D0, ALFA=1D0/137.03604D0) 
      PARAMETER(ALF1   = ALFA/PI)  
      PARAMETER(AMEL   = 0.5111D-3)
      PARAMETER(CEULER = 0.57721566D0)       
      COMMON / INOUT  / NINP,NOUT     
      dimension drvec(3)
      save
      data icont /0/
      icont = icont+1
C=====================================================
      BILG   = DLOG(TRAN/AMEL**2)
      gami   =   ALF1*(BILG-1D0) 
      GAMFAC = EXP(-CEULER*gami)/DPGAMM(1D0+gami)
      call varran(drvec,2)
      r1 = drvec(1)
      r2 = drvec(2)
c v-variables
c by hand regularization to avoid r1=0.d0
      r1 = max(r1, 1d-10)
      r2 = max(r2, 1d-10)
      vi = r1**(1d0/gami)    
      vf = r2**(1d0/gami)    
      vv = vi+vf
c various weights
      WTBASE = gamfac**2
c
      WTPDEL = 0
      WTCON2 = 0
      DISBAS= 0
      WTCONV= 0
      IF(VV.LT. 0.9999999) then
        WTPDEL = WTBASE * EXP(-2*gami*LOG(1-VI))
        DISBAS=  gamfac*gami*vi**(gami-1)
     $          *gamfac*gami*vf**(gami-1)
        TRANP = TRAN*(1D0-VI)**2/(1D0-VV)
        GAMIP = ALF1*(DLOG(TRANP/AMEL**2) -1D0) 
        GAMFP = EXP(-CEULER*gamip)/DPGAMM(1D0+gamip)
        DISTP =  gamfp**2*gamip**2
     $           *vi**(gamip-1)*vf**(gamip-1)
        WTCONV= WTBASE *DISTP/DISBAS 

        WTCON2= WTBASE *DISTP/DISBAS *EXP(-2*gamip*LOG(1-VI))

*! WTCON3 is intermediate step WTBASE--> WTCONV
*! effective powers in vi**(gamip-1)*vf**(gamip-1)
*! excelent approximation ( below 0.2E-4)
        d  =  ALF1*LOG(1D0-VV)
        DISTZ =  gamfac**2 *gamip**2
     $          *vi**(gami-1-d) *vf**(gami-1+d)
        WTCON3= WTBASE *DISTZ/DISBAS

*------------------------CONV--------------------------
*! the only approximation is as in CON3 effective powers
c  otherwise pure rearrangements
c        d     =    alf1*log(1-vv)
c        DISTZ =  gamfac**2 *(
c pole part of gamip**2 seems to be below 0.3E-4
c anyway, it is integrable by hand if necessary!
c     $   +(vi/vv*(gami+d)**2+ vf/vv*(gami-d)**2 )
c exact residue part of gamip**2
c     $   +4*alf1*(gami-d)*(log(1-vi)-(vi/vv)*log(1-vv))
c     $   +4*alf1**2*(log(1-vi)**2-(vi/vv)*log(1-vv)**2)
c     $        )*vi**(gami-1-d) *vf**(gami-1+d)
c        WTCON4= WTBASE *DISTZ/DISBAS
c! the version directly integrable by hand
        d     =    alf1*log(1-vv)
        DISTZ =  gamfac**2 *(
c pole part of gamip**2 seems to be below 0.3E-4
c anyway, it is integrable by hand if necessary!
     $ +(vi/vv*(gami+d)**2+ vf/vv*(gami-d)**2 )
     $            *vi**(gami-1-d) *vf**(gami-1+d)
c approximate residue part integrable analytically
     $ +4*alf1*(gami-d)   *vv**(2*gami)
     $    *(log(1-vi)-(vi/vv)*log(1-vv))/(vi*vf)
c new terms beyond 2-nd pragmatic
c     $ +4*alf1*gami**2    *vv**(2*gami)
c     $    *(log(1-vi)-(vi/vv)*log(1-vv))/(vi*vf) *log(vi*vf/vv/vv)   
c     $ +4*alf1**2 
c     $    *(log(1-vi)**2-(vi/vv)*log(1-vv)**2)/(vi*vf)
     $        )
        WTCON4= WTBASE *DISTZ/DISBAS
        WTDIF = WTCON4-WTBASE
cc        WTDIF = WTCONV-WTCON4

*------------------------CON2--------------------------
*! truncated WTCON2 such that disruption is below 0.0001
        d  =  ALF1*LOG(1D0-VV)
cc        DISTU =  gamfac**2 *gamip**2
cc     $   *vi**(gami-1-d)*vf**(gami-1+d)
cc     $   *(1 -2*(gami+d )*LOG(1-VI) +2*gami**2*LOG(1-VI)**2  )
*! now pole approximations, disruption is below 0.0001 !!!
cc        DISTU =  
cc     $  +1
cc     $      *gamfac**2 *vi**(gami-1-d)*vf**(gami-1+d)*gamip**2
cc     $  -2*(gami+d )*( vi/vv*LOG(1-vv)*(gami+d)**2 )
cc     $      *gamfac**2 *vi**(gami-1-d)*vf**(gami-1+d)
cc     $  +2*vi/vv*(gami+d)**2*(gami+d)**2 *LOG(1-vv)**2
cc     $      *gamfac**2 *vi**(gami-1-d)*vf**(gami-1+d)
*! just reordered result
cc      gamip =  gami +alf1*log((1-vi)**2/(1-vv))
        DISTU =  gamfac**2 *(
     $    vi**(gami-1-d)*vf**(gami-1+d)*gamip**2
* new terms with respect to WTCONV below
     $  -2*(gami+d)**3 *LOG(1-vv)/vv
     $      *vi**(gami-d)*vf**(gami-1+d)
     $  +2*(gami+d)**4 *LOG(1-vv)**2/vv
     $      *vi**(gami-d)*vf**(gami-1+d)
     $  )
*! approximations below cost -0.0001 (only for analyt. v-integration)
cc        DISTU =  gamfac**2 *(
cc     $    vi**(gami-1-d)*vf**(gami-1+d)*gamip**2
* new terms with respect to WTCONV below
cc     $  -2*(gami+d)**3 *LOG(1-vv)/vv
cc     $      *vf**(gami-1)
cc     $  +2*(gami)**4 *LOG(1-vv)**2/vv
cc     $      *vf**(gami-1)
cc     $  )
        WTCON5= WTBASE *DISTU/DISBAS
cc        WTDIF = WTCON5 -WTCON2
      ENDIF
      END     
      SUBROUTINE BHLOG5(MODE,XPAR,NPAR)
C     *********************************  
c This is simple LL generator for 2-line emission.
c It implements NEW BASELINE distribution and beta0
C     ***********************************  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      DIMENSION XPAR(*),NPAR(*)
      CHARACTER*80   BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G 
      PARAMETER (PI     = 3.1415926535897932D0, ALFA=1D0/137.03604D0) 
      PARAMETER (ALF1   = ALFA/PI)  
      PARAMETER (CEULER = 0.57721566D0)       
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )   
      COMMON / INOUT  / NINP,NOUT
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPAR2 / CMSENE,AMEL  
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS 
C Communicates with VARRAN
      COMMON / RANPAR / KEYRND
      COMMON / BHPCOL / PVI,PVF,QVI,QVF
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      SAVE / INOUT  /,/ TRANSR /,/ BHPAR3 /,/ BHPAR2 /,/ BHPAR1 /
      SAVE / RANPAR /,/ BHPCOL /,/ WGTALL /
C
      SAVE NEVGEN
C...
      IF(MODE.EQ.-1) THEN  
*     *******************
      KEYOPT = NPAR(1)
      KEYRAD = NPAR(2)
      CMSENE = XPAR(1)
      TRMIN  = XPAR(2)
      TRMAX  = XPAR(3)
      EPSCM  = XPAR(4)
      KEYRND = MOD(KEYOPT,10)   
C--
      AMEL  =  0.5111D-3
      TRAN  = (TRMIN+TRMAX)/2
      DEL   = EPSCM

      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHLOG5: INPUT PARAMETRES       '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL1I) KEYOPT,     ' options   switch  ','KEYOPT','N1'
      WRITE(NOUT,BXL1I) KEYRND,     ' VARRAN    switch  ','KEYRND','  '
      WRITE(NOUT,BXL1I) KEYRAD,     ' radiation switch  ','KEYRAD','N2'
      WRITE(NOUT,BXL1F) CMSENE,     ' CMS enegry  [GeV] ','CMSENE','X1'
      WRITE(NOUT,BXL1G) TRMIN ,     ' trasf_min [GeV^2] ','TRMIN ','X2'
      WRITE(NOUT,BXL1G) TRMAX ,     ' trasf_min [GeV^2] ','TRMAX ','X3'
      WRITE(NOUT,BXL1G) TRAN  ,     ' trasfer   [GeV^2] ','TRMAS ','X3'
      WRITE(NOUT,BXL1F) EPSCM ,     ' eps_CM infr. cut  ','EPSCM ','X4'
      WRITE(NOUT,BXCLO)  

      NEVGEN=0

      ELSEIF(MODE.EQ.0) THEN
*     **********************
      NEVGEN=NEVGEN+1 

      CALL GENIAK(TRAN,AMEL,DEL,PVi,PVf, wp0,wp1,wp2,wp3)
      CALL GENIAK(TRAN,AMEL,DEL,QVi,QVf, wq0,wq1,wq2,wq3)

c one line case
      WTSET(10) = wp0
      WTSET(11) = Wp1
      WTSET(12) = Wp2
      WTSET(13) = wp3
c two-line case
      WTSET(20) = wp0*wq0
      WTSET(21) = wp1*wq1
      WTSET(22) = wp2*wq2
      WTSET(23) = wp3*wq3

      ELSEIF(MODE.EQ.1.OR.MODE.EQ.2) THEN 
*     ***********************************
      NPAR(10)= NEVGEN
      NPAR(20)= NEVGEN
c x-section equas one
      XPAR(10)= 1D0
      XPAR(11)= 0D0    
C for WEIGHTED events
      XPAR(20)= 1D0
      XPAR(21)= 0D0    
      ENDIF
c     *****
      END

      SUBROUTINE GENIAK(TRAN,AMEL,DEL,Vi,Vf,wt0,wt1,wt2,wt3)
*     ******************************************************
c This is simple LL generator for 2-line emission.
c It implements NEW BASELINE distribution and beta0
c ----------------  distributions ------------------
c wt0 = EXP(DELB1) trivial constant weight
c wt1 = covolution of two NewBaseline (exact)
c wt2 = covolution of two NewBaseline (approx)
c wt3 = convolution with exp(-gam*log(1-vi)+DELB) YFS formfactor
*     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (PI     = 3.1415926535897932D0, ALFA=1D0/137.03604D0) 
      PARAMETER (ALF1   = ALFA/PI)  
      PARAMETER (NMAX=25)
      COMMON / INOUT  / NINP,NOUT     
      SAVE   / INOUT  /
      DIMENSION RR(100),VVI(100),VVF(100)

      BILG   = DLOG(TRAN/AMEL**2)
      BETI   = 2D0*ALF1*(BILG-1D0) 
      DELB   = 0.25D0*BETI -ALF1*(0.5D0 +1/6D0*PI**2)
      DELB1  = -ALF1*1/6D0*PI**2
      FYFS   = EXP(DELB)
      FYFS0  = EXP(0.5*DELB1)
      FYFS1  = EXP(DELB1)
      GAMH   = BETI/2
      AVERG  = GAMH*LOG(1D0/DEL)
c initial line
 10   CONTINUE
      CALL POISSG(AVERG,NMAX,NPH1,RR) 
      IF(NPH1.GT.NMAX) GOTO 10  
      Vi   =0
      proi =1
      sumi =0
      DO 80 I=1,NPH1  
        V      = DEL**RR(I)
        VVI(i) = V
        Vi     = Vi +V
        proi   = proi*(GAMH - 0.5D0*ALF1*LOG(1D0-V))/gamh
        sumi   = sumi - 0.5D0*ALF1*LOG(1D0-V)
 80   CONTINUE
      wti1 = FYFS0*proi
      wti2 = FYFS0*(1 +sumi/gamh) 
c final line
 110  CONTINUE
      CALL POISSG(AVERG,NMAX,NPH2,RR) 
      IF(NPH2.GT.NMAX) GOTO 110  
      Vf   =0
      prof =1
      sumf =0
      DO 180 I=1,NPH2  
        V      = DEL**RR(I)
        VVF(i) = V
        Vf     = Vf +V
        prof   = prof*(GAMH - 0.5D0*ALF1*LOG(1D0-V))/gamh
        sumf   = sumf - 0.5D0*ALF1*LOG(1D0-V)
 180  CONTINUE
      wtf1 = FYFS0*prof
      wtf2 = FYFS0*(1 +sumf/gamh) 
c-------------------------
c merge two lines
      VSUM  = Vi + Vf
      wt0   = EXP(DELB1)
      wt1   = wti1*wtf1
      wt2   = wti2*wtf2
      wt3=0
      if(vsum.lt.1d0) then
        GAMHb = GAMH +ALF1*LOG((1-vi)**2/(1-vsum))
        dl    =       ALF1*LOG((1-vi)**2/(1-vsum))
        GAMB  = 2*GAMHb
c recalculate weight with gamma-bar
        proi =1
        sumi =0
        DO 281 I=1,NPH1
        sumi = sumi    +dl -ALF1/2*LOG(1D0-VVI(i))
        proi = proi*(GAMHb -ALF1/2*LOG(1D0-VVI(i)))/gamh
 281    CONTINUE
        prof =1
        sumf =0
        DO 282 I=1,NPH2
        sumf = sumf    +dl -ALF1/2*LOG(1D0-VVF(i))
        prof = prof*(GAMHb -ALF1/2*LOG(1D0-VVF(i)))/gamh
 282    CONTINUE
        wt3 = proi*prof
     $    *exp(-2*alf1*log((1-vi)**2/(1-vsum))*log(1/del))
     $    *exp(DELB -gamb*log(1-vi))
      endif
      END

      SUBROUTINE GENIAL(TRAN,AMEL,DEL,VSUM,wt1,wt2)
*     *********************************************
c This is simple LL generator for one-line emission.
c It implements NEW BASELINE distribution for one line
c WT0 is the old trivial baseline F(gam)*gam*v**(gam-1)
c WT1 is NEW beseline (NBAS) exact
c WT2 is NEW beseline (NBAS) truncated to O(alf3)prag
*     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (PI     = 3.1415926535897932D0, ALFA=1D0/137.03604D0) 
      PARAMETER (ALF1   = ALFA/PI)  
      PARAMETER (NMAX=25)
      COMMON / INOUT  / NINP,NOUT     
      SAVE   / INOUT  /
      DIMENSION RR(100)

      GAMH   = ALF1*(LOG(TRAN/AMEL**2)-1)
      AVERG  = GAMH*LOG(1D0/DEL)
 10   CONTINUE
      CALL POISSG(AVERG,NMAX,NPH,RR) 
      IF(NPH.GT.NMAX) GOTO 10  
      VSUM=0D0
      prod1=1
      sum1 =0
      DO 80 I=1,NPH  
        V  = DEL**RR(I)
        VSUM = VSUM +V
        prod1= prod1*(GAMH - 0.5D0*ALF1*LOG(1D0-V))
        sum1 = sum1 - 0.5D0*ALF1*LOG(1D0-V)
 80   CONTINUE
      wt1 =  prod1/gamh**nph
      wt2 =  (1 + sum1/gamh) 
      END

      SUBROUTINE GENIUL(TRAN,DEL,VSUM, wt0,wt1,wt2)
*     *********************************************
c This is simple LL generator with one-line emission.
c It implements NEW BASELINE distribution for one line
*     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (PI     = 3.1415926535897932D0, ALFA=1D0/137.03604D0) 
      PARAMETER (ALF1   = ALFA/PI)  
      PARAMETER (CEULER = 0.57721566D0)       
      PARAMETER (AMEL   = 0.5111D-3)
      PARAMETER (NMAX=25)
      COMMON / INOUT  / NINP,NOUT     
      SAVE   / INOUT  /
      DIMENSION RR(100)

ccc      GAMH   = ALF1*LOG(TRAN/AMEL**2)
      GAMH   = 2*ALF1*LOG(TRAN/AMEL**2)
      AVERG  = GAMH*LOG(1D0/DEL)
 10   CONTINUE
      CALL POISSG(AVERG,NMAX,NPH,RR) 
      IF(NPH.GT.NMAX) GOTO 10  
      VSUM=0D0
      prod1=1
      sum1 =0
      DO 80 I=1,NPH  
        V  = DEL**RR(I)
        VSUM = VSUM +V
        prod1= prod1*(GAMH - ALF1*LOG(1D0-V))
        sum1 = sum1 - ALF1*LOG(1D0-V)
 80   CONTINUE
      wt1 =  prod1/gamh**nph
      wt2 =  (1 + sum1/gamh) 

      wt0 =       EXP(-1/6D0*ALF1*PI**2)
      wt1 =  wt1 *EXP(-1/6D0*ALF1*PI**2)
      wt2 =  wt2 *EXP(-1/6D0*ALF1*PI**2)
      END
