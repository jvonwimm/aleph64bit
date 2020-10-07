*************************************************************
*
*        ######################################
*       #     The EXCALIBUR eventgenerator     #
*        ######################################
*
* A four fermion generator for
*
*         e+ e-  --->  f1 f2  f3 f4
*
* based on a program written by 
*
*   F.A. Berends, R. Kleiss, R. Pittea and A.I. van Sighem.
*
* References:
* -----------
*   F.A.Berends, R.Kleiss, R.Pittau   Nucl.Phys.B424:308-342,1994 
*   R.Kleiss, R.Pittau                Comput.Phys.Commun.83:141-146,1994
*   F.A.Berends, R.Kleiss, R.Pittau   Nucl.Phys.B426:344-354,1994 
*   F.A.Berends, R.Kleiss, R.Pittau   INLO-PUB-7/94 
*   R.Pittau                          Phys. Lett. B335 (1994) 490-493
*   F.A.Berends, R.Kleiss, R.Pittau   Comput.Phys.Commun.85:437-452,1995
*   F.A.Berends, A.I.van Sighem       INLO-PUB-7/95
*   J.Hilgart, R.Kleiss, F.L.Diberder Comput.Phys.Commun.75:191-218,1993
*
* Main features of the generator:
* -------------------------------
*     - ISR collinear with the beams,
*     - Anomalous couplings,
*     - Coulomb singularity for WW-diagrams,
*     - Mass-dependent width in the boson propagator
*     - QCD corrections to boson widths (Z/W) (Naive: As in LEP2 report)
*     - Massive final state fermions in the phase space generation
*     - FSR for prompt electrons and muons using PHOTOS or FERMISV procedure.
*     - Parton showers and fragmentation using JETSET 7.4
*     - Tau decays using TAUOLA (interfaced to JETSET)
*
*     - Test option: QQgg final states (from R.Pittau) only string
*                    fragmentation -- NO partonshower.... (FLAG: IEXGG)
*
* History:
* --------
*  Feb-Aug 1995   D.Charlton    Original code by Berends et al. Additional bug-
*                 F.Berends     fixes and introduction of anomalous couplings.
*                 G.Bella       First port of the EXCALIBUR Monte Carlo into
*                 F.A.Berends   an eventgenerator by D.Charlton. Naming scheme
*                 R.Kleiss      EXC*** was introduced by D.Charlton and some
*                 R.Pittau      original names were changed to avoid conflicts:
*                                       RAN    -> EXCRAN
*                                       CUTS   -> EXCUTS
*                                       MATRIX -> EXCMTX
*                                       LABEL  -> EXCLAB
*                               Interface to HEPEVT common by the routines
*                               EXCHEP and EXCFLP. First version of EXCINI,
*                               EXCEVT, EXCRUN + several other routines.
*
*
*  Feb-May 1996    J.B.Hansen   Extensive (total) rewriting of EXCINI, EXCRUN
*                  A.Waananen   and EXCEVT. Introduction of EXCDEF, EXCEND,
*                               EXFRAG and COULMB (from LPWW02). Removal of
*                               several overlapping routines. Present version
*                               has been optimized at the cost of the preser-
*                               vation of the original code. Some changes to
*                               some original routines in order to introduce
*                               massive fermions, mixing, coulomb-singularity
*                               (WW-diagrams), QCD effects and polarized tau
*                               decays.
*                               Original weight self-optimization scheme is now 
*                               used in both the initial integration and the
*                               event generation. Channel-by-channel event
*                               optimization in accordance to the relative
*                               cross-sections is used in the initial inte-
*                               gration.
*  Nov 1996                     Change original name MOMSET -> EXCMOM to avoid
*                               conflicts.
*
* Main routines introduced in addition to the original code:
* -----------------------------------------------------
*    EXCDEF: Set default values for run-time options.
*    EXEINI: EXCALIBUR electroweak generator initialisation. Initial
*            scan over final states.
*    EXCEVT: Obtain one event using the Rejection algorithm.
*    EXCRUN: Main event generation/integration routine.
*    EXCEND: Termination routine for EXCALIBUR - reports on stats..
*    EXCFSR: Interface routine to FSR generation.
*    EXFRAG: Fill /LUJETS/ and call LUND for showering and fragmentation.
*    DCOULM: This function calculates an approximate value for the
*            correction to W+W- production due to the Coulomb singularity.
*    EXCHEP: Conversion to HEPEVT format.
*    EXCFLP: Swap two HEPEVT entries.
*    EXCFID: Final-state decoding into string.
*    EXCDFS: Decode mixed final state into unique EXCALIBUR final state.
*    EXCDCH: Decode phase space channel.
*    SJOST1:\
*    SJOST2: \ Color-reconnection routines tranlated from original
*    SJOST3: / Sjostrand routines.
*    PYWWXX:/
*
* Finally:  The organization of the program has been inspired by the version 
* --------  provided by D.Charlton and by the LPWW02 event generator.
*
* Note:  Code in _small letters_ is the original code by F.A.Berends et al.
* -----
*************************************************************
      SUBROUTINE EXCDEF
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Feb-96
*  CALLED    : (user)
*  CALLS     : (none)
*
*------------------------------------------
      IMPLICIT NONE
* Original common
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
* Additional commons...
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
* Local variables
      INTEGER I,J
*
* Unit number for output messages
*
      LMCWRT=6
*
* Processing options:
*           Contributing diagrams (IEXDIA=Kleiss def.)
*           Debug printout (in org. EXCALIBUR code) (IEXDBG=1)
*           Unweighted events (IEXUWT=1)
*           ISR (IEXISR=1)
*           FSR (IEXFSR=1 (PHOTOS) or IEXFSR=2 (FERMISV))
*           Hadronization (IEXHAD=1)
*           Coulomb singularity (IEXCOU=1)
*           CKM-mixing (IEXMIX=1)
*           QCD corrections to boson widths (=1 (W) or =2 (W+Z))
*           Specified boson widths are EL-WEAK widths (IEXEWD=1)
*           Use TAUOLA for tau-decays (MSTJ(28)=2)
*
* IEXDIA  = 0  all diagrams
*         = 1  WW diagram
*         = 2  ZZ diagrams
*         = 3  W+ e- nue_bar production
*         = 4  Z e+ e- production
*         = 5  Z nue nue_bar production
*
*  WARNING: in order to avoid double counting, final states
*           with different particles should be used if
*           IEXDIA is not 0.
*
      IEXDIA=0
      IEXDBG=0
      IEXUWT=1
      IEXISR=1
      IEXFSR=1
      IEXHAD=1
      IEXCOU=1
      IEXMIX=1
      IEXQCD=1
      IEXEWD=0
* Include QQgg? ---- Test option only for cross-section calculations...
      IEXGG=0
* Max number of FSR photons from FINALG (FERMISV FSR generator)
      IEXNNG=4
* 4 fermion pairing option (0,...,6)
      IEXPAF=4
* Color reconnection option (0,...,9)
      IEXCRC=0
* Q-dependent width in boson-propagators
      IEXQDW=1
* Number of hadronized events to list
      IEXNDB=5
* Default Sjostrand values for string reconnection (IEXCRC>=4)
      TFRAG=1.5D0
      RHAD=0.5D0
      RPROB=0.6D0
* Special Color Reconnection input/output option
      IEXFCR = 0
      IEXUCR = 91
*
* Number of events per process in initial integration
*
      IEXINT=1000
*
* Number of events to generate
*
      NEVT=2000
*
* Centre-of-mass energy (GeV)
*     
      ECM=176.0D0
*     
* Primary vertex smearing
*  this belongs to outside world
C      SDVRT(1) = 0.0124
C      SDVRT(2) = 0.0005
C      SDVRT(3) = 0.72
       do i = 1,4
          vertex(i) = 0.
       enddo
*
* Set values for SM parameters
*
      ZMI=91.1888D0
      WZI=2.4974D0
      WMI=80.25D0
      WWI=-1.0D0
      GMU=1.16639D-5
* STH2=0 Forces use of value consistent with EXCALIBUR-scheme
      STH2=0.0D0
      ALPHAR=1.0D0/137.0359895D0
      ALPHA=1.0D0/128.07D0
      ALS=0.110D0
* ALPHAS at Mz for use in boson width corrections...
      EXALSZ=0.120
*
* Anomalous couplings
*
* Couplings 2,3,4,5,6 and 8 can be changed
* 2 -> delta_z
* 3 -> x_gamma
* 4 -> x_Z          (Bilenky et al. notation)
* 5 -> y_gamma
* 6 -> y_Z
* 8 -> z_Z
*
      DO 10 I=1,14
        DANOMC(I)=0.0D0
 10   CONTINUE
*
* Specify desired final states
*
* IPROC(1-86/91/146/143): 
*            Specify final states (PDG numbering scheme(positive))
*            If IPROC(1)=0 all states are selected
*            If IPROC(..)=-1 all leptonic states are added
*            If IPROC(..)=-2 all semi-leptonic states are added
*            If IPROC(..)=-3 all hadronic states are added
*            If IPROC(..)=-4 all qqgg states are added if IEXGG=1
*            Otherwise set IPROC(I)=8-digit code for f1 f2~ f3 f4~, eg.
*            11120201 for e- anti-nu_e u anti-d.
*            Wildcards for fermion flavours may specified by putting
*                00 match all fermion flavours, ex. 11120001
*                09 match all quark flavours, ex. 11120901
*                19 match all lepton flavours, ex. 11121919
*
      IPROC(1)=0
*
* Initialize cuts -- several types...
*
* CUTS(1) -> SHCUT - invariant mass of colliding e+e- after ISR
      CUTS(1,1)=0.0D0
* CUTS(2-5) -> ECUT(3-6) (Kleiss) - energy cuts on four outgoing
*                                   fermions
      CUTS(2,1)=0.0D0
      CUTS(3,1)=0.0D0
      CUTS(4,1)=0.0D0
      CUTS(5,1)=0.0D0
* CUTS(6-11) -> SCUT(34,35,36,45,46,56) (Kleiss) 
*      - cuts on min inv. mass squared of pairs of outgoing fermions
      CUTS(6,1)=0.0D0
      CUTS(7,1)=0.0D0
      CUTS(8,1)=0.0D0
      CUTS(9,1)=0.0D0
      CUTS(10,1)=0.0D0
      CUTS(11,1)=0.0D0
* CUTS(12-25) -> CMAX(13,14,15,16,23,24,25,26,34,35,36,45,46,56) (Kleiss)
*      - cuts on cos-theta between incoming(1,2) and outgoing(4-6)
*        fermions, and on cos-thetas between outgoing fermion pairs
      CUTS(12,1)=1.0D0
      CUTS(13,1)=1.0D0
      CUTS(14,1)=1.0D0
      CUTS(15,1)=1.0D0
      CUTS(16,1)=1.0D0
      CUTS(17,1)=1.0D0
      CUTS(18,1)=1.0D0
      CUTS(19,1)=1.0D0
      CUTS(20,1)=1.0D0
      CUTS(21,1)=1.0D0
      CUTS(22,1)=1.0D0
      CUTS(23,1)=1.0D0
      CUTS(24,1)=1.0D0
      CUTS(25,1)=1.0D0
      DO 30 I=2,153
        IEXCUT(I)=0
        IEXUWP(I-1)=0
        EXUWEI(I-1)=0.0D0
        EXUMWT(I-1)=0.0D0
        DO 20 J=1,25
          CUTS(J,I)=CUTS(J,1)
 20     CONTINUE
 30   CONTINUE
* Switch on intelligent minimum cuts
      IEXCUT(1)=-5
      CUTS(1,1)=0.999
*
 999  RETURN
      END
*
      SUBROUTINE EXCINI(ISTAT)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Feb-96   (from older routine)
*  CALLED    : (user)
*  CALLS     : EXCRUN SETPRO EXCGAM EXCFID
*
*------------------------------------------
      IMPLICIT NONE
* Original commons/vars/params...
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      common/area11/dz,xg,yg,xz,yz,zz
      double precision dz,xg,yg,xz,yz,zz
      common/printout/nout,monfil,logfil,datfil
      integer nout
      logical monfil,logfil,datfil
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
* Part of LUND common block
C LUDAT1
      INTEGER MSTJ,MSTU
      REAL    PARJ,PARU
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C LUDAT2
      INTEGER KCHG
      REAL    PMAS,PARF,VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
* Additional commons...
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
* Arguments
      INTEGER ISTAT
* Local variables
      LOGICAL LTEMP1,LTEMP2
      INTEGER H,I,J,K,L,INEVT,ITEMP,NUMPRC,ID1,ID2,ID3,ID4,I1,I2
     &  ,I3,I4,NSPEC,INPROC(143),FLFLAG(153),ILEVL,IBEGIN
      DOUBLE PRECISION STH2R,CL1,OMZ,OPZ,EM,EM2,Q2,EXSXSC,SFTYFC,AA(12)
     &  ,VV(12),STH,ACTH,QCD(2)
      DOUBLE PRECISION EXCGAM,DTEMP,DTEMP1,CUTTMP(26,153),EXUWTP(153)
      CHARACTER*2 PAR(3:6)
      INTEGER IFSCOD(151)
      DATA NUMPRC /146/, FLFLAG /153*1/
* Final state codes -- using PDG numbering scheme (only positive numbers)
      DATA IFSCOD
* Leptonic
     &  /11111212,11121413,11121615,12111314,12111516,13131414,15151616
     &  ,13141615,15161413,11111111,11111313,11111515,13131313,15151515
     &  ,13131515,11111414,11111616,12121313,12121515,16161313,14141515
     &  ,12121212,12121414,12121616,14141414,16161616,14141616
* Semi-leptonic
     &  ,11120201,11120403,12110102,12110304,13140201,13140403,14130102
     &  ,14130304,15160201,15160403,16150102,16150304,11110202,11110404
     &  ,11110101,11110303,11110505,13130202,13130404,15150202,15150404
     &  ,13130101,13130303,13130505,15150101,15150303,15150505,12120202
     &  ,12120404,12120101,12120303,12120505,14140202,14140404,16160202
     &  ,16160404,14140101,14140303,14140505,16160101,16160303,16160505
* Hadronic
     &  ,02020101,04040303,02010304,01020403,02020202,04040404,01010101
     &  ,03030303,05050505,02020404,02020303,02020505,04040101,04040505
     &  ,01010303,01010505,03030505
* New mixing processes - first semi-leptonic...
     &  ,11120203,11120205,11120401,11120405,12110302,12110502,12110104
     &  ,12110504,13140203,13140205,13140401,13140405,14130302,14130502
     &  ,14130104,14130504,15160203,15160205,15160401,15160405,16150302
     &  ,16150502,16150104,16150504,12120204,12120402,12120103,12120105
     &  ,12120305,12120301,12120501,12120503
* Hadronic
     &  ,02010302,02010502,02030102,02050102,02050302,02030502,04030104
     &  ,04030504,04010304,04050304,04050104,04010504,02030304,02050304
     &  ,02010104,02010504,02030104,02030504,02050104,02050504,03020403
     &  ,05020403,01020401,01020405,03020401,03020405,05020401,05020405
* QQgg
     &  ,02022121,04042121,01012121,03032121,05052121/
*
      ILEVL=ISTAT
      ISTAT=9
      MSTU(11)=LMCWRT
*
* Banner
*     
      WRITE(LMCWRT,1010)
*
* Report constants
*
      ROOTS=ECM
      WRITE(LMCWRT,1020) ECM
      WRITE(LMCWRT,1030) NEVT
      IF (IEXNDB.GT.0) WRITE(LMCWRT,1031) IEXNDB
*
* Consistency check of Standard model parameters needed here
*
      IF (STH2.LE.0.0D0) THEN
        STH2=PI*ALPHA/(SQRT(2.0D0)*WMI**2*GMU)
      ELSE
        STH2R=PI*ALPHA/(SQRT(2.0D0)*WMI**2*GMU)
        IF (ABS(STH2-STH2R).GT.0.0001D0) THEN
          WRITE(LMCWRT,1070) STH2,STH2R
        ENDIF
      ENDIF
      IF (WWI.LE.0.0D0) THEN
        WWI=3.0D0*GMU*WMI**3/SQRT(8.0D0)/PI
        IEXEWD=1
      ENDIF
      WRITE(LMCWRT,1040) GMU,STH2,ZMI,WZI,WMI,WWI,1.0D0/ALPHA,ALS
*
*  Anomalous couplings
*
      DZ=DANOMC(2)
      XG=DANOMC(3)
      YG=DANOMC(5)
      XZ=DANOMC(4)
      YZ=DANOMC(6)
      ZZ=DANOMC(8)
      DO 10 I=1,14
        IF (DANOMC(I).NE.0.0D0) THEN
          WRITE(LMCWRT,1050) DANOMC(2),DANOMC(3),DANOMC(5),DANOMC(4)
     &      ,DANOMC(6),DANOMC(8)
          GOTO 20
        ENDIF
 10   CONTINUE
      WRITE(LMCWRT,1060)
 20   CONTINUE
*
* Report processing options..
*
      WRITE(LMCWRT,1080) (IEXDBG.GT.0),(IEXUWT.GT.0),(IEXISR.GT.0)
     &  ,(IEXFSR.GT.0),(IEXHAD.GT.0),(IEXCOU.GT.0),(IEXMIX.GT.0)
     &  ,(MSTJ(28).EQ.2)
      IF(IEXDIA.EQ.0) WRITE(LMCWRT,1090)
      IF(IEXDIA.EQ.1) WRITE(LMCWRT,1100)
      IF(IEXDIA.EQ.2) WRITE(LMCWRT,1110)
      IF(IEXDIA.EQ.3) WRITE(LMCWRT,1120)
      IF(IEXDIA.EQ.4) WRITE(LMCWRT,1130)
      IF(IEXDIA.EQ.5) WRITE(LMCWRT,1140)
*
* Report on CKM-mixing...
*
      IF (IEXMIX.GT.0) THEN
        WRITE(LMCWRT,1145) ((VCKM(I,J),J=1,3),I=1,3)
      ELSE
        NUMPRC=86
      ENDIF
*
* Report on qqgg states
*  ---- Test option only for cross-section calculations......
      IF (IEXGG.GT.0) THEN
        NUMPRC=NUMPRC+5
        IF (IEXMIX.LE.0) THEN
          DO I=1,5
            IFSCOD(I+86)=IFSCOD(146+I)
          ENDDO
        ENDIF
        WRITE(LMCWRT,1091)
      ELSE
c        WRITE(LMCWRT,1092)
      ENDIF
*
* Report on Q-dependent width option...
*
      IF (IEXQDW.LE.0) THEN
        WRITE(LMCWRT,1147)
      ELSE
        WRITE(LMCWRT,1148)
      ENDIF
*
* Report on FSR option...
*
      IF (IEXFSR.EQ.1) WRITE(LMCWRT,1141)
      IF (IEXFSR.EQ.1.AND.IEXDIA.NE.1.AND.IEXDIA.NE.2) WRITE(LMCWRT,1142
     &  )
      IF (IEXFSR.EQ.2) WRITE(LMCWRT,1143) IEXNNG
* FSR in parton-showers follow IEXFSR....
      MSTJ(41)=1
      IF (IEXFSR.GT.0) MSTJ(41)=2
*
* Report Alpha_s corrected boson widths option...
*
      IF (IEXQCD.LE.0) THEN
        WRITE(LMCWRT,1081)
      ELSE
        WRITE(LMCWRT,1082) EXALSZ
        IF (IEXEWD.EQ.0.AND.MOD(IEXQCD,2).EQ.0) THEN
          WRITE(LMCWRT,1083) IEXQCD/2
        ELSEIF (IEXEWD.EQ.0) THEN
          WRITE(LMCWRT,1086) (IEXQCD+1)/2
        ENDIF
        IF (IEXEWD.EQ.1) THEN
          CALL EXQCDC(QCD)
* JBH Update total width of the W -- easier than the Z :-)
          WWI=WWI*(1.0D0+2.0D0*QCD(2)*QCD(2))/3.0D0
          IF (MOD(IEXQCD,2).EQ.0) THEN
            DTEMP1=0.0D0
            STH=SQRT(STH2)
            ACTH=SQRT(1.0D0-STH2)
* JBH Update total width of the Z -- first find partial widths (NO top)
            DO I=1,11
              AA(I)=DBLE(2*MOD(I,2)-1)/4.0D0/ACTH
              IF(MOD(I,2).EQ.1.AND.I.LE.5) DTEMP=STH
              IF(MOD(I,2).EQ.1.AND.I.GT.5) DTEMP=STH/3.0D0
              IF(MOD(I,2).EQ.0.AND.I.LE.6) DTEMP=0.0D0
              IF(MOD(I,2).EQ.0.AND.I.GT.6) DTEMP=-2.0D0*STH/3.0D0
              VV(I)= -AA(I)+STH/ACTH*DTEMP
              DTEMP=1.0D0
              IF (I.GE.7) DTEMP=3.0D0
              DTEMP1=DTEMP1+DTEMP*(VV(I)*VV(I)+AA(I)*AA(I))
            ENDDO
            DTEMP1=WZI/DTEMP1
            WZI=0.0D0
            DO I= 1,11
              IF (I.LE.6) WZI=WZI+DTEMP1*(VV(I)*VV(I)+AA(I)*AA(I))
              IF (I.GE.7) WZI=WZI+DTEMP1*3.0D0*(VV(I)*VV(I)+AA(I)*AA(I))
     &          *(QCD(1)*QCD(1))
            ENDDO
            WRITE(LMCWRT,1084) IEXQCD/2,WWI,WZI
          ELSE
            WRITE(LMCWRT,1087) (IEXQCD+1)/2,WWI
          ENDIF
        ENDIF
      ENDIF
*
* Report on fermion pairing option...
*
      IF (IEXPAF.EQ.0) WRITE(LMCWRT,1151)
      IF (IEXPAF.EQ.1) WRITE(LMCWRT,1152)
      IF (IEXPAF.EQ.2) WRITE(LMCWRT,1153)
      IF (IEXPAF.EQ.3) WRITE(LMCWRT,1154)
      IF (IEXPAF.EQ.4) WRITE(LMCWRT,1155)
      IF (IEXPAF.EQ.5) WRITE(LMCWRT,1156)
      IF (IEXPAF.EQ.6) WRITE(LMCWRT,1157)
      IF (IEXPAF.GE.2) WRITE(LMCWRT,1085)
*
* Report on color reconnection option...
*
      IF (IEXHAD.GT.0) THEN
        IF (IEXCRC.EQ.0) WRITE(LMCWRT,1159)
        IF (IEXCRC.EQ.1) WRITE(LMCWRT,1162)
        IF (IEXCRC.EQ.2) WRITE(LMCWRT,1163)
        IF (IEXCRC.EQ.3) WRITE(LMCWRT,1164)
        IF (IEXCRC.EQ.4) WRITE(LMCWRT,1165)
        IF (IEXCRC.EQ.5) WRITE(LMCWRT,1166)
        IF (IEXCRC.EQ.6) WRITE(LMCWRT,1167)
        IF (IEXCRC.EQ.7) WRITE(LMCWRT,1168)
        IF (IEXCRC.EQ.8) WRITE(LMCWRT,1169)
*
* Report on color reconnection parameters if applicable...
*
        IF (IEXCRC.GE.4) WRITE(LMCWRT,1158) RHAD,TFRAG,RPROB
      ENDIF
* Skip if we continue on a previous run
      IF (ILEVL.EQ.1) GOTO 200
*
* Translate the EXUWEI array into EXCALIBUR-formalism
*
* Count number of specified weights
      NSPEC=0
      IF (IEXUWP(1).NE.0) THEN
        NSPEC=1
        DO 25 I=2,NUMPRC
          IF (IEXUWP(I).EQ.0) GOTO 28
          NSPEC=NSPEC+1
 25     CONTINUE
 28     CONTINUE
      ENDIF
      NSPEC=MAX(NSPEC,0)
      DO 22 I=1,NUMPRC
        EXUWTP(I)=0.0D0
        DO 23 H=1,NSPEC
          ID1=INT(IFSCOD(I)/1000000)
          ID2=MOD(INT(IFSCOD(I)/10000),100)
          ID3=MOD(INT(IFSCOD(I)/100),100)
          ID4=MOD(IFSCOD(I),100)
          IF ((ID1.EQ.ID2.AND.ID3.EQ.ID4.AND.MOD(MAX(ID1,ID3),2).EQ.0
     &      .AND.MAX(ID1,ID3).EQ.MIN(ID1,ID3)+1.AND.((IEXUWP(H).EQ.(ID1
     &      *1000000+ID3*10000+ID4*100+ID2)).OR.(IEXUWP(H).EQ.(ID3
     &      *1000000+ID4*10000+ID1*100+ID2)).OR.(IEXUWP(H).EQ.(ID3
     &      *1000000+ID1*10000+ID2*100+ID4)))).OR.(((ID1.EQ.ID2.AND.ID3
     &      .EQ.ID4).OR.(MOD(MAX(ID1,ID2),2).EQ.0.AND.MAX(ID1,ID2).EQ
     &      .MIN(ID1,ID2)+1.OR.MOD(MAX(ID3,ID4),2).EQ.0.AND.MAX(ID3,ID4)
     &      .EQ.MIN(ID3,ID4)+1)).AND.ID1.NE.ID3.AND.IEXUWP(H).EQ.(ID3
     &      *1000000+ID4*10000+ID1*100+ID2))) EXUWTP(I)=EXUWEI(H)
 23     CONTINUE
 22   CONTINUE
*
* Now translate the cuts array into EXCALIBUR-formalism
*
* Count number of specified cuts
      NSPEC=0
      IF (IEXCUT(1).NE.-5) THEN
        DO 35 I=2,NUMPRC
          IF (IEXCUT(I).EQ.0) GOTO 38
          NSPEC=NSPEC+1
 35     CONTINUE
 38     CONTINUE
      ENDIF
      NSPEC=MAX(NSPEC+1,1)
* Check all wildcard specifications
      DO 36 I=1,NUMPRC
        ID1=INT(IFSCOD(I)/1000000)
        ID2=MOD(INT(IFSCOD(I)/10000),100)
        ID3=MOD(INT(IFSCOD(I)/100),100)
        ID4=MOD(IFSCOD(I),100)
        DO 37 H=1,NSPEC
          I1=INT(IEXCUT(H)/1000000)
          I2=MOD(INT(IEXCUT(H)/10000),100)
          I3=MOD(INT(IEXCUT(H)/100),100)
          I4=MOD(IEXCUT(H),100)
          IF ((ID1.EQ.I1.OR.I1.EQ.0.OR.(I1.EQ.9.AND.ID1.LT.7).OR
     &      .(I1.EQ.19.AND.ID1.GT.10)).AND.(ID2.EQ.I2.OR.I2.EQ.0.OR
     &      .(I2.EQ.9.AND.ID2.LT.7).OR.(I2.EQ.19.AND.ID2.GT.10)).AND
     &      .(ID3.EQ.I3.OR.I3.EQ.0.OR.(I3.EQ.9.AND.ID3.LT.7).OR
     &      .(I3.EQ.19.AND.ID3.GT.10)).AND.(ID4.EQ.I4.OR.I4.EQ.0.OR
     &      .(I4.EQ.9.AND.ID4.LT.7).OR.(I4.EQ.19.AND.ID4.GT.10)).OR
     &      .(ID1.GT.7.AND.ID3.GT.7.AND.IEXCUT(H).EQ.-1).OR.(ID1.GT.7
     &      .AND.ID3.LT.7.AND.IEXCUT(H).EQ.-2).OR.(ID1.LT.7.AND.ID3.LT.7
     &      .AND.IEXCUT(H).EQ.-3).OR.(ID1.LT.7.AND.ID3.EQ.21.AND
     &      .IEXCUT(H).EQ.-4).OR.(ID1.EQ.ID2.AND.ID3.EQ.ID4.AND
     &      .MOD(MAX(ID1,ID3),2).EQ.0.AND.MAX(ID1,ID3).EQ.MIN(ID1,ID3)+1
     &      .AND.((IEXCUT(H).EQ.(ID1*1000000+ID3*10000+ID4*100+ID2)).OR
     &      .(IEXCUT(H).EQ.(ID3*1000000+ID4*10000+ID1*100+ID2)).OR
     &      .(IEXCUT(H).EQ.(ID3*1000000+ID1*10000+ID2*100+ID4)))).OR
     &      .(((ID1.EQ.ID2.AND.ID3.EQ.ID4).OR.(MOD(MAX(ID1,ID2),2).EQ.0
     &      .AND.MAX(ID1,ID2).EQ.MIN(ID1,ID2)+1.OR.MOD(MAX(ID3,ID4),2)
     &      .EQ.0.AND.MAX(ID3,ID4).EQ.MIN(ID3,ID4)+1)).AND.ID1.NE.ID3
     &      .AND.IEXCUT(H).EQ.(ID3*1000000+ID4*10000+ID1*100+ID2))) THEN
            IF (ID1.EQ.ID2.AND.ID3.EQ.ID4.AND.MOD(MAX(ID1,ID3),2).EQ.0
     &        .AND.MAX(ID1,ID3).EQ.MIN(ID1,ID3)+1) THEN
              IF (IEXCUT(H).EQ.(ID1*1000000+ID3*10000+ID4*100+ID2)) THEN
                CALL SWAPCT(H,4,6,CUTS)
              ELSEIF (IEXCUT(H).EQ.(ID3*1000000+ID4*10000+ID1*100+ID2))
     &            THEN
                CALL SWAPCT(H,3,5,CUTS)
                CALL SWAPCT(H,4,6,CUTS)
              ELSEIF (IEXCUT(H).EQ.(ID3*1000000+ID1*10000+ID2*100+ID4))
     &            THEN
                CALL SWAPCT(H,3,5,CUTS)
              ENDIF
            ELSEIF (((ID1.EQ.ID2.AND.ID3.EQ.ID4).OR.(MOD(MAX(ID1,ID2),2)
     &          .EQ.0.AND.MAX(ID1,ID2).EQ.MIN(ID1,ID2)+1.AND.MOD(MAX(ID3
     &          ,ID4),2).EQ.0.AND.MAX(ID3,ID4).EQ.MIN(ID3,ID4)+1)).AND
     &          .ID1.NE.ID3.AND.IEXCUT(H).EQ.(ID3*1000000+ID4*10000+ID1
     &          *100+ID2)) THEN
              CALL SWAPCT(H,3,5,CUTS)
              CALL SWAPCT(H,4,6,CUTS)
            ENDIF
            DO J=1,25
              CUTTMP(J,I)=CUTS(J,H)
            ENDDO
            CUTTMP(26,I)=0
            IF (IEXCUT(H).EQ.0) CUTTMP(26,I)=1
            IF (IEXCUT(H).LT.0.OR.I1.EQ.0.OR.I2.EQ.0.OR.I3.EQ.0.OR
     &        .I4.EQ.0.OR.I1.EQ.9.OR.I2.EQ.9.OR.I3.EQ.9.OR
     &        .I4.EQ.9.OR.I1.EQ.19.OR.I2.EQ.19.OR.I3.EQ.19.OR
     &        .I4.EQ.19) CUTTMP(26,I)=2
          ELSEIF ((IEXCUT(H).EQ.-5.OR.IEXCUT(H).EQ.-6).AND.H.EQ.1) THEN
            DO J=1,25
              CUTTMP(J,I)=0.0D0
              IF (J.GT.11) CUTTMP(J,I)=1.0D0
            ENDDO
            CUTTMP(26,I)=1
            IF (ID1.EQ.11.OR.ID2.EQ.11.OR.ID3.EQ.11.OR.ID4.EQ.11)
     &        CUTTMP(26,I)=3
            IF ((ID1.EQ.13.OR.ID2.EQ.13.OR.ID3.EQ.13.OR.ID4.EQ.13).AND
     &        .IEXCUT(H).EQ.-6) CUTTMP(26,I)=3
            IF (ID1.EQ.11) CUTTMP(16,I)=CUTS(1,H)
            IF (ID2.EQ.11) CUTTMP(13,I)=CUTS(1,H)
            IF (ID3.EQ.11) CUTTMP(18,I)=CUTS(1,H)
            IF (ID4.EQ.11) CUTTMP(15,I)=CUTS(1,H)
            IF (IEXCUT(H).EQ.-6) THEN
              IF (ID1.EQ.13) CUTTMP(16,I)=CUTS(2,H)
              IF (ID2.EQ.13) CUTTMP(13,I)=CUTS(2,H)
              IF (ID3.EQ.13) CUTTMP(18,I)=CUTS(2,H)
              IF (ID4.EQ.13) CUTTMP(15,I)=CUTS(2,H)
              CUTTMP(12,I)=CUTTMP(16,I)
              CUTTMP(17,I)=CUTTMP(13,I)
              CUTTMP(14,I)=CUTTMP(18,I)
              CUTTMP(19,I)=CUTTMP(15,I)
            ENDIF
          ELSEIF (IEXCUT(H).LT.-100.AND.IEXCUT(H).GT.-122) THEN
            IF (ID1.EQ.ABS(IEXCUT(H)+100)) THEN
              CUTTMP(2,I)=CUTS(1,H)
              CUTTMP(16,I)=CUTS(2,H)
              CUTTMP(12,I)=CUTS(3,H)
            ENDIF
            IF (ID2.EQ.ABS(IEXCUT(H)+100)) THEN
              CUTTMP(3,I)=CUTS(1,H)
              CUTTMP(17,I)=CUTS(3,H)
              CUTTMP(13,I)=CUTS(2,H)
            ENDIF
            IF (ID3.EQ.ABS(IEXCUT(H)+100)) THEN
              CUTTMP(4,I)=CUTS(1,H)
              CUTTMP(18,I)=CUTS(2,H)
              CUTTMP(14,I)=CUTS(3,H)
            ENDIF
            IF (ID4.EQ.ABS(IEXCUT(H)+100)) THEN
              CUTTMP(5,I)=CUTS(1,H)
              CUTTMP(19,I)=CUTS(3,H)
              CUTTMP(15,I)=CUTS(2,H)
            ENDIF
            IF (ID1.EQ.ID2.AND.ID1.EQ.ABS(IEXCUT(H)+100)) CUTTMP(6,I)
     &        =CUTS(4,H)
            IF (ID1.EQ.ID4.AND.ID1.EQ.ABS(IEXCUT(H)+100)) CUTTMP(8,I)
     &        =CUTS(4,H)
            IF (ID2.EQ.ID3.AND.ID2.EQ.ABS(IEXCUT(H)+100)) CUTTMP(9,I)
     &        =CUTS(4,H)
            IF (ID3.EQ.ID4.AND.ID3.EQ.ABS(IEXCUT(H)+100)) CUTTMP(11,I)
     &        =CUTS(4,H)
            IF (ID1.EQ.ABS(IEXCUT(H)+100).OR.ID2.EQ.ABS(IEXCUT(H)+100)
     &        .OR.ID3.EQ.ABS(IEXCUT(H)+100).OR.ID4.EQ.ABS(IEXCUT(H)+100)
     &        ) CUTTMP(26,I)=0
          ENDIF
 37     CONTINUE
 36   CONTINUE
 200  CONTINUE
*
* Report on default cuts
*
      IF (IEXCUT(1).EQ.0) THEN
        WRITE(LMCWRT,1150) CUTS(2,1),CUTS(3,1),CUTS(4,1),CUTS(5,1)
        IF (IEXISR.GT.0) WRITE(LMCWRT,1160) CUTS(1,1)
        WRITE(LMCWRT,1170) CUTS(6,1),CUTS(7,1),CUTS(8,1),CUTS(9
     &    ,1),CUTS(10,1),CUTS(11,1)
        WRITE(LMCWRT,1180) CUTS(12,1),CUTS(13,1),CUTS(14,1),CUTS(15
     &    ,1),CUTS(16,1),CUTS(17,1),CUTS(18,1),CUTS(19,1),CUTS(20
     &    ,1),CUTS(21,1),CUTS(22,1),CUTS(23,1),CUTS(24,1)
     &    ,CUTS(25,1)
      ELSEIF (IEXCUT(1).EQ.-5) THEN
        WRITE(LMCWRT,1182) CUTS(1,1)
      ENDIF
*
* Initialize ISR
*
      IF (IEXISR.GT.0) THEN
        EM=0.51099906D-3
        EM2=EM*EM
        Q2=ROOTS*ROOTS
        CL=LOG(Q2/EM2)
        ZETA=ALPHAR/PI*(CL-1.0D0)
        OMZ=1.0D0-ZETA
        OPZ=1.0D0+ZETA
        GAEUL=EXCGAM(OPZ)
c-------------------------------------------------------------------c
c                                                                   c
c    cl1 is the logarithm used for the Monte-Carlo mapping, that,   c
c    in principle, may differ from cl used in the computation of    c
c    the flux functions. In order to preserve numerical stability,  c
c    if you want to change scale in the flux function, you should   c
c    choose cl1.le.cl for the actual generation of x1r and x2r.     c
c                                                                   c
c-------------------------------------------------------------------c
        CL1=CL
        ZETA1=ALPHAR/PI*(CL1-1.0D0)
        OMZ1=1.0D0-ZETA1
      ENDIF
*
* Initially kill output from original code!
*
      NOUT=0
      MONFIL=.FALSE.
      LOGFIL=.FALSE.
      DATFIL=.FALSE.
      IF (IEXDBG.GT.0) NOUT=LMCWRT
      IF (ILEVL.EQ.1) GOTO 210
*
*  Safety factor for weights
*
      SAFETY=1.2D0
*
* Initalize event generation and determine relative cross-sections of
* the requested channels
*
      INEVT=IEXINT
      SFTYFC=1.5D0
      NCONP=0
      NNCONP=0
      INFAIL=5000
      EXCFAC=1.05D0
* Count number of specified final states
      NSPEC=0
      DO 49 I=2,NUMPRC
        IF (IPROC(I).EQ.0) GOTO 39
        NSPEC=NSPEC+1
 49   CONTINUE
 39   CONTINUE
      NSPEC=MAX(NSPEC+1,1)
* Expand all wildcard specifications
      J=NSPEC
      DO 50 I=1,NSPEC
        ID1=INT(IPROC(I)/1000000)
        ID2=MOD(INT(IPROC(I)/10000),100)
        ID3=MOD(INT(IPROC(I)/100),100)
        ID4=MOD(IPROC(I),100)
        K=0
        IF (ID1.EQ.0.OR.ID2.EQ.0.OR.ID3.EQ.0.OR.ID4.EQ.0.OR.ID1.EQ.9.OR
     &    .ID2.EQ.9.OR.ID3.EQ.9.OR.ID4.EQ.9.OR.ID1.EQ.19.OR.ID2.EQ.19.OR
     &    .ID3.EQ.19.OR.ID4.EQ.19.OR.IPROC(I).EQ.-1.OR.IPROC(I
     &    ).EQ.-2.OR.IPROC(I).EQ.-3.OR.IPROC(I).EQ.-4) THEN
          L=IPROC(I)
          DO 52 H=1,NUMPRC
            I1=INT(IFSCOD(H)/1000000)
            I2=MOD(INT(IFSCOD(H)/10000),100)
            I3=MOD(INT(IFSCOD(H)/100),100)
            I4=MOD(IFSCOD(H),100)
            IF ((I1.EQ.ID1.OR.ID1.EQ.0.OR.(ID1.EQ.9.AND.I1.LT.7).OR
     &        .(ID1.EQ.19.AND.I1.GT.10)).AND.(I2.EQ.ID2.OR.ID2.EQ.0.OR
     &        .(ID2.EQ.9.AND.I2.LT.7).OR.(ID2.EQ.19.AND.I2.GT.10)).AND
     &        .(I3.EQ.ID3.OR.ID3.EQ.0.OR.(ID3.EQ.9.AND.I3.LT.7).OR
     &        .(ID3.EQ.19.AND.I3.GT.10)).AND.(I4.EQ.ID4.OR.ID4.EQ.0.OR
     &        .(ID4.EQ.9.AND.I4.LT.7).OR.(ID4.EQ.19.AND.I4.GT.10)).OR
     &        .(I1.GT.7.AND.I3.GT.7.AND.L.EQ.-1).OR.(I1.GT.7.AND.I3.LT.7
     &        .AND.L.EQ.-2).OR.(I1.LT.7.AND.I3.LT.7.AND.L.EQ.-3).OR.(I1
     &        .LT.7.AND.I3.EQ.21.AND.L.EQ.-4)) THEN
              IF (K.EQ.0) THEN
                IPROC(I)=IFSCOD(H)
                K=1
              ELSE
                J=J+1
                IPROC(J)=IFSCOD(H)
              ENDIF
            ENDIF
 52       CONTINUE
        ENDIF
 50   CONTINUE
*
* Translate specified channels into EXCALIBUR codes and remove redundant
* codes 
*
      NSPEC=J
      J=0
      DO 55 I=1,NSPEC
        IF (IPROC(I).LE.0) GOTO 56
        IPROC(I-J)=IPROC(I)
        DO 57 H=1,NUMPRC
          IF (IPROC(I).EQ.IFSCOD(H)) THEN
            CALL EXSCUT(I-J,H,CUTTMP,SHCUT,ECUT,SCUT,CMAX,OMCMAX,FLFLAG)
            EXUMWT(I-J)=EXUWTP(H)
          ENDIF
          IF (IPROC(I).EQ.IFSCOD(H)) GOTO 59
          IF (IPROC(I).NE.IFSCOD(H)) THEN
            ID1=INT(IFSCOD(H)/1000000)
            ID2=MOD(INT(IFSCOD(H)/10000),100)
            ID3=MOD(INT(IFSCOD(H)/100),100)
            ID4=MOD(IFSCOD(H),100)
            IF (ID1.EQ.ID2.AND.ID3.EQ.ID4.AND.MOD(MAX(ID1,ID3),2).EQ.0
     &        .AND.MAX(ID1,ID3).EQ.MIN(ID1,ID3)+1) THEN
              IF ((IPROC(I).EQ.(ID1*1000000+ID3*10000+ID4*100+ID2)).OR
     &          .(IPROC(I).EQ.(ID3*1000000+ID4*10000+ID1*100+ID2)).OR
     &          .(IPROC(I).EQ.(ID3*1000000+ID1*10000+ID2*100+ID4)))
     &          THEN
                WRITE(LMCWRT,1185) IPROC(I),IFSCOD(H)
                IF (CUTTMP(26,H).EQ.0.OR.CUTTMP(26,H).EQ.3) THEN
                  CALL EXSCUT(I-J,H,CUTTMP,SHCUT,ECUT,SCUT,CMAX,OMCMAX
     &              ,FLFLAG)
                ELSE
                  IF (IPROC(I).EQ.(ID1*1000000+ID3*10000+ID4*100+ID2))
     &              THEN
                    CALL SWAPCT(H,4,6,CUTTMP)
                    CALL EXSCUT(I-J,H,CUTTMP,SHCUT,ECUT,SCUT,CMAX,OMCMAX
     &                ,FLFLAG)
                    CALL SWAPCT(H,4,6,CUTTMP)
                  ELSEIF (IPROC(I).EQ.(ID3*1000000+ID4*10000+ID1*100+ID2
     &                )) THEN
                    CALL SWAPCT(H,3,5,CUTTMP)
                    CALL SWAPCT(H,4,6,CUTTMP)
                    CALL EXSCUT(I-J,H,CUTTMP,SHCUT,ECUT,SCUT,CMAX,OMCMAX
     &                ,FLFLAG)
                    CALL SWAPCT(H,3,5,CUTTMP)
                    CALL SWAPCT(H,4,6,CUTTMP)
                  ELSEIF (IPROC(I).EQ.(ID3*1000000+ID1*10000+ID2*100+ID4
     &                )) THEN
                    CALL SWAPCT(H,3,5,CUTTMP)
                    CALL EXSCUT(I-J,H,CUTTMP,SHCUT,ECUT,SCUT,CMAX,OMCMAX
     &                ,FLFLAG)
                    CALL SWAPCT(H,3,5,CUTTMP)
                  ENDIF
                  FLFLAG(I-J)=0
                ENDIF
                IPROC(I-J)=IFSCOD(H)
                EXUMWT(I-J)=EXUWTP(H)
              ENDIF
            ELSEIF (((ID1.EQ.ID2.AND.ID3.EQ.ID4).OR.(MOD(MAX(ID1,ID2)
     &          ,2).EQ.0.AND.MAX(ID1,ID2).EQ.MIN(ID1,ID2)+1.OR
     &          .MOD(MAX(ID3,ID4),2).EQ.0.AND.MAX(ID3,ID4).EQ.MIN(ID3
     &          ,ID4)+1)).AND.ID1.NE.ID3.AND.IPROC(I).EQ.(ID3*1000000
     &          +ID4*10000+ID1*100+ID2)) THEN
              WRITE(LMCWRT,1185) IPROC(I),IFSCOD(H)
              IPROC(I-J)=IFSCOD(H)
              EXUMWT(I-J)=EXUWTP(H)
              IF (CUTTMP(26,H).EQ.0.OR.CUTTMP(26,H).EQ.3) THEN
                CALL EXSCUT(I-J,H,CUTTMP,SHCUT,ECUT,SCUT,CMAX,OMCMAX
     &            ,FLFLAG)
              ELSE
                CALL SWAPCT(H,3,5,CUTTMP)
                CALL SWAPCT(H,4,6,CUTTMP)
                CALL EXSCUT(I-J,H,CUTTMP,SHCUT,ECUT,SCUT,CMAX,OMCMAX
     &            ,FLFLAG)
                CALL SWAPCT(H,3,5,CUTTMP)
                CALL SWAPCT(H,4,6,CUTTMP)
                FLFLAG(I-J)=0
              ENDIF
            ENDIF
          ENDIF
 57     CONTINUE
 59     CONTINUE
        IF (J.GT.0) IPROC(I)=0
*
* Has this final state already been specified?
*
        IF (I.GT.1) THEN
          DO 58 H=1,I-J-1
            IF (IPROC(I-J).EQ.IPROC(H)) THEN
              WRITE(LMCWRT,1186) IPROC(I-J)
              IPROC(I-J)=0
              J=J+1
              IF (IPROC(I+1).LE.0.OR.I.EQ.NUMPRC) GOTO 56
              GOTO 55
            ENDIF
 58       CONTINUE
        ENDIF
 55   CONTINUE
 56   CONTINUE        
      IF (IPROC(2).LE.0) THEN
        WRITE(LMCWRT,1190)
        INEVT=MAX(INEVT,NEVT)
      ENDIF
      IF (IPROC(2).GT.0) THEN
        WRITE(LMCWRT,1200)
*  Weighted events with mixed final states not supported!
        IF (IEXUWT.EQ.0) THEN
          IEXUWT=1
          WRITE(LMCWRT,1210)
        ENDIF
      ENDIF
 210  CONTINUE
*
      ISTAT=8
      NNCONP=0
      IBEGIN=1
      IF (ILEVL.EQ.1) IBEGIN=2
      DO 60 H=IBEGIN,2
        EXSXSC=0.0D0
        IF (H.EQ.1) THEN
          EXWMAX(I)=0.0D0
          EXGMWT(I)=0.0D0
        ENDIF
        DO 70 I=1,NUMPRC
 80       CONTINUE
          IF (IPROC(I).LE.0) GOTO 100
          CALL EXCFID(IPROC(I),PAR,ISTAT)
          IF (ILEVL.EQ.1) THEN
            INEVT=INT(EXS(I))
            IEVT(I)=INEVT+IEXINT
          ENDIF
          IF (ISTAT.NE.0) THEN
            WRITE(LMCWRT,1220) IPROC(I),ISTAT
            EXXSEC(I)=0.0D0
            ISTAT=0
          ELSE
            IF (H.EQ.1) THEN
              IGEVT(I)=0
              IFEVT(I)=0
              IEVT(I)=INEVT
              IF (PAR(5).EQ.'gl') THEN
                WRITE(LMCWRT,1231) PAR(3),PAR(4),PAR(5),PAR(6),IPROC(I)
              ELSE
                WRITE(LMCWRT,1230) PAR(3),PAR(4),PAR(5),PAR(6),IPROC(I)
              ENDIF
              IF (FLFLAG(I).NE.1) THEN
                IF (IEXCUT(1).EQ.0) THEN
                  IF (ECUT(3,I).NE.CUTS(2,1).OR.ECUT(4,I).NE.CUTS(3,1)
     &              .OR.ECUT(5,I).NE.CUTS(4,1).OR.ECUT(6,I).NE.CUTS(5,1)
     &              ) WRITE(LMCWRT,1232) ECUT(3,I),ECUT(4,I),ECUT(5,I)
     &              ,ECUT(6,I)
                  IF (SCUT(3,4,I).NE.CUTS(6,1).OR.SCUT(3,5,I).NE.CUTS(7
     &              ,1).OR.SCUT(3,6,I).NE.CUTS(8,1).OR.SCUT(4,5,I).NE
     &              .CUTS(9,1).OR.SCUT(4,6,I).NE.CUTS(10,1).OR.SCUT(5,6
     &              ,I).NE.CUTS(11,1)) WRITE(LMCWRT,1233) SCUT(3,4,I)
     &              ,SCUT(3,5,I),SCUT(3,6,I),SCUT(4,5,I),SCUT(4,6,I)
     &              ,SCUT(5,6,I)
                  LTEMP1=CMAX(1,3,I).NE.CUTS(12,1).OR.CMAX(1,4,I).NE
     &              .CUTS(13,1).OR.CMAX(1,5,I).NE.CUTS(14,1).OR.CMAX(1,6
     &              ,I).NE.CUTS(15,1).OR.CMAX(2,3,I).NE.CUTS(16,1).OR
     &              .CMAX(2,4,I).NE.CUTS(17,1).OR.CMAX(2,5,I).NE.CUTS(18
     &              ,1).OR.CMAX(2,6,I).NE.CUTS(19,1)
                  LTEMP2=CMAX(3,4,I).NE.CUTS(20,1).OR.CMAX(3,5,I).NE
     &              .CUTS(21,1).OR.CMAX(3,6,I).NE.CUTS(22,1).OR.CMAX(4,5
     &              ,I).NE.CUTS(23,1).OR.CMAX(4,6,I).NE.CUTS(24,1).OR
     &              .CMAX(5,6,I).NE.CUTS(25,1)
                  IF (LTEMP1) WRITE(LMCWRT,1234) CMAX(1,3,I),CMAX(1,4,I)
     &              ,CMAX(1,5,I),CMAX(1,6,I),CMAX(2,3,I),CMAX(2,4,I)
     &              ,CMAX(2,5,I),CMAX(2,6,I)
                  IF (LTEMP2.AND..NOT.LTEMP1) WRITE(LMCWRT,1235) CMAX(3
     &              ,4,I),CMAX(3,5,I),CMAX(3,6,I),CMAX(4,5,I),CMAX(4,6,I
     &              ),CMAX(5,6,I)
                  IF (LTEMP1.AND.LTEMP2) WRITE(LMCWRT,1236) CMAX(3,4,I)
     &              ,CMAX(3,5,I),CMAX(3,6,I),CMAX(4,5,I),CMAX(4,6,I)
     &              ,CMAX(5,6,I)
                ELSE
                  IF (FLFLAG(I).LE.2) THEN
                    WRITE(LMCWRT,1232) ECUT(3,I),ECUT(4,I),ECUT(5,I)
     &                ,ECUT(6,I)
                    WRITE(LMCWRT,1233) SCUT(3,4,I),SCUT(3,5,I),SCUT(3,6
     &                ,I),SCUT(4,5,I),SCUT(4,6,I),SCUT(5,6,I)
                  ENDIF
                  WRITE(LMCWRT,1234) CMAX(1,3,I),CMAX(1,4,I),CMAX(1,5
     &              ,I),CMAX(1,6,I),CMAX(2,3,I),CMAX(2,4,I),CMAX(2,5,I
     &              ),CMAX(2,6,I)
                  IF (FLFLAG(I).LE.2) WRITE(LMCWRT,1236) CMAX(3,4,I)
     &              ,CMAX(3,5,I),CMAX(3,6,I),CMAX(4,5,I),CMAX(4,6,I)
     &              ,CMAX(5,6,I)
                ENDIF
              ENDIF
            ENDIF
            IF (IEVT(I)-(H-1)*INEVT.GT.0) CALL EXCRUN(H,IEVT(I)-(H-1
     &        )*INEVT,10,I,ISTAT)
            EXXSEC(I)=0.0D0
            IF (EXS(I).GT.0.0D0) EXXSEC(I)=EXSUMW(I)/EXS(I)
          ENDIF
          IF(H.EQ.1) THEN
            IF (ISTAT.EQ.9.OR.EXXSEC(I).LE.0.0D0) THEN
              IF (ISTAT.EQ.9) WRITE(LMCWRT,1240)
              IF (EXXSEC(I).LE.0.0D0) THEN
                NNCONP=NNCONP+1
                INPROC(NNCONP)=IPROC(I)
                WRITE(LMCWRT,1250)
              ENDIF
              IF (IPROC(2).LE.0) ISTAT=7
              IF (IPROC(2).LE.0) GOTO 999
              DO 90 J=I,NUMPRC-1
                IPROC(J)=IPROC(J+1)
                SHCUT(J)=SHCUT(J+1)
                ECUT(3,J)=ECUT(3,J+1)
                ECUT(4,J)=ECUT(4,J+1)
                ECUT(5,J)=ECUT(5,J+1)
                ECUT(6,J)=ECUT(6,J+1)
                SCUT(3,4,J)=SCUT(3,4,J+1)
                SCUT(3,5,J)=SCUT(3,5,J+1)
                SCUT(3,6,J)=SCUT(3,6,J+1)
                SCUT(4,5,J)=SCUT(4,5,J+1)
                SCUT(4,6,J)=SCUT(4,6,J+1)
                SCUT(5,6,J)=SCUT(5,6,J+1)
                CMAX(1,3,J)=CMAX(1,3,J+1)
                CMAX(1,4,J)=CMAX(1,4,J+1)
                CMAX(1,5,J)=CMAX(1,5,J+1)
                CMAX(1,6,J)=CMAX(1,6,J+1)
                CMAX(2,3,J)=CMAX(2,3,J+1)
                CMAX(2,4,J)=CMAX(2,4,J+1)
                CMAX(2,5,J)=CMAX(2,5,J+1)
                CMAX(2,6,J)=CMAX(2,6,J+1)
                CMAX(3,4,J)=CMAX(3,4,J+1)
                CMAX(3,5,J)=CMAX(3,5,J+1)
                CMAX(3,6,J)=CMAX(3,6,J+1)
                CMAX(4,5,J)=CMAX(4,5,J+1)
                CMAX(4,6,J)=CMAX(4,6,J+1)
                CMAX(5,6,J)=CMAX(5,6,J+1)
                EXUMWT(J)=EXUMWT(J+1)
 90           CONTINUE
              IPROC(NUMPRC)=0
              GOTO 80
            ENDIF
            NCONP=NCONP+1
          ENDIF
          EXXERR(I)=EXSWSQ(I)/EXS(I)-(EXXSEC(I)**2)
          EXXERR(I)=SIGN(SQRT(ABS(EXXERR(I))),EXXERR(I))
          EXSXSC=EXSXSC+EXXSEC(I)
 70     CONTINUE
 100    CONTINUE
        IF (NCONP.LE.0) THEN
          WRITE(LMCWRT,1260)
          ISTAT=6
          GOTO 999
        ENDIF
        DO 110 I=1,NCONP
          EXXSRN(I)=EXXSEC(I)/EXSXSC
c1          IF (H.EQ.2.AND.EXMWEI(I).GT.EXWMAX(I)) EXWMAX(I)=EXMWEI(I)+2
c1     &      .0D0*SQRT(EXMWEI(I))
          IF (H.EQ.2.AND.EXUMWT(I).LE.0.0D0) THEN
            EXWMAX(I)=EXGMWT(I)+2.0D0*SQRT(EXGMWT(I))
          ELSEIF (H.EQ.2) THEN
            EXWMAX(I)=EXUMWT(I)+2.0D0*SQRT(EXUMWT(I))
            WRITE(LMCWRT,1187) IPROC(I),EXGMWT(I),EXUMWT(I)
          ENDIF
          IF (I.EQ.1) THEN
            EXCUMC(I)=EXXSRN(I)
          ELSE
            EXCUMC(I)=EXCUMC(I-1)+EXXSRN(I)
          ENDIF
          IF (H.EQ.2) IEVT(I)=INT(EXS(I))
          IF (IEVT(I).EQ.MAX(INEVT,NEVT).AND.NCONP.EQ.1.AND.H.EQ.1) THEN
            IF (EXUMWT(I).LE.0.0D0) THEN
              EXWMAX(I)=EXGMWT(I)+2.0D0*SQRT(EXGMWT(I))
            ELSE
              EXWMAX(I)=EXUMWT(I)+2.0D0*SQRT(EXUMWT(I))
              WRITE(LMCWRT,1187) IPROC(I),EXGMWT(I),EXUMWT(I)
            ENDIF
            GOTO 120
          ENDIF
*
* Integrate further according to relative cross-section
*
          IF (H.EQ.1) THEN
            IEVT(I)=MAX(0,INT(EXXSRN(I)*DBLE(NEVT)*SFTYFC)-INEVT)
            IF (IEVT(I).GT.0) IEVT(I)=(INEVT/4)*(1+IEVT(I)/(INEVT/4))
            IEVT(I)=IEVT(I)+INEVT
          ENDIF
 110    CONTINUE
 60   CONTINUE
 120  CONTINUE
*
* Report on initialization
*
      WRITE(LMCWRT,1270) NCONP
      DO 130 I=1,NCONP
        CALL EXCFID(IPROC(I),PAR,ISTAT)
        IF ((EXXSRN(I)*100.0D0.GT.1.0D-6.AND.100.0D0*EXXSEC(I).LT.999
     &    .99D0).OR.EXXSRN(I)*100.0D0.LT.1.0D-20) THEN
          IF (PAR(5).EQ.'gl') THEN
            WRITE(LMCWRT,1282) PAR,EXXSRN(I)*100.0D0,EXCUMC(I)*100.0D0
     &        ,IEVT(I)
          ELSE
            WRITE(LMCWRT,1280) PAR,EXXSRN(I)*100.0D0,EXCUMC(I)*100.0D0
     &        ,IEVT(I)
          ENDIF
        ELSE
          IF (PAR(5).EQ.'gl') THEN
            WRITE(LMCWRT,1283) PAR,EXXSRN(I)*100.0D0,EXCUMC(I)*100.0D0
     &        ,IEVT(I)
          ELSE
            WRITE(LMCWRT,1281) PAR,EXXSRN(I)*100.0D0,EXCUMC(I)*100.0D0
     &        ,IEVT(I)
          ENDIF
        ENDIF
 130  CONTINUE
      IF (NNCONP.GT.0) THEN
        WRITE(LMCWRT,1290) NNCONP
        DO 140 I=1,NNCONP
          CALL EXCFID(INPROC(I),PAR,ISTAT)
          WRITE(LMCWRT,1300) PAR(3),PAR(4),PAR(5),PAR(6)
 140    CONTINUE
      ENDIF
*      
      WRITE(LMCWRT,1310)
      ISTAT=0
      ENTRY LSTINI(ISTAT)
      IF (ISTAT.EQ.1) ILEVL=1
      DTEMP=0.0D0
      DTEMP1=0.0D0
      ITEMP=0
      DO 150 I=1,NCONP
        CALL EXCFID(IPROC(I),PAR,ISTAT)
        IF ((EXXSEC(I).GT.1.0D-6.AND.EXXSEC(I).LT.999.99D0).OR.EXXSEC(I
     &    ).LT.1.0D-20) THEN
          IF (PAR(5).EQ.'gl') THEN
            WRITE(LMCWRT,1322) PAR,EXXSEC(I),ABS(EXXERR(I))
     &        /SQRT(DBLE(IEVT(I)-1)),EXXSRN(I),IEVT(I),EXGMWT(I)
          ELSE
            WRITE(LMCWRT,1320) PAR,EXXSEC(I),ABS(EXXERR(I))
     &        /SQRT(DBLE(IEVT(I)-1)),EXXSRN(I),IEVT(I),EXGMWT(I)
          ENDIF
        ELSE
          IF (PAR(5).EQ.'gl') THEN
            WRITE(LMCWRT,1323) PAR,EXXSEC(I),ABS(EXXERR(I))
     &        /SQRT(DBLE(IEVT(I)-1)),EXXSRN(I),IEVT(I),EXGMWT(I)
          ELSE
            WRITE(LMCWRT,1321) PAR,EXXSEC(I),ABS(EXXERR(I))
     &        /SQRT(DBLE(IEVT(I)-1)),EXXSRN(I),IEVT(I),EXGMWT(I)
          ENDIF
        ENDIF
        DTEMP=DTEMP+EXXSEC(I)
        DTEMP1=DTEMP1+EXXERR(I)*EXXERR(I)/DBLE(IEVT(I)-1)
        ITEMP=ITEMP+IEVT(I)
 150  CONTINUE
      WRITE(LMCWRT,1330) DTEMP,SQRT(DTEMP1),ITEMP
*
* Initialize EXCALIBUR to HEPEVT conversion
*
      IF (ILEVL.NE.1) CALL EXCHEP(0,0)
*
* Initialize final-state radiation
*
      IF (ILEVL.NE.1) CALL EXCFSR(0)
*
      WRITE(LMCWRT,1340)
*
      ISTAT=0
 999  CONTINUE
      NOUT=0
      RETURN
 1010 FORMAT(11X,
     &  '===================================================',/,11X,
     &  '= =                                             = =',/,11X,
     &  '= =              E X C A L I B U R              = =',/,11X,
     &  '= =                                             = =',/,11X,
     &  '===================================================',///,11X,
     &  '                    01-Feb-1996',//,11X,
     &  '              J.B.Hansen & A.Waananen',//,1X,
     &  72('-'),/,21X,'EXCALIBUR Monte Carlo Initialisation',/)
 1020 FORMAT(/,1X,'Generating at centre-of-mass energy of',F9.3,' GeV')
 1030 FORMAT(/,1X,'Number of events to be generated: ',I8,/)
 1031 FORMAT(1X,'Number of debug-events to list: ',I8,/)
 1040 FORMAT(1X,'Standard model input parameters:',/
     &  ,'       G_F      = ',G14.8/,'       s^2_thet = ',G14.8/
     &  ,'       Z-mass   = ',G14.8/,'       Z-width  = ',G14.8/
     &  ,'       W-mass   = ',G14.8/,'       W-width  = ',G14.8/
     &  ,'       1/alpha  = ',G14.8/,'       alpha_s  = ',G14.8)
 1050 FORMAT(/,1X,'Anomalous couplings:',/,
     +  '       delta_gz=',G10.4,/,
     +  '       x_gamma =',G10.4,/,
     +  '       y_gamma =',G10.4,/,
     +  '       x_Z     =',G10.4,/,
     +  '       y_Z     =',G10.4,/,
     +  '       z_Z     =',G10.4,/)
 1060 FORMAT(/,1X,'No anomalous couplings.',/)
 1070 FORMAT(1X,'WARNING: sin2w value supplied:',F8.5
     &  ,' inconsistent with',/,1X
     &  ,'         EXCALIBUR recommended scheme, gives:',F8.5)
 1080 FORMAT(/,1X,'Processing options:',/,
     +  '       Debug printout?                     :',L2,/,
     +  '       Unweighted evts?                    :',L2,/,
     +  '       ISR on?                             :',L2,/,
     +  '       FSR on?                             :',L2,/,
     +  '       Hadronization on?                   :',L2,/,
     +  '       Coulomb-singularity?                :',L2,/,
     +  '       CKM-mixing?                         :',L2,/,
     +  '       Use TAUOLA for tau-decays?          :',L2)
 1090 FORMAT(/,4X,
     &  'All Feynman diagrams contribute to the event generation',/)
 1100 FORMAT(/,4X,
     &  'Only W+ W- Feynman diagrams contribute to the event generation'
     &  ,/)
 1110 FORMAT(/,4X,
     &  'Only ZZ  Feynman diagrams contribute to the event generation',/
     &  )
 1120 FORMAT(/,4X,
     &  'Only W+ e- nue~ production contribute to the event generation',
     &  /)
 1130 FORMAT(/,4X,
     &  'Only Z  e+ e- production contribute to the event generation',/)
 1140 FORMAT(/,4X,
     &  'Only Z  nue nue~ production contribute to the event generation'
     &  ,/)
 1091 FORMAT(4X,'QQgg final states may be included if specified'
     &  ,' in IPROC(..).',/,4X
     &  ,'The code for this option has been provided by R.Pittau',/)
 1092 FORMAT(4X,'QQgg final states are _EXCLUDED_ even if specified'
     &  ,' in IPROC(..).',/)
 1145 FORMAT(/,4X,'CKM mixing is included with CKM-matrix:',//
     &  ,'              |',f9.6,2f11.6,' |',/,'      CKM  =  |',f9.6
     &  ,2f11.6,' |',/,'              |',f9.6,2f11.6,' |',//
     &  ,'          (Diagrams involving top are excluded...)',/)
 1147 FORMAT(/,4X,'Fixed width is used in boson-propagators.',/)
 1148 FORMAT(/,4X,'Q-dependent width is used in boson-propagators.',/)
 1141 FORMAT(/,4X
     &  ,'FSR is generated by PHOTOS assuming 2 decaying bosons',/)
 1142 FORMAT(4X,
     &  'WARNING! The assumption of decaying bosons by PHOTOS is _NOT_',
     &  /,4X,'         strictly valid for all 4-fermion processes.',/)
 1143 FORMAT(/,4X,
     &  'FSR is generated using FINALG routine from FERMISV generator.'
     &  ,/,4X
     &  ,'Maxmimum number of photons generated by FINALG per event: ',I2
     &  ,/)
 1081 FORMAT(/,4X,'QCD corrections to boson widths are NOT applied',/)
 1082 FORMAT(/,4X,'QCD corrections to boson widths are applied using:',/
     &  ,/,10X,'Alpha_s(Mz) = ',G12.5,/)
 1083 FORMAT(4X
     &  ,'The specified total widths of the bosons remain unchanged,',/
     &  ,4X
     &  ,'while the partial widths of the bosons are recalculated using'
     &  ,/,4X,I1,'. order QCD corrections.',/)
 1086 FORMAT(4X
     &  ,'The specified total width of the W-boson remain unchanged,',/
     &  ,4X
     &  ,'while the partial widths of the boson are recalculated using'
     &  ,/,4X,I1,'. order QCD corrections.',/)
 1084 FORMAT(4X
     &  ,'The total and partial widths of the bosons are calculated',/
     &  ,4X,'from the specified electro-weak widths using',/,4X
     &  ,I1,'. order QCD corrections. The total widths are: ',//,10X
     &  ,'Gamma_W = ',G14.8,/,10X,'Gamma_Z = ',G14.8,/)
 1087 FORMAT(4X
     &  ,'The total and partial widths of the W-boson are calculated',/
     &  ,4X,'from the specified electro-weak width using',/,4X
     &  ,I1,'. order QCD corrections. The total widths are: ',//,10X
     &  ,'Gamma_W = ',G14.8,/)
 1151 FORMAT(/,4X,'4 FERMION pairing is done assuming WW-events if '
     &  ,'possible',/,4X,'- otherwise as ZZ-events.',/)
 1152 FORMAT(/,4X,'4 FERMION pairing is done according to phase space'
     &  ,' generation.',/)
 1153 FORMAT(/,4X
     &  ,'4 FERMION pairing is done according to MAX(X(1234),X(1432)).')
 1154 FORMAT(/,4X,'4 FERMION pairing is done according to '
     &  ,'MAX(X~(1234),X~(1432)),',/
     &  ,4X,'with X(interference) added according to phase space'
     &  ,' generation.')
 1155 FORMAT(/,4X
     &  ,'4 FERMION pairing is done according to probabilities:',/
     &  ,4X,'     Prob = X(1234)/(X(1234)+X(1432)) and 1-Prob')
 1156 FORMAT(/,4X
     &  ,'4 FERMION pairing is done according to probabilities:',/,4X
     &  ,'        Prob = X~(1234)/(X(1234)+X(1432)+X(interference))'
     &  ,/,4X,'and   1 - Prob',/,4X
     &  ,'where X(interference) is added according'
     &  ,' to phase space generation.')
 1157 FORMAT(/,4X
     &  ,'4 FERMION pairing is done according to probabilities:',/,4X
     &  ,'        Prob = X~(1234)/(X(1234)+X(1432)+X(interference))'
     &  ,/,4X,'and   1 - Prob',/
     &  ,3X,'where X(interference) is added according'
     &  ,' to MAX(X(1234),X(1423)).')
 1085 FORMAT(4X,'Here, X(1234) denote the cross-section for pairing',/
     &  ,4X,'the fermions as 1-2-3-4 etc.',/)
 1159 FORMAT(/,4X,'Color (re)connection mode:',/,7X
     &  ,'Default color connection as determined from pairing option.',/
     &  )
 1162 FORMAT(/,4X,'Color (re)connection mode:',/,7X
     &  ,'Opposite color connection as determined from pairing option,',
     &  /,7X,'but parton-shower is done with default pairing',/,7X
     &  ,'("color reconnection after shower", unphysical).',/)
 1163 FORMAT(/,4X,'Color (re)connection mode:',/,7X
     &  ,'Opposite color connection as determined from pairing option,',
     &  /,7X,'and likewise for the parton-shower',/,7X
     &  ,'("color reconnection before shower", unphysical).',/)
 1164 FORMAT(/,4X,'Color (re)connection mode:',/,7X,
     &  'Color reconnection is done at origin of event, after shower,',/
     &  ,7X,'according to instantaneously spherical space-time overlap',
     &  /,7X,'of the W-systems. Assumes Gaussian probability distri-',/
     &  ,7X,'butions in time and space, respectively.',/)
 1165 FORMAT(/,4X,'Color (re)connection mode:',/,7X,
     &  'Color reconnection is done at origin of event, after shower,',/
     &  ,7X,'according to time-retarded spherical space-time overlap',
     &  /,7X,'of the W-systems. Assumes Gaussian probability distri-',/
     &  ,7X,'butions in time and space, respectively.',/)
 1166 FORMAT(/,4X,'Color (re)connection mode:',/,7X,
     &  'Default color connection and parton-shower. Strings are recon-'
     &  ,/,7X,'nected according to instantaneously space-time overlap',/
     &  ,7X,'of cylindrical string piece volumes (color flux tubes).',/
     &  ,7X,'The probability distribution is taken to be Gaussian in',/
     &  ,7X,'time and space, respectively.',/)
 1167 FORMAT(/,4X,'Color (re)connection mode:',/,7X,
     &  'Default color connection and parton-shower. Strings are recon-'
     &  ,/,7X,'nected according to time-retarded space-time overlap',/
     &  ,7X,'of cylindrical string piece volumes (color flux tubes).',/
     &  ,7X,'The probability distribution is taken to be Gaussian in',/
     &  ,7X,'time and space, respectively.',/)
 1168 FORMAT(/,4X,'Color (re)connection mode:',/,7X,
     &  'Default color connection and parton-shower. Strings may recon-'
     &  ,/,7X,'nect when they cross ("thin vortex lines").',/)
 1169 FORMAT(/,4X,'Color (re)connection mode:',/,7X,
     &  'Default color connection and parton-shower. Strings may recon-'
     &  ,/,7X,'nect when they cross provided the total string length',/
     &  ,7X,'is reduced ("thin vortex lines").',/)
 1158 FORMAT(4X,'The specified color (re)connection mode is based on',
     &  /,4X,'a program written by T. Sjöstrand.',/,4X
     &  ,'References: "QCD Event generators" to appear in LEP2 report.',
     &  /,4X,'            T.Sjöstrand and A.Khoze, Phys. Rev. Lett. 72'
     &  ,' (1994) 28',/,4X
     &  ,'            T.Sjöstrand and A.Khoze, Zeit. Phys. C62 (1994)'
     &  /,4X,'Additional color (re)connection parameters:',/,11X
     &  ,'Rhad   = ',F10.5,/,11X,'Tfrag  = ',F10
     &  .5,/,11X,'RPROB = ',F10.5,/)
 1150 FORMAT(/,'    Following default cuts will be applied to all'
     &  ,' processes for which',/
     &  ,'    specific cuts have not been given:',/
     &  ,'    Energy cuts with e_3 > ',F10.6,/
     &  ,'                     e_4 > ',F10.6,/
     &  ,'                     e_5 > ',F10.6,/
     &  ,'                     e_6 > ',F10.6)
 1160 FORMAT(/,'    Cut on Ecm^2 after ISR    s*x1r*x2r  > ',F10.6)
 1170 FORMAT(/,'    Invariant mass cuts with s_34  > ',F10.6,/,
     +  '                             s_35  > ',F10.6,/,
     +  '                             s_36  > ',F10.6,/,
     +  '                             s_45  > ',F10.6,/,
     +  '                             s_46  > ',F10.6,/,
     +  '                             s_56  > ',F10.6)
 1180 FORMAT(/,'    Angle cuts with cthe_13  < ',F8.6,/,
     +  '                    cthe_14  < ',F8.6,/,
     +  '                    cthe_15  < ',F8.6,/,
     +  '                    cthe_16  < ',F8.6,/,
     +  '                    cthe_23  < ',F8.6,/,
     +  '                    cthe_24  < ',F8.6,/,
     +  '                    cthe_25  < ',F8.6,/,
     +  '                    cthe_26  < ',F8.6,/,
     +  '                    cthe_34  < ',F8.6,/,
     +  '                    cthe_35  < ',F8.6,/,
     +  '                    cthe_36  < ',F8.6,/,
     +  '                    cthe_45  < ',F8.6,/,
     +  '                    cthe_46  < ',F8.6,/,
     +  '                    cthe_56  < ',F8.6,//)
 1182 FORMAT(/,'    In this run cuts on Cos(theta) max wrt beam will'
     &  ,' be applied only',/,'    for forward (backward) going e- ',
     &  '(e+) other cuts are ignored.',/
     &  ,'    The cut on Cos(theta) max wrt beam is:',/,1X
     &  ,'               Cos(theta) max < ',F8.6,//)
 1185 FORMAT(1X,'EXCINI: Specified final state code: ',I8
     &  ,/,1X,'has been translated into unique EXCALIBUR code: ',I8,/,1X
     &  ,'Cuts have been translated accordingly -- CHECK THE CUTS!',/,1X
     &  ,'WARNING! It is recommended to use correct final state'
     &  ,' specification.',/)
 1186 FORMAT(1X,'EXCINI: Redundant final state: ',I8
     &  ,' removed from list of final states.')
 1187 FORMAT(1X,'EXCINI: Maximum weight for ',A2,' ',A2,'~',A2,' ',A2
     &  ,'~ of ',G14.6,/,1X,'has been superseeded by user weight of '
     &  ,G14.6)
 1190 FORMAT(1X
     &  ,'EXCINI: Initial integration over the specified channel:',/)
 1200 FORMAT(1X,'EXCINI: Initial pass through channels to determine',
     +  ' relative weights...',/,1X,
     &  '        Following channels has been selected:',/)
 1210 FORMAT(/,1X,' ***** WARNING: Only UNWEIGHTED events supported',
     +  ' with mixed final states *****',/,1X,
     +  '                Will proceed to generate',
     +  ' unweighted events',/)
 1220 FORMAT(/,1X,' Cannot decode process-code: ',I9,'  errorcode: ',I2,
     &  /)
 1230 FORMAT (1X,'    antiel(1) el(2) ---> ',A2,'(3)',' anti',A2,'(4) '
     &  ,A2,'(5) ','anti',A2,'(6)  (Code=',I9,')')
 1231 FORMAT (1X,'    antiel(1) el(2) ---> ',A2,'(3)',' anti',A2,'(4) '
     &  ,A2,'(5) ','    ',A2,'(6)  (Code=',I9,')')
 1232 FORMAT(1X,'    Final fermion minimum energies:',4F7.2,' GeV')
 1233 FORMAT(1X,'    Final state fermion-pair minimum inv mass^2:',/,1X
     &  ,'      3-4:',F10.5,'    3-5:',F10.5,'    3-6:',F10.5,/,1X
     &  ,'                        4-5:',F10.5,'    4-6:',F10.5,/,1X
     &  ,'                                          5-6:',F10.5)
 1234 FORMAT(1X,'    Maximum cos(fermion pair opening angles):',/,1X
     &  ,'      1-3:',F8.6,'  1-4:',F8.6,'  1-5:',F8.6,'  1-6:',F8.6,/
     &  ,1X,'      2-3:',F8.6,'  2-4:',F8.6,'  2-5:',F8.6,'  2-6:',F8.6)
 1235 FORMAT(1X,'    Maximum cos(fermion pair opening angles):',/,1X
     &  /,1X,'                    3-4:',F8.6,'  3-5:',F8.6,'  3-6:',F8.6
     &  ,/,1X,'                                  4-5:',F8.6,'  4-6:',F8
     &  .6,/,1X,'                                                5-6:'
     &  ,F8.6)
 1236 FORMAT(1X,'                    3-4:',F8.6,'  3-5:',F8.6,'  3-6:'
     &  ,F8.6,/,1X,'                                  4-5:',F8.6
     &  ,'  4-6:',F8.6,/,1X
     &  ,'                                                5-6:',F8.6)
 1240 FORMAT(1X,'ERROR: No processes give that final state.')
 1250 FORMAT (1X
     &  ,'The cross-section for this final state is ZERO - ignored',/)
 1260 FORMAT(1X,'ERROR: No processes give any of the final states.')
 1270 FORMAT(/,1X,'Following',I4,' final state(s) contribute:',/,1X,
     +  '       Code     Relative weight(%)   Cumulative weight(%)   ',
     &  '# Events')
 1280 FORMAT(3X,A2,' ',A2,'~ ',A2,' ',A2,'~',4X,F10.6,12X,F10.6,10X,I7)
 1281 FORMAT(3X,A2,' ',A2,'~ ',A2,' ',A2,'~',4X,G10.4,12X,F10.6,10X,I7)
 1282 FORMAT(3X,A2,' ',A2,'~ ',A2,' ',A2,' ',4X,F10.6,12X,F10.6,10X,I7)
 1283 FORMAT(3X,A2,' ',A2,'~ ',A2,' ',A2,' ',4X,G10.4,12X,F10.6,10X,I7)
 1290 FORMAT(/,1X,'Following',I4,' final state(s) do not contribute:',/)
 1300 FORMAT(11X,A2,' ',A2,'~ ',A2,' ',A2,'~')
 1310 FORMAT(/,1X,'EXCINI: Initial pass through channels to determine',
     +  ' relative weights gave the',/,9X,'following weights: ',//,3X,
     &  'Final state',6X,'Cross-section (pb)',3X,'Redundancy',4X,
     &  '# Events',2X,'Max weight')
 1320 FORMAT(2X,A2,' ',A2,'~ ',A2,' ',A2,'~',1X,F10.6,' +/-',F10.6,1X
     &  ,F10.6,5X,I6,1X,F12.6)
 1321 FORMAT(2X,A2,' ',A2,'~ ',A2,' ',A2,'~',1X,G10.4,' +/-',G10.4,1X
     &  ,F10.6,5X,I6,3X,G10.4)
 1322 FORMAT(2X,A2,' ',A2,'~ ',A2,' ',A2,' ',1X,F10.6,' +/-',F10.6,1X
     &  ,F10.6,5X,I6,1X,F12.6)
 1323 FORMAT(2X,A2,' ',A2,'~ ',A2,' ',A2,' ',1X,G10.4,' +/-',G10.4,1X
     &  ,F10.6,5X,I6,3X,G10.4)
 1330 FORMAT(2X,73('-'),/,11X,F15.6,' +/-',F10.6,13X,I9,//)
 1340 FORMAT(2X,72('-'),/)
      END
*
      SUBROUTINE EXCRUN(IFLAG,INEVT,ISTPMX,NPROC,ISTAT)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Feb-96   (from older routine)
*  CALLED    : EXCINI EXCEVT
*  CALLS     : SETPRO ADDRESS EXCRAN hj EXCMOM MOMARRAY EXCUTS
*              EXCMTX FLUX COULMB
*
*------------------------------------------
      IMPLICIT NONE
* Original commons/vars
      double precision beta
      parameter (beta=0.5d0)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      common/area9/bvel,gvel
      double precision bvel,gvel
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
* Additional commons...
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
*
      INTEGER IEXCLM,IEXABD,IEXNAD,NL,WWDIA
      COMMON /EXCSNG/ IEXCLM,IEXABD(144),IEXNAD(8),NL,WWDIA
      COMMON /SPYVAR/ NLOOP,JAD
* Arguments
      INTEGER IFLAG,INEVT,ISTPMX,NPROC,ISTAT
* Local variables
      INTEGER NCLAS
      PARAMETER (NCLAS=5)
      INTEGER H,I,J,K,L,M,NCT,NCM(1:150),NNM(1:150),INIT,ISTEP(152
     &  ),NCOUNT(152),NI(150,152),NOPT,IW,JAD,NLOOP,NOPRO,LM1,LRUN,LNOT
     &  ,IPRC,IMIX(6)
      REAL ULMASS
      DOUBLE PRECISION S1(150),AL(1:150,1:152),BET(0:150),DJ(1:28
     &  ,1:48),DIFF(30,152),ALM(1:150,1:152),V1(150,152)
     &  ,XFAC,APTOT,S1X,OMXR1,OMXR2,WT,DJM1,ECTOT,DJTOT
     &  ,SQUAREM,EXCRAN,RCHO,TJ,HJ,FLUX,FERMAS(6)
     &  ,X1234,XINT,X1432,X1243,QCD(2),SH3456,SH3546,SH4536
     &  ,SHMIN1,SHMIN2,SHMIN,ZCUT,DTEMP,THEFACNOR
      CHARACTER*2 PAR(3:6)
      CHARACTER*11 THEFMT
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
*
      SAVE NI,ISTEP,NCOUNT
      SAVE AL,ALM,V1,DIFF
      SAVE NCM,NNM,NCT
c---------------------------------------------------------------c
c                                                               c
c  IEXDIA = 0  all diagrams                                     c
c         = 1  WW diagram                                       c
c         = 2  ZZ diagrams                                      c
c         = 3  W+ e- nue_bar production                         c
c         = 4  Z e+ e- production                               c
c         = 5  Z nue nue_bar production                         c
c                                                               c
c  WARNING: in order to avoid double counting, final states     c
c           with different particles should be used if          c
c           IEXDIA is not 0.                                    c
c                                                               c
c---------------------------------------------------------------c
      IF (IFLAG.GT.5) THEN
        INQUIRE(IFLAG,FORM=THEFMT)
        IF (THEFMT(1:1).EQ.'U') THEN
          IF (ISTAT.EQ.1) THEN
            WRITE(IFLAG,ERR=1) NI,ISTEP,NCOUNT
            WRITE(IFLAG,ERR=2) AL,ALM,V1,DIFF
            WRITE(IFLAG,ERR=3) NCM,NNM,NCT
          ELSEIF (ISTAT.EQ.2) THEN
            READ(IFLAG,ERR=1) NI,ISTEP,NCOUNT
            READ(IFLAG,ERR=2) AL,ALM,V1,DIFF
            READ(IFLAG,ERR=3) NCM,NNM,NCT
          ENDIF
        ELSE
          IF (ISTAT.EQ.1) THEN
            WRITE(IFLAG,*,ERR=1) NI,ISTEP,NCOUNT
            WRITE(IFLAG,*,ERR=2) AL,ALM,V1,DIFF
            WRITE(IFLAG,*,ERR=3) NCM,NNM,NCT
          ELSEIF (ISTAT.EQ.2) THEN
            READ(IFLAG,*,ERR=1) NI,ISTEP,NCOUNT
            READ(IFLAG,*,ERR=2) AL,ALM,V1,DIFF
            READ(IFLAG,*,ERR=3) NCM,NNM,NCT
          ENDIF
        ENDIF
        GOTO 4
 1      CONTINUE
        WRITE(LMCWRT,*)
     &    'ERROR! Cannot dump/read NI,ISTEP,NCOUNT on unit',IFLAG
        ISTAT=9
        GOTO 999
 2      CONTINUE
        WRITE(LMCWRT,*) 'ERROR! Cannot dump/read AL,ALM,V1,DIFF on unit'
     &    ,IFLAG
        ISTAT=8
        GOTO 999
 3      CONTINUE
        WRITE(LMCWRT,*) 'ERROR! Cannot dump/read NCM,NNM,NCT on unit'
     &    ,IFLAG
        ISTAT=7
        GOTO 999
 4      CONTINUE
        ISTAT=0
        GOTO 999
      ENDIF
      IEXPHG=0
      LR=NPROC
      W=0.0D0
*
* Decode final state
*
      IPRC=IPROC(NPROC)
      IMIX(1)=0
      QCD(1)=1.0D0
      QCD(2)=1.0D0
      IF (IEXMIX.GT.0) THEN
        CALL EXCDFS(IPROC(NPROC),IPRC)
        IMIX(1)=1
        IMIX(2)=1
        IMIX(3)=INT(IPROC(NPROC)/1000000)
        IMIX(4)=MOD(INT(IPROC(NPROC)/10000),100)
        IMIX(5)=MOD(INT(IPROC(NPROC)/100),100)
        IMIX(6)=MOD(IPROC(NPROC),100)
        DO I=3,6
          IF (IMIX(I).LT.7) THEN
            IMIX(I)=IMIX(I)+6
          ELSEIF (IMIX(I).GT.10.AND.IMIX(I).LT.17) THEN
            IMIX(I)=IMIX(I)-10
          ENDIF
        ENDDO
      ENDIF
      CALL EXCFID(IPRC,PAR,ISTAT)
      IF (IEXQCD.GT.0) CALL EXQCDC(QCD)
*
      ISTAT=9
*
* Set the original routiones and initialize process...
*
      IF (IFLAG.LE.2) THEN
        IEXCLM=IEXCOU
        CALL SETPRO(NCM,NNM,NCT,IEXDIA,NOPRO,PAR)
      ENDIF
      IF (NOPRO.EQ.1) GOTO 999      
      ISTAT=8
*
* Normalization factor
*
      IF (MOD(INT(IPROC(NPROC)/100),100).EQ.21) THEN
        XFAC= PI*PI*16.0D0*ALPHA*ALS
        FACNOR= XFAC*XFAC*PBFAC*STATFAC/2048.0D0/PI**8/ROOTS/ROOTS
      ELSE
        XFAC= PI*PI*16.0D0*ALPHA*ALPHA/STH2/STH2
        FACNOR= XFAC*XFAC*FCOL*PBFAC*STATFAC/2048.0D0/PI**8
      ENDIF
*
* The a-priori weights are initialized to be all equal
*
      IF (IFLAG.EQ.1) THEN
        DO 10 I=1,NCT
          AL(I,NPROC)=1.0D0/NCT
          ALM(I,NPROC)=AL(I,NPROC)
 10     CONTINUE
        NCOUNT(NPROC)=0
        ISTEP(NPROC)=0
        IEXWNL(NPROC)=0
        IEXWNG(NPROC)=0
        EXS(NPROC)=0.0D0
        EXSUMW(NPROC)=0.0D0
        EXSWSQ(NPROC)=0.0D0
        EXSWS3(NPROC)=0.0D0
        EXSWS4(NPROC)=0.0D0
        EXMWEI(NPROC)=0.0D0
        EXGMWT(NPROC)=0.0D0
        INIT=0
      ENDIF
      IF (IFLAG.GE.2) INIT=1
      BET(0)=0.0D0
      DO 20 J=1,NCT
        BET(J)=0.0D0
        DO 30 K=1,J
          BET(J)=BET(J)+AL(K,NPROC)
 30     CONTINUE
 20   CONTINUE
      NOPT=100000
*
* Final state fermion masses
*
      FERMAS(1)=DBLE(ULMASS(11))
      FERMAS(2)=DBLE(ULMASS(11))
      FERMAS(3)=DBLE(ULMASS(INT(IPROC(NPROC)/1000000)))
      FERMAS(4)=DBLE(ULMASS(MOD(INT(IPROC(NPROC)/10000),100)))
      FERMAS(5)=DBLE(ULMASS(MOD(INT(IPROC(NPROC)/100),100)))
      FERMAS(6)=DBLE(ULMASS(MOD(IPROC(NPROC),100)))
* Uncomment to get massless particles, but watch out for singularities
c      do i=1,6
c        fermas(i)=0.000d0
c      enddo
*
* The integration/(one event generation) starts
*
      NLOOP=0
 40   CONTINUE
      DO 50 I=1,INEVT
*
        LNOT=0
        IF (MOD(I,MAX(1,INT(INEVT/10))).EQ.0.AND.INEVT.GT.5000)
     &    WRITE(LMCWRT,1010) I
*
* Self-optimization of the a-priori weights if i < nopt
*
        IF ((MOD(INT(EXS(NPROC))+1,INT(NOPT/ISTPMX)).EQ.0.OR.(I.EQ.INEVT
     &    .AND.IFLAG.LE.1).OR
     &    .INT(EXS(NPROC))+1.EQ.5000.OR.INT(EXS(NPROC))+1.EQ.15000).AND
     &    .INT(EXS(NPROC))+1.GT.0.AND.INT(EXS(NPROC))+1.LE.NOPT) THEN
          ISTEP(NPROC)=ISTEP(NPROC)+1
          IF (NI(M,NPROC).EQ.0) GOTO 111
          DO 60 M=1,NCT
            S1(M)=V1(M,NPROC)/DBLE(NI(M,NPROC))
 60       CONTINUE
          APTOT=0.0D0
          DO 70 M=1,NCT
            APTOT=APTOT+AL(M,NPROC)*S1(M)**BETA
 70       CONTINUE
          IF (APTOT.EQ.0.0D0) GOTO 111
          DO 80 LM1= 1,NCT
            IF (LM1.EQ.1) THEN
              S1X=ABS(APTOT-S1(LM1)**BETA)
            ELSE
              S1X=MAX(S1X,ABS(APTOT-S1(LM1)**BETA))
            ENDIF
 80       CONTINUE
          DIFF(ISTEP(NPROC),NPROC)=S1X
          IF (IEXDBG.GT.1) THEN
            WRITE(LMCWRT,*) NPROC,IPROC(NPROC)
            WRITE(LMCWRT,'(1X,A,7G12.6,/,1X,A,6G12.6)') 'DIFF:',(DIFF(J
     &        ,NPROC),J=1,7),'     ',(DIFF(J,NPROC),J=8,13)
            WRITE(LMCWRT,*) 'MAX: ',EXMWEI(NPROC),INT(EXS(NPROC))+1
            IF (INT(EXS(NPROC))+1.EQ.NOPT) THEN
              DO J=1,NCT
                WRITE(LMCWRT,'(1X,I3,3X,G12.6)') J,AL(J,NPROC)
              ENDDO
            ENDIF
          ENDIF
          IF (ISTEP(NPROC).EQ.1.OR.DIFF(ISTEP(NPROC),NPROC).LE.
     +      DIFF(ISTEP(NPROC)-1-NCOUNT(NPROC),NPROC).OR.(INT(EXS(NPROC))
     &      +1.EQ.NOPT.AND.NCOUNT(NPROC).GT.0)) THEN
            DO 90 IW=1,NCT
              ALM(IW,NPROC)=AL(IW,NPROC)
 90         CONTINUE
            IF (IEXDBG.GT.1) WRITE(LMCWRT,*) 'UPDATING'
            DO 100 M=1,NCT
              AL(M,NPROC)=AL(M,NPROC)*(S1(M)**BETA)/APTOT
 100        CONTINUE
* Update max-weight to follow the optimized generation
            IF (EXMWEI(NPROC).GE.EXUMWT(NPROC)) THEN
              EXWMAX(NPROC)=EXMWEI(NPROC)+2.0D0*SQRT(EXMWEI(NPROC))
            ELSE
              EXWMAX(NPROC)=EXUMWT(NPROC)
            ENDIF
            EXMWEI(NPROC)=0.0D0
            INIT=0
            NCOUNT(NPROC)=0
          ELSE
            NCOUNT(NPROC)=NCOUNT(NPROC)+1
          ENDIF
c1          DO 100 M=1,NCT
c1            AL(M,NPROC)=AL(M,NPROC)*(S1(M)**BETA)/APTOT
c1 100      CONTINUE
c1          INIT=0
          DO 110 J=1,NCT
            BET(J)=0.0D0
            DO 120 K=1,J
              BET(J)=BET(J)+AL(K,NPROC)
 120        CONTINUE
 110      CONTINUE
 111      CONTINUE
        ENDIF
*
* The local densities dj are initialized to 0
*
        DO 130 J=1,28
          DO 140 K=1,48
            DJ(J,K)=0.0D0
 140      CONTINUE
 130    CONTINUE
        RCHO=EXCRAN(0)
        DO 150 LRUN=1,NCT
          IF (BET(LRUN-1).LE.RCHO.AND.RCHO.LE.BET(LRUN)) JAD=LRUN
 150    CONTINUE
        IEXPHG=NCM(JAD)
*
* Generates omxr1= 1-xr1 and omxr2= 1-xr2 for the QED convolution if IEXISR>0
*
        IF (IEXISR.GT.0) THEN
c-      shmin is a cut on the reduced invariant mass squared.
c-      If schut in the input is lower then the minimum value
c-      compatible with the cuts on scut(i,j), shmin is
c-      automatically computed.
          SH3456=SCUT(3,4,LR)+SCUT(5,6,LR)+2.0D0*SQRT(SCUT(3,4,LR)
     &      *SCUT(5,6,LR))
          SH3546=SCUT(3,5,LR)+SCUT(4,6,LR)+2.0D0*SQRT(SCUT(3,5,LR)
     &      *SCUT(4,6,LR))
          SH4536=SCUT(4,5,LR)+SCUT(3,6,LR)+2.0D0*SQRT(SCUT(4,5,LR)
     &      *SCUT(3,6,LR))
          SHMIN1=MAX(SHCUT(LR),SH3456)
          SHMIN2=MAX(SH3546,SH4536)
          SHMIN=MAX(SHMIN1,SHMIN2)
          ZCUT=SHMIN/ROOTS/ROOTS
          OMXR1=TJ(1.0D0,OMZ1,0.0D0,1.0D0,-1)
          OMXR2=TJ(1.0D0,OMZ1,0.0D0,1.0D0,-1)
          XR1=1.0D0-OMXR1
          XR2=1.0D0-OMXR2
          IF (XR1*XR2.LT.ZCUT) THEN
            LNOT=1
            W=0.0D0
            GOTO 180
          ENDIF
          DJM1=HJ(1.0D0,OMZ1,0.0D0,-1.0D0)**2*(OMXR1*OMXR2)**(ZETA-ZETA1
     &      )
        ELSE
          XR1=1.0D0
          XR2=1.0D0
        ENDIF
*
* Calculates the cuts for the generation in the C.M. frame
*
        BVEL=(XR1-XR2)/(XR1+XR2)
        GVEL=1.0D0/SQRT(1.0D0-BVEL*BVEL)
        ECTOT=0.0D0
        DO 160 J= 3,6
* Massless particles (for massive particles the cuts are too weak but we
* cut strictly later in EXCUTS)
          IF (INT(1000000.0D0*FERMAS(J)).EQ.0) THEN
            CMAXH(1,J,LR)=(CMAX(1,J,LR)-BVEL)/(1.0D0-BVEL*CMAX(1,J,LR))
            CMAXH(2,J,LR)=(CMAX(2,J,LR)+BVEL)/(1.0D0+BVEL*CMAX(2,J,LR))
          ELSE
            CMAXH(1,J,LR)=1.0D0
            CMAXH(2,J,LR)=1.0D0
            M=MOD(INT(IPROC(NPROC)/INT(100**(6-J))),100)
            IF (M.EQ.11.AND.MOD(J,2).EQ.0) CMAXH(1,J,LR)=MIN((CMAX(1,J
     &        ,LR)-BVEL)/(1.0D0-BVEL*CMAX(1,J,LR)),0.9999999999D0)
            IF (M.EQ.11.AND.MOD(J,2).EQ.1) CMAXH(2,J,LR)=MIN((CMAX(2,J
     &        ,LR)+BVEL)/(1.0D0+BVEL*CMAX(2,J,LR)),0.9999999999D0)
          ENDIF
          IF (BVEL.GE.0.0D0) THEN
            ECUTH(J,LR)=ECUT(J,LR)*GVEL*(1.0D0-BVEL*CMAX(1,J,LR))
          ELSE
            ECUTH(J,LR)=ECUT(J,LR)*GVEL*(1.0D0+BVEL*CMAX(2,J,LR))
          ENDIF
          ECTOT=ECTOT+MAX(ECUTH(J,LR),FERMAS(J))
 160    CONTINUE
*
* Not enough energy left???
        IF (ROOTS*ROOTS*XR1*XR2.LT.ECTOT*ECTOT) THEN
          LNOT=2
          W=0.0D0
          GOTO 180
        ENDIF
*
* The initial configuration of the momenta and masses is set calling EXCMOM
*
        CALL EXCMOM(FERMAS)
*
* The event momenta is generated
*
        FAILFLAG=0
        CALL ADDRESS(0,NCM(JAD),NNM(JAD),DJ)
        IF (FAILFLAG.NE.0) THEN
          LNOT=3
          W=0.0D0
          GOTO 180
        ENDIF
*
* All momenta are put in a big array
*
        CALL MOMARRAY(NCM(JAD),NNM(JAD))
*
* Cuts are applied
*
        CALL EXCUTS(LNOT)
        IF (LNOT.EQ.1) THEN
          LNOT=4
          W=0.0D0
          GOTO 180
        ENDIF
*
* Local densities are calculated
*
        FAILFLAG=0
        LNOT=0
        DJTOT=0.0D0
        DO 170 J=1,NCT
          CALL ADDRESS(1,NCM(J),NNM(J),DJ)
          DJTOT=DJTOT+AL(J,NPROC)*DJ(NCM(J),NNM(J))
          if (failflag.ne.0.and.lnot.eq.0) lnot=j
 170    CONTINUE
        IF (FAILFLAG.NE.0) THEN
          LNOT=lnot*10+5
          W=0.0D0
          GOTO 180
        ENDIF
        THEFACNOR=1.0D0
*
* The squared matrix element and the weight are computed
*
* First the QQgg matrix element
        IF (MOD(INT(IPROC(NPROC)/100),100).EQ.21) THEN
          CALL MATRIX1(SQUAREM,X1234,X1243,XINT)
          EX1234=X1234
          EX1432=X1243
          EXINT=XINT
          W= SQUAREM/DJTOT
        ELSE
* Else the 4 fermion matrix element
          CALL EXCMTX(SQUAREM,X1234,XINT,X1432,IMIX,QCD)
          IF ((INT(IPROC(NPROC)/1000000).LT.7.AND.MOD(INT(IPROC(NPROC)
     &      /100),100).LT.7).OR.(INT(IPROC(NPROC)/1000000).GT.7.AND
     &      .MOD(INT(IPROC(NPROC)/100),100).GT.7)) THEN
* For 4-quark or 4-lepton states we have to flip...
            EX1234=X1234
            X1234=X1432
            X1432=EX1234
          ENDIF
          EX1234=X1234
          EX1432=X1432
          EXINT=XINT
          W=EX1234+EXINT+EX1432
          THEFACNOR=FACNOR/DJTOT/ROOTS/ROOTS/XR1/XR2
        ENDIF
* Correct for ISR
        IF (IEXISR.GT.0) THEFACNOR=THEFACNOR*FLUX(ALPHAR,CL,ZETA,GAEUL
     &    ,XR1,OMXR1)*FLUX(ALPHAR,CL,ZETA,GAEUL,XR2,OMXR2)*DJM1
        W=THEFACNOR*W
 180    CONTINUE
        IF (IEXDBG.GT.1) THEN
          if (failflag.ne.0) print
     &      '(1x,i2,3x,i3,3x,i6,3x,i3,3x,i3)',failflag,lnot
     &      ,INT(EXS(NPROC)),jad,ncm(jad)
        ENDIF
        IF (INT(EXS(NPROC)).LE.NOPT) THEN
          WT=0.0D0
          DO 190 J=1,NCT
            IF (LNOT.EQ.0) WT=W*W*DJ(NCM(J),NNM(J))/DJTOT
            IF (INIT.EQ.0) THEN
              NI(J,NPROC)=0
              V1(J,NPROC)=0.0D0
            ENDIF
            NI(J,NPROC)=NI(J,NPROC)+1
            V1(J,NPROC)=V1(J,NPROC)+WT
 190      CONTINUE
          IF (INIT.EQ.0) INIT=1
        ENDIF
*
* Update counters...
*
        IF (W.EQ.0.0D0) IEXWNL(NPROC)=IEXWNL(NPROC)+1
        IF (W.LT.0.0D0) IEXWNG(NPROC)=IEXWNG(NPROC)+1
        EXS(NPROC)=EXS(NPROC)+1.0D0
        EXSUMW(NPROC)=EXSUMW(NPROC)+W
        EXSWSQ(NPROC)=EXSWSQ(NPROC)+W*W
        EXSWS3(NPROC)=EXSWSQ(NPROC)+W*W*W
        EXSWS4(NPROC)=EXSWSQ(NPROC)+W*W*W*W
        IF (W.GT.EXMWEI(NPROC)) EXMWEI(NPROC)=W
        IF (W.GT.EXGMWT(NPROC)) EXGMWT(NPROC)=W
*
 50   CONTINUE
*
* If we generate events -- compare weight with max weight
*
      IF (INEVT.EQ.1.AND.IEXUWT.GT.0) THEN
* Have we tried more than INFAIL times to generate an event???
        IF (NLOOP.GT.INFAIL) GOTO 999
        IF (W.GT.EXWMAX(NPROC)*SAFETY) THEN
          CALL EXCFID(IPROC(NPROC),PAR,ISTAT)
          IF (MOD(INT(IPROC(NPROC)/100),100).EQ.21) THEN
            WRITE(LMCWRT,1030) PAR,W,EXWMAX(NPROC)
          ELSE
            WRITE(LMCWRT,1020) PAR,W,EXWMAX(NPROC)
          ENDIF
c1          EXWMAX(NPROC)=W
        ENDIF
        RCHO=EXCRAN(1)
        IF (W.LT.EXWMAX(NPROC)*RCHO*SAFETY) THEN
          IF (LNOT.NE.4) NLOOP=NLOOP+1
          GOTO 40
        ENDIF
      ENDIF
      IF (INEVT.EQ.1) CALL EXCDCH(NCM(JAD),NNM(JAD))
*
      DO I=1,NCLAS
        EXCSIG(1,I)=EXCSIG(1,I)*THEFACNOR
        EXCSIG(2,I)=EXCSIG(2,I)*THEFACNOR
        EXCSIG(3,I)=EXCSIG(3,I)*THEFACNOR
      ENDDO
      EX1234=EX1234*THEFACNOR
      EX1432=EX1432*THEFACNOR
      EXINT=EXINT*THEFACNOR
*
      ISTAT=0
*
 999  CONTINUE
      RETURN
 1010 FORMAT(' ITER=',I8)
 1020 FORMAT(2X,'Warning! Max-weight for the process: ',A2,' -',A2,
     &  ' ',A2,' -',A2,' exceeded.',/,2X,'Event-weight: ',G10.4
     &  ,'. Max-weight: ',G10.4,/)
 1030 FORMAT(2X,'Warning! Max-weight for the process: ',A2,' -',A2,
     &  ' ',A2,'  ',A2,' exceeded.',/,2X,'Event-weight: ',G10.4
     &  ,'. Max-weight: ',G10.4,/)
c1 1020 FORMAT(2X,'Warning! Max-weight for the process: ',A2,' -',A2,
c1     &  ' ',A2,' -',A2,' exceeded.',/,2X,'Event-weight: ',G10.4
c1     &  ,'. Old max-weight: ',G10.4,' increased to: ',G10.4,/)
c1 1030 FORMAT(2X,'Warning! Max-weight for the process: ',A2,' -',A2,
c1     &  ' ',A2,'  ',A2,' exceeded.',/,2X,'Event-weight: ',G10.4
c1     &  ,'. Old max-weight: ',G10.4,' increased to: ',G10.4,/)
      END
*      
      SUBROUTINE EXCEVT(WEIGHT,ISTAT)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Feb-96   (from older routine)
*  CALLED    : (user)
*  CALLS     : EXCRUN EXCRAN EXCFID EXCFSR EXFRAG EXCHEP
*
*------------------------------------------
      IMPLICIT NONE
* Additional commons...
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
* Arguments
      INTEGER ISTAT
      DOUBLE PRECISION WEIGHT
* Local variables
      INTEGER I,IOLDP,INEWP,ITEMP,INUMEV
      CHARACTER*2 PAR(3:6)
      REAL RVEC(3)
      DOUBLE PRECISION EXCRAN,RCHO
*
      DATA INUMEV/0/
      SAVE IOLDP,INEWP,INUMEV
*
      WEIGHT=0.0D0
      ISTAT=9
      IF (NCONP.LE.0) GOTO 999
      IOLDP=INEWP
*
* Select a channel ...
*
      IF (NCONP.GT.1) THEN
        RCHO=EXCRAN(0)
        DO 10 I=1,NCONP
          IF (RCHO.LT.EXCUMC(I)) THEN
            INEWP=IPROC(I)
            GOTO 20
          ENDIF
 10     CONTINUE
        INEWP=IPROC(NCONP)
 20     CONTINUE
      ELSE
        IOLDP=IPROC(1)
        INEWP=IOLDP
        I=1
      ENDIF
      CALL EXCFID(INEWP,PAR,ISTAT)
      IF (ISTAT.NE.0) ISTAT=80+ISTAT
      IF (ISTAT.NE.0) GOTO 999
*
* Generate an event
*
      ITEMP=1
      IEXPRO=I
      IF (IOLDP.EQ.INEWP) THEN
        CALL EXCRUN(3,ITEMP,10,I,ISTAT)
      ELSE
        CALL EXCRUN(2,ITEMP,10,I,ISTAT)
      ENDIF
      IF (ISTAT.NE.0) ISTAT=70+ISTAT
      IF (ISTAT.NE.0) GOTO 999
*
* Generate primary vertex..
*  VERTEX is filled from outside if needed
C      CALL RNORML(RVEC,3)
C      VERTEX(1)=DBLE(RVEC(1))*SDVRT(1)
C      VERTEX(2)=DBLE(RVEC(2))*SDVRT(2)
C      VERTEX(3)=DBLE(RVEC(3))*SDVRT(3)
C      VERTEX(4)=0.0D0
*
* EXCALIBUR to HEPEVT conversion routine
*
      CALL EXCHEP(1,INEWP)
*
* Generate FSR
*
      CALL EXCFSR(1)
*
* Transfer to LUND commons and Hadronization
*
      CALL EXFRAG(ISTAT)
      IF (ISTAT.NE.0) ISTAT=60+ISTAT
      IF (ISTAT.NE.0) GOTO 999
      IGEVT(I)=IGEVT(I)+1
      WEIGHT=1.0D0
      ISTAT=0
*
* Special weight fudge for weighted events
*
      IF (NCONP.LE.1.AND.IEXUWT.LE.0) THEN
        IF (EXXSRN(I).GT.0) THEN
          WEIGHT=EX1234+EXINT+EX1432
        ELSE
          ISTAT=5
          WEIGHT=0.0D0
          WRITE(LMCWRT,1010) I,INEWP
        ENDIF
      ENDIF
*
      INUMEV=INUMEV+1
      IF(NEVT.GE.10) THEN
        IF (MOD(INUMEV,NEVT/10).EQ.0) WRITE(LMCWRT,1020) INUMEV
      ENDIF
 999  CONTINUE
      IF (ISTAT.NE.0) IFEVT(I)=IFEVT(I)+1
      RETURN
 1010 FORMAT(/,'   ERROR: Event generated in channel',I3,' (',I8
     &  ,')!! Shouldnt be there....',/)
 1020 FORMAT(1X,I8,' events has now been generated.')
      END
*     
      SUBROUTINE EXCEND(ISTAT)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Feb-96
*  CALLED    : (user)
*  CALLS     : EXCFID
*
*------------------------------------------
      IMPLICIT NONE
* Additional commons...
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
* Arguments
      INTEGER ISTAT
* Local variables
      INTEGER I,ITEMP,ITEMP1,ITEMP2,ITEMP3,ITEMP4
      CHARACTER*2 PAR(3:6)
      DOUBLE PRECISION DTEMP,DTEMP1,DTEMP2,DTEMP3
*     
      ISTAT=9
      WRITE(LMCWRT,1005)
      DTEMP2=0.0D0
      ITEMP=0
      DO 10 I=1,NCONP
        ITEMP=ITEMP+IEVT(I)
        DTEMP2=DTEMP2+EXSUMW(I)/EXS(I)
 10   CONTINUE
*
* Report on final integration
*
      WRITE(LMCWRT,1040)
      DTEMP1=0.0D0
      DTEMP3=0.0D0
      ITEMP1=0
      ITEMP2=0
      ITEMP4=0
      DO 20 I=1,NCONP
        CALL EXCFID(IPROC(I),PAR,ISTAT)
        IF ((EXSUMW(I)/EXS(I).GT.1.0D-6.AND.EXSUMW(I)/EXS(I).LT.999.99D0
     &    ).OR.EXSUMW(I)/EXS(I).LT.1.0D-20) THEN
          IF (PAR(5).EQ.'gl') THEN
            WRITE(LMCWRT,1052) PAR,EXSUMW(I)/EXS(I),SQRT(ABS(EXSWSQ(I
     &        )-(EXSUMW(I)**2-EXSWSQ(I))/(EXS(I)-1.0D0)))/EXS(I)
     &        ,EXSUMW(I)/EXS(I)/DTEMP2,INT(EXS(I)),EXMWEI(I)
          ELSE
            WRITE(LMCWRT,1050) PAR,EXSUMW(I)/EXS(I),SQRT(ABS(EXSWSQ(I
     &        )-(EXSUMW(I)**2-EXSWSQ(I))/(EXS(I)-1.0D0)))/EXS(I)
     &        ,EXSUMW(I)/EXS(I)/DTEMP2,INT(EXS(I)),EXMWEI(I)
          ENDIF
        ELSE
          IF (PAR(5).EQ.'gl') THEN
            WRITE(LMCWRT,1053) PAR,EXSUMW(I)/EXS(I),SQRT(ABS(EXSWSQ(I
     &        )-(EXSUMW(I)**2-EXSWSQ(I))/(EXS(I)-1.0D0)))/EXS(I)
     &        ,EXSUMW(I)/EXS(I)/DTEMP2,INT(EXS(I)),EXMWEI(I)
          ELSE
            WRITE(LMCWRT,1051) PAR,EXSUMW(I)/EXS(I),SQRT(ABS(EXSWSQ(I
     &        )-(EXSUMW(I)**2-EXSWSQ(I))/(EXS(I)-1.0D0)))/EXS(I)
     &        ,EXSUMW(I)/EXS(I)/DTEMP2,INT(EXS(I)),EXMWEI(I)
          ENDIF
        ENDIF
        DTEMP1=DTEMP1+EXSUMW(I)/EXS(I)
        DTEMP3=DTEMP3+ABS(EXSWSQ(I
     &      )-(EXSUMW(I)**2-EXSWSQ(I))/(EXS(I)-1.0D0))/EXS(I)/EXS(I)
        ITEMP1=ITEMP1+INT(EXS(I))
        ITEMP2=ITEMP2+IEXWNL(I)
        ITEMP4=ITEMP4+IEXWNG(I)
 20   CONTINUE
      WRITE(LMCWRT,1060) DTEMP1,SQRT(DTEMP3),ITEMP1
      WRITE(LMCWRT,1070) ITEMP2
      WRITE(LMCWRT,1080) ITEMP4
*
* Report on event generation
*
      WRITE(LMCWRT,1090)
      ITEMP3=0
      ITEMP4=0
      DO 30 I=1,NCONP
        CALL EXCFID(IPROC(I),PAR,ISTAT)
        IF (PAR(5).EQ.'gl') THEN
          WRITE(LMCWRT,1101) PAR,IGEVT(I),IFEVT(I)
        ELSE
          WRITE(LMCWRT,1100) PAR,IGEVT(I),IFEVT(I)
        ENDIF
        ITEMP3=ITEMP3+IGEVT(I)
        ITEMP4=ITEMP4+IFEVT(I)
 30   CONTINUE
      WRITE(LMCWRT,1110) ITEMP3,ITEMP4
      WRITE(LMCWRT,1120) ITEMP1-ITEMP,ITEMP3
      IF (DTEMP1.GT.0.0D0) THEN
        WRITE(LMCWRT,1130) DBLE(ITEMP3)/DTEMP1
      ELSE
        WRITE(LMCWRT,1130) 0.0D0
      ENDIF
*
* FSR summary
*
      CALL EXCFSR(2)
*
      ISTAT=0
*
 999  RETURN
 1005 FORMAT(//,1X,72('-'),/,21X,'EXCALIBUR Monte Carlo Termination',/)
 1040 FORMAT(1X,' EXCEND: Integration using ALL generated phasespace',
     &  ' points gave the',/,10X,'following weights: ',//,3X,
     &  'Final state',6X,'Cross-section (pb)',3X,'Redundancy',4X,
     &  '# Events',2X,'Max weight')
 1050 FORMAT(2X,A2,' ',A2,'~ ',A2,' ',A2,'~',1X,F10.6,' +/-',F10.6,1X
     &  ,F10.6,3X,I8,1X,G12.6)
 1051 FORMAT(2X,A2,' ',A2,'~ ',A2,' ',A2,'~',1X,G10.4,' +/-',G10.4,1X
     &  ,F10.6,3X,I8,3X,G10.4)
 1052 FORMAT(2X,A2,' ',A2,'~ ',A2,' ',A2,' ',1X,F10.6,' +/-',F10.6,1X
     &  ,F10.6,3X,I8,1X,G12.6)
 1053 FORMAT(2X,A2,' ',A2,'~ ',A2,' ',A2,' ',1X,G10.4,' +/-',G10.4,1X
     &  ,F10.6,3X,I8,3X,G10.4)
 1060 FORMAT(2X,73('-'),/,11X,F15.6,' +/-',F10.6,12X,I10,//)
 1070 FORMAT(/,2X,'Number of events with zero weight: ',I8,/)
 1080 FORMAT(/,2X,'Number of events with negative weight: ',I8,/)
 1090 FORMAT('  EXCEND: Event generation:',4X,'# Events',4X,'# Failed')
 1100 FORMAT(10X,A2,' ',A2,'~ ',A2,' ',A2,'~',10X,I6,4X,I6)
 1101 FORMAT(10X,A2,' ',A2,'~ ',A2,' ',A2,' ',10X,I6,4X,I6)
 1110 FORMAT(10X,40('-'),/,31X,I8,4X,I6)
 1120 FORMAT(/,1X,'In total ',I10,' phasespace points were used to ',
     &  'generate the ',I8,' events.',/)
 1130 FORMAT(1X,'Total integrated luminosity for this run: ',G12.5
     &  ,' (pb)^(-1).',/)
      END
*     
      SUBROUTINE SWAPCT(IP,I,J,CUTTMP)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Jun-96
*  CALLED    : EXCINI
*  CALLS     : (none)
*
*------------------------------------------
      IMPLICIT NONE
* Arguments
      INTEGER IP,I,J
      DOUBLE PRECISION CUTTMP(26,153)
* Local variables
      INTEGER H,II(2,2),I1
      DOUBLE PRECISION DTEMP
*
      DTEMP=CUTTMP(I-1,IP)
      CUTTMP(I-1,IP)=CUTTMP(J-1,IP)
      CUTTMP(J-1,IP)=DTEMP
      DTEMP=CUTTMP(I+9,IP)
      CUTTMP(I+9,IP)=CUTTMP(J+9,IP)
      CUTTMP(J+9,IP)=DTEMP
      DTEMP=CUTTMP(I+13,IP)
      CUTTMP(I+13,IP)=CUTTMP(J+13,IP)
      CUTTMP(J+13,IP)=DTEMP
      IF (I*10+J.EQ.34) THEN
        II(1,1)=7
        II(2,1)=9
        II(1,2)=8
        II(2,2)=10
      ELSEIF (I*10+J.EQ.35) THEN
        II(1,1)=6
        II(2,1)=9
        II(1,2)=8
        II(2,2)=11
      ELSEIF (I*10+J.EQ.36) THEN
        II(1,1)=6
        II(2,1)=10
        II(1,2)=7
        II(2,2)=11
      ELSEIF (I*10+J.EQ.45) THEN
        II(1,1)=6
        II(2,1)=7
        II(1,2)=10
        II(2,2)=11
      ELSEIF (I*10+J.EQ.46) THEN
        II(1,1)=6
        II(2,1)=8
        II(1,2)=9
        II(2,2)=11
      ELSEIF (I*10+J.EQ.56) THEN
        II(1,1)=7
        II(2,1)=8
        II(1,2)=9
        II(2,2)=10
      ENDIF
      I1=0
      DO H=1,4
        IF (H.GT.2) I1=14
        DTEMP=CUTTMP(II(1,H-2*INT(H/3))+I1,IP)
        CUTTMP(II(1,H-2*INT(H/3))+I1,IP)=CUTTMP(II(2,H-2*INT(H/3))+I1,IP
     &    )
        CUTTMP(II(2,H-2*INT(H/3))+I1,IP)=DTEMP
      ENDDO
      RETURN
      END
*
      SUBROUTINE EXSCUT(I,H,CUTTMP,SHCUT,ECUT,SCUT,CMAX,OMCMAX,FLFLAG)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Jun-96
*  CALLED    : EXCINI
*  CALLS     : (none)
*
*------------------------------------------
      IMPLICIT NONE
* Arguments
      INTEGER I,H,FLFLAG(153)
      INTEGER NIM
      PARAMETER (NIM=153)
      DOUBLE PRECISION SHCUT(NIM),SCUT(3:5,4:6,NIM),ECUT(3:6,NIM),
     +  CMAX(1:5,3:6,NIM),OMCMAX(1:5,3:6,NIM),CUTTMP(26,153)
* Local variables
      INTEGER I1,I2
*
      SHCUT(I)=CUTTMP(1,H)
      ECUT(3,I)=CUTTMP(2,H)
      ECUT(4,I)=CUTTMP(3,H)
      ECUT(5,I)=CUTTMP(4,H)
      ECUT(6,I)=CUTTMP(5,H)
      SCUT(3,4,I)=CUTTMP(6,H)
      SCUT(3,5,I)=CUTTMP(7,H)
      SCUT(3,6,I)=CUTTMP(8,H)
      SCUT(4,5,I)=CUTTMP(9,H)
      SCUT(4,6,I)=CUTTMP(10,H)
      SCUT(5,6,I)=CUTTMP(11,H)
      CMAX(1,3,I)=CUTTMP(12,H)
      CMAX(1,4,I)=CUTTMP(13,H)
      CMAX(1,5,I)=CUTTMP(14,H)
      CMAX(1,6,I)=CUTTMP(15,H)
      CMAX(2,3,I)=CUTTMP(16,H)
      CMAX(2,4,I)=CUTTMP(17,H)
      CMAX(2,5,I)=CUTTMP(18,H)
      CMAX(2,6,I)=CUTTMP(19,H)
      CMAX(3,4,I)=CUTTMP(20,H)
      CMAX(3,5,I)=CUTTMP(21,H)
      CMAX(3,6,I)=CUTTMP(22,H)
      CMAX(4,5,I)=CUTTMP(23,H)
      CMAX(4,6,I)=CUTTMP(24,H)
      CMAX(5,6,I)=CUTTMP(25,H)
      FLFLAG(I)=CUTTMP(26,H)
      DO 10 I1=1,5
        DO 20 I2=3,6
          IF(I1.LT.I2) OMCMAX(I1,I2,I)=1.0D0-CMAX(I1,I2,I)
 20     CONTINUE
 10   CONTINUE
*
      RETURN
      END
*
      SUBROUTINE EXCDFS(IPROC,IFPROC)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Mar-96
*  CALLED    : EXCRUN
*  CALLS     : (none)
*
*------------------------------------------
      IMPLICIT NONE
* Part of LUND common block
      INTEGER KCHG
      REAL    PMAS,PARF,VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
* Arguments
      INTEGER IPROC,IFPROC
* Local variables
      INTEGER ID(4),H,I,J,TEMP(3)
*
      J=0
      IFPROC=IPROC
      ID(1)=INT(IPROC/1000000)
      ID(2)=MOD(INT(IPROC/10000),100)
      ID(3)=MOD(INT(IPROC/100),100)
      ID(4)=MOD(INT(IPROC),100)
      IF (ID(1).EQ.ID(2).AND.ID(2).EQ.12.AND.ID(3).LT.7.AND.ID(4).LT.7)
     &  THEN
        IF (ID(3).EQ.ID(4).AND.ID(4).EQ.5) IFPROC=12120303
        IF (ID(3).NE.ID(4)) IFPROC=12120000+ID(3)*100+ID(3)
        RETURN
      ENDIF
      IF (ID(3).EQ.21.OR.ID(4).EQ.21) THEN
        IFPROC=(2-MOD(ID(1),2))*1000000+(2-MOD(ID(2),2))*10000+ID(3)*100
     &    +ID(4)
        RETURN
      ENDIF
      IF (ID(1).LT.7.AND.ID(3).LT.7.AND.ID(1).EQ.ID(2).AND.MOD(ID(1),2
     &  ).EQ.0.AND.MOD(ID(3),2).EQ.1) THEN
        I=ID(2)
        ID(2)=ID(4)
        ID(4)=I
        J=1
      ENDIF
      IF ((ID(1).GT.7.AND.ID(3).LT.7.AND.ID(3).NE.ID(4)).OR.(ID(1).LT.7
     &  .AND.ID(3).LT.7.AND.ID(1).NE.ID(2))) THEN
        DO 10 H=1,3,2
          I=H
          TEMP(H)=ID(I)*100+ID(I)-1
          IF (MOD(ID(I+1),2).EQ.0) THEN
            I=I+1
            TEMP(H)=(ID(I)-1)*100+ID(I)
          ENDIF
          IF (((ID(1).EQ.ID(4).AND.ID(2).NE.ID(3).AND.MOD(ID(1),2).EQ.0)
     &      .OR.(ID(2).EQ.ID(3).AND.ID(1).NE.ID(4).AND.MOD(ID(2),2).EQ.0
     &      )).AND.(MAX(ID(H),ID(H+1)).NE.MIN(ID(H),ID(H+1))+1.OR
     &      .MOD(MAX(ID(H),ID(H+1)),2).NE.0)) THEN
            IF (MAX(ID(H),ID(H+1)).EQ.5.OR.(MAX(ID(4-H),ID(5-H)).EQ
     &        .MIN(ID(4-H),ID(5-H))+1).AND.MOD(MAX(ID(4-H),ID(5-H)),2)
     &        .EQ.0) THEN
              TEMP(H)=ABS(ID(H)-6)*100+ABS(ID(H)-6)-1
              IF (MOD(ID(H),2).NE.0) TEMP(H)=(ABS(ID(H+1)-6)-1)*100
     &          +ABS(ID(H+1)-6)
            ENDIF
          ENDIF
 10     CONTINUE
        IFPROC=TEMP(1)*10000+TEMP(3)
      ENDIF
      IF (J.GT.0) THEN
        ID(1)=INT(IFPROC/1000000)
        ID(2)=MOD(INT(IFPROC/10000),100)
        ID(3)=MOD(INT(IFPROC/100),100)
        ID(4)=MOD(INT(IFPROC),100)
        I=ID(2)
        ID(2)=ID(4)
        ID(4)=I
        IFPROC=ID(1)*1000000+ID(2)*10000+ID(3)*100+ID(4)
      ENDIF
 999  RETURN
      END
*
      SUBROUTINE EXQCDC(QCD)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 14-May-96
*  CALLED    : EXCRUN
*  CALLS     : (none)
*
*------------------------------------------
      IMPLICIT NONE
* Commons
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
* Additional commons
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
* Arguments
      DOUBLE PRECISION QCD(2)
* Local variables
      DOUBLE PRECISION W,COR1,ALPMY,SCALE,ALPS,DTEMP,NF,BETA0,BETA1
     &  ,BETA2,LAMBDA,COR2,COR3
      DOUBLE PRECISION PI
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (NF=5.0D0,BETA0=(33.0D0-2.0D0*NF)/12.0D0/PI,BETA1=(153
     &  .0D0-19.0D0*NF)/24.0D0/PI/PI)
* Function
      W(SCALE)=1.0D0-BETA0*EXALSZ*LOG(ZMI*ZMI/SCALE/SCALE)
      ALPMY(SCALE)=EXALSZ*(1.0D0-BETA1*EXALSZ*LOG(W(SCALE))/BETA0
     &  /W(SCALE))/W(SCALE)
      COR1(ALPS)=ALPS/PI
      COR2(ALPS)=ALPS/PI+1.409D0*ALPS*ALPS/PI/PI
      COR3(ALPS)=ALPS/PI+1.409D0*ALPS*ALPS/PI/PI-12.77D0*ALPS*ALPS*ALPS
     &  /PI/PI/PI
*
      QCD(1)=0.0D0
      QCD(2)=0.0D0
      IF (IEXQCD.GE.5) THEN
        QCD(1)=COR3(EXALSZ)
* Run alpha_s from Mz to Mw (small effect, but anyway...)
        QCD(2)=COR3(ALPMY(WMI))
      ELSEIF (IEXQCD.GE.3) THEN
        QCD(1)=COR2(EXALSZ)
* Run alpha_s from Mz to Mw (small effect, but anyway...)
        QCD(2)=COR2(ALPMY(WMI))
      ELSE
        QCD(1)=COR1(EXALSZ)
* Run alpha_s from Mz to Mw (small effect, but anyway...)
        QCD(2)=COR1(ALPMY(WMI))
      ENDIF
      QCD(1)=SQRT(1.0D0+QCD(1))
      QCD(2)=SQRT(1.0D0+QCD(2))
      IF (MOD(IEXQCD,2).EQ.1) QCD(1)=1.0D0
      RETURN
      END
*
      SUBROUTINE EXCDCH(NC,NN)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 08-Mar-96
*  CALLED    : EXCRUN
*  CALLS     : (none)
*
*------------------------------------------
      IMPLICIT NONE
* Original commons/vars
      common /area7/kf(1:4,1:24)
      integer kf
* Additional commons...
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
* Local variables
      INTEGER I,ID(4),NN,NC
*
      IEXCCF=1
      I=NN-24*INT(NN/24.1)
      ID(1)=KF(1,I)
      ID(2)=KF(2,I)
      ID(3)=KF(3,I)
      ID(4)=KF(4,I)
      IF (((NC.GE.1.AND.NC.LE.17).OR.NC.EQ.21)
     &  .AND.((MOD(MAX(ID(1),ID(2)),2).EQ.0.AND.ABS(ID(1)-ID(2)).GT.1)
     &  .OR.MOD(MAX(ID(1),ID(2)),2).NE.0)) THEN
        IEXCCF=2
      ELSEIF (((NC.GE.18.AND.NC.LE.20).OR.(NC.GE.22.AND.NC.LE.25))
     &    .AND.((MOD(MAX(ID(2),ID(3)),2).EQ.0.AND.ABS(ID(2)-ID(3))
     &    .GT.1).OR.MOD(MAX(ID(2),ID(3)),2).NE.0)) THEN
        IEXCCF=2
      ELSEIF (NC.EQ.26.OR.NC.EQ.27.OR.NC.EQ.28) THEN
        IEXCCF=0
      ENDIF
 999  RETURN
      END
*
      SUBROUTINE EXCFID(ICODE,CHPAR,ISTAT)
*------------------------------------------
*
*  AUTHOR    : D.Charlton
*  CREATED   : 11-Aug-95
*  CALLED    : EXCINI EXCEVT EXCEND
*  CALLS     : (none)
*  MODIFIED  : 01-Feb-96 (J.B.Hansen)
*
*------------------------------------------
      IMPLICIT NONE
* Commons
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
* Arguments
      INTEGER ICODE,ISTAT
      CHARACTER*2 CHPAR(4)
* Local variables
      INTEGER I1,I2,I3,I4
      CHARACTER*2 CHEXCP(21)
      SAVE CHEXCP
      DATA CHEXCP/'dq','uq','sq','cq','bq','tq','xx','xx','xx','xx',
     +  'el','ne','mu','nm','ta','nt','xx','xx','xx','xx','gl'/
*
      ISTAT=9
      I1=INT(ICODE/1000000)
      I2=MOD(INT(ICODE/10000),100)
      I3=MOD(INT(ICODE/100),100)
      I4=MOD(INT(ICODE),100)
      IF (I1.LT.1.OR.(I1.GT.5.AND.I1.LT.11).OR.I1.GT.16) THEN
        WRITE(LMCWRT,1010) ICODE
        ISTAT=8
        GOTO 999
      ENDIF
      IF (I2.LT.1.OR.(I2.GT.5.AND.I2.LT.11).OR.I2.GT.16) THEN
        WRITE(LMCWRT,1010) ICODE
        ISTAT=7
        GOTO 999
      ENDIF
      IF (I3.LT.1.OR.(I3.GT.5.AND.I3.LT.11).OR.(I3.GT.16.AND.I3.LT.21)
     &  .OR.I3.GT.21) THEN
        WRITE(LMCWRT,1010) ICODE
        ISTAT=6
        GOTO 999
      ENDIF
      IF (I4.LT.1.OR.(I4.GT.5.AND.I4.LT.11).OR.(I4.GT.16.AND.I4.LT.21)
     &  .OR.I4.GT.21) THEN
        WRITE(LMCWRT,1010) ICODE
        ISTAT=5
        GOTO 999
      ENDIF
      CHPAR(1)=CHEXCP(I1)
      CHPAR(2)=CHEXCP(I2)
      CHPAR(3)=CHEXCP(I3)
      CHPAR(4)=CHEXCP(I4)
*
      ISTAT=0
*
 999  RETURN
 1010 FORMAT(' ERROR: Invalid final-state code:',I10)
      END
*
      SUBROUTINE EXCFSR(ISTATE)
*------------------------------------------
*! EXCFSR Excalibur FSR interface
*
*  AUTHOR    : A.Waananen
*  CREATED   : 29-Feb-96
*  CALLED    : EXCINI EXCEVT EXCEND
*  CALLS     : PHOINI PHOTOS PHOREP
*              HEPCPY HEPDEL
*      
*------------------------------------------
      IMPLICIT NONE
*
C
C Old Standard HEPEVT common block (single precision)
C
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL    PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C special interface for marking particles not allowed to radiate
C
      LOGICAL QEDRAD
      COMMON /PHOQED/ QEDRAD(NMXHEP)
* FINALG Communication common block
      DOUBLE PRECISION WMAX,WMXX,POUT,PIN
      INTEGER INQ,NC,NN,IDEB,NNG
      COMMON/TALK/WMAX,WMXX,POUT(50,6),PIN(50,6),INQ(50)
     *           ,NC,NN,IDEB,NNG
* Additional Commons
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
* Local variables
      INTEGER ISTATE,KF,KFA,I,J,NCHAR,NNEUT
      INTEGER ISTAT,NISR,NFSR1,NFSR2,IFOF,IBPT1,IBPT2
      INTEGER FP(20),IGPT
      DOUBLE PRECISION P1(0:3),PTMP(0:3),PNEW(0:3),SUMMAS
* Charge*3 of the 25 first LUND particles
      INTEGER KFQ(25)
      DATA KFQ
     &   /-1,2,-1,2,-1,2,-1,2,0,0,-3,0,-3,0,-3,0,-3,0,0,0,0,0,0,3,0/
*
      IF (ISTATE.EQ.0) THEN
* Inititialisation
        IF (IEXFSR.EQ.1) THEN
          CALL PHOINI
        ENDIF
* Executing
      ELSEIF (ISTATE.EQ.1) THEN
* Generate FSR ?
        IF (IEXFSR.GT.0) THEN
* Calc # of ISR photons
          NISR = 0
          IFOF = 4
          IF (IDHEP(IFOF+1).EQ.22) NISR = 1
          IF (IDHEP(IFOF+2).EQ.22) NISR = 2
          IFOF = 4 + NISR
* Only FSR if lepton or muon in final state
          IF((IDHEP(IFOF+1).GE.11.AND.IDHEP(IFOF+1).LE.14).OR.
     &       (IDHEP(IFOF+3).GE.11.AND.IDHEP(IFOF+3).LE.14))THEN
*
* PHOTOS
            IF (IEXFSR.EQ.1) THEN
* Make a space in the HEPEVT common before each fermion pairs
              CALL HEPCPY(IFOF+4,IFOF+6,ISTAT)
              CALL HEPCPY(IFOF+3,IFOF+5,ISTAT)
              CALL HEPCPY(IFOF+2,IFOF+3,ISTAT)
              CALL HEPCPY(IFOF+1,IFOF+2,ISTAT)
* Put two pseudo bosons in the holes
              IBPT1 = IFOF + 1
              IBPT2 = IFOF + 4
* Boson 1 (momenta and vertex)
              IDHEP(IBPT1) = 23
              IF(IDHEP(IBPT1+1)+IDHEP(IBPT1+2).GT.0) IDHEP(IBPT1) =  24
              IF(IDHEP(IBPT1+1)+IDHEP(IBPT1+2).LT.0) IDHEP(IBPT1) = -24
*
              PHEP(1,IBPT1)=PHEP(1,IBPT1+1)+PHEP(1,IBPT1+2)
              PHEP(2,IBPT1)=PHEP(2,IBPT1+1)+PHEP(2,IBPT1+2)
              PHEP(3,IBPT1)=PHEP(3,IBPT1+1)+PHEP(3,IBPT1+2)
              PHEP(4,IBPT1)=PHEP(4,IBPT1+1)+PHEP(4,IBPT1+2)
              PHEP(5,IBPT1)=SQRT(PHEP(5,IBPT1+1)**2+PHEP(5,IBPT1+2)**2+
     &           MAX(0.0D0,2.0d0*(PHEP(4,IBPT1+1)*PHEP(4,IBPT1+2)
     &             -PHEP(1,IBPT1+1)*PHEP(1,IBPT1+2)
     &             -PHEP(2,IBPT1+1)*PHEP(2,IBPT1+2)
     &             -PHEP(3,IBPT1+1)*PHEP(3,IBPT1+2))))
              DO 27 I=1,4
                VHEP(I,IBPT1)=REAL(VERTEX(I))
 27           CONTINUE
              ISTHEP(IBPT1)=3
              JMOHEP(1,IBPT1)=0
              JMOHEP(2,IBPT1)=0
              JDAHEP(1,IBPT1)=IBPT1+1
              JDAHEP(2,IBPT1)=IBPT1+2
*
              JMOHEP(1,IBPT1+1)=IBPT1
              JMOHEP(2,IBPT1+1)=0
              JMOHEP(1,IBPT1+2)=IBPT1
              JMOHEP(2,IBPT1+2)=0
* Boson 2
              IDHEP(IBPT2) = 23
              IF(IDHEP(IBPT2+1)+IDHEP(IBPT2+2).GT.0) IDHEP(IBPT2) =  24
              IF(IDHEP(IBPT2+1)+IDHEP(IBPT2+2).LT.0) IDHEP(IBPT2) = -24
*
              PHEP(1,IBPT2)=PHEP(1,IBPT2+1)+PHEP(1,IBPT2+2)
              PHEP(2,IBPT2)=PHEP(2,IBPT2+1)+PHEP(2,IBPT2+2)
              PHEP(3,IBPT2)=PHEP(3,IBPT2+1)+PHEP(3,IBPT2+2)
              PHEP(4,IBPT2)=PHEP(4,IBPT2+1)+PHEP(4,IBPT2+2)
              PHEP(5,IBPT2)=SQRT(PHEP(5,IBPT2+1)**2+PHEP(5,IBPT2+2)**2+
     &           MAX(0.0D0,2.0D0*(PHEP(4,IBPT2+1)*PHEP(4,IBPT2+2)
     &             -PHEP(1,IBPT2+1)*PHEP(1,IBPT2+2)
     &             -PHEP(2,IBPT2+1)*PHEP(2,IBPT2+2)
     &             -PHEP(3,IBPT2+1)*PHEP(3,IBPT2+2))))
              DO 28 I=1,4
                VHEP(I,IBPT2)=REAL(VERTEX(I))
 28           CONTINUE
              ISTHEP(IBPT2)=3
              JMOHEP(1,IBPT2)=0
              JMOHEP(2,IBPT2)=0
              JDAHEP(1,IBPT2)=IBPT2+1
              JDAHEP(2,IBPT2)=IBPT2+2
*
              JMOHEP(1,IBPT2+1)=IBPT2
              JMOHEP(2,IBPT2+1)=0
              JMOHEP(1,IBPT2+2)=IBPT2
              JMOHEP(2,IBPT2+2)=0
* Allow only electrons and muons (with mass greater than 0) to produce FSR via PHOTOS
              DO 10 I = 1, NHEP
                KF = IABS(IDHEP(I))
                QEDRAD(I) = ((KF.EQ.11).OR.(KF.EQ.13)).AND.
     &             REAL(PHEP(5,I)).GT.0.0
 10           CONTINUE
              NFSR1 = NHEP
* Call Photos only if one of daughters can radiate
              IF(QEDRAD(JDAHEP(1,IBPT1)).OR.QEDRAD(JDAHEP(2,IBPT1)))
     &           CALL PHOTOS(IBPT1)
              NFSR1 = NHEP-NFSR1
* Allow only electrons and muons to produce ISR in Photos
              DO 15 I = 1, NHEP
                KF = IABS(IDHEP(I))
                QEDRAD(I) = ((KF.EQ.11).OR.(KF.EQ.13)).AND.
     &             REAL(PHEP(5,I)).GT.0.0
 15           CONTINUE
* Call PHOTOS with second pseudo boson
              NFSR2 = NHEP
* Call Photos only if one of daughters can radiate
              IF(QEDRAD(JDAHEP(1,IBPT2+NFSR1)).OR.QEDRAD(JDAHEP(2,IBPT2
     &          +NFSR1))) CALL PHOTOS(IBPT2+NFSR1)
              NFSR2 = NHEP-NFSR2
* Delete the "bosons"
              CALL HEPDEL(IBPT2+NFSR1,ISTAT)
              CALL HEPDEL(IBPT1,ISTAT)
              IF (NFSR1.GT.0) THEN
                IEXOFP(8+NISR+NFSR1) = IEXOFP(8+NISR)
                IEXOFP(8+NISR) = 0
                IEXOFP(7+NISR+NFSR1) = IEXOFP(7+NISR)
                IEXOFP(7+NISR) = 0
              ENDIF
*
* FINALG
            ELSEIF (IEXFSR.EQ.2) THEN
* Return if all fermion masses are zero
              SUMMAS = 0.0D0
              DO I = IFOF+1, IFOF+4
                SUMMAS = SUMMAS + DBLE(PHEP(5,I))
              ENDDO
              IF (SUMMAS.EQ.0.0D0) GOTO 80
*
* Debug printing option (0 or 1)
              IDEB = 0
              IF (IEXDBG.GT.0) IDEB = 1
* NNG - no of FSR gammas
              NNG = IEXNNG
* WMXX will be filled with the max. observed weight.
              WMXX = 0.D0
* preset max. weight:
              WMAX = 3.D0*(1+NNG)
* take care of beam particles and ISR photons
              NCHAR = 2
              NNEUT = 0
              CALL VZERO(INQ(1),6)
* Set charge
              IFOF = 4 + NISR
              DO 20 I = IFOF+1, IFOF+4
                IF (ISTHEP(I).NE.3) THEN
                  KF  = IDHEP(I)
                  KFA = ABS(KF)
                  IF ( KFQ(KFA).NE.0 )  THEN
                    NCHAR = NCHAR + 1
                    IF ( (KFA.EQ.11.OR.KFA.EQ.13).AND.
     &                 REAL(PHEP(5,I)).GT.0.0) THEN
                      INQ(NCHAR) = KF/KFA
                    ELSE
                      INQ(NCHAR) = 0
                    ENDIF
                  ELSE
                    NNEUT = NNEUT + 1
                    INQ(NNEUT) = 0
                  ENDIF
                ENDIF
 20           CONTINUE
* Transfer momenta - first beam particles (after ISR)
              DO 25 I = 1,2
                PIN(I,1)=DBLE(PHEP(1,I+2))
                PIN(I,2)=DBLE(PHEP(2,I+2))
                PIN(I,3)=DBLE(PHEP(3,I+2))
                PIN(I,4)=DBLE(PHEP(4,I+2))
                PIN(I,5)=DBLE(PHEP(5,I+2))
                PIN(I,6)=0.0D0
 25           CONTINUE
              J = 2
* Then the charged final state fermions
              DO 31 I = IFOF+1, IFOF+4
                KFA = ABS(KFQ(ABS(IDHEP(I))))
                IF( KFA.GT.0 ) THEN
                  J = J + 1
                  FP(I) = J
                  PIN(J,1)=DBLE(PHEP(1,I))
                  PIN(J,2)=DBLE(PHEP(2,I))
                  PIN(J,3)=DBLE(PHEP(3,I))
                  PIN(J,4)=DBLE(PHEP(4,I))
                  PIN(J,5)=DBLE(PHEP(5,I))
                  PIN(J,6)=0.0D0
                ENDIF
 31           CONTINUE
* Then the neutral final state fermions
              DO 32 I = IFOF+1, IFOF+4
                KFA = ABS(KFQ(ABS(IDHEP(I))))
                IF(KFA.EQ.0 ) THEN
                  J = J + 1
                  FP(I) = J
                  PIN(J,1)=DBLE(PHEP(1,I))
                  PIN(J,2)=DBLE(PHEP(2,I))
                  PIN(J,3)=DBLE(PHEP(3,I))
                  PIN(J,4)=DBLE(PHEP(4,I))
                  PIN(J,5)=DBLE(PHEP(5,I))
                  PIN(J,6)=0.0D0
                ENDIF
 32           CONTINUE
              NC = NCHAR
              NN = NNEUT
*Boost
* e+e-(after ISR) frame
              P1(0) = PIN(1,4) + PIN(2,4)
              P1(1) = PIN(1,1) + PIN(2,1)
              P1(2) = PIN(1,2) + PIN(2,2)
              P1(3) = PIN(1,3) + PIN(2,3)
*
* Boost e+e- and fermions
              DO I = 1, NC+NN
                PTMP(0) = PIN(I,4)
                PTMP(1) = PIN(I,1)
                PTMP(2) = PIN(I,2)
                PTMP(3) = PIN(I,3)
                CALL BOOST(1,P1,PNEW,PTMP)
                PIN(I,4) = PNEW(0)
                PIN(I,1) = PNEW(1)
                PIN(I,2) = PNEW(2)
                PIN(I,3) = PNEW(3)
              ENDDO
*
              CALL FINALG
*
              DO I = 3, NC+NN+NNG
                PTMP(0) = POUT(I,4)
                PTMP(1) = POUT(I,1)
                PTMP(2) = POUT(I,2)
                PTMP(3) = POUT(I,3)
                CALL BOOST(0,P1,PTMP,PNEW)
                POUT(I,4) = PNEW(0)
                POUT(I,1) = PNEW(1)
                POUT(I,2) = PNEW(2)
                POUT(I,3) = PNEW(3)
              ENDDO
* Return corrected fermion momenta
              DO 60 I = IFOF+1,IFOF+4
                PHEP(1,I)=REAL(POUT(FP(I),1))
                PHEP(2,I)=REAL(POUT(FP(I),2))
                PHEP(3,I)=REAL(POUT(FP(I),3))
                PHEP(4,I)=REAL(POUT(FP(I),4))
                PHEP(5,I)=REAL(POUT(FP(I),5))
 60           CONTINUE
* Add FSR photons
              NHEP = NHEP + NNG
              DO 70 I = 1, NNG
                IGPT = IFOF + 4 + I
                IDHEP(IGPT)    = 22
                ISTHEP(IGPT)   = 1
                JMOHEP(1,IGPT) = 0
                JMOHEP(2,IGPT) = 0
                JDAHEP(1,IGPT) = 0
                JDAHEP(2,IGPT) = 0
                PHEP(1,IGPT)=REAL(POUT(NC+NN+I,1))
                PHEP(2,IGPT)=REAL(POUT(NC+NN+I,2))
                PHEP(3,IGPT)=REAL(POUT(NC+NN+I,3))
                PHEP(4,IGPT)=REAL(POUT(NC+NN+I,4))
                PHEP(5,IGPT)=REAL(POUT(NC+NN+I,5))
 70           CONTINUE
 80           CONTINUE
            ELSE
              WRITE(LMCWRT,*) 'EXCFSR: Unknown FSR generator!!'
            ENDIF
          ENDIF
        ENDIF
      ELSEIF (ISTATE.EQ.2) THEN
* Summary
        IF (IEXFSR.EQ.1) CALL PHOREP
      ELSE
        WRITE(LMCWRT,*) 'EXCFSR: Unknown action!!'
      ENDIF
*
      RETURN
      END
*
      SUBROUTINE HEPCPY(ORIG,DEST,ISTAT)
C-----------------------------------------------------------------------
C! Copy entry ORIG to entry DEST in HEPVT common block.
C  Creates new entry if necessary.
C  The routine also copies the mother and daughter relationship of the
C  original entry. These entries probably needs to be changed. After the
C  copy.
C  12-MAR-96 A.Waananen
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
C Old Standard HEPEVT common block (single precision)
C
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL    PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
      INTEGER ORIG,DEST,ISTAT,I
*
      ISTAT = 0
      IF (ORIG.LT.1.OR.ORIG.GT.NHEP) THEN
        ISTAT = 1
        GOTO 999
      ENDIF
*
      NHEP = MAX(NHEP,DEST)
*
      IDHEP(DEST)    = IDHEP(ORIG)
      ISTHEP(DEST)   = ISTHEP(ORIG)
*
      JMOHEP(1,DEST) = JMOHEP(1,ORIG)
      JMOHEP(2,DEST) = JMOHEP(2,ORIG)
      JDAHEP(1,DEST) = JDAHEP(1,ORIG)
      JDAHEP(2,DEST) = JDAHEP(2,ORIG)
*
      DO 20 I=1,5
        PHEP(I,DEST) = PHEP(I,ORIG)
 20   CONTINUE
*
      DO 25 I=1,4
        VHEP(I,DEST) = VHEP(I,ORIG)
 25   CONTINUE
*
 999  CONTINUE
*
      RETURN
      END
*
      SUBROUTINE HEPDEL(ORIG,ISTAT)
C-----------------------------------------------------------------------
C! Delete entry ORIG in HEPVT common block.
C  Creates new entry if necessary. The routine also copies the mother 
C  and daughter relationship of the original entry. These entries 
C  probably needs to be changed, after the copy.
C
C  Created: 12-MAR-96 A.Waananen
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
C Old Standard HEPEVT common block (single precision)
C
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL    PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
      INTEGER ORIG,ISTAT,I
*
      ISTAT = 0
      IF (ORIG.LT.1.OR.ORIG.GT.NHEP) THEN
        ISTAT = 1
        GOTO 999
      ENDIF
*
      DO 30 I=1,NHEP
        IF (JMOHEP(1,I).EQ.ORIG) JMOHEP(1,I) = 0
        IF (JMOHEP(2,I).EQ.ORIG) JMOHEP(2,I) = 0
 30   CONTINUE
*
      DO 35 I=ORIG+1,NHEP
        CALL HEPCPY(I,I-1,ISTAT)
 35   CONTINUE
*      
      NHEP = NHEP - 1
*
 999  CONTINUE
*
      RETURN
      END
*
      SUBROUTINE HEPLST(UNIT,LVL)
C-----------------------------------------------------------------------
C! List HEPEVT common block, much like LULIST.
C
C  12-MAR-96 A.Waananen
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
C Old Standard HEPEVT common block (single precision)
C
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL    PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
*
      INTEGER UNIT,LVL,I,J,K
      REAL PSUM(5)
      CHARACTER*16 NAME
      CHARACTER*12 NAM
*      
      DO 3 I=1,5
      PSUM(I) = 0.0
 3    CONTINUE
*      
      WRITE(UNIT,15)
      IF (LVL.LE.1) THEN
        WRITE(UNIT,16)
      ELSEIF (LVL.EQ.2) THEN
        WRITE(UNIT,17)
      ENDIF
*      
      DO 10 I = 1, NHEP
        CALL LUNAME(IDHEP(I),NAME)
        DO 5 K=1,12
          NAM(K:K)=NAME(K:K)
 5      CONTINUE
        IF (LVL.EQ.1) THEN
          WRITE(UNIT,21) I,NAM,ISTHEP(I),IDHEP(I),
     &       JMOHEP(1,I),JMOHEP(2,I),JDAHEP(1,I),JDAHEP(2,I),
     &       (PHEP(J,I),J=1,5)
        ELSEIF (LVL.EQ.2) THEN
          WRITE(UNIT,22) I,NAM,ISTHEP(I),IDHEP(I),
     &       JMOHEP(1,I),JMOHEP(2,I),JDAHEP(1,I),JDAHEP(2,I),
     &       (PHEP(J,I),J=1,5),(VHEP(J,I),J=1,3)
        ENDIF
        DO 7 J = 1,4
          IF (ISTHEP(I).EQ.1) PSUM(J) = PSUM(J) + REAL(PHEP(J,I))
 7      CONTINUE
        PSUM(5) = PSUM(4)*PSUM(4)-
     &    PSUM(3)*PSUM(3)-PSUM(2)*PSUM(2)-PSUM(1)*PSUM(1)
        PSUM(5) = SQRT(MAX(PSUM(5),0.0))
 10   CONTINUE
      WRITE(UNIT,30) (PSUM(J),J=1,5)
*      
 15   FORMAT(//,30X,'HEPEVT Event listing (summary)')
 16   FORMAT(/,1X,3X,'I','  particle/jet',
     &   ' KS','     KF','  Moth1','  Moth2','   Dau1','   Dau2',
     &   '       p_x','       p_y','       p_z','         E',
     &   '         m',/)
 17   FORMAT(/,1X,3X,'I','  particle/jet',
     &   ' KS','     KF','  Moth1','  Moth2','   Dau1','   Dau2',
     &   '       p_x','       p_y','       p_z','         E',
     &   '         m','       V_x','       V_y','       V_z',/)
 21   FORMAT(1X,I4,2X,A12,I3,5I7,5F10.4)
 22   FORMAT(1X,I4,2X,A12,I3,5I7,5F10.4,3F10.4)
 30   FORMAT(1X,18X,'sum:',34X,5F10.4)
*
      RETURN
      END
*
      SUBROUTINE EXFRAG(ISTAT)
*------------------------------------------
*
*  AUTHOR    : A.Waananen
*  CREATED   : 01-Feb-96
*  CALLED    : EXCEVT
*  CALLS     : LUHEPC LUJOIN LUSHOW LUEXEC LULIST
*
*------------------------------------------
      IMPLICIT NONE
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
C LUDAT1
      INTEGER MSTJ,MSTU
      REAL    PARJ,PARU
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C LUJETS
      INTEGER N,K
      REAL    P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)      
* Additional commons...
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
* Arguments
      INTEGER ISTAT
* Local variables
      INTEGER I,J,I1,I2,IFOS,INEVT
      INTEGER POLFAC
      INTEGER LUJ1,LUJ2
      COMMON /LUQPOS/ LUJ1(2),LUJ2(2)
      DOUBLE PRECISION DTEMP,EXCRAN,BET(0:16)
C
      ISTAT=0
C
C Translate from /HEPEVT/ to /LUJETS/ common blocks
C
      CALL LUHEPC(2)
C
C Return if not asked for hadronization
C 
      IF (IEXHAD.LE.0) GOTO 850
C      
C Fermions Offset in HEPEVT bank
      IFOS = 5
C ISR photons
      IF (K(IFOS,2).EQ.22) IFOS = IFOS + 1
      IF (K(IFOS,2).EQ.22) IFOS = IFOS + 1
      J = IFOS
      LUJ1(1) = J
      LUJ1(2) = J + 1
      J = J + 2
C FSR Photons from PHOTOS
      IF (K(J,2).EQ.22) J = J + 1
      IF (K(J,2).EQ.22) J = J + 1
      LUJ2(1) = J
      LUJ2(2) = J + 1
C
C Pass on tau polarization information for Tau decay packages
C (eg. TAUOLA)
C
      J = 0
      DO 10 I = 0, 3
        IEXTPL(I+1) = 0
        IF (I.LT.2) THEN
          IF (ABS(K(LUJ1(1+I),2)).EQ.15) THEN
            J = J + 1
            IEXTPL(J) = LUJ1(1+I)
          ENDIF
        ELSE
          IF (ABS(K(LUJ2(I-1),2)).EQ.15) THEN
            J = J + 1
            IEXTPL(J) = LUJ2(I-1)
          ENDIF
        ENDIF
 10   CONTINUE
      IF (J.GT.0) THEN
        BET(0) = 0.0D0
        POLFAC = 1
        IF (J.GT.1) THEN
          DO 30 I = 1, J
            DO 20 I1 = I+1, J
              IF (IEXOFP(IEXTPL(I)).GT.IEXOFP(IEXTPL(I1))) THEN
                I2=IEXTPL(I)
                IEXTPL(I)=IEXTPL(I1)
                IEXTPL(I1)=I2
              ENDIF
 20         CONTINUE
 30       CONTINUE
        ENDIF
        IF (J.EQ.1) THEN
          IF (IEXOFP(IEXTPL(1)).EQ.IFOS) BET(1) = EXHELI(1)+EXHELI(2)
     &      +EXHELI(3)+EXHELI(4)+EXHELI(5)+EXHELI(6)+EXHELI(7)+EXHELI(8)
          IF (IEXOFP(IEXTPL(1)).EQ.IFOS+1) BET(1) = EXHELI(1)+EXHELI(2)
     &      +EXHELI(3)+EXHELI(4)+EXHELI(9)+EXHELI(10)+EXHELI(11)
     &      +EXHELI(12)
          IF (IEXOFP(IEXTPL(1)).EQ.IFOS+2) BET(1) = EXHELI(1)+EXHELI(2
     &      )+EXHELI(5)+EXHELI(7)+EXHELI(9)+EXHELI(10)+EXHELI(13)
     &      +EXHELI(15)
          IF (IEXOFP(IEXTPL(1)).EQ.IFOS+3) BET(1) = EXHELI(1)+EXHELI(3)
     &      +EXHELI(4)+EXHELI(6)+EXHELI(9)+EXHELI(11)+EXHELI(13)
     &      +EXHELI(14)
          BET(1) = BET(1)/(EX1234+EX1432+EXINT)
          BET(2) = 1.0D0
        ELSEIF (J.EQ.2) THEN
          IF (IEXOFP(IEXTPL(1)).EQ.IFOS) THEN
            BET(1) = EXHELI(1)+EXHELI(2)+EXHELI(3)+EXHELI(4)
            BET(2) = BET(1) + EXHELI(5)+EXHELI(6)+EXHELI(7)+EXHELI(8)
            BET(3) = BET(2) + EXHELI(9)+EXHELI(10)+EXHELI(11)+EXHELI(12)
          ELSE
            BET(1) = EXHELI(1)+EXHELI(4)+EXHELI(9)+EXHELI(13)
            BET(2) = BET(1) + EXHELI(2)+EXHELI(7)+EXHELI(10)+EXHELI(15)
            BET(3) = BET(2) + EXHELI(3)+EXHELI(6)+EXHELI(11)+EXHELI(14)
          ENDIF
          DO 40 I = 1, 3
            BET(I) = BET(I)/(EX1234+EX1432+EXINT)
 40       CONTINUE
          BET(4) = 1.0D0
        ELSEIF (J.EQ.4) THEN
          DO 50 I = 1, 15
            BET(I) = BET(I-1)+EXHELI(I)/(EX1234+EX1432+EXINT)
 50       CONTINUE
          BET(16) = 1.0D0
        ENDIF
        DTEMP=EXCRAN(1)
        DO 60 I = 1, 2**J
          IF (DTEMP.GT.BET(I-1).AND.DTEMP.LE.BET(I)) GOTO 4
 60     CONTINUE
 4      CONTINUE
        IF (J.EQ.1) THEN
          IEXTPL(1) = -IEXTPL(1)*POLFAC
          IF (I.EQ.1) IEXTPL(1) = ABS(IEXTPL(1))*POLFAC
        ELSEIF (J.EQ.2) THEN
          IEXTPL(1) = IEXTPL(1)*POLFAC
          IEXTPL(2) = IEXTPL(2)*POLFAC
          IF (I.GT.2) IEXTPL(1) = -ABS(IEXTPL(1))*POLFAC
          IF (I.EQ.2.OR.I.EQ.4) IEXTPL(2) = -ABS(IEXTPL(2))*POLFAC
        ELSEIF (J.EQ.4) THEN
          IEXTPL(1) = IEXTPL(1)*POLFAC
          IEXTPL(2) = IEXTPL(2)*POLFAC
          IEXTPL(3) = IEXTPL(3)*POLFAC
          IEXTPL(4) = IEXTPL(4)*POLFAC
          IF (I.GT.8) IEXTPL(1) = -ABS(IEXTPL(1))*POLFAC
          IF ((I.GT.4.AND.I.LT.9).OR.I.GT.12) IEXTPL(2) = -ABS(IEXTPL(2)
     &      )*POLFAC
          IF (I.EQ.3.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.8.OR.I.EQ.11.OR.I.EQ.12
     &      .OR.I.EQ.14.OR.I.EQ.16) IEXTPL(3) = -ABS(IEXTPL(3))*POLFAC
          IF (I.EQ.2.OR.I.EQ.4.OR.I.EQ.7.OR.I.EQ.8.OR.I.EQ.10.OR.I.EQ.12
     &      .OR.I.EQ.15.OR.I.EQ.16) IEXTPL(4) = -ABS(IEXTPL(4))*POLFAC
        ENDIF
      ENDIF
C
C Reconnect and fragment the event 
C
      CALL EXCR(IEXFCR,IEXCRC,WM,WW,ISTAT)
C
C debug some events
C
  850 CONTINUE
      INEVT = 0
      DO 100 I=1,NCONP
        INEVT=INEVT+IGEVT(I)
 100  CONTINUE
      IF (INEVT.LT.IEXNDB) CALL LULIST(1)
      IF (MSTU(24).GT.0) THEN
        WRITE(LMCWRT,1000) MSTU(24),INEVT
        CALL LULIST(1)
        ISTAT=1
      ENDIF
C
      RETURN
 1000 FORMAT(/,' =EXFRAG= Error code ',I2,' in LUEXEC at event #',I10,/)
      END
*      
      INTEGER FUNCTION IPAIR(IDUM)
*------------------------------------------
*
*  AUTHOR    : D.Charlton
*  CREATED   : 15-Aug-95
*  CALLED    : EXCFSR EXFRAG
*  CALLS     : (none)
*      
*------------------------------------------
      IMPLICIT NONE
* Additional commons...
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
* Arguments
      INTEGER IDUM
* Local variables
      DOUBLE PRECISION DRND,EXCRAN,RMIX
      EXTERNAL EXCRAN
* Determine pairing according to user flag
      IPAIR=1
      IF (IEXPAF.GE.4) DRND=EXCRAN(1)
      IF (IEXPAF.EQ.1) THEN
        IPAIR=IEXCCF
      ELSEIF ((IEXPAF.EQ.2.OR.(IEXPAF.EQ.3.AND.IEXCCF.EQ.0)).AND.IDUM.LE
     &    .0) THEN
        IF (EX1234.LT.EX1432) IPAIR=2
      ELSEIF (IEXPAF.EQ.3.AND.IDUM.LE.0) THEN
        IF (IEXCCF.EQ.1) THEN
          RMIX=EX1234+ABS(EXINT)
          IF (RMIX.LT.EX1432) IPAIR=2
        ELSEIF (IEXCCF.EQ.2) THEN
          RMIX=EX1432+ABS(EXINT)
          IF (RMIX.GT.EX1234) IPAIR=2
        ENDIF
      ELSEIF ((IEXPAF.EQ.4.OR.(IEXPAF.EQ.5.AND.IEXCCF.EQ.0)).AND.IDUM.LE
     &    .0) THEN
        RMIX=EX1234/(EX1234+EX1432)
        IF (DRND.GT.RMIX) IPAIR=2
      ELSEIF(IEXPAF.EQ.5.AND.IDUM.LE.0) THEN
        RMIX=(EX1234+EXINT)/(EX1234+EXINT+EX1432)
        IF ((IEXCCF.EQ.2.AND.EXINT.GE.0.0D0).OR.(IEXCCF.EQ.1.AND.EXINT
     &    .LT.0.0D0)) RMIX=EX1234/(EX1234+EXINT+EX1432)
        IF (DRND.GT.RMIX) IPAIR=2
      ELSEIF (IEXPAF.EQ.6.AND.IDUM.LE.0) THEN
        RMIX=(EX1234+EXINT)/(EX1234+EXINT+EX1432)
        IF ((EX1432.GT.EX1234.AND.EXINT.GE.0.0D0).OR.(EX1234.GT.EX1432
     &    .AND.EXINT.LT.0.0D0)) RMIX=EX1234/(EX1234+EXINT+EX1432)
        IF (DRND.GT.RMIX) IPAIR=2
      ENDIF
 999  RETURN
      END
*
      SUBROUTINE EXCHEP(IMODE,IFS)
*------------------------------------------
*! EXCHEP Excalibur event conversion to HEPEVT format
*
*  AUTHOR    : D.Charlton
*  CREATED   : 15-Aug-95
*  CALLED    : EXCINI EXCEVT
*  CALLS     : EXCFLP
*  MODIFIED/ : 01-Feb-96 (A.Waananen)
*  CORRECTED    
*------------------------------------------
      IMPLICIT NONE
      double precision pm1,pm4,omct1
      common/area10/pm1(0:4,1:6),pm4(12:65),omct1(1:6,3:6)
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
C
C Old Standard HEPEVT common block (single precision)
C
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL    PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
* Additional commons...
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
C
      EXTERNAL IPAIR
      REAL ULMASS
      DOUBLE PRECISION EM,DTEMP
      INTEGER IMODE,IFS,IPAIR
      INTEGER I,J,K,NGAMMA
      INTEGER IFFF(4),IFF3Q(4),IFFIX(4),IWWOK,IZZOK,IGGOK
      INTEGER I3Q(16)
      SAVE I3Q,EM
      DATA I3Q/-1,2,-1,2,-1,2,0,0,0,0,-3,0,-3,0,-3,0/
*
      IF (IMODE.EQ.0) THEN
        NEVHEP=0
        EM=DBLE(ULMASS(11))
        GOTO 999
      ENDIF
      IF (IMODE.NE.1) GOTO 999
      IF (NEVHEP.EQ.-1) NEVHEP=0
      NEVHEP=NEVHEP+1
* incoming electron before ISR
      PHEP(1,1)=0.
      PHEP(2,1)=0.
      PHEP(3,1)=-REAL(SQRT((ROOTS/2.)**2-EM**2))
      PHEP(4,1)=REAL(ROOTS)/2.
      PHEP(5,1)=REAL(EM)
      DO 21 K=1,4
        VHEP(K,1)=REAL(VERTEX(K))
 21   CONTINUE
      ISTHEP(1)=3
      IDHEP(1)=-11
      JMOHEP(1,1)=0
      JMOHEP(2,1)=0
      JDAHEP(1,1)=3
      JDAHEP(2,1)=3
* incoming positron before ISR
      PHEP(1,2)=0.
      PHEP(2,2)=0.
      PHEP(3,2)=REAL(SQRT((ROOTS/2.)**2-EM**2))
      PHEP(4,2)=REAL(ROOTS)/2.
      PHEP(5,2)=REAL(EM)
      DO 22 K=1,4
        VHEP(K,2)=REAL(VERTEX(K))
 22   CONTINUE
      ISTHEP(2)=3
      IDHEP(2)=11
      JMOHEP(1,2)=0
      JMOHEP(2,2)=0
      JDAHEP(1,2)=4
      JDAHEP(2,2)=4
* Photons?
      NGAMMA=0
      IF (ABS(SNGL(PM1(1,1))).LT.ABS(PHEP(3,1))) THEN
        NGAMMA=NGAMMA+1
        PHEP(1,4+NGAMMA)=0.
        PHEP(2,4+NGAMMA)=0.
        PHEP(3,4+NGAMMA)=PHEP(3,1)+PM1(1,1)
        PHEP(4,4+NGAMMA)=ABS(PHEP(3,4+NGAMMA))
        PHEP(5,4+NGAMMA)=0.
        DO 23 K=1,4
          VHEP(K,4+NGAMMA)=REAL(VERTEX(K))
 23     CONTINUE
        ISTHEP(4+NGAMMA)=1
        IDHEP(4+NGAMMA)=22
        JMOHEP(1,4+NGAMMA)=1
        JMOHEP(2,4+NGAMMA)=0
        JDAHEP(1,4+NGAMMA)=0
        JDAHEP(2,4+NGAMMA)=0
      ENDIF
      IF (ABS(SNGL(PM1(1,2))).LT.ABS(PHEP(3,2))) THEN
        NGAMMA=NGAMMA+1
        PHEP(1,4+NGAMMA)=0.
        PHEP(2,4+NGAMMA)=0.
        PHEP(3,4+NGAMMA)=PHEP(3,2)+PM1(1,2)
        PHEP(4,4+NGAMMA)=ABS(PHEP(3,4+NGAMMA))
        PHEP(5,4+NGAMMA)=0.
        DO 24 K=1,4
          VHEP(K,4+NGAMMA)=REAL(VERTEX(K))
 24     CONTINUE
        ISTHEP(4+NGAMMA)=1
        IDHEP(4+NGAMMA)=22
        JMOHEP(1,4+NGAMMA)=2
        JMOHEP(2,4+NGAMMA)=0
        JDAHEP(1,4+NGAMMA)=0
        JDAHEP(2,4+NGAMMA)=0
      ENDIF
      NHEP=8+NGAMMA
      DO 100 I=1,6
        IF (I.LT.3) THEN
          J=I+2
        ELSE
          J=I+2+NGAMMA
        ENDIF
        ISTHEP(J)=1
        IF (I.EQ.1) THEN
          IDHEP(J)=-11
          ISTHEP(J)=3
        ELSEIF (I.EQ.2) THEN
          IDHEP(J)=11
          ISTHEP(J)=3
        ELSEIF (I.EQ.3) THEN
          IDHEP(J)=INT(IFS/1000000)
          IFFF(I-2)=IDHEP(J)
          IFF3Q(I-2)=I3Q(IDHEP(J))
          IFFIX(I-2)=J
        ELSEIF (I.EQ.4) THEN
          IDHEP(J)=-MOD(INT(IFS/10000),100)
          IFFF(I-2)=IDHEP(J)
          IFF3Q(I-2)=-I3Q(-IDHEP(J))
          IFFIX(I-2)=J
        ELSEIF (I.EQ.5) THEN
          IDHEP(J)=MOD(INT(IFS/100),100)
          IFFF(I-2)=IDHEP(J)
          IFF3Q(I-2)=I3Q(IDHEP(J))
          IFFIX(I-2)=J
        ELSEIF (I.EQ.6) THEN
          IDHEP(J)=-MOD(IFS,100)
          IFFF(I-2)=IDHEP(J)
          IFF3Q(I-2)=-I3Q(-IDHEP(J))
          IFFIX(I-2)=J
          IF (ABS(IDHEP(J)).EQ.21) IDHEP(J)=21
        ENDIF
        JMOHEP(1,J)=0
        JMOHEP(2,J)=0
        JDAHEP(1,J)=0
        JDAHEP(2,J)=0
* Charlton:
c        PHEP(3,J)=-REAL(PM1(1,I))
c        PHEP(2,J)=-REAL(PM1(2,I))
c        PHEP(1,J)=-REAL(PM1(3,I))
        PHEP(3,J)=-REAL(PM1(1,I))
        PHEP(2,J)=REAL(PM1(2,I))
        PHEP(1,J)=REAL(PM1(3,I))
        PHEP(4,J)=REAL(PM1(0,I))
        PHEP(5,J)=REAL(SQRT(PM1(4,I)))
        DO 25 K=1,4
          VHEP(K,J)=REAL(VERTEX(K))
 25     CONTINUE
 100  CONTINUE
*
* Check if we need to do any reordering of the final state fermions
* EXCALIBUR produces always  f-fbar f-fbar
* Note: we only need to copy IDHEP and PHEP around (VHEP,DA/MO all zero
*   and ISTHEPs all the same...)
*
* First: can the decay products come from W decay?
*
      IEXOFP(5+NGAMMA)=5+NGAMMA
      IEXOFP(6+NGAMMA)=6+NGAMMA
      IEXOFP(7+NGAMMA)=7+NGAMMA
      IEXOFP(8+NGAMMA)=8+NGAMMA
      IWWOK=0
      IF (IFF3Q(1)+IFF3Q(2).EQ.3.AND.IFF3Q(3)+IFF3Q(4).EQ.-3) THEN
        IWWOK=1
      ENDIF
      IF (IFF3Q(1)+IFF3Q(2).EQ.-3.AND.IFF3Q(3)+IFF3Q(4).EQ.3.AND.IWWOK
     &  .EQ.0) THEN
        CALL EXCFLP(IFFIX(1),IFFIX(3))
        CALL EXCFLP(IFFIX(2),IFFIX(4))
        IWWOK=1
        J=IEXOFP(5+NGAMMA)
        IEXOFP(5+NGAMMA)=IEXOFP(7+NGAMMA)
        IEXOFP(7+NGAMMA)=J
        J=IEXOFP(6+NGAMMA)
        IEXOFP(6+NGAMMA)=IEXOFP(8+NGAMMA)
        IEXOFP(8+NGAMMA)=J
      ENDIF
      IF (IFF3Q(1)+IFF3Q(4).EQ.3.AND.IFF3Q(2)+IFF3Q(3).EQ.-3.AND.IWWOK
     &  .EQ.0) THEN
        CALL EXCFLP(IFFIX(2),IFFIX(4))
        IF (IEXCCF.GT.0) IEXCCF=MOD(IEXCCF,2)+1
        DTEMP=EX1234
        EX1234=EX1432
        EX1432=DTEMP
        IWWOK=1
        J=IEXOFP(6+NGAMMA)
        IEXOFP(6+NGAMMA)=IEXOFP(8+NGAMMA)
        IEXOFP(8+NGAMMA)=J
      ENDIF
      IF (IFF3Q(1)+IFF3Q(4).EQ.-3.AND.IFF3Q(2)+IFF3Q(3).EQ.3.AND.IWWOK
     &  .EQ.0) THEN
        CALL EXCFLP(IFFIX(1),IFFIX(3))
        IF (IEXCCF.GT.0) IEXCCF=MOD(IEXCCF,2)+1
        DTEMP=EX1234
        EX1234=EX1432
        EX1432=DTEMP
        IWWOK=1
        J=IEXOFP(5+NGAMMA)
        IEXOFP(5+NGAMMA)=IEXOFP(7+NGAMMA)
        IEXOFP(7+NGAMMA)=J
      ENDIF
*
* All OK if classifiable as possible WW decay...
*
      IF (IWWOK.EQ.1) GOTO 888
*
* Not a W decay: might it be a ZZ decay?
*
      IZZOK=0
      IF (IFF3Q(1)+IFF3Q(2).EQ.0.AND.IFFF(1)+IFFF(2).EQ.0.AND.
     +  IFF3Q(3)+IFF3Q(4).EQ.0.AND.IFFF(3)+IFFF(4).EQ.0) THEN
        IZZOK=1
      ENDIF
      IF (IFF3Q(1)+IFF3Q(4).EQ.0.AND.IFFF(1)+IFFF(4).EQ.0.AND.
     +  IFF3Q(2)+IFF3Q(3).EQ.0.AND.IFFF(2)+IFFF(3).EQ.0.AND.IZZOK.EQ.0)
     &  THEN
        CALL EXCFLP(IFFIX(2),IFFIX(4))
        IF (IEXCCF.GT.0) IEXCCF=MOD(IEXCCF,2)+1
        DTEMP=EX1234
        EX1234=EX1432
        EX1432=DTEMP
        IZZOK=1
        J=IEXOFP(6+NGAMMA)
        IEXOFP(6+NGAMMA)=IEXOFP(8+NGAMMA)
        IEXOFP(8+NGAMMA)=J
      ENDIF
      IF (IZZOK.EQ.1) GOTO 888
*
* Not a W or Z decay: might it be a QQgg final state?
*
      IGGOK=0
      IF (IFF3Q(1)+IFF3Q(2).EQ.0.AND.IFFF(1)+IFFF(2).EQ.0.AND.
     +  IFF3Q(3)+IFF3Q(4).EQ.0.AND.IFFF(3).EQ.IFFF(4).AND.IFFF(3).EQ.21)
     &  THEN
        IGGOK=1
      ENDIF
      IF (IGGOK.EQ.1) GOTO 888
*
* Final state not classifiable as possible WW or ZZ decay
*
      PRINT *,' UNCLASSIFIABLE FINAL STATE!!!!'
 888  CONTINUE
* Special treatment for QQgg final states
      IF (IGGOK.EQ.1) THEN
        IF (IPAIR(IZZOK).EQ.2) CALL EXCFLP(IFFIX(3),IFFIX(4))
        DTEMP=EX1234
        EX1234=EX1432
        EX1432=DTEMP
        IF (IEXCCF.GT.0) IEXCCF=MOD(IEXCCF,2)+1
      ENDIF
* Now pair the 4 fermions according to user flag...
* Only problems when we have 4 leptons or 4 quarks
      IF ((ABS(IFFF(1)).LT.7..AND.ABS(IFFF(3)).GT.7).OR.(ABS(IFFF(1)).GT
     &  .7..AND.ABS(IFFF(3)).LT.7)) GOTO 999
      IZZOK=0
      IF (IPAIR(IZZOK).EQ.2) THEN
        CALL EXCFLP(IFFIX(2),IFFIX(4))
        DTEMP=EX1234
        EX1234=EX1432
        EX1432=DTEMP
        IF (IEXCCF.GT.0) IEXCCF=MOD(IEXCCF,2)+1
        J=IEXOFP(6+NGAMMA)
        IEXOFP(6+NGAMMA)=IEXOFP(8+NGAMMA)
        IEXOFP(8+NGAMMA)=J
      ENDIF
      
 999  RETURN
      END      
*      
      SUBROUTINE EXCFLP(IX1,IX2)
*------------------------------------------
*! EXCFLP  Swap two HEPEVT entries
*
*  AUTHOR    : D.Charlton
*  CREATED   : 15-Aug-95
*  CALLED    : EXCHEP
*  CALLS     : (none)
*      
*------------------------------------------
      IMPLICIT NONE
C
C Old Standard HEPEVT common block (single precision)
C
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL    PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
*
      INTEGER IX1,IX2
      INTEGER I,IDHPSV
      REAL PHEPSV
*
* Only momentum and identity entries need to be swapped.
*
      DO 100 I=1,5
         PHEPSV=PHEP(I,IX1)
         PHEP(I,IX1)=PHEP(I,IX2)
         PHEP(I,IX2)=PHEPSV
 100  CONTINUE
      IDHPSV=IDHEP(IX1)
      IDHEP(IX1)=IDHEP(IX2)
      IDHEP(IX2)=IDHPSV
*
 999  CONTINUE
      END
*
      DOUBLE PRECISION FUNCTION DCOULM(IDUM)
C======================================================================
C This function calculates an approximate value for the correction
C to W+W- production due to the Coulomb singularity.
C Theoretical formula: Coulomb effect from Fadin, Khoze, Martin,
C Stirling, dtp/95/64 first order, eq. 9
C input:   S = center of mass energy squared (after ISR)
*          M1,M2  = Generated boson masses
*          WM,WW  = W Mass and WIDTH
*          ALPHA  = Alpha_QED
C output:  COULMB = correction:   sigma --> sigma * (1 + COULMB)
C          where sigma is the cross section.
C
C 04Aug JBH: Original formulas from lpww02 replaced.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDUM
* Original commons
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
* Additional commons
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      DOUBLE PRECISION M1,M2
      COMMON /BMASS/ M1,M2
* Local varables
      DOUBLE PRECISION S,S1,S2
      DOUBLE PRECISION PP,P,EN,DDEE,P1,P2,DABSKAP2,DREKAP,DIMKAP
      DOUBLE PRECISION PI
      PARAMETER (PI=3.14159265358979324D0)
C
C calculate the correction
C
      DCOULM=0.D0
      S=ROOTS*ROOTS*XR1*XR2
      IF ((S.EQ.0D0).OR.(M1.EQ.0D0).OR.(M2.EQ.0D0)) THEN
        PRINT 100,S,M1,M2
        RETURN
      ENDIF
      IF (((S - (M1-M2)**2) * (S - (M1+M2)**2)).LE.0.D0) THEN
        PRINT 110,S,M1,M2,((S-(M1-M2)**2)*(S-(M1+M2)**2))/S/S
        RETURN
      ENDIF
      S1=M1*M1
      S2=M2*M2
      PP=1.0D0/(4.0D0*S)*(S**2-2.0D0*S*(S1+S2)+(S1-S2)**2 )
      P=DSQRT(PP)
      EN=(S-4.0D0*WMI**2)/(4.0D0*WMI)
      DDEE=DSQRT(EN**2+WWI**2)
      P1=DSQRT(WMI/2.0D0*(DDEE-EN))
      P2=DSQRT(WMI/2.0D0*(DDEE+EN))
      DABSKAP2=WMI*DDEE
      DREKAP=P1
      DIMKAP=-P2
      DCOULM=ALPHAR*SQRT(S)/(4.0D0*P)*(PI-2.0D0*DATAN((DABSKAP2 -PP)
     &  /(2.0D0*P*DREKAP)))   
      RETURN
 100  FORMAT(' Error in DCOULM: zero masses: S,M1,M2=',3E12.5)
 110  FORMAT(' Error in DCOULM: neg sqrt for BETA zero masses: '
     &  ,'S,M1,M2,BETA^2=',4E12.5)
      END
*
      DOUBLE PRECISION FUNCTION I3BODY(S,M1S,M2S,M3S)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Mar-96
*  CALLED    : DEC3F DEC3N
*  CALLS     : ICALC
*  PURPOSE   : Calculates the phase-space volume for 3-body decays
*              with massive particles. Exact if one or more massless
*              particles. When all particles are massive we taylor-
*              expand in the smallest mass (always an electron!)
*              using that first and third order in the expansion is
*              zero.
*
*------------------------------------------
      IMPLICIT NONE
*
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
      DOUBLE PRECISION S,M1S,M2S,M3S,MASS1,MASS2,MASS3,LOWLIM,UPLIM
     &  ,TEMP1,TEMP2,TEMP3,TEMP4,RESULT,ICALC
*
      IF (M2S.EQ.0.0D0.AND.M3S.EQ.0.0D0) THEN
        RESULT=((S*S-M1S*M1S)/2.D0/S+M1S*LOG(M1S/S))/4.0D0
        GOTO 999
      ENDIF
      MASS1=MAX(M1S,M2S)
      MASS2=MIN(M1S,M2S)
      MASS3=M3S
      IF (MASS3.LT.MASS1) THEN
        MASS3=MASS1
        MASS1=M3S
        MASS1=MAX(M3S,MASS2)
        MASS2=MIN(MASS2,M3S)
      ENDIF
      IF (MASS2.GT.0.01D0) PRINT*,'WARNING: ',MASS2
      LOWLIM=SQRT(MASS3)
      UPLIM=SQRT(MASS3+(S-(SQRT(MASS1)+SQRT(MASS2)-LOWLIM)**2)*(S
     &  -(SQRT(MASS1)+SQRT(MASS2)+LOWLIM)**2)/4.0D0/S)
      TEMP4=MASS3+S
      TEMP3=SQRT(UPLIM*UPLIM-MASS3)
      RESULT=(-MASS3*LOG((UPLIM+TEMP3)/LOWLIM)+UPLIM*TEMP3)/2.0D0
      RESULT=RESULT+(MASS1+3.0D0*MASS2)*ICALC(-2.0D0*(MASS3+S),S-MASS3
     &  ,TEMP4-2.0D0*SQRT(S)*UPLIM,TEMP4-2.0D0*SQRT(S)*LOWLIM)/4.0D0/S
      IF (MASS2.EQ.0.0D0) GOTO 999
      TEMP1=2.0D0*(MASS1-MASS3-S)
      TEMP2=SQRT(TEMP1*TEMP1/4.0D0-4.0D0*S*MASS3)
      RESULT=RESULT+MASS2*ICALC(TEMP1,TEMP2,TEMP4-2.0D0*SQRT(S)
     &  *UPLIM-MASS1,TEMP4-2.0D0*SQRT(S)*LOWLIM-MASS1)/2.0D0/S
 999  CONTINUE
      I3BODY=RESULT
      IF (RESULT.EQ.0.0D0) THEN
        I3BODY=1.0D-10
        FAILFLAG=1
      ENDIF
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION ICALC(B,C,U,L)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Mar-96
*  CALLED    : I3BODY
*  CALLS     : (none)
*
*------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION B,C,U,L,TEMP1,TEMP3,TEMP4
      TEMP1=SQRT(C)*SQRT(U*U/C+B*U/C+C)
      TEMP3=B*LOG((2.0D0*TEMP1+2.0D0*U+B)/(2.0D0*L+B))/2.0D0
      TEMP4=-C*LOG((2.0D0*C*TEMP1+B*U+2.0D0*C*C)*L/U/(B*L+2.0D0*C*C))
      ICALC=TEMP1+TEMP3+TEMP4
      RETURN
      END
*
      SUBROUTINE DPRD(UNT,ISTAT)
*------------------------------------------
*
*  AUTHOR    : J.B.Hansen
*  CREATED   : 01-Feb-96
*  CALLED    : (user)
*  CALLS     : EXCFID
*
*------------------------------------------
      IMPLICIT NONE
* Original commons
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
* Additional commons...
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
* Arguments
      INTEGER UNT,ISTAT
* Local variables
      INTEGER UNIT,ITEMP(3)
      CHARACTER*11 THEFMT
      double precision excran
*
* ISTAT=1 -> DUMP
* ISTAT=2 -> READ
      UNIT = UNT
      IF (UNIT.LE.5) THEN
        WRITE(LMCWRT,*) 'Unit nummer must be larger than 5!'
        ISTAT=9
        GOTO 999
      ENDIF
      INQUIRE(UNIT,FORM=THEFMT)
      IF (THEFMT(1:1).EQ.'U') THEN
        IF (ISTAT.EQ.1) THEN
          WRITE(LMCWRT,*) 'Dumping common-blocks on unit',UNIT
          WRITE(UNIT,ERR=5) SCUT,ECUT,CMAX,OMCMAX,ECUTH,CMAXH
          WRITE(UNIT,ERR=10) LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR
     &      ,IEXHAD,IEXUWT,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC
     &      ,IEXNNG,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT
     &      ,IEXINT
          WRITE(UNIT,ERR=20) ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC
     &      ,RHAD,TFRAG,RPROB,EXALSZ,EXUMWT
          WRITE(UNIT,ERR=30) NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT
     &      ,IEXCCF,IEXTPL,IEXOFP,IEXPHG,IEXPRO
          WRITE(UNIT,ERR=40) EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &      ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &      ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
          WRITE(LMCWRT,*) 'Dumping EXCRUN variables on unit',UNIT
        ELSEIF (ISTAT.EQ.2) THEN
          WRITE(LMCWRT,*) 'Reading common-blocks on unit',UNIT
          READ(UNIT,ERR=5) SCUT,ECUT,CMAX,OMCMAX,ECUTH,CMAXH
          READ(UNIT,ERR=10) LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR
     &      ,IEXHAD,IEXUWT,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC
     &      ,IEXNNG,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT
     &      ,IEXINT
          READ(UNIT,ERR=20) ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC
     &      ,RHAD,TFRAG,RPROB,EXALSZ,EXUMWT
          READ(UNIT,ERR=30) NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT
     &      ,IEXCCF,IEXTPL,IEXOFP,IEXPHG,IEXPRO
          READ(UNIT,ERR=40) EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &      ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &      ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
          WRITE(LMCWRT,*) 'Reading EXCRUN variables on unit',UNIT
        ENDIF
      ELSE
        IF (ISTAT.EQ.1) THEN
          WRITE(LMCWRT,*) 'Dumping common-blocks on unit',UNIT
          WRITE(UNIT,*,ERR=5) SCUT,ECUT,CMAX,OMCMAX,ECUTH,CMAXH
          WRITE(UNIT,*,ERR=10) LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR
     &      ,IEXHAD,IEXUWT,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC
     &      ,IEXNNG,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT
     &      ,IEXINT
          WRITE(UNIT,*,ERR=20) ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC
     &      ,RHAD,TFRAG,RPROB,EXALSZ,EXUMWT
          WRITE(UNIT,*,ERR=30) NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT
     &      ,IEXCCF,IEXTPL,IEXOFP,IEXPHG,IEXPRO
          WRITE(UNIT,*,ERR=40) EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &      ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &      ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
          WRITE(LMCWRT,*) 'Dumping EXCRUN variables on unit',UNIT
        ELSEIF (ISTAT.EQ.2) THEN
          WRITE(LMCWRT,*) 'Reading common-blocks on unit',UNIT
          READ(UNIT,*,ERR=5) SCUT,ECUT,CMAX,OMCMAX,ECUTH,CMAXH
          READ(UNIT,*,ERR=10) LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR
     &      ,IEXHAD,IEXUWT,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC
     &      ,IEXNNG,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT
     &      ,IEXINT
          READ(UNIT,*,ERR=20) ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC
     &      ,RHAD,TFRAG,RPROB,EXALSZ,EXUMWT
          READ(UNIT,*,ERR=30) NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT
     &      ,IEXCCF,IEXTPL,IEXOFP,IEXPHG,IEXPRO
          READ(UNIT,*,ERR=40) EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &      ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &      ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
          WRITE(LMCWRT,*) 'Reading EXCRUN variables on unit',UNIT
        ENDIF
      ENDIF
      GOTO 100
 5    CONTINUE
      WRITE(LMCWRT,*) 'ERROR! Cannot dump/read CUTSET AND CUTH on unit'
     &  ,UNIT
      ISTAT=8
      GOTO 999
 10   CONTINUE
      WRITE(LMCWRT,*) 'ERROR! Cannot dump/read EXINII on unit',UNIT
      ISTAT=7
      GOTO 999
 20   CONTINUE
      WRITE(LMCWRT,*) 'ERROR! Cannot dump/read EXINID on unit',UNIT
      ISTAT=6
      GOTO 999
 30   CONTINUE
      WRITE(LMCWRT,*) 'ERROR! Cannot dump/read EXGENI on unit',UNIT
      ISTAT=5
      GOTO 999
 40   CONTINUE
      WRITE(LMCWRT,*) 'ERROR! Cannot dump/read EXGEND on unit',UNIT
      ISTAT=4
      GOTO 999
 100  CONTINUE
      ITEMP(3) = ISTAT
      CALL EXCRUN(UNIT,ITEMP(1),10,ITEMP(2),ITEMP(3))
      IF (ITEMP(3).GT.0) THEN
        ISTAT = ITEMP(3)*10+3
        GOTO 999
      ENDIF
      IF (ISTAT.EQ.1) THEN
        WRITE(LMCWRT,*) 'Dumping RANMAR variables on unit',UNIT
        ITEMP(1)=0
        ITEMP(2)=0
        ITEMP(3)=0
        CALL RMARUT(ITEMP(1),ITEMP(2),ITEMP(3))
        IF (THEFMT(1:1).EQ.'U') THEN
          WRITE(UNIT,ERR=110) ITEMP
        ELSE
          WRITE(UNIT,*,ERR=110) ITEMP
        ENDIF
      ELSEIF (ISTAT.EQ.2) THEN
        WRITE(LMCWRT,*) 'Reading RANMAR variables on unit',UNIT
        ITEMP(1)=0
        ITEMP(2)=0
        ITEMP(3)=0
        IF (THEFMT(1:1).EQ.'U') THEN
          READ(UNIT,ERR=110) ITEMP
        ELSE
          READ(UNIT,*,ERR=110) ITEMP
        ENDIF
        WRITE(LMCWRT,*) 'Now initializing RANMAR...'
        CALL RMARIN(ITEMP(1),ITEMP(2),ITEMP(3))
        WRITE(LMCWRT,*) 'Done.'
        WRITE(LMCWRT,1310)
        ITEMP(1)=1
        CALL LSTINI(ITEMP(1))
      ENDIF
      ISTAT=0
      print*,'random= ',excran(1)
      GOTO 999
 110  CONTINUE
      WRITE(LMCWRT,*) 'ERROR! Cannot dump/read RANMAR-variables on unit'
     &  ,UNIT
      ISTAT=2
 999  RETURN
 1310 FORMAT(/,1X,'DPRD: Initial input from file is',//,3X,
     &  'Final state',6X,'Cross-section (pb)',3X,'Redundancy',4X,
     &  '# Events',2X,'Max weight')
      END
*
*========================================================
* Routines from the original code (with small changes..)
*========================================================
      subroutine address(lflag,nc,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c    contains the addresses of all Monte Carlo channels.         c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      dimension dj(1:28,1:48)
      SAVE NC0,NN0
      if (lflag.eq.0) then
        nc0= 0
        nn0= 0
      endif
      if (nc0.eq.nc.and.nn0.eq.nn) then
        lnot= 1
      else
        lnot= 0
      endif
      if (nc.eq.1.and.lnot.eq.0) then
        call annih2(lflag,zm,wz,nn,dj(nc,nn))
      else if (nc.eq.2.and.lnot.eq.0) then
        call annih1(lflag,nn,dj(nc,nn))
      else if (nc.eq.3.and.lnot.eq.0) then
        call annih2(lflag,wm,ww,nn,dj(nc,nn))
      else if (nc.eq.4.and.lnot.eq.0) then
        call bremf2(lflag,zm,wz,nn,dj(nc,nn))
      else if (nc.eq.5.and.lnot.eq.0) then
        call bremf1(lflag,nn,dj(nc,nn))
      else if (nc.eq.6.and.lnot.eq.0) then
        call bremf2(lflag,wm,ww,nn,dj(nc,nn))
      else if (nc.eq.7.and.lnot.eq.0) then
        call bremf4(lflag,zm,wz,nn,dj(nc,nn))
      else if (nc.eq.8.and.lnot.eq.0) then
        call bremf3(lflag,nn,dj(nc,nn))
      else if (nc.eq.9.and.lnot.eq.0) then
        call bremf4(lflag,wm,ww,nn,dj(nc,nn))
      else if (nc.eq.10.and.lnot.eq.0) then
        call bremb2(lflag,zm,wz,nn,dj(nc,nn))
      else if (nc.eq.11.and.lnot.eq.0) then
        call bremb1(lflag,nn,dj(nc,nn))
      else if (nc.eq.12.and.lnot.eq.0) then
        call bremb2(lflag,wm,ww,nn,dj(nc,nn))
      else if (nc.eq.13.and.lnot.eq.0) then
        call convr2(lflag,zm,wz,nn,dj(nc,nn))
      else if (nc.eq.14.and.lnot.eq.0) then
        call convr1(lflag,nn,dj(nc,nn))
      else if (nc.eq.15.and.lnot.eq.0) then
        call convr2(lflag,wm,ww,nn,dj(nc,nn))
      else if (nc.eq.16.and.lnot.eq.0) then
        call convr3(lflag,zm,wz,zm,wz,nn,dj(nc,nn))
      else if (nc.eq.17.and.lnot.eq.0) then
        call convr3(lflag,wm,ww,wm,ww,nn,dj(nc,nn))
      else if (nc.eq.18.and.lnot.eq.0) then
        call multi1(lflag,nn,dj(nc,nn))
      else if (nc.eq.19.and.lnot.eq.0) then
        call multi2(lflag,nn,dj(nc,nn))
      else if (nc.eq.20.and.lnot.eq.0) then
        call multi3(lflag,nn,dj(nc,nn))
      else if (nc.eq.21.and.lnot.eq.0) then
        call nonab1(lflag,wm,ww,wm,ww,nn,dj(nc,nn))
      else if (nc.eq.22.and.lnot.eq.0) then
        call nonab2(lflag,wm,ww,nn,dj(nc,nn))
      else if (nc.eq.23.and.lnot.eq.0) then
        call nonab3(lflag,nn,dj(nc,nn))
      else if (nc.eq.24.and.lnot.eq.0) then
        call nonab4(lflag,zm,wz,nn,dj(nc,nn))
      else if (nc.eq.25.and.lnot.eq.0) then
        call nonab4(lflag,wm,ww,nn,dj(nc,nn))
      else if (nc.eq.26.and.lnot.eq.0) then
        call rambo4(lflag,nn,dj(nc,nn))
*JBH
      ELSE IF (NC.EQ.27.AND.LNOT.EQ.0) THEN
        CALL NABGG1(LFLAG,NN,DJ(NC,NN))
      ELSE IF (NC.EQ.28.AND.LNOT.EQ.0) THEN
        CALL ANHGG1(LFLAG,NN,DJ(NC,NN))
      endif
      if (lflag.eq.0) then
        nc0= nc
        nn0= nn
      endif
      return
      end
*
       subroutine setpro(ncm,nnm,nct,krel,nopro,par)
c-------------------------------------------------------------------c
c                                                                   c
c The program is set according to the chosen 4-fermion final state. c
c                                                                   c
c-------------------------------------------------------------------c
      implicit DOUBLE PRECISION (a-b,d-h,o-z)
      implicit complex*16 (c)
      character*2 par,ferm,prop
      character*15 kcname
      parameter(ndab=144,nnab=100,ntot=150)
      common/printout/nout,monfil,logfil,datfil
      integer nout
      logical monfil,logfil,datfil
      common/aus/c0,c1,ci
      complex*16 c0,c1,ci
*      common/aus/c0,c1,ci,pi
      double precision pi
      parameter (pi=3.14159265358979324D0)
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      common/area2/vtxn(1:8,100),vtx(1:8,144)
      double precision vtxn,vtx
      common/area3/noc(1:36,1:4),nc(1:36)
      integer noc,nc
      common/area4/kperm(1:6,1:36),jj(1:16,1:144),jn(1:18,1:100),
     +             jv(1:8,1:3),nzza(144),nzzn(100)
      integer kperm,jj,jn,jv,nzza,nzzn
      common/area6/ka,kn,nz
      integer ka,kn,nz
      common/area7/kf(1:4,1:24)
      integer kf
      common/area11/dz,xg,yg,xz,yz,zz
      double precision dz,xg,yg,xz,yz,zz
      common/qcd/fnzz0(144),fnzz1(144)
      double precision fnzz0,fnzz1
      dimension ji(1:6),j(1:8),par(3:6),ferm(1:12),prop(1:3)
      dimension l(1:3),lq(1:4),ln(1:4),lpd(1:4),iphase(1:36)
      dimension aa(1:3,1:12),vv(1:3,1:12),v(1:4),a(1:4),ncn(1:36)
      dimension ncha(1:26,1:48),ncm(1:ntot),nnm(1:ntot)
      dimension k10(1:nnab),ka0(1:ndab),nct0(ndab)
      dimension kcname(26)
      dimension kpermt(1:6,1:36),jvt(1:8,1:3)
* Code from R.Pittau for QQgg
      common/vertex/arge,argq,alge,algq,
     +              arze,arzq,alze,alzq
* JBH
      INTEGER IEXCLM,IEXABD,IEXNAD,NL,WWDIA
      COMMON /EXCSNG/ IEXCLM,IEXABD(144),IEXNAD(8),NL,WWDIA
c-    ji(1) and ji(2) are the initial states
      data ji(1)/1/,ji(2)/-1/
c-    kperm contains all possible permutations of 6 fermions
      data kpermt/1,3,5,2,4,6,1,3,5,2,6,4,1,3,5,4,2,6,1,3,5,4,6,2,
     +            1,3,5,6,2,4,1,3,5,6,4,2,1,5,3,2,4,6,1,5,3,2,6,4,
     +            1,5,3,4,2,6,1,5,3,4,6,2,1,5,3,6,2,4,1,5,3,6,4,2,
     +            3,1,5,2,4,6,3,1,5,2,6,4,3,1,5,4,2,6,3,1,5,4,6,2,
     +            3,1,5,6,2,4,3,1,5,6,4,2,3,5,1,2,4,6,3,5,1,2,6,4,
     +            3,5,1,4,2,6,3,5,1,4,6,2,3,5,1,6,2,4,3,5,1,6,4,2,
     +            5,1,3,2,4,6,5,1,3,2,6,4,5,1,3,4,2,6,5,1,3,4,6,2,
     +            5,1,3,6,2,4,5,1,3,6,4,2,5,3,1,2,4,6,5,3,1,2,6,4,
     +            5,3,1,4,2,6,5,3,1,4,6,2,5,3,1,6,2,4,5,3,1,6,4,2/
c-    relative phase corresponding to a given permutation
      data iphase/1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,-1,1,1,-1,-1,1,1,
     +           -1,-1,1,1,-1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1/
c-    helicity indices
      data jvt/1,1,1,1,-1,-1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,-1,1,-1,
     +         1,-1,1,-1/
      data ferm/'el','ne','mu','nm','ta','nt','dq','uq','sq',
     +          'cq','bq','tq'/,prop/'Z','G','W'/
c-    names of the kinematical channels
      data kcname/'annihi2(zm)','annihi1','annihi2(wm)',
     +            'bremf2(zm)','bremf1','bremf2(wm)','bremf4(zm)',
     +            'bremf3','bremf4(wm)','bremb2(zm)','bremb1',
     +            'bremb2(wm)','conver2(zm)','conver1',
     +            'conver2(wm)','conver3(zm)','conver3(wm)',
     +            'multi1','multi2','multi3','nonab1(wm)',
     +            'nonab2(wm)','nonab3','nonab4(zm)',
     +            'nonab4(wm)','rambo4'/
      data c0t/(0.d0,0.d0)/,c1t/(1.d0,0.d0)/,cit/(0.d0,1.d0)/
*
      isp(m)= mod(abs(m),2)
      
* JBH: Tag if there is any WW diagrams
      WWDIA=0
      DO I1=1,144
        IEXABD(I1)=0
      ENDDO
      
      c0 = c0t
      c1 = c1t
      ci = cit
      do i1=1,6
        do i2=1,36
          kperm(i1,i2) = kpermt(i1,i2)
        enddo
      enddo
      do i1=1,8
        IEXNAD(I1)=0
        do i2=1,3
          jv(i1,i2) = jvt(i1,i2)
        enddo
      enddo
c-    ka0, k10 and nct0 are initialized to 1
      do i= 1,ndab
        ka0(i)= 1
        nct0(i)= 1
      enddo
      do i= 1,nnab
        k10(i)= 1
      enddo
c-    here you can switch off diagrams by putting ka0, k10= 0
c-    initialization
      ka= 0
      kn= 0
      nz= 0
      lph= 0
      ln(1)= 0
      ln(2)= 0
      ln(3)= 0
      ln(4)= 0
      do 35 i= 1,26
        do 35 ii= 1,48
          ncha(i,ii)= 0
   35 continue
      do np= 1,36
        nc(np)= 0
        ncn(np)= 0
        do i= 1,4
          noc(np,i)= 0
        enddo
      enddo
      statfac= 1.d0
* DGC moved initialisation of SM parameters to user routines
      zm=zmi
      wm=wmi
      ww=wwi
      wz=wzi
c-    weak and strong parameters
c      alpha= 1.d0/128.07d0
c      als= 0.103d0
c      zm= 91.1888d0
c      wm= 80.23d0
c      sth2= 0.2310309d0
c      ww= 2.0366d0
c      wz= 2.4974d0
      gamfacw= ww/wm
      gamfacz= wz/zm
      zm= zm/sqrt(1+gamfacz*gamfacz)
      wm= wm/sqrt(1+gamfacw*gamfacw)
      wz= wz/sqrt(1+gamfacz*gamfacz)
      ww= ww/sqrt(1+gamfacw*gamfacw)
c-    picobarn conversion factor
      pbfac= 3.8937d8
      if (nout.gt.0) then
         write (nout,50) sth2,zm,wz,wm,ww,1d0/alpha,als
 50      format (' s^2_thet = ',d15.6/,' Z-mass   = ',d15.6/,
     +     ' Z-width  = ',d15.6/,' W-mass   = ',d15.6/,
     +     ' W-width  = ',d15.6/,' 1/alpha  = ',d15.6/,
     +     ' alpha_s  = ',d15.6 )
         write(nout,*)
         write(nout,*) ' Non-standard couplings'
         write(nout,*)
         write(nout,59) dz,xg,yg,xz,yz,zz
 59      format (' delta_Z = ',d15.6/,' x_gamma = ',d15.6/,
     +     ' y_gamma = ',d15.6/,' x_Z     = ',d15.6/,
     +     ' y_Z     = ',d15.6/,' z_Z     = ',d15.6 )
         write(nout,*)
      endif
      sth= sqrt(sth2)
      acth= sqrt(1.d0-sth2)
c---------------------------------------------------------------c
c                                                               c
c     The final states have to be ordered as follows:           c
c     3-part. 4-antip. 5-part. 6-antip.                         c
c                                                               c
c                 choosing among                                c
c                                                               c
c     [el,ne,mu,nm,ta,nt,dq,uq,sq,cq,bq,tq]                     c
c                                                               c
c---------------------------------------------------------------c
      if (nout.gt.0) then
         write (nout,51) par(3),par(4),par(5),par(6)
 51      format (' process  :    antiel(1) el(2) ---> ',a2,'(3)',
     +     ' anti',a2,'(4) ',a2,'(5) ','anti',a2,'(6)'/)
      endif
c---------------------------------------------------------------c
c                                                               c
c    ji(i) is filled as follows:                                c
c                                                               c
c             el  ne  mu  nm  ta  nt  dq  uq  sq  cq  bq  tq    c
c                                                               c
c    ji(i)=   1   2   3   4   5   6   7   8   9   10  11  12    c
c                                                               c
c    a - sign is assigned to the corresponding antiparticles    c
c                                                               c
c---------------------------------------------------------------c
      do i=3,6
        do ii= 1,12
          if (par(i).eq.ferm(ii)) ji(i)= ii*(2*isp(i)-1)
        enddo
      enddo
c-    axial and vectorial couplings to Z (1), gamma (2) and W (3)
      
      do i= 1,12
        aa(1,i)= (2*isp(i)-1)/4.d0/acth
        aa(2,i)= 0.d0
        aa(3,i)= -sqrt(2.d0)/4.d0
        vv(3,i)= -aa(3,i)
        if(isp(i).eq.1.and.i.le.5) vv(2,i)= sth
        if(isp(i).eq.1.and.i.gt.5) vv(2,i)= sth/3.d0
        if(isp(i).eq.0.and.i.le.6) vv(2,i)= 0.d0
        if(isp(i).eq.0.and.i.gt.6) vv(2,i)= -2.d0*sth/3.d0
        vv(1,i)= -aa(1,i)+sth/acth*vv(2,i)
      enddo
      
*JBH Watch out for the QQgg process
      IF (PAR(3).EQ.PAR(4).AND.PAR(5).EQ.PAR(6).AND.PAR(5).EQ.'gl') THEN
        nopro= 0
        zm=zmi
        wm=wmi
        ww=wwi
        wz=wzi
* Code from R. Pittau....
        statfac= 0.5d0
        
c-    vertex factors
        
        arge= 1.d0
        alge= 1.d0
        arze= (vv(1,1)+aa(1,1))/sth
        alze= (vv(1,1)-aa(1,1))/sth
        DO I=7,12
          IF (FERM(I).EQ.PAR(3)) THEN
            if (isp(ji(3)).eq.0) qf= 2.d0/3.d0
            if (isp(ji(3)).eq.1) qf= -1.d0/3.d0
            argq= -qf
            algq= -qf
            arzq= (vv(1,i)+aa(1,i))/sth
            alzq= (vv(1,i)-aa(1,i))/sth
          ENDIF
        ENDDO
        FCOL= 1.D0
        NCT=NH(1,JJ(9,1))
        NCT=9
        NCM(1)=28
        NNM(1)=1
        NCM(2)=28
        NNM(2)=7
        NCM(3)=28
        NNM(3)=9
        NCM(4)=28
        NNM(4)=11
        NCM(5)=28
        NNM(5)=5
        NCM(6)=28
        NNM(6)=3
        NCM(7)=27
        NNM(7)=3
        NCM(8)=27
        NNM(8)=5
        NCM(9)=26
        NNM(9)=1
        RETURN
      ENDIF
c-    if there are equal particles lph= 1
*      if (ji(1).eq.ji(3).or.ji(1).eq.ji(5).or.ji(3).eq.ji(5)) lph= 1
*      if (ji(2).eq.ji(4).or.ji(2).eq.ji(6).or.ji(4).eq.ji(6)) lph= 1
      lph= 1
c-    statistical factor
      if (ji(3).eq.ji(5)) statfac= 0.25d0
      
*JBH: Split 1234 and 1432 diagrams for 4 leptons as well...
      NL= 1
      IF (JI(3).LT.7.AND.JI(5).LT.7) THEN
        NL= 3
        NZ= 1
        GRAP=0.0D0
      ENDIF
            
c-    colour factor
      if (ji(3).ge.7.and.ji(5).ge.7) then
        fcol= 9.d0
c-      nz controls the colour factor with a 4-quarks final state
        nz= 1
c-      grap is the ratio between weak and strong couplings
c-      to compute the QCD interfering background.
        if (isp(ji(3)).eq.0) qch1= 2.d0/3.d0
        if (isp(ji(3)).eq.1) qch1= -1.d0/3.d0
        if (isp(ji(5)).eq.0) qch2= 2.d0/3.d0
        if (isp(ji(5)).eq.1) qch2= -1.d0/3.d0
        grap= als/alpha/qch1/qch2
        if (als.ne.0.d0.and.nout.gt.0) then
          write (nout,*) '    '
          write (nout,*) ' Interfering QCD background included:      '
          write (nout,*) '    '
          write (nout,*) ' wherever, in the below diagrams, a photon '
          write (nout,*) ' connects two quark lines also the diagram '
          write (nout,*) ' with an exchanged gluon is considered.    '
        endif
      else
        if (ji(3).ge.7.or.ji(5).ge.7) then
          fcol= 3.d0
        else
          fcol= 1.d0
        endif
      endif
c---------------------------------------------------------------c
c                                                               c
c     The diagrams are built through a big do loop on           c
c     all possible permutations np.                             c
c                                                               c
c---------------------------------------------------------------c
      do 150 np= 1,36
c-    non abelian diagrams
c---------------------------------------------------------------c
c                                                               c
c     j(i) is filled with external fermions. The three          c
c     vertices to be put together are:                          c
c                                                               c
c              (j(1),j(2),+) (j(3),j(6),n) (j(7),j(8),-)        c
c                                                               c
c     kperm=      1    4        2    5        3    6            c
c                                                               c
c     where n= 1 (Z) or 2 (gamma)                               c
c                                                               c
c     lflip(a) indicates which (anti)particle is flipped        c
c     to initial states                                         c
c---------------------------------------------------------------c
      if (kperm(1,np).eq.1) lflip= 1
      if (kperm(2,np).eq.1) lflip= 3
      if (kperm(3,np).eq.1) lflip= 7
      if (kperm(4,np).eq.2) lflia= 2
      if (kperm(5,np).eq.2) lflia= 6
      if (kperm(6,np).eq.2) lflia= 8
      do i= 1,6
        kk= 2*i-1-5*(1-sign(1,3-i))/2
        kk= kk+2*(1-sign(1,3-kk))/2
        do ii= 1,3
          ll= 2*ii-1+(1-sign(1,3-i))/2
          if (kperm(i,np).eq.ll) j(kk)= ji(ll)
        enddo
      enddo
      if (j(1)+j(2).eq.1.and.j(7)+j(8).eq.-1.and.j(3)+j(6).eq.0.
     +    and.isp(j(1)).eq.0.and.isp(j(7)).eq.1) then
        ia= int(np/6.+0.999)
        ib= np-6*(ia-1)
        ifa= ((49-ia*ia)*isp(ia)+(1-isp(ia))*(ia*ia-14*ia+56))/8
        ifb= ((49-ib*ib)*isp(ib)+(1-isp(ib))*(ib*ib-14*ib+56))/8
c---------------------------------------------------------------c
c                                                               c
c    np1 is the second momenta permutation needed to evaluate   c
c    the non abelian spinorial structure as a difference        c
c    of abelian contributions.                                  c
c                                                               c
c---------------------------------------------------------------c
        np1= 6*(ifa-1)+ifb
c---------------------------------------------------------------c
c                                                               c
c    np2 and np3 are the third and fourth permutation needed    c
c    to evaluate the non-standard non-abelian spinorial         c
c    structure for the 6-dimensional terms as a sum of three    c
c    contributions.                                             c
c                                                               c
c    np1 --> A(5,6,3,4,1,2)                                     c
c    np2 --> A(1,2,5,6,3,4)                                     c
c    np3 --> A(3,4,1,2,5,6)                                     c
c                                                               c
c---------------------------------------------------------------c
        isa= ib+2*isp(ib)-1
        isb= 6*(ia-2*isp(ia-1))
        np2= isa+isb
        ita= 2*ib+1-3*(1-sign(1,3-ib))/2
        itb= ita-6*int(ita/6.-0.001)
        itc= 2*ia+1-3*(1-sign(1,3-ia))/2
        itd= itc-6*int(itc/6.-0.001)
        np3= 6*(itd-1)+itb
        do n= 1,2
          if ((n-1)*(1-isp(j(3)))*(j(3)-7).lt.0) goto 90
c-        selection of diagrams according to the chosen krel
*          if (krel.ne.0) then
* JBH Tag diagram classes
            lsign= 0
            if (lflip.eq.3.and.lflia.eq.6) then
               lsign= 1
            else if (lflip.ne.3.and.lflia.ne.6) then
              if (n.eq.1) lsign= 5
            else if (lflia.eq.6) then
              lsign= 3
            endif
*            if (lsign.ne.krel) goto 90
            IF (LSIGN.NE.KREL.AND.KREL.NE.0) GOTO 90
*          endif
          k1= kn+1
c-        only the non-abelian diagrams with k10= 1 are selected
          if (k10(k1).eq.0) then
             do i= k1,nnab-1
               k10(i)= k10(i+1)
             enddo
             k10(nnab)= 0
             goto 90
          endif
c---------------------------------------------------------------c
c                                                               c
c  Every constructed non abelian diagram is stored in jn(i,k1): c
c                                                               c
c    i= 1 - 6  particles                                        c
c    i= 7      propagator  (Z or gamma)                         c
c    i= 8 - 13 momenta assignment                               c
c                                                               c
c---------------------------------------------------------------c
          do k= 1,6
            jn(k,k1)= j(k+1-isign(1,3-k))
            jn(7+k,k1)= kperm((k+6-5*isp(k))/2,np)
          enddo
c-        jn(14,k1) is the relative phase for the non abelian diagrams
          jn(14,k1)= isp(lph)*iphase(np)+(1-isp(lph))
          jn(7,k1)= n
          jn(15,k1)= np
          jn(16,k1)= np1
          jn(17,k1)= np2
          jn(18,k1)= np3
*          if (n.eq.1) vx= -acth
*          if (n.eq.2) vx= sth
c-        the 8 possible non abelian vertex combinations are built
          do i= 1,8
            vtxn(i,k1)= (vv(3,abs(jn(1,k1)))+jv(i,1)*
     +                  aa(3,abs(jn(1,k1))))*
     +                  (vv(n,abs(jn(3,k1)))+jv(i,2)*
     +                  aa(n,abs(jn(3,k1))))*
     +                  (vv(3,abs(jn(5,k1)))+jv(i,3)*
     +                  aa(3,abs(jn(5,k1))))
          enddo
c-       the occupation matrices are filled for the non abelian diag.
         do i=1,4
            if (abs(vtxn(i,k1)).gt.1.d-8.or.abs(vtxn(9-i,k1)).
     +          gt.1.d-8) then
              noc(np,i)= 1
              noc(np1,abs(i-2*(i-1)*(i-3)))= 1
              noc(np2,abs(i+5*(i-1)*(i-4)/2))= 1
              noc(np3,abs(i-7*(1-isign(1,2-i))/2))= 1
              ncn(np)= 2
              ncn(np1)= 2
              ncn(np2)= 2
              ncn(np3)= 2
            endif
          enddo
          kn= kn+1
* JBH
          IEXNAD(KN)=LSIGN
* JBH: Tag WW diagrams
          IF (LSIGN.EQ.1) WWDIA=1
 90     enddo
      endif
c-    abelian diagrams:
c---------------------------------------------------------------c
c                                                               c
c  The four vertices to be put together are:                    c
c                                                               c
c  (j(1),j(2),l1) (j(3),j(4),l1) (j(5),j(6),l3) (j(7),j(8),l3)  c
c                                                               c
c---------------------------------------------------------------c
      do 150 l1= 1,3
      l(1)= l1
      do 150 l3= 1,3
      l(3)= l3
      do 150 l2= 1,2
      j(4)= -j(3)+(1-isp(l2))*(1-2*isp(j(3)))
      j(5)= -j(4)
      do k= 1,8
        if (isp(j(k)).eq.0.and.abs(j(k)).le.6) ln((k+isp(k))/2)= 1
        if (isp(k).eq.1) then
          lq((k+1)/2)= j(k)+j(k+1)
          lpd((k+1)/2)= isp(j(k))
        endif
      enddo
      do k= 1,3,2
        if (l(k).eq.1.or.l(k).eq.2) then
          if (lq(k).ne.0.or.lq(k+1).ne.0) goto 100
        endif
        if (l(k).eq.3) then
          if(lpd(k).eq.1.and.lq(k).ne.-1) goto 100
          if(lpd(k).eq.0.and.lq(k).ne.1) goto 100
          if(lpd(k+1).eq.1.and.lq(k+1).ne.-1) goto 100
          if(lpd(k+1).eq.0.and.lq(k+1).ne.1) goto 100
        endif
        if (l(k).eq.2.and.ln(k).eq.1) goto 100
        if (l(k).eq.2.and.ln(k+1).eq.1) goto 100
      enddo
      if (lq(1).ne.-lq(2).or.lq(3).ne.-lq(4)) goto 100
c-    selection of diagrams according to the chosen krel
*      if (krel.ne.0) then
* JBH Tag diagram classes
        lsiga= 0
        if ((lflip.eq.1.and.lflia.eq.2).or.
     +      (lflip.eq.1.and.lflia.eq.6).or.
     +      (lflip.eq.3.and.lflia.eq.2)) then
          if (l3.eq.3.and.j(4-lflip).eq.1) lsiga= 3
          if (l3.eq.1.and.j(4-lflip).eq.1) lsiga= 4
          if (l3.eq.1.and.j(4-lflip).eq.2) lsiga= 5
        else if ((lflip.eq.3.and.lflia.eq.8).or.
     +           (lflip.eq.7.and.lflia.eq.6).or.
     +           (lflip.eq.7.and.lflia.eq.8)) then
          if (l1.eq.3.and.j(10-lflip).eq.1) lsiga= 3
          if (l1.eq.1.and.j(10-lflip).eq.1) lsiga= 4
          if (l1.eq.1.and.j(10-lflip).eq.2) lsiga= 5
        else if (lflip.eq.3.and.lflia.eq.6) then
          if (l1.eq.1.and.l3.eq.1) then
             lsiga= 2
          else if (l1.eq.3.and.l3.eq.3) then
             lsiga= 1
          else if ((l1.eq.1.and.j(7).eq.1).or.
     +             (l3.eq.1.and.j(1).eq.1)) then
             lsiga= 4
          endif
        endif
*        if (lsiga.ne.krel) goto 100
        IF (LSIGA.NE.KREL.AND.KREL.NE.0) GOTO 100
*      endif
      ka= ka+1
c-    only the abelian diagrams with ka0= 1 are selected
      if (ka0(ka).eq.0) then
        do i= ka,ndab-1
          ka0(i)= ka0(i+1)
        enddo
        ka0(ndab)= 0
        ka= ka-1
        goto 100
      endif
* JBH
      IEXABD(KA)=LSIGA
* JBH: Tag WW diagrams
      IF (LSIGA.EQ.1) WWDIA=1
c---------------------------------------------------------------c
c                                                               c
c    Every constructed abelian diagram is stored in jj(i,ka):   c
c                                                               c
c    i= 1 - 6  particles                                        c
c    i= 7 - 8  propagators                                      c
c    i= 9 - 14 momenta assignment                               c
c                                                               c
c---------------------------------------------------------------c
      do k= 1,6
        if (k.le.2) jj(6+k,ka)= l(2*k-1)
        jj(k,ka)= j(int(k+2.d0*(1-isign(1,3-k))/2))
        jj(8+k,ka)= kperm((k+6-5*isp(k))/2,np)
      enddo
c-    jj(15,ka) is the relative phase for the abelian diagrams
      jj(15,ka)= iphase(np)*isp(lph)+(1-isp(lph))
      jj(16,ka)= np
      v(1)= vv(jj(7,ka),abs(jj(1,ka)))
      a(1)= aa(jj(7,ka),abs(jj(1,ka)))
      v(2)= vv(jj(7,ka),abs(jj(3,ka)))
      a(2)= aa(jj(7,ka),abs(jj(3,ka)))
      v(3)= vv(jj(8,ka),abs(jj(4,ka)))
      a(3)= aa(jj(8,ka),abs(jj(4,ka)))
      v(4)= vv(jj(8,ka),abs(jj(5,ka)))
      a(4)= aa(jj(8,ka),abs(jj(5,ka)))
c-    the 8 possible abelian vertex combinations are built
      do i= 1,8
        vtx(i,ka)= (v(1)+jv(i,1)*a(1))*((v(2)*v(3)+a(2)*a(3))+
     +             jv(i,2)*(v(2)*a(3)+a(2)*v(3)))*
     +             (v(4)+jv(i,3)*a(4))
      enddo
c----------------------------------------------------------------c
c                                                                c
c    The occupation matrices are filled for the abelian          c
c    diagrams. In subroutine DIAGA only the permutations with    c
c    nc(np).ne.0 will be evaluated and, among the 4 independent  c
c    amplitudes, only those with noc(np,i).ne.0.                 c
c                                                                c
c----------------------------------------------------------------c
      do i= 1,4
        if (abs(vtx(i,ka)).gt.1.d-8.or.abs(vtx(9-i,ka)).
     +      gt.1.d-8) then
          noc(np,i)= 1
          nc(np)= 1
        endif
      enddo
  100 do 150 k= 1,4
        ln(k)= 0
  150 continue
c-    if 0 Feynman diagrams are found nopro= 1
      if (ka.eq.0.and.kn.eq.0) then
         if (nout.gt.0) then
            write (nout,*) '                  Zero Feynman diagrams '
         endif
         nopro= 1
         return
      else
         nopro= 0
      endif
      if (nout.gt.0) then
         write (nout,*)  '                                     '
         write (nout,*)
     &     '           abelian diagrams             phase '
         write (nout,*)  '                                     '
         write (nout,20) (i,ferm(abs(jj(1,i))),jj(9,i),
     +                 ferm(abs(jj(2,i))),jj(10,i),prop(jj(7,i)),
     +                 ferm(abs(jj(3,i))),jj(11,i),
     +                 ferm(int(abs(jj(3,i))+(2*isp(abs(jj(3,i)))-1)*
     +                     (1-mod(jj(7,i),3)/jj(7,i)))),
     +                 ferm(abs(jj(4,i))),jj(12,i),prop(jj(8,i)),
     +                 ferm(abs(jj(5,i))),jj(13,i),
     +                 ferm(abs(jj(6,i))),jj(14,i),jj(15,i),i=1,ka)
   20 FORMAT (1I3,':  ','[',A2,'(',I1,')',',',A2,'(',I1,')',']',1X,A1,1X
     +                 ,'[',a2,'(',i1,')',',',a2,',',a2,'(',i1,')',
     +        ']',1X,A1,1X ,'[',A2,'(',I1,')',',',A2,'(',I1,')',']',
     +                  '    ph=',i3)
         write (nout,*)  '                                     '
         write (nout,*)
     &     '           non abelian diagrams         phase '
         write (nout,*)  '                                     '
         write (nout,30) (i,ferm(abs(jn(1,i))),jn(8,i),
     +                 ferm(abs(jn(2,i))),jn(9,i),
     +                 ferm(abs(jn(3,i))),jn(10,i),
     +                 ferm(abs(jn(4,i))),jn(11,i),
     +                 ferm(abs(jn(5,i))),jn(12,i),
     +                 ferm(abs(jn(6,i))),jn(13,i),
     +                 prop(jn(7,i)),jn(14,i),i=1,kn)
 30      format (1i3,':  ','[',a2,'(',i1,')',',',a2,'(',i1,')',']'
     +                 ,1X,'[',A2,'(',I1,')',',',A2,'(',I1,')',']'
     +                 ,1X,'[',A2,'(',I1,')',',',A2,'(',I1,')',']'
     +                 ,' (W',a1,'W)','     ph=',i3)
      endif
c----------------------------------------------------------------c
c                                                                c
c    puts together abelian and non abelian occupation matrices   c
c    so that                                                     c
c                                                                c
c         nc(np)= 1 -->  np contributes to ab. diagrams          c
c         nc(np)= 2 -->  np contributes to non ab. diagrams      c
c         nc(np)= 3 -->  np contributes to both ab.              c
c                        and non ab diagrams                     c
c                                                                c
c    for further use in DIAGA .                                  c
c                                                                c
c----------------------------------------------------------------c
      do np= 1,36
        nc(np)= nc(np)+ncn(np)
      enddo
c----------------------------------------------------------------c
c                                                                c
c    The channels for the Monte Carlo integration are built      c
c    looking at the Feynman diagrams of the process.             c
c                                                                c
c----------------------------------------------------------------c
      do kd= 1,ka
        if (jj(9,kd).eq.1.and.jj(10,kd).eq.2) then
          ncha(jj(8,kd),nh(1,jj(9,kd)))= 1
        else if (jj(9,kd).eq.1.and.jj(12,kd).eq.2) then
          if (jj(7,kd).eq.2) then
             ncha(3+jj(8,kd),nh(1,jj(9,kd)))= 1
          else
             ncha(6+jj(8,kd),nh(3,jj(9,kd)))= 1
          endif
        else if (jj(9,kd).eq.1.and.jj(14,kd).eq.2) then
          if (jj(7,kd).eq.2) then
            if (jj(8,kd).eq.2) then
              ncha(18,nh(2,jj(9,kd)))= 1
            else
              ncha(19,nh(2,jj(9,kd)))= 1
            endif
          else
            if (jj(8,kd).eq.2) then
              ncha(19,nh(11,jj(9,kd)))= 1
            else
              ncha(20,nh(0,jj(9,kd)))= 1
            endif
          endif
        else if (jj(11,kd).eq.1.and.jj(10,kd).eq.2) then
          if (jj(7,kd).eq.2) then
            ncha(9+jj(8,kd),nh(7,jj(9,kd)))= 1
          else
            ncha(jj(8,kd),nh(1,jj(9,kd)))= 1
          endif
        else if (jj(11,kd).eq.1.and.jj(12,kd).eq.2) then
          if (jj(7,kd).eq.2) then
            ncha(12+jj(8,kd),nh(3,jj(9,kd)))= 1
          else if (jj(7,kd).eq.1) then
            if (jj(8,kd).eq.2) then
              ncha(13,nh(12,jj(9,kd)))= 1
            else
              ncha(16,nh(3,jj(9,kd)))= 1
            endif
          else
            ncha(17,nh(3,jj(9,kd)))= 1
          endif
        else if (jj(11,kd).eq.1.and.jj(14,kd).eq.2) then
          if (jj(8,kd).eq.2) then
            ncha(3+jj(7,kd),nh(10,jj(9,kd)))= 1
          else
            if (jj(7,kd).eq.2) then
              ncha(8,nh(12,jj(9,kd)))= 1
            else
              ncha(6+jj(7,kd),nh(6,jj(9,kd)))= 1
            endif
         endif
        else if (jj(13,kd).eq.1.and.jj(10,kd).eq.2) then
          if (jj(8,kd).eq.2) then
            if (jj(7,kd).eq.2) then
              ncha(18,nh(5,jj(9,kd)))= 1
            else
              ncha(19,nh(5,jj(9,kd)))= 1
            endif
          else
            if (jj(7,kd).eq.2) then
              ncha(19,nh(8,jj(9,kd)))= 1
            else
              ncha(20,nh(0,jj(9,kd)))= 1
            endif
          endif
        else if (jj(13,kd).eq.1.and.jj(12,kd).eq.2) then
          if (jj(8,kd).eq.2) then
            ncha(9+jj(7,kd),nh(4,jj(9,kd)))= 1
          else
            ncha(jj(7,kd),nh(4,jj(9,kd)))= 1
          endif
        else if (jj(13,kd).eq.1.and.jj(14,kd).eq.2) then
          ncha(jj(7,kd),nh(4,jj(9,kd)))= 1
        endif
      enddo
      do kd= 1,kn
        if (jn(10,kd).eq.1.and.jn(9,kd).eq.2) then
          if (jn(7,kd).eq.1) ncha(25,nh(14,jn(8,kd)))= 1
          if (jn(7,kd).eq.2) ncha(22,nh(15,jn(8,kd)))= 1
        else if (jn(10,kd).eq.1.and.jn(11,kd).eq.2) then
          ncha(21,nh(13,jn(8,kd)))= 1
        else if (jn(12,kd).eq.1.and.jn(9,kd).eq.2) then
          if (jn(7,kd).eq.1) ncha(24,nh(0,jn(8,kd)))= 1
          if (jn(7,kd).eq.2) ncha(23,nh(0,jn(8,kd)))= 1
        else if (jn(12,kd).eq.1.and.jn(11,kd).eq.2) then
          if (jn(7,kd).eq.1) ncha(25,nh(16,jn(8,kd)))= 1
          if (jn(7,kd).eq.2) ncha(22,nh(17,jn(8,kd)))= 1
        endif
      enddo
c-    and in order to include rambo4:
      ncha(26,1)= 1
c-------------------------------------------------------------------c
c                                                                   c
c    with a 4-quark final state (nz=1), currents putting together   c
c    (q(3) q_bar(4)) and (q(5) q_bar(6)) must be distiguished from  c
c    those linking  (q(3) q_bar(6)) and (q(5) q_bar(4)) in order    c
c    to compute the colour factor and introduce the interfering     c
c    QCD background.  nzza and nzzn do this.                        c
c    fnzz0 and fnzz1 are the QCD corrections.                       c
c                                                                   c
c-------------------------------------------------------------------c
      do kd= 1,ka
        if (nz.eq.1.AND.NL.LT.2) then
          if ((jj(9,kd)+jj(10,kd).eq.3.and.jj(8,kd).eq.2).
     +        or.(jj(13,kd)+jj(14,kd).eq.3.
     +        and.jj(7,kd).eq.2)) then
            fnzz1(kd)= grap/2.d0
            fnzz0(kd)= 1.d0-grap/6.d0
          else
            fnzz1(kd)= 0.d0
            fnzz0(kd)= 1.d0
          endif
          if (jj(9,kd).gt.2) then
            if (jj(10,kd).eq.jj(9,kd)+1) then
              nzza(kd)= 0
            else
              nzza(kd)= 1
            endif
          else
            if (jj(14,kd).eq.jj(13,kd)+1) then
              nzza(kd)= 0
            else
              nzza(kd)= 1
            endif
          endif
        else
          fnzz1(kd)= 0.d0
          fnzz0(kd)= 1.d0
          nzza(kd)= 1
* JBH look out for 4-lepton processes
          IF (NL.GT.2) THEN
            IF (JI(3).EQ.1.OR.JI(3).EQ.2) THEN
* e/e~ in the final state
              IF ((JJ(10,KD).EQ.JJ(9,KD)+1.AND.JJ(10,KD).GT.2.AND.JJ(9
     &          ,KD).GT.2).OR.(JJ(12,KD).EQ.JJ(11,KD)+1.AND.JJ(11,KD).GT
     &          .2.AND.JJ(12,KD).GT.2).OR.(JJ(14,KD).EQ.JJ(13,KD)+1.AND
     &          .JJ(13,KD).GT.2.AND.JJ(14,KD).GT.2)) THEN
                NZZA(KD)= 0
              ELSE
                NZZA(KD)= 1
              ENDIF
            ELSE
              IF (JJ(9,KD).GT.2) THEN
                IF (JJ(10,KD).EQ.JJ(9,KD)+1) THEN
                  NZZA(KD)= 0
                ELSE
                  NZZA(KD)= 1
                ENDIF
              ELSE
                IF (JJ(14,KD).EQ.JJ(13,KD)+1) THEN
                  NZZA(KD)= 0
                ELSE
                  NZZA(KD)= 1
                ENDIF
              endif
            ENDIF
          ENDIF
        endif
      enddo
      do kd= 1,kn
        if (nz.eq.1.AND.NL.LT.2) then
          if (jn(9,kd).eq.jn(8,kd)+1) then
            nzzn(kd)= 0
          else
            nzzn(kd)= 1
          endif
        else
          nzzn(kd)= 1
* JBH look out for 4-lepton processes
          IF (NL.GT.2) THEN
            IF (JI(3).EQ.1.OR.JI(3).EQ.2) THEN
* e/e~ in the final state
              IF ((JN(9,KD).EQ.JN(8,KD)+1.AND.JN(9,KD).GT.2.AND.JN(8
     &          ,KD).GT.2).OR.(JN(11,KD).EQ.JN(10,KD)+1.AND.JN(11,KD).GT
     &          .2.AND.JN(10,KD).GT.2).OR.(JN(13,KD).EQ.JN(12,KD)+1.AND
     &          .JN(13,KD).GT.2.AND.JN(12,KD).GT.2)) THEN
                NZZN(KD)= 0
              ELSE
                NZZN(KD)= 1
              ENDIF
            ELSE
              IF (JN(9,KD).EQ.JN(8,KD)+1) THEN
                NZZN(KD)= 0
              ELSE
                NZZN(KD)= 1
              ENDIF
            ENDIF
          ENDIF
        endif
      enddo
      if (nout.gt.0) then
         write (nout,*) '                                      '
         write (nout,*) '           kinematical diagrams       '
         write (nout,*) '                                      '
         write (nout,*) '     channel                     permutation'
         write (nout,*) '                                      '
      endif
c-    here you can switch off kinematical diagrams by putting nct0= 0
      
      nct= 0
      do 38 in= 1,26
        do 38 iin= 1,48
          if (ncha(in,iin).ne.0) then
            nct= nct+1
            ncm(nct)= in
            nnm(nct)= iin
            if (nct0(nct).eq.0) then
              do i= nct,ndab-1
                nct0(i)= nct0(i+1)
              enddo
              nct= nct-1
            else
               if (nout.gt.0) then
                  if (iin.le.24) then
                     write (nout,40) nct,kcname(in),(kf(kw,iin),kw=1,4)
                  else
                  write (nout,41) nct,kcname(in),(kf(kw,iin-24),kw=1,4)
                  endif
               endif
            endif
         endif
   38 continue
   40 FORMAT (1X,I3,':  ',A15,12X,' 1 2',4I2)
   41 FORMAT (1X,I3,':  ',A15,12X,' 2 1',4I2)
      return
      end
*
      subroutine excmtx(squarem,X1234,XINT,X1432,IMIX,QCD)
c-------------------------------------------------------------------c
c                                                                   c
c    The matrix element squared is computed putting together        c
c    numerators, denominators and vertex combinations.              c
c                                                                   c
c-------------------------------------------------------------------c
      implicit DOUBLE PRECISION (a-b,d-h,o-z)
      implicit complex*16 (c)
      parameter(ndab=144,nnab=100,ntot=150)
      common/aus/c0,c1,ci
      complex*16 c0,c1,ci
*      common/aus/c0,c1,ci,pi
      double precision pi
      parameter (pi=3.14159265358979324D0)
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      common/area2/vtxn(1:8,100),vtx(1:8,144)
      double precision vtxn,vtx
      common/area4/kperm(1:6,1:36),jj(1:16,1:144),jn(1:18,1:100),
     +             jv(1:8,1:3),nzza(144),nzzn(100)
      integer kperm,jj,jn,jv,nzza,nzzn
      common/area5/cnuma(1:36,1:4),cnumb(1:36,1:4),cnumc(1:36,1:4),
     +             cnumd(1:36,1:4),cden(1:36,1:4),cinp(1:36,0:2)
      complex*16 cnuma,cnumb,cnumc,cnumd,cden,cinp
      complex*16 ctot
      common/crencom/ctot(1:64,0:1)
      common/area6/ka,kn,nz
      integer ka,kn,nz
      common/qcd/fnzz0(144),fnzz1(144)
      double precision fnzz0,fnzz1
      dimension cres(1:64,0:1)
      dimension ks(1:3,1:8),ky(1:3,1:8)
* JBH
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
* Part of LUND common block
      INTEGER KCHG
      REAL    PMAS,PARF,VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
*
      PARAMETER (NCLAS=5)
      INTEGER ITEMP,HELI(4),IMIX(6)
      DOUBLE PRECISION DCOULM,X1234,XINT,X1432,XTEMP,XTEMP1,XTEMP2
     &  ,XTEMP3,QCD(2)
      COMPLEX*16 CTEMP(0:1)
*
      INTEGER IEXCLM,IEXABD,IEXNAD,NL,WWDIA
      COMMON /EXCSNG/ IEXCLM,IEXABD(144),IEXNAD(8),NL,WWDIA
      COMPLEX*16 EXCAMP,EXREST
      COMMON /JBHMTX/ EXCAMP(NCLAS,1:64,0:1),EXREST(NCLAS,1:64,0:1)
      EXTERNAL DCOULM
      ly(i1,i2,i3,i4,i5,i6)= 2-i6+2*(1-i5)+4*(1-i4)+8*(1-i3)+
     +                       16*(1-i2)+32*(1-i1)
      
c-    call numerators and denominators
      call diaga
      do i1= 1,64
        ctot(i1,0)= c0
        ctot(i1,1)= c0
* JBH: Reset arrays for contributions from different classes of diagrams
        DO I=1,NCLAS
          EXCAMP(I,I1,0)= C0
          EXCAMP(I,I1,1)= C0
          EXREST(I,I1,0)= C0
          EXREST(I,I1,1)= C0
        ENDDO
      enddo
      
c-    abelian amplitudes
      do kd= 1,ka
* JBH  
c        do i1= 1,64
c          cres(i1,0)= c0
c          cres(i1,1)= c0
c        enddo
        do 130 i= 1,3
        do 130 k= 1,8
          ks((jj(2*i+7,kd)+1)/2,k)= (jv(k,i)+1)/2
          ky(jj(2*i+8,kd)/2,k)= (jv(k,i)+1)/2
 130    continue
        cdrs= jj(15,kd)/(cden(jj(16,kd),1)-
     +        cm2(jj(7,kd),cden(jj(16,kd),1)))
     +        /cden(jj(16,kd),2)/(cden(jj(16,kd),3)-
     +        cm2(jj(8,kd),cden(jj(16,kd),3)))
        IF (IMIX(3).EQ.IMIX(4).AND.IMIX(3).EQ.2.AND.IMIX(5).GT.6.AND
     &    .IMIX(6).GT.6.AND.IMIX(5).NE.IMIX(6).AND.(JJ(7,KD).NE.3.OR
     &    .JJ(8,KD).NE.3)) THEN 
          CDRS=DCMPLX(0.0D0,0.0D0)
        ENDIF
* Remember that for abelian diagrams NO corrections are applied
* to vertices with 3 and 4 as quarks!
*JBH: CKM-mixing + QCD corrections to W
        IF ((ABS(JJ(1,KD)).GT.6.OR.ABS(JJ(3,KD)).GT.6
     &    .OR.ABS(JJ(5,KD)).GT.6).AND..NOT.(JJ(7,KD).EQ.3.AND.JJ(8,KD)
     &    .EQ.3.AND.ABS(JJ(3,KD)).GT.6.AND.ABS(JJ(4,KD)).GT
     &    .6)) THEN
          XTEMP=1.0D0
          IF (JJ(7,KD).EQ.3.AND.ABS(JJ(1,KD)).GT.6) THEN
            ITEMP=1
            IF (MOD(IMIX(JJ(8+ITEMP,KD)),2).EQ.1) ITEMP=2
            IF (IMIX(1).GT.0) XTEMP=XTEMP*SQRT(DBLE(VCKM((IMIX(JJ(8
     &        +ITEMP,KD))-6)/2,(IMIX(JJ(11-ITEMP,KD))-6)/2+1)))
            XTEMP=XTEMP*QCD(2)
          ENDIF
          IF (JJ(7,KD).EQ.3.AND.ABS(JJ(3,KD)).GT.6) THEN
            ITEMP=1
            IF (MOD(IMIX(JJ(10+ITEMP,KD)),2).EQ.1) ITEMP=2
            IF (IMIX(1).GT.0) XTEMP=XTEMP*SQRT(DBLE(VCKM((IMIX(JJ(10
     &        +ITEMP,KD))-6)/2,(IMIX(JJ(13-ITEMP,KD))-6)/2+1)))
            IF (JJ(9,KD).LT.3.OR.JJ(10,KD).LT.3) XTEMP=XTEMP*QCD(2)
          ENDIF
          IF (JJ(8,KD).EQ.3.AND.ABS(JJ(4,KD)).GT.6) THEN
            ITEMP=1
            IF (MOD(IMIX(JJ(10+ITEMP,KD)),2).EQ.1) ITEMP=2
            IF (IMIX(1).GT.0) XTEMP=XTEMP*SQRT(DBLE(VCKM((IMIX(JJ(10
     &        +ITEMP,KD))-6)/2,(IMIX(JJ(13-ITEMP,KD))-6)/2+1)))
            IF (JJ(13,KD).LT.3.OR.JJ(14,KD).LT.3) XTEMP=XTEMP*QCD(2)
          ENDIF
          IF (JJ(8,KD).EQ.3.AND.ABS(JJ(5,KD)).GT.6) THEN
            ITEMP=1
            IF (MOD(IMIX(JJ(12+ITEMP,KD)),2).EQ.1) ITEMP=2
            IF (IMIX(1).GT.0) XTEMP=XTEMP*SQRT(DBLE(VCKM((IMIX(JJ(12
     &        +ITEMP,KD))-6)/2,(IMIX(JJ(15-ITEMP,KD))-6)/2+1)))
            XTEMP=XTEMP*QCD(2)
          ENDIF
          CDRS=CDRS*DCMPLX(XTEMP,0.0D0)
        ELSEIF (JJ(7,KD).EQ.3.AND.JJ(8,KD).EQ.3.AND.ABS(JJ(3,KD)).GT
     &      .6.AND.ABS(JJ(4,KD)).GT.6) THEN
          CDRS=CDRS*DCMPLX(QCD(2)*QCD(2),0.0D0)
          IF (ABS(IMIX(5)).NE.ABS(IMIX(6)).AND.IMIX(3).EQ.IMIX(4).AND
     &      .IMIX(3).EQ.2) THEN
            XTEMP=0.0D0
            DO ITEMP=1,3-MOD(ABS(IMIX(5)),2)
              IF (MOD(ABS(IMIX(5)),2).EQ.0) THEN
                XTEMP=XTEMP+SQRT(DBLE(VCKM((IMIX(5)-6)/2,ITEMP)
     &            *VCKM((IMIX(6)-6)/2,ITEMP)))
              ELSE
                XTEMP=XTEMP+SQRT(DBLE(VCKM(ITEMP,(IMIX(5)-6)/2+1)
     &            *VCKM(ITEMP,(IMIX(6)-6)/2+1)))
              ENDIF
            ENDDO
            CDRS=CDRS*DCMPLX(XTEMP,0.0D0)
          ELSEIF (IMIX(5).EQ.IMIX(6).AND.MOD(IMIX(5),2).EQ.1.AND.IMIX(3)
     &        .EQ.IMIX(4).AND.IMIX(3).EQ.2) THEN
            CDRS=CDRS*DCMPLX(DBLE(VCKM(1,(IMIX(5)-6)/2+1)+VCKM(2,(IMIX(5
     &        )-6)/2+1)),0.0D0)
          ENDIF
        ENDIF
*JBH: QCD corrections to Z/gamma
        IF ((JJ(7,KD).NE.3.AND.(ABS(JJ(1,KD)).GT.6.OR.ABS(JJ(3,KD)).GT.6
     &    )).OR.(JJ(8,KD).NE.3.AND.(ABS(JJ(4,KD)).GT.6.OR.ABS(JJ(5,KD))
     &    .GT.6))) THEN
          XTEMP=1.0D0
          IF (JJ(7,KD).NE.3.AND.ABS(JJ(1,KD)).GT.6) XTEMP=XTEMP*QCD(1)
          IF (JJ(7,KD).NE.3.AND.ABS(JJ(3,KD)).GT.6.AND.(JJ(9,KD).LT.3.OR
     &      .JJ(10,KD).LT.3)) XTEMP=XTEMP*QCD(1)
          IF (JJ(8,KD).NE.3.AND.ABS(JJ(4,KD)).GT.6.AND.(JJ(13,KD).LT.3
     &      .OR.JJ(14,KD).LT.3)) XTEMP=XTEMP*QCD(1)
          IF (JJ(8,KD).NE.3.AND.ABS(JJ(5,KD)).GT.6) XTEMP=XTEMP*QCD(1)
          CDRS=CDRS*DCMPLX(XTEMP,0.0D0)
        ENDIF
        do i= 1,8
          ii= i-int(i/4.-0.001)*(2*i-9)
          cres(ly(ks(1,i),ks(2,i),ks(3,i),ky(1,i),ky(2,i),
     +    ky(3,i)),nzza(kd))= dcmplx(dreal(cnuma(jj(16,kd),ii)),jv(i,2)*
     +      dimag(cnuma(jj(16,kd),ii)))*vtx(i,kd)*cdrs
c        enddo       
c        do i1= 1,64
          I1=LY(KS(1,I),KS(2,I),KS(3,I),KY(1,I),KY(2,I),KY(3,I))
          CTEMP(NZZA(KD))=FNZZ0(KD)*CRES(I1,NZZA(KD))
          CTEMP(1-NZZA(KD))=FNZZ1(KD)*CRES(I1,NZZA(KD))
* JBH: Single out the different classes of diagrams (in particular WW)
          IF (IEXABD(KD).NE.0) THEN
            EXCAMP(IEXABD(KD),I1,NZZA(KD))=EXCAMP(IEXABD(KD),I1,NZZA(KD)
     &        )+CTEMP(NZZA(KD))
            EXCAMP(IEXABD(KD),I1,1-NZZA(KD))=EXCAMP(IEXABD(KD),I1,1
     &        -NZZA(KD))+CTEMP(1-NZZA(KD))
          ENDIF
          DO I2=1,NCLAS
            IF (IEXABD(KD).NE.I2) THEN
              EXREST(I2,I1,NZZA(KD))= EXREST(I2,I1,NZZA(KD))
     &          +CTEMP(NZZA(KD))
              EXREST(I2,I1,1-NZZA(KD))= EXREST(I2,I1,1-NZZA(KD))+CTEMP(1
     &          -NZZA(KD))
            ENDIF
          ENDDO
          ctot(i1,nzza(kd))= ctot(i1,nzza(kd))+
     +                       CTEMP(NZZA(KD))
          ctot(i1,1-nzza(kd))= ctot(i1,1-nzza(kd))+
     +                       CTEMP(1-NZZA(KD))
        enddo
      enddo
c-    non abelian amplitudes
      IF (.NOT.(IMIX(3).EQ.IMIX(4).AND.IMIX(3).EQ.2.AND.IMIX(5).GT.6.AND
     &  .IMIX(6).GT.6.AND.IMIX(5).NE.IMIX(6))) THEN
        XTEMP3=1.0D0
        call nonabamp(IMIX,QCD)
      ELSE
        XTEMP3=0.5D0
      ENDIF
c-    squaring
c      squarem= 0.d0
* JBH: Keep track of the different diagram contributions AND their
*      interference
      DO I=1,NCLAS
        EXCSIG(1,I)=0.0D0
        EXCSIG(2,I)=0.0D0
        EXCSIG(3,I)=0.0D0
      ENDDO
* JBH: Keep track of class 1 and class 2 contributions AND their
*      interference
      X1234=0.0D0
      XINT=0.0D0
      X1432=0.0D0
* JBH: Reset arrays for helicity amplitude decomposition 
      DO I=1,16
        EXHELI(I)=0.0D0
      ENDDO
*JBH: Get coulumb-correction...
      DCOU=1.0D0
      IF (IEXCLM.GT.0.AND.WWDIA.NE.0) DCOU=1.0D0+DCOULM(1)
      do i1= 1,64
C        squarem= squarem+ctot(i1,1)*conjg(ctot(i1,1))
C        if (nz.eq.1) squarem= squarem+ctot(i1,0)*conjg(ctot(i1,0))+
C     +      2.d0/3.d0*real(conjg(ctot(i1,0))*ctot(i1,1))
* JBH: Update diagram contributions
        DO I=1,NCLAS
          EXCSIG(1,I)=EXCSIG(1,I)+EXCAMP(I,I1,1)
     &      *CONJG(EXCAMP(I,I1,1))
          EXCSIG(2,I)=EXCSIG(2,I)+EXREST(I,I1,1)
     &      *CONJG(EXREST(I,I1,1))
          EXCSIG(3,I)=EXCSIG(3,I)+2.0D0*DBLE(EXCAMP(I,I1,1)
     &      *CONJG(EXREST(I,I1,1)))
          IF (NZ.EQ.1) THEN
            EXCSIG(1,I)=EXCSIG(1,I)+DBLE(NL)*2.0D0/3.0D0
     &        *DBLE(EXCAMP(I,I1,1)*CONJG(EXCAMP(I,I1,0)))
     &        +EXCAMP(I,I1,0)*CONJG(EXCAMP(I,I1,0))
            EXCSIG(2,I)=EXCSIG(2,I)+DBLE(NL)*2.0D0/3.0D0
     &        *DBLE(EXREST(I,I1,1)*CONJG(EXREST(I,I1,0)))
     &        +EXREST(I,I1,0)*CONJG(EXREST(I,I1,0))
            EXCSIG(3,I)=EXCSIG(3,I)+DBLE(NL)*2.0D0/3.0D0
     &        *DBLE(EXCAMP(I,I1,1)*CONJG(EXREST(I,I1,0))
     &        +EXREST(I,I1,1)*CONJG(EXCAMP(I,I1,0)))+2.0D0
     &        *DBLE(EXCAMP(I,I1,0)*CONJG(EXREST(I,I1,0)))
          ENDIF
        ENDDO
* JBH: Update pairing class contributions
        IF (IEXCLM.GT.0.AND.WWDIA.NE.0) THEN
*JBH: Apply coulumb-correction...
          XTEMP=DCOU*EXCAMP(1,I1,1)*CONJG(EXCAMP(1,I1,1))+EXREST(1,I1,1)
     &      *CONJG(EXREST(1,I1,1))+2.0D0*DBLE(EXCAMP(1,I1,1)
     &      *CONJG(EXREST(1,I1,1)))
          X1234=X1234+XTEMP
          IF (NZ.EQ.1) THEN
            XTEMP1=DBLE(NL)*2.0D0/3.0D0*(DCOU*DBLE(EXCAMP(1,I1,1)
     &        *CONJG(EXCAMP(1,I1,0)))+DBLE(EXCAMP(1,I1,1)
     &        *CONJG(EXREST(1,I1,0))+EXREST(1,I1,1)*CONJG(EXCAMP(1,I1,0)
     &        )+EXREST(1,I1,1)*CONJG(EXREST(1,I1,0))))
            XTEMP2=DCOU*EXCAMP(1,I1,0)*CONJG(EXCAMP(1,I1,0))+EXREST(1,I1
     &        ,0)*CONJG(EXREST(1,I1,0))+2.0D0*DBLE(EXCAMP(1,I1,0)
     &        *CONJG(EXREST(1,I1,0)))
            XINT=XINT+XTEMP1
            X1432=X1432+XTEMP2
            XTEMP= XTEMP+XTEMP1+XTEMP2
          ENDIF          
        ELSE
          X1234=X1234+CTOT(I1,1)*CONJG(CTOT(I1,1))
          XTEMP=CTOT(I1,1)*CONJG(CTOT(I1,1))
          IF (NZ.EQ.1) THEN
            XINT=XINT+DBLE(NL)*2.0D0/3.0D0*DBLE(CONJG(CTOT(I1,0))
     &        *CTOT(I1,1))
            X1432=X1432+CTOT(I1,0)*CONJG(CTOT(I1,0))
            XTEMP=XTEMP+CTOT(I1,0)*CONJG(CTOT(I1,0))+
     +        DBLE(NL)*2.D0/3.D0*DREAL(CONJG(CTOT(I1,0))*CTOT(I1,1))
          ENDIF
        ENDIF
* JBH - Helicity amplitude decomposition
* Order (remember that anti-particles have opposite sign...)
* ++++,+++-,++-+,++--,+-++,+--+,+-+-,+---
* -+++,-++-,-+-+,-+--,--++,---+,--+-,----
        HELI(1)=ISIGN(1,1-2*(MOD(I1-1,32)/16))
        HELI(2)=ISIGN(1,2*(MOD(I1-1,4)/2)-1)
        HELI(3)=ISIGN(1,1-2*(MOD(I1-1,16)/8))
        HELI(4)=ISIGN(1,2*MOD(I1-1,2)-1)
        IF (HELI(1).EQ.1) THEN
          IF (HELI(2).EQ.1.AND.HELI(3).EQ.1.AND.HELI(4).EQ.1) EXHELI(1)
     &      =EXHELI(1)+XTEMP
          IF (HELI(2).EQ.1.AND.HELI(3).EQ.1.AND.HELI(4).EQ.-1) EXHELI(2)
     &      =EXHELI(2)+XTEMP
          IF (HELI(2).EQ.1.AND.HELI(3).EQ.-1.AND.HELI(4).EQ.1) EXHELI(3)
     &      =EXHELI(3)+XTEMP
          IF (HELI(2).EQ.1.AND.HELI(3).EQ.-1.AND.HELI(4).EQ.-1) EXHELI(4
     &      )=EXHELI(4)+XTEMP
          IF (HELI(2).EQ.-1.AND.HELI(3).EQ.1.AND.HELI(4).EQ.1) EXHELI(5)
     &      =EXHELI(5)+XTEMP
          IF (HELI(2).EQ.-1.AND.HELI(3).EQ.-1.AND.HELI(4).EQ.1) EXHELI(6
     &      )=EXHELI(6)+XTEMP
          IF (HELI(2).EQ.-1.AND.HELI(3).EQ.1.AND.HELI(4).EQ.-1) EXHELI(7
     &      )=EXHELI(7)+XTEMP
          IF (HELI(2).EQ.-1.AND.HELI(3).EQ.-1.AND.HELI(4).EQ.-1)
     &      EXHELI(8)=EXHELI(8)+XTEMP
        ELSE
          IF (HELI(2).EQ.1.AND.HELI(3).EQ.1.AND.HELI(4).EQ.1) EXHELI(9)
     &      =EXHELI(9)+XTEMP
          IF (HELI(2).EQ.1.AND.HELI(3).EQ.1.AND.HELI(4).EQ.-1) EXHELI(10
     &      )=EXHELI(10)+XTEMP
          IF (HELI(2).EQ.1.AND.HELI(3).EQ.-1.AND.HELI(4).EQ.1) EXHELI(11
     &      )=EXHELI(11)+XTEMP
          IF (HELI(2).EQ.1.AND.HELI(3).EQ.-1.AND.HELI(4).EQ.-1)
     &      EXHELI(12)=EXHELI(12)+XTEMP
          IF (HELI(2).EQ.-1.AND.HELI(3).EQ.1.AND.HELI(4).EQ.1) EXHELI(13
     &      )=EXHELI(13)+XTEMP
          IF (HELI(2).EQ.-1.AND.HELI(3).EQ.-1.AND.HELI(4).EQ.1)
     &      EXHELI(14)=EXHELI(14)+XTEMP
          IF (HELI(2).EQ.-1.AND.HELI(3).EQ.1.AND.HELI(4).EQ.-1)
     &      EXHELI(15)=EXHELI(15)+XTEMP
          IF (HELI(2).EQ.-1.AND.HELI(3).EQ.-1.AND.HELI(4).EQ.-1)
     &      EXHELI(16)=EXHELI(16)+XTEMP
        ENDIF
      enddo
      X1234=X1234*XTEMP3
      XINT=XINT*XTEMP3
      X1432=X1432*XTEMP3
      SQUAREM= X1234+XINT+X1432
* JBH: Watch out for coulumb-correction
      EXCSIG(1,1)=EXCSIG(1,1)*DCOU
      DO I=2,NCLAS
        EXCSIG(2,I)=SQUAREM-EXCSIG(1,I)-EXCSIG(3,I)
      ENDDO
      return
      end
*
      subroutine nonabamp(IMIX,QCD)
c------------------------------------------------------------------c
c                      non abelian amplitudes                      c
c------------------------------------------------------------------c
c
c   June 6, 1995: changed dz, xz, yz, zz in -dz, -xz, -yz, -zz
c                 to get agreement with Bilenky et al.
c
      implicit DOUBLE PRECISION (a-b,d-h,o-z)
      implicit complex*16 (c)
      parameter(ndab=144,nnab=100,ntot=150)
      common/aus/c0,c1,ci
      complex*16 c0,c1,ci
*      common/aus/c0,c1,ci,pi
      double precision pi
      parameter (pi=3.14159265358979324D0)
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      common/area4/kperm(1:6,1:36),jj(1:16,1:144),jn(1:18,1:100),
     +             jv(1:8,1:3),nzza(144),nzzn(100)
      integer kperm,jj,jn,jv,nzza,nzzn
      common/area5/cnuma(1:36,1:4),cnumb(1:36,1:4),cnumc(1:36,1:4),
     +             cnumd(1:36,1:4),cden(1:36,1:4),cinp(1:36,0:2)
      complex*16 cnuma,cnumb,cnumc,cnumd,cden,cinp
      common/area6/ka,kn,nz
      integer ka,kn,nz
      common/area11/dz,xg,yg,xz,yz,zz
      double precision dz,xg,yg,xz,yz,zz
      complex*16 ctot
      common/crencom/ctot(1:64,0:1)
      dimension crena(1:64,0:1),crenb(1:64,0:1),crenc(1:64,0:1),
     +          crend(1:64,0:1),crene(1:64,0:1)
      dimension ks(1:3,1:8),ky(1:3,1:8)
* JBH
* Part of LUND common block
      INTEGER KCHG
      REAL    PMAS,PARF,VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
*
      INTEGER IMIX(6)
      DOUBLE PRECISION QCD(2)
      
      PARAMETER (NCLAS=5)
      INTEGER ITEMP
      DOUBLE PRECISION XTEMP
*
      INTEGER IEXCLM,IEXABD,IEXNAD,NL,WWDIA
      COMMON /EXCSNG/ IEXCLM,IEXABD(144),IEXNAD(8),NL,WWDIA
      COMPLEX*16 EXCAMP,EXREST
      COMMON /JBHMTX/ EXCAMP(NCLAS,1:64,0:1),EXREST(NCLAS,1:64,0:1)
      
      ly(i1,i2,i3,i4,i5,i6)= 2-i6+2*(1-i5)+4*(1-i4)+8*(1-i3)+
     +                       16*(1-i2)+32*(1-i1)
      
      do kb= 1,kn
* JBH
c        do i1= 1,64
c          crena(i1,0)= c0
c          crena(i1,1)= c0
c          crenb(i1,0)= c0
c          crenb(i1,1)= c0
c          crenc(i1,0)= c0
c          crenc(i1,1)= c0
c          crend(i1,0)= c0
c          crend(i1,1)= c0
c          crene(i1,0)= c0
c          crene(i1,1)= c0
c        enddo
        do 140 i= 1,3
        do 140 k=1,8
          ks((jn(2*i+6,kb)+1)/2,k)= (jv(k,i)+1)/2
          ky(jn(2*i+7,kb)/2,k)= (jv(k,i)+1)/2
  140   continue
        cdrs= jn(14,kb)/(cden(jn(15,kb),1)-
     +        cm2(3,cden(jn(15,kb),1)))/
     +        (cden(jn(15,kb),3)-
     +        cm2(3,cden(jn(15,kb),3)))/
     +        (cden(jn(15,kb),4)-
     +        cm2(jn(7,kb),cden(jn(15,kb),4)))
*JBH: CKM-mixing
        IF (ABS(JN(1,KB)).GT.6.OR.ABS(JN(5,KB)).GT.6) THEN
          XTEMP=1.0D0
          IF (ABS(JN(1,KB)).GT.6) THEN
            ITEMP=1
            IF (MOD(IMIX(JN(7+ITEMP,KB)),2).EQ.1) ITEMP=2
            IF (IMIX(1).GT.0) XTEMP=XTEMP*SQRT(DBLE(VCKM((IMIX(JN(7
     &        +ITEMP,KB))-6)/2,(IMIX(JN(10-ITEMP,KB))-6)/2+1)))
            XTEMP=XTEMP*QCD(2)
          ENDIF
          IF (ABS(JN(5,KB)).GT.6) THEN
            ITEMP=1
            IF (MOD(IMIX(JN(11+ITEMP,KB)),2).EQ.1) ITEMP=2
            IF (IMIX(1).GT.0) XTEMP=XTEMP*SQRT(DBLE(VCKM((IMIX(JN(11
     &        +ITEMP,KB))-6)/2,(IMIX(JN(14-ITEMP,KB))-6)/2+1)))
            XTEMP=XTEMP*QCD(2)
          ENDIF
          CDRS=CDRS*DCMPLX(XTEMP,0.0D0)
        ENDIF
        sth= sqrt(sth2)
        acth= sqrt(1.d0-sth2)
c- Z
        if (jn(7,kb).eq.1) then
          vxa= -acth-dz*sth
          vxb= -sth*xz
          vxc= -sth*yz/wmi/wmi
          vxd= sth*zz/wmi/wmi
        endif
c- photon
        if (jn(7,kb).eq.2) then
          vxa= sth
          vxb= sth*xg
          vxc= sth*yg/wmi/wmi
          vxd= 0.d0
        endif
        do i= 1,8
          i1= i-int(i/4.-0.001)*(2*i-9)
          i2= abs(i1-2*(i1-1)*(i1-3))
          i3= abs(i1+5*(i1-1)*(i1-4)/2)
          i4= abs(i1-7*(1-isign(1,2-i1))/2)
          index= ly(ks(1,i),ks(2,i),ks(3,i),ky(1,i),ky(2,i),ky(3,i))
          call cren(i,i1,i2,i3,i4,kb,vxa,vxb,vxc,vxd,index,cdrs,
     +              crena,crenb,crenc,crend,crene)
c        enddo
c        do i1= 1,64
          I1=INDEX
          CTEMP= CRENA(I1,NZZN(KB))+
     +      CRENB(I1,NZZN(KB))+
     +      CRENC(I1,NZZN(KB))+
     +      CREND(I1,NZZN(KB))+
     +      CRENE(I1,NZZN(KB))
* JBH: Single out the different classes of diagrams (in particular WW)
          IF (IEXNAD(KB).NE.0) THEN
            EXCAMP(IEXNAD(KB),I1,NZZN(KB))=EXCAMP(IEXNAD(KB),I1,NZZN(KB)
     &        )+CTEMP
          ENDIF
          DO I2=1,NCLAS
            IF (IEXNAD(KB).NE.I2) EXREST(I2,I1,NZZN(KB))= EXREST(I2,I1
     &        ,NZZN(KB))+CTEMP
          ENDDO
          ctot(i1,nzzn(kb))= ctot(i1,nzzn(kb))+CTEMP
        enddo
      enddo
      return
      end
*
      subroutine cren(i,i1,i2,i3,i4,kb,vxa,vxb,vxc,vxd,index,cdrs,
     +                crena,crenb,crenc,crend,crene)
c------------------------------------------------------------------c
c                                                                  c
c------------------------------------------------------------------c
      implicit DOUBLE PRECISION (a-b,d-h,o-z)
      implicit complex*16 (c)
      parameter(ndab=144,nnab=100,ntot=150)
      common/aus/c0,c1,ci
      complex*16 c0,c1,ci
*      common/aus/c0,c1,ci,pi
      double precision pi
      parameter (pi=3.14159265358979324D0)
      common/area2/vtxn(1:8,100),vtx(1:8,144)
      double precision vtxn,vtx
      common/area4/kperm(1:6,1:36),jj(1:16,1:144),jn(1:18,1:100),
     +             jv(1:8,1:3),nzza(144),nzzn(100)
      integer kperm,jj,jn,jv,nzza,nzzn
      common/area5/cnuma(1:36,1:4),cnumb(1:36,1:4),cnumc(1:36,1:4),
     +             cnumd(1:36,1:4),cden(1:36,1:4),cinp(1:36,0:2)
      complex*16 cnuma,cnumb,cnumc,cnumd,cden,cinp
      dimension crena(1:64,0:1),crenb(1:64,0:1),crenc(1:64,0:1),
     +          crend(1:64,0:1),crene(1:64,0:1)
      crena(index,nzzn(kb))= dcmplx(dreal(cnuma(jn(15,kb),i1)-
     +  cnuma(jn(16,kb),i2)),jv(i,2)*dimag(cnuma(jn(15,kb),i1)
     +  -cnuma(jn(16,kb),i2)))*vtxn(i,kb)*vxa*cdrs
      crenb(index,nzzn(kb))= dcmplx(dreal(cnumb(jn(15,kb),i1)),
     +  jv(i,2)*dimag(cnumb(jn(15,kb),i1)))*vtxn(i,kb)*vxb*cdrs
      crenc(index,nzzn(kb))=
     +  (cinp(jn(15,kb),0)*dcmplx(dreal(cnumb(jn(17,kb),i3)),
     +                     dimag(jv(i,3)*cnumb(jn(17,kb),i3)))-
     +   cinp(jn(15,kb),1)*dcmplx(dreal(cnumb(jn(15,kb),i1)),
     +                     dimag(jv(i,2)*cnumb(jn(15,kb),i1)))+
     +   cinp(jn(15,kb),2)*dcmplx(dreal(cnumb(jn(18,kb),i4)),
     +                     dimag(jv(i,1)*cnumb(jn(18,kb),i4))))*
     +  vtxn(i,kb)*vxc*cdrs
      crend(index,nzzn(kb))= dcmplx(dreal(cnumc(jn(15,kb),i1)),
     +  jv(i,2)*dimag(cnumc(jn(15,kb),i1)))*vtxn(i,kb)*vxc*cdrs
      crene(index,nzzn(kb))= dcmplx(dreal(cnumd(jn(15,kb),i1)+
     +  cnumd(jn(16,kb),i2)),jv(i,2)*dimag(cnumd(jn(15,kb),i1)+
     +  cnumd(jn(16,kb),i2)))*vtxn(i,kb)*vxd*cdrs*ci
      return
      end
*
      subroutine diaga
c-------------------------------------------------------------------c
c                                                                   c
c    All numerators and denominators of all diagrams are computed   c
c    at once, using the Weyl-van der Waerden formalism, only when   c
c    noc(1:36,1:4) and nc(1:36) are different from 0.               c
c                                                                   c
c-------------------------------------------------------------------c
      implicit DOUBLE PRECISION (a-b,d-h,o-z)
      implicit complex*16 (c)
      parameter(ndab=144,nnab=100,ntot=150)
      common/aus/c0,c1,ci
      complex*16 c0,c1,ci
*      common/aus/c0,c1,ci,pi
      double precision pi
      parameter (pi=3.14159265358979324D0)
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      common/area3/noc(1:36,1:4),nc(1:36)
      integer noc,nc
      common/area4/kperm(1:6,1:36),jj(1:16,1:144),jn(1:18,1:100),
     +             jv(1:8,1:3),nzza(144),nzzn(100)
      integer kperm,jj,jn,jv,nzza,nzzn
      common/area5/cnuma(1:36,1:4),cnumb(1:36,1:4),cnumc(1:36,1:4),
     +             cnumd(1:36,1:4),cden(1:36,1:4),cinp(1:36,0:2)
      complex*16 cnuma,cnumb,cnumc,cnumd,cden,cinp
      common/area11/dz,xg,yg,xz,yz,zz
      double precision dz,xg,yg,xz,yz,zz
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
      dimension crp(1:6),ja(1:6),cp(1:6,1:6)
      dimension cpp(1:6,1:6),cppc(1:6,1:6)
      dimension p(1:6,0:3)
      do 134 j= 1,6
        if (j.gt.2) ph=  1.d0
        if (j.le.2) ph= -1.d0
        do 134 k= 0,3
          p(j,k)= ph*pm(k,j)
  134 continue
      do i=1,6
        crp(i)= sqrt(c1*(p(i,0)-p(i,3)))
      enddo
c-    the Weyl-van der Waerden inner products are evaluated
      do 135 i=1,6
        do 135 j= 1,6
          if (i.ne.j) then
            cp(i,j)= crp(i)/crp(j)*(c1*p(j,1)-ci*p(j,2))
     +              -crp(j)/crp(i)*(c1*p(i,1)-ci*p(i,2))
          else
            cp(i,j)= c0
          endif
  135 continue
      do np= 1,36
        cinp(np,0)= c0
        cinp(np,1)= c0
        cinp(np,2)= c0
        do i=1,4
          cnuma(np,i)= c0
          cnumb(np,i)= c0
          cnumc(np,i)= c0
          cnumd(np,i)= c0
        enddo
        if (nc(np).eq.0) goto 123
        do i= 1,6
          ja(i)= kperm((i+6-5*mod(abs(i),2))/2,np)
        enddo
        do 136 i= 1,6
        do 136 k= 1,6
          if (i.ne.k) then
            if (ja(i)+ja(k).eq.3) then
              cf= -c1
            else if (ja(i).le.2.or.ja(k).le.2) then
              cf= ci
            else
              cf= c1
            endif
            cpp(i,k)= cp(ja(i),ja(k))*cf
            cppc(i,k)= conjg(cp(ja(i),ja(k)))*cf
          endif
  136   continue
c-      denominators
        cden(np,1)= cpp(1,2)*cppc(1,2)
        cden(np,3)= cpp(5,6)*cppc(5,6)
        if (nc(np).eq.1.or.nc(np).eq.3) cden(np,2)=
     +      cden(np,1)+cpp(1,3)*cppc(1,3)+cpp(2,3)*cppc(2,3)
        if (nc(np).eq.2.or.nc(np).eq.3) cden(np,4)=
     +      cpp(3,4)*cppc(3,4)
c-      numerators
        if (noc(np,1).eq.1) cnuma(np,1)= 4.d0*cppc(3,1)*
     +     cpp(4,6)*(cppc(5,1)*cpp(2,1)+cppc(5,3)*cpp(2,3))
        if (noc(np,2).eq.1) cnuma(np,2)= 4.d0*cppc(3,1)*
     +      cpp(4,5)*(cppc(6,1)*cpp(2,1)+cppc(6,3)*cpp(2,3))
        if (noc(np,3).eq.1) cnuma(np,3)= 4.d0*cppc(3,2)*
     +      cpp(4,5)*(cppc(6,2)*cpp(1,2)+cppc(6,3)*cpp(1,3))
        if (noc(np,4).eq.1) cnuma(np,4)= 4.d0*cppc(3,2)*
     +      cpp(4,6)*(cppc(5,2)*cpp(1,2)+cppc(5,3)*cpp(1,3))
c-      only abelian diagrams for permutation np
c       so no anomalous couplings
        if (nc(np).eq.1) goto 123
        if ((abs(xg).gt.0.d0).or.(abs(yg).gt.0.d0).or.
     +      (abs(xz).gt.0.d0).or.(abs(yz).gt.0.d0)) then
          if (noc(np,1).eq.1) cnumb(np,1)=
     +       2.d0*cppc(3,1)*cppc(3,5)*cpp(2,6)*cpp(3,4)
     +      +2.d0*cppc(4,3)*cppc(1,5)*cpp(4,2)*cpp(4,6)
          if (noc(np,2).eq.1) cnumb(np,2)=
     +       2.d0*cppc(3,1)*cppc(3,6)*cpp(2,5)*cpp(3,4)
     +      +2.d0*cppc(4,3)*cppc(1,6)*cpp(4,2)*cpp(4,5)
          if (noc(np,3).eq.1) cnumb(np,3)=
     +       2.d0*cppc(3,2)*cppc(3,6)*cpp(1,5)*cpp(3,4)
     +      +2.d0*cppc(4,3)*cppc(2,6)*cpp(4,1)*cpp(4,5)
          if (noc(np,4).eq.1) cnumb(np,4)=
     +       2.d0*cppc(3,2)*cppc(3,5)*cpp(1,6)*cpp(3,4)
     +      +2.d0*cppc(4,3)*cppc(2,5)*cpp(4,1)*cpp(4,6)
        endif
        if ((abs(yg).gt.0.d0).or.(abs(yz).gt.0.d0)) then
          cinp(np,0)= 0.5d0*
     +      (cppc(3,1)*cpp(3,1)+cppc(3,2)*cpp(3,2)+
     +       cppc(4,1)*cpp(4,1)+cppc(4,2)*cpp(4,2))
          cinp(np,1)= 0.5d0*
     +      (cppc(5,1)*cpp(5,1)+cppc(5,2)*cpp(5,2)+
     +       cppc(6,1)*cpp(6,1)+cppc(6,2)*cpp(6,2))
          cinp(np,2)= 0.5d0*
     +      (cppc(3,5)*cpp(3,5)+cppc(3,6)*cpp(3,6)+
     +       cppc(4,5)*cpp(4,5)+cppc(4,6)*cpp(4,6))
          if (noc(np,1).eq.1) cnumc(np,1)= 2.d0*
     +      (cppc(1,3)*cpp(2,3)+cppc(1,4)*cpp(2,4))*
     +      (cppc(3,5)*cpp(4,5)+cppc(3,6)*cpp(4,6))*
     +      (cppc(5,1)*cpp(6,1)+cppc(5,2)*cpp(6,2))
          if (noc(np,2).eq.1) cnumc(np,2)= 2.d0*
     +      (cppc(1,3)*cpp(2,3)+cppc(1,4)*cpp(2,4))*
     +      (cppc(3,6)*cpp(4,6)+cppc(3,5)*cpp(4,5))*
     +      (cppc(6,1)*cpp(5,1)+cppc(6,2)*cpp(5,2))
          if (noc(np,3).eq.1) cnumc(np,3)= 2.d0*
     +      (cppc(2,3)*cpp(1,3)+cppc(2,4)*cpp(1,4))*
     +      (cppc(3,6)*cpp(4,6)+cppc(3,5)*cpp(4,5))*
     +      (cppc(6,2)*cpp(5,2)+cppc(6,1)*cpp(5,1))
          if (noc(np,4).eq.1) cnumc(np,4)= 2.d0*
     +      (cppc(2,3)*cpp(1,3)+cppc(2,4)*cpp(1,4))*
     +      (cppc(3,5)*cpp(4,5)+cppc(3,6)*cpp(4,6))*
     +      (cppc(5,2)*cpp(6,2)+cppc(5,1)*cpp(6,1))
        endif
        if (abs(zz).gt.0.d0) then
          if (noc(np,1).eq.1) cnumd(np,1)= 2.d0*ci*
     +      (cppc(1,3)*cpp(2,3)+cppc(1,4)*cpp(2,4))*
     +      (cppc(3,5)*cppc(3,5)*cpp(3,4)*cpp(5,6)
     +       -cpp(4,6)*cpp(4,6)*cppc(3,4)*cppc(5,6))
          if (noc(np,2).eq.1) cnumd(np,2)= 2.d0*ci*
     +      (cppc(1,3)*cpp(2,3)+cppc(1,4)*cpp(2,4))*
     +      (cppc(3,6)*cppc(3,6)*cpp(3,4)*cpp(6,5)
     +       -cpp(4,5)*cpp(4,5)*cppc(3,4)*cppc(6,5))
          if (noc(np,3).eq.1) cnumd(np,3)= 2.d0*ci*
     +      (cppc(2,3)*cpp(1,3)+cppc(2,4)*cpp(1,4))*
     +      (cppc(3,6)*cppc(3,6)*cpp(3,4)*cpp(6,5)
     +       -cpp(4,5)*cpp(4,5)*cppc(3,4)*cppc(6,5))
          if (noc(np,4).eq.1) cnumd(np,4)= 2.d0*ci*
     +      (cppc(2,3)*cpp(1,3)+cppc(2,4)*cpp(1,4))*
     +      (cppc(3,5)*cppc(3,5)*cpp(3,4)*cpp(5,6)
     +       -cpp(4,6)*cpp(4,6)*cppc(3,4)*cppc(5,6))
        endif
 123  enddo
  358 continue
      return
      end
*
      subroutine rambo4(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c   Isotropic 4-body decay                                       c
c                                                                c
c----------------------------------------------------------------c
      IMPLICIT NONE
*      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
*AW   
      INTEGER IM,LFLAG,NN,K,J
      dimension im(0:900)
*AW
      DOUBLE PRECISION c,f,s,q,qq,EXCRAN,DSQRT,DCOS,DSIN
      dimension q(6,4),qq(4)
*AW
      DOUBLE PRECISION XMT,ACCU,ACC,XM2,P2,E,V
      DIMENSION XM2(4),P2(4),E(4),V(4)
      DOUBLE PRECISION rq2,X,r,z,dj,ET,XMAX,X2,G0,F0,WT2,WT3,WTM
      INTEGER ITER,ITMAX,NM
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PMM(0:4,0:900)
      SAVE
      DATA ACC/1.D-14/,ITMAX/6/
*
      call exclab(nn,im)
      if (lflag.eq.0) then
C        call exclab(nn,im)
*
* GENERATE 4 MASSLESS MOMENTA IN INFINITE PHASE SPACE
        do 1 k= 3,6
*DGC added dble in next line (not sure why needed?)
          q(k,4)= -dlog(dble(excran(1)*excran(2)))
          c= 2.0d0*excran(3)-1.0d0
          f= 2.0d0*pi*excran(4)
          s= dsqrt(1.0d0-c*c)
          q(k,2)= q(k,4)*s*dsin(f)
          q(k,3)= q(k,4)*s*dcos(f)
          q(k,1)= q(k,4)*c
    1   continue
*
* CALCULATE THE PARAMETERS OF THE CONFORMAL TRANSFORMATION AND
* TRANSFORM THE Q'S CONFORMALLY INTO THE P'S
        do 3 k= 1,4
          qq(k)= 0.d0
          do 2 j= 3,6
            qq(k)= qq(k)+q(j,k)
    2     continue
    3   continue
        rq2= dsqrt(qq(4)**2-qq(1)**2-qq(2)**2-qq(3)**2)
        x= sqrt(pm(4,12))/rq2
        do 6 k= 3,6
          r= (q(k,4)*qq(4)-q(k,1)*qq(1)
     +       -q(k,2)*qq(2)-q(k,3)*qq(3))/rq2
          z= (q(k,4)+r)/(qq(4)+rq2)
          do 4 j= 1,3
            pm(j,im(k))= (q(k,j)-qq(j)*z)*x
    4     continue
          pm(0,im(k))= x*r
    6   continue
      endif
      dj= 96.d0/pi/pi/pi/pm(4,12)/pm(4,12)
* MASSIVE PARTICLES: RESCALE THE MOMENTA BY A FACTOR X
      NM=0
      XMT = 0.D0
      ET  = DSQRT(pm(4,12))
      DO 209 K=3,6
        IF (PMM(4,im(k)).GT.0.0D0) NM=NM+1
 209  XMT = XMT + sqrt(PMM(4,IM(K)))
      IF (NM.LE.0) THEN
        IF (LFLAG.EQ.0) THEN
          DO 401 J=3,6
            DO 400 K=0,3
              PMM(K,IM(J))=PM(K,IM(J))
 400        CONTINUE
 401      CONTINUE
        ENDIF
        RETURN
      ENDIF
      IF(XMT.LE.ET) GOTO 210
      PRINT 1002,XMT,ET
      STOP
*
 210  XMAX=DSQRT(1.-(XMT/sqrt(pm(4,12)))**2)
      DO 301 J=3,6
        XM2(J-2)=PMM(4,IM(J))
  301 P2(J-2)=PM(0,IM(J))**2
      ITER=0
      X=XMAX
      ACCU=ET*ACC
  302 F0=-ET
      G0=0.0D0
      X2=X*X
      DO 303 J=1,4
      E(J)=DSQRT(XM2(J)+X2*P2(J))
      F0=F0+E(J)
  303 G0=G0+P2(J)/E(J)
      IF(DABS(F0).LE.ACCU) GOTO 305
      ITER=ITER+1
      IF(ITER.LE.ITMAX) GOTO 304
      PRINT 1006,ITMAX
      GOTO 305
  304 X=X-F0/(X*G0)
      GOTO 302
      
 305  CONTINUE
      DO 311 J=3,6
 311  V(J-2)=X*PM(0,IM(J))
      IF(LFLAG.EQ.0)THEN
        DO 307 J=3,6
          DO 306 K=1,3
 306      PMM(K,IM(J))=X*PM(K,IM(J))
 307    PMM(0,IM(J))=E(J-2)
      ENDIF
*
* CALCULATE THE MASS-EFFECT WEIGHT FACTOR
      WT2=1.D0
      WT3=0.D0
      DO 308 J=1,4
      WT2=WT2*V(J)/E(J)
  308 WT3=WT3+V(J)**2/E(J)
      WTM=X**5*WT2/WT3*ET
      
      dj = dj/WTM
 1006 FORMAT(' RAMBO WARNS:',I3,' ITERATIONS DID NOT GIVE THE',
     . ' DESIRED ACCURACY =',D15.6)
 1002 FORMAT(' RAMBO FAILS: TOTAL MASS =',D15.6,' IS NOT',
     . ' SMALLER THAN TOTAL ENERGY =',D15.6)
      return
      end
*
      subroutine annih1(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Annihilation with photon emission                            c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(5),im(6),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p5+p6)^2)
      SKM= MAX(SCUT(IM(5),IM(6),LR),
     &  PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p4)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
      SPM= MAX(scut(MIN(im(3),im(4)),MAX(im(3),im(4)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cn1= 1.3d0
      cn2= 1.d0
      cn3= 1.5d0
      cxm= -1.d0
      cxp= 1.d0
      call phots(lflag,cn1,skm,skp,pm(4,im(56)),dj1)
*      spm= pm(4,im(56))
*      spp= skp
* Min: (m4+m56)^2
* Max: (p1+p2-p3)^2
      SPM= PM(4,IM(4))+PM(4,IM(56))+
     &   2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(56)))
      SPP= PM(4,IM(12))+PM(4,IM(3))
     &   -2.0D0*SQRT(PM(4,IM(12)))*SQRT(PM(4,IM(3)))
      call phots(lflag,cn2,spm,spp,pm(4,im(456)),dj2)
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(56))=PM(4,IM(56))
        PMM(4,IM(456))=PM(4,IM(456))
      ENDIF
      call dec2f(lflag,pm(0,im(12)),pm(4,im(3)),pm(4,im(456)),
     +  pm(0,im(3)),pm(0,im(456)),PM(0,IM(12)),PMM(4,IM(3)),PMM(4,IM(456
     &  )),PMM(0,IM(3)),PMM(0,IM(456)),dj3)
      call dec2n(lflag,pm(0,im(456)),pm(4,im(56)),pm(4,im(4)),
     +  cn3,cxm,cxp,pm(0,im(56)),pm(0,im(4)),PMM(0,IM(456)),PMM(4,IM(56)
     &  ),PMM(4,IM(4)),PMM(0,IM(56)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      subroutine annih2(lflag,rm,ga,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Annihilation with massive boson emission                     c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(5),im(6),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p5+p6)^2)
      SKM= MAX(SCUT(IM(5),IM(6),LR),PM(4,IM(5))+PM(4,IM(6))+2.0D0
     &  *SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p4)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
      SPM= MAX(scut(MIN(im(3),im(4)),MAX(im(3),im(4)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      
      cn= 1.d0
      lim= 1
      call reson(lflag,rm,ga,lim,skm,skp,pm(4,im(56)),dj1)
*      spm= pm(4,im(56))
*      spp= skp
* Min: (m4+m56)^2
* Max: (p1+p2-p3)^2
      SPM= PM(4,IM(4))+PM(4,IM(56))+
     &   2.D0*SQRT(PM(4,IM(4))*PM(4,IM(56)))
      SPP= PM(4,IM(12))+PM(4,IM(3))
     &   -2.0D0*SQRT(PM(4,IM(12)))*SQRT(PM(4,IM(3)))
      call phots(lflag,cn,spm,spp,pm(4,im(456)),dj2)
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(56))=PM(4,IM(56))
        PMM(4,IM(456))=PM(4,IM(456))
      ENDIF
      call dec2f(lflag,pm(0,im(12)),pm(4,im(3)),pm(4,im(456)),
     +  pm(0,im(3)),pm(0,im(456)),PM(0,IM(12)),PMM(4,IM(3)),PMM(4,IM(456
     &  )),PMM(0,IM(3)),PMM(0,IM(456)),dj3)
      call dec2f(lflag,pm(0,im(456)),pm(4,im(56)),pm(4,im(4)),
     +  pm(0,im(56)),pm(0,im(4)),PMM(0,IM(456)),PMM(4,IM(56)),PMM(4,IM(4
     &  )),PMM(0,IM(56)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      subroutine bremb1(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Bremsstrahlung backward with photon emission and with a       c
c  t-channel exchanged photon                                    c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/area8/omct(1:6,3:6)
      double precision omct
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(5),im(6),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p5+p6)^2)
      SKM= MAX(SCUT(IM(5),IM(6),LR),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p4)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
      SPM= MAX(scut(MIN(im(3),im(4)),MAX(im(3),im(4)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      
      cn1= 1.4d0
      cn2= 1.d0
      cn3= 3.d0/2.d0
      cn4= 1.d0
      cxp= cmaxh(im(1),im(3),lr)
      cxm= -cmaxh(im(2),im(3),lr)
      call phots(lflag,cn1,skm,skp,pm(4,im(56)),dj1)
*      spm= pm(4,im(56))
*      spp= skp
* Min: (m4+m56)^2
* Max: (p1+p2-p3)^2
      SPM= PM(4,IM(4))+PM(4,IM(56))+
     &   2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(56)))
      SPP= PM(4,IM(12))+PM(4,IM(3))
     &   -2.0D0*SQRT(PM(4,IM(12)))*SQRT(PM(4,IM(3)))
      call phots(lflag,cn2,spm,spp,pm(4,im(456)),dj2)
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(56))=PM(4,IM(56))
        PMM(4,IM(456))=PM(4,IM(456))
      ENDIF
      call tchan(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(3)),
     +  pm(4,im(456)),0.d0,cn3,cxm,cxp,omct(im(1),im(3)),
     +  pm(0,im(3)),pm(0,im(456)),PMM(4,IM(3)),PMM(4,IM(456)),PMM(0,IM(3
     &  )),PMM(0,IM(456)),dj3)
      call dec2n(lflag,pm(0,im(456)),pm(4,im(56)),pm(4,im(4)),
     +  cn4,-1.d0,1.d0,pm(0,im(56)),pm(0,im(4)),PMM(0,IM(456)),PMM(4
     &  ,IM(56)),PMM(4,IM(4)),PMM(0,IM(56)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      subroutine bremb2(lflag,rm,ga,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Bremsstrahlung backward with massive boson emission and with  c
c  a t-channel exchanged photon                                  c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/area8/omct(1:6,3:6)
      double precision omct
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(5),im(6),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p5+p6)^2)
      SKM= MAX(SCUT(IM(5),IM(6),LR),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p4)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
      SPM= MAX(scut(MIN(im(3),im(4)),MAX(im(3),im(4)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cn1= 1.d0
      cn2= 3.d0/2.d0
      cxp= cmaxh(im(1),im(3),lr)
      cxm= -cmaxh(im(2),im(3),lr)
      lim= 1
      call reson(lflag,rm,ga,lim,skm,skp,pm(4,im(56)),dj1)
*      spm= pm(4,im(56))
*      spp= skp
* Min: (m4+m56)^2
* Max: (p1+p2-p3)^2
      SPM= PM(4,IM(4))+PM(4,IM(56))+
     &   2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(56)))
      SPP= PM(4,IM(12))+PM(4,IM(3))
     &   -2.0D0*SQRT(PM(4,IM(12)))*SQRT(PM(4,IM(3)))
      call phots(lflag,cn1,spm,spp,pm(4,im(456)),dj2)
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(56))=PM(4,IM(56))
        PMM(4,IM(456))=PM(4,IM(456))
      ENDIF
      call tchan(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(3)),
     +  pm(4,im(456)),0.d0,cn2,cxm,cxp,omct(im(1),im(3)),
     +  pm(0,im(3)),pm(0,im(456)),PMM(4,IM(3)),PMM(4,IM(456)),PMM(0,IM(3
     &  )),PMM(0,IM(456)),dj3)
      call dec2f(lflag,pm(0,im(456)),pm(4,im(56)),pm(4,im(4)),
     +  pm(0,im(56)),pm(0,im(4)),PMM(0,IM(456)),PMM(4
     &  ,IM(56)),PMM(4,IM(4)),PMM(0,IM(56)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      subroutine bremf1(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Bremsstrahlung forward with photon emission and with a        c
c  t-channel exchanged photon                                    c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/area8/omct(1:6,3:6)
      double precision omct
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM,AUX(0:3),AUX1(0:3),AUX2(0:3)
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
      alpha= 0.98d0
*      skm= scut(im(5),im(6),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p5+p6)^2)
      SKM= MAX(SCUT(IM(5),IM(6),LR),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p4)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
      SPM= MAX(scut(MIN(im(3),im(4)),MAX(im(3),im(4)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cn1= 1.4d0
      cn2= 0.5d0
      cn3= 1.7d0
      cxm= -1.d0
      cxp= 1.d0
      call phots(lflag,cn1,skm,skp,pm(4,im(56)),dj1)
      p0m= sqrt(pm(4,im(56)))
      SKM= PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
*      p0p= 0.5d0*(pm(4,im(12))+pm(4,im(56)))/sqrt(pm(4,im(12)))
* Max: (S-M34^2+M56^2)/2/ROOTS
      P0P= 0.5D0*(PM(4,IM(12))-SKM+PM(4,IM(56)))/SQRT(PM(4,IM(12)))
      call ener(lflag,pm(4,im(56)),cn2,p0m,p0p,
     +          pm(0,im(56)),dj2)
      IF (LFLAG.EQ.0) THEN
        DO I=0,4
          PMM(I,IM(56))=PM(I,IM(56))
        ENDDO
      ENDIF
      call brems(lflag,0.d0,pm(0,im(2)),pm(0,im(56)),pm(4,im(56)),
     +  cn3,-1.d0,1.d0,amct,pm(0,im(56)),
     +  pm(0,im(856)),PMM(4,IM(56)),PMM(0,IM(56)),PMM(0,IM(856)),0,dj3)
      iter= 0
      if (lflag.eq.1) then
        iter= 1
        goto 10
      endif
   20 call tchan(lflag,pm(0,im(1)),pm(0,im(856)),pm(4,im(3)),
     +  pm(4,im(4)),0.d0,alpha,cxm,cxp,omcth,
     +  pm(0,im(3)),pm(0,im(4)),PMM(4,IM(3)),PMM(4,IM(4)),PMM(0,IM(3))
     &  ,PMM(0,IM(4)),dj4)
 30   if (iter.le.1) then
        cvr= 2.d0*pm(0,im(1))*pm(0,im(3))/
     +    (pm(0,im(1))*(pm(0,im(3))+pm(0,im(4)))-
     +    pm(1,im(1))*(pm(1,im(3))+pm(1,im(4)))-
     +    pm(2,im(1))*(pm(2,im(3))+pm(2,im(4)))-
     +    pm(3,im(1))*(pm(3,im(3))+pm(3,im(4))))
      endif
      if (iter.ne.0) iter= iter+1
 10   if (iter.eq.0) then
*JBH: This does not give as good a precision, but works for massive
* fermions
        OMCT(IM(1),IM(3))=1.0D0-(PM(1,IM(1))*PM(1,IM(3))+PM(2,IM(1))
     &    *PM(2,IM(3))+PM(3,IM(1))*PM(3,IM(3)))/SQRT((PM(1,IM(1))*PM(1
     &    ,IM(1))+PM(2,IM(1))*PM(2,IM(1))+PM(3,IM(1))*PM(3,IM(1)))*(PM(1
     &    ,IM(3))*PM(1,IM(3))+PM(2,IM(3))*PM(2,IM(3))+PM(3,IM(3))*PM(3
     &    ,IM(3))))
        IF (OMCT(IM(1),IM(3)).LE.1.0D-11) THEN
          omct(im(1),im(3))= omcth/cvr
          IF (OMCT(IM(1),IM(3)).LE.0.0D0) OMCT(IM(1),IM(3))=1.0d-60
        ENDIF
      else if (iter.eq.1) then
        goto 30
      else if (iter.eq.2) then
*JBH: This does again not give as good a precision, but works for
* massive fermions
        AUX(0)=PM(0,IM(3))+PM(0,IM(4))
        AUX(1)=PM(1,IM(3))+PM(1,IM(4))
        AUX(2)=PM(2,IM(3))+PM(2,IM(4))
        AUX(3)=PM(3,IM(3))+PM(3,IM(4))
        CALL BOOST(1,AUX,AUX1,PM(0,IM(1)))
        CALL BOOST(1,AUX,AUX2,PM(0,IM(3)))
        OMCTH=1.0D0-(AUX1(1)*AUX2(1)+AUX1(2)*AUX2(2)+AUX1(3
     &    )*AUX2(3))/SQRT((AUX1(1)*AUX1(1)+AUX1(2)*AUX1(2)+AUX1(3)
     &    *AUX1(3))*(AUX2(1)*AUX2(1)+AUX2(2)*AUX2(2)+AUX2(3)*AUX2(3)))
        IF (OMCTH.LE.1.0D-11) THEN
          omcth= omct(im(1),im(3))*cvr
          IF (OMCTH.LE.0.0D0) OMCTH=1.0d-60
        ENDIF
        goto 20
      endif
      
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      subroutine bremf2(lflag,rm,ga,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Bremsstrahlung forward with massive boson emission and with   c
c  a t-channel exchanged photon                                  c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/area8/omct(1:6,3:6)
      double precision omct
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
      alpha= 3.d0/2.d0
*      skm= scut(im(5),im(6),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p5+p6)^2)
      SKM= MAX(SCUT(IM(5),IM(6),LR),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p4)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
      SPM= MAX(scut(MIN(im(3),im(4)),MAX(im(3),im(4)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cxp= cmaxh(im(1),im(3),lr)
      cxm= -cmaxh(im(2),im(3),lr)
      lim= 1
      call reson(lflag,rm,ga,lim,skm,skp,pm(4,im(56)),dj1)
      call dec3n(lflag,pm(0,im(1)),pm(0,im(2)),PM(4,IM(56)),
     &   PM(4,IM(3)),PM(4,IM(4)),alpha,
     +   cxm,cxp,omct(im(1),im(3)),pm(0,im(3)),pm(0,im(4)),
     +  pm(0,im(56)),PMM(4,IM(3)),PMM(4,IM(4)),PMM(0,IM(3)),PMM(0,IM(4))
     &  ,dj2)
      IF (LFLAG.EQ.0) THEN
        DO I=0,4
          PMM(I,IM(56))=PM(I,IM(56))
        ENDDO
      ENDIF
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj3)
      dj= dj1*dj2*dj3
      return
      end
*
      subroutine bremf3(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Bremsstrahlung forward with photon emission and with a        c
c  t-channel exchanged massive particle                          c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension aux(0:3)
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(5),im(6),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p5+p6)^2)
      SKM= MAX(SCUT(IM(5),IM(6),LR),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p4)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
      SPM= MAX(scut(MIN(im(3),im(4)),MAX(im(3),im(4)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cn1= 1.4d0
      cn2= 0.5d0
      cn3= 1.7d0
      cxm= -1.d0
      cxp= 1.d0
      call phots(lflag,cn1,skm,skp,pm(4,im(56)),dj1)
      p0m= sqrt(pm(4,im(56)))
      SKM= PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
*      p0p= 0.5d0*(pm(4,im(12))+pm(4,im(56)))/sqrt(pm(4,im(12)))
* Max: (S-M34^2+M56^2)/2/ROOTS
      P0P= 0.5D0*(PM(4,IM(12))-SKM+PM(4,IM(56)))/SQRT(PM(4,IM(12)))
      call ener(lflag,pm(4,im(56)),cn2,p0m,p0p,
     +          pm(0,im(56)),dj2)
      IF (LFLAG.EQ.0) THEN
        DO I=0,4
          PMM(I,IM(56))=PM(I,IM(56))
        ENDDO
      ENDIF
      call brems(lflag,0.d0,pm(0,im(2)),pm(0,im(56)),pm(4,im(56)),
     +  cn3,cxm,cxp,amct,pm(0,im(56)),
     +  pm(0,im(856)),PMM(4,IM(56)),PMM(0,IM(56)),PMM(0,IM(856)),0,dj3)
      do i= 0,3
        aux(i)= pm(i,im(12))-pm(i,im(56))
      enddo
      call dec2f(lflag,aux,pm(4,im(3)),pm(4,im(4)),
     +  pm(0,im(3)),pm(0,im(4)),AUX,PMM(4,IM(3)),PMM(4,IM(4)),
     +  PMM(0,IM(3)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      subroutine bremf4(lflag,rm,ga,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Bremsstrahlung forward with massive boson emission and with   c
c  a t-channel exchanged massive particle                        c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(5),im(6),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p5+p6)^2)
      SKM= MAX(SCUT(IM(5),IM(6),LR),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p4)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
      SPM= MAX(scut(MIN(im(3),im(4)),MAX(im(3),im(4)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      lim= 1
      call reson(lflag,rm,ga,lim,skm,skp,pm(4,im(56)),dj1)
      call dec3f(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(56)),
     &  PM(4,IM(3)),PM(4,IM(4)),
     +  pm(0,im(3)),pm(0,im(4)),pm(0,im(56)),PMM(4,IM(3)),PMM(4,IM(4))
     &  ,PMM(0,IM(3)),PMM(0,IM(4)),dj2)
      IF (LFLAG.EQ.0) THEN
        DO I=0,4
          PMM(I,IM(56))=PM(I,IM(56))
        ENDDO
      ENDIF
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj3)
      
      dj= dj1*dj2*dj3
      return
      end
*
      subroutine convr1(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Double conversion                                             c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
c:    amct should be a dummy argument greater than 1.d-11
      amct= 1.d0
      cn= 1.d0
      cxm= -1.d0
      cxp= 1.d0
*      skm= scut(im(3),im(4),lr)
*      spm= scut(im(5),im(6),lr)
* Min: max(scut.....,(p3+p4)^2)
      SKM= MAX(scut(im(3),im(4),lr),
     &   PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4))))
* Min: max(scut.....,(p5+p6)^2)
      SPM= MAX(scut(im(5),im(6),lr),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      call phots(lflag,cn,skm,skp,pm(4,im(34)),dj1)
      spp= pm(4,im(12))+pm(4,im(34))
     +    -2.d0*sqrt(pm(4,im(34))*pm(4,im(12)))
      call phots(lflag,cn,spm,spp,pm(4,im(56)),dj2)
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(34))=PM(4,IM(34))
        PMM(4,IM(56))=PM(4,IM(56))
      ENDIF
      call tchan(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(34)),
     +           pm(4,im(56)),0.d0,cn,cxm,cxp,amct,
     +  pm(0,im(34)),pm(0,im(56)),PMM(4,IM(34)),PMM(4,IM(56)),PMM(0
     &  ,IM(34)),PMM(0,IM(56)),dj3)
      call dec2f(lflag,pm(0,im(34)),pm(4,im(3)),pm(4,im(4)),
     +  pm(0,im(3)),pm(0,im(4)),PMM(0,IM(34)),PMM(4,IM(3)),PMM(4,IM(4)),
     +  PMM(0,IM(3)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      subroutine convr2(lflag,rm,ga,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Single conversion + resonance                                 c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
c:    amct should be a dummy argument greater than 1.d-11
      amct= 1.d0
      cn1= 3.d0/2.d0
      cn2= 1.d0
      cxm= -1.d0
      cxp= 1.d0
      lim= 1
*      skm= scut(im(3),im(4),lr)
*      spm= scut(im(5),im(6),lr)
* Min: max(scut.....,(p3+p4)^2)
      SKM= MAX(scut(im(3),im(4),lr),
     &   PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4))))
* Min: max(scut.....,(p5+p6)^2)
      SPM= MAX(scut(im(5),im(6),lr),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      call phots(lflag,cn1,skm,skp,pm(4,im(34)),dj1)
      spp= pm(4,im(12))+pm(4,im(34))
     +    -2.d0*sqrt(pm(4,im(34))*pm(4,im(12)))
      call reson(lflag,rm,ga,lim,spm,spp,pm(4,im(56)),dj2)
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(34))=PM(4,IM(34))
        PMM(4,IM(56))=PM(4,IM(56))
      ENDIF
      call tchan(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(34)),
     +           pm(4,im(56)),0.d0,cn2,cxm,cxp,amct,
     +  pm(0,im(34)),pm(0,im(56)),PMM(4,IM(34)),PMM(4,IM(56)),PMM(0
     &  ,IM(34)),PMM(0,IM(56)),dj3)
      call dec2f(lflag,pm(0,im(34)),pm(4,im(3)),pm(4,im(4)),
     +  pm(0,im(3)),pm(0,im(4)),PMM(0,IM(34)),PMM(4,IM(3)),PMM(4,IM(4)),
     +  PMM(0,IM(3)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      subroutine convr3(lflag,rm1,ga1,rm2,ga2,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Conversion with double resonance                              c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* JBH
      DOUBLE PRECISION M1,M2
      COMMON /BMASS/ M1,M2
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
c:    amct should be a dummy argument greater than 1.d-11
      amct= 1.d0
      cn= 1.d0
      cxm= -1.d0
      cxp= 1.d0
      lim= 1
*      skm= scut(im(3),im(4),lr)
*      spm= scut(im(5),im(6),lr)
* Min: max(scut.....,(p3+p4)^2)
      SKM= MAX(scut(im(3),im(4),lr),
     &   PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4))))
* Min: max(scut.....,(p5+p6)^2)
      SPM= MAX(scut(im(5),im(6),lr),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      call reson(lflag,rm1,ga1,lim,skm,skp,pm(4,im(34)),dj1)
      spp= pm(4,im(12))+pm(4,im(34))
     +    -2.d0*sqrt(pm(4,im(34))*pm(4,im(12)))
      call reson(lflag,rm2,ga2,lim,spm,spp,pm(4,im(56)),dj2)
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(34))=PM(4,IM(34))
        PMM(4,IM(56))=PM(4,IM(56))
      ENDIF
      call tchan(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(34)),
     +           pm(4,im(56)),0.d0,cn,cxm,cxp,amct,
     +  pm(0,im(34)),pm(0,im(56)),PMM(4,IM(34)),PMM(4,IM(56)),PMM(0
     &  ,IM(34)),PMM(0,IM(56)),dj3)
      call dec2f(lflag,pm(0,im(34)),pm(4,im(3)),pm(4,im(4)),
     +  pm(0,im(3)),pm(0,im(4)),PMM(0,IM(34)),PMM(4,IM(3)),PMM(4,IM(4)),
     +  PMM(0,IM(3)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
* JBH
      M1=SQRT(PM(4,IM(34)))
      M2=SQRT(PM(4,IM(56)))
      return
      end
*
      subroutine multi1(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Multiperipheral diagram with 2 photons                        c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/area8/omct(1:6,3:6)
      double precision omct
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension aux(0:3)
      dimension im(0:900)
*JBH
      DOUBLE PRECISION PM3,PM6,AUX1(0:3)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(4),im(5),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p4+p5)^2)
      SKM= MAX(scut(im(4),im(5),lr),
     &   PM(4,IM(4))+PM(4,IM(5))+2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(5))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p6)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(6)))
      SPM= MAX(scut(MIN(im(3),im(6)),MAX(im(3),im(6)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      alpha= 3.d0/2.d0
      cn= 1.d0
      cxp= cmaxh(im(1),im(3),lr)
      cxm= -cmaxh(im(2),im(3),lr)
      cxp1= cmaxh(im(2),im(6),lr)
      cxm1= -cmaxh(im(1),im(6),lr)
      fk= 50.d0
      call peaka(lflag,fk,cn,skm,skp,pm(4,im(45)),dj1)
      call brems(lflag,0.d0,pm(0,im(1)),SQRT(1.0D0+PM(4,IM(3))),PM(4
     &  ,IM(3)),alpha,cxm,cxp,omct(im(1),im(3)),pm(0,im(3)),aux,PM(4
     &  ,IM(3)),PMM(0,IM(3)),AUX1,1,dj2)
      call brems(lflag,0.d0,pm(0,im(2)),SQRT(1.0D0+PM(4,IM(6))),PM(4
     &  ,IM(6)),alpha,cxm1,cxp1,omct(im(2),im(6)),pm(0,im(6)),aux,PM(4
     &  ,IM(6)),PMM(0,IM(6)),AUX1,1,dj3)
*      tau= 0.5d0*(pm(0,im(3))*pm(0,im(6))-pm(1,im(3))*pm(1,im(6))-
*     +            pm(2,im(3))*pm(2,im(6))-pm(3,im(3))*pm(3,im(6)))/
*     +            pm(0,im(3))/pm(0,im(6))
      TAU=0.5D0*(1.0D0-(PM(1,IM(3))*PM(1,IM(6))+PM(2,IM(3))*PM(2,IM(6))
     &     +PM(3,IM(3))*PM(3,IM(6)))/SQRT((PM(0,IM(3))*PM(0,IM(3))-PM(4
     &     ,IM(3)))*(PM(0,IM(6))*PM(0,IM(6))-PM(4,IM(6)))))
      en= sqrt(pm(4,im(12)))/2.d0
*      p0m= 0.d0
      p0m=SQRT(PM(4,IM(3)))
*      ar= pm(4,im(45))/pm(4,im(12))
*      p0p= en-en*ar
      p0p=(PM(4,IM(12))-PM(4,IM(45))+PM(4,IM(3)))/4.0D0/EN
      call peaka(lflag,-en/tau,cn,p0m,p0p,pm(0,im(3)),dj4)
      if (lflag.eq.0) then
        PMM(0,IM(3))=PM(0,IM(3))
        PMM(0,IM(6))= (EN*EN-PM(4,IM(45))/4.D0-EN*PM(0,IM(3)))/
     +               (EN-TAU*PM(0,IM(3)))
        PM3=MAX(0.0D0,PM(0,IM(3))*PM(0,IM(3))-PM(4,IM(3)))
        T2=2.0D0*EN-PM(0,IM(3))
        T1=T2*T2-PM(4,IM(45))+PM(4,IM(6))-PM3
        T3=T2*T2-(1.0D0-2.0D0*TAU)*(1.0D0-2.0D0*TAU)*PM3
        PM3=SQRT(PM3)
        PM6=(-T1*PM3*(1.0D0-2.0D0*TAU)+T2*SQRT(MAX(0.0D0,T1*T1-4.0D0
     &    *PM(4,IM(6))*T3)))/2.0D0/T3
*        pm(0,im(6))= (en*en-pm(4,im(45))/4.d0-en*pm(0,im(3)))/
*     +               (en-tau*pm(0,im(3)))
        PM(0,IM(6))=SQRT(PM6*PM6+PM(4,IM(6)))
        do i= 1,3
          pm(i,im(3))= pm(i,im(3))*PM3
          pm(i,im(6))= pm(i,im(6))*PM6
          aux(i)= pm(i,im(12))-pm(i,im(3))-pm(i,im(6))
          PMM(I,IM(3))=PMM(I,IM(3))*PMM(0,IM(3))
          PMM(I,IM(6))=PMM(I,IM(6))*PMM(0,IM(6))
          AUX1(I)=PM(I,IM(12))-PMM(I,IM(3))-PMM(I,IM(6))
        enddo
        aux(0)= pm(0,im(12))-pm(0,im(3))-pm(0,im(6))
        AUX1(0)= PM(0,IM(12))-PMM(0,IM(3))-PMM(0,IM(6))
      endif
      call dec2f(lflag,aux,pm(4,im(4)),pm(4,im(5)),
     +  pm(0,im(4)),pm(0,im(5)),AUX1,PMM(4,IM(4)),PMM(4,IM(5)),
     +  PMM(0,IM(4)),PMM(0,IM(5)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5/pm(0,im(3))/pm(0,im(6))*
     +    (en-tau*pm(0,im(3)))*16.d0
      return
      end
*
      subroutine multi2(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Multiperipheral with 1 photon                                 c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/area8/omct(1:6,3:6)
      double precision omct
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
      alpha= 3.d0/2.d0
*      skm= scut(im(4),im(5),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p4+p5)^2)
      SKM= MAX(scut(im(4),im(5),lr),
     &   PM(4,IM(4))+PM(4,IM(5))+2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(5))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p6)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(6)))
      SPM= MAX(scut(MIN(im(3),im(6)),MAX(im(3),im(6)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cxp= cmaxh(im(1),im(3),lr)
      cxm= -cmaxh(im(2),im(3),lr)
      fk= 500.d0
      call peaka(lflag,fk,1.d0,skm,skp,pm(4,im(45)),dj1)
      call dec3n(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(45)),
     &   PM(4,IM(3)),PM(4,IM(6)),alpha,
     +   cxm,cxp,omct(im(1),im(3)),pm(0,im(3)),pm(0,im(6)),
     +  pm(0,im(45)),PMM(4,IM(3)),PMM(4,IM(6)),PMM(0,IM(3)),PMM(0,IM(6))
     &  ,dj2)
      IF (LFLAG.EQ.0) THEN
        DO I=0,4
          PMM(I,IM(45))=PM(I,IM(45))
        ENDDO
      ENDIF
      call dec2f(lflag,pm(0,im(45)),pm(4,im(4)),pm(4,im(5)),
     +  pm(0,im(4)),pm(0,im(5)),PMM(0,IM(45)),PMM(4,IM(4)),PMM(4,IM(5)),
     +  PMM(0,IM(4)),PMM(0,IM(5)),dj3)
      dj= dj1*dj2*dj3
      return
      end
*
      subroutine multi3(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Multiperipheral without photons                               c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(4),im(5),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p4+p5)^2)
      SKM= MAX(scut(im(4),im(5),lr),
     &   PM(4,IM(4))+PM(4,IM(5))+2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(5))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p6)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(6)))
      SPM= MAX(scut(MIN(im(3),im(6)),MAX(im(3),im(6)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cn= 1.d0
      fk= 500.d0
      call peaka(lflag,fk,cn,skm,skp,pm(4,im(45)),dj1)
      call dec3f(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(45)),
     &   PM(4,IM(3)),PM(4,IM(6)),
     +  pm(0,im(3)),pm(0,im(6)),pm(0,im(45)),PMM(4,IM(3)),PMM(4,IM(6))
     &  ,PMM(0,IM(3)),PMM(0,IM(6)),dj2)
      IF (LFLAG.EQ.0) THEN
        DO I=0,4
          PMM(I,IM(45))=PM(I,IM(45))
        ENDDO
      ENDIF
      call dec2f(lflag,pm(0,im(45)),pm(4,im(4)),pm(4,im(5)),
     +  pm(0,im(4)),pm(0,im(5)),PMM(0,IM(45)),PMM(4,IM(4)),PMM(4,IM(5)),
     +  PMM(0,IM(4)),PMM(0,IM(5)),dj3)
      dj= dj1*dj2*dj3
      return
      end
*
      subroutine nonab1(lflag,rm1,ga1,rm2,ga2,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Non abelian annihilation                                     c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* JBH
      DOUBLE PRECISION M1,M2
      COMMON /BMASS/ M1,M2
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
      lim= 1
*      skm= scut(im(3),im(4),lr)
*      spm= scut(im(5),im(6),lr)
* Min: max(scut.....,(p3+p4)^2)
      SKM= MAX(scut(im(3),im(4),lr),
     &   PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4))))
* Min: max(scut.....,(p5+p6)^2)
      SPM= MAX(scut(im(5),im(6),lr),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-(p5+p6))^2
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      call reson(lflag,rm1,ga1,lim,skm,skp,pm(4,im(34)),dj1)
      spp= pm(4,im(12))+pm(4,im(34))
     +    -2.d0*sqrt(pm(4,im(34))*pm(4,im(12)))
      call reson(lflag,rm2,ga2,lim,spm,spp,pm(4,im(56)),dj2)
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(34))=PM(4,IM(34))
        PMM(4,IM(56))=PM(4,IM(56))
      ENDIF
      call dec2f(lflag,pm(0,im(12)),pm(4,im(34)),pm(4,im(56)),
     +  pm(0,im(34)),pm(0,im(56)),PMM(0,IM(12)),PMM(4,IM(34)),PMM(4
     &  ,IM(56)),PMM(0,IM(34)),PMM(0,IM(56)),dj3)
      call dec2f(lflag,pm(0,im(34)),pm(4,im(3)),pm(4,im(4)),
     +  pm(0,im(3)),pm(0,im(4)),PMM(0,IM(34)),PMM(4,IM(3)),PMM(4,IM(4)),
     +  PMM(0,IM(3)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
* JBH
      M1=SQRT(PM(4,IM(34)))
      M2=SQRT(PM(4,IM(56)))
      return
      end
*
      subroutine nonab2(lflag,rm,ga,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Non abelian fusion  with emission of a massive particle       c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/area8/omct(1:6,3:6)
      double precision omct
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(4),im(5),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p4+p5)^2)
      SKM= MAX(scut(im(4),im(5),lr),
     &   PM(4,IM(4))+PM(4,IM(5))+2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(5))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p6)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(6)))
      SPM= MAX(scut(MIN(im(3),im(6)),MAX(im(3),im(6)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cn= 3.d0/2.d0
      cxp= cmaxh(im(1),im(3),lr)
      cxm= -cmaxh(im(2),im(3),lr)
      lim= 1
      call reson(lflag,rm,ga,lim,skm,skp,pm(4,im(45)),dj1)
      call dec3n(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(45)),
     &   PM(4,IM(3)),PM(4,IM(6)),cn,cxm,
     +   cxp,omct(im(1),im(3)),pm(0,im(3)),pm(0,im(6)),
     +  pm(0,im(45)),PMM(4,IM(3)),PMM(4,IM(6)),PMM(0,IM(3)),PMM(0,IM(6))
     &  ,dj2)
      IF (LFLAG.EQ.0) THEN
        DO I=0,4
          PMM(I,IM(45))=PM(I,IM(45))
        ENDDO
      ENDIF
      call dec2f(lflag,pm(0,im(45)),pm(4,im(4)),pm(4,im(5)),
     +  pm(0,im(4)),pm(0,im(5)),PMM(0,IM(45)),PMM(4,IM(4)),PMM(4,IM(5)),
     +  PMM(0,IM(4)),PMM(0,IM(5)),dj3)
      dj= dj1*dj2*dj3
      return
      end
*
      subroutine nonab3(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Non abelian fusion with emission of a photon                  c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(4),im(5),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p4+p5)^2)
      SKM= MAX(scut(im(4),im(5),lr),
     &   PM(4,IM(4))+PM(4,IM(5))+2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(5))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p6)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(6)))
      SPM= MAX(scut(MIN(im(3),im(6)),MAX(im(3),im(6)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cn= 1.3d0
      call phots(lflag,cn,skm,skp,pm(4,im(45)),dj1)
      call dec3f(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(45)),
     &   PM(4,IM(3)),PM(4,IM(6)),
     +  pm(0,im(3)),pm(0,im(6)),pm(0,im(45)),PMM(4,IM(3)),PMM(4,IM(6))
     &  ,PMM(0,IM(3)),PMM(0,IM(6)),dj2)
      IF (LFLAG.EQ.0) THEN
        DO I=0,4
          PMM(I,IM(45))=PM(I,IM(45))
        ENDDO
      ENDIF
      call dec2f(lflag,pm(0,im(45)),pm(4,im(4)),pm(4,im(5)),
     +  pm(0,im(4)),pm(0,im(5)),PMM(0,IM(45)),PMM(4,IM(4)),PMM(4,IM(5)),
     +  PMM(0,IM(4)),PMM(0,IM(5)),dj3)
      dj= dj1*dj2*dj3
      return
      end
*
      subroutine nonab4(lflag,rm,ga,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c  Fusion with emission of a resonanting particle                c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(4),im(5),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p4+p5)^2)
      SKM= MAX(scut(im(4),im(5),lr),
     &   PM(4,IM(4))+PM(4,IM(5))+2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(5))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p6)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(6)))
      SPM= MAX(scut(MIN(im(3),im(6)),MAX(im(3),im(6)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      lim= 1
      call reson(lflag,rm,ga,lim,skm,skp,pm(4,im(45)),dj1)
      call dec3f(lflag,pm(0,im(1)),pm(0,im(2)),pm(4,im(45)),
     &   PM(4,IM(3)),PM(4,IM(6)),
     +  pm(0,im(3)),pm(0,im(6)),pm(0,im(45)),PMM(4,IM(3)),PMM(4,IM(6))
     &  ,PMM(0,IM(3)),PMM(0,IM(6)),dj2)
      IF (LFLAG.EQ.0) THEN
        DO I=0,4
          PMM(I,IM(45))=PM(I,IM(45))
        ENDDO
      ENDIF
      call dec2f(lflag,pm(0,im(45)),pm(4,im(4)),pm(4,im(5)),
     +  pm(0,im(4)),pm(0,im(5)),PMM(0,IM(45)),PMM(4,IM(4)),PMM(4,IM(5)),
     +  PMM(0,IM(4)),PMM(0,IM(5)),dj3)
      dj= dj1*dj2*dj3
      return
      end
*
      subroutine dec2f(lflag,p,s1,s2,p1,p2,PM,SM1,SM2,PM1,PM2,dj)
c----------------------------------------------------------------c
c                                                                c
c   Isotropic 2-body decay:                                      c
c                                                                c
c                       p ----> p1 + p2                          c
c                       s1= p1^2                                 c
c                       s2= p2^2                                 c
c                                                                c
c              INPUT                          OUTPUT             c
c                                                                c
c  lflag= 0:   p, s1, s2                      p1, p2, dj         c
c  ("massless")  SM1, SM2                    PM1, PM2            c
c                                                                c
c  lflag= 1:   p1, p2                         dj                 c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      dimension p(0:3),p1(0:3),p2(0:3),p1h(0:3),px(0:3)
* JBH
      DOUBLE PRECISION PM(0:3),SM1,SM2,PM1(0:3),PM2(0:3)
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
      
      if (lflag.eq.0) then
        s= p(0)*p(0)-p(1)*p(1)-p(2)*p(2)-p(3)*p(3)
        IF (S.LT.0.0D0) THEN
          S=1.0D-30
          FAILFLAG=1
        ENDIF
        dj= 2.d0/pi/sqlam(s,s1,s2)
        rs= sqrt(s)
        p1h(0)= (s+s1-s2)/rs/2.d0
        p1m= rs/dj/pi
        ct= 2.d0*excran(1)-1.d0
        st= sqrt(1.d0-ct*ct)
        phi= 2.d0*pi*excran(2)
        cp= cos(phi)
        sp= sin(phi)
        call qvec(p1h(0),p1m,st,ct,sp,cp,p1h)
        call boost(0,p,p1h,p1)
        do k= 0,3
           p2(k)= p(k)-p1(k)
        enddo
* JBH: Now create "analog" particles with different masses (massless)
        P1H(0)= (S+SM1-SM2)/RS/2.D0
        P1M= RS*SQLAM(S,SM1,SM2)/2.D0
        CALL QVEC(P1H(0),P1M,ST,CT,SP,CP,P1H)
        CALL BOOST(0,PM,P1H,PM1)
        DO K= 0,3
          PM2(K)= PM(K)-PM1(K)
        ENDDO
      else
        do i= 0,3
          px(i)= p1(i)+p2(i)
        enddo
        s= px(0)*px(0)-px(1)*px(1)-px(2)*px(2)-px(3)*px(3)
        ss1= p1(0)*p1(0)-p1(1)*p1(1)-p1(2)*p1(2)-p1(3)*p1(3)
        ss2= p2(0)*p2(0)-p2(1)*p2(1)-p2(2)*p2(2)-p2(3)*p2(3)
        dj= 2.d0/pi/sqlam(s,ss1,ss2)
      endif
      return
      end
*
      subroutine dec2n(lflag,p,s1,s2,cn,cxm,cxp,p1,p2,PMT,SM1,SM2,PM1
     &  ,PM2,dj)
c----------------------------------------------------------------c
c                                                                c
c   Non isotropic 2-body decay:                                  c
c                                                                c
c                       p ----> p1 + p2                          c
c                       s1= p1^2                                 c
c                       s2= p2^2                                 c
c                                                                c
c   ct between p1 in the rest frame of p and p has the           c
c   distribution                                                 c
c                       1/(a+ct)^cn                              c
c                       cxm < ct < cxp                           c
c                                                                c
c              INPUT                          OUTPUT             c
c                                                                c
c  lflag= 0:   p, s1, s2, cn, cxm, cxp        p1, p2, dj         c
c                                                                c
c  lflag= 1:   p1, p2, cn, cxm, cxp           dj                 c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      dimension p(0:3),p1(0:3),p2(0:3),p1h(0:3),p1r(0:3),
     +          pr(0:3),rot(1:3,1:3),px(0:3)
* JBH
      DOUBLE PRECISION PMT(0:3),SM1,SM2,PM1(0:3),PM2(0:3)
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
      dj= 0.d0
      if (lflag.eq.1) then
        do i= 0,3
          px(i)= p1(i)+p2(i)
        enddo
        ss1= p1(0)*p1(0)-p1(1)*p1(1)-p1(2)*p1(2)-p1(3)*p1(3)
        ss2= p2(0)*p2(0)-p2(1)*p2(1)-p2(2)*p2(2)-p2(3)*p2(3)
        s= px(0)*px(0)-px(1)*px(1)-px(2)*px(2)-px(3)*px(3)
        rs= sqrt(s)
        p1h(0)= (s+ss1-ss2)/rs/2.d0
        p1m= rs*sqlam(s,ss1,ss2)/2.d0
        pm= sqrt(px(0)*px(0)-s)
        a= px(0)*p1h(0)/pm/p1m
        if (1.d0.ge.a.and.a.ge.0.d0) a= 1.d0
        ct= (rs*p1(0)-px(0)*p1h(0))/pm/p1m
        if (ct.lt.cxm.or.ct.gt.cxp) return
        IF (A+CT.LE.0.0D0) THEN
          A=A+1.0D0-25
          FAILFLAG=2
        ENDIF
        dj= 1.d0/(pi*sqlam(s,ss1,ss2)/4.d0*(a+ct)**cn*hj(a,cn,cxm,cxp))
      else
        s= p(0)*p(0)-p(1)*p(1)-p(2)*p(2)-p(3)*p(3)
        rs= sqrt(s)
        p1h(0)= (s+s1-s2)/rs/2.d0
        p1m= rs*sqlam(s,s1,s2)/2.d0
        pm= sqrt(p(0)*p(0)-s)
        a= p(0)*p1h(0)/pm/p1m
        if (1.d0.ge.a.and.a.ge.0.d0) a= 1.d0
        apct= tj(a,cn,cxm,cxp,1)
        ct= -a+apct
        st= sqrt(1.d0-ct*ct)
        phi= 2.d0*pi*excran(2)
        cp= cos(phi)
        sp= sin(phi)
        call qvec(p1h(0),p1m,st,ct,sp,cp,p1h)
        call qvec(p(0),pm,0.d0,1.d0,0.d0,0.d0,pr)
        call boost(0,pr,p1h,p1r)
        call rotat(0,p,pr,rot)
        call rotat(1,p1,p1r,rot)
        do k= 0,3
          p2(k)= p(k)-p1(k)
        enddo
        IF (apct.LE.0.0D0) THEN
          APCT=1.0D-25
          FAILFLAG=3
        ENDIF
        dj= 1.d0/(pi*sqlam(s,s1,s2)/4.d0*(apct)**cn*hj(a,cn,cxm,cxp))
* JBH: Now create "analog" particles with different masses (massless)
        P1H(0)= (S+SM1-SM2)/RS/2.D0
        P1M= RS*SQLAM(S,SM1,SM2)/2.D0
        CALL QVEC(P1H(0),P1M,ST,CT,SP,CP,P1H)
        PM=SQRT(PMT(0)*PMT(0)-S)
        CALL QVEC(PMT(0),PM,0.D0,1.D0,0.D0,0.D0,PR)
        CALL BOOST(0,PR,P1H,P1R)
        CALL ROTAT(1,PM1,P1R,ROT)
        DO K= 0,3
           PM2(K)= PMT(K)-PM1(K)
        ENDDO
      endif
      return
      end
*
      subroutine dec3f(lflag,p1,p2,sq,S1,S2,q1,q2,q,SM1,SM2,PM1,PM2,dj)
c----------------------------------------------------------------c
c                                                                c
c   Isotropic 3-body decay:                                      c
c                                                                c
c                p1 + p2 ----> q1 + q2 + q                       c
c                q1^2= S1                                        c
c                q2^2= S2                                        c
c                sq  = q^2                                       c
c                                                                c
c   WARNING:   p1 + p2 must be in the rest frame                 c
c                                                                c
c              INPUT                          OUTPUT             c
c                                                                c
c  lflag= 0:   p1, p2, sq, S1, S2             q1, q2, q, dj      c
c                                                                c
c  lflag= 1:   q1, q2, q                      dj                 c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      dimension p1(0:3),p2(0:3),q1(0:3),q2(0:3),q1r(0:3),q2r(0:3),
     +  q(0:3),rot(1:3,1:3)
* JBH
      DOUBLE PRECISION I3BODY,MQQ1S,MQQ2S,TEMP1,TEMP2,RMAX,RMIN,RS1
     &  ,RS2,R1,R2
      DOUBLE PRECISION SM1,SM2,PM1(0:3),PM2(0:3)
*
      s= (p1(0)+p2(0))*(p1(0)+p2(0))-(p1(1)+p2(1))*(p1(1)+p2(1))-
     +   (p1(2)+p2(2))*(p1(2)+p2(2))-(p1(3)+p2(3))*(p1(3)+p2(3))
      if (lflag.eq.0) then
        RS1=SQRT(S1)
        RS2=SQRT(S2)
* JBH: First generate as for massless fermions
        rs= sqrt(s)
        e= (s-sq)/2.d0/rs
        f=  e/(1.d0+sq/s)
   10   r1= excran(1)
        r2= excran(2)
        if (r1+r2.lt.1.d0) then
          r1= 1.d0-r1
          r2= 1.d0-r2
        endif
        q1(0)= f*r1+(e-f)*(1.d0-r2)
        q2(0)= f*r2+(e-f)*(1.d0-r1)
        if (q2(0).gt.0.5d0*(sq/(2.d0*q1(0)-rs)+rs)) goto 10
* JBH: Then check if phase space point is okay for massive fermions.
        MQQ1S=S-2.0D0*RS*Q2(0)
        MQQ2S=S-2.0D0*RS*Q1(0)
        IF (MQQ1S.GT.S+S2-2.0D0*RS*RS2.OR.MQQ2S.GT.S+S1-2.0D0*RS*RS1)
     &    GOTO 10
        R2=SQRT(MQQ1S)
        R1=(MQQ1S+SQ-S1)/2.0D0/R2
        R2=(S-MQQ1S-S2)/2.0D0/R2
        TEMP1=SQRT(R1*R1-SQ)
        TEMP2=SQRT(R2*R2-S2)
        RMAX=SQ+S2+2.0D0*(R1*R2+TEMP1*TEMP2)
        RMIN=RMAX-4.0D0*TEMP1*TEMP2
* JBH - Check if phase space point is okay
        IF (MQQ2S.GT.RMAX.OR.MQQ2S.LT.RMIN) GOTO 10
        R1=SQRT(MQQ1S)
        R2=SQRT(MQQ2S)
        Q1(0)=SQRT((S-(R2+RS1)*(R2+RS1))*(S-(R2-RS1)*(R2-RS1)))/2.0D0/RS
        Q2(0)=SQRT((S-(R1+RS2)*(R1+RS2))*(S-(R1-RS2)*(R1-RS2)))/2.0D0/RS
        TEMP1=SQRT(Q1(0)*Q1(0)+S1)
        TEMP2=SQRT(Q2(0)*Q2(0)+S2)
        call qvec(TEMP1,q1(0),0.d0,1.d0,0.d0,0.d0,q1r)
        phi2= 2.d0*pi*excran(3)
        cp2= cos(phi2)
        sp2= sin(phi2)
        CT2=(S+S1+S2-SQ+2.0D0*(TEMP1*TEMP2-RS*(TEMP1+TEMP2)))/2.0D0/Q1(0
     &    )/Q2(0)
        st2= sqrt(1.d0-ct2*ct2)
        call qvec(TEMP2,q2(0),st2,ct2,sp2,cp2,q2r)
        phi1= 2.d0*pi*excran(4)
        cp1= cos(phi1)
        sp1= sin(phi1)
        ct1= 1.d0-2.d0*excran(5)
        st1= sqrt(1.d0-ct1*ct1)
        call qvec(TEMP1,q1(0),st1,ct1,sp1,cp1,q1)
        call rotat(0,q1,q1r,rot)
        call rotat(1,q2,q2r,rot)
        do i= 0,3
          q(i)= p1(i)+p2(i)-q1(i)-q2(i)
        enddo
        DJ=1.0D0/I3BODY(S,SQ,S1,S2)/PI/PI
* JBH: Now create "analog" particles with different masses (massless)
        DO I= 0,3
          PM1(I)=Q1(I)
          Q1R(I)= Q1(I)+Q2(I)
        ENDDO
        CALL BOOST(1,Q1R,Q2R,PM1)
        DTEMP=Q1R(0)*Q1R(0)-Q1R(1)*Q1R(1)-Q1R(2)*Q1R(2)-Q1R(3)*Q1R(3)
        Q2R(0)= (DTEMP+SM1-SM2)/SQRT(DTEMP)/2.D0
        DTEMP= SQRT(DTEMP)*SQLAM(DTEMP,SM1,SM2)/2.D0
        DTEMP1=SQRT(Q2R(1)*Q2R(1)+Q2R(2)*Q2R(2)+Q2R(3)*Q2R(3))
        IF (DTEMP1.GT.0) THEN
          DO I= 1,3
            Q2R(I)=DTEMP*Q2R(I)/DTEMP1
          ENDDO
        ENDIF
        CALL BOOST(0,Q1R,Q2R,PM1)
        DO I= 0,3
          PM2(I)= Q1R(I)-PM1(I)
        ENDDO
      else
        sqq= q(0)*q(0)-q(1)*q(1)-q(2)*q(2)-q(3)*q(3)
        DJ=1.0D0/I3BODY(S,SQQ,S1,S2)/PI/PI
      endif
      return
      end
*
      subroutine dec3n(lflag,p1,p2,sq,S1,S2,cn,cxm,cxp,omct1,q1,q2,q,SM1
     &  ,SM2,PM1,PM2,dj)
c----------------------------------------------------------------c
c                                                                c
c   Non isotropic 3-body decay:                                  c
c                                                                c
c                p1 + p2 ----> q1 + q2 + q                       c
c                q1^2= S1                                        c
c                q2^2= S2                                        c
c                sq  = q^2                                       c
c                                                                c
c   WARNING:   p1 + p2 must be in the rest frame                 c
c                                                                c
c   ct1 between p1 and q1 has the distribution                   c
c                                                                c
c                       1/(1-ct1)^cn                             c
c                       cxm < ct1 < cxp                          c
c                                                                c
c  Furthermore: omct1= 1-ct1                                     c
c                                                                c
c              INPUT                        OUTPUT               c
c                                                                c
c  lflag= 0:   p1, p2, sq, S1, S2,cn,       q1, q2, q, dj,       c
c              cxm, cxp                     omct1                c
c                                                                c
c  lflag= 1:   q1, q2, q, cn,               dj                   c
c              cxm, cxp, omct1                                   c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      dimension p1(0:3),p2(0:3),q1(0:3),q2(0:3),q1r(0:3),q2r(0:3),
     +          q(0:3),rot(1:3,1:3),p1r(0:3),q2rr(0:3)
* JBH
      DOUBLE PRECISION I3BODY,MQQ1S,MQQ2S,TEMP1,TEMP2,RMAX,RMIN,RS1
     &  ,RS2,R1,R2
      DOUBLE PRECISION SM1,SM2,PM1(0:3),PM2(0:3)
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
*
      s= (p1(0)+p2(0))*(p1(0)+p2(0))-(p1(1)+p2(1))*(p1(1)+p2(1))-
     +   (p1(2)+p2(2))*(p1(2)+p2(2))-(p1(3)+p2(3))*(p1(3)+p2(3))
      dj= 0.d0
      if (lflag.eq.0) then
        RS1=SQRT(S1)
        RS2=SQRT(S2)
* JBH: First generate as for massless fermions
        rs= sqrt(s)
        e= (s-sq)/2.d0/rs
        f=  e/(1.d0+sq/s)
   10   r1= excran(1)
        r2= excran(2)
        if (r1+r2.lt.1.d0) then
          r1= 1.d0-r1
          r2= 1.d0-r2
        endif
        q1(0)= f*r1+(e-f)*(1.d0-r2)
        q2(0)= f*r2+(e-f)*(1.d0-r1)
        if (q2(0).gt.0.5d0*(sq/(2.d0*q1(0)-rs)+rs)) goto 10
* JBH: Then check if phase space point is okay for massive fermions.
        MQQ1S=S-2.0D0*RS*Q2(0)
        MQQ2S=S-2.0D0*RS*Q1(0)
        IF (MQQ1S.GT.S+S2-2.0D0*RS*RS2.OR.MQQ2S.GT.S+S1-2.0D0*RS*RS1)
     &    GOTO 10
        R2=SQRT(MQQ1S)
        R1=(MQQ1S+SQ-S1)/2.0D0/R2
        R2=(S-MQQ1S-S2)/2.0D0/R2
        TEMP1=SQRT(R1*R1-SQ)
        TEMP2=SQRT(R2*R2-S2)
        RMAX=SQ+S2+2.0D0*(R1*R2+TEMP1*TEMP2)
        RMIN=RMAX-4.0D0*TEMP1*TEMP2
* JBH - Check if phase space point is okay
        IF (MQQ2S.GT.RMAX.OR.MQQ2S.LT.RMIN) GOTO 10
        R1=SQRT(MQQ1S)
        R2=SQRT(MQQ2S)
        Q1(0)=SQRT((S-(R2+RS1)*(R2+RS1))*(S-(R2-RS1)*(R2-RS1)))/2.0D0/RS
        Q2(0)=SQRT((S-(R1+RS2)*(R1+RS2))*(S-(R1-RS2)*(R1-RS2)))/2.0D0/RS
        TEMP1=SQRT(Q1(0)*Q1(0)+S1)
        TEMP2=SQRT(Q2(0)*Q2(0)+S2)
        call qvec(p1(0),p1(0),0.d0,1.d0,0.d0,0.d0,p1r)
        phi1= 2.d0*pi*excran(3)
        cp1= cos(phi1)
        sp1= sin(phi1)
        omct1= tj(1.d0,cn,cxm,cxp,-1)
        ct1= 1.d0-omct1
        st1= sqrt(1.d0-ct1*ct1)
        call qvec(TEMP1,q1(0),st1,ct1,sp1,cp1,q1r)
        phi2= 2.d0*pi*excran(5)
        cp2= cos(phi2)
        sp2= sin(phi2)
        CT2=(S+S1+S2-SQ+2.0D0*(TEMP1*TEMP2-RS*(TEMP1+TEMP2)))/2.0D0/Q1(0
     &    )/Q2(0)
        st2= sqrt(1.d0-ct2*ct2)
        call qvec(TEMP2,q2(0),st2,ct2,sp2,cp2,q2rr)
        call rotat(0,q1r,p1r,rot)
        call rotat(1,q2r,q2rr,rot)
        call rotat(0,p1,p1r,rot)
        call rotat(1,q1,q1r,rot)
        call rotat(1,q2,q2r,rot)
        do i= 0,3
          q(i)= p1(i)+p2(i)-q1(i)-q2(i)
        enddo
        DJ=1.0D0/(-I3BODY(S,SQ,S1,S2)*(OMCT1)**CN*HJ(1.0D0,CN,-CXM,-CXP)
     &    /2.0D0)/PI/PI
* JBH: Now create "analog" particles with different masses (massless)
        DO I= 0,3
          PM1(I)=Q1(I)
          Q1R(I)= Q1(I)+Q2(I)
        ENDDO
        CALL BOOST(1,Q1R,Q2R,PM1)
        DTEMP=Q1R(0)*Q1R(0)-Q1R(1)*Q1R(1)-Q1R(2)*Q1R(2)-Q1R(3)*Q1R(3)
        Q2R(0)= (DTEMP+SM1-SM2)/SQRT(DTEMP)/2.D0
        DTEMP= SQRT(DTEMP)*SQLAM(DTEMP,SM1,SM2)/2.D0
        DTEMP1=SQRT(Q2R(1)*Q2R(1)+Q2R(2)*Q2R(2)+Q2R(3)*Q2R(3))
        IF (DTEMP1.GT.0) THEN
          DO I= 1,3
            Q2R(I)=DTEMP*Q2R(I)/DTEMP1
          ENDDO
        ENDIF
        CALL BOOST(0,Q1R,Q2R,PM1)
        DO I= 0,3
          PM2(I)= Q1R(I)-PM1(I)
        ENDDO
      else
        q1m= sqrt(q1(1)*q1(1)+q1(2)*q1(2)+q1(3)*q1(3))
        p1m= sqrt(p1(1)*p1(1)+p1(2)*p1(2)+p1(3)*p1(3))
        qdp= q1(1)*p1(1)+q1(2)*p1(2)+q1(3)*p1(3)
        if (omct1.lt.1.d-11) then
          omct10= omct1
        else
          ct1= qdp/q1m/p1m
          if (ct1.lt.cxm.or.ct1.gt.cxp) return
          omct10= 1.d0-ct1
        endif
        sqq= q(0)*q(0)-q(1)*q(1)-q(2)*q(2)-q(3)*q(3)
        IF (OMCT10.LE.0.0D0) THEN
          OMCT10=1.0D-35
          FAILFLAG=4
        ENDIF
        DJ=1.0D0/(-I3BODY(S,SQQ,S1,S2)*(OMCT10)**CN*HJ(1.0D0,CN,-CXM,
     &    -CXP)/2.0D0)/PI/PI
      endif
      return
      end
*
      subroutine tchan(lflag,q1,q2,s1,s2,rm,
     +                 cn,cxm,cxp,amct,p1,p2,SM1,SM2,PM1,PM2,dj)
c----------------------------------------------------------------c
c                                                                c
c   t-channel process:                                           c
c                                                                c
c                       q1 + q2 ----> p1 + p2                    c
c                       s1= p1^2                                 c
c                       s2= p2^2                                 c
c                       rm = mass of the propagating boson       c
c                                                                c
c   ct between q1 and p1 has the distribution (in the rest       c
c   frame of q1 + q2)                                            c
c                       1/(a-ct)^cn                              c
c                       cxm < ct < cxp                           c
c                                                                c
c   Furthermore:        amct= a-ct                               c
c                                                                c
c              INPUT                            OUTPUT           c
c                                                                c
c  lflag= 0:   q1, q2, s1, s2, rm,              p1, p2, dj,      c
c              cn, cxm, cxp                     amct             c
c                                                                c
c  lflag= 1:   q1, q2, p1, p2, cn,              dj               c
c              cxm, cxp, amct                                    c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      dimension q1(0:3),q2(0:3),q(0:3),p1(0:3),p2(0:3),
     +          q1h(0:3),p1h(0:3),q1hr(0:3),p1hr(0:3),rot(1:3,1:3)
* JBH
      DOUBLE PRECISION SM1,SM2,PM1(0:3),PM2(0:3)
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
      dj= 0.d0
      rms= rm*rm
      do i= 0,3
        q(i)= q1(i)+q2(i)
      enddo
      sq= q(0)*q(0)-q(1)*q(1)-q(2)*q(2)-q(3)*q(3)
      rsq= sqrt(sq)
      sq1= q1(0)*q1(0)-q1(1)*q1(1)-q1(2)*q1(2)-q1(3)*q1(3)
      sq2= q2(0)*q2(0)-q2(1)*q2(1)-q2(2)*q2(2)-q2(3)*q2(3)
      q1h(0)= (sq+sq1-sq2)/2.d0/rsq
      q1m= rsq*sqlam(sq,sq1,sq2)/2.d0
      if (lflag.eq.0) then
        call qvec(q1h(0),q1m,0.d0,1.d0,0.d0,0.d0,q1h)
        p1h(0)= (sq+s1-s2)/2.d0/rsq
        p1m= rsq*sqlam(sq,s1,s2)/2.d0
        a= (rms-sq1-s1+2.d0*p1h(0)*q1h(0))/2.d0/p1m/q1m
        if (1.d0.ge.a.and.a.ge.0.d0) a= 1.d0
        amct= tj(a,cn,cxm,cxp,-1)
        ct= a-amct
        st= sqrt(1.d0-ct*ct)
        phi= 2.d0*pi*excran(2)
        cp= cos(phi)
        sp= sin(phi)
        call qvec(p1h(0),p1m,st,ct,sp,cp,p1h)
        call boost(1,q,q1hr,q1)
        call rotat(0,q1hr,q1h,rot)
        call rotat(1,p1hr,p1h,rot)
        call boost(0,q,p1hr,p1)
        do k= 0,3
          p2(k)= q(k)-p1(k)
        enddo
        TEMP=-(amct)**cn*hj(a,cn,-cxm,-cxp)*p1m
        IF (TEMP.EQ.0.0D0) THEN
          FAILFLAG=5
          TEMP=1.0D-50
        ENDIF
        dj= 1.d0/(TEMP/2.d0*pi/rsq)
* JBH: Now create "analog" particles with different masses (massless)
        P1H(0)= (SQ+SM1-SM2)/RSQ/2.D0
        P1M= RSQ*SQLAM(SQ,SM1,SM2)/2.D0
        CALL QVEC(P1H(0),P1M,ST,CT,SP,CP,P1H)
        CALL ROTAT(1,P1HR,P1H,ROT)
        CALL BOOST(0,Q,P1HR,PM1)
        DO K= 0,3
           PM2(K)= Q(K)-PM1(K)
        ENDDO
      else
        ss1= p1(0)*p1(0)-p1(1)*p1(1)-p1(2)*p1(2)-p1(3)*p1(3)
        ss2= p2(0)*p2(0)-p2(1)*p2(1)-p2(2)*p2(2)-p2(3)*p2(3)
        p1h(0)= (sq+ss1-ss2)/2.d0/rsq
        p1m= rsq*sqlam(sq,ss1,ss2)/2.d0
        a= (rms-sq1-ss1+2.d0*p1h(0)*q1h(0))/2.d0/p1m/q1m
        if (1.d0.ge.a.and.a.ge.0.d0) a= 1.d0
        if (amct.lt.1.d-11) then
          amct0= amct
        else
          qdp= q1(0)*p1(0)-q1(1)*p1(1)-q1(2)*p1(2)-q1(3)*p1(3)
          ct= (p1h(0)*q1h(0)-qdp)/p1m/q1m
          if (ct.lt.cxm.or.ct.gt.cxp) return
          amct0= a-ct
        endif
        IF (AMCT0.LE.0.0D0) THEN
          AMCT0=1.0D-25
          FAILFLAG=6
        ENDIF
        dj= 1.d0/(-(amct0)**cn*hj(a,cn,-cxm,-cxp)*p1m/2.d0*pi/rsq)
      endif
      return
      end
*
      subroutine brems(lflag,rm,p,q0,sq,cn,cxm,cxp,amct,q,q1,SM1,PM1,PM2
     &  ,lrm,dj)
c----------------------------------------------------------------c
c                                                                c
c   " t-channel " decay:                                         c
c                                                                c
c                       p ----> q + q1                           c
c                       sq   = q^2                               c
c                       q0   = q(0)                              c
c                                                                c
c   ct between p and q has the distribution                      c
c                                                                c
c                       1/(a-ct)^cn                              c
c                       cxm < ct < cxp                           c
c                                                                c
c           a= (2.d0*p(0)*q0-sq-sp+rm*rm)/2/pm/qm                c
c                                                                c
c   Furthermore:        amct= a-ct                               c
c                                                                c
c                                                                c
c              INPUT                           OUTPUT            c
c                                                                c
c  lflag= 0:   rm, p, q0, sq, cn, cxm,         q, q1, dj, amct   c
c              cxp, lrm                                          c
c                                                                c
c  lflag= 1:   rm, p, q, cn, cxm,              dj                c
c              cxp, amct, lrm                                    c
c                                                                c
c                     when lflag= 1:                             c
c                                                                c
c  if the flag lrm is set to 0 amct is computed                  c
c  if the flag lrm is set to 1 amct is taken as an input         c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      dimension p(0:3),pn(0:3),q(0:3),q1(0:3),qr(0:3),rot(1:3,1:3)
* JBH
      DOUBLE PRECISION SM1,PM1(0:3),PM2(0:3)
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
      dj= 0.d0
      pm= sqrt(p(1)*p(1)+p(2)*p(2)+p(3)*p(3))
      sp= p(0)*p(0)-pm*pm
      if (lflag.eq.0) then
        call qvec(1.d0,1.d0,0.d0,1.d0,0.d0,0.d0,pn)
        qm= sqrt(q0*q0-sq)
        a= (2.d0*p(0)*q0-sq-sp+rm*rm)/2.d0/pm/qm
        if (1.d0.ge.a.and.a.ge.0.d0) a= 1.d0
        amct= tj(a,cn,cxm,cxp,-1)
        ct= a-amct
        st= sqrt(1.d0-ct*ct)
        phi= 2.d0*pi*excran(2)
        cp= cos(phi)
        sp= sin(phi)
        call qvec(q0,qm,st,ct,sp,cp,qr)
        call rotat(0,p,pn,rot)
        call rotat(1,q,qr,rot)
        do i= 0,3
          q1(i)= p(i)-q(i)
        enddo
        dj= 1.d0/(-2.d0*pi*(amct)**cn*hj(a,cn,-cxm,-cxp))
* JBH: Now create "analog" particles with different masses (massless)
        QM= SQRT(Q0*Q0-SM1)
        CALL QVEC(Q0,QM,ST,CT,SP,CP,QR)
        CALL ROTAT(1,PM1,QR,ROT)
        DO I= 0,3
          PM2(I)= P(I)-PM1(I)
        ENDDO
      else
        qm= sqrt(q(1)*q(1)+q(2)*q(2)+q(3)*q(3))
        sqq= q(0)*q(0)-qm*qm
        a= (2.d0*p(0)*q(0)-sqq-sp+rm*rm)/2.d0/pm/qm
        if (1.d0.ge.a.and.a.ge.0.d0) a= 1.d0
        if (lrm.eq.1) then
          amct0= amct
        else
          ct= (q(1)*p(1)+q(2)*p(2)+q(3)*p(3))/pm/qm
          if (ct.lt.cxm.or.ct.gt.cxp) return
          amct0= a-ct
        endif
        IF (AMCT0.LE.0.0D0) THEN
          AMCT0=1.0D-25
          FAILFLAG=7
        ENDIF
        dj= 1.d0/(-2.d0*pi*(amct0)**cn*hj(a,cn,-cxm,-cxp))
      endif
      return
      end
*
      subroutine phots(lflag,cn,cxm,cxp,s,dj)
c----------------------------------------------------------------c
c                                                                c
c   Massless particle propagator:                                c
c                                                                c
c   the invariant mass squared has the distribution              c
c                                                                c
c                       1/s^cn                                   c
c                       cxm < s < cxp                            c
c                                                                c
c              INPUT                           OUTPUT            c
c                                                                c
c  lflag= 0:   cn, cxm, cxp                    s, dj             c
c                                                                c
c  lflag= 1:   cn, cxm, cxp , s                dj                c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      dj= 0.d0
      if (lflag.eq.0) then
        s= tj(0.d0,cn,cxm,cxp,1)
      else
        if (s.lt.cxm.or.s.gt.cxp) return
      endif
      dj= 1.d0/(s**cn*hj(0.d0,cn,cxm,cxp))
      return
      end
*
      subroutine reson(lflag,rm,ga,lim,cxm,cxp,s,dj)
c----------------------------------------------------------------c
c                                                                c
c   Resonant propagator:                                         c
c                                                                c
c   the invariant mass squared has the distribution              c
c                                                                c
c                    1/[(s-rm^2)^2+rm^2*ga^2]                    c
c                                                                c
c   if lim= 0 -inf < s < +inf                                    c
c   if lim= 1  cxm < s < cxp                                     c
c                                                                c
c              INPUT                           OUTPUT            c
c                                                                c
c  lflag= 0:   rm, ga, lim, (cxm, cxp)         s, dj             c
c                                                                c
c  lflag= 1:   rm, ga, lim, s, (cxm, cxp)      dj                c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      dj= 0.d0
      rms= rm*rm
      if (lim.eq.0) then
        if (lflag.eq.0) s= rms+rm*ga*tan(pi*(excran(1)-0.5d0))
        dj= 1.d0/(pi/rm/ga*((s-rms)*(s-rms)+rms*ga*ga))
      else
        if (lflag.eq.1) then
          if (s.lt.cxm.or.s.gt.cxp) return
        endif
        yp= atan((cxp-rms)/rm/ga)
        ym= atan((cxm-rms)/rm/ga)
        if (lflag.eq.0) s= rms+rm*ga*tan(excran(1)*(yp-ym)+ym)
        dj= 1.d0/((yp-ym)/rm/ga*((s-rms)*(s-rms)+rms*ga*ga))
      endif
      return
      end
*
      subroutine ener(lflag,sq,cn,cxm,cxp,q0,dj)
c----------------------------------------------------------------c
c                                                                c
c   Energy of a propagating particle with 4-momentum q:          c
c                                                                c
c                          sq= q^2                               c
c                          q0= q(0)                              c
c                                                                c
c   the energy has the distribution                              c
c                                                                c
c                       1/q0^cn                                  c
c                       cxm < q0 < cxp                           c
c                                                                c
c              INPUT                           OUTPUT            c
c                                                                c
c  lflag= 0:   sq, cn, cxm, cxp                q0, dj            c
c                                                                c
c  lflag= 1:   sq, q0, cn, cxm, cxp            dj                c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      dj= 0.d0
      if (lflag.eq.0) then
        q0= tj(0.d0,cn,cxm,cxp,1)
      else
        if (q0.lt.cxm.or.q0.gt.cxp) return
      endif
      qm= sqrt(q0*q0-sq)
      dj= 1.d0/(q0**cn*hj(0.d0,cn,cxm,cxp)*qm/2.d0)
      return
      end
*
      subroutine peaka(lflag,a,cn,cxm,cxp,x,dj)
c----------------------------------------------------------------c
c                                                                c
c   Peaked distribution:                                         c
c   The variable x has the distribution                          c
c                                                                c
c                   1/(a+x)^cn                                   c
c                   cxm < x < cxp                                c
c                                                                c
c              INPUT                      OUTPUT                 c
c                                                                c
c  lflag= 0:   a, cn, cxm, cxp            x, dj                  c
c                                                                c
c  lflag= 1:   a, cn, cxm, cxp, x         dj                     c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
      dj= 0.d0
      if (lflag.eq.0) then
        apx= tj(a,cn,cxm,cxp,1)
        x= -a+apx
      else
        apx= a+x
        if (x.lt.cxm.or.x.gt.cxp) return
      endif
      IF (APX.EQ.0.0D0.OR.(DBLE(INT(CN)).NE.CN.AND.APX.LE.0.0D0)) THEN
        APX=1.0D-25
        FAILFLAG=8
      ENDIF
* JBH Patch to run on Digital UNIX/VMS
      IF (DBLE(INT(CN)).EQ.CN.AND.APX.LE.0.0D0) THEN
        DJ=1.0D0/((ABS(APX))**CN*HJ(A,CN,CXM,CXP))
        IF (MOD(INT(CN),2).NE.0) DJ=-DJ
      ELSE
        dj= 1.d0/((apx)**cn*hj(a,cn,cxm,cxp))
      ENDIF
      return
      end
*
      subroutine rotat(lflag,p1,p2,rot)
c----------------------------------------------------------------c
c                                                                c
c   Rotation of a 4-vector:                                      c
c                                                                c
c                            p1= rot*p2                          c
c                                                                c
c              INPUT                               OUTPUT        c
c                                                                c
c  lflag= 0:   p1, p2  (even if |p1|.ne.|p2|)      rot           c
c                                                                c
c  lflag= 1:   p2, rot                             p1            c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      dimension p1(0:3),p2(0:3),ct(1:2),st(1:2),cp(1:2),sp(1:2),
     +          r(1:2,1:3,1:3),rot(1:3,1:3),pp(1:2,0:3),pm(1:2)
      if (lflag.eq.0) then
        pm(1)= sqrt(p1(1)*p1(1)+p1(2)*p1(2)+p1(3)*p1(3))
        pm(2)= sqrt(p2(1)*p2(1)+p2(2)*p2(2)+p2(3)*p2(3))
        do k= 0,3
          pp(1,k)= p1(k)/pm(1)
          pp(2,k)= p2(k)/pm(2)
        enddo
        do i= 1,2
          ct(i)= pp(i,3)
          st(i)= sqrt(1.d0-ct(i)*ct(i))
          if (abs(abs(pp(i,3))-1.d0).lt.1.d-10) then
            cp(i)= 1.d0
            sp(i)= 0.d0
          else
            cp(i)= pp(i,2)/st(i)
            sp(i)= pp(i,1)/st(i)
          endif
          r(i,1,1)=  cp(i)
          r(i,1,2)=  sp(i)*ct(i)
          r(i,1,3)=  st(i)*sp(i)
          r(i,2,1)=  -sp(i)
          r(i,2,2)=  ct(i)*cp(i)
          r(i,2,3)=  cp(i)*st(i)
          r(i,3,1)=  0.d0
          r(i,3,2)=  -st(i)
          r(i,3,3)=  ct(i)
        enddo
        do 10 i= 1,3
          do 10 l= 1,3
            rot(i,l)= 0.d0
            do 10 k= 1,3
              rot(i,l)= rot(i,l)+r(1,i,k)*r(2,l,k)
   10 continue
      else
        p1(0)= p2(0)
        do i= 1,3
          p1(i)= 0.d0
          do k= 1,3
            p1(i)= p1(i)+rot(i,k)*p2(k)
          enddo
        enddo
      endif
      return
      end
*
      subroutine boost(lflag,q,ph,p)
c----------------------------------------------------------------c
c                                        _                       c
c   Boost of a 4-vector ( relative speed q/q(0) ):               c
c                                                                c
c   ph is the 4-vector in the rest frame of q                    c
c   p is the corresponding 4-vector in the lab frame             c
c                                                                c
c              INPUT                               OUTPUT        c
c                                                                c
c  lflag= 0:   q, ph                               p             c
c                                                                c
c  lflag= 1:   q, p                                ph            c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      dimension q(0:3),ph(0:3),p(0:3)
      rsq= sqrt(q(0)*q(0)-q(1)*q(1)-q(2)*q(2)-q(3)*q(3))
      if (lflag.eq.0) then
        p(0)= (q(0)*ph(0)+q(1)*ph(1)+q(2)*ph(2)+q(3)*ph(3))/rsq
        c1= (ph(0)+p(0))/(rsq+q(0))
        do i= 1,3
          p(i)= ph(i)+q(i)*c1
         enddo
      else
        ph(0)= (q(0)*p(0)-q(1)*p(1)-q(2)*p(2)-q(3)*p(3))/rsq
        c1= (p(0)+ph(0))/(rsq+q(0))
        do i= 1,3
          ph(i)= p(i)-q(i)*c1
        enddo
      endif
      return
      end
*
      subroutine qvec(q0,qm,st,ct,sp,cp,q)
c----------------------------------------------------------------c
c                                                                c
c   A 4-vector is built                                          c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      dimension q(0:3)
      q(0)= q0
      q(1)= qm*st*sp
      q(2)= qm*st*cp
      q(3)= qm*ct
      return
      end
*
      function sqlam(s,s1,s2)
      implicit DOUBLE PRECISION(a-h,o-z)
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
      x1= s1/s
      x2= s2/s
      IF ((1.D0+(X1-X2)*(X1-X2)-2.D0*(X1+X2)).LT.0.0D0) FAILFLAG=9
      sqlam= sqrt(MAX(1.0D-36,1.d0+x1*x1+x2*x2-2.d0*(x1+x2+x1*x2)))
      return
      end
*
      function tj(a,cn,cxm,cxp,k)
c----------------------------------------------------------------c
c                                                                c
c   tj is produced according to the distribution                 c
c                                                                c
c                    (1/tj)^cn     where:                        c
c                                                                c
c                    tj= a+k*ct                                  c
c                    cxm < ct < cxp                              c
c                    k= +1 or -1                                 c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
 10   CONTINUE
      ran1= excran(1)
      IF (RAN1.EQ.0.0D0) GOTO 10
      ce= 1.d0-cn
      if (abs(ce).gt.1.d-8) then
        if (k.eq.1) then
* JBH
          IF (A+CXP.LT.0.0D0.OR.A+CXM.LT.0.0D0.OR.(A+CXP.EQ.0.0D0.AND.CE
     &      .LE.0.0D0).OR.(A+CXM.EQ.0.0D0.AND.CE.LE.0.0D0)) THEN
            FAILFLAG=10
            TEMP=A+CXM+(CXP-CXM)/2.0D0
            IF (TEMP.EQ.0.0D0) TEMP=1.0D-20
            TJ=TEMP
            RETURN
          ENDIF
           tj= (ran1*(a+cxp)**ce+(1.d0-ran1)*
     +         (a+cxm)**ce)**(1.d0/ce)
        else
* JBH
          IF (A-CXP.LT.0.0D0.OR.A-CXM.LT.0.0D0.OR.(A-CXP.EQ.0.0D0.AND.CE
     &      .LE.0.0D0).OR.(A-CXM.EQ.0.0D0.AND.CE.LE.0.0D0)) THEN
            FAILFLAG=11
            TEMP=A-CXM-(CXP-CXM)/2.0D0
            IF (TEMP.EQ.0.0D0) TEMP=1.0D-20
            TJ=TEMP
            RETURN
          ENDIF
           tj= (ran1*(a-cxm)**ce+(1.d0-ran1)*
     +         (a-cxp)**ce)**(1.d0/ce)
        endif
      else
        if (k.eq.1) then
          if(cxp.lt.-a) then
* JBH
            IF (-A-CXP.LT.0.0D0.OR.-A-CXM.LT.0.0D0) THEN
              FAILFLAG=12
              TEMP=A+CXM+(CXP-CXM)/2.0D0
              IF (TEMP.EQ.0.0D0) TEMP=1.0D-20
              TJ=TEMP
              RETURN
            ENDIF
            IF (-A-CXP.EQ.0.0D0.OR.-A-CXM.EQ.0.0D0) THEN
              TJ=1.0D-50
              RETURN
            ENDIF
            tj= -exp(ran1*log(-a-cxp)+
     +          (1.d0-ran1)*log(-a-cxm))
          else
* JBH
            IF (A+CXP.LT.0.0D0.OR.A+CXM.LT.0.0D0) THEN
              FAILFLAG=13
              TEMP=A+CXM+(CXP-CXM)/2.0D0
              IF (TEMP.EQ.0.0D0) TEMP=1.0D-20
              TJ=TEMP
              RETURN
            ENDIF
            IF (A+CXP.EQ.0.0D0.OR.A+CXM.EQ.0.0D0) THEN
              TJ=1.0D-50
              RETURN
            ENDIF
            tj= exp(ran1*log(a+cxp)+
     +          (1.d0-ran1)*log(a+cxm))
          endif
        else
          if(cxp.lt.a) then
* JBH
            IF (A-CXP.LT.0.0D0.OR.A-CXM.LT.0.0D0) THEN
              FAILFLAG=14
              TEMP=A-CXM-(CXP-CXM)/2.0D0
              IF (TEMP.EQ.0.0D0) TEMP=1.0D-20
              TJ=TEMP
              RETURN
            ENDIF
            IF (A-CXP.EQ.0.0D0.OR.A-CXM.EQ.0.0D0) THEN
              TJ=1.0D-50
              RETURN
            ENDIF
            tj= exp(ran1*log(a-cxm)+
     +          (1.d0-ran1)*log(a-cxp))
          else
* JBH
            IF (-A+CXP.LT.0.0D0.OR.-A+CXM.LT.0.0D0) THEN
              FAILFLAG=15
              TEMP=A-CXM-(CXP-CXM)/2.0D0
              IF (TEMP.EQ.0.0D0) TEMP=1.0D-20
              TJ=TEMP
              RETURN
            ENDIF
            IF (-A+CXP.EQ.0.0D0.OR.-A+CXM.EQ.0.0D0) THEN
              TJ=1.0D-50
              RETURN
            ENDIF
            tj= -exp(ran1*log(-a+cxm)+
     +          (1.d0-ran1)*log(-a+cxp))
          endif
        endif
      endif
      return
      end
*
      function hj(a,cn,cxm,cxp)
c----------------------------------------------------------------c
c                                                                c
c   Normalization factor for tj                                  c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
* JBH: Crash-protection
      INTEGER FAILFLAG
      COMMON /FAULTS/ FAILFLAG
      ce= 1.d0-cn
      if (abs(ce).gt.1.d-8) then
* JBH
        IF (A+CXP.LT.0.0D0.OR.A+CXM.LT.0.0D0.OR.(A+CXP.EQ.0.0D0.AND.CE
     &    .LE.0.0D0).OR.(A+CXM.EQ.0.0D0.AND.CE.LE.0.0D0)) THEN
          FAILFLAG=16
          HJ=1.0D0
          RETURN
        ENDIF
        hj= ((a+cxp)**ce-(a+cxm)**ce)/ce
      else
* JBH
        IF (((A+CXP)*(A+CXM)).LE.0.0D0) THEN
          FAILFLAG=17
          HJ=1.0D0
          RETURN
        ENDIF
        hj= log((a+cxp)/(a+cxm))
      endif
      return
      end
*
*      subroutine EXCMOM      --- original code...
      subroutine EXCMOM(FERMAS)
c----------------------------------------------------------------c
c                                                                c
c  The initial configuration of the momenta and masses is set.   c
c  The beam is along the x axis                                  c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION FERMAS(6),PMM
      COMMON /EXCPMM/ PMM(0:4,0:900)
      
      pm(0,1)= roots*sqrt(xr1*xr2)/2.d0
      pm(1,1)= pm(0,1)
      
      pm(0,2)= pm(0,1)
      pm(1,2)= -pm(1,1)
      
      pm(0,12)= pm(0,1)+pm(0,2)
      pm(1,12)= pm(1,1)+pm(1,2)
      pm(2,12)= pm(2,1)+pm(2,2)
      pm(3,12)= pm(3,1)+pm(3,2)
      pm(4,12)= 4.d0*pm(0,1)*pm(0,1)
* JBH
* Massive particles
      PMM(0,1)= PM(0,1)
      PMM(1,1)= SQRT(PM(0,1)*PM(0,1)-FERMAS(1)*FERMAS(1))
      PMM(0,2)= PM(0,1)
      PMM(1,2)= -PMM(1,1)
      
      PMM(0,12)= PMM(0,1)+PMM(0,2)
      PMM(1,12)= PMM(1,1)+PMM(1,2)
      PMM(2,12)= PMM(2,1)+PMM(2,2)
      PMM(3,12)= PMM(3,1)+PMM(3,2)
      PMM(4,12)= 4.D0*PMM(0,1)*PMM(0,1)
      do j= 0,4
        pm(j,21)= pm(j,12)
        PMM(J,21)= PMM(J,12)
      enddo
*
* Set masses of final state fermions (J.B.Hansen 27-Feb-1996)
*
      PMM(4,1)=FERMAS(1)*FERMAS(1)
      PMM(4,2)=FERMAS(2)*FERMAS(2)
      PMM(4,3)=FERMAS(3)*FERMAS(3)
      PMM(4,4)=FERMAS(4)*FERMAS(4)
      PMM(4,5)=FERMAS(5)*FERMAS(5)
      PMM(4,6)=FERMAS(6)*FERMAS(6)
      return
      end
*
      subroutine momarray(nc,nn)
c----------------------------------------------------------------c
c                                                                c
c   All momenta are put in a big array pm(0:4,0:900) and         c
c   the array omct(i,j) is filled with the quantities            c
c   1-cos(p_i,p_j), where p_i is always an incoming momentum.    c
c   When there is the possibility that 1-cos is very small       c
c   the more precise value for omct is taken according to nc     c
c   and nn.                                                      c
c   The arrays pm1 and omct1 are filled with the momenta and     c
c   1-cos(p_i,p_j) calculated in the Lab system, while pm4       c
c   contains four momenta squared of any couple of particles.    c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      common/area8/omct(1:6,3:6)
      double precision omct
      common/area9/bvel,gvel
      double precision bvel,gvel
      common/area10/pm1(0:4,1:6),pm4(12:65),omct1(1:6,3:6)
      double precision pm1,pm4,omct1
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
      common/area7/kf(1:4,1:24)
      integer kf
      dimension ln0(1:6,3:6)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PMM(0:4,0:900)
      
      do 20 l= 0,4
        do 10 i= 1,6
        do 10 j= 1,6
          if (i.ne.j) then
            l1= i*10+j
            if (l.le.3) then
              pm(l,l1)= pm(l,i)+pm(l,j)
              PMM(L,L1)= PMM(L,I)+PMM(L,J)
            else
              pm(l,l1)= pm(0,l1)*pm(0,l1)-pm(1,l1)*pm(1,l1)-
     +          pm(2,l1)*pm(2,l1)-pm(3,l1)*pm(3,l1)
              PMM(L,L1)= PMM(0,L1)*PMM(0,L1)-PMM(1,L1)*PMM(1,L1)-
     +          PMM(2,L1)*PMM(2,L1)-PMM(3,L1)*PMM(3,L1)
              pm4(l1)= pmM(l,l1)
            endif
          endif
  10  continue
      do 20 i= 1,6
        do 20 j= 1,6
          if (j.ge.3) ln0(i,j)= 0
          do 20 k= 1,6
            if (i.ne.j.and.i.ne.k.and.k.ne.j) then
              l2= i*100+j*10+k
              if (l.le.3) then
                pm(l,l2)= pm(l,i)+pm(l,j*10+k)
                PMM(L,L2)= PMM(L,I)+PMM(L,J*10+K)
              else
                pm(4,l2)= pm(0,l2)*pm(0,l2)-pm(1,l2)*pm(1,l2)-
     +            pm(2,l2)*pm(2,l2)-pm(3,l2)*pm(3,l2)
                PMM(4,L2)= PMM(0,L2)*PMM(0,L2)-PMM(1,L2)*PMM(1,L2)-
     +            PMM(2,L2)*PMM(2,L2)-PMM(3,L2)*PMM(3,L2)
              endif
              if (i.le.2) then
                l3= l2+600
                if (l.le.3) then
                  pm(l,l3)= pm(l,i)-pm(l,j*10+k)
                  PMM(L,L3)= PMM(L,I)-PMM(L,J*10+K)
                else
                  pm(4,l3)= pm(0,l3)*pm(0,l3)-pm(1,l3)*pm(1,l3)-
     +              pm(2,l3)*pm(2,l3)-pm(3,l3)*pm(3,l3)
                  PMM(4,L3)= PMM(0,L3)*PMM(0,L3)-PMM(1,L3)*PMM(1,L3)-
     +              PMM(2,L3)*PMM(2,L3)-PMM(3,L3)*PMM(3,L3)
                endif
              endif
            endif
 20   continue
      is= int(1+nn/24.1)
      n1= nn-24*int(nn/24.1)
      if (nc.eq.4.or.nc.eq.5.or.nc.eq.6.or.nc.eq.10.or.
     +  nc.eq.11.or.nc.eq.12.or.nc.eq.18.or.nc.eq.19.or.
     +  nc.eq.22) ln0(is,kf(1,n1))= 1
      if (nc.eq.18) ln0(3-is,kf(4,n1))= 1
      do 30 i= 1,6
        pm1(0,i)= gvel*(pmM(0,i)+bvel*pmM(1,i))
        pm1(1,i)= gvel*(pmM(1,i)+bvel*pmM(0,i))
        pm1(2,i)= pmM(2,i)
        pm1(3,i)= pmM(3,i)
        pm1(4,i)= pmM(4,i)
        do 30 j= 3,6
          if (ln0(i,j).eq.0) omct(i,j)= 1.d0-(pmM(1,i)*pmM(1,j)+
     +      pmM(2,i)*pmM(2,j)+pmM(3,i)*pmM(3,j))/sqrt(pmM(1,i)*pmM(1,i)
     &      +pmM(2,i)*pmM(2,i)+pmM(3,i)*pmM(3,i))/sqrt(pmM(1,j)*pmM(1,j)
     &      +pmM(2,j)*pmM(2,j)+pmM(3,j)*pmM(3,j))
 30   continue
      do 40 i= 1,6
        do 40 j= 3,6
          omct1(i,j)= MAX(0.0D0,1.d0-(pm1(1,i)*pm1(1,j)+pm1(2,i)*pm1(2,j
     &      )+pm1(3,i)*pm1(3,j))/sqrt(pm1(1,i)*pm1(1,i)+pm1(2,i)*pm1(2,i
     &      )+pm1(3,i)*pm1(3,i))/sqrt(pm1(1,j)*pm1(1,j)+pm1(2,j)*pm1(2,j
     &      )+pm1(3,j)*pm1(3,j)))
 40   continue
      return
      end
*
      integer function nh(lo,jj)
c-------------------------------------------------------------------c
c                                                                   c
c       jj(1:6) are 6 momenta-indices and nh is the                 c
c       corresponding final momenta configuration.                  c
c       lo allows the implementation of symmetries among            c
c       the final momenta indices.                                  c
c                                                                   c
c-------------------------------------------------------------------c
      implicit DOUBLE PRECISION (a-h,o-z)
      common/area7/kf(1:4,1:24)
      integer kf
      dimension jj(1:6),jv(1:4)
      DIMENSION KFT(1:4,1:24)
      DATA KFT/3,4,5,6,3,4,6,5,3,5,4,6,3,5,6,4,3,6,4,5,3,6,5,4,
     +         4,3,5,6,4,3,6,5,4,5,3,6,4,5,6,3,4,6,3,5,4,6,5,3,
     +         5,3,4,6,5,3,6,4,5,4,3,6,5,4,6,3,5,6,3,4,5,6,4,3,
     +         6,3,4,5,6,3,5,4,6,4,3,5,6,4,5,3,6,5,3,4,6,5,4,3/
      DO I=1,4
        DO J=1,24
          KF(I,J) = KFT(I,J)
        ENDDO
      ENDDO
      if (lo.le.13) then
        if (mod(lo,6).eq.0.or.mod(lo,6).ge.4) then
          lf= 1
        else
          lf= 0
        endif
      else
        if (lo.le.15) lf= 2
        if (lo.gt.15) lf= 3
      endif
      j= 0
      do i= 1,4
   10   j= j+1
        if(jj(j).eq.1.or.jj(j).eq.2) goto 10
        if (lf.eq.0) jv(i)  = jj(j)
        if (lf.eq.1) jv(5-i)= jj(j)
        if (lf.eq.2) jv(i-1+4*int((5-i)/4))= jj(j)
        if (lf.eq.3) jv(8-i-4*mod(i,4)/i)= jj(j)
      enddo
      if (lo.eq.0.or.lo.eq.14.or.lo.eq.16) then
        l1= min(jv(1),jv(4))
        l2= min(jv(2),jv(3))
        l3= max(jv(2),jv(3))
        l4= max(jv(1),jv(4))
      else if (mod(lo,3).eq.1.and.lo.le.12) then
        l1= jv(1)
        l2= jv(2)
        l3= min(jv(3),jv(4))
        l4= max(jv(3),jv(4))
      else if ((mod(lo,3).eq.2.and.lo.le.12).or.
     +         (lo.eq.15.or.lo.eq.17)) then
        l1= jv(1)
        l2= min(jv(2),jv(3))
        l3= max(jv(2),jv(3))
        l4= jv(4)
      else if (mod(lo,3).eq.0.and.lo.le.12) then
        l1= min(jv(1),jv(2))
        l2= max(jv(1),jv(2))
        l3= min(jv(3),jv(4))
        l4= max(jv(3),jv(4))
      else if (lo.eq.13) then
        l1= min(jv(1),jv(2))
        l2= max(jv(1),jv(2))
        l3= min(jv(3),jv(4))
        l4= max(jv(3),jv(4))
        if (l1.gt.l3) then
           ls1= l1
           ls2= l2
           l1= l3
           l3= ls1
           l2= l4
           l4= ls2
        endif
      endif
      is= 0
      if ((lo.gt.6.and.lo.le.12).or.lo.eq.17) is= 1
      do j= 1,24
        if (l1.eq.kf(1,j).and.l2.eq.kf(2,j).and.l3.eq.kf(3,j).
     +      and.l4.eq.kf(4,j)) nh= j+24*is
      enddo
      return
      end
*
      subroutine exclab(nn,im)
c----------------------------------------------------------------c
c                                                                c
c    im contains the labels of the momenta                       c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION (a-h,o-z)
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
      common/area7/kf(1:4,1:24)
      integer kf
      dimension ig(1:6)
      dimension im(0:900)
      n1= nn-24*int(nn/24.1)
      if (nn.le.24) then
        ig(1)= 1
        ig(2)= 2
      else
        ig(1)= 2
        ig(2)= 1
      endif
      ig(3)= kf(1,n1)
      ig(4)= kf(2,n1)
      ig(5)= kf(3,n1)
      ig(6)= kf(4,n1)
      do 10 i= 1,6
        im(i)= ig(i)
        do 10 j= 1,6
          im(10*i+j)= 10*ig(i)+ig(j)
          do 10 k= 1,6
            l1= 100*i+10*j+k
            im(l1)= 100*ig(i)+10*ig(j)+ig(k)
            if (i.le.2) im(600+l1)= 600+im(l1)
   10 continue
      return
      end
*
      function cm2(lp,cp2)
c-    boson complex masses
      implicit DOUBLE PRECISION (a-b,d-h,o-z)
      implicit complex*16 (c)
      common/aus/c0,c1,ci
      complex*16 c0,c1,ci
*      common/aus/c0,c1,ci,pi
      double precision pi
      parameter (pi=3.14159265358979324D0)
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
* JBH
* Additional commons
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      if (lp.eq.1) then
C c        if (real(cp2).ge.0) then
C c          cm2= zm*zm*c1-ci*cp2*wz/zm
C         cm2= zm*zm*c1-ci*wz*zm
C c        else
C c          cm2= zm*zm*c1
C c        endif
* JBH (Q-dependent width has been turned into fixed width in setpro)
        IF (IEXQDW.LE.0) THEN
          cm2= zmi*zmi*c1-ci*wzi*zmi
        ELSE
          cm2= zm*zm*c1-ci*wz*zm
        ENDIF
      else if (lp.eq.2) then
        cm2= c0
      else
C c        if (real(cp2).ge.0) then
C c          cm2= wm*wm*c1-ci*cp2*ww/wm
C         cm2= wm*wm*c1-ci*ww*wm
C c        else
C c          cm2= wm*wm*c1
C c        endif
* JBH (Q-dependent width has been turned into fixed width in setpro)
        IF (IEXQDW.LE.0) THEN
          cm2= wmi*wmi*c1-ci*wwi*wmi
        ELSE
          cm2= wm*wm*c1-ci*ww*wm
        ENDIF
      endif
      return
      end
*
      subroutine excuts(lnot)
      implicit DOUBLE PRECISION (a-h,o-z)
      common/area10/pm1(0:4,1:6),pm4(12:65),omct1(1:6,3:6)
      double precision pm1,pm4,omct1
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      kaus= 0
      lnot= 0
      if (omct1(1,3).lt.omcmax(1,3,lr))   goto 10
      if (omct1(1,4).lt.omcmax(1,4,lr))   goto 10
      if (omct1(1,5).lt.omcmax(1,5,lr))   goto 10
      if (omct1(1,6).lt.omcmax(1,6,lr))   goto 10
      if (omct1(2,3).lt.omcmax(2,3,lr))   goto 10
      if (omct1(2,4).lt.omcmax(2,4,lr))   goto 10
      if (omct1(2,5).lt.omcmax(2,5,lr))   goto 10
      if (omct1(2,6).lt.omcmax(2,6,lr))   goto 10
      if (omct1(3,4).lt.omcmax(3,4,lr))   goto 10
      if (omct1(3,5).lt.omcmax(3,5,lr))   goto 10
      if (omct1(3,6).lt.omcmax(3,6,lr))   goto 10
      if (omct1(4,5).lt.omcmax(4,5,lr))   goto 10 
      if (omct1(4,6).lt.omcmax(4,6,lr))   goto 10
      if (omct1(5,6).lt.omcmax(5,6,lr))   goto 10
      if (pm1(0,3).lt.ecut(3,lr))         goto 10
      if (pm1(0,4).lt.ecut(4,lr))         goto 10
      if (pm1(0,5).lt.ecut(5,lr))         goto 10
      if (pm1(0,6).lt.ecut(6,lr))         goto 10
      if (pm4(34).lt.scut(3,4,lr))        goto 10
      if (pm4(35).lt.scut(3,5,lr))        goto 10
      if (pm4(36).lt.scut(3,6,lr))        goto 10
      if (pm4(45).lt.scut(4,5,lr))        goto 10
      if (pm4(46).lt.scut(4,6,lr))        goto 10
      if (pm4(56).lt.scut(5,6,lr))        goto 10
      kaus= 1
 10   if (kaus.eq.0) lnot= 1
      return
      end
*
      function excgam(opx)
c---------------------------------------------------------------c
c                                                               c
c     computes Gamma(1+x) x<<1                                  c
c     with x < 0.03 the error is less than 1.E-10               c
c                                                               c
c---------------------------------------------------------------c
      implicit DOUBLE PRECISION (a-h,o-z)
      data ge/0.5772156649d0/,s3/1.2020569032d0/,
     +     s5/1.0369277551d0/,s7/1.0083492774d0/,
     +     pi/3.14159265358979324d0/
      x= opx-1.d0
      excgam= exp(0.5d0*log(x*pi/sin(pi*x))-
     +       ge*x-s3*x*x*x/3.d0-s5*x*x*x*x*x/5.d0)
      return
      end
*
      function flux(alpha,cl,zeta,gaeul,x,omx)
c---------------------------------------------------------------c
c                                                               c
c     flux function for QED convolution                         c
c     ( the pole part has been exctracted out )                 c
c                                                               c
c---------------------------------------------------------------c
      implicit DOUBLE PRECISION (a-h,o-z)
      data pi/3.14159265358979324d0/,ge/0.5772156649d0/
      ap= alpha/2.d0/pi
      opx= 1.d0+x
      opz= 1.d0+zeta
      flux0= exp(-zeta*ge+3.d0*cl*ap/2.d0)/gaeul*
     +       zeta
      flux1= -ap*opx*cl*omx**(1.d0-zeta)
      flux2= -0.5d0*ap*ap*cl*cl*((1.d0+3.d0*x*x)/omx*log(x)+
     +       4.d0*opx*log(omx)+5.d0+x)*omx**(1.d0-zeta)
      flux= flux0+flux1+flux2
      return
      end
*
      function excran(kdummy)
      implicit DOUBLE PRECISION(a-h,o-z)
*DGC put seed info into common to allow inspection
*      common /excrns/ s1,s2,s3
*JBH,BBL
      REAL RRAN,RNDM
C      CALL RANMAR(RRAN,1)
      EXCRAN=DBLE(RNDM(RRAN))
C      save
C      data init/0/,s1/0.d0/,s2/0.d0/,s3/0.d0/
C      if(init.eq.0) then
C        init=1
C        s1=dsqrt(2.d0)-1.d0
C        s2=dsqrt(3.d0)-1.d0
C        s3=dsqrt(5.d0)-2.d0
C      endif
C      s1=dmod(s1+s2+s3,1.d0)
C      s2=dmod(s1+s2+s3,1.d0)
C      s3=dmod(s1+s2+s3,1.d0)
C      excran=s1
      end
*
*--------------------------------------------------------------
* Code from R. Pittau for QQgg...(Thanks)
* All credit should be given to him....
*--------------------------------------------------------------
*
      subroutine anhgg1(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c   Annihilation with photon emission                            c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
      call exclab(nn,im)
*      skm= scut(im(5),im(6),lr)
*      skp= pm(4,im(12))
* Min: max(scut.....,(p5+p6)^2)
* Due to the G -> GG vertex with only massless gluons we need a cutoff,
* however we set it very low... (Sorry)
      SKM= MAX(1.0D-10,SCUT(IM(5),IM(6),LR),
     &  PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
* Max: (p1+p2-sqrt(max(scut.....,(p3+p4)^2)))^2
      SPM=PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4)))
      SPM= MAX(scut(MIN(im(3),im(4)),MAX(im(3),im(4)),lr),SPM)
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cn1= 1.d0
      cn2= 1.d0
      cn3= 1.d0
      cxm= -1.d0
      cxp= 1.d0
      call phots(lflag,cn1,skm,skp,pm(4,im(56)),dj1)
*      spm= pm(4,im(56))
*      spp= skp
* Min: (m4+m56)^2
* Max: (p1+p2-p3)^2
      SPM= PM(4,IM(4))+PM(4,IM(56))+
     &   2.0D0*SQRT(PM(4,IM(4))*PM(4,IM(56)))
      SPP= PM(4,IM(12))+PM(4,IM(3))
     &   -2.0D0*SQRT(PM(4,IM(12)))*SQRT(PM(4,IM(3)))
      call phots(lflag,cn2,spm,spp,pm(4,im(456)),dj2)
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(56))=PM(4,IM(56))
        PMM(4,IM(456))=PM(4,IM(456))
      ENDIF
      call dec2f(lflag,pm(0,im(12)),pm(4,im(3)),pm(4,im(456)),
     +  pm(0,im(3)),pm(0,im(456)),PM(0,IM(12)),PMM(4,IM(3)),PMM(4,IM(456
     &  )),PMM(0,IM(3)),PMM(0,IM(456)),dj3)
      call dec2n(lflag,pm(0,im(456)),pm(4,im(56)),pm(4,im(4)),
     +  cn3,cxm,cxp,pm(0,im(56)),pm(0,im(4)),PMM(0,IM(456)),PMM(4,IM(56)
     &  ),PMM(4,IM(4)),PMM(0,IM(56)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      subroutine nabgg1(lflag,nn,dj)
c----------------------------------------------------------------c
c                                                                c
c     g g bremstrahlung                                          c
c                                                                c
c----------------------------------------------------------------c
      implicit DOUBLE PRECISION(a-h,o-z)
      double precision pi
      parameter (pi=3.14159265358979324D0)
**      common/area0/pi
      common/momenta/roots,xr1,xr2,pmM(0:4,0:900)
      double precision roots,xr1,xr2,pm
      integer nim
      parameter (nim=153)
      common/cutset/scut(3:5,4:6,nim),ecut(3:6,nim),
     +              cmax(1:5,3:6,nim),omcmax(1:5,3:6,nim)
      double precision scut,ecut,cmax,omcmax
      common/cuth/ecuth(3:6,nim),cmaxh(1:2,3:6,nim)
      double precision ecuth,cmaxh
      common/lrnum/lr
      integer lr
      dimension im(0:900)
* JBH
      DOUBLE PRECISION M1,M2
      COMMON /BMASS/ M1,M2
* J.B.Hansen 27-Feb-1996
      DOUBLE PRECISION PMM
      COMMON /EXCPMM/ PM(0:4,0:900)
*      implicit real*8(a-h,o-z)
*      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
*      common/cutset/scut(3:5,4:6),ecut(3:6),
*     +              cmax(1:5,3:6),omcmax(1:5,3:6)
*      dimension ig(1:6)
*      dimension im(0:900)
*
*      ig(1)= 1
*      ig(2)= 2
*      ig(3)= lg3
*      ig(4)= lg4
*      ig(5)= lg5
*      ig(6)= lg6
      call EXClab(nn,im)
*      skm= scut(im(3),im(4))
*      spm= scut(im(5),im(6))
* Min: max(scut.....,(p3+p4)^2)
      SKM= MAX(scut(im(3),im(4),lr),
     &   PM(4,IM(3))+PM(4,IM(4))+2.0D0*SQRT(PM(4,IM(3))*PM(4,IM(4))))
* Min: max(scut.....,(p5+p6)^2)
      SPM= MAX(scut(im(5),im(6),lr),
     &   PM(4,IM(5))+PM(4,IM(6))+2.0D0*SQRT(PM(4,IM(5))*PM(4,IM(6))))
      skp= pm(4,im(12))+spm-2.d0*sqrt(spm*pm(4,im(12)))
      cn1= 1.d0
      cn2= 1.d0
      call phots(lflag,cn1,skm,skp,pm(4,im(34)),dj1)
      spp= pm(4,im(12))+pm(4,im(34))
     +    -2.d0*sqrt(pm(4,im(34))*pm(4,im(12)))
      call phots(lflag,cn2,spm,spp,pm(4,im(56)),dj2)
      
      IF (LFLAG.EQ.0) THEN
        PMM(4,IM(34))=PM(4,IM(34))
        PMM(4,IM(56))=PM(4,IM(56))
      ENDIF
      call dec2f(lflag,pm(0,im(12)),pm(4,im(34)),pm(4,im(56)),
     +  pm(0,im(34)),pm(0,im(56)),PMM(0,IM(12)),PMM(4,IM(34)),PMM(4
     &  ,IM(56)),PMM(0,IM(34)),PMM(0,IM(56)),dj3)
      call dec2f(lflag,pm(0,im(34)),pm(4,im(3)),pm(4,im(4)),
     +  pm(0,im(3)),pm(0,im(4)),PMM(0,IM(34)),PMM(4,IM(3)),PMM(4,IM(4)),
     +  PMM(0,IM(3)),PMM(0,IM(4)),dj4)
      call dec2f(lflag,pm(0,im(56)),pm(4,im(5)),pm(4,im(6)),
     +  pm(0,im(5)),pm(0,im(6)),PMM(0,IM(56)),PMM(4,IM(5)),PMM(4,IM(6)),
     +  PMM(0,IM(5)),PMM(0,IM(6)),dj5)
      dj= dj1*dj2*dj3*dj4*dj5
      return
      end
*
      function cver(n1,n2)
      implicit real*8 (a-b,d-h,o-z)
      implicit complex*16 (c)
c      common/aus/c0,c1,ci,pi
      common/aus/c0,c1,ci
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
      common/vertex/arge,argq,alge,algq,
*     !              arze,arzq,alze,alzq,facnor,zm,wz
     +  arze,arzq,alze,alzq
*JBH - From excalibur
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
*JBH
      DOUBLE PRECISION PI
      PARAMETER (PI=3.14159265358979324D0)
      s= pm(4,12)
      if (n1.eq.1.and.n2.eq.1)   cver= 
     $   c1*arge*argq/s+c1*arze*arzq/(c1*s-zm*zm*c1+ci*s*wz/zm)
      if (n1.eq.1.and.n2.eq.-1)  cver= 
     $   c1*arge*algq/s+c1*arze*alzq/(c1*s-zm*zm*c1+ci*s*wz/zm)
      if (n1.eq.-1.and.n2.eq.1)  cver= 
     $   c1*alge*argq/s+c1*alze*arzq/(c1*s-zm*zm*c1+ci*s*wz/zm)
      if (n1.eq.-1.and.n2.eq.-1) cver= 
     $   c1*alge*algq/s+c1*alze*alzq/(c1*s-zm*zm*c1+ci*s*wz/zm)
      return
      end
      subroutine matrix1(squarem,X1234,X1243,XINT)
      implicit real*8 (a-b,d-h,o-z)
      implicit complex*16 (c)
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
c      common/aus/c0,c1,ci,pi
      common/aus/c0,c1,ci
      common/wdw/cpp(1:6,1:6),cppc(1:6,1:6)
      common/vertex/arge,argq,alge,algq,
*     !              arze,arzq,alze,alzq,facnor,zm,wz
     $              arze,arzq,alze,alzq
      dimension crp(1:6),cp(1:6,1:6)
      dimension p(1:6,0:3)
      dimension camp12(-1:1,-1:1,-1:1,-1:1),camp21(-1:1,-1:1,-1:1,-1:1)
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
*JBH
      DOUBLE PRECISION PI
      PARAMETER (PI=3.14159265358979324D0)
 
      do 134 j= 1,6
        if (j.gt.2) ph=  1.d0
        if (j.le.2) ph= -1.d0
        do  134 k= 0,3
          p(j,k)= ph*pm(k,j)
  134 continue
      do i=1,6
       crp(i)= sqrt(c1*(p(i,0)-p(i,3)))
      enddo
c-    the Weyl-van der Waerden inner products are evaluated
      do 135 i=1,6
        do 135 j= 1,6 
          if (i.ne.j) then
            cp(i,j)= crp(i)/crp(j)*(c1*p(j,1)-ci*p(j,2))
     $              -crp(j)/crp(i)*(c1*p(i,1)-ci*p(i,2))
          else
            cp(i,j)= c0
          endif
  135 continue 
      do 136 i= 1,6
      do 136 k= 1,6
        if (i.ne.k) then
          if (i+k.eq.3) then
            cf= -c1
          else if (i.le.2.or.k.le.2) then
            cf= ci
          else
            cf= c1
          endif
          cpp(i,k)= cp(i,k)*cf
          cppc(i,k)= conjg(cp(i,k))*cf
        endif
  136 continue
      
      do 137 il= -1,1,2
        do 137 ir= -1,1,2
          do 137 is= -1,1,2
            do 137 it= -1,1,2
              camp12(il,ir,is,it)= 2.d0*cver(il,ir)*ca0(il,ir,is,it,5,6)
              camp21(il,ir,is,it)= 2.d0*cver(il,ir)*ca0(il,ir,it,is,6,5)
  137 continue
 
*JBH Keep track of gluon ordering
      X1234=0.0D0
      X1243=0.0D0
      XINT=0.0D0
*
      squarem= 0.d0
      do 138 il= -1,1,2
        do 138 ir= -1,1,2
          do 138 is= -1,1,2
            do 138 it= -1,1,2
              squarem= squarem+16.d0/3.d0*(camp12(il,ir,is,it)*
     $           conjg(camp12(il,ir,is,it))+camp21(il,ir,is,it)*
     $           conjg(camp21(il,ir,is,it)))-4.d0/3.d0*
     $           dreal(conjg(camp12(il,ir,is,it))*camp21(il,ir,is,it))
*JBH Keep track of gluon ordering
              X1234=X1234+16.d0/3.d0*camp12(il,ir,is,it)*conjg(camp12(il
     &          ,ir,is,it))
              X1243=X1243+16.d0/3.d0*camp21(il,ir,is,it)*conjg(camp21(il
     &          ,ir,is,it))
              XINT=XINT-4.d0/3.d0*DREAL(camp21(il,ir,is,it)
     &          *conjg(camp12(il,ir,is,it)))
  138 continue 
      squarem= squarem*facnor/xr1/xr2
      return
      end
      complex*16 function ca0(js0,js1,js2,js3,j5,j6)  
      implicit real*8 (a-b,d-h,o-z)
      implicit complex*16 (c)
      if (js0.eq.1.and.js1.eq.1.and.js2.eq.1.
     $   and.js3.eq.1) then
        ca0= cb0(1,1,1,1,2,j5,j6)      
      else if (js0.eq.1.and.js1.eq.-1.and.js2.eq.1.
     $         and.js3.eq.1) then
        ca0=  conjg(cb0(1,-1,-1,2,1,j5,j6))      
      else if (js0.eq.-1.and.js1.eq.1.and.js2.eq.1.
     $         and.js3.eq.1) then
        ca0= cb0(1,1,1,2,1,j5,j6)      
      else if (js0.eq.-1.and.js1.eq.-1.and.js2.eq.1.
     $         and.js3.eq.1) then
        ca0= conjg(cb0(1,-1,-1,1,2,j5,j6))      
      else if (js0.eq.1.and.js1.eq.1.and.js2.eq.1.
     $         and.js3.eq.-1) then
        ca0= cb0(1,1,-1,1,2,j5,j6)      
      else if (js0.eq.1.and.js1.eq.-1.and.js2.eq.1.
     $         and.js3.eq.-1) then
        ca0= conjg(cb0(1,-1,1,2,1,j5,j6))      
      else if (js0.eq.-1.and.js1.eq.1.and.js2.eq.1.
     $         and.js3.eq.-1) then
        ca0= cb0(1,1,-1,2,1,j5,j6)      
      else if (js0.eq.-1.and.js1.eq.-1.and.js2.eq.1.
     $         and.js3.eq.-1) then
        ca0= conjg(cb0(1,-1,1,1,2,j5,j6))      
      else if (js0.eq.1.and.js1.eq.1.and.js2.eq.-1.
     $         and.js3.eq.1) then
        ca0= cb0(1,-1,1,1,2,j5,j6)      
      else if (js0.eq.1.and.js1.eq.-1.and.js2.eq.-1.
     $         and.js3.eq.1) then
        ca0= conjg(cb0(1,1,-1,2,1,j5,j6))      
      else if (js0.eq.-1.and.js1.eq.1.and.js2.eq.-1.
     $         and.js3.eq.1) then
        ca0= cb0(1,-1,1,2,1,j5,j6)      
      else if (js0.eq.-1.and.js1.eq.-1.and.js2.eq.-1.
     $         and.js3.eq.1) then
        ca0= conjg(cb0(1,1,-1,1,2,j5,j6))      
      else if (js0.eq.1.and.js1.eq.1.and.js2.eq.-1.
     $         and.js3.eq.-1) then
        ca0= cb0(1,-1,-1,1,2,j5,j6)      
      else if (js0.eq.1.and.js1.eq.-1.and.js2.eq.-1.
     $         and.js3.eq.-1) then
        ca0= conjg(cb0(1,1,1,2,1,j5,j6))      
      else if (js0.eq.-1.and.js1.eq.1.and.js2.eq.-1.
     $         and.js3.eq.-1) then
        ca0= cb0(1,-1,-1,2,1,j5,j6)      
      else if (js0.eq.-1.and.js1.eq.-1.and.js2.eq.-1.
     $         and.js3.eq.-1) then
        ca0= conjg(cb0(1,1,1,1,2,j5,j6))
      else
       print*, ' ERROR '
      endif 
      return
      end     
 
      complex*16 function cb0(js1,js2,js3,j1,j2,j5,j6)  
      implicit real*8 (a-b,d-h,o-z)
      implicit complex*16 (c)
      common/wdw/cpp(1:6,1:6),cppc(1:6,1:6)
c      common/aus/c0,c1,ci,pi
      common/aus/c0,c1,ci
*JBH
      DOUBLE PRECISION PI
      PARAMETER (PI=3.14159265358979324D0)
      if (js1.eq.1.and.js2.eq.1.and.js3.eq.1) then
        cb1= 2.d0*cpp(4,j2)/cpp(3,j5)/cpp(j5,j6)/cpp(j6,4)
        cb0= cb1*(cppc(3,j1)*cpp(3,4)+cppc(j5,j1)*cpp(j5,4)+
     $            cppc(j6,j1)*cpp(j6,4))
      else if (js1.eq.1.and.js2.eq.1.and.js3.eq.-1) then
        cb1= cpp(4,j2)*cppc(3,j5)*cpp(3,j6)/cpp(3,j5)*
     $      (cppc(3,j1)*cpp(3,j6)+cppc(j5,j1)*cpp(j5,j6))/
     $      (cppc(3,j5)*cpp(3,j5)+cppc(3,j6)*cpp(3,j6)+
     $       cppc(j5,j6)*cpp(j5,j6))
        cb2= cb1+cppc(3,j1)*cppc(4,j5)*cpp(4,j6)/cppc(4,j6)*
     $      (cppc(j6,j5)*cpp(j6,j2)+cppc(4,j5)*cpp(4,j2))/
     $      (cppc(j5,j6)*cpp(j5,j6)+cppc(j5,4)*cpp(j5,4)+
     $       cppc(j6,4)*cpp(j6,4))
        cb3= cb2+(cppc(j6,j5)*cpp(j6,j2)+cppc(4,j5)*cpp(4,j2))/
     $       cpp(3,j5)/cppc(j6,4)*
     $      (cppc(3,j1)*cpp(3,j6)+cppc(j5,j1)*cpp(j5,j6))
        cb0= -2.d0*cb3/cpp(j5,j6)/cppc(j5,j6)
      else if (js1.eq.1.and.js2.eq.-1.and.js3.eq.1) then
        cb1= cppc(3,j1)*cpp(4,j5)*cpp(4,j5)/cpp(4,j6)*
     $      (cppc(j5,j6)*cpp(j5,j2)+cppc(4,j6)*cpp(4,j2))/
     $      (cppc(j5,j6)*cpp(j5,j6)+cppc(j5,4)*cpp(j5,4)+
     $       cppc(j6,4)*cpp(j6,4))
        cb2= cb1+cpp(4,j2)*cppc(3,j6)*cppc(3,j6)*
     $      (cppc(3,j1)*cpp(3,j5)+cppc(j6,j1)*cpp(j6,j5))/
     $       cppc(3,j5)/(cppc(3,j5)*cpp(3,j5)+cppc(3,j6)*cpp(3,j6)+
     $       cppc(j5,j6)*cpp(j5,j6))
        cb3= cb2-cpp(4,j2)*cpp(4,j5)*cppc(3,j6)*cppc(3,j1)/
     $       cppc(3,j5)/cpp(4,j6)
        cb0= -2.d0*cb3/cpp(j5,j6)/cppc(j5,j6)
      else if (js1.eq.1.and.js2.eq.-1.and.js3.eq.-1) then
        cb1= cppc(j5,3)*cpp(j5,j2)+cppc(j6,3)*cpp(j6,j2)+
     $       cppc(4,3)*cpp(4,j2)
        cb0= -2.d0*cb1*cppc(3,j1)/cppc(3,j5)/cppc(j5,j6)/cppc(j6,4)
      else
        print*,'  ERROR'
      endif
      return
      end
      SUBROUTINE EXCRR(IMODE,IER)
C======================================================================
C! Color Reconnection Read
C
C  Author      : A.Waananen
C  Created     : 01-Oct-96
C
C  Description :
C
C   Read event from file.
C
C   This file is generator specific. The code
C   below is an implementation for Excalibur. It
C   should be fairly easy to port to another 
C   generator. Only the routines EXCRR and EXCRW
C   needs to be modified.
C
C  Input:
C     IMODE : Mode
C       = 0 Read generator specific stuff
C       = 1 Read Lund variables
C       = 2 Skip Lund variables
C     IER   : Error code
C
C  Called : EXCR
C  Calls  :  
C======================================================================
      IMPLICIT NONE
C
C LUJETS
      INTEGER N,K
      REAL    P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
C
C Local variables
C
      INTEGER IMODE,IER
      INTEGER LUN,I,I1
C
C User variables to store
C
C (LUJ1 and LUJ2 is a must!)
      INTEGER LUJ1,LUJ2
      COMMON /LUQPOS/ LUJ1(2),LUJ2(2)
C Excalibur variables (Matrix elements)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
C Logical Unit
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
C W mass and width
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
C Aleph Variables
      INTEGER ALIDPR
      COMMON /ALEXPR/ ALIDPR
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON / BCS / IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
*CD bmacrod
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
      INTEGER NLINK,JKXCR,JLUNH,JLUND
*CD bmacro
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
C Set logical unit
C
      LUN = IEXUCR
      IER = 0
      IF (IMODE.EQ.0) THEN
C
C KXCR
C
        JKXCR=NLINK('KXCR',0)
        IF(JKXCR.NE.0) THEN
          IF (IW(JKXCR).NE.34+2) THEN
            WRITE(LMCWRT,112) 'KXCR',IW(JKXCR),34+2
            STOP
          ENDIF
        ELSE
          WRITE(LMCWRT,111) 'KXCR'
          STOP
        ENDIF
        ALIDPR = ITABL(JKXCR,1,1)
        WM     = DBLE(RTABL(JKXCR,1,2))
        WW     = DBLE(RTABL(JKXCR,1,3))
        EX1234 = DBLE(RTABL(JKXCR,1,4))
        EX1432 = DBLE(RTABL(JKXCR,1,5))
        EXINT  = DBLE(RTABL(JKXCR,1,6))
        DO 10 I=1,8
          EXCSIG(1,I) = DBLE (RTABL(JKXCR,1,6+(I-1)*3+1))
          EXCSIG(2,I) = DBLE (RTABL(JKXCR,1,6+(I-1)*3+2))
          EXCSIG(3,I) = DBLE (RTABL(JKXCR,1,6+(I-1)*3+3))
 10     CONTINUE
        VERTEX(1) = DBLE(RTABL(JKXCR,1,31))
        VERTEX(2) = DBLE(RTABL(JKXCR,1,32))
        VERTEX(3) = DBLE(RTABL(JKXCR,1,33))
        VERTEX(4) = DBLE(RTABL(JKXCR,1,34))
      ELSEIF (IMODE.EQ.1) THEN
C
C LUNH
C
        JLUNH=NLINK('LUNH',0)
        IF(JLUNH.NE.0) THEN
          IF (IW(JLUNH).NE.5+2) THEN
            WRITE(LMCWRT,112) 'LUNH',IW(JLUNH),5+2
            STOP
          ENDIF
        ELSE
          WRITE(LMCWRT,111) 'LUNH'
          STOP
        ENDIF
        LUJ1(1) = ITABL(JLUNH,1,1)
        LUJ1(2) = ITABL(JLUNH,1,2)
        LUJ2(1) = ITABL(JLUNH,1,3)
        LUJ2(2) = ITABL(JLUNH,1,4)
        N       = ITABL(JLUNH,1,5)
C
C LUND
C
        JLUND=NLINK('LUND',0)
        IF(JLUND.NE.0) THEN
          IF (IW(JLUND).NE.N*15+2) THEN
            WRITE(LMCWRT,112) 'LUND',IW(JLUND),N*15+2
            STOP
          ENDIF
        ELSE
          WRITE(LMCWRT,111) 'LUND'
          STOP
        ENDIF
        DO 30 I=1,N
          DO 20 I1=1,5
            K(I,I1) = ITABL(JLUND,I,I1+ 0)
            P(I,I1) = RTABL(JLUND,I,I1+ 5)
            V(I,I1) = RTABL(JLUND,I,I1+10)
 20       CONTINUE
 30     CONTINUE
      ELSEIF (IMODE.EQ.2) THEN
C Do nothing since skipping a bank means not reading it.
      ENDIF
 111  FORMAT(1X,'Unable to read bank: ',A4)
 112  FORMAT(1X,'Error in bank length of bank ',A4,'. Found: ',
     &   1I8,' <> ',1I8)
C
      RETURN
      END
      SUBROUTINE EXCRW(IMODE,IER)
C======================================================================
C! Color Reconnection Write
C
C  Author      : A.Waananen
C  Created     : 01-Oct-96
C
C  Description :
C
C   Write event to file.
C
C   This file is generator specific. The code
C   below is an implementation for Excalibur. It
C   should be fairly easy to port to another 
C   generator. Only the routines EXCRR and EXCRW
C   needs to be modified.
C
C  Input:
C     IMODE : Mode
C     IMODE : Mode
C       = 0 Write generator specific stuff
C       = 1 Write Lund variables
C     IER   : Error code
C
C  Called : EXCR
C  Calls  :  
C======================================================================
      IMPLICIT NONE
C
C LUJETS
      INTEGER N,K
      REAL    P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)      
C
C Local variables
C
      INTEGER IMODE,IER
      INTEGER LUN,I,ID(4)
C
C User variables+commons 
C (LUJ1 and LUJ2 is a must!)
      INTEGER LUJ1,LUJ2
      COMMON /LUQPOS/ LUJ1(2),LUJ2(2)
C Excalibur variables (Matrix elements)
      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
     &  ,IEXOFP,IEXPHG,IEXPRO
      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
     &  ,EXCSIG
      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
     &  ,EXCSIG(3,8)
C Logical Unit
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,SDVRT,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153) 
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
C W mass and width
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
C Aleph Variables
      INTEGER ALIDPR
      COMMON /ALEXPR/ ALIDPR
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON / BCS / IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
      INTEGER ALTABL,JKXCR,JLUNH,JLUND
      REAL TABL(4000*15)
      INTEGER KABL(4000*15)
      EQUIVALENCE (TABL,KABL)
C
C Set logical unit
C
      LUN = IEXUCR
      IER = 0
C
C Write generator specific variables
C
      IF (IMODE.EQ.0) THEN
C Pack process code
        ID(1)  = IPROC(IEXPRO)/1000000
        ID(2)  = MOD(IPROC(IEXPRO)/10000,100)
        ID(3)  = MOD(IPROC(IEXPRO)/100,100)
        ID(4)  = MOD(IPROC(IEXPRO),100)
        ALIDPR = ID(1)*32768 + ID(2)*1024 + ID(3)*32 + ID(4)
        ALIDPR = ALIDPR*1000 + IEXPHG*10 + IEXCCF
C        
        KABL(1) = ALIDPR
        TABL(2) = REAL(WM)
        TABL(3) = REAL(WW)
        TABL(4) = REAL(EX1234)
        TABL(5) = REAL(EX1432)
        TABL(6) = REAL(EXINT)
        DO I=1,8
          TABL(6+(I-1)*3+1) = REAL(EXCSIG(1,I))
          TABL(6+(I-1)*3+2) = REAL(EXCSIG(2,I))
          TABL(6+(I-1)*3+3) = REAL(EXCSIG(3,I))
        ENDDO
        TABL(31) = REAL(VERTEX(1))
        TABL(32) = REAL(VERTEX(2))
        TABL(33) = REAL(VERTEX(3))
        TABL(34) = REAL(VERTEX(4))
        JKXCR = ALTABL('KXCR',34,1,TABL,'2I,(1I,33F)','T')
        IF (JLUND.EQ.0) IER = IER + 1
      ELSEIF (IMODE.EQ.1) THEN
        KABL(1) = LUJ1(1)
        KABL(2) = LUJ1(2)
        KABL(3) = LUJ2(1)
        KABL(4) = LUJ2(2)
        KABL(5) = N
        JLUNH = ALTABL('LUNH',5,1,TABL,'2I,(5I)','T')
        IF (JLUNH.EQ.0) IER = IER + 2
        DO 10 I=1,N
          KABL((I-1)*15+ 1) = K(I,1)
          KABL((I-1)*15+ 2) = K(I,2)
          KABL((I-1)*15+ 3) = K(I,3)
          KABL((I-1)*15+ 4) = K(I,4)
          KABL((I-1)*15+ 5) = K(I,5)
          TABL((I-1)*15+ 6) = REAL(P(I,1))
          TABL((I-1)*15+ 7) = REAL(P(I,2))
          TABL((I-1)*15+ 8) = REAL(P(I,3))
          TABL((I-1)*15+ 9) = REAL(P(I,4))
          TABL((I-1)*15+10) = REAL(P(I,5))
          TABL((I-1)*15+11) = REAL(V(I,1))
          TABL((I-1)*15+12) = REAL(V(I,2))
          TABL((I-1)*15+13) = REAL(V(I,3))
          TABL((I-1)*15+14) = REAL(V(I,4))
          TABL((I-1)*15+15) = REAL(V(I,5))
 10     CONTINUE
        JLUND = ALTABL('LUND',15,N,TABL,'2I,(5I,10F)','T')
        IF (JLUND.EQ.0) IER = IER + 4
      ENDIF
C
      RETURN
      END
      SUBROUTINE EXCR(IMODE,ITYPE,WM,WW,ISTAT)
C======================================================================
C! Color Reconnection
C
C  Author      : A.Waananen
C  Created     : 01-Oct-96
C
C  Description :
C    Color reconnection main routine
C
C  Input:
C    IMODE : mode
C        = 0 Normal
C        = 1 Read
C        = 2 Write
C    ITYPE   : Color reconection type
C        = 0 : No reconnection
C        = 1 : Instantaneous reconnection - reconnection before shower
C        = 2 : Intermediate  reconnection - reconnection after  shower
C        = 3 : Strings described as spheres - instantaneous overlap.
C        = 4 : Strings described as spheres - time retarded overlap.
C        = 5 : Strings described as cylinders - instantaneous overlap.
C        = 6 : Strings described as cylinders - time retarded overlap.
C        = 7 : Strings as Vortex lines. May reconnect when they cross.
C        = 8 : Strings as Vortex lines. May reconnect when the total
C              string length is reduced.
C    WM      : W mass
C    WW      : W width
C    ISTAT   : Status
C
C  Called    : USER
C  Calls     : EXCRR,EXCRW,LUJOIN,LUSHOW,LUPREP,LUEDIT
C======================================================================
      IMPLICIT NONE
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
      DOUBLE PRECISION WM,WW
C LUDAT1
      INTEGER MSTJ,MSTU
      REAL    PARJ,PARU
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C LUJETS
      INTEGER N,K
      REAL    P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)      
C
      INTEGER IMODE,ITYPE
      INTEGER LUJ1,LUJ2
      COMMON /LUQPOS/ LUJ1(2),LUJ2(2)
C
      INTEGER MREC,SJOST1,IER,I,ISTAT
      INTEGER LUS1(2),LUS2(2),LUTMP(4)
      DOUBLE PRECISION B1(5),B2(5)
C
C Read event if in read-mode
C
      IF (IMODE.EQ.1) THEN
        CALL EXCRR(0,IER)
C EXCRR Returns IER=1 means EOF!
        IF (IER.EQ.1) GOTO 400
        CALL EXCRR(1,IER)
        NCREVT = NCREVT + 1
      ENDIF
C      
C No hadronization if quarks are massless
C
      IF((ABS(K(LUJ1(1),2)).LT.7.AND.P(LUJ1(1),5).LE.0.0D0).OR
     &  .(ABS(K(LUJ1(2),2)).LT.7.AND.P(LUJ1(2),5).LE.0.0D0).OR
     &  .(ABS(K(LUJ2(1),2)).LT.7.AND.P(LUJ2(1),5).LE.0.0D0).OR
     &  .(ABS(K(LUJ2(2),2)).LT.7.AND.P(LUJ2(2),5).LE.0.0D0)) GOTO
     &  850
C No hadronization if decaying leptons are massless
C
      IF((ABS(K(LUJ1(1),2)).EQ.15.AND.P(LUJ1(1),5).LE.0.0D0).OR
     &  .(ABS(K(LUJ1(2),2)).EQ.15.AND.P(LUJ1(2),5).LE.0.0D0).OR
     &  .(ABS(K(LUJ2(1),2)).EQ.15.AND.P(LUJ2(1),5).LE.0.0D0).OR
     &  .(ABS(K(LUJ2(2),2)).EQ.15.AND.P(LUJ2(2),5).LE.0.0D0)) GOTO
     &  850
C
C Initialize strings
C
      LUS1(1) = LUJ1(1)
      LUS1(2) = LUJ1(2)
      LUS2(1) = LUJ2(1)
      LUS2(2) = LUJ2(2)
C
C Find masses of the two strings
C 
      B1(5)=0.0D0
      B2(5)=0.0D0
      DO 5 I = 1, 4
        B1(I) = DBLE(P(LUS1(1),I)+P(LUS1(2),I))
        B2(I) = DBLE(P(LUS2(1),I)+P(LUS2(2),I))
        IF (I.GT.3) GOTO 5
        B1(5) = B1(5) + B1(I)*B1(I)
        B2(5) = B2(5) + B2(I)*B2(I)
 5    CONTINUE
      B1(5) = SQRT(MAX(0.0D0,B1(4)*B1(4)-B1(5)))
      B2(5) = SQRT(MAX(0.0D0,B2(4)*B2(4)-B2(5)))
C
C First, disable particle decays.
C
      MSTJ(21) = 0
C
C Do Reconnection
C
      MREC = 0
C
C 4 quark states are treated differently...
C
      IF (ABS(K(LUJ1(1),2)).LT.7.AND.ABS(K(LUJ2(1),2)).LT.7) THEN
C
C Handle simple color reconnection options
C
        IF (ITYPE.EQ.1) THEN
          MREC=1
          I = LUJ1(2)
          LUJ1(2) = LUJ2(2)
          LUJ2(2) = I
        ELSEIF (ITYPE.EQ.2) THEN
          MREC=1
          I = LUJ1(2)
          LUJ1(2) = LUJ2(2)
          LUJ2(2) = I
          I = LUS1(2)
          LUS1(2) = LUS2(2)
          LUS2(2) = I
        ELSEIF (ITYPE.EQ.3.OR.ITYPE.EQ.4) THEN
          IF ((ITYPE.EQ.3.AND.SJOST1(1,B1,B2,WM,WW).EQ.1).OR.(ITYPE.EQ
     &      .4.AND.SJOST1(2,B1,B2,WM,WW).EQ.1)) THEN
            MREC=1
            I = LUJ1(2)
            LUJ1(2) = LUJ2(2)
            LUJ2(2) = I
          ENDIF
        ENDIF
*
* Shower the event if not read from file
*
        IF (IMODE.NE.1) THEN
          CALL LUJOIN(2,LUJ1)
          CALL LUJOIN(2,LUJ2)
          B1(5)=0.0D0
          B2(5)=0.0D0
          DO 6 I = 1, 4
            B1(I) = DBLE(P(LUS1(1),I)+P(LUS1(2),I))
            B2(I) = DBLE(P(LUS2(1),I)+P(LUS2(2),I))
            IF (I.GT.3) GOTO 6
            B1(5) = B1(5) + B1(I)*B1(I)
            B2(5) = B2(5) + B2(I)*B2(I)
 6        CONTINUE
          B1(5) = SQRT(MAX(0.0D0,B1(4)*B1(4)-B1(5)))
          B2(5) = SQRT(MAX(0.0D0,B2(4)*B2(4)-B2(5)))
          CALL LUSHOW(LUS1(1),LUS1(2),REAL(B1(5)))
          CALL LUSHOW(LUS2(1),LUS2(2),REAL(B2(5)))
        ENDIF
*
* Edit the event record for the special LUND 'after-burner'
* color reconnection options....
*
        IF (ITYPE.GE.5.AND.ITYPE.LE.8) THEN
          I=MSTJ(14)
          MSTJ(14)=-1
          CALL LUPREP(0)
          MSTJ(14)=I
* Offset original partons so that they are not removed by LUEDIT.
          K(LUJ1(1),1)=K(LUJ1(1),1)-20
          K(LUJ1(2),1)=K(LUJ1(2),1)-20
          K(LUJ2(1),1)=K(LUJ2(1),1)-20
          K(LUJ2(2),1)=K(LUJ2(2),1)-20
          CALL LUEDIT(12)
          CALL LUEDIT(14)
          K(LUJ1(1),1)=K(LUJ1(1),1)+20
          K(LUJ1(2),1)=K(LUJ1(2),1)+20
          K(LUJ2(1),1)=K(LUJ2(1),1)+20
          K(LUJ2(2),1)=K(LUJ2(2),1)+20
        ENDIF
*
* Save event record for 4 quark state
*
        IF (IMODE.EQ.2) THEN
          CALL EXCRW(0,IER)
          CALL EXCRW(1,IER)
        ENDIF
*
        IF (ITYPE.EQ.5) CALL SJOST2(1,B1,B2,WM,WW,LUJ1,LUJ2,MREC)
        IF (ITYPE.EQ.6) CALL SJOST2(2,B1,B2,WM,WW,LUJ1,LUJ2,MREC)
        IF (ITYPE.EQ.7) CALL SJOST3(1,B1,B2,WM,WW,LUJ1,LUJ2,MREC)
        IF (ITYPE.EQ.8) CALL SJOST3(2,B1,B2,WM,WW,LUJ1,LUJ2,MREC)
C
C Skip (read) old event if color reconnection did (did not) occur
C
* Skip the rest if no where no Color Reconnection and the event was read
        IF (IMODE.EQ.1) THEN
          IF(MREC.EQ.0) THEN
* Read everything from the old event
            CALL EXCRR(1,IER)
            GOTO 850
          ELSE
* Skip the fragmentation from the old event
            CALL EXCRR(2,IER)
          ENDIF
        ENDIF
      ELSEIF (ABS(K(LUJ1(1),2)).LT.7.AND.ABS(K(LUJ2(1),2)).LE.16)
     &    THEN
        CALL LUJOIN(2,LUJ1)
        CALL LUSHOW(LUS1(1),LUS1(2),REAL(B1(5)))
      ELSEIF (ABS(K(LUJ2(1),2)).LT.7.AND.ABS(K(LUJ1(1),2)).LE.16)
     &    THEN
        CALL LUJOIN(2,LUJ2)
        CALL LUSHOW(LUS2(1),LUS2(2),REAL(B2(5)))
      ELSEIF (ABS(K(LUJ1(1),2)).LT.7.AND.ABS(K(LUJ2(1),2)).EQ.21)
     &    THEN
*  ---- Test option only for cross-section calculations......
        CALL LUFLP(LUJ1(2),LUJ2(1))
        CALL LUFLP(LUJ2(1),LUJ2(2))
        LUTMP(1)=LUJ1(1)
        LUTMP(2)=LUJ1(2)
        LUTMP(3)=LUJ2(1)
        LUTMP(4)=LUJ2(2)
        LUS1(2)=LUJ2(2)
        CALL LUJOIN(4,LUTMP)
* Be aware: NO shower for this option....
*           (LUSHOW can only handle up to 3 partons)
      ENDIF
C
C Now fragment the string(s)...
C
      CALL LUEXEC
C
C Finally, re-enable particle decays, and let LUND decay them.
C
      MSTJ(21) = 2
      CALL LUEXEC
*
* Write: For 4 quark state output the record after
* reconnection (ID=1). Otherwise output whole event record (ID=2).
*
      IF (IMODE.EQ.2.AND.
     &   ABS(K(LUJ1(1),2)).LT.7.AND.ABS(K(LUJ2(1),2)).LT.7)
     &   THEN
        CALL EXCRW(1,IER)
        NCREVT = NCREVT + 1
      ENDIF
 850  CONTINUE
      RETURN
C Return here if EOF is reached during READ
 400  ISTAT = 2
      RETURN
      END
      SUBROUTINE EXCRI(IMODE,LUN)
C======================================================================
C! Color Reconnection Initialisation
C
C  Author      : A.Waananen
C  Created     : 01-Oct-96
C
C  Description : Write banner
C
C  Input:
C     IMODE : Color Reconnection mode (see EXCR)
C     LUN   : Terminal output unit
C
C  Called : USER
C  Calls  :  
C======================================================================
      IMPLICIT NONE
C
      INTEGER IMODE,LUN,I
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
C
      NCREVT = 0
      WRITE(LUN,1020)
      WRITE(LUN,1010) ('=',I=1,65)
      IF (IMODE.EQ.1) THEN
        WRITE(LUN,1000) '  R E A D'
      ELSEIF (IMODE.EQ.2) THEN
        WRITE(LUN,1000) 'W R I T E'
      ELSE
        WRITE(LUN,1000) 'E R R O R'
      ENDIF
      WRITE(LUN,1010) ('=',I=1,65)
      WRITE(LUN,1020)
 1000 FORMAT(
     &   11X,'= =',59X,'= =',/,11X,
     &  '= = C O L O R - R E C O N N E C T I O N   ',A,
     &   '   M O D E = =',/,
     &   11X,'= =',59X,'= =')
 1010 FORMAT(11X,65A)
 1020 FORMAT(///)
      RETURN
      END
      SUBROUTINE EXCRE(IMODE,LUN)
C======================================================================
C! Color Reconnection End
C
C  Author      : A.Waananen
C  Created     : 01-Oct-96
C
C  Description : Report on number of processed events
C
C  Input:
C     IMODE : Color Reconnection mode (see EXCR)
C     LUN   : Terminal output unit
C
C  Called : USER
C  Calls  :  
C======================================================================
      IMPLICIT NONE
C
      INTEGER IMODE,LUN,I
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
C
      WRITE(LUN,1020)
      WRITE(LUN,1010) ('=',I=1,65)
      IF (IMODE.EQ.1) THEN
        WRITE(LUN,1000) '  R E A D'
      ELSEIF (IMODE.EQ.2) THEN
        WRITE(LUN,1000) 'W R I T E'
      ELSE
        WRITE(LUN,1000) 'E R R O R'
      ENDIF
      WRITE(LUN,1030) NCREVT
      WRITE(LUN,1010) ('=',I=1,65)
      WRITE(LUN,1020)
 1000 FORMAT(
     &   11X,'= =',59X,'= =',/,11X,
     &  '= = C O L O R - R E C O N N E C T I O N   ',A,
     &   '   M O D E = =',/,
     &   11X,'= =',59X,'= =')
 1010 FORMAT(11X,65A)
 1020 FORMAT(///)
 1030 FORMAT(
     &   11X,'= =',59X,'= =',/,11X,
     &  '= = S U M M A R Y :   ',1I7,
     &   '  E V E N T S  P R O C E S S E D = =',/,
     &   11X,'= =',59X,'= =')
      RETURN
      END
      SUBROUTINE LUFLP(L1,L2)
C======================================================================
C! Flip two entries in LUND common
C======================================================================
C LUJETS
      INTEGER N,K
      REAL    P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
*
      INTEGER I,L1,L2
      INTEGER KTMP
      REAL PTMP,VTMP
      DIMENSION KTMP(5),PTMP(5),VTMP(5)
*
      DO I = 1,5
        KTMP(I) = K(L1,I)
        PTMP(I) = P(L1,I)
        VTMP(I) = V(L1,I)
        K(L1,I) = K(L2,I)
        P(L1,I) = P(L2,I)
        V(L1,I) = V(L2,I)
        K(L2,I) = KTMP(I)
        P(L2,I) = PTMP(I)
        V(L2,I) = VTMP(I)
      ENDDO
*
      RETURN
      END
      INTEGER FUNCTION SJOST1(IMODE,B1,B2,WM,WW)
C========================================================
C! Reconnect strings at origin of event, after shower,
C  based on spherical overlap.
C
C  Input:  MODE  = 0 : no reconnection.
C                = 1 : reconnect, instantaneous sphere.
C                = 2 : reconnect, time-retarded sphere.
C          B1,B2 = Masses of the strings
C          WM,WW = W mass and width
C
C  This routine is mainly extracted from the pywwa3 routine
C  of the "color rearrangement in WW events" routines of
C  Sjostrand.
C========================================================
      IMPLICIT NONE
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
* Arguments
      INTEGER IMODE
      DOUBLE PRECISION B1(5),B2(5),WM,WW
* Local variables
      INTEGER IPT
      REAL RLU
      DOUBLE PRECISION PM1,PM2,PE1,PE2,PCOM,BE1,BE2,GA1,GA2,TAU1,TAU2
     &  ,GTMAX,SUM,R,PHI,X,Y,Z,T,WTSMP,R2,WT1,WT2,WT,TR,RES,PREC
      INTEGER NPT
      PARAMETER (NPT=100)
      DOUBLE PRECISION PI,HBAR
      PARAMETER (PI=3.14159265358979324D0,HBAR=0.197327053D0)
      EXTERNAL RLU
*
      SJOST1=0
C...Kinematics: boost factors etc.
      PM1=B1(5)
      PM2=B2(5)
      PE1=B1(4)
      PE2=B2(4)
      PCOM=SQRT(B1(1)*B1(1)+B1(2)*B1(2)+B1(3)*B1(3))
      BE1=PCOM/PE1
      GA1=PE1/PM1
      BE2=PCOM/PE2
      GA2=PE2/PM2
 
C...Select W decay times.
      TAU1=HBAR*(-LOG(DBLE(RLU(0))))*PM1/
     &SQRT((PM1*PM1-WM*WM)**2+(PM1*PM1*WW/WM)**2)
      TAU2=HBAR*(-LOG(DBLE(RLU(0))))*PM2/
     &SQRT((PM2*PM2-WM*WM)**2+(PM2*PM2*WW/WM)**2)
      GTMAX=MAX(GA1*TAU1,GA2*TAU2)
 
C...Loop over number of space-time points.
      SUM=0.0D0
      DO 110 IPT=1,NPT
 
C...Pick x,y,z,t Gaussian (width RHAD and TFRAG, respectively).
      R=SQRT(-LOG(DBLE(RLU(0))))
      PHI=2.0D0*PI*DBLE(RLU(0))
      X=RHAD*R*COS(PHI)
      Y=RHAD*R*SIN(PHI)
      R=SQRT(-LOG(DBLE(RLU(0))))
      PHI=2.0D0*PI*DBLE(RLU(0))
      Z=RHAD*R*COS(PHI)
      T=GTMAX+SQRT(0.5D0)*TFRAG*R*ABS(SIN(PHI))
 
C...Weight for sample distribution and W+ and W- distributions.
      WTSMP=EXP(-(X*X+Y*Y+Z*Z)/RHAD/RHAD)*
     &EXP(-2.0D0*(T-GTMAX)**2/TFRAG/TFRAG)
      R2=X*X+Y*Y+GA1*GA1*(Z-BE1*T)**2
      TR=GA1*(T-BE1*Z)-TAU1
      WT1=EXP(-R2/(2.0D0*RHAD*RHAD))*EXP(-TR*TR/TFRAG/TFRAG)
      IF(IMODE.EQ.2.AND.TR-SQRT(R2).LT.0.0D0) WT1=0.0D0
      R2=X*X+Y*Y+GA2*GA2*(Z+BE2*T)**2
      TR=GA2*(T+BE2*Z)-TAU2
      WT2=EXP(-R2/(2.0D0*RHAD*RHAD))*EXP(-TR*TR/TFRAG/TFRAG)
      IF(IMODE.EQ.2.AND.TR-SQRT(R2).LT.0.) WT2=0.0D0
C...Result of integration.
      WT=WT1*WT2/WTSMP
      SUM=SUM+WT
  110 CONTINUE
      RES=SUM/NPT
C...Decide whether to reconnect.
      PREC=0.5D0*(1.0D0-EXP(-RPROB*RES))
      IF(PREC.GT.DBLE(RLU(0))) SJOST1=1
 999  RETURN
      END
      SUBROUTINE SJOST2(IMODE,B1,B2,WM,WW,LUJ1,LUJ2,MREC)
C========================================================
C! Reconnect strings according to overlap of cylindrical 
C  string piece volumes.
C
C Input:  MODE  = 0 : no reconnection.
C               = 1 : reconnect, instantaneous cylinders.
C               = 2 : reconnect, time-retarded cylinders.
C         B1,B2 = Masses of the strings
C         WM,WW = W mass and width
C         LUJ1,LUJ2 = Pointers to strings in LUND common
C
C Output: MREC  = 0 : no reconnection
C               = 1 : reconnection occurred
C
C  This routine is mainly extracted from the pywwa4 routine
C  of the "color rearrangement in WW events" routines of
C  Sjostrand.
C
C  Created  01-Mar-96 : J.B.Hansen
C  Modified 01-Oct-96 : A.Waananen
C========================================================
      IMPLICIT NONE
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
* Part of LUND common...
      INTEGER N,K
      REAL    P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)      
* Arguments
      INTEGER IMODE,LUJ1(2),LUJ2(2)
      DOUBLE PRECISION B1(5),B2(5),WM,WW
* Local variables
      INTEGER NPT
      PARAMETER (NPT=100)
      INTEGER I,J,NNP,NNM,ISGP,ISGM,INP(50),INM(50),IJOIN(100),IIP,I1,I2
     &  ,IIM,NACC,IPT,IMAXP,IMAXM,IAP(NPT),IAM(NPT),MREC,IA,NJOIN,IS
     &  ,IACC
      REAL RLU
      DOUBLE PRECISION TP,TM,XP(3),XM(3),GTMAX,P1A,P2A,V1(3),V2(3)
     &  ,BETP(50,4),DIRP(50,3),DIRL,BETM(50,4),DIRM(50,3),SUM,R,PHI,X,Y
     &  ,Z,T,BLOWR,BLOWT,WTSMP,WTMAXP,XD(4),XB(4),BED,BEDG,SR2,SZ2,WTP
     &  ,WTMAXM,WTM,WT,WTA(NPT),RSUM,RES,PREC
      PARAMETER (BLOWR=2.50D0)
      PARAMETER (BLOWT=2.0D0)
      DOUBLE PRECISION PI,HBAR
      PARAMETER (PI=3.14159265358979324D0,HBAR=0.197327053D0)
      EXTERNAL RLU      
*
      IACC=0
C...Select decay vertices of W+ and W-.
      TP=HBAR*(-LOG(DBLE(RLU(0))))*B1(4)/
     &SQRT((B1(5)*B1(5)-WM*WM)**2+(B1(5)*B1(5)*WW/WM)**2)
      TM=HBAR*(-LOG(DBLE(RLU(0))))*B2(4)/
     &SQRT((B2(5)*B2(5)-WM*WM)**2+(B2(5)**2*WW/WM)**2)
      DO 110 J=1,3
      XP(J)=TP*B1(J)/B1(4)
      XM(J)=TM*B2(J)/B2(4)
  110 CONTINUE
      GTMAX=MAX(TP,TM)
 
C...Find partons pointing back to "W+"-pairing and "W-"-pairing; store them with quark
C...end of string first.
      NNP=0
      NNM=0
      ISGP=0
      ISGM=0
      J=MAX(LUJ1(1),LUJ1(2),LUJ2(1),LUJ2(2))+1
      DO 140 I=J,N
      IF(K(I,1).NE.1.AND.K(I,1).NE.2) GOTO 140
C...Do not count photons (added 19/6 1995).
      IF(K(I,2).EQ.22) GOTO 140
      IF(K(I,3).EQ.LUJ1(1).OR.K(I,3).EQ.LUJ1(2)) THEN
        IF(ISGP.EQ.0) ISGP=ISIGN(1,K(I,2))
        NNP=NNP+1
        IF(ISGP.EQ.1) THEN
          INP(NNP)=I
        ELSE
          DO 120 I1=NNP,2,-1
  120     INP(I1)=INP(I1-1)
          INP(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGP=0
      ELSEIF(K(I,3).EQ.LUJ2(1).OR.K(I,3).EQ.LUJ2(2)) THEN
        IF(ISGM.EQ.0) ISGM=ISIGN(1,K(I,2))
        NNM=NNM+1
        IF(ISGM.EQ.1) THEN
          INM(NNM)=I
        ELSE
          DO 130 I1=NNM,2,-1
  130     INM(I1)=INM(I1-1)
          INM(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGM=0
      ENDIF
  140 CONTINUE
 
C...Reconstruct velocity and direction of "W+"-paired string pieces.
      DO 180 IIP=1,NNP-1
      IF(K(INP(IIP),2).LT.0) GOTO 180
      I1=INP(IIP)
      I2=INP(IIP+1)
      P1A=DBLE(SQRT(P(I1,1)*P(I1,1)+P(I1,2)*P(I1,2)+P(I1,3)*P(I1,3)))
      P2A=DBLE(SQRT(P(I2,1)*P(I2,1)+P(I2,2)*P(I2,2)+P(I2,3)*P(I2,3)))
      DO 150 J=1,3
      V1(J)=DBLE(P(I1,J))/P1A
  150 V2(J)=DBLE(P(I2,J))/P2A
      DO 160 J=1,3
      BETP(IIP,J)=0.5D0*(V1(J)+V2(J))
  160 DIRP(IIP,J)=V1(J)-V2(J)
      BETP(IIP,4)=1.0D0/SQRT(1.0D0-BETP(IIP,1)*BETP(IIP,1)-BETP(IIP,2)
     &  *BETP(IIP,2)-BETP(IIP,3)*BETP(IIP,3))
      DIRL=SQRT(DIRP(IIP,1)*DIRP(IIP,1)+DIRP(IIP,2)*DIRP(IIP,2)+DIRP(IIP
     &  ,3)*DIRP(IIP,3))
      DO 170 J=1,3
  170 DIRP(IIP,J)=DIRP(IIP,J)/DIRL
  180 CONTINUE
 
C...Reconstruct velocity and direction of "W-"-paired string pieces.
      DO 230 IIM=1,NNM-1
      IF(K(INM(IIM),2).LT.0) GOTO 230
      I1=INM(IIM)
      I2=INM(IIM+1)
      P1A=DBLE(SQRT(P(I1,1)*P(I1,1)+P(I1,2)*P(I1,2)+P(I1,3)*P(I1,3)))
      P2A=DBLE(SQRT(P(I2,1)*P(I2,1)+P(I2,2)*P(I2,2)+P(I2,3)*P(I2,3)))
      DO 190 J=1,3
      V1(J)=DBLE(P(I1,J))/P1A
  190 V2(J)=DBLE(P(I2,J))/P2A
      DO 200 J=1,3
      BETM(IIM,J)=0.5D0*(V1(J)+V2(J))
  200 DIRM(IIM,J)=V1(J)-V2(J)
      BETM(IIM,4)=1.0D0/SQRT(1.0D0-BETM(IIM,1)*BETM(IIM,1)-BETM(IIM,2)
     &  *BETM(IIM,2)-BETM(IIM,3)*BETM(IIM,3))
      DIRL=SQRT(DIRM(IIM,1)*DIRM(IIM,1)+DIRM(IIM,2)*DIRM(IIM,2)+DIRM(IIM
     &  ,3)*DIRM(IIM,3))
      DO 220 J=1,3
  220 DIRM(IIM,J)=DIRM(IIM,J)/DIRL
  230 CONTINUE
 
C...Loop over number of space-time points.
      NACC=0
      SUM=0.0D0
      DO 280 IPT=1,NPT
 
C...Pick x,y,z,t Gaussian (width RHAD and TFRAG, respectively).
      R=SQRT(-LOG(DBLE(RLU(0))))
      PHI=2.0D0*PI*DBLE(RLU(0))
      X=BLOWR*RHAD*R*COS(PHI)
      Y=BLOWR*RHAD*R*SIN(PHI)
      R=SQRT(-LOG(DBLE(RLU(0))))
      PHI=2.0D0*PI*DBLE(RLU(0))
      Z=BLOWR*RHAD*R*COS(PHI)
      T=GTMAX+BLOWT*SQRT(0.5D0)*TFRAG*R*ABS(SIN(PHI))
 
C...Weight for sample distribution.
      WTSMP=EXP(-(X*X+Y*Y+Z*Z)/(BLOWR*BLOWR*RHAD*RHAD))*
     &EXP(-2.0D0*(T-GTMAX)**2/(BLOWT*BLOWT*TFRAG*TFRAG))
 
C...Loop over W+ string pieces and find one with largest weight.
      IMAXP=0
      WTMAXP=1.0D-10
      XD(1)=X-XP(1)
      XD(2)=Y-XP(2)
      XD(3)=Z-XP(3)
      XD(4)=T-TP
      DO 250 IIP=1,NNP-1
      IF(K(INP(IIP),2).LT.0) GOTO 250
      BED=BETP(IIP,1)*XD(1)+BETP(IIP,2)*XD(2)+BETP(IIP,3)*XD(3)
      BEDG=BETP(IIP,4)*(BETP(IIP,4)*BED/(1.0D0+BETP(IIP,4))-XD(4))
      DO 240 J=1,3
  240 XB(J)=XD(J)+BEDG*BETP(IIP,J)
      XB(4)=BETP(IIP,4)*(XD(4)-BED)
      SR2=XB(1)*XB(1)+XB(2)*XB(2)+XB(3)*XB(3)
      SZ2=(DIRP(IIP,1)*XB(1)+DIRP(IIP,2)*XB(2)+DIRP(IIP,3)*XB(3))**2
      WTP=EXP(-(SR2-SZ2)/(2.0D0*RHAD*RHAD))*EXP(-(XB(4)*XB(4)-SZ2)/TFRAG
     &  /TFRAG)
      IF(IMODE.EQ.1.AND.XB(4)-SQRT(SZ2).LT.0.0D0) WTP=0.0D0
      IF(IMODE.EQ.2.AND.XB(4)-SQRT(SR2).LT.0.0D0) WTP=0.0D0
      IF(WTP.GT.WTMAXP) THEN
        IMAXP=IIP
        WTMAXP=WTP
      ENDIF
  250 CONTINUE
 
C...Loop over W- string pieces and find one with largest weight.
      IMAXM=0
      WTMAXM=1.0D-10
      XD(1)=X-XM(1)
      XD(2)=Y-XM(2)
      XD(3)=Z-XM(3)
      XD(4)=T-TM
      DO 270 IIM=1,NNM-1
      IF(K(INM(IIM),2).LT.0) GOTO 270
      BED=BETM(IIM,1)*XD(1)+BETM(IIM,2)*XD(2)+BETM(IIM,3)*XD(3)
      BEDG=BETM(IIM,4)*(BETM(IIM,4)*BED/(1.0D0+BETM(IIM,4))-XD(4))
      DO 260 J=1,3
  260 XB(J)=XD(J)+BEDG*BETM(IIM,J)
      XB(4)=BETM(IIM,4)*(XD(4)-BED)
      SR2=XB(1)*XB(1)+XB(2)*XB(2)+XB(3)*XB(3)
      SZ2=(DIRM(IIM,1)*XB(1)+DIRM(IIM,2)*XB(2)+DIRM(IIM,3)*XB(3))**2
      WTM=EXP(-(SR2-SZ2)/(2.0D0*RHAD*RHAD))*EXP(-(XB(4)*XB(4)-SZ2)/TFRAG
     &  /TFRAG)
      IF(IMODE.EQ.1.AND.XB(4)-SQRT(SZ2).LT.0.0D0) WTM=0.0D0
      IF(IMODE.EQ.2.AND.XB(4)-SQRT(SR2).LT.0.0D0) WTM=0.0D0
      IF(WTM.GT.WTMAXM) THEN
        IMAXM=IIM
        WTMAXM=WTM
      ENDIF
  270 CONTINUE
 
C...Result of integration.
      WT=0.0D0
      IF(IMAXP.NE.0.AND.IMAXM.NE.0) THEN
        WT=WTMAXP*WTMAXM/WTSMP
        SUM=SUM+WT
        NACC=NACC+1
        IAP(NACC)=IMAXP
        IAM(NACC)=IMAXM
        WTA(NACC)=WT
      ENDIF
 
  280 CONTINUE
      RES=BLOWR*BLOWR*BLOWR*BLOWT*SUM/NPT
 
C...Decide whether to reconnect.
      PREC=1.0D0-EXP(-RPROB*RES)
      MREC=0
      IF(PREC.GT.RLU(0)) MREC=1
      IF(MREC.EQ.0) GOTO 320
 
C...Decide which pair of strings to reconnect.
      RSUM=DBLE(RLU(0))*SUM
      DO 290 IA=1,NACC
      IACC=IA
      RSUM=RSUM-WTA(IA)
      IF(RSUM.LE.0.0D0) GOTO 300
  290 CONTINUE
  300 IIP=IAP(IACC)
      IIM=IAM(IACC)
C...Recouple strings.
      NJOIN=0
      DO 310 IS=1,NNP+NNM
      NJOIN=NJOIN+1
      IF(IS.LE.IIP) THEN
        I=INP(IS)
      ELSEIF(IS.LE.IIP+NNM-IIM) THEN
        I=INM(IS-IIP+IIM)
      ELSEIF(IS.LE.IIP+NNM) THEN
        I=INM(IS-IIP-NNM+IIM)
      ELSE
        I=INP(IS-NNM)
      ENDIF
      IJOIN(NJOIN)=I
      IF(K(I,2).LT.0) THEN
        CALL LUJOIN(NJOIN,IJOIN)
        NJOIN=0
      ENDIF
  310 CONTINUE
  320 CONTINUE
 999  RETURN
      END
      SUBROUTINE SJOST3(IMODE,B1,B2,WM,WW,LUJ1,LUJ2,MREC)
C========================================================
C! Reconnect strings when they cross (type II superconductor).
C
C Input: MODE   = 0 : no string reconnection.
C               = 1 : string reconnection.
C               = 2 : string reconnection if string length decreased.
C         B1,B2 = Masses of the strings
C         WM,WW = W mass and width
C         LUJ1,LUJ2 = Pointers to strings in LUND common
C
C Output: MREC  = 0 : no reconnection
C               = 1 : reconnection occurred
C
C  This routine is mainly extracted from the pywwa5 routine
C  of the "color rearrangement in WW events" routines of
C  Sjostrand.
C
C  Created  01-Mar-96 : J.B.Hansen
C  Modified 01-Oct-96 : A.Waananen
C========================================================
      IMPLICIT NONE
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
* Part of LUND common...
      INTEGER N,K
      REAL    P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)      
* Arguments
      INTEGER IMODE,LUJ1(2),LUJ2(2)
      DOUBLE PRECISION B1(5),B2(5),WM,WW
* Local variables
      INTEGER I,J,NNP,NNM,ISGP,ISGM,INP(50),INM(50),NPAIR,NCROSS,I1P,I2P
     &  ,I1M,I2M,IPC(20),IMC(20),IC,IACC,NJOIN,IS,IANSW,I1,IIP,IIM
     &  ,IJOIN(100)
      REAL RLU
      DOUBLE PRECISION TP,TM,XP(3),XM(3),P1A,P2A,V1P(3),V2P(3),V1M(3)
     &  ,V2M(3),TC(0:20),TPC(20),TMC(20),TAUP,TAUM,PNFRAG,ELOLD,ELNEW,T
     &  ,FOUR
      INTEGER NPT,MREC
      PARAMETER (NPT=100)
      DOUBLE PRECISION PI,HBAR
      PARAMETER (PI=3.14159265358979324D0,HBAR=0.197327053D0)
      EXTERNAL RLU      
C...Function to give four product.
      FOUR(I,J)=DBLE(P(I,4)*P(J,4)-P(I,1)*P(J,1)-P(I,2)*P(J,2)-P(I,3)
     &  *P(J,3))
*
      IACC=0
      MREC=0
C...Select decay vertices of W+ and W-.
      TP=HBAR*(-LOG(DBLE(RLU(0))))*B1(4)/
     &SQRT((B1(5)*B1(5)-WM*WM)**2+(B1(5)*B1(5)*WW/WM)**2)
      TM=HBAR*(-LOG(DBLE(RLU(0))))*B2(4)/
     &SQRT((B2(5)*B2(5)-WM*WM)**2+(B2(5)**2*WW/WM)**2)
      DO 110 J=1,3
      XP(J)=TP*B1(J)/B1(4)
      XM(J)=TM*B2(J)/B2(4)
  110 CONTINUE
 
C...Find partons pointing back to "W+"-pairing and "W-"-pairing; store them with quark
C...end of string first.
      NNP=0
      NNM=0
      ISGP=0
      ISGM=0
      J=MAX(LUJ1(1),LUJ1(2),LUJ2(1),LUJ2(2))+1
      DO 140 I=J,N
      IF(K(I,1).NE.1.AND.K(I,1).NE.2) GOTO 140
C...Do not count photons (added 19/6 1995).
      IF(K(I,2).EQ.22) GOTO 140
      IF(K(I,3).EQ.LUJ1(1).OR.K(I,3).EQ.LUJ1(2)) THEN
        IF(ISGP.EQ.0) ISGP=ISIGN(1,K(I,2))
        NNP=NNP+1
        IF(ISGP.EQ.1) THEN
          INP(NNP)=I
        ELSE
          DO 120 I1=NNP,2,-1
  120     INP(I1)=INP(I1-1)
          INP(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGP=0
      ELSEIF(K(I,3).EQ.LUJ2(1).OR.K(I,3).EQ.LUJ2(2)) THEN
        IF(ISGM.EQ.0) ISGM=ISIGN(1,K(I,2))
        NNM=NNM+1
        IF(ISGM.EQ.1) THEN
          INM(NNM)=I
        ELSE
          DO 130 I1=NNM,2,-1
  130     INM(I1)=INM(I1-1)
          INM(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGM=0
      ENDIF
  140 CONTINUE
C...Loop through all string pieces, one from W+ and one from W-.
      NPAIR=0
      NCROSS=0
      TC(0)=0.0D0
      DO 200 IIP=1,NNP-1
      IF(K(INP(IIP),2).LT.0) GOTO 200
      I1P=INP(IIP)
      I2P=INP(IIP+1)
      DO 190 IIM=1,NNM-1
      IF(K(INM(IIM),2).LT.0) GOTO 190
      I1M=INM(IIM)
      I2M=INM(IIM+1)
      NPAIR=NPAIR+1
 
C...Find endpoint velocity vectors and determine crossing.
      DO 150 J=1,3
      V1P(J)=DBLE(P(I1P,J)/P(I1P,4))
      V2P(J)=DBLE(P(I2P,J)/P(I2P,4))
      V1M(J)=DBLE(P(I1M,J)/P(I1M,4))
      V2M(J)=DBLE(P(I2M,J)/P(I2M,4))
  150 CONTINUE
      CALL PYWWXX(TP,TM,XP,XM,V1P,V2P,V1M,V2M,IANSW,T,TAUP,TAUM)
      
C...Order crossings by time. End loop over crossings.
      IF(IANSW.EQ.1.AND.NCROSS.LT.20) THEN
        NCROSS=NCROSS+1
        DO 160 I1=NCROSS,1,-1
        IF(T.GT.TC(I1-1).OR.I1.EQ.1) THEN
          IPC(I1)=IIP
          IMC(I1)=IIM
          TC(I1)=T
          TPC(I1)=TAUP
          TMC(I1)=TAUM
          GOTO 170
        ELSE
          IPC(I1)=IPC(I1-1)
          IMC(I1)=IMC(I1-1)
          TC(I1)=TC(I1-1)
          TPC(I1)=TPC(I1-1)
          TMC(I1)=TMC(I1-1)
        ENDIF
  160   CONTINUE
  170   CONTINUE
      ENDIF
  190 CONTINUE
  200 CONTINUE
 
C...Loop over crossings; find first (if any) acceptable one.
      IF(NCROSS.GE.1) THEN
        DO 210 IC=1,NCROSS
        PNFRAG=EXP(-(TPC(IC)*TPC(IC)+TMC(IC)*TMC(IC))/TFRAG/TFRAG)
        IF(PNFRAG.GT.DBLE(RLU(0))) THEN
          IF(IMODE.EQ.1) THEN
            IACC=IC
            GOTO 220
          ELSE
            IIP=IPC(IC)
            IIM=IMC(IC)
            I1P=INP(IIP)
            I2P=INP(IIP+1)
            I1M=INM(IIM)
            I2M=INM(IIM+1)
            ELOLD=FOUR(I1P,I2P)*FOUR(I1M,I2M)
            ELNEW=FOUR(I1P,I2M)*FOUR(I1M,I2P)
            IF(ELNEW.LT.ELOLD) THEN
              IACC=IC
              GOTO 220
            ENDIF
          ENDIF
        ENDIF
  210   CONTINUE
  220   CONTINUE
      ENDIF
 
C...Recouple strings.
      IF(IACC.NE.0) THEN
        IIP=IPC(IACC)
        IIM=IMC(IACC)
        NJOIN=0
        DO 230 IS=1,NNP+NNM
        NJOIN=NJOIN+1
        IF(IS.LE.IIP) THEN
          I=INP(IS)
        ELSEIF(IS.LE.IIP+NNM-IIM) THEN
          I=INM(IS-IIP+IIM)
        ELSEIF(IS.LE.IIP+NNM) THEN
          I=INM(IS-IIP-NNM+IIM)
        ELSE
          I=INP(IS-NNM)
        ENDIF
        IJOIN(NJOIN)=I
        IF(K(I,2).LT.0) THEN
          CALL LUJOIN(NJOIN,IJOIN)
          NJOIN=0
        ENDIF
  230   CONTINUE
        MREC=1
      ENDIF
 999  RETURN
      END
      SUBROUTINE PYWWXX(TP,TM,XP,XM,V1P,V2P,V1M,V2M,IANSW,T,TAUP,TAUM)
C...Subroutine to determine if two string pieces cross.
C...P stands for W+ related quantities, M for W- related ones.
C...Input: TP, TM times of creation of string pieces.
C          XP, XM vertices of creation of string pieces.
C          V1P, V2P, V1M, V2M velocity vectors of endpoint partons.
C...Output: IANSW = 0 if not cross; = 1 if do cross; -1 if error.
C           T time of crossing (when they do cross, else 0).
C           TAUP, TAUM invariant times of strings at crossing point.
C
C JBH+AW: Changed: All REAL'S to DOUBLE PRECISION, introduced
C         IMPLICIT NONE and declarations of all variables
      IMPLICIT NONE
      INTEGER IANSW,I,J,K
      DOUBLE PRECISION TP,TM,XP(3),XM(3),V1P(3),V2P(3),V1M(3),V2M(3),T
     &  ,TAUP,TAUM,Q(4,3),XPP(3),XMM(3),DETER,S11,S12,S13,S21,S22,S23
     &  ,DEN,ALP,BET,D2PM,D2P,D2M
 
C...Function to do determinant of three rows of q matrix.
      DETER(I,J,K)=Q(I,1)*Q(J,2)*Q(K,3)-Q(I,1)*Q(K,2)*Q(J,3)+
     &Q(J,1)*Q(K,2)*Q(I,3)-Q(J,1)*Q(I,2)*Q(K,3)+
     &Q(K,1)*Q(I,2)*Q(J,3)-Q(K,1)*Q(J,2)*Q(I,3)
 
C...Define q matrix and find t.
      DO 100 J=1,3
      Q(1,J)=V2P(J)-V1P(J)
      Q(2,J)=-(V2M(J)-V1M(J))
      Q(3,J)=XP(J)-XM(J)-TP*V1P(J)+TM*V1M(J)
      Q(4,J)=V1P(J)-V1M(J)
  100 CONTINUE
      T=-DETER(1,2,3)/DETER(1,2,4)
 
C...Find alpha and beta.
      S11=Q(1,1)*(T-TP)
      S12=Q(2,1)*(T-TM)
      S13=Q(3,1)+Q(4,1)*T
      S21=Q(1,2)*(T-TP)
      S22=Q(2,2)*(T-TM)
      S23=Q(3,2)+Q(4,2)*T
      DEN=S11*S22-S12*S21
      ALP=(S12*S23-S22*S13)/DEN
      BET=(S21*S13-S11*S23)/DEN
 
C...Check if solution acceptable.
      IANSW=1
      IF(T.LT.TP.OR.T.LT.TM) IANSW=0
      IF(ALP.LT.0.0D0.OR.ALP.GT.1.0D0) IANSW=0
      IF(BET.LT.0.0D0.OR.BET.GT.1.0D0) IANSW=0
 
C...Find point of crossing and check that not inconsistent.
      DO 110 J=1,3
      XPP(J)=XP(J)+(V1P(J)+ALP*(V2P(J)-V1P(J)))*(T-TP)
      XMM(J)=XM(J)+(V1M(J)+BET*(V2M(J)-V1M(J)))*(T-TM)
  110 CONTINUE
      D2PM=(XPP(1)-XMM(1))**2+(XPP(2)-XMM(2))**2+(XPP(3)-XMM(3))**2
      D2P=XPP(1)*XPP(1)+XPP(2)*XPP(2)+XPP(3)*XPP(3)
      D2M=XMM(1)*XMM(1)+XMM(2)*XMM(2)+XMM(3)*XMM(3)
      IF(D2PM.GT.1.0D-4*(D2P+D2M)) IANSW=-1
 
C...Find string eigentimes at crossing.
      IF(IANSW.EQ.1) THEN
        TAUP=SQRT(MAX(0.0D0,(T-TP)**2-(XPP(1)-XP(1))**2-
     &  (XPP(2)-XP(2))**2-(XPP(3)-XP(3))**2))
        TAUM=SQRT(MAX(0.0D0,(T-TM)**2-(XMM(1)-XM(1))**2-
     &  (XMM(2)-XM(2))**2-(XMM(3)-XM(3))**2))
      ELSE
        TAUP=0.0D0
        TAUM=0.0D0
      ENDIF
 999  RETURN
      END
      SUBROUTINE FINALG
C***********************************************************************
C
C AUTHOR : FRANCOIS LE DIBERDER (1991)
C          DIBERDER@FRCPN11
C          LAL: (33-1) 64 46 89 15
C
C***********************************************************************
C-----------------------------------------------------------------------
C RADIATIVE CORRECTIONS TO THE REACTION
C              X Y  ->- CHARGED AND NEUTRAL PARTICLES
C-----------------------------------------------------------------------
C COMMUNICATION WITH THE PROGRAM IS DONE THROUGH THE COMMON : TALK
C-----------------------------------------------------------------------
C INPUT :
C        PIN(50,6)  AT MOST 50 TRACKS (INCLUDING PHOTONS TO BE RADIATED)
C                   THE FIRST TWO TRACKS ARE X AND Y
C        PIN(  ,1): PX
C               2 : PY
C               3 : PZ
C               4 : E   (RECALCULATED)
C               5 : M
C               6 : P   (RECALCULATED)
C        INQ(50)    CHARGE   (RADIATING TRACKS)
C                   TO KEEP A PAIR OF TRACKS FROM RADIATING
C                   SET THEIR INQ() TO 0
C        NC         NUMBER OF CHARGED TRACKS
C        NN         NUMBER OF NEUTRAL (AFTER THE CHARGED TRACKS)
C        IDEB       PRINTING OPTION (=1 => PRINT)
C        WMAX       MAXIMUM WEIGHT
C        NNG        NUMBER OF PHOTONS TO ADD TO THE FINAL STATE
C                   THE MINIMUM ENERGY OF EACH PHOTON IS SET TO
C                   KM*EBEAM IN ORDER TO AVOID TROUBLE WITH DIVISIONS
C-----------------------------------------------------------------------
C OUTPUT:
C        POUT(50,6) OUTPUT 4-MOMENTA,  INCLUDING THE NEW PHOTON(S)
C                   WHICH ARE ADDED AFTER THE INPUT PARTICLES
C        WMXX       MAXIMUM WEIGHT WHICH WAS SEEN
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C**************************************
C INTERNAL PARAMETERS [CAN BE MODIFIED]
C**************************************
C        KM         MINIMUM VALUE OF EGAM/EBEAM ( 0 < KM < KMAX )
C        NTRMAX     MAXIMUM NUMBER OF TRIALS. IF NTRIAL>NTRMAX THE
C                   THE CURRENT PHOTON GENERATION IS RESTARTED.
C        IW         FLAG TO SELECT BETWEEN VARIOUS 'FRAGMENTATION'
C                   SCHEMES.
C             =0    IS THE RECOMMENDED VALUE
C             =1    DEPOPULATES SLIGHTLY THE HIGH-ENERGY PART OF THE
C                   PHOTON ENERGY SPECTRUM
C             =2    DEMOCRATIC OVERALL BOOSTING OF THE FINAL STATE
C             =3    1 AND 2
C-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,K,O-Z)
      SAVE
C Final state radiation common
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG
      COMMON/TALK/WMAX,WMXX,POUT(50,6),PIN(50,6),INQ(50)
     *           ,NC,NN,IDEB,NNG
      DATA IPAS/0/
      JDEBUG=IDEB
      REDUCE=1.D+00/NNG
C INITIALIZATION DONE ONCE FOR ALL
      IF(IPAS.EQ.0)             THEN
         IPAS =  1
      PI    =  3.141592653D+00
      ALFA  =  1.D+00/(137.035989D+00*PI)
      CST0  =  ALFA*0.5D+00
C SET THE INTERNAL PARAMETERS
      KM    =  1.D-06
      NTRMAX=  1000
      IW    =  0
                                END IF
      EBEAM =PIN(1,4)
      NCHA  =NC
      NTOTAL=NC+NN
         DO 21 I=1,NTOTAL
         DO 22 J=1,6
         POUT(I,J)=PIN(I,J)
 22      CONTINUE
 21      CONTINUE
                    SUMASS=0.D+00
         DO 31 I=1,NTOTAL
         ICH(I)=INQ(I)
         IF(I.GE.2) SUMASS=SUMASS+POUT(I,5)/EBEAM
 31      CONTINUE
C THE MAXIMAL VALUE OF KMAX IS DEFINED BY THE MASSES
      KMAX =1.D+00-SUMASS**2*0.25D+00
C LOOP OVER THE TO BE PRODUCED PHOTONS
      DO 11111 NGAM=1,NNG
      NTRIAL=0
C RETURN LINE TO PRODUCE UN-WEIGHTED EVENTS
10000 CONTINUE
      NTRIAL=NTRIAL+1
C RESET THE GENERATION IF TOO MANY TRIALS
      IF(NTRIAL.GT.NTRMAX) NTRIAL=1
C (RE)-LOAD THE DATA
      DO 10 I=1,NTOTAL
                       PTOT=0.D+00
          DO 41 J=1,3
          P(I,J)=POUT(I,J)/EBEAM
                       PTOT=PTOT+P(I,J)**2
 41       CONTINUE
          P(I,5)=POUT(I,5)/EBEAM
          P(I,6)=DSQRT(PTOT)
          P(I,4)=DSQRT(PTOT+P(I,5)**2)
 10   CONTINUE
          IF(NTRIAL.EQ.1) THEN
            RADSUM          =0.D+00
            DO 51 I=1,NTOTAL
                 RADLOG(I,3)=0.D+00
                 RADLOG(I,4)=0.D+00
            IF(ICH(I).NE.0) THEN
                 RADLOG(I,3)=2.D+00*DLOG((P(I,4)+P(I,6))/P(I,5))
                 RADLOG(I,4)=RADLOG(I,3)*P(I,4)/P(I,6)
            RADSUM          =RADSUM+RADLOG(I,4)
                            END IF
 51       CONTINUE
      CST=CST0*RADSUM
            RADSUM          =1.D+00/RADSUM
                                           RAD0=0.D+00
            DO 61 I=1,NTOTAL
            RADLOG(I,2)=RADLOG(I,4)*RADSUM
            RADLOG(I,1)=RADLOG(I,4)*RADSUM+RAD0
                                           RAD0=RADLOG(I,1)
 61       CONTINUE
            IF(JDEBUG.EQ.1)     THEN
            PRINT *,'RADLOG-2'
            PRINT *,(RADLOG(I,2),I=1,NTOTAL)
                                END IF
                          END IF
       IF(JDEBUG.EQ.1.AND.NTRIAL.EQ.1) THEN
       PRINT *,' INPUT PARTICLES '
         DO 71 I=1,NTOTAL
         PRINT *,' PARTICLE ',I,' PX-Y-Z [Q= ',ICH(I),' ] '
         PRINT *,(P(I,J),J=1,3)
         PRINT *,'            ','  E-M-P '
         PRINT *,(P(I,J),J=4,6)
 71       CONTINUE
                                       END IF
C **************************************
C GO FOR IT !!!!!!!!!!!!!!!!!!!!!!!!!!!!
C **************************************
      CALL RADCOR
C **************************************
      IF(WCOR.GT.WMXX) WMXX=WCOR
      IF(WMAX*EXCRAN(1).GT.WCOR) GO TO 10000
C LOAD THE MODIFIED 4-VECTORS
           DO 81 J=1,6
          DO 11 I=3,NTOTAL
          POUT(I,J)=P(I,J)*EBEAM
 11       CONTINUE
 81       CONTINUE
      NTOTAL=NTOTAL+1
           DO 91 J=1,4
           POUT(NTOTAL,J)=K(J)*EBEAM
 91       CONTINUE
           POUT(NTOTAL,5)=0.D+00
           POUT(NTOTAL,6)=POUT(NTOTAL,4)
           ICH(NTOTAL)=0
11111 CONTINUE
      RETURN
      END
*
      SUBROUTINE RADCOR
      IMPLICIT REAL*8(A-H,K,O-Z)
      SAVE
C Final state radiation common
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG
      DIMENSION IETA(50)
      DATA IETA/-1,-1,48*1/
C...............................................................
      IF(NTRIAL.EQ.1.OR.IW.EQ.1..OR.IW.EQ.3)          THEN
C----SET THE ZC FACTORS
        DO 21 I=1,NTOTAL
        ZC(I)=ICH(I)*IETA(I)
 21     CONTINUE
C----CALCULATION OF BETA(=B)
      CALL GETBET
C----CHOICE OF IRAD
      SELECT=EXCRAN(2)
        DO 31 I=1,NTOTAL
      IF(SELECT.LT.RADLOG(I,1).AND.ICH(I).NE.0) THEN
                                                IRAD=I
                                                GO TO 111
                                                END IF
 31     CONTINUE
      IRAD=NCHA
 111                                            CONTINUE
C----GENERATION OF K0 : B*K0**(1-B)*(1.-KL+0.5*KL**2) [NORMALIZED]
 112  CONTINUE
      K0=KM
      AR=EXCRAN(3)
C.........................
      IF(AR.GT.KM**B) THEN
C.........................
      K0=DEXP(DLOG(AR)/B)
      IF(K0.GT.KMAX)                            GO TO 112
      KL=K0/P(IRAD,6)
      IF(KL.GT.1.D+00)                          GO TO 112
      WK0=1.D+00-KL+0.5D+00*KL**2
      IF(EXCRAN(4).GT.WK0)                       GO TO 112
C----UPDATE THE ZC FACTORS
        DO 41 I=1,NTOTAL
        IF(ICH(I).NE.0.AND.I.NE.IRAD) THEN
         KL=K0/P(IRAD,6)
         IF(KL.GT.1.D+00) WKL=0.D+00
                          WKL=1.D+00-KL+0.5D+00*KL**2
         ZC(I)=ZC(I)*DSQRT(WKL/WK0)
                                      END IF
 41     CONTINUE
C.........................
                      END IF
C.........................
C...............................................................
                                                          END IF
C...............................................................
C----GENERATION OF PHI
      RN1=EXCRAN(5)
      PHI=RN1*2.D+00*PI
C----GENERATION OF COS(THE)
      RN2=EXCRAN(6)
      V=P(IRAD,6)/P(IRAD,4)
      CTH=(1.D+00-(1.D+00+V)/DEXP(RADLOG(IRAD,3)*RN2))/V
C----GET K-VECTOR IN LABORATORY
      CALL TURN(PHI,CTH,K0)
C----CORRECTION WEIGHT
      CALL GETWEI
      RETURN
      END
*
      SUBROUTINE GETBET
      IMPLICIT REAL*8(A-H,K,O-Z)
      SAVE
C Final state radiation common
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG
      B=0.D+00
      DO 10 I=1  ,NCHA-1
         DO 21 J=I+1,NCHA
         EMIJ=P(I,5)*P(J,5)
         PIPJ=P(I,4)*P(J,4)-P(I,1)*P(J,1)-P(I,2)*P(J,2)-P(I,3)*P(J,3)
         PIPJ=DABS(PIPJ)
         SQIJ=DSQRT(PIPJ**2-EMIJ**2)
         FIJ=PIPJ/SQIJ*DLOG((PIPJ+SQIJ)/EMIJ)-1.D+00
         B=B-FIJ*ZC(I)*ZC(J)
 21      CONTINUE
   10 CONTINUE
      B=B*2.D+00*ALFA*REDUCE
      IF(JDEBUG.EQ.1) PRINT *,' BETA FACTOR = ',B
      RETURN
      END
*
      SUBROUTINE GETWEI
      IMPLICIT REAL*8(A-H,K,O-Z)
      SAVE
C Final state radiation common
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG
         WSSS=0.D+00
         DO 21 I=1,NCHA
         IF(ICH(I).NE.0) THEN
         VL=(K(1)*P(I,1)+K(2)*P(I,2)+K(3)*P(I,3))/K(4)/P(I,4)
         WSSS=WSSS+1.D+00/(1.D+00-VL)
                         END IF
 21      CONTINUE
      CALL MOVE
      CALL GETMAT(ELEM)
      IF (B.EQ.0.0D0) THEN
        WCOR=CST*ELEM/WSSS*REDUCE
      ELSE
        WCOR=CST*ELEM/WSSS/B*REDUCE
      ENDIF
      RETURN
      END
*
      SUBROUTINE GETMAT(ELEM)
      IMPLICIT REAL*8(A-H,K,O-Z)
      SAVE
C Final state radiation common
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG
      ELEM=0.D+00
      DO 20 J=1,3
                SUM=0.D+00
                DO 331 I=1,NCHA
                VL=K(1)*P(I,1)+K(2)*P(I,2)+K(3)*P(I,3)
                VP=P(I,J)*K(4)-VL*K(J)/K(4)
                SUM=SUM+VP/(K(4)*P(I,4)-VL)*ZC(I)
 331            CONTINUE
      ELEM=ELEM+SUM*SUM
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE MOVE
      IMPLICIT REAL*8(A-H,K,O-Z)
      SAVE
C Final state radiation common
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG
      COMMON/LOOK/PRO,EGAM,K0,PART(50),TR(4)
      DIMENSION PIN(4),POUT(4)
      DIMENSION PTOT(4)
C THE PURPOSE OF THIS ROUTINE IS TO ENSURE ENERGY-MOMENTUM CONSERVATION
C WHICH WAS EXPLICITELY VIOLATED ABOVE BY THE ADJONCTION OF A PHOTON TO
C THE FINAL STATE.
C THE PROCEDURE TO RESTORE (EXACTLY) P_IN=P_OUT IS AS FOLLOWS.
C  0) REMOVE THE PHOTON FROM THE RADIATING PARTICLES (IW=2 OR 3)
C                3_P(I)=3_P(I)-3_P(PHOTON)*PART(I)
C     IN MOST CASES THIS STEP ALREADY ALMOST ENSURES  P_INI=P_FIN
C  1) RESCALE THE FINAL STATE MOMENTA TO SATISFY :
C              P(FINAL STATE)**2=S*(1-K0)
C     IN DOING SO, THE MASSES OF THE PARTICLES ARE ALSO SCALED DOWN.
C  2) APPLY A BOOST TO THE FINAL STATE TO SATISFY :
C            3_P(FINAL STATE)   = - 3_P(PHOTON)
C  3) MODIFY THE ENERGIES OF THE FINAL STATE PARTICLES TO SATISFY :
C            MASSE(PARTICLE)**2 = E(PARTICLE)**2- 3_P(PARTICLE)**2
C  4) RESCALE THE PHOTON 4-MOMENTA (K0 ->- EGAM) TO RECOVER :
C              P(FINAL STATE)**2=S*(1-EGAM)
C  5) RE-APPLY A BOOST TO THE FINAL STATE TO RECOVER :
C            3_P(FINAL STATE) + 3_P(PHOTON) =0.
C----
C[0]-
C----
      IF(IW.EQ.0.OR.IW.EQ.1)
C
     *THEN
C
C  REMOVE THE PHOTON FROM THE RADIATING PARTICLES
C GET THE RELATIVE RADIATION PROBABILITIES
      PARTOT=0.D+00
      DO 31 I=1,NTOTAL
       COSTET=0.D+00
         DO 21 J=1,3
         COSTET=COSTET+P(I,J)*K(J)
 21      CONTINUE
C COSTET(PARTICLE-PHOTON) AFTER FULL SUBTRACTION : [(P-K).K/(P-K)*K]
       COSTET=(COSTET-K(4)**2)
     *       /(DSQRT(P(I,6)**2+K(4)**2-2.D+00*COSTET)*K(4))
       VELOCI=P(I,6)/P(I,4)
       PART(I)=VELOCI**2*(1.D+00-COSTET**2)/(1.D+00-VELOCI*COSTET)**2
     *        *ZC(I)**2
       PARTOT=PARTOT+PART(I)
 31    CONTINUE
C REMOVE THE PHOTON BY PART FROM THE FINAL STATE ONLY
      DO 51 I=3,NTOTAL
      P(I,6)=0.D+00
      PART(I)=PART(I)/PARTOT
        DO 41 J=1,3
        P(I,J)=P(I,J)-K(J)*PART(I)
        P(I,6)=P(I,6)+P(I,J)**2
 41     CONTINUE
      P(I,4)=DSQRT(P(I,6)+P(I,5)**2)
      P(I,6)=DSQRT(P(I,6))
 51   CONTINUE
C GET THE FINAL STATE INVARIANT MASS
      DO 71 J=1,4
      PTOT(J)=0.D+00
         DO 61 I=3,NTOTAL
         PTOT(J)=PTOT(J)+P(I,J)
 61      CONTINUE
 71   CONTINUE
      WTOT=PTOT(4)**2-PTOT(1)**2-PTOT(2)**2-PTOT(3)**2
      PRO=DSQRT(4.D+00*(1.D+00-K(4))/WTOT)
C
      ELSE
C
      PRO=DSQRT(1.D+00-K(4))
C
      END IF
C
C----
C[1]-
C----
C  RESCALE THE FINAL STATE MOMENTA
      DO 91 I=3,NTOTAL
         DO 81 J=1,4
         P(I,J)=P(I,J)*PRO
 81      CONTINUE
 91   CONTINUE
C----
C[2]-
C----
C  APPLY A BOOST TO THE FINAL STATE
      IF(IW.EQ.0.OR.IW.EQ.1)
C
     *THEN
C
C  BRING THE FINAL STATE TO ITS C.M.S
      TR2  = 0.D+00
       DO 121 J=1,3
       TR(J)=-PTOT(J)/PTOT(4)
       TR2  =TR2+TR(J)**2
 121   CONTINUE
      TR(4)=1.D+00/DSQRT(1.D+00-TR2)
      DO 151 I=3,NTOTAL
         DO 131 J=1,4
         PIN(J)=P(I,J)
 131     CONTINUE
      CALL LORENZ(TR,PIN,POUT)
         DO 141 J=1,4
         P(I,J)=POUT(J)
 141     CONTINUE
 151  CONTINUE
C
      END IF
C
      TR(4)= 2.D+00-K(4)
      TR2  = 0.D+00
      DO 161 J=1,3
      TR(J)=-K(J)/TR(4)
      TR2  =TR2+TR(J)**2
 161  CONTINUE
      TR(4)=1.D+00/DSQRT(1.D+00-TR2)
      DO 191 I=3,NTOTAL
         DO 171 J=1,4
         PIN(J)=P(I,J)
 171     CONTINUE
      CALL LORENZ(TR,PIN,POUT)
         DO 181 J=1,3
         P(I,J)=POUT(J)
 181     CONTINUE
 191  CONTINUE
C----
C[3]-
C----
C  MODIFY THE ENERGIES OF THE FINAL STATE PARTICLES
      DO 231 I=3,NTOTAL
      P(I,6)=0.D+00
         DO 221 J=1,3
         P(I,6)=P(I,6)+P(I,J)**2
 221     CONTINUE
      P(I,4)=DSQRT(P(I,6)+P(I,5)**2)
      P(I,6)=DSQRT(P(I,6))
 231  CONTINUE
C----
C[4]-
C----
C  RESCALE THE PHOTON 4-MOMENTA
      DO 251 J=1,4
         PTOT(J)=0.D+00
         DO 241 I=3,NTOTAL
         PTOT(J)=PTOT(J)+P(I,J)
 241     CONTINUE
 251  CONTINUE
      WTOT=PTOT(4)**2-PTOT(1)**2-PTOT(2)**2-PTOT(3)**2
      EGAM=1.D+00-WTOT*.25D+00
      IF(EGAM.LT.KM) EGAM=KM
         K0=K(4)
         DO 261 J=1,4
         K(J)=K(J)*EGAM/K(4)
 261     CONTINUE
C----
C[5]-
C----
      E2=(2.D+00-K(4))**2
      BETA=(PTOT(4)*K0-EGAM*(2.D+00-EGAM))/(E2+K0**2)
      TR(4)=1.D+00/DSQRT(1.D+00-BETA**2)
      DO 271 J=1,3
      TR(J)=K(J)/K(4)*BETA
 271  CONTINUE
      DO 321 I=3,NTOTAL
         DO  281 J=1,4
         PIN(J)=P(I,J)
 281     CONTINUE
      CALL LORENZ(TR,PIN,POUT)
         DO  291 J=1,4
         P(I,J)=POUT(J)
 291     CONTINUE
 321  CONTINUE
      RETURN
      END
*
      SUBROUTINE TECPRO(A,B,C)
      IMPLICIT REAL*8(A-H,K,O-Z)
      DIMENSION A(3),B(3),C(3)
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      CNORM=0.D+00
         DO 351 J=1,3
         CNORM=CNORM+C(J)**2
 351     CONTINUE
      CNORM=1.D+00/DSQRT(CNORM)
         DO 361 J=1,3
         C(J)=C(J)*CNORM
 361     CONTINUE
      RETURN
      END
*
      SUBROUTINE TURN(PHI,CTH,K0)
      IMPLICIT REAL*8(A-H,K,O-Z)
      SAVE
C Final state radiation common
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG
      DIMENSION X(3),Y(3),Z(3),R(3)
      DATA R/1.D0,1.D0,0.D0/
C----ROTATION TO RETURN TO THE LABORATORY
      STH=DSQRT(1.D+00-CTH**2)
      CPH=DCOS(PHI)
      SPH=DSIN(PHI)
        DO 21 J=1,3
        Z(J)=P(IRAD,J)
 21     CONTINUE
      CALL TECPRO(Z,R,X)
      CALL TECPRO(Z,X,Y)
        REN=1.D+00/P(IRAD,6)
        DO 31 J=1,3
        K(J)=(X(J)*STH*CPH+Y(J)*STH*SPH+Z(J)*CTH*REN)*K0
 31     CONTINUE
      K(4)=K0
      IF(JDEBUG.EQ.1) THEN
      PRINT *,' PHOTON : KX-Y-Z-E ',K
      END IF
      RETURN
      END
*
      SUBROUTINE LORENZ(X,A,B)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(4),A(4),B(4)
      XA=X(1)*A(1)+X(2)*A(2)+X(3)*A(3)
        DO 341 I=1,3
        B(I)=A(I)+X(I)*X(4)*(X(4)/(X(4)+1.D+00)*XA+A(4))
 341    CONTINUE
      B(4)=X(4)*(A(4)+XA)
      RETURN
      END
