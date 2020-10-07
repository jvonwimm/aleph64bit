      PARAMETER (JULVD=1,JULIT=2,JULTP=3,JULEC=4,JULLC=5)
      PARAMETER (JULSA=6,JULHC=7,JULMU=8,JULCA=9,JULSK=10)
      PARAMETER (JULYR=11,JULYT=12,JULEF=13,JULBC=14,JULFA=15
     +           ,JULBO=16,JULSI=17)
      PARAMETER (MODEND=0,MODREA=1,MODPRV=2,MODPRI=3,MODPRT=4)
      PARAMETER (MODPRE=5,MODPRL=6,MODPRS=7,MODPRH=8,MODPRM=9)
      PARAMETER (MODTPT=10,MODTPW=11,MODDEX=12,MODITC=13 ,MODVDE=14)
      PARAMETER (MODFIT=15,MODFTR=16,MODECL=17,MODMIP=18,MODEID=19)
      PARAMETER (MODFEO=20,MODFEP=21,MODHCL=22,MODHDP=23,MODEHG=24)
      PARAMETER (MODMUF=25,MODMUA=26,MODLCA=27,MODSAT=28,MODLTR=29)
      PARAMETER (MODPID=30,MODYMA=31,MODYTO=32,MODYV0=33,MODHIS=34)
      PARAMETER (MODPOT=35,MODBSP=36,MODWRI=37)
      PARAMETER (MODCOM=38,MODBCA=39,MODMUO=40,MODMON=41,MODENF=42)
      PARAMETER (MODGAM=43,MODEDI=44,MODGMX=45)
      PARAMETER (MODSUM=46,MODBOM=47)
      PARAMETER (MODPSI=48,MODSIC=49,MODGMP=50)
      PARAMETER (LASTMO=50)
      CHARACTER*33 RNTXTM  (LASTMO)
      CHARACTER*8  RNTXTS  (LASTMO)
      INTEGER RNCALL (LASTMO)
      REAL    RNTIME (LASTMO)
C ----> Title of Modules
      DATA RNTXTM(1 )/'Reading records..................'/
      DATA RNTXTM(2 )/'Prepare data VDET................'/
      DATA RNTXTM(3 )/'Prepare data ITC.................'/
      DATA RNTXTM(4 )/'Prepare data TPC.................'/
      DATA RNTXTM(5 )/'Prepare data ECAL................'/
      DATA RNTXTM(6 )/'Prepare data LCAL................'/
      DATA RNTXTM(7 )/'Prepare data SATR................'/
      DATA RNTXTM(8 )/'Prepare data HCAL................'/
      DATA RNTXTM(9 )/'Prepare data MUON................'/
      DATA RNTXTM(10)/'TPC track finding................'/
      DATA RNTXTM(11)/'TPC wire association.............'/
      DATA RNTXTM(12)/'dE/dx calculation................'/
      DATA RNTXTM(13)/'ITC reconstruction...............'/
      DATA RNTXTM(14)/'VDET reconstruction..............'/
      DATA RNTXTM(15)/'Global fit VDET,ITC,TPC..........'/
      DATA RNTXTM(16)/'Extrapolate track ECAL,Coil,Hcal.'/
      DATA RNTXTM(17)/'Find ECAL clusters...............'/
      DATA RNTXTM(18)/'Minimum ionizing in ECAL.........'/
      DATA RNTXTM(19)/'Electron identification..........'/
      DATA RNTXTM(20)/'ECAL cluster analysis............'/
      DATA RNTXTM(21)/'ECAL statistics..................'/
      DATA RNTXTM(22)/'Find HCAL clusters...............'/
      DATA RNTXTM(23)/'HCAL digital pattern analysis....'/
      DATA RNTXTM(24)/'ECAL - HCAL global analysis......'/
      DATA RNTXTM(25)/'HCAL muon finding................'/
      DATA RNTXTM(26)/'MUON-hit - track association.....'/
      DATA RNTXTM(27)/'LCAL reconstruction..............'/
      DATA RNTXTM(28)/'SATR reconstruction..............'/
      DATA RNTXTM(29)/'Luminosity track-cluster assoc...'/
      DATA RNTXTM(30)/'Particle identification summary..'/
      DATA RNTXTM(31)/'Main vertex finding (Mermikides).'/
      DATA RNTXTM(32)/'Topology reconstruction..........'/
      DATA RNTXTM(33)/'V0 reconstruction................'/
      DATA RNTXTM(34)/'Histogramming....................'/
      DATA RNTXTM(35)/'Build pot banks..................'/
      DATA RNTXTM(36)/'Beam Spot Position...............'/
      DATA RNTXTM(37)/'Write event......................'/
      DATA RNTXTM(38)/'Compress data....................'/
      DATA RNTXTM(39)/'BCAL reconstruction..............'/
      DATA RNTXTM(40)/'MUON HCAL + Chambers association.'/
      DATA RNTXTM(41)/'Monitoring.......................'/
      DATA RNTXTM(42)/'Energy Flow......................'/
      DATA RNTXTM(43)/'ECAL Photon identification.......'/
      DATA RNTXTM(44)/'EDIR Class Word..................'/
      DATA RNTXTM(45)/'Gammas in charged clusters.......'/
      DATA RNTXTM(46)/'Summary File.....................'/
      DATA RNTXTM(47)/'BOM Calculate Beam Parameters....'/
      DATA RNTXTM(48)/'SICAL prepare data...............'/
      DATA RNTXTM(49)/'SICAL reconstruction.............'/
      DATA RNTXTM(50)/'GAMPEC + mod PECO + BULOS........'/
C ----> Name of Moduls
      DATA RNTXTS(1 )/'RLOOPR'/
      DATA RNTXTS(2 )/'VPREDA'/
      DATA RNTXTS(3 )/'IPREDA'/
      DATA RNTXTS(4 )/'TPREDA'/
      DATA RNTXTS(5 )/'EPREDA'/
      DATA RNTXTS(6 )/'LPREDA'/
      DATA RNTXTS(7 )/'SPREDA'/
      DATA RNTXTS(8 )/'HPREDA'/
      DATA RNTXTS(9 )/'MPREDA'/
      DATA RNTXTS(10)/'TPCREC'/
      DATA RNTXTS(11)/'TRKWIR'/
      DATA RNTXTS(12)/'TRKELS'/
      DATA RNTXTS(13)/'ITCREC'/
      DATA RNTXTS(14)/'VDLINK'/
      DATA RNTXTS(15)/'FITALL'/
      DATA RNTXTS(16)/'FTRACK'/
      DATA RNTXTS(17)/'ECFCLU'/
      DATA RNTXTS(18)/'ECRMIP'/
      DATA RNTXTS(19)/'ELECID'/
      DATA RNTXTS(20)/'ECFOBJ'/
      DATA RNTXTS(21)/'ECPRIN'/
      DATA RNTXTS(22)/'HCFCLU'/
      DATA RNTXTS(23)/'HDPREC'/
      DATA RNTXTS(24)/'ECHCGL'/
      DATA RNTXTS(25)/'HMFIND'/
      DATA RNTXTS(26)/'MUASS '/
      DATA RNTXTS(27)/'LCAREC'/
      DATA RNTXTS(28)/'SATREC'/
      DATA RNTXTS(29)/'LTRACK'/
      DATA RNTXTS(30)/'FPIDEN'/
      DATA RNTXTS(31)/'YFMAIN'/
      DATA RNTXTS(32)/'YTOPOL'/
      DATA RNTXTS(33)/'YMFV0S'/
      DATA RNTXTS(34)/'RHSEVT'/
      DATA RNTXTS(35)/'RDEFPO'/
      DATA RNTXTS(36)/'VBSPOT'/
      DATA RNTXTS(37)/'RWREVT'/
      DATA RNTXTS(38)/'CMPLIS'/
      DATA RNTXTS(39)/'BPREDA'/
      DATA RNTXTS(40)/'MUIDO '/
      DATA RNTXTS(41)/'RMONIT'/
      DATA RNTXTS(42)/'EMSKEV'/
      DATA RNTXTS(43)/'EBEGID'/
      DATA RNTXTS(44)/'ALCLASW'/
      DATA RNTXTS(45)/'ECGMIX'/
      DATA RNTXTS(46)/'RCJSUM'/
      DATA RNTXTS(47)/'OMBREC'/
      DATA RNTXTS(48)/'SIPREDA'/
      DATA RNTXTS(49)/'SICREC'/
      DATA RNTXTS(50)/'GASTEER'/
#if defined(DOC)
C
C
C-------> PARAMETERS GLOBAL OVER PROGRAM
C
C SKELETON Parameters
C -------------------
C JULVD   =  1, VDET    identifier
C JULIT   =  2, ITC     identifier
C JULTP   =  3, TPC     identifier
C JULEC   =  4, ECAL    identifier
C JULLC   =  5, LCAL    identifier
C JULSA   =  6, SATR    identifier
C JULHC   =  7, HCAL    identifier
C JULMU   =  8, MUON    identifier
C JULCA   =  9, CALO    identifier
C JULSK   = 10, SKEL    identifier
C JULYR   = 11, YREC    identifier
C JULYT   = 12, YTOP    identifier
C JULEF   = 13, EFLO    identifier
C JULBC   = 14, BCAL    identifier
C JULFA   = 15, FALCON  identifier
C JULBO   = 16, BOM     identifier
C JULSI   = 17, SICAL   identifier
C
C MODULE identifiers USED IN RNXMOD
C ---------------------------------
C MODEND              = 0   NOMODULE -used to close a module
C LASTMO              = 50  Largest module number
C RNTXTM  (LASTMO)    = Text describing the Modules
C RNTXTS  (LASTMO)    = Subroutine name of the Module
C RNCALL  (LASTMO)    = Number of call to the Module
C RNTIME  (LASTMO)    = Time spent in the Module
C
#endif
