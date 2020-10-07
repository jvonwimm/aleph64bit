      SUBROUTINE SUSYGDAT                                               SUSYGDA2
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**SUSYGDA3
*                                                                      *SUSYGDA4
*-- Author :    Stavros Katsanevas   14/04/95                          *SUSYGDA5
*                                                                      *SUSYGDA6
*-- modified by Y.GAO  1/02/96                                         *SUSYGDA7
*                                                                      *SUSYGDA8
*-- change it from BLOCK DATA to SUBROUTINE  Y.GAO 6/03/96             *SUSYGDA9
*                                                                      *SUSYGD10
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**SUSYGD11
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               SUSYGD12
C                                                                       SUSYGD13
C     common blocks                                                     SUSYGD14
C     =============                                                     SUSYGD15
*     reorder sparticles and particles                                  SUSYGD16
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         SUSYGD17
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   SUSYGD18
     +FLC(12),FRC(12),gms(12),echar(12)                                 SUSYGD19
       common/spartcl/fmal(12),fmar(12),ratq(12),fgamc(12),fgamcr(12)   SUSYGD20
     +,cosmi(12)                                                        SUSYGD21
       COMMON/SFLOOP/ILOOP                                              SUSYGD22
                                                                        SUSYGD23
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
      COMMON /BCS/   IW(LBCS )                                          BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
      Integer       IUT,i,j                                             SUSYGD25
      Integer       mproc                                               SUPROC 2
      Parameter    (mproc=35)                                           SUPROC 3
      Integer       iproc,iprod,ipar1,ipar2,ihand,isusy,iopen,ievnt     SUPROC 4
      Real          xproc                                               SUPROC 5
      common/SUPROC/iproc(mproc),iprod(mproc)                           SUPROC 6
     &             ,ipar1(mproc),ipar2(mproc)                           SUPROC 7
     &             ,ihand(mproc),isusy(mproc)                           SUPROC 8
     &             ,iopen(mproc),ievnt(mproc)                           SUPROC 9
     &             ,xproc(mproc)                                        SUPROC10
C                                                                       SUSYGD27
C     data statements are only for local variables                      SUSYGD28
C     ============================================                      SUSYGD29
      Integer       kl_loc(2,18)                                        SUSYGD30
      data kl_loc/2,-2,1,-1,12,-12,11,-11,2,-1,12,-11,                  SUSYGD31
     +            4,-4,3,-3,14,-14,13,-13,4,-3,14,-13,                  SUSYGD32
     +            6,-6,5,-5,16,-16,15,-15,6,-5,16,-15/                  SUSYGD33
                                                                        SUSYGD34
      Integer       ispa_loc(12,2)                                      SUSYGD35
      data ispa_loc/42,41,52,51,44,43,54,53,46,45,56,55,                SUSYGD36
     +              48,47, 0,57,50,49, 0,58,62,61, 0,59/                SUSYGD37
                                                                        SUSYGD38
      Integer       klap_loc(2,18)                                      SUSYGD39
      data klap_loc/42,42,41,41,52,52,51,51,42,41,52,51,                SUSYGD40
     +              44,44,43,43,54,54,53,53,44,43,54,53,                SUSYGD41
     +              46,46,45,45,56,56,55,55,46,45,56,55/                SUSYGD42
                                                                        SUSYGD43
      Integer       idecs_loc(12,2)                                     SUSYGD44
      data idecs_loc/2,1,12,11,4,3,14,13,6,5,16,15,                     SUSYGD45
     +               1,2,11,12,3,4,13,14,5,6,15,16/                     SUSYGD46
                                                                        SUSYGD47
      Integer       iloop_loc                                           SUSYGD48
      data iloop_loc/1/                                                 SUSYGD49
                                                                        SUSYGD50
      Integer       jproc                                               SUSYGD51
      Integer       iproc_loc(mproc),iprod_loc(mproc)                   SUSYGD52
     &             ,ipar1_loc(mproc),ipar2_loc(mproc)                   SUSYGD53
     &             ,ihand_loc(mproc),isusy_loc(mproc)                   SUSYGD54
      DATA ( iproc_loc(jproc),iprod_loc(jproc)                          SUSYGD55
     &      ,ipar1_loc(jproc),ipar2_loc(jproc)                          SUSYGD56
     &      ,ihand_loc(jproc),isusy_loc(jproc),jproc=1,mproc ) /        SUSYGD57
*    -------------------------------                                    SUSYGD58
*    spar1,spar2 here are LUND codes                                    SUSYGD59
*    -------------------------------                                    SUSYGD60
     &   1,  1,  71,  71,   0,   0,                                     SUSYGD61
     &   2,  1,  72,  71,   0,   0,                                     SUSYGD62
     &   3,  1,  72,  72,   0,   0,                                     SUSYGD63
     &   4,  1,  73,  71,   0,   0,                                     SUSYGD64
     &   5,  1,  73,  72,   0,   0,                                     SUSYGD65
     &   6,  1,  73,  73,   0,   0,                                     SUSYGD66
     &   7,  1,  74,  71,   0,   0,                                     SUSYGD67
     &   8,  1,  74,  72,   0,   0,                                     SUSYGD68
     &   9,  1,  74,  73,   0,   0,                                     SUSYGD69
     &  10,  1,  74,  74,   0,   0,                                     SUSYGD70
     &  11,  2,  75, -75,   0,   0,                                     SUSYGD71
     &  12,  2,  76, -75,   0,   0,                                     SUSYGD72
     &  13,  2,  76, -76,   0,   0,                                     SUSYGD73
     &  14,  3,  52, -52,   1,   3,                                     SUSYGD74
     &  15,  3,  54, -54,   1,   7,                                     SUSYGD75
     &  16,  3,  56, -56,   1,  11,                                     SUSYGD76
     &  17,  3,  51, -51,   1,   4,                                     SUSYGD77
     &  18,  3,  51, -57,   1,   4,                                     SUSYGD78
     &  19,  3,  57, -57,   2,   4,                                     SUSYGD79
     &  20,  3,  53, -53,   1,   8,                                     SUSYGD80
     &  21,  3,  58, -58,   2,   8,                                     SUSYGD81
     &  22,  3,  55, -55,   1,  12,                                     SUSYGD82
     &  23,  3,  59, -59,   2,  12,                                     SUSYGD83
     &  24,  3,  42, -42,   1,   1,                                     SUSYGD84
     &  25,  3,  48, -48,   2,   1,                                     SUSYGD85
     &  26,  3,  41, -41,   1,   2,                                     SUSYGD86
     &  27,  3,  47, -47,   2,   2,                                     SUSYGD87
     &  28,  3,  44, -44,   1,   5,                                     SUSYGD88
     &  29,  3,  50, -50,   2,   5,                                     SUSYGD89
     &  30,  3,  43, -43,   1,   6,                                     SUSYGD90
     &  31,  3,  49, -49,   2,   6,                                     SUSYGD91
     &  32,  3,  45, -45,   1,   9,                                     SUSYGD92
     &  33,  3,  61, -61,   2,   9,                                     SUSYGD93
     &  34,  3,  46, -46,   1,  10,                                     SUSYGD94
     &  35,  3,  62, -62,   2,  10/                                     SUSYGD95
C                                                                       SUSYGD96
C     transfer the value of local variables to global, painful...       SUSYGD97
C     ===========================================================       SUSYGD98
      do i=1,18                                                         SUSYGD99
      do j=1, 2                                                         SUSYG100
        kl(j,i)  =kl_loc(j,i)                                           SUSYG101
        klap(j,i)=klap_loc(j,i)                                         SUSYG102
      enddo                                                             SUSYG103
      enddo                                                             SUSYG104
                                                                        SUSYG105
      do i=1, 2                                                         SUSYG106
      do j=1,12                                                         SUSYG107
        ispa(j,i) =ispa_loc(j,i)                                        SUSYG108
        idecs(j,i)=idecs_loc(j,i)                                       SUSYG109
      enddo                                                             SUSYG110
      enddo                                                             SUSYG111
                                                                        SUSYG112
      iloop=iloop_loc                                                   SUSYG113
                                                                        SUSYG114
      do j=1,mproc                                                      SUSYG115
        iproc(j)=iproc_loc(j)                                           SUSYG116
        iprod(j)=iprod_loc(j)                                           SUSYG117
        ipar1(j)=ipar1_loc(j)                                           SUSYG118
        ipar2(j)=ipar2_loc(j)                                           SUSYG119
        ihand(j)=ihand_loc(j)                                           SUSYG120
        isusy(j)=isusy_loc(j)                                           SUSYG121
      enddo                                                             SUSYG122
C                                                                       SUSYG123
C     write a message                                                   SUSYG124
C     ===============                                                   SUSYG125
      IUT=IW(6)                                                         SUSYG126
      write(IUT,*) ' SUSYGDAT has been called '                         SUSYG127
      end                                                               SUSYG128
                                                                        SUSYG129
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECSS,WEIT)                 ASKUSE 2
C ------------------------------------------------------------          ASKUSE 3
C -  B. Bloch  - March 1994                                             ASKUSE 4
C! get an event from LUND 7.4                                           ASKUSE 5
C  then transfer the information into KINE and VERT banks.              ASKUSE 6
C -  Y. Gao    - Jan   1996                                             ASKUSE 7
C! modified to be used to get an event from SUSYGEN & JETSET74          ASKUSE 8
C  the argument of ECMS changed to ECSS                                 ASKUSE 9
C                                                                       ASKUSE10
C                                                                       ASKUSE11
C     structure : subroutine                                            ASKUSE12
C     output arguments :                                                ASKUSE13
C          IDPR   : process identification,each digit corresponds to    ASKUSE14
C          the flavor of the evnt ( several flavors /event is possible) ASKUSE15
C          ISTA   : status flag ( 0 means ok), use it to reject         ASKUSE16
C                   unwanted events                                     ASKUSE17
C          NTRK   : number of tracks generated and kept                 ASKUSE18
C                  (i.e. # KINE banks  written)                         ASKUSE19
C          NVRT   : number of vertices generated                        ASKUSE20
C                   (i.e. # VERT banks written)                         ASKUSE21
C          ECMS   : center of mass energy for the event (may be         ASKUSE22
C                   different from nominal cms energy)                  ASKUSE23
C          WEIT   : event weight ( not 1 if a weighting method is used) ASKUSE24
C -----------------------------------------------------------------     ASKUSE25
      Implicit None                                                     ASKUSE26
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
      COMMON /BCS/   IW(LBCS )                                          BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
      INTEGER    L1MST,L1PAR                                            LUNDCOM2
     &          ,L2PAR,L2PARF                                           LUNDCOM3
     &          ,LJNPAR                                                 LUNDCOM4
      PARAMETER (L1MST=200, L1PAR=200)                                  LUNDCOM5
      PARAMETER (L2PAR=500, L2PARF=2000)                                LUNDCOM6
      PARAMETER (LJNPAR=4000)                                           LUNDCOM7
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUNDCOM8
      INTEGER         MSTU,MSTJ                                         LUNDCOM9
      REAL            PARU,PARJ                                         LUNDCO10
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUNDCO11
      INTEGER         KCHG                                              LUNDCO12
      REAL            PMAS,PARF,VCKM                                    LUNDCO13
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCO14
     &                KFDP(L2PARF,5)                                    LUNDCO15
      INTEGER         MDCY,MDME,KFDP                                    LUNDCO16
      REAL            BRAT                                              LUNDCO17
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCO18
      CHARACTER*8     CHAF                                              LUNDCO19
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUNDCO20
      INTEGER         N7LU,K7LU                                         LUNDCO21
      REAL            P7LU,V7LU                                         LUNDCO22
********************* start of commons of SUSYGEN **********************SUSCOM 2
      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rfmsq,rfmstopl,rfmstopr, SUSCOM 3
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)        SUSCOM 4
      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,                    SUSCOM 5
     +        rfmsq,rfmstopl,rfmstopr,                                  SUSCOM 6
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan           SUSCOM 7
                                                                        SUSCOM 8
      real*8  gmaum,gmaur,gm0,gtanb,gatri                               SUSCOM 9
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM10
      common/steer/gmaum,gmaur,gm0,gtanb,gatri                          SUSCOM11
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM12
                                                                        SUSCOM13
      Integer modes,MIX                                                 SUSCOM14
      common/mds/modes,MIX                                              SUSCOM15
                                                                        SUSCOM16
      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota             SUSCOM17
      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota       SUSCOM18
                                                                        SUSCOM19
      logical wrt,scan,lepi                                             SUSCOM20
      common/str/wrt,scan,lepi                                          SUSCOM21
                                                                        SUSCOM22
      real*8  flum,ecm,s,roots,T,Q,Q2,EN                                SUSCOM23
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        SUSCOM24
                                                                        SUSCOM25
      real*8  QK                                                        SUSCOM26
      COMMON/ISR/ QK(4)                                                 SUSCOM27
                                                                        SUSCOM28
      real erad,srad                                                    SUSCOM29
      common/srada/erad(100),srad(100)                                  SUSCOM30
                                                                        SUSCOM31
      integer  idbg,igener,irad                                         SUSCOM32
      COMMON /CONST/ idbg,igener,irad                                   SUSCOM33
                                                                        SUSCOM34
      integer  index,index1,index2,nevt                                 SUSCOM35
      COMMON/INDEXX/index,index1,index2,nevt                            SUSCOM36
                                                                        SUSCOM37
      real*8   fmpr1,fmpr2,XCROST,APRO                                  SUSCOM38
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO                             SUSCOM39
                                                                        SUSCOM40
      real*8   cosphimix,facqcd,fgama,spartmas,ratq                     SUSCOM41
      common/mixings/cosphimix,facqcd,fgama,spartmas,ratq               SUSCOM42
                                                                        SUSCOM43
      real*8   FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,    SUSCOM44
     +         FLC,FRC,gms,echar                                        SUSCOM45
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   SUSCOM46
     +FLC(12),FRC(12),gms(12),echar(12)                                 SUSCOM47
                                                                        SUSCOM48
      real*8      TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM49
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM50
                                                                        SUSCOM51
      real*8   fmal,fmar,ratqa,fgamc,fgamcr,cosmi                       SUSCOM52
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)   SUSCOM53
     +,cosmi(12)                                                        SUSCOM54
                                                                        SUSCOM55
      real*4 phimix,stop1                                               SUSCOM56
      common/stopmix/phimix,stop1                                       SUSCOM57
                                                                        SUSCOM58
      real*8   xgaug,xeta                                               SUSCOM59
      COMMON/XCROS/xgaug(8),xeta(8)                                     SUSCOM60
                                                                        SUSCOM61
      integer  ispa,kl,klap,idecs                                       SUSCOM62
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         SUSCOM63
                                                                        SUSCOM64
      Integer  idecsel                                                  SUSCOM65
      common/decsel/idecsel(18)                                         SUSCOM66
                                                                        SUSCOM67
      real*8   brsum,brsuma                                             SUSCOM68
      common/brsum/ brsum(5,6,6),brsuma(6)                              SUSCOM69
                                                                        SUSCOM70
      real*8   fms,fmi,fmk,fml1,fml2,etai,etak,brspa                    SUSCOM71
     +        ,brgaug,fmelt,fmert,fmelu,fmeru                           SUSCOM72
      Integer  lind                                                     SUSCOM73
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     SUSCOM74
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                SUSCOM75
                                                                        SUSCOM76
      Real*4   zr,was,esa                                               SUSCOM77
      Real*8   VOIJL,VOIJR,gfir,gfil                                    SUSCOM78
      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),                              SUSCOM79
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)                         SUSCOM80
                                                                        SUSCOM81
      real*8   OIJL,OIJR,V,U,FM,ETA                                     SUSCOM82
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)     SUSCOM83
                                                                        SUSCOM84
      real*8   oijlp,oijrp                                              SUSCOM85
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)                               SUSCOM86
                                                                        SUSCOM87
      Integer       ILOOP                                               SUSCOM88
      COMMON/SFLOOP/ILOOP                                               SUSCOM89
                                                                        SUSCOM90
      Real*8   PV                                                       SUSCOM91
      Integer  IFLAV,NFLAV                                              SUSCOM92
      COMMON /PARTC / PV(5,20),IFLAV(20,2),NFLAV                        SUSCOM93
                                                                        SUSCOM94
      real*8   gw,widfl,widfr,fmeltw,fmertw,fmeluw,fmeruw,gent,gentl    SUSCOM95
      integer  linda                                                    SUSCOM96
      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw, SUSCOM97
     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)                       SUSCOM98
********************* end  of commons of SUSYGEN ***********************SUSCOM99
      Integer       IPRI,IHST                                           SUWGHT 2
      Real          ECMS,SVERT                                          SUWGHT 3
      Integer       mact                                                SUWGHT 4
      Parameter    (mact=100)                                           SUWGHT 5
      Integer       nact,iact,icoulu                                    SUWGHT 6
      Real          sact,wact                                           SUWGHT 7
      common/SUWGHT/IPRI,ECMS,SVERT(3),icoulu(10)                       SUWGHT 8
     &             ,nact,iact(mact)                                     SUWGHT 9
     &                  ,sact(mact)                                     SUWGHT10
     &                  ,wact(mact)                                     SUWGHT11
      Integer       IDPR,ISTA,NTRK,NVRT                                 ASKUSE31
      Real          ECSS,WEIT                                           ASKUSE32
                                                                        ASKUSE33
      Real          RTEST,IDUMM                                         ASKUSE34
      Real          RX,RY,RZ,RE                                         ASKUSE35
      Real          VRTX(4)                                             ASKUSE36
                                                                        ASKUSE37
      Integer       i                                                   ASKUSE38
      Integer       IG,JG                                               ASKUSE39
      Real          XC                                                  ASKUSE40
                                                                        ASKUSE41
      Integer       IFI                                                 ASKUSE42
      Data          IFI/0/                                              ASKUSE43
      Save          IFI                                                 ASKUSE44
                                                                        ASKUSE45
      Real          RNDM                                                ASKUSE46
      External      RNDM                                                ASKUSE47
C ------------------------------------------------------------------    ASKUSE48
C                                                                       ASKUSE49
      ISTA = 0                                                          ASKUSE50
C                                                                       ASKUSE51
C     Reset entries in /LUJETS/                                         ASKUSE52
C     =========================================                         ASKUSE53
      N7LU = 0                                                          ASKUSE54
C                                                                       ASKUSE55
C     Reset fragmentation storage in common                             ASKUSE56
C     =====================================                             ASKUSE57
      MSTU(90) = 0                                                      ASKUSE58
C                                                                       ASKUSE59
C     get the process number                                            ASKUSE60
C     ======================                                            ASKUSE61
10    continue                                                          ASKUSE62
      RTEST=RNDM(IDUMM)                                                 ASKUSE63
      i=1                                                               ASKUSE64
      do while(RTEST.gt.wact(i).and.i.lt.nact)                          ASKUSE65
        i=i+1                                                           ASKUSE66
      enddo                                                             ASKUSE67
      IDPR=iact(i)                                                      ASKUSE68
C                                                                       ASKUSE69
C     get one event from SUSYGEN                                        ASKUSE70
C     ==========================                                        ASKUSE71
      IG=1                                                              ASKUSE72
      call SUMSSM(IDPR,IG,JG,XC)                                        ASKUSE73
      if( JG.eq.0 ) goto 10                                             ASKUSE74
                                                                        ASKUSE75
      ECSS = ECM                                                        ASKUSE76
      WEIT = 1.0                                                        ASKUSE77
      IFI  = IFI +1                                                     ASKUSE78
      IF( IFI.le.5 )    call LULIST(1)                                  ASKUSE79
C                                                                       ASKUSE80
C     fill BOS banks                                                    ASKUSE81
C     ==============                                                    ASKUSE82
C -   get the primary vertex                                            ASKUSE83
      CALL RANNOR (RX,RY)                                               ASKUSE84
      CALL RANNOR (RZ,RE)                                               ASKUSE85
      VRTX(1) = RX*SVERT(1)                                             ASKUSE86
      VRTX(2) = RY*SVERT(2)                                             ASKUSE87
      VRTX(3) = RZ*SVERT(3)                                             ASKUSE88
      VRTX(4) = 0.                                                      ASKUSE89
                                                                        ASKUSE90
C -   Call the specific routine KXL7AL to fill BOS banks                ASKUSE91
      CALL KXL7AL (VRTX,ISTA,NVRT,NTRK)                                 ASKUSE92
C -   check for errors                                                  ASKUSE93
      IF (ISTA.EQ.0 ) THEN                                              ASKUSE94
         ICOULU(10) = ICOULU(10)+1                                      ASKUSE95
      ELSEIF (ISTA.GT.0) THEN                                           ASKUSE96
         ICOULU(1) = ICOULU(1) +1                                       ASKUSE97
         ICOULU(9) = ICOULU(9) +1                                       ASKUSE98
      ELSEIF ( ISTA.LT.0) THEN                                          ASKUSE99
         ICOULU(-ISTA) = ICOULU(-ISTA) +1                               ASKUS100
         ICOULU(9) = ICOULU(9) +1                                       ASKUS101
      ENDIF                                                             ASKUS102
                                                                        ASKUS103
      END                                                               ASKUS104
      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C ------------------------------------------------------------------    ASKUSI 3
C - Y. GAO    - Jan 1996                                                ASKUSI 4
C! Initialization routine of SUSYGEN & JETSET7.4  generator             ASKUSI 5
C                                                                       ASKUSI 6
C ------------------------------------------------------------------    ASKUSI 7
      Implicit None                                                     ASKUSI 8
                                                                        ASKUSI 9
C     IGCOD  for MSSM01                                                 ASKUSI10
      Integer       IGCO                                                ASKUSI11
      PARAMETER    (IGCO=7018)                                          ASKUSI12
      Integer       IGCOD                                               ASKUSI13
                                                                        ASKUSI14
      Real          FVERS                                               ASKUSI15
      DATA          FVERS/                                              ASKUSI16
     $1.01                                                              VERSION2
     $                   /                                              ASKUSI18
      CHARACTER*30  DATE                                                ASKUSI19
      DATA DATE/                                                        DATE   2
     $ 'March 22  ,1996'                                                BBL001 1
     $/                                                                 DATE   4
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
      COMMON /BCS/   IW(LBCS )                                          BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
      Integer       MXIDHS                                              WORKHS 2
      Parameter    (MXIDHS=500)                                         WORKHS 3
      Integer       NIDOLD,IIDOLD,NIDNEW,IIDNEW                         WORKHS 4
      Logical       HSKEEP                                              WORKHS 5
      Common/WORKHS/HSKEEP,NIDOLD,IIDOLD(MXIDHS),NIDNEW,IIDNEW(MXIDHS)  WORKHS 6
      INTEGER    L1MST,L1PAR                                            LUNDCOM2
     &          ,L2PAR,L2PARF                                           LUNDCOM3
     &          ,LJNPAR                                                 LUNDCOM4
      PARAMETER (L1MST=200, L1PAR=200)                                  LUNDCOM5
      PARAMETER (L2PAR=500, L2PARF=2000)                                LUNDCOM6
      PARAMETER (LJNPAR=4000)                                           LUNDCOM7
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUNDCOM8
      INTEGER         MSTU,MSTJ                                         LUNDCOM9
      REAL            PARU,PARJ                                         LUNDCO10
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUNDCO11
      INTEGER         KCHG                                              LUNDCO12
      REAL            PMAS,PARF,VCKM                                    LUNDCO13
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCO14
     &                KFDP(L2PARF,5)                                    LUNDCO15
      INTEGER         MDCY,MDME,KFDP                                    LUNDCO16
      REAL            BRAT                                              LUNDCO17
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCO18
      CHARACTER*8     CHAF                                              LUNDCO19
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUNDCO20
      INTEGER         N7LU,K7LU                                         LUNDCO21
      REAL            P7LU,V7LU                                         LUNDCO22
********************* start of commons of SUSYGEN **********************SUSCOM 2
      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rfmsq,rfmstopl,rfmstopr, SUSCOM 3
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)        SUSCOM 4
      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,                    SUSCOM 5
     +        rfmsq,rfmstopl,rfmstopr,                                  SUSCOM 6
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan           SUSCOM 7
                                                                        SUSCOM 8
      real*8  gmaum,gmaur,gm0,gtanb,gatri                               SUSCOM 9
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM10
      common/steer/gmaum,gmaur,gm0,gtanb,gatri                          SUSCOM11
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM12
                                                                        SUSCOM13
      Integer modes,MIX                                                 SUSCOM14
      common/mds/modes,MIX                                              SUSCOM15
                                                                        SUSCOM16
      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota             SUSCOM17
      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota       SUSCOM18
                                                                        SUSCOM19
      logical wrt,scan,lepi                                             SUSCOM20
      common/str/wrt,scan,lepi                                          SUSCOM21
                                                                        SUSCOM22
      real*8  flum,ecm,s,roots,T,Q,Q2,EN                                SUSCOM23
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        SUSCOM24
                                                                        SUSCOM25
      real*8  QK                                                        SUSCOM26
      COMMON/ISR/ QK(4)                                                 SUSCOM27
                                                                        SUSCOM28
      real erad,srad                                                    SUSCOM29
      common/srada/erad(100),srad(100)                                  SUSCOM30
                                                                        SUSCOM31
      integer  idbg,igener,irad                                         SUSCOM32
      COMMON /CONST/ idbg,igener,irad                                   SUSCOM33
                                                                        SUSCOM34
      integer  index,index1,index2,nevt                                 SUSCOM35
      COMMON/INDEXX/index,index1,index2,nevt                            SUSCOM36
                                                                        SUSCOM37
      real*8   fmpr1,fmpr2,XCROST,APRO                                  SUSCOM38
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO                             SUSCOM39
                                                                        SUSCOM40
      real*8   cosphimix,facqcd,fgama,spartmas,ratq                     SUSCOM41
      common/mixings/cosphimix,facqcd,fgama,spartmas,ratq               SUSCOM42
                                                                        SUSCOM43
      real*8   FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,    SUSCOM44
     +         FLC,FRC,gms,echar                                        SUSCOM45
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   SUSCOM46
     +FLC(12),FRC(12),gms(12),echar(12)                                 SUSCOM47
                                                                        SUSCOM48
      real*8      TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM49
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM50
                                                                        SUSCOM51
      real*8   fmal,fmar,ratqa,fgamc,fgamcr,cosmi                       SUSCOM52
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)   SUSCOM53
     +,cosmi(12)                                                        SUSCOM54
                                                                        SUSCOM55
      real*4 phimix,stop1                                               SUSCOM56
      common/stopmix/phimix,stop1                                       SUSCOM57
                                                                        SUSCOM58
      real*8   xgaug,xeta                                               SUSCOM59
      COMMON/XCROS/xgaug(8),xeta(8)                                     SUSCOM60
                                                                        SUSCOM61
      integer  ispa,kl,klap,idecs                                       SUSCOM62
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         SUSCOM63
                                                                        SUSCOM64
      Integer  idecsel                                                  SUSCOM65
      common/decsel/idecsel(18)                                         SUSCOM66
                                                                        SUSCOM67
      real*8   brsum,brsuma                                             SUSCOM68
      common/brsum/ brsum(5,6,6),brsuma(6)                              SUSCOM69
                                                                        SUSCOM70
      real*8   fms,fmi,fmk,fml1,fml2,etai,etak,brspa                    SUSCOM71
     +        ,brgaug,fmelt,fmert,fmelu,fmeru                           SUSCOM72
      Integer  lind                                                     SUSCOM73
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     SUSCOM74
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                SUSCOM75
                                                                        SUSCOM76
      Real*4   zr,was,esa                                               SUSCOM77
      Real*8   VOIJL,VOIJR,gfir,gfil                                    SUSCOM78
      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),                              SUSCOM79
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)                         SUSCOM80
                                                                        SUSCOM81
      real*8   OIJL,OIJR,V,U,FM,ETA                                     SUSCOM82
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)     SUSCOM83
                                                                        SUSCOM84
      real*8   oijlp,oijrp                                              SUSCOM85
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)                               SUSCOM86
                                                                        SUSCOM87
      Integer       ILOOP                                               SUSCOM88
      COMMON/SFLOOP/ILOOP                                               SUSCOM89
                                                                        SUSCOM90
      Real*8   PV                                                       SUSCOM91
      Integer  IFLAV,NFLAV                                              SUSCOM92
      COMMON /PARTC / PV(5,20),IFLAV(20,2),NFLAV                        SUSCOM93
                                                                        SUSCOM94
      real*8   gw,widfl,widfr,fmeltw,fmertw,fmeluw,fmeruw,gent,gentl    SUSCOM95
      integer  linda                                                    SUSCOM96
      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw, SUSCOM97
     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)                       SUSCOM98
********************* end  of commons of SUSYGEN ***********************SUSCOM99
      Integer       mproc                                               SUPROC 2
      Parameter    (mproc=35)                                           SUPROC 3
      Integer       iproc,iprod,ipar1,ipar2,ihand,isusy,iopen,ievnt     SUPROC 4
      Real          xproc                                               SUPROC 5
      common/SUPROC/iproc(mproc),iprod(mproc)                           SUPROC 6
     &             ,ipar1(mproc),ipar2(mproc)                           SUPROC 7
     &             ,ihand(mproc),isusy(mproc)                           SUPROC 8
     &             ,iopen(mproc),ievnt(mproc)                           SUPROC 9
     &             ,xproc(mproc)                                        SUPROC10
      Integer       IPRI,IHST                                           SUWGHT 2
      Real          ECMS,SVERT                                          SUWGHT 3
      Integer       mact                                                SUWGHT 4
      Parameter    (mact=100)                                           SUWGHT 5
      Integer       nact,iact,icoulu                                    SUWGHT 6
      Real          sact,wact                                           SUWGHT 7
      common/SUWGHT/IPRI,ECMS,SVERT(3),icoulu(10)                       SUWGHT 8
     &             ,nact,iact(mact)                                     SUWGHT 9
     &                  ,sact(mact)                                     SUWGHT10
     &                  ,wact(mact)                                     SUWGHT11
      Integer       IUT                                                 ASKUSI27
      Real          TABL(100)                                           ASKUSI28
      Integer       NWBL,INDL                                           ASKUSI29
      Integer       IEBEAM,JRLEP,ALTABL,ALRLEP                          ASKUSI30
      External                   ALTABL,ALRLEP                          ASKUSI31
      Integer       IPART,IKLIN,IDBNK                                   ASKUSI32
      Integer       NLINK,NAMIND                                        ASKUSI33
      External      NLINK,NAMIND                                        ASKUSI34
      Integer       LUCOMP                                              ASKUSI35
      External      LUCOMP                                              ASKUSI36
                                                                        ASKUSI37
      Integer       mfail,i,j                                           ASKUSI38
      Real          XC,XT                                               ASKUSI39
      Integer       IG,JG                                               ASKUSI40
                                                                        ASKUSI41
      Integer       ID                                                  ASKUSI42
C*CA BDECL                                                              ASKUSI43
C*CA BMACRO                                                             ASKUSI44
C                                                                       ASKUSI45
C     LOG unit                                                          ASKUSI46
C     ========                                                          ASKUSI47
      IUT = IW(6)                                                       ASKUSI48
C                                                                       ASKUSI49
C     Return generator code IGCOD                                       ASKUSI50
C     ===========================                                       ASKUSI51
      IGCOD = IGCO                                                      ASKUSI52
                                                                        ASKUSI53
      WRITE(IUT,1002)                                                   ASKUSI54
      WRITE(IUT,1000)                                                   ASKUSI55
      WRITE(IUT,1001) FVERS,DATE                                        ASKUSI56
      WRITE(IUT,1003) IGCOD                                             ASKUSI57
      WRITE(IUT,1002)                                                   ASKUSI58
C                                                                       ASKUSI59
C     GENE card                                                         ASKUSI60
C     =========                                                         ASKUSI61
      IDBNK = NLINK('GENE',0)                                           ASKUSI62
      if( IDBNK.gt.0 ) then                                             ASKUSI63
        ECMS = RW(IDBNK+1)                                              ASKUSI64
        IPRI = IW(IDBNK+2)                                              ASKUSI65
        IHST = IW(IDBNK+3)                                              ASKUSI66
      else                                                              ASKUSI67
        write(IUT,*) 'ASKUSI: no GENE card given',                      ASKUSI68
     &               'default values are taken'                         ASKUSI69
        ECMS = 200.0                                                    ASKUSI70
        IPRI = 10                                                       ASKUSI71
        IHST =  0                                                       ASKUSI72
      endif                                                             ASKUSI73
      HSKEEP=.false.                                                    ASKUSI74
      if( IHST.eq.1 )   HSKEEP=.true.                                   ASKUSI75
      recm  = ECMS                                                      ASKUSI76
      rflum = 0.0      ! we don't use luminosity but no. of events      ASKUSI77
C                                                                       ASKUSI78
C     make use of a smearing of the vertex                              ASKUSI79
C     ====================================                              ASKUSI80
      IDBNK = NLINK('SVRT',0)                                           ASKUSI81
      IF( IDBNK.gt.0 ) then                                             ASKUSI82
         SVERT(1) = RW(IDBNK+1)                                         ASKUSI83
         SVERT(2) = RW(IDBNK+2)                                         ASKUSI84
         SVERT(3) = RW(IDBNK+3)                                         ASKUSI85
      ELSE                                                              ASKUSI86
         SVERT(1) = 0.018                                               ASKUSI87
         SVERT(2) = 0.001                                               ASKUSI88
         SVERT(3) = 0.7                                                 ASKUSI89
      ENDIF                                                             ASKUSI90
C                                                                       ASKUSI91
C     initialize counters                                               ASKUSI92
C     ===============                                                   ASKUSI93
      DO 5  i=1,10                                                      ASKUSI94
 5    ICOULU(i)=0                                                       ASKUSI95
C                                                                       ASKUSI96
C     initialize LUND                                                   ASKUSI97
C     ===============                                                   ASKUSI98
C -   load block data from JETSET with brute force.                     ASKUSI99
C -   so it can't be missed                                             ASKUS100
C     IF ( ECMS.LT.0.) CALL LUDATA                                      ASKUS101
C -   keep fragmentation info                                           ASKUS102
      MSTU(17) = 1                                                      ASKUS103
C -   Final   state radiation                                           ASKUS104
      MSTJ(41) = 2                                                      ASKUS105
C -   use non discrete masses for resonnances                           ASKUS106
      MSTJ(24) = 2                                                      ASKUS107
C -   SLAC fragm. functions for b,c  Symetric LUND for u,d,s            ASKUS108
      MSTJ(11) = 3                                                      ASKUS109
C -   mod to lund fragm. functions params                               ASKUS110
      PARJ ( 21)  =0.358                                                ASKUS111
      PARJ  (41)  =0.500                                                ASKUS112
      PARJ  (42)  =0.840                                                ASKUS113
      PARJ  (81)  =0.310                                                ASKUS114
      PARJ  (82)  =1.500                                                ASKUS115
C -   mod Peterson's fragm. functions params                            ASKUS116
      PARJ  (54)  = -0.200                                              ASKUS117
      PARJ  (55)  = -0.006                                              ASKUS118
      PARU (102)  =  0.232                                              ASKUS119
      PARJ (123)  =  91.17                                              ASKUS120
      PARU (124)  =  2.5                                                ASKUS121
C -   Set up some default values for masses and initial conditions      ASKUS122
C -   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by  ASKUS123
C -   a PMA1 card                                                       ASKUS124
      PMAS(LUCOMP(25),1)= 100.                                          ASKUS125
      PMAS(LUCOMP( 6),1)= 174.                                          ASKUS126
      PMAS(LUCOMP(23),1)= 91.2                                          ASKUS127
C                                                                       ASKUS128
C     save the histo-ids have been booked                               ASKUS129
C     ===================================                               ASKUS130
      call HIDALL(IIDOLD,NIDOLD)                                        ASKUS131
C                                                                       ASKUS132
C     get the SUSYGEN parameters from data card                         ASKUS133
C     =========================================                         ASKUS134
      CALL SUSYGDAT                                                     ASKUS135
      call SUCARD                                                       ASKUS136
C                                                                       ASKUS137
C     set SUSY-particle's name, charge etc                              ASKUS138
C     ====================================                              ASKUS139
      call SUPART                                                       ASKUS140
C                                                                       ASKUS141
C     generate mass & decay branching ratios etc                        ASKUS142
C     ==========================================                        ASKUS143
      call SUSANA(IUT,mfail)                                            ASKUS144
      if( mfail.ne.0 ) then                                             ASKUS145
        write(IUT,*) 'ASKUSI: SUSANA error, -stop-'                     ASKUS146
        call EXIT                                                       ASKUS147
      endif                                                             ASKUS148
      call SUSETM                                                       ASKUS149
C                                                                       ASKUS150
C     complete PART bank with LUND  particles                           ASKUS151
C     use the library routine KXL74A                                    ASKUS152
C     it should work even for the SUSYGEN particles included            ASKUS153
C     ======================================================            ASKUS154
      CALL KXL74A (IPART,IKLIN)                                         ASKUS155
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUS156
        WRITE(IUT,*) 'ASKUSI: error in PART or KLIN bank -STOP -'       ASKUS157
        CALL EXIT                                                       ASKUS158
      ENDIF                                                             ASKUS159
                                                                        ASKUS160
      IF ( MOD(IPRI,10).GT.0) THEN                                      ASKUS161
         CALL PRPART                                                    ASKUS162
      ENDIF                                                             ASKUS163
C                                                                       ASKUS164
C     save the working histograms created by SUSYGEN                    ASKUS165
C     ==============================================                    ASKUS166
      call HIDALL(IIDNEW,NIDNEW)                                        ASKUS167
C                                                                       ASKUS168
C     calculate cross-sections for each process                         ASKUS169
C     =========================================                         ASKUS170
      nact=0                                                            ASKUS171
      IG  =0                                                            ASKUS172
      XT  =0.0                                                          ASKUS173
      do i=1,mproc                                                      ASKUS174
        call SUMSSM(i,IG,JG,XC)                                         ASKUS175
        xproc(i)=XC                                                     ASKUS176
        if( xproc(i).gt.0.0 .and. iopen(i).eq.1 ) then                  ASKUS177
          nact      =nact+1                                             ASKUS178
          iact(nact)=i                                                  ASKUS179
          sact(nact)=xproc(i)                                           ASKUS180
          XT        =XT+sact(nact)                                      ASKUS181
        endif                                                           ASKUS182
      enddo                                                             ASKUS183
C                                                                       ASKUS184
C     give a summary of initialization                                  ASKUS185
C     ================================                                  ASKUS186
      call SUPRNT(1)                                                    ASKUS187
C                                                                       ASKUS188
C     no active process                                                 ASKUS189
C     =================                                                 ASKUS190
      if( nact.eq.0 ) then                                              ASKUS191
        write(IUT,*) 'ASKUSI: all processes have been switched off'     ASKUS192
        write(IUT,*) '            or the ECMS below threshold     '     ASKUS193
        write(IUT,*) '               stop after the summary       '     ASKUS194
        call SUPRNT(2)                                                  ASKUS195
        write(IUT,*) '                      -STOP-                '     ASKUS196
        call EXIT                                                       ASKUS197
      endif                                                             ASKUS198
C                                                                       ASKUS199
C     check the parameter space has been excluded by LEPI searches or noASKUS200
C     ==================================================================ASKUS201
      call LEPLIM(mfail)                                                ASKUS202
      if( mfail.ne.0 ) then                                             ASKUS203
        write(IUT,*) 'Warning: The parameters chosen for this run '     ASKUS204
        write(IUT,*) '         has been excluded by LEPI searches '     ASKUS205
        if( LEPI ) then                                                 ASKUS206
        write(IUT,*) '              stop after the summary        '     ASKUS207
        call SUPRNT(2)                                                  ASKUS208
        write(IUT,*) '                      -STOP-                '     ASKUS209
        call EXIT                                                       ASKUS210
        else                                                            ASKUS211
        write(IUT,*) '                    _CONTINUE-              '     ASKUS212
        endif                                                           ASKUS213
      endif                                                             ASKUS214
C                                                                       ASKUS215
C     get the generation weights                                        ASKUS216
C     ==========================                                        ASKUS217
      wact(1)=sact(1)/XT                                                ASKUS218
      do i=2,nact                                                       ASKUS219
        wact(i)=sact(i)/XT+wact(i-1)                                    ASKUS220
      enddo                                                             ASKUS221
C                                                                       ASKUS222
C     dump the generator parameters for this run in a bank              ASKUS223
C     assume all parameters are real and stored as a single row         ASKUS224
C     =========================================================         ASKUS225
      call VZERO(TABL,100)                                              ASKUS226
      NWBL = 0                                                          ASKUS227
C -   1:35 cross sections for process 1-35. =0 if not permitted or off  ASKUS228
      do i=1,nact                                                       ASKUS229
        TABL(iact(i)) = sact(i)                                         ASKUS230
      enddo                                                             ASKUS231
      NWBL = NWBL+35                                                    ASKUS232
C -   MSSM parameters: 5 words                                          ASKUS233
      TABL(NWBL+ 1) = TANB                                              ASKUS234
      TABL(NWBL+ 2) = FMGAUG                                            ASKUS235
      TABL(NWBL+ 3) = FMR                                               ASKUS236
      TABL(NWBL+ 4) = FM0                                               ASKUS237
      TABL(NWBL+ 5) = ATRI                                              ASKUS238
      NWBL = NWBL+ 5                                                    ASKUS239
C -   neutralino mass matrix: 16 words                                  ASKUS240
      do i=1,4                                                          ASKUS241
      do j=1,4                                                          ASKUS242
        NWBL = NWBL+1                                                   ASKUS243
        TABL(NWBL) = zr(j,i)                                            ASKUS244
      enddo                                                             ASKUS245
      enddo                                                             ASKUS246
C -   chargino mass matrix u,v : 8 words                                ASKUS247
      do i=1,2                                                          ASKUS248
      do j=1,2                                                          ASKUS249
        NWBL = NWBL+1                                                   ASKUS250
        TABL(NWBL) = U(j,i)                                             ASKUS251
      enddo                                                             ASKUS252
      enddo                                                             ASKUS253
                                                                        ASKUS254
      do i=1,2                                                          ASKUS255
      do j=1,2                                                          ASKUS256
        NWBL = NWBL+1                                                   ASKUS257
        TABL(NWBL) = V(j,i)                                             ASKUS258
      enddo                                                             ASKUS259
      enddo                                                             ASKUS260
C -   finally the smearing vertex                                       ASKUS261
      TABL(NWBL+1) = SVERT(1)                                           ASKUS262
      TABL(NWBL+2) = SVERT(2)                                           ASKUS263
      TABL(NWBL+3) = SVERT(3)                                           ASKUS264
      NWBL = NWBL + 3                                                   ASKUS265
      INDL = ALTABL('KPAR',NWBL,1,TABL,'2I,(F)','C')                    ASKUS266
C                                                                       ASKUS267
C     Fill RLEP bank                                                    ASKUS268
C     ==============                                                    ASKUS269
      IEBEAM = NINT(ECM* 500  )                                         ASKUS270
      JRLEP  = ALRLEP(IEBEAM,'    ',0,0,0)                              ASKUS271
      CALL PRTABL('RLEP',0)                                             ASKUS272
      IF ( IPRI.GT.0 ) CALL PRTABL('KPAR',0)                            ASKUS273
                                                                        ASKUS274
1000  FORMAT(/,10X,'* WELCOME TO SUSYGEN/JETSET7.4 as MSSM01',          ASKUS275
     &       /,10X,'*   (Ref: ALEPH 96-?? PHYSIC 96-?? ) ' )            ASKUS276
1001  FORMAT(  10X,'* ','Version ',F6.2,' -Last modified on   ',A30)    ASKUS277
1002  FORMAT(  10X,72('*'))                                             ASKUS278
1003  FORMAT(/,10X,'* MSSM01 - CODE NUMBER = ',I10)                     ASKUS279
      RETURN                                                            ASKUS280
      END                                                               ASKUS281
      subroutine interf                                                 INTERF 2
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**INTERF 3
*-- Author :    Stavros Katsanevas   14/04/95                          *INTERF 4
*-- modified by Y.GAO 1/02/96                                          *INTERF 5
*   bugs are fixed                                                     *INTERF 6
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**INTERF 7
      PARAMETER (NOUT=33)                                               INTERF 8
      INTEGER IDOUT(NOUT)                                               INTERF 9
                                                                        INTERF10
      SAVE /SSMODE/                                                     INTERF11
C          SM ( JETSET 7.03 ) ident code definitions.                   INTERF12
      INTEGER IDUP,IDDN,IDST,IDCH,IDBT,IDTP                             INTERF13
      INTEGER IDNE,IDE,IDNM,IDMU,IDNT,IDTAU                             INTERF14
      INTEGER IDGL,IDGM,IDW,IDZ                                         INTERF15
                                                                        INTERF16
      PARAMETER (IDUP=2,IDDN=1,IDST=3,IDCH=4,IDBT=5,IDTP=6)             INTERF17
      PARAMETER (IDNE=12,IDE=11,IDNM=14,IDMU=13,IDNT=16,IDTAU=15)       INTERF18
      PARAMETER (IDGL=21,IDGM=22,IDW=24,IDZ=23)                         INTERF19
                                                                        INTERF20
      PARAMETER (ISUPL=42,ISDNL=41,ISSTL=43,ISCHL=44,ISBTL=45,ISTPL=46) INTERF21
      PARAMETER (ISNEL=52,ISEL=51,ISNML=54,ISMUL=53,ISNTL=56,ISTAUL=55) INTERF22
      PARAMETER (ISUPR=48,ISDNR=47,ISSTR=49,ISCHR=50,ISBTR=61,ISTPR=62) INTERF23
      PARAMETER (ISER=57,ISMUR=58,ISTAUR=59)                            INTERF24
      PARAMETER (ISGL=70)                                               INTERF25
      PARAMETER (ISZ1=71,ISZ2=72,ISZ3=73,ISZ4=74,ISW1=75,ISW2=76)       INTERF26
      PARAMETER (ISWN1=77,ISWN2=78,ISHL=25,ISHH=35,ISHA=36,ISHC=37)     INTERF27
                                                                        INTERF28
      DATA IDOUT/                                                       INTERF29
     +ISZ1,ISZ2,ISZ3,ISZ4,ISW1,ISW2,                                    INTERF30
     +ISGL,ISUPL,ISDNL,ISSTL,ISCHL,ISBTL,ISTPL,ISUPR,ISDNR,             INTERF31
     +ISSTR,ISCHR,ISBTR,ISTPR,ISEL,ISMUL,ISTAUL,ISNEL,ISNML,ISNTL,      INTERF32
     +ISER,ISMUR,ISTAUR,ISHL,ISHH,ISHA,ISHC,IDTP/                       INTERF33
                                                                        INTERF34
                                                                        INTERF35
C          MXSS                 = maximum number of modes               INTERF36
C          NSSMOD               = number of modes                       INTERF37
C          ISSMOD               = initial particle                      INTERF38
C          JSSMOD               = final particles                       INTERF39
C          GSSMOD               = width                                 INTERF40
C          BSSMOD               = branching ratio                       INTERF41
      INTEGER MXSS                                                      INTERF42
      PARAMETER (MXSS=2000)                                             INTERF43
      COMMON/SSMODE/NSSMOD,ISSMOD(MXSS),JSSMOD(5,MXSS)                  INTERF44
      COMMON/SSMOD1/GSSMOD(MXSS),BSSMOD(MXSS)                           INTERF45
      INTEGER NSSMOD,ISSMOD,JSSMOD                                      INTERF46
      DOUBLE PRECISION GSSMOD,BSSMOD                                    INTERF47
                                                                        INTERF48
      double precision fms,fmi,fmk,fml1,fml2,etai,etak,brspa            INTERF49
     +,brgaug,fmelt,fmert,fmelu,fmeru                                   INTERF50
                                                                        INTERF51
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     INTERF52
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                INTERF53
                                                                        INTERF54
      double precision fmal,fmar,cosmi,ratqa,fgamc,fgamcr               INTERF55
                                                                        INTERF56
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)   INTERF57
     +,cosmi(12)                                                        INTERF58
                                                                        INTERF59
                                                                        INTERF60
                                                                        INTERF61
      DOUBLE PRECISION flum,ECM,s,roots,T,Q,Q2,EN                       INTERF62
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        INTERF63
                                                                        INTERF64
      DOUBLE PRECISION XGAUG,XETA,brtot                                 INTERF65
      COMMON/XCROS/xgaug(8),xeta(8)                                     INTERF66
                                                                        INTERF67
      common/ubra/ndeca(-80:80)                                         INTERF68
      common/ubra1/brtot(2,50,-80:80)                                   INTERF69
C                                                                       INTERF70
C      brgaug(i,j,k) is the integrated branching ratios for gauginos    INTERF71
C      i is one of the 18 patterns uu,dd,ll,vv,ud,lv x3 generations     INTERF72
C      j is one of the son gauginos                                     INTERF73
C      k is one of the father gauginos                                  INTERF74
C                                                                       INTERF75
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         INTERF76
      common/decsel/idecsel(18)                                         INTERF77
                                                                        INTERF78
      n=0                                                               INTERF79
      do 30 k=1,6                                                       INTERF80
        do 20 j=1,6                                                     INTERF81
          do 10 i=1,18                                                  INTERF82
            if(brgaug(i,j,k).eq.0)go to 10                              INTERF83
            if(i.le.18)then                                             INTERF84
              if(idecsel(i).eq.0)go to 10                               INTERF85
              n=n+1                                                     INTERF86
              issmod(n)=70+k                                            INTERF87
              jssmod(1,n)=70+j                                          INTERF88
              jssmod(2,n)=-kl(2,i)                                      INTERF89
              jssmod(3,n)=-kl(1,i)                                      INTERF90
              if(k.eq.5.or.k.eq.6)jssmod(2,n)=kl(1,i)                   INTERF91
              if(k.eq.5.or.k.eq.6)jssmod(3,n)=kl(2,i)                   INTERF92
              gssmod(n)=brgaug(i,j,k)                                   INTERF93
            endif                                                       INTERF94
                                                                        INTERF95
   10     continue                                                      INTERF96
   20   continue                                                        INTERF97
   30 continue                                                          INTERF98
                                                                        INTERF99
C                                                                       INTER100
C      brspa(i,j) is the integrated branching ratios for sparticles     INTER101
C      i is one of the 8 gaugino decays                                 INTER102
C      j are the sparticle codes 12 left + 12 right                     INTER103
                                                                        INTER104
      do 50 j=1,24                                                      INTER105
        do 40 i=1,6                                                     INTER106
          if(brspa(i,j).eq.0.)go to 40                                  INTER107
          n=n+1                                                         INTER108
          il=mod(j-1,12)+1                                              INTER109
          kla=(j-1)/12+1                                                INTER110
          issmod(n) =ispa(il,kla)                                       INTER111
          jssmod(1,n)=70+i                                              INTER112
          jssmod(2,n)=idecs(il,1)                                       INTER113
CYG       if(i.gt.4)jssmod(2,n)=idecs(il,2)                             INTER114
CYG                                                                     INTER115
CYG       carefull on charge conservation                               INTER116
CYG       ===============================                               INTER117
          if(i.gt.4) then                                               INTER118
            jssmod(2,n)=idecs(il,2)                                     INTER119
            kf_lund=issmod(n)                                           INTER120
            ic_chag=LUCHGE(kf_lund)                                     INTER121
            if( ic_chag.lt.0 ) jssmod(1,n)=-jssmod(1,n)                 INTER122
          endif                                                         INTER123
CYG                                                                     INTER124
          jssmod(3,n)=0                                                 INTER125
          gssmod(n)=brspa(i,j)                                          INTER126
                                                                        INTER127
   40   continue                                                        INTER128
   50 continue                                                          INTER129
                                                                        INTER130
      nssmod=n                                                          INTER131
                                                                        INTER132
                                                                        INTER133
CYG       WRITE(1,10000)                                                INTER134
10000 FORMAT(/' PARENT -->     DAUGHTERS',14X,'WIDTH (KeV) ',7X,        INTER135
     +'BRANCHING RATIO'/)                                               INTER136
                                                                        INTER137
C          Write all modes                                              INTER138
                                                                        INTER139
      DO 60  J=1,NOUT                                                   INTER140
                                                                        INTER141
        call ssnorm(idout(j))                                           INTER142
                                                                        INTER143
        CALL SSPRT(IDOUT(J))                                            INTER144
                                                                        INTER145
   60 CONTINUE                                                          INTER146
                                                                        INTER147
      CALL NEWSUM                                                       INTER148
                                                                        INTER149
c                                                                       INTER150
c integrate the branching ratios                                        INTER151
c                                                                       INTER152
      do 70  k=41,76                                                    INTER153
   70 call sstot(k)                                                     INTER154
                                                                        INTER155
      return                                                            INTER156
      end                                                               INTER157
      SUBROUTINE SFERMION(mfail)                                        SFERMI 2
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**SFERMI 3
*                                                                      *SFERMI 4
*-- Author :    Stavros Katsanevas   14/04/95                          *SFERMI 5
*-- Modified by Y.GAO     1/02/96                                      *SFERMI 6
*   quite a lot has been changed, refer the original code to see the   *SFERMI 7
*   differences.                                                       *SFERMI 8
*                                                                      *SFERMI 9
*   mfail=1   L mass negative                                          *SFERMI10
*        =2   R mass negative                                          *SFERMI11
*        =3   1 mass negative after mixing                             *SFERMI12
*        =4   2 mass negative after mixing                             *SFERMI13
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**SFERMI14
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               SFERMI15
* Keys                                                                  SFERMI16
      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rfmsq,rfmstopl,rfmstopr, SFERMI17
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)        SFERMI18
      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,                    SFERMI19
     +        rfmsq,rfmstopl,rfmstopr,                                  SFERMI20
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan           SFERMI21
                                                                        SFERMI22
      logical wrt,scan,lepi                                             SFERMI23
      common/str/wrt,scan,lepi                                          SFERMI24
                                                                        SFERMI25
                                                                        SFERMI26
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   SFERMI27
     +FLC(12),FRC(12),gms(12),echar(12)                                 SFERMI28
                                                                        SFERMI29
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SFERMI30
                                                                        SFERMI31
      common/spartcl/fmal(12),fmar(12),ratq(12),fgamc(12),fgamcr(12)    SFERMI32
     +,cosmi(12)                                                        SFERMI33
      dimension fm1(12),fm2(12)                                         SFERMI34
                                                                        SFERMI35
                                                                        SFERMI36
      common/steer/gmaum,gmaur,gm0,gtanb,gatri,                         SFERMI37
     +fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu                    SFERMI38
      common/mds/modes,MIX                                              SFERMI39
                                                                        SFERMI40
      real*4 phimix,stop1                                               SFERMI41
      common/stopmix/phimix,stop1                                       SFERMI42
                                                                        SFERMI43
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         SFERMI44
C                                                                       SFERMI45
C     decide if we should call SMGUT, all the s-fermion masses will be  SFERMI46
C     overwritten after the calling                                     SFERMI47
C     =============================                                     SFERMI48
      mfail=0                                                           SFERMI49
      if( modes.eq.1 ) call SMGUT                                       SFERMI50
C                                                                       SFERMI51
C     inexistent snu right                                              SFERMI52
C     ====================                                              SFERMI53
      fmar( 3) = 10000.                                                 SFERMI54
      fmar( 7) = 10000.                                                 SFERMI55
      fmar(11) = 10000.                                                 SFERMI56
C                                                                       SFERMI57
C     checking                                                          SFERMI58
C     ========                                                          SFERMI59
      do 10 k=1,12                                                      SFERMI60
        IF(FMAL(k).LE.0.) mfail=1                                       SFERMI61
        IF(FMAR(k).LE.0.) mfail=2                                       SFERMI62
                                                                        SFERMI63
        if(fmal(k).lt.1.) fmal(k)=1.                                    SFERMI64
        if(fmar(k).lt.1.) fmar(k)=1.                                    SFERMI65
   10 continue                                                          SFERMI66
C                                                                       SFERMI67
C     mixings                                                           SFERMI68
C     =======                                                           SFERMI69
      COS2B=ABS(COSB**2-SINB**2)                                        SFERMI70
      DO 40 i=1,12                                                      SFERMI71
        fm1(i)=fmal(i)**2                                               SFERMI72
        fm2(i)=fmar(i)**2                                               SFERMI73
                                                                        SFERMI74
        costh=1.                                                        SFERMI75
                                                                        SFERMI76
        if(i.ge.9.and.MIX.ne.0)then                                     SFERMI77
          ctanb=tanb                                                    SFERMI78
          if(mod(i,2).eq.1)ctanb=1./tanb                                SFERMI79
          ALT=((ATRI+FMR*CTANB))                                        SFERMI80
C -                                                                     SFERMI81
C -       mixing for the 3d family                                      SFERMI82
C -       ------------------------                                      SFERMI83
          sums=fmal(i)**2+fmar(i)**2                                    SFERMI84
          difs=fmal(i)**2-fmar(i)**2                                    SFERMI85
          delta=difs**2+4.d0*alt**2*gms(i)**2                           SFERMI86
          if(delta.ne.0.)then                                           SFERMI87
            FM1(I)=0.5D0*(sums-dsqrt(delta))                            SFERMI88
            FM2(I)=0.5D0*(sums+dsqrt(delta))                            SFERMI89
            cos2th=difs/dsqrt(delta)                                    SFERMI90
            costh=(1+COS2TH)/2.                                         SFERMI91
            if(costh.ne.0.)costh=dsqrt(costh)                           SFERMI92
          endif                                                         SFERMI93
C -                                                                     SFERMI94
C -       overwrite stop mixing angle, provision to read in stop mass   SFERMI95
C -       and mixing                                                    SFERMI96
C -       ==========                                                    SFERMI97
          if(stop1.gt.0..and.i.eq.9)then                                SFERMI98
            phimix=phimix*3.1415926535/180.                             SFERMI99
            phamix=dble(phimix)                                         SFERM100
            costh=cos(phamix)                                           SFERM101
            tanth=tan(phamix)                                           SFERM102
            fm1(i)=stop1**2                                             SFERM103
            fm2(i)=stop1**2+4.d0*alt*gms(i)*tanth                       SFERM104
          endif                                                         SFERM105
        endif                                                           SFERM106
                                                                        SFERM107
                                                                        SFERM108
        COSMI(I)=costh                                                  SFERM109
        ratq(i)=-(ECHAR(I))                                             SFERM110
        FGAMC(i) =(FLC(I)-FRC(I))*COSMI(I)**2+FRC(I)                    SFERM111
        FGAMCR(i)=(FRC(I)-FLC(I))*COSMI(I)**2+FLC(I)                    SFERM112
                                                                        SFERM113
                                                                        SFERM114
        IF(FM1(I).LE.0.)then                                            SFERM115
          mfail=3                                                       SFERM116
        else                                                            SFERM117
          FM1(I)=DSQRT(FM1(I))                                          SFERM118
        endif                                                           SFERM119
                                                                        SFERM120
        IF(FM2(I).LE.0.)then                                            SFERM121
          mfail=4                                                       SFERM122
        else                                                            SFERM123
          FM2(I)=DSQRT(FM2(I))                                          SFERM124
        endif                                                           SFERM125
                                                                        SFERM126
        if(I.lt.9)go to 40                                              SFERM127
                                                                        SFERM128
        fmal(I)=fm1(I)                                                  SFERM129
        fmar(I)=fm2(I)                                                  SFERM130
   40 CONTINUE                                                          SFERM131
      end                                                               SFERM132
      SUBROUTINE SFRAGMENT(IFLAG)                                       SFRAGM 2
C                                                                       SFRAGM 3
C-----------------------------------------                              SFRAGM 4
C                                                                       SFRAGM 5
C   Author   :- Y. Gao                30-JAN-1996                       SFRAGM 6
C                                                                       SFRAGM 7
C=========================================                              SFRAGM 8
C                                                                       SFRAGM 9
C   Purpose   : interface with LUND, the original code in SUSYGEN       SFRAGM10
C               has been rewritten.                                     SFRAGM11
C   Inputs    : none                                                    SFRAGM12
C   Outputs   : IFLAG=0 success                                         SFRAGM13
C                    =1 cannot find the colour partner                  SFRAGM14
C                                                                       SFRAGM15
C=========================================                              SFRAGM16
      real*8  flum,ecm,s,roots,T,Q,Q2,EN                                SFRAGM17
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        SFRAGM18
                                                                        SFRAGM19
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)                     SFRAGM20
                                                                        SFRAGM21
      DOUBLE PRECISION PV                                               SFRAGM22
      COMMON /PARTC / PV(5,20),IFLAV(20,2),NFLAV                        SFRAGM23
                                                                        SFRAGM24
      INTEGER      NP,KP(10),NG,KG(10)                                  SFRAGM25
      REAL*8       PP(5),PG(5)                                          SFRAGM26
                                                                        SFRAGM27
      INTEGER      NMOT,IMOT(100,2)                                     SFRAGM28
      REAL*4       QMX                                                  SFRAGM29
C*                                                                      SFRAGM30
C*    pre-treatment, careful on the particle-antiparticle relationship  SFRAGM31
C*    ================================================================  SFRAGM32
      IFLAG=0                                                           SFRAGM33
      DO I=1,NFLAV                                                      SFRAGM34
        IF( IFLAV(I,1).EQ.-71 ) IFLAV(I,1)=71                           SFRAGM35
        IF( IFLAV(I,1).EQ.-72 ) IFLAV(I,1)=72                           SFRAGM36
        IF( IFLAV(I,1).EQ.-73 ) IFLAV(I,1)=73                           SFRAGM37
        IF( IFLAV(I,1).EQ.-74 ) IFLAV(I,1)=74                           SFRAGM38
        IF( IFLAV(I,1).EQ.-75 ) IFLAV(I,1)=77                           SFRAGM39
        IF( IFLAV(I,1).EQ.-76 ) IFLAV(I,1)=78                           SFRAGM40
      ENDDO                                                             SFRAGM41
C*                                                                      SFRAGM42
C*    original particles and gamma                                      SFRAGM43
C*    ============================                                      SFRAGM44
      N=0                                                               SFRAGM45
      if(NFLAV.EQ.0) RETURN                                             SFRAGM46
                                                                        SFRAGM47
      NP=0                                                              SFRAGM48
      NG=0                                                              SFRAGM49
      DO I=1,5                                                          SFRAGM50
        PG(I)=0.0D0                                                     SFRAGM51
        PP(I)=0.0D0                                                     SFRAGM52
      ENDDO                                                             SFRAGM53
      DO 10 I=1,NFLAV                                                   SFRAGM54
        IF( IFLAV(I,1).EQ.0 ) GOTO 10        ! empty line               SFRAGM55
        IF( IFLAV(I,2).NE.0 ) GOTO 10        ! not the original         SFRAGM56
        IF( IFLAV(I,1).EQ.22) THEN           ! ISR photon               SFRAGM57
           NG=NG+1                                                      SFRAGM58
           PG(1)=PG(1)+PV(1,I)                                          SFRAGM59
           PG(2)=PG(2)+PV(2,I)                                          SFRAGM60
           PG(3)=PG(3)+PV(3,I)                                          SFRAGM61
           PG(4)=PG(4)+PV(4,I)                                          SFRAGM62
           KG(NG)=I                                                     SFRAGM63
        ELSE                                 ! other originals          SFRAGM64
           NP=NP+1                                                      SFRAGM65
           PP(1)=PP(1)+PV(1,I)                                          SFRAGM66
           PP(2)=PP(2)+PV(2,I)                                          SFRAGM67
           PP(3)=PP(3)+PV(3,I)                                          SFRAGM68
           PP(4)=PP(4)+PV(4,I)                                          SFRAGM69
           KP(NP)=I                                                     SFRAGM70
        ENDIF                                                           SFRAGM71
10    CONTINUE                                                          SFRAGM72
      PP(5)=PP(4)**2-PP(1)**2-PP(2)**2-PP(3)**2                         SFRAGM73
      PP(5)=DSIGN(1.0D0,PP(5))*DSQRT(DABS(PP(5)))                       SFRAGM74
      PG(5)=PG(4)**2-PG(1)**2-PG(2)**2-PG(3)**2                         SFRAGM75
      PG(5)=DSIGN(1.0D0,PG(5))*DSQRT(DABS(PG(5)))                       SFRAGM76
C*                                                                      SFRAGM77
C*    save beam particles in the first two lines (ALEPH e- ->+z)        SFRAGM78
C*    ==========================================================        SFRAGM79
      K(N+1,1)= 21                                                      SFRAGM80
      K(N+1,2)= 11                                                      SFRAGM81
      K(N+1,3)=  0                                                      SFRAGM82
      K(N+1,4)=  0                                                      SFRAGM83
      K(N+1,5)=  0                                                      SFRAGM84
      P(N+1,1)=  0.0                                                    SFRAGM85
      P(N+1,2)=  0.0                                                    SFRAGM86
      P(N+1,3)=  ECM/2.0D0                                              SFRAGM87
      P(N+1,4)=  ECM/2.0D0                                              SFRAGM88
      P(N+1,5)=  0.0                                                    SFRAGM89
      V(N+1,1)=  0.0                                                    SFRAGM90
      V(N+1,2)=  0.0                                                    SFRAGM91
      V(N+1,3)=  0.0                                                    SFRAGM92
      V(N+1,4)=  0.0                                                    SFRAGM93
      V(N+1,5)=  0.0                                                    SFRAGM94
                                                                        SFRAGM95
      K(N+2,1)= 21                                                      SFRAGM96
      K(N+2,2)=-11                                                      SFRAGM97
      K(N+2,3)=  0                                                      SFRAGM98
      K(N+2,4)=  0                                                      SFRAGM99
      K(N+2,5)=  0                                                      SFRAG100
      P(N+2,1)=  0.0                                                    SFRAG101
      P(N+2,2)=  0.0                                                    SFRAG102
      P(N+2,3)= -ECM/2.0D0                                              SFRAG103
      P(N+2,4)=  ECM/2.0D0                                              SFRAG104
      P(N+2,5)=  0.0                                                    SFRAG105
      V(N+2,1)=  0.0                                                    SFRAG106
      V(N+2,2)=  0.0                                                    SFRAG107
      V(N+2,3)=  0.0                                                    SFRAG108
      V(N+2,4)=  0.0                                                    SFRAG109
      V(N+2,5)=  0.0                                                    SFRAG110
                                                                        SFRAG111
      N=N+2                                                             SFRAG112
C*                                                                      SFRAG113
C*    ISR GAMMAS                                                        SFRAG114
C*    ==========                                                        SFRAG115
      DO 20 I=1,NG                                                      SFRAG116
        N      =  N+1                                                   SFRAG117
        K(N,1) =  1                                                     SFRAG118
        K(N,2) = 22                                                     SFRAG119
        K(N,3) =  0                                                     SFRAG120
        K(N,4) =  0                                                     SFRAG121
        K(N,5) =  0                                                     SFRAG122
        P(N,1) =  PV(1,KG(I))                                           SFRAG123
        P(N,2) =  PV(2,KG(I))                                           SFRAG124
        P(N,3) =  PV(3,KG(I))                                           SFRAG125
        P(N,4) =  PV(4,KG(I))                                           SFRAG126
        P(N,5) =  PV(5,KG(I))                                           SFRAG127
        V(N,1) =  0.0                                                   SFRAG128
        V(N,2) =  0.0                                                   SFRAG129
        V(N,3) =  0.0                                                   SFRAG130
        V(N,4) =  0.0                                                   SFRAG131
        V(N,5) =  0.0                                                   SFRAG132
20    CONTINUE                                                          SFRAG133
C*                                                                      SFRAG134
C*    mediate state: SUSY st                                            SFRAG135
C*    ======================                                            SFRAG136
      N      =  N+1                                                     SFRAG137
      K(N,1) = 11                                                       SFRAG138
      K(N,2) = 79                                                       SFRAG139
      K(N,3) =  0                                                       SFRAG140
      K(N,4) =  N+1                                                     SFRAG141
      K(N,5) =  N+NP                                                    SFRAG142
      P(N,1) =  PP(1)                                                   SFRAG143
      P(N,2) =  PP(2)                                                   SFRAG144
      P(N,3) =  PP(3)                                                   SFRAG145
      P(N,4) =  PP(4)                                                   SFRAG146
      P(N,5) =  PP(5)                                                   SFRAG147
      V(N,1) =  0.0                                                     SFRAG148
      V(N,2) =  0.0                                                     SFRAG149
      V(N,3) =  0.0                                                     SFRAG150
      V(N,4) =  0.0                                                     SFRAG151
      V(N,5) =  0.0                                                     SFRAG152
                                                                        SFRAG153
      NM=N                                                              SFRAG154
C*                                                                      SFRAG155
C*    Store original particles                                          SFRAG156
C*    ========================                                          SFRAG157
      NMOT=0                                                            SFRAG158
      DO 30 I=1,NP                                                      SFRAG159
        N      =  N+1                                                   SFRAG160
        K(N,1) =  1                                                     SFRAG161
        K(N,2) =  IFLAV(KP(I),1)                                        SFRAG162
        K(N,3) =  NM                                                    SFRAG163
        K(N,4) =  0                                                     SFRAG164
        K(N,5) =  0                                                     SFRAG165
        P(N,1) =  PV(1,KP(I))                                           SFRAG166
        P(N,2) =  PV(2,KP(I))                                           SFRAG167
        P(N,3) =  PV(3,KP(I))                                           SFRAG168
        P(N,4) =  PV(4,KP(I))                                           SFRAG169
        P(N,5) =  PV(5,KP(I))                                           SFRAG170
        V(N,1) =  0.0                                                   SFRAG171
        V(N,2) =  0.0                                                   SFRAG172
        V(N,3) =  0.0                                                   SFRAG173
        V(N,4) =  0.0                                                   SFRAG174
        V(N,5) =  0.0                                                   SFRAG175
                                                                        SFRAG176
        NMOT        =NMOT+1                                             SFRAG177
        IMOT(NMOT,1)=KP(I)                                              SFRAG178
        IMOT(NMOT,2)=N                                                  SFRAG179
30    CONTINUE                                                          SFRAG180
C*                                                                      SFRAG181
C*    Other particles, by order                                         SFRAG182
C*    =========================                                         SFRAG183
      JMOT=0                                                            SFRAG184
41    JMOT=JMOT+1                                                       SFRAG185
      IF(JMOT.GT.NMOT) GOTO 40                                          SFRAG186
                                                                        SFRAG187
      DO 42 I=1,NFLAV                                                   SFRAG188
        IF( IFLAV(I,2).NE.IMOT(JMOT,1) ) GOTO 42                        SFRAG189
                                                                        SFRAG190
        N      =  N+1                                                   SFRAG191
        K(N,1) =  1                                                     SFRAG192
        K(N,2) =  IFLAV(I,1)                                            SFRAG193
        K(N,3) =  IMOT(JMOT,2)                                          SFRAG194
        K(N,4) =  0                                                     SFRAG195
        K(N,5) =  0                                                     SFRAG196
        P(N,1) =  PV(1,I)                                               SFRAG197
        P(N,2) =  PV(2,I)                                               SFRAG198
        P(N,3) =  PV(3,I)                                               SFRAG199
        P(N,4) =  PV(4,I)                                               SFRAG200
        P(N,5) =  PV(5,I)                                               SFRAG201
        V(N,1) =  0.0                                                   SFRAG202
        V(N,2) =  0.0                                                   SFRAG203
        V(N,3) =  0.0                                                   SFRAG204
        V(N,4) =  0.0                                                   SFRAG205
        V(N,5) =  0.0                                                   SFRAG206
                                                                        SFRAG207
        IF( K(IMOT(JMOT,2),4).EQ.0 ) THEN                               SFRAG208
          K(IMOT(JMOT,2),1)=11                                          SFRAG209
          K(IMOT(JMOT,2),4)= N                                          SFRAG210
          K(IMOT(JMOT,2),5)= N                                          SFRAG211
        ELSE                                                            SFRAG212
          K(IMOT(JMOT,2),5)=K(IMOT(JMOT,2),5)+1                         SFRAG213
        ENDIF                                                           SFRAG214
                                                                        SFRAG215
        NMOT         = NMOT+1                                           SFRAG216
        IMOT(NMOT,1) = I                                                SFRAG217
        IMOT(NMOT,2) = N                                                SFRAG218
42    CONTINUE                                                          SFRAG219
      GOTO 41                                                           SFRAG220
40    CONTINUE                                                          SFRAG221
C*                                                                      SFRAG222
C*    check the colour string                                           SFRAG223
C*    =======================                                           SFRAG224
      DO 50 I=1,N                                                       SFRAG225
        IF( ABS(K(I,1)).NE. 1 ) GOTO 50                                 SFRAG226
        IF( ABS(K(I,2)).GT.10 ) GOTO 50                                 SFRAG227
                                                                        SFRAG228
        IF( ABS(K(I+1,2)).GT.10 ) THEN                                  SFRAG229
           IFLAG=1                                                      SFRAG230
           WRITE(6,*) 'SFRAGMENT: No colour partner!'                   SFRAG231
        ELSE                                                            SFRAG232
           K(I  ,1)=3                                                   SFRAG233
           K(I  ,4)=10000*(I+1)                                         SFRAG234
           K(I  ,5)=10000*(I+1)                                         SFRAG235
           K(I+1,1)=3                                                   SFRAG236
           K(I+1,4)=10000*I                                             SFRAG237
           K(I+1,5)=10000*I                                             SFRAG238
           QMX=(P(I,4)+P(I+1,4))**2                                     SFRAG239
     &        -(P(I,3)+P(I+1,3))**2                                     SFRAG240
     &        -(P(I,2)+P(I+1,2))**2                                     SFRAG241
     &        -(P(I,1)+P(I+1,1))**2                                     SFRAG242
           QMX=SIGN(1.0,QMX)*SQRT(ABS(QMX))                             SFRAG243
           CALL LUSHOW(I,I+1,QMX)                                       SFRAG244
        ENDIF                                                           SFRAG245
50    CONTINUE                                                          SFRAG246
C*                                                                      SFRAG247
C*    Finally, LUEXEC                                                   SFRAG248
C*    ===============                                                   SFRAG249
      CALL LUEXEC                                                       SFRAG250
      END                                                               SFRAG251
      SUBROUTINE SUCARD                                                 SUCARD 2
C---------------------------------------------------------------------- SUCARD 3
C!  -                                                                   SUCARD 4
C!                                                                      SUCARD 5
C!   Author   :- Y. Gao                 1-FEB-1996                      SUCARD 6
C!                                                                      SUCARD 7
C!   Inputs:                                                            SUCARD 8
C!        - none                                                        SUCARD 9
C!                                                                      SUCARD10
C!   Outputs:                                                           SUCARD11
C!        - none                                                        SUCARD12
C!                                                                      SUCARD13
C!   Libraries required:                                                SUCARD14
C!                                                                      SUCARD15
C!   Description                                                        SUCARD16
C!   ===========                                                        SUCARD17
C!   set the SUSY parameters by data cards. Modified from the original  SUCARD18
C!   SCARDS,SUSANA,and part of SFERMION                                 SUCARD19
C?                                                                      SUCARD20
C!======================================================================SUCARD21
      Implicit None                                                     SUCARD22
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
      COMMON /BCS/   IW(LBCS )                                          BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
********************* start of commons of SUSYGEN **********************SUSCOM 2
      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rfmsq,rfmstopl,rfmstopr, SUSCOM 3
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)        SUSCOM 4
      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,                    SUSCOM 5
     +        rfmsq,rfmstopl,rfmstopr,                                  SUSCOM 6
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan           SUSCOM 7
                                                                        SUSCOM 8
      real*8  gmaum,gmaur,gm0,gtanb,gatri                               SUSCOM 9
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM10
      common/steer/gmaum,gmaur,gm0,gtanb,gatri                          SUSCOM11
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM12
                                                                        SUSCOM13
      Integer modes,MIX                                                 SUSCOM14
      common/mds/modes,MIX                                              SUSCOM15
                                                                        SUSCOM16
      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota             SUSCOM17
      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota       SUSCOM18
                                                                        SUSCOM19
      logical wrt,scan,lepi                                             SUSCOM20
      common/str/wrt,scan,lepi                                          SUSCOM21
                                                                        SUSCOM22
      real*8  flum,ecm,s,roots,T,Q,Q2,EN                                SUSCOM23
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        SUSCOM24
                                                                        SUSCOM25
      real*8  QK                                                        SUSCOM26
      COMMON/ISR/ QK(4)                                                 SUSCOM27
                                                                        SUSCOM28
      real erad,srad                                                    SUSCOM29
      common/srada/erad(100),srad(100)                                  SUSCOM30
                                                                        SUSCOM31
      integer  idbg,igener,irad                                         SUSCOM32
      COMMON /CONST/ idbg,igener,irad                                   SUSCOM33
                                                                        SUSCOM34
      integer  index,index1,index2,nevt                                 SUSCOM35
      COMMON/INDEXX/index,index1,index2,nevt                            SUSCOM36
                                                                        SUSCOM37
      real*8   fmpr1,fmpr2,XCROST,APRO                                  SUSCOM38
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO                             SUSCOM39
                                                                        SUSCOM40
      real*8   cosphimix,facqcd,fgama,spartmas,ratq                     SUSCOM41
      common/mixings/cosphimix,facqcd,fgama,spartmas,ratq               SUSCOM42
                                                                        SUSCOM43
      real*8   FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,    SUSCOM44
     +         FLC,FRC,gms,echar                                        SUSCOM45
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   SUSCOM46
     +FLC(12),FRC(12),gms(12),echar(12)                                 SUSCOM47
                                                                        SUSCOM48
      real*8      TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM49
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM50
                                                                        SUSCOM51
      real*8   fmal,fmar,ratqa,fgamc,fgamcr,cosmi                       SUSCOM52
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)   SUSCOM53
     +,cosmi(12)                                                        SUSCOM54
                                                                        SUSCOM55
      real*4 phimix,stop1                                               SUSCOM56
      common/stopmix/phimix,stop1                                       SUSCOM57
                                                                        SUSCOM58
      real*8   xgaug,xeta                                               SUSCOM59
      COMMON/XCROS/xgaug(8),xeta(8)                                     SUSCOM60
                                                                        SUSCOM61
      integer  ispa,kl,klap,idecs                                       SUSCOM62
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         SUSCOM63
                                                                        SUSCOM64
      Integer  idecsel                                                  SUSCOM65
      common/decsel/idecsel(18)                                         SUSCOM66
                                                                        SUSCOM67
      real*8   brsum,brsuma                                             SUSCOM68
      common/brsum/ brsum(5,6,6),brsuma(6)                              SUSCOM69
                                                                        SUSCOM70
      real*8   fms,fmi,fmk,fml1,fml2,etai,etak,brspa                    SUSCOM71
     +        ,brgaug,fmelt,fmert,fmelu,fmeru                           SUSCOM72
      Integer  lind                                                     SUSCOM73
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     SUSCOM74
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                SUSCOM75
                                                                        SUSCOM76
      Real*4   zr,was,esa                                               SUSCOM77
      Real*8   VOIJL,VOIJR,gfir,gfil                                    SUSCOM78
      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),                              SUSCOM79
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)                         SUSCOM80
                                                                        SUSCOM81
      real*8   OIJL,OIJR,V,U,FM,ETA                                     SUSCOM82
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)     SUSCOM83
                                                                        SUSCOM84
      real*8   oijlp,oijrp                                              SUSCOM85
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)                               SUSCOM86
                                                                        SUSCOM87
      Integer       ILOOP                                               SUSCOM88
      COMMON/SFLOOP/ILOOP                                               SUSCOM89
                                                                        SUSCOM90
      Real*8   PV                                                       SUSCOM91
      Integer  IFLAV,NFLAV                                              SUSCOM92
      COMMON /PARTC / PV(5,20),IFLAV(20,2),NFLAV                        SUSCOM93
                                                                        SUSCOM94
      real*8   gw,widfl,widfr,fmeltw,fmertw,fmeluw,fmeruw,gent,gentl    SUSCOM95
      integer  linda                                                    SUSCOM96
      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw, SUSCOM97
     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)                       SUSCOM98
********************* end  of commons of SUSYGEN ***********************SUSCOM99
      INTEGER    L1MST,L1PAR                                            LUNDCOM2
     &          ,L2PAR,L2PARF                                           LUNDCOM3
     &          ,LJNPAR                                                 LUNDCOM4
      PARAMETER (L1MST=200, L1PAR=200)                                  LUNDCOM5
      PARAMETER (L2PAR=500, L2PARF=2000)                                LUNDCOM6
      PARAMETER (LJNPAR=4000)                                           LUNDCOM7
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUNDCOM8
      INTEGER         MSTU,MSTJ                                         LUNDCOM9
      REAL            PARU,PARJ                                         LUNDCO10
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUNDCO11
      INTEGER         KCHG                                              LUNDCO12
      REAL            PMAS,PARF,VCKM                                    LUNDCO13
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCO14
     &                KFDP(L2PARF,5)                                    LUNDCO15
      INTEGER         MDCY,MDME,KFDP                                    LUNDCO16
      REAL            BRAT                                              LUNDCO17
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCO18
      CHARACTER*8     CHAF                                              LUNDCO19
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUNDCO20
      INTEGER         N7LU,K7LU                                         LUNDCO21
      REAL            P7LU,V7LU                                         LUNDCO22
      Integer       mproc                                               SUPROC 2
      Parameter    (mproc=35)                                           SUPROC 3
      Integer       iproc,iprod,ipar1,ipar2,ihand,isusy,iopen,ievnt     SUPROC 4
      Real          xproc                                               SUPROC 5
      common/SUPROC/iproc(mproc),iprod(mproc)                           SUPROC 6
     &             ,ipar1(mproc),ipar2(mproc)                           SUPROC 7
     &             ,ihand(mproc),isusy(mproc)                           SUPROC 8
     &             ,iopen(mproc),ievnt(mproc)                           SUPROC 9
     &             ,xproc(mproc)                                        SUPROC10
C -   LOCAL                                                             SUCARD27
      Real*8        one_third,two_thirds,one_half,one,zero              SUCARD28
      Parameter    (one_third =1.0d0/3.0d0                              SUCARD29
     &             ,two_thirds=one_third+one_third                      SUCARD30
     &             ,one_half  =0.5d0                                    SUCARD31
     &             ,one       =1.0d0                                    SUCARD32
     &             ,zero      =0.0d0)                                   SUCARD33
      Real*8        Tev,Pev                                             SUCARD34
      Parameter    (Tev       =1.0D+03)                                 SUCARD35
      Parameter    (Pev       =1.0D+06)                                 SUCARD36
                                                                        SUCARD37
      Integer       NLINK,NAMIND,IDBNK,NMBNK                            SUCARD38
      External      NLINK,NAMIND                                        SUCARD39
                                                                        SUCARD40
      Real*8        SIN2W,T3(12)                                        SUCARD41
      Integer       I                                                   SUCARD42
C                                                                       SUCARD43
C     standard model constants                                          SUCARD44
C     ========================                                          SUCARD45
C -   sinW                                                              SUCARD46
      IDBNK = NLINK('SW2 ',0)                                           SUCARD47
      if( IDBNK.gt.0 ) then                                             SUCARD48
        SIN2W = RW(IDBNK+1)                                             SUCARD49
      else                                                              SUCARD50
        SIN2W = 0.231243d0                                              SUCARD51
      endif                                                             SUCARD52
C -   coupling                                                          SUCARD53
      IDBNK = NLINK('ALPH',0)                                           SUCARD54
      if( IDBNK.gt.0 ) then                                             SUCARD55
        ALPHA = RW(IDBNK+1)                                             SUCARD56
      else                                                              SUCARD57
        ALPHA = 1.0d0/128.0d0                                           SUCARD58
      endif                                                             SUCARD59
                                                                        SUCARD60
      PI    = 3.1415926535d0                                            SUCARD61
      TWOPI = PI+PI                                                     SUCARD62
      e2    = 4.0d0*PI*ALPHA                                            SUCARD63
      SINW  = DSQRT(SIN2W)                                              SUCARD64
      COSW  = DSQRT(1.0d0-SIN2W)                                        SUCARD65
      G2    = e2/SIN2W                                                  SUCARD66
      do I=1,12                                                         SUCARD67
        IF(MOD(I-1,4).EQ.0) ECHAR(I) = two_thirds                       SUCARD68
        IF(MOD(I-1,4).EQ.1) ECHAR(I) =-one_third                        SUCARD69
        IF(MOD(I-1,4).EQ.2) ECHAR(I) = zero                             SUCARD70
        IF(MOD(I-1,4).EQ.3) ECHAR(I) =-one                              SUCARD71
                                                                        SUCARD72
        IF(MOD(I-1,2).EQ.0) T3(I) = one_half                            SUCARD73
        IF(MOD(I-1,2).EQ.1) T3(I) =-one_half                            SUCARD74
                                                                        SUCARD75
        FLC(I) = T3(I)-ECHAR(I)*SIN2W                                   SUCARD76
        FRC(I) =-ECHAR(I)*SIN2W                                         SUCARD77
      enddo                                                             SUCARD78
                                                                        SUCARD79
C -   mass and width should be consistent with LUND                     SUCARD80
C -   z0 mass & width                                                   SUCARD81
                                                                        SUCARD82
      FMZ     = PMAS(23,1)                                              SUCARD83
      GAMMAZ  = PMAS(23,2)                                              SUCARD84
      FMW     = PMAS(24,1)                                              SUCARD85
      GAMMAW  = PMAS(24,2)                                              SUCARD86
      GMS( 4) = PMAS(11,1)  ! e                                         SUCARD87
      GMS( 3) = PMAS(12,1)  ! nu_e                                      SUCARD88
      GMS( 8) = PMAS(13,1)  ! mu                                        SUCARD89
      GMS( 7) = PMAS(14,1)  ! nu_mu                                     SUCARD90
      GMS(12) = PMAS(15,1)  ! tau                                       SUCARD91
      GMS(11) = PMAS(16,1)  ! nu_tau                                    SUCARD92
      GMS( 2) = PMAS( 1,1)  ! d                                         SUCARD93
      GMS( 1) = PMAS( 2,1)  ! u                                         SUCARD94
      GMS( 6) = PMAS( 3,1)  ! s                                         SUCARD95
      GMS( 5) = PMAS( 4,1)  ! c                                         SUCARD96
      GMS(10) = PMAS( 5,1)  ! b                                         SUCARD97
      GMS( 9) = PMAS( 6,1)  ! t                                         SUCARD98
C                                                                       SUCARD99
C     switch on/off virtual Z and W decay channels                      SUCAR100
C     ============================================                      SUCAR101
      IDBNK=NLINK('DESC',0)                                             SUCAR102
      if( IDBNK.gt.0 ) then                                             SUCAR103
        do i=1,18                                                       SUCAR104
        idecsel(i) = IW(IDBNK+i)                                        SUCAR105
        enddo                                                           SUCAR106
      else                                                              SUCAR107
        do i=1,18                                                       SUCAR108
        idecsel(i) = 1                                                  SUCAR109
        enddo                                                           SUCAR110
      endif                                                             SUCAR111
C                                                                       SUCAR112
C     SUSY parameters                                                   SUCAR113
C     ===============                                                   SUCAR114
      IDBNK=NLINK('SUPA',0)                                             SUCAR115
      if( IDBNK.gt.0 ) then                                             SUCAR116
        rgmaum = RW(IDBNK+1)                                            SUCAR117
        rgmaur = RW(IDBNK+2)                                            SUCAR118
        rgm0   = RW(IDBNK+3)                                            SUCAR119
        rgtanb = RW(IDBNK+4)                                            SUCAR120
        rgatri = RW(IDBNK+5)                                            SUCAR121
      else                                                              SUCAR122
        rgmaum = 90.0                                                   SUCAR123
        rgmaur = 90.0                                                   SUCAR124
        rgm0   = 90.0                                                   SUCAR125
        rgtanb =  4.0                                                   SUCAR126
        rgatri =  0.0                                                   SUCAR127
      endif                                                             SUCAR128
C -   gluino mass                                                       SUCAR129
      IDBNK=NLINK('MGLU',0)                                             SUCAR130
      if( IDBNK.gt.0 ) then                                             SUCAR131
        rfmglu = RW(IDBNK+1)                                            SUCAR132
      else                                                              SUCAR133
        rfmglu = Tev                                                    SUCAR134
      endif                                                             SUCAR135
C -   the masses setup here are useless, because we'll set them later   SUCAR136
      rfmstopl = Tev                                                    SUCAR137
      rfmstopr = Tev                                                    SUCAR138
      rfmsell  = Tev                                                    SUCAR139
      rfmselr  = Tev                                                    SUCAR140
      rfmsnu   = Tev                                                    SUCAR141
                                                                        SUCAR142
       gmaum   = dble(rgmaum)                                           SUCAR143
       gmaur   = dble(rgmaur)                                           SUCAR144
       gm0     = dble(rgm0)                                             SUCAR145
       gtanb   = dble(rgtanb)                                           SUCAR146
       gatri   = dble(rgatri)                                           SUCAR147
       fmsq    = dble(rfmsq)                                            SUCAR148
       fmstopl = dble(rfmstopl)                                         SUCAR149
       fmstopr = dble(rfmstopr)                                         SUCAR150
       fmsell  = dble(rfmsell)                                          SUCAR151
       fmselr  = dble(rfmselr)                                          SUCAR152
       fmsnu   = dble(rfmsnu)                                           SUCAR153
       fmglu   = dble(rfmglu)                                           SUCAR154
                                                                        SUCAR155
       ecm     = dble(recm)                                             SUCAR156
       flum    = dble(rflum)                                            SUCAR157
                                                                        SUCAR158
      FMGAUG = gmaum                                                    SUCAR159
      FMR    = gmaur                                                    SUCAR160
      FM0    = gm0                                                      SUCAR161
      TANB   = gtanb                                                    SUCAR162
      COSB   = 1.0D0/DSQRT(1.0D0+TANB**2)                               SUCAR163
      SINB   = TANB*COSB                                                SUCAR164
      ATRI   = gatri                                                    SUCAR165
C                                                                       SUCAR166
C     the masses of S-particles                                         SUCAR167
C     =========================                                         SUCAR168
C -   sleptons                                                          SUCAR169
      IDBNK=NLINK('MLSL',0)                                             SUCAR170
      if( IDBNK.gt.0 ) then                                             SUCAR171
        fmal( 4) = RW(IDBNK+1)                                          SUCAR172
        fmal( 3) = RW(IDBNK+2)                                          SUCAR173
        fmal( 8) = RW(IDBNK+3)                                          SUCAR174
        fmal( 7) = RW(IDBNK+4)                                          SUCAR175
        fmal(12) = RW(IDBNK+5)                                          SUCAR176
        fmal(11) = RW(IDBNK+6)                                          SUCAR177
      else                                                              SUCAR178
        fmal( 4) = Tev                                                  SUCAR179
        fmal( 3) = Tev                                                  SUCAR180
        fmal( 8) = Tev                                                  SUCAR181
        fmal( 7) = Tev                                                  SUCAR182
        fmal(12) = Tev                                                  SUCAR183
        fmal(11) = Tev                                                  SUCAR184
      endif                                                             SUCAR185
      IDBNK=NLINK('MRSL',0)                                             SUCAR186
      if( IDBNK.gt.0 ) then                                             SUCAR187
        fmar( 4) = RW(IDBNK+1)                                          SUCAR188
        fmar( 3) = RW(IDBNK+2)                                          SUCAR189
        fmar( 8) = RW(IDBNK+3)                                          SUCAR190
        fmar( 7) = RW(IDBNK+4)                                          SUCAR191
        fmar(12) = RW(IDBNK+5)                                          SUCAR192
        fmar(11) = RW(IDBNK+6)                                          SUCAR193
      else                                                              SUCAR194
        fmar( 4) = Tev                                                  SUCAR195
        fmar( 3) = Pev                                                  SUCAR196
        fmar( 8) = Tev                                                  SUCAR197
        fmar( 7) = Pev                                                  SUCAR198
        fmar(12) = Tev                                                  SUCAR199
        fmar(11) = Pev                                                  SUCAR200
      endif                                                             SUCAR201
C -   squarks                                                           SUCAR202
      IDBNK=NLINK('MLSQ',0)                                             SUCAR203
      if( IDBNK.gt.0 ) then                                             SUCAR204
        fmal( 2) = RW(IDBNK+1)                                          SUCAR205
        fmal( 1) = RW(IDBNK+2)                                          SUCAR206
        fmal( 6) = RW(IDBNK+3)                                          SUCAR207
        fmal( 5) = RW(IDBNK+4)                                          SUCAR208
        fmal(10) = RW(IDBNK+5)                                          YGAO0011
        fmal( 9) = RW(IDBNK+6)                                          YGAO0012
      else                                                              SUCAR211
        fmal( 2) = Tev                                                  SUCAR212
        fmal( 1) = Tev                                                  SUCAR213
        fmal( 6) = Tev                                                  SUCAR214
        fmal( 5) = Tev                                                  SUCAR215
        fmal(10) = Tev                                                  YGAO0013
        fmal( 9) = Tev                                                  SUCAR216
      endif                                                             SUCAR218
      IDBNK=NLINK('MRSQ',0)                                             SUCAR219
      if( IDBNK.gt.0 ) then                                             SUCAR220
        fmar( 2) = RW(IDBNK+1)                                          SUCAR221
        fmar( 1) = RW(IDBNK+2)                                          SUCAR222
        fmar( 6) = RW(IDBNK+3)                                          SUCAR223
        fmar( 5) = RW(IDBNK+4)                                          SUCAR224
        fmar(10) = RW(IDBNK+5)                                          YGAO0014
        fmar( 9) = RW(IDBNK+6)                                          YGAO0015
      else                                                              SUCAR227
        fmar( 2) = Tev                                                  SUCAR228
        fmar( 1) = Tev                                                  SUCAR229
        fmar( 6) = Tev                                                  SUCAR230
        fmar( 5) = Tev                                                  SUCAR231
        fmar(10) = Tev                                                  YGAO0016
        fmar( 9) = Tev                                                  SUCAR232
      endif                                                             SUCAR234
C                                                                       SUCAR235
C     which mode                                                        SUCAR236
C     ==========                                                        SUCAR237
      IDBNK=NLINK('MODE',0)                                             SUCAR238
      if( IDBNK.gt.0 ) then                                             SUCAR239
        modes=IW(IDBNK+1)                                               SUCAR240
      else                                                              SUCAR241
        modes=1                                                         SUCAR242
      endif                                                             SUCAR243
C                                                                       SUCAR244
C     mixing                                                            SUCAR245
C     ======                                                            SUCAR246
      IDBNK=NLINK('MIX ',0)                                             SUCAR247
      if( IDBNK.gt.0 ) then                                             SUCAR248
        MIX = IW(IDBNK+1)                                               SUCAR249
      else                                                              SUCAR250
        MIX = 0                                                         SUCAR251
      endif                                                             SUCAR252
                                                                        SUCAR253
      IDBNK=NLINK('MIX3',0)                                             SUCAR254
      if( IDBNK.gt.0 ) then                                             SUCAR255
        phimix = RW(IDBNK+1)                                            SUCAR256
        stop1  = RW(IDBNK+2)                                            SUCAR257
      else                                                              SUCAR258
        phimix = 0.0                                                    SUCAR259
        stop1  = 0.0                                                    SUCAR260
      endif                                                             SUCAR261
C                                                                       SUCAR262
C     if SCAN? it is not allowed to scan                                SUCAR263
C     ==================================                                SUCAR264
      scan      = .false.                                               SUCAR265
      rvscan(1) =  3.0                                                  SUCAR266
      rvscan(2) = 80.0                                                  SUCAR267
      rvscan(3) =100.0                                                  SUCAR268
      rvscan(4) =  3.0                                                  SUCAR269
      rvscan(5) = 80.0                                                  SUCAR270
      rvscan(6) =100.0                                                  SUCAR271
C                                                                       SUCAR272
C     use LEPI limit                                                    SUCAR273
C     ==============                                                    SUCAR274
      IDBNK=NLINK('LEPI',0)                                             SUCAR275
      if( IDBNK.gt.0 ) then                                             SUCAR276
        lepi = IW(IDBNK+1).gt.0                                         SUCAR277
      else                                                              SUCAR278
        lepi = .false.                                                  SUCAR279
      endif                                                             SUCAR280
C                                                                       SUCAR281
C     ISR                                                               SUCAR282
C     ===                                                               SUCAR283
      IDBNK=NLINK('IRAD',0)                                             SUCAR284
      if( IDBNK.gt.0 ) then                                             SUCAR285
        irad = IW(IDBNK+1)                                              SUCAR286
      else                                                              SUCAR287
        irad = 1                                                        SUCAR288
      endif                                                             SUCAR289
C                                                                       SUCAR290
C     don't write to FOR012                                             SUCAR291
C     =====================                                             SUCAR292
      wrt = .false.                                                     SUCAR293
C                                                                       SUCAR294
C     we have to always set igener to 1                                 SUCAR295
C     =================================                                 SUCAR296
      igener=1                                                          SUCAR297
C                                                                       SUCAR298
C     PROC: which process to be generated                               SUCAR299
C     ===================================                               SUCAR300
      do I=1,mproc                                                      SUCAR301
        iopen(I)=1                                                      SUCAR302
      enddo                                                             SUCAR303
      NMBNK=NAMIND('PROC')                                              SUCAR304
      IDBNK=NMBNK+1                                                     SUCAR305
80    IDBNK=IW(IDBNK-1)                                                 SUCAR306
      if( IDBNK.gt.0 ) then                                             SUCAR307
        I       =IW(IDBNK-2)                                            SUCAR308
        iopen(I)=IW(IDBNK+1)                                            SUCAR309
        goto 80                                                         SUCAR310
      endif                                                             SUCAR311
      END                                                               SUCAR312
      SUBROUTINE SUMSSM(NP,IG,JG,XC)                                    SUMSSM 2
C---------------------------------------------------------------------- SUMSSM 3
C!  -                                                                   SUMSSM 4
C!                                                                      SUMSSM 5
C!   Author   :- Y. Gao                 2-FEB-1996                      SUMSSM 6
C!                                                                      SUMSSM 7
C!   Inputs:                                                            SUMSSM 8
C!        - NP process number                                           SUMSSM 9
C!        - IG calculate cross-section & generate IG events             SUMSSM10
C!                                                                      SUMSSM11
C!   Outputs:                                                           SUMSSM12
C!        - JG number of nevets generated                               SUMSSM13
C!        - XC cross-section                                            SUMSSM14
C!                                                                      SUMSSM15
C!   Libraries required:                                                SUMSSM16
C!                                                                      SUMSSM17
C!   Description                                                        SUMSSM18
C!   ===========                                                        SUMSSM19
C!   modified from MSSMSUSY                                             SUMSSM20
C?                                                                      SUMSSM21
C!======================================================================SUMSSM22
      Implicit None                                                     SUMSSM23
********************* start of commons of SUSYGEN **********************SUSCOM 2
      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rfmsq,rfmstopl,rfmstopr, SUSCOM 3
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)        SUSCOM 4
      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,                    SUSCOM 5
     +        rfmsq,rfmstopl,rfmstopr,                                  SUSCOM 6
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan           SUSCOM 7
                                                                        SUSCOM 8
      real*8  gmaum,gmaur,gm0,gtanb,gatri                               SUSCOM 9
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM10
      common/steer/gmaum,gmaur,gm0,gtanb,gatri                          SUSCOM11
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM12
                                                                        SUSCOM13
      Integer modes,MIX                                                 SUSCOM14
      common/mds/modes,MIX                                              SUSCOM15
                                                                        SUSCOM16
      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota             SUSCOM17
      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota       SUSCOM18
                                                                        SUSCOM19
      logical wrt,scan,lepi                                             SUSCOM20
      common/str/wrt,scan,lepi                                          SUSCOM21
                                                                        SUSCOM22
      real*8  flum,ecm,s,roots,T,Q,Q2,EN                                SUSCOM23
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        SUSCOM24
                                                                        SUSCOM25
      real*8  QK                                                        SUSCOM26
      COMMON/ISR/ QK(4)                                                 SUSCOM27
                                                                        SUSCOM28
      real erad,srad                                                    SUSCOM29
      common/srada/erad(100),srad(100)                                  SUSCOM30
                                                                        SUSCOM31
      integer  idbg,igener,irad                                         SUSCOM32
      COMMON /CONST/ idbg,igener,irad                                   SUSCOM33
                                                                        SUSCOM34
      integer  index,index1,index2,nevt                                 SUSCOM35
      COMMON/INDEXX/index,index1,index2,nevt                            SUSCOM36
                                                                        SUSCOM37
      real*8   fmpr1,fmpr2,XCROST,APRO                                  SUSCOM38
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO                             SUSCOM39
                                                                        SUSCOM40
      real*8   cosphimix,facqcd,fgama,spartmas,ratq                     SUSCOM41
      common/mixings/cosphimix,facqcd,fgama,spartmas,ratq               SUSCOM42
                                                                        SUSCOM43
      real*8   FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,    SUSCOM44
     +         FLC,FRC,gms,echar                                        SUSCOM45
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   SUSCOM46
     +FLC(12),FRC(12),gms(12),echar(12)                                 SUSCOM47
                                                                        SUSCOM48
      real*8      TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM49
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM50
                                                                        SUSCOM51
      real*8   fmal,fmar,ratqa,fgamc,fgamcr,cosmi                       SUSCOM52
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)   SUSCOM53
     +,cosmi(12)                                                        SUSCOM54
                                                                        SUSCOM55
      real*4 phimix,stop1                                               SUSCOM56
      common/stopmix/phimix,stop1                                       SUSCOM57
                                                                        SUSCOM58
      real*8   xgaug,xeta                                               SUSCOM59
      COMMON/XCROS/xgaug(8),xeta(8)                                     SUSCOM60
                                                                        SUSCOM61
      integer  ispa,kl,klap,idecs                                       SUSCOM62
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         SUSCOM63
                                                                        SUSCOM64
      Integer  idecsel                                                  SUSCOM65
      common/decsel/idecsel(18)                                         SUSCOM66
                                                                        SUSCOM67
      real*8   brsum,brsuma                                             SUSCOM68
      common/brsum/ brsum(5,6,6),brsuma(6)                              SUSCOM69
                                                                        SUSCOM70
      real*8   fms,fmi,fmk,fml1,fml2,etai,etak,brspa                    SUSCOM71
     +        ,brgaug,fmelt,fmert,fmelu,fmeru                           SUSCOM72
      Integer  lind                                                     SUSCOM73
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     SUSCOM74
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                SUSCOM75
                                                                        SUSCOM76
      Real*4   zr,was,esa                                               SUSCOM77
      Real*8   VOIJL,VOIJR,gfir,gfil                                    SUSCOM78
      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),                              SUSCOM79
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)                         SUSCOM80
                                                                        SUSCOM81
      real*8   OIJL,OIJR,V,U,FM,ETA                                     SUSCOM82
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)     SUSCOM83
                                                                        SUSCOM84
      real*8   oijlp,oijrp                                              SUSCOM85
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)                               SUSCOM86
                                                                        SUSCOM87
      Integer       ILOOP                                               SUSCOM88
      COMMON/SFLOOP/ILOOP                                               SUSCOM89
                                                                        SUSCOM90
      Real*8   PV                                                       SUSCOM91
      Integer  IFLAV,NFLAV                                              SUSCOM92
      COMMON /PARTC / PV(5,20),IFLAV(20,2),NFLAV                        SUSCOM93
                                                                        SUSCOM94
      real*8   gw,widfl,widfr,fmeltw,fmertw,fmeluw,fmeruw,gent,gentl    SUSCOM95
      integer  linda                                                    SUSCOM96
      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw, SUSCOM97
     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)                       SUSCOM98
********************* end  of commons of SUSYGEN ***********************SUSCOM99
      Integer       mproc                                               SUPROC 2
      Parameter    (mproc=35)                                           SUPROC 3
      Integer       iproc,iprod,ipar1,ipar2,ihand,isusy,iopen,ievnt     SUPROC 4
      Real          xproc                                               SUPROC 5
      common/SUPROC/iproc(mproc),iprod(mproc)                           SUPROC 6
     &             ,ipar1(mproc),ipar2(mproc)                           SUPROC 7
     &             ,ihand(mproc),isusy(mproc)                           SUPROC 8
     &             ,iopen(mproc),ievnt(mproc)                           SUPROC 9
     &             ,xproc(mproc)                                        SUPROC10
      Integer       NP,IG,JG                                            SUMSSM26
      Real          XC                                                  SUMSSM27
      Integer       l,k                                                 SUMSSM28
      Real*8        f                                                   SUMSSM29
                                                                        SUMSSM30
      Integer       l_np                                                SUMSSM31
      Real          l_xc                                                SUMSSM32
      Data          l_np,l_xc/0,0.0/                                    SUMSSM33
                                                                        SUMSSM34
      Real*8        Shat,EBEAM                                          SUMSSM35
      Real*8        SIGMA,ssmass                                        SUMSSM36
      External      SIGMA,ssmass                                        SUMSSM37
      SAVE                                                              SUMSSM38
C                                                                       SUMSSM39
C     check if it is the same as the last one                           SUMSSM40
C     =======================================                           SUMSSM41
      if( NP.eq.l_np ) then                                             SUMSSM42
        XC = l_xc                                                       SUMSSM43
        goto 40                                                         SUMSSM44
      endif                                                             SUMSSM45
C                                                                       SUMSSM46
C     get the index and hand                                            SUMSSM47
C     ======================                                            SUMSSM48
      index  = iprod(NP)                                                SUMSSM49
      index1 = ipar1(NP)                                                SUMSSM50
      index2 = ipar2(NP)                                                SUMSSM51
      l      = ihand(NP)                                                SUMSSM52
      k      = isusy(NP)                                                SUMSSM53
C                                                                       SUMSSM54
C     QCD factor                                                        SUMSSM55
C     ==========                                                        SUMSSM56
      if( NP.le.23 ) then                                               SUMSSM57
        f=0.0d0                                                         SUMSSM58
      else                                                              SUMSSM59
        f=1.0d0                                                         SUMSSM60
      endif                                                             SUMSSM61
C                                                                       SUMSSM62
C     choose the specific process                                       SUMSSM63
C     ===========================                                       SUMSSM64
      s    =ecm*ecm                                                     SUMSSM65
      roots=sqrt(s)                                                     SUMSSM66
      EBEAM=ecm/2.0d0                                                   SUMSSM67
      Shat =ecm*ecm                                                     SUMSSM68
      JG   =0                                                           SUMSSM69
      XC   =0.0                                                         SUMSSM70
C                                                                       SUMSSM71
C     calculate cross-sections                                          SUMSSM72
C     ========================                                          SUMSSM73
      if( index .eq. 1 ) then                                           SUMSSM74
C                                                                       SUMSSM75
C       NEUTRALINOS                                                     SUMSSM76
C       ------------                                                    SUMSSM77
        fmpr1=ssmass(index1)                                            SUMSSM78
        fmpr2=ssmass(index2)                                            SUMSSM79
        if(fmpr1+fmpr2.gt.ECM)  goto 50                                 SUMSSM80
        if(irad.eq.0) then                                              SUMSSM81
          XC = SIGMA(Shat)                                              SUMSSM82
        else                                                            SUMSSM83
          call REMT1(EBEAM,SIGMA)                                       SUMSSM84
          XC = xcrost                                                   SUMSSM85
        endif                                                           SUMSSM86
                                                                        SUMSSM87
      elseif( index.eq.2 ) then                                         SUMSSM88
C                                                                       SUMSSM89
C       CHARGINOS                                                       SUMSSM90
C       ---------                                                       SUMSSM91
        fmpr1=ssmass(index1)                                            SUMSSM92
        fmpr2=ssmass(index2)                                            SUMSSM93
        if(fmpr1+fmpr2.gt.ECM) goto 50                                  SUMSSM94
        if(irad.eq.0) then                                              SUMSSM95
          XC = SIGMA(Shat)                                              SUMSSM96
        else                                                            SUMSSM97
          call REMT1(EBEAM,SIGMA)                                       SUMSSM98
          XC = xcrost                                                   SUMSSM99
        endif                                                           SUMSS100
                                                                        SUMSS101
      elseif( index.eq.3 ) then                                         SUMSS102
C                                                                       SUMSS103
C       SPARTICLES                                                      SUMSS104
C       ----------                                                      SUMSS105
        fmpr1=ssmass(index1)                                            SUMSS106
        fmpr2=ssmass(index2)                                            SUMSS107
        if(fmpr1+fmpr2.gt.ECM) goto 50                                  SUMSS108
                                                                        SUMSS109
        spartmas=fmpr1                                                  SUMSS110
        ratq=ratqa(k)                                                   SUMSS111
        facqcd=f                                                        SUMSS112
        if(l.eq.1)then                                                  SUMSS113
          fgama=fgamc(k)                                                SUMSS114
          cosphimix=1.d0                                                SUMSS115
        else                                                            SUMSS116
          fgama=fgamcr(k)                                               SUMSS117
          cosphimix=0.                                                  SUMSS118
        endif                                                           SUMSS119
                                                                        SUMSS120
        if(irad.eq.0) then                                              SUMSS121
          XC = SIGMA(Shat)                                              SUMSS122
        else                                                            SUMSS123
          call REMT1(EBEAM,SIGMA)                                       SUMSS124
          XC = xcrost                                                   SUMSS125
        endif                                                           SUMSS126
      endif                                                             SUMSS127
C                                                                       SUMSS128
C     generate events                                                   SUMSS129
C     ===============                                                   SUMSS130
40    continue                                                          SUMSS131
      nevt = IG                                                         SUMSS132
      call MSSM_GENE                                                    SUMSS133
      JG   = IG                                                         SUMSS134
                                                                        SUMSS135
      ievnt(NP) = ievnt(NP)+IG                                          SUMSS136
                                                                        SUMSS137
50    continue                                                          SUMSS138
      l_np = NP                                                         SUMSS139
      l_xc = XC                                                         SUMSS140
      end                                                               SUMSS141
      SUBROUTINE SUPART                                                 SUPART 2
C---------------------------------------------------------------------- SUPART 3
C!  -                                                                   SUPART 4
C!                                                                      SUPART 5
C!   Author   :- Y. Gao                 1-FEB-1996                      SUPART 6
C!                                                                      SUPART 7
C!   Inputs:                                                            SUPART 8
C!        - none                                                        SUPART 9
C!                                                                      SUPART10
C!   Outputs:                                                           SUPART11
C!        - none                                                        SUPART12
C!                                                                      SUPART13
C!   Libraries required:                                                SUPART14
C!                                                                      SUPART15
C!   Description                                                        SUPART16
C!   ===========                                                        SUPART17
C!   set the SUSY-particle name, charge etc                             SUPART18
C?                                                                      SUPART19
C!======================================================================SUPART20
      Implicit None                                                     SUPART21
      INTEGER    L1MST,L1PAR                                            LUNDCOM2
     &          ,L2PAR,L2PARF                                           LUNDCOM3
     &          ,LJNPAR                                                 LUNDCOM4
      PARAMETER (L1MST=200, L1PAR=200)                                  LUNDCOM5
      PARAMETER (L2PAR=500, L2PARF=2000)                                LUNDCOM6
      PARAMETER (LJNPAR=4000)                                           LUNDCOM7
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUNDCOM8
      INTEGER         MSTU,MSTJ                                         LUNDCOM9
      REAL            PARU,PARJ                                         LUNDCO10
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUNDCO11
      INTEGER         KCHG                                              LUNDCO12
      REAL            PMAS,PARF,VCKM                                    LUNDCO13
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCO14
     &                KFDP(L2PARF,5)                                    LUNDCO15
      INTEGER         MDCY,MDME,KFDP                                    LUNDCO16
      REAL            BRAT                                              LUNDCO17
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCO18
      CHARACTER*8     CHAF                                              LUNDCO19
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUNDCO20
      INTEGER         N7LU,K7LU                                         LUNDCO21
      REAL            P7LU,V7LU                                         LUNDCO22
      Integer       I,J                                                 SUPART23
C                                                                       SUPART24
C     set the charges                                                   SUPART25
C     ===============                                                   SUPART26
      Integer       JCHG(500,3)                                         SUPART27
      DATA ( JCHG(I,1),I=41,79 )                                        SUPART28
     +/-1, 2,-1, 2,-1, 2,-1, 2,-1, 2                                    SUPART29
     +,-3, 0,-3, 0,-3, 0,-3,-3,-3, 0                                    SUPART30
     +,-1, 2,-1, 2,-3,-1, 2,-3, 0                                       SUPART31
     +, 0, 0, 0, 0, 0, 3, 3,-3,-3, 0/                                   SUPART32
      DATA ( JCHG(I,2),I=41,79 )                                        SUPART33
     +/10*1,10*0, 2*1, 2*1, 0, 2*1, 0, 0, 2, 9*0/                       SUPART34
      DATA ( JCHG(I,3),I=41,79 )                                        SUPART35
     +/19*1, 0,8*1,6*0,4*0,0/                                           SUPART36
      SAVE                                                              SUPART37
*                                                                       SUPART38
*     SSID from SUSYGEN, used to set the particle name                  SUPART39
*     ================================================                  SUPART40
      Character*5 SSID,NAME                                             SUPART41
      External    SSID                                                  SUPART42
C                                                                       SUPART43
C     set names                                                         SUPART44
C     =========                                                         SUPART45
      DO I=41,78                                                        SUPART46
        NAME=SSID(I)                                                    SUPART47
        DO J=5,1,-1                                                     SUPART48
          IF(NAME(J:J).NE.' ') GOTO 10                                  SUPART49
        ENDDO                                                           SUPART50
10      IF( NAME(J:J).EQ.'+' .OR. NAME(J:J).EQ.'-' ) NAME(J:J)=' '      SUPART51
        CHAF(I)=NAME                                                    SUPART52
      ENDDO                                                             SUPART53
      CHAF(79)='SusyProd'                                               SUPART54
C                                                                       SUPART55
C     set charges                                                       SUPART56
C     ===========                                                       SUPART57
      DO I=41,79                                                        SUPART58
      DO J=1,3                                                          SUPART59
        KCHG(I,J)=JCHG(I,J)                                             SUPART60
      ENDDO                                                             SUPART61
      ENDDO                                                             SUPART62
      END                                                               SUPART63
      SUBROUTINE SUPRNT(IPRINT)                                         SUPRNT 2
C---------------------------------------------------------------------- SUPRNT 3
C!  -                                                                   SUPRNT 4
C!                                                                      SUPRNT 5
C!   Author   :- Y. Gao                 5-FEB-1996                      SUPRNT 6
C!                                                                      SUPRNT 7
C!   Inputs:                                                            SUPRNT 8
C!        - IPRINT                                                      SUPRNT 9
C!                                                                      SUPRNT10
C!   Outputs:                                                           SUPRNT11
C!        - none                                                        SUPRNT12
C!                                                                      SUPRNT13
C!   Libraries required:                                                SUPRNT14
C!                                                                      SUPRNT15
C!   Description                                                        SUPRNT16
C!   ===========                                                        SUPRNT17
C!                                                                      SUPRNT18
C?                                                                      SUPRNT19
C!======================================================================SUPRNT20
      Implicit None                                                     SUPRNT21
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
      COMMON /BCS/   IW(LBCS )                                          BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
********************* start of commons of SUSYGEN **********************SUSCOM 2
      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rfmsq,rfmstopl,rfmstopr, SUSCOM 3
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)        SUSCOM 4
      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,                    SUSCOM 5
     +        rfmsq,rfmstopl,rfmstopr,                                  SUSCOM 6
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan           SUSCOM 7
                                                                        SUSCOM 8
      real*8  gmaum,gmaur,gm0,gtanb,gatri                               SUSCOM 9
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM10
      common/steer/gmaum,gmaur,gm0,gtanb,gatri                          SUSCOM11
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM12
                                                                        SUSCOM13
      Integer modes,MIX                                                 SUSCOM14
      common/mds/modes,MIX                                              SUSCOM15
                                                                        SUSCOM16
      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota             SUSCOM17
      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota       SUSCOM18
                                                                        SUSCOM19
      logical wrt,scan,lepi                                             SUSCOM20
      common/str/wrt,scan,lepi                                          SUSCOM21
                                                                        SUSCOM22
      real*8  flum,ecm,s,roots,T,Q,Q2,EN                                SUSCOM23
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        SUSCOM24
                                                                        SUSCOM25
      real*8  QK                                                        SUSCOM26
      COMMON/ISR/ QK(4)                                                 SUSCOM27
                                                                        SUSCOM28
      real erad,srad                                                    SUSCOM29
      common/srada/erad(100),srad(100)                                  SUSCOM30
                                                                        SUSCOM31
      integer  idbg,igener,irad                                         SUSCOM32
      COMMON /CONST/ idbg,igener,irad                                   SUSCOM33
                                                                        SUSCOM34
      integer  index,index1,index2,nevt                                 SUSCOM35
      COMMON/INDEXX/index,index1,index2,nevt                            SUSCOM36
                                                                        SUSCOM37
      real*8   fmpr1,fmpr2,XCROST,APRO                                  SUSCOM38
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO                             SUSCOM39
                                                                        SUSCOM40
      real*8   cosphimix,facqcd,fgama,spartmas,ratq                     SUSCOM41
      common/mixings/cosphimix,facqcd,fgama,spartmas,ratq               SUSCOM42
                                                                        SUSCOM43
      real*8   FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,    SUSCOM44
     +         FLC,FRC,gms,echar                                        SUSCOM45
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   SUSCOM46
     +FLC(12),FRC(12),gms(12),echar(12)                                 SUSCOM47
                                                                        SUSCOM48
      real*8      TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM49
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM50
                                                                        SUSCOM51
      real*8   fmal,fmar,ratqa,fgamc,fgamcr,cosmi                       SUSCOM52
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)   SUSCOM53
     +,cosmi(12)                                                        SUSCOM54
                                                                        SUSCOM55
      real*4 phimix,stop1                                               SUSCOM56
      common/stopmix/phimix,stop1                                       SUSCOM57
                                                                        SUSCOM58
      real*8   xgaug,xeta                                               SUSCOM59
      COMMON/XCROS/xgaug(8),xeta(8)                                     SUSCOM60
                                                                        SUSCOM61
      integer  ispa,kl,klap,idecs                                       SUSCOM62
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         SUSCOM63
                                                                        SUSCOM64
      Integer  idecsel                                                  SUSCOM65
      common/decsel/idecsel(18)                                         SUSCOM66
                                                                        SUSCOM67
      real*8   brsum,brsuma                                             SUSCOM68
      common/brsum/ brsum(5,6,6),brsuma(6)                              SUSCOM69
                                                                        SUSCOM70
      real*8   fms,fmi,fmk,fml1,fml2,etai,etak,brspa                    SUSCOM71
     +        ,brgaug,fmelt,fmert,fmelu,fmeru                           SUSCOM72
      Integer  lind                                                     SUSCOM73
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     SUSCOM74
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                SUSCOM75
                                                                        SUSCOM76
      Real*4   zr,was,esa                                               SUSCOM77
      Real*8   VOIJL,VOIJR,gfir,gfil                                    SUSCOM78
      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),                              SUSCOM79
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)                         SUSCOM80
                                                                        SUSCOM81
      real*8   OIJL,OIJR,V,U,FM,ETA                                     SUSCOM82
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)     SUSCOM83
                                                                        SUSCOM84
      real*8   oijlp,oijrp                                              SUSCOM85
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)                               SUSCOM86
                                                                        SUSCOM87
      Integer       ILOOP                                               SUSCOM88
      COMMON/SFLOOP/ILOOP                                               SUSCOM89
                                                                        SUSCOM90
      Real*8   PV                                                       SUSCOM91
      Integer  IFLAV,NFLAV                                              SUSCOM92
      COMMON /PARTC / PV(5,20),IFLAV(20,2),NFLAV                        SUSCOM93
                                                                        SUSCOM94
      real*8   gw,widfl,widfr,fmeltw,fmertw,fmeluw,fmeruw,gent,gentl    SUSCOM95
      integer  linda                                                    SUSCOM96
      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw, SUSCOM97
     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)                       SUSCOM98
********************* end  of commons of SUSYGEN ***********************SUSCOM99
      Integer       mproc                                               SUPROC 2
      Parameter    (mproc=35)                                           SUPROC 3
      Integer       iproc,iprod,ipar1,ipar2,ihand,isusy,iopen,ievnt     SUPROC 4
      Real          xproc                                               SUPROC 5
      common/SUPROC/iproc(mproc),iprod(mproc)                           SUPROC 6
     &             ,ipar1(mproc),ipar2(mproc)                           SUPROC 7
     &             ,ihand(mproc),isusy(mproc)                           SUPROC 8
     &             ,iopen(mproc),ievnt(mproc)                           SUPROC 9
     &             ,xproc(mproc)                                        SUPROC10
                                                                        SUPRNT25
      Integer       IPRINT                                              SUPRNT26
      Integer       IUT,i,j                                             SUPRNT27
                                                                        SUPRNT28
      Integer       number,iname1,iname2                                SUPRNT29
      Character*8   pname1,pname2                                       SUPRNT30
      Character*16  cname1,cname2                                       SUPRNT31
      Real          xsecti                                              SUPRNT32
      Character*3   switch                                              SUPRNT33
      Integer       nevent                                              SUPRNT34
                                                                        SUPRNT35
                                                                        SUPRNT36
      Integer       kf                                                  SUPRNT37
      Character*8   cf                                                  SUPRNT38
      Real          mf,wf                                               SUPRNT39
      Integer       md                                                  SUPRNT40
      Parameter    (md=100)                                             SUPRNT41
      Integer       nd                                                  SUPRNT42
      Character*8   cd(3,md)                                            SUPRNT43
      Real          wd(md),bd(md)                                       SUPRNT44
      Character*4   ch(md)                                              SUPRNT45
*-----------------------------------------------------------------------SUPRNT46
      IUT=IW(6)                                                         SUPRNT47
      if( IPRINT.eq.2 ) goto 200                                        SUPRNT48
C                                                                       SUPRNT49
C     print out basic parameters & mass matrice etc                     SUPRNT50
C     =============================================                     SUPRNT51
      WRITE(IUT,1001) TANB,FMGAUG,FMR,FM0,ATRI                          SUPRNT52
      WRITE(IUT,1002) (was(i),NINT(esa(i)),(zr(j,i),j=1,4),i=1,4)       SUPRNT53
      WRITE(IUT,1003) (FM(i),NINT(ETA(i)),U(1,i),U(2,i),i=1,2),V        SUPRNT54
                                                                        SUPRNT55
1003  FORMAT(                                                           SUPRNT56
     &10x,'Chargino mass matrix:',/,                                    SUPRNT57
     &10x,60('-'),/,                                                    SUPRNT58
     &10x,'mass (Gev)',5x,' eta',5x,'U-MATRIX  ','  Wino  Higgsino',/,  SUPRNT59
     &10x,f8.2,7x,I3,6x,            '   W1SS+ (',F8.3,F8.3,' )',/,      SUPRNT60
     &10x,f8.2,7x,I3,6x,            '   W2SS+ (',F8.3,F8.3,' )',//,     SUPRNT61
     &10x,24x,                      'V-MATRIX  ','  Wino  Higgsino',/,  SUPRNT62
     &10x,24x,                      '   W1SS- (',F8.3,F8.3,' )',/,      SUPRNT63
     &10x,24x,                      '   W2SS- (',F8.3,F8.3,' )',/,      SUPRNT64
     &10x,70('='))                                                      SUPRNT65
1002  FORMAT(                                                           SUPRNT66
     &10x,'Neutralino mass matrix:',/,                                  SUPRNT67
     &10X,60('-'),/,                                                    SUPRNT68
     &10x,'mass (Gev)',5x,' eta',5x,12x,'eigenvector',/,                SUPRNT69
     &10x,F8.2,7x,I3,6x,'(',F8.3,F8.3,F8.3,F8.3,' )',/,                 SUPRNT70
     &10x,F8.2,7x,I3,6x,'(',F8.3,F8.3,F8.3,F8.3,' )',/,                 SUPRNT71
     &10x,F8.2,7x,I3,6x,'(',F8.3,F8.3,F8.3,F8.3,' )',/,                 SUPRNT72
     &10x,F8.2,7x,I3,6x,'(',F8.3,F8.3,F8.3,F8.3,' )',/,                 SUPRNT73
     &10X,70('='))                                                      SUPRNT74
                                                                        SUPRNT75
1001  FORMAT(//,                                                        SUPRNT76
     &10X,70('='),/,                                                    SUPRNT77
     &10x,'MSSM  Parameters:',/,                                        SUPRNT78
     &10X,50('-'),/,                                                    SUPRNT79
     &10x,'                     tan(beta) = ', F8.2 ,/,                 SUPRNT80
     &10x,'SU(2) gaugino mass   M         = ', F8.2 ,/,                 SUPRNT81
     &10x,'Higgs mixing term    mu        = ', F8.2 ,/,                 SUPRNT82
     &10x,'Common scalar mass   m0        = ', F8.2 ,/,                 SUPRNT83
     &10x,'trilinear coupling   A         = ', F8.2 ,/,                 SUPRNT84
     &10x,70('='))                                                      SUPRNT85
C                                                                       SUPRNT86
C     print out masses etc                                              SUPRNT87
C     ====================                                              SUPRNT88
100   continue                                                          SUPRNT89
                                                                        SUPRNT90
      write(IUT,5001)                                                   SUPRNT91
      do kf=71,76                                                       SUPRNT92
        call SUTABL(kf,cf,mf,wf,nd,cd,wd,bd,ch,1)                       SUPRNT93
      enddo                                                             SUPRNT94
      write(IUT,5002)                                                   SUPRNT95
      do i=1,12                                                         SUPRNT96
      do j=1,2                                                          SUPRNT97
        kf=ispa(i,j)                                                    SUPRNT98
        if( kf.ne.0 )                                                   SUPRNT99
     &  call SUTABL(kf,cf,mf,wf,nd,cd,wd,bd,ch,1)                       SUPRN100
      enddo                                                             SUPRN101
      enddo                                                             SUPRN102
                                                                        SUPRN103
5001  format(10x,//,                                                    SUPRN104
     &       10x,'Gaugino  Summary Table',/,                            SUPRN105
     &       10x,'Gaugino  Summary Table',/,                            SUPRN106
     &       10x,'Gaugino  Summary Table')                              SUPRN107
5002  format(10x,//,                                                    SUPRN108
     &       10x,'Sfermion Summary Table',/,                            SUPRN109
     &       10x,'Sfermion Summary Table',/,                            SUPRN110
     &       10x,'Sfermion Summary Table')                              SUPRN111
      return                                                            SUPRN112
C                                                                       SUPRN113
C     end of job summary                                                SUPRN114
C     ==================                                                SUPRN115
200   continue                                                          SUPRN116
      write(IUT,*) ' '                                                  SUPRN117
      write(IUT,*) ' '                                                  SUPRN118
      write(IUT,1)                                                      SUPRN119
      do i=1,mproc                                                      SUPRN120
        iname1=ipar1(i)                                                 SUPRN121
        iname2=ipar2(i)                                                 SUPRN122
        if( iname1.eq.-75 ) iname1=77                                   SUPRN123
        if( iname1.eq.-76 ) iname1=78                                   SUPRN124
        if( iname2.eq.-75 ) iname2=77                                   SUPRN125
        if( iname2.eq.-76 ) iname2=78                                   SUPRN126
        call LUNAME(iname1,cname1)                                      SUPRN127
        call LUNAME(iname2,cname2)                                      SUPRN128
        pname1=cname1(1:8)                                              SUPRN129
        pname2=cname2(1:8)                                              SUPRN130
        number=i                                                        SUPRN131
        xsecti=xproc(i)                                                 SUPRN132
        if( iopen(i).eq.0 ) switch='OFF'                                SUPRN133
        if( iopen(i).eq.1 ) switch='ON '                                SUPRN134
        nevent=ievnt(i)                                                 SUPRN135
                                                                        SUPRN136
        write(IUT,3) number,pname1,pname2,xsecti,switch,nevent          SUPRN137
        write(IUT,2)                                                    SUPRN138
      enddo                                                             SUPRN139
                                                                        SUPRN140
1     format(                                                           SUPRN141
     &8x,'------------------ Generation  Summary --------------------'  SUPRN142
     &,/,                                                               SUPRN143
     &8x,'| PROC ||     e+e- -->     || cross-sect || ON  || events |'  SUPRN144
     &,/,                                                               SUPRN145
     &8x,'|      ||                  ||   (pb)     || OFF ||        |'  SUPRN146
     &,/,                                                               SUPRN147
     &8x,'-----------------------------------------------------------'  SUPRN148
     &)                                                                 SUPRN149
2     format(                                                           SUPRN150
     &8x,'-----------------------------------------------------------'  SUPRN151
     &)                                                                 SUPRN152
3     format(                                                           SUPRN153
     &8x,'|  ',I2,'  || '                                               SUPRN154
     &   ,a8,1x,a8,'|| ',e10.4,' || ',a3,' || ',I6,' |')                SUPRN155
      end                                                               SUPRN156
      SUBROUTINE SUSANA(IUT,mfail)                                      SUSANA 2
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**SUSANA 3
*                                                                      *SUSANA 4
*-- Author :    Stavros Katsanevas   14/04/95                          *SUSANA 5
*                                                                      *SUSANA 6
*-- Modified by Y.GAO  1/02/96                                         *SUSANA 7
*   quite a lot changes, refer to the original code for differences    *SUSANA 8
*                                                                      *SUSANA 9
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**SUSANA10
      Implicit None                                                     SUSANA11
********************* start of commons of SUSYGEN **********************SUSCOM 2
      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rfmsq,rfmstopl,rfmstopr, SUSCOM 3
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)        SUSCOM 4
      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,                    SUSCOM 5
     +        rfmsq,rfmstopl,rfmstopr,                                  SUSCOM 6
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan           SUSCOM 7
                                                                        SUSCOM 8
      real*8  gmaum,gmaur,gm0,gtanb,gatri                               SUSCOM 9
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM10
      common/steer/gmaum,gmaur,gm0,gtanb,gatri                          SUSCOM11
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM12
                                                                        SUSCOM13
      Integer modes,MIX                                                 SUSCOM14
      common/mds/modes,MIX                                              SUSCOM15
                                                                        SUSCOM16
      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota             SUSCOM17
      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota       SUSCOM18
                                                                        SUSCOM19
      logical wrt,scan,lepi                                             SUSCOM20
      common/str/wrt,scan,lepi                                          SUSCOM21
                                                                        SUSCOM22
      real*8  flum,ecm,s,roots,T,Q,Q2,EN                                SUSCOM23
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        SUSCOM24
                                                                        SUSCOM25
      real*8  QK                                                        SUSCOM26
      COMMON/ISR/ QK(4)                                                 SUSCOM27
                                                                        SUSCOM28
      real erad,srad                                                    SUSCOM29
      common/srada/erad(100),srad(100)                                  SUSCOM30
                                                                        SUSCOM31
      integer  idbg,igener,irad                                         SUSCOM32
      COMMON /CONST/ idbg,igener,irad                                   SUSCOM33
                                                                        SUSCOM34
      integer  index,index1,index2,nevt                                 SUSCOM35
      COMMON/INDEXX/index,index1,index2,nevt                            SUSCOM36
                                                                        SUSCOM37
      real*8   fmpr1,fmpr2,XCROST,APRO                                  SUSCOM38
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO                             SUSCOM39
                                                                        SUSCOM40
      real*8   cosphimix,facqcd,fgama,spartmas,ratq                     SUSCOM41
      common/mixings/cosphimix,facqcd,fgama,spartmas,ratq               SUSCOM42
                                                                        SUSCOM43
      real*8   FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,    SUSCOM44
     +         FLC,FRC,gms,echar                                        SUSCOM45
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   SUSCOM46
     +FLC(12),FRC(12),gms(12),echar(12)                                 SUSCOM47
                                                                        SUSCOM48
      real*8      TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM49
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM50
                                                                        SUSCOM51
      real*8   fmal,fmar,ratqa,fgamc,fgamcr,cosmi                       SUSCOM52
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)   SUSCOM53
     +,cosmi(12)                                                        SUSCOM54
                                                                        SUSCOM55
      real*4 phimix,stop1                                               SUSCOM56
      common/stopmix/phimix,stop1                                       SUSCOM57
                                                                        SUSCOM58
      real*8   xgaug,xeta                                               SUSCOM59
      COMMON/XCROS/xgaug(8),xeta(8)                                     SUSCOM60
                                                                        SUSCOM61
      integer  ispa,kl,klap,idecs                                       SUSCOM62
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         SUSCOM63
                                                                        SUSCOM64
      Integer  idecsel                                                  SUSCOM65
      common/decsel/idecsel(18)                                         SUSCOM66
                                                                        SUSCOM67
      real*8   brsum,brsuma                                             SUSCOM68
      common/brsum/ brsum(5,6,6),brsuma(6)                              SUSCOM69
                                                                        SUSCOM70
      real*8   fms,fmi,fmk,fml1,fml2,etai,etak,brspa                    SUSCOM71
     +        ,brgaug,fmelt,fmert,fmelu,fmeru                           SUSCOM72
      Integer  lind                                                     SUSCOM73
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     SUSCOM74
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                SUSCOM75
                                                                        SUSCOM76
      Real*4   zr,was,esa                                               SUSCOM77
      Real*8   VOIJL,VOIJR,gfir,gfil                                    SUSCOM78
      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),                              SUSCOM79
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)                         SUSCOM80
                                                                        SUSCOM81
      real*8   OIJL,OIJR,V,U,FM,ETA                                     SUSCOM82
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)     SUSCOM83
                                                                        SUSCOM84
      real*8   oijlp,oijrp                                              SUSCOM85
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)                               SUSCOM86
                                                                        SUSCOM87
      Integer       ILOOP                                               SUSCOM88
      COMMON/SFLOOP/ILOOP                                               SUSCOM89
                                                                        SUSCOM90
      Real*8   PV                                                       SUSCOM91
      Integer  IFLAV,NFLAV                                              SUSCOM92
      COMMON /PARTC / PV(5,20),IFLAV(20,2),NFLAV                        SUSCOM93
                                                                        SUSCOM94
      real*8   gw,widfl,widfr,fmeltw,fmertw,fmeluw,fmeruw,gent,gentl    SUSCOM95
      integer  linda                                                    SUSCOM96
      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw, SUSCOM97
     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)                       SUSCOM98
********************* end  of commons of SUSYGEN ***********************SUSCOM99
                                                                        SUSANA13
      Integer       IUT,mfail                                           SUSANA14
C                                                                       SUSANA15
C     set the flag                                                      SUSANA16
C     ============                                                      SUSANA17
      mfail=0                                                           SUSANA18
C                                                                       SUSANA19
C     calculate the mass and mixings of the sparticles                  SUSANA20
C     ================================================                  SUSANA21
                                                                        SUSANA22
      CALL sfermion(mfail)                                              SUSANA23
      if( mfail.ne.0 ) then                                             SUSANA24
        write(IUT,*) 'SUSANA: error in SFERMION'                        SUSANA25
        write(IUT,*) '        return code =    ',mfail                  SUSANA26
        mfail=1                                                         SUSANA27
        return                                                          SUSANA28
      endif                                                             SUSANA29
C                                                                       SUSANA30
C     calculate the mass and mixings of gauginos                        SUSANA31
C     ==========================================                        SUSANA32
      CALL gaugino(mfail)                                               SUSANA33
      if( mfail.ne.0 ) then                                             SUSANA34
        write(IUT,*) 'SUSANA: error in GAUGINO'                         SUSANA35
        mfail=2                                                         SUSANA36
        return                                                          SUSANA37
      endif                                                             SUSANA38
C                                                                       SUSANA39
C     LEP limits: move this part to ASKUSI   Y.GAO 25/02/96             SUSANA40
C     =====================================================             SUSANA41
C      if(LEPI) call leplim(mfail)                                      SUSANA42
C      if( mfail.ne.0 ) then                                            SUSANA43
C        write(IUT,*) 'SUSANA: exclude by LEP I limit'                  SUSANA44
C        mfail=3                                                        SUSANA45
C        return                                                         SUSANA46
C      endif                                                            SUSANA47
C                                                                       SUSANA48
C     calculate decay BR                                                SUSANA49
C     ==================                                                SUSANA50
      CALL branch                                                       SUSANA51
      end                                                               SUSANA52
      SUBROUTINE SUSETM                                                 SUSETM 2
C---------------------------------------------------------------------- SUSETM 3
C!  -                                                                   SUSETM 4
C!                                                                      SUSETM 5
C!   Author   :- Y. Gao                 6-FEB-1996                      SUSETM 6
C!                                                                      SUSETM 7
C!   Inputs:                                                            SUSETM 8
C!        - none                                                        SUSETM 9
C!                                                                      SUSETM10
C!   Outputs:                                                           SUSETM11
C!        - none                                                        SUSETM12
C!                                                                      SUSETM13
C!   Libraries required:                                                SUSETM14
C!                                                                      SUSETM15
C!   Description                                                        SUSETM16
C!   ===========                                                        SUSETM17
C!   set the mass & width of sparticles in LUND common block.           SUSETM18
C?                                                                      SUSETM19
C!======================================================================SUSETM20
      Implicit None                                                     SUSETM21
      INTEGER    L1MST,L1PAR                                            LUNDCOM2
     &          ,L2PAR,L2PARF                                           LUNDCOM3
     &          ,LJNPAR                                                 LUNDCOM4
      PARAMETER (L1MST=200, L1PAR=200)                                  LUNDCOM5
      PARAMETER (L2PAR=500, L2PARF=2000)                                LUNDCOM6
      PARAMETER (LJNPAR=4000)                                           LUNDCOM7
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUNDCOM8
      INTEGER         MSTU,MSTJ                                         LUNDCOM9
      REAL            PARU,PARJ                                         LUNDCO10
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUNDCO11
      INTEGER         KCHG                                              LUNDCO12
      REAL            PMAS,PARF,VCKM                                    LUNDCO13
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCO14
     &                KFDP(L2PARF,5)                                    LUNDCO15
      INTEGER         MDCY,MDME,KFDP                                    LUNDCO16
      REAL            BRAT                                              LUNDCO17
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCO18
      CHARACTER*8     CHAF                                              LUNDCO19
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUNDCO20
      INTEGER         N7LU,K7LU                                         LUNDCO21
      REAL            P7LU,V7LU                                         LUNDCO22
                                                                        SUSETM23
      Integer       kf,ki                                               SUSETM24
      Character*8   cf                                                  SUSETM25
      Real          mf,wf                                               SUSETM26
      Integer       md                                                  SUSETM27
      Parameter    (md=100)                                             SUSETM28
      Integer       nd                                                  SUSETM29
      Character*8   cd(3,md)                                            SUSETM30
      Real          wd(md),bd(md)                                       SUSETM31
      Character*4   ch(md)                                              SUSETM32
C --------------------------------------------------------------------- SUSETM33
      do 10 kf=41,78                                                    SUSETM34
        if( kf.eq.60 ) goto 10                                          SUSETM35
        if( kf.eq.69 ) goto 10                                          SUSETM36
        if( kf.eq.70 ) goto 10                                          SUSETM37
                                                                        SUSETM38
        ki=0                                                            SUSETM39
        if( kf.eq.63 ) ki=45                                            SUSETM40
        if( kf.eq.64 ) ki=46                                            SUSETM41
        if( kf.eq.65 ) ki=55                                            SUSETM42
        if( kf.eq.66 ) ki=61                                            SUSETM43
        if( kf.eq.67 ) ki=62                                            SUSETM44
        if( kf.eq.68 ) ki=59                                            SUSETM45
        if( kf.eq.77 ) ki=75                                            SUSETM46
        if( kf.eq.78 ) ki=76                                            SUSETM47
        if( ki.gt.0 ) then                                              SUSETM48
          PMAS(kf,1)=PMAS(ki,1)                                         SUSETM49
          PMAS(kf,2)=PMAS(ki,2)                                         SUSETM50
        else                                                            SUSETM51
          call SUTABL(kf,cf,mf,wf,nd,cd,wd,bd,ch,0)                     SUSETM52
          PMAS(kf,1)=mf                                                 SUSETM53
          PMAS(kf,2)=wf                                                 SUSETM54
        endif                                                           SUSETM55
10    continue                                                          SUSETM56
      END                                                               SUSETM57
      SUBROUTINE SUTABL(k,c,m,w,nd,cd,wd,bd,ch,IPRINT)                  SUTABL 2
C---------------------------------------------------------------------- SUTABL 3
C!  -                                                                   SUTABL 4
C!                                                                      SUTABL 5
C!   Author   :- Y. Gao                 5-FEB-1996                      SUTABL 6
C!                                                                      SUTABL 7
C!   Inputs:                                                            SUTABL 8
C!        - k        LUND code of S-particle                            SUTABL 9
C!        - IPRINT   1=PRINT/0=NOPRINT particle table                   SUTABL10
C!                                                                      SUTABL11
C!   Outputs:                                                           SUTABL12
C!        - c        particle name                                      SUTABL13
C!        - m        particle mass                                      SUTABL14
C!        - w        total width (kev)                                  SUTABL15
C!        -nd        total number of decay modes                        SUTABL16
C!        -cd(3,*)   the name of daughter particles                     SUTABL17
C!        -wd        partial widthes                                    SUTABL18
C!        -bd        branching ratios*100                               SUTABL19
C!        -ch        '    ' if no charged conj. mode                    SUTABL20
C!                   '+cc.' if charged conj. mode                       SUTABL21
C!                                                                      SUTABL22
C!   Libraries required:                                                SUTABL23
C!                                                                      SUTABL24
C!   Description                                                        SUTABL25
C!   ===========                                                        SUTABL26
C!                                                                      SUTABL27
C?                                                                      SUTABL28
C!======================================================================SUTABL29
      Implicit None                                                     SUTABL30
                                                                        SUTABL31
      Integer       k                                                   SUTABL32
      Character*8   c                                                   SUTABL33
      Real          m,w                                                 SUTABL34
      Integer       nd                                                  SUTABL35
      Character*8   cd(3,*)                                             SUTABL36
      Real          wd(*),bd(*)                                         SUTABL37
      Character*4   ch(*)                                               SUTABL38
      Integer       IPRINT                                              SUTABL39
                                                                        SUTABL40
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
      COMMON /BCS/   IW(LBCS )                                          BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
********************* start of commons of SUSYGEN **********************SUSCOM 2
      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rfmsq,rfmstopl,rfmstopr, SUSCOM 3
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)        SUSCOM 4
      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,                    SUSCOM 5
     +        rfmsq,rfmstopl,rfmstopr,                                  SUSCOM 6
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan           SUSCOM 7
                                                                        SUSCOM 8
      real*8  gmaum,gmaur,gm0,gtanb,gatri                               SUSCOM 9
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM10
      common/steer/gmaum,gmaur,gm0,gtanb,gatri                          SUSCOM11
     +       ,fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu            SUSCOM12
                                                                        SUSCOM13
      Integer modes,MIX                                                 SUSCOM14
      common/mds/modes,MIX                                              SUSCOM15
                                                                        SUSCOM16
      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota             SUSCOM17
      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota       SUSCOM18
                                                                        SUSCOM19
      logical wrt,scan,lepi                                             SUSCOM20
      common/str/wrt,scan,lepi                                          SUSCOM21
                                                                        SUSCOM22
      real*8  flum,ecm,s,roots,T,Q,Q2,EN                                SUSCOM23
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        SUSCOM24
                                                                        SUSCOM25
      real*8  QK                                                        SUSCOM26
      COMMON/ISR/ QK(4)                                                 SUSCOM27
                                                                        SUSCOM28
      real erad,srad                                                    SUSCOM29
      common/srada/erad(100),srad(100)                                  SUSCOM30
                                                                        SUSCOM31
      integer  idbg,igener,irad                                         SUSCOM32
      COMMON /CONST/ idbg,igener,irad                                   SUSCOM33
                                                                        SUSCOM34
      integer  index,index1,index2,nevt                                 SUSCOM35
      COMMON/INDEXX/index,index1,index2,nevt                            SUSCOM36
                                                                        SUSCOM37
      real*8   fmpr1,fmpr2,XCROST,APRO                                  SUSCOM38
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO                             SUSCOM39
                                                                        SUSCOM40
      real*8   cosphimix,facqcd,fgama,spartmas,ratq                     SUSCOM41
      common/mixings/cosphimix,facqcd,fgama,spartmas,ratq               SUSCOM42
                                                                        SUSCOM43
      real*8   FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,    SUSCOM44
     +         FLC,FRC,gms,echar                                        SUSCOM45
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   SUSCOM46
     +FLC(12),FRC(12),gms(12),echar(12)                                 SUSCOM47
                                                                        SUSCOM48
      real*8      TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM49
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    SUSCOM50
                                                                        SUSCOM51
      real*8   fmal,fmar,ratqa,fgamc,fgamcr,cosmi                       SUSCOM52
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)   SUSCOM53
     +,cosmi(12)                                                        SUSCOM54
                                                                        SUSCOM55
      real*4 phimix,stop1                                               SUSCOM56
      common/stopmix/phimix,stop1                                       SUSCOM57
                                                                        SUSCOM58
      real*8   xgaug,xeta                                               SUSCOM59
      COMMON/XCROS/xgaug(8),xeta(8)                                     SUSCOM60
                                                                        SUSCOM61
      integer  ispa,kl,klap,idecs                                       SUSCOM62
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         SUSCOM63
                                                                        SUSCOM64
      Integer  idecsel                                                  SUSCOM65
      common/decsel/idecsel(18)                                         SUSCOM66
                                                                        SUSCOM67
      real*8   brsum,brsuma                                             SUSCOM68
      common/brsum/ brsum(5,6,6),brsuma(6)                              SUSCOM69
                                                                        SUSCOM70
      real*8   fms,fmi,fmk,fml1,fml2,etai,etak,brspa                    SUSCOM71
     +        ,brgaug,fmelt,fmert,fmelu,fmeru                           SUSCOM72
      Integer  lind                                                     SUSCOM73
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     SUSCOM74
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                SUSCOM75
                                                                        SUSCOM76
      Real*4   zr,was,esa                                               SUSCOM77
      Real*8   VOIJL,VOIJR,gfir,gfil                                    SUSCOM78
      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),                              SUSCOM79
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)                         SUSCOM80
                                                                        SUSCOM81
      real*8   OIJL,OIJR,V,U,FM,ETA                                     SUSCOM82
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)     SUSCOM83
                                                                        SUSCOM84
      real*8   oijlp,oijrp                                              SUSCOM85
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)                               SUSCOM86
                                                                        SUSCOM87
      Integer       ILOOP                                               SUSCOM88
      COMMON/SFLOOP/ILOOP                                               SUSCOM89
                                                                        SUSCOM90
      Real*8   PV                                                       SUSCOM91
      Integer  IFLAV,NFLAV                                              SUSCOM92
      COMMON /PARTC / PV(5,20),IFLAV(20,2),NFLAV                        SUSCOM93
                                                                        SUSCOM94
      real*8   gw,widfl,widfr,fmeltw,fmertw,fmeluw,fmeruw,gent,gentl    SUSCOM95
      integer  linda                                                    SUSCOM96
      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw, SUSCOM97
     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)                       SUSCOM98
********************* end  of commons of SUSYGEN ***********************SUSCOM99
                                                                        SUTABL43
      Integer       klund                                               SUTABL44
      Character*16  clund                                               SUTABL45
                                                                        SUTABL46
      Integer       ksusy,fsusy                                         SUTABL47
      Integer       id1,id2,id3                                         SUTABL48
                                                                        SUTABL49
      Integer       i,j,l,ll,la,charge                                  SUTABL50
      Integer       IUT                                                 SUTABL51
                                                                        SUTABL52
      Real          g_to_k,g_to_m                                       SUTABL53
      Parameter    (g_to_k=1.0E+06,g_to_m=1.0E+03)                      SUTABL54
                                                                        SUTABL55
      Integer       LUCHGE                                              SUTABL56
      Real*8        SSMASS                                              SUTABL57
      External      LUCHGE,SSMASS                                       SUTABL58
C                                                                       SUTABL59
C     output unit                                                       SUTABL60
C     ===========                                                       SUTABL61
      IUT=IW(6)                                                         SUTABL62
C                                                                       SUTABL63
C     get the particle name                                             SUTABL64
C     =====================                                             SUTABL65
      klund=k                                                           SUTABL66
      if( k.eq.-75 ) klund=77                                           SUTABL67
      if( k.eq.-76 ) klund=78                                           SUTABL68
      if( k.eq.-77 ) klund=75                                           SUTABL69
      if( k.eq.-78 ) klund=76                                           SUTABL70
      if( abs(k).ge.71 .or.abs(k).le.74 ) klund=abs(klund)              SUTABL71
      call LUNAME(klund,clund)                                          SUTABL72
      c      = clund(1:8)                                               SUTABL73
      m      = ssmass(klund)                                            SUTABL74
      charge = LUCHGE(klund)                                            SUTABL75
      fsusy  = charge/3                                                 SUTABL76
C                                                                       SUTABL77
C     gaugino                                                           SUTABL78
C     =======                                                           SUTABL79
      if( abs(k).ge.71 .and. abs(k).le.78 ) then                        SUTABL80
        ksusy=abs(k)-70                                                 SUTABL81
        if( ksusy.gt.6 ) ksusy=ksusy-2                                  SUTABL82
        w    =brsuma(ksusy)                                             SUTABL83
                                                                        SUTABL84
        nd=0                                                            SUTABL85
        do 11 j=1,6                                                     SUTABL86
        do 12 i=1,18                                                    SUTABL87
          if( brgaug(i,j,ksusy).le.0.0 ) goto 12                        SUTABL88
                                                                        SUTABL89
          nd  = nd + 1                                                  SUTABL90
                                                                        SUTABL91
          if( ksusy.le.4 ) then                                         SUTABL92
            id1 = -(j+70)                                               SUTABL93
            id2 = kl(1,i)                                               SUTABL94
            id3 = kl(2,i)                                               SUTABL95
          else                                                          SUTABL96
            id1 = fsusy*(j+70)                                          SUTABL97
            id2 = fsusy*kl(1,i)                                         SUTABL98
            id3 = fsusy*kl(2,i)                                         SUTABL99
          endif                                                         SUTAB100
          if( abs(id1).le.74 ) id1=abs(id1)                             SUTAB101
          if( id1.eq.-75 ) id1=77                                       SUTAB102
          if( id1.eq.-76 ) id1=78                                       SUTAB103
                                                                        SUTAB104
          call LUNAME(id1,clund)                                        SUTAB105
          cd(1,nd)=clund(1:8)                                           SUTAB106
          call LUNAME(id2,clund)                                        SUTAB107
          cd(2,nd)=clund(1:8)                                           SUTAB108
          call LUNAME(id3,clund)                                        SUTAB109
          cd(3,nd)=clund(1:8)                                           SUTAB110
                                                                        SUTAB111
          l = mod(i-1,6)+1                                              SUTAB112
          if( j.gt.4 .and. l.gt.4 ) then                                SUTAB113
            ch(nd)='+cc.'                                               SUTAB114
            wd(nd)=brgaug(i,j,ksusy)*2.0d0                              SUTAB115
            bd(nd)=brgaug(i,j,ksusy)*2.0d0/w                            SUTAB116
          else                                                          SUTAB117
            ch(nd)='    '                                               SUTAB118
            wd(nd)=brgaug(i,j,ksusy)                                    SUTAB119
            bd(nd)=brgaug(i,j,ksusy)/w                                  SUTAB120
          endif                                                         SUTAB121
                                                                        SUTAB122
  12    enddo                                                           SUTAB123
  11    enddo                                                           SUTAB124
C                                                                       SUTAB125
C     sFermion                                                          SUTAB126
C     ========                                                          SUTAB127
      elseif( (abs(k).ge.41 .and. abs(k).le.59)                         SUTAB128
     &    .or.(abs(k).ge.61 .and. abs(k).le.62) ) then                  SUTAB129
        do i=1,12                                                       SUTAB130
        do j=1,2                                                        SUTAB131
          if( abs(k).eq.ispa(i,j) ) then                                SUTAB132
            ll=i                                                        SUTAB133
            la=j                                                        SUTAB134
          endif                                                         SUTAB135
        enddo                                                           SUTAB136
        enddo                                                           SUTAB137
                                                                        SUTAB138
        if( la.eq.1 ) w=widfl(ll)                                       SUTAB139
        if( la.eq.2 ) w=widfr(ll)                                       SUTAB140
                                                                        SUTAB141
        ksusy=12*(la-1)+ll                                              SUTAB142
        nd  =0                                                          SUTAB143
        do 20 i=1,6                                                     SUTAB144
          if( brspa(i,ksusy).le.0.0d0 ) goto 20                         SUTAB145
                                                                        SUTAB146
          nd=nd+1                                                       SUTAB147
          ch(nd)='    '                                                 SUTAB148
          wd(nd)=brspa(i,ksusy)                                         SUTAB149
          bd(nd)=brspa(i,ksusy)/w                                       SUTAB150
                                                                        SUTAB151
          id1=70+i                                                      SUTAB152
          id2=idecs(ll,1)                                               SUTAB153
          if(i.gt.4) then                                               SUTAB154
            id2=idecs(ll,2)                                             SUTAB155
            if( charge.lt.0 ) id1=-id1                                  SUTAB156
          endif                                                         SUTAB157
          if( id1.eq.-75 ) id1=77                                       SUTAB158
          if( id1.eq.-76 ) id1=78                                       SUTAB159
                                                                        SUTAB160
          call LUNAME(id1,clund)                                        SUTAB161
          cd(1,nd)=clund(1:8)                                           SUTAB162
          call LUNAME(id2,clund)                                        SUTAB163
          cd(2,nd)=clund(1:8)                                           SUTAB164
          cd(3,nd)=' '                                                  SUTAB165
20      continue                                                        SUTAB166
C                                                                       SUTAB167
C     unknown code                                                      SUTAB168
C     ============                                                      SUTAB169
      else                                                              SUTAB170
        write(IUT,*) 'SUTABL: unknown code = ',k                        SUTAB171
        return                                                          SUTAB172
      endif                                                             SUTAB173
                                                                        SUTAB174
      if( IPRINT.lt.1 ) RETURN                                          SUTAB175
C                                                                       SUTAB176
C     print a table for this particle                                   SUTAB177
C     ===============================                                   SUTAB178
                                                                        SUTAB179
      write(IUT,1)                                                      SUTAB180
      write(IUT,2) c                                                    SUTAB181
      write(IUT,3) m,w*g_to_k                                           SUTAB182
      write(IUT,4)                                                      SUTAB183
      if( nd.eq.0 ) then                                                SUTAB184
        write(IUT,5)                                                    SUTAB185
      else                                                              SUTAB186
        do i=1,nd                                                       SUTAB187
        if    ( bd(i).gt.1.0E-04 ) then                                 SUTAB188
          write(IUT,6) cd(1,i),cd(2,i),cd(3,i),ch(i)                    SUTAB189
     &                ,wd(i)*g_to_k,bd(i)*100.0                         SUTAB190
        elseif( bd(i).gt.1.0E-07 ) then                                 SUTAB191
          write(IUT,7) cd(1,i),cd(2,i),cd(3,i),ch(i)                    SUTAB192
     &                ,wd(i)*g_to_k*1000.0,bd(i)*100.0*1000.0           SUTAB193
        else                                                            SUTAB194
          write(IUT,8) cd(1,i),cd(2,i),cd(3,i),ch(i)                    SUTAB195
     &                ,wd(i)*g_to_k*1000000.0,bd(i)*100.0*1000000.0     SUTAB196
        endif                                                           SUTAB197
        enddo                                                           SUTAB198
      endif                                                             SUTAB199
      write(IUT,1)                                                      SUTAB200
1     format(10x,85('='))                                               SUTAB201
2     format(10x,a8,/)                                                  SUTAB202
3     format(10x,10x,'Mass  = ',f12.2,'      Gev',/,                    SUTAB203
     &       10x,10x,'Width = ',f12.2,'      Kev',/)                    SUTAB204
4     format(10x,'MODE',26x                                             SUTAB205
     &          ,8x                                                     SUTAB206
     &          ,'partial width (kev)',6x                               SUTAB207
     &          ,'Branching ratio (%)',/,10x,85('-'))                   SUTAB208
5     format(10x,'stable')                                              SUTAB209
6     format(10x,a8,1x,a8,1x,a8,4x                                      SUTAB210
     &          ,a4,4x                                                  SUTAB211
     &          ,f15.4,10x,f15.4,10x)                                   SUTAB212
7     format(10x,a8,1x,a8,1x,a8,4x                                      SUTAB213
     &          ,a4,4x                                                  SUTAB214
     &          ,f15.4,'E-03',6x,f15.4,'E-03',6x)                       SUTAB215
8     format(10x,a8,1x,a8,1x,a8,4x                                      SUTAB216
     &          ,a4,4x                                                  SUTAB217
     &          ,f15.4,'E-06',6x,f15.4,'E-06',6x)                       SUTAB218
      END                                                               SUTAB219
      SUBROUTINE USCJOB                                                 USCJOB 2
C-------------------------------------------------------------------    USCJOB 3
C! End of job routine    SUSYGEN & Lund 7.4                             USCJOB 4
C                                                                       USCJOB 5
C   To be filled by user to print any relevant info                     USCJOB 6
C                                                                       USCJOB 7
C------------------------------------------------------------------     USCJOB 8
      Implicit None                                                     USCJOB 9
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
      COMMON /BCS/   IW(LBCS )                                          BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
      Integer       MXIDHS                                              WORKHS 2
      Parameter    (MXIDHS=500)                                         WORKHS 3
      Integer       NIDOLD,IIDOLD,NIDNEW,IIDNEW                         WORKHS 4
      Logical       HSKEEP                                              WORKHS 5
      Common/WORKHS/HSKEEP,NIDOLD,IIDOLD(MXIDHS),NIDNEW,IIDNEW(MXIDHS)  WORKHS 6
      Integer       IPRI,IHST                                           SUWGHT 2
      Real          ECMS,SVERT                                          SUWGHT 3
      Integer       mact                                                SUWGHT 4
      Parameter    (mact=100)                                           SUWGHT 5
      Integer       nact,iact,icoulu                                    SUWGHT 6
      Real          sact,wact                                           SUWGHT 7
      common/SUWGHT/IPRI,ECMS,SVERT(3),icoulu(10)                       SUWGHT 8
     &             ,nact,iact(mact)                                     SUWGHT 9
     &                  ,sact(mact)                                     SUWGHT10
     &                  ,wact(mact)                                     SUWGHT11
      Logical       HEXIST,XWORKH                                       USCJOB13
      External      HEXIST                                              USCJOB14
      Integer       i,j,iut                                             USCJOB15
C                                                                       USCJOB16
C     delete working histograms                                         USCJOB17
C     =========================                                         USCJOB18
      if( .not.HSKEEP ) then                                            USCJOB19
        do i=1,NIDNEW                                                   USCJOB20
          XWORKH=.true.                                                 USCJOB21
          do j=1,NIDOLD                                                 USCJOB22
            if( IIDOLD(j).eq.IIDNEW(i) ) XWORKH=.false.                 USCJOB23
          enddo                                                         USCJOB24
                                                                        USCJOB25
          if( XWORKH.and.HEXIST(IIDNEW(i)) ) call HDELET(IIDNEW(i))     USCJOB26
        enddo                                                           USCJOB27
      endif                                                             USCJOB28
C                                                                       USCJOB29
C     end of job summary                                                USCJOB30
C     ==================                                                USCJOB31
      call SUPRNT(2)                                                    USCJOB32
                                                                        USCJOB33
      iut = iw(6)                                                       USCJOB34
      WRITE(IUT,101)                                                    USCJOB35
  101 FORMAT(//20X,'EVENTS STATISTICS',                                 USCJOB36
     &        /20X,'*****************')                                 USCJOB37
      WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)          USCJOB38
  102 FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,         USCJOB39
     &       /5X,'# OF ACCEPTED  EVENTS                = ',I10,         USCJOB40
     &       /5X,'# OF REJECTED  EVENTS                = ',I10)         USCJOB41
      WRITE(IUT,103)                                                    USCJOB42
  103 FORMAT(//20X,'REJECT STATISTICS',                                 USCJOB43
     &        /20X,'*****************')                                 USCJOB44
      WRITE(IUT,104) (ICOULU(I),I=1,6)                                  USCJOB45
  104 FORMAT(/10X,'IR= 1 LUND ERROR unknown part    # OF REJECT =',I10, USCJOB46
     &       /10X,'IR= 2 BOS  ERROR KINE/VERT       # OF REJECT =',I10, USCJOB47
     &       /10X,'IR= 3 LUND ERROR too many tracks # OF REJECT =',I10, USCJOB48
     &       /10X,'IR= 4 LUND ERROR Beam wrong pos  # OF REJECT =',I10, USCJOB49
     &       /10X,'IR= 5 LUND ERROR status code >5  # OF REJECT =',I10, USCJOB50
     &       /10X,'IR= 6-7-8 free for user          # OF REJECT =',I10) USCJOB51
                                                                        USCJOB52
      RETURN                                                            USCJOB53
      END                                                               USCJOB54
      SUBROUTINE LUNAME(KF,CHAU)                                        LUNAME 2
                                                                        LUNAME 3
C...Purpose: to give the particle/parton name as a character string.    LUNAME 4
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)             LUNAME 5
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)        LUNAME 6
      COMMON/LUDAT4/CHAF(500)                                           LUNAME 7
      CHARACTER CHAF*8                                                  LUNAME 8
      SAVE /LUDAT1/,/LUDAT2/,/LUDAT4/                                   LUNAME 9
      CHARACTER CHAU*16                                                 LUNAME10
                                                                        LUNAME11
C...Initial values. Charge. Subdivide code.                             LUNAME12
      CHAU=' '                                                          LUNAME13
      KFA=IABS(KF)                                                      LUNAME14
      KC=LUCOMP(KF)                                                     LUNAME15
      IF(KC.EQ.0) RETURN                                                LUNAME16
      KQ=LUCHGE(KF)                                                     LUNAME17
      KFLA=MOD(KFA/1000,10)                                             LUNAME18
      KFLB=MOD(KFA/100,10)                                              LUNAME19
      KFLC=MOD(KFA/10,10)                                               LUNAME20
      KFLS=MOD(KFA,10)                                                  LUNAME21
      KFLR=MOD(KFA/10000,10)                                            LUNAME22
                                                                        LUNAME23
C...Read out root name and spin for simple particle.                    LUNAME24
      IF(KFA.LE.100.OR.(KFA.GT.100.AND.KC.GT.100)) THEN                 LUNAME25
        CHAU=CHAF(KC)                                                   LUNAME26
        LEN=0                                                           LUNAME27
        DO 100 LEM=1,8                                                  LUNAME28
        IF(CHAU(LEM:LEM).NE.' ') LEN=LEM                                LUNAME29
  100   CONTINUE                                                        LUNAME30
                                                                        LUNAME31
C...Construct root name for diquark. Add on spin.                       LUNAME32
      ELSEIF(KFLC.EQ.0) THEN                                            LUNAME33
        CHAU(1:2)=CHAF(KFLA)(1:1)//CHAF(KFLB)(1:1)                      LUNAME34
        IF(KFLS.EQ.1) CHAU(3:4)='_0'                                    LUNAME35
        IF(KFLS.EQ.3) CHAU(3:4)='_1'                                    LUNAME36
        LEN=4                                                           LUNAME37
                                                                        LUNAME38
C...Construct root name for heavy meson. Add on spin and heavy flavour. LUNAME39
      ELSEIF(KFLA.EQ.0) THEN                                            LUNAME40
        IF(KFLB.EQ.5) CHAU(1:1)='B'                                     LUNAME41
        IF(KFLB.EQ.6) CHAU(1:1)='T'                                     LUNAME42
        IF(KFLB.EQ.7) CHAU(1:1)='L'                                     LUNAME43
        IF(KFLB.EQ.8) CHAU(1:1)='H'                                     LUNAME44
        LEN=1                                                           LUNAME45
        IF(KFLR.EQ.0.AND.KFLS.EQ.1) THEN                                LUNAME46
        ELSEIF(KFLR.EQ.0.AND.KFLS.EQ.3) THEN                            LUNAME47
          CHAU(2:2)='*'                                                 LUNAME48
          LEN=2                                                         LUNAME49
        ELSEIF(KFLR.EQ.1.AND.KFLS.EQ.3) THEN                            LUNAME50
          CHAU(2:3)='_1'                                                LUNAME51
          LEN=3                                                         LUNAME52
        ELSEIF(KFLR.EQ.1.AND.KFLS.EQ.1) THEN                            LUNAME53
          CHAU(2:4)='*_0'                                               LUNAME54
          LEN=4                                                         LUNAME55
        ELSEIF(KFLR.EQ.2) THEN                                          LUNAME56
          CHAU(2:4)='*_1'                                               LUNAME57
          LEN=4                                                         LUNAME58
        ELSEIF(KFLS.EQ.5) THEN                                          LUNAME59
          CHAU(2:4)='*_2'                                               LUNAME60
          LEN=4                                                         LUNAME61
        ENDIF                                                           LUNAME62
        IF(KFLC.GE.3.AND.KFLR.EQ.0.AND.KFLS.LE.3) THEN                  LUNAME63
          CHAU(LEN+1:LEN+2)='_'//CHAF(KFLC)(1:1)                        LUNAME64
          LEN=LEN+2                                                     LUNAME65
        ELSEIF(KFLC.GE.3) THEN                                          LUNAME66
          CHAU(LEN+1:LEN+1)=CHAF(KFLC)(1:1)                             LUNAME67
          LEN=LEN+1                                                     LUNAME68
        ENDIF                                                           LUNAME69
                                                                        LUNAME70
C...Construct root name and spin for heavy baryon.                      LUNAME71
      ELSE                                                              LUNAME72
        IF(KFLB.LE.2.AND.KFLC.LE.2) THEN                                LUNAME73
          CHAU='Sigma '                                                 LUNAME74
          IF(KFLC.GT.KFLB) CHAU='Lambda'                                LUNAME75
          IF(KFLS.EQ.4) CHAU='Sigma*'                                   LUNAME76
          LEN=5                                                         LUNAME77
          IF(CHAU(6:6).NE.' ') LEN=6                                    LUNAME78
        ELSEIF(KFLB.LE.2.OR.KFLC.LE.2) THEN                             LUNAME79
          CHAU='Xi '                                                    LUNAME80
          IF(KFLA.GT.KFLB.AND.KFLB.GT.KFLC) CHAU='Xi'''                 LUNAME81
          IF(KFLS.EQ.4) CHAU='Xi*'                                      LUNAME82
          LEN=2                                                         LUNAME83
          IF(CHAU(3:3).NE.' ') LEN=3                                    LUNAME84
        ELSE                                                            LUNAME85
          CHAU='Omega '                                                 LUNAME86
          IF(KFLA.GT.KFLB.AND.KFLB.GT.KFLC) CHAU='Omega'''              LUNAME87
          IF(KFLS.EQ.4) CHAU='Omega*'                                   LUNAME88
          LEN=5                                                         LUNAME89
          IF(CHAU(6:6).NE.' ') LEN=6                                    LUNAME90
        ENDIF                                                           LUNAME91
                                                                        LUNAME92
C...Add on heavy flavour content for heavy baryon.                      LUNAME93
        CHAU(LEN+1:LEN+2)='_'//CHAF(KFLA)(1:1)                          LUNAME94
        LEN=LEN+2                                                       LUNAME95
        IF(KFLB.GE.KFLC.AND.KFLC.GE.4) THEN                             LUNAME96
          CHAU(LEN+1:LEN+2)=CHAF(KFLB)(1:1)//CHAF(KFLC)(1:1)            LUNAME97
          LEN=LEN+2                                                     LUNAME98
        ELSEIF(KFLB.GE.KFLC.AND.KFLB.GE.4) THEN                         LUNAME99
          CHAU(LEN+1:LEN+1)=CHAF(KFLB)(1:1)                             LUNAM100
          LEN=LEN+1                                                     LUNAM101
        ELSEIF(KFLC.GT.KFLB.AND.KFLB.GE.4) THEN                         LUNAM102
          CHAU(LEN+1:LEN+2)=CHAF(KFLC)(1:1)//CHAF(KFLB)(1:1)            LUNAM103
          LEN=LEN+2                                                     LUNAM104
        ELSEIF(KFLC.GT.KFLB.AND.KFLC.GE.4) THEN                         LUNAM105
          CHAU(LEN+1:LEN+1)=CHAF(KFLC)(1:1)                             LUNAM106
          LEN=LEN+1                                                     LUNAM107
        ENDIF                                                           LUNAM108
      ENDIF                                                             LUNAM109
                                                                        LUNAM110
C...Add on bar sign for antiparticle (where necessary).                 LUNAM111
      IF(KF.GT.0.OR.LEN.EQ.0) THEN                                      LUNAM112
      ELSEIF(KFA.GT.10.AND.KFA.LE.40.AND.KQ.NE.0.AND.MOD(KQ,3).EQ.0)    LUNAM113
     &THEN                                                              LUNAM114
CYG                                                                     LUNAM115
      ELSEIF(KFA.GT.40.AND.KFA.LE.80.AND.KQ.NE.0.AND.MOD(KQ,3).EQ.0)    LUNAM116
     &THEN                                                              LUNAM117
CYG                                                                     LUNAM118
      ELSEIF(KFA.EQ.89.OR.(KFA.GE.91.AND.KFA.LE.99)) THEN               LUNAM119
      ELSEIF(KFA.GT.100.AND.KFLA.EQ.0.AND.KQ.NE.0) THEN                 LUNAM120
      ELSEIF(MSTU(15).LE.1) THEN                                        LUNAM121
        CHAU(LEN+1:LEN+1)='~'                                           LUNAM122
        LEN=LEN+1                                                       LUNAM123
      ELSE                                                              LUNAM124
        CHAU(LEN+1:LEN+3)='bar'                                         LUNAM125
        LEN=LEN+3                                                       LUNAM126
      ENDIF                                                             LUNAM127
                                                                        LUNAM128
C...Add on charge where applicable (conventional cases skipped).        LUNAM129
      IF(KQ.EQ.6) CHAU(LEN+1:LEN+2)='++'                                LUNAM130
      IF(KQ.EQ.-6) CHAU(LEN+1:LEN+2)='--'                               LUNAM131
      IF(KQ.EQ.3) CHAU(LEN+1:LEN+1)='+'                                 LUNAM132
      IF(KQ.EQ.-3) CHAU(LEN+1:LEN+1)='-'                                LUNAM133
      IF(KQ.EQ.0.AND.(KFA.LE.22.OR.LEN.EQ.0)) THEN                      LUNAM134
      ELSEIF(KQ.EQ.0.AND.(KFA.GE.81.AND.KFA.LE.100)) THEN               LUNAM135
      ELSEIF(KFA.EQ.28.OR.KFA.EQ.29) THEN                               LUNAM136
      ELSEIF(KFA.GT.100.AND.KFLA.EQ.0.AND.KFLB.EQ.KFLC.AND.             LUNAM137
     &KFLB.NE.1) THEN                                                   LUNAM138
      ELSEIF(KQ.EQ.0) THEN                                              LUNAM139
        CHAU(LEN+1:LEN+1)='0'                                           LUNAM140
      ENDIF                                                             LUNAM141
                                                                        LUNAM142
      RETURN                                                            LUNAM143
      END                                                               LUNAM144
      CHARACTER*5 FUNCTION SSID(ID)                                     SSID   2
C-----------------------------------------------------------------------SSID   3
C                                                                       SSID   4
C     Return character name for ID, assuming the default IDENT codes    SSID   5
C     are used in /SSTYPE/.                                             SSID   6
C                                      modified ISAJET                  SSID   7
C-----------------------------------------------------------------------SSID   8
                                                                        SSID   9
      common/ludat4/chaf(500)                                           SSID  10
      character chaf*8                                                  SSID  11
                                                                        SSID  12
      CHARACTER*5 LABEL(-80:80)                                         SSID  13
      SAVE LABEL                                                        SSID  14
                                                                        SSID  15
                                                                        SSID  16
      DATA LABEL(0)/'     '/                                            SSID  17
                                                                        SSID  18
      DATA (LABEL(J),J=1,10)                                            SSID  19
     +/'DN   ','UP   ','ST   ','CH   ','BT   ','TP   '                  SSID  20
     +,'ERROR','ERROR','ERROR','ERROR'/                                 SSID  21
      DATA (LABEL(J),J=-1,-10,-1)                                       SSID  22
     +/'DB   ','UB   ','SB   ','CB   ','BB   ','TB   '                  SSID  23
     +,'ERROR','ERROR','ERROR','ERROR'/                                 SSID  24
                                                                        SSID  25
      DATA (LABEL(J),J=11,20)                                           SSID  26
     +/'E-   ','NUE  ','MU-  ','NUM  ','TAU- ','NUT  '                  SSID  27
     +,'ERROR','ERROR','ERROR','ERROR'/                                 SSID  28
      DATA (LABEL(J),J=-11,-20,-1)                                      SSID  29
     +/'E+   ','ANUE ','MU+  ','ANUM ','TAU+ ','ANUT '                  SSID  30
     +,'ERROR','ERROR','ERROR','ERROR'/                                 SSID  31
                                                                        SSID  32
      DATA (LABEL(J),J=21,30)                                           SSID  33
     +/'GLUON','GAMMA','Z0   ','W+   ','H0L  ','ERROR'                  SSID  34
     +,'ERROR','ERROR','ERROR','ERROR'/                                 SSID  35
      DATA (LABEL(J),J=-21,-30,-1)                                      SSID  36
     +/'GLUON','GAMMA','Z0   ','W-   ','H0L  ','ERROR'                  SSID  37
     +,'ERROR','ERROR','ERROR','ERROR'/                                 SSID  38
                                                                        SSID  39
      DATA (LABEL(J),J=31,40)                                           SSID  40
     +/'ERROR','ERROR','ERROR','ERROR','H0H  ','A0   '                  SSID  41
     +,'H+   ','ERROR','ERROR','ERROR'/                                 SSID  42
      DATA (LABEL(J),J=-31,-40,-1)                                      SSID  43
     +/'ERROR','ERROR','ERROR','ERROR','H0H  ','A0   '                  SSID  44
     +,'H-   ','ERROR','ERROR','ERROR'/                                 SSID  45
                                                                        SSID  46
      DATA (LABEL(J),J=41,50)                                           SSID  47
     +/'DNL  ','UPL  ','STL ','CHL  ','BT1  ','TP1  '                   SSID  48
     +,'DNR  ','UPR  ','STR  ','CHR  '/                                 SSID  49
      DATA (LABEL(J),J=-41,-50,-1)                                      SSID  50
     +/'DNLb ','UPLb ','STLb ','CHLb ','BT1b ','TP1b '                  SSID  51
     +,'DNRb ','UPRb ','STRb ','CHRb '/                                 SSID  52
                                                                        SSID  53
      DATA (LABEL(J),J=51,60)                                           SSID  54
     +     /'EL-  ','NUEL ','MUL- ','NUML ','TAU1-','NUTL '             SSID  55
     +     ,'ER-  ','MUR- ','TAU2-','ERROR'/                            SSID  56
      DATA (LABEL(J),J=-51,-60,-1)                                      SSID  57
     +     /'EL+  ','ANUEL','MUL+ ','ANUML','TAU1+','ANUTL'             SSID  58
     +     ,'ER+  ','MUR+ ','TAU2+','ERROR'/                            SSID  59
                                                                        SSID  60
      DATA (LABEL(J),J=61,70)                                           SSID  61
     +/'BT2  ','TP2  ','BTL  ','TPL  ','TAUL ','BTR  '                  SSID  62
     +,'TPR  ','TAUR ','ERROR','GLSS '/                                 SSID  63
      DATA (LABEL(J),J=-61,-70,-1)                                      SSID  64
     +/'BT2b ','TP2b ','BTLb ','TPLb ','TAULb','BTRb '                  SSID  65
     +,'TPRb ','TAURb','ERROR','GLSS '/                                 SSID  66
                                                                        SSID  67
      DATA (LABEL(J),J=71,80)                                           SSID  68
     +/'Z1SS ','Z2SS ','Z3SS ','Z4SS ','W1SS+','W2SS+'                  SSID  69
     +,'W1SS-','W2SS-','ERROR','ERROR'/                                 SSID  70
      DATA (LABEL(J),J=-71,-80,-1)                                      SSID  71
     +/'Z1SS ','Z2SS ','Z3SS ','Z4SS ','W1SS-','W2SS-'                  SSID  72
     +,'W1SS+','W2SS+','ERROR','ERROR'/                                 SSID  73
                                                                        SSID  74
      IF(IABS(ID).GT.80) THEN                                           SSID  75
CYG        WRITE(1,*) 'SSID: ID = ',ID                                  SSID  76
        STOP99                                                          SSID  77
      ENDIF                                                             SSID  78
                                                                        SSID  79
CYG      if(id.ne.0)then                                                SSID  80
CYG        id1=iabs(id)                                                 SSID  81
CYG        chaf(id1)=label(id1)                                         SSID  82
CYG      endif                                                          SSID  83
                                                                        SSID  84
      SSID=LABEL(ID)                                                    SSID  85
      RETURN                                                            SSID  86
      END                                                               SSID  87
      SUBROUTINE mssm_gene                                              MSSMGE 2
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**MSSMGE 3
*-- Author :    Stavros Katsanevas   14/04/95                          *MSSMGE 4
*                                                                      *MSSMGE 5
*-- Modified by Y.GAO    5/02/96                                       *MSSMGE 6
*                                                                      *MSSMGE 7
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**MSSMGE 8
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               MSSMGE 9
                                                                        MSSMGE10
                                                                        MSSMGE11
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        MSSMGE12
      COMMON/INDEXX/index,index1,index2,nevt                            MSSMGE13
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO                             MSSMGE14
                                                                        MSSMGE15
                                                                        MSSMGE16
      COMMON/ISR/ QK(4)                                                 MSSMGE17
      COMMON /CONST/ idbg,igener,irad                                   MSSMGE18
                                                                        MSSMGE19
      real erad,srad                                                    MSSMGE20
      common/srada/erad(100),srad(100)                                  MSSMGE21
      logical wrt,scan,lepi                                             MSSMGE22
      common/str/wrt,scan,lepi                                          MSSMGE23
                                                                        MSSMGE24
      external gensel,gensmu,gensnue,gensnu,photi                       MSSMGE25
                                                                        MSSMGE26
      common/kev/jev                                                    MSSMGE27
C                                                                       MSSMGE28
C       MAXIMUM OF  PRODUCTION, UNIFORM STEP SAMPLING                   MSSMGE29
C                                                                       MSSMGE30
      if(nevt.lt.1)return                                               MSSMGE31
                                                                        MSSMGE32
CYG      if(irad.eq.1)then                                              MSSMGE33
CYG        STEP = 2.                                                    MSSMGE34
CYG        NSORT =ecm/step                                              MSSMGE35
CYG        ZSLEPT = 0.                                                  MSSMGE36
CYG        DO 10 J=1,NSORT                                              MSSMGE37
CYG          ZSLEPT = ZSLEPT+STEP                                       MSSMGE38
CYG          s=zslept**2                                                MSSMGE39
CYG          roots=dsqrt(s)                                             MSSMGE40
CYG          if(index.eq.1)amplit=photi(index1,index2)                  MSSMGE41
CYG          if(index.eq.2)amplit=chargi(index1,index2)                 MSSMGE42
CYG          if(index.eq.3)then                                         MSSMGE43
CYG            IF(INDEX1.EQ.51.or.index1.eq.57)then                     MSSMGE44
CYG              if(index1.eq.-index2)amplit=ssdint(-1.d0,gensel,1.d0)  MSSMGE45
CYG              if(index1.ne.-index2)amplit=genselrs(dummy)            MSSMGE46
CYG            elseif(index1.eq.52)then                                 MSSMGE47
CYG              amplit=ssdint(-1.d0,gensnue,1.d0)                      MSSMGE48
CYG            elseif(index1.eq.54.or.index1.eq.56)then                 MSSMGE49
CYG              amplit=ssdint(-1.d0,gensnu,1.d0)                       MSSMGE50
CYG            else                                                     MSSMGE51
CYG              amplit=gensmus(dummy)                                  MSSMGE52
CYG            endif                                                    MSSMGE53
CYG          endif                                                      MSSMGE54
CYG          erad(j)=real(roots)                                        MSSMGE55
CYG          srad(j)=real(amplit)                                       MSSMGE56
CYG          if(srad(j).ne.0.)write(1,10000) erad(j),srad(j)            MSSMGE57
CYG10000 format('  ECM =  ',f10.3,'  CROSS SECTION =  ',f10.3)          MSSMGE58
CYG   10   CONTINUE                                                     MSSMGE59
CYG      endif                                                          MSSMGE60
                                                                        MSSMGE61
                                                                        MSSMGE62
      s=ecm**2                                                          MSSMGE63
                                                                        MSSMGE64
      nfail=0                                                           MSSMGE65
                                                                        MSSMGE66
      DO 30 JEV=1,NEVT                                                  MSSMGE67
                                                                        MSSMGE68
   20   continue                                                        MSSMGE69
                                                                        MSSMGE70
        CALL suseve(ifail)                                              MSSMGE71
                                                                        MSSMGE72
        if(ifail.eq.1)then                                              MSSMGE73
          nfail=nfail+1                                                 MSSMGE74
          if(nfail.gt.1000)then                                         MSSMGE75
            print *,' warning nfail = ',nfail,' event = ',jev           MSSMGE76
            print *,' ********  Event skipped  ******** '               MSSMGE77
            nfail=0                                                     MSSMGE78
            goto 30                                                     MSSMGE79
          endif                                                         MSSMGE80
          go to 20                                                      MSSMGE81
        endif                                                           MSSMGE82
                                                                        MSSMGE83
c                                                                       MSSMGE84
c                                                                       MSSMGE85
c write Lund common LUJETS to LUNIT                                     MSSMGE86
c                                                                       MSSMGE87
        if(wrt) CALL SXWRLU(12)                                         MSSMGE88
                                                                        MSSMGE89
        call user                                                       MSSMGE90
                                                                        MSSMGE91
   30 CONTINUE                                                          MSSMGE92
                                                                        MSSMGE93
                                                                        MSSMGE94
      RETURN                                                            MSSMGE95
      END                                                               MSSMGE96
      subroutine branch                                                 BRANCH 2
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**BRANCH 3
*-- Author :    Stavros Katsanevas   14/04/95                          *BRANCH 4
*-- Modified by Y. Gao               06/02/96                          *BRANCH 5
*                                                                      *BRANCH 6
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**BRANCH 7
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               BRANCH 8
                                                                        BRANCH 9
      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw, BRANCH10
     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)                       BRANCH11
                                                                        BRANCH12
      COMMON/OUTMAP/ND,NDIM0,XI(50,10)                                  BRANCH13
                                                                        BRANCH14
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI                    BRANCH15
                                                                        BRANCH16
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   BRANCH17
     +FLC(12),FRC(12),gms(12),echar(12)                                 BRANCH18
                                                                        BRANCH19
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)   BRANCH20
     +,cosmi(12)                                                        BRANCH21
                                                                        BRANCH22
      common/stulim/smin,smax,tmin,tmax,umin,umax                       BRANCH23
                                                                        BRANCH24
      real ala1,ala2,rfm0                                               BRANCH25
                                                                        BRANCH26
      real*4 zr,was,esa                                                 BRANCH27
                                                                        BRANCH28
      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),                             BRANCH29
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)                         BRANCH30
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)     BRANCH31
                                                                        BRANCH32
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)                        BRANCH33
                                                                        BRANCH34
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     BRANCH35
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                BRANCH36
c                                                                       BRANCH37
C index1 =1,6 daughter particles                                        BRANCH38
c index2 =1,6 parent   particles                                        BRANCH39
c i1 =1,6 for 6 kinds of couplings uu,dd,vv,ll,ud,lv                    BRANCH40
                                                                        BRANCH41
                                                                        BRANCH42
      COMMON/SDECAY/DAS,DBS,DCS,DATL,DAUL,DATUL,DASTL,DBSTL,DASUL,      BRANCH43
     +DBSUL,DATR,DAUR,DATUR,DASTR,DBSTR,DASUR,DBSUR,xdec(17,64)         BRANCH44
      DIMENSION CURRENT(17)                                             BRANCH45
      EQUIVALENCE (CURRENT(1),DAS)                                      BRANCH46
                                                                        BRANCH47
      COMMON /CONST/ idbg,igener,irad                                   BRANCH48
                                                                        BRANCH49
                                                                        BRANCH50
      COMMON/XCROS/xgaug(8),xeta(8)                                     BRANCH51
                                                                        BRANCH52
      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)         BRANCH53
      common/curind/index1,index2,i1                                    BRANCH54
                                                                        BRANCH55
                                                                        BRANCH56
      logical logvar                                                    BRANCH57
      logical hexist                                                    BRANCH58
      external hexist                                                   BRANCH59
      external sbrdec                                                   BRANCH60
      real sbrdec                                                       BRANCH61
      external wsc,brodec1,brodec2,brodec3                              BRANCH62
                                                                        BRANCH63
      dimension x(2)                                                    BRANCH64
                                                                        BRANCH65
c                                                                       BRANCH66
c  sparticle widths                                                     BRANCH67
c                                                                       BRANCH68
C carefull formulas below valid only for stop/bottom  below top         BRANCH69
C                                                                       BRANCH70
                                                                        BRANCH71
      do 50 k=1,12                                                      BRANCH72
        cos2=cosmi(k)**2                                                BRANCH73
        sin2=1.d0-cos2                                                  BRANCH74
        widfr(k)=0.                                                     BRANCH75
        widfl(k)=0.                                                     BRANCH76
                                                                        BRANCH77
        do 10 l=1,4                                                     BRANCH78
          index=70+l                                                    BRANCH79
          fmii=ssmass(index)                                            BRANCH80
          brspa(l,k)=0                                                  BRANCH81
          ik1=idecs(k,1)                                                BRANCH82
          fmpart=ssmass(ik1)                                            BRANCH83
          if(fmal(k).le.(fmii+fmpart))go to 10                          BRANCH84
          const=g2/16.d0/pi/fmal(k)**3*(fmal(k)**2-fmii**2)**2          BRANCH85
          k1=mod(k-1,4)+1                                               BRANCH86
          brspa(l,k)=const*(gfil(l,k1)**2*cos2+gfir(l,k1)**2*sin2)      BRANCH87
c                                                                       BRANCH88
c different stop/sbottom                                                BRANCH89
c                                                                       BRANCH90
          if(k.eq.9.or.k.eq.10) brspa(l,k)=0.0000000003d0/fmal(k)**3*   BRANCH91
     +    (fmal(k)**2-fmii**2)**2                                       BRANCH92
c                                                                       BRANCH93
          widfl(k)=widfl(k)+brspa(l,k)                                  BRANCH94
   10   continue                                                        BRANCH95
                                                                        BRANCH96
        do 20 l=5,6                                                     BRANCH97
          index=70+l                                                    BRANCH98
          fmii=ssmass(index)                                            BRANCH99
          brspa(l,k)=0                                                  BRANC100
          ik1=idecs(k,2)                                                BRANC101
          fmpart=ssmass(ik1)                                            BRANC102
          if(fmal(k).le.(fmii+fmpart))go to 20                          BRANC103
          const=g2/16.d0/pi/fmal(k)**3*(fmal(k)**2-fmii**2)**2          BRANC104
          k1=mod(k-1,2)+1                                               BRANC105
          if(k1.eq.1)brspa(l,k)=const*v(l-4,1)**2*cos2                  BRANC106
          if(k1.eq.2)brspa(l,k)=const*u(l-4,1)**2*cos2                  BRANC107
          widfl(k)=widfl(k)+brspa(l,k)                                  BRANC108
   20   continue                                                        BRANC109
                                                                        BRANC110
        do 30 l=1,4                                                     BRANC111
          brspa(l,k+12)=0                                               BRANC112
          index=70+l                                                    BRANC113
          fmii=ssmass(index)                                            BRANC114
          ik1=idecs(k,1)                                                BRANC115
          fmpart=ssmass(ik1)                                            BRANC116
          if(fmar(k).le.(fmii+fmpart))go to 30                          BRANC117
          k1=mod(k-1,4)+1                                               BRANC118
          const=g2/16.d0/pi/fmar(k)**3*(fmar(k)**2-fmii**2)**2          BRANC119
          brspa(l,k+12)=const*(gfil(l,k1)**2*sin2+gfir(l,k1)**2*cos2)   BRANC120
c                                                                       BRANC121
c different stop sbottom                                                BRANC122
c                                                                       BRANC123
          if(k.eq.9.or.k.eq.10) brspa(l,k+12)=0.0000000003d0/fmar(k)**  BRANC124
     +    3*(fmar(k)**2-fmii**2)**2                                     BRANC125
c                                                                       BRANC126
CYG                                                                     BRANC127
CYG this is a bug                                                       BRANC128
CYG       widfr(k)=widfr(k)+brspa(l,k)                                  BRANC129
          widfr(k)=widfr(k)+brspa(l,k+12)                               BRANC130
   30   continue                                                        BRANC131
                                                                        BRANC132
CYG                                                                     BRANC133
CYG this part should be out from the loop                               BRANC134
CYG =====================================                               BRANC135
CYG        do 40 ki=9,9                                                 BRANC136
CYG          do 40 l=5,6                                                BRANC137
CYG            brspa(l,ki+12)=0                                         BRANC138
CYG            index=70+l                                               BRANC139
CYG            fmii=ssmass(index)                                       BRANC140
CYG            ik1=idecs(ki,1)                                          BRANC141
CYG            fmpart=ssmass(ik1)                                       BRANC142
CYG            if(fmar(k).le.(fmii+fmpart))go to 40                     BRANC143
CYG            k1=mod(ki-1,4)+1                                         BRANC144
CYG            const=g2/16.d0/pi/fmar(ki)**3*(fmar(ki)**2-fmii**2)**2   BRANC145
CYG            brspa(l,ki+12)=const*v(l-4,2)**2*gms(9)**2/fmw**2/sinb**2BRANC146
CYG     +      2.d0                                                     BRANC147
CYG            brspa(l,ki+12)=brspa(l,ki+12)*cos2                       BRANC148
CYG                                                                     BRANC149
CYG this is a bug                                                       BRANC150
CYG         widfr(ki)=widfr(ki)+brspa(l,ki)                             BRANC151
CYG            widfr(ki)=widfr(ki)+brspa(l,ki+12)                       BRANC152
CYG   40   continue                                                     BRANC153
                                                                        BRANC154
   50 continue                                                          BRANC155
CYG                                                                     BRANC156
CYG   moved from inside                                                 BRANC157
CYG                                                                     BRANC158
      do 40 ki=9,9                                                      BRANC159
        do 40 l=5,6                                                     BRANC160
          brspa(l,ki+12)=0                                              BRANC161
          index=70+l                                                    BRANC162
          fmii=ssmass(index)                                            BRANC163
          ik1=idecs(ki,1)                                               BRANC164
          fmpart=ssmass(ik1)                                            BRANC165
          if(fmar(ki).le.(fmii+fmpart))go to 40                         BRANC166
          k1=mod(ki-1,4)+1                                              BRANC167
          const=g2/16.d0/pi/fmar(ki)**3*(fmar(ki)**2-fmii**2)**2        BRANC168
          brspa(l,ki+12)=const*v(l-4,2)**2*gms(9)**2/fmw**2/sinb**2/    BRANC169
     +    2.d0                                                          BRANC170
          brspa(l,ki+12)=brspa(l,ki+12)*cos2                            BRANC171
CYG this is a bug                                                       BRANC172
CYG       widfr(ki)=widfr(ki)+brspa(l,ki)                               BRANC173
          widfr(ki)=widfr(ki)+brspa(l,ki+12)                            BRANC174
   40 continue                                                          BRANC175
                                                                        BRANC176
CYG       write (1,*) ' Sparticle widths  (GeV) '                       BRANC177
CYG       write (1,10000) ' SUPR ',widfl(1),' SUPL ',widfr(1)           BRANC178
CYG       write (1,10000) ' SDNR ',widfl(2),' SDNL ',widfr(2)           BRANC179
CYG       write (1,10000) ' SELR ',widfl(4),' SELL ',widfr(4)           BRANC180
CYG       write (1,10000) ' SNU ',widfl(3)                              BRANC181
CYG       write (1,10000) ' STPL ',widfl(9) ,' STPR ',widfr(9)          BRANC182
CYG       write (1,10000) ' SBTL ',widfl(10),' SBTR ',widfr(10)         BRANC183
CYG       write (1,10000) ' STAL ',widfl(12),' STAR ',widfr(12)         BRANC184
                                                                        BRANC185
10000 FORMAT(/a5,f20.10,a7,f20.10)                                      BRANC186
                                                                        BRANC187
      call wiconst                                                      BRANC188
                                                                        BRANC189
      fma=ssmass(71)                                                    BRANC190
      fmb=ssmass(75)                                                    BRANC191
                                                                        BRANC192
      indx=0                                                            BRANC193
                                                                        BRANC194
      do 80 index1=1,6                                                  BRANC195
        do 80 index2=1,6                                                BRANC196
          do 80 i1=1,18                                                 BRANC197
                                                                        BRANC198
                                                                        BRANC199
            brgaug(i1,index2,index1)=0.                                 BRANC200
                                                                        BRANC201
c                                                                       BRANC202
c products                                                              BRANC203
c                                                                       BRANC204
            li1=mod(i1-1,6)+1                                           BRANC205
            nc=lind(li1,index2,index1)                                  BRANC206
            if(nc.eq.0)go to 80                                         BRANC207
c                                                                       BRANC208
            fmi=ssmass(70+index1)                                       BRANC209
            fmk=ssmass(70+index2)                                       BRANC210
                                                                        BRANC211
                                                                        BRANC212
CYG get them all                                                        BRANC213
CYG            if(index1.le.4.and.(fmi+fma).gt.ecm)go to 80             BRANC214
CYG            if(index1.gt.4.and.(fmi+fmb).gt.ecm)go to 80             BRANC215
CYG                                                                     BRANC216
                                                                        BRANC217
c                                                                       BRANC218
c find mass of 2 fermion products                                       BRANC219
c                                                                       BRANC220
            imk1=kl(1,i1)                                               BRANC221
            imk2=kl(2,i1)                                               BRANC222
                                                                        BRANC223
            fml1=ssmass(imk1)                                           BRANC224
            fml2=ssmass(imk2)                                           BRANC225
                                                                        BRANC226
c                                                                       BRANC227
c check whether I have enough energy to produce them                    BRANC228
c                                                                       BRANC229
            q=fmi-fmk-fml1-fml2                                         BRANC230
            if(q.le.0.)go to 80                                         BRANC231
                                                                        BRANC232
            smin=(fml1+fml2)**2                                         BRANC233
            smax=(fmi-fmk)**2                                           BRANC234
                                                                        BRANC235
            umin=(fmk+fml2)**2                                          BRANC236
            umax=(fmi-fml1)**2                                          BRANC237
                                                                        BRANC238
            tmin=(fmk+fml1)**2                                          BRANC239
            tmax=(fmi-fml2)**2                                          BRANC240
                                                                        BRANC241
                                                                        BRANC242
            if(smin.ge.smax)go to 80                                    BRANC243
            if(umin.ge.umax)go to 80                                    BRANC244
            if(tmin.ge.tmax)go to 80                                    BRANC245
                                                                        BRANC246
c                                                                       BRANC247
c intermediate particles                                                BRANC248
c                                                                       BRANC249
                                                                        BRANC250
            fms=fmz                                                     BRANC251
            gw=fmz*gammaz                                               BRANC252
                                                                        BRANC253
            if(index2.gt.4.and.index1.le.4)then                         BRANC254
              fms=fmw                                                   BRANC255
              gw=fmw*gammaw                                             BRANC256
            endif                                                       BRANC257
                                                                        BRANC258
            if(index1.gt.4.and.index2.le.4)then                         BRANC259
              fms=fmw                                                   BRANC260
              gw=fmw*gammaw                                             BRANC261
            endif                                                       BRANC262
                                                                        BRANC263
                                                                        BRANC264
            lk1= klap(1,i1)                                             BRANC265
            lk2= klap(2,i1)                                             BRANC266
                                                                        BRANC267
                                                                        BRANC268
            do 60 k=1,12                                                BRANC269
              if(ispa(k,1).eq.lk1)then                                  BRANC270
                                                                        BRANC271
                fmelt=fmal(k)                                           BRANC272
                fmert=fmar(k)                                           BRANC273
                fmeltw=widfl(k)*fmelt                                   BRANC274
                fmertw=widfr(k)*fmert                                   BRANC275
c                                                                       BRANC276
c different for 3d generation because of mixing.                        BRANC277
c                                                                       BRANC278
                if(k.ge.9)then                                          BRANC279
                  cos2=cosmi(k)**2                                      BRANC280
                  sin2=1.d0-cos2                                        BRANC281
                  fmelt=dsqrt(fmal(k)**2*cos2+fmar(k)*sin2)             BRANC282
                  fmert=dsqrt(fmar(k)**2*cos2+fmal(k)*sin2)             BRANC283
                  fmeltw=(widfl(k)*cos2+widfr(k)*sin2)*fmelt            BRANC284
                  fmertw=(widfr(k)*cos2+widfl(k)*sin2)*fmert            BRANC285
                endif                                                   BRANC286
                                                                        BRANC287
              endif                                                     BRANC288
              if(ispa(k,1).eq.lk2)then                                  BRANC289
                fmelu=fmal(k)                                           BRANC290
                fmeru=fmar(k)                                           BRANC291
                fmeluw=widfl(k)*fmelu                                   BRANC292
                fmeruw=widfr(k)*fmeru                                   BRANC293
c                                                                       BRANC294
c different for 3d generation because of mixing.                        BRANC295
c                                                                       BRANC296
                if(k.ge.9)then                                          BRANC297
                  cos2=cosmi(k)**2                                      BRANC298
                  sin2=1.d0-cos2                                        BRANC299
                  fmelu=dsqrt(fmal(k)**2*cos2+fmar(k)*sin2)             BRANC300
                  fmeru=dsqrt(fmar(k)**2*cos2+fmal(k)*sin2)             BRANC301
                  fmeluw=(widfl(k)*cos2+widfr(k)*sin2)*fmelu            BRANC302
                  fmeruw=(widfr(k)*cos2+widfl(k)*sin2)*fmeru            BRANC303
                endif                                                   BRANC304
              endif                                                     BRANC305
                                                                        BRANC306
   60       continue                                                    BRANC307
                                                                        BRANC308
                                                                        BRANC309
            etai=xeta(index1)                                           BRANC310
            etak=xeta(index2)                                           BRANC311
c                                                                       BRANC312
c decay constants                                                       BRANC313
c                                                                       BRANC314
            do 70 j=1,17                                                BRANC315
   70       current(j)=xdec(j,nc)                                       BRANC316
                                                                        BRANC317
            fa1=ssdint(smin,wsc,smax)                                   BRANC318
                                                                        BRANC319
c      call VEGAS(brodec1,0.005d0,2,5000,20,0,0,avgi1,SD1,IT1)          BRANC320
c      call VEGAS(brodec2,0.005d0,2,5000,20,0,0,avgi2,SD2,IT2)          BRANC321
c      call VEGAS(brodec3,0.005d0,2,5000,20,0,0,avgi3,SD3,IT3)          BRANC322
c      avgi=avgi1+avgi2+avgi3                                           BRANC323
c      if(avgi.eq.0.)go to 4                                            BRANC324
c      diff=(avgi-fa1)/fa1*100.                                         BRANC325
c      idiff=diff                                                       BRANC326
c      if(dabs(diff).gt.2.d0)then                                       BRANC327
c      print *,' WARNING '                                              BRANC328
c      print *,' vegas differs from analytical by ',idiff,'%'           BRANC329
c      print *,' vegas ',avgi,it1,it2,it3                               BRANC330
c      print *,' analytical ',fa1                                       BRANC331
c      print *,' decay type  ',index1,index2,i1                         BRANC332
c      endif                                                            BRANC333
                                                                        BRANC334
            brgaug(i1,index2,index1)=fa1                                BRANC335
                                                                        BRANC336
            if(igener.eq.0)go to 80                                     BRANC337
c                                                                       BRANC338
c prepare throwing matrix                                               BRANC339
c                                                                       BRANC340
            indx=indx+1                                                 BRANC341
            if(indx.gt.168)then                                         BRANC342
              print *,' error more than 168 BR '                        BRANC343
              stop 99                                                   BRANC344
            endif                                                       BRANC345
                                                                        BRANC346
            linda(i1,index2,index1)=indx                                BRANC347
                                                                        BRANC348
c      gentl(1,1,indx)=smin                                             BRANC349
c      gentl(2,1,indx)=smax                                             BRANC350
c      gentl(1,2,indx)=umin                                             BRANC351
c      gentl(2,2,indx)=umax                                             BRANC352
c      do 441 li=1,50                                                   BRANC353
c      sb=xi(li,1)*(smax-smin)+smin                                     BRANC354
c      ub=xi(li,2)*(umax-umin)+umin                                     BRANC355
c      gent(li,1,indx)=sb                                               BRANC356
c      gent(li,2,indx)=ub                                               BRANC357
c 441  continue                                                         BRANC358
                                                                        BRANC359
            nbin1=30                                                    BRANC360
            nbin2=30                                                    BRANC361
            logvar=hexist(indx)                                         BRANC362
            if(logvar)call hdelet(indx)                                 BRANC363
            call hbfun2(indx,' ',nbin1,sngl(smin),sngl(smax) ,nbin2,    BRANC364
     +      sngl(umin),sngl(umax),sbrdec)                               BRANC365
                                                                        BRANC366
   80 continue                                                          BRANC367
                                                                        BRANC368
                                                                        BRANC369
      CALL INTERF                                                       BRANC370
                                                                        BRANC371
      return                                                            BRANC372
      end                                                               BRANC373
      double precision FUNCTION wsc(sb)                                 WSC    2
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**WSC    3
*                                                                      *WSC    4
*-- Author :    Stavros Katsanevas   14/04/95                          *WSC    5
*-- Modified by Y.GAO to deal the crash with right sneutrinos  6/02/96 *WSC    6
****$****|****$****|****$****|****$****|****$****|****$****|****$****|**WSC    7
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               WSC    8
                                                                        WSC    9
      COMMON/SDECAY/DAS,DBS,DCS,DATL,DAUL,DATUL,DASTL,DBSTL,DASUL,      WSC   10
     +DBSUL,DATR,DAUR,DATUR,DASTR,DBSTR,DASUR,DBSUR,xdec(17,64)         WSC   11
                                                                        WSC   12
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,   WSC   13
     +FLC(12),FRC(12),gms(12),echar(12)                                 WSC   14
                                                                        WSC   15
      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),     WSC   16
     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru                WSC   17
                                                                        WSC   18
      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw, WSC   19
     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)                       WSC   20
      common/mass/sfmi,sfmk,sfms,sgw,sfmelt,sfmelu,alt,alu,c0           WSC   21
CYG                                                                     WSC   22
CYG   give a non-zero width to right sneutrinos, it will not            WSC   23
CYG   affect the results                                                WSC   24
CYG                                                                     WSC   25
CBBL   use d-35 instead of d-50 to please some compilers ....           BBL001 2
      if( fmert.gt.9999.0d0 .and. fmertw.le.1.0d-35 ) then              BBL001 3
        YGfmertw=0.01d0                                                 WSC   27
      else                                                              WSC   28
        YGfmertw=fmertw                                                 WSC   29
      endif                                                             WSC   30
                                                                        WSC   31
      if( fmeru.gt.9999.0d0 .and. fmeruw.le.1.0d-35 ) then              BBL001 4
        YGfmeruw=0.01d0                                                 WSC   33
      else                                                              WSC   34
        YGfmeruw=fmeruw                                                 WSC   35
      endif                                                             WSC   36
CYG                                                                     WSC   37
CYG                                                                     WSC   38
      wsc=0.                                                            WSC   39
                                                                        WSC   40
      sfmi=fmi                                                          WSC   41
      sfmk=fmk                                                          WSC   42
      sfms=fms                                                          WSC   43
      sgw=gw                                                            WSC   44
                                                                        WSC   45
      c0=fmi**2+fmk**2+fml1**2+fml2**2                                  WSC   46
c                                                                       WSC   47
c t sneutrino                                                           WSC   48
c                                                                       WSC   49
      GW1 = ((SB+FML1**2-FML2**2)/(2.*DSQRT(SB))+                       WSC   50
     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2                    WSC   51
      GW2 = ((SB+FML1**2-FML2**2)**2/(4.*SB)-FML1**2)                   WSC   52
      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)                      WSC   53
      if(GW2.le.0..or.GW3.le.0.)return                                  WSC   54
      GW2 = DSQRT(GW2)                                                  WSC   55
      GW3 = DSQRT(GW3)                                                  WSC   56
      TMIN = GW1-(GW2+GW3)**2                                           WSC   57
      TMAX = GW1-(GW2-GW3)**2                                           WSC   58
c                                                                       WSC   59
c  u= charged slepton                                                   WSC   60
c                                                                       WSC   61
      GW1 = ((SB+FML2**2-FML1**2)/(2.*DSQRT(SB))+                       WSC   62
     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2                    WSC   63
      GW2 = ((SB+FML2**2-FML1**2)**2/(4.*SB)-FML2**2)                   WSC   64
      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)                      WSC   65
      if(GW2.le.0..or.GW3.le.0.)return                                  WSC   66
      GW2 = DSQRT(GW2)                                                  WSC   67
      GW3 = DSQRT(GW3)                                                  WSC   68
      UMIN = GW1-(GW2+GW3)**2                                           WSC   69
      UMAX = GW1-(GW2-GW3)**2                                           WSC   70
                                                                        WSC   71
c                                                                       WSC   72
c s channel integrals                                                   WSC   73
c                                                                       WSC   74
                                                                        WSC   75
      f1=ff1(sb,tmax)-ff1(sb,tmin)                                      WSC   76
      f2=ff1(sb,umax)-ff1(sb,umin)                                      WSC   77
      f3=ff2(sb,tmax)-ff2(sb,tmin)                                      WSC   78
                                                                        WSC   79
      wsc1=das*f1+dbs*f2+2.*dcs*etai*etak*fmi*fmk*f3                    WSC   80
c                                                                       WSC   81
c t channel                                                             WSC   82
c                                                                       WSC   83
                                                                        WSC   84
      sfmelt=fmelt                                                      WSC   85
      alt=fmeltw                                                        WSC   86
      f4l=ff3(sb,tmax)-ff3(sb,tmin)                                     WSC   87
                                                                        WSC   88
      sfmelt=fmert                                                      WSC   89
      alt=YGfmertw                                                      WSC   90
      f4r=ff3(sb,tmax)-ff3(sb,tmin)                                     WSC   91
                                                                        WSC   92
      wsc2=datl*f4l+datr*f4r                                            WSC   93
c                                                                       WSC   94
c u channel                                                             WSC   95
c                                                                       WSC   96
      sfmelt=fmelu                                                      WSC   97
      alt=fmeluw                                                        WSC   98
      f5l=ff3(sb,umax)-ff3(sb,umin)                                     WSC   99
                                                                        WSC  100
      sfmelt=fmeru                                                      WSC  101
      alt=YGfmeruw                                                      WSC  102
      f5r=ff3(sb,umax)-ff3(sb,umin)                                     WSC  103
                                                                        WSC  104
      wsc3=daul*f5l+daur*f5r                                            WSC  105
                                                                        WSC  106
c                                                                       WSC  107
c st channel                                                            WSC  108
c                                                                       WSC  109
                                                                        WSC  110
      sfmelt=fmelt                                                      WSC  111
      alt=fmeltw                                                        WSC  112
      f4l=ff3(sb,tmax)-ff3(sb,tmin)                                     WSC  113
      f6l=ff4(sb,tmax)-ff4(sb,tmin)                                     WSC  114
      f7l=ff5(sb,tmax)-ff5(sb,tmin)                                     WSC  115
                                                                        WSC  116
      sfmelt=fmert                                                      WSC  117
      alt=YGfmertw                                                      WSC  118
      f4r=ff3(sb,tmax)-ff3(sb,tmin)                                     WSC  119
      f6r=ff4(sb,tmax)-ff4(sb,tmin)                                     WSC  120
      f7r=ff5(sb,tmax)-ff5(sb,tmin)                                     WSC  121
                                                                        WSC  122
      wsc4=2.*dastl*f7l+dastl*gw*f4l/((sb-fms**2)**2+gw**2)             WSC  123
     +    +2.*dbstl*etai*etak*fmi*fmk*sb*f6l                            WSC  124
     +    +2.*dastr*f7r+dastr*gw*f4r/((sb-fms**2)**2+gw**2)             WSC  125
     +    +2.*dbstr*etai*etak*fmi*fmk*sb*f6r                            WSC  126
c                                                                       WSC  127
c su channel                                                            WSC  128
c                                                                       WSC  129
                                                                        WSC  130
      sfmelt=fmelu                                                      WSC  131
      alt=fmeluw                                                        WSC  132
      f5l=ff3(sb,umax)-ff3(sb,umin)                                     WSC  133
      f8l=ff4(sb,umax)-ff4(sb,umin)                                     WSC  134
      f9l=ff5(sb,umax)-ff5(sb,umin)                                     WSC  135
                                                                        WSC  136
      sfmelt=fmeru                                                      WSC  137
      alt=YGfmeruw                                                      WSC  138
      f5r=ff3(sb,umax)-ff3(sb,umin)                                     WSC  139
      f8r=ff4(sb,umax)-ff4(sb,umin)                                     WSC  140
      f9r=ff5(sb,umax)-ff5(sb,umin)                                     WSC  141
                                                                        WSC  142
      wsc5=2.*dasul*f9l+dasul*gw*f5l/((sb-fms**2)**2+gw**2)             WSC  143
     +    +2.*dbsul*etai*etak*fmi*fmk*sb*f8l                            WSC  144
     +    +2.*dasur*f9r+dasur*gw*f5r/((sb-fms**2)**2+gw**2)             WSC  145
     +    +2.*dbsur*etai*etak*fmi*fmk*sb*f8r                            WSC  146
                                                                        WSC  147
                                                                        WSC  148
c                                                                       WSC  149
c tu channel integrals                                                  WSC  150
c                                                                       WSC  151
      sfmelt=fmelt                                                      WSC  152
      alt=fmeltw                                                        WSC  153
      sfmelu=fmelu                                                      WSC  154
      alu=fmeluw                                                        WSC  155
      altl=alt                                                          WSC  156
      alul=alu                                                          WSC  157
      f01l=ff6(sb,tmax)-ff6(sb,tmin)                                    WSC  158
      f02l=ff7(sb,tmax)-ff7(sb,tmin)                                    WSC  159
                                                                        WSC  160
      sfmelt=fmert                                                      WSC  161
      alt=YGfmertw                                                      WSC  162
      sfmelu=fmeru                                                      WSC  163
      alu=YGfmeruw                                                      WSC  164
      altr=alt                                                          WSC  165
      alur=alu                                                          WSC  166
      f01r=ff6(sb,tmax)-ff6(sb,tmin)                                    WSC  167
      f02r=ff7(sb,tmax)-ff7(sb,tmin)                                    WSC  168
                                                                        WSC  169
      wsc6=2.*datul*etai*etak*fmi*fmk*sb*(altl*alul*f01l+f02l)          WSC  170
     +    +2.*datur*etai*etak*fmi*fmk*sb*(altr*alur*f01r+f02r)          WSC  171
                                                                        WSC  172
      wsc=wsc1+wsc2+wsc3+wsc4+wsc5+wsc6                                 WSC  173
      wsc=wsc*alpha**2/32./pi/sinw**4/fmi**3                            WSC  174
                                                                        WSC  175
      RETURN                                                            WSC  176
      END                                                               WSC  177
