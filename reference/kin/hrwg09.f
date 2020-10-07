      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C                                                                       ASKUSI 3
C --------------------------------------------------------------------  ASKUSI 4
C Ebi Lange  December 1988.                                             ASKUSI 5
C Modified - A.S.Thompson, February 1992.                               ASKUSI 6
C Modified - A.S.Thompson, August 1992.                                 ASKUSI 7
C Modified - A.S.Thompson, November 1993.                               ASKUSI 8
C --------------------------------------------------------------------  ASKUSI 9
C                                                                       ASKUSI10
      IMPLICIT NONE                                                     ASKUSI11
C                                                                       ASKUSI12
                                                                        ASKUSI13
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(1000)                                                     BCS    6
      COMMON /BCS/   IW(1000)                                           BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      INTEGER NCOL,MAXFLA                                               DTMINP 2
C                                                                       DTMINP 3
      PARAMETER (NCOL = 41)                                             DTMINP 4
      PARAMETER (MAXFLA=7)                                              DTMINP 5
C                                                                       DTMINP 6
      INTEGER NEVENT,IFLC,IOUT,IDEBB,IDEBE                              DTMINP 7
      REAL SDVRT,TABL                                                   DTMINP 8
C                                                                       DTMINP 9
      COMMON / DTMILL / SDVRT(3),TABL(NCOL),NEVENT(10),IFLC(MAXFLA)     DTMINP10
      COMMON / INPOUT / IOUT                                            DTMINP11
      COMMON / DTBUG / IDEBB,IDEBE                                      DTMINP12
C                                                                       DTMINP13
      INTEGER IHARD,IFL                                                 INIPRO 2
C                                                                       INIPRO 3
      COMMON /INIPRO/ IHARD,IFL                                         INIPRO 4
C                                                                       INIPRO 5
      INTEGER JPARGN,JPARNA,JPARCO,JPARMA,JPARCH,JPARLT,                PARTJJ 2
     &        JPARMW,JPARAN,LPARTA                                      PARTJJ 3
      PARAMETER(JPARGN=1,JPARNA=2,JPARCO=5,JPARMA=6,JPARCH=7,JPARLT=8,  PARTJJ 4
     +          JPARMW=9,JPARAN=10,LPARTA=10)                           PARTJJ 5
C                                                                       PARTJJ 6
C                                                                       ASKUSI18
      COMMON/ZDUMP / IDEBU , NEVT, IOUTDY                               ASKUSI19
      COMMON /WEAK/ WEAKPAR(11)                                         ASKUSI20
      COMMON /BEAM/ S0,EBEAM                                            ASKUSI21
      INTEGER IDEBU,NEVT,IOUTDY                                         ASKUSI22
      REAL*4 WEAKPAR,S0,EBEAM                                           ASKUSI23
      CHARACTER*4 CHAINT,CHBDEC(3)                                      ASKUSI24
C                                                                       ASKUSI25
      INTEGER NAMIND                                                    ASKUSI26
      INTEGER DUMMY                                                     ASKUSI27
      REAL*4 HWRGET                                                     ASKUSI28
C                                                                       ASKUSI29
      INTEGER ALTABL,ALRLEP                                             ASKUSI30
      EXTERNAL ALTABL,ALRLEP                                            ASKUSI31
C                                                                       ASKUSI32
      INTEGER HEDE,IGCO                                                 ASKUSI33
C                                                                       ASKUSI34
      PARAMETER (IGCO = 5124)                                           ASKUSI35
      PARAMETER (HEDE = 52)                                             ASKUSI36
C                                                                       ASKUSI37
      INTEGER HEDEC(HEDE)                                               ASKUSI38
                                                                        ASKUSI39
C                                                                       ASKUSI40
C common blocks for HERWIG                                              ASKUSI41
C                                                                       ASKUSI42
      DOUBLE PRECISION                                                  HWDEFI 2
     & PHEP,VHEP,PBEAM1,PBEAM2,QCDLAM,VGCUT,VQCUT,VPCUT,BETAF,CAFAC,    HWDEFI 3
     & CFFAC,CLMAX,CLPOW,PSPLT,QSPAC,PTRMS,PXRMS,QG,QV,SWEIN,SCABI,     HWDEFI 4
     & PDIQK,QDIQK,ENSOF,TMTOP,ZBINM,GAMW,GAMZ,GAMH,PGSMX,PGSPL,PPAR,   HWDEFI 5
     & VPAR,PHIPAR,DECPAR,RHOPAR,RHOHEP,XFACT,PTINT,EVWGT,AVWGT,WGTMAX, HWDEFI 6
     & WGTSUM,WSQSUM,WBIGST,TLOUT,YJMIN,YJMAX,PTMIN,PTMAX,PTPOW,EMMIN,  HWDEFI 7
     & EMMAX,EMPOW,Q2MIN,Q2MAX,Q2POW,THMAX,QLIM,XXMIN,XLMIN,EMSCA,      HWDEFI 8
     & EMLST,COSTH,GPOLN,GCOEF,XX,DISF,RESN,RMIN,CTMAX,FBTM,            HWDEFI 9
     & FTOP,FHVY,RMASS,BFRAC,CMMOM,ACCUR,QEV,SUD,VECWT,TENWT,DECWT,     HWDEFI10
     & QWT,PWT,SWT,SWTEF,RESWT,PIFAC,QCDL3,QCDL5,BRHIG,GAMMAX,          HWDEFI11
     & ENHANC,B1LIM,ALPFAC,Q2WWMN,Q2WWMX,BTCLM,ETAMIX,                  HWDEFI12
     & QFCH,VFCH,AFCH,VCKM,TQWT,CLQ,EPOLN,PPOLN,COSS,SINS,GAMZP,GEV2NB, HWDEFI13
     & ALPHEM,PRSOF,EBEAM1,EBEAM2,ZJMAX,YBMIN,YBMAX,HARDST,CLSMR,PHOMAS HWDEFI14
C                                                                       HWDEFI15
      INTEGER NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP,            HWDEFI16
     & IPROC,MAXEV,IPRINT,LRSUD,LWSUD,NCOLO,NFLAV,MODPDF(2),NSTRU,      HWDEFI17
     & NZBIN,NBTRY,NCTRY,NDTRY,NETRY,NSTRY,NGSPL,NMXPAR,NEVPAR,         HWDEFI18
     & NPAR,ISTPAR,IDPAR,JMOPAR,JDAPAR,JCOPAR,INHAD,JNHAD,NSPAC,NRN,    HWDEFI19
     & MAXER,MAXPR,LWEVT,ISTAT,IERROR,NWGTS,IDHW,IBSH,IBRN,IPRO,        HWDEFI20
     & IFLMIN,IFLMAX,MAXFL,IDCMF,IHPRO,IDN,ICO,LOCN,                    HWDEFI21
     & NRES,IDPDG,ICHRG,MADDR,MODES,MODEF,IDPRO,INTER,NQEV,             HWDEFI22
     & NSUD,NMXSUD,MODBOS,IOPHIG,MODMAX,SUDORD,CLDIR,NUMER,NUMERU,      HWDEFI23
     & MAPQ,IPART1,IPART2,ISPAC,IAPHIG                                  HWDEFI24
C                                                                       HWDEFI25
      LOGICAL AZSOFT,AZSPIN,FROST,GENEV,BGSHAT,NOWGT,TMPAR,PRNDEC,      HWDEFI26
     & HVFCEN,FSTEVT,FSTWGT,BREIT,USECMF,ZPRIME,TPOL,GENSOF,            HWDEFI27
     & HARDME,SOFTME,NOSPAC                                             HWDEFI28
C                                                                       HWDEFI29
      CHARACTER*4 PART1,PART2,RNAME,BDECAY                              HWDEFI30
      CHARACTER*20 AUTPDF(2)                                            HWDEFI31
C                                                                       HWDEFI32
      PARAMETER (NMXHEP=2000)                                           HWDEFI33
      INTEGER NMXJET                                                    HWDEFI34
      PARAMETER (NMXJET=200)                                            HWDEFI35
C                                                                       HWDEFI36
      COMMON/HWBEAM/PART1,PART2,IPART1,IPART2                           HWBEAM 2
      COMMON/HWEVNT/EVWGT,AVWGT,WGTMAX,WGTSUM,WSQSUM,WBIGST,TLOUT,      HWEVNT 2
     & NRN(2),MAXER,NUMER,NUMERU,MAXPR,LWEVT,ISTAT,IERROR,NOWGT,NWGTS,  HWEVNT 3
     & IDHW(NMXHEP),GENSOF                                              HWEVNT 4
      PARAMETER (MODMAX=5)                                              HWBOSC 2
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),HWBOSC 3
     &  MODBOS(MODMAX),IOPHIG                                           HWBOSC 4
C                                                                       HWBOSC 5
      COMMON/HWHARD/YJMIN,YJMAX,PTMIN,PTMAX,PTPOW,EMMIN,EMMAX,EMPOW,    HWHARD 2
     & Q2MIN,Q2MAX,Q2POW,Q2WWMN,Q2WWMX,THMAX,QLIM,XXMIN,XLMIN,ZJMAX,    HWHARD 3
     & YBMIN,YBMAX,EMSCA,EMLST,COSTH,CTMAX,GPOLN,GCOEF(7),XX(2),PHOMAS, HWHARD 4
     & DISF(13,2),TQWT,CLQ(7,6),MAPQ(6),EPOLN(3),PPOLN(3),COSS,SINS,    HWHARD 5
     & IBSH,IBRN(2),IPRO,IFLMIN,IFLMAX,MAXFL,IDCMF,IHPRO,               HWHARD 6
     & IDN(10),ICO(10),GENEV,FSTEVT,FSTWGT,BGSHAT,HVFCEN,               HWHARD 7
     & TPOL,IAPHIG                                                      HWHARD 8
C---UTILITIES COMMON                                                    HWHARD 9
      COMMON/HWUCLU/RESN(12,12),RMIN(12,12),LOCN(12,12)                 HWHARD10
      COMMON/HWPRAM/QCDLAM,VGCUT,VQCUT,VPCUT,PIFAC,BETAF,CAFAC,CFFAC,   HWPRAM 2
     & CLMAX,CLPOW,PSPLT,QSPAC,PTRMS,PXRMS,QG,QV,SWEIN,SCABI,PDIQK,     HWPRAM 3
     & QDIQK,BTCLM,ENSOF,TMTOP,ZBINM,GAMW,GAMZ,GAMH,QCDL3,QCDL5,B1LIM,  HWPRAM 4
     & PGSMX,PGSPL(4),QFCH(16),VFCH(16,2),AFCH(16,2),VCKM(3,3),         HWPRAM 5
     & GAMZP,GEV2NB,ALPHEM,PRSOF,CLSMR,                                 HWPRAM 6
     & IPRINT,LRSUD,LWSUD,NCOLO,NFLAV,MODPDF,NSTRU,NZBIN,NBTRY,         HWPRAM 7
     & NCTRY,NDTRY,NETRY,NSTRY,NGSPL,AZSOFT,AZSPIN,CLDIR,PRNDEC,ZPRIME, HWPRAM 8
     & HARDME,SOFTME,ISPAC,NOSPAC,                                      HWPRAM 9
     & AUTPDF                                                           HWPRAM10
C                                                                       HWPRAM11
      COMMON/HWPROC/PBEAM1,PBEAM2,EBEAM1,EBEAM2,IPROC,MAXEV             HWPROC 2
C                                                                       HWPROC 3
      COMMON/HWUFHV/FBTM(6,2),FTOP(6,2),FHVY(6,2),BDECAY                HWUFHV 2
      COMMON/HWUNAM/RNAME(264)                                          HWUNAM 2
      COMMON/HWUPDT/RMASS(264),BFRAC(460),CMMOM(460),ETAMIX,IDPDG(264), HWUPDT 2
     & ICHRG(264),MADDR(264),MODES(264),MODEF(264),IDPRO(3,460),NRES    HWUPDT 3
C---MAX NUMBER OF ENTRIES IN LOOKUP TABLES OF SUDAKOV FORM FACTORS      HWUPDT 4
      COMMON/HWUWTS/VECWT,TENWT,DECWT,QWT(3),PWT(12),SWT(264),          HWUWTS 2
     & SWTEF(264),RESWT(264)                                            HWUWTS 3
                                                                        HWUWTS 4
      PARAMETER (NMXSUD=1024)                                           HWUSUD 2
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,  HWUSUD 3
     & SUDORD                                                           HWUSUD 4
C                                                                       ASKUSI55
      INTEGER IGCOD,NDAT                                                ASKUSI56
      INTEGER IPART,IKLIN,IPPART,JGHRW,IQUARK,JGGSW,JGBDE,IIBDEC        ASKUSI57
      INTEGER JGSPAR,NAMI,JDEBU,JGFSR,JGSUD,JGPRC,JGPOL                 ASKUSI58
      INTEGER JGPAR,JGMAS,MNUM,INPART,IANTI,JGSTA                       ASKUSI59
      INTEGER MXDEC,NROW                                                ASKUSI60
      INTEGER KGPART,KNODEC,I,JSVRT,JKPAR,IEBEAM,JRLEP,JANTI            ASKUSI61
C                                                                       ASKUSI62
      REAL RAZSOF,RAZSPI,XTOT                                           ASKUSI63
      REAL*8 ECMS                                                       ASKUSI64
C                                                                       ASKUSI65
      INTEGER ID,NRBOS,L                                                BMACRO 2
      INTEGER LCOLS,LROWS,KNEXT,KROW,LFRWRD,LFRROW,ITABL                BMACRO 3
      REAL RTABL                                                        BMACRO 4
C                                                                       BMACRO 5
      INTEGER NAPART,JVK,JPA,NLINK                                      BMACRO 6
      INTEGER KNEXVK,LFRVK,KPARVK,KLISVK,NOFVK,KINTYP,INPTRK,           BMACRO 7
     &        INPVRT,MOTHVK                                             BMACRO 8
      REAL CHARGE,PARMAS,TIMLIF,PMODVK,ENERVK,TOFLIT,RADVK,PMASVK       BMACRO 9
C - # of words/row in bank with index ID                                BMACRO10
      LCOLS(ID) = IW(ID+1)                                              BMACRO11
C - # of rows in bank with index ID                                     BMACRO12
      LROWS(ID) = IW(ID+2)                                              BMACRO13
C - index of next row in the bank with index ID                         BMACRO14
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO15
C - index of row # NRBOS in the bank with index ID                      BMACRO16
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO17
C - # of free words in the bank with index ID                           BMACRO18
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO19
C - # of free rows in the bank with index ID                            BMACRO20
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO21
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO22
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO23
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO24
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO25
C                                                                       BMACRO26
C - index of the next vertex/track to be stored in KINE/VERT            KMACRO 2
C   bank known by its index JVK                                         KMACRO 3
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)                 KMACRO 4
C - # of vertices/tracks which could be stored in KINE/VERT             KMACRO 5
C   bank known by its index JVK                                         KMACRO 6
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))           KMACRO 7
C - index of the 1st parameter of KINE/VERT bank known by its           KMACRO 8
C   index JVK                                                           KMACRO 9
      KPARVK(JVK) = JVK + IW(JVK+1)                                     KMACRO10
C - index of 1st vertex/track # contained into the list of              KMACRO11
C   bank KINE/VERT known by its index JVK                               KMACRO12
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)                         KMACRO13
C - charge of ALEPH particle# JPA                                       KMACRO14
      CHARGE(JPA) = RTABL(IW(NAPART),JPA,7)                             KMACRO15
C - mass of ALEPH particle# JPA                                         KMACRO16
      PARMAS(JPA) = RTABL(IW(NAPART),JPA,6)                             KMACRO17
C - time of life of ALEPH particle# JPA                                 KMACRO18
      TIMLIF(JPA) = RTABL(IW(NAPART),JPA,8)                             KMACRO19
C - # of vertices on a track known by its BOS index /                   KMACRO20
C   # of outgoing tracks of a vertex known by its BOS index             KMACRO21
      NOFVK(JVK)  = IW(JVK+3)                                           KMACRO22
C - Particle type of a track known by its BOS index                     KMACRO23
      KINTYP(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO24
C - incoming track # of a vertex known by its BOS index                 KMACRO25
      INPTRK(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO26
C - origin vertex # of a track known by its BOS index                   KMACRO27
      INPVRT(JVK) = IW(KLISVK(JVK)+1)                                   KMACRO28
C - momentum of a track known by its BOS index                          KMACRO29
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2   KMACRO30
     &                   +RW(KPARVK(JVK)+3)**2)                         KMACRO31
C - MASS of a track known by its BOS index  JVK                         KMACRO32
      PMASVK(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO33
C - energy of a track known by its BOS index JVK                        KMACRO34
      ENERVK(JVK) = SQRT(PMODVK(JVK)**2+PMASVK(JVK)**2)                 KMACRO35
C - time of flight of the icoming particle to the vertex known by its   KMACRO36
C   BOS index JVK                                                       KMACRO37
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO38
C - radius of the vertex known by its BOS index                         KMACRO39
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)  KMACRO40
C - mother track # of a track known by its BOS index                    KMACRO41
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))                  KMACRO42
C                                                                       KMACRO43
C                                                                       ASKUSI68
      DATA CHBDEC /'HERW','EURO','CLEO'/                                ASKUSI69
      DATA NAPART /0/                                                   ASKUSI70
C                                                                       ASKUSI71
      IF (NAPART.EQ.0) NAPART = NAMIND('PART')                          ASKUSI74
C                                                                       ASKUSI75
C   Return the generator code as defined in the KINGAL library          ASKUSI76
C                                                                       ASKUSI77
      IGCOD = IGCO                                                      ASKUSI78
      IOUT = IW(6)                                                      ASKUSI79
      WRITE(IOUT,101) IGCOD                                             ASKUSI80
 101  FORMAT(/,10X,                                                     ASKUSI81
     & 'HRWG09 - Code Number = ',I4,' Last Modifications ',             ASKUSI82
     $' October 9th  1995'                                              BBL001 1
     & ,/,10X,'***********************************************',        ASKUSI84
     &'********************',//)                                        ASKUSI85
C                                                                       ASKUSI86
C initialization                                                        ASKUSI87
C                                                                       ASKUSI88
      NDAT = 0                                                          ASKUSI89
C                                                                       ASKUSI90
C create the KLIN bank and complete the PART  bank                      ASKUSI91
C                                                                       ASKUSI92
      CALL KXHEPA(IPART,IKLIN)                                          ASKUSI93
C                                                                       ASKUSI94
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUSI95
         WRITE (IOUT,'(1X,''+++ASKUSI+++ IPART IKLIN '',2I5)')          ASKUSI96
     &          IPART,IKLIN                                             ASKUSI97
         WRITE(IOUT,'('' ASKUSI error filling PART or KLIN -STOP-'')')  ASKUSI98
         CALL EXIT                                                      ASKUSI99
      ENDIF                                                             ASKUS100
C                                                                       ASKUS101
C   Input parameters (see description in HERWIG)                        ASKUS102
C                                                                       ASKUS103
      PART1 = 'E-  '                                                    ASKUS104
      PART2 = 'E+  '                                                    ASKUS105
C                                                                       ASKUS106
C beam momentum                                                         ASKUS107
C                                                                       ASKUS108
      PBEAM1 = 46.1D0                                                   ASKUS109
C                                                                       ASKUS110
C HERWIG process identifier                                             ASKUS111
C                                                                       ASKUS112
      IPROC = 100                                                       ASKUS113
C                                                                       ASKUS114
C initial process (e+e- --> qqbar) simulated by HERWIG                  ASKUS115
C                                                                       ASKUS116
      IHARD = 0                                                         ASKUS117
C                                                                       ASKUS118
C print PART bank per default                                           ASKUS119
C                                                                       ASKUS120
      IPPART = 1                                                        ASKUS121
C                                                                       ASKUS122
C the default values can be changed by the DATA CARD GHRW               ASKUS123
C                                                                       ASKUS124
      JGHRW = NLINK('GHRW',0)                                           ASKUS125
C                                                                       ASKUS126
      IF(JGHRW.NE.0) THEN                                               ASKUS127
        NDAT = IW(JGHRW)                                                ASKUS128
        IF (NDAT.GE.1) PBEAM1 = DBLE(RW(JGHRW+1))                       ASKUS129
        IF (NDAT.GE.2) IPROC = IW(JGHRW+2)                              ASKUS130
        IF (NDAT.GE.3) IPRINT = IW(JGHRW+3)                             ASKUS131
        IF (NDAT.GE.4) IHARD = IW(JGHRW+4)                              ASKUS132
        IF (NDAT.GE.5) IPPART = IW(JGHRW+5)                             ASKUS133
      ENDIF                                                             ASKUS134
C                                                                       ASKUS135
      PBEAM2 = PBEAM1                                                   ASKUS136
C                                                                       ASKUS137
C initialise other common blocks                                        ASKUS138
C                                                                       ASKUS139
      CALL HWIGIN                                                       ASKUS140
      DUMMY=HWRGET(NRN)                                                 ASKUS141
      IBRN(1)=12348765                                                  ASKUS142
      IBRN(2)=0                                                         ASKUS143
C                                                                       ASKUS144
C we set the ALEPH default for some masses                              ASKUS145
C which have been overwritten in HWIGIN                                 ASKUS146
C (see ALEPH 88-66, PHYSIC 88-18)                                       ASKUS147
C quarks, W+, W-, Z0, Higgs                                             ASKUS148
C (n.b.: the masses of the antiquarks will be computed in HWUINC)       ASKUS149
C                                                                       ASKUS150
      RMASS(1) = DBLE(PARMAS(102))                                      ASKUS151
      RMASS(2) = DBLE(PARMAS(101))                                      ASKUS152
C                                                                       ASKUS153
      DO 100 IQUARK = 3,6                                               ASKUS154
        RMASS(IQUARK) = DBLE(PARMAS(100+IQUARK))                        ASKUS155
  100 CONTINUE                                                          ASKUS156
C                                                                       ASKUS157
      RMASS(198) = DBLE(PARMAS(42))                                     ASKUS158
      RMASS(199) = DBLE(PARMAS(43))                                     ASKUS159
      RMASS(200) = DBLE(PARMAS(44))                                     ASKUS160
      RMASS(201) = DBLE(PARMAS(54))                                     ASKUS161
C                                                                       ASKUS162
      ECMS   = PBEAM1 + PBEAM2                                          ASKUS163
      IFL=MOD(IPROC,10)                                                 ASKUS164
C                                                                       ASKUS165
C AST LOOK AT PROCESS SPECIFIED, CHECK IT IS VIABLE                     ASKUS166
      IF (IPROC.GE.100.AND.IPROC.LT.150) THEN                           ASKUS167
C ee -> hadrons                                                         ASKUS168
        IF (IFL.EQ.7) WRITE(IOUT,                                       ASKUS169
     &   '('' +++ASKUSI+++ Note gluon-gluon(-gluon) process'')')        ASKUS170
        IF(IPROC.GE.120) WRITE(IOUT,                                    ASKUS171
     &   '('' +++ASKUSI+++ Note no hard gluon corrections'')')          ASKUS172
        IF (IFL.NE.0.AND.IFL.NE.7) THEN                                 ASKUS173
          IF (2.*RMASS(IFL)+0.2.GE.ECMS) THEN                           ASKUS174
            WRITE(IOUT,1005)                                            ASKUS175
            WRITE(IOUT,1006)                                            ASKUS176
            CALL EXIT                                                   ASKUS177
          ENDIF                                                         ASKUS178
        ENDIF                                                           ASKUS179
C                                                                       ASKUS180
      ELSEIF(IPROC.EQ.200) THEN                                         ASKUS181
        WRITE(IOUT,'('' +++ASKUSI+++ Note WW production '')')           ASKUS182
        IF (RMASS(198)+RMASS(199)+0.2.GE.ECMS) THEN                     ASKUS183
          WRITE(IOUT,1005)                                              ASKUS184
          WRITE(IOUT,1007)                                              ASKUS185
        ENDIF                                                           ASKUS186
      ELSEIF(IPROC.EQ.250) THEN                                         ASKUS187
        WRITE(IOUT,'('' +++ASKUSI+++ Note ZZ production '')')           ASKUS188
        IF (RMASS(200)+RMASS(200)+0.2.GE.ECMS) THEN                     ASKUS189
          WRITE(IOUT,1005)                                              ASKUS190
          WRITE(IOUT,1007)                                              ASKUS191
        ENDIF                                                           ASKUS192
      ELSEIF(IPROC.GE.300.AND.IPROC.LT.400) THEN                        ASKUS193
        WRITE(IOUT,'('' +++ASKUSI+++ Note ZH production '')')           ASKUS194
        IF (RMASS(200)+RMASS(201)+0.2.GE.ECMS) THEN                     ASKUS195
          WRITE(IOUT,1005)                                              ASKUS196
          WRITE(IOUT,1007)                                              ASKUS197
        ENDIF                                                           ASKUS198
      ELSEIF(IPROC.GE.400.AND.IPROC.LT.500) THEN                        ASKUS199
        WRITE(IOUT,'('' +++ASKUSI+++ Note Hll production '')')          ASKUS200
        IF (RMASS(201)+0.2.GE.ECMS) THEN                                ASKUS201
          WRITE(IOUT,1005)                                              ASKUS202
          WRITE(IOUT,1007)                                              ASKUS203
        ENDIF                                                           ASKUS204
      ELSEIF(IPROC.GE.500.AND.IPROC.LT.600) THEN                        ASKUS205
        WRITE(IOUT,'('' +++ASKUSI+++ Note, 2 photon production'')')     ASKUS206
      ELSE                                                              ASKUS207
        WRITE(IOUT,1000)IPROC                                           ASKUS208
 1000   FORMAT(1X,'+++ASKUSI+++ Wrong process identifier IPROC = ',I5)  ASKUS209
          WRITE(IOUT,1006)                                              ASKUS210
        CALL EXIT                                                       ASKUS211
      ENDIF                                                             ASKUS212
C                                                                       ASKUS213
C we define ALEPH defaults for some more standard model parameters      ASKUS214
C lamda-QCD, Weinberg-angle, width of the Z0                            ASKUS215
C                                                                       ASKUS216
      QCDLAM = 0.18D0                                                   ASKUS217
      SWEIN = 0.2293D0                                                  ASKUS218
      GAMZ = 2.56D0                                                     ASKUS219
C                                                                       ASKUS220
C here the user can change more standard model parameters               ASKUS221
C                                                                       ASKUS222
      JGGSW = NLINK('GGSW',0)                                           ASKUS223
C                                                                       ASKUS224
C AST remove NAFLA                                                      ASKUS225
      IF(JGGSW.NE.0) THEN                                               ASKUS226
        NDAT = IW(JGGSW)                                                ASKUS227
        IF (NDAT.GE.1) QCDLAM = DBLE(RW(JGGSW+1))                       ASKUS228
        IF (NDAT.GE.2) NFLAV = IW(JGGSW+2)                              ASKUS229
        IF (NDAT.GE.3) GAMZ = DBLE(RW(JGGSW+3))                         ASKUS230
        IF (NDAT.GE.4) SWEIN = DBLE(RW(JGGSW+4))                        ASKUS231
        IF (NDAT.GE.5) SCABI = DBLE(RW(JGGSW+5))                        ASKUS232
      ENDIF                                                             ASKUS233
C                                                                       ASKUS234
C AST here the user can change B-decay parameter and choices            ASKUS235
C                                                                       ASKUS236
      JGBDE = NLINK('GBDE',0)                                           ASKUS237
      IIBDEC = 1                                                        ASKUS238
C                                                                       ASKUS239
      IF(JGBDE.NE.0) THEN                                               ASKUS240
        NDAT = IW(JGBDE)                                                ASKUS241
        IF (NDAT.GE.1) B1LIM = DBLE(RW(JGBDE+1))                        ASKUS242
        IF (NDAT.GE.2) THEN                                             ASKUS243
          BDECAY = CHAINT(IW(JGBDE+2))                                  ASKUS244
          DO I=3,1,-1                                                   ASKUS245
            IF(BDECAY.EQ.CHBDEC(I)) IIBDEC=I                            ASKUS246
          ENDDO                                                         ASKUS247
        ENDIF                                                           ASKUS248
      ENDIF                                                             ASKUS249
C                                                                       ASKUS250
      RAZSOF = 1.                                                       ASKUS251
      RAZSPI = 1.                                                       ASKUS252
C                                                                       ASKUS253
C here the user can change the most important                           ASKUS254
C parameters for the cluster fragmentation                              ASKUS255
C                                                                       ASKUS256
      JGPAR = NLINK('GPRM',0)                                           ASKUS257
      IF(JGPAR.NE.0) THEN                                               ASKUS258
        NDAT = IW(JGPAR)                                                ASKUS259
        IF (NDAT.GE.1) CLMAX = DBLE(RW(JGPAR+1))                        ASKUS260
        IF (NDAT.GE.2) CLPOW = DBLE(RW(JGPAR+2))                        ASKUS261
        IF (NDAT.GE.3) PSPLT = DBLE(RW(JGPAR+3))                        ASKUS262
        IF (NDAT.GE.4) THMAX = DBLE(RW(JGPAR+4))                        ASKUS263
        IF (NDAT.GE.5) VQCUT = DBLE(RW(JGPAR+5))                        ASKUS264
        IF (NDAT.GE.6) VGCUT = DBLE(RW(JGPAR+6))                        ASKUS265
        IF (NDAT.GE.7) QDIQK = DBLE(RW(JGPAR+7))                        ASKUS266
        IF (NDAT.GE.8) PDIQK = DBLE(RW(JGPAR+8))                        ASKUS267
      ENDIF                                                             ASKUS268
C                                                                       ASKUS269
C Now some further parameters controlling the program                   ASKUS270
C                                                                       ASKUS271
      RAZSOF = 1.                                                       ASKUS272
      RAZSPI = 1.                                                       ASKUS273
      JGPRC = NLINK('GHRC',0)                                           ASKUS274
      IF(JGPRC.NE.0) THEN                                               ASKUS275
        NDAT = IW(JGPRC)                                                ASKUS276
        IF (NDAT.GE.1) THEN                                             ASKUS277
          IF (RW(JGPRC+1).EQ.0.) THEN                                   ASKUS278
            AZSOFT = .FALSE.                                            ASKUS279
            RAZSOF = 0.D0                                               ASKUS280
          ENDIF                                                         ASKUS281
        ENDIF                                                           ASKUS282
        IF (NDAT.GE.2) THEN                                             ASKUS283
          IF (RW(JGPRC).EQ.0.) THEN                                     ASKUS284
            AZSPIN = .FALSE.                                            ASKUS285
            RAZSPI = 0.D0                                               ASKUS286
          ENDIF                                                         ASKUS287
        ENDIF                                                           ASKUS288
        IF (NDAT.GE.3) CLDIR = IW(JGPRC+3)                              ASKUS289
        IF (NDAT.GE.4) CLSMR = DBLE(RW(JGPRC+4))                        ASKUS290
        IF (NDAT.GE.5) BTCLM = DBLE(RW(JGPRC+5))                        ASKUS291
        IF (NDAT.GE.6) ETAMIX = DBLE(RW(JGPRC+6))                       ASKUS292
        IF (NDAT.GE.7) PRSOF  = DBLE(RW(JGPRC+7))                       ASKUS293
      ENDIF                                                             ASKUS294
C                                                                       ASKUS295
C Beam Polarizations                                                    ASKUS296
C                                                                       ASKUS297
      JGPOL = NLINK('GPOL',0)                                           ASKUS298
      IF(JGPOL.NE.0) THEN                                               ASKUS299
        NDAT = IW(JGPOL)                                                ASKUS300
        IF (NDAT.GE.1) EPOLN(1) = DBLE(RW(JGPOL+1))                     ASKUS301
        IF (NDAT.GE.2) EPOLN(2) = DBLE(RW(JGPOL+2))                     ASKUS302
        IF (NDAT.GE.3) EPOLN(3) = DBLE(RW(JGPOL+3))                     ASKUS303
        IF (NDAT.GE.4) PPOLN(1) = DBLE(RW(JGPOL+4))                     ASKUS304
        IF (NDAT.GE.5) PPOLN(2) = DBLE(RW(JGPOL+5))                     ASKUS305
        IF (NDAT.GE.6) PPOLN(3) = DBLE(RW(JGPOL+6))                     ASKUS306
      ENDIF                                                             ASKUS307
C                                                                       ASKUS308
C change here the parameters governing final state radiation            ASKUS309
C                                                                       ASKUS310
      JGFSR = NLINK('GFSR',0)                                           ASKUS311
      IF(JGFSR.NE.0) THEN                                               ASKUS312
        NDAT = IW(JGFSR)                                                ASKUS313
        IF (NDAT.GE.1) VPCUT = DBLE(RW(JGFSR+1))                        ASKUS314
        IF (NDAT.GE.2) ALPFAC = DBLE(RW(JGFSR+2))                       ASKUS315
      ENDIF                                                             ASKUS316
C                                                                       ASKUS317
C Sudakov form factor options                                           ASKUS318
C                                                                       ASKUS319
      LWSUD = 0                                                         ASKUS320
      LRSUD = 0                                                         ASKUS321
      JGSUD = NLINK('GSUD',0)                                           ASKUS322
C                                                                       ASKUS323
      IF(JGSUD.NE.0) THEN                                               ASKUS324
        NDAT = IW(JGSUD)                                                ASKUS325
        IF (NDAT.GE.1) SUDORD = IW(JGSUD+1)                             ASKUS326
        IF (NDAT.GE.2) LWSUD  = IW(JGSUD+2)                             ASKUS327
        IF (NDAT.GE.3) LRSUD  = IW(JGSUD+3)                             ASKUS328
      ENDIF                                                             ASKUS329
C                                                                       ASKUS330
C W and Z decay options                                                 ASKUS331
C                                                                       ASKUS332
      JGPAR = NLINK('GHMB',0)                                           ASKUS333
      IF(JGPAR.NE.0) THEN                                               ASKUS334
        NDAT = IW(JGPAR)                                                ASKUS335
        IF(NDAT.GE.1) THEN                                              ASKUS336
          DO I=1,NDAT                                                   ASKUS337
            MODBOS(I) = IW(JGPAR+I)                                     ASKUS338
          ENDDO                                                         ASKUS339
        ENDIF                                                           ASKUS340
      ENDIF                                                             ASKUS341
C                                                                       ASKUS342
C Allow to vary some relative weight parameters                         ASKUS343
C                                                                       ASKUS344
      JGPAR = NLINK('GHWT',0)                                           ASKUS345
      IF(JGPAR.NE.0) THEN                                               ASKUS346
        NDAT = IW(JGPAR)                                                ASKUS347
        IF (NDAT.GE.1) VECWT = DBLE(RW(JGPAR+1))                        ASKUS348
        IF (NDAT.GE.2) TENWT = DBLE(RW(JGPAR+2))                        ASKUS349
        IF (NDAT.GE.3) DECWT = DBLE(RW(JGPAR+3))                        ASKUS350
        IF(NDAT.GT.10) NDAT=10                                          ASKUS351
        IF(NDAT.GE.4) THEN                                              ASKUS352
          DO I=4,NDAT                                                   ASKUS353
            PWT(I-3) = DBLE(RW(JGPAR+I))                                ASKUS354
          ENDDO                                                         ASKUS355
        ENDIF                                                           ASKUS356
      ENDIF                                                             ASKUS357
C                                                                       ASKUS358
C finally we redefine some steering parameters of the                   ASKUS359
C HERWIG program:                                                       ASKUS360
C don't print out event listings                                        ASKUS361
C increase number of possible HERWIG errors                             ASKUS362
C                                                                       ASKUS363
      MAXPR = 0                                                         ASKUS364
      MAXER = 100                                                       ASKUS365
C                                                                       ASKUS366
      IDEBB = 0                                                         ASKUS367
      IDEBE = 0                                                         ASKUS368
C                                                                       ASKUS369
C switch off time check in HERWIG                                       ASKUS370
C                                                                       ASKUS371
      TLOUT = 0.D0                                                      ASKUS372
C                                                                       ASKUS373
C the user can debug specific events with the                           ASKUS374
C data card DEBU                                                        ASKUS375
C                                                                       ASKUS376
      NAMI = NAMIND('DEBU')                                             ASKUS377
C                                                                       ASKUS378
      JDEBU = IW(NAMI)                                                  ASKUS379
C                                                                       ASKUS380
      IF(JDEBU.NE.0) THEN                                               ASKUS381
C                                                                       ASKUS382
        NDAT = IW(JDEBU)                                                ASKUS383
C                                                                       ASKUS384
        IF (NDAT.EQ.1) THEN                                             ASKUS385
          IDEBB = IW(JDEBU+1)                                           ASKUS386
          IDEBE = IDEBB                                                 ASKUS387
        ENDIF                                                           ASKUS388
C                                                                       ASKUS389
        IF (NDAT.EQ.2) THEN                                             ASKUS390
          IDEBB = IW(JDEBU+1)                                           ASKUS391
          IDEBE = IW(JDEBU+2)                                           ASKUS392
        ENDIF                                                           ASKUS393
C                                                                       ASKUS394
      ENDIF                                                             ASKUS395
C                                                                       ASKUS396
C user can reset parameters at                                          ASKUS397
C this point by data cards, otherwise values                            ASKUS398
C set in HWIGIN/HWUINC will be used.                                    ASKUS399
C                                                                       ASKUS400
C the user can define different values with the data cards GMAS         ASKUS401
C the masses of the t-mesons, t-baryons and the diquarks should         ASKUS402
C be redefined after the call to HWUINC                                 ASKUS403
C                                                                       ASKUS404
      NAMI = NAMIND ('GMAS')                                            ASKUS405
C                                                                       ASKUS406
      IF (IW(NAMI).NE.0) THEN                                           ASKUS407
C                                                                       ASKUS408
        JGMAS = NAMI + 1                                                ASKUS409
C                                                                       ASKUS410
C loop over all GMAS banks                                              ASKUS411
C                                                                       ASKUS412
   5    JGMAS = IW(JGMAS-1)                                             ASKUS413
        IF (JGMAS.EQ. 0) GOTO 6                                         ASKUS414
C                                                                       ASKUS415
        MNUM = IW(JGMAS-2)                                              ASKUS416
        IF ((MNUM.LT.109.OR.MNUM.GT.120).AND.                           ASKUS417
     &      (MNUM.LT.232.OR.MNUM.GT.244).AND.                           ASKUS418
     &       MNUM.LT.255) THEN                                          ASKUS419
          RMASS(MNUM) = RW(JGMAS+1)                                     ASKUS420
          INPART = IW(NAPART)                                           ASKUS421
          IPART = KGPART(MNUM)                                          ASKUS422
          IANTI = ITABL(INPART,IPART,JPARAN)                            ASKUS423
          RW(KROW(INPART,IPART)+JPARMA) = RW(JGMAS+1)                   ASKUS424
          IF (IANTI.NE.IPART) RW(KROW(INPART,IANTI)+JPARMA)=RW(JGMAS+1) ASKUS425
        ENDIF                                                           ASKUS426
        GOTO 5                                                          ASKUS427
C                                                                       ASKUS428
   6    CONTINUE                                                        ASKUS429
C                                                                       ASKUS430
      ENDIF                                                             ASKUS431
C                                                                       ASKUS432
C compute parameter-dependent constants                                 ASKUS433
C HWUINC overwrites the masses of the t-mesons, t-baryons               ASKUS434
C and the diquarks                                                      ASKUS435
C                                                                       ASKUS436
      CALL HWUINC                                                       ASKUS437
C                                                                       ASKUS438
C reset the overwritten masses back to the ALEPH values                 ASKUS439
C (if available)                                                        ASKUS440
C                                                                       ASKUS441
      RMASS(232) = DBLE(PARMAS(140))                                    ASKUS442
      RMASS(233) = DBLE(PARMAS(133))                                    ASKUS443
      RMASS(234) = DBLE(PARMAS(131))                                    ASKUS444
      RMASS(235) = DBLE(PARMAS(135))                                    ASKUS445
      RMASS(236) = DBLE(PARMAS(239))                                    ASKUS446
      RMASS(237) = DBLE(PARMAS(271))                                    ASKUS447
      RMASS(238) = DBLE(PARMAS(243))                                    ASKUS448
C                                                                       ASKUS449
      RMASS(241) = DBLE(PARMAS(249))                                    ASKUS450
      RMASS(242) = DBLE(PARMAS(137))                                    ASKUS451
      RMASS(243) = DBLE(PARMAS(139))                                    ASKUS452
C                                                                       ASKUS453
      RMASS(255) = DBLE(PARMAS(134))                                    ASKUS454
      RMASS(256) = DBLE(PARMAS(132))                                    ASKUS455
      RMASS(257) = DBLE(PARMAS(136))                                    ASKUS456
      RMASS(258) = DBLE(PARMAS(240))                                    ASKUS457
      RMASS(259) = DBLE(PARMAS(272))                                    ASKUS458
      RMASS(260) = DBLE(PARMAS(244))                                    ASKUS459
C                                                                       ASKUS460
      RMASS(263) = DBLE(PARMAS(250))                                    ASKUS461
      RMASS(264) = DBLE(PARMAS(138))                                    ASKUS462
C                                                                       ASKUS463
C now we we have to set the user defined masses of the                  ASKUS464
C t-mesons, t-baryons and diquarks                                      ASKUS465
C                                                                       ASKUS466
      NAMI = NAMIND ('GMAS')                                            ASKUS467
C                                                                       ASKUS468
      IF (IW(NAMI).NE.0) THEN                                           ASKUS469
C                                                                       ASKUS470
        JGMAS = NAMI + 1                                                ASKUS471
C                                                                       ASKUS472
C loop over all GMAS banks                                              ASKUS473
C                                                                       ASKUS474
  15    JGMAS = IW(JGMAS-1)                                             ASKUS475
        IF (JGMAS.EQ. 0) GOTO 16                                        ASKUS476
C                                                                       ASKUS477
        MNUM = IW(JGMAS-2)                                              ASKUS478
        IF ((MNUM.GE.109.AND.MNUM.LE.120).OR.                           ASKUS479
     &      (MNUM.GE.232.AND.MNUM.LE.244).OR.                           ASKUS480
     &       MNUM.GE.255) THEN                                          ASKUS481
          RMASS(MNUM) = RW(JGMAS+1)                                     ASKUS482
          INPART = IW(NAPART)                                           ASKUS483
          IPART = KGPART(MNUM)                                          ASKUS484
          RW(KROW(INPART,IPART)+JPARMA) = RW(JGMAS+1)                   ASKUS485
        ENDIF                                                           ASKUS486
        GOTO 15                                                         ASKUS487
C                                                                       ASKUS488
  16    CONTINUE                                                        ASKUS489
C                                                                       ASKUS490
      ENDIF                                                             ASKUS491
C                                                                       ASKUS492
C the user can set any particle stable                                  ASKUS493
C with the data cards GSTA                                              ASKUS494
C                                                                       ASKUS495
      NAMI = NAMIND ('GSTA')                                            ASKUS496
C                                                                       ASKUS497
      IF (IW(NAMI).NE.0) THEN                                           ASKUS498
C                                                                       ASKUS499
        JGSTA = NAMI + 1                                                ASKUS500
C                                                                       ASKUS501
C loop over all GSTA banks                                              ASKUS502
C                                                                       ASKUS503
 105    JGSTA = IW(JGSTA-1)                                             ASKUS504
        IF (JGSTA.EQ. 0) GOTO 106                                       ASKUS505
C                                                                       ASKUS506
        MNUM = IW(JGSTA-2)                                              ASKUS507
C                                                                       ASKUS508
        IF (CHAINT(IW(JGSTA+1)).EQ.'OFF'.AND.MODES(MNUM).NE.0)          ASKUS509
     &    CALL HWUSTA(RNAME(MNUM))                                      ASKUS510
        IF (CHAINT(IW(JGSTA+1)).EQ.'ON'.AND.MODES(MNUM).EQ.0)           ASKUS511
     &    WRITE(IOUT,1001)MNUM                                          ASKUS512
 1001     FORMAT(1X,'+++ASKUSI+++ Herwig particle # = ',I5,             ASKUS513
     &           ' no decay modes available - left stable')             ASKUS514
C                                                                       ASKUS515
        GOTO 105                                                        ASKUS516
C                                                                       ASKUS517
 106    CONTINUE                                                        ASKUS518
C                                                                       ASKUS519
      ENDIF                                                             ASKUS520
C                                                                       ASKUS521
C get particles which should not be decayed by the generator            ASKUS522
C                                                                       ASKUS523
      MXDEC = KNODEC (HEDEC,HEDE)                                       ASKUS524
      MXDEC = MIN (MXDEC,HEDE)                                          ASKUS525
C                                                                       ASKUS526
C now set them stable, except the user has provided a data              ASKUS527
C card for them                                                         ASKUS528
C                                                                       ASKUS529
      DO 200 IPART = 1,MXDEC                                            ASKUS530
C                                                                       ASKUS531
C look if data card is present                                          ASKUS532
C                                                                       ASKUS533
         JGSTA = NLINK('GSTA',HEDEC(IPART))                             ASKUS534
C                                                                       ASKUS535
C if not, inhibit decay                                                 ASKUS536
C                                                                       ASKUS537
         IF (JGSTA.LE.0) THEN                                           ASKUS538
           IF (HEDEC(IPART).GT.0.AND.MODES(HEDEC(IPART)).NE.0)          ASKUS539
     &       CALL HWUSTA(RNAME(HEDEC(IPART)))                           ASKUS540
         ENDIF                                                          ASKUS541
C                                                                       ASKUS542
  200 CONTINUE                                                          ASKUS543
C                                                                       ASKUS544
      IF(IHARD.EQ.1.OR.IHARD.EQ.2) THEN                                 ASKUS545
C                                                                       ASKUS546
        WRITE(IOUT,*) '+++ASKUSI+++ LUND OPTION NO LONGER AVAILABLE'    ASKUS547
        WRITE(IOUT,1006)                                                ASKUS548
        CALL EXIT                                                       ASKUS549
      ELSEIF (IHARD.EQ.3) THEN                                          ASKUS550
        IF(IPROC.LT.107) THEN                                           ASKUS551
          WEAKPAR(1)=RMASS(121)                                         ASKUS552
          WEAKPAR(3)=RMASS(200)                                         ASKUS553
          WEAKPAR(4)=GAMZ                                               ASKUS554
          WEAKPAR(5)=SWEIN                                              ASKUS555
          IF(IFL.GT.0) THEN                                             ASKUS556
            WEAKPAR(2)=RMASS(IFL)                                       ASKUS557
            WEAKPAR(11)=ICHRG(IFL)                                      ASKUS558
          ENDIF                                                         ASKUS559
          EBEAM=PBEAM1                                                  ASKUS560
          IOUTDY=IOUT                                                   ASKUS561
          CALL HWIDY3(IFL,NFLAV)                                        ASKUS562
        ELSE                                                            ASKUS563
          WRITE(IOUT,*) '+++ASKUSI+++ Process ',IPROC, ' is ',          ASKUS564
     &     'meaningless with DYMU02'                                    ASKUS565
          IHARD=0                                                       ASKUS566
        ENDIF                                                           ASKUS567
      ENDIF                                                             ASKUS568
C                                                                       ASKUS569
C book default histograms                                               ASKUS570
C                                                                       ASKUS571
      CALL UHRBK                                                        ASKUS572
C                                                                       ASKUS573
C user's initial calculations, redefine any other parameter in HERWIG   ASKUS574
C                                                                       ASKUS575
      CALL USTART                                                       ASKUS576
C                                                                       ASKUS577
C initialise elementary process (Needed for DYMU02 as well)             ASKUS578
C                                                                       ASKUS579
      CALL HWEINI                                                       ASKUS580
C                                                                       ASKUS581
      SDVRT(1) = 0.035                                                  ASKUS582
      SDVRT(2) = 0.0012                                                 ASKUS583
      SDVRT(3) = 1.28                                                   ASKUS584
C                                                                       ASKUS585
      JSVRT = NLINK('SVRT',0)                                           ASKUS586
C                                                                       ASKUS587
      IF(JSVRT.NE.0) THEN                                               ASKUS588
        SDVRT(1) = RW(JSVRT+1)                                          ASKUS589
        SDVRT(2) = RW(JSVRT+2)                                          ASKUS590
        SDVRT(3) = RW(JSVRT+3)                                          ASKUS591
      ENDIF                                                             ASKUS592
C                                                                       ASKUS593
C reset event counters                                                  ASKUS594
C                                                                       ASKUS595
      DO 120 I = 1,10                                                   ASKUS596
        NEVENT(I) = 0                                                   ASKUS597
  120 CONTINUE                                                          ASKUS598
C                                                                       ASKUS599
C reset flavour counters                                                ASKUS600
      DO 130 I = 1,MAXFLA                                               ASKUS601
        IFLC(I) = 0                                                     ASKUS602
  130 CONTINUE                                                          ASKUS603
C                                                                       ASKUS604
C  Fill the KPAR bank with the generator parameters                     ASKUS605
C AST extend, adding GAMZ, VPCUT, B1LIM and IIBDEC (Bdecay choice)      ASKUS606
C AST also add CLPOW, ALPFAC and SUDORD                                 ASKUS607
C                                                                       ASKUS608
      TABL(1) = SNGL(PBEAM1)                                            ASKUS609
      TABL(2) = FLOAT(IPROC)                                            ASKUS610
      TABL(3) = SNGL(RMASS(6))                                          ASKUS611
      TABL(4) = SNGL(RMASS(13))                                         ASKUS612
      TABL(5) = SNGL(RMASS(199))                                        ASKUS613
      TABL(6) = SNGL(RMASS(200))                                        ASKUS614
      TABL(7) = SNGL(RMASS(201))                                        ASKUS615
      TABL(8) = SNGL(QCDLAM)                                            ASKUS616
      TABL(9) = FLOAT(NFLAV)                                            ASKUS617
      TABL(10) = SNGL(GAMZ)                                             ASKUS618
      TABL(11) = SNGL(SWEIN)                                            ASKUS619
      TABL(12) = SNGL(SCABI)                                            ASKUS620
      TABL(13) = RAZSOF                                                 ASKUS621
      TABL(14) = RAZSPI                                                 ASKUS622
      TABL(15) = SNGL(CLMAX)                                            ASKUS623
      TABL(16) = SNGL(CLPOW)                                            ASKUS624
      TABL(17) = SNGL(PSPLT)                                            ASKUS625
      TABL(18) = SNGL(THMAX)                                            ASKUS626
      TABL(19) = SNGL(VQCUT)                                            ASKUS627
      TABL(20) = SNGL(VGCUT)                                            ASKUS628
      TABL(21) = SNGL(VPCUT)                                            ASKUS629
      TABL(22) = SNGL(QDIQK)                                            ASKUS630
      TABL(23) = SNGL(PDIQK)                                            ASKUS631
      TABL(24) = SNGL(B1LIM)                                            ASKUS632
      TABL(25) = FLOAT(IIBDEC)                                          ASKUS633
      TABL(26) = SNGL(ALPFAC)                                           ASKUS634
      TABL(27) = FLOAT(SUDORD)                                          ASKUS635
      TABL(28) = SDVRT(1)                                               ASKUS636
      TABL(29) = SDVRT(2)                                               ASKUS637
      TABL(30) = SDVRT(3)                                               ASKUS638
      TABL(31) = FLOAT(IHARD)                                           ASKUS639
      TABL(32) = FLOAT(CLDIR)                                           ASKUS640
      TABL(33) = SNGL(CLSMR)                                            ASKUS641
      TABL(34) = SNGL(BTCLM)                                            ASKUS642
      TABL(35) = SNGL(ETAMIX)                                           ASKUS643
      TABL(36) = SNGL(EPOLN(1))                                         ASKUS644
      TABL(37) = SNGL(EPOLN(2))                                         ASKUS645
      TABL(38) = SNGL(EPOLN(3))                                         ASKUS646
      TABL(39) = SNGL(PPOLN(1))                                         ASKUS647
      TABL(40) = SNGL(PPOLN(2))                                         ASKUS648
      TABL(41) = SNGL(PPOLN(3))                                         ASKUS649
C                                                                       ASKUS650
      NROW = 1                                                          ASKUS651
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')                ASKUS652
C                                                                       ASKUS653
C  Fill RLEP bank                                                       ASKUS654
C                                                                       ASKUS655
       IEBEAM = NINT(SNGL(PBEAM1) *1000  )                              ASKUS656
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                              ASKUS657
C                                                                       ASKUS658
C print KPAR and RLEP banks                                             ASKUS659
C                                                                       ASKUS660
      CALL PRTABL('RLEP',0)                                             ASKUS661
      CALL PRTABL('KPAR',0)                                             ASKUS662
C                                                                       ASKUS663
C  Print PART and KLIN bank                                             ASKUS664
C                                                                       ASKUS665
      IF (IPPART.EQ.1) CALL PRPART                                      ASKUS666
C                                                                       ASKUS667
      RETURN                                                            ASKUS668
 1005 FORMAT(1X,'+++ASKUSI+++ Beam energy too small for selected ',     ASKUS669
     &           'process, respecify')                                  ASKUS670
 1006 FORMAT(1X,' +++ASKUSI+++   --- stopping ---')                     ASKUS671
 1007 FORMAT(1X,' +++ASKUSI+++   --- carrying on ---')                  ASKUS672
      END                                                               ASKUS673
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C                                                                       ASKUSE 3
C --------------------------------------------------------------------  ASKUSE 4
C Ebi Lange  December 1988.                                             ASKUSE 5
C updated AST December 1993.                                            ASKUSE 6
C --------------------------------------------------------------------  ASKUSE 7
C                                                                       ASKUSE 8
      IMPLICIT NONE                                                     ASKUSE 9
C                                                                       ASKUSE10
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(1000)                                                     BCS    6
      COMMON /BCS/   IW(1000)                                           BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      INTEGER NCOL,MAXFLA                                               DTMINP 2
C                                                                       DTMINP 3
      PARAMETER (NCOL = 41)                                             DTMINP 4
      PARAMETER (MAXFLA=7)                                              DTMINP 5
C                                                                       DTMINP 6
      INTEGER NEVENT,IFLC,IOUT,IDEBB,IDEBE                              DTMINP 7
      REAL SDVRT,TABL                                                   DTMINP 8
C                                                                       DTMINP 9
      COMMON / DTMILL / SDVRT(3),TABL(NCOL),NEVENT(10),IFLC(MAXFLA)     DTMINP10
      COMMON / INPOUT / IOUT                                            DTMINP11
      COMMON / DTBUG / IDEBB,IDEBE                                      DTMINP12
C                                                                       DTMINP13
      INTEGER IHARD,IFL                                                 INIPRO 2
C                                                                       INIPRO 3
      COMMON /INIPRO/ IHARD,IFL                                         INIPRO 4
C                                                                       INIPRO 5
C                                                                       ASKUSE14
C HERWIG commons                                                        ASKUSE15
C                                                                       ASKUSE16
      DOUBLE PRECISION                                                  HWDEFI 2
     & PHEP,VHEP,PBEAM1,PBEAM2,QCDLAM,VGCUT,VQCUT,VPCUT,BETAF,CAFAC,    HWDEFI 3
     & CFFAC,CLMAX,CLPOW,PSPLT,QSPAC,PTRMS,PXRMS,QG,QV,SWEIN,SCABI,     HWDEFI 4
     & PDIQK,QDIQK,ENSOF,TMTOP,ZBINM,GAMW,GAMZ,GAMH,PGSMX,PGSPL,PPAR,   HWDEFI 5
     & VPAR,PHIPAR,DECPAR,RHOPAR,RHOHEP,XFACT,PTINT,EVWGT,AVWGT,WGTMAX, HWDEFI 6
     & WGTSUM,WSQSUM,WBIGST,TLOUT,YJMIN,YJMAX,PTMIN,PTMAX,PTPOW,EMMIN,  HWDEFI 7
     & EMMAX,EMPOW,Q2MIN,Q2MAX,Q2POW,THMAX,QLIM,XXMIN,XLMIN,EMSCA,      HWDEFI 8
     & EMLST,COSTH,GPOLN,GCOEF,XX,DISF,RESN,RMIN,CTMAX,FBTM,            HWDEFI 9
     & FTOP,FHVY,RMASS,BFRAC,CMMOM,ACCUR,QEV,SUD,VECWT,TENWT,DECWT,     HWDEFI10
     & QWT,PWT,SWT,SWTEF,RESWT,PIFAC,QCDL3,QCDL5,BRHIG,GAMMAX,          HWDEFI11
     & ENHANC,B1LIM,ALPFAC,Q2WWMN,Q2WWMX,BTCLM,ETAMIX,                  HWDEFI12
     & QFCH,VFCH,AFCH,VCKM,TQWT,CLQ,EPOLN,PPOLN,COSS,SINS,GAMZP,GEV2NB, HWDEFI13
     & ALPHEM,PRSOF,EBEAM1,EBEAM2,ZJMAX,YBMIN,YBMAX,HARDST,CLSMR,PHOMAS HWDEFI14
C                                                                       HWDEFI15
      INTEGER NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP,            HWDEFI16
     & IPROC,MAXEV,IPRINT,LRSUD,LWSUD,NCOLO,NFLAV,MODPDF(2),NSTRU,      HWDEFI17
     & NZBIN,NBTRY,NCTRY,NDTRY,NETRY,NSTRY,NGSPL,NMXPAR,NEVPAR,         HWDEFI18
     & NPAR,ISTPAR,IDPAR,JMOPAR,JDAPAR,JCOPAR,INHAD,JNHAD,NSPAC,NRN,    HWDEFI19
     & MAXER,MAXPR,LWEVT,ISTAT,IERROR,NWGTS,IDHW,IBSH,IBRN,IPRO,        HWDEFI20
     & IFLMIN,IFLMAX,MAXFL,IDCMF,IHPRO,IDN,ICO,LOCN,                    HWDEFI21
     & NRES,IDPDG,ICHRG,MADDR,MODES,MODEF,IDPRO,INTER,NQEV,             HWDEFI22
     & NSUD,NMXSUD,MODBOS,IOPHIG,MODMAX,SUDORD,CLDIR,NUMER,NUMERU,      HWDEFI23
     & MAPQ,IPART1,IPART2,ISPAC,IAPHIG                                  HWDEFI24
C                                                                       HWDEFI25
      LOGICAL AZSOFT,AZSPIN,FROST,GENEV,BGSHAT,NOWGT,TMPAR,PRNDEC,      HWDEFI26
     & HVFCEN,FSTEVT,FSTWGT,BREIT,USECMF,ZPRIME,TPOL,GENSOF,            HWDEFI27
     & HARDME,SOFTME,NOSPAC                                             HWDEFI28
C                                                                       HWDEFI29
      CHARACTER*4 PART1,PART2,RNAME,BDECAY                              HWDEFI30
      CHARACTER*20 AUTPDF(2)                                            HWDEFI31
C                                                                       HWDEFI32
      PARAMETER (NMXHEP=2000)                                           HWDEFI33
      INTEGER NMXJET                                                    HWDEFI34
      PARAMETER (NMXJET=200)                                            HWDEFI35
C                                                                       HWDEFI36
      COMMON/HWPROC/PBEAM1,PBEAM2,EBEAM1,EBEAM2,IPROC,MAXEV             HWPROC 2
C                                                                       HWPROC 3
      COMMON/HWEVNT/EVWGT,AVWGT,WGTMAX,WGTSUM,WSQSUM,WBIGST,TLOUT,      HWEVNT 2
     & NRN(2),MAXER,NUMER,NUMERU,MAXPR,LWEVT,ISTAT,IERROR,NOWGT,NWGTS,  HWEVNT 3
     & IDHW(NMXHEP),GENSOF                                              HWEVNT 4
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),           HEPEVT 2
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)  HEPEVT 3
      INTEGER MK,NZ0                                                    RADPAR 2
      REAL PAK,ALPK,THEK,PHIK                                           RADPAR 3
C                                                                       RADPAR 4
      COMMON /RADPAR/   MK,PAK,ALPK,THEK,PHIK,NZ0                       RADPAR 5
C                                                                       RADPAR 6
C                                                                       ASKUSE22
      INTEGER ISEED(3)                                                  ASKUSE23
      INTEGER ITRK,IDPR,ISTA,IST,NVRT,NTRK,IMOTH,IDAUG                  ASKUSE24
C                                                                       ASKUSE25
      REAL VRTEX(4)                                                     ASKUSE26
      REAL RN1,RN2,RN3,DUM                                              ASKUSE27
      REAL ECMS,WEIT                                                    ASKUSE28
C                                                                       ASKUSE29
      INTEGER ID,NRBOS,L                                                BMACRO 2
      INTEGER LCOLS,LROWS,KNEXT,KROW,LFRWRD,LFRROW,ITABL                BMACRO 3
      REAL RTABL                                                        BMACRO 4
C                                                                       BMACRO 5
      INTEGER NAPART,JVK,JPA,NLINK                                      BMACRO 6
      INTEGER KNEXVK,LFRVK,KPARVK,KLISVK,NOFVK,KINTYP,INPTRK,           BMACRO 7
     &        INPVRT,MOTHVK                                             BMACRO 8
      REAL CHARGE,PARMAS,TIMLIF,PMODVK,ENERVK,TOFLIT,RADVK,PMASVK       BMACRO 9
C - # of words/row in bank with index ID                                BMACRO10
      LCOLS(ID) = IW(ID+1)                                              BMACRO11
C - # of rows in bank with index ID                                     BMACRO12
      LROWS(ID) = IW(ID+2)                                              BMACRO13
C - index of next row in the bank with index ID                         BMACRO14
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO15
C - index of row # NRBOS in the bank with index ID                      BMACRO16
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO17
C - # of free words in the bank with index ID                           BMACRO18
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO19
C - # of free rows in the bank with index ID                            BMACRO20
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO21
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO22
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO23
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO24
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO25
C                                                                       BMACRO26
C                                                                       ASKUSE31
C store the current random number seeds                                 ASKUSE32
C                                                                       ASKUSE33
      CALL RDMOUT(ISEED)                                                ASKUSE34
C                                                                       ASKUSE35
C  Generate primary vertex                                              ASKUSE36
C                                                                       ASKUSE37
      CALL RANNOR (RN1,RN2)                                             ASKUSE38
      CALL RANNOR (RN3,DUM)                                             ASKUSE39
C                                                                       ASKUSE40
      VRTEX(1) = RN1*SDVRT(1)                                           ASKUSE41
      VRTEX(2) = RN2*SDVRT(2)                                           ASKUSE42
      VRTEX(3) = RN3*SDVRT(3)                                           ASKUSE43
      VRTEX(4) = 0.                                                     ASKUSE44
C                                                                       ASKUSE45
C Event generation                                                      ASKUSE46
C                                                                       ASKUSE47
C initialise event                                                      ASKUSE48
C                                                                       ASKUSE49
      CALL HWUINE                                                       ASKUSE50
C                                                                       ASKUSE51
C generate hard subprocess, check LUND is not used                      ASKUSE52
C                                                                       ASKUSE53
      IF(IHARD.NE.0.AND.IHARD.NE.3) THEN                                ASKUSE54
        WRITE(6,*) 'WARNING ihard is now illegal',ihard                 ASKUSE55
        IHARD=0                                                         ASKUSE56
      ENDIF                                                             ASKUSE57
      IF (IHARD.EQ.0) THEN                                              ASKUSE58
        CALL HWEPRO                                                     ASKUSE59
      ELSEIF(IHARD.EQ.3) THEN                                           ASKUSE60
        CALL HWEDY3                                                     ASKUSE61
      ENDIF                                                             ASKUSE62
C                                                                       ASKUSE63
C find flavour, look for first parton                                   ASKUSE64
C                                                                       ASKUSE65
      IF(IPROC.LT.150) THEN                                             ASKUSE66
      DO 10 ITRK = 1,NHEP                                               ASKUSE67
        IF (ISTHEP(ITRK).EQ.113) THEN                                   ASKUSE68
          IDPR = ABS(IDHEP(ITRK))                                       ASKUSE69
          GOTO 20                                                       ASKUSE70
        ENDIF                                                           ASKUSE71
   10 CONTINUE                                                          ASKUSE72
C                                                                       ASKUSE73
   20 CONTINUE                                                          ASKUSE74
      IF(IDPR.EQ.21) IDPR=7                                             ASKUSE75
C                                                                       ASKUSE76
      IF(IDPR.LE.7) IFLC(IDPR) = IFLC(IDPR) + 1                         ASKUSE77
      ENDIF                                                             ASKUSE78
C                                                                       ASKUSE79
C generate parton cascades                                              ASKUSE80
C                                                                       ASKUSE81
      CALL HWBGEN                                                       ASKUSE82
C                                                                       ASKUSE83
C do heavy quark decays                                                 ASKUSE84
C                                                                       ASKUSE85
      CALL HWDHQK                                                       ASKUSE86
C                                                                       ASKUSE87
C do cluster formation                                                  ASKUSE88
C                                                                       ASKUSE89
      CALL HWCFOR                                                       ASKUSE90
C                                                                       ASKUSE91
C do cluster decays                                                     ASKUSE92
C                                                                       ASKUSE93
      CALL HWCDEC                                                       ASKUSE94
C                                                                       ASKUSE95
C do unstable particle decays                                           ASKUSE96
C                                                                       ASKUSE97
      CALL HWDHAD                                                       ASKUSE98
C                                                                       ASKUSE99
C do heavy flavour decays                                               ASKUS100
C                                                                       ASKUS101
      CALL HWDHVY                                                       ASKUS102
C                                                                       ASKUS103
C add soft underlying event if required                                 ASKUS104
C                                                                       ASKUS105
      CALL HWMEVT                                                       ASKUS106
C                                                                       ASKUS107
C finish event                                                          ASKUS108
C                                                                       ASKUS109
      CALL HWUFNE                                                       ASKUS110
C                                                                       ASKUS111
      NEVENT(1) = NEVENT(1) + 1                                         ASKUS112
C                                                                       ASKUS113
      IF (IERROR.EQ.0) ISTAT = 100                                      ASKUS114
C                                                                       ASKUS115
C do we want to print the event ?                                       ASKUS116
C                                                                       ASKUS117
      IF (NEVHEP.GE.IDEBB.AND.NEVHEP.LE.IDEBE) CALL HWUEPR              ASKUS118
C                                                                       ASKUS119
      IF(ISTAT.GE.100) THEN                                             ASKUS120
        ISTA = 0                                                        ASKUS121
      ELSE                                                              ASKUS122
        ISTA = -99                                                      ASKUS123
        NEVENT(6) = NEVENT(6) + 1                                       ASKUS124
        GOTO 97                                                         ASKUS125
      ENDIF                                                             ASKUS126
C                                                                       ASKUS127
      ECMS = SNGL(PBEAM1 + PBEAM2)                                      ASKUS128
C                                                                       ASKUS129
      WEIT = 1.                                                         ASKUS130
C                                                                       ASKUS131
      IST = 0                                                           ASKUS132
C                                                                       ASKUS133
      CALL KXHEAL(VRTEX,IST,NVRT,NTRK)                                  ASKUS134
C                                                                       ASKUS135
      ISTA = IST                                                        ASKUS136
C                                                                       ASKUS137
C here we have to do the loop over the possible KXHEAL errors           ASKUS138
C                                                                       ASKUS139
      IF (ISTA.EQ.-2) THEN                                              ASKUS140
        NEVENT(3) = NEVENT(3) + 1                                       ASKUS141
        GOTO 97                                                         ASKUS142
      ENDIF                                                             ASKUS143
C                                                                       ASKUS144
      IF (ISTA.EQ.-5) THEN                                              ASKUS145
        NEVENT(4) = NEVENT(4) + 1                                       ASKUS146
        GOTO 97                                                         ASKUS147
      ENDIF                                                             ASKUS148
C                                                                       ASKUS149
      IF (ISTA.GT.0) THEN                                               ASKUS150
        NEVENT(7) = NEVENT(7) + 1                                       ASKUS151
        GOTO 97                                                         ASKUS152
      ENDIF                                                             ASKUS153
C                                                                       ASKUS154
   97 IF(ISTA.NE.0) THEN                                                ASKUS155
C                                                                       ASKUS156
C here we are if an error has occured                                   ASKUS157
C count them and print out the random number                            ASKUS158
C                                                                       ASKUS159
        NEVENT(9) = NEVENT(9) + 1                                       ASKUS160
        WRITE(IOUT,1000)ISTA,ISEED                                      ASKUS161
 1000   FORMAT(1X,'+++ASKUSE+++ Error occured status code = ',I5,/,1X,  ASKUS162
     &            '             RMAR seeds = ',3(I15))                  ASKUS163
C                                                                       ASKUS164
      ELSE                                                              ASKUS165
        NEVENT(10) = NEVENT(10) + 1                                     ASKUS166
C                                                                       ASKUS167
C user's event processing                                               ASKUS168
C                                                                       ASKUS169
        CALL USTORE                                                     ASKUS170
C                                                                       ASKUS171
C fill default histograms                                               ASKUS172
C                                                                       ASKUS173
        CALL UHRFI                                                      ASKUS174
C                                                                       ASKUS175
      ENDIF                                                             ASKUS176
C                                                                       ASKUS177
      RETURN                                                            ASKUS178
      END                                                               ASKUS179
      SUBROUTINE USCJOB                                                 USCJOB 2
C                                                                       USCJOB 3
C --------------------------------------------------------------------  USCJOB 4
C Ebi Lange, December 1988.                                             USCJOB 5
C updated AST December 1993.                                            USCJOB 6
C --------------------------------------------------------------------  USCJOB 7
C                                                                       USCJOB 8
      IMPLICIT NONE                                                     USCJOB 9
C                                                                       USCJOB10
      DOUBLE PRECISION                                                  HWDEFI 2
     & PHEP,VHEP,PBEAM1,PBEAM2,QCDLAM,VGCUT,VQCUT,VPCUT,BETAF,CAFAC,    HWDEFI 3
     & CFFAC,CLMAX,CLPOW,PSPLT,QSPAC,PTRMS,PXRMS,QG,QV,SWEIN,SCABI,     HWDEFI 4
     & PDIQK,QDIQK,ENSOF,TMTOP,ZBINM,GAMW,GAMZ,GAMH,PGSMX,PGSPL,PPAR,   HWDEFI 5
     & VPAR,PHIPAR,DECPAR,RHOPAR,RHOHEP,XFACT,PTINT,EVWGT,AVWGT,WGTMAX, HWDEFI 6
     & WGTSUM,WSQSUM,WBIGST,TLOUT,YJMIN,YJMAX,PTMIN,PTMAX,PTPOW,EMMIN,  HWDEFI 7
     & EMMAX,EMPOW,Q2MIN,Q2MAX,Q2POW,THMAX,QLIM,XXMIN,XLMIN,EMSCA,      HWDEFI 8
     & EMLST,COSTH,GPOLN,GCOEF,XX,DISF,RESN,RMIN,CTMAX,FBTM,            HWDEFI 9
     & FTOP,FHVY,RMASS,BFRAC,CMMOM,ACCUR,QEV,SUD,VECWT,TENWT,DECWT,     HWDEFI10
     & QWT,PWT,SWT,SWTEF,RESWT,PIFAC,QCDL3,QCDL5,BRHIG,GAMMAX,          HWDEFI11
     & ENHANC,B1LIM,ALPFAC,Q2WWMN,Q2WWMX,BTCLM,ETAMIX,                  HWDEFI12
     & QFCH,VFCH,AFCH,VCKM,TQWT,CLQ,EPOLN,PPOLN,COSS,SINS,GAMZP,GEV2NB, HWDEFI13
     & ALPHEM,PRSOF,EBEAM1,EBEAM2,ZJMAX,YBMIN,YBMAX,HARDST,CLSMR,PHOMAS HWDEFI14
C                                                                       HWDEFI15
      INTEGER NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP,            HWDEFI16
     & IPROC,MAXEV,IPRINT,LRSUD,LWSUD,NCOLO,NFLAV,MODPDF(2),NSTRU,      HWDEFI17
     & NZBIN,NBTRY,NCTRY,NDTRY,NETRY,NSTRY,NGSPL,NMXPAR,NEVPAR,         HWDEFI18
     & NPAR,ISTPAR,IDPAR,JMOPAR,JDAPAR,JCOPAR,INHAD,JNHAD,NSPAC,NRN,    HWDEFI19
     & MAXER,MAXPR,LWEVT,ISTAT,IERROR,NWGTS,IDHW,IBSH,IBRN,IPRO,        HWDEFI20
     & IFLMIN,IFLMAX,MAXFL,IDCMF,IHPRO,IDN,ICO,LOCN,                    HWDEFI21
     & NRES,IDPDG,ICHRG,MADDR,MODES,MODEF,IDPRO,INTER,NQEV,             HWDEFI22
     & NSUD,NMXSUD,MODBOS,IOPHIG,MODMAX,SUDORD,CLDIR,NUMER,NUMERU,      HWDEFI23
     & MAPQ,IPART1,IPART2,ISPAC,IAPHIG                                  HWDEFI24
C                                                                       HWDEFI25
      LOGICAL AZSOFT,AZSPIN,FROST,GENEV,BGSHAT,NOWGT,TMPAR,PRNDEC,      HWDEFI26
     & HVFCEN,FSTEVT,FSTWGT,BREIT,USECMF,ZPRIME,TPOL,GENSOF,            HWDEFI27
     & HARDME,SOFTME,NOSPAC                                             HWDEFI28
C                                                                       HWDEFI29
      CHARACTER*4 PART1,PART2,RNAME,BDECAY                              HWDEFI30
      CHARACTER*20 AUTPDF(2)                                            HWDEFI31
C                                                                       HWDEFI32
      PARAMETER (NMXHEP=2000)                                           HWDEFI33
      INTEGER NMXJET                                                    HWDEFI34
      PARAMETER (NMXJET=200)                                            HWDEFI35
C                                                                       HWDEFI36
      COMMON/HWPROC/PBEAM1,PBEAM2,EBEAM1,EBEAM2,IPROC,MAXEV             HWPROC 2
C                                                                       HWPROC 3
      INTEGER NCOL,MAXFLA                                               DTMINP 2
C                                                                       DTMINP 3
      PARAMETER (NCOL = 41)                                             DTMINP 4
      PARAMETER (MAXFLA=7)                                              DTMINP 5
C                                                                       DTMINP 6
      INTEGER NEVENT,IFLC,IOUT,IDEBB,IDEBE                              DTMINP 7
      REAL SDVRT,TABL                                                   DTMINP 8
C                                                                       DTMINP 9
      COMMON / DTMILL / SDVRT(3),TABL(NCOL),NEVENT(10),IFLC(MAXFLA)     DTMINP10
      COMMON / INPOUT / IOUT                                            DTMINP11
      COMMON / DTBUG / IDEBB,IDEBE                                      DTMINP12
C                                                                       DTMINP13
      INTEGER IHARD,IFL                                                 INIPRO 2
C                                                                       INIPRO 3
      COMMON /INIPRO/ IHARD,IFL                                         INIPRO 4
C                                                                       INIPRO 5
C                                                                       USCJOB15
C terminate elementary process                                          USCJOB16
C                                                                       USCJOB17
      IF (IHARD.EQ.0) CALL HWEFIN                                       USCJOB18
      IF (IHARD.EQ.3) CALL DYMUND                                       USCJOB19
C                                                                       USCJOB20
C user's terminal calculations                                          USCJOB21
C                                                                       USCJOB22
      CALL USTOPP                                                       USCJOB23
C                                                                       USCJOB24
      WRITE(IOUT,101)                                                   USCJOB25
C                                                                       USCJOB26
  101 FORMAT(//20X,'EVENTS STATISTICS',                                 USCJOB27
     &        /20X,'*****************')                                 USCJOB28
C                                                                       USCJOB29
      WRITE(IOUT,102)NEVENT(1),NEVENT(10),NEVENT(9)                     USCJOB30
C                                                                       USCJOB31
  102 FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,         USCJOB32
     &       /5X,'# OF ACCEPTED  EVENTS                = ',I10,         USCJOB33
     &       /5X,'# OF REJECTED  EVENTS                = ',I10)         USCJOB34
C                                                                       USCJOB35
      IF(IPROC.LT.150) THEN                                             USCJOB36
        WRITE(IOUT,103)                                                 USCJOB37
C                                                                       USCJOB38
  103   FORMAT(//20X,'GENERATED FLAVOURS',                              USCJOB39
     &        /20X,'******************')                                USCJOB40
C                                                                       USCJOB41
        WRITE(IOUT,104)IFLC(2),IFLC(1),IFLC(3),IFLC(4),IFLC(5),IFLC(6)  USCJOB42
     &  ,IFLC(7)                                                        USCJOB43
C                                                                       USCJOB44
  104   FORMAT(/5X,'# OF UUBAR EVENTS                    = ',I10,       USCJOB45
     &       /5X,'# OF DDBAR EVENTS                    = ',I10,         USCJOB46
     &       /5X,'# OF SSBAR EVENTS                    = ',I10,         USCJOB47
     &       /5X,'# OF CCBAR EVENTS                    = ',I10,         USCJOB48
     &       /5X,'# OF BBBAR EVENTS                    = ',I10,         USCJOB49
     &       /5X,'# OF TTBAR EVENTS                    = ',I10,         USCJOB50
     &       /5X,'# OF GGBAR EVENTS                    = ',I10)         USCJOB51
      ENDIF                                                             USCJOB52
C                                                                       USCJOB53
      WRITE(IOUT,110)                                                   USCJOB54
C                                                                       USCJOB55
  110 FORMAT(//20X,'ERRORS STATISTICS',                                 USCJOB56
     &        /20X,'*****************')                                 USCJOB57
C                                                                       USCJOB58
      WRITE(IOUT,111)NEVENT(3),NEVENT(4),                               USCJOB59
     &               NEVENT(6),NEVENT(7)                                USCJOB60
C                                                                       USCJOB61
  111 FORMAT(/10X,'ISTA =  -2 BOS ERROR VERT/KINE     # OF REJECT = ',  USCJOB62
     & I10,                                                             USCJOB63
     &       /10X,'ISTA =  -5 INTERFACE ERROR         # OF REJECT = ',  USCJOB64
     & I10,                                                             USCJOB65
     &       /10X,'ISTA = -99 GENERATOR ERROR         # OF REJECT = ',  USCJOB66
     & I10,                                                             USCJOB67
     &       /10X,'ISTA >   0 UNKNOWN PART            # OF REJECT = ',  USCJOB68
     & I10,/,/)                                                         USCJOB69
C                                                                       USCJOB70
      RETURN                                                            USCJOB71
      END                                                               USCJOB72
      SUBROUTINE USTART                                                 USTART 2
C                                                                       USTART 3
C user's routine for initialization                                     USTART 4
C                                                                       USTART 5
C---------------------------------------------------------------------- USTART 6
C                                                                       USTART 7
      IMPLICIT NONE                                                     USTART 8
C                                                                       USTART 9
      RETURN                                                            USTART10
  999 END                                                               USTART11
      SUBROUTINE USTOPP                                                 USTOPP 2
C                                                                       USTOPP 3
C user's routine for terminal calculations, histogram output, etc       USTOPP 4
C                                                                       USTOPP 5
C---------------------------------------------------------------------- USTOPP 6
C                                                                       USTOPP 7
      IMPLICIT NONE                                                     USTOPP 8
C                                                                       USTOPP 9
      RETURN                                                            USTOPP10
  999 END                                                               USTOPP11
      SUBROUTINE USTORE                                                 USTORE 2
C                                                                       USTORE 3
C user's routine to process data from event                             USTORE 4
C                                                                       USTORE 5
C---------------------------------------------------------------------- USTORE 6
C                                                                       USTORE 7
      IMPLICIT NONE                                                     USTORE 8
C                                                                       USTORE 9
      RETURN                                                            USTORE10
C                                                                       USTORE11
  999 END                                                               USTORE12
       SUBROUTINE KXHEPA (JPART,JKLIN)                                  KXHEPA 2
C --------------------------------------------------                    KXHEPA 3
C - B.Bloch - 870300      modified by F.Ranjard - 870423                KXHEPA 4
C - arranged for HERWIG by E. Lange - 881214                            KXHEPA 5
C - updated by E. Lange for new PART bank - 890910                      KXHEPA 6
C                                                                       KXHEPA 7
C  fill 'PART' bank with HERWIG particles                               KXHEPA 8
C  Get  the NOtracking marker word NOTRK from KRUN bank                 KXHEPA 9
C  Fill KLIN bank with HERWIG particle# which correspond                KXHEPA10
C       to GEANT particles                                              KXHEPA11
C  Get  HERWIG particles and transfer them to PART bank                 KXHEPA12
C       with a GEANT# and a tracking type set to NOTRK                  KXHEPA13
C       because they are not used by GEANT.                             KXHEPA14
C  Reduce PART and KLIN banks to their normal size                      KXHEPA15
C                                                                       KXHEPA16
C - structure: SUBROUTINE subprogram                                    KXHEPA17
C              User Entry Name: KXHEPA                                  KXHEPA18
C              External References: NAMIND(BOS77)                       KXHEPA19
C                                   KGPART/KBKLIN/KBPART/KXLUTO/AUBPRS  KXHEPA20
C                                   (ALEPHLIB)                          KXHEPA21
C                                   IUCOMP(CERNLIB)                     KXHEPA22
C              Comdecks referenced: BCS, BMACRO,KMACRO                  KXHEPA23
C                                                                       KXHEPA24
C - Usage    : CALL KXHEPA (JPART,JKLIN)                                KXHEPA25
C - Output   : JPART   = KBPART return flag                             KXHEPA26
C                        .gt. 0 means OK                                KXHEPA27
C              JKLIN   = KBKLIN return flag                             KXHEPA28
C                        .gt. 0 means OK                                KXHEPA29
C                                                                       KXHEPA30
      IMPLICIT NONE                                                     KXHEPA31
C                                                                       KXHEPA32
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(1000)                                                     BCS    6
      COMMON /BCS/   IW(1000)                                           BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C                                                                       KXHEPA34
      REAL ELEP,DMAS                                                    KXHEPA35
C                                                                       KXHEPA36
      PARAMETER (ELEP=120.)                                             KXHEPA37
      PARAMETER (DMAS=0.)                                               KXHEPA38
C                                                                       KXHEPA39
C IHEGE(LHEGE) are the HERWIG numbers corresponding to the first        KXHEPA40
C              part of PART bank (used by GEANT)                        KXHEPA41
C ILUAL(LLUAL) are the HERWIG numbers corresponding to the rest of      KXHEPA42
C              the PART bank                                            KXHEPA43
C                                                                       KXHEPA44
      INTEGER LHEGE,LHEAL,LASTP                                         KXHEPA45
C                                                                       KXHEPA46
      PARAMETER (LHEGE=52, LHEAL=315)                                   KXHEPA47
      PARAMETER (LASTP=264)                                             KXHEPA48
C                                                                       KXHEPA49
      INTEGER IHEGE(LHEGE), IHEAL(LHEAL)                                KXHEPA50
      INTEGER JSTORE(LASTP)                                             KXHEPA51
      INTEGER JPART,JKLIN                                               KXHEPA52
      INTEGER I,KNOTR,IPART,MYP,IUCOMP,JANTI(LASTP),MPART               KXHEPA53
      INTEGER KBKLIN,KBPART,KMPART,KNOTRK                               KXHEPA54
C                                                                       KXHEPA55
      REAL ZMAS,CHAR,TLIF                                               KXHEPA56
C                                                                       KXHEPA57
      INTEGER NAMIND                                                    KXHEPA58
      EXTERNAL NAMIND                                                   KXHEPA59
C                                                                       KXHEPA60
      CHARACTER*12 TNAM                                                 KXHEPA61
C                                                                       KXHEPA62
      DATA IHEGE /59,127,121,0,129,123,21,38,30,61,                     KXHEPA63
     &           46,34,75,73,91,60,0,78,86,79,                          KXHEPA64
     &           81,88,83,90,93,96,104,97,99,106,                       KXHEPA65
     &           101,108,131,125,136,171,140,175,144,179,               KXHEPA66
     &           150,198,199,200,0,0,0,0,0,0,                           KXHEPA67
     &           0,0/                                                   KXHEPA68
C                                                                       KXHEPA69
      DATA IHEAL /185,201,  0, 50, 42,122,128,124,                      KXHEPA70
     &            130,126,132, 40, 32, 27,  0,  0, 0,165,               KXHEPA71
     &              0,  0,164,166,163, 39, 31, 47, 35, 51,              KXHEPA72
     &             43,141,176,137,172,145,180, 23, 24, 56,              KXHEPA73
     &              0,  0,  0,231,  0,  0,  0,  0,  0, 13,              KXHEPA74
     &              2,  1,  3,  4,  5,  6,  8,  7,  9, 10,              KXHEPA75
     &             11,  12, 0,  0,222,246,245,221,247,223,              KXHEPA76
     &            230,254,  0,  0,  0,  0,  0,  0,  0,  0,              KXHEPA77
     &            234,256,233,255,235,257,242,264,243,232,10*0,         KXHEPA78
     &             85,103, 74, 92, 76, 94, 77, 95, 87,105,              KXHEPA79
     &             80, 98, 82,100, 89,107, 84,102,148,183,              KXHEPA80
     &            151,186,153,188,156,191,159,194,161,196,              KXHEPA81
     &            155,190,158,193,149,184,152,187,154,189,              KXHEPA82
     &            157,192,160,195,162,197,  0,  0,  0,  0,10*0,         KXHEPA83
     &            224,248,  0,  0,226,250,227,251,228,252,              KXHEPA84
     &            229,253,  0,  0,  0,  0,  0,  0,  0,  0,              KXHEPA85
     &              0,  0,  0,  0,  0,  0,  0,  0,236,258,              KXHEPA86
     &              0,  0,238,260,239,261,240,262,241,263,              KXHEPA87
     &              0,  0,  0,  0,  0,  0,  0,  0,225,249,              KXHEPA88
     &              0,  0,  0,  0,  0,  0,  0,  0,  0,  0,              KXHEPA89
     &            237,259,  0,  0,  0,  0,  0,  0,  0,  0,80*0,         KXHEPA90
     &              0,  0,  0,  0,  0,  0,  0/                          KXHEPA91
      DATA JANTI /                                                      KXHEPA92
     &  7,  8,  9, 10, 11, 12,  1,  2,  3,  4,                          KXHEPA93
     &  5,  6, 13, 14, 15, 16, 17, 18, 19, 20,                          KXHEPA94
     & 21, 22, 23, 24, 25, 26, 27, 28, 29, 38,                          KXHEPA95
     & 39, 40, 41, 46, 47, 48, 49, 30, 31, 32,                          KXHEPA96
     & 33, 50, 51, 52, 53, 34, 35, 36, 37, 42,                          KXHEPA97
     & 43, 44, 45, 54, 55, 56, 57, 58, 59, 60,                          KXHEPA98
     & 61, 62, 63, 64, 65, 66, 67, 68, 69, 70,                          KXHEPA99
     & 71, 72, 91, 92, 93, 94, 95, 96, 97, 98,                          KXHEP100
     & 99,100,101,102,103,104,105,106,107,108,                          KXHEP101
     & 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,                          KXHEP102
     & 83, 84, 85, 86, 87, 88, 89, 90,115,116,                          KXHEP103
     &117,118,119,120,109,110,111,112,113,114,                          KXHEP104
     &127,128,129,130,131,132,121,122,123,124,                          KXHEP105
     &125,126,133,134,135,171,172,173,174,175,                          KXHEP106
     &176,177,178,179,180,181,182,183,184,185,                          KXHEP107
     &186,187,188,189,190,191,192,193,194,195,                          KXHEP108
     &196,197,163,164,165,166,167,168,169,170,                          KXHEP109
     &136,137,138,139,140,141,142,143,144,145,                          KXHEP110
     &146,147,148,149,150,151,152,153,154,155,                          KXHEP111
     &156,157,158,159,160,161,162,199,198,200,                          KXHEP112
     &201,202,203,204,205,206,207,208,217,216,                          KXHEP113
     &217,212,213,214,211,210,211,218,219,220,                          KXHEP114
     &245,246,247,248,249,250,251,252,253,254,                          KXHEP115
     &231,243,255,256,257,258,259,260,261,262,                          KXHEP116
     &263,264,232,244,221,222,223,224,225,226,                          KXHEP117
     &227,228,229,230,233,234,235,236,237,238,                          KXHEP118
     &239,240,241,242 /                                                 KXHEP119
C                                                                       KXHEP120
C HERWIG commons                                                        KXHEP121
C                                                                       KXHEP122
      DOUBLE PRECISION                                                  HWDEFI 2
     & PHEP,VHEP,PBEAM1,PBEAM2,QCDLAM,VGCUT,VQCUT,VPCUT,BETAF,CAFAC,    HWDEFI 3
     & CFFAC,CLMAX,CLPOW,PSPLT,QSPAC,PTRMS,PXRMS,QG,QV,SWEIN,SCABI,     HWDEFI 4
     & PDIQK,QDIQK,ENSOF,TMTOP,ZBINM,GAMW,GAMZ,GAMH,PGSMX,PGSPL,PPAR,   HWDEFI 5
     & VPAR,PHIPAR,DECPAR,RHOPAR,RHOHEP,XFACT,PTINT,EVWGT,AVWGT,WGTMAX, HWDEFI 6
     & WGTSUM,WSQSUM,WBIGST,TLOUT,YJMIN,YJMAX,PTMIN,PTMAX,PTPOW,EMMIN,  HWDEFI 7
     & EMMAX,EMPOW,Q2MIN,Q2MAX,Q2POW,THMAX,QLIM,XXMIN,XLMIN,EMSCA,      HWDEFI 8
     & EMLST,COSTH,GPOLN,GCOEF,XX,DISF,RESN,RMIN,CTMAX,FBTM,            HWDEFI 9
     & FTOP,FHVY,RMASS,BFRAC,CMMOM,ACCUR,QEV,SUD,VECWT,TENWT,DECWT,     HWDEFI10
     & QWT,PWT,SWT,SWTEF,RESWT,PIFAC,QCDL3,QCDL5,BRHIG,GAMMAX,          HWDEFI11
     & ENHANC,B1LIM,ALPFAC,Q2WWMN,Q2WWMX,BTCLM,ETAMIX,                  HWDEFI12
     & QFCH,VFCH,AFCH,VCKM,TQWT,CLQ,EPOLN,PPOLN,COSS,SINS,GAMZP,GEV2NB, HWDEFI13
     & ALPHEM,PRSOF,EBEAM1,EBEAM2,ZJMAX,YBMIN,YBMAX,HARDST,CLSMR,PHOMAS HWDEFI14
C                                                                       HWDEFI15
      INTEGER NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP,            HWDEFI16
     & IPROC,MAXEV,IPRINT,LRSUD,LWSUD,NCOLO,NFLAV,MODPDF(2),NSTRU,      HWDEFI17
     & NZBIN,NBTRY,NCTRY,NDTRY,NETRY,NSTRY,NGSPL,NMXPAR,NEVPAR,         HWDEFI18
     & NPAR,ISTPAR,IDPAR,JMOPAR,JDAPAR,JCOPAR,INHAD,JNHAD,NSPAC,NRN,    HWDEFI19
     & MAXER,MAXPR,LWEVT,ISTAT,IERROR,NWGTS,IDHW,IBSH,IBRN,IPRO,        HWDEFI20
     & IFLMIN,IFLMAX,MAXFL,IDCMF,IHPRO,IDN,ICO,LOCN,                    HWDEFI21
     & NRES,IDPDG,ICHRG,MADDR,MODES,MODEF,IDPRO,INTER,NQEV,             HWDEFI22
     & NSUD,NMXSUD,MODBOS,IOPHIG,MODMAX,SUDORD,CLDIR,NUMER,NUMERU,      HWDEFI23
     & MAPQ,IPART1,IPART2,ISPAC,IAPHIG                                  HWDEFI24
C                                                                       HWDEFI25
      LOGICAL AZSOFT,AZSPIN,FROST,GENEV,BGSHAT,NOWGT,TMPAR,PRNDEC,      HWDEFI26
     & HVFCEN,FSTEVT,FSTWGT,BREIT,USECMF,ZPRIME,TPOL,GENSOF,            HWDEFI27
     & HARDME,SOFTME,NOSPAC                                             HWDEFI28
C                                                                       HWDEFI29
      CHARACTER*4 PART1,PART2,RNAME,BDECAY                              HWDEFI30
      CHARACTER*20 AUTPDF(2)                                            HWDEFI31
C                                                                       HWDEFI32
      PARAMETER (NMXHEP=2000)                                           HWDEFI33
      INTEGER NMXJET                                                    HWDEFI34
      PARAMETER (NMXJET=200)                                            HWDEFI35
C                                                                       HWDEFI36
      COMMON/HWUPDT/RMASS(264),BFRAC(460),CMMOM(460),ETAMIX,IDPDG(264), HWUPDT 2
     & ICHRG(264),MADDR(264),MODES(264),MODEF(264),IDPRO(3,460),NRES    HWUPDT 3
C---MAX NUMBER OF ENTRIES IN LOOKUP TABLES OF SUDAKOV FORM FACTORS      HWUPDT 4
      COMMON/HWUNAM/RNAME(264)                                          HWUNAM 2
C                                                                       KXHEP126
      INTEGER ID,NRBOS,L                                                BMACRO 2
      INTEGER LCOLS,LROWS,KNEXT,KROW,LFRWRD,LFRROW,ITABL                BMACRO 3
      REAL RTABL                                                        BMACRO 4
C                                                                       BMACRO 5
      INTEGER NAPART,JVK,JPA,NLINK                                      BMACRO 6
      INTEGER KNEXVK,LFRVK,KPARVK,KLISVK,NOFVK,KINTYP,INPTRK,           BMACRO 7
     &        INPVRT,MOTHVK                                             BMACRO 8
      REAL CHARGE,PARMAS,TIMLIF,PMODVK,ENERVK,TOFLIT,RADVK,PMASVK       BMACRO 9
C - # of words/row in bank with index ID                                BMACRO10
      LCOLS(ID) = IW(ID+1)                                              BMACRO11
C - # of rows in bank with index ID                                     BMACRO12
      LROWS(ID) = IW(ID+2)                                              BMACRO13
C - index of next row in the bank with index ID                         BMACRO14
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO15
C - index of row # NRBOS in the bank with index ID                      BMACRO16
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO17
C - # of free words in the bank with index ID                           BMACRO18
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO19
C - # of free rows in the bank with index ID                            BMACRO20
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO21
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO22
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO23
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO24
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO25
C                                                                       BMACRO26
C - index of the next vertex/track to be stored in KINE/VERT            KMACRO 2
C   bank known by its index JVK                                         KMACRO 3
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)                 KMACRO 4
C - # of vertices/tracks which could be stored in KINE/VERT             KMACRO 5
C   bank known by its index JVK                                         KMACRO 6
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))           KMACRO 7
C - index of the 1st parameter of KINE/VERT bank known by its           KMACRO 8
C   index JVK                                                           KMACRO 9
      KPARVK(JVK) = JVK + IW(JVK+1)                                     KMACRO10
C - index of 1st vertex/track # contained into the list of              KMACRO11
C   bank KINE/VERT known by its index JVK                               KMACRO12
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)                         KMACRO13
C - charge of ALEPH particle# JPA                                       KMACRO14
      CHARGE(JPA) = RTABL(IW(NAPART),JPA,7)                             KMACRO15
C - mass of ALEPH particle# JPA                                         KMACRO16
      PARMAS(JPA) = RTABL(IW(NAPART),JPA,6)                             KMACRO17
C - time of life of ALEPH particle# JPA                                 KMACRO18
      TIMLIF(JPA) = RTABL(IW(NAPART),JPA,8)                             KMACRO19
C - # of vertices on a track known by its BOS index /                   KMACRO20
C   # of outgoing tracks of a vertex known by its BOS index             KMACRO21
      NOFVK(JVK)  = IW(JVK+3)                                           KMACRO22
C - Particle type of a track known by its BOS index                     KMACRO23
      KINTYP(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO24
C - incoming track # of a vertex known by its BOS index                 KMACRO25
      INPTRK(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO26
C - origin vertex # of a track known by its BOS index                   KMACRO27
      INPVRT(JVK) = IW(KLISVK(JVK)+1)                                   KMACRO28
C - momentum of a track known by its BOS index                          KMACRO29
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2   KMACRO30
     &                   +RW(KPARVK(JVK)+3)**2)                         KMACRO31
C - MASS of a track known by its BOS index  JVK                         KMACRO32
      PMASVK(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO33
C - energy of a track known by its BOS index JVK                        KMACRO34
      ENERVK(JVK) = SQRT(PMODVK(JVK)**2+PMASVK(JVK)**2)                 KMACRO35
C - time of flight of the icoming particle to the vertex known by its   KMACRO36
C   BOS index JVK                                                       KMACRO37
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO38
C - radius of the vertex known by its BOS index                         KMACRO39
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)  KMACRO40
C - mother track # of a track known by its BOS index                    KMACRO41
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))                  KMACRO42
C                                                                       KMACRO43
C                                                                       KXHEP129
      DATA NAPART/0/                                                    KXHEP130
C                                                                       KXHEP131
C ------------------------------------------------------                KXHEP132
C                                                                       KXHEP133
      IF (NAPART.EQ.0) NAPART = NAMIND('PART')                          KXHEP134
C                                                                       KXHEP135
      DO 100 I = 1,LASTP                                                KXHEP136
        JSTORE(I) = 0                                                   KXHEP137
  100 CONTINUE                                                          KXHEP138
C                                                                       KXHEP139
C NOtrack marker word stored in KRUN bank                               KXHEP140
C                                                                       KXHEP141
      KNOTRK = ITABL (IW(NAMIND('KRUN')),1,2)                           KXHEP142
C                                                                       KXHEP143
C Fill KLIN with HERWIG particle# for the GEANT particles               KXHEP144
C which are the 1st LHEGE particles of PART                             KXHEP145
C adjust masses to standard ALEPH masses                                KXHEP146
C                                                                       KXHEP147
      DO 1 IPART = 1,LHEGE                                              KXHEP148
        IF (IHEGE(IPART).NE.0) THEN                                     KXHEP149
          JKLIN = KBKLIN (IPART,IHEGE(IPART))                           KXHEP150
          IF (JKLIN .LE. 0) GOTO 998                                    KXHEP151
          RMASS(IHEGE(IPART)) = DBLE(PARMAS(IPART))                     KXHEP152
        ENDIF                                                           KXHEP153
 1    CONTINUE                                                          KXHEP154
C                                                                       KXHEP155
C extent the KLIN bank                                                  KXHEP156
C adjust masses to standard ALEPH masses                                KXHEP157
C                                                                       KXHEP158
      DO 2 IPART = LHEGE+1,LHEAL+LHEGE                                  KXHEP159
        IF (IHEAL(IPART-LHEGE).NE.0) THEN                               KXHEP160
          JKLIN = KBKLIN(IPART,IHEAL(IPART-LHEGE))                      KXHEP161
          IF (JKLIN .LE. 0) GOTO 998                                    KXHEP162
          RMASS(IHEAL(IPART-LHEGE)) = DBLE(PARMAS(IPART))               KXHEP163
        ENDIF                                                           KXHEP164
    2 CONTINUE                                                          KXHEP165
C                                                                       KXHEP166
C Get HERWIG particles and transfer them to PART                        KXHEP167
C if their mass is in the ELEP energy range                             KXHEP168
C these particles are not tracked so their GEANT#                       KXHEP169
C and tracking type are set to KNOTRK                                   KXHEP170
C                                                                       KXHEP171
      DO 1000 MYP = 1,LASTP                                             KXHEP172
        IF (IUCOMP(MYP,IHEGE,LHEGE).GT.0) GOTO 1000                     KXHEP173
        IF (IUCOMP(MYP,IHEAL,LHEAL).GT.0) GOTO 1000                     KXHEP174
        IF (JSTORE(MYP).NE.0) GOTO 1000                                 KXHEP175
        TNAM = RNAME(MYP)                                               KXHEP176
        IF (TNAM .EQ. '    ') GOTO 1000                                 KXHEP177
        ZMAS = SNGL(RMASS (MYP))                                        KXHEP178
        IF (ZMAS.GT.ELEP) GOTO 1000                                     KXHEP179
C                                                                       KXHEP180
C take special care for charge of diquarks                              KXHEP181
C                                                                       KXHEP182
        IF (MYP.GE.109.AND.MYP.LE.120) THEN                             KXHEP183
          CHAR = FLOAT(ICHRG(MYP))/3.                                   KXHEP184
        ELSE                                                            KXHEP185
          CHAR = FLOAT(ICHRG(MYP))                                      KXHEP186
        ENDIF                                                           KXHEP187
C                                                                       KXHEP188
        CALL KXHETO(MYP,TLIF)                                           KXHEP189
C                                                                       KXHEP190
C store the new particle# JPART                                         KXHEP191
C                                                                       KXHEP192
        JPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,CHAR,TLIF)              KXHEP193
        IF (JPART.LE.0) GOTO 998                                        KXHEP194
        JSTORE(MYP) = 1                                                 KXHEP195
C                                                                       KXHEP196
C store the user generator particle# of the new particle                KXHEP197
C                                                                       KXHEP198
        JKLIN = KBKLIN (JPART,MYP)                                      KXHEP199
        IF (JKLIN.LE.0) GOTO 998                                        KXHEP200
C                                                                       KXHEP201
C do the same for the antiparticle except if identical                  KXHEP202
C                                                                       KXHEP203
        IF (JANTI(MYP).EQ.MYP) THEN                                     KXHEP204
          MPART = KMPART(JPART,DMAS,JPART)                              KXHEP205
          IF (MPART.LE.0) GOTO 998                                      KXHEP206
        ELSE                                                            KXHEP207
          TNAM = RNAME(JANTI(MYP))                                      KXHEP208
          JPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,-CHAR,TLIF)           KXHEP209
          IF (JPART.LE.0) GOTO 998                                      KXHEP210
                                                                        KXHEP211
          JSTORE(JANTI(MYP)) = 1                                        KXHEP212
          MPART = KMPART(JPART,DMAS,JPART-1)                            KXHEP213
          IF (MPART.LE.0) GOTO 998                                      KXHEP214
          MPART = KMPART(JPART-1,DMAS,JPART)                            KXHEP215
          IF (MPART.LE.0) GOTO 998                                      KXHEP216
          JKLIN = KBKLIN (JPART,JANTI(MYP))                             KXHEP217
          IF (JKLIN.LE.0) GOTO 998                                      KXHEP218
        ENDIF                                                           KXHEP219
C                                                                       KXHEP220
 1000 CONTINUE                                                          KXHEP221
C                                                                       KXHEP222
      CALL AUBPRS ('PARTKLIN')                                          KXHEP223
C                                                                       KXHEP224
      GOTO 999                                                          KXHEP225
C                                                                       KXHEP226
C not enough space                                                      KXHEP227
C                                                                       KXHEP228
 998  CONTINUE                                                          KXHEP229
C                                                                       KXHEP230
      WRITE (IW(6),'(/1X,''+++KXHEPA+++ not enough space for PART''     KXHEP231
     &         ,'' or KLIN bank'')')                                    KXHEP232
C                                                                       KXHEP233
C End                                                                   KXHEP234
C                                                                       KXHEP235
 999  CONTINUE                                                          KXHEP236
C                                                                       KXHEP237
      END                                                               KXHEP238
        SUBROUTINE KXHETO(KHER,TLIF)                                    KXHETO 2
C-----------------------------------------------------------------------KXHETO 3
C A. Blondel  24 - 06 - 86                                              KXHETO 4
C Modified june 15, 1988   B. Bloch for updated B life times            KXHETO 5
C Arranged for HERWIG  by E. Lange 881214                               KXHETO 6
C application routine to find lifetime                                  KXHETO 7
C  of non-standard particles                                            KXHETO 8
C - input   :  KHER = HERWIG particle code                              KXHETO 9
C - output  :  TLIF = particle lifetime ( in seconds )                  KXHETO10
C Useful only for particles not defined in the ALEPH database           KXHETO11
C-----------------------------------------------------------------------KXHETO12
C                                                                       KXHETO13
      IMPLICIT NONE                                                     KXHETO14
C                                                                       KXHETO15
      INTEGER K,KHER                                                    KXHETO16
C                                                                       KXHETO17
      REAL TCHAB,TBMES,TBBAR                                            KXHETO18
      REAL TLIF                                                         KXHETO19
C                                                                       KXHETO20
        DATA       TCHAB ,          TBMES  ,         TBBAR              KXHETO21
     + /        3.E-13            , 1.0E-12    ,    1.0E-12      /      KXHETO22
C         Charmed baryons          B   mesons     B   baryons           KXHETO23
C                                                                       KXHETO24
        TLIF=0.                                                         KXHETO25
        K=KHER                                                          KXHETO26
C                                                                       KXHETO27
C neutrinos                                                             KXHETO28
C                                                                       KXHETO29
        IF(     K.EQ.122 .OR. K.EQ.124 .OR. K.EQ.126                    KXHETO30
     &     .OR. K.EQ.128 .OR. K.EQ.130 .OR. K.EQ.132) TLIF = 1.E+15     KXHETO31
C                                                                       KXHETO32
C GEANT-known particles and mesons (except beauty)                      KXHETO33
C                                                                       KXHETO34
         IF (K.LE.135.OR.K.GE.255) GOTO 999                             KXHETO35
C                                                                       KXHETO36
C charmed baryons                                                       KXHETO37
C                                                                       KXHETO38
         IF (K.GE.148.AND.K.LE.162) TLIF = TCHAB                        KXHETO39
         IF (K.GE.183.AND.K.LE.197) TLIF = TCHAB                        KXHETO40
C                                                                       KXHETO41
C beauty mesons                                                         KXHETO42
C                                                                       KXHETO43
         IF (K.GE.221.AND.K.LE.223) TLIF = TBMES                        KXHETO44
         IF (K.GE.230.AND.K.LE.231) TLIF = TBMES                        KXHETO45
         IF (K.GE.245.AND.K.LE.247) TLIF = TBMES                        KXHETO46
         IF (K.EQ.254) TLIF = TBMES                                     KXHETO47
C                                                                       KXHETO48
C beauty baryons                                                        KXHETO49
C                                                                       KXHETO50
         IF (K.GE.224.AND.K.LE.229) TLIF = TBBAR                        KXHETO51
         IF (K.GE.248.AND.K.LE.253) TLIF = TBBAR                        KXHETO52
                                                                        KXHETO53
 999    RETURN                                                          KXHETO54
        END                                                             KXHETO55
      SUBROUTINE KXHEAL(VMAIN,ISTATU,MVX,MTRK)                          KXHEAL 2
C ---------------------------------------------------------             KXHEAL 3
C - B.Bloch-Devaux - J.Boucrot - F.Ranjard - 870516                     KXHEAL 4
C - arranged for HERWIG  E. Lange  891801                               KXHEAL 5
C   B.Bloch :modified to allow for change from Energy to mass in        KXHEAL 6
C   KINE banks (April 89)                                               KXHEAL 7
C - A.S.Thompson :modified February 1992 for quark radiation in HERWIG  KXHEAL 8
C - A.S.Thompson :modified August 1992 to handle partons at the end of  KXHEAL 9
C                                      the shower                       KXHEAL10
C - A.S.Thompson :modified March 1993 for slight change in status codes KXHEAL11
C - A.S.Thompson :modified December 1993 for additional processes       KXHEAL12
C - Fill    : PTRAK(ix,n)  = px,py,pz,E( or Mass from Alephlib 9.0) of  KXHEAL13
C                            track(n)                                   KXHEAL14
C                            if E or M=0.it will be filled by the systemKXHEAL15
C             IPVNU(1,n)   = origin vertex # of track(n)                KXHEAL16
C                  (2,n)   = decay vertex # of track(n)                 KXHEAL17
C                             0 if no decay                             KXHEAL18
C                  (3,n)   = ALEPH particle #                           KXHEAL19
C             IPCOD(n)     = status and history code of track(n)        KXHEAL20
C                          = KS*1000000 + KH                            KXHEAL21
C                            KS = HERWIG status code                    KXHEAL22
C                            KH = history code                          KXHEAL23
C                                 for particles = mother track imoth    KXHEAL24
C                                 for clusters 1000*imoth1+imoth2       KXHEAL25
C - Book    : KHIS bank filled with IPCOD(n)                            KXHEAL26
C - Call    : KFEVBK (VMAIN,PTRAK,IPVNU,MTRK,JSTAT)                     KXHEAL27
C             to book propagate the decay and fill VERT and KINE        KXHEAL28
C                                                                       KXHEAL29
C - structure: SUBROUTINE subprogram                                    KXHEAL30
C              User Entry Name: KXHEAL                                  KXHEAL31
C              External References: ALTABL/ALVERS/KFEVBK(ALEPHLIB)      KXHEAL32
C              Comdecks referenced: BCS, HETRCK                         KXHEAL33
C                                                                       KXHEAL34
C - usage   : CALL KXHEAL (VMAIN,ISTATU,MVX,MTRK)                       KXHEAL35
C - Input   : VMAIN = vx,vy,vz,tof of the primary vertex                KXHEAL36
C - Output  : ISTATU = status word ( = 0 means OK)                      KXHEAL37
C                     - 2 means not enough space for VERT or KINE       KXHEAL38
C                     - 5 means interface error                         KXHEAL39
C                     > 0 means unknown HERWIG particle# ISTATU         KXHEAL40
C             MVX   = # of vertices                                     KXHEAL41
C             MTRK  = # of tracks to be propagated ( no beam electrons )KXHEAL42
C                                                                       KXHEAL43
C ------------------------------------------------------                KXHEAL44
C                                                                       KXHEAL45
      IMPLICIT NONE                                                     KXHEAL46
C                                                                       KXHEAL47
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(1000)                                                     BCS    6
      COMMON /BCS/   IW(1000)                                           BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      DOUBLE PRECISION                                                  HWDEFI 2
     & PHEP,VHEP,PBEAM1,PBEAM2,QCDLAM,VGCUT,VQCUT,VPCUT,BETAF,CAFAC,    HWDEFI 3
     & CFFAC,CLMAX,CLPOW,PSPLT,QSPAC,PTRMS,PXRMS,QG,QV,SWEIN,SCABI,     HWDEFI 4
     & PDIQK,QDIQK,ENSOF,TMTOP,ZBINM,GAMW,GAMZ,GAMH,PGSMX,PGSPL,PPAR,   HWDEFI 5
     & VPAR,PHIPAR,DECPAR,RHOPAR,RHOHEP,XFACT,PTINT,EVWGT,AVWGT,WGTMAX, HWDEFI 6
     & WGTSUM,WSQSUM,WBIGST,TLOUT,YJMIN,YJMAX,PTMIN,PTMAX,PTPOW,EMMIN,  HWDEFI 7
     & EMMAX,EMPOW,Q2MIN,Q2MAX,Q2POW,THMAX,QLIM,XXMIN,XLMIN,EMSCA,      HWDEFI 8
     & EMLST,COSTH,GPOLN,GCOEF,XX,DISF,RESN,RMIN,CTMAX,FBTM,            HWDEFI 9
     & FTOP,FHVY,RMASS,BFRAC,CMMOM,ACCUR,QEV,SUD,VECWT,TENWT,DECWT,     HWDEFI10
     & QWT,PWT,SWT,SWTEF,RESWT,PIFAC,QCDL3,QCDL5,BRHIG,GAMMAX,          HWDEFI11
     & ENHANC,B1LIM,ALPFAC,Q2WWMN,Q2WWMX,BTCLM,ETAMIX,                  HWDEFI12
     & QFCH,VFCH,AFCH,VCKM,TQWT,CLQ,EPOLN,PPOLN,COSS,SINS,GAMZP,GEV2NB, HWDEFI13
     & ALPHEM,PRSOF,EBEAM1,EBEAM2,ZJMAX,YBMIN,YBMAX,HARDST,CLSMR,PHOMAS HWDEFI14
C                                                                       HWDEFI15
      INTEGER NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP,            HWDEFI16
     & IPROC,MAXEV,IPRINT,LRSUD,LWSUD,NCOLO,NFLAV,MODPDF(2),NSTRU,      HWDEFI17
     & NZBIN,NBTRY,NCTRY,NDTRY,NETRY,NSTRY,NGSPL,NMXPAR,NEVPAR,         HWDEFI18
     & NPAR,ISTPAR,IDPAR,JMOPAR,JDAPAR,JCOPAR,INHAD,JNHAD,NSPAC,NRN,    HWDEFI19
     & MAXER,MAXPR,LWEVT,ISTAT,IERROR,NWGTS,IDHW,IBSH,IBRN,IPRO,        HWDEFI20
     & IFLMIN,IFLMAX,MAXFL,IDCMF,IHPRO,IDN,ICO,LOCN,                    HWDEFI21
     & NRES,IDPDG,ICHRG,MADDR,MODES,MODEF,IDPRO,INTER,NQEV,             HWDEFI22
     & NSUD,NMXSUD,MODBOS,IOPHIG,MODMAX,SUDORD,CLDIR,NUMER,NUMERU,      HWDEFI23
     & MAPQ,IPART1,IPART2,ISPAC,IAPHIG                                  HWDEFI24
C                                                                       HWDEFI25
      LOGICAL AZSOFT,AZSPIN,FROST,GENEV,BGSHAT,NOWGT,TMPAR,PRNDEC,      HWDEFI26
     & HVFCEN,FSTEVT,FSTWGT,BREIT,USECMF,ZPRIME,TPOL,GENSOF,            HWDEFI27
     & HARDME,SOFTME,NOSPAC                                             HWDEFI28
C                                                                       HWDEFI29
      CHARACTER*4 PART1,PART2,RNAME,BDECAY                              HWDEFI30
      CHARACTER*20 AUTPDF(2)                                            HWDEFI31
C                                                                       HWDEFI32
      PARAMETER (NMXHEP=2000)                                           HWDEFI33
      INTEGER NMXJET                                                    HWDEFI34
      PARAMETER (NMXJET=200)                                            HWDEFI35
C                                                                       HWDEFI36
      REAL TLIMI                                                        HETRCK 2
C                                                                       HETRCK 3
      PARAMETER(TLIMI = 1.E-15)                                         HETRCK 4
C                                                                       HETRCK 5
      COMMON/HWEVNT/EVWGT,AVWGT,WGTMAX,WGTSUM,WSQSUM,WBIGST,TLOUT,      HWEVNT 2
     & NRN(2),MAXER,NUMER,NUMERU,MAXPR,LWEVT,ISTAT,IERROR,NOWGT,NWGTS,  HWEVNT 3
     & IDHW(NMXHEP),GENSOF                                              HWEVNT 4
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),           HEPEVT 2
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)  HEPEVT 3
C                                                                       KXHEAL53
      REAL PTRAK(4,NMXHEP),VMAIN(4)                                     KXHEAL54
      REAL TLIF                                                         KXHEAL55
C                                                                       KXHEAL56
      INTEGER IPVNU(3,NMXHEP),IPCOD(NMXHEP)                             KXHEAL57
      INTEGER IPVAL(2,NMXHEP),NEWTR(NMXHEP)                             KXHEAL58
      INTEGER ALTABL                                                    KXHEAL59
      INTEGER JP1,JP2,I,IT,MVX,MTRK                                     KXHEAL60
      INTEGER IST,NPARL,INOTR,NVER,ITRK,ITR,IHER,IPART,KS               KXHEAL61
      INTEGER ISTATU,IMOTH,IMOTH2,IGRAND,KSMOTH,KSGRAN,IFAIL,JKHIS      KXHEAL62
      INTEGER KGPART,KBKINE,NAMIND                                      KXHEAL63
C                                                                       KXHEAL64
      INTEGER ID,NRBOS,L                                                BMACRO 2
      INTEGER LCOLS,LROWS,KNEXT,KROW,LFRWRD,LFRROW,ITABL                BMACRO 3
      REAL RTABL                                                        BMACRO 4
C                                                                       BMACRO 5
      INTEGER NAPART,JVK,JPA,NLINK                                      BMACRO 6
      INTEGER KNEXVK,LFRVK,KPARVK,KLISVK,NOFVK,KINTYP,INPTRK,           BMACRO 7
     &        INPVRT,MOTHVK                                             BMACRO 8
      REAL CHARGE,PARMAS,TIMLIF,PMODVK,ENERVK,TOFLIT,RADVK,PMASVK       BMACRO 9
C - # of words/row in bank with index ID                                BMACRO10
      LCOLS(ID) = IW(ID+1)                                              BMACRO11
C - # of rows in bank with index ID                                     BMACRO12
      LROWS(ID) = IW(ID+2)                                              BMACRO13
C - index of next row in the bank with index ID                         BMACRO14
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO15
C - index of row # NRBOS in the bank with index ID                      BMACRO16
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO17
C - # of free words in the bank with index ID                           BMACRO18
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO19
C - # of free rows in the bank with index ID                            BMACRO20
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO21
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO22
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO23
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO24
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO25
C                                                                       BMACRO26
C - index of the next vertex/track to be stored in KINE/VERT            KMACRO 2
C   bank known by its index JVK                                         KMACRO 3
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)                 KMACRO 4
C - # of vertices/tracks which could be stored in KINE/VERT             KMACRO 5
C   bank known by its index JVK                                         KMACRO 6
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))           KMACRO 7
C - index of the 1st parameter of KINE/VERT bank known by its           KMACRO 8
C   index JVK                                                           KMACRO 9
      KPARVK(JVK) = JVK + IW(JVK+1)                                     KMACRO10
C - index of 1st vertex/track # contained into the list of              KMACRO11
C   bank KINE/VERT known by its index JVK                               KMACRO12
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)                         KMACRO13
C - charge of ALEPH particle# JPA                                       KMACRO14
      CHARGE(JPA) = RTABL(IW(NAPART),JPA,7)                             KMACRO15
C - mass of ALEPH particle# JPA                                         KMACRO16
      PARMAS(JPA) = RTABL(IW(NAPART),JPA,6)                             KMACRO17
C - time of life of ALEPH particle# JPA                                 KMACRO18
      TIMLIF(JPA) = RTABL(IW(NAPART),JPA,8)                             KMACRO19
C - # of vertices on a track known by its BOS index /                   KMACRO20
C   # of outgoing tracks of a vertex known by its BOS index             KMACRO21
      NOFVK(JVK)  = IW(JVK+3)                                           KMACRO22
C - Particle type of a track known by its BOS index                     KMACRO23
      KINTYP(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO24
C - incoming track # of a vertex known by its BOS index                 KMACRO25
      INPTRK(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO26
C - origin vertex # of a track known by its BOS index                   KMACRO27
      INPVRT(JVK) = IW(KLISVK(JVK)+1)                                   KMACRO28
C - momentum of a track known by its BOS index                          KMACRO29
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2   KMACRO30
     &                   +RW(KPARVK(JVK)+3)**2)                         KMACRO31
C - MASS of a track known by its BOS index  JVK                         KMACRO32
      PMASVK(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO33
C - energy of a track known by its BOS index JVK                        KMACRO34
      ENERVK(JVK) = SQRT(PMODVK(JVK)**2+PMASVK(JVK)**2)                 KMACRO35
C - time of flight of the icoming particle to the vertex known by its   KMACRO36
C   BOS index JVK                                                       KMACRO37
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO38
C - radius of the vertex known by its BOS index                         KMACRO39
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)  KMACRO40
C - mother track # of a track known by its BOS index                    KMACRO41
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))                  KMACRO42
C                                                                       KMACRO43
C                                                                       KXHEAL67
      DATA NAPART/0/                                                    KXHEAL68
C                                                                       KXHEAL69
C initialization                                                        KXHEAL70
C                                                                       KXHEAL71
      JP1 = 0                                                           KXHEAL72
      JP2 = 0                                                           KXHEAL73
      IST = 0                                                           KXHEAL74
C                                                                       KXHEAL75
      IF (NAPART.EQ.0) NAPART = NAMIND('PART')                          KXHEAL76
C                                                                       KXHEAL77
C Build array containing vertex # and particle # of each track          KXHEAL78
C                                                                       KXHEAL79
      NPARL = 0                                                         KXHEAL80
      INOTR = 0                                                         KXHEAL81
      NVER = 1                                                          KXHEAL82
C                                                                       KXHEAL83
      DO 10 ITRK = 1,NHEP                                               KXHEAL84
C                                                                       KXHEAL85
        ITR = ITRK - INOTR                                              KXHEAL86
C                                                                       KXHEAL87
C get ALEPH particle#                                                   KXHEAL88
C                                                                       KXHEAL89
        IHER = IDHW(ITRK)                                               KXHEAL90
        IPART = KGPART(IHER)                                            KXHEAL91
C                                                                       KXHEAL92
        IF (IPART.LE.0) GOTO 998                                        KXHEAL93
C                                                                       KXHEAL94
C store momenta                                                         KXHEAL95
C                                                                       KXHEAL96
        DO 9 I = 1,3                                                    KXHEAL97
          PTRAK(I,ITR) = SNGL(PHEP(I,ITRK))                             KXHEAL98
   9    CONTINUE                                                        KXHEAL99
C                                                                       KXHEA100
C KBKINE needs the mass of the particles instead of the energy          KXHEA101
C                                                                       KXHEA102
        PTRAK(4,ITR) = SNGL(PHEP(5,ITRK))                               KXHEA103
C                                                                       KXHEA104
C store new line number                                                 KXHEA105
C                                                                       KXHEA106
        NEWTR(ITRK) = ITR                                               KXHEA107
C                                                                       KXHEA108
        IPVNU(3,ITR) = IPART                                            KXHEA109
        KS = ISTHEP(ITRK)                                               KXHEA110
        IMOTH = JMOHEP(1,ITRK)                                          KXHEA111
        IF(IMOTH.GT.0) THEN                                             KXHEA112
          KSMOTH=ISTHEP(IMOTH)                                          KXHEA113
          IGRAND=JMOHEP(1,IMOTH)                                        KXHEA114
          IF(IGRAND.GT.0) KSGRAN=ISTHEP(IGRAND)                         KXHEA115
        ENDIF                                                           KXHEA116
C                                                                       KXHEA117
C now we have look for different status codes to decide what            KXHEA118
C we want to do                                                         KXHEA119
C just in case this is the mother of an initial state track             KXHEA120
           IPVAL(2,ITRK)=1                                              KXHEA121
C allow virtual photons from 2-photon events                            KXHEA122
      IF(KS.EQ.3.OR.KS.EQ.100.OR.KS.EQ.103.OR.KS.EQ.121.OR.             KXHEA123
     &   KS.EQ.122) THEN                                                KXHEA124
C special HERWIG objects - also virtual photons from 2-photon events    KXHEA125
C we just skip them                                                     KXHEA126
C                                                                       KXHEA127
          INOTR = INOTR + 1                                             KXHEA128
C                                                                       KXHEA129
        ELSEIF (KS.EQ.101.OR.KS.EQ.102) THEN                            KXHEA130
C                                                                       KXHEA131
C beam electrons are stored with negative numbers in the KINE bank      KXHEA132
C                                                                       KXHEA133
          IST = KBKINE(-ITRK,PTRAK(1,ITR),IPART,0)                      KXHEA134
C                                                                       KXHEA135
          IF (IST.LE.0) THEN                                            KXHEA136
            IHER = -2                                                   KXHEA137
            GOTO 998                                                    KXHEA138
          ENDIF                                                         KXHEA139
C                                                                       KXHEA140
          INOTR = INOTR + 1                                             KXHEA141
C just in case this is the mother of an initial state track             KXHEA142
           IPVAL(2,ITRK)=1                                              KXHEA143
C                                                                       KXHEA144
        ELSEIF (KS.EQ.120) THEN                                         KXHEA145
C                                                                       KXHEA146
C this should be the Z0/gamma or W+,W-                                  KXHEA147
C                                                                       KXHEA148
C could also be the CMF in 2-photon events                              KXHEA149
        if((iher.lt.198.or.iher.gt.200).and.                            KXHEA150
     &     (iher.ne.14.and.iher.ne.15)) then                            KXHEA151
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ found illegal exchange''  KXHEA152
     &         ,'' boson : HERWIG particle# = '',I5,'' in event ''      KXHEA153
     &         ,I6)')IHER,NEVHEP                                        KXHEA154
            ISTATU= -5                                                  KXHEA155
            GOTO 999                                                    KXHEA156
          ENDIF                                                         KXHEA157
C                                                                       KXHEA158
C create primary vertex                                                 KXHEA159
C                                                                       KXHEA160
          IPVNU(1,ITR) = 1                                              KXHEA161
          IPVNU(2,ITR) = IPVNU(1,ITR)                                   KXHEA162
C                                                                       KXHEA163
          IPVAL(1,ITRK) = IPVNU(1,ITR)                                  KXHEA164
          IPVAL(2,ITRK) = IPVNU(2,ITR)                                  KXHEA165
C                                                                       KXHEA166
C define new track number                                               KXHEA167
C and store in KINE bank                                                KXHEA168
C                                                                       KXHEA169
          NEWTR(ITRK) = ITRK - INOTR                                    KXHEA170
          IPCOD(ITR) = 0                                                KXHEA171
C                                                                       KXHEA172
          NPARL = NPARL + 1                                             KXHEA173
C                                                                       KXHEA174
        ELSEIF (KS.GE.123.AND.KS.LE.124) THEN                           KXHEA175
C                                                                       KXHEA176
C outgoing partons will be linked to their mother                       KXHEA177
C                                                                       KXHEA178
C they should come from the exchange boson or a heavy quark before decayKXHEA179
C                                                                       KXHEA180
C handle the initial outgoing partons from ZH and nunuH events          KXHEA181
        IF(KSMOTH.NE.120.AND.KSMOTH.NE.155.AND.                         KXHEA182
     &    (KSMOTH.NE.103.OR.IDHW(IMOTH).NE.14).AND.(KSMOTH.NE.195.OR.   KXHEA183
     &    (IDHW(IMOTH).LT.198.OR.IDHW(IMOTH).GT.201))) THEN             KXHEA184
            WRITE (IW(6),'(/1X,                                         KXHEA185
     &      ''+++KXHEAL+++ outgoing parton, ISTHEP = '',                KXHEA186
     &         I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')   KXHEA187
     &         KS,KSMOTH,NEVHEP                                         KXHEA188
            ISTATU = -5                                                 KXHEA189
            GOTO 999                                                    KXHEA190
          ENDIF                                                         KXHEA191
C                                                                       KXHEA192
          IPVNU(1,ITR) = IPVAL(2,IMOTH)                                 KXHEA193
          IPVNU(2,ITR) = IPVNU(1,ITR)                                   KXHEA194
C                                                                       KXHEA195
          IPVAL(1,ITRK) = IPVNU(1,ITR)                                  KXHEA196
          IPVAL(2,ITRK) = IPVNU(2,ITR)                                  KXHEA197
C                                                                       KXHEA198
C we don't keep them in the KINE bank                                   KXHEA199
C                                                                       KXHEA200
            INOTR = INOTR + 1                                           KXHEA201
C                                                                       KXHEA202
        ELSEIF(KS.EQ.125) THEN                                          KXHEA203
C                                                                       KXHEA204
C spectator parton after processing                                     KXHEA205
C it should come from a decayed heavy flavour hadron                    KXHEA206
C                                                                       KXHEA207
          IF (KSMOTH.NE.199) THEN                                       KXHEA208
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ spectator parton, ''      KXHEA209
     &         ,''ISTHEP = ''                                           KXHEA210
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')  KXHEA211
     &         KS,KSMOTH,NEVHEP                                         KXHEA212
            ISTATU = -5                                                 KXHEA213
            GOTO 999                                                    KXHEA214
          ENDIF                                                         KXHEA215
C                                                                       KXHEA216
          IPVNU(1,ITR) = IPVAL(2,IMOTH)                                 KXHEA217
          IPVNU(2,ITR) = IPVNU(1,ITR)                                   KXHEA218
C                                                                       KXHEA219
          IPVAL(1,ITRK) = IPVNU(1,ITR)                                  KXHEA220
          IPVAL(2,ITRK) = IPVNU(2,ITR)                                  KXHEA221
C                                                                       KXHEA222
C  we don't want to track it                                            KXHEA223
C                                                                       KXHEA224
          INOTR = INOTR + 1                                             KXHEA225
C                                                                       KXHEA226
        ELSEIF (KS.EQ.143.OR.KS.EQ.144) THEN                            KXHEA227
C                                                                       KXHEA228
C outgoing parton jets                                                  KXHEA229
C they should come from their outgoing partons                          KXHEA230
C                                                                       KXHEA231
          IF (KSMOTH+20.NE.KS) THEN                                     KXHEA232
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = ''          KXHEA233
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')  KXHEA234
     &         KS,KSMOTH,NEVHEP                                         KXHEA235
            ISTATU = -5                                                 KXHEA236
            GOTO 999                                                    KXHEA237
          ENDIF                                                         KXHEA238
C                                                                       KXHEA239
C and the grand should be the Z0/gamma or from a heavy quark            KXHEA240
C before decay                                                          KXHEA241
C                                                                       KXHEA242
C basically taus from ZZ,ZH events                                      KXHEA243
          IF(KSGRAN.NE.120.AND.KSGRAN.NE.155.AND.KSGRAN.NE.195) THEN    KXHEA244
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = '',I3,      KXHEA245
     &      '' has grandma, ISTHEP =  '',I3,'' in event '',I6)')        KXHEA246
     &      KS,KSGRAN,NEVHEP                                            KXHEA247
            ISTATU = -5                                                 KXHEA248
            GOTO 999                                                    KXHEA249
          ENDIF                                                         KXHEA250
C                                                                       KXHEA251
C this must not be any particle which might be tracked by GEANT         KXHEA252
C                                                                       KXHEA253
          IF (IHER.GT.13) THEN                                          KXHEA254
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = '',I3,      KXHEA255
     &      '' is not a quark in event '',I6)') KS,NEVHEP               KXHEA256
            ISTATU = -5                                                 KXHEA257
            GOTO 999                                                    KXHEA258
          ENDIF                                                         KXHEA259
C                                                                       KXHEA260
          IPVNU(1,ITR) = IPVAL(2,IMOTH)                                 KXHEA261
          IPVNU(2,ITR) = IPVNU(1,ITR)                                   KXHEA262
C                                                                       KXHEA263
          IPVAL(1,ITRK) = IPVNU(1,ITR)                                  KXHEA264
          IPVAL(2,ITRK) = IPVNU(2,ITR)                                  KXHEA265
C                                                                       KXHEA266
C store them in the KINE bank                                           KXHEA267
C KS = 123,124 is not stored in KINE, we have to link to the grands     KXHEA268
C                                                                       KXHEA269
          IPCOD(ITR) = NEWTR(IGRAND)                                    KXHEA270
          NPARL = NPARL + 1                                             KXHEA271
C                                                                       KXHEA272
        ELSEIF(KS.EQ.155) THEN                                          KXHEA273
C                                                                       KXHEA274
C heavy quark or tau before decay                                       KXHEA275
C                                                                       KXHEA276
          IF (.NOT.(IHER.LE.12.OR.IHER.EQ.125.OR.IHER.EQ.131)) THEN     KXHEA277
            WRITE(IW(6),'(/1X,''+++KXHEAL+++ heavy quark or tau ''      KXHEA278
     &                 ,'' before decay has IHER = '',I3                KXHEA279
     &                 ,'' in event '',I6)')IHER,NEVHEP                 KXHEA280
            ISTATU = -5                                                 KXHEA281
            GOTO 999                                                    KXHEA282
          ENDIF                                                         KXHEA283
C                                                                       KXHEA284
          IF (KSMOTH.NE.199.AND.KSMOTH.NE.124.AND.KSMOTH.NE.123) THEN   KXHEA285
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ part. ISTHEP = ''         KXHEA286
     &         ,I3,'' has mother, ISTHEP =  ''                          KXHEA287
     &         ,I3,'' in event '',I6)')KS,KSMOTH,NEVHEP                 KXHEA288
            ISTATU = -5                                                 KXHEA289
            GOTO 999                                                    KXHEA290
          ENDIF                                                         KXHEA291
C                                                                       KXHEA292
          IPVNU(1,ITR) = IPVAL(2,IMOTH)                                 KXHEA293
          IPVNU(2,ITR) = IPVNU(1,ITR)                                   KXHEA294
C                                                                       KXHEA295
          IPVAL(1,ITRK) = IPVNU(1,ITR)                                  KXHEA296
          IPVAL(2,ITRK) = IPVNU(2,ITR)                                  KXHEA297
C                                                                       KXHEA298
          NPARL = NPARL + 1                                             KXHEA299
C                                                                       KXHEA300
C update history code                                                   KXHEA301
C                                                                       KXHEA302
          IF (KSMOTH.EQ.199) THEN                                       KXHEA303
C                                                                       KXHEA304
            IPCOD(ITR) = NEWTR(IMOTH)                                   KXHEA305
C                                                                       KXHEA306
          ELSE                                                          KXHEA307
C                                                                       KXHEA308
C KS = 123,124 is not stored in KINE, we have to link to the grands     KXHEA309
C                                                                       KXHEA310
            IPCOD(ITR) = NEWTR(IGRAND)                                  KXHEA311
C                                                                       KXHEA312
          ENDIF                                                         KXHEA313
C                                                                       KXHEA314
        elseif((ks.ge.157.and.ks.le.159).or.ks.eq.149) then             KXHEA315
C                                                                       KXHEA316
          IF (IHER.GT.13.AND.IHER.NE.59) THEN                           KXHEA317
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ particle from splitting'' KXHEA318
     &      ,'' is not a parton IHER= '',I3,'' KS= '',I3                KXHEA319
     &      ,'' in event '',I7)') IHER,KS,NEVHEP                        KXHEA320
            ISTATU = -5                                                 KXHEA321
            GOTO 999                                                    KXHEA322
          ENDIF                                                         KXHEA323
C                                                                       KXHEA324
          IF (KS.EQ.157.AND.KSMOTH.NE.143.AND.KSMOTH.NE.144) THEN       KXHEA325
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ parton from ''            KXHEA326
     &         ,'' QCD branching not from parton jet ''                 KXHEA327
     &         ,'' in event '',I6)')NEVHEP                              KXHEA328
            ISTATU = -5                                                 KXHEA329
            GOTO 999                                                    KXHEA330
          ENDIF                                                         KXHEA331
C                                                                       KXHEA332
          IF (KS.EQ.159.AND.KSMOTH.NE.143.AND.                          KXHEA333
     &                      KSMOTH.NE.144) THEN                         KXHEA334
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ parton from ''            KXHEA335
     &         ,'' cluster splitting not from parton jet ''             KXHEA336
     &         ,'' in event '',I6)')NEVHEP                              KXHEA337
            ISTATU = -5                                                 KXHEA338
            GOTO 999                                                    KXHEA339
          ENDIF                                                         KXHEA340
C AST ignore FSR photons appearing here                                 KXHEA341
          IF(IHER.EQ.59) THEN                                           KXHEA342
            INOTR=INOTR+1                                               KXHEA343
          else                                                          KXHEA344
            IPVNU(1,ITR) = IPVAL(2,IMOTH)                               KXHEA345
            IPVNU(2,ITR) = IPVNU(1,ITR)                                 KXHEA346
C                                                                       KXHEA347
            IPVAL(1,ITRK) = IPVNU(1,ITR)                                KXHEA348
            IPVAL(2,ITRK) = IPVNU(2,ITR)                                KXHEA349
C                                                                       KXHEA350
            NPARL = NPARL + 1                                           KXHEA351
C                                                                       KXHEA352
C update history code                                                   KXHEA353
C                                                                       KXHEA354
            IPCOD(ITR) = NEWTR(IMOTH)                                   KXHEA355
          ENDIF                                                         KXHEA356
C                                                                       KXHEA357
        ELSEIF (KS.EQ.160) THEN                                         KXHEA358
C                                                                       KXHEA359
C spectator after heavy decay, just link                                KXHEA360
C                                                                       KXHEA361
          IF (.NOT.(IHER.LE.12.OR.(IHER.GE.109.AND.IHER.LE.120))) THEN  KXHEA362
            WRITE(IW(6),'(/1X,''+++KXHEAL+++ spectator after ''         KXHEA363
     &         ,''heavy decay is not a (di)quark, IHER = '',I3          KXHEA364
     &         ,'' in event '',I6)')IHER,NEVHEP                         KXHEA365
            ISTATU = -5                                                 KXHEA366
            GOTO 999                                                    KXHEA367
          ENDIF                                                         KXHEA368
C                                                                       KXHEA369
C it should come from a spectator parton                                KXHEA370
C                                                                       KXHEA371
          IF (KSMOTH.NE.125) THEN                                       KXHEA372
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ spectator after ''        KXHEA373
     &         ,''heavy decay, ISTHEP = ''                              KXHEA374
     &         ,I3,'' has mother, ISTHEP = '',I3,'' in event '',I6)')   KXHEA375
     &         KS,KSMOTH,NEVHEP                                         KXHEA376
            ISTATU = -5                                                 KXHEA377
            GOTO 999                                                    KXHEA378
          ENDIF                                                         KXHEA379
C                                                                       KXHEA380
          IPVNU(1,ITR) = IPVAL(2,IMOTH)                                 KXHEA381
          IPVNU(2,ITR) = IPVNU(1,ITR)                                   KXHEA382
C                                                                       KXHEA383
          IPVAL(1,ITRK) = IPVNU(1,ITR)                                  KXHEA384
          IPVAL(2,ITRK) = IPVNU(2,ITR)                                  KXHEA385
C                                                                       KXHEA386
          NPARL =  NPARL + 1                                            KXHEA387
C                                                                       KXHEA388
C update history code                                                   KXHEA389
C                                                                       KXHEA390
          IF (KSGRAN.NE.199) THEN                                       KXHEA391
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ quark, ISTHEP = ''        KXHEA392
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')  KXHEA393
     &         KS,KSGRAN,NEVHEP                                         KXHEA394
            ISTATU = -5                                                 KXHEA395
            GOTO 999                                                    KXHEA396
          ENDIF                                                         KXHEA397
C                                                                       KXHEA398
          IPCOD(ITR) = NEWTR(IGRAND)                                    KXHEA399
C                                                                       KXHEA400
        ELSEIF (KS.EQ.183) THEN                                         KXHEA401
C                                                                       KXHEA402
C this is a hard cluster                                                KXHEA403
C find it's partons first                                               KXHEA404
C                                                                       KXHEA405
          IMOTH2 = JMOHEP(2,ITRK)                                       KXHEA406
C                                                                       KXHEA407
C we check that the partons are coming from the same vertex             KXHEA408
C                                                                       KXHEA409
          IF (IPVAL(2,IMOTH).NE.IPVAL(2,IMOTH2)) THEN                   KXHEA410
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ cluster particles''       KXHEA411
     &         ,'' ITRK = '',I4,I4,'' of cluster ITRK = '',I4           KXHEA412
     &         ,'' do not come from the same vertex in event ''         KXHEA413
     &         ,I6)')JP1,JP2,ITRK,NEVHEP                                KXHEA414
            ISTATU = -5                                                 KXHEA415
            GOTO 999                                                    KXHEA416
          ENDIF                                                         KXHEA417
C                                                                       KXHEA418
          IPVNU(1,ITR) = IPVAL(2,IMOTH)                                 KXHEA419
          IPVNU(2,ITR) = IPVNU(1,ITR)                                   KXHEA420
C                                                                       KXHEA421
          IPVAL(1,ITRK) = IPVNU(1,ITR)                                  KXHEA422
          IPVAL(2,ITRK) = IPVNU(2,ITR)                                  KXHEA423
C                                                                       KXHEA424
          NPARL = NPARL + 1                                             KXHEA425
C                                                                       KXHEA426
C update history, we may have two different mothers                     KXHEA427
C                                                                       KXHEA428
          IPCOD(ITR) = NEWTR(IMOTH)*1000 + NEWTR(IMOTH2)                KXHEA429
C                                                                       KXHEA430
        ELSEIF (KS.GE.195.AND.KS.LE.199) THEN                           KXHEA431
C                                                                       KXHEA432
C these are unstable leptons and hadrons                                KXHEA433
C link them, propagate if necessary                                     KXHEA434
C                                                                       KXHEA435
C direct unstable hadrons and decayed heavy flavour hadrons             KXHEA436
C should come from the cluster                                          KXHEA437
C                                                                       KXHEA438
          IF ((KS.EQ.196.OR.KS.EQ.197.OR.KS.EQ.199)                     KXHEA439
     &       .AND.KSMOTH.NE.183) THEN                                   KXHEA440
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ unstab. hadr. or ''       KXHEA441
     &         ,''dec. heavy flav. hadr. not from clus. ''              KXHEA442
     &         ,'' in event '',I6)')NEVHEP                              KXHEA443
            ISTATU = -5                                                 KXHEA444
            GOTO 999                                                    KXHEA445
          ENDIF                                                         KXHEA446
C                                                                       KXHEA447
C indirect unstable hadrons or leptons should come from direct          KXHEA448
C or indirect unstable hadrons or direct unstable leptons               KXHEA449
C                                                                       KXHEA450
          IF (KS.EQ.198.AND.KSMOTH.LT.195.OR.KSMOTH.GT.198) THEN        KXHEA451
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ indir unstab. hadr. ''    KXHEA452
     &         ,'' or lept. not from dir. unstab. hadr.''               KXHEA453
     &         ,'' in event '',I6)')NEVHEP                              KXHEA454
            ISTATU = -5                                                 KXHEA455
            GOTO 999                                                    KXHEA456
          ENDIF                                                         KXHEA457
C                                                                       KXHEA458
C direct unstable leptons should come from outgoing partons and their   KXHEA459
C grandma should be a heavy quark before decay                          KXHEA460
C AST allow Z,H from initial process as well                            KXHEA461
C                                                                       KXHEA462
          IF (KS.EQ.195) THEN                                           KXHEA463
            IF (KSMOTH.NE.123.AND.KSMOTH.NE.124.AND.KSMOTH.NE.120) THEN KXHEA464
              WRITE (IW(6),'(/1X,''+++KXHEAL+++ dir unstab. lept. '',   KXHEA465
     &         '' not from outgoing partons in event '',I6)')NEVHEP     KXHEA466
              ISTATU = -5                                               KXHEA467
              GOTO 999                                                  KXHEA468
            ENDIF                                                       KXHEA469
            IF (KSGRAN.NE.155.AND.KSGRAN.NE.120.AND.KSGRAN.NE.195.AND.  KXHEA470
C basically taus from ZZ,ZH events                                      KXHEA471
     &         (IHER.LT.198.OR.IHER.GT.201)) THEN                       KXHEA472
              WRITE (IW(6),'(/1X,''+++KXHEAL+++ dir unstab. lept. ''    KXHEA473
     &           ,'' not from heavy quark before decay''                KXHEA474
     &           ,'' in event '',I6)')NEVHEP                            KXHEA475
              ISTATU = -5                                               KXHEA476
              GOTO 999                                                  KXHEA477
            ENDIF                                                       KXHEA478
          ENDIF                                                         KXHEA479
C                                                                       KXHEA480
          IPVNU(1,ITR) = IPVAL(2,IMOTH)                                 KXHEA481
C                                                                       KXHEA482
          TLIF = TIMLIF(IPART)                                          KXHEA483
C                                                                       KXHEA484
          IF (TLIF.GT.TLIMI) THEN                                       KXHEA485
C                                                                       KXHEA486
C these particles will be propagated                                    KXHEA487
C                                                                       KXHEA488
            NVER = NVER + 1                                             KXHEA489
            IPVNU(2,ITR) = NVER                                         KXHEA490
C                                                                       KXHEA491
          ELSE                                                          KXHEA492
C                                                                       KXHEA493
C these particles will decay immediately                                KXHEA494
C link them to the main vertex                                          KXHEA495
C                                                                       KXHEA496
            IPVNU(2,ITR) = IPVNU(1,ITR)                                 KXHEA497
C                                                                       KXHEA498
          ENDIF                                                         KXHEA499
C                                                                       KXHEA500
          IPVAL(1,ITRK) = IPVNU(1,ITR)                                  KXHEA501
          IPVAL(2,ITRK) = IPVNU(2,ITR)                                  KXHEA502
C                                                                       KXHEA503
          NPARL = NPARL + 1                                             KXHEA504
C                                                                       KXHEA505
C update the history code                                               KXHEA506
C for the direct unstable leptons (tau's) we link directly              KXHEA507
C to the heavy quark before decay                                       KXHEA508
C                                                                       KXHEA509
          IF (KS.EQ.195) THEN                                           KXHEA510
            IPCOD(ITR) = NEWTR(IGRAND)                                  KXHEA511
          ELSE                                                          KXHEA512
            IPCOD(ITR) = NEWTR(IMOTH)                                   KXHEA513
          ENDIF                                                         KXHEA514
C                                                                       KXHEA515
        ELSEIF (KS.EQ.1) THEN                                           KXHEA516
C                                                                       KXHEA517
C final state particles, just link them                                 KXHEA518
C                                                                       KXHEA519
          IMOTH2= JMOHEP(2,ITRK)                                        KXHEA520
C AST                                                                   KXHEA521
          IF(IDHW(IMOTH2).LE.12.AND.IHER.EQ.59.AND.                     KXHEA522
     &     IDHW(IMOTH).EQ.59.AND.KSMOTH.EQ.157) THEN                    KXHEA523
C AST                                                                   KXHEA524
C final state photons . drop them since they have appeared in the       KXHEA525
C parton shower                                                         KXHEA526
            INOTR = INOTR + 1                                           KXHEA527
          else                                                          KXHEA528
C spectator e+e- from 2-photon events                                   KXHEA529
            if(isthep(imoth).eq.103.and.iher.eq.59) then                KXHEA530
C           if((isthep(imoth).eq.103.and.iher.eq.59).or.                KXHEA531
C    &        ((iher.eq.121.or.iher.eq.127.).and.                       KXHEA532
C    &        (isthep(imoth).eq.101.or.isthep(imoth).eq.102)))then      KXHEA533
C AST                                                                   KXHEA534
C initial state photons are linked to the main vertex                   KXHEA535
C                                                                       KXHEA536
              IPVNU(1,ITR) = 1                                          KXHEA537
C                                                                       KXHEA538
            ELSE                                                        KXHEA539
C                                                                       KXHEA540
C other particles are linked to the decay vertex of their mother        KXHEA541
C                                                                       KXHEA542
              IPVNU(1,ITR) = IPVAL(2,IMOTH)                             KXHEA543
C                                                                       KXHEA544
            ENDIF                                                       KXHEA545
C                                                                       KXHEA546
C no decay                                                              KXHEA547
C                                                                       KXHEA548
            IPVNU(2,ITR) = 0                                            KXHEA549
C                                                                       KXHEA550
            IPVAL(1,ITRK) = IPVNU(1,ITR)                                KXHEA551
            IPVAL(2,ITRK) = IPVNU(2,ITR)                                KXHEA552
C                                                                       KXHEA553
            NPARL = NPARL + 1                                           KXHEA554
C                                                                       KXHEA555
C update the history code                                               KXHEA556
C                                                                       KXHEA557
            IF(KSMOTH.EQ.123.OR.KSMOTH.EQ.124) THEN                     KXHEA558
C                                                                       KXHEA559
C leptons from heavy quarks                                             KXHEA560
C                                                                       KXHEA561
              IPCOD(ITR) = NEWTR(IGRAND)                                KXHEA562
C                                                                       KXHEA563
            ELSEIF (KSMOTH.EQ.103.AND.IHER.EQ.59) THEN                  KXHEA564
C                                                                       KXHEA565
C initial state photons are initial particles                           KXHEA566
C                                                                       KXHEA567
              IPCOD(ITR) = 0                                            KXHEA568
C                                                                       KXHEA569
            ELSE                                                        KXHEA570
C                                                                       KXHEA571
              IPCOD(ITR) = NEWTR(IMOTH)                                 KXHEA572
C                                                                       KXHEA573
            ENDIF                                                       KXHEA574
          ENDIF                                                         KXHEA575
                                                                        KXHEA576
C                                                                       KXHEA577
        ELSEIF (KS.EQ.2) THEN                                           KXHEA578
C                                                                       KXHEA579
C AST parton before hadronization. Needed if work required at parton    KXHEA580
C level. This can also apply to partons from heavy quark decay so       KXHEA581
C give those partons whose greatgrandmother is the Z a 2nd mother       KXHEA582
C                                                                       KXHEA583
          IF(IHER.LE.13.OR.IHER.EQ.59) THEN                             KXHEA584
            IMOTH2= JMOHEP(2,ITRK)                                      KXHEA585
            IPCOD(ITR) = NEWTR(IMOTH)                                   KXHEA586
C                                                                       KXHEA587
            IF (ISTHEP(JMOHEP(1,IGRAND)).EQ.120)                        KXHEA588
     &       IPCOD(ITR)=IPCOD(ITR)+1000*NEWTR(IGRAND)                   KXHEA589
C                                                                       KXHEA590
C we link it to its mother                                              KXHEA591
C                                                                       KXHEA592
            IPVNU(1,ITR) = IPVAL(2,IMOTH)                               KXHEA593
            IPVNU(2,ITR) = IPVNU(1,ITR)                                 KXHEA594
            IF(IHER.EQ.59) IPVNU(2,ITR)=0                               KXHEA595
C                                                                       KXHEA596
            IPVAL(1,ITRK) = IPVNU(1,ITR)                                KXHEA597
            IPVAL(2,ITRK) = IPVNU(2,ITR)                                KXHEA598
C                                                                       KXHEA599
C  we don't want to track it                                            KXHEA600
C                                                                       KXHEA601
            NPARL = NPARL + 1                                           KXHEA602
C non-partons                                                           KXHEA603
          else                                                          KXHEA604
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ particle in parton ''     KXHEA605
     &      ,''shower is not a parton IHER= '',I3,'' KS= '',I3          KXHEA606
     &      ,'' in event '',I7)') iher,ks,nevhep                        KXHEA607
            ISTATU = -5                                                 KXHEA608
            GOTO 999                                                    KXHEA609
          endif                                                         KXHEA610
C                                                                       KXHEA611
        ELSE                                                            KXHEA612
C                                                                       KXHEA613
          WRITE (IW(6),'(/1X,''+++KXHEAL+++ non interfaced''            KXHEA614
     &         ,'' HERWIG status code found = '',I3,'' in event ''      KXHEA615
     &         ,I6)') KS,NEVHEP                                         KXHEA616
          ISTATU = -5                                                   KXHEA617
          GOTO 999                                                      KXHEA618
C                                                                       KXHEA619
        ENDIF                                                           KXHEA620
C                                                                       KXHEA621
C add status code                                                       KXHEA622
C                                                                       KXHEA623
        IPCOD(ITR) = IPCOD(ITR) + KS*1000000                            KXHEA624
C                                                                       KXHEA625
 10   CONTINUE                                                          KXHEA626
C                                                                       KXHEA627
C Propagate decays and fill KINE and VERT banks                         KXHEA628
C                                                                       KXHEA629
      CALL KFEVBK(VMAIN,PTRAK,IPVNU,NPARL,IFAIL)                        KXHEA630
C                                                                       KXHEA631
C Fill history bank KHIS                                                KXHEA632
C                                                                       KXHEA633
      JKHIS = ALTABL ('KHIS',1,NPARL,IPCOD,'I','E')                     KXHEA634
C                                                                       KXHEA635
      MVX = NVER                                                        KXHEA636
      MTRK = NPARL                                                      KXHEA637
      ISTATU = IFAIL                                                    KXHEA638
      GOTO 999                                                          KXHEA639
C                                                                       KXHEA640
C Error                                                                 KXHEA641
C                                                                       KXHEA642
 998  ISTATU = IHER                                                     KXHEA643
C                                                                       KXHEA644
 999  RETURN                                                            KXHEA645
      END                                                               KXHEA646
      SUBROUTINE UHRBK                                                  UHRBK  2
C --------------------------------------------------------------------  UHRBK  3
C Ebi Lange  February 1989.                                             UHRBK  4
C --------------------------------------------------------------------  UHRBK  5
C                                                                       UHRBK  6
      IMPLICIT NONE                                                     UHRBK  7
C                                                                       UHRBK  8
      INTEGER IHARD,IFL                                                 INIPRO 2
C                                                                       INIPRO 3
      COMMON /INIPRO/ IHARD,IFL                                         INIPRO 4
C                                                                       INIPRO 5
C                                                                       UHRBK 10
C book default histograms                                               UHRBK 11
C                                                                       UHRBK 12
      CALL HBOOK1(10010,'COS. POL. ANGLE OF QUARK',                     UHRBK 13
     &            50,-1.,1.,0.)                                         UHRBK 14
      CALL HBOOK1(10011,'COS. POL. ANGLE OF ANTIQUARK',                 UHRBK 15
     &            50,-1.,1.,0.)                                         UHRBK 16
      CALL HBOOK1(10020,'MULTIPLICITY',75,0.,150.,0.)                   UHRBK 17
      CALL HBOOK1(10021,'CHARGED MULTIPLICITY',50,0.,100.,0.)           UHRBK 18
      CALL HBOOK1(10030,'MOMENTUM SPECTRUM',100,0.,50.,0.)              UHRBK 19
C                                                                       UHRBK 20
      CALL HBOOK1(10100,'MOMENTA OF RAD. PHOTONS',                      UHRBK 21
     &              50,0.,50.,0.)                                       UHRBK 22
      CALL HBOOK1(10110,'COS. POL. ANGLE OF RAD. PHOTONS',              UHRBK 23
     &              50,-1.,1.,0.)                                       UHRBK 24
      CALL HBOOK1(10120,'Flavour depend. OF RAD. PHOTONS',              UHRBK 25
     &              10, 0.,10.,0.)                                      UHRBK 26
      CALL HBOOK1(10200,'MOMENTA OF INITIAL STATE PHOTONS',             UHRBK 27
     &          50,0.,50.,0.)                                           UHRBK 28
      CALL HBOOK1(10210,'COS POLAR ANGLE OF INITIAL STATE PHOTONS',     UHRBK 29
     &          50,-1.,1.,0.)                                           UHRBK 30
C                                                                       UHRBK 31
      RETURN                                                            UHRBK 32
C                                                                       UHRBK 33
      END                                                               UHRBK 34
      SUBROUTINE UHRFI                                                  UHRFI  2
C --------------------------------------------------------------------  UHRFI  3
C Ebi Lange  February 1989.                                             UHRFI  4
C A.S.Thompson Modified February 1992                                   UHRFI  5
C --------------------------------------------------------------------  UHRFI  6
C                                                                       UHRFI  7
C fill default histograms                                               UHRFI  8
C                                                                       UHRFI  9
      IMPLICIT NONE                                                     UHRFI 10
C                                                                       UHRFI 11
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      INTEGER IW                                                        BCS    5
      REAL RW(1000)                                                     BCS    6
      COMMON /BCS/   IW(1000)                                           BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      DOUBLE PRECISION                                                  HWDEFI 2
     & PHEP,VHEP,PBEAM1,PBEAM2,QCDLAM,VGCUT,VQCUT,VPCUT,BETAF,CAFAC,    HWDEFI 3
     & CFFAC,CLMAX,CLPOW,PSPLT,QSPAC,PTRMS,PXRMS,QG,QV,SWEIN,SCABI,     HWDEFI 4
     & PDIQK,QDIQK,ENSOF,TMTOP,ZBINM,GAMW,GAMZ,GAMH,PGSMX,PGSPL,PPAR,   HWDEFI 5
     & VPAR,PHIPAR,DECPAR,RHOPAR,RHOHEP,XFACT,PTINT,EVWGT,AVWGT,WGTMAX, HWDEFI 6
     & WGTSUM,WSQSUM,WBIGST,TLOUT,YJMIN,YJMAX,PTMIN,PTMAX,PTPOW,EMMIN,  HWDEFI 7
     & EMMAX,EMPOW,Q2MIN,Q2MAX,Q2POW,THMAX,QLIM,XXMIN,XLMIN,EMSCA,      HWDEFI 8
     & EMLST,COSTH,GPOLN,GCOEF,XX,DISF,RESN,RMIN,CTMAX,FBTM,            HWDEFI 9
     & FTOP,FHVY,RMASS,BFRAC,CMMOM,ACCUR,QEV,SUD,VECWT,TENWT,DECWT,     HWDEFI10
     & QWT,PWT,SWT,SWTEF,RESWT,PIFAC,QCDL3,QCDL5,BRHIG,GAMMAX,          HWDEFI11
     & ENHANC,B1LIM,ALPFAC,Q2WWMN,Q2WWMX,BTCLM,ETAMIX,                  HWDEFI12
     & QFCH,VFCH,AFCH,VCKM,TQWT,CLQ,EPOLN,PPOLN,COSS,SINS,GAMZP,GEV2NB, HWDEFI13
     & ALPHEM,PRSOF,EBEAM1,EBEAM2,ZJMAX,YBMIN,YBMAX,HARDST,CLSMR,PHOMAS HWDEFI14
C                                                                       HWDEFI15
      INTEGER NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP,            HWDEFI16
     & IPROC,MAXEV,IPRINT,LRSUD,LWSUD,NCOLO,NFLAV,MODPDF(2),NSTRU,      HWDEFI17
     & NZBIN,NBTRY,NCTRY,NDTRY,NETRY,NSTRY,NGSPL,NMXPAR,NEVPAR,         HWDEFI18
     & NPAR,ISTPAR,IDPAR,JMOPAR,JDAPAR,JCOPAR,INHAD,JNHAD,NSPAC,NRN,    HWDEFI19
     & MAXER,MAXPR,LWEVT,ISTAT,IERROR,NWGTS,IDHW,IBSH,IBRN,IPRO,        HWDEFI20
     & IFLMIN,IFLMAX,MAXFL,IDCMF,IHPRO,IDN,ICO,LOCN,                    HWDEFI21
     & NRES,IDPDG,ICHRG,MADDR,MODES,MODEF,IDPRO,INTER,NQEV,             HWDEFI22
     & NSUD,NMXSUD,MODBOS,IOPHIG,MODMAX,SUDORD,CLDIR,NUMER,NUMERU,      HWDEFI23
     & MAPQ,IPART1,IPART2,ISPAC,IAPHIG                                  HWDEFI24
C                                                                       HWDEFI25
      LOGICAL AZSOFT,AZSPIN,FROST,GENEV,BGSHAT,NOWGT,TMPAR,PRNDEC,      HWDEFI26
     & HVFCEN,FSTEVT,FSTWGT,BREIT,USECMF,ZPRIME,TPOL,GENSOF,            HWDEFI27
     & HARDME,SOFTME,NOSPAC                                             HWDEFI28
C                                                                       HWDEFI29
      CHARACTER*4 PART1,PART2,RNAME,BDECAY                              HWDEFI30
      CHARACTER*20 AUTPDF(2)                                            HWDEFI31
C                                                                       HWDEFI32
      PARAMETER (NMXHEP=2000)                                           HWDEFI33
      INTEGER NMXJET                                                    HWDEFI34
      PARAMETER (NMXJET=200)                                            HWDEFI35
C                                                                       HWDEFI36
      COMMON/HWEVNT/EVWGT,AVWGT,WGTMAX,WGTSUM,WSQSUM,WBIGST,TLOUT,      HWEVNT 2
     & NRN(2),MAXER,NUMER,NUMERU,MAXPR,LWEVT,ISTAT,IERROR,NOWGT,NWGTS,  HWEVNT 3
     & IDHW(NMXHEP),GENSOF                                              HWEVNT 4
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),           HEPEVT 2
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)  HEPEVT 3
      COMMON/HWUPDT/RMASS(264),BFRAC(460),CMMOM(460),ETAMIX,IDPDG(264), HWUPDT 2
     & ICHRG(264),MADDR(264),MODES(264),MODEF(264),IDPRO(3,460),NRES    HWUPDT 3
C---MAX NUMBER OF ENTRIES IN LOOKUP TABLES OF SUDAKOV FORM FACTORS      HWUPDT 4
C                                                                       UHRFI 17
      INTEGER NMUL,NCHMUL                                               UHRFI 18
      INTEGER IMOTH,ITRK,JANTI,IDPR,IDPR1                               UHRFI 19
C                                                                       UHRFI 20
      REAL WEIT,COSTHE                                                  UHRFI 21
      REAL PX,PY,PZ,PMOM                                                UHRFI 22
C                                                                       UHRFI 23
C initialize some counters                                              UHRFI 24
C                                                                       UHRFI 25
      NMUL = 0                                                          UHRFI 26
      NCHMUL = 0                                                        UHRFI 27
C                                                                       UHRFI 28
      WEIT = 1.                                                         UHRFI 29
C                                                                       UHRFI 30
C loop over the tracks                                                  UHRFI 31
C                                                                       UHRFI 32
      DO 100 ITRK = 1,NHEP                                              UHRFI 33
C                                                                       UHRFI 34
C look for primary quarks/antiquarks                                    UHRFI 35
C                                                                       UHRFI 36
        IF (ISTHEP(ITRK).EQ.123.OR.ISTHEP(ITRK).EQ.124) THEN            UHRFI 37
C                                                                       UHRFI 38
          IMOTH = JMOHEP(1,ITRK)                                        UHRFI 39
C                                                                       UHRFI 40
          IF (ISTHEP(IMOTH).EQ.120) THEN                                UHRFI 41
C                                                                       UHRFI 42
C compute momentum and polar angle                                      UHRFI 43
C                                                                       UHRFI 44
            PX = SNGL(PHEP(1,ITRK))                                     UHRFI 45
            PY = SNGL(PHEP(2,ITRK))                                     UHRFI 46
            PZ = SNGL(PHEP(3,ITRK))                                     UHRFI 47
C                                                                       UHRFI 48
            PMOM = SQRT(PX*PX + PY*PY + PZ*PZ)                          UHRFI 49
C                                                                       UHRFI 50
            IF (PMOM.GT.0) THEN                                         UHRFI 51
C                                                                       UHRFI 52
              COSTHE = PZ / PMOM                                        UHRFI 53
C                                                                       UHRFI 54
C quarks                                                                UHRFI 55
C                                                                       UHRFI 56
              IF (IDHW(ITRK).GE.1.AND.IDHW(ITRK).LE.6)                  UHRFI 57
     &          CALL HFILL(10010,COSTHE,0.,WEIT)                        UHRFI 58
C                                                                       UHRFI 59
C antiquarks                                                            UHRFI 60
C                                                                       UHRFI 61
              IF (IDHW(ITRK).GE.7.AND.IDHW(ITRK).LE.12)                 UHRFI 62
     &          CALL HFILL(10011,COSTHE,0.,WEIT)                        UHRFI 63
C                                                                       UHRFI 64
            ELSE                                                        UHRFI 65
C                                                                       UHRFI 66
              WRITE (IW(6),'(/1X,''+++UHRFI+++ track with zero''        UHRFI 67
     &          ,'' zero momentum found in event '',I6)')NEVHEP         UHRFI 68
C                                                                       UHRFI 69
            ENDIF                                                       UHRFI 70
C                                                                       UHRFI 71
          ENDIF                                                         UHRFI 72
C                                                                       UHRFI 73
        ENDIF                                                           UHRFI 74
C                                                                       UHRFI 75
        IF (ISTHEP(ITRK).EQ.1) THEN                                     UHRFI 76
C                                                                       UHRFI 77
C we have a stable particle                                             UHRFI 78
C                                                                       UHRFI 79
          NMUL = NMUL + 1                                               UHRFI 80
C                                                                       UHRFI 81
          IF (ICHRG(IDHW(ITRK)).NE.0) NCHMUL = NCHMUL +1                UHRFI 82
C                                                                       UHRFI 83
C get momenta                                                           UHRFI 84
C                                                                       UHRFI 85
          PX = SNGL(PHEP(1,ITRK))                                       UHRFI 86
          PY = SNGL(PHEP(2,ITRK))                                       UHRFI 87
          PZ = SNGL(PHEP(3,ITRK))                                       UHRFI 88
C                                                                       UHRFI 89
C compute momentum                                                      UHRFI 90
C                                                                       UHRFI 91
          PMOM = SQRT(PX*PX + PY*PY + PZ*PZ)                            UHRFI 92
C                                                                       UHRFI 93
          CALL HFILL(10030,PMOM,0.,WEIT)                                UHRFI 94
C                                                                       UHRFI 95
        ENDIF                                                           UHRFI 96
C                                                                       UHRFI 97
C radiative photons                                                     UHRFI 98
C AST                                                                   UHRFI 99
        if(isthep(itrk).eq.2.and.idhw(itrk).eq.59) then                 UHRFI100
C AST                                                                   UHRFI101
          IMOTH = JMOHEP(1,ITRK)                                        UHRFI102
          IDPR1= ABS(IDHW(IMOTH))                                       UHRFI103
          IF(IDPR1.GT.6) IDPR1=IDPR1-6                                  UHRFI104
          IF(IDPR1.EQ.1) THEN                                           UHRFI105
            IDPR1=2                                                     UHRFI106
          ELSEIF(IDPR1.EQ.2) THEN                                       UHRFI107
            IDPR1=1                                                     UHRFI108
          ENDIF                                                         UHRFI109
          PZ = SNGL(PHEP(3,ITRK))                                       UHRFI110
          PMOM = SNGL(PHEP(4,ITRK))                                     UHRFI111
          CALL HFILL(10100,PMOM,0.,1.)                                  UHRFI112
          CALL HFILL(10120,FLOAT(IDPR1),0.,1.)                          UHRFI113
C                                                                       UHRFI114
          IF (PMOM.GT.0) THEN                                           UHRFI115
            COSTHE = PZ / PMOM                                          UHRFI116
            CALL HFILL(10110,COSTHE,0.,1.)                              UHRFI117
          ELSE                                                          UHRFI118
            WRITE (IW(6),'(/1X,''+++UHRFI+++ rad. photon with zero''    UHRFI119
     &        ,'' zero momentum found in event '',I6)')NEVHEP           UHRFI120
          ENDIF                                                         UHRFI121
C                                                                       UHRFI122
        ENDIF                                                           UHRFI123
C                                                                       UHRFI124
C INITIAL STATE PHOTONS                                                 UHRFI125
        IF(IDHW(ITRK).EQ.59.AND.JMOHEP(1,ITRK).EQ.3) THEN               UHRFI126
          PZ = SNGL(PHEP(3,ITRK))                                       UHRFI127
          PMOM = SNGL(PHEP(4,ITRK))                                     UHRFI128
          CALL HFILL(10200,PMOM,0.,1.)                                  UHRFI129
          IF(PMOM.GT.0.) THEN                                           UHRFI130
            COSTHE=PZ/PMOM                                              UHRFI131
            CALL HFILL(10210,COSTHE,0.,1.)                              UHRFI132
          ENDIF                                                         UHRFI133
        ENDIF                                                           UHRFI134
C                                                                       UHRFI135
  100 CONTINUE                                                          UHRFI136
C                                                                       UHRFI137
      CALL HFILL(10020,FLOAT(NMUL)+0.0001,0.,WEIT)                      UHRFI138
      CALL HFILL(10021,FLOAT(NCHMUL)+0.0001,0.,WEIT)                    UHRFI139
C                                                                       UHRFI140
      RETURN                                                            UHRFI141
C                                                                       UHRFI142
      END                                                               UHRFI143
      SUBROUTINE HWIDY3(ITYPE,NQUA)                                     HWIDY3 2
C--------------------------------------------------------------------   HWIDY3 3
C    A.S.Thompson    May   1992 Implementation of DYMU3 inside HRWG06   HWIDY3 4
C      based on :-                                                      HWIDY3 5
C    B.Bloch-Devaux  April 1991 Implementation of DYMU3 inside HVFL02   HWIDY3 6
C!  initialisation routine for DYMU3 generator                          HWIDY3 7
C                                                                       HWIDY3 8
C     structure : subroutine                                            HWIDY3 9
C                                                                       HWIDY310
C     input     : ITYPE  type of quark to generate                      HWIDY311
C                        (note in HERWIG order)                         HWIDY312
C                 NQUA   no. of quark types                             HWIDY313
C     output    : none                                                  HWIDY314
C                                                                       HWIDY315
C     Note      : Input parameters are set by ASKUSI in HRWG06          HWIDY316
C--------------------------------------------------------------------   HWIDY317
C                                                                       HWIDY318
      COMMON/HWUPDT/RMASS(264),BFRAC(460),CMMOM(460),ETAMIX,IDPDG(264), HWUPDT 2
     & ICHRG(264),MADDR(264),MODES(264),MODEF(264),IDPRO(3,460),NRES    HWUPDT 3
C---MAX NUMBER OF ENTRIES IN LOOKUP TABLES OF SUDAKOV FORM FACTORS      HWUPDT 4
      COMMON /CONST/ ALFA,PI,ALFA1                                      DYMUCOM2
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0              DYMUCOM3
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI       DYMUCOM4
      COMMON /BEAM/ S0,EBEAM                                            DYMUCOM5
      COMMON /TAU / TAUV,CPTAU,HEL,PITAU(4)                             DYMUCOM6
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT                            DYMUCOM7
      COMMON/ZDUMP / IDEBU , NEVT, IOUT                                 DYMUCOM8
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY                 DYMUCOM9
      REAL*8 RMASS,BFRAC,CMMOM,ETAMIX                                   HWIDY321
      DIMENSION ISEED(3)                                                HWIDY322
      INTEGER ALTABL                                                    HWIDY323
      XTOT = 0.                                                         HWIDY324
C                                                                       HWIDY325
C      DYMU3 PARAMETERS     DEFAULT VALUES                              HWIDY326
C                                                                       HWIDY327
      SOLD = -1.                                                        HWIDY328
      ID2 = 0                                                           HWIDY329
      ID3 = 0                                                           HWIDY330
      IEXPO = 1                                                         HWIDY331
      FINEXP =-1.                                                       HWIDY332
      POIDS = 1.                                                        HWIDY333
      INTERF = 0                                                        HWIDY334
      IDEBU = 0                                                         HWIDY335
      NEVT = 0                                                          HWIDY336
      TAUV = 0.                                                         HWIDY337
      NEVMA = 5000                                                      HWIDY338
C                         K0 MINIMUM HARD PHOTON ENERGY/EBEAM           HWIDY339
      XK0 = 0.003                                                       HWIDY340
C                         WEAK ISOSPIN AND CHARGE OF OUTGOING FERMION   HWIDY341
      QCDFAC = 1.04                                                     HWIDY342
C                                                                       HWIDY343
      AEL = 0.511E-3                                                    HWIDY344
      T3     = SIGN(0.5,QI)                                             HWIDY345
      COL    = 3.*QCDFAC                                                HWIDY346
C    Quarks                                                             HWIDY347
      IF ( ITYPE.EQ.0)  THEN                                            HWIDY348
C    Save initial seeds to start the real events                        HWIDY349
        CALL RDMOUT(ISEED)                                              HWIDY350
        I1 = ISEED(1)                                                   HWIDY351
        I2 = ISEED(2)                                                   HWIDY352
        I3 = ISEED(3)                                                   HWIDY353
C    Quarks mixture                                                     HWIDY354
        DO 15  II=1,6                                                   HWIDY355
 15     XSECT(II)=0.                                                    HWIDY356
C   No DEBUG needed here!                                               HWIDY357
        IDEBU = 0                                                       HWIDY358
        DO 6 II = 1,NQUA                                                HWIDY359
          NEVT = 0                                                      HWIDY360
          QI     = FLOAT(ICHRG(II))/3.                                  HWIDY361
          T3     = SIGN(0.5,QI)                                         HWIDY362
          COL    = 3.*QCDFAC                                            HWIDY363
          AMU    = RMASS(II)                                            HWIDY364
          CALL DYMUIN(II)                                               HWIDY365
C    COPY coupling constants for this type in an array                  HWIDY366
          CALL UCOPY(AEL,WEAKC(1,II),11)                                HWIDY367
          DO 66 KEV=1,NEVMA                                             HWIDY368
            CALL DYMUS(WEI)                                             HWIDY369
  66      CONTINUE                                                      HWIDY370
          CALL FINISH(II,NEVMA)                                         HWIDY371
C   Store cross-sections for each process                               HWIDY372
          XSECT(II) = SIGTOT                                            HWIDY373
  6     CONTINUE                                                        HWIDY374
C   Store total cross-section                                           HWIDY375
        XTOT = XSECT(1)                                                 HWIDY376
        DO 75 II = 2,NQUA                                               HWIDY377
  75    XTOT = XTOT+XSECT(II)                                           HWIDY378
        DO 7 II=2,NQUA                                                  HWIDY379
  7     XSECT(II) = XSECT(II-1)+XSECT(II)                               HWIDY380
C     Normalize                                                         HWIDY381
        DO 8 II = 1,NQUA                                                HWIDY382
  8     XSECT(II) = XSECT(II)/XSECT(NQUA)                               HWIDY383
        WRITE(IOUT,100)   XSECT                                         HWIDY384
 100    FORMAT('       INTEGRATED CROSS SECTIONS FOR QUARKS ',          HWIDY385
     $           /,9X,6F10.4)                                           HWIDY386
C   Restore initial seed                                                HWIDY387
        CALL RMARIN(I1,I2,I3)                                           HWIDY388
      ENDIF                                                             HWIDY389
C                                                                       HWIDY390
C       INITIALIZE DYMU3                                                HWIDY391
C                                                                       HWIDY392
      CALL DYMUIN(ITYPE)                                                HWIDY393
C                                                                       HWIDY394
      RETURN                                                            HWIDY395
      END                                                               HWIDY396
      SUBROUTINE DYMUIN(NTYP)                                           DYMUIN 2
C-----------------------------------------------------------------------DYMUIN 3
C    B.Bloch -Devaux APRIL 1991                                         DYMUIN 4
C         ORIGINAL VERSION OF DYMU3 AS PROVIDED BY J.E.Campagne         DYMUIN 5
C                       June 1989                                       DYMUIN 6
C  This is a subset of original subroutine INIRUN to compute secondary  DYMUIN 7
C  quantities according to requested final state.                       DYMUIN 8
C    A.S.Thompson   MAY 1992. adapted to run under HERWIG               DYMUIN 9
C-----------------------------------------------------------------------DYMUIN10
      COMMON /CONST/ ALFA,PI,ALFA1                                      DYMUCOM2
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0              DYMUCOM3
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI       DYMUCOM4
      COMMON /BEAM/ S0,EBEAM                                            DYMUCOM5
      COMMON /TAU / TAUV,CPTAU,HEL,PITAU(4)                             DYMUCOM6
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT                            DYMUCOM7
      COMMON/ZDUMP / IDEBU , NEVT, IOUT                                 DYMUCOM8
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY                 DYMUCOM9
C                                                                       DYMUIN12
      COMMON / VECLAB / PFP(4),PFM(4),GAP(4),GAM(4),GAF(4)              VECLAB 2
      REAL*4 PFP,PFM,GAP,GAM,GAF                                        VECLAB 3
      COMMON /COUNTS/ SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2              DYMUIN14
      COMMON /EVTS/ NEVT1,NEVT2,NFWD,NBKW                               DYMUIN15
      REAL*8 SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2                       DYMUIN16
C                                                                       DYMUIN17
      DIMENSION PBEA(4)                                                 DYMUIN18
      DIMENSION ISEED(3)                                                DYMUIN19
      LOGICAL FIRST                                                     DYMUIN20
      DATA FIRST/.TRUE./                                                DYMUIN21
      IF(FIRST)THEN                                                     DYMUIN22
        FIRST = .FALSE.                                                 DYMUIN23
        WRITE(IOUT,*)'*****************************************'        DYMUIN24
        WRITE(IOUT,*)'*         WELCOME    TO    DYMU3        *'        DYMUIN25
        WRITE(IOUT,*)'*                                       *'        DYMUIN26
        WRITE(IOUT,*)'*         AUTHORS: J.E.CAMPAGNE         *'        DYMUIN27
        WRITE(IOUT,*)'*                  R.ZITOUN             *'        DYMUIN28
        WRITE(IOUT,*)'*                                       *'        DYMUIN29
        WRITE(IOUT,*)'*         19 nov  89 RELEASE            *'        DYMUIN30
        WRITE(IOUT,*)'*                                       *'        DYMUIN31
        WRITE(IOUT,*)'*****************************************'        DYMUIN32
        WRITE(IOUT,*)' '                                                DYMUIN33
C                                                                       DYMUIN34
      ENDIF                                                             DYMUIN35
C                                                                       DYMUIN36
      ITYP = NTYP                                                       DYMUIN37
      NTYPO = ITYP                                                      DYMUIN38
C                                                                       DYMUIN39
       WRITE(IOUT,1533) ITYP                                            DYMUIN40
 1533 FORMAT(1X,'**************************************************'/   DYMUIN41
     X      ,1X,'* D Y M U 3    A D A P T E D   T O   K I N G A L *'/   DYMUIN42
     X      ,1X,'* GENERATING FERMION TYPE:',I3,'    ( HERWIG 5.8 )  *'/DYMUIN43
     &      ,1X,'**************************************************')   DYMUIN44
*---- BEAM ENERGY                                                       DYMUIN45
*                                                                       DYMUIN46
      ECMS = 2.*EBEAM                                                   DYMUIN47
      S0 = 4.*EBEAM**2                                                  DYMUIN48
*                                                                       DYMUIN49
*----  COUPLING CONSTANTS                                               DYMUIN50
*                                                                       DYMUIN51
      CV    = (-1.+4.*SW2)/4./SQRT(SW2*(1.-SW2))                        DYMUIN52
      CA    = -1./4./SQRT(SW2*(1.-SW2))                                 DYMUIN53
      CVPRI = (-2*T3/QI+4.*SW2)/4./SQRT(SW2*(1.-SW2))                   DYMUIN54
      CAPRI = -T3/QI/2./SQRT(SW2*(1.-SW2))                              DYMUIN55
      CV2 = CVPRI*CV                                                    DYMUIN56
      CA2 = CAPRI*CA                                                    DYMUIN57
      CA2CV2 = ( CV**2+CA**2 )*( CVPRI**2+CAPRI**2 )                    DYMUIN58
      CALL DYMUSI                                                       DYMUIN59
*                                                                       DYMUIN60
*---- CONST1                                                            DYMUIN61
*                                                                       DYMUIN62
      ALFA  = 1./137.036                                                DYMUIN63
      PI    = 3.14159265                                                DYMUIN64
      ALFA1 = ALFA/PI                                                   DYMUIN65
*                                                                       DYMUIN66
*----                                                                   DYMUIN67
*                                                                       DYMUIN68
      WRITE(IOUT,*)'*************************************************'  DYMUIN69
      WRITE(IOUT,*)'*     RUN PARAMETERS FOR RUN',ITYP                  DYMUIN70
      WRITE(IOUT,*)'*************************************************'  DYMUIN71
      WRITE(IOUT,1000) AMZ,GAMM,SW2                                     DYMUIN72
      WRITE(IOUT,1003) ECMS,EBEAM                                       DYMUIN73
 1000   FORMAT('     Z MASS =',F8.3,' GEV ,      Z WIDTH =',F6.3,       DYMUIN74
     &         ' GEV ,  SIN2 TETA =',F7.4)                              DYMUIN75
 1003   FORMAT(' CMS ENERGY =',F8.3,' GEV ,  BEAM ENERGY =',F8.3)       DYMUIN76
*                                                                       DYMUIN77
      IF(POIDS.EQ.1)THEN                                                DYMUIN78
        WRITE(IOUT,*)'UNWEIGHTED EVENTS'                                DYMUIN79
      ELSE                                                              DYMUIN80
        WRITE(IOUT,*)'WEIGHTED EVENTS'                                  DYMUIN81
      ENDIF                                                             DYMUIN82
      WRITE(IOUT,*)'INITIAL STATE EXPONENTIATION'                       DYMUIN83
      IF(FINEXP.EQ.1)THEN                                               DYMUIN84
        WRITE(IOUT,*)'FINAL STATE EXPONENTIATION'                       DYMUIN85
      ELSE IF(FINEXP.EQ.0) THEN                                         DYMUIN86
        WRITE(IOUT,*)'NO FINAL STATE EXPONENTIATION.'                   DYMUIN87
      ELSE IF(FINEXP.EQ.-1) THEN                                        DYMUIN88
        WRITE(IOUT,*)'NO FINAL STATE PHOTON'                            DYMUIN89
      ENDIF                                                             DYMUIN90
      IF(ID2.EQ.1)THEN                                                  DYMUIN91
        WRITE(IOUT,*)'DII IN D(X)'                                      DYMUIN92
      ELSE                                                              DYMUIN93
        WRITE(IOUT,*)'DII NOT IN D(X)'                                  DYMUIN94
      ENDIF                                                             DYMUIN95
      IF(ID3.EQ.1)THEN                                                  DYMUIN96
        WRITE(IOUT,*)'DIII IN D(X)'                                     DYMUIN97
      ELSE                                                              DYMUIN98
        WRITE(IOUT,*)'DIII NOT IN D(X)'                                 DYMUIN99
      ENDIF                                                             DYMUI100
      IF(INTERF.EQ.0)THEN                                               DYMUI101
        WRITE(IOUT,*)'NO INTERFERENCE'                                  DYMUI102
      ELSE                                                              DYMUI103
        WRITE(IOUT,*)'INTERFERENCE WITH K0 =',XK0                       DYMUI104
      ENDIF                                                             DYMUI105
*                                                                       DYMUI106
      CALL RDMOUT(ISEED)                                                DYMUI107
      WRITE(IOUT,*)'INITIAL SEEDS ARE',ISEED                            DYMUI108
      WRITE(IOUT,*)'*************************************************'  DYMUI109
*                                                                       DYMUI110
*---- SET TO ZERO                                                       DYMUI111
*                                                                       DYMUI112
      SIG    = 0.                                                       DYMUI113
      SIG2   = 0.                                                       DYMUI114
      SECFWD = 0.                                                       DYMUI115
      SECBKW = 0.                                                       DYMUI116
      SCFWD2 = 0.                                                       DYMUI117
      SCBKW2 = 0.                                                       DYMUI118
      NEVT1  = 0                                                        DYMUI119
      NEVT2  = 0                                                        DYMUI120
      NFWD   = 0                                                        DYMUI121
      NBKW   = 0                                                        DYMUI122
C                                                                       DYMUI123
      RETURN                                                            DYMUI124
C ----------------------------------------------------------------------DYMUI125
C                                                                       DYMUI126
      ENTRY HWEDY3                                                      DYMUI127
C                                                                       DYMUI128
      IF (NTYPO.EQ.0 ) THEN                                             DYMUI129
C   Decide which flavor to generate                                     DYMUI130
        XX = RNDM(DUM)                                                  DYMUI131
        DO 30 I=1,6                                                     DYMUI132
          IF (XX.LT.XSECT(I)) GO TO 40                                  DYMUI133
  30    CONTINUE                                                        DYMUI134
  40    ITYP = I                                                        DYMUI135
C   Copy corresponding coupling constants                               DYMUI136
        CALL UCOPY(WEAKC(1,I),AEL,11)                                   DYMUI137
        CALL DYMUSI                                                     DYMUI138
      ENDIF                                                             DYMUI139
C                                                                       DYMUI140
      CALL DYMUS (WE)                                                   DYMUI141
      NEVT = NEVT+1                                                     DYMUI142
C                              HERWIG INTERFACE                         DYMUI143
      CALL DYTOHW(ITYP)                                                 DYMUI144
C                                                                       DYMUI145
  100 CONTINUE                                                          DYMUI146
      RETURN                                                            DYMUI147
C                                                                       DYMUI148
C-----------------------------------------------------------------------DYMUI149
      ENTRY DYMUND                                                      DYMUI150
C                                                                       DYMUI151
      CALL FINISH(NTYPO,NEVT)                                           DYMUI152
C                                                                       DYMUI153
      RETURN                                                            DYMUI154
      END                                                               DYMUI155
      SUBROUTINE DYTOHW(ITYP)                                           DYTOHW 2
C --------------------------------------------------------------------  DYTOHW 3
C A.S.Thompson   May 1992                                               DYTOHW 4
C                                                                       DYTOHW 5
C fill HERWIG event common with DYMU3 qqbar(gamma)                      DYTOHW 6
C --------------------------------------------------------------------  DYTOHW 7
C                                                                       DYTOHW 8
      IMPLICIT NONE                                                     DYTOHW 9
C                                                                       DYTOHW10
      DOUBLE PRECISION                                                  HWDEFI 2
     & PHEP,VHEP,PBEAM1,PBEAM2,QCDLAM,VGCUT,VQCUT,VPCUT,BETAF,CAFAC,    HWDEFI 3
     & CFFAC,CLMAX,CLPOW,PSPLT,QSPAC,PTRMS,PXRMS,QG,QV,SWEIN,SCABI,     HWDEFI 4
     & PDIQK,QDIQK,ENSOF,TMTOP,ZBINM,GAMW,GAMZ,GAMH,PGSMX,PGSPL,PPAR,   HWDEFI 5
     & VPAR,PHIPAR,DECPAR,RHOPAR,RHOHEP,XFACT,PTINT,EVWGT,AVWGT,WGTMAX, HWDEFI 6
     & WGTSUM,WSQSUM,WBIGST,TLOUT,YJMIN,YJMAX,PTMIN,PTMAX,PTPOW,EMMIN,  HWDEFI 7
     & EMMAX,EMPOW,Q2MIN,Q2MAX,Q2POW,THMAX,QLIM,XXMIN,XLMIN,EMSCA,      HWDEFI 8
     & EMLST,COSTH,GPOLN,GCOEF,XX,DISF,RESN,RMIN,CTMAX,FBTM,            HWDEFI 9
     & FTOP,FHVY,RMASS,BFRAC,CMMOM,ACCUR,QEV,SUD,VECWT,TENWT,DECWT,     HWDEFI10
     & QWT,PWT,SWT,SWTEF,RESWT,PIFAC,QCDL3,QCDL5,BRHIG,GAMMAX,          HWDEFI11
     & ENHANC,B1LIM,ALPFAC,Q2WWMN,Q2WWMX,BTCLM,ETAMIX,                  HWDEFI12
     & QFCH,VFCH,AFCH,VCKM,TQWT,CLQ,EPOLN,PPOLN,COSS,SINS,GAMZP,GEV2NB, HWDEFI13
     & ALPHEM,PRSOF,EBEAM1,EBEAM2,ZJMAX,YBMIN,YBMAX,HARDST,CLSMR,PHOMAS HWDEFI14
C                                                                       HWDEFI15
      INTEGER NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP,            HWDEFI16
     & IPROC,MAXEV,IPRINT,LRSUD,LWSUD,NCOLO,NFLAV,MODPDF(2),NSTRU,      HWDEFI17
     & NZBIN,NBTRY,NCTRY,NDTRY,NETRY,NSTRY,NGSPL,NMXPAR,NEVPAR,         HWDEFI18
     & NPAR,ISTPAR,IDPAR,JMOPAR,JDAPAR,JCOPAR,INHAD,JNHAD,NSPAC,NRN,    HWDEFI19
     & MAXER,MAXPR,LWEVT,ISTAT,IERROR,NWGTS,IDHW,IBSH,IBRN,IPRO,        HWDEFI20
     & IFLMIN,IFLMAX,MAXFL,IDCMF,IHPRO,IDN,ICO,LOCN,                    HWDEFI21
     & NRES,IDPDG,ICHRG,MADDR,MODES,MODEF,IDPRO,INTER,NQEV,             HWDEFI22
     & NSUD,NMXSUD,MODBOS,IOPHIG,MODMAX,SUDORD,CLDIR,NUMER,NUMERU,      HWDEFI23
     & MAPQ,IPART1,IPART2,ISPAC,IAPHIG                                  HWDEFI24
C                                                                       HWDEFI25
      LOGICAL AZSOFT,AZSPIN,FROST,GENEV,BGSHAT,NOWGT,TMPAR,PRNDEC,      HWDEFI26
     & HVFCEN,FSTEVT,FSTWGT,BREIT,USECMF,ZPRIME,TPOL,GENSOF,            HWDEFI27
     & HARDME,SOFTME,NOSPAC                                             HWDEFI28
C                                                                       HWDEFI29
      CHARACTER*4 PART1,PART2,RNAME,BDECAY                              HWDEFI30
      CHARACTER*20 AUTPDF(2)                                            HWDEFI31
C                                                                       HWDEFI32
      PARAMETER (NMXHEP=2000)                                           HWDEFI33
      INTEGER NMXJET                                                    HWDEFI34
      PARAMETER (NMXJET=200)                                            HWDEFI35
C                                                                       HWDEFI36
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),           HEPEVT 2
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)  HEPEVT 3
      COMMON/HWBEAM/PART1,PART2,IPART1,IPART2                           HWBEAM 2
      COMMON/HWEVNT/EVWGT,AVWGT,WGTMAX,WGTSUM,WSQSUM,WBIGST,TLOUT,      HWEVNT 2
     & NRN(2),MAXER,NUMER,NUMERU,MAXPR,LWEVT,ISTAT,IERROR,NOWGT,NWGTS,  HWEVNT 3
     & IDHW(NMXHEP),GENSOF                                              HWEVNT 4
      COMMON/HWHARD/YJMIN,YJMAX,PTMIN,PTMAX,PTPOW,EMMIN,EMMAX,EMPOW,    HWHARD 2
     & Q2MIN,Q2MAX,Q2POW,Q2WWMN,Q2WWMX,THMAX,QLIM,XXMIN,XLMIN,ZJMAX,    HWHARD 3
     & YBMIN,YBMAX,EMSCA,EMLST,COSTH,CTMAX,GPOLN,GCOEF(7),XX(2),PHOMAS, HWHARD 4
     & DISF(13,2),TQWT,CLQ(7,6),MAPQ(6),EPOLN(3),PPOLN(3),COSS,SINS,    HWHARD 5
     & IBSH,IBRN(2),IPRO,IFLMIN,IFLMAX,MAXFL,IDCMF,IHPRO,               HWHARD 6
     & IDN(10),ICO(10),GENEV,FSTEVT,FSTWGT,BGSHAT,HVFCEN,               HWHARD 7
     & TPOL,IAPHIG                                                      HWHARD 8
C---UTILITIES COMMON                                                    HWHARD 9
      COMMON/HWUCLU/RESN(12,12),RMIN(12,12),LOCN(12,12)                 HWHARD10
      COMMON/HWPROC/PBEAM1,PBEAM2,EBEAM1,EBEAM2,IPROC,MAXEV             HWPROC 2
C                                                                       HWPROC 3
      COMMON/HWUPDT/RMASS(264),BFRAC(460),CMMOM(460),ETAMIX,IDPDG(264), HWUPDT 2
     & ICHRG(264),MADDR(264),MODES(264),MODEF(264),IDPRO(3,460),NRES    HWUPDT 3
C---MAX NUMBER OF ENTRIES IN LOOKUP TABLES OF SUDAKOV FORM FACTORS      HWUPDT 4
      COMMON / VECLAB / PFP(4),PFM(4),GAP(4),GAM(4),GAF(4)              VECLAB 2
      REAL*4 PFP,PFM,GAP,GAM,GAF                                        VECLAB 3
C                                                                       DYTOHW19
      INTEGER ITYP,NZ0,ICA                                              DYTOHW20
      REAL*8 PMULT(4)                                                   DYTOHW21
      DATA PMULT/-1.,-1.,-1.,1./                                        DYTOHW22
C                                                                       DYTOHW23
C set up initial state                                                  DYTOHW24
C                                                                       DYTOHW25
      NHEP=1                                                            DYTOHW26
      ISTHEP(NHEP)=101                                                  DYTOHW27
      PHEP(1,NHEP)=0.                                                   DYTOHW28
      PHEP(2,NHEP)=0.                                                   DYTOHW29
      PHEP(3,NHEP)=PBEAM1                                               DYTOHW30
      PHEP(4,NHEP)=EBEAM1                                               DYTOHW31
      PHEP(5,NHEP)=RMASS(IPART1)                                        DYTOHW32
      JMOHEP(1,NHEP)=0                                                  DYTOHW33
      JMOHEP(2,NHEP)=0                                                  DYTOHW34
      JDAHEP(1,NHEP)=0                                                  DYTOHW35
      JDAHEP(2,NHEP)=0                                                  DYTOHW36
      IDHW(NHEP)=IPART1                                                 DYTOHW37
      IDHEP(NHEP)=IDPDG(IPART1)                                         DYTOHW38
      NHEP=NHEP+1                                                       DYTOHW39
      ISTHEP(NHEP)=102                                                  DYTOHW40
      PHEP(1,NHEP)=0.                                                   DYTOHW41
      PHEP(2,NHEP)=0.                                                   DYTOHW42
      PHEP(3,NHEP)=-PBEAM2                                              DYTOHW43
      PHEP(4,NHEP)=EBEAM2                                               DYTOHW44
      PHEP(5,NHEP)=RMASS(IPART2)                                        DYTOHW45
      JMOHEP(1,NHEP)=0                                                  DYTOHW46
      JMOHEP(2,NHEP)=0                                                  DYTOHW47
      JDAHEP(1,NHEP)=0                                                  DYTOHW48
      JDAHEP(2,NHEP)=0                                                  DYTOHW49
      IDHW(NHEP)=IPART2                                                 DYTOHW50
      IDHEP(NHEP)=IDPDG(IPART2)                                         DYTOHW51
C---NEXT ENTRY IS OVERALL CM FRAME                                      DYTOHW52
      NHEP=NHEP+1                                                       DYTOHW53
      IDHW(NHEP)=14                                                     DYTOHW54
      IDHEP(NHEP)=0                                                     DYTOHW55
      ISTHEP(NHEP)=103                                                  DYTOHW56
      JMOHEP(1,NHEP)=NHEP-2                                             DYTOHW57
      JMOHEP(2,NHEP)=NHEP-1                                             DYTOHW58
      JDAHEP(1,NHEP)=0                                                  DYTOHW59
      JDAHEP(2,NHEP)=0                                                  DYTOHW60
      CALL HWVSUM(4,PHEP(1,NHEP-1),PHEP(1,NHEP-2),PHEP(1,NHEP))         DYTOHW61
      CALL HWUMAS(PHEP(1,NHEP))                                         DYTOHW62
      EMSCA=PHEP(5,NHEP)                                                DYTOHW63
C                                                                       DYTOHW64
C radiative photon                                                      DYTOHW65
C                                                                       DYTOHW66
      IF (GAP(4).GT.0.001) THEN                                         DYTOHW67
        NHEP = NHEP + 1                                                 DYTOHW68
        IDHW(NHEP) = 59                                                 DYTOHW69
        IDHEP(NHEP) = IDPDG(IDHW(NHEP))                                 DYTOHW70
        ISTHEP(NHEP) = 1                                                DYTOHW71
        JMOHEP(1,NHEP) = 3                                              DYTOHW72
        JMOHEP(2,NHEP) = 3                                              DYTOHW73
        JDAHEP(1,NHEP) = 0                                              DYTOHW74
        JDAHEP(2,NHEP) = 0                                              DYTOHW75
        DO 310 ICA = 1,4                                                DYTOHW76
          PHEP(ICA,NHEP) = DBLE(GAP(ICA))                               DYTOHW77
  310   CONTINUE                                                        DYTOHW78
        PHEP(5,NHEP) = 0.                                               DYTOHW79
      ENDIF                                                             DYTOHW80
C                                                                       DYTOHW81
C radiative photon                                                      DYTOHW82
C                                                                       DYTOHW83
      IF (GAM(4).GT.0.001) THEN                                         DYTOHW84
        NHEP = NHEP + 1                                                 DYTOHW85
        IDHW(NHEP) = 59                                                 DYTOHW86
        IDHEP(NHEP) = IDPDG(IDHW(NHEP))                                 DYTOHW87
        ISTHEP(NHEP) = 1                                                DYTOHW88
        JMOHEP(1,NHEP) = 3                                              DYTOHW89
        JMOHEP(2,NHEP) = 3                                              DYTOHW90
        JDAHEP(1,NHEP) = 0                                              DYTOHW91
        JDAHEP(2,NHEP) = 0                                              DYTOHW92
        DO 315 ICA = 1,4                                                DYTOHW93
          PHEP(ICA,NHEP) = DBLE(GAM(ICA))                               DYTOHW94
  315   CONTINUE                                                        DYTOHW95
        PHEP(5,NHEP) = 0.                                               DYTOHW96
      ENDIF                                                             DYTOHW97
C the Z0                                                                DYTOHW98
C                                                                       DYTOHW99
      NHEP = NHEP + 1                                                   DYTOH100
      NZ0 = NHEP                                                        DYTOH101
      IDHW(NHEP) = 200                                                  DYTOH102
      IDHEP(NHEP) = IDPDG(IDHW(NHEP))                                   DYTOH103
      ISTHEP(NHEP) = 110                                                DYTOH104
      JMOHEP(1,NHEP) = 1                                                DYTOH105
      JMOHEP(2,NHEP) = 2                                                DYTOH106
      JDAHEP(1,NHEP) = NHEP + 1                                         DYTOH107
      JDAHEP(2,NHEP) = NHEP + 2                                         DYTOH108
      PHEP(5,NHEP)=0.                                                   DYTOH109
      DO 320 ICA = 1,4                                                  DYTOH110
        PHEP(ICA,NHEP) = DBLE(PFP(ICA)+PFM(ICA))                        DYTOH111
        PHEP(5,NHEP)=PHEP(5,NHEP)+PMULT(ICA)*PHEP(ICA,NHEP)**2          DYTOH112
  320 CONTINUE                                                          DYTOH113
      PHEP(5,NHEP)=DSQRT(PHEP(5,NHEP))                                  DYTOH114
C                                                                       DYTOH115
C the partons                                                           DYTOH116
C                                                                       DYTOH117
      NHEP = NHEP + 1                                                   DYTOH118
      IDHW(NHEP) = ITYP                                                 DYTOH119
      IDHEP(NHEP) = IDPDG(IDHW(NHEP))                                   DYTOH120
      ISTHEP(NHEP) = 113                                                DYTOH121
      JMOHEP(1,NHEP) = NZ0                                              DYTOH122
      JMOHEP(2,NHEP) = NHEP + 1                                         DYTOH123
      JDAHEP(1,NHEP) = 0                                                DYTOH124
      JDAHEP(2,NHEP) = NHEP + 1                                         DYTOH125
      DO 430 ICA = 1,4                                                  DYTOH126
        PHEP(ICA,NHEP) = DBLE(PFP(ICA))                                 DYTOH127
  430 CONTINUE                                                          DYTOH128
      PHEP(5,NHEP)=RMASS(ITYP)                                          DYTOH129
C                                                                       DYTOH130
      NHEP = NHEP + 1                                                   DYTOH131
      IDHW(NHEP) = ITYP+6                                               DYTOH132
      IDHEP(NHEP) = IDPDG(IDHW(NHEP))                                   DYTOH133
      ISTHEP(NHEP) = 114                                                DYTOH134
      JMOHEP(1,NHEP) = NZ0                                              DYTOH135
      JMOHEP(2,NHEP) = NHEP - 1                                         DYTOH136
      JDAHEP(1,NHEP) = 0                                                DYTOH137
      JDAHEP(2,NHEP) = NHEP - 1                                         DYTOH138
      DO 440 ICA = 1,4                                                  DYTOH139
        PHEP(ICA,NHEP) = DBLE(PFM(ICA))                                 DYTOH140
  440 CONTINUE                                                          DYTOH141
      PHEP(5,NHEP)=RMASS(ITYP)                                          DYTOH142
C                                                                       DYTOH143
      RETURN                                                            DYTOH144
C                                                                       DYTOH145
      END                                                               DYTOH146
      SUBROUTINE HWAEND                                                 HWAEND 2
      CALL USCJOB                                                       HWAEND 3
      CALL HPHST(0)                                                     HWAEND 4
      RETURN                                                            HWAEND 5
      END                                                               HWAEND 6
      FUNCTION XKSECT(EI)                                               XKSECT 2
      XKSECT=0                                                          XKSECT 3
      RETURN                                                            XKSECT 4
      END                                                               XKSECT 5
      SUBROUTINE USKRIN(EI)                                             USKRIN 2
      RETURN                                                            USKRIN 3
      END                                                               USKRIN 4
