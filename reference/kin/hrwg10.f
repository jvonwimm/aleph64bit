      SUBROUTINE ASKUSI(IGCOD)
C
C --------------------------------------------------------------------
C Ebi Lange  December 1988.
C Modified - A.S.Thompson, February 1992.
C Modified - A.S.Thompson, August 1992.
C Modified - A.S.Thompson, November 1993.
C Modified - M.Lehto and A.S.Thompson, November 1995.
C Modified - A.S.Thompson, May 1998
C Modified - B.Bloch, April 99 for specific b treatment
C --------------------------------------------------------------------
C
C common blocks for HERWIG
C
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
      DOUBLE PRECISION PSPB,PSPSAV,CLSSAV
      INTEGER IFLB
      COMMON /HEXTRAB/ PSPB,PSPSAV,CLSSAV,IFLB
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
      INTEGER JPARGN,JPARNA,JPARCO,JPARMA,JPARCH,JPARLT,JPARMW,JPARAN,
     +          LPARTA
      PARAMETER(JPARGN=1,JPARNA=2,JPARCO=5,JPARMA=6,JPARCH=7,JPARLT=8,
     +          JPARMW=9,JPARAN=10,LPARTA=10)
C some declarations in KMACRO
      INTEGER NAPAR,JPA,NLINK
      REAL PARMAS,TIMLIF
C
      COMMON /HWRECO/ NRECO
      INTEGER NRECO
C Flags and counters for the Herwig interface
      INTEGER NCOL,MAXFLA
C
      PARAMETER (NCOL = 49)
      PARAMETER (MAXFLA=7)
C
      INTEGER NEVENT,IFLC,IOUT,IDEBB,IDEBE,IFVRT
      REAL SDVRT,TABL,XVRT,SXVRT
C
      COMMON / DTMILL / SDVRT(3),TABL(NCOL),NEVENT(10),IFLC(MAXFLA),
     &                  XVRT(3),sXVRT(3),IFVRT
      COMMON / INPOUT / IOUT
      COMMON / DTBUG / IDEBB,IDEBE
C flag for the initial process and whether DIS process
      INTEGER IHARD,IFL,IDIS
C
      COMMON /INIPRO/ IHARD,IFL,IDIS
      character*20 datemod
      parameter (datemod=' September 25 2000')
C
      COMMON/ZDUMP / IDEBU , NEVT, IOUTDY
      COMMON /WEAK/ WEAKPAR(11)
      COMMON /BEAM/ S0,EBEAM
      INTEGER IDEBU,NEVT,IOUTDY
      REAL*4 WEAKPAR,S0,EBEAM
      CHARACTER*4 CHAINT,CHBDEC(3)
C
      INTEGER NAMIND,NOSOFT
      REAL*8 HWRGET,DUMMY
      EXTERNAL HWRGET
C
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL,ALRLEP,NDROP,NLINK
C
      INTEGER HEDE,IGCO,IVER,JHWC
C
      PARAMETER (IGCO = 5136)
      PARAMETER (IVER = 1041)   
      PARAMETER (HEDE = 52)
C
      INTEGER HEDEC(HEDE)
C Standard variables
      INTEGER IGCOD,NDAT,IMAXER,IPROCI
      INTEGER IPART,IKLIN,IPPART,JGHRW,IQUARK,IIBDEC
      INTEGER JGSPAR,NAMI,JDEBU,JGHRC,JXVRT,NAXVRT
      INTEGER JGPAR,JGMAS,MNUM,INPART,IANTI,JGSTA
      INTEGER MXDEC,NROW
      INTEGER KGPART,KNODEC,I,JSVRT,JKPAR,IEBEAM,JRLEP,JANTI
      INTEGER NDROP,IQ
C
C Gamma-gamma variables
      INTEGER PDFGROUP1,PDFGROUP2
      CHARACTER*20 CGGPDF(8)
C
      REAL RAZSOF,RAZSPI,XTOT
      REAL*8 ECMS
C
C AST - 22.4.98
      REAL*8 BOSMASS(4),GLUMA
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
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)
C
C
      DATA CHBDEC /'HERW','EURO','CLEO'/
      DATA NAPAR /0/
      DATA CGGPDF /'DOph','DG','LAC','GS','GRVph','ACF','WHIT','SaSph'/
C
C    trick to invoke block data, does not work for some compilers!
C      IF(IGCO.EQ.-1) CALL HWUDAT
      IF (NAPAR.EQ.0) NAPAR = NAMIND('PART')
C
C   Return the generator code as defined in the KINGAL library
C
      IGCOD = IGCO
      IOUT = IW(6)
      WRITE(IOUT,101) IGCOD,DATEMOD,IVER
 101  FORMAT(/,10X,
     & 'HRWG10 - Code Number = ',I4,' Last Modifications ',A20
     & ,/,10X,' Version 5.9 known bugs fixed up to 15th November 1999'
     & ,/,10X,' Aleph version ',i5,' bugs fixed up to Oct  2000'
     & ,/,10X,'***********************************************',
     &'********************',//)
C
C initialization
C
      NDAT = 0
      CALL HWIGIN
      DUMMY=HWRGET(NRN)
      IBRN(1)=12348765
      IBRN(2)=0
C
C create the KLIN bank and complete the PART  bank
C
C AST - 22.4.98  save boson masses
C
      DO I=1,4
        BOSMASS(I) = RMASS(197+I)
      ENDDO
      CALL KXHEPA(IPART,IKLIN)
C
C AST - 22.4.98 restore boson masses
      DO I=1,4
        RMASS(I+197) = BOSMASS(I)
      ENDDO
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
         WRITE (IOUT,'(1X,''+++ASKUSI+++ IPART IKLIN '',2I5)')
     &          IPART,IKLIN
         WRITE(IOUT,'('' ASKUSI error filling PART or KLIN -STOP-'')')
         CALL EXIT
      ENDIF
C Adjust masses when decays have become forbidden
C K0s K0l
       rmass(42)  = rmass(60)
       rmass(50)  = rmass(60)
C F0(980)
       rmass(293) = 0.996
C A0(980)
       rmass(290) = 0.996
C XIb s
       rmass(227) = 5.800
       rmass(228) = rmass(227)
       rmass(251) = rmass(227)
       rmass(252) = rmass(227)
C   Input parameters (see description in HERWIG)
C
      PART1 = 'E-      '
      PART2 = 'E+      '
C
C beam momentum
C
      PBEAM1 = 46.1D0
C
C HERWIG process identifier
C
      IPROC = 100
C
C initial process (e+e- --> qqbar) simulated by HERWIG
C
      IHARD = 0
C
C print PART bank per default
C
      IPPART = 1
C
C the default values can be changed by the DATA CARD GHRW
C
      JGHRW = NLINK('GHRW',0)
C
      IF(JGHRW.NE.0) THEN
        NDAT = IW(JGHRW)
        IF (NDAT.GE.1) PBEAM1 = DBLE(RW(JGHRW+1))
        IF (NDAT.GE.2) IPROC = IW(JGHRW+2)
        IF (NDAT.GE.3) IPRINT = IW(JGHRW+3)
        IF (NDAT.GE.4) IHARD = IW(JGHRW+4)
        IF (NDAT.GE.5) IPPART = IW(JGHRW+5)
        IF (NDAT.GE.6) IMAXER = IW(JGHRW+6)
      ENDIF
C
      PBEAM2 = PBEAM1
      ECMS   = PBEAM1 + PBEAM2
      IFL=MOD(IPROC,10)
C
C  LOOK AT PROCESS SPECIFIED, CHECK IT IS VIABLE
      IDIS   = 0
      NOSOFT = IPROC/10000
      IPROC  = IABS(NOSOFT*10000 - IPROC)
      IF (IPROC.GE.100.AND.IPROC.LT.150) THEN
C ee -> hadrons
        IF (IFL.EQ.7) WRITE(IOUT,
     &   '('' +++ASKUSI+++ Gluon-gluon(-gluon) process'')')
        IF(IPROC.GE.120) WRITE(IOUT,
     &   '('' +++ASKUSI+++ Note no hard gluon corrections'')')
        IF (IFL.NE.0.AND.IFL.NE.7) THEN
          IF (2.*RMASS(IFL)+0.2.GE.ECMS) THEN
            WRITE(IOUT,1005)
            WRITE(IOUT,1006)
            CALL EXIT
          ENDIF
        ENDIF
C
      ELSEIF(IPROC.EQ.200) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ WW production '')')
        IF (RMASS(198)+RMASS(199)-2..GE.ECMS) THEN
          WRITE(IOUT,1005)
        ENDIF
      ELSEIF(IPROC.EQ.250) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ ZZ production '')')
        IF (RMASS(200)+RMASS(200)-2..GE.ECMS) THEN
          WRITE(IOUT,1005)
        ENDIF
      ELSEIF((IPROC.GE.300.AND.IPROC.LE.312).OR.IPROC.EQ.399) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ ZH production '')')
        IF (RMASS(200)+RMASS(201)-2..GE.ECMS) THEN
          WRITE(IOUT,1005)
        ENDIF
      ELSEIF((IPROC.GE.400.AND.IPROC.LE.412).OR.IPROC.EQ.499) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ Hll production '')')
        IF (RMASS(201)-2..GE.ECMS) THEN
          WRITE(IOUT,1005)
        ENDIF
      ELSEIF(IPROC.GE.500.AND.IPROC.LT.510) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ Note, 2 photon production'',
     &   '' QPM'')')
      ELSEIF(IPROC.EQ.510) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ Note, 2 photon production'',
     &   '' WW'')')
      ELSEIF(IPROC.GE.550.AND.IPROC.LT.559) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ gamma W production'')')
      ELSEIF(IPROC.EQ.1500) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ Note, 2 photon production'',
     &   '' double resolved'')')
      ELSEIF(IPROC.GE.1704.AND.IPROC.LE.1706) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ Note, 2 photon production'',
     &   '' QCD heavy flavour'')')
      ELSEIF(IPROC.GE.5000.AND.IPROC.LE.5206) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ Note, 2 photon production'',
     &   '' single resolved'')')
      ELSEIF(IPROC.EQ.8000) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ Note, 2 photon production'',
     &   '' VDM'')')
      ELSEIF(IPROC.GE.9000.AND.IPROC.LT.9145) THEN
        WRITE(IOUT,'('' +++ASKUSI+++ Note, 2 photon production'',
     &   '' with DIS'')')
        IDIS = IPROC
        WRITE(IOUT,'(''              Equal tagging now implemented'')')
      ELSE
        WRITE(IOUT,1000)IPROC
 1000   FORMAT(1X,'+++ASKUSI+++ Wrong process identifier IPROC = ',I5)
          WRITE(IOUT,1006)
        CALL EXIT
      ENDIF
      IF(NOSOFT.GT.0) WRITE(IOUT,
     & '('' +++ASKUSI+++ Soft underlying event is suppressed'')')
      IPROC = IPROC + 10000*NOSOFT
C
C we define ALEPH defaults for some more standard model parameters
C lamda-QCD, Weinberg-angle, width of the Z0
C AST 22.4.98 not any more !
C     QCDLAM = 0.18D0
C     SWEIN = 0.2293D0
C     GAMZ = 2.56D0
C
C here the user can change more standard model parameters
C
      JGPAR = NLINK('GGSW',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) QCDLAM = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) NFLAV = IW(JGPAR+2)
        IF (NDAT.GE.3) GAMZ = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) SWEIN = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) SCABI = DBLE(RW(JGPAR+5))
      ENDIF
C
C here the user can change B-decay parameter and choices
C
      JGPAR = NLINK('GBDE',0)
      IIBDEC = 1
C
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) B1LIM = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) THEN
          BDECAY = CHAINT(IW(JGPAR+2))
          DO I=3,1,-1
            IF(BDECAY.EQ.CHBDEC(I)) IIBDEC=I
          ENDDO
        ENDIF
      ENDIF
C
C choose B mixing and mass and width differences
      JGPAR = NLINK('GBMI',0)
C
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) MIXING = IW(JGPAR+1).EQ.1
        IF (NDAT.GE.2) XMIX(1) = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) XMIX(2) = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) YMIX(1) = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) YMIX(2) = DBLE(RW(JGPAR+5))
      ENDIF
C
C Here the user can set the max lifetime for an unstable particle or
C if it outside a specified volume
      JGPAR = NLINK('GMLT',0)
      MAXDKL=.FALSE.
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF(NDAT.GE.1) PLTCUT = DBLE(RW(JGPAR+1))
        IF(NDAT.GE.2) MAXDKL = IW(JGPAR+2).EQ.1
        IF(NDAT.GE.3) PRVTX  = IW(JGPAR+3).EQ.1
      ENDIF
C here the user can change the most important
C parameters for the cluster fragmentation
C
      JGPAR = NLINK('GPRM',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) CLMAX = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) CLPOW = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) PSPLT = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) THMAX = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) VQCUT = DBLE(RW(JGPAR+5))
        IF (NDAT.GE.6) VGCUT = DBLE(RW(JGPAR+6))
        IF (NDAT.GE.7) QDIQK = DBLE(RW(JGPAR+7))
        IF (NDAT.GE.8) PDIQK = DBLE(RW(JGPAR+8))
        IF (NDAT.GE.9) IOPREM = IW(JGPAR+9)
      ENDIF
C
C Now some further parameters controlling the program
C
C     RAZSOF = 1.
C     RAZSPI = 1.
      JGPAR = NLINK('GHRC',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) THEN
          IF (RW(JGPAR+1).EQ.0.) THEN
            AZSOFT = .FALSE.
            RAZSOF = 0.D0
          ENDIF
        ENDIF
        IF (NDAT.GE.2) THEN
        IF (RW(JGPAR+2).EQ.0.) THEN
            AZSPIN = .FALSE.
            RAZSPI = 0.D0
          ENDIF
        ENDIF
        IF (AZSOFT) RAZSOF = 1.D0
        IF (AZSPIN) RAZSPI = 1.D0
        IF (NDAT.GE.3) CLDIR = IW(JGPAR+3)
        IF (NDAT.GE.4) CLSMR = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) BTCLM = DBLE(RW(JGPAR+5))
        IF (NDAT.GE.6) ETAMIX = DBLE(RW(JGPAR+6))
        IF (NDAT.GE.7) PRSOF  = DBLE(RW(JGPAR+7))
      ENDIF
C
C Here the user can specifiy various gamma gamma
C options.
C
C For QPM we have certain cuts
      JGPAR = NLINK('GQPM',0)
      IF (JGPAR.NE.0) THEN
        NDAT=IW(JGPAR)
        IF (NDAT.GE.1) EMMIN = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) EMMAX = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) PTMIN = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) PTMAX = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) CTMAX = DBLE(RW(JGPAR+5))
      ENDIF
C Parameters governing Equivalent Photon or WW Approximations
      JGPAR = NLINK('GGEN',0)
      IF (JGPAR.NE.0) THEN
        NDAT=IW(JGPAR)
        IF (NDAT.GE.1) Q2WWMN = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) Q2WWMX = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) YBMIN  = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) YBMAX  = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) YWWMIN = DBLE(RW(JGPAR+5))
        IF (NDAT.GE.6) YWWMAX = DBLE(RW(JGPAR+6))
      ENDIF
      JGPAR = NLINK('GGQ2',0)
      IF (JGPAR.NE.0) THEN
         NDAT=IW(JGPAR)
         IF (NDAT.GE.1) WHMIN = DBLE(RW(JGPAR+1))
         IF (NDAT.GE.2) Q2MIN = DBLE(RW(JGPAR+2))
         IF (NDAT.GE.3) Q2MAX = DBLE(RW(JGPAR+3))
         IF (NDAT.GE.4) PHOMAS = DBLE(RW(JGPAR+4))
         IF (NDAT.GE.5) ISPAC = IW(JGPAR+5)
         IF (NDAT.GE.6) QSPAC = DBLE(RW(JGPAR+6))
      ENDIF
C Author group and set from PDFLIB for each photon
      modpdf(1)=-1
      modpdf(2)=-1
      JGPAR = NLINK('GPDF',0)
      IF (JGPAR.NE.0) THEN
        NDAT=IW(JGPAR)
        IF (NDAT.GE.2) THEN
          PDFGROUP1 = IW(JGPAR+1)
          IF(PDFGROUP1.GT.0.AND.PDFGROUP1.LE.8) THEN
            AUTPDF(1) = CGGPDF(PDFGROUP1)
            MODPDF(1) = IW(JGPAR+2)
          ELSE
            WRITE(IOUT,1007) PDFGROUP1
          ENDIF
        ENDIF
        IF (NDAT.GE.4) THEN
          PDFGROUP2 = IW(JGPAR+3)
          IF(PDFGROUP2.GT.0.AND.PDFGROUP2.LE.8) THEN
            AUTPDF(2) = CGGPDF(PDFGROUP2)
            IF (PHOMAS.GT.0..AND.(PDFGROUP1.GT.7 .AND.
     1                            PDFGROUP2.GT.7)) THEN
              WRITE(IOUT,*)'ERROR - PHOMAS GT 0 FOR P2 DEPENDANT S.F.',
     1                   'PHOMAS SET TO 0'
              PHOMAS = 0.
            ENDIF
            MODPDF(2) = IW(JGPAR+4)
          ELSE
            WRITE(IOUT,1007) PDFGROUP2
          ENDIF
        ENDIF
      ENDIF
C
C Beam Polarizations
C
      JGPAR = NLINK('GPOL',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) EPOLN(1) = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) EPOLN(2) = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) EPOLN(3) = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) PPOLN(1) = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) PPOLN(2) = DBLE(RW(JGPAR+5))
        IF (NDAT.GE.6) PPOLN(3) = DBLE(RW(JGPAR+6))
      ENDIF
C
C change here the parameters governing final state radiation
C
      JGPAR = NLINK('GFSR',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) VPCUT = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) ALPFAC = DBLE(RW(JGPAR+2))
      ENDIF
C
C change here the parameters governing initial state radiation
C
      JGPAR = NLINK('GISR',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) TMNISR = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) ZMXISR = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) COLISR = IW(JGPAR+3).eq.1
      ENDIF
C
C Sudakov form factor options
C
      LWSUD = 0
      LRSUD = 0
      JGPAR = NLINK('GSUD',0)
C
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) SUDORD = IW(JGPAR+1)
        IF (NDAT.GE.2) LWSUD  = IW(JGPAR+2)
        IF (NDAT.GE.3) LRSUD  = IW(JGPAR+3)
      ENDIF
C
C W and Z decay options
C
      JGPAR = NLINK('GHMB',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF(NDAT.GE.1) THEN
          DO I=1,NDAT
            MODBOS(I) = IW(JGPAR+I)
          ENDDO
        ENDIF
      ENDIF
C
C Allow some relative weight parameters to vary
C
      JGPAR = NLINK('GHWT',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) SNGWT = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) DECWT = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) REPWT(0,1,0) = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) REPWT(0,2,0) = DBLE(RW(JGPAR+4))
        IF(NDAT.GT.12) NDAT=12
        IF(NDAT.GT.4) THEN
          DO I=5,NDAT
            PWT(I-4) = DBLE(RW(JGPAR+I))
          ENDDO
        ENDIF
      ENDIF
C
C Colour rearrangement model
C
      JGPAR = NLINK('GCLR',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) CLRECO = IW(JGPAR+1).EQ.1
        IF (NDAT.GE.2) PRECO  = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) EXAG   = DBLE(RW(JGPAR+3))
      ENDIF
C finally we redefine some steering parameters of the
C HERWIG program:
C don't print out event listings
C increase number of possible HERWIG errors
C
      MAXPR = 0
      MAXER = 100
      IPROCI = IPROC-10000*nosoft
      IF ((IPROCI.gt.500).and.(IPROCI.ne.550)) MAXER = MAX(MAXER,IMAXER)
        WRITE ( 6,1010) maxer
 1010     FORMAT(1X,'+++ASKUSI+++ Maximum error limit >100 set to ',I5)
C
      IDEBB = 0
      IDEBE = 0
C
C switch off time check in HERWIG
C
      TLOUT = 0.D0
C
C the user can debug specific events with the
C data card DEBU
C
      NAMI = NAMIND('DEBU')
C
      JDEBU = IW(NAMI)
C
      IF(JDEBU.NE.0) THEN
C
        NDAT = IW(JDEBU)
C
        IF (NDAT.EQ.1) THEN
          IDEBB = IW(JDEBU+1)
          IDEBE = IDEBB
        ENDIF
C
        IF (NDAT.EQ.2) THEN
          IDEBB = IW(JDEBU+1)
          IDEBE = IW(JDEBU+2)
        ENDIF
C
      ENDIF
C
C user can reset parameters at
C this point by data cards, otherwise values
C set in HWIGIN/HWUINC will be used.
C
C stop smearing of primary vertex (done in ASKUSE)
      PIPSMR = .FALSE.
C    Make possible a special tuning to deal with b quarks and
C    lighter quarks differently. In that case, quark masses should
C    stay at their default values, so drop any GMAS related to them !
      IFLB = 0
      PSPB = PSPLT
      JGPAR = NLINK('GBFR',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) QCDLAM = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) CLMAX  = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) CLSMR  = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) PSPLT  = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) PSPB   = DBLE(RW(JGPAR+5))
        IF (NDAT.GE.6) DECWT  = DBLE(RW(JGPAR+6))
        IF (NDAT.GE.7) then
            GLUMA  = DBLE(RW(JGPAR+7))
            RMASS(13) = GLUMA
        ENDIF
        IF (NDAT.GE.8) PRECO  = DBLE(RW(JGPAR+8))
        IF (PRECO.gt.0.D0) CLRECO = .TRUE.
        IF (NDAT.GE.9) VMIN2 = DBLE(RW(JGPAR+9))
        IFLB = 1
C   drop any modification to quark masses ( except top )
        do iq = 1,5
           JGMAS = NLINK('GMAS',IQ)
           IF ( JGMAS.gt.0) MNUM = NDROP('GMAS',IQ)
        enddo
C   remove any mod to gluon mass
        JGMAS = NLINK('GMAS',13)
        IF ( JGMAS.gt.0) MNUM = NDROP('GMAS',13)
      ENDIF

C
C the user can define different values with the data cards GMAS
C the masses of the t-mesons, t-baryons and the diquarks should
C be redefined after the call to HWUINC
C
      NAMI = NAMIND ('GMAS')
C
      IF (IW(NAMI).NE.0) THEN
C
        JGMAS = NAMI + 1
C
C loop over all GMAS banks
C
   5    JGMAS = IW(JGMAS-1)
        IF (JGMAS.EQ. 0) GOTO 6
C
        MNUM = IW(JGMAS-2)
        IF ((MNUM.LT.109.OR.MNUM.GT.120).AND.
     &      (MNUM.LT.232.OR.MNUM.GT.244).AND.
     &      (MNUM.LT.255.OR.MNUM.GT.264)) THEN
          RMASS(MNUM) = RW(JGMAS+1)
          INPART = IW(NAPAR)
          IPART = KGPART(MNUM)
          IANTI = ITABL(INPART,IPART,JPARAN)
          RW(KROW(INPART,IPART)+JPARMA) = RW(JGMAS+1)
          IF (IANTI.NE.IPART) RW(KROW(INPART,IANTI)+JPARMA)=RW(JGMAS+1)
        ENDIF
        GOTO 5
C
   6    CONTINUE
C
      ENDIF
C
C create the KLIN bank and complete the PART  bank
C
C Deactivate D-wave mesons (this removes the isospin violation bug):
C Important: these 3 lines have to come BEFORE call HWUINC !
      do  JHWC = 363, 387
         VTOCDK(JHWC) = .true.
      end  do
C compute parameter-dependent constants
C HWUINC overwrites the masses of the t-mesons, t-baryons
C and the diquarks
C
      CALL HWUINC
      IF ( IFLB.EQ.1) THEN
        PSPB = 1./PSPB
        PSPSAV = PSPLT
        CLSSAV = CLSMR
        WRITE(IOUT,102) 1./PSPSAV,CLSSAV,1./PSPB,zero,VMIN2
 102  FORMAT('  ASKUSI : You will ne using a modified version of HERWIG'
     $,/,3x,'         PSLPT          CLSMR',/,3x,'   udsc :',2G10.4
     $,/,3x,'    b   :',2G10.4
     $,/,3x,' VMIN**2 in Gev**2 ',G10.4)
      ENDIF
C
C reset the overwritten masses back to the ALEPH values
C (if available)
C
      RMASS(232) = DBLE(PARMAS(140))
      RMASS(233) = DBLE(PARMAS(133))
      RMASS(234) = DBLE(PARMAS(131))
      RMASS(235) = DBLE(PARMAS(135))
      RMASS(236) = DBLE(PARMAS(239))
      RMASS(237) = DBLE(PARMAS(271))
      RMASS(238) = DBLE(PARMAS(243))
C
      RMASS(241) = DBLE(PARMAS(249))
      RMASS(242) = DBLE(PARMAS(137))
      RMASS(243) = DBLE(PARMAS(139))
C
      RMASS(255) = DBLE(PARMAS(134))
      RMASS(256) = DBLE(PARMAS(132))
      RMASS(257) = DBLE(PARMAS(136))
      RMASS(258) = DBLE(PARMAS(240))
      RMASS(259) = DBLE(PARMAS(272))
      RMASS(260) = DBLE(PARMAS(244))
C
      RMASS(263) = DBLE(PARMAS(250))
      RMASS(264) = DBLE(PARMAS(138))
C
C now we we have to set the user defined masses of the
C t-mesons, t-baryons and diquarks
C
      NAMI = NAMIND ('GMAS')
C
      IF (IW(NAMI).NE.0) THEN
C
        JGMAS = NAMI + 1
C
C loop over all GMAS banks
C
  15    JGMAS = IW(JGMAS-1)
        IF (JGMAS.EQ. 0) GOTO 16
C
        MNUM = IW(JGMAS-2)
        IF ((MNUM.GE.109.AND.MNUM.LE.120).OR.
     &      (MNUM.GE.232.AND.MNUM.LE.244).OR.
     &      (MNUM.GE.255.AND.MNUM.LE.264)) THEN
          RMASS(MNUM) = RW(JGMAS+1)
          INPART = IW(NAPAR)
          IPART = KGPART(MNUM)
          RW(KROW(INPART,IPART)+JPARMA) = RW(JGMAS+1)
        ENDIF
        GOTO 15
C
  16    CONTINUE
C
      ENDIF
      WRITE(IOUT,103) (RMASS(iq),iq=1,6),RMASS(13)
 103  FORMAT (' Masses to be used : d         u         s         c    '
     &,'      b         t       gluon',/, '                    ',7G10.3)
C
C the user can set any particle stable
C with the data cards GSTA
C
      NAMI = NAMIND ('GSTA')
C
      IF (IW(NAMI).NE.0) THEN
C
        JGSTA = NAMI + 1
C
C loop over all GSTA banks
C
 105    JGSTA = IW(JGSTA-1)
        IF (JGSTA.EQ. 0) GOTO 106
C
        MNUM = IW(JGSTA-2)
C
        IF (CHAINT(IW(JGSTA+1)).EQ.'OFF'.AND.NMODES(MNUM).NE.0)
     &    CALL HWUSTA(RNAME(MNUM))
        IF (CHAINT(IW(JGSTA+1)).EQ.'ON'.AND.NMODES(MNUM).EQ.0)
     &    WRITE(IOUT,1001)MNUM
 1001     FORMAT(1X,'+++ASKUSI+++ Herwig particle # = ',I5,
     &           ' no decay modes available - left stable')
C
        GOTO 105
C
 106    CONTINUE
C
      ENDIF
C
C get particles which should not be decayed by the generator
C
      MXDEC = KNODEC (HEDEC,HEDE)
      MXDEC = MIN (MXDEC,HEDE)
C
C now set them stable, unless the user has provided a data
C card for them
C
      DO 200 IPART = 1,MXDEC
C
C look if data card is present
C
         JGSTA = NLINK('GSTA',HEDEC(IPART))
C
C if not, inhibit decay
C
         IF (JGSTA.LE.0) THEN
           IF (HEDEC(IPART).GT.0.AND.NMODES(HEDEC(IPART)).NE.0)
     &       CALL HWUSTA(RNAME(HEDEC(IPART)))
         ENDIF
C
  200 CONTINUE
C
      IF(IHARD.EQ.1.OR.IHARD.EQ.2) THEN
C
        WRITE(IOUT,*) '+++ASKUSI+++ LUND OPTION NO LONGER AVAILABLE'
        WRITE(IOUT,1006)
        CALL EXIT
      ELSEIF (IHARD.EQ.3) THEN
        IF(IPROC.LT.107) THEN
          WEAKPAR(1)=RMASS(121)
          WEAKPAR(3)=RMASS(200)
          WEAKPAR(4)=GAMZ
          WEAKPAR(5)=SWEIN
          IF(IFL.GT.0) THEN
            WEAKPAR(2)=RMASS(IFL)
            WEAKPAR(11)=ICHRG(IFL)
          ENDIF
          EBEAM=PBEAM1
          IOUTDY=IOUT
          CALL HWIDY3(IFL,NFLAV)
        ELSE
          WRITE(IOUT,*) '+++ASKUSI+++ Process ',IPROC, ' is ',
     &     'meaningless with DYMU02'
          IHARD=0
        ENDIF
      ENDIF
C
C book default histograms
C
      CALL UHRBK
C
C user's initial calculations, redefine any other parameter in HERWIG
C
      CALL USTART
C
C initialise elementary process (Needed for DYMU02 as well)
C
      CALL HWEINI
C
      SDVRT(1) = 0.035
      SDVRT(2) = 0.0012
      SDVRT(3) = 1.28
C
      JSVRT = NLINK('SVRT',0)
C
      IF(JSVRT.NE.0) THEN
        SDVRT(1) = RW(JSVRT+1)
        SDVRT(2) = RW(JSVRT+2)
        SDVRT(3) = RW(JSVRT+3)
      ENDIF
C   get an offset for position of interaction point
C   if needed get a smearing on this position
C   XVRT    x      y      z    ( sz    sy    sz)
C
      call vzero(XVRT,3)
      CALL VZERO(SXVRT,3)
      IFVRT = 0
      NAXVRT=NAMIND('XVRT')
      JXVRT=IW(NAXVRT)
      IF (JXVRT.NE.0) THEN
         IFVRT = 1
         XVRT(1)=RW(JXVRT+1)
         XVRT(2)=RW(JXVRT+2)
         XVRT(3)=RW(JXVRT+3)
         IF ( IW(JXVRT).gt.3) then
            IFVRT = 2
            SXVRT(1)=RW(JXVRT+4)
            SXVRT(2)=RW(JXVRT+5)
            SXVRT(3)=RW(JXVRT+6)
         ENDIF
      ENDIF
C
C reset event counters
C
      DO 120 I = 1,10
        NEVENT(I) = 0
  120 CONTINUE
C
C reset flavour counters
      DO 130 I = 1,MAXFLA
        IFLC(I) = 0
  130 CONTINUE
C
C  Fill the KPAR bank with the generator parameters
C AST extend, adding GAMZ, VPCUT, B1LIM and IIBDEC (Bdecay choice)
C AST also add CLPOW, ALPFAC and SUDORD
C
      TABL(1) = SNGL(PBEAM1)
      TABL(2) = FLOAT(IPROC)
      TABL(3) = SNGL(RMASS(6))
      TABL(4) = SNGL(RMASS(13))
      TABL(5) = SNGL(RMASS(199))
      TABL(6) = SNGL(RMASS(200))
      TABL(7) = SNGL(RMASS(201))
      TABL(8) = SNGL(QCDLAM)
      TABL(9) = FLOAT(NFLAV)
      TABL(10) = SNGL(GAMZ)
      TABL(11) = SNGL(SWEIN)
      TABL(12) = SNGL(SCABI)
      TABL(13) = RAZSOF
      TABL(14) = RAZSPI
      TABL(15) = SNGL(CLMAX)
      TABL(16) = SNGL(CLPOW)
      TABL(17) = SNGL(PSPLT)
      TABL(18) = SNGL(THMAX)
      TABL(19) = SNGL(VQCUT)
      TABL(20) = SNGL(VGCUT)
      TABL(21) = SNGL(VPCUT)
      TABL(22) = SNGL(QDIQK)
      TABL(23) = SNGL(PDIQK)
      TABL(24) = SNGL(B1LIM)
      TABL(25) = FLOAT(IIBDEC)
      TABL(26) = SNGL(ALPFAC)
      TABL(27) = FLOAT(SUDORD)
      TABL(28) = SDVRT(1)
      TABL(29) = SDVRT(2)
      TABL(30) = SDVRT(3)
      TABL(31) = FLOAT(IHARD)
      TABL(32) = FLOAT(CLDIR)
      TABL(33) = SNGL(CLSMR)
      TABL(34) = SNGL(BTCLM)
      TABL(35) = SNGL(ETAMIX)
      TABL(36) = SNGL(EPOLN(1))
      TABL(37) = SNGL(EPOLN(2))
      TABL(38) = SNGL(EPOLN(3))
      TABL(39) = SNGL(PPOLN(1))
      TABL(40) = SNGL(PPOLN(2))
      TABL(41) = SNGL(PPOLN(3))
      TABL(42) = SNGL(PSPB)
      TABL(43) = XVRT(1)
      TABL(44) = XVRT(2)
      TABL(45) = XVRT(3)
      TABL(46) = sXVRT(1)
      TABL(47) = sXVRT(2)
      TABL(48) = sXVRT(3)
      TABL(49) = float(iver)
C
      NROW = 1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
C
C  Fill RLEP bank
C
       IEBEAM = NINT(SNGL(PBEAM1) *1000  )
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
C
C print KPAR and RLEP banks
C
      CALL PRTABL('RLEP',0)
      CALL PRTABL('KPAR',0)
C
C  Print PART and KLIN bank
C
      IF (IPPART.EQ.1) CALL PRPART
C
      RETURN
 1005 FORMAT(1X,'+++ASKUSI+++ Beam energy low for selected ',
     &           'process, possible inefficiencies')
 1006 FORMAT(1X,'+++ASKUSI+++   --- stopping ---')
 1007 FORMAT(1X,'+++ASKUSI+++  Unknown PDF Author group',I5)
      END
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C
C --------------------------------------------------------------------
C Ebi Lange  December 1988.
C updated AST December 1993.
C --------------------------------------------------------------------
C
C common blocks for HERWIG
C
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
C
      DOUBLE PRECISION PSPB,PSPSAV,CLSSAV
      INTEGER IFLB
      COMMON /HEXTRAB/ PSPB,PSPSAV,CLSSAV,IFLB
      COMMON /HWRECO/ NRECO
      INTEGER NRECO
C Flags and counters for the Herwig interface
      INTEGER NCOL,MAXFLA
C
      PARAMETER (NCOL = 49)
      PARAMETER (MAXFLA=7)
C
      INTEGER NEVENT,IFLC,IOUT,IDEBB,IDEBE,IFVRT
      REAL SDVRT,TABL,XVRT,SXVRT
C
      COMMON / DTMILL / SDVRT(3),TABL(NCOL),NEVENT(10),IFLC(MAXFLA),
     &                  XVRT(3),sXVRT(3),IFVRT
      COMMON / INPOUT / IOUT
      COMMON / DTBUG / IDEBB,IDEBE
C flag for the initial process and whether DIS process
      INTEGER IHARD,IFL,IDIS
C
      COMMON /INIPRO/ IHARD,IFL,IDIS
C
      INTEGER ISEED(3),IND,KWGTBK
      INTEGER ITRK,IDPR,ISTA,IST,NVRT,NTRK,IMOTH,IDAUG
C
      REAL VRTEX(4)
      REAL RN1,RN2,RN3,DUM,RNDM,RXX,RYY,RZZ
      REAL ECMS,WEIT,WEIK
      LOGICAL KEEP
      real*4 amarset,amar
      external amarset,KWGTBK
C******MHL mods*********************************************************
      INTEGER IOPT,IDUM
      EXTERNAL RNDM
C*End**MHL mods*********************************************************
C
C store the current random number seeds
C
      CALL RDMOUT(ISEED)
C
C  Generate primary vertex
C
      CALL RANNOR (RN1,RN2)
      CALL RANNOR (RN3,DUM)
C
      VRTEX(1) = RN1*SDVRT(1)
      VRTEX(2) = RN2*SDVRT(2)
      VRTEX(3) = RN3*SDVRT(3)
      VRTEX(4) = 0.
      IF ( IFVRT.ge.2) then
         CALL RANNOR(RXX,RYY)
         CALL RANNOR(RZZ,DUM)
         VRTEX(1) = VRTEX(1) + RXX*SXVRT(1)
         VRTEX(2) = VRTEX(2) + RYY*SXVRT(2)
         VRTEX(3) = VRTEX(3) + RZZ*SXVRT(3)
      ENDIF
      IF ( IFVRT.ge.1) then
         VRTEX(1) = VRTEX(1) + XVRT(1)
         VRTEX(2) = VRTEX(2) + XVRT(2)
         VRTEX(3) = VRTEX(3) + XVRT(3)
      ENDIF

C
C Event generation
C
C***********************************************************************
C.. MHL MODS
C
C.. Switch beams for DIS gamma gamma half the time
C
C      IF ( (IPROC.GE.9000.AND.IPROC.LE. 9006) .OR.
C     +     (IPROC.GE.19000.AND.IPROC.LE. 19006) ) THEN
       IF(IDIS.GT.0) THEN
         DUM = RNDM(DUM)
         IOPT = 3

         IF (DUM.LT.0.5) THEN
C
C.. E- TAG
C
            PART1 = 'E-      '
            PART2 = 'E+      '
            CALL HWUIDT(IOPT,IDUM,IPART1,PART1)
            CALL HWUIDT(IOPT,IDUM,IPART2,PART2)
            PBEAM1 = EBEAM1
            PBEAM2 = EBEAM1
         ELSE
C
C.. E+ TAG
C
            PART1 = 'E+      '
            PART2 = 'E-      '
            PBEAM1 = -EBEAM1
            PBEAM2 = -EBEAM1
            CALL HWUIDT(IOPT,IDUM,IPART1,PART1)
            CALL HWUIDT(IOPT,IDUM,IPART2,PART2)
         ENDIF
      ENDIF
C
C.. END MHL mods
C***********************************************************************
C initialise event
C
      NRECO = 0
      CALL HWUINE
C
C generate hard subprocess, check LUND is not used
C
      IF(IHARD.NE.0.AND.IHARD.NE.3) THEN
        WRITE(6,*) 'WARNING ihard is now illegal',ihard
        IHARD=0
      ENDIF
      IF (IHARD.EQ.0) THEN
        CALL HWEPRO
      ELSEIF(IHARD.EQ.3) THEN
        CALL HWEDY3
      ENDIF
C
C find flavour, look for first parton
C
      IF(IPROC.LT.150) THEN
      DO 10 ITRK = 1,NHEP
        IF (ISTHEP(ITRK).EQ.113) THEN
          IDPR = ABS(IDHEP(ITRK))
          GOTO 20
        ENDIF
   10 CONTINUE
C
   20 CONTINUE
      IF(IDPR.EQ.21) IDPR=7
C
      IF(IDPR.LE.7) IFLC(IDPR) = IFLC(IDPR) + 1
      ENDIF
C   switch to second random sequence for cascade generation
      amar = amarset(1)
C
C generate parton cascades
C
      CALL HWBGEN
C
C Modify fragmentation parameters for b if needed
      IF ( IFLB.EQ.1) THEN
         IF ( IDPR.eq.5)  then
            CLSMR = 0.D0
            PSPLT = PSPB
         endif
      ENDIF
C
C do heavy quark decays
C
      CALL HWDHQK
C
C do cluster formation
C
      CALL HWCFOR
C
C do cluster decays
C
      CALL HWCDEC
C
C do unstable particle decays
C
      CALL HWDHAD
C
C do heavy flavour decays
C
      CALL HWDHVY
C
C add soft underlying event if required
C
      CALL HWMEVT
C
C finish event
C
      CALL HWUFNE
C
      NEVENT(1) = NEVENT(1) + 1
C
      IF (IFLB.EQ.1) THEN
         IF ( IDPR.eq.5) then
            CLSMR = CLSSAV
            PSPLT = PSPSAV
         endif
      ENDIF
      IF (IERROR.EQ.0) ISTAT = 101
C selection for gamma-gamma events
      CALL AMCSEL(KEEP)
      IF(.NOT.KEEP) ISTAT = 99
C
C do we want to print the event ?
C
      IF (NEVHEP.GE.IDEBB.AND.NEVHEP.LE.IDEBE) CALL HWUEPR
C
      IF(ISTAT.GE.101) THEN
        ISTA = 0
      ELSE
        ISTA = -99
        NEVENT(6) = NEVENT(6) + 1
        GOTO 97
      ENDIF
C
      ECMS = SNGL(PBEAM1 + PBEAM2)
C
      WEIT = 1.
C
      IST = 0
C
      CALL KXHEAL(VRTEX,IST,NVRT,NTRK)
C
C    if color reconnection requested, store # of reconnection
C    in KWGT
      if (preco.gt.0.D0) then
         weik = float(NRECO)
         ind = kwgtbk(1,2000,weik)
      endif
C
      ISTA = IST
C
C here we have to do the loop over the possible KXHEAL errors
C
      IF (ISTA.EQ.-2) THEN
        NEVENT(3) = NEVENT(3) + 1
        GOTO 97
      ENDIF
C
      IF (ISTA.EQ.-5) THEN
        NEVENT(4) = NEVENT(4) + 1
        GOTO 97
      ENDIF
C
      IF (ISTA.GT.0) THEN
        NEVENT(7) = NEVENT(7) + 1
        GOTO 97
      ENDIF
C
   97 IF(ISTA.NE.0) THEN
C
C here we are if an error has occured
C count them and print out the random number if it is an interface error
C
        NEVENT(9) = NEVENT(9) + 1
        if(ista.ne.-99) WRITE(IOUT,1000)ISTA,ISEED
 1000   FORMAT(1X,'+++ASKUSE+++ Error occured status code = ',I5,/,1X,
     &            '             RMAR seeds = ',3(I15))
C
      ELSE
        NEVENT(10) = NEVENT(10) + 1
C
C user's event processing
C
        CALL USTORE
C
C fill default histograms
C
        CALL UHRFI
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE USCJOB
C
C --------------------------------------------------------------------
C Ebi Lange, December 1988.
C updated AST December 1993.
C --------------------------------------------------------------------
C
C common blocks for HERWIG
C
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
C
      COMMON /HWRECO/ NRECO
      INTEGER NRECO
C Flags and counters for the Herwig interface
      INTEGER NCOL,MAXFLA
C
      PARAMETER (NCOL = 49)
      PARAMETER (MAXFLA=7)
C
      INTEGER NEVENT,IFLC,IOUT,IDEBB,IDEBE,IFVRT
      REAL SDVRT,TABL,XVRT,SXVRT
C
      COMMON / DTMILL / SDVRT(3),TABL(NCOL),NEVENT(10),IFLC(MAXFLA),
     &                  XVRT(3),sXVRT(3),IFVRT
      COMMON / INPOUT / IOUT
      COMMON / DTBUG / IDEBB,IDEBE
C flag for the initial process and whether DIS process
      INTEGER IHARD,IFL,IDIS
C
      COMMON /INIPRO/ IHARD,IFL,IDIS
************************************************************************
      REAL ERWGT
************************************************************************
      CALL UGTSEC
C
C terminate elementary process
C
      IF (IHARD.EQ.0) CALL HWEFIN
      IF (IHARD.EQ.3) CALL DYMUND
C
C user's terminal calculations
C
      CALL USTOPP
C
C***********************************************************************
C... MHL MODS
C
C     IF ( (IPROC.GE.9000.AND.IPROC.LE. 9006) .OR.
C    +     (IPROC.GE.19000.AND.IPROC.LE. 19006)) THEN
      IF(IDIS.GT.0) THEN
C
C.. Recalculate the error on c-s as in HWEFIN
C
         IF (NWGTS.GT.0) THEN
            ERWGT = SQRT(MAX(WSQSUM/FLOAT(NWGTS)-AVWGT**2,ZERO))
            ERWGT = ERWGT*SQRT(1./FLOAT(NWGTS))
         ELSE
            ERWGT = 0.
         ENDIF
C
         WRITE(IOUT,100) 2000*AVWGT,2000*ERWGT
C
  100 FORMAT(10X,'!!!!!!!!! GAMMA GAMMA ALERT !!!!!!!!!',
     &      /10X,'* THE ABOVE CROSS SECTION SHOULD BE *',
     &      /10X,'* MULTIPLIED BY A FACTOR OF 2, IE   *',
     &      /10X,'* CROSS SECTION (PB) = ',G12.4,' *',
     &      /10X,'* ERROR IN C-S  (PB) = ',G12.4,' *',
     &      /10X,'!!!!!!!!!     END ALERT     !!!!!!!!!')
      ENDIF
C
C.. End MHL mods
C***********************************************************************
      WRITE(IOUT,101)
C
  101 FORMAT(//20X,'EVENTS STATISTICS',
     &        /20X,'*****************')
C
      WRITE(IOUT,102)NEVENT(1),NEVENT(10),NEVENT(9)
C
  102 FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,
     &       /5X,'# OF ACCEPTED  EVENTS                = ',I10,
     &       /5X,'# OF REJECTED  EVENTS                = ',I10)
C
      IF(IPROC.LT.150) THEN
        WRITE(IOUT,103)
C
  103   FORMAT(//20X,'GENERATED FLAVOURS',
     &        /20X,'******************')
C
        WRITE(IOUT,104)IFLC(2),IFLC(1),IFLC(3),IFLC(4),IFLC(5),IFLC(6)
     &  ,IFLC(7)
C
  104   FORMAT(/5X,'# OF UUBAR EVENTS                    = ',I10,
     &       /5X,'# OF DDBAR EVENTS                    = ',I10,
     &       /5X,'# OF SSBAR EVENTS                    = ',I10,
     &       /5X,'# OF CCBAR EVENTS                    = ',I10,
     &       /5X,'# OF BBBAR EVENTS                    = ',I10,
     &       /5X,'# OF TTBAR EVENTS                    = ',I10,
     &       /5X,'# OF GGBAR EVENTS                    = ',I10)
      ENDIF
C
      WRITE(IOUT,110)
C
  110 FORMAT(//20X,'ERRORS STATISTICS',
     &        /20X,'*****************')
C
      WRITE(IOUT,111)NEVENT(3),NEVENT(4),
     &               NEVENT(6),NEVENT(7)
C
  111 FORMAT(/10X,'ISTA =  -2 BOS ERROR VERT/KINE     # OF REJECT = ',
     & I10,
     &       /10X,'ISTA =  -5 INTERFACE ERROR         # OF REJECT = ',
     & I10,
     &       /10X,'ISTA = -99 GENERATOR ERROR         # OF REJECT = ',
     & I10,
     &       /10X,'ISTA >   0 UNKNOWN PART            # OF REJECT = ',
     & I10,/,/)
C
      RETURN
      END
      SUBROUTINE USTART
C
C user's routine for initialization
C
C----------------------------------------------------------------------
C
      RETURN
  999 END
      SUBROUTINE USTOPP
C
C user's routine for terminal calculations, histogram output, etc
C
C----------------------------------------------------------------------
C
      RETURN
  999 END
      SUBROUTINE USTORE
C
C user's routine to process data from event
C
C----------------------------------------------------------------------
C
      RETURN
C
  999 END
       SUBROUTINE KXHEPA (JPART,JKLIN)
C --------------------------------------------------
C - B.Bloch - 870300      modified by F.Ranjard - 870423
C - arranged for HERWIG by E. Lange - 881214
C - updated by E. Lange for new PART bank - 890910
C - updated by A.S.Thompson to use same codes as KXL7PA
C   and removing call to KXHETO - 960828
C
C  fill 'PART' bank with HERWIG particles
C  Get  the NOtracking marker word NOTRK from KRUN bank
C  Fill KLIN bank with HERWIG particle# which correspond
C       to GEANT particles
C  Get  HERWIG particles and transfer them to PART bank
C       with a GEANT# and a tracking type set to NOTRK
C       because they are not used by GEANT.
C  Reduce PART and KLIN banks to their normal size
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXHEPA
C              External References: NAMIND(BOS77)
C                                   KGPART/KBKLIN/KBPART/KXLUTO/AUBPRS
C                                   (ALEPHLIB)
C                                   IUCOMP(CENLIB)
C              Comdecks referenced: BCS, BMACRO,KMACRO
C
C - Usage    : CALL KXHEPA (JPART,JKLIN)
C - Output   : JPART   = KBPART return flag
C                        .gt. 0 means OK
C              JKLIN   = KBKLIN return flag
C                        .gt. 0 means OK
C
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C some declarations in KMACRO
      INTEGER NAPAR,JPA,NLINK
      REAL PARMAS,TIMLIF
C
      REAL ELEP,DMAS
C
      PARAMETER (ELEP=190.)
      PARAMETER (DMAS=0.)
C     ILUGE (LLUGE) are the LUND numbers corresponding to the first
C                   part of PART bank ( used by GEANT)
C     ILUAL (LLUAL) are the LUND numbers corresponding to the rest of
C                   the PART bank
C
      INTEGER LLUGE,LLUAL,LASTP,PDGCODE,IH
C
      PARAMETER (LLUGE=52, LLUAL=315)
      PARAMETER (LASTP=393)
C
      INTEGER ILUGE(LLUGE), ILUAL(LLUAL)
      INTEGER JSTORE(LASTP)
      INTEGER JPART,JKLIN
      INTEGER I,KNOTR,IPART,MYP,IUCOMP,MPART
      INTEGER KBKLIN,KBPART,KMPART,KNOTRK
C
      REAL ZMAS,CHAR,TLIF
C
      INTEGER NAMIND
      EXTERNAL NAMIND
C
      CHARACTER*12 TNAM
C
      DATA ILUGE /22,-11,11,0,-13,13,111,211,-211,130,321,-321,2112,
     +           2212,-2212,310,221,3122,3222,3212,3112,
     +           3322,3312,3334,-2112,-3122,-3222,-3212,-3112,
     +           -3322,-3312,-3334,-15,15,411,-411,421,-421,
     +           431,-431,4122,24,-24,23,8*0/
C
      DATA ILUAL /-4122,25,551,311,-311,12,-12,14,-14,16,-16,20213,
     &-20213,20113,10221,10111,331,10441,20443,445,443,440,441,213,-213,
     &323,-323,313,-313,423,-423,413,-413,433,-433,113,223,333,10551,
     &20553,555,553,0,661,10661,20663,665,21,2,1,3,4,5,6,-2,-1,-3,-4,-5,
     &-6,663,0,-521,521,511,-511,531,-531,-541,541,-523,523,513,-513,
     &533,-533,-543,543,621,-621,611,-611,631,-631,641,-641,
     &651,-651,623,-623,613,-613,
     &633,-633,643,-643,653,-653,2224,-2224,2214,-2214,2114,-2114,1114,
     &-1114,3224,-3224,3214,-3214,3114,-3114,3324,-3324,3314,-3314,4222,
     &-4222,4212,-4212,4112,-4112,4322,-4322,4312,-4312,4332,-4332,4232,
     &-4232,4132,-4132,4224,-4224,4214,-4214,4114,-4114,4324,-4324,4314,
     &-4314,4334,-4334,4422,-4422,4412,-4412,4432,-4432,4424,-4424,4414,
     &-4414,4434,-4434,4444,-4444,5222,-5222,5212,-5212,5112,-5112,5322,
     &-5322,5312,-5312,5332,-5332,5242,-5242,5142,-5142,5342,-5342,5442,
     &-5442,5522,-5522,5512,-5512,5532,-5532,5542,-5542,6222,-6222,6212,
     &-6212,6112,-6112,6232,-6232,6132,-6132,6332,-6332,6242,-6242,6142,
     &-6142,6342,-6342,6442,-6442,5122,-5122,5232,-5232,5132,-5132,5422,
     &-5422,5412,-5412,5432,-5432,6122,-6122,6322,-6322,6312,-6312,6422,
     &-6422,6412,-6412,6432,-6432,5224,-5224,5214,-5214,5114,-5114,5324,
     &-5324,5314,-5314,5334,-5334,5424,-5424,5414,-5414,5434,-5434,5444,
     &-5444,6224,-6224,6214,-6214,6114,-6114,6324,-6324,6314,-6314,6334,
     &-6334,6424,-6424,6414,-6414,6434,-6434,6444,-6444,6524,-6524,6514,
     &-6514,6534,-6534,6544,-6544,6554,-6554,6252,-6252,6152,-6152,6352,
     &-6352,6452,-6452,6552,-6552,6522,-6522,6512,-6512,6532,-6532,6542,
     &-6542,5524,-5524,5514,-5514,5534,-5534,5544,-5544,5554,-5554,   0,
     & 0,5*0/
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
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)
C
C
      DATA NAPAR/0/
C
C ------------------------------------------------------
C
      IF (NAPAR.EQ.0) NAPAR = NAMIND('PART')
C
      DO 100 I = 1,LASTP
        JSTORE(I) = 0
  100 CONTINUE
C
C NOtrack marker word stored in KRUN bank
C
      KNOTRK = ITABL (IW(NAMIND('KRUN')),1,2)
C
C Fill KLIN with HERWIG particle# for the GEANT particles
C adjust masses and lifetimes to standard ALEPH values
C
      DO IPART = 1,LLUGE
        PDGCODE = ILUGE(IPART)
          IF (PDGCODE.NE.0) THEN
          IH = IUCOMP(PDGCODE,IDPDG(1),LASTP)
          IF(IH.GT.0) THEN
            JKLIN = KBKLIN (IPART,IH)
            IF (JKLIN .LE. 0) GOTO 998
            RMASS(IH) = DBLE(PARMAS(IPART))
            RLTIM(IH) = DBLE(TIMLIF(IPART))
            JSTORE(IH)=1
          ENDIF
        ENDIF
      ENDDO
C
C extent the KLIN bank
C adjust masses and lifetimes to standard ALEPH values
C
      DO IPART = 1,LLUAL
        PDGCODE = ILUAL(IPART)
        IF(PDGCODE.NE.0) THEN
          IH = IUCOMP(PDGCODE,IDPDG(1),LASTP)
          IF(IH.GT.0) THEN
            JKLIN = KBKLIN(IPART+LLUGE,IH)
            IF (JKLIN .LE. 0) GOTO 998
            RMASS(IH) = DBLE(PARMAS(IPART+LLUGE))
            RLTIM(IH) = DBLE(TIMLIF(IPART+LLUGE))
            JSTORE(IH)=1
          ENDIF
        ENDIF
      ENDDO
C
C Get HERWIG particles and transfer them to PART
C if their mass is in the ELEP energy range
C these particles are not tracked so their GEANT#
C and tracking type are set to KNOTRK
C
      DO 1000 MYP = 1,LASTP
        IF (JSTORE(MYP).NE.0) GOTO 1000
        TNAM = RNAME(MYP)
        IF (TNAM .EQ. '    ') GOTO 1000
        ZMAS = SNGL(RMASS (MYP))
        IF (ZMAS.GT.ELEP) GOTO 1000
C
C take special care for charge of diquarks
C
        IF (MYP.GE.109.AND.MYP.LE.120) THEN
          CHAR = FLOAT(ICHRG(MYP))/3.
        ELSE
          CHAR = FLOAT(ICHRG(MYP))
        ENDIF
C
        TLIF = RLTIM(MYP)
C
C store the new particle# JPART
C
        JPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,CHAR,TLIF)
        IF (JPART.LE.0) GOTO 998
        JSTORE(MYP) = 1
C
C store the user generator particle# of the new particle
C
        JKLIN = KBKLIN (JPART,MYP)
        IF (JKLIN.LE.0) GOTO 998
C
C do the same for the antiparticle except if identical
C
        PDGCODE = -IDPDG(MYP)
        IH = IUCOMP(PDGCODE,IDPDG(1),LASTP)
        IF (IH.EQ.0) THEN
          MPART = KMPART(JPART,DMAS,JPART)
          IF (MPART.LE.0) GOTO 998
        ELSE
          TNAM = RNAME(IH)
          JPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,-CHAR,TLIF)
          IF (JPART.LE.0) GOTO 998

          JSTORE(IH) = 1
          MPART = KMPART(JPART,DMAS,JPART-1)
          IF (MPART.LE.0) GOTO 998
          MPART = KMPART(JPART-1,DMAS,JPART)
          IF (MPART.LE.0) GOTO 998
          JKLIN = KBKLIN (JPART,IH)
          IF (JKLIN.LE.0) GOTO 998
        ENDIF
C
 1000 CONTINUE
C
      CALL AUBPRS ('PARTKLIN')
C
      GOTO 999
C
C not enough space
C
 998  CONTINUE
C
      WRITE (IW(6),'(/1X,''+++ KXHEPA+++ not enough space for PART''
     &         ,'' or KLIN bank'')')
C
C End
C
 999  CONTINUE
C
      END
      SUBROUTINE KXHEAL(VMAIN,ISTATU,MVX,MTRK)
C ---------------------------------------------------------
C - B.Bloch-Devaux - J.Boucrot - F.Ranjard - 870516
C - arranged for HERWIG  E. Lange  891801
C   B.Bloch :modified to allow for change from Energy to mass in
C   KINE banks (April 89)
C - A.S.Thompson :modified February 1992 for quark radiation in HERWIG
C - A.S.Thompson :modified August 1992 to handle partons at the end of
C                                      the shower
C - A.S.Thompson :modified March 1993 for slight change in status codes
C - A.S.Thompson :modified December 1993 for additional processes
C - B.Bloch Sept 2000 : modified to prevent B0's to have two vertices
C - Fill    : PTRAK(ix,n)  = px,py,pz,E( or Mass from Alephlib 9.0) of
C                            track(n)
C                            if E or M=0.it will be filled by the system
C             IPVNU(1,n)   = origin vertex # of track(n)
C                  (2,n)   = decay vertex # of track(n)
C                             0 if no decay
C                  (3,n)   = ALEPH particle #
C             IPCOD(n)     = status and history code of track(n)
C                          = KS*1000000 + KH
C                            KS = HERWIG status code
C                            KH = history code
C                                 for particles = mother track imoth
C                                 for clusters 1000*imoth1+imoth2
C - Book    : KHIS bank filled with IPCOD(n)
C - Call    : KFEVBK (VMAIN,PTRAK,IPVNU,MTRK,JSTAT)
C             to book propagate the decay and fill VERT and KINE
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXHEAL
C              External References: ALTABL/ALVERS/KFEVBK(ALEPHLIB)
C              Comdecks referenced: BCS, HETRCK
C
C - usage   : CALL KXHEAL (VMAIN,ISTATU,MVX,MTRK)
C - Input   : VMAIN = vx,vy,vz,tof of the primary vertex
C - Output  : ISTATU = status word ( = 0 means OK)
C                     - 2 means not enough space for VERT or KINE
C                     - 5 means interface error
C                     > 0 means unknown HERWIG particle# ISTATU
C             MVX   = # of vertices
C             MTRK  = # of tracks to be propagated ( no beam electrons )
C
C ------------------------------------------------------
C
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C some declarations in KMACRO
      INTEGER NAPAR,JPA,NLINK
      REAL PARMAS,TIMLIF
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      REAL PTRAK(4,NMXHEP),VMAIN(4)
      REAL TLIF
      REAL TLIMI
      PARAMETER(TLIMI = 1.E-15)
C
      INTEGER IPVNU(3,NMXHEP),IPCOD(NMXHEP)
      INTEGER IPVAL(2,NMXHEP),NEWTR(NMXHEP)
      INTEGER ALTABL
      INTEGER JP1,JP2,I,IT,MVX,MTRK
      INTEGER IST,NPARL,INOTR,NVER,ITRK,ITR,IHER,IPART,KS
      INTEGER ISTATU,IMOTH,IMOTH2,IGRAND,KSMOTH,KSGRAN,IFAIL,JKHIS
      INTEGER KGPART,KBKINE,NAMIND,NDAU
      LOGICAL LHVDEC,LCLUST,LPAJET,LBMIX
C
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
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)
C

C
      LPAJET(I) = I.EQ.141.OR.I.EQ.142.OR.I.EQ.143.OR.I.EQ.144
      LHVDEC(I) = I.EQ.196.OR.I.EQ.197.OR.I.EQ.199.OR.I.EQ.200
      LCLUST(I) = I.GE.183.AND.I.LE.186
      LBMIX(I,L) = (I.eq.200).and.(L.eq.1) ! mixing of B0's
C
      DATA NAPAR/0/
C
C initialization
C
      JP1 = 0
      JP2 = 0
      IST = 0
C
      IF (NAPAR.EQ.0) NAPAR = NAMIND('PART')
C
C Build array containing vertex # and particle # of each track
C
      NPARL = 0
      INOTR = 0
      NVER = 1
C
      DO 10 ITRK = 1,NHEP
C
        ITR = ITRK - INOTR
C
C get ALEPH particle#
C
        IHER = IDHW(ITRK)
        IPART = KGPART(IHER)
C
        IF (IPART.LE.0) GOTO 998
C
C store momenta
C
        DO 9 I = 1,3
          PTRAK(I,ITR) = SNGL(PHEP(I,ITRK))
   9    CONTINUE
C
C KBKINE needs the mass of the particles instead of the energy
C
        PTRAK(4,ITR) = SNGL(PHEP(5,ITRK))
C
C store new line number
C
        NEWTR(ITRK) = ITR
C
        IPVNU(3,ITR) = IPART
        ipcod(itr) = 0
        KS = ISTHEP(ITRK)
        IMOTH = JMOHEP(1,ITRK)
        IF(IMOTH.GT.0) THEN
          KSMOTH=ISTHEP(IMOTH)
          if(imoth.lt.itrk) IPCOD(ITR) = NEWTR(IMOTH)
          IGRAND=JMOHEP(1,IMOTH)
          IF(IGRAND.GT.0) KSGRAN=ISTHEP(IGRAND)
        ENDIF
C just in case this is the mother of an initial state track
        IPVAL(2,ITRK)=1
C
C now we have look for different status codes to decide what
C we want to do
C
        IF(KS.EQ.3.OR.KS.EQ.100.OR.KS.EQ.121.OR.KS.EQ.122) THEN
C special HERWIG objects - also virtual photons from 2-photon events
C we just skip them
C
          INOTR = INOTR + 1
C
        ELSEIF (KS.EQ.101.OR.KS.EQ.102) THEN
C
C beam electrons are stored with negative numbers in the KINE bank
C
          IST = KBKINE(-ITRK,PTRAK(1,ITR),IPART,0)
C
          IF (IST.LE.0) THEN
            IHER = -2
            GOTO 998
          ENDIF
C
          INOTR = INOTR + 1
C just in case this is the mother of an initial state track
          IPVAL(2,ITRK)=1
C
        ELSEIF (KS.EQ.103.OR.KS.EQ.110) THEN
C
C Normally just the mass frame, but use it as the main vertex if it has
C daughters
C
          IF(JDAHEP(1,ITRK)*JDAHEP(2,ITRK).NE.0) THEN
            IPVNU(1,ITR) = 1
            IPVNU(2,ITR) = IPVNU(1,ITR)
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
            NEWTR(ITRK) = ITRK - INOTR
            IPCOD(ITR) = 0
            KS = 120
            NPARL = NPARL + 1
          ELSE
            INOTR = INOTR + 1
          ENDIF
        ELSEIF (KS.EQ.120) THEN
C
C this should be the Z0/gamma or W+,W-
C
C could also be the CMF in 2-photon events
          if((iher.lt.198.or.iher.gt.201).and.
     &       (iher.ne.14.and.iher.ne.15)) then
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ found illegal exchange''
     &       ,'' boson : HERWIG particle# = '',I5,'' in event ''
     &       ,2I6)')IHER,NEVHEP,KS
            ISTATU= -5
            GOTO 999
          ENDIF
C
C create primary vertex
C
          IPVNU(1,ITR) = 1
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C define new track number
C and store in KINE bank
C
          NEWTR(ITRK) = ITRK - INOTR
          IPCOD(ITR) = 0
C
          NPARL = NPARL + 1
C
        ELSEIF (KS.EQ.170) THEN
C
C could be the CMF in 2-photon events
          if(iher.ne.16) then
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ found illegal exchange''
     &         ,'' boson : HERWIG particle# = '',I5,'' in event ''
     &         ,2I6)')IHER,NEVHEP,KS
            ISTATU= -5
            GOTO 999
          ENDIF
          if(ksgran.eq.3.and.idhw(igrand).eq.59) then
C
C create primary vertex
C
            IPVNU(1,ITR) = 1
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C define new track number
C and store as through a HARD object
C
            NEWTR(ITRK) = ITRK - INOTR
            IPCOD(ITR) = 0
            KS = 120
          else
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
          endif
C
          NPARL = NPARL + 1
C
        ELSEIF (KS.GE.123.AND.KS.LE.124) THEN
C
C outgoing partons will be linked to their mother
C
C they should come from the exchange boson or a heavy quark before decay
C Force a main vertex just in case
          if(ksmoth.eq.120) IPVAL(2,IMOTH) = 1
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C we don't keep them in the KINE bank
C AST we do now ! 15.01.1998
C         INOTR = INOTR + 1
          IPCOD(ITR) = NEWTR(IMOTH )
          NPARL = NPARL + 1
C
        ELSEIF(KS.EQ.125) THEN
C
C spectator parton after processing
C it should come from a decayed heavy flavour hadron
C
          IF (KSMOTH.NE.199) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ spectator parton, ''
     &         ,''ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C  we don't want to track it
C
          INOTR = INOTR + 1
C
        ELSEIF (LPAJET(KS)) THEN
C
C outgoing parton jets (KS = 141 - 144)
C they should come from their outgoing partons
C
          IF (KSMOTH+20.NE.KS) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
C and the grand should be the Z0/gamma or from a heavy quark
C before decay
C
C basically taus from ZZ,ZH events
          IF(KSGRAN.NE.120.AND.KSGRAN.NE.155.AND.KSGRAN.NE.195.AND.
     &     .NOT.LHVDEC(KSGRAN)) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = '',I3,
     &      '' has grandma, ISTHEP =  '',I3,'' in event '',I6)')
     &      KS,KSGRAN,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
C this must not be any particle which might be tracked by GEANT
C
          IF (IHER.GT.13) THEN
C could be a virtual photon
            if(iher.eq.59) then
C ignore it, but give a warning if it has daughters, this could compromi
C succeeding mother-daughter relationships
              INOTR=INOTR+1
              if(jdahep(1,i).gt.0.or.jdahep(2,i).gt.0)
     &        WRITE (IW(6),'(/1X,''+++KXHEAL+++ Virtual photon has'',
     &        '' daughters in event '',i6)') nevhep
            else
              WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = '',I3,
     &        '' is not a quark in event '',I6)') KS,NEVHEP
              ISTATU = -5
              GOTO 999
            ENDIF
          Else
C
C just possible that the mother had appeared before the grand for
C the hard process, force this to be the primary vertex anyway
C AST 15.01.1998 Now NOT if we keep their mothers
C            if(ksgran.eq.120) IPVAL(2,IMOTH) = 1
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C store them in the KINE bank
C KS = 123,124 is not stored in KINE, we have to link to the grands
C AST 15.01.1998 But link to their mothers if they ARE kept
            IPCOD(ITR) = NEWTR(IMOTH )
            NPARL = NPARL + 1
          endif
C
        ELSEIF (KS.EQ.161.OR.KS.EQ.162) then
C
C beam or target spectators after gluon splitting
C
          IF (KSMOTH+20.NE.KS) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C store them in the KINE bank
C

          NPARL = NPARL + 1
C
        ELSEIF(KS.EQ.155) THEN
C
C heavy quark or tau before decay
C
          IF (.NOT.(IHER.LE.12.OR.IHER.EQ.125.OR.IHER.EQ.131)) THEN
            WRITE(IW(6),'(/1X,''+++KXHEAL+++ heavy quark or tau ''
     &                 ,'' before decay has IHER = '',I3
     &                 ,'' in event '',I6)')IHER,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IF (KSMOTH.NE.199.AND.KSMOTH.NE.124.AND.KSMOTH.NE.123) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ part. ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  ''
     &         ,I3,'' in event '',I6)')KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
          NPARL = NPARL + 1
C
C update history code
C
          IF (KSMOTH.EQ.199) THEN
C
            IPCOD(ITR) = NEWTR(IMOTH)
C
          ELSE
C
C KS = 123,124 is not stored in KINE, we have to link to the grands
C
            IPCOD(ITR) = NEWTR(IGRAND)
C
          ENDIF
C
        elseif((ks.ge.157.and.ks.le.159).or.ks.eq.149.or.
     &                                      ks.eq.162) then
C
          IF (IHER.GT.13.AND.IHER.NE.59) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ particle from splitting''
     &      ,'' is not a parton IHER= '',I3,'' KS= '',I3
     &      ,'' in event '',I7)') IHER,KS,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IF (KS.EQ.157.AND..NOT.LPAJET(KSMOTH)) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ parton from ''
     &         ,'' QCD branching not from parton jet ''
     &         ,'' in event '',I6)')NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IF (KS.EQ.159.AND..NOT.LPAJET(KSMOTH)) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ parton from ''
     &         ,'' cluster splitting not from parton jet ''
     &         ,'' in event '',4I6)')NEVHEP,itr,ks,ksmoth
            ISTATU = -5
            GOTO 999
          ENDIF
C AST ignore FSR photons appearing here
          IF(IHER.EQ.59) THEN
            INOTR=INOTR+1
          else
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
            NPARL = NPARL + 1
C
C update history code
C
            IPCOD(ITR) = NEWTR(IMOTH)
          ENDIF
C
        ELSEIF (KS.EQ.160) THEN
C
C spectator after heavy decay, just link
C
          IF (.NOT.(IHER.LE.12.OR.(IHER.GE.109.AND.IHER.LE.120))) THEN
            WRITE(IW(6),'(/1X,''+++KXHEAL+++ spectator after ''
     &         ,''heavy decay is not a (di)quark, IHER = '',I3
     &         ,'' in event '',I6)')IHER,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
C it should come from a spectator parton
C
          IF (KSMOTH.NE.125) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ spectator after ''
     &         ,''heavy decay, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP = '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
          NPARL =  NPARL + 1
C
C update history code
C
          IF (KSGRAN.NE.199) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ quark, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSGRAN,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPCOD(ITR) = NEWTR(IGRAND)
C
        ELSEIF (KS.EQ.167.or.ks.eq.168) THEN
C
C clusters from beam or target
C just ignore if from virtual photon
C
          if(ksmoth.eq.3.and.idhw(imoth).eq.59) then
            inotr = inotr + 1
          else
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
            nparl = nparl + 1
          endif
C
        ELSEIF (KS.EQ.183.or.ks.eq.163.or.
     +        ((ks.eq.185.or.ks.eq.184).and.ksmoth.ne.170)) THEN
C
C this is a hard cluster or soft cluster from DIS event
C find it's partons first
C
          IMOTH2 = JMOHEP(2,ITRK)
C
C we check that the partons are coming from the same vertex
C
          IF (IPVAL(2,IMOTH).NE.IPVAL(2,IMOTH2).AND.
     &        INT(IPROC/10).ne.900) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ cluster particles''
     &         ,'' ITRK = '',I4,I4,'' of cluster ITRK = '',I4
     &         ,'' do not come from the same vertex in event ''
     &         ,I6)')JP1,JP2,ITRK,NEVHEP
            WRITE(IW(6),'(1X,''Vertices are'',2I4)') IPVAL(2,IMOTH),
     &       IPVAL(2,IMOTH2)
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
          NPARL = NPARL + 1
C
C update history, we may have two different mothers
C
          IPCOD(ITR) = NEWTR(IMOTH)*1000 + NEWTR(IMOTH2)
C
        ELSEIF (KS.EQ.184.or.ks.eq.185.OR.KS.EQ.186) THEN
C
C clusters from soft interactions
C
          IF (KSMOTH.NE.170) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ soft cluster''
     &         ,'' not from soft process, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C  we don't want to track it
C
C          INOTR = INOTR + 1
           nparl = nparl + 1
C
        ELSEIF (KS.GE.195.AND.KS.LE.200) THEN
C
C these are unstable leptons and hadrons
C link them, propagate if necessary
C
C direct unstable hadrons and decayed heavy flavour hadrons
C should come from the cluster
C
C         IF ((KS.EQ.196.OR.KS.EQ.197.OR.KS.EQ.199.OR.KS.EQ.200)
C    &    .AND.(KSMOTH.LT.183.OR.KSMOTH.GT.186).AND.KSMOTH.NE.197)THEN
          IF (LHVDEC(KS).AND.(.NOT.KSMOTH.EQ.198.AND.
     &    .NOT.LHVDEC(KSMOTH).AND..NOT.LCLUST(KSMOTH))) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ unstab. hadr. or ''
     &         ,''dec. heavy flav. hadr. not from clus. ''
     &         ,'' in event '',2I6)')NEVHEP
            write(6,*) 'track',itrk,' mother,grand ',ksmoth,ksgran
            ISTATU = -5
            GOTO 999
          ENDIF
C
C indirect unstable hadrons or leptons should come from direct
C or indirect unstable hadrons or direct unstable leptons
C
          IF (KS.EQ.198.AND.(KSMOTH.LT.195.OR.KSMOTH.GT.200)) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ indir unstab. hadr. ''
     &         ,'' or lept. not from dir. unstab. hadr.''
     &         ,'' in event '',I6)')NEVHEP
            write(6,*) 'track',itrk,' mother',ksmoth
            ISTATU = -5
            GOTO 999
          ENDIF
C
C direct unstable leptons should come from outgoing partons and their
C grandma should be a heavy quark before decay
C AST allow Z,H from initial process as well
C
          IF (KS.EQ.195.and.ksmoth.ne.110) THEN
            IF (KSMOTH.NE.123.AND.KSMOTH.NE.124.AND.KSMOTH.NE.120.AND.
     &          KSMOTH.NE.110) THEN
              WRITE (IW(6),'(/1X,''+++KXHEAL+++ dir unstab. lept. '',
     &         '' not from outgoing partons in event '',I6)')NEVHEP
              ISTATU = -5
              GOTO 999
            ENDIF
            IF (KSGRAN.NE.155.AND.KSGRAN.NE.120.AND.KSGRAN.NE.195.AND.
     &          KSGRAN.NE.3.AND.(IHER.LT.198.OR.IHER.GT.201)) THEN
C basically taus from ZZ,ZH AND GAMMA-GAMMA events
              WRITE (IW(6),'(/1X,''+++KXHEAL+++ dir unstab. lept. ''
     &           ,'' not from heavy quark before decay''
     &           ,'' in event '',I6)')NEVHEP
              ISTATU = -5
              GOTO 999
            ENDIF
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
C
          TLIF = TIMLIF(IPART)
          NDAU = JDAHEP(2,ITRK)-JDAHEP(1,ITRK)+1
          
C
          IF ((TLIF.GT.TLIMI).AND.(.not.LBMIX(KS,NDAU))) THEN
C
C these particles will be propagated
C
            NVER = NVER + 1
            IPVNU(2,ITR) = NVER
C
          ELSE
C
C these particles will decay immediately
C link them to the main vertex
C
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
          ENDIF
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
          NPARL = NPARL + 1
C
C update the history code
C for the direct unstable leptons (tau's) we link directly
C to the heavy quark before decay
C WE HAVE KEPT THESE HEAVY QUARKS NOW
C          IF (KS.EQ.195) THEN
C            IPCOD(ITR) = NEWTR(IGRAND)
C          ELSE
            IPCOD(ITR) = NEWTR(IMOTH)
C          ENDIF
C
        ELSEIF (KS.EQ.1) THEN
C
C final state particles, just link them
C
          IMOTH2= JMOHEP(2,ITRK)
C AST
          IF(IDHW(IMOTH2).LE.12.AND.IHER.EQ.59.AND.
     &     IDHW(IMOTH).EQ.59.AND.KSMOTH.EQ.157) THEN
C AST
C final state photons . drop them since they have appeared in the
C parton shower
            INOTR = INOTR + 1
          else
C spectator e+e- from 2-photon events
            if(isthep(imoth).eq.103.and.iher.eq.59) then
C           if((isthep(imoth).eq.103.and.iher.eq.59).or.
C    &        ((iher.eq.121.or.iher.eq.127.).and.
C    &        (isthep(imoth).eq.101.or.isthep(imoth).eq.102)))then
C AST
C initial state photons are linked to the main vertex
C
              IPVNU(1,ITR) = 1
C
            ELSE
C
C other particles are linked to the decay vertex of their mother
C
              IPVNU(1,ITR) = IPVAL(2,IMOTH)
C
            ENDIF
C
C no decay
C
            IPVNU(2,ITR) = 0
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
            NPARL = NPARL + 1
C
C update the history code
C
C           IF(KSMOTH.EQ.123.OR.KSMOTH.EQ.124) THEN
C
C leptons from heavy quarks
C WE HAVE KEPT THESE HEAVY QUARK NOW
C              IPCOD(ITR) = NEWTR(IGRAND)
C
            IF (KSMOTH.EQ.103.AND.IHER.EQ.59) THEN
C
C initial state photons are initial particles
C
              IPCOD(ITR) = 0
C
            ELSE
C
              IPCOD(ITR) = NEWTR(IMOTH)
C
            ENDIF
          ENDIF

C
        ELSEIF (KS.EQ.2) THEN
C
C AST parton before hadronization. Needed if work required at parton
C level. This can also apply to partons from heavy quark decay so
C give those partons whose greatgrandmother is the Z a 2nd mother
C
          IF(IHER.LE.13.OR.IHER.EQ.59) THEN
            IMOTH2= JMOHEP(2,ITRK)
            IPCOD(ITR) = NEWTR(IMOTH)
C
            IF (ISTHEP(JMOHEP(1,IGRAND)).EQ.120)
     &       IPCOD(ITR)=IPCOD(ITR)+1000*NEWTR(IGRAND)
C
C we link it to its mother
C
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
            IF(IHER.EQ.59) IPVNU(2,ITR)=0
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C  we don't want to track it
C
            NPARL = NPARL + 1
C non-partons
          else
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ particle in parton ''
     &      ,''shower is not a parton IHER= '',I3,'' KS= '',I3
     &      ,'' in event '',I7)') iher,ks,nevhep
            ISTATU = -5
            GOTO 999
          endif
C
        ELSE
C
          WRITE (IW(6),'(/1X,''+++KXHEAL+++ non interfaced''
     &         ,'' HERWIG status code found = '',I3,'' in event ''
     &         ,I6)') KS,NEVHEP
          ISTATU = -5
          GOTO 999
C
        ENDIF
C
C add status code
C
        IPCOD(ITR) = IPCOD(ITR) + KS*1000000
C
 10   CONTINUE
C
C Propagate decays and fill KINE and VERT banks
C
      CALL KFEVBK(VMAIN,PTRAK,IPVNU,NPARL,IFAIL)
C
C Fill history bank KHIS
C
      JKHIS = ALTABL ('KHIS',1,NPARL,IPCOD,'I','E')
C
      MVX = NVER
      MTRK = NPARL
      ISTATU = IFAIL
      if(ifail.ne.0) write(iw(6),*)'+++KXHEAL+++ KFEVBK error',ifail
      GOTO 999
C
C Error
C
 998  ISTATU = IHER
C
 999  RETURN
      END
      SUBROUTINE UHRBK
C --------------------------------------------------------------------
C Ebi Lange  February 1989.
C --------------------------------------------------------------------
C
      IMPLICIT NONE
C
C flag for the initial process and whether DIS process
      INTEGER IHARD,IFL,IDIS
C
      COMMON /INIPRO/ IHARD,IFL,IDIS
C
C book default histograms
C
      CALL HBOOK1(10010,'COS. POL. ANGLE OF QUARK',
     &            50,-1.,1.,0.)
      CALL HBOOK1(10011,'COS. POL. ANGLE OF ANTIQUARK',
     &            50,-1.,1.,0.)
      CALL HBOOK1(10020,'MULTIPLICITY',75,0.,150.,0.)
      CALL HBOOK1(10021,'CHARGED MULTIPLICITY',50,0.,100.,0.)
      CALL HBOOK1(10030,'MOMENTUM SPECTRUM',100,0.,50.,0.)
C
      CALL HBOOK1(10100,'MOMENTA OF RAD. PHOTONS',
     &              50,0.,50.,0.)
      CALL HBOOK1(10110,'COS. POL. ANGLE OF RAD. PHOTONS',
     &              50,-1.,1.,0.)
      CALL HBOOK1(10120,'Flavour depend. OF RAD. PHOTONS',
     &              10, 0.,10.,0.)
      CALL HBOOK1(10200,'MOMENTA OF INITIAL STATE PHOTONS',
     &          50,0.,50.,0.)
      CALL HBOOK1(10210,'COS POLAR ANGLE OF INITIAL STATE PHOTONS',
     &          50,-1.,1.,0.)
C
      RETURN
C
      END
      SUBROUTINE UHRFI
C --------------------------------------------------------------------
C Ebi Lange  February 1989.
C A.S.Thompson Modified February 1992
C --------------------------------------------------------------------
C
C fill default histograms
C
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      INTEGER NMUL,NCHMUL
      INTEGER IMOTH,ITRK,JANTI,IDPR,IDPR1
C
      REAL WEIT,COSTHE
      REAL PX,PY,PZ,PMOM
C
C initialize some counters
C
      NMUL = 0
      NCHMUL = 0
C
      WEIT = 1.
C
C loop over the tracks
C
      DO 100 ITRK = 1,NHEP
C
C look for primary quarks/antiquarks
C
        IF (ISTHEP(ITRK).EQ.123.OR.ISTHEP(ITRK).EQ.124) THEN
C
          IMOTH = JMOHEP(1,ITRK)
C
          IF (ISTHEP(IMOTH).EQ.120) THEN
C
C compute momentum and polar angle
C
            PX = SNGL(PHEP(1,ITRK))
            PY = SNGL(PHEP(2,ITRK))
            PZ = SNGL(PHEP(3,ITRK))
C
            PMOM = SQRT(PX*PX + PY*PY + PZ*PZ)
C
            IF (PMOM.GT.0) THEN
C
              COSTHE = PZ / PMOM
C
C quarks
C
              IF (IDHW(ITRK).GE.1.AND.IDHW(ITRK).LE.6)
     &          CALL HFILL(10010,COSTHE,0.,WEIT)
C
C antiquarks
C
              IF (IDHW(ITRK).GE.7.AND.IDHW(ITRK).LE.12)
     &          CALL HFILL(10011,COSTHE,0.,WEIT)
C
            ELSE
C
              WRITE (IW(6),'(/1X,''+++UHRFI+++ track with zero''
     &          ,'' zero momentum found in event '',I6)')NEVHEP
C
            ENDIF
C
          ENDIF
C
        ENDIF
C
        IF (ISTHEP(ITRK).EQ.1) THEN
C
C we have a stable particle
C
          NMUL = NMUL + 1
C
          IF (ICHRG(IDHW(ITRK)).NE.0) NCHMUL = NCHMUL +1
C
C get momenta
C
          PX = SNGL(PHEP(1,ITRK))
          PY = SNGL(PHEP(2,ITRK))
          PZ = SNGL(PHEP(3,ITRK))
C
C compute momentum
C
          PMOM = SQRT(PX*PX + PY*PY + PZ*PZ)
C
          CALL HFILL(10030,PMOM,0.,WEIT)
C
        ENDIF
C
C radiative photons
C AST
        if(isthep(itrk).eq.2.and.idhw(itrk).eq.59) then
C AST
          IMOTH = JMOHEP(1,ITRK)
          IDPR1= ABS(IDHW(IMOTH))
          IF(IDPR1.GT.6) IDPR1=IDPR1-6
          IF(IDPR1.EQ.1) THEN
            IDPR1=2
          ELSEIF(IDPR1.EQ.2) THEN
            IDPR1=1
          ENDIF
          PZ = SNGL(PHEP(3,ITRK))
          PMOM = SNGL(PHEP(4,ITRK))
          CALL HFILL(10100,PMOM,0.,1.)
          CALL HFILL(10120,FLOAT(IDPR1),0.,1.)
C
          IF (PMOM.GT.0) THEN
            COSTHE = PZ / PMOM
            CALL HFILL(10110,COSTHE,0.,1.)
          ELSE
            WRITE (IW(6),'(/1X,''+++UHRFI+++ rad. photon with zero''
     &        ,'' zero momentum found in event '',I6)')NEVHEP
          ENDIF
C
        ENDIF
C
C INITIAL STATE PHOTONS
        IF(IDHW(ITRK).EQ.59.AND.JMOHEP(1,ITRK).LE.3) THEN
          PZ = SNGL(PHEP(3,ITRK))
          PMOM = SNGL(PHEP(4,ITRK))
          CALL HFILL(10200,PMOM,0.,1.)
          IF(PMOM.GT.0.) THEN
            COSTHE=PZ/PMOM
            CALL HFILL(10210,COSTHE,0.,1.)
          ENDIF
        ENDIF
C
  100 CONTINUE
C
      CALL HFILL(10020,FLOAT(NMUL)+0.0001,0.,WEIT)
      CALL HFILL(10021,FLOAT(NCHMUL)+0.0001,0.,WEIT)
C
      RETURN
C
      END
      SUBROUTINE HWIDY3(ITYPE,NQUA)
C--------------------------------------------------------------------
C    A.S.Thompson    May   1992 Implementation of DYMU3 inside HRWG06
C      based on :-
C    B.Bloch-Devaux  April 1991 Implementation of DYMU3 inside HVFL02
C!  initialisation routine for DYMU3 generator
C
C     structure : subroutine
C
C     input     : ITYPE  type of quark to generate
C                        (note in HERWIG order)
C                 NQUA   no. of quark types
C     output    : none
C
C     Note      : Input parameters are set by ASKUSI in HRWG06
C--------------------------------------------------------------------
C
CC *CA HWUPDT
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
C commons to pass information to DYMU2
      REAL*4 ALFA,PI,ALFA1,SOLD,FINEXP,POIDS,XK0,
     + AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI,S0,EBEAM,
     + TAUV,CPTAU,HEL,PITAU,WEAKC,XSECT,XTOT,
     + SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY
      INTEGER ID2,ID3,INTERF,IDEBU , NEVT, IOUT
      COMMON /CONST/ ALFA,PI,ALFA1
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      COMMON /BEAM/ S0,EBEAM
      COMMON /TAU / TAUV,CPTAU,HEL,PITAU(4)
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT
      COMMON/ZDUMP / IDEBU , NEVT, IOUT
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY
C      REAL*8 RMASS,BFRAC,CMMOM,ETAMIX
      real*4 qcdfac,wei
      integer iseed,iexpo,nevma,itype,nqua,i1,i2,i3,ii,kev
      DIMENSION ISEED(3)
      INTEGER ALTABL
      XTOT = 0.
C
C      DYMU3 PARAMETERS     DEFAULT VALUES
C
      SOLD = -1.
      ID2 = 0
      ID3 = 0
      IEXPO = 1
      FINEXP =-1.
      POIDS = 1.
      INTERF = 0
      IDEBU = 0
      NEVT = 0
      TAUV = 0.
      NEVMA = 5000
C                         K0 MINIMUM HARD PHOTON ENERGY/EBEAM
      XK0 = 0.003
C                         WEAK ISOSPIN AND CHARGE OF OUTGOING FERMION
      QCDFAC = 1.04
C
      AEL = 0.511E-3
      T3     = SIGN(0.5,QI)
      COL    = 3.*QCDFAC
C    Quarks
      IF ( ITYPE.EQ.0)  THEN
C    Save initial seeds to start the real events
        CALL RDMOUT(ISEED)
        I1 = ISEED(1)
        I2 = ISEED(2)
        I3 = ISEED(3)
C    Quarks mixture
        DO 15  II=1,6
 15     XSECT(II)=0.
C   No DEBUG needed here!
        IDEBU = 0
        DO 6 II = 1,NQUA
          NEVT = 0
          QI     = FLOAT(ICHRG(II))/3.
          T3     = SIGN(0.5,QI)
          COL    = 3.*QCDFAC
          AMU    = RMASS(II)
          CALL DYMUIN(II)
C    COPY coupling constants for this type in an array
          CALL UCOPY(AEL,WEAKC(1,II),11)
          DO 66 KEV=1,NEVMA
            CALL DYMUS(WEI)
  66      CONTINUE
          CALL FINISH(II,NEVMA)
C   Store cross-sections for each process
          XSECT(II) = SIGTOT
  6     CONTINUE
C   Store total cross-section
        XTOT = XSECT(1)
        DO 75 II = 2,NQUA
  75    XTOT = XTOT+XSECT(II)
        DO 7 II=2,NQUA
  7     XSECT(II) = XSECT(II-1)+XSECT(II)
C     Normalize
        DO 8 II = 1,NQUA
  8     XSECT(II) = XSECT(II)/XSECT(NQUA)
        WRITE(IOUT,100)   XSECT
 100    FORMAT('       INTEGRATED CROSS SECTIONS FOR QUARKS ',
     $           /,9X,6F10.4)
C   Restore initial seed
        CALL RMARIN(I1,I2,I3)
      ENDIF
C
C       INITIALIZE DYMU3
C
      CALL DYMUIN(ITYPE)
C
      RETURN
      END
      SUBROUTINE DYMUIN(NTYP)
C-----------------------------------------------------------------------
C    B.Bloch -Devaux APRIL 1991
C         ORIGINAL VERSION OF DYMU3 AS PROVIDED BY J.E.Campagne
C                       June 1989
C  This is a subset of original subroutine INIRUN to compute secondary
C  quantities according to requested final state.
C    A.S.Thompson   MAY 1992. adapted to run under HERWIG
C-----------------------------------------------------------------------
C*CA HWHARD
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
C commons to pass information to DYMU2
      REAL*4 ALFA,PI,ALFA1,SOLD,FINEXP,POIDS,XK0,
     + AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI,S0,EBEAM,
     + TAUV,CPTAU,HEL,PITAU,WEAKC,XSECT,XTOT,
     + SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY
      INTEGER ID2,ID3,INTERF,IDEBU , NEVT, IOUT
      COMMON /CONST/ ALFA,PI,ALFA1
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      COMMON /BEAM/ S0,EBEAM
      COMMON /TAU / TAUV,CPTAU,HEL,PITAU(4)
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT
      COMMON/ZDUMP / IDEBU , NEVT, IOUT
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY
      COMMON / VECLAB / PFP(4),PFM(4),GAP(4),GAM(4),GAF(4)
      REAL*4 PFP,PFM,GAP,GAM,GAF

C the last two include declarations for standard include files
      COMMON /COUNTS/ SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2
      COMMON /EVTS/ NEVT1,NEVT2,NFWD,NBKW
      REAL*8 SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2
      REAL*4 ECMS,CV,CA,CVPRI,CAPRI,DUM,XXXX,RNDM,WE,PBEA
      INTEGER ITYP,NTYP,NTYPO,ISEED,NEVT1,NEVT2,NFWD,NBKW,I
C
      DIMENSION PBEA(4)
      DIMENSION ISEED(3)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST = .FALSE.
        WRITE(IOUT,*)'*****************************************'
        WRITE(IOUT,*)'*         WELCOME    TO    DYMU3        *'
        WRITE(IOUT,*)'*                                       *'
        WRITE(IOUT,*)'*         AUTHORS: J.E.CAMPAGNE         *'
        WRITE(IOUT,*)'*                  R.ZITOUN             *'
        WRITE(IOUT,*)'*                                       *'
        WRITE(IOUT,*)'*         19 nov  89 RELEASE            *'
        WRITE(IOUT,*)'*                                       *'
        WRITE(IOUT,*)'*****************************************'
        WRITE(IOUT,*)' '
C
      ENDIF
C
      ITYP = NTYP
      NTYPO = ITYP
C
       WRITE(IOUT,1533) ITYP
 1533 FORMAT(1X,'**************************************************'/
     X      ,1X,'* D Y M U 3    A D A P T E D   T O   K I N G A L *'/
     X      ,1X,'* GENERATING FERMION TYPE:',I3,'    ( HERWIG 5.8 )  *'/
     &      ,1X,'**************************************************')
*---- BEAM ENERGY
*
      ECMS = 2.*EBEAM
      S0 = 4.*EBEAM**2
*
*----  COUPLING CONSTANTS
*
      CV    = (-1.+4.*SW2)/4./SQRT(SW2*(1.-SW2))
      CA    = -1./4./SQRT(SW2*(1.-SW2))
      CVPRI = (-2*T3/QI+4.*SW2)/4./SQRT(SW2*(1.-SW2))
      CAPRI = -T3/QI/2./SQRT(SW2*(1.-SW2))
      CV2 = CVPRI*CV
      CA2 = CAPRI*CA
      CA2CV2 = ( CV**2+CA**2 )*( CVPRI**2+CAPRI**2 )
      CALL DYMUSI
*
*---- CONST1
*
      ALFA  = 1./137.036
      PI    = 3.14159265
      ALFA1 = ALFA/PI
*
*----
*
      WRITE(IOUT,*)'*************************************************'
      WRITE(IOUT,*)'*     RUN PARAMETERS FOR RUN',ITYP
      WRITE(IOUT,*)'*************************************************'
      WRITE(IOUT,1000) AMZ,GAMM,SW2
      WRITE(IOUT,1003) ECMS,EBEAM
 1000   FORMAT('     Z MASS =',F8.3,' GEV ,      Z WIDTH =',F6.3,
     &         ' GEV ,  SIN2 TETA =',F7.4)
 1003   FORMAT(' CMS ENERGY =',F8.3,' GEV ,  BEAM ENERGY =',F8.3)
*
      IF(POIDS.EQ.1)THEN
        WRITE(IOUT,*)'UNWEIGHTED EVENTS'
      ELSE
        WRITE(IOUT,*)'WEIGHTED EVENTS'
      ENDIF
      WRITE(IOUT,*)'INITIAL STATE EXPONENTIATION'
      IF(ZMXISR.GT.0.) THEN
        ZMXISR = 0.
        WRITE(IOUT,*)'HERWIG ISR HAS BEEN TURNED OFF'
      ENDIF
      IF(FINEXP.EQ.1)THEN
        WRITE(IOUT,*)'FINAL STATE EXPONENTIATION'
      ELSE IF(FINEXP.EQ.0) THEN
        WRITE(IOUT,*)'NO FINAL STATE EXPONENTIATION.'
      ELSE IF(FINEXP.EQ.-1) THEN
        WRITE(IOUT,*)'NO FINAL STATE PHOTON'
      ENDIF
      IF(ID2.EQ.1)THEN
        WRITE(IOUT,*)'DII IN D(X)'
      ELSE
        WRITE(IOUT,*)'DII NOT IN D(X)'
      ENDIF
      IF(ID3.EQ.1)THEN
        WRITE(IOUT,*)'DIII IN D(X)'
      ELSE
        WRITE(IOUT,*)'DIII NOT IN D(X)'
      ENDIF
      IF(INTERF.EQ.0)THEN
        WRITE(IOUT,*)'NO INTERFERENCE'
      ELSE
        WRITE(IOUT,*)'INTERFERENCE WITH K0 =',XK0
      ENDIF
*
      CALL RDMOUT(ISEED)
      WRITE(IOUT,*)'INITIAL SEEDS ARE',ISEED
      WRITE(IOUT,*)'*************************************************'
*
*---- SET TO ZERO
*
      SIG    = 0.
      SIG2   = 0.
      SECFWD = 0.
      SECBKW = 0.
      SCFWD2 = 0.
      SCBKW2 = 0.
      NEVT1  = 0
      NEVT2  = 0
      NFWD   = 0
      NBKW   = 0
C
      RETURN
C ----------------------------------------------------------------------
C
      ENTRY HWEDY3
C
      IF (NTYPO.EQ.0 ) THEN
C   Decide which flavor to generate
        XXXX = RNDM(DUM)
        DO 30 I=1,6
          IF (XXXX.LT.XSECT(I)) GO TO 40
  30    CONTINUE
  40    ITYP = I
C   Copy corresponding coupling constants
        CALL UCOPY(WEAKC(1,I),AEL,11)
        CALL DYMUSI
      ENDIF
C
      CALL DYMUS (WE)
      NEVT = NEVT+1
C                              HERWIG INTERFACE
      CALL DYTOHW(ITYP)
C
  100 CONTINUE
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY DYMUND
C
      CALL FINISH(NTYPO,NEVT)
C
      RETURN
      END
      SUBROUTINE DYTOHW(ITYP)
C --------------------------------------------------------------------
C A.S.Thompson   May 1992
C
C fill HERWIG event common with DYMU3 qqbar(gamma)
C --------------------------------------------------------------------
C
C*CA HWDEFI
C*CA HEPEVT
C*CA HWBEAM
C*CA HWEVNT
C*CA HWHARD
C*CA HWPROC
C*CA HWUPDT
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
      COMMON / VECLAB / PFP(4),PFM(4),GAP(4),GAM(4),GAF(4)
      REAL*4 PFP,PFM,GAP,GAM,GAF

C the last two include declarations for standard include files
C
      INTEGER ITYP,NZ0,ICA
      REAL*8 PMULT(4)
      DATA PMULT/-1.,-1.,-1.,1./
C
C   set up main vertex
C
      do ica=1,4
        VTXPIP(ica) = 0.
      enddo
C
C set up initial state
C
      NHEP=1
      ISTHEP(NHEP)=101
      PHEP(1,NHEP)=0.
      PHEP(2,NHEP)=0.
      PHEP(3,NHEP)=PBEAM1
      PHEP(4,NHEP)=EBEAM1
      PHEP(5,NHEP)=RMASS(IPART1)
      JMOHEP(1,NHEP)=0
      JMOHEP(2,NHEP)=0
      JDAHEP(1,NHEP)=0
      JDAHEP(2,NHEP)=0
      IDHW(NHEP)=IPART1
      IDHEP(NHEP)=IDPDG(IPART1)
      NHEP=NHEP+1
      ISTHEP(NHEP)=102
      PHEP(1,NHEP)=0.
      PHEP(2,NHEP)=0.
      PHEP(3,NHEP)=-PBEAM2
      PHEP(4,NHEP)=EBEAM2
      PHEP(5,NHEP)=RMASS(IPART2)
      JMOHEP(1,NHEP)=0
      JMOHEP(2,NHEP)=0
      JDAHEP(1,NHEP)=0
      JDAHEP(2,NHEP)=0
      IDHW(NHEP)=IPART2
      IDHEP(NHEP)=IDPDG(IPART2)
C---NEXT ENTRY IS OVERALL CM FRAME
      NHEP=NHEP+1
      IDHW(NHEP)=14
      IDHEP(NHEP)=0
      ISTHEP(NHEP)=103
      JMOHEP(1,NHEP)=NHEP-2
      JMOHEP(2,NHEP)=NHEP-1
      JDAHEP(1,NHEP)=0
      JDAHEP(2,NHEP)=0
      CALL HWVSUM(4,PHEP(1,NHEP-1),PHEP(1,NHEP-2),PHEP(1,NHEP))
      CALL HWUMAS(PHEP(1,NHEP))
      EMSCA=PHEP(5,NHEP)
C
C radiative photon
C
      IF (GAP(4).GT.0.001) THEN
        NHEP = NHEP + 1
        IDHW(NHEP) = 59
        IDHEP(NHEP) = IDPDG(IDHW(NHEP))
        ISTHEP(NHEP) = 1
        JMOHEP(1,NHEP) = 3
        JMOHEP(2,NHEP) = 3
        JDAHEP(1,NHEP) = 0
        JDAHEP(2,NHEP) = 0
        DO 310 ICA = 1,4
          PHEP(ICA,NHEP) = DBLE(GAP(ICA))
  310   CONTINUE
        PHEP(5,NHEP) = 0.
      ENDIF
C
C radiative photon
C
      IF (GAM(4).GT.0.001) THEN
        NHEP = NHEP + 1
        IDHW(NHEP) = 59
        IDHEP(NHEP) = IDPDG(IDHW(NHEP))
        ISTHEP(NHEP) = 1
        JMOHEP(1,NHEP) = 3
        JMOHEP(2,NHEP) = 3
        JDAHEP(1,NHEP) = 0
        JDAHEP(2,NHEP) = 0
        DO 315 ICA = 1,4
          PHEP(ICA,NHEP) = DBLE(GAM(ICA))
  315   CONTINUE
        PHEP(5,NHEP) = 0.
      ENDIF
C the Z0
C
      NHEP = NHEP + 1
      NZ0 = NHEP
      IDHW(NHEP) = 200
      IDHEP(NHEP) = IDPDG(IDHW(NHEP))
      ISTHEP(NHEP) = 110
      JMOHEP(1,NHEP) = 1
      JMOHEP(2,NHEP) = 2
      JDAHEP(1,NHEP) = NHEP + 1
      JDAHEP(2,NHEP) = NHEP + 2
      PHEP(5,NHEP)=0.
      DO 320 ICA = 1,4
        PHEP(ICA,NHEP) = DBLE(PFP(ICA)+PFM(ICA))
        PHEP(5,NHEP)=PHEP(5,NHEP)+PMULT(ICA)*PHEP(ICA,NHEP)**2
  320 CONTINUE
      PHEP(5,NHEP)=DSQRT(PHEP(5,NHEP))
C
C the partons
C
      NHEP = NHEP + 1
      IDHW(NHEP) = ITYP
      IDHEP(NHEP) = IDPDG(IDHW(NHEP))
      ISTHEP(NHEP) = 113
      JMOHEP(1,NHEP) = NZ0
      JMOHEP(2,NHEP) = NHEP + 1
      JDAHEP(1,NHEP) = 0
      JDAHEP(2,NHEP) = NHEP + 1
      DO 430 ICA = 1,4
        PHEP(ICA,NHEP) = DBLE(PFP(ICA))
  430 CONTINUE
      PHEP(5,NHEP)=RMASS(ITYP)
C
      NHEP = NHEP + 1
      IDHW(NHEP) = ITYP+6
      IDHEP(NHEP) = IDPDG(IDHW(NHEP))
      ISTHEP(NHEP) = 114
      JMOHEP(1,NHEP) = NZ0
      JMOHEP(2,NHEP) = NHEP - 1
      JDAHEP(1,NHEP) = 0
      JDAHEP(2,NHEP) = NHEP - 1
      DO 440 ICA = 1,4
        PHEP(ICA,NHEP) = DBLE(PFM(ICA))
  440 CONTINUE
      PHEP(5,NHEP)=RMASS(ITYP)
C
      RETURN
C
      END
      SUBROUTINE HWAEND
      CALL USCJOB
      CALL HPHST(0)
      RETURN
      END
      FUNCTION XKSECT(EI)
      XKSECT=0
      RETURN
      END
      SUBROUTINE USKRIN(EI)
      RETURN
      END
      SUBROUTINE AMCSEL(KEEP)
*-----------------------------------------------------------------------
*  AMCSEL         Demand a minimum tag energy and two particle in the
*                 the detector (angle greater than 20 mr). Single tag.
*  Author:        M H Lehto
*  Version:       1.00
*  Lastmod:       18-02-93
*  Modification Log:
*
*-----------------------------------------------------------------------
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
C ALTERATIONS: See 5.8 for list of previous revisions
C              Layout completely overhauled
C
C The following variables have been removed:
C              FBTM,FTOP,FHVY,VECWT,TENWT,SWT,RESWT
C              MADDR,MODES,MODEF,IDPRO
C The following COMMON BLOCK has been removed
C              /HWUFHV/   - BDECAY moved to /HWPRCH/
C The following COMMON BLOCKs have been added
C              /HWBMCH/   -contains PART1, PART2 from /HWBEAM/
C              /HWPRCH/   -contains AUTPDF from /HWPARM/ & BDECAY
C              /HWPROP/   -contains many variables from /HWUPDT/
C              /HWDIST/   -contains variables for mixing and verticies
C              /HWQDKS/   -contains heavy flavour decay information
C The following variables have been changed to CHARACTER*8:
C              PART1,PART2,RNAME
C The following parameters have been added:
C              NMXCDK,NMXDKS,NMXMOD,NMXQDK,NMXRES
C The following variables have been added:
C              CSPEED,F0MIX,F1MIX,F2MIX,H1MIX,
C              PHIMIX,IOPREM,PRVTX                 see HWPRAM
C              ANOMSC,ISLENT                       see HWBRCH
C              GAMWT                               see HWEVNT
C              ASFIXD,OMEGA0,TMNISR,WHMIN,YWWMAX,
C              YWWMIN,ZMXISR,COLISR                see HWHARD
C              IFLAV,RLTIM,RSPIN,VTOCDK,VTORDK     see HWPROP
C              DKLTM,IDK,IDKPRD,LNEXT,LSTRT,
C              NDKYS,NME,NMODES,NPRODS,
C              DKPSET,RSTAB                        see HWUPDT
C              REPWT,SNGWT                         see HWUWTS
C              CLDKWT,CTHRPW,PRECO,NCLDK,CLRECO    see HWUCLU
C              EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,
C              VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
C              IOPDKL,MAXDKL,MIXING,PIPSMR         see HWDIST
C              VTXQDK,IMQDK,LOCQ,NQDK              see HWQDKS
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)

C also the following include files from the interface
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
*
      INTEGER JPART,JJ,NTAG,NTRK,ITKCUT,KF
      REAL TAGINN,TAGOUT,ETAG,TKCUT,ABSP,ABSE,COSTHETA,THETA
      LOGICAL TAG,LTAG,LTRK,KEEP,DTAG
*
      INTEGER IGTAG,NAMIND
      EXTERNAL NAMIND
*
      KEEP = .TRUE.
*
*.. If gamma gamma events then check events for tag angle and tag energy
*
      IF ((IPROC.GE.500.AND.IPROC.LE. 510) .OR.
     +    (IPROC.EQ.1500) .OR.
     +    (IPROC.GE.1700.AND.IPROC.LE. 1706) .OR.
     +    (IPROC.GE.5100.AND.IPROC.LE. 5106) .OR.
     +    (IPROC.GE.5200.AND.IPROC.LE. 5206) .OR.
     +    (IPROC.EQ.8000) .OR.
     +    (IPROC.GE.9000.AND.IPROC.LE. 9006) .OR.
     +    (IPROC.GE.10500.AND.IPROC.LE. 10510) .OR.
     +    (IPROC.EQ.11500) .OR.
     +    (IPROC.GE.11700.AND.IPROC.LE. 11706) .OR.
     +    (IPROC.GE.15100.AND.IPROC.LE. 15106) .OR.
     +    (IPROC.GE.15200.AND.IPROC.LE. 15206) .OR.
     +    (IPROC.EQ.18000) .OR.
     +    (IPROC.GE.19000.AND.IPROC.LE. 19006)) THEN

         KEEP = .FALSE.
         LTAG = .FALSE.
         LTRK = .FALSE.
         TAG = .FALSE.
         DTAG = .FALSE.
         TAGINN = 0.0
         TAGOUT = 3.2
         NTRK = 0
         NTAG = 0
         ETAG = 0.
*
*.. To keep it similar to PHOT02 there is a GCUT card
*..      TAG TAGINN TAGOUT
*.. GCUT  1   0.02   1.51
*
*.. TAG is 0 for no tag requirement, 1 for at least one
*..     tag in TAGINN < tag theta < TAGOUT, and 2 for
*..     double tag (ie two tags in that region)
*
         IGTAG=IW(NAMIND('GCUT'))
         IF (IGTAG.NE.0) THEN
            NPAR = IW(IGTAG)
            IF (NPAR.GE.1) THEN
               TAG = IW(IGTAG+1).NE.0
               DTAG = IW(IGTAG+1).EQ.2
            ENDIF
            IF (NPAR.GE.2) TAGINN = RW(IGTAG+2)
            IF (NPAR.GE.3) TAGOUT = RW(IGTAG+3)
         ENDIF
*
*.. ETAG is a card for a minimum tag energy cut (GeV)
*
         IGTAG=IW(NAMIND('ETAG'))
         IF (IGTAG.NE.0) THEN
            NPAR = IW(IGTAG)
            IF (NPAR.GE.1) ETAG = RW(IGTAG+1)
         ENDIF
*
*.. TKCUT demands ITKCUT charged tracks with
*.. theta > TKCUT (radians)
*
         IGTAG=IW(NAMIND('TKCU'))
         IF (IGTAG.NE.0) THEN
            NPAR = IW(IGTAG)
            IF (NPAR.GE.1) ITKCUT = IW(IGTAG+1)
            IF (NPAR.GE.2) TKCUT = RW(IGTAG+2)
         ENDIF
*
         DO 10 JJ=1,NHEP
            JPART=ABS(IDHEP(JJ))
            KF=ISTHEP(JJ)
*
*.. Ignore neutrinos
*
            IF(JPART.EQ.8.OR.JPART.EQ.10.OR.JPART.EQ.12) GOTO 10
*
*.. Make sure its final state
*
            IF (KF .NE. 1) GOTO 10
*
            ABSP=SQRT(PHEP(1,JJ)**2+PHEP(2,JJ)**2+PHEP(3,JJ)**2)
            ABSE=PHEP(4,JJ)
            COSTHETA=PHEP(3,JJ)/ABSP
            THETA=ACOS(ABS(COSTHETA))
*
*.. Electrons are potential tags
*
            IF (JPART.EQ.11) THEN
               IF (TAG) THEN
                  IF ((THETA.GT.TAGINN) .AND.
     +               (THETA.LT.TAGOUT).AND.(ABSE.GT.ETAG)) THEN
                     LTAG = .TRUE.
                     NTAG=NTAG+1
                  ENDIF
                  IF(THETA.GT.TKCUT) NTRK=NTRK+1
               ELSE
*
*.. Count tracks in acceptance
*
                  LTAG = .TRUE.
                  IF(THETA.GT.TKCUT) NTRK=NTRK+1
               ENDIF
            ELSE
               IF(THETA.GT.TKCUT) NTRK=NTRK+1
            ENDIF
 10      CONTINUE
*
         IF (NTRK.GE.ITKCUT) LTRK = .TRUE.
*
*.. If double tag demanded then count tags.
*
         IF (DTAG.AND.NTAG.LT.2) LTAG = .FALSE.
*
*.. Keep event if it passes tag cut and
*.. final state track cuts
*
         IF (LTRK .AND. LTAG)  KEEP = .TRUE.
*
      ENDIF
*
      RETURN
      END
      SUBROUTINE UGTSEC
C --------------------------------------------------------------------
C - B.Bloch, April 99 for KSEC bank creation
C --------------------------------------------------------------------
C flag for the initial process and whether DIS process
      INTEGER IHARD,IFL,IDIS
C
      COMMON /INIPRO/ IHARD,IFL,IDIS
C   now part of HERWIG commons...
C
      DOUBLE PRECISION
     $ AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,PHEP,VHEP
      INTEGER
     $ IDHW,NMXHEP,IERROR,ISTAT,LWEVT,MAXER,MAXPR,NRN,NUMER,
     & NUMERU,NWGTS,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      LOGICAL GENSOF,NOWGT
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO =0.D0, ONE =1.D0)
      PARAMETER (NMXHEP=2000)
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C New standard event common
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      PARAMETER (IGCO = 5136)
      PARAMETER (IVER = 1041)
C===============================================================
      IF ( IHARD.eq.0) THEN
       IF (NWGTS.NE.0) THEN
         RNWGT=1./FLOAT(NWGTS)
         AVWGT=WGTSUM*RNWGT
         SPWGT=SQRT(MAX(WSQSUM*RNWGT-AVWGT**2,ZERO))
         ERWGT=SPWGT*SQRT(RNWGT)
         IF (.NOT.NOWGT) WGTMAX=AVWGT
         IF (WGTMAX.EQ.ZERO) WGTMAX=ONE
         NTOT = NEVHEP
         XTOT = AVWGT
         RTOT = ERWGT
         NACC = NTOT
         XACC = XTOT
         RACC = RTOT
         is =1
         IDC = IGCO
C
         ISEC =  KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
         CALL PRTABL('KSEC',0)
       ENDIF
      ENDIF
      RETURN
      END

