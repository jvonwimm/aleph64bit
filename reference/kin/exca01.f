C*HE 01/16/97 11:48:55 C
C*DK ASKUSI
      SUBROUTINE ASKUSI(IGCOD)
C======================================================================
C! User initialization routine for Excalibur inside KINGAL.
C
C Set generator code
C Read generator cards
C======================================================================
      IMPLICIT NONE
C
C*CA AREA1
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
C*CC AREA1
C*CA EXCINI
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      REAL *4 SDVRT
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153)
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      common/ exckin/SDVRT(3)
C*CC EXCINI
C*CA EXCGEN
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
C*CC EXCGEN
C*CA SJKHCR
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT

C*CC SJKHCR
C*CA LUDAT3
C LUDAT3
      INTEGER MDCY,MDME,KFDP,L2PAR,l2PARF
      PARAMETER (L2PAR=500, L2PARF=2000 )
      REAL BRAT
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
C*CC LUDAT3
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON / BCS / IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C*CA ALEINI
      INTEGER NGEVT
      DOUBLE PRECISION EXEPHO
      COMMON /ALEINI/ EXEPHO,NGEVT
C*CC ALEINI
C Generator specifications
      INTEGER IGCOD,IGCODE,IVERS
      CHARACTER*15 CHRELS
      PARAMETER(IGCODE= 5028 )
      PARAMETER(IVERS = 200)
      PARAMETER(CHRELS=' 4 April,1998')
C LUND/GALEPH decay variables
      INTEGER LPDEC
      PARAMETER( LPDEC= 48 )
      INTEGER NODEC(LPDEC),MXDEC,KNODEC,JIDB
C CARD variables
      INTEGER IPRO,ICUT,IWEI,JTRIG
      INTEGER JSVRT,JGSMO,JGENE,JGCRC,JANOM,JCUTS,JPROC,JXXCR,JWEIG
      INTEGER NSVRT,NGSMO,NGENE,NGCRC,NANOM,NCUTS,NPROC,NXXCR,NWEIG
      PARAMETER(NSVRT=3,NGSMO=10,NGENE=15,NGCRC=3,NANOM=6,NCUTS=26,
     &   NPROC=152,NXXCR=2,NWEIG=2)
C
      INTEGER IEBEAM,ALTABL,ALRLEP,I,J
      INTEGER JKPAR,JRLEP,JPART,JKLIN
      INTEGER NTABL,NOFF
      PARAMETER (NTABL=NSVRT+NGSMO+NGENE+NGCRC+NANOM+
     &   (1+NCUTS*NPROC)+(1+NPROC)+(1+2*152))
      REAL TABL(NTABL)
      INTEGER NLINK,NCOL
      INTEGER KGPART,LUCOMP
      EXTERNAL NLINK,ALTABL,ALRLEP,KGPART,LUCOMP
      INTEGER ISTAT
C
      INTEGER IER
      CHARACTER*80 FNAME
      CHARACTER*4 CNAME,DMODE,FDEVI
      CHARACTER*10 ATYPE,DTYPE
C Particle Codes LUND/ALEPH
      INTEGER KKZ,KKW,KKPART
      PARAMETER( KKZ= 23, KKW= 24)
C Mass and Width index in PART bank
      INTEGER JPARMA,JPARMW
      PARAMETER( JPARMA =  6, JPARMW= 9)
C
      DATA CNAME / 'CRIO' /
      DATA DTYPE / 'EPIO      '/
      DATA DMODE / 'A   ' /
C
C Set generator code.
C
      IGCOD = IGCODE
      WRITE(6,1) IGCOD,IVERS,CHRELS
C
C Set default values
C
      CALL EXCDEF
CC      IEXDBG = 1
      NGEVT  = 0
      EXEPHO = 0.0001D0
C
C Read cards from cards file. All cards except PROC must have a
C certain length otherwise default values are used. All cards except
C PROC,CUTS and WEIG can only be specified once.
C
C  Number of events : TRIG        n1    n2    n3
C
      NEVT = 100
      JTRIG = NLINK('TRIG',0)
      IF(JTRIG.NE.0) THEN
        IF (IW(JTRIG).GE.2) NEVT = IW(JTRIG+2)-IW(JTRIG+1)+1
      ENDIF
C
C Beam spot : SVRT sx sy sz
C
      SDVRT(1) = 0.0124
      SDVRT(2) = 0.0005
      SDVRT(3) = 0.72
      JSVRT = NLINK('SVRT',0)
      IF(JSVRT.NE.0) THEN
        IF (IW(JSVRT).EQ.NSVRT) THEN
          SDVRT(1) = RW(JSVRT+1)
          SDVRT(2) = RW(JSVRT+2)
          SDVRT(3) = RW(JSVRT+3)
        ELSE
          WRITE(LMCWRT,100) 'SVRT',IW(JSVRT),NSVRT
        ENDIF
      ENDIF
C
C Standard Model parameters : GSMO
C
      JGSMO = NLINK('GSMO',0)
      IF (JGSMO.NE.0) THEN
        IF (IW(JGSMO).EQ.NGSMO) THEN
          ZMI    = DBLE( RW(JGSMO+ 1) )
          WZI    = DBLE( RW(JGSMO+ 2) )
          WMI    = DBLE( RW(JGSMO+ 3) )
          WWI    = DBLE( RW(JGSMO+ 4) )
          GMU    = DBLE( RW(JGSMO+ 5) )
          STH2   = DBLE( RW(JGSMO+ 6) )
          ALPHAR = 1.0D0/DBLE( RW(JGSMO+ 7) )
          ALPHA  = 1.0D0/DBLE( RW(JGSMO+ 8) )
          ALS    = DBLE( RW(JGSMO+9) )
          EXALSZ = DBLE( RW(JGSMO+10) )
        ELSE
          WRITE(LMCWRT,100) 'GSMO',IW(JGSMO),NGSMO
        ENDIF
      ENDIF
C
C Generator options : GENE
C
      JGENE = NLINK('GENE',0)
      IF (JGENE.NE.0) THEN
        IF (IW(JGENE).EQ.NGENE) THEN
          IEXDIA = IW(JGENE+ 1)
          IEXUWT = IW(JGENE+ 2)
          IEXISR = IW(JGENE+ 3)
          IEXFSR = IW(JGENE+ 4)
          IEXNNG = IW(JGENE+ 5)
          IEXHAD = IW(JGENE+ 6)
          IEXCOU = IW(JGENE+ 7)
          IEXPAF = IW(JGENE+ 8)
          IEXCRC = IW(JGENE+ 9)
          IEXQDW = IW(JGENE+10)
          IEXQCD = IW(JGENE+11)
          IEXEWD = IW(JGENE+12)
          IEXINT = IW(JGENE+13)
          ECM    = DBLE( RW(JGENE+14) )
          EXEPHO = DBLE( RW(JGENE+15) )
        ELSE
          WRITE(LMCWRT,100) 'GENE',IW(JGENE),NGENE
        ENDIF
      ELSE
        WRITE(LMCWRT,110)
        STOP
      ENDIF
C
C Color reconnection : GCRC
C
      JGCRC = NLINK('GCRC',0)
      IF (JGCRC.NE.0) THEN
        IF (IW(JGCRC).EQ.NGCRC) THEN
          TFRAG = DBLE( RW(JGCRC+1) )
          RHAD  = DBLE( RW(JGCRC+2) )
          RPROB = DBLE( RW(JGCRC+3) )
        ELSE
          WRITE(LMCWRT,100) 'GCRC',IW(JGCRC),NGCRC
        ENDIF
      ENDIF
C
C Special Color Reconnection file : XXCR  mode unit filename
C
      JXXCR = NLINK('XXCR',0)
      IF(JXXCR.NE.0) THEN
        IF (IW(JXXCR).EQ.NXXCR) THEN
          IEXFCR = IW(JXXCR+1)
          IEXUCR = IW(JXXCR+2)
        ELSE
          WRITE(LMCWRT,100) 'XXCR',IW(JXXCR),NXXCR
        ENDIF
      ENDIF
C
C Anomalous couplings : ANOM
C
      JANOM = NLINK('ANOM',0)
      IF (JANOM.NE.0) THEN
        IF (IW(JANOM).EQ.NANOM) THEN
          DANOMC(2) = DBLE( RW(JANOM+1) )
          DANOMC(3) = DBLE( RW(JANOM+2) )
          DANOMC(4) = DBLE( RW(JANOM+3) )
          DANOMC(5) = DBLE( RW(JANOM+4) )
          DANOMC(6) = DBLE( RW(JANOM+5) )
          DANOMC(8) = DBLE( RW(JANOM+6) )
        ELSE
          WRITE(LMCWRT,100) 'ANOM',IW(JANOM),NANOM
        ENDIF
      ENDIF
C
C Process card : PROC
C
      IPRO = 0
      JPROC = NLINK('KPRO',0)
 5    IF (JPROC.NE.0) THEN
        DO 10 I = 1, IW(JPROC)
          IPROC(IPRO+I) = IW(JPROC+I)
 10     CONTINUE
        IPRO = IPRO + IW(JPROC)
        JPROC = IW(JPROC-1)
        GOTO 5
      ENDIF
C
C Cuts : CUTS
C
      ICUT = 0
      JCUTS = NLINK('CUTS',0)
 15   IF (JCUTS.NE.0) THEN
        IF (IW(JCUTS).GT.1) THEN
          ICUT = ICUT + 1
          IEXCUT(ICUT) = IW(JCUTS+1)
          DO 20 I = 1, IW(JCUTS)-1
            CUTS(I,ICUT) = DBLE( RW(JCUTS+I+1) )
 20       CONTINUE
        ELSE
          WRITE(LMCWRT,100) 'CUTS',IW(JCUTS),NCUTS
        ENDIF
        JCUTS = IW(JCUTS-1)
        GOTO 15
      ENDIF
C
C Weights : WEIG
C
      IWEI = 0
      JWEIG = NLINK('WEIG',0)
 25   IF (JWEIG.NE.0) THEN
        IF (IW(JWEIG).EQ.2) THEN
          IWEI = IWEI + 1
          IEXUWP(IWEI) = IW(JWEIG+1)
          EXUWEI(IWEI) = DBLE( RW(JWEIG+2) )
        ELSE
          WRITE(LMCWRT,100) 'WEIG',IW(JWEIG),NWEIG
        ENDIF
        JWEIG = IW(JWEIG-1)
        GOTO 25
      ENDIF
C
C  All the parameters are stored in TABL(I)
C
C SVRT
      TABL( 1) = SDVRT(1)
      TABL( 2) = SDVRT(2)
      TABL( 3) = SDVRT(3)
C GSMO
      TABL( 4) = SNGL( ZMI )
      TABL( 5) = SNGL( WZI )
      TABL( 6) = SNGL( WMI )
      TABL( 7) = SNGL( WWI )
      TABL( 8) = SNGL( GMU )
      TABL( 9) = SNGL( STH2   )
      TABL(10) = SNGL( ALPHAR )
      TABL(11) = SNGL( ALPHA  )
      TABL(12) = SNGL( ALS    )
      TABL(13) = SNGL( EXALSZ )
C GENE
      TABL(14) = REAL(IEXDIA)
      TABL(15) = REAL(IEXUWT)
      TABL(16) = REAL(IEXISR)
      TABL(17) = REAL(IEXFSR)
      TABL(18) = REAL(IEXNNG)
      TABL(19) = REAL(IEXHAD)
      TABL(20) = REAL(IEXCOU)
      TABL(21) = REAL(IEXPAF)
      TABL(22) = REAL(IEXCRC)
      TABL(23) = REAL(IEXQDW)
      TABL(24) = REAL(IEXQCD)
      TABL(25) = REAL(IEXEWD)
      TABL(26) = REAL(IEXINT)
      TABL(27) = SNGL(ECM   )
      TABL(28) = SNGL(EXEPHO)
C GCRC
      TABL(29) = SNGL( TFRAG  )
      TABL(30) = SNGL( RHAD   )
      TABL(31) = SNGL( RPROB )
C ANOM
      TABL(32) = SNGL(DANOMC(2))
      TABL(33) = SNGL(DANOMC(3))
      TABL(34) = SNGL(DANOMC(4))
      TABL(35) = SNGL(DANOMC(5))
      TABL(36) = SNGL(DANOMC(6))
      TABL(37) = SNGL(DANOMC(8))
C
      NOFF = NSVRT+NGSMO+NGENE+NGCRC+NANOM
C PROC
      TABL(NOFF+1) = REAL(IPRO)
      DO 50 I = 1, IPRO
        TABL(NOFF+I+1)  = REAL(IPROC(I))
 50   CONTINUE
      NOFF = NOFF+(IPRO+1)
C CUTS
      TABL(NOFF+1) = REAL(ICUT)
      DO 70 J = 1, ICUT
        TABL(NOFF+2+(J-1)*NCUTS)   = REAL(IEXCUT(J))
        DO 60 I = 1, NCUTS-1
          TABL(NOFF+2+(J-1)*NCUTS+I) = SNGL( CUTS(I,J) )
 60     CONTINUE
 70   CONTINUE
      NOFF = NOFF+(NCUTS*ICUT+1)
C WEIG
      TABL(NOFF+1) = REAL(IWEI)
      DO 80 J = 1, IWEI
        TABL(NOFF+2+(J-1)*2) = REAL(IEXUWP(J))
        TABL(NOFF+3+(J-1)*2) = REAL(EXUWEI(J))
 80   CONTINUE
      NOFF = NOFF+(2*IWEI+1)
C
C  Fill the KPAR bank with the generator parameters
C
      NCOL  = NOFF
      JKPAR = ALTABL('KPAR',NCOL,1,TABL,'2I,(F)','C')
C
C  Fill RLEP bank
C
      IEBEAM = NINT(500*SNGL(ECM))
      JRLEP  = ALRLEP(IEBEAM,'    ',0,0,0)
C
C  Print PART and KLIN banks
C
      CALL PRTABL('KPAR',0)
      CALL PRTABL('RLEP',0)
C
C Create PART bank, using JETSET7.4 scheme.
C
      CALL KXL74A(JPART,JKLIN)
      IF ((JPART.LE.0).OR.(JKLIN.LE.0)) THEN
        WRITE(LMCWRT,210) JPART,JKLIN
        STOP
      ENDIF
C
C Update masses in the PART bank for Z, W.
C
      JPART = NLINK('PART',0)
      IF (JPART.GT.0) THEN
C
        KKPART = KGPART(KKZ)
        IF (KKPART.GT.0) THEN
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMA) = SNGL(ZMI)
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMW) = SNGL(WZI)
        ENDIF
C
        KKPART = KGPART(KKW)
        IF (KKPART.GT.0) THEN
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMA) = SNGL(WMI)
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMW) = SNGL(WWI)
        ENDIF
C
        KKPART = KGPART(-KKW)
        IF (KKPART.GT.0) THEN
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMA) = SNGL(WMI)
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMW) = SNGL(WWI)
        ENDIF
      ELSE
        WRITE(LMCWRT,220) JPART
      ENDIF
C
C   Inhibit decays
C
      MXDEC=KNODEC(NODEC,LPDEC)
      MXDEC=MIN(MXDEC,LPDEC)
      IF (MXDEC.GT.0) THEN
         DO 90 I=1,MXDEC
          IF (NODEC(I).GT.0) THEN
            JIDB = NLINK('MDC1',NODEC(I))
            IF ( JIDB .EQ. 0 ) MDCY(LUCOMP(NODEC(I)),1) = 0
          ENDIF
 90     CONTINUE
      ENDIF
C
C Open color reconnection file for READ or WRITE
C
      IF (IEXFCR.EQ.1.OR.IEXFCR.EQ.2) THEN
        CALL EXCRI(IEXFCR,LMCWRT)
        CALL ACDARG(CNAME,DTYPE,DMODE,FNAME,ATYPE,FDEVI,IER)
        IF(IER.GT.0)THEN
          WRITE(LMCWRT,140) IER, 'ACDARG'
          STOP
        ENDIF
        IF (IEXFCR.EQ.1) THEN
          IF(IER.EQ.0)THEN
            CALL AOPEN(IEXUCR,FNAME,ATYPE,FDEVI,IER)
            IF(IER.NE.0)THEN
              WRITE(LMCWRT,140) IER,'AOPEN '
              IF(IER.EQ.-1) WRITE(LMCWRT,150)
              STOP
            ENDIF
            CALL ABRSEL('E',' ',IER)
          ENDIF
          GOTO 999
        ELSEIF (IEXFCR.EQ.2) THEN
          IF(IER.EQ.0)THEN
            CALL AOPENW(IEXUCR,FNAME,ATYPE,FDEVI,IER)
            IF(IER.NE.0)THEN
              WRITE(LMCWRT,140) IER,'AOPENW'
              STOP
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C Initialize the generator.
C
      CALL EXCINI(ISTAT)
      IF (ISTAT.NE.0) THEN
        WRITE(LMCWRT,120)
        STOP
      ENDIF
C
 999  CONTINUE
C
      RETURN
 1    FORMAT(///,
     * 4X,'***************************************************',/,
     * 4X,'*                                                 *',/,
     * 4X,'*              EXCALIBUR GENERATOR                *',/,
     * 4X,'*                                                 *',/,
     * 4X,'*    KINGAL CODE = ',I5,'                          *',/,
     * 4X,'*    VERSION     = ',I5,'                          *',/,
     * 4X,'*    RELEASED    = ',A11,20X,'*',/,
     * 4X,'*                                                 *',/,
     * 4X,'***************************************************',/)
 100  FORMAT(/,1X,'ASKUSI: Warning -- ',/,
     &   1X,' Length of data card ',A4,' is ',I3,
     &   '. It should be 0 or ',I2,' - Default values are used!')
 110   FORMAT(/,1X,//,'ASKUSI: ************* ERROR ************ ',/,/,
     * 8X,'YOU MUST PROVIDE A "GENE" CARD.',///)
 120  FORMAT(/,1X,'ASKUSI: EXCINI failed -- terminating')
 140  FORMAT(/,1X,'ASKUSI: ERROR ',I8,' FROM ',A6,/)
 150  FORMAT(/,1X,'ASKUSI: FILE :',A,' DOES NOT EXIST ',/)
 210  FORMAT(/,1X,'ASKUSI: Error creating PART bank !!! ',/,
     * 10X,'IPART=',I4,4X,'JKLIN=',I4,/)
 220  FORMAT(/,1X,'ASKUSI: PART bank is missing !!! ',/)
      END
C*DK ASKUSE
      SUBROUTINE ASKUSE(IDPR,ISTA,NITR,NIVX,ECMS,WEIT)
C======================================================================
C! Generate one Excalibur event, inside KINGAL.
C======================================================================
      IMPLICIT NONE
C
C*CA AREA1
      common/area1/sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
      double precision sth2,zm,wz,wm,ww,alpha,grap,statfac,
     +   pbfac,fcol,facnor,als,zmi,wzi,wmi,wwi
C*CC AREA1
C*CA EXCINI
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      REAL *4 SDVRT
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153)
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      common/ exckin/SDVRT(3)
C*CC EXCINI
C*CA EXCGEN
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
C*CC EXCGEN
      INTEGER ALIDPR
      COMMON /ALEXPR/ ALIDPR
C*CA MOMENTA
      common/momenta/roots,xr1,xr2,pm(0:4,0:900)
      double precision roots,xr1,xr2,pm
C
C*CC MOMENTA
C*CA LUJETS
C LUJETS
      INTEGER JN7LU,K7LU,LJNPAR
      REAL    P7LU,V7LU
      PARAMETER( LJNPAR= 4000)
      COMMON /LUJETS/ JN7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     &                V7LU(LJNPAR,5)
C
C*CC LUJETS
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON / BCS / IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C*CA ALEINI
      INTEGER NGEVT
      DOUBLE PRECISION EXEPHO
      COMMON /ALEINI/ EXEPHO,NGEVT
C*CC ALEINI
C Local variables
      INTEGER IDPR,ISTA,ISTAT,NITR,NIVX,I,J,ID(4)
      REAL ECMS,WEIT
      DOUBLE PRECISION EXWEI
C
      INTEGER ALTABL,NROW,JKPOL,JKXME,ivx
      REAL TABL(200)
      INTEGER KABL(200)
      EQUIVALENCE (TABL,KABL)
C
      REAL    VCOOR(4),rn1,rn2,rn3,dum
      INTEGER KKPART,KGPART
      INTEGER KKEP,KKEN,KKPHOT,NTR,NVX,JKINE,KBKINE
      PARAMETER( KKEN= 11, KKEP= -11, KKPHOT= 22)
      REAL    PXYZM(4),EPHMIN,EMASS
      PARAMETER(EPHMIN=0.001)
      PARAMETER( EMASS = 0.51099906E-3 )
      EXTERNAL KGPART,KBKINE
      LOGICAL EOFR
      SAVE EOFR
      DATA EOFR /.FALSE./
C
C  Initialization ASKUSEs arguments
C
      IDPR   = 0
      ISTA   = 0
      NITR   = 0
      NIVX   = 0
      NGEVT  = NGEVT + 1
C
C======================== GENERATE AN EVENT ============================
C
      call rannor(rn1,rn2)
      call rannor(rn3,dum)
      VCOOR(1) = sdvrt(1) * rn1
      VCOOR(2) = sdvrt(2) * rn2
      VCOOR(3) = sdvrt(3) * rn3
      VCOOR(4) = 0.
      do ivx = 1,4
        VERTEX(ivx) = vcoor(ivx)
      enddo
C
 888  ISTAT=0
      IF (IEXFCR.NE.1) THEN
        CALL EXCEVT(EXWEI,ISTAT)
        IF ( ISTAT.NE.0) THEN
          WRITE(LMCWRT,100) ISTAT,NGEVT
          GOTO 888
        ENDIF
        IF (IEXFCR.EQ.2) THEN
C          CALL BEPWR(IW,IEXUCR,'KXCRLUNHLUND')
          CALL BLIST (IW,'T=','KXCRLUNHLUND')
          CALL BWRITE(IW,IEXUCR,'T')
          CALL BDROP (IW,'T')
          CALL BGARB (IW)
        ENDIF
      ELSE
        EXWEI = 1.0D0
 11     IF(.NOT.EOFR) CALL ABREAD(IW,IEXUCR,'T',*13,*14)
        GOTO 16
 13     WRITE(LMCWRT,*)
     &     'ASKUSE: ABREAD: Error reading event - continuing'
        GOTO 11
 14     ISTA = 2
C EOF reached, but there is no way to tell Kingal that!
C In the mean time set variable EOFR=.TRUE. so we avoid many error
C messages from ABREAD.
        EOFR=.TRUE.
        GOTO 40
 16     CONTINUE
        CALL EXCR(IEXFCR,IEXCRC,WM,WW,ISTAT)
        ISTA  = ISTAT
        IF (ISTAT.EQ.2) GOTO 40
      ENDIF
C
C======================= INTERFACE TO ALEPH ============================
C
C Initial beam particles are document (empty) lines (ALEPH convention)
C and are removed
      K7LU(1,1) = 0
      K7LU(2,1) = 0
      CALL LUEDIT(12)
C Update Tau-info if not in color reconnection mode 1
      NROW = 0
      IF (IEXFCR.NE.1) THEN
        DO 5 I=1,4
          ID(I)=0
          IF (IEXTPL(I).NE.0) THEN
            ID(I)=IEXTPL(I)-SIGN(4,IEXTPL(I))
            NROW=NROW+1
          ENDIF
 5      CONTINUE
      ENDIF
C
C Fill KINE banks with beam particles. The beams are listed according to
C their energy after ISR.
C
C e-
      PXYZM(1) = 0.0
      PXYZM(2) = 0.0
      PXYZM(3) = SNGL(0.5D0*XR1*ECM)
      PXYZM(4) = 0.0
      KKPART = KGPART(KKEN)
      NTR = -1
      NVX = 0
      JKINE = KBKINE(NTR,PXYZM,KKPART,NVX)
      IF (JKINE.LE.0) THEN
        WRITE(LMCWRT,110) JKINE
        STOP
      ENDIF
C e+
      PXYZM(3) = -SNGL(0.5D0*XR2*ECM)
      PXYZM(4) = 0.0
      KKPART = KGPART(KKEP)
      NTR = NTR-1
      NVX = 0
      JKINE = KBKINE(NTR,PXYZM,KKPART,NVX)
      IF (JKINE.LE.0) THEN
        WRITE(LMCWRT,110) JKINE
        STOP
      ENDIF
C
C Remove photons with too small energy ( < EXEPHO )
C
      I = 1
 10   IF (K7LU(I,2).EQ.22) THEN
        IF (P7LU(I,4).LT.SNGL(EXEPHO)) THEN
          K7LU(I,1) = 0
          CALL LUEDIT(12)
C Update Tau-info
          DO 15 J=1,NROW
            IF (ABS(ID(J)).GT.I) ID(J)=ID(J)-SIGN(1,ID(J))
 15       CONTINUE
          IF (I.LE.JN7LU) GOTO 10
        ENDIF
      ENDIF
      I = I + 1
      IF(I.LT.JN7LU) GOTO 10
C
C Build ALEPH event interface to LUND
C
      CALL KXL7AL(VCOOR,ISTAT,NIVX,NITR)
C
C  Book and fill the polarization bank 'KPOL' if necessary
C
      IF (NROW.GT.0) THEN
        DO 20 I=1,NROW
          KABL( (I-1)*4 + 1) = ABS(ID(I))
          TABL( (I-1)*4 + 2) = 0.
          TABL( (I-1)*4 + 3) = 0.
          TABL( (I-1)*4 + 4) = REAL(SIGN(1,ID(I)))
 20     CONTINUE
        JKPOL = ALTABL('KPOL',4,NROW,TABL,'2I,(I,3F)','E')
        IF (JKPOL.LT.0) WRITE(LMCWRT,130) 'KPOL'
      ENDIF
C
C  Book and fill the Matrix Element bank 'KXME'
C
      TABL(1) = 0.0
      TABL(2) = SNGL(EX1234)
      TABL(3) = SNGL(EX1432)
      TABL(4) = SNGL(EXINT)
      NROW = 1
      DO 30 I=1,8
        IF (EXCSIG(1,I)+EXCSIG(2,I)+EXCSIG(3,I).GT.0.0D0) THEN
          KABL( NROW*4 + 1) = I
          TABL( NROW*4 + 2) = SNGL(EXCSIG(1,I))
          TABL( NROW*4 + 3) = SNGL(EXCSIG(2,I))
          TABL( NROW*4 + 4) = SNGL(EXCSIG(3,I))
          NROW = NROW + 1
        ENDIF
 30   CONTINUE
      JKXME = ALTABL('KXME',4,NROW,TABL,'2I,(I,3F)','E')
      IF (JKXME.LT.0) WRITE(LMCWRT,130) 'KXME'
C
C Pack process code
C
      IF (IEXFCR.NE.1) THEN
        ID(1) = IPROC(IEXPRO)/1000000
        ID(2) = MOD(IPROC(IEXPRO)/10000,100)
        ID(3) = MOD(IPROC(IEXPRO)/100,100)
        ID(4) = MOD(IPROC(IEXPRO),100)
        IDPR  = ID(1)*32768 + ID(2)*1024 + ID(3)*32 + ID(4)
        IDPR  = IDPR*1000 + IEXPHG*10 + IEXCCF
      ELSE
        IDPR  = ALIDPR
      ENDIF
C
C Return other information
C
      ISTA  = ISTAT
      ECMS  = SNGL(ECM)
      WEIT  = SNGL(EXWEI)
C
 40   RETURN
 100  FORMAT(/,' ASKUSE: EXCALIBUR returned error code,',I8,
     &   ' in event',I8)
 110  FORMAT(/,' ASKUSE: KINE bank for beam electrons not created:',
     &   3X,'JKINE=',I4,/)
 120  FORMAT(/,' ASKUSE: No PART bank !!!! STOP.',/)
 130  FORMAT(/,' ASKUSE: Warning: could not create ',A4,' bank!',/)
      END
C*DK USCJOB
      SUBROUTINE USCJOB
C======================================================================
C! finish job
C======================================================================
      IMPLICIT NONE
C
C*CA EXCINI
      INTEGER LMCWRT,IPROC,NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD,IEXUWT
     &  ,IEXCOU,IEVT,INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG,IEXQDW,IEXNDB
     &  ,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT,IEXINT,IEXFCR,IEXUCR,IEXUWP
      DOUBLE PRECISION ECM,GMU,ALPHAR,DANOMC,CUTS,EXCFAC,EXALSZ
     &  ,EXUMWT,EXUWEI
      REAL *4 SDVRT
      COMMON /EXINII/ LMCWRT,IPROC(300),NEVT,IEXDIA,IEXISR,IEXFSR,IEXHAD
     &  ,IEXUWT,IEXCOU,IEVT(152),INFAIL,IEXMIX,IEXPAF,IEXCRC,IEXNNG
     &  ,IEXQDW,IEXNDB,IEXDBG,IEXQCD,IEXEWD,IEXGG,IEXCUT(153)
     &  ,IEXINT,IEXFCR,IEXUCR,IEXUWP(152)
      COMMON /EXINID/ ECM,GMU,ALPHAR,DANOMC(14),CUTS(26,153)
     &  ,EXCFAC,EXALSZ,EXUMWT(152),EXUWEI(152)
      common/ exckin/SDVRT(3)
C*CC EXCINI
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON / BCS / IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
      INTEGER ISTAT,imstj,itau,ndec,nlink
      external nlink
C
      IF (IEXFCR.EQ.1.OR.IEXFCR.EQ.2) THEN
        IF (IEXFCR.EQ.2) CALL BWRITE(IW,IEXUCR,'T')
        CLOSE (IEXUCR)
        CALL EXCRE(IEXFCR,LMCWRT)
      ENDIF
      IF (IEXFCR.NE.1) CALL EXCEND(ISTAT)
C   tau decay summary if needed
      imstj = nlink('MSTJ',28)
      IF(imstj.gt.0)then
         if ( iw(imstj+1).eq.2) call taudkay(itau,ndec,100)
      endif
      RETURN
      END
C*DK UTOPOL
      REAL FUNCTION UTOPOL(ITAU,IORIG,KFORIG,TPOL)
C======================================================================
C EXCALIBUR interface to LUTAUD (ALEPH interface to TAUOLA)
C======================================================================
      IMPLICIT NONE
C*CA EXCGEN
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
C*CC EXCGEN
C      INTEGER NCONP,NNCONP,IEXWNL,IEXWNG,IGEVT,IFEVT,IEXCCF,IEXTPL
C     &  ,IEXOFP,IEXPHG,IEXPRO
C      COMMON /EXGENI/ NCONP,NNCONP,IEXWNL(152),IEXWNG(152),IGEVT(152)
C     &  ,IFEVT(152),IEXCCF,IEXTPL(4),IEXOFP(15),IEXPHG,IEXPRO
C      DOUBLE PRECISION EXXSEC,EXXERR,EXXSRN,EXWMAX,EXCUMC,EXSUMW
C     &  ,EXSWSQ,EXMWEI,CL,ZETA,ZETA1,OMZ1,SHCUT,SAFETY,EXGMWT
C     &  ,GAEUL,EXS,EXSWS3,EXSWS4,W,EX1234,EXINT,EX1432,VERTEX,EXHELI
C     &  ,EXCSIG
C      COMMON /EXGEND/ EXXSEC(152),EXXERR(152),EXXSRN(152),EXWMAX(152)
C     &  ,EXCUMC(152),EXSUMW(152),EXSWSQ(152),EXSWS3(152),EXSWS4(152)
C     &  ,EXMWEI(152),EXS(152),CL,ZETA,ZETA1,OMZ1,SHCUT(153),EXGMWT(153)
C     &  ,GAEUL,SAFETY,W,EX1234,EXINT,EX1432,VERTEX(4),EXHELI(16)
C     &  ,EXCSIG(3,8)
*CC EXCGEN
C*CA LUJETS
C LUJETS
      INTEGER JN7LU,K7LU,LJNPAR
      REAL    P7LU,V7LU
      PARAMETER( LJNPAR= 4000)
      COMMON /LUJETS/ JN7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     &                V7LU(LJNPAR,5)
C
C*CC LUJETS
      INTEGER ITAU,IORIG,KFORIG
      REAL TPOL
      INTEGER PRIM,I
C
      TPOL = 0.0
C
C Primary or secondary tau?
C
      PRIM = 0
      DO 10 I = 1,4
        IF (ITAU.EQ.ABS(IEXTPL(I))) PRIM = I
 10   CONTINUE
C
C Determine polarization. For initial taus the polarizations are
C determined from the matrix element.
C
      IF (PRIM.GT.0)
     &   TPOL =
     &   SIGN(1.,REAL(K7LU(ITAU,2)))*
     &   SIGN(1.,REAL(IEXTPL(PRIM)))
C
      UTOPOL = TPOL
      END
