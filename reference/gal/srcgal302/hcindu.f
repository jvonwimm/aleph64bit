      SUBROUTINE HCINDU
C------------------------------------------------
C!  Consider effects of inductions between tubes
CKEY HCAL INDUCTION
C!      Author       : G. Iaselli  880119
C!                     F.Ranjard   920115
C!
C!      Input  Bank    : JDHCHI  McHcHits
C!                       JDHCSE  McHcTubesSegment
C!                       HWHT    McHcTubeSignal
C!
C!      Output Bank    : HWHT
C!      temporary Bank : JDHTEM
C!      Called by      : HCASIG
C!
C!
C ----------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
      PARAMETER (LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER (LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER (LPHCT = 4)
      PARAMETER (LPHCBM = 24,LPHCES = 6)
      COMMON/HCGEGA/ HCSMTH,HCIRTH,HCLSLA,HCTUTH, NHCINL,NHCOUL,NHCTRE,
     +HCPHOF,NHCTU1(LHCNL), HCLARA(LHCNL),HCLAWI(LHCNL),IHCREG(LHCTR),
     +HCZMIN(LPHC),HCZMAX(LPHC),HCRMIN(LPHC), HCRMAX(LPHC),HCTIRF(LPHC),
     +NHCPLA(LPHC), HCWINO(LPHC),HCLTNO(LPHC)
      PARAMETER(JHCSTN=1,JHCSLN=2,JHCSMN=3,JHCSPN=4,JHCSTA=5,JHCSTX=6,
     +          JHCSTY=7,JHCSPW=8,JHCSXA=9,JHCSYA=10,JHCSZA=11,
     +          JHCSTR=12,JHCSHT=13,JHCSFT=14,JHCSFS=15,LHCSEA=15)
      PARAMETER(JHCHTS=1,JHCHSY=2,JHCHSP=3,JHCHNS=4,LHCHIA=4)
      PARAMETER(JHWHTA=1,LHWHTA=1)
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON /HCCONG/ HCTUAC,HCSTDT,HCADCE,HADCMX,HCPFAC,
     &                HCTINS,RHBAMN,ZHECMN,ZHBAMX,HSTREA,HSTUST,
     +                NHCFSS,HCFSS1,HCFSS2,HCFLSS(100)
     &               ,HTLEMX,HCTEFF(3),HPINDU
C
      PARAMETER (LTUB=320)
      DIMENSION IVET(LTUB)
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
C  ---------------------------------------------------------------
C - return if no HWHT bank
      JHWHT=IW(NAHWHT)
      IF(JHWHT.EQ.0) RETURN
C - initialize temporary storing space
      JDHTEM = 0
      CALL WBANK(IW,JDHTEM,3*IW(JHWHT),*10)
      IW(JDHTEM+1)=LHWHTA
      IW(JDHTEM+2)=0
C
      NRHI = 1
C
      IPL0=-1
      IPOR0=-1
      NSTRE=0
      NTR=0
C?    Loop on fired tubes
      DO 90 ITUBE= 1,LROWS(JHWHT)
         ITADR=ITABL (JHWHT,ITUBE,1)
         NSEG=ITABL(JDHCHI,NRHI,JHCHTS)
         KHCSE=KROW(JDHCSE,NSEG)
C
C?    Count number of streamers
         NSTRE=0
   30    CONTINUE
         IF(NRHI.GT.LROWS(JDHCHI)) GO TO 40
         NSEG = ITABL(JDHCHI,NRHI,JHCHTS)
         ITADR1=ITABL(JDHCSE,NSEG,JHCSTA)
         IF(ITADR.NE.ITADR1) GOTO 40
         NSTRE=NSTRE+ITABL(JDHCHI,NRHI,JHCHNS)
         NRHI = NRHI+1
         GO TO 30
   40    CONTINUE
C
C?    Compute number of tubes to be fired
         CALL POISSN(NSTRE*HPINDU,NT,IER)
         IS=SIGN(1.,(RNDM(DUMMY)-.5))
C
C
         IPL=IW(KHCSE+2)
         IPOR = IW(KHCSE+4)
         IF (IPL.NE.IPL0 .OR. IPOR.NE.IPOR0)THEN
            IPL0=IPL
            IPOR0=IPOR
            IF (IPOR.EQ.LPBAR) THEN
               ITLAS = NHCTU1(IPL)
            ELSE
               ITLAS = NHCTRE
            ENDIF
C
            IF (NTR.GT.0) THEN
C            Fill the temporary bank with tubes addresses
               IF(NTR.GT.1)CALL SORTIQ(IVET,1,NTR,1)
               IF (LFRROW(JDHTEM).LT.NTR) THEN
                  IF (NTR.GT.50) THEN
                     CALL ALTELL ('HCINDU: POISSN is corrupted-STOP',0,
     &                            'END')
                  ENDIF
                  CALL WBANK (IW,JDHTEM,IW(JDHTEM)+50,*10)
               ENDIF
               KHTEM = KNEXT(JDHTEM)
               CALL UCOPY(IVET,IW(KHTEM+1),NTR)
               IW(JDHTEM+2)=LROWS(JDHTEM) + NTR
               NTR=0
            ENDIF
C
         ENDIF
C
         ITEM=MOD(ITADR,1000)
         MTEM = (ITEM-1) / ITLAS
         NEW = 0
C     fill IVET but suppress double tube addresses
         DO 80 IT=1,NT+1
            JADR=ITADR+IS*(IT-1)
            DO 60 NV=1,NTR
               IF(JADR.EQ.IVET(NV)) GO TO 80
   60       CONTINUE
            ITUL=ITEM+IS*(IT-1)
            MTUL = (ITUL-1) / ITLAS
            IF (ITUL.GE.1.AND.MTUL.EQ.MTEM)THEN
               NEW=NEW+1
               IVET(NTR+NEW)=JADR
            ENDIF
   80    CONTINUE
         NTR = NTR + NEW
   90 CONTINUE
C - copy last set of tube addresses into the temporary buffer
      IF (NTR.GT.0) THEN
C       Fill the temporary bank with tubes addresses
         IF(NTR.GT.1)CALL SORTIQ(IVET,1,NTR,1)
         IF (LFRROW(JDHTEM).LT.NTR) THEN
            CALL WBANK (IW,JDHTEM,IW(JDHTEM)+50,*10)
         ENDIF
         KHTEM = KNEXT(JDHTEM)
         CALL UCOPY(IVET,IW(KHTEM+1),NTR)
         IW(JDHTEM+2)=LROWS(JDHTEM) + NTR
      ENDIF
C
C - copy the temporary buffer into HWHT
      CALL WBANK(IW,JDHTEM,IW(JDHTEM+1)*IW(JDHTEM+2)+LMHLEN,*10)
      CALL BKFRW(IW,'HWHT',0,IW,JDHTEM,*10)
      CALL WDROP(IW,JDHTEM)
      RETURN
C
C - not enough space to book temporary space
   10 CONTINUE
      CALL ALTELL('HCINDU:not enough space to book JDHTEM ',1,'NEXT')
      END
