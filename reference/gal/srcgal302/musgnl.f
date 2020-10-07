      SUBROUTINE MUSGNL
C
C***********************************************************************
C
C T.Wang -860108
C
C! induce signals on strips by streamers.
C
C       Called by MUHIT
C       Calls ALBOS,MUFLHT                    in this .HLB
C             BLIST                           in BOS
C
C***********************************************************************
C
      SAVE
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
C
      PARAMETER (JMUSFF= 1,JMUSPL= 2,JMUSET= 3,JMUSTU= 4,JMUSXI= 5,
     +           JMUSYI= 6,JMUSZI= 7,JMUSXO= 8,JMUSYO= 9,JMUSZO=10,
     +           JMUSXY=11,JMUSXZ=12,JMUSYZ=13,LMUSTA=13)
      PARAMETER(JMUHTN=1,JMUHEL=2,JMUHSP=3,JMUHSA=4,LMUHTA=4)
C! The general constants to create MU signals
      COMMON/MUGNCN/WDEIMU,WDTBMU,OFTBMU,WDATMU,HTATMU,HFHTMU,
     *              SGLSMU,PSCRMU,PTSPMU,DSSPMU,
     *              PTXSMU,PTYSMU,SNSXMU(4),SNSYMU(4)
C! The current constants to create MU signals
      COMMON/MUSGCN/WDMDMU,XLMDMU,NMEIMU,XLEIMU,KPMDMU,ZPSWMU(2),
     *              NASLMU,NUSLMU,NUEMMU,NYSTMU,NXSTMU,XSOFMU(2),
     *              YSOFMU(2),GP16MU,WD16MU,YCUTSL(2),YCUTSB(2)
      COMMON/MUSGKN/   TMUCVO,TMU3VO
      CHARACTER*4 TMUCVO,TMU3VO
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
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
C       Check if there is any streamer in JDMUST
C
      IF (JDMUST .EQ. 0) RETURN
      IF (LROWS(JDMUST) .EQ. 0) RETURN
      NSTRM = LROWS (JDMUST)
C
C       Fill histograms of # of segments and # of streamers
C
      IF( FHISJO(8) )THEN
        X = NSTRM
        CALL HFILL(801,X)
        MSTRM = 0
        KMUST = JDMUST + LMHLEN
        DO  5 ISTRM=1,NSTRM
        JFIRE = IW(KMUST + JMUSFF)
        IF( JFIRE .EQ. 0 )GOTO 5
        MSTRM = MSTRM + 1
    5   KMUST = KMUST + LMUSTA
        X = MSTRM
        CALL HFILL(802,X)
      ENDIF
C
C       Create or link to BOS bank 'MUHT'
C
      JMUHT = IW(NAMUHT)
      IF( JMUHT .EQ. 0 )THEN
         ND = LMHLEN + NHTIN*LMUHTA
         CALL ALBOS ('MUHT',0,ND,JMUHT,IGARB)
         CALL BLIST(IW,'E+','MUHT')
         IW(JMUHT + LMHCOL) = LMUHTA
         IW(JMUHT + LMHROW) = 0
      ENDIF
C
C       Do loop for existing stremers in JDMUST
C
      DO 50 ISTRM=1,NSTRM
         KMUST = KROW(JDMUST,ISTRM)
         JFIRE = IW(KMUST + JMUSFF)
         IF( JFIRE .EQ.  0)GOTO 50
         JPLAN = IW(KMUST + JMUSPL)
         JEIGH = IW(KMUST + JMUSET)
         JTUBE = IW(KMUST + JMUSTU)
         Y1 = RW(KMUST + JMUSYI)
         Y2 = RW(KMUST + JMUSYO)
C
C       Deal with X-strip
C
C       X-strips are aligned with eight-fold tubes
C
         NX = (JEIGH-1)*8 + JTUBE
         R = RNDM(0.)
         IF( R .GT. SNSXMU(1) )GOTO 20
         CALL MUFLHT (JPLAN,1,NX)
C
C       Deal with adjacent strips
C
         IF( JTUBE .EQ. 1 )GOTO 10
         R = RNDM(0.)
         IF( R .GT. SNSXMU(2) )GOTO 10
         NS = NX - 1
         CALL MUFLHT (JPLAN,1,NS)
         IF( JTUBE .EQ. 2 )GOTO 10
         R = RNDM(0.)
         IF( R .GT. SNSXMU(3) )GOTO 10
         NS = NX - 2
         CALL MUFLHT (JPLAN,1,NS)
   10    IF( JTUBE .EQ. 8 )GOTO 20
         R = RNDM(0.)
         IF( R .GT. SNSXMU(2) )GOTO 20
         NS = NX + 1
         CALL MUFLHT (JPLAN,1,NS)
         IF( JTUBE .EQ. 7 )GOTO 20
         R = RNDM(0.)
         IF( R .GT. SNSXMU(3) )GOTO 20
         NS = NX + 2
         CALL MUFLHT (JPLAN,1,NS)
   20    CONTINUE
C
C       Deal with Y-strip
C
         NY1 = Y1/PTYSMU + 1
         IF (NY1 .GT. NYSTMU) GOTO 50
         NY2 = Y2/PTYSMU + 1
         DO 40 NS=NY1,NY2
            R = RNDM(0.)
            IF (R .GT. SNSYMU(1)) GOTO 40
            CALL MUFLHT (JPLAN,2,NS)
C
            IF( NS .NE. NY1 )GOTO 30
            R = RNDM(0.)
            IF( R .GT. SNSYMU(2) )GOTO 30
            N = NS - 1
            CALL MUFLHT (JPLAN,2,N)
            R = RNDM(0.)
            IF( R .GT. SNSYMU(3) )GOTO 30
            N = NS - 2
            CALL MUFLHT (JPLAN,2,N)
   30       IF( NS .NE. NY2 )GOTO 40
            R = RNDM(0.)
            IF( R .GT. SNSYMU(2) )GOTO 40
            N = NS + 1
            CALL MUFLHT (JPLAN,2,N)
            R = RNDM(0.)
            IF( R .GT. SNSYMU(3) )GOTO 40
            N = NS + 2
            CALL MUFLHT (JPLAN,2,N)
   40    CONTINUE
   50 CONTINUE
C
      END
