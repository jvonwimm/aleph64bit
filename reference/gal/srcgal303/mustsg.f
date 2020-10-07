      SUBROUTINE MUSTSG(XLOC,DCLOC)
C
C***********************************************************************
C
C T.Wang -851212
C
C
C             modified for DAF and new bank layout
C             by F.Bossi,D.Kuhn,R.Xu 87-09-24
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C
C!Calculate segments of a track element in active
C       area of tubes in two tube planes with straight extrapolation.
C
C       Input :
C             XLOC(*)  -- Coordinates of track element
C             DCLOC(*) -- Direction cosine of track element
C       Output :
C             in BOS bank JDMUST
C
C       Called by MUSTRM
C       Calls ALBOS                        in this .HLB
C             BLIST                  in BOS
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
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
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
      REAL XLOC(3),DCLOC(3)
      COMMON /GCVOLU/ NGLEVE,NGAMES(15),NGUMBR(15),
     &  LGVOLU(15),LGINDX(15),IGNFRO,NGLVMX,NGLDEV(15),LGINMX(15),
     &  GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      PARAMETER(JMUOID=1,JMUOVR=2,JMUONS=4,JMUOWI=5,JMUOHE=6,JMUOTU=7,
     +          JMUOAC=8,JMUOSP=9,JMUODI=10,JMUODE=11,JMUOXS=12,
     +          JMUOYS=13,JMUOSE=14,JMUOET=15,JMUOEX=16,JMUOEY=17,
     +          JMUOX1=18,JMUOX2=19,JMUOX3=20,JMUOX4=21,JMUOY1=22,
     +          JMUOY2=23,JMUOY3=24,JMUOY4=25,LMUOGA=25)
      PARAMETER(JMBAID=1,JMBAVR=2,JMBASU=4,JMBANB=5,JMBATH=6,JMBALE=7,
     +          JMBAR1=8,JMBAR2=9,JMBAPD=10,JMBAY1=11,JMBAY2=12,
     +          LMBAGA=12)
      PARAMETER(JMBSID=1,JMBSVR=2,JMBSNO=4,JMBSVO=5,JMBSZC=6,JMBSRC=7,
     +          JMBSDE=8,JMBST1=9,JMBST2=10,JMBSTA=11,JMBSNA=12,
     +          JMBSK1=13,JMBSK2=14,LMBSGA=14)
      PARAMETER(JMBTID=1,JMBTVR=2,JMBTNA=4,JMBTZB=5,JMBTRB=6,JMBTY1=7,
     +          JMBTY2=8,JMBTNX=9,JMBTZT=10,JMBTRT=11,JMBTW1=12,
     +          LMBTGA=12)
      PARAMETER(JMBBID=1,JMBBVR=2,JMBBB1=4,JMBBL1=5,JMBBU1=6,JMBBB2=7,
     +          JMBBL2=8,JMBBU2=9,JMBBB3=10,JMBBL3=11,JMBBU3=12,
     +          JMBBB4=13,JMBBL4=14,JMBBU4=15,JMBBP3=16,JMBBP4=17,
     +          LMBBGA=17)
      PARAMETER(JMECID=1,JMECVR=2,JMECSU=4,JMECNS=5,JMECZI=6,JMECZE=7,
     +          JMECPD=8,JMECTH=9,JMECDZ=10,JMECXO=11,LMECGA=11)
      PARAMETER(JMEBID=1,JMEBVR=2,JMEBB1=4,JMEBL1=5,JMEBU1=6,JMEBB2=7,
     +          JMEBL2=8,JMEBU2=9,JMEBB3=10,JMEBL3=11,JMEBU3=12,
     +          JMEBB4=13,JMEBL4=14,JMEBU4=15,LMEBGA=15)
      PARAMETER(JMETID=1,JMETVR=2,JMETNA=4,JMETXB=5,JMETYB=6,JMETX1=7,
     +          JMETX2=8,JMETYS=9,JMETPI=10,JMETNX=11,JMETNY=12,
     +          JMETNP=13,JMETN2=14,JMETN1=15,JMETLE=16,LMETGA=40)
      PARAMETER(JMESID=1,JMESVR=2,JMESNO=4,JMESXC=5,JMESYC=6,JMESZC=7,
     +          JMESTA=8,JMESNA=9,JMESK1=10,JMESK2=11,LMESGA=11)
      PARAMETER(JMMAID=1,JMMAVR=2,JMMASU=4,JMMANS=5,JMMAZ0=6,JMMAPD=7,
     +          JMMATH=8,JMMAPI=9,JMMADS=10,JMMAZ1=11,JMMATB=12,
     +          LMMAGA=12)
      PARAMETER(JMMBID=1,JMMBVR=2,JMMBNO=4,JMMBB1=5,JMMBO1=6,JMMBB2=7,
     +          JMMBO2=8,JMMBB3=9,JMMBO3=10,JMMBB4=11,JMMBO4=12,
     +          JMMBB5=13,JMMBO5=14,JMMBL1=15,JMMBU1=16,JMMBL2=17,
     +          JMMBU2=18,JMMBL3=19,JMMBU3=20,JMMBL4=21,JMMBU4=22,
     +          JMMBL5=23,JMMBU5=24,JMMBL6=25,JMMBU6=26,JMMBL7=27,
     +          JMMBU7=28,JMMBL8=29,JMMBU8=30,JMMBL9=31,JMMBU9=32,
     +          JMMBL0=33,JMMBU0=34,LMMBGA=34)
      PARAMETER(JMMTID=1,JMMTVR=2,JMMTNA=4,JMMTZB=5,JMMTRB=6,JMMTZT=7,
     +          JMMTRT=8,JMMTNX=9,LMMTGA=9)
      PARAMETER(JMMSID=1,JMMSVR=2,JMMSNO=4,JMMSL1=5,JMMSL2=6,JMMSR1=7,
     +          JMMSR2=8,JMMSTL=9,JMMSRL=10,JMMSNY=11,JMMSX1=12,
     +          JMMSX2=13,JMMSDZ=14,JMMSZC=15,JMMSRC=16,JMMSDE=17,
     +          JMMSNA=18,JMMSTA=19,JMMSOS=20,JMMSVO=21,JMMSK1=22,
     +          JMMSK2=23,LMMSGA=23)
      EXTERNAL MUNSTA
      CHARACTER*4 CHAHOL
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
C!     INDICES OF MUON-BANKS FOR USE WITHIN ROUTINES
      JMBAG = IW(NAMBAG)
      JMUOG = IW(NAMUOG)
      JMBBG = IW(NAMBBG)
      JMBSG = IW(NAMBSG)
      JMBTG = IW(NAMBTG)
C
      JMECG = IW(NAMECG)
      JMEBG = IW(NAMEBG)
      JMETG = IW(NAMETG)
      JMESG = IW(NAMESG)
C
      JMMAG = IW(NAMMAG)
      JMMBG = IW(NAMMBG)
      JMMTG = IW(NAMMTG)
      JMMSG = IW(NAMMSG)
C
C
C       Do loop for two tube planes
C
C
      TXY = DCLOC(1)/DCLOC(2)
      TXZ = DCLOC(1)/DCLOC(3)
      TYZ = DCLOC(2)/DCLOC(3)
      DO 30 IPLN=1,2
C
C       Straight extrapolate track element to up and down face of
C       active area for one tube plane
C
        ZUP = ZPSWMU(IPLN) + HFHTMU - XLOC(3)
        ZDN = ZPSWMU(IPLN) - HFHTMU - XLOC(3)
        XUP = XLOC(1) + ZUP*TXZ + WDMDMU - XSOFMU(IPLN)
        XDN = XLOC(1) + ZDN*TXZ + WDMDMU - XSOFMU(IPLN)
        YUP = XLOC(2) + ZUP*TYZ + XLMDMU - YSOFMU(IPLN)
        YDN = XLOC(2) + ZDN*TYZ + XLMDMU - YSOFMU(IPLN)
        IF( XUP .LE. XDN )THEN
          X1 = XUP
          Y1 = YUP
          Z1 = ZUP
          X2 = XDN
          Y2 = YDN
          Z2 = ZDN
        ELSE
          X1 = XDN
          Y1 = YDN
          Z1 = ZDN
          X2 = XUP
          Y2 = YUP
          Z2 = ZUP
        ENDIF
C
        IF(X1.LE.0..AND.X2.LE.0.) GO TO 30
C       Calculate the eight-fold tube # and tube # taking the gap
C       between two 16-fold tubes into account
C
        L1 = X1/WD16MU
        XMOD1 = MOD(X1,WD16MU)
        M1 = XMOD1/WDEIMU
        N1 = 2*L1 + M1 + 1
        IF( M1 .EQ. 2 )THEN
          NT1 = 1
        ELSE
          NT1 = (X1 - L1*WD16MU - M1*WDEIMU - OFTBMU )/
     *            WDTBMU + 1
        ENDIF
C
        L2 = X2/WD16MU
        XMOD2 = MOD(X2,WD16MU)
        M2 = XMOD2/WDEIMU
        IF( M2 .EQ. 2 )THEN
          N2 = 2*L2 + M2
          NT2 = 8
        ELSE
          N2 = 2*L2 + M2 + 1
          NT2 = (X2 - L2*WD16MU - M2*WDEIMU - OFTBMU)/
     *            WDTBMU + 1
        ENDIF
        IF(M1.EQ.2.AND.M2.EQ.2) GO TO 30
        IF( (N1.LE.0) .AND. (N2.LE.0) )THEN
          GOTO 30
        ELSE IF( (N1.GT.NMEIMU) .AND. (N2.GT.NMEIMU) )THEN
          GOTO 30
        ENDIF
C
C       The first and last eight-fole tubes, also the
C       first tube insidethe first eight-fole tube and
C       thelast tube inside the last eight-fole tube
C
        IF( N1 .LE.  0) THEN
          N1 = 1
          NT1 = 1
        ENDIF
        IF( N2 .GT. NMEIMU ) THEN
          IF(NT1.EQ.9.AND.N2.EQ.(N1+1)) GO TO 30
          N2 = NMEIMU
          NT2 = 8
        ENDIF
C
        IF(NT1.LE.0.AND.NT2.LE.0) GO TO 30
        IF(NT1.GE.9.AND.NT2.GE.9) GO TO 30
        IF(NT1.GE.9.AND.NT2.LE.0) THEN
          IF(N2.EQ.(N1+1)) THEN
            GO TO 30
          ELSE
            NT1=1
            NT2=8
            N1=N1+1
            N2=N2-1
          ENDIF
        ENDIF
        IF(NT1.LE.0.AND.NT2.GE.9) THEN
          NT1=1
          NT2=8
        ENDIF
        IF(NT1.LE.0) NT1=1
        IF(NT2.LE.0) THEN
          IF(M1.EQ.2.AND.N1.EQ.N2) GO TO 30
          NT2=8
          IF(N1.NE.N2)N2=N2-1
        ENDIF
        IF(NT2.GE.9) NT2=8
        IF(NT1.GE.9) THEN
          IF(M2.EQ.2.AND.N1.EQ.N2) GO TO 30
          NT1=1
          IF(N1.NE.N2)N1=N1+1
        ENDIF
C
C    check bank size
C
        MXWRD = (N2-N1+1)*8 * LMUSTA
        IF (JDMUST .EQ. 0) THEN
          CALL WBANK (IW,JDMUST,2*MXWRD+LMHLEN,*998)
          IW(JDMUST+1) = LMUSTA
        ELSEIF (LFRWRD(JDMUST) .LT. MXWRD) THEN
          CALL WBANK (IW,JDMUST,IW(JDMUST)+MXWRD,*998)
        ENDIF
        KMUST = KNEXT(JDMUST)
C
C       Loop of eightfold tubes
C
        DO 20 IEIT=N1,N2
C
C       Fetch length of eightfold tube for endcap and m.a., because
C       it is variable
C
          IF( (TMUCVO.EQ.'MUC1') .OR. (TMUCVO.EQ.'MUC2') )THEN
            NSLOT = ITRKEL(10)
            TMUCVO = TRKVOL
            TMU3VO = CHAHOL(NGAMES(3))
            NUMV = MUNSTA(NSLOT,TMUCVO,TMU3VO)
            IDXMD = ITABL(JMESG,NUMV,JMESK2)
            NPRSER = ITABL(JMETG,IDXMD,JMETNP)
            NEDA2 = ITABL(JMETG,IDXMD,JMETN2)
            NEDA1 = ITABL(JMETG,IDXMD,JMETN1)
            IDX1 = MOD(IDXMD,2)
            IF (TMU3VO.EQ.'MUEA') THEN
              IF(IDX1.EQ.0) THEN
                I8TU = IEIT
              ELSE
                I8TU = -(IEIT - NMEIMU)
              ENDIF
            ELSE
              IF(IDX1.EQ.0) THEN
                I8TU = -(IEIT - NMEIMU)
              ELSE
                I8TU = IEIT
              ENDIF
            ENDIF
            IF(I8TU.LT.NPRSER) THEN
              IR = 1
            ELSE
              IF(I8TU.LE.(NPRSER + 2*NEDA2)) THEN
                IDTU = I8TU - NPRSER
                IRES = MOD(IDTU,2)
                IR = IDTU/2 + IRES + 1
              ELSE
                IR = NEDA2 + 2
              ENDIF
            ENDIF
            XLEIMU = RTABL(JMETG,IDXMD,JMETN1+IR) + YCUTSL(IPLN)
          ENDIF
C
          IF( (TMUCVO.EQ.'MUM1') .OR. (TMUCVO.EQ.'MUM2') )
     +         XLEIMU = YCUTSL(IPLN)
C
C       # of tube in eightfold tube
C
          IF( IEIT .EQ. N1 )THEN
            NTB1 = NT1
          ELSE
            NTB1 = 1
          ENDIF
          IF( IEIT .EQ. N2 )THEN
            NTB2 = NT2
          ELSE
            NTB2 = 8
          ENDIF
C
C       Loop of tubes
C
          DO 10 ITUB=NTB1,NTB2
C
            IF( IEIT.EQ.N1) THEN
              IF( ITUB.EQ.NTB1 ) THEN
                IF( M1 .EQ. 2 )THEN
                  XIN = (L1+1)*WD16MU + OFTBMU
                  ZIN = Z1 + (XIN-X1)/TXZ
                ELSE
                  XIN = X1
                  XT = XIN - L1*WD16MU - M1*WDEIMU -
     *                       OFTBMU - (ITUB-1)*WDTBMU
                  IF( XT .GT. WDATMU )GOTO 10
                  ZIN = Z1
                ENDIF
              ELSE
                XIN = L1*WD16MU + M1*WDEIMU +
     *                        OFTBMU + (ITUB-1)*WDTBMU
                ZIN = Z1 + (XIN-X1)/TXZ
              ENDIF
              XOUT = L1*WD16MU + M1*WDEIMU +
     *                OFTBMU + (ITUB-1)*WDTBMU + WDATMU
              ZOUT = Z1 + (XOUT-X1)/TXZ
            ENDIF
C
            IF( IEIT.EQ.N2 ) THEN
              IF( ITUB.EQ.NTB2 ) THEN
                IF( M2 .EQ. 2 )THEN
                  XOUT = L2*WD16MU + 2*WDEIMU +
     *                              WDATMU - WDTBMU
                  ZOUT = Z1 + (XOUT-X1)/TXZ
                ELSE
                  XOUT = X2
                  ZOUT = Z2
                  XT = XOUT - L2*WD16MU - M2*WDEIMU -
     *                           OFTBMU - (ITUB-1)*WDTBMU
                  IF( XT .GT. WDATMU )THEN
                    XOUT = L2*WD16MU + M2*WDEIMU +
     *                                OFTBMU + (ITUB-1)*WDTBMU +
     *                                WDATMU
                    ZOUT = Z1 + (XOUT-X1)/TXZ
                  ENDIF
                ENDIF
              ELSE
                XOUT = L2*WD16MU + M2*WDEIMU + OFTBMU +
     *                           (ITUB-1)*WDTBMU + WDATMU
                ZOUT = Z1 + (XOUT-X1)/TXZ
              ENDIF
              IF(N1.NE.N2) THEN
                XIN = L2*WD16MU + M2*WDEIMU +
     *                   OFTBMU + (ITUB-1)*WDTBMU
                ZIN = Z1 + (XIN-X1)/TXZ
              ENDIF
            ENDIF
            IF (IEIT.NE.N1.AND.IEIT.NE.N2) THEN
              LL = IEIT/2
              IM = IEIT-1
              MM = MOD(IM,2)
              XIN = LL*WD16MU + MM*WDEIMU +
     *                    OFTBMU + (ITUB-1)*WDTBMU
              ZIN = Z1 + (XIN-X1)/TXZ
              XOUT = LL*WD16MU + MM*WDEIMU + OFTBMU +
     *                     (ITUB-1)*WDTBMU + WDATMU
              ZOUT = Z1 + (XOUT-X1)/TXZ
            ENDIF
            YIN =  Y1 + (ZIN-Z1)*TYZ
            YOUT = Y1 + (ZOUT-Z1)*TYZ

C
C       Check if part or whole of track element is outside of tube
C       in Y-direction, considering both ends.
C
            IF( YIN .GT. YOUT )THEN
              REG = YIN
              YIN = YOUT
              YOUT = REG
              REG = XIN
              XIN = XOUT
              XOUT = REG
              REG = ZIN
              ZIN = ZOUT
              ZOUT = REG
            ENDIF
C
            IF( YIN .GE. XLEIMU )THEN
              GOTO 10
            ELSE IF( YIN .GE. YCUTSB(IPLN) )THEN
              IF( YOUT .GT. XLEIMU )THEN
                XOUT = XOUT + (XLEIMU - YOUT)*TXY
                ZOUT = ZOUT + (XLEIMU - YOUT)/TYZ
                YOUT = XLEIMU
              ENDIF
            ELSE
              IF( YOUT .GT. YCUTSB(IPLN) )THEN
                XIN = XIN - YIN*TXY
                ZIN = ZIN - YIN/TYZ
                YIN = YCUTSB(IPLN)
              ELSE
                GOTO 10
              ENDIF
            ENDIF
C
C       Fill bank JDMUST
C
            IW(JDMUST+LMHROW) = LROWS(JDMUST) + 1
            IW(KMUST + JMUSFF) = 0
            IW(KMUST + JMUSPL) = IPLN
            IW(KMUST + JMUSET) = IEIT
            IW(KMUST + JMUSTU) = ITUB
            RW(KMUST + JMUSXI ) = XIN
            RW(KMUST + JMUSYI ) = YIN
            RW(KMUST + JMUSZI ) = ZIN
            RW(KMUST + JMUSXO) = XOUT
            RW(KMUST + JMUSYO) = YOUT
            RW(KMUST + JMUSZO) = ZOUT
            RW(KMUST + JMUSXY ) = TXY
            RW(KMUST + JMUSXZ ) = TXZ
            RW(KMUST + JMUSYZ ) = TYZ
            KMUST = KMUST + LMUSTA
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      RETURN
C
  998 CONTINUE
      CALL ALTELL ('MUSTSG: not enough space to enlarge JDMUST ',1,
     &             'NEXT')
      RETURN
      END
