               SUBROUTINE HDEADM
C-------------------------------------------------------------------
CKEY HCALDES HCAL DEAD TUBE  / INTERNAL
C
C!   Create and fill Look_up Tables for Hcal dead Tubes
C!
C!                            Author:G.Catanesi 1/08/89
C!                            Modify:L.Silvestris 07/07/90
C!
C - Banks : read HTXD
C         : read HRDT
C-------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C!    GEOMETRY COMMONS FOR HADRON CALORIMETER
      PARAMETER( LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER( LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER( LPHCT = 4)
      PARAMETER( LPHCBM = 24,LPHCES = 6)
      COMMON /HCALGE/HCRMIN(LSHC),HCRMAX(LSHC),HCZMIN(LSHC),HCZMAX(LSHC)
     &              , NHCSUB,NHCTWR,NHCPHC,NHCREF,IHCTID(LHCRE-1)
     &              , HCTUTH,HCIRTH,HCLSLA,NHCPLA(LPHC),HCTIRF(LPHC)
     &              , HCSMTH
      COMMON / HBAR / NHCBMO,NHCBFS,NHCBBS,NHCBLA,HCLTNO(LHCNO)
     &              , HCWINO(LHCNO),HCDEWI,NHBLA2,NHBLI3,NHBLO3
     &              , NEITHC(LHCNL),NEITSP(LHCNL,LHCSP)
     &              , HCSPLT(LHCNL,LHCSP), HFSPBL,HFSRBL,HCPHOF
C
      COMMON /HEND/  HCDREC,HCDSTP,HCAPSL,HFSPEC,NHCSEX
     &           ,NHCEFS,NHCEBS,NHCTRE,NHCINL,NHCOUL
     &           ,NHCIND,NHCOUD
C
       PARAMETER (LHCBL=4,LHCEI=10,LHCEO=20,LHNLA=4)
      COMMON /HCCONS/ HCTHRF,HCRSIZ,HCZSIZ,NHCBAR,NHCECA
     &               ,NHCEIT,HCEIWI,HCDOWI,HCTUGA,HCSEPO
     &               ,HCSABL,HCSAEC,HCTUEN,XLNHCE(LHCBL)
     &               ,HCTLEI(LHNLA,LHCEI),HCTLEO(LHNLA,LHCEO)
     &               ,HCTAEI(LHCEI),HCTAEO(LHCEO),HTINBL,HTINEC(2)
     &               ,HTPIEC,HTPOEC,HBWREC,HBWCEC(2),HBSREC
     &               ,HBSCEC,HBWRBL,HBSCBL,NHMBDF(2),NHTY4D
     &               ,NHTY3D,NHTY2D,NHMBFL(2),NHBDOU,NHDLEC
     &               ,NHDET0,NHDEBS,NHL8EC(LHCNL-1),HCTUSH,XHCSHI(LHCBL)

      PARAMETER (LHCTR1=LHCTR+1)
      COMMON /HCSEVA/ NTHCFI(LHCRE),HCAPDE(LPHCT),HCFITW(LHCRE)
     &               ,HCBLSP(LHCNL,LHCSP),NHCTU1(LHCNL),HCTHUL(LHCTR1)
     &               ,PHCTOR(LHCTR),IHCREG(LHCTR)
     &               ,HCLARA(LHCNL),HCLAWI(LHCNL)
     &               ,YBAST1,YBARMX,ZENST1,ZENDMX
     &               ,XBARR0,XENDC0
C
C
C! global HCAL data base HAC parameters
      PARAMETER(LHBDN8=23,LHBDT8=46,LHBDSP=46)
      PARAMETER(LHETLT=4,LHETLI=40,LHETLO=80,LHETAI=10,LHETAO=20)
      PARAMETER(LHSBXS=3,LHSBRS=3)
      PARAMETER(LHSCST=4)
      PARAMETER(LHSEXS=3,LHSERS=3)
      PARAMETER(LHTIIE=2,LHTIPE=2,LHTUMN=2,LHTUMS=2)
      PARAMETER(JHBAID=1,JHBAVR=2,JHBASN=4,JHBANS=5,JHBARP=6,JHBARR=9,
     +          JHBANF=12,JHBANB=13,JHBANL=14,LHBARA=14)
      PARAMETER(JHBDID=1,JHBDVR=2,JHBDL1=4,JHBDW1=5,JHBDL2=6,JHBDW2=7,
     +          JHBDL3=8,JHBDW3=9,JHBDDW=10,JHBDT2=11,JHBDT3=12,
     +          JHBDNB=13,JHBDN8=14,JHBDT8=37,JHBDSP=83,JHBDHB=129,
     +          LHBDEA=129)
      PARAMETER(JHBGID=1,JHBGVR=2,JHBGYI=4,JHBGYX=5,JHBGZX=6,JHBGIT=7,
     +          JHBGRA=8,JHBGPO=9,JHBGHB=10,LHBGEA=10)
      PARAMETER(JHCAID=1,JHCAVR=2,JHCACN=4,JHCANS=5,JHCANT=6,JHCANP=7,
     +          JHCANR=8,JHCAIT=9,JHCAIH=10,JHCAGA=11,JHCATF=12,
     +          JHCATL=13,LHCALA=13)
      PARAMETER(JHCCID=1,JHCCVR=2,JHCCTR=4,JHCCRR=5,JHCCZR=6,JHCCNB=7,
     +          JHCCNE=8,LHCCOA=8)
      PARAMETER(JHCTID=1,JHCTVR=2,JHCTEP=4,JHCTBP=5,JHCTET=6,JHCTBT=7,
     +          JHCTTM=8,LHCTGA=193)
      PARAMETER(JHEDID=1,JHEDVR=2,JHEDWT=4,JHEDST=5,JHEDDS=6,JHEDFP=7,
     +          JHEDHE=8,LHEDEA=8)
      PARAMETER(JHEGID=1,JHEGVR=2,JHEGZI=4,JHEGZX=6,JHEGRI=8,JHEGRX=10,
     +          JHEGHE=12,LHEGEA=12)
      PARAMETER(JHEMID=1,JHEMVR=2,JHEMMT=4,JHEMXM=5,JHEMYM=13,JHEMNI=21,
     +          JHEMNO=22,JHEMHS=23,LHEMTA=23)
      PARAMETER(JHENID=1,JHENVR=2,JHENSN=4,JHENNS=5,JHENRP=6,JHENRR=9,
     +          JHENSG=12,JHENNF=13,JHENNB=14,JHENNW=15,JHENNL=16,
     +          LHENDA=17)
      PARAMETER(JHEPID=1,JHEPVR=2,JHEPHN=4,JHEPMT=5,JHEPHE=6,JHEPHS=7,
     +          LHEPMA=7)
      PARAMETER(JHETID=1,JHETVR=2,JHETNT=4,JHETEW=5,JHETDW=6,JHETTG=7,
     +          JHETE2=8,JHETIZ=9,JHETLT=10,JHETLI=14,JHETLO=54,
     +          JHETAI=134,JHETAO=144,LHETCA=163)
      PARAMETER(JHRDEA=1,LHRDTA=1)
      PARAMETER(JHSBID=1,JHSBVR=2,JHSBEN=4,JHSBSN=5,JHSBMT=6,JHSBXS=7,
     +          JHSBRS=10,JHSBPM=13,JHSBHB=14,LHSBAA=14)
      PARAMETER(JHSCID=1,JHSCVR=2,JHSCHC=4,JHSCST=5,JHSCTS=9,LHSCOA=9)
      PARAMETER(JHSEID=1,JHSEVR=2,JHSEFN=4,JHSESN=5,JHSESL=6,JHSEMT=7,
     +          JHSEXS=8,JHSERS=11,JHSEPM=14,JHSEHE=15,LHSECA=15)
      PARAMETER(JHTIIB=1,JHTIIE=2,JHTICI=4,JHTICO=5,JHTIPC=6,JHTIPE=7,
     +          JHTIPR=9,JHTIPB=10,JHTIPL=11,JHTIPO=12,LHTIDA=12)
      PARAMETER(JHTUMN=1,JHTUN4=3,JHTUN3=4,JHTUN2=5,JHTUMS=6,JHTUN8=8,
     +          JHTULN=9,JHTUN0=10,JHTUNF=11,LHTUEA=11)
      PARAMETER(JHTXEA=1,LHTXDA=1)
C! Hcal Look_up Tables of dead tubes,Tower ecc.ecc.
      COMMON/HCDETU/ MH8FDB(LPHCBM,LHCNL),MH8FDE(LPHCES*2,LHCNL-1,2)
     &              ,LSH8DB(LPHCBM,LHCNL),LSH8DE(LPHCES*2,LHCNL-1,2)
C
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
C? Set to zero the Barrel table
      DO 1 I=1,LPHCBM
         DO 2 J=1,LHCNL
            MH8FDB(I,J)=0
            LSH8DB(I,J)=0
 2       CONTINUE
 1    CONTINUE
C
C  Set to zero the EndCap table
C
      DO 3 K=1,LPHCES*2
         DO 4 N1= 1,LHCNL-1
            DO 5 N2=1,2
               MH8FDE(K,N1,N2) = 0
               LSH8DE(K,N1,N2) = 0
 5          CONTINUE
 4       CONTINUE
 3    CONTINUE
C
C     Look the unrecoverable dead tubes
C
      JHTXD = IW(NAMIND( 'HTXD'))
      IF(JHTXD.EQ.0) GOTO 90
C
      NHTXD = LROWS(JHTXD)
      IF(NHTXD.EQ.0)GOTO 90
C
C? Loop on bank HTDX
      DO 6 K=1,NHTXD
C
         IADR  =  ITABL(JHTXD,K,JHTXEA)
C
C? Decode the Adress: IADR ==> ITUB, ILAY, IMOD, IPOR
C! decode the tube address
      NFLROR = IADR
      ITUB   = MOD (NFLROR,1000)
      NFLROR = NFLROR/1000
      ILAY   = MOD (NFLROR,100)
      NFLROR = NFLROR/100
      IMOD   = MOD (NFLROR,100)
      IPOR   = NFLROR/100
C
C? Fill the Look_up Table
      IF(IPOR.EQ.LPBAR)THEN
C?     Barrel Case
         IF(ITUB.GT.NEITHC(ILAY))THEN
C           even module
            ITUB = 2*NEITHC(ILAY)+1 - ITUB
            IMOD = IMOD*2
         ELSE
C           odd module
            IMOD = IMOD*2 -1
         ENDIF
         MH8FDB(IMOD,ILAY) = MH8FDB(IMOD,ILAY)+2**ITUB
      ELSE
C?     EndCap
         IF(IPOR.EQ.LPECB)IMOD = IMOD + LPHCES
         IF(MOD(ITUB,2).EQ.0)THEN
            ITUB = ITUB/2
            IDTU = 2
         ELSE
            ITUB = ITUB/2 + 1
            IDTU = 1
         ENDIF
         MH8FDE(IMOD,ILAY,IDTU) = MH8FDE(IMOD,ILAY,IDTU) + 2**ITUB
      ENDIF
C
 6    CONTINUE
C
90    CONTINUE
C
C     Look the recoverable dead tubes
C
      JHRDT = IW(NAMIND( 'HRDT'))
      IF(JHRDT.EQ.0) GOTO 99
C
      NHRDT = LROWS(JHRDT)
      IF(NHRDT.EQ.0)GOTO 99
C
C? Loop on bank HRDT
      DO 16 K=1,NHRDT
C
         IADR  =  ITABL(JHRDT,K,JHRDEA)
C
C? Decode the Adress: IADR ==> ITUB, ILAY, IMOD, IPOR
C! decode the tube address
      NFLROR = IADR
      ITUB   = MOD (NFLROR,1000)
      NFLROR = NFLROR/1000
      ILAY   = MOD (NFLROR,100)
      NFLROR = NFLROR/100
      IMOD   = MOD (NFLROR,100)
      IPOR   = NFLROR/100
C
C? Fill the Look_up Table
      IF(IPOR.EQ.LPBAR)THEN
C?     Barrel Case
         IF(ITUB.GT.NEITHC(ILAY))THEN
C           even module
            ITUB = 2*NEITHC(ILAY)+1 - ITUB
            IMOD = IMOD*2
         ELSE
C           odd module
            IMOD = IMOD*2 -1
         ENDIF
         LSH8DB(IMOD,ILAY) = LSH8DB(IMOD,ILAY)+2**ITUB
      ELSE
C?     EndCap
         IF(IPOR.EQ.LPECB)IMOD = IMOD + LPHCES
         IF(MOD(ITUB,2).EQ.0)THEN
            ITUB = ITUB/2
            IDTU = 2
         ELSE
            ITUB = ITUB/2 + 1
            IDTU = 1
         ENDIF
         LSH8DE(IMOD,ILAY,IDTU) = LSH8DE(IMOD,ILAY,IDTU) + 2**ITUB
      ENDIF
C
16    CONTINUE
C
99    CONTINUE
      RETURN
      END
