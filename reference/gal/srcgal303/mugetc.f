      SUBROUTINE MUGETC
C***********************************************************************
C
C T.Wang -851216
C
C             modified for DAF and new bank layout
C             by F.Bossi,D.Kuhn,R.Xu 87-09-19
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C
C! get the constants needed by the process "Create MU signals"
C        from BOS bank 'M*RD' and to fill them in common
C       block /MUGNCN/ and /MUSGCN/.
C       Common block /MUGNCN/ keeps the general information of muon
C       detector and is filled only once, common block /MUSGCN/
C       keeps the information of the volume in which the current
C       track element is.
C
C       Called by MUHIT
C       Calls     MUNSTA              in this .HLB
C
C***********************************************************************
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
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
C! The parameters needed by the geometry routine AGMUCH
      PARAMETER (NMBIN = 12, NMBOU = 12 )
      PARAMETER (NMMIN = 10, NMMOU =  9, NMMA = NMMIN+NMMOU, IMMBT =  9)
      PARAMETER (NMCIN = 4, NMCOU = 4, NMCA = NMCIN+NMCOU)
      PARAMETER (NMMBI = NMMA+NMMIN, NMCBI = NMCA+NMCIN)
      COMMON /MUG1PR/   MMADPR(12,4)
C
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
C       Now, the constants dependent on the volume
C       The function MUNSTA finds the address in BOS bank by means
C       of the slot number etc.
C
      NSLOT = ITRKEL(10)
      TMUCVO = TRKVOL
      TMU3VO = CHAHOL (NGAMES(3))
      NUMV = MUNSTA(NSLOT,TMUCVO,TMU3VO)
      IF( TMUCVO(1:3).EQ.'MUB' )THEN
C                     Barrel
         DO 7 IJ=1,34
         NM=ITABL(JMBSG,IJ,JMBSVO)
         IF(IJ.GT.17)NM=NM+12
         IF(NM.NE.NUMV) GO TO 7
         IDXMD = ITABL(JMBSG,IJ,JMBSK2)
         XSOFMU(1) = RTABL(JMBSG,IJ,JMBST1)
         XSOFMU(2) = RTABL(JMBSG,IJ,JMBST1+1)
         IJ1=IJ
         IF(IJ.GT.17)IJ1=IJ-17
         IF(IJ1.GE.2.AND.IJ1.LE.7) THEN
              XSOFMU(1) = RTABL(JMBSG,IJ,JMBST1+1)
              XSOFMU(2) = RTABL(JMBSG,IJ,JMBST1)
         ENDIF
         R2 = RTABL(JMBSG,IJ,JMBSRC)
         NUEMMU = ITABL(JMBSG,IJ,JMBSID)
         GO TO 8
 7       CONTINUE
 8       WDMDMU = 0.5*RTABL(JMBTG,IDXMD,JMBTRB)
         XLMDMU = 0.5*RTABL(JMBTG,IDXMD,JMBTZB)
         YSOFMU(1) = RTABL(JMBAG,1,JMBAY1)
         YSOFMU(2) = RTABL(JMBAG,1,JMBAY2)
         DL = RTABL(JMBAG,1,JMBAPD)
         IF(NUMV.LE.12) THEN
            R1 = RTABL(JMBAG,1,JMBAR1)
         ELSE
            R1 = RTABL(JMBAG,1,JMBAR2)
         END IF
         IF(NSLOT.GE.7.OR.NSLOT.EQ.1) THEN
              ZPSWMU(2) = R2-R1
              ZPSWMU(1) = R2-R1+DL
         ELSE
              ZPSWMU(1) = R2-R1
              ZPSWMU(2) = R2-R1+DL
         ENDIF
         YCUTSB(1) = 0.
         YCUTSB(2) = 0.
         YCUTSL(1) = 0.
         YCUTSL(2) = 0.
         NYSTMU = ITABL(JMBTG,IDXMD,JMBTY1)
         NXSTMU = ITABL(JMBTG,IDXMD,JMBTNX)
         WD16MU = RTABL(JMBTG,IDXMD,JMBTW1)
         XLEIMU = FLOAT(NYSTMU)*PTYSMU
         NMEIMU = NXSTMU/8
         GO TO 99
C
      ELSE IF( TMUCVO(1:3).EQ.'MUM'  )THEN
C                   Middle Angle
         IDXMD = ITABL(JMMSG,NUMV,JMMSK2)
         WDMDMU = 0.5*RTABL(JMMTG,IDXMD,JMMTZB)
         XLMDMU = 0.5*RTABL(JMMTG,IDXMD,JMMTRB)
         THCKM = RTABL(JMMAG,1,JMMATB)
         Z0PLAN = RTABL(JMMAG,1,JMMAZ0)
         IF(NSLOT.EQ.10) THEN
            Z0PLAN = RTABL(JMMAG,1,JMMAZ1)
            THCKM = RTABL(JMMAG,1,JMMATH)
         ENDIF
         DL = RTABL(JMMAG,1,JMMAPD)
         IF(NSLOT.GE.2.AND.NSLOT.LE.6) THEN
             ZPSWMU(1) = Z0PLAN - 0.5*THCKM
             ZPSWMU(2) = ZPSWMU(1) + DL
         ELSE
             ZPSWMU(1) = -Z0PLAN + 0.5*THCKM
             ZPSWMU(2) = ZPSWMU(1) - DL
         ENDIF
         DEAZ = RTABL(JMMAG,1,JMMADS)
         NYSTMU = ITABL(JMMSG,NUMV,JMMSNY)
         TL = RTABL(JMMSG,NUMV,JMMSTL)
         TR = RTABL(JMMSG,NUMV,JMMSRL)
C                B side
      IF(NUMV.GE.20) THEN
          YSOFMU(1) = RTABL(JMMSG,NUMV,JMMSL1)
          YSOFMU(2) = RTABL(JMMSG,NUMV,JMMSL2)
          YCUTSB(1) = DEAZ + TL - YSOFMU(1)
          YCUTSB(2) = DEAZ + TL - YSOFMU(2)
          YCUTSL(1) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TR
     +                + RTABL(JMMSG,NUMV,JMMSR1)
          YCUTSL(2) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TR
     +                + RTABL(JMMSG,NUMV,JMMSR2)
          IF(NSLOT.GE.2.AND.NSLOT.LE.6) THEN
             YSOFMU(1) = RTABL(JMMSG,NUMV,JMMSR1)
             YSOFMU(2) = RTABL(JMMSG,NUMV,JMMSR2)
             YCUTSB(1) = DEAZ + TR - YSOFMU(1)
             YCUTSB(2) = DEAZ + TR - YSOFMU(2)
             YCUTSL(1) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TL
     +                   + RTABL(JMMSG,NUMV,JMMSL1)
             YCUTSL(2) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TL
     +                   + RTABL(JMMSG,NUMV,JMMSL2)
          ENDIF
      ELSE
C                 A side
          YSOFMU(1) = RTABL(JMMSG,NUMV,JMMSR1)
          YSOFMU(2) = RTABL(JMMSG,NUMV,JMMSR2)
          YCUTSB(1) = DEAZ + TR - YSOFMU(1)
          YCUTSB(2) = DEAZ + TR - YSOFMU(2)
          YCUTSL(1) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TL
     +                + RTABL(JMMSG,NUMV,JMMSL1)
          YCUTSL(2) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TL
     +                + RTABL(JMMSG,NUMV,JMMSL2)
          IF(NSLOT.GE.2.AND.NSLOT.LE.6) THEN
             YSOFMU(1) = RTABL(JMMSG,NUMV,JMMSL1)
             YSOFMU(2) = RTABL(JMMSG,NUMV,JMMSL2)
             YCUTSB(1) = DEAZ + TL - YSOFMU(1)
             YCUTSB(2) = DEAZ + TL - YSOFMU(2)
             YCUTSL(1) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TR
     +                   + RTABL(JMMSG,NUMV,JMMSR1)
             YCUTSL(2) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TR
     +                   + RTABL(JMMSG,NUMV,JMMSR2)
           ENDIF
      ENDIF
      XSOFMU(1) = RTABL(JMMSG,NUMV,JMMSX1)
      XSOFMU(2) = RTABL(JMMSG,NUMV,JMMSX2)
      WD16MU = RTABL(JMMAG,1,JMMAPI)
      NXSTMU = ITABL(JMMTG,IDXMD,JMMTNX)
      NMEIMU = NXSTMU/8
      NUEMMU = NUMV +200
C
      ELSE IF( TMUCVO(1:3).EQ.'MUC' )THEN
C                   End Caps
         IDXMD = ITABL(JMESG,NUMV,JMESK2)
         XLMDMU = 0.5*RTABL(JMETG,IDXMD,JMETXB)
         WDMDMU = 0.5*RTABL(JMETG,IDXMD,JMETYB)
         NXSTMU = ITABL(JMETG,IDXMD,JMETNX)
         NYSTMU = ITABL(JMETG,IDXMD,JMETNY)
         NMEIMU = NXSTMU/8
         HTCK = 0.5*RTABL(JMECG,1,JMECTH)
         R1 = RTABL(JMECG,1,JMECZI)
         IF(NUMV.GT.4.AND.NUMV.LT.9)  R1 = RTABL(JMECG,1,JMECZE)
         IF(NUMV.GT.12) R1 = RTABL(JMECG,1,JMECZE)
         DL = RTABL(JMECG,1,JMECPD)
         ZPSWMU(1) = R1 - HTCK
         ZPSWMU(2) = R1 - HTCK + DL
         XSOFMU(1) = RTABL(JMETG,IDXMD,JMETX1)
         XSOFMU(2) = RTABL(JMETG,IDXMD,JMETX2)
         WD16MU = RTABL(JMETG,IDXMD,JMETPI)
         IMDM = MOD(NUMV,2)
      IF(NUMV.LT.9.AND.IMDM.NE.0.OR.NUMV.GE.9.AND.IMDM.EQ.0) THEN
        XSOFMU(1) = 2.*WDMDMU-(FLOAT(NXSTMU)/16.)*WD16MU-XSOFMU(1)
        XSOFMU(2) = 2.*WDMDMU-(FLOAT(NXSTMU)/16.)*WD16MU-XSOFMU(2)
      ENDIF
         YSOFMU(1) = RTABL(JMETG,IDXMD,JMETYS)
         YSOFMU(2) = RTABL(JMETG,IDXMD,JMETYS)
         DEAZ = RTABL(JMECG,1,JMECDZ)
         X0 = RTABL(JMECG,1,JMECXO)
         YCUTSB(1) = DEAZ + X0 - YSOFMU(1)
         YCUTSB(2) = DEAZ + X0 - YSOFMU(2)
         YCUTSL(1) = X0 - DEAZ - YSOFMU(1)
         YCUTSL(2) = X0 - DEAZ - YSOFMU(2)
         NUEMMU = NUMV + 100
      ENDIF
C
 99   CONTINUE
C
      RETURN
      END
