      SUBROUTINE MUSPMD(XLOC,IOUFG)
C
C***********************************************************************
C
C T.Wang - 860728
C
C
C             modified for DAF and new bank layout
C             by F.Bossi,D.Kuhn,R.Xu 87-09-28
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C
C! find if the track element is in a special module
C     and change the geometric and readout constants.
C
C     Called by MUSTRM        in this .HLB
C
C***********************************************************************
      SAVE
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
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
      REAL XLOC(3)
      EXTERNAL MUNSTA
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
      IOUFG = 0
      NSLOT = ITRKEL(10)
C
C       Only the barrel modules 4, 9, and middle angle 4 are special
C
      IF( .NOT. ((TRKVOL.EQ.'MUB1').OR.(TRKVOL.EQ.'MUB2')) ) GOTO 900
      IF( .NOT. ((NSLOT.EQ.4) .OR. (NSLOT.EQ.9) .OR.
     *           (NSLOT.EQ.11)) ) GOTO 900
C
      IF( TRKVOL .EQ. 'MUB1' )THEN
         INDMD = 0
         INDSL = 0
      ELSE
         INDMD = 4
         INDSL = 17
      ENDIF
C
C       For top module, use X coordinate to define in which special
C       module the track element is, for bottom ones, use Y.
C
      IF( NSLOT .EQ. 4 )THEN
        OLDWD = 0.5*RTABL(JMBTG,INDMD+1,JMBTRB)
      WDMDMU = 0.5*RTABL(JMBTG,INDMD+4,JMBTRB)
      RX = RTABL(JMBTG,INDMD+4,JMBTRT)
      RY1 = RTABL(JMBTG,INDMD+4,JMBTZT)
      RY = XLMDMU - RY1
      XLOW = RTABL(JMBSG,INDSL+4,JMBSDE)
      XHGH = RTABL(JMBSG,INDSL+5,JMBSDE)
        X1 = - OLDWD + 2.*WDMDMU - RX
        X2 = OLDWD + 2.*XLOW
        X3 = 2.*XHGH - OLDWD
        X4 = OLDWD - 2.*WDMDMU + RX
        X = XLOC(1)
        ABSY = ABS(XLOC(2))
        INDPS = INDMD + 4
        RDIF = WDMDMU - RX
        NYSTMU = ITABL(JMBTG,INDPS,JMBTY1)
        OFSTMU = 0.
        IF( X .LE. X2 )THEN
          IF( (X.GT.X1) .AND. (ABSY.GT.RY) )GOTO 999
          IF(XLOC(1).GT.X1)THEN
               NYSTMU = ITABL(JMBTG,INDPS,JMBTY2)
               OFSTMU = RY1
          ENDIF
          XLOC(1) = XLOC(1) - XLOW
          IEMMU = INDSL+4
        ELSE IF( X .GE. X3 )THEN
          IF( (X.LT.X4) .AND. (ABSY.GT.RY) )GOTO 999
          IF(XLOC(1).LT.X4) THEN
               NYSTMU = ITABL(JMBTG,INDPS,JMBTY2)
               OFSTMU = RY1
           ENDIF
          XLOC(1) = XLOC(1) - XHGH
          IEMMU = INDSL+5
        ELSE
          GOTO 999
        ENDIF
        XLEIMU = FLOAT(NYSTMU)*PTYSMU + OFSTMU
      ELSE
        IF( NSLOT .EQ. 9 )THEN
           IROW = 9
        ELSE
           IROW = 13
        ENDIF
      NMROW = INDSL + IROW
      XLCNT = 0.5*RTABL(JMBTG,INDMD+3,JMBTZB)
      XLSID = 0.5*RTABL(JMBTG,INDMD+2,JMBTZB)
      RTOO = RTABL(JMBTG,INDMD+2,JMBTRT)
      ZTOO = RTABL(JMBTG,INDMD+2,JMBTZT)
      YMDL = RTABL(JMBSG,NMROW+2,JMBSZC)
      YLOW = RTABL(JMBSG,NMROW+3,JMBSZC)
      YHGH = RTABL(JMBSG,NMROW+1,JMBSZC)
        Y0 = YLOW + XLSID - ZTOO
        Y1 = YLOW + XLSID
        Y2 = YMDL - XLCNT
        Y3 = YMDL + XLCNT
        Y4 = YHGH - XLSID
        Y5 = YHGH - XLSID + ZTOO
        Y = XLOC(2)
        X = XLOC(1) + WDMDMU
C
        IF(NSLOT.EQ.9)THEN
            XTOO = RTOO
        ELSE
            XTOO = 2.*WDMDMU - RTOO
        ENDIF
C
        IF( Y .LE. Y1 )THEN
          IF(Y .GE. Y0 )THEN
           IF(NSLOT.EQ.9.AND.X.LE.XTOO) GO TO 999
           IF(NSLOT.EQ.11.AND.X.GE.XTOO) GO TO 999
          ENDIF
          XLOC(2) = Y - YLOW
          INDPS = INDMD + 2
          NYSTMU = ITABL(JMBTG,INDPS,JMBTY1)
          IEMMU = NMROW + 3
          XLMDMU = XLSID
        ELSE IF( Y .LT. Y2 )THEN
          GOTO 999
        ELSE IF( Y .LE. Y3 )THEN
          XLOC(2) = Y - YMDL
          INDPS = INDMD + 3
          NYSTMU = ITABL(JMBTG,INDPS,JMBTY1)
          IEMMU = NMROW + 2
          XLMDMU = XLCNT
        ELSE IF( Y .LT. Y4 )THEN
          GOTO 999
        ELSE
          IF(Y .LE. Y5 ) THEN
            IF(NSLOT.EQ.9.AND.X.LE.XTOO) GO TO 999
            IF(NSLOT.EQ.11.AND.X.GE.XTOO) GO TO 999
          END IF
          XLOC(2) = Y - YHGH
          INDPS = INDMD + 2
          NYSTMU = ITABL(JMBTG,INDPS,JMBTY1)
          IF(NSLOT.EQ.9.AND.X.LE.XTOO) NYSTMU =
     +                     ITABL(JMBTG,INDPS,JMBTY2)
          IF(NSLOT.EQ.11.AND.X.GE.XTOO) NYSTMU =
     +                     ITABL(JMBTG,INDPS,JMBTY2)
          IEMMU = NMROW + 1
          XLMDMU = XLSID
        ENDIF
      ENDIF
C
C
C       Get the readout constants
C
      NUEMMU    = ITABL(JMBSG,IEMMU,JMBSID)
      NXSTMU    = ITABL(JMBTG,INDPS,JMBTNX)
      NYSTMU    = ITABL(JMBTG,INDPS,JMBTY1)
      XLEIMU    = FLOAT(NYSTMU)*PTYSMU
C
  900 CONTINUE
C                    Middle Angle slot 4
      IF( .NOT. ((TRKVOL.EQ.'MUM1').OR.(TRKVOL.EQ.'MUM2')) ) GOTO 950
      IF( .NOT. ((NSLOT.EQ.4) )) GOTO 950
      NUMV = MUNSTA(NSLOT,TRKVOL,TMU3VO)
      IDXMD = ITABL(JMMSG,NUMV,JMMSK2)
      RTOO = RTABL(JMMTG,IDXMD,JMMTRT)
      ZTOO = RTABL(JMMTG,IDXMD,JMMTZT)
      X1 = -WDMDMU + ZTOO
      Y = XLOC(2) + XLMDMU
      IF(TMU3VO.EQ.'MUEB') THEN
          XTOO = 2.*XLMDMU - RTOO
          IF(XLOC(1).LE.X1.AND.Y.GE.XTOO) GO TO 999
      ELSE
          XTOO = RTOO
          IF(XLOC(1).LE.X1.AND.Y.LE.XTOO) GO TO 999
      ENDIF
  950 RETURN
  999 IOUFG = -1
      RETURN
      END
