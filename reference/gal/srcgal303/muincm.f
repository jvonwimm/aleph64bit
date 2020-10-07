      SUBROUTINE MUINCM(NSLOT,IPLANE,NSTRPF,NAST,NBUS,NSTRPB,IFLAG)
C-----------------------------------------------------------
C!  Transforms m.angle slot and strip nb. to astros and bus nb.
C
C  Author :   A. Antonelli, F. Bossi 1 July 89
C.  -Called by MUDGTZ                  from this .HLB
C.  -Calls none
C.
C-----------------------------------------------------------
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
C! The parameters needed by the geometry routine AGMUCH
      PARAMETER (NMBIN = 12, NMBOU = 12 )
      PARAMETER (NMMIN = 10, NMMOU =  9, NMMA = NMMIN+NMMOU, IMMBT =  9)
      PARAMETER (NMCIN = 4, NMCOU = 4, NMCA = NMCIN+NMCOU)
      PARAMETER (NMMBI = NMMA+NMMIN, NMCBI = NMCA+NMCIN)
      COMMON /MUG1PR/   MMADPR(12,4)
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
C
      SAVE
        PARAMETER(NROW1=25)
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
C            Read astros type and astros number
        ISLOT = NSLOT - 200
        NTPAS= ITABL(JMMSG,ISLOT,JMMSTA)
        NAST = ITABL(JMMSG,ISLOT,JMMSNA)
        NOSL = ITABL(JMMSG,ISLOT,JMMSOS)
        IDZFL = ITABL(JMMSG,ISLOT,JMMSDZ)
        INAS = 2*(NTPAS - 1)
        IBTP = ITABL(JMMBG,NROW1,JMMBL1+INAS)
        IPOIN = 2*(IBTP - 1)
C                Bus type  1= X1 =1  Nb. in MUHT
C                          2= Y1 =2
C                          3= X2 =4
C                          4= Y2 =3
C                          5= X3 =1
C                          6= Y3 =2
C                          7= X4 =4
C                          8= Y4 =3
C.......
        NBUSTY= IPLANE+4
        IF(ISLOT.LE.10) NBUSTY = IPLANE
        IF(ISLOT.GE.20.AND.ISLOT.LE.29) NBUSTY = IPLANE
        IF(IPLANE.EQ.3) NBUSTY = NBUSTY + 1
        IF(IPLANE.EQ.4) NBUSTY = NBUSTY - 1
C
        IREM=MOD(NBUSTY,2)
        IF(IREM.EQ.0) THEN
C
                NMAXY = ITABL(JMMSG,ISLOT,JMMSNY)
                IF(ISLOT.GE.2.AND.ISLOT.LE.6)
     +             NSTRPF = - (NSTRPF - NMAXY) - 1
                IF(ISLOT.GE.12.AND.ISLOT.LE.16)
     +             NSTRPF = - (NSTRPF - NMAXY) - 1
                IF(ISLOT.GE.21.AND.ISLOT.LE.25)
     +             NSTRPF = - (NSTRPF - NMAXY) - 1
                IF(ISLOT.GE.31.AND.ISLOT.LE.35)
     +             NSTRPF = - (NSTRPF - NMAXY) - 1
         ELSE
                 JT = ITABL(JMMSG,ISLOT,JMMSK2)
                 NMAXX = ITABL(JMMTG,JT,JMMTNX)
                 IF(ISLOT.LE.19) THEN
                   IF(IDZFL.EQ.-1) NSTRPF = - (NSTRPF - NMAXX) - 1
                 ELSE
                   IF(IDZFL.EQ.1) NSTRPF = - (NSTRPF - NMAXX) - 1
                 ENDIF
           ENDIF
C
        KK = 0
        NBUS = 0
        NSTRPB = 0
        IFLAG = 0
        DO 1 J=1,NROW1-1
        IFIND = ITABL(JMMBG,J,JMMBB1+IPOIN)
        IFSL = ITABL(JMMBG,J,JMMBO1+IPOIN)
        IF(IFIND.NE.NBUSTY.OR.(IFSL+NOSL).NE.ISLOT) GO TO 1
        KK=KK+1
        IF(KK.EQ.1)NSTPF1=NSTRPF+ITABL(JMMBG,J,JMMBL1+INAS)
        LFIN= ITABL(JMMBG,J,JMMBU1+INAS)
        IF(NSTPF1.GT.LFIN) GO TO 1
        LIN = ITABL(JMMBG,J,JMMBL1+INAS)
        NBUS = J - 1
        NSTRPB = NSTPF1 - LIN
        IF(KK.EQ.1) NSTRPB=NSTPF1
        IFLAG = 1
        GO TO 3
 1      CONTINUE
 3      CONTINUE
        RETURN
        END
