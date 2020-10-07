      SUBROUTINE YBIPAT(ITR,IBVDET,IBITPC,IBMVD,IBMIT,IBMTP,IFLAG,LDMP)
C----------------------------------------------------------*
C!    Find bit pattern of hits in VDET ITC and TPC
CKEY YTOP
C!    Author :     W. Manner 25/01/93
C!
C!    Description
C!    ===========
C!     For track ITR the bit pattern is found and stored in
C!     INTEGER arrays IBMVD, IBMIT, IBMTP 1 word/bit and packed
C!     in words IBVDET IBITPC
C!     If bank DTRA exists (MINI) it is used and unpacked.
C!     Otherwise the banks VDCO ITCO TPCO are used
C!     IBVDET gives a somewhat more detailed VDET bit pattern
C!     than DTRA. It is:
C!     bit 1:  1st r-phi hit in layer 1
C!     bit 2:  1st   z   hit in layer 1
C!     bit 3:  2nd r-phi hit in layer 1 (overlap)
C!     bit 4:  2nd   z   hit in layer 1 (overlap)
C!     bit 5:  1st r-phi hit in layer 2
C!     bit 6:  1st   z   hit in layer 2
C!     bit 7:  2nd r-phi hit in layer 2 (overlap)
C!     bit 8:  2nd   z   hit in layer 2 (overlap)
C!     If this pattern is filled from DTRA
C!     it is fudged.
C!     IBITPC is identical to DTRA.JDTRHO
C----------------------------------------------------------*
      SAVE
      INTEGER IBMVD(8),IBMIT(8),IBMTP(21)
      REAL XYZ(3,4),RVDC(4),FVDC(4),ZVDC(4)
      LOGICAL LDMP
      PARAMETER(JITCWN=1,JITCRA=2,JITCP1=3,JITCP2=4,JITCZH=5,JITCSR=6,
     +          JITCSZ=7,JITCDT=8,LITCOA=8)
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      PARAMETER(JDTRCH=1,JDTRP0=2,JDTRTH=3,JDTRPH=4,JDTRD0=5,JDTRZ0=6,
     +          JDTRER=7,JDTREM=12,JDTRTF=16,JDTRHO=17,JDTRHM=18,
     +          JDTRDE=19,JDTRNS=20,JDTRTL=21,JDTRVB=22,JDTRDC=23,
     +          LDTRAA=23)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      PARAMETER(JFICII=1,LFICLA=1)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C ----------------------------------------------------------
      IFLAG=1
      IBVDET=0
      IBITPC=0
      CALL UZERO(IBMVD,1,8)
      CALL UZERO(IBMIT,1,8)
      CALL UZERO(IBMTP,1,21)
C if bank DTRA exists (MINI) fill bit pattern from DTRHO
      KDTRA=IW(NAMIND('DTRA'))
      IF(KDTRA.EQ.0) GO TO 100
      IWD=ITABL(KDTRA,ITR,JDTRHO)
      IBITPC=IWD
C Fudge VDET bit pattern
      IB=1
      DO IL=1,2
        IF(IAND(IWD,IB).NE.0) THEN
          IBMVD(1+(IL-1)*4) = 1
          IBVDET=IBVDET+2**((IL-1)*4)
          IBMVD(2+(IL-1)*4) = 1
          IBVDET=IBVDET+2**((IL-1)*4+1)
        ENDIF
        IB=IB*2
      ENDDO
C Fill ITC bit pattern
      DO IL=1,8
        IF(IAND(IWD,IB).NE.0) THEN
          IBMIT(IL)=1
        ENDIF
        IB=IB*2
      ENDDO

C Fill TPC bit pattern
      DO IL=1,21
        IF(IAND(IWD,IB).NE.0) THEN
          IBMTP(IL)=1
        ENDIF
        IB=IB*2
      ENDDO
      RETURN

  100 CONTINUE
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
C Find VDET bit pattern
      CALL YVDCOF(ITR,NUVD,XYZ,RVDC,FVDC,ZVDC,NCOM,.FALSE.)
      KBI=0
      DO IV=1,NUVD
        KBI=KBI+1
        IF(KBI.LE.2.AND.RVDC(IV).GT.8.) KBI=3
        IF(XYZ(1,IV).LT.900.) THEN
          IBMVD(1+(KBI-1)*2) = 1
          IBVDET=IBVDET+2**((KBI-1)*2)
        ENDIF
        IF(XYZ(3,IV).LT.900.) THEN
          IBMVD(2+(KBI-1)*2) = 1
          IBVDET=IBVDET+2**(1+(KBI-1)*2)
        ENDIF
      ENDDO
      IF(IBMVD(1).EQ.1.AND.IBMVD(2).EQ.1) IBITPC=IBITPC+1
      IF(IBMVD(5).EQ.1.AND.IBMVD(6).EQ.1) IBITPC=IBITPC+2

C Find ITC bit pattern
      IOIT=ITABL(KFRTL,ITR,JFRTII)
      NNIT=ITABL(KFRTL,ITR,JFRTNI)
      KFICL=IW(NAMIND('FICL'))
      KITCO=IW(NAMIND('ITCO'))
      IF(KFRTL.EQ.0.OR.KFICL.EQ.0.OR.KITCO.EQ.0) RETURN
      DO IT=1,NNIT
        KOR=ABS(ITABL(KFICL,IOIT+IT,1))
        IWI=ITABL(KITCO,KOR,JITCWN)
        IB=IWI/1000
        IBMIT(IB)=1
        IBITPC=IBITPC+2**(IB-1) * 4
      ENDDO
C Find TPC bit pattern
      IOTP=ITABL(KFRTL,ITR,JFRTIT)
      NNTP=ITABL(KFRTL,ITR,JFRTNT)
      KFTCL=IW(NAMIND('FTCL'))
      KTPCO=IW(NAMIND('TPCO'))
      IF(KFTCL.EQ.0.OR.KTPCO.EQ.0) RETURN
      DO IT=1,NNTP
        KOR=ITABL(KFTCL,IOTP+IT,1)
        IWI=ITABL(KTPCO,KOR,JTPCIN)
        IB=IWI/100000
        IBMTP(IB)=1
        IBITPC=IBITPC+2**(IB-1) * 2**10
      ENDDO
      IFLAG=0
      END
