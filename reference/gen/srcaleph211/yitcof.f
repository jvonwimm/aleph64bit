      SUBROUTINE YITCOF (ITR,NUIT,XYZ,RITC,FITC,ZITC,LDMP)
C
C----------------------------------------------------------*
C!    find ITC coordinates on a track
CKEY YTOP ITC
C!    Author :     W. Maenner    /06/91
C!    Modified :   M. Bosman   23/07/91
C!
C!    Description
C!    ===========
C! find IT coordinates for track ITR
C! NCOM is no. of complete hits (ie rphi and z coordinates)
C----------------------------------------------------------*
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      PARAMETER(JFICII=1,LFICLA=1)
      PARAMETER(JITCWN=1,JITCRA=2,JITCP1=3,JITCP2=4,JITCZH=5,JITCSR=6,
     +          JITCSZ=7,JITCDT=8,LITCOA=8)
C
C----------------------------------------------------------*
      REAL XYZ(3,20),RITC(20),FITC(20),ZITC(20)
      LOGICAL LDMP
C----------------------------------------------------------*
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
C
      LOUT = IW(6)
C
      DO 50 I=1,20
        XYZ(1,I)=-100000.
        XYZ(2,I)=-100000.
        XYZ(3,I)=-100000.
   50 CONTINUE
C
C FIND TRACK ITR IN FRTL BANK
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
      IF(IFAIL.GT.0) THEN
        RETURN
      ENDIF
      IF(ITR.GT.LROWS(KFRTL)) THEN
        RETURN
      ENDIF
      NUIT=ITABL(KFRTL,ITR,JFRTNI)
      NCOM=NUIT
      INIT=ITABL(KFRTL,ITR,JFRTII)
      IF(LDMP) WRITE(LOUT,*)'TRACK NUMBER NUIT OFFSET',ITR,NUIT,INIT
      IF(NUIT.LE.0) RETURN
C NU OF COORD IN FICL
      KFICL = IW(NAMIND('FICL'))
      KITCO = IW(NAMIND('ITCO'))
      IF(LDMP) WRITE(LOUT,*)'KFICL,KITCO',KFICL,KITCO
      DO 60 I=1,NUIT
        IICL0=ITABL(KFICL,INIT+I,JFICII)
        IF(LDMP) WRITE(LOUT,*)'HIT NB, INDEX',I,IICL0
        IICL=IABS(IICL0)
COORD IN ITCO
        RITC(I)=RTABL(KITCO,IICL,JITCRA)
        IF(LDMP) WRITE(LOUT,*)' RITC ',RITC(I)
        IF(IICL0.GT.0) THEN
          FITC(I)=RTABL(KITCO,IICL,JITCP1)
        ELSE
          FITC(I)=RTABL(KITCO,IICL,JITCP2)
        ENDIF
        ZITC(I)=RTABL(KITCO,IICL,JITCZH)
        XYZ(1,I)=RITC(I)*COS(FITC(I))
        XYZ(2,I)=RITC(I)*SIN(FITC(I))
        XYZ(3,I)=ZITC(I)
        IF(LDMP)
     &    WRITE(LOUT,999)
     &    (XYZ(J,I),J=1,3),RITC(I),FITC(I)
  999   FORMAT('  XYZ F1/2,R,F1/2 ',5F10.3)
   60 CONTINUE
      RETURN
      END
