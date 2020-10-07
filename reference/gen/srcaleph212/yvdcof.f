      SUBROUTINE YVDCOF (ITR,NUVD,XYZ,RVDC,FVDC,ZVDC,NCOM,LDMP)
C
C----------------------------------------------------------*
C!    find VDET coordinates on a track
CKEY YTOP
C!    Author :     W. Maenner    /06/91
C!    Modified :   M. Bosman   23/07/91
C!
C!    Description
C!    ===========
C! find vd coordinates for track ITR
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
      PARAMETER(JFVCIV=1,LFVCLA=1)
      PARAMETER(JVDCWI=1,JVDCR0=2,JVDCPH=3,JVDCZ0=4,JVDCSR=5,JVDCSZ=6,
     +          JVDCQF=7,JVDCTN=8,LVDCOA=8)
C
C----------------------------------------------------------*
      REAL XYZ(3,4),RVDC(4),FVDC(4),ZVDC(4)
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
      DO 50 I=1,4
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
      NUVD=ITABL(KFRTL,ITR,JFRTNV)
      NCOM=NUVD
      INVD=ITABL(KFRTL,ITR,JFRTIV)
      IF(LDMP) WRITE(LOUT,*)'TRACK NUMBER NUVD',ITR,NUVD
      IF(NUVD.LE.0) RETURN
C NU OF COORD IN FVCL
      KFVCL = IW(NAMIND('FVCL'))
      KVDCO = IW(NAMIND('VDCO'))
      IF(KFVCL.EQ.0.OR.KVDCO.EQ.0) THEN
        NCOM=-1
        RETURN
      ENDIF
      DO 60 I=1,NUVD
        IVCL=ITABL(KFVCL,INVD+I,JFVCIV)
COORD IN VDCO
        LLLL=ITABL(KVDCO,IVCL,JVDCTN)
        RVDC(I)=RTABL(KVDCO,IVCL,JVDCR0)
        FVDC(I)=RTABL(KVDCO,IVCL,JVDCPH)
        ZVDC(I)=RTABL(KVDCO,IVCL,JVDCZ0)
        IDWA=ITABL(KVDCO,IVCL,1)
        XYZ(1,I)=RVDC(I)*COS(FVDC(I))
        XYZ(2,I)=RVDC(I)*SIN(FVDC(I))
        XYZ(3,I)=ZVDC(I)
C SET DUMMY VALUES (ERROR 1000) TO 1000
        IF(RTABL(KVDCO,IVCL,JVDCSR).GE.999.) THEN
          XYZ(1,I)=1000.
          XYZ(2,I)=1000.
        ENDIF
        IF(RTABL(KVDCO,IVCL,JVDCSZ).GE.999.) THEN
          XYZ(3,I)=1000.
        ENDIF
        IF(XYZ(1,I).EQ.1000..OR.XYZ(3,I).EQ.1000.) NCOM=NCOM-1
        IF(LDMP)
     +    WRITE(LOUT,*)' XYZ R F',(XYZ(J,I),J=1,3),RVDC(I),FVDC(I),IDWA
   60 CONTINUE
      RETURN
      END
