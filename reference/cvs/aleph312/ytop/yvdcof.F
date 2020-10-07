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
#ifndef DOC
      SAVE
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "fvcljj.h"
#include "vdcojj.h"
C
C----------------------------------------------------------*
      REAL XYZ(3,4),RVDC(4),FVDC(4),ZVDC(4)
      LOGICAL LDMP
C----------------------------------------------------------*
#include "bmacro.h"
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
#if defined(YDEBUG)
        WRITE(LOUT,*) ' NO FRTL BANK, RETURN '
#endif
        RETURN
      ENDIF
      IF(ITR.GT.LROWS(KFRTL)) THEN
#if defined(YDEBUG)
        WRITE(LOUT,*) ' EV TRK NO',ITR,' GT FRTL',LROWS(KFRTL)
#endif
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
#endif
