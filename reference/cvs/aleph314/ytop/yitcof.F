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
#ifndef DOC
      SAVE
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "ficljj.h"
#include "itcojj.h"
C
C----------------------------------------------------------*
      REAL XYZ(3,20),RITC(20),FITC(20),ZITC(20)
      LOGICAL LDMP
C----------------------------------------------------------*
#include "bmacro.h"
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
#endif
