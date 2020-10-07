      SUBROUTINE VDGTHT(IV,NRBK,IHIT,IVDHT,IERR)
C ----------------------------------------------------------------------
C!  Given the VDZT (IV=1) or VDXY (IV=2) bank number (NRBK)
C!  and the hit number (IHIT), this truth finding routine returns
C!  the true hit number IVDHT in VDHT bank
C!
C -  Author         Alain Bonissent  5-Nov-1994
C! Modified November 1996, handle properly strips which 
C!                belong to two clusters (after splitting)
CKEY V_DECK VDET
C
C  Input :   IV    - view
C            NRBK  - bank number for VDZT or VDXY
C            IHIT  - row number in the bank
C
C  Output :  IVDHT - true hit row number in the VDHT bank
C            IERR  - zero if sucess
C
C ----------------------------------------------------------------------
#ifndef DOC
      INTEGER MAXHIT
      PARAMETER(MAXHIT = 50)
      INTEGER NLINK
      INTEGER NSTR(MAXHIT),JVDHT(MAXHIT), IERR, IVDHT, NMAX, IV, KVDZT,
     $   KVDXY, IH, IHIT, NBA, IKOD, NMATC, IT, NRBK
      REAL QFRAC(MAXHIT), QMAX
#include "vdztjj.h"
#include "vdxyjj.h"
#include "bcs.h"
#include "bmacro.h"
      IERR=0
C
C--  Prevent returning the previous ivdht, if nmatc=0
C
      IVDHT = 0
      NMAX=MAXHIT
      IF(IV.EQ.1)THEN
         KVDZT=NLINK('VDZT',NRBK)
         IH=ITABL(KVDZT,IHIT,JVDZIH)
         NBA=ITABL(KVDZT,IHIT,JVDZIW)
         NRVS = NRBK
      ELSEIF(IV.EQ.2)THEN
         KVDXY=NLINK('VDXY',NRBK)
         IH = ITABL(KVDXY,IHIT,JVDXIH)
         NBA = ITABL(KVDXY,IHIT,JVDXIW)
         NRVS = NRBK+1
      ENDIF
      IKOD = 1
      CALL VTRURL(IKOD,NBA,IH,IHIT,NRVS,NMAX,
     $   NMATC,JVDHT,NSTR,QFRAC,IERR)
      IF(IERR.NE.0)GO TO 999
      QMAX=0.
      DO 10 IT=1,NMATC
      IF(QFRAC(IT).GT.QMAX.AND.JVDHT(IT).NE.0)THEN
         IVDHT=JVDHT(IT)
         QMAX=QFRAC(IT)
      ENDIF
   10 CONTINUE
      IF(IVDHT.EQ.0) THEN
         IERR=-20
C this will happen often now delta ray production is simulated
C         CALL RERROR('VDGTHT',1,'No match IVDHT=0')
      ENDIF
  999 CONTINUE
      RETURN
      END
#endif
