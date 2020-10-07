      SUBROUTINE VDGTXL(IV,NR,IHIT,XYZ,TNG,IERR)
C ----------------------------------------------------------------------
C!  Given the VDZT (IV=1) or VDXY (IV=2) bank number (NR)
C!  and the hit number (IHIT), this truth finding routine returns
C!  the true hit position XYZ (and the cotangent of the angle
C!  between the track and wafer normal projected to the view)
C!  if possible (IERR=0), if not IERR is non-zero.
C!
C -  Author         Alain Bonissent and Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C
C  Input :   IV    - view
C            NR    - bank number for VDZT or VDXY
C            IHIT  - row number in the bank
C
C  Output :  XYZ   - true position from the VDHT bank
C            TNG   - the cotangent of the angle that the track makes
C                    with the wafer, projected to the view
C            IERR  - zero if sucess
C
C -----------------------------------------------------------------------
#ifndef DOC
      INTEGER VJFACI, NAMIND, VJWABW, VXYZWA
      EXTERNAL VJFACI, NAMIND, VJWABW, VXYZWA
      REAL XYZ(3), XI(3), XO(3)
      INTEGER NAVDHT, IV, NR, IHIT, IVDHT, IERR, KVDHT, ILAY, IFAC, IER,
     $   JFAC, I, JWAF
      REAL TNG, RTAN(2)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE NAVDHT
#include "bcs.h"
#include "vdhtjj.h"
#include "bmacro.h"
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVDHT=NAMIND('VDHT')
      ENDIF
      KVDHT=IW(NAVDHT)
      CALL VDGTHT(IV,NR,IHIT,IVDHT,IERR)

C  if the VDHT bank could be found, return the accurate coordinates
      IF (IERR.EQ.0) THEN
        DO 1 I = 1,3
          XI(I) = RTABL(KVDHT,IVDHT,JVDHXE-1+I)
          XO(I) = RTABL(KVDHT,IVDHT,JVDHXL-1+I)
          XYZ(I) = (XI(I)+XO(I))/2.0
 1      CONTINUE
        IFAC = ITABL(KVDHT,IVDHT,JVDHPN)
        ILAY = ITABL(KVDHT,IVDHT,JVDHLN)
        IER = VJFACI(ILAY,IFAC,JFAC)
        CALL VDTAN(IV,JFAC,XI,XO,TNG)
      ELSE
C  else return the center of the wafer (read-out module) as the possible
C  position
        IER = VJWABW(NR, JWAF)
        IER = VXYZWA(JWAF,XYZ)
C  return the rough estimate of the cotangent
        CALL VDRTAN(JWAF,XYZ,RTAN)
        TNG = RTAN(IV)
      ENDIF
 999  CONTINUE
      RETURN
      END
#endif
