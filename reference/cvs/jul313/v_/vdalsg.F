      SUBROUTINE VDALSG(IBREAL,JWAF,IV,XYZ,TNG,SIGMA)
C----------------------------------------------------------------------
C!  VDALSG calculates the sigmas to smear the VDZT (IV=1) or VDXY (IV=2)
C!         bank.
C!
C -  Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C!
C!  Description
C!  ===========
C!  Loads and unpacks the error matrix from the database
C!  Calls VDSIGM to calculate the SIGMA
C
C  Input :  IBREAL - the VALC row number
C           JWAF  - the global wafer number
C           IV    - the view
C           XYZ   - the global coordinates of the hit
C           TNG   - the cotangent of the angle that the track makes
C                   with the wafer, projected to the view
C
C  Output : SIGMA - the smear
C
C -----------------------------------------------------------------------
#ifndef DOC
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bcs.h"
      INTEGER NAVALC, KVALC, IRNUM, IDUM, IND, IERR, JWAF, IBREAL, IV
      REAL TNG
      INTEGER NAMIND, VXYZVU
      EXTERNAL NAMIND, VXYZVU
C-- local declarations
      REAL XYZ(3), VUW(3), AMAT(21), EMAT(6,6), SIGMA
      SAVE NAVALC
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVALC = NAMIND('VALC')
      ENDIF
      KVALC = IW(NAVALC)
      IF (KVALC.LE.0) THEN
        CALL RERROR('VDALSG',1,'No alignment matrix!')
        RETURN
      ENDIF
C-- find the row number corresponding to the banknumber
      IRNUM = 0
      DO IDUM = 1, LROWS(KVALC)
        IF (ITABL(KVALC,IDUM,1).EQ.IBREAL) THEN
          IRNUM = IDUM
          GOTO 11
        ENDIF
      ENDDO
C-- load the alignment error matrix
 11   CONTINUE
      IND = KROW(KVALC,IRNUM)+8
C-- copy the triangular mat into a local array
      CALL UCOPY(RW(IND),AMAT,21)
C-- unpack the triangular matrix.  Note that the UCOPY is not really
C-- necessary and RW(IND) can be passed directly to TRUPCK
      CALL TRUPCK(AMAT,EMAT,6)
C-- go to the local coordinates VUW
      IERR = VXYZVU(XYZ,JWAF,VUW)
C-- calculate the Z and RPHI sigma
      CALL VDSIGM(EMAT,IV,VUW,TNG,SIGMA)
      RETURN
      END
#endif
