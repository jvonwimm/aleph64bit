      SUBROUTINE QGFOXW
CKEY SHAPE /INTERNAL
C----------------------------------------------------------------------
C   Author   : E. Blucher     1-MAR-1989
C
C   Description
C   ===========
C!  Calculate Fox-Wolfrom moments H0 -- H4.
C
C        QTBOR(1), ..., QTBOR(5) = H0, ..., H4
C======================================================================
#ifndef DOC
#include "qcde.h"
#include "qctbuf.h"
C
C---Legendre polynomials as statement functions (from DLK).
C
      PLEG2(X)=(3.*X**2-1.)/2.
      PLEG3(X)=(5.*X**3-3.*X)/2.
      PLEG4(X)=(35.*X**4-30.*X**2+3.)/8.
C-----------------------------------------------------------------------
      IERR=0
      DO 10 I=1,5
        QTBOR(I)=0.
   10 CONTINUE
      IF (QELEP.LT.1.)THEN
        IERR=1
        GOTO 999
      ENDIF
      IF(KTBI.LT.2)THEN
        IERR=2
        GOTO 999
      ENDIF
      H0=0.
      H1=0.
      H2=0.
      H3=0.
      H4=0.
      SSS=QELEP**2
      DO 101 I=1,KTBI
        PPARI=QTBIX(I)**2 + QTBIY(I)**2 + QTBIZ(I)**2
        DO 102 J=I,KTBI
          IF(J .NE. I)THEN
            PPARJ=QTBIX(J)**2 + QTBIY(J)**2 + QTBIZ(J)**2
            CXANG=(QTBIX(I)*QTBIX(J) + QTBIY(I)*QTBIY(J)
     1             + QTBIZ(I)*QTBIZ(J))/SQRT(PPARI*PPARJ)
C----------Factor of 2 for i,j and j,i combinations.
            DELH0=2.*SQRT(PPARI*PPARJ)
            H0=H0+DELH0
            H1=H1+DELH0*CXANG
            H2=H2+DELH0*PLEG2(CXANG)
            H3=H3+DELH0*PLEG3(CXANG)
            H4=H4+DELH0*PLEG4(CXANG)
          ELSE
            DELH0 = PPARI
            H0 = H0 + DELH0
            H1 = H1 + DELH0
            H2 = H2 + DELH0
            H3 = H3 + DELH0
            H4 = H4 + DELH0
          ENDIF
  102   CONTINUE
  101 CONTINUE
C---normalize and store result.
      QTBOR(1)=H0/SSS
      QTBOR(2)=H1/SSS
      QTBOR(3)=H2/SSS
      QTBOR(4)=H3/SSS
      QTBOR(5)=H4/SSS
  999 CONTINUE
      END
#endif
