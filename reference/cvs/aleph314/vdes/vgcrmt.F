       SUBROUTINE VGCRMT (IWAFA,CORMAT,IER)
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN JULIA / USER
C!  Retrieves local wafer alignment covariance matrix from VALC
C - Alain Bonissent, March 1995
C
C   This routine returns the error correlation matrix from the
C   VALC bank (local alignment).  By definition, the error on the
C   global alignment is zero.
C
C   This operation was previously performed in VDHTER for VDET91.
C   The correspondence between the wafers and the VALC row numbers
C   was given by the routine VDINDX.  Accordingly, the VALC banks
C   for VDET91 contained 108 rows (=96 for the wafers + 12 dummies).
C
C   VGCRMT checks the wafer identifier in each row of VALC, so the
C   order of the rows is arbitrary.  The dummy rows are not needed.
C
C   The row and column indices of CORMAT are interchangeable, being
C   markers for the alignment quantities in the order:
C    V displacement,
C    U displacement,
C    W displacement,
C    V rotation,
C    U rotation,
C    W rotation.
C   The returned quantities are SQUARED errors (in the case of
C   diagonal elements).
C
C - Input:
C   IWAFA       / I  Decimal wafer address
C
C - Output:
C   CORMAT(6,6) / R  Covariance matrix
C   IER         / I  = 0 if successful
C                    = 1 if error occurred
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
      SAVE FIRST, NAVALC, IORD
#include "bcs.h"
#include "valcjj.h"
C
C     Arguments:
C
      INTEGER IWAFA, IER
      REAL CORMAT(6,6)
C
C     Local variables
C
      INTEGER NAVALC, KVALC, IVALC, NVALC, IWL, I, J, IORD(6,6)
      INTEGER NAMIND
      LOGICAL FIRST
#include "bmacrod.h"
C
      DATA IORD / 1, 2, 4, 7,11,16,
     >            2, 3, 5, 8,12,17,
     >            4, 5, 6, 9,13,18,
     >            7, 8, 9,10,14,19,
     >           11,12,13,14,15,20,
     >           16,17,18,19,20,21 /
      DATA FIRST / .TRUE. /
C
#include "bmacro.h"
C
C ----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVALC = NAMIND('VALC')
      ENDIF
C
      KVALC = IW(NAVALC)
      IER = 1
      IF (KVALC .EQ. 0) GO TO 999
      NVALC = LROWS(KVALC)
      DO 100 IVALC=1,NVALC
        IWL = ITABL(KVALC,IVALC,JVALWI)
        IF (IWAFA .EQ. IWL) THEN
          DO I=1,6
            DO J=1,6
              CORMAT(I,J) = RTABL(KVALC,IVALC,JVALEM-1+IORD(I,J))
            ENDDO
          ENDDO
          IER = 0
          GO TO 999
        ENDIF
  100 CONTINUE
C
  999 CONTINUE
      RETURN
      END
#endif
