      SUBROUTINE VDGTMV(IROW,AVEC)
C ----------------------------------------------------------------------
C!  VDGTMV unpacks the alignment error matrix and calculates
C!         a random vector AVEC (6-dim), the displacements in the
C!         alignment quanties.
C!
C - Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C
C  Input :  IROW  - the VALC row number to be used.
C
C  Output : AVEC  - six dimensional vector of displacements in
C                   the three alignment translations and three
C                   rotations.
C
C -----------------------------------------------------------------------
#ifndef DOC
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bcs.h"
      INTEGER NAMIND
      REAL AVEC(6)
      INTEGER IROW
C-- local declarations
      INTEGER NAVALC, KVALC, IND
      INTEGER MXALN, MXIND
      PARAMETER (MXALN = 6, MXIND=MXALN*(MXALN+1)/2)
      REAL AMAT(MXIND), EMAT(MXALN,MXALN)
      SAVE NAVALC
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVALC = NAMIND('VALC')
      ENDIF
      KVALC = IW(NAVALC)
      IF (KVALC.LE.0) THEN
        CALL RERROR('VDGTMV',1,'No alignment matrix!')
        RETURN
      ENDIF
C-- load the alignment error matrix
      IND = KROW(KVALC,IROW)+8
C-- copy the triangular mat into a local array
      CALL UCOPY(RW(IND),AMAT,21)
C-- unpack the triangular matrix.  Note that the UCOPY is not really
C-- necessary and RW(IND) can be passed directly to TRUPCK
      CALL TRUPCK(AMAT,EMAT,MXALN)
C-- generate an array of MXALN normally distributed variables,
C-- given the full covariance matrix
      CALL VDRNDM(MXALN,EMAT,AVEC)
      RETURN
      END
#endif
