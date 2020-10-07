C-----------------------------------------------------------------------
      SUBROUTINE TPDIAG(NDIM,EMAT,EVEC,EVAL)
C ----------------------------------------------------------------------
C!  TPDIAG diagonalizes the 21x21 hit error matrix
C!         returns the eigen vectors (as columns in EVEC)
C!         and the eigen values (as EVAL)
C!
C - Author         Manoj Thulasidas  5-Nov-1994
C
C  Input :  NDIM  - dimension
C           EMAT   - error matrix
C
C  Output : EVEC   - eigen vectors of EMAT in columns
C           EVAL   - eigen values
C
C -----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      INTEGER NDIM
      REAL EMAT(NDIM,NDIM), EVEC(NDIM,NDIM), EVAL(NDIM)
      INTEGER LMAX
      PARAMETER ( LMAX = 21 )
      REAL WORK(LMAX)
      INTEGER IERR
C -----------------------------------------------------------------------
      IF (NDIM.GT.LMAX) THEN
        CALL RERROR ('TPRNDM',1,'Dimension overflow')
        RETURN
      ENDIF
C-- diagonalize the error matrix
C-- NDIM = 21, the number of tpc padrows
C-- EMAT is the input error matrix
C-- EVAL contains the eigen values (=diagonal elements)
C-- EVEC contains the eigen vectors as columns
C-- IERR.NE.0 => goofed
C-- WORK is a work area
      CALL EISRS1(NDIM,NDIM,EMAT,EVAL,EVEC,IERR,WORK)
C-- check the return code here
      IF (IERR.NE.0) THEN
        CALL RERROR('TPDIAG',1,'Diagonalization fails')
      ENDIF
      RETURN
      END
#endif
