C-----------------------------------------------------------------------
      SUBROUTINE TPRNDM(NDIM,EVEC,EVAL,RVEC)
C ----------------------------------------------------------------------
C!  TPRNDM calculates a random vector RVEC of NDIM dimensions
C!         depending on a NDIM by NDIM error matrix whose eigen
C!         vectors (EVEC) and eigen values (EVAL) are provided
C!
C - Author         Manoj Thulasidas  5-Nov-1994
C
C  Input :  NDIM  - dimension
C           EVEC   - eigen vectors of EMAT in columns
C           EVAL   - eigen values
C
C  Output : RVEC   - random vector
C
C -----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      INTEGER NDIM
      REAL EVAL(NDIM),EVEC(NDIM,NDIM), RVEC(NDIM)

      INTEGER LMAX, LMAXE
      PARAMETER ( LMAX = 21, LMAXE=LMAX+1 )
      REAL WORK(LMAX), VTMP(LMAXE)
      INTEGER IERR, MXBY2, I
C -----------------------------------------------------------------------
C-- WORK will be filled with NDIM normally dist ran vars
      MXBY2 = LMAXE/2
      DO I = 1, MXBY2
        CALL RANNOR(VTMP(I),VTMP(MXBY2+I))
      ENDDO
C-- scale work by sqrt of eigen values.  gets the random variables in
C-- the eigen basis
      DO I = 1, NDIM
C-- temporary fix
        IF (EVAL(I).LT.0.0) EVAL(I) = 0.0
        WORK(I) = SQRT(EVAL(I))*VTMP(I)
      ENDDO
C-- go to the original basis.
C-- [origanal] = [eigen basis][eigen vecs in rows]
      CALL VMATR(WORK,EVEC,RVEC,NDIM,NDIM)
      RETURN
      END
#endif
