      SUBROUTINE VDRNDM(MXALN,EMAT,RVEC)
C ----------------------------------------------------------------------
C!  VDRNDM calculates a random vector RVEC of MXALN dimensions
C!         depending on a MXALN by MXALN error matrix EMAT
C!
C - Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C
C  Input :  MXALN  - dimension
C           EMAT   - error matrix
C
C  Output : RVEC   - random vector
C
C -----------------------------------------------------------------------
#ifndef DOC
      INTEGER MXALN
      REAL EMAT(MXALN,MXALN), RVEC(MXALN)

      INTEGER LMAX
      PARAMETER ( LMAX = 6 )
      REAL EVAL(LMAX),EVEC(LMAX,LMAX),WORK(LMAX), VTMP(LMAX)
      INTEGER IERR, MXBY2, I
      IF (MXALN.GT.LMAX) THEN
        CALL RERROR ('VDRNDM',1,'Dimension overflow')
        RETURN
      ENDIF
C-- diagonalize the error matrix
C-- MXALN = 6, the number of alignment parameters
C-- EMAT is the input error matrix
C-- EVAL contains the eigen values (=diagonal elements)
C-- EVEC contains the eigen vectors as columns
C-- IERR.NE.0 => goofed
C-- WORK is a work area
      CALL EISRS1(MXALN,MXALN,EMAT,EVAL,EVEC,IERR,WORK)
CCC CHECK THE RETURN CODE HERE SOMETIME
C-- WORK will be filled with MXALN normally dist ran vars
      MXBY2 = MXALN/2
      DO I = 1, MXBY2
        CALL RANNOR(VTMP(I),VTMP(MXBY2+I))
      ENDDO
C-- scale work by sqrt of eigen values.  gets the random variables in
C-- the eigen basis
      DO I = 1, MXALN
        WORK(I) = SQRT(EVAL(I))*VTMP(I)
      ENDDO
C-- go to the original basis.
C-- [origanal] = [eigen basis][eigen vecs in rows]
      CALL VMATR(WORK,EVEC,RVEC,MXALN,MXALN)
      RETURN
      END
#endif
