      SUBROUTINE BBFWRS(LUN,IACM,NRECW,BUF,NDIM)

*     FORTRAN write

      INTEGER    BUF(NDIM)
      IF(IACM.EQ.0) THEN
*        sequential - FORTRAN version
         WRITE(LUN) BUF
      ELSE IF(IACM.EQ.1) THEN
*        direct access - FORTRAN version
         WRITE(LUN,REC=NRECW) BUF
      ELSE IF(IACM.EQ.2) THEN
*        direct access - FORTRAN version
         WRITE(LUN,REC=NRECW) BUF
      END IF
  100 RETURN
      END
