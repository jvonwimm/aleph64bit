      FUNCTION YCHI00 (VSEC,SVSEC,VPRIM,SIVPR,IFAIL)
C----------------------------------------------------------------------
C! Calculate the c2 variation constraining a vertex to the primary vertex
C
CKEY YV0 CHI2
C    AUTHOR: M.A.CIOCCI 28/1/93
C
C          DESCRIPTION
C          ===========
C  Computes the chi2 variation constraining a vertex to the primary
C  vertex
C
C    INPUT:
C               VSEC/R    COORDINATES OF A VERTEX
C               SVSEC/R   COVARIANCE OF VSEC VERTEX COORDINATES
C
C               VPRIM/R   COORDINATES OF THE PRIMARY
C                         VERTEX
C               SIVPR/R   COVARIANCE MATRIX OF THE PRIMARY
C
C    OUTPUT:
C               IFAIL/I IF 0 ALL IS OK
C                       IF 1 PROBLEM WITH COV MAT
C-------------------------------------------------
#ifndef DOC
      SAVE
C
       DOUBLE PRECISION VVMAT(3,3)
       REAL VPRIM(3),SIVPR(3,3),VSEC(3),SVSEC(3,3),WR(3)
        DO 123 II=1,3
        DO 124 JJ=1,3
        VVMAT(II,JJ)=DBLE(SVSEC(II,JJ))+DBLE(SIVPR(II,JJ))
 124    CONTINUE
 123    CONTINUE
C
C+ INVERT THE MATRIX VVMAT
C
#ifndef BIT64
        CALL DINV(3,VVMAT,3,WR,IFAIL)
#else
        CALL RINV(3,VVMAT,3,WR,IFAIL)
#endif
                    IF (IFAIL.NE.0) THEN
              GOTO 999
                    ENDIF
C
C   TEST THE CHISQUARE INCREASE WHEN CONSTRAINING THE
C   VERTEX TO THE ORIGIN
C
        CHI0=0
        DO 74 I1=1,3
        DO 73 J1=1,3
        CHI0=CHI0+DBLE(VSEC(I1)-VPRIM(I1))
     $     *VVMAT(I1,J1)
     $     *DBLE(VSEC(J1)-VPRIM(J1))
   73   CONTINUE
   74   CONTINUE
C
        YCHI00=CHI0
        RETURN
  999      CONTINUE
           RETURN
        END
#endif
