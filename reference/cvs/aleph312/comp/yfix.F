      SUBROUTINE YFIX(C,RIN,IERR)
C----------------------------------------------------------------------
C!   Fix matrix convention in tgft
C!   Created by Juergen Knobloch       1-JUL-1988
C!
C!   Purpose   :
C!   Inputs    : C(5,5) double precision cov matrix old convention
C!               RIN    radius new sign convention
C!   Outputs   : C(5,5) error matrix new convention
C!             : IERR = 1 if matrix singular
C!   Calls     : DINV / RINV (IF BIT64)
C!
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
       DOUBLE PRECISION C(5,5),S,RIN
       REAL WS(5)
       IERR=0
       S = SIGN(1.D0,RIN)
       DO 1 I=1,5
       C(1,I) = -C(1,I)
       C(I,1) = -C(I,1)
       C(4,I) = C(4,I)*S
    1  C(I,4) = C(I,4)*S
#ifndef BIT64
       CALL DINV(5,C,5,WS,IFAIL)
#else
       CALL RINV(5,C,5,WS,IFAIL)
#endif
       IERR=IFAIL
C----------------------------------------------------------------------
  999 RETURN
      END
#endif
