      SUBROUTINE YFIXIS(C,RIN,IFAIL)
C----------------------------------------------------------------------
C!   Fix matrix convention in FRFT
CKEY YV0 FRFT / USER
C    Created by Juergen Knobloch       1-JUL-1988
C   M.A. CIOCCI CHANGE THE NAME (IT WAS VFIX...)
C               AND THE NUMBER OF ARGUMENTS
C   Inputs    : C(5,5) double precision cov matrix old convention
C               RIN    radius new sign convention
C   Outputs   : C(5,5) error matrix new convention
C
C   BANKS: NONE
C   Calls     : DINV
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
       DOUBLE PRECISION C(5,5),S,RIN
       REAL WS(5)
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
C----------------------------------------------------------------------
  999 RETURN
      END
#endif
