C************************************************************
       SUBROUTINE PLEGEN(N,X,REP)
C-----------------------------------------
C    POUR CALCULER LES POLYNOMES DE LEGENDRE
C    N   DEGRE DU POLYNOME (<=5)
C    X   VARIABLE
C    REP RESULTAT
C----------------------------------------
       GOTO(1,2,3,4,5)N
    1  REP=X
       GOTO 1000
    2   REP=(-1.+3.*X*X)/2.
       GOTO 1000
    3  REP=(-3.*X+5.*X*X*X)/2.
       GOTO 1000
    4  REP=(3.-30.*X*X+35.*X**4)/8.
       GOTO 1000
    5  REP=(15.*X-70.*X**3+63.*X**5)/8.
 1000  CONTINUE
       RETURN
       END
C
C
