      DOUBLE PRECISION FUNCTION TFIPOL(ITYP,IDEG,X)
C!----------------------------------------------------------------------
C!      Function called in TFICOR to calculate regression
C!      Adapted from HBOOK function HSTELF
C!
C!
C!      GIVES THE VALUE COMPUTED AT X OF A STANDARD
C!      ELEMENTARY FUNCTION (POLYNOMIAL) OF TYPE ITYP
C!      AND DEGREE IDEG
C!      ITYP = 0   MONOMIAL
C!             1   TCHEBYSHEV POLYNOMIAL
C!             2   LEGENDRE POLYNOMIAL
C!             3   SHIFTED TCHEBYSHEV POLYNOMIAL
C!             4   LAGUERRE POLYNOMIAL
C!             5   HERMITE POLYNOMIAL
C!
CKEY TPC FIELD-CORRECTION
C!
C!----------------------------------------------------------------------
#ifndef DOC
      SAVE
      DOUBLE PRECISION P0,P1,P2
      P0=1
      I1=ITYP+1
      GOTO (10,20,30,40,50,60),I1
*
   10 P1=X
      DO 15 I=2,IDEG
         P2=P1*X
         P1=P2
   15 CONTINUE
      GOTO 70
*
   20 P1=X
      DO 25 I=2,IDEG
         P2=2.*X*P1-P0
         P0=P1
         P1=P2
   25 CONTINUE
      GOTO 70
*
   30 P1=X
      DO 35 I=2,IDEG
         P2=(2.*I-1.)/I*X*P1-(I-1.)/I*P0
         P0=P1
         P1=P2
   35 CONTINUE
      GOTO 70
*
   40 P1=2.*X-1.
      DO 45 I=2,IDEG
         P2=2.*(2.*X-1.)*P1-P0
         P0=P1
         P1=P2
   45 CONTINUE
      GOTO 70
*
   50 P1=1.-X
      DO 55 I=2,IDEG
         P2=(2.*I-1.-X)*P1-(I-1.)**2*P0
         P0=P1
         P1=P2
   55 CONTINUE
      GOTO 70
*
   60 P1=2.*X
      DO 65 I=2,IDEG
         P2=2.*X*P1-2.*(I-1.)*P0
         P0=P1
         P1=P2
   65 CONTINUE
*
   70 TFIPOL=P1
      END
#endif
