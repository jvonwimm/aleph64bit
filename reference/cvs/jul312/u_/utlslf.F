      SUBROUTINE UTLSLF(X,Y,W,N,A,B,CHISQ,VAR)
C----------------------------------------------------------------*
C! Do weighted least squares fit
C! to line  Y = A*X + B
C!
C!   Author:     M.Mermikides
C!   Modified:   M.Mermikides    08-04-87
C!
C!  INPUT:
C!
C!        N  = Number of data points
C!      X(N) = x-values
C!      Y(N) = y-values
C!      W(N) = weights (1./sigma(y)**2)
C!
C!  OUTPUT:
C!
C!         A  = Slope
C!         B  = Intercept
C!      CHISQ = Chisquared of fit
C!      VAR(3)= Covariance matrix {1=V(AA), 2=V(AB), 3=V(BB)}
C!
C----------------------------------------------------------------
#ifndef DOC
C
      DOUBLE PRECISION  SUM,SX,SY,SXX,SXY,DET
C
      DIMENSION X(*),Y(*),W(*),VAR(3)
C
      CHI=999.
C
      SUM=0.D0
      SX =0.D0
      SY =0.D0
      SXX=0.D0
      SXY=0.D0
      DO 10 I=1,N
         SUM=SUM + W(I)
         SX =SX  + W(I)*X(I)
         SY =SY  + W(I)*Y(I)
         SXX=SXX + W(I)*X(I)*X(I)
         SXY=SXY + W(I)*X(I)*Y(I)
   10 CONTINUE
C
      DET= SUM*SXX -SX*SX
      IF(ABS(DET).LT.1.D-20) GO TO 90
      A = (SUM*SXY -  SX*SY)/DET
      B = (SY*SXX  - SXY*SX)/DET
      CHI=0.
      DO 20 I=1,N
         CHI=CHI+ W(I)*(Y(I)-A*X(I)-B)**2
   20 CONTINUE
      VAR(1) = SUM/DET
      VAR(2) = -SX/DET
      VAR(3) = SXX/DET
   90 CHISQ=CHI
      RETURN
      END
#endif
