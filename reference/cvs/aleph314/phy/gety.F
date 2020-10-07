        SUBROUTINE GETY(RSQ,A,X1,Y1)
CKEY PHYSICS MCARLO /INTERNAL
C.******************************************************************
C. Author : Jane Ratcliffe   6/6/88                                *
C. Called by SUBROUTINE PIDECY                                     *
C! Given a value of X (X1) Returns a Y value (Y1) which defines the Dalitz 
C. Decay in C. of M.                                               *
C.******************************************************************
#ifndef DOC
      SAVE
C
        AY=((1.+A*X1)**2)*((1.-X1)**3/(4.*X1))*(1.+RSQ/X1)
        BY=((1.+A*X1)**2)*((1.-X1)**3/(4.*X1))
        C=SQRT(1.-RSQ/X1)
          R1=RNDM(0)
          YLIM=(2.*AY*C)/(2.*AY*C+(2./3.)*BY*C**3)
          R2=RNDM(0)
          IF(R1.LT.YLIM)THEN
            Y1=2.*C*R2-C
          ELSE
            Y1A=2.*R2-1.
            YSIGN=SIGN(1.0,Y1A)
            Y1B=ABS(Y1A)
            Y1=(C*Y1B**(1./3.))*YSIGN
          ENDIF
C
        RETURN
        END
#endif
