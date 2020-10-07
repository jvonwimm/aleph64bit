C-----------------------------------------------------------------------
C           A L E P H   I N S T A L L A T I O N    N O T E S           |
C                                                                      |
C    Aleph    name  : MGAM01.                                          |
C    original code  : MGAM from E. Milotti - Trieste.                  |
C    transmitted by : E. Milotti April 18, 1988.                       |
C                                                                      |
C       (see KINLIB DOC for a description of this version)             |
C                                                                      |
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
      double precision FUNCTION RND1DD(DISTR,A,B,EPS,IERR)
C
C  for a given distribution function (i.e. undefined integral of a
C  density function) DISTR (to be declared external in the
C  calling program), defined between A and B, this routine returns
C  a number RND1DD, randomly distributed between A and B according
C  to DISTR. (NOTE: DISTR(A)=0 is not needed; the undefined integral i
C  given as input and the 0-base is computed inside the routine.
C  This clearly does not disturb defined integrals)
C  Given the analytical shape of the integral distribution, the
C  routine numerically solves the eq. rndm=int(a,x) with the method of
C  regula falsi.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 RNDM,DUMMY
      EXTERNAL RNDM,DISTR
      PARAMETER (MAXIT=1000)
C
      IERR=0
      IF(A.GE.B) THEN
        IERR=1
        RETURN
      END IF
      IF(EPS.LE.0) THEN
        IERR=2
        RETURN
      END IF
C
      BASE=DISTR(A)
      RN=RNDM(DUMMY)
      XL=A
      XH=B
      YL=-RN
      YH=1.-RN
      DMAX=DISTR(B)-BASE
      DO 1 IT=1,MAXIT
        RND1DD=(YH*XL-YL*XH)/(YH-YL)
        Y=(DISTR(RND1DD)-BASE)/DMAX-RN
        IF(Y.LT.0) THEN
          DEL=RND1DD-XL
          XL=RND1DD
          YL=Y
        ELSE
          DEL=RND1DD-XH
          XH=RND1DD
          YH=Y
        END IF
        IF(ABS(DEL).LT.EPS.OR.Y.EQ.0) RETURN
    1 CONTINUE
      IERR=3
      END
      DOUBLE PRECISION FUNCTION XSEC1(C)
C
C  This is the integral of the first order 2-gamma angular cross secti
C  C=cos(theta); XSEC1(-1)=0, XSEC1(1)=total cross-section
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /CONS3G/ PI,ALFA,EMASS
      REAL*8 M,K,KP
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT
C
      XSEC1=((1.+E**2)/2./E)*LOG((E+1.)*(E+C)/((E-1.)*(E-C)))-(C+1.)
      XSEC1=PI*ALFA**2*XSEC1/S
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION WEI2(C)
C
C  This is the weight for an uncorrected two-photon event
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /CONS3G/ PI,ALFA,EMASS
      REAL*8 M,K,KP
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT
C
      U=0.5*LOG(2*(E+C)/M/M)
      W=0.5*LOG(2*(E-C)/M/M)
C
      WEI2= 2*(1-2*V)*(LOG(K)+V) + 3/2 - PI**2/3 +
     +      (-4*V*V*(3-C*C)-8*V*C*C+4*U*V*(5+2*C+C*C)+
     +       4*W*V*(5-2*C+C*C)-U*(5-6*C+C*C)-W*(5+6*C+C*C)-
     -       2*U*U*(5+2*C+C*C)-2*W*W*(5-2*C+C*C))/2/(1+C*C)
      WEI2= 1-ALFA*WEI2/PI
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION XSEC2(X)
C
C  Integrated distribution: formula 4.10 from BK paper
C  X is X3
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /CONS3G/ PI,ALFA,EMASS
      REAL*8 M,K,KP
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT
C
      XP=E-X
      IF(XP.NE.0) THEN
        XSEC2=4*V*LOG(X/K)-(2*V-1)*(X-K)-KP*LOG(KP)+XP*LOG(XP)
      ELSE
        XSEC2=-4*V*LOG(K)-(2*V-1)*KP-KP*LOG(KP)
      END IF
      XSEC2=4*ALFA**3*V*XSEC2/S
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION XSEC3(C)
C
C  Integrated distribution: formula 4.6 from BK paper
C  C is C1
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /CONS3G/ PI,ALFA,EMASS
      REAL*8 M,K,KP
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT
C
      X3P=E-X3
      RHO=2*E+X3*(E-1)
      XSEC3=(X3P/X3/E+1/RHO)*LOG((E+C)/(E-C))+LOG(Y(C)/Y(-C))/RHO
      XSEC3=4*ALFA**3*V*XSEC3/S
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION XSEC4(C)
C
C  Integrated distribution: formula 4.4 from BK paper
C  C is C3
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /CONS3G/ PI,ALFA,EMASS
      REAL*8 M,K,KP
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT
C
      X3P=E-X3
      AM=(2*X3P/X3)/(E**2-C1**2)+2/Y(C1)/(E-C1)
      AP=(2*X3P/X3)/(E**2-C1**2)+2/Y(-C1)/(E+C1)
      XSEC4=AP*LOG(E+C)-AM*LOG(E-C)
      XSEC4=ALFA**3*XSEC4/S
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION Y(Z)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 M,K,KP
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT
      Y=2*E-X3+X3*Z
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION WEI3(Q)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /CONS3G/ PI,ALFA,EMASS
      REAL*8 M,K,KP
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT
      REAL*4 Q(4,3)
C
      FT=0.
      FA=0.
C
C  123
      Z=CA(Q,1,3)
      FT=FT+TN(X1,X2,X3,C1,C2,C3)
      FA=FA+AN(X1,X2,X3,C1,C2,C3,Z)
C
C  132
      Z=CA(Q,1,2)
      FT=FT+TN(X1,X3,X2,C1,C3,C3)
      FA=FA+AN(X1,X3,X3,C1,C3,C2,Z)
C
C  213
      Z=CA(Q,2,3)
      FT=FT+TN(X2,X1,X3,C2,C1,C3)
      FA=FA+AN(X2,X1,X3,C2,C1,C3,Z)
C
C  231
      Z=CA(Q,2,1)
      FT=FT+TN(X2,X3,X1,C2,C3,C1)
      FA=FA+AN(X2,X3,X1,C2,C3,C1,Z)
C
C  312
      Z=CA(Q,3,2)
      FT=FT+TN(X3,X1,X2,C3,C1,C2)
      FA=FA+AN(X3,X1,X2,C3,C1,C2,Z)
C
C  321
      Z=CA(Q,3,1)
      FT=FT+TN(X3,X2,X1,C3,C2,C1)
      FA=FA+AN(X3,X2,X1,C3,C2,C1,Z)
C
      WEI3=FT/FA
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION TN(XI,XJ,XK,CI,CJ,CK)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /CONS3G/ PI,ALFA,EMASS
      REAL*8 M,K,KP
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT
C
      TN=16*E*E*(E-XK)**2/XI**2/XK**2/(E*E-CI**2)/(E*E-CK**2)-
     -   8/XK**2/(E*E-CK**2) + 4/XI/XK/(E-CI)/(E-CK)+
     +   4/XI/XK/(E+CI)/(E+CK)
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION AN(XI,XJ,XK,CI,CJ,CK,Z2)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /CONS3G/ PI,ALFA,EMASS
      REAL*8 M,K,KP
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT
C
      AN=8*(E-XK)/E/XK/(E*E-CI*CI)/(E*E-CK*CK)+
     +   4/Y(CI)/(E-CI)/(E-CK) + 4/Y(-CI)/(E+CI)/(E+CK)
      OMEGA=XI*XK/Y(Z2)
      AN=AN/OMEGA
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION CA(Q,I,K)
      REAL*4 Q(4,3),VDOT
      CA=VDOT(Q(1,I),Q(1,K),3)/Q(4,I)/Q(4,K)
      RETURN
      END
