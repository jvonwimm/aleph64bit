      SUBROUTINE TK2IVM(HP1,ER1,HP2,ER2,M,MASS2,EM2)
C
C--------------------------------------------------------------------------
C! Invariant mass squared of two tracks, with errors
CKEY COMPUTE MASS HELIX /USER
C!
C!  R. Johnson  26-12-90
C!
C!  Input:   HP1,HP2        /R      Helix parameters of 2 tracks as in FRFT
C!           ER1,ER2        /R      Error matrices for HP1,2 in symmetric
C!                                  storage (as in FRFT)
C!                                 *Note that only the 1st 3 helix
C!                                  parameters and the first 6 elements of
C!                                  the error matrices are actually used.
C!           M(2)           /R      Masses of the two particles
C!  Output:  MASS2,EM2      /R      Invariant mass squared and error
C!
C--------------------------------------------------------------------------
C
#ifndef DOC
      SAVE
      EXTERNAL ALFIEL
C
#include "alcons.h"
C
      DIMENSION HP1(*),ER1(*),M(*),HP2(*),ER2(*)
      REAL HP1,ER1,HP2,ER2,MASS2,EM2,M
C
      INTEGER I,J,L
      REAL K(2),TL(2),PHI(2)          !1st 3 helix parameters
      REAL B,ALFIEL                   !Magnetic field strength
      REAL PT(2),P(2)                 !Track transverse momentum & momentum
      REAL CSEC(2)                    !Cosecant lambda for each track
      REAL CS(2),SN(2)                !Cosine lambda and sine lambda
      REAL CS12                       !Cosine of angle between 2 tracks
      REAL E(2)                       !Energies of the two particles
      REAL DCSP(2)                    !Derivative of CS12 w.r.t phi1,2
      REAL DCST(2)                    !Derivative of CS12 w.r.t tl1,2
      REAL DM2DP(2)                   !Derivative of m**2 w.r.t. P1,2
      REAL DMC12                      !Derivative of m**2 w.r.t. CS12
      REAL DPDK(2),DPDTL(2)           !Derivatives of P1,2
      REAL DM2(3,2)                   !Derivatives of M**2 w.r.t the 3
                                      !first helix parameters
      REAL A(3,3,2)                   !Error matices for 1st 3 helix parms
      INTEGER IDX(3,3)                !Translate from symmetrix matrix form
      DATA IDX/1,2,4,2,3,5,4,5,6/
C
C++   Get the momenta of the two tracks
C
      B=ALFIEL(HP1)
      K(1)=HP1(1)
      K(2)=HP2(1)
      TL(1)=HP1(2)
      TL(2)=HP2(2)
      PHI(1)=HP1(3)
      PHI(2)=HP2(3)
      DO 2 I=1,2
        IF (K(I).EQ.0.) THEN
          K(I)=0.0000001
        ENDIF
        PT(I)= ABS(B*CLGHT/(K(I)*100000.))
        CSEC(I)=SQRT(1.+TL(I)**2)
        P(I)=PT(I)*CSEC(I)
        E(I)=SQRT(P(I)**2 + M(I)**2)
        CS(I)=1./CSEC(I)
        SN(I)=CS(I)*TL(I)
    2 CONTINUE
C
C++   Angle between the two tracks
C
      CS12= (COS(PHI(1)-PHI(2))+TL(1)*TL(2))*CS(1)*CS(2)
C
C++   The invariant mass
C
      MASS2= M(1)**2 + M(2)**2 + 2.*(E(1)*E(2) - P(1)*P(2)*CS12)
C
C++   Derivatives of CS12 w.r.t. phi and tl
C
      DCSP(1)= SIN(PHI(1)-PHI(2))*CS(1)*CS(2)
      DCSP(2)= -DCSP(1)
      DCST(1)= CS(1)*(SN(2)-CS12*SN(1))
      DCST(2)= CS(2)*(SN(1)-CS12*SN(2))
C
C++   Derivatives of M**2 w.r.t. p and cos12
C
      DM2DP(1)= 2.*((E(2)/E(1))*P(1) - P(2)*CS12)
      DM2DP(2)= 2.*((E(1)/E(2))*P(2) - P(1)*CS12)
      DMC12= -2.*P(1)*P(2)
C
C++   Derivatives of p w.r.t TL and K
C
      DO 4 I=1,2
        DPDK(I)= -P(I)/K(I)
        DPDTL(I)= PT(I)*SN(I)
    4 CONTINUE
C
C++   Derivatives of m**2
C
      DO 5 I=1,2
        DM2(2,I)= DM2DP(I)*DPDTL(I) + DMC12*DCST(I)
        DM2(1,I)= DM2DP(I)*DPDK(I)
        DM2(3,I)= DMC12*DCSP(I)
    5 CONTINUE
C
C++   Copy the error matrix into more convenient form
C
      DO 8 I=1,3
        DO 9 J=1,3
          A(I,J,1)=ER1(IDX(I,J))
          A(I,J,2)=ER2(IDX(I,J))
    9   CONTINUE
    8 CONTINUE
C
C++   Calculate the error on M**2
C
      EM2=0.
      DO 20 L=1,2
        DO 12 I=1,3
          DO 13 J=1,3
            EM2= EM2 + DM2(I,L)*A(I,J,L)*DM2(J,L)
   13     CONTINUE
   12   CONTINUE
   20 CONTINUE
      EM2=SQRT(MAX(0.,EM2))
C
      END
#endif
