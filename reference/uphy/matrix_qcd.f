C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE ELEMAT1(MQQ2)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Manel Martinez         27-MAR-1996
C!
C!   Inputs: 4-momenta corresponding to each of the 6 fermions.
C!          ( Convention follows EXCALIBUR.
C!            If needed, apply relabeling in the data statement)
C!
C!        e+(1) + e-(2) --> g(3) g(4) q(5) qbar(6)
C!        e+(1) + e-(2) --> q'(3) qbar'(4) q(5) qbar(6)
C!
C!
C!   Momentum components: 1-px 2-py 3-pz 4-E
C!
C!   Outputs: squared amplitude
C!        -
C!
C!   Libraries required: NONE
C!
C!   Description: Evaluation of the squared amplitude of the processes
C!   ===========  e+ e- --> g g q qbar and e+ e- --> q' qbar' q qbar
C!                through s-channel photon exchange.
C!
C!                From:
C!        "Four-jet production in e^+ e^- annihilation",
C!      D.Danckaert, P.DeCausmaecker, R.Gastmans, W.Troost and T.T.Wu,
C!                Phys. Lett. 114B 203 (1982).
C!
C!======================================================================
C
      IMPLICIT REAL*8(A-H,L,M,O-Z)
      COMPLEX*16 F1,CMQQQQ2
      COMMON/MOMENTA/PK(6,4)
      COMMON/XCONST/EB,EB2,XN
C
      PARAMETER(ALPHA=1.D0/128.896D0, ALPHAS= 0.123D0, 
     &  PI=3.1415926535897932D0, XNF = 5.D0)
C
C Evaluate factors
C   ---  Some constants  ---
      QED = DSQRT(ALPHA*4.D0*PI)
      QCD = DSQRT(ALPHAS*4.D0*PI)
      QF2T = 3*(1.D0/3.D0)**2 + 2*(2.D0/3.D0)**2  
      ECM2 = 2.D0*SC(1,2)
      EB = SQRT(ECM2)/2.D0
      EB2 = EB*EB
C   ---  Some intermediate expressions ---
      A = SC(3,4)*SC(3,5)*SC(3,6)*SC(4,5)*SC(4,6)*SC(5,6)
      B = QED**2 * QCD**2/
     & (4.D0 * EB2 * SQRT( (PK(3,4)+PK(3,3))*(PK(4,4)+PK(4,3))*
     &  (PK(5,4)+PK(5,3))*(PK(6,4)+PK(6,3)) ) )
      XN = 3.D0
C
C Sum and average over initial spin configurations
      MGGQQ2 = (XN**2-1.D0)/(4.D0*XN) * QED**4 *QCD**4 /A * QF2T *
     &  (G1(3,4,5,6) + G2(3,4,5,6))
      CMQQQQ2 = (XN**2-1.D0)/(4.D0*XN) * B**2 * QF2T *
     &  ( 2.D0 * XN * XNF * F1(3,4,5,6) + F1(3,5,4,6)
     &   + F1(6,4,5,3)) * DCONJG(F1(3,4,5,6))
      MQQQQ2 = DREAL(CMQQQQ2)
      MQQ2 = MGGQQ2 + MQQQQ2
C
      RETURN
      END
C
C---------------------------------------------------------------------
      FUNCTION F1(I3,I4,I5,I6)
C----------------------------------------------------------------------
C
      IMPLICIT REAL*8(A-H,L,M,O-Z)
      COMPLEX*16 Z,CI,F1
      COMMON/XCONST/EB,EB2,XN
      COMMON/MOMENTA/PK(6,4)
      DATA CI/(0.D0,1.D0)/
C
      F1 = (PK(I6,1) + CI*PK(I6,2))/( SC(I5,I6)*(PK(I6,4) - PK(I6,3)))
     & * ( (PK(I4,4) + PK(I4,3)) /(EB-PK(I4,4))*
     &   DCONJG(Z(I3,I6))*( Z(I5,I3) + Z(I5,I6) ) 
     & +   (PK(I3,1) + CI*PK(I3,2)) * (PK(I5,1) - CI*PK(I5,2))/
     &     ( (EB-PK(I3,4)) * (PK(I5,4) - PK(I5,3)) )*
     &     Z(I4,I5)* DCONJG(Z(I4,I6) + Z(I5,I6)) )
      RETURN
      END
C
C
C---------------------------------------------------------------------
      FUNCTION G1(I3,I4,I5,I6)
C----------------------------------------------------------------------
C
      IMPLICIT REAL*8(A-H,L,M,O-Z)
      COMMON/MOMENTA/PK(6,4)
      COMPLEX*16 C1,C2
      COMMON/XCONST/EB,EB2,XN
C
      G1 = ( (PK(I6,4) + PK(I6,3))**2 + (PK(I6,4) - PK(I6,3))**2 )*
     &      SC(I5,I6)*( 2.D0*XN**2 * SC(I3,I5)*SC(I4,I6) -
     &                               SC(I3,I4)*SC(I5,I6) )
      RETURN
      END
C
C---------------------------------------------------------------------
      FUNCTION G2(I3,I4,I5,I6)
C----------------------------------------------------------------------
C
      IMPLICIT REAL*8(A-H,L,M,O-Z)
      COMPLEX*16 Z,ALFA,BETA,GAMMA,C1,C2,F1
      COMMON/MOMENTA/PK(6,4)
      COMMON/XCONST/EB,EB2,XN
C
      ALFA = Z(I3,I4)*Z(I5,I6) - Z(I3,I6)*Z(I5,I4)
      BETA = DCONJG(Z(I5,I4)*( Z(I3,I4) + Z(I3,I6) ))
      GAMMA = Z(I3,I6)*( Z(I3,I4) + Z(I5,I4) )
      C1 = ALFA * DCONJG(ALFA)
     &   + ( SC(I4,I6)/(2.D0*EB*(EB-PK(I5,4)) ) )* ALFA * BETA
     &   + ( SC(I3,I5)/(2.D0*EB*(EB-PK(I6,4)) ) )* DCONJG(ALFA) * GAMMA
      C2 = BETA * GAMMA
     &   + ( SC(I3,6)/(2.D0*EB*(EB-PK(I5,4)) ) )* ALFA * BETA
     &   + ( SC(I4,5)/(2.D0*EB*(EB-PK(I6,4)) ) )* DCONJG(ALFA) * GAMMA
C
      G2 = SC(I5,I6)/( 32.D0*EB2 * SC(I3,I4)
     & * (PK(I3,4) + PK(I3,3))**2 * (PK(I4,4) - PK(I4,3))**2
     & * (PK(I5,4) + PK(I5,3)) * (PK(I6,4) - PK(I6,3)) )
     & * ( (XN**2 - 2.D0) * (C1+C2)*DCONJG(C1+C2)
     &    + XN**2 * (C1-C2)*DCONJG(C1-C2) )
      RETURN
      END
C
C----------------------------------------------------------------------
      FUNCTION SC(I,J)
C----------------------------------------------------------------------
C
      IMPLICIT REAL*8(A-H,L,M,O-Z)
      COMMON/MOMENTA/PK(6,4)
C
      SC = PK(I,4)*PK(J,4)
     &    -PK(I,1)*PK(J,1)-PK(I,2)*PK(J,2)-PK(I,3)*PK(J,3)
      RETURN
      END
C
C----------------------------------------------------------------------
      FUNCTION Z(I,J)
C----------------------------------------------------------------------
C
      IMPLICIT REAL*8(A-H,L,M,O-Z)
      COMPLEX*16 Z,CI
      COMMON/MOMENTA/PK(6,4)
      DATA CI/(0.D0,1.D0)/
C
      Z = ( PK(I,4) + PK(I,3) )*( PK(J,4) - PK(J,3) )
     &  - ( PK(I,1) - CI*PK(I,2) )*( PK(J,1) + CI*PK(J,2) )
      RETURN
      END
C---------------------------------------------------------------------
