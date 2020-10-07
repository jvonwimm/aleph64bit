      SUBROUTINE V0KINE(M2,SM2,P,T1,RADIUS,TERR,
     &    DELTA,BVEC,OMAT,KERR2)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Compute some kinematic information for a pair of V0 candidate tracks
C  Author                                                D. Brown 6-9-93
C  Called from CHIV0
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Inputs; V0 mass, particle masses (squared),Track momentum magnitude a
C  direction, curvature radius, track error matrix upper corner
C
      REAL M2,SM2(2),P(2),T1(3,2),RADIUS(2),TERR(3,3,2)
C
C  Outputs
C
      REAL DELTA,BVEC(3),OMAT(3,3),KERR2
C
C  Local variables
C
      INTEGER ICOR,JCOR,LCOR,ITRK,JTRK
      INTEGER IERR,JERR
      DOUBLE PRECISION DOT,OMEGA(3),KERRD2
      DOUBLE PRECISION ENE(2),PT(2),PZ(2)
      DOUBLE PRECISION COSL(2),DK(3,2)
      DOUBLE PRECISION COSNU,SINNU,CVEC,CMAT
      DOUBLE PRECISION MINDEL/0.002D0/
C  Energies
C
      DO ITRK=1,2
        ENE(ITRK) = SQRT(SM2(ITRK)+P(ITRK)**2)
      END DO
C
C  Transverse and longitudinal momentum
C
      DO ITRK=1,2
        COSL(ITRK) = SQRT(T1(1,ITRK)**2+T1(2,ITRK)**2)
        PT(ITRK) = P(ITRK)*COSL(ITRK)
        PZ(ITRK) = P(ITRK)*T1(3,ITRK)
      END DO
C
C  momentum dependence vector
C
      OMEGA(3) = 0.0
      DO ICOR=1,2
        OMEGA(ICOR) = T1(ICOR,1)/(COSL(1)*RADIUS(1))-
     &        T1(ICOR,2)/(COSL(2)*RADIUS(2))
      END DO
C
C  Momentum dot
C
      DOT = 0.0
      DO ICOR=1,3
        DOT = DOT + T1(ICOR,1)*T1(ICOR,2)
      END DO
C
C  Energy gap
C
      DELTA = ENE(1)*ENE(2) - P(1)*P(2)*DOT - .5D0*(M2-SM2(1)-SM2(2))
C
C  Phi angle difference
C
      COSNU = (T1(1,1)*T1(1,2)+T1(2,1)*T1(2,2))/(COSL(1)*COSL(2))
      SINNU = (T1(2,2)*T1(1,1)-T1(2,1)*T1(1,2))/(COSL(1)*COSL(2))
C
C  Constant terms for vector and matrix; make sure the matrix
C  has a positive coefficient to avoid problems inverting the
C  matrix.  Physically, this just says that the photon mass isn't
C  well defined in a conversion.
C
      CVEC = SINNU*PT(1)*PT(2)*DELTA
      CMAT = PT(1)*PT(2)*MAX(MINDEL,PT(1)*PT(2)*SINNU**2 + COSNU*DELTA)
C
C  Build up some simple tensors
C
      DO ICOR=1,3
        BVEC(ICOR) = OMEGA(ICOR)*CVEC
        DO JCOR=1,3
          OMAT(JCOR,ICOR) = OMEGA(JCOR)*OMEGA(ICOR)*CMAT
        END DO
      END DO
C
C  Derrivatives of the energy gap
C
      DO ITRK=1,2
        JTRK = MOD(ITRK,2)+1
C
C  Inverse radius term
C
        DK(1,ITRK) = RADIUS(ITRK)*(P(ITRK)*P(JTRK)*DOT -
     &        P(ITRK)**2*ENE(JTRK)/ENE(ITRK))
C
C  Tanl term
C
        DK(2,ITRK) = PT(ITRK)*(PZ(ITRK)*ENE(JTRK)/ENE(ITRK)- PZ(JTRK))
C
C  Phi0 term
C
        DK(3,ITRK) = P(1)*P(2)*(T1(2,ITRK)*T1(1,JTRK)-
     &        T1(1,ITRK)*T1(2,JTRK))
      END DO
C
C  Build the energy gap error
C
      KERRD2 = 0.D0
      DO ITRK=1,2
        DO IERR=1,3
          DO JERR=1,3
            KERRD2 = KERRD2 + DK(IERR,ITRK)*DK(JERR,ITRK)*
     &            TERR(JERR,IERR,ITRK)
          END DO
        END DO
      END DO
      KERR2 = MAX(KERRD2,0.0000001D0)
C
C  Done
C
      RETURN
      END
