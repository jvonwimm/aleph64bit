      SUBROUTINE XXP0F1(IRET, PGAM1,PGAM2,P0FIT,XMFIT,CHI2)
CKEY  QPI0DO / INTERNAL
C-----------------------------------------------------------------------
C! Subroutine to constraint the p0 mass.
C  Called from QPI0DO
C
C      Uses a Lagrange multipliers technique
C  Author    G.Batignani 10 / Apr /1992
C                        10 / Apr /1992      Modified error
C  Modified  J.P.Lees    20 / May /1992      Modified arguments
C
C  Input  arguments :
C            PGAM1         first photon 4 momentum
C            PGAM2        second photon 4 momentum
C      Remarks:   1.   the 4-momentum of IPI0 must be the sum of
C                      the two 4-momenta of IG1 and IG2, in order
C                      to have a significant fit.
C                 2.   the invariant mass of IPI0 should lie in the
C                      range 50 - 250 Mev.   If not, the energy of
C                      the pi0  is redefined badly.
C
C  Output arguments :
C
C             IRET     return code :  -1 and -2 : the fit is failed and
C                                     P0FIT and XMFIT are set to 0,
C                                    from 1 to 5 : the fit has converged
C                                    and the Pi0 new mass is within 0.2
C                                    MeV around the nominal Pi0 mass and
C                                   the fit has converged in IRET iter.,
C                                    6  : the fit has not converged in
C                                    less than 5 iteration and the
C                                    fitted values are the ones calculat
C                                    at the 5-th iteration.
C             P0FIT(I)  fitted 4-momentum of Pi0 ( p,E) : it MUST be
C                       dimensioned 4 in the calling routine.
C             XMFIT     inveriant mass after the fit
C
C      Remarks : - so far the fit has always converged in 1,2 or 3
C                 iterations, so the invariant mass is the nominal pi0
C                 mass (  within 0.2 MeV ).   However, it is better
C                 to check IRET > 0.
C                - (repeated) : call this routine only if the pi0
C                 invariant mass ( of IPI0 ) is in the range 50-200 MeV.
C                - in first approximation this routine redefines the
C                  pi0 energy (and not the direction) and improves the
C                  energy resolution from (roughly) 26%/sqrt(E) to
C                  6%  (constant with energy).
C
C-----------------------------------------------------------------------
#ifndef DOC
      DIMENSION PGAM1(4),PGAM2(4),PPI0(4)
      DIMENSION P0FIT(4),DEGAM(2),PGFIT(4,2)
C..parameters: ECAL rough energy resolution RISE=0.18
C..                       pi0 mass          XMP0=0.13497
C.. precision on pi0 mass for convergence   XMCUT=0.0002
C.. maximum number of iterations allowed    ITMAX=5
      DATA RISE, XMP0, XMCUT, ITMAX / 0.18, 0.13497, 0.0002, 5 /
C-----------------------------------------------------------------------
C
      ITER = 1
      CALL VADD(PGAM1,PGAM2,PPI0,4)
      XMFIT = PPI0(4)**2-PPI0(1)**2-PPI0(2)**2-PPI0(3)**2
      IF (XMFIT.GT.0.) THEN
        XMFIT=SQRT(XMFIT)
      ELSE
        XMFIT=1.E-6
      ENDIF
      CALL UCOPY(PGAM1,PGFIT(1,1),4) !Energie momentum gam1
      CALL UCOPY(PGAM2,PGFIT(1,2),4) !                 gam2
      S1         = RISE*RISE*PGAM1(4) !Energy resolution gam1
      S2         = RISE*RISE*PGAM2(4) !Energy resolution Gam2
C
C        LOOP
C
   30  CONTINUE
      AH = (XMP0*XMP0)/(XMFIT*XMFIT)  - 1.
      BH = S1*PGFIT(4,2)*PGFIT(4,2) + S2*PGFIT(4,1)*PGFIT(4,1)
      CH = AH * PGFIT(4,1) * PGFIT(4,2) / BH
      DEGAM(1) = CH * PGFIT(4,2) * S1
      DEGAM(2) = CH * PGFIT(4,1) * S2
C
      IRET = 0
      DO 90 JF = 1,2
      FACT =  1. + DEGAM(JF) /PGFIT(4,JF)
      IF (FACT.LE.0.) THEN
         IRET = IRET - 1
         FACT = 0.
         ENDIF
      DO 70 JH = 1,4
      PGFIT(JH,JF) = FACT * PGFIT(JH,JF)
  70  CONTINUE
  90  CONTINUE
C
      DO 110 JH = 1,4
      P0FIT(JH) = PGFIT(JH,1) + PGFIT(JH,2)
 110  CONTINUE

      XMFIT = P0FIT(4)*P0FIT(4) - P0FIT(1)*P0FIT(1) -
     &        P0FIT(2)*P0FIT(2) - P0FIT(3)*P0FIT(3)
      IF (XMFIT.LT.0.) XMFIT = 1.E-6
      XMFIT = SQRT(XMFIT)
C
C  Check if the loop is finished :
C
      IF (IRET.LT.0) GO TO 300
C
      IF (ABS(XMFIT-XMP0).LT.XMCUT) THEN
         IRET = ITER
         GO TO 300
      ENDIF
C
      IF (ITER.GE.ITMAX) THEN
         IRET = ITMAX + 1
         GO TO 300
      ENDIF
C
      ITER = ITER + 1
      GO TO 30
 300  CONTINUE
C
C.. COMPUTE chi2 after fit
C
      CHI2 = (PGAM1(4)-PGFIT(4,1))**2/S1+
     +       (PGAM2(4)-PGFIT(4,2))**2/S2
C
      RETURN
      END
#endif
