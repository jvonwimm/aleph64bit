      SUBROUTINE KINEFIT(PGAM1,PGAM2,PPI0,CHISQ,PROBP0,IFAIL)
CKEY QPI0DO / INTERNAL
C-----------------------------------------------------------------------
C! Kinematic fitting of the pi0 mass
C    Author   :- Marcello Maggi        27-Jan-1992
C    Called from QPI0DO
C
C    Inputs:
C         - PGAM1         : 4 momentum of first photon
C         - PGAM2         : 4 momentum of second photon
C
C    Outputs:
C         - PROBP0  /R    : probability to form a pi0
C         - IFAIL   /I    : return code (0 means OK!)
C         - PPI0    /R    : Refitted pi0 momentum
C    Description
C    ===========
C
C  This set of routines perform a kinematical fit to the pi0 mass.
C  The chisquare is defined as:
C    ((E1measured-E1fitted)/Sigma1Eresolution))**2 +
C    ((E2measured-E2fitted)/Sigma2Eresolution))**2 +
C    ( ((cos(angle between photon)-(1-pimass**2/2/E1fitted/e2fitted))/
C                                    SigmacoshtetaResolution) **2
C   and it is minimized by a Newtonian iterative procedure, getting
C   E1fitted and E2fitted as the result of the fit.
C   The resolutions are taken from the nominal resolution of ECAL and
C   depend on the Energy fitted.
C   The Probability is then computed for 1 d.o.f.
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      DIMENSION PGAM1(4), PGAM2(4),PG1FIT(4),PG2FIT(4),PPI0(4)
      DOUBLE PRECISION E1REC,E2REC,T1REC,T2REC,P1REC,P2REC
      DOUBLE PRECISION COSP0
      DOUBLE PRECISION ERIS,TRISA,TRISB
      DOUBLE PRECISION DE1,DE2,DA1,DA2,DELTACOS
      DOUBLE PRECISION EFIT(2),EFIN(2),SHES(2,2),GED(2),CHI2,CHI2N
C.. ERIS: energy resolution            SIGMA=ERIS/SQRT(E)
C.. TRISA,TRISB angular resolution     SIGMA=TRISA+TRISB/SQRT(E)
C.. NITER maximum number of iterations
      PARAMETER (ERIS=0.18D0 , TRISA=0.00032D0 , TRISB=0.002816D0)
      PARAMETER (PIMASS=0.1349739)
      PARAMETER (NITER=50)
C-----------------------------------------------------------------------
      E1REC = DBLE(PGAM1(4))
      E2REC = DBLE(PGAM2(4))
      THET1 = ACOS(PGAM1(3)/PGAM1(4))
      PHI1  = ATAN2(PGAM1(2),PGAM1(1))
      THET2 = ACOS(PGAM2(3)/PGAM2(4))
      PHI2  = ATAN2(PGAM2(2),PGAM2(1))
      CST12 = VDOTN(PGAM1,PGAM2,3)
C
      COSP0 = DBLE(CST12)
C
      T1REC = DBLE(THET1)
      T2REC = DBLE(THET2)
      P1REC = DBLE(PHI1)
      P2REC = DBLE(PHI2)
*
* compute the resolutions
*
      DE1 = DSQRT(E1REC)*ERIS
      DE2 = DSQRT(E2REC)*ERIS
      DA1 = TRISA + TRISB/DSQRT(E1REC)
      DA2 = TRISA + TRISB/DSQRT(E2REC)
      CALL DECOS(T1REC,P1REC,T2REC,P2REC,DA1,DA2,DELTACOS)
*
* let's start the fit
*
      EFIT(1)=E1REC
      EFIT(2)=E2REC
      CALL CHISCOM(EFIT,E1REC,E2REC,COSP0,DE1,DE2,DELTACOS,CHI2)
      CHI2N=9999.D0
      NVOLTE=0
      DO NVOLTE=1,NITER
        CALL HESSE(EFIT,COSP0,DE1,DE2,DELTACOS,SHES,IFAIL)
        IF(IFAIL.NE.0) THEN
          WRITE (IW(6),*)' ## KINEFIT ## : I CANNOT FIT'
          GOTO 70
        ENDIF
        CALL DERIV(EFIT,E1REC,E2REC,COSP0,DE1,DE2,DELTACOS,GED)
        CALL NEWTONIT(EFIT,GED,SHES,EFIN)
        DE1=DSQRT(EFIN(1))*ERIS
        DE2=DSQRT(EFIN(2))*ERIS
        DA1 = TRISA + TRISB/DSQRT(EFIN(1))
        DA2 = TRISA + TRISB/DSQRT(EFIN(2))
        CALL DECOS(T1REC,P1REC,T2REC,P2REC,DA1,DA2,DELTACOS)
        CALL CHISCOM(EFIN,E1REC,E2REC,COSP0,DE1,DE2,DELTACOS,CHI2N)
        IF(DABS(CHI2N-CHI2).GT.0.0001) THEN
          CHI2=CHI2N
          EFIT(1)=EFIN(1)
          EFIT(2)=EFIN(2)
        ELSE
          GO TO 70
        ENDIF
      ENDDO
   70 CONTINUE
C
      E1FIT=SNGL(EFIN(1))
      SCAL1 = E1FIT/PGAM1(4)
      CALL VSCALE(PGAM1,SCAL1,PG1FIT,3)
      E2FIT=SNGL(EFIN(2))
      SCAL2 = E2FIT/PGAM2(4)
      CALL VSCALE(PGAM2,SCAL2,PG2FIT,3)
C
C.. Find the direction of refitted pi0
      CALL VADD(PG1FIT,PG2FIT,PPI0,3)
      PMOM2 = VDOT(PPI0,PPI0,3)
C
C.. Angular correction now
      EPI0 = E1FIT+E2FIT
      PFIT2= EPI0**2-PIMASS**2
      SCALE = SQRT(PFIT2/PMOM2)
      CALL VSCALE(PPI0,SCALE,PPI0,3)
      PPI0(4) = EPI0
      IF(E1FIT.LE.0.2) E1FIT=0.2
      IF(E2FIT.LE.0.2) E2FIT=0.2
      COFITTED=1.-PIMASS*PIMASS/2./E1FIT/E2FIT
      CHISQ = SNGL(CHI2N)
      PROBP0=PROB(SNGL(CHI2N),1)
      RETURN
      END
#endif
