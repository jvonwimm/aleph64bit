      SUBROUTINE FMELOS(IRG,P,SLENG,DP)
C-----------------------------------------------------------------
C!  Compute energy loss in current step
C! J. Hilgart 23/03/88
C : Treat particles with Bethe-Bloch formula assuming muon mass
CBethe-Bloch: dE/dx = c1/beta**2*[ ln(c2*(beta*gam)**2)-beta**2][Mev/cm]
C-----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "fmgeom.h"
C
C
C Assume it's a muon.
      E = SQRT(P*P + XMUMS2)
      BETA = (P/E)
      BETA2 = BETA*BETA
C Beta*gamma = eta
      ETA2 = (P/XMUMAS)*(P/XMUMAS)
C
C Don't bother to use formula if FRATIO = 0.0
      IF (FRATIO(IRG) .LE. 0.0) THEN
         DP = 0.
         RETURN
      ELSE
         DE = FRATIO(IRG)*SLENG*(CONBB1(IRG)/BETA2)*
     &         (LOG(CONBB2(IRG)*ETA2) - BETA2)/1000.
      ENDIF
C
C Be careful about stopping particles
      IF (DE .LT. 0.) THEN
         DP = P
      ELSE
         DP = DE/BETA
      ENDIF
C
      RETURN
      END
#endif
