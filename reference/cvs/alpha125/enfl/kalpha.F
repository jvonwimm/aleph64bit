      FUNCTION KALPHA(ICH)
C-----------------------------------------------------------------
C! Return the original ALPHA index of an ENFLW object.
C
C  Warning: It works only with charged particles and photons
C  since the neutral hadrons are oftently made of several
C  ECAL and HCAL objects...  Return 0 in that case.
C
C  Patrick Janot - 27 June 1991
C-----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
      jch = ichefw(ich)
      IF     ( jch .LT. kfrtot(1) ) THEN
        kalpha = 0
      ELSEIF ( jch .LE. klstot(1) ) THEN
        kalpha = kan(jch-kfrtot(1)+1)
      ELSEIF ( jch .LE. klstot(2) ) THEN
        kalpha = kav(jch-kfrtot(2)+1)
      ELSEIF ( jch .LE. klstot(3) ) THEN
        kalpha = kag(jch-kfrtot(3)+1)
      ELSE
        kalpha = 0
      ENDIF
C
      RETURN
      END
#endif
