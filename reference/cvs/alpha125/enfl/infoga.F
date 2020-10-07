      SUBROUTINE INFOGA(IPH,ICLUS,NCHFLG,NCRFLG,NGA)
C-----------------------------------------------------------------
C! Return information about a photon IPH
C
C  Input:   -- IPH,    the photon index (ENFLW object)
C  Output:  -- ICLUS,  the original index of the ECALobject
C           -- NCHFLG, 1 if the photon has been extracted from
C                        a charged cluster, 0 otherwise
C           -- NCRFLG, 1 if the photon is in a crack region
C                      0 otherwise.
C           -- NG,     number of extracted photons in cluster ICLUS
C
C  Patrick Janot - 02 July 1991
C-----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
      iclus = 0
      nchflg = 0
      ncrflg = 0
      nga = 0
      iiph = ichefw(iph)
      IF ( iiph .LT. kfrtot(3) .OR. iiph .GT. klstot(3)) GOTO 999
      iclus = kalpha(iiph)
      nchflg = nphoch(iiph-kfrtot(3)+1)
      ncrflg = nphocr(iiph-kfrtot(3)+1)
      DO 1 jph = kfrtot(3), klstot(3)
    1 IF ( kalpha(jph) .EQ. iclus ) nga = nga+1
C
  999 RETURN
      END
#endif
