      INTEGER FUNCTION HNOISY(ITHE,IPHI)
C***********************************************************
C! Tells if an Hcal tower is noisy by looking in NOICOD.   *
C! Call HNOISE first (once per event) to fill NOICOD.      *
C! Author: S. Wasserbaech       061289                     *
C!                                                         *
C! Input:                                                  *
C!   ITHE = Theta index                                    *
C!   IPHI = Phi index                                      *
C!                                                         *
C! Output:                                                 *
C! HNOISY = -1  no hit in this tower;                      *
C!           0  good hit;                                  *
C!         999  ithe and/or iphi out of range.             *
C!        else  noise found in this tower; the set         *
C!              bits indicate the noise type, as           *
C!              defined in HNOISE.  Type i corresponds     *
C!              to bit (i-1) (bit numbering as in BTEST),  *
C!              where i=1,2,...,5.  At most two bits will  *
C!              be set: one for the barrel (Type 1, 2,     *
C!              or 3) and one for the endcaps (Type 4      *
C!              or 5).                                     *
C!                                                         *
C?
C!======================================================================
#ifndef DOC
      INTEGER ITHE, IPHI
#include "hnoicd.h"
      IF ((ITHE .LT. 1) .OR. (ITHE .GT. 62) .OR.
     >    (IPHI .LT. 1) .OR. (IPHI .GT. 96)) THEN
        HNOISY = 999
      ELSE
        HNOISY = NOICOD(ITHE,IPHI)
      ENDIF
      RETURN
      END
#endif
