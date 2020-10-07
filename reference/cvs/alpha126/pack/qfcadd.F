      SUBROUTINE QFCADD (NTO, NFROM)
CKEY FILL CAL /INTERNAL
C----------------------------------------------------------------------
C! add 4-momenta of the composite calorimeter object NTO.
C! Called once for each contributing ECAL/HCAL object NFROM.
C
C! called from QFCASC
C!                                                   H.Albrecht 27.2.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qmacro.h"
      JQVEC(I,N) = KOQVEC + I * KCQVEC + N
C----------------------------------------------------------------------
C
      IF (XECAL(NFROM).AND.KNHCAL(NFROM).GT.0)  THEN
C
C       ECAL is ass. to several HCAL objects. Distribute the energy
C       equally amongst all composite objects.
C       Future : find a better solution but do not count energy twice!
C
        WEIGHT = 1. / FLOAT (KNHCAL(NFROM))
      ELSE
        WEIGHT = 1.
      ENDIF
C
C           add 4-momenta
C
      RW(JQVEC(NTO,JQVEQX)) = RW(JQVEC(NTO,JQVEQX)) +
     &  WEIGHT * RW(JQVEC(NFROM,JQVEQX))
      RW(JQVEC(NTO,JQVEQY)) = RW(JQVEC(NTO,JQVEQY)) +
     &  WEIGHT * RW(JQVEC(NFROM,JQVEQY))
      RW(JQVEC(NTO,JQVEQZ)) = RW(JQVEC(NTO,JQVEQZ)) +
     &  WEIGHT * RW(JQVEC(NFROM,JQVEQZ))
      RW(JQVEC(NTO,JQVEQE)) = RW(JQVEC(NTO,JQVEQE)) +
     &  WEIGHT * RW(JQVEC(NFROM,JQVEQE))
C
      W2 = WEIGHT**2
      RW(JQVEC(NTO,JQVEEM)) = RW(JQVEC(NTO,JQVEEM)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM))
      RW(JQVEC(NTO,JQVEEM+1)) = RW(JQVEC(NTO,JQVEEM+1)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM+1))
      RW(JQVEC(NTO,JQVEEM+2)) = RW(JQVEC(NTO,JQVEEM+2)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM+2))
      RW(JQVEC(NTO,JQVEEM+3)) = RW(JQVEC(NTO,JQVEEM+3)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM+3))
      RW(JQVEC(NTO,JQVEEM+4)) = RW(JQVEC(NTO,JQVEEM+4)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM+4))
      RW(JQVEC(NTO,JQVEEM+5)) = RW(JQVEC(NTO,JQVEEM+5)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM+5))
      RW(JQVEC(NTO,JQVEEM+6)) = RW(JQVEC(NTO,JQVEEM+6)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM+6))
      RW(JQVEC(NTO,JQVEEM+7)) = RW(JQVEC(NTO,JQVEEM+7)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM+7))
      RW(JQVEC(NTO,JQVEEM+8)) = RW(JQVEC(NTO,JQVEEM+8)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM+8))
      RW(JQVEC(NTO,JQVEEM+9)) = RW(JQVEC(NTO,JQVEEM+9)) +
     &  W2 * RW(JQVEC(NFROM,JQVEEM+9))
C
      END
#endif
