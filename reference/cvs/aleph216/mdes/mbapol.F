      SUBROUTINE MBAPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,THETA,PHI)
C------------------------------------------------------------------
C! muon barrel : convert local XX,YY to polar coordinates
C!
C!   F.Bossi/G.Capon/D.Kuhn             861107
C!
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "mupar1.h"
#include "mbg1da.h"
#include "mrddat.h"
#include "alcons.h"
C
      XCS=WIDTHB(NSLOT)/2.
      DXCC=XX-XCS+CENDVB(NSLOT)
      RINT=RINTEB(NSLOT)+0.5*THICKB+ZZOFFS(ISUBC,LAYER)
      DFI=ATAN2(DXCC,RINT)
      PHI=PI/6.*FLOAT(INPHIB(NSLOT)-1)
      PHI=PHI+DFI
      RR=RINT/COS(DFI)
      DHEMS=YY-ZWIDTB(NSLOT)/2.
      RHO = SQRT(RR*RR + DHEMS*DHEMS)
      THETA=ACOS(DHEMS/RHO)
C
      RETURN
      END
#endif
