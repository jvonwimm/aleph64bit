      SUBROUTINE MMAPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,THETA,PHI)
C------------------------------------------------------------------
C! muon middle angle : convert local XX,YY to polar coordinates
C!
C!   F.Bossi/G.Capon/D.Kuhn             861107
C!
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "mupar1.h"
#include "mmg1da.h"
#include "mrddat.h"
#include "alcons.h"
C
C?    values for shift of modules on B side taken from A side
C?    rotating by pi around y global
C
      NSL=NSLOT-NSLBAR
      PHI=PI/6.*FLOAT(INPHIM(NSL)-1)
      YCS=WIDTHM(NSL)/2.
      SIGNZ=1.
      IF (NSL.GE.20) SIGNZ=-1.
      DYCS = SIGNZ*(YCS - YY)+ CENDVM(NSL)
      ZL = SIGNZ*(ZMODUL(NSL) + XX)
      RINT=RINTEM(NSL)+0.5*THICKM+ZZOFFS(ISUBC,LAYER)
      DFI=ATAN2(DYCS,RINT)
      PHI=PHI+DFI
      RR=RINT/COS(DFI)
      RHO = SQRT(RR*RR + ZL*ZL)
      THETA=ACOS(ZL/RHO)
C
      RETURN
      END
#endif
