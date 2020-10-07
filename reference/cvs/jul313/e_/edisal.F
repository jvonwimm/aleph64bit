      SUBROUTINE EDISAL(IDTPC)
C********************************************************
C! Compute distance for alignement (sum,x,y,z) in Ecal
C! Create histograms of distance for each module(36)
C!  and may create auxiliary storage file to modify
C!  alignement correction matrix.
C! Author : JP Albanese
C!
C*********************************************************
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "eanclu.h"
#include "eclujj.h"
#include "etp1jj.h"
#include "ect1jj.h"
#include "erl3jj.h"
#include "ecnamc.h"
#include "bmacro.h"
C
      KECLU = IW(NAECLU)
      KETP1 = IW(NAETP1)
C
      KLINE = KROW(KETP1,IDTPC)
      XT = RW(KLINE+JETPX3)
      YT = RW(KLINE+JETPX3+1)
      ZT = RW(KLINE+JETPX3+2)
      PX = RW(KLINE+JETPP3)
      PY = RW(KLINE+JETPP3+1)
      PZ = RW(KLINE+JETPP3+2)
      D = SQRT(PX*PX+PY*PY+PZ*PZ)
      PX = PX/D
      PY = PY/D
      PZ = PZ/D
      ET =RW(KLINE+JETPPT)
C? Here store data for alignement correction
C     WRITE(67)ET,XT,YT,ZT,PX,PY,PZ,XYZBEA(1),XYZBEA(2),XYZBEA(3)
C? Now compute distance to XYZBEA
      D1 = (XYZBEA(1)-XT)**2+(XYZBEA(2)-YT)**2+(XYZBEA(3)-ZT)**2
      D2 = PX*(XYZBEA(1)-XT)+PY*(XYZBEA(2)-YT)+PZ*(XYZBEA(3)-ZT)
      DX = XYZBEA(1)-XT-PX*D2
      DY = XYZBEA(2)-YT-PY*D2
      DZ = XYZBEA(3)-ZT-PZ*D2
      DI = D1 - D2*D2
      DI =SQRT(ABS(DI))
      CALL HFILL(4300+LMOLEA(1),DI,0.,1.)
C? FILL PROJECTION (NEED LOT OF SPACE)
C      CALL HFILL(4332+LMOLEA(1),DX,0.,1.)
C      CALL HFILL(4364+LMOLEA(1),DY,0.,1.)
C      CALL HFILL(4396+LMOLEA(1),DZ,0.,1.)
      RETURN
      END
#endif
