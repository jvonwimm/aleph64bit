         SUBROUTINE MXYLOC(ISUBC,LAYER,NSTRX,LCLUX,NSTRY,LCLUY,XX,YY)
C----------------------------------------------------------------
C!  converts x,y strip numbers to local coordinates
C!  NSTRPX,NSTRPY=strip #   LCLUX,LCLUY=cluster width
C!  XX,YY = local coordinates in muon chamber ref system
C!
C!   F.Bossi/G.Capon/D.Kuhn             861107
C!
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "mupar1.h"
#include "mrddat.h"
C
      FND8=NSTRX/16
      FNS8=MOD(NSTRX,16)/8
      FNSTR=MOD(NSTRX,8)
      XX=OFTBMU+FND8*WD16MU(ISUBC)+FNS8*WDEIMU+(FNSTR+0.5)*XPITCH
      IF (LCLUX.GT.0) THEN
         NLAST=NSTRX+LCLUX
         FND8=NLAST/16
         FNS8=MOD(NLAST,16)/8
         FNSTR=MOD(NLAST,8)
         XX=0.5*(XX+OFTBMU+FND8*WD16MU(ISUBC)+FNS8*WDEIMU+
     +   (FNSTR+0.5)*XPITCH)
      ENDIF
C
      FNY=FLOAT(NSTRY)+0.5*FLOAT(LCLUY)
      XX=XX+XXOFFS(ISUBC,LAYER)
      YY=FNY*YPITCH+YYOFFS(ISUBC,LAYER)
C
      RETURN
      END
#endif
