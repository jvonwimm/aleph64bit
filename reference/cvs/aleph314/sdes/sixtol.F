      SUBROUTINE SIXTOL(IRD,IPH,IST,IMD,DRA,DPH,RLO,PLO,ZLO)
C.---------------------------------------------------------------------
CKEY SCALDES ENCODE ADDRESS / USER
C     B.BLOCH       October 91
C! Geometrical transformation routine from ARS cylindrical
C! coordinates indices to local cylindrical coordinates
C   Input :
C          IRD ,IPH,IST  R,PHI,Z indices of space point
C          IMD           Module  index
C          DRA           offset[0.,1.] Within bin IRD
C          DPH           offset[0.,1.] Within bin IPH of cluster z plane
C   Output:
C          RLO,PLO,ZLO   corresponding coordinates in local sytem
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
#include "sigeco.h"
#include "bmacro.h"
C
C Now local coordinates
C
      ITP = MOD(IST-1,3)+1
      RLO = RMINSI(IMD) + (IRD -1 )*RADSTP +DRA*RADSTP
      PLO = PHSHFT(ITP,IMD) + (IPH -1 )*PHISTP +DPH*PHISTP
      ZLO = Z0SNSI(IMD) + (IST -1 )* ZWIDSI
      RETURN
      END
#endif
