*CD gmacro     SET OF INTRINSIC FUNCTIONS TO ANALYZE GEANT STEP
#include "gckine.h"
#include "gctmed.h"
#include "gctrak.h"
#include "gcvolu.h"
      CHARACTER*4 CHAHOL
      EXTERNAL CHAHOL
C - STOP flag for particles which create a shower
      PARAMETER (NOMOR = 3)
      LOGICAL FIRST,FINTO,FSENS,FGAMS,FCHRG,FTINO
C - 1st point in the volume
      FIRST = IGNWVO.EQ.1
C - point is inside the volume
      FINTO = IGNWVO.EQ.0 .OR. IGNWVO.EQ.2
C - it is a charge particle
      FCHRG = GCHARG.NE.0.
C - the particle is a geantino
      FTINO = IGTRTY.EQ.6
C - photon which stops loosing all its energy
      FGAMS = IGTRTY.EQ.1 .AND. NGKINE.LT.2 .AND. IGSTOP.NE.0
C - the point is in a sensitive region
      FSENS = IGSVOL.GT.0
C
