      SUBROUTINE BISELU(LINDAT,LINSEL,LINSE2,LOTDAT)

*     initialize the input units:
*        LINDAT   = input data record unit
*        LINSEL   = input select record unit
*        LINSE2   = secondary input select unit
*        LOTDAT   = output data unit, if record pointers should refer
*                   to this output unit (0 means: record pointer
*                   refer to input data unit)
#include "boscom.h"
      LUNDAT=LINDAT
      LUNSEL=LINSEL
      LUNSE2=LINSE2
      LUTDAT=LOTDAT
      MASKR =0
  100 RETURN
      END
