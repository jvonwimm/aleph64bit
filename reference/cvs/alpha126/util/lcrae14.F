
      integer function lcra_e14()
C-----------------------------------------------------------------------
C Maximum pulse height seen in the LCAL crack scintillators
C Author G. Taylor Oct 1997
C-----------------------------------------------------------------------
#include "bcs.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
      lcra_e14=-100
      klcra=iw(namind('LCRA'))
      if (klcra.gt.0) then
       do i=1,8
        if (itabl(klcra,1,i).gt.lcra_e14) lcra_e14=itabl(klcra,1,i)
       enddo
      endif
      return
      end
