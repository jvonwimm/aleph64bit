      SUBROUTINE X1WSUM
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 95/01/15  FOR ALEPHLIB 204
C.
C.
C! - Print Level1 Trigger Summary
C.
C.
C. - Called by      ASWSUM                        from .GALEPH
C ------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "x1tsto.h"
#include "xtrcom.h"
C ----------------------------------------------------------------------
C
C.
C. - Print trigger summary
C.
      WRITE(IW(6),100) NACCL1,NPHYTR
 100  FORMAT(///,
     x 22X,' Events accepted by Level1        :',I10,////,
     x 26X,' Single Muon            Trigger   :',I10,/,
     x 26X,' Single Charged El.mag. Trigger   :',I10,/,
     x 26X,' Single Neutral El.mag. Trigger   :',I10,/,
     x 26X,' Total Energy Barrel    Trigger   :',I10,/,
     x 26X,' Total Energy Endcap_A  Trigger   :',I10,/,
     x 26X,' Total Energy Endcap_B  Trigger   :',I10,/,
     x 26X,' Total Energy A*B       Trigger   :',I10,/,
     x 26X,' Bhabha LCAL            Trigger   :',I10,/,
     x 26X,' Bhabha SICAL           Trigger   :',I10)
C.
      RETURN
      END
#endif
