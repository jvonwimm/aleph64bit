*DK tpcdet
      FUNCTION TPCDET(ITYPE,KCHAN,JCHN)
C---------------------------------------------------------------------
C! Return the detector capacitance for channel type KCHAN
C  and number JCHN
C
C  Called from:  TSSHAM
C  Calls      :  None
C
C  Inputs:  PASSED:       --ITYPE, the sector type
C                         --KCHAN, the channel type
C                         --JCHN, the channel number
C
C            /TPGEOP/     --TDPHGT, the height of a long pad
C            /TPGEOT/     --TPTRBG, TPTRST, TPTPHW: trigger pad
C                           geometric factors to get trigger-pad lengths
C
C  Outputs:  Func. return --TPCDET, the capacitance in farads
C
C  D. DeMille
C-----------------------------------------------------------------------
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpgeot.h"
C
      DATA CPRCM/2.8E-12/
C
      IF ( KCHAN .EQ. 1 ) THEN
         CDET = 5.E-12
C
      ELSEIF ( KCHAN .EQ. 2 ) THEN
         CDET = CPRCM * TPDHGT(ITYPE)
C
      ELSEIF ( KCHAN .EQ. 3 ) THEN
C
         NROW = ((JCHN-1)/4) + 1
         NTPAD = JCHN - 4*(NROW-1)
C
         RAD = TPTRBG(ITYPE) + (NROW-1)*TPTRST(ITYPE)
         PHWD = 2.*TPTPHW(NTPAD,NROW,ITYPE)
         PADLN = RAD * PHWD
C
         CDET = CPRCM * PADLN
C
      ENDIF
C
      TPCDET = CDET
C
      RETURN
      END
