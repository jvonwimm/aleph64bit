*DK tgfini
      SUBROUTINE TGFINI
C---------------------------------------------------------------
C! Flush buffers, drop work banks
C
C  Called from: GALEPH/TPWSUM
C  Calls      : WDROP
C
C  P. Janot  --  24 June 1988
C---------------------------------------------------------------
#include "bcs.h"
#include "tpcbos.h"
#include "tpcond.h"
#include "faster.h"
C
C  Drop work Banks used in TPCSIM
C
      DO 1 KCHAN=1,NCHAN
        IF(INDREF(KCHAN) .NE. 0) CALL WDROP(IW,INDREF(KCHAN))
1     CONTINUE
      CALL WDROP(IW,ITPULS)
      CALL WDROP(IW,ITSNOI)
      CALL WDROP(IW,ITPNOI)
      CALL WDROP(IW,ITSHAP)
      CALL WDROP(IW,ITMADC)
C
C  Drop Landau Fluctuation Banks
C
      CALL BDROP(IW,'TLAN')
      CALL WDROP(IW,INTWRD)
      CALL BDROP(IW,'TIND')
      RETURN
      END
