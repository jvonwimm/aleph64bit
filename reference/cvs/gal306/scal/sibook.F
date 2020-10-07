*DK sibook
      SUBROUTINE SIBOOK
C.-----------------------------------------------------------------
CKEY SICAL BOOK HISTO
C! Book SCAL histograms
C  B. BLOCH October 91
C. - Called by  ASBOOK                           from this .HLB
C.-----------------------------------------------------------------
#ifndef DOC
C
#include "sistat.h"
C
C    booking of histograms
C
      IDOF = 900
      NX = 50
      NY = 15
      X0 = 0.
      Y0 = 0.
      X1 = 100.
      X2 = 200.
      X3 = 50.
      WX = 0.
      CALL HBOOK1(IDOF + 1,'SCAL-Pad energy -analog signal',NX,X0,X1,WX)
      CALL HBOOK2(IDOF + 2,'SCAL-Pad energy A vs B-analog signal',NX,X0,
     $             X3,NX,X0,X3,WX)
      CALL HBOOK1(IDOF + 3,'SCAL-NPad/event -analog signal',NX,X0,X2,WX)
      CALL HBOOK1(IDOF + 6,'SCAL-NPad>20 Mev-analog signal',NX,X0,X2,WX)
      CALL HBOOK1(IDOF + 4,'SCAL- Z Profile -analog signal',NY,0.,15.,
     $                WX)
      CALL HBOOK2(IDOF + 5,'SCAL-NPad/plane vs plane -analog signal',
     $             NX,X0,X3,NY,0.,15.,WX )
      CALL HBOOK1(IDOF +11,'SCAL-Pad energy -digital signal',NX,X0,X1,
     $                 WX)
      CALL HBOOK2(IDOF +12,'SCAL-Pad energy A vs B-digital signal',NX,
     $          X0,X3,NX,X0,X3,WX)
      CALL HBOOK1(IDOF +13,'SCAL-NPad/event -digital signal',NX,X0,X2,
     $              WX)
      CALL HBOOK1(IDOF +16,'SCAL-NPad>20Mev -digital signal',NX,X0,X2,
     $              WX)
      CALL HBOOK1(IDOF +14,'SCAL- Z Profile -digital signal',NY,0.,15.,
     $                WX)
      CALL HBOOK2(IDOF +15,'SCAL-NPad/plane vs plane -digital signal',
     $             NX,X0,X3,NY,0.,15.,WX )
      RETURN
      END
#endif
