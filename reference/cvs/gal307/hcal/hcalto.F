      REAL FUNCTION HCALTO(ISUB,IMD)
C--------------------------------------------------------------------
C
C!  Evaluates the total calibration constant applied to data in
C!  the year of generation
C!
C!          Author: A. Sciaba`   24/04/98
C!
C!
C!        -Called by : HCFORA
C--------------------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "iocom.h"
#include "bcs.h"
#include "hcalct.h"
#include "bmacro.h"
C -------------------------------------------------------------
C
      HCALTO = 1.
      IF(IMD.EQ.0) RETURN
C
C     If barrel:
      IF(ISUB.EQ.1) THEN
         HCALTO = CT0MCB * CALMCB(IMD) / ( CALTUB * CALTBA
     &        * CALBA(IMD) * CALIMB(IMD) * CT0DAB )
C     If endcap:
      ELSEIF(ISUB.EQ.2) THEN
         HCALTO = CT0MCE * CALMCE(IMD) / ( CALTUE * CALTEC
     &        * CALEC(IMD) * CALIME(IMD) * CT0DAE )
      ENDIF
C
      RETURN
      END
#endif



