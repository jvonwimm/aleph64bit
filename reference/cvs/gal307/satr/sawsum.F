*DK sawsum
      SUBROUTINE SAWSUM
C-----------------------------------------------------------------------
C!    print out averages for the SATR test run
C                                H.Burkhardt October 1986
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "satrco.h"
      IF(XEVTSA.GT.0.) THEN
        WRITE(LOUTIO,*) ' +++SAWSUM+++ SATR run summary '
        WRITE(LOUTIO,*)
     &   ' Total number of events with hits in the SATR ',
     &     INT(XEVTSA)
        WRITE(LOUTIO,*)' For these events:'
        WRITE(LOUTIO,*)
     &   '   Average number of hits per event in sens. region  :',
     &       XHITSA/XEVTSA
        WRITE(LOUTIO,*)      
     &   '   Average number of hits lost in dead material      :',
     &       XLOSSA/XEVTSA
        WRITE(LOUTIO,*)
     &   '   Average number of wires set (excluding multihits) :',
     &       XWIRSA/XEVTSA
      ELSE
         WRITE (LOUTIO,*)' +++SAWSUM+++ NO signal in SATR'
      ENDIF
      END
#endif
