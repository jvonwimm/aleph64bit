      FUNCTION THPADD(PHIN,PHOUT)
C
C--------------------------------------------------------------------
C! Add PH of two half pads
C!
C!    Author:      R. Johnson  17-10-86
C!    Modified:    A. ROTH     01-03-90
C!
C!    Input:
C!         - PHIN    /R   Pulse height of inner half pad
C!         - PHOUT   /R   Pulse height of outer half pad
C!
C!    Called by:  TCOALG
C!
C!===================================================================
#ifndef DOC
#include "bcs.h"
#include "ternjj.h"
C
C
      DATA NTERN/0/
C
      IF (NTERN.EQ.0) NTERN=NAMIND('TERN')
      KTERN=IW(NTERN)
      IF (KTERN.EQ.0) THEN
        CALL RERROR('THPADD',1,'Cannot find the TERN bank.'//
     +               '  Half pad pulse height uncorrected.')
        THPADD = PHIN + PHOUT
        RETURN
      ELSE
        CORR11 = RW(KTERN+LMHLEN+JTERD1)
        CORR21 = RW(KTERN+LMHLEN+JTERD2)
        CORR22 = RW(KTERN+LMHLEN+JTERD3)
      ENDIF
C
      IF (PHOUT .LT. 0.01) THEN
C--      Outer half pad does not contribute to sub-cluster
         PHIN  = PHIN  * CORR11
      ELSE
C--      Two half pads contribute to sub-cluster
         PHIN  = PHIN  * CORR21
         PHOUT = PHOUT * CORR22
      END IF
C
      THPADD   = PHIN + PHOUT
      RETURN
      E N D
#endif
