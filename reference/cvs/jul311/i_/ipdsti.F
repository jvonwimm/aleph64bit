      SUBROUTINE IPDSTI
C-----------------------------------------------------------------------
C! ITC Prepare Data Statistics Initialise
C!
C!   Author     :-  J. Sedgbeer  88/05/09
C!   Modified   :-
C!
C!   Input:  none
C!
C!   Output:  common: /IPDSTC/ prepare data stats. common initialised
C!
C!   called by : IINIJO
C!   calls     : none
C!
C-----------------------------------------------------------------------
#ifndef DOC
C I/O commons etc.
#include "ipdstc.h"
C-----------------------------------------------------------------------
      DO 10 I=1,NSTAIP
        ISTAIP(I) = 0
   10 CONTINUE
      ISTAIP(7)  = 9999
      ISTAIP(11) = 9999
C
      DO 30 I=1,2
        DO 20 J=1,LSTAIP
          ILSTIP(J,I) = 0
   20   CONTINUE
   30 CONTINUE
C
      DO 40 J=1,LSTAIP
        ILSTIP(J,3) = 9999
   40 CONTINUE
C
      END
#endif
