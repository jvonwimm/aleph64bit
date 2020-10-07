      SUBROUTINE BSEQH(NAME,NR,IRUN,IEVT)

*     define another pair (name,nr) for events

      CHARACTER*4 NAME
#include "boscom.h"
      IF(NRE.GE.3) GOTO 100
      NRE=NRE+1
      NAMERE(NRE)=INTCHA(NAME)
      NUMMRE(NRE)=NR
      IRUNRE(NRE)=IRUN
      IEVTRE(NRE)=IEVT
  100 RETURN
      END
