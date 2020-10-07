      SUBROUTINE USBOS (NAME,NR,LE,KNDX,IGARB)
C ------------------------------------------------------------
CKEY ALEF BOS BANK / USER
C - F.Ranjard - 900220
C! user routine called when not enough space
C  dummy routine on the ALEPHLIB
C - Input    : NAME   / CHAR4 = bank name
C              NR     / I     = bank number
C              LE     / I     = bank length
C              KNDX   / I     = bank index
C              IGARB  / I     = garbage collection flag
C - Called by: AUBOS when not enough space after a garbage
C              collection.
C
C -------------------------------------------------------------
#ifndef DOC
      CHARACTER*4 NAME
#include "bcs.h"
C --------------------------------------------------------------
      END
#endif
