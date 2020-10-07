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
      CHARACTER*4 NAME
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C --------------------------------------------------------------
      END
