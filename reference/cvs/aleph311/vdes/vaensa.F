      SUBROUTINE VAENSA (IADDR,ILAY,IROM,IFAC,IVIEW,IECH)
C ----------------------------------------------------------------------
C!  Encode an electronics channel address (for VDET91 only)
CKEY VDETDES ENCODE ADDRESS / USER
C - Francesco Forti, 3 August 1986
C - Steve Wasserbaech, 6 February 1995: use parameters in VHLSBT
C
C   Encode an electronics channel or strip channel address.  (This is
C   the same as VADDPK except the number of channels is not packed.)
C   VADESA unpacks these addresses.
C
C   WARNING: this routine works only for VDET91.
C   Use VPKADD for general applications.
C
C   Packing scheme:
C   Bits 0-9:  IECH        (0-1023)
C         10:  IVIEW - 1   (0-1)
C      11-14:  IFAC - 1    (0-14)
C      15-16:  IROM - 1    (0-3)
C         17:  ILAY - 1    (0-1)
C
C - Input:
C   ILAY  / I  Layer index of this channel
C   IROM  / I  Readout module of this channel
C   IFAC  / I  Local face index of this channel
C   IVIEW / I  View number (=1 for z, =2 for r-phi) of this channel
C   IECH  / I  Electronics channel or strip channel number
C
C - Output:
C   IADDR / I  Packed address
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vhlsbt.h"
C
C     Arguments:
      INTEGER IADDR, ILAY, IROM, IFAC, IVIEW, IECH
C
C ----------------------------------------------------------------------
C
      IADDR = IOR(IOR(IOR(IOR(
     &          ISHFT(IECH,ISSTRP),
     &          ISHFT(IVIEW-1,ISVIEW)),
     &          ISHFT(IFAC-1,ISPHI)),
     &          ISHFT(IROM-1,ISWAF)),
     &          ISHFT(ILAY-1,ISLAY))
C
      RETURN
      END
#endif
