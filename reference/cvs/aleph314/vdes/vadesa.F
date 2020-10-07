      SUBROUTINE VADESA (IADDR,ILAY,IROM,IFAC,IVIEW,IECH)
C ----------------------------------------------------------------------
C!  Decode an electronics channel address (for VDET91 only)
CKEY VDETDES DECODE ADDRESS / USER
C - Francesco Forti, 3 August 1986
C - Steve Wasserbaech, 6 February 1995: use parameters in VHLSBT
C
C   Decode an electronics channel or strip channel address.  (This is
C   the same as VADDUN except the number of channels is not unpacked.)
C   VAENSA packs these addresses.  See VAENSA for the packing scheme.
C
C   WARNING: this routine works only for VDET91.
C   Use VUNADD for general applications.
C
C   Note: up to and including ALEPHLIB version 204, VADESA gave an
C   error condition if IADDR = 0 was input; in the present version,
C   IADDR = 0 is valid.
C
C - Input:
C   IADDR / I  Packed address
C
C - Output:
C   ILAY  / I  Layer index of this channel
C   IROM  / I  Readout module of this channel
C   IFAC  / I  Local face index of this channel
C   IVIEW / I  View number (=1 for z, =2 for r-phi) of this channel
C   IECH  / I  Electronics channel or strip channel number
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
      ILAY  = IAND(ISHFT(IADDR,-ISLAY),MVLAY)+1
      IROM  = IAND(ISHFT(IADDR,-ISWAF),MVWAF)+1
      IFAC  = IAND(ISHFT(IADDR,-ISPHI),MVPHI)+1
      IVIEW = IAND(ISHFT(IADDR,-ISVIEW),MVVIEW)+1
      IECH  = IAND(ISHFT(IADDR,-ISSTRP),MVSTRP)
C
      RETURN
      END
#endif
