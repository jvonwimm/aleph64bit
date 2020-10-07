      SUBROUTINE VADDPK (IADDR,NECH,ILAY,IROM,IFAC,IVIEW,IECH)
C ----------------------------------------------------------------------
C!  Pack a full VHLS address (for VDET91 only)
CKEY VDETDES PACK ADDRESS / USER
C - Dave Brown, 26-9-1990
C - Steve Wasserbaech, 2 November 1994: change variable names, doc
C
C   This routine packs a bit-packed address as found in VHLS or VFHL
C   banks, including the number of electronics channels or strip
C   channels.  VADDUN unpacks these addresses.
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
C      18-31:  NECH        (0-16383)
C
C - Input:
C   NECH  / I  Number of electronics channels (VHLS) or strip channels
C              (VFHL) in this cluster
C   ILAY  / I  Layer index of this cluster
C   IROM  / I  Readout module of this cluster
C   IFAC  / I  Local face index of this cluster
C   IVIEW / I  View number (=1 for z, =2 for r-phi) of this cluster
C   IECH  / I  First electronics channel (VHLS) or strip channel (VFHL)
C              number of this cluster.  The numbering of electronics
C              channels begins with zero; the numbering of strip
C              channels begins with one.
C
C - Output:
C   IADDR / I  Packed address
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vhlsbt.h"
C
C     Arguments:
      INTEGER IADDR, NECH, ILAY, IROM, IFAC, IVIEW, IECH
C
C ----------------------------------------------------------------------
C
      IADDR = IOR(IOR(IOR(IOR(IOR(
     &          ISHFT(IECH,ISSTRP),
     &          ISHFT(IVIEW-1,ISVIEW)),
     &          ISHFT(IFAC-1,ISPHI)),
     &          ISHFT(IROM-1,ISWAF)),
     &          ISHFT(ILAY-1,ISLAY)),
     &          ISHFT(NECH,ISNSTR))
C
      RETURN
      END
#endif
