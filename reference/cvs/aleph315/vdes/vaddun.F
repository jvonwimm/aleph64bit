      SUBROUTINE VADDUN (IADDR,NECH,ILAY,IROM,IFAC,IVIEW,IECH)
C ----------------------------------------------------------------------
C!  Unpack a full VHLS address (for VDET91 only)
CKEY VDETDES UNPACK ADDRESS / USER
C - Dave Brown, 26-9-1990
C - Steve Wasserbaech, 1 November 1994: change variable names, doc
C
C   This routine unpacks a bit-packed address as found in the VHLS and
C   VFHL banks into its components.  VADDPK packs these addresses.
C   See VADDPK for the packing scheme.
C
C   WARNING: this routine works only for VDET91.
C   Use VUNADD for general applications.
C
C - Input:
C   IADDR / I  Packed address
C
C - Output:
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
      NECH  = IAND(ISHFT(IADDR,-ISNSTR),MVNSTR)
      ILAY  = IAND(ISHFT(IADDR,-ISLAY),MVLAY)+1
      IROM  = IAND(ISHFT(IADDR,-ISWAF),MVWAF)+1
      IFAC  = IAND(ISHFT(IADDR,-ISPHI),MVPHI)+1
      IVIEW = IAND(ISHFT(IADDR,-ISVIEW),MVVIEW)+1
      IECH  = IAND(ISHFT(IADDR,-ISSTRP),MVSTRP)
C
      RETURN
      END
#endif
