      SUBROUTINE VUNADD (IADDR,NECH,ILAY,IROM,IFAC,IVIEW,IECH)
C ----------------------------------------------------------------------
C!  Unpack a full VHLS address including number of electronics channels
CKEY VDETDES UNPACK ADDRESS / USER
C - Steve Wasserbaech, 28 July 1995
C
C   This routine unpacks a bit-packed address as found in the VHLS and
C   VFHL banks into its components.  VPKADD packs these addresses.
C
C   Packing scheme: depends on VDET version.  See documentation in
C   comdeck VHLSBT.
C
C   WARNING: the VDET Geometry Package must be initialized (by means
C   of a call to VRDDAF) before VPKADD/VUNADD will work.
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
#include "vglobl.h"
#include "vdetge.h"
#include "vhlsbt.h"
C
C     Arguments:
      INTEGER IADDR, NECH, ILAY, IROM, IFAC, IVIEW, IECH
C
C ----------------------------------------------------------------------
C
      IF (ISSLAY .GT. 0) THEN
C
        NECH  = IAND(ISHFT(IADDR,-ISSNST),MSVNST)
        ILAY  = IAND(ISHFT(IADDR,-ISSLAY),MVLAY)+1
        IROM  = IAND(ISHFT(IADDR,-ISWAF),MSVWAF)+1
        IFAC  = IAND(ISHFT(IADDR,-ISPHI),MVPHI)+1
        IVIEW = IAND(ISHFT(IADDR,-ISVIEW),MVVIEW)+1
        IECH  = IAND(ISHFT(IADDR,-ISSTRP),MVSTRP)
C
      ELSE
C
C     VDET Geometry package not initialized!
        NECH  = 0
        ILAY  = 0
        IROM  = 0
        IFAC  = 0
        IVIEW = 0
        IECH  = 0
C
      ENDIF
C
      RETURN
      END
#endif
