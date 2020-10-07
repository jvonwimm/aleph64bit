      SUBROUTINE VPKADD (IADDR,NECH,ILAY,IROM,IFAC,IVIEW,IECH)
C ----------------------------------------------------------------------
C!  Pack a full VHLS address including number of electronics channels
CKEY VDETDES PACK ADDRESS / USER
C - Steve Wasserbaech, 28 July 1995
C
C   This routine packs a bit-packed address as found in VHLS or VFHL
C   banks, including the number of electronics channels or readout
C   strips.  VUNADD unpacks these addresses.
C
C   Packing scheme: depends on VDET version.  See documentation in
C   comdeck VHLSBT.
C
C   WARNING: the VDET Geometry Package must be initialized (by means
C   of a call to VRDDAF) before VPKADD/VUNADD will work.
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
        IADDR = IOR(IOR(IOR(IOR(IOR(
     &            ISHFT(IECH,ISSTRP),
     &            ISHFT(IVIEW-1,ISVIEW)),
     &            ISHFT(IFAC-1,ISPHI)),
     &            ISHFT(IROM-1,ISWAF)),
     &            ISHFT(ILAY-1,ISSLAY)),
     &            ISHFT(NECH,ISSNST))
C
      ELSE
C
C     VDET Geometry package not initialized!
        IADDR = 0
C
      ENDIF
C
      RETURN
      END
#endif
