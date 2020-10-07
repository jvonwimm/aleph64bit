      SUBROUTINE VAENWA (NBNUM,ILAY,IWFF,IFAC,IVIEW)
C ----------------------------------------------------------------------
C!  Encode wafer address into bank number
CKEY VDETDES ENCODE WAFER / USER
C - Dave Brown, 9-26-1990
C - Steve Wasserbaech, 2 November 1994: change variable names, doc
C
C   This routine packs a base-10 encoded address (used as the bank
C   number for the VDXY and VDZT banks) from its wafer address
C   components.  VADEWA unpacks these addresses.
C
C   Packing scheme:
C   Bank number = LWFFV, where
C     L = layer number - 1            (= ILAY - 1);
C     W = wafer-in-face index - 1     (= IWFF - 1);
C    FF = local face index - 1        (= IFAC - 1);
C     V = view number - 1             (= IVIEW - 1).
C   Thus the lowest possible bank number produced is 0.
C
C - Input:
C   ILAY  / I  Layer index of this wafer
C   IWFF  / I  Local wafer-in-face index
C   IFAC  / I  Local face index of this wafer
C   IVIEW / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   NBNUM / I  Bank number
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C
C     Arguments:
      INTEGER NBNUM, ILAY, IWFF, IFAC, IVIEW
C
C ----------------------------------------------------------------------
C
      NBNUM = 10000*(ILAY-1)+1000*(IWFF-1)+10*(IFAC-1)+(IVIEW-1)
C
      RETURN
      END
#endif
