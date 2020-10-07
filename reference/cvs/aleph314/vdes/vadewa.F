      SUBROUTINE VADEWA (NBNUM,ILAY,IWFF,IFAC,IVIEW)
C ----------------------------------------------------------------------
C!  Decode wafer address into bank number
CKEY VDETDES DECODE WAFER / USER
C - Dave Brown, 9-26-1990
C - Steve Wasserbaech, 2 November 1994: change variable names, doc
C
C   This routine unpacks a base-10 encoded address (used as the bank
C   number for the VDXY and VDZT banks) into its wafer address
C   components.  VAENWA packs these addresses.  See VAENWA for the
C   packing scheme.
C
C - Input:
C   NBNUM / I  Bank number
C
C - Output:
C   ILAY  / I  Layer index of this wafer
C   IWFF  / I  Local wafer-in-face index
C   IFAC  / I  Local face index of this wafer
C   IVIEW / I  View number (=1 for z, =2 for r-phi)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C
C     Arguments:
      INTEGER NBNUM, ILAY, IWFF, IFAC, IVIEW
C
C ----------------------------------------------------------------------
C
      IVIEW = MOD(NBNUM,10) + 1
      IFAC  = MOD(NBNUM/10,100) + 1
      IWFF  = MOD(NBNUM/1000,10) + 1
      ILAY  = NBNUM/10000 + 1
C
      RETURN
      END
#endif
