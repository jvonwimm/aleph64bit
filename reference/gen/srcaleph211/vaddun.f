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
C     IMPLICIT NONE
C!    Packing parameters for VHLS channel/strip addresses
C
C     Two packing schemes are possible: the wafer number
C     requires two bits for VDET91 and three bits for VDET95.
C     The routines VADDPK, VADDUN, VAENSA, and VADESA use the
C     VDET91 scheme only.  The routines VPKADD and VUNADD use
C     the scheme that is appropriate for the VDET setup that is
C     loaded in the VDET Geometry Package commons at the time
C     of the call.  The number of bits allocated for the wafer
C     number is read from the VRDO database bank.
C
C ----------------------------------------------------------------------
C
C     Bit masks (wafer number gets two bits, as in VDET91):
      INTEGER MVSTRP, MVVIEW, MVPHI, MVWAF, MVLAY, MVNSTR
      PARAMETER (MVSTRP = 1023)
      PARAMETER (MVVIEW = 1)
      PARAMETER (MVPHI  = 15)
      PARAMETER (MVWAF  = 3)
      PARAMETER (MVLAY  = 1)
      PARAMETER (MVNSTR = 16383)
C
C     Bit shifts (wafer number gets two bits, as in VDET91):
      INTEGER ISSTRP, ISVIEW, ISPHI, ISWAF, ISLAY, ISNSTR
      PARAMETER (ISSTRP = 0)
      PARAMETER (ISVIEW = 10)
      PARAMETER (ISPHI  = 11)
      PARAMETER (ISWAF  = 15)
      PARAMETER (ISLAY  = 17)
      PARAMETER (ISNSTR = 18)
C
C     Bit masks (wafer number gets three bits, as in VDET95):
C     The only differences with respect to VDET91:
      INTEGER M3VWAF, M3VNST
      PARAMETER (M3VWAF = 7)
      PARAMETER (M3VNST = 8191)
C
C     Bit shifts (wafer number gets three bits, as in VDET95):
C     The only differences with respect to VDET91:
      INTEGER I3SLAY, I3SNST
      PARAMETER (I3SLAY = 18)
      PARAMETER (I3SNST = 19)
C
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
