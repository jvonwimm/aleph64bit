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
