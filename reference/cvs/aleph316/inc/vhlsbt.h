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
#if defined(DOC)
      Packing schemes for VHLS channel/strip addresses:

      > VDET91 (wafer number gets two bits):
      Bits 0-9:  IECH (or IROS)          (0-1023)
            10:  IVIEW - 1               (0-1)
         11-14:  IFAC - 1                (0-14)
         15-16:  IROM - 1 (or IWFF - 1)  (0-3)
            17:  ILAY - 1                (0-1)
         18-31:  NECH (or NROS)          (0-16383)

      > VDET95 (wafer number gets three bits):
      Bits 0-9:  IECH (or IROS)          (0-1023)
            10:  IVIEW - 1               (0-1)
         11-14:  IFAC - 1                (0-14)
         15-17:  IROM - 1 (or IWFF - 1)  (0-7)
            18:  ILAY - 1                (0-1)
         19-31:  NECH (or NROS)          (0-8191)

      where IECH  = electronics channel number
            IROS  = readout strip number
            IVIEW = view number (=1 for z, =2 for r-phi)
            IFAC  = local face index
            IROM  = readout module number
            IWFF  = wafer-in-face index
            ILAY  = layer number
            NECH  = number of channels in cluster
            NROS  = number of strips in cluster

      MVSTRP = bit mask for readout strip number (=3FF)
      MVVIEW = bit mask for view number (=1)
      MVPHI  = bit mask for local face index (=F)
      MVWAF  = bit mask for wafer number, VDET91 (=3)
      MVLAY  = bit mask for layer number (=1)
      MVNSTR = bit mask for number of strips, VDET91 (=3FFF)

      ISSTRP = bit shift for electronics channel number
      ISVIEW = bit shift for view number
      ISPHI  = bit shift for local face index
      ISWAF  = bit shift for readout module number
      ISLAY  = bit shift for layer number, VDET91
      ISNSTR = bit shift for number of channels, VDET91

      M3VWAF = bit mask for wafer number, VDET95 (=7)
      M3VNST = bit mask for number of strips, VDET95 (=1FFF)

      I3SLAY = bit shift for layer number, VDET95
      I3SNST = bit shift for number of channels, VDET95

#endif
