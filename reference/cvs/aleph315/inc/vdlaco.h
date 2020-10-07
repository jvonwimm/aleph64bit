C!    Common for VDLA data: VDET layers
C ----------------------------------------------------------------------
      INTEGER IORIEN
      REAL RWVDLA, WATILT
C
      COMMON / VDLACO / RWVDLA(NVLAYR), WATILT(NVLAYR), IORIEN(NVLAYR)
C
#if defined(DOC)
      RWVDLA(JLAY)  Perp distance between z axis and plane of wafers
      WATILT(JLAY)  Wafer tilt angle (rad)
      IORIEN(JLAY)  Wafer orient (=1 if rphi inward; =2 if z inward)
#endif
