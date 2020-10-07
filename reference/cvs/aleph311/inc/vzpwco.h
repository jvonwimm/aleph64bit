C!    Common for VZPW data: Wafer z positions
C ----------------------------------------------------------------------
      INTEGER NWAFEF
      REAL WAFERZ
C
      COMMON / VZPWCO / NWAFEF, WAFERZ(NVWFMX)
C
#if defined(DOC)
      NWAFEF        Number of wafers per face
      WAFERZ(IWFF)  z coordinate (cm) of wafer-in-face IWFF
#endif
