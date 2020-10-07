C!    Common for VWGE data: Wafer geometry
C ----------------------------------------------------------------------
      INTEGER NZSTRP, NPSTRP
      REAL WSIZEA, WSIZEB, STPITZ, STPITP, STLENZ, STLENP
      REAL AMNSRZ, AMNSRP, BMNSRZ, BMNSRP, WTHICK
C
      COMMON / VWGECO / WSIZEA, WSIZEB, NZSTRP, NPSTRP, STPITZ, STPITP,
     >                  STLENZ, STLENP, AMNSRZ, AMNSRP, BMNSRZ, BMNSRP,
     >                  WTHICK
C
#if defined(DOC)
      WSIZEA      Wafer dimension (cm) along a direction
      WSIZEB      Wafer dimension (cm) along b direction
      NZSTRP      Number of physical strips per wafer, z side
      NPSTRP      Number of physical strips per wafer, r-phi side
      STPITZ      Physical strip pitch (cm), z side
      STPITP      Physical strip pitch (cm), r-phi side
      STLENZ      Strip length (cm), z side
      STLENP      Strip length (cm), r-phi side
      AMNSRZ      Minimum a coord (cm) of sensitive region, z side
      AMNSRP      Minimum a coord (cm) of sensitive region, r-phi side
      BMNSRZ      Minimum b coord (cm) of sensitive region, z side
      BMNSRP      Minimum b coord (cm) of sensitive region, r-phi side
      WTHICK      Wafer thickness (cm)
#endif
