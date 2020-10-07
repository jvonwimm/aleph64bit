C!    Common for miscellaneous calculated VDET geometry quantities
C ----------------------------------------------------------------------
      REAL RVDMIN, RVDMAX, ZVDMAX, WAXCEN, WAYCEN, WAZCEN
      REAL WARHOC, WAPHIC, CPHIOF, SPHIOF, TNWTLT, AMXSRZ, AMXSRP
      REAL BMXSRZ, BMXSRP
      INTEGER NZSROM, NPSROM, NZSMOD, NPSMOD, NPRSSC, NZROMM
      INTEGER MSVWAF, MSVNST, ISSLAY, ISSNST
      LOGICAL LZMULT
C
      COMMON / VDETGE / RVDMIN, RVDMAX, ZVDMAX,
     >                  WAXCEN(NVWMAX), WAYCEN(NVWMAX), WAZCEN(NVWMAX),
     >                  WARHOC(NVFMAX), WAPHIC(NVFMAX), CPHIOF(NVFMAX),
     >                  SPHIOF(NVFMAX), TNWTLT(NVLAYR), AMXSRZ, AMXSRP,
     >                  BMXSRZ, BMXSRP, NZSROM, NPSROM, NZSMOD, NPSMOD,
     >                  NPRSSC, NZROMM, LZMULT, MSVWAF, MSVNST, ISSLAY,
     >                  ISSNST
C
#if defined(DOC)
      RVDMIN         Minimum radius (cm) of VDET wafers
      RVDMAX         Maximum radius (cm) of VDET wafers
      ZVDMAX         Maximum abs(z) (cm) of VDET wafers
      WAXCEN(JWAF)   x coordinate (cm) of wafer center
      WAYCEN(JWAF)   y coordinate (cm) of wafer center
      WAZCEN(JWAF)   z coordinate (cm) of wafer center
      WARHOC(JFAC)   Distance between wafer center and origin in xy
      WAPHIC(JFAC)   phi (rad) of wafer center
      CPHIOF(JFAC)   cos(PHIOFF)
      SPHIOF(JFAC)   sin(PHIOFF)
      TNWTLT(JLAY)   tan(WATILT)
      AMXSRZ         Maximum a coord (cm) of sensitive region, z
      AMXSRP         Maximum a coord (cm) of sensitive region, r-phi
      BMXSRZ         Maximum b coord (cm) of sensitive region, z
      BMXSRP         Maximum b coord (cm) of sensitive region, r-phi
      NZSROM         Number of strip channels per readout module, z
      NPSROM         Number of strip channels per readout module, r-phi
      NZSMOD         Number of strip channels per module, z
      NPSMOD         Number of strip channels per module, r-phi
      NPRSSC         Number of readout strips per strip channel, r-phi
      NZROMM         Number of readout modules per module, z
      LZMULT         = .TRUE. if z readout is multiplexed
      MSVWAF         Bit mask for wafer number in packed address
      MSVNST         Bit mask for number of strips in packed address
      ISSLAY         Bit shift for layer number in packed address
      ISSNST         Bit shift for number of channels in packed address

#endif
