      INTEGER FUNCTION VDSCRS (IVIEW,IROM,ISCH,IMOD,IWAF1,IROS1)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Convert strip channel number to readout strip number
C - Steve Wasserbaech, October 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   IROM   / I  Readout module
C   ISCH   / I  Strip channel number
C
C - Output:
C   VDSCRS / I  = VDOK if successful
C               = VDERR if error occurred
C   IMOD   / I  Local module index
C   IWAF1  / I  Local wafer index; if the strip channel is associate
C               with more than one readout strip, the strip with the
C               smallest IWAF is returned
C   IROS1  / I  Readout strip number
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
C
C     Labels for return codes:
C
      INTEGER VDERR, VDOK
      PARAMETER (VDERR = -1)
      PARAMETER (VDOK  = 1)
C
C     Labels for views:
C
      INTEGER VVIEWZ, VVIEWP
      PARAMETER (VVIEWZ = 1)
      PARAMETER (VVIEWP = 2)
C
C     Fixed VDET geometry parameters:
C
      INTEGER NVLAYR, NVMODF, NVVIEW, NPROMM, IROMAX
      PARAMETER (NVLAYR = 2)
      PARAMETER (NVMODF = 2)
      PARAMETER (NVVIEW = 2)
      PARAMETER (NPROMM = 1)
      PARAMETER (IROMAX = 4)
C
C     Array dimensions:
C
      INTEGER NVWMMX, NVWFMX, NVFLMX, NVFMAX, NVMMAX, NVWMAX
      INTEGER NVZRMX, NVPRMX
      PARAMETER (NVWMMX = 3)
      PARAMETER (NVWFMX = NVWMMX*NVMODF)
      PARAMETER (NVFLMX = 15)
      PARAMETER (NVFMAX = 24)
      PARAMETER (NVMMAX = NVFMAX*NVMODF)
      PARAMETER (NVWMAX = NVFMAX*NVWFMX)
      PARAMETER (NVZRMX = NVFMAX*IROMAX)
      PARAMETER (NVPRMX = NVMMAX*NPROMM)
C
C!    Common for VRDO data: Readout configuration
C ----------------------------------------------------------------------
      INTEGER NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP
      INTEGER NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
      COMMON / VRDOCO / NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP,
     >                  NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
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
C
C     Arguments:
      INTEGER IVIEW, IROM, ISCH, IMOD, IWAF1, IROS1
C
C     Local variables
      INTEGER IRET, IWFF, IDUM
C
C     External references:
      INTEGER VIMODR, VFWAFF
C
C ----------------------------------------------------------------------
C
C     Calculate IMOD; let VIMODR check the validity of IVIEW and IROM:
C
      IRET = VIMODR(IVIEW,IROM,IMOD)
C
      IF (IRET .EQ. VDERR) THEN
        VDSCRS = VDERR
C
      ELSEIF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
C     Check the validity of ISCH:
        IF ((ISCH .LT. 1) .OR. (ISCH .GT. NZSROM)) THEN
          VDSCRS = VDERR
C
        ELSEIF (LZMULT) THEN
C
C     Multiplexing in the z readout--one ROM per module.
C
          IWAF1 = (ISCH-1)/NRDSTZ + 1
          IROS1 = ISCH - (IWAF1 - 1)*NRDSTZ
          VDSCRS = VDOK
C
        ELSE
C
C     No multiplexing--one ROM per wafer.
C
          IWFF = IROM
          IRET = VFWAFF(IWFF,IDUM,IWAF1)
          IROS1 = ISCH
          VDSCRS = VDOK
C
        ENDIF
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
C
C     Check the validity of ISCH:
        IF ((ISCH .LT. 1) .OR. (ISCH .GT. NPSROM)) THEN
          VDSCRS = VDERR
C
        ELSE
C
C     The r-phi view is always multiplexed (in a trivial way).
C     The strip channel number is equal to IROS, independent of
C     IWAF.
C
          IWAF1 = 1
          IROS1 = ISCH
          VDSCRS = VDOK
C
        ENDIF
C
C-----------------------------------------------------------------------
C
C     Invalid view:
C
      ELSE
C
        VDSCRS = VDERR
C
      ENDIF
C
      RETURN
      END
