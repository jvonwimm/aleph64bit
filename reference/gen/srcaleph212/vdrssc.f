      INTEGER FUNCTION VDRSSC (IMOD,IWAF,IVIEW,IROS,IROM,ISCH)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Convert readout strip number to strip channel number
C - Steve Wasserbaech, October 1994
C
C - Input:
C   IMOD   / I  Local module index
C   IWAF   / I  Local wafer index
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   IROS   / I  Readout strip number
C
C - Output:
C   VDRSSC / I  = VDOK if successful
C               = VDERR if error occurred
C   IROM   / I  Readout module
C   ISCH   / I  Strip channel number
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
      INTEGER IMOD, IWAF, IVIEW, IROS, IROM, ISCH
C
C     Local variables:
      INTEGER IRET, IROSIM
C
C     External references:
      INTEGER VIROMW
C
C-----------------------------------------------------------------------
C
C     Calculate IROM; let VIROMW check the validity of IMOD, IWAF,
C     and IVIEW:
C
      IRET = VIROMW(IMOD,IWAF,IVIEW,IROM)
C
      IF (IRET .EQ. VDERR) THEN
        VDRSSC = VDERR
C
      ELSEIF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
C     Check the validity of the readout strip number:
C
        IF ((IROS .LT. 1) .OR. (IROS .GT. NRDSTZ)) THEN
          VDRSSC = VDERR
C
        ELSE
          IF (LZMULT) THEN
C
C     Multiplexing in the z readout--one ROM per module.
C     The strip channel number is obtained by converting IWAF,IROS
C     into a "readout-strip-in-module" index IROSIM which runs from
C     1 to 2*NZSROM.  Strip channel I then corresponds to readout-
C     strips-in-module IROSIM = I and I+NZSROM.
C
            IROSIM = NRDSTZ*(IWAF-1) + IROS
            IF (IROSIM .GT. NZSROM) THEN
              ISCH = IROSIM - NZSROM
            ELSE
              ISCH = IROSIM
            ENDIF
            VDRSSC = VDOK
C
          ELSE
C
C     No multiplexing--one ROM per wafer.
C     The strip channel number is equal to the readout strip number:
C
            ISCH = IROS
            VDRSSC = VDOK
          ENDIF
C
        ENDIF
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
C
C     Check the validity of the readout strip number:
C
        IF ((IROS .LT. 1) .OR. (IROS .GT. NRDSTP)) THEN
          VDRSSC = VDERR
C
        ELSE
C
C     The r-phi view is always multiplexed (in a trivial way).
C     The strip channel number is equal to IROS, independent of IWAF.
C
          ISCH = IROS
          VDRSSC = VDOK
C
        ENDIF
C
C-----------------------------------------------------------------------
C
C     Invalid view:
C
      ELSE
        VDRSSC = VDERR
C
      ENDIF
C
      RETURN
      END
