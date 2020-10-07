      INTEGER FUNCTION VDSCEC (IVIEW,ISCH,IECH)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Calculate electronics channel number from strip channel number
C - Steve Wasserbaech, November 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   ISCH   / I  Strip channel number
C
C - Output:
C   VDSCEC / I  = VDOK if successful
C               = VDERR if error occurred
C   IECH   / I  Electronics channel number
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
      INTEGER IVIEW, ISCH, IECH
C
C     Local variables
      INTEGER NSROM, IECOR
C
C ----------------------------------------------------------------------
C
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
C
C     invalid view:
        IECH = 0
        VDSCEC = VDERR
C
      ELSE
C
        IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
          NSROM = NZSROM
          IECOR = IECORZ
C
        ELSE
C
C     r-phi view:
          NSROM = NPSROM
          IECOR = IECORP
C
        ENDIF
C
C     Check the validity of ISCH:
C
        IF ((ISCH .LT. 1) .OR. (ISCH .GT. NSROM)) THEN
          IECH = 0
          VDSCEC = VDERR
C
        ELSE
C
C     The numbering of electronics channels is flipped if IECOR = -1:
C
          IF (IECOR .EQ. 1) THEN
            IECH = ISCH
          ELSE
            IECH = NSROM + 1 - ISCH
          ENDIF
C
C     Shift IECH down by one so the electronics channel numbers
C     start with zero:
          IECH = IECH - 1
          VDSCEC = VDOK
C
        ENDIF
C
      ENDIF
C
      RETURN
      END
