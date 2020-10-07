      INTEGER FUNCTION VROMAX (IVIEW,IROMX,INCR)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns limit and step size for looping over readout modules
C - Steve Wasserbaech, January 1995
C
C   To loop over all readout modules in a face/view, loop from 1 to
C   IROMX with step size INCR.  This handles the peculiar numbering
C   of the readout modules.  (If the view has multiplexing, then each
C   face has two readout modules numbered IROM=1 and IROMAX.)
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VROMAX / I  = VDOK if successful;
C               = VDERR if IVIEW is invalid.
C   IROMX  / I  Maximum readout module index in face IROM
C   INCR   / I  Step size for looping
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
C!    Common for VZPW data: Wafer z positions
C ----------------------------------------------------------------------
      INTEGER NWAFEF
      REAL WAFERZ
C
      COMMON / VZPWCO / NWAFEF, WAFERZ(NVWFMX)
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
      INTEGER IVIEW, IROMX, INCR
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
        IF (LZMULT) THEN
          IROMX = IROMAX
          INCR = IROMAX - 1
        ELSE
          IROMX = NWAFEF
          INCR = 1
        ENDIF
        VROMAX = VDOK
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        IROMX = IROMAX
        INCR = IROMAX - 1
        VROMAX = VDOK
C
      ELSE
C
C     invalid view:
        IROMX = 0
        INCR = 1
        VROMAX = VDERR
C
      ENDIF
C
      RETURN
      END
