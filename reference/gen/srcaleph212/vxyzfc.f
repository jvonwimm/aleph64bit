      INTEGER FUNCTION VXYZFC (JFAC,XYZ)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns (x,y,z) of face center
C - Steve Wasserbaech, February 1994
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VXYZFC / I  = VDOK if successful;
C               = VDERR if JFAC is invalid.
C   XYZ(3) / R  Coordinates of face center (cm) in ALEPH system;
C               the "face" consists of the NWAFEF wafers
C               and the spaces in between them.
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
C!    Common for VSLT data: VDET slots
C ----------------------------------------------------------------------
      INTEGER NSLOTS, JJLAYF, ISSFLG
      REAL PHIOFF
C
      COMMON / VSLTCO / NSLOTS, JJLAYF(NVFMAX), PHIOFF(NVFMAX),
     >                  ISSFLG(NVFMAX)
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
      INTEGER JFAC
      REAL XYZ(3)
C
C     Local variables
      INTEGER JWF, STATUS
C
C     External references:
      INTEGER VJWAFF
C
C ----------------------------------------------------------------------
C
      IF ((JFAC .LT. 1) .OR. (JFAC .GT. NSLOTS)) THEN
C
        VXYZFC = VDERR
        CALL VZERO(XYZ,3)
C
      ELSE
C
        VXYZFC = VDOK
C
C     Get the global index of the first wafer in the face:
C
        STATUS = VJWAFF(JFAC,1,JWF)
C
C     All wafers in a face have the same (x,y), so we can get
C     the first two components directly from WAXCEN and WAYCEN:
        XYZ(1) = WAXCEN(JWF)
        XYZ(2) = WAYCEN(JWF)
C
C     The face center lies midway between the first and last wafers:
C
        XYZ(3) = (WAFERZ(1) + WAFERZ(NWAFEF)) / 2.
C
      ENDIF
C
      RETURN
      END
