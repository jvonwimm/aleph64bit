      INTEGER FUNCTION VFADIM (DVUW)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns dimensions of a face
C - Steve Wasserbaech, February 1994
C
C - Input:
C   (none)
C
C - Output:
C   VFADIM  / I  = VDOK if successful;
C                = VDERR if an error occurred
C
C   DVUW(3) / R  Dimensions of face (cm) along local
C                v, u, and w directions
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
C
C     Arguments:
      REAL DVUW(3)
C
C ----------------------------------------------------------------------
C
C     Along v and u, we simply need the wafer dimensions:
C
      DVUW(1) = WTHICK
      DVUW(2) = WSIZEB
C
C     Along w, we need the wafer dimension plus the distance
C     between the centers of the first and last wafers in the face:
C
      DVUW(3) = WSIZEA + (WAFERZ(NWAFEF) - WAFERZ(1))
C
      VFADIM = VDOK
C
      RETURN
      END
