      INTEGER FUNCTION VWFFND (Z)
C ----------------------------------------------------------------------
CKEY VDETDES WAFER / USER
C!  Find which wafer in a face is nearest to a given point
C - Steve Wasserbaech, April 1994
C
C   Note: this routine relies on the basic assumptions that the
C   wafer centers have the same set of z coordinates in every face,
C   and that the wafers in a particular face have the same geometry.
C   For a particular point in space (x,y,z) and a particular face JFAC,
C   the local wafer-in-face index IWFF of the wafer nearest to the
C   point is independent of JFAC, x, and y under these assumptions.
C
C - Input:
C   Z      / R  z coordinate (cm) of point in ALEPH system
C
C - Output:
C   VWFFND / I  Local wafer-in-face index of wafer
C               centered nearest to Z
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
C
C     Arguments:
      REAL Z
C
C     Local variables
      INTEGER IWFF0
      REAL DZ, DZMIN
C
C ----------------------------------------------------------------------
C
C     Loop over IWFF to find the wafer whose center lies closest
C     to the input value of Z:
C
      VWFFND = 1
      DZMIN = ABS(Z - WAFERZ(1))
      DO IWFF0=2,NWAFEF
        DZ = ABS(Z - WAFERZ(IWFF0))
        IF (DZ .LT. DZMIN) THEN
          VWFFND = IWFF0
          DZMIN = DZ
        ENDIF
      ENDDO
C
      RETURN
      END
