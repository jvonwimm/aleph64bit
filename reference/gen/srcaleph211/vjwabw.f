      INTEGER FUNCTION VJWABW (NBWAF, JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Convert decimal encoded wafer address to global wafer index JWAF
C - Manoj Thulasidas, 5 November 1994
C
C   This function converts a decimal encoded wafer address (as produced
C   by VAENWA) to the corresponding global wafer index JWAF.
C
C - Input:
C   NBWAF  / I  Decimal encoded wafer address
C
C - Output:
C   VJWABW / I  = VDOK if successful
C               = VDERR if error occurred
C   JWAF   / I  Global wafer index
C ----------------------------------------------------------------------
C      IMPLICIT NONE
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
C
C     Arguments:
C
      INTEGER NBWAF, JWAF
C
C     Local variables
C
      INTEGER IRET, ILAY, IWFF, IFAC, IVIEW, IMOD, IWAF
      INTEGER VFWAFF, VJWAFI
C
C ----------------------------------------------------------------------
C
      JWAF = 0
      VJWABW = VDERR
C
C     Decode the bank number:
C
      CALL VADEWA(NBWAF,ILAY,IWFF,IFAC,IVIEW)
C
C     Get the module and wafer numbers from wafer-in-face number:
C
      IRET = VFWAFF(IWFF,IMOD,IWAF)
C
C     Get the global wafer number:
C
      IF (IRET .EQ. VDOK) VJWABW = VJWAFI(ILAY,IFAC,IMOD,IWAF,JWAF)
C
      RETURN
      END
