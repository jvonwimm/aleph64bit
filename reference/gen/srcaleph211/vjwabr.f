      INTEGER FUNCTION VJWABR (NBROM, Z, JWAF, NBWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Convert decimal encoded readout module to global wafer index
C - Manoj Thulasidas, 5 November 1994
C - Modified: Steve Wasserbaech, 29 March 1995
C
C   This function converts a decimal encoded readout module address
C   (as produced by VAENWA) to a global wafer index JWAF and the
C   corresponding decimal encoded wafer address.  The layer and
C   face are taken from NBROM.  The wafer-in-face index is calculated
C   from the Z coordinate of the hit.  It is the user's responsibility
C   to ensure that NBROM and Z are consistent.
C
C - Input:
C   NBROM  / I  Decimal encoded readout module address
C   Z      / R  z coordinate of hit (cm)
C
C - Output:
C   VJWABR / I  = VDOK if successful
C               = VDERR if error occurred
C   JWAF   / I  Global wafer index
C   NBWAF  / I  Decimal encoded wafer address
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
      INTEGER NBROM, JWAF, NBWAF
      REAL Z
C
C     Local variables
C
      INTEGER IRET, ILAY, IROM, IFAC, IVIEW, IWFF
      INTEGER VWFFND, VJWFFW
C
C ----------------------------------------------------------------------
C
      JWAF = 0
      NBWAF = -1
      VJWABR = VDERR
C
C     Decode the readout module address:
C
      CALL VADEWA(NBROM,ILAY,IROM,IFAC,IVIEW)
C
C     Find the wafer-in-face nearest to Z:
C
      IWFF = VWFFND(Z)
C
C     Get the global wafer index:
C
      IRET = VJWFFW(ILAY,IFAC,IWFF,JWAF)
C
C     Encode the wafer address:
C
      IF (IRET .EQ. VDOK) THEN
        CALL VAENWA(NBWAF,ILAY,IWFF,IFAC,IVIEW)
        VJWABR = VDOK
      ENDIF
C
      RETURN
      END
