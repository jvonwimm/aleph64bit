      INTEGER FUNCTION VBRJWA (JWAF, IVIEW, NBROM)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Convert global wafer index JWAF to decimal encoded readout module
C - Manoj Thulasidas, 5 November 1994
C - Modified: Steve Wasserbaech, 29 March 1995
C
C   This function converts a global wafer index JWAF and a view number
C   to the decimal encoded address of the corresponding readout module
C   (encoding scheme as in VAENWA).
C
C - Input:
C   JWAF   / I  Global wafer index
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VBRJWA / I  = VDOK if successful
C               = VDERR if error occurred
C   NBROM  / I  Decimal encoded readout module address
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
      INTEGER JWAF, IVIEW, NBROM
C
C     Local variables
C
      INTEGER IRET, ILAY, IFAC, IMOD, IWAF, IROM
      INTEGER VIWAFI, VIROMW

C ----------------------------------------------------------------------
C
      NBROM = -1
      VBRJWA = VDERR
C
C     Convert to local indices:
C
      IRET = VIWAFI(JWAF,ILAY,IFAC,IMOD,IWAF)
C
C     Find the readout module index:
C
      IF (IRET .EQ. VDOK) THEN
        IRET = VIROMW(IMOD,IWAF,IVIEW,IROM)
C
C     Encode the readout module address.
C     We always use IVIEW = VVIEWZ so that the last digit of
C     NBROM will be zero, for use as a bank number:
C
        IF (IRET .EQ. VDOK) THEN
          CALL VAENWA(NBROM,ILAY,IROM,IFAC,VVIEWZ)
          VBRJWA = VDOK
        ENDIF
      ENDIF
C
      RETURN
      END
