      INTEGER FUNCTION VIMODM (MMOD,ILAY,IFAC)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the layer and local face for a given mglobal module
C - Joe Rothberg, January 1994
C
C - Input:
C   MMOD   / I  Signed global module index
C
C - Output:
C   VIMODM / I  = VDOK if successful
C               = VDERR if error occurred
C   ILAY   / I  Local layer index
C   IFAC   / I  Local face index
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
C
      INTEGER MMOD, ILAY, IFAC
C
C local variables
C
      INTEGER JMOD, IMOD, ISTATUS
C functions
      INTEGER VJMODM, VIMODI
C
C ----------------------------------------------------------------------
C
      VIMODM = VDERR
      IF ((MMOD.GE.-NSLOTS).AND.(MMOD.LE.NSLOTS).AND.(MMOD.NE.0)) THEN
C
        JMOD = VJMODM(MMOD)
        ISTATUS = VIMODI(JMOD,ILAY,IFAC,IMOD)
C
        VIMODM = VDOK
      ENDIF
C
      RETURN
      END
