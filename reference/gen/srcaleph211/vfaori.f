      INTEGER FUNCTION VFAORI (JFAC,IORI)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns orientation of a face
C - Steve Wasserbaech, February 1994
C
C - Input:
C   JFAC    / I  Global face index
C
C - Output:
C   VFAORI  / I  = VDOK if successful;
C                = VDERR if JFAC is invalid.
C   IORI    / I  Face orientation
C                = 1 if the r-phi side faces inward;
C                = 2 if outward.  (Taken from VDLA.)
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
C!    Common for VDLA data: VDET layers
C ----------------------------------------------------------------------
      INTEGER IORIEN
      REAL RWVDLA, WATILT
C
      COMMON / VDLACO / RWVDLA(NVLAYR), WATILT(NVLAYR), IORIEN(NVLAYR)
C
C
C     Arguments:
      INTEGER JFAC, IORI
C
C ----------------------------------------------------------------------
C
      IF ((JFAC .LT. 1) .OR. (JFAC .GT. NSLOTS)) THEN
C
        VFAORI = VDERR
        IORI = 0
C
      ELSE
C
        VFAORI = VDOK
        IORI = IORIEN(JJLAYF(JFAC))
C
      ENDIF
C
      RETURN
      END
