      INTEGER FUNCTION VPHIFC (JFAC,PHI)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns phi of face normal (along +c direction)
C - Steve Wasserbaech, September 1994
C
C   Note: no error condition is returned if the slot JFAC is empty.
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VPHIFC / I  = VDOK if successful;
C               = VDERR if JFAC is invalid.
C   PHI    / R  Phi of face normal in the +c direction (radians),
C               calculated from VSLT bank
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
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
      INTEGER JFAC
      REAL PHI
C
C     Local variables
      INTEGER JLAY
C
C ----------------------------------------------------------------------
C
      IF ((JFAC .LT. 1) .OR. (JFAC .GT. NSLOTS)) THEN
C
        VPHIFC = VDERR
        PHI = 0.
C
      ELSE
C
        VPHIFC = VDOK
        JLAY = JJLAYF(JFAC)
        IF (IORIEN(JLAY) .EQ. 1) THEN
          PHI = PHIOFF(JFAC) + PI
          IF (PHI .GT. TWOPI) PHI = PHI - TWOPI
        ELSE
          PHI = PHIOFF(JFAC)
        ENDIF
C
      ENDIF
C
      RETURN
      END
