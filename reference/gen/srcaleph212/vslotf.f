      LOGICAL FUNCTION VSLOTF (JFAC)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Determine whether a given slot contains a face
C - Steve Wasserbaech, February 1994
C   Modified 2 June 1995, S. Wasserbaech: ISSFLG = face serial number
C
C - Input:
C   JFAC   / I  Global index of slot
C
C - Output:
C   VSLOTF / L  = .TRUE. if slot contains a face;
C               = .FALSE. if slot is empty, or if JFAC is invalid
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
C     Arguments:
      INTEGER JFAC
C
C ----------------------------------------------------------------------
C
      IF ((JFAC .LT. 1) .OR. (JFAC .GT. NSLOTS)) THEN
C
        VSLOTF = .FALSE.
C
      ELSE
C
        VSLOTF = (ISSFLG(JFAC) .NE. 0)
C
      ENDIF
C
      RETURN
      END
