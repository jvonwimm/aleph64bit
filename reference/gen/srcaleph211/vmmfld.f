      REAL FUNCTION VMMFLD (MMOD)
C ----------------------------------------------------------------------
CKEY VDETDES FIELD / USER
C!  Return magnetic field in local (a,b,c) coordinates
C - Joe Rothberg, 15 January 1994
C
C - Input:
C   MMOD   / I  Signed global module index
C
C - Output:
C   VMMFLD / R  Component of magnetic field along +a direction (Tesla)
C               = 0 if error
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
C     Functions:
      REAL ALFIEL
C
C     Local variables
      INTEGER MMOD
      INTEGER MODSN
      REAL FIELD
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF (MMOD.GE.-NSLOTS .AND. MMOD.LE.NSLOTS .AND. MMOD.NE.0) THEN
C
C sign of z (module)
        MODSN = ISIGN(1,MMOD)
C
C temporary
        FIELD = ALFIEL(0.)
        VMMFLD = MODSN*FIELD
C
      ELSE
C
C     argument JWAF out of range
C
        VMMFLD = 0.
C
      ENDIF
C
      RETURN
      END
