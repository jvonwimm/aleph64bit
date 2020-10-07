      INTEGER FUNCTION VMODMC(MCODE)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Returns global module number for mcode
C
C - Joe Rothberg, August 1995
C
C - Input:
C   MCODE   / I  module mcode
C
C - Output:
C           returns global module number, JMOD
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C ----------------------------------------------------------------------
C!   VDET Unconnected, extra channels; Face-module content
C ------------------------------------------------------------
      INTEGER VUECH, VEXCH, VIGBM
      INTEGER MAXFACE
      PARAMETER(MAXFACE=40)
      CHARACTER*4 FACEC
      INTEGER FACEN,MODNEG,MODPOS
c
      COMMON/VDUEFC/VUECH(2),VEXCH(2),VIGBM,
     >      FACEN(MAXFACE),FACEC(MAXFACE),
     >      MODNEG(MAXFACE),MODPOS(MAXFACE)
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
C!  VDET mcode/global module number conversion
C ----------------------------------------------------
      INTEGER MAX_MCODE
      PARAMETER( MAX_MCODE=126 )
      INTEGER MAX_MODULES
      PARAMETER( MAX_MODULES = 48 )
      INTEGER JMOD_FROM_MCODE(0:MAX_MCODE)
      DATA JMOD_FROM_MCODE/
     +                              1, 3, 5, 7, 9,11,13,15,17,-1,
     +                             -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     +                             -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     +                             -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     +                             -1,-1,-1,-1,-1,-1,-1,-1, 2, 4,
     +                              6, 8,10,12,14,16,18,-1,-1,-1,
     +                             -1,-1,-1,-1,19,21,23,25,27,29,
     +                             31,33,35,37,39,41,43,45,47,-1,
     +                             -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     +                             -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     +                             -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     +                             -1,-1,20,22,24,26,28,30,32,34,
     +                             36,38,40,42,44,46,48/
      INTEGER MCODE_FROM_JMOD(MAX_MODULES)
      DATA MCODE_FROM_JMOD/
     +                                   0, 48,  1, 49,  2, 50,
     +                                   3, 51,  4, 52,  5, 53,
     +                                   6, 54,  7, 55,  8, 56,
     +                                  64,112, 65,113, 66,114,
     +                                  67,115, 68,116, 69,117,
     +                                  70,118, 71,119, 72,120,
     +                                  73,121, 74,122, 75,123,
     +                                  76,124, 77,125, 78,126/
C -------------------------------------------------------------
C ---------------------------------------------------------------------
C Arguments
      INTEGER MCODE
C Local variables
C Functions
C ------------------------------------------------------
      VMODMC = VDERR
C
      IF(MCODE .GE. 1 .AND. MCODE .LE. max_MCODE) THEN
C
        VMODMC = JMOD_FROM_MCODE(MCODE)
C
      ENDIF
C
      RETURN
      END
