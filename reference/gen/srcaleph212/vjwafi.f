      INTEGER FUNCTION VJWAFI (ILAY,IFAC,IMOD,IWAF,JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Global wafer index from the local layer, face, module and wafer
C - Joe Rothberg, December 1993
C
C - Input:
C   IWAF   / I  Local wafer index
C   ILAY   / I  Local layer index
C   IMOD   / I  Local module index
C   IWAF   / I  Local wafer index
C
C - Output:
C   VJWAFI / I  = VDOK if successful
C               = VDERR if error occurred
C   JWAF   / I  Global wafer index
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
C!    Common for lookup tables for index manipulation
C ----------------------------------------------------------------------
      INTEGER IVSTUP, NMODUL, NWAFER, NFACEL, NWAFEM, JJFACM, JJMODW
      INTEGER JIFACF, JIMODM, JIWAFW, IJFACE, IJMODU, IJWAFR, JIWFFW
      INTEGER IJWFFR
      CHARACTER*4 TXMODU
C
      COMMON / VGINDX / IVSTUP, NMODUL, NWAFER, NFACEL(NVLAYR),
     >                  NWAFEM, JJFACM(NVMMAX), JJMODW(NVWMAX),
     >                  JIFACF(NVFMAX), JIMODM(NVMMAX),
     >                  JIWAFW(NVWMAX), IJFACE(NVLAYR,NVFLMX),
     >                  IJMODU(NVLAYR,NVFLMX,NVMODF),
     >                  IJWAFR(NVLAYR,NVFLMX,NVMODF,NVWMMX),
     >                  JIWFFW(NVWMAX), IJWFFR(NVLAYR,NVFLMX,NVWFMX),
     >                  TXMODU(NVLAYR,NVFMAX,NVMODF)
C
C
      INTEGER JWAF,ILAY,IFAC, IMOD,IWAF
C
C  functions:
      INTEGER VJMODI
C
C     Local variables
C     JMOD  global face index
C
      INTEGER JMOD
C
C ----------------------------------------------------------------------
C
      IF ((VJMODI(ILAY,IFAC,IMOD,JMOD).EQ.VDOK).AND.
     >     (IWAF.GE.1).AND.(IWAF.LE.NWAFEM))  THEN
C
        JWAF = IJWAFR(ILAY,IFAC,IMOD,IWAF)
        VJWAFI = VDOK
      ELSE
        VJWAFI = VDERR
      ENDIF
C
      RETURN
      END
