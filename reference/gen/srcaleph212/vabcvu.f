      INTEGER FUNCTION VABCVU (ABC,JWAF,VUW)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM / USER
C!  Transform local wafer coords (a,b,c) into wafer coords (v,u,w)
C - Joe Rothberg, 15 January 1994
C
C - Input:
C   ABC(3) / R  Coordinates of point in local wafer system
C   JWAF   / I  Global wafer index
C
C - Output:
C   VABCVU / I  = VDOK if successful
C               = VDERR if error occurred
C   VUW(3) / R  Coordinates of point in wafer system
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
C!    Common for VDLA data: VDET layers
C ----------------------------------------------------------------------
      INTEGER IORIEN
      REAL RWVDLA, WATILT
C
      COMMON / VDLACO / RWVDLA(NVLAYR), WATILT(NVLAYR), IORIEN(NVLAYR)
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
      REAL VUW(3)
      REAL ABC(3)
      INTEGER JWAF
C
C     Local variables
C
      INTEGER JMOD,MMOD,JLAY
      INTEGER MODSN, IORSN
      INTEGER VJLAYW, VJMODW, VMMODJ
C
C ----------------------------------------------------------------------
C check validity of arguments
C
      IF (JWAF .GE. 1 .AND. JWAF .LE. NWAFER) THEN
C
        JLAY = VJLAYW(JWAF)
        JMOD = VJMODW(JWAF)
        MMOD = VMMODJ(JMOD)
C
C sign of z (module)
        MODSN = ISIGN(1,MMOD)
C
C orientation sign, r-phi outward is +1,  r-phi inward is -1
        IORSN = 2*IORIEN(JLAY) - 3
C
C transform
        VUW(3) = MODSN*ABC(1)
C
        VUW(1) = IORSN*ABC(3)
C
        VUW(2) = -MODSN*IORSN*ABC(2)
C
        VABCVU = VDOK
C
      ELSE
C
C     argument JWAF out of range
C
        CALL VZERO(VUW,3)
        VABCVU = VDERR
C
      ENDIF
C
      RETURN
      END
