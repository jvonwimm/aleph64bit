      INTEGER FUNCTION VITEXI (TXTM,ILAY,IFAC,IMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates local layer, face, and module indices from text module
C - Joe Rothberg, March 1994
C
C - Input:
C   TXTM   / C*4  Text module name
C
C - Output:
C   VITEXI / I  = VDOK if successful
C               = VDERR if error occurred
C   IFAC   / I    Local face index
C   ILAY   / I    Local layer index
C   IMOD   / I    Local module index
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
      INTEGER ILAY,IFAC, IMOD
      CHARACTER*4 TXTM
C
C local variables
      CHARACTER*2 CHFAC
      CHARACTER*1 CHMOD, CHLAY
C
C ----------------------------------------------------------------------
C
      VITEXI = VDERR
C
      CHMOD = TXTM(1:1)
      CHLAY = TXTM(2:2)
      CHFAC = TXTM(3:4)
C
      IMOD = 0
      ILAY = 0
      IFAC = 0
      READ(CHFAC,'(I2)',ERR=990)IFAC
      IF (CHMOD .EQ. 'B') IMOD = 1
      IF (CHMOD .EQ. 'A') IMOD = 2
      IF (CHLAY .EQ. 'I') ILAY = 1
      IF (CHLAY .EQ. 'O') ILAY = 2
C
      IF (ILAY .GT. 0 .AND. IMOD .GT. 0) THEN
        IF (IFAC .GT. 0 .AND. IFAC .LE. NFACEL(ILAY)) VITEXI=VDOK
      ENDIF
C
      RETURN
C ----------------------------------------------------------------------
 990  CONTINUE
      RETURN
      END
