      INTEGER FUNCTION VSMMTX (TXTM,ISMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Returns serial number of module text name, VDET95
C
C - Joe Rothberg, August 1995
C
C - Input:
C   TXTM   / I  module text name
C
C - Output:
C   ISMOD   / I  Serial number of Module
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
C ---------------------------------------------------------------------
C Arguments
      INTEGER  ISMOD
      CHARACTER*4 TXTM
C Local variables
      INTEGER IRET, ILAY, IFAC, IMOD, IFACS, JMOD, JFAC
C Functions
      INTEGER VITEXI, VJMODI, VJFACM, VFACSN
C ------------------------------------------------------
      VSMMTX = VDERR
C
      IRET = VITEXI(TXTM,ILAY,IFAC,IMOD)
          IF(IRET .EQ. VDOK) THEN
             IRET = VJMODI(ILAY,IFAC,IMOD,JMOD)
             JFAC = VJFACM(JMOD)
C find face serial number
             IFACS = VFACSN(JFAC)
C
             IF (IFACS .GE. 1 .AND. IFACS .LE. maxface) THEN
                IF(IMOD .EQ. 2)   ISMOD = MODPOS(IFACS)
                IF(IMOD .EQ. 1)   ISMOD = MODNEG(IFACS)
                VSMMTX = VDOK
             ENDIF
           ENDIF
C
      RETURN
      END
