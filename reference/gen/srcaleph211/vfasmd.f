      INTEGER FUNCTION VFASMD (JFAC,TXFA,ISMN,ISMP)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C! Returns serial number of modules in global face JFAC and face name.VD
C
C - Joe Rothberg, August 1995
C
C - Input:
C   JFAC   / I  Global Face number
C
C - Output:
C   TXFA   / C*4  face name
C   ISMN   / I  Serial number of Module on side B (negative z)
C   ISMP   / I  Serial number of Module on side A (positive z)
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
      INTEGER JFAC, ISMN, ISMP
      CHARACTER*4 TXFA
C Local variables
      INTEGER IFACS
C Functions
      INTEGER VFACSN
C ------------------------------------------------------
      VFASMD = VDERR
C
      IF(JFAC .GE. 1 .AND. JFAC .LE. NSLOTS) THEN
C
C find face serial number
            IFACS = VFACSN(JFAC)
C
            IF (IFACS .GE. 1 .AND. IFACS .LE. maxface) THEN
                TXFA = FACEC(IFACS)
                ISMN = MODNEG(IFACS)
                ISMP = MODPOS(IFACS)
                VFASMD = VDOK
            ENDIF
      ENDIF
C
      RETURN
      END
