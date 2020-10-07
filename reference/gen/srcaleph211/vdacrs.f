      INTEGER FUNCTION VDACRS (IDATC,IVIEW,IWAF,IROS)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the strip, wafer for given data channel and view.
C!      returns one wafer and readout strip even if multiplexed. VDET95
C - Joe Rothberg, August 1995
C
C - Input:
C   IDATC  / I  Data Channel number
C   IVIEW  / I  View
C
C - Output:
C   IWAF   / I  Local wafer index
C   IROS   / I  Readout Strip number
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C ----------------------------------------------------------------------
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
C ----------------------------------------------------------------------
C     Arguments:
      INTEGER IROS, IWAF, IVIEW, IDATC
C
C     Local variables
      INTEGER IRET, IROM, ISCH, IECH, IMOD
C
C     External references:
      INTEGER VDSCRS, VDECSC
C ----------------------------------------------------------------------
C
      VDACRS = VDERR
C
C Data channel to electronics channel
C       both start from zero
      IF(IVIEW .EQ. 1 .OR. IVIEW .EQ. 2) THEN
         IECH  = IDATC - VUECH(IVIEW) -VEXCH(IVIEW)
C Electronics channel to strip channel
C   vdecsc checks for negative electronics channels
         IRET = VDECSC(IVIEW,IECH,ISCH)
         IF(IRET .EQ. VDOK) THEN
C strip channel to Readout Strip
            IROM = 1
            IRET = VDSCRS(IVIEW,IROM,ISCH,IMOD,IWAF,IROS)
            IF(IRET .EQ. VDOK) THEN
               VDACRS = VDOK
            ENDIF
         ENDIF
      ENDIF
C
      RETURN
      END
