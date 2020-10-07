      INTEGER FUNCTION VDCWRS (IDATC,IVIEW,IWFRS,ISTRS)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the strips and wafers for given data channel and view.
C!  returns ALL wafers and readout strips. (3 for rphi; 2 for z). VDET95
C - Joe Rothberg, August 1995
C
C - Input:
C   IDATC  / I  Data Channel number
C   IVIEW  / I  View
C
C - Output:
C   IWFRS(3)   / I  Local wafer index, (2 for z or 3 for rphi)
C   ISTRS(3)   / I  Readout Strips (2 for z or 3 for rphi)
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
      INTEGER  IWFRS(3), ISTRS(3), IVIEW, IDATC
C
C     Local variables
      INTEGER IRET, IWAF, IROS, IWAF2, IROS2, I
C
C     External references:
      INTEGER  VDACRS, VZRSRS
C ----------------------------------------------------------------------
C
      VDCWRS = VDERR
C
      DO I=1,3
       IWFRS(I) = -1
       ISTRS(I) = -1
      ENDDO
C
      IF(IVIEW .EQ. 1 .OR. IVIEW .EQ. 2) THEN
C first wafer
         IRET = VDACRS (IDATC,IVIEW,IWAF,IROS)
         IF(IRET .EQ. VDOK) THEN
C find partners
            IF(IVIEW .EQ. VVIEWP) THEN
               DO I = 1,3
                  IWFRS(I) = I
                  ISTRS(I) = IROS
               ENDDO
               VDCWRS = VDOK
            ELSEIF (IVIEW .EQ. VVIEWZ) THEN
               IRET =  VZRSRS(IWAF,IROS,IWAF2,IROS2)
                  IF(IRET .EQ. VDOK) THEN
                     IWFRS(1) = IWAF
                     ISTRS(1) = IROS
                     IWFRS(2) = IWAF2
                     ISTRS(2) = IROS2
                     VDCWRS = VDOK
                  ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      RETURN
      END
