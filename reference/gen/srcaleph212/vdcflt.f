      INTEGER FUNCTION VDCFLT(IDATC,IVIEW,ISTRS,IFLTC)
C
CKEY VDETDES INDEX / USER
C!  Returns strips, fault code
C - Joe Rothberg, June 1995
C
C - Input:
C   IDATC   / I  Data Channel
C   IVIEW   / I  View
C
C
C - Output:
C   ISTRS(3) / I  strips
C   IFLTC(3) / I  fault code by wafer
C ----------------------------------------------------------------------
      IMPLICIT NONE
C ----------------------------------------------------------------------
C! VDET Electronics channels arrays
C
C  electronics channels; 3 wafers; 3 wafer flags
C  index is electronics channel starting from 1
      INTEGER EFLAG
      PARAMETER(EFLAG=7)
      INTEGER IELCHP(1024,eflag)
      INTEGER IELCHZ(1024,eflag)
      COMMON/VELCHN/IELCHP, IELCHZ
C ------------------------------------------------
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
C --------------------------------------------------------
C   Arguments:
      INTEGER  IWFRS(3),ISTRS(3),IFLTC(3), MMOD, IVIEW, IDATC
C
      INTEGER I
C ---------------------------------------------------------
      IF(IDATC .LE. 0 .OR. IDATC .GT. 1023) THEN
         VDCFLT = VDERR
         DO I =1,3
           ISTRS(I) = -1
           IFLTC(I) = -1
         ENDDO
         GOTO 999
      ENDIF
C ----------------------------------------------------------
      VDCFLT = VDOK
C
      IF(IVIEW .EQ. vviewz) THEN
        DO I=1,3
          ISTRS(I) =  IELCHZ(IDATC,I)
          IFLTC(I) =  IELCHZ(IDATC,I+3)
        ENDDO
C
C correct r phi view for readout order when finding strips (temporary me
      ELSEIF(IVIEW .EQ. vviewp) THEN
        DO I=1,3
          ISTRS(I) = 1022 - IELCHP(IDATC,I)
          IFLTC(I) =  IELCHP(IDATC,I+3)
        ENDDO
C
      ELSE
C
        VDCFLT = VDERR
C
      ENDIF
C
 999  RETURN
      END
