      INTEGER FUNCTION VRSWMX (IROS,IWAF,IVIEW,IDATC,IMX)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the MX7 chip number and data channel
C!                    for a given strip, wafer, view. VDET95
C - Joe Rothberg, August 1995
C
C - Input:
C   IROS   / I  Readout strip number
C   IWAF   / I  Local wafer index
C   IVIEW  / I  View
C
C - Output:
C   IMX   / I  MX7 chip number
C   IDATC / I  Data channel number
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
C
C     Arguments:
      INTEGER IROS, IWAF, IVIEW, IDATC, IMX
C
C     Local variables
      INTEGER IRET, IROM, ISCH, IECH, IMOD
C
C     External references:
      INTEGER VDRSSC, VDSCEC
C
C ----------------------------------------------------------------------
C
      VRSWMX = VDERR
C
C readout strip to strip channel
             IMOD = 1
             IRET = VDRSSC(IMOD,IWAF,IVIEW,IROS,IROM,ISCH)
             IF( IRET .NE. VDERR) THEN
C strip channel to electronics channel
                IRET = VDSCEC(IVIEW,ISCH, IECH)
                  IF(IRET .NE. VDERR ) THEN

C electronics channels to data channels
                     IDATC = IECH + VUECH(IVIEW) +VEXCH(IVIEW)
C data channels to MX chips
                     IMX    = (IDATC-VEXCH(IVIEW))/128 + 1
                     VRSWMX = VDOK
                  ENDIF
            ENDIF
C
      RETURN
      END
