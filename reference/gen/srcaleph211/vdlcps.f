      INTEGER FUNCTION VDLCPS (XCOOR,IVIEW,PSTRP)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM STRIP / USER
C!  Local coordinate to physical strip.
C - Joe Rothberg, 10 February 1994
C      Returns physical strip number,
C      given position in local wafer coordinates and view number.
C
C
C - Input:
C   XCOOR  / R  Position in local wafer coordinates
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VDLCPS / I  = VDOK if successful
C               = VDERR if error occurred
C   PSTRP  / R  physical strip number (floating number)
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
C!    Common for VWGE data: Wafer geometry
C ----------------------------------------------------------------------
      INTEGER NZSTRP, NPSTRP
      REAL WSIZEA, WSIZEB, STPITZ, STPITP, STLENZ, STLENP
      REAL AMNSRZ, AMNSRP, BMNSRZ, BMNSRP, WTHICK
C
      COMMON / VWGECO / WSIZEA, WSIZEB, NZSTRP, NPSTRP, STPITZ, STPITP,
     >                  STLENZ, STLENP, AMNSRZ, AMNSRP, BMNSRZ, BMNSRP,
     >                  WTHICK
C
C
      INTEGER IVIEW
      REAL XCOOR, PSTRP
C
C     Local variables
C
C     PITCH       pitch
C     NSTRP       number of strips
C     ALEDG       distance from low edge of active region to center
C     GCEDG       distance from coordinate to low edge active region
C     IGMX        geometry index
C     JFAC        global face index
C
C
      REAL PITCH, ALEDG, GCEDG
      INTEGER NSTRP
C
C ----------------------------------------------------------------------
C
      IF ((IVIEW .EQ. VVIEWZ) .OR. (IVIEW .EQ. VVIEWP)) THEN
C
        IF (IVIEW .EQ. VVIEWZ) THEN
C     z-side
          PITCH = STPITZ
          NSTRP = NZSTRP
          ALEDG = AMNSRZ
        ELSEIF (IVIEW .EQ. VVIEWP) THEN
C     rphi-side
          PITCH = STPITP
          NSTRP = NPSTRP
          ALEDG = BMNSRP
        ENDIF
C ----------------------------------------------------------------------
C  distance from given coordinate to  low edge of active region
        GCEDG = XCOOR - ALEDG
C
C  find  real(floating) strip number for given coordinate
        PSTRP = GCEDG/PITCH + 0.5
C
C  the given position is outside active region of wafer
        IF (PSTRP .GT. FLOAT(NSTRP)+0.5 .OR. PSTRP .LT. 0.5) THEN
          VDLCPS  = VDERR
          PSTRP = 0.
        ELSE
          VDLCPS  = VDOK
        ENDIF
C ----------------------------------------------------------------------
      ELSE
C     invalid input arguments
        PSTRP = 0.
        VDLCPS = VDERR
      ENDIF
C
      RETURN
      END
