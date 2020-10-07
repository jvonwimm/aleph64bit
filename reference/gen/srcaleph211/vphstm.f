      INTEGER FUNCTION VPHSTM (IVIEW,NPHSTR,PPITCH)
C ----------------------------------------------------------------------
CKEY VDETDES STRIP / USER
C!  Returns the number and pitch of the physical strips on a wafer
C - Steve Wasserbaech, February 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VPHSTM / I  = VDOK if successful;
C               = VDERR if IVIEW is invalid.
C   NPHSTR / I  Number of physical strips
C   PPITCH / R  Physical strip pitch (cm)
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
C     Arguments:
      INTEGER IVIEW, NPHSTR
      REAL PPITCH
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
        NPHSTR = NZSTRP
        PPITCH = STPITZ
        VPHSTM = VDOK
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        NPHSTR = NPSTRP
        PPITCH = STPITP
        VPHSTM = VDOK
C
      ELSE
C
C     invalid view:
        NPHSTR = 0
        PPITCH = 0.
        VPHSTM = VDERR
C
      ENDIF
C
      RETURN
      END
