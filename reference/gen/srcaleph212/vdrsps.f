      INTEGER FUNCTION VDRSPS (RSTRP,IVIEW,PSTRP)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Readout strip number to physical strip number.
C - Joe Rothberg, 10 February 1994
C      Returns physical strip number,
C      given readout strip number and view
C
C - Input:
C   RSTRP  / R  readout strip (floating number)
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VDRSPS / I  = VDOK if successful
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
C!    Common for VRDO data: Readout configuration
C ----------------------------------------------------------------------
      INTEGER NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP
      INTEGER NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
      COMMON / VRDOCO / NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP,
     >                  NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
C
      INTEGER IVIEW
      REAL RSTRP, PSTRP
C
C     Local variables
C
C     NRDFQ       Readout strip frequency
C     NSTRP       number of physical strips
C     NSTRR       number of readout strips
C     NOFFS       offset of 1st readout strip from 1st physical strip
C
      INTEGER NRDFQ, NSTRP, NSTRR, NOFFS
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      PSTRP = 0.
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
        VDRSPS = VDERR
      ELSE
C ----------------------------------------------------------------------
        IF (IVIEW .EQ. VVIEWZ) THEN
C  z-side
          NRDFQ = NREFRZ
          NSTRP = NZSTRP
          NSTRR = NRDSTZ
          NOFFS = NOFRDZ
        ELSE
C  rphi-side
          NRDFQ = NREFRP
          NSTRP = NPSTRP
          NSTRR = NRDSTP
          NOFFS = NOFRDP
        ENDIF
C ----------------------------------------------------------------------
C
        IF ((RSTRP .LT. 0.5) .OR. (RSTRP .GT. FLOAT(NSTRR)+0.5)) THEN
          VDRSPS = VDERR
C
        ELSE
C
C  readout strip to physical strip
          PSTRP = (RSTRP - 1.)*FLOAT(NRDFQ) + 1. + NOFFS
          IF ((PSTRP .LT. 0.5) .OR. (PSTRP .GT. FLOAT(NSTRP)+0.5)) THEN
            VDRSPS = VDERR
          ELSE
            VDRSPS  = VDOK
          ENDIF
        ENDIF
C ----------------------------------------------------------------------
C  valid input arguments
      ENDIF
C
      RETURN
      END
