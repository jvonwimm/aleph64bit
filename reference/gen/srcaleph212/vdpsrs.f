      INTEGER FUNCTION VDPSRS (PSTRP,IVIEW,RSTRP)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Physical strip number to readout strip number.
C - Joe Rothberg, February 1994
C      Returns readout strip number,
C      given physical strip number, view, and global wafer index
C
C - Input:
C   PSTRP  / R  physical strip (floating number)
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VDPSRS / I  = VDOK if successful
C               = VDERR if error occurred
C   RSTRP  / R  readout strip number (floating number)
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
C     NSTRR       number of readout strips
C     NOFFS       offset of 1st readout strip from 1st physical strip
C
      INTEGER NRDFQ, NSTRR, NOFFS
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
        VDPSRS = VDERR
      ELSE
C
        IF (IVIEW .EQ. VVIEWZ) THEN
C  z-side
          NRDFQ = NREFRZ
          NSTRR = NRDSTZ
          NOFFS = NOFRDZ
        ELSE
C  rphi-side
          NRDFQ = NREFRP
          NSTRR = NRDSTP
          NOFFS = NOFRDP
        ENDIF
C ----------------------------------------------------------------------
C  physical strip to readout strip
        RSTRP = (PSTRP - 1. - NOFFS)/FLOAT(NRDFQ) + 1.
C
        IF ((RSTRP .LT. 0.5) .OR. (RSTRP .GT. FLOAT(NSTRR)+0.5)) THEN
          VDPSRS = VDERR
        ELSE
          VDPSRS  = VDOK
        ENDIF
C ----------------------------------------------------------------------
C  valid input arguments
      ENDIF
C
      RETURN
      END
