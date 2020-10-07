      INTEGER FUNCTION VZRSRS (IWAF1,IROS1,IWAF2,IROS2)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Find partner of readout strip in multiplexed z strip channel
C - Steve Wasserbaech, November 1994
C
C - Input:
C   IWAF1  / I  Local wafer index of a z strip
C   IROS1  / I  Readout strip number of a z strip
C
C - Output:
C   VZRSRS / I  = VDOK if successful
C               = VDERR if error occurred or if readout not multiplexed
C   IWAF2  / I  Local wafer index of partner readout strip
C               in strip channel
C   IROS2  / I  Readout strip number of partner readout strip
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
C!    Common for lookup tables for index manipulation
C ----------------------------------------------------------------------
      INTEGER IVSTUP, NMODUL, NWAFER, NFACEL, NWAFEM, JJFACM, JJMODW
      INTEGER JIFACF, JIMODM, JIWAFW, IJFACE, IJMODU, IJWAFR, JIWFFW
      INTEGER IJWFFR
      CHARACTER*4 TXMODU
C
      COMMON / VGINDX / IVSTUP, NMODUL, NWAFER, NFACEL(NVLAYR),
     >                  NWAFEM, JJFACM(NVMMAX), JJMODW(NVWMAX),
     >                  JIFACF(NVFMAX), JIMODM(NVMMAX),
     >                  JIWAFW(NVWMAX), IJFACE(NVLAYR,NVFLMX),
     >                  IJMODU(NVLAYR,NVFLMX,NVMODF),
     >                  IJWAFR(NVLAYR,NVFLMX,NVMODF,NVWMMX),
     >                  JIWFFW(NVWMAX), IJWFFR(NVLAYR,NVFLMX,NVWFMX),
     >                  TXMODU(NVLAYR,NVFMAX,NVMODF)
C
C!    Common for miscellaneous calculated VDET geometry quantities
C ----------------------------------------------------------------------
      REAL RVDMIN, RVDMAX, ZVDMAX, WAXCEN, WAYCEN, WAZCEN
      REAL WARHOC, WAPHIC, CPHIOF, SPHIOF, TNWTLT, AMXSRZ, AMXSRP
      REAL BMXSRZ, BMXSRP
      INTEGER NZSROM, NPSROM, NZSMOD, NPSMOD, NPRSSC, NZROMM
      INTEGER MSVWAF, MSVNST, ISSLAY, ISSNST
      LOGICAL LZMULT
C
      COMMON / VDETGE / RVDMIN, RVDMAX, ZVDMAX,
     >                  WAXCEN(NVWMAX), WAYCEN(NVWMAX), WAZCEN(NVWMAX),
     >                  WARHOC(NVFMAX), WAPHIC(NVFMAX), CPHIOF(NVFMAX),
     >                  SPHIOF(NVFMAX), TNWTLT(NVLAYR), AMXSRZ, AMXSRP,
     >                  BMXSRZ, BMXSRP, NZSROM, NPSROM, NZSMOD, NPSMOD,
     >                  NPRSSC, NZROMM, LZMULT, MSVWAF, MSVNST, ISSLAY,
     >                  ISSNST
C
C
C     Arguments:
      INTEGER IWAF1, IROS1, IWAF2, IROS2
C
C     Local variables
      INTEGER IROSM1, IROSM2
C
C ----------------------------------------------------------------------
C
C     Make sure the z readout is multiplexed:
C
      IF (.NOT. LZMULT) THEN
        IWAF2 = 0
        IROS2 = 0
        VZRSRS = VDERR
C
C     Check the validity of IWAF1 and IROS1:
C
      ELSEIF ((IWAF1 .LT. 1) .OR. (IWAF1 .GT. NWAFEM) .OR.
     >        (IROS1 .LT. 1) .OR. (IROS1 .GT. NRDSTZ)) THEN
        IWAF2 = 0
        IROS2 = 0
        VZRSRS = VDERR
C
      ELSE
C
C     Compute the readout-strip-in-module index IROSM1, which runs
C     from 1 to 2*NZSROM:
C
        IROSM1 = NRDSTZ*(IWAF1-1) + IROS1
C
C     Count up or down NZSROM readout strips to find the partner:
C
        IF (IROSM1 .LE. NZSROM) THEN
          IROSM2 = IROSM1 + NZSROM
        ELSE
          IROSM2 = IROSM1 - NZSROM
        ENDIF
C
C     Convert IROSM2 to wafer and readout strip:
C
        IWAF2 = (IROSM2-1)/NRDSTZ + 1
        IROS2 = IROSM2 - (IWAF2 - 1)*NRDSTZ
        VZRSRS = VDOK
C
      ENDIF
C
      RETURN
      END
