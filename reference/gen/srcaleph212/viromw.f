      INTEGER FUNCTION VIROMW (IMOD,IWAF,IVIEW,IROM)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates readout module from module, wafer, and view
C - Steve Wasserbaech, October 1994
C - Input:
C   IMOD   / I  Local module index
C   IWAF   / I  Local wafer index
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VIROMW / I  = VDOK if successful
C               = VDERR if error occurred
C   IROM   / I  Readout module
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
      INTEGER IMOD, IWAF, IVIEW, IROM
C
C     Local variables
      INTEGER IRET, IWFF
C
C     External references:
      INTEGER VIWFFW
C
C ----------------------------------------------------------------------
C
      IF ((IMOD .GE. 1) .AND. (IMOD .LE. NVMODF) .AND.
     >    (IWAF .GE. 1) .AND. (IWAF .LE. NWAFEM)) THEN
C
        IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
          IF (LZMULT) THEN
C
C     multiplexing; one readout module per module:
C
            IF (IMOD .EQ. 1) THEN
              IROM = 1
            ELSE
              IROM = IROMAX
            ENDIF
C
          ELSE
C
C     no multiplexing; one readout module per face:
C
            IRET = VIWFFW(IMOD,IWAF,IWFF)
            IROM = IWFF
          ENDIF
C
          VIROMW = VDOK
C
        ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view; one readout module per module:
          IF (IMOD .EQ. 1) THEN
            IROM = 1
          ELSE
            IROM = IROMAX
          ENDIF
          VIROMW = VDOK
C
        ELSE
C
C     invalid view:
          IROM = 0
          VIROMW = VDERR
C
        ENDIF
C
      ELSE
C
        IROM = 0
        VIROMW = VDERR
C
      ENDIF
C
      RETURN
      END
