      INTEGER FUNCTION VSCMSC (MMOD,IVIEW,ISCM,IROM,ISCH)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Convert strip-channel-in-module number to strip channel number
C - Steve Wasserbaech, 4 November 1994
C
C - Input:
C   MMOD   / I  Signed global module index
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   ISCM   / I  Strip-channel-in-module number
C
C - Output:
C   VSCMSC / I  = VDOK if successful
C               = VDERR if error occurred
C   IROM   / I  Readout module
C   ISCH   / I  Strip channel number
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
C!    Common for VSLT data: VDET slots
C ----------------------------------------------------------------------
      INTEGER NSLOTS, JJLAYF, ISSFLG
      REAL PHIOFF
C
      COMMON / VSLTCO / NSLOTS, JJLAYF(NVFMAX), PHIOFF(NVFMAX),
     >                  ISSFLG(NVFMAX)
C
C!    Common for VZPW data: Wafer z positions
C ----------------------------------------------------------------------
      INTEGER NWAFEF
      REAL WAFERZ
C
      COMMON / VZPWCO / NWAFEF, WAFERZ(NVWFMX)
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
      INTEGER MMOD, IVIEW, ISCM, IROM, ISCH
C
C-----------------------------------------------------------------------
C
C     Check the validity of MMOD:
      IF ((MMOD .EQ. 0) .OR. (IABS(MMOD) .GT. NSLOTS)) THEN
        VSCMSC = VDERR
C
      ELSEIF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
C     Check the validity of the strip-channel-in-module number:
C
        IF ((ISCM .LT. 1) .OR. (ISCM .GT. NZSMOD)) THEN
          VSCMSC = VDERR
C
        ELSE
C
          IF (MMOD .LT. 0) THEN
C
C     This is the module at z < 0:
C
            IF (NZROMM .EQ. 1) THEN
              IROM = 1
            ELSE
              IROM = NWAFEM - (ISCM-1)/NZSROM
            ENDIF
C
          ELSE
C
C     This is the module at z > 0:
C
            IF (NZROMM .EQ. 1) THEN
              IF (LZMULT) THEN
                IROM = IROMAX
              ELSE
                IROM = NWAFEF
              ENDIF
            ELSE
              IROM = IROMAX - NWAFEM + 1 + (ISCM-1)/NZSROM
            ENDIF
C
          ENDIF
C
          ISCH = MOD(ISCM-1,NZSROM) + 1
          VSCMSC = VDOK
C
        ENDIF
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
C
C     Check the validity of the strip-channel-in-module number:
C
        IF ((ISCM .LT. 1) .OR. (ISCM .GT. NPSMOD)) THEN
          VSCMSC = VDERR
C
        ELSE
C
          IF (MMOD .LT. 0) THEN
C
C     This is the module at z < 0:
C
            IROM = 1
C
          ELSE
C
C     This is the module at z > 0:
C
            IROM = IROMAX
C
          ENDIF
C
          ISCH = ISCM
          VSCMSC = VDOK
C
        ENDIF
C
      ELSE
C
C     Invalid view:
C
        VSCMSC = VDERR
C
      ENDIF
C
      RETURN
      END
