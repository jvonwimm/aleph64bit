      INTEGER FUNCTION VXYZVU (XYZ,JWAF,VUW)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM / USER
C!  Transform ALEPH coordinates into wafer coordinates
C - Joe Rothberg and Rainer Wallny, 15 January 1994
C
C - Input:
C   XYZ(3) / R  Coordinates of point in ALEPH system
C   JWAF   / I  Global wafer index
C
C - Output:
C   VXYZVU / I  = VDOK if successful
C               = VDERR if error occurred
C   VUW(3) / R  Coordinates of point in wafer system
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
      REAL VUW(3)
      REAL XYZ(3)
      INTEGER JWAF
C
C     Local variables
      INTEGER JFAC
      INTEGER VJFACW
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF (JWAF .GE. 1 .AND. JWAF .LE. NWAFER) THEN
C
C ----------------------------------------------------------------------
        JFAC = VJFACW(JWAF)
C
C        undo the translation
C
C        [x']    [x]   [x wafer center]
C        [y']  = [y] - [y wafer center]
C        [z']    [z]   [z wafer center]
C
C        and undo the rotation
C
C        [v]    [ cos(phi)  sin(phi) 0]   [x']
C        [u]  = [-sin(phi)  cos(phi) 0] * [y']
C        [w]    [    0          0    1]   [z']
C
        VUW(1) = CPHIOF(JFAC)*(XYZ(1)-WAXCEN(JWAF))
     &          +SPHIOF(JFAC)*(XYZ(2)-WAYCEN(JWAF))
        VUW(2) =-SPHIOF(JFAC)*(XYZ(1)-WAXCEN(JWAF))
     &          +CPHIOF(JFAC)*(XYZ(2)-WAYCEN(JWAF))
        VUW(3) =  (XYZ(3) -  WAZCEN(JWAF))
C
        VXYZVU = VDOK
C
      ELSE
C
C     argument JWAF out of range
C
        VXYZVU = VDERR
C
      ENDIF
C
      RETURN
      END
