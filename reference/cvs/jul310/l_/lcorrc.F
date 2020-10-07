      SUBROUTINE LCORRC(IADDR,DIST,XOLD,EOLD,XYZ,ECLU,XLOC)
C-----------------------------------------------------------------------
C! Correct Lcal cluster attributes
C!
C!   Author   : P. H. Hansen   880801
C!   Modified : H. Meinhard    24-Apr-1991 : XLOC output added
C!   Input    : DIST           Distances from edges
C!              EOLD           LCLU cluster energy
C!              XOLD           LCLU cluster barycenter
C!              IADDR          Central tower address
C!   Output   : ECLU           Corrected cluster energy
C!              XYZ            Corrected cluster centroid position
C!              DIST           Corrected distance to acceptance edge
C!              XLOCi          Local cluster coordinates with all correc-
C!                             tions but misalignment
C!
C!   Description :
C!   ===========
C?   Correct position for finite cell size effect
C?   Extrapolate position to observed longitudinal barycenter.
C?   Use the wire energy when |Ewir-Eclus| < 3sigma
C?   Correct position and energy for the most probable leakages
C?   Finally make alignment corrections
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "lcaljj.h"
#include "lmtyjj.h"
#include "llayjj.h"
#include "lpdajj.h"
#include "lcrejj.h"
#include "lcpgjj.h"
#include "lalijj.h"
#include "lslojj.h"
      DIMENSION XOLD(*),XYZ(*),DIST(*),XLOC(*)
      DATA C0,C1/0.61311,0.66041/
#include "bmacro.h"
#include "lcmacr.h"
C----------------------------------------------------------------------
C
      ECLU = EOLD
      CALL UCOPY(XOLD(1),XYZ(1),3)
      CALL UCOPY(XOLD(1),XLOC(1),3)
C
      KLPDA = IW(NALPDA)
      IF(KLPDA.LE.0)                         GOTO 999
      KLLAY = IW(NALLAY)
      KLMTY = IW(NALMTY)
      KLCAL = IW(NALCAL)
      KLCRE = IW(NALCRE)
      KLCPG = IW(NALCPG)
C Nominal distance to first plane
      Z0 = RTABL(KLCAL,1,JLCAZD) + RTABL(KLMTY,1,JLMTFL)
C Thicknesses of first 29 planes
      S1 = RTABL(KLMTY,1,JLMTST)/9.
C Thicknesses of next 9 planes
      S2 = RTABL(KLMTY,1,JLMTST+2)/9.
C Pad-size in standard plane (cm)
      SREF = RTABL(KLCRE,1,JLCRSR)
C Cad-cam unit
      CUNIT = RTABL(KLMTY,1,JLMTCC)
C   Energy error
      EERR = RTABL(KLCRE,1,JLCREE)
C
C Module, row,column
      MODU = LCMOD(IADDR)
      IX = LCCOL(IADDR)
      IY = LCROW(IADDR)
C
C S corrections
      XPAD =  ABS(XYZ(1))/SREF-FLOAT(IX)
      YPAD =  XYZ(2)/SREF - (FLOAT(IY)-15.5)
      XC = XPAD*(1.-2.*ABS(XPAD))/(2.*C1*ABS(XPAD)+C0)
      YC = YPAD*(1.-2.*ABS(YPAD))/(2.*C1*ABS(YPAD)+C0)
      XYZ(1) = XYZ(1) + XC*SREF*SIGN(1.,XYZ(1))
      XYZ(2) = XYZ(2) +  YC*SREF + 0.5*CUNIT*SIGN(1.,XYZ(2))
      DIST(1) = DIST(1) + XC*SREF
      DIST(2) = DIST(2) +  YC*SREF + 0.5*CUNIT
C
C Find the average Z position and average pad-size using wire planes
      PLAV  = 0.
      ETOT  = 0.
      SAVR = 0.
      SCOR = 0.
      OFFS = 0.
      DO 100 IPL=1,38
        E = RTABL(KLPDA,MODU,JLPDEN+IPL-1)
        PLAV = PLAV + FLOAT(IPL)*E
        SAVR = SAVR + FLOAT(ITABL(KLLAY,IPL,JLLAPS))*CUNIT*E
        OFFS = OFFS + FLOAT(MOD(ITABL(KLLAY,IPL,JLLAPS),2))*0.5*CUNIT*E
        ETOT = ETOT + E
        IF(KLCPG.GT.0) THEN
          SAVR = SAVR + RTABL(KLCPG,IPL,JLCPPS)*E
          OFFS = OFFS + RTABL(KLCPG,IPL,JLCPPO)*E
        ENDIF
  100 CONTINUE
      IF(ETOT.LT.0.15)                     GOTO 999
      PLAV = PLAV/AMAX1(ETOT,0.15)
      PLAV = AMIN1(PLAV,38.)
      PLAV = AMAX1(PLAV,1.)
      ZAV  = Z0 + S1*AMIN1(PLAV,29.) + S2*AMAX1(0.,PLAV-29.)
      SAVR = SAVR/AMAX1(ETOT,0.15)
      OFFS = OFFS/AMAX1(ETOT,0.15)
      XYZ(1) = XYZ(1)*SAVR/SREF + OFFS*SIGN(1.,XYZ(1))
      XYZ(2) = XYZ(2)*SAVR/SREF
      XYZ(3) = XYZ(3)*(ZAV-0.400)/RTABL(KLCRE,1,JLCRZC)
      DIST(1) = DIST(1)*SAVR/SREF + OFFS
      DIST(2) = DIST(2)*SAVR/SREF
C
C Extrapolate distances back to the reference plane
      DIST(1) = DIST(1)*RTABL(KLCRE,1,JLCRZC)/ZAV
      DIST(2) = DIST(2)*RTABL(KLCRE,1,JLCRZC)/ZAV
C
C H. Meinhard: We have applied all corrections but the misalignment.
C Store the final cluster coordinate in the local (LCAL module) system
      CALL UCOPY(XYZ(1),XLOC(1),3)
C
C ** For now we do nothing on leakage corrections **
C
C Alignment correction :
C  Let the local module system be translated by
C  (Oxyz+Dxyz) and rotated by Rotvec with respect to ALEPH.
C  Let ALEPH coordinates be XYZ and local coordinates xyz:
C
C  XYZ = Oxyz + Dxyz + xyz + Rotvec x xyz
C
      KLALI = IW(NALALI)
      KLSLO = IW(NALSLO)
      ZLOC = XYZ(3) - RTABL(KLSLO,MODU,JLSLXS+2)
      XYZ(1) = XYZ(1) + RTABL(KLALI,MODU,JLALDX)
     &                + RTABL(KLALI,MODU,JLALDR+2)*XYZ(2)
     &                - RTABL(KLALI,MODU,JLALDR+1)*ZLOC
      XYZ(2) = XYZ(2) + RTABL(KLALI,MODU,JLALDX+1)
     &                + RTABL(KLALI,MODU,JLALDR)*ZLOC
     &                - RTABL(KLALI,MODU,JLALDR+2)*XYZ(1)
      XYZ(3) = XYZ(3) + RTABL(KLALI,MODU,JLALDX+2)
     &                + RTABL(KLALI,MODU,JLALDR+1)*XYZ(1)
     &                - RTABL(KLALI,MODU,JLALDR)*XYZ(2)
C
  999 CONTINUE
      END
#endif
