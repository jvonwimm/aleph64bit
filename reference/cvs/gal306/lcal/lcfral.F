*DK lcfral
      SUBROUTINE LCFRAL(IFB,MODU,XIN,XOUT,DXIN)
C--------------------------------------------------------------
C! Track element coordinate tranformation
C. - J.Dines Hansen & P.Hansen - 860505
C. - Mdified P.Hansen - 900415
C. - Transform track element from global to subdetector frame
C. - Input  : TRKELE
C. - Output : MODU   = Module No. [1,4]
C.            IFB    = endcap #
C. -          XIN    = xyz of track element TRKELE
C. -          XOUT   = xyz of track element TRKNXT
C. -          DXIN   = direction cosines of TRKELE
C. - Called by  LCSHOW, LCTRAK                   from this .HLB
C -------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION XIN(*),XOUT(*),DXIN(*)
#include "bcs.h"
#include "lcnamc.h"
#include "ljjpar.h"
#include "lccomc.h"
#include "trkcom.h"
#include "bmacro.h"
C -------------------------------------------------------------
C - Define origin of module at the start of the sensitive area
      MODU=1
      IF(TRKELE(3).GT.0.) MODU=3
      IF(TRKELE(1).GT.0.) MODU=MODU+1
      IFB=(MODU-1)/2+1
C
      KLMTY = IW(NALMTY)
      KLALI = IW(NALALI)
C
C
C - Translate track element to nominal local coordinates
      DO 10 I=1,3
        XIN(I)  = TRKELE(I) - XYZOLC(I,IFB)
        IF (ITRKEL(8).NE.1) XOUT(I)  = TRKNXT(I) - XYZOLC(I,IFB)
        DXIN(I) = TRKELE(3+I)
 10   CONTINUE
C
C Alignment correction :
C  Let the local module system be translated by
C  (Oxyz+Dxyz) and rotated by Rotvec with respect to ALEPH.
C  Let ALEPH coordinates be XYZ and local coordinates xyz:
C
C  XYZ = Oxyz + Dxyz + xyz + Rotvec x xyz
C
C  We modifify the nominal xyz in the following approximate way:
C
      ZIN  = RTABL(KLMTY,1,JLMTFL)*SIGN(1.,XIN(3))
     &      + XIN(3) - RTABL(KLALI,MODU,JLALDX+2)
C
      DXIN(3) = DXIN(3)-RTABL(KLALI,MODU,JLALDR+1)*DXIN(1)
     &                 +RTABL(KLALI,MODU,JLALDR)*DXIN(2)
      DXIN(1) = DXIN(1)
     &               - RTABL(KLALI,MODU,JLALDR+2)*DXIN(2)
     &               + RTABL(KLALI,MODU,JLALDR+1)*DXIN(3)
C
      DXIN(2) = DXIN(2)
     &               - RTABL(KLALI,MODU,JLALDR)*DXIN(3)
     &               + RTABL(KLALI,MODU,JLALDR+2)*DXIN(1)
C
C
      XIN(1) = XIN(1) - RTABL(KLALI,MODU,JLALDX)
     &                - RTABL(KLALI,MODU,JLALDR+2)*XIN(2)
     &                + RTABL(KLALI,MODU,JLALDR+1)*ZIN
C
      XIN(2)= XIN(2) - RTABL(KLALI,MODU,JLALDX+1)
     &               - RTABL(KLALI,MODU,JLALDR)*ZIN
     &               + RTABL(KLALI,MODU,JLALDR+2)*XIN(1)
C
      XIN(3)= XIN(3) - RTABL(KLALI,MODU,JLALDX+2)
     &               - RTABL(KLALI,MODU,JLALDR+1)*XIN(1)
     &               + RTABL(KLALI,MODU,JLALDR)*XIN(2)
C
      IF (ITRKEL(8).EQ.1) GOTO 999
C
      ZOUT = RTABL(KLMTY,1,JLMTFL)*SIGN(1.,XOUT(3))
     &      + XOUT(3) - RTABL(KLALI,MODU,JLALDX+2)
C
      XOUT(1) = XOUT(1) - RTABL(KLALI,MODU,JLALDX)
     &                - RTABL(KLALI,MODU,JLALDR+2)*XOUT(2)
     &                + RTABL(KLALI,MODU,JLALDR+1)*ZOUT
C
      XOUT(2)= XOUT(2) - RTABL(KLALI,MODU,JLALDX+1)
     &               - RTABL(KLALI,MODU,JLALDR)*ZOUT
     &               + RTABL(KLALI,MODU,JLALDR+2)*XOUT(1)
C
      XOUT(3)= XOUT(3) - RTABL(KLALI,MODU,JLALDX+2)
     &               - RTABL(KLALI,MODU,JLALDR+1)*XOUT(1)
     &               + RTABL(KLALI,MODU,JLALDR)*XOUT(2)
C
 999  CONTINUE
      END
#endif
