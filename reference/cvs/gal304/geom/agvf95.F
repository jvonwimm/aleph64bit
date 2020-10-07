*DK agvf95
      SUBROUTINE AGVF95
C------------------------------------------------------
C!  Defines and positions VDET 95 faces in GEANT
CKEY VDET GEOM FACE
C - Author : F.Ranjard 950419 from W.Manner and A.Bonissent
C.  -Called by AGVDET                 from this .HLB
C.  -Calls GSROTM,GSPOS               from  GEANT3
C.
C. - positions volumes VFAO and VFAS in VDET
C    VFAC is a box which contains : wafers, omega beam,
C.   and the electronic on the omega side.
C    VFPB is an adjacent box which contains the protection bonding
C    on the silicon side.
C---------------------------------------------------------------
#ifndef DOC
      REAL XYZ(3), XVFAO
      CHARACTER*4 VOLU, VDAU, CHAINT, CHFLAG
      INTEGER VNTFAC,VXYZFC,VPHIFC
#include "iocom.h"
#include "bcs.h"
#include "vposjj.h"
#include "vvoljj.h"
#include "agcons.h"
#include "bmacro.h"
C -------------------------------------------------------------------
C
C     Find X-position of the center of box VFAO with respect to
C     X-center of silicon active region VSWA in the face
C
      KVPOS = IW(NAMIND('VPOS'))
      IF(KVPOS.EQ.0)THEN
          CALL ALTELL ('AGVF95 : VPOS bank missing',0,'STOP')
      ENDIF
      VOLU = 'VSWA'
      XVFAO = 0.
   13 CONTINUE
      IF(VOLU .NE.'VFAC')THEN
         DO  IVPOS = 1, LROWS(KVPOS)
            VDAU = CHAINT(ITABL(KVPOS,IVPOS,JVPODA))
            IF(VDAU.EQ.VOLU)THEN
              VOLU = CHAINT(ITABL(KVPOS,IVPOS,JVPOMO))
              XVFAO = XVFAO + RTABL(KVPOS,IVPOS,JVPOPO)
              GO TO 13
            ENDIF
         ENDDO
         CALL ALTELL ('AGVF95 : NO VFAC in VPOS bank',0,'STOP')
      ENDIF
C
      VOLU = 'VEPS'
      ZVEPS = 0.
   14 CONTINUE
      IF(VOLU .NE.'VFAC')THEN
         DO  IVPOS = 1, LROWS(KVPOS)
            VDAU = CHAINT(ITABL(KVPOS,IVPOS,JVPODA))
            IF(VDAU.EQ.VOLU)THEN
              VOLU = CHAINT(ITABL(KVPOS,IVPOS,JVPOMO))
              ZVEPS = ZVEPS + RTABL(KVPOS,IVPOS,JVPOPO+2)
              GO TO 14
            ENDIF
         ENDDO
         CALL ALTELL ('AGVF95 : NO VEPS in VPOS bank',0,'STOP')
      ENDIF
C
C     Find X-distance of the center of box VFPB with respect to
C     X-center of box VFAC.
C
      KVVOL = IW(NAMIND('VVOL'))
      IF(KVVOL.EQ.0)THEN
          CALL ALTELL ('AGVF95 : VVOL bank missing',0,'STOP')
      ELSE
         XO = 0.
         XS = 0.
         DO  IVVOL = 1, LROWS(KVVOL)
            VOLU = CHAINT(ITABL(KVVOL,IVVOL,JVVOVO))
            IF (VOLU.EQ.'VFAC') THEN
              XO =  RTABL(KVVOL,IVVOL,JVVOPA)
            ELSEIF (VOLU.EQ.'VFPB') THEN
              XS =  RTABL(KVVOL,IVVOL,JVVOPA)
            ENDIF
         ENDDO
         IF (XO.EQ.0. OR. XS.EQ.0.) THEN
            CALL ALTELL ('AGVF95 : NO VFAC/VFPB in VVOL bank',0,'STOP')
         ELSE
            XOS = XO + XS
         ENDIF
      ENDIF
C
C-----Loop over faces
C
      NFAC = VNTFAC()
      DO IFAC = 1,NFAC
C
C-----Define the rotation angle of each side, possibly with inversion
C
         IDUM = VPHIFC(IFAC,PHI)
         ROTAN = PHI*RADEG
         IF(ROTAN.GT.D360)   ROTAN = ROTAN-D360
         ROTA2 = ROTAN+D90
         IF(ROTA2.GT.D360)   ROTA2 = ROTA2-D360
         IAGROT= IAGROT+1
         CALL GSROTM(IAGROT,D90,ROTAN,D90,ROTA2,0.,0.)
C
C-----Find the position of the centre of each face
C
         IDUM = VXYZFC(IFAC,XYZ)
C
C Compute displacements for the center of the box
C
         XXO = XYZ(1) - XVFAO*COS(PHI)
         YYO = XYZ(2) - XVFAO*SIN(PHI)
         XXS = XYZ(1) - (XVFAO-XOS)*COS(PHI)
         YYS = XYZ(2) - (XVFAO-XOS)*SIN(PHI)
         ZZS1 = XYZ(3) + ZVEPS
         ZZS2 = XYZ(3) -ZVEPS
C
C?----Put in place the faces
C
         CHFLAG = 'MANY'
         CALL GSPOS('VFAC',IFAC,'VDET',XXO,YYO,XYZ(3),IAGROT,CHFLAG)
         CALL GSPOS('VFPB',2*IFAC-1,'VDET',XXS,YYS,ZZS1,IAGROT,CHFLAG)
         CALL GSPOS('VFPB',2*IFAC,'VDET',XXS,YYS,ZZS2,IAGROT,CHFLAG)
      ENDDO
C
      RETURN
      END
#endif
