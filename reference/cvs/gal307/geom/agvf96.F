*DK agvf96
      SUBROUTINE AGVF96
C------------------------------------------------------
CKEY VDET GEOM FACE
C!  Defines and positions VDET 96 faces described in
C   ADBSCONS no.> 218 in GEANT.
C
C - Author : F.Ranjard 961113 from AGVF95
C
C - Called by AGVDET                 from this .HLB
C - Calls GSROTM,GSPOS               from  GEANT3
C
C - description: 
C   Positions volumes VFSI and VFPB in VDET
C   VFSI is a box which contains : wafers, omega beam,
C.  and the electronic on the omega side.
C   VFPB is an adjacent box which contains the protection bonding
C   on the silicon side.
C   Positions VOUP, VOKE, VOPI, VMEO in VDET
C---------------------------------------------------------------
#ifndef DOC
      REAL XYZ(3), XVFSI
      CHARACTER*4 VOLU, VDAU, CHAINT
      INTEGER VNTFAC,VXYZFC,VPHIFC
#include "iocom.h"
#include "bcs.h"
#include "vposjj.h"
#include "vvoljj.h"
#include "agcons.h"
#include "bmacro.h"
C -------------------------------------------------------------------
C
C     Find X-position of the center of box VFSI with respect to
C     X-center of silicon active region VSWA in the face
C
      KVPOS = IW(NAMIND('VPOS'))
      IF(KVPOS.EQ.0)THEN
          CALL ALTELL ('AGVF95 : VPOS bank missing',0,'STOP')
      ENDIF
      VOLU = 'VSWA'
      XVFSI = 0.
   13 CONTINUE
      IF(VOLU .NE.'VFSI')THEN
         DO  IVPOS = 1, LROWS(KVPOS)
            VDAU = CHAINT(ITABL(KVPOS,IVPOS,JVPODA))
            IF(VDAU.EQ.VOLU)THEN
              VOLU = CHAINT(ITABL(KVPOS,IVPOS,JVPOMO))
              XVFSI = XVFSI + RTABL(KVPOS,IVPOS,JVPOPO)
              GO TO 13
            ENDIF
         ENDDO
         CALL ALTELL ('AGVF95 : NO VFSI in VPOS bank',0,'STOP')
      ENDIF
C
      VOLU = 'VEPS'
      ZVEPS = 0.
   14 CONTINUE
      IF(VOLU .NE.'VFSI')THEN
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
C     X-center of box VFSI.
C
      KVVOL = IW(NAMIND('VVOL'))
      IF(KVVOL.EQ.0)THEN
          CALL ALTELL ('AGVF95 : VVOL bank missing',0,'STOP')
      ELSE
         DO  IVVOL = 1, LROWS(KVVOL)
            VOLU = CHAINT(ITABL(KVVOL,IVVOL,JVVOVO))
            IF (VOLU.EQ.'VFSI') THEN
              VFSIX = RTABL(KVVOL,IVVOL,JVVOPA)
              VFSIZ = RTABL(KVVOL,IVVOL,JVVOPA+2) 
            ELSEIF (VOLU.EQ.'VFPB') THEN
              XVFPB = VFSIX + RTABL(KVVOL,IVVOL,JVVOPA)
            ELSEIF (VOLU.EQ.'VOUP') THEN
              VOUPX = RTABL(KVVOL,IVVOL,JVVOPA)
              XVOUP = VFSIX + VOUPX
            ELSEIF (VOLU.EQ.'VOKE') THEN
              VOKEX = RTABL(KVVOL,IVVOL,JVVOPA)
              XVOKE = VFSIX + 2.*VOUPX + VOKEX 
              VOKEZ = RTABL(KVVOL,IVVOL,JVVOPA+2)
            ELSEIF (VOLU.EQ.'VMEO') THEN
              XVMEO = VFSIX + RTABL(KVVOL,IVVOL,JVVOPA)
              ZVMEO = VFSIZ - RTABL(KVVOL,IVVOL,JVVOPA+2)
            ELSEIF (VOLU.EQ.'VOPI') THEN
              VOPIX = RTABL(KVVOL,IVVOL,JVVOPA)
              VOPIZ = RTABL(KVVOL,IVVOL,JVVOPA+2)
              XVOPI = VFSIX + 2.*VOUPX + VOPIX
              ZVOPI = VOKEZ + VOPIZ
            ENDIF
         ENDDO
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
C     VFSI
         XXO = XYZ(1) - XVFSI*COS(PHI)
         YYO = XYZ(2) - XVFSI*SIN(PHI)
         CALL GSPOS('VFSI',IFAC,'VDET',XXO,YYO,XYZ(3),IAGROT,'ONLY')
C     VFPB
         XXS = XYZ(1) - (XVFSI-XVFPB)*COS(PHI)
         YYS = XYZ(2) - (XVFSI-XVFPB)*SIN(PHI)
         ZZS1 = XYZ(3) + ZVEPS
         ZZS2 = XYZ(3) - ZVEPS
         CALL GSPOS('VFPB',2*IFAC-1,'VDET',XXS,YYS,ZZS1,IAGROT,'ONLY')
         CALL GSPOS('VFPB',2*IFAC,'VDET',XXS,YYS,ZZS2,IAGROT,'ONLY')
C     VOUP
         XXO = XYZ(1) - (XVFSI+XVOUP)*COS(PHI)
         YYO = XYZ(2) - (XVFSI+XVOUP)*SIN(PHI)
         CALL GSPOS('VOUP',IFAC,'VDET',XXO,YYO,XYZ(3),IAGROT,'ONLY')
C     VOKE
         XXS = XYZ(1) - (XVFSI+XVOKE)*COS(PHI)
         YYS = XYZ(2) - (XVFSI+XVOKE)*SIN(PHI)
         CALL GSPOS('VOKE',IFAC,'VDET',XXS,YYS,XYZ(3),IAGROT,'MANY')
C     VOPI 
         XXS = XYZ(1) - (XVFSI+XVOPI)*COS(PHI)
         YYS = XYZ(2) - (XVFSI+XVOPI)*SIN(PHI)
         ZZS1 = XYZ(3) - ZVOPI
         ZZS2 = XYZ(3) + ZVOPI
         CALL GSPOS('VOPI',2*IFAC-1,'VDET',XXS,YYS,ZZS1,IAGROT,'MANY')
         CALL GSPOS('VOPI',2*IFAC,'VDET',XXS,YYS,ZZS2,IAGROT,'MANY')
C     VMEO 
         XXS = XYZ(1) - (XVFSI+XVMEO)*COS(PHI)
         YYS = XYZ(2) - (XVFSI+XVMEO)*SIN(PHI)
         ZZS1 = XYZ(3) - ZVMEO
         ZZS2 = XYZ(3) + ZVMEO
         CALL GSPOS('VMEO',2*IFAC-1,'VDET',XXS,YYS,ZZS1,IAGROT,'ONLY')
         ROTA2 = ROTAN+D270
         IF(ROTA2.GT.D360)   ROTA2 = ROTA2-D360
         CALL GSROTM(IAGROT+1,D90,ROTAN,D90,ROTA2,D180,0.)
         CALL GSPOS('VMEO',2*IFAC,'VDET',XXS,YYS,ZZS2,IAGROT+1,'ONLY')
         IAGROT= IAGROT+1
C
C?----next face
C
      ENDDO
C
      RETURN
      END
#endif
