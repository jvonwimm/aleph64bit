      REAL FUNCTION HTULEN(ITUB,ILAY,IMOD,IPOR,IPOS)
C-------------------------------------------------------------
CKEY HCALDES HCAL TUBE LENGTH /USER
C
C!   Return the active length of each tube in HCAL
C!   If 0. the tube is absent or dead
C!
C!                    Author : G.Catanesi 23/5/89
C!                    Mod by: Andrea Venturi & L.Silvestris 24/6/92
C!                    to take in account the shift of tubes in barrel...
C!
C!         INPUT:
C!                 ITUB/I = Tube#
C!                 ILAY/I = Layer#
C!                 IMOD/I = Module#
C!                 IPOR/I = Portion#
C!                 IPOS/I = position sign (for the barrel only)
C!
C----------------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
      HTUACL = 0.
C
C   Evaluate eightfold and double_eightfold number
C
      CALL HNEIGH(ITUB,ILAY,IPOR,IHEIF,IDHEI)
      IF(IHEIF.EQ.0)GOTO 99
C
      IF (IPOR.EQ.LPBAR) THEN
C
C  Barrel Case  ============================================
C
         IF (IMOD.NE.NHMBDF(1).AND.IMOD.NE.NHMBDF(2)) THEN
C
C  Standard module case
C
            IF ((IHEIF.LE.NHBLI3) .OR.
     &        (IHEIF.GT.(NEITHC(ILAY)-NHBLO3)))THEN
                 HTUACL = XLNHCE(3)
            ELSEIF((IHEIF.GT.NHBLI3) .AND.
     &        (IHEIF.LE.(NHBLI3+NHBLA2)))THEN
                 HTUACL = XLNHCE(2)
            ELSE
                 HTUACL = XLNHCE(1)
            ENDIF
C
C  Kills last 2 eigthfold of first plane in modules 15 and 22
C
            IF((IMOD.EQ.NHMBFL(1).OR.IMOD.EQ.NHMBFL(2)) .AND.
     &         (ILAY.EQ.1))THEN
                  ILAST = NEITHC(ILAY) - NHBDOU
                  IF(IHEIF.GT.ILAST)GOTO 99
            ENDIF
C
         ELSE
C
C  Special case (modules 6 and 7)
C
            ISEC = NHTY4D + NHTY3D
            ITHR = ISEC + NHTY2D
            IF(IHEIF.LE.NHTY4D)THEN
                 HTUACL = XLNHCE(4)
            ELSEIF((IHEIF.GT.NHTY4D).AND.(IHEIF.LE.ISEC))THEN
                 HTUACL = XLNHCE(3)
            ELSEIF((IHEIF.GT.ISEC).AND.(IHEIF.LE.ITHR))THEN
                 HTUACL = XLNHCE(2)
            ELSEIF((IHEIF.GT.ITHR) .OR.
     &             (IHEIF.GT.(NEITHC(ILAY)-NHBLO3)))THEN
                 HTUACL = XLNHCE(1)
            ELSE
                 HTUACL = XLNHCE(3)
            ENDIF
         ENDIF
C
         XBAR = HTUACL / 2.
         IS = IPOS
         IF(MOD(IMOD,2).NE.0) IS=-IS
C
C   Take into account the tubes shift in XBAR
C
C   There are 4 types of tubes
         DO J=1,4
            IF (ABS(XBAR-XLNHCE(J)*.5) .LT. 0.5) THEN
               IF (IS.GE.0)  THEN
                  XBAR=XBAR-HCTUSH-XHCSHI(J)
               ELSE
                  XBAR=XBAR+XHCSHI(J)
               ENDIF
               GOTO 10
            ENDIF
         ENDDO
C
 10      HTULEN = XBAR*2.
C
      ELSE
C
C Endcap Case ===========================================
C
C   Evaluate angle and length of the double_eightfold profile
C
            DTUBL = HTXDTL(IDHEI,ILAY)
            IF(DTUBL.EQ.0.)GOTO 99
            ANGLE = HTXANG(IDHEI,ILAY)
            STEP  = HTXSPL(IDHEI,ILAY)
C
C Calculates Tube_profile length
C
            IF(ANGLE.GT.0.) THEN
C           Tube_profile length 30 degrees case
                 TUBL = DTUBL - (STEP*.5+MOD((ITUB-1),(2*NHCEIT))*STEP)
C
            ELSEIF (ANGLE.LT.0.) THEN
C           Tube_profile length -30 degrees case
                 IF(MOD(IHEIF,2).EQ.0)THEN
                    TUBL = HCTLEI(3,IDHEI)
                 ELSE
                    TUBL = DTUBL
                 ENDIF
C
            ELSE
C           Tube_profile length 0 degrees case
                 TUBL = DTUBL
                 IF((IHEIF.EQ.4) .AND. (ILAY.GT.NHCINL))
     &                TUBL = HCTLEO(3,IDHEI)
            ENDIF
C
C Evaluate non active zones at the boundaries of the tube profile
C
            XOUT = HTINEC(1)
            XINN = HTINEC(2)
C
C Subtract these zones to the tube-profile length
C
            HTULEN = TUBL - XOUT - XINN
            IF(HTULEN.LE.0.)GOTO 99
C
         ENDIF        ! ==========================================
C
         RETURN
C
C The tube is absent or dead
C
99       CONTINUE
         HTULEN = 0.
         END
#endif
