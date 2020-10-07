*DK hccoin
      SUBROUTINE HCCOIN
C ---------------------------------------------------------
C
C! Compute intersection with Tubes for the Full Generator Mode level
C!
C!     Author :G.Catanesi 87/01/30
C!     Mod. by G.Catanesi 89/04/13 to take in account
C!                                    tube's inefficency
C!            Mod. by G.Catanesi 89/08/01
C!                    to take in account dead zone and dead tubes
C           Mod by L. Silvestris 18/3/92
C!
C!         Input  : McTrackElement stored in common TRKCOM
C!
C!         Output : HCSE  McHcTubesSegment
C!
C!
C!   -Called by :HCHIT
C!       -Calls : HCFITU, HCSTSE, HTDEAD from this .HLB
C!                GTRNSF from CERNLIB
C!                HCMDPL, HCDEIT         from Alephlib
C ------------------------------------------
#ifndef DOC
      SAVE
#include "trkcom.h"
#include "jobcom.h"
#include "iocom.h"
#include "hcloc.h"
#include "hccoun.h"
#include "hccong.h"
#include "hcgega.h"
      PARAMETER (LTUB=100)
      INTEGER ITUBE(LTUB)
      LOGICAL HCDEIT,LDEAD
      INTEGER HCEFF
      REAL XTUB(LTUB),YTUB(LTUB),DXTUB(LTUB),YPART(LTUB)
      DIMENSION VIN(3),VOUT(3),WIN(3),WOUT(3),COORI(2),COORO(2)
C ----------------------------------------------------------------
      IF (ITRKEL(8).EQ.1) THEN
         IF (TRKVOL.EQ.'HBL1' .OR. TRKVOL.EQ.'HBL2') THEN
            CALL GETRAN (TRKELE(15))
         ENDIF
         VIN(1) = TRKELE(1)
         VIN(2) = TRKELE(2)
         VIN(3) = TRKELE(3)
         CALL GTRNSF (TRKELE(1),TRKELE(15),TRKELE(18),WIN(1))
C
      ELSE IF((ITRKEL(8).EQ.2) .OR. (ITRKEL(9).NE.0)) THEN
C
         IHCIPL = ITRKEL(6)
         IMOD = ITRKEL(10)
         IHCMOD=IMOD
         CALL GTRNSF (TRKNXT(1),TRKELE(15),TRKELE(18),WOUT(1))
C
         CALL HCMDPL(IHCPOR,IMOD,IHCIPL,WIN,COORI)
         CALL HCMDPL(IHCPOR,IMOD,IHCIPL,WOUT,COORO)
C
C      Find hit tubes
C
         XX = (COORI(2)+COORO(2))/2.
         DX = ABS(COORI(2)-COORO(2))
         YY = ABS(COORI(1)+COORO(1))/2.
         DY = ABS(COORI(1)-COORO(1))
C
         IF(IHCPOR.EQ.2)THEN
            COSXZ = TRKELE(6)
         ELSE
            COSXZ = TRKELE(4)
         ENDIF
C
         IF(FHCDB2)THEN
            WRITE(LOUTIO,500)ITRKEL(1),IHCIPL
         ENDIF
C
C evaluates the projection on the wire DX
C
         DX = DX * HSTUST
         DY = DY * HSTUST
         XL = VDIST(WIN,WOUT,3) * HSTUST
C
C  cuts very long track elements
C
         IF(XL.GT.HTLEMX)THEN
            DX = DX * (HTLEMX/XL)
            DY = DY * (HTLEMX/XL)
         ENDIF
C
C - get a list of hit tubes
C
         CALL HCFITU(IHCPOR,IHCIPL,YY,DY,ITUBF,NTUB,YPART,NTUSP,WSPAC)
         IF(FHCDB2)THEN
            WRITE(LOUTIO,510)YY,DY,ITUBF,NTUB
         ENDIF
         IF(NTUB.LE.0) GOTO 20
C
         DO 10 I=1,NTUB
C
            ITUBE(I) = ITUBF+I-1
            DXTUB(I) = DX*YPART(I)
            IF(I.EQ.1)THEN
C           1st tube of the list
               YTUB(I) = YY + (YPART(I)-1.)*DY/2.
               XTUB(I) = XX + SIGN ((YPART(I)-1.)*DX/2.,COSXZ)
            ELSEIF (I.NE.NTUSP) THEN
C           next tube of the list
               YTUB(I) = YTUB(I-1)+ (YPART(I-1)+YPART(I))*DY/2.
               XTUB(I) = XTUB(I-1)+
     &                      SIGN ((DXTUB(I-1)+DXTUB(I))/2.,COSXZ)
            ELSE
C           tube which is replaced by a spacer
               XTUB(I) = XTUB(I) + SIGN (WSPAC*ATAN2(DX,DY),COSXZ)
               YTUB(I) = YTUB(I) + WSPAC
            ENDIF
            DXTUB(I)=MAX (HCSTDT,DXTUB(I))
C
   10    CONTINUE
C
C - if required kill not working tubes
C
          IF(ICHCJO(6).EQ.0)THEN
            DO 60 J=1,NTUB
               IF(ITUBE(J).GT.0)THEN
                  LDEAD = HCDEIT(ITUBE(J),IHCIPL,IHCMOD,IHCPOR)
                  IF(LDEAD) ITUBE(J) = -ITUBE(J)
               ENDIF
 60         CONTINUE
         ENDIF
C
C - Kill tubes for inefficency if required (==> tube# < 0)
C
         IF(ICHCJO(5).EQ.0)THEN
            DO 30 I=1,NTUB
              IF (ITUBE(I).GT.0) THEN
                IF (RNDM(DUM).GE.HCTEFF(IHCPOR)) ITUBE(I) = -ITUBE(I)
              ENDIF
 30         CONTINUE
         ENDIF
C
C - Take into account dead zones inside tubes
C
         IF(ICHCJO(3).EQ.0)THEN
            DO 40 J=1,NTUB
               IF(ITUBE(J).GT.0)THEN
                  CALL HTDEAD(XTUB,DXTUB,ITUBE(J),IDEAD)
                  IF(IDEAD.EQ.1) ITUBE(J) = -ITUBE(J)
               ENDIF
 40         CONTINUE
         ENDIF
C
C - Store fired tubes in the working bank JDHCSE
C
         CALL HCSTSE (VIN,IHCPOR,IMOD,IHCIPL,YTUB,XTUB,DXTUB,ITUBE,NTUB)
C
      ENDIF
C
   20 CONTINUE
      RETURN
  500 FORMAT(1X,' +++HCCOIN+++ Track #',I6,' Plane #',I3)
  510 FORMAT(1X,' +++HCCOIN+++ YY  , dy ',2F12.4,3X,'1st tube',I4,3X,
     & '# of tubes',I3)
      END
#endif
