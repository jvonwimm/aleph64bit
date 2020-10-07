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
C!          Mod by A.Venturi and A.Sciaba 98/07/01
C!       bug fixes, improvments of local and abs coord evaluation
C!       and consistency checks
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
      DIMENSION VTUB(3,LTUB),WWIN(3),WWOUT(3)
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
         VOUT(1) = TRKNXT(1)
         VOUT(2) = TRKNXT(2)
         VOUT(3) = TRKNXT(3)
C
         WWIN(1)=WIN(1)
         WWIN(2)=WIN(2)
         WWIN(3)=WIN(3)
         WWOUT(1)=WOUT(1)
         WWOUT(2)=WOUT(2)
         WWOUT(3)=WOUT(3)
         CALL HCMDPL(IHCPOR,IMOD,IHCIPL,WWIN,COORI)
         CALL HCMDPL(IHCPOR,IMOD,IHCIPL,WWOUT,COORO)
C consistency check
         if (ihcpor.eq.2) then
            irem = mod(imod,2)
            if((vout(3)*wout(3).gt.0.and.irem.eq.1).or.
     &           (vout(3)*wout(3).lt.0.and.irem.eq.0).or.
     &           (wout(3).ne.cooro(2)))then
             WRITE(LOUTIO,*) 'HCCOIN: inconsistency in barrel'
             WRITE(LOUTIO,*) 'MOD,VOUT,WOUT,COORD',
     &                       IMOD,VOUT(3),WOUT(3),COORO(2)
            endif
         endif
C
C      Find hit tubes
C
         XX = (COORI(2)+COORO(2))/2.
         DX = ABS(COORI(2)-COORO(2))
         YY = ABS(COORI(1)+COORO(1))/2.
         DY = ABS(COORI(1)-COORO(1))
C
         IF(COORO(1).NE.COORI(1)) THEN
           COSXZ=(COORO(2)-COORI(2))/(COORO(1)-COORI(1))
         ELSE
           COSXZ=1.
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
C - save yy and dy to use it to redo xx and dx
         YYOL=YY
         DYOL=DY
C
         CALL HCFITU(IHCPOR,IHCIPL,YY,DY,ITUBF,NTUB,YPART,NTUSP,WSPAC)
         IF(FHCDB2)THEN
            WRITE(LOUTIO,510)YY,DY,ITUBF,NTUB
         ENDIF
C - re-evaluation of XX and DX
         IF(DYOL.NE.0) THEN
           XX=XX+COSXZ*(YY-YYOL)
           DX=DX*DY/DYOL
         ENDIF
         IF(NTUB.LE.0) GOTO 20
C
         IF(NTUB.GT.LTUB) 
     +   CALL ALTELL('HCCOIN: NTUB is too large',0,'RETURN')
         DO 10 I=1,MIN(NTUB,LTUB)
C
            ITUBE(I) = ITUBF+I-1
            DXTUB(I) = DX*YPART(I)
            IF(I.EQ.1)THEN
C           1st tube of the list
              YTUB(I) = YY + (YPART(I)-1.)*DY/2.
              XTUB(I) = XX + 
     +        SIGN ((YPART(I)-1.)*DX/2.,(YPART(I)-1.)*DX/2.*COSXZ)
            ELSE
C           next tube of the list
              YTUB(I) = YTUB(I-1)+ (YPART(I-1)+YPART(I))*DY/2.
              XTUB(I) = XTUB(I-1)+
     &                  SIGN ((DXTUB(I-1)+DXTUB(I))/2.,COSXZ)
C
              IF(I.EQ.NTUSP) THEN
C           tube which is replaced by a spacer
                 XTUB(I) = XTUB(I) + SIGN (WSPAC*(DX/DY),COSXZ)
                 YTUB(I) = YTUB(I) + WSPAC
              ENDIF
            ENDIF
C          coordinate in the absolute reference system
            IF(COORO(2).EQ.COORI(2)) THEN
              IF(NTUB.NE.1) 
     +        WRITE(LOUTIO,*) 'HCCOIN: NO STEP IN X !!'
              VTUB(1,I) = (VIN(1)+VOUT(1))/2.
              VTUB(2,I) = (VIN(2)+VOUT(2))/2.
              VTUB(3,I) = (VIN(3)+VOUT(3))/2.
            ELSE
              VTUB(1,I)=VIN(1)+
     +         (XTUB(I)-COORI(2))/(COORO(2)-COORI(2))*(VOUT(1)-VIN(1))
              VTUB(2,I)=VIN(2)+
     +         (XTUB(I)-COORI(2))/(COORO(2)-COORI(2))*(VOUT(2)-VIN(2))
              VTUB(3,I)=VIN(3)+
     +         (XTUB(I)-COORI(2))/(COORO(2)-COORI(2))*(VOUT(3)-VIN(3))
            ENDIF
c          consistency check
            IF(VTUB(1,I).LT.MIN(VIN(1),VOUT(1))-1E-2.OR.
     +         VTUB(1,I).GT.MAX(VIN(1),VOUT(1))+1E-2.OR.
     +         VTUB(2,I).LT.MIN(VIN(2),VOUT(2))-1E-2.OR.
     +         VTUB(2,I).GT.MAX(VIN(2),VOUT(2))+1E-2.OR.
     +         VTUB(3,I).LT.MIN(VIN(3),VOUT(3))-1E-2.OR.
     +         VTUB(3,I).GT.MAX(VIN(3),VOUT(3))+1E-2) THEN
              WRITE(LOUTIO,*) 'HCCOIN: problem in abs coord.'
              WRITE(LOUTIO,*) I,
     +        (VTUB(JJ,I),JJ=1,3),(VIN(JJ),JJ=1,3),(VOUT(JJ),JJ=1,3)
            ENDIF 
C         consistency check
            IF(XTUB(I).LT.MIN(COORI(2),COORO(2))-1E-2.OR.
     +         XTUB(I).GT.MAX(COORI(2),COORO(2))+1E-2.OR.
     +         YTUB(I).LT.MIN(COORI(1),COORO(1))-1E-2.OR.
     +         YTUB(I).GT.MAX(COORI(1),COORO(1))+1E-2) THEN
              WRITE(LOUTIO,*) 'HCCOIN: problem in loc coord.'
              WRITE(LOUTIO,*) I,
     +         YTUB(I),XTUB(I),(COORI(JJ),JJ=1,2),(COORO(JJ),JJ=1,2)
              write(loutio,*) ihcpor,ihcmod,ihcipl,itube(i),
     +                        yy,xx,dy,dx,cosxz,ntusp,wspac
            ENDIF 
C
   10    CONTINUE
C  -  the projection along the wire must be ge HCSTDT
         DO 15 I=1,NTUB
            DXTUB(I)=MAX (HCSTDT,DXTUB(I))
 15      CONTINUE
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
         CALL HCSTSE(VTUB,IHCPOR,IMOD,IHCIPL,YTUB,XTUB,DXTUB,ITUBE,NTUB)
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
