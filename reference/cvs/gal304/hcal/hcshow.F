*DK hcshow
      SUBROUTINE HCSHOW
C---------------------------------------------------------------
C
C! Parametrize showers (Geantino mode)
C!
C!           Authors     :G.Catanesi 87/12/14
C!           Mod         :G.Catanesi 89/08/01
C!           Modified by :L.Silvestris 18/3/93
C!
C!       Called by :HCHIT                         from this .HLB
C!       Calls to :CAHMOD, CAHERB, HCPROJ, HCBACK from this .HLB
C!                 HCFITU,HCSTEN,HCSTSE
C!                 HCDEIT,HTDEAD                   from Alephlib
C!
C ---------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
#include "jobcom.h"
#include "iocom.h"
#include "hcgega.h"
#include "hccong.h"
#include "hccoun.h"
#include "hcloc.h"
#include "capano.h"
#include "tmacrod.h"
      REAL POS(3),VIN(3)
      LOGICAL HCDEIT,LDEAD
C --------------------------------------------------------------
#include "tmacrof.h"
C --------------------------------------------------------------
C?    Here it is a shower
      IF (FHCDB2)THEN
         WRITE (LOUTIO,*)' +++HCSHOW++ ITRKEL ',
     &   (ITRKEL(I),I=1,4),TRKVOL,(ITRKEL(I),I=6,11),FTRHAD
         WRITE (LOUTIO,'(1X,''TRKELE '',10F12.4)') TRKELE
         WRITE (LOUTIO,'(1X,''TRKNXT '',10F12.4)') TRKNXT
      ENDIF
C
C - fill bank HCVL,NR=track# and common /CAHTRA/
C   get the plane# IHPLN where interaction occurs
      CALL CAHMOD (IPAR,IHPLN)
      IF (FHCDB2) THEN
        WRITE(LOUTIO,*) ' after CAHMOD IHCPOR IHCMOD IHPLN',
     &                                 IHCPOR,IHCMOD,IHPLN
      ENDIF
C
C - IF initialization (IPAR=0) THEN  RETURN
      IF (IPAR . NE. 1) GO TO 30
C
      IEMG = 0
      CALL VZERO(POS,3)
      CALL CAHERB(EMGNRJ,IEMG,POS,NBEM)
      IF (IEMG.LT.1) GO TO 20
      IF(IHPLN.LT.1.OR.(IHPLN.GT.NHCPLA(IHCPOR))) GO TO 20
      IPOR = IHCPOR
      IMOD = IHCMOD
      IGO = SIGN(1.,HCPDIP)
      HCENMN = 0.
C
      IF (IEMG .EQ. 0) GO TO 20
C
      NEM = 0
C   LOOP ON E.M. SHOWER IN HCAL
   10 CONTINUE
      NBEM = 0
      IF (IEMG.EQ.0) GO TO 20
      CALL VZERO(POS,3)
      CALL CAHERB(EMGNRJ,IEMG,POS,NBEM)
      IF (NBEM .GT.0) NEM = NEM+1
      IF(NBEM.EQ.0) GO TO 10
C
C?....Search projection on sensible plane
C
      IF (FHCDB2) THEN
         WRITE(LOUTIO,*) ' before HCPROJ - POS ',POS
      ENDIF
      CALL HCPROJ(POS,IHPLN,IGO,KLEAV)
C..
C?....Transform back to the Aleph reference system
C..
      IF (FHCPRJ) THEN
         CALL HCBACK(POS,KLEAV,VIN,XX,YY)
C?        find tube number
         YY = ABS(YY)
         DY = 0.
         CALL HCFITU(IHCPOR,IHCIPL,YY,DY,ITUBF,NTUB,YPART,NTUSP,WSPAC)
C
C?        Store information in HCSE bank
         IF (NTUB.GT.0) THEN
            IHCIPL = IHPLN
C
C          if required kill not working tubes
            IF(ICHCJO(6).EQ.0)THEN
               LDEAD = HCDEIT(ITUBF,IHCIPL,IHCMOD,IHCPOR)
               IF(LDEAD)GOTO 40
            ENDIF
C
C - Kill tubes for inefficency if required
C
            IF(ICHCJO(5).EQ.0)THEN
               IF(RNDM(DUM).GE.HCTEFF(IHCPOR)) ITUBF = -ITUBF
               IF(ITUBF.LT.0) GOTO 40
            ENDIF
C
C          Take in account dead zones inside tubes
            IF(ICHCJO(3).EQ.0)THEN
               DXX = 0.
               CALL HTDEAD(XX,DXX,ITUBF,IDEAD)
               IF(IDEAD.EQ.1)GOTO 40
            ENDIF
C
C?          Take in account single streamer energy deposit fluctuation
            HCENOR = HCSTEN(1)
            IF (FHCDB2) THEN
              WRITE(LOUTIO,*)
     &       ' before HCSTSE VIN XX YY IHCPOR IHCIPL IHCMOD ITUBF= ',
     &                       VIN,XX,YY,IHCPOR,IHCIPL,IHCMOD,ITUBF
            ENDIF 
            CALL HCSTSE(VIN,IHCPOR,IHCMOD,IHCIPL,YY,XX,HCENOR,ITUBF,1)
         ENDIF
 40      CONTINUE
      ENDIF
C?....End of loop
C
      IHCPOR = IPOR
      IHCMOD = IMOD
C.................................................
C?Check if shower enters a new module to change step
C
      IF (IEMG.NE.0) GO TO 10
C
   20 CONTINUE
C
      IF (IHPLN.EQ.NHCPLA(IHCPOR)) THEN
C-    STOP GEANTINO TRACKING
         ITRKEL (9) = NOMOR
      ENDIF
   30 CONTINUE
C
      END
#endif
