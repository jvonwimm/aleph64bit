*DK astgea
      SUBROUTINE ASTGEA
C -----------------------------------------------------
C - F.Ranjard - 880408 / 881005
C! clean KINE BOS banks and transfert to GEANT
C - called by   ASPEVE                        from this .HLB
C - calls       ASRKIN                        from this .HLB
C               FYTOKI                        from ALEPHLIB
C               NDROP,NLINK                   from BOSlib
C ---------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "jqcom.h"
#include "kincom.h"
#include "bmacro.h"
#include "kmacro.h"
C ---------------------------------------------------------
C - IF no KINE/VERT banks THEN
C      get them from FKIN/FVER if any
C   ENDIF
      IF (IW(NAKINE).EQ.0 .OR. IW(NAVERT).EQ.0) THEN
         CALL FYTOKI
      ENDIF
C
C - Drop banks produced by the TRAC module
      KVX = NLINK ('VERT',NIVXKI)
 1    KVX = IW(KVX-1)
      IF (KVX .NE. 0) THEN
         IDRP = NDROP ('VERT',IW(KVX-2))
         GOTO 1
      ENDIF
      KKI = NLINK ('KINE',NITRKI)
 2    KKI = IW(KKI-1)
      IF (KKI .NE. 0) THEN
         IDRP = NDROP ('KINE',IW(KKI-2))
         GOTO 2
      ENDIF
      CALL BDROP (IW,'IMPA')
C
C - clean banks coming from primary vertices and tracks
      KKI = NAKINE+1
 4    KKI = IW(KKI-1)
      IF (KKI .NE. 0) THEN
         NSV = NOFVK(KKI)
         KSV = KLISVK(KKI)
         DO 3 NS = 1,NOFVK(KKI)
            IF (IW(KSV+NS) .GT. NIVXKI) THEN
               NSV = NSV - 1
               IW(KSV+NS) = 0
            ENDIF
 3       CONTINUE
         IW(KKI+3) = NSV
         GOTO 4
      ENDIF
      KVX = NAVERT + 1
 5    KVX = IW(KVX-1)
      IF (KVX .NE. 0) THEN
         NST = NOFVK(KVX)
         KST = KLISVK(KVX)
         DO 6 NS = 1,NOFVK(KVX)
            IF (IW(KST+NS) .GT. NITRKI) THEN
               NST = NST - 1
               IW(KST+NS) = 0
            ENDIF
  6      CONTINUE
         IW(KVX+3) = NST
         GOTO 5
      ENDIF
C
      IF (FDEBJO .AND. IPRIJO(16).EQ.1) CALL PRKINE
C
C - reset KVOL length to the # of input vertices
      JKVOL = IW(NAKVOL)
      IW(JKVOL+LMHROW) = NIVXKI
C - add kinematic banks to 'E' list
      CALL BLIST (IW,'E+','KVOLIMPA')
C
C - transfert KINE and VERT banks to GEANT banks
      CALL ASRKIN
C
      END
#endif
