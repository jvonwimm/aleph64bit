      SUBROUTINE LCHIT
C-------------------------------------------------------------
C! Controls tracking in LCal
C. - J.Dines Hansen & P.Hansen - 860417
C.                               modified by F.Ranjard - 890317
C. - Called by   GUSTEP                          from this .HLB
C. - Calls       LCSHOW, LCTRAK                  from this .HLB
C.               BLIST                           from BOS
C -----------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "iocom.h"
#include "jobcom.h"
#include "lccomc.h"
      PARAMETER (LWROW = 50, LSROW = 100)
#include "lcnamc.h"
#include "lshijj.h"
#include "lwhijj.h"
#include "tmacrod.h"
#include "tmacrof.h"
C -------------------------------------------------------------
      IF(FBEGJO(5)) THEN
C - Drop existing banks
         CALL BDROP (IW,'LWHTLTHTLSHILWHI')
C
C - Book hit banks at the beginning of events
C
C -  Wire plane history bank LWHI
         LEN = LWROW * LLWHIA + LMHLEN
         CALL ALBOS ('LWHI',0,LEN,KINDX,IGARB)
         IW(KINDX+LMHCOL) = LLWHIA
C
C - Tower history bank LSHI
         LEN = LSROW * LLSHIA + LMHLEN
         CALL ALBOS ('LSHI',0,LEN,KINDX,IGARB)
         IW(KINDX+LMHCOL) = LLSHIA
C
C - Add to the 'E' list or 'T' list
         CALL BLIST(IW,'E+','LSHI')
         CALL BLIST(IW,'T+','LWHILTHTLWHT')
C
C - Count events with tracks hitting LCAL
         KWS = IW(NALCWS)
         IF(KWS .LE. 0) GOTO 10
         IW(KWS+LMHLEN+1) = IW(KWS+LMHLEN+1) + 1
   10    CONTINUE
C
C - Set LC print flag
         IF(FDEBJO) THEN
         IF (IPRIJO(5) .NE. 0) LCPRNT = MAX0 (ICLCJO(1),1)
         ELSE
            LCPRNT = 0
         ENDIF
C
C - Set begin of event flag to false
         FBEGJO(5) = .FALSE.
      ENDIF
C
C ========
C
      IF(FELEC) THEN
C      e+/e- : create geantino and stop e+/e-
        IF (ITRKEL(8).EQ.1) THEN
         DO I=1,LNTRK
           TRKNXT(I) = TRKELE(I)
         ENDDO
         CALL CATINO
         ITRKEL(9) = NOMOR
        ENDIF
C
      ELSEIF(FTINO) THEN
C      develop geantino shower
        CALL LCSHOW
C
      ELSE
C      full tracking for hadron/muon
        CALL LCTRAK
      ENDIF
C
  999 CONTINUE
      RETURN
      END
#endif
