*DK sisidi
      SUBROUTINE SISIDI
C.----------------------------------------------------------------
C  B.Bloch-Devaux  October 91 - January 93
C! SCAL : Format digitizings apllying zero suppresion scheme
C     - Called by SIDIGI
C     - Calls     BDROP,BLIST                    from BOS lib
C                 ALBOS                          from ALEPHlib
C.----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
#include "siconst.h"
#include "sidijj.h"
#include "sihtjj.h"
#include "bmacro.h"
C-----------------------------------------------------------
C - Drop existing banks
      CALL BDROP (IW,'SIDI')
C - Find length of pad hit bank
      JSIHT = IW(NASIHT)
      IF (JSIHT.LE.0) GO TO 999
      IF (LROWS(JSIHT).LE.0) GO TO 999
C - Create new banks
      LEN = LROWS(JSIHT)*LSIDIA+LMHLEN
      CALL ALBOS('SIDI',0,LEN,JSIDI,IGARB)
      IW(JSIDI+LMHCOL) = LSIDIA
      IW(JSIDI+LMHROW) = 0
      IF ( IGARB.GT.0) JSIHT = IW(NASIHT)
C - Add to the 'E' list
      CALL BLIST(IW,'E+','SIDI')
C
C - Fill SIDI bank
C   copy info from SIHT into SIDI  if at least one of the pads of the
C   triplet is above Zero suppession threshold
C
      DO 10 IP = 1,LROWS(JSIHT)
         KHIT = KROW(JSIHT,IP)
         KDIG = KNEXT(JSIDI)
         IF( (IW(KHIT+JSIHE1).GT.ISIZCT).OR.(IW(KHIT+JSIHE2).GT.ISIZCT)
     $   .OR.     (IW(KHIT+JSIHE3).GT.ISIZCT)) THEN
            IW(KDIG+JSIDAD) = IW(KHIT+JSIHAD)
            IW(KDIG+JSIDE1) = IW(KHIT+JSIHE1)
            IW(KDIG+JSIDE2) = IW(KHIT+JSIHE2)
            IW(KDIG+JSIDE3) = IW(KHIT+JSIHE3)
            IW(JSIDI+LMHROW) = IW(JSIDI+LMHROW) + 1
         ENDIF
 10   CONTINUE
C
 999  RETURN
      END
#endif
