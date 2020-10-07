*DK lcpad
      SUBROUTINE LCPAD(NAMI,ISTOR,IPAD,NHIT)
C--------------------------------------------------------------
C! Store energies in towers
C. - J.Dines Hansen & P.Hansen - 860417
C. - Store energy deposition on pads in banks LTHT or LTDI
C. - Input : NAMI   = name-index of bank
C.           ISTOR  = storey number
C.           IPAD   = tower address
C.           NHIT   = number of hits to be stored
C. - Called by  LCSHOW, LCTRAK                   from this .HLB
C -----------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*4 CHAINT, NAME
#include "iocom.h"
#include "jqcom.h"
#include "lcnamc.h"
#include "lccomc.h"
#include "bmacro.h"
C -------------------------------------------------------------
C
      KINDX = IW(NAMI)
      IF(KINDX .LE. 0)                        GOTO 999
      NROW = LROWS(KINDX)
      NCOL = LCOLS(KINDX)
C
C - Loop over towers starting from bottom of table
      KLL = KROW(KINDX,NROW)
      DO 200 I = 1,NROW
        IF (IPAD .EQ. IW(KLL+1))              GOTO 300
        KLL = KLL - NCOL
  200 CONTINUE
C
C - If address IPAD was not found then expand the table
  100 NROW = NROW + 1
      IF(LFRWRD(KINDX) .LE. NCOL) THEN
         LEN = (NROW+20)*NCOL + LMHLEN
         NAME = CHAINT (IW(KINDX-3))
         CALL ALBOS (NAME,0,LEN,KINDX,IGARB)
      ENDIF
      IW(KINDX+2) = NROW
      KLL = KROW(KINDX,NROW)
      IW(KLL+1) = IPAD
C
C - Deposit here the number of hits
  300 IW(KLL+1+ISTOR) = IW(KLL+1+ISTOR)+NHIT
  999 RETURN
      END
#endif
