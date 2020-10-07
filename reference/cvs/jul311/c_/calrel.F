      SUBROUTINE CALREL (IREF,NREF,IREL,NREL)
C------------------------------------------------------------------------
C!   Finds relation between any 2 CHRL rows
C!
C!    Authors : M.N. Minard , M. Pepe       16/03/89
C    INPUT:
C-    IREF = Column number in CHRL for which you want
C-           to test whether relation exists
C-    NREF = Element number in column for which you
C-           test relation
C-    IREL = Column number in CHRL of wanted relation
C-   OUTPUT:
C-    NREL = First relation number found
C?
C!======================================================================
#ifndef DOC
#include "chrljj.h"
#include "bcs.h"
#include "bmacro.h"
C
C-    Initialization
C
      NREL = 0
      NACHRL = NAMIND('CHRL')
      KCHRL  = IW(NACHRL)
      IF ( KCHRL.EQ.0 ) GO TO 100
C
C-    Loop on CHRL  bank
C
      NCHRL = LROWS (KCHRL)
      DO 30 ICHRL = 1,NCHRL
C
C-    Search for the reference element
C
      IF ( ITABL(KCHRL,ICHRL,IREF).EQ.NREF) THEN
         IF( ITABL(KCHRL,ICHRL,IREL).NE.0 ) THEN
             NREL = ITABL(KCHRL,ICHRL,IREL)
C
C-    Stop search at the first relation
C
             GO TO 100
         ENDIF
      ENDIF
 30   CONTINUE
100   CONTINUE
      RETURN
      END
#endif
