      SUBROUTINE ELIMIN
C-----------------------------------------------------------------------
CKEY ECALDES MODULE SENSITIVE LIMIT / INTERNAL
C     H.Videau      Creation 17/08/86   Modification 26/01/88
C! Defines modules sensitive limits               M.Rumpf  03/19/87
C Initializes the limiting planes for the sensitive regions of the
C 12 modules of the 3 subcomponents.
C   Calls: EVOLPL
C   Called by EXPNEC. (Internal)
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Locales
      INTEGER LEPLAN,SC,MD
      CHARACTER*16 VOLNAM(ECALSC)
      DATA VOLNAM/'E sensitive','B sensitive','E sensitive'/
C
      DO 2014 SC=1,ECALSC
        DO 2013 MD=1,ECALMD
          CALL EVOLPL(VOLNAM(SC),SC,MD,LEPLAN,EALILP(1,1,MD,SC))
 2013   CONTINUE
 2014 CONTINUE
      END
#endif
