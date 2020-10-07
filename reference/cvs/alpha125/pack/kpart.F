      FUNCTION KPART (CINP)
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! KPART: Get particle code KPART for particle name CNAM
C  ENTRY KPANTI : Get particle code KPART for antiparticle of CNAM
C                                                   H.Albrecht 27.11.88
C  Completely rewritten to avoid ASSIGN statements  J.Boucrot  21.09.93
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER *(*)  CINP
#include "qcdesh.h"
      CHARACTER*12 OLDINP
      INTEGER      OLDPAR
      DATA         OLDINP / '????????????' /
C----------------------------------------------------------------------
      IANTP=0
      GO TO 1
C----------------------------------------------------------------------
      ENTRY KPANTI (CINP, IANTI)
      IANTP=1
C
 1    CONTINUE
      IF (CINP .EQ. OLDINP )  THEN
         KPART = OLDPAR
      ELSE
         KPART  = KPCOMP( CINP )
         OLDINP = CINP
         OLDPAR = KPART
      ENDIF
      IF (KPART .EQ. 0)  GO TO 900
C
C       set flag (particle is used inside the program)
C
      IW(KOQPBT+KPART) = IOR (IW(KOQPBT+KPART), KBIT(1))
      IF (IANTP.EQ.0) GO TO 900
      IF (IANTI .NE. 0)
     &  KPANTI = IW (KOQPAR + KPART * KCQPAR + JQPAAN)
  900 END
#endif
