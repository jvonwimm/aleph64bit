       INTEGER FUNCTION KGPART (IUSER)
C -----------------------------------------------------------
C - J.Boucrot - F.Ranjard - 870516
C! Return ALEPH particle # of user particle # IUSER
CKEY KINE KINGAL PART /  USER INTERNAL
C
C - structure: INTEGER FUNCTION subprogram
C              User Entry Name: KGPART
C              External References: NAMIND(BOS77)
C                                   IUCOMP(CERNLIB)
C              Comdecks referenced: BCS, BMACRO
C
C - usage   : IPART = KGPART (IUSER)
C - input   : IUSER = user generator particle #
C - Output  : KGPART = ALEPH#
C                      0 if not found
C                      -1 if KLIN bank not there
#ifndef DOC
      SAVE
#include "bcs.h"
      DATA NAKLI /0/
#include "bmacro.h"
C ----------------------------------------------------------
      IF (NAKLI.EQ.0) NAKLI = NAMIND ('KLIN')
C
      JKLIN = IW(NAKLI)
      IF (JKLIN.EQ.0) THEN
         KGPART = -1
         GOTO 999
      ELSE
         NPART = LROWS (JKLIN)
         KGPART = IUCOMP (IUSER,IW(JKLIN+LMHLEN+1),NPART)
      ENDIF
C
 999  CONTINUE
      END
#endif
