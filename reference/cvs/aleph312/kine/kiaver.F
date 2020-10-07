      SUBROUTINE KIAVER (AVER,IPROG)
C ----------------------------------------------------------------
C - F.Ranjard - 890201
C
C! Return the ALEPHLIB version # and the program origin
CKEY KINE KINGAL VERSION  /  USER INTERNAL
C  Return the ALEPHLIB version # used during KINGAL processing.
C  if no KINGAL processing return the ALEPHLIB version# used during
C  GALEPH processing.
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KIAVER
C              External References: NAMIND(BOS77)
C              Comdecks referenced: BCS, KGJJPAR, AJJPAR, BMACRO
C              Banks referenced: KJOB, AJOB
C
C - usage    : CALL KIAVER (AVER,IPROG)
C - output   : AVER  = ALEPHLIB version # used during the 1st step
C                      of the generation.
C                      0. means not a montecarlo file
C              IPROG = 1 means KINGAL generation
C                      2       GALEPH generation
C                      0       not a montecarlo file
#ifndef DOC
      SAVE
      REAL AVER
      INTEGER IPROG
#include "bcs.h"
#include "ajobjj.h"
#include "kjobjj.h"
#include "bmacro.h"
C -------------------------------------------------------------------
C
C - If KJOB exists (KINGAL was run) then get alephlib version #
      JKJOB = IW(NAMIND('KJOB'))
      IF (JKJOB .NE. 0) THEN
         IPROG = 1
         AVER = REAL(ITABL(JKJOB,1,JKJOAV)) / 10.
      ELSE
C     if KJOB does there but AJOB exists then get alephlib version#
         JAJOB = IW(NAMIND('AJOB'))
         IF (JAJOB .NE. 0) THEN
            IPROG = 2
            AVER  = REAL(ITABL(JAJOB,1,JAJOAV)) / 10.
         ELSE
C        there is no KJOB nor AJOB ==> not a montecarlo file
            IPROG = 0
            AVER  = 0.
         ENDIF
      ENDIF
C
      END
#endif
