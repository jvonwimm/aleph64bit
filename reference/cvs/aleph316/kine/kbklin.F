      INTEGER FUNCTION KBKLIN (IPART,IUGEN)
C -----------------------------------------------------------
C - J.Boucrot - F.Ranjard - 870423
C
C! Book and fill KLIN bank
CKEY KINE KINGAL FILL BANK /   INTERNAL
C  the KLIN bank contains the user particle# IUGEN of an ALEPH particle#
C  IPART
C
C - structure : INTEGER FUNCTION subprogram
C               User Entry Names : KBKLIN
C               External References: AUBOS (ALEPHLIB)
C                                    BLIST/BKFMT(BOS77)
C               Comdecks referenced: BCS, KIPARA, BMACRO
C
C - usage:  JKLIN = KBKLIN (IPART,IUGEN)
C - input:  IPART = ALEPH particle# (row# in PART bank)
C           IUGEN = user generator particle#
C - output: KBKLIN= index of KLIN bank
C                   0  means : PART or KLIN bank missing
C                   -N means : cannot extend KLIN after row# N
#ifndef DOC
      SAVE
#include "bcs.h"
#include "kipara.h"
      DATA NAPAR /0/
#include "bmacro.h"
C ---------------------------------------------------------
C
      KBKLIN = 0
C
#include "kiparlin.h"
C
C - Get PART index
C
      JPART = IW(NAPAR)
      IF (JPART.EQ.0) GOTO 999
      NPART = LROWS (JPART)
C
C - Get KLIN index
C
      JKLIN = IW(NAKLI)
      IF (JKLIN .EQ.0) THEN
C     Create the KLIN bank
         CALL AUBOS ('KLIN',0,LCKLIN*NPART+LMHLEN,JKLIN,IGARB)
         IF (JKLIN.EQ.0) GOTO 999
         IW(JKLIN+LMHCOL) = LCKLIN
         IW(JKLIN+LMHROW) = NPART
         CALL BKFMT ('KLIN','I')
         CALL BLIST (IW,'C+','KLIN')
      ELSE
C     KLIN exists, test the length
         NKLIN = LROWS(JKLIN)
         IF (IPART .GT. NKLIN) THEN
            CALL AUBOS ('KLIN',0,LCKLIN*NPART+LMHLEN,JKLIN,IGARB)
            IF (JKLIN .EQ. 0) THEN
               KBKLIN = - NKLIN
               GOTO 999
            ELSE
               IW(JKLIN+LMHROW) = NPART
            ENDIF
         ENDIF
      ENDIF
C
C - Fill KLIN bank
C
      KKLIN = KROW(JKLIN,IPART)
      IW(KKLIN+1) = IUGEN
C
      KBKLIN = JKLIN
C
 999  RETURN
      END
#endif
