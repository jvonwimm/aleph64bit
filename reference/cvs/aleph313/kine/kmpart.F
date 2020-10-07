      INTEGER FUNCTION KMPART (IPART,WIDTH,IANTI)
C -----------------------------------------------------------
C - F.Ranjard - 890203
C
C! Complete the PART bank with the width and the antiparticle#
CKEY KINE KINGAL PART FORMAT  /  USER  INTERNAL
C  first Get the necessary name-indices and check that the # of
C  columns of the PART bank is .ge. 10
C  then  IF the particle exists THEN
C           fill word(9) and word(10) of the given particle
C        ELSE
C           RETURN
C        END IF
C  then  Return the particle row # or 0
C
C - structure : INTEGER FUNCTION subprogram
C               User Entry Name: KBPART
C               External References: NAMIND(BOS77)
C               Comdeck References: BCS, BMACRO, PARTJJ
C
C - usage : IRETU  = KMPART (IPART,WIDTH,IANTI)
C - input : IPART  = Aleph particle# (row# in PART bank).
C           WIDTH  = Mass width (or 0.)
C           IANTI  = row# of the antiparticle
C - output: KMPART = Aleph particle#
C                    0 means no PART bank
C                   -1       PART bank has not enough columns
C                   -2       Aleph particle does not exist
C                   -3       antiparticle does not exist
#ifndef DOC
      SAVE
#include "bcs.h"
#include "kipara.h"
#include "partjj.h"
      EXTERNAL NAMIND
      DATA NAPAR /0/
#include "bmacro.h"
C ---------------------------------------------------------
      KMPART = 0
C
C - The 1st time get PART bank and check the # of columns
C
      IF (NAPAR .EQ.0 ) THEN
         NAPAR = NAMIND ('PART')
         JPART = IW(NAPAR)
         IF (JPART.EQ.0) THEN
            KMPART = 0
            GOTO 999
         ELSEIF (LCOLS(JPART).LT.JPARAN) THEN
            KMPART = -1
            GOTO 999
         ENDIF
      ENDIF
C
C - Get PART bank, return if does not exist or if part# or antipart#
C   does not exist
      JPART = IW(NAPAR)
      IF (JPART.EQ.0) GOTO 999
      NPART = IW(JPART+LMHROW)
      IF (IPART .GT. NPART) THEN
         KMPART = -2
         GOTO 999
      ELSEIF (IANTI .GT. NPART) THEN
         KMPART = -3
         GOTO 999
      ENDIF
C
C - Normal entry
      KPART = KROW(JPART,IPART)
      RW(KPART+JPARMW) = WIDTH
      IW(KPART+JPARAN) = IANTI
C
      KMPART = IPART
C
 999  CONTINUE
      END
#endif
