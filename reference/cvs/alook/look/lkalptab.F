      SUBROUTINE LKALPTAB(TABLE,I1,I2,IOK)
C -------------------------------------------------------------
C - author: A.Bonissent
C! search into Aleph bank description on DAF
C input : bank name TABLE
C output : first and last row relevant for TABLE
C
C         return code : IOK 0=wrong
C -------------------------------------------------------------
#ifndef DOC
      CHARACTER*4 TABLE,TABLOC
#include "bcs.h"
      DATA NATADB, NATPRO /2*0/
#include "bmacro.h"
C -----------------------------------------------------------
      IF(NATPRO .EQ. 0)THEN
         IRET=MDARD(IW,JUNIDB(0),'TADB',0)
         IRET=MDARD(IW,JUNIDB(0),'TPRO',0)
         NATADB=NAMIND('TADB')
         NATPRO=NAMIND('TPRO')
      ENDIF
      CALL CLTOU(TABLE)
      IOK=0
      DO 10 I=1,2
      IF(I.EQ.1)THEN
         KGLOB=IW(NATADB)
         JP=2
      ELSEIF(I.EQ.2)THEN
         KGLOB=IW(NATPRO)
         JP=1
      ENDIF
      IF (KGLOB.EQ.0) RETURN
C
      NGLOB=LROWS(KGLOB)
      DO 10 IGLOB=1,NGLOB
         ICAR=ITABL(KGLOB,IGLOB,1)
         CALL UHTOC(ICAR,4,TABLOC,4)
         IF(TABLOC.EQ.TABLE)THEN
            IPOS=ITABL(KGLOB,IGLOB,2)
            IF(I.EQ.1)THEN
               I1=IPOS
               I2=0
            ENDIF
            IF(I.EQ.2)THEN
               I2=IPOS
               I1=0
            ENDIF
            IOK=1
            GO TO 20
         ENDIF
   10 CONTINUE
   20 CONTINUE
      RETURN
      END
#endif
