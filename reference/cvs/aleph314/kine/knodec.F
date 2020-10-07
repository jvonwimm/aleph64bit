      INTEGER FUNCTION KNODEC (NODEC,LPDEC)
C -----------------------------------------------------------
C - F.Ranjard -870423                    modified - 880831
C - B.Bloch                              modified - 920507
CKEY KINE KINGAL DECAY       /  USER  INTERNAL
C! Return in the array NODEC the user particle# of the part.
C  which have not to be decayed by the generator.
C  particles with time of life < 1.E-15 are still decayed by
C  the generator.
C  If a card KTMX exists , the life time is taken from the Data card
C  and supersedes the 1.E-15 value
C  they are KNODEC such particles
C  IF the bank KLIN does not exist THEN
C    KNODEC = - the # of nodecay particles
C  LPDEC is the dimension of the array NODEC
C  if KNODEC .gt. LPDEC it means that the buffer is too small
C
C - structure: INTEGER FUNCTION subprogram
C              User Entry Name: KNODEC
C              External References: NAMIND(BOS77)
C              Comdecks referenced: BCS, KIPARA, BMACRO
C
C - usage  : MXDEC = KNODEC (NODEC,LPDEC)
C - input  : NODEC = array to contain user generator particle #
C            LPDEC = length of the NODEC array
C - output : KNODEC= # of particles which should not be decayed
C                    by the generator (the #s of the 1st LPDEC
C                    are stored in NODEC)
#ifndef DOC
      SAVE
#include "bcs.h"
#include "kipara.h"
      INTEGER NODEC(*)
      DATA NAPAR /0/,IFIR/0/
      DATA TIMLI / 1.E-15/
#include "bmacro.h"
#include "kmacro.h"
C -----------------------------------------------------------
C  Get user's TIMLI value from KTMX data card if any
      JKTMX = IW(NAMIND('KTMX'))
      IF ( JKTMX.GT.0) THEN
         TIMLI = RW(JKTMX+1)
      ENDIF
#include "kiparlin.h"
      IDEC = 0
      JPART = IW(NAPAR)
      IF (JPART.EQ.0) THEN
         IDEC = -LGDCA
      ELSE
         JKLIN = IW(NAKLI)
         IF (JKLIN.EQ.0) THEN
            IDEC = -LGDCA
         ELSE
            DO 1 I=1,LROWS(JKLIN)
               IGDCA = ITABL (JPART,I,1)
               IUSER = ITABL (JKLIN,I,1)
               IF (IGDCA.GT.0 .AND. IGDCA.LE.LGDCA .AND.
     &             TIMLIF(I).GT.TIMLI .AND. IUSER.NE.0)THEN
                  IDEC = IDEC + 1
                  IF (IDEC.LE.LPDEC) NODEC(IDEC) = IUSER
               ENDIF
 1          CONTINUE
         ENDIF
      ENDIF
      KNODEC = IDEC
      IF (IFIR.EQ.0) THEN
         WRITE(IW(6),100) LGDCA,TIMLI
         IFIR = 1
      ENDIF
      RETURN
 100  FORMAT(1X,110('*'),/,1X,'*  For this run , among the first ',I5,
     $' particles , those with life time smaller than ',E12.5,' will be'
     $,/,1X,'*  decayed at generator level ',/,1x,110('*'))
      END
#endif
