*DK agmedi
      INTEGER FUNCTION AGMEDI (NAME)
C -------------------------------------------------------------
C - W.Manner  - 901203
C! returns the GEANT tracking medium # known by its name
CKEY GEOM
C - Input :   - NAME   / CHA16     = tracking medium name
C                                    if ' ' get all names in ALLNAM
C - Output :  - AGMEDI / I         = Geant tracking medium #
C                                   = IAGMED if NAME is ' '
C                                   = 0  if NAME does not exist
C                                   = -IAGMED if ALLNAM too short
C ---------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*(*) NAME
      PARAMETER (NTMED=200, LCHNAM=16, LALL=NTMED*LCHNAM)
      CHARACTER*3200 ALLNAM
      CHARACTER*16 NAMG
      CHARACTER*80 MSG
      INTEGER INAME(5)
#include "gcbank.h"
#include "iocom.h"
#include "agcons.h"
C ---------------------------------------------------------
      IF (NAME.EQ.' ') THEN
C - get all existing tracking media , store them into ALLNAM
C
         IF (IAGMED*LCHNAM.GT.LALL) THEN
            ITMED  = -IAGMED
            WRITE (LOUTIO,*) ' AGMEDI NTMED IAGMED=',NTMED,IAGMED
            CALL ALTELL ('AGMEDI: ALLNAM is too short',0,'STOP')
         ELSE
            ALLNAM = ' '
            DO 200 I=1,IAGMED
               IF (GVERSN .LT. 3.15) THEN
                 CALL GFTMED (I,INAME,ND,ID,JD,FD,TL,DD,ED,PD,IFD,DU,ND)
                 CALL ALSTHO (INAME,5,NAMG)
               ELSE
                 CALL GFTMED (I,NAMG,ND,ID,JD,FD,TL,DD,ED,PD,IFD,DU,ND)
               ENDIF
               L = LNBLNK (NAMG)
               IND = INDEX (NAMG,'$')
               IF (IND.NE.0) L=IND-1
               L = MIN (L,LCHNAM)
               IC = (I-1)*LCHNAM
               ALLNAM(IC+1:IC+L)  = NAMG(1:L)
 200        CONTINUE
            ITMED = IAGMED
         ENDIF
      ELSE
C
C - get the number from the name
         IND = INDEX(ALLNAM,NAME)
         IF (IND.EQ.0) THEN
C         this tracking medium does not exist
           ITMED = 0
           L = LENOCC(NAME)
           MSG = ' AGMEDI '//NAME(1:L)//' does not exist '
           L = LENOCC(MSG)
           CALL ALTELL (MSG(1:L),0,'STOP')
         ELSE
C        this tracking medium exists
           ITMED  = IND/LCHNAM + 1
         ENDIF
      ENDIF
      AGMEDI = ITMED
      END
#endif
