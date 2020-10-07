*DK agmate
      INTEGER FUNCTION AGMATE (NAME)
C -------------------------------------------------------------
C - F.Ranjard - 910121
C! returns the GEANT material # known by its name
CKEY GEOM MATERIAL
C - Input :   - NAME   / CHA16     = material name
C                                    if ' ' get all names in ALLNAM
C - Output :  - AGMATE / I         = Geant tracking medium #
C                                   = IAGMAT if NAME is ' '
C                                   = 0  if NAME does not exist
C                                   = -IAGMAT if ALLNAM too short
C ---------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*(*) NAME
      PARAMETER (NTMAT=200, LCHNAM=16, LALL=NTMAT*LCHNAM)
      CHARACTER*3200 ALLNAM
      CHARACTER*20 NAMG
      CHARACTER*80 MSG
      INTEGER INAME(5)
#include "gcbank.h"
#include "iocom.h"
#include "agcons.h"
C ---------------------------------------------------------
      IF (NAME.EQ.' ') THEN
C - get all existing materials , store them into ALLNAM
C
        IF (IAGMAT*LCHNAM.GT.LALL) THEN
          ITMAT  = -IAGMAT
          WRITE (LOUTIO,*) ' AGMATE NTMAT IAGMAT=',NTMAT,IAGMAT
          CALL ALTELL ('AGMATE: ALLNAM is too short',0,'STOP')
        ELSE
          ALLNAM = ' '
          DO 200 I=1,IAGMAT
            IF (GVERSN.LT.3.15) THEN
               CALL GFMATE (I,INAME,AD,ZD,DD,RD,AL,DUM,ND)
               CALL ALSTHO (INAME,5,NAMG)
            ELSE
               CALL GFMATE (I,NAMG,AD,ZD,DD,RD,AL,DUM,ND)
            ENDIF
            L = LENOCC (NAMG)
            IND = INDEX (NAMG,'$')
            IF (IND.NE.0) L=IND-1
            L = MIN (L,LCHNAM)
            IC = (I-1)*LCHNAM
            ALLNAM(IC+1:IC+L)  = NAMG(1:L)
  200     CONTINUE
          ITMAT = IAGMAT
        ENDIF
      ELSE
C
C - get the number from the name
        IND = INDEX(ALLNAM,NAME)
        IF (IND.EQ.0) THEN
C         this material does not exist
          ITMAT = 0
          L = LENOCC(NAME)
          MSG = ' AGMATE '//NAME(1:L)//' does not exist '
          L = LENOCC(MSG)
          CALL ALTELL (MSG(1:L),0,'STOP')
        ELSE
C        this material exists
          ITMAT  = IND/LCHNAM + 1
        ENDIF
      ENDIF
      AGMATE = ITMAT
      END
#endif
