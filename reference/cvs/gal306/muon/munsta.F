*DK munsta
      FUNCTION MUNSTA(NSLOT,TMUCVO,TMU3VO)
C
C*******************************************************************************
C
C T.Wang -860422
C
C! finds the address of a module in the geometric BOS
C       bank by means of the slot number, the name of the current volume
C       and the name of the third level volume.
C
C       Called by MUGETC
C
C*******************************************************************************
#ifndef DOC
      SAVE
      CHARACTER*4 TMUCVO,TMU3VO
#include "bcs.h"
#include "mug1pr.h"
C
      IF( TMUCVO .EQ. 'MUB1' )THEN
         MUNSTA = NSLOT
      ELSE IF( TMUCVO .EQ. 'MUB2' )THEN
         MUNSTA = NSLOT + NMBIN
      ELSE IF( TMUCVO .EQ. 'MUM1' )THEN
         IF( TMU3VO .EQ. 'MUEA' )THEN
            MUNSTA = MMADPR(NSLOT,1)
         ELSE IF( TMU3VO .EQ. 'MUEB' )THEN
            MUNSTA = MMADPR(NSLOT,3)
         ENDIF
      ELSE IF( TMUCVO .EQ. 'MUM2' )THEN
         IF( TMU3VO .EQ. 'MUEA' )THEN
            MUNSTA = MMADPR(NSLOT,2)
         ELSE IF( TMU3VO .EQ. 'MUEB' )THEN
            MUNSTA = MMADPR(NSLOT,4)
         ENDIF
      ELSE IF( TMUCVO .EQ. 'MUC1' )THEN
         IF( TMU3VO .EQ. 'MUEA' )THEN
            MUNSTA = NSLOT
         ELSE IF( TMU3VO .EQ. 'MUEB' )THEN
            MUNSTA = NSLOT + NMCA
         ENDIF
      ELSE IF( TMUCVO .EQ. 'MUC2' )THEN
         IF( TMU3VO .EQ. 'MUEA' )THEN
            MUNSTA = NSLOT + NMCIN
         ELSE IF( TMU3VO .EQ. 'MUEB' )THEN
            MUNSTA = NSLOT + NMCBI
         ENDIF
      ENDIF
      RETURN
      END
#endif
