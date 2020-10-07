      FUNCTION MUNSTA(NSLOT,TMUCVO,TMU3VO)
C
C***********************************************************************
C
C T.Wang -860422
C
C! finds the address of a module in the geometric BOS
C       bank by means of the slot number, the name of the current volume
C       and the name of the third level volume.
C
C       Called by MUGETC
C
C***********************************************************************
      SAVE
      CHARACTER*4 TMUCVO,TMU3VO
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! The parameters needed by the geometry routine AGMUCH
      PARAMETER (NMBIN = 12, NMBOU = 12 )
      PARAMETER (NMMIN = 10, NMMOU =  9, NMMA = NMMIN+NMMOU, IMMBT =  9)
      PARAMETER (NMCIN = 4, NMCOU = 4, NMCA = NMCIN+NMCOU)
      PARAMETER (NMMBI = NMMA+NMMIN, NMCBI = NMCA+NMCIN)
      COMMON /MUG1PR/   MMADPR(12,4)
C
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
