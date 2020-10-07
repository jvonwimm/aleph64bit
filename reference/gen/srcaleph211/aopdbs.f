      SUBROUTINE AOPDBS(FNAME,IRC)
C--------------------------------------------------------------------
CKEY ALEF OPEN DB / USER
C!  OPEN  ALEPH database ( DEF= ADBSCONS DAF )
C - Author:   D.Schlatter
C - modified by: F.Ranjard - 890302
C                          - 920513 - use the ALEPH name
C   Input:    FNAME       filename,
C                         if '  ' then
C                           try to get file from FDBA data card
C                           if OK return
C                         endif
C                           try to open 'ADBSCONS' from the environment
C                         endif
C   Output:   IRC         zero for successful opening
C
C   Calls:    AOPEN , JUNIDB
C--------------------------------------------------------------------
      CHARACTER*(*) FNAME
      CHARACTER*80 FDEVI, FNAM
C-------------------------------------------------------------------
C
      LDBAS = JUNIDB(0)
      FDEVI = ' '
      IF (FNAME .EQ. ' ') THEN
         CALL AGTFIL ('FDBA','READ',LDBAS,IRET)
         IF (IRET .EQ. -1) THEN
            FNAM = 'ADBSCONS'
         ELSE
            IRC = IRET
            GOTO 999
         ENDIF
      ELSE
         FNAM = FNAME
      ENDIF
      CALL AOPEN(LDBAS,FNAM,'DAF ',FDEVI,IRC)
C
999   RETURN
      END
