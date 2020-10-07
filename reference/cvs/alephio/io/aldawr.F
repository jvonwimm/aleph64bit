      SUBROUTINE ALDAWR (IW,LUN,LIST)
C ----------------------------------------------------------------------
C - F.Ranjard - 900517
CKEY ALEF WRITE BANK DA
C! write list of banks on direct access BOS file
C
C - Input:    IW      / INTE = BOS array
C             LUN     / INTE = d.a file logical unit
C             LIST    / A    = list of banks to be written
C ----------------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) LIST
      CHARACTER*4 NAME,NLISTB
      INTEGER IW(*)
C ----------------------------------------------------------------
C
C - get one bank at a time from the LIST
C
      I=0
 10   I=I+1
      NAME=NLISTB(IW,I,LIST)
      IF (NAME.NE.'    ') THEN
         NAMI = NAMIND(NAME)
         JNAME= NAMI+1
 11      JNAME= IW(JNAME-1)
         IF (JNAME.NE.0) THEN
            CALL BDAWR(IW,LUN,NAME,IW(JNAME-2))
            GOTO 11
         ENDIF
         GOTO 10
      ENDIF
C
      END
#endif
