      INTEGER FUNCTION ALFIND (LIST,NAME)
C ----------------------------------------------------------
C - F.Ranjard - 880301
C!  Find NAME bank in LIST list
C - Input  : LIST   = list name
C             NAME  = bank name which is searched
C - Output : ALFIND = serial number of the NAME bank in the list
C                     0 means the bank is not in the list
C --------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER LIST*(*), NAME*4, NLISTB*4, NAML*4
C ----------------------------------------------------------------
      I = 0
 1    I = I+1
      NAML = NLISTB(IW,I,LIST)
      IF (NAML .NE. ' ') THEN
         IF (NAML .NE. NAME) GOTO 1
         ALFIND = I
      ELSE
         ALFIND = 0
      ENDIF
C
      END
