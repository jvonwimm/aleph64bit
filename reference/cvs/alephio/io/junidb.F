      INTEGER FUNCTION JUNIDB(LDUM)
C ----------------------------------------------------------------
C - F.Ranjard - 881110
C!  return the data base logical unit
C   LDUM is a dummy argument
C   JUNIDB = LDEF (=4 by default)
C   there is an entry point : NUNIDB(LDUM) to redefine the default
C   IF 0< LDUM <99 THEN
C      reset default unit LDEF = LDUM
C   ENDIF
C   2 more entry points are forseen for the LongTermConstant file:
C   JUNISQ(LDUM) and NUNISQ(LDUM)
C   The default logical unit # for the LTC file is LSEQ=8
C   2 more entry points are forseen for the Documentation file:
C   JUNIDO(LDUM) and NUNIDO(LDUM)
C   The default logical unit # for the DOC file is LDOC=9
C
C - called by USER
C   -------------------------------------------------
#ifndef DOC
      DATA LDEF /4/ , LSEQ /0/ , LDOC /0/
C -----------------------------------------------------------------
C
      JUNIDB = LDEF
      RETURN
C
C - redefine logical unit #
      ENTRY NUNIDB (LDUM)
      NUNIDB = LDEF
      IF (LDUM.GT.0 .AND. LDUM.LE.99) THEN
         LDEF = LDUM
      ENDIF
      RETURN
C
C - get logical unit # for LongTermConstant file
      ENTRY JUNISQ (LDUM)
      JUNISQ = LSEQ
      RETURN
C
C - redefine logical unit # for LTC file
      ENTRY NUNISQ (LDUM)
      NUNISQ = LSEQ
      IF (LDUM.GT.0 .AND. LDUM.LE.99) THEN
         LSEQ = LDUM
      ENDIF
C
C - get logical unit # for Documentation file
      ENTRY JUNIDO (LDUM)
      JUNIDO = LDOC
      RETURN
C
C - redefine logical unit # for DOC file
      ENTRY NUNIDO (LDUM)
      NUNIDO = LDOC
      IF (LDUM.GT.0 .AND. LDUM.LE.99) THEN
         LDOC = LDUM
      ENDIF
C
      END
#endif
