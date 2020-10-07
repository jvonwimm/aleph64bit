      INTEGER FUNCTION JALREC(LDUM)
C ----------------------------------------------------------------
CKEY ALREAD RECORD LENGTH
C - F.Ranjard - 881110
C!  return the aleph record length
C   LDUM is a dummy argument
C - JALREC = LDEF (=32040 by default) for EPIO and NATIVE files
C   on IBMRS6000 the default length of NATIVE files is 30720
C - NALREC(LDUM) is an entry point to redefine the default
C   IF 1800< LDUM <32760 THEN
C      reset default record length for EPIO and NATIVE files
C      LDEF = LDUM
C   ENDIF
C - KALREC (ATYPE) is an entry point to get record length for
C   a specific aleph type ATYPE
C   KALREC('EPIO') = 32040 bytes
C   KALREC('NAT')  = 32040 bytes   (30720 on IBMRS6000)
C   KALREC('DAF')  = 1024 words
C   KALREC('CARD') = 72 characters
C   KALREC('EDIR') = 80 characters
C - MALREC(ATYPE,LDUM) is an entry point to redefine the record length
C   to LDUM for a given alephtype ATYPE
C   a subsequent call to KALREC(ATYPE) will return the new record length
C   for alephtype ATYPE.
C
C - called by USER, AOPENW, AWRTAP
C ---------------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) ATYPE
      PARAMETER (LTYP=5,LALL=LTYP*3)
      CHARACTER*15 ALLTYP
      INTEGER LDEF(LTYP)
      DATA ALLTYP /'EPINATDAFCAREDI'/
#if defined(RS6K)
      DATA LDEF /32040,30720,1024,72,80/
#else
      DATA LDEF /32040,32040,1024,72,80/
#endif
C -----------------------------------------------------------------
C
      JALREC = LDEF(1)
      RETURN
C
C - redefine record length
      ENTRY NALREC (LDUM)
      IF (LDUM.GE.1800 .AND. LDUM.LE.32760) THEN
         LDEF(1) = LDUM
      ENDIF
      RETURN
C
C - return record length for a given ALEPH type
      ENTRY KALREC (ATYPE)
      K = INDEX (ALLTYP,ATYPE(1:3))
      IF (K.EQ.0) THEN
         KALREC = 0
      ELSE
         KALREC = LDEF (K/3+1)
      ENDIF
      RETURN
C
C - change the record length of a given ALEPH type
      ENTRY MALREC (ATYPE,LDUM)
      M = INDEX (ALLTYP,ATYPE(1:3))
      IF (M.EQ.0) THEN
         MALREC = 0
      ELSE
         LDEF(M/3+1) = LDUM
         MALREC = LDUM
      ENDIF
      RETURN
C
      END
#endif
