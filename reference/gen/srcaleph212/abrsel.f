      SUBROUTINE ABRSEL (ELIST,ULIST,JRET)
C --------------------------------------------------------------------
C! ALPHARD steering routine
CKEY ALPHARD STEERING
C - F.Ranjard - 900927
C   the package works in BATCH mode by default. A call to ABMODE
C   before the call to ABRSEL allows the user to switch to INTEractive
C   mode.
C   in BATCH mode the routine reproduces the behaviour of ABRSEL in
C   ALEPHLIB up to version 11.9
C - F.Ranjard - 911015
C   add INTERACTIVE mode
C
C - Input   : ELIST / CH*1  = input list
C             ULIST / A     = unpacking list . Look at AUNPCK for more
C                             complete description.
C                             'NODE' for no decompress and no unpacking
C                             '    ' for no unpacking
C                             'AL  ' for unpack everything
C - Output  : IRET  / I     = return code. Look at ABRREC for more
C                             details
C                             1  event record
C                             2  run record
C                             3  unknown record
C                             4  EOF on EDIR file
C                             5  EOF on data file
C                             6  no more input file
C                            10  input file not opened
C - Entry   : ABMODE (NEWMOD)
C             to switch from BATCH to INTERACTIVE mode
C   i.e. - CALL ABMODE ('INTE')
C          CALL ABRSEL ('E','AL ',IRET)
C    or  - CALL ABMODE ('BATC')
C          CALL ABRSEL ('E','TP IT FT ',IRET)
C
C -------------------------------------------------------------------
      CHARACTER*(*) ELIST,ULIST,NEWMOD
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /ABRCOM/ BATCH,INIT,CLOSE1,CLOSE2,FWFILM
     &               ,IUNDAT(2),IUTDAT(5),IUNSEL,IUNSE2,IUTSEL
     &               ,MASKA,MASKW
     &               ,WLIST,TLIST
      LOGICAL BATCH, INIT, CLOSE1, CLOSE2, FWFILM
      CHARACTER*1   TLIST, WLIST
C
      LOGICAL START
      CHARACTER*4 MODE
      INTEGER ACARD1
      SAVE IRET
      DATA  START /.TRUE./, MODE/'BATC'/
C --------------------------------------------------------------------
C
C - IRET is a local variable which keeps the status of the previous
C   call to ABRSEL
C - NRET is an intermediate return code
C - JRET is the return argument. It is set to IRET before RETURN
C
C - 1st entry
C
      IF (START)  THEN
         INIT   = .TRUE.
         CLOSE1 = .FALSE.
         CLOSE2 = .FALSE.
C     check data cards: in case of error RETURN
         CALL ABCHCK (IRET)
         IF (IRET.EQ.11) GOTO 900
C     set BATCH flag depending of the running mode
         BATCH = .TRUE.
         IF (MODE.EQ.'INTE') BATCH = .FALSE.
         IF (BATCH) THEN
C        open files following FILI and FILO
            IRET = 5
            CALL ABOPEN ('BOTH',NRET)
            IF (NRET.NE.5) THEN
               IRET = NRET
               GOTO 900
            ENDIF
         ENDIF
      ENDIF
C
C - next entry
C
C     ! ========================================
      IF (BATCH) THEN
C
C     if the user call ABRSEL after a IRET=6 it means
C     that he wants to loop over FILI cards
         IF (IRET.EQ.6) THEN
            IRET = 5
            IDUM = ACARD1 ('    ')
         ELSE
C     read a record
            CALL ABRREC (ELIST,ULIST,IRET)
         ENDIF
C     if EOF on edir or data file then open a new one
         IF (IRET.EQ.4 .OR. IRET.EQ.5) THEN
C        open files following FILI and FILO
            CALL ABOPEN('BOTH',NRET)
            IF (NRET .NE. 5) THEN
               IRET = NRET
               GOTO 900
            ENDIF
         ENDIF
C
C     ! =======================================
      ENDIF
C
C - End
C
  900 JRET = IRET
      START = .FALSE.
      RETURN
C
C ---------------------------------------------------------------------
C
      ENTRY ABMODE (NEWMOD)
C
      MODE = NEWMOD
      BATCH = .TRUE.
      IF (MODE.EQ.'INTE') BATCH=.FALSE.
      GOTO 999
C
C ---------------------------------------------------------------------
C
  999 RETURN
      END
