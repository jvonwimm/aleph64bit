      INTEGER FUNCTION ALGTDB(LBASE,LIST,KRUN)
C --------------------------------------------------------------------
C! Load  list LIST of Bank names from the Data Base for run IRUN
C   B. Bloch -Devaux    august 87      modified by - F.Ranjard - 890802
C                                                    F.Ranjard - 891001
C                                                    F.Ranjard - 900626
C             to introduce a data card bank number NRDC.ne.0
C                                                    F.Ranjard - 910308
C             to remove LTC file and introduce a check on the validity
C             range of the data base
C
C  Input  :   LBASE = data base logical unit
C             LIST  = list of banks names to be accessed from data base
C                     single name or list of names
C             KRUN  = current run number
C                     make sure that the bank is there
C
C  Ouput : ALGTDB = > 0  if existing bank(s) are still valid
C                   = 0  if error occurs ( no valid bank found)
C                   < 0  if one or more existing banks were reloaded
C
C  Entry : ALGTD1 (INEW) will set the data card bank number
C                        NRDC = INEW
C                        ALGTD1 = previous data card bank number
C
C     At 1st entry get the ADBR,NR=0 bank which contains the setup
C     code.
C     At next entry check whether the run number has changed.
C     IF yes THEN
C       Check that the opened data base is the right one.
C     ELSE
C       call AOPDBS again which will use the setup code to open
C       the right data base
C     ENDIF
C
C     the default priority is: data cards, data base, tape
C     a bank can be put on data cards with NR=NRDC. It is used all
C     over the job.  NRDC =-1 by default , it can be reset by  the
C     user in calling ADBSWP (IOLD,INEW) after reading data cards.
C     if a valid bank is found on the data base it is used even
C     if the bank is on the tape.
C     The priority can be changed to anything provided the user put
C     a 'UDAF' data card and gives an INTEGER FUNCTION USGTDB.
C
C     If the run is still in the validity range of the
C     bank , the already loaded bank is kept.
C     If the run requires a new bank to be loaded, the
C     old bank will be dropped before loading the new one.
C      So only one bank of a given name is present in the
C     Bos common at a given time.
C--------------------------------------------------------------
      CHARACTER*(*)  LIST,NAME*4
      PARAMETER (LTNAM = 400)
      CHARACTER*4 TNAM(LTNAM)
      DIMENSION LRUN(LTNAM)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER USGTDB, ALGTD1, GTDBAS
      SAVE NCDAF, TNAM
      DATA MRUN/0/, NUDAF/0/
      DATA IPRUN/-1/
      DATA NRDC /-1/, NRDEF /-999/, LRUN /LTNAM*-999/
C ----------------------------------------------------------------------
      IF (NUDAF.EQ.0) THEN
         NCDAF = NAMIND('CDAF')
         NUDAF = NAMIND('UDAF')
      ENDIF
      IRUN = IABS (KRUN)
C
C - check validity range of the data base for real data
      IF (IW(NCDAF).EQ.0 .AND. IPRUN.NE.IRUN .AND. IRUN.GE.2001) THEN
         IPRUN = IRUN
         IGET = GTDBAS (LBASE,IRUN)
         IF (IGET.NE.0) THEN
            ALGTDB = 0
            RETURN
         ENDIF
      ENDIF
C
C - IF a 'UDAF' data card is there THEN
C      the priority is: data cards, tape, data base
C   ELSE
C      the priority is: data cards, data base, tape
C   ENDIF
C
      IF (IW(NUDAF).NE.0) THEN
         ALGTDB = USGTDB (LBASE,LIST,IRUN)
         RETURN
      ENDIF
C
C - NO 'UDAF' data card THEN
C
C -    The priority is as follow: data cards, data base, tape
C      if a valid bank is on the data base it is used in priority
      ISTAT= 1
      IOLD = 1
C                     Analyse list of Banks
      IMAX = (LNBLNK(LIST)+3)/4
      I=0
  2   I=I+1
      IF (I.LE.IMAX) THEN
         NAME=LIST((I-1)*4+1:I*4)
C
C     get the bank# valid for run# IRUN
         NEW = NDANR (LBASE,NAME,'LE',IRUN)
C
C     does a bank with this name exist already ?
         DO 3 M=1,MRUN
            IF (NAME .EQ. TNAM(M)) GOTO 4
 3       CONTINUE
         M = MRUN + 1
         TNAM(M) = NAME
         MRUN = MRUN + 1
 4       CONTINUE
         NOLD = LRUN(M)
C
         IF (NOLD.EQ.NRDEF)THEN
C        it is a new bank name, check wheither the bank is there
            IND = IW(NAMIND(NAME))
            IF (IND .GT. 0) THEN
C           the bank exists: keep track of it if it comes from d.c.
                IF (IW(IND-2).EQ.NRDC) THEN
                   IOLD = -1
                   NOLD=IW(IND-2)
                   LRUN(M) = NOLD
                ENDIF
            ENDIF
         ENDIF
C
C     drop existing banks which are no longer valid
         IND = NAMIND(NAME) + 1
 5       IND = IW(IND-1)
         IF (IND .NE. 0) THEN
            NR = IW(IND-2)
C           keep bank which comes from d.c. (NR=NRDC)
            IF (NR.EQ.NRDC) GOTO 5
C           drop bank which comes from tape
            IF (NR.NE.NOLD) THEN
               IDRP = NDROP(NAME,NR)
C           drop bank which is no longer valid
            ELSEIF (NR.NE.NEW) THEN
               IDRP = NDROP(NAME,NR)
            ENDIF
            GOTO 5
         ENDIF
C
C     if the bank is on data card keep it
         IF (NOLD.EQ.NRDC) GOTO 2
C
C     if there is a valid bank then load it
         IF (NEW .NE. 0) THEN
            IND = MDARD (IW,LBASE,NAME,NEW)
            IOLD=-1
            IF (IND .GT. 0) THEN
               LRUN(M) = NEW
            ENDIF
         ELSE
            ISTAT = 0
         ENDIF
C
C     get next bank name in the list
         GO TO 2
      ENDIF
C
      ALGTDB=ISTAT*IOLD
      RETURN
C
C - Entry point to reset NRDC (data card bank number)
C
      ENTRY ALGTD1 (INEW)
      ALGTD1 = NRDC
      NRDC = INEW
      RETURN
C
      END
