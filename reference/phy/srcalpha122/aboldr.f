      SUBROUTINE ABOLDR (IRUN)
CKEY ABREAD READ BOS /INTERNAL
C-----------------------------------------------------------------------
C  AUTHOR :      H. Albrecht            Mar 90
C
C!  User supplied routine. Called from ABRSEL before a new run starts.
C   Exactly : when a run record with a new run number is encountered.
C   IRUN refers to the old run. The old run records banks are still
C   available; the new ones are not yet read in.
C
C   Alpha version :
C     IRUN < 0 : ABOLDR called from QMREAD
C-----------------------------------------------------------------------
      DATA IROLD /0/
C
      JRUN = IABS (IRUN)
      IF (IROLD .EQ. JRUN)  GO TO 90
      CALL QMNEWR (IROLD, JRUN)
      IF (IRUN .GT. 0)  CALL QWMESE
     +  ('_ABOLDR_ No event processed for this run')
      IROLD = JRUN
 90   CONTINUE
      END
