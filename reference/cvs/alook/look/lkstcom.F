      SUBROUTINE LKSTCOM(PRECOM,NEWCOM,COMNUM)
C -----------------------------------------------------
C! store previous commands to recall them eventually
C - Input: PRECOM  / A  = array to store previous commands
C          NEWCOM  / A  = new command to be stored
C          COMNUM  / I  = number of commands already stored
C -----------------------------------------------------
#ifndef DOC
      CHARACTER*80 PRECOM(20),NEWCOM
      INTEGER COMNUM
C -----------------------------------------------------
      IF (COMNUM.EQ.0) THEN
          COMNUM=COMNUM+1
          PRECOM(COMNUM)=NEWCOM
      ELSEIF (COMNUM.LT.20) THEN
          IF (PRECOM(COMNUM).NE.NEWCOM) THEN
              COMNUM=COMNUM+1
              PRECOM(COMNUM)=NEWCOM
          ENDIF
      ELSE
          IF (PRECOM(20).NE.NEWCOM) THEN
              DO 10 I=2,20
 10           PRECOM(I-1)=PRECOM(I)
              PRECOM(20)=NEWCOM
          ENDIF
      ENDIF
      RETURN
      END
#endif
