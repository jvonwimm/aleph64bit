CKEY MUCAL MUON  CALOBJ / USER
C! give the relation muon-calobjects in HCAL

   Long writeup : ALEPH 89-123/SOFTWR 89-16,G.Bagliesi et al.
   Since an Aleph Note on the subject is available, here we will
   describe just the usage of the package. The idea is to
   give for each calobject a probability of being associated
   to a muon (not necessarily isolated). The probability
   is calculated using the HCAL information only, hence can
   be used succesfully even when the TPC or Muon Chambers
   are not available.
   There is a single routine (MUCALO) interfacing the package to
   the user program. For the calling sequence and arguments
   please look at the routine header. If you want to use it
   naively look at the following example, a POT analysis
   routine that accesses the probabilty for prompt muon
   for each calobject.
                            *** EXAMPLE ***
      SUBROUTINE CALOMU
      ......................
      ......................
      PARAMETER(MAXL=20)
      DIMENSION NCAVEC(MAXL)
      ......................
      ......................
      KCALO=NLINK('PCOB',0)
      IF(KCALO.EQ.0) GO TO 999
      NCALO=LROWS(KCALO)
      DO 10 ICAL=1,NCALO
         CALL MUCALO(ICAL,PR1,ERPR1,PR2,ERPR2,NCAL,NCAVEC,MAXL,IER)
         IF(IER.EQ.-1) THEN
C
C ECAL ONLY SKIP IT
C
         ELSEIF(IER.EQ.0) THEN
C
C TAKE PR2 AND ERPR2 AND DO YOUR ANALYSIS
C
         ELSEIF(IER.GT.0)
C
C ERROR : REPORT IT TO THE AUTHORS
C
         ENDIF
 10   CONTINUE
 999  END
                            *** END EXAMPLE ***
