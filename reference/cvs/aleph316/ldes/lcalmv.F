      INTEGER FUNCTION LCALMV (LSTAK)
C ---------------------------------------------------
C - F.Ranjard - 890428
C! return the number of MeV per LCAL ADC count
C  information is taken from the data base
CKEY LCALDES LCAL ADC ENERGY / USER
C - Input   : none
C - Output  : LSTAK(3) / I  number of MeV per ADC count
C                           for the 3 stacks
C             LCALMV        return code :
C                           =0 means information not avalaible
C - Banks   : none
C
#ifndef DOC
      SAVE
      INTEGER LSTAK(*)
C ------------------------------------------------------------
      LSTAK(1) = 32.
      LSTAK(2) = 32.
      LSTAK(3) = 32.06
C
      LCALMV = 1
C
      RETURN
      END
#endif
