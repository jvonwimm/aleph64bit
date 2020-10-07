      LOGICAL FUNCTION SICAOK(KRUN)
C----------------------------------------------------------------------
C!  - Checks HV bits and Trigger enabled for Sical analysis
C!
C!
C!    Author:     B. Bloch-Devaux   11-May-1993
C!          Check X1RG if XTOP missing - April 29 , 1994
C!                B. Bloch-Devaux  November 97
C!          use XLUMOK from alephlib for complete checks
C!                B. Bloch-Devaux  March 2K
C!          use XLUM2K from alephlib for year >= 2000
C!
C!    Input:      - KRUN : current Run number
C!
C!    Output:     SICAOK is TRUE if all conditions ok
C?
C!======================================================================
#ifndef DOC
      LOGICAL xlum2k
      INTEGER GTSTUP
      EXTERNAL GTSTUP,xlum2k
      DATA IFIR / 0/
C
      IF ( IFIR.EQ.0) THEN
        IFIR = 1
        ISITP = GTSTUP('SI',KRUN)
      ENDIF
C
      SICAOK = .FALSE.
C   Monte Carlo
      IF (KRUN.LE.2000   ) THEN
         IF ( ISITP.GT.0 )  SICAOK = .TRUE.
         GOTO 999
      ENDIF
      SICAOK = xlum2k(dum)
 999  RETURN
      END
#endif
