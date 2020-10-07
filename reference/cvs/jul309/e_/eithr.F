      INTEGER FUNCTION EITHR(IT,IP,IS)
C--------------------------------------------------
C! Return ON-LINE ROC threshold in Kev for tower iteta,iphi,istack
CKEY ECAL THRESHOLD   / JULIA  USER
C   B.Bloch-Devaux January 90
C
C  Structure : FUNCTION
C             External references: EBTZTH  ( ALEPHLIB)
C             Comdecks references:none
C  Input : IT  / Integer  = theta index of tower
C          IP  / Integer  = phi   index of tower
C          IS  / Integer  = stack index of tower
C  Output : EITHR / Integer  = threshold value in Kev
C  Banks  : none
C
C  Action : returns on-line ROC threshold used for this  storey
C?
C!======================================================================
#ifndef DOC
      DIMENSION ETHR(3,3)
C
      EITHR = 0
      IF ( IT.LE.0 ) RETURN
      IF (IT.GT.228 ) RETURN
      ISC = 1
      IF ( IT.GE.51 ) ISC = 2
      IF ( IT.GE.179) ISC = 3
C
      CALL EBTZTH(ETHR,IRC)
      EITHR = NINT(ETHR (IS,ISC)*1000.)
      RETURN
      END
#endif
