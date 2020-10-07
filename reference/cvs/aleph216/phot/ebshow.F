      REAL FUNCTION EBSHOW(S)
C-----------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C!  Unormalized longitudinal shower distribution.One argument.
CKEY PHOTONS SHAPE / INTERNAL
C   Used for integrations
C
C   Input :   S    Longitudinal abcissa in radiation lengthes.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls EBSHAP which has to be initialised in order to define
C   the showers parameters.
C   Called by EBPARA through GAUSS.
C ----------------------------------------------------
#ifndef DOC
      SAVE
C
      IF(S .GT. 0.) THEN
        CALL EBSHAP( S , ALPH , BETA , 0 , EBSHOW )
      ELSE
        EBSHOW = 0.
      ENDIF
      RETURN
      END
#endif
