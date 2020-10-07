      SUBROUTINE EBSHAP( S , ALPH , BETA , INIT , VALE )
C-----------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C!  Unormalized longitudinal shower distribution.Several arguments.
CKEY PHOTONS GAMMA SHAPE / INTERNAL
C           EBSHAP = S ** (Alpha) * exp(-Beta)
C
C   Input : S      Longitudinal abcissa expressed in rad.l.
C           ALPH   Alpha parameter of the shower.
C           BETA   Beta parameter of the shower.
C           INIT   Initialisation index.
C
C   Output: VALE   Shower profile at the S abcissa.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Called by EBPARA , EBSHOW
C ----------------------------------------------------
#ifndef DOC
      SAVE
C    XMAX : Protection for EXP function.
      DATA   XMAX / 100. /
C
      IF( INIT .NE. 0 ) THEN
C   Initialisation of the shower parameters.
        AM1 = ALPH - 1.
        BET = BETA
      ELSE
C
C ----- Unormalised longitudinal parametrisation.
        X = BET * S
        A = -X + AM1 * ALOG(S)
        IF(ABS(A) .LT. XMAX)  THEN
          VALE = EXP(A)
        ELSE
          VALE = 0.
        ENDIF
      ENDIF
      RETURN
      END
#endif
