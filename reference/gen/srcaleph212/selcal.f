      SUBROUTINE SELCAL ( IECAL )
C-----------------------------------------------------------------------
CKEY EDIR DEF CLASS17
C! Steering routine for calorimeter selection.
C-
C   Input   : None
C   Output  : IECAL  = Class 17 flag word
C-
C   Called by   : SELEVT
C   Calls  : ESUMW,TRIOFF,ESWEH,TRKHAD,ECAGET,REJBHA,TIZERO,WCLUS
C   Input banks : None
C-
C                                     Author: M.N.Minard  - 910400
C-----------------------------------------------------------------------
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C --
      DIMENSION XTOW ( 7)
      DATA ETHRS / 0.2/
      IECAL = 0
C --
C   Store wire energy
C --
      CALL ESUMW
C --
C     Performs off line pre-trigger
C --
      CALL TRIOFF ( IGOO )
      IF ( IGOO.GT.0 ) THEN
C --
C     Check energy threshold
C --
          CALL ESWEH ( EWTOT )
          IF ( EWTOT.GT.ETHRS ) THEN
            CALL TRKHAD(NGDTR,ECHRG)
            IF (NGDTR.GT.4) IECAL = 1
            IF ( NGDTR.LT.5) THEN
C --
C       Reject bhabhas
C --
                 CALL ECAGET ( XTOW , EWTOT )
                 CALL REJBHA ( XTOW , EWTOT , IQQT1 , IQQT2 ,IOK )
                 IF ( IOK.EQ.0.AND.(IQQT1+IQQT2).GT.0) THEN
                   IECAL =  1
                 ENDIF
             ENDIF
             IF ( NGDTR.LT.1 ) THEN
               IBCAL = IECAL
               IECAL = 0
C --
C     Check event timing
C --
               CALL TIZERO ( T0 , IT0 )
               IF ( IT0 . GT. 0 ) THEN
C --
C     Check cosmics
C --
                 CALL WCLUS ( NCLUS )
                 IF ( NCLUS.GE.2 ) THEN
                   IF ( IBCAL.NE.0) IECAL = IBCAL
                 ENDIF
               ENDIF
             ENDIF
           ENDIF
      ENDIF
      RETURN
      END
