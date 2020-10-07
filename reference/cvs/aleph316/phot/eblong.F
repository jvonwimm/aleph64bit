      SUBROUTINE EBLONG( STAC , TETA , PHI , ITY2 , ITY3 , SHAP , IER )
C ---------------------------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C!  Longitudinal analysis of a neutral cluster.
CKEY PHOTONS GAMMA LONGITUDINAL / USER
C
C   A first e.m./hadron separation is obtained from the configuration
C   of the hitted stacks. Later a more refined treatment leads to
C   identification parameters. A set of standard cut'off is proposed.
C   It is clear that they are physic dependant.
C
C
C   Input  :  STAC(3)   3 stacks contents.
C             TETA      Polar angle (radians)
C             PHI       Azimuthal angle (radians)
C
C   Output :  ITY2      Identification indice.
C                       0 : Electromagnetic.
C                       1 : Hadronic
C                       2 : Ambiguous
C             ITY3      Gives the available identifiers.
C                       0 : SHAP(1) and SHAP(2) are available.
C                       1 : SHAP(1) is available.
C                       2 : SHAP(2) is available.
C                       3 : The two identifiers are not available.
C             SHAP      Identification parameters.
C             SHAP(1)   1 : Materialisation depth.
C             SHAP(2)   2 : Deviation from the mean shape.
C             IER       Error code.
C                       0 : No error
C                       1 : Missing bank.
C                       2 : Too many storeys
C                       3 : No storey in this cluster.
C                       4 : Energy > EMIN = .05
C                       5 : Energy < EMIN = 80.
C                      -1 : Truncated endcap , 3 stacks.
C                      -2 : Truncated endcap , 2 stacks.
C                      -3 : Overlap near endcap , 2 stacks.
C                      -4 : Overlap near endcap , 3 stacks.
C                      -5 : Overlap near barrel.
C                      -6 : Truncated barrel , 2 stacks.
C                      -7 : Truncated barrel , 3 stacks.
C                     -11 : Crack.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls EBLPST , EBPRGA
C   Called by USER
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION  STAC(*) , SHAP(*) , KODE(4) , NREG(3)
C    XMAX : Protection for EXP function.
      PARAMETER( XMAX = 50. )
C   Probability thresholds.
      PARAMETER ( CUT1 = .01 , CUT2 = .05 , GSEF = 7./9. )
      PARAMETER ( CUT3 = 7.5 , SCUT = 5. )
C -----------------------------------------------------------------
      ENER = STAC(1) + STAC(2) + STAC(3)
C   Configuration probability.
      CALL EBLPST( STAC , TETA , PHI , KLAS , PROB , JER )
C   Missing banks or input out of range.
      IF( JER .GT. 0 ) GO TO 101
      KER = JER
C   Raw rejection.
      IF( ( KER  .EQ. 0   .OR.   KLAS .GE. 4    .OR.
     +    ( KLAS .EQ. 1 .AND. KER .NE. -3 ) )   .AND.
     +    PROB .LT. CUT1 / ENER                ) GO TO 1
C   One needs a more refined analysis.
      CALL EBPRGA( STAC , TETA , PHI , ITY3 , SHAP , JER )
      IF( JER .GT. 0 ) GO TO 101
C   No estimated parameters.
      IF( ITY3 . EQ. 3 ) GO TO 2
C   Threshold to eleminate the e.m. hypothesis.
      PCUT = CUT2 / ENER
C   Available parameters.
      IF( ITY3 .LT. 2 ) THEN
C   At least the first identifier.
        IF( ABS(SHAP(1)) .GT. XMAX ) SHAP(1) = XMAX
        P1 = EXP( - GSEF * SHAP(1) )
        IF( ITY3 .EQ. 0 ) THEN
C   Two identifiers.
          IF( ABS(SHAP(2)) .GT. XMAX ) SHAP(2) = XMAX
          P2 = EXP( - .5 * SHAP(2) ** 2 )
          IF( P1 .LT. PCUT .OR. P2 .LT. PCUT ) THEN
C   It is not e.m.
            ITY2 = 1
C   Energy less than 5 Gev excepted
            IF( ABS( SHAP(2) ) .LT. CUT3 - ENER ) ITY2 = 2
          ELSE
            IF( P1 .GT. CUT2 .AND. P2 .GT. CUT2 ) THEN
C   It is purely e.m.
              ITY2 = 0
            ELSE
C   It is ambiguous.
              ITY2 = 2
            ENDIF
          ENDIF
        ELSE
C   Estimated materialisation depth alone.
C   Invalid in the overlap.
          IF( KER .EQ. -3 ) GO TO 101
          IF( P1 .LT. PCUT ) THEN
C   It is not e.m.
            ITY2 = 1
          ELSE
            IF( P1 .GT. CUT2 ) THEN
C   It is purely e.m.
              ITY2 = 0
            ELSE
C   It is ambiguous.
              ITY2 = 2
            ENDIF
          ENDIF
        ENDIF
        IER = 0
      ELSE
C   Non convergence en classe 7.
        IER = 0
        ITY2 = 1
      ENDIF
      GO TO 98
C   Hadron decision from the configuration.
    1 CONTINUE
      ITY3 = 3
      SHAP(1) = 0.
      SHAP(2) = 0.
    2 CONTINUE
      IF( JER .NE. 0 .OR. KER .NE. 0 ) GO TO 101
C   It is an hadronic shape if no error code.
      ITY2 = 1
      IER = 0
      GO TO 98
C   Error : ambiguous without identification.
  101 CONTINUE
      ITY2 = 2
      ITY3 = 3
      SHAP(1) = 0.
      SHAP(2) = 0.
      IER = JER
   98 CONTINUE
C   One keeps the overlap proximity information.
      IF( IER .EQ. 0 .AND. JER .LT. 0 ) IER = JER
      IF( IER .EQ. 0 ) THEN
        CALL EBCDRG( TETA , PHI , ITRW , JFCL , KODE , NREG , KER )
        IF( KODE(3) .NE. 0 ) IER = -11
      ENDIF
      RETURN
      END
#endif
