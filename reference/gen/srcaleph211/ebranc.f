      SUBROUTINE EBRANC( IFLG )
C ----------------------------------------------------
C   AUTHOR   :  R.Clifft   08/06/88
C               J.Badier   29/11/89
C! Steering routine for treatment of neutral clusters in crack regions
CKEY PHOTONS CRACK STEERING / INTERNAL
C   A photon is estimated in a crack proper ( IFLG = 2 ) or in the
C   module near the crack ( IFLG = 1 ).
C   No correction if no energy in pad rows next to crack and IFLG = 0
C   Barrel and endcap have not the same treatment.
C
C
C   Input     NONE
C
C   Output    IFLG   Treatment flag.
C
C     called by      EBRACK
C     calls          EBIMPC,EBCRAD ,EBECMD,EBPHCC,EBYPHI
C
C     banks          NONE
C
C ----------------------------------------------------
      SAVE
C            YCSC = distance of second pad row from crack
      PARAMETER( YCSC = 4.6 )
      COMMON/EBENEC/ENCRAT,ENECRA(2),ENECA1(2),EESTYA(3),EESTYB(3),
     1        RATIO1,RATIO2,R11STY,R12STY,
     2        ITRWEB,JFCLEB, KODEEB(4),NREGEB(3),SINCEB,
     3        ENETOT,ENEERR,YCOFIN,YCOERR,PHICOR,
     4        YLIMIT(3)
C
      PARAMETER ( ETHRL = 0.03 , CECT1 = 0.0121 , DISFE = 255. )
      PARAMETER ( CECT2 = 0.1904 , PETIT = .0001 )
      PARAMETER ( YLIM1 = 1.8 , YLIM2 = 3.2 , YLIM3 = 1.3 )
C
C *** left or right side of endcap or barrel but not overlap
C *** Determine crack region
C
      IF( NREGEB(1) .EQ. 2 .AND. ENCRAT .GT. .0 .AND.
     +    RATIO1 + RATIO2 .LT. PETIT )        THEN
C
C *** No correction if no energy in pad rows next to crack.
C
        ENETOT = ENCRAT
        YCOFIN = YCSC
C   Set output flag to 0
        IFLG = 0
      ELSE
C
        CALL EBIMPC( IFLG )
C
        IF( IFLG .EQ. 1 ) THEN
C
C *** Hit module close to crack , IFLG = 1
C
          IF( NREGEB(1) .EQ. 2 ) THEN
            CALL EBCRAD
          ELSE
            CALL EBECMD
          ENDIF
        ELSE
C
C *** Hit crack proper , IFLG = 2
C
C   JREG index.
      IF( NREGEB(1) .EQ. 2 ) THEN
        JREG = 3
      ELSE
        IF( KODEEB(3) .GT. 2 ) THEN
          JREG = 2
        ELSE
          JREG = 1
        ENDIF
          IF( NREGEB(2) .EQ. 3 ) JREG = 3 - JREG
      ENDIF
          CALL EBPHCC( JREG )
C
        ENDIF
      ENDIF
C
C *** Call service routine to handle coordinates.
C
      CALL EBYPHI( YCOFIN , PHICOR )
C
   98 CONTINUE
      RETURN
      END
