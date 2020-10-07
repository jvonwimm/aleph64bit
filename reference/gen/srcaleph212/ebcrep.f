      SUBROUTINE EBCREP( ECLS , CLUR  )
C ----------------------------------------------------
C   AUTHOR   :  R.Clifft   08/06/88
C               J.Badier   18/07/89
C               J.Badier   29/11/89
C! Set initial information in the EBENEC common.
CKEY PHOTONS CRACK IDENTIFICATORS / INTERNAL
C
C        RATIO1  energy of last pad row/total energy ,for ECAL module
C                wuth greater energy
C        RATIO2  energy of lesser energy module/total cluster energy
C
C
C   Input     ECLS(3,3)
C             CLUR(3,3,3)
C
C     called by      EBRACK
C     calls          EBCOSI
C
C     banks          NONE
C
C ----------------------------------------------------
      SAVE
      DIMENSION ECLS(3,*) , CLUR(3,3,*)
      COMMON/EBENEC/ENCRAT,ENECRA(2),ENECA1(2),EESTYA(3),EESTYB(3),
     1        RATIO1,RATIO2,R11STY,R12STY,
     2        ITRWEB,JFCLEB, KODEEB(4),NREGEB(3),SINCEB,
     3        ENETOT,ENEERR,YCOFIN,YCOERR,PHICOR,
     4        YLIMIT(3)
C
      PARAMETER ( ETHRL = 0.03 , CECT1 = 0.0121 , DISFE = 255. )
      PARAMETER ( CECT2 = 0.1904 , PETIT = .0001 )
      PARAMETER ( YLIM1 = 1.8 , YLIM2 = 3.2 , YLIM3 = 1.3 )
      DATA KDEB / 0 /
C
      IF( KDEB .EQ. 0 ) THEN
        YLIMIT(1) = YLIM1
        YLIMIT(2) = YLIM2
        YLIMIT(3) = YLIM3
        KDEB = 1
      ENDIF
C
C *** Zero variables per event
C
      ENECRA(1) = 0.
      ENECRA(2) = 0.
      ENECA1(1) = 0.
      ENECA1(2) = 0.
      RATIO1 = 0.
      RATIO2 = 0.
      R11STY = 0.
      R12STY = 0.
C
C *** Get energies. Max and min module energies for crack cluster
C *** and corresponding storey energies.
C
      DO 5 I = 1,3
        ENECRA(1) = ENECRA(1) + ECLS(I,1)
        ENECRA(2) = ENECRA(2) + ECLS(I,2)
        EESTYA(I) = ECLS(I,1)
        EESTYB(I) = ECLS(I,2)
C
C *** Energy of crack adjacent pad row in max energy module.
C
        IJ=2
        IF(KODEEB(3) .EQ. 3) IJ = 3
        IF(KODEEB(3) .EQ. 2) IJ = 1
C
        DO 6 J = 1 , 3
          ENECA1(1) = ENECA1(1) + CLUR(I,IJ,J)
    6   CONTINUE
C
    5 CONTINUE
C
C *** Apply nominal energy thresholds
C
      IF(ENECRA(1) .LT. ETHRL) ENECRA(1) = 0.
      IF(ENECRA(2) .LT. ETHRL) ENECRA(2) = 0.
      IF(ENECA1(1) .LT. ETHRL) ENECA1(1) = 0.
      DO 2 I = 1 , 3
      IF(EESTYA(I) .LT. ETHRL) EESTYA(I) = 0.
      IF(EESTYB(I) .LT. ETHRL) EESTYB(I) = 0.
    2 CONTINUE
C   ENCRAT may be different from the raw energy.
      ENCRAT = ENECRA(1) + ENECRA(2)
C
C *** Calculate energy ratios for crack clusters.
C
      IF(ENECRA(1) .LE. PETIT) GO TO 3
      RATIO1 = ENECA1(1) / ENECRA(1)
      RATIO2 = ENECRA(2) / ENCRAT
C
C *** Stack energy ratios for max energy module.
C
      R11STY = EESTYA(1) / ENECRA(1)
      R12STY = (EESTYA(1) + EESTYA(2)) / ENECRA(1)
    3 CONTINUE
      RETURN
      END
