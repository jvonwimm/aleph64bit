      SUBROUTINE EBPHCC( JREG )
C ----------------------------------------------------
C   AUTHOR   : R.Clifft 08/06/88
C               J.Badier   29/11/89
C! Analysis of a photon in a crack proper.
CKEY PHOTONS CRACK ENERGY POSITION / INTERNAL
C  Calculate a corrected energy and a position.
C
C   Input  :  JREG  Region index.
C
C     called by      EBRANC
C     calls          NONE
C
C     banks          NONE
C
C ----------------------------------------------------
      SAVE
      PARAMETER( EXTR = 1. , PTIT = .001 , YOF1 = .7 )
      PARAMETER( YOU1 = .65 , YOU2 = .45 )
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
      YOFS = .0
      YSI = YLIMIT(JREG) - 2.
      IF(JREG .LT. 3) YOFS = SIGN( YOF1 , YSI )
C
      EOUT = ENCRAT + EXTR
C
C *** Derive a photon impact coordinate orthogonal to the crack
C
      YOUT = 0.
      IF(RATIO2 .GT. PTIT) THEN
        IF(JREG .GT. 2) THEN
          YOUT = - ALOG(2. * RATIO2) / YOU1
        ELSE
          YOUT = - ALOG(2. * RATIO2) / YOU2
          YOUT = YOUT + YOFS
        ENDIF
        IF(YOUT .GT. YLIMIT(JREG)) YOUT = YLIMIT(JREG)
      ELSE
        YOUT = YLIMIT(JREG) / 2.
      ENDIF
      ENETOT = EOUT
      IF(R12STY .LT. PTIT) YOUT = YOFS
      YCOFIN = YOUT
      ENEERR = ENCRAT / 2.
      YCOERR = YLIMIT(JREG) / 2.
C
      RETURN
      END
