      SUBROUTINE YVSML1(NB,UL,UH,VLF,UM,EUM,VLFM)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Finds maximum of 1-dimensional likelhood function
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  NB,UL,UH ARE NUMBER OF BINS, LOWER AND UPPER LIMITS
C  *  VLF() IS ARRAY CONTAINING LIKELIHOOD FUNCTION
C  Output Arguments :
C      (FROM LOCAL PARABOLA FIT TO 3 BINS AT PEAK)
C  *  UM IS LOCATION OF MAXIMUM
C  *  EUM IS ERROR (NEGATIVE IF NO REAL MAXIMUM FOUND)
C  *  VLFM IS VALUE AT MAXIMUM
C
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION VLF(*)
C ------------------------------------------------------------------------------
C FIND PEAK BIN (AVOID END BINS)
      WMAX=VLF(2)-1.
      DO 150 IB=2,NB-1
        IF (VLF(IB) .GT. WMAX) THEN
          IPEAK=IB
          WMAX=VLF(IB)
        ENDIF
  150 CONTINUE
C FIND COEFFICIENTS OF LOCAL PARABOLA (BIN UNITS)
C A*DU**2+B*DU+C
      C=VLF(IPEAK)
      B=.5*(VLF(IPEAK+1)-VLF(IPEAK-1))
      A=VLF(IPEAK+1)-B-C
C FIND DISTANCE TO PARABOLA MAXIMUM
C AND ERROR FROM CURVATURE (BOTH IN BIN UNITS)
      IF (A .NE. 0.) THEN
        DU=-.5*B/A
C MAKE ERROR NEGATIVE IF REALLY A MINIMUM (CAN HAPPEN AT EDGES)
        EU=SIGN(SQRT(ABS(.5/A)),-A)
      ELSE
C NO CURVATURE, DON'T MOVE
        DU=0.
C SET ERROR TO FULL BIN SIZE
        EU=-1.
      ENDIF
C VALUE AT PEAK
      VLFM=A*DU*DU+B*DU+C
C CONVERT TO REAL UNITS
      BW=(UH-UL)/NB
C PEAK POSITION
      UM=UL+(IPEAK-.5+DU)*BW
C ERROR
      EUM=EU*ABS(BW)
      RETURN
      END
#endif
