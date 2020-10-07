      SUBROUTINE YVSML2(NBU,UL,UH,NBV,VL,VH,VLF,
     > UM,EUM,VM,EVM,UVCR,VLFM)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Finds maximum of 2-dimensional likelhood function
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  NBU,UL,UH ARE NUMBER OF BINS, LOWER AND UPPER LIMITS
C        FOR FIRST VARIABLE
C  *  NBU,UL,UH ARE SAME FOR SECOND VARIABLE
C  *  VLF(NBU,NBV) IS ARRAY CONTAINING LIKELIHOOD FUNCTION
C  Output Arguments :
C      (FROM LOCAL PARABOLA FIT TO 9 BINS AT PEAK)
C  *  UM IS LOCATION OF MAXIMUM IN FIRST VARIABLE
C  *  EUM IS ERROR (NEGATIVE IF NO REAL MAXIMUM FOUND)
C  *  VM,EVM ARE SAME FOR SECOND
C  *  UVCR IS NORMALIZED CORRELATION
C  *  VLFM IS VALUE AT MAXIMUM
C
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION VLF(*)
      DIMENSION PAR(6)
C ------------------------------------------------------------------------------
C
C FIND PEAK BIN
      WMAX=VLF(1)-1.
      IB=0
      DO 250 IBV=1,NBV
        DO 150 IBU=1,NBU
          IB=IB+1
          IF (VLF(IB) .GT. WMAX) THEN
            IPEAK=IB
            WMAX=VLF(IB)
          ENDIF
  150   CONTINUE
  250 CONTINUE
      MBV=(IPEAK-1)/NBU+1
      MBU=IPEAK-(MBV-1)*NBU
C
C KEEP AWAY FROM EDGES FOR PARABOLOIDS
      MBU=MAX(2,MBU)
      MBU=MIN(NBU-1,MBU)
      MBV=MAX(2,MBV)
      MBV=MIN(NBV-1,MBV)
C FIND LOCAL PARABOLOID
      CALL YVSPAR(VLF,NBU,MBU,MBV,PAR)
C FIND PEAK OF PARABOLOID (STILL BIN UNITS)
      CALL YVSPRM(PAR,DU,EU,DV,EV,UVCR,VLFM)
C FIND BIN WIDTHS
      BWU=(UH-UL)/NBU
      BWV=(VH-VL)/NBV
C ADD OFFSETS, SCALE ERRORS
      UM=UL+(MBU-.5+DU)*BWU
      VM=VL+(MBV-.5+DV)*BWV
      EUM=EU*ABS(BWU)
      EVM=EV*ABS(BWV)
      RETURN
      END
#endif
