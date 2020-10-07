      SUBROUTINE YVSLF1(NB,UL,UH,U,EU,EUMX,VLF)
CKEY  QVSRCH / INTERNAL
C ----------------------------------------------------------------------
C! Adds track to 1-dimension log-likelihood distribution vlf
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  NB,UL,UH ARE NUMBER OF BINS, LOW AND HIGH LIMITS
C  *  U,EU ARE VALUE AND SIGMA
C  *  EUMX IS HOW MANY SIGMAS THE DISTRIBUTION REMAINS GAUSSIAN
C  *  VLF(NB) IS ARRAY CONTAINING LOG-LIKELIHOOD FUNCTION
C  Output Argument :
C     ALTERS VLF()
C
C ----------------------------------------------------------------------
      DIMENSION VLF(*)
C ----------------------------------------------------------------------
C FIND LIMITING LOG-LIKELIHOOD VALUE
      UMX=-.5*EUMX**2
C INVERSE ERROR**2
      REU=1./EU
C BIN WIDTH
      BW=(UH-UL)/NB
C STARTING POINT FOR BIN CENTERS
      UC=UL-.5*BW
C LOOP OVER BINS
      DO 150 IB=1,NB
C UPDATE BIN CENTER
        UC=UC+BW
C GAUSSIAN LOG-LIKELIHOOD
        GSN=-.5*((U-UC)*REU)**2
C INCREMENT WITH LARGER OF THIS AND LIMIT
        VLF(IB)=VLF(IB)+MAX(GSN,UMX)
  150 CONTINUE
      RETURN
      END
