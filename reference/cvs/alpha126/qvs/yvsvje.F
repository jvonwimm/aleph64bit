      SUBROUTINE YVSVJE(EU,EDUDV,SGMX,NBU,UL,UH,NBV,VL,VH,VLF)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Puts vertex and jet angle resolution into likelihood function
C     CENTERED AT U=0, DU/DV=0
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION VLF(*)
C ------------------------------------------------------------------------------
C
C FIND BIN WIDTHS
      BWU=(UH-UL)/NBU
      BWV=(VH-VL)/NBV
C FIND CUTOFF
      GMN=-.5*SGMX**2
      IB=0
C LOOP OVER V BINS
      DO 250 IV=1,NBV
C FIND V VALUE
        V=VL+(IV-.5)*BWV
C COMBINE VERTEX ERROR AND JET ANGLE ERROR
        ERR2=EU**2+(V*EDUDV)**2
        RSIG=1./SQRT(ERR2)
C LOOP OVER U BINS
        DO 150 IU=1,NBU
C FIND U VALUE
          U=UL+(IU-.5)*BWU
C CALCULATE GAUSSIAN
          GSN=-.5*(U*RSIG)**2
C INCREMENT FUNCTION
          IB=IB+1
          VLF(IB)=VLF(IB)+MAX(GSN,GMN)
  150   CONTINUE
  250 CONTINUE
      RETURN
      END
#endif
