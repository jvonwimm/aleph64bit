      SUBROUTINE YVSPAR(VLF,NBU,MU,MV,PAR)
CKEY  QVSRCH / INTERNAL
C ----------------------------------------------------------------------
C! Finds Paraboloid parameters for 2-d sampled distribution
C     VLF(MU+DU,MV+DV)
C      =  P1*DU**2+P2*DU+P3
C        +P4*DV**2+P5*DV+P6*DU*DV
C ----------------------------------------------------------------------
      DIMENSION VLF(*),PAR(6)
C ----------------------------------------------------------------------
C CENTRAL BIN
      IBIN=MU+(MV-1)*NBU
C FIRST DO U-ONLY TERMS
      P3=VLF(IBIN)
      P2=.5*(VLF(IBIN+1)-VLF(IBIN-1))
      P1=VLF(IBIN+1)-P2-P3
C THEN V-ONLY
      P5=.5*(VLF(IBIN+NBU)-VLF(IBIN-NBU))
      P4=VLF(IBIN+NBU)-P3-P5
C     THEN CROSS TERM
      TPP=VLF(IBIN+NBU+1)-(P1+P2+P3+P4+P5)
      TMM=VLF(IBIN-NBU-1)-(P1-P2+P3+P4-P5)
      TPM=VLF(IBIN+NBU-1)-(P1-P2+P3+P4+P5)
      TMP=VLF(IBIN-NBU+1)-(P1+P2+P3+P4-P5)
      P6=.25*(TPP+TMM-TMP-TPM)
C
      PAR(1)=P1
      PAR(2)=P2
      PAR(3)=P3
      PAR(4)=P4
      PAR(5)=P5
      PAR(6)=P6
      RETURN
      END
