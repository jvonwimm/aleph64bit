C! DETECTOR NAMES
      PARAMETER (NCHDET=4,MDETS=17)
      COMMON /RDETNM/TNAMRD(MDETS)
      CHARACTER*(NCHDET) TNAMRD
#if defined(DOC)
C NCHDET  = Number of characters for detector names
C MDETS   = Number of detectors
C TNAMRD  = Detector name for detector i (numbering as FDETRF)
C
#endif
