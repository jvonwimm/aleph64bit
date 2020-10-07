C! FLAGS
      INTEGER JDBDRF,JHISRF,JPRSRF,JPRERF,MDET, IDEBBC, IDEBTC
      LOGICAL FDEBRF,FDETRF,FREPRO,FPASS0
      PARAMETER (MDET=17)
      PARAMETER (NCHDET=4)
      CHARACTER*(NCHDET) TNAMRD
      COMMON /RFLAGS/FDEBRF,FDETRF(MDET),FREPRO,FPASS0,JDBDRF(MDET),
     &        JHISRF(MDET),JPRSRF(MDET),JPRERF(MDET),JCMORF,
     &        JEBIRF(2),JFBIRF(2),TNAMRD(MDET), IDEBBC, IDEBTC
#if defined(DOC)
C
C MDET    = Number of detectors
C NCHDET  = Number of characters for detector names
C
C
C FDEBRF  = .true. if general debug requested
C FDETRFi = array of logical flags: type of reconstruction
C       1 - VDET
C       2 - ITC
C       3 - TPC
C       4 - ECAL            (SEE PARAMETERS JULXX IN RPARAC)
C       5 - LCAL
C       6 - SATR
C       7 - HCAL
C       8 - MUON
C       9 - CALO
C      10 - SKEL
C      11 - YREC
C      12 - YTOP
C      13 - EFLO
C      14 - BCAL
C      15 - FALCON
C      16 - BOM
C      17 - SICAL
C
C FREPRO  = True if POT-POT reprocessing
C FPASS0  = True if PASS0 mode
C JDBDRFi = array of integers: debug level for detectors
C JHISRFi = array of integers: histogram level for detectors
C JPRSRFi = Debug levels for start of JOB printing per detector
C JPRERFi = Debug levels for end of JOB printing per detector
C JCMORF = current module - set in RNXMOD
C JEBIRF = Bit map of non fatal errors (1 bit per module)
C JFBIRF = Bit map of fatal errors (1 bit per module)
C-------> DETECTOR NAMES
C
C TNAMRDi  = Detector name for detector i (numbering as FDETRF)
C
C  IDEBBC = First evt to debug
C  IDEBTC = Last evt to debug
C
#endif
