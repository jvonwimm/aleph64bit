      REAL SIFOXA,SIDIFO
      INTEGER ISITHR,ISIDIS
      COMMON/SITRCO/ISITHR(4),ISIDIS(2),SIFOXA,SIDIFO
C
#ifndef DOC
C     ISITHR(4) : thresholds in ADC counts for very low , low ,
C                 high , very high triggers on odd/even sums
C     ISIDIS(2) : pattern of disabled segments in sical A and B
C     SIFOXA    : # if SIXA ADC count per SIFO ADC count
C     SIDIFO    : # of SIFO ADC count per Mev
#endif
