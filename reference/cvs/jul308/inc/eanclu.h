      COMMON /EANCLU/ ICLNEA,ICHREA,NBMOEA,LMOLEA(3),KLASEA
     +,ECLTEA,ECLSEA(3,3),XYZBEA(3),NBDEEA,LDETEA(2),ICREEA
     +,NBRGEA,LREGEA(5),ENRGEA(5)
#if defined(DOC)
C!  ICLNEA :  Cluster number.
C!  ICHREA :  Number of associated tracks.
C!  NBMOEA :  Number of hit modules ( < 4 )
C!  LMOLEA :  Module numbers
C!  KLASEA :  Classification code based over stacks contents.
C!  ECLTEA :  Total energy.
C!  ECLSEA :  Energy per stack and module.
C!  XYZBEA :  Barycenter.
C!  NBDEEA :  Number of hit detectors ( < 3 )
C!  LDETEA :  Detectors numbers
C!  ICREEA :  Region code.
C!  NBRGEA :  Number of hit regions.
C!  LREGEA :  Regions numbers
C!  ENRGEA :  Energy per region.
#endif
