      INTEGER FUNCTION KD0FL(DUMMY)   
CKEY  FILL CHARGED / USER     
C----------------------------------------------------------------------
C! Tells which interaction point used to compute d0,z0           
C!                                          J.Boucrot  03.02.98
C   KD0FL = 1 if QFNDIP event primary vertex used
C   KD0FL = 2 if event-chunk beam position used 
C   To get any of the above values, the data card 'D0NW' must be
C   present in the user's data cards
C   KD0FL = 3 if LEP 1 run-by-run average beam position used
C   KD0FL = 0 if none of the above values available
C              in that case a beam position x=y=z=0. was used
C----------------------------------------------------------------------
#ifndef DOC
      COMMON /IDZVRT/ ID0Z0FL
C----------------------------------------------------------------------
      KD0FL = ID0Z0FL
      RETURN
      END
#endif


