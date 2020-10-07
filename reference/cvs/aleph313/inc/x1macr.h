C!    Set of statement functions to handle trigger banks
C - Track multiplicity decoded from IWORD by masking with MASK and
C   shifting the bits by NSHFT positions
C   for ITC Track count            MASK = 3840 , NSHFT = -8
C   for TPC Inner zone track count MASK =   15 , NSHFT =  0
C   for TPC Outer zone track count MASK =  240 , NSHFT = -4
      MULTIP(IWORD,MASK,NSHFT) = ISHFT(IAND(IWORD,MASK),NSHFT)
