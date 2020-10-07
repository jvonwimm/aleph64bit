C - Name of row KROW in bank X??? when CHSTR=CHX???
      ROWNAM(CHSTR,KROW) = CHSTR((KROW-1)*4+1:(KROW-1)*4+4)
C - Row number of row named NAM in bank X??? when CHSTR=CHX???
      NUMROW(CHSTR,NAM) = (INDEX(CHSTR,NAM)+3)/4
C - Track multiplicity decoded from IWORD by masking with MASK and
C   shifting the bits by NSHFT positions
C   for ITC Track count            MASK = 3840 , NSHFT = -8
C   for TPC Inner zone track count MASK =   15 , NSHFT =  0
C   for TPC Outer zone track count MASK =  240 , NSHFT = -4
      MULTIP(IWORD,MASK,NSHFT) = ISHFT(IAND(IWORD,MASK),NSHFT)
#if defined(DOC)
C
C!    Set of statement functions to handle trigger banks
C
#endif
