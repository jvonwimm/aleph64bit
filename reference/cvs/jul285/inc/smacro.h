C----------------------------------------------------------------------
C! Set of statement functions for the Small Angle Tracking Device
C------->  decode TDC addresses from raw data word JXXXX
#if defined(DOC)
C          Format of raw data words:
C           31  28 27  24 23 21 20    16 15                         0
C          +------+------+-----+--------+----------------------------+
C          |unused| TDC  |Crate|  Card  |       TDC-Contents         |
C          +------+------+-----+--------+----------------------------+
#endif
C - Crate number
      JCRAT(JXXXX) = IBITS(JXXXX,21,3)
C - Card number
      JCARD(JXXXX) = IBITS(JXXXX,16,5) + 1
C - TDC number
      JTDCN(JXXXX) = IBITS(JXXXX,24,4) + 1
C - TDC contents
      JTDCC(JXXXX) = IBITS(JXXXX,0,16)
C
C------->  convert phys. layer number to layer number in electronics
C          and vice versa
      KXX(IXX) = MOD(IXX-1,3)*3 + (IXX-1)/3 + 1
C
C------->  convert sector number ISXX to mean azimuth angle of sector
C          in layer ILXX
      PHIXXX(ILXX,ISXX) = AMOD (PHIBSG(ILXX) + (FLOAT(ISXX)-.5)*TWOPI/
     +     FLOAT(MSECSG), TWOPI)
C
