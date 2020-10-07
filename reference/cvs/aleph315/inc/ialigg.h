      PARAMETER (JGTRIA=3)
      COMMON/IALIGG/GROTIA(JGTRIA,JGTRIA),GTRNIA(JGTRIA)
#if defined(DOC)
C Global Alignment constants for ITC+TPC wrt ALEPH frame.
C
C JGTRIA       : Number of dimensions.
C
C GROTIA(i,j)  : Rotation matrix for transforming from ITC+TPC
C                               frame to the ALEPH frame
C GTRNIA(i)    : Displacement of centres of ITC+TPC in ALEPH frame.
C
#endif
