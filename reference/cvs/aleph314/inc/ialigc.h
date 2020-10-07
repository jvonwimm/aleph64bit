      PARAMETER (JXYZIA=3)
      COMMON/IALIGC/ROTNIA(JXYZIA,JXYZIA),DXYZIA(JXYZIA)
#if defined(DOC)
C ITC Alignment constants wrt ITC+TPC.
C
C JXYZIA       : Number of dimensions.
C
C ROTNIA(i,j)  : Rotation matrix for transforming from ITC frame
C                                 to ITC+TPC frame
C DXYZIA(i)    : Displacement of centre of ITC wrt ITC+TPC frame.
C
#endif
