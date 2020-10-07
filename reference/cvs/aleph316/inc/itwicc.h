      PARAMETER (JWIRIT=8,MWIRIT=960)
      COMMON/ITWICC/RWIRIT(JWIRIT),NWIRIT(JWIRIT),IWIRIT(JWIRIT),
     +              PHWRIT(JWIRIT),CELWIT(JWIRIT),WZMXIT,SGMXIT
#if defined(DOC)
C ITC Wire geometry
C
C JWIRIT        = Length of arrays: no. of ITC wire layers
C MWIRIT        = Total number of sense wires in ITC
C
C RWIRIT(Layer) = Radius of sense wire layer                     (cm
C NWIRIT(Layer) = Number of sense wires per layer
C IWIRIT(Layer) = Offset for start of wire numbering in this layer.
C PHWRIT(Layer) = Angular offset of first wire in layer.    (radians
C CELWIT(Layer) =  Width of cell in azimuthal direction.       (cm.)
C WZMXIT        =  Maximum sensitive length of wires           (cm.)
C SGMXIT        =  Maximum wire sag                            (cm.)
#endif
