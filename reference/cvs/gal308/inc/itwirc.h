*CD itwirc
      COMMON/ITWIRC/RWIRIT(8),NWIRIT(8),IWIRIT(8),PHWRIT(8),CELHIT(8),
     +              CELWIT(8),WZMXIT
C
#if defined(DOC)
     Constants describing ITC cell geometry.
        RWIRIT = Radius of sense wire (cm).
        NWIRIT = Number of sense wires per layer.
        IWIRIT = Offset for start of wire numbering in this layer.
        PHWRIT = Angular offset of first sense wire in layer (radians).
        CELHIT = Height of drift cell in radial direction (cm).
        CELWIT = Width of drift cell in azimuthal direction (cm).
        WZMXIT = Maximum sensitive length of wires (cm).
#endif
