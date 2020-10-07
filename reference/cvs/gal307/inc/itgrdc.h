*CD itgrdc
      COMMON /ITGRDC/DTIMIT(20,20,8)
C
#if defined(DOC)
        Arrays of drift-time values for a 20x20 grid across an ITC cell,
        centered on the sense wire.  There is one array per layer.
        DTIMIT = drift-time values at various positions in cell.
#endif
