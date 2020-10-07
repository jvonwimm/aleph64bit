*CD itrotc
      COMMON/ITROTC/EULRIT(3),DXYZIT(3),ROTITC(3,3),ITSHFT,WSAGIT
      REAL EULRIT,DXYZIT,ROTITC,WSAGIT
      LOGICAL ITSHFT
C
#if defined(DOC)
      Alignment constants for ITC.
      EULRIT(3)    = Euler angles of ITC w.r.t. ALEPH
      DXYZIT(3)    = Displacement of centre of ITC in ALEPH system
      ROTITC(3,3)  = Rotation matrix for transforming hit data.
      ITSHFT       = .TRUE. if alignment constants are used.
      WSAGIT       = Sag of ITC wires at centre of chamber (cm).
#endif
