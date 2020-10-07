*CD tpgeot
      INTEGER NTPTRW, NTPTPR, ITPADG
      REAL TPTRBG, TPTRST, TPTRHG, TPTPHC, TPTPHW
      COMMON /TPGEOT/ NTPTRW(LTSTYP),NTPTPR(LTTSRW,LTSTYP),
     &                TPTRBG(LTSTYP),TPTRST(LTSTYP),TPTRHG(LTSTYP),
     &                TPTPHC(LTTPAD,LTTSRW,LTSTYP),
     &                TPTPHW(LTTPAD,LTTSRW,LTSTYP),
     &                ITPADG(LTTPAD,LTTSRW,LTSTYP)
C
#if defined(DOC)
C-----------------------------------------------------------------------
C NOTE: Parameters are defined in comdeck TPGPAR
C
C! TPC Trigger pad readout geometry
C
C    NTPTRW(is)    = No of trigger pad rows in each sector type
C    NTPTPR(ir,is) = No of trigger pads on each row of each sector type
C    TPTRBG(is)    = First trigger padrow radius of each sector type
C    TPTRST(is)    = Radial step between trigger padrow centers
C    TPTRHG(is)    = Trigger pad height
C    TPTPHC(ip,ir,is) = Phi position at centre of each pad on each
C                       padrow (with respect to sector axis)
C    TPTPHW(ip,ir,is) = Half phi width of each pad on each padrow
C    ITPADG(ip,ir,is) = ????? (ask R. Richter)
C-----------------------------------------------------------------------
#endif
