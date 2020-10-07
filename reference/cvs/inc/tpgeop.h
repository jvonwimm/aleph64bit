*CD tpgeop
      INTEGER NTPDRW, NTPDPR
      REAL TPDRBG, TPDRST, TPDHGT, TPDSEP, TPDWID, TPDHWD, TPDPHF,
     &     TPDPHW, TPDPHS    
      COMMON /TPGEOP/ NTPDRW(LTSTYP),NTPDPR(LTSROW,LTSTYP),
     &                TPDRBG(LTSTYP),TPDRST(LTSTYP),TPDHGT(LTSTYP),
     &                TPDSEP(LTSTYP),TPDWID(LTSTYP),TPDHWD(LTSTYP),
     &                TPDPHF(LTSROW,LTSTYP),TPDPHW(LTSROW,LTSTYP),
     &                TPDPHS(LTSROW,LTSTYP)
C
#if defined(DOC)
C----------------------------------------------------------------------
C   NOTE: Parameters are defined in comdeck TPGPAR
C
C!   Pad readout geometry for TPC
C   ============================
C
C   NTPDRW(is)      = Number of padrows in each sector type
C   NTPDPR(ir,is)   = No of equivalent full pads on each padrow
C   TPDRBG(is)      = First padrow radius of each sector type
C   TPDRST(is)      = Radial step between padrow centres
C   TPDHGT(is)      = Pad height
C   TPDSEP(is)      = Spacing between pad centres (along r-phi)
C   TPDWID(is)      = Pad width (along r-phi)
C   TPDHWD(is)      = Half-pad width
C   TPDPHF(irs,is)  =  Half angle subtended by frame at each padrow
C   TPDPHW(irs,is)  =  Phi at leading edge of 1st pad on row irs
C   TPDPHS(irs,is)  =  Phi width of single pad on row irs
C
C---------------------------------------------------------------------
#endif
