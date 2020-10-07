*CD tpgeow
      INTEGER NTWIRE, ITLWIF, ITLWIL, NTREG1
      REAL TWSTEP, TWIRE1, TWIRMN, TWIRMX, TWIRLE, TFRATH
      COMMON /TPGEOW/ TWSTEP(LTSTYP),TWIRE1(LTSTYP),NTWIRE(LTSTYP),
     &                TWIRMN(LTWIRE,LTSTYP),TWIRMX(LTWIRE,LTSTYP),
     &                TWIRLE(LTWIRE,LTSTYP),ITLWIF(LTSTYP),
     &                ITLWIL(LTSTYP),NTREG1(4,LTSTYP),TFRATH
C
#if defined(DOC)
C----------------------------------------------------------------------
C
C!   Wire readout geometry for TPC
C   =============================
C
C   TWSTEP(is)  = sense wire spacing in each sector type
C   TWIRE1(is)  = first wire position along sector axis (measured from
C                 the center of curvature of the padrow)
C   NTWIRE(is)  = no of sense wires in each sector type
C   TWIRMN(iw,is) = Minimum wire extent for each wire in sector.
C                   Distance from symmetry axis to beginning of wire.
C   TWIRMX(iw,is) = Maximum wire extent for each wire in sector.
C                   Distance from symmetry axis to end of wire.
C   TWIRLE(iw,is) = Wire length
C   ITLWIF(is)    = wire number on first electronics channel
C   ITLWIL(is)    = wire number on last electronics channel
C   NTREG1(ir,is) = first wire in region 'ir'
C-------------------------------------------------------------------
#endif
