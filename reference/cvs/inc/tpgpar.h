*CD tpgpar
      INTEGER LTPDRO, LTTROW, LTSROW, LTWIRE, LTSTYP, LTSLOT, LTCORN,
     +        LTSECT, LTTPAD, LMXPDR, LTTSRW
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
#if defined(DOC)
C-----------------------------------------------------------------------
C! Parameter definitions for TPC geometry array dimensions
C
C   LTSLOT  = number of sectors of each type
C   LTSECT  = maximum number of sectors in TPC
C   LTCORN  = maximum number of corners in a sector
C   LTPDRO  = maximum number of padrows in TPC
C   LTTROW  = maximum number of trigger padrows in TPC
C   LTTSRW  = maximum number of trigger padrows per sector
C   LTTPAD  = maximum number of pads on trigger padrow
C   LTSTYP  = maximum number of TPC sector types
C   LTWIRE  = maximum number of wires on TPC sector
C   LTSROW  = maximum number of rows on sector
C   LMXPDR  = maximum number of pads in a single row
C-----------------------------------------------------------------------
#endif
