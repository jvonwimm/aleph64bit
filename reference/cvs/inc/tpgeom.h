*CD tpgeom
      INTEGER NTSECT, NTPROW, NTPCRN, ITPTYP, ITPSEC, IENDTP
      REAL RTPCMN, RTPCMX, ZTPCMX, DRTPMN, DRTPMX, DZTPMX, TPFRDZ,
     &     TPFRDW, TPAVDZ, TPFOF1, TPFOF2, TPFOF3, TPPROW, TPTROW,
     &     TPCORN, TPPHI0, TPCPH0, TPSPH0
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
#if defined(DOC)
C----------------------------------------------------------------------
C  NOTE: Parameters are defined in comdeck TPGPAR
C
C!  Global geometry for the TPC
C  ===========================
C
C  RTPCMN          =  Min radius of active volume
C  RTPCMX          =  Maximum radius of active volume
C  ZTPCMX          =  Half length of active volume
C  DRTPMN          =  Inner wall thickness
C  DRTPMX          =  Outer wall thickness
C  DZTPMX          =  Endplate thickness
C  TPFRDZ          =  Thickness of Al sector support frame
C  TPFRDW          =  Width of Al sector support frame
C  TPAVDZ          =  Thickness of equivalent endplate av. material
C  TPFOF1          =  Offset of frame "kink" (see drawings)
C  TPFOF2          =  Offset of frame "kink" (see drawings)
C  TPFOF3          =  Offset of frame "kink" (see drawings)
C  TPPROW(ipr)     =  Nominal padrow radii in master system
C  TPTROW(itr)     =  Nominal trig. padrow radii in master system
C  NTSECT          =  Number of sectors in TPC
C  NTPROW          =  Total number of rows in TPC
C
C  TPC Sector geometry
C  ===================
C
C               LTSTYP is number of sector types
C               LTSLOT is number of sector slots of each type
C               LTCORN is max number of corners for describing
C                      sector geometry
C
C  NTPCRN(is)      =  No of corners in each sector type
C                     (1=K, 2=M, 3=W)
C  TPCORN(2,ic,is) =  {x,y} coordinates of corners
C  TPPHI0(ntsec)   =  Global phi positions of sectors
C  TPCPH0(iloc)    =  Cosines of sector phi positions
C  TPSPH0(iloc)    =  Sines of sector phi positions
C  ITPTYP(iloc)    =  Type number of sector at each slot
C  ITPSEC(iloc)    =  Sector number (of given type) at each slot
C  IENDTP(iloc)    =  Endplate number of each sector
C                     Endplate 1 is at +z, Endplate 2 is at -z
C
C-------------------------------------------------------------------
#endif
