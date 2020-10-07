C ---------------------------------------------------------------------------
      subroutine VDGTCH(XCOR,NR,IVIEW,JCHIP)
C ---------------------------------------------------------------------------
C! Given the local coordinate, bank number and view, returns the chip  
C! number
C!
C - Author         Manoj Thulasidas  3-Jul-1998
C  
C ---------------------------------------------------------------------------
#ifndef DOC
      implicit none
C  arguments
      INTEGER NR, IVIEW, JCHIP
      REAL XCOR
C  functions
      EXTERNAL VDLCRS, VRSWMX, VFWAFF
      INTEGER VDLCRS, VRSWMX, VFWAFF
C  locals
      INTEGER IRET, JLAY, IWFF, IFAC, JVIEW, IROS, IWAF, IDATC, ICHIP,
     $   IMOD, JMOD
      REAL RSTRP
C============================================================================
C  decode the address
      CALL VADEWA (NR,JLAY,IWFF,IFAC,JVIEW)
      IRET = VFWAFF(IWFF,IMOD,IWAF)
C  get the read out strip from coord
      IRET = VDLCRS (XCOR,IVIEW,RSTRP)
C  integerize
      IROS = INT(RSTRP)
C  read out strip to chip#
      IRET = VRSWMX (IROS,IWAF,IVIEW,IDATC,ICHIP)
C  return a line driver#
C  8 chips per module, 2 modules per face, (9+15=24) faces, 2 views
      CALL VDCHNB(JLAY, IFAC, IMOD, IVIEW, ICHIP, JCHIP)
 999  return
      end
#endif
