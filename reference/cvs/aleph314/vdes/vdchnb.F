C ---------------------------------------------------------------------------
      subroutine VDCHNB(JLAY, IFAC, IMOD, IVIEW, ICHIP, JCHIP)
C ---------------------------------------------------------------------------
C! Given the layer (JLAY), face (IFAC), module (IMOD), view (IVIEW) 
C! and chip in module (ICHIP), returns a global chip number (JCHIP)
C!
C - Author         Manoj Thulasidas  3-Jul-1998
C  
C ---------------------------------------------------------------------------
#ifndef DOC
      implicit none
C  arguments
      INTEGER JLAY, IFAC, IMOD, IVIEW, ICHIP, JCHIP
C  functions
      EXTERNAL VJMODI
      INTEGER VJMODI
C  locals
      INTEGER IRET, JMOD
C============================================================================
C  get the global module
      IRET = VJMODI (JLAY,IFAC,IMOD,JMOD)
C  return a line driver#
C  8 chips per module, 2 modules per face, (9+15=24) faces, 2 views
      JCHIP = (IVIEW-1)*384 + (JMOD-1)*8 + ICHIP
 999  return
      end
#endif
