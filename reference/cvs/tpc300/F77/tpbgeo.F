*DK tpbgeo
      SUBROUTINE TPBGEO
C-----------------------------------------------------------------------
C!  Calculate parameters referring to the sector boundaries for use
C!  in breaking track elements there.
C
C  Called from: TSINIT
C  Calls:       None
C
C  Inputs:  /TPGEOM/   --NTPCRN, the number of corners needed to
C                                describe each sector type
C                      --TPCORN, the coordinates of these corners
C
C  Outputs: /SCTBND/   --NLINES, the total number of lines forming each
C                                sector type
C                      --SLOPES, the slopes of the lines segments
C                                forming an "extended sector" (i.e.,
C                                a polygon slightly larger all around
C                                than a sector)
C                      --YCEPTS, the y-intercepts of these line segments
C                      --XLWLIM, x at the left-hand edge of each segment
C                      --XUPLIM, " "   "  right-hand "   "   "      "
C                      --PHIMAX, the phi-acceptance of each type of
C                                "extended sector"
C  D. DeMille
C--------------------------------------------------------------------
#include "tpgpar.h"
#include "tpgeom.h"
#include "sctbnd.h"
C
      DIMENSION EXTCRN(2,5),ADDFAC(2,5,3),MCORN(3)
C
C  ADDFAC contains factors which will be added to each coordinate of
C  each corner so as to extend the sector boundaries.  These factors
C  are extremely geometry dependent, so that any change in the sector
C  geometry may very well necessitate a change in ADDFAC.  A little
C  patience and some graph paper will show you that the region with
C  corners given in EXTCRN = CORNER+ADDFAC has a boundary which is
C  always about 2 cm outside the boundary of the region with corners
C  given in CORNER.
C
      DATA ADDFAC/-2., 0.,-3., 1., 1., 3., 2., 2., 0., 0.,
     2            -2., 2.,-2., 2.,-2., 2., 1., 3., 2., 0.,
     3            -2., 2., 1., 3., 1., 3., 1., 3., 2., 0./
C
C  MCORN--the corner at which the sector has its max phi-acceptance
C  --is also very geometry-dependent and should be handled carefully.
C
      DATA MCORN/2,3,1/
C
      DO 3 ITYPE = 1, LTSTYP
C
C  NLINES = number of lines in boundary of sector
C  NSSLN = number of lines of non-zero slope in one half-sector (when
C  the sector is divided along its axis of symmetry).
C
         NLINES(ITYPE) = 2*NTPCRN(ITYPE)
         NSSLN = NTPCRN(ITYPE) - 1
C
C  Extend the boundaries of each type of sector (for the purposes of
C  track breaking only)
C
         DO 1 ICORN = 1, NTPCRN(ITYPE)
            DO 1 IDIM = 1, 2
 1             EXTCRN(IDIM,ICORN) =   TPCORN(IDIM,ICORN,ITYPE)
     *                              + ADDFAC(IDIM,ICORN,ITYPE)
C
C  Calculate slope, intercept, and max & min x-values for each boudary
C  line with non-zero slope
C
         DO 2 JLN = 1, NSSLN
C
            SLOPES(JLN,ITYPE) = ( EXTCRN(1,JLN+1) - EXTCRN(1,JLN) )/
     *                          ( EXTCRN(2,JLN+1) - EXTCRN(2,JLN) )
            YCEPTS(JLN,ITYPE) = ( EXTCRN(1,JLN) -
     *                            EXTCRN(2,JLN)*SLOPES(JLN,ITYPE) )
            XLWLIM(JLN,ITYPE) = AMIN1( EXTCRN(2,JLN), EXTCRN(2,JLN+1) )
            XUPLIM(JLN,ITYPE) = AMAX1( EXTCRN(2,JLN), EXTCRN(2,JLN+1) )
C
            SLOPES(JLN+NSSLN,ITYPE) = -SLOPES(JLN,ITYPE)
            YCEPTS(JLN+NSSLN,ITYPE) =  YCEPTS(JLN,ITYPE)
            XLWLIM(JLN+NSSLN,ITYPE) = -XUPLIM(JLN,ITYPE)
            XUPLIM(JLN+NSSLN,ITYPE) = -XLWLIM(JLN,ITYPE)
C
 2       CONTINUE
C
C  Take care of the top and bottom lines, which have zero slope and
C  don't have the same symmetry conditions
C
         SLOPES(2*NSSLN+1,ITYPE) = 0.
         YCEPTS(2*NSSLN+1,ITYPE) = EXTCRN(1,NSSLN+1)
         XLWLIM(2*NSSLN+1,ITYPE) = -EXTCRN(2,NSSLN+1)
         XUPLIM(2*NSSLN+1,ITYPE) =  EXTCRN(2,NSSLN+1)
C
         SLOPES(2*NSSLN+2,ITYPE) = 0.
         YCEPTS(2*NSSLN+2,ITYPE) = EXTCRN(1,1)
         XLWLIM(2*NSSLN+2,ITYPE) = -EXTCRN(2,1)
         XUPLIM(2*NSSLN+2,ITYPE) =  EXTCRN(2,1)
C
C  Find the maximum phi acceptance of this type of extended sector
C
         MC = MCORN(ITYPE)
         PHIMAX(ITYPE) = ATAN( EXTCRN(2,MC)/EXTCRN(1,MC) )
C
C  End loop over sector types
C
 3    CONTINUE
C
      RETURN
      END
