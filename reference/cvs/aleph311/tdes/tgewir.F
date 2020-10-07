      SUBROUTINE TGEWIR
C -------------------------------------------------------------------
C! Fill wire geometry
C!
C!  Author:   D.Demille   2-2-86
C!  Modified: R.Johnson   6-5-87
C!  Modified: R.Richter  29-4-88  (removed frame-part from wire-length)
C!
C!  Called from TRDDAF
C!
C!  Description:
C!  ===========
C!  This routine determines the horizontal extent of each wire on each
C!  type of sector in the ALEPH TPC.
C!-------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeow.h"
      DIMENSION CORN(2,LTCORN), HRGIO(4), NMNLN(4), NMXLN(4)
C
      DATA HRGIO /4*-1./, NMNLN /4*-1/, NMXLN /4*-1/
C
C  LOOP OVER SECTOR TYPES.
C
      DO 5 ITYPE = 1,LTSTYP
C
C  Get the number of corners and their coordinates for this sector type
C
         NCORN = NTPCRN (ITYPE)
C
         CALL UCOPY (TPCORN(1,1,ITYPE), CORN(1,1), 2*NCORN)
C
C  Get the position of the first wire.
C
         TPR = TWIRE1 (ITYPE)
C
C
         IF ( ITYPE .EQ. 1 ) THEN
C
            NREGI = 3
C
            HRGIO(1) = CORN(1,1)
            NMNLN(1) = 1
            NMXLN(1) = 2
C
            HRGIO(2) = CORN(1,3)
            NMNLN(2) = 0
            NMXLN(2) = 2
C
            HRGIO(3) = CORN(1,4)
            NMNLN(3) = 0
            NMXLN(3) = 3
C
         ELSEIF ( ITYPE .EQ. 2 ) THEN
C
            NREGI = 3
C
            HRGIO(1) = CORN(1,2)
            NMNLN(1) = 0
            NMXLN(1) = 1
C
            HRGIO(2) = CORN(1,4)
            NMNLN(2) = 0
            NMXLN(2) = 3
C
            HRGIO(3) = CORN(1,5)
            NMNLN(3) = 0
            NMXLN(3) = 4
C
         ELSE
C
            NREGI = 4
C
            HRGIO(1) = CORN(1,2)
            NMNLN(1) = 0
            NMXLN(1) = 1
C
            HRGIO(2) = CORN(1,3)
            NMNLN(2) = 0
            NMXLN(2) = 2
C
            HRGIO(3) = CORN(1,4)
            NMNLN(3) = 0
            NMXLN(3) = 3
C
            HRGIO(4) = CORN(1,5)
            NMNLN(4) = 0
            NMXLN(4) = 4
C
         ENDIF
C
C  The wire - length is defined as the length of the sensitive
C  part of the wire (.i.e. excluding the fraction inside the frame).
C  THIS DEFINITION IS DIFFERENT FROM THE DEFINITION BEFORE
C  april 29, 1988 which included the "dead" part of the wire.
C
C  We split up the sector into "REGIONs" whose side boundaries
C  are formed by only  one line segment (between y(i) and y(i+1)).
C  The beginning of the REGION is defined by the number of the first
C  wire located in it.  Here's a picture of what is meant:
C
C | Y-AXIS
C I
C I
C I______________________________________
C I                                     |\
C I                                     I \
C I          REGION 3                   I  \
C I                                     I   \
C I_____________________________________I____\
C I                                     I    /|
C I                                 HRGIO(3)/ I
C I                                     I  /  I
C I                                     I /   I
C I                   REGION 2          I/    I
C I                                     /     I
C I                                    /I     I
C I                                   / I     I
C I                                  /  I  HRGIO(2)
C I                                 /   I     I
C I______________________ ________ /    I     I
C I                           \   /|    I     I
C I                       \      / I    I     I
C I                        \ *--/-----------------REGION 1
C I                         \  /   I    I     I
C I                          \/    I    I     I
C I                             HRGIO(1)I     I
C I                                I    I     I
C I                                I    I     I
C I                                I    I     I
C --------------------------------------------------------------> X-AXIS
C
C  In other words, HRGIO(j) is the y-coordinate of the upper corner
C  of the (outerside) border of the frame of region j.
C  NMXLN(j) is the number of the first corner which defines the
C  frame of region j.
C  NMNLN(j) is analagous and is needed because in REGION 1 of
C  the K - sector the wires dont extent to the y - axis.
C  In all other cases NMNLN(j) = 0.
C
C   END of INITIALIZATION PART for this SECTOR TYPE  ------
C
C
         KRGIO   = 0
         N1NEXT = NTREG1 (1, ITYPE)
C
C  Now loop over wires in this sector type and determine the min and
C  max x value of each
C
         DO 10 IWIR = 1, NTWIRE (ITYPE)
C
C  Determine the height of the wire. See if we've stepped over a
C  region boundary.
C
            HWIR = TPR + (IWIR-1) * TWSTEP (ITYPE)
            IF (IWIR .EQ. N1NEXT) THEN
C
               KRGIO   = KRGIO + 1
               IF (KRGIO .LT. NREGI) N1NEXT = NTREG1((KRGIO+1),ITYPE)
C
C  Determine the slope and intercept of the line which forms the
C  outer boundary of this region
C
               DYMX  = CORN(1,NMXLN(KRGIO)+1) - CORN(1,NMXLN(KRGIO))
               DXMX  = CORN(2,NMXLN(KRGIO)+1) - CORN(2,NMXLN(KRGIO))
               SLMX  = DYMX / DXMX
               BMX   = CORN(1,NMXLN(KRGIO)) - SLMX*CORN(2,NMXLN(KRGIO))
               AX    = ATAN (SLMX)
               FRAMX = ABS (TFRATH / SIN(AX))
C
C  Determine the slope and intercept of the line which forms the
C  inner boundary of this region
C
               IF ( NMNLN(KRGIO) .NE. 0 ) THEN
                  DYMN = CORN(1,NMNLN(KRGIO)+1) - CORN(1,NMNLN(KRGIO))
                  DXMN = CORN(2,NMNLN(KRGIO)+1) - CORN(2,NMNLN(KRGIO))
                  SLMN = DYMN / DXMN
                  BMN  = CORN(1,NMNLN(KRGIO))- SLMN*CORN(2,NMNLN(KRGIO))
                  FCMN = 1.
                  AN   = ATAN (SLMN)
                  FRAMN= ABS (TFRATH / SIN (AN))
               ELSE
                  SLMN  = 1.
                  BMN   = 0.
                  FCMN  = 0.
                  FRAMN = 0.
               ENDIF
C
            ENDIF
C
C  Find the min and max x values for this wire and put them in the
C  appropriate array
C
            TWIRMN(IWIR,ITYPE) = (HWIR-BMN) / SLMN * FCMN + FRAMN
            TWIRMX(IWIR,ITYPE) = (HWIR-BMX) / SLMX        - FRAMX
            TWIRLE(IWIR,ITYPE) = TWIRMX(IWIR,ITYPE) - TWIRMN(IWIR,ITYPE)
C
C  Next wire
C
 10      CONTINUE
C
C  Next sector type
C
  5   CONTINUE
C
      RETURN
      END
#endif
