      INTEGER FUNCTION VINTLF (XYZ1,XYZ2,JLAY,IFAC,
     >                             NWI,MMOD,IWAF,ABCI,ABCO)
C ----------------------------------------------------------------------
CKEY VDETDES LINE WAFER / USER
C!  Find intersection points of line and wafers in a face
C - Steve Wasserbaech, April 1994
C
C   VINTLF returns all of the intersections points of a line and the
C   surfaces of the wafers in a face.  VINTLF calls VINTLW to find the
C   wafer entry and exit points.
C
C   Nominal geometry parameters are used in the calculations.
C   No wafer intersections are returned if XYZ1 and XYZ2 are less
C   than 0.0001 cm apart.
C
C - Input:
C   XYZ1(3)        / R  (x,y,z) (cm) of first point on line;
C                       the line is defined by two points in
C                       the ALEPH system.
C   XYZ2(3)        / R  (x,y,z) (cm) of second point on line
C   JLAY           / I  Global layer index
C   IFAC           / I  Local face index
C
C - Output:
C   VINTLF         / I  = VDOK if successful
C                         *and* the line intersects a wafer;
C                       = VDERR if an error occurs, or if the line
C                         intersects no wafer in the specified face
C   NWI            / I  Number of wafers intersected
C   MMOD(NVWFMX)   / I  Signed global module indices of
C                       intersected wafers; 1 =< index =< NWI
C   IWAF(NVWFMX)   / I  Local wafer indices of intersected wafers
C   ABCI(3,NVWFMX) / R  (a,b,c) coordinates of wafer entry point in
C                       IWAF local system; 1 <= second index <= NWI
C   ABCO(3,NVWFMX) / R  (a,b,c) coordinates of wafer exit point in
C                       IWAF local system
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
#include "vgindx.h"
C
C     Arguments:
      INTEGER JLAY, IFAC, NWI, MMOD(NVWFMX), IWAF(NVWFMX)
      REAL XYZ1(3), XYZ2(3), ABCI(3,NVWFMX), ABCO(3,NVWFMX)
C
C     Local variables
      INTEGER JWAF, IWFF1, IWFF2, ISTEP, IWFF, I, IRET
      REAL ABCIN(3), ABCOUT(3)
      LOGICAL LINTC
C
C     External references:
      INTEGER VWFFND, VMWAFJ, VINTLW
C
C ----------------------------------------------------------------------
C
      VINTLF = VDERR
      NWI = 0
      CALL VZERO(MMOD,NVWFMX)
      CALL VZERO(IWAF,NVWFMX)
      CALL VZERO(ABCI,3*NVWFMX)
      CALL VZERO(ABCO,3*NVWFMX)
C
C     Find the local wafer-in-face index for the wafer nearest to XYZ1:
C
      IWFF1 = VWFFND(XYZ1(3))
C
C     Check whether the line intersects wafer-in-face IWFF1:
C
      IRET = VINTLW(XYZ1,XYZ2,JLAY,IFAC,IWFF1,ABCIN,ABCOUT,LINTC)
      IF (IRET .EQ. VDOK) THEN
C
C     If LINTC = .TRUE. then the line passed through the two large
C     surfaces of the wafer--it cannot intersect any other wafer
C     in this face:
C
        IF (LINTC) THEN
          NWI = NWI + 1
          JWAF = IJWFFR(JLAY,IFAC,IWFF1)
          IRET = VMWAFJ(JWAF,MMOD(NWI),IWAF(NWI))
          DO I=1,3
            ABCI(I,NWI) = ABCIN(I)
            ABCO(I,NWI) = ABCOUT(I)
          ENDDO
          GO TO 900
        ENDIF
      ENDIF
C
C     The line did not intersect the two large surfaces of IWFF1.
C     We will loop over all wafers in the face.  Choose the direction
C     of looping so the order of the intersected wafers corresponds
C     to moving along the line in the direction from XYZ1 toward XYZ2:
C
      IF (XYZ2(3) .GT. XYZ1(3)) THEN
        IWFF1 = 1
        IWFF2 = NWAFEF
      ELSE
        IWFF1 = NWAFEF
        IWFF2 = 1
      ENDIF
C
C     Loop on wafers in the face from IWFF1 to IWFF2 and
C     get the intersection points:
C
      ISTEP = ISIGN(1,IWFF2-IWFF1)
      DO IWFF=IWFF1,IWFF2,ISTEP
        IRET = VINTLW(XYZ1,XYZ2,JLAY,IFAC,IWFF,ABCIN,ABCOUT,LINTC)
        IF (IRET .EQ. VDOK) THEN
          NWI = NWI + 1
          JWAF = IJWFFR(JLAY,IFAC,IWFF)
          IRET = VMWAFJ(JWAF,MMOD(NWI),IWAF(NWI))
          DO I=1,3
            ABCI(I,NWI) = ABCIN(I)
            ABCO(I,NWI) = ABCOUT(I)
          ENDDO
        ENDIF
      ENDDO
C
C     Success!
C
 900  CONTINUE
      IF (NWI .GT. 0) VINTLF = VDOK
C
 1000 CONTINUE
      RETURN
      END
#endif
