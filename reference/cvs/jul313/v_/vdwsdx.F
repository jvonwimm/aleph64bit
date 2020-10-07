      SUBROUTINE VDWSDX(IV,VUW,AVEC,TNG,DISP)
C ----------------------------------------------------------------------
C!  VDWSDX calculates the displacement in a hit (at VUW) in a
C!         given view for a misalignment described by the alignment
C!         quantities in AVEC for wafer JWAF.
C!
C - Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C
C  Input :  IV    - view
C           VUW   - local coordinates
C           AVEC  - six dimensional vector of displacements in
C                   the three alignment translations and three
C                   rotations.
C           TNG   - tangent of the track incident angle (= track slope)
C  Output : DISP  - displacement in position (to be applied to either
C                   U [view=1] or W [view=2] coordinate) by the
C                   caller.
C
C -----------------------------------------------------------------------
#ifndef DOC
      REAL VUW(3), AVEC(6), TNG, DISP
      INTEGER IV
      REAL  JACOB(6), VDOT
C-- get the jacobian
      CALL VDJACB(IV,VUW,TNG,JACOB)
C-- error disp = [jacob].[avec]
      DISP = VDOT(AVEC,JACOB,6)
      RETURN
      END
#endif
