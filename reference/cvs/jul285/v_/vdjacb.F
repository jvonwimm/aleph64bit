      SUBROUTINE VDJACB(IV,VUW,TNG,JACOB)
C----------------------------------------------------------------------
C!  VDJACB calculates the jacobian of local coordinate (W or U)
C!  with respect to the alignment parameters
C!
C - Author         Manoj Thulasidas  10-Apr-96
CKEY V_DECK VDET
C!
C!  Description
C!  ===========
C  Input :
C           IV    - the view
C           VUW   - the local coordinate
C           TNG   - the cotangent of the angle that the track makes
C                   with the wafer, projected to the view
C
C  Output : JACOB(6) - the jacobian
C
C ----------------------------------------------------------------------
#ifndef DOC
      INTEGER IV
      REAL VUW(3), TNG, JACOB(6)
      REAL V,U,W
      V = VUW(1)
      U = VUW(2)
      W = VUW(3)
C-- the jacobian of the displacements
      IF (IV.EQ.1) THEN
        JACOB(1) = TNG
        JACOB(2) = 0.0
        JACOB(3) = -1.0
        JACOB(4) = -U
        JACOB(5) = TNG*W
        JACOB(6) = -(TNG*U)
      ELSEIF (IV.EQ.2) THEN
        JACOB(1) = TNG
        JACOB(2) = -1.0
        JACOB(4) = W
        JACOB(5) = TNG*W
        JACOB(6) = -(TNG*U)
      ENDIF
      RETURN
      END
#endif
