      SUBROUTINE VDSIGM(EMAT,IV,VUW,TNG,SIGMA)
C----------------------------------------------------------------------
C!  VDSIGM calculates the sigmas to smear the VDZT (IV=1) or VDXY (IV=2)
C!         bank.
C!
C - Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C!
C!  Description
C!  ===========
C!  given the alignment error matrix, wafer center and
C!  the position of the hit,  returns the error due to the
C!  alignment to be applied in the MC.
C!  in order to make a fair comparison with the data 3-hit method,
C!  the local coordinate vector VUW has to be taken in the overlap
C!  region.
C
C  Input :  EMAT  - the error matrix
C           IV    - the view
C           VUW   - the local coordinate
C           TNG   - the cotangent of the angle that the track makes
C                   with the wafer, projected to the view
C
C  Output : SIGMA - the smear
C
C -----------------------------------------------------------------------
#ifndef DOC
      REAL EMAT(6,6), VUW(3), TNG, SIGMA
      INTEGER IV
      REAL JACOB(6), VTMP(6), VDOT
C-- get the jacobian
      CALL VDJACB(IV,VUW,TNG,JACOB)
C-- error sigma^2 = [J][errmat][J]^t
      CALL VMATR(JACOB,EMAT,VTMP,6,6)
      SIGMA = VDOT(VTMP,JACOB,6)
      SIGMA = SQRT(SIGMA)
      RETURN
      END
#endif
