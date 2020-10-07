      SUBROUTINE YZPHAN(PR,RL1,Z0,PSIN,XDN)
C-----------------------------------------------------
CKEY YV0 ANGLE DISTANCE /INTERNAL
C! Find psi angle respect to the point pr,minimizing the z distance
C
C AUTHOR : M.A.CIOCCI 20/1/90
C MODIFIED:
C    INPUT
C          PR(3)/R  Generally the beam-beam crossing point coordinates
C          RL1/R    (Radius of curvature)*(Tangent of dip)
C          Z0/R     Z0 (see frft bank)
C          PSIN/R   psi angle mod twopi
C
C    OUTPUT
C          PSIN/R   psi angle
C
C     CALLED:
C             YPSIVN
C
C     CALLS:
C             NONE
C
C                      DESCRIPTION
C                      ============
C     Select the correct phase for the psi angle minimizing also
C     the z distance and recalculates psi
C
C
C------------------------------------------------------------------
#ifndef DOC
      SAVE
       REAL PR(*),XDN(*)
      XDN(3)=Z0+RL1*(PSIN)
C
      RETURN
      END
#endif
