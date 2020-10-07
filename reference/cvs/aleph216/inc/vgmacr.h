C! macros to be used in geometry package.
C
C Matrix to array index for the rotation matrix
      KVINDX(III,JJJ) = JVTERO + (JJJ-1)*3 + III-1
C
C Geometrical quantities for track extrapolation
C VFXGWC : Gives the position in xyz of the wafer center
      VFXGWC(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       JVTETR+IIXYZ-1 , IIFAC, IIWFF, IILAY )
C VFNLxx : Gives the three xyz coordinates of basis vector in the local
C          coordinate. xx can be VV, UU, or WW for the three vectors.
      VFNLVV(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       KVINDX(IIXYZ,1), IIFAC, IIWFF, IILAY )
      VFNLUU(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       KVINDX(IIXYZ,2), IIFAC, IIWFF, IILAY )
      VFNLWW(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       KVINDX(IIXYZ,3), IIFAC, IIWFF, IILAY )
