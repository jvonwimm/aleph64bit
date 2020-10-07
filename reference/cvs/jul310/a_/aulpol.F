      SUBROUTINE AULPOL(RAD,ZMAX,PHI1,PAR,PAREX,ICODE)
C-----------------------------------------------------------------
C! intersection of a line with a 12-side poligonal surface
C! G. Capon  25-sep-87
C!
C!   Input variables : RAD = internally tangent radius
C!                     ZMAX = max Z value
C!                     PHI1 = phi for first vertex above x-axis
C!                     PAR  = x,y,z dx/ds,dy/ds,dz/ds
C!
C!   Output          : PAREX = x,y,z,dx/ds,dy/ds,dz/ds at intersection p
C?
C!======================================================================
#ifndef DOC
      PARAMETER(COS15=0.9659258,PIBY6=0.5235987)
      DIMENSION PAR(*),PAREX(*)
C
      ICODE=0
      RE=RAD/COS15
      XP=PAR(1)
      YP=PAR(2)
      ZP=PAR(3)
      ALFA=PAR(4)
      BETA=PAR(5)
      GAMMA=PAR(6)
C                        set protection if line is parallel to z axis
      IF (ABS(ALFA)+ABS(BETA).LT.0.0001) ALFA=0.0001
C
      PHI=PHI1
      X1=RE*COS(PHI)
      Y1=RE*SIN(PHI)
C
C                             loop on consecutive poligon vertices P1,P2
C
      DO 10 N=1,12
        PHI=PHI+PIBY6
        X2=RE*COS(PHI)
        Y2=RE*SIN(PHI)
        PV1=BETA*(X1-XP)-ALFA*(Y1-YP)
        PV2=BETA*(X2-XP)-ALFA*(Y2-YP)
        IF (PV1*PV2.LT.0.) THEN
C                             line crosses P1P2 segment
          DX=X2-X1
          DY=Y2-Y1
          T1=X1*DY-Y1*DX
          T2=XP*BETA-YP*ALFA
          DET=BETA*DX-ALFA*DY
          X=(DX*T2-ALFA*T1)/DET
          Y=(DY*T2-BETA*T1)/DET
          PS=ALFA*(X-XP)+BETA*(Y-YP)
C                             test consistency with line orientation
          IF (PS.GT.0.) GO TO 20
        ENDIF
        X1=X2
        Y1=Y2
   10 CONTINUE
      GO TO 999
C
C             intersection is found
C             choice barrel or endcap
C
   20 CONTINUE
      Z=ZP+GAMMA*(ABS(X-XP)+ABS(Y-YP))/(ABS(ALFA)+ABS(BETA))
      IF (ABS(Z).LT.ZMAX) THEN
        ICODE=1
      ELSE
        Z=SIGN(ZMAX,GAMMA)
        IF (ABS(GAMMA).LT.0.0001) GAMMA = 0.0001
        X=XP+ALFA*(Z-ZP)/GAMMA
        Y=YP+BETA*(Z-ZP)/GAMMA
        ICODE=2
C
      ENDIF
      PAREX(1)=X
      PAREX(2)=Y
      PAREX(3)=Z
      CALL UCOPY(PAR(4),PAREX(4),3)
C
  999 RETURN
      END
#endif
