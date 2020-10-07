      SUBROUTINE VALFCO(VLOC,ILAY,IWFF,IFAC,IRUN)
C ----------------------------------------------------------------------
CKEY VDETDES GEOMETRY
C!   Correct time dependent bending of the faces of VDET
C - David Rousseau, May 1996
C
C  Called by :    VGRDAL
C
C - Input/output:
C   VLOC(*)    / R  local alignment vector from VALC corrected on output
C - Input:
C   ILAY      / I  Layer number
C   IWFF      / I  Wafer-in-face number
C   IFAC      / I  Face number
C   IRUN      / I  Run number
C
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      INTEGER IRUN
      INTEGER NRUNMAX,NRUNMAX1
      PARAMETER (NRUNMAX=30)
      INTEGER RUNLIM(NRUNMAX)
      INTEGER VALCRUN(NRUNMAX)
C approximate fill by fill division of LEP 1.4 running period
C 40265-40290 Z peak
C 40290 - 40399 130 Gev
C 40399 - 40530 130 Gev
      DATA RUNLIM  /40265,
     &              40290,40300,40313, 40327,40331,
     &              40336,40342,40351, 40356,40364,
     &              40371,40387,40393, 40399,
     &              40407,40412,40418, 40423,40432,
     &              40441,40444,40454, 40476,40485,
     &              40492,40497,40506, 40512,40517/
C displacement as measured in microns with 3-layer method in inner layer
      DATA VALCRUN  /0,
     &              -1, -4,-26,-36,-37,
     &             -53,-71,-81,-88,-93,
     &             -77,-77,-73,-73,
     &             -57,-46,-46,-46,-40,
     &             -35,-28,-33,-46,-63,
     &             -77,-84,-97,-93,-106/
      INTEGER IRGR
      REAL DELALI(6,2)
C conversion factor : average overlap in inner layer *sqrt(2) -> max bending
C (positive)
      REAL ZMAX,CONVBEND
      PARAMETER (ZMAX=16,CONVBEND=-1.8)
      INTEGER IRET,JWAF,I,IRO,IWFF,ILAY,IFAC,IVIEW,IRUNO,IWFFI
      REAL VLOC(*)
      REAL H0,H1,H2,ROT,VUWMAX(3),Z1,Z2
      REAL VUW(3),XYZ(3),DIRBEND,BEND
      DATA VUW/3*0./
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST,DELALI,RUNLIM,VALCRUN
      INTEGER  VJWAFF,VVUWXY,VWADIM
      EXTERNAL VJWAFF,VVUWXY,VWADIM
C
C
C
      IF (IRUN.LT.RUNLIM(1)) GOTO 999
      IF (IRUN.GT.40530) GOTO 999
C
C Calculate V translation and U rotation for a one micron bending
C
      IF (FIRST) THEN
        IRET=VWADIM(VUWMAX)
C
C loop on wafer in face
C
        DO IWFFI=1,6
          IRET=VJWAFF(1,IWFFI,JWAF)
C
C calculate displacement of ends of wafer (simple parabola)
C
          VUW(3)=-VUWMAX(3)/2.
          IRET=VVUWXY(VUW,JWAF,XYZ)
          Z1=XYZ(3)
          VUW(3)=VUWMAX(3)/2.
          IRET=VVUWXY(VUW,JWAF,XYZ)
          Z2=XYZ(3)
          H1=(1-(Z1/ZMAX)**2)*1E-4
          H2=(1-(Z2/ZMAX)**2)*1E-4
C
C translation of the center of the wafer (along V)
C
          H0=(H1+H2)/2.
C
C rotation around the center (around U axis)
C
          ROT=(H2-H1)/VUWMAX(3)
          DELALI(IWFFI,1)=H0
          DELALI(IWFFI,2)=ROT
        ENDDO
      ENDIF
C
C get run by run correction (hard-wired)
C
      IRUNO=0
      DO IRGR=1,NRUNMAX
        IF (IRUN.LT.RUNLIM(IRGR).AND.IRUNO.EQ.0) THEN
          IRUNO=IRGR-1
        ENDIF
      ENDDO
      IF (IRUNO.EQ.0) THEN
        IRUNO=NRUNMAX
      ENDIF
      BEND=VALCRUN(IRUNO)*CONVBEND
C
C direction of the bending (layer dependent)
C
      IF (ILAY.EQ.1) THEN
        DIRBEND=-1.
      ELSE
        DIRBEND=1.
      ENDIF
C
C correct only v translation and U rotation
C
      VLOC(1)=VLOC(1)+BEND*DIRBEND*DELALI(IWFF,1)
      VLOC(5)=VLOC(5)+ATAN(BEND*DIRBEND*DELALI(IWFF,2))
 999  CONTINUE
      FIRST=.FALSE.
      RETURN
      END
#endif
