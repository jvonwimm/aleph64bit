      SUBROUTINE VALCOR(KVALC,IRUN)
C! ---------------------------------------------------------------------
CKEY VDETDES GEOMETRY
C!   Correct time dependent bending of the faces of VDET
C!   Correct the VALC bank
C! - David Rousseau, Decembre 1995
C!
C!  Called by :    VGRDAL
C!  Calls     :    FUNCTION ALGTDB               from ALEPHLIB
C!                 VGEXRO, VGCMTR, VGINTR        from ALEPHLIB
C!
C! - Input:
C!   KVALC   / I  VALC bank index
C!   IRUN    / I  Run number
C!
C! ---------------------------------------------------------------------
C      IMPLICIT NONE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER JVALWI, JVALTR, JVALRO, JVALEM, LVALCA
      PARAMETER (JVALWI=1, JVALTR=2, JVALRO=5, JVALEM=8, LVALCA=28)
      INTEGER KVALC,IRUN
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
C
C displacement as measured in microns with 3-layer method in inner layer
C
      DATA VALCRUN  /0,
     &              -1, -4,-26,-36,-37,
     &             -53,-71,-81,-88,-93,
     &             -77,-77,-73,-73,
     &             -57,-46,-46,-46,-40,
     &             -35,-28,-33,-46,-63,
     &             -77,-84,-97,-93,-106/
      INTEGER IRGR
      REAL DELALI(6,2)
C
C conversion factor : average displacement in inner layer -> max bending
C
      REAL ZMAX,CONVBEND
      PARAMETER (ZMAX=16,CONVBEND=-1.8)
      INTEGER IRET,JWAF,I,IRO,IWFF,ILAY,IFAC,IVIEW,IRUNO,IVALC
      REAL H0,H1,H2,ROT,VUWMAX(3),Z1,Z2
      REAL VUW(3),XYZ(3),DIRBEND,BEND
      DATA VUW/3*0./
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST, DELALI, VALCRUN, RUNLIM
      INTEGER  VJWAFF,VVUWXY,VWADIM
      EXTERNAL VJWAFF,VVUWXY,VWADIM
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C

      IF (FIRST) THEN
        FIRST=.FALSE.
C
C calculate V translation and U rotation for a one micron bending
C
        IRET=VWADIM(VUWMAX)
        DO IWFF=1,6
          IRET=VJWAFF(1,IWFF,JWAF)
C
C calculate displacement of ends of wafer
C
          VUW(3)=-VUWMAX(3)/2.
          IRET=VVUWXY(VUW,JWAF,XYZ)
          Z1=XYZ(3)
          VUW(3)=VUWMAX(3)/2.
          IRET=VVUWXY(VUW,JWAF,XYZ)
          Z2=XYZ(3)
C
C parabolic distortion
C
          H1=(1-(Z1/ZMAX)**2)*1E-4
          H2=(1-(Z2/ZMAX)**2)*1E-4
C
C displacement of the center of the wafer (along V)
C
          H0=(H1+H2)/2.
C
C rotation around the center (around U axis)
C
          ROT=(H2-H1)/VUWMAX(3)
          DELALI(IWFF,1)=H0
          DELALI(IWFF,2)=ROT
        ENDDO
      ENDIF
C
C get run by run correction
C
      IF (IRUN.LT.RUNLIM(1)) GOTO 999
      IF (IRUN.GT.40530) GOTO 999
C
C find run group number
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
C Correct VALC bank
C
      DO IRO=1,LROWS(KVALC)
C
C     Decode the wafer index
C
        CALL VADEWA(ITABL(KVALC,IRO,JVALWI),ILAY,IWFF,IFAC,IVIEW)
C
C direction of the bending
C
        IF (ILAY.EQ.1) THEN
          DIRBEND=-1.
        ELSE
          DIRBEND=1.
        ENDIF
C
C correct only v translation and U rotation
C
        IVALC=KROW(KVALC,IRO)
        RW(IVALC+JVALTR)=RW(IVALC+JVALTR)+
     &                     BEND*DIRBEND*DELALI(IWFF,1)
        RW(IVALC+JVALRO+1)=RW(IVALC+JVALRO+1)+
     &                     ATAN(BEND*DIRBEND*DELALI(IWFF,2))
      ENDDO
 999  CONTINUE
      RETURN
      END
