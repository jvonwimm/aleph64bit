      SUBROUTINE TGHPAL(P,PA)
C
C----------------------------------------------------------------------
C! Transform helix from ALEPH coordinates to TPC coordinates
CKEY TPCDES HELIX TRANSFORM / USER
C     Author:   R. Johnson   19-08-89
C     Modified: R. Johnson   21-1-91 separate the geometry calculations
C                            into another routine which can be called
C                            in a different context
C
C     Input:
C       - P(5)       /R     Helix parameters in ALEPH frame
C                           1/r,tanl,phi0,d0,z0
C                           (d0>0 = positive ang. mom. about z axis)
C                           (r>0  = counterclockwise rotation)
C     Output:
C       - PA(5)     /R      Helix parameters in TPC frame
C
C   Remarks:  only translations and rotations about the z axis are
C             possible in this case.  Rotations about x and y are
C             ignored since the helix model always assumes a helix
C             about the z axis.
C
C----------------------------------------------------------------------
      SAVE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER(JTPOID=1,JTPOVR=2,JTPOTL=4,JTPORT=7,JTPOIT=10,JTPOTC=11,
     +          LTPOSA=11)
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      DIMENSION P(*),PA(*),DX(3)
      INTEGER ALGTDB
      LOGICAL FIRST
      DOUBLE PRECISION XC,YC
      DATA FIRST/.TRUE./,EPS/0.001/
C
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
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPOS=NAMIND('TPOS')
        NEVEH=NAMIND('EVEH')
      ENDIF
      DO 10 I=1,5
        PA(I)=P(I)
   10 CONTINUE
C
C++   Find the run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        CALL ALTELL('TGHPAL: cannot find EVEH bank',0,'RETURN')
        RETURN
      ENDIF
      IRUN=IW(KEVEH+JEVERN)
C
C++   Link to the bank of alignment constants.  Read them from the
C++   database if necessary.
C
      KTPOS=IW(NTPOS)
      IF (KTPOS.EQ.0) THEN
        IRET=ALGTDB(JUNIDB(0),'TPOS',IRUN)
        IF (IRET.EQ.0) THEN
          CALL ALTELL('TGHPAL: cannot find TPOS on database',0,'RETURN')
          RETURN
        ENDIF
        KTPOS=IW(NTPOS)
      ELSE
        IF (IRUN.LT.ITABL(KTPOS,1,JTPOVR)
     &        .OR. IRUN.GT.ITABL(KTPOS,1,JTPOVR+1)) THEN
          IRET=ALGTDB(JUNIDB(0),'TPOS',IRUN)
          IF (IRET.EQ.0) THEN
            CALL ALTELL('TGHPAL: cannot find TPOS on database',
     &                                                  0,'RETURN')
            RETURN
          ENDIF
          KTPOS=IW(NTPOS)
        ENDIF
      ENDIF
C
      DX(1)=RTABL(KTPOS,1,JTPOTL)
      DX(2)=RTABL(KTPOS,1,JTPOTL+1)
      DX(3)=RTABL(KTPOS,1,JTPOTL+2)
      DPHI=RTABL(KTPOS,1,JTPORT+2)
      CALL THTRAN(P,DX,DPHI,PA)
C
      END
