      SUBROUTINE THTFLG(ITK,IFLG)
C
C---------------------------------------------------------------------
C! Which TPC padrows is a given track expected to hit?
CKEY TPCDES TRACK TPC / USER
C  Author:  R. Johnson    13-07-89
C
C  Input:     ITK       /I      Track number in FRFT
C  Output:    IFLG(21)  /I      0 if no hit expected on this row
C                               1 if a hit is expected on this row
C
C  NOTE:  the TPC geometry commons must be initialized by a call to
C         TRDDAF before this routine is called
C
C----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
C
      SAVE
      DIMENSION HP(5),HPT(5),IFLG(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
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
        NFRFT=NAMIND('FRFT')
      ENDIF
      KFRFT=IW(NFRFT)
      IF (KFRFT.EQ.0) THEN
        DO 10 IR=1,LTPDRO
          IFLG(IR)=0
   10   CONTINUE
      ENDIF
C
C++   Find new helix parameters after scattering at the ITC-TPC wall
C
      HP(1)=RTABL(KFRFT,ITK,JFRFIR)
      HP(2)=RTABL(KFRFT,ITK,JFRFTL)
      HP(5)=RTABL(KFRFT,ITK,JFRFZ0)
      CALL UNEWDP(RW(KROW(KFRFT,ITK)+JFRFIR),HP(4),HP(3))
C
C++   Transform to the TPC frame (alignment correction)
C
      CALL TGHPAL(HP,HPT)
C
C++   Get the padrows expected to be hit
C
      CALL TRHFLG(HPT,IFLG)
C
      RETURN
      END
