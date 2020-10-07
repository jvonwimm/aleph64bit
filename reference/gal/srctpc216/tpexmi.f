      SUBROUTINE TPEXMI(ZMAX,XEX)
C
C! Fast sim : Determine super-broken segment middle and extremities coor
C
C  Called from:    T2TRAN
C
C  Calls:          none
C
C  Inputs:   PASSED       --JSEGT,  num of current segment
C                         --NSEGT,  total number of segments
C
C  Outputs:  PASSED       --XEX,    useful segment coords.
C                          .XEX(1,J) :  beginning  coords (J=1,2,3,4)
C                          .XEX(2,J) :  middle coords     (J=1,2,3,4)
C                          .XEX(3,J) :  end coords        (J=1,2,3,4)
C                                 ( 1,2,3,4  <--> X,Y,Z,T )
C                          .XEX(4,J) :  num of electrons in 1st
C                                       (2nd) part : J=1(2).
C
C  P. Janot   11/15/87
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C  TPCBOS contains parameters for handling BOS banks used in the
C  generation of analog and digitized signals in the TPC
C  NCHAN = number of channel types for analog signals and digitizations
C  at present, NCHAN = 3; 1 = wires, 2 = pads, 3 = trigger pads.
      PARAMETER ( NCHAN = 3 )
C
C  Work bank id's.  INDREF(ich) = index for signal reference bank for
C  channel of type ich; INDSIG = index for signal bank for current
C  channel.  IDCLUS = index for cluster bank
C
      COMMON/WORKID/INDREF(NCHAN),INDSIG,IDCLUS,ITSHAP,ITDSHP,
     *              ITPNOI,ITSNOI,ITPULS,ITMADC,INDBRT,INDHL,INDDI
C
C  Parameters for analog signal work banks:  for each type of channel,
C  include max number of channels, default number of channels in
C  signal bank, and number of channels by which to extend signal bank
C  if it becomes full; also keep counter for number of blocks actually
C  filled in signal bank
C
      COMMON/ANLWRK/MAXNCH(NCHAN),NDEFCH,NEXTCH,NTSGHT
C
C  Parameters for digitises (TPP) output banks
C
      COMMON/DIGBNK/NDIDEF(3),NDIEXT(3)
C
C  Hit list and digitization bank parameters: for each type of channel
C  include name nam, default length ndd, and length of extension nde.
C
      COMMON/TPBNAM/DIGNAM(2*NCHAN)
      CHARACTER*4 DIGNAM
C  Name index for track element bank
      COMMON/TPNAMI/NATPTE
C
C
C  TRAKEL:  track parameters for dE/dX and carrying around broken
C  tracks
C
      COMMON/TRAKEL/NTRK,X(3),VECT(3),ABSMOM,SEGLEN,TOF,AMASS,CHARGE,
     *              RAD,CENT(2),DELPSI,PSI1,ALPH01,ALPH02
C - MXBRK = 2* MAX(NLINES(1..3)) + 2 , NLINES= 8,10,10 in /SCTBND/
      PARAMETER (MXBRK=22, MXBRTE=MXBRK/2)
      COMMON/BRKNTK/XB(3,6),VECTB(3,6),SEGLNB(6)
C
C  FASTER : variables used in fast simulation
C
      COMMON / LANDAU / NITLAN,NITIND,INTWRD
      COMMON / EXBEFF / XPROP,TTTT,XXXX(1001),XSHFT(50)
      COMMON / EVT / IEVNT,ISECT
      COMMON / T3TR / JSEGT,NSEGT,ITYPE,WIRRAD(4),WIRPHI(4)
     &               ,AVTIM(4),NELE(4),NCL,SIGT(4)
      COMMON / TPSG / CC(3),XX(3),TOTDIS,XLEN,ISTY,IE
      COMMON / XBIN / IBIN(4),NAVBIN(4),NB
      DIMENSION XEX(4,4)
      PARAMETER (SQ2=1.4142136,SQ3=1.7320508)
C
C  Segment bank index
C
      INDEX = 2 + IDCLUS + (JSEGT-1)*10
C
C  Now calculate the XEX(I,J), I,J = 1,2,3
C
 1    CONTINUE
      IF(JSEGT.EQ.1.OR.JSEGT.EQ.2) THEN
        XEX(1,1) = X(1)
        XEX(1,2) = X(2)
        XEX(1,3) = X(3)
        IF(XEX(1,3).GT. ZMAX) XEX(1,3) = ZMAX
        IF(XEX(1,3).LT.-ZMAX) XEX(1,3) =-ZMAX
        XEX(1,4) = TOF
        XEX(3,1) = RW(INDEX+1)
        XEX(3,2) = RW(INDEX+2)
        XEX(3,3) = RW(INDEX+3)
        XEX(3,4) = RW(INDEX+4)
        IF(JSEGT.EQ.1) THEN
           XEX(2,1) = XEX(1,1)
           XEX(2,2) = XEX(1,2)
           XEX(2,3) = XEX(1,3)
           XEX(2,4) = XEX(1,4)
           XEX(4,1) = 0.
           XEX(4,2) = IW(INDEX+9)
           GOTO 60
        ELSE
           XEX(2,1) = RW(INDEX-9)
           XEX(2,2) = RW(INDEX-8)
           XEX(2,3) = RW(INDEX-7)
           XEX(2,4) = RW(INDEX-6)
           XEX(4,1) = IW(INDEX-1)
           XEX(4,2) = IW(INDEX+9)
           GOTO 60
        ENDIF
      ELSEIF (JSEGT.EQ.NSEGT.AND.IW(INDEX+10).EQ.1) THEN
        XEX(1,1) =  RW(INDEX-9)
        XEX(1,2) =  RW(INDEX-8)
        XEX(1,3) =  RW(INDEX-7)
        XEX(1,4) =  RW(INDEX-6)
        XEX(3,1) =  RW(INDEX+1)
        XEX(3,2) =  RW(INDEX+2)
        XEX(3,3) =  RW(INDEX+3)
        XEX(3,4) =  RW(INDEX+4)
        XEX(2,1) =  XEX(3,1)
        XEX(2,2) =  XEX(3,2)
        XEX(2,3) =  XEX(3,3)
        XEX(2,4) =  XEX(3,4)
        XEX(4,1) = IW(INDEX+9)
        XEX(4,2) = 0.
        GOTO 60
      ELSE
        XEX(1,1) = RW(INDEX-19)
        XEX(1,2) = RW(INDEX-18)
        XEX(1,3) = RW(INDEX-17)
        XEX(1,4) = RW(INDEX-16)
        XEX(2,1) = RW(INDEX-9)
        XEX(2,2) = RW(INDEX-8)
        XEX(2,3) = RW(INDEX-7)
        XEX(2,4) = RW(INDEX-6)
        XEX(3,1) = RW(INDEX+1)
        XEX(3,2) = RW(INDEX+2)
        XEX(3,3) = RW(INDEX+3)
        XEX(3,4) = RW(INDEX+4)
        XEX(4,1) = IW(INDEX-1)
        XEX(4,2) = IW(INDEX+9)
      ENDIF
60    CONTINUE
      IF (IW(INDEX+10).EQ.0 .AND. IW(INDEX).EQ.0) XEX(4,1)=0.
      RETURN
      END
