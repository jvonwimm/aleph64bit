      SUBROUTINE ITXFLG(ICLW,DRF,ADJC,IFLG,IWIR,ICO,IER)
C-----------------------------------------------------------------------
CKEY ITC
C! Get coords,wires and flags from list of cells crossed for one track.
C!
C!    Author  :  W. B. Atwood  2-Oct-89
C!    Modified:  J.Sedgbeer    20/04/90 To go in ALEPHLIB.
C!
C!   Input:
C!     ICLW(i) /I  : List of wire numbers for layers i=1,8
C!                     ICLW(i) = 0 if no crossing on layer i.
C!     DRF(i)  /I  : Expected drift distance (signed) as fraction
C!                   of cell half-width.
C!     ADJC    /R  : 'Adjacent cell' cut, i.e. if no coord. in closest
C!                    cell and closest cell is not dead then look in
C!                    adjacent cell if ABS(DRF) > ADJC.
C!     common   /BCS/ for BOS banks IWCR (ITC wire to coord. relation)
C!                                  ILIV (ITC live/dead channel info.)
C!              /ITWICC/ ITC Wire geometry
C!
C!   Output:
C!    IFLG(i)  /I  : flag: = 1 to 5  See Below.
C!    IWIR(i)  /I  : wire for layer i
C!    ICO(i)   /I  : Coord. no. (in ITCO) for wire IWIR(i)
C!    IER      /I  : error flag:
C!                   = 0 all O.K.
C!                   = 1 IWCR missing - fatal - No useful output info.
C!                   =-1 ILIV missing - Output arrays filled but info.
C!                                  about dead/live channels not used.
C!
C!   Libraries required: BOS
C!
C! ---------------------------------------------------------------------
C! ICLW|IFLG IWIR  ICO     | ICLW  Require   Comment
C!     |                   |status
C! ----|-------------------|--------------------------------------------
C!   N |  1   N    IC      |  -       -      Coord. #IC on wire N
C!   N |  2   N+-1 IC(N+-1)| live  DRF>ADJC  N live, but no coord. on N,
C!     |                   |                 and track near edge of cell
C!     |                   |                 and coord. on adjacent wire
C!   N |  3   N     0      | live  DRF>ADJC  N live, but no coord. on N,
C!     |                   |                 and track near edge of cell
C!     |                   |                 but no coord. on adj. wire
C!   N |  3   N     0      | live  DRF<ADJC  N live, but no coord. on N,
C!     |                   |                 and track NOT near edge
C!     |                   |                 of cell.
C!   N |  4   N     0      | dead     -      Dead R-phi channel - don't
C!     |                   |                 look in adjacent cell.
C!   0 |  5   0     0      |  -       -      No Crossing
C! ---------------------------------------------------------------------
C!
C? Set error flag IER depending on existence of IWCR and ILIV banks.
C? If no IWCR bank then return.
C? Loop over ITC layers
C?   Set default values IFLG=5, IWIR=ICLW, ICO=0
C?   If no crossing at this layer (ICLW=0) then skip
C?   If there is coord. at closest wire (look in IWCR) then
C?      set IFLG=1,IWIR=wire#, ICO=coord# (in ITCO)
C?   Else
C?      If wire dead (look in ILIV) then IFLG = 4
C?      If wire live and close to edge of cell then
C?        If there is a coord on adjacent wire then
C?          IFLG = 2, IWIR = adjacent wire number
C?        else
C?          IFLG = 3
C?        endif
C?      Endif
C?      If wire live and not close to edge of cell then
C?        IFLG = 3 (inefficient)
C?      Endif
C?   Endif
C? Endloop
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "itwicc.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND
      INTEGER ICLW(*),IFLG(*),IWIR(*),ICO(*),IER
      REAL DRF(*),ADJC
      LOGICAL  FIRST,DEB
      DATA FIRST/.TRUE./,DEB/.FALSE./
C-----------------------------------------------------------------------
C Initialise and set error flag
      IF(FIRST) THEN
        LPR = IW(6)
        LIWCR = NAMIND('IWCR')
        LILIV = NAMIND('ILIV')
        FIRST = .FALSE.
      ENDIF
C
      IER = 0
      JIWCR = IW(LIWCR)
      JILIV = IW(LILIV)
      IF(JIWCR.LE.0) THEN
        IER = 1
        GOTO 999
      ENDIF
      IF(JILIV.LE.0) IER = -1
C
      IF(DEB) WRITE(LPR,1000) ADJC,(I,ICLW(I),DRF(I),I=1,8)
 1000 FORMAT(' ITXFLG: Inputs - Adjacent cell cut:',F8.2,/5X,'Layer',
     +        ' Closest wire    Drift Dist.(scaled)',/(5X,I4,I8,F14.3))
C-----------------------------------------------------------------------
C Loop over the ITC Layers set up flag, wire and coord lists
C
      DO 100 LAY=1,8
        IWIRE = ICLW(LAY)
        IFLG(LAY) = 5
        IWIR(LAY) = IWIRE
        ICO(LAY)  = 0
C
C Check if track crossed this ITC Layer
        IF(IWIRE.LE.0) GOTO 100
C
C Check if wire has an associated coord. (look in IWCR)
        IC = IW(JIWCR+LMHLEN+IWIRE)
C
CC        IF(DEB) WRITE(LPR,1001) LAY,IWIRE,IC
CC 1001   FORMAT(' ITXFLG - Loop 100: LAYER-WIRE-COORD:',3I8)
C
        IF(IC.GT.0) THEN
          IFLG(LAY) = 1
          ICO(LAY)  = IC
        ELSE
C
C If no assoc. coord. call the wire Inefficient.. (default)
          IFLG(LAY) = 3
C
C See if this wire is alive. Look at wire status flag in ILIV.
C If no ILIV bank is available then assume all wires are live.
          IF(JILIV.GT.0) THEN
            JWST = IW(JILIV+LMHLEN+IWIRE)
          ELSE
            JWST = 0
          ENDIF
C
CC          IF(DEB) WRITE(LPR,1002) LAY,JWST
CC 1002     FORMAT(10X,' No coord on Layer',I4,' Wire Status',I6)
C
C Wire status 1 or 4 indicates dead R-phi channel.
          IF(JWST.EQ.1 .OR. JWST.EQ.4) THEN
            IFLG(LAY) = 4
C
C Find out if there is a hit in an adjacent cell
          ELSEIF(ABS(DRF(LAY)).GT.ADJC) THEN
            IF(DRF(LAY).LT.0.) THEN
              IWIRP = IWIRE - 1
              IF(IWIRP.LT.IWIRIT(LAY)+1) IWIRP=IWIRIT(LAY)+NWIRIT(LAY)
            ELSE
              IWIRP = IWIRE + 1
              IF(IWIRP.GT.IWIRIT(LAY)+NWIRIT(LAY)) IWIRP=IWIRIT(LAY)+1
            ENDIF
            IC = IW(JIWCR+LMHLEN+IWIRP)
C
CC            IF(DEB) WRITE(LPR,1003) IWIRP,IC
CC 1003       FORMAT(10X,' Adjacent cell: wire-coord',2I7)
C
            IF(IC.GT.0) THEN
              IFLG(LAY) = 2
              IWIRE = IWIRP
              IWIR(LAY) = IWIRE
              ICO(LAY) = IC
            ENDIF
          ENDIF
        ENDIF
  100 CONTINUE
C
      IF(DEB) WRITE(LPR,2000) (I,IFLG(I),IWIR(I),ICO(I),I=1,8)
 2000 FORMAT(' ITXFLG: Final values:',
     +      /(5X,'Layer',I3,' Flag',I4,' Wire',I4,' Coord',I4))
C
  999 CONTINUE
      END
#endif
