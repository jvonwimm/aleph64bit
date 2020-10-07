      SUBROUTINE ITCFLG(NLC,LCO,ICLW,IFLG,IWIR,ICO,IER)
C-----------------------------------------------------------------------
CKEY ITC
C! Get coords,wires and flags from track coord. list
C!
C!    Author  :  J.Sedgbeer 20/04/90
C!
C!   Input:
C!     NLC      /I  : No. of coords. in LCO. (Maximum of 8)
C!     LCO(i)   /I  : List of coords (as in FICL etc)
C!     ICLW(i)  /I  : List of closest wires to track crossings
C!     common   /BCS/ for BOS banks ITCO (ITC coords. bank)
C!                                  ILIV (ITC live/dead channel info.)
C!              /ITWICC/ ITC Wire geometry
C!     params   ITCOJJ
C!
C!   Output:
C!    IFLG(i)  /I  : flag: = 1 to 5  See Below.
C!    IWIR(i)  /I  : wire for layer i
C!    ICO(i)   /I  : Coord. no. (in ITCO) for wire IWIR(i)
C!    IER      /I  : error flag:
C!                   =    0 all O.K.
C!                   =    1 ITCO missing - fatal - No useful output info
C!                   =  < 0 - non-fatal problem: contributions:-
C!                       -1 ILIV missing - Output arrays filled but info
C!                                  about dead/live channels not used.
C!                      -10 IWIR not adjacent to ICLW encountered at
C!                          least once.
C!                     -100 At least one coord. in list LCO is on a
C!                          supposedly dead wire! (Coord is kept)
C!                    -1000 At least one coord. in list LCO is at layer
C!                          with no track crossing point! (Coord. is
C!                          rejected).
C!
C!   Libraries required: BOS
C!
C! ---------------------------------------------------------------------
C! ICLW|IFLG IWIR  ICO     | ICLW  Comment
C!     |                   |status
C! ----|-------------------|--------------------------------------------
C!   N |  1   N    IC(N)   |  -    Coord. #IC on wire N
C!   N |  2   M    IC(M)   |  -    Coord. on wire M. (M.ne.N)
C!   N |  3   N     0      | live  N live and no entry in list LCO
C!     |                   |       corresponding to this layer.
C!   N |  4   N     0      | dead  N Dead and no entry in list LCO
C!     |                   |       corresponding to this layer.
C!   0 |  5   0     0      |  -    No Crossing
C! ---------------------------------------------------------------------
C!
C? Set error flag IER depending on existence of ITCO and ILIV banks.
C? If no ITCO bank then return.
C? Loop over ITC layers
C?   Set default values IFLG=5, IWIR=ICLW, ICO=0
C?   If ICLW is dead (look in ILIV) then
C?     set IFLG = 4
C?   else
C?     set IFLG = 3
C?   endif
C? Endloop
C? Loop over coords. in list LCO
C?   Get corresponding wire and layer number from ITCO
C?   If wire supposedly dead the set error flag IE4
C?   If no crossing at this layer then
C?     set error flag IE5
C?     skip to end of loop
C?   endif
C?   Set IFLG=1 and set IWIR and ICO
C?   If IWIR not same as ICLW then set IFLG = 2
C?   If IWIR and ICLW not adjacent set error flag IEA.
C? Endloop
C? Update error flag IER
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "itwicc.h"
#include "itcojj.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND
      INTEGER NLC,LCO(*),ICLW(*),IFLG(*),IWIR(*),ICO(*),IER
      LOGICAL  FIRST,DEB
      DATA FIRST/.TRUE./,DEB/.FALSE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise and set error flag
      IF(FIRST) THEN
        LPR = IW(6)
        LITCO = NAMIND('ITCO')
        LILIV = NAMIND('ILIV')
        FIRST = .FALSE.
      ENDIF
C
      IER = 0
      JITCO = IW(LITCO)
      JILIV = IW(LILIV)
      IF(JITCO.LE.0) THEN
        IER = 1
        GOTO 999
      ENDIF
      IF(JILIV.LE.0) IER = -1
C
      IF(DEB) WRITE(LPR,1000) NLC,(LCO(I),I=1,NLC)
 1000 FORMAT(' ITCFLG: No. Coords',I4,' Coords',8I5)
C
C-----------------------------------------------------------------------
C Loop over layers - set defaults: no crossing IFLG=5; wire dead IFLG=4;
C                                  otherwise IFLG=3.
      DO 10 LAY=1,8
        IWIRE = ICLW(LAY)
        IFLG(LAY) = 5
        IWIR(LAY) = IWIRE
        ICO(LAY)  = 0
C
        IF(DEB) WRITE(LPR,1001) LAY,IWIRE,IFLG(LAY),ICO(LAY)
 1001   FORMAT('   LAY-ICLW-IFLG-ICO',4I6)
C
        IF(IWIRE.LE.0) GOTO 10
C
C Call the wire Inefficient.. (default)
        IFLG(LAY) = 3
C
C See if this wire is alive. Look at wire status flag in ILIV.
C If no ILIV bank is available then assume all wires are live.
C Wire status 1 or 4 indicates dead R-phi channel.
        IF(JILIV.GT.0) THEN
          JWST = IW(JILIV+LMHLEN+IWIRE)
          IF(JWST.EQ.1 .OR. JWST.EQ.4) IFLG(LAY) = 4
          IF(DEB) WRITE(LPR,1002) JWST
 1002     FORMAT('          WIRE STATUS',I6)
        ENDIF
   10 CONTINUE
C
C-----------------------------------------------------------------------
C Loop over list of coords LCO.
C
      IEA = 0
      IE4 = 0
      IE5 = 0
      DO 100 I=1,NLC
        IC = IABS(LCO(I))
        IWL = ITABL(JITCO,IC,JITCWN)
        LAY = IWL/1000
        IWIRE = MOD(IWL,1000)
C
C If flag previously set to 4 (dead) then set error flag IE4
        IF(IFLG(LAY).EQ.4) IE4 = -100
C
C If flag previously set to 5 (no crossing) then set error flag IE5
C                                drop this coord (i.e. leave IFLG=5)
        IF(IFLG(LAY).EQ.5) THEN
          IE5 = -1000
          GOTO 100
        ENDIF
        IFLG(LAY) = 1
        IWIR(LAY) = IWIRE
        ICO(LAY)  = IC
        IF(IWIRE.NE.ICLW(LAY)) THEN
          IFLG(LAY) = 2
          IDEL = IWIRE-ICLW(LAY)
          NWIR = NWIRIT(LAY)
          NWI2 = NWIR/2
          IF(IABS(IDEL).LE.NWI2) THEN
            IDEL = IABS(IDEL)
          ELSE
            IDEL = MOD(IDEL+5*NWI2,NWIR) - NWI2
          ENDIF
          IF(IDEL.GT.1) IEA = -10
        ENDIF
C
        IF(DEB) WRITE(LPR,1003) I,LCO(I),IC,LAY,IWIRE,IFLG(LAY)
 1003   FORMAT('   Loop',I4,' coord/abs',2I4,' LAYER-WIRE-Flag',3I4)
C
  100 CONTINUE
C
      IF(DEB) WRITE(LPR,2000) (I,IFLG(I),IWIR(I),ICO(I),I=1,8)
 2000 FORMAT(' ITCFLG:',/(5X,'Layer',I3,' Flag',I4,' Wire',I4,
     +                        ' Coord',I4))
C
      IER = IER + IEA + IE4 + IE5
  999 CONTINUE
      END
#endif
