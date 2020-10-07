      SUBROUTINE IASIGA(ITK,VV0,ADJC,SIGMX,NHIT,NGHT,CHIP,IER)
C-----------------------------------------------------------------------
C! Asign ITC coords to a track
CKEY ITC
C!
C!    Author  :  W. B. Atwood  2-Oct-89
C!    Modified:  J.Sedgbeer 20/04/90 To go in ALEPHLIB
C!
C!   Input:
C!     ITK    /I4 : number of bank/track
C!     VV0(5) /R  : track params (1/r tanl phi0 d0 z0)
C!     ADJC   /R  : Adjacent cell cut (fraction 0. -> 1.)
C!     SIGMX  /R  : Sigma R-Phi maximum cut (number of sigma)
C!     commons:    /BCS/ for BOS banks.
C!     params:     IQXTJJ
C!
C!   Output:
C!     IER    /I  : Error flag:
C!                  =  0 if O.K.
C!                  =  1 Missing input banks. No output
C!                  =  2 No room to create BOS banks. No output.
C!                  =  3 Track doesn't cross any ITC layers. No output
C!                  = -1 O.K. but garbage collection done
C!                  = -10 O.K. but missing ILIV bank (i.e. no info
C!                        on dead wires etc. available
C!     NHIT   /I  : Number of coords found on track (in same cell)
C!     NGHT   /I  : Number of coords within SIGMX (in same cell)
C!     CHIP   /R  : Chisq per point = Sum of (residual/err)**2 divided
C!                                     by number of good hits (NGHT)
C!     IQXT bank number ITK  - track quality and resolution info
C!
C!   calls     : ITXING, ITXFLG, ITQUAL
C!
C!   Libraries required: BOS
C!
C! IASIGA: Given a set of track parameters VV0 and a track number, ITK,
C! this routine makes a bank IQXT number ITK containing information on
C! the track crossings, coordinates, residuals etc (see DDL).
C!
C? Get Track crossings in ITC - ITXING
C? If no crossings then skip
C? Set flag, wire and coordinate lists for the crossings
C? Create bank IQXT number ITK: contains all info for this track.
C? Update flags in IQXT bank: good coords (flag=1) within SIGMX of
C?                                            track get flag=0
C? Make global quantities NHIT,NGHT and CHIP.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "iqxtjj.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND,NLINK
      INTEGER ITK,NHIT,NGHT,IER
      INTEGER NXNG,ICLW(8),IFLG(8),IWIR(8),ICO(8)
      REAL VV0(*),ADJC,SIGMX,CHIP,XHT(3,8),XDC(3,8),DRF(8)
      LOGICAL  FIRST,DEB
      DATA FIRST/.TRUE./,DEB/.FALSE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise
      IF(FIRST) THEN
        LPR = IW(6)
        FIRST = .FALSE.
      ENDIF
      CHIP = 0.
      NHIT = 0
      NGHT = 0
C
      IF(DEB) WRITE(LPR,1000) ITK, (VV0(I),I=1,5),ADJC,SIGMX
 1000 FORMAT('0IASIGA: Track',I4,' pars',5E11.3,/9X,'ADJC-SIGMX',2F9.2)
C
C-----------------------------------------------------------------------
C Get the track crossings in the ITC (Use Z test)
C
      IZTST = 0
      CALL ITXING(VV0,IZTST,NXNG,ICLW,XHT,XDC,DRF)
      IF(NXNG.LE.0) THEN
        IER = 3
        GOTO 999
      ENDIF
C
      IF(DEB) THEN
        WRITE(LPR,1001) NXNG
 1001   FORMAT(' IASIGA: After call ITXING: NXNG',I5,
     +          /8X,' iclw',9X,'XHT',18X,'XDC',12X,'DRF')
        DO 10 I=1,8
          WRITE(LPR,1002) ICLW(I),(XHT(J,I),J=1,3),
     +                            (XDC(J,I),J=1,3),DRF(I)
 1002     FORMAT(8X,I5,2F7.2,F7.1,3F7.3,F6.2)
   10   CONTINUE
      ENDIF
C
C Set flag,wire and coord lists (IERF flag for missing ILIV)
C
      CALL ITXFLG(ICLW,DRF,ADJC,IFLG,IWIR,ICO,IER)
      IF(IER.GT.0) GOTO 999
      IERF = 0
      IF(IER.LT.0) IERF = -10
C
      IF(DEB) THEN
        WRITE(LPR,1011) IER,(IFLG(I),IWIR(I),ICO(I),I=1,8)
 1011   FORMAT(' IASIGA: After call ITXFLG - IER=',I5,
     +       ' Flag   Wire   Coord',/(38X,3I5))
      ENDIF
C
C Get track info. (DOCAs,residuals etc.) Create IQXT bank number ITK
C
      CALL ITQUAL(ITK,IFLG,IWIR,ICO,XHT,XDC,IER)
      IF(IER.GT.0) GOTO 999
C
CC      IF(DEB) WRITE(LPR,1012) IER
CC 1012 FORMAT(' IASIGA:  After call ITQUAL IER=',I7)
C
C-----------------------------------------------------------------------
C Get IQXT bank : update flags for SIGMX cut and make global track
C                                                     quantities
      JIQXT = NLINK('IQXT',ITK)
      IF(JIQXT.LE.0) THEN
        IER = 1
        GOTO 999
      ENDIF
C
      DO 300 LAY=1,8
        JJ = KROW(JIQXT,LAY)
        IFL = IW(JJ+JIQXFL)
C Only consider 'good' coords i.e. those that already have flag=1
        IF(IFL.GT.1) GOTO 300
        NHIT = NHIT + 1
        RSD = RW(JJ+JIQXRD)
        DER = RW(JJ+JIQXER)
        RES = ABS(RSD/DER)
        IF(RES.LE.SIGMX) THEN
          IW(JJ+JIQXFL) = 0
          CHIP = CHIP + RES**2
          NGHT = NGHT + 1
        ENDIF
  300 CONTINUE
C
C  Normalize Xi**2 to number of hits
C
      CHIP = CHIP/AMAX1(FLOAT(NGHT),1.)
C
      IER = IER + IERF
C
      IF(DEB) THEN
        WRITE(LPR,2002) NHIT,NGHT,CHIP
 2002   FORMAT('0IASIGA: IQXT After update: NHIT-NGHT-CHIP',2I4,F8.2,
     +    /1X,'Lay Flag Wire',' Coord    DOCA      DD    ERR',
     +    '     RD      RF      RZ')
        DO 400 LAY=1,8
          JJ = KROW(JIQXT,LAY)
          WRITE(LPR,2001) LAY,(IW(JJ+I),I=1,3),RW(JJ+11),
     +           (RW(JJ+I),I=13,17)
 2001     FORMAT(1X,I3,3I5,F9.4,F8.4,F8.4,2F8.4,F7.1)
  400   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
  999 CONTINUE
      END
#endif
