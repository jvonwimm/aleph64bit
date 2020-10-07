      SUBROUTINE ITLNK2(ITRY,IER)
C-----------------------------------------------------------------------
C! ITC: search for links
C!
C!    Author      :- I. Tomalin  88/08/25
C!    Modified    :- I. Tomalin  89/02/28
C!    Modified    :- I. Tomalin  90/01/19
C!    Modified    :- J.Sedgbeer  91/02/21 Fix EXTPH for 0-2pi cross-over
C!    Modified    :- J.Sedgbeer  91/09/10 curvature calc. - better for
C!                                        low p tracks.
C!   Input:
C!      ITRY /I : Search type: ITRY = 1 -> high Pt (tight cuts)
C!                             ITRY = 3 -> down to low Pt (loose cuts)
C!      commons:     /BCS/
C!                   /IWBNKT/
C!                   /ITRKPA/
C!                   /ITRKER/
C!                   /ITGAPC/
C!                   /ITWICC/
C!      params.:     /ALCONS/
C!                   /ITCOJJ/
C!   Output:
C!      IER  /I : Error flag:  IER = 0 if all O.K.
C!                             IER = 1 if there were too many links.
C!      work bank:   ILNKIW
C!
C!   called by : ITRAK
C!   calls     : IENANG and IPHCOR
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O commons and parameters
#include "alcons.h"
#include "bcs.h"
#include "iwbnkt.h"
#include "itcojj.h"
#include "itwicc.h"
#include "itrker.h"
#include "itrkpa.h"
#include "itgapc.h"
C-----------------------------------------------------------------------
      INTEGER NLINK
      EXTERNAL NLINK
      REAL IPHCOR
      EXTERNAL IPHCOR
      LOGICAL FOUND
      LOGICAL SETZ
      DATA SETZ/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialisation
C
      IER = 0
      IF (SETZ) THEN
        ILNKIW = 0
        SETZ = .FALSE.
      END IF
      IF (ILNKIW.GT.0) THEN
        CALL WDROP(IW,ILNKIW)
        CALL RERROR('ITLNK2',1,'ILNKIW work bank still exists from '//
     +  'earlier. It should have been dropped.')
      END IF
      LEN = LMHLEN + 4*350
      CALL WBANK(IW,ILNKIW,LEN,*950)
      IW(ILNKIW+LMHCOL) = 4
C
      KITC0 = NLINK('ITCO',0)
      IF (KITC0.EQ.0.OR.IUCOIW.EQ.0.OR.IWHTIW.EQ.0) GOTO 999
      NCO = LROWS(KITC0)
C
      MXJPC = MXJMPC(2)
  10  CONTINUE
      IW(ILNKIW+LMHROW) = 0
C
C Loop over all coordinates (in order of decreasing wire number).
C
      DO 100 IC=NCO,1,-1
C Ignore this coordinate if it has already been assigned to a track.
        MUSE = IW(IUCOIW+LMHLEN+IC)
        IF (MUSE.EQ.1) GOTO 100
C Find the layer number of the coordinate.
        ILAY = ITABL(KITC0,IC,JITCWN)/1000
C Stop if inner layer has been reached.
        IF (ILAY.EQ.1) GOTO 999
C Check coord. is in cell
        PHI1 = RTABL(KITC0,IC,JITCP1)
        PHI2 = RTABL(KITC0,IC,JITCP2)
        SRPQ = RTABL(KITC0,IC,JITCSR)
        DIST = 0.5*RWIRIT(ILAY)*(MOD(PHI1-PHI2+5.0*PI,TWOPI)-PI)
        IF (DIST.GT.0.5*CELWIT(ILAY)+
     +            SIGNUM(ITRY)*SQRT(SRPQ)) GOTO 100
C Note the radius of the coordinate.
        RADI = RTABL(KITC0,IC,JITCRA)
C Find z-coordinate and error**2 thereon.
        ZI = RTABL(KITC0,IC,JITCZH)
        IF (FZTSIT(2)) THEN
          SZI = RTABL(KITC0,IC,JITCSZ)
C          IF (SZI.GT.10000.0) GOTO 100
        END IF
C Loop over the two ambiguities for this coordinate.
        DO 90 IAMB=-1,0
C Find the phi of the ambiguity.
          PHI = RTABL(KITC0,IC,JITCP2+IAMB)
C
          NLCON = 0
C Loop over the links to the layers inside ILAY.
          DO 80 NLAY=ILAY-1,1,-1
C See if the links are now jumping over too many layers.
            IF (ILAY-NLAY-1.GT.MXJMPD(2)) GOTO 90
            IF (NLCON.GT.MXJPC) GOTO 90
            FOUND = .FALSE.
C Note the approximate change in phi and radius due to detector misalign
C at this coordinate
            RADN = RWIRIT(NLAY)
            PHIT = PHI
C Make an approx. estimate of the z-coordinate.
            ZT = ZI*RWIRIT(NLAY)/RWIRIT(ILAY)
            CALL ITROTN(RADN,PHIT,ZT)
            EXTPH = MOD(PHI-PHIT+5.0*PI,TWOPI)-PI
C Get the radial distance between the two layers.
            DR = RADI - RADN
C Calculate window in phi and hence get wire no's to be searched.
            DX = 0.5*DR/CTR0IT(ITRY)
            PHID = PHI - MIN(DX,PI)
            PHIU = PHI + MIN(DX,PI)
C Note cell size and phi shift beginning of 1st cell.
            DLN  = TWOPI/FLOAT(NWIRIT(NLAY))
            SHFN = PHWRIT(NLAY) - DLN*0.5
            PHDS = PHID - SHFN + EXTPH
            PHUS = PHIU - SHFN + EXTPH
            PHDS = AMOD(PHDS+TWOPI,TWOPI)
            PHUS = AMOD(PHUS+TWOPI,TWOPI)
            IWD = INT(PHDS/DLN) + IWIRIT(NLAY) + 1
            IWU = INT(PHUS/DLN) + IWIRIT(NLAY) + 1
C
C Estimate z-coordinate of track in layer NLAY (and error**2 thereon).
            IF (FZTSIT(2)) THEN
              ZG = ZI*RADN/RADI
              SZG = SZI*(RADN/RADI)**2
            END IF
C
C 'Loop' over wires to be searched in layer "NLAY".
C  (with care at the 0-2pi cross-over).
            IF (IWU.LT.IWD) IWU = IWU + NWIRIT(NLAY)
            DO 70 ILOOP=IWD,IWU
              IWIRE = ILOOP
              IF (IWIRE.GT.IWIRIT(NLAY+1)) IWIRE = IWIRE - NWIRIT(NLAY)
C See if this wire has an unused hit associated with it.
              NC = IW(IWHTIW+LMHLEN+IWIRE)
              IF (NC.LE.0) GOTO 70
              MUSE = IW(IUCOIW+LMHLEN+NC)
              IF (MUSE.EQ.1) GOTO 70
C Check coord. is in cell
              PHI1 = RTABL(KITC0,NC,JITCP1)
              PHI2 = RTABL(KITC0,NC,JITCP2)
              SRPQN= RTABL(KITC0,NC,JITCSR)
              DIST = 0.5*RWIRIT(NLAY)*(MOD(PHI1-PHI2+5.0*PI,TWOPI)-PI)
              IF (DIST.GT.0.5*CELWIT(NLAY)+
     +            SIGNUM(ITRY)*SQRT(SRPQN)) GOTO 70
C See if hit lies inside the road in z.
              IF (FZTSIT(2)) THEN
                ZN = RTABL(KITC0,NC,JITCZH)
                SZN = RTABL(KITC0,NC,JITCSZ)
C                IF (SZN.GT.10000.0) GOTO 70
                IF ((ZN-ZG)**2.GT.(SZG+SZN)*ZFACIT(2)**2) GOTO 70
              ENDIF
C Test both ambigs.(in phi) to see if they fall within range.
C (taking care of 0-2pi cross over).
              DO 50 NAMB=-1,0
C Do 1st order drift time correction.
                CALL IENANG(NC,NAMB,IC,IAMB,ENN,ENI)
                PHNC = IPHCOR(NC,NAMB,ENN,0.0)
                IF (PHID.LT.PHNC.AND.PHNC.LT.PHIU.OR.
     +          PHIU.LT.PHID.AND.(PHID.LT.PHNC.OR.PHNC.LT.PHIU)) THEN
C This link is O.K. so store it.
                  PHIC = IPHCOR(IC,IAMB,ENI,0.0)
                  FOUND = .TRUE.
                  KLNK = KNEXT(ILNKIW)
                  IF (IAMB.EQ.-1) IW(KLNK+1) = IC
                  IF (IAMB.EQ.0) IW(KLNK+1) = -IC
                  IF (NAMB.EQ.-1) IW(KLNK+2) = NC
                  IF (NAMB.EQ.0) IW(KLNK+2) = -NC
                  IW(KLNK+3) = IGAPIT(NLAY,ILAY)
                  RW(KLNK+4) = 2.0*SIN(PHIC-PHNC)/
     +              (SQRT(RADI**2+RADN**2-2.*RADI*RADN*COS(PHIC-PHNC)))
                  IW(ILNKIW+LMHROW) = IW(ILNKIW+LMHROW) + 1
                  IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
                    WRITE(LDEBRL,35) LROWS(ILNKIW),(IW(KLNK+II),II=1,3),
     +              RW(KLNK+4)
   35               FORMAT(5X,'ITLNK2 : Link formed: Link=',I4,
     +              '  Coords.= ',2I4,' IGAP',I4,' Curvature',F10.5)
                  ENDIF
                  IF(LFRWRD(ILNKIW).LE.0) THEN
                    LEN = IW(ILNKIW)
                    IF (LEN.GT.LMHLEN+4*5000) GOTO 900
                    CALL WBANK(IW,ILNKIW,LEN+4*250,*900)
                  END IF
                ENDIF
   50         CONTINUE
   70       CONTINUE
C Note the number of layers with links connected to ILAY found so far.
            IF (FOUND) NLCON = NLCON + 1
   80     CONTINUE
   90   CONTINUE
  100 CONTINUE
C Finished !
      GOTO 999
C Error return.
  900 CONTINUE
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.1) CALL RERROR('ITLNK2',2,
     +'No. of links too great. Search being simplified.')
C Simplify and try again.
      IF (MXJPC.GT.0) THEN
        MXJPC = MXJPC - 1
        GOTO 10
      END IF
C
      IF(FDEBRF) CALL RERROR('ITLNK2',3,
     +'Link search could not be '//
     +'simplified enough, so was not completed.')
      GOTO 975
  950 CONTINUE
      CALL RERROR('ITLNK2',4,'No more space to create ILNKIW'//
     +' work bank.')
  975 CONTINUE
      IER = 1
      IF (ILNKIW.NE.0) CALL WDROP(IW,ILNKIW)
      GOTO 1000
C Normal ending
  999 CONTINUE
C Compress ILNKIW work bank.
      CALL WBANK(IW,ILNKIW,LMHLEN+LROWS(ILNKIW)*LCOLS(ILNKIW),*1000)
 1000 CONTINUE
      END
#endif
