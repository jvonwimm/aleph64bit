      SUBROUTINE TFTWTB(ISLOT,JGARB)
C
C----------------------------------------------------------------------
C! Fill the TWTB bank
C!
C!    Author:   R. Johnson     21-02-89
C!    Modified: R. Johnson     21-01-91  hard cut on wire residual for
C!                                       good wire hits
C!                             11-08-91  wire number and edge corrects.
C!                                       and fixes for 2-track dedx
C!                             24-08-92  include null wire hits
C!                             2-3-93    database control of constants
C!                             5-9-93    get rid of TWPU and add DZNLTW
C!    Modified: W. Wiedenmann   6-07-93  suppress error message #6 for
C!                                       wire# 220 in islot=4
C!                                       (t0 measurement)
C!
C!              D. Casper     26-07-98   suppress missed hits if they
C!                                       will not be used
C!
C!    Input:  ISLOT       /I   TPC sector slot number
C!    Output: JGARB       /I   2 if BOS runs out or space
C!                             1 if BOS collected garbage
C!    Called by TRKWRA
C!
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "tzcorr.h"
#include "alcons.h"
#include "bcs.h"
#include "tpgpar.h"
#include "tpgeow.h"
#include "tpgeop.h"
#include "tpgeom.h"
#include "twtbjj.h"
#include "twitjj.h"
#include "tflags.h"
#include "tkrwwc.h"
#include "twirct.h"
#include "twircl.h"
#include "tpccon.h"
#include "twoljj.h"
C
      PARAMETER (MXMT=8)
      DIMENSION IMAT(MXMT),DZMAT(MXMT)
      DIMENSION XCS(2),SGNA(2),BV(2),RCHK2(0:LTPDRO)
      LOGICAL FIRST,FSKIP,GOOD
      CHARACTER TEXT*121
      SAVE FIRST,NTWTB,NTWIT,NTWOL,RCHK2,SGNA
C
      DATA FIRST/.TRUE./,SGNA/-1.,1./
C
C------------------------- Statement Function Definitions --------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      JGARB=0
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTWTB=NAMIND('TWTB')
        NTWIT=NAMIND('TWIT')
        NTWOL=NAMIND('TWOL')
        IWROFW=0
C
C++     Make an array of squared radii; one halfway between each padrow
C
        DO 745 I=1,LTPDRO
          RCHK2(I)=(TPPROW(I)+0.5*TPDRST(1))**2
  745   CONTINUE
        RCHK2(0)=(TPPROW(1)-0.5*TPDRST(1))**2
      ENDIF
      KTWTB=IW(NTWTB)
      KTWIT=IW(NTWIT)
      KTWOL=IW(NTWOL)
C
C++   Get some calibrations constants for radial dependence
C
      ISTYP=ITPTYP(ISLOT)
      P0=PWDPCL(0,ISTYP)
      P1=PWDPCL(1,ISTYP)
      P2=PWDPCL(2,ISTYP)
      P3=PWDPCL(3,ISTYP)
C
C++   Get the maximum number of tracks allowed to match a single hit
C
      MXMAT=MIN(MXMT,MXMATW)
C
      ZGCUT=ZWINTW/2.
      ZSCUT= ZWSATW/TVDRFT(IENDTP(ISLOT))
C
C++   Link to the reduced wire banks
C
      KTWRR=NLINK('TWRR',ISLOT)
      IF (KTWRR.EQ.0) GO TO 999
      KTWLE=NLINK('TWLE',ISLOT)
      IF (KTWLE.EQ.0) THEN
        CALL RERROR('TFTWTB',4,'No wire pulse length info available.')
        GO TO 999
      ENDIF
C
      Z2CUT=ZTPCMX+ZALOTW
C
C++   Make a list of offsets to the wires in TWRR
C
      LEN= 2*NTWIRE(ISTYP)+LMHLEN
      CALL WBANK(IW,IWROFW,LEN,*998)
      IW(IWROFW+LMHCOL)=2
      IW(IWROFW+LMHROW)=NTWIRE(ISTYP)
      IWRLS=-1
      DO 3501 IWP=1,LROWS(KTWRR)
        IHIT=IW(KTWRR+LMHLEN+IWP)
        IWRNM=IBITS(IHIT,24,8)
        IF (IWRNM.LE.0.OR.IWRNM.GT.LROWS(IWROFW)) THEN
C
C........ Suppress error message for the channel which
C........ is used for the t0 measurement
C
          IF ((ISLOT.NE.4).OR.(IWRNM.NE.220))
     &       CALL RERROR('TFTWTB',6,'Input data error: '//
     &        'wire number out of range in TPC wire hit list.')
          IF (IWRLS.LE.0) GO TO 3501
          IWRNM=IWRLS
        ENDIF
        IF (IWRNM.NE.IWRLS) THEN
          IWRLS=IWRNM
          IW(KROW(IWROFW,IWRNM)+1)=IWP-1
          IW(KROW(IWROFW,IWRNM)+2)=0
        ENDIF
        IW(KROW(IWROFW,IWRNM)+2)=ITABL(IWROFW,IWRNM,2)+1
 3501 CONTINUE
C
C++   Loop over all wires in this sector and analyze one at a time
C++   Skip wires not in the readout, as well as the first and last
C++   wires read out (which are badly affected by gating pickup).
C
      DO 3800 IWRNM=ITLWIF(ISTYP)+1,ITLWIL(ISTYP)-1
C
C++     Skip wires which have been flagged dead by the calibration
C
        DO 3808 II=1,LROWS(ITKAPW)
          IF (IWRNM.EQ.IW(ITKAPW+LMHLEN+II)) THEN
            NPLS=ITABL(IWROFW,IWRNM,2)
            IF (NPLS.GT.0) THEN
              CALL RERROR('TFTWTB',5,
     &           'Wire flagged dead in TKAP has pulses in TWRR!')
              GO TO 3808
            ENDIF
            GO TO 3800
          ENDIF
 3808   CONTINUE
C
C++     Find intersections of all tracks with this wire
C++     Get the x position of the wire (coord perpendicular to wire)
C
        XW=TWIRE1(ISTYP)+FLOAT(IWRNM-1)*TWSTEP(ISTYP)
        YTST=TWIRLE(IWRNM,ISTYP)+WRTLTW
C
C++     Loop over all tracks and arcs in this sector
C
        IW(ITKWRW+LMHROW)=0
        DO 200 II=1,LROWS(ITGFTW)
C
C++       Get track parameters from the workbank
C
          RAD=ABS(1./RTABL(ITGFTW,II,JTGWIR))
          SGN=SIGN(1.0,RTABL(ITGFTW,II,JTGWIR))
          XCS(1)=RTABL(ITGFTW,II,JTGWXC)
          XCS(2)=RTABL(ITGFTW,II,JTGWYC)
          PHI0=RTABL(ITGFTW,II,JTGWP0)
          Z0=RTABL(ITGFTW,II,JTGWZ0)
          TANL=RTABL(ITGFTW,II,JTGWTL)
C
          Q=RAD**2 - (XW-XCS(1))**2
C
C++       Does this track intersect the wire even if wire is infinite?
C
          IF (Q.LE.0.) GO TO 200
          Q=SQRT(Q)
C
C++       Try both solutions of intersection of circle and line
C
          DO 100 ISOL=1,2
C
C++         Calculate y position of intersection (along wire)
C
            YW=XCS(2)+SGNA(ISOL)*Q
C
C++         Crossing point must be no more than slightly outside
C++         the ends of the wire.
C
            YABS=ABS(YW)
            IF (YABS.GT.YTST) GO TO 100
C
C++         Make a cut on the turning angle of the track
C
            BV(1)=XW-XCS(1)
            BV(2)=YW-XCS(2)
            THETA=ATAN2(BV(2),BV(1))
            IF (THETA.LT.0.) THETA=THETA+TWOPI
            ALPHA= PIBY2 + SGN*(THETA-PHI0)
            IF (ALPHA.LT.0.) THEN
              ALPHA=ALPHA+TWOPI
            ELSEIF (ALPHA.GT.TWOPI) THEN
              ALPHA=ALPHA-TWOPI
            ENDIF
            IF (ALPHA.GT.TRMXTW) GO TO 100
C
C++         Find the z position on the track at the intersection
C
            ZTRKS= Z0 + (RAD*ALPHA)*TANL
            IF (ZTRKS.LT.-ZALOTW .OR. ZTRKS.GT.Z2CUT) GO TO 100
C
C++         Find the angle of the track with the wire in x-y
C
            CA=ABS(BV(2)/RAD)
C
C++         Store all intersections in a workbank
C
            IW(ITKWRW+LMHROW)=LROWS(ITKWRW)+1
            IPT=LROWS(ITKWRW)
            RW(KROW(ITKWRW,IPT)+JTKWZI)=ZTRKS
            IW(KROW(ITKWRW,IPT)+JTKWTK)=II
            RW(KROW(ITKWRW,IPT)+JTKWYI)=YW
            RW(KROW(ITKWRW,IPT)+JTKWCA)=CA
            IW(KROW(ITKWRW,IPT)+JTKWFG)=0
  100     CONTINUE
  200   CONTINUE
C
C++     Skip this wire if no tracks intersect it
C
        IF (LROWS(ITKWRW).EQ.0) GO TO 3800
C
C++     Also skip wires if there are lots of tracks but no pulses, since
C++     that would be a sign of a dead wire.
C
        NPLS=ITABL(IWROFW,IWRNM,2)
        IF (LROWS(ITKWRW).GT.MXTNTW
     &                .AND.NPLS.EQ.0.AND.(.NOT.FZERTW)) THEN
          GO TO 3800
        ENDIF
C
C++     Loop over all pulses on this wire
C
        IOFF=ITABL(IWROFW,IWRNM,1)
        IHT2=IW(KTWRR+LMHLEN+IOFF+1)
        TWR2=FLOAT(IBITS(IHT2,0,13))/TPACKW
        ZP2=(TWR2-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
        FSKIP=.FALSE.
        DO 3700 MM=1,NPLS
          IWP=IOFF+MM
C
C++       Get the z position of this pulse
C
          TWR=TWR2
          ZP=ZP2
          IHTWD=IW(KTWRR+LMHLEN+IWP)
          ICHRG=IBITS(IHTWD,14,10)
C
C++       Find all tracks which match the time of this hit within a wind
C++       of +-ZWINTW.
C
          NMAT=0
          RESMN=9999.
          GOOD=.TRUE.
          DO 400 JJ=1,LROWS(ITKWRW)
            RES=ZP-RTABL(ITKWRW,JJ,JTKWZI)
            IF (ABS(RES).LT.ZWINTW) THEN
              II=ITABL(ITKWRW,JJ,JTKWTK)
              ITK=ITABL(ITGFTW,II,JTGWTK)
C
C++           Set a flag showing that this track does match a hit,
C++           so that it won't be considered as a below-threshold hit.
C
              IW(KROW(ITKWRW,JJ)+JTKWFG)=1
C
C++           Drop any hit which matches an arc from TARC
C
              IF (ITK.LT.0) GOOD=.FALSE.
C
C++           Drop hits which are too close to end of wire
C
              YABS= ABS(RTABL(ITKWRW,JJ,JTKWYI))
              IF (YABS.GT.(TWIRLE(IWRNM,ISTYP)-WRTLTW)) GOOD=.FALSE.
C
C++           Drop hits with too large track angle
C
              CA=RTABL(ITKWRW,JJ,JTKWCA)
              IF (CA.LT.CTMNTW) GOOD=.FALSE.
C
C++           Drop hits with matches across central membrane
C
              ZTRKS= RTABL(ITKWRW,JJ,JTKWZI)
              IF (ZTRKS.GT.ZTPCMX.OR.ZTRKS.LT.0.) GOOD=.FALSE.
C
C++           Drop the hit if more than MXMAT tracks match it
C
              IF (NMAT.GE.MXMAT) THEN
                GOOD=.FALSE.
              ELSE
                NMAT=NMAT+1
                IMAT(NMAT)=JJ
                DZMAT(NMAT)=RES
                IF (ABS(RES).LT.RESMN) THEN
                  RESMN=ABS(RES)
                  IMN=NMAT
                ENDIF
              ENDIF
            ENDIF
  400     CONTINUE
C
C++       Kill all hits which are too close to the preceeding or
C++       following hit.
C
          IF (FSKIP) GOOD=.FALSE.
          IF (MM.LT.NPLS) THEN
            IHT2=IW(KTWRR+LMHLEN+IWP+1)
            TWR2=FLOAT(IBITS(IHT2,0,13))/TPACKW
            ZP2=(TWR2-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
            IF (ABS(ZP2-ZP).LT.SPACTW) THEN
              FSKIP=.TRUE.
              GO TO 3700
            ELSE
              FSKIP=.FALSE.
            ENDIF
          ENDIF
          IF (NMAT.EQ.0 .OR. .NOT.GOOD) GO TO 3700
C
C++       Kill hits with zero charge.  These are the extra-long
C++       pulses for which the digitizations were thrown away by
C++       the online TPP wire reduction program.
C
          IF (ICHRG.EQ.0) GO TO 3700
C
C++       Give special consideration to hits which match 2 tracks.
C++       Often this is due to a track which has been broken in two
C++       by the pattern recognition.  We want to match only the
C++       half which passes over the wire in question.
C
          IF (NMAT.EQ.2) THEN
            KO=ITABL(ITKWRW,IMAT(1),JTKWTK)
            IR1O=ITABL(ITGFTW,KO,JTGWFR)
            K=ITABL(ITKWRW,IMAT(2),JTKWTK)
            IR1=ITABL(ITGFTW,K,JTGWFR)
            IF (IR1.LT.IR1O) THEN
              IR2=ITABL(ITGFTW,K,JTGWLR)
              IF (IR2.LT.IR1O) THEN
                YW= RTABL(ITKWRW,IMAT(2),JTKWYI)
                RADW2=XW**2 + YW**2
                IF (RADW2.LT.RCHK2(IR2)) THEN
                  NMAT=1
                  IMAT(1)=IMAT(2)
                  DZMAT(1)=DZMAT(2)
                  IMN=1
                ELSE
                  NMAT=1
                  IMN=1
                ENDIF
              ENDIF
            ELSEIF (IR1.GT.IR1O) THEN
              IR2O=ITABL(ITGFTW,KO,JTGWLR)
              IF (IR1.GT.IR2O) THEN
                YW= RTABL(ITKWRW,IMAT(1),JTKWYI)
                RADW2=XW**2 + YW**2
                IF (RADW2.GT.RCHK2(IR2O)) THEN
                  NMAT=1
                  IMAT(1)=IMAT(2)
                  DZMAT(1)=DZMAT(2)
                  IMN=1
                ELSE
                  NMAT=1
                  IMN=1
                ENDIF
              ENDIF
            ENDIF
          ELSEIF (NMAT.EQ.1.AND.(.NOT.FZERTW)) THEN
C
C++         Drop hits outside the radial range in which the
C++         track was measured.
C
            YW= RTABL(ITKWRW,IMAT(1),JTKWYI)
            RADW2=XW**2+YW**2
            II=ITABL(ITKWRW,IMAT(1),JTKWTK)
            IR1=ITABL(ITGFTW,II,JTGWFR)
            IF (RADW2.LT.RCHK2(IR1-1)) GO TO 3700
            IR2=ITABL(ITGFTW,II,JTGWLR)
            IF (RADW2.GT.RCHK2(IR2)) GO TO 3700
          ENDIF
C
C++       Second special consideration of hits that really match
C++       two tracks.  Keep track of a list of tracks that share hits.
C++       this is to speed things up later on in TRKELS.
C
          IF (NMAT.EQ.2) THEN
            KK = ITABL(ITKWRW,IMAT(1),JTKWTK)
            ITK1 = ITABL(ITGFTW,KK,JTGWTK)
            KK = ITABL(ITKWRW,IMAT(2),JTKWTK)
            ITK2 = ITABL(ITGFTW,KK,JTGWTK)
            IF (ITK1.EQ.ITK2) GOTO 11
            IF (LROWS(KTWOL).EQ.0) THEN
              IW(KROW(KTWOL,1)+JTWOT1) = ITK1
              IW(KROW(KTWOL,1)+JTWOT2) = ITK2
              IW(KTWOL+LMHROW)=LROWS(KTWOL)+1
              GOTO 11
            ENDIF
C
C++         Search to see that this track pair is not already in the ban
C
            DO 10 IPR = 1,LROWS(KTWOL)
              IF (ITK1.EQ.ITABL(KTWOL,IPR,1)) THEN
                IF (ITK2.EQ.ITABL(KTWOL,IPR,2)) GOTO 11
              ELSEIF (ITK1.EQ.ITABL(KTWOL,IPR,2)) THEN
                IF (ITK2.EQ.ITABL(KTWOL,IPR,1)) GOTO 11
              ENDIF
   10       CONTINUE
C
C++         So this is a new match
C
            IW(KTWOL+LMHROW)=LROWS(KTWOL)+1
            IF (LROWS(KTWOL)*LCOLS(KTWOL)+LMHLEN.GT.IW(KTWOL)) THEN
              LEN = IW(KTWOL) + LTWOLA*10
              CALL AUBOS('TWOL',0,LEN,KTWOL,IGARB)
              IF (IGARB.EQ.2) THEN
                JGARB = IGARB
                GOTO 999
              ENDIF
              IF (IGARB.EQ.1) THEN
                JGARB=IGARB
                KTWRR=NLINK('TWRR',ISLOT)
                KTWIT=IW(NTWIT)
                KTWTB=IW(NTWTB)
                KTWLE=NLINK('TWLE',ISLOT)
              ENDIF
            ENDIF
            IW(KROW(KTWOL,LROWS(KTWOL))+JTWOT1) = ITK1
            IW(KROW(KTWOL,LROWS(KTWOL))+JTWOT2) = ITK2
          ENDIF
   11     CONTINUE
C
C++       The closest track must be within +-0.5*ZWINTW for the hit
C++       to be considered to have a match.  Those additional tracks
C++       which are within +-ZWINTW constitute overlapping tracks.
C++       If there are any, then the hit is considered to be bad.
C++       ZWINTW must be less than a pulse length (less than SPACTW)
C++       so that it is not possible for a single track to match more
C++       than one pulse on the same wire within the cut 0.5*ZWINTW.
C
          IF (ABS(DZMAT(IMN)).GT.ZGCUT) GO TO 3700
C
C++       Cut out pulses if they are following too closely in time to
C++       a badly saturated pulse
C
          IF (ZWSATW.GT.0.) THEN
            DO 3716 IWHE=IWP-1,1,-1
              IHTE=IW(KTWRR+LMHLEN+IWHE)
              IWRE=IBITS(IHTE,24,8)
              IF (IWRE.NE.IWRNM) GO TO 3717
              ICHGE=IBITS(IHTE,14,10)
              IF (ICHGE.GE.1023) THEN
                TWRE=FLOAT(IBITS(IHTE,0,13))/TPACKW
                IF (TWR.GT.TWRE) THEN
                  IF (TWR-TWRE .LT. ZSCUT) GO TO 3700
                ELSE
                  CALL RERROR('TFTWTB',2,'Pulses not arranged in '//
     &                        'the proper time ordering in TWRR.')
                ENDIF
              ENDIF
 3716       CONTINUE
 3717       CONTINUE
          ENDIF
C
C++       For good hits which match only a single track, we make an
C++       even tighter cut on the residual.  Those which fail go into
C++       the bad category.
C
          IFLG=IBITS(IHTWD,13,1)
          IF (IFLG.EQ.1 .AND. NMAT.EQ.1) THEN
            IF (ABS(DZMAT(IMN)).GT.RSMXTW) THEN
              IFLG=0
            ENDIF
          ENDIF
C
C++       Fill the matches into the output banks
C++       Increase size of output bank, if necessary
C
          IW(KTWTB+LMHROW)=LROWS(KTWTB)+1
          IF ((LROWS(KTWTB)*LCOLS(KTWTB)+LMHLEN) .GT. IW(KTWTB)) THEN
            LEN=IW(KTWTB)+LROWS(KTWRR)*LCOLS(KTWTB)
            IW(1)=1
            CALL AUBOS('TWTB',0,LEN,KTWTB,IGARB)
            IF (IGARB.EQ.2) THEN
              JGARB=IGARB
              GO TO 999
            ENDIF
            IF (IGARB.EQ.1) THEN
              JGARB=IGARB
              KTWRR=NLINK('TWRR',ISLOT)
              KTWIT=IW(NTWIT)
              KTWOL=IW(NTWOL)
              KTWLE=NLINK('TWLE',ISLOT)
            ENDIF
          ENDIF
          IPTR=LROWS(KTWTB)
          SMPLN=0.
          DO 450 II=1,NMAT
            JJ=IMAT(II)
            KK=ITABL(ITKWRW,JJ,JTKWTK)
            ITK=ITABL(ITGFTW,KK,JTGWTK)
            COSL=RTABL(ITGFTW,KK,JTGWCL)
C
C++         It should never happen that one track picks up more than
C++         352 wires, so there should always be enough room in
C++         ITWATW for each track.  However, if the input data
C++         structure is screwed up, then bad things can happen
C++         if this protection is not here.
C
            NBAD=ITABL(KTWIT,ITK,JTWINB)
            NGOOD=ITABL(KTWIT,ITK,JTWING)
            IF (NBAD+NGOOD .GE. LCOLS(ITWATW)) THEN
              WRITE(TEXT,361) ITK,ISLOT,LCOLS(ITWATW)
  361         FORMAT('Track ',I3,' in sector ',I2,' has picked ',
     &               ' up more than ',I3,' wire pulses.&',
     &               'This indicates an error in the input',
     &               ' wire data format.')
              CALL RERROR('TFTWTB',1,TEXT)
              GO TO 450
            ENDIF
C
C++         Update count of hits in TWIT bank.
C++         Fill match into TWAT workbank for good and bad hits.
C++         The hit is bad if it matches more than one track or if it
C++         was marked bad in TWIRES (pulse too long).
C
            IF (NMAT.GT.1 .OR. IFLG.EQ.0) THEN
              IW(KROW(KTWIT,ITK)+JTWINB)=NBAD+1
              IW(KROW(ITWATW,ITK+1)+1-ITABL(KTWIT,ITK,JTWINB))=IPTR
            ELSE
              IW(KROW(KTWIT,ITK)+JTWING)=NGOOD+1
              IW(KROW(ITWATW,ITK)+ITABL(KTWIT,ITK,JTWING))=IPTR
            ENDIF
C
C++         Accumulate the sample length for all matched tracks
C
            CA=RTABL(ITKWRW,JJ,JTKWCA)
            SMPLN=SMPLN + TWSTEP(ISTYP)/CA/COSL
C
C++         Store the match in TWTB for the closest track
C
            IF (II.EQ.IMN) THEN
              IW(KROW(KTWTB,IPTR)+JTWTWI)=ISLOT*65536 + IWP
              CHRG=FLOAT(ICHRG*JCPAKW)
C
C++           Apply calibration/normalization corrections to charge
C
              CHRG= CHRG*RNRMCL(ISLOT)
C
C++           Wire number correction
C
              IF (ISTYP.EQ.1) THEN
                XWRNM=FLOAT(IWRNM)
              ELSE
                XWRNM=FLOAT(IWRNM+NTWIRE(1))
              ENDIF
              PCOR=((P3*XWRNM+P2)*XWRNM+P1)*XWRNM+P0
              CHRG= CHRG/PCOR
C
C++           Correction for distance from sector edge.  Don't bother
C++           with this for 2-track dE/dx, since it becomes too
C++           confusing in that case.
C
              YW=RTABL(ITKWRW,JJ,JTKWYI)
              IF (NMAT.EQ.1) THEN
                DELY=TWIRLE(IWRNM,ISTYP)-ABS(YW)
                CHRG=CHRG/(1.-EDGECL(1,ISTYP)
     &                 * EXP(-DELY/EDGECL(2,ISTYP)))
              ENDIF
C
              ZTRKS=RTABL(ITKWRW,JJ,JTKWZI)
C
              RW(KROW(KTWTB,IPTR)+JTWTCE)=CHRG
              RW(KROW(KTWTB,IPTR)+JTWTPN)=ZTRKS
              RADW2=XW**2 + YW**2
              RW(KROW(KTWTB,IPTR)+JTWTRP)=SQRT(RADW2)
            ENDIF
C
  450     CONTINUE
          RW(KROW(KTWTB,IPTR)+JTWTSL)=SMPLN
 3700   CONTINUE
C
C++     Go back over the track intersections and count those which
C++     did not match any pulse as below threshold (zeroes)
C
        if (fzertw) goto 3800
        DO 3900 JJ=1,LROWS(ITKWRW)
          IF (ITABL(ITKWRW,JJ,JTKWFG).NE.0) GO TO 3900
          KK=ITABL(ITKWRW,JJ,JTKWTK)
C
C++       Don't count secondary arcs of tracks!
C
          ITK=ITABL(ITGFTW,KK,JTGWTK)
          IF (ITK.LT.0) GO TO 3900
C
C++       Cut on the distance to the end of the wire
C
          YW=RTABL(ITKWRW,JJ,JTKWYI)
          IF (ABS(YW).GT.(TWIRLE(IWRNM,ISTYP)-WRTLTW)) GO TO 3900
C
C++       Cut on the track angle and z of the track
C
          CA=RTABL(ITKWRW,JJ,JTKWCA)
          IF (CA.LT.CTMNTW) GO TO 3900
          ZTRKS=RTABL(ITKWRW,JJ,JTKWZI)
          IF (ZTRKS.GT.ZTPCMX.OR.ZTRKS.LT.0.) GO TO 3900
C
C++       Check that there is no other track intersection close
C++       to this one
C
          DO 3934 LL=1,LROWS(ITKWRW)
            IF (LL.EQ.JJ) GO TO 3934
            ZTRK2=RTABL(ITKWRW,LL,JTKWZI)
            IF (ABS(ZTRK2-ZTRKS).LT.SPACTW) GO TO 3900
 3934     CONTINUE
C
C++       Check carefully, using pulse length information, that
C++       this track really doesn't overlap any valid data.
C
          DO 3965 MM=1,NPLS
            IWP=IOFF+MM
            IHTWD=IW(KTWRR+LMHLEN+IWP)
C
C++         Correct the time back to the pulse center
C
            TWR=FLOAT(IBITS(IHTWD,0,13))/TPACKW
            ZPUL=(TWR-TZOFFS(9)-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
C
C++         Get the pulse length
C
            IWORD=KTWLE+LMHLEN+1+(IWP-1)/4
            IBIT0=24-8*MOD(IWP-1,4)
            RLNPH=FLOAT(IBITS(IW(IWORD),IBIT0,8)+4)
     &                                 * TVDRFT(IENDTP(ISLOT))
            ZP1=ZPUL - 0.5*RLNPH - DZNLTW*1.5
            ZP2=ZPUL + 0.5*RLNPH + DZNLTW
C
            IF (ZTRKS.GT.ZP1.AND.ZTRKS.LT.ZP2) GO TO 3900
C
C++         Cut if the track is closely following a badly saturated puls
C
            ZPUL=(TWR-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
            IF (ZTRKS-ZPUL.LT.ZWSATW.AND.ZTRKS.GT.ZPUL) THEN
              ICHGE=IBITS(IHTWD,14,10)
              IF (ICHGE.GE.1023) GO TO 3900
            ENDIF
 3965     CONTINUE
C
C++       Add a hit to the output with zero pulse height
C
          IW(KTWTB+LMHROW)=LROWS(KTWTB)+1
          IF ((LROWS(KTWTB)*LCOLS(KTWTB)+LMHLEN) .GT. IW(KTWTB)) THEN
            LEN=IW(KTWTB)+LROWS(KTWRR)*LCOLS(KTWTB)
            IW(1)=1
            CALL AUBOS('TWTB',0,LEN,KTWTB,IGARB)
            IF (IGARB.EQ.2) THEN
              JGARB=IGARB
              GO TO 999
            ENDIF
            IF (IGARB.EQ.1) THEN
              JGARB=IGARB
              KTWRR=NLINK('TWRR',ISLOT)
              KTWIT=IW(NTWIT)
              KTWOL=IW(NTWOL)
              KTWLE=NLINK('TWLE',ISLOT)
            ENDIF
          ENDIF
          IPTR=LROWS(KTWTB)
          NBAD=ITABL(KTWIT,ITK,JTWINB)
          NGOOD=ITABL(KTWIT,ITK,JTWING)
          IF (NBAD+NGOOD .GE. LCOLS(ITWATW)) THEN
            WRITE(TEXT,361) ITK,ISLOT,LCOLS(ITWATW)
            CALL RERROR('TFTWTB',1,TEXT)
            GO TO 3900
          ENDIF
          IW(KROW(KTWIT,ITK)+JTWING)=NGOOD+1
          IW(KROW(ITWATW,ITK)+ITABL(KTWIT,ITK,JTWING))=IPTR
          COSL=RTABL(ITGFTW,KK,JTGWCL)
          RW(KROW(KTWTB,IPTR)+JTWTSL)=TWSTEP(ISTYP)/CA/COSL
          IW(KROW(KTWTB,IPTR)+JTWTWI)=ISLOT*65536 + IWRNM
          RW(KROW(KTWTB,IPTR)+JTWTCE)=0.
          RW(KROW(KTWTB,IPTR)+JTWTPN)=ZTRKS
          RW(KROW(KTWTB,IPTR)+JTWTRP)=SQRT(XW**2+YW**2)
 3900   CONTINUE
 3800 CONTINUE
C
  999 CONTINUE
      CALL WDROP(IW,IWROFW)
      RETURN
  998 CONTINUE
      CALL RERROR('TFTWTB',3,'No room to create workbank.')
      END
#endif
