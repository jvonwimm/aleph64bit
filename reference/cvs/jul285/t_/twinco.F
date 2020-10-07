      SUBROUTINE TWINCO(ISLOT,IROW,ISC,ICH1,ICH2,
     &                                       ITPCO,RTPCO,IRET)
C
C--------------------------------------------------------------------
C! Make 2 coords from 1 subcluster
C
C  Author:  R. Johnson   14-12-89
C
C     Input:  ISLOT         /I    TPC sector slot number
C             IROW          /I    TPC global row number of subcluster
C             ISC           /I    Subcluster number in bank TSCL
C             ICH1          /I    1st chain contributing to the
C                                 subcluster (pointer into TCHA)
C             ICH2          /I    2nd chain contributing to ISC
C     Output: ITPCO         /I    Two rows of TPCO for new coords
C             RTPCO         /I    Same as ITPCO
C             IRET          /I    =0 if calculation successful
C
C     Called by TOVRLP
C
C--------------------------------------------------------------------
#ifndef DOC
C
#include "alcons.h"
#include "tparac.h"
#include "tchajj.h"
#include "tscljj.h"
#include "tspujj.h"
#include "bcs.h"
#include "tpccut.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpccon.h"
#include "tpcojj.h"
#include "tflags.h"
C
      LOGICAL FIRST
      DIMENSION ITPCO(LTPCOA,*),RTPCO(LTPCOA,*),JCH(2),HP(5),HPT(5)
      DIMENSION S(2),PHIP(2),ZP(2),ZB(2),RPHP(2),RS(2),RPHS(2),ZS(2)
      DIMENSION R(2),PHI(2),Z(2),RPHI(2),ERRM(3)
      DIMENSION RPULS(LMXPCL),RTIME(LMXPCL)
      DATA FIRST/.TRUE./
      DATA CHRDMN/90./       !Minimum radius of curvature in cm
      DATA ATKPMX/0.10/      !Maximum angle of track with pads
      DATA RMNRTP/0.20/      !Minimum ratio of edge ph to adjacent ph
                             !in order to use only edge for time
      DATA RMNPHE/15./       !Minimum edge ph in order to use only the
                             !edge pulse for the time estimate
C
C-------------------- Statement Function Definitions ---------------
C
#include "bmacro.h"
C
C-------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTCHA=NAMIND('TCHA')
      ENDIF
C
      KTCHA=IW(NTCHA)
      IF (KTCHA.EQ.0) THEN
        IRET=2
        GO TO 999
      ENDIF
      KTSCL=NLINK('TSCL',ISLOT)
      IF (KTSCL.EQ.0) THEN
        IRET=2
        GO TO 999
      ENDIF
      KTSPU=NLINK('TSPU',ISLOT)
      IF (KTSPU.EQ.0) THEN
        IRET=2
        GO TO 999
      ENDIF
C
      ISTYP=ITPTYP(ISLOT)
C
C++   Require both tracks to be radial at the padrow in question
C
      JCH(1)=ICH1
      JCH(2)=ICH2
      DO 100 I=1,2
        ICH=JCH(I)
        RAD=1.0/RTABL(KTCHA,ICH,JTCHIR)
        IF (ABS(RAD).LT.CHRDMN) THEN
          IRET=3
          GO TO 999
        ENDIF
C
C++     Change the helix parameters to the standard convention
C
        HP(1)=-RTABL(KTCHA,ICH,JTCHIR)
        HP(2)= RTABL(KTCHA,ICH,JTCHTL)
        HP(3)= RTABL(KTCHA,ICH,JTCHP0)
        HP(4)= SIGN(1.0,HP(1))*RTABL(KTCHA,ICH,JTCHD0)
        HP(5)= RTABL(KTCHA,ICH,JTCHZ0)
C
C++     Convert to the TPC coordinate system
C
        CALL TGHPAL(HP,HPT)
C
C++     Find intersection of the track with the padrow
C
        CALL THLCIR(HPT,TPPROW(IROW),S,PHIP,ZP,IER)
        IF (IER.NE.0) THEN
          IRET=4
          GO TO 999
        ENDIF
C
C++     Get angle of the track with the radial direction
C
        SP=S(1)/SQRT(1.+HPT(2)**2)
        RADCU=1./HPT(1)
        ALPHA=SP/RADCU
        THETA= ALPHA + HPT(3)
        IF (THETA.LT.0.) THEN
          THETA=THETA+TWOPI
        ELSEIF (THETA.GE.TWOPI) THEN
          THETA=THETA-TWOPI
        ENDIF
        ALPH=THETA-PHIP(1)
        IF (ALPH.LT.-PI) THEN
          ALPH=ALPH+TWOPI
        ELSEIF (ALPH.GE.PI) THEN
          ALPH=ALPH-TWOPI
        ENDIF
        IF (ABS(ALPH).GT.ATKPMX) THEN
          IRET=5
          GO TO 999
        ENDIF
  100 CONTINUE
C
C++   Link to the subcluster and subpulse banks
C
      KTSCL=NLINK('TSCL',ISLOT)
      KTSPU=NLINK('TSPU',ISLOT)
      IF (KTSCL.EQ.0 .OR. KTSPU.EQ.0) THEN
        CALL RERROR('TWINCO',1,'Banks TSCL and TSPU are missing.')
        IRET=6
        GO TO 999
      ENDIF
C
C++   Find which time and charge estimators to use.
C
      IALGW=ITABL(KTSCL,ISC,JTSCAW)
      IQE=IALGW/1000
      IF (IQE.EQ.0) THEN
        IRET=7
        GO TO 999
      ENDIF
      ITE=IALGW-1000*IQE
      IF (ITE.EQ.0) THEN
        IRET=8
        GO TO 999
      ENDIF
C
C++   Get the padrow number in the sector
C
      ISTYP=ITPTYP(ISLOT)
      IF (ISTYP.EQ.1) THEN
        IROWS=IROW
      ELSE
        IROWS=IROW-NTPDRW(1)
      ENDIF
C
C++   Get the pad pulse heights
C
      CALL TGETQT(IROWS,KTSCL,ISC,KTSPU,IQE,RPULS,RTIME,NSPL,JP1,IFLG)
      IF (IFLG.NE.0) THEN
        IRET=9
        GO TO 999
      ENDIF
C
C++   We must have at least 4 pulses for twin coordinates
C
      IF (NSPL.LT.4) THEN
        IRET=10
        GO TO 999
      ENDIF
C
C++   Remove pulses which are too small.
C
      IF (NSPL.GT.4) THEN
        IOFSP=ITABL(KTSCL,ISC,JTSCPO)
        PKMX=0.
        DO 834 I=1,ITABL(KTSCL,ISC,JTSCNS)
          PKMX=AMAX1(PKMX,RTABL(KTSPU,IOFSP+I,JTSPPC))
  834   CONTINUE
      ENDIF
C
  856 IF (NSPL.EQ.4) GO TO 859
        NSAV=NSPL
        IF (RPULS(JP1).LT.RPULS(JP1+NSPL-1)) THEN
          PEAK=RTABL(KTSPU,IOFSP+JP1,JTSPPC)
          RAT=PEAK/PKMX
          IF (PEAK.LT.CMNPTP .AND. RAT.LT.RLTHTP) THEN
            NSPL=NSPL-1
            JP1=JP1+1
          ENDIF
        ELSE
          PEAK=RTABL(KTSPU,IOFSP+JP1+NSPL-1,JTSPPC)
          RAT=PEAK/PKMX
          IF (PEAK.LT.CMNPTP .AND. RAT.LT.RLTHTP) THEN
            NSPL=NSPL-1
          ENDIF
        ENDIF
        IF (NSPL.NE.NSAV) GO TO 856
  859 CONTINUE
C
C++   We cannot handle here any cluster with more than 5 pulses
C
      IF (NSPL.GT.5) THEN
        IRET=11
        GO TO 999
      ENDIF
C
C++   In case of five pads, don't use the edge pad with the smallest
C++   pulse height.
C
      IF (NSPL.EQ.5) THEN
        IF (RPULS(JP1).LT.RPULS(JP1+NSPL-1)) THEN
          JP1=JP1+1
          NSPL=NSPL-1
        ELSE
          NSPL=NSPL-1
        ENDIF
      ENDIF
C
C++   To get the times, average two subpulses for one coordinate and
C++   the other two for the other coordinate.  Or better yet, if the
C++   edge pulse has a large enough charge, use only its time.
C
      IF (RPULS(JP1)/RPULS(JP1+1).GT.RMNRTP
     &                                .AND. RPULS(JP1).GT.RMNPHE) THEN
        CALL TSZCOR(ITE,RTIME(JP1),RPULS(JP1),1,ZB(1))
      ELSE
        CALL TSZCOR(ITE,RTIME(JP1),RPULS(JP1),2,ZB(1))
      ENDIF
      IF (RPULS(JP1+NSPL-1)/RPULS(JP1+NSPL-2).GT.RMNRTP
     &                         .AND. RPULS(JP1+NSPL-1).GT.RMNPHE) THEN
        CALL TSZCOR(ITE,RTIME(JP1+NSPL-1),RPULS(JP1+NSPL-1),1,ZB(2))
      ELSE
        CALL TSZCOR(ITE,RTIME(JP1+NSPL-2),RPULS(JP1+NSPL-2),2,ZB(2))
      ENDIF
C
C++   Get the pad response width
C
      ZDRFT=(ZB(1)-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
      PRFS2 = TPRF2(ZDRFT,ISLOT,0,0.,0.)
C
C++   Calculate two r*phi coordinates.  This gives position with
C++   respect to the first pad.
C
      CALL TCO4PA(1,RPULS(JP1),PRFS2,RPHP(1),RPHP(2),IERR)
      IF (IERR.NE.0) THEN
        IRET=13
        GO TO 999
      ENDIF
C
C++   Add the number of the first pad to get r*phi in pad units
C
      IPDMN=ITABL(KTSCL,ISC,JTSCFP)+JP1-1
      RPHP(1)=RPHP(1)+FLOAT(IPDMN)-1.0
      RPHP(2)=RPHP(2)+FLOAT(IPDMN)-1.0
C
      DO 888 ICO=1,2
C
C++     Convert the r*phi and z to units of centimeters in the sector
C++     frame of reference, making any necessary corrections for
C++     misalignment of the pads within the sector.
C
        CALL TSCOOR(ISLOT,IROWS,RPHP(ICO),ZB(ICO),
     &                             RS(ICO),RPHS(ICO),ZS(ICO))
C
C++     Transform the coordinate to the ALEPH frame of reference,
C++     including all necessary alignment corrections.
C
        IF (FTPC90) THEN
          R(ICO)=RS(ICO)
          PHI(ICO)=RPHS(ICO)/RS(ICO)
          IF (PHI(ICO).LT.0.) PHI(ICO)=PHI(ICO)+TWOPI
          RPHI(ICO)=R(ICO)*PHI(ICO)
          Z(ICO)=ZS(ICO)
        ELSE
          CALL TCTGLB(ISLOT,RS(ICO),RPHS(ICO),ZS(ICO),
     &                                     R(ICO),RPHI(ICO),Z(ICO))
        ENDIF
C
C++     Make an estimate of the uncertainty.  At least for now this is
C++     done the same as for an ordinary coordinate, using the full
C++     cluster width and length in the parameterization. . .
C
        MSPL=ITABL(KTSCL,ISC,JTSCNS)
        WPS=RTABL(KTSCL,ISC,JTSCSP)
        WZS=RTABL(KTSCL,ISC,JTSCSB)
        IQS=ITABL(KTSCL,ISC,JTSCTC)
        CALL TCOERR(ERRM,MSPL,WPS,WZS,IQS)
C
C++     Correct for field distortions
C
        PHI(ICO)= RPHI(ICO)/R(ICO)
        IF (FFCORR) THEN
          MODE=0
          IF (FFMODE) MODE=1
          CALL TLACOR(R(ICO),PHI(ICO),Z(ICO),RCR,PHICR,ZCR,MODE)
          R(ICO)=RCR
          PHI(ICO)=PHICR
          Z(ICO)=ZCR
        ENDIF
        IF (FECORR) CALL TCRTRA(IENDTP(ISLOT),R(ICO),PHI(ICO),Z(ICO),
     &                          RCR,PHICR)
        R(ICO)=RCR
        PHI(ICO)=PHICR
C
C Correct coordinate z for time-of-flight
C
        CALL TCRTOF(BFIELD,R(ICO),Z(ICO),0,' ',ZCR)
        Z(ICO)=ZCR
C
C Correct coordinate for z distortions measured by VDET
C
        CALL TZCRVD(IENDTP(ISLOT),R(ICO),Z(ICO),ZCR)
        Z(ICO)=ZCR
        CALL TZCSVD(ISLOT,R(ICO),PHI(ICO),Z(ICO),ZCR)
        Z(ICO)=ZCR
C
C++   Correct for Residual field distortions
C
        CALL TCORES(IENDTP(ISLOT),IROW,R(ICO),PHI(ICO),Z(ICO),
     >                                 RCR,PHICR,ZCR)
        R(ICO)  =RCR
        PHI(ICO)=PHICR
        Z(ICO)  =ZCR
        CALL TFICOR(IENDTP(ISLOT),IROW,R(ICO),PHI(ICO),Z(ICO),
     >                                 RCR,PHICR,ZCR)
        R(ICO)  =RCR
        PHI(ICO)=PHICR
        Z(ICO)  =ZCR
C
C++     Fill the output arrays for the coordinates
C
        IPDCH=ITABL(KTSCL,ISC,JTSCFP)+ITABL(KTSCL,ISC,JTSCNS)/2
        ITPCO(JTPCIN,ICO)=ISLOT*1000+IROW*100000+IPDCH
        RTPCO(JTPCRV,ICO)=R(ICO)
        RTPCO(JTPCPH,ICO)=PHI(ICO)
        RTPCO(JTPCZV,ICO)=Z(ICO)
        RTPCO(JTPCSR,ICO)=ERRM(1)
        RTPCO(JTPCSZ,ICO)=ERRM(3)
        ITPCO(JTPCOF,ICO)=2
        ITPCO(JTPCTN,ICO)=0
        ITPCO(JTPCIT,ICO)=0
        ITPCO(JTPCCN,ICO)=ISC
        RTPCO(JTPCRR,ICO)=RPHS(ICO)
        RTPCO(JTPCRZ,ICO)=ZS(ICO)
  888 CONTINUE
      IRET=0
C
  999 CONTINUE
      RETURN
      END
#endif
