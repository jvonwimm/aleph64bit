      SUBROUTINE TCOOR(IROW,IC,KTCLU,KTSPU,KTSCL,KTHRP)
C
C----------------------------------------------------------------------
C! Calculate coordinates from subclusters
C!
C!    Author:    R. Johnson 20-09-86
C!    Modified:  R. Johnson  8-04-88
C!           D. Casper  09-06-95  Support for Gaussian charge estimator
C!                                for pad dE/dx in call to TCOALG
C!           D. Casper  12-06-95  Support for low momentum track coords
C!                                Store #pads in subcluster, instead of
C!                                cluster, in TBCO
C!
C!    Input:
C!       IROW     /I       Padrow number within the sector
C!       IC       /I       Subcluster number for this sector
C!       KTCLU    /I       BOS offset to TCLU cluster bank
C!       KTSPU    /I       BOS offset to TSPU subpulse bank
C!       KTSCL    /I       BOS offset to TSCL subcluster bank
C!       KTHRP    /I       BOS offset to THRP TPD threshold pointers
C!
C!    Called by TCODRV
C!
C!    Description
C!    ===========
C!    This routine cycles through all subclusters in TSCL row by
C!    row (using the TSRL bank to point to row beginnings in
C!    TSCL) over all TPC sectors.  For each good cluster, a
C!    coordinate is calculated by TCOALG.  All coordinates for
C!    the entire TPC are filled into the TPCO bank and
C!    the bank TCRL is filled with pointers to the row beginnings
C!    in TPCO.
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "alcons.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tsrljj.h"
#include "tpcojj.h"
#include "tbcojj.h"
#include "tspujj.h"
#include "tscljj.h"
#include "tcrljj.h"
#include "thpljj.h"
#include "thrpjj.h"
#include "tclujj.h"
#include "bcs.h"
#include "tpccon.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tflags.h"
#include "tpccut.h"
#include "tzcorr.h"
C
      DIMENSION ZCUT(LTPDRO)
      DIMENSION RPULS(LMXPCL),RTIME(LMXPCL),ERRM(3)
      LOGICAL BAD,FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTBCO=NAMIND('TBCO')
        NTCRL=NAMIND('TCRL')
        NTPCO=NAMIND('TPCO')
        NTHPL=NAMIND('THPL')
C
C++     For each padrow, calculate a limit on how close the coordinate
C++     can be to the endplate before becoming distorted due to the
C++     track passing through the endplate and not traversing the full
C++     length of the pad (assuming the track came from the I.P.)
C
        DO 10 I=1,LTPDRO
          ZCUT(I)=0.5*TPDHGT(1)*ZTPCMX/(TPPROW(I)+0.5*TPDHGT(1)) + 0.1
   10   CONTINUE
      ENDIF
C
C++   Get the sector slot number
C
      ISLOT=IW(KTSCL-2)
C
C++   Skip subclusters with multiple peaks or saturation or
C++   large overlapping pulses
C
      BAD=.FALSE.
      IQW=IW(KROW(KTSCL,IC)+JTSCQU)
C
C++   Find which time and charge estimators to use.
C
      IALGW=ITABL(KTSCL,IC,JTSCAW)
      IQE=IALGW/1000
      ITE=IALGW-1000*IQE
      IF (IQW.GT.99) THEN
        IFLG=20
        GO TO 100
      ENDIF
      IF (IQE.EQ.0) THEN
        IFLG=30
        GO TO 100
      ENDIF
      IF (ITE.EQ.0) THEN
        IFLG=40
        GO TO 100
      ENDIF
C
C++   Make lists of charge and time estimates for the subpulses.  Here
C++   we also handle the problem of half pads by adding them together,
C++   if possible, to give corresponding full pads.
C
      CALL TGETQT(IROW,KTSCL,IC,KTSPU,IQE,RPULS,RTIME,NSPL,JP1,IFLG)
      IF (IFLG.NE.0) GO TO 100
C
C++   Check that the subcluster is not adjacent to a dead pad.  If it
C++   is, then reject the coordinate if the edge pad is a local
C++   maximum or if the subcluster contains greater than 3 pads.
C
      IPDMN=ITABL(KTSCL,IC,JTSCFP)
      IF (KTHRP.NE.0) THEN
        KTHPL=IW(NTHPL)
        IF (KTHPL.NE.0) THEN
          IP1=IPDMN-1
          IP2=ITABL(KTSCL,IC,JTSCNS)+IPDMN
          DO 68 I=1,ITABL(KTHRP,IROW,JTHRNB)
            IBD= I + ITABL(KTHRP,IROW,JTHRPT)
            ITHR=ITABL(KTHPL,IBD,JTHPTH)
            IF (ITHR.NE.255) GO TO 68
            IBPAD=ITABL(KTHPL,IBD,JTHPPN)
            IF (IBPAD.GT.IP2) GO TO 69
            IF (IBPAD.EQ.IP1) THEN
              IF (NSPL.GT.3 .OR. RPULS(JP1).GE.RPULS(JP1+1)) THEN
                IFLG=50
                GO TO 100
              ENDIF
            ELSEIF (IBPAD.EQ.IP2) THEN
              IF (NSPL.GT.3 .OR. RPULS(JP1+NSPL-1)
     &                           .GE. RPULS(JP1+NSPL-2)) THEN
                IFLG=50
                GO TO 100
              ENDIF
            ENDIF
   68     CONTINUE
   69     CONTINUE
        ENDIF
      ENDIF
C
C++   Remove pulses which are too small.  In 3-pad clusters, remove
C++   the smallest pulse only if the two side ones are not balanced.
C
      CALL TRMVEP(KTSCL,IC,KTSPU,RPULS,JP1,NSPL)
      IPDMN=IPDMN+JP1-1
C
C++   Average the time estimates to give z for the cluster in buckets.
C
      CALL TSZCOR(ITE,RTIME(JP1),RPULS(JP1),NSPL,ZB)
C
C++   Apply the appropriate coordinate algorithm for r*phi (in pad units
C
      ZDRFT=(ZB-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
      PFRS2=TPRF2(ZDRFT,ISLOT,0,0.,0.)
      CALL TCOALG(ISLOT,IROW,IQW,NSPL,RPULS(JP1),PFRS2,
     &                   IPDMN,RPHIP,QCO,IFLG)
      IF (IFLG.NE.0) THEN
        GO TO 100
      ENDIF
C
C Apply a fudge in the case that two half pads are used
C
      CALL TNHLFP(ISLOT,IROW,KROW(KTSCL,IC),NHALF)
      IF (NHALF.EQ.2) THEN
        CALL THPFDG(IPDMN,ISLOT,IROW,RPHIP,RFUDG,IFLG)
        IF (IFLG.NE.0) THEN
          GO TO 100
        ENDIF
        RPHIP=RFUDG
      ENDIF
      GO TO 200
C
C++   Calculate rough r*phi and z values for subclusters which have
C++   failed the previous tests and calculations.
C
  100 CONTINUE
        IW(KROW(KTSCL,IC)+JTSCNC)=-IFLG
        BAD=.TRUE.
        NSPL=ITABL(KTSCL,IC,JTSCNS)
        IPDMN=ITABL(KTSCL,IC,JTSCFP)
C
C++     Average the first and last pad numbers for r*phi
C
        RPHIP=FLOAT(IPDMN)+FLOAT(NSPL-1)/2.
C
C++     Average the minimum and maximum threshold crossings for z
C
        IOFSP=IW(KROW(KTSCL,IC)+JTSCPO)
        T1MIN=513.
        T2MAX=0.
        DO 92 J=1,NSPL
          ISP=IOFSP+J
          T1=RTABL(KTSPU,ISP,JTSPT1)
          T2=RTABL(KTSPU,ISP,JTSPT2)
          IF (T1.LT.T1MIN) T1MIN=T1
          IF (T2.GT.T2MAX) T2MAX=T2
   92   CONTINUE
        ZB= 0.5*(T1MIN+T2MAX) + TZOFFS(ITE)
C
C++   Convert the r*phi and z to units of centimeters in the sector
C++   frame of reference, making any necessary corrections for
C++   misalignment of the pads within the sector.
C
  200 CONTINUE
      CALL TSCOOR(ISLOT,IROW,RPHIP,ZB,RS,RPHIS,ZS)
C
      ISTYP=ITPTYP(ISLOT)
      IF (ISTYP.EQ.1) THEN
        IROWG=IROW
      ELSE
        IROWG=IROW+NTPDRW(1)
      ENDIF
C
C++   See if the coordinate is too close to the endplate
C
      IF (ZS.LT.ZCUT(IROWG)) BAD=.TRUE.
C
C++   Transform the coordinate to the ALEPH frame of reference,
C++   including all necessary alignment corrections.
C
      IF (FTPC90) THEN
        R=RS
        PHI=RPHIS/RS
        IF (PHI.LT.0.) PHI=PHI+TWOPI
        RPHI=R*PHI
        Z=ZS
      ELSE
        CALL TCTGLB(ISLOT,RS,RPHIS,ZS,R,RPHI,Z)
      ENDIF
C
C++   For good coordinates, make an estimate of the uncertainty.
C
      IF (.NOT.BAD) THEN
        MSPL=ITABL(KTSCL,IC,JTSCNS)
        WPS=RTABL(KTSCL,IC,JTSCSP)
        WZS=RTABL(KTSCL,IC,JTSCSB)
        IQS=ITABL(KTSCL,IC,JTSCTC)
        CALL TCOERR(ERRM,MSPL,WPS,WZS,IQS)
      ENDIF
C
C++   Fill the results into the appropriate banks, and make a rowlist
C++   for the good coordinates.
C
      IF (BAD) THEN
        KTBCO=IW(NTBCO)
        IW(KTBCO+LMHROW)=LROWS(KTBCO)+1
        NBC=LROWS(KTBCO)
        IPDCH=IPDMN+NSPL/2
        IW(KROW(KTBCO,NBC)+JTBCIN)=ISLOT*1000+IROWG*100000+IPDCH
        RW(KROW(KTBCO,NBC)+JTBCRV)=R
        RW(KROW(KTBCO,NBC)+JTBCPH)=RPHI/R
        RW(KROW(KTBCO,NBC)+JTBCZV)=Z
        ICL=ITABL(KTSCL,IC,JTSCCL)
        IBK1=ITABL(KTCLU,ICL,JTCLFB)
        IBK2=ITABL(KTCLU,ICL,JTCLLB)
        IW(KROW(KTBCO,NBC)+JTBCRW)=ITABL(KTSCL,IC,JTSCNS)
        IW(KROW(KTBCO,NBC)+JTBCZW)=IBK2-IBK1+1
        IW(KROW(KTBCO,NBC)+JTBCSC)=IC
      ELSE
        KTCRL=IW(NTCRL)
        IW(KROW(KTCRL,IROWG)+JTCRNC)=IW(KROW(KTCRL,IROWG)+JTCRNC)+1
        IF (IENDTP(ISLOT).EQ.1) THEN
          IW(KROW(KTCRL,IROWG)+JTCRN1)
     &                            = IW(KROW(KTCRL,IROWG)+JTCRN1)+1
        ENDIF
        KTPCO=IW(NTPCO)
        IW(KTPCO+LMHROW)=IW(KTPCO+LMHROW)+1
        NC=IW(KTPCO+LMHROW)
        IPDCH=IPDMN+NSPL/2
C
C++     Correct only good coordinates for drift field distortions
C
        PHI= RPHI/R
        IF (FFCORR) THEN
          MODE=0
          IF (FFMODE) MODE=1
          CALL TLACOR(R,PHI,Z,RCR,PHICR,ZCR,MODE)
          R=RCR
          PHI=PHICR
          Z=ZCR
        ENDIF
C
C++     Correct coordinate z for time-of-flight
C
        CALL TCRTOF(BFIELD,R,Z,0,' ',ZCR)
        Z=ZCR
C
C++     Correct coordinate for transverse drift
C
        IF (FECORR) CALL TCRTRA(IENDTP(ISLOT),R,PHI,Z,RC,PHIC)
C
C++     Correct for z distortions measured by VDET
C
        CALL TZCRVD(IENDTP(ISLOT),RC,Z,ZCR)
        Z=ZCR
        CALL TZCSVD(ISLOT,RC,PHIC,Z,ZCR)
        Z=ZCR
C
C++   Correct for Residual field distortions
C
        CALL TCORES(IENDTP(ISLOT),IROWG,RC,PHIC,Z,RCR,PHICR,ZCR)
        RC  =RCR
        PHIC=PHICR
        Z   =ZCR
        CALL TFICOR(IENDTP(ISLOT),IROWG,RC,PHIC,Z,RCR,PHICR,ZCR)
        RC  =RCR
        PHIC=PHICR
        Z   =ZCR
C
        IW(KROW(KTPCO,NC)+JTPCIN)=ISLOT*1000+IROWG*100000+IPDCH
        RW(KROW(KTPCO,NC)+JTPCRV)=RC
        RW(KROW(KTPCO,NC)+JTPCPH)=PHIC
        RW(KROW(KTPCO,NC)+JTPCZV)=Z
        RW(KROW(KTPCO,NC)+JTPCSR)=ERRM(1)
        RW(KROW(KTPCO,NC)+JTPCSZ)=ERRM(3)
        IW(KROW(KTPCO,NC)+JTPCOF)=1
        IW(KROW(KTPCO,NC)+JTPCTN)=0
        IW(KROW(KTPCO,NC)+JTPCIT)=0
        RW(KROW(KTPCO,NC)+JTPCRR)=RPHIS
        RW(KROW(KTPCO,NC)+JTPCRZ)=ZS
C
C++     Put the coordinate reference into the subcluster bank
C++     and vice-versa
C
        IW(KROW(KTSCL,IC)+JTSCCO)=NC-1
        IW(KROW(KTSCL,IC)+JTSCNC)=1
        IW(KROW(KTPCO,NC)+JTPCCN)=IC
      ENDIF
C
  999 CONTINUE
      RETURN
      END
#endif
