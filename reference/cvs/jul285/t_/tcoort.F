      SUBROUTINE TCOORT(IER)
C----------------------------------------------------------------------
C! Calculate corrected coordinates from initial track fit and initial
C! coordinate bank
C!
C!    Author:    D. Cinabro  6-12-89
C!    Modified:  R. Johnson 19-10-90
C!            D. Casper  25-04-95  Support for wire corrections to TPC
C!                                 coordinates.
C!            D. Casper  09-06-95  Support for Gaussian charge estimator
C!                                 for pad dE/dx
C!
C!    Output:   IER      /I     Return code=0 if all is OK
C!
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "alcons.h"
#include "rparac.h"
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
#include "rlunit.h"
#include "bcs.h"
#include "tpatel.h"
#include "tpccon.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tflags.h"
#include "tpccut.h"
#include "tgftjj.h"
C
      COMMON/TCRTWB/ ITSCLW,ITSPUW
C
      DIMENSION ZCUT(LTPDRO)
      DIMENSION RPULS(LMXPCL),RTIME(LMXPCL),ERRM(3)
      CHARACTER TEXT*35
      LOGICAL BAD,FIRST
      DATA FIRST/.TRUE./
      SAVE NTSCL,NTSPU,NTPCO,NTGFT,NTWRP,NTNGS
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        ITSCLW=0
        ITSPUW=0
        NTSCL=NAMIND('TSCL')
        NTSPU=NAMIND('TSPU')
        NTPCO=NAMIND('TPCO')
        NTGFT=NAMIND('TGFT')
        NTWRP=NAMIND('TWRP')
        NTNGS=NAMIND('TNGS')
      ENDIF
C
C Is there a TPCO bank with number greater than 0?
C If not, forget the whole thing.
C
      KTPCO = IW(NTPCO)
      IF (KTPCO.LE.0) GOTO 999
      NUM = IW(KTPCO-2)
      IF (NUM.LE.0) THEN
        KTPCO=IW(KTPCO-1)
        IF (KTPCO.EQ.0) GOTO 999
        NUM = IW(KTPCO-2)
      ENDIF
C
C Drop the output bank if it already exists
C
      IND=NLINK('TPCO',0)
      IF (IND.NE.0) IND=NDROP('TPCO',0)
C
C If there are no tracks, just rename the existing bank
C
      KTGFT=IW(NTGFT)
      IF (KTGFT.EQ.0) THEN
        IW(KTPCO-2)=0
        GOTO 997
      ENDIF
C
C Make a new one, number = 0
C
      LEN = IW(KTPCO)
      CALL AUBOS('TPCO',0,LEN,KNTPCO,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      IF (IGARB.EQ.1) KTPCO = NLINK('TPCO',NUM)
      IW(KNTPCO+LMHCOL) = LTPCOA
      IW(KNTPCO+LMHROW) = LROWS(KTPCO)
      KTGFT = IW(NTGFT)
C
C Link to the subcluster banks
C
      KTSCL=IW(NTSCL)
      IF (KTSCL.EQ.0) GOTO 999
      CALL BWIND(IW,'TSCL',LTSECT,ITSCLW)
C
C Link to the subpulse banks
C
      KTSPU=NAMIND('TSPU')
      IF (KTSPU.EQ.0) GOTO 999
      CALL BWIND(IW,'TSPU',LTSECT,ITSPUW)
C
C Loop over the existing coordinates
C
      DO 10 ICO = 1,LROWS(KTPCO)
C
C Skip coordinates not on the first loop of a track
C
        ITK = ITABL(KTPCO,ICO,JTPCTN)
        IF (ITK.LE.0) GOTO 100
C
C Skip fake coordinates or those already done by this routine
C
        IF (ITABL(KTPCO,ICO,JTPCOF).GE.4) GOTO 100
C
C Get the sector slot number
C
        IND = ITABL(KTPCO,ICO,JTPCIN)
        IROW = IND/100000
        ISLOT = IND/1000 - IROW*100
        ISTYP=ITPTYP(ISLOT)
        IF (ISTYP.EQ.1) THEN
          IROW=IROW
        ELSE
          IROW=IROW-NTPDRW(1)
        ENDIF
C
C Get the subcluster number and link to the TSCL and TSPU banks.
C
        IC = ITABL(KTPCO,ICO,JTPCCN)
        IF (ISLOT.GT.IW(ITSCLW)) THEN
          KTSCL=0
        ELSE
          KTSCL=IW(ITSCLW+ISLOT)
        ENDIF
        IF (KTSCL.EQ.0) THEN
          WRITE(TEXT,359) ISLOT
  359     FORMAT('TSCL bank missing for sector ',I2)
          CALL RERROR('TCOORT',3,TEXT(1:31))
          GOTO 100
        ENDIF
        IF (ISLOT.GT.IW(ITSPUW)) THEN
          KTSPU=0
        ELSE
          KTSPU=IW(ITSPUW+ISLOT)
        ENDIF
        IF (KTSPU.EQ.0) THEN
          WRITE(TEXT,360) ISLOT
  360     FORMAT('TSPU bank missing for sector ',I2)
          CALL RERROR('TCOORT',4,TEXT(1:31))
          GOTO 100
        ENDIF
C
C Find number of half pads
C
        CALL TNHLFP(ISLOT,IROW,KROW(KTSCL,IC),NHALF)
C
C Call routine to calculate track parameters at the padrow
C
        KCO = KROW(KTPCO,ICO)
        KTR = KROW(KTGFT,ITK)
        CALL TFILPA(KCO,KTR,PADCRO,WIRCRO)
C
C If this is a part of a twin coordinate or a coordinate from TSAPAT
C (small angle) then only recalculate the error estimates.
C
        NCOR = ITABL(KTSCL,IC,JTSCNC)
        IF (NCOR.GT.1) GOTO 101
        IF (ITABL(KTPCO,ICO,JTPCOF).EQ.2) GOTO 101
        IF (ITABL(KTPCO,ICO,JTPCOF).EQ.3) GOTO 101
C
C Only if the cluster is of 2 or 3 pads will the coordinate
C be recalculated.  Else simply recalculate the errors.
C
        NSUB=ITABL(KTSCL,IC,JTSCNS)
        IF (NSUB.NE.2.AND.NSUB.NE.3) GOTO 101
C
C Find which time and charge estimators to use.
C
        IALGW=ITABL(KTSCL,IC,JTSCAW)
        IQE=IALGW/1000
        IF (IQE.EQ.0) GOTO 100
        ITE=IALGW-1000*IQE
        IF (ITE.EQ.0) GOTO 100
C
C Make lists of charge and time estimates for the subpulses.  Here
C we also handle the problem of half pads by adding them together,
C if possible, to give corresponding full pads.
C
        CALL TGETQT(IROW,KTSCL,IC,KTSPU,IQE,RPULS,RTIME,NSPL,JP1,IFLG)
        IF (IFLG.NE.0) GO TO 100
C
        IPDMN=ITABL(KTSCL,IC,JTSCFP)
C
C Remove edge pulses which are too small.  In 3-pad clusters, remove
C the smallest pulse only if the two side ones are not balanced.
C
        CALL TRMVEP(KTSCL,IC,KTSPU,RPULS,JP1,NSPL)
        IPDMN=IPDMN+JP1-1
C
C Average the time estimates to give z for the cluster in buckets.
C
        CALL TSZCOR(ITE,RTIME(JP1),RPULS(JP1),NSPL,ZB)
C
C Apply the appropriate coordinate algorithm for r*phi (in pad units
C
        ZDRFT=(ZB-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
        PRFS2=TPRF2(ZDRFT,ISLOT,NSUB,PADCRO,WIRCRO)
        CALL TCOALG(ISLOT,IROW,IQW,NSPL,RPULS(JP1),PRFS2,IPDMN,RPHIP,
     &     QCO,IFLG)
        IF (IFLG.NE.0) THEN
          GO TO 100
        ENDIF
C
C Update charge in the TSCL bank (for pad dE/dx)
C
        IF(IW(NTNGS).EQ.0)IW(KROW(KTSCL,IC)+JTSCTC) = QCO
C
C Apply a fudge in the case that two half pads are used
C
        IF (NHALF.EQ.2) THEN
          CALL THPFDG(IPDMN,ISLOT,IROW,RPHIP,RFUDG,IFLG)
          IF (IFLG.NE.0) THEN
            GO TO 100
          ENDIF
          RPHIP=RFUDG
        ENDIF
        GO TO 200
  100   CONTINUE
C
C Here coordinates without tracks.  Copy them without change
C
        CALL UCOPY(IW(KROW(KTPCO,ICO)+JTPCIN),
     &             IW(KROW(KNTPCO,ICO)+JTPCIN),LTPCOA)
        GOTO 10
C
C Here coordinates with tracks for which we do not want to recalculate
C the position but just the error
C
  101   CONTINUE
        CALL UCOPY(IW(KROW(KTPCO,ICO)+JTPCIN),
     &             IW(KROW(KNTPCO,ICO)+JTPCIN),LTPCOA)
        TALA = RTABL(KTGFT,ITK,JTGFTL)
        Z = RTABL(KTPCO,ICO,JTPCZV)
        CALL TNHLFP(ISLOT,IROW,KROW(KTSCL,IC),NHALF)
        CALL TERPAR(PADCRO,Z,TALA,NHALF,0,SGRPHI,SIGZ)
        RW(KROW(KNTPCO,ICO)+JTPCSR)=SGRPHI**2
        RW(KROW(KNTPCO,ICO)+JTPCSZ)=SIGZ**2
        GOTO 10
  200   CONTINUE
C
        CALL TSCOOR(ISLOT,IROW,RPHIP,ZB,RS,RPHIS,ZS)
C
        IF (ISTYP.EQ.1) THEN
          IROWG=IROW
        ELSE
          IROWG=IROW+NTPDRW(1)
        ENDIF
C
C Transform the coordinate to the ALEPH frame of reference,
C including all necessary alignment corrections.
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
C For coordinates, make an estimate of the uncertainty.
C
        TALA = RTABL(KTGFT,ITK,JTGFTL)
        CALL TERPAR(PADCRO,Z,TALA,NHALF,0,SGRPHI,SIGZ)
C
C Correct coordinate z for time-of-flight
C
        CALL TCRTOF(BFIELD,R,Z,ITK,'TGFT',ZCR)
        Z=ZCR
C
C Correct coordinates for drift field distortions
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
        IF (FECORR) CALL TCRTRA(IENDTP(ISLOT),R,PHI,Z,RCR,PHICR)
        PHI=PHICR
        R=RCR
C
C++     Correct for z distortions measured by VDET
C
        CALL TZCRVD(IENDTP(ISLOT),R,Z,ZCR)
        Z=ZCR
        CALL TZCSVD(ISLOT,R,PHI,Z,ZCR)
        Z=ZCR
C
C++   Correct for Residual field distortions
C
        CALL TCORES(IENDTP(ISLOT),IROWG,R,PHI,Z,RCR,PHICR,ZCR)
        R   =RCR
        PHI =PHICR
        Z   =ZCR
        CALL TFICOR(IENDTP(ISLOT),IROWG,R,PHI,Z,RCR,PHICR,ZCR)
        R   =RCR
        PHI =PHICR
        Z   =ZCR
C
C Fill the results into the appropriate banks
C
        IW(KROW(KNTPCO,ICO)+JTPCIN)=ITABL(KTPCO,ICO,JTPCIN)
        RW(KROW(KNTPCO,ICO)+JTPCRV)=R
        RW(KROW(KNTPCO,ICO)+JTPCPH)=PHI
        RW(KROW(KNTPCO,ICO)+JTPCZV)=Z
        RW(KROW(KNTPCO,ICO)+JTPCSR)=SGRPHI**2
        RW(KROW(KNTPCO,ICO)+JTPCSZ)=SIGZ**2
        IW(KROW(KNTPCO,ICO)+JTPCOF)=4
        IW(KROW(KNTPCO,ICO)+JTPCTN)=ITABL(KTPCO,ICO,JTPCTN)
        IW(KROW(KNTPCO,ICO)+JTPCCN)=IC
        IW(KROW(KNTPCO,ICO)+JTPCIT)=ITABL(KTPCO,ICO,JTPCIT)
        RW(KROW(KNTPCO,ICO)+JTPCRR)=RPHIS
        RW(KROW(KNTPCO,ICO)+JTPCRZ)=ZS
C
   10 CONTINUE
C
C So that's it.  Drop the old TPCO bank
C
      KDUM = MDROP(IW,'TPCO',IW(KTPCO-2))
C
C  Now do corrections for wires
C
      IER = 0
      IF(IW(NTWRP).NE.0) THEN
        CALL TWIREZ(IER)
C
C  Reprocess TPCO
C
        IF(IER.EQ.0)THEN
          DO 20 ICO = 1, LROWS(KNTPCO)
            ICORRW = ITABL(KNTPCO,ICO,JTPCOF)/10
C
C  Only coordinates changed by the wires will be processed again
C
            IF (ICORRW .NE. 0)THEN
              ITK = ITABL(KNTPCO,ICO,JTPCTN)
              IND = ITABL(KNTPCO,ICO,JTPCIN)
              IROW = IND/100000
              IROWG = IROW
              ISLOT = IND/1000 - IROW*100
              ISTYP = ITPTYP(ISLOT)
              IF(ISTYP.NE.1) IROW = IROW - NTPDRW(1)
              IC = ITABL(KNTPCO,ICO,JTPCCN)
              IF(ISLOT.GT.IW(ITSCLW))THEN
                KTSCL = 0
              ELSE
                KTSCL = IW(ITSCLW+ISLOT)
              ENDIF
              IF (KTSCL.EQ.0)THEN
                WRITE(TEXT,359)ISLOT
                CALL RERROR('TCOORT',3,TEXT(1:31))
                GO TO 20
              ENDIF
C
C Get quantities needed to recalculate the error
C
              CALL TNHLFP(ISLOT,IROW,KROW(KTSCL,IC),NHALF)
              KCO = KROW(KNTPCO,ICO)
              KTR = KROW(KTGFT,ITK)
              CALL TFILPA(KCO,KTR,PADCRO,WIRCRO)
C
C For coordinates, make an estimate of the uncertainty.
C
              Z = RTABL(KNTPCO,ICO,JTPCZV)
              TALA = RTABL(KTGFT,ITK,JTGFTL)
              CALL TERPAR(PADCRO,Z,TALA,NHALF,ICORRW,SGRPHI,SIGZ)
C
C Get (newly adjusted) coordinate values in sector frame.
C
              RS = TPPROW(IROWG)
              RPHIS = RTABL(KNTPCO,ICO,JTPCRR)
              ZS = RTABL(KNTPCO,ICO,JTPCRZ)
C
C Transform the coordinate to the ALEPH frame of reference,
C including all necessary alignment corrections.
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
C Correct coordinate z for time-of-flight
C
              CALL TCRTOF(BFIELD,R,Z,ITK,'TGFT',ZCR)
              Z=ZCR
C
C Correct coordinates for drift field distortions
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
              IF (FECORR) CALL TCRTRA(IENDTP(ISLOT),R,PHI,Z,RCR,PHICR)
              PHI=PHICR
              R=RCR
C
C++     Correct for z distortions measured by VDET
C
              CALL TZCRVD(IENDTP(ISLOT),R,Z,ZCR)
              Z=ZCR
              CALL TZCSVD(ISLOT,R,PHI,Z,ZCR)
              Z=ZCR
C
C++   Correct for Residual field distortions
C
              CALL TCORES(IENDTP(ISLOT),IROWG,R,PHI,Z,RCR,PHICR,ZCR)
              R   =RCR
              PHI =PHICR
              Z   =ZCR
              CALL TFICOR(IENDTP(ISLOT),IROWG,R,PHI,Z,RCR,PHICR,ZCR)
              R   =RCR
              PHI =PHICR
              Z   =ZCR
C
C Fill the results into the appropriate banks
C
              RW(KROW(KNTPCO,ICO)+JTPCRV)=R
              RW(KROW(KNTPCO,ICO)+JTPCPH)=PHI
              RW(KROW(KNTPCO,ICO)+JTPCZV)=Z
              RW(KROW(KNTPCO,ICO)+JTPCSR)=SGRPHI**2
              RW(KROW(KNTPCO,ICO)+JTPCSZ)=SIGZ**2
            ENDIF
   20     CONTINUE
        ENDIF
      ENDIF
C
  997 CONTINUE
      IER=0
      GOTO 1000
C
  998 CALL RERROR('TCOORT',1,'Not enough room in BOS for named bank')
      IER=1
C
  999 CALL RERROR('TCOORT',2,
     &           'Input TPCO, TSPU or TSCL banks not found')
      IER=2
 1000 CONTINUE
      CALL WDROP(IW,ITSCLW)
      CALL WDROP(IW,ITSPUW)
      RETURN
      END
#endif
