      SUBROUTINE TPADDX
C----------------------------------------------------------------------
C! Store subcluster pulseheights in TPDX bank for measurement of pad
C!  dE/dx
C!
C! There is a TPDX bank for each tpc track.  Each row is a single
C! subcluster measurement.
C!
C!    Author:    D. Casper  04-11-94
C!
C!    Modified:  D. Casper/ 17-04-97  add protection for logarithmic
C!               J-Y. Nief            correction to specific ionization
C!                                    calculation
C!    Modified:  F. Palla   15-11-98  consider a large pad crossing angle
C!    Modified:  M.Cattaneo 22-11-00  protection against ASIN argument > 1.
C!
C?
C!=====================================================================
#ifndef DOC
#include "alcons.h"
#include "rparac.h"
#include "tparac.h"
#include "tpcojj.h"
#include "tgtljj.h"
#include "tscljj.h"
#include "tgftjj.h"
#include "rlunit.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpgeow.h"
#include "bcs.h"
#include "twircl.h"
#include "tpadcl.h"
#include "tpdxjj.h"
C
      CHARACTER*35 TEXT
      COMMON/TSCLWB/ITSCLW
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE NTSCL,NTPCO,NTGFT,NTGTL,NTGCL
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTSCL=NAMIND('TSCL')
        NTPCO=NAMIND('TPCO')
        NTGFT=NAMIND('TGFT')
        NTGTL=NAMIND('TGTL')
        NTGCL=NAMIND('TGCL')
      ENDIF
C
C++ Clear workbank index
C
      ITSCLW = 0
C
C++ Check existence of necessary banks
C
      KTPCO = IW(NTPCO)
      KTGFT = IW(NTGFT)
      KTGTL = IW(NTGTL)
      KTGCL = IW(NTGCL)
      IF ( KTPCO.EQ.0 .OR. KTGFT.EQ.0 .OR. KTGTL.EQ.0
     &  .OR. KTGCL.EQ.0) GOTO 999
C
C++ Drop output bank if it exists
C
      IF (IW(NTPDX).NE.0) CALL BDROP(IW,'TPDX')
C
C++ Loop over tracks
C
      NTRK = LROWS(KTGFT)
      DO 20 ITRK = 1, NTRK
C
C++ Find TPC coordinates on this track
C
        NTPC   = ITABL(KTGTL,ITRK,JTGTN1)
        IOTGCL = ITABL(KTGTL,ITRK,JTGTIO)
C
C++ Create the output bank for this track
C
        LBANK = LMHLEN + NTPC * LTPDXA
        CALL AUBOS('TPDX',ITRK,LBANK,KTPDX,IGARB)
        IF (IGARB.NE.0 .OR. KTPDX.EQ.0) GOTO 998
        IW(KTPDX + LMHCOL) = LTPDXA
        IW(KTPDX + LMHROW) = 0
C
C++ Link to the subcluster banks
C
        KTSCL=IW(NTSCL)
        IF (KTSCL.EQ.0) GOTO 999
        CALL BWIND(IW,'TSCL',LTSECT,ITSCLW)
C
C++ Get necessary information about track
C
        OMEGA = RTABL(KTGFT,ITRK,JTGFIR)
        IF (OMEGA .EQ. 0.) GO TO 20
        TANL = RTABL(KTGFT,ITRK,JTGFTL)
        IF (TANL .EQ. 0.) GO TO 20
        COSL = ABS(COS(ATAN(TANL)))
        D0 = RTABL(KTGFT,ITRK,JTGFD0)
        IF (OMEGA*D0 .GT. 1.) GO TO 20
C
C++ Loop over the TPC coordinates
C
        NSCL = 0
        DO 10 ICO = 1, NTPC
          MCO = IW(KTGCL + LMHLEN + IOTGCL + ICO)
          ITPCO = KROW(KTPCO,MCO)
C
C++ Skip coordinates not on the first loop of a track
C
          ITK = IW(ITPCO+JTPCTN)
          IF (ITK.LE.0) GOTO 10
C
C++ Skip fake coordinates
C
          IF(MOD(IW(ITPCO+JTPCOF),10).EQ.5)GOTO 10
C
C++ Get the sector slot number
C
          IND = IW(ITPCO+JTPCIN)
          IROWG = IND/100000
          ISLOT = IND/1000 - IROWG*100
          IPAD = MOD(IND,1000)
          ISTYP = ITPTYP(ISLOT)
          IF (ISTYP.EQ.1)THEN
            IROW = IROWG
          ELSE
            IROW = IROWG - NTPDRW(1)
          ENDIF
C
C++ Get coordinates
C

          RGLOB = RW(ITPCO+JTPCRV)
          IF (RGLOB .LE. ABS(D0)) GO TO 10
          ZRAW= RW(ITPCO+JTPCRZ)
          RPRAW=RW(ITPCO+JTPCRR)
C
C++ Get the number of wires associated with this coordinate
C++ (filled in TWIREZ)
C
          NWIR = IW(ITPCO+JTPCIT)
C
C++ Get the subcluster number and link to the TSCL bank.
C
          IC = IW(ITPCO+JTPCCN)
          IF (ISLOT.GT.IW(ITSCLW)) THEN
            KTSCL=0
          ELSE
            KTSCL=IW(ITSCLW+ISLOT)
          ENDIF
          IF (KTSCL.EQ.0) THEN
            WRITE(TEXT,359) ISLOT
  359       FORMAT('TSCL bank missing for sector ',I2)
            CALL RERROR('TPADDX',3,TEXT(1:31))
            GOTO 10
          ENDIF
C
C++ If this is a part of a twin coordinate then skip it.
C
          NCOR = ITABL(KTSCL,IC,JTSCNC)
          IF (NCOR .GT. 1) GOTO 10
C
C++ Half pad coordinate ?  yes=>NHALF=1 no=>NHALF=0
C
          CALL TNHLFP(ISLOT,IROW,KROW(KTSCL,IC),NHALF)
          IF (NHALF .NE. 0) GO TO 10
C
C++ Get subcluster pulseheight, #pads, and quality flag
C
          IPULS = ITABL(KTSCL,IC,JTSCTC)
          IQUAL = ITABL(KTSCL,IC,JTSCQU)
          NPADS = ITABL(KTSCL,IC,JTSCNS)
C
C++  Cut out coordinates of abnormal size and suspicious subclusters
C
          IF (NPADS .LT. MNPDTP) GO TO 10
          IF (IQUAL .NE. 0) GO TO 10
C
C++ Correct the pulseheight according to calibration
C
          QPULS = IPULS * PNRMCL(ISLOT)
C
C++ Correct the pulseheight for sector edge effects
C++ First find the X and Y of the coordinate
C
          RPAD = TPPROW(IROWG)
          PHI_COORD = RPRAW/RPAD
          X_COORD = RPAD * COS(PHI_COORD)
          Y_COORD = RPAD * SIN(PHI_COORD)
C
C++ Find nearest wire to coord
C
          IWIRE = INT((X_COORD - TWIRE1(ISTYP))/TWSTEP(ISTYP) + 0.5)
          IF (IWIRE.LT.1) IWIRE = 1
          IF (IWIRE.GT.NTWIRE(ISTYP)) IWIRE = NTWIRE(ISTYP)
C
C++ Find distance from coord to sector edge
C
          DELY = TWIRLE(IWIRE,ISTYP) - ABS(Y_COORD)
C
C++ Apply exponential correction
C
          COREDG = 1.-EDGECL(1,ISTYP)*EXP(-DELY/EDGECL(2,ISTYP))
          QPULS = QPULS/COREDG
C
C++ Call routine to calculate track parameters at the padrow
C
          KCO = ITPCO
          KTR = KROW(KTGFT,ITRK)
          CALL TFILPA(KCO,KTR,PADCRO,WIRCRO)
          COSPCA = ABS(COS(PADCRO))
          IF (COSPCA.EQ.0) GOTO 10
C
C++ Calculate the sampled track length, including curvature effects
C
          ARG = ATAN2(TPDWID(ISTYP)*NPADS,TPDHGT(ISTYP))
          IF(ARG.GT.ABS(PADCRO)) THEN
            SMPL = TPDHGT(ISTYP)/COSL/COSPCA
          ELSE
            IF(ABS(SIN(PADCRO)).GT.0.) THEN
              ASARG = TPDWID(ISTYP)*NPADS/2./ABS(SIN(PADCRO))*OMEGA
              IF( ABS(ASARG) .GT. 1 ) GOTO 10
              SMPL = 2./OMEGA*ASIN(ASARG)/COSL
            ELSE 
              SMPL = 0.
            ENDIF
          ENDIF
          
          IF(SMPL.LE.1.E-6) GOTO 10
C
C++ Calculate difference in r-phi between coordinate and nearest pad
C
          RPNRM = 0.5*(NTPDPR(IROW,ISTYP)+1) - RPRAW/TPDSEP(ISTYP)
          DRPHI = TPDSEP(ISTYP) * (0.5-ABS(AMOD(RPNRM,1.)-0.5))
C
C
C++ Add data to TPDX bank for this track
C
          NSCL = NSCL + 1
          IW(KTPDX + LMHROW) = IW(KTPDX + LMHROW) + 1
          IOTPDX = KROW(KTPDX,NSCL)
          IW(IOTPDX + JTPDIN) = IND
          IW(IOTPDX + JTPDNP) = NPADS
          IW(IOTPDX + JTPDNW) = NWIR
          RW(IOTPDX + JTPDTC) = QPULS
          RW(IOTPDX + JTPDSL) = SMPL
          RW(IOTPDX + JTPDRZ) = ZRAW
          RW(IOTPDX + JTPDCA) = COSPCA
          RW(IOTPDX + JTPDDR) = DRPHI
 10     CONTINUE
   20 CONTINUE
      GOTO 1000
C
  998 CALL RERROR('TPADDX',1,'Not enough room in BOS for named bank')
      GOTO 1000
C
  999 CALL RERROR('TPADDX',2,
     &           'Input TPCO, TGTL or TSCL banks not found')
 1000 CONTINUE
C
C++ So that's it.  Add TPDX banks to the E list
C
      CALL BLIST(IW,'T+','TPDX')
      CALL AUBPRS('TPDX')
      IF (ITSCLW .NE. 0) CALL WDROP(IW,ITSCLW)
      RETURN
      END
#endif
