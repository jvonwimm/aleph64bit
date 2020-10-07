      SUBROUTINE TSAWIR
C----------------------------------------------------------------------
C!
C!   Created by Mermikides            26-SEP-1989
C!   Modified:  D. Casper 09-06-95    Support for pad dE/dx in TCOALG
C!              D. Casper 26-02-97    Fix grotesque bugs in field/alignment
C!                                    correction.
C!                        05-08-97    Replace call to THPFDG with new routine
C!                                    THPCOR, to correct 1 half-pad coordinates
C!                                    too.
C!
C!   Purpose   : To associate wires with pads for z-coord. refinement
C!   Inputs    : Banks TPCO,TBCO, JTPHCL,JTHPHI
C!   Outputs   : Workbank JTNWCO (see common /TSAWRK/)
C!
C!   Called by : TSAPAT
C!   Calls     : TPADPO,TWINDO,TSAWTK,UTLSLF
C!
C----------------------------------------------------------------------
#ifndef DOC
#include "tpcojj.h"
#include "tbcojj.h"
#include "tscljj.h"
#include "bcs.h"
#include "rlunit.h"
#include "alcons.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpgeow.h"
#include "tpccon.h"
#include "tsawrk.h"
#include "tflags.h"
C
      DIMENSION WIRE(50),BUCK(50),R0(50),BK(50),ITIND(50),WT(50)
      DIMENSION SIGT(50),RS(50),VAR(3)
      DIMENSION RPULS(LMXPCL),RTIME(LMXPCL),ERRM(3)
C
      LOGICAL LFRST
      SAVE NTPCO,NTBCO
C
      DATA LFRST/.TRUE./
      DATA NTNWC/50/
      DATA CHMAX/10./
C
#include "bmacro.h"
C
      IF (LFRST) THEN
         LFRST=.FALSE.
         NTPCO=NAMIND('TPCO')
         NTBCO=NAMIND('TBCO')
      ENDIF
      KTPCO = IW(NTPCO)
      KTBCO = IW(NTBCO)
C Create coordinate work bank
      CALL WBANK(IW,JTNWCO,LMHLEN+NTNWC*LNWCOA,*800)
      IW(JTNWCO+LMHCOL) = LNWCOA
      IW(JTNWCO+LMHROW) = 0
      IW(JTNWCO-3) = INTCHA('JTP4')
C
      IF (LROWS(JTHPHI).LE.1) GO TO 999
C
C  Loop over clusters        <=================================
      DO 30 I=1,LROWS(JTPHCL)
         NC = ITABL(JTPHCL,I,3)
C  Skip clusters with less than 3 entries
         IF (NC.LT.3) GO TO 30
         IOFF = ITABL(JTPHCL,I,4)
C  Loop over coordinates in each cluster  <====================
C  (These are assumed to be sorted in increasing row number)
         DO 60 JC = 1,NC
            IC = ITABL(JTHPHI,IOFF+JC,3)
            ISEC = ITABL(JTHPHI,IOFF+JC,5)
            ITYPE = ITPTYP(ISEC)
C  Good coordinates
            IF (IC.GT.0) THEN
               ZZ = RTABL(KTPCO,IC,JTPCZV)
               PHI = RTABL(KTPCO,IC,JTPCPH)
               S2RP = RTABL(KTPCO,IC,JTPCSR)
               RR = RTABL(KTPCO,IC,JTPCRV)
               DZ = SQRT(RTABL(KTPCO,IC,JTPCSZ))
               IRS = ITABL(KTPCO,IC,JTPCIN)
            ELSE
C  Bad coordinates
               ZZ = RTABL(KTBCO,-IC,JTBCZV)
               PHI = RTABL(KTBCO,-IC,JTBCPH)
C  For r-phi error store no of pads
               S2RP = ITABL(KTBCO,-IC,JTBCRW)
               RR = RTABL(KTBCO,-IC,JTBCRV)
C  For z error take length of pulse
               DZ = ITABL(KTBCO,-IC,JTBCZW)
               IRS = ITABL(KTBCO,-IC,JTBCIN)
            ENDIF
            IROW = IRS/100000
            IROWG = IROW
            ISTYP=ITPTYP(ISEC)
            IF (ISTYP.NE.1) THEN
              IROW=IROW-NTPDRW(1)
            ENDIF
            IPAD = MOD(IRS,1000)
            DZDR = ABS(ZZ/RR)
            CALL TPADPO(ISEC,IROW,IPAD,RPAD,PHIS,PWID)
C
C Get time and wire # window using geometry. (We could also get
C this information from TCLU/TSCL)
C Range of wires to use
            R1 = RPAD - 3.2
            R2 = RPAD + 3.2
            ZD1 = ZTPCMX - ABS(ZZ) + 3.2*DZDR
            ZD2 = ZTPCMX - ABS(ZZ) - 3.2*DZDR
C Time window
            IT1 = ZD1/TVDRFT(IENDTP(ISEC)) + TVOFFS(ISEC)
            IT2 = ZD2/TVDRFT(IENDTP(ISEC)) + TVOFFS(ISEC)
            R01 = R1*COS(PHIS)
            R02 = R2*COS(PHIS)
            RM = 0.5*(R01+R02)
            TINT = (IT1+IT2)/2.
            TGRD = -DZDR/(TVDRFT(IENDTP(ISEC))*COS(PHIS))
            TIS2 = 5.E-4
            TSG2 = DZ*DZ/TVDRFT(IENDTP(ISEC))**2
            ZGLB = ZZ
C Get wires in time and R0 window
            CALL TWINDO(ISEC,R01,R02,IT1,IT2,NWIR,WIRE,BUCK,50,SIGT)
C
C Look for tracks in wire hits
C NTRK = No of track candidates found
C ITIND = Track candidate pointer for eack hit
C
            CALL TSAWTK(NWIR,WIRE,BUCK,NTCAN,ITIND)
C
C  If we can't find any candidates among wire data, just store original
C  TPCO hit (Don't use TBCO in this case).
C
            IF (NTCAN.EQ.0) THEN
               IF (IC.LT.0) GO TO 60
               IF (LFRROW(JTNWCO).LT.1) THEN
                  NDAT = IW(JTNWCO) + 20*LCOLS(JTNWCO)
                  CALL WBANK(IW,JTNWCO,NDAT,*800)
               ENDIF
               IW(KNEXT(JTNWCO)+JTNWIC) = IC
               IW(KNEXT(JTNWCO)+JTNWNW) = NWIR
               RW(KNEXT(JTNWCO)+JTNWRM) = RM
               RW(KNEXT(JTNWCO)+JTNWTG) = TGRD
               RW(KNEXT(JTNWCO)+JTNWTI) = TINT
               RW(KNEXT(JTNWCO)+JTNWZZ) = ZZ
               RW(KNEXT(JTNWCO)+JTNWD2) = TIS2
               RW(KNEXT(JTNWCO)+JTNWG2) = TSG2
               RW(KNEXT(JTNWCO)+JTNWNN) = 999.
               RW(KNEXT(JTNWCO)+JTNWRR) = RR
               RW(KNEXT(JTNWCO)+JTNWPH) = PHI
               RW(KNEXT(JTNWCO)+JTNWS2) = S2RP
               IW(KNEXT(JTNWCO)+JTNWQW) = I*1000 + IT
               IW(KNEXT(JTNWCO)+JTNWZO) = 0
               IW(KNEXT(JTNWCO)+JTNWIR) = IROW
               RW(KNEXT(JTNWCO)+JTNWPS)=RTABL(KTPCO,IC,JTPCRR)
               RW(KNEXT(JTNWCO)+JTNWZS)=RTABL(KTPCO,IC,JTPCRZ)
               IW(JTNWCO+LMHROW) = IW(JTNWCO+LMHROW) + 1
               GO TO 60
            ENDIF
C
C  Loop over track candidates from wires  <===========
C
            NSTOR = 0
            DO 70 IT=1,NTCAN
               NW = 0
               DO 75 II = 1,NWIR
                  IF(ITIND(II).NE.IT) GO TO 75
                  NW = NW + 1
                  BK(NW) = BUCK(II)
                  R0(NW) = WIRE(II)
                  RS(NW) = WIRE(II) - RM
                  WT(NW) = 1./SIGT(II)**2
   75          CONTINUE
C
C  Obtain time at centre of wire group by fitting wire pulses
C
               CALL UTLSLF(RS,BK,WT,NW, A,B,CHISQ,VAR)
C
               IF (CHISQ/FLOAT(NW-2).LE.CHMAX) THEN
                  TGRD = A
                  TINT = B
                  TIS2 = VAR(1)
                  TSG2 = VAR(3)
C  Skip if we have a poor wire fit on a bad coord
               ELSE
                  IF(IC.LT.0) GO TO 70
               ENDIF
C
               ZDRFT=(B-TVOFFS(ISEC))*TVDRFT(IENDTP(ISEC))
C
C If ZFIT is outside the TPC, redefine R, Z and Z error at endplate
               IF (ZDRFT.LT.0) THEN
                  DRR = ZDRFT/ABS(DZDR)
                  ZDRFT = 0.
               ELSE
                  DRR = 0.
               ENDIF
C
C  If we are dealing with a bad coordinate try to recompute phi.
C  (Skip if unsucessful)
C
               IF (IC.LT.0) THEN
                  KTSPU = NLINK('TSPU',ISEC)
                  KTSCL = NLINK('TSCL',ISEC)
                  ISUB = ITABL(KTBCO,-IC,JTBCSC)
C  If pointer is negative, it means that we don't have good
C  sub-clusters.
                  IF (ISUB.LT.0) GO TO 60
C  Skip subcluster with saturated pulses
                  IQW=IW(KROW(KTSCL,ISUB)+JTSCQU)
                  IF (IQW.GE.100000) GO TO 60
                  IALGW=ITABL(KTSCL,ISUB,JTSCAW)
                  IQE=IALGW/1000
                  IF (IQE.EQ.0) GO TO 60
                  ITE = IALGW - 1000*IQE
C
C  Make lists of charge and time estimates for the subpulses.  Here
C
                  TANL = (ZTPCMX-ZDRFT)/RM
                  CALL TGETQT(IROW,KTSCL,ISUB,KTSPU,IQE,ITE,TANL,
     &                         RPULS,RTIME,NSPL,JP1,IFLG)
                  IF (IFLG.NE.0) GO TO 60
C
                  IPDMN = ITABL(KTSCL,ISUB,JTSCFP)
C  Apply the appropriate coordinate algorithm for r*phi (in pad units)
                  PRFS2=TPRF2(ZDRFT,ISEC,0,0.,0.)
                  CALL TCOALG(ISEC,IROW,IQW,NSPL,RPULS(JP1),PRFS2,
     &                        IPDMN,RPHIP,QCO,IFLG)
                  IF (IFLG.NE.0) GO TO 60
C
C  Apply a fudge in the case that half pads are used
C
                  CALL TNHLFP(ISEC,IROW,KROW(KTSCL,ISUB),NHALF)
                  IF (NHALF.GT.2) THEN
                    CALL THPCOR(NHALF,IPDMN,ISEC,IROW,RPHIP,RFUDG,IFLG)
                    IF (IFLG.NE.0) THEN
                      GO TO 60
                    ENDIF
                    RPHIP=RFUDG
                  ENDIF
C  Convert from pad units to sector coordinates, for phi
                  CALL TSCOOR(ISEC,IROW,RPHIP,B,RSEC,RPHIS,ZSEC)
C  Add the possible correction calculated above for RR, and use the
C  value for z calculated from the wires.
                  RSEC= RSEC + DRR
                  ZSEC= ZDRFT
C  Convert to global coordinates, with all alignment corrections
                  CALL TCTGLB(ISEC,RSEC,RPHIS,ZSEC,RGLB,RPHIG,ZGLB)
                  PHIG = RPHIG/RGLB
C  Correct z for time-of-flight
                  CALL TCRTOF(BFIELD,RGLB,ZGLB,0,' ',ZCR)
                  ZGLB=ZCR
C  Correct for drift field distortions
                  IF (FFCORR) THEN
                    MODE=0
                    IF (FFMODE) MODE=1
                    CALL TLACOR(RGLB,PHIG,ZGLB,RCR,PHICR,ZCR,MODE)
                    PHIG=PHICR
                    RGLB=RCR
                    ZGLB=ZCR
                  ENDIF
C  Correct for alignment of E and B fields (transverse drift)
                  IF (FECORR) THEN
                    CALL TCRTRN(ISEC,RGLB,PHIG,ZGLB,RCR,PHICR,ZCR)
                    RGLB=RCR
                    PHIG=PHICR
                    ZGLB=ZCR
                  ENDIF
C
C  Correct for z distortions measured by VDET
C
                  CALL TZCRVD(IENDTP(ISEC),RGLB,ZGLB,ZCR)
                  ZGLB=ZCR
                  CALL TZCSVD(ISEC,RGLB,PHIG,ZGLB,ZCR)
                  ZGLB=ZCR
C
C++   Correct for Residual field distortions
C
                  CALL TCORES(IENDTP(ISEC),IROWG,RGLB,PHIG,ZGLB,
     >                                           RCR,PHICR,ZCR)
                  RGLB =RCR
                  PHIG =PHICR
                  ZGLB =ZCR
                  CALL TFICOR(IENDTP(ISEC),IROWG,RGLB,PHIG,ZGLB,
     >                                           RCR,PHICR,ZCR)
                  RGLB =RCR
                  PHIG =PHICR
                  ZGLB =ZCR
C
C  Get the error estimate for the coordinate
                  MSPL=ITABL(KTSCL,ISUB,JTSCNS)
                  WPS=RTABL(KTSCL,ISUB,JTSCSP)
                  WZS=RTABL(KTSCL,ISUB,JTSCSB)
                  IQS=ITABL(KTSCL,ISUB,JTSCTC)
                  CALL TCOERR(ERRM,MSPL,WPS,WZS,IQS)
                  S2RP = ERRM(1)
               ELSE
C
C  For good coordinates, we just have to apply all transformations and
C  corrections to the new z value.  Get R and RPHI in sector system.
C
                  RSEC= TPDRBG(ISTYP)+(IROW-1)*TPDRST(ISTYP)
                  RPHIS= RTABL(KTPCO,IC,JTPCRR)
C  Now transform to global system, using the new z drift length
                  CALL TCTGLB(ISEC,RSEC,RPHIS,ZDRFT,RGLB,RPHIG,ZGLB)
C  Correct z for time-of-flight
                  CALL TCRTOF(BFIELD,RGLB,ZGLB,0,' ',ZCR)
                  ZGLB=ZCR
C  Correct for drift-field distortions.  
                  IF (FFCORR) THEN
                    PHIG=RPHIG/RGLB
                    MODE=0
                    IF (FFMODE) MODE=1
                    CALL TLACOR(RGLB,PHIG,ZGLB,RCR,PHICR,ZCR,MODE)
                    RGLB=RCR
                    PHIG=PHICR
                    ZGLB=ZCR
                  ENDIF
                  IF (FECORR) THEN
                    CALL TCRTRN(ISEC,RGLB,PHIG,ZGLB,
     &                                      RCR,PHICR,ZCR)
                    PHIG = PHICR
                    RGLB = RCR
                    ZGLB = ZCR
                  ENDIF
C
C  Correct for z distortions measured by VDET
C
                  CALL TZCRVD(IENDTP(ISEC),RGLB,ZGLB,ZCR)
                  ZGLB=ZCR
                  CALL TZCSVD(ISEC,RGLB,PHIG,ZGLB,ZCR)
                  ZGLB=ZCR
C
C++   Correct for Residual field distortions
C
                  CALL TCORES(IENDTP(ISEC),IROWG,RGLB,PHIG,ZGLB,
     >                                           RCR,PHICR,ZCR)
                  RGLB =RCR
                  PHIG =PHICR
                  ZGLB =ZCR
                  CALL TFICOR(IENDTP(ISEC),IROWG,RGLB,PHIG,ZGLB,
     >                                           RCR,PHICR,ZCR)
                  RGLB =RCR
                  PHIG =PHICR
                  ZGLB =ZCR
C
C  Add the radial correction determined from the wires.
C
                  RGLB = RGLB + DRR
               ENDIF
C  Here we store coordinate
               IF (LFRROW(JTNWCO).LT.1) THEN
                  NDAT = IW(JTNWCO) + 20*LCOLS(JTNWCO)
                  CALL WBANK(IW,JTNWCO,NDAT,*800)
               ENDIF
C
               IW(KNEXT(JTNWCO)+JTNWIC) = IC
               IW(KNEXT(JTNWCO)+JTNWNW) = NW
               RW(KNEXT(JTNWCO)+JTNWRM) = RM
               RW(KNEXT(JTNWCO)+JTNWTG) = A
               RW(KNEXT(JTNWCO)+JTNWTI) = B
               RW(KNEXT(JTNWCO)+JTNWZZ) = ZGLB
               RW(KNEXT(JTNWCO)+JTNWD2) = VAR(1)
               RW(KNEXT(JTNWCO)+JTNWG2) = VAR(3)
               RW(KNEXT(JTNWCO)+JTNWNN) = CHISQ/FLOAT(NW-2)
               RW(KNEXT(JTNWCO)+JTNWRR) = RGLB
               RW(KNEXT(JTNWCO)+JTNWPH) = PHIG
               RW(KNEXT(JTNWCO)+JTNWS2) = S2RP
               IW(KNEXT(JTNWCO)+JTNWQW) = I*1000 + IT
               IW(KNEXT(JTNWCO)+JTNWZO) = 0
               IW(KNEXT(JTNWCO)+JTNWIR) = IROW
               RW(KNEXT(JTNWCO)+JTNWPS)=RPHIS
               RW(KNEXT(JTNWCO)+JTNWZS)=ZDRFT
               IW(JTNWCO+LMHROW) = IW(JTNWCO+LMHROW) + 1
C Store a maximum of two good fits
               NSTOR = NSTOR + 1
               IF (NSTOR.EQ.2) GO TO 60
C
C End of loop over "wire track" candidates     ===============>
   70       CONTINUE
C End of loop over coordinates in cluster  ===================>
   60    CONTINUE
C End of loop over clusters ==================================>
   30 CONTINUE
C
C  Compress JTNWCO workbank
      IF (LROWS(JTNWCO).LE.0) GO TO 999
      CALL WBANK(IW,JTNWCO,LMHLEN+LCOLS(JTNWCO)*LROWS(JTNWCO),*800)
  999 RETURN
C
  800 CALL RERROR('TSAWIR',4,'Error extending work bank JTNWCO')
      END
#endif
