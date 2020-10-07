      SUBROUTINE TWIREZ(IER)
C---------------------------------------------------------------------
C! Process Landau corrections and wire Z coordinates
C!   Author D.Casper
C!   Modified from TWIRES by I.Tomalin
C!   Modified       D. Casper   20-05-97  Protect vs. negative VARFIT(3)
C!                  M.Cattaneo  26-07-97  Protect vs. ABS(B1) .GT. RPAD
C!                  D.Casper    05-08-97  Do not apply Landau to coords
C!                                        with half-pads, since the PH
C!                                        response is falling off at the
C!                                        sector edge.
C!
C! Input:  TGFT, TGTL, TGCL, TPCO, TWIT, TWAT, TWTB and TWRR banks.
C! Output: TWZZ bank.
C!         Modifies raw r-phi and z values in TPCO
C!         Integer IER: = 0 if no error occured.
C---------------------------------------------------------------------
#ifndef DOC
C Max. allowed x distance of wire from pad coordinate if it is to
C be used.
      PARAMETER(MAXSEP=3.2)
C Minimum number of wire pulses on track to bother using them.
      PARAMETER(MINWIR=10)
C Reject wires for which ABS(X(wire)-XCENT(track))/R(track) > REJECT.
      PARAMETER(REJECT=0.8)
C Min. acceptable confidence level in wire fit.
      PARAMETER(CLMIN=0.01)
C Max. no. of wires to reject for each pad coordinate, in order to
C obtain an acceptable confidence level.
      PARAMETER(MAXREJ=1)
C Parameterization of wire z errors.
      DIMENSION EZWPAR(0:5)
      DIMENSION IBTOR(0:7)
      DATA IBTOR/24,28,16,20,8,12,0,4/
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C HAC parameters for wire coords and Landau correction params
#include "twcojj.h"
C HAC parameters for TPC tracks.
#include "tgftjj.h"
#include "tgtljj.h"
#include "tgcljj.h"
C HAC parameters for TPC coordinates.
#include "tpcojj.h"
#include "tscljj.h"
#include "tspujj.h"
#include "twzajj.h"
C HAC parameters for TPC wire information.
#include "twitjj.h"
#include "twatjj.h"
#include "twtbjj.h"
#include "twirct.h"
#include "twhtjj.h"
C HAC parameters for TWZZ bank
#include "twzzjj.h"
C Parameters for TPC geometry.
#include "tpgpar.h"
C Common for TPC padrow radii etc.
#include "tpgeom.h"
C COMMON for TPC pads
#include "tpgeop.h"
C Common for TPC wires.
#include "tpgeow.h"
C Commons for TPC drift velocity and time offsets.
#include "tzcorr.h"
#include "tpccon.h"
#include "rconds.h"
C Common for Landau correction params
#include "tshpco.h"
C Dummy variables to hold Kalman filter internals (most not used)
      PARAMETER (MPT = 40)
      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)
      DIMENSION TRACK(6), COV(21)
C Arrays to store coord. list, sector number, and raw pad coordinates
C in sector frame.
      DIMENSION TRACK_TRIM(6), COV_TRIM(21),TRACK_POS(2)
C Arrays needed by straight line fitter UTLSLF.
      PARAMETER(MAXWIR=352)
      DIMENSION SFIT(MAXWIR),ZFIT(MAXWIR),WFIT(MAXWIR),QFIT(MAXWIR)
      DIMENSION VARFIT(3)
#include "alcons.h"
#include "bcs.h"
      INTEGER IRET
      INTEGER AGETDB
      INTEGER UF2TPO,UFTTRA,UFDALI
      EXTERNAL AGETDB, UF2TPO, UFTTRA, UFDALI
      SAVE FLAT,SIG,ICORU,ICORZ,EZWPAR
      DATA LRUN/0/
c      DIMENSION A(5),B(5),C(4)
#include "bmacro.h"
      ERZWIR(ZE,TANE) = (EZWPAR(0) + EZWPAR(1)*ABS(ZE)/ZTPCMX)**2
     &          * (EZWPAR(2)+EZWPAR(3)*ABS(TANE)+EZWPAR(4)*TANE**2)
C-----------------------------------------------------------------------
      IER = 0
C
C Find constants for parameterization of errors and corrections
C to wire coordinates.
      IF (FIRST) THEN
        FIRST = .FALSE.
        NTWCO = NAMIND('TWCO')
        NTSIR = NAMIND('TSIR')
      END IF
      CALL ABRUEV(IRUN,IEVT)
      KTWCO = IW(NTWCO)
      IF (KTWCO .EQ. 0 .OR. IRUN.NE.LRUN) THEN
        ISTAT = AGETDB('TWCO',IRUN)
        IF (ISTAT .EQ. 0)THEN
            IER = 1
            CALL RERROR('TWIREZ',IER,'TWCO bank not found; returning.')
            GO TO 999
        ENDIF
        KTWCO = IW(NTWCO)
        LRUN = IRUN
        FLAT = RTABL(KTWCO,1,JTWCFL)
        SIG  = RTABL(KTWCO,1,JTWCSG)
        ICORU = ITABL(KTWCO,1,JTWCUF)
        ICORZ = ITABL(KTWCO,1,JTWCZF)
        EZWPAR(0) = RTABL(KTWCO,1,JTWCE0)
        EZWPAR(1) = RTABL(KTWCO,1,JTWCE1)
        EZWPAR(2) = RTABL(KTWCO,1,JTWCC0)
        EZWPAR(3) = RTABL(KTWCO,1,JTWCC1)
        EZWPAR(4) = RTABL(KTWCO,1,JTWCC2)
      ENDIF
C
C Create TWZZ bank.
      KTPCO = NLINK('TPCO',0)
      IF (KTPCO.EQ.0) THEN
        IER = 2
        CALL RERROR('TWIREZ',IER,'TPCO bank not found.')
        GOTO 999
      END IF
      LTWZZ = LMHLEN + LTWZZA*LROWS(KTPCO)
      CALL AUBOS('TWZZ',0,LTWZZ,KTWZZ,IGARB)
      IF (IGARB.NE.0) THEN
        IER = 3
        CALL RERROR('TWIREZ',IER,'Insufficient BOS space.')
        GOTO 999
      END IF
      IW(KTWZZ + LMHCOL) = LTWZZA
      IW(KTWZZ + LMHROW) = 0
C
      KTGFT = NLINK('TGFT',1)
      KTGTL = NLINK('TGTL',1)
      KTGCL = NLINK('TGCL',1)
      KTPCO = NLINK('TPCO',0)
      CALL BWIND(IW,'TSCL',36,IDTSCL)
      CALL BWIND(IW,'TSPU',36,IDTSPU)
C
C Check that the TPC track and coordinate banks exist.
      IF (KTGFT.EQ.0.OR.KTGTL.EQ.0.OR.KTGCL.EQ.0.OR.KTPCO.EQ.0) THEN
        IER = 4
        CALL RERROR('TWIREZ',IER,'Missing track reconstruction banks.')
        GOTO 999
      END IF
C
      KTWIT = NLINK('TWIT',0)
      KTWAT = NLINK('TWAT',0)
      KTWTB = NLINK('TWTB',0)
      
      CALL BWIND(IW,'TWRR',LTSECT,IDTWRR)
      CALL BWIND(IW,'TWRF',LTSECT,IDTWRF)
C
C+++  Up to 1991 the banks (TSIR,TSDI,TSLE) were called (TRIR,TRDI,TRLE) 
C
      If (iw(ntsir).ne.0) Then
         CALL BWIND(IW,'TSIR',LTSECT,IDTSIR)
         CALL BWIND(IW,'TSLE',LTSECT,IDTSLE)
      Else
         CALL BWIND(IW,'TRIR',LTSECT,IDTSIR)
         CALL BWIND(IW,'TRLE',LTSECT,IDTSLE)
      Endif   
C
C Check the BOS bank structure for the TPC wire information.
      IF (KTWIT.GT.0) THEN
        IF (KTWAT.EQ.0.OR.KTWTB.EQ.0) THEN
          IER = 5
          CALL RERROR('TWIREZ',IER,'TWIT exists but TWAT/TWTB do not.')
          GOTO 999
        END IF
        IF (LROWS(KTWIT).NE.LROWS(KTGFT)) THEN
          IER = 6
          CALL RERROR('TWIREZ',IER,'TWIT bank has wrong length.')
          GOTO 999
        END IF
      ELSE
C No wire information for this event.
        IER = 7
        CALL RERROR('TWIREZ',IER,'No wire information available.')
        GOTO 999
      END IF
C
C--- Loop over tracks coming from primary vertex.
C
      DO 100 ITRK = 1,LROWS(KTGFT)
C Note number of good TPC wires on track and offset in TWAT bank.
        IF (KTWIT.GT.0) THEN
          ITWIT  = KROW(KTWIT,ITRK)
          NG     = IW(ITWIT + JTWING)
          IF (NG.GT.MAXWIR) THEN
            IER = 8
            CALL RERROR('TWIREZ',IER,'Too many wires on track.')
            GOTO 999
          END IF
          IF (NG.EQ.0) GOTO 100
          IOTWAT = IW(ITWIT + JTWIOS)
          LTWHT = LMHLEN + NG * LTWHTA
          CALL AUBOS('TWHT',ITRK,LTWHT,KTWHT,IGARB)
          CALL BLIST(IW,'E+','TWHT')
          IF (IGARB.NE.0)THEN
            IER = 3
            CALL RERROR('TWIREZ',IER,'Insufficient BOS space.')
            GOTO 999
          END IF
          IW(KTWHT+LMHROW) = 0
          IW(KTWHT+LMHCOL) = LTWHTA
C Loop over the good TPC wires pulses on this track.
          DO 35 IWP = 1,NG
C Get pointer to TWTB bank for this pulse.
            IPTWTB = IW(KTWAT + LMHLEN + IOTWAT + IWP)
            ITWTB = KROW(KTWTB,IPTWTB)
C
C First find charge of this pulse. If it is zero, this was a below
C threshhold hit. It is probably unreliable and in any case the TWTB
C pointer points not to the TWRR bank but simply gives the wire number.
            IF (RW(ITWTB + JTWTCE).LT.1.0E-3) GOTO 35
C
C Get sector number of wire and pointer to TWRR bank.
            IDWIRE        = IW(ITWTB + JTWTWI)
            ISECT         = IDWIRE/65536
C
C Find coordinates in sector frame. (N.B. Wire is perpendicular to x ax is).
            IF (IW(IDTWRR).LT.ISECT .OR. IW(IDTWRR+ISECT).EQ.0)THEN
              IER = 9
              CALL RERROR('TWIREZ',IER,'Missing TWRR bank.')
              GOTO 999
            END IF
C
            KTWRR = IW(IDTWRR+ISECT)
            IF (IW(IDTWRF).LT.ISECT .OR. IW(IDTWRF+ISECT).EQ.0)THEN
                 IER = 10
                 CALL RERROR('TWIREZ',IER,'Missing TWRF bank')
                 GOTO 999
            ENDIF
            KTWRF = IW(IDTWRF+ISECT)
            IF (IW(IDTSIR).LT.ISECT .OR. IW(IDTSIR+ISECT).EQ.0)THEN
                 IER = 11
                 CALL RERROR('TWIREZ',IER,'Missing TSIR bank')
                 GOTO 999
            ENDIF
            KTSIR = IW(IDTSIR+ISECT)
            IPTWRR = MOD(IDWIRE,65536)
            IHIT   = IW(KTWRR + LMHLEN + IPTWRR)
            IF (IBITS(IHIT,13,1).EQ.1) THEN
              IREF   = IW(KTWRF + LMHLEN + IPTWRR)
              IOTSIR = IBITS(IREF,0,16)
              IPUL   = IW(KTSIR + IOTSIR)
              IREDU  = IBITS(IPUL,13,1)
              IF (IREDU.NE.0) THEN
C Note number of pulses above threshhold.
                ITWHT = KNEXT(KTWHT)
                IW(KTWHT+LMHROW) = IW(KTWHT+LMHROW)+1
C  Get wire, sector, etc                
                IF (IW(IDTSLE).LT.ISECT .OR.
     &                      IW(IDTSLE+ISECT).EQ.0)THEN
                  IER = 12
                  CALL RERROR('TWIREZ',IER,'Missing TSLE bank')
                  GOTO 999
                ENDIF
                KTSLE = IW(IDTSLE+ISECT)
                ILEN4 = IW(KTSLE+(IOTSIR-1)/8+1)
                IBIT0 = IBTOR(MOD(IOTSIR,8))
                ILEN = IBITS(ILEN4,IBIT0,4)
                IWIRE  = IBITS(IHIT,24,8)
                ISTYP  = ITPTYP(ISECT)

C Resort to stupidity, since the t0 offset stored in TZOFFS(9) is
C too coarse-grained due to packing (any change of less than 1/16th of
C a bin = ~5.6 ns = 290um is rounded to zero when TWRR is filled).
C So we remove the rounded-off correction, and then re-apply
C it in a more accurate way.

                TUNCORR = FLOAT(IBITS(IPUL,0,13))/TPACKW
                TRECORR = TUNCORR + TZOFFS(9) - TVOFFS(ISECT)
                ZRECORR = TRECORR * TVDRFT(IENDTP(ISECT))
                
C Load TWHT with information presently available
                IW(ITWHT+JTWHSN) = ISECT
                RW(ITWHT+JTWHXW) = TWIRE1(ISTYP) +
     +               FLOAT(IWIRE - 1)*TWSTEP(ISTYP)
C                RW(ITWHT+JTWHZW) = ((IBITS(IHIT,0,13))/TPACKW
C     +               - TVOFFS(ISECT))*TVDRFT(IENDTP(ISECT))
                RW(ITWHT+JTWHZW) = ZRECORR
                IW(ITWHT+JTWHLW) = MAX(1,ILEN)
                IW(ITWHT+JTWHQW) = IBITS(IHIT,14,10)
              ENDIF
            ENDIF
   35     CONTINUE
C Update length of TWHT
          LTWHT = LMHLEN + IW(KTWHT+LMHCOL)*IW(KTWHT+LMHROW)
          IW(1) = 1
          CALL AUBOS('TWHT',ITRK,LTWHT,KTWHT,IGARB)
        END IF
C
C--- Now associate wires to pad coordinates.
C
        IF (LROWS(KTWHT).GE.MINWIR) THEN
          NTPC   = ITABL(KTGTL,ITRK,JTGTN1)
          IOTGCL = ITABL(KTGTL,ITRK,JTGTIO)
          IF(NTPC.LT.4 .OR. NTPC.GT.21) GOTO 100

C  Fit the track with the Kalman filter
          IER = UFTTRA(ITRK,FIELRC,RW(KROW(KTGFT,ITRK)+JTGFIR),
     +                RTABL(KTGFT,ITRK,JTGFCD),NTPC,0,0,
     +                IW(KTGCL+LMHLEN+IOTGCL+1),IDUM,IDUM,
     +                TRACK,COV,CHI2,NDEG)
          IF (IER .NE. 0) THEN
            IER = IER + 100
            CALL RERROR('UFTKAL',IER,'Kalman Filter error')
            GOTO 100
          ENDIF
C Loop over TPC pad coordinates.
          DO 95 M = 1,NTPC
            MCO = IW(KTGCL + LMHLEN + IOTGCL + M)
            ITPCO = KROW(KTPCO,MCO)
C Ignore and flag coordinates produced by TWINCO or TSACOR.
            IF (IW(ITPCO + JTPCOF).NE.1 .AND.
     &          IW(ITPCO + JTPCOF).NE.4) THEN
              IW(ITPCO + JTPCIT) = -1
              GOTO 95
            ENDIF
C Clear flag for wire data overlapping this coordinate.
            IW(ITPCO + JTPCIT) = 0
C Find this coordinate in the list used in the fit.
            IRET = UFDALI(NLOW,NHIGH,XSTS,XME,RF,IUSED,VMEAS)
            DO IFC = NLOW, NHIGH
              IF(ABS(RF(IFC) - RW(ITPCO + JTPCRV)).LE. 1.
     &              .AND. IUSED(IFC).GT.0) GOTO 49
            ENDDO
C If coord not used in fit, forget it
            GOTO 95
   49       CONTINUE
C Note helix parameters of track.
            ISTAT = UF2TPO(IFC, TRACK_TRIM, COV_TRIM,TRACK_POS)
            IF (ISTAT.NE.0) GO TO 95
            RADINV = TRACK_TRIM(1)
            TANL   = TRACK_TRIM(2)
            PHI0   = TRACK_TRIM(3)
            D0     = TRACK_TRIM(4)
C See what sector and row we're in
            INDCO     = IW(ITPCO + JTPCIN)
            IROWP     = INDCO/100000
            ISECTP    = (INDCO - 100000*IROWP)/1000
            ISTYP     = ITPTYP(ISECTP)
            IF (ISTYP.EQ.1) THEN
                IROWS = IROW
            ELSE
                IROWS = IROW - NTPDRW(1)
            ENDIF
C Find subcluster that produced this guy
            ISCL      = IW(ITPCO+JTPCCN)
            KTSCL     = IW(IDTSCL+ISECTP)
C Find the number of half pads; coordinates with half-pads will not receive
C Landau correction, but wire z coordinates are still OK (not pulse-height
C dependent)
            JTSCL     = KROW(KTSCL,ISCL)
            CALL TNHLFP(ISECTP,IROWS,JTSCL,NHALF)
C Get time algorithm flag
            ITALG     = MOD(ITABL(KTSCL,ISCL,JTSCAW),1000)
C Find the subpulses which comprise the coordinate
            KTSPU     = IW(IDTSPU+ISECTP)
            IF (KTSPU.NE.0 .AND. KTSCL.NE.0) THEN
                ISP = ITABL(KTSCL,ISCL,JTSCPO)
                NSP = ITABL(KTSCL,ISCL,JTSCNS)
            ELSE
                NSP = 0
            ENDIF
            RPAD      = TPPROW(IROWP)
            RPHIPAD   = RW(ITPCO + JTPCRR)
            PHIPAD    = RPHIPAD/RPAD
            XPAD      = RPAD*COS(PHIPAD)
            YPAD      = RPAD*SIN(PHIPAD)
            ZPAD      = RW(ITPCO + JTPCRZ)
            EZPAD2    = RW(ITPCO + JTPCSZ)
C Things get a bit complicated here.  We have the track fit in the global
C frame, including field corrections.  But we only have the wire measurements
C in the sector frame, without field corrections.  We will want to predict
C the track parameters in the same coordinate system as the measurements.
C To do this, we will express the track as a circle in the r-phi plane, and
C a line in the s-z plane.  So we need to know the center of the track circle
C in the sector frame, without field corrections, and the point in z (again
C in the sector frame) where the track crosses the pad row.  To do this, we
C have to determine the field correction and subtract it from the global
C values simply (geometrically) transformed into the sector frame.
C
C Find the field correction which applies at the coordinate:
C Translate coordinate to global frame, but don't do field correction.
            CALL TCTGLB(ISECTP,RPAD,RPHIPAD,ZPAD,RG,RPHIG,ZG)
C Compare x,y before corrections with value stored in TPCO (including field)
            PHIG = RPHIG/RG
            XG = RG * COS(PHIG)
            YG = RG * SIN(PHIG)
            XC = RW(ITPCO + JTPCRV) * COS(RW(ITPCO+JTPCPH))
            YC = RW(ITPCO + JTPCRV) * SIN(RW(ITPCO+JTPCPH))
            CORX = XC - XG
            CORY = YC - YG
            CORZ = RW(ITPCO + JTPCZV) - ZG
C Find the center of the track circle in the r-phi plane, in the global frame.
            RCIRC = ABS(1.0/RADINV - D0)
            PCIRC = PHI0 + SIGN(PIBY2,1.0/RADINV - D0)
            PCIRC = MOD(PCIRC + TWOPI,TWOPI)
            XCIRC = RCIRC * COS(PCIRC)
            YCIRC = RCIRC * SIN(PCIRC)
C Adjust track center and z crossing value in global frame for field
            XCIRCC = XCIRC - CORX
            YCIRCC = YCIRC - CORY
            ZCIRCC = TRACK_POS(2) - CORZ
            RCIRCC = SQRT(XCIRCC**2 + YCIRCC**2)
            PCIRCC = ATAN2(YCIRCC, XCIRCC)
C Finally, find track center and z crossing in sector frame, before corrections
C This is what is comparable to the raw measurements
            CALL TCTSEC(ISECTP,RCIRCC,PCIRCC,ZCIRCC,
     &          RCIRCS,UCIRCS,ZCIRCS)
            PCIRCS = UCIRCS/RCIRCS
            XCIRCS = RCIRCS*COS(PCIRCS)
            YCIRCS = RCIRCS*SIN(PCIRCS)
C Find place where helix crosses the padrow
            IF(IENDTP(ISECTP).EQ.1)THEN
              OMEGA = -RADINV
            ELSE
              OMEGA = RADINV
            ENDIF
            DD0 = SQRT(XCIRCS**2+YCIRCS**2)
            RMAX = MAX(ABS(1./OMEGA),RPAD)
            RMIN = MIN(ABS(1./OMEGA),RPAD)
            IF (DD0.LE.RMAX-RMIN .OR. DD0.GE.RMAX+RMIN) GOTO 95
            B1 = (RPAD**2+DD0**2-1./OMEGA**2)/(2.*DD0)
            IF (ABS(B1).GT.RPAD) GOTO 95
            AA = SQRT(RPAD**2-B1**2)
            X1 = B1*XCIRCS/DD0
            X2 = AA*YCIRCS/DD0
            Y1 = B1*YCIRCS/DD0
            Y2 = AA*XCIRCS/DD0
            XL1 = X1 - X2
            YL1 = Y1 + Y2
            XL2 = X1 + X2
            YL2 = Y1 - Y2
            DL1 = (XL1-XPAD)**2 + (YL1-YPAD)**2
            DL2 = (XL2-XPAD)**2 + (YL2-YPAD)**2
            IF (DL1.LT.DL2)THEN
                XCROSS = XL1
                YCROSS = YL1
            ELSE
                XCROSS = XL2
                YCROSS = YL2
            ENDIF
            CPSPAD = (XCROSS-XCIRCS)*OMEGA
C Skip coordinate if track runs nearly parallel to wire
            IF (ABS(CPSPAD).LE.REJECT) THEN
              SPSPAD = (YCROSS-YCIRCS)*OMEGA
              PSIPAD = ATAN2(SPSPAD,CPSPAD)
              PHIPAD = ATAN2(YCROSS,XCROSS)
              COSPCA = -SIN(PSIPAD - PHIPAD)
              SINPCA = COS(PSIPAD - PHIPAD)
              TANPCA = SINPCA/COSPCA
C Note range in x (raw sector coordinate) that each pad row can
C claim as its own.
              XLOW = XCROSS - MAXSEP
              XHIGH = XCROSS + MAXSEP
C Calculate extent of the flat part of pad response
              RFLAT = FLAT * TPDHGT(ITPTYP(ISECTP))/2.
C Note range of r (raw sector coordinate) that each pad coordinate can
C claim as its own. (Pad height)
              DELRAD = RFLAT + 3. * SIG * TPDHGT(ITPTYP(ISECTP)) / 2.
C Clear sums for coordinate corrections
              RPCORR = 0.
              ZCORR = 0.
              SUMR = 0.
              SUMWR = 0.
              SUMQR = 0.
              SUMZ = 0.
              SUMWZ = 0.
              SUMQZ = 0.
              AVE = 0.
              NAVE = 0
C Find the wires lying in this x range.
              NFIT = 0
              DO 75 IWP = 1,LROWS(KTWHT)
                ITWHT = KROW(KTWHT,IWP)
                ISECTW = IW(ITWHT+JTWHSN)
                IF (ISECTW.EQ.ISECTP) THEN
                  XWIRE = RW(ITWHT+JTWHXW)
                  IF (XWIRE.GT.XLOW.AND.XWIRE.LT.XHIGH)THEN
C Find intersection of track with wire
                    CPSWIR = (XWIRE - XCIRCS)*OMEGA
C Reject wire if track is moving almost parallel to it.
                    IF (ABS(CPSWIR).LE.REJECT)THEN
                      SPSWIR = SIGN(SQRT(1.-CPSWIR**2),SPSPAD)
                      PSIWIR = SIGN(ACOS(CPSWIR),PSIPAD)
C Find path length along track relative to x=0.
                      SWIRE = (PSIWIR - PSIPAD)/OMEGA
                      NFIT = NFIT+1
                      SFIT(NFIT) = SWIRE
                      ZFIT(NFIT) = RW(ITWHT+JTWHZW) - ZPAD
                      WFIT(NFIT) = 1.0/ERZWIR(ZPAD,TANL)
                      QFIT(NFIT) = IW(ITWHT+JTWHQW)
C Calculations for r-phi and z correction (approximate track as a straight line
                      IF(ABS(SWIRE * COSPCA) .LE. DELRAD)THEN
C Count number of wires overlapping this coordinate for pad dE/dx
                        IW( ITPCO + JTPCIT ) = IW( ITPCO + JTPCIT) + 1
                        NAVE = NAVE + 1
                        AVE = AVE + QFIT(NFIT)
                        DR = SWIRE * ABS(COSPCA)
                        IF ( ABS(DR) .LE. RFLAT )THEN
                          WT = 1.
                        ELSE
                          WT = EXP ( -(ABS(DR) - RFLAT)**2 / (2. *
     &                      SIG**2) )
                        ENDIF
                        SUMZ = SUMZ + QFIT(NFIT) * DR * WT
                        SUMWZ = SUMWZ + DR * WT
                        SUMQZ = SUMQZ + QFIT(NFIT) * WT
                        SUMR = SUMR + QFIT(NFIT) * DR * WT
                        SUMWR = SUMWR + DR * WT
                        SUMQR = SUMQR + QFIT(NFIT) * WT
                      ENDIF
                    ENDIF
                  ENDIF
                END IF
   75         CONTINUE
C
C  Calculate coordinate corrections
C  See if enough wires are available
C
              IF (NAVE .GE. 4) THEN
                AVE = AVE / NAVE
C
C  Compute coefficient for Landau correction of TPC coords
C  (Put code of TFUNL inline - FLR)
C
                IF (ABS(TANL).LT.TSHPLC(1)) THEN
                   COEFFL = TSHPLC(2) * ABS(TANL)
                ELSE IF (ABS(TANL).LT.TSHPLC(3))THEN
                   COEFFL = TSHPLC(2) * ABS(TSHPLC(1))
                ELSE
                   Y = ABS(TANL) - ABS(TSHPLC(3))
                   COEFFL = TSHPLC(2) * ABS(TSHPLC(1)) + TSHPLC(4)*Y
                ENDIF
C
C  Process Z correction
C
                IF(SUMQZ.GT.0.)THEN
                  RCORR = NAVE * (SUMZ - AVE * SUMWZ)/SUMQZ/(NAVE-1)
                  ZCORR = COEFFL
     &                  * SIGN(MIN(ABS(RCORR),0.8),RCORR)
C
C  Subtract the correction factor to get a new Z in sector coordinates
C  Do not correct half-pad coordinates
C
                  IF(ICORZ.NE.0 .AND. NHALF.EQ.0)THEN
                    RW( ITPCO + JTPCRZ ) = RW( ITPCO +
     &                JTPCRZ ) - ZCORR
                    IW( ITPCO + JTPCOF ) = IW( ITPCO +
     &                JTPCOF ) + 10
                  ENDIF
                ENDIF
C
C  Process r-phi correction
C
                IF(SUMQR.GT.0.)THEN
                  RCORR = NAVE * (SUMR - AVE * SUMWR)/SUMQR/(NAVE-1)
                  RPCORR = 0.65 * TANPCA *
     &                  SIGN(MIN(ABS(RCORR),0.8),RCORR)
C
C  Subtract the correction factor to get a new r-phi in sector coordinates
C  Do not correct half-pad coordinates
C
                  IF(ICORU.NE.0 .AND. NHALF.EQ.0)THEN
                    RW( ITPCO + JTPCRR ) = RW( ITPCO +
     &                JTPCRR ) - RPCORR
                    IW( ITPCO + JTPCOF ) = IW( ITPCO +
     &                JTPCOF ) + 100
                  ENDIF
                ENDIF
              ENDIF
C
C Create wire z coordinates for TWZZ
C
              NREJEC = 0
   80         CONTINUE
              IF (NFIT.GT.2) THEN
C Do straight line fit.
                CALL UTLSLF(SFIT,ZFIT,WFIT,NFIT,GRAD,ZFITW,CHI2,
     &            VARFIT)
                CL = PROB(CHI2,NFIT - 2)
C If confidence level is suspicious, reject worst point and try again.
                IF (CL.LT.CLMIN .OR. VARFIT(3).LE.0.) THEN
                  IF (NREJEC.LT.MAXREJ) THEN
C Find worst point.
                    JWORST = 0
                    CHI2JW = 0.0
                    DO 85 J = 1,NFIT
                      ZWDIFF = ZFIT(J) - (GRAD*SFIT(J) + ZFITW)
                      CHI2J = WFIT(J)*ZWDIFF**2
                      IF (CHI2J.GE.CHI2JW) THEN
                        JWORST = J
                        CHI2JW = CHI2J
                      END IF
   85               CONTINUE
C Remake list of wires to be used in fit, rejecting worst point.
                    NREJEC = NREJEC + 1
                    NFOLD = NFIT
                    NFIT = 0
                    DO 90 K = 1,NFOLD
                      IF (K.NE.JWORST) THEN
                        NFIT = NFIT + 1
                        SFIT(NFIT) = SFIT(K)
                        ZFIT(NFIT) = ZFIT(K)
                        WFIT(NFIT) = WFIT(K)
                        QFIT(NFIT) = QFIT(K)
                      END IF
   90               CONTINUE
                    GOTO 80
                  END IF
                ELSE
C Fit successful.
C Get next row in TWZZ, handle bookkeeping

                  ITWZZ = KNEXT(KTWZZ)
                  IW(KTWZZ+LMHROW) = IW(KTWZZ+LMHROW)+1
                  IW( ITPCO + JTPCOF ) = 
     &                  IW( ITPCO + JTPCOF ) + 1000*IW(KTWZZ+LMHROW)

C   Calculate consistency of pad and wire z coordinates.
C   Fill TWZZ bank.
                  IW(ITWZZ + JTWZNW) = NFIT
                  RW(ITWZZ + JTWZC2) = CHI2
                  RW(ITWZZ + JTWZDZ) = ZFITW + ZCORR
                  RW(ITWZZ + JTWZSZ) = VARFIT(3)
                  RW(ITWZZ + JTWZPU) = (ZFITW + ZCORR)/
     &                  SQRT(EZPAD2 + VARFIT(3))
C   Create a TWZA bank for this coordinate if possible (for calibration)
                  IF (NSP.GT.0) THEN
                    IW(1) = 1
                    LTWZA = LMHLEN + NSP*LTWZAA
                    CALL AUBOS('TWZA',IW(KTWZZ+LMHROW),LTWZA,
     &                  KTWZA,IGARB)
                    IF (IGARB.NE.0) THEN
                      IER = 13
                      CALL RERROR('TWIREZ',IER,
     &                      'Insufficient BOS space.')
                      GOTO 999
                    ENDIF
                    IW(KTWZA+LMHROW) = 0
                    IW(KTWZA+LMHCOL) = LTWZAA
                    QSC = 0.
                    DO JSP = ISP+1,ISP+NSP
                        QSC = QSC + ITABL(KTSPU,JSP,JTSPTC)
                    ENDDO
                    DO JSP = ISP+1,ISP+NSP
C Twire-Tpad
                      IF (ITABL(KTSPU,JSP,JTSPTC)/QSC .GE. 0.2) THEN
                        ITWZA = KNEXT(KTWZA)
                        IW(KTWZA+LMHROW) = IW(KTWZA+LMHROW)+1
                        RW(ITWZA+JTWZDT) = (ZFITW+ZPAD+ZCORR)
     &                      /TVDRFT(IENDTP(ISECTP))
     &                      -(RTABL(KTSPU,JSP,JTSPPT)
     &                      +TZOFFS(ITALG)-TVOFFS(ISECTP))
                        RW(ITWZA+JTWZM1) = RTABL(KTSPU,JSP,JTSPIT)
                        RW(ITWZA+JTWZTL) = TANL
                      ENDIF
                    ENDDO
                  ENDIF
                END IF
              END IF
            ENDIF
   95     CONTINUE
        END IF
C
  100 CONTINUE
C
C Conclude
  999 CONTINUE
C If successful, add TWZZ bank to output list. Otherwise drop it.
      IF (IER.EQ.0) THEN
        CALL AUBPRS('TWZZ')
        CALL BLIST(IW,'E+','TWZZ')
        CALL AUBPRS('TWZA')
        CALL BLIST(IW,'E+','TWZA')
      ELSE
        IF (KTWZZ.GT.0) KTWZZ = NDROP('TWZZ',0)
        CALL BDROP(IW,'TWZA')
        CALL BDROP(IW,'TWHT')
      END IF
      CALL WDROP(IW,IDTSCL)
      CALL WDROP(IW,IDTSPU)
      CALL WDROP(IW,IDTWRR)
      CALL WDROP(IW,IDTWRF)
      CALL WDROP(IW,IDTSIR)
      CALL WDROP(IW,IDTSLE)

      RETURN
      END
#endif
