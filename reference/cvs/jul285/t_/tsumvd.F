      SUBROUTINE TSUMVD
C!----------------------------------------------------------------------
C! Create and fill TSVD bank.
C!
C!  Author: I.Tomalin  4/8/95
C!
C! Input banks: FRFT, FRTL, FTCL, TPCO, VDZT, VDXY, VTXT
C! Output bank: TSVD
C!
C! Loop over good tracks having all coords. in one TPC half.
C!   Extrapolate each track back to VDET using VTXT bank.
C!   Find VDET z hits in +-2cm window around extrapolation.
C!   If only 1 hit in window note its residual in TSVD bank.
C! End Loop
C! See ALEPH note 95-108 for details.
C?
C!======================================================================
#ifndef DOC
      SAVE
C Track selection cuts.
      PARAMETER(D0CUT=2.0,Z0CUT=10.0,PCUT=0.5,NTCUT=10,INTCUT=0,NICUT=0)
      PARAMETER(EPS=1.0E-15)
C
      LOGICAL VINSEN,VSLOTF
      INTEGER VNRWFF
      EXTERNAL VINSEN,VSLOTF,VNRWFF
      DIMENSION ABC(3),VUW(3)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C TSVD work bank containing histogram of VDET z residuals.
#include "tsvdwb.h"
C Window in z residual.
      PARAMETER(ZWIN=2.0)
C Number of bins in z residual.
      PARAMETER(NZBIN=JTSVNZ-JTSVHZ)
C Bin size in z residual.
      PARAMETER(ZBIN=2.0*ZWIN/NZBIN)
C
#include "frftjj.h"
#include "frtljj.h"
#include "tpcojj.h"
#include "vtxtjj.h"
#include "vdztjj.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpccon.h"
#include "rcurnt.h"
#include "bcs.h"
#include "bosext.h"
#include "bmacro.h"
C---------------------------------------------------------------------------
C Initialize TSVD work bank index.
      IF (FIRST) THEN
        FIRST = .FALSE.
        KTSVD = 0        
      END IF
C
C Require event to be in time with beam crossing using ECAL wires.
      CALL TIZERO(TMEAN,IGOOD)
C     IF (IGOOD.EQ.0) GOTO 999
C If in bunch train mode, require bunch number to be fairly certain.
      CALL RQBUNC(IBUN,INBU,NWAG,IQUA)
      IF (NWAG.NE.1.AND.IQUA.LT.2) GOTO 999
C
C Find track banks.
      KFRFT = NLINK('FRFT',0)
      KFRTL = NLINK('FRTL',0)
      KFTCL = NLINK('FTCL',0)
      KTPCO = NLINK('TPCO',0)
      IF (KFRFT.EQ.0.OR.KFRTL.EQ.0.OR.KFTCL.EQ.0.OR.KTPCO.EQ.0) GOTO 999
C
      NTRACK = LROWS(KFRFT)
      IF (NTRACK.EQ.0) GOTO 999
C
C Require some activity in the VDET (to check that it is on).
      IF (IW(NAMIND('VDZT')).EQ.0.AND.IW(NAMIND('VDXY')).EQ.0) GOTO 999
C
C Create TSVD work bank if it does not already exist.
      IF (KTSVD.EQ.0) THEN
C Detector has two sides (A and B).
        NSIDE = 2
        CALL WBANK(IW,KTSVD,LMHLEN+LTSVDA*NSIDE,*990)
        IW(KTSVD - 3) = INTCHA('TSVD')
        IW(KTSVD + LMHCOL) = LTSVDA
        IW(KTSVD + LMHROW) = NSIDE
C Note size of window in z residual
        RW(KROW(KTSVD,1) + JTSVWZ) = ZWIN
        RW(KROW(KTSVD,2) + JTSVWZ) = ZWIN
      END IF
C
C Loop over tracks fitted in ITC+TPC.
C
      DO 100 I = 1,NTRACK
C
C Select decent tracks.
C
        IFRFT = KROW(KFRFT,I)
        D0   = RW(IFRFT + JFRFD0)
        Z0   = RW(IFRFT + JFRFZ0)
        TANL = RW(IFRFT + JFRFTL)
        RI   = RW(IFRFT + JFRFIR)
        IF (RI.NE.0.0) THEN
          R0 = 1.0/RI
        ELSE
          R0 = 1.0/EPS
        END IF
        PP = ABS(BCFGEV)*SQRT(1.0 + TANL**2)*ABS(R0)
        IF (ABS(D0).GT.D0CUT) GOTO 100
        IF (ABS(Z0).GT.Z0CUT) GOTO 100
        IF (PP.LT.PCUT)       GOTO 100
C
C Cut on number of ITC+TPC coordinates.
C
        IFRTL = KROW(KFRTL,I)
        NTPC  = IW(IFRTL + JFRTNT)
        NITC  = IW(IFRTL + JFRTNI)
        NVDET = IW(IFRTL + JFRTNV)
        IF (NTPC.LT.NTCUT)    GOTO 100
        IF (NITC.LT.NICUT)    GOTO 100
C
C Require all TPC coordinates on the track to be on one side of TPC.
C Also, to reject tracks produced outside VDET, require some coords.
C in inner 5 TPC layers, if no cut is being made on ITC.
        ITOFF = IW(IFRTL + JFRTIT)
        NINNER = 0
        DO 35 N = 1,NTPC
          NCO   = IW(KFTCL + LMHLEN + ITOFF + N)
          ID    = ITABL(KTPCO,NCO,JTPCIN)
          ISLOT = MOD(ID/1000,100)
          IF (N.EQ.1) THEN
            ISIDE = IENDTP(ISLOT)
          ELSE
            IF (ISIDE.NE.IENDTP(ISLOT)) GOTO 100
          END IF
C
          IPROW = ID/100000
          IF (IPROW.LE.5) NINNER = NINNER + 1
   35   CONTINUE
C
        IF (NINNER.LT.INTCUT.AND.NICUT.EQ.0) GOTO 100
C
C Use VTXT bank to extrapolate track to VDET.
        KVTXT = NLINK('VTXT',I)
        IF (KVTXT.GT.0) THEN
C
C Loop over faces intersected by track.
C As error is dominated by TPC track, average these to produce just one
C entry in TSVD for the track.
          NEXPEC = 0
          NFOUND = 0
          SUMZR = 0.0
          SUMWR = 0.0
          DO 75 J = 1,LROWS(KVTXT)
            IVTXT = KROW(KVTXT,J)
C Find global face and wafer number.
            IWID = IW(IVTXT + JVTXWI)
            CALL VADEWA(IWID,ILAY,IWFF,IFACE,IVIEW)
            CALL VJFACI(ILAY,IFACE,JFACE)
            CALL VJWFFW(ILAY,IFACE,IWFF,JWAF)
C Does this slot contain a face ?
            IF (VSLOTF(JFACE)) THEN
C Are we inside sensitive area of wafer ?
              VUW(1) = 0.0
              VUW(2) = RW(IVTXT + JVTXUC)
              VUW(3) = RW(IVTXT + JVTXWC)
              CALL VVUWAB(VUW,JWAF,ABC)
              IF (VINSEN(1,ABC(1),ABC(2))) THEN
C Find intercept of track with wafer in z, and its error.
                ZINT   = RW(IVTXT + JVTXZC)
                ERZINT = RW(IVTXT + JVTXSW)
C Loop over VDET hits inside wafer (and two neighbouring wafers in z).
                INSIDE = 0
                DO 60 INEIGH = -1,1
                  IF (IWFF.EQ.1.AND.INEIGH.EQ.-1) GOTO 60
                  IF (IWFF.EQ.VNRWFF().AND.INEIGH.EQ.1) GOTO 60
                  KVDZT = NLINK('VDZT',IWID + 1000*INEIGH)
                  IF (KVDZT.GT.0) THEN
                    DO 50 IHIT = 1,LROWS(KVDZT)
                      IVDZT = KROW(KVDZT,IHIT)
C If coordinate lies close to track, note its z and error.
                      IF (ABS(RW(IVDZT+JVDZZC) - ZINT).LT.ZWIN) THEN
                        INSIDE = INSIDE + 1
                        ZCO    = RW(IVDZT+JVDZZC)
                        ERZCO  = RW(IVDZT+JVDZSW)
                        ZRES   = ZCO - ZINT
                        E2ZRES = ERZINT**2 +  ERZCO**2
                      END IF
   50               CONTINUE
                  END IF
   60           CONTINUE
C
C Require coordinate association to be unambiguous.
                IF (INSIDE.GT.1) GOTO 75
C
C Hit expected for this track.
                NEXPEC = 1
C Check if hit is present.
                IF (INSIDE.EQ.1) THEN
C Convert VDET residual and its error to that expected for full drift
C length.
                  ZRESF  = ZRES/(1.0 - ABS(ZCO)/ZTPCMX)
                  E2RESF = E2ZRES/(1.0 - ABS(ZCO)/ZTPCMX)**2
C Begin to form average for this track.
C
                  NFOUND = NFOUND + 1
                  SUMZR = SUMZR + ZRESF
                  SUMWR = SUMWR + 1.0/E2RESF
                END IF
C
              END IF
            END IF
   75     CONTINUE
C
C Fill TSVD bank using this track.
C
          ITSVD = KROW(KTSVD,ISIDE)
C
C Note expected number of VDET hits.
          IW(ITSVD + JTSVEX) = IW(ITSVD + JTSVEX) + NEXPEC

          IF (NFOUND.GE.1) THEN
C
C Calculate average VDET residual (at full drift length) for this track.
            ZRESF = SUMZR/FLOAT(NFOUND)
            WRESF = SUMWR/FLOAT(NFOUND)
C Make weighted histogram VDET z residual.
            IZBIN = 1 + INT((ZRESF + ZWIN)/ZBIN)
            IF (IZBIN.GE.1.AND.IZBIN.LE.NZBIN) THEN
C Note observed number of VDET hits.
              IW(ITSVD + JTSVNU) = IW(ITSVD + JTSVNU) + 1
C Fill TSVD histogram.
              ICOL = (JTSVHZ - 1) + IZBIN
              RW(ITSVD + ICOL) = RW(ITSVD + ICOL) + WRESF
C Also fill unweighted histogram.
              ICOL = (JTSVNZ - 1) + IZBIN
              IW(ITSVD + ICOL) = IW(ITSVD + ICOL) + 1
            END IF
C
          END IF
        ELSE
          CALL RERROR ('TSUMVD',1,'Missing VTXT bank')
        END IF
C
  100 CONTINUE
      GOTO 999
C
  990 CONTINUE
      CALL RERROR ('TSUMVD',2,'Unable to create TSVD work bank')
C
  999 CONTINUE
      END
#endif
