      SUBROUTINE TRKELS(IER)
C
C---------------------------------------------------------------------
C! Sum up dE/dx for tracks
C!
C!    Author:   R. Johnson   23-04-87
C!    Modified: R. Johnson   21-01-91 flag track segments with more
C!                           than 40% of the samples saturated
C!                           11-09-91 nonlinear and z corrections
C!                           24-08-92 2-sided truncated mean
C!                           2-3-93   data-base control such that
C!                                    old algorithm still works
C!              D. Casper    26-7-98  ensure that zero charge samples
C!                                    remain zero after correction
C!
C!    Output: IER     /I      Set to 1 if BOS runs out of space.
C!                            Set to 0 for a good return.
C!
C!    Called by TPCREC
C!
C!    Description
C!    -----------
C!    This routine calculates the truncated mean for each TPC track.
C!    The output goes into the banks TELS and TEXS.
C!
C!--------------------------------------------------------------------
#ifndef DOC
C
#include "tgftjj.h"
#include "alcons.h"
#include "tparac.h"
#include "tflags.h"
#include "tpgpar.h"
#include "telsjj.h"
#include "texsjj.h"
#include "twitjj.h"
#include "twtbjj.h"
#include "bcs.h"
#include "tpgeow.h"
#include "tpgeom.h"
#include "tpccon.h"
#include "twirct.h"
#include "twircl.h"
C
C++   Workbank indices:
C
      COMMON/TRKEWB/ ISORTW,ITWRRW
C
C++   Local variables:
C
      DIMENSION NCLST(LTSECT),SPLST(LTSECT),SZLST(LTSECT)
      DIMENSION SLLST(LTSECT),NHTOT(LTSECT),ISFLG(LTSECT)
      DIMENSION NHSTR(LTSECT)
      CHARACTER TEXT*120
      LOGICAL FIRST
      SAVE RMEL,RMPI,RMKA,RMPR,FIRST,NTWTB,NTWIT,NTWAT,NTELS,NTEXS
      DATA FIRST/.TRUE./
C
      DATA RMEL/0.000511/,RMPI/0.1395685/,
     &     RMKA/0.493667/,RMPR/0.9382796/
C
C
C-------------------------- Statement Functions ---------------------
C
#include "bmacro.h"
C
C--------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTWTB=NAMIND('TWTB')
        NTWIT=NAMIND('TWIT')
        NTWAT=NAMIND('TWAT')
        NTELS=NAMIND('TELS')
        NTEXS=NAMIND('TEXS')
        NTGFT=NAMIND('TGFT')
        ISORTW=0
        ITWRRW=0
      ENDIF
C
      IER=0
C
C++   Drop the output banks if old ones still are hanging around
C
      IF (IW(NTELS).NE.0) CALL BDROP(IW,'TELS')
      IF (IW(NTEXS).NE.0) CALL BDROP(IW,'TEXS')
C
C++   Check that the required input banks are available
C
      KTWTB=IW(NTWTB)
      IF (KTWTB.EQ.0) GO TO 999
      KTWIT=IW(NTWIT)
      IF (KTWIT.EQ.0) GO TO 999
      KTWAT=IW(NTWAT)
      IF (KTWAT.EQ.0) GO TO 999
      KTGFT=IW(NTGFT)
      IF (KTGFT.EQ.0) GO TO 999
C
C++   Create a workbank for sorting dE/dx samples
C
      NWTOT=NTWIRE(1)+NTWIRE(2)
      IW(1)=1
      CALL WBANK(IW,ISORTW,2*NWTOT,*998)
      IW(ISORTW-3)=INTCHA('WRTW')
C
C++   Create the named banks for output
C
      LEN= LROWS(KTWIT)*LTELSA + LMHLEN
      IW(1)=1
      CALL AUBOS('TELS',0,LEN,KTELS,IGARB)
      IF (IGARB.EQ.2) GO TO 997
      CALL BLIST(IW,'T+','TELS')
      IW(KTELS+LMHCOL)=LTELSA
      IW(KTELS+LMHROW)=0
C
      KTWIT=IW(NTWIT)
      MXTEX=3*LROWS(KTWIT)
      LEN= MXTEX*LTEXSA + LMHLEN
      IW(1)=1
      CALL AUBOS('TEXS',0,LEN,KTEXS,IGARB)
      IF (IGARB.EQ.2) GO TO 997
      CALL BLIST(IW,'T+','TEXS')
      IW(KTEXS+LMHCOL)=LTEXSA
      IW(KTEXS+LMHROW)=0
C
      KTWTB=IW(NTWTB)
      KTWIT=IW(NTWIT)
      KTWAT=IW(NTWAT)
      KTELS=IW(NTELS)
      KTGFT=IW(NTGFT)
C
      CALL BWIND(IW,'TWRR',LTSECT,ITWRRW)
C
C++   Loop over all tracks with associated wires
C
      CINT=1.0-SLOPCL*ALOG(TWSTEP(1))
      DO 500 ITK=1,LROWS(KTWIT)
        NWR=ITABL(KTWIT,ITK,JTWING)
C
C++     Cut on number of associated wire pulses
C
        IF (NWR.LT.MNSPTW) GO TO 500
        IF (NWR.GT.NWTOT) THEN
          WRITE(TEXT,360) NWR
          CALL RERROR('TRKELS',1,TEXT(1:115))
  360     FORMAT('Too many (',I4,') wire samples on a single track.&',
     &           'The excess data are being lost.  ',
     &           'This indicates a problem in TRKWRA')
          NWR=NWTOT
        ENDIF
C
        TANL=RTABL(KTGFT,ITK,JTGFTL)
        SINT=1./SQRT(1.+TANL**2)
C
        IW(KTELS+LMHROW)=LROWS(KTELS)+1
        MTELS=LROWS(KTELS)
C
        IOFF=ITABL(KTWIT,ITK,JTWIOS)+LMHLEN
C
C++     Make a list of dE/dx values to use for sorting.  Also, count the
C++     number of samples in each sector and the track length.  We
C++     apply here a correction for variations in sample length, which
C++     includes a logarithmic correction.  Since the sample length
C++     usually varies only slowly along a track, we can use a truncated
C++     Taylor expansion to keep the logarithm updated.
C
        SMP0=RTABL(KTWTB,IW(KTWAT+IOFF+1),JTWTSL)
        SMPL=ALOG(SMP0)
        CALL VZERO(NHTOT(1),LTSECT)
        DO 400 IWR=1,NWR
          ITW=IW(KTWAT+IOFF+IWR)
          CHRGS=RTABL(KTWTB,ITW,JTWTCE)
          SMP=RTABL(KTWTB,ITW,JTWTSL)
          RNCR= (SMP-SMP0)/SMP0
          IF (ABS(RNCR).LT.RNCRTW) THEN
            SMPL=SMPL + (1. - 0.5*RNCR)*RNCR
          ELSE
            SMPL=ALOG(SMP)
          ENDIF
          IDD=ITABL(KTWTB,ITW,JTWTWI)
          ISLOT=IDD/65536
C
C++       Make a non-linear correction for sample length, plus a
C++       correction for the drift dependence
C
          ZTK=RTABL(KTWTB,ITW,JTWTPN)
          Q=CHRGS/(CINT+SLOPCL*SMPL)
          IF (Q .GT. 0) THEN
           IF (GRNMCL.GT.0.) THEN
            IF (.NOT.FZERTW) THEN
C
C++           Here, correct only for the dependence on the angle
C++           in the x-y plane (used starting 1992).
C
              SPERP=SMP*SINT
              QCNL=GRNMCL*(Q/SMP + (1.-(TWSTEP(1)/SPERP))*RNLNCL)
     &                                             / (1.+ADSPCL*ZTK)
            ELSE
C
C++           Here correct for overall dependence on angle (1991 data)
C
              QCNL=GRNMCL*(Q/SMP + (1.-(TWSTEP(1)/SMP))*RNLNCL)
     &                                             / (1.+ADSPCL*ZTK)
            ENDIF
           ELSE
            QCNL=Q/SMP
           ENDIF
          ELSE
            QCNL = 0.
          ENDIF
          RW(ISORTW+IWR)=QCNL
          IF (FZERTW) THEN
            IF (CHRGS.GT.0.) NHTOT(ISLOT)=NHTOT(ISLOT)+1
          ELSE
            NHTOT(ISLOT)=NHTOT(ISLOT)+1
          ENDIF
          SMP0=SMP
  400   CONTINUE
C
C++     Sort the dE/dx values
C
        CALL SORTZV(RW(ISORTW+1),IW(ISORTW+NWR+1),NWR,1,0,0)
C
C++     If requested, we can ignore the hits with zero charge
C++     (from wires below threshold)
C
        IBEG=1
        IF (FZERTW) THEN
          DO 3189 K=1,NWR
            IWR=IW(ISORTW+NWR+K)
            CHRGS=RW(ISORTW+IWR)
            IF (CHRGS.EQ.0.) THEN
              IBEG=IBEG+1
            ELSE
              GO TO 3190
            ENDIF
 3189     CONTINUE
 3190     CONTINUE
        ENDIF
C
C++     Calculate the truncated sum of dE/dx and the average drift
C++     separately for each sector.
C
        CALL VZERO(NCLST(1),LTSECT)
        CALL VZERO(SZLST(1),LTSECT)
        CALL VZERO(SPLST(1),LTSECT)
        CALL VZERO(SLLST(1),LTSECT)
        CALL VZERO(ISFLG(1),LTSECT)
        DO 368 ISLOT=1,LTSECT
          NHSTR(ISLOT)=MAX(1,NHTOT(ISLOT)*KTRNTW/100)
          NHTOT(ISLOT)=NHTOT(ISLOT)*JTRNCL/100
  368   CONTINUE
        DO 450 K=IBEG,NWR
          IWR=IW(ISORTW+NWR+K)
          ITW=IW(KTWAT+IOFF+IWR)
          IDD=ITABL(KTWTB,ITW,JTWTWI)
          ISLOT=IDD/65536
          IF (NCLST(ISLOT).GE.NHTOT(ISLOT)) GO TO 450
          CHRGS=RW(ISORTW+IWR)
C
C++       Are any saturated pulses ending up in the final sum?
C++       Since the samples are arranged in order of PH, if any
C++       are saturated, the last one will be.
C
          IF (NCLST(ISLOT).EQ.NHTOT(ISLOT)-1.AND.CHRGS.GT.0.) THEN
            IF (IW(ITWRRW).GE.ISLOT) THEN
              KTWRR=IW(ITWRRW+ISLOT)
            ELSE
              KTWRR=0
            ENDIF
            IF (KTWRR.GT.0) THEN
              IHPT=IDD - ISLOT*65536
              IHTWD=IW(KTWRR+LMHLEN+IHPT)
              IPH=IBITS(IHTWD,14,10)
              IF (IPH.GE.1023) ISFLG(ISLOT)=1
            ELSE
              CALL RERROR('TRKELS',4,'Cannot find the TWRR bank in '//
     &                    'order to test for saturation.')
            ENDIF
          ENDIF
          NCLST(ISLOT)=NCLST(ISLOT) + 1
          SLLST(ISLOT)=SLLST(ISLOT) + RTABL(KTWTB,ITW,JTWTSL)
          IF (NCLST(ISLOT).GE.NHSTR(ISLOT)) THEN
            IF (CHRGS.LE.0.) THEN
              SPLST(ISLOT)=SPLST(ISLOT) + RNRMCL(ISLOT)*ADCMTW
            ELSE
              SPLST(ISLOT)=SPLST(ISLOT) + CHRGS
            ENDIF
            ZPUL=RTABL(KTWTB,ITW,JTWTPN)
            SZLST(ISLOT)=SZLST(ISLOT) + ZPUL
          ENDIF
  450   CONTINUE
C
C++     Fill an entry into TEXS for each sector with hits on this track.
C
        IOFXS=LROWS(KTEXS)
        NSECT=0
        DO 470 ISLOT=1,LTSECT
          IF (NCLST(ISLOT).LT.4) GO TO 470
C
C++       Extend the TEXS bank in the unlikely event that it
C++       runs out of space.
C
          IF (LROWS(KTEXS).EQ.MXTEX) THEN
            LEN= IW(KTEXS) + MXTEX*LTEXSA
            MXTEX=2*MXTEX
            CALL AUBOS('TEXS',0,LEN,KTEXS,IGARB)
            IF (IGARB.EQ.2) GO TO 997
            IF (IGARB.NE.0) THEN
              KTELS=IW(NTELS)
              KTWTB=IW(NTWTB)
              KTWIT=IW(NTWIT)
              KTWAT=IW(NTWAT)
            ENDIF
          ENDIF
          NSECT=NSECT+1
          IW(KTEXS+LMHROW)=LROWS(KTEXS)+1
          MTEXS=LROWS(KTEXS)
C
          RTRNK=FLOAT(NCLST(ISLOT)-NHSTR(ISLOT)+1)
          IW(KROW(KTEXS,MTEXS)+JTEXSI)=ISLOT
          RW(KROW(KTEXS,MTEXS)+JTEXTM)=SPLST(ISLOT)/RTRNK
          RW(KROW(KTEXS,MTEXS)+JTEXTL)=SLLST(ISLOT)
          IW(KROW(KTEXS,MTEXS)+JTEXNS)=NCLST(ISLOT)
          RW(KROW(KTEXS,MTEXS)+JTEXAD)=SZLST(ISLOT)/RTRNK
          IW(KROW(KTEXS,MTEXS)+JTEXTN)=ITK
          IW(KROW(KTEXS,MTEXS)+JTEXSF)=ISFLG(ISLOT)
  470   CONTINUE
C
C++     Fill the pointer bank, TELS
C
        IW(KROW(KTELS,MTELS)+JTELNG)= NSECT
        IW(KROW(KTELS,MTELS)+JTELSO)= IOFXS
        IW(KROW(KTELS,MTELS)+JTELTN)= ITK
  500 CONTINUE
C
C++   Set final sizes of the output banks
C
      CALL AUBPRS('TELSTEXS')
      GO TO 999
C
  997 CONTINUE
      IER=2
      CALL RERROR('TRKELS',2,'No room in BOS for name banks')
      GO TO 999
  998 CONTINUE
      IER=3
      CALL RERROR('TRKELS',3,'No room in BOS for workbanks')
  999 CONTINUE
      CALL WDROP(IW,ISORTW)
      CALL WDROP(IW,ITWRRW)
      RETURN
      END
#endif
