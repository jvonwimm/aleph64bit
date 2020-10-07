      SUBROUTINE VDECOD
C----------------------------------------------------------------------
C!  - Main VDET hit reconstruction routine
C!
C!
C!  This is the micro-vertex silicon detector internal co-ordinate
C!  reconstruction routine.  This takes as input the raw detector
C!  data (VHLS and VPLH), plus maps of the detector bonding
C!  taken from the ALEPH database, and produces final hit banks.
C!
C!  12-1-93  Rewrite for 95 VDET compatibility  D. Brown
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C!           Nov   1995 P. Bright-Thomas
C!                 Rewrite fix for cabling errors using new bank VCAB
C!
C?
C!======================================================================
#ifndef DOC
C      IMPLICIT NONE
       SAVE FIRST, NAVCSG, NAVPLH, NAVTRS, NAVPHN, NAVCAB
C
C
C  Global includes
C
#include "rcurnt.h"
#include "vcsgjj.h"
#include "vprtnc.h"
#include "vrecon.h"
#include "vdflgs.h"
C      INTEGER JVFPPH,LVFPHA
#include "vfphjj.h"
C      INTEGER JVFLFG,LVFLGA
#include "vflgjj.h"
C      INTEGER JVFHHA,LVFHLA
#include "vfhljj.h"
C      INTEGER JVCOCM,LVCOMA
#include "vcomjj.h"
#include "vhlsjj.h"
#include "vcabjj.h"
#include "vphnjj.h"
C
C  Function calls
C
      INTEGER NLINK
      INTEGER VDYEAR
      INTEGER NDROP, VNSCRM, VNELRM, NSWAP
C
C  Local variables
C
      INTEGER NAVCSG,NAVPLH,NAVTRS,NAVFPH,NAVPHN,NAVCAB
      INTEGER IGARB,IRET
      INTEGER IRW,ICT
      INTEGER NAMIND,NBANK
      INTEGER KVHLS,KVPLH,KVCSG,KVTRS
      INTEGER KVCAB,NVCAB,IVCAB
      INTEGER JWAF,JWNX,KWAF,LWAF
      INTEGER JLAY,JFAC,JWFF,JVIEW
      INTEGER KLAY,KFAC,KWFF,KVIEW,LLAY,LFAC,LWFF,LVIEW
      INTEGER NVHLS,NVPLH,NVFHL,NVFPH,NVTRS,NVTRSN
      INTEGER NCHAN,MCHNX,NSTRP,NDATA,NSTRI,NDATP
      INTEGER JCHAN,KCHAN,MCHAN,ICHAN,ICHNM
      INTEGER IVHLS,JCLU,KCLU,LCLU,MCLU
      INTEGER IADDR,ILAY,IWFF,IFAC,IVIEW
      INTEGER KVFHL,KVFPH,KVFLG,KVCOM,KVTRSN,KVPHN
      INTEGER IFLAG,NGOOD
      INTEGER NR,NROMD,NRONX,DECMD,KLINE
      INTEGER KRWPH,KMPPH,KRWFL,KMPFL,KCMMD
      LOGICAL GOOD,FIRST
      REAL PH
C
C  Variables for inline functions
C
      INTEGER I,J,PULH,FLAG
      INTEGER SHFT1(0:1)
      INTEGER IAND,ISHFT,IDUM
      INTEGER ODD
#include "bcs.h"
C
C  Inline functions
C
      LOGICAL IHIS1,IHIS8
C
C  Data
C
      DATA SHFT1/16,0/
      DATA FIRST /.TRUE./
C      INTEGER LCOLS,LROWS,ID,KNEXT,KROW,NRBOS,LFRWRD,LFRROW,ITABL,L
C      REAL RTABL
#include "bmacro.h"
C
C  Inlines to unpack the raw VPLH digitized data and flag bits
C
      ODD(J) = IAND(J,1)
      PULH(I,J) = IAND(ISHFT(IW(I+LMHLEN+1+J/2),
     &  -SHFT1(ODD(J))),OBPULH) - IPHOFF
      FLAG(I,J) = IAND(ISHFT(IW(I+LMHLEN+1+J/2),
     &  -SHFT1(ODD(J))),OBFBIT) + 1
      IHIS1(I) = IAND(I,1) .EQ. 1
      IHIS8(I) = IAND(I,8) .EQ. 8


      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVCSG=NAMIND('VCSG')
         NAVPLH=NAMIND('VPLH')
         NAVTRS=NAMIND('VTRS')
         NAVPHN=NAMIND('VPHN')
         NAVCAB=NAMIND('VCAB')
      ENDIF
C
C For VDet95, we need a larger VTRS bank, 3 times longer,
C which we create here with NR=100
C
      KVTRS=IW(NAVTRS)
      IF(KVTRS.NE.0)THEN
         NVTRS=LROWS(KVTRS)
         NDATA=(IW(KVTRS)-LMHLEN)*3+LMHLEN
         CALL AUBOS('VTRS',100,NDATA,KVTRSN,IGARB)
         IF(IGARB.EQ.2)GO TO 300
         IW(KVTRSN+LMHCOL)=IW(KVTRS+LMHCOL)
         IW(KVTRSN+LMHROW)=0
      ENDIF
C
C  Drop the old final banks.
C
      CALL BDROP(IW,'VFHLVFPHVFLGVCOM')
      NEVNTS = NEVNTS + 1
C
C  See if a wafer swap bank exists (do not use for MC).
C
      KVCAB = IW(NAVCAB)
      IF( .NOT.MCEVNT .AND. KVCAB.GT.0)THEN
        NVCAB = LROWS(KVCAB)
      ELSE
        NVCAB = 0
      END IF
C
C Make max. number of channels in Z view, useful for wraparound cluster
C Different for MC and data
C
        IF(MCEVNT)THEN
          ICHNM = VNSCRM(1)
        ELSE
          ICHNM = VNELRM(1)
        ENDIF
C
C  Find the first raw data bank
C
      KVPLH = IW(NAVPLH)
C
C  If any banks exist, create the banks to be used for the full
C  strip mapping
C
      IF(KVPLH.GT.0)THEN
        NDATA = NRCHAN*LVCSGA+LMHLEN
        CALL AUBOS('VCSG',0,NDATA,KVCSG,IGARB)
        IF(IGARB.EQ.2)GOTO 300
        IW(KVCSG+LMHROW)=NRCHAN
        IW(KVCSG+LMHCOL)=LVCSGA
C
C Create also the bank for pulseheight in N strips
C
        KVPHN=0
        IF(VDYEAR().EQ.95)THEN
          NDATP = NRCHAN*LVPHNA+LMHLEN
          CALL AUBOS('VPHN',0,NDATP,KVPHN,IGARB)
          IF(IGARB.EQ.2)GOTO 300
          IW(KVPHN+LMHROW)=NRCHAN
          IW(KVPHN+LMHCOL)=LVPHNA
          KVCSG=IW(NAVCSG)
        ENDIF
        KVPLH = IW(NAVPLH)
      ELSE
C
C  Nothing to do here
C
        GOTO 999
      END IF
C
C  Loop over input VPLH banks
C
C  Do While accepted in JULIA as now works on all compilers
C                                               (P. Comas, May 1995)
      DO WHILE(KVPLH.GT.0)
C
C  Find the corresponding VHLS bank
C
        NR = IW(KVPLH-2)
        KVHLS = NLINK('VHLS',NR)
        IF(KVHLS.LE.0)THEN
          CALL RERROR('VDECOD',2,'Missing VHLS bank')
          GOTO 999
        END IF
C
C get number of raw data clusters and channels
C
        NVHLS=LROWS(KVHLS)
        NVPLH=LROWS(KVPLH)
C
C The raw data are packed 2 readout channels per row
C
        NSTRI=NVPLH*2
C
C  Build the output name banks- maximum possible size of 1 cluster/strip
C  For VDET95, number of strips is 3*number of el. channels
C
        IF(VDYEAR().EQ.95)NSTRI=NSTRI*3
C
C  Plus maximum number of final strips being twice the number of strips.
C  Initialize their mini-headers
C
        KVFHL = NBANK('VFHL',NR,LMHLEN+NSTRI)
        KVCOM = NBANK('VCOM',NR,LMHLEN+NSTRI)
        KVFPH = NBANK('VFPH',NR,LMHLEN+LVFPHA*NSTRI*2)
        KVFLG = NBANK('VFLG',NR,LMHLEN+LVFLGA*NSTRI*2)
        IF(KVFHL.LE.0 .OR. KVCOM.LE.0 .OR. KVFPH.LE.0 .OR.
     &       KVFLG.LE.0 )GOTO 300
C
C Reaccess
C
        KVCSG=IW(NAVCSG)
        KVPLH=NLINK('VPLH',NR)
        KVFHL=NLINK('VFHL',NR)
        KVCOM=NLINK('VCOM',NR)
        KVFPH=NLINK('VFPH',NR)
        KVFLG=NLINK('VFLG',NR)
C
        IW(KVFHL+LMHCOL)=LVFHLA
        IW(KVFHL+LMHROW)=0
        IW(KVCOM+LMHCOL)=LVCOMA
        IW(KVCOM+LMHROW)=0
        IW(KVFPH+LMHCOL)=LVFPHA
        IW(KVFPH+LMHROW)=0
        IW(KVFLG+LMHCOL)=LVFLGA
        IW(KVFLG+LMHROW)=0
C
C  Uncode the wafer address of the first cluster
C
        IADDR = ITABL(KVHLS,1,JVHLHC)
        CALL VUNADD(IADDR,MCHAN,ILAY,IWFF,IFAC,IVIEW,JCHAN)
        CALL VAENWA(NROMD,ILAY,IWFF,IFAC,IVIEW)
        CALL VJWFFW(ILAY,IFAC,IWFF,JWAF)
C
C  Keep the (uncoded) wafer address of this cluster
C
        JLAY = ILAY
        JFAC = IFAC
        JWFF = IWFF
        JVIEW = IVIEW
C
C  Loop over clusters in this bank
C
        ICT=0
        DO IVHLS=1,NVHLS
C
C  Histograms
C
          IF(IHIS1(HISTLV))THEN
            CALL HFILL(HISOFF+15,FLOAT(MCHAN),0.0,1.0)
          END IF
C
C  Increment counter for run summary
C
          NRAWHT(IVIEW) = NRAWHT(IVIEW) + 1
C
C  Loop over the channels readout in this cluster
C
          DO KCHAN=1,MCHAN
C
C  Row in VPLH banks
C
            IRW = ICT+KCHAN-1
C
C  Channel number for this channel, including offset
C
            ICHAN = JCHAN+KCHAN-1
C
C  For Vdet 95 there can be a cluster which wraps around the end
C
            IF(VDYEAR().EQ.95.AND.IVIEW.EQ.1)THEN
              IF(ICHAN.GT.ICHNM)ICHAN=ICHAN-ICHNM
            ENDIF
C
C   Now we transform the MC data into the on-line convention :
C   there are CHNOFF unused readout channels at the beginning
C
            ICHAN = ICHAN+CHNOFF(IVIEW)
C
C Protect against the channel 0 which should never contain any data
C
            IF(ICHAN.LE.0)GO TO 101
C
            KLINE = KROW(KVCSG,ICHAN)
C
C  Unpack the pulseheights and flags into the appropriate raw columns
C
            IW(KLINE+JVCSRP) = PULH(KVPLH,IRW)
            IW(KLINE+JVCSRF) = FLAG(KVPLH,IRW)
C
C  Histograms
C
            IF(IHIS1(HISTLV))THEN
              PH = FLOAT(ITABL(KVCSG,ICHAN,JVCSRP))
              IFLAG = ITABL(KVCSG,ICHAN,JVCSRF)
              IF(IAND(IFLAG,OBFBIT).EQ.0)THEN
                CALL HFILL(HISOFF+10,PH,0.0,1.0)
              ELSE IF(IAND(IFLAG,OBSUPP).EQ.OBSUPP)THEN
                CALL HFILL(HISOFF+11,PH,0.0,1.0)
              ELSE
                CALL HFILL(HISOFF+12,PH,0.0,1.0)
              END IF
              CALL HFILL(HISOFF+50+IVIEW,FLOAT(JWAF),
     &             FLOAT(ISHFT(IFLAG,-14)),1.0)
            END IF
            IF(IHIS8(HISTLV).AND. PH .GT.MNPULS)THEN
              IF(IVIEW .EQ. 1)THEN
                CALL HFILL(HISOFF+300+JWAF,FLOAT(ICHAN),0.0,1.0)
              ELSE
                CALL HFILL(HISOFF+500+JWAF,FLOAT(ICHAN),0.0,1.0)
              END IF
            END IF
  101     CONTINUE
          END DO
          IF(IVHLS.LT.NVHLS)THEN
C
C  Uncode the wafer address of the next cluster
C
            IADDR = ITABL(KVHLS,IVHLS+1,JVHLHC)
            CALL VUNADD(IADDR,MCHNX,ILAY,IWFF,IFAC,IVIEW,JCHAN)
            CALL VAENWA(NRONX,ILAY,IWFF,IFAC,IVIEW)
            CALL VJWFFW(ILAY,IFAC,IWFF,JWNX)
          ENDIF
C
C  If next readout module is different, or end of bank,
C  one readout module has been finished, process it
C
          IF(NRONX.NE.NROMD.OR.IVHLS.EQ.NVHLS)THEN
             DECMD = NROMD
C
C  Fix cabling errors if needed (require NVCAB.gt.0 before executing)
C
            IF( NVCAB.GT.0 ) THEN
              DO IVCAB=1,NVCAB
C
C  Get the apparent (wrong) wafer.
C
                LLAY = ITABL(KVCAB,IVCAB,JVCAAL)
                LFAC = ITABL(KVCAB,IVCAB,JVCAAF)
                LWFF = ITABL(KVCAB,IVCAB,JVCAAW)
                LVIEW= ITABL(KVCAB,IVCAB,JVCAAV)
C
C  If the apparent wafer is wrong, replace it with the true wafer.
C
                IF( JLAY.EQ.LLAY .AND. JFAC.EQ.LFAC .AND.
     +              JWFF.EQ.LWFF .AND. JVIEW.EQ.LVIEW )THEN
                  KLAY = ITABL(KVCAB,IVCAB,JVCATL)
                  KFAC = ITABL(KVCAB,IVCAB,JVCATF)
                  KWFF = ITABL(KVCAB,IVCAB,JVCATW)
                  KVIEW= ITABL(KVCAB,IVCAB,JVCATV)
                  CALL VAENWA(DECMD,KLAY,KWFF,KFAC,KVIEW)
                  CALL RERROR('VDECOD',9,' Cable swap fix.')
                END IF
              END DO
            ENDIF
C
C  Map the channels onto strips.
C


C  There is a different treatment for old and new Vdet :
C  -  Different versions of the EC to strips mapping
C  -  for new Vdet, common mode subtraction is done on-line
C
            IF(VDYEAR().EQ.91)THEN
C
C  Map the channels onto strips.
C  This also suppresses hot channels and strips
C
              CALL VDSMAP(DECMD,GOOD)
C
C treatment of pinholes in the Monte Carlo
C
              IF(MCEVNT) CALL VDPINH(DECMD)
C
C
C  Subtract common mode
C  For new Vdet, will be done on-line
C
              CALL VCALCM(NSTRP)
            ELSE
              CALL VDSM95(DECMD)
            ENDIF
C
C  Final reclustering
C
            CALL VRECLU(DECMD,NR)
C
C  Zero the mapping bank for the new wafer
C
            CALL VZERO(IW(KVCSG+LMHLEN+1),NDATA-LMHLEN)
            IF(KVPHN.NE.0)CALL VZERO(IW(KVPHN+LMHLEN+1),NDATP-LMHLEN)
          ENDIF
C
C Move along the VPLH bank, by the length of the cluster
C
          ICT=ICT+MCHAN
C
C  Reset variables
C
          NROMD=NRONX
          JWAF=JWNX
          JLAY=ILAY
          JFAC=IFAC
          JWFF=IWFF
          JVIEW=IVIEW
          MCHAN=MCHNX
        ENDDO
C
C  Remake the named banks to size, if there are at least 1 cluster
C
        CALL AUBPRS('VFHLVCOMVFPHVFLG')
C
C  Move to the next VPLH bank
C
        KVPLH = IW(KVPLH-1)
      END DO
C
C  Drop the strip mapping bank, and clean up
C
      CALL BDROP(IW,'VCSGVPHN')
      CALL BGARB(IW)
C
C Now some bookkeeping of VTRS banks
C
      KVTRSN=NLINK('VTRS',100)
      IF(KVTRSN.NE.0)THEN
        NVTRSN=LROWS(KVTRSN)
        IF(NVTRSN.GT.0)THEN
          IRET=NSWAP('VTRS',0,'VTRS',100)
          IRET=NDROP('VTRS',100)
          CALL AUBPRS('VTRS')
        ENDIF
      ENDIF
      GOTO 999
C
C  Insufficient BOS space return
C
  300 CONTINUE
      CALL RERROR('VDECOD',1,'Out of BOS space- event rejected')
      CALL BDROP(IW,'VFHLVCOMVFPHVFLGVCSGVTRSVCAB')
      CALL BGARB(IW)
  999 RETURN
      END
#endif
