      SUBROUTINE HMFIND
C-----------------------------------------------------------------
C!    -Looks for muons in hcal
C!
C!    Author:    F.Bossi -LNF-   3/88
C!
C!    Description:
C!               This routine looks if TPC tracks are
C!               associated to muon-like (i.e. penetrating,
C!               minimum ionizing) particles. A preliminary
C!               identification test is done and the bank
C!               HMAD is filled.
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "rflags.h"
#include "hdebug.h"
#include "rconds.h"
#include "alcons.h"
#include "frftjj.h"
#include "rlunit.h"
#include "hmadjj.h"
#include "muocut.h"
#include "bmacro.h"
C
C              Looks for TPC tracks
      NFRFT=NAMIND('FRFT')
      JFRFT=IW(NFRFT)
      IF(JFRFT.EQ.0) GO TO 99
      NTRKS=IW(JFRFT+2)
      JHTUB=NLINK('HTUB',0)
      IF(JHTUB.EQ.0) GO TO 99
      NHITS=IW(JHTUB+2)
C
C            create HMAD bank (Nr=0)
         LEN=2+NTRKS*LHMADA
         CALL AUBOS('HMAD',0,LEN,JHMAD,IGARB)
         IF (IGARB.EQ.1) JFRFT=IW(NFRFT)
         IF(IGARB.EQ.2) THEN
             CALL RERROR('HMFIND',1,'no space for HMAD bank')
             GO TO 99
         ENDIF
         IW(JHMAD+1)=LHMADA
         IW(JHMAD+2)=0
         KHMAD=JHMAD+2
C               loop over tracks
      DO 40 ITRAC=1,NTRKS
        JTREX=NLINK('TREX',ITRAC)
        IF (JTREX.EQ.0) GO TO 40
         CURV =RTABL(JFRFT,ITRAC,JFRFIR)
         TADIP=RTABL(JFRFT,ITRAC,JFRFTL)
         RHO=1./ABS(CURV)
         PT=FIELRC*RHO*CLGHT/100000.
         THETA = ATAN2 (1.,TADIP)
         STHET = SIN(THETA)
         PPINI  = PT/STHET
         IF (PPINI .LT. PCUTHA) GO TO 40
        CALL HMROAD(ITRAC,NFPL,NEXP,NENF,NFLT,MHLT,IGFL,IEBM,ITBM)
        IF(IGFL.EQ.-1) GO TO 99
        IF(JHISRF(JULHC).GE.1) CALL HFILL(7105,FLOAT(NEXP),0.,1.)
        IF(NEXP.LT.10) GO TO 40
        NEXP=NEXP-NENF
        ECLU=0.
        CALL HMENER(ITRAC,ECLU)
        CHIS=0.
        NDFR=0
C               chisquare calculation
C       CALL HMTEST
C               Fills HMAD bank
        IW(KHMAD+JHMANF)=NFPL
        IW(KHMAD+JHMANE)=NEXP
        IW(KHMAD+JHMANL)=NFLT
        IW(KHMAD+JHMAMH)=MHLT
        IW(KHMAD+JHMAIG)=IGFL
        RW(KHMAD+JHMAED)=ECLU
        RW(KHMAD+JHMACS)=CHIS
        IW(KHMAD+JHMAND)=NDFR
        IW(KHMAD+JHMAIE)=IEBM
        IW(KHMAD+JHMAIT)=ITBM
        IW(KHMAD+JHMATN)=ITRAC
C               compute preliminary classification flag IASS
C               (IASS=-1 --> hadron; =0 --> not classif, =1 --> muon)
      NDIF=NEXP-NFPL
      IF(NDIF.LT.0)NDIF=0
      IF(NFLT.GT.10)NFLT=10
      IASS=1
      IF(NFLT.LT.NLAS10.OR.NDIF.GE.NEXNFO) IASS=0
      IF(NFLT.LT.NLAS10.AND.NDIF.GE.NEXNFO) IASS=-1
      IF(IASS.EQ.1.AND.NFPL.LT.NMINFP) IASS=0
      JFKIN=NLINK('FKIN',0)
      IF(JFKIN.GT.0 .AND. IASS.GE.0 .AND.IHPRIN.GE.1)
     + CALL MUKINE(1,IASS,ITRAC)
      IF(IHPRIN.GE.1.AND.IASS.GE.0)
     +WRITE(LDEBRL,78)ITRAC,NHITS,NFPL,NEXP,NFLT,MHLT,IGFL,IASS
 78    FORMAT(' ITRAC,NHITS,NFPL,NEXP,NFLT,MHLT,IGFL,IASS=',8I4)
      IW(KHMAD+JHMAIF)=IASS
      KHMAD=KHMAD+LHMADA
      IW(JHMAD+2)=IW(JHMAD+2)+1
C                         fill histograms
      IF(JHISRF(JULHC).GE.1) THEN
         IOFFS=0
         IF(IGFL.GT.0) IOFFS=30
         FIASS=IASS+5+IOFFS
         FDIFF=NHITS-NFPL+IOFFS
         FNDIF=NDIF+IOFFS
         FNFLT=NFLT+IOFFS
         FMHLT=MHLT+IOFFS
         FNFPL=NFPL+IOFFS
         CALL HFILL(7101,FLOAT(IGFL),0.,1.)
         CALL HFILL(7102,FIASS,0.,1.)
         IF(NTRKS.EQ.1) CALL HFILL(7103,FDIFF,0.,1.)
         CALL HFILL(7104,FNDIF,0.,1.)
         CALL HFILL(7106,FNFLT,0.,1.)
         CALL HFILL(7107,FMHLT,0.,1.)
         CALL HFILL(7108,FNFPL,0.,1.)
      ENDIF
C               next track
 40   CONTINUE
C
      IF ( LROWS(JHMAD) .EQ. 0 ) THEN
         IND = NDROP('HMAD',0)
      ELSE
         LEN=2+LROWS(JHMAD)*LHMADA
         CALL AUBOS('HMAD',0,LEN,JHMAD,IGARB)
         IF(IGARB.EQ.2) CALL RERROR('HMFIND',2,'no update of HMAD bank')
      ENDIF
 99   CONTINUE
      RETURN
      END
#endif
