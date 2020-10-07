        SUBROUTINE MUASS
C-------------------------------------------------------------------
C
C!  Associate extrapolated TPC track to muon hits
C!
C!  G.Capon, M.Dreucci        28-May-87
C!
C!
C!       steering routine : calls Muhits, Mucuts, Mutest, Mukine
C?
C!======================================================================
#ifndef DOC
      DIMENSION DV(7)
#include "bcs.h"
#include "muocut.h"
#include "rcurnt.h"
#include "munamc.h"
#include "mcadjj.h"
#include "alcons.h"
#include "rlunit.h"
#include "rparac.h"
#include "mdebug.h"
#include "mwbank.h"
#include "mthrjj.h"
#include "trexjj.h"
#include "muexjj.h"
#include "bmacro.h"
C
      NFRFT=NAMIND('FRFT')
      JFRFT = IW(NFRFT)
      JMHIT = IW(NAMHIT)
      IF(JFRFT.EQ.0) GO TO 99
      NTRKS=IW(JFRFT+2)
C
C?       Create MCAD bank (Muon Ch. Assoc. Data)
C
      LMCAD = 2 + NTRKS*LMCADA
      CALL AUBOS ('MCAD',0,LMCAD,JMCAD,IGARB)
      IF(IGARB.EQ.2) THEN
          CALL RERROR('MUASS ',1,'no space for MCAD bank')
          GO TO 99
          ENDIF
      IW(JMCAD+1)= LMCADA
      IW(JMCAD+2)= 0
C
C                       create MTHR bank (track to muon-hit relation)
C
      JMHIT = IW(NAMHIT)
C         100 rows are allocated to bank MTHR. Should be by far enough.
C         In any case the bank can be enlarged in routine Mutest.
C                                                    G. Capon Oct-91
      LEN=LMHLEN+100*LMTHRA
      CALL AUBOS('MTHR',0,LEN,JMTHR,IGARB)
      IF(IGARB.EQ.2) THEN
          CALL RERROR('MUASS ',2,'no space for MTHR bank')
          GO TO 99
          ENDIF
      IW(JMTHR+1)=LMTHRA
      IW(JMTHR+2)=0
C
C?      put in work bank: cartesian coord.of muon hits, Int/Ext flag , I
C
      IF(JMHIT.GT.0) CALL MUHITG
C
C?             loop on TPC tracks
C?             NTASS = number of associated tracks
C
      NTASS = 0
      KMUEX = IW(NAMIND('MUEX'))
      DO 10 ITRAC = 1,NTRKS
C
      IF (KMUEX.GT.0) THEN
         JMUEX=NLINK('MUEX',ITRAC)
         IF (JMUEX.LE.0) GO TO 10
         GO TO 15
      ENDIF
C
      JTREX=NLINK('TREX',ITRAC)
      IF(JTREX.EQ.0) GO TO 10
C
      NVETT=IW(JTREX+2)
      PX=RTABL(JTREX,1,JTREP3)
      PY=RTABL(JTREX,1,JTREP3+1)
      PZ=RTABL(JTREX,1,JTREP3+2)
      PPINI=SQRT(PX*PX+PY*PY+PZ*PZ)
C
C?             select penetrating particles
C
      IF (PPINI . LT. PCUTMA) GO TO 10
C
C            reject tracks stopped in Hcal
C
      IFAIL=RTABL(JTREX,1,7)
      IF(IFAIL.NE.1) THEN
       IF (IMPRIN.GE.1) WRITE(LDEBRL,*)' routine MUASS -',
     + ' track stopped in Hcal - IFAIL = ',IFAIL
         GO TO 10
      ENDIF
C
C?             compute cuts on distance and angle (according to mult.sca
C
      CALL MUCUTS (ITRAC,C0,C1,C2)
C
C              fill MUEX bank
C
      KVETT=KROW(JTREX,NVETT)
      CALL UCOPY(RW(KVETT+1),DV,6)
      LEN=2+LMUEXA
      CALL AUBOS('MUEX',ITRAC,LEN,JMUEX,IGARB)
      IF(IGARB.EQ.2) THEN
         CALL RERROR('MUASS',5,' no space for bank MUEX')
         GO TO 99
      ENDIF
      IW(JMUEX+1)=LMUEXA
      IW(JMUEX+2)=1
      CALL UCOPY(DV,RW(JMUEX+2+JMUEXE),6)
      RW(JMUEX+2+JMUEC0)=C0
      RW(JMUEX+2+JMUEC1)=C1
      RW(JMUEX+2+JMUEC2)=C2
      IF (IMPRIN.GE.3) CALL PRTABL('MUEX',ITRAC)
C
C?             search for association to muon chambers hits
C
 15   CONTINUE
      IASS=0
      IF(JMHIT.GT.0) CALL MUTEST (ITRAC,C0,C1,C2,IASS)
      NTASS = NTASS+IASS
C
C?        if bank KINE from Galeph present, find particle type
C
      JFKIN = NLINK('FKIN',0)
      IF(JFKIN.GT.0 .AND.IMPRIN.GE.1) CALL MUKINE(2,IASS,ITRAC)
C
C?             ...and now the next track
C
 10   CONTINUE
C
C?       print number of associated tracks
C
      IF (IMPRIN.GE.1) WRITE (LDEBRL,78) NTASS
C
C?       drop work bank, drop or update MCAD bank
C
      CALL WDROP (IW,IWBANK)
      IF (NTASS.EQ.0) THEN
         IND = NDROP('MCAD',0)
         IND = NDROP('MTHR',0)
      ELSE
         JMCAD=IW(NAMCAD)
         LMCAD= 2+LROWS(JMCAD)*LCOLS(JMCAD)
         CALL AUBOS ('MCAD',0,LMCAD,JMCAD,IGARB)
         IF(IGARB.EQ.2) THEN
             CALL RERROR('MUASS ',3,'NO UPDATE OF MCAD BANK')
             GO TO 99
         ENDIF
         JMTHR=IW(NAMIND('MTHR'))
         LEN=LMHLEN+LROWS(JMTHR)*LCOLS(JMTHR)
         CALL AUBOS ('MTHR',0,LEN,JMTHR,IGARB)
         IF(IGARB.EQ.2) THEN
             CALL RERROR('MUASS ',4,'NO UPDATE OF MTHR BANK')
             GO TO 99
          ENDIF
          IF (IMPRIN.GE.3) CALL PRTABL('MCAD',0)
          IF (IMPRIN.GE.3) CALL PRTABL('MTHR',0)
      ENDIF
C
 99   RETURN
C
 78   FORMAT(I5,' Tracks associated to muon chambers in this event')
C
      END
#endif
