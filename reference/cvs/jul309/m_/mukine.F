      SUBROUTINE MUKINE (ICALL,IASS,ITRAC)
C----------------------------------------------------------
C!      Search KINE bank associated to current TPC track
C!
C!      G.Capon, M.Dreucci                                    28-May-87
C!
C!     This routine looks for Galeph info on the track
C!     (particle type, if primary or secondary, if prompt
C!      muon was not associated)
C!
C! input : ICALL =1 (Hcal association), =2 (Mu chambers association)
C!         IASS  = 0/1 track is : not associated/associated
C!         ITRAC : track index in FRFT bank
C! output : none (only debugging printout)
C!
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rparac.h"
#include "rcurnt.h"
#include "rconds.h"
#include "alcons.h"
#include "fverjj.h"
#include "fkinjj.h"
      CHARACTER*10 MESS(4),CALLS(2)
      REAL PLOW(2)
      DATA PLOW/0.5,1.5/
      DATA CALLS/'  HCAL-ASS','  MUCH-ASS'/
      DATA MESS /'  HADRON  ','  PRI-MUON','  SEC-MUON',' LOST-MUPR'/
#include "bmacro.h"
C
      JFKIN=NLINK('FKIN',0)
C
C?      get px,py,pz of TPC fit
C
      NFRFT=NAMIND('FRFT')
      JFRFT=IW(NFRFT)
      CURV =RTABL(JFRFT,ITRAC,1)
      TADIP=RTABL(JFRFT,ITRAC,2)
      PHI  =RTABL(JFRFT,ITRAC,3)
      RHO=1./ABS(CURV)
      PT=FIELRC*RHO*CLGHT/100000.
      PXTR = PT*COS(PHI)
      PYTR = PT*SIN(PHI)
      PZTR = PT*TADIP
      PFRFT=SQRT(PT*PT+PZTR*PZTR)
      DELMI  = 999999.
      ITSEL = 0
      NPMU=0
C
C?      loop on KINE tracks, get px,py,pz (KINE values)
C?      skip neutrals and unstable particles
C?      skip low momentum (0.5 Gev) particles
C
       NTRKK=LROWS(JFKIN)
       DO 10 ITRKK=1,NTRKK
      JTYPE=ITABL(JFKIN,ITRKK,JFKIPA)
      IF(JTYPE.EQ. 1.OR.JTYPE.EQ. 4) GO TO 10
      IF(JTYPE.EQ. 7.OR.JTYPE.EQ.10) GO TO 10
      IF(JTYPE.EQ.13.OR.JTYPE.GT.15) GO TO 10
      PXKI = RTABL(JFKIN,ITRKK,JFKIPX)
      PYKI = RTABL(JFKIN,ITRKK,JFKIPY)
      PZKI = RTABL(JFKIN,ITRKK,JFKIPZ)
      PPKIN=SQRT(PXKI*PXKI+PYKI*PYKI+PZKI*PZKI)
      IF(PPKIN.LE.PLOW(ICALL)) GO TO 10
C           skip if track origin vertex outside TPC fiducial volume
      NVERT=ITABL(JFKIN,ITRKK,JFKIOV)
      IF (JTYPE.EQ.5 .OR. JTYPE.EQ.6) THEN
         IF(NVERT.EQ.1) NPMU=NPMU+1
      ENDIF
      JFVER=NLINK('FVER',0)
      RV=SQRT(RTABL(JFVER,NVERT,JFVEVX)**2+RTABL(JFVER,NVERT,JFVEVY)**2)
      ZV=RTABL(JFVER,NVERT,JFVEVZ)
      IF(RV.GT.160.  .OR.  ABS(ZV).GT.220.) GO TO 10
C
C?      select KINE values closest to TPC values
C
      DELTA = (PXTR-PXKI)**2+(PYTR-PYKI)**2+(PZTR-PZKI)**2
      IF(DELTA.LT.DELMI) THEN
             ITSEL = ITRKK
             DELMI = DELTA
             ENDIF
 10   CONTINUE
C
C?        if associated : print particle type (Geant) and prim/secondary
C?        if not associated and prompt muon : print warning
C
 20   CONTINUE
      IF(ITSEL.EQ.0) GO TO 99
      PKINE = SQRT (RTABL(JFKIN,ITSEL,JFKIPX)**2+
     + RTABL(JFKIN,ITSEL,JFKIPY)**2+RTABL(JFKIN,ITSEL,JFKIPZ)**2)
      ITYPE = ITABL(JFKIN,ITSEL,JFKIPA)
      IMUON=0
      IF(ITYPE.EQ.5 .OR. ITYPE.EQ.6) IMUON=1
      MVERT = ITABL(JFKIN,ITSEL,JFKIOV)
      IF(IASS.EQ.0) THEN
           IMESS=0
           IF(MVERT.EQ.1 .AND. IMUON.EQ.1) IMESS=4
      ELSE
         IMESS=1
         IF(IMUON.EQ.1 .AND. MVERT.EQ.1) IMESS=2
         IF(IMUON.EQ.1 .AND. MVERT.GT.1) IMESS=3
      ENDIF
      IF(IMESS.GT.0)WRITE(LDEBRL,77)IEVTRC,NPMU,ITRAC,PFRFT,ITYPE,
     +PKINE,CALLS(ICALL),MESS(IMESS)
C
 99   RETURN
C
 77   FORMAT(' EVT,NPMU,ITRAC,PFRFT:TYPE,PKINE',I6,I2,I3,F6.2,
     +I3,F6.2,2A10)
C
      END
#endif
