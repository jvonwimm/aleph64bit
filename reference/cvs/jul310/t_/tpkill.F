      SUBROUTINE TPKILL
C
C-----------------------------------------------------------------------
C! Remove TPC bad channels from Monte Carlo input
C
C  Author:   R. Johnson   7-09-89
C  Modified: R. Johnson  21-01-91  handle the TRIR and TRDI banks
C  Modified: W. Wiedenmann 3-2-92  handle the TSIR and TSDI banks
C  Modified: R. Johnson 2-3-93     fix bug introduced in mods of
C                                  3-2-92
C
C  NOTE: this routine requires as input the TCLB and TKAP banks, which
C        provide lists of bad channels.  These banks may be extracted
C        for the run headers of real data tapes for the runs of interest
C
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "tclbjj.h"
#include "tpgpar.h"
#include "thrpjj.h"
#include "thpljj.h"
#include "rcurnt.h"
#include "rlunit.h"
C
      DIMENSION IOBAD(LTSECT),NBADS(LTSECT)
      CHARACTER TEXT*80
      LOGICAL FIRST,EASY,GOOD,BTEST,FPRIN
      LOGICAL FSMART
      CHARACTER NAM1*4,NAM2*4
      INTEGER GTSTUP,ALGTDB
      DATA FIRST/.TRUE./,FPRIN/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPAD=NAMIND('TPAD')
        NTPDI=NAMIND('TPDI')
        NTRIR=NAMIND('TRIR')
        NTRDI=NAMIND('TRDI')
        NTSIR=NAMIND('TSIR')
        NTSDI=NAMIND('TSDI')
        NTCLB=NAMIND('TCLB')
        NTKAP=NAMIND('TKAP')
        NTHPL=NAMIND('THPL')
        NTHRP=NAMIND('THRP')
      ENDIF
C
C++   Find pointers into TKAP for each sector
C
      KTCLB=IW(NTCLB)
      KTKAP=IW(NTKAP)
      IF (KTCLB.EQ.0 .OR. KTKAP.EQ.0) THEN
C
C++     Read them from the database if they weren't present in cards
C
        IRET = 0
        IF(IRUNRC.LE.2000)THEN
            ITP = GTSTUP('TP',IRUNRC)
        ELSE
            ITP = IRUNRC
        ENDIF
        IRET = ALGTDB(JUNIDB(NTPAD),'TCLBTKAP',-ITP)
        IF (IRET .EQ. 0) THEN
          CALL RERROR('TPKILL',2,'No bad channel data available.'//
     &                   ' No channels will be killed.')
          RETURN
        ENDIF
        KTKAP = IW(NAMIND('TKAP'))
        KTCLB = IW(NAMIND('TCLB'))
      ENDIF
      IF (FPRIN) THEN
        FPRIN=.FALSE.
        WRITE(LOUTRL,3512) LROWS(KTKAP)
 3512   FORMAT(/' TPKILL: dead channels will be simulated in the TPC.'/
     &          I4,' channels have been flagged dead by the ',
     &          'TKAP bank.'/)
      ENDIF
C
      KTHPL=IW(NTHPL)
      KTHRP=IW(NTHRP)
      EASY= KTHPL.NE.0 .AND. KTHRP.NE.0
C
      CALL VZERO(NBADS,LTSECT)
      NBTOT=0
      DO 83 ISLOT=1,LROWS(KTCLB)
        NBADS(ISLOT)=ITABL(KTCLB,ISLOT,JTCLNT)
        IOBAD(ISLOT)=NBTOT
        NBTOT=NBTOT+NBADS(ISLOT)
   83 CONTINUE
C
C++   First do the wires
C
      KTSIR=IW(NTSIR)
      IF (KTSIR.NE.0) THEN
         FSMART=.TRUE.
         NAM1='TSIR'
         NAM2='TSDI'
         KTRIR=KTSIR
      ELSE
         FSMART=.FALSE.
         NAM1='TRIR'
         NAM2='TRDI'
         KTRIR=IW(NTRIR)
      ENDIF
  802 IF (KTRIR.EQ.0) GO TO 901
        IF (IW(KTRIR).EQ.0) THEN
          KTRIR=IW(KTRIR-1)
          GO TO 802
        ENDIF
        ISLOT=IW(KTRIR-2)
        KTRDI=NLINK(NAM2,ISLOT)
        IF (KTRDI.EQ.0) THEN
          KTRIR=IW(KTRIR-1)
          GO TO 802
        ENDIF
C
C++     Loop over all hits and copy the good ones to temp banks
C
        IWRD1=KTRDI+1
        IBT1=24
        IWRD2=KTRDI+1
        IBT2=24
        NHIT=0
        KTKAP=IW(NTKAP)
        DO 850 IH=1,IW(KTRIR)
          IHTWD=IW(KTRIR+IH)
          JWR=IBITS(IHTWD,24,8)
C
C++       Is this wire on the bad channel list?
C
          GOOD=.TRUE.
          DO 840 JB=1,NBADS(ISLOT)
            JWD=IW((KTKAP+LMHLEN+IOBAD(ISLOT))+JB)
            JSLOT=IBITS(JWD,24,8)
            IF (JSLOT.NE.ISLOT) THEN
              CALL RERROR('TPKILL',3,
     &              'TCLB and TKAP are out of sequence.')
              GO TO 840
            ENDIF
            JROW=IBITS(JWD,8,8)
            IF (JROW.NE.0) GO TO 840
            JTHR=IBITS(JWD,16,8)
            IF (JTHR.NE.255) GO TO 840
            IWR=IBITS(JWD,0,8)
            IF (IWR.EQ.JWR) THEN
              GOOD=.FALSE.
              GO TO 841
            ENDIF
  840     CONTINUE
  841     CONTINUE
C
C++       Copy good hits to an earlier position in the bank
C
          IF (GOOD) THEN
            NHIT=NHIT+1
            IW(KTRIR+NHIT)=IHTWD
          ENDIF
          IF (.NOT.BTEST(IHTWD,13)) THEN
C
C++         Also copy the digitizations if pulse isn't reduced
C
            IF ((NAM1.EQ.'TSIR').AND.(BTEST(IHTWD,14)
     &                         .OR.BTEST(IHTWD,15))) THEN
              NSMP=0
            ELSE
              NSMP=IBITS(IHTWD,16,8)
            ENDIF
            DO 845 I=1,NSMP
              IF (GOOD) THEN
                CALL MVBITS(IW(IWRD1),IBT1,8,IW(IWRD2),IBT2)
                IBT2=IBT2-8
                IF (IBT2.LT.0) THEN
                  IBT2=24
                  IWRD2=IWRD2+1
                ENDIF
              ENDIF
              IBT1=IBT1-8
              IF (IBT1.LT.0) THEN
                IBT1=24
                IWRD1=IWRD1+1
              ENDIF
  845       CONTINUE
          ENDIF
  850   CONTINUE
C
C++     Reset the size of the banks
C
        LEN=IWRD2-KTRDI
        IF (IBT2.EQ.24) LEN=LEN-1
        CALL AUBOS(NAM2,ISLOT,LEN,KTRDI,IGARB)
        CALL AUBOS(NAM1,ISLOT,NHIT,KTRIR,IGARB)
C
        KTRIR=IW(KTRIR-1)
        GO TO 802
  901 CONTINUE
C
C++   Loop over all sectors and do the pads
C
      KTPAD=IW(NTPAD)
   84 IF (KTPAD.EQ.0) GO TO 94
        ISLOT=IW(KTPAD-2)
        KTPDI=NLINK('TPDI',ISLOT)
        IF (KTPDI.NE.0) THEN
C
C++     Book new pad banks with temporary names
C
          IW(1)=1
          CALL AUBOS('xxxh',ISLOT,IW(KTPAD),KXXXH,IGARB)
          IF (IGARB.EQ.2) THEN
            CALL RERROR('TPKILL',1,'No room to kill bad hits')
            CALL BDROP(IW,'xxxhxxxd')
            RETURN
          ENDIF
          IF (IGARB.EQ.1) THEN
            KTPAD=NLINK('TPAD',ISLOT)
            KTPDI=NLINK('TPDI',ISLOT)
          ENDIF
          CALL AUBOS('xxxd',ISLOT,IW(KTPDI),KXXXD,IGARB)
          IF (IGARB.EQ.2) THEN
            CALL RERROR('TPKILL',1,'No room to kill bad hits')
            CALL BDROP(IW,'xxxhxxxd')
            RETURN
          ENDIF
          IF (IGARB.EQ.1) THEN
            KTPAD=NLINK('TPAD',ISLOT)
          ENDIF
        ENDIF
        KTPAD=IW(KTPAD-1)
        GO TO 84
   94 CONTINUE
C
      KTPAD=IW(NTPAD)
  103 IF (KTPAD.EQ.0) GO TO 150
        IF (IW(KTPAD).EQ.0) THEN
          KTPAD=IW(KTPAD-1)
          GO TO 103
        ENDIF
        ISLOT=IW(KTPAD-2)
        KTPDI=NLINK('TPDI',ISLOT)
        IF (KTPDI.EQ.0) THEN
          KTPAD=IW(KTPAD-1)
          GO TO 103
        ENDIF
        KXXXH=NLINK('xxxh',ISLOT)
        IF (KXXXH.EQ.0) THEN
          KTPAD=IW(KTPAD-1)
          GO TO 103
        ENDIF
        KXXXD=NLINK('xxxd',ISLOT)
        IF (KXXXD.EQ.0) THEN
          KTPAD=IW(KTPAD-1)
          GO TO 103
        ENDIF
        IF (EASY) THEN
          KTHRP=NLINK('THRP',ISLOT)
        ELSE
          KTHRP=0
        ENDIF
C
C++     Loop over all hits and copy the good ones to temp banks
C
        NSMO=0
        NSMN=0
        NCP=0
        IPT=0
   40   IF (IPT.GE.IW(KTPAD)-2) GO TO 90
          IPT=IPT+1
          IPTS=IPT
          IROW=IBITS(IW(KTPAD+IPT),0,4)
          IPT=IPT+1
          NHT=IW(KTPAD+IPT)
C
C++       Protect against missing hits (data format error)
C
          IF (IPT+NHT.GT.IW(KTPAD)) GO TO 90
C
C++       Loop over all hits on this padrow
C
          NCPS=-1
          DO 60 I=1,NHT
            IPT=IPT+1
            IHTWD=IW(KTPAD+IPT)
            JPD=IBITS(IHTWD,24,8)
            NSMP=IBITS(IHTWD,16,8)
C
C++         Is this pad in the bad channel list?  Use unpacked data
C++         if they are available (as in JULIA)
C
            GOOD=.TRUE.
            IF (KTHRP.NE.0) THEN
              NBD=ITABL(KTHRP,IROW,JTHRNB)
              IOF=ITABL(KTHRP,IROW,JTHRPT)
              DO 45 JB=1,NBD
                IPAD=ITABL(KTHPL,IOF+JB,JTHPPN)
                IF (IPAD.GT.JPD) GO TO 46
                ITHR=ITABL(KTHPL,IOF+JB,JTHPTH)
                IF (ITHR.NE.255) GO TO 46
                IF (IPAD.EQ.JPD) THEN
                  GOOD=.FALSE.
                  GO TO 46
                ENDIF
   45         CONTINUE
   46         CONTINUE
            ELSE
              KTKAP=IW(NTKAP)
              DO 50 JB=1,NBADS(ISLOT)
                JWD=IW((KTKAP+LMHLEN+IOBAD(ISLOT))+JB)
                JSLOT=IBITS(JWD,24,8)
                IF (JSLOT.NE.ISLOT) THEN
                  CALL RERROR('TPKILL',3,
     &                  'TCLB and TKAP are out of sequence.')
                  GO TO 50
                ENDIF
                JROW=IBITS(JWD,8,8)
                IF (JROW.NE.IROW) GO TO 50
                JTHR=IBITS(JWD,16,8)
                IF (JTHR.NE.255) GO TO 50
                IPAD=IBITS(JWD,0,8)
                IF (IPAD.EQ.JPD) THEN
                  GOOD=.FALSE.
                  GO TO 51
                ENDIF
   50         CONTINUE
   51         CONTINUE
            ENDIF
C
C++         Copy good hits to the temporary bank
C
            IF (GOOD) THEN
C
C++           Copy the row header for the first good point
C
              IF (NCPS.EQ.-1) THEN
                NCP=NCP+1
                IW(KXXXH+NCP)=IW(KTPAD+IPTS)
                NCP=NCP+1
                NCPS=NCP
                IW(KXXXH+NCPS)=0
              ENDIF
              NCP=NCP+1
              IW(KXXXH+NCP)=IW(KTPAD+IPT)
              IW(KXXXH+NCPS)=IW(KXXXH+NCPS)+1
C
C++           Copy the samples too
C
              DO 58 JS=1,NSMP
                ISO=NSMO+JS
                IWDO= (ISO-1)/4 + 1
                IBYO= 1 - ISO + 4*IWDO
                NSMN=NSMN+1
                IWDN= (NSMN-1)/4 + 1
                IBYN= 1 - NSMN + 4*IWDN
                CALL MVBITS(IW(KTPDI+IWDO),8*(IBYO-1),8,
     &                      IW(KXXXD+IWDN),8*(IBYN-1))
   58         CONTINUE
            ENDIF
            NSMO=NSMO+NSMP
   60     CONTINUE
          GO TO 40
   90   CONTINUE
C
C++     Set the size of the new banks
C
        CALL AUBOS('xxxh',ISLOT,NCP,KXXXH,IGARB)
        NWDS= NSMN/4
        IF (NWDS.NE.NSMN*4) NWDS=NWDS+1
        CALL AUBOS('xxxd',ISLOT,NWDS,KXXXD,IGARB)
C
        KTPAD=IW(KTPAD-1)
        GO TO 103
  150 CONTINUE
C
C++   Swap names and drop the old input data
C
      CALL BSWAP(IW,'TPAD','xxxh')
      CALL BSWAP(IW,'TPDI','xxxd')
      CALL BDROP(IW,'xxxhxxxd')
C
  999 CONTINUE
      RETURN
      END
#endif
