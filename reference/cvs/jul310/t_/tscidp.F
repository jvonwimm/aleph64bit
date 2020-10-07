      SUBROUTINE TSCIDP(IUNIT)
C
C----------------------------------------------------------------------
C! Dump slow control bank for TPC
C
C  Author:  R. Johnson   21-09-89
C
C  Input:   IUNIT      /I    FORTRAN output unit
C
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "evehjj.h"
#include "runhjj.h"
C
      DIMENSION IBT(0:31),VBUF(6),IBUF(6),JBUF(6)
C
      KTSCI=IW(NAMIND('TSCI'))
      KTSSR=IW(NAMIND('TSSR'))
      IF (KTSCI.NE.0) THEN
        KEVEH=IW(NAMIND('EVEH'))
        IF (KEVEH.NE.0) THEN
          IRUN=IW(KEVEH+JEVERN)
          IEVT=IW(KEVEH+JEVEEV)
        ELSE
          IRUN=0
          IEVT=0
        ENDIF
        WRITE(IUNIT,'(/,A,I6,A,I6)')
     &         ' ****** Dump of bank TSCI, Run ',IRUN,', Event ',IEVT
        KNEVE=IW(NAMIND('NEVE'))
        IF (KNEVE.NE.0) THEN
          ITIM=IW(KNEVE+2)
          IDAT=IW(KNEVE+1)
          WRITE(IUNIT,197) ITIM,IDAT
  197     FORMAT(8X,'TPC slow control information at time=',I10,
     &           ' and date=',I10)
        ELSE
          WRITE(IUNIT,'(A)') ' No NEVE bank found for time and date.'
        ENDIF
      ELSEIF (KTSSR.NE.0) THEN
        KRUNH=IW(NAMIND('RUNH'))
        IF (KRUNH.NE.0) THEN
          IRUN=IW(KRUNH+JRUNRN)
          ITIM=IW(KRUNH+JRUNST)
          IDAT=IW(KRUNH+JRUNSD)
        ELSE
          ITIM=0
          IDAT=0
          KRUNR=IW(NAMIND('RUNR'))
          IF (KRUNR.NE.0) THEN
            IRUN=IW(KRUNR+JRUNRN)
          ELSE
            IRUN=0
          ENDIF
        ENDIF
        WRITE(IUNIT,'(/,A,I6)') ' ****** Dump of bank TSSR, Run ',IRUN
        WRITE(IUNIT,197) ITIM,IDAT
      ENDIF
      IF (KTSCI.EQ.0) THEN
        KTSCI=KTSSR
      ENDIF
      IF (KTSCI.EQ.0) THEN
        WRITE(IUNIT,'(A)') ' No TSSR or TSCI bank found'
        RETURN
      ENDIF
C
C++   Loop over hardware indices
C
      IPT=2
   10 IF (IPT.GE.IW(KTSCI)) GO TO 800
        IPT=IPT+1
        IDXH= IW(KTSCI+IPT)
        IF (IDXH.EQ.1) THEN
          IPT=IPT+1
          WRITE(IUNIT,'(/,A)') ' Fastbus status bits:'
          NW=IW(KTSCI+IPT)
          DO 20 I=1,NW
            IPT=IPT+1
            DO 29 J=0,31
              IBT(J)=IBITS(IW(KTSCI+IPT),J,1)
   29       CONTINUE
            WRITE(IUNIT,'(4X,32(1X,I1))') IBT
   20     CONTINUE
        ELSEIF (IDXH.EQ.2) THEN
          IPT=IPT+1
          WRITE(IUNIT,23)
   23     FORMAT(/,' Preamplifier voltages:',/
     &             3(1X,'Slot',3X,'V1',5X,'V2',4X))
          NW=IW(KTSCI+IPT)
          K=0
          DO 30 I=1,NW
            IPT=IPT+1
            K=K+1
            VBUF(K)=FLOAT(IW(KTSCI+IPT))/100.
            IBUF(K)=(I-1)/2 + 1
            IF (K.EQ.6) THEN
              WRITE(IUNIT,'(3(2X,I2,2X,F5.1,2X,F5.1,3X))')
     &                  IBUF(1),VBUF(1),VBUF(2),
     &                  IBUF(3),VBUF(3),VBUF(4),
     &                  IBUF(5),VBUF(5),VBUF(6)
              K=0
            ENDIF
   30     CONTINUE
        ELSEIF (IDXH.EQ.3) THEN
          IPT=IPT+1
          WRITE(IUNIT,33) ' Gating voltages:'
   33     FORMAT(/,A,/,2(1X,'Slot',2X,'V1',5X,'V2',5X,'V2',4X))
          NW=IW(KTSCI+IPT)
          K=0
          DO 40 I=1,NW
            IPT=IPT+1
            K=K+1
            VBUF(K)=FLOAT(IW(KTSCI+IPT))/100.
            IBUF(K)= (I-1)/3 + 1
            IF (K.EQ.6) THEN
              WRITE(IUNIT,'(2(1X,I2,2X,F6.1,1X,F6.1,1X,F6.1,2X))')
     &              IBUF(1),(VBUF(L),L=1,3),
     &              IBUF(4),(VBUF(L),L=4,6)
              K=0
            ENDIF
   40     CONTINUE
        ELSEIF (IDXH.EQ.4) THEN
          IPT=IPT+1
          NW=IW(KTSCI+IPT)
          WRITE(IUNIT,43) ' High voltage currents:'
   43     FORMAT(/,A,/,4(1X,'Slot',2X,'I',4X))
          K=0
          DO 50 I=1,36
            K=K+1
            IBUF(K)=IW(KTSCI+IPT+I)
            JBUF(K)=I
            IF (K.EQ.4) THEN
              WRITE(IUNIT,'(4(1X,I2,2X,I5,2X))')
     &             (JBUF(L),IBUF(L),L=1,4)
              K=0
            ENDIF
   50     CONTINUE
          WRITE(IUNIT,51) ' High voltage:'
   51     FORMAT(/,A,/,4(1X,'Slot',2X,'V',4X))
          K=0
          DO 52 I=1,36
            K=K+1
            IBUF(K)=IW(KTSCI+IPT+I+36)
            JBUF(K)=I
            IF (K.EQ.4) THEN
              WRITE(IUNIT,'(4(1X,I2,2X,I5,2X))')
     &             (JBUF(L),IBUF(L),L=1,4)
              K=0
            ENDIF
   52     CONTINUE
          IPT=IPT+NW
        ELSEIF (IDXH.EQ.5) THEN
          IPT=IPT+1
          WRITE(IUNIT,53) ' Sector temperatures:'
   53     FORMAT(/,A,/,1X,'Slot',2X,'T1',5X,'T2',5X,'T3',5X,'T4',
     &                           5X,'T5')
          NW=IW(KTSCI+IPT)
          K=0
          DO 60 I=1,NW
            IPT=IPT+1
            K=K+1
            VBUF(K)=FLOAT(IW(KTSCI+IPT))/10.
            ISLOT=(I-1)/5 + 1
            IF (K.EQ.5) THEN
              WRITE(IUNIT,'(1X,I2,5(2X,F5.1))') ISLOT,(VBUF(L),L=1,5)
              K=0
            ENDIF
   60     CONTINUE
        ELSEIF (IDXH.EQ.7) THEN
          IPT=IPT+1
          NW=IW(KTSCI+IPT)
          WRITE(IUNIT,'(/,A)') ' TPC gas system:'
          DO 70 I=1,6
            VBUF(I)=FLOAT(IW(KTSCI+IPT+I))/10.
   70     CONTINUE
          WRITE(IUNIT,'(1X,A,6(2X,F5.1))') 'Gas pressures:   ',
     &                                           (VBUF(L),L=1,6)
          DO 80 I=1,6
            VBUF(I)=FLOAT(IW(KTSCI+IPT+I+6))/10.
   80     CONTINUE
          WRITE(IUNIT,'(1X,A,6(2X,F5.1))') 'Gas temperatures:',
     &                                           (VBUF(L),L=1,6)
          DO 81 I=1,4
            VBUF(I)=FLOAT(IW(KTSCI+IPT+I+12))/10.
   81     CONTINUE
          WRITE(IUNIT,'(1X,A,4(2X,F5.1))') 'Misc. parameters:',
     &                                           (VBUF(L),L=1,4)
          WRITE(IUNIT,'(1X,A)') 'Gas alarm status:'
          DO 82 J=0,31
            IBT(J)=IBITS(IW(KTSCI+IPT+17),J,1)
   82     CONTINUE
          WRITE(IUNIT,'(4X,32(1X,I1))') IBT
          DO 83 J=0,31
            IBT(J)=IBITS(IW(KTSCI+IPT+18),J,1)
   83     CONTINUE
          WRITE(IUNIT,'(4X,32(1X,I1))') IBT
          IPT=IPT+NW
        ELSEIF (IDXH.EQ.8) THEN
          IPT=IPT+1
          NW=IW(KTSCI+IPT)
          MODE=IW(KTSCI+IPT+1)
          MODEF=IW(KTSCI+IPT+2)
          MTIM=IW(KTSCI+IPT+3)
          PEAK=FLOAT(IW(KTSCI+IPT+4))/100.
          AMP=FLOAT(IW(KTSCI+IPT+5))/100.
          DAMP=FLOAT(IW(KTSCI+IPT+6))
          WPOT=FLOAT(IW(KTSCI+IPT+7))/100.
          CPOT=FLOAT(IW(KTSCI+IPT+8))/100.
          PRE=FLOAT(IW(KTSCI+IPT+9))/100.
          TMP=FLOAT(IW(KTSCI+IPT+10))/100.
          WRITE(IUNIT,85) MODE,MODEF,MTIM,PEAK,AMP,DAMP,WPOT,CPOT,
     &                    PRE,TMP
   85     FORMAT(' Monitoring chamber:',/
     &           '    Run mode= ',I4,5X,'Failure mode=',I4,/
     &           '    Run time= ',I12,' minutes',/
     &           '    Fe55 peak= ',F10.2,/
     &           '    Gas amplification= ',F10.2,5X,'Delta=',F8.0,/
     &           '    Wire potential= ',F10.2,' volts',/
     &           '    Corr. potential=',F10.2,' volts',/
     &           '    Pressure=',F6.1,5X,'Temperature=',F6.1)
          IPT=IPT+NW
        ELSE
          IPT=IPT+1
          WRITE(IUNIT,'(/,A,I2,A)') ' Hardware index ',IDXH,':'
          NW=IW(KTSCI+IPT)
          K=0
          DO 90 I=1,NW
            IPT=IPT+1
            K=K+1
            IBUF(K)=IW(KTSCI+IPT)
            IF (K.EQ.6) THEN
              WRITE(IUNIT,'(6(1X,I12))') (IBUF(L),L=1,6)
              K=0
            ENDIF
   90     CONTINUE
        ENDIF
        GO TO 10
  800 CONTINUE
C
      WRITE(IUNIT,820)
  820 FORMAT(/' End of dump of the TPC Slow-Control bank ***********'/)
C
      RETURN
      END
#endif
