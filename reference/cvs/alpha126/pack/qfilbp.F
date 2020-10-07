      SUBROUTINE QFILBP
CKEY FILL BEAMPOS/INTERNAL
C-----------------------------------------------------------------------
C! fill beam position from GET_BP algorithm
C  then find the interaction point from QFNDIP algorithm
C  called from QFILL
C                                                 J.Boucrot   14.06.93
C   Modified to call QFNDIP                       J.Boucrot   06.10.93
C   Modified to call KGTBLQ                       O.Schneider 12.11.96
C   Change QFGET_BP defaults to crazy values    S.Wasserbaech 03.06.98
C   Split the execution in two                    J.Boucrot   24.09.98
C-----------------------------------------------------------------------
#ifndef DOC
      INTEGER KGTBLQ
#include "maxtrk.h"
#include "qcde.h"
      COMMON / QFILBPI / ISWGET 
      SAVE
      REAL VTX(3),VTE(3),VTS(3),UJETS(3,MAXJET),ZINTP(3),ZERRP(3,3)
      REAL SIG2_BP(3)
      INTEGER NUSED(2)
      INTEGER ISTAT
      INTEGER KBOBS,IERR,MINICHU,BPMGAIN(2)
      CHARACTER*1 COORD(2)
      DATA COORD/'x','y'/
      REAL BOMSIG(2)
      SAVE BOMSIG
      DATA BOMSIG / 0.0040, 0.0010 /
      INTEGER KFILBP
      SAVE    KFILBP
      DATA    KFILBP / -1 /
      LOGICAL FIRST,USEBOM,BOMONLY,DEBUG
      SAVE    FIRST,USEBOM,BOMONLY,DEBUG
      DATA    FIRST,USEBOM,BOMONLY,DEBUG / .TRUE. , 3*.FALSE. /
#include "qmacro.h"
C-----------------------------------------------------------------------
C Initialisations  on first call  :
      IF (FIRST) THEN
         FIRST = .FALSE.
C       Check presence of BOBS card
         KBOBS = IW(NAMIND('BOBS'))
         USEBOM = .FALSE.
         BOMONLY = .FALSE.
         IF(KBOBS.GT.0) THEN 
           DO K = 1,2
             IF(IW(KBOBS).GE.K) BOMSIG(K) = RW(KBOBS+K)
             IF(BOMSIG(K).GE.0.) THEN
               USEBOM=.TRUE.
               WRITE(LOUT,1001) COORD(K),COORD(K),BOMSIG(K) 
             ENDIF
           ENDDO
           IF(USEBOM) THEN
             IF(IW(KBOBS).GE.3) BOMONLY=IW(KBOBS+3).NE.0
             IF(IW(KBOBS).GE.4) DEBUG=IW(KBOBS+4).NE.0
             IF(BOMONLY) THEN
               WRITE(LOUT,1002) 
     &          'VDET or LEP_BOM+QS0 data (whichever is more precise)'
             ELSE
               WRITE(LOUT,1002)
     &          'the combination of VDET and LEP_BOM+QS0 data'
             ENDIF
           ENDIF
 1001      FORMAT(' _QFILBP_ ',T11,'LEP_BOM+QS0 data will be used for ',
     &            A,' beam spot position when available:',/,
     &            T11,'---> assume sigma_',A,'(LEP_BOM+QS0) = ',F8.4)
 1002      FORMAT(' _QFILBP_ Beam spot position determined from ',A)
         ENDIF
      ENDIF
C Execute qfget_bp if ISWGET = 1  and QFNDIp if ISWGET = 2 :
      IF (ISWGET.LE.0.OR.ISWGET.GT.2) GO TO 999
      IF (ISWGET.EQ.2) GO TO 10
C ----------------------------------------------------------------------
C Find the event-chunk beam position :
      KFILBP = 0
C
C     SW, 3 June 1998:
C     Make the default position, uncertainty, and size outrageous to
C     encourage people to check XGETBP:
      DO K = 1,3
         QVTXBP(K) = 999.
         QVTEBP(K) = 0.
         QVTSBP(K) = 0.
      ENDDO
C
C Call QFGET_BP and fill internal ALPHA variables :
C    
      CALL QFGET_BP(KRUN,KEVT,ICHUNK,VTX,VTE,VTS)
      XGETBP=ICHUNK.GE.0
      IF(.NOT.XGETBP) GO TO 999
      KFILBP=1
      DO K=1,3
        QVTXBP(K)=VTX(K)
        QVTEBP(K)=VTE(K)
        QVTSBP(K)=VTS(K)
      ENDDO
C If BOM data wanted, call KGTBLQ and modify ALPHA variables :
      IF (USEBOM) THEN
        IERR = KGTBLQ(KRUN,KEVT,MINICHU,VTX,VTE,BPMGAIN)
        IF(DEBUG) THEN 
          CALL QWMESE('Beam spot debug (QFILBP):')
          PRINT *,'VDET: chunk    =',ICHUNK
          PRINT *,'VDET: position =',QVTXBP
          PRINT *,'VDET: error    =',QVTEBP
          PRINT *,'VDET: size     =',QVTSBP
          PRINT *,'BLQP: KGTBLQ   =',IERR
        ENDIF
        IF (IERR.EQ.0) THEN
          IF(DEBUG) THEN
            PRINT *,'BLQP: minichunk=',MINICHU
            PRINT *,'BLQP: position =',VTX(1),VTX(2)
            PRINT *,'BLQP: error    =',VTE(1),VTE(2)
            PRINT *,'BLQP: error int=',BOMSIG
          ENDIF
          DO K = 1,2
            IF(BOMSIG(K).GE.0.) THEN
              VTE(K) = SQRT(VTE(K)**2+BOMSIG(K)**2)
              IF(BOMONLY) THEN
                IF (VTE(K).LT.QVTEBP(K)) THEN
                  QVTXBP(K) = VTX(K)
                  QVTEBP(K) = VTE(K)
                  KFILBP = KFILBP+K
                ENDIF
              ELSE
                WVDET = 1./QVTEBP(K)**2
                WBLQP = 1./VTE(K)**2
                QVTXBP(K) = (QVTXBP(K)*WVDET+VTX(K)*WBLQP)/
     &                      (          WVDET+       WBLQP)
                QVTEBP(K) = SQRT(1./(  WVDET+       WBLQP))
                IF(KFILBP.EQ.1) KFILBP = 4
                KFILBP = KFILBP+K
              ENDIF
            ENDIF
          ENDDO
          IF(DEBUG) THEN
            PRINT *,'BLQP: error tot=',VTE(1),VTE(2)
            PRINT *,'USED: position =',QVTXBP
            PRINT *,'USED: error    =',QVTEBP
          ENDIF
        ENDIF
        IF(DEBUG) PRINT *,'USED: KFILBP   =',KFILBP
      ENDIF
      GO TO 999
C ------------------------------------------------------------------
C
C If the data card QFND was provided , call the QFNDIP algorithm
C to determine the interaction point for this event :
C
 10   IF (IW(NAMIND('QFND')).EQ.0) GO TO 999
      IF (.NOT.XGETBP) GO TO 999
      NJET=0
      NUSED(1)=0
      NUSED(2)=0
      DO 40 JJ=1,3
         SIG2_BP(JJ)=QVTEBP(JJ)**2+QVTSBP(JJ)**2
         ZINTP(JJ) = 0.
         DO 30 KK = 1,MAXJET
 30      UJETS(JJ,KK) = 0.
         DO 35 LL = 1,3
 35      ZERRP(JJ,LL) = 0.
 40   CONTINUE
      CHI2F=0.
C
      CALL QFNDIP(QVTXBP,SIG2_BP,NJET,UJETS,ZINTP,ZERRP,CHI2F,NUSED)
C
C The ALPHA variables relative to the main ( primary ) vertex are now
C overwritten by the results of QFNDIP :
C
      IF (KVTYPE(KFREV).NE.1)  GO TO 999
      IF (NUSED(2).EQ.-1) THEN
         RW(KOQVRT+KFREV*KCQVRT+JQVRCF)   = -999999.
      ELSE
         RW(KOQVRT+KFREV*KCQVRT+JQVRVX)   = ZINTP(1)
         RW(KOQVRT+KFREV*KCQVRT+JQVRVY)   = ZINTP(2)
         RW(KOQVRT+KFREV*KCQVRT+JQVRVZ)   = ZINTP(3)
         RW(KOQVRT+KFREV*KCQVRT+JQVRCF)   = CHI2F
         RW(KOQVRT+KFREV*KCQVRT+JQVREM  ) = ZERRP(1,1)
         RW(KOQVRT+KFREV*KCQVRT+JQVREM+1) = ZERRP(1,2)
         RW(KOQVRT+KFREV*KCQVRT+JQVREM+2) = ZERRP(2,2)
         RW(KOQVRT+KFREV*KCQVRT+JQVREM+3) = ZERRP(1,3)
         RW(KOQVRT+KFREV*KCQVRT+JQVREM+4) = ZERRP(2,3)
         RW(KOQVRT+KFREV*KCQVRT+JQVREM+5) = ZERRP(3,3)
      ENDIF
C
 999  RETURN
C
C     Entry to return the status of the beam position
C
      ENTRY QFILBP_STATUS(ISTAT)
      ISTAT=KFILBP
      END
#endif
