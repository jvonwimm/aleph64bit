      SUBROUTINE KXP6ST(KPP)
C------------------------------------------------------------------
C          B.Bloch-Devaux -901106
C! Print decay modes and rates in PYTHIA 6.1 for requested particle code KPP
CKEY KINE KINGAL LUND7 DECAY  /  INTERNAL   USER
C
C - structure : SUBROUTINE
C               User entry name :KXP6ST
C               External references:PYCOMP/PYNAME(PYTHIA 6.1)
C               Comdecks refenced :PYT6COM
C
C - usage : CALL KXP6ST(KPP)
C------------------------------------------------------------------
#ifndef DOC
#include "pyt6com.h"
      CHARACTER CHAP*16,CHAN*16,CHAD(5)*16
      IF (MSTU(11).GT.0) THEN
          MSTJ24=MSTJ(24)
          MSTJ(24)=0
          WRITE(MSTU(11),2800)
          KF=KPP
          KC = PYCOMP(KF)
          IF(KF.EQ.0) GOTO 300
          IF(KF.LT.MSTU(1).OR.(MSTU(2).GT.0.AND.KF.GT.MSTU(2)))
     &    GOTO 300

C...Find particle name and mass. Print information.
          CALL PYNAME(KF,CHAP)
          IF(KF.LE.100.AND.CHAP.EQ.' '.AND.MDCY(KC,2).EQ.0) GOTO 300
          CALL PYNAME(-KF,CHAN)
          WRITE(MSTU(11),2900) KF,KC,CHAP,CHAN,(KCHG(KC,J1),J1=1,3),
     &    (PMAS(KC,J2),J2=1,4),MDCY(KC,1)

C...Particle decay: channel number, branching ratios, matrix element,
C...decay products.
          DO 290 IDC=MDCY(KC,2),MDCY(KC,2)+MDCY(KC,3)-1
            DO 280 J=1,5
              CALL PYNAME(KFDP(IDC,J),CHAD(J))
  280       CONTINUE
            WRITE(MSTU(11),3000) IDC,MDME(IDC,1),MDME(IDC,2),BRAT(IDC),
     &      (CHAD(J),J=1,5)
  290     CONTINUE
  300   CONTINUE
C
        MSTJ(24)=MSTJ24
      ENDIF
 2800 FORMAT(///30X,'Particle/parton data table'//8X,'KF',5X,'KC',4X,
     &'particle',8X,'antiparticle',6X,'chg  col  anti',8X,'mass',7X,
     &'width',7X,'w-cut',5X,'lifetime',1X,'decay'/11X,'IDC',1X,'on/off',
     &1X,'ME',3X,'Br.rat.',4X,'decay products')
 2900 FORMAT(/1X,I9,3X,I4,4X,A16,A16,3I5,1X,F12.5,2(1X,F11.5),
     &1X,1P,E13.5,3X,I2)
 3000 FORMAT(10X,I4,2X,I3,2X,I3,2X,F10.6,4X,5A16)
      RETURN
      END
#endif
