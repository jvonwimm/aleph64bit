      SUBROUTINE KXL7ST(KF)
C------------------------------------------------------------------
C          B.Bloch-Devaux -901106
C! Print decay modes and rates in LUND7.3 for requested particle code KF
CKEY KINE KINGAL LUND7 DECAY  /  INTERNAL   USER
C
C - structure : SUBROUTINE
C               User entry name :KXL7ST
C               External references:LUCOMP/LUNAME/ULMAS(JETSET7.3)
C               Comdecks refenced :LUN7COM
C
C - usage : CALL KXL7ST(KF)
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "lun7com.h"
C
      CHARACTER CHAP*16,CHAN*16,CHAD(5)*16
      IF (MSTU(11).GT.0) THEN
         WRITE(MSTU(11),2800)
         MSTJ24=MSTJ(24)
         MSTJ(24)=0
         KC=LUCOMP(KF)
         IF(KC.EQ.0) GOTO 99
         IF(MSTU(14).EQ.0.AND.KF.GT.100.AND.KC.LE.100) GOTO 99
         IF(MSTU(14).GT.0.AND.KF.GT.100.AND.MAX(MOD(KF/1000,10),
     &    MOD(KF/100,10)).GT.MSTU(14)) GOTO 99
C...Find particle name and mass. Print information.
         CALL LUNAME(KF,CHAP)
         IF(KF.LE.100.AND.CHAP.EQ.' '.AND.MDCY(KC,2).EQ.0) GOTO 99
         CALL LUNAME(-KF,CHAN)
         PM=ULMASS(KF)
         WRITE(MSTU(11),2900) KF,KC,CHAP,CHAN,KCHG(KC,1),KCHG(KC,2),
     &   KCHG(KC,3),PM,PMAS(KC,2),PMAS(KC,3),PMAS(KC,4),MDCY(KC,1)
C...Particle decay: channel number, branching ration, matrix element,
C...decay products.
         IF(KF.GT.100.AND.KC.LE.100) GOTO 99
         DO 210 IDC=MDCY(KC,2),MDCY(KC,2)+MDCY(KC,3)-1
         DO 200 J=1,5
  200     CALL LUNAME(KFDP(IDC,J),CHAD(J))
  210     WRITE(MSTU(11),3000) IDC,MDME(IDC,1),MDME(IDC,2),BRAT(IDC),
     &    (CHAD(J),J=1,5)
         MSTJ(24)=MSTJ24
      ENDIF
C
 2800 FORMAT(///30X,'Particle/parton data table'//5X,'KF',5X,'KC',4X,
     &'particle',8X,'antiparticle',6X,'chg  col  anti',8X,'mass',7X,
     &'width',7X,'w-cut',5X,'lifetime',1X,'decay'/11X,'IDC',1X,'on/off',
     &1X,'ME',3X,'Br.rat.',4X,'decay products')
 2900 FORMAT(/1X,I6,3X,I4,4X,A16,A16,3I5,1X,F12.5,2(1X,F11.5),
     &2X,F12.5,3X,I2)
 3000 FORMAT(10X,I4,2X,I3,2X,I3,2X,F8.5,4X,5A16)
 99   RETURN
      END
#endif
