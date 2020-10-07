      SUBROUTINE HSTOFI(KHSTO)
C----------------------------------------------------
C
C! fill HSTO bank
C
C      G.Capon               date : 861001
C!          bank HSTO contains coordinates of storey corners
C!          input/output : KHSTO pointer to bank HSTO
C!          called by HPRANA
C
C?
C!======================================================================
#ifndef DOC
      CHARACTER SYSTM
#include "bcs.h"
#include "hstadd.h"
#include "hsdajj.h"
#include "hstojj.h"
      PARAMETER (LHPOI=8)
      REAL CORNR(3,11)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
         KTWO=NLINK('HTWO',0)
         FIRST=.FALSE.
      ENDIF
C
C?              compute coordinates of storey corners
C
      CALL VZERO(CORNR,33)
      SYSTM='ALEPH'
      JHTDI=NLINK('HTDI',0)
      IF(JHTDI.NE.0) THEN
         NSTKS=IW(JHTDI+1)-1
      ELSE
C
C We assume that a POT has a single layer of tower, since there is
C no way to know that from the present POT ...
C
C .... if you use a Galeph version less than 231 you should
C put the datacard HTWO in your Julia Cards
C
         IF(KTWO.EQ.0) THEN
            NSTKS=1
            IF(ISTKHS.EQ.2) THEN
               CALL REPORT('HPREDA','POT: SECOND STACK STOREY FOUND',1)
            ENDIF
         ELSE
            NSTKS=2
         ENDIF
      ENDIF
      IF(NSTKS.EQ.2)CALL HSRCR(SYSTM,ITETHS,IPHIHS,ISTKHS,LHPOI,CORNR)
      IF(NSTKS.EQ.1)CALL HTWCR(SYSTM,ITETHS,IPHIHS,LHPOI,CORNR)
C
C?              compute coordinates of center of inner face,outer face,
C
      DO 11 K=1,3
         DO 12 N=1,4
            CORNR(K,9)=CORNR(K,9)+CORNR(K,N)/4.
            CORNR(K,10)=CORNR(K,10)+CORNR(K,N+4)/4.
   12    CONTINUE
         CORNR(K,11)=(CORNR(K,9)+CORNR(K,10))/2.
   11 CONTINUE
C
C?              fill HSTO bank
C
      I=0
      DO 20 K=1,3
         DO 20 N=1,11
            I=I+1
            RW(KHSTO+I)=CORNR(K,N)
   20 CONTINUE
C
      IW(KHSTO+JHSTSN)=ISUBHS
      IW(KHSTO+JHSTMN)=IMODHS
      IW(KHSTO+JHSTOF)=IOVRHS
      KHSTO=KHSTO+LHSTOA
C
      RETURN
      END
#endif
