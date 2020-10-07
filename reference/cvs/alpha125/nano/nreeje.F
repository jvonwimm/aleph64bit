      SUBROUTINE NREEJE(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : create EJET bank from NDEJ bank
C!
C!   Author   :- Gerrit Graefe         15-FEB-1994
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created EJET from NDEJ
C!                                       1=can't find NDEJ
C!                                       2=no space to book EJET
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the bank EJET from the NDEJ bank.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ejetjj.h"
#include "ndejjj.h"
      INTEGER NLINK,NVERS,NAMIND
      INTEGER I,J,INDEJ,INDEJE,IGARB,IROWN,IROWP,KRNDEJ,KOEJET,IRET
      INTEGER NANDEJ/0/
#include "qmacro.h"
C----------------------------------------------------------------------
C
      IF (NANDEJ.EQ.0) THEN
         NANDEJ = NAMIND('NDEJ')
         NAEJET = NAMIND('EJET')
      ENDIF
C
C!..LINK TO BANK NDEJ
C
      IRET=3
      INDEJ=IW(NANDEJ)
      IF(INDEJ.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDEJ=INDEJ
      KCNDEJ=IW(KONDEJ+1)
      KRNDEJ=IW(KONDEJ+2)
C
C!..CREATE BANK EJET
C
      CALL AUBOS('EJET',0,LMHLEN+LEJETA*KRNDEJ,INDEJE,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### NREEJE ### No space to book EJET')
        IRET=2
        RETURN
      ENDIF
      CALL BKFMT('EJET','2I,(4F)')
      KOEJET=INDEJE
      IW(KOEJET+1)=LEJETA
      IW(KOEJET+2)=KRNDEJ
C
C!..FILL BANK EJET
C
      DO 100 I=1,KRNDEJ
        IROWN=KROW(KONDEJ,I)
        IROWP=KROW(KOEJET,I)
C!..MOMENTA
        RW(IROWP+JEJEPX)=FLOAT(IW(IROWN+JNDEPX)) / 10000000.0
        RW(IROWP+JEJEPY)=FLOAT(IW(IROWN+JNDEPY)) / 10000000.0
        RW(IROWP+JEJEPZ)=FLOAT(IW(IROWN+JNDEPZ)) / 10000000.0
        RW(IROWP+JEJEPE)=FLOAT(IW(IROWN+JNDEEJ)) / 10000000.0
  100 CONTINUE
      CALL BLIST(IW,'S+','EJET')
      IRET=0
  999 RETURN
      END
#endif
