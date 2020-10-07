      SUBROUTINE NREFRT(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : create FRTL bank from NDTK bank
C!
C!   Author   :- Gerrit Graefe         15-FEB-1994
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created FRTL from NDTK
C!                                       1=can't find NDTK
C!                                       2=no space to book FRTL
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the bank FRTL from the NDTK bank.
C?   Comments:
C?              See NREFRF subroutine descriptions !!
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ndtkjj.h"
      INTEGER NLINK,NVERS,NAMIND
      INTEGER I,J,INDNTK,INDFRT,IGARB,IROWN,IROWF,KRNDTK,KOFRTL,IRET
      INTEGER MASK2,MASK4,MASK5
      DATA MASK2,MASK4,MASK5/3,15,31/
#include "qmacro.h"
C----------------------------------------------------------------------
C
C!..LINK TO BANK NDTK
C
      IRET=3
      INDNTK=IW(NAMIND('NDTK'))
      IF(INDNTK.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDTK=INDNTK
      KCNDTK=IW(KONDTK+1)
      KRNDTK=IW(KONDTK+2)
C
C!..CREATE BANK FRTL
C
      CALL AUBOS('FRTL',2,LMHLEN+LFRTLA*KRNDTK,INDFRT,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### NREFRT ### No space to book FRTL')
        IRET=2
        RETURN
      ENDIF
      CALL BKFMT('FRTL','2I,(8I)')
      NAFRTL=NAMIND('FRTL')
      KOFRTL=INDFRT
      IW(KOFRTL+1)=LFRTLA
      IW(KOFRTL+2)=KRNDTK
C
C!..FILL BANK FRTL
C
      DO 100 I=1,KRNDTK
C!..ROW IN FRTL BANK
         IROWF=KOFRTL+LMHLEN+LFRTLA*(I-1)
         IROWN=KONDTK+LMHLEN+LNDTKA*(I-1)
C!..VDET COORDINATES
        IW(IROWF+JFRTNV)=IAND(ISHFT(IW(IROWN+JNDTTA), -8),MASK2 )
C!..ITC  COORDINATES
        IW(IROWF+JFRTNI)=IAND(ISHFT(IW(IROWN+JNDTTQ),-23),MASK4 )
C!..TPC  COORDINATES
        IW(IROWF+JFRTNT)=IAND(ISHFT(IW(IROWN+JNDTTQ),-27),MASK5 )
  100 CONTINUE
      CALL BLIST(IW,'S+','FRTL')
      IRET=0
  999 RETURN
      END
#endif
