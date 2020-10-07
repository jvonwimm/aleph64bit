      SUBROUTINE BCAREC
C--------------------------------------------------
C!    Event processing for BCAL
C!
C!    Author:    G.Boix, I.C.Park, M.Cattaneo  November 1997
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"
#include "bosext.h"
#include "blumjj.h"
#include "blctjj.h"
C
      INTEGER NBLUM
C
      INTEGER IRUN,IEVT
      INTEGER KBLUM, KBLCT
      INTEGER IGARB,I

      DATA NBLUM /0/
C-----------------------------------------------------------------------
C Get name index of BLUM once only (never changes)
      IF( NBLUM .EQ. 0 ) NBLUM = NAMIND('BLUM')

C Data not useful before run 44544
      CALL ABRUEV( IRUN, IEVT )
      IF( IRUN .LT. 44544 ) GOTO 999

C Access input bank
      KBLUM = IW(NBLUM)
      IF( KBLUM .EQ. 0 ) GOTO 999

C  Drop output bank, if it exists
      CALL BDROP(IW,'BLCT')
C Create output bank
      CALL AUBOS ('BLCT',0,LMHLEN+JBLCC2,KBLCT,IGARB)
      IF (IGARB.EQ.2) THEN
C No space to create bank
        GOTO 999
      ELSEIF( IGARB .EQ. 1 ) THEN
C Garbage collection, reaccess BLUM
        KBLUM = IW(NBLUM)
      ENDIF

C Fill output bank
      IW(KBLCT+LMHCOL) = JBLCC2
      IW(KBLCT+LMHROW) = 1
      DO I=JBLCNS,JBLCC2
        IW(KBLCT+LMHLEN+I) = IW(KBLUM+JBLUNS+I-1)
      ENDDO

C Put output bank on E list
      CALL BLIST(IW,'E+','BLCT')

  999 RETURN
      END
#endif
