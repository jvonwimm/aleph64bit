      SUBROUTINE IDIGPR
C-----------------------------------------------------------------------
C! Print IDIG bank.
C!
C!    Author     :- J. Sedgbeer   89/04/11
C!    Modified   :-
C!
C!    Input:
C!      commons:     /BCS/  for bank IDIG
C!                   /RCURNT/
C!
C!    calls:      none
C!
C!    Libraries required: BOS
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
C-----------------------------------------------------------------------
C I/O commons and parameters
#include "rcurnt.h"
#include "bcs.h"
#include "idigjp.h"
      EXTERNAL NAMIND
C-----------------------------------------------------------------------
C Check that IDIG bank exists.
C
      KIDIG = IW(NAMIND('IDIG'))
      IF (KIDIG.EQ.0) THEN
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) CALL RERROR('IDIGPR',1,
     +  'Not printing IDIG bank as it does not exist.')
        GOTO 999
      ENDIF
C
C Find number bank number and number of digis.
C
      NRDI = IW(KIDIG-2)
      NDIG = IW(KIDIG+LMHROW)
C
      WRITE(LDEBRL,1000) IRUNRC,IEVTRC,NRDI,NDIG
 1000 FORMAT(1H0,'IDIGPR: Run/Event',2I8,' IDIG bank number',I4,
     +  ' No. of digis.',I6/
     +  /2(' Digit',1X,'Wire',2X,'TDC',1X,'Z-TDC    '))
C
C Loop over digis.
C
      DO 5 ID=1,NDIG,2
        IWIRE = IBITS(IW(KIDIG+LMHLEN+ID),IBITWN,NBITWN)
        ITDC  = IBITS(IW(KIDIG+LMHLEN+ID),IBITRP,NBITRP)
        IZDIG = IBITS(IW(KIDIG+LMHLEN+ID),IBITZT,NBITZT)*2
        IF(ID.EQ.NDIG) THEN
          WRITE(LDEBRL,1001) ID,IWIRE,ITDC,IZDIG
 1001     FORMAT(1X,I4,I5,2I6)
        ELSE
          I2    = ID + 1
          IWIR2 = IBITS(IW(KIDIG+LMHLEN+I2),IBITWN,NBITWN)
          ITD2  = IBITS(IW(KIDIG+LMHLEN+I2),IBITRP,NBITRP)
          IZDI2 = IBITS(IW(KIDIG+LMHLEN+I2),IBITZT,NBITZT)*2
          WRITE(LDEBRL,1002) ID,IWIRE,ITDC,IZDIG,I2,IWIR2,ITD2,IZDI2
 1002     FORMAT(1X,I4,I5,2I6,I9,I5,2I6)
        ENDIF
    5 CONTINUE
C
  999 CONTINUE
      END
#endif
