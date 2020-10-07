      SUBROUTINE QVSWBK(LEN,LBL)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C!    Creates BOS work bank - internal to QVSRCH
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  LEN IS NUMBER OF WORDS NEEDED
C  *  LBL IS STRING USED AS BANK LABEL
C  *  IND SHOULD BE IN LABELLED COMMON AND ZEROED AT START OF JOB
C  Output Arguments :
C  *  IPTR IN /YVSWRK/ IS POINTER TO WORK BANK
C  *  RW(IPTR+1) IS WORK ARRAY
C     USER SHOULD CALL WDROP(IW,IPTR) WHEN DONE
C
C ------------------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      CHARACTER*(*) LBL
      COMMON /YVSWRK/ IPTR
C
C ------------------------------------------------------------------------------
C POINTER SHOULD HAVE BEEN ZEROED
      IF (IPTR .NE. 0) THEN
        WRITE (IW(6),*) ' QVSBOS: NON-ZERO IPTR=',IPTR,' LEN=',LEN
      ENDIF
C
C MAKE THE BANK
      CALL WBANK(IW,IPTR,LEN,*199)
C
C PUT IN LABEL
      IW(IPTR-3)=INTCHA(LBL)
      RETURN
C
  199 CONTINUE
C NOT ENOUGH SPACE
      WRITE (IW(6),*) 'QVSBOS: NOT ENOUGH SPACE, IPTR=',IPTR,' LEN=',LEN
      WRITE (IW(6),*) 'QVSBOS: CALLING BOSBK AND QMTERM'
C DUMP THE BOS STATUS
      CALL BOSBK(IW)
C GRACEFULLY TERMINATE THE JOB
      CALL QMTERM('QVSBOS')
      RETURN
      END
#endif
