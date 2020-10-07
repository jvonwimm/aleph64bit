      SUBROUTINE TLCONS(A,LIST)
C
C----------------------------------------------
C! Add on top of a list
C--
C--   ADD A ON TOP OF LIST.
C--     IF 'A' IS A LIST THEN A=0 ON EXIT.
C
C?
C!======================================================================
#ifndef DOC
C
#include "tlibuf.h"
#include "bcs.h"
C----------------------------------------------
C
        IF (LISFRE.EQ.0) RETURN
        IW(IELEME+1+LISFRE)=A
        ITEMP=IW(IELEME+LISFRE)
        IW(IELEME+LISFRE)=-LIST
        LIST=-LISFRE
        IF(ITEMP.EQ.0) THEN
          CALL TLOVFL(LISFRE)
        ELSE
          LISFRE=ITEMP
        ENDIF
        IF(A.LT.0) A=0
        RETURN
        END
        SUBROUTINE TLDELM(LIST,ISTHS)
C
C----------------------------------------------
C! Delete one element from a list
C--
C--   DELETES ONE IELEMET FROM LIST.
C--   ISTHS POINTS TO THE DESIRED IELEMET.
C--
C----------------------------------------------
C
#include "tlibuf.h"
#include "bcs.h"
C
C----------------------------------------------
C
        POINT=LIST
        CALL TLNEXT(POINT,CONT)
        IF(POINT.EQ.ISTHS) GO TO 20
        PREPT=POINT
 10     CALL TLNEXT(POINT,CONT)
        IF(POINT.EQ.ISTHS) GO TO 30
        IF(POINT.EQ.0)RETURN
        PREPT=POINT
        GO TO 10
 20     LIST=-IW(IELEME+POINT)
        IW(IELEME+POINT)=LISFRE
        LISFRE=POINT
        RETURN
 30     IW(IELEME+PREPT)=IW(IELEME+POINT)
        IW(IELEME+POINT)=LISFRE
        LISFRE=POINT
        RETURN
        END
      SUBROUTINE TLADD(LIST1,LIST2)
C
C--------------------------------------
C! Add two lists
C
C     AGGIUNGE LIST1 IN TESTA A LIST2
C     SENZA CANCELLARE LIST1
C--------------------------------------
C
      IMPLICIT INTEGER (A-Z)
C
      POINT=LIST1
    1 CALL TLNEXT(POINT,VALUE)
      IF(POINT.EQ.0) RETURN
C
      CALL TLCONS(VALUE,LIST2)
C
      GO TO 1
      END
      SUBROUTINE TLINIT
C
C----------------------------------------------
C! Initialize the list package buffer
C--
C--   FREE CELL LIST IS CREATED.
C--
C----------------------------------------------
C
#include "tlibuf.h"
#include "bcs.h"
C
C----------------------------------------------
C
C  set up IELEME workbank with initial
C  dimension LISDIM:
C
        NTLMAX=LISDIM
        IELEME=0
        IW(1)=1
        CALL WBANK(IW,IELEME,NTLMAX,*997)
C
        LISFRE=1
        LISPT=1
 10     NEXT=LISPT+2
        IF(NEXT.GT.NTLMAX) GO TO 20
        IW(IELEME+LISPT)=NEXT
        IW(IELEME+1+LISPT)=0
        LISPT=NEXT
        GO TO 10
 20     IW(IELEME+LISPT)=0
        IW(IELEME+1+LISPT)=0
        GOTO 999
C
C  warning printout if no bank space
C
 997    CONTINUE
        CALL RERROR('TLINIT',1,'No space for IELEME work bank')
C
 999    RETURN
        END
      SUBROUTINE TLOVFL(LAST)
C
C-----------------------------------------------------
C! Print list overflow warning
C! and extend IELEME work bank if possible
C-----------------------------------------------------
C
#include "tlibuf.h"
#include "bcs.h"
#include "rparac.h"
#include "rcurnt.h"
#include "rlunit.h"
C
      CHARACTER TEXT*105
C
      IXPND=LISDIM/10
      IXPND=IXPND+MOD(IXPND,2)
      LISFRE = NTLMAX + 1
      NTLMAX=NTLMAX+IXPND
      IW(1)=1
      CALL WBANK(IW,IELEME,NTLMAX,*997)
C
      WRITE(TEXT,1000)  IXPND
      CALL RERROR('TLOVFL',1,TEXT(1:105))
C
      LISPT = LISFRE
 10   NEXT=LISPT+2
      IF(NEXT.GT.NTLMAX) GO TO 20
      IW(IELEME+LISPT)=NEXT
      IW(IELEME+1+LISPT)=0
      LISPT=NEXT
      GO TO 10
 20   IW(IELEME+LISPT)=0
      IW(IELEME+1+LISPT)=0
      GOTO 999
C
C  warning print if out of bank space
C
 997  CONTINUE
      NTLMAX=NTLMAX-IXPND
      LISFRE = 0
      WRITE(TEXT,1001) IXPND
      CALL RERROR('TLOVFL',1,TEXT)
 999  CONTINUE
      RETURN
 1000 FORMAT('L I S T   O V E R F L O W : in TPC list package:',
     &       '&SIZE OF WORK BANK IELEME INCREASED BY ', I6,
     &       ' elements.')
 1001 FORMAT('L I S T   O V E R F L O W : in TPC list package:',
     &       '&NOT ENOUGH BANK SPACE FOR',I6,' MORE ELEMENTS',
     &       ' in IELEME.')
      END
      SUBROUTINE TLNEXT(POINT,CONT)
C
C----------------------------------------------
C! Return the value of the next element
C--
C--   RETURNS THE VALUE OF THE NEXT IELEMET.
C--   POINT MAY BE A LIST (FIRST IELEMET RETURNED)
C--   OR AN ABSOLUTE POINTER(NEXT IELEMET & POINTER RETURNED)
C--   POINTER=0 AT END OF LIST
C--
C----------------------------------------------
C
#include "tlibuf.h"
#include "bcs.h"
C
C----------------------------------------------
C
        IF(POINT) 10,20,30
 10     POINT=-POINT
        CONT=IW(IELEME+1+POINT)
        RETURN
 20     POINT=0
        RETURN
 30     POINT=IW(IELEME+POINT)
        CONT=IW(IELEME+1+POINT)
        RETURN
        END
        SUBROUTINE TLPOP(LIST,CONT)
C
C----------------------------------------------
C! Return value of 1st element and drop it
C--
C--   RETURNS THE VALUE OF THE FIRST IELEMET (CONT)
C--   AND DROPS IT FROM THE LIST.
C--
C----------------------------------------------
C
#include "tlibuf.h"
#include "bcs.h"
C
C----------------------------------------------
C
        IF(LIST.EQ.0)RETURN
        POINT=LIST
        CALL TLNEXT(POINT,CONT)
        CALL TLDELM(LIST,POINT)
        RETURN
        END
        SUBROUTINE TLSCRA(LIST)
C
C----------------------------------------------
C! Scratch list and collect memory
C
C----------------------------------------------
C
#include "tlibuf.h"
#include "bcs.h"
C
C----------------------------------------------
C
        IF(LIST.GE.0)RETURN
        LISNW=LIST
        LIST=0
        LISLS=0
 1      POINT=-LISNW
 10     CONT=IW(IELEME+1+POINT)
        IF(CONT.LT.0) CALL TLCONS(CONT,LISLS)
        INEXT=IW(IELEME+POINT)
        IF(INEXT.EQ.0) GO TO 20
        POINT=INEXT
        GO TO 10
 20     IW(IELEME+POINT)=LISFRE
        LISFRE=-LISNW
        IF(LISLS.EQ.0) RETURN
        CALL TLPOP(LISLS,LISNW)
        GO TO 1
        END
#endif
