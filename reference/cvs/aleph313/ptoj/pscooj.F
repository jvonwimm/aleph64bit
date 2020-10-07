      SUBROUTINE PSCOOJ(LIST,IER)
C----------------------------------------------------------------------
C! Convert POT bank PSCO into JULIA bank SCOO
C!
C!    Author:     H. Meinhard       26-May-1989
C!
C!    Input:      - LIST      /C    BOS event list
C!                                  If LIST(2:2) .eq. '-' drop POT bk
C!    Output:     - IER       /I    = 0  successful
C!                                  = 1  input bank does not exist or
C!                                       is empty
C!                                  = 2  not enough space
C!                                  = -1 ok, but garbage collection
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "pscojj.h"
#include "scoojj.h"
      CHARACTER LIST*(*),PLIST*4,JLIST*4
      LOGICAL FIRST
      EXTERNAL NAMIND
      DATA FIRST/.TRUE./
#include "bmacro.h"
C----------------------------------------------------------------------
      IF (FIRST) THEN
        NPSCO = NAMIND('PSCO')
        NSCOO = NAMIND('SCOO')
        CALL BKFMT('SCOO','2I,(3I,2F)')
        FIRST = .FALSE.
      ENDIF
C
      IER = 1
      JPSCO = IW(NPSCO)
      IF (JPSCO .LE. 0)                                     GOTO 999
      NPROW = LROWS(JPSCO)
      IF (NPROW .EQ. 0)                                     GOTO 999
C
      CALL AUBOS('SCOO',0,LMHLEN+NPROW*LSCOOA,JSCOO,IER)
      IF (IER .EQ. 2)                                       GOTO 999
      JLIST = 'SCOO'
      JPSCO = IW(NPSCO)
      IW(JSCOO+LMHCOL) = LSCOOA
      IW(JSCOO+LMHROW) = NPROW
C
      DO 300 IPROW = 1, NPROW
        KSCOO = KROW(JSCOO,IPROW)
        IW(KSCOO+JSCOSI) = ITABL(JPSCO,IPROW,JPSCSI) + 1
        IW(KSCOO+JSCOLA) = ITABL(JPSCO,IPROW,JPSCLA)
        IW(KSCOO+JSCOSC) = ITABL(JPSCO,IPROW,JPSCSC) + 1
        RW(KSCOO+JSCOTT) = REAL(ITABL(JPSCO,IPROW,JPSCTT))/100000.
        RW(KSCOO+JSCOTT+1) = REAL(ITABL(JPSCO,IPROW,JPSCTT+1))/100000.
  300 CONTINUE
C
      PLIST = 'PSCO'
#include "jplist.h"
C
      IF (IER .EQ. 1) IER = -1
C
  999 CONTINUE
      RETURN
      END
#endif
