      SUBROUTINE PSTCPJ(LIST,IER)
C----------------------------------------------------------------------
C! Convert POT bank PSPO into JULIA bank STCP
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
#include "pspojj.h"
#include "stcpjj.h"
      CHARACTER LIST*(*),PLIST*4,JLIST*4
      LOGICAL FIRST
      EXTERNAL NAMIND
      DATA FIRST/.TRUE./
#include "bmacro.h"
C----------------------------------------------------------------------
      IF (FIRST) THEN
        NPSPO = NAMIND('PSPO')
        NSTCP = NAMIND('STCP')
        CALL BKFMT('STCP','I')
        FIRST = .FALSE.
      ENDIF
C
      IER = 1
      JPSPO = IW(NPSPO)
      IF (JPSPO .LE. 0)                                     GOTO 999
      NPROW = LROWS(JPSPO)
      IF (NPROW .EQ. 0)                                     GOTO 999
C
      CALL AUBOS('STCP',0,LMHLEN+NPROW*LSTCPA,JSTCP,IER)
      IF (IER .EQ. 2)                                       GOTO 999
      JLIST = 'STCP'
      JPSPO = IW(NPSPO)
      IW(JSTCP+LMHCOL) = LSTCPA
      IW(JSTCP+LMHROW) = NPROW
C
      DO 300 IPROW = 1, NPROW
        KSTCP = KROW(JSTCP,IPROW)
        KPSPO = KROW(JPSPO,IPROW)
        CALL RVCPY(LSTCPA,RW(KPSPO+1),RW(KPSPO+2),
     +      RW(KSTCP+1),RW(KSTCP+2))
  300 CONTINUE
C
      PLIST = 'PSPO'
#include "jplist.h"
C
      IF (IER .EQ. 1) IER = -1
C
  999 CONTINUE
      RETURN
      END
#endif
