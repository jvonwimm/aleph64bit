      SUBROUTINE SRSEL2(IER)
C----------------------------------------------------------------------
C! Select double arm fitted tracks
C!
C!    Author:     H. Meinhard       20-Feb-1990
C!
C!    Input:      - banks 'SFTR','STCP'
C!    Output:     - IER       /I    error code:
C!                                  0 = no error
C!                                  1 = input banks missing
C!                - banks 'SFTR','STCP'
C!
C!    Description
C!    ===========
C!    Select double arm fitted tracks according to number of degrees
C!    of freedom and chisquare such that only one pair of fitted tracks
C!    remains
C?
C!======================================================================
#ifndef DOC
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "sftrjj.h"
#include "stcpjj.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C link to banks
      JSFTR = IW(NASFTR)
      JSTCP = IW(NASTCP)
      IF (JSFTR .EQ. 0 .OR. JSTCP .EQ. 0)                   GOTO 901
C NDFMX is the maximum number of degrees of freedom, C2MIN the minimum
C chisquare value found so far
      NDFMX = 0
      C2MIN = 1.E6
C loop over track pairs in order to find the best one
      DO 300 ITRK = 1, LROWS(JSFTR), 2
        NDF = ITABL(JSFTR,ITRK,JSFTNF)
        C2  = RTABL(JSFTR,ITRK,JSFTC2)
        IF (NDF .GT. NDFMX .OR. NDF .EQ. NDFMX .AND. C2 .LT. C2MIN) THEN
          IBEST = ITRK
          NDFMX = NDF
          C2MIN = C2
        ENDIF
  300 CONTINUE
C copy best track pair to banks, purge the other ones
      IF (IBEST .NE. 1) THEN
        KSFTR = KROW(JSFTR,1)
        DO 310 I = 1, 2*LSFTRA
          IW(KSFTR+I) = ITABL(JSFTR,IBEST,I)
  310   CONTINUE
        KSTCP = KROW(JSTCP,1)
        DO 320 I = 1, 2*LSTCPA
          IW(KSTCP+I) = ITABL(JSTCP,IBEST,I)
  320   CONTINUE
      ENDIF
C compress 'SFTR' and 'STCP' to actual size
      IW(JSFTR+LMHROW) = 2
      IW(JSTCP+LMHROW) = 2
      CALL AUBPRS('SFTRSTCP')
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
#endif
