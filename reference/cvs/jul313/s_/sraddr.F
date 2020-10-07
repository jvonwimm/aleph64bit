      SUBROUTINE SRADDR(ISIDE,IORIE,IBUFF,IER)
C----------------------------------------------------------------------
C! Fill address buffer for zone finding
C!
C!    Author:     H. Meinhard       04-Jun-1987
C!    Modified:   H. Meinhard       20-Nov-1989  (2)
C!
C!    Input:        ISIDE       /I      current detector side (1|2)
C!                  IORIE       /I      current orientation ([1,3])
C!    Output:       IBUFFijk    /I      Address buffer, see below
C!                  IER         /I      error flag (.ne. 0 on error)
C!
C!    Description
C!    ===========
C!    The addresses of all coordinates belonging to side ISIDE and
C!    orientation IORIE are filled into an address buffer IBUFFijk,sor-
C!    ted along the layer i within the orientation ([1,3]) and the sec-
C!    tor number j ([1,8]). Addresses are stored consecutively in k>1;
C!    k=1 gives the number of addresses stored for current i,j.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "sparam.h"
#include "sscpjj.h"
      INTEGER IBUFF(3,MGRSG*MSCSG,*)
#include "bmacro.h"
C----------------------------------------------------------------------
C locate raw coordinates and index, zero address buffer
      KSCOO = IW(NASCOO)
      KSSCP = IW(NASSCP)
      IF (KSCOO.EQ.0 .OR. KSSCP.EQ.0)                       GOTO 901
      CALL VZERO(IBUFF,3*MSECSG)
C
C fill buffers: loop over three layers
      DO 310 KLAYE = 1, 3
C
C loop over hits
      IND = (ISIDE-1)*MLAYSG + (IORIE-1)*MLAYSG/3 + KLAYE
      DO 300 ICOR = ITABL(KSSCP,IND,JSSCPB), ITABL(KSSCP,IND,JSSCPE)
      ISEC = ITABL(KSCOO,ICOR,JSCOSC)
C
C fill buffer
      IADR = IBUFF(KLAYE,ISEC,1)+1
      IF (IADR+1 .GT. MADSP)                                GOTO 902
      IBUFF(KLAYE,ISEC,1) = IADR
      IBUFF(KLAYE,ISEC,IADR+1) = ICOR
C
C bottom of loop over hits
  300 CONTINUE
C
C bottom of loop over layers
  310 CONTINUE
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRADDR',IER,
     +  'Maximal number of coordinate addresses exceeded')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
