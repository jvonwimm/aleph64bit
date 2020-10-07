      SUBROUTINE SR3CLU(ISIDE,IORIE,IBUFF,IER)
C----------------------------------------------------------------------
C! find clusters of 3 fired wires
C!
C!    Author:     H. Meinhard       05-Jan-1988
C!    Modified:   H. Meinhard       02-Mar-1990  (2)
C!
C!    Input:      - ISIDE     /I    current side [1,2]
C!                - IORIE     /I    current orientation [1,3]
C!                - IBUFFijk  /I    buffer of addresses of coordinates
C!    Output:     - IER       /I    error flag (.ne. 0 on error)
C!
C!    Description
C!    ===========
C!    loop over sectors, within sectors over fired wires of given side
C!    and orientation, check sector number, look whether corresponding
C!    tubes overlap
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "sccpjj.h"
#include "sclujj.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "srecpc.h"
#include "sscpjj.h"
      INTEGER IBUFF(3,MGRSG*MSCSG,*)
#include "bmacro.h"
C----------------------------------------------------------------------
C get half width of tubes in tan(theta)
      Z = ZZERSG - ZOFFSG - FLOAT(MLAYSG-1)*ZDELSG/2.
      DTBY2 = ZONWSR/(2.*Z)
C
C link to coordinates and clusters
      KSCOO = IW(NASCOO)
      KSSCP = IW(NASSCP)
      KSCLU = IW(NASCLU)
      KSCCP = IW(NASCCP)
C
C top of loop over sectors
      DO 330 ISECT = 1, MSECSG
C
C loop over coordinates in layers
        DO 320 IADR1 = 2, IBUFF(1,ISECT,1)+1
          IWIR1 = IBUFF(1,ISECT,IADR1)
C TTi is the tangent(theta) of the fired wire
          TT1 = (RTABL(KSCOO,IWIR1,JSCOTT)+
     +           RTABL(KSCOO,IWIR1,JSCOTT+1))/2.
C
          DO 310 IADR2 = 2, IBUFF(2,ISECT,1)+1
            IWIR2 = IBUFF(2,ISECT,IADR2)
            TT2 = (RTABL(KSCOO,IWIR2,JSCOTT)+
     +             RTABL(KSCOO,IWIR2,JSCOTT+1))/2.
C
            DO 300 IADR3 = 2, IBUFF(3,ISECT,1)+1
              IWIR3 = IBUFF(3,ISECT,IADR3)
              TT3 = (RTABL(KSCOO,IWIR3,JSCOTT)+
     +               RTABL(KSCOO,IWIR3,JSCOTT+1))/2.
C
C get minimal and maximal tangent(theta) of common region
              TTL = MAX(TT1-DTBY2,TT2-DTBY2,TT3-DTBY2)
              TTU = MIN(TT1+DTBY2,TT2+DTBY2,TT3+DTBY2)
C
C if overlay region exists, cluster of three fired wires
              IF (TTL .LT. TTU) THEN
                IF (LFRROW(KSCLU) .LE. 0)                   GOTO 901
                IW(KNEXT(KSCLU)+JSCLSI) = ISIDE
                IW(KNEXT(KSCLU)+JSCLOR) = IORIE
                IW(KNEXT(KSCLU)+JSCLSC) = ISECT
                IW(KNEXT(KSCLU)+JSCLNW) = 3
                RW(KNEXT(KSCLU)+JSCLTL) = TTL
                RW(KNEXT(KSCLU)+JSCLTU) = TTU
                IW(KNEXT(KSCCP)+JSCCCC) = IWIR1
                IW(KNEXT(KSCCP)+JSCCCC+1) = IWIR2
                IW(KNEXT(KSCCP)+JSCCCC+2) = IWIR3
                IW(KSCLU+LMHROW) = LROWS(KSCLU)+1
                IW(KSCCP+LMHROW) = LROWS(KSCCP)+1
              ENDIF
C
C bottom of loop over coordinates in layers
  300       CONTINUE
  310     CONTINUE
  320   CONTINUE
C
C bottom of loop over sectors
  330 CONTINUE
C
      GOTO 999
C----------------------------------------------------------------------
C error condition: not enough free space in cluster banks
  901 IER = 1
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
