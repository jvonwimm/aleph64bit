      SUBROUTINE SR2CLU(ISIDE,IORIE,IBUFF,ILA1,ILA2,N3TOT,IER)
C----------------------------------------------------------------------
C! Find clusters of 2 fired wires
C!
C!    Author:     H. Meinhard       15-Feb-1988
C!    Modified:   H. Meinhard       02-Mar-1990  (2)
C!
C!    Input:      - ISIDE     /I    current side [1,2]
C!                - IORIE     /I    current orientation [1,3]
C!                - IBUFFijk  /I    buffer of addresses of coordinates
C!                - ILAi      /I    layers to be looked at in orient.
C!                - N3TOT     /I    number of clusters found, without
C!                                  actual 2-clusters [0,*]
C!    Output:     - IER       /I    error flag (.ne. 0 on error)
C!
C!    Description
C!    ===========
C!    Loop over sectors, within sectors over fired wires of given side
C!    and orientation, look whether corresponding tubes overlap.
C!    If yes, check that cluster candidate is no subset of 3-cluster
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
#include "ssbpjj.h"
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
      KSSBP = IW(NASSBP)
C
C top of loop over sectors
      DO 340 ISECT = 1, MSECSG
C
C loop over coordinates in layers
        DO 330 IADR1 = 2, IBUFF(ILA1,ISECT,1)+1
          IWIR1 = IBUFF(ILA1,ISECT,IADR1)
C TTi is the tangent(theta) of the fired wire
          TT1 = (RTABL(KSCOO,IWIR1,JSCOTT)+
     +           RTABL(KSCOO,IWIR1,JSCOTT+1))/2.
C
          DO 320 IADR2 = 2, IBUFF(ILA2,ISECT,1)+1
            IWIR2 = IBUFF(ILA2,ISECT,IADR2)
            TT2 = (RTABL(KSCOO,IWIR2,JSCOTT)+
     +             RTABL(KSCOO,IWIR2,JSCOTT+1))/2.
C
C get minimal and maximal tangent(theta) of common region
            TTL = MAX(TT1-DTBY2,TT2-DTBY2)
            TTU = MIN(TT1+DTBY2,TT2+DTBY2)
C
C if overlay region exists, cluster of two fired wires. Check that
C 2-cluster is not a subset of an existing 3-cluster
            IF (TTL .LT. TTU) THEN
C
C loop over existing 3-clusters
              DO 300 IC = ITABL(KSSBP,ISIDE,JSSBPB), N3TOT
                IF ((IWIR1 .EQ. ITABL(KSCCP,IC,JSCCCC) .OR.
     +               IWIR1 .EQ. ITABL(KSCCP,IC,JSCCCC+1)) .AND.
     +              (IWIR2 .EQ. ITABL(KSCCP,IC,JSCCCC+1) .OR.
     +               IWIR2 .EQ. ITABL(KSCCP,IC,JSCCCC+2)))  GOTO 310
  300         CONTINUE
              IF (LFRROW(KSCLU) .LE. 0)                     GOTO 901
              IW(KNEXT(KSCLU)+JSCLSI) = ISIDE
              IW(KNEXT(KSCLU)+JSCLOR) = IORIE
              IW(KNEXT(KSCLU)+JSCLSC) = ISECT
              IW(KNEXT(KSCLU)+JSCLNW) = 2
              RW(KNEXT(KSCLU)+JSCLTL) = TTL
              RW(KNEXT(KSCLU)+JSCLTU) = TTU
              IW(KNEXT(KSCCP)+JSCCCC) = IWIR1
              IW(KNEXT(KSCCP)+JSCCCC+1) = IWIR2
              IW(KSCLU+LMHROW) = LROWS(KSCLU)+1
              IW(KSCCP+LMHROW) = LROWS(KSCCP)+1
  310         CONTINUE
            ENDIF
C
C bottom of loop over coordinates in layers
  320     CONTINUE
  330   CONTINUE
C
C bottom of loop over sectors
  340 CONTINUE
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
