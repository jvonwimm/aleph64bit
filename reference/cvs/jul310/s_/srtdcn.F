      SUBROUTINE SRTDCN(ICOOR,ICRAT,ICARD,ITDCN)
C----------------------------------------------------------------------
C! Calculate electronics address of coordinate
C!
C!    Author:     H. Meinhard       20-Nov-1989
C!    Modified:   H. Meinhard       26-Aug-1991
C!                correct for miscablings etc.
C!
C!    Input:      - ICOOR     /I    # of coordinate in SCOO
C!    Output:     - ICRAT     /I    crate # [1,3,-99 for error]
C!                - ICARD     /I    card #  [1,24]
C!                - ITDCN     /I    TDC #   [1,16]
C!
C!    Common block SGEOMC is required.
C----------------------------------------------------------------------
#ifndef DOC
C----------------------------------------------------------------------
#include "bcs.h"
#include "rcurnt.h"
#include "sanamc.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "swtujj.h"
C     KGR(sector,oring_mode): no of tdc card within group of 4 cards
C     LGR(sector,oring_mode): 1 if first ored sector, 2 if second
      PARAMETER (NSEC=8,NOR=3)
      INTEGER KGR(NSEC,NOR), LGR(NSEC,NOR),ITDC(16),ICAR(24)
      DATA KGR /1,1,2,2,3,3,4,4, 1,2,1,2,3,4,3,4, 1,2,2,1,3,4,4,3/
      DATA LGR /1,2,1,2,1,2,1,2, 1,1,2,2,1,1,2,2, 1,1,2,2,1,1,2,2/
      DATA ITDC / 8,9,10,11,12,13,14,15, 0,1,2,3,4,5,6,7 /
      DATA ICAR / 12,13,14,15,16,17,18,19,20,21,22,23,
     +             0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11 /
#include "bmacro.h"
C----------------------------------------------------------------------
C
C---------- link to coordinates; if they don't exist, or specified coor-
C           dinate does not exist, return with error status
C
      JSCOO = IW(NAMIND('SCOO'))
      IF (JSCOO .EQ. 0)                                     GOTO 901
      IF (ICOOR .LT. 1 .OR. ICOOR .GT. LROWS(JSCOO))        GOTO 901
C
C
C---------- get side, layer, sector, and wire of coordinate
C
      ISIDE = ITABL(JSCOO,ICOOR,JSCOSI)
      ILAYE = ITABL(JSCOO,ICOOR,JSCOLA)
      ISECT = ITABL(JSCOO,ICOOR,JSCOSC)
      TANTH = (RTABL(JSCOO,ICOOR,JSCOTT)+RTABL(JSCOO,ICOOR,JSCOTT+1))/2.
      RWIRE = TANTH * (ZZERSG - ZOFFSG - REAL(MLAYSG-ILAYE)*ZDELSG)
      IWIRE = INT((RWIRE-RZERSG)/RDELSG) + 1
C correct for wrong cabling (see routine SRWIRE)
      JSWTU = IW(NASWTU)
      IF (JSWTU .NE. 0) THEN
        DO 300 IENT = 1, LROWS(JSWTU)
          IF (ISIDE .EQ. ITABL(JSWTU,IENT,JSWTSI) .AND.
     +        ILAYE .EQ. ITABL(JSWTU,IENT,JSWTLA) .AND.
     +        ISECT .EQ. ITABL(JSWTU,IENT,JSWTSC)) THEN
            IWIRE = MWIRSG + 1 - IWIRE
            GOTO 301
          ENDIF
  300   CONTINUE
  301   CONTINUE
      ENDIF
C
C---------- get electronics channel
C
      MODEO = MDORSG(ILAYE) - 20
      IF (MODEO .LT. 1 .OR. MODEO .GT. 3)                   GOTO 901
      IF (IWIRE .EQ. 1 .OR. IWIRE .EQ. 2) THEN
        ITDCN = IWIRE
      ELSE IF (IWIRE .EQ. 3) THEN
        ITDCN = 2 + LGR(ISECT,MODEO)
      ELSE IF (IWIRE .EQ. 4) THEN
        ITDCN = 4 + LGR(ISECT,MODEO)
      ELSE IF (IWIRE .GE. 5) THEN
        ITDCN = IWIRE + 2
      ENDIF
      ICRAT = MOD(ILAYE-1,MCRASG) + 1
      ICARD = (ISIDE-1) * MCRDSG/2 + ((ILAYE-1)/MCRASG)*MSECSG/2 +
     +  KGR(ISECT,MODEO)
C correct for wrong cabling (see routine SREALD)
      IF (.NOT.FMCRUN .AND. IRUNRC .LE. 2666) THEN
        IF (ISIDE .EQ. 2) ITDCN = ITDC(ITDCN) + 1
      ENDIF
      IF (IRUNRC .GE. 5354 .AND. IRUNRC .LE. 5488) THEN
        IF (ICRAT .EQ. 1) ICARD = ICAR(ICARD) + 1
      ENDIF
      IF (IRUNRC .GT. 10000) THEN
        IF (ICRAT .EQ. 2 .AND. ICARD .EQ. 17) ITDCN = ITDC(ITDCN) + 1
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  901 ICRAT = -99
      GOTO 999
  999 CONTINUE
      END
#endif
