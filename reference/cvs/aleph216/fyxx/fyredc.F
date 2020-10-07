      SUBROUTINE FYREDC (FDRTK,FDRSH,CUTF)
C ------------------------------------------------------------
CKEY FYXX MCARLO KINE CARDS / USER
C - F.Ranjard - 880622
C! decode FXXX data card
C - output arguments:
C            FDRTK /L   = .T. if one of the arguments = 'DRTK'
C                       = .F. if one of the arguments = 'KPTK'
C            FDRSH /L   = .T. if one of the arguments = 'DRSH'
C                       = .F. if one of the arguments = 'KPSH'
C            CUTF  /R   = momentum cut
#ifndef DOC
      SAVE
#include "bcs.h"
      LOGICAL FDRTK,FDRSH
      CHARACTER*4 ZCHAR,CHAINT
C -------------------------------------------------------------
      JFXXX = IW(NAMIND('FXXX'))
      IF (JFXXX .EQ. 0) RETURN
C
      IF (IW(JFXXX) .GT. 0) THEN
C        FXXX output : FXXX  'flag1'   'flag2'  cut
C                      flag1 : DRTK if tracks must be dropped (def)
C                              KPTK if tracks must be kept
C                      flag2 : DRSH if showers must be dropped (def
C                              KPSH if showers must be kept
C                      flag3 : CUTF if momentum cut has to be redefined
C                              must be followed by:
C                      cut   : momemtum cut (=0.1 by def.)
C
         I=1
 301     CONTINUE
            ZCHAR = CHAINT (IW(JFXXX+I))
            IF (ZCHAR .EQ. 'DRTK') THEN
               FDRTK = .TRUE.
            ELSEIF (ZCHAR .EQ. 'KPTK') THEN
               FDRTK = .FALSE.
            ELSEIF (ZCHAR .EQ. 'DRSH') THEN
               FDRSH = .TRUE.
            ELSEIF (ZCHAR .EQ. 'KPSH') THEN
               FDRSH = .FALSE.
            ELSEIF (ZCHAR .EQ. 'CUTF') THEN
               I = I + 1
               CUTF = RW(JFXXX+I)
            ENDIF
         I = I + 1
         IF (I .LE. IW(JFXXX)) GOTO 301
      ENDIF
C
      END
#endif
