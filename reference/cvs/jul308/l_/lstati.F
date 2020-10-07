      SUBROUTINE LSTATI
C-----------------------------------------------------------------------
C! Accumulate job statistics
C!
C!   Author   : P. H. Hansen  890101
C!
C!     Input  : 'LOBJ'           cluster bank
C!              'LPDA'           wire bank
C!     Output : 'LSTA'           Job statistics
C!
C!   Description :
C!   ===========
C?
C!======================================================================
#ifndef DOC
C
#ifndef DOC
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "lclujj.h"
#include "lidtjj.h"
#include "lobjjj.h"
#include "lpdajj.h"
#include "bmacro.h"
C---------------------------------------------------------------------
      KLSTA = IW(NALSTA)
      IF(KLSTA.LE.0)                                GOTO 999
C
C Count events with Lcal clusters
      KLOBJ = IW(NALOBJ)
      IF(KLOBJ.LE.0)                                GOTO 999
      IF(LROWS(KLOBJ).LE.0)                         GOTO 999
      ISTA = LMHLEN+1
      RW(KLSTA+ISTA) = RW(KLSTA+ISTA) + 1.
C
C Get energy per module from wire data
      KLPDA = IW(NALPDA)
      IF(KLPDA.GT.0) THEN
         DO 10 I=1,4
            ISTA = ISTA + 1
            RW(KLSTA+ISTA) = RW(KLSTA+ISTA) + RTABL(KLPDA,I,JLPDES)
  10     CONTINUE
      ENDIF
C
  999 CONTINUE
      END
#endif
#endif
