      SUBROUTINE QUINIT
C-----------------------------------------------------------------------
      CALL QWMESS('_QUINIT_ *** MINI 205 - Writing *** ---------- ')
      END
      SUBROUTINE QUEVNT (QT,KT,QV,KV)
#include "qcde.h"
C----------------------------------------------------
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
C-----------------------------------------------------------------------
 999  return 
      END
      SUBROUTINE QUTERM
      return
      END
      SUBROUTINE QUIBOS(LOUT,LINTE)
CKEY BANKS INIT /USER
C----------------------------------------------------------------------
C! initialize BOS
C called from QMINIT
C                                                   H.Albrecht 20.09.88
C                                       modified    J.Boucrot  03.05.93
C----------------------------------------------------------------------
#ifndef DOC
      PARAMETER ( MXBNK = 3000 , LQBOS = 5000000 )
      COMMON /BCS/  IW(LQBOS)
C
      WRITE ( LOUT , 1001 ) LQBOS
      CALL BNAMES (MXBNK)
      CALL BOS (IW,LQBOS)
 1001 FORMAT ('0_QUIBOS_ Init BOS with ',I8,' words working space')
      END
#endif
      INTEGER FUNCTION MINVSN(DUMMY)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Mini-DST version number (of current library).
C
C     This can be overrode by use of MVSN card which allows a new
C     version to be started without changing MINI code.
C     Useful if other code has major modifications.
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "mversn.h"
C
      PARAMETER ( MVEROK = 205 )
      LOGICAL FIRST
      SAVE MVER,FIRST
      DATA FIRST / .TRUE. /
C-----------------------------------------------------------------------
C
C++   Check for card input.
C
      IF (FIRST) THEN
         KMVSN = NLINK('MVSN',0)
         IF (KMVSN.GT.0) THEN
            MVER = IW(KMVSN+1)
      ELSE
         MVER = MINIVER
         ENDIF
         FIRST = .FALSE.
      ENDIF
C Overwrite MVSN card:
      MINVSN = MVEROK
C
      RETURN
      END
#endif
