      SUBROUTINE QUINIT
C-----------------------------------------------------------------------
      CALL QWMESS('_QUINIT_ *** MINI-Writing program *** ---------- ')
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
      PARAMETER ( MXBNK = 3000 , LQBOS = 4000000 )
      COMMON /BCS/  IW(LQBOS)
C
      WRITE ( LOUT , 1001 ) LQBOS
      CALL BNAMES (MXBNK)
      CALL BOS (IW,LQBOS)
 1001 FORMAT ('0_QUIBOS_ Init BOS with ',I8,' words working space')
      END
#endif
