      SUBROUTINE QUIBOS(LOUT,LINTE)
CKEY BANKS INIT /USER
C----------------------------------------------------------------------
C! initialize BOS
C called from QMINIT
C                                                   H.Albrecht 20.09.88
C                                       modified    J.Boucrot  03.05.93
C----------------------------------------------------------------------
#ifndef DOC
      PARAMETER ( MXBNK = 3000 , LQBOS = 1500000 )
      COMMON /BCS/  IW(LQBOS)
C
      WRITE ( LOUT , 1001 ) LQBOS
      CALL BNAMES (MXBNK)
      CALL BOS (IW,LQBOS)
 1001 FORMAT ('0_QUIBOS_ Init BOS with ',I8,' words working space')
      END
#endif
