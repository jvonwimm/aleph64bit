      SUBROUTINE CESCLU(I,J,K,IECO,NECO,MXCLU,IER)
C***********************************************************************
C! Navigate from IJK to EcObject(s)                                    *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : EBOS,ESDA                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   I,J,K   : Storey Address                                          *
C!   IECO    : Table of EcObs                                          *
C!   NECO    : Number of Ecobs                                         *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rlunit.h"
#include "bcs.h"
#include "esdajj.h"
#include "ebosjj.h"
#include "ecnamc.h"
      DIMENSION IECO(*)
      DATA FRCMI/0.5/
#include "bmacro.h"
      ICCL=0
      IENC=I*2**8+J*2**16+K*2**24
      KESDA=IW(NAESDA)
      NESDA=LROWS(KESDA)
      KEBOS=IW(NAEBOS)
      NEBOS=LROWS(KEBOS)
C
C?  loop on ebos to find ecobs which contain this esda
C
         DO 20 IBOS=1,NEBOS
            FRAC=RTABL(KEBOS,IBOS,JEBOFR)
            IF(FRAC.GT.FRCMI)THEN
               NSDA=ITABL(KEBOS,IBOS,JEBOES)
               IFL=ITABL(KESDA,NSDA,JESDFI)
               JTL=ITABL(KESDA,NSDA,JESDTJ)
               KL=ITABL(KESDA,NSDA,JESDDK)
               IENCL=IFL*2**8+JTL*2**16+KL*2**24
               IF(IENCL.EQ.IENC)THEN
                  ICOB=ITABL(KEBOS,IBOS,JEBOEC)
C
C?   check that this ecob is not yet in list
C
                  DO 30 IECLD=1,NECO
                  IF(ICOB.EQ.IECO(IECLD))GO TO 31
   30             CONTINUE
C
C?   Add ecob to list
C
                  IF(NECO.GE.MXCLU)GO TO 998
                  NECO=NECO+1
                  IECO(NECO)=ICOB
   31             CONTINUE
               ENDIF
            ENDIF
   20    CONTINUE
         GO TO 999
  999 CONTINUE
      IER=0
      RETURN
  998 CALL RERROR('CESCLU',1,' MORE THAN MXCLU CLUSTERS')
      IER=1
      RETURN
      END
#endif
