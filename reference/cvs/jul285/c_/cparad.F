      SUBROUTINE CPARAD(ICHY,IEHY,IHHY,IPHY,NATUR,E,TH,FI,R1,R2,IER)
C***********************************************************************
C! Add one Calorimeter particle in bank CPAR                           *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  881216                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : CPAR                                                   *CFPASS11
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   Ichy   =  cal hypothesis number                                   *
C!   Iehy   =  ecob hypothesis number                                  *
C!   Ihhy   =  Hc hypothesis number                                    *
C!   Iphy   =  Pattern hypothesis number                               *
C!   Nature =  Hypothesis nature                                       *
C!   E      =  Partricle energy                                        *
C!   FI     =  Phi                                                     *
C!   TH     =  Theta                                                   *
C!   R1     =  R1 estimator                                            *
C!   R2     =  R2 estimator                                            *
C!   Ier    =  Return code, 0=OK                                       *
C!                                                                     *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "chprjj.h"
#include "ccnamc.h"
#include "cparjj.h"
#include "rlunit.h"
#include "bcs.h"
#include "bmacro.h"
      KCPAR=IW(NACPAR)
C
C?   check that we have at least one free row in bank CPAR;
C?     else, increase size of bank for 10 more ecobjects
C
      IF(LFRROW(KCPAR).LE.1)THEN
        NDATA=IW(KCPAR)+LCOLS(KCPAR)*10
        CALL AUBOS('CPAR',0,NDATA,KCPAR,IGARB)
        IF(IGARB.EQ.2)GO TO 998
      ENDIF
C
C?   add one line in bank cpar;
C?   fill cpar attributes
C
      NCPAR=LROWS(KCPAR)+1
      IW(KCPAR+LMHROW)=NCPAR
      KLINE=KROW(KCPAR,NCPAR)
      IW(KLINE+JCPANA)=NATUR
      RW(KLINE+JCPAEN)=E
      RW(KLINE+JCPATE)=TH
      RW(KLINE+JCPAFI)=FI
      RW(KLINE+JCPAR1)=R1
      RW(KLINE+JCPAR2)=R2
      IW(KLINE+JCPACH)=ICHY
C
C? Now, put the relations in CHPR bank
C
      KCHPR=IW(NACHPR)
C
C?   check that we have at least one free row in bank CHPR;
C?     else, increase size of bank for 10 more ecobjects
C
      IF(LFRROW(KCHPR).LE.1)THEN
        NDATA=IW(KCHPR)+LCOLS(KCHPR)*10
        CALL AUBOS('CHPR',0,NDATA,KCHPR,IGARB)
        IF(IGARB.EQ.2)GO TO 997
      ENDIF
C
C?   add one line in bank CHPR;
C?   fill  attributes
C
      NCHPR=LROWS(KCHPR)+1
      IW(KCHPR+LMHROW)=NCHPR
      KLINE=KROW(KCHPR,NCHPR)
      IW(KLINE+JCHPCP)=NCPAR
      IW(KLINE+JCHPEH)=IEHY
      IW(KLINE+JCHPPH)=IPHY
      IW(KLINE+JCHPPP)=IPHY
      IER=0
  999 CONTINUE
      RETURN
  998 CONTINUE
      CALL RERROR(
     +' CPARAD',1,' Too little space to book/enlarge bank cpar')
      IER=1
      RETURN
  997 CONTINUE
      CALL RERROR(
     +' CPARAD',2,' Too little space to book/enlarge bank chpr')
      IER=1
      RETURN
      END
#endif
