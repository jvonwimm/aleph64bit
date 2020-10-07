      SUBROUTINE CINPAR(IER)
C***********************************************************************
C! Create cpar bank                                                    *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  881216                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : CALO                                                   *
C!    CREATED : CPAR                                                   *
C!                                                                     *
C!   Ier  = Return code (0=OK)                                         *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "chprjj.h"
#include "cparjj.h"
#include "ccnamc.h"
#include "bcs.h"
#include "rlunit.h"
#include "bmacro.h"
C? Get number of Calobjects
      KCALO=IW(NACALO)
      NCALO=LROWS(KCALO)
C
C Npcalo = average max. number of particles per calobject
C
      NPCAL=2
      CALL AUBOS('CPAR',0,NPCAL*NCALO*LCPARA+LMHLEN,KCPAR,IGARB)
      IF(IGARB.EQ.2)GO TO 998
C
C?  Fill miniheader
C
      IW(KCPAR+LMHROW)=0
      IW(KCPAR+LMHCOL)=LCPARA
C
C? Put bank in list
C
      CALL BLIST(IW,'T+','CPAR')
C
C NRCAL = average max. number of relations per calobject
C
      NRCAL=2
      CALL AUBOS('CHPR',0,NRCAL*NCALO*LCHPRA+LMHLEN,KCHPR,IGARB)
      IF(IGARB.EQ.2)GO TO 997
C
C?  Fill miniheader
C
      IW(KCHPR+LMHROW)=0
      IW(KCHPR+LMHCOL)=LCHPRA
C
C? Put bank in list
C
      CALL BLIST(IW,'T+','CHPR')
      IER=0
  999 CONTINUE
      RETURN
  998 CALL RERROR(' CINPAR',1,' Too little space to book bank CPAR')
      IER=1
      RETURN
  997 CALL RERROR(' CINPAR',2,' Too little space to book bank CHPR')
      IER=1
      RETURN
      END
#endif
