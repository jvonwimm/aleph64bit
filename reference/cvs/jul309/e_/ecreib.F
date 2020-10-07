      SUBROUTINE ECREIB(IER)
C----------------------------------------------------------------------
C!  - CREATE EIDT BANK
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ecnamc.h"
#include "eidtjj.h"
#include "rlunit.h"
#include "bmacro.h"
C
C  WE RESERVE ONE ROW FOR EACH TRACK
C GET NBER  OF TRACKS
      IER=0
      NAFRFT=NAMIND('FRFT')
      KFRFT=IW(NAFRFT)
      NTRKS=LROWS(KFRFT)
C CREATE BANK
      CALL AUBOS('EIDT',0,NTRKS*LEIDTA+LMHLEN,KEIDT,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      CALL BLIST(IW,'T+','EIDT')
      IW(KEIDT+LMHCOL)=LEIDTA
      IW(KEIDT+LMHROW)=0
      LEIDT=KEIDT+LMHLEN
      DO 88 NTK=1,NTRKS
      IW(LEIDT+LEIDTA*(NTK-1)+JEIDIF)=0
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDR1)=1000.
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDR2)=1000.
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDR3)=1000.
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDR4)=1000.
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDR5)=1000.
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDR6)=1000.
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDR7)=1000.
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDEC)=1000.
      IW(LEIDT+LEIDTA*(NTK-1)+JEIDIP)=0
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDE1)=1000.
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDE2)=1000.
      RW(LEIDT+LEIDTA*(NTK-1)+JEIDE3)=1000.
      IW(LEIDT+LEIDTA*(NTK-1)+JEIDFR)=0
      IW(LEIDT+LEIDTA*(NTK-1)+JEIDPE)=0
 88   CONTINUE
      RETURN
  998 CONTINUE
      CALL RERROR('ECREIB',1,' exit')
      IER=1
      IND = NDROP('EIDT',0)
      RETURN
      END
#endif
