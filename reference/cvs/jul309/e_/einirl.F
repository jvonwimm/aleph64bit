      SUBROUTINE EINIRL(IER)
C
C***********************************************************************
C! INITIALIZE ETP REL BANKS                                            *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETP1,ECLU                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : ECT1,ETC2,ERL3                                         *
C!                                                                     *
C                                                                      *
C  ECT1 IS FOR CLUSTERS TOWARDS TRACKS (CLTR)                          *
C  ETC2 IS FOR TRACKS TOWARDS CLUSTERS (TRCL)                          *
C  ERL3 IS FOR THE RELATIONS BETWEEN CLUSTERS AND TRACKS (RLCT)        *
C                                                                      *
C!         NO INPUT                                                    *
C!          ECT1  : FROM CLUSTERS TO TRACKS (CLTR)                     *
C!                  POINTER TO FIRST RELATION FOR EACH CLUSTER         *
C!          ETC2  : FROM TRACKS TO CLUSTERS (TRCL)                     *
C!                  POINTER TO FIRST RELATION FAR EACH TRACK           *
C!          ERL3  : RELATIONS BETWEEN CLUSTERS AND TRACKS (RLCT)       *
C!                  CREATED WITH MAX SIZE : NCLU*NTRAKS RELATIONS      *
C!                                                                     *
C!                                                                     *
C!       IER = return code ( 0 if normal)                              *
C!                                                                     *
C* *********************************************************************
#ifndef DOC
C
#include "bcs.h"
#include "rlunit.h"
#include "rparac.h"
#include "ecnamc.h"
#include "ect1jj.h"
#include "etc2jj.h"
#include "erl3jj.h"
#include "bmacro.h"
      IER=0
C?
C?     GET NUMBER OF TRACKS AND NUMBER OF CLUSTERS
C?
      KETP1=IW(NAETP1)
      IF(KETP1.EQ.0)GO TO 40
      NTRAK=LROWS(KETP1)
      KECLU=IW(NAECLU)
      NCLU=LROWS(KECLU)
C
C?     CREATE BANKS
C
      CALL AUBOS('ECT1',0,NCLU*LECT1A+LMHLEN,KECT1,IGARB)
      IF (IGARB.EQ.2) GOTO 30
      CALL AUBOS('ETC2',0,NTRAK*LETC2A+LMHLEN,KETC2,IGARB)
      IF (IGARB.EQ.2) GOTO 30
      CALL AUBOS('ERL3',0,NTRAK*NCLU*LERL3A+LMHLEN,KERL3,IGARB)
      IF (IGARB.EQ.2) GOTO 30
C?
C? UPDATE INDICES IN CASE OF A GARBAGE COLLECTION
C?
      KECT1=IW(NAECT1)
      KETC2=IW(NAETC2)
      KERL3=IW(NAERL3)
C
C?
C?     PUT BANKS IN LIST T
C?
      CALL BLIST(IW,'T+','ECT1')
      CALL BLIST(IW,'T+','ETC2')
      CALL BLIST(IW,'T+','ERL3')
C?     FILL MINIHEADER FOR EACH BANK EXCEPT ERL3, SINCE THE NUMBER
C?     OF RELATIONS IS SET TO 0. WILL BE UPDATED IN ECLORL
C?
      IW(KECT1+LMHCOL)=LECT1A
      IW(KECT1+LMHROW)=NCLU
      IW(KETC2+LMHCOL)=LETC2A
      IW(KETC2+LMHROW)=NTRAK
      IW(KERL3+LMHCOL)=LERL3A
      IW(KERL3+LMHROW)=0
      RETURN
C
C?     ERROR CONDITION
C?
   40 CALL RERROR('EINIRL',1,' BANK ETP1 DOES NOT EXIST' )
C
   30 CALL RERROR('EINIRL',2,' exit')
      IER=1
      KECT1=NDROP('ECT1',0)
      KETC2=NDROP('ETC2',0)
      KERL3=NDROP('ERL3',0)
      RETURN
      END
#endif
