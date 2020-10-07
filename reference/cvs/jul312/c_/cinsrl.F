      SUBROUTINE CINSRL(IDTPC,IHCAL,PT,IECOB)
C
C***********************************************************************
C!INSERTS A TRK-CLUS RELATION  (TPC-HCAL)                              *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870615                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : CCT1,CTC2,CRL3,COCR                                    *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!  CCT1  = POINTER TO HCLU RELATION BANK                              *
C!        1 WORD PER CLUSTER : ADDRESS OF FIRST                        *
C!          RELATION FOR THIS CLUSTER)                                 *
C!        PARALLEL TO HCLU BANK                                        *
C!                                                                     *
C!  CTC2  = POINTER TO TRACK RELATION BANK                             *
C!        1 WORD PER TRACK : ADDRESS OF FIRST RELATION FOR THIS TRACK) *
C!        PARALLEL TO TRACK BANK                                       *
C!                                                                     *
C!  COCR  = POINTER TO ECOB RELATION                                   *
C!        1 WORD PER ECOB : ADDRESS OF FIRST RELATION FOR THIS ECOB)   *
C!        PARALLEL TO ECOB BANK                                        *
C!                                                                     *
C!  CRL3  = POINTER TO RELATION BANK BETWEEN TRACKS AND CLUSTERS (RLCT)*
C!        5 WORDS PER RELATION :                                       *
C!                                                                     *
C!          WORD 1 = TRACK NUMBER                                      *
C!          WORD 2 = CLUSTER NUMBER                                    *
C!          WORD 3 = POINTER TO NEXT RELATION FOR CLUSTER (0 IF LAST)  *
C!                   TRANSVERSE MOMENTUM                               *
C!                   ECAL CLUSTER NUMBER (0 IF NONE)                   *
C!          NO POINTER TO NEXT TRACK (SEQUENTIAL ORDER IN TRACKS)      *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
#include "cct1jj.h"
#include "ctc2jj.h"
#include "crl3jj.h"
#include "cocrjj.h"
#include "ccnamc.h"
#include "bmacro.h"
C
C? GET NAME INDICES FOR BANKS
C
      KCOCR=IW(NACOCR)
      KCLTR=IW(NACCT1)
      KTRCL=IW(NACTC2)
      KCRL3=IW(NACRL3)
C
C? GET NUMBER OF EXISTING RELATIONS
C
      NREL=LROWS(KCRL3)
C
C?  If first relation for a track, set pointer
C?
      IF(ITABL(KTRCL,IDTPC,JCTCCR).EQ.0.AND.IDTPC.NE.0) THEN
        INDEX=KROW(KTRCL,IDTPC)+JCTCCR
        IW(INDEX)=NREL+1
      ENDIF
C
C
C? Special treatment for ecob and track, with no hclu
C
      IF(IHCAL.EQ.0)THEN
C
C?  CREATE THE RELATION
C?
         NREL=NREL+1
C
C  KRLXX = BEGINING OF THE CURRENT LINE IN THE RELATION BANK CRL3
C
         KRLXX=KROW(KCRL3,NREL)
         IW(KRLXX+JCRLCT)=IDTPC
         IW(KRLXX+JCRLCC)=IHCAL
C        IW(KHCCF)=NREL !!!What's going on here?  This was overwriting
C                       !!!the BOS common at random locations!  R.J.
         IW(KRLXX+JCRLNH)=0
         IW(KRLXX+JCRLCO)=IECOB
         RW(KRLXX+JCRLPT)=0.
      ELSE
C
C? Now, the normal case (there is an hclu)
C
C
C MAKE KHCCF = INDEX TO INITIAL RELATION FOR THE HC CLUSTER
C                (IN FACT THE LAST ONE)
C
      KHCCF=KROW(KCLTR,IHCAL)+JCCTCR
C
C?   Look if relation already exists
C?   ( it will be the last one since we go sequentially on tracks)
C?   if it exists, replace pt by the new pt if it is smaller
C
         IRLHF=IW(KHCCF)
         IF(IRLHF.NE.0)THEN
             KRLXX=KROW(KCRL3,IRLHF)
             IF(IW(KRLXX+JCRLCT).EQ.IDTPC.AND.IW(KRLXX+JCRLCO).EQ.IECOB)
     +        THEN
                IF(PT.LT.RW(KRLXX+JCRLPT))RW(KRLXX+JCRLPT)=PT
                GO TO 999
             ENDIF
         ENDIF
C
C?  CREATE THE RELATION
C?
         NREL=NREL+1
         IW(KHCCF)=NREL
C
C  KRLXX = BEGINING OF THE CURRENT LINE IN THE RELATION BANK CRL3
C
         KRLXX=KROW(KCRL3,NREL)
         IW(KRLXX+JCRLCT)=IDTPC
         IW(KRLXX+JCRLCC)=IHCAL
         IW(KHCCF)=NREL
         IW(KRLXX+JCRLNH)=IRLHF
         IW(KRLXX+JCRLCO)=IECOB
         RW(KRLXX+JCRLPT)=PT
      ENDIF
C
C? FINALLY, STORE THE NEW NUMBER OF RELATIONS IN BANK
C
      IW(KCRL3+LMHROW)=NREL
C
C?   UPDATE RELATION FOR ECOB
C
      IF(IECOB.NE.0)THEN
         KLEC=KROW(KCOCR,IECOB)
         IPRL=ITABL(KCOCR,IECOB,JCOCCR)
         IW(KLEC+JCOCCR)=NREL
         IW(KRLXX+JCRLNE)=IPRL
      ENDIF
  999 CONTINUE
      RETURN
      END
#endif
