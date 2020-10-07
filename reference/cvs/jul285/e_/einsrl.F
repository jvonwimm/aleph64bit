      SUBROUTINE EINSRL(IDTPC,ICCL)
C
C***********************************************************************
C!INSERTS A TRK-CLUS RELATION                                          *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : ECT1,ETC2,ERL3                                         *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!  ECT1  = POINTER TO CLUSTERS RELATION BANK (CLTR)                   *
C!        1 WORD PER CLUSTER : ADDRESS OF FIRST                        *
C!          RELATION FOR THIS CLUSTER)                                 *
C!  ETC2  = POINTER TO TRACK RELATION BANK (TRCL)                      *
C!        1 WORD PER TRACK : ADDRESS OF FIRST RELATION FOR THIS TRACK) *
C!  ERL3  = POINTER TO RELATION BANK BETWEEN TRACKS AND CLUSTERS (RLCT)*
C!        3 WORDS PER RELATION :                                       *
C!          WORD 1 = TRACK NUMBER                                      *
C!          WORD 2 = CLUSTER NUMBER                                    *
C!          WORD 3 = POINTER TO NEXT RELATION FOR CLUSTER (0 IF LAST)  *
C!          NO POINTER TO NEXT TRACK (SEQUENTIAL ORDER IN TRACKS)      *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
#include "ect1jj.h"
#include "etc2jj.h"
#include "erl3jj.h"
#include "ecnamc.h"
#include "bmacro.h"
C
C? GET NAME INDICES FOR BANKS
C
      KCLTR=IW(NAECT1)
      KTRCL=IW(NAETC2)
      KRLCT=IW(NAERL3)
C
C? GET NUMBER OF EXISTING RELATIONS
C
      NREL=LROWS(KRLCT)
C
C?  IF FIRST CLUSTER FOR A TRACK, SET POINTER
C?
      IF(ITABL(KTRCL,IDTPC,JETCER).EQ.0) THEN
        INDEX=KROW(KTRCL,IDTPC)+JETCER
        IW(INDEX)=NREL+1
      ENDIF
C
C
C MAKE KCTCF = INDEX TO INITIAL RELATION FOR THE CLUSTER
C                (IN FACT THE LAST ONE)
C
      KCTCF=KROW(KCLTR,ICCL)+JECTER
C
C? IF FIRST TRACK FOR A CLUSTER ....
C?   CREATE THE INITIAL RELATION
C?
      IF(IW(KCTCF).EQ.0)THEN
         NREL=NREL+1
         IW(KCTCF)=NREL
C
C   KRLXX = BEGINING OF THE CURRENT LINE IN THE RELATION BANK (RLCT)
C
         KRLXX=KROW(KRLCT,NREL)
         IW(KRLXX+JERLET)=IDTPC
         IW(KRLXX+JERLEC)=ICCL
         IW(KRLXX+JERLER)=0
      ELSE
C
C?  ELSE ...
C?  CHECK THAT THE RELATION DOES NOT ALREADY EXIST
C?  IT IS SUFFICIENT TO TEST LAST RELATION FOR THE CLUSTER SINCE
C?  WE GO SEQUENTIALLY ON TRACKS
C?
C
C  IRLCF= LINE NUMBER OF INITIAL RELATION FOR THE CLUSTER
C
         IRLCF=IW(KCTCF)
         IF(ITABL(KRLCT,IRLCF,JERLET).NE.IDTPC)THEN
C
C?  IF THE RELATION DOES NOT EXIST, CREATE IT
C?
            NREL=NREL+1
            IW(KCTCF)=NREL
C
C  KRLXX SAME AS BEFORE
C
            KRLXX=KROW(KRLCT,NREL)
            IW(KRLXX+JERLET)=IDTPC
            IW(KRLXX+JERLEC)=ICCL
            IW(KRLXX+JERLER)=IRLCF
         ENDIF
      ENDIF
C
C? FINALLY, STORE THE NEW NUMBER OF RELATIONS IN BANK
C
      IW(KRLCT+LMHROW)=NREL
      RETURN
      END
#endif
