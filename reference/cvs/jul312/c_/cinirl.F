      SUBROUTINE CINIRL(IER)
C
C***********************************************************************
C! Initialize TPC to Hcal relation banks                               *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870610                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETP1,HCLU                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : CCT1,CTC2,CRL3                                         *
C!                                                                     *
C                                                                      *
C  CCT1 IS FOR CLUSTERS TOWARDS TRACKS (CLTR)                          *
C  CTC2 IS FOR TRACKS TOWARDS CLUSTERS (TRCL)                          *
C  CRL3 IS FOR THE RELATIONS BETWEEN CLUSTERS AND TRACKS (RLCT)        *
C     ( Clusters are Hcal)                                             *
C                                                                      *
C!         NO INPUT                                                    *
C!          CCT1  : FROM CLUSTERS TO TRACKS (CLTR)                     *
C!                  POINTER TO FIRST RELATION FOR EACH CLUSTER         *
C!          CTC2  : FROM TRACKS TO CLUSTERS (TRCL)                     *
C!                  POINTER TO FIRST RELATION FAR EACH TRACK           *
C!          CRL3  : RELATIONS BETWEEN CLUSTERS AND TRACKS (RLCT)       *
C!                  CREATED WITH MAX SIZE : NCLU*NTRAKS RELATIONS      *
C!                                                                     *
C* *********************************************************************
#ifndef DOC
C
#include "rlunit.h"
#include "bcs.h"
#include "rparac.h"
#include "ccnamc.h"
#include "ecnamc.h"
#include "hcnamc.h"
#include "cct1jj.h"
#include "ctc2jj.h"
#include "crl3jj.h"
#include "cocrjj.h"
#include "bmacro.h"
C?
C?     GET NUMBER OF TRACKS, NUMBER OF  HC CLUSTERS AND NUMBER OF ECOB
C?
      KETP1=IW(NAETP1)
      NTRAK=LROWS(KETP1)
      KHCLU=IW(NAHCLU)
      NCLU=LROWS(KHCLU)
      KECOB=IW(NAECOB)
      NECOB=LROWS(KECOB)
C
C?     CREATE BANKS
C
      CALL AUBOS('CCT1',0,NCLU*LCCT1A+LMHLEN,KCCT1,IGARB)
      IF (IGARB.EQ.2) GOTO 30
      CALL AUBOS('CTC2',0,NTRAK*LCTC2A+LMHLEN,KCTC2,IGARB)
      IF (IGARB.EQ.2) GOTO 30
      NCRL3=NTRAK*NCLU+NECOB*NCLU+NTRAK*NECOB
      CALL AUBOS('CRL3',0,NCRL3*LCRL3A+LMHLEN,KCRL3,IGARB)
      IF (IGARB.EQ.2) GOTO 30
      CALL AUBOS('COCR',0,NECOB*LCOCRA+LMHLEN,KCOCR,IGARB)
      IF (IGARB.EQ.2) GOTO 30
C?
C? UPDATE INDICES IN CASE OF A GARBAGE COLLECTION
C?
      KCCT1=IW(NACCT1)
      KCTC2=IW(NACTC2)
      KCRL3=IW(NACRL3)
C
C?
C?     PUT BANKS IN LIST T
C?
      CALL BLIST(IW,'T+','CCT1')
      CALL BLIST(IW,'T+','CTC2')
      CALL BLIST(IW,'T+','CRL3')
      CALL BLIST(IW,'T+','COCR')
C?     FILL MINIHEADER FOR EACH BANK EXCEPT CRL3, SINCE THE NUMBER
C?     OF RELATIONS IS SET TO 0. WILL BE UPDATED IN CCLORL
C?
      IW(KCCT1+LMHCOL)=LCCT1A
      IW(KCCT1+LMHROW)=NCLU
      IW(KCTC2+LMHCOL)=LCTC2A
      IW(KCTC2+LMHROW)=NTRAK
      IW(KCRL3+LMHCOL)=LCRL3A
      IW(KCRL3+LMHROW)=0
      IW(KCOCR+LMHCOL)=LCOCRA
      IW(KCOCR+LMHROW)=NECOB
C
C?   SET ERROR FLAG
C
      IER=0
      RETURN
C
C?     ERROR CONDITION
C?
C
C  CALL EXIT WILL BE REPLACED BY STANDARD TREATMENT OF ERRORS
C   TO COME LATER
C
   30 CALL RERROR(' CINIRL',-1,' Not enaugh space for relation banks')
      INDX= NDROP('CCT1',0)
      INDX= NDROP('CTC2',0)
      INDX= NDROP('CRL3',0)
      INDX= NDROP('COCR',0)
      IER=1
      END
#endif
