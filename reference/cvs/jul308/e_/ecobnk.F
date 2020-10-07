      SUBROUTINE ECOBNK(IER)
C***********************************************************************
C!Create EcObject banks                                                *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870819                                    *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ECLU,ESDA,ETP1                                         *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : ECOB,ETCK,EBOS,EHYP,EPAR                               *
C!                                                                     *
C!    Return code =IER  normal : 0                                     *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "ecnamc.h"
#include "etckjj.h"
#include "ecobjj.h"
#include "ebosjj.h"
#include "ehypjj.h"
#include "eparjj.h"
#include "bmacro.h"
      IER=0
C
C?  Get number of tracks
C
      KETP1=IW(NAETP1)
      NTRKS=LROWS(KETP1)
C
C?  Create bank etck
C
      CALL AUBOS('ETCK',0,NTRKS*LETCKA+LMHLEN,KETCK,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      IW(KETCK+LMHCOL)=LETCKA
      IW(KETCK+LMHROW)=NTRKS
      CALL BLIST(IW,'T+','ETCK')
C
C?  Get number of clusters
C
      KECLU=IW(NAECLU)
      NECLU=LROWS(KECLU)
C
C?   COCLN= Number of EcObjects per Eclu
C
      COCLN=1.
      NECOB=IFIX(FLOAT(NECLU)*COCLN)
C
C?  Create bank ecob
C
      CALL AUBOS('ECOB',0,NECOB*LECOBA+LMHLEN,KECOB,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      CALL BLIST(IW,'T+','ECOB')
      IW(KECOB+LMHCOL)=LECOBA
C
C?  Get number of hit storeys
C
      KESDA=IW(NAESDA)
      NESDA=LROWS(KESDA)
C
C?   ESEBN= Number of EBos per Esda
C
      ESEBN=1.
      NEBOS=IFIX(FLOAT(NESDA)*ESEBN)
C
C?  Create bank ebos
C
      CALL AUBOS('EBOS',0,NEBOS*LEBOSA+LMHLEN,KEBOS,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      CALL BLIST(IW,'T+','EBOS')
      IW(KEBOS+LMHCOL)=LEBOSA
C
C?   ECEHN= Number of Ehyp per Ecob
C
         ECEHN = 2.
      NEHYP=IFIX(FLOAT(NECOB)*ECEHN)
C
C?  Create bank ehyp
C
      CALL AUBOS('EHYP',0,NEHYP*LEHYPA+LMHLEN,KEHYP,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      CALL BLIST(IW,'T+','EHYP')
      IW(KEHYP+LMHCOL)=LEHYPA
C
C?   EPEHN= Number of Epar per Ehyp
C
      EPEHN=2.
      NEPAR=IFIX(FLOAT(NEHYP)*EPEHN)
C
C?  Create bank epar
C
      CALL AUBOS('EPAR',0,NEPAR*LEPARA+LMHLEN,KEPAR,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      CALL BLIST(IW,'T+','EPAR')
      IW(KEPAR+LMHCOL)=LEPARA
  999 CONTINUE
      RETURN
  998 CONTINUE
      CALL RERROR(' ECOBNK',-1,' not enaugh space')
      IER=1
      IND = NDROP('ETCK',0)
      IND = NDROP('ECOB',0)
      IND = NDROP('EHYP',0)
      IND = NDROP('EPAR',0)
      IND = NDROP('EBOS',0)
      RETURN
      END
#endif
