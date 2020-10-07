      SUBROUTINE ECRCOB(IER)
C***********************************************************************
C!Fill EcObject banks                                                  *
C!                                                                     *
C!  temporary, one ecobject is created for each eccluster              *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870819                                    *
C!  MODIFIED : D. PALLIN     88/12/19
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "egmajj.h"
#include "eigajj.h"
#include "eibrjj.h"
#include "emipjj.h"
#include "eratio.h"
#include "rlunit.h"
#include "eanclu.h"
#include "ecnamc.h"
#include "ecobjj.h"
#include "etckjj.h"
#include "ebosjj.h"
#include "ehypjj.h"
#include "eparjj.h"
#include "eclujj.h"
#include "esdajj.h"
#include "eidtjj.h"
#include "erl3jj.h"
#include "ect1jj.h"
#include "bmacro.h"
      IER=0
      KECOB=IW(NAECOB)
      NECOB=LROWS(KECOB)
C
C?   check that we have at least one free row in bank ecob;
C?     else, increase size of bank for 10 more ecobjects
C
      IF(LFRROW(KECOB).LE.1)THEN
         NDATA=IW(KECOB)+LCOLS(KECOB)*10
         CALL AUBOS('ECOB',0,NDATA,KECOB,IGARB)
         IF(IGARB.EQ.2)GO TO 998
      ENDIF
C
C?   add one line in bank ecob;
C?   fill ecob attributes
C
      NECOB=NECOB+1
      IW(KECOB+LMHROW)=NECOB
      KLINE=KROW(KECOB,NECOB)
      RW(KLINE+JECOEG)=ECLTEA
      DO 10 I=1,3
      IM1=I-1
      RW(KLINE+JECOEN+IM1)=ECLSEA(I,1) +ECLSEA(I,2) + ECLSEA(I,3)
   10 CONTINUE
C
C?   region code
C
      IW(KLINE+JECORC)=ICREEA
C
C?  loop on tracks;
C?     for each track,
C?     if association should be kept (routine eslas) then
C?        if first track for the ecob, fill pointer
C?        modify the links in etck
C?     endif
C
      NBTR=0
      IF (ICHREA.EQ.0) GO TO 21
      KETCK=IW(NAETCK)
      KECT1=IW(NAECT1)
      KERL3=IW(NAERL3)
      IREL= ITABL(KECT1,ICLNEA,JECTER)
   20 IF(IREL.NE.0)THEN
         NTK = ITABL(KERL3,IREL,JERLET)
         CALL ESLAS(ICLNEA,NTK,IKEPT)
         IF(IKEPT.NE.0)THEN
            NTPR=IW(KLINE+JECOET)
            INDEX=KROW(KETCK,NTK)
            IW(INDEX+JETCEC)=NECOB
            IW(INDEX+JETCET)=NTPR
            IW(KLINE+JECOET)=NTK
            NBTR=NBTR+1
         ENDIF
         IREL= ITABL(KERL3,IREL,JERLER)
         GO TO 20
      ENDIF
   21 CONTINUE
      IW(KLINE+JECONT)=NBTR
C
C?   Make relation with esda : loop on storeys, and fill bank ebos;
C?    fraction is always one, since we assume that one eclu is one ecob.
C
      KECLU=IW(NAECLU)
      KESDA=IW(NAESDA)
      KEBOS=IW(NAEBOS)
      ISTO=ITABL(KECLU,ICLNEA,JECLES)
      JPRV=0
      NEBOS=IW(KEBOS+LMHROW)
   30 CONTINUE
      NEBOS=NEBOS+1
      INDEX=KROW(KEBOS,NEBOS)
      IW(INDEX+JEBOES)=ISTO
      IW(INDEX+JEBOEC)=NECOB
      RW(INDEX+JEBOFR)=1.
      IW(INDEX+JEBOEB)=JPRV
      JPRV=NEBOS
      ISTOS=ISTO
      ISTO=ITABL(KESDA,ISTO,JESDES)
      IF(ISTO.NE.0)GO TO 30
      IW(KLINE+JECOEB)=NEBOS
      IW(KEBOS+LMHROW)=NEBOS
C
C---------------------------
C?  Create hypotheses
C---------------------------
C?   EMBEDED PHOTON HYPOTHESIS
C?   LOOK IF A PHOTON WAS EXTRACTED FROM THIS CLUSTER
C? (WE EXTRACT ONLY ONE PHOTON/CLUSTER  (LOOK AT EIBR FIRST,THEN EGMA
C?    ,EIGA FINALLY)
C?   AND ADD ONE EPAR ROW IF NECESSARY
C
C
C? First, let us initialize the energies
C
      EELEC = 0.
      EPHOT = 0.
      EHAD  = 0.
C
      KEPAR=IW(NAEPAR)
      IPHOT=0
C   BREMS PHOTONS ?
      KEIBR=IW(NAEIBR)
      NEIBR=LROWS(KEIBR)
      IF(NEIBR.EQ.0)GOTO 61
      DO 60 KK=1,NEIBR
      KLEIB=KROW(KEIBR,KK)
      KCLU=IW(KLEIB+JEIBPE)
      IF(KCLU.EQ.NECOB)THEN
         IPHOT=1
         NEPAR=LROWS(KEPAR)
         NEPAR=NEPAR+1
         IW(KEPAR+LMHROW)=NEPAR
         KLEPA=KROW(KEPAR,NEPAR)
C       TYPE:PHOTON BREMS
         IW(KLEPA+JEPATY)=4
C       THETA
         RW(KLEPA+JEPATH)=RW(KLEIB+JEIBTE)
C       PHI
         RW(KLEPA+JEPAPH)=RW(KLEIB+JEIBPH)
C       ENERGY
         RW(KLEPA+JEPAEN)=RW(KLEIB+JEIBEN)
C       EPAR INDEX
         IW(KLEPA+JEPAEP)=0
C       WE ACCEPT ONLY ONE PHOTON PER CLUSTER
         GOTO 90
      ENDIF
 60   CONTINUE
 61   CONTINUE
C  PHOTON FROM MARSEILLE ?
      KEGMA=IW(NAEGMA)
      NEGMA=LROWS(KEGMA)
      IF(NEGMA.EQ.0)GOTO 71
      DO 70 KK=1,NEGMA
      KLEGM=KROW(KEGMA,KK)
      KCLU=IW(KLEGM+JEGMPE)
      IF(KCLU.EQ.NECOB)THEN
         IPHOT=1
         NEPAR=LROWS(KEPAR)
         NEPAR=NEPAR+1
         IW(KEPAR+LMHROW)=NEPAR
         KLEPA=KROW(KEPAR,NEPAR)
C       TYPE:PHOTON FOR CHARGED CLUSTER
         IW(KLEPA+JEPATY)=5
C       THETA
             RW(KLEPA+JEPATH)= RW(KLEGM+JEGMTE)
C       PHI
             RW(KLEPA+JEPAPH)= RW(KLEGM+JEGMPH)
C       ENERGY
             RW(KLEPA+JEPAEN)= RW(KLEGM+JEGMEN)
C       EPAR INDEX
         IW(KLEPA+JEPAEP)=0
C       WE ACCEPT ONLY ONE PHOTON PER CLUSTER
         GOTO 90
      ENDIF
 70   CONTINUE
 71   CONTINUE
C PHOTON EXTRACTED USING EIDT ESTIMATORS
      KEIGA=IW(NAEIGA)
      NEIGA=LROWS(KEIGA)
      IF(NEIGA.EQ.0)GOTO 81
      DO 80 KK=1,NEIGA
      KLEIG=KROW(KEIGA,KK)
      KCLU=IW(KLEIG+JEIBPE)
      IF(KCLU.EQ.NECOB)THEN
         IPHOT=1
         NEPAR=LROWS(KEPAR)
         NEPAR=NEPAR+1
         IW(KEPAR+LMHROW)=NEPAR
         KLEPA=KROW(KEPAR,NEPAR)
C       TYPE:PHOTON BREMS
         IW(KLEPA+JEPATY)=4
C       THETA
         RW(KLEPA+JEPATH)=RW(KLEIG+JEIBTE)
C       PHI
         RW(KLEPA+JEPAPH)=RW(KLEIG+JEIBPH)
C       ENERGY
         RW(KLEPA+JEPAEN)=RW(KLEIG+JEIBEN)
C       EPAR INDEX
         IW(KLEPA+JEPAEP)=0
C       WE ACCEPT ONLY ONE PHOTON PER CLUSTER
         GOTO 90
      ENDIF
 80   CONTINUE
 81   CONTINUE
 90   CONTINUE
C
C?   MIP HYPOTHESIS
C
      NAEMIP=NAMIND('EMIP')
      KEMIP=IW(NAEMIP)
      NEMIP=LROWS(KEMIP)
C LOOK IF A MIP IS ASSOCIATED TO THIS CLUSTER
      KERL3=IW(NAERL3)
      KECT1=IW(NAECT1)
      NREL=ITABL(KECT1,ICLNEA,JECTER)
      ITK1=ITABL(KERL3,NREL,JERLET)
      IFLMP=0
      DO  50 ITI=1,NEMIP
      NTMIP=IW(KROW(KEMIP,ITI)+JEMIFR)
      IF(ITK1.EQ.NTMIP)THEN
         IFLMP=1
         GOTO 51
      ENDIF
 50   CONTINUE
C
C
C? ELECTRON HYPOTHESIS
C
C
C!!!!!! WE CONSIDER THAT FOR ONE ECLU ROW ,THERE IS ONE AND ONLY ONE
C!!!!!! ECOB ROW, AND WITH THE SAME ROW INDEX
C
C? WHAT IS THE NATURE OF TRACKS ASSOCIATED TO CLUSTER NUMBER ICL
C
      NBE=0
      NBH=0
      EELEC=0.
      KEIDT=IW(NAEIDT)
      NEID=IW(KEIDT+LMHROW)
C?  ASSOCIATED TRACK SEARCH
      DO 1 KTR=1,NEID
      ICLU=IW(KROW(KEIDT,KTR)+JEIDPE)
      IF(ICLU.EQ.NECOB)THEN
C NATURE
         NATU=IW(KROW(KEIDT,KTR)+JEIDIP)
         IF(NATU.EQ.0)THEN
                NBH=NBH+1
         ELSE
                NBE=NBE+1
                EELEC=EELEC+RW(KROW(KEIDT,KTR)+JEIDEC)
         ENDIF
      ENDIF
 1    CONTINUE
C
C? WE BUILD AN  HYPOTHESIS ON THE CLUSTER
C
      IF((NBE+NBH).EQ.1)THEN
C
C>> ONLY 1 TRACK
C
C>      NON ELECTRON (H)
         IF(NBH.EQ.1)IHYP=18
C>      ELECTRON (E,E G)
         IF(NBE.EQ.1)THEN
            IF(IPHOT.EQ.1)THEN
C              ELECTRON GAMMA  (E G)
                IHYP=11
            ELSE
C              ELECTRON (E)
                IHYP=10
            ENDIF
         ENDIF
C
C>>
C
      ELSE
C
C>> MORE THAN 1 TRACK
C
C>      NO ELECTRON
         IF(NBE.EQ.0)IHYP=17
C>      1 ELECTRON
         IF(NBE.EQ.1)THEN
C          E+H
            IF(NBH.EQ.1)IHYP= 13
C          E+NH
            IF(NBH.GT.1)IHYP= 15
         ENDIF
C>      >1 ELECTRON
         IF(NBE.GT.1)THEN
C          NE
            IF(NBH.EQ.0)IHYP=12
C          NE+H
            IF(NBH.EQ.1)IHYP=14
C          NE+NH
            IF(NBH.GT.1)IHYP=16
        ENDIF
C
C>>
C
      ENDIF
C
C? FILL ECOB ET EHYP
C
 51   KEHYP=IW(NAEHYP)
      NHYP=IW(KEHYP+LMHROW)
      NHYP=NHYP+1
C fill ecob index nber of hypoth for this ecob
      NOHYP=IW(KLINE+JECONH)
      NOHYP=NOHYP+1
      IW(KLINE+JECONH)=NOHYP
C fill ecob index first hyp
      IW(KLINE+JECOEH)=NHYP
C fill ecob flag for hcal association
      IF(IHYP.GT.12)THEN
        IW(KLINE+JECOPF)=1
      ENDIF
C
      INDEX=KROW(KEHYP,NHYP)
C fill ehyp type
      IF(IFLMP.EQ.1)THEN
         IW(INDEX+JEHYTY)=1
      ELSE
        IW(INDEX+JEHYTY)=IHYP
      ENDIF
C fill ehyp index ecob
      IW(INDEX+JEHYEC)=NECOB

C fill ehyp header total nber of hypot
      IW(KEHYP+LMHROW)=NHYP
C FILL EPAR :INDEX EHYP
C FILL EHYP :INDEX EPAR
      IF(IPHOT.EQ.1)THEN
         NEPAR=LROWS(KEPAR)
         KLEPA=KROW(KEPAR,NEPAR)
         IW(KLEPA+JEPAEH)=NHYP
                IW(INDEX+JEHYEP)= NEPAR
         EPHOT=RW(KLEPA+JEPAEN)
      ENDIF
C
C
C
C fill ehyp corrected energy
C E electrons+E par +(Ecluster-Epar-Eelectrons)*coeff
C
      IF(IFLMP.EQ.0)THEN
         EHAD=(RW(KLINE+JECOEG)-EPHOT-EELEC)*RAPEPI
         ELSE
         EHAD=RW(KLINE+JECOEG)*RAPEMU
      ENDIF
      RW(INDEX+JEHYEN)=EELEC+EPHOT+EHAD
      RW(INDEX+JEHYRA)=RTABL(KECLU,ICLNEA,JECLR4)
      RW(INDEX+JEHYTH)=RTABL(KECLU,ICLNEA,JECLT4)
      RW(INDEX+JEHYPH)=RTABL(KECLU,ICLNEA,JECLF4)
      RETURN
  998 CONTINUE
      CALL RERROR(' ECRCOB',1,' exit')
      IER=1
      IND = NDROP('ETCK',0)
      IND = NDROP('ECOB',0)
      IND = NDROP('EHYP',0)
      IND = NDROP('EPAR',0)
      IND = NDROP('EBOS',0)
      RETURN
      END
#endif
