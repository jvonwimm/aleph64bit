      SUBROUTINE ECNCOB(ICL,IER)
C***********************************************************************
C!     Fill Neutral EcObject banks                                     *
C      IER .NE. 0 : Abend without banks filling.                       *
C   One ecobject is created for each eccluster                         *
C   Information is obtained from ENEUTW,ENPART commons.                *
C                                                                      *
C   AUTHOR   : A. BONISSENT  870819                                    *
C              J. Badier     880422                                    *
C              J. Badier     881128                                    *
C              J. Badier     891114
C                                                                      *
C   BANKS :                                                            *
C     INPUT   : ECOB,ECLU,ESDA,EBOS                                    *
C     OUTPUT  : ECOB,EHYP,EPAR                                         *
C     CREATED : NONE                                                   *
C                                                                      *
C                                                                      *
C                                                                      *
C?
C!======================================================================
#ifndef DOC
      DIMENSION IOPT(6) , CORE(6) , IFOT(19) , PHOT(20)
#include "ecnamc.h"
#include "bcs.h"
#include "eclujj.h"
#include "ecobjj.h"
#include "etckjj.h"
#include "ebosjj.h"
#include "ehypjj.h"
#include "eparjj.h"
#include "esdajj.h"
#include "bmacro.h"
      DATA IOPT / 6 * 0 /
      IER=0
C   ECOB bank.
      KECOB=IW(NAECOB)
      NECOB=LROWS(KECOB)
C
C?   Check that we have at least one free row in bank ecob;
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
C
C At present, ECOB and ECLU should be parallel. Check it !
C
      IF(NECOB.NE.ICL)GO TO 997
      IW(KECOB+LMHROW)=NECOB
      KLINE=KROW(KECOB,NECOB)
C ----- Number of charged tracks.
      IW(KLINE+JECONT)= 0
C ----- Pointer to first track.
      IW(KLINE+JECOET)= 0
C   Cluster analysis.
      DO 1 I = 1 , 19
        IFOT(I) = 0
        PHOT(I) = 0.
    1 CONTINUE
      PHOT(20) = 0.
      CALL EBNEUT(ICL,IOPT,IFOT,PHOT,CORE,JER)
C   ECLU bank.
      KECLU = IW(NAECLU)
      IF( KECLU .EQ. 0 ) GO TO 998
      RPOL = RTABL(KECLU,NECOB,JECLR4)
      IF( JER .GT. 0 ) THEN
        PHOT(1) = RTABL(KECLU,NECOB,JECLE4)
        IF( PHOT(1) .NE. 0. ) THEN
          PHOT(2) = RTABL(KECLU,NECOB,JECLE1) / PHOT(1)
          PHOT(3) = RTABL(KECLU,NECOB,JECLE2) / PHOT(1)
          PHOT(4) = PHOT(1)
        ENDIF
        PHOT(5) = RTABL(KECLU,NECOB,JECLT4)
        PHOT(6) = RTABL(KECLU,NECOB,JECLF4)
        PHOT(14) = PHOT(4)
        PHOT(15) = PHOT(5)
        PHOT(16) = PHOT(6)
        PHOT(20) = PHOT(4)
        IFOT(15) = 3
        IFOT(16) = 1
        IFOT(17) = 1
C
C? Setup particle code to : ebneut error. (see ddl of PCPA)
C
        IFOT(18) = 21
      ENDIF
C   Raw energy.
      RW(KLINE+JECOEG) = PHOT(4)
C   Stacks content.
        RW(KLINE+JECOEN)   = PHOT(1) * PHOT(2)
        RW(KLINE+JECOEN+1) = PHOT(1) * PHOT(3)
        RW(KLINE+JECOEN+2) = PHOT(1) * ( 1. - PHOT(2) - PHOT(3) )
C   Three words devoted to the cracks treatment.
      DO 10 IM1 = 0 , 2
        RW(KLINE+JECOCV+IM1) = PHOT( 9 + IM1 )
   10 CONTINUE
C ----- Region code
      IW(KLINE+JECORC) =
     +  64 * IFOT(13) + 16 * IFOT(12) + 4 * IFOT(11) + IFOT(10)
C ----- Identificators.
      RW(KLINE+JECOIT) = PHOT(12)
      RW(KLINE+JECOIT+1) = PHOT(13)
        I2 = 0
C ----- Number of hypotheses.
      IW(KLINE+JECONH)= 1
      IF ( IFOT(16) .EQ. 2 ) IW(KLINE+JECONH)= 2
C ----- Tagging flag : I1 + I2 , I1 = 1 if HCAL association needed
C                                I2 = 2 if POET needed.
      I1 = 0
      IF ( IFOT(16) .GT. 0 ) I1 = 1
      I2 = 2
      IF( PHOT(1) .LT. 2. ) I2 = 0
      IW( KLINE + JECOPF ) = I1 + I2
C
C?   Make relation with esda : loop on storeys, and fill bank ebos;
C?   One eclu for one ecob.
C
      KECLU=IW(NAECLU)
      KESDA=IW(NAESDA)
      KEBOS=IW(NAEBOS)
      ISTO=ITABL(KECLU,ICL,JECLES)
      JPRV=0
      NEBOS=IW(KEBOS+LMHROW)
C   Loop over ECLU storeys.
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
C   ---------------------------------------------------------
C?  Create hypotheses
      KEHYP=IW(NAEHYP)
      NHYP=IW(KEHYP+LMHROW)
C?   Check that we have at least one free row in bank EHYP;
C?     else, increase size of bank for 10 more hypothesis
C
      IF(LFRROW(KEHYP). LE. 1) THEN
        NDATA = IW(KEHYP) + LCOLS(KEHYP) * 10
        CALL AUBOS('EHYP',0,NDATA,KEHYP,IGARB)
        KECOB=IW(NAECOB)
        KLINE=KROW(KECOB,NECOB)
        IF(IGARB.EQ.2)GO TO 998
      ENDIF
C
      IPO = 1
   41 CONTINUE
      NHYP=NHYP+1
      KEHYP=IW(NAEHYP)
      INDEX=KROW(KEHYP,NHYP)
C ----- Hypothesis type for neutral.
      IW(INDEX+JEHYTY) =
     +  64 * IFOT(14) + 16 * IFOT(15) + 4 * IFOT(16) + IFOT(17)
      IF( JER .GT. 0 ) IW(INDEX+JEHYTY) = - JER
C ----- Corrected energy
      RW(INDEX+JEHYEN) = PHOT(4)
C   Barycenter.
        RW(INDEX+JEHYRA) = RPOL
        RW(INDEX+JEHYTH) = PHOT(5)
        RW(INDEX+JEHYPH) = PHOT(6)
C   ECOB index.
      IW(INDEX+JEHYEC) = NECOB
      IW(KEHYP+LMHROW) = NHYP
C   ----------------------------------------------------------
C
      IF( IPO .EQ. 1 ) THEN
C   First hypothesis index
        IW(KLINE+JECOEH) = NHYP
C?  Create particles.
        IPA = 0
        KEPAR = IW(NAEPAR)
        NPAR = IW(KEPAR+LMHROW)
C
C?   Check that we have at least one free row in bank EHYP;
C?     else, increase size of bank for 10 more hypothesis
C
        IF(LFRROW(KEPAR). LE. 1) THEN
          NDATA = IW(KEPAR) + LCOLS(KEPAR) * 10
          CALL AUBOS('EPAR',0,NDATA,KEPAR,IGARB)
          KEHYP=IW(NAEHYP)
          INDEX=KROW(KEHYP,NHYP)
          KECOB=IW(NAECOB)
          KLINE=KROW(KECOB,NECOB)
          IF(IGARB.EQ.2)GO TO 998
        ENDIF
C
   51   CONTINUE
        IPA = IPA + 1
        NPAR = NPAR + 1
        IPART = KROW(KEPAR,NPAR)
        IW(KEPAR+LMHROW) = NPAR
C   Particle type.
        IW(IPART+JEPATY) = IFOT( 17 + IPA )
C   Theta.
        RW(IPART+JEPATH) = PHOT( 12 + 3 * IPA )
C   Phi
        RW(IPART+JEPAPH) = PHOT( 13 + 3 * IPA )
        RW(IPART+JEPADT) = .0
        RW(IPART+JEPADP) = .0
C   Compactness
        RW(IPART+JEPAIT) = PHOT( 12 )
C   Pi0 ident.
        RW(IPART+JEPAIT+1) = PHOT( 13 )
C   Corrected energy
        RW(IPART+JEPAEN) = PHOT( 11 + 3 * IPA )
C   EHYP index
        IW(IPART+JEPAEH) = NHYP
C   Next particle
        IW(IPART+JEPAEP) = NPAR + 1
C   First particle index in EHYP
        IF (IPA .EQ. 1) IW(INDEX+JEHYEP) = NPAR
C   Other particle for this hypothesis ?
        IF( IFOT(19) .NE. 0 .AND. IPO .EQ. 1
     +                      .AND. IPA .EQ. 1 ) GO TO 51
C   No next particle
        IW(IPART+JEPAEP) = 0
      ELSE
C   Second hypothesis.
        NPAR = IW(KEPAR+LMHROW)
        NPAR = NPAR+1
        IPART = KROW(KEPAR,NPAR)
C   Particle type.
        IW(IPART+JEPATY) = 17
C   Theta.
        RW(IPART+JEPATH) = PHOT( 5 )
C   Phi
        RW(IPART+JEPAPH) = PHOT( 6 )
        RW(IPART+JEPADT) = .0
        RW(IPART+JEPADP) = .0
C   Compactness
        RW(IPART+JEPAIT) = PHOT(12)
C   Pi0 ident.
        RW(IPART+JEPAIT+1) = PHOT(13)
C   Hadron energy
        RW(IPART+JEPAEN) = PHOT(20)
C   EHYP index
        IW(IPART+JEPAEH) = NHYP
C   No next particle
        IW(IPART+JEPAEP) = 0
C   First particle index in EHYP
        IW( INDEX + JEHYEP ) = NPAR
        IW(KEPAR+LMHROW) = NPAR
      ENDIF
C   ----------------------------------------------------------
C   Autre hypothese?
      IF( IFOT(16) .EQ. 2 .AND. IPO .EQ. 1 ) THEN
        IW(INDEX+JEHYEH) = NHYP + 1
        IPO = 2
        GO TO 41
      ELSE
        IW(INDEX+JEHYEH)= 0
      ENDIF
C   ----------------------------------------------------------
   98 CONTINUE
      RETURN
  998 CONTINUE
      CALL RERROR(' ECNCOB',-1,' Not enaugh space')
      IER=1
      IND = NDROP('ECOB',0)
      IND = NDROP('EHYP',0)
      IND = NDROP('EPAR',0)
      IND = NDROP('EBOS',0)
      RETURN
  997 CONTINUE
      CALL RERROR(' ECNCOB',-2,' ECOB and ECLU not parallel')
      IER=2
      IND = NDROP('ECOB',0)
      IND = NDROP('EHYP',0)
      IND = NDROP('EPAR',0)
      IND = NDROP('EBOS',0)
      RETURN
      END
#endif
