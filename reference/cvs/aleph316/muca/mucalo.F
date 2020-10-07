      SUBROUTINE MUCALO(ICAL,PR1,ERPR1,PR2,ERPR2,NCAL,NCAVEC,MAXL,IER)
C***********************************************************
C! Looking for muons in Calobs                             *
CKEY MUCAL MUON CALOBJ / USER                              *
C  Authors: U.Bottigli,A.Messineo,R.Tenchini  890216       *
C                                                          *
C  INPUT Banks : PCRL,PPOB,PPDS,PHCO                       *
C  OUTPUT Banks : none                                     *
C                                                          *
C  INPUT Arguments :                                       *
C                                                          *
C  ICAL = Calob pointer                                    *
C  MAXL = Dimension of NCAVEC (you must dimension          *
C           NCAVEC in your routine, DIMENSION NCAVEC(20)   *
C           should be enough)                              *
C                                                          *
C  OUTPUT Arguments :                                      *
C                                                          *
C  PR1  = Identification Probabilty as jet with mu-prompt  *
C         or mu-decay                                      *
C  ERPR1= Error on the Identification probability PR1      *
C  PR2  = Identification Probabilty as jet with mu-prompt  *
C  ERPR2= Error on the Identification probability PR2      *
C  NCAL = --> 0 no Digital Pattern for that ICAL           *
C               we cannot calculate the probabilities      *
C         --> 1 the probability is unambiguously given     *
C               for ICAL                                   *
C         --> N the probability is given for ICAL + (N-1)  *
C               other objects                              *
C               (in practice if NCAL=1 no problem,         *
C                else if NCAL.GT.1 the probability         *
C                concerns the NCAL objects as a whole      *
C  NCAVEC = Vector containing ICAL plus the (N-1)          *
C           extra objects .                                *
C  IER    = -1 --> Calob without Digital Patterns          *
C         = 0 ---> OK                                      *
C         = 1 ---> Insufficient MAXL                       *
C         = 2 ---> Error from MAKLIS                       *
C         = 3 ---> Insufficient LENMAX                     *
************************************************************
#ifndef DOC
      SAVE
#include "bcs.h"
#include "pcrljj.h"
#include "phcojj.h"
#include "ppobjj.h"
#include "ppdsjj.h"
      DIMENSION NCAVEC(*)
#include "mucapa.h"
      INTEGER ICLIS(LENVEC),IPLIS(LENVEC),ICLOB(LENVEC)
      INTEGER NVEC(LENVEC),NVE1(LENVEC),ITEMD(LENVEC)
      INTEGER NHPLT(MUFLAG)
      LOGICAL TROCAL
#include "bmacro.h"
C
C  Initializations
C
      DO 2000 LL=1,MUFLAG
       NHPLT(LL)=0
 2000 CONTINUE
      IZNDS=0
      IER  =0
      PR1  =0.
      ERPR1=0.
      PR2  =0.
      ERPR2=0.
      NCAL =0
C
C We link PCRL,PHCO,PPOB,PPDS
C
      KPCRL=NLINK('PCRL',0)
      IF(KPCRL.EQ.0) GOTO 999
      NPCRL=LROWS(KPCRL)
      KPHCO=NLINK('PHCO',0)
      IF(KPHCO.EQ.0) GOTO 999
      NPHCO=LROWS(KPHCO)
      KPPOB=NLINK('PPOB',0)
      IF(KPPOB.EQ.0) GOTO 999
      NPPOB=LROWS(KPPOB)
      KPPDS=NLINK('PPDS',0)
      IF(KPPDS.EQ.0) GOTO 999
      NPPDS=LROWS(KPPDS)
C
C
      NPLIS=0
      NCLIS=0
      NCLOB=0
C
C  And now  we fill the vectors with the lists of Hclus and Patterns
C
      CALL MAKLIS(KPCRL,JPCRPC,JPCRPP,ICAL,NPATT,NVEC,IER)
      IF(IER.NE.0) GO TO 997
      IF(NPATT.GT.LENVEC) GO TO 996
      IF(NPATT.EQ.0) THEN
         IER=-1
         GO TO 999
      ENDIF
      CALL UCOPY(NVEC(1),IPLIS(1),NPATT)
      CALL MAKLIS(KPCRL,JPCRPC,JPCRPH,ICAL,NCLUS,NVEC,IER)
      IF(IER.NE.0) GO TO 997
      IF(NCLUS.GT.LENVEC) GO TO 996
      CALL UCOPY(NVEC(1),ICLIS(1),NCLUS)
      NPLIS=NPATT
      NCLIS=NCLUS
      NCLOB=1
      ICLOB(1)=ICAL
C
C  We check if there are Pattern ambiguities among Calobs
C
      DO 40 I=1,NPATT
         IPTTR=IPLIS(I)
         CALL MAKLIS(KPCRL,JPCRPP,JPCRPC,IPTTR,NCOBS,NVEC,IER)
         IF(IER.NE.0) GO TO 997
         IF(NCOBS.GT.1) THEN
            DO 50 J=1,NCOBS
               TROCAL=.FALSE.
               DO 60 K=1,NCLOB
                  IF(ICLOB(K).EQ.NVEC(J)) TROCAL=.TRUE.
 60            CONTINUE
               IF(.NOT.TROCAL) THEN
                  IF(NCLOB.GE.LENVEC) GO TO 996
                  NCLOB=NCLOB+1
                  ICA1=NVEC(J)
                  ICLOB(NCLOB)=ICA1
                  CALL MAKLIS(KPCRL,JPCRPC,JPCRPH,ICA1,NCLUS,NVE1,IER)
                  IF(IER.NE.0) GO TO 997
                  IF(NCLUS.GT.LENVEC) GO TO 996
                  CALL UCOPY(NVE1(1),ICLIS(NCLIS+1),NCLUS)
                  NCLIS=NCLIS+NCLUS
               ENDIF
 50         CONTINUE
         ENDIF
 40   CONTINUE
      IF(NCLOB.LE.MAXL) THEN
         CALL UCOPY(ICLOB(1),NCAVEC(1),NCLOB)
         NCAL=NCLOB
      ELSE
C     Array NAVEC too short - RETURN
         NCAL=NCLOB
         IER = 1
         GO TO 999
      ENDIF
C
C Here we can start the discriminant analysis
C
C
C We define the pulse high for ICAL and flag for overlap IZNDS
C
      PHDIS=0.
      DO 110 LL=1,NCLIS
       NCL=ICLIS(LL)
       PHDIS=PHDIS+RTABL(KPHCO,NCL,JPHCER)
       IRDIS=ITABL(KPHCO,NCL,JPHCKD)
       IF(IRDIS.EQ.1.OR.IRDIS.EQ.3)IZNDS=IZNDS+1
  110 CONTINUE
C
C Fill digital-plane  pattern vector  NHPLT( 1- LASPLN) (LASPLN=23)
C and muon flag NHPLT(MUFLAG)   (MUFLAG=24)
C
       IF(IZNDS.EQ.0) THEN
        CALL MUDBE(NPLIS,IPLIS,KPPDS,NPPDS,KPPOB,NHPLT)
       ELSE
        CALL MUDSO(NPLIS,IPLIS,KPPDS,NPPDS,KPPOB,NHPLT)
       ENDIF
C
C Define  discriminant variables and compute probability
C
       CALL MDSCR(NPLIS,IPLIS,IZNDS,PHDIS,NHPLT,PRA,ERPRA,PRB,ERPRB)
       PR2=PRA
       ERPR2=ERPRA
       PR1=PRB
       ERPR1=ERPRB
 999  RETURN
C - error from MAKLIS
 997  CONTINUE
      IER=2
      RETURN
C - LENVEC too short
 996  CONTINUE
      IER=3
      RETURN
      END
#endif
