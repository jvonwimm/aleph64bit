      SUBROUTINE ESRPL(SYSTM,RW,CL,ST,LEPLAN,PLANES)
C-----------------------------------------------------------------
CKEY ECALDES STOREY PLANE LIMIT / USER
C     H.Videau      Creation 25/10/86   modification 14/03/90
C! Gives planes limiting storey
C  Gives the LEPLAN planes (8) limiting a storey (RW,CL,ST) in the
C  system 'SYSTM'. They are given in the following order:
C  stack plane interior, stack plane exterior
C  row plane inferior, row plane superior in theta
C   Input :
C           SYSTEM           character*5
C           RW,CL,ST         integer
C   Output:
C           LEPLAN           integer
C           PLANES(4,8)      real
C   Calls: EPLNOR
C   Called by USER.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Variables d'input
        CHARACTER*5 SYSTM
        INTEGER RW,CL,ST
C   Variables d'ouput
        INTEGER LEPLAN
        REAL PLANES(ECALLS,*)
C     Fonctions
        INTEGER NINT
C     Locales
        INTEGER LOCCL,ITCL,MD,SC,RG,SS,I,BARIL,IHRW
        INTEGER STINF,STSUP,CLINF,CLSUP,RWINF,RWSUP
        DATA BARIL/2/
        DATA STINF,STSUP,CLINF,CLSUP,RWINF,RWSUP/1,2,3,4,5,6/
C
        LEPLAN=6
      IHRW=EXROER(RW)
      RG = EXRGER(EROWEX(IHRW))
      SC = EXROES(RW)
        MD=FLOAT(CL-1)/FLOAT(EREGCN(RG))-FLOAT(ESCOCF(SC))*.5+1.
C  Attention a l'offset dans les bouchons
        LOCCL=FLOAT(CL)-
     &       (FLOAT(MD-1)+FLOAT(ESCOCF(SC))*.5)*FLOAT(EREGCN(RG))
C   INTRODUIRE LES BORDS DE MODULES  A VOIR
        IF(MD.EQ.13)MD=1
        ITCL=EREGFR(RG)+LOCCL-1
        SS=1
        IF(SC.NE.BARIL.AND.LOCCL.GT.EREGCN(RG)/2) SS=2
               IF(SYSTM.EQ.'ALEPH')        THEN
          DO 1 I=1,ECALLS
          PLANES(I,STINF)=-ESSCPC(2,ST,SC) *EALIPL(I,1,MD,SC)+
     &                     ESSCPC(1,ST,SC) *EALIPL(I,2,MD,SC)
          PLANES(I,STSUP)=+ESSCPS(2,ST,SC) *EALIPL(I,1,MD,SC)-
     &                     ESSCPS(1,ST,SC) *EALIPL(I,2,MD,SC)
          PLANES(I,CLINF)=-ECOLPC(2,ITCL)  *EALICL(I,1,MD,SC)+
     &                     ECOLPC(1,ITCL)  *EALICL(I,2,MD,SC)
          PLANES(I,CLSUP)=+ECOLPC(2,ITCL+1)*EALICL(I,1,MD,SC)-
     &                     ECOLPC(1,ITCL+1)*EALICL(I,2,MD,SC)
          PLANES(I,RWINF)=-EXROPC(2,RW+SC-1)*EALIRW(I,1,SS,MD,SC)+
     &                     EXROPC(1,RW+SC-1)*EALIRW(I,2,SS,MD,SC)
          PLANES(I,RWSUP)=+EXROPC(2,RW+SC)*EALIRW(I,1,SS,MD,SC)-
     &                     EXROPC(1,RW+SC)*EALIRW(I,2,SS,MD,SC)
 1      CONTINUE
                                    ELSE
          DO 2 I=1,ECALLS
          PLANES(I,STINF)=-ESSCPC(2,ST,SC) *ELOCPL(I,1)+
     &                     ESSCPC(1,ST,SC) *ELOCPL(I,2)
          PLANES(I,STSUP)=+ESSCPS(2,ST,SC) *ELOCPL(I,1)-
     &                     ESSCPS(1,ST,SC) *ELOCPL(I,2)
          PLANES(I,CLINF)=-ECOLPC(2,ITCL)  *ELOCCL(I,1)+
     &                     ECOLPC(1,ITCL)  *ELOCCL(I,2)
          PLANES(I,CLSUP)=+ECOLPC(2,ITCL+1)*ELOCCL(I,1)-
     &                     ECOLPC(1,ITCL+1)*ELOCCL(I,2)
          PLANES(I,RWINF)=-EXROPC(2,RW+SC-1)*ELOCRW(I,1,SS)+
     &                     EXROPC(1,RW+SC-1)*ELOCRW(I,2,SS)
          PLANES(I,RWSUP)=+EXROPC(2,RW+SC)*ELOCRW(I,1,SS)-
     &                     EXROPC(1,RW+SC)*ELOCRW(I,2,SS)
 2      CONTINUE
                                    END IF
C    Normaliser les plans
        DO 3 I=1,LEPLAN
        CALL EPLNOR(PLANES(1,I))
 3      CONTINUE
        END
#endif
