      SUBROUTINE EDEFSP
C.----------------------------------------------------------------------
CKEY ECALDES PLANE STACK  INITIALISATION / INTERNAL
C     H.Videau      Creation 15/12/85   Modification 26/01/88
C! Defines STACKS and PLANES geometry .
C  We assume that the stack and plane structure is identical in all the
C  modules of a same subcomponent.
C  Planes and stack geometry is first defined in module_type for each
C  subcomponent,then placed in ALEPH system.
C  The reference frame for stacks and planes in a module depends on
C  the subcomponent.
C  We make use explicitly of the fact that the number of modules is
C  the same in the different subcomponents.
C   Calls: EGPTSP, EROTPL
C   Called by EXPNEC
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
      EXTERNAL EROTPL,VDOT
      REAL VDOT
C    Locales
      INTEGER I,SC,MD,ST,PL,PN,BPSP(ECALLP,ECALSC)
C  Points definissant les plans limites de stacks et plans
      REAL PTSTI(ECALLS,ECALST,ECALSC)
      REAL PTSTS(ECALLS,ECALST,ECALSC)
      REAL PTPL (ECALLS,ECALPL+1,ECALSC)
C  Plans temporaires. Variables locales.
      REAL PTEC(ECALLS),PTSC(ECALLS),PTMD(ECALLS)
C
      DO 2000 SC=1,ECALSC
      BPSP(1,SC)=ESCOS1(SC)
      BPSP(2,SC)=ESCOS2(SC)
 2000 CONTINUE
C
      CALL EGPTSP(PTSTI,PTSTS,PTPL)
C
C  Definition des coord locales (faisceau)
       DO 2012 SC=1,ECALSC
C des plans (stacks)
        DO 2011 ST=1,ECALST
         DO 2021 PN=1,ECALLP
      ESSCPC(PN,ST,SC)=VDOT(PTSTI(1,ST,SC),EBPLLF(1,BPSP(PN,SC)),ECALLS)
      ESSCPS(PN,ST,SC)=VDOT(PTSTS(1,ST,SC),EBPLLF(1,BPSP(PN,SC)),ECALLS)
 2021    CONTINUE
 2011   CONTINUE
C des plans  (plans)
        DO 2018 PL=1,ECALPL+1
          EPSCID(PL,SC)=(SC-1)*(ECALPL+1)+PL
           DO 2020 PN=1,ECALLP
      EPSCPC(PN,PL,SC)=VDOT(PTPL(1,PL,SC),EBPLLF(1,BPSP(PN,SC)),ECALLS)
 2020      CONTINUE
 2018    CONTINUE
 2012 CONTINUE

C  Passage dans le systeme general des plans de reference pour chaque
C  module:   Rotations theoriques et alignement.
C   Faire  ECALDM, ETSCDM, puis EMODDM, puis EALIAM.
      DO 2048 PN=1,ECALLP
      DO 2014 SC=1,ECALSC
        CALL EROTPL(PTEC,EBPLLF(1,BPSP(PN,SC)),ECALDM)
        CALL EROTPL(PTSC,PTEC,ETSCDM(1,1,ESCOET(SC)))
        DO 2013 MD=1,ECALMD
          CALL EROTPL(PTMD,PTSC,EMODDM(1,1,MD))
          CALL EROTPL(EALIPL(1,PN,MD,SC),PTMD,EALIAM(1,1,MD,SC))
 2013   CONTINUE
 2014 CONTINUE
 2048 CONTINUE
      END
#endif
