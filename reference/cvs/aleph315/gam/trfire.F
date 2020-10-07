      SUBROUTINE TRFIRE (JULTRA,NMXSTO,NSTOFI,STOFIR,WARNING)
C-----------------------------------------------------------------
C     M.Verderi                                            2-10-94
C!    Finds the storeys crossed by the track extrapolation
C     Input : JULTRA = julia track number         (Integer)
C             NMXSTO = maximum number of storeys  (Integer)
C     Output: NSTOFI = number of storeys fired    (Integer)
C             STOFIR (01/02/03,i) = itheta,jphi,kstack of
C                      storeys fired              (integer(3,NMXSTO))
C             WARNING= -10 jultra not in PFRF
C                       1  track doesn't reach the ECAL, nstofi = 0
C                       3   "      "     stack 3
C                       10 too many storeys found (>nmxsto)
C-----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      INTEGER   JULTRA,NMXSTO,NSTOFI,STOFIR(3,*),WARNING
      INTEGER   JPFRF,NAMIND,IPTP,CODE,SC,MD
      INTEGER   EFNDSC,EFNDMD
      INTEGER   NSTEP,I
      PARAMETER(NSTEP=10)
      REAL      PLL(4),RSTACK1,ZSTACK1,RSTACK3,ZSTACK3
      REAL      PLAN1(4),PLAN3(4),IMPDIR(6),IMPACT1(3),IMPACT3(3)
      REAL      VECTEUR(3),D0,D,X,Y,Z,VDOT,PLANVECT,DX,DY,DZ,POINT(3)
      INTEGER   JPHI,ITHETA,KSTACK,PL,JPOLD,ITOLD,KSOLD
      CHARACTER*16 WRONG
      EXTERNAL  NAMIND,EFNDSC,EFNDMD,VDOT
      LOGICAL   FIRST
      DATA      FIRST /.TRUE./
      DATA      RSTACK1 /194./

C Rayons et z des cylindres passant par milieu de stack 1 et 3:
      IF (FIRST) THEN
         FIRST = .FALSE.
         CALL EPLMST('ALEPH',1,1,1,PLL)
         ZSTACK1 = ABS( PLL(4) )
C risque aux endroits ou le cylindre intersecte le plan d'entree.
C        call eplmst('ALEPH',2,1,1,pll)
C        rstack1 = abs( pll(4) )
         CALL EPLMST('ALEPH',1,1,3,PLL)
         ZSTACK3 = ABS( PLL(4) )
         CALL EPLMST('ALEPH',2,1,3,PLL)
         RSTACK3 = ABS( PLL(4) )
      ENDIF
      NSTOFI  = 0
      WARNING = 0
      JPFRF = IW( NAMIND('PFRF') )
      IF (JPFRF.LE.0) THEN
         WARNING = -10
         GOTO 999
      ENDIF
C Premiere evaluation des points d'entree et de sortie de la trace:
      IPTP = JPFRF + 2 + (JULTRA-1) * IW(JPFRF+1) + 1
      CALL EXPFRF(RSTACK1,ZSTACK1,RW(IPTP),IMPDIR,CODE)
C Si trace n'atteint pas stack1 on sort:
      IF (CODE.EQ.0) THEN
         WARNING = 1
         GOTO 999
      ENDIF
      IMPACT1(1) = IMPDIR(1)
      IMPACT1(2) = IMPDIR(2)
      IMPACT1(3) = IMPDIR(3)
      CALL VUNIT (IMPDIR(4),VECTEUR,3)
C Si trace n'atteint pas stack3 : warning
      CALL EXPFRF(RSTACK3,ZSTACK3,RW(IPTP),IMPDIR,CODE)
      IF (CODE.EQ.0) WARNING = 3
      CALL ECYLND(RSTACK3,ZSTACK3,IMPACT1,VECTEUR,IMPACT3)
C Les deux impacts grossiers servent a trouver les equations des plans
C d'entree et de sortie de la trace:
      SC = EFNDSC(IMPACT1)
      MD = EFNDMD(SC,IMPACT1)
      CALL EPLST('ALEPH',SC,MD,1.001,PLAN1)
      SC = EFNDSC(IMPACT3)
      MD = EFNDMD(SC,IMPACT3)
      CALL EPLST('ALEPH',SC,MD,3.999,PLAN3)

C A l'aide de ces deux plans et de l'extrapolation de la trace,recalcul
C des points d'entree et de sorties sur ces plans:
C ( Henri decrit plan : ax+by+cz+d=0)
C Recalcul de impact3:
      D0 =-VDOT(PLAN3(1),IMPACT1,3)
      D  = PLAN3(4)
      PLANVECT = VDOT(VECTEUR,PLAN3(1),3)
      IF (PLANVECT.LT.1.E-9) GOTO 999
      X = (D0-D)*VECTEUR(1)/PLANVECT
      Y = (D0-D)*VECTEUR(2)/PLANVECT
      Z = (D0-D)*VECTEUR(3)/PLANVECT
      IMPACT3(1) = X + IMPACT1(1)
      IMPACT3(2) = Y + IMPACT1(2)
      IMPACT3(3) = Z + IMPACT1(3)
C Recalcul de impact1:
      D0 =-VDOT(PLAN1(1),IMPACT1,3)
      D  = PLAN1(4)
      PLANVECT = VDOT(VECTEUR,PLAN1(1),3)
      IF (PLANVECT.LT.1.E-9) GOTO 999
      X = (D0-D)*VECTEUR(1)/PLANVECT
      Y = (D0-D)*VECTEUR(2)/PLANVECT
      Z = (D0-D)*VECTEUR(3)/PLANVECT
      IMPACT1(1) = X + IMPACT1(1)
      IMPACT1(2) = Y + IMPACT1(2)
      IMPACT1(3) = Z + IMPACT1(3)
C Boucle de l'entree a la fin pour memoriser storeys sur ligne de tir:
      DX = (IMPACT3(1) - IMPACT1(1))/FLOAT(NSTEP)
      DY = (IMPACT3(2) - IMPACT1(2))/FLOAT(NSTEP)
      DZ = (IMPACT3(3) - IMPACT1(3))/FLOAT(NSTEP)
      POINT(1) = IMPACT1(1)
      POINT(2) = IMPACT1(2)
      POINT(3) = IMPACT1(3)
      JPOLD = 0
      ITOLD = 0
      KSOLD = 0
      DO I = 1,NSTEP
         CALL EFNDTW(POINT,'ALEPH',JPHI,ITHETA,KSTACK,SC,MD,PL,WRONG)
         IF (WRONG.EQ.'CORRECT') THEN
            IF (JPHI.NE.JPOLD.OR.ITHETA.NE.ITOLD
     &                       .OR.KSTACK.NE.KSOLD) THEN
               IF (NSTOFI.LT.NMXSTO) THEN
                  NSTOFI           = NSTOFI + 1
                  STOFIR(1,NSTOFI) = ITHETA
                  STOFIR(2,NSTOFI) = JPHI
                  STOFIR(3,NSTOFI) = KSTACK
               ELSE
                  WARNING = 10
                  GOTO 999
               ENDIF
            ENDIF
            JPOLD = JPHI
            ITOLD = ITHETA
            KSOLD = KSTACK
            POINT(1) = POINT(1)+DX
            POINT(2) = POINT(2)+DY
            POINT(3) = POINT(3)+DZ
         ENDIF
      ENDDO
999   CONTINUE
      RETURN
      END
#endif
