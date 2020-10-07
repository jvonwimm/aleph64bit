      SUBROUTINE CHECKQ(MTRA,IPROC,ISTOG,IPOP)
CKEY  QTRUTH/INTERNAL
C ...............................................................
C! Determines the production process in QTRUTH
C  Authors  D. Abbaneo , F. Ligabue 27/07/94  from FINLEP / UPHY
C  Called from QTRUTH
C .                                                             .
C . Input                                                       .
C .   MTRA   / I   Alpha index of the MC track which is the     .
C .                daughter of the string or of the last        .
C .                H.F. hadron                                  .
C .                                                             .
C .   IPROC  / I   Production process                           .
C .                 1    b -> c X                               .
C .                 2    b -> u X                               .
C .                 3    b -> cbar c ; cbar -> X                .
C .                 4    b -> cbar u ; cbar -> X                .
C .                 5    b -> c -> X                            .
C .                 6    c -> X                                 .
C .                 7    fragmentation or uds                   .
C .                                                             .
C .   ISTOG  / I   Flavour of the event                         .
C .                1=d, 2=u, 3=s, 4=c, 5=b                      .
C .                                                             .
C . Output                                                      .
C .   IPOP   / I   Flavour of the quark from gluon splitting    .
C .                4 or 5 if the chain is originating from a    .
C .                       c or a b quark respectively, which    .
C .                       are coming from gluon splitting       .
C .                0      otherwise                             .
C .                                                             .
C ...............................................................
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C - functions
      LOGICAL FHVFL,FQQUA,FHQUA,FBHAD,ENDCH
C
C - variables
      INTEGER MTRA,IPROC,ISTOG,IPOP,ITRU,NHVH,JSTRI,JQ,JQQ,
     @     NPARQ,JPARQ,MGM
C
C - macros
#include "qmacro.h"
C----------------------------------------------------------------------
C - check flavour
      IPOP=0
      IF (IPROC.EQ.7) RETURN
C
C - if ISTOG and IPROC do not match set IPOP
      IF (IPROC.LE.5.AND.ISTOG.NE.5) then
        IPOP=5
        RETURN
      ENDIF
      IF (IPROC.EQ.6.AND.ISTOG.NE.4) then
        IPOP=4
        RETURN
      ENDIF
C
C - count H.F. hadrons in the event
      NHVH = 0
      DO ITRU=KFMCT,KLMCT
        IF (FHVFL(ITRU)) THEN
          JSTRI=KMOTH(ITRU,1)
          IF (ENDCH(JSTRI))
     @         NHVH = NHVH+1
        ENDIF
      ENDDO
      IF (NHVH.NE.4) RETURN
C
C - go back to the first H.F. hadron and to the quark before the string
C   (or quark)
 9    JQ=KMOTH(MTRA,1)
      IF (ENDCH(JQ)) GO TO 10
      MTRA=JQ
      GOTO 9
 10   continue
C go one step upstream only if it's a string and not already a quark
      IF (CQTPN(JQ).EQ.'string'.OR.CQTPN(JQ).EQ.'cluster')
     ^     JQ = KMOTH(JQ,1)
      JQQ = JQ
C
C - see if quark was coming from a gluon
 19   NPARQ=KNMOTH(JQ)
      IF (NPARQ.GT.0) THEN
        JPARQ=KMOTH(JQ,1)
C
C - set IPOP
        IF (CQTPN(JPARQ).EQ.'gluon') THEN
          IF ((.NOT.FQQUA(JQQ)).AND.FHQUA(MTRA)) RETURN
          IF (FQQUA(JQQ).AND.(.NOT.FHQUA(MTRA))) RETURN
          IPOP = ISTOG
          RETURN
        ENDIF
        JQ = JPARQ
        GO TO 19
      ENDIF
      IF (FQQUA(JQQ).AND.FHQUA(MTRA)) RETURN
      IF ((.NOT.FQQUA(JQQ)).AND.(.NOT.FHQUA(MTRA))) RETURN
      IPOP = ISTOG
      RETURN
      END
#endif


