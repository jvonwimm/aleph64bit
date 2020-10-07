CKEY  QTRUTH/INTERNAL
C ...............................................................
C! Auxiliary functions in QTRUTH
C  Authors  D. Abbaneo , F. Ligabue 27/07/94  from FINLEP / UPHY
C  Called from everywhere in QTRUTH package
C---------------------------------------------------------------------
#ifndef DOC
      LOGICAL FUNCTION CCBMES(J)
C ...............................................................
C . True if J is a c-cbar meson                                 .
C .   J    / I   Alpha index of the track                       .
C ...............................................................
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C - variables
      INTEGER J
C
C - macros
#include "qmacro.h"
C
C - define CCBMES
      CCBMES=XPEQOR(J,'Jpsi').OR.XPEQOR(J,'psipr').OR.
     @ XPEQOR(J,'chic0').OR.XPEQOR(J,'chic1').OR.
     @ XPEQOR(J,'chic2').OR.XPEQOR(J,'etac')
      RETURN
      END
      LOGICAL FUNCTION FBBAR(IND)
C ...............................................................
C . True if hadron IND contains a b-bar quark                   .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IHQUA,IND
      FBBAR = .FALSE.
      IF (IHQUA(IND).EQ.-1) FBBAR = .TRUE.
      RETURN
      END
C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FBHAD(IND)
C ...............................................................
C . True if hadron IND is bottomed                              .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IND
      LOGICAL FBQUA,FBBAR
      FBHAD = FBQUA(IND).OR.FBBAR(IND)
      RETURN
      END
C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FBQUA(IND)
C ...............................................................
C . True if hadron IND contains a b quark                       .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IHQUA,IND
      FBQUA = .FALSE.
      IF (IHQUA(IND).EQ.1) FBQUA = .TRUE.
      RETURN
      END
C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FCBAR(IND)
C ...............................................................
C . True if hadron IND contains a c-bar quark                   .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IHQUA,IND
      FCBAR = .FALSE.
      IF (IHQUA(IND).EQ.-2) FCBAR = .TRUE.
      RETURN
      END
C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FCHAD(IND)
C ...............................................................
C . True if hadron IND is charmed                               .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IND
      LOGICAL FCQUA,FCBAR
      FCHAD = FCQUA(IND).OR.FCBAR(IND)
      RETURN
      END
C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FCQUA(IND)
C ...............................................................
C . True if hadron IND contains a c quark                       .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IHQUA,IND
      FCQUA = .FALSE.
      IF (IHQUA(IND).EQ.2) FCQUA = .TRUE.
      RETURN
      END
C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FHQUA(IND)
C ...............................................................
C . True if hadron IND contains a b or a c quark                .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IND
      LOGICAL FBQUA,FCQUA
      FHQUA = FBQUA(IND).OR.FCQUA(IND)
      RETURN
      END
C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FHVFL(IND)
C ...............................................................
C . True if hadron IND is heavy-flavoured                       .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IND
      LOGICAL FBHAD,FCHAD
      FHVFL = FBHAD(IND).OR.FCHAD(IND)
      RETURN
      END
C---------------------------------------------------------------------
      LOGICAL FUNCTION FQQUA(IND)
C ...............................................................
C . True if IND is a b or a c quark                             .
C .   IND    / I   Alpha index of the quark                     .
C ...............................................................
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
      INTEGER ICOD,IND
#include "qmacro.h"
      ICOD=KTPCOD(IND)
      FQQUA = ((ICOD.EQ.105).OR.(ICOD.EQ.104))
      RETURN
      END
C
C---------------------------------------------------------------------
      INTEGER FUNCTION IHQUA(IND)
C ...............................................................
C . IND    / I   Alpha index of the hadron                      .
C .                                                             .
C . IHQUA  / I   1   if IND contains a b quark (not a b-bar)    .
C .             -1   if IND contains a b-bar quark              .
C .              2   if IND contains a c quark (not a c-bar)    .
C .             -2   if IND contains a c-bar quark              .
C ...............................................................
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
      INTEGER ICOD,J,IDAU,IDAUCOD,IND,INDD,ILQUA
      LOGICAL ENDCH
C
#include "qmacro.h"
      IHQUA = 0
C
C - discard string, cluster, gluons, quarks etc.
      IF (ENDCH(IND)) RETURN
C
      ICOD=KTPCOD(IND)
      IF (ICOD.GE.368) THEN
        INDD=IND
38      CONTINUE
C
C - loop on daughters
        DO J=1,KNDAU(INDD)
          IDAU=KDAU(INDD,J)
          IDAUCOD=KTPCOD(IDAU)
          IF(IDAUCOD.GE.368) THEN
            INDD=IDAU
            GO TO 38
          ENDIF
          IF (ABS(ILQUA(IDAUCOD)).GT.0) ICOD = IDAUCOD
        ENDDO
      ENDIF
      IHQUA=ILQUA(ICOD)
      RETURN
      END
      INTEGER FUNCTION ILQUA(ICOD)
C ...............................................................
C . ICOD   / I   Alpha hadron code                              .
C .                                                             .
C . ILQUA  / I   1   if the hadron contains a b (not a b-bar)   .
C .             -1   if the hadron contains a b-bar             .
C .              2   if the hadron contains a c (not a c-bar)   .
C .             -2   if the hadron contains a c-bar             .
C ...............................................................
      IMPLICIT NONE
      INTEGER ICOD,IPAR
      LOGICAL BBARY,BQMES,BBMES,CQUA,CBAR
C
C - b baryons or anti-baryons
      BBARY =        (ICOD.GE.211 .AND. ICOD.LE.238)
     &          .OR. (ICOD.GE.259 .AND. ICOD.LE.270)
     &          .OR. (ICOD.GE.283 .AND. ICOD.LE.300)
     &          .OR. (ICOD.GE.351 .AND. ICOD.LE.360)
C
C - b mesons
      BQMES = (ICOD.eq.115)
     &   .OR. (ICOD.eq.118)
     &   .OR. (ICOD.eq.120)
     &   .OR. (ICOD.eq.121)
     &   .OR. (ICOD.eq.123)
     &   .OR. (ICOD.eq.126)
     &   .OR. (ICOD.eq.128)
     &   .OR. (ICOD.eq.129)
C
C - b-bar mesons
      BBMES = (ICOD.eq.116)
     &   .OR. (ICOD.eq.117)
     &   .OR. (ICOD.eq.119)
     &   .OR. (ICOD.eq.122)
     &   .OR. (ICOD.eq.124)
     &   .OR. (ICOD.eq.125)
     &   .OR. (ICOD.eq.127)
     &   .OR. (ICOD.eq.130)
C
      IPAR = ICOD - 2*(icod/2)
C
C - c mesons and baryons
      CQUA=          (ICOD.EQ.35)
     &          .OR. (ICOD.EQ.37)
     &          .OR. (ICOD.EQ.39)
     &          .OR. (ICOD.EQ.41)
     &          .OR. (ICOD.EQ.82)
     &          .OR. (ICOD.EQ.84)
     &          .OR. (ICOD.EQ.86)
     &          .OR. (ICOD.GE.169 .AND. ICOD.LE.210.and.ipar.eq.1)
C
C - c-bar mesons and c anti-baryons
      CBAR=          (ICOD.EQ.36)
     &          .OR. (ICOD.EQ.38)
     &          .OR. (ICOD.EQ.40)
     &          .OR. (ICOD.EQ.53)
     &          .OR. (ICOD.EQ.83)
     &          .OR. (ICOD.EQ.85)
     &          .OR. (ICOD.EQ.87)
     &          .OR. (ICOD.GE.169 .AND. ICOD.LE.210.and.ipar.eq.0)
      ILQUA = 0
C
C - fill ILQUA
      IF ((Ipar.eq.1.and.BBARY).or.BQmes) ILQUA=1
      IF ((Ipar.eq.0.and.BBARY).or.BBmes) ILQUA=-1
      IF (CQUA) ILQUA=2
      IF (CBAR) ILQUA=-2
      RETURN
      END
C
C---------------------------------------------------------------------
      LOGICAL FUNCTION ENDCH(J)
C ...............................................................
C . True if J is at the end of the backwards decay-chain loop   .
C .      i.e. if it's a string, a cluster, or a quark.          .
C .   J    / I   Alpha index of the track                       .
C ...............................................................
      IMPLICIT NONE
      INTEGER J
#include "qdecl.h"
#include "qcde.h"
C
      ENDCH = .false.
      IF (CQTPN(J).EQ.'string'.OR.CQTPN(J).EQ.'cluster'
     +    .or.xpeqor(j,'gluon')
     +    .or.xpeqor(j,'Charged')
     +    .or.xpeqor(j,'dquark')
     +    .or.xpeqor(j,'uquark')
     +    .or.xpeqor(j,'squark')
     +    .or.xpeqor(j,'cquark')
     +    .or.xpeqor(j,'bquark'))  ENDCH = .true.
      RETURN
      END
#endif
