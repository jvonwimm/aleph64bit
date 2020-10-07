      SUBROUTINE KMLEPT(IMC,ISRC,JFLV)
CKEY MUON /INTERNAL
C-----------------------------------------------------------------------
C!  FIND TRUE MC SOURCE OF A DETECTED LEPTON
C
C!    Author    - G.Taylor              15-MAY-1992
C
C Input    IMC       /I      Associated MC track
C Output   ISRC      /I      MC source type
C                            0= none of the below
C                            1= misidentified hadron
C                            2= photon conversion
C                            3= pi0 or eta dalitz decay
C                            4= K decay
C                            5= pi decay
C                            6= b decay
C                            7= secondary c decay
C                            8= primary c decay
C                            9= tau decay
C                           10= vector meson decay
C                           11= muon decay
C                           12= sigma barion decay
C          JFLV      /I      Flavor of mother quark
C                               1= u  2=d  3=s  4=c  5=b  6=t
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      DATA MXDMP/1/,NDMP/0/
#include "qmacro.h"
C-----------------------------------------------------------------------
      ISRC=0
      IF ((.NOT.XPEQOR(IMC,'e+')).AND.(.NOT.XPEQOR(IMC,'mu+'))) THEN
        ISRC=1
C
C ++  Check if charged pi or charged K has a daughter muon
C

        IF (XPEQOR(IMC,'pi+')) THEN
          IADD=1
        ELSEIF (XPEQOR(IMC,'K+')) THEN
          IADD=0
        ELSE
          RETURN
        ENDIF
        DO 32 IDAU = 1,KNDAU(IMC)
   32   IF (XPEQOR(KDAU(IMC,IDAU),'mu+')) ISRC = 4 + IADD
        RETURN
      ENDIF
C
      NMOM = KNMOTH(IMC)
      IF (NMOM.EQ.0) THEN
        ISRC = 0
        RETURN
      ENDIF
      MOM=KMOTH(IMC,1)
C
C++   Handle case of e-->e,gamma
C
   46 IF (CQTPN(MOM).EQ.CQTPN(IMC)) THEN
        IF (KNMOTH(MOM).EQ.0) THEN
          ISRC = 0
          JFLV = 0
          RETURN
        ENDIF
        MOM=KMOTH(MOM,1)
        GO TO 46
      ENDIF
      ICOD=KTPCOD(MOM)
C
C++   Find the original quark flavor
C
      NMOM=KNMOTH(MOM)
      IF (NMOM.EQ.0) THEN
        JMOM=0
      ELSE
        JMOM=KMOTH(MOM,1)
      ENDIF
   20 IF (JMOM.EQ.0) GO TO 40
      JCOD=KTPCOD(JMOM)
      IF (JCOD.GE.101 .AND. JCOD.LE.112) THEN
        IF (JCOD.GT.106) THEN
          JCOD=JCOD-6
        ENDIF
        JFLV=JCOD-100
        GO TO 41
      ENDIF
      NMOM=KNMOTH(JMOM)
      IF (NMOM.EQ.0) THEN
        JMOM=0
      ELSE
        JMOM=KMOTH(JMOM,1)
      ENDIF
      GO TO 20
   40 CONTINUE
      JFLV=0
   41 CONTINUE
C
      IF (XPEQOR(MOM,'mu+').AND.XPEQOR(IMC,'e+')) THEN
        ISRC=11
        RETURN
      ENDIF
      IF (XPEQOR(MOM,'gamma')) THEN
        ISRC=2
        RETURN
      ENDIF
      IF (XPEQOR(MOM,'pi0') .OR. XPEQOR(MOM,'eta')) THEN
        ISRC=3
        RETURN
      ENDIF
      IF (XPEQOR(MOM,'tau+')) THEN
        ISRC=9
        RETURN
      ENDIF
      IF (XPEQOR(MOM,'pi+')) THEN
        ISRC=5
        RETURN
      ENDIF
      IF (XPEQOR(MOM,'K+')) THEN
        ISRC=4
        RETURN
      ENDIF
      IF (XPEQOR(MOM,'K0l')) THEN
        ISRC=4
        RETURN
      ENDIF
      IF (XPEQOR(MOM,'rho0') .OR. XPEQOR(MOM,'omega0')
     &    .OR. XPEQOR(MOM,'Phi') .OR. XPEQOR(MOM,'Jpsi')
     &    .OR. XPEQOR(MOM,'psipr') .OR. XPEQOR(MOM,'Upsilon')
     &    .OR. XPEQOR(MOM,'Ups2s')) THEN
        ISRC=10
        RETURN
      ENDIF
      IF ((ICOD.GE.27 .AND. ICOD.LE.29)
     &     .OR. (ICOD.GE.19 .AND. ICOD.LE.21)) THEN
        ISRC=12
        RETURN
      ENDIF
      IF (ICOD.EQ.2 .OR. ICOD.EQ.3 .OR. ICOD.EQ.5 .OR. ICOD.EQ.6) THEN
        ISRC=0
        RETURN
      ENDIF
C
C++   C hadrons
C
      IF ((ICOD.GE.35 .AND. ICOD.LE.41)
     &          .OR. (ICOD.EQ.53)
     &          .OR. (ICOD.GE.82 .AND. ICOD.LE.87)
     &          .OR. (ICOD.GE.169 .AND. ICOD.LE.210)) THEN
        IF (JFLV.EQ.5) THEN
          ISRC=7
        ELSE
          ISRC=8
        ENDIF
        RETURN
      ENDIF
C
C++   B hadrons
C
      IF (           (ICOD.GE.115 .AND. ICOD.LE.130)
     &          .OR. (ICOD.GE.211 .AND. ICOD.LE.238)
     &          .OR. (ICOD.GE.259 .AND. ICOD.LE.270)
     &          .OR. (ICOD.GE.283 .AND. ICOD.LE.300)
     &          .OR. (ICOD.GE.351 .AND. ICOD.LE.360)) THEN
        ISRC=6
        RETURN
      ENDIF
      IF (NDMP.LT.MXDMP) THEN
        NDMP=NDMP+1
        CALL QWSEC(KSMCT,' ')
        CALL QWSEC(KSMCV,' ')
      ENDIF
      RETURN
      END
#endif
