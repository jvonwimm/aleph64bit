      SUBROUTINE QTRUTH(ITK,IFLA,IPOP,ICHAIN,ISPLIT,LEPID,IBMES)
C ...............................................................
C! User routine to get the history of MCarlo reconstructed track
C  Authors  D.Abbaneo , F. Ligabue 27/07/94  from FINLEP / UPHY
CKEY  QTRUTH / USER
C .                                                             .
C .  This routine gives information about the "history" of      .
C .  the reconstructed track ITK. It works on Monte Carlo       .
C .  hadronic events generated with LUND. The information       .
C .  provided is aimed to the classification of candidate       .
C .  leptons for heavy flavour tagging, but may be useful       .
C .  also in many other cases.                                  .
C .                                                             .
C .                                                             .
C . Input                                                       .
C .   ITK    / I   Alpha index of the reconstructed track       .
C .                                                             .
C . Output                                                      .
C .   IFLA   / I   Flavour of the event                         .
C .                1=d, 2=u, 3=s, 4=c, 5=b                      .
C .                                                             .
C .   IPOP   / I   Flavour of the quark from gluon splitting    .
C .                4 or 5 if the chain is originating from a    .
C .                       c or a b quark respectively, which    .
C .                       are coming from gluon splitting       .
C .                0      otherwise                             .
C .                                                             .
C .   ICHAIN / I   H.F. process originating the decay chain     .
C .                -2    b -> cbar u ; cbar -> X                .
C .                -1    b -> u X                               .
C .                 1    b -> c X                               .
C .                 2    b -> cbar c ; cbar -> X                .
C .                 3    b -> c -> X                            .
C .                 4    c -> X                                 .
C .                 5    fragmentation or uds                   .
C .                                                             .
C .   ISPLIT / I   End of the decay chain                       .
C .                 0   lepton from H.F. hadron                 .
C .                 1   K -> l                                  .
C .                 2   pi -> l                                 .
C .                 3   gamma -> l                              .
C .                 4   J/psi -> l                              .
C .                 5   psi' -> l                               .
C .                 6   pi0 -> l                                .
C .                 7   other -> l                              .
C .                 8   K                                       .
C .                 9   pi                                      .
C .                10   other                                   .
C .                                                             .
C .   LEPID  / I   Track identity                               .
C .                -1   unidentified                            .
C .                 0   non lepton                              .
C .                 1   mu                                      .
C .                 2   tau -> mu                               .
C .                 3   electron                                .
C .                 4   tau -> e                                .
C .                                                             .
C .   IBMES  / I   Identity of the decaying b particle (if any) .
C .                -2   "oscillated" Bs                         .
C .                -1   "oscillated" Bd                         .
C .                 0   b baryon or non b particle              .
C .                 1   Bd                                      .
C .                 2   Bs                                      .
C .                 3   B+                                      .
C .                                                             .
C ...............................................................
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C - subroutines and functions
      INTEGER NLINK,NAMIND,JMBEST,IBRE
C
C - variables
      INTEGER ITK,IFLA,IPOP,ICHAIN,ISPLIT,LEPID,IBMES,MOM,IDAU,
     @ JKEVH,MCT,IBR,JFKIN,JFVER,ILFI,NVERA,ICIAO,MTRA,IPROC
      REAL DX,DY,DZ,RHO
      CHARACTER*4 CIAO
C
      EQUIVALENCE(CIAO,ICIAO)
C
C - macros
#include "qmacro.h"
C
C - flavour of the event
      IFLA=0
      IF (IW(NAMIND('KHVF')).EQ.0) RETURN
      JKEVH=NLINK('KEVH',0)
      IF (JKEVH.NE.0) THEN
         IFLA=ITABL(JKEVH,1,4)
         IFLA=IFLA/1000
      ENDIF
C
C - MC particle code
      IF (KCLASS(ITK).EQ.KMONTE) THEN
         MCT = ITK
         IF(KSTABC(ITK).LE.0) THEN
            LEPID = -1
            return
         ENDIF
      ELSE
         MCT=JMBEST(ITK)
         IF (MCT.EQ.0) THEN
           LEPID=-1
           RETURN
         ENDIF
      ENDIF
C
C - particle ID
      LEPID=0
C
C - in case of kaons and pions check for "late" decays (but within
C   the TPC) avoiding delta-rays
      IF (XPEQOR(MCT,'pi+').OR.XPEQOR(MCT,'K+')) THEN
        JFKIN = NLINK('FKIN',0)
        JFVER = NLINK('FVER',0)
        DO IDAU = 1,KNDAU(MCT)
          ILFI = KDAU(MCT,IDAU)
          NVERA = ITABL(JFKIN,KTN(ILFI),6)
          ICIAO = ITABL(JFVER,NVERA,9)
C
C - look at the decay vertex location
          DX = RTABL(JFVER,NVERA,1)
          DY = RTABL(JFVER,NVERA,2)
          DZ = ABS(RTABL(JFVER,NVERA,3))
          RHO = SQRT(DX**2+DY**2)
C
C - exclude case of Delta-ray and verify the volume:
C   define as 'decay' only if the decay vertex is within the TPC
          IF ((XPEQOR(ILFI,'e+')).OR.(XPEQOR(ILFI,'mu+'))) THEN
            IF ((CIAO.NE.'DRAY').AND.(RHO.LT.180).AND.(DZ.LT.235)) THEN
              MCT=ILFI
              GO TO 333
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
333   IF (XPEQOR(MCT,'e+')) LEPID=3
      IF (XPEQOR(MCT,'mu+')) LEPID=1
C
C - handle case of bremsstrahlung
      IF (LEPID.NE.0) THEN
        IBR=IBRE(MCT)
        IF (IBR.EQ.0) THEN    ! motherless particle
          LEPID=-1
          RETURN
        ELSE
          MCT=IBR
        ENDIF
C
C - tau decays
        MOM = KMOTH(MCT,1)
        IF (XPEQOR(MOM,'tau+')) THEN
          LEPID=LEPID+1
          MCT=MOM                     ! record tau track
C
C - handle case of tau bremsstrahlung (?!?)
          IBR=IBRE(MCT)
          IF (IBR.EQ.0) THEN
            LEPID=-1
            RETURN
          ELSE
            MCT=IBR
          ENDIF
        ENDIF
      ENDIF
C
C - check mother track ID and fill ISPLIT
      CALL ISP(MCT,MTRA,ISPLIT)
C
C - handle conversions of ISR and FSR photons
      if (isplit.eq.-3) then
        ipop   = 0
        isplit = 3
        ichain = 5
        ibmes  = 0
        return
      endif
      IF (MTRA.EQ.0) THEN
        LEPID=-1
        RETURN
      ENDIF
C
C - go back to production process and fill other variables
      CALL GEN(MTRA,IPROC,IBMES)
C
C - check for gluon splitting and fill IPOP
      CALL CHECKQ(MTRA,IPROC,IFLA,IPOP)
C
C - map IPROC into ICHAIN and that's it
      IF (IPROC.LE.5) THEN
        ICHAIN = (IPROC+1)/2*(MOD(IPROC,2)*2-1)
      ELSE
        ICHAIN = IPROC-2
      ENDIF
      RETURN
      END
#endif
