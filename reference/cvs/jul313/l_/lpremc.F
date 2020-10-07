      SUBROUTINE LPREMC
C-----------------------------------------------------------------------
C!   Prepare Lcal data
C!
C!   Author   : P. H. Hansen  870401
C!   Modified : P. H. Hansen  890905
C!   Modified : P. H. Hansen  970801
C!   (Ressurrection of dead wires at storey 2/3 interface corrected,
C!    Storey energy redistributed in case of dead wires)
C!   (Suppression of negative energies)
C!   Modified : P. H. Hansen  981111
C!     Remove offline pedestal follower after run 45000
C!
C!     Input   : 'LTDI' 'LWDI' 'LECA' 'LDST' 'LDWP' 'LOLE'
C!     Output  : 'LSDA' 'LPDA' 'LOLE'
C!     Created : 'LSDA' 'LPDA'
C!
C!     Description :
C!     ===========
C?     Check for online errors (Fastbus errors, HV trip, Sparks)
C?     Repair LWDI formatting errors during pilot run
C?     Subtract pedestals measured in a previous event
C?     Fill LSDA with calibrated Storey Data LTDI
C?     Fill LPDA with calibrated Wire Data LWDI
C?     Assign to dead storey channels the storey energy fraction
C?     measured in the other storeys.
C?     Assign to dead planes the average energy of the neighbours.
C?
C!======================================================================
#ifndef DOC
#include "lcnamc.h"
#include "lsdajj.h"
#include "lpdajj.h"
#include "ltdijj.h"
#include "lwdijj.h"
#include "lcrejj.h"
#include "lecajj.h"
#include "ldstjj.h"
#include "ldwpjj.h"
#include "bcs.h"
#include "rcurnt.h"
      DIMENSION LREL(4),SCORR(4),WCORR(4)
      DIMENSION ESUM(4),EWSTO(3,4),PEDS(3,48),PEDW(39,4)
C
      DIMENSION IAPED(12,4),MPED(4),CDW(3,4),FDW(4),WFRAC(38),SFRAC(3)
      LOGICAL PILOT,BUG,FIRST
      DATA FIRST/.TRUE./
C
C Tower addresses for channels without zero-suppression
      DATA IAPED/390,250, 72,451,302,180,345,233, 37,411,190,145,
     &           636,807,931,632,799,967,535,695,819,595,719,907,
     &  1148,1319,1443,1144,1311,1479,1047,1207,1331,1107,1231,1419,
     &  1926,1786,1608,1987,1838,1716,1881,1769,1573,1947,1726,1681/
C
C Standard energy fraction in wire plane:
      DATA WFRAC/0.010,0.014,0.019,0.026,0.033,0.039,0.044,0.046,0.047,
     &           0.047,0.054,0.051,0.047,0.047,0.047,0.046,
     &           0.042,0.038,0.038,0.034,0.029,0.025,0.024,
     &           0.021,0.017,0.016,0.014,0.012,0.011,
     &           0.016,0.013,0.009,0.007,0.005,0.004,0.003,0.002,0.002/
C
C Standard energy fraction in storeys
      DATA SFRAC/0.278,0.660,0.061/
C
#include "bmacro.h"
#include "lcmacr.h"
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL VZERO(PEDS,144)
        CALL VZERO(PEDW,156)
        IERR = 0
        FIRST = .FALSE.
      ENDIF
      CALL VZERO(CDW,12)
C
C Check for online errors and sparks
      IF(.NOT.FMCRUN) CALL LOLERR(IERR)
C
C Pilot run?
      PILOT = .FALSE.
      IF(.NOT.FMCRUN.AND.IRUNRC.LE.2663) PILOT = .TRUE.
C
C Global calibration factor
      KLCRE = IW(NALCRE)
      TGEV = RTABL(KLCRE,1,JLCRMC)
C
C Correction factor for each module
      DO 10 I=1,4
        SCORR(I) = 1.
        WCORR(I) = 1.
   10 CONTINUE
      KLECA = IW(NALECA)
      IF(KLECA.GT.0) THEN
        DO 11 I=1,LROWS(KLECA)
          SCORR(I) = RTABL(KLECA,I,JLECSC)
          WCORR(I) = RTABL(KLECA,I,JLECWC)
   11   CONTINUE
      ENDIF
C
C bunch-to-bunch corrections for bunch trains.
      IF(IRUNRC.GE.35000) THEN
        CALL LBUNCA(SCORR,WCORR)
      ENDIF
C
C Raw Wire Data
      KLWDI = IW(NALWDI)
      IF (KLWDI.LE.0)                                    GOTO 999
C
C Make relation from LPDA to LWDI
      CALL VZERO(LREL,4)
      CALL VZERO(MPED,4)
      BUG = .FALSE.
      DO 50 I=1,LROWS(KLWDI)
        MODU = ITABL(KLWDI,I,JLWDMO)
C
C Protect against module number out of range
        IF(MODU.LE.0.OR.MODU.GE.5) GOTO 50
C
C Fix module number during pilot run
        IF(PILOT) THEN
          IF(MODU.NE.I) BUG=.TRUE.
          MODU=I
          IF(I.EQ.3) MODU=4
          IF(I.EQ.4) MODU=3
        ENDIF
        LREL(MODU)=I
   50 CONTINUE
C
C Make room for wire Plane Data
      NM  = 4
      ND  = LLPDAA*NM + LMHLEN
      CALL AUBOS('LPDA',0,ND,KLPDA,IGARB)
      IF(IGARB.EQ.2)                                     GOTO 998
      IF(IGARB.NE.0) THEN
         KLWDI = IW(NALWDI)
         KLSDA = IW(NALSDA)
      ENDIF
      CALL BLIST(IW,'T+','LPDA')
C LPDA header
      IW(KLPDA + LMHCOL) = LLPDAA
      IW(KLPDA + LMHROW) = NM
C LPDA data
      DO 60 MODU=1,4
        ESUM(MODU) = 0.
        KDA = KROW(KLPDA,MODU)
        ILWD = LREL(MODU)
        IF(ILWD.NE.0) THEN
          KDI = KROW(KLWDI,ILWD)
          IW(KDA + JLPDMO) = IW(KDI + JLWDMO)
          IF(PILOT) IW(KDA + JLPDMO) = MODU
          DO 55 K=JLPDEN,JLPDES-1
            EMEV = FLOAT(IW(KDI+K))-PEDW(K-1,MODU)
            EMEV = MAX(EMEV,0.)
C
C Reorder the planes during the pilot run
            IF(BUG) THEN
               IF(K.LE.29) EMEV = FLOAT(IW(KDI+K-1))
               IF(K.EQ.30) GOTO 55
            ENDIF
C
            RW(KDA+K) = TGEV*WCORR(MODU)*EMEV
            ESUM(MODU) = ESUM(MODU) + RW(KDA+K)
   55     CONTINUE
C
C Record the analog sum if available
          IF(LCOLS(KLWDI).GE.JLPDES) THEN
            ESUM(MODU) = TGEV*WCORR(MODU)
     &                  *FLOAT(IW(KDI+JLPDES))
          ENDIF
          RW(KDA+JLPDES) = ESUM(MODU)
        ELSE
          IW(KDA + JLPDMO) = MODU
          CALL VZERO(RW(KDA+JLPDEN),39)
        ENDIF
C
C Flag modules to be used for pedestals
        IF(IERR.EQ.0.AND.ABS(ESUM(MODU)).LT.0.01.AND.
     &     IRUNRC.LT.43000) MPED(MODU)=1
   60 CONTINUE
C
C Store new wire pedestals
      DO 62 MODU=1,4
        ILWD = LREL(MODU)
        IF(MPED(MODU).EQ.1.AND.ILWD.LE.LROWS(KLWDI).AND.ILWD.NE.0) THEN
          KDI = KROW(KLWDI,ILWD)
          DO 61 K=JLPDEN,JLPDES-1
            PEDW(K-1,MODU)=(PEDW(K-1,MODU)+FLOAT(IW(KDI+K)))*0.5
   61     CONTINUE
        ENDIF
   62 CONTINUE
C
C Handle dead wire planes
      KLDWP = IW(NALDWP)
      IF(KLDWP.GT.0.AND.LROWS(KLDWP).GE.1) THEN
        DO 80 I=1,LROWS(KLDWP)
          IADDR = ITABL(KLDWP,I,JLDWAD)
          MODU = IADDR/64+1
          IPLAN = IADDR - (MODU-1)*64
          KDA = KROW(KLPDA,MODU)
C
C Take care of special cases at interfaces between storeys
          IF(IPLAN.EQ.1) THEN
            RW(KDA+JLPDEN) = 0.75*RW(KDA+JLPDEN+1)
          ELSEIF(IPLAN.EQ.38) THEN
            RW(KDA+JLPDEN+37) = 0.75*RW(KDA+JLPDEN+36)
          ELSEIF(IPLAN.EQ.29) THEN
            RW(KDA+JLPDEN+IPLAN-1) =
     &        0.5*(RW(KDA+JLPDEN+IPLAN-2)
     &             +0.5*RW(KDA+JLPDEN+IPLAN))
          ELSEIF(IPLAN.EQ.30) THEN
            RW(KDA+JLPDEN+IPLAN-1) =
     &        0.5*(2.*RW(KDA+JLPDEN+IPLAN-2)
     &             +RW(KDA+JLPDEN+IPLAN))
          ELSEIF(IPLAN.GE.2.AND.IPLAN.LE.37) THEN
            RW(KDA+JLPDEN+IPLAN-1) =
     &        0.5*(RW(KDA+JLPDEN+IPLAN-2)
     &             +RW(KDA+JLPDEN+IPLAN))
          ENDIF
C
C Calculate a missing fraction for each storey due to dead wires
C Assume a standard shower profile (45 GeV electron)
          IF(IPLAN.LE.9) THEN
            CDW(1,MODU) = CDW(1,MODU) + WFRAC(IPLAN)/SFRAC(1)
          ELSEIF(IPLAN.LE.29) THEN
            CDW(2,MODU) = CDW(2,MODU) + WFRAC(IPLAN)/SFRAC(2)
          ELSE
            CDW(3,MODU) = CDW(3,MODU) + WFRAC(IPLAN)/SFRAC(3)
          ENDIF
   80   CONTINUE
      ENDIF
C
C Here sum the wire energies in each storey
      DO 82 MODU=1,4
        EWSTO(1,MODU) = 0.
        EWSTO(2,MODU) = 0.
        EWSTO(3,MODU) = 0.
        KDA = KROW(KLPDA,MODU)
        DO 81 K=JLPDEN,JLPDES-1
          IF(K.LE.9) THEN
            EWSTO(1,MODU) = EWSTO(1,MODU) + RW(KDA+K)
          ELSEIF(K.LE.29) THEN
            EWSTO(2,MODU) = EWSTO(2,MODU) + RW(KDA+K)
          ELSE
            EWSTO(3,MODU) = EWSTO(3,MODU) + RW(KDA+K)
          ENDIF
   81   CONTINUE
C
C An overall calibration factor for storeys due to dead wires
C is assumed to have been already applied. Here is its inverse:
        FDW(MODU) = 1. - CDW(1,MODU)*SFRAC(1)
     &           - CDW(2,MODU)*SFRAC(2)
     &           - CDW(3,MODU)*SFRAC(3)
   82 CONTINUE
C
C Raw storey data
      KLTDI  = IW(NALTDI)
      IF (KLTDI.LE.0)                                    GOTO 999
      NTOW   = LROWS(KLTDI)
      IPED = 1
C
C Make room for Storey Data
      ND  = LLSDAA*NTOW + LMHLEN
      CALL AUBOS('LSDA',0,ND,KLSDA,IGARB)
      IF (IGARB.EQ.2)                                    GOTO 998
      IF (IGARB.NE.0) KLTDI = IW(NALTDI)
      CALL BLIST(IW,'T+','LSDA')
      IW(KLSDA + LMHCOL) = LLSDAA
      IW(KLSDA + LMHROW) = NTOW
C
C Loop over towers
      DO 40 ITOW = 1,NTOW
        KDA = KROW(KLSDA,ITOW)
        KDI = KROW(KLTDI,ITOW)
C
C Find address, module and summing amplifier
        IADDR = IW(KDI + JLTDAD)
C
C Change miscabelled addresses
        IF(.NOT.FMCRUN.AND.IRUNRC.LE.8169) THEN
          IA = IADDR
          IF(IADDR.EQ.806) IA=840
          IF(IADDR.EQ.840) IA=806
          IF(IADDR.EQ.1318)IA=1352
          IF(IADDR.EQ.1352)IA=1318
          IADDR = IA
        ENDIF
C
        IAMP = LSUMAM(IADDR)
        MODU = LCMOD(IADDR)
C
C Protect against tower address out of range
        IF(IAMP.EQ.0.OR.MODU.LE.0.OR.MODU.GE.5) THEN
          IW(KDA+JLSDAD) = 1
          GOTO 40
        ENDIF
C
        IW(KDA + JLSDAD) = IADDR
C
C Store calibrated and pedestal subtracted data
C also reweigh storeys due to dead wire planes
        DO 30 K = 1,3
          RW(KDA+JLSDAD+K) = TGEV*SCORR(MODU)*FDW(MODU)*
     &       (FLOAT(IW(KDI+JLTDAD+K))-PEDS(K,IAMP))/
     &       MAX(1.-CDW(K,MODU),0.4)
          RW(KDA+JLSDAD+K) = MAX(RW(KDA+JLSDAD+K),0.)
   30   CONTINUE
C
C Dirty fix for large and rapid pedestal shifts
        IF(IRUNRC.GE.6000.AND.IRUNRC.LE.7427) THEN
           IF(IAMP.EQ.30) RW(KDA+JLSDAD+3) = 0.
        ENDIF
C
C Initialise cluster relation
        IW(KDA + JLSDLC) = 0
C
C Store new pedestals
        IF(.NOT.FMCRUN) THEN
          IF(MPED(MODU).EQ.1) THEN
            IF(IADDR.EQ.IAPED(IPED,MODU)) THEN
              PEDS(1,IAMP) = (PEDS(1,IAMP)+FLOAT(IW(KDI+JLTDAD+1)))*0.5
              PEDS(2,IAMP) = (PEDS(2,IAMP)+FLOAT(IW(KDI+JLTDAD+2)))*0.5
              PEDS(3,IAMP) = (PEDS(3,IAMP)+FLOAT(IW(KDI+JLTDAD+3)))*0.5
              IPED = IPED + 1
              IF(IPED.GE.13) IPED = 1
            ENDIF
          ENDIF
        ENDIF
   40 CONTINUE
C
C Handle dead storeys
      KLDST = IW(NALDST)
      IF(KLDST.GT.0.AND.LROWS(KLDST).GE.1.
     &  AND.NTOW.GE.1) THEN
        DO 91 I=1,LROWS(KLDST)
          IADDR = ITABL(KLDST,I,JLDSAD)
          MODU = LCMOD(IADDR)
          IF(MODU.GE.5.OR.MODU.LE.0) GOTO 91
          ISTOR = ITABL(KLDST,I,JLDSST)
          IF(ISTOR.GE.4.OR.ISTOR.LE.0) GOTO 91
          I1 = MOD(ISTOR,3)+1
          I2 = MOD(ISTOR+1,3)+1
          ETOT = EWSTO(ISTOR,MODU)
          ETOT1 = EWSTO(I1,MODU)
          ETOT2 = EWSTO(I2,MODU)
          ISDA = 0
          DO 90 J=1,NTOW
            IADDN = ITABL(KLSDA,J,JLSDAD)
            MODN  = LCMOD(IADDN)
            IF(MODN.NE.MODU) GOTO 90
            IF(IADDN.EQ.IADDR) THEN
              ISDA = J
              E1 = RTABL(KLSDA,J,JLSDEN+I1-1)
              E2 = RTABL(KLSDA,J,JLSDEN+I2-1)
            ENDIF
   90     CONTINUE
          IF(ISDA.EQ.0) GOTO 91
          FRAC = (E1+E2)/AMAX1(ETOT1+ETOT2,0.4)
          KDA = KROW(KLSDA,ISDA)
          RW(KDA+JLSDEN+ISTOR-1) = FRAC*ETOT
   91   CONTINUE
      ENDIF
C
      GOTO 999
  998 CALL REPORT('LPREMC','AUBOS failure',1)
  999 CONTINUE
      END
#endif
