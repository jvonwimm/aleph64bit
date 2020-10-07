      SUBROUTINE SIPREDA
C----------------------------------------------------------------------
C!  - SICAL Prepare data
C!
C!   Author   :- B. Bloch                     8-NOV-1991
C!
C?
C!    Modified:   B. Bloch 18/10/94
C!  - Allow to start from existing SPDA bank if SIDI not available
C!  - Apply overall E-scale factor from SECA in calibration mode only
C!    Modified:   B. Bloch 24/10/95
C!  - Apply a bunch dependant calibration for juke box mode in 95
C?
C!======================================================================
#ifndef DOC
#include "secajj.h"
#include "secbjj.h"
#include "spdajj.h"
#include "sidijj.h"
#include "smpdjj.h"
#include "sinamc.h"
#include "rcurnt.h"
#include "rparac.h"
#include "bcs.h"
#include "rlunit.h"
#include "rflags.h"
      EXTERNAL NAMIND
      EXTERNAL SILIST,SISWAP,SIGTWA,SIEVST,SIEPCR
      INTEGER  SILIST ,SISWAP ,SIGTWA
      DIMENSION P0COR(2),QCOR(2,4),PCOR(2)
      LOGICAL FIRST,CALIB
      LOGICAL TLCAL,TRAND,TPHYS,TSCAL(4),FAIL
      PARAMETER ( XSTIM0 = 272)
      DATA FIRST /.TRUE./ ,CALIB /.FALSE./
      DATA IRUNO/0/
#include "bmacro.h"
      IF (FIRST) THEN
        FIRST = .FALSE.
        EMIN = 0.
        NASECB = NAMIND('SECB')
        CALIB = .TRUE.
        IF ( IW(NAMIND('NSCA')).GT.0 ) CALIB = .FALSE.
                IF ( IRUNRC.LT.2001 ) CALIB = .FALSE.
      ENDIF
      IF ( IRUNRC.NE.IRUNO ) THEN
C - calibration constants
        DO 10 IMOD=1,2
          PCOR(IMOD) = 1.
          P0COR(IMOD) = 1.
          QCOR(IMOD,1) = -1.
   10   CONTINUE
        KSECA = IW(NASECA)
      IF ( (KSECA.GT.0).AND.CALIB) THEN
          DO 11 IMOD =1,LROWS(KSECA)
            P0COR(IMOD) = RTABL(KSECA,IMOD,JSECPC)
   11     CONTINUE
        ENDIF
C   Special for 1995 : we want to have a calibrated energy for physics
C   triggers even if hold is not the same as ALEPH
C   if the calibration per bunch is available from the SECB bank, use it
C   if not , use the timing curve to find where we are ....
      IF ( IRUNRC.GT.35000) THEN
C - get bunch train configuration if any
        CALL SIJUKE(NSWAG,SDELAY,SDELTA)
        KSECB = IW(NASECB)
        IF (KSECB.GT.0) THEN
           DO 12 IMOD = 1,2
           DO 13 IBUN = 1,4
              QCOR(IMOD,IBUN) = RTABL(KSECB,IBUN,IMOD)
   13      CONTINUE
   12      CONTINUE
        ELSE
           ESINOM = SIEVST(SDELAY)
           ESINOM0 = SIEVST(XSTIM0)
        ENDIF
      ENDIF
      IF(JDBDRF(JULSI).GE.1) WRITE(LOUTRL,777) IRUNRC,PCOR
 777  FORMAT(' == SICAL : E PER MODULE FOR RUN  ',I8,2F8.4)
      IF (KSECB.GT.0) WRITE(LOUTRL,778) QCOR
 778  FORMAT(' == SICAL : E PER MODULE AND PER BUNCH ',2F8.4)
       IRUNO = IRUNRC
       ENDIF
C - Raw data
                KSIDI = IW(NASIDI)
                KSMPD = IW(NASMPD)
                IF (KSIDI.GT.0) THEN
C   Drop bank if already here
            IND = NDROP('SPDA',0)
                  NTPT = LROWS(KSIDI)
                  NPAD = 3*NTPT
                  IF (NPAD.LE.0) GO TO 999
                  NDEAD = 0
                  IF ( KSMPD.GT.0) NDEAD = LROWS(KSMPD)
C - Book prepared pad data
        LEN = LSPDAA*(NPAD+NDEAD)+LMHLEN
        CALL AUBOS('SPDA',0,LEN,KSPDA,IGARB)
        IF ( IGARB.EQ.2) GO TO 998
                  IF ( KSPDA.LE.0) GO TO 998
C        CALL BLIST(IW,'T+','SPDA')
        IW(KSPDA+LMHCOL) = LSPDAA
        IW(KSPDA+LMHROW) = NPAD
C - Get the calibration factor as :
C     overall P0COR ( MODULE )   from SECA
C     then correct for bunch calibration QCOR   from SECB
C     or from timing curve 1./SIESCA
        DO 14 IMOD = 1,2
           PCOR(IMOD) = P0COR(IMOD)
  14    CONTINUE
C    check trigger type
        IF ( IRUNRC.GT.35000) THEN
           CALL STRIGF(IRUNRC,TLCAL,TRAND,TPHYS,TSCAL,FAIL)
           IF ( TPHYS ) THEN
C    Get Aleph and Sical bunch numbers
             CALL RQBUNC(IABUN,INBU,NWAG,IQUA)
             ISBUN = SIGTWA(IDUM)
             XSTIM = SDELTA*(ISBUN-1)-(IABUN-1)*INBU+SDELAY
             IF ( NWAG.EQ.NSWAG)  THEN
                SIESCA = SIEVST(XSTIM)/ESINOM
             ELSE
                SIESCA = SIEVST(XSTIM)/ESINOM0
             ENDIF
             DO 15 IMOD =1,2
                IF (QCOR(IMOD,1).GT.0.) THEN
                   PCOR(IMOD) = P0COR(IMOD)*QCOR(IMOD,IABUN)
                ELSE
                   IF ( SIESCA.NE.0.) PCOR(IMOD) = P0COR(IMOD)/SIESCA
                ENDIF
  15         CONTINUE
           ENDIF
        ENDIF
        KSIDI = IW(NASIDI)
        NPA =0
        DO 20 IT = 1,NTPT
          KDI = KROW(KSIDI,IT)
          IAD = IW(KDI+JSIDAD)
          CALL SIDCOD(IAD,0,IMD,IST,IPH,IRD)
          IF ( IMD.LT.0 .OR. IST.LT.0 .OR. IPH.LT.0 .OR. IRD.LT.0 ) THEN
            CALL RERROR ('SIPREDA', -2, 'Invalid SIDI Address')
            GOTO 20
          ENDIF
          IOK = SISWAP(IMD,IST,IPH,IRD,IM2,IS2,IP2,IR2)
          IF ( IOK.NE.0) THEN
            IMD = IM2
            IST = IS2
            IPH = IP2
            IRD = IR2
          ENDIF
          DO 21 IA =1 ,3
            IE= IW(KDI+JSIDE1+IA-1)
                  RA = FLOAT(IE)/1000.
                  RE = FLOAT(IE)/1000.
                  IF (CALIB) RA = SIEPCR(RE,IMD,IST+IA-1,IPH,IRD)
                  IF ( RA*PCOR(IMD).LE.EMIN) GO TO 21
            NPA = NPA + 1
            CALL SIENCZ(IBD,IMD,IST+IA-1,IPH,IRD)
            IOV = 0
            IF ( IBITS(IAD,11+IA,1).NE.0) IOV = 1
            KDA = KROW(KSPDA,NPA)
            IW(KDA+JSPDAD) = IBD
                  RW(KDA+JSPDEN) = RA * PCOR(IMD)
                  RW(KDA+JSPDER) = RE
            IW(KDA+JSPDOV) = IOV
            IW(KDA+JSPDSC) = 0
            IW(KDA+JSPDSP) = 0
            IW(KDA+JSPDSM) = 0
   21     CONTINUE
   20   CONTINUE
C Now loop on dead pad adresses
        IF ( NDEAD.LE.0) GO TO 23
        DO 22 IT = 1 , NDEAD
          IAD = ITABL(KSMPD,IT,JSMPAD)
          INEW = SILIST(IAD,NPA)
          IF (INEW.EQ.0) THEN
            NPA = NPA+1
            KDA = KROW(KSPDA,NPA)
            IW(KDA+JSPDAD) = IAD
            RW(KDA+JSPDEN) = 0.
                  RW(KDA+JSPDER) = 0.
            IW(KDA+JSPDOV) = 0
            IW(KDA+JSPDSC) = 0
            IW(KDA+JSPDSP) = 0
            IW(KDA+JSPDSM) = IT
          ELSE
                  KDA = KROW(KSPDA,INEW)
                  RW(KDA+JSPDEN) = 0.
                  RW(KDA+JSPDER) = 0.
            IW(KDA+JSPDSM) = IT
          ENDIF
   22   CONTINUE
   23   CONTINUE
C Shrink SPDA to real size
        IW(KSPDA + LMHROW) = NPA
        LEN = NPA*LSPDAA + LMHLEN
        CALL AUBOS('SPDA',0,LEN,KSPDA,IGARB)
        IF(IGARB .EQ. 2)  GOTO 998
        ELSE
C recreate the (non)calibrated energy from raw energy and reset previous
C clustering
           KSPDA = IW(NASPDA)
           IF ( KSPDA.GT.0) THEN
              DO 24 IT = 1,LROWS(KSPDA)
                 RAW = RTABL(KSPDA,IT,JSPDER)
                 RCO = RAW
                 IF ( CALIB) THEN
                   IAD = ITABL(KSPDA,IT,JSPDAD)
                   CALL SIDCOZ(IAD,IMD,IST,IPH,IRD)
                   RCO = SIEPCR(RAW,IMD,IST,IPH,IRD)
                 ENDIF
                 RCO = RCO*PCOR(IMD)
                 KDA = KROW(KSPDA,IT)
                 RW(KDA+JSPDEN) = RCO
                 IW(KDA+JSPDSC) = 0
                 IW(KDA+JSPDSP) = 0
  24          CONTINUE
           ENDIF
        ENDIF
        IF(FDEBRF.AND.JDBDRF(JULSI).GE.2) CALL PRTABL('SPDA',0)
      GO TO 999
  998 CALL RERROR ('SIPREDA', -1, 'Unable to create SPDA')
  999 RETURN
      END
#endif
