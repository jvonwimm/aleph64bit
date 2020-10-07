      SUBROUTINE TIZERO ( TMEANR,IGOO )
C-----------------------------------------------------------------------
CKEY EDIR T0 BEAM
C! Calculate T0 from Ecal wire module PASTIS.
C-
C   Input  : None
C   Output : TMEANR= -9999. Tzero not available
C                  = Otherwise average mean including bunch train
C            IGOO  = 0  Not in time with beam
C                    1  GOOD beam tag  (+-100ns)
C-
C   Called by   : SELEVT,SELCAL
C   Calls  : None
C   Input banks : PEWI/PWEI, EVEH
C-
C                                   Author: M.N.Minard       date !
C - modified by : F.Ranjard - 950511
C                 use PWEI when PEWI is not there
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "pewijj.h"
C --
      DIMENSION ETCAL ( 3 ) , ECAL ( 3 )
      DATA NAPEWI, NAPWEI /2*0/
C --
#include "bmacro.h"
C --
      IF (NAPEWI.EQ.0) THEN
         NAPEWI = NAMIND ('PEWI')
         NAPWEI = NAMIND ('PWEI')
      ENDIF
      KPEWI = IW ( NAPEWI)
      IF (KPEWI.EQ.0) KPEWI = IW(NAPWEI)
C
      CALL VZERO (ETCAL,3)
      CALL VZERO (ECAL,3)
      IGOO = 0
      NWIRE = 0
      TMEAN = -9999.
      TMEANR = -9999.
      TZERO = -9999.
      CALL RQBUNC(IBUN,INBU,NWAG,IQUA)
C --
C   Exclude run range where no t0 for End cap
C --
      KEVEH = IW (NAMIND('EVEH'))
      IRUNC = IW ( KEVEH + 2 )
C --
C   Calculate weigthed mean
C --
      IF ( KPEWI.EQ.0) GO TO 980
      LCOL = LCOLS (KPEWI)
      DO 100 IPEWI = 1,LROWS(KPEWI)
      IMOD = ITABL ( KPEWI,IPEWI,JPEWMN)
      ISC = 2
      IF ( IMOD .LT.13 ) ISC = 1
      IF ( IMOD.GT.24  ) ISC = 1
      IF (IRUNC.GT.2000 .AND. IRUNC.LT.6100) THEN
         IF (IMOD.EQ.25.OR.IMOD.EQ.26) GO TO 100
      ENDIF
C --
C    Calculate wire energy
C --
      ENSUM  = 0.
      DO 50 IADC = 1, 45
      ENSUM = ENSUM + FLOAT(ITABL(KPEWI,IPEWI,IADC+1))/1000000.
 50   CONTINUE
      IF ( ENSUM.LT. .50 ) GO TO 100
      NWIRE = NWIRE + 1
      IADC0 = ITABL ( KPEWI,IPEWI,JPEWSS)
      IF ( IADC0.GT.300) GO TO 100
      ECAL ( ISC ) = ECAL ( ISC) + ENSUM
      TZER = ITABL(KPEWI,IPEWI,LCOL)
      ETCAL (ISC) = ETCAL(ISC)+(ENSUM*TZER)
 100  CONTINUE
C --
C   Calculate pastis
C --
       DO 60 ISC =1,2
       IF ( ECAL(ISC) .LT.1.) THEN
          ETCAL ( ISC) = -9999.
          GO TO 60
       ENDIF
       ETCA = ETCAL ( ISC) / ECAL (ISC)
       ETCAL ( ISC) = ETCA
       TMEAN = -9999.
 60    CONTINUE
       ITYP = 0
       DO 70 ISC = 1,2
       IF ( ABS(ETCAL(ISC)).LT.ABS(TMEAN)) THEN
          TMEAN = ETCAL (ISC)
          ITYP = ISC
       ENDIF
 70    CONTINUE
       TMEANR = TMEAN
       IF (NWAG.GT.1)THEN
           IF (INBU.LT.240.OR.INBU.GT.500)INBU = 0.
           TMEAN = TMEAN-FLOAT(IBUN-1)*FLOAT(INBU)
       ENDIF
       IF ( TMEAN.LT.-9000.AND.NWIRE.EQ.0) GO TO 980
       IF ( TMEAN.LT.-9000.AND.NWIRE.NE.0) TMEAN = -9000.
       IF ( ITYP.EQ.1.AND.(ABS(TMEAN).LT.100. )) IGOO = 1
       IF ( ITYP.EQ.2.AND.ABS(TMEAN).LT.100.) IGOO = 1
 980   CONTINUE
       RETURN
       END
#endif
