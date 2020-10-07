      SUBROUTINE FIXPECO
CKEY  EVENT / INTERNAL
C----------------------------------------------------------------------
C!  - Correct PECO/DECO corrected E for evts proc. w/ jul258/ale133
C!
C!   Author   :- E. Lancon             30-MAR-1992
C! Modified   :- S. Haywood             1-SEP-1992
C!   Description
C!   ===========
C!
C!      Real Data (NOT Monte-Carlo) produced with Julia version 2.58 and
C!      Alephlib 133 have ECAL Pad energies too low. Due to a wrong clus
C!      correction applied to clusters in PECO, DECO banks.
C!      NOTE : Since DECO does not contain the cluster raw energy, it is
C!      impossible to make the true correction. To be consistent the clu
C!      correction is determined from the corrected energy for BOTH PECO
C!      DECO.
C!
C!      The clustering correction should have been :
C!
C!          corr =   1. + a / Sqrt(E_raw) , a = 0.091 for Barrel
C!                                              0.102 for End-Caps
C!
C!      Instead a was taken as 0.046 for the Barrel and 0.071 for the En
C!
C! Called from QMEVNT
C?
C!======================================================================
#ifndef DOC
#include "qcde.h"
#include "evehjj.h"
#include "rhahjj.h"
#include "decojj.h"
      EXTERNAL CHAINT,NAMIND
      COMMON / JUVV / IJVER
      CHARACTER*4 CHAINT
      CHARACTER*8 PRNAM
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
#include "bmacro.h"
C ----------------------------------------------------------------------
      IJVER=0
      IF (INDEX(CQUNPK,'EC').EQ.0.AND.INDEX(CQUNPK,'AL').EQ.0)
     +    GO TO 999
      IF (FIRST) THEN
        NAPECO = NAMIND('PECO')
        NADECO = NAMIND('DECO')
        NARHAH = NAMIND('RHAH')
        NAEVEH = NAMIND('EVEH')
      ENDIF
      CORR=1.
C
C?   Initialisation
C
C
C When reading a POT/DST ( and not a MINI ! ) :
C Build the PECO 1 bank either  - if a data card 'PEC1' was provided
C                               - or if the ENFLW package was required
C
      IF (.NOT.XMINI) THEN
         IF ( IW(NAMIND('PEC1')) .GT.0 .OR.
     +      (XFILEF .AND. KEFOPT.NE.2 .AND. NLINK('EFOL',3).LE.0) )
     +      CALL QUPEC1
      ENDIF
      KRHAH = IW(NARHAH)
      KEVEH = IW(NAEVEH)
      KPECO = IW(NAPECO)
      JPEC1 = NLINK('PECO',1)
      IF (JPEC1.GT.0) KPECO = JPEC1
      KDECO = IW(NADECO)
C
      IF (KRHAH.EQ.0.OR.KEVEH.EQ.0 .OR. (KPECO.EQ.0.AND.KDECO.EQ.0) ) GO
     &  TO 999
      NPECO = LROWS(KPECO)
      NDECO = LROWS(KDECO)
      IF( NPECO.EQ.0 .AND. NDECO.EQ.0 ) GOTO 999
C
C
C?   Get Run Number, do nothing on monte-carlo events (irun<=2000)
C
      IRUN = IW(KEVEH+JEVERN)
      IEVT = IW(KEVEH+JEVEEV)
      IF ( IRUN.LE.2000 ) GO TO 999
C
C?   Get last version of julia used for processing this event.
C?   and eventually last alpha version in case of filtering
C?   For the alpha version take last raw - 1 in RHAH if FILO card exists
C
      ILVER = 0
      IAVER = 0
      IACOR = 0
      IMVER = MINGTV(DUM)
      NRHAH=LROWS(KRHAH)
      DO 10 IRHAH = 1, NRHAH
        PRNAM(1:4) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN))
        PRNAM(5:8) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN+1))
        IF (PRNAM.EQ.'JULIA') THEN
          IJVER = ITABL(KRHAH,IRHAH,JRHAPV)
          ILVER = ITABL(KRHAH,IRHAH,JRHAAV)
        ENDIF
        IF (PRNAM.EQ.'ALPHA') THEN
          IF ( IW(NAMIND('FILO')).EQ.0 .OR. IRHAH.NE.NRHAH ) THEN
            IAVER = ITABL(KRHAH,IRHAH,JRHAPV)
            IACOR = ITABL(KRHAH,IRHAH,JRHACV)
          ENDIF
        ENDIF
C       IF (PRNAM(1:4).EQ.'MINI') THEN
C         IF ( IW(NAMIND('FILO')).EQ.0 .OR. IRHAH.NE.NRHAH ) THEN
C           IMVER = ITABL(KRHAH,IRHAH,JRHAPV)
C         ENDIF
C       ENDIF
   10 CONTINUE
C
C?   Do the correction ?
C?   Correction made if :
C?                        Julia processing 258 with alephlib 133 and
C?                        no filtering with alpha version >= 113.9 and
C?                        not processed with Mini 80
C
      IF ( IJVER.EQ.258 .AND. ILVER.EQ.133 .AND.
     &  (IAVER.LE.112 .OR. (IAVER.EQ.113.AND.IACOR.LT.9)) .AND.
     &  IMVER.LT.80) THEN
        IF (FIRST) WRITE (IW(6),'(///,5(/,10x,a),///)')
     &'+------------------------------------------------------------+',
     &'|  Data Processed with Incorrect ECAL Clustering Correction  |',
     &'|      FIXPECO called to Fix PECO/DECO Banks                 |',
     &'|  !!!! Output banks are not equal to Input banks !!!!       |',
     &'+------------------------------------------------------------+'
        FIRST = .FALSE.
        IF ( KPECO.GT.0 ) THEN
C
C?   POT/DST bank
C
          DO 20 IPECO =  1,  NPECO
            THETA = RTABL (KPECO,IPECO,JPECTH)*RADEG
            ECOR  = RTABL (KPECO,IPECO,JPECEC)
            IF ( THETA.GT.40. .AND. THETA.LT.140. ) THEN
              CORR = (SQRT(ECOR) + 0.102) / (SQRT(ECOR) + 0.071)
            ELSEIF ( THETA.GT.10. .AND. THETA.LT.170. ) THEN
              CORR = (SQRT(ECOR) + 0.091) / (SQRT(ECOR) + 0.046)
            ENDIF
            RW (KROW(KPECO,IPECO)+JPECEC) =
     &        RW (KROW(KPECO,IPECO)+JPECEC) * CORR
   20     CONTINUE
        ENDIF
        IF ( KDECO.GT.0 ) THEN
C
C?   MINIs the correction is only an approximation
C?   (energies are in MeV, angles are in mrad/10)
C
          DO 30 IDECO = 1 ,  NDECO
            ECOR  = FLOAT(ITABL (KDECO,IDECO,JDECE0))/1000.
            THETA = FLOAT(ITABL (KDECO,IDECO,JDECTH))*RADEG/10000.
            IF ( THETA.GT.40. .AND. THETA.LT.140. ) THEN
              CORR = (SQRT(ECOR) + 0.102) / (SQRT(ECOR) + 0.071)
            ELSEIF ( THETA.GT.10. .AND. THETA.LT.170. ) THEN
              CORR = (SQRT(ECOR) + 0.091) / (SQRT(ECOR) + 0.046)
            ENDIF
            IW (KROW(KDECO,IDECO)+JDECE0) = NINT (
     &        IW (KROW(KDECO,IDECO)+JDECE0) * CORR )
   30     CONTINUE
        ENDIF
      ENDIF
C
  999 CONTINUE
C
      RETURN
      END
#endif
