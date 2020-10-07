      SUBROUTINE GAPGID
C----------------------------------------------------------------------
C!  - Build PGID bank (result from EBNEUT )
CKEY PHOTONS PGID
C!   Author   :- MN Minard             27-JAN-1993
C!======================================================================
#ifndef DOC
      DIMENSION IOPT(6),IFOT(19),PHOT(23),CORE(6)
      DIMENSION CMINFO(16)
      DIMENSION XGER(3),XVXR(3),DX(3)
#include "alcons.h"
#include "bcs.h"
#include "pgidjj.h"
#include "pecojj.h"
#include "egtejj.h"
#include "pcrljj.h"
      INTEGER ALGTDB,GTSTUP
      CHARACTER DET*2, LIST*4
      DATA NAEGTE/0/, IROLD/0/, DET/'EC'/,LIST/'EGTE'/
      DATA NAPECO,NAPCRL /2*0/
#include "bmacro.h"
C
C-   Initialise pointers
C
      IF(NAEGTE.EQ.0) THEN
        NAPECO = NAMIND('PECO')
        NAEGTE = NAMIND('EGTE')
        NAPCRL = NAMIND('PCRL')
      ENDIF
C
C-   Initialise threshold
C
#include "gtdbbk.h"
      ETHGAM = 0.
      KEGTE = IW(NAEGTE)
      NEGTE = 0
      IF ( KEGTE.NE.0) NEGTE = LROWS(KEGTE)
      DO IEGTE = 1,NEGTE
        IF(ITABL(KEGTE,IEGTE,2).EQ.1) THEN
          ETHGAM = RTABL(KEGTE,IEGTE,JEGTET)
        ENDIF
      ENDDO
C
C-   Define vertex
C
      CALL VZERO(XVXR(1),3)
      CALL EVTVER (X,Y,Z)
      XVXR(1) = X
      XVXR(2) = Y
      XVXR(3) = Z
C
C    Built PGID bank
C
      KPECO = IW(NAPECO)
      KPCRL = IW(NAPCRL)
C
C    Cluster bank present ?
C
      NCLU = 0
      IF(KPECO.LE.0) GO TO 999
      NCLU = LROWS(KPECO)
      K = NDROP('PGID',0)
      KMAX = LMHLEN + LPGIDA * NCLU
      CALL AUBOS('PGID',0,KMAX,KPGID,IGARB)
      IF ( IGARB.EQ.2 ) GO TO 999
      IW(KPGID+LMHROW) = 0
      IW(KPGID+LMHCOL) = LPGIDA
      IF ( IGARB.NE.0 ) THEN
        KPECO = IW(NAPECO)
        KPRCL = IW(NAPCRL)
      ENDIF
C
C-  Loop over cluster
C
      IGAM = 0
      DO 1 ICLN = 1, NCLU
         EC = RTABL(KPECO,ICLN,JPECEC)
         KCODE = ITABL(KPECO,ICLN,JPECKD)
         IF ( KCODE.LT.192) THEN
         ICH = ITABL(KPECO,ICLN,JPECRB)
C
C-   Remove charged clusters
C
         IF( ICH.NE.0.AND.ICH.NE.2) GO TO 1
C
C-   Take cluster above threshold
C
         IF ( EC.LT.ETHGAM) GO TO 1
C
C-    Cluster treated by EBNEUT
C
         CALL GBNEUT(ICLN,IOPT,IFOT,PHOT,CORE,IER)
C-
         KFLG = 0
         IF(IER.GT.10) KFLG = 10
         IF ( IFOT(14).EQ.11 ) KFLG = 1
         IF ( IFOT(14).EQ.12 ) KFLG = 2
         IF ( IFOT(9).EQ.1.OR.IFOT(9).EQ.6) KFLG = 3
         IF ( IFOT(9).GE.2.AND.IFOT(9).LE.5) KFLG = 4
         IF ( IER.LT.-2.AND.IER.GT.-6) KFLG = 4
         IGAM = IGAM + 1
         JPGID = KROW(KPGID,IGAM)
         IW(JPGID+JPGIIF) = KFLG
         CALL ENOL12(PHOT(7),PHOT(8),EC,PHOT(5),ESL1N,ESL2N)
         RW(JPGID+JPGIDE) = PHOT(7)
         CALL ENOF4(PHOT(12),EC,PHOT(5),ESF4N)
         RW(JPGID+JPGICM) = ESF4N
C
C-    Perform moment analysis
C
         CALL CLMOMS(ICLN,NIMP,CMINFO,IWARN,IERROR,EC)
         CALL ENOW12(CMINFO(1),CMINFO(2),EC,PHOT(5),ESW1N,ESW2N)
         IF( IERROR.EQ.0) THEN
           RW(JPGID+JPGIM1) = ESW1N
           RW(JPGID+JPGIM2) = ESW2N
           RW(JPGID+JPGIM3) = CMINFO(5)
         ENDIF
         CALL GVERCR(ICLN,RAD,IER)
         RW(JPGID+JPGIPH) = PHOT(6)
         RW(JPGID+JPGITH) = PHOT(5)
         RW(JPGID+JPGICE) = PHOT(14)
         IF ( IER.EQ.0) THEN
         DR = 0
         XGER(1) = RAD * COS(PHOT(6))* SIN(PHOT(5))
         XGER(2) = RAD * SIN(PHOT(6))* SIN(PHOT(5))
         XGER(3) = RAD * COS(PHOT(5))
         DO IV =1,3
           DX (IV) = XGER(IV)-XVXR(IV)
         ENDDO
         DXER = SQRT(DX(1)**2+DX(2)**2)
         IF ( DXER .GT. 0 ) THEN
         XPHIX = ATAN2(DX(2),DX(1))
         IF (XPHIX.LT.0.) XPHIX = XPHIX + TWOPI
         RW(JPGID+JPGIPH) = XPHIX
         RW(JPGID+JPGITH) = ATAN2(DXER,DX(3))
         ENDIF
         ENDIF
         IW(JPGID+JPGIPE) = ICLN
        ENDIF
 1    CONTINUE
      IW(KPGID+LMHROW) = IGAM
      CALL AUBPRS('PGID')
 999  CONTINUE
      RETURN
      END
#endif
