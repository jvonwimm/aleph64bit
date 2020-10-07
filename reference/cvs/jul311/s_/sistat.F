      SUBROUTINE SISTAT
C----------------------------------------------------------------------
C!  - SICAL Accumulate job statistics
C!
C!   Author   :- B. Bloch                     24/08/92
C!              Modified for Bunch train mode October 1994 :
C!              Cumulate statistics per wagon
C!
C!  Input:       'SPDA' , 'SCLS'
C!
C!  Output:      'SSTA'
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
      INTEGER SIGTWA
      EXTERNAL SIGTWA
#include "spdajj.h"
#include "sclsjj.h"
      PARAMETER ( MASKM = 2048)
#include "bmacro.h"
C
C---------------------------------------------------------------------
C
      KSSTA = IW(NASSTA)
      IF(KSSTA.LE.0)                            GOTO 999
C- Count events with Sical Energy
      KSPDA = IW(NASPDA)
      IF(KSPDA.LE.0)                            GOTO 999
      IF(LROWS(KSPDA).LE.0)                     GOTO 999
C get wagon number
      NWA = SIGTWA(IDUM)
      ISTA = LMHLEN+(NWA-1)*LCOLS(KSSTA)+1
      RW(KSSTA+ISTA) = RW(KSSTA+ISTA)+1.
C- Energy and pads per module from SPDA
      DO 10 I = 1,LROWS(KSPDA)
        IDE = ITABL(KSPDA,I,JSPDSM)
        IF ( IDE.EQ.0) THEN
          IAD = ITABL(KSPDA,I,JSPDAD)-1
          MODU = 1
          IF ( IAND(IAD,MASKM).GT.0) MODU=2
          RW(KSSTA+ISTA+MODU)=RW(KSSTA+ISTA+MODU)+RTABL(KSPDA,I,JSPDEN)
          RW(KSSTA+ISTA+2+MODU)=RW(KSSTA+ISTA+2+MODU)+1.
        ENDIF
   10 CONTINUE
      ISTA = ISTA+4
C- Cluster energy and clusters per module from SCLS
      KSCLS = IW(NASCLS)
      IF(KSCLS.LE.0)                            GOTO 999
      NCLS = LROWS(KSCLS)
      IF(NCLS.LE.0)                             GOTO 999
      DO 20 I = 1,NCLS
        ZCL = RTABL(KSCLS,I,JSCLZC)
        MODU = 1
        IF ( ZCL.LT.0.) MODU=2
        RW(KSSTA+ISTA+MODU)=RW(KSSTA+ISTA+MODU)+RTABL(KSCLS,I,JSCLEC)
        RW(KSSTA+ISTA+2+MODU)=RW(KSSTA+ISTA+2+MODU)+1.
   20 CONTINUE
  999 RETURN
      END
#endif
