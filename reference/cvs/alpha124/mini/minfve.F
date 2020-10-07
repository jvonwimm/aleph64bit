      SUBROUTINE MINFVE
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill FVER from DVMC.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : DVMC bank
C     Output : FVER bank
C
C     Called by MINFIL
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "fverjj.h"
#include "dtmcjj.h"
#include "dvmcjj.h"
#include "alcons.h"
#include "minfac.h"
C
C++   Speed of light in cm/s.
C
      PARAMETER (CCMPS=1.E9*CLGHT)
C
C++   Labels for vertex mechanism.
C
      PARAMETER (NVMEC=31)
      DIMENSION VMEC(NVMEC),IVMEC(NVMEC)
      CHARACTER*4 VMEC
      DATA VMEC /
     &  'NEXT','MULS','LOSS','FIEL','DCAY','PAIR','COMP','PHOT',
     &  'BREM','DRAY','ANNI','HADR','ECOH','EVAP','FISS','ABSO',
     &  'ANNH','CAPT','EINC','INHE','MUNU','TOFM','PFIS','SCUT',
     &  'RAYL','NONE','PRED','LOOP','NULL','STOP','SHOW' /
      LOGICAL FIRST
      SAVE FIRST,IVMEC,NULL
      DATA FIRST,IVMEC,NULL / .TRUE.,NVMEC*0,0 /
C
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up DVMC bank.
C
      KDVMC = NLINK('DVMC',0)
      IF (KDVMC.LE.0) RETURN
      NDVMC = LROWS(KDVMC)
      IF(NDVMC.LE.0) RETURN
C
C++   Initialisation for vertex mechanism labels.
C
      IF(FIRST) THEN
         DO I=1,NVMEC
            IVMEC(I) = INTCHA(VMEC(I))
         ENDDO
         NULL = INTCHA('    ')
         FIRST = .FALSE.
      ENDIF
C
C++   Create the FVER bank.
C
      LEN = LMHLEN + LFVERA * NDVMC
      CALL AUBOS('FVER',0,LEN, KFVER,IGARB)
      CALL BLIST(IW,'S+','FVER')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDVMC = NLINK('DVMC',0)
      ENDIF
      IW(KFVER+LMHCOL) = LFVERA
      IW(KFVER+LMHROW) = NDVMC
C
C++   Fill FVER bank.
C
      DO I=1,NDVMC
         RW(KROW(KFVER,I)+JFVEVX) = FLOAT(ITABL(KDVMC,I,JDVMVX))/DFACTM
         RW(KROW(KFVER,I)+JFVEVY) = FLOAT(ITABL(KDVMC,I,JDVMVY))/DFACTM
         RW(KROW(KFVER,I)+JFVEVZ) = FLOAT(ITABL(KDVMC,I,JDVMVZ))/DFACTM
C        CT = FLOAT(ITABL(KDVMC,I,JDVMTO))/DFACTM
C        RW(KROW(KFVER,I)+JFVETO) = CT / CCMPS
         IW(KROW(KFVER,I)+JFVEIP) = ITABL(KDVMC,I,JDVMIP)
         IW(KROW(KFVER,I)+JFVEVN) = ITABL(KDVMC,I,JDVMVN)
C
C++      Reassign the vertex mechanism.
C
         LABEL = ITABL(KDVMC,I,JDVMVM)
         IF(LABEL.EQ.0) THEN
            JVMEC = NULL
         ELSE
            IF(LABEL.GT.0 .AND. LABEL.LE.NVMEC) THEN
               JVMEC = IVMEC(LABEL)
            ELSE
               JVMEC = LABEL
            ENDIF
         ENDIF
         IW(KROW(KFVER,I)+JFVEVM) = JVMEC
      ENDDO
C
C++   Reconstruct the relationships to the FKIN bank.
C++   The code to identify the mother of the vertex from the DTMC (FKIN)
C++   bank is not used since some tracks end at more than one vertex.
C
      KDTMC = NLINK('DTMC',0)
      IF(KDTMC.LE.0) RETURN
      NDTMC = LROWS(KDTMC)
      IF(NDTMC.LE.0) RETURN
C
      DO 10 I=1,NDTMC
         IORIG = ITABL(KDTMC,I,JDTMOV)
         IF (IORIG.LE.0.OR.IORIG.GT.NDVMC) GO TO 10
         IF(ITABL(KFVER,IORIG,JFVENS).EQ.0) THEN
            IW(KROW(KFVER,IORIG)+JFVEIS) = I - 1
         ENDIF
         IW(KROW(KFVER,IORIG)+JFVENS) = IW(KROW(KFVER,IORIG)+JFVENS) + 1
 10   CONTINUE
C
      RETURN
      END
#endif
