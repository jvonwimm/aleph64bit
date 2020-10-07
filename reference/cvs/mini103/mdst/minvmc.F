      SUBROUTINE MINVMC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill vertex MC bank DVMC for Mini-DST.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : FVER bank
C     Output : DVMC bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "fverjj.h"
#include "dvmcjj.h"
#include "minfac.h"
#include "alcons.h"
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
      SAVE FIRST,IVMEC,NULL,CTMAX
      DATA FIRST,IVMEC,NULL / .TRUE.,NVMEC*0,0 /
      DATA CTMAX / 1000. /
C
#include "bmacro.h"
C
C++   Pick up FVER bank.
C
      KFVER = NLINK('FVER',0)
      IF (KFVER.LE.0) RETURN
      NFVER = LROWS(KFVER)
      IF(NFVER.LE.0) RETURN
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
C++   Create the DVMC bank.
C
      LEN = LMHLEN + LDVMCA * NFVER
      CALL AUBOS('DVMC',0,LEN, KDVMC,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINVMC: Cannot create DVMC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KFVER = NLINK('FVER',0)
      ENDIF
      IW(KDVMC+LMHCOL) = LDVMCA
      IW(KDVMC+LMHROW) = NFVER
C
C++   Copy FVER to DVMC, integerising positions.
C++   Time of flight is stored as a distance.
C++   The track relation information is dropped since it is contained
C++   in DTMC.
C
      DO I=1,NFVER
         IW(KROW(KDVMC,I)+JDVMVX) = NINT(DFACTM * RTABL(KFVER,I,JFVEVX))
         IW(KROW(KDVMC,I)+JDVMVY) = NINT(DFACTM * RTABL(KFVER,I,JFVEVY))
         IW(KROW(KDVMC,I)+JDVMVZ) = NINT(DFACTM * RTABL(KFVER,I,JFVEVZ))
C        CT = CCMPS * RTABL(KFVER,I,JFVETO)
C        IF(CT.GT.CTMAX) CT = CTMAX
C        IW(KROW(KDVMC,I)+JDVMTO) = NINT(DFACTM * CT)
         IW(KROW(KDVMC,I)+JDVMIP) = ITABL(KFVER,I,JFVEIP)
         IW(KROW(KDVMC,I)+JDVMVN) = ITABL(KFVER,I,JFVEVN)
C
C++      Represent the vertex mechanism by an integer corresponding to
C++      list VMEC. New mechanisms are represented by the integer
C++      form of their character string.
C
         IF(LCOLS(KFVER).GE.JFVEVM) THEN
            LABEL = ITABL(KFVER,I,JFVEVM)
            IF(LABEL.EQ.NULL) THEN
               JVMEC = 0
            ELSE
               INDX = IUCOMP(LABEL,IVMEC,NVMEC)
               IF(INDX.GT.0) THEN
                  JVMEC = INDX
               ELSE
                  JVMEC = LABEL
               ENDIF
            ENDIF
         ELSE
            JVMEC = 0
         ENDIF
         IW(KROW(KDVMC,I)+JDVMVM) = JVMEC
      ENDDO
C
C++   Add the bank to the Mini list.
C++   Since the format includes an 'A' it is explicitly envoked.
C
      CALL MINLIS('DVMC')
      CALL BKFMT('DVMC','2I,(4I,A,I)')
C
      RETURN
      END
#endif
