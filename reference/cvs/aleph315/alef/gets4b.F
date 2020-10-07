      SUBROUTINE GETS4B(IRUN,IFOUN,ILUQ,NBUN,NBHA,XLUSI,XBASI)
C----------------------------------------------------------------------
C! Gets the SICAL Luminosity per bunch for multibunch run IRUN
C  Author  J.Boucrot  10-Oct-1995
CKEY ALEF LFIL
C Input argument :
C  IRUN = run number
C Output arguments :
C   IFOUN = 0 if no information found for run IRUN
C         = 1 if information found in bank 'RS4B' of ADBSCONS DAF
C         = 2 if information found in run header bank 'SLUM'
C
C The following output arguments are defined ONLY if IFOUN.GT.0  :
C   ILUQ     = Luminosity determination status :
C                  1 = provisional
C                  2 = final
C   NBUN     = Number of bunches for this run ( must be 2 , 3 or 4 )
C   NBHA (I) = Number of SICAL Bhabha events for bunch I ( I=1,.NBUN )
C  XLUSI (I) = Best lumi estimate for run IRUN from SICAL , in nb**-1
C              for bunch I ( I=1,..NBUN )
C  XBASI (I) = Background estimate for bunch I ( I=1,..NBUN )
C
C   NBHA,XLUSI,XBASI must be dimensioned to 4 in the calling routine
C
C Description:
C gets the SICAL LUMI bunch informations from the 'RS4B' or 'SLUM' bank.
C ---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rs4bjj.h"
#include "slumjj.h"
      INTEGER ALGTRO,NBHA(*)
      REAL XLUSI(*),XBASI(*)
#include "bmacro.h"
C----------------------------------------------------------------------
      IFOUN=0
      ILUQR=0
      NBUN=0
      DO 1 I=1,4
         NBHA(I)=0
         XLUSI(I)=0.
         XBASI(I)=0.
 1    CONTINUE
C Try first on the ADBSCONS DAF bank 'RS4B' :
      JRS4B = ALGTRO ('RS4B',IRUN,JRS4RN,JROWR)
      IF (JRS4B.NE.0 .AND. JROWR.GT.0) THEN
C  RS4B bank found in ADBSCONS DAF : get the output arguments
         JRS4B = IABS (JRS4B)
         KRS4B = KROW(JRS4B,JROWR)
         ILUQR = IW(KRS4B+JRS4LQ)
         NBHA(1)  = IW(KRS4B+JRS4B1)
         XLUSI(1) = RW(KRS4B+JRS4L1)
         XBASI(1) = RW(KRS4B+JRS4K1)
         NBHA(2)  = IW(KRS4B+JRS4B2)
         XLUSI(2) = RW(KRS4B+JRS4L2)
         XBASI(2) = RW(KRS4B+JRS4K2)
         NBHA(3)  = IW(KRS4B+JRS4B3)
         XLUSI(3) = RW(KRS4B+JRS4L3)
         XBASI(3) = RW(KRS4B+JRS4K3)
         NBHA(4)  = IW(KRS4B+JRS4B4)
         XLUSI(4) = RW(KRS4B+JRS4L4)
         XBASI(4) = RW(KRS4B+JRS4K4)
C There must be at least 2 significant bunches :
         DO 10 IK=1,4
 10      IF (NBHA(I).GT.0.AND.XLUSI(I).GT.0.) NBUN=NBUN+1
         IF (NBUN.LE.1) GO TO 999
         IFOUN=1
C
C  Nothing found in RS4B : try the run header bank 'SLUM' :
C
      ELSE
         JSLUM=NLINK('SLUM',IRUN)
         IF (JSLUM.GT.0) THEN
            NSLUM=LROWS(JSLUM)
            DO 100 IK=4,NSLUM,2
               II=IK/2-1
               NBHA(II)  = RTABL (JSLUM,IK,JSLUTI)
               XLUSI(II) = RTABL (JSLUM,IK,JSLULU)
               IF (NBHA(II).GT.0.AND.XLUSI(II).GT.0.) NBUN=NBUN+1
 100        CONTINUE
C There must be at least 2 significant bunches :
            IF (NBUN.LE.1) GO TO 999
            IFOUN = 2
            ILUQR=1
         ENDIF
      ENDIF
C
 999  RETURN
      END
#endif
