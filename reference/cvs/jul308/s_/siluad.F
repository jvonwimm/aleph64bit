      REAL FUNCTION SILUAD(IM,ICUT)
C----------------------------------------------------------------------
C!  - Fill bank SILUM by increasing counts in requested row IM ,col ICUT
C!
C!
C!    Author:     B. Bloch-Devaux   11-May-1993
C!    Modified:   B. Bloch 18/05/94 take downscale factor into account
C!              Modified for Bunch train mode October 1994
C!              Modified to take into account energy dependence of the
C!              cross-section  November 1995
C!
C!    Input:      - IM : method number [1,2]
C!                - ICUT :cut number
C!    Output:
C!                - SILUAD =  0.  OK
C!                - SILUAD = -1.  Problem?
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "sinamc.h"
#include "sbhajj.h"
#include "silhjj.h"
      DIMENSION SIG(2),SIGV(2),SIGH(2),ERSY(2)
      INTEGER SIGTWA,ALGTWA
      EXTERNAL SIGTWA,ALGTWA
      DATA SIG /  104.147 ,84.1019  /
      DATA SIGV/   38.9451,31.4237  /
      DATA SIGH/   39.0950,31.6103  /
      DATA ISDWN/   1 /
      DATA IFIR /0/
#include "bmacro.h"
      IF ( IFIR.EQ.0 ) THEN
         IFIR = 1
         NAKEVH = NAMIND('KEVH')
         KSBHA = IW ( NASBHA)
         IF ( KSBHA.GT.0) THEN
            DO 10 IMETHOD = 1 ,2
C   check the Energy corresponding to the given cross-section
              EGENE = 2.*RTABL(KSBHA,IMETHOD,JSBHEG)
              COE = (EGENE/RENLEP)**2
C   Correct cross-section for 1/s variation
              SIG(IMETHOD)  = COE*RTABL(KSBHA,IMETHOD,JSBHCS)
              SIGH(IMETHOD) = COE*RTABL(KSBHA,IMETHOD,JSBHXH)
              SIGV(IMETHOD) = COE*RTABL(KSBHA,IMETHOD,JSBHXV)
              ERSY(IMETHOD) = RTABL(KSBHA,IMETHOD,JSBHER)
 10         CONTINUE
         ENDIF
         KSILH = IW ( NASILH)
         IF ( KSILH.GT.0) ISDWN = ITABL(KSILH,1,JSILDW+1)
         NWA = ALGTWA(IRUNRC)
         IF ( NWA.GT.0) ISDWN = ISDWN * NWA
      ENDIF
C Event weight
      W = 1.
C  If coincidence trigger was downscaled , take it into account
      IF ( ISDWN.GT.1 ) W = FLOAT(ISDWN)
      KKEVH = IW(NAKEVH)
      IF(KKEVH.GT.0) W = RTABL(KKEVH,1,5)
      SILUAD =-1.
C Access SLUM bank
      JSLUM = IW(NASLUM)
      IF ( JSLUM.LE.0) GO TO 999
C get wagon number
      NWA = SIGTWA(IDUM)
      KPOI = KROW(JSLUM,2*NWA+IM)
      KPOIT= KROW(JSLUM,IM)
      IF ( ICUT.LE.9) THEN
         RW(KPOI+ICUT) = RW(KPOI+ICUT) + W
         RW(KPOIT+ICUT) = RW(KPOIT+ICUT) + W
      ELSE IF ( ICUT.EQ.10 ) THEN
         RW(KPOI+10) = RW(KPOI+ICUT)+ W/SIG(IM)
         RW(KPOI+13) = SQRT(RW(KPOI+10)*ISDWN/SIG(IM))
         RW(KPOI+14) = RW(KPOI+10)*ERSY(IM)
         RW(KPOIT+10) = RW(KPOIT+ICUT) + W/SIG(IM)
         RW(KPOIT+13) = SQRT(RW(KPOIT+10)*ISDWN/SIG(IM))
         RW(KPOIT+14) = RW(KPOIT+10)*ERSY(IM)
      ELSE IF ( ICUT.EQ.11 ) THEN
         RW(KPOI+11) = RW(KPOI+ICUT)+ W/SIGV(IM)
         RW(KPOIT+11) = RW(KPOIT+ICUT) + W/SIGV(IM)
      ELSE IF ( ICUT.EQ.12 ) THEN
         RW(KPOI+12) = RW(KPOI+ICUT)+ W/SIGH(IM)
         RW(KPOIT+12) = RW(KPOIT+ICUT) + W/SIGH(IM)
      ENDIF
      SILUAD = 0.
 999  RETURN
      END
#endif
