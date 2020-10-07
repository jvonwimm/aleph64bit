      SUBROUTINE VCALCM
C
C----------------------------------------------------------------------
C!  Removes coherent noise from hits
C
C   Computes the Common Mode independently for each cluster belonging
C   to a particular wafer stored in IPULS
C   CM is defined by an iterative procedure which removes all elements
C   higher or lower than the average plus or minus given numbers of
C   standard deviations.
C   The maximum element is removed at entry upon the assumption that it,
C   at least, constitutes the signal.
C   Convergence can be achieved either when no more elements are removed
C   by a cycle or a number of elements smaller than a given value are
C   left in the cluster.
C   Author : C. Vannini 1/4/90
C
C  Modified 15-1-94 Dave Brown for 95 VDET
C  Modified March 1995 A. Bonissent, M. Thulasidas
C                 reorganise and debug
C-----------------------------------------------------------------------
C
C
C   Internal variables:
C      REAL CMODE             ! Common mode computed on a subset of PHIN
C      REAL CSIGM             ! Std. Dev. of the NCHAN elements of PHIN
C      LOGICAL RETRY          ! Used in the iterative calculation of CM
C      REAL HIGTR             ! Pulses above this value are discarded
C                             ! under the assumption that "it's signal"
C      REAL LOWTR             ! Pulses below this value are discarded
C                             ! as too negative spikes "are not real"
C?
C!======================================================================
#ifndef DOC
#include "vdflgs.h"
#include "vcsgjj.h"
#include "vrecon.h"
#include "vprtnc.h"
      INTEGER NVCSG,IVCSG
      INTEGER IMAX, MXVAL
      INTEGER ICM, ICS
      INTEGER NAVCSG,NAMIND,KVCSG,IFLG,KLIN,KLMX
      INTEGER IPULS
      INTEGER JSTRP,FSTRP,LSTRP,MSTRP,NUSED
      REAL PULS
      REAL CMODE, CSIGM, DUMMY
      REAL HIGTR, LOWTR
      LOGICAL OUTLY, FIRST
C
C  Parameters- these define the flag conditions for channels not to be
C  used in computing the common mode, and those not to be adjusted for
C  the common mode
C
      INTEGER MSNCM,MSCNG
      PARAMETER (MSNCM = VBUNBD+VBUNUS+VBOVER+VBSUPP+VBHOTC)
C-
C- Dave B. wanted it to be :
C--      PARAMETER (MSCNG = VBUNBD+VBUNUS)
C- But since we do not understand it,
C  we restored what it was in old code (gives same results as before)
C- Manoj 13-feb-1995
C-
      PARAMETER (MSCNG = VBUNBD+VBUNUS+VBOVER)
      DATA FIRST /.TRUE./
C
C  Inline functions- bit comparisons
C  NCMFL .TRUE. means the channel shouldn't be used for common mode comp
C  NCHNG .TRUE. means the channel value shouldn't be changed.
C
      LOGICAL NCMFL, NCHNG, IHIS1
      INTEGER INPUT
      INTEGER IAND,ISHFT
#include "bcs.h"
#include "bmacro.h"
      NCMFL(INPUT) = IAND(INPUT,MSNCM) .GT. 0
      NCHNG(INPUT) = INPUT.EQ.0.OR.IAND(INPUT,MSCNG).GT.0
      IHIS1(INPUT) = IAND(INPUT,1) .EQ. 1
C----------------------------------------------------------------------
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVCSG=NAMIND('VCSG')
      ENDIF
      KVCSG=IW(NAVCSG)
      NVCSG=LROWS(KVCSG)
      MSTRP=0
      MXVAL  = -999
C
C  Loop over the wafer, finding the clusters
C
      DO 100 IVCSG=1,NVCSG
C
C  Look for non-blank strips
C
        IFLG = ITABL(KVCSG,IVCSG,JVCSMF)
        IF(IFLG.NE.0)THEN
           KLIN=KROW(KVCSG,IVCSG)
C
C If there is no current cluster, we start one. otherwise
C keep extending it
C
           IF(MSTRP.EQ.0)THEN
             FSTRP=IVCSG
C-- need to initialize NUSED
             NUSED = 0
           ENDIF
           LSTRP=IVCSG
           MSTRP=MSTRP+1
           IF (NCMFL(IFLG)) THEN
             IW(KLIN+JVCSRF) = 1
           ELSE
             IW(KLIN+JVCSRF) = 0
             NUSED = NUSED + 1
             IPULS=  RTABL(KVCSG,IVCSG,JVCSMP)
             IF (IPULS.GT.MXVAL) THEN
               MXVAL = IPULS
               IMAX = IVCSG
             ENDIF
           ENDIF
        ELSEIF(MSTRP.NE.0)THEN
C
C  Here we finish a cluster
C
C
C   Remove maximum
C
          IW(KROW(KVCSG,IMAX)+JVCSRF) = 1
          NUSED =  NUSED-1
C
C  Histogram
C
          IF(IHIS1(HISTLV))THEN
            CALL HFILL(HISOFF+42,FLOAT(NUSED),DUMMY,1.0)
          END IF
C
C   Iterate common mode computation, as long as outlying pulseheights
C   and enough channels remain
C
          OUTLY = .TRUE.
C
C--  need to initialize the common mode in case the while
C--  is not entered.
C
          CMODE = 0.0
          CSIGM = 0.0
C  Do While accepted in JULIA as now works on all compilers
C                                               (P. Comas, May 1995)
          DO WHILE(OUTLY.AND.NUSED.GT.MINCMD)
C
C   Compute mean on currently enabled cluster elements.
C
            CALL VCMEAN (FSTRP,MSTRP,CMODE,CSIGM)
C
C  Count the channels inside the limits
C
            NUSED = 0
            OUTLY = .FALSE.
            HIGTR = CMODE + HCUTCM*CSIGM
            LOWTR = CMODE - LCUTCM*CSIGM
            DO JSTRP = FSTRP, LSTRP
              IF (ITABL(KVCSG,JSTRP,JVCSRF).EQ.0) THEN
                PULS = RTABL(KVCSG,JSTRP,JVCSMP)
                IF (PULS .GT. HIGTR. OR. PULS .LT. LOWTR) THEN
                  IW(KROW(KVCSG,JSTRP)+JVCSRF) = 1    ! Outlyer
                  OUTLY = .TRUE.
                ELSE
                  NUSED = NUSED + 1
                ENDIF
              ENDIF
            END DO
          END DO
C
C   Subtract the CM from the input pulse-heights of good strips
C
          ICM   = NINT(CMODE)
          ICS   = NINT(CSIGM)
          DO JSTRP = FSTRP, LSTRP
            KLIN = KROW(KVCSG,JSTRP)
            IF (.NOT.NCHNG(ITABL(KVCSG,JSTRP,JVCSMF))) THEN
              RW(KLIN + JVCSMP) = RTABL(KVCSG,JSTRP,JVCSMP) - ICM
            END IF
C
C  Pack common mode; offset avoids problems with negative numbers
C
            IW(KLIN + JVCSCM) = IPHOFF + ICM + ISHFT(ICS,16)
          END DO
C
C  Flag noisy strips
C
          IF(ICS .GT. MXSIGM)THEN
            NNOSHT(1) = NNOSHT(1) + 1
            DO JSTRP = FSTRP, LSTRP
              IW(KROW(KVCSG,JSTRP)+JVCSMF) =
     >           IOR(ITABL(KVCSG,JSTRP,JVCSMF),VBNOIS)
            END DO
          END IF
C
C  Histograms
C
          IF(IHIS1(HISTLV))THEN
            CALL HFILL(HISOFF+40,CMODE,DUMMY,1.0)
            CALL HFILL(HISOFF+41,CSIGM,DUMMY,1.0)
            CALL HFILL(HISOFF+43,FLOAT(NUSED),DUMMY,1.0)
          END IF
C
C Say that we have no cluster started
C and that the maximum value is very small
C
          MSTRP=0
          MXVAL=-999
        END IF
  100 CONTINUE
      RETURN
      END
#endif
