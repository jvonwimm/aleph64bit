      SUBROUTINE VSEPAR(PHCUT,IFST,NSTRP,NCLU,ISTRT,NSCLU)
C
C----------------------------------------------------------------------
C!  - Separate merged VDET hits
C!
C!   Author   :- Dave Brown 18-8-91.
C!
C!  This takes as input a VFHL cluster list of pulseheights,
C!  and checks to see if they are consistent with coming from a
C!  single cluster.  If not, it attempts to separate into separate
C!  hits.
C!
C! This routine is an adaptation of the one extracted from old code.
C! A newer and better version was written by D. Brown, but the
C! algorithms were slightly changed. For the moment, and for best
C! compatibility with the old code, we use this.
C! A. Bonissent January 1995
C!
C!  Inputs;    RPULS = Array of pulseheights
C!             NVCSG = Number of strips in array
C!             PHCUT = Minimum PH difference to define a significant val
C!  Outputs;   NCLU  = Number of clusters found (>=1 normally, 0=error)
C!             ISTRT = Starting strip number for each cluster
C!             NSCLU = Number of strips for each cluster
C!
C!   NOTE: in case a strip is shared by 2 clusters, it's PH value will
C!   be REDUCED BY HALF in this routine.
C!
C?
C!======================================================================
#ifndef DOC
C      IMPLICIT NONE
      SAVE FIRST, NAVCSG, NAVPLH, NAVTRS
C
      INTEGER NAVCSG,KVCSG,NVCSG,IVCSG
      INTEGER NAVPLH,NAVTRS
      INTEGER IFST,NSTRP
      INTEGER NAMIND
      INTEGER JVCOCM,LVCOMA
      INTEGER JVDXXC,JVDXYC,JVDXUC,JVDXSX,JVDXSY,JVDXSU,
     +          JVDXPH,JVDXQF,JVDXNA,JVDXIP,JVDXIW,
     +          JVDXIH,LVDXYA
      INTEGER JVDZZC,JVDZWC,JVDZSZ,JVDZSW,JVDZPH,JVDZQF,
     +          JVDZNA,JVDZIP,JVDZIW,JVDZIH,LVDZTA
      INTEGER JVFHHA,LVFHLA
      INTEGER JVFLFG,LVFLGA
      INTEGER JVFPPH,LVFPHA
      INTEGER JVHOHA,LVHOTA
      INTEGER JVMGWA,JVMGMN,JVMGM1,JVMGM2,JVMGWG,LVMGNA
      INTEGER JVMMWA,JVMMMN,JVMMM1,JVMMM2,JVMMC1,JVMMC2,
     +          JVMMC3,JVMMC4,JVMMC5,JVMMC6,JVMMC7,
     +          JVMMC8,JVMMWC,JVMMST,LVMMOA
      INTEGER JVMPCA,JVMPSF,LVMPCA
      INTEGER JVMRFC,JVMRLC,JVMRCS,JVMRRF,LVMREA
      INTEGER JVRCMX,JVRCMF,JVRCMH,JVRCMN,JVRCPU,JVRCCM,JVRCMU,
     +   JVRCMS,JVRCFS,JVRCHC,JVRCLC,JVRCMZ,JVRCMP,LVRCNA
      INTEGER JVTCEL,JVTCES,JVTCIU,JVTCOU,JVTCLW,JVTCOW,
     +          JVTCUN,JVTCWN,JVTCMS,JVTCAC,JVTCDF,
     +          JVTCNP,JVTCRH,LVTCEA
      INTEGER JVTXWI,JVTXHF,JVTXUC,JVTXWC,JVTXSU,JVTXSW,
     +          JVTXUW,JVTXXC,JVTXYC,JVTXZC,JVTXPV,
     +          JVTXPU,JVTXPW,JVTXUR,JVTXUT,JVTXUP,
     +          JVTXUD,JVTXUZ,JVTXWR,JVTXWT,JVTXWP,
     +          JVTXWD,JVTXWZ,LVTXTA
C
      INTEGER MAXST
      PARAMETER (MAXST = 50)
      REAL RPULS(MAXST),PHCUT,AVEPH
      INTEGER ISTRP,JSTRP,KSTRP,NSEP,NCLU,ISTRT(*),NSCLU(*)
      INTEGER INDEX(MAXST),ISEED(3,MAXST)
      INTEGER IEDGE,ICLU,JCLU,IMIN,IIII,IDIF, KLINE
      LOGICAL LEVEN
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "vcsgjj.h"
#include "bcs.h"
C      INTEGER LCOLS,LROWS,ID,KNEXT,KROW,NRBOS,LFRWRD,LFRROW,ITABL,L
C      REAL RTABL
#include "bmacro.h"
C
C  Inline functions
C
      LEVEN(IIII) = IAND(IIII,1) .EQ. 0
      IF(FIRST)THEN
         NAVCSG=NAMIND('VCSG')
         FIRST=.FALSE.
      ENDIF
      KVCSG=IW(NAVCSG)
      NVCSG=LROWS(KVCSG)
C
C  Check size
C
      IF(NSTRP .GT. MAXST)THEN
        NCLU = 0
        RETURN
      END IF
      DO ISTRP=1,NSTRP
         IVCSG=IFST+ISTRP-1
         RPULS(ISTRP)=RTABL(KVCSG,IVCSG,JVCSMP)
      ENDDO
C
C  First, order the pulseheights
C
C     CALL SORTZV(RPULS,INDEX,NSTRP,1=real,1=descending,0=normal sort)
      CALL SORTZV(RPULS,INDEX,NSTRP,1,1,0)
C
C  The highest pulseheight is the first 'seed'
C
      NCLU = 1
      DO 20 IEDGE=1,3
        ISEED(IEDGE,NCLU) = INDEX(1)
20    CONTINUE
C
C  Loop over the remaining strips
C
      DO 10 ISTRP=2,NSTRP
        JSTRP = INDEX(ISTRP)
C
C  Loop over the current seeds
C
        DO 11 ICLU=1,NCLU
C
C  Is this strip adjacent to this cluster? If so, add it to that cluster
C  and continue to the next strip
C
          DO 12 IEDGE=1,3,2
            IF(ABS(JSTRP-ISEED(IEDGE,ICLU)).EQ.1)THEN
              ISEED(1,ICLU) = MIN(JSTRP,ISEED(1,ICLU))
              ISEED(3,ICLU) = MAX(JSTRP,ISEED(3,ICLU))
              GOTO 10
            END IF
12        CONTINUE
11      CONTINUE
C
C  If we get to here, this strip isn't adjacent; see how far it is from
C  nearest cluster center.  If the strip is equi-distant to 2 clusters,
C  it will take the one with the largest seed strip (first in line).
C
        IMIN = 1000
        DO 13 ICLU=1,NCLU
          IF(ABS(JSTRP-ISEED(2,ICLU)).LT.IMIN)THEN
            IMIN = ABS(JSTRP-ISEED(2,ICLU))
            JCLU = ICLU
          END IF
13      CONTINUE
C
C  If the separation is statistically significant, create a new seed.
C
        AVEPH = 0.0
        NSEP = 0
        DO 31 KSTRP=MIN(JSTRP,ISEED(2,JCLU))+1,
     &              MAX(JSTRP,ISEED(2,JCLU))-1
          AVEPH = AVEPH + RPULS(KSTRP)
          NSEP = NSEP + 1
31      CONTINUE
        AVEPH = AVEPH/NSEP
        IF(RPULS(JSTRP)-AVEPH .GT. PHCUT/SQRT(FLOAT(NSEP)))THEN
          NCLU = NCLU + 1
          DO 21 IEDGE=1,3
            ISEED(IEDGE,NCLU) = JSTRP
21        CONTINUE
        ELSE
C
C  Otherwise, add it to the closest cluster IF it is also adjacent
C  If the strip isn't adjacent, it's simply lost, but will be recovered
C  in the final separation process
C
          IF(ABS(JSTRP-ISEED(1,JCLU)) .EQ. 1 .OR.
     &       ABS(JSTRP-ISEED(3,JCLU)) .EQ. 1 )THEN
            IF(JSTRP .GT. ISEED(2,JCLU))THEN
              ISEED(3,JCLU) = JSTRP
            ELSE
              ISEED(1,JCLU) = JSTRP
            END IF
          END IF
        END IF
10    CONTINUE
C
C  All the clusters are separated; determine the boundary positions
C  between them.
C
      IF(NCLU .EQ. 1)THEN
        ISTRT(1) = 1
        NSCLU(1) = NSTRP
      ELSE
C
C  First, order the clusters by the position of their seeds
C
C       CALL SORTIQ(ISEED,3=# of col,NCLU,+2=col of seed PH, sort asscen
        CALL SORTIQ(ISEED,3,NCLU,2)
C
C  Now loop over the clusters, setting the starting strip and ending str
C  If a strip is shared by 2 adjacent clusters, the starting and ending
C  strips will overlap by this strip AND THE PH VALUE WILL BE REDUCED BY
C
        ISTRT(1) = 1
        DO 30 ICLU=2,NCLU
C
C  Number of strips between the 2 seeds
C
          IDIF = ISEED(2,ICLU)-ISEED(2,ICLU-1)-1
          IF(LEVEN(IDIF))THEN
C
C  If even, divide the strips evenly
C
            NSCLU(ICLU-1) = ISEED(2,ICLU) - IDIF/2 - ISTRT(ICLU-1)
            ISTRT(ICLU) = ISEED(2,ICLU)-IDIF/2
          ELSE
C
C  Otherwise split the shared strip
C
            NSCLU(ICLU-1) = ISEED(2,ICLU) - (IDIF-1)/2 - ISTRT(ICLU-1)
            ISTRT(ICLU) = ISEED(2,ICLU)-(IDIF+1)/2
C
C-- this line doesn't do anything, becauase RPULS is a local
C-- variable, it has to be propagated back through VCSG
C-- Manoj Feb 14,1995
C
            RPULS(ISTRT(ICLU)) = .5*RPULS(ISTRT(ICLU))
          END IF
30      CONTINUE
        NSCLU(NCLU) = NSTRP-ISTRT(NCLU)+1
      END IF
C
C-- propagating RPULS back
C
      DO ISTRP=1,NSTRP
         IVCSG=IFST+ISTRP-1
         KLINE = KROW(KVCSG,IVCSG)+JVCSMP
C         RPULS(ISTRP)=RTABL(KVCSG,IVCSG,JVCSMP)
         RW(KLINE)=RPULS(ISTRP)
      ENDDO
      RETURN
      END
#endif
