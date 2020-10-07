      SUBROUTINE VSEPLT(ILAY,IWAF,IFST,NSTRP,NSEP,SSPOS,PPSUM,
     >              IBEG,NSCLU)
C
C----------------------------------------------------------------------
C!  - Separate merged VDET hits at low theta
C!
C!   Author   :- A. Bonissent 19-12-95
C!
C!  This takes as input a VFHL cluster list of pulseheights,
C!  and checks to see if they are consistent with coming from a
C!  single cluster.  If not, it attempts to separate into separate
C!  hits.
C!
C!  Inputs;
C!             ILAY  = Layer
C!             IWAF  = Wafer in module
C!             FSTRP = First strip in the cluster
C!             NSTRP = Number of strips in cluster
C!  Outputs;
C!             NSEP  = Number of clusters found (>=1 normally, 0=error)
C!             SSPOS = Position of each separated cluster
C!             PPSUM = Pulseheight for each separated cluster
C!             IBEG  = Starting position of each subcluster
C!             NSCLU = number of strips in each subcluster
C!
C?
C!======================================================================
#ifndef DOC
C      IMPLICIT NONE
      SAVE FIRST, NAVCSG, NAVTRP
      INTEGER NAVCSG,NAVTRP
      INTEGER IVCSG
      INTEGER NVCSG
      INTEGER KVCSG,KVTRP
      INTEGER NSTRP,ISTRP,IFST,IRET,NROSTR,IFREQ,IWAF
      INTEGER ILAY,JWAF,JFAC,NEXP
      REAL ZL,PSUM,PITCH,ZZZ,COSTH,RAD,SINTH,RADIUS
C
      INTEGER NAMIND,VROSTM,VDRSLC,VJWAFM,VABCXY,VXYZFC,VJFACI
C
      INTEGER MAXST
      PARAMETER (MAXST = 50)
      REAL RPULS(MAXST)
      REAL ABC(3),XYZ(3),SSPOS(2),PPSUM(2)
      INTEGER IBEG(*),NSCLU(*)
      INTEGER ILST,ISEP,I,NSEP,NCOMM
      INTEGER IRUN,IROLD,ITP,IEVT
      REAL PHPIK,THICK,ALEN,PHEXP,ANEXP,PEND,ANCOR,PHEXC
      REAL PMIN2
      CHARACTER*4 LIST
      CHARACTER*2 DET
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA  IROLD/0/
C
      REAL VWTHCK
      INTEGER GTSTUP,ALGTDB,JUNIDB
#include "vcsgjj.h"
#include "vtrpjj.h"
#include "bcs.h"
C      INTEGER LCOLS,LROWS,ID,KNEXT,KROW,NRBOS,LFRWRD,LFRROW,ITABL,L
C      REAL RTABL
#include "bmacro.h"
C
      IF(FIRST)THEN
         NAVCSG=NAMIND('VCSG')
         FIRST=.FALSE.
         NAVTRP=NAMIND('VTRP')
      ENDIF
      LIST='VTRP'
      DET='VD'
#include "gtdbbk.h"
      KVTRP=IW(NAVTRP)
      PHPIK = RTABL(KVTRP,1,JVTRWP)
      PMIN2 = RTABL(KVTRP,1,JVTRMP)
      KVCSG=IW(NAVCSG)
      NVCSG=LROWS(KVCSG)
C
C  Check size
C
      IF(NSTRP .GT. MAXST)THEN
        NSEP = 0
        GO TO 999
      END IF
      ZL=0.
      PSUM=0.
      DO ISTRP=1,NSTRP
         IVCSG=IFST+ISTRP-1
         RPULS(ISTRP)=RTABL(KVCSG,IVCSG,JVCSMP)
         ZL=ZL+RPULS(ISTRP)*ISTRP
         PSUM=PSUM+RPULS(ISTRP)
      ENDDO
C
C Get the pitch
C
      IRET = VROSTM(1,NROSTR,PITCH,IFREQ)
C
C Estimate approximate Z
C
      ZL=ZL/PSUM
      CALL VZERO(ABC,3)
      IRET = VDRSLC(ZL,1,ABC(1))
      ABC(1)=ABC(1)+IFST*PITCH
      IRET = VJWAFM(1,IWAF,JWAF)
      IRET = VABCXY(ABC,JWAF,XYZ)
      ZZZ=XYZ(3)
      RAD = SQRT(XYZ(1)**2+XYZ(2)**2)
      COSTH=ZZZ/SQRT(ZZZ**2+RAD**2)
      SINTH=SQRT(1.-COSTH**2)
C
C Estimate expected pulseheight
C
      IRET = VJFACI(ILAY,1,JFAC)
      IRET = VXYZFC(JFAC,XYZ)
      RADIUS = SQRT(XYZ(1)**2+XYZ(2)**2)
C Wafer thickness
      THICK = VWTHCK()

      ALEN = ZZZ*THICK/RADIUS
      PHEXP = PHPIK*SQRT(THICK**2+ALEN**2)/THICK
C
      ANEXP = ALEN/PITCH
      IF(ANEXP.LT.1)ANEXP=1
C
C Compute corrected cluster length
C
      ILST = NSTRP
      PEND = RPULS(1)+RPULS(ILST)
      ANCOR = NSTRP-2+MIN(2.,ANEXP*PEND/PHEXP)
C pulseheight-expected, corrected should be more than min. for 1 track
      PHEXC=(PSUM-PHEXP)*SINTH
      ISEP=0
      IF(PHEXC.GT.PMIN2.AND.ANCOR.GT.ANEXP*2.)THEN
C Cluster is twice longer than expected, separate
          ISEP=1
      ENDIF
      IF(ISEP.EQ.0)THEN
        NSEP=1
        SSPOS(1)=ZL
        PPSUM(1)=PSUM
        IBEG(1)=1
        NSCLU(1)=NSTRP
      ELSE
        NSEP=2
        SSPOS(1)=1+ANEXP/2.
        SSPOS(2)=ILST-ANEXP/2.
        PPSUM(1)=0.
        PPSUM(2)=0.
        NEXP=ANEXP
        IBEG(1)=1
        IBEG(2)=NSTRP-NEXP+1
        NSCLU(1)=NEXP
        NSCLU(2)=NEXP
        DO I=1,NEXP
          PPSUM(1)=PPSUM(1)+RPULS(I)
          PPSUM(2)=PPSUM(2)+RPULS(NSTRP+1-I)
        ENDDO
      ENDIF
  999 CONTINUE
      RETURN
      END
#endif
