      SUBROUTINE VDBGCL(IBEG)
C
C----------------------------------------------------------------------
C!  - Find where clustering should start
C!    Used only for VET95
C!    If first and last strips are fired, start after the first
C!    non fired strip. The beginning will be attached to the
C!    last cluster at the end
C!
C! Author A. Bonissent March 1995
C?
C!======================================================================
#ifndef DOC
#include "vdflgs.h"
C PARAMETERS-
C MSKIP = mask for defining generic 'unbonded' strip
C MSGOOD = ANTI-mask for defining generic good strip
C
      INTEGER NAMIND
C
      INTEGER IBEG
C
      INTEGER MGOOD,MSKIP
      PARAMETER (MSKIP = VBSUPP+VBHOTC+VBNOIS+VBUNBD+VBUNUS)
      PARAMETER (MGOOD = VBSUPP+VBHOTC+VBNOIS+VBUNBD+VBUNUS)
      INTEGER KVFHL,KVFPH,KVFLG,KVCOM,NAVCSG,NVCSG,NSKL
      INTEGER IVCSG,IPH,IFL,IFLG,NGAPL,NSKH,NSKIP,ICURR,NGAP
#include "vrecon.h"
      LOGICAL FIRST,SSKIP,SGOOD
      DATA FIRST /.TRUE./
#include "vcsgjj.h"
#include "bcs.h"
#include "bmacro.h"
      SGOOD(IFLG) = IFLG.GT.0.AND.IAND(IFLG,MGOOD).EQ.0
      SSKIP(IFLG) = IAND(IFLG,MSKIP).GT.0
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVCSG=NAMIND('VCSG')
      ENDIF
      KVCSG=IW(NAVCSG)
      NVCSG=LROWS(KVCSG)
      NSKL=0
C
C Find first fired strip, and count number of skipped strips
C from the beginning
C
      DO 10 IVCSG=1,NVCSG
        IPH=RTABL(KVCSG,IVCSG,JVCSMP)
        IFL=ITABL(KVCSG,IVCSG,JVCSMF)
        IF(SSKIP(IFL))NSKL=NSKL+1
        IF(SGOOD(IFL).AND.IPH.GE.MNPULS)THEN
          IBEG=IVCSG
          NGAPL=IVCSG-1
          GO TO 11
        ENDIF
 10   CONTINUE
 11   CONTINUE
      NSKH=0
C
C Find last fired strip, and count number of skipped strips
C to the end
C
      DO 20 IVCSG=1,NVCSG
        ICURR=NVCSG+1-IVCSG
        IPH=RTABL(KVCSG,ICURR,JVCSMP)
        IFL=ITABL(KVCSG,ICURR,JVCSMF)
        IF(SSKIP(IFL))NSKH=NSKH+1
        IF(SGOOD(IFL).AND.IPH.GE.MNPULS)THEN
          NGAPH=IVCSG-1
          GO TO 21
        ENDIF
 20   CONTINUE
 21   CONTINUE
      NSKIP=NSKL+NSKH
      NGAP=NGAPL+NGAPH
C
C if the cluster which would connect the last to first
C strips does not contain only dead channels,
C or is too long, then clustering should start at first
C strip
      IF((NGAP-NSKIP).NE.0.OR.NSKIP.GT.MXUNBD)IBEG=1
      RETURN
      END
#endif
