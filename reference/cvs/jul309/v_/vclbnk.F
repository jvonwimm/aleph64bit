      SUBROUTINE VCLBNK(
     >      KVFHL,KVFPH,KVFLG,KVCOM,IFRS,ILS,ILAY,IPHI,IVIEW,IWAF,IWFF)
C
C----------------------------------------------------------------------
C!  - Put Vdet cluster into banks
C!
C!   Inputs:
C!        - KVFHL,KVFPH,KVFLG,KVCOM : Bank indices
C!                    (necessary because they are members of families)
C!        - IFRS first strip
C!        - ILS  last strip
C!        - ILAY layer
C!        - IPHI face
C!        - IVIEW view
C!        - IWAF wafer in module
C!        - IWFF wafer in face
C
C!  15-12-96 A. Bonissent
C?
C!======================================================================
#ifndef DOC
C      IMPLICIT NONE
      SAVE FIRST, NAVCSG
      INTEGER NAVCSG
      INTEGER KVCSG,KVFHL,KVCOM,KVFLG,KVFPH
      INTEGER NSIZ,ILS,IFRS,IADDR,ILAY,IWFF,IPHI,IVIEW
      INTEGER JSTRP,IWAF
C
      INTEGER NAMIND
C
C      INTEGER JVFLFG,LVFLGA
#include "vflgjj.h"
C      INTEGER JVFHHA,LVFHLA
#include "vfhljj.h"
C      INTEGER JVFPPH,LVFPHA
#include "vfphjj.h"
C      INTEGER JVCOCM,LVCOMA
#include "vcomjj.h"
#include "vcsgjj.h"
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bcs.h"
C      INTEGER LCOLS,LROWS,ID,KNEXT,KROW,NRBOS,LFRWRD,LFRROW,ITABL,L
C      REAL RTABL
#include "bmacro.h"
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVCSG=NAMIND('VCSG')
      ENDIF
      KVCSG=IW(NAVCSG)
      NSIZ = ILS-IFRS+1
      CALL VPKADD(IADDR,NSIZ,ILAY,IWFF,IPHI,IVIEW,IFRS)
      IW(KNEXT(KVFHL)+JVFHHA)=IADDR
      IW(KVFHL+LMHROW) = LROWS(KVFHL) + 1
C
C  Pack common mode word- use first strip of cluster
C
      IW(KNEXT(KVCOM)+JVCOCM) = ITABL(KVCSG,IFRS,JVCSCM)
      IW(KVCOM+LMHROW) = LROWS(KVCOM) + 1
C
C  Pack strips and flags
C
      DO JSTRP=IFRS,ILS
        IW(KNEXT(KVFPH)+JVFPPH) = RTABL(KVCSG,JSTRP,JVCSMP+IWAF-1)
        IW(KNEXT(KVFLG)+JVFLFG) = ITABL(KVCSG,JSTRP,JVCSMF+IWAF-1)
        IW(KVFPH+LMHROW) = LROWS(KVFPH) + 1
        IW(KVFLG+LMHROW) = LROWS(KVFLG) + 1
      END DO
      RETURN
      END
#endif
