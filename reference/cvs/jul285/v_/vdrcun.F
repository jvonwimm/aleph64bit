      SUBROUTINE VDRCUN
C----------------------------------------------------------------------
C!  - This subroutine unpacks the VRCN bank into the VRECON common
C!
C!   Author   :- D. Brown
C!
C!   Modified :- E. Lancon              5-APR-1993
C!              Set only final P.H. and Hot chanels for PASS0 mode
C!   Modified :- I. Tomalin            16-SEP-1996
C!              Set MKVDXY to .TRUE. if in PASS0 and running at high
C!              energy using PVGG card.
C?
C!======================================================================
#ifndef DOC
#include "vrcnjj.h"
#include "vrecon.h"
#include "rflags.h"
C
C  Functions
C
      INTEGER NAMIND
      LOGICAL ALLEP1
      INTEGER VDYEAR
      EXTERNAL NAMIND,ALLEP1,VDYEAR
C
C  Local parameters
C
      INTEGER IND
C
C  Variables for inline functions
C
#include "bcs.h"
#include "bmacro.h"
C
C  Find the bank
C
      IND = IW(NAMIND('VRCN'))
      IF(IND .EQ. 0)THEN
        CALL RERROR('VDRCUN',1,'Failed to find VRCN bank')
C
C Load default values
C
        MNPULS = 8
        MNPSUM = 24
        MXUNBD = 3
        MXSIGM = 20
        MKVDXY = .TRUE.
        MKVFPH = .TRUE.
        MKVHOT = .FALSE.
        NCHANM = 4
        HCUTCM = 2.2
        LCUTCM = 3.0
        MXZOCC = 0.1
        MXPOCC = 0.1
        FNLSUM = 40
      ELSE
C
C  Final reclustering parameters
C
        MNPULS = ITABL(IND,1,JVRCPU)
        MNPSUM = ITABL(IND,1,JVRCCM)
        MXUNBD = ITABL(IND,1,JVRCMU)
        MXSIGM = ITABL(IND,1,JVRCMS)
        FNLSUM = ITABL(IND,1,JVRCFS)
C
C  Control flags
C
        MKVDXY = ITABL(IND,1,JVRCMX).EQ.-1
        MKVFPH = ITABL(IND,1,JVRCMF).EQ.-1
        MKVHOT = ITABL(IND,1,JVRCMH).EQ.-1
C
C  Common mode suppression parameters
C
        NCHANM = ITABL(IND,1,JVRCMN)
        HCUTCM = RTABL(IND,1,JVRCHC)
        LCUTCM = RTABL(IND,1,JVRCLC)
C
C  Hot channel suppression parameters
C
        MXZOCC = RTABL(IND,1,JVRCMZ)
        MXPOCC = RTABL(IND,1,JVRCMP)
      END IF
C
C  Overwrite the control flags if reprocessing  or PASS0
C
      IF ( FREPRO ) THEN
        MKVDXY = .TRUE.
        MKVFPH = .FALSE.
        MKVHOT = .FALSE.
      ENDIF
      IF ( FPASS0 ) THEN
        MKVDXY = .FALSE.
        IF (IW(NAMIND('PVGG')).GT.0.AND..NOT.ALLEP1(IRUNRC)) 
     +  MKVDXY = .TRUE.
        MKVFPH = .TRUE.
        MKVHOT = .TRUE.
      ENDIF
C
C Never produce hot channels for new Vdet
C
      IF(VDYEAR().EQ.95)MKVHOT = .FALSE.
C
      RETURN
      END
#endif
