      SUBROUTINE RWREVT
C--------------------------------------------------------------------
C!  - Write the reconstructed event to output streams
C!
C!  Author       J. Bunn
C!
C!  Modified by  S. Orteu   24-FEB-1987   New BOS I/O routines
C!   Modified :- E. Lancon             18-SEP-1991
C!            Get Beam position (used to fill JSUM) before  compression
C!            banks (Call BMSUM)
C!
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "bosext.h"
#include "rflags.h"
#include "rstate.h"
#include "rlunit.h"
#include "monejj.h"
#include "jsumjj.h"
      CHARACTER*600 ELIST
      CHARACTER*4 POTBK
      LOGICAL WRTIT
      EXTERNAL LNBLNK
      DATA LNC / 0 /
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
      IF(.NOT.FREPRO) CALL RPOTCL
C
C     get beam position from all tracks (d0,z0)
      CALL BMSUM
C
      WRTIT = .FALSE.
      DO 1 I=2,2
        I15=IW(15)-IW(17)
        LGAIN=I15-(IW(15)-IW(17))
        L=LNBLNK(OUTLIS(I))
C
C?   Be sure that banks in OUTLIS are written out
C?   T list is drop at end of reconstruction
C
        CALL BLIST (IW,'T+','E')
        CALL BLIST (IW,'E=',OUTLIS(I)(1:L))
        IF(JOUTRL(I).NE.NOOUTP)THEN
          WRTIT = .TRUE.
          LNC = LENLIS ('E')
          CALL RNXMOD(MODCOM,RMOD)
          CALL CMPLIS ('E', NBNK)
          LGAIN = LNC - LENLIS ('E')
          CALL RSHRIN(IW,'E',ELIST)
          L = LNBLNK(ELIST)
          CALL BLIST (IW,'E=',ELIST(1:L))
          CALL BLIST (IW,'T-',ELIST(1:L))
          CALL RNXMOD(MODWRI,RMOD)
          CALL ABWSEL ('E')
        ELSE
          CALL BLIST (IW,'T-',OUTLIS(I)(1:L))
        ENDIF
    1 CONTINUE
C     write record to monitoring file
C
      KMONE=IW(NAMIND('MONE'))
      IF(KMONE.NE.0)THEN
        IMONE=KMONE+LMHLEN
        IW(IMONE+JMONLP)= LNC
        IW(IMONE+JMONLC)= LNC-LGAIN
        IW(IMONE+JMONF1)=JFBIRF(1)
        IW(IMONE+JMONF2)=JFBIRF(2)
        IW(IMONE+JMONN1)=JEBIRF(1)
        IW(IMONE+JMONN2)=JEBIRF(2)
        IF(LMONRL.NE.0)THEN
          WRITE(LMONRL)LMONEA,(IW(IMONE+I),I=1,LMONEA)
        ENDIF
      ENDIF
C
C     fill JSUM bank
C
      CALL RNXMOD (MODSUM,RMOD)
      CALL RCJSUM
C
C?   if no output required drop E list
C?   normaly this is done in ABRSEL if output is required
C
      IF ( .NOT.WRTIT ) THEN
        CALL BDROP (IW,'E')
      ELSE
        NEVTRS(2) = NEVTRS(2) + 1
        IF (FDEBRF.AND.JDBDRF(JULSK).GE.3) THEN
          WRITE(LOUTRL,'(A)') '0RWREVT- List of banks written'
          CALL AUBLIS('E')
        ENDIF
      ENDIF
C
      END
#endif
