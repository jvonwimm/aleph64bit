      SUBROUTINE IGETTI(IER)
C-----------------------------------------------------------------------
C!  Decide in which order to try extending the TPC tracks.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- J. Sedgbeer 88/06/20
C!    Modified    :- I. Tomalin  89/02/28
C!
C!   Input:
C!     need commons /BCS/ for TGFT,TPCO,TGTL,TGCL banks
C!          params.  TGFTJJ
C!
C!   Output:
C!           /IWBNKT/ Contains pointer to IWTPCP work bank.
C!        IER   /I  : IER = 0 if everything is O.K.
C!                    IER = 1 if insufficient BOS space.
C!
C!   called by : ITCTPC
C!   calls     : SORTZV
C!
C!   Libraries required: CERNLIB
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O commons
#include "bcs.h"
#include "iwbnkt.h"
#include "rcurnt.h"
#include "tgftjj.h"
#include "bosext.h"
C-----------------------------------------------------------------------
C Dummy common block to save work bank indices.
      COMMON/IDUMYA/IWRHOT
      LOGICAL SETZ
      DATA SETZ/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      IER = 0
      IF (SETZ) THEN
        IWTPCP = 0
        IWRHOT = 0
        SETZ = .FALSE.
      END IF
C
C Get TGFT bank.
      KTGFT  = IW(NAMIND('TGFT'))
      NTGF = 0
      IF (KTGFT.GT.0) NTGF = LROWS(KTGFT)
C
C Create work bank for storing order for using TPC tracks.
      IF (IWTPCP.GT.0) THEN
        CALL WDROP(IW,IWTPCP)
        CALL RERROR('IGETTI',1,'IWTPCP work bank still exists'//
     &  ' from earlier. It should have been dropped.')
      END IF
      CALL WBANK(IW,IWTPCP,LMHLEN+NTGF,*998)
      IW(IWTPCP+LMHCOL) = 1
      IW(IWTPCP+LMHROW) = NTGF
C
      IF (NTGF.EQ.0) THEN
        CALL RERROR('IGETTI',2,'No TGFT bank found')
        GOTO 999
      END IF
C
C Create work bank for storing TPC track radii.
      IF (IWRHOT.GT.0) THEN
        CALL WDROP(IW,IWRHOT)
        CALL RERROR('IGETTI',3,'IWRHOT work bank still exists '//
     &  'from earlier. It should have been dropped.')
      END IF
      CALL WBANK(IW,IWRHOT,LMHLEN+NTGF,*998)
      IW(IWRHOT+LMHCOL) = 1
      IW(IWRHOT+LMHROW) = NTGF
C
C Note TPC track radii.
C
      DO 10 I=1,NTGF
        RW(IWRHOT+LMHLEN+I) = 1.0/ABS(RTABL(KTGFT,I,JTGFIR))
   10 CONTINUE
C
C Sort all the chains in order of decreasing Pt and hence provide the
C order in which the TPC tracks will be used.
C
      CALL SORTZV(RW(IWRHOT+LMHLEN+1),IW(IWTPCP+LMHLEN+1),NTGF,1,1,0)
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
        WRITE(LDEBRL,1005) NTGF,(RW(IWRHOT+LMHLEN+I),I=1,NTGF)
 1005   FORMAT(10X,'TPC tracks:  NTGF',I4,/(10X,' RHO',5F7.1))
        WRITE(LDEBRL,1010) (IW(IWTPCP+LMHLEN+I),I=1,NTGF)
 1010   FORMAT(5X,' Indices after sort: ',/(10X,10I5))
      ENDIF
C
      GOTO 999
C
  998 CONTINUE
      IER = 1
      CALL RERROR('IGETTI',4,' Insufficient BOS space')
      IF (IWTPCP.GT.0) CALL WDROP(IW,IWTPCP)
  999 CONTINUE
      IF (IWRHOT.GT.0) CALL WDROP(IW,IWRHOT)
      END
#endif
