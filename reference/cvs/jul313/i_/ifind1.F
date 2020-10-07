      SUBROUTINE IFIND1(ITRY,IDTPC,SPIRAL)
C-----------------------------------------------------------------------
C! Find paths in trees
C!
C!    Author      :- J. Sedgbeer 88/04/14
C!    Modified    :- J. Sedgbeer 88/05/09
C!    Modified    :- I. Tomalin  89/06/07
C!
C!   Input:
C!      ITRY   /I : Type of search flag
C!      commons:     /BCS/
C!                   /ITRKPA/
C!                   /ITGAPC/
C!                   /IWBNKT/
C!
C!   Output:
C!      commons:     /IPATHC/ initialse no. of paths to zero
C!                   /ITTIME/ CPU time taken by IFITP
C!
C!   called by : ITRAK
C!   calls     : ICLIMB,IFITP
C!
C!   Libraries required: none
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O Commons etc.
#include "bcs.h"
#include "itrkpa.h"
#include "itgapc.h"
#include "ipathc.h"
#include "iwbnkt.h"
#include "ittime.h"
      LOGICAL SPIRAL
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialize error flag for subroutine ISAVE.
      IESAVE = 0
      MXLINK(1) = 999999
C Loop over the minimum number of links allowed for a track.
C      MNLNK = MNLINK(1)
C      MXJMP = MXJMPD(1)
C      DO 250 N=NILIIT,MNLNK,-1
C        MNLINK(1) = N
C        MXLINK(1) = N
C        MXJMPD(1) = MIN(MXJMP,NILIIT-MNLINK(1))
C
        NPTHIP = 0
C
C Loop over the links.
C
        DO 25 ILNK=1,LROWS(ILNKIW)
          IF(IW(INNXIW+LMHLEN+ILNK).EQ.0) GOTO 25
          IGAP = ITABL(ILNKIW,ILNK,3)
          ILAY = IOGPIT(IGAP)
C See if there are too many missing hits outside layer "ILAY".
          IMISS = NOLIIT - ILAY
          IF (IMISS.GT.MXJMPD(1)) GOTO 100
C
          IF(FDEBRF.AND.JDBDRF(JULIT).GE.5) THEN
            WRITE(LDEBRL,35) ILNK,IW(INNXIW+LMHLEN+ILNK),IGAP,ILAY
   35       FORMAT('0IFIND1 : Starting Link',I3,' No.next',I2,
     +              ' Gap,Layer',2I3)
          END IF
          CALL ICLIMB(ILNK,1)
   25   CONTINUE
C
C Fit paths. (Error => no space to create banks)
C
  100   CONTINUE
C If one or more tracks were found, then fit them.
        IF (NPTHIP.GT.0) THEN
      IF( IFITTI ) CALL ALTIME ( TIME1 )
          CALL IFITP(ITRY,1,SPIRAL,IDTPC,IERR)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
          TFITP1=TFITP1+(TIME2-TIME1)
C Exit if a track was found or there was a BOS error.
          IF (IERR.NE.1) GOTO 999
          IF (IESAVE.EQ.1) GOTO 999
        END IF
  250 CONTINUE
C
  999 CONTINUE
      IF (IESAVE.EQ.1.AND.FDEBRF.AND.JDBDRF(JULIT).GE.1) CALL
     +RERROR('IFIND1',1,
     +'ISAVE could not store all longest trees for this track.')
C      MNLINK(1) = MNLNK
C      MXJMPD(1) = MXJMP
      END
#endif
