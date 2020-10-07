      SUBROUTINE IFIND2(ITRY)
C-----------------------------------------------------------------------
C! Find paths in trees
C!
C!    Author      :- J. Sedgbeer 88/04/14
C!    Modified    :- J. Sedgbeer 88/05/09
C!    Modified    :- I. Tomalin  89/02/28
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
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Loop over the minimum number of links allowed for a track.
      MNLNK = MNLINK(2)
      MXJMP = MXJMPD(2)
      DO 250 N=NILIIT,MNLNK,-1
        MNLINK(2) = N
        MXLINK(2) = N
        MXJMPD(2) = MIN(MXJMP,NILIIT-MNLINK(2))
C
C Loop over the links.
C
        NPTHIP = 0
C Initialize error flag for subroutine ISAVE.
        IESAVE = 0
        DO 50 ILNK=1,LROWS(ILNKIW)
C Don't bother climbing from this link if it doesn't go anywhere.
          IF (IW(INNXIW+LMHLEN+ILNK).EQ.0) GOTO 35
C
          IGAP = ITABL(ILNKIW,ILNK,3)
          IF (IGAP.EQ.0) GOTO 35
C
C If too many layers were jumped outside this link, then it may not be
C possible for it to give a track of the desired length.
          ILAY = IOGPIT(IGAP)
          IMISS = NOLIIT - ILAY
          IF (NILIIT-IMISS.LT.MNLINK(2)) GOTO 250
C
          CALL ICLIMB(ILNK,2)
C
   35     CONTINUE
C If no tracks were found, then go to the next link.
          IF (NPTHIP.EQ.0) GOTO 50
C If the next link starts with the same coordinate as this one
C (regardless of which ambiguity), then do not carry out the fit yet.
          IF (ILNK.LT.LROWS(ILNKIW).AND.ABS(ITABL(ILNKIW,ILNK,1)).EQ.
     +    ABS(ITABL(ILNKIW,ILNK+1,1))) GOTO 50
C
C Fit paths.
C
      IF( IFITTI ) CALL ALTIME ( TIME1 )
          CALL IFITP(ITRY,2,.FALSE.,IDUM,IER)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
          TFITP2=TFITP2+(TIME2-TIME1)
C Exit if there was a BOS error.
          IF (IER.EQ.2) GOTO 999
C
          NPTHIP = 0
C
          IF (IESAVE.EQ.1.AND.FDEBRF.AND.JDBDRF(JULIT).GE.1) CALL
     +    RERROR('IFIND2',1,
     +  'ISAVE could not store all longest trees for this track')
C Reinitialize error flag for subroutine ISAVE.
          IESAVE = 0
C
   50   CONTINUE
  250 CONTINUE
C
  999 CONTINUE
      MNLINK(2) = MNLNK
      MXJMPD(2) = MXJMP
      END
#endif
