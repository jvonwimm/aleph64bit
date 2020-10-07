C
C***********************************************************************
C
      INTEGER FUNCTION OGTBLQ(IRUN,IEVT,BLQPOS,BLQERR)
C     ================================================

C     Olivier Schneider, CERN-PPE/ALE, Aug 22, 1996
C
C!    Get the BOM+QS0 beam position stored in BLQP bank
C!    for run IRUN and event IEVENT.
C!
C!    Input:   IRUN = run number 
C!    ------   IEVT = event number 
C!
C!    Output:  BLQPOS(1) = x coordinate of beam position (cm)
C!    -------  BLQPOS(1) = y coordinate of beam position (cm)
C!             BLQERR(1) = error on BLQPOS(1)
C!             BLQERR(2) = error on BLQPOS(2)
C!
C!    Function value: error code with the following meaning:
C!    ---------------   0 means OK
C!                      1 means "unaligned" data is returned
C!                      2 means "unaligned" data without QS0 correction
C!                     -1 means bad input arguments
C!                     -2 means missing BLQP bank for run IRUN 
C!                     -3 means no valid data for event IEVT in BLQP bank
C!
C!                   N.B: a negative fuction value indicates a fatal 
C!                        error; in that case the components of BLQPOS 
C!                        are set to 0. and those of BLQERR to -1.)
C!
#ifndef DOC
      IMPLICIT NONE
C
C     Arguments
C
      INTEGER IRUN,IEVT
      REAL BLQPOS(2),BLQERR(2)
C
C     Externals
C
      INTEGER NLINK
C
C     Parameters
C
      INTEGER MASK
      PARAMETER(MASK='0F001F00'X) ! bits 8-12 and 24-27
#include "blqpjj.h"
C
C     Static local variables
C
      INTEGER IEVTOLD,IROWOLD,IRUNOLD
      SAVE IROWOLD,IEVTOLD,IRUNOLD
      DATA IROWOLD,IEVTOLD,IRUNOLD/0,0,0/
C
C     Non-static local variables
C
      INTEGER KBLQP,IROW,NZERO,I
      REAL ALPHAA(2),ALPHAB(2)
C
C     BOS common block
C
#include "bcs.h"
C
C     Function definition statements
C
#include "bmacrod.h"
#include "bmacro.h"
C
C     Set default output arguments
C
      DO I=1,2
        BLQPOS(I)=0.
        BLQERR(I)=-1.
      ENDDO
C
C     Check input arguments
C
      IF(IEVT.LE.0.OR.IRUN.LE.0) THEN 
        OGTBLQ=-1
        RETURN
      ENDIF
      IF(IRUN.NE.IRUNOLD) THEN 
        IRUNOLD=IRUN
        IROWOLD=0
        IEVTOLD=0
      ENDIF
C
C     Check presence of BLQP bank for this run
C
      KBLQP=NLINK('BLQP',IRUN)
      IF(KBLQP.LE.0) THEN 
        OGTBLQ=-2
        RETURN
      ENDIF
C
C     Get row number in BLQP corresponding to event IEVT
C
      IF(IEVT.GT.IEVTOLD) THEN 
        DO IROW=IROWOLD,LROWS(KBLQP)-1
          IF(ITABL(KBLQP,IROW+1,JBLQFE).GT.IEVT) GOTO 11 
        ENDDO
        IROW=LROWS(KBLQP)
      ELSE IF(IEVT.LT.IEVTOLD) THEN
        DO IROW=IROWOLD,1,-1
          IF(ITABL(KBLQP,IROW,JBLQFE).LE.IEVT) GOTO 11
        ENDDO
        IROW=0
      ELSE
        IROW=IROWOLD
      ENDIF
C
C     Check that data is available and valid
C
  11  OGTBLQ=-3
      IF(IROW.LE.0) RETURN
      IF(ITABL(KBLQP,IROW,JBLQFE).LE.0) RETURN
      IF((IAND(ITABL(KBLQP,IROW,JBLQSF  ),MASK).NE.0).OR.
     &   (IAND(ITABL(KBLQP,IROW,JBLQSF+1),MASK).NE.0)) RETURN
      IROWOLD = IROW
      IEVTOLD = IEVT
C
C     Get QS0 correction factors and BOM alignement constants
C
      IF(ITABL(KBLQP,1,JBLQFE).LE.0) THEN 
        NZERO=0
        DO I=1,2
          ALPHAA(I)=RTABL(KBLQP,1,JBLQQS+I-1)
          ALPHAB(I)=RTABL(KBLQP,1,JBLQQS+I+1)
          BLQPOS(I)=RTABL(KBLQP,1,JBLQBO+I-1)
          BLQERR(I)=RTABL(KBLQP,1,JBLQBO+I+1)
          IF(BLQPOS(I).EQ.0.) NZERO=NZERO+1
          IF(BLQERR(I).EQ.0.) NZERO=NZERO+1
        ENDDO
        IF(NZERO.LT.4.AND.BLQERR(1).GE.0..AND.BLQERR(2).GE.0.) THEN
          OGTBLQ=0
        ELSE
          OGTBLQ=1
          DO I=1,2
            BLQPOS(I)=0.
            BLQERR(I)=-1.
          ENDDO
        ENDIF
      ELSE
        OGTBLQ=2
        CALL UZERO(ALPHAA,1,2)
        CALL UZERO(ALPHAB,1,2)
      ENDIF
C
C     Compute beam spot position
C
      DO I=1,2
        BLQPOS(I)=BLQPOS(I)+0.5*(RTABL(KBLQP,IROW,JBLQBO+I-1)+
     &                           RTABL(KBLQP,IROW,JBLQBO+I+1)+
     &                           RTABL(KBLQP,IROW,JBLQQS+I-1)*ALPHAA(I)+
     &                           RTABL(KBLQP,IROW,JBLQQS+I+1)*ALPHAB(I))
        BLQPOS(I)=BLQPOS(I)*1.E-4 ! convert from microns to cm
        BLQERR(I)=BLQERR(I)*1.E-4 ! convert from microns to cm
      ENDDO
      END
#endif
