      INTEGER FUNCTION KGTBLQ(IRUN,IEVT,MINICHUNK,BLQPOS,BLQERR,IGAIN)
C     ================================================================
C
C     Olivier Schneider, CERN-PPE/ALE, Aug 22, 1996
C
C!    Get the BOM+QS0 beam position stored in BLQP bank
C!    for run IRUN and event IEVT.
C!
C!    Input:   IRUN = run number 
C!    ------   IEVT = event number or 0 (see note below)
C!
C!    Output:  MINICHUNK = mini-chunk number in run (= row number in BLQP bank)
C!    -------  BLQPOS(1) = x coordinate of beam position (cm)
C!             BLQPOS(2) = y coordinate of beam position (cm)
C!             BLQERR(1) = statistical error on BLQPOS(1)
C!             BLQERR(2) = statistical error on BLQPOS(2)
C!             IGAIN(1)  = gain flag side A
C!             IGAIN(2)  = gain flag side B
C!
C!    Function value: error code with the following meaning:
C!    ---------------   0 means OK
C!                      1 means "unaligned" data is returned
C!                      2 means "unaligned" data without QS0 correction
C!                     -1 means bad input arguments
C!                     -2 means missing BLQP bank for run IRUN 
C!                     -3 means no available data for event IEVT in BLQP bank
C!                     -4 means bad data for event IEVT in BLQP bank
C!
C!    Notes: 1) A function value strictly below 0 indicates a fatal error; 
C!    ------    in that case, and if the function value is not -4,
C!              MINICHUNK is set to 0, the components of BLQPOS and IGAIN 
C!              are set to 0, and those of BLQERR to -1.
C!           2) A function value strictly above 0 indicates that only 
C!              "raw" data could be returned, i.e. data in the BOM system
C!              of coordinates (rather than in the ALEPH system of coordinates);
C!              this data might not include the QS0 corrections; in this case
C!              the components of BLQERR are returnbed as -1.
C!           3) The statistical errors returned in BLQERR include only the
C!              statistical uncertainties from the BOM alignment with 
C!              respect to ALEPH, as determined with VDET tracks; these 
C!              errors don't include the statistical and systematic 
C!              uncertainties attached to the BOM data itself.
C!           4) In the special case where IEVT=0 on input, BLQPOS and BLQERR 
C!              are filled with the fitted offset between the BOM coordinates
C!              and the ALEPH (VDET) coordinates, the components of IGAIN are 
C!              filled with 0 and MINICHUNK is returned as 1 (because the 
C!              alignment constants are stored as the first mini-chunk !).
C!           5) The gain flags are bit coded words with the following meaning:
C!                bits 0-1: gain for QS0 BPM e+
C!                bits 2-3: gain for QS4 BPM e+
C!                bits 4-5: gain for QS0 BPM e-
C!                bits 6-7: gain for QS4 BPM e-
C!              where each of the 4 gains has the following meaning:
C!                          gain = 0: -10 dB or info not available
C!                          gain = 1:   0 dB (low  gain physics setting)
C!                          gain = 2:  10 dB (high gain physics setting)
C!                          gain = 3:  20 dB or more
C!              For example:
C!                IGAIN(I)=  0  ---> gain information not available
C!                IGAIN(I)= 85  ---> all 4 gains are 0 dB
C!                IGAIN(I)= 90  ---> e-  gains are 0 dB and e+  gains are 10 dB
C!                IGAIN(I)=102  ---> QS4 gains are 0 dB and QS0 gains are 10 dB
C!                IGAIN(I)=153  ---> QS0 gains are 0 dB and QS4 gains are 10 dB
C!                IGAIN(I)=165  ---> e+  gains are 0 dB and e-  gains are 10 dB
C!                IGAIN(I)=170  ---> all 4 gains are 10 dB
C!              Only 0 dB (low gain) and 10 dB (high gain) are supposed to be 
C!              used during physics; with the "new gain setting strategy" in 
C!              effect since 1996, the gains are set independently for each
C!              beam polarity in each of the 4 BPM's ("Beam Position Monitors");
C!              the change of gain (from 0 dB to 10 dB) should normally occur 
C!              only once per BPM in the course of a fill, for bunch currents 
C!              around 220 microAmps.
C!                
#ifndef DOC
      IMPLICIT NONE
C
C     Arguments
C
      INTEGER IRUN,IEVT,MINICHUNK
      REAL BLQPOS(2),BLQERR(2)
      INTEGER IGAIN(2)
C
C     Externals
C
      INTEGER NLINK,AND
C
C     Parameters
C
      INTEGER BAD_BITS,GAIN_BITS
      PARAMETER(BAD_BITS ='0F001F00'X) ! bits 8-12 and 24-27
      PARAMETER(GAIN_BITS='000000FF'X) ! bits 0-7
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
      INTEGER KBLQP,IROW,NZERO,I,ISTAT
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
      MINICHUNK=0
      DO I=1,2
        BLQPOS(I)=0.
        BLQERR(I)=-1.
        IGAIN(I)=0
      ENDDO
C
C     Check input arguments
C
      IF(IEVT.LT.0.OR.IRUN.LE.0) THEN 
        KGTBLQ=-1
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
        KGTBLQ=-2
        RETURN
      ENDIF
C
C     Get row number in BLQP corresponding to event IEVT
C
      IF(IEVT.GT.0) THEN
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
C       Check that data is available
C
  11    KGTBLQ=-3
        IF(IROW.LE.0) RETURN
        IF(ITABL(KBLQP,IROW,JBLQFE).LE.0) RETURN
        IROWOLD = IROW
        IEVTOLD = IEVT
      ENDIF
C
C     Get QS0 correction factors and BOM alignement constants
C
      IF(ITABL(KBLQP,1,JBLQFE).LE.0) THEN 
        MINICHUNK=1
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
          KGTBLQ=0
        ELSE
          KGTBLQ=1
          DO I=1,2
            BLQPOS(I)=0.
            BLQERR(I)=-1.
          ENDDO
        ENDIF
      ELSE
        KGTBLQ=2
        CALL UZERO(ALPHAA,1,2)
        CALL UZERO(ALPHAB,1,2)
      ENDIF
      IF(IEVT.LE.0.AND.KGTBLQ.NE.0) KGTBLQ=-3
C
C     Compute beam spot position
C
      IF(IEVT.GT.0) THEN
        MINICHUNK=IROW
        DO I=1,2
          BLQPOS(I)=BLQPOS(I)+0.5*(RTABL(KBLQP,IROW,JBLQBO+I-1)+
     &                             RTABL(KBLQP,IROW,JBLQBO+I+1)+
     &                   ALPHAA(I)*RTABL(KBLQP,IROW,JBLQQS+I-1)+
     &                   ALPHAB(I)*RTABL(KBLQP,IROW,JBLQQS+I+1))
C
C         Check that data is valid
C
          ISTAT=ITABL(KBLQP,IROW,JBLQSF+I-1)
          IF(AND(ISTAT,BAD_BITS).NE.0) KGTBLQ=-4
C
C         Extract gain flags
C
          IGAIN(I)=AND(ISTAT,GAIN_BITS)
        ENDDO
      ENDIF
C
C     Convert from microns to cm
C
      DO I=1,2
        BLQPOS(I)=BLQPOS(I)*1.E-4
        BLQERR(I)=BLQERR(I)*1.E-4
      ENDDO
      END
#endif
