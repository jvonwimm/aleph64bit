      SUBROUTINE IXHITS(IRUN,NT,IV,IER)
C-----------------------------------------------------------------------
C! See if a track should have had a coord. in each ITC layer.
C!
C!    Author  :- I. Tomalin  89/06/24
C!    Modified:- J.Sedgbeer  89/10/12 Remove obsolete IHTD bank
C!    Modified:- J.Sedgbeer  91/01/07 Ensure correct IWST bank. Test
C!                                    r-phi coords only.
C!   Input:
C!     IRUN   /I4 : Run number.
C!     NT     /I4 : FRFT number of track.
C!     need common /BCS/ for FRFT bank.
C!     need common /ITWICC/ for ITC geom.
C!
C! IMPORTANT: If you wish to use this subroutine, then at the beginning
C!            of each run, you must call the ALEPHLIB routine, IRDDAF,
C!            to fill commons with the ITC geometry etc.
C!
C!   Output:
C!     IVi    /I4:  =1 if coordinate was expected in ITC layer i
C!                  =0 if it was not.
C!     IER    /I4:  =1 if FRFT bank was missing (and IV is not filled).
C!                  =0 if all O.K.
C!
C!   calls     : none
C!
C!   Libraries required: BOS
C!
C? This subroutine determines in which layers of the ITC, the track NT
C? should have had coordinates. This is done on the basis of whether the
C? track actually intersected each layer, and upon whether the wire at
C? the intersection point was "alive".
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "itwicc.h"
#include "frftjj.h"
#include "iwstjj.h"
#include "alcons.h"
C-----------------------------------------------------------------------
      DIMENSION IV(*)
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IER=1
      KFRFT = NLINK('FRFT',0)
      IF (KFRFT.EQ.0) GOTO 999
      IER = 0
C Get the parameters of the track
      IFRFT = KROW(KFRFT,NT)
      R0 = 1.0/RW(IFRFT+JFRFIR)
      TANL = RW(IFRFT+JFRFTL)
      PHI0 = RW(IFRFT+JFRFP0)
      D0 = RW(IFRFT+JFRFD0)
      D0Q   = D0*D0
      Z0 = RW(IFRFT+JFRFZ0)
C
C Get the IWST bank
      KIWST = IW(NAMIND('IWST'))
C
C Loop over the layers of the ITC and find their intersection points wit
C the track.
      DO 50 LAY = 1,8
        IV(LAY) = 0
        RAD   = RWIRIT(LAY)
C Check that there is an intersection.
        IF (ABS(D0).GT.RAD.OR.ABS(2.0*R0-D0).LT.RAD) GOTO 50
        RADQ  = RAD*RAD
        FACT1 = RADQ - D0Q
        FACT2 = 0.5*FACT1/(R0-D0)
C Get the phi coordinate of the intercept.
        SINA  = (FACT2-D0)/RAD
        IF(ABS(SINA).GT.1.0) GOTO 50
        PHIT = PHI0 + ASIN(SINA)
        PHIT = MOD(PHIT+TWOPI,TWOPI)
C Get the z coordinate of the intersept.
        SPSI2 = 0.5*FACT2/R0
        IF (SPSI2.LT.0.0.OR.SPSI2.GT.1.0) GOTO 50
        PSI = ASIN(SQRT(SPSI2))
        ZT  = Z0 + 2.0*PSI*ABS(R0)*TANL
        IF(ABS(ZT).GT.WZMXIT) GOTO 50
C
C Find wire number at the intersection.
C First note cell size and phi shift at the beginning of the 1st cell.
        DLN  = TWOPI/FLOAT(NWIRIT(LAY))
        SHFN = PHWRIT(LAY) - DLN*0.5
C
        PHIT = PHIT - SHFN
        PHIT = AMOD(PHIT+TWOPI,TWOPI)
        IWIRE = INT(PHIT/DLN) + IWIRIT(LAY) + 1
C See if this wire is alive.
C   Loop over the list of dead wires.
        IF (KIWST.GT.0) THEN
          DO 20 I = 1,LROWS(KIWST)
            IF (IWIRE.EQ.ITABL(KIWST,I,JIWSIW)) THEN
              IFLG = ITABL(KIWST,I,JIWSFL)
              IF(IFLG.EQ.1.OR.IFLG.EQ.4) GOTO 50
            ENDIF
   20     CONTINUE
        END IF
C Note that this wire should have produced a coordinate on the track.
        IV(LAY) = 1
   50 CONTINUE
  999 CONTINUE
      END
#endif
