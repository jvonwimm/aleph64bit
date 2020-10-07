      SUBROUTINE IGETTP(INTPC,SPIRAL,PAR,COV,OUT,DSPIR,IGOOD,MORE)
C-----------------------------------------------------------------------
C!   See if TPC track INTPC is a good candidate for extension and if it
C!   is then store the helix parameters decribing it.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- J. Sedgbeer 87/10/30
C!    Modified    :- I. Tomalin  89/02/28
C!    Modified    :- D. Cinabro  91/05/29
C!
C!   Input:
C!     - INTPC  /I4  : number of track in TGFT bank
C!     - SPIRAL /L4  : =FALSE if one is studying the 1st arc of a track.
C!                     =TRUE otherwise (N.B. spiralling tracks can have
C!                        many arcs/orbits).
C!     need commons /BCS/    for TGFT bank
C!          params  ALCONS
C!                  TGFTJJ
C!                  TARCJJ
C!                  TGTLJJ
C!                  TPCOJJ
C!
C!   Output:
C!     - PARi   /R4  : TPC track parameters
C!                     i=1,6  sig*r0,tanl,phi0,d0,z0,TPC track no.
C!     - COVi   /R4  : TPC track covariance matrix (as in TGFT bank).
C!     - OUT    /L4  : .TRUE. if particle moved outwards though the
C!                     detector along the current arc.
C!     - DSPIR  /R4  : Approximately, this is the path length which
C!                     the particle has traversed.
C!     - IGOOD  /L4  : .TRUE. if this TPC track is a good candidate for
C!                     extension
C!     - MORE   /L4  : .TRUE. if there remain further arcs of this
C!                      track to be explored.
C!
C!   called by : ITCTPC
C!   calls     : none
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
#include "alcons.h"
#include "tgftjj.h"
#include "tarcjj.h"
#include "tgtljj.h"
#include "tpcojj.h"
#include "t1ftjj.h"
C-----------------------------------------------------------------------
      LOGICAL SPIRAL,OUT,IGOOD,MORE
      DIMENSION PAR(*),COV(*)
C
C Cuts on TPC tracks:
      DATA  AD0MX/20./,  AZ0MX/150.0/,  ATLMX/4./
C AD0MX = Maximum value of ABS(D0) cm.
C AZ0MX = Maximum value of ABS(Z0) cm.
C ATLMX = Maximum value of Tanl. Track goes thro' 3 or more TPC layers
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      KTGFT = IW(NAMIND('TGFT'))
      JTGFT = KROW(KTGFT,INTPC)
      KT1FT = IW(NAMIND('T1FT'))
      KTARC = NLINK('TARC',0)
      IF (.NOT.SPIRAL) THEN
        PAR(1) = 1./RW(JTGFT+JTGFIR)
        PAR(2) = RW(JTGFT+JTGFTL)
        PAR(3) = RW(JTGFT+JTGFP0)
        PAR(4) = RW(JTGFT+JTGFD0)
        PAR(5) = RW(JTGFT+JTGFZ0)
        PAR(6) = INTPC
        DO 25 I = 1,15
          COV(I) = RW(JTGFT+JTGFEM+I-1)
   25   CONTINUE
        PATH = 0.0
        OUT = .TRUE.
C Find out if this track spirals.
        MORE = .FALSE.
        IF (KTARC.EQ.0.OR.KT1FT.EQ.0) GOTO 900
C
C Must loop on T1FT to find the arcs
C
        DO 100 IT1 = 1,LROWS(KT1FT)
          IF (INTPC.EQ.ITABL(KT1FT,IT1,JT1FTG)) THEN
            NTCA = ITABL(KT1FT,IT1,JT1FTC)
            DO 27 N = 1,LROWS(KTARC)
              NTCAP = ITABL(KTARC,N,JTARTN)
              IF (NTCA.NE.NTCAP) GOTO 27
              MORE = .TRUE.
              NARC = N
C Store the track radius for future reference.
              TRAD = PAR(1)
C Find out if the TGFT track was going outwards or not, by looking at
C the NEXT arc (therefore opposite sign to normal).
              OUT = ITABL(KTARC,NARC,JTARAN).LT.0
C If it is going inwards, it must have taken some time to get out ...
              IF (.NOT.OUT) PATH = ABS(2.0*TRAD)
              GOTO 900
   27       CONTINUE
          ENDIF
  100   CONTINUE
      ELSE
        JTARC = KROW(KTARC,NARC)
C Add on the path length of the last arc.
        PATH = PATH + PI*ABS(TRAD)
C Find out if this arc was going outwards or not.
        OUT = IW(JTARC+JTARAN).GT.0
        PAR(1) = 1.0/RW(JTARC+JTARIR)
        PAR(2) = RW(JTARC+JTARTL)
        PAR(3) = RW(JTARC+JTARP0)
        PAR(4) = RW(JTARC+JTARD0)
        PAR(5) = RW(JTARC+JTARZ0)
        PAR(6) = INTPC
        DO 30 I = 1,15
          COV(I) = 0.0
   30   CONTINUE
        COV(1) = 1.0/(1.0E-9 + RW(JTARC+JTARER))
        COV(3) = 1.0/(1.0E-9 + RW(JTARC+JTARER+1))
        COV(6) = 1.0/(1.0E-9 + RW(JTARC+JTARER+2))
        COV(10) = 1.0/(1.0E-9 + RW(JTARC+JTARER+3))
        COV(15) = 1.0/(1.0E-9 + RW(JTARC+JTARER+4))
C Store arc radius for future reference.
        TRAD = PAR(1)
C See if there are any more arcs on this spiral.
        MORE = .FALSE.
        IF (NARC.LT.LROWS(KTARC)) THEN
          NARC = NARC + 1
          NTCAP = ITABL(KTARC,NARC,JTARTN)
          IF (NTCA.EQ.NTCAP) MORE = .TRUE.
        END IF
      END IF
C
  900 CONTINUE
      DSPIR = PATH
C
C See if is worth trying to extend this track.
      IGOOD = .FALSE.
      IF(ABS(PAR(4)).GT.AD0MX) GOTO 999
      IF(ABS(PAR(5)).GT.AZ0MX) GOTO 999
      IF(ABS(PAR(2)).GT.ATLMX) GOTO 999
      IGOOD = .TRUE.
  999 CONTINUE
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) WRITE(LDEBRL,1000) INTPC,IGOOD,
     +(PAR(II),II=1,5),SPIRAL,OUT,DSPIR
 1000 FORMAT(' IGETTP : TPC track no.=',I3,' IGOOD=',L1,' PAR=',5E12.3,
     +' SPIRAL=',L1,' OUT=',L1,' DSPIR=',E10.3)
      END
#endif
