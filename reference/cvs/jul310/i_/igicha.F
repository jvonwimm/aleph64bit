      SUBROUTINE IGICHA(PAR,COV,SPIRAL,OUT,DSPIR)
C-----------------------------------------------------------------------
C!   Get ITC chain from TPC track parameters.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- J. Sedgbeer 88/05/09
C!    Modified    :- I. Tomalin  89/06/07
C!
C!   Input:
C!     PARi   /R4 : TPC track parameters.
C!                 i=1,6 (sig*r0,tanl,phi0,d0,z0,TPC track no.)
C!     COVi   /R4 : TPC track covariance matrix (as in TGFT bank).
C!     SPIRAL /L4 : =FALSE if one is studying the 1st arc of a track.
C!                  =TRUE otherwise (N.B. spiralling tracks can have
C!                    many arcs/orbits).
C!     OUT    /L4  : .TRUE. if particle moved outwards though the
C!                     detector along the current arc.
C!     DSPIR  /R4  : Approximately, this is the path length which
C!                     the particle has traversed.
C!         commons: /BCS/
C!                  /ITRKPA/ tracking consts.
C!                  /ITRKCC/ scattering radius between ITC and TPC.
C!                  /IWBNKT/ pointers to work banks.
C!         params.:  ALCONS
C!
C!   Output:
C!        commons:  /IROADC/
C!                  /ITTIME/
C!
C!   called by : ITCTPC
C!
C!   calls :  INSECT   Intersection of TPC track with ITC layer
C!            ISRCH    search ITC coords.
C!            ITLNK1   form links
C!            ITREE    form tree
C!            IFIND1   find paths
C!            IFITP    fit paths
C!
C!   Libraries required: none
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
#include "rcurnt.h"
#include "itrkcc.h"
#include "itrkpa.h"
#include "iwbnkt.h"
#include "iroadc.h"
#include "ittime.h"
C-----------------------------------------------------------------------
      LOGICAL SPIRAL,OUT
      DIMENSION PAR(*),COV(*)
      DIMENSION XPAR(3),ROAD(2),IWWIN(2)
C-----------------------------------------------------------------------
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) WRITE(LDEBRL,1000) PAR(6)
 1000 FORMAT(1H0,'IGICHA: TPC track number',F5.0)
C
      ITRY = 3
C
      R0   = PAR(1)
      D0   = PAR(4)
C Find the TPC track's phi coord. at intersection with scattering radius
      SINA = (RSCAIT**2+D0**2-2.0*R0*D0)/(2.0*RSCAIT*(R0-D0))
      IF (ABS(SINA).GT.1.0) GOTO 999
      TPHIS = PAR(3)+ASIN(SINA)
      TPHIS = MOD(TPHIS+TWOPI,TWOPI)
C
C Calculate the derivatives of TPHIS w.r.t. 1/PAR(1), PAR(3) and PAR(4).
C First get the derivative of TPHIS w.r.t. SINA and of SINA w.r.t.
C 1/PAR(1) and PAR(4). (N.B. COV is in terms of 1/PAR(1)).
C
      DPDSA = 1.0/SQRT(1.0-SINA**2)
      DSDT1 = -(SINA + D0/RSCAIT)/(R0 - D0)
      DSDT1 = -DSDT1*R0**2
      DSDT4 = SINA/(R0 - D0) - 1.0/RSCAIT
C
      DPDT1 = DPDSA*DSDT1
      DPDT3 = 1.0
      DPDT4 = DPDSA*DSDT4
C Calculate error on TPHIS due to errors on TPC track (also include some
C protection, incase COV was not positive definate).
      ERPHI = DPDT1*(COV(1)*DPDT1 + 2.0*COV(4)*DPDT3 + 2.0*COV(7)*DPDT4)
     +      + DPDT3*(COV(6)*DPDT3 + 2.0*COV(9)*DPDT4)
     +      + DPDT4*COV(10)*DPDT4
      ERPHI = SQRT(ABS(ERPHI))
C The next line displays a lack of faith in the TPC covariance matrix.
      ERPHI = MIN(ERPHI,0.03)
C
C Calculate the angle of the track at the scattering radius.
      SINB = (0.5*(RSCAIT**2-D0**2)+R0*D0)/(RSCAIT*R0)
      IF(ABS(SINB).GT.1.0) GOTO 999
      TPENA = ASIN(SINB)
C
C Calculate the r.m.s. scattering angle of the track.
      PT = FRHOIT*ABS(R0)
      THSCT = FSCAIT/(PT*SQRT(COS(TPENA)*SQRT(1.0+PAR(2)**2)))
C Project it into the r-phi plane.
      THSCT = THSCT*SQRT(1.0+PAR(2)**2)
C
C Search outer layers of ITC for hits. Fill /IROADC/
C
      NHIT = 0
      LAST = NLAYIR + 1
      CALL VZERO(NHITIR,NLAYIR)
C
      DO 25 IL=NLAYIR,1,-1
C   See if it will be possible to get enough links on the track.
        IF (IL+NHIT-1.LT.MNLINK(1)) GOTO 35
C
C   Get intersection of track with ITC layer IL and set road widths.
      IF( IFITTI ) CALL ALTIME ( TIME1 )
        CALL INSECT(ITRY,IL,PAR,COV,TPHIS,THSCT,XPAR,ROAD,IWWIN,IERR)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
        TNSECT=TNSECT+(TIME2-TIME1)
C   Stop if there was no intersection.
        IF(IERR.NE.0) GOTO 35
C
C   Search for ITC coordinates in the road on this layer.
C
      IF( IFITTI ) CALL ALTIME ( TIME1 )
        CALL ISRCH(ITRY,2,IL,XPAR,ROAD,IWWIN,PAR,OUT,DSPIR)

      IF( IFITTI ) CALL ALTIME ( TIME2 )
        TSRCH=TSRCH+(TIME2-TIME1)
C
        IF (NHITIR(IL).GT.0) THEN
          LAST=IL
          NHIT = NHIT + 1
        END IF
C
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.5) THEN
          NC = NHITIR(IL)
          WRITE(LDEBRL,1001) IL,NC,(IHITIR(K,IL),K=1,NC)
 1001     FORMAT(' IGICHA: layer',I2,' #hits',I3,' coords.',10(I5,:))
        ENDIF
C   Check that there have not been too many empty layers recently.
        IF (LAST-IL.GT.MXJMPD(1)) GOTO 35
   25 CONTINUE
   35 CONTINUE
C
C See if enough links were found.
      IF (NHIT-1.LT.MNLINK(1)) GOTO 999
C
C Form links.
C
C Note the TPC track's curvature and the error**2 on it.
      TCURV = 1.0/PAR(1)
      TCUER = COV(1)
C
      IF( IFITTI ) CALL ALTIME ( TIME1 )
      CALL ITLNK1(ITRY,TCURV,TCUER,TPHIS,ERPHI,IERR)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
      TTLNK1=TTLNK1+(TIME2-TIME1)
      IF (IERR.NE.0) GOTO 999
C
C Form tree
C
      IF( IFITTI ) CALL ALTIME ( TIME1 )
      CALL ITREE(ITRY,1,IERR)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
      TTREE1=TTREE1+(TIME2-TIME1)
      IF (IERR.NE.0) GOTO 999
C
C Find paths in tree
C
      IDTPC = INT(PAR(6)+0.00001)
      IF( IFITTI ) CALL ALTIME ( TIME1 )
      CALL IFIND1(ITRY,IDTPC,SPIRAL)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
      TFIND1=TFIND1+(TIME2-TIME1)
C
C Drop some work banks.
      IF (INNXIW.NE.0) CALL WDROP(IW,INNXIW)
      IF (IOFNIW.NE.0) CALL WDROP(IW,IOFNIW)
      IF (INXLIW.NE.0) CALL WDROP(IW,INXLIW)
C
  999 CONTINUE
      END
#endif
