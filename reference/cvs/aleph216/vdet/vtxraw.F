      SUBROUTINE VTXRAW(TPAR,RADI,GPOS0,PTRK,IERR)
C-----------------------------------------------------------------------
C! Compute a crude VDET track extrapolation point
CKEY VDET TRACK
C!
C!  - Calculate Z and phi for a given R.
C!
C!   AUTHORS  :- S. MENARY   JAN.  8 1990
C!               J. LAUBER   MAY  16 1990
C!               D. BROWN    OCT  14 1990
C!               D. Brown  13-10-93  Rewritten to avoid
C!                                   recurrant problems
C!   Inputs:
C!           TPAR  = FRFT- track parameters
C!           RADI  = Radius at which to calculate the co-ordinates
C!
C!   Outputs:
C!           GPOS0   = XYZ position of track at radius radi
C!           PTRK    = phi of track at radius radi
C!           IERR    = 0     o.k.
C!                   = 1     problem
C!
C!======================================================================
#ifndef DOC
C
C  Inputs+outputs
C
      REAL TPAR(5),RADI,PTRK,GPOS0(3)
      INTEGER IERR
C
C  Local variables
C
      DOUBLE PRECISION OMEGA,TANLAM,PHI0,D0,Z0,RTRK
      DOUBLE PRECISION DELTA,COSWS,SINWS
      DOUBLE PRECISION COSP0,SINP0,SINP,COSP
      DOUBLE PRECISION PHI,TARC
      REAL PI,TWOPI
      DATA PI/3.1415926/,TWOPI/6.2831853/
C
C-----------------------------------------------------------------------
C  Preset flag to success
C
      IERR  = 0
C
C  pack input parameters into local variables
C
      OMEGA = TPAR(1)
      TANLAM = TPAR(2)
      PHI0 = TPAR(3)
      D0 = TPAR(4)
      Z0 = TPAR(5)
C
C  Reject the unphysical cases; first, perfect concentric circles
C
      DELTA = 1.-D0*OMEGA
      IF(DELTA.EQ.0.0)THEN
         IERR=1
         GOTO 1000
      END IF
C
C  Next, check for intersection
C
      COSWS = ( 1.0 + DELTA**2 - (RADI*OMEGA)**2)/(2.0*DELTA)
      IF(ABS(COSWS).GT.1.0)THEN
         IERR=2
         GOTO 1000
      END IF
      SINWS = SIGN(SQRT(1.0-COSWS**2),OMEGA)
      PHI = ATAN2(SINWS,COSWS)
      TARC = PHI/OMEGA
C
C  Compute the track position at this point
C
      RTRK = 1.0/OMEGA
      SINP0 = SIN(PHI0)
      COSP0 = COS(PHI0)
      SINP = SINWS*COSP0 + COSWS*SINP0
      COSP = COSWS*COSP0 - SINWS*SINP0
      GPOS0(1) =  RTRK*SINP-(RTRK-D0)*SINP0
      GPOS0(2) = -RTRK*COSP+(RTRK-D0)*COSP0
      GPOS0(3) = Z0 + TANLAM*TARC
C
C  Get phi at that point
C
      PTRK = ATAN2(GPOS0(2),GPOS0(1))
      IF (PTRK .LT. 0.0) THEN
        PTRK = PTRK+TWOPI
      END IF
 1000 RETURN
      END
#endif
