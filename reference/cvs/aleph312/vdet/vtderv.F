      SUBROUTINE VTDERV(TPAR,RCW,VHAT,RERRP)
C----------------------------------------------------------------------
C!  Compute the derivatives of local position wrt. track parameters
CKEY VDET TRACK
C!
C!   Author   :- Jochen A. Lauber      23-MAY-1990
C!               modified by David Brown 16-10-90
C!               Remodified by G. Redlinger 26-10-90
C!                 Fixed some derivative calculations and declared
C!                  RSXY to be REAL*8 rather than REAL*4.
C!               Re-remodified by G. Redlinger 30-1-91
C!                 Re-fixed some derivatives according to the
C!                  correct calculations of T. Lohse.  This time
C!                  it's really correct!
C!               Modif A. Bonissent 14-Oct. 1997 :
C!                  Improve the protection against rounding errors,
C!                  replace 1. by 0.99999 so that cos is never 0.
C!
C!   Inputs: TPAR(5)  = track parameters as in FRFT bank
C!           RCW(2)   = ALEPH coordinate of wafer center in XY plane
C!                      PASSED IN DOUBLE PRECISION.
C!           VHAT(2)  = X and Y coordinates of the vector
C!                        perpendicular to the intersected wafer.
C!                        PASSED IN DOUBLE PRECISION
C!
C!   Outputs: RERRP = derivatives of U,W wrt track parameters
C!
C!   Libraries required: None
C!
C!   Description
C!   ===========
C?   This routine computes the derivatives of the track extrapolation
C?   point (in local coordinates) with respect to the track parameters.
C?   For simplicity, it is assumed that the phi strips are parallel to
C?   the z-axis (i.e. w = z).
C!
C!======================================================================
#ifndef DOC
      SAVE
C
C  Inputs
C
      DOUBLE PRECISION RCW(2),VHAT(2)
      REAL TPAR(5)
C
C  Outputs
C
      REAL RERRP(5,2)
C
C  LOCAL VARIABLES
C
      DOUBLE PRECISION RTRK,OMEGA,TL,PHI0,D0,D0MR
      DOUBLE PRECISION KAPPA,ADOTV,SXY
      DOUBLE PRECISION DSDO,DSDD,DSDP
      DOUBLE PRECISION PK,WS,WSP,WSPK
      DOUBLE PRECISION CPK,SPK,CWS,SWS,CWSP,SWSP,CWSPK,SWSPK
      DOUBLE PRECISION BIG,PI,TWOPI
      DOUBLE PRECISION NORM,VVHAT(2)
      DOUBLE PRECISION THING
C
      PARAMETER (BIG=1.D20)
      PARAMETER(PI=3.141592653589D0)
      PARAMETER(TWOPI=2.0D0*PI)
C
C
C   Get the track parameters
C
      OMEGA  = TPAR(1)
      IF(TPAR(1).NE.0.0) THEN
        RTRK  = 1.0/OMEGA
      ELSE
        RTRK = SIGN(BIG,OMEGA)
      ENDIF
      TL     = TPAR(2)
      PHI0   = TPAR(3)
      D0     = TPAR(4)
C
C  Compute the angle that the wafer normal (ignoring the z-component)
C  makes with respect to the x-axis.
C
      KAPPA = ATAN2(VHAT(2),VHAT(1))
      IF(KAPPA.LT.0.) KAPPA = KAPPA+TWOPI
C
C  Normalize VHAT
C
      NORM = SQRT(VHAT(1)**2+VHAT(2)**2)
      VVHAT(1) = VHAT(1)/NORM
      VVHAT(2) = VHAT(2)/NORM
C
C  Calculate the transverse path length to the intersection point in
C  the approximation that the phi strips are parallel to the z-axis.
C  Note: although we have calculated the exact value of the transverse
C  path length elsewhere, we use the approximate path length here for
C  consistency with the assumptions in our calculations of the
C  derivatives.
C
      D0MR = D0 - RTRK
      ADOTV = RCW(1)*VVHAT(1) + RCW(2)*VVHAT(2)
C
      PK = PHI0 - KAPPA
      SPK = SIN(PK)
      CPK = COS(PK)
C
      THING = OMEGA*(ADOTV-D0MR*SPK)
      IF(ABS(THING).GT.0.99999D0) THEN
        THING = SIGN(0.99999D0,THING)
      ENDIF
      WS = -PK + ASIN( THING )
C
C  Make sure WS is between -pi and pi
C
10    IF(WS.LT.-PI) THEN
        WS = WS+TWOPI
        GO TO 10
      ELSEIF(WS.GT.PI) THEN
        WS = WS-TWOPI
        GO TO 10
      ENDIF
      SXY = WS/OMEGA
C
C  Now some other useful angles, their sines and cosines
C
      WSP = WS + PHI0
      WSPK = WSP - KAPPA
C
      CWS = COS(WS)
      SWS = SIN(WS)
      CWSP = COS(WSP)
      SWSP = SIN(WSP)
      CWSPK = COS(WSPK)
      SWSPK = SIN(WSPK)
C
C  Derivatives of the path length
C
C  ds/d(omega)
C
      DSDO = -RTRK*SXY + (RTRK**2)*(SWSPK-SPK)/CWSPK
C
C  ds/d(D0)
C
      DSDD = -SPK/CWSPK
C
C  ds/d(phi0)
C
      DSDP = -RTRK - D0MR*CPK/CWSPK
C
C  Now the derivatives for w
C  dw/d(omega) = dw/ds * ds/d(omega)
C  But dw/ds is just tan(lambda) for wafers parallel to the z-axis,
C    so we get....
C
      RERRP(1,2) = TL*DSDO
C
C  dw/d(tanl) = dz/d(tanl) = s
C
      RERRP(2,2) = SXY
C
C  dw/d(phi0) = dw/ds * ds/d(phi0)
C
      RERRP(3,2) = TL*DSDP
C
C  dw/d(D0) = dw/ds * ds/d(D0)
C
      RERRP(4,2) = TL*DSDD
C
C  Finally, dw/d(Z0) is 1 in the approximation that w=z+const.
C
      RERRP(5,2) = 1.0
C
C  Now we do the derivatives for u
C
C  du/d(omega)
C
      RERRP(1,1) = (RTRK**2) * (1.0-CWS)/CWSPK
C
C  du/d(phi0)
C
      RERRP(3,1) = -D0MR*SWS/CWSPK
C
C   du/d(D0)
C
      RERRP(4,1) = -CWS/CWSPK
C
C  The partial derivative of u doesn't depend on TL,Z0, assuming
C   w is parallel to the z-axis
C
      RERRP(2,1) = 0.
      RERRP(5,1) = 0.
C
C  That's all folks!
C
  999 RETURN
      END
#endif
