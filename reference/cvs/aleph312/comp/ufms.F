      SUBROUTINE UFMS(RADOUT, RADIN, VV0IN, COV)
C!-------------------------------------------------------------------
C! Calculates the contribution to the covariance matrix of a track
C! coming from multiple scattering from the radius RADOUT to the
C! radius RADIN using a realistic approximation for the VDET geometry.
C! This routine treats the multiple scattering in exactly the same way
C! as in UFTKAL, most of the code is lifted directly
C! from that routine.
CKEY VDET TRACK
C!
C!    AUTHOR: G. Taylor   28/9/92
C!
C!    INPUT:  VV0IN  =  input helix parameters
C!            RADIN  =  inner radius to go to
C!            RADOUT =  outer radius to start at
C!    OUTPUT: COV    =  5*5 track covariance matrix at RIN
C!
C!-------------------------------------------------------------------
#ifndef DOC
      DOUBLE PRECISION ROUT,RIN,RR0,SST,PH0,DD0,ZZ0,DZDS,CUR
      DOUBLE PRECISION SCACO,SCAFAC,CLAM2,CLAM,SXYROUT,SXYRIN
      DOUBLE PRECISION XLL,XL4,SXYTPC,XLOR,SXYITC,RMSO,RMSI,FACT
      DOUBLE PRECISION SUM,DEN,ALP,RAD,DELPHI,XL3
#include "vrldcom.h"
#include "bcs.h"
      REAL FIELD,VV0IN(6),COV(5,5),RADOUT,RADIN
      REAL ALFIEL
#include "bmacrod.h"
      DOUBLE PRECISION QPROC(5,5),Q(2,2)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
C -------------------------------------------------------------
       CALL VZERO(COV,25)
C-
C get the multiple scattering  material description from the database
C-
      CALL VRLDGT( IER)
      IF(IER.LT.0) RETURN
C
      CALL ABRUEV (KRUN,KEVE)
      FIELD=ALFIEL(DUM)
C protect against unphysical tracks
      IF(VV0IN(1)*VV0IN(4).GT.1.) RETURN
C
      ROUT=RADOUT
      RIN=RADIN
      RR0  = VV0IN(1)
      IF ( RR0 .EQ. 0. )   RR0 = .0000001
      SST  = 1.
      IF ( RR0 .LT. 0. )  SST = -1.
      PH0  = VV0IN(3)
      DD0  = VV0IN(4)
      ZZ0  = VV0IN(5)
      DZDS = VV0IN(2)
      CUR  = DABS(RR0)
C
      SCACO = 1000./0.29979/FIELD
      SCAFAC = 0.0141 * SCACO
C
C--->   GENERAL PARAMETER OF TRACK
      CLAM2   = 1. + DZDS**2
      CLAM   = DSQRT(CLAM2)
      SCACO  = (SCAFAC*RR0)**2 / CLAM2
C--->  INITIALIZE M.S. COVARIANCE MATRICES
      DO  221  J = 1, 5
        QPROC(J,1) = 0.
        QPROC(J,2) = 0.
        QPROC(J,3) = 0.
        QPROC(J,4) = 0.
        QPROC(J,5) = 0.
  221 CONTINUE
C--->   LOOP OVER COORDINATE PAIRS
C if last coordinaate and no swimming
      Q(1,1) = 0.
      Q(1,2) = 0.
      Q(2,1) = 0.
      Q(2,2) = 0.
      DELPHI=0.5*RR0*DSQRT(DABS((ROUT**2-DD0**2)/(1.-RR0*DD0)))
      IF(ABS(DELPHI).GT..9999999) GOTO 777
      SXYROUT = 2./RR0*DASIN(DELPHI)
      DELPHI=0.5*RR0*DSQRT(DABS((RIN**2-DD0**2)/(1.-RR0*DD0)))
      IF(ABS(DELPHI).GT..9999999) GOTO 777
      SXYRIN = 2./RR0*DASIN(DELPHI)
      XLL = SXYROUT-SXYRIN
C--->    CALCULATE MATRICES
C
C--->    GAS OF TPC AND TPC/ITC WALL
      XL4 = 0.
      IF ( ROUT .GT. UKRTPC )  THEN
        XL4 = XLL
        IF ( RIN .LE. UKRTPC ) THEN
          DELPHI=0.5*RR0*DSQRT(DABS((UKRTPC**2-DD0**2)/(1.-RR0*DD0)))
          IF(ABS(DELPHI).GT..9999999) GOTO 777
          SXYTPC = 2./RR0*DASIN(DELPHI)
          XL4 = SXYROUT-SXYTPC
          XLOR = UKRTPC*RR0*CLAM/DSIN(RR0*SXYTPC)/(1.-DD0*RR0)
        ENDIF
C---> EFFECTIVE LENGTH DUE TO MULTIPLE SCATTERING
        XL4 = XL4*(1.+0.35355*XL4*UKSPTPC*SCACO)
        XL4 = XL4 * CLAM
        Q(1,1) = SCACO*UKSPTPC*XL4**3/3.
        Q(1,2) = SCACO*UKSPTPC*XL4*XL4*.5
        Q(2,1) = Q(1,2)
        Q(2,2) = SCACO*UKSPTPC*XL4
        IF ( RIN .LE. UKRTPC )
     +      Q(2,2) = Q(2,2) + SCACO*UKSTPC*XLOR
      ENDIF
C--->    GAS OF ITC AND ITC INNER WALL
      XL3 = 0.
      IF ( RIN .LT. UKRTPC ) THEN
        IF ( RIN .LE. UKRITC .AND. ROUT .GT. UKRITC ) THEN
          DELPHI=0.5*RR0*DSQRT(DABS((UKRITC**2-DD0**2)/(1.-RR0*DD0)))
          IF(ABS(DELPHI).GT..9999999) GOTO 777
          SXYITC = 2./RR0*DASIN(DELPHI)
          XLOR = UKRITC*RR0*CLAM/DSIN(RR0*SXYITC)/(1.-DD0*RR0)
          XL3 = SXYROUT - SXYITC
          IF ( ROUT .GT. UKRTPC ) XL3 = SXYTPC - SXYITC
        ELSEIF ( RIN .GT. UKRITC .AND. ROUT .GT. UKRTPC) THEN
          XL3 = XLL - SXYROUT + SXYTPC
        ELSEIF ( RIN .GT. UKRITC .AND. ROUT .LE. UKRTPC) THEN
          XL3 = XLL
        ENDIF
C---> EFFECTIVE LENGTH DUE TO MULTIPLE SCATTERING
        XL3 = XL3*(1.+0.35355*XL3*UKSPITC*SCACO)
        XL3 = XL3 * CLAM
        Q(1,1) = Q(1,1) + 2.*XL3*Q(1,2) + XL3*XL3*Q(2,2)
        Q(1,2) = Q(1,2) + XL3*Q(2,2)
        Q(1,1) = Q(1,1) + SCACO*UKSPITC*XL3**3/3.
        Q(1,2) = Q(1,2) + SCACO*UKSPITC*XL3*XL3*.5
        Q(2,1) = Q(1,2)
        Q(2,2) = Q(2,2) + SCACO*UKSPITC*XL3
        IF ( RIN .LE. UKRITC .AND. ROUT .GT. UKRITC )
     +      Q(2,2) = Q(2,2) + SCACO*UKSITC*XLOR
      ENDIF

C--->    MATERIAL OF VDET
      RMSO = ROUT
      IF ( ROUT .GT. UKRITC )    RMSO = UKRITC
      RMSI = RIN
C--->  SWIMMING IS REQUIRED
      IF ( RMSO .GT. DABS(RMSI) )
     +       CALL UFVDMS( FIELD, RR0, DZDS, PH0, DD0, ZZ0,
     +       RMSI, RMSO, Q )
C           THESE ARE THE MULTIPLE SCATTERING PARAMETERS FOR THE
C           GLOBAL FIT PARAMETERS ON THE WAY TO THE ORIGIN
      FACT = DSQRT(1.+DZDS**2)
      SUM  = 1. - RR0*DD0/FACT**2
      DEN  = 1. - DD0*RR0
      ALP  = 1./DSQRT(1.+1./DZDS**2)
      IF ( DZDS .LT. 0. )   ALP = - ALP
      QPROC(1,1) = (RR0*DZDS)**2*Q(2,2)
      QPROC(1,2) = RR0*DZDS*FACT**2 * Q(2,2)
      QPROC(2,1) = QPROC(1,2)
      QPROC(1,3) = -RR0**2*DZDS*ALP/DEN * Q(1,2)
      QPROC(3,1) =  QPROC(1,3)
      QPROC(1,5) = -RR0*DZDS*SUM/DEN*FACT * Q(1,2)
      QPROC(5,1) = QPROC(1,5)
      QPROC(2,2) = FACT**4 * Q(2,2)
      QPROC(2,3) = -RR0*ALP*FACT**2/DEN * Q(1,2)
      QPROC(3,2) = QPROC(2,3)
      QPROC(2,5) = -FACT**3*SUM/DEN * Q(1,2)
      QPROC(5,2) = QPROC(2,5)
      QPROC(3,3) = (FACT/DEN)**2 * Q(2,2) +
     +                   (RR0*ALP/DEN)**2 * Q(1,1)
      QPROC(3,4) = +FACT/DEN * Q(1,2)
      QPROC(4,3) = QPROC(3,4)
      QPROC(3,5) = DD0*DZDS*FACT**2/DEN**2 * Q(2,2)
     +                 + RR0*DZDS*SUM/DEN**2*Q(1,1)
      QPROC(5,3) = QPROC(3,5)
      QPROC(4,4) = Q(1,1)
      QPROC(4,5) = +DD0*DZDS*FACT/DEN * Q(1,2)
      QPROC(5,4) = QPROC(4,5)
      QPROC(5,5) = (DD0*DZDS*FACT/DEN)**2 * Q(2,2)
     +                   + (FACT*SUM/DEN)**2 * Q(1,1)
 777  CONTINUE
      DO  271  I = 1, 5
        DO  271  J = 1, 5
          COV(I,J)=REAL(QPROC(I,J))
  271 CONTINUE
      RETURN
      END
#endif
