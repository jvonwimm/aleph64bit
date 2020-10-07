      SUBROUTINE SICLC2(RACLU,PHCLU,DRA,DPH,RACOR,PHCOR)
C----------------------------------------------------------------
C!  - Cluster position correction in Sical
C!
C!   Author   :         Bertrand VALLAGE, adapted from SICLCO 25-01-94
C!
C!   Inputs:  SiCAL shower cluster radial and phi positions
C!            raclu = Cluster radius as from barycenter method
C!            phclu = Cluster Phi postion as from barycenter method
C!
C!   Outputs: Returns positions corrected for pad curvature and
C!            barycenter algorithm,racor and phcor.
C!
C!   Warning : Raclu, racor, phclu, phcor are in a referential
C!             perfectly centered assuming SiCAL theoretical geometry
C!             (assumed to get these corrections)
C!
C?   Description: Position relative to pad's lowest edge is used
C?   ===========  to calculate corrections using periodic function
C?                with phase shift. The abberation due to pad curvature
C?                is corrected in R using 1/R**2 variation of the effect
C?
C!======================================================================
#ifndef DOC
* constants
      DOUBLE PRECISION PI, TWOPI, PIBY16, FISTEP
      PARAMETER (PI=3.141592653589)
      PARAMETER (TWOPI = 2.*PI , PIBY16 = PI/16.)
      PARAMETER (FISTEP = PI / 48.)
* Fit of radial dist for TOY MC file ---> MCTOYRE2.HIST
* Racoon correction coefficients
      DOUBLE PRECISION ALPHA,PULSE,PHASR,POWER,OFFSET,BETA
      PARAMETER (ALPHA =  0.741858D-01)
      PARAMETER (PULSE =  0.119848D+02)
      PARAMETER (PHASR =  0.178303D+01)
      PARAMETER (POWER =  0.495441D+00)
      PARAMETER (OFFSET=  0.116909D+01)
      PARAMETER (BETA  = -0.194828D+00)
* Leakage correction coefficients
      DOUBLE PRECISION AMPIN,OFFIN,AMPOUT,OFFOUT
      PARAMETER (AMPIN =  0.177078D+01)
      PARAMETER (OFFIN =  0.571784D+01)
      PARAMETER (AMPOUT=  0.126537D+01)
      PARAMETER (OFFOUT=  0.150326D+02)
* Fit of TOY MC file ---> MCTOYRE2.HIST
* Phiclus correction coefficients
      DOUBLE PRECISION AMPL1,AMPL2,AMPL3,PHASFI
      PARAMETER (AMPL1 =  0.132512D-01)
      PARAMETER (AMPL2 =  0.143896D-01)
      PARAMETER (AMPL3 =  0.145191D-01)
      PARAMETER (PHASFI=  0.883701D+00)
* variables
      REAL RACLU,RACOR,DRA
      REAL PHCLU,PHCOR,DPH
      DOUBLE PRECISION RCLUS,RCLMOD,RTRUE,RSNAKE,RLEAK
      DOUBLE PRECISION PCLUS,PHI,PBARY,PTRUE
*
* Radius correction
* barycenter algorithm and pad curvature
      RCLUS = DBLE(RACLU)
      RCLMOD = DBLE(DRA)
      RSNAKE=ALPHA*DSIN(PULSE*RCLUS+PHASR+BETA*DSIN(TWOPI*RCLMOD))
     +     /RCLUS**POWER + OFFSET/RCLUS**2
* leakage effects on the sides
      RLEAK = ( 1.D0 - DTANH( AMPIN  * (RCLUS - OFFIN ) ) )
     +      + (-1.D0 - DTANH( AMPOUT * (RCLUS - OFFOUT) ) )
* corrected R ->
      RTRUE = RCLUS - RSNAKE - RLEAK
      RACOR = REAL(RTRUE)
*
* Phi correction
      PCLUS = DBLE(PHCLU)
      PHI = DBLE(DPH)
      PBARY =
     +  AMPL1*DSIN(TWOPI*(PHI-1./3.)+PHASFI*DSIN(TWOPI*(PHI-1./3.)))
     + +AMPL2*DSIN(TWOPI*(PHI      )+PHASFI*DSIN(TWOPI*(PHI      )))
     + +AMPL3*DSIN(TWOPI*(PHI+1./3.)+PHASFI*DSIN(TWOPI*(PHI+1./3.)))
* corrected Phi ->
      PTRUE = DMOD(PCLUS - PBARY + TWOPI,TWOPI)
      PHCOR = REAL(PTRUE)
*
  999 RETURN
      END
#endif
