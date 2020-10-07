      SUBROUTINE QUPAIR(itk,z,p1,p2)
C----------------------------------------------------------------------
C! Simulates e+e- pair production by photons (adaptation from GEANT315)
C
C  Input :  -- ITK,           the ALPHA index of the photon
C           -- Z,             the atomic number of the material
C  Output:  -- p1(4),p2(4)    the e+e- momenta-energies
C
C        The secondary electron energies are sampled using the
C        Coulomb corrected BETHE-HEITLER cross-sections.For this the
C        modified version of the random number techniques of
C        BUTCHER and MESSEL (NUCL.PHYS,20(1960),15) are employed.
C
C        NOTE :
C        (1) Effects due to the breakdown of the BORN approximation at
C            low energies are ignored.
C        (2) The differential cross-section implicitly takes account
C            of pair production in both nuclear and atomic electron
C            fields. However, triplet production is not generated.
C
C             Authors              G.Patrick, L.Urban
C             Modified for QUFSIM  Patrick Janot, 02 Nov 1993
C----------------------------------------------------------------------
      DIMENSION p1(4), p2(4), q1(4), q2(4)
      DATA EMASS/0.511E-3/, EMAS2/1.022E-3/, ALFA/7.29735E-3/
#include "qcde.h"
#include "qmacro.h"
C
      CALL vzero(p1(1),4)
      CALL vzero(p2(1),4)
      CALL vzero(q1(1),4)
      CALL vzero(q2(1),4)
C
C No pair production if not enough energy (E_gamma < 2m_e)
C
      egam   = qe(itk)
      IF ( egam .LT. EMAS2 ) GOTO 999
C
C For low energy photons approximate the electron energy by sampling
C sampling from a uniform distribution in the interval EMASS -> EGAM/2.
C
      IF ( egam .LE. 2.1E-03 ) THEN
         EEL1   = EMASS + (RNDM(1)*(EGAM/2. - EMASS))
         GO TO 60
      ENDIF
C
C Compute constants for the energy spectrum
C
      X      = (Z*ALFA)**2
      FC     = (( - 0.002 * X + 0.0083) * X - 0.0369) * X + 0.20206
      FC     = X * (FC + 1. / (1. + X))
      Z3     = Z**0.333333
      F      = 8.*LOG(Z3)
      IF ( EGAM .GT. 0.05 ) F = F + 8.*FC
      X0     = EMASS/EGAM
      DX     = 0.5 - X0
      DMIN   = 544.*X0/Z3
      DMIN2  = DMIN*DMIN
C
      IF ( DMIN .LE. 1. ) THEN
         F10 = 42.392 - 7.796*DMIN + 1.961*DMIN2  - F
         F20 = 41.405 - 5.828*DMIN + 0.8945*DMIN2 - F
      ELSE
         F10 = 42.24 - 8.368*LOG(DMIN+0.952) - F
         F20 = F10
      ENDIF
C
C Calculate limit for screening variable,DELTA, to ensure
C that screening rejection functions always remain positive.
C
      DMAX   = EXP((42.24-F)/8.368) - 0.952
C
C Differential cross-section factors which form the coefficients
C of the screening functions.
C
      DSIG1   = DX*DX * F10/3.
      DSIG2   = 0.5*F20
      BPAR    = DSIG1 / (DSIG1 + DSIG2)
C
C Decide which screening rejection function to use and sample the
C electron/photon fractional energy BR.
C
  20  CONTINUE
      IF ( RNDM(2) .LT. BPAR ) THEN
         X    = 0.5 - DX*EXP(LOG(RNDM(3))/3.)
         IREJ = 1
      ELSE
         X    = X0 + DX*RNDM(3)
         IREJ = 2
      ENDIF
C
C Calculate DELTA ensuring positivity.
C
      D    = 0.25 * DMIN / (X*(1.-X))
      IF ( D .GE. DMAX ) GOTO 20
      D2   = D * D
C
C Calculate F1 and F2 functions using approximations. F10 and F20 are th
C F1 and F2 functions calculated for the DELTA=DELTA minimum.
C
      IF ( D .LE. 1. ) THEN
         F1 = 42.392 - 7.796*D + 1.961*D2  - F
         F2 = 41.405 - 5.828*D + 0.8945*D2 - F
      ELSE
         F1 = 42.24  - 8.368*LOG(D+0.952)  - F
         F2 = F1
      ENDIF
C
      IF ( IREJ .NE. 2 ) THEN
         SCREJ = F1/F10
      ELSE
         SCREJ = F2/F20
      ENDIF
C
C Accept or reject on basis of random variate.
C
      IF ( RNDM(4) .GT. SCREJ ) GOTO 20
      EEL1 = X * EGAM
C
   60 CONTINUE
C
C Generate electron decay angles with respect to a Z-axis defined
C along the parent photon. PHI is generated isotropically and THETA
C is assigned an approximate average value given by electron mass/
C photon energy.
C
      THETA  = EMASS / EGAM
      SINTH  = SIN(THETA)
      COSTH  = COS(THETA)
      PHI    = TWOPI * RNDM(5)
      COSPHI = COS(PHI)
      SINPHI = SIN(PHI)
C
C Polar co-ordinates to momentum components.
C
      pel1   = SQRT(eel1**2-emass**2)
      p1(1)  = PEL1 * SINTH * COSPHI
      p1(2)  = PEL1 * SINTH * SINPHI
      p1(3)  = PEL1 * COSTH
      p1(4)  = EEL1
C
C Momentum vector of second electron. Recoil momentum of target
C nucleus/electron  ignored.
C
      eel2   = egam - eel1
      pel2   = SQRT(eel2**2-emass**2)
      p2(1)  =-PEL2 * SINTH * COSPHI
      p2(2)  =-PEL2 * SINTH * SINPHI
      p2(3)  = PEL2 * COSTH
      p2(4)  = EEL2
C
C Rotate tracks into ALEPH frame
C
      cosal  = qct(itk)
      sinal  = SQRT(1.-cosal**2)
      cosbt  = COS(qph(itk))
      sinbt  = SIN(qph(itk))
C
      q1(1)  = p1(1)*cosal*cosbt - p1(2)*sinal + p1(3)*sinal*cosbt
      q1(2)  = p1(1)*cosal*sinbt + p1(2)*cosal + p1(3)*sinal*sinbt
      q1(3)  =-p1(1)*sinal                     + p1(3)*cosal
      q1(4)  = p1(4)
C
      q2(1)  = p2(1)*cosal*cosbt - p2(2)*sinal + p2(3)*sinal*cosbt
      q2(2)  = p2(1)*cosal*sinbt + p2(2)*cosal + p2(3)*sinal*sinbt
      q2(3)  =-p2(1)*sinal                     + p2(3)*cosal
      q2(4)  = p2(4)
C
C Select charges randomly.
C
      IF ( RNDM(6) .GT. 0.5 ) THEN
        CALL ucopy(q1(1),p1(1),4)
        CALL ucopy(q2(1),p2(1),4)
      ELSE
        CALL ucopy(q2(1),p1(1),4)
        CALL ucopy(q1(1),p2(1),4)
      ENDIF
C
  999 RETURN
      END
