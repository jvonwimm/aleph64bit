      DOUBLE PRECISION FUNCTION UFBERR(X)
C
C!  Function to roughly estimate the Landau fluctuations in energy loss
C!  for Kalman filter.  Uses expression in particle data book for dE/dx
C!  resolution in Argon gas sampling chamber, and generalizes to any material
C!  by assuming the spread is a function of the number of radiation lengths
C!  traversed.  Add 15% uncertainty on Bethe-Bloch curve in quadrature.

C!  INPUT:      X       Number of radiation lengths traversed
C!  OUTPUT:     UFBERR  Sigma(%)^2 of energy loss
#ifndef DOC

      DOUBLE PRECISION X

      DOUBLE PRECISION  RHO_ARGON, X0_ARGON, X0_1CM, FWHM

      DATA RHO_ARGON /1.782D-3/       ! Density of Ar in gm/cm^3
      DATA X0_ARGON /19.55D0/         ! Radiation length of Ar in g/cm^2

C  How many radiation lengths is 1 cm of Ar?

      X0_1CM = 1.D0 * RHO_ARGON / X0_ARGON

C  Get FWHM from PDG formula

      FWHM = 0.96D0 * (X / X0_1CM)**(-0.32D0)

C  Assume Gaussian and convert FWHM to sigma

      UFBERR = (FWHM/2.D0)/1.17D0

C  Square and add 15% systematic in quadrature

      UFBERR = UFBERR**2 + 0.15D0**2

      RETURN
      END
#endif
