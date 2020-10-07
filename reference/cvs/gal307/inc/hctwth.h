C
      INTEGER NUHFTH,IFHFTH,ILHFTH
      PARAMETER(NUHFTH=4,IFHFTH=14,ILHFTH=17)
C
      REAL ENHFTW(NUHFTH,96,2,2)
C
      COMMON/HCTWTH/ENHFTW
C
#if DOC
C! HCAL half-tower energies
    NUHFTH = number of half-tower rows per side
    IFHFTH = first row of half-towers
    ILHFTH = last row of half-towers
    ENHFTW(N,IPH,ISIDE,IHALF) = energy in each half of the half-towers
#endif
