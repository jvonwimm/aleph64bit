C
C ! Quality word bit flags for VDET data banks
C
C
C  First, VDXY,VDZY quality flag bits
C
      INTEGER ISEPBT,IVETOC,IMCNEF
      PARAMETER (ISEPBT = 1)
      PARAMETER (IVETOC = 2)
      PARAMETER (IMCNEF = 536870912)
C
C  Then, VDCO quality flag bits
C

      INTEGER IVPHIT,IVZHIT
      INTEGER IVPAMB,IVZAMB
      PARAMETER (IVPHIT = 1)
      PARAMETER (IVZHIT = 2)
      PARAMETER (IVPAMB = 4)
      PARAMETER (IVZAMB = 8)
#if defined(DOC)
C
C  First, VDXY,VDZY quality flag bits
C
C      PARAMETER (ISEPBT = 1)  !  Separated hit
C      PARAMETER (IVETOC = 2)  !  Generic veto hit
C      PARAMETER (IMCNEF = bit 30 )  !  MC inefficiency rejected hit
C
C  Then, VDCO quality flag bits
C
C      PARAMETER (IVPHIT = 1)  ! R-Phi hit
C      PARAMETER (IVZHIT = 2)  ! Z hit
C      PARAMETER (IVPAMB = 4)  ! R-Phi ambiguous hit
C      PARAMETER (IVZAMB = 8)  ! Z ambiguous hit
#endif
