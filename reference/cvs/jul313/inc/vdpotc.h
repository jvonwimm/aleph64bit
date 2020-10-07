      PARAMETER (LPVCOB = 7,
     &          UNIT1 = 0.0002,
     &          UNIT2 = 0.00001,
     &          UNIT3 = 0.0002,
     &          UNIT4 = 0.0002,
     &          UNIT5 = 0.0002,
     &          IZOFFS = 50000,
     &          LEN2BY = 65535,
     &          LEN3BY = 16777215)
C
#if defined(DOC)
C
C!  Parameters of MVD Pot
C
C   LPVCOB : lenght of row in PVCO bank
C   UNIT1  : units of r
C   UNIT2  : units of phi
C   UNIT3  : units of Z
C   UNIT4  : units of d(r-phi)
C   UNIT5  : units of d(z)
c   IZOFFS : offset of z coordinate (to avoid negatives)
C   LEN2BY : largest number with 2 bytes (unsigned)
C   LEN3BY : largest number with 3 bytes (unsigned)
#endif
