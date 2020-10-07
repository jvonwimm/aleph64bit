      COMMON/TSVDWB/KTSVD
      INTEGER JTSVWZ,JTSVEX,JTSVNU,JTSVHZ,JTSVNZ,LTSVDA
      PARAMETER(JTSVWZ=1,JTSVEX=2,JTSVNU=3,JTSVHZ=4,JTSVNZ=504,
     +          LTSVDA=1003)
#if defined(DOC)
C! TSVD work bank containing histogram of VDET z residuals.
C  (used by subroutines TSUMVD and TVDVEL).
C
C KTSVD  : Index of work bank TSVD
C JTSVWZ : HAC parameter for word WZ - Size of window in Z used
C JTSVEX : HAC parameter for word EX - Expected no. of VDET coords
C JTSVNU : HAC parameter for word NU - Observed no. of VDET coords
C JTSVHZ : HAC parameter for word HZ - Weighted histo. of VDET z residuals
C JTSVNZ : HAC parameter for word NZ - Unweighted histo. of VDET z residuals
C LTSVDA : Length of row of TSVD work bank
#endif
