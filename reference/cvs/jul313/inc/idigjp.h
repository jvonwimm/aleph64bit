      INTEGER NBITWN,NBITRP,NBITZT,NBITQR,NBITQZ,NBITAM,NBITVS
      INTEGER IBITWN,IBITRP,IBITZT,IBITQR,IBITQZ,IBITAM,IBITVS
      PARAMETER(NBITWN=10,NBITRP=9 ,NBITZT=9 ,NBITQR=1 ,NBITQZ=1,
     +          NBITAM=1 ,NBITVS=1)
      PARAMETER(IBITWN=0 ,IBITRP=10,IBITZT=19,IBITQR=28,IBITQZ=29,
     +          IBITAM=30,IBITVS=31)
#if defined(DOC)
C!   ITC (packed) digits: pointers to bits within packed word.
C!   Use for PIDI and IDIG banks.
C!
C!   NBITWN  = # of bits for wire number
C!   NBITRP  = # of bits for R-Phi TDC
C!   NBITZT  = # of bits for Z TDC
C!   NBITQR  = # of bits for R-Phi TDC Quality Flag
C!   NBITQZ  = # of bits for Z TDC Quality Flag
C!   NBITAM  = # of bits for Coord. ambiguity flag
C!   NBITVS  = # of bits for version flag
C!
C!   IBITWN  = start position (bit) of wire number
C!   IBITRP  = start position (bit) of R-Phi TDC
C!   IBITZT  = start position (bit) of Z TDC
C!   IBITQR  = start position (bit) of R-Phi TDC Quality Flag
C!   IBITQZ  = start position (bit) of Z TDC Quality Flag
C!   IBITAM  = start position (bit) of Coord. ambiguity flag
C!   IBITVS  = start position (bit) of version flag
#endif
