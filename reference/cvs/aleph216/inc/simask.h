C! Sical masks to decode address
      INTEGER  SIBIMD , SIBIPH , SIBIRD , SIBITP ,SIBIOR
      INTEGER  SIMD , SIPH , SIRD , SIST ,SIOR
      PARAMETER (SIBIMD = 2048, SIBIPH = 64 , SIBIRD =  4, SIBITP = 1)
      PARAMETER (SIBIOR = 4096 )
      PARAMETER (MSSIMD = 2048 ,MSSIPH = 1984 ,MSSIRD = 60,MSSITP = 3)
      PARAMETER (MSSIOR = 12288)
#if defined(DOC)
C     SIBIxx  correspond to the shift in address to obtain bin#
C         OR  :tplane bin   divide by 4096 = 2**12 to shift by 12 bits
C         MD  :module bin             2048 = 2**11 to shift by 11 bits
C         PH  :   phi bin               64 = 2**6               6 bits
C         RD  :radius bin                4 = 2**2               2 bits
C         TP  :triplet bin               1 = 2**0               0 bit
C     MSSIxx  correspond to the subcomponent mask in address
C         OR  :tplane mask           12288 = '3000' hexadecomal 2 bits
C         MD  :module mask            2048 = '800'              1 bit
C         PH  :   phi mask            1984 = '7C0'              5 bits
C         RD  :radius mask              60 = '03C'              4 bits
C         TP  :triplet mask              3 = '003'              2 bits
#endif
