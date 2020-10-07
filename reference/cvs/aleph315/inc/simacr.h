C! Sical statement functions to access subcomponents
      SIMD(IXX) = ( IAND(IXX,MSSIMD))/SIBIMD +1
      SIST(IXX,IYY) =(( IAND(IXX,MSSITP))/SIBITP )*3 + IYY +1
      SIPH(IXX) = ( IAND(IXX,MSSIPH))/SIBIPH +1
      SIRD(IXX) = ( IAND(IXX,MSSIRD))/SIBIRD +1
      SIOR(IXX) = ( IAND(IXX,MSSIOR))/SIBIOR -1
#if defined(DOC)
C     SIxx  Statement function gives the bin number from address  IXX
C       OR  :tplane bin    [0,2]
C       MD  :module bin    [1,2]
C       PH  :   phi bin    [1,32]
C       RD  :radius bin    [1,16]
C       ST  :zstack bin    [1,12]   from address IXX and tplane bin IYY
#endif
