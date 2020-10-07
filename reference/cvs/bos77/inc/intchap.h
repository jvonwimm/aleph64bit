#if defined(IBM) || defined(GOULD)
      INTCHA=NCHAR
#else
#if defined(ALEPH_DEC) || defined(ALEPH_LINUX)
      CHARACTER*4 NCHAR
      CALL UCTOH(NCHAR,INTG,4,4)
      INTCHA=INTG
#else
#if defined(CRAY)
      CHARACTER*4 NCHAR
      INTCHA=OR(SHIFTL(ICHAR(NCHAR(1:1)),24),
     +       OR(SHIFTL(ICHAR(NCHAR(2:2)),16),
     +       OR(SHIFTL(ICHAR(NCHAR(3:3)), 8),ICHAR(NCHAR(4:4)))))
#else
*     MACHINE INDEPENDENT VERSION
      CHARACTER*4 NCHAR
      INTCHA=IOR(ISHFT(ICHAR(NCHAR(1:1)),24),
     +       IOR(ISHFT(ICHAR(NCHAR(2:2)),16),
     +       IOR(ISHFT(ICHAR(NCHAR(3:3)), 8),ICHAR(NCHAR(4:4)))))
#endif
#endif
#endif
