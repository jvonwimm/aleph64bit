*DK itmsk2
      SUBROUTINE ITMSK2(LBUS, NMEM, ICALS)
C.
C...ITMSK2  1.00  860520  20:33                        R.Beuselinck
C.
C!  Map the 18 ITC trigger bus-lines with the 8 bit memory address
C.  to produce the final 72 trigger bits corresponding to the
C.  calorimeter segments.
C.  To understand this routine you need to look at the schematic
C.  diagram of the ITC trigger.
C.
C.  Calling arguments:
C.  LBUS  - word containing 18 significant low order bits,   (INPUT)
C.          produced by the first stage of the trigger.
C.  NMEM  - word containing 8 bits to gate the bus-lines     (INPUT)
C.          onto the 72 latches.
C.  ICALS - 3 words containing the 72 latch bits.     (INPUT+OUTPUT)
C.
C-----------------------------------------------------------------------
#ifndef DOC
      INTEGER MEM(8,3), IBUS(18,3), ICALS(3), MASKM(3), MASKB(3)
      INTEGER LBUS, NMEM, I, IOR, IAND
      LOGICAL BTEST
C
#if defined(IBM)
      DATA MEM/0,0,0,0,ZFF000000,ZFFF000,ZFC0,Z3F,
     2         0,ZF0000000,ZFFF0000,ZFFF0,ZF,0,0,0,
     3         ZFC,Z3,0,0,0,0,0,0/
C
      DATA IBUS/Z1001000,Z41,Z2002000,Z4004000,Z82,Z8008000,Z10010000,
     +Z104,Z20020000,Z40040000,Z208,Z80080000,Z100000,Z410,Z200000,
     +Z400000,Z820,Z800000,Z10010,Z10000000,Z20020,Z40040,Z20000000,
     +Z80080,Z100100,Z40000000,Z200200,Z400400,Z80000000,Z800800,
     +Z1001001,0,Z2002002,Z4004004,0,Z8008008,0,Z4,0,0,Z8,0,0,
     +Z10,0,0,Z20,0,0,Z41,0,0,Z82,0/
#else
      DATA MEM/0,0,0,0,-16777216,16773120,4032,63,
     2         0,-268435456,268369920,65520,15,0,0,0,
     3         252,3,0,0,0,0,0,0/
C
      DATA IBUS/16781312,65,33562624,67125248,130,134250496,
     1      268500992,260,537001984,1074003968,520,-2146959360,
     1      1048576,1040,2097152,4194304,2080,8388608,
     2      65552,268435456,131104,262208,536870912,524416,
     2      1048832,1073741824,2097664,4195328,-2147483648,
     2      8390656,16781313,0,33562626,67125252,0,134250504,
     3      0,4,0,0,8,0,0,16,0,0,32,0,0,65,0,0,130,0/
#endif
C --------------------------------------------------------------------
C--  Clear the memory and bus-line masks on each entry.
C--
      MASKM(1) = 0
      MASKM(2) = 0
      MASKM(3) = 0
      MASKB(1) = 0
      MASKB(2) = 0
      MASKB(3) = 0
C
C--  Setup the memory masks.
C--
      DO 10 I=1,8
        IF (BTEST(NMEM,I-1)) THEN
          IF (MEM(I,1).NE.0) MASKM(1) = IOR(MASKM(1),MEM(I,1))
          IF (MEM(I,2).NE.0) MASKM(2) = IOR(MASKM(2),MEM(I,2))
          IF (MEM(I,3).NE.0) MASKM(3) = IOR(MASKM(3),MEM(I,3))
        ENDIF
   10 CONTINUE
C
C--  Setup the bus-line masks.
C--
      DO 20 I=1,18
        IF (BTEST(LBUS,I-1)) THEN
          IF (IBUS(I,1).NE.0) MASKB(1) = IOR(MASKB(1),IBUS(I,1))
          IF (IBUS(I,2).NE.0) MASKB(2) = IOR(MASKB(2),IBUS(I,2))
          IF (IBUS(I,3).NE.0) MASKB(3) = IOR(MASKB(3),IBUS(I,3))
        ENDIF
   20 CONTINUE
C
C--  Set bits in ICALS according to the overlap of the two sets
C--  of masks just defined.
C--
      ICALS(1) = IOR(ICALS(1),IAND(MASKM(1),MASKB(1)))
      ICALS(2) = IOR(ICALS(2),IAND(MASKM(2),MASKB(2)))
      ICALS(3) = IOR(ICALS(3),IAND(MASKM(3),MASKB(3)))
      END
#endif
