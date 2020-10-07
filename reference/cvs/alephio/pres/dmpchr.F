      SUBROUTINE DMPCHR(ICH,LMACHI,N)
C----------------------------------------------------------------------
C!    Convert a CHARACTER*4 array between VAX, IBM, or CRAY formats.
CKEY PRESS DMPCHR DECOMPRESS /INTERNAL
C!
C!    AUTHOR:   D. Harvatis          MAY 89
C     modified by F.Ranjard          JAN 90
C                             - to set I2 to 0
C!
C!    INPUT :
C!           - ICH  : integer array equivalent to character*4 array to
C!                    be converted
C!           - LMACHI : control flag
C!           - N    : number of variables to be converted
C!
C!    Description :
C!    On the VAX, if LMACHI = 0  convert from IBM to VAX format
C!                if LMACHI = -1 convert from CRAY to VAX format.
c!                if LMACHI = 1 convert from APOLLO to VAX format.
C!    On IBM,     if LMACHI = 0  convert VAX to IBM format
C!                if LMACHI = -1 convert from CRAY to IBM format.
C!                if LMACHI = 1 convert from APOLLO to IBM format.
C!    On CRAY,    if LMACHI = 0  convert from IBM to CRAY format
C!                if LMACHI = -1 convert from VAX to CRAY format.
C!                if LMACHI = 1 convert from APOLLO to CRAY format.
C!    On APOLLO   if LMACHI = 0  convert from IBM to APOLLO format
C!                if LMACHI = -1 convert from VAX to APOLLO format.
C!                if LMACHI = 1 convert from CRAY to APOLLO format.
C!    On DECS,    if LMACHI = 0  convert from IBM to DECS format
C!                if LMACHI = -1 convert from CRAY to DECS format.
C!                if LMACHI = 1 convert from APOLLO to DECS format.
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
      INTEGER     ICH(*),N,LOW
      INTEGER LMACHI
      CHARACTER*4 STR,STR2,STR3,CHAINT
C this is a fix for ALPHA machines
      COMMON /LOCALP/ STR,STR2,STR3
      EQUIVALENCE (I1,STR)
      EQUIVALENCE (I2,STR2)
      EQUIVALENCE (I3,STR3)
      CHARACTER*256 TABLE
C     LOW is the number of the character stored in the LSB of the 32-bit
C     word. On the VAX, it is the 1st character. On the IBM and the CRAY
C     it is the 4th.
C     On computers that use the ASCII character set, TABLE contains the
C     EBCDIC character set.
C     On computers that use the EBCDIC character set, TABLE contains the
C     ASCII character set.
C     The position of each character in the
C     TABLE string is its collating sequense number in the above
C     character set plus one. Control and non printable characters are
C     replaced by spaces. These characters are not converted correctly
C     by this routine.
#if defined(ALEPH_DEC) || defined(ALEPH_LINUX)
      DATA LOW/1/
#else
      DATA LOW/4/
#endif
#if defined(IBM)
      DATA TABLE(1:32)/' '/
      DATA TABLE(129:256)/' '/
      DATA TABLE(33:48)/' !"#$%& ()*+,-./'/
      DATA TABLE(49:64)/'0123456789:;<=>?'/
      DATA TABLE(65:80)/'@ABCDEFGHIJKLMNO'/
      DATA TABLE(81:96)/'PQRSTUVWXYZ[\]|_'/
      DATA TABLE(97:112)/'`abcdefghijklmno'/
      DATA TABLE(113:128)/'pqrstuvwxyz{|}^ '/
#else
      DATA TABLE(1:74)/' '/
      DATA TABLE(75:81)/'+.<(+|&'/
      DATA TABLE(82:90)/' '/
      DATA TABLE(91:98)/'!$*); -/'/
      DATA TABLE(99:106)/' '/
      DATA TABLE(107:112)/'|,%_>?'/
      DATA TABLE(113:121)/' '/
      DATA TABLE(122:138)/'`:#@ =" abcdefghi'/
      DATA TABLE(139:154)/'       jklmnopqr'/
      DATA TABLE(155:170)/'        stuvwxyz'/
      DATA TABLE(171:192)/' '/
      DATA TABLE(193:208)/'{ABCDEFGHI'/
      DATA TABLE(209:224)/'}JKLMNOPQR'/
      DATA TABLE(225:240)/'\ STUVWXYZ'/
      DATA TABLE(241:256)/'0123456789|'/
#endif
C----------------------------------------------------------------------
      I2 = 0
      DO 5 J=1,N
          I1=ICH(J)
#if defined(ALEPH_DEC) || defined(ALEPH_LINUX)
          IF (LMACHI.EQ.0) THEN
              DO 10 I=1,4
                  STR2(LOW:LOW)=STR(I:I)
   10             STR3(5-I:5-I)=TABLE(I2+1:I2+1)
          ELSE
              DO 20 I=1,4
   20             STR3(5-I:5-I)=STR(I:I)
          END IF
#else
#if defined(IBM)
          IF (LMACHI.EQ.0) THEN
              DO 11 I=1,4
                  STR2(LOW:LOW)=STR(I:I)
   11             STR3(5-I:5-I)=TABLE(I2+1:I2+1)
          ELSE
              DO 21 I=1,4
                  STR2(LOW:LOW)=STR(I:I)
   21             STR3(I:I)=TABLE(I2+1:I2+1)
          END IF
#else
#if defined(APOLLO) || defined(ALEPH_HP) || defined(RS6K) || defined(ALEPH_SGI)
          IF (LMACHI.EQ.0) THEN
              DO 13 I=1,4
                  STR2(LOW:LOW)=STR(I:I)
   13             STR3(I:I)=TABLE(I2+1:I2+1)
          ELSEIF (LMACHI.EQ.-1) THEN
              DO 23 I=1,4
   23             STR3(5-I:5-I)=STR(I:I)
          ELSE
             DO 33 I=1,4
   33             STR3(I:I)=STR(I:I)
          END IF
#endif
#endif
#endif
    5     ICH(J)=I3
      RETURN
      END
#endif
