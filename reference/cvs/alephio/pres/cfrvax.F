      SUBROUTINE CFRVAX(IARRAY,NWORDS)
C
CKEY PRESS DMPBAN DECOMPRESS /USER
C!
C! Author:  T. Wildish. October 1989
C! Purpose: Convert vax real format to UNIX real format.
C!
C! Uses a cray version of SVXSX from
C! IBM, followed by CFRIBM to convert to UNIX. Messy, but it works.
C! This should only be considered as a stopgap solution until the
C! problem can be fixed at source!
C!
C             This is a subroutine to convert from VAX floating
C             point format (32 Bits) to IBM floating point format
C             (32 bits).
C
C             IARRAY   an area of storage 32*NWORDS bits long in which
C                      are stored NWORDS 32 bits VAX floating point
C                      numbers.
C                      On output it will contain NWORDS 32 bits IBM
C                      floating point numbers
C
C             NWORDS   number of floating point numbers to convert
C
#ifndef DOC
#if defined(APOLLO)
      DIMENSION IARRAY(*)
      LOGICAL BTEST
      DATA KMASK /16#FFFFFFFF/
#else
#if defined(ALEPH_HP) || defined(RS6K) || defined(ALEPH_SGI)
      DIMENSION IARRAY(*)
      LOGICAL BTEST
      DATA KMASK /Z'FFFFFFFF'/
#endif
#endif
#if defined(APOLLO) || defined(ALEPH_HP) || defined(RS6K) || defined(ALEPH_SGI)
      IF(NWORDS.LE.0) GO TO 999
      DO 1 J=1,NWORDS
C
C             Check for exact 0
C
         IF(IARRAY(J).EQ.0) GO TO 1
C
C             Get exponent
C
         IEXPO = ISHFT(IARRAY(J),17)
         IEXPO = IAND(IEXPO,KMASK)
         IEXPO = ISHFT(IEXPO,-24)
         IEXPO = IAND(IEXPO,KMASK)
C                VAX EXP + VAX BIAS + IBM BIAS
C        IEXPO = IEXPO   -    128   +   260
         IEXPO = IEXPO + 132
         IEX16 = IEXPO/4
         ILEFT = 4 - MOD(IEXPO,4)
         IF(ILEFT.EQ.4) THEN
            ILEFT = 0
            IEX16 = IEX16 - 1
         END IF
         IDUMMY = ISHFT(IARRAY(J),25)
         IDUMMY = IAND(IDUMMY,KMASK)
         IDUMMY = ISHFT(IDUMMY,-9)
         IDUMMY = IAND(IDUMMY,KMASK)
         IDUMMY = IBSET(IDUMMY,23)
         IDUMY1 = IAND(IARRAY(J),KMASK)
         IDUMY1 = ISHFT(IDUMY1,-16)
         IDUMY1 = IAND(IDUMY1,KMASK)
         IDUMMY = IOR(IDUMMY,IDUMY1)
         IDUMMY = IAND(IDUMMY,KMASK)
         IDUMMY = IDUMMY + 2**(ILEFT-1)
         IDUMMY = ISHFT(IDUMMY,-ILEFT)
         IDUMMY = IAND(IDUMMY,KMASK)
         IDUMY1 = ISHFT(IEX16,24)
         IDUMY1 = IAND(IDUMY1,KMASK)
         IDUMMY = IOR(IDUMMY,IDUMY1)
         IDUMMY = IAND(IDUMMY,KMASK)
         IF(BTEST(IARRAY(J),15)) IDUMMY = IBSET(IDUMMY,31)
         IARRAY(J) = IDUMMY
   1  CONTINUE
#endif
      CALL CFRIBM(IARRAY,NWORDS,3)
 999  END
#endif
