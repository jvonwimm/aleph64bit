      SUBROUTINE SIBSDR (IARRAY,NW)
C-------------------------------------------------------------------------
CKEY IO DECS CONVERT
C! convert from IBM to DEC RISC floating point format
C - Author - LatB
C -- i.e. byte-swapped IEEE floating point
C -- stolen from CFRIBM of EPIO lib
C - Input  : - IARRAY /I = array of IBM floating point
C            - NW     /I = length of IARRAY
C - Output : - IARRAY /I = array of DEC RISC floating point
C ------------------------------------------------------------------------
#ifndef DOC
#if defined(ALEPH_DEC) && defined(UNIX) || defined(ALEPH_LINUX)
      IMPLICIT NONE
      INTEGER NW
      INTEGER IARRAY(1)

      INTEGER IBIG, ISMA, ILEFT, IEXPO
      PARAMETER (IBIG=2147483647,ISMA=8388608)

      INTEGER J, IDUMMY, LW
C --------------------------------------------------------------
      IF(NW.LE.0)   GO TO 999
C
C                Convert the first NW words of IARRAY from
C                IBM 32-bit floating point representation to
C                IEEE single-precision (32-bit) floating point
C                representation
        DO 2 J=1,NW
C
           LW = IARRAY(J)
C             Check for exact 0
           IF (LW .EQ. 0) GOTO 2
C
C              Make sure ILEFT gets defined to something!
           ILEFT = 0
C
C                Get exponent
           IEXPO  = ISHFT(ISHFT(LW,1),-25)
           IF(BTEST(LW,23)) THEN
              ILEFT = 0
           ELSEIF(BTEST(LW,22)) THEN
              ILEFT = 1
           ELSEIF(BTEST(LW,21)) THEN
              ILEFT = 2
           ELSEIF(BTEST(LW,20)) THEN
              ILEFT = 3
           END IF
           IEXPO = IEXPO * 4 - ILEFT - 130
           IF(IEXPO.LE.0) THEN
              IDUMMY = ISMA
              GO TO 1
           ELSEIF(IEXPO.GT.255) THEN
              IDUMMY = IBIG
              GO TO 1
           END IF
           IDUMMY = ISHFT(LW,ILEFT)
           IDUMMY = IOR(IAND(IDUMMY,2**23-1),ISHFT(IEXPO,23))
   1       IF(BTEST(LW,31)) IDUMMY = IBSET(IDUMMY,31)
           IARRAY(J) = IDUMMY
   2    CONTINUE
C
 999  CONTINUE
#endif
      END
#endif
