      SUBROUTINE SXSDR(IARRAY, NW)
C------------------------------------------------------------------
C! convert from DEC VAX floating format to DEC RISC format
CKEY IO DECS CONVERT
C - Author - LatB
C - Input  : - IARRAY / I = array of DEC VAX floating format numbers
C            - NW     / I = length of IARRAY
C - Output : - IARRAY / I = array of DEC RISC floating format numbers
C -------------------------------------------------------------------
#ifndef DOC
#if defined(ALEPH_DEC) && defined(UNIX) || defined(ALEPH_LINUX)
      INTEGER NW
      INTEGER IARRAY(*)

      INTEGER LW,LW0
      INTEGER*1 B(4),B0(4)
      EQUIVALENCE (LW,B),(LW0,B0)

      INTEGER I
C --------------------------------------------------------
      IF (NW .LE. 0) RETURN
      DO I = 1, NW
         LW = IARRAY(I)
         IF (ISHFT(B(2),1) .NE. 0) THEN
            B0(1) = B(3)
            B0(2) = B(4)
            B0(3) = B(1)
            B0(4) = B(2)-1
            IARRAY(I) = LW0
         ELSEIF (LW .NE. 0) THEN
            IARRAY(I) = 0
         END IF
      END DO
#endif
      END
#endif
