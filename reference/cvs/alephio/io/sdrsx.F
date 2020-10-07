      SUBROUTINE SDRSX(IARRAY, NW)
C------------------------------------------------------------------
C! Convert from DEC RISC floating format to DEC VAX format
CKEY IO DECS CONVERT
C - Author - LatB
C - Input  : - IARRAY /I = array of DEC RISC floating point
C            - NW     /I = length of IARRAY
C - Output : - IARRAY /I = array of DEC VAX floating point
C ------------------------------------------------------------------
#ifndef DOC
#if defined(ALEPH_DEC) && defined(UNIX) || defined(ALEPH_LINUX)
      INTEGER NW
      INTEGER IARRAY(*)

      INTEGER LW,LW0
      INTEGER*1 B(4),B0(4)
      EQUIVALENCE (LW,B),(LW0,B0)

      INTEGER I
C ------------------------------------------------------------
      IF (NW .LE. 0) RETURN
      DO I = 1, NW
         LW = IARRAY(I)
         IF (LW .NE. 0) THEN
            B0(1) = B(3)
            B0(2) = B(4)+1
            B0(3) = B(1)
            B0(4) = B(2)
            IARRAY(I) = LW0
         END IF
      END DO
#endif
      RETURN
      END
#endif
