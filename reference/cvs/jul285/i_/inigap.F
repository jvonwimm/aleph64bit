      SUBROUTINE INIGAP
C-----------------------------------------------------------------------
C! Set up ITC gap common
C!
C!    Author      :- J. Sedgbeer 88/03/29
C!    Modified    :- I. Tomalin  89/02/28
C!
C!   Input:
C!
C!   Output: common /ITGAPC/
C!
C!   called by : ITRKCI
C!   calls     : None
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
C I/O Commons etc.
#include "itgapc.h"
C-----------------------------------------------------------------------
      INTEGER IGAP(7,8),NMISS(25),IPR1(54),IPR2(54),IPR3(54)
C
C    Inner layer 1   2   3   4   5   6   7
      DATA IGAP/ 0,  0,  0,  0,  0,  0,  0,
     2           1,  0,  0,  0,  0,  0,  0,
     3           8,  2,  0,  0,  0,  0,  0,
     4          14,  9,  3,  0,  0,  0,  0,
     5          19, 15, 10,  4,  0,  0,  0,
     6          23, 20, 16, 11,  5,  0,  0,
     7           0, 24, 21, 17, 12,  6,  0,
     8           0,  0, 25, 22, 18, 13,  7/
C
      DATA NMISS/0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
     +           3, 3, 3, 3, 4, 4, 4/
C
      DATA IPR1/ 1, 2, 3, 4, 5, 6, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
     1           1, 2, 3, 4, 1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2, 3, 3,
     1           1, 2, 1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2,
     1           1, 1/
      DATA IPR2/ 2, 3, 4, 5, 6, 7, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7,
     2           3, 4, 5, 6, 2, 4, 3, 5, 4, 6, 5, 7, 3, 4, 4, 5, 5, 6,
     2           4, 5, 2, 5, 3, 6, 4, 7, 3, 5, 4, 6, 4, 5, 2, 6, 3, 7,
     2           3, 6/
      DATA IPR3/ 3, 4, 5, 6, 7, 8, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
     3           5, 6, 7, 8, 5, 5, 6, 6, 7, 7, 8, 8, 6, 6, 7, 7, 8, 8,
     3           7, 8, 6, 6, 7, 7, 8, 8, 7, 7, 8, 8, 8, 8, 7, 7, 8, 8,
     3           8, 8/
C-----------------------------------------------------------------------
C Give each type of link a code number.
      DO 20 I=1,8
        DO 10 J=1,7
          IGAPIT(J,I) = IGAP(J,I)
   10   CONTINUE
   20 CONTINUE
C
C Note the number of layers which each type of link jumped over.
      DO 25 J=1,25
        NUMISS(J) = NMISS(J)
   25 CONTINUE
C
C      DO 30 I=1,54
C        IPR1IT(I) = IPR1(I)
C        IPR2IT(I) = IPR2(I)
C        IPR3IT(I) = IPR3(I)
C   30 CONTINUE
C
C Note the inner and outer layers touched by each link type.
      DO 50 IL1=1,7
        DO 40 IL2=IL1+1,8
          IGP12 = IGAPIT(IL1,IL2)
          IF(IGP12.EQ.0) GOTO 40
          IIGPIT(IGP12) = IL1
          IOGPIT(IGP12) = IL2
   40   CONTINUE
   50 CONTINUE
C
      END
#endif
