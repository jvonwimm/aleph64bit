      SUBROUTINE kxlupr(ifl)
C************************************************************************
C      print out jetset parameters mstj, parj,mstu,paru
C      B.Bloch july 96
C      input  IFL : printout flag
C               1 = print only the 100 first values of MSTJ,PARJ
C               2 = print also the 100 first values of MSTU,PARU
C************************************************************************
#ifndef DOC
#include "lun7com.h"
C
      WRITE(MSTU(11),*)
      if ( Ifl.ge.1) then
      WRITE(MSTU(11),1000)
      DO 100 I=1,50
        WRITE(MSTU(11),1010) I,MSTJ(I),MSTJ(I+50),PARJ(I),PARJ(I+50)
 100  CONTINUE
      endif
      if ( Ifl.ge.2) then
      WRITE(MSTU(11),1001)
      DO 101 I=1,50
        WRITE(MSTU(11),1010) I,MSTU(I),MSTU(I+50),PARU(I),PARU(I+50)
 101  CONTINUE
      endif
      WRITE(MSTU(11),*)
 1000 FORMAT(10X,'Parameters and switches used by Jetset:',/,/,
     $     '         I   MSTJ(I) MSTJ(I+50)   PARJ(I) PARJ(I+50)',/)
 1001 FORMAT(10X,'Parameters and switches used by Jetset:',/,/,
     $     '         I   MSTU(I) MSTU(I+50)   PARU(I) PARU(I+50)',/)
 1010 FORMAT(2I10,I11,3X,2G11.3)
      RETURN
      END
#endif
