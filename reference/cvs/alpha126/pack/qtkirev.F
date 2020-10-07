      subroutine qtkirev
C-----------------------------------------------------------------------
C printout end-of-job stats
C called from QMTERM
C-----------------------------------------------------------------------
#include "bcs.h"
#include "krsktrg.h"
C-----------------------------------------------------------------------
      if (nrukin.le.0) go to 999
      write (iw(6),1234) nrukin
 1234 format (/' == QTKIREV : Summary of KINGAL events generated -'/,
     +        15X,'Total number of FILI cards read =',i6/)
C summary printout:
      do k=1,nrukin
         ntrg=litrmx(k)-litrmi(k)+1
         if (ntrg.le.0) ntrg=0
         write (iw(6),1235) filrlis(k),lrukin(k),ntrg
1235     format (5x,'** Tape read:',2x,a12,' Run:',I5,
     +           2x,'Number of Kingal triggers =',i8)
      enddo 
C
 999  return
      END
