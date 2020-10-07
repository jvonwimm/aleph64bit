      SUBROUTINE ALBUNCH(NBUN)
C----------------------------------------------------------------------
C!  - Get number of bunches from LEP information in 1997
C!
C!   Author   :- B. Bloch                     25-Oct-1997
C!
C?
C!   get currents in each bunch from LXUN bank
C!   return number of bunches with non zero current
C!  Input  : LXUN bank
C!  Output : NBUN Number of wagons per train with non zero current
C?
C!======================================================================
#ifndef DOC
#include "lxunjj.h"
#include "bcs.h"
      EXTERNAL NAMIND
      dimension cur(4,2)
#include "bmacro.h"
      id = iw(namind('LXUN'))
      nbun = 0
      call vzero(cur,8)
      if ( id.le.0) go to 99
      do 10 i= 1,lrows(id)
         itrain = itabl(id,i,jlxutr)
         iwag   = itabl(id,i,jlxuwa)
         if (itrain.le.4) then
           cur(iwag,1) = cur(iwag,1) + rtabl(id,i,jlxuce)
           cur(iwag,2) = cur(iwag,2) + rtabl(id,i,jlxucp)             
         endif
  10  continue
      do 11 i = 1,4
        if ( (cur(i,1).gt.0.).and.(cur(i,2).gt.0.) ) nbun = nbun+1
  11  continue
C      write(iw(6),999) nbun,((cur(i,1),cur(i,2)),i=1,4)
  99  return
  999 format (1x,'albunch',I4,' bunches',
     $        4(/,1x,'currents e- & e+ in each bunch',2F8.4)) 
      end
#endif
