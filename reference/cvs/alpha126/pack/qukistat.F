      subroutine qukistat(nfili,tfilis,lirun,nktrg,ifail)
C-----------------------------------------------------------------------
C  user access to  KINGAL statistics on generated triggers
C  at the end of job
C
C To be called in QUTERM - Meaningful for MCarlos only !
C Output arguments:
C
C  nfili     = number of FILI CARDS read during the job
C  tfilis(i) = list of tapes_files read during the job, e.g. R01234_125 
C              i = 1 , nfili   
C              must be declared character*12 tfilis(100) in the 
C              calling subroutine
C  lirun(i)  = list of runs read during the job (Integer lirun(100))
C  nktrg(i)  = number of generated KINGAL trigger for dataset read on
C              tfilis(i)   - To be declared as Integer nktrg(100)
C  ifail     = 1 if more than 100 FILI cards read on input 
C-----------------------------------------------------------------------
      save
#include "krsktrg.h"
      character*(*) tfilis(*)
      integer lirun(*),nktrg(*)
C-----------------------------------------------------------------------
      ifail=ifailmx
      nfili=nrukin
C access to the content of krsktrg:
      do i=1,nrukin
         tfilis(i)=filrlis(i)
         lirun(i)=lrukin(i)
         nktrg(i)=litrmx(i)-litrmi(i)+1
      enddo
C
 999  return
      END

