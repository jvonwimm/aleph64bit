      subroutine qfkirev
C-----------------------------------------------------------------------
C  fills common  /kinruev / for kinrun statistics
C  called from QFILL
C-----------------------------------------------------------------------
      save
#include "qcde.h"
#include "evehjj.h"
#include "krsktrg.h"
#include "alrcom.h"
      character filic*12,filio*12,cartf*80,tape*6,filex*5
      data filio / '????????????' /
      data ipass,irunol / 0 , 0 /
C-----------------------------------------------------------------------
C First call : initialize common;
      if (ipass.eq.0) then
         ipass=1
         nrukin=0
         ifailmx=0
         filic=' '
         call vzero(lrukin,nrumx)
         do i=1,nrumx
            litrmi(i)=999999999
            litrmx(i)=-1
            filrlis(i)=' '
         enddo
      endif
C Never execute for real data :
      if (krun.gt.2000) go to 999
C standard processing:
C new cartridge read ?:
      jfili=nlink('FILI',NRFILI) 
      if (jfili.le.0) go to 999
      ndafi=iw(jfili)
      if (ndafi.le.0) go to 999
      call alstin(iw(jfili+1),ndafi,cartf)
      call qanfili(cartf,tape,filex)
      ltap=lnblnk(tape)
      lfil=lnblnk(filex)
      if (ltap.eq.0.or.lfil.eq.0) go to 999
      filic=tape(1:ltap)//'_'//filex(1:lfil)
      if (filic.ne.filio) then
         nrukin=nrukin+1
         if (nrukin.gt.nrumx) then
            ifailmx=1 
            nrukin=nrumx  
            go to 999
         endif
         irunol=krun
         lrukin(nrukin)=krun
         filrlis(nrukin)=filic
         filio=filic  
      endif
C event #:
      jeveh=iw(namind('EVEH'))
      if (jeveh.le.0) go to 999 
      itrig=iw(jeveh+jeveev)
      litrmi(nrukin)=min0(litrmi(nrukin),itrig)
      litrmx(nrukin)=max0(litrmx(nrukin),itrig)
C
 999  return
      END
