      integer function ndigh_e14(Ihco)
C======================================================================
C For PHCO row number IHCO how many distinct layers of digital hits
C are associated to it
C Auxiliary to QFE14
C Author G. Taylor Oct 1997
C======================================================================
      SAVE
#include "qcde.h"
#include "phstjj.h"
      parameter ( nhsmi = 0 , nhsmx = 190 )
      integer ifired(3,24,23)
      integer imodh(100),isch(100)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"
C----------------------------------------------------------------------
      ndigh_e14=0
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NAPHST=NAMIND('PHST')
      ENDIF
C
      IF (IW(NAPHST).LE.0) return
      NHSTO=LROWS (IW(NAPHST))
      IF (NHSTO.LE.0) return
C
C
      KPHST=IW(NAPHST)
      nmod=0
      DO 100 IST=1,NHSTO
C?           get module #, subcomp #
        ITETHS=ITABL(KPHST,IST,JPHSTI)
        ISTKHS=1
        IF (ITETHS.GT.128) THEN
          ITETHS=ITETHS-128
          ISTKHS=2
        ENDIF
        if (iteths.le.nhsmi.or.iteths.gt.nhsmx) go to 100
        IPHIHS=ITABL(KPHST,IST,JPHSPI)
        CALL HFNDMD(ITETHS,IPHIHS,ISTKHS,ISUBHS,IMODHS,IOVRHS)
        if (ITABL(KPHST,IST,JPHSPH).eq.ihco) then
         nmod=nmod+1
         imodh(nmod)=imodhs
         isch(nmod)=isubhs
        endif 
C
 100  continue  
C
      nhtub=0
      khtub=iw(namind('HTUB'))
      call vzero(ifired(1,1,1),23*3*24)
      if (khtub.gt.0) then
       nhtub=lrows(khtub)
       do i=1,lrows(khtub)
        isc=itabl(khtub,i,1)
        imod=itabl(khtub,i,2)
        ilay=itabl(khtub,i,3)
        ifired(isc,imod,ilay)=ifired(isc,imod,ilay)+1
       enddo
       do ilay=1,23
        iuse=0
        do isc=1,3
         do imod=1,24
          if (ifired(isc,imod,ilay).ne.0) then
           do i=1,nmod
            if (isc.eq.isch(i).and.imod.eq.imodh(i)) iuse=1
           enddo
          endif
         enddo
        enddo
        if (iuse.eq.1) ndigh_e14=ndigh_e14+1
       enddo
      endif
      return
      END
