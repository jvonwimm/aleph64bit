      SUBROUTINE QWNEWTR
CKEY WRITE EVENT / USER
C-----------------------------------------------------------------------
C! Print ALPHA new tracking banks in readable format
C      J. Boucrot  14-Apr-1997    revised 15-Oct 1997
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      character*19 blaver(6)
      parameter (ndaum = 15)
      integer lidau(ndaum)
      INTEGER LISKINK(50)
      data blaver / 'Standard V0        ' , 'Main Vertex        ',
     +              'Vertex from KVFITXX' , 'LV0, new tracking  ',
     +              'Nuclear Interaction' , 'Kink Vertex        ' /
#include "qmacro.h"
C-----------------------------------------------------------------------
      write (iw(6),2001) krun,kevt
 2001 format (//' ===> QWNEWTR entry : run,evt =',2i8,
     +        '  List of vertices :'/)
C LOOP on vertices. Print explicit messages for the new ones:
      DO 500 IVV=KFREV,KLREV
         ityp=kvtype(ivv)
         write (iw(6),2200) ivv,ityp,blaver(ityp+1)
 2200    format(' *** Vertex number,type =',
     +          2i5,2x,a19)
         write (iw(6),2201) QVX(IVV),QVY(IVV),QVZ(IVV),QVCHIF(IVV)
 2201    format(3X,'** X,Y,Z VERTEX =',3F12.4,' -- Chi2 =',f10.3)
         itk=kvincp(ivv)
         ndau=kvndau(ivv)
         write (iw(6),2202) itk,ndau
 2202    format(8X,'== Vertex from incoming track =',i6,2x,
     +          'Nb of daughters =',i6)
         if (ndau.gt.ndaum) go to 500
         call vzero(lidau,ndaum)
         do 300 i=1,ndau
 300     lidau(i)=kvdau(ivv,i)
         write (iw(6),2203) (lidau(kk),kk=1,ndau)
 2203    format (8x,'== List of daughters =',15i6)
C DEBUG some kink parameters :
         IF (ITYP.NE.5) GO TO 500
         CALL QVKINK(IVV,PKINK,PT,PL,IHYP,IER)
         WRITE (IW(6),2205) PKINK,PT,PL,IHYP,IER
 2205    FORMAT (15X,'$$$ QVKINK Kink P,Pt,Pl  =',3f12.4/,
     +           15x,'$$$ QVKINK bit patt,err  =',i12,i5)
 500  continue
C Verify the outputs of new routines:
      write (iw(6),2210)  
 2210 format (/' ===> QWNEWTR Results of QKINKV,QKINKT,QNUCL',
     +        ',QLV0V,QLVOT'/)
      CALL QKINKV(KFKIV,KLKIV)
      DO 775 II=KFKIV,KLKIV
         WRITE (IW(6),2211) II
 2211    FORMAT (20X,'++ QKINKV --> Vertex number =',I8)
 775  CONTINUE
C
      CALL QKINKT(NKINKS,LISKINK)
      do 883 iki=1,nkinks
         WRITE (IW(6),2212) liskink(Iki)
 2212    FORMAT (20X,'++ QKINKT --> Track number  =',I8)
 883  continue
C
      CALL QNUCL(KFNIV,KLNIV)
      DO 776 II=KFNIV,KLNIV
         WRITE (IW(6),2213) II
 2213    FORMAT (20X,'++ QNUCL  --> Vertex number =',I8)
 776  CONTINUE
C
      CALL QLV0V(KFLV0,KLLV0)
      DO 777 II=KFLV0,KLLV0
         WRITE (IW(6),2214) II
 2214    FORMAT (20X,'++ QLV0V  --> Vertex number =',I8)
 777  CONTINUE
C
      CALL QLV0T(KFLVT,KLLVT)
      DO 778 II=KFLVT,KLLVT
         WRITE (IW(6),2215) II
 2215    FORMAT (20X,'++ QLV0T  --> Track number  =',I8)
         IENDV=KENDV(II)
         write (iw(6),2216) iendv
 2216    format(23x,'++ QLV0T end vertex for this V0 =',I6)
         ndaut=kndau(ii)
         write (iw(6),2217) ndaut,(kdau(ii,kk),kk=1,ndaut)
 2217    format (25x,'++ QLV0T ndaughters =',i5,
     +           ' List of daughters =',2i8)
 778  CONTINUE
C
C VERIFY SOME TRACK ATTRIBUTES:
C
      write (iw(6),2300)
 2300 format (/' ===> QWNEWTR Verification of track/vertex',
     +         ' relations :'/)
      DO 900 ITK=KFCHT,KLCHT
        IOVER=KORIV(ITK)
        IENVER=KENDV(ITK)
        IF (IOVER.NE.0) THEN
           KNM=KNMOTH(ITK)
           kmomo=0
           if (knm.gt.0) kmomo=kmoth(itk,knm) 
           WRITE (IW(6),2305) ITK,IOVER,KMOMO
 2305      FORMAT (10X,'*** For track =',i6,' Origin vertex =',i6,
     +         5x,' Mother track =',i6)
        endif
        IF (IENVER.NE.0) THEN
           NDOG=KNDAU(ITK)
           WRITE (IW(6),2306) ITK,IENVER,NDOG
 2306      FORMAT (10X,'*** For track =',i6,3X,' End vertex =',i6,      
     +             2x,'--- N daughters =',i6)
           DO 800 IHA=1,NDOG
              WRITE (IW(6),2307) IHA,KDAU(ITK,IHA)
 2307         FORMAT (15X,'>> Daughter # ',i5,'  is Track # ',i7)         
 800       continue
        endif
 900  continue
C
 999  return
      END
#endif

