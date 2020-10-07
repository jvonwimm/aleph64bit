      SUBROUTINE FTRGAL(IOK)
C
C--------------------------------------------------------------
C! Fast simulation of the trigger -- L. Duflot  Fev. 1996
C
C  IOK= 1  if yes
C--------------------------------------------------------------
C
C known limitations/problems :
C
C  - no muon energy deposit in ECAL
C  - no HCAL pattern for electrons and photons
C  - there is no overlap treatment : all energy is in
C    barrel or endcap. idem for planes in HCAL
C  - cracks not taken into account for neutral hadrons in HCAL
C  - tracks are not extrapolated to ITC using B field
C  - electron energy deposit is not in agrement with Rt
C  - penetration is underestimated for muons
C    no extrapolation for P<1
C  - in penetration of hadron, ECAL interaction is 'simulated'
C    but not propagated to the calculation of Rt,Rl nor
C    to the ECAL energy deposit.
C
#include "parcut.h"
#include "qcde.h"
#include "ftrig.h"
      integer ITCbits(3),AGETDB
      logical first,nox1tv,BTEST
      save first,nox1tv

      data first / .TRUE. /

#include "qmacro.h"
      if ( first ) then
        first = .FALSE.
        if ( NLINK('X1TV',1) .le. 0 ) then
          IND = AGETDB('X1TV',1)
          if ( ind .eq. 0 ) then
            print *,' no X1TV bank !!! no trigger information !!'
            nox1tv = .TRUE.
          else
            nox1tv = .FALSE.
          endif
        endif
        call BKFMT('ewtr','(I)')
        call BKFMT('hwtr','(I)')
        call BKFMT('ittr','(I)')
        call BKFMT('x1rg','2I,(A,3I)')
        call BKFMT('pwei','(I)')
        call BKFMT('pewi','(I)')

        CALL BLIST(IW,'E+','PWEI')
        CALL BLIST(IW,'E+','PEWI')

      endif
C
      IOK= 0
C
      if ( nox1tv ) then
        iok = 1            ! trigger cannot be applied suppose it is OK
        return
      endif
C
C... rename existing banks (i.e. if we are running on POT/DST)
      call bdrop(iw,'ewtrettrhwtrlwtrittrsifix1rgx1ip')
      call bdrop(iw,'pweipewi')
      call BSWAP(IW,'EWTR','ewtr')
      call BSWAP(IW,'ETTR','ettr')
      call BSWAP(IW,'HWTR','hwtr')
      call BSWAP(IW,'LWTR','lwtr')
      call BSWAP(IW,'ITTR','ittr')
      call BSWAP(IW,'SIFO','sifo')
      call BSWAP(IW,'X1RG','x1rg')
      call BSWAP(IW,'X1IP','x1ip')
      call BSWAP(IW,'PWEI','pwei')
      call BSWAP(IW,'PEWI','pewi')
C
C... make calorimeter-type trigger segments from ITC trigger segments
      call MkITCbits(ITCbits)
C
C... create relevant banks and make trigger
      LMODU = 12
      LCOMP = 3
      CALL ALBOS( 'EWTR', 0, LMHLEN+2*LMODU*LCOMP, JEWTR, IGARB)
      IW( JEWTR+LMHCOL) = 2
      IW( JEWTR+LMHROW) = LMODU*LCOMP
      if ( IGARB .eq. 1 ) call qmterm(
     &  '++++ FTRGAL +++ GARBAGE collection occured ! Increase BOS !!')
      do i = 1 , 36
        e = E_ECAL(i)*1.e6/2.          ! energy in keV of odd/even planes
        IW(JEWTR+LMHLEN+2*i-1) = E
        IW(JEWTR+LMHLEN+2*i) = E
      enddo

      NPEWI = 36
      LPEWI = 55
      CALL ALBOS( 'PEWI', 0, LMHLEN+LPEWI*NPEWI, JPEWI, IGARB)
      IW( JPEWI+LMHCOL) = LPEWI
      IW( JPEWI+LMHROW) = NPEWI
      if ( IGARB .eq. 1 ) call qmterm(
     &  '++++ FTRGAL +++ GARBAGE collection occured ! Increase BOS !!')
      do i = 1 , 36
        e = E_ECAL(i)*1.e6/45.            ! energy in keV per plane
        IW(JPEWI+LMHLEN+LPEWI*(i-1)+1) = i
        do j = 1 , 45
           IW(JPEWI+LMHLEN+LPEWI*(i-1)+1+j) = e
        enddo
        do j = 46 , LPEWI 
           IW(JPEWI+LMHLEN+LPEWI*(i-1)+1+j) = 0
        enddo
      enddo
      NPEWI = 36
      LPEWI = 55
      CALL ALBOS( 'PWEI', 0, LMHLEN+LPEWI*NPEWI, JPWEI, IGARB)
      do i = 1 , LMHLEN+LPEWI*NPEWI
         IW(JPWEI + i) = IW(JPEWI + i)
      enddo
C... fill also QEECWI (ALPHA variable)
      call QFECWD

c      call prtabl('PWEI',0)

      NHWTR = 36
      LHWTRA = 1
      CALL ALBOS ('HWTR',0,NHWTR*LHWTRA+LMHLEN,JHWTR,IGARB)
      IW(JHWTR+1) = LHWTRA
      IW(JHWTR+2) = NHWTR
       if ( IGARB .eq. 1 ) call qmterm(
     &  '++++ FTRGAL +++ GARBAGE collection occured ! Increase BOS !!')
      do i = 1 , 36
C should be double planes isn't it ? Or only in the barrel ?
        IW(JHWTR+LMHLEN+i) = T_HCAL(i) / 2
      enddo


      NRL1 = 1
      NDL1 = 4
      CALL ALBOS('ITTR',NRL1,NDL1+LMHLEN,ITTR,IGARB)
      IW(ITTR+LMHCOL) = NDL1
      IW(ITTR+LMHROW) = 1
      if ( IGARB .eq. 1 ) call qmterm(
     &  '++++ FTRGAL +++ GARBAGE collection occured ! Increase BOS !!')
      do i = 1 , 3
        IW(ITTR+LMHLEN+i) = ITCbits(i)
      enddo
      IW(ITTR+LMHLEN+4) = 0
C
C... build the trigger banks X1RG,X1IP
      call x1trig
C
      call altrig(itrig1,itrig2,itrig3)
      if ( BTEST(itrig1,8)  .OR.            !  single muon
     &     BTEST(itrig1,9)  .OR.            !  single charged EM
     &     BTEST(itrig1,17) .OR.            !  total energy barrel
     &     BTEST(itrig1,18) .OR.            !  total energy end cap A
     &     BTEST(itrig1,19) .OR.            !  total energy end cap B
     &     BTEST(itrig1,20)                 !  total energy coincid. A B
     &   ) iok = 1
C
C... fill ALPHA variables ( do not appear in the ALPHA manual !)
      KXTET1 = itrig1
      KXTET2 = itrig2
      KXTEL2 = itrig3

      RETURN
      END
