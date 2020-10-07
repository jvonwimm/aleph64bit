      SUBROUTINE BCALINFO(dummy)
*=========================================================================
*	BCAL object reconstruction program
*-------------------------------------------------------------------------
*
* Author	: I.C. PARK	- 19970424
*
* Description	:
*
*	This routine may be called inside ALPHA to have physics objects
*	reconstructed from BCAL. Mainly the objects are the scattered
*	beam particles
*
* Input		: nothing
* Output	: by COMMON
* Banks		: BCTR, BCSC, BCSL, BCGN are used
*                 BCHG from 1998 
*-----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
      logical first/.true./
      integer ireco(4),itrue(4),Nbc(4)
      real    P3(3),ENR(4),THR(4),PHR(4),ENT(4),THT(4),PHT(4)
      integer Year / 98 /
      integer iBCAL
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      IF (First) THEN
        write(*,*)'+---------------------------------------------+'
        write(*,*)'|                BCALINFO v. 2.0              |'
        write(*,*)'|---------------------------------------------|'  
c-> Read the BCAL alpha card for the geometry year (97 or 98)
        if (XMCEV) then
         iBCAL = IW(namind('BCAL'))
         if (iBCAL.ne.0) Year = IW(iBCAL+1)
        else
         if (KRUN.lt.44900) then 
            Year = 97
         else 
            Year = 98
         endif
        endif
c
        write(*,10) Year
        write(*,*) '+---------------------------------------------+' 
 10     format(' |          Using BCAL++ ',i2,' Geometry           |')
c
        call FillGains(Year)
c
        First=.FALSE.
      ENDIF
*- - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*	reset common variables
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      KFBCR=0
      KLBCR=0
      KNBCR=0
      KFBCT=0
      KLBCT=0
      KNBCT=0
      NbR=0
      NbT=0
      call vzero(IMbR,4)
      call vzero(ENbR,4)
      call vzero(THbR,4)
      call vzero(PHbR,4)
      call vzero(IMbT,4)
      call vzero(ENbT,4)
      call vzero(THbT,4)
      call vzero(PHbT,4)
      call vzero(BErawAPD,4)
      call vzero(BErawPMT,4)
      call vzero(Sch_APD,4)
      call vzero(Sch_PMT,4)
      call vzero(BRcen,4)
      call vzero(BPcen,4)
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*	access BCAL information
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
c-> Fill the ctes commons for this year's geometry
      call FillCtes(Year) 

      IF (XMCEV) THEN
        call bcsimu(Year,Nbc,EnR,ThR,PhR,EnT,ThT,PhT)
      ELSE
        call bcdata(Year,Nbc,EnR,ThR,PhR)
      ENDIF
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*	fill bcalinfo.h
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      do i=1,4
        if (Nbc(i).ne.0) then
          NbR=NbR+1
          IMbR(NbR)=i
          ENbR(NbR)=ENR(i)
          THbR(NbR)=THR(i)
          PHbR(NbR)=PHR(i)
          if (XMCEV) then
            NbT=NbT+1
            IMbT(NbT)=i
            ENbT(NbT)=ENT(i)
            THbT(NbT)=THT(i)
            PHbT(NbT)=PHT(i)
          endif
        endif
      enddo
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*	create & save as ALPHA track
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
* save reco
      KNBCR=NbR
      if (KNBCR.ne.0) then
        do i=1,KNBCR
          ireco(i)=KVNEW(dummy)
          P3(1)=ENbR(i)*SIN(THbR(i))*COS(PHbR(i))
          P3(2)=ENbR(i)*SIN(THbR(i))*SIN(PHbR(i))
          P3(3)=ENbR(i)*COS(THbR(i))
          call QVSET3(ireco(i),P3)
        enddo
        KFBCR=ireco(1)
        KLBCR=KFBCR+KNBCR-1
      endif
* save true
      KNBCT=NbT
      if (KNBCT.ne.0) then
        do i=1,KNBCT
          itrue(i)=KVNEW(dummy)
          P3(1)=ENbT(i)*SIN(THbT(i))*COS(PHbT(i))
          P3(2)=ENbT(i)*SIN(THbT(i))*SIN(PHbT(i))
          P3(3)=ENbT(i)*COS(THbT(i))
          call QVSET3(itrue(i),P3)
        enddo
        KFBCT=itrue(1)
        KLBCT=KFBCT+KNBCT-1
      endif
*      write(*,*)'----- BCALINFO 1 -----'
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      RETURN
      END
*_________________________________________________________________________

      subroutine bcsimu(Year,Nbc,EnR,ThR,PhR,EnT,ThT,PhT)
*=========================================================================
*  BCAL simulation
*-------------------------------------------------------------------------
*
*  Author	: Inkyu PARK - 19970222
*
*  Input	: Year          - Year (97 or 98) Geometry
*  Output	: Nbc/I(4)	- Number of MC objects in the module i
*		  EnR/R(4)	- Energy deposit in the module i
*		  ThR/R(4)	- Theta angle of centroid
*		  PhR/R(4)	- Phi angle of centroid
*		  EnT/R(4)	- True energy deposit
*		  ThT/R(4)	- True theta angle of MC SUM track
*		  PhT/R(4)	- True phi angle of MC SUM track
*
*  Description	:
*
*	A BCAL module can not distinguish multiple particles. Here all 
*	MC particles in a BCAL module acceptance are used to give 
*	reconstructed energy. MC true quantities are derived from the 
*	sum track of all engaged particles.
*
*  Modifications : 
*
*   G. Merino - 19980223 : Add the possibility of obtaining the R and Phi centroid values
*                          from the "Fast Simulation" bcalmc. 
*-----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
      integer   Year
      integer 	Nbc(4),isum(4),Npar,Ipar(2,50)
      real	P(4),V(3),ENR(4),THR(4),PHR(4),ENT(4),THT(4),PHT(4)
      real      Ppar(4,50),Vpar(3,50)
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*	reset common array
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      do imod=1,4
        Nbc(imod)=0
        ENR(imod)=0.0
        THR(imod)=0.0
        PHR(imod)=0.0
        ENT(imod)=0.0
        THT(imod)=0.0
        PHT(imod)=0.0
        isum(imod)=KVNEW(dummy)
      enddo
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*	loop on all MC
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      do Imod=1,4
        Npar=0
        call vzero(Ipar,50)
        call vzero(Ppar,4*50)
        call vzero(Vpar,3*50)
        do imct=kfmct,klmct
        if (KSTABC(imct).ge.1) then
        if (ABS(QCT(imct)).GE.0.995) then
          Kcharge=KCH(imct)
          Kparcod=KTPCOD(imct)
          if (kparcod .eq. 1) then			! gamma
            iparcod = 0
          elseif (kparcod.eq.2 .or. kparcod.eq.3) then	! electron
            iparcod = 1
          elseif (kparcod.eq.5 .or. kparcod.eq.6) then	! muon
            iparcod = 2
          elseif (kparcod.ge.8.and. kparcod.lt.16) then	! hadron
            iparcod = 3
          else						! etc.
            iparcod = 4
          endif
          call QVGET4(P,imct)
          ivtx=KORIV(imct)
          V(1)=QVX(ivtx)
          V(2)=QVY(ivtx)
          V(3)=QVZ(ivtx)
          call BCAL_TAG(QELEP/2.0,Kcharge,P,V,Imodule)
          if (Imodule.EQ.Imod) then
            Npar=Npar+1
            Ipar(1,Npar)=Iparcod
            Ipar(2,Npar)=Kcharge
            do i=1,4
              Ppar(i,Npar)=P(i)
            enddo
            do i=1,3
              Vpar(i,Npar)=V(i)
            enddo
            call qvaddn(isum(imod),imct)
          endif
        endif
        endif
        enddo
        call bcalmc(QELEP/2.0,Year,Npar,Ipar,Ppar,Vpar,
     |              En,Th,Ph,Rcen,Pcen,Ierr)
        if (Ierr.eq.0.and.En.gt.0.0) then
          Nbc(Imod)=1
          ENR(Imod)=En
          THR(Imod)=Th
          PHR(Imod)=Ph
          BErawAPD(imod)=En
          BErawPMT(imod)=En
          BRcen(imod)=Rcen
          BPcen(imod)=Pcen
        endif
      enddo
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*     MC True information. Add all the particles hitting BCAL... 
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      do imod=1,4
        if (Nbc(imod).ne.0) then
          ENT(imod)=QE(isum(imod))
          THT(imod)=ACOS(QCT(isum(imod)))
          PHT(imod)=QPH(isum(imod))
        endif
      enddo
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      return
      end
*_________________________________________________________________________

      subroutine bcdata(Year,Nbc,EnR,ThR,PhR)
*=============================================================================
*   BCAL OBJECT INFORMATION
*-----------------------------------------------------------------------------
*
*  Author	: Inkyu PARK - 19970222
*
*  Input	: Year          - Year (97 or 98) Geometry
*  Output	: Nbc/I(4)	- Number of objects in the module i
*		  EnR/R(4)	- Energy deposit in the module i
*		  ThR/R(4)	- Theta angle of centroid
*		  PhR/R(4)	- Phi angle of centroid
*
*  Description	:
*
*	Access BCAL banks in data and extract information and fill
*
*  Modifications :  
*
*   G. Merino - 19980223 : Add the conversion constants from Fast-Bus (2g) ADC counts to
*                          VME LeCroy (lumi) ADC counts (APD_calib & PMT_calib).
*
*   G. Merino - 19980522 : -Look at the trigger mask silicon and 
*                          HV (only after 1998) and DAQ error bits 
*                          to reject "bad" events.
*                          -Add the readout of BCHG bank. 
*-------------------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
      integer 	Nbc(4)
      real	ENR(4),THR(4),PHR(4)
      integer	adcsil(16,6),adcsum
      integer   PMT(4),APD(4),SIL(4,6,16)
      parameter (Nadc=3,Ncha=8,Nmul=16,Ncom=2)
      integer Jmod(Ncha,Nadc)/
     | 0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3 /
      integer Jpad(Nmul)/
     |  0, 2, 4, 6, 8,10,12,14,15,13,11, 9, 7, 5, 3, 1 /
      integer Jwaf(Ncha,Ncom,Nadc)/
     |	4,5,2,3,0,1,1,0,        5,4,3,2,1,0,0,1,
     |	3,2,5,4,1,0,3,2,        2,3,4,5,0,1,2,3,
     |	5,4,4,5,3,2,0,1,        4,5,5,4,2,3,1,0/
      logical first/.true./      
      integer ON      /'2000'X/
      integer STBY    /'1000'X/
      integer SLUM    /'0002'X/
      integer DAQERR  /'8000'X/ 
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
*      WRITE(*,*)'----- BCALINFO 0 -----'
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*	reset array
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      call vzero(PMT,4)
      call vzero(APD,4)
      call vzero(SIL,4*6*16)
      do imod=1,4
        Nbc(imod)=0
        ENR(imod)=0.0
        THR(imod)=0.0
        PHR(imod)=0.0
      enddo
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*	Banking
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*      nBLUM=NAMIND('BLUM')
*      kBLUM=IW(nBLUM)
*      IF (kBLUM .ne. 0) THEN
*        write(*,*)'BCALINFO : Random Trigger Event?',KRUN,KEVT
*        write(*,*)'BCALINFO : KCLAS-29',JBIT(KCLASW,29)
*        write(*,*)'BCALINFO : BCTR-BCSC-BCSL',IW(NAMIND('BCTR')),
*     |             IW(NAMIND('BCSC')),IW(NAMIND('BCSL'))
*      ENDIF

*       -----------------------------
*	  Searches Bank information
*	-----------------------------
      nBCHG = NAMIND('BCHG')
      kBCHG = IW(nBCHG)
      IF (kBCHG .EQ. 0) GOTO 100
      if (IAND(STBY,MskTrg).eq.STBY.or.
     .    IAND(ON,MskTrg).eq.ON) then
         DO i=1,IW(kBCHG+2)
            Isci=IBITS(IW(kBCHG+2+i),16,16)+1
            Kadc=IBITS(IW(kBCHG+2+i),0,16)
            if (Isci .le. 4) then
               Ipmt=Isci
               Sch_PMT(Ipmt)= 
     .    float(Kadc)*Gain_PMT(Ipmt)*PMT_lumitofb(Ipmt)*PMT_fbtosc(Ipmt)
            elseif (Isci .gt. 4) then
               Iapd=Isci-4
               Sch_APD(Iapd)= 
     .    float(Kadc)*Gain_APD(Iapd)*APD_lumitofb(Iapd)*APD_fbtosc(Iapd)
            endif
         ENDDO
      else
         goto 100
      endif
 100  continue
*	-----------------------------
*	2g events Trigger information
*	-----------------------------
      MskTrg=0
      call vzero(ModTrg,4)
      nBCTR = NAMIND('BCTR')
      kBCTR = IW(nBCTR)
      IF (kBCTR .EQ. 0) GOTO 999
      MskTrg = IW(kBCTR+2+1)
      do imod=1,4
        idx=8-imod
        if (IBITS(INT(MskTrg),idx,1).eq.1) ModTrg(imod)=1
      enddo

*       -----------------------------------------
*       col<->row swapped in 1997 for BCSC BCSL!!
*       -----------------------------------------
      if (KRUN.lt.44900) then
         ncol = 1
      else
         ncol = 2
      endif
*	------------------------
*	Scintillator information
*	------------------------
      nBCSC = NAMIND('BCSC')
      kBCSC = IW(nBCSC)
      IF (kBCSC .EQ. 0) GOTO 999
c      write(*,*)'number of scints',IW(kBCSC+1)
      if (KRUN.lt.44900.or.
     .    IAND(STBY,MskTrg).eq.STBY.or.
     .    IAND(ON,MskTrg).eq.ON) then
         DO i=1,IW(kBCSC+ncol)
            Isci=IBITS(IW(kBCSC+2+i),16,16)+1
            Kadc=IBITS(IW(kBCSC+2+i),0,16)
            if (Isci .le. 4) then
               Ipmt=Isci
               PMT(Ipmt)=Kadc
            elseif (Isci .gt. 4) then
               Iapd=Isci-4
               APD(Iapd)=Kadc
            endif
         ENDDO
      else
         goto 999
      endif
*	-------------------
*	Silicon Information
*	-------------------
c
c Make sure that the event has SILICONFORALEPH
c From 1998 on, make sure that:
c      The BCAL++ HV was in STBY mode.
c      There was no timeout error when reading the silicon       
c
      if (IAND(SLUM,MskTrg).ne.SLUM.and.
     .    (KRUN.lt.44900.or.      
     .     (IAND(DAQERR,MskTrg).ne.DAQERR.and.
     .      (IAND(STBY,MskTrg).eq.STBY.or.
     .       IAND(ON,MskTrg).eq.ON)))) then 
         nBCSL = namind('BCSL') 
         kBCSL = IW(nBCSL)
         IF (kBCSL .EQ. 0) GOTO 999
c        write(*,*)'number of silicons infos.',IW(kBCSL+1)
         DO i=1,IW(kBCSL+ncol)
            Iacm=IBITS(INT(IW(kBCSL+2+i)),16,16) ! Iacm 3*8*16(0->383)
            IADC=INT(Iacm/Nmul/Ncha)+1
            ICHA=MOD(MOD(Iacm,Nmul*Ncha),Ncha)+1
            IMUL=INT(MOD(Iacm,Nmul*Ncha)/Ncha)+1
            ICOM=(IMUL-1)/8+1
            Imod=Jmod(ICHA,IADC)+1
            Ipad=Jpad(IMUL)+1
            Iwaf=Jwaf(ICHA,Icom,IADC)+1
            SIL(Imod,Iwaf,Ipad)=FLOAT(IBITS(IW(kBCSL+2+i),0,16))
         ENDDO
      else
         continue
      endif
*       --------------
*	 Gains Bank
*	--------------
      nBCGN = namind('BCGN')
      kBCGN = IW(nBCGN)
      IF (kBCGN .NE. 0) THEN
*        write(*,*)'----- BCALGAIN 0 -----'
*        write(*,*)' BCAL++ gains are changed in (KRUN:KEVT)',KRUN,KEVT
*        write(*,*)' Old:',Gain_PMT,Gain_APD
        DO i=1,4
          Gain_PMT(i)=RW(kBCGN+2+i)
          Gain_APD(i)=RW(kBCGN+2+i+4)
        ENDDO
*        write(*,*)' New:',Gain_PMT,Gain_APD
*        write(*,*)'----- BCALGAIN 1 -----'
      ENDIF
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*	result
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      do imod=1,4
        adcpmt=float(PMT(imod))*Gain_PMT(imod)*PMT_lumitofb(imod)
        adcapd=float(APD(imod))*Gain_APD(imod)*APD_lumitofb(imod)
        adcsum=0
        do iwaf=1,6
        do ipad=1,16
          adcsil(ipad,iwaf)=SIL(imod,iwaf,ipad)
          adcsum=adcsum+adcsil(ipad,iwaf)
        enddo
        enddo
        if (adcpmt.gt.0.0 .and. adcapd.gt.0.0 .and. adcsum.gt.0) then
c-> Use the APD for 1997 data but the PMT for 1998 data... 
          if (KRUN.ge.44900) then
             idev = 2
          else
             idev = 1
          endif  
          call bcaldata(QELEP/2.0,Year,imod,idev,adcpmt,adcapd,adcsil,
     .                  En,Th,Ph,Rcen,Pcen,Ierr)
          if (Ierr .eq. 0) then
            Nbc(imod)=1
            ENR(imod)=En
            THR(imod)=Th
            PHR(imod)=Ph
            BErawAPD(imod)=adcapd
            BErawPMT(imod)=adcpmt
            BRcen(imod)=Rcen
            BPcen(imod)=Pcen
*            if (En .eq. 0.0) then
*            write(*,*)'BCAL object',imod,En,Th,Ph,adcpmt,adcapd,adcsum
*            endif
          endif
        endif
      enddo
*      WRITE(*,*)'----- BCALPHYS 1 -----'
      RETURN
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
  999 CONTINUE
*      WRITE(*,*)'BCDATA   : ',nBCTR,kBCTR
      RETURN
      END
C_______________________________________________________________________

      LOGICAL FUNCTION XBCA(itrk)
*=======================================================================
*	BCAL "raw" information 
*-----------------------------------------------------------------------
*
* Author	: I.C. PARK	- 19971203
*
* Description	:
* 
*     Is true if the "raw" information for BCAL is available for that event
*
* Modifications :
*
*  G. Merino - 19980223 : The MC events will contain also "raw" information 
*                         (centroids...)  
*
*----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      XBCA=.FALSE.
      IF (XMCEV) XBCA=.TRUE.
      IF (itrk.ge.KFBCR .and. itrk.LE.KLBCR) XBCA=.TRUE.
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
      RETURN
      END
C_______________________________________________________________________

      REAL FUNCTION QBCAER(itrk)
*=======================================================================
*	Raw energy of BCAL object
*-----------------------------------------------------------------------
*
* Author	: I.C. PARK	- 19971211
*
* Description	:
*	
*	Return the raw energy of BCAL++ object. Currently we use APD 
*	ADC count.
*
*----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      QBCAER=0.0
      Index=itrk-KFBCR+1
      IF (Index.le.0 .or. Index.gt.4) RETURN
      Imodule=IMbR(Index)
      QBCAER=BErawAPD(Imodule)
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
      RETURN
      END
C_______________________________________________________________________
      REAL FUNCTION QBCAPM(itrk)
*=======================================================================
*	Raw energy of BCAL object
*-----------------------------------------------------------------------
*
* Author	: G.Merino	- 19980522
*
* Description	:
*	
*	Return the raw energy of BCAL++ object as measured by the PMT.
*
*----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      QBCAPM=0.0
      Index=itrk-KFBCR+1
      IF (Index.le.0 .or. Index.gt.4) RETURN
      Imodule=IMbR(Index)
      QBCAPM=BErawPMT(Imodule)
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
      RETURN
      END
C_______________________________________________________________________

      REAL FUNCTION QBCARC(itrk)
*=======================================================================
*	R position of centroid of BCAL object
*-----------------------------------------------------------------------
*
* Author	: I.C. PARK	- 19971211
*
* Description	:
*	
*	Return the R position of centroid reconstruction
*
*----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      QBCARC=0.0
      Index=itrk-KFBCR+1
      IF (Index.le.0 .or. Index.gt.4) RETURN
      Imodule=IMbR(Index)
      QBCARC=BRcen(Imodule)
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
      RETURN
      END
C_______________________________________________________________________

      REAL FUNCTION QBCAPC(itrk)
*=======================================================================
*	Phi position of a BCAL object
*-----------------------------------------------------------------------
*
* Author	: I.C. PARK	- 19971211
*
* Description	:
*	
*	Return the Phi position of a BCAL++ object.
*
*----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      QBCAPC=0.0
      Index=itrk-KFBCR+1
      IF (Index.le.0 .or. Index.gt.4) RETURN
      Imodule=IMbR(Index)
      QBCAPC=BPcen(Imodule)
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
      RETURN
      END
C_______________________________________________________________________

      INTEGER FUNCTION KBCALTAG(itrk)
*=======================================================================
*	Checking tag
*-----------------------------------------------------------------------
*
* Author	: I.C. PARK	- 19971218
*
* Input		: itrk	/I
*
* Description	:
*	
*	Return the module number if the track passes a BCAL module.
*	0: can not be tagged by BCAL
*	i: should be tagged by BCAL module i
*
*----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
      REAL P(4),V(3)
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      KBCALTAG=0
      IF (KCLASS(itrk) .EQ. KMONTE) THEN
        if (KSTABC(itrk).ge.1) then
        if (ABS(QCT(itrk)).GE.0.995) then
          Kcharge=KCH(itrk)
          Kparcod=KTPCOD(itrk)
          if (Kparcod .eq. 1) then			! gamma
            iparcod = 0
          elseif (Kparcod.eq.2 .or. Kparcod.eq.3) then	! electron
            iparcod = 1
          elseif (Kparcod.eq.5 .or. Kparcod.eq.6) then	! muon
            iparcod = 2
          elseif (Kparcod.ge.8.and. Kparcod.lt.16) then	! hadron
            iparcod = 3
          else						! etc.
            iparcod = 4
          endif
          call QVGET4(P,itrk)
          ivtx=KORIV(itrk)
          V(1)=QVX(ivtx)
          V(2)=QVY(ivtx)
          V(3)=QVZ(ivtx)
          call BCAL_TAG(QELEP/2.0,Kcharge,P,V,Imodule)
          KBCALTAG=Imodule
        endif
        endif
      ELSE
        Index=itrk-KFBCR+1
        IF (Index.le.0 .or. Index.gt.4) RETURN
        Imodule=IMbR(Index)
        KBCALTAG=Imodule
      ENDIF
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
      RETURN
      END
C_______________________________________________________________________


      REAL FUNCTION QBCHER(Imodule)
*=======================================================================
*	Raw energy of BCAL object measured by the APD
*-----------------------------------------------------------------------
*
* Author	: G. Merino - 19980522
*
* Description	:
*	
*	Return the raw energy measured by the APD in the  BCAL++ module 
*       number Imodule (Imodule=1,2,3,4)
*
*----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      QBCHER=0.0
      IF (Imodule.le.4 .and. Imodule.ge.1) then
         QBCHER=Sch_APD(Imodule)
      ENDIF
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
      RETURN
      END
C_______________________________________________________________________

      REAL FUNCTION QBCHPM(Imodule)
*=======================================================================
*	Raw energy of BCAL object measured by the PMT
*-----------------------------------------------------------------------
*
* Author	: G. Merino - 19980522
*
* Description	:
*	
*	Return the raw energy measured by the PMT in the  BCAL++ module 
*       number Imodule (Imodule=1,2,3,4)
*
*----------------------------------------------------------------------
#include "qcde.h"
#include "bcalinfo.h"
#include "qmacro.h"
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      QBCHPM=0.0
      IF (Imodule.le.4 .and. Imodule.ge.1) then
         QBCHPM=Sch_PMT(Imodule)
      ENDIF
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
      RETURN
      END
C_______________________________________________________________________


      subroutine FillGains(Year)
*=========================================================================
* 
* Author: G. Merino  - 19980522
*
* Description:
*  
*         Subroutine called only once at the beggining of each event. 
*         Fills the calibration constants arrays of commons in bcalinfo.h with
*          some default values.
*
*-------------------------------------------------------------------------
#include "bcalinfo.h"

      integer i,j,I_db
      integer Year

c-> Constants DATABASE (_db)
      integer Nperiods 
      parameter (Nperiods = 2)

c -> Default  APD/PMT gains and Inter-ADC calibrations
      real Gain_PMT_db(4,Nperiods),Gain_APD_db(4,Nperiods)
      real PMT_lumitofb_db(4,Nperiods),APD_lumitofb_db(4,Nperiods)
      real PMT_fbtosc_db(4,Nperiods),APD_fbtosc_db(4,Nperiods)
      common/Gains_db/ Gain_PMT_db,Gain_APD_db,
     .                 PMT_lumitofb_db,APD_lumitofb_db,
     .                 PMT_fbtosc_db,APD_fbtosc_db 
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =      

      if (Year.eq.97) then	
c*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c 1997 setting
c*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         I_db = 1         
      elseif (Year.eq.98) then
c*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c 1998 setting
c*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         I_db = 2
      else
         write(6,*) 'FillGains: Wrong Geometry Year',Year
         stop
      endif
        
c-> Default APD/PMT gains and Inter-ADC calibrations
      do i = 1,4
         Gain_PMT(i)= Gain_PMT_db(i,I_db)
         Gain_APD(i)= Gain_APD_db(i,I_db)
c
         PMT_lumitofb(i) = PMT_lumitofb_db(i,I_db)
         APD_lumitofb(i) = APD_lumitofb_db(i,I_db)
c
         PMT_fbtosc(i) = PMT_fbtosc_db(i,I_db)
         APD_fbtosc(i) = APD_fbtosc_db(i,I_db)
      enddo	
c*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      return
      end
