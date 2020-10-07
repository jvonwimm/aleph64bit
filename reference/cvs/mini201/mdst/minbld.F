      SUBROUTINE MINBLD(NWANT,WLIST,KEEP,CLASS)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Build Mini banks for data.
C
C     Author: Stephen Haywood      06-Jan-93
C     Modify: Agnieszka Jacholkowska   04-Feb-98
C
C     Input  : NWANT  = number of banks listed on MINI card
C              WLIST  = Wish-list of banks to be written on Mini
C              KEEP   = flag indicating whether event should be kept
C              CLASS  = logical array set according to class word
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      CHARACTER*800 WLIST
      CHARACTER*4 NAME
      LOGICAL WANT,CLASS
      DIMENSION CLASS(32)
C
#include "bmacro.h"
C
C++   Logical function to decide whether a bank is wanted.
C++   An empty list means want all banks.
C
      WANT(NAME) = WLIST.EQ.' ' .OR. INDEX(WLIST(1:4*NWANT),NAME).GT.0
C
      CALL ABRUEV(IRUN,IEVT)
C
      IGEOYR=0
      IASIM=IW(NAMIND('ASIM'))
      IF (IASIM.GT.0) THEN
         IYG=ITABL(IASIM,1,1)
         IGEOYR=1900+IYG/100
      ENDIF
C
C++   Add EVEH (compulsory) and REVH to Mini list.
C
      CALL MINLIS('EVEH')
      IF (WANT('REVH')) THEN
         KREVH = NLINK('REVH',0)
         IF (KREVH.GT.0) CALL MINLIS('REVH')
      ENDIF
C
C++   Add X1RG  to Mini list.
C
      IF (WANT('X1RG')) THEN
         KX1RG = NLINK('X1RG',0)
         IF (KX1RG.GT.0) CALL MINLIS('X1RG')
      ENDIF
C
C++   Add PLPD  to Mini list.
C
      IF (WANT('PLPD')) THEN
         KPLPD = NLINK('PLPD',0)
         IF (KPLPD.GT.0) CALL MINLIS('PLPD')
      ENDIF
C
C++   If LUPA, add it to Mini list.
C
      IF (WANT('LUPA')) THEN
         KLUPA = NLINK('LUPA',0)
         IF (KLUPA.GT.0) CALL MINLIS('LUPA')
      ENDIF
C
C++   Fill Sical banks and if SILH exists, add it to Mini list.
C
      IF (WANT('DSIC')) THEN
         CALL MINSIC
      ENDIF
C
C++   If this is a simple lumi event, don't create more banks.
C
      IF (KEEP.LE.0) RETURN
C
C++   Store version information for interesting events - compulsory.
C
      CALL MINVRS
C
C++   Save BOM information.
C
      IF (WANT('BOMB')) THEN
         IF (LROWS(NLINK('BOMB',0)).GT.0) CALL MINLIS('BOMB')
      ENDIF
C
C++   Fill event information banks.
C
      IF (WANT('DEVT')) CALL MINEVT
C
C++   Fill trigger banks.
C
      IF (WANT('DTBP')) CALL MINTBP
C
C++   Fill vertex banks.
C
      IF (WANT('DVER')) CALL MINVER
C
C++   Fill track banks.
C
      IF (WANT('DTRA')) CALL MINTRA
C
C++   Save dE/dx from wires.
C
      IF (WANT('PTEX')) THEN
         IF (LROWS(NLINK('PTEX',0)).GT.0) CALL MINLIS('PTEX')
      ENDIF
C
C++   Save dE/dx from pads.
C
      IF (WANT('PTPX')) THEN
         IF (LROWS(NLINK('PTPX',0)).GT.0) CALL MINLIS('PTPX')
      ENDIF
C
C++   Fill photon identification banks.
C
      IF (WANT('DGID')) CALL MINGID
      IF (WANT('DGAC')) CALL MINGAC
C
C++   Fill electron identification banks.
C
      IF (WANT('DEID')) CALL MINEID
C
C++   Fill NEW lepton identification banks.
C
      IF (WANT('DDLT')) CALL MINDLT
      IF (WANT('DMLT')) CALL MINMLT
      IF (WANT('DLJT')) CALL MINJLT
C
C++   Fill NEW kink banks.
C
      IF (WANT('DKNK')) CALL MINKNK
C
C++   Fill NEW odd-even energy sums banks.
C
      IF (WANT('DWES')) CALL MINEWS
C
C++   Save muon identification banks.
C
      IF (WANT('MUID')) CALL MINMUI
C
C++   Fill further calorimeter banks - for non-qqbar events.
C
C++      Modification for Runs LEP2.
C
C     IF(IRUN .GT. 40000) THEN
      IF(IGEOYR .GE. 1996 .OR.IRUN .GT. 40000) THEN
         IF (WANT('DECO')) CALL MINECO
         IF (WANT('DHCO')) CALL MINHCO
         IF (.NOT. (CLASS(16).OR.CLASS(17)) .OR.
     &     CLASS(5) .OR. CLASS(20) .OR. CLASS(24)) THEN
C
C++      Fill calorimeter relations bank.
C
           IF (WANT('DCRL')) CALL MINCRL
C
C++      Fill Ecal tower and wire banks.
C
CC         IF (WANT('DECO')) CALL MINECO
           IF (WANT('DEWI')) CALL MINEWI
C
C++      Fill Hcal tower and digital pattern banks.
C
CC         IF (WANT('DHCO')) CALL MINHCO
           IF (WANT('DHRL')) CALL MINHRL
           IF (WANT('DPOB')) CALL MINPOB
         ENDIF
      ELSE
         IF (.NOT. (CLASS(16).OR.CLASS(17)) .OR.
     &     CLASS(5) .OR. CLASS(20) .OR. CLASS(24)) THEN
C
C++      Fill calorimeter relations bank.
C
           IF (WANT('DCRL')) CALL MINCRL
C
C++      Fill Ecal tower and wire banks.
C
           IF (WANT('DECO')) CALL MINECO
           IF (WANT('DEWI')) CALL MINEWI
C
C++      Fill Hcal tower and digital pattern banks.
C
           IF (WANT('DHCO')) CALL MINHCO
           IF (WANT('DHRL')) CALL MINHRL
           IF (WANT('DPOB')) CALL MINPOB
         ENDIF
      ENDIF
C
C++   Multiple scaterring angle for charged tracks for lepton events.
C
      IF (CLASS(24)) THEN
         IF (WANT('DMSC')) CALL MINMSC
      ENDIF
C
C++   Fill energy flow banks.
C
      IF (WANT('DENF')) CALL MINENF
      IF (WANT('DJET')) CALL MINJET
C
C++   Copy extra new POT/DST - nuclear interactions and V0s.
C
      IF (WANT('YNLI')) THEN
         IF (LROWS(NLINK('YNLI',0)).GT.0) CALL MINLIS('YNLI')
      ENDIF
C
      IF (WANT('YNVH')) THEN
         IF (LROWS(NLINK('YNVH',0)).GT.0) CALL MINLIS('YNVH')
      ENDIF
C
      IF (WANT('YLV0')) THEN
         IF (LROWS(NLINK('YLV0',0)).GT.0) CALL MINLIS('YLV0')
      ENDIF
C
C++   Copy extra POT/DST banks for all LEP2 events.
C
C11/02/98      IF (CLASS(24)) THEN
C04/03/98      IF (CLASS(24).OR.CLASS(5)) THEN
c muons
      IF (WANT('HPDI')) THEN
         IF (LROWS(NLINK('HPDI',0)).GT.0) CALL MINLIS('HPDI')
      ENDIF
      IF (WANT('HROA')) THEN
         IF (LROWS(NLINK('HROA',0)).GT.0) CALL MINLIS('HROA')
      ENDIF
      IF (WANT('HMAD')) THEN
         IF (LROWS(NLINK('HMAD',0)).GT.0) CALL MINLIS('HMAD')
      ENDIF
      IF (WANT('MUID')) THEN
         IF (LROWS(NLINK('MUID',0)).GT.0) CALL MINLIS('MUID')
      ENDIF
      IF (WANT('MCAD')) THEN
         IF (LROWS(NLINK('MCAD',0)).GT.0) CALL MINLIS('MCAD')
      ENDIF
      IF (WANT('MUEX')) THEN
         IF (LROWS(NLINK('MUEX',0)).GT.0) CALL MINLIS('MUEX')
      ENDIF
      IF (WANT('MHIT')) THEN
         IF (LROWS(NLINK('MHIT',0)).GT.0) CALL MINLIS('MHIT')
      ENDIF
      IF (WANT('MTHR')) THEN
         IF (LROWS(NLINK('MTHR',0)).GT.0) CALL MINLIS('MTHR')
      ENDIF
      IF (WANT('MUDG')) THEN
         IF (LROWS(NLINK('MUDG',0)).GT.0) CALL MINLIS('MUDG')
      ENDIF
c Eflow
      IF (WANT('PHCO')) THEN
         IF (LROWS(NLINK('PHCO',0)).GT.0) CALL MINLIS('PHCO')
      ENDIF
      IF (WANT('PCPA')) THEN
         IF (LROWS(NLINK('PCPA',0)).GT.0) CALL MINLIS('PCPA')
      ENDIF
      IF (WANT('PEWI')) THEN
         IF (LROWS(NLINK('PEWI',0)).GT.0) CALL MINLIS('PEWI')
      ENDIF
      IF (WANT('PWEI')) THEN
         IF (LROWS(NLINK('PWEI',0)).GT.0) CALL MINLIS('PWEI')
      ENDIF
C
C++   If want HTUB, ensure do not pick up HTUB/1.
C
      IF (WANT('HTUB')) THEN
         KHTU1 = NSWAP('HTUB',1,'HTU1',0)
         CALL BLIST(IW,'E+','HTU1')
         IF (LROWS(NLINK('HTUB',0)).GT.0) CALL MINLIS('HTUB')
      ENDIF
c      IF (WANT('PCRL')) THEN
c         IF (LROWS(NLINK('PCRL',0)).GT.0) CALL MINLIS('PCRL')
c      ENDIF
c      IF (WANT('PPOB')) THEN
c         IF (LROWS(NLINK('PPOB',0)).GT.0) CALL MINLIS('PPOB')
c      ENDIF
c      IF (WANT('PPDS')) THEN
c         IF (LROWS(NLINK('PPDS',0)).GT.0) CALL MINLIS('PPDS')
c      ENDIF
c      IF (WANT('PCOB')) THEN
c         IF (LROWS(NLINK('PCOB',0)).GT.0) CALL MINLIS('PCOB')
c      ENDIF
c      IF (WANT('PHHY')) THEN
c         IF (LROWS(NLINK('PHHY',0)).GT.0) CALL MINLIS('PHHY')
c      ENDIF
c      IF (WANT('PCHY')) THEN
c         IF (LROWS(NLINK('PCHY',0)).GT.0) CALL MINLIS('PCHY')
c      ENDIF
c      IF (WANT('PEST')) THEN
c         IF (LROWS(NLINK('PEST',0)).GT.0) CALL MINLIS('PEST')
c      ENDIF
c      IF (WANT('PHST')) THEN
c         IF (LROWS(NLINK('PHST',0)).GT.0) CALL MINLIS('PHST')
c      ENDIF
c      IF (WANT('HNTO')) THEN
c         IF (LROWS(NLINK('HNTO',0)).GT.0) CALL MINLIS('HNTO')
c      ENDIF
c      IF (WANT('EWHE')) THEN
c         IF (LROWS(NLINK('EWHE',0)).GT.0) CALL MINLIS('EWHE')
c      ENDIF
c      IF (WANT('PLSD')) THEN
c         IF (LROWS(NLINK('PLSD',0)).GT.0) CALL MINLIS('PLSD')
c      ENDIF
c      IF (WANT('ETDI')) THEN
c         IF (LROWS(NLINK('ETDI',0)).GT.0) CALL MINLIS('ETDI')
c      ENDIF
c      IF (WANT('PECO')) THEN
c         IF (LROWS(NLINK('PECO',0)).GT.0) CALL MINLIS('PECO')
c      ENDIF
c      IF (WANT('SILU')) THEN
c         IF (LROWS(NLINK('SILU',0)).GT.0) CALL MINLIS('SILU')
c      ENDIF
c      IF (WANT('ECNS')) THEN
c         IF (LROWS(NLINK('ECNS',0)).GT.0) CALL MINLIS('ECNS')
c      ENDIF
c lcal
      IF (WANT('LCRA')) THEN
         IF (LROWS(NLINK('LCRA',0)).GT.0) CALL MINLIS('LCRA')
      ENDIF
c bcal++
      IF (WANT('BCTR')) THEN
         IF (LROWS(NLINK('BCTR',0)).GT.0) CALL MINLIS('BCTR')
      ENDIF
      IF (WANT('BCSC')) THEN
         IF (LROWS(NLINK('BCSC',0)).GT.0) CALL MINLIS('BCSC')
      ENDIF
      IF (WANT('BCSL')) THEN
         IF (LROWS(NLINK('BCSL',0)).GT.0) CALL MINLIS('BCSL')
      ENDIF
      IF (WANT('BCGN')) THEN
         IF (LROWS(NLINK('BCGN',0)).GT.0) CALL MINLIS('BCGN')
      ENDIF
      IF (WANT('BCHG')) THEN
         IF (LROWS(NLINK('BCHG',0)).GT.0) CALL MINLIS('BCHG')
      ENDIF
c
      IF (WANT('HCCV')) THEN
         IF (LROWS(NLINK('HCCV',0)).GT.0) CALL MINLIS('HCCV')
      ENDIF
c Itc
      IF (WANT('PIDI')) THEN
         IF (LROWS(NLINK('PIDI',0)).GT.0) CALL MINLIS('PIDI')
      ENDIF
c
      IF (WANT('EIDT')) THEN
         IF (LROWS(NLINK('EIDT',0)).GT.0) CALL MINLIS('EIDT')
      ENDIF
      IF (WANT('XTHH')) THEN
         IF (LROWS(NLINK('XTHH',0)).GT.0) CALL MINLIS('XTHH')
      ENDIF
C
C++   If want EFOL, ensure do (not) pick up EFOL/3.
C
      IF (WANT('EFOL')) THEN
         KEFOL = NLINK('EFOL',3)
         IF (KEFOL.GT.0) CALL MINLIS('EFOL')
      ENDIF
C
C
      IF (CLASS(24).OR.CLASS(5)) THEN
      ENDIF
C
      RETURN
      END
#endif
