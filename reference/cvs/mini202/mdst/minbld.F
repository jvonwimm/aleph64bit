      SUBROUTINE MINBLD(NWANT,WLIST,KEEP,CLASS)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Build Mini banks for data.
C
C     Author: Stephen Haywood      06-Jan-93
C     Modify: Agnieszka Jacholkowska   30-Sep-98
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
      INTEGER IDTYP
C
#include "bmacro.h"
C
C++   Logical function to decide whether a bank is wanted.
C++   An empty list means want all banks.
C
      WANT(NAME) = WLIST.EQ.' ' .OR. INDEX(WLIST(1:4*NWANT),NAME).GT.0
C
C++   Determine whether this is a LEP1 or LEP2 data
C
      IDTYP = 0
      CALL MINDTYP(IDTYP)
      IF (IDTYP.GT.0) THEN
C         WRITE(6,'(//40X,''**** Mini-DST LEP'',I2,'' ****'')')
C     &   IDTYP
      ELSE
         WRITE(6,'(//40X,''**** Mini-DST Unkown Type'',I2,'' ****'')')
     &   IDTYP
      ENDIF
C
      CALL ABRUEV(IRUN,IEVT)
C
C!      IGEOYR=0
C!      IASIM=IW(NAMIND('ASIM'))
C!      IF (IASIM.GT.0) THEN
C!         IYG=ITABL(IASIM,1,1)
C!         IGEOYR=1900+IYG/100
C!      ENDIF
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
C++   Multiple scaterring angle for charged tracks for lepton events.
C
      IF (CLASS(24)) THEN
         IF (WANT('DMSC')) CALL MINMSC
      ENDIF
C
C++   Fill energy flow banks.
C
      IF (WANT('DENF')) CALL MINENF
      IF (WANT('DENW')) CALL MINENW
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
      DO IY = 1,20
        IF (WANT('YSMO')) THEN
          IF (LROWS(NLINK('YSMO',IY)).GT.0) CALL MINLIS('YSMO')
        ENDIF
      ENDDO
C
C++   Modifications specific to LEP1 and LEP2.
C
C+++++++++++++++++++++++++++++++++++++++++++++
C
C++   Here starts LEP 2.
C
      IF(IDTYP.EQ.2) THEN
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
           IF (WANT('DEWI')) CALL MINEWI
C
C++      Fill Hcal tower and digital pattern banks.
C
           IF (WANT('DHRL')) CALL MINHRL
           IF (WANT('DPOB')) CALL MINPOB
         ENDIF
C
C++   Copy extra POT/DST banks for all LEP2 events.
C
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
         IF (WANT('PCRL')) THEN
           IF (LROWS(NLINK('PCRL',0)).GT.0) CALL MINLIS('PCRL')
         ENDIF
         IF (WANT('PPOB')) THEN
           IF (LROWS(NLINK('PPOB',0)).GT.0) CALL MINLIS('PPOB')
         ENDIF
         IF (WANT('PPDS')) THEN
           IF (LROWS(NLINK('PPDS',0)).GT.0) CALL MINLIS('PPDS')
         ENDIF
c Eflow
         IF (WANT('PHCO')) THEN
           IF (LROWS(NLINK('PHCO',0)).GT.0) CALL MINLIS('PHCO')
         ENDIF
         IF (WANT('PEWI')) THEN
           IF (LROWS(NLINK('PEWI',0)).GT.0) CALL MINLIS('PEWI')
         ENDIF
         IF (WANT('PWEI')) THEN
           IF (LROWS(NLINK('PWEI',0)).GT.0) CALL MINLIS('PWEI')
         ENDIF
C
C++ If want HTUB, ensure do not pick up HTUB/1.
C
         IF (WANT('HTUB')) THEN
           KHTU1 = NSWAP('HTUB',1,'HTU1',0)
           CALL BLIST(IW,'E+','HTU1')
           IF (LROWS(NLINK('HTUB',0)).GT.0) CALL MINLIS('HTUB')
         ENDIF
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
C+++++++++++++++++++++++++++++++++++++++++++++
C
C++   Here starts LEP 1 or unknown type data.
C
      ELSEIF(IDTYP.LE.1) THEN
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
C
C++   Copy extra POT/DST banks for lepton events.
C
C11/02/98      IF (CLASS(24)) THEN
         IF (CLASS(24).OR.CLASS(5)) THEN
           IF (WANT('ETDI')) THEN
              IF (LROWS(NLINK('ETDI',0)).GT.0) CALL MINLIS('ETDI')
           ENDIF
           IF (WANT('HPDI')) THEN
              IF (LROWS(NLINK('HPDI',0)).GT.0) CALL MINLIS('HPDI')
           ENDIF
C
C++      If want HTUB, ensure do not pick up HTUB/1.
C
           IF (WANT('HTUB')) THEN
             KHTU1 = NSWAP('HTUB',1,'HTU1',0)
             CALL BLIST(IW,'E+','HTU1')
             IF (LROWS(NLINK('HTUB',0)).GT.0) CALL MINLIS('HTUB')
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
           IF (WANT('PEST')) THEN
             IF (LROWS(NLINK('PEST',0)).GT.0) CALL MINLIS('PEST')
           ENDIF
           IF (WANT('PHST')) THEN
             IF (LROWS(NLINK('PHST',0)).GT.0) CALL MINLIS('PHST')
           ENDIF
           IF (WANT('PLSD')) THEN
             IF (LROWS(NLINK('PLSD',0)).GT.0) CALL MINLIS('PLSD')
           ENDIF
         ENDIF
      ENDIF
C
      RETURN
      END
#endif
