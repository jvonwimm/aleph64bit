      SUBROUTINE MINBLD(NWANT,WLIST,KEEP,CLASS)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Build Mini banks for data.
C
C     Author: Stephen Haywood      06-Jan-93
C     Modify: Agnieszka Jacholkowska   01-Sep-94
C
C     Input  : NWANT  = number of banks listed on MINI card
C              WLIST  = Wish-list of banks to be written on Mini
C              KEEP   = flag indicating whether event should be kept
C              CLASS  = logical array set according to class word
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      CHARACTER*800 WLIST
      CHARACTER*4 NAME
      LOGICAL WANT,CLASS
      DIMENSION CLASS(32)
C
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
C++   Logical function to decide whether a bank is wanted.
C++   An empty list means want all banks.
C
      WANT(NAME) = WLIST.EQ.' ' .OR. INDEX(WLIST(1:4*NWANT),NAME).GT.0
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
C++   Save dE/dx.
C
      IF (WANT('PTEX')) THEN
         IF (LROWS(NLINK('PTEX',0)).GT.0) CALL MINLIS('PTEX')
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
C++   Save muon identification banks.
C
      IF (WANT('MUID')) CALL MINMUI
C
C++   Fill further calorimeter banks - for non-qqbar events.
C
      IF (.NOT. (CLASS(16).OR.CLASS(17)) .OR.
     &  CLASS(5) .OR. CLASS(20) .OR. CLASS(24)) THEN
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
C++   Copy extra POT/DST banks for lepton events.
C
      IF (CLASS(24)) THEN
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
C
      RETURN
      END