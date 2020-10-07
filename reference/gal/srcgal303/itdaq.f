      SUBROUTINE ITDAQ
C.
C...ITDAQ  2.10  890413  13:11                         R.Beuselinck.
C.
C!  Simulate ITC readout.  Produce the raw data bank and trigger bits.
C.
C.  Called by: ITDIGI                                from this .HLB
C.      Calls: WBANK, WDROP, BLIST                   from BOS77
C.             ITSDIG, ITRPP                         from this .HLB
C.             ALBOS                                 from ALEPHLIB
C.
C.  Work banks used:
C.   JDITNW, JDITAB, JDITDC, JDIZSC          - created and dropped
C.                                             in this routine.
C.   JDITFN  - read only.
C.
C.  Named banks used:
C.   IDIG - raw data (DST bank).
C.   IDHR - correlation bank. Digits to Hits Relation.
C.   ITTR - level 1 trigger bank.
C.   IXRP - ITC R-Phi mask bits for level 3.
C.
C-----------------------------------------------------------------------
      SAVE
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LIHIT=8, MIHIT=500, LITWP=3)
      PARAMETER (LITFN=6, LITFP=2)
      COMMON /ITNAMC/NAIHIT,NAIDIG,NAITTR,NAIDHR,NAIXBW,
     + JDITWP,JDITFN,JDITFP,JDITNW,JDITAB,JDITDC,JDIZSC
C
      PARAMETER (LITWBK = 7)
      INTEGER JDITWB(LITWBK)
      EQUIVALENCE (JDITWB(1),JDITWP)
C
      COMMON/ITSUMC/NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,DIGHIT,
     +      NTSMIT(10),NHSMIT(10),NDSMIT(10),NDHSIT(10)
      INTEGER NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,
     +      NTSMIT,NHSMIT,NDSMIT,NDHSIT
      REAL DIGHIT
C
C
      REAL TAB(2),TSTRT,TSTOP
      EQUIVALENCE (TSTRT,TAB(1)),(TSTOP,TAB(2))
      INTEGER IDAB(2),IRPBT(3),IZPBT(3),IRPHM(6),ISPBT
      LOGICAL LAST
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
C -----------------------------------------------------------
C
C - Set index of local working banks to 0
      JDITNW = 0
      JDITAB = 0
      JDITDC = 0
      JDIZSC = 0
C
C--  Access the front end signals bank.
C--
      IF (JDITFN.EQ.0) GO TO 999
      LWFN = LCOLS(JDITFN)
      LNFN = LROWS(JDITFN)
C
C--  Create work banks for digitising.  Each bank has one word per wire.
C--  Preset JDITNW bank to -layer number for each wire.
C--  Maximum number of entries required for most work banks is LNFN/2.
C--
C--   JDITNW is set to +layer no. if wire is hit.
      CALL WBANK(IW,JDITNW,960,*998)
      ND = 3*(LNFN/2)
      CALL WBANK(IW,JDITAB,ND,*998)
      ND = LNFN/2
C--   JDITDC stores TDC count. JDIZSC stores Z-scalar.
      CALL WBANK(IW,JDITDC,ND,*998)
      CALL WBANK(IW,JDIZSC,ND,*998)
      DO 10 I=1,4
        II = (I-1)*96
        CALL VFILL(IW(JDITNW+II+1),96,-I)
   10 CONTINUE
      DO 20 I=5,8
        II = (I-5)*144 + 4*96
        CALL VFILL(IW(JDITNW+II+1),144,-I)
   20 CONTINUE
C
C--  Loop over the signals for each hit wire in turn and find the first
C--  pulses to arrive at each end
C--
      TSTRT = 9999.
      TSTOP = 9999.
      NWIRS = 0
      DO 50 I=1,LNFN
        II = KROW(JDITFN,I)
        IDH  = IW(II+1)
        IWIR = IW(II+2)
        IEND = IW(II+3)
        TFE  = RW(II+4)
        LAST = (I.EQ.LNFN).OR.(IWIR.NE.IW(II+2+LWFN))
        IF (IEND.LT.0) IEND = 2
        IF (TFE.LT.TAB(IEND)) THEN
          TAB(IEND) = TFE
          IDAB(IEND) = IDH
        ENDIF
        IF (LAST) THEN
C
C--       Store data for digitisings and trigger.
C--       Flip the sign of JDITNW entries if wire is hit.
C--
          CALL ITSDIG(NWIRS,IWIR,TAB,IDAB)
          TSTRT = 9999.
          TSTOP = 9999.
        ENDIF
   50 CONTINUE
      IF (NWIRS.GT.LNFN/2) THEN
        WRITE(LOUTIO,'(//A,2I6)')
     +  ' *** FATAL ERROR IN ITDAQ. NWIRS,LNFN=',NWIRS,LNFN
        CALL ALTELL('ITDAQ ',5,'NEXT')
        GOTO 999
      ENDIF
C
C--  Create raw data bank.
C--
      CALL ALBOS('IDIG',0,NWIRS+LMHLEN,JIDIG,IGARB)
      IW(JIDIG+LMHCOL) = 1
      IW(JIDIG+LMHROW) = NWIRS
      NDCUIT = NWIRS
C
C--  Fill data part from work banks JDITNW, JDITDC, JDIZSC.
C--
      NHIT = 0
      DO 100 I=1,960
        IF (IW(JDITNW+I).LT.0) GO TO 100
        NHIT = NHIT + 1
        II = JIDIG + LMHLEN + NHIT
        IW(II) = I
        IRD = IW(JDITDC+NHIT)
        IZD = IW(JDIZSC+NHIT)
        CALL MVBITS(IRD,0,9,IW(II),10)
        CALL MVBITS(IZD,1,9,IW(II),19)
        IF (IRD.EQ.0) IW(II) = IBSET(IW(II),28)
        IF (IZD.EQ.0) IW(II) = IBSET(IW(II),29)
  100 CONTINUE
C
C--  Simulate trigger processors.
C--
      CALL ITRPP(IW(JDITNW+1),IW(JDITAB+1),IRPBT,IZPBT,ISPBT,IRPHM)
C
C--  Create the trigger bank.  Fill the 72 correlation bits from either
C--  the R-phi basic trigger or from the R-phi-Z trigger (default)
C--  according to the ITC run flag 3.
C--  Fill the 24 'special' trigger bits.
C--
      NRL1 = 1
      NDL1 = 4
      CALL ALBOS('ITTR',NRL1,NDL1+LMHLEN,ITTR,IGARB)
      IW(ITTR+LMHCOL) = NDL1
      IW(ITTR+LMHROW) = 1
C
      IF (ICITJO(3).EQ.0) THEN
        IW(ITTR+3) = IZPBT(1)
        IW(ITTR+4) = IZPBT(2)
        IW(ITTR+5) = IZPBT(3)
      ELSE IF (ICITJO(3).EQ.1) THEN
        IW(ITTR+3) = IRPBT(1)
        IW(ITTR+4) = IRPBT(2)
        IW(ITTR+5) = IRPBT(3)
      ENDIF
      IW(ITTR+6) = ISPBT
C
C--  Fill the IXRP bank with the results of the R-phi trigger masks.
C--  This information is wanted by the level 3.
C--
      NDX = 6
      CALL ALBOS('IXRP',0,NDX+LMHLEN,IXRP,IGARB)
      IW(IXRP+LMHCOL) = NDX
      IW(IXRP+LMHROW) = 1
      DO 150 I=1,NDX
        IW(IXRP+LMHLEN+I) = IRPHM(I)
  150 CONTINUE
C
C--  Fill the digit => hits correlations.
C--
      CALL ALBOS('IDHR',0,2*NWIRS+LMHLEN,JIDHR,IGARB)
      IW(JIDHR+LMHCOL) = 2
      IW(JIDHR+LMHROW) = NWIRS
C
      DO 200 I=1,NWIRS
        II = JDITAB + (I-1)*3
        KKDH = KROW(JIDHR,I)
        IW(KKDH+1) = IW(II+2)
        IW(KKDH+2) = IW(II+3)
  200 CONTINUE
C
C--  Put output banks on the event list and delete work banks.
C--
      CALL BLIST(IW,'E+','IDIGIDHRITTRIXRP')
      IW(1) = LITWBK
      CALL WDROP(IW,JDITWB)
      GOTO 999
C
C--  Handle lack of BOS space.
C--
  998 IW(1) = LITWBK
      CALL WDROP(IW,JDITWB)
      CALL ALTELL('ITDAQ: not enough space for ITxx banks ',1,'NEXT')
C
  999 CONTINUE
      END
