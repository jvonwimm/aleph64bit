      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C ------------------------------------------------------------
C -  B. Bloch  - March 1997
C! get an event from ARIADNE 4.08
C  then transfer the information into KINE and VERT banks.
C
C
C     structure : subroutine
C     output arguments :
C          IDPR   : process identification,each digit corresponds to
C          the flavor of the evnt ( several flavors /event is possible)
C          ISTA   : status flag ( 0 means ok), use it to reject
C                   unwanted events
C          NTRK   : number of tracks generated and kept
C                  (i.e. # KINE banks  written)
C          NVRT   : number of vertices generated
C                   (i.e. # VERT banks written)
C          ECMS   : center of mass energy for the event (may be
C                   different from nominal cms energy)
C          WEIT   : event weight ( not 1 if a weighting method is used)
C -----------------------------------------------------------------
      REAL VRTX(4)
      INTEGER KFL(8)
C
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC
C     IFL      : LUND flavour , set to 0 by default, can be changed
C     ECM      : nominal cms energy
C     SVERT    : vertex smearing, set to 0. by default, can be changed
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3
C                                 3= PYTHIA  , 4 = LEPTO
      COMMON / VERTX / IFVRT,XVRT(3),SXVRT(3)
      COMMON / GLUSTA / ICOULU(10)
      DATA IFI / 0/
C initialize photon counter
      NPH=0
C ------------------------------------------------------------------
C
      ISTA = 0
C
C   get an event from the generator
C
      IFI  =  IFI + 1
C - set the process identification IDPR = IFL
      IDPR = IFL
C - set the cms energy for this event ECMS = ECM
      ECMS = ECM
      WEIT = 1.
C - get an event from ARIADNE generator
      IF (IPROC.EQ.1 .OR. IPROC.EQ.2) THEN
        CALL LUEEVT (IFL,ECMS)
      ENDIF
C count number of photons emitted
      DO 200 I=1,N7LU
        IF(K7LU(I,2).EQ.22) NPH=NPH+1
200   CONTINUE
      CALL HFILL(10022,FLOAT(NPH),0.,1.)
C do fragmentation
      IF (IPROC.EQ.1) THEN
         CALL AREXEC
      ELSE IF ( IPROC.EQ.2) THEN
         CALL LUEXEC
      ENDIF
C
C   fill BOS banks
C
C - get the primary vertex
      CALL RANNOR (RX,RY)
      CALL RANNOR (RZ,DUM)
      VRTX(1) = RX*SVERT(1)
      VRTX(2) = RY*SVERT(2)
      VRTX(3) = RZ*SVERT(3)
      VRTX(4) = 0.
      IF ( IFVRT.ge.2) then
         CALL RANNOR(RXX,RYY)
         CALL RANNOR(RZZ,DUM)
         VRTX(1) = VRTX(1) + RXX*SXVRT(1)
         VRTX(2) = VRTX(2) + RYY*SXVRT(2)
         VRTX(3) = VRTX(3) + RZZ*SXVRT(3)
      ENDIF
      IF ( IFVRT.ge.1) then
         VRTX(1) = VRTX(1) + XVRT(1)
         VRTX(2) = VRTX(2) + XVRT(2)
         VRTX(3) = VRTX(3) + XVRT(3)
      ENDIF
      IF (IFI.LE.5) CALL LULIST(1)
      DO 30 ITR = 1 , N7LU
        IMOTH = K7LU(ITR,3)
        IMTYP = IABS(K7LU(IMOTH,2))
        ILUN = K7LU(ITR,2)
C   Final state radiation photon comes from a quark
        IF (ILUN.EQ.22 .AND. IMTYP.GE.1 .AND. IMTYP.LE.6)
     &       CALL HFILL(10023,P7LU(ITR,4),DUM,1.)
C  Main vertex particles
        IF (IMOTH.LE.1.AND. (ILUN.EQ.22))THEN
C    There is a photon
           CALL HFILL(10003,P7LU(ITR,4),DUM,1.)
        ELSEIF (ILUN.GT.0 .AND. ILUN.LE.6.AND.IMTYP.EQ.23) THEN
C   fermion
           CALL HFILL(10001,P7LU(ITR,4),DUM,1.)
           EE=P7LU(ITR,3)/SQRT(P7LU(ITR,1)**2+P7LU(ITR,2)**2+
     $       P7LU(ITR,3)**2)
           CALL HFILL(10004,EE,DUM,1.)
        ELSEIF (ILUN.LT.0 .AND. ILUN.GE.-6.AND.IMTYP.EQ.23)  THEN
C   anti-fermion
           CALL HFILL(10002,P7LU(ITR,4),DUM,1.)
           EE= P7LU(ITR,3)/SQRT(P7LU(ITR,1)**2+P7LU(ITR,2)**2+
     &     P7LU(ITR,3)**2)
           CALL HFILL(10005,EE,DUM,1.)
      ENDIF
  30  CONTINUE
      EE=N7LU
      CALL HFILL(10006,EE,DUM,1.)
C
C      Call the specific routine KXL7AL to fill BOS banks
C      the secondary vertices are propagated
        CALL KXL7AL (VRTX,ISTA,NVRT,NTRK)
C
C   Update IDPR
C
        IDPR = 0
C Look for flavor generated
        DO 5 I=1,8
 5      KFL(I)=0
        NFL=0
        DO 40 I=1,N7LU
           ITYP=ABS(KLU(I,9))
           IF (ITYP.GT.8 .OR. ITYP.EQ.0) GO TO 40
           IF ( NFL.GT.0) THEN
              DO 41 J=1,NFL
              IF (ITYP.EQ.KFL(J)) GO TO 40
  41          CONTINUE
           ENDIF
           NFL=NFL+1
           KFL(NFL)=ITYP
           IDPR=10*IDPR+ITYP
  40    CONTINUE
C
      IF (MSTU(24).NE.0) THEN
        WRITE(6,'(''  ---ERROR LUEXEC AT EVENT #  '',I10)') IFI
        CALL LULIST(1)
        ISTA = -8
      ENDIF
      IF (ISTA.EQ.0 ) THEN
         ICOULU(10) = ICOULU(10)+1
      ELSEIF (ISTA.GT.0) THEN
         ICOULU(1) = ICOULU(1) +1
         ICOULU(9) = ICOULU(9) +1
      ELSEIF ( ISTA.LT.0) THEN
         ICOULU(-ISTA) = ICOULU(-ISTA) +1
         ICOULU(9) = ICOULU(9) +1
      ENDIF
      RETURN
      END
      SUBROUTINE ASKUSI(IGCOD)
C ------------------------------------------------------------------
C - B. Bloch  - September 1990
C! Initialization routine of ARIADNE 4.08  generator
C ------------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC
C     IFL      : LUND flavour , set to 0 by default, can be changed
C     ECM      : nominal cms energy
C     SVERT    : vertex smearing, set to 0. by default, can be changed
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3
C                                 3= PYTHIA  , 4 = LEPTO
      COMMON / VERTX / IFVRT,XVRT(3),SXVRT(3)
      COMMON / GLUSTA / ICOULU(10)
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      PARAMETER (LPDEC=48)
      INTEGER NODEC(LPDEC)
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL,ALRLEP
      DIMENSION TABL(25),MODE(4)
      CHARACTER*6  MODE
      CHARACTER*4  CHAINT,LEPM
C    IGCOD  for ARIADNE 4.08
      PARAMETER ( IGCO  =  5037)
      PARAMETER ( IPMX  =  2   )
      DATA MODE /'JETSET','JETSET','PYTHIA','LEPTO'/
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
C -----------------------------------------------------------------
C - get the LUND flavour IFL if given on data card
         NLUND = NAMIND ('GARI')
         JLUND = IW(NLUND)
         IF (JLUND .NE. 0) THEN
            IFL = IW(JLUND+1)
            ECM  =  RW(JLUND+2)
            IPRT = IW(JLUND+3)
            IPROC= IW(JLUND+4)
            IF ( IPROC.GT.IPMX)THEN
               WRITE (IUT,'(''   ++++++++++ THIS OPTION IS NOT YET IMPLE
     $MENTED.....MAX OPTION IS '',I10)') IPMX
               CALL EXIT
            ENDIF
            LEPM = '    '
            IF (JLUND.GT.4 ) LEPM   = CHAINT(IW(JLUND+5))
         ELSE
            IFL = 0
            ECM = 91.2
            IPRT = 0
            IPROC= 1
         ENDIF
C - make use of a smearing of the vertex
C   if it is given
         NSVER = NAMIND ('SVRT')
         JSVER = IW(NSVER)
         IF (JSVER .NE. 0) THEN
            SVERT(1) = RW(JSVER+1)
            SVERT(2) = RW(JSVER+2)
            SVERT(3) = RW(JSVER+3)
         ELSE
            SVERT(1) = 0.035
            SVERT(2) = 0.0012
            SVERT(3) = 1.28
         ENDIF
C   get an offset for position of interaction point
C   if needed get a smearing on this position
C   XVRT    x      y      z    ( sz    sy    sz)
C
        call vzero(XVRT,3)
        CALL VZERO(SXVRT,3)
        IFVRT = 0
        NAXVRT=NAMIND('XVRT')
        JXVRT=IW(NAXVRT)
        IF (JXVRT.NE.0) THEN
           IFVRT = 1
           XVRT(1)=RW(JXVRT+1)
           XVRT(2)=RW(JXVRT+2)
           XVRT(3)=RW(JXVRT+3)
           IF ( IW(JXVRT).gt.3) then
              IFVRT = 2
              SXVRT(1)=RW(JXVRT+4)
              SXVRT(2)=RW(JXVRT+5)
              SXVRT(3)=RW(JXVRT+6)
           ENDIF
        ENDIF
C
      IUT = IW(6)
C
C   Return generator code IGCOD
C
      IGCOD = IGCO
      WRITE(IUT,101) IGCOD
 101  FORMAT(/,10X,
     &       'ARIA04 - CODE NUMBER =',I4,' Last modification ',
     $ 'September  11,1998'
     & ,/,10X,'****************************************************',//)
C
C   Issue the relevant parameters
C
      WRITE (IUT,1000) IPROC,MODE(IPROC)
      WRITE (IUT,1007)
 1000 FORMAT(1X,88('*'),/,/,10X,
     $ 'WELCOME TO ARIADNE version 4.10,ARIA04',/,1X,'*',
     $ 13X,' Process chosen :',I8 ,' initial state provided by'
     $ ,A10,'   ',10X,'*')
 1007 FORMAT (1X,88('*') )
C
      DO 5  K=1,10
 5    ICOULU(K)=0
C  Set up some default values for masses and initial conditions
      PMAS(LUCOMP(25),1)= 100.
      PMAS(LUCOMP( 6),1)= 100.
      PMAS(LUCOMP(23),1)= 91.2
C   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by
C   a PMA1 card
      IF ( IPROC.NE.2 ) THEN
C    init gene
          msta(7) = iw(6)
          msta(8) = iw(6)
          msta(3) = 0
          CALL ARINIT(MODE(IPROC))
          IF ( LEPM.EQ.'ALEP') CALL ARTUNE('ALEPH')
          IF ( LEPM.EQ.'DELP') CALL ARTUNE('DELPHI')
          IF ( LEPM.EQ.'OPAL') CALL ARTUNE('OPAL')
C    init user's ARIADNE
          CALL KXARCO(LAPAR)
      ENDIF
C    init user's JETSET
C -- complete PART bank with LUND  particles
C    use the library routine KXL74A
      CALL KXL74A (IPART,IKLIN)
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
         WRITE (IW(6),
     &     '(1X,''error in PART or KLIN bank - STOP - '' ,2I3)')
         CALL EXIT
      ENDIF
C   Make sure that masses in PART bank are consistent
      NAPAR = NAMIND('PART')
C This is the aleph number of the Z0(lund code=23),top (6) and Higgs(25)
C function KGPART returns the ALEPH code corresponding to the LUND code
C required.
      JPART = IW(NAPAR)
      IZPART = KGPART(23)
      IF (IZPART.GT.0)  THEN
        ZMAS = PMAS(LUCOMP(23),1)
        KPART = KROW(JPART,IZPART)
        RW(KPART+6)=ZMAS
        IANTI = ITABL(JPART,IZPART,10)
        IF (IANTI.NE.IZPART) THEN
          KAPAR = KROW(JPART,IANTI)
          RW(KAPAR+6)=ZMAS
        ENDIF
      ENDIF
      ITPART = KGPART(6)
      IF (ITPART.GT.0)  THEN
        ZMAS = PMAS(LUCOMP( 6),1)
        KPART = KROW(JPART,ITPART)
        RW(KPART+6)=ZMAS
        IANTI = ITABL(JPART,ITPART,10)
        IF (IANTI.NE.ITPART) THEN
          KAPAR = KROW(JPART,IANTI)
          RW(KAPAR+6)=ZMAS
        ENDIF
      ENDIF
      IHPART = KGPART(25)
      IF (IHPART.GT.0)  THEN
        ZMAS = PMAS(LUCOMP(25),1)
        KPART = KROW(JPART,IHPART)
        RW(KPART+6)=ZMAS
        IANTI = ITABL(JPART,IHPART,10)
        IF (IANTI.NE.IHPART) THEN
          KAPAR = KROW(JPART,IANTI)
          RW(KAPAR+6)=ZMAS
        ENDIF
      ENDIF
C
C - Print PART and KLIN banks
      IF (IPRT.GT.0) CALL PRPART
C
C -- get list of  particle# which should not be decayed
C    in LUND  because they are decayed in GALEPH.
C    the routines uses the KLIN bank and fills the user array
C    NODEC in the range [1-LPDEC]
      MXDEC = KNODEC (NODEC,LPDEC)
      MXDEC = MIN (MXDEC,LPDEC)
C
C -- inhibit decays in LUND
C    If the user has set some decay channels by data cards they will
C    will not be overwritten
      IF (MXDEC .GT. 0) THEN
         DO 10 I=1,MXDEC
            IF (NODEC(I).GT.0) THEN
               JIDB = NLINK('MDC1',NODEC(I))
               IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0
            ENDIF
   10    CONTINUE
      ENDIF
C
      CALL HBOOK1(10001,'OUTGOING FERMION ENERGY$',50,0.,50.,0.)
      CALL HBOOK1(10002,'OUTGOING ANTIFERMION ENERGY$',50,0.,50.,0.)
      CALL HBOOK1(10003,'ISR PHOTON ENERGY IF ANY$',50,0.,20.,0.)
      CALL HBOOK1(10004,'POLAR ANGLE FERMION$',50,-1.,1.,0.)
      CALL HBOOK1(10005,'POLAR ANGLE ANTIFERMION$',50,-1.,1.,0.)
      CALL HBOOK1(10006,'FINAL MULTIPLICITY GENERATED$',50,20.,120.,0.)
      CALL HBOOK1(10023,'FSR PHOTON ENERGIES IF ANY      ',50,0.,20.,0.)
      CALL HBOOK1(10022,'# OF PHOTON PER EVENT    ',50,0.,50.,0.)
C
C   dump the generator parameters for this run in a bank
C assume all parameters are real and stored as a single row
      TABL(1) = FLOAT(IFL)
      TABL(2) = ECM
      TABL(3) = FLOAT(IPROC)
      DO 11 I=1,3
 11   TABL(3+I) = SVERT(I)
      TABL(7) = FLOAT(INTCHA(LEPM))
      TABL(8) = XVRT(1)
      TABL(9) = XVRT(2)
      TABL(10) = XVRT(3)
      TABL(11) = sXVRT(1)
      TABL(12) = sXVRT(2)
      TABL(13) = sXVRT(3)
      NWB = 13
      IND = ALTABL('KPAR',NWB,1,TABL,'2I,(F)','C')
C
C  Fill RLEP bank
      IEBEAM = NINT(ECM* 500  )
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
      CALL PRTABL('RLEP',0)
      CALL PRTABL('KPAR',0)
      IF ( IPROC.NE.2) CALL ARPRDA
      RETURN
      END
      SUBROUTINE USCJOB
C-------------------------------------------------------------------
C! End of job routine    ARIADNE 4.08
C
C   To be filled by user to print any relevant info
C
C------------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC
C     IFL      : LUND flavour , set to 0 by default, can be changed
C     ECM      : nominal cms energy
C     SVERT    : vertex smearing, set to 0. by default, can be changed
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3
C                                 3= PYTHIA  , 4 = LEPTO
      COMMON / VERTX / IFVRT,XVRT(3),SXVRT(3)
      COMMON / GLUSTA / ICOULU(10)
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
C.......................................................................
       IUT=IW(6)
       WRITE(IUT,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)
       WRITE(IUT,103)
  103  FORMAT(//20X,'REJECT STATISTICS',
     &         /20X,'*****************')
       WRITE(IUT,104) (ICOULU(I),I=1,8)
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part    # OF REJECT =',I10,
     &        /10X,'IR= 2 BOS  ERROR KINE/VERT       # OF REJECT =',I10,
     &        /10X,'IR= 3 LUND ERROR too many tracks # OF REJECT =',I10,
     &        /10X,'IR= 4 LUND ERROR Beam wrong pos  # OF REJECT =',I10,
     &        /10X,'IR= 5 LUND ERROR status code >5  # OF REJECT =',I10,
     &        /10X,'IR= 6 free for user              # OF REJECT =',I10,
     &        /10X,'IR= 7 free for user              # OF REJECT =',I10,
     &        /10X,'IR= 8 LUND error in LUEXEC       # OF REJECT =',I10)
        WRITE (IUT,105) (PARJ(II),II=141,148)
 105    FORMAT(//,20X,'FINAL RESULTS FROM LUND GENERATION',
     $          /,20X,'**********************************',
     $          /,10X,'R value as given in massless QED   :',F10.5,
     $          /,10X,'R value including weak effects     :',F10.5,
     $          /,10X,'R value including QCD corrections  :',F10.5,
     $          /,10X,'R value including I.S.R. effects   :',F10.5,
     $          /,10X,'Absolute cross sections in nb      :',
     $          /,10X,'As given  in massless QED   :',F10.5,
     $          /,10X,'Including  weak effects     :',F10.5,
     $          /,10X,'Including  QCD corrections  :',F10.5,
     $          /,10X,'Including  I.S.R. effects   :',F10.5)
      RETURN
      END
      SUBROUTINE KXARCO(LAPAR)
C-------------------------------------------------------------------
C
C - Modified for Ariadne 4.01       B.Bloch   - 920330
C
C! Set ARIADNE parameters by data cards
CKEY KINE KINGAL   /  USER INTERNAL
C  Every ARIADNE parameter is a BOS data card keyword,the index of the
C  parameter is the bank number.
C
C  the list of keywords with their format is given below:
C
C 'MSTA'(I),'PARA'(F)
C
C
C    KEY  i  /  ival     ====>  KEY(i)=ival
C    RKEY i  /  value    ====>  RKEY(i)=value
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXARCO
C              External References: NAMIND/BKFMT/BLIST(BOS77)
C              Comdecks referenced: BCS,ARDAT1
C
C - usage    : CALL KXARCO(LUPAR)
C - output  : LUPAR=No. of read data cards
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
C
      PARAMETER (LKEYS=2)
      CHARACTER*4 KEY(LKEYS),CHAINT
      CHARACTER*1 FMT(LKEYS)
      DATA KEY / 'MSTA','PARA'/
      DATA FMT /'I','F'/
      LUPAR=0
      DO 50 I=1,LKEYS
         NAMI=NAMIND(KEY(I))
         IF (IW(NAMI).EQ.0) GOTO 50
         KIND=NAMI+1
   15    KIND=IW(KIND-1)
         IF (KIND.EQ.0) GOTO 49
         LUPAR = LUPAR+1
         J = IW(KIND-2)
         GOTO (21,22 ) I
   21    MSTA(J) = IW(KIND+1)
       GOTO 15
   22    PARA(J) = RW(KIND+1)
       GOTO 15
   49    CONTINUE
         CALL BKFMT (KEY(I),FMT(I))
         CALL BLIST (IW,'C+',KEY(I))
   50 CONTINUE
      RETURN
      END
      SUBROUTINE USKRIN(EI)
C
C! Reinitialise the generator with energy  EI and reinit some quantities
C! ARIA02 version  B.Bloch 30 MARCH 1992
C
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC
C     IFL      : LUND flavour , set to 0 by default, can be changed
C     ECM      : nominal cms energy
C     SVERT    : vertex smearing, set to 0. by default, can be changed
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3
C                                 3= PYTHIA  , 4 = LEPTO
      COMMON / VERTX / IFVRT,XVRT(3),SXVRT(3)
      COMMON / GLUSTA / ICOULU(10)
      DIMENSION TABL(25)
      INTEGER ALTABL
      EXTERNAL ALTABL
      ECM = EI
C Get kpar bank, modify
      IKPAR= NLINK('KPAR',0)
      IF (IKPAR.GT.0) THEN
         NCO = IW(IKPAR+LMHCOL)
         CALL UCOPY(RW(IKPAR+LMHLEN+1),TABL(1),NCO)
         CALL BDROP(IW ,'KPAR')
         TABL(2) = EI
         IND = ALTABL('KPAR',NCO,1,TABL,'2I,(F)','C')
         CALL PRTABL('KPAR',0)
      ENDIF
      CALL VZERO(ICOULU,10)
      RETURN
      END
      FUNCTION XKSECT(ECMI)
C
C! Returns the cross section value in nb for energy ECM
C! ARIA02 version  B.Bloch 30 MARCH 1992
C
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC
C     IFL      : LUND flavour , set to 0 by default, can be changed
C     ECM      : nominal cms energy
C     SVERT    : vertex smearing, set to 0. by default, can be changed
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3
C                                 3= PYTHIA  , 4 = LEPTO
      COMMON / VERTX / IFVRT,XVRT(3),SXVRT(3)
      COMMON / GLUSTA / ICOULU(10)
      CALL LUXTOT(IFL,ECMI,XTOT)
      XKSECT = XTOT
      ECM = ECMI
      RETURN
      END
