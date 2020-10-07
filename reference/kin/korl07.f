      SUBROUTINE ASKUSI(IGCOD)
C--------------------------------------------------------------------
C Initialization                 G. Bonneaud February 1989.
C                                "     "     June     1989.
C (JETSET7.3                     AM. Lutz    February 1994
C  also e+e- s channel final state  B Bloch  May      1996
C  add possible input of vertex offset B Bloch Sept   1998
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / IDPART/ IA1
      COMMON / INOUT / INUT,IOUT
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8)
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
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
      COMMON / VERTX / IFVRT,XVRT(3),SXVRT(3)
      DIMENSION E1(3),E2(3),SDVRT(3)
C
C Generator code (see KINLIB DOC)
C
      PARAMETER ( IGCO = 4007 )
      PARAMETER ( IVER = 107  )
C
      PARAMETER (LPDEC = 48)
      INTEGER NODEC(LPDEC)
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL ,ALRLEP
      CHARACTER TNAM*12
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
C   Return generator code
C
      IGCOD= IGCO
      INUT = IW(5)
      IOUT = IW(6)
      WRITE(IOUT,101) IGCOD ,IVER
 101  FORMAT(/,10X,'KORL07 - CODE NUMBER =',I4,
     &       /,10X,'**************************',
     &       /,10X,' SUBVERSION  :',I10 ,
     &   /,10x,'Last mod = February  6,1999  ')
C
C Input parameters for the generator (see subroutine koralz for comments
C
      ENE    = 45.625
      AMZ    = 91.18
      AMTOP  = 100.
      AMH    = 100.
      AMNUTA = 0.001
      AMNEUT = 0.010
      SINW2  = 0.2293
      GAMM   = 2.484
      KEYGSW = 4
      KEYRAD = 12                   ! warning  new default YFS3.0
      KEYWLB = 1
      ITFIN  = 1
      NNEUT  = 3
      XK0    = 0.01
      VVMIN  = 0.00001
      VVMAX  = 1.00000
      KEYYFS = 1000011
C
C Lund7.3 identifier for electron = -11
C
      KFB =-11
      DO 10 I = 1,3
       E1(I) = 0.
   10  E2(I) = 0.
      JAK1   =  0
      JAK2   =  0
      ISPIN  =  1
      ITDKRC =  1
      XK0DEC =  .001
      GV     =  1.
      GA     = -1.
C
C  The default values can be changed by the DATA CARD GKR7
C
      NAGKOR = NAMIND('GKR7')
      JGENE = IW(NAGKOR)
      IF(JGENE.NE.0) THEN
       AMZ    = RW(JGENE+1)
       AMTOP  = RW(JGENE+2)
       AMH    = RW(JGENE+3)
       AMNUTA = RW(JGENE+4)
       AMNEUT = RW(JGENE+5)
       SINW2  = RW(JGENE+6)
       GAMM   = RW(JGENE+7)
       KEYGSW = IW(JGENE+8)
       KEYRAD = IW(JGENE+9)
       KEYWLB = IW(JGENE+10)
       ITFIN  = IW(JGENE+11)
       NNEUT  = IW(JGENE+12)
       XK0    = RW(JGENE+13)
       VVMIN  = RW(JGENE+14)
       VVMAX  = RW(JGENE+15)
       KEYYFS = IW(JGENE+16)
      ENDIF
C
C  by the DATA CARD GBEA
C
      NAGBEA = NAMIND('GBE7')
      JGBEA = IW(NAGBEA)
      IF(JGBEA.NE.0) THEN
       ENE   = RW(JGBEA+1)
       KFB   = IW(JGBEA+2)
       E1(1) = RW(JGBEA+3)
       E1(2) = RW(JGBEA+4)
       E1(3) = RW(JGBEA+5)
       E2(1) = RW(JGBEA+6)
       E2(2) = RW(JGBEA+7)
       E2(3) = RW(JGBEA+8)
      ENDIF
C
C  by the DATA CARD GTAU
C
      NAGTAU = NAMIND('GTAU')
      JGTAU = IW(NAGTAU)
      IF(JGTAU.NE.0) THEN
       JAK1   = IW(JGTAU+1)
       JAK2   = IW(JGTAU+2)
       ISPIN  = IW(JGTAU+3)
       ITDKRC = IW(JGTAU+4)
       XK0DEC = RW(JGTAU+5)
       GV     = RW(JGTAU+6)
       GA     = RW(JGTAU+7)
      ENDIF
C
C  All the parameters are stored in TABL(I)
C
      vvmax  = 1.0 -(1.8/ENE)**2
      TABL(1)  = AMZ
      TABL(2)  = AMTOP
      TABL(3)  = AMH
      TABL(4)  = AMNUTA
      TABL(5)  = AMNEUT
      TABL(6)  = SINW2
      TABL(7)  = GAMM
      TABL(8)  = KEYGSW
      TABL(9)  = KEYRAD
      TABL(10) = KEYWLB
      TABL(11) = ITFIN
      TABL(12) = NNEUT
      TABL(13) = XK0
      TABL(14) = VVMIN
      TABL(15) = VVMAX
      TABL(34) = KEYYFS
      TABL(16) = ENE
      TABL(17) = KFB
      TABL(18) = E1(1)
      TABL(19) = E1(2)
      TABL(20) = E1(3)
      TABL(21) = E2(1)
      TABL(22) = E2(2)
      TABL(23) = E2(3)
      TABL(24) = JAK1
      TABL(25) = JAK2
      TABL(26) = ISPIN
      TABL(27) = ITDKRC
      TABL(28) = XK0DEC
      TABL(29) = GV
      TABL(30) = GA
C
C  Main vertex initialization
C
      SDVRT(1) = 0.0185
      SDVRT(2) = 0.0008
      SDVRT(3) = 1.02
      NASVRT = NAMIND('SVRT')
      JSVRT = IW(NASVRT)
      IF(JSVRT.NE.0) THEN
       SDVRT(1) = RW(JSVRT+1)
       SDVRT(2) = RW(JSVRT+2)
       SDVRT(3) = RW(JSVRT+3)
      ENDIF
      TABL(31) = SDVRT(1)
      TABL(32) = SDVRT(2)
      TABL(33) = SDVRT(3)
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
       TABL(35) = XVRT(1)
       TABL(36) = XVRT(2)
       TABL(37) = XVRT(3)
       TABL(38) = sXVRT(1)
       TABL(39) = sXVRT(2)
       TABL(40) = sXVRT(3)
C
C  Fill the KPAR bank with the generator parameters
C
      NCOL = 40
      NROW = 1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
c
C  Fill RLEP bank
       IEBEAM = NINT(ENE *1000  )
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
C
C Initialization event counters
C
      DO 20 I = 1,8
       NEVENT(I) = 0
   20 CONTINUE
C
C Initialization particle data
C
      CALL KXL7PA (IPART,IKLIN)
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
       WRITE (IOUT,'(1X,''ASKUSI :error in PART or KLIN bank - STOP - ''
     &                 ,2I3)') IPART,IKLIN
       STOP
      ENDIF
C
CBBL + aml  modify Lund masses according to input masses
      PMAS(LUCOMP(16),1)= AMNUTA
      PMAS(LUCOMP(23),1)= AMZ
      PMAS(LUCOMP(25),1)= AMH
      PMAS(LUCOMP( 6),1)= AMTOP
      PMAS(LUCOMP( 7),1)= 150.
      PMAS(LUCOMP( 8),1)= 300.
      ia1=20213                                  !jetset7.3 code for a1
      PMAS(LUCOMP(ia1),1)= 1.251
      PMAS(LUCOMP(ia1),2)= 0.599
CBBL    aml
c
C   Make sure that masses and width in PART bank are consistent
C function KGPART returns the ALEPH code corresponding to the LUND code
C required.
C Z0(lund code=23) top (lund code=6)  Higgs (lund code=25)
C a1(lund code=20213)
      NAPAR = NAMIND('PART')
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

      IHPART = KGPART(20213)
      IF (IHPART.GT.0)  THEN
        ZMAS = PMAS(LUCOMP(20213),1)
        ZWID = PMAS(LUCOMP(20213),2)
        KPART = KROW(JPART,IHPART)
        RW(KPART+6)=ZMAS
        RW(KPART+9)=ZWID
        IANTI = ITABL(JPART,IHPART,10)
        IF (IANTI.NE.IHPART) THEN
          KAPAR = KROW(JPART,IANTI)
          RW(KAPAR+6)=ZMAS
          RW(KAPAR+9)=ZWID
        ENDIF
      ENDIF
C
C
C   Inhibit decays
C
      MXDEC=KNODEC(NODEC,LPDEC)
      MXDEC=MIN(MXDEC,LPDEC)
      IF (MXDEC.GT.0) THEN
         DO 50 I=1,MXDEC
            IF (NODEC(I).GT.0) THEN
               JIDB = NLINK('MDC1',NODEC(I))
               IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0
            ENDIF
   50    CONTINUE
      ENDIF
C
C  Generator initialization
C
      LENTRY = 1
      CALL KORL04(LENTRY)
c initialize Tau decay modes in case of qqbar final states
      if ( itfin.gt.500) call initdk
C
C    possibly update branching ratios  with card GKBR
C
      NAGKBR = NAMIND('GKBR')
      JGKBR = IW(NAGKBR)
      IF(JGKBR.NE.0) THEN
C check consitency of length
C NCHAN is defined only for Tau and quarks channels, not muons
       IF ( ITFIN.eq.1.or.itfin.gt.500) then
        NLEN = IW(JGKBR)
        IF ( NLEN .NE.NCHAN+4 ) THEN
            WRITE (IW(6),'(1X,'' Inconsistent number of Brs should be'',
     $                    I5,'' is '',I5)') NCHAN,NLEN-4
            CALL EXIT
        ENDIF
        BRA1   = RW(JGKBR+1)
        BRK0   = RW(JGKBR+2)
        BRK0B  = RW(JGKBR+3)
        BRKS   = RW(JGKBR+4)
        DO 51 I = 1,NCHAN
           GAMPRT(I) = RW(JGKBR+4+I)
 51     CONTINUE
        IF ( GAMPRT(1).NE.1.) THEN
         DO 52 I = 1, NCHAN
           GAMPRT(I) = GAMPRT(I)/GAMPRT(1)
 52      CONTINUE
        ENDIF
       ENDIF
      ENDIF
C
C   Store the version used in the job and the branching ratios in
C   header bank  KORL
      NCOL = NCHAN+5
      NROW = 1
      TABL(1) = IVER
      TABL(2) = BRA1
      TABL(3) = BRK0
      TABL(4) = BRK0B
      TABL(5) = BRKS
      DO 57 IBR = 1,NCHAN
          TABL(5+IBR) = GAMPRT(IBR)
 57   CONTINUE
      JKORL = ALTABL('KORL',NCOL,NROW,TABL,'2I,(F)','C')

C
C  Print PART and KPAR banks
C
c     CALL LULIST(12)
c     CALL PRPART
      CALL PRTABL('RLEP',0)
      CALL PRTABL('KPAR',0)
      CALL PRTABL('KORL',0)
C
      RETURN
      END
      SUBROUTINE ASKUSE (IDP,IST,NTRK,NVRT,ECM,WEI)
C --------------------------------------------------------------------
C Generation                     G. Bonneaud August, October 1988.
C                                G. Bonneaud February 1989.
C                                "     "     June     1989.
C --------------------------------------------------------------------
C    mod B.Bloch september 98 : add possible offset to vertex postion
C--------------------------------------------------------------------
C     input     : none
C
C     output    : 6 arguments
C          IDP    : process identification
C          IST    : status flag ( 0 means ok)
C          NTRK   : number of tracks generated and kept
C          NVRT   : number of vertices generated
C          ECM    : center of mass energy for the event
C          WEI    : event weight always equal to 1
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8)
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
      COMMON / TAUHEL / HELT1,HELT2
      REAL*4 HELT1,HELT2
      COMMON / VERTX / IFVRT,XVRT(3),SXVRT(3)
      DIMENSION E1(3),E2(3)
      PARAMETER (LWP = 4)
C
      IST  = 0
      IDP  = 0
      ECM  = 0.
      WEI  = 0.
C
C  Generate primary vertex
C
      CALL RANNOR (RN1,RN2)
      CALL RANNOR (RN3,DUM)
      VRTEX(1) = RN1*TABL(31)
      VRTEX(2) = RN2*TABL(32)
      VRTEX(3) = RN3*TABL(33)
      VRTEX(4) = 0.
      IF ( IFVRT.ge.2) then
         CALL RANNOR(RXX,RYY)
         CALL RANNOR(RZZ,DUM)
         VRTEX(1) = VRTEX(1) + RXX*SXVRT(1)
         VRTEX(2) = VRTEX(2) + RYY*SXVRT(2)
         VRTEX(3) = VRTEX(3) + RZZ*SXVRT(3)
      ENDIF
      IF ( IFVRT.ge.1) then
         VRTEX(1) = VRTEX(1) + XVRT(1)
         VRTEX(2) = VRTEX(2) + XVRT(2)
         VRTEX(3) = VRTEX(3) + XVRT(3)
      ENDIF
C
C  Event generation
C
      LENTRY = 2
      NEVENT(1) = NEVENT(1) + 1
      CALL KORL04(LENTRY)
      IDP  = IDPR
      ECM  = ECMS
      WEI  = WEIT
      IST  = ISTA
      IF(IST.NE.0) THEN
       NEVENT(4) = NEVENT(4) + 1
       GO TO 20
      ENDIF
C  decay remaining pi0's
      CALL LUEXEC
C  Book all banks
C
      CALL KXL7AL(VRTEX,ISTX,NVRT,NTRK)
      IST = ISTX
      IF(IST.NE.0) THEN
       NEVENT(5) = NEVENT(5) + 1
       GO TO 20
      ENDIF
C
C  Now book the polarization bank 'KPOL' if necessary
C
      E1(3) = TABL(20)
      E2(3) = TABL(23)
      ISPIN = TABL(26)
      IF(E1(3).NE.0..OR.E2(3).NE.0..OR.ISPIN.EQ.1) THEN
       NPART = 4
       LE = LMHLEN + NPART*LWP
       CALL AUBOS('KPOL',0,LE,JKPOL,IGARB)
       CALL BLIST(IW,'E+','KPOL')
       CALL BKFMT('KPOL','2I,(I,3F)')
       IF(JKPOL.GT.0) THEN
        IW(JKPOL+LMHCOL) = LWP
        IW(JKPOL+LMHROW) = NPART
        IW(JKPOL+LMHLEN+1) = -1
        RW(JKPOL+LMHLEN+2) = TABL(18)
        RW(JKPOL+LMHLEN+3) = TABL(19)
        RW(JKPOL+LMHLEN+4) = TABL(20)
        IW(JKPOL+LMHLEN+LWP+1) = -2
        RW(JKPOL+LMHLEN+LWP+2) = TABL(21)
        RW(JKPOL+LMHLEN+LWP+3) = TABL(22)
        RW(JKPOL+LMHLEN+LWP+4) = -TABL(23)
        IW(JKPOL+LMHLEN+2*LWP+1) = 1
        RW(JKPOL+LMHLEN+2*LWP+2) = 0.
        RW(JKPOL+LMHLEN+2*LWP+3) = 0.
        RW(JKPOL+LMHLEN+2*LWP+4) = HELT1
        IW(JKPOL+LMHLEN+3*LWP+1) = 2
        RW(JKPOL+LMHLEN+3*LWP+2) = 0.
        RW(JKPOL+LMHLEN+3*LWP+3) = 0.
        RW(JKPOL+LMHLEN+3*LWP+4) = HELT2
       ELSE
        IST = 1
        NEVENT(6) = NEVENT(6) + 1
       ENDIF
      ENDIF
C
C  Event counters
C
      IF(IST.EQ.0) THEN
       NEVENT(2) = NEVENT(2) + 1
       DO 10 IP = 1,N7LU
        IF(K7LU(Ip,2).EQ.22) then
         NEVENT(8) = NEVENT(8) + 1
         GO TO 30
        ENDIF
   10  CONTINUE
       NEVENT(7) = NEVENT(7) + 1
      ENDIF
   20 IF(IST.NE.0) NEVENT(3) = NEVENT(3) + 1
C
   30 RETURN
      END
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C End of generation              G. Bonneaud August, October 1988.
C --------------------------------------------------------------------
      COMMON / INOUT / INUT,IOUT
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8)
C
C End of generation
C
      LENTRY = 3
      CALL KORL04(LENTRY)
C
C Print event counters
C
       WRITE(IOUT,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(IOUT,102) NEVENT(1),NEVENT(2),NEVENT(3),
     &                 NEVENT(7),NEVENT(8)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10,
     &        /5X,'# OF EVENTS WITHOUT PHOTON                 = ',I10,
     &        /5X,'# OF EVENTS WITH PHOTON                    = ',I10)
       WRITE(IOUT,103)
  103  FORMAT(//20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
       WRITE(IOUT,104) NEVENT(4),NEVENT(5),NEVENT(6)
  104  FORMAT(/10X,'ISTA # 0 FROM KORL04        # OF REJECT = ',I10,
     &        /10X,'ISTA # 0 FROM KXLUAL        # OF REJECT = ',I10,
     &        /10X,'ISTA # 0 FROM JKPOL         # OF REJECT = ',I10)
C
      RETURN
      END
