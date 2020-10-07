      SUBROUTINE ASKUSI(IGCOD)
C--------------------------------------------------------------------
C Initialization                 B. Bloch-Devaux June 1992.
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
      PARAMETER (L2PAR=500, L2PARF=2000)
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ NparLU,KodeLU(LJNPAR,5),PartLU(LJNPAR,5),
     &                V7LU(LJNPAR,5)
C
      DIMENSION E1(3),E2(3),SDVRT(3)
      PARAMETER (LPDEC = 48)
C
C Generator code (see KINLIB DOC)
C
      PARAMETER ( IGCO = 4009 )
C
      INTEGER NODEC(LPDEC)
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL ,ALRLEP
      CHARACTER TNAM*12
C
C   Return generator code
C
      IGCOD= IGCO
      INUT = IW(5)
      IOUT = IW(6)
      WRITE(IOUT,101) IGCOD
 101  FORMAT(/,10X,'KORB01 - CODE NUMBER =',I4,
     &       /,10X,'**************************',
     &   /,10x,'Last mod = December 9  , 1996  ')
C
C Input parameters for the generator (see subroutine koralb for comments
C
      AMZ    = 91.17
      AMTOP  = 100.
      AMH    = 100.
      AMNUTA = 0.010
      AMNEUT = 0.010
      SINW2  = 0.232
      GAMM   = 2.5
      KEYGSW = 0
      KEYRAD = 0
      ITRANS = 1
      ITFIN  = 1
C     NNEUT  = 3
      XK0    = 0.01
      VVMIN  = 0.00001
      VVMAX  = 1.
      ENE    = 45.6
C
C Lund identifier for electron = 7
C
      KFB =  7
      DO 10 I = 1,3
       E1(I) = 0.
   10  E2(I) = 0.
      JAK1   =  0
      JAK2   =  0
      ISPIN  =  1
      ITDKRC =  0
      XK0DEC =  .001
      GV     =  1.
      GA     = -1.
C
C  The default values can be changed by the DATA CARD GKOB
C
      JGENE = NLINK('GKOB',0)
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
       ITRANS = IW(JGENE+10)
       ITFIN  = IW(JGENE+11)
C      NNEUT  = IW(JGENE+12)
       XK0    = RW(JGENE+13)
       VVMIN  = RW(JGENE+14)
       VVMAX  = RW(JGENE+15)
      ENDIF
C
C  by the DATA CARD GBEA
C
      JGBEA = NLINK('GBEA',0)
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
      JGTAU = NLINK('GTAU',0)
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
      TABL(1)  = AMZ
C     TABL(2)  = AMTOP
      TABL(3)  = GAMM
      TABL(4)  = AMNUTA
      TABL(5)  = AMNEUT
      TABL(6)  = SINW2
C     TABL(7)  = GAMM
      TABL(8)  = KEYGSW
      TABL(9)  = KEYRAD
      TABL(10) = ITRANS
      TABL(11) = ITFIN
C     TABL(12) = NNEUT
      TABL(13) = XK0
      TABL(14) = VVMIN
      TABL(15) = VVMAX
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
      JSVRT = NLINK('SVRT',0)
      IF(JSVRT.NE.0) THEN
       SDVRT(1) = RW(JSVRT+1)
       SDVRT(2) = RW(JSVRT+2)
       SDVRT(3) = RW(JSVRT+3)
      ENDIF
      TABL(31) = SDVRT(1)
      TABL(32) = SDVRT(2)
      TABL(33) = SDVRT(3)
C
C  Fill the KPAR bank with the generator parameters
C
      NCOL = 33
      NROW = 1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
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
C   If tau generation then define A1 and anti-A1 for Lund ....
C   first we look at a free place to book it in the range 91-100
C
      IA1 = 20213
C
C   then, we book the mass, charge and name in the following way :
C   mass and charge in /LUDAT2/, name in /LUDAT4/
C
      PMAS(LUCOMP(ia1),1)= 1.251
      PMAS(LUCOMP(ia1),2)= 0.599
CBBL modify Lund masses according to input masses
      PMAS(LUCOMP( 6),1) = AMTOP
      PMAS(LUCOMP(25),1) = AMH
      PMAS(LUCOMP(23),1) = AMZ
      PMAS(LUCOMP(16),1) = AMNUTA
      PMAS(LUCOMP( 7),1) = 150.
      PMAS(LUCOMP( 8),1) = 300.
CBBL
      CALL KXL74A (IPART,IKLIN)
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
       WRITE (IOUT,'(1X,''ASKUSI :error in PART or KLIN bank - STOP - ''
     &                 ,2I3)') IPART,IKLIN
       STOP
      ENDIF
CBBL Z0 mass must be updated directly in PART bank (Z0 IS #2 FOR LUND)
      NAPAR = NAMIND('PART')
      JPART = IW(NAPAR)
      IZ0 = KGPART(2)
      IF (JPART.NE.0 .AND. IZ0.GT.0)
     $    RW(JPART+LMHLEN+(IZ0-1)*IW(JPART+LMHCOL)+6) = AMZ
CBBL
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
C   Call generator for initialization
C
      LENTRY = 1
      CALL KORB01(LENTRY)
C
C  Print PART and KPAR banks
C
      CALL PRPART
      CALL PRTABL('RLEP',0)
      CALL PRTABL('KPAR',0)
C
 60   RETURN
      END
      SUBROUTINE ASKUSE (IDP,IST,NTRK,NVRT,ECM,WEI)
C --------------------------------------------------------------------
C Generation                     G. Bonneaud August, October 1988.
C                                G. Bonneaud February 1989.
C                                "     "     June     1989.
C --------------------------------------------------------------------
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
      PARAMETER (L2PAR=500, L2PARF=2000)
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ NparLU,KodeLU(LJNPAR,5),PartLU(LJNPAR,5),
     &                V7LU(LJNPAR,5)
C
      COMMON / TAUHEL / HELT1,HELT2
      REAL*4 HELT1,HELT2
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
C
C  Event generation
C
      LENTRY = 2
      NEVENT(1) = NEVENT(1) + 1
      CALL KORB01(LENTRY)
      IDP  = IDPR
      ECM  = ECMS
      WEI  = WEIT
      IST  = ISTA
      IF(IST.NE.0) THEN
       NEVENT(4) = NEVENT(4) + 1
       GO TO 20
      ENDIF
C   Decay pi0's
      CALL LUEXEC
C  Book all banks
C
      CALL KXL7AL(VRTEX,ISTA,NVRT,NTRK)
      IST = ISTA
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
       DO 10 IP = 3,NPARLU
        IF(KODELU(IP,1).EQ.0.AND.KODELU(IP,2).EQ.1) THEN
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
      CALL KORB01(LENTRY)
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
  104  FORMAT(/10X,'ISTA # 0 FROM KORALB        # OF REJECT = ',I10,
     &        /10X,'ISTA # 0 FROM KXL7AL        # OF REJECT = ',I10,
     &        /10X,'ISTA # 0 FROM JKPOL         # OF REJECT = ',I10)
C
      RETURN
      END
      SUBROUTINE KORB01(LENTRY)
C --------------------------------------------------------------------
C Koralb                         B.Bloch-Devaux June  1992.
C --------------------------------------------------------------------
C
C     MONTE CARLO EVENT GENERATOR FOR THE PROCESSES
C
C         E+(PB1) E-(PB2)   ---->  TAU+(QP) TAU-(QM)
C
C     AND
C
C         E+(PB1) E-(PB2)   ---->  TAU+(QP) TAU-(QM) PHOTON(PH)
C
C
C  THE INPUT QUANTITIES ARE
C ENE    ENERGY OF A BEAM (GEV)
C AMZ    Z0    MASS (GEV)
C AMTOP  TOP   MASS (GEV)
C AMH    HIGGS MASS (GEV)
C AMNUTA NEUTRINO TAU MASS (GEV)
C E1     =  SPIN POLARIZATION VECTOR FOR THE FIRST BEAM.
C E2     =  SPIN POLARIZATION VECTOR FOR THE SECOND BEAM,
C           BOTH IN THE CORRESPONDING BEAM PARTICLE REST FRAME
C           AND IN BOTH CASES THIRD AXIS DIRECTED ALONG FIRST BEAM,
C           I.E. EE1(3) AND -EE2(3) ARE HELICITIES.
C ISPIN  =  0,1  SPIN EFFECTS IN DECAY SWITCHED OFF,ON.
C INRAN  =  INITIALISATION CONSTANT FOR RAND. NUM. GEN. RNF100, POSITIVE
C KEYGSW,   IMPLEMENTATION LEVEL OF GLASHOW-SALAM-WEINBERG MODEL:
C        =  0,  N0 Z0, ONLY PHOTON EXCHANGE, NO Z0, NO VAC. POL.,
C        =  1,  PHOTON AND Z0, NO VACUUM POLARISATIONS,
C        =  2,  PHOTON AND Z0, GSW VACUUM POLARISATIONS INCLUDED,
C        =  3,  ALL GSW CORRECTIONS INCLUDED
C KEYRAD =  0,  NO QED BREMSSTRAHLUNG,
C        =  1,  WITH QED BREMSSTRAHLUNG.
C JAK1,JAK2, DECAY TYPE FOR TAU+ AND TAU-.
C            DECAY MODES INCLUDED ARE:
C            JAK  =  1  ELECTRON DECAY
C                 =  2  MU  DECAY,
C                 =  3  PI DECAY ,
C                 =  4  RHO DECAY,
C                 =  5  A1  DECAY,
C                 =  0  INCLUSIVE:  JAK=1,2,3,4,5
C                 = -1  NO DECAY.
C ITFIN  =  1  TAU PAIR PRODUCTION,
C        =  2  MUON PAIR PRODUCTION.
C KFB = 7,-7 FLAVOUR CODE OF FIRST BEAM, KFB=7  FOR ELECTRON.
C ITDKRC=0 DECAY OF TAU USING TAUOLA,
C       >0 RESERVED FOR FUTURE DEVELOPEMENT.
C GV AND GA ARE COUPLING CONSTANTS OF W-BOSON TO TAU LEPTON,
C       GV=1,GA=-1 REPRESENT THE STANDARD V-A COUPLING.
C
C --------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / INOUT / INUT,IOUT
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8)
C
      DIMENSION PB1(4),PB2(4),XPAR(40)
      DIMENSION E1(3),E2(3)
      INTEGER NPAR(40)
      INTEGER ALTABL
      EXTERNAL ALTABL
C
C
C  INITIALIZATION            *********************
C
      IF(LENTRY.EQ.1) THEN
       NADEB = NAMIND('DEBU')
       JDEBU = IW(NADEB)
       IF(JDEBU.NE.0) THEN
        IDB1 = IW(JDEBU+1)
        IDB2 = IW(JDEBU+2)
       ENDIF
C
C Initialization of XPAR and NPAR tables : generator's parameters
C
       NPAR(1)  = TABL(26)
       NPAR(2)  = 0.
       NPAR(3)  = TABL(8)
       NPAR(4)  = TABL(9)
       NPAR(5)  = TABL(24)
       NPAR(6)  = TABL(25)
       NPAR(7)  = TABL(11)
       NPAR(8)  = TABL(27)
       NPAR(9)  = TABL(10)
C      NPAR(11) = TABL(12)
       XPAR(1)  = TABL(1)
       XPAR(2)  = TABL(3)
C      XPAR(3)  = TABL(2)
       XPAR(4)  = TABL(29)
       XPAR(5)  = TABL(30)
       XPAR(6)  = TABL(6)
C      XPAR(7)  = TABL(7)
       XPAR(8)  = TABL(4)
       XPAR(9)  = TABL(5)
       XPAR(11) = TABL(13)
       XPAR(12) = TABL(14)
       XPAR(13) = TABL(15)
       XPAR(14) = TABL(5)
       DO 1 I = 1,3
        E1(I) = TABL(17+I)
    1   E2(I) = TABL(20+I)
       PB1(1)  = 0.
       PB1(2)  = 0.
       PB1(3)  = TABL(16)
       PB1(4)  = TABL(16)
       DO 2 I=1,3
    2  PB2(I) = -PB1(I)
       PB2(4) =  PB1(4)
       KFB    =  TABL(17)
C
C KORALB initialization step
C
       NMODE  = -1
       CALL KORALB(NMODE,KFB,PB1,E1,-KFB,PB2,E2,XPAR,NPAR)
C
C Booking histos
C
       ITFIN = NPAR(7)
       CALL BUKERD(NMODE,ITFIN)
C
       RETURN
      ENDIF
C
C  EVENT GENERATION          *********************
C
      IF(LENTRY.EQ.2) THEN
C
C Event generation
C
       NMODE = 0
       CALL KORALB(NMODE,KFB,PB1,E1,-KFB,PB2,E2,XPAR,NPAR)
C
       ECMS = 2.*TABL(16)
       WEIT = 1.
       ISTA = 0
C
C Update process code
C
       IF(ITFIN.EQ.1) THEN
        ID1 = JAKP
        ID2 = JAKM
cam warning !!! KKEVID to be decoded differently in ALPHA !!!
        IDPR = 100*ID1+ID2
        XPR = FLOAT(IDPR)
        CALL HFILL(10000,FLOAT(ID1)+.1,FLOAT(ID2)+.1,1.)
       ELSE
        IDPR= ITFIN
       ENDIF
C
C Print first events depending of DEBUG option
C
       IF(NEVENT(1).GE.IDB1.AND.NEVENT(1).LE.IDB2) THEN
C       CALL DUMPL8
        CALL LULIST(1)
       ENDIF
C
C Fill histos
C
       CALL BUKERD(NMODE,ITFIN)
C
       RETURN
      ENDIF
C
C  END OF GENERATION         *********************
C
      IF(LENTRY.EQ.3) THEN
C
C Generator end
C
       NMODE = 1
       NPAR1 = 0
       NPAR2 = 0
       CALL KORALB(NMODE,NPAR1,PB1,E1,NPAR2,PB2,E2,XPAR,NPAR)
C
C Print histos
C
       CALL BUKERD(NMODE,ITFIN)
      ENDIF
C
      RETURN
      END
      SUBROUTINE BUKERD(IMOD,ITFIN)
C --------------------------------------------------------------------
C Book histos                    B.Bloch-Devaux  June 1992
C --------------------------------------------------------------------
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000)
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ NparLU,KodeLU(LJNPAR,5),PartLU(LJNPAR,5),
     &                V7LU(LJNPAR,5)
C
      COMMON / UTIL4 / AQP(4),AQM(4),APH(4)
      REAL*4           AQP   ,AQM   ,APH
      PARAMETER ( IDFF=15)
C
C     ====================
C
      IF(IMOD.EQ.-1) THEN
C
      IF(ITFIN.EQ.2) THEN
       CALL HTITLE('MU+ MU- GAMMA(s) FINAL STATE')
       CALL HBOOK1(10001,'MU+ energy distribution $',50,0.,50.,0.)
       CALL HIDOPT(10001,'LOGY')
       CALL HBOOK1(10002,'MU+ polar angle distribution$',41,-1.,1.05,0.)
       CALL HIDOPT(10002,'LOGY')
       CALL HBOOK1(10003,'MU+ azimuthal angle distribution $',
     &                                                   40,0.,360.,0.)
       CALL HBOOK1(10004,'MU- energy distribution $',50,0.,50.,0.)
       CALL HIDOPT(10004,'LOGY')
       CALL HBOOK1(10005,'MU- polar angle distribution$',41,-1.,1.05,0.)
       CALL HIDOPT(10005,'LOGY')
       CALL HBOOK1(10006,'MU- azimuthal angle distribution $',
     &                                                   40,0.,360.,0.)
       CALL HBOOK1(10007,'MU+/MU- accolinearity distribution (degr.)$',
     &                                                   40,0.,180.,0.)
       CALL HIDOPT(10007,'LOGY')
       CALL HBOOK1(10008,'MU+/MU- accoplanarity distribution (degr.)$',
     &                                                   40,0.,180.,0.)
       CALL HIDOPT(10008,'LOGY')
       CALL HBOOK1(10009,'Photon multiplicity $',40,0.,40.,0.)
       CALL HBOOK1(10010,'Photon energy distribution $',50,0.,50.,0.)
       CALL HIDOPT(10010,'LOGY')
       CALL HBOOK1(10011,'Photon angular spectrum $', 50,-1.,1.,0.)
       CALL HIDOPT(10011,'LOGY')
C
      ELSE IF(ITFIN.EQ.1) THEN
       CALL HTITLE('TAU+ TAU- KORLAB   FINAL STATE')
       CALL HTABLE(10000,'TAU DECAY MODES$',11,0.,11.,11,.0,11.,0.)
       CALL HBOOK1(10001,'PHOTON ENERGY SPECTRUM $', 50,0.,1.,0.)
       CALL HIDOPT(10001,'LOGY')
       CALL HBOOK1(10002,'PHOTON ANGULAR SPECTRUM $', 50,-1.,1.,0.)
       CALL HIDOPT(10002,'LOGY')
       CALL HBOOK1(10003,'POSITRON ENERGY SPECTRUM $', 50,0.,1.,0.)
       CALL HBOOK1(10004,'POSITRON ANGULAR SPECTRUM $', 50,-1.,1.,0.)
       CALL HBOOK1(10005,'MU+ ENERGY SPECTRUM $', 50,0.,1.,0.)
       CALL HBOOK1(10006,'MU+ ANGULAR SPECTRUM $', 50,-1.,1.,0.)
       CALL HBOOK1(10007,'PI+ ENERGY SPECTRUM DIRECT FROM TAU$', 50,
     &   0.,1.,0.)
       CALL HBOOK1(10008,'PI+ ANGULAR SPECTRUM DIRECT FROM TAU$', 50,
     &   -1.,1.,0.)
       CALL HBOOK1(10009,'PI+ ENERGY SPECTRUM FROM RHO$', 50,
     &   0.,1.,0.)
       CALL HBOOK1(10010,'PI+ ANGULAR SPECTRUM FROM RHO $', 50,
     &   -1.,1.,0.)
       CALL HBOOK1(10011,'PI+ ENERGY SPECTRUM FROM A1$', 50,
     &   0.,1.,0.)
       CALL HBOOK1(10012,'PI+ ANGULAR SPECTRUM FROM A1 $', 50,
     & -1.,1.,0.)
       CALL HBOOK1(10013,'PI+ ENERGY  SPECTR. FROM K*+$',50,
     &   .0,1.,.0)
       CALL HBOOK1(10014,'PI+ ANGULAR SPECTR. FROM K*+$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(10015,'PI+- ENERGY  SPECTR. FROM MULTIPI+$',50,
     &   .0,1.,.0)
       CALL HBOOK1(10016,'PI+- ANGULAR SPECTR. FROM MULTIPI+$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(10017,'K+ ENERGY  SPECTR. DIRECT FROM TAU+',50,
     &   .0,1.,.0)
       CALL HBOOK1(10018,'K+ ANGULAR SPECTR. DIRECT FROM TAU+$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(10019,'K+ ENERGY  SPECTR. FROM K*+$',50,
     &   .0,1.,.0)
       CALL HBOOK1(10020,'K+ ANGULAR SPECTR. FROM K*+$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(20003,'E-  ENERGY SPECTR. FROM TAU-$',50,
     &   .0,1.,.0)
       CALL HBOOK1(20004,'E- ANGULAR SPECTR. FROM TAU-$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(20005,'MU- ENERGY SPECTR. FROM TAU-$',50,
     &   .0,1.,.0)
       CALL HBOOK1(20006,'MU- ANGULAR SPECTR. FROM TAU-$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(20007,'PI- ENERGY SPECTR. FROM TAU-$',50,
     &   .0,1.,.0)
       CALL HBOOK1(20008,'PI- ANGULAR SPECTR. FROM TAU-$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(20009,'PI- ENERGY SPECTR. FROM RHO-$',50,
     &   .0,1.,.0)
       CALL HBOOK1(20010,'PI- ANGULAR SPECTR. FROM RHO-$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(20011,'PI+- ENERGY SPECTR. FROM A1-$',50,
     &   .0,1.,.0)
       CALL HBOOK1(20012,'PI+- ANGULAR SPECTR. FROM A1-$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(20013,'PI- ENERGY SPECTR. FROM K*-$',50,
     &   .0,1.,.0)
       CALL HBOOK1(20014,'PI- ANGULAR SPECTR. FROM K*-$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(20015,'PI+- ENERGY  SPECTR. FROM MULTIPI-$',50,
     &   .0,1.,.0)
       CALL HBOOK1(20016,'PI+- ANGULAR SPECTR. FROM MULTIPI-$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(20017,'K- ENERGY  SPECTR. DIRECT FROM TAU-',50,
     &   .0,1.,.0)
       CALL HBOOK1(20018,'K- ANGULAR SPECTR. DIRECT FROM TAU-$',50,
     &   -1.,1.,.0)
       CALL HBOOK1(20019,'K- ENERGY  SPECTR. FROM K*-$',50,
     &   .0,1.,.0)
       CALL HBOOK1(20020,'K- ANGULAR SPECTR. FROM K*-$',50,
     &   -1.,1.,.0)
      ELSE IF(ITFIN.EQ.3) THEN
       CALL HTITLE('NU NUBAR GAMMA(s) FINAL STATE')
       CALL HBOOK1(10009,'Photon multiplicity $',40,0.,40.,0.)
       CALL HBOOK1(10010,'Photon energy distribution $',50,0.,50.,0.)
       CALL HIDOPT(10010,'LOGY')
       CALL HBOOK1(10011,'PHOTON ANGULAR SPECTRUM $', 50,-1.,1.,0.)
       CALL HIDOPT(10011,'LOGY')
      ELSE IF(ITFIN.GT.3) THEN
       CALL HTITLE('Q QBAR  GAMMA(s) FINAL STATE')
       CALL HBOOK1(10001,'QBAR energy distribution $',50,0.,50.,0.)
       CALL HIDOPT(10001,'LOGY')
       CALL HBOOK1(10002,'QBAR polar angle distribution',41,-1.,1.05,0.)
       CALL HIDOPT(10002,'LOGY')
       CALL HBOOK1(10003,'QBAR azimuthal angle distribution ',
     &                                                   40,0.,360.,0.)
       CALL HBOOK1(10004,'Q   energy distribution $',50,0.,50.,0.)
       CALL HIDOPT(10004,'LOGY')
       CALL HBOOK1(10005,'Q   polar angle distribution$',41,-1.,1.05,0.)
       CALL HIDOPT(10005,'LOGY')
       CALL HBOOK1(10006,'Q   azimuthal angle distribution $',
     &                                                   40,0.,360.,0.)
       CALL HBOOK1(10007,'QBAR/Q  accolinearity distribution (degr.)$',
     &                                                   40,0.,180.,0.)
       CALL HIDOPT(10007,'LOGY')
       CALL HBOOK1(10008,'QBAR/Q  accoplanarity distribution (degr.)$',
     &                                                   40,0.,180.,0.)
       CALL HIDOPT(10008,'LOGY')
       CALL HBOOK1(10009,'Photon multiplicity $',40,0.,40.,0.)
       CALL HBOOK1(10010,'Photon energy distribution $',50,0.,50.,0.)
       CALL HIDOPT(10010,'LOGY')
       CALL HBOOK1(10011,'Photon angular spectrum $', 50,-1.,1.,0.)
       CALL HIDOPT(10011,'LOGY')
      ENDIF
C
C     =======================
C
      ELSE IF(IMOD.EQ.0) THEN
C
      IF(ITFIN.GE.2) THEN
       IF (ITFIN.NE.3) THEN
       CALL HFILL(10001,PARTLU(3,4),0.,1.)
       THEMUP = PARTLU(3,3)/PARTLU(3,4)
       IF(THEMUP.GE. 1.) THEMUP =  0.999999
       IF(THEMUP.LE.-1.) THEMUP = -0.999999
       CALL HFILL(10002,THEMUP,0.,1.)
       PHIMUP = PARTLU(3,2)/SQRT(PARTLU(3,1)**2+PARTLU(3,2)**2)
       PHIMUP = 180.*ACOS(PHIMUP)/3.14159
       IF(PARTLU(3,1).LT.0.) PHIMUP = 360. - PHIMUP
       CALL HFILL(10003,PHIMUP,0.,1.)
       CALL HFILL(10004,PARTLU(4,4),0.,1.)
       THEMUM = PARTLU(4,3)/PARTLU(4,4)
       IF(THEMUM.GE. 1.) THEMUM =  0.999999
       IF(THEMUM.LE.-1.) THEMUM = -0.999999
       CALL HFILL(10005,THEMUM,0.,1.)
       PHIMUM = PARTLU(4,2)/SQRT(PARTLU(4,1)**2+PARTLU(4,2)**2)
       PHIMUM = ACOS(PHIMUM)
       PHIMUM = 180.*PHIMUM/3.14159
       IF(PARTLU(4,1).LT.0.) PHIMUM = 360. - PHIMUM
       CALL HFILL(10006,PHIMUM,0.,1.)
       ACCOLI = PARTLU(3,1)*PARTLU(4,1)+PARTLU(3,2)*PARTLU(4,2)+
     &          PARTLU(3,3)*PARTLU(4,3)
       ACCOLI = ACCOLI/
     &           (SQRT(PARTLU(3,1)**2+PARTLU(3,2)**2+PARTLU(3,3)**2)*
     &            SQRT(PARTLU(4,1)**2+PARTLU(4,2)**2+PARTLU(4,3)**2))
       IF(ACCOLI.GE. 1.) ACCOLI =  0.999999
       IF(ACCOLI.LE.-1.) ACCOLI = -0.999999
       ACCOLI = 180.*(1.-ACOS(ACCOLI)/3.14159)
       CALL HFILL(10007,ACCOLI,0.,1.)
       ACCOPL = PARTLU(3,1)*PARTLU(4,1)+PARTLU(3,2)*PARTLU(4,2)
       ACCOPL = ACCOPL/(SQRT(PARTLU(3,1)**2+PARTLU(3,2)**2)*
     &                  SQRT(PARTLU(4,1)**2+PARTLU(4,2)**2))
       IF(ACCOPL.GE. 1.) ACCOPL =  0.999999
       IF(ACCOPL.LE.-1.) ACCOPL = -0.999999
       ACCOPL = 180.*(1.-ACOS(ACCOPL)/3.14159)
       CALL HFILL(10008,ACCOPL,0.,1.)
       ENDIF
       XPHOTON = NPARLU - 4
       IF(XPHOTON.NE.0.) THEN
        CALL HFILL(10009,XPHOTON,0.,1.)
        DO 10 I = 5,NPARLU
         THEPHO = PARTLU(I,3)/PARTLU(I,4)
         IF(THEPHO.GE. 1.) THEPHO =  0.999999
         IF(THEPHO.LE.-1.) THEPHO = -0.999999
         CALL HFILL(10011,THEPHO,0.,1.)
   10    CALL HFILL(10010,PARTLU(I,4),0.,1.)
       ENDIF
C
      ELSE IF(ITFIN.EQ.1) THEN
       ENE = PARTLU(1,4)
C PHOTON ENERGY AND ANGULAR SPECTRUM
       IF(APH(4).GT.0.0001) THEN
        CALL HFILL(10001,APH(4)/ENE,0.,1.)
        THEPHO = APH(3)/APH(4)
        IF(THEPHO.GE. 1.) THEPHO =  0.999999
        IF(THEPHO.LE.-1.) THEPHO = -0.999999
        CALL HFILL(10002,THEPHO,0.,1.)
       ENDIF
       DO 50 IP = 5,NPARLU
CAM  SKIP RADIATIVE GAMMA OR UNSTABLE PARTICLE
        IF(KODELU(IP,1).EQ.0.OR.KODELU(IP,1).GE.11) GO TO 50
C
        IPORIG=KODELU(IP,3)
        KKFORI=KODELU(IPORIG,2)
        KFORIG=ABS(KKFORI)
        IF(KKFORI.NE.15.AND.KKFORI.NE.-15) THEN
         IPORIG=KODELU(IPORIG,3)
         KKFORI=KODELU(IPORIG,2)
        ENDIF
            IF    (KKFORI.EQ.+IDFF) THEN
              JAK=JAKP
            ELSEIF(KKFORI.EQ.-IDFF) THEN
              JAK=JAKM
        ELSE
         PRINT *,' ILLEGAL KFORIG IN BUKERD',
     &    IP,KKFORI,KODELU(IP,1),KODELU(IP,2),KODELU(KODELU(IP,1),1),
     &    KODELU(KODELU(IP,1),2),IPORIG
         STOP
        ENDIF
            IH0 = 15000+SIGN(5000,KKFORI)
C
        XMOD = SQRT(PARTLU(IP,1)*PARTLU(IP,1)+
     &           PARTLU(IP,2)*PARTLU(IP,2)+PARTLU(IP,3)*PARTLU(IP,3))
C POSITRON ENERGY AND ANGULAR SPECTRUM
        IF(KODELU(IP,2).EQ.11.OR.KODELU(IP,2).EQ.-11) THEN
C ELECTRON ENERGY AND ANGULAR SPECTRUM FROM TAU
         CALL HFILL(IH0+3, PARTLU(IP,4)/ENE ,0.,1.)
         CALL HFILL(IH0+4, PARTLU(IP,3)/XMOD,0.,1.)
        ELSEIF(KODELU(IP,2).EQ.13.OR.KODELU(IP,2).EQ.-13) THEN
C MU ENERGY AND ANGULAR SPECTRUM FROM TAU
         CALL HFILL(IH0+5, PARTLU(IP,4)/ENE ,0.,1.)
         CALL HFILL(IH0+6, PARTLU(IP,3)/XMOD,0.,1.)
        ELSEIF(KODELU(IP,2).EQ.211.OR.KODELU(IP,2).EQ.-211) THEN
         ISGN=SIGN(1,KODELU(IP,2))
         IF( KFORIG.EQ.15.AND.JAK.EQ.3) THEN
C PI ENERGY AND ANGULAR SPECTRUM DIRECT FROM TAU
          CALL HFILL(IH0+7, PARTLU(IP,4)/ENE ,0.,1.)
          CALL HFILL(IH0+8,ISGN*PARTLU(IP,3)/XMOD,0.,1.)
         ELSE IF(KFORIG.EQ.213) THEN
C PI ENERGY AND ANGULAR SPECTRUM FROM RHO
          CALL HFILL(IH0+9,  PARTLU(IP,4)/ENE ,0.,1.)
          CALL HFILL(IH0+10,ISGN*PARTLU(IP,3)/XMOD,0.,1.)
         ELSE IF(KFORIG.EQ.20213) THEN
C PI ENERGY AND ANGULAR SPECTRUM FROM A1
          CALL HFILL(IH0+11, PARTLU(IP,4)/ENE ,0.,1.)
          CALL HFILL(IH0+12, PARTLU(IP,3)/XMOD,0.,1.)
         ELSE IF(KFORIG.EQ.323) THEN
C PI ENERGY AND ANGULAR SPECTRUM FROM K*
          CALL HFILL(IH0+13, PARTLU(IP,4)/ENE ,0.,1.)
          CALL HFILL(IH0+14,ISGN*PARTLU(IP,3)/XMOD,0.,1.)
         ELSE
C PI+- ENERGY AND ANGULAR SPECTRUM FROM MULTIPION DECAYS
          CALL HFILL(IH0+15, PARTLU(IP,4)/ENE ,0.,1.)
          CALL HFILL(IH0+16, PARTLU(IP,3)/XMOD,0.,1.)
         ENDIF
        ELSEIF(KODELU(IP,2).EQ.321.OR.KODELU(IP,2).EQ.-321) THEN
         ISGN=SIGN(1,KODELU(IP,2))
         IF( KFORIG.EQ.15) THEN
C K ENERGY AND ANGULAR SPECTRUM DIRECT FROM TAU
          CALL HFILL(IH0+17, PARTLU(IP,4)/ENE ,0.,1.)
          CALL HFILL(IH0+18,ISGN*PARTLU(IP,3)/XMOD,0.,1.)
         ELSE
C K ENERGY AND ANGULAR SPECTRUM FROM K*
          CALL HFILL(IH0+19,  PARTLU(IP,4)/ENE ,0.,1.)
          CALL HFILL(IH0+20,ISGN*PARTLU(IP,3)/XMOD,0.,1.)
         ENDIF
        END IF
 50    CONTINUE
      ENDIF
C
C     ========================
C
      ELSE IF(IMOD.EQ. 1) THEN
C
       CALL HMINIM(0,0.)
       CALL HIDOPT(0,'1EVL')
       CALL HIDOPT(0,'INTE')
       CALL HINDEX
      ENDIF
C
      RETURN
      END
