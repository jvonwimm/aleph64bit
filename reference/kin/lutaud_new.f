      SUBROUTINE LUTAUD(ITAU,IORIG,KFORIG,NDECAY)
C -----------------------------------------------------------------------
C
C!  Decay of a (polarised) tau
C
C L.Duflot : interface with the TAUOLA library       OCTOBER 1994
C B.Bloch  : cleaning to use directly TAUOLA without KORALZ 10 November 96
C
C Warning : TAUOLA will overwrite the HEPEV common. Any information in
C           it will be destroyed!
C
C -----------------------------------------------------------------------
C
C...Input:
C...ITAU is the position where the decaying tau is stored in /LUJETS/.
C...IORIG is the position where the mother of the tau is stored;
C...     is 0 when the mother is not stored.
C...KFORIG is the flavour of the mother of the tau;
C...     is 0 when the mother is not known.
C...Note that IORIG=0 does not necessarily imply KFORIG=0;
C...     e.g. in B hadron semileptonic decays the W  propagator
C...     is not explicitly stored but the W code is still unambiguous.
C...Output:
C...NDECAY is the number of decay products in the current tau decay.
C...These decay products should be added to the /LUJETS/ common block,
C...in positions N+1 through N+NDECAY. For each product I,you must
C...give the flavour codes K(I,2) and the five-momenta P(I,1), P(I,2),
C...P(I,3), P(I,4) and P(I,5). The rest will be stored automatically.
C -----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
      IF ( FIRST ) THEN
        CALL TAUOLAI
        FIRST = .FALSE.
      ENDIF
C try to determine tau polar from its origine
      CALL TAUPOL(ITAU,IORIG,KFORIG)
C
      CALL TAUDKAY(ITAU,NDECAY,0)
      RETURN
      END
      SUBROUTINE TAUOLAI
C--------------------------------------------------------------------
C!  initialiase TAUOLA
C L.Duflot/B. Bloch                         september 1994
C
C--------------------------------------------------------------------
      COMMON / INOUT / INUT,IOUT
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (L2PAR=500, L2PARF=2000 )
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
C
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON / IDFC  / IDFF
      COMMON / IDPART/ IA1
      COMMON / TAURAD / XK0DEC,ITDKRC
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
C 
      REAL  PDUM1(4),PDUM2(4),PDUM3(4),PDUM4(4),PDUM5(4)
      REAL  PDUM(4)
      REAL  PDUMI(4,9) 
      DIMENSION E1(3),E2(3),XPAR(40),PB1(4),PB2(4)
      INTEGER NPAR(40)
C
      INUT = 5
      IOUT = MSTU(11)
C
C  THE INPUT QUANTITIES ARE
C JAK1,JAK2, DECAY TYPE FOR TAU+ AND TAU-.
C   JAK  =  1  ELECTRON DECAY , = 2  MU  DECAY,   =  3  PI DECAY ,
C        =  4  RHO DECAY,     , =  5  A1  DECAY, ETC
C        =  0  INCLUSIVE   , = -1  NO DECAY.
C ITDKRC=0 NO RAD IN TAU DECAY , 1 = WITH RAD
      JAK1   =  0
      JAK2   =  0
      ITDKRC =  1
      XK0DEC =  .001
C
      idff = -15     ! tau+ ident
      call inimas    ! masses in /PARMAS/
      call iniphx    ! constants in /QEDPRM/
      call initdk    ! decay params in /DECPAR/TAUBRA/TAUKLE/DECOMP/
C more default  values
      AMNUTA = 0.001
C set the a1 mass coherently
      IA1=20213                                  !JETSET7.3 CODE FOR A1
      PMAS(LUCOMP(ia1),1)= ama1
      PMAS(LUCOMP(ia1),2)= gama1
C
C    possibly update branching ratios  with card GKBR
C
      NAGKBR = NAMIND('GKBR')
      JGKBR = IW(NAGKBR)
      IF(JGKBR.NE.0) THEN
C check consitency of length
        NLEN = IW(JGKBR)
        IF ( NLEN .NE.NCHAN+4 ) THEN
            WRITE (IW(6),
     &        '(1X,'' Inconsistent number of Brs should be'',
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
C
C  by the DATA CARD GTAU
C
      NAGTAU = NAMIND('GTAU')
      JGTAU = IW(NAGTAU)
      IF(JGTAU.NE.0) THEN
       JAK1   = IW(JGTAU+1)
       JAK2   = IW(JGTAU+2)
       ISPIN  = IW(JGTAU+3)   ! not used
       ITDKRC = IW(JGTAU+4)
       XK0DEC = RW(JGTAU+5)
       GV     = RW(JGTAU+6)   ! not used 
       GA     = RW(JGTAU+7)   ! not used 
      ENDIF
C --------------------------------------------------------------------
C
C decaytaus initialisation
C
      CALL TAUDKAY(i,n,-1)
C
      RETURN
      END
      SUBROUTINE TAUDKAY(ITAU,NDECAY,KTO)
C ----------------------------------------------------------------------
C! generates decay of polarized taus
C
C L.D. : ADAPTED FROM KORL07 ROUTINE DEXAY   October 1994
C the tau polarisation is to be taken from the common /LUPOL/.
C HEL    is the helicity 4-vector. To follow the TAUOLA convention
C only the third component is non-zero (i.e. helicity vector in
C the tau rest frame with momentum along Z axis). If zero, the
C polarisation is chosen randomly.
C KTO=-1 INITIALISATION (MANDATORY)
C KTO=0  performs TAU decay :DEXAY1 is called and JETSET common appenned
C with decay products
C KTO=100, PRINT FINAL REPORT (OPTIONAL).
C
C ----------------------------------------------------------------------
      SAVE
      COMMON / INOUT / INUT,IOUT
C
      PARAMETER (NMODE=15,NM1=0,NM2=1,NM3=8,NM4=2,NM5=1,NM6=3)
      COMMON / TAUDCD /IDFFIN(9,NMODE),MULPIK(NMODE)
     &                ,NAMES
      CHARACTER NAMES(NMODE)*31
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      REAL  PDUM1(4),PDUM2(4),PDUM3(4),PDUM4(4),PDUM5(4)
      REAL  PDUM(4)
      REAL  PDUMI(4,9)
C LD
C
      REAL*4 TPOL
      COMMON/TPOLAR/TPOL
      REAL  POL(4)
C
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
      REAL PTO(4)
      REAL*8 QV(4),betax,betay,betaz,gamma
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      SAVE
      DATA IWARM/0/
      KTOM=KTO
C
      IF(KTO.EQ.-1) THEN
C     ==================
C       INITIALISATION OR REINITIALISATION
        IWARM=1
       WRITE(IOUT, 7001) JAK1,JAK2
        NEVTOT=0
        NEV1=0
        NEV2=0
        IF(JAK1.NE.-1.OR.JAK2.NE.-1) THEN
          CALL DEXEL(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5)
          CALL DEXMU(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5)
          CALL DEXPI(-1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DEXRO(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DEXAA(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5,IDUM)
          CALL DEXKK(-1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DEXKS(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,IDUM)
          CALL DEXNEW(-1,IDUM,PDUM,PDUM1,PDUM2,PDUMI,IDUM)
        ENDIF
        DO 21 I=1,30
        NEVDEC(I)=0
        GAMPMC(I)=0
 21     GAMPER(I)=0
 
        do i = 1 , 4
          POL(I) = 0.
        enddo
      ELSEIF(KTO.EQ.0) THEN
C     =====================
C DECAYS ALL TAUS IN THEIR REST FRAME
C
        IF(IWARM.EQ.0) GOTO 902
        IF ( ABS(K7LU(ITAU,2)) .NE. 15 .OR.
     .       K7LU(ITAU,1)  .NE. 1           )     Return
        NEVTOT=NEVTOT+1
        ISGN = K7LU(ITAU,2)/15
        IF ( ISGN .EQ. 1 ) THEN       !   tau-
          NEV1=NEV1+1
          JAK=JAK1
          KTOM=1
          KTPOS = 3
        ELSE                          !   tau+
          NEV2=NEV2+1
          JAK=JAK2
          KTOM=2
          KTPOS = 4
        ENDIF
        IF ( JAK .LT. 0 ) RETURN
        PTO(1) = P7LU(ITAU,1)
        PTO(2) = P7LU(ITAU,2)
        PTO(3) = P7LU(ITAU,3)
        PTO(4) = P7LU(ITAU,4)
        PMTO   = P7LU(ITAU,5)
C       CALL LULIST(1)
C.. put tau in HEP common
        CALL FILHEP(KTPOS,1,K7LU(ITAU,2),0,0,0,0,PTO,PMTO,.TRUE. )
        NHEP = 4
 
C    the POLARISATION is TPOL filled in routine TAUPOL
        POL(3) = TPOL
        IF ( POL(3) .EQ. 0. )  POL(3) = SIGN(1.,RNDM(POL)-0.5)
C
C the routine TRALO4  is  now dummy : the output 4-momenta are in
C the tau CM , no boost to cm needed
C
        CALL DEXAY1(KTOM,JAK,JAKOUT,POL,ISGN)
        IF ( ISGN .LT. 0 ) THEN
          JAKP = JAKOUT
        ELSE
          JAKM = JAKOUT
        ENDIF
C... boost back : in LUDECY , particle are along +z
        betax  = 0D0
        betay  = 0D0
        betaz  = -P7LU(ITAU,3)/P7LU(ITAU,4)
        gamma  =  P7LU(ITAU,4)/P7LU(ITAU,5)
        do i = 5 , NHEP
          do j = 1 , 4
            QV(j) = PHEP(j,i)
          enddo
          if ( KTOM .eq. 2 ) then
            QV(1) = -QV(1)
            QV(3) = -QV(3)
          endif
          call myboost(betax,betay,betaz,gamma,QV)
          do j = 1 , 4
            PHEP(j,i) = QV(j)
          enddo
        enddo
 
C... Put decay particles in LUND common  (from LUHEP routine)
        NDECAY = NHEP - 4
C
C with a modified LUDECY (do not overwrite daughter-mother relations
C nor status code ) you can have the (nicer) full tau decay chain as in
C TAUOLA
C
C... Put decay particles in LUND common  (from LUHEP routine)
         N7SAV = N7LU
         CALL TLUHEPC(ITAU,KTPOS,5,NHEP)
C        K7LU(ITAU,1) = 11
         N7LU = N7SAV      ! one must not change N7LU in LUTAUD
         K7LU(ITAU,1) = 1  ! let LUDECY update tau status
 
 
      ELSEIF(KTO.EQ.100) THEN
C     =======================
        IF(JAK1.NE.-1.OR.JAK2.NE.-1) THEN
          CALL DEXEL( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5)
          CALL DEXMU( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5)
          CALL DEXPI( 1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DEXRO( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DEXAA( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5,IDUM)
          CALL DEXKK( 1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DEXKS( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,IDUM)
          CALL DEXNEW( 1,IDUM,PDUM,PDUM1,PDUM2,PDUMI,IDUM)
          WRITE(IOUT,7010) NEV1,NEV2,NEVTOT
          WRITE(IOUT,7011) (NEVDEC(I),GAMPMC(I),GAMPER(I),I= 1,7)
          WRITE(IOUT,7012)
     $         (NEVDEC(I),GAMPMC(I),GAMPER(I),NAMES(I-7),I=8,7+NMODE)
          WRITE(IOUT,7013)
        ENDIF
      ELSE
        GOTO 910
      ENDIF
      RETURN
 7001 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'*****TAUOLA LIBRARY: VERSION 2.6 ******',9X,1H*,
     $ /,' *',     25X,'***********August   1995***************',9X,1H*,
     $ /,' *',     25X,'**AUTHORS: S.JADACH, Z.WAS*************',9X,1H*,
     $ /,' *',     25X,'**R. DECKER, M. JEZABEK, J.H.KUEHN*****',9X,1H*,
     $ /,' *',     25X,'**AVAILABLE FROM: WASM AT CERNVM ******',9X,1H*,
     $ /,' *',     25X,'***** PUBLISHED IN COMP. PHYS. COMM.***',9X,1H*,
     $ /,' *',     25X,'*******CERN-TH-5856 SEPTEMBER 1990*****',9X,1H*,
     $ /,' *',     25X,'*******CERN-TH-6195 SEPTEMBER 1991*****',9X,1H*,
     $ /,' *',     25X,'*******CERN-TH-6793 NOVEMBER  1992*****',9X,1H*,
     $ /,' *',     25X,'**5 or more pi dec.: precision limited ',9X,1H*,
     $ /,' *',     25X,'******DEXAY ROUTINE: INITIALIZATION****',9X,1H*
     $ /,' *',I20  ,5X,'JAK1   = DECAY MODE FERMION1 (TAU+)    ',9X,1H*
     $ /,' *',I20  ,5X,'JAK2   = DECAY MODE FERMION2 (TAU-)    ',9X,1H*
     $  /,1X,15(5H*****)/)
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'*****TAUOLA LIBRARY: VERSION 2.6 ******',9X,1H*,
     $ /,' *',     25X,'***********August   1995***************',9X,1H*,
     $ /,' *',     25X,'**AUTHORS: S.JADACH, Z.WAS*************',9X,1H*,
     $ /,' *',     25X,'**R. DECKER, M. JEZABEK, J.H.KUEHN*****',9X,1H*,
     $ /,' *',     25X,'**AVAILABLE FROM: WASM AT CERNVM ******',9X,1H*,
     $ /,' *',     25X,'***** PUBLISHED IN COMP. PHYS. COMM.***',9X,1H*,
     $ /,' *',     25X,'*******CERN-TH-5856 SEPTEMBER 1990*****',9X,1H*,
     $ /,' *',     25X,'*******CERN-TH-6195 SEPTEMBER 1991*****',9X,1H*,
     $ /,' *',     25X,'*******CERN-TH-6793 NOVEMBER  1992*****',9X,1H*,
     $ /,' *',     25X,'******DEXAY ROUTINE: FINAL REPORT******',9X,1H*
     $ /,' *',I20  ,5X,'NEV1   = NO. OF TAU+ DECS. ACCEPTED    ',9X,1H*
     $ /,' *',I20  ,5X,'NEV2   = NO. OF TAU- DECS. ACCEPTED    ',9X,1H*
     $ /,' *',I20  ,5X,'NEVTOT = SUM                           ',9X,1H*
     $ /,' *','    NOEVTS ',
     $   ' PART.WIDTH     ERROR       ROUTINE    DECAY MODE    ',9X,1H*)
 7011 FORMAT(1X,'*'
     $       ,I10,2F12.7       ,'     DADMEL     ELECTRON      ',9X,1H*
     $ /,' *',I10,2F12.7       ,'     DADMMU     MUON          ',9X,1H*
     $ /,' *',I10,2F12.7       ,'     DADMPI     PION          ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMRO     RHO (->2PI)   ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMAA     A1  (->3PI)   ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMKK     KAON          ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMKS     K*            ',9X,1H*)
 7012 FORMAT(1X,'*'
     $       ,I10,2F12.7,A31                                    ,8X,1H*)
 7013 FORMAT(1X,'*'
     $       ,20X,'THE ERROR IS RELATIVE AND  PART.WIDTH      ',10X,1H*
     $ /,' *',20X,'IN UNITS GFERMI**2*MASS**5/192/PI**3       ',10X,1H*
     $  /,1X,15(5H*****)/)
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DEXAY: LACK OF INITIALISATION')
      STOP
 910  WRITE(IOUT, 9100)
 9100 FORMAT(' ----- DEXAY: WRONG VALUE OF KTO ')
      STOP
      END
      SUBROUTINE TAUPOL(ITAU,IORIG,KFORIG)
C---------------------------------------------------------------------
C deduce the tau polarisation from the tau mother and fill /TPOLAR/
C     if Tau comes from B--> Tau- nutau    POLAR = +1
C     if Tau comes from B--> Tau- nutau X  POLAR = f(Etau) and <0
C     See Ian Tomalin's ALEPH Note 94-138
C---------------------------------------------------------------------
      PARAMETER (LJNPAR=4000)
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      parameter ( kftau=15 , KFW = 24)
      parameter ( rmasb=4.8, rmasc = 1.42 , RC = (RMASC/RMASB)**2)
 
      REAL*4 TPOL
      COMMON/TPOLAR/TPOL
      SAVE /TPOLAR/
 
      LOGICAL     H0, HCH
      H0(I) =     I.EQ.25 .OR. I.EQ.35 .OR. I.EQ.36 
      HCH(I)=     I.EQ.37
 
      TPOL = 0.
 
      KTAU  = K7LU(ITAU,2)
      IMOTH = K7LU(ITAU,3)
      KFMOTH= K7LU(IMOTH,2)
      NMULT = K7LU(IMOTH,5)-K7LU(IMOTH,4)+1
 
C
C           ACHTUNG!!!!!!!!!!!!!!!!!
C
C     TPOL is NOT the tau helicity, but the product 
C
C           -(  Helicity * electric_charge )
C
 
C     B- to tau- vu_tau   case
c
c++      P. Perrodo 19/02/96
c++      in the B+ -> tau nu, helicity=-1
c++      in the B- -> tau nu, helicity=+1
c
      IF ((ABS(KFMOTH).EQ.521).AND.(NMULT.EQ.2)) then
         if (K7LU(IMOTH,2).eq.521) TPOL = -1
         if (K7LU(IMOTH,2).eq.-521) TPOL = 1.
C b--->tau- nu_tau X case : origine is considered to be a W
      ELSEIF ( ABS(KFORIG).EQ.KFW) THEN
c
c++      compute the average tau polarisation function of the tau energy
c++      P. Perrodo 19/02/96
c
         Y = 2.*P7LU(ITAU,4)/RMASB
         RT = (ULMASS(KFTAU)/RMASB)**2
         X0 = 1.-(RC/(1.+RT-Y))
         RNUM1 = 0.
         if (Y*Y-4.*RT.ge.0.) rnum1=sqrt(Y*Y-4.*RT)
         RNUM2 = X0*(3.-Y-RT)+3.*(Y-2.)
         RDEM = X0*(Y*Y-3.*Y*(RT+1.)+8.*RT)-3.*Y*Y+6.*Y*(RT+1)-12.*RT
         if (rdem.ne.0.) TPOLA = rnum1 * rnum2 / rdem
c
c++      generate an helicity which is +-1.
c
         TPOL=-1.
         if (rndm(dummy).lt.(tpola+1.)/2.) TPOL=1.
      ELSE IF( H0(ABS(KFMOTH)) ) THEN
        CALL H0TPOL(KTAU,KFMOTH,TPOL)
      ELSE IF( HCH(ABS(KFMOTH)) ) THEN
        CALL HCHTPOL(KFMOTH,TPOL)
      ENDIF
C let the user overwrite the polarisation for special cases
      TPOL = UTOPOL(ITAU,IORIG,KFORIG,TPOL)
      RETURN
      END
      REAL FUNCTION UTOPOL(ITAU,IORIG,KFORIG,TPOL)
      REAL*4 TPOL
      UTOPOL = TPOL
      END
C      SUBROUTINE BUKERD(NMODE,ITFIN)   ! was used to book histos
C      RETURN
C      END
C      SUBROUTINE DEXAY(K,P)            ! changed into TAUDEKAY
C      RETURN
C      END
      SUBROUTINE TRALO4(KTO,AP,BP,XMP)  ! do not boost to the lab
      REAL*4 AP(4),BP(4),XMP
      DO 10 I = 1 , 4
        BP(I) = AP(I)
10    CONTINUE
C   fix typo here thanks to A. Waananen ! 5 nov 96
      XMP = SQRT(MAX(0.,BP(4)**2-BP(3)**2-BP(2)**2-BP(1)**2))
      RETURN
      END
      SUBROUTINE myboost(BETAX,BETAY,BETAZ,GAMMA,P)
C-------------------------------------------------------------------
C!  Perform a Lorentz transformation of the quadriimpulsion P
C   from frame 1 to frame 2. The boost GAMMA is passed as an
C   argument here to cope with the extreme values taken for a
C   tau, leading to BETA2 = 1, and thus a division by 0 when
C   computing GAMMA as 1/SQRT(1-BETA2).
C
C   Input:     Passed:    --BETAX,Y,Z    2's velocity / 1
C                         --GAMMA        boost parameter
C                         --P,           quadriimpulsion in 1
C
C   Output:    Passed:    --P,           quadriimpulsion in 2
C
C   P. Janot  --  20 oct 1988
C-------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,M-Z)
      DIMENSION P(4)
C
      BETA2 = BETAX**2 + BETAY**2 + BETAZ**2
      ONE   = BETAX*P(1) + BETAY*P(2) + BETAZ*P(3)
      TWO   = (GAMMA-1.D0)*ONE/BETA2- GAMMA*P(4)
      P(1)  = P(1) + BETAX*TWO
      P(2)  = P(2) + BETAY*TWO
      P(3)  = P(3) + BETAZ*TWO
      P(4)  = GAMMA*(-ONE+P(4))
C
      RETURN
      END
 
      SUBROUTINE TLUHEPC(ILT,IHT,ISTART,ILAST)
C----------------------------------------------------------------------
C...Purpose: to convert JETSET event record contents  from
C...the standard event record commonblock.
C
C  take particles from ISTART to ILAST in HEP and append to LUND common
C
C the tau is in postion ILT in LUND and IHT in HEP
C
C L.D. from routine LUHEPC in JETSET73
C----------------------------------------------------------------------
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /HEPEVT/
      SAVE /LUJETS/,/LUDAT1/,/LUDAT2/
 
C...Conversion from standard to JETSET, the easy part.
      MHEP = ILAST-ISTART+1
      IF(MHEP.GT.MSTU(4)) CALL LUERRM(8,
     &  '(LUHEPC:) no more space in /LUJETS/')
      II = N
      NSAV = N
      IFTD = 0
      ILTD = 0
      N=N+MIN(MHEP,MSTU(4))
      NKQ=0
      KQSUM=0
      DO 180 I=ISTART,ISTART+MIN(MHEP,MSTU(4))-1
        II = II + 1
        K(II,1)=0
        IF(ISTHEP(I).EQ.1) K(II,1)=1
        IF(ISTHEP(I).EQ.2) K(II,1)=11
        IF(ISTHEP(I).EQ.3) K(II,1)=21
        K(II,2)=IDHEP(I)
        IF ( JMOHEP(1,I) .EQ. IHT ) THEN
          K(II,3) = ILT
          IF ( IFTD .EQ. 0 ) THEN
            IFTD = II
          ELSE
            ILTD = II
          ENDIF
        ELSE
          K(II,3)=JMOHEP(1,I)+NSAV-ISTART+1
        ENDIF
        IF(JDAHEP(1,I).le.0) then
           K(II,4)=0
           K(II,5)=0
        ELSE
           K(II,4)=JDAHEP(1,I)+NSAV-ISTART+1
           K(II,5)=JDAHEP(2,I)+NSAV-ISTART+1
        ENDIF
        DO 160 J=1,5
  160   P(II,J)=PHEP(J,I)
        DO 170 J=1,4
  170   V(II,J)=VHEP(J,I)
        V(II,5)=0.
        IF(ISTHEP(I).EQ.2.AND.PHEP(4,I).GT.PHEP(5,I)) THEN
          I1=JDAHEP(1,I)
          IF(I1.GT.0.AND.I1.LE.NHEP) V(II,5)=(VHEP(4,I1)-VHEP(4,I))*
     &    PHEP(5,I)/PHEP(4,I)
        ENDIF
C...Fill in missing information on colour connection in jet systems.
        IF(ISTHEP(I).EQ.1) THEN
          KC=LUCOMP(K(II,2))
          KQ=0
          IF(KC.NE.0) KQ=KCHG(KC,2)*ISIGN(1,K(II,2))
          IF(KQ.NE.0) NKQ=NKQ+1
          IF(KQ.NE.2) KQSUM=KQSUM+KQ
          IF(KQ.NE.0.AND.KQSUM.NE.0) THEN
            K(II,1)=2
          ELSEIF(KQ.EQ.2.AND.I.LT.N) THEN
            IF(K(II+1,2).EQ.21) K(II,1)=2
          ENDIF
        ENDIF
  180 CONTINUE
        IF(NKQ.EQ.1.OR.KQSUM.NE.0) CALL LUERRM(8,
     &  '(LUHEPC:) input parton configuration not colour singlet')
C.... Update daughters of the tau
        K(ILT,4) = IFTD
        K(ILT,5) = ILTD
      END
 
      SUBROUTINE HCHTPOL( MOTHER, POL)
C------------------------------------------------------------------------------
C! Determine tau helicity in H+- -> tau neutrino decay 
C
C     INPUT:        MOTHER     JETSET code of tau mother (H+-) (INTEGER)
C     OUTPUT:       POL        tau polarization with respect of 
C                              tau- axis, i.e. 
C                            -(el_charge*helicity)      (+-1.)   (REAL)
C
C     G.Ganis       June 30, 1996
C------------------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
C - Arguments
      INTEGER    MOTHER
      REAL       POL
C - Common blocks
C - Auxiliary variables
      REAL       HEL, QTAU
C - Data statements
C - Macros
C - Statements
      HEL= 0.
      IF( MOTHER.GT.0 ) THEN
        QTAU= 1.
        HEL= -1.                   ! H+ decay ... negative tau+ helicity
      ELSE IF( MOTHER.LT.0 ) THEN
        QTAU=-1.
        HEL=  1.                   ! H- decay ... positive tau- helicity
      END IF
C - Output
      POL = -QTAU*HEL
 
      RETURN
      END 
 
      SUBROUTINE H0TPOL( TAU, MOTHER, POL)
C------------------------------------------------------------------------------
C! Determine tau helicity in H0 -> tau+ tau- decay (H0= h,H,A)
C
C     INPUT:        TAU        JETSET code of tau             (INTEGER)
C                   MOTHER     JETSET code of tau mother (H0) (INTEGER)
C     OUTPUT:       POL        tau polarization with respect of 
C                              tau- axis, i.e. 
C                            -(el_charge*helicity)      (+-1.)   (REAL)
C
C     REMARKS: this subroutine remembers the history of calls for the 
C              same H0, and sets the tau helicity accordingly
C
C     G.Ganis       June 30, 1996
C------------------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
C - Arguments
      INTEGER    TAU, MOTHER
      REAL       POL
C - Common blocks
C - Auxiliary variables
      INTEGER    H0(3), IH
      REAL       H0HEL(3), RNDM, DUM, QTAU, HEL
      SAVE       H0, H0HEL
C - Data statements
      DATA       H0 /3*0/
C - Macros
C - Statements
      IF( MOTHER.EQ.25 ) THEN
        IH= 1
      ELSE IF( MOTHER.EQ.35 ) THEN
        IH= 2
      ELSE IF( MOTHER.EQ.36 ) THEN
        IH= 3
      ELSE
        HEL= 0.
        WRITE(*,*) ' +++ H0TPOL - WARNING: wrong mother code (',
     .             MOTHER,') - set helicty to 0. '
        RETURN
      END IF
C - Get electric charge of the tau
      QTAU= -1.
      IF( TAU.LT.0 ) QTAU= 1.
C
      IF( H0(IH).EQ.0 ) THEN
        HEL= 1.
        IF( RNDM(DUM).GT..5 ) HEL= -1.  ! Choose helicity randomly 
        H0HEL(IH)= HEL                   ! Store relevant info
        H0(IH)= 1                        ! Flag this higgs 
      ELSE
        HEL= -H0HEL(IH)              ! Choose opposite helicity
        H0(IH)= 0                    ! Reset flag
      END IF
C - Output
      POL = -QTAU*HEL
 
      RETURN
      END 
 
      SUBROUTINE UTDCHA( J1, J2 )
C------------------------------------------------------------------------------
C! Set tau decay channels by data card TAUD if present
C!
C!    OUTPUT:            J1     tau- decay channel
C!                       J2     tau- decay channel
C!
C!    G.Ganis        July 96
C------------------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
C - Arguments
      INTEGER    J1, J2
C - Common blocks
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(500000)
      COMMON /BCS/ IW(500000)
      EQUIVALENCE (RW(1),IW(1))
C - Auxiliary variables
      INTEGER    JM, JP, JTAUD, NLINK, I
      LOGICAL    GOODCHAN
C - Data statements
C - Macros
      GOODCHAN(I)= I.GE.0.AND.I.LE.22 
C - Statements
      JTAUD= NLINK('TAUD',0)        ! data card to define decay channels
      IF( JTAUD.GT.0 ) THEN
        JM= IW( JTAUD+1 )
        JP= IW( JTAUD+2 )
      END IF
C - If reasonable decay channel are found set them
      IF( GOODCHAN(JM) ) J1= JM
      IF( GOODCHAN(JP) ) J2= JP
C
      RETURN
      END 
