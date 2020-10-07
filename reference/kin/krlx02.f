************************************************************************
*
* Kingal interface
*
************************************************************************
      SUBROUTINE ASKUSI(IGCOD)
C--------------------------------------------------------------------
C Initialization                   A.Valassi May 1996
C Adapted from KRLW01 interface:   P.Perez   August 1995
C                                  B.Bloch   November 1995
C add possible vertex offset       B.Bloch   September 98 
C add  possible W+- mass diff      A.Valassi November 2000
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      COMMON / KGCOMM / ISTA,VRTEX(4),SDVRT(3),NEVENT(8),ECMI
      COMMON / VERTX / IFVRT,XVRT(3),SXVRT(3)
      COMMON / PSHOW/ IPSHO,IFHADMS,IFHADPS
      COMMON / DECAYS / IFLAV(4),AMDEC(4),BRRAT(2),BREL
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)
      REAL *8 WTCRUD,WTMOD,WTSET,AMDEC,BRRAT,BREL
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
      COMMON /ARDAT1/ PARA(40),MSTA(40)
C
      COMMON / INOUT / INUT,IOUT
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
      COMMON / CGLIB / BLIBK(20000)
      SAVE   / CGLIB /
      REAL*4         spcut,
     &               ecut1,  ecut2,  ecut3,  ecut4,
     &               scut12, scut13, scut14, scut23, scut24, scut34,
     &               cmax1,  cmax2,  cmax3,  cmax4,
     &               cmin1,  cmin2,  cmin3,  cmin4,
     &               cmax12, cmax13, cmax14, cmax23, cmax24, cmax34
      COMMON/KWCTE0/ spcut,                                         !cav
     &               ecut1,  ecut2,  ecut3,  ecut4,                 !cav
     &               scut12, scut13, scut14, scut23, scut24, scut34,!cav
     &               cmax1,  cmax2,  cmax3,  cmax4,                 !cav
     &               cmin1,  cmin2,  cmin3,  cmin4,                 !cav
     &               cmax12, cmax13, cmax14, cmax23, cmax24, cmax34 !cav
      REAL*4         cmaxem, cminem, cmaxep, cminep
      COMMON/KWCTE1/ cmaxem, cminem, cmaxep, cminep                 !cav
      REAL*4         enemee, enepee, scutee, ptsmee
      COMMON/KWCTE2/ enemee, enepee, scutee, ptsmee                 !cav
      REAL*4         spctuu, scutuu
      COMMON/KWCTUU/ spctuu, scutuu                                 !cav
      REAL*4         dmwmmp
      COMMON/KWDMW / dmwmmp                                         !cav
      CHARACTER*80 BXOPE,BXTXT,BXL1F,BXCLO
      DOUBLE PRECISION PI
      PARAMETER (PI=3.1415926535897932D0)
      DIMENSION TABL(142),phoi(4),phof(4)
      DIMENSION YPAR(100)
      DOUBLE PRECISION XPAR(100)
      DIMENSION NPAR(100)
C
C Generator code (see KINLIB DOC)
C
      PARAMETER ( IGCO = 5035 )
      PARAMETER ( IVER = 105  )
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
  101 FORMAT(/,10X,'KRLW02 - CODE NUMBER =',I4,
     &       /,10X,'**************************',
     &       /,10X,' SUBVERSION  :',I10 ,
     &       /,10x,'Last mod = November 15,2000  ')
C
C Input parameters for the generator (see subroutine KORALW for comment)
C
      CALL GLIMIT(20000)
      CALL GOUTPU(IOUT)
      KEYRAD = 1101
      KEYPHY = 101111
      KEYTEK = 110
      KEYMIS = 10
      KEYDWM = 0
      KEYDWP = 0
      JAK1   = 0
      JAK2   = 0
      ITDKRC = 1
      IFPHOT = 1
      IFHADM = 1
      IFHADP = 1
      NOUT   = IOUT
C
      CMSENE = 161.
      GFERMI = 1.16639E-5
      ALFWIN = 128.07
      AMAZ   = 91.1888
      GAMMZ  = 2.4974
      AMAW   = 80.25
      GAMMW  = -2.03
      VVMIN  = 0.000001
      VVMAX  = 0.99
      WTMAX  = -1.
      IPSHO  = 1
C
C  Anomalous coupling constants as defined in the paper:
C   K.Hagiwara, R.D.Peccei, D.Zeppenfeld and K.Hikasa,
C   Nucl. Phys. B282 (1987) 253.
C  Must set KeyACC=1 in order to activate them.
C  Standard Model values taken as default:
C
      YPAR(21) = 1E0 ! g1            WWgamma vertex Real part
      YPAR(22) = 1E0 ! kappa         WWgamma vertex Real part
      YPAR(23) = 0E0 ! lambda        WWgamma vertex Real part
      YPAR(24) = 0E0 ! g4            WWgamma vertex Real part
      YPAR(25) = 0E0 ! g5            WWgamma vertex Real part
      YPAR(26) = 0E0 ! kappa-tilde   WWgamma vertex Real part
      YPAR(27) = 0E0 ! lambda-tilde  WWgamma vertex Real part
      YPAR(31) = 0E0 ! g1            WWgamma vertex Imag part
      YPAR(32) = 0E0 ! kappa         WWgamma vertex Imag part
      YPAR(33) = 0E0 ! lambda        WWgamma vertex Imag part
      YPAR(34) = 0E0 ! g4            WWgamma vertex Imag part
      YPAR(35) = 0E0 ! g5            WWgamma vertex Imag part
      YPAR(36) = 0E0 ! kappa-tilde   WWgamma vertex Imag part
      YPAR(37) = 0E0 ! lambda-tilde  WWgamma vertex Imag part
      YPAR(41) = 1E0 ! g1            WWZ     vertex Real part
      YPAR(42) = 1E0 ! kappa         WWZ     vertex Real part
      YPAR(43) = 0E0 ! lambda        WWZ     vertex Real part
      YPAR(44) = 0E0 ! g4            WWZ     vertex Real part
      YPAR(45) = 0E0 ! g5            WWZ     vertex Real part
      YPAR(46) = 0E0 ! kappa-tilde   WWZ     vertex Real part
      YPAR(47) = 0E0 ! lambda-tilde  WWZ     vertex Real part
      YPAR(51) = 0E0 ! g1            WWZ     vertex Imag part
      YPAR(52) = 0E0 ! kappa         WWZ     vertex Imag part
      YPAR(53) = 0E0 ! lambda        WWZ     vertex Imag part
      YPAR(54) = 0E0 ! g4            WWZ     vertex Imag part
      YPAR(55) = 0E0 ! g5            WWZ     vertex Imag part
      YPAR(56) = 0E0 ! kappa-tilde   WWZ     vertex Imag part
      YPAR(57) = 0E0 ! lambda-tilde  WWZ     vertex Imag part
C
C  Higgs mass and width (for Grace 2.0 amplitudes)
C
      AMH = 1000.
      AGH =    0.
C
C  Cuts on final state particles (>=0 e+/e-: common block /KWCTE0/)
C
      SPCUT  =  0E0 ! Cut on min sqrt(s')=invariant mass e+e- after ISR
      ECUT1  =  0E0 ! Cuts on min fermion energies
      ECUT2  =  0E0 !
      ECUT3  =  0E0 !
      ECUT4  =  0E0 !
      SCUT12 =  0E0 ! Cuts on min invariant masses of fermion pairs
      SCUT13 =  0E0 !
      SCUT14 =  0E0 !
      SCUT23 =  0E0 !
      SCUT24 =  0E0 !
      SCUT34 =  0E0 !
      CMAX1  =  1E0 ! Cuts on max cos-theta of outgoing fermions
      CMAX2  =  1E0 !
      CMAX3  =  1E0 !
      CMAX4  =  1E0 !
      CMIN1  = -1E0 ! Cuts on max cos-theta of outgoing fermions
      CMIN2  = -1E0 !
      CMIN3  = -1E0 !
      CMIN4  = -1E0 !
      CMAX12 =  1E0 ! Cuts on max cos-theta between outgoing fermions
      CMAX13 =  1E0 !
      CMAX14 =  1E0 !
      CMAX23 =  1E0 !
      CMAX24 =  1E0 !
      CMAX34 =  1E0 !
C
C  Cuts on final state particles (>=1 e+/e-: common block /KWCTE1/)
C
      CMAXEM =  1E0 ! Cut on max cos-theta of outgoing electron
      CMINEM = -1E0 ! Cut on min cos-theta of outgoing electron
      CMAXEP =  1E0 ! Cut on max cos-theta of outgoing positron
      CMINEP = -1E0 ! Cut on min cos-theta of outgoing positron
C
C  Cuts on final state particles (>=2 e+/e-: common block /KWCTE2/)
C
      ENEMEE =  0E0 ! Cut on min energy of e-
      ENEPEE =  0E0 ! Cut on min energy of e+
      SCUTEE =  0E0 ! Cut on min invariant mass of e+ and e-
      PTSMEE =  0E0 ! Cut on min sum of |pT(e+)| and |pT(e-)|
C
C  Cuts on final state particles (udud events: common block /KWCTUU/)
C
      SPCTUU =  0E0 ! Cut on min invariant mass of uudd after ISR (s')
      SCUTUU =  0E0 ! Cut on min invariant mass of uu and dd pairs
C
C  Mass difference between W- and W+
C
      DMWMMP =  0E0 ! mass difference between W- and W+ [GeV]
C
C  Default values can be changed by the DATA CARDS:
C  GKRW, GTDK, GKAC, GKHG, GCE0, GCE1, GCE2, GCUU, GDMW
C
      NAGKOR = NAMIND('GKRW')
      JGENE = IW(NAGKOR)
      IF(JGENE.NE.0) THEN
        KEYRAD = IW(JGENE+1)
        KEYPHY = IW(JGENE+2)
        KEYTEK = IW(JGENE+3)
        KEYMIS = IW(JGENE+4)
        KEYDWM = IW(JGENE+5)
        KEYDWP = IW(JGENE+6)
C
        CMSENE = RW(JGENE+7)
        GFERMI = RW(JGENE+8)
        ALFWIN = RW(JGENE+9)
        AMAZ   = RW(JGENE+10)
        GAMMZ  = RW(JGENE+11)
        AMAW   = RW(JGENE+12)
        GAMMW  = RW(JGENE+13)
        VVMIN  = RW(JGENE+14)
        VVMAX  = RW(JGENE+15)
        WTMAX  = RW(JGENE+16)
        if ( iw(JGENE).gt.16 ) ipsho = IW(JGENE+17)
      ENDIF
C
      NAGTAU = NAMIND('GTDK')
      JGTAU = IW(NAGTAU)
      IF(JGTAU.NE.0) THEN
        JAK1   = IW(JGTAU+1)
        JAK2   = IW(JGTAU+2)
        ITDKRC = IW(JGTAU+3)
        IFPHOT = IW(JGTAU+4)
        IFHADM = IW(JGTAU+5)
        IFHADP = IW(JGTAU+6)
      ENDIF
C   store initial hadronisation flags
      IFHADMS = IFHADM
      IFHADPS = IFHADP
C    init ariadne if necessary
      IF ( IPSHO.eq.2) then
          msta(7) = iw(6)
          msta(8) = iw(6)
          msta(3) = 0
          mstj(105) = 0
          IFHADM = 0
          IFHADP = 0
         CALL ARINIT('JETSET')
         CALL ARTUNE('EMC')
         CALL KXARCO(LAPAR)
         CALL ARPRDA
      ENDIF
C
      NAGKAC = NAMIND('GKAC')
      JGKAC = IW(NAGKAC)
      IF(JGKAC.NE.0) THEN
        YPAR(21) = RW(JGKAC+1)
        YPAR(22) = RW(JGKAC+2)
        YPAR(23) = RW(JGKAC+3)
        YPAR(24) = RW(JGKAC+4)
        YPAR(25) = RW(JGKAC+5)
        YPAR(26) = RW(JGKAC+6)
        YPAR(27) = RW(JGKAC+7)
        YPAR(31) = RW(JGKAC+8)
        YPAR(32) = RW(JGKAC+9)
        YPAR(33) = RW(JGKAC+10)
        YPAR(34) = RW(JGKAC+11)
        YPAR(35) = RW(JGKAC+12)
        YPAR(36) = RW(JGKAC+13)
        YPAR(37) = RW(JGKAC+14)
        YPAR(41) = RW(JGKAC+15)
        YPAR(42) = RW(JGKAC+16)
        YPAR(43) = RW(JGKAC+17)
        YPAR(44) = RW(JGKAC+18)
        YPAR(45) = RW(JGKAC+19)
        YPAR(46) = RW(JGKAC+20)
        YPAR(47) = RW(JGKAC+21)
        YPAR(51) = RW(JGKAC+22)
        YPAR(52) = RW(JGKAC+23)
        YPAR(53) = RW(JGKAC+24)
        YPAR(54) = RW(JGKAC+25)
        YPAR(55) = RW(JGKAC+26)
        YPAR(56) = RW(JGKAC+27)
        YPAR(57) = RW(JGKAC+28)
      ENDIF
C
      NAGKHG = NAMIND('GKHG')
      JGKHG = IW(NAGKHG)
      IF(JGKHG.NE.0) THEN
        AMH = RW(JGKHG+1)
        AGH = RW(JGKHG+2)
      ENDIF
C
      NAGCE0 = NAMIND('GCE0')
      JGCE0 = IW(NAGCE0)
      IF(JGCE0.NE.0) THEN
        SPCUT  = RW(JGCE0+1)
        ECUT1  = RW(JGCE0+2)
        ECUT2  = RW(JGCE0+3)
        ECUT3  = RW(JGCE0+4)
        ECUT4  = RW(JGCE0+5)
        SCUT12 = RW(JGCE0+6)
        SCUT13 = RW(JGCE0+7)
        SCUT14 = RW(JGCE0+8)
        SCUT23 = RW(JGCE0+9)
        SCUT24 = RW(JGCE0+10)
        SCUT34 = RW(JGCE0+11)
        CMAX1  = RW(JGCE0+12)
        CMAX2  = RW(JGCE0+13)
        CMAX3  = RW(JGCE0+14)
        CMAX4  = RW(JGCE0+15)
        CMIN1  = RW(JGCE0+16)
        CMIN2  = RW(JGCE0+17)
        CMIN3  = RW(JGCE0+18)
        CMIN4  = RW(JGCE0+19)
        CMAX12 = RW(JGCE0+20)
        CMAX13 = RW(JGCE0+21)
        CMAX14 = RW(JGCE0+22)
        CMAX23 = RW(JGCE0+23)
        CMAX24 = RW(JGCE0+24)
        CMAX34 = RW(JGCE0+25)
      ENDIF
C
      NAGCE1 = NAMIND('GCE1')
      JGCE1 = IW(NAGCE1)
      IF(JGCE1.NE.0) THEN
        CMAXEM = RW(JGCE1+1)
        CMINEM = RW(JGCE1+2)
        CMAXEP = RW(JGCE1+3)
        CMINEP = RW(JGCE1+4)
      ENDIF
C
      NAGCE2 = NAMIND('GCE2')
      JGCE2 = IW(NAGCE2)
      IF(JGCE2.NE.0) THEN
        ENEMEE = RW(JGCE2+1)
        ENEPEE = RW(JGCE2+2)
        SCUTEE = RW(JGCE2+3)
        PTSMEE = RW(JGCE2+4)
      ENDIF
C
      NAGCUU = NAMIND('GCUU')
      JGCUU = IW(NAGCUU)
      IF(JGCUU.NE.0) THEN
        SPCTUU = RW(JGCUU+1)
        SCUTUU = RW(JGCUU+2)
      ENDIF
C
      NAGDMW = NAMIND('GDMW')
      JGDMW = IW(NAGDMW)
      IF(JGDMW.NE.0) THEN
        DMWMMP = RW(JGDMW+1)
      ENDIF
C
C  Consistency checks between switches
C
      KeyWgt = MOD(KeyTek,10)
      IF (KeyWgt.NE.0) THEN
        JAK1   = -1
        JAK2   = -1
        ITDKRC = 0
        IFPHOT = 0
        IFHADM = 0
        IFHADP = 0
        WRITE (IOUT,*)
        WRITE (IOUT,*)
     +  '+++ASKUSI+++ Weighted events required -> no fragmentation'
        WRITE (IOUT,*)
     +  '             (GTDK card values superseeded)'
        WRITE (IOUT,*)
      ENDIF
C
      KeyMas = MOD(KeyPhy,100)/10
      IF (KeyMas.NE.1) THEN
        JAK1   = -1
        JAK2   = -1
        ITDKRC = 0
        IFPHOT = 0
        IFHADM = 0
        IFHADP = 0
        WRITE (IOUT,*)
        WRITE (IOUT,*)
     +  '+++ASKUSI+++ Massless kinematics required -> no fragmentation'
        WRITE (IOUT,*)
     +  '             (GTDK card values superseeded)'
        WRITE (IOUT,*)
      ENDIF
C
C  Warn the user that the W width may be not compatible to other inputs
C
      IF (GAMMW.GT.0E0) THEN
        KeyBra = MOD(KeyPhy,10)
        IF (KeyBra.EQ.1) THEN
          brelectron = 0.1084
          alphas = 3E0/2E0*PI * (1/9E0/brelectron-1E0)
          gammw_ok = 9E0 * GFERMI * amaw**3 /( 6E0 * sqrt(2E0) * PI)
     +             * (1E0 + 2E0/3E0 * alphas/pi)
        ELSE
          gammw_ok = 9E0 * GFERMI * amaw**3 /( 6E0 * sqrt(2E0) * PI)
        ENDIF
        WRITE (IOUT,*)
        WRITE (IOUT,
     +    '(A45,f10.7)')
     +    '+++ASKUSI+++ W width (user input) will be   ', gammw
        WRITE (IOUT,
     +    '(A45,f10.7)')
     +    '             Self-consistent value would be ', gammw_ok
        WRITE (IOUT,*)
      ENDIF
C
C  Hardwired correction for alpha strong in W width (AV)
C   Value of alphas derived from hardwired (in routine filexp) electron
C   branching ratio, used in all cross-section normalizations.
C
      KeyBra = MOD(KeyPhy,10)
      IF (KeyBra.EQ.1 .AND. GAMMW.LE.0E0) THEN
        brelectron = 0.1084
        alphas = 3E0/2E0*PI * (1/9E0/brelectron-1E0)
        gammw = 9E0 * GFERMI * amaw**3 /( 6E0 * sqrt(2E0) * PI)
     +        * (1E0 + 2E0/3E0 * alphas/pi)
        WRITE (IOUT,*)
        WRITE (IOUT,*)
     +  '+++ASKUSI+++ Branching ratios with QCD required'
        WRITE (IOUT,102) alphas
 102    FORMAT
     +('              -> correct GammaW for QCD (alphas=',f8.6,')')
        WRITE (IOUT,*)
      ENDIF
C
C Warn user if fixed width is used in the calculation for either W or Z.
C
      KeyZet = MOD(KeyPhy,1000)/100
      IF (KeyZet.EQ.1) THEN
        amaz_run  = amaz  * (1. + gammz**2/amaz**2/2.)
        gammz_run = gammz * (1. + gammz**2/amaz**2/2.)
        WRITE (IOUT,*)
        WRITE (IOUT,*)
     +  '+++ASKUSI+++ Fixed Z width required'
        WRITE (IOUT,*)
     +  ' !!!!!! Be aware of resulting MASS SHIFT !!!!!!'
        WRITE (IOUT,*)
     +  ' Ref. Phys.Lett.B206(1988)539; CERN96-01,vol.1,page153'
        WRITE (IOUT,111) gammz
 111    FORMAT
     +('  Z width (fixed width) is ',f8.5,' GeV')
        WRITE (IOUT,112) gammz_run
 112    FORMAT
     +('  ===> corresponds to Z width (running width) of ',f8.5,' GeV')
        WRITE (IOUT,113) amaz
 113    FORMAT
     +('  Z mass  (fixed width) is ',f8.5,' GeV')
        WRITE (IOUT,114) amaz_run
 114    FORMAT
     +('  ===> corresponds to Z mass  (running width) of ',f8.5,' GeV')
        WRITE (IOUT,*)
      ENDIF
C
      KeyWu = MOD(KeyPhy,1000000)/100000
      IF (KeyWu.EQ.1) THEN
        amaw_run  = amaw  * (1. + gammw**2/amaw**2/2.)
        gammw_run = gammw * (1. + gammw**2/amaw**2/2.)
        WRITE (IOUT,*)
        WRITE (IOUT,*)
     +  '+++ASKUSI+++ Fixed W width required'
        WRITE (IOUT,*)
     +  ' !!!!!! Be aware of resulting MASS SHIFT !!!!!!'
        WRITE (IOUT,*)
     +  ' Ref. Phys.Lett.B206(1988)539; CERN96-01,vol.1,page153'
        WRITE (IOUT,211) gammw
 211    FORMAT
     +('  W width (fixed width) is ',f8.5,' GeV')
        WRITE (IOUT,212) gammw_run
 212    FORMAT
     +('  ===> corresponds to W width (running width) of ',f8.5,' GeV')
        WRITE (IOUT,213) amaw
 213    FORMAT
     +('  W mass  (fixed width) is ',f8.5,' GeV')
        WRITE (IOUT,214) amaw_run
 214    FORMAT
     +('  ===> corresponds to W mass  (running width) of ',f8.5,' GeV')
        WRITE (IOUT,*)
      ENDIF
C
C  Print out cuts used in generation
C
      BXOPE =  '(//1X,15(5H*****)    )'
      BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'
      BXL1F =  '(1X,1H*,F17.8,               12X, A30,7X,A6, 1X,1H*)'
      BXCLO =  '(1X,15(5H*****)/   )'
      WRITE(IOUT,BXOPE)
      WRITE(IOUT,BXTXT) '  +++ASKUSI+++ Following cuts will be   '
      WRITE(IOUT,BXTXT) '    applied on final state fermions     '
      WRITE(IOUT,BXTXT) '                                        '
      WRITE(IOUT,BXTXT) '         Regardless of flavour:         '
      WRITE(IOUT,BXL1F)  spcut,' Min sqrt(s_prime) after ISR ',' spcut'
      WRITE(IOUT,BXL1F)  ecut1,' Min energy fermion 1        ',' ecut1'
      WRITE(IOUT,BXL1F)  ecut2,' Min energy fermion 2        ',' ecut2'
      WRITE(IOUT,BXL1F)  ecut3,' Min energy fermion 3        ',' ecut3'
      WRITE(IOUT,BXL1F)  ecut4,' Min energy fermion 4        ',' ecut4'
      WRITE(IOUT,BXL1F) scut12,' Min inv. mass fermions 1-2  ','scut12'
      WRITE(IOUT,BXL1F) scut13,' Min inv. mass fermions 1-3  ','scut13'
      WRITE(IOUT,BXL1F) scut14,' Min inv. mass fermions 1-4  ','scut14'
      WRITE(IOUT,BXL1F) scut23,' Min inv. mass fermions 2-3  ','scut23'
      WRITE(IOUT,BXL1F) scut24,' Min inv. mass fermions 2-4  ','scut24'
      WRITE(IOUT,BXL1F) scut24,' Min inv. mass fermions 3-4  ','scut34'
      WRITE(IOUT,BXL1F)  cmax1,' Max cos theta fermion 1     ',' cmax1'
      WRITE(IOUT,BXL1F)  cmax2,' Max cos theta fermion 2     ',' cmax2'
      WRITE(IOUT,BXL1F)  cmax3,' Max cos theta fermion 3     ',' cmax3'
      WRITE(IOUT,BXL1F)  cmax4,' Max cos theta fermion 4     ',' cmax4'
      WRITE(IOUT,BXL1F)  cmin1,' Min cos theta fermion 1     ',' cmin1'
      WRITE(IOUT,BXL1F)  cmin2,' Min cos theta fermion 2     ',' cmin2'
      WRITE(IOUT,BXL1F)  cmin3,' Min cos theta fermion 3     ',' cmin3'
      WRITE(IOUT,BXL1F)  cmin4,' Min cos theta fermion 4     ',' cmin4'
      WRITE(IOUT,BXL1F) cmax12,' Max cos theta fermions 1-2  ','cmax12'
      WRITE(IOUT,BXL1F) cmax13,' Max cos theta fermions 1-3  ','cmax13'
      WRITE(IOUT,BXL1F) cmax14,' Max cos theta fermions 1-4  ','cmax14'
      WRITE(IOUT,BXL1F) cmax23,' Max cos theta fermions 2-3  ','cmax23'
      WRITE(IOUT,BXL1F) cmax24,' Max cos theta fermions 2-4  ','cmax24'
      WRITE(IOUT,BXL1F) cmax34,' Max cos theta fermions 3-4  ','cmax34'
      WRITE(IOUT,BXTXT) '                                        '
      WRITE(IOUT,BXTXT) '  For e-nu-x-x evts (incl. e-nu-e-nu):  '
      WRITE(IOUT,BXL1F) cmaxem,' Max cos theta electron      ','cmaxem'
      WRITE(IOUT,BXL1F) cminem,' Min cos theta electron      ','cminem'
      WRITE(IOUT,BXL1F) cmaxep,' Max cos theta positron      ','cmaxep'
      WRITE(IOUT,BXL1F) cminep,' Min cos theta positron      ','cminep'
      WRITE(IOUT,BXTXT) '                                        '
      WRITE(IOUT,BXTXT) '          For e-nu-e-nu evts:           '
      WRITE(IOUT,BXL1F) enemee,' Min energy electron         ','enemee'
      WRITE(IOUT,BXL1F) enepee,' Min energy positron         ','enepee'
      WRITE(IOUT,BXL1F) scutee,' Min invariant mass e+/e-    ','scutee'
      WRITE(IOUT,BXL1F) ptsmee,' Min sum of |pT| e+/e-       ','ptsmee'
      WRITE(IOUT,BXTXT) '                                        '
      WRITE(IOUT,BXTXT) '           For u-d-u-d evts:            '
      WRITE(IOUT,BXL1F) spctuu,' Min sqrt(s_prime) after ISR ','spctuu'
      WRITE(IOUT,BXL1F) scutuu,' Min invariant mass uu or dd ','scutuu'
      WRITE(IOUT,BXCLO)
C  forces loading of TRALO4 from this library
      If ( jak1.lt.-1) call TRALO4(KTOS,PHOI,PHOF,AM)
C
C  All the parameters are stored in TABL(I)
C
      TABL(1)  = KEYRAD
      TABL(2)  = KEYPHY
      TABL(3)  = KEYTEK
      TABL(4)  = KEYMIS
      TABL(5)  = KEYDWM
      TABL(6)  = KEYDWP
      TABL(7)  = IOUT
      TABL(8)  = FLOAT(IPSHO)
      TABL(11) = JAK1
      TABL(12) = JAK2
      TABL(13) = ITDKRC
      TABL(14) = IFPHOT
      TABL(15) = IFHADM
      TABL(16) = IFHADP

      TABL(21) = CMSENE
      TABL(22) = GFERMI
      TABL(23) = ALFWIN
      TABL(24) = AMAZ
      TABL(25) = GAMMZ
      TABL(26) = AMAW
      TABL(27) = GAMMW
      TABL(28) = VVMIN
      TABL(29) = VVMAX
      TABL(30) = WTMAX
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
       TABL(34) = XVRT(1)
       TABL(35) = XVRT(2)
       TABL(36) = XVRT(3)
       TABL(37) = sXVRT(1)
       TABL(38) = sXVRT(2)
       TABL(39) = sXVRT(3)
      DO I=1,7
         TABL(40+I)=YPAR(20+I)
         TABL(50+I)=YPAR(30+I)
         TABL(60+I)=YPAR(40+I)
         TABL(70+I)=YPAR(50+I)
      ENDDO
      TABL(81) = AMH
      TABL(82) = AGH
      TABL(91)  = SPCUT
      TABL(92)  = ECUT1
      TABL(93)  = ECUT2
      TABL(94)  = ECUT3
      TABL(95)  = ECUT4
      TABL(96)  = SCUT12
      TABL(97)  = SCUT13
      TABL(98)  = SCUT14
      TABL(99)  = SCUT23
      TABL(100) = SCUT24
      TABL(101) = SCUT34
      TABL(102) = CMAX1
      TABL(103) = CMAX2
      TABL(104) = CMAX3
      TABL(105) = CMAX4
      TABL(106) = CMIN1
      TABL(107) = CMIN2
      TABL(108) = CMIN3
      TABL(109) = CMIN4
      TABL(110) = CMAX12
      TABL(111) = CMAX13
      TABL(112) = CMAX14
      TABL(113) = CMAX23
      TABL(114) = CMAX24
      TABL(115) = CMAX34
      TABL(121) = CMAXEM
      TABL(122) = CMINEM
      TABL(123) = CMAXEP
      TABL(124) = CMINEP
      TABL(131) = ENEMEE
      TABL(132) = ENEPEE
      TABL(133) = SCUTEE
      TABL(134) = PTSMEE
      TABL(141) = SPCTUU
      TABL(142) = SCUTUU
C
      ECMI = CMSENE
C  Fill the KPAR bank with the generator parameters
C
      NCOL = 142
      NROW = 1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
C
C  Fill RLEP bank
      IEBEAM = NINT(CMSENE * 500.  )
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
      CALL KXL74A (IPART,IKLIN)
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
        WRITE (IOUT,
     &    '(1X,''ASKUSI :error in PART or KLIN bank - STOP - ''
     &                 ,2I3)') IPART,IKLIN
        STOP
      ENDIF
C
C  modify Lund masses according to input masses
      PMAS(LUCOMP(23),1)= AMAZ
      PMAS(LUCOMP(24),1)= AMAW
Cc      PMAS(LUCOMP(16),1)= AMNUTA
Cc      PMAS(LUCOMP( 6),1)= AMTOP
      PMAS(LUCOMP( 7),1)= 150.
      PMAS(LUCOMP( 8),1)= 300.
      IA1=20213                                  !jetset7.3 code for a1
      PMAS(LUCOMP(IA1),1)= 1.251
      PMAS(LUCOMP(IA1),2)= 0.599
C
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
   50   CONTINUE
      ENDIF
C
C  Generator initialization
      NPAR(1)=KEYRAD ! KeyRad =1000*KeyCul+100*KeyNLL+10*KeyFSR+KeyISR
      NPAR(2)=KEYPHY ! KeyPhy =100000*KeyWu+10000*KeyRed+1000*KeySpn+
                     !         100*KeyZet+10*KeyMas+KeyBra
      NPAR(3)=KEYTEK ! KeyTek =100*KeySmp+10*KeyRnd+KeyWgt
      NPAR(4)=KEYMIS ! KeyMis =100*KeyAcc+10*Key4f+KeyMix
      NPAR(5)=KEYDWM ! KEYDWM    W- decay: 7=(ev), 0=all ch.
      NPAR(6)=KEYDWP ! KEYDWP    W+ decay: 7=(ev), 0=all ch.
      NPAR(7)=NOUT   ! NOUT   Output unit number (if Nout.LE.0, Nout=16)
C
      NPAR(21)=JAK1   ! JAK1      Decay mode tau+
      NPAR(22)=JAK2   ! JAK2      Decay mode tau-
      NPAR(23)=ITDKRC ! ITDKRC    Bremsstrahlung switch in Tauola
      NPAR(24)=IFPHOT ! IFPHOT    PHOTOS switch
      NPAR(25)=IFHADM ! IFHADM    Hadronisation W-
      NPAR(26)=IFHADP ! IFHADP    Hadronisation W+IFHADP

      XPAR(1)=CMSENE  ! CMSENE CMS total energy
      XPAR(2)=GFERMI  ! GFERMI Fermi Constant
      XPAR(3)=ALFWIN  ! ALFWIN alpha QED at WW tresh. scale (inverse)
      XPAR(4)=AMAZ    ! AMAZ   Z mass
      XPAR(5)=GAMMZ   ! GAMMZ  Z width
      XPAR(6)=AMAW    ! AMAW   W mass
      XPAR(7)=GAMMW   ! GAMMW  W with, For GAMMW<0 RECALCULATED inside p
      XPAR(8)=VVMIN   ! VVMIN  Photon spectrum parameter
      XPAR(9)=VVMAX   ! VVMAX  Photon spectrum parameter
      XPAR(10)=WTMAX  ! WTMAX  max weight for reject.
                      !        WTMAX<0 = default setting
      XPAR(11)=AMH
      XPAR(12)=AGH
C
      DO I=1,7
         XPAR(20+I)=YPAR(20+I) ! Anomalous coupling constants
         XPAR(30+I)=YPAR(30+I) ! ( They will be activated in
         XPAR(40+I)=YPAR(40+I) !   the matrix element
         XPAR(50+I)=YPAR(50+I) !   only if KeyAcc=1 )
      ENDDO
C
      LENTRY = -1
      CALL KORALW(LENTRY,XPAR,NPAR)
C
C  Print PART and KPAR banks
C
C     CALL LULIST(12)
C     CALL PRPART
      CALL PRTABL('RLEP',0)
      CALL PRTABL('KPAR',0)
      CALL kxlupr(2)
C
C
C    possibly update branching ratios  with card GKBR
C
      NAGKBR = NAMIND('GKBR')
      JGKBR = IW(NAGKBR)
      IF(JGKBR.NE.0) THEN
C check consitency of length
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
      CALL PRTABL('KORL',0)
C
C Book some control histograms - only for unweighted events
C
      IF (KeyWgt.EQ.0) THEN
        call hbook1  (121,'W- generated mass ',100,0.,150.,0.)
        call hbook1  (122,'W+ generated mass ',100,0.,150.,0.) 
        call hbook1  (14,'Energy fermion 1',100,0.,cmsene,0.)
        call hbook1  (24,'Energy fermion 2',100,0.,cmsene,0.)
        call hbook1  (34,'Energy fermion 3',100,0.,cmsene,0.)
        call hbook1  (44,'Energy fermion 4',100,0.,cmsene,0.)
        call hbook1  (16,'Theta fermion 1',90,0.,180.,0.)
        call hbook1  (26,'Theta fermion 2',90,0.,180.,0.)
        call hbook1  (36,'Theta fermion 3',90,0.,180.,0.)
        call hbook1  (46,'Theta fermion 4',90,0.,180.,0.)
        call hbook1  (19,
     +       'Flavour (pdg-code) fermion 1',33,-16.5,16.5,0.)
        call hbook1  (29,
     +       'Flavour (pdg-code) fermion 2',33,-16.5,16.5,0.)
        call hbook1  (39,
     +       'Flavour (pdg-code) fermion 3',33,-16.5,16.5,0.)
        call hbook1  (49,
     +       'Flavour (pdg-code) fermion 4',33,-16.5,16.5,0.)
        call hbook1(50,'total number of stable ',100,0.,150.,0.) 
        call hbook1(51,'total charge of stable ',10,-5.,5.,0.) 
        call lutabu(10)
        call lutabu(20)
        call hbook1 (125,'Invariant mass fermions 1-2',100,0.,cmsene,0.)
        call hbook1 (135,'Invariant mass fermions 1-3',100,0.,cmsene,0.)
        call hbook1 (145,'Invariant mass fermions 1-4',100,0.,cmsene,0.)
        call hbook1 (235,'Invariant mass fermions 2-3',100,0.,cmsene,0.)
        call hbook1 (245,'Invariant mass fermions 2-4',100,0.,cmsene,0.)
        call hbook1 (345,'Invariant mass fermions 3-4',100,0.,cmsene,0.)
        call hbook1 (601,'Number of ISR photons',20,-0.5,19.5,0.)
        call hbook1 (602,'Total energy of ISR photons',70,0.,70.,0.)
        call hbook1 (603,'Transverse component of ISR momentum',
     +       100,0.,20.,0.)
        call hbook1(900,
     +'(Wt/WtMAX)      accepted evts      (Wt.LT.WtMAX) - All flavours',
     +100,0.,1.,0.)
        call hbook1(901,
     +'Log10(Wt/WtMAX) acc. OVERWGTD evts (Wt.GE.WtMAX) - All flavours',
     +100,0.,4.,0.)
        call hbook1(902,
     +'Log10(WEI)      acc. OVERWGTD evts (WEI.GT.1)    - All flavours',
     +100,0.,4.,0.)
        call hbook1(11900,
     +'(Wt/WtMAX)      accepted evts      (Wt.LT.WtMAX) - udud',
     +100,0.,1.,0.)
        call hbook1(11901,
     +'Log10(Wt/WtMAX) acc. OVERWGTD evts (Wt.GE.WtMAX) - udud',
     +100,0.,4.,0.)
        call hbook1(22900,
     +'(Wt/WtMAX)      accepted evts      (Wt.LT.WtMAX) - cscs',
     +100,0.,1.,0.)
        call hbook1(22901,
     +'Log10(Wt/WtMAX) acc. OVERWGTD evts (Wt.GE.WtMAX) - cscs',
     +100,0.,4.,0.)
        call hbook1(12900,
     +'(Wt/WtMAX)      accepted evts      (Wt.LT.WtMAX) - other qqqq',
     +100,0.,1.,0.)
        call hbook1(12901,
     +'Log10(Wt/WtMAX) acc. OVERWGTD evts (Wt.GE.WtMAX) - other qqqq',
     +100,0.,4.,0.)
        call hbook1(17900,
     +'(Wt/WtMAX)      accepted evts      (Wt.LT.WtMAX) - evqq',
     +100,0.,1.,0.)
        call hbook1(17901,
     +'Log10(Wt/WtMAX) acc. OVERWGTD evts (Wt.GE.WtMAX) - evqq',
     +100,0.,4.,0.)
        call hbook1(18900,
     +'(Wt/WtMAX)      accepted evts      (Wt.LT.WtMAX) - other lvqq',
     +100,0.,1.,0.)
        call hbook1(18901,
     +'Log10(Wt/WtMAX) acc. OVERWGTD evts (Wt.GE.WtMAX) - other lvqq',
     +100,0.,4.,0.)
        call hbook1(77900,
     +'(Wt/WtMAX)      accepted evts      (Wt.LT.WtMAX) - evev',
     +100,0.,1.,0.)
        call hbook1(77901,
     +'Log10(Wt/WtMAX) acc. OVERWGTD evts (Wt.GE.WtMAX) - evev',
     +100,0.,4.,0.)
        call hbook1(78900,
     +'(Wt/WtMAX)      accepted evts      (Wt.LT.WtMAX) - other evlv',
     +100,0.,1.,0.)
        call hbook1(78901,
     +'Log10(Wt/WtMAX) acc. OVERWGTD evts (Wt.GE.WtMAX) - other evlv',
     +100,0.,4.,0.)
        call hbook1(88900,
     +'(Wt/WtMAX)      accepted evts      (Wt.LT.WtMAX) - other lvlv',
     +100,0.,1.,0.)
        call hbook1(88901,
     +'Log10(Wt/WtMAX) acc. OVERWGTD evts (Wt.GE.WtMAX) - other lvlv',
     +100,0.,4.,0.)
      ENDIF
      RETURN
      END
      SUBROUTINE kxlupr(ifl)
C************************************************************************
C         print out jetset parameters mstj, parj
C************************************************************************
      PARAMETER (L1MST=200, L1PAR=200)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      WRITE(MSTU(11),*)
      if ( Ifl.ge.1) then
      WRITE(MSTU(11),1000)
      DO 100 I=1,50
        WRITE(MSTU(11),1010) I,MSTJ(I),MSTJ(I+50),PARJ(I),PARJ(I+50)
 100  CONTINUE
      endif
      if ( Ifl.ge.2) then
      WRITE(MSTU(11),1001)
      DO 101 I=1,50
        WRITE(MSTU(11),1010) I,MSTU(I),MSTU(I+50),PARU(I),PARU(I+50)
 101  CONTINUE
      endif
      WRITE(MSTU(11),*)
 1000 FORMAT(10X,'Parameters and switches used by Jetset:',/,/,
     $     '         I   MSTJ(I) MSTJ(I+50)   PARJ(I) PARJ(I+50)',/)
 1001 FORMAT(10X,'Parameters and switches used by Jetset:',/,/,
     $     '         I   MSTU(I) MSTU(I+50)   PARU(I) PARU(I+50)',/)
 1010 FORMAT(2I10,I11,3X,2G11.3)
      RETURN
      END
************************************************************************
      SUBROUTINE ASKUSE (IDP,IST,NTRK,NVRT,ECM,WEI)
C --------------------------------------------------------------------
C Generation                       A.Valassi May 1996
C Adapted from KRLW01 interface:   P.Perez   August 1995
C                                  B.Bloch   November 1995
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
      COMMON / KGCOMM / ISTA,VRTEX(4),SDVRT(3),NEVENT(8),ECMI
      COMMON / VERTX / IFVRT,XVRT(3),SXVRT(3)
      COMMON / PSHOW/ IPSHO,IFHADMS,IFHADPS
      COMMON / DECAYS / IFLAV(4),AMDEC(4),BRRAT(2),BREL
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)
      REAL *8 WTCRUD,WTMOD,WTSET,AMDEC,BRRAT,BREL
      REAL *8 WTMOD4F
      COMMON / WGTGEN /WTMAX,WTMAX4F(20),WTVES,WTYFS,WTSS,WTBWIG,WTBORN
      DOUBLE PRECISION WTMAX,WTMAX4F,WTVES,WTYFS,WTSS,WTBWIG,WTBORN
      COMMON / WGTUNW /wtunw,wtunwmax                              !cav
      DOUBLE PRECISION wtunw,wtunwmax
      COMMON / OVRWGT /nevovrwgt                                   !cav
      DATA             nevovrwgt/0/
      COMMON / SELCTO /nkwcte0,   nkwcte1,   nkwcte2,   nkwctuu    !cav
      DATA             nkwcte0/0/,nkwcte1/0/,nkwcte2/0/,nkwctuu/0/
      COMMON / KeyKey / KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT
      COMMON / MOMDEC / Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
      DOUBLE PRECISION  QEFF1,   QEFF2,   SPHUM,   SPHOT
      DOUBLE PRECISION  Q1,   Q2,   P1,   P2,   P3,   P4
      DOUBLE PRECISION  SUMISR(4)
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
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      COMMON / LIBRA  / JAK1,JAK2,ITDKRC,IFPHOT,IFHADM,IFHADP
      DOUBLE PRECISION XPAR(100)
      DIMENSION NPAR(100)
      DIMENSION E1(3),E2(3)
      bcosd(x)= 57.29577951*acos(x)
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
      VRTEX(1) = RN1*SDVRT(1)
      VRTEX(2) = RN2*SDVRT(2)
      VRTEX(3) = RN3*SDVRT(3)
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
      KeyWgt = MOD(KeyTek,10)
      NEVENT(1) = NEVENT(1) + 1
      if(mod(nevent(1),1000).eq.0)
     $              print *,' KORALW starting event', NEVENT(1)
      CALL KORALW(0,XPAR,NPAR)
      IDP  = abs(IFLAV(1))+100*abs(IFLAV(2))+10000*abs(IFLAV(3))
     $        +1000000*abs(IFLAV(4))
      ECM  = ECMI
      IF (KeyWgt.EQ.0) THEN       ! Unweighted events (wtmod>=1)
        WEI = wtmod
      ELSE IF (KeyWgt.EQ.1) THEN  ! Weighted events
        wtmod4f = wtset(40)
        WEI = wtmod*wtmod4f
      ELSE IF (KeyWgt.EQ.2) THEN  ! Weighted events (wtmod>=1 for CC3)
        wtmod4f = wtset(40)
        WEI = wtmod*wtmod4f
      ENDIF
cav   IF(IST.NE.0) THEN           ! No path to following two statements
cav     NEVENT(4) = NEVENT(4) + 1 ! These can be used to reject events
cav     GO TO 20                  ! for which KORALW has "bad" status
cav   ENDIF                       ! (must set IST#0 in that case...)
CB      call lulist(1)
      IF( IPSHO.EQ.2) then
        call AREXEC
CB      call lulist(1)
      ENDIF
C   store ISR weights in KWGT
      weik = wtset(1)    ! Born
      ind = kwgtbk(1,1,weik)
      weik = wtset(2)    ! 1st order
      ind = kwgtbk(2,2,weik)
      weik = wtset(3)    ! 2nd order
      ind = kwgtbk(3,3,weik)
C   reset weights for next event
      DO i=1,100
         WTSET(i)=0
      ENDDO
      IFHADM = IFHADMS
      IFHADP = IFHADPS
c   CALL LUEXEC only if Koralw set to full hadronization mode!
        IF (IFHADM.EQ.1 .AND. IFHADP.EQ.1) CALL LUEXEC
CB        call lulist(1)
C  reset JETSET hadronisation for ariadne
      IF( IPSHO.EQ.2) then 
         IFHADM = 0
         IFHADP = 0
      ENDIF
C  Book all banks
C
      CALL KXL7AL(VRTEX,ISTA,NVRT,NTRK)
      IST = ISTA
      IF(IST.NE.0) THEN
        NEVENT(5) = NEVENT(5) + 1
        GO TO 20
      ENDIF
C    do not write non fully hadronic events if CR requested
      IF (IPSHO.EQ.2 .and. MSTA(35).gt.0) then
        do iflv = 1,4
         If( abs(IFLAV(iflv)).gt.5) ist = -1
        enddo

        IF(IST.NE.0) THEN
          NEVENT(5) = NEVENT(5) + 1
          GO TO 20
        ENDIF
C   add info for reconnected evnts in KWGT
        weik = float(mhar(135))  ! # of reconnections in event
        ind = kwgtbk(4,2000,weik)
      ENDIF
C
C Fill histograms - only for unweighted events
C
      IF(IST.EQ.0) THEN
        IF (KeyWgt.EQ.0) THEN
          call hfill (14,real(p1(4)),0.,1.)
          call hfill (24,real(p2(4)),0.,1.)
          call hfill (34,real(p3(4)),0.,1.)
          call hfill (44,real(p4(4)),0.,1.)
          call hfill (121,real(sqrt(q1(4)*q1(4)
     &                     -q1(3)*q1(3)-q1(2)*q1(2)-q1(1)*q1(1))),0.,1.)
          call hfill (122,real(sqrt(q2(4)*q2(4)
     &                     -q2(3)*q2(3)-q2(2)*q2(2)-q2(1)*q2(1))),0.,1.)
          call hfill (16,
     &      bcosd(real( p1(3)/sqrt(p1(1)**2+p1(2)**2+p1(3)**2) )),0.,1.)
          call hfill (26,
     &      bcosd(real( p2(3)/sqrt(p2(1)**2+p2(2)**2+p2(3)**2) )),0.,1.)
          call hfill (36,
     &      bcosd(real( p3(3)/sqrt(p3(1)**2+p3(2)**2+p3(3)**2) )),0.,1.)
          call hfill (46,
     &      bcosd(real( p4(3)/sqrt(p4(1)**2+p4(2)**2+p4(3)**2) )),0.,1.)
          call hfill (19,real(iflav(1)),0.,1.)
          call hfill (29,real(iflav(2)),0.,1.)
          call hfill (39,real(iflav(3)),0.,1.)
          call hfill (49,real(iflav(4)),0.,1.)
          call hfill (50,float(klu(0,2)),dum,1.)
          call hfill (51,float(klu(0,6))/3.,dum,1.)
          call lutabu(11)
          call lutabu(21)
          call hfill (125,
     &      real(sqrt( (p1(4)+p2(4))**2 -
     &                 (p1(1)+p2(1))**2 -
     &                 (p1(2)+p2(2))**2 -
     &                 (p1(3)+p2(3))**2   )), 0., 1.)
          call hfill (135,
     &      real(sqrt( (p1(4)+p3(4))**2 -
     &                 (p1(1)+p3(1))**2 -
     &                 (p1(2)+p3(2))**2 -
     &                 (p1(3)+p3(3))**2   )), 0., 1.)
          call hfill (145,
     &      real(sqrt( (p1(4)+p4(4))**2 -
     &                 (p1(1)+p4(1))**2 -
     &                 (p1(2)+p4(2))**2 -
     &                 (p1(3)+p4(3))**2   )), 0., 1.)
          call hfill (235,
     &      real(sqrt( (p2(4)+p3(4))**2 -
     &                 (p2(1)+p3(1))**2 -
     &                 (p2(2)+p3(2))**2 -
     &                 (p2(3)+p3(3))**2   )), 0., 1.)
          call hfill (245,
     &      real(sqrt( (p2(4)+p4(4))**2 -
     &                 (p2(1)+p4(1))**2 -
     &                 (p2(2)+p4(2))**2 -
     &                 (p2(3)+p4(3))**2   )), 0., 1.)
          call hfill (345,
     &      real(sqrt( (p3(4)+p4(4))**2 -
     &                 (p3(1)+p4(1))**2 -
     &                 (p3(2)+p4(2))**2 -
     &                 (p3(3)+p4(3))**2   )), 0., 1.)
          do k=1,4
            sumisr(k)=0d0
            do i=1,nphot
              sumisr(k) = sumisr(k) + sphot (i,k)
            end do
          end do
          call hfill (601,real(nphot),0.,1.)
          call hfill (602,real(sumisr(4)),0.,1.)
          call hfill (603,real(sqrt(sumisr(1)**2+sumisr(2)**2)),0.,1.)
          IF (wtunw.lt.1d0) then
            call hfill(900,real(wtunw),0.,1.)
            IF(     abs(iflav(1)).le.6 .and. abs(iflav(2)).le.6 .and.
     &              abs(iflav(3)).le.6 .and. abs(iflav(4)).le.6 )THEN
              IF(     abs(iflav(1)).eq.1 .and. abs(iflav(2)).eq.2 .and.
     &                abs(iflav(3)).eq.2 .and. abs(iflav(4)).eq.1 )THEN
                call hfill(11900,real(wtunw),0.,1.)
              ELSEIF( abs(iflav(1)).eq.3 .and. abs(iflav(2)).eq.4 .and.
     &                abs(iflav(3)).eq.4 .and. abs(iflav(4)).eq.3 )THEN
                call hfill(22900,real(wtunw),0.,1.)
              ELSE
                call hfill(12900,real(wtunw),0.,1.)
              ENDIF
            ELSEIF( abs(iflav(1)).eq.11 .and.
     &              abs(iflav(4)).eq.11 )THEN
                call hfill(77900,real(wtunw),0.,1.)
            ELSEIF( abs(iflav(1)).eq.11 )THEN
              IF(     abs(iflav(4)).eq.13 .or.
     &                abs(iflav(4)).eq.15 )THEN
                call hfill(78900,real(wtunw),0.,1.)
              ELSE
                call hfill(17900,real(wtunw),0.,1.)
              ENDIF
            ELSEIF( abs(iflav(4)).eq.11 )THEN
              IF(     abs(iflav(1)).eq.13 .or.
     &                abs(iflav(1)).eq.15 )THEN
                call hfill(78900,real(wtunw),0.,1.)
              ELSE
                call hfill(17900,real(wtunw),0.,1.)
              ENDIF
            ELSEIF( abs(iflav(1)).le.6 .or.
     &              abs(iflav(4)).le.6 )THEN
                call hfill(18900,real(wtunw),0.,1.)
            ELSE
                call hfill(88900,real(wtunw),0.,1.)
            ENDIF
          ELSE
            call hfill(901,real(log(wtunw)/log(10d0)),0.,1.)
            IF(     abs(iflav(1)).le.6 .and. abs(iflav(2)).le.6 .and.
     &              abs(iflav(3)).le.6 .and. abs(iflav(4)).le.6 )THEN
              IF(     abs(iflav(1)).eq.1 .and. abs(iflav(2)).eq.2 .and.
     &                abs(iflav(3)).eq.2 .and. abs(iflav(4)).eq.1 )THEN
                call hfill(11901,real(log(wtunw)/log(10d0)),0.,1.)
              ELSEIF( abs(iflav(1)).eq.3 .and. abs(iflav(2)).eq.4 .and.
     &                abs(iflav(3)).eq.4 .and. abs(iflav(4)).eq.3 )THEN
                call hfill(22901,real(log(wtunw)/log(10d0)),0.,1.)
              ELSE
                call hfill(12901,real(log(wtunw)/log(10d0)),0.,1.)
              ENDIF
            ELSEIF( abs(iflav(1)).eq.11 .and.
     &              abs(iflav(4)).eq.11 )THEN
                call hfill(77901,real(log(wtunw)/log(10d0)),0.,1.)
            ELSEIF( abs(iflav(1)).eq.11 )THEN
              IF(     abs(iflav(4)).eq.13 .or.
     &                abs(iflav(4)).eq.15 )THEN
                call hfill(78901,real(log(wtunw)/log(10d0)),0.,1.)
              ELSE
                call hfill(17901,real(log(wtunw)/log(10d0)),0.,1.)
              ENDIF
            ELSEIF( abs(iflav(4)).eq.11 )THEN
              IF(     abs(iflav(1)).eq.13 .or.
     &                abs(iflav(1)).eq.15 )THEN
                call hfill(78901,real(log(wtunw)/log(10d0)),0.,1.)
              ELSE
                call hfill(17901,real(log(wtunw)/log(10d0)),0.,1.)
              ENDIF
            ELSEIF( abs(iflav(1)).le.6 .or.
     &              abs(iflav(4)).le.6 )THEN
                call hfill(18901,real(log(wtunw)/log(10d0)),0.,1.)
            ELSE
                call hfill(88901,real(log(wtunw)/log(10d0)),0.,1.)
            ENDIF
          ENDIF
          IF (wei.GT.1.) then
            nevovrwgt = nevovrwgt+1
            call hfill(902,log(wei)/log(10.),0.,1.)
          ENDIF
        ENDIF
      ENDIF
C
C  Event counters
C
      IF(IST.EQ.0) THEN
        NEVENT(2) = NEVENT(2) + 1
        DO 10 IP = 1,N7LU
          IF(K7LU(IP,2).EQ.22) THEN
            NEVENT(8) = NEVENT(8) + 1
            GO TO 30
          ENDIF
   10   CONTINUE
        NEVENT(7) = NEVENT(7) + 1
      ENDIF
   20 IF(IST.NE.0) NEVENT(3) = NEVENT(3) + 1
C
   30 RETURN
      END
************************************************************************
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C End of generation                A.Valassi May 1996
C Adapted from KRLW01 interface:   P.Perez   August 1995
C                                  B.Bloch   November 1995
C --------------------------------------------------------------------
      COMMON / INOUT  / INUT,IOUT
      COMMON / KGCOMM / ISTA,VRTEX(4),SDVRT(3),NEVENT(8),ECMI
      COMMON / DECAYS / IFLAV(4),AMDEC(4),BRRAT(2),BREL
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)
      REAL *8 WTCRUD,WTMOD,WTSET,AMDEC,BRRAT,BREL
      COMMON / WGTGEN /WTMAX,WTMAX4F(20),WTVES,WTYFS,WTSS,WTBWIG,WTBORN
      DOUBLE PRECISION WTMAX,WTMAX4F,WTVES,WTYFS,WTSS,WTBWIG,WTBORN
      COMMON / KeyKey / KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
cav   COMMON / cmonit / averwt,errela,nevtot,nevacc,nevneg,nevove,nevzer
cav   DOUBLE PRECISION  averwt,errela
      DOUBLE PRECISION svar,Xtot,erra,Xtot2e,errabs
      COMMON / WGTUNW /wtunw,wtunwmax                              !cav
      DOUBLE PRECISION wtunw,wtunwmax
      COMMON / OVRWGT /nevovrwgt                                   !cav
      COMMON / SELCTO /nkwcte0,   nkwcte1,   nkwcte2,   nkwctuu    !cav
      DOUBLE PRECISION XPAR(100)
      DIMENSION NPAR(100)
C
C End of generation
C
      LENTRY = 1
      CALL KORALW(LENTRY,XPAR,NPAR)
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
       WRITE(IOUT,104) NEVENT(4),NEVENT(5)
  104  FORMAT(/10X,'ISTA # 0 FROM KORALW        # OF REJECT = ',I10,
     &        /10X,'ISTA # 0 FROM KXL7AL        # OF REJECT = ',I10)
C
C Print cross-section (MC and Semi-analytical calculations)
C ans store it in KSEC bank
       NTOT = NPAR(10)
       XTOT = XPAR(20)/1000.
       RTOT = XPAR(21)/1000.
       IS = 1
       IDC = 5035
       IVER = 104
       NACC = NTOT
       XACC = XTOT
       RACC = RTOT
C       isec = KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)       
      call ugtsec
      svar=ecmi**2
      keypho=0
      keypre=1
      call korwan(svar,0d0,1d0,keypho,keypre,Xtot,erra)
      keypho=302
      keypre=1
      call korwan(svar,0d0,1d0,keypho,keypre,Xtot2e,errabs)
cav   XSCRUD = xpar(30)
cav   XSKB   = xscrud*averwt
cav   ERKB   = xskb*errela
      XSECT  = xpar(20)
      ERSECT = xpar(21)
      KEYWGT = MOD(KeyTek,10)
      IF(KeyWgt .EQ. 0) then
        NVTRU = npar(10)
        NVTOT = npar(11)
cav     EREL = (1-dble(nvtru)/dble(nvtot))/sqrt(dble(nvtot))
      ENDIF
      WRITE(iout,*)
      WRITE(iout,'(A60)') '============ KRLW02: X-sections ============'
      WRITE(iout,*)
      WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30)')
     $     Xtot,erra,'SemiAnal Born, KORWAN'
      WRITE(iout,*)
      WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30)')
     $     Xtot2e,errabs,'SemiAnal O(alf2)exp.LL, KORWAN'
      WRITE(iout,*)
      WRITE(iout,*)
      IF(KeyWgt .EQ. 0) then
        WRITE(iout,'(1X,A40,F20.4)')
     $       'Max weight for rejection was ',WTMAX
        WRITE(iout,'(1X,A40,I20)')
     $       'Total generated events ',NVTOT
        WRITE(iout,'(1X,A40,I20)')
     $       '- rejected by GCE0 cuts ',nkwcte0
        WRITE(iout,'(1X,A40,I20)')
     $       '- rejected by GCE1 cuts ',nkwcte1
        WRITE(iout,'(1X,A40,I20)')
     $       '- rejected by GCE2 cuts ',nkwcte2
        WRITE(iout,'(1X,A40,I20)')
     $       '- rejected by GCUU cuts ',nkwctuu
        WRITE(iout,'(1X,A40,I20)')
     $       'Total accepted events ',NVTRU
        WRITE(iout,'(1X,A40,I20)')
     $       'amongst which, overweighted ',NEVOVRWGT
        IF (NEVOVRWGT.GT.0) THEN
          WRITE(iout,'(1X,A40,f20.9)')
     $       '===> INCREASE MAX WEIGHT TO AT LEAST ',WTUNWMAX*WTMAX
          WRITE(iout,'(1X,A40,f20.9)')
     $       '     SLOWDOWN FACTOR WILL BE ',WTUNWMAX
          WRITE(iout,'(1X,A40)')
     $       '     (OR USE TIGHTER CUTS INSTEAD...)'
        ENDIF
        WRITE(iout,*)
cav     WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30)')
cav  $       Xscrud,Xscrud*erel, 'MC Best, XSCRUD, KORALW'
cav     WRITE(iout,*)
      ENDIF
cav   WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30)')
cav  $     xskb,erkb, 'MC Best, WTMOD, KORALW'
cav   WRITE(iout,*)
      WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30, 4H <<<)')
     $     xsect,ersect, 'MC Best, XPAR, KORALW'
      WRITE(iout,*)
      WRITE(iout,'(A60)') '========== KRLW02: End X-sections =========='
      WRITE(iout,*)
      call lutabu(12)
      call lutabu(22)
C
      RETURN
      END

************************************************************************
*
* Original code routines largely modified
*
************************************************************************
      subroutine selecto(p1,p2,p3,p4,qeff1,qeff2,wt)
c
c Modified A.Valassi 26/06/96 => introduce Kingal interfaced cuts in the
c          A.Valassi 16/07/96 => change qadra (integer!) to qadra (ZW 5/
c Input:  p1, p2, p3, p4 -> 4-momenta of 4 final state fermions
c         qeff1, qeff2   -> effective 4-mom. of initial state e+/e- afte
c         wt             -> Current weight # 0
c         /DECAYS/       -> Event properties (including flavour) via com
c         /KWCTE0/       -> Cuts via common
c         /KWCTE1/       -> Cuts via common
c         /KWCTE2/       -> Cuts via common
c         /KWCTUU/       -> Cuts via common
c Output: wt             -> Weight = 0 if event outside allowed phase sp
c Remark: cuts are in single precision. Protection added on cos(theta).
c Cut description:
c 0. (COMMON/KWCTE0/) - at least 0 e+/e-
c    => Cuts as in Excalibur.
c       Applied to all events, regardless of flavour.
c       Recommended values for production: NO CUTS.
c 1. (COMMON/KWCTE1/) - at least 1 e+/e-
c    => Further cuts on final state electron/positron theta.
c       Useful to remove high-weight all-4f-diagram e-nu-x-x events with
c       e+- lost along the beam pipe (mainly t-channel photon exchange).
c 2. (COMMON/KWCTE2/) - at least 2 e+/e-
c    => Further cuts on final state electron and positron in e+nue~e-nue
c       Useful to remove high-weight all-4f-diagram e+nue~e-nue events w
c       low-energy, low-mass, low-Pt e+- pairs from photon brehmsstrahlu
c 3. (COMMON/KWCTE2/) - only uudd final states
c    => Further cuts on uudd final states.
c       Useful to remove high-weight all-4f-diagram udud events with
c       low-mass uu or dd pairs from photon brehmsstrahlung.
c
      implicit real*8 (a-h,o-z)
      dimension p1(4),p2(4),p3(4),p4(4)
      dimension qeff1(4),qeff2(4)
      COMMON /DECAYS/ IFLAV(4), AMDEC(4), BR(2), BREL
      save   /DECAYS/
      REAL*4          spcut,
     &                ecut1,  ecut2,  ecut3,  ecut4,
     &                scut12, scut13, scut14, scut23, scut24, scut34,
     &                cmax1,  cmax2,  cmax3,  cmax4,
     &                cmin1,  cmin2,  cmin3,  cmin4,
     &                cmax12, cmax13, cmax14, cmax23, cmax24, cmax34
      COMMON /KWCTE0/ spcut,                                         !ca
     &                ecut1,  ecut2,  ecut3,  ecut4,                 !ca
     &                scut12, scut13, scut14, scut23, scut24, scut34,!ca
     &                cmax1,  cmax2,  cmax3,  cmax4,                 !ca
     &                cmin1,  cmin2,  cmin3,  cmin4,                 !ca
     &                cmax12, cmax13, cmax14, cmax23, cmax24, cmax34 !ca
      SAVE   /KWCTE0/
      REAL*4          cmaxem, cminem, cmaxep, cminep
      COMMON /KWCTE1/ cmaxem, cminem, cmaxep, cminep                 !ca
      SAVE   /KWCTE1/
      REAL*4          enemee, enepee, scutee, ptsmee
      COMMON /KWCTE2/ enemee, enepee, scutee, ptsmee                 !ca
      SAVE   /KWCTE2/
      REAL*4          spctuu, scutuu
      COMMON /KWCTUU/ spctuu, scutuu                                 !ca
      SAVE   /KWCTUU/
      COMMON /SELCTO/ nkwcte0,   nkwcte1,   nkwcte2,   nkwctuu       !ca
      SAVE   /SELCTO/
c
      if (wt.eq.0d0) return
c
c 0. /KWCTE0/ cuts
c ----------------
c Cut on min sqrt(s') - invariant mass of beam e+e- after ISR
c
      if (qadra(qeff1,qeff2).lt.spcut**2) wt=0d0
c
c Cuts on min fermion energies
c
      if (p1(4).lt.ecut1) wt=0d0
      if (p2(4).lt.ecut2) wt=0d0
      if (p3(4).lt.ecut3) wt=0d0
      if (p4(4).lt.ecut4) wt=0d0
c
c Cuts on min invariant masses of fermion pairs
c
      if (qadra(p1,p2).lt.scut12**2) wt=0d0
      if (qadra(p1,p3).lt.scut13**2) wt=0d0
      if (qadra(p1,p4).lt.scut14**2) wt=0d0
      if (qadra(p2,p3).lt.scut23**2) wt=0d0
      if (qadra(p2,p4).lt.scut24**2) wt=0d0
      if (qadra(p3,p4).lt.scut34**2) wt=0d0
c
c Cuts on max and min cos-theta of outgoing fermions (w.r.t. the beam ax
c
      if (cmax1.lt.1e0 .and.
     &    p1(3) .gt. dsqrt(p1(1)**2+p1(2)**2+p1(3)**2) * cmax1) wt=0d0
      if (cmax2.lt.1e0 .and.
     &    p2(3) .gt. dsqrt(p2(1)**2+p2(2)**2+p2(3)**2) * cmax2) wt=0d0
      if (cmax3.lt.1e0 .and.
     &    p3(3) .gt. dsqrt(p3(1)**2+p3(2)**2+p3(3)**2) * cmax3) wt=0d0
      if (cmax4.lt.1e0 .and.
     &    p4(3) .gt. dsqrt(p4(1)**2+p4(2)**2+p4(3)**2) * cmax4) wt=0d0
c
      if (cmin1.gt.-1e0 .and.
     &    p1(3) .lt. dsqrt(p1(1)**2+p1(2)**2+p1(3)**2) * cmin1) wt=0d0
      if (cmin2.gt.-1e0 .and.
     &    p2(3) .lt. dsqrt(p2(1)**2+p2(2)**2+p2(3)**2) * cmin2) wt=0d0
      if (cmin3.gt.-1e0 .and.
     &    p3(3) .lt. dsqrt(p3(1)**2+p3(2)**2+p3(3)**2) * cmin3) wt=0d0
      if (cmin4.gt.-1e0 .and.
     &    p4(3) .lt. dsqrt(p4(1)**2+p4(2)**2+p4(3)**2) * cmin4) wt=0d0
c
c Cuts on max cos-theta between outgoing fermions
c
      if (cmax12.lt.1e0.and.cos(angle(p1,p2)).gt.cmax12) wt=0d0
      if (cmax13.lt.1e0.and.cos(angle(p1,p3)).gt.cmax13) wt=0d0
      if (cmax14.lt.1e0.and.cos(angle(p1,p4)).gt.cmax14) wt=0d0
      if (cmax23.lt.1e0.and.cos(angle(p2,p3)).gt.cmax23) wt=0d0
      if (cmax24.lt.1e0.and.cos(angle(p2,p4)).gt.cmax24) wt=0d0
      if (cmax34.lt.1e0.and.cos(angle(p3,p4)).gt.cmax34) wt=0d0
c
      if (wt.eq.0d0) then
        nkwcte0=nkwcte0+1
        return
      endif
c
c 1. /KWCTE1/ cuts
c ----------------
c Cuts on max and min cos-theta of outgoing electrons (w.r.t. the beam a
c
      if (iflav(1).eq.11) then
        if (cmaxem.lt.1e0 .and.
     $      p1(3) .gt. dsqrt(p1(1)**2+p1(2)**2+p1(3)**2) * cmaxem)
     $    wt=0d0
        if (cminem.gt.-1e0 .and.
     $      p1(3) .lt. dsqrt(p1(1)**2+p1(2)**2+p1(3)**2) * cminem)
     $    wt=0d0
      endif
c
c Cuts on max and min cos-theta of outgoing positrons (w.r.t. the beam a
c
      if (iflav(4).eq.-11) then
        if (cmaxep.lt.1e0 .and.
     $      p4(3) .gt. dsqrt(p4(1)**2+p4(2)**2+p4(3)**2) * cmaxep)
     $    wt=0d0
        if (cminep.gt.-1e0 .and.
     $      p4(3) .lt. dsqrt(p4(1)**2+p4(2)**2+p4(3)**2) * cminep)
     $    wt=0d0
      endif
c
      if (wt.eq.0d0) then
        nkwcte1=nkwcte1+1
        return
      endif
c
c 2. /KWCTE2/ cuts
c ----------------
c Cuts only apply to e+nue~e-nue events
c
      if (iflav(1).eq.11 .and. iflav(4).eq.-11) then
c
c Cuts on energies of e- and e+
c
        if (p1(4).lt.enemee) wt=0d0
        if (p4(4).lt.enepee) wt=0d0
c
c Cut on invariant mass of e-/e+ system
c
        if (qadra(p1,p4).lt.scutee**2) wt=0d0
c
c Cut on sum of |pT(e+)| and |pT(e-)|
c
        if (dsqrt(p1(1)**2+p1(2)**2)+
     &      dsqrt(p4(1)**2+p4(2)**2).lt.ptsmee) wt=0d0
c
      endif
c
      if (wt.eq.0d0) then
        nkwcte2=nkwcte2+1
        return
      endif
c
c 3. /KWCTUU/ cuts
c ----------------
c Cuts only apply to uudd final states
c
      if (iflav(1).eq.1 .and. iflav(2).eq.-2 .and.
     &    iflav(3).eq.2 .and. iflav(4).eq.-1       ) then
c
c Cut on invariant mass of uudd system after ISR (s') in udud events
c
        if (qadra(qeff1,qeff2).lt.spctuu**2) wt=0d0
c
c Cuts on invariant masses of uu and dd systems
c
        if (qadra(p1,p4).lt.scutuu**2) wt=0d0
        if (qadra(p2,p3).lt.scutuu**2) wt=0d0
c
      endif
c
      if (wt.eq.0d0) then
        nkwctuu=nkwctuu+1
        return
      endif
c
      return
      end
      integer function KWGTBK(IS,IWEI,wei)
C--------------------------------------------------------------------
C!  BOOK and fill bank KWGT with weight info
C      B. Bloch -Devaux January 1999
C     structure : integer function
C
C     input     : IS   row number to be filled
C                 IWEI weight number to be stored
C                 WEI  weight value  to be stored
C     output    : index of KWGT bank ( should be >0 if OK)
C                 KWGT bank is written to Event list
C
C--------------------------------------------------------------------
C#ifndef DOC
C#include "bcs.h"
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C#include "kwgtjj.h"
      INTEGER JKWGWN,JKWGWV,LKWGTA
      PARAMETER(JKWGWN=1,JKWGWV=2,LKWGTA=2)
C#include "bmacro.h"
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
C--------------------------------------------------------------
C
      KWGTBK = -1
C
C   Get KWGT index
      JKWGT = IW(NAMIND('KWGT'))
      IF ( JKWGT.LE.0) THEN
C   Create KWGT bank
         CALL AUBOS('KWGT',0,LKWGTA+LMHLEN,JKWGT,IGARB)
         IF ( JKWGT.LE.0) GO TO 999
         IW(JKWGT+LMHCOL) = LKWGTA
         IW(JKWGT+LMHROW) = 1
         CALL BKFMT('KWGT','2I,(I,F)')
         CALL BLIST(IW,'E+','KWGT')
      ELSE
C  KWGT EXISTS, TEST THE LENGTH AND EXTEND IF NEEDED
         NKWGT=LROWS(JKWGT)
         IF ( IS.GT.NKWGT) THEN
           CALL AUBOS('KWGT',0,LKWGTA*IS+LMHLEN,JKWGT,IGARB)
           IF ( JKWGT.LE.0) THEN
              KWGTBK= -IS
              GO TO 999
           ELSE
              IW(JKWGT+LMHROW) = IS
           ENDIF
         ENDIF
      ENDIF
C  Fill KWGT BANK
      KKWGT = KROW(JKWGT,IS)
      IW(KKWGT+JKWGWN)   = IWEI 
      RW(KKWGT+JKWGWV)   = WEI 
      KWGTBK = JKWGT
C
 999  RETURN
      END
C#endif
      SUBROUTINE UGTSEC
C --------------------------------------------------------------------
C     B.Bloch vreate x-section bank January 1999
C --------------------------------------------------------------------
      PARAMETER ( IGCO = 5035 )
      PARAMETER ( IVER = 104  )
      COMMON / KGCOMM / ISTA,VRTEX(4),SDVRT(3),NEVENT(8),ECMI
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! communicates with gmonit
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      REAL *8 AVERWT,ERRELA
      REAL *8 xcrude,xcvesk,dumm1,DUMM2,DUMM3,XSBEST,ERBEST
C --------------------------------------------------------------------
! This is "pointer" for internal monitoring histograms/averages
      IDYFS = 0
!-- Initialization of QED part
      KeyISR = MOD(KeyRad,10)
      Key4f  = MOD(KeyMis,100)/10
      CALL karlud(1,xcrude,xcvesk,dumm1)
!-- best xsection, total, for output purposes only!
      IF( Key4f .EQ. 0 ) THEN
!  No 4fermion introduced
        IF( KeyISR .eq. 0) THEN
          CALL GMONIT(1,IDYFS+58,DUMM1,DUMM2,DUMM3)
        ELSE
          CALL GMONIT(1,IDYFS+78,DUMM1,DUMM2,DUMM3)
        ENDIF
      ELSEIF( Key4f .NE. 0 ) THEN
!  4fermion introduced
        CALL GMONIT(1,IDYFS+92,DUMM1,DUMM2,DUMM3)
      ENDIF
      XSBEST  = XCRUDE*AVERWT
      ERBEST  = XSBEST*ERRELA
C
C Print cross-section (MC and Semi-analytical calculations)
C ans store it in KSEC bank
      NTOT = NEVENT(1)
      XTOT = XSBEST/1000.
      RTOT = ERBEST/1000.
      IS = 1
      IDC = IGCO
      IVERS = IVER
      NACC = NTOT
      XACC = XTOT
      RACC = RTOT
      isec = KSECBK(IS,IDC,IVERS,NTOT,NACC,XTOT,RTOT,XACC,RACC)
      CALL PRTABL('KSEC',0)
      RETURN
      END