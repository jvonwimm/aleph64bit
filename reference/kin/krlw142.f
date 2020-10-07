*////////////////////////////////////////////////////////////////////////////////
*//   ======================================================================   //
*//   ======================================================================   //
*//   ===========================KoralW=====================================   //
*//   ======================WW pair production==============================   //
*//   ======================================================================   //
*//   ======================================================================   //
*//   ======================== Version 1.42 ================================   //
*//   ======================================================================   //
*//   ======================= September 1998 ===============================   //
*//   ======================================================================   //
*////////////////////////////////////////////////////////////////////////////////
*
*     Author list:
*          S. Jadach      (Stanislaw.Jadach@cern.ch)
*          W. Placzek     (Wieslaw.Placzek@cern.ch)
*          M. Skrzypek    (Maciej.Skrzypek@cern.ch)
*          B.F.L. Ward    (bflw@slac.stanford.edu)
*          Z. Was         (Zbigniew.Was@cern.ch)
*
*////////////////////////////////////////////////////////////////////////////////
*// Principal entries:                                                         //
*//   CALL KW_ReaDataX("./data_DEFAULTS", 1, 10000, xpar)  ! read defaults     //
*//   CALL KW_ReaDataX("./user.input",    0, 10000, xpar)  ! read user input   //
*//   CALL KW_Initialize(xpar)           ! Initialization                      //
*//   CALL KW_Make                       ! Generate one event                  //
*//   CALL KW_Finalize                   ! Final bookkeeping/printouts         //
*//   CALL KW_GetXSecMC(XSecMC,XErrMC)   ! Get MC total xsection [pb]          //
*//   CALL KW_GetNevMC(NevMC)            ! Get no. of MC events                //
*// For more information see:                                                  //
*//   Complete documentation:            LongWriteUp,                          //
*//   Highlights:                        README file                           //
*//   Differences with previous vers.:   RELEASE.NOTES                         //
*//   How to use the program:            Example of the main program KWdemo.f  //
*//   All possible Input data:           Data_DEFAULTS file                    //
*////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE KW_Initialize(xpar_input)
*////////////////////////////////////////////////////////////////////////////////
*//                                                                            //
*//   Compulsory initialization of KoralW Mote Carlo Generator                 //
*//                                                                            //
*////////////////////////////////////////////////////////////////////////////////
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z)
      SAVE
*
      INTEGER     m_Npar
      REAL*8      m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL*8      m_Xcrude,  m_svar,      m_alpha_s,   Preco,    Wtu
      REAL*8      m_XSecMC,  m_XErrMC
      REAL*8      m_XSecNR,  m_XErrNR
      INTEGER     m_KeyWgt,  m_KeySmp,    m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru,  IevAcc,      NevTot
      INTEGER     m_i_4f,    m_i_beta
*
      COMMON   /c_KW/
     $  m_Npar( 1000),           ! old Npar local Temporary/Obsolete
     $  m_Xpar(10000),           ! Xpar local
     $  m_XSecMC,                ! Final Main Best Monte Carlo Cross Section [pb]
     $  m_XErrMC,                ! and its error [pb]
     $  m_XSecNR,                ! Normalization Carlo Cross Section [pb]
     $  m_XErrNR,                ! and its error [pb]
     $  m_svar,                  ! CMSene**2
     $  m_alpha_s,               ! Alpha strong
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta                 ! Monitoring
*
      SAVE    /c_KW/
CBB      INCLUDE   "KW.h"
      REAL*8  xpar_input( 10000) ! in main program
*////////////////////////////////////////////////////////////////////////////////
*//    Common blocks sorted in alphabetic order!!!!                            //
*//    They shoud GET OUT of here.                                             //
*////////////////////////////////////////////////////////////////////////////////
*   --Anomalous WWV Couplings, for WWamgc only
      DOUBLE COMPLEX    g1,kap,lam,g4,g5,kapt,lamt
      COMMON / ancoco / g1(2),kap(2),lam(2),g4(2),g5(2),kapt(2),lamt(2)
      COMMON / articut/ arbitr,arbitr1,themin,arbitr2  !   <-- cuts for selecto
      COMMON / bxfmts / bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g 
      CHARACTER*80      bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g       
      CHARACTER*80      bxl2c      
      COMMON / cumask / user_umask(202) ! user mask on final states
      COMMON / decdat / amafin(20), br(20)
      COMMON / inout  / ninp,nout     
      COMMON / KeyKey / KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      COMMON / libra  / jak1,jak2,itdkrc,ifphot,ifhadm,ifhadp 
                        ! TAUOLA, PHOTOS and JETSET
      COMMON / matpar / pi,ceuler     
      COMMON / phypar / alfinv,gpicob     
      COMMON / ranpar / Keyrnd
      COMMON / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf  
      COMMON / wekin2 / amaw,gammw,gmu,alphaw
* /wekin3/ params of non-established particles such as higgs 
* (to be activated by Ihig)
      COMMON / wekin3 / amhig,gamhig,Ihig
      COMMON / wgtall / wtcrud,wtmod,wtset(100) ! this common should go away
      COMMON / wt_max / wtmax,wtmax_cc03     
      COMMON / vvrec  / vvmin,vvmax,vv,beti                   
*//////////////////////////////////////////////////////////////////////
*//    Other variables and data statements                           //
*//////////////////////////////////////////////////////////////////////
      DIMENSION wt4f(9)         !<-- 4fermion weights
!-- The CKM mixing matrix and VV+ which should be =1 (V - unitary) 
      DIMENSION VCKM(3,3),VVH(3,3)
*//////////////////////////////////////////////////////////////////////
      CHARACTER*64 Logo(44)
      DATA Logo /
     $'***************************************************************',
     $'***************************************************************',
     $'***************************************************************',
     $'*  ###   ###                                   ###       ###  *',
     $'*  ###  ###  ####    ######      ##     ##     ###       ###  *',
     $'*  ### ###  ##  ##   ##   ##    ####    ##     ###       ###  *',
     $'*  ######  ##    ##  ##   ##   ##  ##   ##     ###       ###  *',
     $'*  ######  ##    ##  #####    ##    ##  ##     ###   #   ###  *',
     $'*  ### ###  ##  ##   ##  ##   ########  ##      ### ### ###   *',
     $'*  ###  ###  ####    ##   ##  ##    ##  #######  #### ####    *',
     $'*  ###   ###            version 1.42              ##   ##     *',
     $'***************************************************************',
     $'********************** September 1998 *************************',
     $'***************************************************************',
     $'               Last modification: 16.10.1998                   ',
     $'***************************************************************',
     $'*  Written by:                                                *',
     $'*    S. Jadach      (Stanislaw.Jadach@cern.ch)                *',
     $'*    W. Placzek     (Wieslaw.Placzek@cern.ch)                 *',
     $'*    M. Skrzypek    (Maciej.Skrzypek@cern.ch)                 *',
     $'*    B.F.L. Ward    (bflw@slac.stanford.edu)                  *',
     $'*    Z. Was         (Zbigniew.Was@cern.ch)                    *',
     $'*  Papers:                                                    *',
     $'*    M. Skrzypek, S. Jadach, W. Placzek, Z. Was               *',
     $'*      CERN-TH/95-205, Jul 1995, CPC 94 (1996) 216            *',
     $'*    M. Skrzypek, S. Jadach, M. Martinez, W. Placzek, Z. Was  *',
     $'*      CERN-TH/95-246, Sep 1995, Phys. Lett. B372 (1996) 289  *',
     $'*    S. Jadach, W. Placzek, M. Skrzypek, B.F.L. Ward, Z. Was  *',
     $'*   CERN-TH/98-242, UTHEP-98-0702, Jul 1998, submitted to CPC *',
     $'*  Related papers:                                            *',
     $'*    T. Ishikawa, Y. Kurihara, M. Skrzypek, Z. Was            *',
     $'*      CERN-TH/97-11, Jan 1997, Eur. Phys. J. C4 (1998) 75    *',
     $'*    S. Jadach, K. Zalewski                                   *',
     $'*    CERN-TH/97-29, Jan 1997, Acta Phys. Pol. B28 (1997) 1363 *',
     $'*  WWW:                                                       *',
     $'*    http://hpjmiady.ifj.edu.pl/                              *',
     $'*  Acknowledgements:                                          *',
     $'*    We acknowledge warmly very useful help of:               *',
     $'*      M. Martinez in testing versions 1.01 and 1.02,         *',
     $'*      M. Gruenewald and A. Valassi in testing version 1.21   *',
     $'*      S. Jezequel in testing versions 1.31-1.33              *',
     $'*      M. Witek in testing version 1.41                       *',
     $'***************************************************************',
     $' '/ 

* ...BX-formats for nice and flexible outputs                 
      BXOPE =  '(//1X,15(5H*****)    )'     
      BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'   
      BXL1I =  '(1X,1H*,I17,                 16X, A20,A12,A7, 1X,1H*)'
      BXL1F =  '(1X,1H*,F17.8,               16X, A20,A12,A7, 1X,1H*)' 
      BXL2F =  '(1X,1H*,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXL2C ='(1X,1H*,1H(,F14.8,3H +i,F13.7,1H),1X,A20,A12,A7,1X,1H*)'
      BXL1G =  '(1X,1H*,G17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2G =  '(1X,1H*,G17.8, 4H  +-, G11.5, 1X, A20,A12,A7, 1X,1H*)'
      BXCLO =  '(1X,15(5H*****)/   )'       
*///////////////////////////////////////////////////////////
*//   Math. constants, should go to PARAMETER statement
      Pi=3.1415926535897932D0
      CEuler = 0.57721566D0                  
*////////////////////////////////////////////////////////////////////////////
*// xpar should be  essentialy an image of the input in the main program   //
*// In the present version it also plays role of additional "common block" //
*// communicating between subprograms, vide filexp.f and setmas_koralw.f   //
*// In order to split this double role I introduce xpar_input which is not //
*// modified (see exception below)  and local xpar which acts as additional//
*// address area residing in THIS program and sent as a pointer downwards. //
*// Now the main program (including Korwan) knows nothing about changes    //
*// in local the xpar done in filexp.                                      //
*// Exception is that xpar_input sends information outside for mode=1,2.   //
*// This role should disappear, and final XsecMC should have its "getter"  //
*// Of course, xpar_input is copied into xpar, see below:                  //
*////////////////////////////////////////////////////////////////////////////
      DO i = 1, 10000
         m_Xpar(i) = xpar_input(i)
      ENDDO
*////////////////////////////////////////////////////////////////
*//                                                            //
*//  m_Npar is now pure internal object (obsolete)             //
*//  below we translate m_Xpar--> m_Npar fot internal use      //
*//                                                            //
*////////////////////////////////////////////////////////////////
*     KeyRad = 1000*KeyCul +100*KeyNLL +10*KeyFSR +KeyISR
      m_Npar(1)= 
     $           +NINT(m_Xpar(1011))  ! KeyISR
     $        +10*NINT(m_Xpar(1012))  ! KeyFSR
     $       +100*NINT(m_Xpar(1013))  ! KeyNLL
     $      +1000*NINT(m_Xpar(1014))  ! KeyCul
      KeyRad = m_Npar(1)
*----------------------------------------------------------------
*     KeyPhy = 100000*KeyWu +10000*KeyRed +1000*KeySpn+100*KeyZet+10*KeyMas+KeyBra 
      m_Npar(2)= 
     $           +NINT(m_Xpar(1021))  ! KeyBra 
     $        +10*NINT(m_Xpar(1022))  ! KeyMas
     $       +100*NINT(m_Xpar(1023))  ! KeyZet
     $      +1000*NINT(m_Xpar(1024))  ! KeySpn
     $     +10000*NINT(m_Xpar(1025))  ! KeyRed
     $    +100000*NINT(m_Xpar(1026))  ! KeyWu
      KeyPhy = m_Npar(2)
*----------------------------------------------------------------
*     KeyTek = 100*KeySmp +10*KeyRnd +KeyWgt
      m_Npar(3)= 
     $           +NINT(m_Xpar(1031))  ! KeyWgt
     $        +10*NINT(m_Xpar(1032))  ! KeyRnd
     $       +100*NINT(m_Xpar(1033))  ! KeySmp
      KeyTek = m_Npar(3)
*----------------------------------------------------------------
*     KeyMis = 10000*KeyWon +1000*KeyZon+100*KeyAcc+10*Key4f +KeyMix
      m_Npar(4)= 
     $           +NINT(m_Xpar(1041))  ! KeyMix
     $        +10*NINT(m_Xpar(1042))  ! Key4f
     $       +100*NINT(m_Xpar(1043))  ! KeyAcc
     $      +1000*NINT(m_Xpar(1044))  ! KeyZon
     $     +10000*NINT(m_Xpar(1045))  ! KeyWon
      KeyMis = m_Npar(4)
*----------------------------------------------------------------
*     Other Keys
      m_Npar( 5)= NINT(m_Xpar(1055))  ! KeyDWM
      m_Npar( 6)= NINT(m_Xpar(1056))  ! KeyDWP
      m_Npar( 7)= NINT(m_Xpar(1057))  ! Nout
*
      m_Npar(21)= NINT(m_Xpar(1071))  ! JAK1
      m_Npar(22)= NINT(m_Xpar(1072))  ! JAK2
      m_Npar(23)= NINT(m_Xpar(1073))  ! ITDKRC
      m_Npar(24)= NINT(m_Xpar(1074))  ! IFPHOT
      m_Npar(25)= NINT(m_Xpar(1075))  ! IFHADM
      m_Npar(26)= NINT(m_Xpar(1076))  ! IFHADP
*----------------------------------------------------------------
* Umask
      DO i=101,302
         m_Npar(i)=NINT(m_Xpar(i+1000))
      ENDDO
*///////////////////////////////////////////////////////////
*//         end of translation m_Xpar-->m_Npar            //
*///////////////////////////////////////////////////////////
*-----------------------------------------------------------------------
* Below this line Npar Replaced with Xpar wherever possible!!!
*-----------------------------------------------------------------------
* these are realy used in KW class
      m_KeyIsr =NINT(m_Xpar(1011))
      m_KeyWgt =NINT(m_Xpar(1031))
      m_Key4f  =NINT(m_Xpar(1042))
      m_KeyAcc =NINT(m_Xpar(1043))
      m_KeySmp =NINT(m_Xpar(1033))
* for printout only, to be shifted to other classes
      KeyFSR =NINT(m_Xpar(1012))
      KeyNLL =NINT(m_Xpar(1013))
      KeyCul =NINT(m_Xpar(1014))
      KeyBra =NINT(m_Xpar(1021))
      KeyMas =NINT(m_Xpar(1022))
      KeyZet =NINT(m_Xpar(1023))
      KeySpn =NINT(m_Xpar(1024))
      KeyRed =NINT(m_Xpar(1025))
      KeyWu  =NINT(m_Xpar(1026))
      KeyRnd =NINT(m_Xpar(1032))
      KeyMix =NINT(m_Xpar(1041))
      KeyZon =NINT(m_Xpar(1044))
      KeyWon =NINT(m_Xpar(1045))
      KeyDWM =NINT(m_Xpar(1055))
      KeyDWP =NINT(m_Xpar(1056))
*-----------------------------------------------------------------------
* To be mooved to data???
* Higgs pre-sampler dipswitch
      IHIG=0
*-----------------------------------------------------------------------
      IF((KeyWon*KeyZon*(KeyDWP+KeyDWM) .NE. 0) .OR.
     $   (KeyWon .EQ. 0 .AND. KeyZon.eq.0       )     ) THEN
          WRITE(6,*) 'FILEXP==> inconsistent input: '
          WRITE(6,*) 'KeyWon=',KeyWon,'KeyZon=',KeyZon
          WRITE(6,*) 'KeyDWP=',KeyDWP,'KeyDWM=',KeyDWM
          STOP
      ENDIF
*///////////////////////////////////////////////////////////
*//            Loading Common blocks, cont.               //                  
*///////////////////////////////////////////////////////////
      amel    =m_Xpar(100)
      AlfInv  =m_Xpar(101)
      Gpicob  =m_Xpar(102)
      Nout   = NINT(m_xpar(1057))
      Jak1   = NINT(m_Xpar(1071))
      Jak2   = NINT(m_Xpar(1072))
      itdkrc = NINT(m_Xpar(1073))
      ifphot = NINT(m_Xpar(1074))
      ifhadm = NINT(m_Xpar(1075))
      ifhadp = NINT(m_Xpar(1076))

      cmsene = m_Xpar(1)      
      gmu    = m_Xpar(2)   
      alfwin = m_Xpar(3)
      amaz   = m_Xpar(4)
      gammz  = m_Xpar(5)
      amaw   = m_Xpar(6)
      gammw  = m_Xpar(7)
      vvmin  = m_Xpar(8)
      vvmax  = m_Xpar(9)
      wtmax  = m_Xpar(10)
      amhig  = m_Xpar(11)
      gamhig = m_Xpar(12)
      m_alpha_s= m_Xpar(13)
      arbitr = m_Xpar(14)
      arbitr1= m_Xpar(15)
      themin = m_Xpar(16)
      arbitr2= m_Xpar(17)
      wtmax_cc03= m_Xpar(18)
      PReco  = m_Xpar(19)
      ene    = CMSene/2d0      
*????????????????????????????????????????????????????????
* ?????????????  too small for yfs3 !!!!!!!!!!!!!!!!!!!!!
*????????????????????????????????????????????????????????
      vvmax  = MIN( vvmax, 1d0-(amel/ene)**2 )                   
c?????????????????????????????????????????????
c re-used in KoralW
      m_Xpar(9) =VVMAX ! send it back !!!
c?????????????????????????????????????????????

*/////////////////////////////////////////////////////////////////////////
*//               If arbitr2=<0 reset it to the maximum value           //
      IF (arbitr2 .LE. 0d0) THEN
        arbitr2 = cmsene**2
c?????????????????????????????????????????????
c seems to be unused
c        m_Xpar(17) = arbitr2
c?????????????????????????????????????????????
      ENDIF
*/////////////////////////////////////////////////////////////////////////
*//           Is this realy used in Karlud???                           //
      IDE=2               
      IDF=2               
      XK0=3.D-3         
*/////////////////////////////////////////////////////////////////////////
*//                       users mask                                    //
      DO i=1,202
         user_umask(i)= NINT(m_Xpar(1100+i))
      ENDDO
*/////////////////////////////////////////////////////////////////////////
*//                         alpha_w                                     //
      alphaw = 1d0/ AlfWin
*/////////////////////////////////////////////////////////////////////////
*//         Electroweak renormalisation schemes                         //
*/////////////////////////////////////////////////////////////////////////
      IF(KeyMix .EQ. 2) THEN
* this option is turned into 'bare Born' 
* so, we reset ALFWIN to alfa_QED
         SINW2 = 1D0 -AMAW**2/AMAZ**2
         ALFWIN = alfinv
c??????????????????????????????????????
c re-used in setmas_koralw.f
         m_Xpar(3) = alfwin
c??????????????????????????????????????
         ALPHAW = 1D0/ ALFWIN
      ELSEIF(KeyMix .EQ. 1) THEN
!.. this option is turned into G_mu scheme, 
!.. so, we recalculate ALFWIN
         SINW2 = 1D0 -AMAW**2/AMAZ**2
         ALFWIN = pi/( sqrt(2d0)*gmu*amaw**2*sinw2 )
c??????????????????????????????????????
c re-used in setmas_koralw.f
         m_Xpar(3) = alfwin
c??????????????????????????????????????
         ALPHAW = 1D0/ ALFWIN
      ELSE 
* LEP2 workshop definition
         sinw2 = pi * alphaw /( sqrt(2d0) * amaw**2 * gmu )
      ENDIF
*/////////////////////////////////////////////////////////////////////////
*//            cuts for selecto removed for CC03                        //
*/////////////////////////////////////////////////////////////////////////
      IF (m_Key4f .EQ. 0) THEN
* no cuts for CC03 presampler
        arbitr = 0d0  !  min. vis p_t**2 
        arbitr1= 0d0  !  add. cut for e+e-ch+ 
        themin = 0d0  !  min. theta [rad] with beam   
        arbitr2= cmsene**2  !  max p_t**2 of photons for e+e-ch+ 
c????????????????????????????????????????
c seems to be unused
c        m_Xpar(14)=arbitr    
c        m_Xpar(15)=arbitr1   
c        m_Xpar(16)=themin    
c        m_Xpar(17)=arbitr2  
c????????????????????????????????????????
      ENDIF
*/////////////////////////////////////////////////////////////////////////
*//             alpha_s/pi for naive QCD corrections                    //
*/////////////////////////////////////////////////////////////////////////
      aspi = m_alpha_s/pi
*/////////////////////////////////////////////////////////////////////////
*//                                                                     //
*//           Branching ratios for W decay channels:                    //
*//                                                                     //
*/////////////////////////////////////////////////////////////////////////
      IF(  KeyBra .EQ. 0 )THEN
*/////////////////////////////////////////////////////////////////////////
*//                    Born values                                      //
*/////////////////////////////////////////////////////////////////////////
         BR(1)=(1D0/3D0)*(1D0+aspi)/(1D0+2D0/3D0*aspi) !  <== ud
         BR(2)=0D0                                     !  <== cd
         BR(3)=0D0                                     !  <== us
         BR(4)=(1D0/3D0)*(1D0+aspi)/(1D0+2D0/3D0*aspi) !  <== cs
         BR(5)=0D0                                     !  <== ub
         BR(6)=0D0                                     !  <== cb
         BR(7)=(1D0/9D0)           /(1D0+2D0/3D0*aspi) !  <== e
         BR(8)=(1D0/9D0)           /(1D0+2D0/3D0*aspi) !  <== mu
         BR(9)=(1D0/9D0)           /(1D0+2D0/3D0*aspi) !  <== tau
      ELSEIF(  KeyBra .EQ. 1 )THEN
*/////////////////////////////////////////////////////////////////////////
*//          Values of CKM and BRanchings for KeyBra = 1                //
*// note that these br ratios correspond to alfa_s=0.12 (gamma_W->el    //
*// constant) and to nontrivial CKM matrix simultaneously               //
*// this is 'bullet proof' default setting                              //
*/////////////////////////////////////////////////////////////////////////
         m_alpha_s = 0.12d0  ! make sure alpha_s is consistent
c??????????????????????????????????????????????
c re-used in setmas_koralw.f and KoralW
         m_Xpar(13)=m_alpha_s  ! <== send it back
c??????????????????????????????????????????????
         aspi = m_alpha_s/pi
         gammw=-1d0        ! make sure W width will be recalculated
         DO i=1,9
            BR(i) = m_Xpar(130 +i)
         ENDDO
      ELSEIF(  KeyBra .EQ. 2 )THEN
*/////////////////////////////////////////////////////////////////////////
*//              Default values of CKM and BRanchings                   //
*// Recalculate br. ratios from the CKM matrix and alpha_s according to //
*// theoretical formula of A. Denner, Fortschr. Phys. 41 (1993) 307.    //
*// Values of the CKM matrix elements from 1996 PDG Review:             //
*//  http://www-pdg.lbl.gov/pdg.html (mean values of given ranges)      //
*/////////////////////////////////////////////////////////////////////////
         VCKM(1,1) =m_Xpar(111)   ! V_ud  real part
         VCKM(1,2) =m_Xpar(112)   ! V_us  real part
         VCKM(1,3) =m_Xpar(113)   ! V_ub  real part
         VCKM(2,1) =m_Xpar(114)   ! V_cd  real part
         VCKM(2,2) =m_Xpar(115)   ! V_cs  real part
         VCKM(2,3) =m_Xpar(116)   ! V_cb  real part
         VCKM(3,1) =m_Xpar(117)   ! V_td  real part
         VCKM(3,2) =m_Xpar(118)   ! V_ts  real part
         VCKM(3,3) =m_Xpar(119)   ! V_tb  real part
* Unitarity check of the CKM matrix: VVH should be =1
         DO i = 1,3
         DO j = 1,3
           sum = 0d0
           DO k = 1,3
             sum = sum + VCKM(i,k)*VCKM(j,k)
           ENDDO
           VVH(i,j) = sum
         ENDDO
         ENDDO
* IBA formulae for branching ratios
         brlep = 1d0/9d0/(1 + 2d0/3d0*aspi)
         brqua = 3*brlep*(1 + aspi)
         BR(1) = VCKM(1,1)**2 *brqua  !  <== ud
         BR(2) = VCKM(2,1)**2 *brqua  !  <== cd
         BR(3) = VCKM(1,2)**2 *brqua  !  <== us
         BR(4) = VCKM(2,2)**2 *brqua  !  <== cs
         BR(5) = VCKM(1,3)**2 *brqua  !  <== ub
         BR(6) = VCKM(2,3)**2 *brqua  !  <== cb
         BR(7) = brlep                !  <== e
         BR(8) = brlep                !  <== mu
         BR(9) = brlep                !  <== tau  
* make sure W width will be recalculated       
         gammw =-1d0        
      ELSE
        WRITE(6,*)'filexp=> wrong KeyBra: ',Keybra
        STOP
      ENDIF
*///////////////////////////////////////////////////////////
*//  Check if requested final state has a ZERO br. ratio  //
      IF(KeyWon.NE.0 .AND. KeyZon.EQ.0) THEN
        IF(Keydwm.NE.0 .AND. Keydwp.NE.0 .AND. Keydwp.NE.Keydwm) THEN
          IF(br(Keydwm) .EQ.0d0 .OR. br(Keydwp) .EQ. 0d0 ) THEN
           WRITE(6,*)'filexp=> requested CKM-nondiagonal WW final state'
           WRITE(6,*)'has zero xsect if used with br. ratios specified'
           STOP
          ENDIF
        ENDIF
      ENDIF
*/////////////////////////////////////////////////////////////////////////
*//             W width recalculated on request                         //
*/////////////////////////////////////////////////////////////////////////
      IF ( gammw .LE. 0d0 ) THEN
         gwc  =  9d0 * Gmu * amaw**2 /( 6d0 * sqrt(2d0) * pi)
         gammw = amaw * gwc
*-- Naive QCD correction to the width
         gammw=gammw*(1D0+2D0/3D0*ASPI) 
c????????????????????????????????????????????
c re-used in setmas_koralw.f
         m_Xpar(7) = GAMMW  ! send it back !!!
c????????????????????????????????????????????
      ENDIF
*///////////////////////////////////////////////////////////////////
*//               final fermions masses                           //
*///////////////////////////////////////////////////////////////////
      DO i = 1,6
         amafin(   i) = m_Xpar(500+i*10 +6) ! leptons
         amafin(10+i) = m_Xpar(600+i*10 +6) ! quarks
      ENDDO
      amel   = amafin(11)       ! <---now probably not necessary ?????
      IF(  KeyMas .EQ. 0 ) THEN
         DO i = 1,6
            amafin(   i) = 0d0
            amafin(10+i) = 0d0
         ENDDO
      ENDIF
      IF(wtmax.LE.0d0) THEN
        wtmax=2d0
      ENDIF
      IF(wtmax_cc03 .LE. 0d0) THEN
        wtmax_cc03 = m_Xpar(151)
        IF(cmsene.GT.162) wtmax_cc03 = m_Xpar(152)
        IF(cmsene.GT.175) wtmax_cc03 = m_Xpar(153)
        IF(cmsene.GT.200) wtmax_cc03 = m_Xpar(154)
        IF(cmsene.GT.250) wtmax_cc03 = m_Xpar(155)
        IF(cmsene.GT.350) wtmax_cc03 = m_Xpar(156)
        IF(cmsene.GT.700) wtmax_cc03 = m_Xpar(157)
c?????????????????????????????????????????????
c seems to be unused
c        m_Xpar(18) = wtmax_cc03   ! send it back !!!
c?????????????????????????????????????????????
      ENDIF
!-- if WW-CC03 matrix el. is requested, use wtmax_cc03 instead of wtmax
      IF(m_Key4f .EQ. 0) THEN
        wtmax=wtmax_cc03
c?????????????????????????????????????????????
c seems to be unused
c        m_Xpar(10) = wtmax   ! send it back !!!
c?????????????????????????????????????????????
      ENDIF
*
      WRITE(6,   '(10X,A)') Logo
      WRITE(NOUT,'(10X,A)') Logo

      WRITE(NOUT,BXOPE)         
      WRITE(NOUT,BXTXT) '           KORALW input parameters used    '
      WRITE(NOUT,BXL1F) CMSENE,     'CMS energy total   ','CMSENE','I.0'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyRad,     'QED super-switch   ','KeyRad','IQ1'
      WRITE(NOUT,BXL1I) m_KeyIsr,   'Init. state Rad.   ','KeyISR','IQ2'
      WRITE(NOUT,BXL1I) KeyFSR,     'Final state Rad.   ','KeyFSR','IQ3'
      WRITE(NOUT,BXL1I) KeyNLL,     'Next. To Leading   ','KeyNLL','IQ4'
      WRITE(NOUT,BXL1I) KeyCul,     'Coulomb corr.      ','KeyCul','IQ5'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyPhy,     'Physics super-switc','KeyPhy','IP1'
      WRITE(NOUT,BXL1I) KeyRed,     'FS mass reduction  ','KeyRed','IP2'
      WRITE(NOUT,BXL1I) KeySpn,     'Spin in W decays   ','KeySpn','IP3'
      WRITE(NOUT,BXL1I) KeyZet,     'Z propag.          ','KeyZet','IP4'
      WRITE(NOUT,BXL1I) KeyMas,     'Mass kinematics.   ','KeyMas','IP5'
      WRITE(NOUT,BXL1I) KeyBra,     'Branching Rat.     ','KeyBra','IP6'
      WRITE(NOUT,BXL1I) KeyWu,      'W propag.          ','KeyWu ','IP7'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyTek,     'Technical super-swi','KeyTek','IT1'
      WRITE(NOUT,BXL1I) m_KeySmp,   'presampler type    ','KeySmp','IT2'
      WRITE(NOUT,BXL1I) KeyRnd,     'rand Numb type     ','KeyRnd','IT3'
      WRITE(NOUT,BXL1I) m_KeyWgt,   'weighting  switch  ','KeyWgt','IT4'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyMis,     'Miscelaneous       ','KeyMis','IM1'
      WRITE(NOUT,BXL1I) KeyMix,     'sinW2 input type   ','KeyMix','IM2'
      WRITE(NOUT,BXL1I) m_Key4f,    '4 fermion matr el  ','Key4f ','IM3'
      WRITE(NOUT,BXL1I) m_KeyAcc,   'Anomalous couplings','KeyAcc','IM4'
      WRITE(NOUT,BXL1I) KeyWon,     'WW type final state','KeyWon','IM5'
      WRITE(NOUT,BXL1I) KeyZon,     'ZZ type final state','KeyZon','IM6'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KEYDWM,     'W-/Z decay mode    ','KEYDWM','ID1'
      WRITE(NOUT,BXL1I) KEYDWP,     'W+/Z decay mode    ','KEYDWP','ID2'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1F) GMU*1d5,    'G_mu * 1d5         ','GMU   ','I.1'
      WRITE(NOUT,BXL1F) ALFWIN,     'inv alpha_w        ','ALFWIN','I.2'
      WRITE(NOUT,BXL1F) AMAZ,       'Z mass   [GeV]     ','AMAZ  ','I.3'
      WRITE(NOUT,BXL1F) GAMMZ,      'Z width  [GeV]     ','GAMMZ ','I.4'
      WRITE(NOUT,BXL1F) AMAW,       'W mass   [GeV]     ','AMAW  ','I.5'
      WRITE(NOUT,BXL1F) GAMMW,      'W width  [GeV]     ','GAMMW ','I.6'
      WRITE(NOUT,BXL1F) VVMIN,      'dummy infrared cut ','VVMIN ','I.7'
      WRITE(NOUT,BXL1F) VVMAX,      'v_max ( =1 )       ','VVMAX ','I.8'
      WRITE(NOUT,BXL1F) WTMAX,      'max wt for rejectn.','WTMAX ','I.9'
      WRITE(NOUT,BXL1F) WTMAX_CC03, 'max wt for CC03 rej','WTMAX ','I10'
      WRITE(NOUT,BXL1F) m_alpha_s,  'alpha_s: QCD coupl.','ALPHAS','I11'
      WRITE(NOUT,BXL1F) PReco  ,    'Color Re-Con. Prob.','PReco ','I12'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1F) SINW2,      'sin(theta_W)**2    ','SINW2 ','I13'
      WRITE(NOUT,BXTXT)'***********************************************'
*!-----------------------------------------------------------------------
      WRITE(NOUT,BXTXT)'***********************************************'
      IF(Keyzet.eq.0) THEN 
        WRITE(NOUT,BXTXT) '  Z width in Z propagator: s/M_Z *GAMM_Z '
      ELSEIF(Keyzet.eq.1) THEN 
        WRITE(NOUT,BXTXT) '  Z width in Z propagator:   M_Z *GAMM_Z '
      ELSEIF(Keyzet.eq.2) THEN 
        WRITE(NOUT,BXTXT) '  Z width in Z propagator:   0           '
      ELSE
        WRITE(NOUT,BXTXT) '  FILEXP ==> wrong KeyZET =',Keyzet
        STOP
      ENDIF
      WRITE(NOUT,BXTXT)'***********************************************'
      IF(Keyspn.ne.1) THEN 
        WRITE(NOUT,BXTXT) '         WARNING!  spin in decays is OFF: '
        WRITE(NOUT,BXL1I) KeySPN, 'spin in decays switch','KeySPN','A13'
      WRITE(NOUT,BXTXT)'***********************************************'
      ENDIF
      IF (KeyBra.EQ.2) THEN
       WRITE(NOUT,BXTXT) '                                    '
       WRITE(NOUT,BXTXT) '                CKM matrix elements:'
       WRITE(NOUT,BXL1F) VCKM(1,1),       'V_ud','VCKM(1,1)','IV1'
       WRITE(NOUT,BXL1F) VCKM(1,2),       'V_us','VCKM(1,2)','IV2'
       WRITE(NOUT,BXL1F) VCKM(1,3),       'V_ub','VCKM(1,3)','IV3'
       WRITE(NOUT,BXL1F) VCKM(2,1),       'V_cd','VCKM(2,1)','IV4'
       WRITE(NOUT,BXL1F) VCKM(2,2),       'V_cs','VCKM(2,2)','IV5'
       WRITE(NOUT,BXL1F) VCKM(2,3),       'V_cb','VCKM(2,3)','IV6'
       WRITE(NOUT,BXL1F) VCKM(3,1),       'V_td','VCKM(3,1)','IV7'
       WRITE(NOUT,BXL1F) VCKM(3,2),       'V_ts','VCKM(3,2)','IV8'
       WRITE(NOUT,BXL1F) VCKM(3,3),       'V_tb','VCKM(3,3)','IV9'
       WRITE(NOUT,BXTXT)
     $          '              Unitarity check of the CKM matrix:'
       WRITE(NOUT,'(1X,1H*,20X,3f10.3,23X,1H*)')(VVH(1,j),j=1,3)
       WRITE(NOUT,'(1X,1H*,15X,5HVV+ =,3f10.3,23X,1H*)')(VVH(2,j),j=1,3)     
       WRITE(NOUT,'(1X,1H*,20X,3f10.3,23X,1H*)')(VVH(3,j),j=1,3)
      ENDIF
      WRITE(NOUT,BXTXT) '                                             '
      WRITE(NOUT,BXTXT) '                Branching ratios of W decays:'
      WRITE(NOUT,BXL1F) BR(1),            'ud','BR(1)','IB1'
      WRITE(NOUT,BXL1F) BR(2),            'cd','BR(2)','IB2'
      WRITE(NOUT,BXL1F) BR(3),            'us','BR(3)','IB3'
      WRITE(NOUT,BXL1F) BR(4),            'cs','BR(4)','IB4'
      WRITE(NOUT,BXL1F) BR(5),            'ub','BR(5)','IB5'
      WRITE(NOUT,BXL1F) BR(6),            'cb','BR(6)','IB6'
      WRITE(NOUT,BXL1F) BR(7),            ' e','BR(7)','IB7'
      WRITE(NOUT,BXL1F) BR(8),           ' mu','BR(8)','IB8'
      WRITE(NOUT,BXL1F) BR(9),          ' tau','BR(9)','IB9'

      WRITE(NOUT,BXTXT) '                              fermion masses:'
      WRITE(NOUT,BXL1F) AMAFIN(1),     ' d','AMAFIN(1)','IM1'
      WRITE(NOUT,BXL1F) AMAFIN(2),     ' u','AMAFIN(2)','IM2'
      WRITE(NOUT,BXL1F) AMAFIN(3),     ' s','AMAFIN(3)','IM3'
      WRITE(NOUT,BXL1F) AMAFIN(4),     ' c','AMAFIN(4)','IM4'
      WRITE(NOUT,BXL1F) AMAFIN(5),     ' b','AMAFIN(5)','IM5'
      WRITE(NOUT,BXL1F) AMAFIN(11),    ' e','AMAFIN(11)','IM6'
      WRITE(NOUT,BXL1F) AMAFIN(12),    've','AMAFIN(12)','IM7'
      WRITE(NOUT,BXL1F) AMAFIN(13),    'mu','AMAFIN(13)','IM8'
      WRITE(NOUT,BXL1F) AMAFIN(14),   'vmu','AMAFIN(14)','IM9'
      WRITE(NOUT,BXL1F) AMAFIN(15),   'tau','AMAFIN(15)','IM10'
      WRITE(NOUT,BXL1F) AMAFIN(16),  'vtau','AMAFIN(16)','IM11'
      WRITE(NOUT,BXTXT) '                                             '
      IF (m_KeySmp.NE.0) THEN
        WRITE(NOUT,BXTXT) ' Predefined cuts on final state fermions'
        WRITE(NOUT,BXL1F)arbitr, 'min. vis p_t**2    ','GeV^2','X2'
        WRITE(NOUT,BXL1F)arbitr1,'add. cut for e+e-ch+ch-','GeV^2','X3'
        WRITE(NOUT,BXL1G)themin, 'min. theta with beam','rad ','X6'
        WRITE(NOUT,BXL1F)arbitr2,'max. p_t**2 phot eexx','GeV^2','X3'
      ENDIF

      IF( m_KeyAcc .NE.0 ) THEN 
*!----------------------------------------------------------------------!
*! Setting up the anomalous couplings as given in the paper:            !
*!     K. Hagiwara, R.D. Peccei, D. Zeppenfeld and K. Hikasa,           !
*!                 Nucl. Phys. B282 (1987) 253;                         !
*!     see also: YR CERN-96-01, "Physics at LEP2" Vol. 1, p. 525.       !
*! The variables used in this routine correspond to the following       !
*! contants defined in the above paper:                                 !
*!           constant name     corresponding variable                   ! 
*!                g_1^V                g1(2)                            !
*!                kappa_V              kap(2)                           !
*!                lambda_V             lam(2)                           !
*!                g_4^V                g4(2)                            !
*!                g_5^V                g5(2)                            !
*!                kappa-tilde_V        kapt(2)                          !
*!                lambda-tilde_V       lamt(2)                          ! 
*!----------------------------------------------------------------------!      
         IF( m_KeyAcc .EQ. 1) THEN 
*!-- Set 1:
*!       --Set up constants OTHER than SM:
*!       --for WWgamma vertex
          g1(1)   = DCMPLX(m_Xpar(21),m_Xpar(31))
          kap(1)  = DCMPLX(m_Xpar(22),m_Xpar(32))
          lam(1)  = DCMPLX(m_Xpar(23),m_Xpar(33))
          g4(1)   = DCMPLX(m_Xpar(24),m_Xpar(34))
          g5(1)   = DCMPLX(m_Xpar(25),m_Xpar(35))
          kapt(1) = DCMPLX(m_Xpar(26),m_Xpar(36))
          lamt(1) = DCMPLX(m_Xpar(27),m_Xpar(37))
*!       --WWZ vertex
          g1(2)   = DCMPLX(m_Xpar(41),m_Xpar(51))
          kap(2)  = DCMPLX(m_Xpar(42),m_Xpar(52))
          lam(2)  = DCMPLX(m_Xpar(43),m_Xpar(53))
          g4(2)   = DCMPLX(m_Xpar(44),m_Xpar(54))
          g5(2)   = DCMPLX(m_Xpar(45),m_Xpar(55))
          kapt(2) = DCMPLX(m_Xpar(46),m_Xpar(56))
          lamt(2) = DCMPLX(m_Xpar(47),m_Xpar(57))
*!======================================================
*!====== Other TGC parametrizations disussed in: ======= 
*!== YR CERN-96-01, "Physics at LEP2" Vol. 1, p. 525. ==
*!======================================================
        ELSEIF (m_KeyAcc.EQ.2) THEN
*!-- Set 2:  
	  delta_Z = m_Xpar(61)
	  x_gamma = m_Xpar(62)
	  x_Z     = m_Xpar(63)
	  y_gamma = m_Xpar(64)
	  y_Z     = m_Xpar(65)
*!... Calculate general (internal) TGC's (cf. Hagiwara et al.)  
          tW = SQRT(SINW2/(1-SINW2))
*!       --for WWgamma vertex
          g1(1)   = 1
          kap(1)  = 1 + x_gamma 
          lam(1)  = y_gamma
          g4(1)   = 0
          g5(1)   = 0
          kapt(1) = 0
          lamt(1) = 0
*!       --WWZ vertex
          g1(2)   = 1 + tW*delta_Z 
          kap(2)  = 1 + tW*(x_Z + delta_Z)
          lam(2)  = y_Z
          g4(2)   = 0
          g5(2)   = 0
          kapt(2) = 0
          lamt(2) = 0
        ELSEIF (m_KeyAcc.EQ.3) THEN
*!-- Set 3:  
	  alpha_Wphi = m_Xpar(71)
	  alpha_Bphi = m_Xpar(72)
	  alpha_W    = m_Xpar(73)
*!... Calculate general (internal) TGC's (cf. Hagiwara et al.)  
          sW2 = SINW2
          cW2 = 1 - SINW2
*!       --for WWgamma vertex
          g1(1)   = 1
          kap(1)  = 1 + alpha_Wphi + alpha_Bphi
          lam(1)  = alpha_W
          g4(1)   = 0
          g5(1)   = 0
          kapt(1) = 0
          lamt(1) = 0
*!       --WWZ vertex
          g1(2)   = 1 + alpha_Wphi/cW2 
          kap(2)  = 1 + alpha_Wphi - sW2/cW2*alpha_Bphi
          lam(2)  = alpha_W
          g4(2)   = 0
          g5(2)   = 0
          kapt(2) = 0
          lamt(2) = 0
        ELSE
          write(6,*)'FILEXP==> Wrong KeyAcc: ',m_KeyAcc
          STOP
        ENDIF
*!
        WRITE(NOUT,BXTXT)' '
	IF (m_KeyAcc.EQ.2) THEN
          WRITE(NOUT,BXTXT)'Anomalous Couplings - set 2; YR CERN 96-01'
          WRITE(NOUT,BXTXT)'******************************************'
          WRITE(NOUT,BXL1F) delta_Z,'delta_Z','delta_Z','IA21'
          WRITE(NOUT,BXL1F) x_gamma,'x_gamma','x_gamma','IA22'
          WRITE(NOUT,BXL1F) x_Z    ,'x_Z    ','x_Z    ','IA23'
          WRITE(NOUT,BXL1F) y_gamma,'y_gamma','y_gamma','IA24'
          WRITE(NOUT,BXL1F) y_Z    ,'y_Z    ','y_Z    ','IA25'
	ELSEIF (m_KeyAcc.EQ.3) THEN
          WRITE(NOUT,BXTXT)'Anomalous Couplings - set 3; YR CERN 96-01'
          WRITE(NOUT,BXTXT)'******************************************'
          WRITE(NOUT,BXL1F) alpha_Wphi,'alpha_Wphi','alpha_Wphi','IA21'
          WRITE(NOUT,BXL1F) alpha_Bphi,'alpha_Bphi','alpha_Bphi','IA22'
          WRITE(NOUT,BXL1F) alpha_W   ,'alpha_W   ','alpha_W   ','IA23'
	ENDIF
        WRITE(NOUT,BXTXT)' '
*!
        WRITE(NOUT,BXTXT)'Internal Anomalous Couplings Activated'
        WRITE(NOUT,BXTXT)'Convention from:'
        WRITE(NOUT,BXTXT)
     $         'K.Hagiwara, R.D.Peccei, D.Zeppenfeld, K.Hikasa,'
        WRITE(NOUT,BXTXT)'                Nucl. Phys. B282 (1987) 253.'
        WRITE(NOUT,BXTXT)'                        for WWZ vertex'  
        WRITE(NOUT,BXL2C) g1(2),             'g_1^Z','g1(2)  ','IC21'
        WRITE(NOUT,BXL2C) kap(2),          'kappa_Z','kap(2) ','IC22'
        WRITE(NOUT,BXL2C) lam(2),         'lambda_Z','lam(2) ','IC23' 
        WRITE(NOUT,BXL2C) g4(2),             'g_4^Z','g4(2)  ','IC24'
        WRITE(NOUT,BXL2C) g5(2),             'g_5^Z','g5(2)  ','IC25'     
        WRITE(NOUT,BXL2C) kapt(2),   'kappa-tilde_Z','kapt(2)','IC26'       
        WRITE(NOUT,BXL2C) lamt(2),  'lambda-tilde_Z','lamt(2)','IC27'          
        WRITE(NOUT,BXTXT)'                    for WWg vertex (gamma)'  
        WRITE(NOUT,BXL2C) g1(1),             'g_1^g','g1(1)  ','IC21'
        WRITE(NOUT,BXL2C) kap(1),          'kappa_g','kap(1) ','IC22'
        WRITE(NOUT,BXL2C) lam(1),         'lambda_g','lam(1) ','IC23' 
        WRITE(NOUT,BXL2C) g4(1),             'g_4^g','g4(1)  ','IC24'
        WRITE(NOUT,BXL2C) g5(1),             'g_5^g','g5(1)  ','IC25'     
        WRITE(NOUT,BXL2C) kapt(1),   'kappa-tilde_g','kapt(1)','IC26'       
        WRITE(NOUT,BXL2C) lamt(1),  'lambda-tilde_g','lamt(1)','IC27'          
        WRITE(NOUT,BXTXT)' '
      ENDIF

      WRITE(NOUT,BXTXT) '                              DECAY LIBRARIES'
      WRITE(NOUT,BXL1I) JAK1,         'TAUOLA for W+' ,'JAK1','IL1'
      WRITE(NOUT,BXL1I) JAK2,         'TAUOLA for W-' ,'JAK2','IL2'
      WRITE(NOUT,BXL1I) ITDKRC,   'TAUOLA Ord(alpha)' ,'ITDKRC','IL3'
      WRITE(NOUT,BXL1I) IFPHOT,              'PHOTOS' ,'IFPHOT','IL4'
      WRITE(NOUT,BXL1I) IFHADM,       'JETSET for W-' ,'IFHADM','IL5'
      WRITE(NOUT,BXL1I) IFHADP,       'JETSET for W+' ,'IFHADP','IL6'
      WRITE(NOUT,BXCLO)         


*///////////////////////////////////////////////////////////////
*//        Compulsory Initialization of GLIBK                 //
*///////////////////////////////////////////////////////////////
      CALL glimit(50000)
      CALL goutpu(nout)
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
* identificator for this generator
      m_IdGen = 7
* important histo which remembers total x-section
      CALL gmonit(-1, m_IdGen,1d0,1d0,1d0) ! m_IdGen=7
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
* this is "pointer" for internal monitoring histograms/averages
      idyfs = 0
      m_svar=4*ene**2

      DO i=1,100
        wtset(i)=0
      ENDDO
*!!!!!!!!!! this should go out to tests [[[[[[[[[[[[[[[
* ============================================================
* let us keep for KORALW the glibk id-ent range from 2 to 1000
* ============================================================
* Principal weight 
      CALL gmonit(-1,idyfs+80,0d0,1d0,1d0)  ! total xs
      CALL gmonit(-1,idyfs+81,0d0,1d0,1d0)  ! xs for wt<0
      CALL gmonit(-1,idyfs+82,0d0,1d0,1d0)  ! xs for wt>wtmax
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!]]]]]]]]]]]]]]]
      WRITE(6,*) 'KORALW  <-3>'
*-- initialization of qed part
      CALL karlud(-1,m_Xcrude,dum2,dum3)
      ievacc=0
      nevtot=0
      m_NevTru=0
*///////////////////////////////////////////////////////////////
*//     initialization tauola photos etc.                     //
*///////////////////////////////////////////////////////////////
         WRITE(6,*) '>>>>>>>> initialization tauola photos etc.'
         CALL  inietc(Jak1, Jak2, itdkrc, ifphot)
         CALL  inimas
         CALL  iniphx(0.01d0)   !<--What is this 0.01d0? should go to data!
         CALL  initdk
         CALL  phoini
*///////////////////////////////////////////////////////////////
*//  initialization of 4fermion matrix el. libraries          //
*///////////////////////////////////////////////////////////////
      IF( m_Key4f .NE. 0 ) THEN
        CALL ampinw(m_Xpar,m_Npar)
      ENDIF
*///////////////////////////////////////////////////////////////
      wtu=0d0
*-- activates 4fermion tests
      m_i_4f=1
*-- beta functions tests, activated with i_beta=1
      m_i_beta=1
      IF( m_KeyIsr .NE. 0 ) THEN
*-- initialize tests of beta functions
         IF(m_i_beta. EQ. 1) 
     $        CALL KW_beta_tests(-1,idyfs,m_Xcrude,wtkarl,wtset)
      ENDIF
*///////////////////////////////////////////////////////////////
*//               4fermion monitoring                         //
*///////////////////////////////////////////////////////////////
      IF(m_i_4f .EQ. 1) 
     $  CALL KW_f4_tests(-1,idyfs,m_Xcrude,wttot,wtboww,wtbo4f,wt4f) 
* monitoring xsections in different decay channels
      CALL decay_monit(-1,wtmod,m_Xcrude,m_svar,label_dumm,nout)
* counter for z_ libraries reset
      CALL z_counter(-1,idum)
      DO i=58,60
        CALL gmonit(-1,idyfs+i,0d0,1d0,1d0)
      ENDDO
      END       



      SUBROUTINE KW_Make
*//////////////////////////////////////////////////////////////////
*//                                                              //
*//   Generation of single Monte Carlo event                     //
*//                                                              //
*//                                                              //
*//////////////////////////////////////////////////////////////////
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      SAVE
*
      INTEGER     m_Npar
      REAL*8      m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL*8      m_Xcrude,  m_svar,      m_alpha_s,   Preco,    Wtu
      REAL*8      m_XSecMC,  m_XErrMC
      REAL*8      m_XSecNR,  m_XErrNR
      INTEGER     m_KeyWgt,  m_KeySmp,    m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru,  IevAcc,      NevTot
      INTEGER     m_i_4f,    m_i_beta
*
      COMMON   /c_KW/
     $  m_Npar( 1000),           ! old Npar local Temporary/Obsolete
     $  m_Xpar(10000),           ! Xpar local
     $  m_XSecMC,                ! Final Main Best Monte Carlo Cross Section [pb]
     $  m_XErrMC,                ! and its error [pb]
     $  m_XSecNR,                ! Normalization Carlo Cross Section [pb]
     $  m_XErrNR,                ! and its error [pb]
     $  m_svar,                  ! CMSene**2
     $  m_alpha_s,               ! Alpha strong
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta                 ! Monitoring
*
      SAVE    /c_KW/
CBB      INCLUDE   "KW.h"
*//////////////////////////////////////////////////////////////////
*//    Common blocks sorted in alphabetic order!!!!              //
*//////////////////////////////////////////////////////////////////
      COMMON / decays / IFlav(4), amdec(4) 
      COMMON / decdat / amafin(20), br(20)
      COMMON / cms_eff_momdec /
     $      effbeam1(4),effbeam2(4),effp1(4),effp2(4),effp3(4),effp4(4)
      COMMON / inout  / ninp,nout
* tauola, photos and jetset overall switches
      COMMON / libra  / jak1,jak2,itdkrc,ifphot,ifhadm,ifhadp 
!<-- only ifhadm,ifhadp used
      COMMON / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot
      COMMON / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      COMMON / phypar / alfinv,gpicob
      COMMON / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf
      COMMON / wekin2 / amaw,gammw,gmu,alphaw
      COMMON / wgtgen / wtves,wtyfs,wtborn !<-- only wtborn is used
      COMMON / wgtall / wtcrud,wtmod,wtset(100)
      COMMON / wt_max / wtmax,wtmax_cc03     
      COMMON / vvrec  / vvmin,vvmax,vv,beti                   
*//////////////////////////////////////////////////////////////////
*//    Other variables and data statements                       //
*//////////////////////////////////////////////////////////////////
      DIMENSION wt4f(9)         !<-- 4fermion weights
      DIMENSION drvec(100)      !<-- vector of random numbers
      DIMENSION bsp(4)
      DATA kardmp /1/ !<-- dipswitch kardmp: printout on wt over wtmax: 1-on, 0-off
*//////////////////////////////////////////////////////////////////
*//    End of declarations UFFF!!!                               //
*//////////////////////////////////////////////////////////////////
      m_NevTru=m_NevTru+1
! fill the counter (it counts m_NevTru)
      CALL z_counter(0,idum)
 200  CONTINUE
      nevtot=nevtot+1
      CALL karlud( 0,xcborn,wtkarl,wtdumm)
! find out the label
      CALL store_label(1,label)
c these states added to Born, 10/7/98 ms
c! check if requested final state is a CC03 + MIX + doubly CKM suppresses 
c! if so it is 'NC03'  and we suppress it
c! equivalently it is ZZ with CC03
c      CALL linear_to_WZ_label(1,label,icwm,icwp,if_z,if_w)
c      IF(m_Key4f.EQ.0 .AND. if_z.EQ.1) THEN
c        wtkarl=0d0
c        wtborn=0d0
c      ENDIF
      wtcrud = wtkarl
*     ============================
      IF( m_KeyWgt .EQ. 2 ) THEN
*     ============================
*********************************************************
*   Constant weight (wt=1) events for internal CC03     *
*********************************************************
         IF (wtcrud .NE. 0d0) THEN
           wtcc03 = wwborn(effp1,effp2,effp3,effp4,m_KeyAcc)
           DO i=1,4
             bsp(i) = effp1(i)+effp2(i)+effp3(i)+effp4(i)
           ENDDO
           sp = dmas2(bsp)
*-- Born level flux factor 1/2s'
           fluxf = 1d0/(2d0*sp)
           wtcc03 = wtcc03*fluxf*gpicob
!---------------------------
!-- pseudo-rejection loop for internal CC03
           CALL varran(drvec,1)
           rn = drvec(1)
           IF (wtcrud*wtcc03 .LT. rn*wtmax_cc03) THEN
             wtcrud = 0d0
           ELSE
             wtcrud = wtmax_cc03/wtcc03
           ENDIF
!---------------------------
         ENDIF
*      ===========================
       ENDIF
*      ===========================
********************************************************
*-- (CC03,4-fermions) + Coulomb + ACC + nQCD weights --*
********************************************************
      CALL KW_model_4f(wtcrud,wtboww,wtbo4f,wt4f,br,m_alpha_s,
     $              effp1,effp2,effp3,effp4,label,m_Key4f,m_KeyAcc)
!-- Total Born weight (4f) 
      wtborn = wtbo4f*gpicob

      IF( wtborn.NE.0d0 .AND. wtcrud.NE.0d0) THEN
!-- tohep sets into HEPEVT all generated particles.
!   it decays taus and generates bremsstrahlung
!   in tau and W decays.
!-- it must be called here since eexx_wt_cor needs PHOTOS (for now)
        CALL tohep
!-- If ISR switched ON, calculate betas and corrective weights
        IF( m_KeyIsr.NE.0 ) THEN
!-- Correcting "bad" QED t-channel weights for eexx final states 
          CALL eexx_wt_cor(wtcrud,m_svar,amel,iflav,vvmin,wtcort)
!-- QED ISR model weight
          CALL betar(alfinv,wtborn*wtcort,m_svar,amel,nphot,sphot,wtset)
        ELSE
          DO i=1,100
            wtset(i)=0d0
          ENDDO
        ENDIF
        wtset(40)=wtbo4f
        DO i4f=1,9
          wtset(40+i4f)=wt4f(i4f)
        ENDDO
      ELSE
        wtcrud=0d0
        wtborn=0d0
        DO i=1,100
          wtset(i)=0d0
        ENDDO
      ENDIF



* **********************************
*       Total (principal) weight
* **********************************
! add the option of downgrading the ISR part of principal weight
!     i_principal_weight=2       ! First  order ISR
!     i_principal_weight=3       ! Second order ISR
!     i_principal_weight=4       ! Third  order ISR
! by default we use the best one !

      i_principal_weight=4       ! Third  order ISR

      IF( m_KeyIsr .EQ. 0 ) THEN
        wttot    =wtcrud*wtborn
        wtset(1) =wtborn
      ELSE
        wttot    =wtcrud*wtset(i_principal_weight)
      ENDIF

!==============================
!== weights monitoring begin ==
!==
!-- phase space volume (crude weight, no Born) 
      CALL gmonit(0,idyfs+59,wtcrud, 0d0,0d0)
!-- CC03 Born, no betas
      wtbww = wtcrud*wtboww*gpicob
      CALL gmonit(0,idyfs+58,wtbww, wtmax_cc03,0d0)
!-- cc03 born OVER wtmax, no betas
      wtovr = MAX(0d0,wtbww - wtmax_cc03)
      CALL gmonit(0,idyfs+60,wtovr, 0d0,0d0)
!-- total weight monitoring
      CALL gmonit(0,idyfs+80,wttot,  wtmax,0d0)
!-- events with wt<0
      wtneg = MIN(wttot,0d0)
      CALL gmonit(0,idyfs+81,wtneg,  0d0,0d0)
!-- events with wt>wtmax
      wtovr = MAX(0d0,wttot-wtmax)
      CALL gmonit(0,idyfs+82,wtovr,  0d0,0d0)

!-- monitoring xsections in different decay channels
      CALL decay_monit(0,wttot,m_Xcrude,m_svar,label,nout)

!-- 4fermion monitoring
      IF(m_i_4f .EQ. 1) 
     $     CALL KW_f4_tests(0,idyfs,m_Xcrude,wttot,wtboww,wtbo4f,wt4f)

!-- tests of beta functions
      IF(m_KeyIsr.NE.0 .AND. m_i_beta. EQ. 1) 
     $       CALL KW_beta_tests(0,idyfs,m_Xcrude,wtcrud,wtset) 
!==
!== weights monitoring end   ==
!==============================

*     ==============================
      IF( m_KeyWgt .EQ. 0 ) THEN
*     ==============================
*     Constant weight (wt=1) events

!-- principal rejection loop
         CALL varran(drvec,1)
         rn = drvec(1)
         IF (wttot .LT. rn*wtmax ) GOTO 200

! ms 10/10/98         DO i=1,100
! ms 10/10/98            wtset(i)=0
! ms 10/10/98         ENDDO
         wtcrud = 1d0
! Principal event weight =1 now!
         wtmod  = 1d0
*     ==============================
      ELSEIF( m_KeyWgt .EQ. 1 ) THEN
*     ==============================
*        Variable weight events

! Principal event weight
         wtmod = wttot
! Remembers crude x-section and total number of events 
         CALL gmonit(  0, m_IdGen,      m_Xcrude, wtmax,0d0)
*     ==============================
      ELSEIF( m_KeyWgt .NE. 2) THEN
*     ==============================
         WRITE(6,*)'KORALW==>wrong Keywgt=',m_KeyWgt
         STOP
*     ==============================
      ENDIF
*     ==============================

*-------------------
* dump for debugging
*-------------------
      IF(m_KeySmp .EQ. 2  .AND.  kardmp .EQ. 1
     @   .AND.  wttot/wtmax .GE. 1d0) THEN
        wtu=max(wttot/wtmax,wtu)
        CALL mm_dumper(12,6,m_NevTru,wttot,wtu,wtmax,wtmod,wtbo4f,iflav)
        CALL ww_dumper(6,m_svar,amel,wtcort) 
      ELSEIF( m_KeySmp .EQ. 1  .AND.  kardmp .EQ. 1
     @   .AND. (wttot/wtmax .GT. 1d0  .OR.  m_NevTru .EQ. -3320) ) THEN
        wtu=max(wttot/wtmax,wtu)
        CALL zz_dumper(6,m_NevTru,wttot,wtu,wtmax,wtmod,wtbo4f,iflav)
        CALL ww_dumper(6,m_svar,amel,wtcort) 
      ELSEIF( m_KeySmp .EQ. 3  .AND.  kardmp .EQ. 1
     @    .AND.  wttot/wtmax .GT. 1d0 ) THEN
        wtu=max(wttot/wtmax,wtu)
        CALL mm_dumper(12,6,m_NevTru,wttot,wtu,wtmax,wtmod,wtbo4f,iflav)
        CALL ww_dumper(6,m_svar,amel,wtcort) 
        CALL zz_dumper_short(6)
      ENDIF
*-------------------
* END dump for debugging
*-------------------

* ccccccccccccccccccccccccccccccccc
      IF( wttot  .NE.  0d0) THEN
* ccccccccccccccccccccccccccccccccc
* tohep sets into hepevt all generated particles.
*       it decays taus and generates bremsstrahlung
*       in tau and W decays.
!WP (now called earlier)         CALL tohep
* and tohad moves to lund FORMAT.
* it hadronizes whatever requires.
         CALL tohad(ifhadm,ifhadp,PReco)
      ELSE
* some routine to set hepevt to 0 should be here <<<<<============
        CONTINUE
* ccccccccccccccccccccccccccccccccc
      ENDIF
* ccccccccccccccccccccccccccccccccc
      IF(  (m_NevTru .LE. 1 .OR. m_NevTru .EQ. 2000000) 
     $     .AND. wtkarl .GT. 0d0       ) THEN
         CALL dumpl(6,p1,p2,p3,p4,qeff1,qeff2,sphot,nphot)
         CALL dumpw(nout)
      ENDIF
*-- presampler channels monitoring
      IF( m_KeySmp  .EQ.  2 ) CALL pres_monit(0,wtcrud,wtmod,wtset)
!!! temporary, to monitor progress of the accuracy with statistics
      IF(mod(nevtot,200000).EQ.0) THEN
        CALL decay_monit(1,wtmod,m_Xcrude,m_svar,label_dumm,6)
      ENDIF
      END


      SUBROUTINE KW_Finalize
*//////////////////////////////////////////////////////////////////
*//                                                              //
*//   Final printouts and calculation of the total Xsection      //
*//                                                              //
*//   Final Xsections and statistics available from getters:     //
*//   KW_GetXSecMC(XSecMC,XErrMC)                                //
*//   KW_GetXSecNR(XSecNR,XErrNR)                                //
*//   KW_GetNevMC(NevMC)                                         //
*//                                                              //
*//////////////////////////////////////////////////////////////////
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      SAVE
*
      INTEGER     m_Npar
      REAL*8      m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL*8      m_Xcrude,  m_svar,      m_alpha_s,   Preco,    Wtu
      REAL*8      m_XSecMC,  m_XErrMC
      REAL*8      m_XSecNR,  m_XErrNR
      INTEGER     m_KeyWgt,  m_KeySmp,    m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru,  IevAcc,      NevTot
      INTEGER     m_i_4f,    m_i_beta
*
      COMMON   /c_KW/
     $  m_Npar( 1000),           ! old Npar local Temporary/Obsolete
     $  m_Xpar(10000),           ! Xpar local
     $  m_XSecMC,                ! Final Main Best Monte Carlo Cross Section [pb]
     $  m_XErrMC,                ! and its error [pb]
     $  m_XSecNR,                ! Normalization Carlo Cross Section [pb]
     $  m_XErrNR,                ! and its error [pb]
     $  m_svar,                  ! CMSene**2
     $  m_alpha_s,               ! Alpha strong
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta                 ! Monitoring
*
      SAVE    /c_KW/
CBB      INCLUDE   "KW.h"

      COMMON / bxfmts / bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      CHARACTER*80      bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      COMMON / inout  / ninp,nout
      COMMON / wt_max / wtmax,wtmax_cc03     
      DIMENSION wt4f(9)
      DIMENSION wtset(100)
      REAL pol(4)               !<-- single precision parameter for tauola

*--------------------------------------------------------------------------------
      CALL karlud(1,m_Xcrude,xcvesk,dumm1)
*-- presampler channels monitoring
      IF( m_KeySmp  .EQ.  2 ) CALL pres_monit(1,wtcrud,wtmod,wtset)
* ccccccccccccccccccccccccccccccccc
* final printouts of tauola
      IF( m_KeyWgt  .EQ.  0) THEN
         CALL dexay(100,pol)
      ENDIF
c remove next line
         CALL dexay(100,pol)
* ccccccccccccccccccccccccccccccccc
!---- stuff moved from karlud beg ----
*-- crude xs. no born
      CALL gmonit(1,idyfs+59,wtkacr,erkacr,parm3)
      CALL gmonit(2,idyfs+59,evacc,evneg,evove)
      nevneg = evneg
      nevtot = parm3
      WRITE(nout,bxope)
      WRITE(nout,bxtxt) '         KORALW  final  report '
      WRITE(nout,bxtxt) '               Window A        '
      WRITE(nout,bxtxt) '            WEIGHTED evts.     '
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) ' ccru matrix element means:    '
      WRITE(nout,bxtxt) ' a) Born matrix element for CC03 processes  '
      WRITE(nout,bxtxt) ' b) technical crude m.e. for nc processes or'
      WRITE(nout,bxtxt) '    for keysmp .NE. 0                       '
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) '     xsect with no matrix element   '
      WRITE(nout,bxl1i) nevtot,'total no of events      ','nevtot','a0'
      WRITE(nout,bxl1i) nevneg,'wtcrud < 0 evts         ','nevneg','a1'
      xskr   = m_Xcrude*wtkacr
      erkr   = xskr*erkacr
      WRITE(nout,bxl1g) m_Xcrude,'sigma_crude           ','Xcrude','a2'
      WRITE(nout,bxl2g) 
     $           wtkacr,erkacr,'<wtcrud>, rel err       ','wtkacr','a3'
      WRITE(nout,bxl2g)
     $              xskr,erkr,'phsp. vol, no beta-0     ','xskr  ','a4'
      WRITE(nout,bxtxt) ' '

*-- born xsection, total
      CALL gmonit(1,idyfs+58,wtkabo,erkabo,parm3)
      CALL gmonit(2,idyfs+58,evacc,evneg,evove)
      nevneg = evneg
      nevove = evove
      nevtot = parm3
      xskb0  = m_Xcrude*wtkabo
      erkb0  = xskb0*erkabo
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) ' xsect with ccru matrix el. only, no betas'
      WRITE(nout,bxl1i) nevtot,'total no of events      ','nevtot','a5'
      WRITE(nout,bxl1i) nevneg,'wtcrud*wtborn <0 evts   ','nevneg','a6'
      WRITE(nout,bxl2g)
     $           wtkabo,erkabo,'<wtcrud*wtborn>, rel err','wtkabo','a7'
      WRITE(nout,bxl2g)
     $           xskb0,erkb0,  'sigma (born m.el.)      ','xska0','a8'

*-- born xsection from above wtmax
      CALL gmonit(1,idyfs+60,wtkabo,erkabo,parm3)
      xskb   = m_Xcrude*wtkabo
      erkb   = xskb*erkabo
      IF (xskb.NE.0d0) THEN
        xx=xskb/xskb0
        ee=erkb/xskb0
      ELSE
         xx=0d0
         ee=0d0
      ENDIF
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) '     xsect over wtmax_cc03     '
      WRITE(nout,bxtxt) ' ccru matrix el. only, no betas'
      WRITE(nout,bxl1i) nevove,   'evts: wt>wtmax_cc03  ','nevove','a9'
      WRITE(nout,bxl2g) xskb,erkb,'sigma: wt>wtmax_cc03 ','xskabo','a10'
      WRITE(nout,bxl2g) xx,ee,    'relat sigma: wt>wtmax','xskabo','a11'
      WRITE(nout,bxclo)
!---- stuff moved from karludw end ----
      IF( m_KeyIsr  .NE.  0 ) THEN
*-- tests of beta functions
        IF(m_i_beta. EQ. 1) 
     $        CALL KW_beta_tests(1,idyfs,m_Xcrude,wtkarl,wtset)
      ENDIF
*-- best xsection printout, total and over
      CALL gmonit(1,idyfs+80,averwt,errela,evtot)
      m_XSecMC  = m_Xcrude*averwt
      m_XErrMC  = m_XSecMC*errela
      CALL gmonit(2,idyfs+80,evacc,evneg,evove)
      nevacc = evacc
      nevneg = evneg
      nevove = evove
      CALL gmonit(1,idyfs+81,averwn,erreln,evtot)
      xsneg   = averwn/averwt
      erneg   = xsneg*erreln
      CALL gmonit(1,idyfs+82,averwo,errelo,evtot)
      xsove   = averwo/averwt
      erove   = xsove*errelo
      WRITE(nout,bxope)
      WRITE(nout,bxtxt) '         KORALW  final  report '
      WRITE(nout,bxtxt) '               Window C        '
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) '     BEST order total xsect.   '
      WRITE(nout,bxl1i)nevtot,       'total no of events ','nevtot','c1'
      WRITE(nout,bxl1i)m_NevTru,   'accepted events      ','NevTru','c2'
      WRITE(nout,bxl2g)
     $             m_XSecMC,m_XErrMC,'sigma_tot [pb]     ','xskabo','c3'
      WRITE(nout,bxl1f)errela,       'relative error     ','errela','c4'
      WRITE(nout,bxl1i)nevneg,       'events: wt<0       ','nevneg','c5'
      WRITE(nout,bxl2g)xsneg,erneg,  'xsec/xtot: wt<0    ','xsneg ','c6'
      WRITE(nout,bxl1i)nevove,       'events: wt>wtmax   ','nevove','c7'
      WRITE(nout,bxl2g)xsove,erove,  'xsec/xtot: wt>wtmax','xsove ','c8'
      WRITE(nout,bxclo)
      IF( m_Key4f  .NE.  0 ) THEN
!-- 4fermion monitoring
         IF(m_i_4f .EQ. 1) 
     $     CALL KW_f4_tests( 1,idyfs,m_Xcrude,wttot,wtboww,wtbo4f,wt4f)
      ENDIF
! monitoring xsections in different decay channels
      CALL decay_monit(1,wtmod,m_Xcrude,m_svar,label_dumm,nout)
**********
* on request also the printout for the photonic pre-tabulation
*********      CALL decay_monit(2,wtmod,m_Xcrude,m_svar,label_dumm,nout)
*-----------------------------------------------------------------------
      IF( m_KeyWgt .EQ. 0 ) THEN
        m_XSecNR      = m_XSecMC
        m_XErrNR      = m_XErrMC
      ELSE
        m_XSecNR      = m_Xcrude
        m_XErrNR      = 0d0
      ENDIF
      END


      SUBROUTINE KW_model_4f(wtcrud,wtboww,wtbo4f,wt4f,br,alpha_s,
     $                    effp1,effp2,effp3,effp4,label,Key4f,Keyacc)
!     ****************************************************************
! external weight connected with 4fermions
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / MATPAR / PI,CEULER    
      SAVE / MATPAR /

      DIMENSION wt4f(9),br(20)
      DIMENSION effp1(4),effp2(4),effp3(4),effp4(4)
      DIMENSION bq1(4),bq2(4),bsp(4)

      DIMENSION ipdg(4)
      CHARACTER*3 chuman(4)

      IF (wtcrud .EQ. 0d0) THEN
        wtboww = 0d0
        wtbo4f = 0d0
        DO i4f=1,9
          wt4f(i4f)=0d0
        ENDDO
        RETURN
      ENDIF

! from now on WTCRUD <> 0 !

! common part
      DO i=1,4
        bq1(i) = effp1(i)+effp2(i)
        bq2(i) = effp3(i)+effp4(i)
        bsp(i) = bq1(i)+bq2(i)
      ENDDO
      s1 = dmas2(bq1)
      s2 = dmas2(bq2)
      sp = dmas2(bsp)
*-- Born level flux factor 1/2s'
      fluxf = 1d0/(2d0*sp)
*-- identify WW type final states
      CALL linear_to_WZ_label(1,label,iwm,iwp,if_z,if_w)
*-- Internal WW Born matrix element, not necesarily used later on
      wtborn = wwborn(effp1,effp2,effp3,effp4,Keyacc)
*-- Coulomb corr.
      IF( if_w .NE. 0 ) THEN
        cc = CulMC(sp,s1,s2)
      ELSE 
        cc = 1
      ENDIF
      wtboww = fluxf*wtborn*cc
      wtbo4f = wtboww
      DO i4f=1,9
        wt4f(i4f)=0d0
      ENDDO
        
      IF (Key4f.GT.0) THEN
!    1====================1
*-- External 4fermion matrix el.
        CALL ampext(wtmod4f,wt4f )
        IF( if_w .NE. 0 ) THEN
!      2======================2
*-- WW-extras
*-- SM CC03 Born
          IF (Keyacc .EQ. 0) THEN
            wtborn_sm = wtborn
          ELSE
            wtborn_sm = wwborn(effp1,effp2,effp3,effp4,0)
          ENDIF
*-- Divide off naive QCD correction from wtborn_sm if included
          CALL KW_Naive_QCD(label,br,wt_qcd)
*-- ACC & Coulomb corrections to CC03 Born (divide off naive QCD)
          dif_bc = (wtborn*cc - wtborn_sm)/wt_qcd
*-- 4fermion "improved" Born (naive QCD correction - multiplicative) 
          wtbo4f = (wtmod4f + dif_bc)*wt_qcd*fluxf
!--------- ms tests beg
!          rat_bc = wtborn*cc / wtborn_sm
!          wtbo4f = (wtmod4f*rat_bc)*fluxf
!--------- ms tests end
          DO i4f=1,9
             wt4f(i4f) = (wt4f(i4f) + dif_bc)*wt_qcd*fluxf
          ENDDO
        ELSE
!      2====2
*-- ZZ-extras
*-- alpha_s/pi for naive QCD corrections
          aspi = alpha_s/pi
*-- Naive QCD correction - multiplicative
          wt_qcd = 1d0
          CALL linear_to_pdg_label(1,label,ipdg,chuman)
          IF (abs(ipdg(1)) .LT. 10) wt_qcd = wt_qcd*(1 + aspi)
          IF (abs(ipdg(3)) .LT. 10) wt_qcd = wt_qcd*(1 + aspi)
*-- 4fermion "improved" Born 
          wtbo4f = wtmod4f*wt_qcd*fluxf
          DO i4f=1,9
             wt4f(i4f) = wt4f(i4f)*wt_qcd*fluxf
          ENDDO
          wtboww = 0d0
        ENDIF
!      2=====2
      ELSE
!    1====1 
        wtbo4f = wtboww
        DO i4f=1,9
          wt4f(i4f)=0d0
        ENDDO
      ENDIF
!    1=====1

      END

      SUBROUTINE KW_beta_tests(mode,idyfs,Xcrude,wtkarl,wtset)
!     ***********************************************
! beta functions related tests
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / matpar / pi,ceuler
      COMMON / phypar / alfinv,gpicob
 
      COMMON / bxfmts / bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      CHARACTER*80      bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g

      COMMON / inout  / ninp,nout

      DIMENSION wtset(*)

      IF(mode .EQ. -1) THEN
* Totals O(alf0-alf3)
         DO k=10,14
            CALL gmonit(-1,idyfs+k,0d0,1d0,1d0) ! 10-14
         ENDDO
* Betas O(alf0-alf3) and differences
         DO k=20,38
            CALL gmonit(-1,idyfs+k,0d0,1d0,1d0) ! 20-38
         ENDDO
      ELSEIF(mode .EQ. 0) THEN
*     Totals O(alf0-alf3)
         DO k=1,4
            CALL gmonit(0,idyfs+10+k,wtkarl*wtset(k),1d0,1d0)
         ENDDO
* Betas O(alf0-alf3)
         DO k=10,19
            CALL gmonit(0,idyfs+10+k,wtkarl*wtset(k),1d0,1d0)
         ENDDO
* Differences total O(alf0-alf3)
         CALL gmonit(0,idyfs+30,wtkarl*(wtset(2)-wtset(1)),1d0,1d0)
         CALL gmonit(0,idyfs+31,wtkarl*(wtset(3)-wtset(2)),1d0,1d0)
         CALL gmonit(0,idyfs+32,wtkarl*(wtset(4)-wtset(3)),1d0,1d0)
* bt01-bt00, bt10
         CALL gmonit(0,idyfs+33,wtkarl*(wtset(11)-wtset(10)),1d0,1d0)
* bt02-bt01, bt11-bt10, bt20
         CALL gmonit(0,idyfs+34,wtkarl*(wtset(13)-wtset(11)),1d0,1d0)
         CALL gmonit(0,idyfs+35,wtkarl*(wtset(14)-wtset(12)),1d0,1d0)
* bt03-bt02, bt12-bt11, bt21-bt20,bt30
         CALL gmonit(0,idyfs+36,wtkarl*(wtset(16)-wtset(13)),1d0,1d0)
         CALL gmonit(0,idyfs+37,wtkarl*(wtset(17)-wtset(14)),1d0,1d0)
         CALL gmonit(0,idyfs+38,wtkarl*(wtset(18)-wtset(15)),1d0,1d0)
      ELSEIF(mode .EQ. 1) THEN
        WRITE(nout,bxope)
        WRITE(nout,bxtxt) '         KORALW  final  report '
        WRITE(nout,bxtxt) '               Window B        '
        WRITE(nout,bxtxt) '            Xsec-s in [pb]     '
        WRITE(nout,bxtxt) '                               '
*****************************************************************
*****************************************************************
        CALL gmonit(1,idyfs+11,averwt,errela,evtot)
        xstot0   = Xcrude*averwt
        ertot0   = xstot0*errela
        WRITE(nout,bxl2f) xstot0,ertot0,'xsec total    ','O(alf0)','b3'
*****************************************************************
        CALL gmonit(1,idyfs+12,averwt,errela,evtot)
        xstot1   = Xcrude*averwt
        ertot1   = xstot1*errela
        WRITE(nout,bxl2f) xstot1,ertot1,'xsec total    ','O(alf1)','b4'
*****************************************************************
        CALL gmonit(1,idyfs+13,averwt,errela,evtot)
        xstot2   = Xcrude*averwt
        ertot2   = xstot2*errela
        WRITE(nout,bxl2f) xstot2,ertot2,'xsec total    ','O(alf2)','b5'
*****************************************************************
        CALL gmonit(1,idyfs+14,averwt,errela,evtot)
        xstot3   = Xcrude*averwt
        ertot3   = xstot3*errela
        WRITE(nout,bxl2f) xstot3,ertot3,'xsec total    ','O(alf3)','b6'
*****************************************************************
        CALL gmonit(2,idyfs+11,evacc1,evneg1,evove1)
        CALL gmonit(2,idyfs+12,evacc2,evneg2,evove2)
        CALL gmonit(2,idyfs+13,evacc3,evneg3,evove3)
        CALL gmonit(2,idyfs+14,evacc4,evneg4,evove4)
        neg0=evneg1
        neg1=evneg2
        neg2=evneg3
        neg3=evneg4
        WRITE(nout,bxl1i) neg0,         'wt<0  events  ','O(alf0)',' '
        WRITE(nout,bxl1i) neg1,         'wt<0  events  ','O(alf1)',' '
        WRITE(nout,bxl1i) neg2,         'wt<0  events  ','O(alf2)',' '
        WRITE(nout,bxl1i) neg2,         'wt<0  events  ','O(alf3)',' '
*****************************************************************
        CALL gmonit(1,idyfs+20,averwt,errela,evtot)
        xsbt00   = Xcrude*averwt
        erbt00   = xsbt00*errela
        WRITE(nout,bxl2f) xsbt00,erbt00,'xsec(beta00)  ','O(alf0)','b7'
*****************************************************************
        CALL gmonit(1,idyfs+21,averwt,errela,evtot)
        xsbt01   = Xcrude*averwt
        erbt01   = xsbt01*errela
        WRITE(nout,bxl2f) xsbt01,erbt01,'xsec(beta01)  ','O(alf1)','b8'
*****************************************************************
        CALL gmonit(1,idyfs+22,averwt,errela,evtot)
        xsbt10   = Xcrude*averwt
        erbt10   = xsbt10*errela
        WRITE(nout,bxl2f) xsbt10,erbt10,'xsec(beta10)  ','O(alf1)','b9'
*****************************************************************
        CALL gmonit(1,idyfs+23,averwt,errela,evtot)
        xsbt02   = Xcrude*averwt
        erbt02   = xsbt02*errela
        WRITE(nout,bxl2f) xsbt02,erbt02,'xsec(beta02)  ','O(alf2)','b10'
*****************************************************************
        CALL gmonit(1,idyfs+24,averwt,errela,evtot)
        xsbt11   = Xcrude*averwt
        erbt11   = xsbt11*errela
        WRITE(nout,bxl2f) xsbt11,erbt11,'xsec(beta11)  ','O(alf2)','b11'
*****************************************************************
        CALL gmonit(1,idyfs+25,averwt,errela,evtot)
        xsbt20   = Xcrude*averwt
        erbt20   = xsbt20*errela
        WRITE(nout,bxl2f) xsbt20,erbt20,'xsec(beta20)  ','O(alf2)','b12'
*****************************************************************
*****************************************************************
        CALL gmonit(1,idyfs+26,averwt,errela,evtot)
        xsbt03   = Xcrude*averwt
        erbt03   = xsbt03*errela
        WRITE(nout,bxl2f) xsbt03,erbt03,'xsec(beta03)  ','O(alf3)','b13'
*****************************************************************
        CALL gmonit(1,idyfs+27,averwt,errela,evtot)
        xsbt12   = Xcrude*averwt
        erbt12   = xsbt12*errela
        WRITE(nout,bxl2f) xsbt12,erbt12,'xsec(beta12)  ','O(alf3)','b14'
*****************************************************************
        CALL gmonit(1,idyfs+28,averwt,errela,evtot)
        xsbt21   = Xcrude*averwt
        erbt21   = xsbt21*errela
        WRITE(nout,bxl2f) xsbt21,erbt21,'xsec(beta21)  ','O(alf3)','b15'
*****************************************************************
        CALL gmonit(1,idyfs+29,averwt,errela,evtot)
        xsbt30   = Xcrude*averwt
        erbt30   = xsbt30*errela
        WRITE(nout,bxl2f) xsbt30,erbt30,'xsec(beta30)  ','O(alf3)','b16'
*****************************************************************
        WRITE(nout,bxtxt) ' xsec_tot differences '
*****************************************************************
        CALL gmonit(1,idyfs+30,averwt,errela,evtot)
        xsdel1   = Xcrude*averwt
        erdel1   = xsdel1*errela
        CALL gmonit(1,idyfs+31,averwt,errela,evtot)
        xsdel2   = Xcrude*averwt
        erdel2   = xsdel2*errela
        CALL gmonit(1,idyfs+32,averwt,errela,evtot)
        xsdel3   = Xcrude*averwt
        erdel3   = xsdel3*errela
        WRITE(nout,bxl2f) xsdel1,erdel1,'xstot(alf1-0)','O(alf1)','b17'
        WRITE(nout,bxl2f) xsdel2,erdel2,'xstot(alf2-1)','O(alf2)','b18'
        WRITE(nout,bxl2f) xsdel3,erdel3,'xstot(alf3-2)','O(alf3)','b19'
*****************************************************************
        WRITE(nout,bxtxt) ' betas differences '
*****************************************************************
        CALL gmonit(1,idyfs+33,averwt,errela,evtot)
        xsdt01   = Xcrude*averwt
        erdt01   = xsdt01*errela
        WRITE(nout,bxl2f) xsdt01,erdt01,'xs(beta01-00)','O(alf1)','b20'
        WRITE(nout,bxl2f) xsbt10,erbt10,'xs(beta10)   ','O(alf1)','b21'
*****************************************************************
        CALL gmonit(1,idyfs+34,averwt,errela,evtot)
        xsdt02   = Xcrude*averwt
        erdt02   = xsdt02*errela
        CALL gmonit(1,idyfs+35,averwt,errela,evtot)
        xsdt11   = Xcrude*averwt
        erdt11   = xsdt11*errela
        WRITE(nout,bxl2f) xsdt02,erdt02,'xs(beta02-01)','O(alf2)','b19'
        WRITE(nout,bxl2f) xsdt11,erdt11,'xs(beta11-10)','O(alf2)','b20'
        WRITE(nout,bxl2f) xsbt20,erbt20,'xs(beta20)   ','O(alf2)','b21'
*****************************************************************
        CALL gmonit(1,idyfs+36,averwt,errela,evtot)
        xsdt03   = Xcrude*averwt
        erdt03   = xsdt03*errela
        CALL gmonit(1,idyfs+37,averwt,errela,evtot)
        xsdt12   = Xcrude*averwt
        erdt12   = xsdt12*errela
        CALL gmonit(1,idyfs+38,averwt,errela,evtot)
        xsdt21   = Xcrude*averwt
        erdt21   = xsdt21*errela
        WRITE(nout,bxl2f) xsdt03,erdt03,'xs(beta03-02)','O(alf3)','b22'
        WRITE(nout,bxl2f) xsdt12,erdt12,'xs(beta12-11)','O(alf3)','b23'
        WRITE(nout,bxl2f) xsdt21,erdt21,'xs(beta21-20)','O(alf3)','b24'
        WRITE(nout,bxl2f) xsbt30,erbt30,'xs(beta30)   ','O(alf3)','b25'
*****************************************************************
        WRITE(nout,bxclo)
      ENDIF
      END

      SUBROUTINE KW_f4_tests(mode,idyfs,Xcrude,wttot,wtboww,wtbo4f,wt4f)
!     ***************************************************************
! 4fermion related tests
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / matpar / pi,ceuler
      COMMON / phypar / alfinv,gpicob
 
      COMMON / bxfmts / bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      CHARACTER*80      bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g

      COMMON / inout  / ninp,nout

      DIMENSION wt4f(*)

      IF(mode .EQ. -1) THEN
*-- all 4-fermion
        DO k=91,96
          CALL gmonit(-1,idyfs+k,0d0,1d0,1d0) ! 91-96
        ENDDO
      ELSEIF(mode .EQ. 0) THEN
*-- 4-fermion monitoring
         IF (ABS(wtbo4f).GT.1d-50) THEN
           wttww = wtboww/wtbo4f*wttot
         ELSE
           wttww = 0d0
         ENDIF
         CALL gmonit(0,idyfs+91,wtbo4f,1d0,0d0)
         CALL gmonit(0,idyfs+92,wttot,1d0,0d0)
         CALL gmonit(0,idyfs+93,wttww,1d0,0d0)
         CALL gmonit(0,idyfs+94,wt4f(1),1d0,0d0)
         CALL gmonit(0,idyfs+95,wt4f(2),1d0,0d0)
         CALL gmonit(0,idyfs+96,wttot-wttww,1d0,0d0)
*-- END 4-fermion monitoring
      ELSEIF(mode .EQ. 1) THEN
*-- 4-fermion monitoring
        WRITE(nout,bxope)
        WRITE(nout,bxtxt) '         KORALW  final  report '
        WRITE(nout,bxtxt) '               Window D        '
        WRITE(nout,bxtxt) '                               '
        WRITE(nout,bxtxt) '     Complete 4-fermion process'
        WRITE(nout,bxtxt) '                               '
        WRITE(nout,bxtxt) '   I. Best ord. W-pair total xsect.    '
*****************************************************************
        CALL gmonit(1,idyfs+93,averwt,errela,evtot)
        WRITE(nout,bxl2g)
     $           averwt,errela,      '<wttww>: WW weight ','averwt','d1'
        xskbs   = Xcrude*averwt
        erkbs   = xskbs*errela
        WRITE(nout,bxl2g)xskbs,erkbs,'sigma_WW, best [pb]','xskabo','d2'
        WRITE(nout,bxtxt) '                               '
        WRITE(nout,bxtxt) '   II. Best ord. 4-fermion total xsect.'
*****************************************************************
        CALL gmonit(1,idyfs+91,averwt,errela,evtot)
        WRITE(nout,bxl2g)
     $          averwt,errela,       '<wtbo4f>, rel err  ','averwt','d3'
*****************************************************************
        CALL gmonit(1,idyfs+92,averwt,errela,evtot)
        WRITE(nout,bxl2g)
     $          averwt,errela,       '<wttot>,rel err    ','averwt','d4'
        xskbb   = Xcrude*averwt
        erkbb   = xskbb*errela
        WRITE(nout,bxl2g)xskbb,erkbb,'sigma_4f, best [pb]','xskabo','d5'
        stob = 1- xskbs/xskbb
        stober = dsqrt( (erkbs*xskbb)**2 +(erkbb*xskbs)**2 ) / xskbb**2
        WRITE(nout,bxl2g)stob,stober,'sigma 1-Wpair/4ferm','1-d2/5','d6'
*****************************************************************
        CALL gmonit(1,idyfs+96,averwt,errela,evtot)
        xskbd   = Xcrude*averwt
        erkbd   = xskbd*errela
        stob = xskbd/xskbb
        stober = dsqrt( (erkbd*xskbb)**2 +(erkbb*xskbd)**2 ) / xskbb**2
        WRITE(nout,bxl2g)stob,stober,'sigma 1-Wpair/4ferm','wtbgr ','d7'
        WRITE(nout,bxclo)
      ENDIF
*****************************************************************
      END


      SUBROUTINE KW_Naive_QCD(label,br,wt_qcd)
!******************************************
! This routine adds the naive QCD correction to the external matrix el.
! It is done only for the WW final states, and is justified for CC03.
! Also the effective nontrivial CKM is introduced on request.
! Everything is done based solely on the deviation of branching ratios
! from the 1/3, 1/9 settings.

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION br(20),br0(20)
      DATA init /0/
      SAVE
 
      IF(init .EQ. 0) THEN
         init=1

         br0(1)=(1d0/3d0) !  <== ud
         br0(2)=0d0       !  <== cd
         br0(3)=0d0       !  <== us
         br0(4)=(1d0/3d0) !  <== cs
         br0(5)=0d0       !  <== ub
         br0(6)=0d0       !  <== cb
         br0(7)=(1d0/9d0) !  <== e
         br0(8)=(1d0/9d0) !  <== mu
         br0(9)=(1d0/9d0) !  <== tau
      ENDIF
! do not redefine off-diagonal states, they are corrected already in
! Born
! off-diagonal are also SINGLE off-diag.  m.s. 3/13/98
         
      CALL linear_to_WZ_label(1,label,iwm,iwp,if_z,if_w)
      IF( if_w .NE. 0 ) THEN
         IF(br0(iwm) .NE. 0d0 .AND. br0(iwp) .NE. 0d0) THEN
           wm = (br(iwm)/br(7)) / (br0(iwm)/br0(7))
           wp = (br(iwp)/br(7)) / (br0(iwp)/br0(7))
         ELSE
           wm = 1d0
           wp = 1d0
         ENDIF
         wt_q= wm*wp
      ELSE
         wt_q=1d0
      ENDIF
           
      wt_qcd=wt_q
          
      END

      SUBROUTINE KW_ReadMask(DiskFile,iTalk,xpar)
*///////////////////////////////////////////////////////////////////
*//                                                               //
*//   DiskFile  = input file to read                              //
*///////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      CHARACTER*80 DiskFile
      REAL*8 xpar(*)
      INTEGER iTalk
*
* User mask on final states 
      INTEGER i_umask(202)
      CHARACTER*1 chdum
      CHARACTER*60 comm60
      CHARACTER*6  beg6
      INTEGER ninp,i,j,line
*--------------------------------------------
      IF(iTalk .EQ. 1) THEN
         WRITE(  *,*) '**************************'
         WRITE(  *,*) '*   KW_ReadMask starts   *'
         WRITE(  *,*) '**************************'
      ENDIF
* read user umask from file only here 
      ninp=13
      OPEN(unit=ninp, file=DiskFile, status='unknown')

* Search for 'BeginM'
      DO line =1,10000
         READ(ninp,'(a6,a)') beg6,comm60
         IF(beg6 .EQ. 'BeginM') THEN
            IF(iTalk .EQ. 1)   WRITE( *,'(a6,a)') beg6,comm60
            GOTO 200
         ENDIF
      ENDDO
 200  CONTINUE
*
      READ(ninp,'(a1)') chdum
      READ(ninp,'(a1)') chdum
      DO i=1,9
         READ(ninp,*) (i_umask((i-1)*9+j),j=1,9)
         IF(iTalk .EQ. 1) WRITE( *,'(20i5)') 
     $                (i_umask((i-1)*9+j),j=1,9)
      ENDDO
      READ(ninp,'(a1)') chdum
      READ(ninp,'(a1)') chdum
      DO i=1,11
         READ(ninp,*) (i_umask(81+(i-1)*11+j),j=1,11)
         IF(iTalk .EQ. 1) WRITE( *,'(20i5)') 
     $                (i_umask(81+(i-1)*11+j),j=1,11)
      ENDDO
*
      DO i=1,202
         xpar(1100+i)=i_umask(i)
      ENDDO
      CLOSE(ninp)
      IF(iTalk .EQ. 1) THEN
         WRITE(  *,*) '**************************'
         WRITE(  *,*) '*    KW_ReadMask Ends    *'
         WRITE(  *,*) '**************************'
      ENDIF
      END

      SUBROUTINE KW_ReaDataX(DiskFile,iReset,imax,xpar)
*///////////////////////////////////////////////////////////////////
*//                                                               //
*//   DiskFile  = input file to read                              //
*//   imax   = maximum index in xpar                              //
*//   iReset = 1, resets xpar to 0d0                              //
*//   iTalk=1,     prints echo into standard input                //
*//                                                               //
*//   Single data card is:    (a1,i4,d15.0,a60)                   //
*//   First data card: BeginX                                     //
*//   Last  data card: EndX                                       //
*//   First character * defines comment card!                     //
*//                                                               //
*///////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      CHARACTER*80 DiskFile
      REAL*8 xpar(*)
      CHARACTER*6 beg6
      CHARACTER*4 end4
      CHARACTER*1 mark1
      CHARACTER*60 comm60
      CHARACTER*80 comm80
      INTEGER  imax,iReset,iTalk
*
      INTEGER   ninp,i,line,index
      REAL*8   value
*
*////////////////////////////////////////
*//  Clear xpar and read default Umask //
*////////////////////////////////////////
      iTalk = 1
      IF(iReset .EQ. 1 ) THEN
         iTalk = 0
         DO i=1,imax
            xpar(i)=0d0
         ENDDO
         CALL KW_ReadMask(DiskFile,iTalk,xpar)
      ENDIF
      ninp = 13
      OPEN(ninp,file=DiskFile)
      IF(iTalk .EQ. 1) THEN
         WRITE(  *,*) '**************************'
         WRITE(  *,*) '*   KW_ReaDataX starts   *'
         WRITE(  *,*) '**************************'
      ENDIF
* Search for 'BeginX'
      DO line =1,10000
         READ(ninp,'(a6,a)') beg6,comm60
         IF(beg6 .EQ. 'BeginX') THEN
            IF(iTalk .EQ. 1)   WRITE( *,'(a6,a)') beg6,comm60
            GOTO 200
         ENDIF
      ENDDO
 200  CONTINUE
* Read data, 'EndX' terminates data, '*' marks comment
      DO line =1,1000
         READ(ninp,'(a)') mark1
         IF(mark1 .EQ. ' ') THEN
            BACKSPACE(ninp)
            READ(ninp,'(a1,i4,d15.0,a60)') mark1,index,value,comm60
            IF(iTalk .EQ. 1) 
     $           WRITE( *,'(a1,i4,g15.6,a60)') mark1,index,value,comm60
            IF( (index .LE. 0) .OR. (index .GE. imax)) GOTO 990
            xpar(index) = value
         ELSEIF(mark1 .EQ. 'E') THEN
            BACKSPACE(ninp)
            READ(  ninp,'(a4,a)') end4,comm60
            IF(iTalk .EQ. 1)   WRITE( *,'(a4,a)') end4,comm60
            IF(end4 .EQ. 'EndX') GOTO 300
            GOTO 991
         ELSEIF(mark1 .EQ. '*') THEN
            BACKSPACE(ninp)
            READ(  ninp,'(a)') comm80
            IF(iTalk .EQ. 1)    WRITE( *,'(a)') comm80
         ENDIF
      ENDDO
 300  CONTINUE
      IF(iTalk .EQ. 1)  THEN
         WRITE(  *,*) '************************'
         WRITE(  *,*) '*  KW_ReaDataX ends    *'
         WRITE(  *,*) '************************'
      ENDIF
      CLOSE(ninp)
      RETURN
*-----------
 990  WRITE(    *,*) '+++ KW_ReaDataX: wrong index= ',index
      STOP
      RETURN
 991  WRITE(    *,*) '+++ KW_ReaDataX: wrong end of data '
      STOP
      END

      SUBROUTINE KW_VecPrint(nunit,word,pp)
*/////////////////////////////////////////////////////////////////////////
*//                                                                     //
*//   prints single momentum "pp" on unit "nunit" with comment "word"   //
*//                                                                     //
*/////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER      nunit
      CHARACTER*8  word
      REAL*8       pp(4),ams
      INTEGER      i
*----
      ams = pp(4)**2 -pp(3)**2 -pp(2)**2 -pp(1)**2
      IF(ams .GT. 0.0) ams = SQRT(ams)
      WRITE(nunit,'(a8,5(1x,f20.13))') word,(pp(i),i=1,4),ams
      END

      SUBROUTINE KW_GetWtMain(WtMain)
*//////////////////////////////////////////////////////////////////
*//   Main weights  WtMain                                       //
*//////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INTEGER     m_Npar
      REAL*8      m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL*8      m_Xcrude,  m_svar,      m_alpha_s,   Preco,    Wtu
      REAL*8      m_XSecMC,  m_XErrMC
      REAL*8      m_XSecNR,  m_XErrNR
      INTEGER     m_KeyWgt,  m_KeySmp,    m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru,  IevAcc,      NevTot
      INTEGER     m_i_4f,    m_i_beta
*
      COMMON   /c_KW/
     $  m_Npar( 1000),           ! old Npar local Temporary/Obsolete
     $  m_Xpar(10000),           ! Xpar local
     $  m_XSecMC,                ! Final Main Best Monte Carlo Cross Section [pb]
     $  m_XErrMC,                ! and its error [pb]
     $  m_XSecNR,                ! Normalization Carlo Cross Section [pb]
     $  m_XErrNR,                ! and its error [pb]
     $  m_svar,                  ! CMSene**2
     $  m_alpha_s,               ! Alpha strong
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta                 ! Monitoring
*
      SAVE    /c_KW/
CBB      INCLUDE "KW.h"
      REAL*8      WtMain
*
      COMMON / wgtall / m_WtCrud,m_WtMod,m_WtSet(100)
      REAL*8            m_WtCrud,m_WtMod,m_WtSet
*--------------------------------------------------------------
      WtMain = m_WtMod   ! the best total weight
*
      END                       !!! KW_GetWt


      SUBROUTINE KW_GetWtAll(WtMain,WtCrud,WtSet)
*///////////////////////////////////////////////////////////////////
*//   Weights ALL                                                 //
*///////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INTEGER     m_Npar
      REAL*8      m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL*8      m_Xcrude,  m_svar,      m_alpha_s,   Preco,    Wtu
      REAL*8      m_XSecMC,  m_XErrMC
      REAL*8      m_XSecNR,  m_XErrNR
      INTEGER     m_KeyWgt,  m_KeySmp,    m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru,  IevAcc,      NevTot
      INTEGER     m_i_4f,    m_i_beta
*
      COMMON   /c_KW/
     $  m_Npar( 1000),           ! old Npar local Temporary/Obsolete
     $  m_Xpar(10000),           ! Xpar local
     $  m_XSecMC,                ! Final Main Best Monte Carlo Cross Section [pb]
     $  m_XErrMC,                ! and its error [pb]
     $  m_XSecNR,                ! Normalization Carlo Cross Section [pb]
     $  m_XErrNR,                ! and its error [pb]
     $  m_svar,                  ! CMSene**2
     $  m_alpha_s,               ! Alpha strong
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta                 ! Monitoring
*
      SAVE    /c_KW/
CBB      INCLUDE "KW.h"
*
      COMMON / wgtall / m_WtCrud,m_WtMod,m_WtSet(100)
      REAL*8            m_WtCrud,m_WtMod,m_WtSet
*
      INTEGER  j
      REAL*8   WtMain,WtCrud,WtSet(*)
*--------------------------------------------------------------
      WtMain = m_WtMod   ! the best total weight
      WtCrud = m_WtCrud  ! Crude weight (helps to avoid bad events)
      DO j=1,100
         WtSet(j) = m_WtSet(j)
      ENDDO
      END                       !!! KW_GetWtAll

      SUBROUTINE KW_GetMomDec(p1,p2,p3,p4)
*//////////////////////////////////////////////////////////////////////
*//   Final parton momenta                                           //
*//////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INTEGER     m_Npar
      REAL*8      m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL*8      m_Xcrude,  m_svar,      m_alpha_s,   Preco,    Wtu
      REAL*8      m_XSecMC,  m_XErrMC
      REAL*8      m_XSecNR,  m_XErrNR
      INTEGER     m_KeyWgt,  m_KeySmp,    m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru,  IevAcc,      NevTot
      INTEGER     m_i_4f,    m_i_beta
*
      COMMON   /c_KW/
     $  m_Npar( 1000),           ! old Npar local Temporary/Obsolete
     $  m_Xpar(10000),           ! Xpar local
     $  m_XSecMC,                ! Final Main Best Monte Carlo Cross Section [pb]
     $  m_XErrMC,                ! and its error [pb]
     $  m_XSecNR,                ! Normalization Carlo Cross Section [pb]
     $  m_XErrNR,                ! and its error [pb]
     $  m_svar,                  ! CMSene**2
     $  m_alpha_s,               ! Alpha strong
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta                 ! Monitoring
*
      SAVE    /c_KW/
CBB      INCLUDE "KW.h"
      REAL*8          p1(4), p2(4), p3(4), p4(4)   
*
      COMMON /momdec/ m_q1(4),m_q2(4),m_p1(4),m_p2(4),m_p3(4),m_p4(4)
      REAL*8          m_q1   ,m_q2   ,m_p1   ,m_p2   ,m_p3   ,m_p4   
      INTEGER  j
*--------------------------------------------------------------
      DO j=1,4
         p1(j) = m_p1(j)
         p2(j) = m_p2(j)
         p3(j) = m_p3(j)
         p4(j) = m_p4(j)
      ENDDO
      END                       !!! KW_GetMomDec

      SUBROUTINE KW_GetBeams(q1,q2)
*///////////////////////////////////////////////////////////////////////
*//   Four-momenta of beams                                           //
*///////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
CBB      INCLUDE "KW.h"
      REAL*8  q1(4),q2(4)
*
      COMMON /momdec/ m_q1(4),m_q2(4),m_p1(4),m_p2(4),m_p3(4),m_p4(4)
      REAL*8          m_q1   ,m_q2   ,m_p1   ,m_p2   ,m_p3   ,m_p4   
      INTEGER k
*--------------------------------------------------------------
      DO k=1,4
         q1(k) = m_q1(k)
         q2(k) = m_q2(k)
      ENDDO
      END                       !!! KW_GetBeams

      SUBROUTINE KW_GetPhotAll(NphAll,PhoAll)
*///////////////////////////////////////////////////////////////////////
*//                                                                   //
*//   Get all photons, note that they are ordered in energy           //
*//                                                                   //
*///////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
CBB      INCLUDE "KW.h"
*
      INTEGER  NphAll
      REAL*8   PhoAll(100,4)
*
      COMMON / momset / 
     $     m_qeff1(4),m_qeff2(4),m_sphum(4),m_sphot(100,4),m_nphot
      REAL*8      m_qeff1, m_qeff2, m_sphum, m_sphot
      INTEGER     m_nphot
*
      INTEGER  j,k
*------------------
      NphAll = m_nphot
      DO j=1,100
         DO k=1,4
            PhoAll(j,k) = m_sphot(j,k)
         ENDDO
      ENDDO
      END                       !!! KW_GetPhotAll !!!

      SUBROUTINE KW_GetNevMC(NevMC)
*/////////////////////////////////////////////////////////////////////////
*//   Number of generated events from MC generator                      //
*//   CALL KW_Finalize before !!!                                       //
*/////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INTEGER     m_Npar
      REAL*8      m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL*8      m_Xcrude,  m_svar,      m_alpha_s,   Preco,    Wtu
      REAL*8      m_XSecMC,  m_XErrMC
      REAL*8      m_XSecNR,  m_XErrNR
      INTEGER     m_KeyWgt,  m_KeySmp,    m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru,  IevAcc,      NevTot
      INTEGER     m_i_4f,    m_i_beta
      COMMON   /c_KW/
     $  m_Npar( 1000),           ! old Npar local Temporary/Obsolete
     $  m_Xpar(10000),           ! Xpar local
     $  m_XSecMC,                ! Final Main Best Monte Carlo Cross Section [pb]
     $  m_XErrMC,                ! and its error [pb]
     $  m_XSecNR,                ! Normalization Carlo Cross Section [pb]
     $  m_XErrNR,                ! and its error [pb]
     $  m_svar,                  ! CMSene**2
     $  m_alpha_s,               ! Alpha strong
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta                 ! Monitoring
*
      SAVE    /c_KW/
CBB      INCLUDE "KW.h"
      INTEGER       NevMC
*--------------------------------------------------------------
      NevMC = M_NevTru
      END                       !!! KW_GetNevTot

      SUBROUTINE KW_GetXSecNR(XSecNR,XErrNR)
*////////////////////////////////////////////////////////////////////////
*//   Normalization   Monte Carlo Cross Section [pb] and its error     //
*//   To be used for normalization of histograms                       //
*//   CALL KW_Finalize before using this !!!!                          //
*//   For KeyWgt = 0    XSecNR =    XSecMC                             //
*//   For KeyWgt = 1    XSecNR =    XCruMC                             //
*////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INTEGER     m_Npar
      REAL*8      m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL*8      m_Xcrude,  m_svar,      m_alpha_s,   Preco,    Wtu
      REAL*8      m_XSecMC,  m_XErrMC
      REAL*8      m_XSecNR,  m_XErrNR
      INTEGER     m_KeyWgt,  m_KeySmp,    m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru,  IevAcc,      NevTot
      INTEGER     m_i_4f,    m_i_beta
      COMMON   /c_KW/
     $  m_Npar( 1000),           ! old Npar local Temporary/Obsolete
     $  m_Xpar(10000),           ! Xpar local
     $  m_XSecMC,                ! Final Main Best Monte Carlo Cross Section [pb]
     $  m_XErrMC,                ! and its error [pb]
     $  m_XSecNR,                ! Normalization Carlo Cross Section [pb]
     $  m_XErrNR,                ! and its error [pb]
     $  m_svar,                  ! CMSene**2
     $  m_alpha_s,               ! Alpha strong
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta                 ! Monitoring
*
      SAVE    /c_KW/
CBB      INCLUDE "KW.h"
      REAL*8        XSecNR,XErrNR
*--------------------------------------------------------------
      XSecNR = m_XSecNR
      XErrNR = m_XErrNR
      END                       !!! KW_GetXSecNR


      SUBROUTINE KW_GetXSecMC(XSecMC,XErrMC)
*////////////////////////////////////////////////////////////////////////
*//   Final Main Best Monte Carlo Cross Section [pb] and its error     //
*//   CALL KW_Finalize before !!!                                      //
*////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INTEGER     m_Npar
      REAL*8      m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL*8      m_Xcrude,  m_svar,      m_alpha_s,   Preco,    Wtu
      REAL*8      m_XSecMC,  m_XErrMC
      REAL*8      m_XSecNR,  m_XErrNR
      INTEGER     m_KeyWgt,  m_KeySmp,    m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru,  IevAcc,      NevTot
      INTEGER     m_i_4f,    m_i_beta
      COMMON   /c_KW/
     $  m_Npar( 1000),           ! old Npar local Temporary/Obsolete
     $  m_Xpar(10000),           ! Xpar local
     $  m_XSecMC,                ! Final Main Best Monte Carlo Cross Section [pb]
     $  m_XErrMC,                ! and its error [pb]
     $  m_XSecNR,                ! Normalization Carlo Cross Section [pb]
     $  m_XErrNR,                ! and its error [pb]
     $  m_svar,                  ! CMSene**2
     $  m_alpha_s,               ! Alpha strong
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta                 ! Monitoring
*
      SAVE    /c_KW/
CBB      INCLUDE "KW.h"
      REAL*8        XSecMC,XErrMC
*--------------------------------------------------------------
      XSecMC = m_XSecMC
      XErrMC = m_XErrMC
      END                       !!! KW_GetXSecMC

*////////////////////////////////////////////////////////////////////////
*//                                                                    //
*//   End of Class KW                                                  //
*//                                                                    //
*////////////////////////////////////////////////////////////////////////


      SUBROUTINE decay(sprim,rndm,label)
****************************************
! this is 'buffor' routine that calls make_decay and then 
! fills up necessary commons for decay state
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / decdat / amafin(20), br(20)
! Codes, br ratios and masses for final particles 
      COMMON / decays / iflav(4), amdec(4) 
      SAVE / decays /,/ decdat /
      SAVE

      DIMENSION ipdg(4)
      CHARACTER*3 chuman(4)

! actual generation of final state type
      CALL make_decay(label,sprim,rndm)

! Assigning masses and codes to final particles according to 
! PDG coding convention (1992)
      CALL linear_to_pdg_label(1,label,ipdg,chuman)
      DO i=1,4
        iflav(i) = ipdg(i)
        amdec(i) = amafin( abs(iflav(i)))
      ENDDO

      END  

      FUNCTION get_decay_prob(sprim,label)
************************************************************
c gets the decay channel probability factor
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER(max_e_bin=1000,max_label=202)
      COMMON /c_decay/ prob_chan(max_e_bin,max_label),       !plain pr.
     $                 prob_chan_cumul(max_e_bin,max_label), !cumul. pr
     $                 prob_e_total(max_e_bin),              !total pr.
     $                 emin,emax,      ! range of svar in prob. matrices
     $                 umask_lbl(max_label)  ! extrnal mask on decay ch
      SAVE /c_decay/
CBB      INCLUDE 'decay.h'

! we take value from nearest smaller energy. NO interpolation so far.
      estep=(emax-emin)/(max_e_bin-1)
      ielower=int( (dsqrt(sprim)-emin)/estep ) +1
      gdp=prob_chan(ielower,label)

      get_decay_prob=gdp

c      write(6,*)'get_decay_prob=',gdp

      END

      FUNCTION get_total_crude(sprim)
************************************************************
c gets the total (summed over channels) crude photonic distribution
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER(max_e_bin=1000,max_label=202)
      COMMON /c_decay/ prob_chan(max_e_bin,max_label),       !plain pr.
     $                 prob_chan_cumul(max_e_bin,max_label), !cumul. pr
     $                 prob_e_total(max_e_bin),              !total pr.
     $                 emin,emax,      ! range of svar in prob. matrices
     $                 umask_lbl(max_label)  ! extrnal mask on decay ch
      SAVE /c_decay/
CBB      INCLUDE 'decay.h'

! we take value from nearest smaller energy. NO interpolation so far.
      estep=(emax-emin)/(max_e_bin-1)
      ielower=int( (dsqrt(sprim)-emin)/estep ) +1
      gtc=prob_e_total(ielower)

      get_total_crude=gtc

      END

      SUBROUTINE make_decay(label,sprim,rndm)
************************************************************
c chooses the decay channel
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER(max_e_bin=1000,max_label=202)
      COMMON /c_decay/ prob_chan(max_e_bin,max_label),       !plain pr.
     $                 prob_chan_cumul(max_e_bin,max_label), !cumul. pr
     $                 prob_e_total(max_e_bin),              !total pr.
     $                 emin,emax,      ! range of svar in prob. matrices
     $                 umask_lbl(max_label)  ! extrnal mask on decay ch
      SAVE /c_decay/
CBB      INCLUDE 'decay.h'
!

! we take value from nearest smaller energy. NO interpolation so far.
      estep=(emax-emin)/(max_e_bin-1)
      ielower=int( (dsqrt(sprim)-emin)/estep ) +1
! highly inefficient, to be optimized later, 
      DO ilab=1,max_label
        IF (rndm .LE. prob_chan_cumul(ielower,ilab)) GOTO 12
      ENDDO
      WRITE(6,*)'make_decay=>sth. is wrong'
      STOP
 12   CONTINUE

c      if(ilab.gt.1) then
c      write(6,*)'prob_chan_cumul=',
c     $   prob_chan_cumul(ielower,ilab)-prob_chan_cumul(ielower,ilab-1)
c      else
c      write(6,*)'prob_chan_cumul=',
c     $   prob_chan_cumul(ielower,ilab)
c      endif

      label=ilab

      END

      SUBROUTINE linear_to_pdg_label(mode,label,ipdg,chuman)
********************************
! converts linear labels to pdg and human conventions (mode=1)
! DOES NOT work back (yet) but it would be a fairly good tool
! 1-81: WW Wp=1:1-9; 2:10-18..
! 82-202: ZZ Z1=1:82-92; 2:93-103..

      IMPLICIT DOUBLE PRECISION (a-h,o-z)

      DIMENSION ipdg(4)
      DIMENSION icod (20)
      DIMENSION icodz(20)
      CHARACTER*3 chuman(4)
      CHARACTER*2 clabl(20)

      SAVE

      DATA init /0/

      IF(mode.NE.1) THEN
        WRITE(6,*)'linear_to_pdg_label=> only mode=1 implemented'
        STOP
      ENDIF

      IF(init.EQ.0) THEN
        init=1
! Codes for WW final state flavors
        icod(1)=102
        icod(2)=104
        icod(3)=302
        icod(4)=304
        icod(5)=502
        icod(6)=504
        icod(7)=1112
        icod(8)=1314
        icod(9)=1516
! Codes for ZZ final state flavors
        icodz(1)=101
        icodz(2)=202
        icodz(3)=303
        icodz(4)=404
        icodz(5)=505
        icodz(6)=1111
        icodz(7)=1313
        icodz(8)=1515
        icodz(9)=1212
        icodz(10)=1414
        icodz(11)=1616

! human readable
        clabl(1) ='dq'
        clabl(2) ='uq'
        clabl(3) ='sq'
        clabl(4) ='cq'
        clabl(5) ='bq'
        clabl(6) ='tq'

        clabl(11) ='el'
        clabl(12) ='ne'
        clabl(13) ='mu'
        clabl(14) ='nm'
        clabl(15) ='ta'
        clabl(16) ='nt'

      ENDIF


      CALL linear_to_WZ_label(1,label,icwm,icwp,if_z,if_w)

! Assigning codes to final particles according to PDG
! coding convention (1992)
      IF(if_w .EQ. 1) THEN
!-- WW
          ipdg(1)= icod(icwm)/100
          ipdg(2)=-mod(icod(icwm),100)
          ipdg(3)= mod(icod(icwp),100)
          ipdg(4)=-icod(icwp)/100
      ELSEIF(if_z .EQ. 1) THEN
!-- ZZ
          ipdg(1)= icodz(icwm)/100
          ipdg(2)=-mod(icodz(icwm),100)
          ipdg(3)= mod(icodz(icwp),100)
          ipdg(4)=-icodz(icwp)/100
      ENDIF

! Assigning human labels
      DO i=1,4
        IF(ipdg(i).LT.0) THEN
          chuman(i)= '~'//clabl(abs(ipdg(i)))
        ELSE
          chuman(i)= ' '//clabl(abs(ipdg(i)))
        ENDIF
      ENDDO

      END




      SUBROUTINE linear_to_WZ_label(mode,label,icwm,icwp,ifznow,ifwnow)
********************************
! converts linear labels to KoralW.13x convention (mode=1) 
! and back (mode=-1) 
! 1-81: WW Wp=1:1-9; 2:10-18..
! 82-202: ZZ Z1=1:82-92; 2:93-103..

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IF(mode .EQ. 1) THEN
!--======================= 
!-- prepare W/Z type labelling
        IF(label .LE. 81) THEN
!-- WW
          ifwnow=1
          ifznow=0
          icwp=(label-1)/9 +1
          icwm=mod(label-1,9)+1
        ELSEIF(label .LE. 202) THEN
!-- ZZ
          ifznow=1
          ifwnow=0
          icwp=(label-81-1)/11 +1
          icwm=mod((label-81-1),11)+1
        ELSE
          WRITE(6,*)'linear_to_WZ_label=> stop 1',1/(mode-1)
          STOP
        ENDIF
      ELSEIF(mode .EQ. -1) THEN
!--======================= 
!-- prepare linear labelling
        IF(ifwnow .EQ. 1) THEN
          label=(icwp-1)*9+icwm
        ELSEIF(ifznow .EQ. 1) THEN
          label=81+(icwp-1)*11 +icwm
        ELSE
          WRITE(6,*)'linear_to_WZ_label=> stop 2'
          STOP
        ENDIF
      ELSE
!-- ======================
        WRITE(6,*)'linear_to_WZ_label=> stop 3'
        STOP
      ENDIF
!-- ======================

      END

      SUBROUTINE umask_init(user_umask)
*******************************************************
! lblmin,lblmax give the range of channels to be generated
! umask_lbl(max_label) is a user mask for suppressing certain final states on
! request. only 0 or 1 are expected.
! matrices to be filled:
! prob_chan(ienergy,label)
!   each bin contains value from its beginning. last bin=smax
! prob_chan_cumul(ienergy,label) is cumulative in index label starting
! from label=1  

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER(max_e_bin=1000,max_label=202)
      COMMON /c_decay/ prob_chan(max_e_bin,max_label),       !plain pr.
     $                 prob_chan_cumul(max_e_bin,max_label), !cumul. pr
     $                 prob_e_total(max_e_bin),              !total pr.
     $                 emin,emax,      ! range of svar in prob. matrices
     $                 umask_lbl(max_label)  ! extrnal mask on decay ch
      SAVE /c_decay/
CBB      INCLUDE 'decay.h'
      DIMENSION user_umask(*)


! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! I/O unit numbers
      COMMON / INOUT  / NINP,NOUT     

      CHARACTER*3 chuman(4)

      DIMENSION iflav(4)

      KeyZon = MOD(KeyMis,10000)/1000
      KeyWon = MOD(KeyMis,100000)/10000

      DO i=1,max_label
        umask_lbl(i)=0d0
      ENDDO

      IF( keywon .EQ. 1 .AND.
     $    keyzon .EQ. 1 .AND.
     $    keydwm .EQ. 0 .AND.
     $    keydwp .EQ. 0       ) THEN
! read user's umask only here
        DO i=1,202
          umask_lbl(i)=user_umask(i)
        ENDDO

      ELSEIF( keywon .EQ. 1 .AND.
     $        keyzon .EQ. 0 .AND.
     $        keydwm .EQ. 0 .AND.
     $        keydwp .EQ. 0       ) THEN
! WW inclusive
        DO i=1,81
          umask_lbl(i)=1d0
        ENDDO
!... mix must be added in ZZ part, but only: ussu ubbu cddc cbbb
        DO label=82,202
          CALL linear_to_pdg_label(1,label,iflav,chuman)

          DO I=1,2
            IA=iflav(2*I-1)
            IB=iflav(5-2*I)

            IF( 
     $        (IA.eq. 3.and.IB.eq. 2).or.  ! s   u
     $        (IA.eq. 5.and.IB.eq. 2).or.  ! b   u
     $        (IA.eq. 1.and.IB.eq. 4).or.  ! d   c
     $        (IA.eq. 5.and.IB.eq. 4)      ! b   c
     $        )  THEN
!-- mix detected
              umask_lbl(label)=1d0
            ENDIF
          ENDDO
        ENDDO
! end mix-adding
!... mix must be added in ZZ part, but only: ussu ubbu cddc cbbb
      ELSEIF( keywon .EQ. 0 .AND.
     $        keyzon .EQ. 1 .AND.
     $        keydwm .EQ. 0 .AND.
     $        keydwp .EQ. 0       ) THEN
! ZZ inclusive
        DO i=82,202
          umask_lbl(i)=1d0
        ENDDO
!... mix must be added in WW part, but only: uudd, ccss, llll
        DO ibra=7,9 ! leptons
          CALL linear_to_WZ_label(-1,label_mix,ibra,ibra,0,1)
          umask_lbl(label_mix)=1d0
        ENDDO
        ibra=1 ! uddu
        CALL linear_to_WZ_label(-1,label_mix,ibra,ibra,0,1)
        umask_lbl(label_mix)=1d0
        ibra=4 ! cssc
        CALL linear_to_WZ_label(-1,label_mix,ibra,ibra,0,1)
        umask_lbl(label_mix)=1d0
      ELSEIF( keywon .EQ. 1 .AND.
     $        keyzon .EQ. 0 .AND.
     $        keydwm .NE. 0 .AND.
     $        keydwp .NE. 0       ) THEN
! WW exclusive
          CALL linear_to_WZ_label(-1,label,keydwm,keydwp,keyzon,keywon)
          umask_lbl(label)=1d0
      ELSEIF( keywon .EQ. 0 .AND.
     $        keyzon .EQ. 1 .AND.
     $        keydwm .NE. 0 .AND.
     $        keydwp .NE. 0       ) THEN
! ZZ exclusive
          CALL linear_to_WZ_label(-1,label,keydwm,keydwp,keyzon,keywon)
          umask_lbl(label)=1d0
      ELSE
        WRITE(6,*)'umask_init=> do not know this flavor setting'
        STOP
      ENDIF

! now we kill the mix-type final states in ZZ configuration
      DO label=82,202
        CALL linear_to_pdg_label(1,label,iflav,chuman)

        DO I=1,2
          IA=iflav(2*I-1)
          IB=iflav(5-2*I)

          IF( 
     $        (IA.eq.11 .and. IB.eq.12) .or.  ! e   nue
     $        (IA.eq.13 .and. IB.eq.14) .or.  ! mu  numu
     $        (IA.eq.15 .and. IB.eq.16) .or.  ! tau nutau
     $        (IA.eq. 1 .and. IB.eq. 2) .or.  ! d   u
     $        (IA.eq. 3 .and. IB.eq. 4)       ! s   c
     $      )  THEN
!-- mix detected
            umask_lbl(label)=0d0
          ENDIF
        ENDDO
      ENDDO
! ...and in WW configurations...
      DO ibra=2,3 ! leptons
        CALL linear_to_WZ_label(-1,label_mix,ibra,ibra,0,1)
        umask_lbl(label_mix)=0d0
      ENDDO
      DO ibra=5,6 ! leptons
        CALL linear_to_WZ_label(-1,label_mix,ibra,ibra,0,1)
        umask_lbl(label_mix)=0d0
      ENDDO
! end mix-killing

! additional stuff from selecto !

* kill DOUBLE counting in different zz final states.
      DO label=82,202
        CALL linear_to_WZ_label(1,label,iwm,iwp,if_z,if_w)
        IF(iwm .GT. iwp) umask_lbl(label)=0d0
      ENDDO

* remove e+e-e+e- final state.  this is desactivated !!!!!
* it may be useful. will SAVE a lot of cpu time IF you DO not need
* these states.
*     CALL linear_to_WZ_label(-1,label,6,6,1,0)
*     umask_lbl(label)=0d0

! end selecto stuff !

! dump umask for control
      WRITE(6,*)'umask_init=>umask:'
      DO i=1,9
        WRITE(6,'(9f5.1)') (umask_lbl((i-1)*9+j),j=1,9)
      ENDDO
      DO i=1,11
        WRITE(6,'(11f5.1)') (umask_lbl(81+(i-1)*11+j),j=1,11)
      ENDDO
!!!      OPEN(unit=9,file='umask_actual')
      WRITE(nout,*)'umask_init=>umask:'
      DO i=1,9  
        WRITE(nout,'(9f5.1)') (umask_lbl((i-1)*9+j),j=1,9)
      ENDDO
      DO i=1,11
        WRITE(nout,'(11f5.1)') (umask_lbl(81+(i-1)*11+j),j=1,11)
      ENDDO
!!!      CLOSE(9)
! Check if open at least one channel
      mark=0
      DO i=1,202
        IF(umask_lbl(i).NE.0d0) mark=1
      ENDDO
      IF(mark.EQ.0) THEN
        WRITE(6,*)'umask_init=> There are no opened decay channels'
        WRITE(6,*)'umask_init=> Check your input cards, STOP'
        STOP  
      ENDIF

      END

      SUBROUTINE decay_prob_init(emi,ema,user_umask,
     @                           i_file,KeyWgt,keysmp)
******************************************************************
! umask_lbl(max_label) is a user mask for suppressing certain final states on
! request. only 0 or 1 are expected.
! matrices to be filled:
! prob_chan(ienergy,label)
!   each bin contains value from its beginning. last bin=emax
! prob_chan_cumul(ienergy,label) is cumulative in index label starting
! from label=1  
! this routine generates probabilities from the 
! -analytical function  phot_spec_crud (i_file=0)
! -pretabulated data (i_file=1)
!    (keywgt<>0 -pretabulated xsections)
!    (keywgt =0 -pretabulated wtmax)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER(max_e_bin=1000,max_label=202)
      COMMON /c_decay/ prob_chan(max_e_bin,max_label),       !plain pr.
     $                 prob_chan_cumul(max_e_bin,max_label), !cumul. pr
     $                 prob_e_total(max_e_bin),              !total pr.
     $                 emin,emax,      ! range of svar in prob. matrices
     $                 umask_lbl(max_label)  ! extrnal mask on decay ch
      SAVE /c_decay/
CBB      INCLUDE 'decay.h'
      DIMENSION user_umask(202)

! fill in smin and smax
      emin=emi
      emax=ema
! read umask_lbl
      CALL umask_init(user_umask)

      estep=(emax-emin)/(max_e_bin-1)
      eloc=emin
      sloc=emin**2
      smax=emax**2
      DO iebin=1,max_e_bin
        prob_e_total(iebin)=0d0
        DO label=1,max_label 
          prob_chan(iebin,label) =
     $      give_phot_spec_crud(smax,sloc,label,i_file,KeyWgt,keysmp)
     $                           *umask_lbl(label)
          prob_e_total(iebin) =prob_e_total(iebin)
     $                        +prob_chan(iebin,label)
        ENDDO
        eloc=eloc+estep
        sloc=eloc**2
      ENDDO
c      write(6,*)'prob_e_total',prob_e_total
c      write(6,*)'prob_e_total',i_file,keywgt
c      write(6,*)'prob_e_total',umask_lbl
c      write(6,*)'eloc,estep,emax,emin',eloc,estep,ema,emi,max_e_bin
      IF( abs( (eloc-estep)/emax-1 ) .GT. 1d-8 ) THEN
        WRITE(6,*)'decay_prob_init=> sth. is wrong'
        STOP
      ENDIF

      DO iebin=1,max_e_bin
        prcum=0d0
        DO label=1,max_label
          prob_chan(iebin,label) =prob_chan(iebin,label)
     $                           /prob_e_total(iebin)
          prcum=prcum+prob_chan(iebin,label)
          prob_chan_cumul(iebin,label)=prcum
        ENDDO
        IF( abs( prcum-1 ) .GT. 1d-12 ) THEN
          WRITE(6,*)'decay_prob_init=> 2 sth. is wrong'
          STOP
        ENDIF
      ENDDO

      WRITE(6,*)'decay_prob_init=> photonic pretabulation done'

      END

      FUNCTION give_phot_spec_crud(svar,sprim,
     @                             label,i_file,KeyWgt,keysmp)
******************************************************************
! provides crude photonic spectrum either from 
! pretabulated file (i_file=1),     or from
!    (keywgt<>0 -pretabulated xsections)
!    (keywgt =0 -pretabulated wtmax)
! analytic function (i_file=0) as of v. 1.33

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER(max_e_bin=1000,max_label=202)
      COMMON /c_decay/ prob_chan(max_e_bin,max_label),       !plain pr.
     $                 prob_chan_cumul(max_e_bin,max_label), !cumul. pr
     $                 prob_e_total(max_e_bin),              !total pr.
     $                 emin,emax,      ! range of svar in prob. matrices
     $                 umask_lbl(max_label)  ! extrnal mask on decay ch
      SAVE /c_decay/
CBB      INCLUDE 'decay.h'
      PARAMETER (n_ene=100) ! number of actual energy bins in file
                            ! do not confuse with internal binning
      DIMENSION xsec_lcl(n_ene,max_label)
      DIMENSION wtma_lcl(n_ene,max_label)
      CHARACTER*80 name_wt, name_xs

      DATA init /0/

      SAVE

      IF(init .EQ. 0 .AND. i_file .EQ. 1) THEN
        DO ilbl=1,max_label
          DO iene=1,n_ene
            xsec_lcl(iene,ilbl)=1
          ENDDO
        ENDDO
        init=1
c        IF(KeyWgt .EQ. 0) THEN
        IF(KeySmp .EQ. 1) THEN
          name_wt='data_wtmax.fit.smp1'
          name_xs='data_xsect.fit'
! tobedone        ELSEIF(KeySmp .EQ. 3) THEN
! tobedone          name_wt='data_wtmax.fit.smp3'
! tobedone          name_xs='data_xsect.fit'
        ELSE
          name_wt='data_wtmax.fit.smp2'
          name_xs='data_xsect.fit'
        ENDIF

! read wtmax
        CALL open_data(name_wt,name_wt,io_number)
        DO ilbl=1,max_label
          IF(umask_lbl(ilbl) .NE. 0d0) THEN
ccc            write(6,*)'ddd',umask_lbl(ilbl),ilbl
 200        CONTINUE
            READ(io_number,*) lbl_lcl,eminx,emaxx
            READ(io_number,*) ( wtma_lcl(iene,ilbl), iene=1,n_ene )
            IF(ilbl .GT. lbl_lcl) THEN
              WRITE(6,*)'give_phot_spec_crud=>wt skipped label ',lbl_lcl
              GOTO 200
            ELSEIF(ilbl .LT. lbl_lcl) THEN
              WRITE(6,*)'give_phot_spec_cr=>wt not found label ',ilbl
              STOP
            ENDIF
          ENDIF
        ENDDO
        CALL close_data(name_wt,name_wt,io_number)

        WRITE(6,*)' '

! read xsectn
        CALL open_data(name_xs,name_xs,io_number)
        DO ilbl=1,max_label
          IF(umask_lbl(ilbl) .NE. 0d0) THEN
ccc            write(6,*)'ddd',umask_lbl(ilbl),ilbl
 201         CONTINUE
            READ(io_number,*) lbl_lcl,eminx,emaxx
            READ(io_number,*) ( xsec_lcl(iene,ilbl), iene=1,n_ene )
            IF(ilbl .GT. lbl_lcl) THEN
              WRITE(6,*)'give_phot_spec_crud=>xs skipped label ',lbl_lcl
              GOTO 201
            ELSEIF(ilbl .LT. lbl_lcl) THEN
              WRITE(6,*)'give_phot_spec_cr=>xs not found label ',ilbl
              STOP
            ENDIF
          ENDIF
        ENDDO
        CALL close_data(name_xs,name_xs,io_number)

        DO ilbl=1,max_label
          IF(umask_lbl(ilbl) .NE. 0d0) THEN
            DO iene=1,n_ene
c              xsec_lcl(iene,ilbl)=dsqrt( 
c     #                  xsec_lcl(iene,ilbl)*wtma_lcl(iene,ilbl) )
c              xsec_lcl(iene,ilbl)=1/2d0*( 
c     #                  xsec_lcl(iene,ilbl)+wtma_lcl(iene,ilbl) )
              IF(KeySmp .EQ. 0) THEN
                xsec_lcl(iene,ilbl)=
     #                  xsec_lcl(iene,ilbl)
              ELSE
                xsec_lcl(iene,ilbl)=
     #                  wtma_lcl(iene,ilbl) 
              ENDIF
            ENDDO
          ENDIF
        ENDDO

        ebinx= (emaxx-eminx)/(n_ene-1)

      ENDIF

      IF(i_file .EQ. 0) THEN
        g_p_s_c=phot_spec_crud(svar,sprim,label)
      ELSEIF(i_file .EQ. 1) THEN
        ie_lcl=nint((dsqrt(sprim)-eminx)/ebinx)
        IF(ie_lcl .GT. n_ene) ie_lcl=n_ene
        IF(ie_lcl .LT. 1) ie_lcl=1
        g_p_s_c=xsec_lcl(ie_lcl,label)
      ENDIF

        give_phot_spec_crud=g_p_s_c

      END

      SUBROUTINE decay_monit(mode,wtmod,crud,svar,label,nout)
************************************************
! bookkeeping routine for monitoringg xsections in different flavor
! decay channels, used also to pre-tabulate the photonic spectra
! label_dumm later on to be converted into real label (when relocated
! to karludw. for now label comes from get_label routine
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER(max_e_bin=1000,max_label=202)
      COMMON /c_decay/ prob_chan(max_e_bin,max_label),       !plain pr.
     $                 prob_chan_cumul(max_e_bin,max_label), !cumul. pr
     $                 prob_e_total(max_e_bin),              !total pr.
     $                 emin,emax,      ! range of svar in prob. matrices
     $                 umask_lbl(max_label)  ! extrnal mask on decay ch
      SAVE /c_decay/
CBB      INCLUDE 'decay.h'

      PARAMETER (label_max=202)
      DIMENSION num_phot(label_max)
      DIMENSION num_phot_non0(label_max)
      DIMENSION wgt_phot(label_max)
      DIMENSION wgt2_phot(label_max)
      DIMENSION wt_biggest(label_max)
      DIMENSION wt_smallest(label_max)
      DIMENSION ipdg(4)
      REAL*4 rewt_biggest(label_max)
      REAL*4 r0wt_biggest(label_max)
      REAL*4 rewgt_phot(label_max)
      CHARACTER*3 chuman(4)
! wtover is inactive until wtmax is settled
!      DIMENSION wgt_over(label_max)
      SAVE

!------------------------------
      IF(mode .EQ. -1) THEN
!------------------------------
        DO i=1,label_max
          num_phot(i)=0
          num_phot_non0(i)=0
          wgt_phot(i)=0d0
          wgt2_phot(i)=0d0
          wt_biggest(i)=0d0
        ENDDO
        ntot=0
!------------------------------
      ELSEIF(mode .EQ. 0) THEN
!------------------------------
        IF(label .GT. label_max) THEN
          WRITE(6,*)'phot_spec_book=> label_max too small:',label_max
          STOP
        ENDIF
        ntot=ntot+1
        IF( wtmod .GT. 0d0 ) 
     $     num_phot_non0(label) =num_phot_non0(label)+1
        num_phot(label) =num_phot(label)+1
        wgt_phot(label) =wgt_phot(label)+wtmod
        wgt2_phot(label)=wgt2_phot(label)+wtmod**2
        IF( wt_biggest(label).LT.wtmod ) wt_biggest(label)=wtmod
        IF( wt_smallest(label).GT.wtmod ) wt_smallest(label)=wtmod
!------------------------------
      ELSEIF(mode .EQ. 1) THEN
!------------------------------
! normalisation
c        DO i=1,label_max
c          wgt_phot(i ) =wgt_phot(i )*crud
c          wgt2_phot(i )=wgt2_phot(i )*crud**2
c          wt_biggest(i )=wt_biggest(i )*crud
c          wt_smallest(i )=wt_smallest(i )*crud
c        ENDDO
! printout 
        WRITE(nout,*)
        WRITE(nout,'(80a)') ' ',('*',iii=1,75)
        WRITE(nout,*)'                     '
     $      //'Decay Report on Different Channels '
        WRITE(nout,*)
        WRITE(nout,'(80a)')
     $    '                                            '
     $    //' wt_max  wt_max  nev_ch nev_non0'
        WRITE(nout,'(80a)')
     $    '  wm wp        human   sigma [pb] +- abs_err'
     $    //' ------ -------- ------ --------'
        WRITE(nout,'(80a)')
     $    '                                            '
     $    //'  <wt>  <wt_non0>   tot   nev_ch'
        WRITE(nout,*)
        xstot=0d0
        xstot2=0d0
        DO i=1,label_max
          IF( umask_lbl(i).GT.0d0 ) THEN
            xstot=xstot+wgt_phot(i)*crud
            xstot2=xstot2+wgt2_phot(i)*crud**2
            wgt_ph=wgt_phot(i)*crud/ntot
            wgt2_ph=wgt2_phot(i)*crud**2/ntot
            wterro=dsqrt((wgt2_ph -wgt_ph**2)/ntot)
            procevt=dble(num_phot(i))/dble(ntot)
c            procevt0=dble(num_phot_non0(i))/dble(ntot)
            IF( num_phot(i) .GT. 0d0) THEN
              procevt0=dble(num_phot_non0(i))/dble(num_phot(i))
            ELSE
              procevt0=0d0
            ENDIF
            IF(wgt_ph.GT.0d0) THEN
              wtma=wt_biggest(i)/wgt_phot(i) *num_phot(i)
              wtma_non0=wt_biggest(i)/wgt_phot(i) *num_phot_non0(i)
            ENDIF

            CALL linear_to_WZ_label(1,i,kwm,kwp,if_z,if_w)
            CALL linear_to_pdg_label(1,i,ipdg,chuman)

       WRITE(nout,
     $ '(a,2i3,a,4a3,a,e13.7,a,e8.2,a,e8.2,a,e8.2,a,f6.4,a,f6.4)')
     $        ' ',kwm,kwp,' ',
     $        chuman(1),chuman(2),chuman(3),chuman(4),
     $        ' ',real(wgt_ph),'+-',real(wterro),
     $        ' ',real(wtma),' ',real(wtma_non0),
     $        ' ',real(procevt),
     $        ' ',real(procevt0)
            WRITE(nout,*)' '
         ENDIF
        ENDDO
        xstot=xstot/ntot
        xstot2=xstot2/ntot
        ertot=dsqrt((xstot2 -xstot**2)/ntot)
        WRITE(nout,*)'   total xsection = ',xstot,' +- ',ertot,' [pb]'
        WRITE(nout,'(80a)') ' ',('*',iii=1,75)
        WRITE(nout,*)' '
!------------------------------
      ELSEIF(mode .EQ. 2) THEN
!------------------------------
! printout for pre-tabulation
        DO i=1,label_max
          rewgt_phot(i)=wgt_phot(i)*crud/ntot
!          procevt=dble(num_phot(i))/dble(ntot)
          IF(wgt_phot(i).GT.0d0) THEN
            r0wt_biggest(i)=wt_biggest(i)/wgt_phot(i) *num_phot_non0(i)
            rewt_biggest(i)=wt_biggest(i)/wgt_phot(i) *num_phot(i)
          ENDIF
        ENDDO
        OPEN(unit=19,file='spectrum',status='unknown')
        WRITE(19,*)svar,'  svar point'
        WRITE(19,*)rewgt_phot
        CLOSE(19)
        OPEN(unit=19,file='wtmax.non0',status='unknown')
        WRITE(19,*)svar,'  svar point'
        WRITE(19,*)r0wt_biggest
        CLOSE(19)
        OPEN(unit=19,file='wtmax',status='unknown')
        WRITE(19,*)svar,'  svar point'
        WRITE(19,*)rewt_biggest
        CLOSE(19)
!------------------------------
      ENDIF
!------------------------------

      END

      SUBROUTINE store_label(mode,label)
****************************************
! tempotary routine to carry label from karludw to koralw and wwborn 
! for the phot_spec_book and born only !!!!!
! later on I hope external matrix el. calculation will migrate
! to karludw also and then problem of label will disappear.
!
! NOT ANY MORE a temporary routine. Now it is used by few routines, in
! the place of / WorZ / common, M.S. 3/3/98, Knox. 
!
! mode 0=store
!      1=retrieve

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      SAVE ilabel

      IF(mode .EQ. 0) THEN
        ilabel=label
      ELSEIF(mode .EQ. 1) THEN
        label=ilabel
      ELSE
        WRITE(6,*) 'store_label=>wrong mode ',mode
      ENDIF

      END
      SUBROUTINE DUMPS(NOUT)                 
C     **********************                 
C THIS PRINTS OUT FOUR MOMENTA OF PHOTONS    
C ON UNIT NO. NOUT                           
C     **********************                 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT  
      SAVE   / MOMSET /
      SAVE
      DIMENSION SUM(4)                       
      WRITE(NOUT,*) '=====================DUMPS===================='
      WRITE(NOUT,3100) 'QF1',(  qeff1(  K),K=1,4) 
      WRITE(NOUT,3100) 'QF2',(  qeff2(  K),K=1,4) 
      DO 100 I=1,NPHOT                       
  100 WRITE(NOUT,3100) 'PHO',(SPHOT(I,K),K=1,4) 
      DO 200 K=1,4                           
  200 SUM(K)=qeff1(K)+qeff2(K)                   
      DO 210 I=1,NPHOT                       
      DO 210 K=1,4                           
  210 SUM(K)=SUM(K)+SPHOT(I,K)               
      WRITE(NOUT,3100) 'SUM',(  SUM(  K),K=1,4) 
 3100 FORMAT(1X,A3,1X,5F18.14)               
      END                                    

      subroutine moms(nout)
c     *********************
      implicit double precision (a-h,o-z)    
      common / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot  
      common / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      common / decays / iflav(4), amdec(4) 
      save   / momset /,/ momdec /,/ decays /
      save
 11   format(1G23.16,/,3G23.16,A5)
 12   format(1G23.16,/,3G23.16,I5)
      write(nout,11) qeff1(4),(qeff1(k),k=1,3),'  11'  
      write(nout,11) qeff2(4),(qeff2(k),k=1,3),' -11'
      write(nout,12) p4(4),(p4(k),k=1,3),iflav(4)
      write(nout,12) p3(4),(p3(k),k=1,3),iflav(3)
      write(nout,12) p1(4),(p1(k),k=1,3),iflav(1)  
      write(nout,12) p2(4),(p2(k),k=1,3),iflav(2)
      write(nout,*)
      end

      subroutine dumpw(nout)     
*     **********************     
c Prints out four-momenta and flavors of inermediate and 
c final particles on output unit nout
c 
c Written by: Wieslaw Placzek        date: 26.07.1994
c Last update: 27.07.1994            by: W.P.  
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / momset / QEFF1(4),QEFF2(4),sphum(4),sphot(100,4),nphot 
      common / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      common / decays / iflav(4), amdec(4) 
      save   / momset /,/ momdec /,/ decays /
      DOUBLE PRECISION sum(4)
       IF (IFLAV(1).eq.-iflav(2)) then
       iflwm= 23
       iflwp= 23
      else
       iflwm=-24
       iflwp= 24       
      endif
      iflph= 22
      write(nout,*) '=====================dumpw===================='
      write(nout,3200) ' p(1)',' p(2)',' p(3)',' p(4)',' pdg-code'
      do 100 i=1,nphot                       
  100 write(nout,3100) ' PHO',(sphot(i,k),k=1,4),iflph 
      write(nout,3100) 'Z/W-',(q1(k),k=1,4),iflwm   
      write(nout,3100) 'Z/W+',(q2(k),k=1,4),iflwp   
      write(nout,3100) '  p1',(p1(k),k=1,4),iflav(1)   
      write(nout,3100) '  p2',(p2(k),k=1,4),iflav(2)
      write(nout,3100) '  p3',(p3(k),k=1,4),iflav(3)   
      write(nout,3100) '  p4',(p4(k),k=1,4),iflav(4)
      do 101 k=1,4      
 101  sum(k)=p1(k)+p2(k)+p3(k)+p4(k)         
      do 210 i=1,nphot                       
      do 210 k=1,4                           
  210 sum(k)=sum(k)+sphot(i,k)               
      isfla=iflav(1)+iflav(2)+iflav(3)+iflav(4)
      write(nout,3100) 'sum',(sum(k),k=1,4),isfla 
      write(nout,*) '=============================================='  
 3100 format(1x,a4,1x,4f15.8,i7)   
c 3100 format(1x,a4,1x,5f18.14)               
 3200 format(5x,4a15,a10)
      end   

      subroutine dumpl(nout,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)     
*     ****************************************************************     
c Prints out four-momenta and flavors of inermediate and 
c final particles on output unit nout
c 
c Written by: M.Skrzypek        date: 17.03.95
c Last update:             by:  
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / decays / iflav(4), amdec(4) 
      save   / decays /
      DOUBLE PRECISION sum(4),pho(4),sphot(100,4)     
      dimension p1(4),p2(4),p3(4),p4(4),qeff1(4),qeff2(4)

      p4mass=dmas2(p4)
      p3mass=dmas2(p3)
      p2mass=dmas2(p2)
      p1mass=dmas2(p1)
      e2mass=dmas2(qeff2)
      e1mass=dmas2(qeff1)

      p1mass=dsign(dsqrt(abs(p1mass)),p1mass)
      p2mass=dsign(dsqrt(abs(p2mass)),p2mass)
      p3mass=dsign(dsqrt(abs(p3mass)),p3mass)
      p4mass=dsign(dsqrt(abs(p4mass)),p4mass)
      e1mass=dsign(dsqrt(abs(e1mass)),e1mass)
      e2mass=dsign(dsqrt(abs(e2mass)),e2mass)


      write(nout,*) '=====================dumpl===================='
      write(nout,3200) ' p(1)',' p(2)',' p(3)',' p(4)','  sign*mass'

      do 100 i=1,nphot    
      do 110 k=1,4    
 110  pho(k)=sphot(i,k)
      amphot=dmas2(pho)
      phmass=dsign(dsqrt(abs(amphot)),amphot)
  100 write(nout,3100) 'PHO',(sphot(i,k),k=1,4),phmass
      write(nout,3100) ' p1',(p1(k),k=1,4),p1mass,iflav(1)  
      write(nout,3100) ' p2',(p2(k),k=1,4),p2mass,iflav(2)
      write(nout,3100) ' p3',(p3(k),k=1,4),p3mass,iflav(3)
      write(nout,3100) ' p4',(p4(k),k=1,4),p4mass,iflav(4)
      write(nout,3100) 'qf1',(qeff1(k),k=1,4),e1mass  
      write(nout,3100) 'qf2',(qeff2(k),k=1,4),e2mass
      do 101 k=1,4      
 101  sum(k)=p1(k)+p2(k)+p3(k)+p4(k)         
      do 210 i=1,nphot                       
      do 210 k=1,4                           
  210 sum(k)=sum(k)+sphot(i,k)               
      isfla=iflav(1)+iflav(2)+iflav(3)+iflav(4)
      e2mass=dmas2(sum)
      sumas=dsign(dsqrt(abs(e2mass)),e2mass)
      write(nout,3100) 'sum',(sum(k),k=1,4), sumas !,isfla 
      write(nout,*) '=============================================='  
 3100 format(1x,a3,5f21.15,i4)   
c 3100 format(1x,a3,1x,5f18.14)               
 3200 format(5x,4a22,a10)
      end   

      subroutine dumpb_unused(nout)     
*     **********************     
c Prints out four-momenta and flavors of inermediate and 
c final particles on output unit nout
c 
c Written by: M.Skrzypek        date: 17.03.95
c Last update:             by:  
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / momset / QEFF1(4),QEFF2(4),sphum(4),sphot(100,4),nphot
! / bormom / does not exist any more, m.s. 10/18/97
      common / bormom / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      common / decays / iflav(4), amdec(4) 
      save   / momset /,/ bormom /,/ decays /
      DOUBLE PRECISION sum(4),pho(4)     

      p4mass=dmas2(p4)
      p3mass=dmas2(p3)
      p2mass=dmas2(p2)
      p1mass=dmas2(p1)
      q2mass=dmas2(q2)
      q1mass=dmas2(q1)
      e2mass=dmas2(qeff2)
      e1mass=dmas2(qeff1)

      q1mass=dsign(dsqrt(abs(q1mass)),q1mass)
      q2mass=dsign(dsqrt(abs(q2mass)),q2mass)
      p1mass=dsign(dsqrt(abs(p1mass)),p1mass)
      p2mass=dsign(dsqrt(abs(p2mass)),p2mass)
      p3mass=dsign(dsqrt(abs(p3mass)),p3mass)
      p4mass=dsign(dsqrt(abs(p4mass)),p4mass)
      e1mass=dsign(dsqrt(abs(e1mass)),e1mass)
      e2mass=dsign(dsqrt(abs(e2mass)),e2mass)


      write(nout,*) '=====================dump born================'
      write(nout,3200) ' p(1)',' p(2)',' p(3)',' p(4)','  sign*mass'

      do 100 i=1,nphot    
      do 110 k=1,4    
 110  pho(k)=sphot(i,k)
      amphot=dmas2(pho)
      phmass=dsign(dsqrt(abs(amphot)),amphot)
  100 write(nout,3100) 'PHO',(sphot(i,k),k=1,4),phmass
      write(nout,3100) 'bW-',(q1(k),k=1,4),q1mass   
      write(nout,3100) 'bW+',(q2(k),k=1,4),q2mass 
      write(nout,3100) 'bp1',(p1(k),k=1,4),p1mass  
      write(nout,3100) 'bp2',(p2(k),k=1,4),p2mass
      write(nout,3100) 'bp3',(p3(k),k=1,4),p3mass   
      write(nout,3100) 'bp4',(p4(k),k=1,4),p4mass
      write(nout,3100) 'ef1',(qeff1(k),k=1,4),e1mass  
      write(nout,3100) 'ef2',(qeff2(k),k=1,4),e2mass
      do 101 k=1,4      
 101  sum(k)=p1(k)+p2(k)+p3(k)+p4(k)         
      do 210 i=1,nphot                       
      do 210 k=1,4                           
  210 sum(k)=sum(k)+sphot(i,k)               
      isfla=iflav(1)+iflav(2)+iflav(3)+iflav(4)
      write(nout,3100) 'sum',(sum(k),k=1,4) !,isfla 
      write(nout,*) '=============================================='  
 3100 format(1x,a3,1x,5f22.15)   
c 3100 format(1x,a3,1x,5f18.14)               
 3200 format(5x,4a22,a10)
      end   
      SUBROUTINE Filexp_Obsolete(Xpar,Npar)   
*////////////////////////////////////////////////////////////////////////////////
*//                                                                            //
*//  Filexp transfers input params form Xpar to common blocks                  //
*//  Actualy it does part of the job, KoralW(-1) does another part.            //
*//  Some parameters are modified according to values of Key's (filtering)     //
*//  Obsolete Npar is also defined, it should disappear in next version.       //
*//                                                                            //
*//  Xpar should be ONLY and ONLY an input object,                             //
*//  just image of the input data from the disk                                //
*//  it should never transfer any values between program units, because        //
*//  this doubles the role of common blocks, i.e. the same variables are       //
*//  communicated between routines through static commons and (pointer) xpar!! //
*//                                                                            //
*//  In actual version xpar re-defined in filexp is used in only in            //
*//         ampli4f.grc.sgl/grc4f_init/setmas_koralw.f:                        //
*//         ampli4f.grc.all/grc4f_init/setmas_koralw.f:                        //
*//  through  CALL ampinw(xpar,npar) in KoralW                                 //
*//  This cross-talk should be eliminated, for example filtering from xpar to  //
*//  common blocks in setmas_koralw.f should be done independently,            //
*//  or it should be eliminated,ie.  xpar replaced by getters from KW class.   //
*//                                                                            //
*////////////////////////////////////////////////////////////////////////////////
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z)  
      DIMENSION  XPAR( *),NPAR( *)          
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF  
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW
! This common contains parameters of non-established particles such as higgs
! Which need to be used by pre-sampler (to be activated by dipswitch IHIG
      COMMON / WEKIN3 / AMHIG,GAMHIG,IHIG
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! TAUOLA, PHOTOS and JETSET overall switches
      COMMON / LIBRA  / JAK1,JAK2,ITDKRC,IFPHOT,IFHADM,IFHADP

      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI                   
      COMMON / WT_MAX / WTMAX,WTMAX_CC03     
      COMMON / DECDAT / AMAFIN(20), BR(20)
      COMMON / INOUT  / NINP,NOUT     
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
!   -- cuts for selecto
      COMMON /articut/ arbitr,arbitr1,themin,arbitr2  
!   --Anomalous WWV Couplings, for WWamgc only
      COMMON / ancoco / g1(2),kap(2),lam(2),g4(2),g5(2),kapt(2),lamt(2)
      DOUBLE COMPLEX g1,kap,lam,g4,g5,kapt,lamt
! user mask on final states
      COMMON /cumask/ user_umask(202)
!   --Formats for anyone
      COMMON / BXFMTS / BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G 
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G       
      CHARACTER*80      BXL2C      
      COMMON / RANPAR / KEYRND
!-- The CKM mixing matrix and VV+ which should be =1 (V - unitary) 
      DIMENSION VCKM(3,3),VVH(3,3)
!   -- upspeeding
      SAVE / WEKING /,/ KEYkey /,/ VVREC  /
      SAVE / INOUT  /,/ BXFMTS /,/ RANPAR /,/ WEKIN2 /,/WEKIN3/
      SAVE  / ancoco /, / decdat /
      SAVE

!-- Data
      CHARACTER*64 Logo(44)
      DATA Logo /
     $'***************************************************************',
     $'***************************************************************',
     $'***************************************************************',
     $'*  ###   ###                                   ###       ###  *',
     $'*  ###  ###  ####    ######      ##     ##     ###       ###  *',
     $'*  ### ###  ##  ##   ##   ##    ####    ##     ###       ###  *',
     $'*  ######  ##    ##  ##   ##   ##  ##   ##     ###       ###  *',
     $'*  ######  ##    ##  #####    ##    ##  ##     ###   #   ###  *',
     $'*  ### ###  ##  ##   ##  ##   ########  ##      ### ### ###   *',
     $'*  ###  ###  ####    ##   ##  ##    ##  #######  #### ####    *',
     $'*  ###   ###            version 1.42              ##   ##     *',
     $'***************************************************************',
     $'********************** September 1998 *************************',
     $'***************************************************************',
     $'               Last modification: 16.10.1998                   ',
     $'***************************************************************',
     $'*  Written by:                                                *',
     $'*    S. Jadach      (Stanislaw.Jadach@cern.ch)                *',
     $'*    W. Placzek     (Wieslaw.Placzek@cern.ch)                 *',
     $'*    M. Skrzypek    (Maciej.Skrzypek@cern.ch)                 *',
     $'*    B.F.L. Ward    (bflw@slac.stanford.edu)                  *',
     $'*    Z. Was         (Zbigniew.Was@cern.ch)                    *',
     $'*  Papers:                                                    *',
     $'*    M. Skrzypek, S. Jadach, W. Placzek, Z. Was               *',
     $'*      CERN-TH/95-205, Jul 1995, CPC 94 (1996) 216            *',
     $'*    M. Skrzypek, S. Jadach, M. Martinez, W. Placzek, Z. Was  *',
     $'*      CERN-TH/95-246, Sep 1995, Phys. Lett. B372 (1996) 289  *',
     $'*    S. Jadach, W. Placzek, M. Skrzypek, B.F.L. Ward, Z. Was  *',
     $'*   CERN-TH/98-242, UTHEP-98-0702, Jul 1998, submitted to CPC *',
     $'*  Related papers:                                            *',
     $'*    T. Ishikawa, Y. Kurihara, M. Skrzypek, Z. Was            *',
     $'*      CERN-TH/97-11, Jan 1997, Eur. Phys. J. C4 (1998) 75    *',
     $'*    S. Jadach, K. Zalewski                                   *',
     $'*    CERN-TH/97-29, Jan 1997, Acta Phys. Pol. B28 (1997) 1363 *',
     $'*  WWW:                                                       *',
     $'*    http://hpjmiady.ifj.edu.pl/                              *',
     $'*  Acknowledgements:                                          *',
     $'*    We acknowledge warmly very useful help of:               *',
     $'*      M. Martinez in testing versions 1.01 and 1.02,         *',
     $'*      M. Gruenewald and A. Valassi in testing version 1.21   *',
     $'*      S. Jezequel in testing versions 1.31-1.33              *',
     $'*      M. Witek in testing version 1.41                       *',
     $'***************************************************************',
     $' '/ 
* ...BX-formats for nice and flexible outputs                 
      BXOPE =  '(//1X,15(5H*****)    )'     
      BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'   
      BXL1I =  '(1X,1H*,I17,                 16X, A20,A12,A7, 1X,1H*)'
      BXL1F =  '(1X,1H*,F17.8,               16X, A20,A12,A7, 1X,1H*)' 
      BXL2F =  '(1X,1H*,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXL2C ='(1X,1H*,1H(,F14.8,3H +i,F13.7,1H),1X,A20,A12,A7,1X,1H*)'
      BXL1G =  '(1X,1H*,G17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2G =  '(1X,1H*,G17.8, 4H  +-, G11.5, 1X, A20,A12,A7, 1X,1H*)'
      BXCLO =  '(1X,15(5H*****)/   )'       
*///////////////////////////////////////////////////////////
*//   Math. constants, should go to PARAMETER !!!
      PI=3.1415926535897932D0
      CEULER = 0.57721566D0                  
      ZET2= PI**2/6D0  
      ZET3= 1.2020569031595942854D0
*///////////////////////////////////////////////////////////
*//                                                       //
*//  Npar is now pure internal object (obsolete)          //
*//  below we translate xpar--> npar fot internal use     //
*//                                                       //
*///////////////////////////////////////////////////////////
*     KeyRad = 1000*KeyCul +100*KeyNLL +10*KeyFSR +KeyISR
      npar(1)= 
     $           +NINT(xpar(1011))  ! KeyISR
     $        +10*NINT(xpar(1012))  ! KeyFSR
     $       +100*NINT(xpar(1013))  ! KeyNLL
     $      +1000*NINT(xpar(1014))  ! KeyCul
*
*     KeyPhy = 100000*KeyWu +10000*KeyRed +1000*KeySpn+100*KeyZet+10*KeyMas+KeyBra 
      npar(2)= 
     $           +NINT(xpar(1021))  ! KeyBra 
     $        +10*NINT(xpar(1022))  ! KeyMas
     $       +100*NINT(xpar(1023))  ! KeyZet
     $      +1000*NINT(xpar(1024))  ! KeySpn
     $     +10000*NINT(xpar(1025))  ! KeyRed
     $    +100000*NINT(xpar(1026))  ! KeyWu
*     KeyTek = 100*KeySmp +10*KeyRnd +KeyWgt
      npar(3)= 
     $           +NINT(xpar(1031))  ! KeyWgt
     $        +10*NINT(xpar(1032))  ! KeyRnd
     $       +100*NINT(xpar(1033))  ! KeySmp
*     KeyMis = 10000*KeyWon +1000*KeyZon+100*KeyAcc+10*Key4f +KeyMix
      npar(4)= 
     $           +NINT(xpar(1041))  ! KeyMix
     $        +10*NINT(xpar(1042))  ! Key4f
     $       +100*NINT(xpar(1043))  ! KeyAcc
     $      +1000*NINT(xpar(1044))  ! KeyZon
     $     +10000*NINT(xpar(1045))  ! KeyWon
*
      npar( 5)= NINT(xpar(1055))  ! KeyDWM
      npar( 6)= NINT(xpar(1056))  ! KeyDWP
      npar( 7)= NINT(xpar(1057))  ! Nout
*
      npar(21)= NINT(xpar(1071))  ! JAK1
      npar(22)= NINT(xpar(1072))  ! JAK2
      npar(23)= NINT(xpar(1073))  ! ITDKRC
      npar(24)= NINT(xpar(1074))  ! IFPHOT
      npar(25)= NINT(xpar(1075))  ! IFHADM
      npar(26)= NINT(xpar(1076))  ! IFHADP
* anomalki
      DO i=101,302
         npar(i)=NINT(xpar(i+1000))
      ENDDO
*///////////////////////////////////////////////////////////
*//         end of translation xpar-->npar                //
*///////////////////////////////////////////////////////////
*  Some phys. constants
      amel    =xpar(100)
      AlfInv  =xpar(101)
      gpicob  =xpar(102)
!-----------------------------------------------------------------------
! Physics switches 
! KeyRad =  1000*KeyCul+100*KeyNLL+10*KeyFSR+KeyISR
      KeyRad = NPAR(1)
      KeyISR = MOD(KeyRad,10)
      KeyFSR = MOD(KeyRad,100)/10
      KeyNLL = MOD(KeyRad,1000)/100
      KeyCul = MOD(KeyRad,10000)/1000
!
! KeyPhy = 100000*KeyWu +10000*KeyRed +1000*KeySpn 
!                       +100*KeyZet +10*KeyMas +KeyBra
      KeyPhy = NPAR(2)
      KeyBra = MOD(KeyPhy,10)
      KeyMas = MOD(KeyPhy,100)/10
      KeyZet = MOD(KeyPhy,1000)/100
      KeySpn = MOD(KeyPhy,10000)/1000
      KeyRed = MOD(KeyPhy,100000)/10000
      KeyWu  = MOD(KeyPhy,1000000)/100000
!-----------------------------------------------------------------------
! Technical switches
! KeyTek = 100*KeySmp +10*KeyRnd +KeyWgt
      KeyTek = NPAR(3)
      KeyWgt = MOD(KeyTek,10)
      KeyRnd = MOD(KeyTek,100)/10
      KeySmp = MOD(KeyTek,1000)/100 
!-----------------------------------------------------------------------
! Miscelaneous, for future develoment
! KeyMis = 10000*KeyWon +1000*KeyZon+100*KeyAcc +10*Key4f +KeyMix
      KeyMis = NPAR(4)
      KeyMix = MOD(KeyMis,10)
      Key4f  = MOD(KeyMis,100)/10
      KeyAcc = MOD(KeyMis,1000)/100
      KeyZon = MOD(KeyMis,10000)/1000
      KeyWon = MOD(KeyMis,100000)/10000
! Higgs pre-sampler dipswitch
      IHIG=0
!-----------------------------------------------------------------------
      KEYDWM = NPAR(5)     
      KEYDWP = NPAR(6)     
      IF((KeyWon*KeyZon*(KEYDWP+KEYDWM) .NE. 0) .OR.
     $   (KeyWon .EQ. 0 .AND. KeyZon.eq.0       )     ) THEN
          WRITE(6,*) 'FILEXP==> inconsistent input: '
          WRITE(6,*) 'KeyWon=',KeyWon,'KeyZon=',KeyZon
          WRITE(6,*) 'KeyDWP=',KeyDWP,'KeyDWM=',KeyDWM
          STOP
      ENDIF
      nout = npar(7)
      IF(nout. LE. 0) nout=16     
      jak1 = npar(21)
      jak2 = npar(22)
      itdkrc = npar(23)
      ifphot = npar(24)
      ifhadm = npar(25)
      ifhadp = npar(26)
      cmsene = xpar(1)      
      gmu    = xpar(2)   
      alfwin = xpar(3)
      amaz   = xpar(4)
      gammz  = xpar(5)
      amaw   = xpar(6)
      gammw  = xpar(7)
      vvmin  = xpar(8)
      vvmax  = xpar(9)
      wtmax  = xpar(10)
      amhig  = xpar(11)
      gamhig = xpar(12)
      alpha_s= xpar(13)
      arbitr = xpar(14)
      arbitr1= xpar(15)
      themin = xpar(16)
      arbitr2= xpar(17)
      wtmax_cc03= xpar(18)
      PReco  = xpar(19)
      ene    = CMSene/2d0      
* ?????????????  too small for yfs3 !!!!!!!!!!!!!!!!!!!!!
      vvmax  = MIN( vvmax, 1d0-(amel/ene)**2 )                   
c?????????????????????????????????????????????
c re-used in KoralW
      XPAR(9) =VVMAX ! send it back !!!
c?????????????????????????????????????????????

*/////////////////////////////////////////////////////////////////////////
*//               If arbitr2=<0 reset it to the maximum value           //
      IF (arbitr2 .LE. 0d0) THEN
        arbitr2 = cmsene**2
c?????????????????????????????????????????????
c seems to be unused
        xpar(17) = arbitr2
c?????????????????????????????????????????????
      ENDIF
      IDE=2               
      IDF=2               
      XK0=3.D-3         
*/////////////////////////////////////////////////////////////////////////
*//                       users mask                                    //
      DO i=1,202
         user_umask(i)=npar(100+i)
      ENDDO
*/////////////////////////////////////////////////////////////////////////
*//                         alpha_w                                     //
      alphaw = 1d0/ AlfWin
*/////////////////////////////////////////////////////////////////////////
*//         Electroweak renormalisation schemes                         //
*/////////////////////////////////////////////////////////////////////////
      IF(KeyMix .EQ. 2) THEN
* this option is turned into 'bare Born' 
* so, we reset ALFWIN to alfa_QED
         SINW2 = 1D0 -AMAW**2/AMAZ**2
         ALFWIN = alfinv
c??????????????????????????????????????
c re-used in setmas_koralw.f
         xpar(3) = alfwin
c??????????????????????????????????????
         ALPHAW = 1D0/ ALFWIN
      ELSEIF(KeyMix .EQ. 1) THEN
!.. this option is turned into G_mu scheme, 
!.. so, we recalculate ALFWIN
         SINW2 = 1D0 -AMAW**2/AMAZ**2
         ALFWIN = pi/( sqrt(2d0)*gmu*amaw**2*sinw2 )
c??????????????????????????????????????
c re-used in setmas_koralw.f
         xpar(3) = alfwin
c??????????????????????????????????????
         ALPHAW = 1D0/ ALFWIN
      ELSE 
* LEP2 workshop definition
         sinw2 = pi * alphaw /( sqrt(2d0) * amaw**2 * gmu )
      ENDIF
*/////////////////////////////////////////////////////////////////////////
*//            cuts for selecto removed for CC03                        //
*/////////////////////////////////////////////////////////////////////////
      IF (Key4f .EQ. 0) THEN
* no cuts for CC03 presampler
        arbitr = 0d0  !  min. vis p_t**2 
        arbitr1= 0d0  !  add. cut for e+e-ch+ 
        themin = 0d0  !  min. theta [rad] with beam   
        arbitr2= cmsene**2  !  max p_t**2 of photons for e+e-ch+ 
c????????????????????????????????????????
c seems to be unused
        xpar(14)=arbitr    
        xpar(15)=arbitr1   
        xpar(16)=themin    
        xpar(17)=arbitr2  
c????????????????????????????????????????
      ENDIF
*/////////////////////////////////////////////////////////////////////////
*//             alpha_s/pi for naive QCD corrections                    //
*/////////////////////////////////////////////////////////////////////////
      aspi = alpha_s/pi
*/////////////////////////////////////////////////////////////////////////
*//                                                                     //
*//           Branching ratios for W decay channels:                    //
*//                                                                     //
*/////////////////////////////////////////////////////////////////////////
      IF(  KeyBra .EQ. 0 )THEN
*/////////////////////////////////////////////////////////////////////////
*//                    Born values                                      //
*/////////////////////////////////////////////////////////////////////////
         BR(1)=(1D0/3D0)*(1D0+aspi)/(1D0+2D0/3D0*aspi) !  <== ud
         BR(2)=0D0                                     !  <== cd
         BR(3)=0D0                                     !  <== us
         BR(4)=(1D0/3D0)*(1D0+aspi)/(1D0+2D0/3D0*aspi) !  <== cs
         BR(5)=0D0                                     !  <== ub
         BR(6)=0D0                                     !  <== cb
         BR(7)=(1D0/9D0)           /(1D0+2D0/3D0*aspi) !  <== e
         BR(8)=(1D0/9D0)           /(1D0+2D0/3D0*aspi) !  <== mu
         BR(9)=(1D0/9D0)           /(1D0+2D0/3D0*aspi) !  <== tau
      ELSEIF(  KeyBra .EQ. 1 )THEN
*/////////////////////////////////////////////////////////////////////////
*//          Values of CKM and BRanchings for KeyBra = 1                //
*// note that these br ratios correspond to alfa_s=0.12 (gamma_W->el    //
*// constant) and to nontrivial CKM matrix simultaneously               //
*// this is 'bullet proof' default setting                              //
*/////////////////////////////////////////////////////////////////////////
         ALPHA_S = 0.12d0  ! make sure alpha_s is consistent
c??????????????????????????????????????????????
c re-used in setmas_koralw.f and KoralW
         xpar(13)=alpha_s  ! <== send it back
c??????????????????????????????????????????????
         aspi = alpha_s/pi
         gammw=-1d0        ! make sure W width will be recalculated
         DO i=1,9
            BR(i) = xpar(130 +i)
         ENDDO
      ELSEIF(  KeyBra .EQ. 2 )THEN
*/////////////////////////////////////////////////////////////////////////
*//              Default values of CKM and BRanchings                   //
*// Recalculate br. ratios from the CKM matrix and alpha_s according to //
*// theoretical formula of A. Denner, Fortschr. Phys. 41 (1993) 307.    //
*// Values of the CKM matrix elements from 1996 PDG Review:             //
*//  http://www-pdg.lbl.gov/pdg.html (mean values of given ranges)      //
*/////////////////////////////////////////////////////////////////////////
         VCKM(1,1) =xpar(111)   ! V_ud  real part
         VCKM(1,2) =xpar(112)   ! V_us  real part
         VCKM(1,3) =xpar(113)   ! V_ub  real part
         VCKM(2,1) =xpar(114)   ! V_cd  real part
         VCKM(2,2) =xpar(115)   ! V_cs  real part
         VCKM(2,3) =xpar(116)   ! V_cb  real part
         VCKM(3,1) =xpar(117)   ! V_td  real part
         VCKM(3,2) =xpar(118)   ! V_ts  real part
         VCKM(3,3) =xpar(119)   ! V_tb  real part
* Unitarity check of the CKM matrix: VVH should be =1
         DO i = 1,3
         DO j = 1,3
           sum = 0d0
           DO k = 1,3
             sum = sum + VCKM(i,k)*VCKM(j,k)
           ENDDO
           VVH(i,j) = sum
         ENDDO
         ENDDO
* IBA formulae for branching ratios
         brlep = 1d0/9d0/(1 + 2d0/3d0*aspi)
         brqua = 3*brlep*(1 + aspi)
         BR(1) = VCKM(1,1)**2 *brqua  !  <== ud
         BR(2) = VCKM(2,1)**2 *brqua  !  <== cd
         BR(3) = VCKM(1,2)**2 *brqua  !  <== us
         BR(4) = VCKM(2,2)**2 *brqua  !  <== cs
         BR(5) = VCKM(1,3)**2 *brqua  !  <== ub
         BR(6) = VCKM(2,3)**2 *brqua  !  <== cb
         BR(7) = brlep                !  <== e
         BR(8) = brlep                !  <== mu
         BR(9) = brlep                !  <== tau  
* make sure W width will be recalculated       
         gammw =-1d0        
      ELSE
        WRITE(6,*)'filexp=> wrong KeyBra: ',keybra
        STOP
      ENDIF
*///////////////////////////////////////////////////////////
*//  Check if requested final state has a ZERO br. ratio  //
      IF(KeyWon.NE.0 .AND. KeyZon.EQ.0) THEN
        IF(Keydwm.NE.0 .AND. Keydwp.NE.0 .AND. Keydwp.NE.Keydwm) THEN
          IF(br(Keydwm) .EQ.0d0 .OR. br(Keydwp) .EQ. 0d0 ) THEN
           WRITE(6,*)'filexp=> requested CKM-nondiagonal WW final state'
           WRITE(6,*)'has zero xsect if used with br. ratios specified'
           STOP
          ENDIF
        ENDIF
      ENDIF
*/////////////////////////////////////////////////////////////////////////
*//             W width recalculated on request                         //
*/////////////////////////////////////////////////////////////////////////
      IF ( gammw .LE. 0d0 ) THEN
         gwc  =  9d0 * Gmu * amaw**2 /( 6d0 * sqrt(2d0) * pi)
         gammw = amaw * gwc
*-- Naive QCD correction to the width
         gammw=gammw*(1D0+2D0/3D0*ASPI) 
c????????????????????????????????????????????
c re-used in setmas_koralw.f
         XPAR(7) = GAMMW  ! send it back !!!
c????????????????????????????????????????????
      ENDIF
*///////////////////////////////////////////////////////////////////
*//               final fermions masses                           //
*///////////////////////////////////////////////////////////////////
      DO i = 1,6
         amafin(   i) = xpar(500+i*10 +6) ! leptons
         amafin(10+i) = xpar(600+i*10 +6) ! quarks
      ENDDO
      amel   = amafin(11)       ! <---now probably not necessary ?????
      IF(  KeyMas .EQ. 0 ) THEN
         DO i = 1,6
            amafin(   i) = 0d0
            amafin(10+i) = 0d0
         ENDDO
      ENDIF
cccc*///////////////////////////////////////////////////////////////////
cccc*// Not necessary because default wtmax is in data_DEFAUTS anyway //
cccc*///////////////////////////////////////////////////////////////////
!!!!!!   NECESSARY this is a part of algorithm, not data   !!!!!!!  m.s.
      IF(wtmax.LE.0d0) THEN
        wtmax=2d0
      ENDIF
ccccc??????????????????????????????????????????????
ccccc seems to be unused
ccccc        xpar(10) = wtmax   ! send it back !!!
ccccc???????????????????????????????????????????????
      IF(wtmax_cc03 .LE. 0d0) THEN
        wtmax_cc03 = xpar(151)
        IF(cmsene.GT.162) wtmax_cc03 = xpar(152)
        IF(cmsene.GT.175) wtmax_cc03 = xpar(153)
        IF(cmsene.GT.200) wtmax_cc03 = xpar(154)
        IF(cmsene.GT.250) wtmax_cc03 = xpar(155)
        IF(cmsene.GT.350) wtmax_cc03 = xpar(156)
        IF(cmsene.GT.700) wtmax_cc03 = xpar(157)
c?????????????????????????????????????????????
c seems to be unused
        xpar(18) = wtmax_cc03   ! send it back !!!
c?????????????????????????????????????????????
      ENDIF
!-- if WW-CC03 matrix el. is requested, use wtmax_cc03 instead of wtmax
      IF(key4f .EQ. 0) THEN
        wtmax=wtmax_cc03
c?????????????????????????????????????????????
c seems to be unused
        xpar(10) = wtmax   ! send it back !!!
c?????????????????????????????????????????????
      ENDIF
*
      WRITE(6,   '(10X,A)') Logo
      WRITE(NOUT,'(10X,A)') Logo

      WRITE(NOUT,BXOPE)         
      WRITE(NOUT,BXTXT) '           KORALW input parameters used    '
      WRITE(NOUT,BXL1F) CMSENE,     'CMS energy total   ','CMSENE','I.0'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyRad,     'QED super-switch   ','KeyRad','IQ1'
      WRITE(NOUT,BXL1I) KeyISR,     'Init. state Rad.   ','KeyISR','IQ2'
      WRITE(NOUT,BXL1I) KeyFSR,     'Final state Rad.   ','KeyFSR','IQ3'
      WRITE(NOUT,BXL1I) KeyNLL,     'Next. To Leading   ','KeyNLL','IQ4'
      WRITE(NOUT,BXL1I) KeyCul,     'Coulomb corr.      ','KeyCul','IQ5'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyPhy,     'Physics super-switc','KeyPhy','IP1'
      WRITE(NOUT,BXL1I) KeyRed,     'FS mass reduction  ','KeyRed','IP2'
      WRITE(NOUT,BXL1I) KeySpn,     'Spin in W decays   ','KeySpn','IP3'
      WRITE(NOUT,BXL1I) KeyZet,     'Z propag.          ','KeyZet','IP4'
      WRITE(NOUT,BXL1I) KeyMas,     'Mass kinematics.   ','KeyMas','IP5'
      WRITE(NOUT,BXL1I) KeyBra,     'Branching Rat.     ','KeyBra','IP6'
      WRITE(NOUT,BXL1I) KeyWu,      'W propag.          ','KeyWu ','IP7'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyTek,     'Technical super-swi','KeyTek','IT1'
      WRITE(NOUT,BXL1I) KeySmp,     'presampler type    ','KeySmp','IT2'
      WRITE(NOUT,BXL1I) KeyRnd,     'rand Numb type     ','KeyRnd','IT3'
      WRITE(NOUT,BXL1I) KeyWgt,     'weighting  switch  ','KeyWgt','IT4'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyMis,     'Miscelaneous       ','KeyMis','IM1'
      WRITE(NOUT,BXL1I) KeyMix,     'sinW2 input type   ','KeyMix','IM2'
      WRITE(NOUT,BXL1I) Key4f,      '4 fermion matr el  ','Key4f ','IM3'
      WRITE(NOUT,BXL1I) KeyAcc,     'Anomalous couplings','KeyAcc','IM4'
      WRITE(NOUT,BXL1I) KeyWon,     'WW type final state','KeyWon','IM5'
      WRITE(NOUT,BXL1I) KeyZon,     'ZZ type final state','KeyZon','IM6'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KEYDWM,     'W-/Z decay mode    ','KEYDWM','ID1'
      WRITE(NOUT,BXL1I) KEYDWP,     'W+/Z decay mode    ','KEYDWP','ID2'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1F) GMU*1d5,    'G_mu * 1d5         ','GMU   ','I.1'
      WRITE(NOUT,BXL1F) ALFWIN,     'inv alpha_w        ','ALFWIN','I.2'
      WRITE(NOUT,BXL1F) AMAZ,       'Z mass   [GeV]     ','AMAZ  ','I.3'
      WRITE(NOUT,BXL1F) GAMMZ,      'Z width  [GeV]     ','GAMMZ ','I.4'
      WRITE(NOUT,BXL1F) AMAW,       'W mass   [GeV]     ','AMAW  ','I.5'
      WRITE(NOUT,BXL1F) GAMMW,      'W width  [GeV]     ','GAMMW ','I.6'
      WRITE(NOUT,BXL1F) VVMIN,      'dummy infrared cut ','VVMIN ','I.7'
      WRITE(NOUT,BXL1F) VVMAX,      'v_max ( =1 )       ','VVMAX ','I.8'
      WRITE(NOUT,BXL1F) WTMAX,      'max wt for rejectn.','WTMAX ','I.9'
      WRITE(NOUT,BXL1F) WTMAX_CC03, 'max wt for CC03 rej','WTMAX ','I10'
      WRITE(NOUT,BXL1F) alpha_s,    'alpha_s: QCD coupl.','ALPHAS','I11'
      WRITE(NOUT,BXL1F) PReco  ,    'Color Re-Con. Prob.','PReco ','I12'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1F) SINW2,      'sin(theta_W)**2    ','SINW2 ','I13'
      WRITE(NOUT,BXTXT)'***********************************************'
*!-----------------------------------------------------------------------
*! Feynman rules and their printout, LEP200 style
*!-----------------------------------------------------------------------
*c      QE =-1
*c      VEI= 1-4*SINW2
*c      AEI= 1
*c      EEW = SQRT(4D0*PI*ALPHAW)
*c      GAE =-EEW/sqrt(16D0*SinW2*(1d0-SinW2))
*c      GVE = GAE*VEI
*c      GWF = EEW/(2D0*sqrt(2d0)*sqrt(SinW2))
*c      GWWG= EEW
*c      GWWZ= EEW * sqrt(1d0-SinW2) /sqrt(SinW2)
*c      WRITE(NOUT,BXL1F) GVE,        'LEP200 workshop      ','GVE ','***'
*c      WRITE(NOUT,BXL1F) GAE,        'LEP200 workshop      ','GAE ','***'
*c      WRITE(NOUT,BXL1F) GWF,        'LEP200 workshop      ','GWF ','***'
*c      WRITE(NOUT,BXL1F) GWWG,       'LEP200 workshop      ','GWWG','***'
*c      WRITE(NOUT,BXL1F) GWWZ,       'LEP200 workshop      ','GWWZ','***'
*!-----------------------------------------------------------------------
      WRITE(NOUT,BXTXT)'***********************************************'
c>    WRITE(NOUT,BXTXT) '       sin(th_W) from G_mu, alpha_w and M_Z: '
c>    WRITE(NOUT,BXTXT) '        A2 = PI / ( ALFWIN*SQRT(2D0)*GMU )   '
c>    WRITE(NOUT,BXTXT) '     SINW2 = ( 1-SQRT( 1-(4*A2/AMAZ**2) ) )/2'
c>    WRITE(NOUT,BXL1F) SINW2,      'sin(theta_W)**2    ','SINW2 ','A6'
c>    WRITE(NOUT,BXTXT)'***********************************************'
      IF(keyzet.eq.0) THEN 
        WRITE(NOUT,BXTXT) '  Z width in Z propagator: s/M_Z *GAMM_Z '
      ELSEIF(keyzet.eq.1) THEN 
        WRITE(NOUT,BXTXT) '  Z width in Z propagator:   M_Z *GAMM_Z '
      ELSEIF(keyzet.eq.2) THEN 
        WRITE(NOUT,BXTXT) '  Z width in Z propagator:   0           '
      ELSE
        WRITE(NOUT,BXTXT) '  FILEXP ==> wrong KEYZET =',keyzet
        STOP
      ENDIF
      WRITE(NOUT,BXTXT)'***********************************************'
      IF(keyspn.ne.1) THEN 
        WRITE(NOUT,BXTXT) '         WARNING!  spin in decays is OFF: '
        WRITE(NOUT,BXL1I) KEYSPN, 'spin in decays switch','KEYSPN','A13'
      WRITE(NOUT,BXTXT)'***********************************************'
      ENDIF
      IF (KeyBra.EQ.2) THEN
       WRITE(NOUT,BXTXT) '                                    '
       WRITE(NOUT,BXTXT) '                CKM matrix elements:'
       WRITE(NOUT,BXL1F) VCKM(1,1),       'V_ud','VCKM(1,1)','IV1'
       WRITE(NOUT,BXL1F) VCKM(1,2),       'V_us','VCKM(1,2)','IV2'
       WRITE(NOUT,BXL1F) VCKM(1,3),       'V_ub','VCKM(1,3)','IV3'
       WRITE(NOUT,BXL1F) VCKM(2,1),       'V_cd','VCKM(2,1)','IV4'
       WRITE(NOUT,BXL1F) VCKM(2,2),       'V_cs','VCKM(2,2)','IV5'
       WRITE(NOUT,BXL1F) VCKM(2,3),       'V_cb','VCKM(2,3)','IV6'
       WRITE(NOUT,BXL1F) VCKM(3,1),       'V_td','VCKM(3,1)','IV7'
       WRITE(NOUT,BXL1F) VCKM(3,2),       'V_ts','VCKM(3,2)','IV8'
       WRITE(NOUT,BXL1F) VCKM(3,3),       'V_tb','VCKM(3,3)','IV9'
       WRITE(NOUT,BXTXT)
     $          '              Unitarity check of the CKM matrix:'
       WRITE(NOUT,'(1X,1H*,20X,3f10.3,23X,1H*)')(VVH(1,j),j=1,3)
       WRITE(NOUT,'(1X,1H*,15X,5HVV+ =,3f10.3,23X,1H*)')(VVH(2,j),j=1,3)     
       WRITE(NOUT,'(1X,1H*,20X,3f10.3,23X,1H*)')(VVH(3,j),j=1,3)
      ENDIF
      WRITE(NOUT,BXTXT) '                                             '
      WRITE(NOUT,BXTXT) '                Branching ratios of W decays:'
      WRITE(NOUT,BXL1F) BR(1),            'ud','BR(1)','IB1'
      WRITE(NOUT,BXL1F) BR(2),            'cd','BR(2)','IB2'
      WRITE(NOUT,BXL1F) BR(3),            'us','BR(3)','IB3'
      WRITE(NOUT,BXL1F) BR(4),            'cs','BR(4)','IB4'
      WRITE(NOUT,BXL1F) BR(5),            'ub','BR(5)','IB5'
      WRITE(NOUT,BXL1F) BR(6),            'cb','BR(6)','IB6'
      WRITE(NOUT,BXL1F) BR(7),            ' e','BR(7)','IB7'
      WRITE(NOUT,BXL1F) BR(8),           ' mu','BR(8)','IB8'
      WRITE(NOUT,BXL1F) BR(9),          ' tau','BR(9)','IB9'

      WRITE(NOUT,BXTXT) '                              fermion masses:'
      WRITE(NOUT,BXL1F) AMAFIN(1),     ' d','AMAFIN(1)','IM1'
      WRITE(NOUT,BXL1F) AMAFIN(2),     ' u','AMAFIN(2)','IM2'
      WRITE(NOUT,BXL1F) AMAFIN(3),     ' s','AMAFIN(3)','IM3'
      WRITE(NOUT,BXL1F) AMAFIN(4),     ' c','AMAFIN(4)','IM4'
      WRITE(NOUT,BXL1F) AMAFIN(5),     ' b','AMAFIN(5)','IM5'
      WRITE(NOUT,BXL1F) AMAFIN(11),    ' e','AMAFIN(11)','IM6'
      WRITE(NOUT,BXL1F) AMAFIN(12),    've','AMAFIN(12)','IM7'
      WRITE(NOUT,BXL1F) AMAFIN(13),    'mu','AMAFIN(13)','IM8'
      WRITE(NOUT,BXL1F) AMAFIN(14),   'vmu','AMAFIN(14)','IM9'
      WRITE(NOUT,BXL1F) AMAFIN(15),   'tau','AMAFIN(15)','IM10'
      WRITE(NOUT,BXL1F) AMAFIN(16),  'vtau','AMAFIN(16)','IM11'
      WRITE(NOUT,BXTXT) '                                             '
      IF (KeySmp.NE.0) THEN
        WRITE(NOUT,BXTXT) ' Predefined cuts on final state fermions'
        WRITE(NOUT,BXL1F)arbitr, 'min. vis p_t**2    ','GeV^2','X2'
        WRITE(NOUT,BXL1F)arbitr1,'add. cut for e+e-ch+ch-','GeV^2','X3'
        WRITE(NOUT,BXL1G)themin, 'min. theta with beam','rad ','X6'
        WRITE(NOUT,BXL1F)arbitr2,'max. p_t**2 phot eexx','GeV^2','X3'
      ENDIF

      IF( KeyAcc .NE.0 ) THEN 
*!----------------------------------------------------------------------!
*! Setting up the anomalous couplings as given in the paper:            !
*!     K. Hagiwara, R.D. Peccei, D. Zeppenfeld and K. Hikasa,           !
*!                 Nucl. Phys. B282 (1987) 253;                         !
*!     see also: YR CERN-96-01, "Physics at LEP2" Vol. 1, p. 525.       !
*! The variables used in this routine correspond to the following       !
*! contants defined in the above paper:                                 !
*!           constant name     corresponding variable                   ! 
*!                g_1^V                g1(2)                            !
*!                kappa_V              kap(2)                           !
*!                lambda_V             lam(2)                           !
*!                g_4^V                g4(2)                            !
*!                g_5^V                g5(2)                            !
*!                kappa-tilde_V        kapt(2)                          !
*!                lambda-tilde_V       lamt(2)                          ! 
*!----------------------------------------------------------------------!      
         IF( KeyAcc .EQ. 1) THEN 
*!-- Set 1:
*!       --Set up constants OTHER than SM:
*!       --for WWgamma vertex
          g1(1)   = DCMPLX(xpar(21),xpar(31))
          kap(1)  = DCMPLX(xpar(22),xpar(32))
          lam(1)  = DCMPLX(xpar(23),xpar(33))
          g4(1)   = DCMPLX(xpar(24),xpar(34))
          g5(1)   = DCMPLX(xpar(25),xpar(35))
          kapt(1) = DCMPLX(xpar(26),xpar(36))
          lamt(1) = DCMPLX(xpar(27),xpar(37))
*!       --WWZ vertex
          g1(2)   = DCMPLX(xpar(41),xpar(51))
          kap(2)  = DCMPLX(xpar(42),xpar(52))
          lam(2)  = DCMPLX(xpar(43),xpar(53))
          g4(2)   = DCMPLX(xpar(44),xpar(54))
          g5(2)   = DCMPLX(xpar(45),xpar(55))
          kapt(2) = DCMPLX(xpar(46),xpar(56))
          lamt(2) = DCMPLX(xpar(47),xpar(57))
*!======================================================
*!====== Other TGC parametrizations disussed in: ======= 
*!== YR CERN-96-01, "Physics at LEP2" Vol. 1, p. 525. ==
*!======================================================
        ELSEIF (KeyAcc.EQ.2) THEN
*!-- Set 2:  
	  delta_Z = xpar(61)
	  x_gamma = xpar(62)
	  x_Z     = xpar(63)
	  y_gamma = xpar(64)
	  y_Z     = xpar(65)
*!... Calculate general (internal) TGC's (cf. Hagiwara et al.)  
          tW = SQRT(SINW2/(1-SINW2))
*!       --for WWgamma vertex
          g1(1)   = 1
          kap(1)  = 1 + x_gamma 
          lam(1)  = y_gamma
          g4(1)   = 0
          g5(1)   = 0
          kapt(1) = 0
          lamt(1) = 0
*!       --WWZ vertex
          g1(2)   = 1 + tW*delta_Z 
          kap(2)  = 1 + tW*(x_Z + delta_Z)
          lam(2)  = y_Z
          g4(2)   = 0
          g5(2)   = 0
          kapt(2) = 0
          lamt(2) = 0
        ELSEIF (KeyAcc.EQ.3) THEN
*!-- Set 3:  
	  alpha_Wphi = xpar(71)
	  alpha_Bphi = xpar(72)
	  alpha_W    = xpar(73)
*!... Calculate general (internal) TGC's (cf. Hagiwara et al.)  
          sW2 = SINW2
          cW2 = 1 - SINW2
*!       --for WWgamma vertex
          g1(1)   = 1
          kap(1)  = 1 + alpha_Wphi + alpha_Bphi
          lam(1)  = alpha_W
          g4(1)   = 0
          g5(1)   = 0
          kapt(1) = 0
          lamt(1) = 0
*!       --WWZ vertex
          g1(2)   = 1 + alpha_Wphi/cW2 
          kap(2)  = 1 + alpha_Wphi - sW2/cW2*alpha_Bphi
          lam(2)  = alpha_W
          g4(2)   = 0
          g5(2)   = 0
          kapt(2) = 0
          lamt(2) = 0
        ELSE
          write(6,*)'FILEXP==> Wrong KeyAcc: ',keyacc
          STOP
        ENDIF
*!
        WRITE(NOUT,BXTXT)' '
	IF (KeyAcc.EQ.2) THEN
          WRITE(NOUT,BXTXT)'Anomalous Couplings - set 2; YR CERN 96-01'
          WRITE(NOUT,BXTXT)'******************************************'
          WRITE(NOUT,BXL1F) delta_Z,'delta_Z','delta_Z','IA21'
          WRITE(NOUT,BXL1F) x_gamma,'x_gamma','x_gamma','IA22'
          WRITE(NOUT,BXL1F) x_Z    ,'x_Z    ','x_Z    ','IA23'
          WRITE(NOUT,BXL1F) y_gamma,'y_gamma','y_gamma','IA24'
          WRITE(NOUT,BXL1F) y_Z    ,'y_Z    ','y_Z    ','IA25'
	ELSEIF (KeyAcc.EQ.3) THEN
          WRITE(NOUT,BXTXT)'Anomalous Couplings - set 3; YR CERN 96-01'
          WRITE(NOUT,BXTXT)'******************************************'
          WRITE(NOUT,BXL1F) alpha_Wphi,'alpha_Wphi','alpha_Wphi','IA21'
          WRITE(NOUT,BXL1F) alpha_Bphi,'alpha_Bphi','alpha_Bphi','IA22'
          WRITE(NOUT,BXL1F) alpha_W   ,'alpha_W   ','alpha_W   ','IA23'
	ENDIF
        WRITE(NOUT,BXTXT)' '
*!
        WRITE(NOUT,BXTXT)'Internal Anomalous Couplings Activated'
        WRITE(NOUT,BXTXT)'Convention from:'
        WRITE(NOUT,BXTXT)
     $         'K.Hagiwara, R.D.Peccei, D.Zeppenfeld, K.Hikasa,'
        WRITE(NOUT,BXTXT)'                Nucl. Phys. B282 (1987) 253.'
        WRITE(NOUT,BXTXT)'                        for WWZ vertex'  
        WRITE(NOUT,BXL2C) g1(2),             'g_1^Z','g1(2)  ','IC21'
        WRITE(NOUT,BXL2C) kap(2),          'kappa_Z','kap(2) ','IC22'
        WRITE(NOUT,BXL2C) lam(2),         'lambda_Z','lam(2) ','IC23' 
        WRITE(NOUT,BXL2C) g4(2),             'g_4^Z','g4(2)  ','IC24'
        WRITE(NOUT,BXL2C) g5(2),             'g_5^Z','g5(2)  ','IC25'     
        WRITE(NOUT,BXL2C) kapt(2),   'kappa-tilde_Z','kapt(2)','IC26'       
        WRITE(NOUT,BXL2C) lamt(2),  'lambda-tilde_Z','lamt(2)','IC27'          
        WRITE(NOUT,BXTXT)'                    for WWg vertex (gamma)'  
        WRITE(NOUT,BXL2C) g1(1),             'g_1^g','g1(1)  ','IC21'
        WRITE(NOUT,BXL2C) kap(1),          'kappa_g','kap(1) ','IC22'
        WRITE(NOUT,BXL2C) lam(1),         'lambda_g','lam(1) ','IC23' 
        WRITE(NOUT,BXL2C) g4(1),             'g_4^g','g4(1)  ','IC24'
        WRITE(NOUT,BXL2C) g5(1),             'g_5^g','g5(1)  ','IC25'     
        WRITE(NOUT,BXL2C) kapt(1),   'kappa-tilde_g','kapt(1)','IC26'       
        WRITE(NOUT,BXL2C) lamt(1),  'lambda-tilde_g','lamt(1)','IC27'          
        WRITE(NOUT,BXTXT)' '
      ENDIF

      WRITE(NOUT,BXTXT) '                              DECAY LIBRARIES'
      WRITE(NOUT,BXL1I) JAK1,         'TAUOLA for W+' ,'JAK1','IL1'
      WRITE(NOUT,BXL1I) JAK2,         'TAUOLA for W-' ,'JAK2','IL2'
      WRITE(NOUT,BXL1I) ITDKRC,   'TAUOLA Ord(alpha)' ,'ITDKRC','IL3'
      WRITE(NOUT,BXL1I) IFPHOT,              'PHOTOS' ,'IFPHOT','IL4'
      WRITE(NOUT,BXL1I) IFHADM,       'JETSET for W-' ,'IFHADM','IL5'
      WRITE(NOUT,BXL1I) IFHADP,       'JETSET for W+' ,'IFHADP','IL6'
      WRITE(NOUT,BXCLO)         

      END       
      SUBROUTINE karlud(mode,par1,par2,par3)
*     **************************************
* low level  monte-carlo generator
* administrates directly generation of v-variable
* and indirectly of all other variables.
*     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER (nmax= 40)
      COMMON / matpar / pi,ceuler
      COMMON / phypar / alfinv,gpicob
      COMMON / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf
      COMMON / wekin2 / amaw,gammw,gmu,alphaw
! user mask on final states
      COMMON /cumask/ user_umask(202)
* this COMMON can be everywhere, contains various switches
      COMMON / keykey/  keyrad,keyphy,keytek,keymis,keydwm,keydwp
      COMMON / inout  / ninp,nout
      COMMON / bxfmts / bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      CHARACTER*80      bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g

      COMMON / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot
      COMMON / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      COMMON / cms_eff_momdec /
     $      effbeam1(4),effbeam2(4),effp1(4),effp2(4),effp3(4),effp4(4)
      COMMON / wgtgen / wtves,wtyfs,wtborn
      COMMON / wgtall / wtcrud,wtmod,wtset(100)

! this common breaks modularity. Should be removed somehow.
* communicates with vesko/rhosko
      COMMON / vvrec  / vvmin,vvmax,vv,beti

      EXTERNAL rhosko
      DIMENSION drvec(1)

      DIMENSION effq1(4),effq2(4)

      SAVE

*     ==================================================================
*     ===================initialization=================================
*     ==================================================================
      IF(mode .EQ. -1) THEN
*     ==================================================================
      KeyMas = MOD(KeyPhy,100)/10
      IF(  KeyMas .EQ. 0 ) THEN
        ambeam=0d0
      ELSE
        ambeam=amel
      ENDIF

      svar = 4d0*ene**2
* this is "pointer" for internal monitoring histograms/averages
      idyfs = 0


* 4f m-type phase space debugg mode, 
* READs 4vects and ikan from the disk,
* activated with msdump =1
* (requires keyisr=0 and keysmp=2 or 3)
      msdump=0

* m probability in m/z mixing of 4f phase space generators
      prob_ps_m=0.5d0

!-- yfs internal tests, activated with i_yfs=1
      i_yfs=0

*-- initialize decays
! what should emin be ??????? some additional cut-off ?????
      emin=1d0
      emax=2*ene
! i_file=1 requests xsection data from the file 
!          (50-250GeV for now, flat elsewhere)
! i_file=0 requests xsection data from analytic function
      i_file=1
      KeyWgt = MOD(KeyTek,10)
      KeySmp = MOD(KeyTek,1000)/100 
      CALL decay_prob_init(emin,emax,user_umask,i_file,KeyWgt,keysmp)
*-- initialize decays end

! is beti needed by / vvrec / ????
      beti = 2d0/alfinv/pi*(dlog(4d0*ene**2/amel**2)-1d0)

*-- initialization of qed part
      keyisr = MOD(keyrad,10)
*-- calculation of crude normalization
      IF( keyisr .NE. 0)THEN
         CALL vesk1w(-1,rhosko,dum1,dum2,xcvesk)
         CALL gifyfs(svar,amel,fyfs)
         xcrude = xcvesk*fyfs

*-- initialize internal tests of yfs
         IF(i_yfs. EQ. 1) 
     @     CALL yfs_tests(-1,amel,ene,idyfs,wtves,xcvesk,wt1,wt2,wt3)
      ELSE
! note, for no ISR xcrude is completely dummy, it will be divided out
! later on. can be set to 1 as well...
         sprim=svar
         xcrude=get_total_crude(sprim)
         fyfs=1d0
         xcvesk=0d0
      ENDIF
*-- outputs
      par1=xcrude
      par2=xcvesk
      par3=xcrude
      keysmp = MOD(keytek,1000)/100

* ==================================================================
* ====================generation====================================
* ==================================================================
      ELSEIF( mode  .EQ.  0) THEN
* ==================================================================

      CALL cleanup

      wtves=1d0
      wtyfs=1d0
* generate vv
      IF( keyisr .NE. 0 ) THEN
         CALL vesk1w( 0,rhosko,dum1,dum2,wtves)
* low-level multiphoton generator
         CALL yfsgen(vv,vvmin,nmax,wt1,wt2,wt3)

*-- internal tests of yfs (should that be before or after next line???)
         IF(i_yfs. EQ. 1) 
     @      CALL yfs_tests(0,amel,ene,idyfs,wtves,xcvesk,wt1,wt2,wt3)
*-- photons under ir cut treated as 0
         IF( vv  .LT.  vvmin) vv=0d0
      ELSE
         vv=0d0
         wt1=1d0
         wt2=1d0
         wt3=1d0
      ENDIF
      wtyfs=wt1*wt2*wt3
      IF( wtyfs  .EQ.  0d0) THEN
         wtkarl=0d0
         wtborn=0d0
         GOTO 150
      ENDIF

      sprim=svar*(1-vv)

*-- generate decay channel
      CALL varran(drvec,1)
      rndm=drvec(1)
      CALL decay(sprim,rndm,label)
*-- store label for future use from phot_spec_book ! temporary !!
      CALL store_label(0,label)

*======================================================
*=========== 4-body phase space begin =================
*======================================================

*-- make a choice of kinematical branch
*-- generate 4f hyper-point
      IF( keysmp .EQ. 2 ) THEN
*-- m branch
         CALL make_phsp_point
     $        (msdump,label,ambeam,svar,sprim,fak_phsp,
     $          effbeam1,effbeam2,effp1,effp2,effp3,effp4)
      ELSEIF( keysmp .LE. 1 ) THEN
*-- z branch
         CALL make_phsp_point_z
     $      (msdump,ambeam,svar,sprim,fak_phsp,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
      ELSEIF( keysmp .EQ. 3 ) THEN
        WRITE(6,*)'karludw=> KeySmp=3 is a test option. Disabled'
        WRITE(6,*)'If you really want to play with it contact authors'
        STOP
*-- mixed branch
         CALL make_phsp_point_mz
     $      (prob_ps_m,msdump,ambeam,svar,sprim,fak_phsp,i_m,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
      ELSE
         WRITE(6,*)'karludw=>wrong keysmp=',keysmp
         STOP
      ENDIF

*-- event outside physical phase space?
      IF(fak_phsp .EQ. 0d0)  THEN
         wtkarl=0d0
         wtborn=0d0
         GOTO 150
      ENDIF

! set auxilliary effq-i
      DO i=1,4
        effq1(i)=effp1(i)+effp2(i)
        effq2(i)=effp3(i)+effp4(i)
      ENDDO

* transform everything back to cms (lab)
      CALL from_cms_eff(1,svar,sprim,ambeam,sphum,sphot,nphot,
     $      effq1,effq2,effp1,effp2,effp3,effp4,
     $      qeff1,qeff2,q1,q2,p1,p2,p3,p4)

*... place selecto on some phase space regions to zero them
      fak_sel=1d0
      CALL selecto(p1,p2,p3,p4,qeff1,qeff2,fak_sel)
*... place users cuts on some phase space regions to zero them
      user_fak_sel=1d0
      CALL user_selecto(p1,p2,p3,p4,qeff1,qeff2,user_fak_sel)

      fak_sel=fak_sel*user_fak_sel

*... event outside physical phase space?
      IF(fak_sel .EQ. 0d0)  THEN
         fak=0d0
         wtkarl=0d0
         wtborn=0d0
         GOTO 150
      ENDIF

*-- calculate total jacobian (summed over branches)
      IF( keysmp  .EQ.  2 ) THEN
         CALL get_phsp_weight
     $      (label,ambeam,svar,sprim,fak,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
      ELSEIF( keysmp  .LE.  1 ) THEN
         CALL get_phsp_weight_z
     $      (svar,sprim,fak,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
      ELSEIF( keysmp .EQ. 3 ) THEN
         CALL get_phsp_weight_mz
     $      (prob_ps_m,ambeam,svar,sprim,fak,i_m,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
      ENDIF

      IF(fak .EQ. 0d0)  THEN
         wtkarl=0d0
         wtborn=0d0
         GOTO 150
      ENDIF

*...  statistical factors for identical particles
      CALL selstfac(stat_fac)
      fak=fak*stat_fac

*======================================================
*=========== 4-body phase space END ===================
*======================================================

*------------------------------------------------------
*------------------ END generation --------------------
*------------------------------------------------------

* first come weights from vesko, inverse of the dummy
* born used in photon spectrum and yfs.
c ms      xcborn=1d0/tot_born_crud(svar,sprim)
      xcborn=1d0/get_total_crude(sprim)
      wtkarl=wtves*xcborn*wtyfs

*-- weight for phase space  volume
       wtkarl=wtkarl *fak

!============================
      IF(wtkarl .NE. 0d0)THEN
!============================

! decay channel probability normalisation factor 
         wmp= get_decay_prob(sprim,label)
         IF(wmp .LE. 0d0) THEN
           WRITE(6,*)'karludw=>generated channel with 0 probability',wmp
           STOP
         ELSE
           wtkarl=wtkarl/wmp
         ENDIF

!-- we move born and other weights up to koralw, as they need the 4fermion
!-- matr. el also

!==========
      ENDIF
!==========

      IF(wtkarl .LT. 0d0) THEN
         WRITE(*,*) 'ujemna waga itype=',itype
         WRITE(*,*) wtkarl
         WRITE(*,*) 'aa'
         WRITE(*,*) faki
         WRITE(*,*) 'bb'
         WRITE(*,*) p1
         WRITE(*,*) p2
         WRITE(*,*) p3
         WRITE(*,*) p4
      ENDIF

 150  CONTINUE

      par1=xcborn
      par2=wtkarl
      par3=0d0

*     ======================
      ELSEIF(mode .EQ. 1) THEN
*     ======================

*-- crude xsection xcvesk and yfs formfactor fyfs
      IF( keyisr .NE. 0 )THEN
         CALL vesk1w( 1,rhosko,xsve,erelve,xcvesk)
         CALL gifyfs(svar,amel,fyfs)
         xcrude=xcvesk*fyfs

*-- internal tests of yfs
         IF(i_yfs. EQ. 1) 
     @     CALL yfs_tests(1,amel,ene,idyfs,wtves,xcvesk,wt1,wt2,wt3)
      ELSE
c ms         xcrude=tot_born_crud(svar,sprim)
         xcrude=get_total_crude(sprim)
         fyfs=1d0
         xcvesk=0d0
      ENDIF
*-- outputs
      par1=xcrude
      par2=xcvesk
      par3=xcrude

* ============
      ELSE
* ============

*-- internal tests of yfs
      IF( keyisr .NE. 0 ) THEN
        IF(i_yfs.EQ.1) 
     @    CALL yfs_tests(mode,amel,ene,idyfs,wtves,xcvesk,wt1,wt2,wt3)
      ENDIF

*-- born xsection, total!
      IF( keyisr .NE. 0 ) THEN
        CALL vesk1w( 1,rhosko,xsve,erelve,xcvesk)
        CALL gifyfs(svar,amel,fyfs)
        xcrude=xcvesk*fyfs
      ELSE
c ms        xcrude=tot_born_crud(svar,sprim)
        xcrude=get_total_crude(sprim)
        fyfs=1d0
        xcvesk=0d0
      ENDIF
      CALL gmonit(1,idyfs+58,averw,errel,parm3)
      xsborn  = xcrude*averw
      erborn  = xsborn*errel
      par1=0d0
      par2=xsborn
      par3=erborn
* ==========
      ENDIF
* ==========
      END

      SUBROUTINE from_cms_eff(mode,svar,sprim,ambeam,sphum,sphot,
     $      nphot, bq1,bq2,bp1,bp2,bp3,bp4,
     $      qeff1,qeff2,q1,q2,p1,p2,p3,p4)
*     **********************************************************
* this routine transforms 4momenta between cms (lab) and
*      effective cms (p_final)
*
* mode =  1   from cms_eff to cms
* mode = -1   from cms to cms_eff
* bq1,bq2,bp1,bp2,bp3,bp4   arbitrary 4momenta to be transformed
*
* output
*  q1, q2, p1, p2, p3, p4   transformed 4momenta
*  qeff1,qeff2              effective beams in cms (mode=1)
*                                           in cms_eff (mode=-1)
*
* written by: m. skrzypek              date: 7/6/96
*
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION qeff1(4),qeff2(4),sphum(4),sphot(100,4)
      DIMENSION bq1(4),bq2(4),bp1(4),bp2(4),bp3(4),bp4(4)
      DIMENSION q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      DIMENSION qsu(4),ef1(4),ef2(4),photd(4),sphotd(100,4)
      DIMENSION def1(4),def2(4)

* to activate dumps kindmp=1
      kindmp=0

      DO 124, i=1,4
      q1(i)=bq1(i)
      q2(i)=bq2(i)
      p1(i)=bp1(i)
      p2(i)=bp2(i)
      p3(i)=bp3(i)
 124  p4(i)=bp4(i)


      IF(nphot .EQ. 0) THEN
*     ===================

* define effective beams (massive) in cms''

        qeff1(4)= sqrt(svar)/2d0
        qeff1(3)= dsqrt(svar/4d0-ambeam**2)
        qeff1(2)= 0d0
        qeff1(1)= 0d0
        qeff2(4)= sqrt(svar)/2d0
        qeff2(3)=-dsqrt(svar/4d0-ambeam**2)
        qeff2(2)= 0d0
        qeff2(1)= 0d0

      ELSEIF(nphot .GE. 1) THEN
*     =======================

* effective beams (in lab)
        ef1(4)= sqrt(svar)/2d0
        ef1(3)= dsqrt(svar/4d0-ambeam**2)
        ef1(2)= 0d0
        ef1(1)= 0d0
        ef2(4)= sqrt(svar)/2d0
        ef2(3)=-dsqrt(svar/4d0-ambeam**2)
        ef2(2)= 0d0
        ef2(1)= 0d0
        DO 11 ii=1,nphot
        IF(sphot(ii,3) .GE. 0d0) THEN
          DO 12 jj=1,4
 12       ef1(jj)= ef1(jj)-sphot(ii,jj)
        ELSE
          DO 13 jj=1,4
 13       ef2(jj)= ef2(jj)-sphot(ii,jj)
        ENDIF
 11     CONTINUE

      IF(kindmp .EQ. 1)THEN
        WRITE(6,*)'__lab0, ef1,ef2'
        CALL dumpl(6,p1,p2,p3,p4,ef1,ef2,sphot,nphot)
      ENDIF

* qsu is 4momentum of decay products (p1-p4) in cms
        DO 110 k=1,4
  110 qsu(k)=-sphum(k)
        qsu(4)=qsu(4)+sqrt(svar)

* transform ef1,2 to rest frame (cms')
* cms' is rotated cms_eff (z+ not along qeff1) !!!
        CALL boostv(1,qsu,ef1,ef1)
        CALL boostv(1,qsu,ef2,ef2)

      IF(kindmp .EQ. 1)THEN
        WRITE(6,*)'__cms ef1,2, phots lab'
        CALL dumpl(6,p1,p2,p3,p4,ef1,ef2,sphot,nphot)
        DO 70 i=1,nphot
        DO 71 j=1,4
 71     photd(j)=sphot(i,j)
*.. photons to cms'
        CALL boostv( 1,qsu,photd,photd)
*.. photons to cms''
        CALL rotatv(1,ef1,photd,photd)
        DO 72 j=1,4
 72     sphotd(i,j)=photd(j)
 70     CONTINUE
        WRITE(6,*)'cms ef1,2 phots cmsbis'
        CALL dumpl(6,p1,p2,p3,p4,ef1,ef2,sphotd,nphot)
* control
        CALL rotatv(-1,ef1,ef2,def2)
        WRITE(6,*)'__control'
        CALL dumpl(6,p1,p2,p3,p4,ef1,def2,sphot,nphot)
        CALL rotatv( 1,ef1,ef2,def2)
        WRITE(6,*)'__control'
        CALL dumpl(6,p1,p2,p3,p4,ef1,def2,sphot,nphot)
      ENDIF

*++++++++++++++++++++++++
      IF(mode .EQ. 1) THEN
*       from cms_eff to cms
*++++++++++++++++++++++++

* define effective beams (massive) in cms'' (cms_eff, z+ along qeff1)
        qeff1(4)= sqrt(sprim)/2d0
        qeff1(3)= dsqrt(sprim/4d0-ambeam**2)
        qeff1(2)= 0d0
        qeff1(1)= 0d0
        qeff2(4)= sqrt(sprim)/2d0
        qeff2(3)=-dsqrt(sprim/4d0-ambeam**2)
        qeff2(2)= 0d0
        qeff2(1)= 0d0

        IF(kindmp .EQ. 1)THEN
          WRITE(6,*)'___cmsbis, qeff1,qeff2'
          CALL dumpl(6,p1,p2,p3,p4,qeff1,qeff2,sphotd,nphot)
        ENDIF

* rotate from cms'' (z along ef1) to cms' (z along e- beam)
          CALL rotatv(-1,ef1,qeff1,qeff1)
          CALL rotatv(-1,ef1,qeff2,qeff2)
          CALL rotatv(-1,ef1,q1,q1)
          CALL rotatv(-1,ef1,q2,q2)
          CALL rotatv(-1,ef1,p1,p1)
          CALL rotatv(-1,ef1,p2,p2)
          CALL rotatv(-1,ef1,p3,p3)
          CALL rotatv(-1,ef1,p4,p4)

        IF(kindmp .EQ. 1)THEN
          WRITE(6,*)'__cmsprim'
          CALL dumpl(6,p1,p2,p3,p4,qeff1,qeff2,sphot,nphot)
        ENDIF

* transform back to lab
          CALL boostv(-1,qsu,qeff1,qeff1)
          CALL boostv(-1,qsu,qeff2,qeff2)
          CALL boostv(-1,qsu,q1,q1)
          CALL boostv(-1,qsu,q2,q2)
          CALL boostv(-1,qsu,p1,p1)
          CALL boostv(-1,qsu,p2,p2)
          CALL boostv(-1,qsu,p3,p3)
          CALL boostv(-1,qsu,p4,p4)
*.. fine tuning on masses
          qeff1(4)=dsqrt(ambeam**2+qeff1(1)**2+qeff1(2)**2+qeff1(3)**2)
          qeff2(4)=dsqrt(ambeam**2+qeff2(1)**2+qeff2(2)**2+qeff2(3)**2)

        IF(kindmp .EQ. 1)THEN
          WRITE(6,*)'__lab, qeff'
          CALL dumpl(6,p1,p2,p3,p4,qeff1,qeff2,sphot,nphot)
* transform ef1 back to lab
          CALL boostv(-1,qsu,ef1,def1)
          CALL boostv(-1,qsu,ef2,def2)
          WRITE(6,*)'__lab all, ef'
          CALL dumpl(6,p1,p2,p3,p4,def1,def2,sphot,nphot)
        ENDIF
*++++++++++++++++++++++++
        ELSEIF(mode .EQ. -1) THEN
*       to cms_eff from cms
*++++++++++++++++++++++++
          CALL boostv(1,qsu,q1,q1)
          CALL boostv(1,qsu,q2,q2)
          CALL boostv(1,qsu,p1,p1)
          CALL boostv(1,qsu,p2,p2)
          CALL boostv(1,qsu,p3,p3)
          CALL boostv(1,qsu,p4,p4)
          CALL rotatv(1,ef1,q1,q1)
          CALL rotatv(1,ef1,q2,q2)
          CALL rotatv(1,ef1,p1,p1)
          CALL rotatv(1,ef1,p2,p2)
          CALL rotatv(1,ef1,p3,p3)
          CALL rotatv(1,ef1,p4,p4)
*++++++++++++++++++++++++
        ELSE
*++++++++++++++++++++++++
          WRITE(6,*)'from_cms_eff==> wrong mode: ',mode
          STOP
*++++++++++++++++++++++++
        ENDIF
*++++++++++++++++++++++++
      ELSE
*     ====
        WRITE(6,*)'from_cms_eff==> wrong no of phots: ',nphot
        STOP
      ENDIF
*     =====
      END

      SUBROUTINE selecto(p1,p2,p3,p4,p5,p6,wt)
* #################################################
* #        mask on phase space regions            #
* #             NOT TO BE MODIFIED                #
* #      use user_selecto for users cuts          #
* #################################################
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON /articut/ arbitr,arbitr1,themin,arbitr2
      COMMON / decays / IFlav(4), amdec(4) 
* this COMMON can be everywhere, contains various switches
      COMMON / keykey/  keyrad,keyphy,keytek,keymis,keydwm,keydwp
      SAVE /articut/,/ decays /,/ keykey/
      SAVE
      DIMENSION p1(4),p2(4),p3(4),p4(4),p5(4),p6(4)

      IF (wt .EQ. 0d0) RETURN
      angarb = SIN(themin)**2

* remove section of e+e-ch+ch- phase space where grace has numerical
* problem for config. with two nearly massless high p_t photons and electrons
* in final state.
!WP: tests ...
c         IF ((iflav(2) .EQ. -11) .OR. (iflav(3) .EQ. 11))THEN
c      IF (qadra(p1,p2)+qadra(p3,p4) .LE. arbitr1) wt=0d0
c         ENDIF
c         IF ((iflav(2) .EQ. -11) .AND. (iflav(3) .EQ. 11))THEN
c      IF (qadra(p1,p4)+qadra(p2,p3) .LE. arbitr1) wt=0d0
c         ENDIF
c      IF (wt .EQ. 0d0) RETURN

         IF ((iflav(2) .EQ. -11) .OR. (iflav(3) .EQ. 11))THEN
      IF (qadra(p1,p2).LE.arbitr1 .OR. qadra(p3,p4).LE.arbitr1) wt=0d0
         ENDIF
         IF ((iflav(2) .EQ. -11) .AND. (iflav(3) .EQ. 11))THEN
      IF (qadra(p1,p4).LE.arbitr1 .OR. qadra(p2,p3).LE.arbitr1) wt=0d0
         ENDIF
      IF (wt .EQ. 0d0) RETURN

!!!!!!!!!!!! additional cut on angle...temporary fixup

      IF(abs(iflav(1)) .NE. 12  .AND.  abs(iflav(1)) .NE. 14
     $  .AND.  abs(iflav(1)) .NE. 16  .AND.
     $ (p1(1)**2+p1(2)**2)/(p1(1)**2+p1(2)**2+p1(3)**2) .LT. angarb )
     $  wt=0d0

      IF(abs(iflav(2)) .NE. 12  .AND.  abs(iflav(2)) .NE. 14
     $  .AND.  abs(iflav(2)) .NE. 16  .AND.
     $ (p2(1)**2+p2(2)**2)/(p2(1)**2+p2(2)**2+p2(3)**2) .LT. angarb )
     $  wt=0d0

      IF(abs(iflav(3)) .NE. 12  .AND.  abs(iflav(3)) .NE. 14
     $  .AND.  abs(iflav(3)) .NE. 16  .AND.
     $ (p3(1)**2+p3(2)**2)/(p3(1)**2+p3(2)**2+p3(3)**2) .LT. angarb )
     $  wt=0d0

      IF(abs(iflav(4)) .NE. 12  .AND.  abs(iflav(4)) .NE. 14
     $  .AND.  abs(iflav(4)) .NE. 16  .AND.
     $ (p4(1)**2+p4(2)**2)/(p4(1)**2+p4(2)**2+p4(3)**2) .LT. angarb )
     $  wt=0d0

      IF (wt .EQ. 0d0) RETURN

!!!!!!!!!!!!!

      pt2=0
      IF(abs(iflav(1)) .NE. 12  .AND.  abs(iflav(1)) .NE. 14
     $  .AND.  abs(iflav(1)) .NE. 16  .AND.
     $ (p1(1)**2+p1(2)**2)/(p1(1)**2+p1(2)**2+p1(3)**2) .GT. angarb )
     $  pt2=pt2+p1(1)**2+p1(2)**2

      IF(abs(iflav(2)) .NE. 12  .AND.  abs(iflav(2)) .NE. 14
     $  .AND.  abs(iflav(2)) .NE. 16  .AND.
     $ (p2(1)**2+p2(2)**2)/(p2(1)**2+p2(2)**2+p2(3)**2) .GT. angarb )
     $  pt2=pt2+p2(1)**2+p2(2)**2

      IF(abs(iflav(3)) .NE. 12  .AND.  abs(iflav(3)) .NE. 14
     $  .AND.  abs(iflav(3)) .NE. 16  .AND.
     $ (p3(1)**2+p3(2)**2)/(p3(1)**2+p3(2)**2+p3(3)**2) .GT. angarb )
     $  pt2=pt2+p3(1)**2+p3(2)**2

      IF(abs(iflav(4)) .NE. 12  .AND.  abs(iflav(4)) .NE. 14
     $  .AND.  abs(iflav(4)) .NE. 16  .AND.
     $ (p4(1)**2+p4(2)**2)/(p4(1)**2+p4(2)**2+p4(3)**2) .GT. angarb )
     $  pt2=pt2+p4(1)**2+p4(2)**2

*!!!!!      IF(pt2 .LE. arbitr) wt=0d0
* set  .LT.  to have a chance to `see' electron neutrinos.
      IF(pt2 .LT. arbitr) wt=0d0

* this is dirty trick to get rid of events with high p_t photons
* which spoil e+e-xx final states.
* begin ==========================
       pt3=0
      IF(abs(iflav(2)) .EQ. 11 .OR. abs(iflav(3)) .EQ. 11) THEN
        pt3=pt3 +(p1(1)+p2(1)+p3(1)+p4(1))**2
        pt3=pt3 +(p1(2)+p2(2)+p3(2)+p4(2))**2
        IF(pt3 .GT. arbitr2) wt=0d0
      ENDIF
* end   =========================
* the following is the cut as used by v. 1.3x . It is practically
* identical to the above one that uses fermion four-momenta 
* instead of effective beams. also its numeric value was glued to the
* fermionic cut.
* begin ==========================
c       pt3=0
c      IF(abs(iflav(2)) .EQ. 11 .OR. abs(iflav(3)) .EQ. 11) THEN
c       pt3=pt3+p5(1)**2+p5(2)**2
c       pt3=pt3+p6(1)**2+p6(2)**2
c       IF(pt3 .GT. arbitr/2) wt=0d0
c      ENDIF
* end   =========================
* this is the END of routine!!
      RETURN


*rubbish kept `in any case'
*
*      IF ((abs(iflav(1)) .EQ. 11) .AND. (abs(iflav(4)) .EQ. 11)) THEN
*      IF (  (    ((p4(3)/p4(4))**2 .GT. 0.96d0)
*     $        .AND. ((p1(3)/p1(4))**2 .GT. 0.96d0) ) .OR.
*     $ ((p4(1)**2+p4(2)**2+p1(1)**2+p1(2)**2) .LE. 49d0)) wt=0d0
*      ENDIF
*!!!      IF (qadra(p1,p2) .LE. 1d0) wt=0d0
*      IF (qadra(p1,p3) .LE. arbitr) wt=0d0
*      IF (qadra(p1,p4) .LE. arbitr) wt=0d0
*      IF (qadra(p1,p5) .LE. arbitr) wt=0d0
*      IF (qadra(p1,p6) .LE. arbitr) wt=0d0
*      IF (qadra(p2,p3) .LE. arbitr) wt=0d0
*      IF (qadra(p2,p4) .LE. arbitr) wt=0d0
*      IF (qadra(p2,p5) .LE. arbitr) wt=0d0
*      IF (qadra(p2,p6) .LE. arbitr) wt=0d0
*      IF (qadra(p3,p4) .LE. arbitr) wt=0d0
*      IF (qadra(p3,p5) .LE. arbitr) wt=0d0
*      IF (qadra(p3,p6) .LE. arbitr) wt=0d0
*      IF (qadra(p4,p5) .LE. arbitr) wt=0d0
*      IF (qadra(p4,p6) .LE. arbitr) wt=0d0
*      IF (qadra(p5,p6) .LE. arbitr) wt=0d0
*       IF (wt .EQ. 0d0) RETURN
*      IF ((p1(1)**2+p1(2)**2) .LE. arbitr) wt=0d0
*      IF ((p2(1)**2+p2(2)**2) .LE. arbitr) wt=0d0
*      IF ((p3(1)**2+p3(2)**2) .LE. arbitr) wt=0d0
*      IF ((p4(1)**2+p4(2)**2) .LE. arbitr) wt=0d0

*       IF (wt .EQ. 0d0) RETURN
*      IF ((p1(1)**2+p1(2)**2)/p1(4)**2 .LE. angarb) wt=0d0
*      IF ((p2(1)**2+p2(2)**2)/p2(4)**2 .LE. angarb) wt=0d0
*      IF ((p3(1)**2+p3(2)**2)/p3(4)**2 .LE. angarb) wt=0d0
*      IF ((p4(1)**2+p4(2)**2)/p4(4)**2 .LE. angarb) wt=0d0
*       WRITE(*,*) p1
*       WRITE(*,*) p2
*       WRITE(*,*) p3
*       WRITE(*,*) p4
*       WRITE(*,*) '---------'
*       WRITE(*,*) p5
*       WRITE(*,*) p6
      END

      SUBROUTINE selstfac(wt)
* ######################################
* introduces statistical factor for identical particles
      IMPLICIT DOUBLE PRECISION (a-h,o-z)

      COMMON / decays / IFlav(4), amdec(4) 

      IF(iflav(1) .EQ. iflav(3)) THEN
        wt=1/4d0
      ELSE
        wt=1d0
      ENDIF

      END

      FUNCTION qadra(p1,p2)
*     ***************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION p1(4),p2(4),pp(4)
      DO k=1,4
        pp(k)=p1(k)+p2(k)
      ENDDO
      qadra=dmas2(pp)
      END

      SUBROUTINE cleanup
!     ******************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT 
      COMMON / MOMDEC / Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)

      DO i=1,4
        DO j=1,nphot
          SPHOT(j,i)=0d0
        ENDDO
        sphum(i)=0d0
        qeff1(i)=0d0
        qeff2(i)=0d0
        q1(i)=0d0
        q2(i)=0d0
        p1(i)=0d0
        p2(i)=0d0
        p3(i)=0d0
        p4(i)=0d0
      ENDDO
      nphot=0

      END

      SUBROUTINE yfs_tests(mode,amel,ene,idyfs,wtves,xcvesk,wt1,wt2,wt3)
!     ******************************************************************
! yfs related tests
! WARNING these tests use random numbers generator. 
! That means they will alter the series.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / matpar / pi,ceuler
      COMMON / phypar / alfinv,gpicob
 
      COMMON / bxfmts / bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      CHARACTER*80      bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g

* communicates with vesko/rhosko
      COMMON / vvrec  / vvmin,vvmax,vv,beti

      EXTERNAL rhosko

      COMMON / inout  / ninp,nout

      DIMENSION drvec(100)

      SAVE

* ============
      IF(mode .EQ. -1) THEN
* ============

        svar=4*ene**2

        beti = 2d0/alfinv/pi*(dlog(4d0*ene**2/amel**2)-1d0)
        beti2= 2d0/alfinv/pi* dlog(4d0*ene**2/amel**2)
        gamfap =1d0-pi**2*beti**2/12d0
        gamfac =exp(-ceuler*beti)/dpgamm(1d0+beti)
        gamfa2 =exp(-ceuler*beti2)/dpgamm(1d0+beti2)

        prec = 1d-7
        xcgaus =bremkf(1,prec)
        xdel = xcvesk/xcgaus-1
        WRITE(nout,bxtxt) '  vesko initialisation report'
        WRITE(nout,bxtxt) '          mode -1            '
        WRITE(nout,bxl1f) xcvesk,'approx xs_crude  vesko','xcvesk','v1'
        WRITE(nout,bxl1f) xcgaus,'exact  xs_crude  gauss','xcgaus','v2'
        WRITE(nout,bxl1f) xdel   ,'xcvesk/xcgaus-1      ','      ','v3'
        WRITE(nout,bxtxt) 'initialize karlud  END '
        WRITE(nout,bxclo)

*-- initialize control histos for yfsgen
        DO k=51,57
          CALL gmonit(-1,idyfs+k,0d0,1d0,1d0)
        ENDDO

* ============
      ELSEIF(mode .EQ. 0) THEN
* ============

         ref  = vvrho(50,svar,amel,vv,vvmin)
         wtr  = ref/vvrho(1,svar,amel,vv,vvmin)
         CALL varran(drvec,1)
         CALL gmonit(0,idyfs+56,wtr,1d0,drvec(1))
* pseudorejection in order to introduce reference xsection
         IF(drvec(1) .GT. wtr) GOTO 110
         wf1  = wt1*vvrho(51,svar,amel,vv,vvmin)/ref
         wf2  = wt2*vvrho(52,svar,amel,vv,vvmin)/ref
         wf3  = wt3
         wf13 = wf1*wf3
         wf123= wf1*wf2*wf3
         CALL gmonit(0,idyfs+51,wf1,  1d0,1d0)
         CALL gmonit(0,idyfs+52,wf2,  1d0,1d0)
         CALL gmonit(0,idyfs+53,wf3,  1d0,1d0)
         CALL gmonit(0,idyfs+54,wf13, 1d0,1d0)
         CALL gmonit(0,idyfs+55,wf123,1d0,1d0)
 110     CONTINUE
         CALL gmonit(0,idyfs+57,wtves,  1d0,1d0)

* ============
      ELSEIF(mode .EQ. 1) THEN
* ============

         CALL vesk1w( 1,rhosko,xsve,erelve,xcves)
         CALL gmonit(1,idyfs+57,wtnoss,ernoss,dumm3)
         prec   = 1d-7
         xsgs   = bremkf(1,prec)
         ergs   = xsgs*prec
         erve   = xsve*erelve
         ddv    = xsve/xsgs-1d0
         ddr    = erelve + 1d-6
         xdel   = xcves/xsgs-1
         WRITE(nout,bxope)
         WRITE(nout,bxtxt) '            window v           '
         WRITE(nout,bxtxt) '       vesko final report      '
         WRITE(nout,bxtxt) '             mode  1           '
        WRITE(nout,bxl1f)xcves ,   'approx xs_crude vesko','xcvesk','v4'
        WRITE(nout,bxl2f)xsve,erve,'exact  xs_crude vesko','xsve  ','v5'
        WRITE(nout,bxl2f)xsgs,ergs,'exact  xs_crude gauss','xsgs  ','v6'
        WRITE(nout,bxl1f) xdel    ,'xcvesk_appr/xsgs-1   ','      ','v7'
        WRITE(nout,bxl2f)ddv,ddr,  ' xsve_exact/xsgs-1   ','      ','v8'
         WRITE(nout,bxclo)
* ============
      ELSE
* ============

      CALL gmonit(1,idyfs+51,del1,dwt1,parm3)
      del1   = del1-1d0
      CALL gmonit(1,idyfs+52,awf2,dwt2,parm3)
      CALL gmonit(1,idyfs+53,awf3,dwt3,parm3)
      del3   = awf3-gamfa2
      CALL gmonit(1,idyfs+54,awf4,dwt4,parm3)
      del4   = awf4-gamfac
      WRITE(nout,bxope)
      WRITE(nout,bxtxt) '     karlud  final  report     '
      WRITE(nout,bxtxt) '         window b              '
      WRITE(nout,bxl2f) del1,dwt1,  '<wf1>-1  mass wt   ','del1  ','b1'
      WRITE(nout,bxl2f) awf2,dwt2,  '<wf2> dilat. weight','awf2  ','b2'
      WRITE(nout,bxl2f) awf3,dwt3,  '<wf3> dilat. weight','awf3  ','b3'
      WRITE(nout,bxl2f) del3,dwt3,  '<wf3>-ygf(beti2)   ','del3  ','b4'
      WRITE(nout,bxl2f) awf4,dwt4,  '<wf1*wf3>          ','awf4  ','b5'
      WRITE(nout,bxl2f) del4,dwt4,  '<wf1*wf3>-ygf(beti)','del4  ','b6'
      WRITE(nout,bxclo)
*     ==================================================================
      CALL gmonit(1,idyfs+59,wtkarl,erkarl,parm3)
      CALL vesk1w( 1,rhosko,xsve,erelve,xcves)
      xskr=xcves*wtkarl
      CALL gmonit(1,idyfs+55,awf5,dwt5,parm3)
      del5   = awf5-gamfac
      CALL gmonit(1,idyfs+56,awf6,parm2,parm3)
      prec = 1d-6
      xrefer = bremkf(50,prec)
      delkar = xrefer*awf5/xskr  -1d0
      delref = xcves*awf6/xrefer-1d0
      WRITE(nout,bxope)
      WRITE(nout,bxtxt) '     karlud  final  report cont.   '
      WRITE(nout,bxtxt) '         window c                  '
      WRITE(nout,bxtxt) 'beti= 2*alfa/pi*(log(s/mel**2)-1)       '
      WRITE(nout,bxtxt) 'gamfap= 1-pi**2*beti**2/12              '
      WRITE(nout,bxtxt) 'gamfac=exp(-ceuler*beti)/gamma(1+beti)  '
      WRITE(nout,bxtxt) 'gamfa2=exp(-ceuler*beti2)/gamma(1+beti2)'
      WRITE(nout,bxl1f)  beti,        '                =','beti  ','c1'
      WRITE(nout,bxl1f)  gamfap,      '                =','gamfap','c2'
      WRITE(nout,bxl1f)  gamfac,      '                =','gamfac','c3'
      WRITE(nout,bxl1f)  gamfa2,      '                =','gamfa2','c4'
      WRITE(nout,bxl2f) awf5,dwt5, ' <wf1*wf3*wf4>      ','awf5  ','c5'
      WRITE(nout,bxl2f) del5,dwt5, ' <wf1*wf3>-ygf(beti)','del5  ','c6'
      WRITE(nout,bxtxt) 'delkar=xrefer*aver(wf1*wf1*wf3)/xskarl-1'
      WRITE(nout,bxtxt) 'delref=xcrude*aver(wtr)/xrefer-1        '
      WRITE(nout,bxl1f) xrefer,    'reference x_sect.   ','xrefer','c7'
      WRITE(nout,bxl1f) delkar,    'xrefer*awf5/xskr  -1','delkar','c8'
      WRITE(nout,bxl1f) delref,    'xcvesk*awf6/xrefer-1','delref','c9'
      WRITE(nout,bxclo)

* ============
      ENDIF
* ============

      END

      subroutine boostv(idir,vv,pp,q)
*     *******************************
c Boost along arbitrary vector v (see eg. J.D. Jacson, "Classical 
c Electrodynamics).
c Four-vector pp is boosted from an actual frame to the rest frame 
c of the four-vector v (for idir=1) or back (for idir=-1). 
c q is a resulting four-vector.
c Note: v must be time-like, pp may be arbitrary.
c
c Written by: Wieslaw Placzek            date: 22.07.1994
c Last update: 3/29/95                     by: M.S.
c 
      implicit DOUBLE PRECISION (a-h,o-z)
      parameter (nout=6)
      DOUBLE PRECISION v(4),p(4),q(4),pp(4),vv(4)  
      save
!
      do 1 i=1,4
      v(i)=vv(i)
 1    p(i)=pp(i)
      amv=(v(4)**2-v(1)**2-v(2)**2-v(3)**2)
      if (amv.le.0d0) then
        write(6,*) 'bosstv: warning amv**2=',amv
      endif
      amv=sqrt(abs(amv))
      if (idir.eq.-1) then
        q(4)=( p(1)*v(1)+p(2)*v(2)+p(3)*v(3)+p(4)*v(4))/amv
        wsp =(q(4)+p(4))/(v(4)+amv)
      elseif (idir.eq.1) then
        q(4)=(-p(1)*v(1)-p(2)*v(2)-p(3)*v(3)+p(4)*v(4))/amv
        wsp =-(q(4)+p(4))/(v(4)+amv)
      else
        write(nout,*)' >>> boostv: wrong value of idir = ',idir
      endif
      q(1)=p(1)+wsp*v(1)
      q(2)=p(2)+wsp*v(2)
      q(3)=p(3)+wsp*v(3)
      end
             
 
      subroutine rotatv(mode,qq,pp,r)        
c     *******************************        
c rotation along arbitrary axis.
c pp rotated into r  from actual frame to frame with z-axis along qq  
c NOT TRUE 7/8/96 ms: (mode = 1) or back (mode = -1).      
c     TRUE 7/8/96 ms: (mode = -1) or back (mode = 1).      
c Written by: M. Skrzypek           date: 04.1995
c Last update: 7/9/96               by: M.S.   
      implicit double precision (a-h,o-z) 
      dimension qq(4),pp(4),r(4),tt(4)   
      parameter (pi = 3.1415926535897932D0)
      save
      the= asin(qq(1)/sqrt(qq(1)**2+qq(2)**2+qq(3)**2))
      phi= asin(qq(2)/sqrt(qq(2)**2+qq(3)**2))   
! ms 7/8/96, following line was missing (plus definition of PI)
      if(qq(3).lt.0d0) phi=pi-phi
      if(mode.eq.-1)then
        call rxtod1(phi,pp,tt)
        call rxtod2(-the,tt,r)
      elseif(mode.eq. 1)then
        call rxtod2(the,pp,tt)
        call rxtod1(-phi,tt,r)
      else
        write(6,*)'rotatv==> wrong mode:',mode
      endif
      end
 
      FUNCTION ANGLE(P,Q)
*     ******************
*  ANGLE BETWEEN TWO 3-COMPONENTS OF FOUR-VECTORS
*     ******************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION P(4),Q(4)
      PQ=P(3)*Q(3)+P(2)*Q(2)+P(1)*Q(1)
      PP=P(3)*P(3)+P(2)*P(2)+P(1)*P(1)
      QQ=Q(3)*Q(3)+Q(2)*Q(2)+Q(1)*Q(1)
      ARG=PQ/SQRT(PP*QQ)
      IF(ARG.GT. 1D0) ARG= 1D0
      IF(ARG.LT.-1D0) ARG=-1D0
      ANGLE=ACOS(ARG)
      END

      function dmas2(p)
c     *******************
      implicit DOUBLE PRECISION (a-h,o-z)
      DOUBLE PRECISION p(4)
      d3m = dsqrt(p(3)**2 +p(2)**2 +p(1)**2)  
      dmas2= (p(4)-d3m)*(p(4)+d3m)
      end

      function dot(p,q)
c     *******************
      implicit DOUBLE PRECISION (a-h,o-z)
      DOUBLE PRECISION p(4),q(4)  
      dot= p(4)*q(4) -p(3)*q(3) -p(2)*q(2) -p(1)*q(1)
      end

      FUNCTION dd2(x,y)
!     *********************************
      implicit double precision (a-h,o-z) 
      dimension x(4),y(4)   
      dd2 = (x(4)+y(4))**2 -(x(3)+y(3))**2 -(x(2)+y(2))**2
     %     -(x(1)+y(1))**2 
      end

! this is the original kineww.f, parts common for both kinematics
! other auxilliary common routines moved to  kinelib.f

!============================================
! used by M for original WW presampler only
! used by Z also 
!============================================
! invkin is used by Born !!! Not any more,11/20/97 ms. 
!! YES again, 3/4/98 ms.

      subroutine invkin(ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $                  amwmn,amwpn,  bp1,bp2,bp3,bp4)
*     **********************************************************
c This routine calculates inverse kinematics for W-W+ pair production
c and decay 
c OUTPUT:
c         ctn,fin - W-  production angles 
c         ct1n,fi1n - W- decay products angles
c         ct2n,fi2n - W+ decay products angles
c         amwm, amwp - masses of W- and W+ resonances
c INPUT:
c         bp1(4), bp2(4) - four-momenta of W- decay products
c         bp3(4), bp4(4) - four-momenta of W+ decay products
c
c Written by: Wieslaw Placzek            date: 22.07.1994
c Rewritten by: M. Skrzypek              date: 3/15/95
c Last update: 9/5/96                    by: Z.W.
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      save   / matpar /
      save
      DOUBLE PRECISION bq1(4),bq2(4),aa(4)
      dimension bp1(4),bp2(4),bp3(4),bp4(4)

      do i=1,4
           aa(i)= bp1(i)+bp2(i)
          bq1(i)= bp1(i)+bp2(i)
      enddo
        amwmn=sqrt(dmas2(aa))
      do i=1,4
           aa(i)= bp3(i)+bp4(i)
          bq2(i)= bp3(i)+bp4(i)
      enddo
        amwpn=sqrt(dmas2(aa))
cc ms      qq=dsqrt( (bq1(4)-amwmn)*(bq1(4)+amwmn) )      
      qq=dsqrt(bq1(1)**2+bq1(2)**2+bq1(3)**2)
      qqt=dsqrt(bq1(1)**2+bq1(2)**2)
cc ms
      ctn=bq1(3)/qq
      stn=sqrt(1d0-ctn**2)
cc ms      cfi=bq1(1)/stn/qq
cc ms      sfi=bq1(2)/stn/qq
      IF (QQT.GT.0D0) THEN                                             !cav
        cfi=bq1(1)/qqt
        sfi=bq1(2)/qqt
      ELSE                                                             !cav
        CFI=1D0                                                        !cav
        SFI=0D0                                                        !cav
!        WRITE (6,*)                                                    !cav
!     &    '+++ INVKIN +++ Warning: ThetaW=0. Phi undefined, set=0.'    !cav
      ENDIF                                                            !cav
cc ms
      fin=acos(cfi)
      if(sfi.le.0d0) fin=2*pi-fin
!
      call boostv(1,bq1,bp1,aa)
      qq=sqrt(aa(1)**2+aa(2)**2+aa(3)**2)
      ct1n=aa(3)/qq
cc ms      stn=sqrt(1d0-ct1n**2)
cc ms      cfi=aa(1)/stn/qq
cc ms      sfi=aa(2)/stn/qq
      qqt=sqrt(aa(1)**2+aa(2)**2)
      IF (QQT.GT.0D0) THEN                                             !cav
        cfi=aa(1)/qqt
        sfi=aa(2)/qqt
      ELSE                                                             !cav
        CFI=1D0                                                        !cav
        SFI=0D0                                                        !cav
!        WRITE (6,*)                                                    !cav
!     &    '+++ INVKIN +++ Warning: ThetaW1=0. Phi undefined, set=0.'   !cav
      ENDIF                                                            !cav
cc ms
      fi1n=acos(cfi)
      if(sfi.le.0d0) fi1n=2*pi-fi1n
!
      call boostv(1,bq2,bp3,aa)
      qq=sqrt(aa(1)**2+aa(2)**2+aa(3)**2)
      ct2n=aa(3)/qq
cc ms      stn=sqrt(1d0-ct2n**2)
cc ms      cfi=aa(1)/stn/qq
cc ms      sfi=aa(2)/stn/qq
      qqt=sqrt(aa(1)**2+aa(2)**2)
      IF (QQT.GT.0D0) THEN                                             !cav
        cfi=aa(1)/qqt
        sfi=aa(2)/qqt
      ELSE                                                             !cav
        CFI=1D0                                                        !cav
        SFI=0D0                                                        !cav
!        WRITE (6,*)                                                    !cav
!     &    '+++ INVKIN +++ Warning: ThetaW2=0. Phi undefined, set=0.'   !cav
      ENDIF                                                            !cav

cc ms
      fi2n=acos(cfi)
      if(sfi.le.0d0) fi2n=2*pi-fi2n
! 
      end

      subroutine kineww(sprim,ct,fi,ct1,fi1,ct2,fi2,
     $            amwm,amwp,amdec,  q1,q2,p1,p2,p3,p4)
*     **********************************************************
c This routine calculates kinematics for W-W+ pair production
c and decay in e+e- collision in the CMS with z-axis pointing 
c in the e- direction.
c fixes also the 'effective beams', qeff1,qeff2
c INPUT:  s    - beams energy squared (in GeV**2) !THIS IS DUMMY
                                                  !!!!!!!!!!! ms
c         sprim - actual center mass energy squared (in GeV**2)
c         cthe,fi - W-  production angles 
c         cdec1,fi1 - W- decay products angles
c         cdec2,fi2 - W+ decay products angles
c         amwm, amwp - masses of W- and W+ resonances
c         amdec(4) - decay products masses
c OUTPUT:
c         qeff1(4)      -effective (massless) e- beam in /MOMSET/
c         qeff2(4)      -effective (massless) e+ beam in /MOMSET/
c         q1(4)        - four-momentum of W-  
c         q2(4)        - four-momentum of W+
c         p1(4), p2(4) - four-momenta of W- decay products
c         p3(4), p4(4) - four-momenta of W+ decay products
c
c Written by: Wieslaw Placzek            date: 22.07.1994
c Rewritten by: M. Skrzypek              date: 3/15/95
c Last update: 4/1/95                by: M.S.
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
! sphum,sphot and nphot are almost-dummy (used for printout only)!!!! ms
      common / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot  
      save   / matpar /,/ momset /
      save
      DOUBLE PRECISION ef1(4),ef2(4)
      dimension amdec(4),  q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
!
c to activate dumps KINDMP=1
      KINDMP=0

      do i=1,4
        q1(i)=0d0      
        q2(i)=0d0      
        p1(i)=0d0      
        p2(i)=0d0      
        p3(i)=0d0      
        p4(i)=0d0      
        ef1(i)=0d0      
        ef2(i)=0d0      
      enddo
      ecm=sqrt(sprim)
      amwm2=amwm**2
      amwp2=amwp**2
      s1=amwm2
      s2=amwp2
      amp1s=amdec(1)**2
      amp2s=amdec(2)**2
      amp3s=amdec(3)**2
      amp4s=amdec(4)**2
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*) AMDEC(1),AMDEC(2),AMDEC(3),AMDEC(4)
        WRITE(6,*) AMP1S,AMP2S,AMP3S,AMP4S
        WRITE(6,*)'S,SPRIM,S1,S2',S,SPRIM,S1,S2
        WRITE(6,*)'DECAY COS1,2',CT1,CT2
      ENDIF
      st =sqrt(max(0D0,((1d0-ct )*(1d0+ct ))))
c..
      st1=sqrt(max(0D0,((1d0-ct1)*(1d0+ct1))))
      st2=sqrt(max(0D0,((1d0-ct2)*(1d0+ct2))))
c..
!... Momentum q1 of the first resonance
      q1(4)=(sprim+amwm2-amwp2)/(2d0*ecm)
!      qq=dsqrt( (q1(4)-amwm)*(q1(4)+amwm) )
      qq=dsqrt( (sprim-amwm2-amwp2)**2 -4*amwm2*amwp2 )/(2*ecm)
      q1(1)=qq*st*cos(fi)
      q1(2)=qq*st*sin(fi)
      q1(3)=qq*ct
      q1(4)=dsqrt(amwm2+q1(1)**2+q1(2)**2+q1(3)**2)
!... Momentum p1 in the rest frame of the first resonance
      ppene=(s1+amp1s-amp2s)/(2d0*amwm)
!      ppe=dsqrt( (ppene-amdec(1))*(ppene+amdec(1)) )
      ppe=dsqrt( (s1-amp1s-amp2s)**2 -4*amp1s*amp2s )/(2d0*amwm)
      p1(1)=ppe*st1*cos(fi1)
      p1(2)=ppe*st1*sin(fi1)
      p1(3)=ppe*ct1
c      p1(4)=ppene
      p1(4)=dsqrt(amdec(1)**2+p1(1)**2+p1(2)**2+p1(3)**2)
c...
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1 '
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
!... Boost to CMS-WW frame
      call boostv(-1,q1,p1,p1)
!... Momentum p2 of the second product of first resonance decay
      do 10 k=1,4
 10   p2(k)=q1(k)-p1(k)
c.. fine tuning on masses
      p1(4)=dsqrt(amdec(1)**2+p1(1)**2+p1(2)**2+p1(3)**2)
      p2(4)=dsqrt(amdec(2)**2+p2(1)**2+p2(2)**2+p2(3)**2)
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1,P2 '
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
!... Momentum q2 of the second resonance
!ms      q2(4)=ecm-q1(4)
      do 20 k=1,3
 20   q2(k)=-q1(k)
      q2(4)=dsqrt(amwp2+q2(1)**2+q2(2)**2+q2(3)**2)
!... Momentum p3 in the rest frame of the second resonance
      ppene=(s2+amp3s-amp4s)/(2d0*amwp)
!      ppe=dsqrt( (ppene-amdec(3))*(ppene+amdec(3)) )
      ppe=dsqrt( (s2-amp3s-amp4s)**2 -4*amp3s*amp4s )/(2d0*amwp)
      p3(1)=ppe*st2*cos(fi2)
      p3(2)=ppe*st2*sin(fi2)
      p3(3)=ppe*ct2
c      p3(4)=ppene
      p3(4)=dsqrt(amdec(3)**2+p3(1)**2+p3(2)**2+p3(3)**2)
c...
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1,P2,Q2,P3 '
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
!... Boost to CMS-WW frame
      call boostv(-1,q2,p3,p3)
!... Momentum p2 of the second product of first resonance decay
      do 30 k=1,4
 30   p4(k)=q2(k)-p3(k)
c.. fine tuning on masses
      p4(4)=dsqrt(amdec(4)**2+p4(1)**2+p4(2)**2+p4(3)**2)
      p3(4)=dsqrt(amdec(3)**2+p3(1)**2+p3(2)**2+p3(3)**2)
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1,P2,Q2,P3,P4, WW frame '
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'LAB NO PHOTS'
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF

      end

      SUBROUTINE res2gn(mode,svar,sprim,rmas,rgam,amdec,s1,s2,wt)
! #############################################
! LIBRARY of angular etc sub-generators start #
! #############################################

!     ***************************************************************
! Generation of ds_1ds_2 distribution within phase space boundaries
! using weighted (pre-sampled) events
!---------------------
! note:
! so far generation is within theta_crude and fine tuning is added at the
! end. For non-acceptable events weight is set to zero.
!---------------------
! breit-wigners pre-samplers in both s_1 and s_2 channels are set.
! total volume 'prnorm' ( S(s') defined in formula 31 of koralw 1.02 manual) 
! is calculated including additional W(s_1)*W(s_2) factor 
! (see koralw 1.02 manual). To obtain proper ds_1ds_2 distribution
! weight wt=prnorm/W(s_1)/W(s_2) must be included, and this will help later
! cancelling singularities of matrix element
! 
! note: both resonances have the same mass distribution function
!         svar    - max sprim
!         sprim   - actual s
!         rmas    - central value of a resonance mass distribution
!         rgam    - width of a resonance
! OUTPUT: s1, s2  - svar's of two resonances
!         wt      - weight
! for mode=1
! INPUT:  s1, s2  - no generation,  just calculation of weight. 
!
! Written by: M. Skrzypek            date: 2/16/95
! Last update: 5/5/96                  by: Z. Was
!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler     
      DOUBLE PRECISION AMDEC(4)
      DOUBLE PRECISION drvec(100)
      SAVE a,b,winf,wmi,wma,wsqr,wrec,prsqr,prrec
      SAVE uma,umi,uinf,usqr,urec
      SAVE
!
!        write(6,*)'resms2',rmas,rgam
      a=rmas**2
      b=rmas*rgam
! arctg
      winf = 1/b*atan((svar   -a)/b)
      wma  = 1/b*atan((sprim/4d0-a)/b)
      wmi  = 1/b*atan(        -a /b)
! logarithm
      uinf =1/2d0/a*dlog((svar   -a)**2 +b**2)
      uma  =1/2d0/a*dlog((sprim/4d0-a)**2 +b**2)
      umi  =1/2d0/a*dlog(                b**2)
! thetas
      thespr=1d0
      thesvr=1d0
      IF((sprim/4d0).lt.a) thespr=0d0
      IF( svar      .lt.a) thesvr=0d0
      ulo= thespr*uma +(1d0-thespr)*umi
! normalisations
      wsqr=wma-wmi 
      usqr=thespr*(uma-umi)
      prsqr=(wsqr+usqr)**2
      wrec=winf-wma 
      urec=thesvr*(uinf -ulo)
      prrec=(wsqr+usqr)*(wrec+urec)
      prnorm=prsqr+2*prrec
!
!     ====================
      if (mode.ne.1) then 
!     ====================
!
 10   call varran(drvec,5)
      r1=drvec(1)
      r2=drvec(2)
      r3=drvec(3)
      r4=drvec(4)
      r5=drvec(5)

      IF(r3.le.prsqr/prnorm) THEN
!     ..square

!     ....s1
        IF(r4.ge.usqr/(wsqr+usqr)) THEN
!       ..arctg
          w1=r1*(wma-wmi) +wmi
          s1=b*tan(b*w1) +a
        ELSE
!       ..log
          u1=r1*(uma-umi) +umi
          s1=dsqrt(exp(2*a*u1) -b**2)+a
        ENDIF
!     ....s2
        IF(r5.ge.usqr/(wsqr+usqr)) THEN
!       ..arctg
          w2=r2*(wma-wmi) +wmi
          s2=b*tan(b*w2) +a
        ELSE
!       ..log
          u2=r2*(uma-umi) +umi
          s2=dsqrt(exp(2*a*u2) -b**2)+a
        ENDIF

      ELSEIF(r3.le.(prsqr+prrec)/prnorm) THEN
!     ..rectangle 1

!     ....s1
        IF(r4.ge.usqr/(wsqr+usqr)) THEN
!       ..arctg
          w1=r1*(wma-wmi) +wmi
          s1=b*tan(b*w1) +a
        ELSE
!       ..log
          u1=r1*(uma-umi) +umi
          s1=dsqrt(exp(2*a*u1) -b**2)+a
        ENDIF
!     ....s2
        IF(r5.ge.urec/(wrec+urec)) THEN
!       ..arctg
          w2=r2*(winf-wma) +wma     
          s2=b*tan(b*w2) +a
        ELSE
!       ..log
          u2=r2*(uinf-ulo) +ulo
          s2=dsqrt(exp(2*a*u2) -b**2)+a
        ENDIF

      ELSE
!     ..rectangle 2
!         write(6,*)'rect 1'
!     ....s1
        IF(r4.ge.urec/(wrec+urec)) THEN
!       ..arctg
          w1=r1*(winf-wma) +wma     
          s1=b*tan(b*w1) +a
        ELSE
!       ..log
          u1=r1*(uinf-ulo) +ulo
          s1=dsqrt(exp(2*a*u1) -b**2)+a
        ENDIF
!     ....s2
        IF(r5.ge.usqr/(wsqr+usqr)) THEN
!       ..arctg
          w2=r2*(wma-wmi) +wmi
          s2=b*tan(b*w2) +a
        ELSE
!       ..log
          u2=r2*(uma-umi) +umi
          s2=dsqrt(exp(2*a*u2) -b**2)+a
        ENDIF

      ENDIF
!
!     =====
      endif
!     =====

!
! crude distrib. value is 1/W(s_1)*1/W(s_2) see manual for definition
      xcrud=1d0
      IF(s1.gt.a) xcrud=xcrud*a/s1
      IF(s2.gt.a) xcrud=xcrud*a/s2
      xcrud=xcrud
     $  *((s1-rmas**2)**2 +(rmas*rgam)**2)
     $  *((s2-rmas**2)**2 +(rmas*rgam)**2)
!
      wt=prnorm*xcrud
! thresholds
      IF(sqrt(s1)+sqrt(s2).gt.sqrt(sprim)) THEN
        wt=0d0
      ENDIF
!-- check thresholds on decays
      IF(amdec(1)+amdec(2).gt.sqrt(s1)) THEN
        wt=0D0
      ENDIF
      IF(amdec(3)+amdec(4).gt.sqrt(s2)) THEN
        wt=0D0
      ENDIF
      END


      SUBROUTINE cospro(mode,s,s1,s2,costhe,phipro,wt)
*     ***************************************************
! Crude generation of costhe according to a simplified distribution.
! OUTPUT: costhe - cos(theta), theta - polar angle of W- in the CMS 
!         of the incoming beams (+z axis along e- direction)
!         xccos - value of the function
!                      (for mode=1 costhe becames input for xccos
!                                     - no generation)
c
! Written by: M. Skrzypek            date: 3/1/95
! Last update:                         by: 
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      DOUBLE PRECISION drvec(100)
      save
!
CC==>>

!ms      wlambd=s**2+s1**2+s2**2-2*s*s1-2*s*s2-2*s1*s2
      wlambd=max(0d0,(s-s1-s2)**2 -4*s1*s2)
!      write(6,*)s,s1,s2,wlambd
      aa=(s-s1-s2)/2d0
      bb=-dsqrt(wlambd)/2d0
      ymi=dlog(aa-bb)/bb
      yma=dlog(s1*s2/(aa-bb))/bb

!      z=.4d0/s*(aa-bb)*2/s
      z=0d0  ! auxilliary, supposed to be 0


! this was `dead code' 05.06.96 ZW:      wt=2*pi/xccos
!
      IF(mode.ne.1) then
      call varran(drvec,3)
      y=drvec(1)*(yma-ymi)+ymi
      costhe=(exp(bb*y)-aa)/bb
      IF( drvec(2).gt.(yma-ymi)/(z+yma-ymi) )  costhe=2*drvec(1)-1  !
c++      write(6,*)'tran cosgen',aa+bb*costhe
      phipro=2*pi*drvec(3)
      endif
!
      xccos=(1/((s1*s2/(aa-bb))+(bb*costhe-bb)) +z/2d0)/(yma-ymi+z) 
      wt=2*pi/xccos 
      end

      SUBROUTINE cosdec(mode,svar,cdec,phi,wt)
*     ***************************************
! Crude generation of decay costhe according to a simplified distribution.
!   mode: 0-generation
!         1-xccos of given cdec
!   cdec:  value of generated cosine
!   xccos: value of distribution function
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      common / wekin2 / amaw,gammw,gmu,alphaw   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      DOUBLE PRECISION drvec(100)
      save

      KeySpn = MOD(KeyPhy,10000)/1000

      IF(keyspn.eq.1) THEN                        !1002=.78
        IF(svar.gt.500**2) THEN                    !502=.4
          delta=0.4d0+ (svar/500**2 -1)/8d0
        ELSEIF(svar.gt.4*amaw**2) THEN             !162=.4
          delta=.4d0
        ELSEIF(svar.gt.4*(amaw-5*gammw)**2) THEN   !142=.78
          delta=.4d0+ (1-svar/(4*amaw**2))*2d0
        ELSEIF(svar.gt.4*(amaw-10*gammw)**2) THEN  !122=40
          delta=.844d0+ (1-svar/(4*(amaw-5*gammw)**2))*100d0
        ELSE
          delta=40d0
        ENDIF

        IF(mode.eq.0)THEN
 11       call varran(drvec,3)
          cdec=2*drvec(1)-1
          xccos=(1+delta+cdec)/(1+delta)
          IF((2+delta)/(1+delta)*drvec(2).gt.xccos) goto 11
          phi =2*pi*drvec(3)
        ELSE
          xccos=(1+delta+cdec)/(1+delta)
        ENDIF
      ELSEIF(keyspn.eq.0) THEN
        IF(mode.eq.0)THEN
          call varran(drvec,3)
          cdec=2*drvec(1)-1
          phi =2*pi*drvec(3)
        ENDIF
        xccos=1D0
      ENDIF
      wt= 4*pi/xccos
      end

!====================== unused by MS ====================!
!====================== unused by MS ====================!
!====================== unused by MS ====================!

      subroutine invkintt(ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $                  amwmn,amwpn,  bp1,bp2,bp3,bp4)
*     **********************************************************
c This routine calculates inverse kinematics for W-W+ pair production
c and decay 
c OUTPUT:
c         ctn,fin - W-  production angles 
c         ct1n,fi1n - W- decay products angles
c         ct2n,fi2n - W+ decay products angles
c         amwm, amwp - masses of W- and W+ resonances
c INPUT (to be taken from bormom!):
c         bp1(4), bp2(4) - four-momenta of W- decay products
c         bp3(4), bp4(4) - four-momenta of W+ decay products
c
c Written by: Wieslaw Placzek            date: 22.07.1994
c Rewritten by: M. Skrzypek              date: 3/15/95
c Last update: 9/5/96                    by: Z.W.
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
c.. four momenta in CMS' (effective e- beam along z+, exclusively for WWBORN)
      save  / matpar / 
      save
      DOUBLE PRECISION bq1(4),bq2(4),aa(4),bb(4),e1(4)
      dimension bp1(4),bp2(4),bp3(4),bp4(4)
      do i=1,4
           aa(i)= bp1(i)+bp2(i)
          bq1(i)= bp1(i)+bp2(i)
      enddo
        amwmn=sqrt(dmas2(aa))
      do i=1,4
           aa(i)= bp3(i)+bp4(i)
          bq2(i)= bp3(i)+bp4(i)
      enddo
        amwpn=sqrt(dmas2(aa))
      do i=1,4
           bb(i)= bp1(i)+bp2(i)+bp3(i)+bp4(i)
      enddo
        s=(dmas2(bb))
cc ms      qq=dsqrt( (bq1(4)-amwmn)*(bq1(4)+amwmn) )      
      qq=dsqrt(bq1(1)**2+bq1(2)**2+bq1(3)**2)
      qqt=dsqrt(bq1(1)**2+bq1(2)**2)
cc ms
      ctn=bq1(3)/qq
      stn=sqrt(1d0-ctn**2)
cc ms      cfi=bq1(1)/stn/qq
cc ms      sfi=bq1(2)/stn/qq
      IF (QQT.GT.0D0) THEN                                             !cav
        cfi=bq1(1)/qqt
        sfi=bq1(2)/qqt
      ELSE                                                             !cav
        CFI=1D0                                                        !cav
        SFI=0D0                                                        !cav
!        WRITE (6,*)                                                    !cav
!     &    '+++ INVKINTT +++ Warning: ThetaW=0. Phi undefined, set=0.'  !cav
      ENDIF                                                            !cav

cc ms
      fin=acos(cfi)
      if(sfi.le.0d0) fin=2*pi-fin
!
      e1(4)=dsqrt(s/4d0)
      e1(3)=dsqrt(s/4d0)
      e1(2)=0d0
      e1(1)=0d0
      call boostv(1,bq1,bp1,aa)
      call boostv(1,bq1,e1,e1)
      call rotatv(-1,e1,aa,aa)
      qq=sqrt(aa(1)**2+aa(2)**2+aa(3)**2)
      ct1n=aa(3)/qq
cc ms      stn=sqrt(1d0-ct1n**2)
cc ms      cfi=aa(1)/stn/qq
cc ms      sfi=aa(2)/stn/qq
      qqt=sqrt(aa(1)**2+aa(2)**2)
      IF (QQT.GT.0D0) THEN                                             !cav
        cfi=aa(1)/qqt
        sfi=aa(2)/qqt
      ELSE                                                             !cav
        CFI=1D0                                                        !cav
        SFI=0D0                                                        !cav
!        WRITE (6,*)                                                    !cav
!     &    '+++ INVKINTT +++ Warning: ThetaW1=0. Phi undefined, set=0.' !cav
      ENDIF                                                            !cav
cc ms
      fi1n=acos(cfi)
      if(sfi.le.0d0) fi1n=2*pi-fi1n
! dotad ok
!
!
      e1(4)=dsqrt(s/4d0)
      e1(3)=-dsqrt(s/4d0)
      e1(2)=0d0
      e1(1)=0d0
      call boostv(1,bq2,bp4,aa)
      call boostv(1,bq2,e1,e1)
      aa(3)=-aa(3)
      call rotatv(-1,e1,aa,aa)
      qq=sqrt(aa(1)**2+aa(2)**2+aa(3)**2)
      ct2n=aa(3)/qq
cc ms      stn=sqrt(1d0-ct2n**2)
cc ms      cfi=aa(1)/stn/qq
cc ms      sfi=aa(2)/stn/qq
      qqt=sqrt(aa(1)**2+aa(2)**2)
      IF (QQT.GT.0D0) THEN                                             !cav
        cfi=aa(1)/qqt
        sfi=aa(2)/qqt
      ELSE                                                             !cav
        CFI=1D0                                                        !cav
        SFI=0D0                                                        !cav
!        WRITE (6,*)                                                    !cav
!     &    '+++ INVKINTT +++ Warning: ThetaW2=0. Phi undefined, set=0.' !cav
      ENDIF                                                            !cav

cc ms
      fi2n=acos(cfi)
      if(sfi.le.0d0) fi2n=2*pi-fi2n
! 
      end

      SUBROUTINE cosprozz(mode,s,s1,s2,costhe,phipro,wt)
*     ***************************************************
! Crude generation of costhe according to a simplified distribution.
! OUTPUT: costhe - cos(theta), theta - polar angle of W- in the CMS 
!         of the incoming beams (+z axis along e- direction)
!         xccos - value of the function
!                      (for mode=1 costhe becames input for xccos
!                                     - no generation)
c
! Written by: M. Skrzypek            date: 3/1/95
! Last update:                         by: 
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler 
      common /nevik/ nevtru,ifprint
      save /nevik/

      DOUBLE PRECISION drvec(100)
      save
!
CC==>>

!ms      wlambd=s**2+s1**2+s2**2-2*s*s1-2*s*s2-2*s1*s2
      wlambd=max(0d0,(s-s1-s2)**2 -4*s1*s2)
!      write(6,*)s,s1,s2,wlambd
      aa=(s-s1-s2)/2d0
      bb=-dsqrt(wlambd)/2d0
      ymi=dlog(aa-bb)/bb
!      yma=dlog(aa+bb)/bb
      yma=dlog(s1*s2/(aa-bb))/bb
!      z=.4d0/s*(aa-bb)*2/s
      z=0d0  ! auxilliary, supposed to be 0
      IF(mode.eq.0) then

       call varran(drvec,3)

       y=drvec(1)*(yma-ymi)+ymi
       IF( drvec(2).lt.1d0/3d0) then
        costhe=(exp(bb*y)-aa)/bb
       elseIF( drvec(2).lt.2d0/3d0) then
        costhe=-(exp(bb*y)-aa)/bb
       else
        costhe=2*drvec(1)-1  !
       endif
       phipro=2*pi*drvec(3)
      endif
      xccos=1d0/3d0/((s1*s2/(aa-bb))+(bb*costhe-bb))/(yma-ymi)
     $     +1d0/3d0/((s1*s2/(aa-bb))-(bb*costhe+bb))/(yma-ymi)
     $     +1d0/6d0
c++      write(6,*)'tran cosgen',aa+bb*costhe
      wt=2*pi/xccos 
      if (ifprint.eq.1) then
      write(*,*) 'cosprozz',aa,bb,costhe
      write(*,*) ((s1*s2/(aa-bb))+(bb*costhe-bb))*(yma-ymi)
      write(*,*) (s1*s2/(aa-bb)-bb)*(yma-ymi),bb*(yma-ymi)
      write(*,*) ((s1*s2/(aa-bb))-(bb*costhe+bb))*(yma-ymi)
      endif

      end

!========== unused by anybody, kept for future generations =========
!========== unused by anybody, kept for future generations =========
!========== unused by anybody, kept for future generations =========

      subroutine kinett(s,sprim,ct,fi,ct1,fi1,ct2,fi2,
     $                  amwm,amwp,amdec,  q1,q2,p1,p2,p3,p4)
*     **********************************************************
c This routine calculates kinematics for W-W+ pair production
c and decay in e+e- collision in the CMS with z-axis pointing 
c in the e- direction.
c fixes also the 'effective beams', qeff1,qeff2
c INPUT:  s    - beams energy squared (in GeV**2)
c         sprim - actual center mass energy squared (in GeV**2)
c         cthe,fi - W-  production angles 
c         cdec1,fi1 - W- decay products angles
c         cdec2,fi2 - W+ decay products angles
c         amwm, amwp - masses of W- and W+ resonances
c         amdec(4) - decay products masses
c OUTPUT:
c         qeff1(4)      -effective (massless) e- beam in /MOMSET/
c         qeff2(4)      -effective (massless) e+ beam in /MOMSET/
c         q1(4)        - four-momentum of W-  
c         q2(4)        - four-momentum of W+
c         p1(4), p2(4) - four-momenta of W- decay products
c         p3(4), p4(4) - four-momenta of W+ decay products
c
c Written by: Wieslaw Placzek            date: 22.07.1994
c Rewritten by: M. Skrzypek              date: 3/15/95
c Last update: 4/1/95                by: M.S.
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      common / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot  
      save   / matpar /,/ momset /
      save
      DOUBLE PRECISION ef1(4),ef2(4)
      dimension amdec(4), q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      dimension e1(4),e2(4)   !! t-channel stuff
!
c to activate dumps KINDMP=1
      KINDMP=0
c to activate t-channel KINTCH=1
      KINTCH=1

      do i=1,4
        q1(i)=0d0      
        q2(i)=0d0      
        p1(i)=0d0      
        p2(i)=0d0      
        p3(i)=0d0      
        p4(i)=0d0      
        ef1(i)=0d0      
        ef2(i)=0d0      
      enddo

      ecm=sqrt(sprim)
      amwm2=amwm**2
      amwp2=amwp**2
      s1=amwm2
      s2=amwp2
      amp1s=amdec(1)**2
      amp2s=amdec(2)**2
      amp3s=amdec(3)**2
      amp4s=amdec(4)**2
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*) AMDEC(1),AMDEC(2),AMDEC(3),AMDEC(4)
        WRITE(6,*) AMP1S,AMP2S,AMP3S,AMP4S
        WRITE(6,*)'S,SPRIM,S1,S2',S,SPRIM,S1,S2
        WRITE(6,*)'DECAY COS1,2',CT1,CT2
      ENDIF
      st =sqrt(max(0D0,((1d0-ct )*(1d0+ct ))))
c..
      st1=sqrt(max(0D0,((1d0-ct1)*(1d0+ct1))))
      st2=sqrt(max(0D0,((1d0-ct2)*(1d0+ct2))))
c..
!... Momentum q1 of the first resonance
      q1(4)=(sprim+amwm2-amwp2)/(2d0*ecm)
!      qq=dsqrt( (q1(4)-amwm)*(q1(4)+amwm) )
      qq=dsqrt( (sprim-amwm2-amwp2)**2 -4*amwm2*amwp2 )/(2*ecm)
      q1(1)=qq*st*cos(fi)
      q1(2)=qq*st*sin(fi)
      q1(3)=qq*ct
      q1(4)=dsqrt(amwm2+q1(1)**2+q1(2)**2+q1(3)**2)
!... Momentum p1 in the rest frame of the first resonance
      ppene=(s1+amp1s-amp2s)/(2d0*amwm)
!      ppe=dsqrt( (ppene-amdec(1))*(ppene+amdec(1)) )
      ppe=dsqrt( (s1-amp1s-amp2s)**2 -4*amp1s*amp2s )/(2d0*amwm)
      p1(1)=ppe*st1*cos(fi1)
      p1(2)=ppe*st1*sin(fi1)
      p1(3)=ppe*ct1
c      p1(4)=ppene
      p1(4)=dsqrt(amdec(1)**2+p1(1)**2+p1(2)**2+p1(3)**2)
      IF(KINTCH.EQ.1)THEN
!!!!!!! for cosdec_t !!!!!!!
!beam
      e1(4) =        dsqrt(s/4d0)
      e1(3) =        dsqrt(s/4d0)
      e1(2) =        0d0
      e1(1) =        0d0
!boost beam to W- rest fr.
      call boostv(1,q1,e1,e1)
!rotate p1 from frame parallel to ef1 to CMS-oriented
      call rotatv(1,e1,p1,p1)
!!!!!!! end for cosdec_t !!!!!!!
      ENDIF

c...
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1 '
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
!... Boost to CMS-WW frame
      call boostv(-1,q1,p1,p1)
!... Momentum p2 of the second product of first resonance decay
      do 10 k=1,4
 10   p2(k)=q1(k)-p1(k)
c.. fine tuning on masses
      p1(4)=dsqrt(amdec(1)**2+p1(1)**2+p1(2)**2+p1(3)**2)
      p2(4)=dsqrt(amdec(2)**2+p2(1)**2+p2(2)**2+p2(3)**2)
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1,P2 '
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
!... Momentum q2 of the second resonance
!ms      q2(4)=ecm-q1(4)
      do 20 k=1,3
 20   q2(k)=-q1(k)
      q2(4)=dsqrt(amwp2+q2(1)**2+q2(2)**2+q2(3)**2)
      IF(KINTCH.NE.1)THEN
!... Momentum p3 in the rest frame of the second resonance
      ppene=(s2+amp3s-amp4s)/(2d0*amwp)
!      ppe=dsqrt( (ppene-amdec(3))*(ppene+amdec(3)) )
      ppe=dsqrt( (s2-amp3s-amp4s)**2 -4*amp3s*amp4s )/(2d0*amwp)
      p3(1)=ppe*st2*cos(fi2)
      p3(2)=ppe*st2*sin(fi2)
      p3(3)=ppe*ct2
c      p3(4)=ppene
      p3(4)=dsqrt(amdec(3)**2+p3(1)**2+p3(2)**2+p3(3)**2)
c...
      ELSE
!!!!!!! for cosdec_t !!!!!!!
!... Momentum p4 in the rest frame of the second resonance
      ppene=(s2+amp4s-amp3s)/(2d0*amwp)
!      ppe=dsqrt( (ppene-amdec(4))*(ppene+amdec(4)) )
      ppe=dsqrt( (s2-amp3s-amp4s)**2 -4*amp3s*amp4s )/(2d0*amwp)
      p4(1)=ppe*st2*cos(fi2)
      p4(2)=ppe*st2*sin(fi2)
      p4(3)=ppe*ct2
c      p4(4)=ppene
      p4(4)=dsqrt(amdec(4)**2+p4(1)**2+p4(2)**2+p4(3)**2)
!beam
      e2(4) =        dsqrt(s/4d0)
      e2(3) =       -dsqrt(s/4d0)
      e2(2) =        0d0
      e2(1) =        0d0
!boost beam to W- rest fr.
      call boostv(1,q2,e2,e2)
!rotate p1 from frame parallel to ef1 to CMS-oriented
      call rotatv(1,e2,p4,p4)
      p4(1)=p4(1)
      p4(2)=p4(2)
      p4(3)=-p4(3)
!!!!!!! end for cosdec_t !!!!!!!
      ENDIF

      IF(KINDMP.EQ.1)THEN
        IF(KINTCH.NE.1) WRITE(6,*)'Q1,P1,P2,Q2,P3 '
        IF(KINTCH.EQ.1) WRITE(6,*)'Q1,P1,P2,Q2,P4 '
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
!... Boost to CMS-WW frame
        IF(KINTCH.NE.1) THEN
          call boostv(-1,q2,p3,p3)
        ELSE
          call boostv(-1,q2,p4,p4)
        ENDIF
!... Momentum p3 of the second product of second resonance decay
      do 30 k=1,4
        IF(KINTCH.NE.1) THEN
          p4(k)=q2(k)-p3(k)
        ELSE
          p3(k)=q2(k)-p4(k)
        ENDIF
 30     continue
c.. fine tuning on masses
      p4(4)=dsqrt(amdec(4)**2+p4(1)**2+p4(2)**2+p4(3)**2)
      p3(4)=dsqrt(amdec(3)**2+p3(1)**2+p3(2)**2+p3(3)**2)
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1,P2,Q2,P3,P4, WW frame '
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'LAB NO PHOTS'
        CALL DUMPL(6,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF

      end


      SUBROUTINE cosdec_t_mm(mode,ibeam,svar,sprim,s1,s2,ct,fi,ambeam,
     @                    amfi1,amfi2,   costhe,phi,wt)
*     ***************************************
! Crude generation of decay costhe according to a simplified distribution.
!   mode: 0-generation
!         1-xccos of given cdec
!   cdec:  value of generated cosine
!   xccos: value of distribution function
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      common / wekin2 / amaw,gammw,gmu,alphaw   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      DOUBLE PRECISION drvec(100)
      save
C simplified version of this routine.
      xx=4*ambeam**2/svar
      beta=sqrt(1d0-4*ambeam**2/svar)
      xlog=log((1+beta)**2/xx)
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
      IF(mode.eq.0) then
 5      continue
        call varran(drvec,3)
       IF( drvec(2).lt.1d0/4d0 ) then
        costhe=-1d0/beta*(xx/(1+beta)*exp(xlog*drvec(1))-1d0)
        u=(log((1D0+beta)/4D0))**2 +xlog*xlog1*drvec(1)
        costhe=-1D0/beta*(4D0*EXP(-SQRT(u))-1)
       elseIF( drvec(2).lt.1d0/2d0 ) then
        costhe=-1d0/beta*(xx/(1+beta)*exp(-xlog*drvec(1))-1d0)
!       elseIF( drvec(2).lt.3d0/4d0 ) then
!        costhe= 1d0/beta*(xx/(1+beta)*exp(-xlog*drvec(1))-1d0)
       else
        costhe=2*drvec(1)-1 !
       endif
       phi=2*pi*drvec(3)
      IF (COSTHE.eq.1d0.or.COSTHE.eq.-1D0) goto 5
      endif
      cost=min(1d0,costhe)
      xccos=1d0/4d0+1d0/4d0/beta/(-xlog)*
     $      (1d0/(xx/(1d0+beta)+beta*(1D0-cost)))
!      xccos=xccos+1d0/3d0/beta/(-xlog)*
!     $      (1d0/(xx/(1d0+beta)+beta*(1D0+cost)))

      xccos=xccos+1d0/8d0*beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-cost)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-cost))))!!! +1d0/(1+beta*costhe))
! ms 17.06.96 here was wrong sign.
      wt= 4*pi/xccos/2
!
      end

      SUBROUTINE cosdec_xt(mode,ibeam,svar,sprim,s1,s2,ct,fi,ambeam,
     @                    amfi1,amfi2,   costhe,phi,wt)
*     ***************************************
! Crude generation of decay costhe according to a simplified distribution.
!   mode: 0-generation
!         1-xccos of given cdec
!   cdec:  value of generated cosine
!   xccos: value of distribution function
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      common / wekin2 / amaw,gammw,gmu,alphaw   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      DOUBLE PRECISION drvec(100),q1(4),e1(4),e1p(4)
      save

      ecm=sqrt(sprim)
      amwm=dsqrt(s1)
      amwp=dsqrt(s2)
      amwm2=s1
      amwp2=s2
      st =sqrt((1-ct )*(1+ct ))

!... Momentum q1 of the first resonance
      q1(4)=(sprim+amwm2-amwp2)/(2d0*ecm)
!      qq=dsqrt( (q1(4)-amwm)*(q1(4)+amwm) )
      qq=dsqrt( (sprim-amwm2-amwp2)**2 -4*amwm2*amwp2 )/(2*ecm)
      q1(1)=-ibeam*qq*st*cos(fi)
      q1(2)=-ibeam*qq*st*sin(fi)
      q1(3)=-ibeam*qq*ct
      q1(4)=dsqrt(amwm2+q1(1)**2+q1(2)**2+q1(3)**2)

!beam
      e1(4) =        dsqrt(svar/4d0)
      e1(3) = -ibeam*dsqrt(svar/4d0 -ambeam**2)
      e1(2) =        0d0
      e1(1) =        0d0
!transform beam to W rest fr.
!      call boostv(1,q1,q1,e1p)
!      write(6,*)'cosdec_t=>',e1
!      write(6,*)'        =>',e1p,amwm
!      call rotatv(-1,q1,q1,e1p)
!      write(6,*)'        =>',e1p
      call boostv(1,q1,e1,e1p)
!first fermion
      x0 = 1/2d0/amwm*(amwm2-amfi2**2+amfi1**2)
      xx = dsqrt(x0**2 -amfi1**2)
      ee = dsqrt(e1p(4)**2 -ambeam**2)


      aa = x0*e1p(4)   !-(ambeam**2+amfi1**2)/2d0
      bb = -xx*ee
cc      write(6,*)'aa,bb',aa,bb,aa+bb
cc      write(6,*)'x0,e1p(4),xx,ee',x0,e1p(4),xx,ee

      cosmax =   1-2d-8
      cosmin = -(1-2d-8)

      ymi=dlog(aa+cosmin*bb)/bb
      yma=dlog(aa+cosmax*bb)/bb
cc      tmin=11d0
cc      yma=dlog(tmin)/bb

      z=.4d0/svar*(aa-bb)*2/svar
      z=.1d0*(aa-bb)/svar
      z=.5d0*(aa-bb)**2/svar**2
      z=.02d0
      z=.004d0*svar/(aa-bb)          !! best of all (180gev) !!
!!      write(6,*)'zety ',(aa-bb)/svar,(aa-bb)**2/svar,(aa-bb)**2/svar**2
!      z=0d0  ! auxilliary, supposed to be 0

      IF(mode.eq.0) then

       call varran(drvec,3)

       y=drvec(1)*(yma-ymi)+ymi
       costhe=(exp(bb*y)-aa)/bb
       IF( drvec(2).gt.(yma-ymi)/(z+yma-ymi) )  costhe=2*drvec(1)-1 !
       phi=2*pi*drvec(3)
      endif
      xccos=2*(1/(aa+bb*costhe) +z/(cosmax-cosmin))/(yma-ymi+z) 
      wt= 4*pi/xccos
!
cc      write(6,*)'tran cosgen',aa+bb*costhe,costhe,xccos,xcc/xccos
cc      write(6,*)'tran yma,ymi',yma,ymi,yma-ymi

      end

      SUBROUTINE res3gn_mm(mode,svar,sprim,rmas,rgam,amdec,s1,s2,wt)
!     ***************************************************************
! Generation of ds_1ds_2 distribution within phase space boundaries
! using weighted (pre-sampled) events
!---------------------
! note:
! so far generation is within theta_crude and fine tuning is added at the
! end. For non-acceptable events weight is set to zero.
!---------------------
! breit-wigners pre-samplers in both s_1 and s_2 channels are set.
! total volume 'prnorm' ( S(s') defined in formula 31 of koralw 1.02 manual) 
! is calculated including additional W(s_1)*W(s_2) factor 
! (see koralw 1.02 manual). To obtain proper ds_1ds_2 distribution
! weight wt=prnorm/W(s_1)/W(s_2) must be included, and this will help later
! cancelling singularities of matrix element
! 
! note: both resonances have the same mass distribution function
!         svar    - max sprim
!         sprim   - actual s
!         rmas    - central value of a resonance mass distribution
!         rgam    - width of a resonance
! OUTPUT: s1, s2  - svar's of two resonances
!         wt      - weight
! for mode=1
! INPUT:  s1, s2  - no generation,  just calculation of weight. 
!
! Written by: M. Skrzypek            date: 2/16/95
! Last update: 5/5/96                  by: Z. Was
!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
! This common contains parameters of non-established particles such as higgs
! Which need to be used by pre-sampler (to be activated by dipswitch IHIG
      COMMON / WEKIN3 / AMHIG,GAMHIG,IHIG
      SAVE / matpar /,/articut/,/ WEKIN3 /     
      DOUBLE PRECISION AMDEC(4),amd(4)
      DOUBLE PRECISION drvec(100)
      SAVE
      do k=1,4
       amd(k)=amdec(k)
       if (amd(k).lt.0.0005d0) amd(k)=0.000511d0
      enddo

      ALP2=ATAN((sprim-rmas**2)/rmas/rgam)
      ALP1=ATAN(((amd(1)+amd(2))**2-rmas**2)/rmas/rgam)
      BLP2=ATAN((sprim-rmas**2)/rmas/rgam)
      BLP1=ATAN(((amd(3)+amd(4))**2-rmas**2)/rmas/rgam)
      IF (IHIG.EQ.1) THEN
       CLP2=ATAN((sprim-AMHIG**2)/AMHIG/GAMHIG)
       CLP1=ATAN(((amd(1)+amd(2))**2-AMHIG**2)/AMHIG/GAMHIG)
       DLP2=ATAN((sprim-AMHIG**2)/AMHIG/GAMHIG)
       DLP1=ATAN(((amd(3)+amd(4))**2-AMHIG**2)/AMHIG/GAMHIG)
       PROB1=1D0/3D0
       PROB2=2D0/3D0
       PROB3=2D0/3D0
       PROB4=1D0
      ELSE
       CLP2=ATAN((sprim-rmas**2)/rmas/rgam)
       CLP1=ATAN(((amd(1)+amd(2))**2-rmas**2)/rmas/rgam)
       DLP2=ATAN((sprim-rmas**2)/rmas/rgam)
       DLP1=ATAN(((amd(3)+amd(4))**2-rmas**2)/rmas/rgam)
       PROB1=1D0/2D0
       PROB2=1D0/2D0
       PROB3=1D0/2D0
       PROB4=1D0
      ENDIF
      biglog1=log(sprim/(amd(1)+amd(2))**2)
      biglog2=log(sprim/(amd(3)+amd(4))**2)
!
!     ====================
      if (mode.ne.1) then 
!     ====================
!
 10   call varran(drvec,5)
      r1=drvec(1)
      r2=drvec(2)
      r3=drvec(3)
      r4=drvec(4)
      r5=drvec(5)

      if(r3.lt.PROB1) then      
        ALP=ALP1+R1*(ALP2-ALP1)
        s1=rmas**2+rmas*rgam*TAN(ALP)
      elseif(r3.lt.PROB2) then  
        CLP=CLP1+R1*(CLP2-CLP1)
        s1=AMHIG**2+AMHIG*GAMHIG*TAN(DLP)    
      elseif(r3.lt.PROB3) then      
        s1=(sprim-(amd(1)+amd(2))**2)*r1+(amd(1)+amd(2))**2 
      else
        s1=(amd(1)+amd(2))**2*exp(r1*biglog1)
      endif
      if(r4.lt.PROB1) then   
        ALP=BLP1+R2*(BLP2-BLP1)
        s2=rmas**2+rmas*rgam*TAN(ALP)
      elseif(r4.lt.PROB2) then  
        DLP=DLP1+R2*(DLP2-DLP1)
        s2=AMHIG**2+AMHIG*GAMHIG*TAN(DLP) 
      elseif(r4.lt.PROB3) then   
        s2=(sprim-(amd(3)+amd(4))**2)*r2+(amd(3)+amd(4))**2
      else
        s2=(amd(3)+amd(4))**2*exp(r2*biglog2)
      endif
!     =====
      endif
!     =====
      ph1c=(sprim-(amdec(1)+amdec(2))**2)
      ph2c=(sprim-(amdec(3)+amdec(4))**2)
!
      PH1a=((s1-rmas**2)**2+(rmas*rgam)**2)/(rmas*rgam)
      PH1a=PH1a*(ALP2-ALP1)
      PH2a=((s2-rmas**2)**2+(rmas*rgam)**2)/(rmas*rgam)
      PH2a=PH2a*(BLP2-BLP1)
!
      IF (IHIG.EQ.1) THEN
       PH1b=((s1-AMHIG**2)**2+(AMHIG*GAMHIG)**2)/(AMHIG*GAMHIG)
       PH1b=PH1b*(CLP2-CLP1)
       PH2b=((s2-AMHIG**2)**2+(AMHIG*GAMHIG)**2)/(AMHIG*GAMHIG)
       PH2b=PH2b*(DLP2-DLP1)
      ELSE
       PH1b=((s1-rmas**2)**2+(rmas*rgam)**2)/(rmas*rgam)
       PH1b=PH1b*(CLP2-CLP1)
       PH2b=((s2-rmas**2)**2+(rmas*rgam)**2)/(rmas*rgam)
       PH2b=PH2b*(DLP2-DLP1)
      ENDIF
!      
      ph1d=s1*biglog1
      ph2d=s2*biglog2
!!!
      ph1=1/( PROB1       /ph1a+(PROB2-PROB1)/ph1b
     $      +(PROB3-PROB2)/ph1c+(PROB4-PROB3)/ph1d)
      ph2=1/( PROB1       /ph2a+(PROB2-PROB1)/ph2b
     $      +(PROB3-PROB2)/ph2c+(PROB4-PROB3)/ph2d)
!      ph1=3/(1d0/ph1a+1d0/ph1b+1d0/ph1c)
!      ph2=3/(1d0/ph2a+1d0/ph2b+1d0/ph2c)
      prnorm=ph1*ph2
      wt=prnorm
! thresholds
      IF(sqrt(s1)+sqrt(s2).gt.sqrt(sprim)) THEN
        wt=0d0
      ENDIF
!-- check thresholds on decays
      IF(amdec(1)+amdec(2).gt.sqrt(s1)) THEN
        wt=0D0
      ENDIF
      IF(amdec(3)+amdec(4).gt.sqrt(s2)) THEN
        wt=0D0
      ENDIF
      if(mode.eq.1.and.wt.eq.0d0) then
      write(6,*) 'vol=',ph1,ph2
      write(6,*) sqrt(s1),'+',sqrt(s2),'.gt.',sqrt(sprim)
      write(6,*) amdec
      endif
      END


*////////////////////////////////////////////////////////////////////////////////
*//     !!!!!!      This file is OBSOLETE           !!!!!!                     //
*//               Replacement is KW.f + KW.h                                   //
*////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE koralw_obsolete(mode,xpar_input)
*////////////////////////////////////////////////////////////////////////////////
*//   ======================================================================   //
*//   ======================================================================   //
*//   ===========================KORALW=====================================   //
*//   ======================WW pair production==============================   //
*//   =================initial state exponentiation=========================   //
*//   ======================================================================   //
*//   ======================================================================   //
*//   ======================== version 1.42 ================================   //
*//   ======================================================================   //
*//   ========================= August 1998 ================================   //
*//   ======================================================================   //
*////////////////////////////////////////////////////////////////////////////////
*     This program is written by:
*          S. Jadach      (Stanislaw.Jadach@cern.ch)
*          W. Placzek     (Wieslaw.Placzek@cern.ch)
*          M. Skrzypek    (Maciej.Skrzypek@cern.ch)
*          B.F.L. Ward    (bflw@slac.stanford.edu)
*          Z. Was         (Zbigniew.Was@cern.ch)
*////////////////////////////////////////////////////////////////////////////////
* INPUT:
* mode =-1/0/1/2 defines
*       initialization/generation/give-xsection/final-report
* All other in put is in data_DEAFAULT file which has to be read
* separately with KoralW_ReaDataX, see demo demostration program
*////////////////////////////////////////////////////////////////////////////////
* OUTPUT momenta:
* Four-momenta and photon multiplicity in standard COMMON hepevt:
*      COMMON/hepevt/nevhep,nhep,isthep(nmxhep),idhep(nmxhep),
*     &jmohep(2,nmxhep),jdahep(2,nmxhep),phep(5,nmxhep),vhep(4,nmxhep)
* Also in the KORALW internal COMMON's /momset/ and /momdec/:
*     COMMON / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot
*         effective PARAMETERs for matr.el. only (e-,e+); ... ;photons ;phot. multip
*     COMMON / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
*        w- ;w+ ;f (w-);\bar f (w-);f (w+);\bar f (w+);
*////////////////////////////////////////////////////////////////////////////////
* OUTPUT normalization:
* xpar(20)  =XsecMC = Mote Carlo total cross section in picobarns
* xpar(21)  =XErrMC = Mote Carlo error in picobarns
* xpar(30)  =XnorMC = Normalization  cross section in picobarns,  
*                   = XsecMC for WtMod=1 events, and 
*                   = Crude xsection for variable weight events
* xpar(31)  =wtmax  = maximum wt used for rejection (obsolete)
* xpar(1010)=nevtru = number of accepted unweighted events or
*                   number of generated weighted events
* xpar(1011)=nevtot = number of generated events (before rejection)
*                   (for weighted events nevtru=nevtot)
*////////////////////////////////////////////////////////////////////////////////
* OUTPUT weights:
* Normaly the user may request with KeyWgt=1 weighted events and then
* should use  PRINCIPAL model weight WtMod from the COMMON block /wgtall/.
*
* For advanced users only: for KeyWgt=1 instead of WtMod user
* may use WtAux = wtcrud*wtset(i), where  wtcrud and wtset(i) are from
* in COMMON block /wgtall/.
* The meaning of auxiliary WtAux is the following:
*     wtset( 1) =   born
*     wtset( 2) =   first order
*     wtset( 3) =   second order
*     wtset( 4) =   third order
* and the corresponding components
*     wtset(11-12) =   first order, betas
*     wtset(13-15) =   second order, betas
*     wtset(16-19) =   third order, betas
* N.B. wtmod=wtcrud*wtset(4) in standard case with ISR
*      wtmod=wtcrud*wtset(1) in case of no ISR bremsstrahlung
* in the case of the external four-fermion matrix el. the corresponding 
* Matrix element is provided in
*     wtset(40)=wtbo4f 
*     wtset(40+i4f)=wt4f(i4f) provide place for additional information
* from external matrix el. if set by the user. These slots are not used.
*////////////////////////////////////////////////////////////////////////////////
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      SAVE
      REAL*8  xpar_input( 10000) ! in main program
      REAL*8  xpar( 10000)       ! local
      INTEGER npar( 1000)        ! local
* Work-area for internal histogramming package Glibk
      COMMON / cglib / blibk(50000)
      SAVE   / cglib /
*
      COMMON / matpar / pi,ceuler
      COMMON / phypar / alfinv,gpicob
      COMMON / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot
      COMMON / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      COMMON / cms_eff_momdec /
     $      effbeam1(4),effbeam2(4),effp1(4),effp2(4),effp3(4),effp4(4)
      COMMON / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf
      COMMON / wekin2 / amaw,gammw,gmu,alphaw
      COMMON / wt_max / wtmax,wtmax_cc03     
*
* this COMMON can be everywhere, contains various switches
      COMMON / keykey/  keyrad,keyphy,keytek,keymis,keydwm,keydwp
* tauola, photos and jetset overall switches
      COMMON / libra  / jak1,jak2,itdkrc,ifphot,ifhadm,ifhadp
*
* wtborn is added in koralw
      COMMON / wgtgen / wtves,wtyfs,wtborn
      COMMON / wgtall / wtcrud,wtmod,wtset(100)
      COMMON / inout  / ninp,nout
      COMMON / bxfmts / bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      CHARACTER*80      bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      COMMON / decays / IFlav(4), amdec(4) 
      COMMON / DECDAT / AMAFIN(20), BR(20)

*-- 4fermion weights
      DIMENSION wt4f(9)
*-- vector of random numbers
      DIMENSION drvec(100)
*-- single precision parameter for tauola
      REAL pol(4)

      DIMENSION bsp(4)
*-- dipswitch kardmp:  printout on wt over wtmax: 1-on, 0-off
      DATA kardmp /1/
* ==================================================================
* =====================initialization===============================
* ==================================================================
*     *******************
      IF(mode .EQ. -1) THEN
*     *******************
*///////////////////////////////////////////////////////////////
*//        Compulsory Initialization of GLIBK                 //
*///////////////////////////////////////////////////////////////
      CALL glimit(50000)
      CALL goutpu(nout)
*////////////////////////////////////////////////////////////////////////////
*// Xpar should be  essentialy an image of the input in the main program   //
*// In the present version it also plays role of additional "common block" //
*// communicating between subprograms, vide filexp.f and setmas_koralw.f   //
*// In order to split this double role I introduce xpar_input which is not //
*// modified (see exception below)  and local xpar which acts as additional//
*// address area residing in THIS program and sent as a pointer downwards. //
*// Now the main program (including Korwan) knows nothing about changes    //
*// in local the xpar done in filexp.                                      //
*// Exception is that xpar_input sends information outside for mode=1,2.   //
*// This role should disappear, and final XsecMC should have its "getter"  //
*// Of course, xpar_input is copied into xpar, see below:                  //
*////////////////////////////////////////////////////////////////////////////
      DO i = 1, 10000
         xpar(i) = xpar_input(i)
      ENDDO
* initialization of COMMON blocks
* translation xpar-->npar
      CALL FILExp_Obsolete(xpar,npar)
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
* identificator for this generator
      idgen = 7
* important histo which remembers total x-section
      CALL gmonit(-1, idgen,1d0,1d0,1d0) ! idgen=7
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
* this is "pointer" for internal monitoring histograms/averages
      idyfs = 0
      svar=4*ene**2
      keywgt = MOD(keytek,10)
      keysmp = MOD(keytek,1000)/100
      keyisr = MOD(keyrad,10)
      key4f  = MOD(keymis,100)/10
      KeyAcc = MOD(KeyMis,1000)/100
! Soft photon cut-off
      vvmin  = xpar(8)
! Strong coupling constant
      alpha_s = xpar(13)
! Colour Re-Connection probability
      PReco = xpar(19)
      DO i=1,100
        wtset(i)=0
      ENDDO
*!!!!!!!!!! this should go out to tests [[[[[[[[[[[[[[[
* ============================================================
* let us keep for KORALW the glibk id-ent range from 2 to 1000
* ============================================================
* Principal weight 
      CALL gmonit(-1,idyfs+80,0d0,1d0,1d0)  ! total xs
      CALL gmonit(-1,idyfs+81,0d0,1d0,1d0)  ! xs for wt<0
      CALL gmonit(-1,idyfs+82,0d0,1d0,1d0)  ! xs for wt>wtmax
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!]]]]]]]]]]]]]]]
      WRITE(6,*) 'KORALW  <-3>'
*-- initialization of qed part
      CALL karlud(-1,xcrude,dum2,dum3)
      ievacc=0
      nevtot=0
      nevtru=0
cccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccc
* initialization tauola photos etc.
*!      IF( keywgt  .EQ.  0) THEN
         WRITE(6,*) '>>>>>>>> initialization tauola photos etc.'
         CALL  inietc(npar(21),npar(22),npar(23),npar(24))
         CALL  inimas
         CALL  iniphx(0.01d0)
         CALL  initdk
         CALL  phoini
*!      ENDIF
cccccccccccccccccccccccccccccccccc
* initialization of 4fermion matrix el. libraries
      IF( key4f .NE. 0 ) THEN
        CALL ampinw(xpar,npar)
      ENDIF
cccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccc
      wtu=0d0
!-- activates 4fermion tests
      i_4f=1
!-- beta functions tests, activated with i_beta=1
      i_beta=1
      IF( keyisr .NE. 0 ) THEN
*-- initialize tests of beta functions
         IF(i_beta. EQ. 1) CALL beta_tests(-1,idyfs,xcrude,wtkarl,wtset)
      ENDIF
!-- 4fermion monitoring
      IF(i_4f .EQ. 1) 
     $  CALL f4_tests(-1,idyfs,xcrude,wttot,wtboww,wtbo4f,wt4f) 
! monitoring xsections in different decay channels
      CALL decay_monit(-1,wtmod,xcrude,svar,label_dumm,nout)
! counter for z_ libraries reset
      CALL z_counter(-1,idum)
!======== weights monitoring beg ===========
      DO i=58,60
        CALL gmonit(-1,idyfs+i,0d0,1d0,1d0)
      ENDDO
!======== weights monitoring end ===========
* ==================================================================
* ========================generation================================
* ==================================================================
*     ***********************
      ELSEIF(mode .EQ. 0) THEN
*     ***********************
      nevtru=nevtru+1
! fill the counter (it counts nevtru)
      CALL z_counter(0,idum)
 200  CONTINUE
      nevtot=nevtot+1
      CALL karlud( 0,xcborn,wtkarl,wtdumm)
! find out the label
      CALL store_label(1,label)
! check if requested final state is a CC03 + MIX + doubly CKM suppresses 
! if so it is 'NC03'  and we suppress it
! equivalently it is ZZ with CC03
      CALL linear_to_WZ_label(1,label,icwm,icwp,if_z,if_w)
      IF(key4f.EQ.0 .AND. if_z.EQ.1) THEN
        wtkarl=0d0
        wtborn=0d0
      ENDIF
      wtcrud = wtkarl
*     ============================
      IF( KeyWgt .EQ. 2 ) THEN
*     ============================
*********************************************************
*   Constant weight (wt=1) events for internal CC03     *
*********************************************************
         IF (wtcrud .NE. 0d0) THEN
           wtcc03 = wwborn(effp1,effp2,effp3,effp4,keyacc)
           DO i=1,4
             bsp(i) = effp1(i)+effp2(i)+effp3(i)+effp4(i)
           ENDDO
           sp = dmas2(bsp)
*-- Born level flux factor 1/2s'
           fluxf = 1d0/(2d0*sp)
           wtcc03 = wtcc03*fluxf*gpicob
           wtbww = wtcrud*wtcc03
!---------------------------
!-- pseudo-rejection loop for internal CC03
           CALL varran(drvec,1)
           rn = drvec(1)
           IF (wtbww .LT. rn*wtmax_cc03) THEN
             wtcrud = 0d0
           ELSE
             wtcrud = wtmax_cc03/wtcc03
           ENDIF
!---------------------------
         ENDIF
       ENDIF
********************************************************
*-- (CC03,4-fermions) + Coulomb + ACC + nQCD weights --*
********************************************************
      CALL model_4f(wtcrud,wtboww,wtbo4f,wt4f,br,alpha_s,
     $              effp1,effp2,effp3,effp4,label,key4f,keyacc)
*---
!-- CC03 Born weight
      wtwwpb = wtboww*gpicob
!-- Total Born weight (4f) 
      wtborn = wtbo4f*gpicob
*---------------------------------------------------------
!======== weights monitoring begin ===========
!-- phase space volume (crude weight, no Born) 
      CALL gmonit(0,idyfs+59,wtcrud, 0d0,0d0)
!-- CC03 Born, no betas
      wtbww = wtcrud*wtwwpb
      CALL gmonit(0,idyfs+58,wtbww, wtmax_cc03,0d0)
!-- cc03 born OVER wtmax, no betas
      wtovr = MAX(0d0,wtbww - wtmax_cc03)
      CALL gmonit(0,idyfs+60,wtovr, 0d0,0d0)
* Correcting "bad" QED t-channel weights for eexx final states 
      CALL eexx_wt_cor(wtcrud,svar,amel,iflav,vvmin,wtcort)
      wtborc = wtborn *wtcort
*  QED  ISR model weight
      IF( keyisr .NE. 0 ) THEN
         CALL betar(alfinv,wtborc,svar,amel,nphot,sphot,wtset)
*-- tests of beta functions
         IF(i_beta. EQ. 1) 
     $       CALL beta_tests(0,idyfs,xcrude,wtcrud,wtset) 
      ENDIF
*---------------------------------------------------------
* **********************************
*       Total (principal) weight
* **********************************
      IF( keyisr  .EQ.  0 ) THEN
        wttot    =wtcrud*wtborn
        wtset(1) =wtborn
      ELSE
        wttot    =wtcrud*wtset(4)
      ENDIF
!==================================================
! total weight monitoring
      CALL gmonit(0,idyfs+80,wttot,  wtmax,0d0)
! events with wt<0
      wtneg = MIN(wttot,0d0)
      CALL gmonit(0,idyfs+81,wtneg,  0d0,0d0)
! events with wt>wtmax
      wtovr = MAX(0d0,wttot-wtmax)
      CALL gmonit(0,idyfs+82,wtovr,  0d0,0d0)

! monitoring xsections in different decay channels
      CALL decay_monit(0,wttot,xcrude,svar,label,nout)

!-- 4fermion monitoring
      IF(i_4f .EQ. 1) 
     $       CALL f4_tests(mode,idyfs,xcrude,wttot,wtboww,wtbo4f,wt4f)
!==================================================
!======== weights monitoring end ===========
*     ============================
      IF( KeyWgt .EQ. 0 ) THEN
*     ============================
********************************************
*       Constant weight (wt=1) events      *
********************************************
!---------------------------
!-- principal rejection loop
         CALL varran(drvec,1)
         rn = drvec(1)
         IF (wttot .LT. rn*wtmax ) GOTO 200
!---------------------------
         DO i=1,100
            wtset(i)=0
         ENDDO
         wtcrud = 1d0
! Principal event weight =1 now!
         wtmod  = 1d0
*     ============================
      ELSEIF( KeyWgt .EQ. 1 ) THEN
*     ============================
*****************************************
*        Variable weight events         *
*****************************************
! Principal event weight
         wtmod = wttot
! Remembers crude x-section and total number of events 
         CALL gmonit(  0, idgen,      xcrude, wtmax,0d0)
*     =====
      ELSEIF( KeyWgt .NE. 2) THEN
*     =====
         WRITE(6,*)'KORALW==>wrong keywgt=',keywgt
         STOP
*     =====
      ENDIF
*     =====
*-------------------
* dump for debugging
*-------------------
      IF(keysmp .EQ. 2  .AND.  kardmp .EQ. 1
     @    .AND.  wttot/wtmax .GE. 1d0) THEN
        wtu=max(wttot/wtmax,wtu)
        CALL mm_dumper(12,6,nevtru,wttot,wtu,wtmax,wtmod,wtbo4f,iflav)
        CALL ww_dumper(6,svar,amel,wtcort) 
      ELSEIF( keysmp .EQ. 1  .AND.  kardmp .EQ. 1
     @    .AND.  (wttot/wtmax .GT. 1d0  .OR.  nevtru .EQ. -3320) ) THEN
        wtu=max(wttot/wtmax,wtu)
        CALL zz_dumper(6,nevtru,wttot,wtu,wtmax,wtmod,wtbo4f,iflav)
        CALL ww_dumper(6,svar,amel,wtcort) 
      ELSEIF( keysmp .EQ. 3  .AND.  kardmp .EQ. 1
     @    .AND.  wttot/wtmax .GT. 1d0 ) THEN
        wtu=max(wttot/wtmax,wtu)
        CALL mm_dumper(12,6,nevtru,wttot,wtu,wtmax,wtmod,wtbo4f,iflav)
        CALL ww_dumper(6,svar,amel,wtcort) 
        CALL zz_dumper_short(6)
      ENDIF
*-------------------
* END dump for debugging
*-------------------
      wtset(40)=wtbo4f
      DO i4f=1,9
        wtset(40+i4f)=wt4f(i4f)
      ENDDO
*--------------------------------------------------------------------
* ccccccccccccccccccccccccccccccccc
      IF( wtcrud  .NE.  0d0) THEN
* ccccccccccccccccccccccccccccccccc
* tohep sets into hepevt all generated particles.
*       it decays taus and generates bremsstrahlung
*       in tau and w decays.
         CALL tohep
* and tohad moves to lund FORMAT.
* it hadronizes whatever requires.
         CALL tohad(ifhadm,ifhadp,PReco)
      ELSE
* some routine to set hepevt to 0 should be here <<<<<============
        CONTINUE
* ccccccccccccccccccccccccccccccccc
      ENDIF
* ccccccccccccccccccccccccccccccccc
      IF(  (nevtru .LE. 1 .OR. nevtru .EQ. 2000000) 
     $     .AND. wtkarl .GT. 0d0       ) THEN
         CALL dumpl(6,p1,p2,p3,p4,qeff1,qeff2,sphot,nphot)
         CALL dumpw(nout)
      ENDIF
*-- presampler channels monitoring
      IF( keysmp  .EQ.  2 ) CALL pres_monit(0,wtcrud,wtmod,wtset)
!!! temporary, to monitor progress of the accuracy with statistics
      IF(mod(nevtot,200000).EQ.0) THEN
        CALL decay_monit(1,wtmod,xcrude,svar,label_dumm,6)
      ENDIF
* ==================================================================
* ====================postgeneration================================
* ==================================================================
*     *************************
      ELSEIF( mode .EQ. 1) THEN
*     *************************
      CALL karlud(1,xcrude,xcvesk,dumm1)
*-- presampler channels monitoring
      IF( keysmp  .EQ.  2 ) CALL pres_monit(1,wtcrud,wtmod,wtset)
* ccccccccccccccccccccccccccccccccc
* final printouts of tauola
      keywgt = MOD(keytek,10)
      IF( keywgt  .EQ.  0) THEN
         CALL dexay(100,pol)
      ENDIF
* ccccccccccccccccccccccccccccccccc
!---- stuff moved from karlud beg ----
*-- crude xs. no born
      CALL gmonit(1,idyfs+59,wtkacr,erkacr,parm3)
      CALL gmonit(2,idyfs+59,evacc,evneg,evove)
      nevneg = evneg
      nevtot = parm3
      WRITE(nout,bxope)
      WRITE(nout,bxtxt) '         KORALW  final  report '
      WRITE(nout,bxtxt) '               Window A        '
      WRITE(nout,bxtxt) '            WEIGHTED evts.     '
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) ' ccru matrix element means:    '
      WRITE(nout,bxtxt) ' a) Born matrix element for CC03 processes  '
      WRITE(nout,bxtxt) ' b) technical crude m.e. for nc processes or'
      WRITE(nout,bxtxt) '    for keysmp .NE. 0                       '
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) '     xsect with no matrix element   '
      WRITE(nout,bxl1i) nevtot,'total no of events      ','nevtot','a0'
      WRITE(nout,bxl1i) nevneg,'wtcrud < 0 evts         ','nevneg','a1'
      xskr   = xcrude*wtkacr
      erkr   = xskr*erkacr
      WRITE(nout,bxl1g) xcrude,'sigma_crude             ','xcrude','a2'
      WRITE(nout,bxl2g) 
     $           wtkacr,erkacr,'<wtcrud>, rel err       ','wtkacr','a3'
      WRITE(nout,bxl2g)
     $              xskr,erkr,'phsp. vol, no beta-0     ','xskr  ','a4'
      WRITE(nout,bxtxt) ' '

*-- born xsection, total
      CALL gmonit(1,idyfs+58,wtkabo,erkabo,parm3)
      CALL gmonit(2,idyfs+58,evacc,evneg,evove)
      nevneg = evneg
      nevove = evove
      nevtot = parm3
      xskb0  = xcrude*wtkabo
      erkb0  = xskb0*erkabo
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) ' xsect with ccru matrix el. only, no betas'
      WRITE(nout,bxl1i) nevtot,'total no of events      ','nevtot','a5'
      WRITE(nout,bxl1i) nevneg,'wtcrud*wtborn <0 evts   ','nevneg','a6'
      WRITE(nout,bxl2g)
     $           wtkabo,erkabo,'<wtcrud*wtborn>, rel err','wtkabo','a7'
      WRITE(nout,bxl2g)
     $           xskb0,erkb0,  'sigma (born m.el.)      ','xska0','a8'

*-- born xsection from above wtmax
      CALL gmonit(1,idyfs+60,wtkabo,erkabo,parm3)
      xskb   = xcrude*wtkabo
      erkb   = xskb*erkabo
      IF (xskb.NE.0d0) THEN
        xx=xskb/xskb0
        ee=erkb/xskb0
      ELSE
         xx=0d0
         ee=0d0
      ENDIF
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) '     xsect over wtmax_cc03     '
      WRITE(nout,bxtxt) ' ccru matrix el. only, no betas'
      WRITE(nout,bxl1i) nevove,   'evts: wt>wtmax_cc03  ','nevove','a9'
      WRITE(nout,bxl2g) xskb,erkb,'sigma: wt>wtmax_cc03 ','xskabo','a10'
      WRITE(nout,bxl2g) xx,ee,    'relat sigma: wt>wtmax','xskabo','a11'
      WRITE(nout,bxclo)
!---- stuff moved from karludw end ----
      IF( keyisr  .NE.  0 ) THEN
*-- tests of beta functions
        IF(i_beta. EQ. 1) CALL beta_tests(1,idyfs,xcrude,wtkarl,wtset)
      ENDIF
*-- best xsection printout, total and over
      CALL gmonit(1,idyfs+80,averwt,errela,evtot)
      xsbest  = xcrude*averwt
      erbest  = xsbest*errela
      CALL gmonit(2,idyfs+80,evacc,evneg,evove)
      nevacc = evacc
      nevneg = evneg
      nevove = evove
      CALL gmonit(1,idyfs+81,averwn,erreln,evtot)
      xsneg   = averwn/averwt
      erneg   = xsneg*erreln
      CALL gmonit(1,idyfs+82,averwo,errelo,evtot)
      xsove   = averwo/averwt
      erove   = xsove*errelo
      WRITE(nout,bxope)
      WRITE(nout,bxtxt) '         KORALW  final  report '
      WRITE(nout,bxtxt) '               Window C        '
      WRITE(nout,bxtxt) '                               '
      WRITE(nout,bxtxt) '     BEST order total xsect.   '
      WRITE(nout,bxl1i)nevtot,       'total no of events ','nevtot','c1'
      WRITE(nout,bxl1i)nevtru,       'accepted events    ','nevtru','c2'
      WRITE(nout,bxl2g)xsbest,erbest,'sigma_tot [pb]     ','xskabo','c3'
      WRITE(nout,bxl1f)errela,       'relative error     ','errela','c4'
      WRITE(nout,bxl1i)nevneg,       'events: wt<0       ','nevneg','c5'
      WRITE(nout,bxl2g)xsneg,erneg,  'xsec/xtot: wt<0    ','xsneg ','c6'
      WRITE(nout,bxl1i)nevove,       'events: wt>wtmax   ','nevove','c7'
      WRITE(nout,bxl2g)xsove,erove,  'xsec/xtot: wt>wtmax','xsove ','c8'
      WRITE(nout,bxclo)
      IF( key4f  .NE.  0 ) THEN
!-- 4fermion monitoring
         IF(i_4f .EQ. 1) 
     $     CALL f4_tests(mode,idyfs,xcrude,wttot,wtboww,wtbo4f,wt4f)
      ENDIF
! monitoring xsections in different decay channels
      CALL decay_monit(1,wtmod,xcrude,svar,label_dumm,nout)
**********
* on request also the printout for the photonic pre-tabulation
*********      CALL decay_monit(2,wtmod,xcrude,svar,label_dumm,nout)
*-- output parameters
      IF( KeyWgt .EQ. 0 ) xcrude = xsbest
c[[[[[[[[[[ OBSOLETE [[[[[[[[[[[[[[[[[[[[
      xpar_input(20)=xsbest
      xpar_input(21)=erbest
***   npar(10)=nevtru           !!! <--not accsessible outside koralw
***   npar(11)=nevtot           !!! <--not accsessible outside koralw
      xpar_input(1010)=nevtru
      xpar_input(1011)=nevtot
*-- normalization factor for histogramming
      IF( KeyWgt .EQ. 0 ) THEN
        xpar_input(30)=xsbest
      ELSE
        xpar_input(30)=xcrude
      ENDIF
      xpar_input(31)=wtmax
      xpar_input(32)=wtmax_cc03
c]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
*****************************************************************
*****************************************************************
*     **********************
      ELSEIF(mode .EQ. 2) THEN
*     **********************
      CALL karlud(mode,dum1,xsborn,erborn)
*     ****
      ELSE
*     ****
      WRITE(nout,*) '===>KORALW: wrong mode'
      STOP
*     *****
      ENDIF
*     *****
      END

      SUBROUTINE model_4f(wtcrud,wtboww,wtbo4f,wt4f,br,alpha_s,
     $                    effp1,effp2,effp3,effp4,label,key4f,keyacc)
!     ****************************************************************
! external weight connected with 4fermions
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / MATPAR / PI,CEULER    
      SAVE / MATPAR /

      DIMENSION wt4f(9),br(20)
      DIMENSION effp1(4),effp2(4),effp3(4),effp4(4)
      DIMENSION bq1(4),bq2(4),bsp(4)

      DIMENSION ipdg(4)
      CHARACTER*3 chuman(4)

      IF (wtcrud .NE. 0d0) THEN
         DO i=1,4
            bq1(i) = effp1(i)+effp2(i)
            bq2(i) = effp3(i)+effp4(i)
            bsp(i) = bq1(i)+bq2(i)
         ENDDO
         s1 = dmas2(bq1)
         s2 = dmas2(bq2)
         sp = dmas2(bsp)
*-- Born level flux factor 1/2s'
         fluxf = 1d0/(2d0*sp)
*-- WW type final states
        CALL linear_to_WZ_label(1,label,iwm,iwp,if_z,if_w)
        IF( if_w .NE. 0 ) THEN
*--- Internal WW Born matrix element
           wtborn = wwborn(effp1,effp2,effp3,effp4,keyacc)
*-- Coulomb corr.
           cc = CulMC(sp,s1,s2)
           wtborc = wtborn*cc
           wtboww = fluxf*wtborc

           IF (key4f.GT.0) THEN
*---  External 4fermion matrix el.
              CALL ampext(wtmod4f,wt4f )
*---  SM CC03 Born
              IF (keyacc .EQ. 0) THEN
                wtborn_sm = wtborn
              ELSE
                wtborn_sm = wwborn(effp1,effp2,effp3,effp4,0)
              ENDIF
*--- Divide off naive QCD correction from wtborn_sm if included
              CALL NAIVE_QCD(label,br,wt_qcd)
*--- ACC & Coulomb corrections to CC03 Born (divide off naive QCD)
              dif_bc = (wtborc - wtborn_sm)/wt_qcd
!ms tests beg
!              rat_bc = wtborc / wtborn_sm
!ms tests end
*--- 4fermion "improved" Born (naive QCD correction - multiplicative) 
              wtbo4f = (wtmod4f + dif_bc)*wt_qcd*fluxf
!ms tests beg
!              wtbo4f = (wtmod4f*rat_bc)*fluxf
!ms tests end
              DO i4f=1,9
                 wt4f(i4f) = (wt4f(i4f) + dif_bc)*wt_qcd*fluxf
              ENDDO
           ELSE
              wtbo4f = wtboww
              DO i4f=1,9
                 wt4f(i4f)=0d0
              ENDDO
           ENDIF
*-- ZZ type final states (pure 4fermion Born)
         ELSE
           CALL ampext(wtmod4f,wt4f ) 
* alpha_s/pi for naive QCD corrections
           aspi = alpha_s/pi
* Naive QCD correction - multiplicative
           wt_qcd = 1d0
           CALL linear_to_pdg_label(1,label,ipdg,chuman)
           IF (abs(ipdg(1)) .LT. 10) wt_qcd = wt_qcd*(1 + aspi)
           IF (abs(ipdg(3)) .LT. 10) wt_qcd = wt_qcd*(1 + aspi)
* 4fermion "improved" Born 
           wtbo4f = wtmod4f*wt_qcd*fluxf
           wtboww = 0d0
         ENDIF
      ELSE
         wtboww = 0d0
         wtbo4f = 0d0
         DO i4f=1,9
            wt4f(i4f)=0d0
         ENDDO
      ENDIF

      END

      SUBROUTINE beta_tests(mode,idyfs,xcrude,wtkarl,wtset)
!     ***********************************************
! beta functions related tests
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / matpar / pi,ceuler
      COMMON / phypar / alfinv,gpicob
 
      COMMON / bxfmts / bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      CHARACTER*80      bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g

      COMMON / inout  / ninp,nout

      DIMENSION wtset(*)

      IF(mode .EQ. -1) THEN
* Totals O(alf0-alf3)
         DO k=10,14
            CALL gmonit(-1,idyfs+k,0d0,1d0,1d0) ! 10-14
         ENDDO
* Betas O(alf0-alf3) and differences
         DO k=20,38
            CALL gmonit(-1,idyfs+k,0d0,1d0,1d0) ! 20-38
         ENDDO
      ELSEIF(mode .EQ. 0) THEN
*     Totals O(alf0-alf3)
         DO k=1,4
            CALL gmonit(0,idyfs+10+k,wtkarl*wtset(k),1d0,1d0)
         ENDDO
* Betas O(alf0-alf3)
         DO k=10,19
            CALL gmonit(0,idyfs+10+k,wtkarl*wtset(k),1d0,1d0)
         ENDDO
* Differences total O(alf0-alf3)
         CALL gmonit(0,idyfs+30,wtkarl*(wtset(2)-wtset(1)),1d0,1d0)
         CALL gmonit(0,idyfs+31,wtkarl*(wtset(3)-wtset(2)),1d0,1d0)
         CALL gmonit(0,idyfs+32,wtkarl*(wtset(4)-wtset(3)),1d0,1d0)
* bt01-bt00, bt10
         CALL gmonit(0,idyfs+33,wtkarl*(wtset(11)-wtset(10)),1d0,1d0)
* bt02-bt01, bt11-bt10, bt20
         CALL gmonit(0,idyfs+34,wtkarl*(wtset(13)-wtset(11)),1d0,1d0)
         CALL gmonit(0,idyfs+35,wtkarl*(wtset(14)-wtset(12)),1d0,1d0)
* bt03-bt02, bt12-bt11, bt21-bt20,bt30
         CALL gmonit(0,idyfs+36,wtkarl*(wtset(16)-wtset(13)),1d0,1d0)
         CALL gmonit(0,idyfs+37,wtkarl*(wtset(17)-wtset(14)),1d0,1d0)
         CALL gmonit(0,idyfs+38,wtkarl*(wtset(18)-wtset(15)),1d0,1d0)
      ELSEIF(mode .EQ. 1) THEN
        WRITE(nout,bxope)
        WRITE(nout,bxtxt) '         KORALW  final  report '
        WRITE(nout,bxtxt) '               Window B        '
        WRITE(nout,bxtxt) '                               '
*****************************************************************
*****************************************************************
        CALL gmonit(1,idyfs+11,averwt,errela,evtot)
        xstot0   = xcrude*averwt
        ertot0   = xstot0*errela
        WRITE(nout,bxl2f) xstot0,ertot0,'xsec total    ','O(alf0)','b3'
*****************************************************************
        CALL gmonit(1,idyfs+12,averwt,errela,evtot)
        xstot1   = xcrude*averwt
        ertot1   = xstot1*errela
        WRITE(nout,bxl2f) xstot1,ertot1,'xsec total    ','O(alf1)','b4'
*****************************************************************
        CALL gmonit(1,idyfs+13,averwt,errela,evtot)
        xstot2   = xcrude*averwt
        ertot2   = xstot2*errela
        WRITE(nout,bxl2f) xstot2,ertot2,'xsec total    ','O(alf2)','b5'
*****************************************************************
        CALL gmonit(1,idyfs+14,averwt,errela,evtot)
        xstot3   = xcrude*averwt
        ertot3   = xstot3*errela
        WRITE(nout,bxl2f) xstot3,ertot3,'xsec total    ','O(alf3)','b6'
*****************************************************************
        CALL gmonit(2,idyfs+11,evacc1,evneg1,evove1)
        CALL gmonit(2,idyfs+12,evacc2,evneg2,evove2)
        CALL gmonit(2,idyfs+13,evacc3,evneg3,evove3)
        CALL gmonit(2,idyfs+14,evacc4,evneg4,evove4)
        neg0=evneg1
        neg1=evneg2
        neg2=evneg3
        neg3=evneg4
        WRITE(nout,bxl1i) neg0,         'wt<0  events  ','O(alf0)',' '
        WRITE(nout,bxl1i) neg1,         'wt<0  events  ','O(alf1)',' '
        WRITE(nout,bxl1i) neg2,         'wt<0  events  ','O(alf2)',' '
        WRITE(nout,bxl1i) neg2,         'wt<0  events  ','O(alf3)',' '
*****************************************************************
        CALL gmonit(1,idyfs+20,averwt,errela,evtot)
        xsbt00   = xcrude*averwt
        erbt00   = xsbt00*errela
        WRITE(nout,bxl2f) xsbt00,erbt00,'xsec(beta00)  ','O(alf0)','b7'
*****************************************************************
        CALL gmonit(1,idyfs+21,averwt,errela,evtot)
        xsbt01   = xcrude*averwt
        erbt01   = xsbt01*errela
        WRITE(nout,bxl2f) xsbt01,erbt01,'xsec(beta01)  ','O(alf1)','b8'
*****************************************************************
        CALL gmonit(1,idyfs+22,averwt,errela,evtot)
        xsbt10   = xcrude*averwt
        erbt10   = xsbt10*errela
        WRITE(nout,bxl2f) xsbt10,erbt10,'xsec(beta10)  ','O(alf1)','b9'
*****************************************************************
        CALL gmonit(1,idyfs+23,averwt,errela,evtot)
        xsbt02   = xcrude*averwt
        erbt02   = xsbt02*errela
        WRITE(nout,bxl2f) xsbt02,erbt02,'xsec(beta02)  ','O(alf2)','b10'
*****************************************************************
        CALL gmonit(1,idyfs+24,averwt,errela,evtot)
        xsbt11   = xcrude*averwt
        erbt11   = xsbt11*errela
        WRITE(nout,bxl2f) xsbt11,erbt11,'xsec(beta11)  ','O(alf2)','b11'
*****************************************************************
        CALL gmonit(1,idyfs+25,averwt,errela,evtot)
        xsbt20   = xcrude*averwt
        erbt20   = xsbt20*errela
        WRITE(nout,bxl2f) xsbt20,erbt20,'xsec(beta20)  ','O(alf2)','b12'
*****************************************************************
*****************************************************************
        CALL gmonit(1,idyfs+26,averwt,errela,evtot)
        xsbt03   = xcrude*averwt
        erbt03   = xsbt20*errela
        WRITE(nout,bxl2f) xsbt03,erbt03,'xsec(beta03)  ','O(alf3)','b13'
*****************************************************************
        CALL gmonit(1,idyfs+27,averwt,errela,evtot)
        xsbt12   = xcrude*averwt
        erbt12   = xsbt20*errela
        WRITE(nout,bxl2f) xsbt12,erbt12,'xsec(beta12)  ','O(alf3)','b14'
*****************************************************************
        CALL gmonit(1,idyfs+28,averwt,errela,evtot)
        xsbt21   = xcrude*averwt
        erbt21   = xsbt20*errela
        WRITE(nout,bxl2f) xsbt21,erbt21,'xsec(beta21)  ','O(alf3)','b15'
*****************************************************************
        CALL gmonit(1,idyfs+29,averwt,errela,evtot)
        xsbt30   = xcrude*averwt
        erbt30   = xsbt20*errela
        WRITE(nout,bxl2f) xsbt30,erbt30,'xsec(beta30)  ','O(alf3)','b16'
*****************************************************************
        WRITE(nout,bxtxt) ' xsec_tot differences '
*****************************************************************
        CALL gmonit(1,idyfs+30,averwt,errela,evtot)
        xsdel1   = xcrude*averwt
        erdel1   = xsdel1*errela
        CALL gmonit(1,idyfs+31,averwt,errela,evtot)
        xsdel2   = xcrude*averwt
        erdel2   = xsdel2*errela
        CALL gmonit(1,idyfs+32,averwt,errela,evtot)
        xsdel3   = xcrude*averwt
        erdel3   = xsdel3*errela
        WRITE(nout,bxl2f) xsdel1,erdel1,'xstot(alf1-0)','O(alf1)','b17'
        WRITE(nout,bxl2f) xsdel2,erdel2,'xstot(alf2-1)','O(alf2)','b18'
        WRITE(nout,bxl2f) xsdel3,erdel3,'xstot(alf3-2)','O(alf3)','b19'
*****************************************************************
        WRITE(nout,bxtxt) ' betas differences '
*****************************************************************
        CALL gmonit(1,idyfs+33,averwt,errela,evtot)
        xsdt01   = xcrude*averwt
        erdt01   = xsdt01*errela
        WRITE(nout,bxl2f) xsdt01,erdt01,'xs(beta01-00)','O(alf1)','b20'
        WRITE(nout,bxl2f) xsbt10,erbt10,'xs(beta10)   ','O(alf1)','b21'
*****************************************************************
        CALL gmonit(1,idyfs+34,averwt,errela,evtot)
        xsdt02   = xcrude*averwt
        erdt02   = xsdt02*errela
        CALL gmonit(1,idyfs+35,averwt,errela,evtot)
        xsdt11   = xcrude*averwt
        erdt11   = xsdt11*errela
        WRITE(nout,bxl2f) xsdt02,erdt02,'xs(beta02-01)','O(alf2)','b19'
        WRITE(nout,bxl2f) xsdt11,erdt11,'xs(beta11-10)','O(alf2)','b20'
        WRITE(nout,bxl2f) xsbt20,erbt20,'xs(beta20)   ','O(alf2)','b21'
*****************************************************************
        CALL gmonit(1,idyfs+36,averwt,errela,evtot)
        xsdt03   = xcrude*averwt
        erdt03   = xsdt03*errela
        CALL gmonit(1,idyfs+37,averwt,errela,evtot)
        xsdt12   = xcrude*averwt
        erdt12   = xsdt12*errela
        CALL gmonit(1,idyfs+38,averwt,errela,evtot)
        xsdt21   = xcrude*averwt
        erdt21   = xsdt21*errela
        WRITE(nout,bxl2f) xsdt03,erdt03,'xs(beta03-02)','O(alf3)','b22'
        WRITE(nout,bxl2f) xsdt12,erdt12,'xs(beta12-11)','O(alf3)','b23'
        WRITE(nout,bxl2f) xsdt21,erdt21,'xs(beta21-20)','O(alf3)','b24'
        WRITE(nout,bxl2f) xsbt30,erbt30,'xs(beta30)   ','O(alf3)','b25'
*****************************************************************
        WRITE(nout,bxclo)
      ENDIF
      END

      SUBROUTINE f4_tests(mode,idyfs,xcrude,wttot,wtboww,wtbo4f,wt4f)
!     ***************************************************************
! 4fermion related tests
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / matpar / pi,ceuler
      COMMON / phypar / alfinv,gpicob
 
      COMMON / bxfmts / bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g
      CHARACTER*80      bxope,bxclo,bxtxt,bxl1i,bxl1f,bxl2f,bxl1g,bxl2g

      COMMON / inout  / ninp,nout

      DIMENSION wt4f(*)

      IF(mode .EQ. -1) THEN
*-- all 4-fermion
        DO k=91,96
          CALL gmonit(-1,idyfs+k,0d0,1d0,1d0) ! 91-96
        ENDDO
      ELSEIF(mode .EQ. 0) THEN
*-- 4-fermion monitoring
         IF (ABS(wtbo4f).GT.1d-50) THEN
           wttww = wtboww/wtbo4f*wttot
         ELSE
           wttww = 0d0
         ENDIF
         CALL gmonit(0,idyfs+91,wtbo4f,1d0,0d0)
         CALL gmonit(0,idyfs+92,wttot,1d0,0d0)
         CALL gmonit(0,idyfs+93,wttww,1d0,0d0)
         CALL gmonit(0,idyfs+94,wt4f(1),1d0,0d0)
         CALL gmonit(0,idyfs+95,wt4f(2),1d0,0d0)
         CALL gmonit(0,idyfs+96,wttot-wttww,1d0,0d0)
*-- END 4-fermion monitoring
      ELSEIF(mode .EQ. 1) THEN
*-- 4-fermion monitoring
        WRITE(nout,bxope)
        WRITE(nout,bxtxt) '         KORALW  final  report '
        WRITE(nout,bxtxt) '               Window D        '
        WRITE(nout,bxtxt) '                               '
        WRITE(nout,bxtxt) '     Complete 4-fermion process'
        WRITE(nout,bxtxt) '                               '
        WRITE(nout,bxtxt) '   I. Best ord. W-pair total xsect.    '
*****************************************************************
        CALL gmonit(1,idyfs+93,averwt,errela,evtot)
        WRITE(nout,bxl2g)
     $           averwt,errela,      '<wttww>: WW weight ','averwt','d1'
        xskbs   = xcrude*averwt
        erkbs   = xskbs*errela
        WRITE(nout,bxl2g)xskbs,erkbs,'sigma_WW, best [pb]','xskabo','d2'
        WRITE(nout,bxtxt) '                               '
        WRITE(nout,bxtxt) '   II. Best ord. 4-fermion total xsect.'
*****************************************************************
        CALL gmonit(1,idyfs+91,averwt,errela,evtot)
        WRITE(nout,bxl2g)
     $          averwt,errela,       '<wtbo4f>, rel err  ','averwt','d3'
*****************************************************************
        CALL gmonit(1,idyfs+92,averwt,errela,evtot)
        WRITE(nout,bxl2g)
     $          averwt,errela,       '<wttot>,rel err    ','averwt','d4'
        xskbb   = xcrude*averwt
        erkbb   = xskbb*errela
        WRITE(nout,bxl2g)xskbb,erkbb,'sigma_4f, best [pb]','xskabo','d5'
        stob = 1- xskbs/xskbb
        stober = dsqrt( (erkbs*xskbb)**2 +(erkbb*xskbs)**2 ) / xskbb**2
        WRITE(nout,bxl2g)stob,stober,'sigma 1-Wpair/4ferm','1-d2/5','d6'
*****************************************************************
        CALL gmonit(1,idyfs+96,averwt,errela,evtot)
        xskbd   = xcrude*averwt
        erkbd   = xskbd*errela
        stob = xskbd/xskbb
        stober = dsqrt( (erkbd*xskbb)**2 +(erkbb*xskbd)**2 ) / xskbb**2
        WRITE(nout,bxl2g)stob,stober,'sigma 1-Wpair/4ferm','wtbgr ','d7'
        WRITE(nout,bxclo)
      ENDIF
*****************************************************************
      END


      SUBROUTINE NAIVE_QCD(label,br,wt_qcd)
!******************************************
! This routine adds the naive QCD correction to the external matrix el.
! It is done only for the WW final states, and is justified for CC03.
! Also the effective nontrivial CKM is introduced on request.
! Everything is done based solely on the deviation of branching ratios
! from the 1/3, 1/9 settings.

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION br(20),br0(20)
      DATA init /0/
      SAVE
 
      IF(init .EQ. 0) THEN
         init=1

         br0(1)=(1d0/3d0) !  <== ud
         br0(2)=0d0       !  <== cd
         br0(3)=0d0       !  <== us
         br0(4)=(1d0/3d0) !  <== cs
         br0(5)=0d0       !  <== ub
         br0(6)=0d0       !  <== cb
         br0(7)=(1d0/9d0) !  <== e
         br0(8)=(1d0/9d0) !  <== mu
         br0(9)=(1d0/9d0) !  <== tau
      ENDIF
! do not redefine off-diagonal states, they are corrected already in
! Born
! off-diagonal are also SINGLE off-diag.  m.s. 3/13/98
         
      CALL linear_to_WZ_label(1,label,iwm,iwp,if_z,if_w)
      IF( if_w .NE. 0 ) THEN
         IF(br0(iwm) .NE. 0d0 .AND. br0(iwp) .NE. 0d0) THEN
           wm = (br(iwm)/br(7)) / (br0(iwm)/br0(7))
           wp = (br(iwp)/br(7)) / (br0(iwp)/br0(7))
         ELSE
           wm = 1d0
           wp = 1d0
         ENDIF
         wt_q= wm*wp
      ELSE
         wt_q=1d0
      ENDIF
           
      wt_qcd=wt_q
          
      END
      subroutine mm_brancher(sprim,itype,prob)
!     ******************************************************    
! ###########################################
! sets probablilities of channels, can be   #
! function of sprim and iflav(4)            #
! ###########################################
      implicit DOUBLE PRECISION (a-h,o-z)

      PARAMETER (mm_nrchan=65)
      DIMENSION prob(mm_nrchan)

      SAVE

! common / WorZ / is replaced by  store_label  routine !!!!!!
! convert linear labels to KoralW.13x convention (mode=1)
! these routines comes from the decay.f package !!! 
      CALL store_label(1,label)
      CALL linear_to_WZ_label(1,label,icwm,icwp,ifznow,ifwnow)

        IF(ifwnow.EQ.1) THEN
          CALL mm_brancher_WW(sprim,itype,prob)
        ELSE
          CALL mm_brancher_ZZ(sprim,itype,prob)
        ENDIF
      END


      subroutine mm_brancher_WW(sprim,itype,prob)
!     ******************************************************    
! ###########################################
! sets probablilities of channels, can be   #
! function of sprim and iflav(4)            #
! ###########################################
      implicit DOUBLE PRECISION (a-h,o-z)
      COMMON / DECAYS / IFLAV(4), AMDEC(4) 
      COMMON / INOUT  / NINP,NOUT 
      COMMON / BXFMTS / BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      CHARACTER*80      BX11F
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      save / DECAYS /,/ INOUT  /,/ BXFMTS / 
      DIMENSION drvec(1)
      parameter (mm_nrchan=65)
      DIMENSION PROB(mm_nrchan)
      data istart /0/
      save
      BX11F =  '(1X,1H*,F17.8,               16X, A20,I12,A7, 1X,1H*)' 
! random choice of generation branch as function of sprime, and final state chosen.
! should be emptied from rn. generation to preserve generation series. 
! you can adopt your choice of the presampling type on sprim and iflav.
! may be one should coonect it with placer? For choice of presampling resonances?
        KeySmp = MOD(KeyTek,1000)/100
        IF (KeySmp.eq.0) THEN
          prob(1)= 1d0
          do i=2,mm_nrchan
            prob(i)=0d0
          enddo
        ELSE
          do i=1,7
            prob(i)=0d0
          enddo
ccc          prob(1)=1d-5
          do i=8,mm_nrchan
ccc            prob(i)=1.0d-5
            prob(i)=0d0
ccc            prob(i)=.02d0
          enddo
          prob(62)=0d0
          prob(63)=0d0
          prob(64)=0d0
          prob(65)=0d0


          IF(abs(iflav(1)).NE.11 .AND. abs(iflav(2)).NE.11 .AND. 
     @       abs(iflav(3)).NE.11 .AND. abs(iflav(4)).NE.11 ) THEN 
!  1-1 type channels
            prob(  1 )= 0.5d0  
            prob( 10 )= 0.1d0  
            prob( 13 )= 0.26d0
            prob( 14 )= 0.4d-1
            prob( 28 )= 0.2d-1
            prob( 38 )= 0.3d-1 !fsr .3d-1
            prob( 42 )= 0.2d-1
            prob( 45 )= 0.3d-1 !fsr .3d-1
            prob( 49 )= 0.2d-1
            prob( 50 )= 0.2d-1
            prob( 54 )= 0.2d-1
            prob( 61 )= 0.2d-1
          ELSEIF(abs(iflav(1)).EQ.11 .AND. abs(iflav(2)).NE.11 .AND. 
     @           abs(iflav(3)).NE.11 .AND. abs(iflav(4)).NE.11 ) THEN 
!  7-1 type channels (e,nu u,d)
            prob(  1 )= .6d0
            prob(  8 )= .1d0 !
            prob(  9 )= .3d-1 ! it was 46
            prob( 17 )= .2d0 
            prob( 18 )= .5d-1 !
            prob( 25 )= .1d0 
            prob( 37 )= .3d-1 !
            prob( 38 )= .3d-1 
            prob( 41 )= .3d-1 
            prob( 43 )= .2d0 !
            prob( 55 )= .1d0 
            prob( 58 )= .5d-1 
          ELSEIF(abs(iflav(1)).NE.11 .AND. abs(iflav(2)).NE.11 .AND. 
     @           abs(iflav(3)).NE.11 .AND. abs(iflav(4)).EQ.11 ) THEN 
! 1-7 type channels (u,d e,nu) 
            prob(  1 )= .6d0
            prob( 11 )= .1d0  !8
                              !9
            prob( 34 )= .2d0  !17
            prob( 33 )= .5d-1 !18
            prob( 26 )= .1d0  !25
            prob( 14 )= .3d-1 !37
                              !38
                              !41
            prob( 56 )= .2d0  !43
            prob( 44 )= .1d0  !55
                              !58
          ELSEIF(abs(iflav(1)).EQ.11 .AND. abs(iflav(2)).NE.11 .AND. 
     @           abs(iflav(3)).NE.11 .AND. abs(iflav(4)).EQ.11 ) THEN 
! 7-7 type channels (e,nu e,nu)


            prob(  1 )= .30d0
            prob( 10 )= 0.2d0  
            prob( 13 )= 0.2d0
            prob( 17 )= .5d-1 
            prob( 18 )= .8d-1 !
            prob( 33 )= .3d-1 !18
            prob( 34 )= .1d0  !17
            prob( 38 )= 0.1d0 !fsr .3d-1
            prob( 42 )= .1d0 !
            prob( 43 )= .1d0 !
!            prob( 44 )= .5d-1  !55
!            prob( 50 )= .6d-1 
            prob( 54 )= .1d0 
            prob( 56 )= .1d0  !43
            prob( 57 )= .6d-1 
            prob( 61 )= .1d0

       prob( 1)= .1528066401360303 !max= 5.126895446892595E-02
       prob( 10)= .123819037133896 !max= .4567310012216404
       prob( 13)= .13 !max= .4700347361740743
       prob( 17)= 2.401704334716281E-02 !max= .2876706027685168
       prob( 18)= 2.919633584784117E-02 !max= .1806467520754505
       prob( 33)= 2.875826008779435E-02 !max= .1422278918002859
       prob( 34)= 3.956069899541728E-02 !max= .3848683236527629
       prob( 38)= 5.530102479893887E-02 !max= .1747483992128647
       prob( 42)= 2.878073109354603E-02 !max= .3837148649525202
       prob( 43)= 6.778407127531376E-02 !max= 6.724114199142956E-02
       prob( 54)= 7.187468560675748E-02 !max= .7073283431479492
       prob( 56)= 8.208749745836423E-02 !max= .1134218362601451
       prob( 57)= 3.559681491376532E-02 !max= .1169604669046596
       prob( 61)= .175781241844806 !max= 1.0
          ELSE
            WRITE(6,*)'mm_brancher_WW=>unexpected iflav:',iflav
            STOP
          ENDIF
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!! danger !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!! danger !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          prob(1)=1.0d-5
!          do i=8,mm_nrchan
!            prob(i)=1.0d-5
!          enddo
!          prob(62)=0d0
!          prob(63)=0d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!! danger !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!! danger !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        prtot=0d0
        DO I=1,mm_nrchan
          prtot=prtot+prob(I)
        ENDDO
        DO I=1,mm_nrchan  
          prob(I)=prob(I)/prtot
        ENDDO
!     ---------------------
      IF (istart.eq.0) THEN
!     ---------------------
        istart=1
        WRITE(NOUT,BXOPE) 
        WRITE(NOUT,BXTXT) '             Window X_WW                '
        WRITE(NOUT,BXTXT) '          mm_brancher_WW report         '
        if (KeySmp.eq.0) then
        WRITE(NOUT,BXTXT) ' WARNING: KeySmp =0 Brancher is off !   '
        else
        WRITE(NOUT,BXTXT) '          mm_brancher_WW  is on         '
        DO I=1,mm_nrchan  
          WRITE(NOUT,BX11F) prob(I),'prob. for branch NR: ',I,'X1'
        ENDDO
        endif
        WRITE(NOUT,BXCLO)         
!     -----
      ENDIF
!     -----

      if(Keysmp.eq.0) then
!     ====================
        itype=1
      else
!     ====================
        CALL varran(drvec,1)
        PROBI=0D0
        DO I=1,mm_nrchan
          PROBI=PROBI+PROB(I)
          if(drvec(1).lt.probI) THEN
            itype=I
            GOTO 10
           ENDIF
        enddo
        write(6,*) 
     $ 'mm_brancher_WW has problem prob=',prtot
        stop
 10     continue
      endif
!     =====================
      end 


      subroutine mm_brancher_ZZ(sprim,itype,prob)
!     ******************************************************    
! ###########################################
! sets probablilities of channels, can be   #
! function of sprim and iflav(4)            #
! ###########################################
      implicit DOUBLE PRECISION (a-h,o-z)
      COMMON / DECAYS / IFLAV(4), AMDEC(4) 
      COMMON / INOUT  / NINP,NOUT 
      COMMON / BXFMTS / BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      CHARACTER*80      BX11F
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      save / DECAYS /,/ INOUT  /,/ BXFMTS /
      DIMENSION drvec(1)
      parameter (mm_nrchan=65)
      DIMENSION PROB(mm_nrchan)
      data istart /0/
      save
      BX11F =  '(1X,1H*,F17.8,               16X, A20,I12,A7, 1X,1H*)' 
! random choice of generation branch as function of sprime, and final state chosen.
! should be emptied from rn. generation to preserve generation series. 
! you can adopt your choice of the presampling type on sprim and iflav.
! may be one should coonect it with placer? For choice of presampling resonances?
        KeySmp = MOD(KeyTek,1000)/100
        IF (KeySmp.eq.0) THEN
          prob(1)= 1d0
          do i=2,mm_nrchan
            prob(i)=0d0
          enddo
        ELSE
          do i=1,7
            prob(i)=0d0
          enddo
ccc          prob(1)=1d-5
          do i=8,mm_nrchan
            prob(i)=0d0
ccc            prob(i)=1.0d-5
ccc            prob(i)=.02d0
          enddo
          prob(62)=0d0
          prob(63)=0d0
          prob(64)=0d0
          prob(65)=0d0


          IF(abs(iflav(1)).NE.11 .AND. abs(iflav(2)).NE.11 .AND. 
     @       abs(iflav(3)).NE.11 .AND. abs(iflav(4)).NE.11 .AND. 
     @       abs(iflav(1)).NE.abs(iflav(4)) ) THEN 
!  3-1 type channels(c,c,u,u)
       prob( 1 )= 0.105942985204660847  !max= 0.422468563492594429
       prob( 8 )= 0.104089346949555211  !max= 0.961147585632993628
       prob( 11 )= 0.195082323650622030  !max= 1.00000000000000000

       prob( 14 )= 0.248228255805263420E-01  !max= 0.101387425860385891
       prob( 17 )= 0.271209922326381137E-01  !max= 0.208362394638424042
       prob( 22 )= 0.30E-01 
       prob( 26 )= 0.247906173207529008E-01  !max= 0.128756761020349664
       prob( 29 )= 0.303159280499325189E-01  !max= 0.389546245736929753
       prob( 34 )= 0.27E-01  
       prob( 37 )= 0.228127925024490461E-01  !max= 0.157216473763183984
       prob( 40 )= 0.208963525281628468E-01  !max= 0.983818184116975469E-01
       prob( 43 )= 0.551212725876649504E-01  !max= 0.333970332517378732
       prob( 44 )= 0.482877207213861098E-01  !max= 0.200693014877614762
       prob( 47 )= 0.781729255217893798E-01  !max= 0.573873279622014842
       prob( 52 )= 0.235323459040677439E-01  !max= 0.134825656145201422
       prob( 55 )= 0.294615817248881640E-01  !max= 0.304665728733879870
       prob( 56 )= 0.436209697611461625E-01  !max= 0.173587530792248423
       prob( 59 )= 0.409452531335373071E-01  !max= 0.925181790377090085E-01

          ELSEIF(abs(iflav(1)).NE.11 .AND. abs(iflav(2)).NE.11 .AND. 
     @       abs(iflav(3)).NE.11 .AND. abs(iflav(4)).NE.11 ) THEN 
!  1-1 type channels(u,u,u,u)
            prob(  1 )= 0.1d0  
            prob( 10 )= 0.1d0  
            prob( 13 )= 0.26d0
            prob( 14 )= 0.4d-1
            prob( 28 )= 0.2d-1
            prob( 38 )= 0.3d-1 !fsr .3d-1
            prob( 42 )= 0.2d-1
            prob( 45 )= 0.3d-1 !fsr .3d-1
            prob( 49 )= 0.2d-1
            prob( 50 )= 0.2d-1
            prob( 54 )= 0.2d-1
            prob( 61 )= 0.2d-1

            prob(  8 )= 0.2d0
            prob( 11 )= 0.2d0
            prob( 29 )= 0.2d-1
            prob( 59 )= 0.1d0
          ELSEIF(abs(iflav(1)).EQ.11 .AND. abs(iflav(2)).EQ.11 .AND. 
     @           (abs(iflav(3)).EQ.14 .OR. abs(iflav(3)).EQ.16) ) THEN 
!  6-10 type channels (e,e,nm,nm)
!!            prob(  1 )= .6d0
            prob(  8 )= .1d0 !
            prob(  9 )= .3d-1 ! it was 46
            prob( 17 )= .2d0 
            prob( 18 )= .5d-1 !
            prob( 25 )= .1d0 
            prob( 37 )= .5d-1 !
            prob( 38 )= .3d-1 
            prob( 41 )= .3d-1 
            prob( 43 )= .2d0 !
            prob( 55 )= .1d0 
            prob( 58 )= .5d-1 

            prob( 10 )= 1d-1 !
            prob( 11 )= .1d0 !
            prob( 20 )= .5d-1 !
            prob( 21 )= .5d-1 !
            prob( 22 )= .5d-1 !
            prob( 32 )= .5d-1 !
            prob( 33 )= .5d-1 !
            prob( 34 )= .5d-1 !
            prob( 35 )= .5d-1 !
            prob( 36 )= .5d-1 !
            prob( 40 )= .1d0 !
            prob( 42 )= .5d-1 !
            prob( 44 )= .5d-1 !
            prob( 45 )= .5d-1 !
            prob( 50 )= .5d-1 !
            prob( 52 )= .5d-1 !
            prob( 56 )= .5d-1 !
            prob( 57 )= .5d-1 !
            prob( 59 )= .5d-1 !
            prob( 60 )= .5d-1 !

          ELSEIF( (abs(iflav(1)).EQ.11 .AND. abs(iflav(2)).EQ.11 .AND.
     @           abs(iflav(3)).NE.11 .AND. abs(iflav(4)).NE.11 )
     @           .OR.
     @            (abs(iflav(1)).EQ.11 .AND. abs(iflav(2)).EQ.11 .AND. 
     @           abs(iflav(3)).EQ.11 .AND. abs(iflav(4)).EQ.11 )
     @          ) THEN 
!  6-1 type channels (e,e,u,u)
!  6-6 type channels (e,e,e,e)

! jacob. optym.
            prob(  8 )= .5d-1 !c
            prob(  9 )= .3d-1 ! it was 46
            prob( 11 )= .5d-1 !c
            prob( 17 )= .5d-1 !c
            prob( 18 )= .5d-1 !
            prob( 25 )= .2d0 !c
            prob( 37 )= .5d-1 !
            prob( 38 )= .3d-1 
            prob( 41 )= .3d-1 
            prob( 43 )= .5d-1 !c
            prob( 55 )= .5d-1 !c 
            prob( 58 )= .5d-1 

            prob( 20 )= .5d-1 !
            prob( 21 )= .5d-1 !
            prob( 22 )= 1d-1 !c
            prob( 33 )= .5d-1 !
            prob( 34 )= 1.5d-1 !c
            prob( 36 )= 1.5d-1 !c
            prob( 40 )= 3.5d-1 !c
            prob( 42 )= .5d-1 !
            prob( 45 )= .5d-1 !
            prob( 50 )= .5d-1 !
            prob( 52 )= .5d-1 !
            prob( 57 )= .5d-1 !
            prob( 60 )= .5d-1 !

            prob( 10 )= 1d-1 !rev dodac
            prob( 23 )= .5d-1 !rev dodac
            prob( 35 )= 1d-1 !rev dodac
            prob( 46 )= .5d-1 !rev dodac
            prob( 49 )= .5d-1 !rev dodac

          ELSEIF( (abs(iflav(1)).EQ.14 .OR. abs(iflav(1)).EQ.16) .AND. 
     @           abs(iflav(3)).EQ.11 .AND. abs(iflav(4)).EQ.11 ) THEN 
! 10-6 type channels (nm,nm e,e) 
            prob( 11 )= .1d0 !
            prob( 12 )= .3d-1 ! it was 46
            prob( 34 )= .2d0 
            prob( 33 )= .5d-1 !
            prob( 26 )= .1d0 
            prob( 14 )= .5d-1 !
            prob( 61 )= .3d-1 
            prob( 58 )= .3d-1 
            prob( 56 )= .2d0 !
            prob( 44 )= .1d0 
            prob( 41 )= .5d-1 

            prob( 13 )= .1d0 !eenn
            prob( 8 )= .1d0 !eenn
            prob( 31 )= .5d-1 !
            prob( 30 )= .5d-1 !
            prob( 29 )= .5d-1 !
            prob( 19 )= .5d-1 !
            prob( 18 )= .5d-1 !
            prob( 17 )= .5d-1 !
            prob( 16 )= .5d-1 !
            prob( 15 )= .5d-1 !
            prob( 59 )= .1d0 !eenn
            prob( 57 )= .5d-1 !
            prob( 55 )= .5d-1 !eenn
            prob( 54 )= .5d-1 !
            prob( 49 )= .5d-1 !
            prob( 47 )= .5d-1 !
            prob( 43 )= .5d-1 !
            prob( 42 )= .5d-1 !
            prob( 40 )= .5d-1 !
            prob( 39 )= .5d-1 !

          ELSEIF(abs(iflav(1)).NE.11 .AND. abs(iflav(2)).NE.11 .AND. 
     @           abs(iflav(3)).EQ.11 .AND. abs(iflav(4)).EQ.11 ) THEN 
! 1-6 type channels (u,u e,e) 

! jacob. optym.
            prob( 11 )= .5d-1 !c
            prob( 12 )= .3d-1 ! it was 46
            prob(  8 )= .5d-1 !c
            prob( 34 )= .5d-1 !c
            prob( 33 )= .5d-1 !
            prob( 26 )= .2d0 !c
            prob( 14 )= .5d-1 !
            prob( 61 )= .3d-1 
            prob( 58 )= .3d-1 
            prob( 56 )= .5d-1 !c
            prob( 44 )= .5d-1 !c 
            prob( 41 )= .5d-1 

            prob( 31 )= .5d-1 !
            prob( 30 )= .5d-1 !
            prob( 29 )= 1d-1 !c
            prob( 18 )= .5d-1 !
            prob( 17 )= 1.5d-1 !c
            prob( 15 )= 1.5d-1 !c
            prob( 59 )= 3.5d-1 !c
            prob( 57 )= .5d-1 !
            prob( 54 )= .5d-1 !
            prob( 49 )= .5d-1 !
            prob( 47 )= .5d-1 !
            prob( 42 )= .5d-1 !
            prob( 39 )= .5d-1 !

            prob( 13 )= 1d-1 !rev dodac
            prob( 28 )= .5d-1 !rev dodac
            prob( 16 )= 1d-1 !rev dodac
            prob( 53 )= .5d-1 !rev dodac
            prob( 50 )= .5d-1 !rev dodac


          ENDIF
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!! danger !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!! danger !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          prob(1)=1.0d-5
!          do i=8,mm_nrchan
!            prob(i)=1.0d-5
!          enddo
!          prob(62)=0d0
!          prob(63)=0d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!! danger !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!! danger !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!! flat distr. for Mariusz begin !!!!!!!!!!!!!!!!!!!!
!          prob(2)=.5d0
!!!!!!!!!!!!! flat distr. for Mariusz end !!!!!!!!!!!!!!!!!!!!




        prtot=0d0
        DO I=1,mm_nrchan
          prtot=prtot+prob(I)
        ENDDO
        DO I=1,mm_nrchan  
          prob(I)=prob(I)/prtot
        ENDDO
!     ---------------------
      IF (istart.eq.0) THEN
!     ---------------------
        istart=1
        WRITE(NOUT,BXOPE) 
        WRITE(NOUT,BXTXT) '             Window X_ZZ                '
        WRITE(NOUT,BXTXT) '          mm_brancher_ZZ report         '
        if (KeySmp.eq.0) then
        WRITE(NOUT,BXTXT) ' WARNING: KeySmp =0 Brancher is off !   '
        else
        WRITE(NOUT,BXTXT) '          mm_brancher_ZZ  is on         '
        DO I=1,mm_nrchan  
          WRITE(NOUT,BX11F) prob(I),'prob. for branch NR: ',I,'X1'
        ENDDO
        WRITE(NOUT,BXL1F) prtot,'total probability ','XX'
        WRITE(NOUT,BXTXT) '                                        '
        endif
        WRITE(NOUT,BXCLO)         
!     -----
      ENDIF
!     -----

      if(Keysmp.eq.0) then
!     ====================
        itype=1
      else
!     ====================
        CALL varran(drvec,1)
        PROBI=0D0
        DO I=1,mm_nrchan
          PROBI=PROBI+PROB(I)
          if(drvec(1).lt.probI) THEN
            itype=I
            GOTO 10
           ENDIF
        enddo
        write(6,*) 
     $ 'mm_brancher_ZZ has problem prob=',prtot
        stop
 10     continue
      endif
!     =====================
      end 
!=======================================================
!====== central library of phase-space routines  =======
!=======================================================
! (Re)Written by: M.Skrzypek        date: 
! Last update:             by:  
!=======================================================
! Notice,
! This is an (almost) COMMON-free environment.
! Please respect it and neither introduce new COMMONs
! nor modify the unfortunate ones that still survived.
!=======================================================
      SUBROUTINE permut_tab(itype,iperm,iflip)
!     **************************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION itable_par(6),itable_ser(24),itable_ser2(24)
      SAVE
      DATA init /0/

      IF(init.EQ.0) THEN
        init = 1
        itable_par(1)=1234
        itable_par(2)=1324
        itable_par(3)=1423

        itable_ser(1)=1234
        itable_ser(2)=1324
        itable_ser(3)=2314

        itable_ser(4)=1243
        itable_ser(5)=1423
        itable_ser(6)=2413

        itable_ser(7)=1342
        itable_ser(8)=1432
        itable_ser(9)=3412

        itable_ser(10)=2341
        itable_ser(11)=2431
        itable_ser(12)=3421

        itable_ser2(1)=1234
        itable_ser2(2)=1243
        itable_ser2(3)=1342

        itable_ser2(4)=2134
        itable_ser2(5)=2143
        itable_ser2(6)=2341

        itable_ser2(7)=3124
        itable_ser2(8)=3142
        itable_ser2(9)=3241

        itable_ser2(10)=4123
        itable_ser2(11)=4132
        itable_ser2(12)=4231

        DO i=1,3
          itable_par(3+i)=itable_par(i)
        ENDDO
        DO i=1,12
!!          itable_ser(12+i)=ifliper(itable_ser(i))
          itable_ser(12+i)=itable_ser(i)
          itable_ser2(12+i)=itable_ser2(i)
        ENDDO
      ENDIF

      iflip = 1

      IF(itype.GE.2 .AND. itype.LE.7) THEN
! extra slots fixed to parallel without permutation (ie 1234)
        iperm=itable_par(1)

      ELSEIF(itype.GE.8 .AND. itype.LE.13) THEN
! 8-10,11-13 parall. perm.
        iperm=itable_par(itype-7)
        IF(itype.GE.11) iflip = -1 

      ELSEIF(itype.GE.14 .AND. itype.LE.37) THEN
! 14-25,26-37 serl.1 perm.
        iperm=itable_ser(itype-13)
        IF(itype.GE.26) iflip = -1 

      ELSEIF(itype.GE.38 .AND. itype.LE.61) THEN
! 38-49,50-61 serl.2 perm.
        iperm=itable_ser2(itype-37)
        IF(itype.GE.50) iflip = -1 
      ENDIF

      END

      FUNCTION ifliper(iperm)
!     ***************************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      SAVE

      it4= mod(iperm,10)
      it3= mod(iperm,100)/10
      it2= mod(iperm,1000)/100
      it1= mod(iperm,10000)/1000

      ifliper = 1000*it4 +100*it3 +10*it2 + it1

      END

      SUBROUTINE permut(mode,iperm,amdec,wkamdec,
     $           bp1,bp2,bp3,bp4,wkbp1,wkbp2,wkbp3,wkbp4)
!     ***************************************************
! mode =  1    amdec -> wkamdec
!              bp*   -> wkbp*
! mode = -1    wkbp* -> bp*
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION bp1(4),bp2(4),bp3(4),bp4(4)
      DIMENSION wkbp1(4),wkbp2(4),wkbp3(4),wkbp4(4)
      DIMENSION amdec(4),wkamdec(4)
      DIMENSION it(4),pmtrx(4,4)
      SAVE

      it(4)= mod(iperm,10)
      it(3)= mod(iperm,100)/10
      it(2)= mod(iperm,1000)/100
      it(1)= mod(iperm,10000)/1000

      IF(mode.EQ.1) THEN
        DO i=1,4
          wkamdec(i)=amdec(it(i))

          pmtrx(1,i)=bp1(i)
          pmtrx(2,i)=bp2(i)
          pmtrx(3,i)=bp3(i)
          pmtrx(4,i)=bp4(i)

          wkbp1(i)=pmtrx(it(1),i)
          wkbp2(i)=pmtrx(it(2),i)
          wkbp3(i)=pmtrx(it(3),i)
          wkbp4(i)=pmtrx(it(4),i)
        ENDDO

c        write(6,*)'permut,mode',iperm,mode
c        write(6,*)bp1(4),bp2(4),bp3(4),bp4(4)
c        write(6,*)wkbp1(4),wkbp2(4),wkbp3(4),wkbp4(4)
c        write(6,*)amdec
c        write(6,*)wkamdec

      ELSEIF(mode.EQ.-1) THEN
        DO i=1,4
          pmtrx(it(1),i)=wkbp1(i)
          pmtrx(it(2),i)=wkbp2(i)
          pmtrx(it(3),i)=wkbp3(i)
          pmtrx(it(4),i)=wkbp4(i)

          bp1(i)=pmtrx(1,i)
          bp2(i)=pmtrx(2,i)
          bp3(i)=pmtrx(3,i)
          bp4(i)=pmtrx(4,i)
        ENDDO
c        write(6,*)'permut,mode',iperm,mode
c        write(6,*)wkbp1(4),wkbp2(4),wkbp3(4),wkbp4(4)
c        write(6,*)bp1(4),bp2(4),bp3(4),bp4(4)
      ENDIF

      END

      SUBROUTINE reader(itypek,qeff1,qeff2,dbp1,dbp2,dbp3,dbp4) 
!     *********************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      DIMENSION dbp1(4),dbp2(4),dbp3(4),dbp4(4),qeff1(4),qeff2(4) 
      read(13,*) itypek
      read(13,*) qeff1
      read(13,*) qeff2

c      read(13,*) dbp1  !original
c      read(13,*) dbp2  !original
c      read(13,*) dbp3  !original
c      read(13,*) dbp4  !original
c
c      read(13,*) dbp4  !conjug. ch.
c      read(13,*) dbp3  !conjug. ch.
c      read(13,*) dbp2  !conjug. ch.
c      read(13,*) dbp1  !conjug. ch.

      read(13,*) dbp1 
      read(13,*) dbp2 
      read(13,*) dbp3 
      read(13,*) dbp4 

      END

      SUBROUTINE writer(nout,itypek,qeff1,qeff2,dbp1,dbp2,dbp3,dbp4)
!     *********************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      DIMENSION dbp1(4),dbp2(4),dbp3(4),dbp4(4),qeff1(4),qeff2(4)
      write(nout,*) itypek
      write(nout,*) qeff1
      write(nout,*) qeff2
      write(nout,*) dbp1
      write(nout,*) dbp2
      write(nout,*) dbp3
      write(nout,*) dbp4
      END

      FUNCTION wlambda(s,s1,s2)
!     ********************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      x1=s1/s
      x2=s2/s
      wlambda=dsqrt( (1-x1-x2)**2 - 4*x1*x2 )
      END

      SUBROUTINE set_eff_beams(sprim,ambeam,qeff1,qeff2)
!     ***************************************************
!-- sets effective beams (CMS')
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION qeff1(4),qeff2(4)
      SAVE
        qeff1(1)=0d0
        qeff1(2)=0d0
        qeff1(3)=dsqrt(sprim/4d0  - ambeam**2)
        qeff1(4)=dsqrt(sprim/4d0)

        qeff2(1)=0d0
        qeff2(2)=0d0
        qeff2(3)=-dsqrt(sprim/4d0  - ambeam**2)
        qeff2(4)=dsqrt(sprim/4d0)

      END

      SUBROUTINE set_param(prob_ang,prob_mass,rmas,rgam,
     $                    amreg2_mas,amreg2_ang,itype,i_flav)
!     *************************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      SAVE
      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      DIMENSION i_flav(4)

! common / WorZ / is replaced by  store_label  routine !!!!!!
! convert linear labels to KoralW.13x convention (mode=1)
! these routines comes from the decay.f package !!! 
      CALL store_label(1,label)
      CALL linear_to_WZ_label(1,label,icwm,icwp,ifznow,ifwnow)

        IF(ifwnow.EQ.1) THEN
          CALL set_param_WW(prob_ang,prob_mass,rmas,rgam,
     $                      amreg2_mas,amreg2_ang,itype,i_flav)
        ELSE
          CALL set_param_ZZ(prob_ang,prob_mass,rmas,rgam,
     $                      amreg2_mas,amreg2_ang,itype,i_flav)
        ENDIF

      END

      FUNCTION flip_flop(itype)
!     **************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IF(itype.LT.8) THEN
        itpe_loc = itype
      ELSEIF(itype.GE.8 .AND. itype.LE.10) THEN
        itpe_loc = itype+3
      ELSEIF(itype.GE.11 .AND. itype.LE.13) THEN
        itpe_loc = itype-3
      ELSEIF(itype.GE.14 .AND. itype.LE.25) THEN
        itpe_loc = 37 -(itype-14)
      ELSEIF(itype.GE.26 .AND. itype.LE.37) THEN
        itpe_loc = 37 -(itype-14)
      ELSEIF(itype.GE.38 .AND. itype.LE.49) THEN
        itpe_loc = 61 -(itype-38)
      ELSEIF(itype.GE.50 .AND. itype.LE.61) THEN
        itpe_loc = 61 -(itype-38)
      ELSE
        write(6,*)'flip_flop=> wrong itype: ',itype
        stop
      ENDIF
      flip_flop=itpe_loc
      END

      SUBROUTINE kinga_2serl(mode,iflip,sprim,ambeam,amdet,i_flav,
     @            itype,dp1,dp2,dp3,dp4,fakp,wjac)
!     ***************************************************
!-- 1 x 3 channel  [4x(3x(1,2))]
! generation of angles (hidden) 
! construction of kinematics
! 4moms are inputs in mode 1
! iflip 1 - normal, -1 - mirror configuration (qeff1 <-> qeff2)
! prob_mass(2)=prob_inv
! prob_mass(1)=prob_flat
! prob_mass(2+i)=prob_bw(i)
! prob_ang(mkrok,5): 1 - flat
!                    2 - 1/t
!                    3 - 1/u
!                    4 - ln t/t
!                    5 - ln u/u
!     ***************************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION amdet(4),qeff1(4),qeff2(4),pt1(4),pt2(4)
      DIMENSION bq12(4),bq123(4),bp1(4),bp2(4),bp3(4),bp4(4)
      DIMENSION dq12(4),dq123(4),dp1(4),dp2(4),dp3(4),dp4(4)
      DIMENSION i_flav(4)

      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang),crd_ang(0:m_step,n_ang)
      DIMENSION mtype(0:m_step),crd_mass(m_step,n_mass)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      SAVE

      kindmp = 0
!-- set effective beams (CMS)
      IF(iflip.EQ.1) THEN
        CALL set_eff_beams(sprim,ambeam,qeff1,qeff2)
      ELSEIF(iflip.EQ.-1) THEN
        CALL set_eff_beams(sprim,ambeam,qeff2,qeff1)
      ELSE
        WRITE(6,*) 'kinga_ ==> wrong iflip ',iflip
      ENDIF

      CALL set_param(prob_ang,prob_mass,rmas,rgam,
     $               amreg2_mas,amreg2_ang,itype,i_flav)

      smi=(amdet(1)+amdet(2))**2
      IF(sprim.LT.(amdet(1)+amdet(2)+amdet(3)+amdet(4))**2) THEN
        fakp = 0d0
        wjac = 0d0
        RETURN
      ENDIF

      IF(mode.EQ.1) THEN
        DO i=1,4
          bq12(i)=dp1(i)+dp2(i)
          bq123(i)=bq12(i)+dp3(i)
          bp1(i)=dp1(i)
          bp2(i)=dp2(i)
          bp3(i)=dp3(i)
          bp4(i)=dp4(i)
        ENDDO
        s12  = dmas2(bq12)
        s123 = dmas2(bq123)
      ENDIF

      CALL mass_2serl_gen(mode,sprim,
     @     smi,prob_mass,rmas,rgam,amreg2_mas,
     @     s12,s123,sscru,mtype,crd_mass)
!------- check if outside phase-space
      IF(s12.LT.smi .OR. s123.LT.(dsqrt(s12)+amdet(3))**2
     @    .OR. sprim.LT.(dsqrt(s123)+amdet(4))**2 
     @    .OR. sscru.EQ.0d0) THEN 
        fakp = 0d0
        wjac = 0d0
        RETURN
      ENDIF

!------- generate primary angle and construct 4-vects
      do i=1,4
        pt1(i) = qeff1(i)
        pt2(i) = qeff2(i)
      enddo
      mkrok=0
      call kin_step(mode,amreg2_ang,prob_ang,sprim,s123,amdet(4)**2,
     $  mtype,mkrok,pt1,pt2, bq123,bp4, xccos,crd_ang)
      IF(xccos.EQ.0d0) THEN 
        fakp=0d0
        wjac=0d0
        RETURN
      ENDIF

      IF(KINDMP.EQ.1)THEN
        write(6,*) 'bq123, bp4'
        CALL
     $    DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot) 
      ENDIF
!------- generate secondary angles and construct 4-vects
      do i=1,4
        pt1(i) = -qeff1(i) +bq123(i)
      enddo
      mkrok=1
      call kin_step(mode,amreg2_ang,prob_ang, s123,s12,amdet(3)**2,
     $       mtype,mkrok,qeff1,pt1, bq12,bp3, xccos1,crd_ang)
      IF(xccos1.EQ.0d0) THEN 
        fakp=0d0
        wjac=0d0
        RETURN
      ENDIF

      IF(KINDMP.EQ.1)THEN
        write(6,*)'masses bq123,bq12,s123,s12 '
        write(6,*) dsqrt(dmas2(bq123)),dsqrt(dmas2(bq12))
        write(6,*) dsqrt(s123),dsqrt(s12)
        write(6,*) 'bq123, bq12, bp3, bp4'
        CALL
     $      DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
      do i=1,4
        pt1(i) = -qeff1(i) +bq12(i)
      enddo
      mkrok=2
      call kin_step(mode,amreg2_ang,prob_ang,
     $     s12,amdet(1)**2,amdet(2)**2, 
     $     mtype,mkrok,qeff1,pt1, bp1,bp2, xccos2,crd_ang)
      IF(xccos2.EQ.0d0) THEN 
        fakp=0d0
        wjac=0d0
        RETURN
      ENDIF

      IF(KINDMP.EQ.1)THEN
        write(6,*) 'in CMSeff, all'
        CALL
     $      DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot) 
      ENDIF
      IF(KINDMP.EQ.1)THEN
!----- consistency checks 
!-- require smineww.unused.f file --!
!        IF(mode.EQ.0) 
!     $  call kinebr(sprim,sprim,costhe,phi,cosde1,phi1,cosde2,phi2,
!     $  ambeam,dsqrt(s12),dsqrt(s123),amdet,bq12,bq123,bp1,bp2,bp3,bp4)
!        write(6,*) 'with kinebr'
!       CALL DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot)
!----- end consistency checks
      ENDIF

cccc m.s.25.04     CALL norm_2ser(crd_mass,prob_mass,
cccc m.s.25.04     $     crd_ang,prob_ang,fakp)

      fakp = xccos1*xccos2*xccos*sscru

! lambda factors

      bmain = wlambda(sprim,s123,amdet(4)**2)
      bwm   = wlambda(s123,s12,amdet(3)**2)
      bwp   = wlambda(s12,amdet(1)**2,amdet(2)**2)

      wjac=bmain*bwp*bwm

      IF(mode.EQ.0) THEN
        DO i=1,4
          dq12(i)=bq12(i)
          dq123(i)=bq123(i)
          dp1(i)=bp1(i)
          dp2(i)=bp2(i)
          dp3(i)=bp3(i)
          dp4(i)=bp4(i)
        ENDDO
      ENDIF

      END

      SUBROUTINE norm_2parl(crd_mass_p,crd_mass_g,prob_mass,
     $     crd_ang,prob_ang,   fakp)
!     ***************************************************
!!!!!!!!!!!!!! THIS ROUTINE IS WRONG !!!!!!!!!!!!
! IT IS NOT CORRECTED FOR THE NEW MEANING OF prob_ang !
!!!!!!!!!!!!!! THIS ROUTINE IS WRONG !!!!!!!!!!!!

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang),crd_ang(0:m_step,n_ang)
      DIMENSION crd_mass_g(m_step,n_mass),crd_mass_p(m_step,n_mass)
      DIMENSION prob_mass(n_mass)

      wta0=0d0
      DO k=1,n_ang
        wta0= wta0 +prob_ang(0,k)*crd_ang(0,k)
      ENDDO

      wt_=1d0
      wt_norm=1d0
      DO i=1,m_step
        wt_i=0d0
        wt_i_norm=0d0
        DO j=1,n_mass
          wt_ji=0d0
          DO k=1,n_ang
            wt_ji= wt_ji +prob_ang(j,k)*crd_ang(i,k)
          ENDDO
          wt_i= wt_i +wt_ji*prob_mass(j)*crd_mass_g(i,j)
          wt_i_norm= wt_i_norm 
     $              +prob_mass(j)*crd_mass_g(i,j)/crd_mass_p(i,j)
        ENDDO
        wt_= wt_ *wt_i 
        wt_norm = wt_norm *wt_i_norm
      ENDDO

      wt_norm = 1d0 - wt_norm

      fakp = wt_norm/(wt_*wta0)

      END

      SUBROUTINE norm_2ser(crd_mass,prob_mass,
     $     crd_ang,prob_ang,   fakp)
!     ***************************************************
!!!!!!!!!!!!!! THIS ROUTINE IS WRONG !!!!!!!!!!!!
! IT IS NOT CORRECTED FOR THE NEW MEANING OF prob_ang !
!!!!!!!!!!!!!! THIS ROUTINE IS WRONG !!!!!!!!!!!!

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang),crd_ang(0:m_step,n_ang)
      DIMENSION crd_mass(m_step,n_mass),prob_mass(n_mass)

      wta0=0d0
      DO k=1,n_ang
        wta0= wta0 +prob_ang(0,k)*crd_ang(0,k)
      ENDDO

      wt_=1d0
      wt_norm=1d0
      DO i=1,m_step
        wt_i=0d0
        DO j=1,n_mass
          wt_ji=0d0
          DO k=1,n_ang
            wt_ji= wt_ji +prob_ang(j,k)*crd_ang(i,k)
          ENDDO
          wt_i= wt_i +wt_ji*prob_mass(j)*crd_mass(i,j)
        ENDDO
        wt_norm=wt_norm/i
        wt_= wt_ *wt_i 
      ENDDO

      fakp = wt_norm/(wt_*wta0)

      END

      SUBROUTINE kinga_2serl_2(mode,iflip,sprim,ambeam,amdet,i_flav,
     @            itype,dp1,dp2,dp3,dp4,fakp,wjac)
!     ***************************************************
!-- 1 x 3 channel  [4x((3x2)x1)]
! generation of angles, construction of kinematics
! (angles, s23, s123) TOGETHER with 4moms are inputs in mode 1
! in mode 0 (angles, s23, s123) are redundant!
! prob_mass(2)=prob_inv
! prob_mass(1)=prob_flat
! prob_mass(2+i)=prob_bw(i)
!  prob_ang(mkrok,5): 1 - flat
!                     2 - 1/t
!                     3 - 1/u
!                     4 - ln t/t
!                     5 - ln u/u
!     ***************************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION amdet(4),qeff1(4),qeff2(4),pt1(4),pt2(4)
      DIMENSION bq23(4),bq123(4),bp1(4),bp2(4),bp3(4),bp4(4)
      DIMENSION dq23(4),dq123(4),dp1(4),dp2(4),dp3(4),dp4(4)
      DIMENSION i_flav(4)

      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang),crd_ang(0:m_step,n_ang)
      DIMENSION mtype(0:m_step),crd_mass(m_step,n_mass)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      SAVE

      kindmp = 0

!-- set effective beams (CMS)
      IF(iflip.EQ.1) THEN
        CALL set_eff_beams(sprim,ambeam,qeff1,qeff2)
      ELSEIF(iflip.EQ.-1) THEN
        CALL set_eff_beams(sprim,ambeam,qeff2,qeff1)
      ELSE
        WRITE(6,*) 'kinga_ ==> wrong iflip ',iflip
      ENDIF

      CALL set_param(prob_ang,prob_mass,rmas,rgam,
     $               amreg2_mas,amreg2_ang,itype,i_flav)

      smi=(amdet(2)+amdet(3))**2
      IF(sprim.LT.(amdet(1)+amdet(2)+amdet(3)+amdet(4))**2) THEN
        fakp = 0d0
        wjac = 0d0
        RETURN
      ENDIF

      IF(mode.EQ.1) THEN
        DO i=1,4
          bq23(i)=dp2(i)+dp3(i)
          bq123(i)=bq23(i)+dp1(i)
          bp1(i)=dp1(i)
          bp2(i)=dp2(i)
          bp3(i)=dp3(i)
          bp4(i)=dp4(i)
        ENDDO
        s23  = dmas2(bq23)
        s123 = dmas2(bq123)
      ENDIF

      CALL mass_2serl_gen(mode,sprim,
     @     smi,prob_mass,rmas,rgam,amreg2_mas,
     @     s23,s123,sscru,mtype,crd_mass)
!------- check if outside phase-space
      IF(s23.LT.smi .OR. s123.LT.(dsqrt(s23)+amdet(1))**2
     @    .OR. sprim.LT.(dsqrt(s123)+amdet(4))**2 
     @    .OR. sscru.EQ.0d0) THEN 
        fakp = 0d0
        wjac = 0d0
        RETURN
      ENDIF

!------- generate primary angle and construct 4-vects
      do i=1,4
        pt1(i) = qeff1(i)
        pt2(i) = qeff2(i)
      enddo
      mkrok=0
      call kin_step(mode,amreg2_ang,prob_ang, sprim,s123,amdet(4)**2,
     $  mtype,mkrok,pt1,pt2, bq123,bp4, xccos,crd_ang)
      IF(xccos.EQ.0d0) THEN 
        fakp=0d0
        wjac=0d0
        RETURN
      ENDIF

      IF(KINDMP.EQ.1)THEN
        write(6,*) 'bq123, bp4'
        CALL
     $    DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot) 
      ENDIF
!------- generate secondary angles and construct 4-vects
      do i=1,4
        pt1(i) =-qeff1(i) +bq123(i)
      enddo
      mkrok=1
      call kin_step(mode,amreg2_ang,prob_ang, s123,amdet(1)**2,s23,
     $  mtype,mkrok,qeff1,pt1, bp1,bq23, xccos1,crd_ang)
      IF(xccos1.EQ.0d0) THEN 
        fakp=0d0
        wjac=0d0
        RETURN
      ENDIF

      IF(KINDMP.EQ.1)THEN
        write(6,*)'masses bq123,bq23,s123,s23 '
        write(6,*) dsqrt(dmas2(bq123)),dsqrt(dmas2(bq23))
        write(6,*) dsqrt(s123),dsqrt(s23)
        write(6,*) 'fakp,wjac,xccos,xccos1,xccos2'
        write(6,*) fakp,wjac,xccos,xccos1,xccos2
        write(6,*) 'bq123, bq23, bp1, bp4'
        CALL
     $      DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
      do i=1,4
        pt1(i) = qeff1(i) -bp1(i)
        pt2(i) = -bp4(i) +qeff2(i)
      enddo
      mkrok=2
      call kin_step(mode,amreg2_ang,prob_ang,
     $     s23,amdet(2)**2,amdet(3)**2, 
     $     mtype,mkrok,pt1,pt2,bp2,bp3, xccos2,crd_ang)
      IF(xccos2.EQ.0d0) THEN 
        fakp=0d0
        wjac=0d0
        RETURN
      ENDIF

      IF(KINDMP.EQ.1)THEN
        write(6,*) 'in CMSeff, all'
        CALL
     $      DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot) 
      ENDIF

ccc m.s. 25.04      CALL norm_2ser(crd_mass,prob_mass,
ccc m.s. 25.04     $     crd_ang,prob_ang,fakp)

      fakp = xccos1*xccos2*xccos*sscru

! lambda factors

      bmain = wlambda(sprim,s123,amdet(4)**2)
      bwm   = wlambda(s123,s23,amdet(1)**2)
      bwp   = wlambda(s23,amdet(2)**2,amdet(3)**2)

      wjac=bmain*bwp*bwm

      IF(mode.EQ.0) THEN
        DO i=1,4
          dq23(i)=bq23(i)
          dq123(i)=bq123(i)
          dp1(i)=bp1(i)
          dp2(i)=bp2(i)
          dp3(i)=bp3(i)
          dp4(i)=bp4(i)
        ENDDO
      ENDIF

      END


      SUBROUTINE kinga_2parl(mode,iflip,sprim,ambeam,amdet,i_flav,
     @            itype,dp1,dp2,dp3,dp4,fakp,wjac)
!     ***************************************************
!-- 2 x 2 channel [(1,2)x(3,4)]
! generation of angles, construction of kinematics
! (angles, s1, s2) TOGETHER with 4moms are inputs in mode 1
! in mode 0 (angles, s1, s2) are redundant!
! prob_mass(2)=prob_inv
! prob_mass(1)=prob_flat
! prob_mass(2+i)=prob_bw(i)
! prob_ang(mkrok,5): 1 - flat
!                    2 - 1/t
!                    3 - 1/u
!                    4 - ln t/t
!                    5 - ln u/u
!     ***************************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION amdet(4),qeff1(4),qeff2(4),pt1(4),pt2(4)
      DIMENSION bq1(4),bq2(4),bp1(4),bp2(4),bp3(4),bp4(4)
      DIMENSION dq1(4),dq2(4),dp1(4),dp2(4),dp3(4),dp4(4)
      DIMENSION i_flav(4)

      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang),crd_ang(0:m_step,n_ang)
      DIMENSION mtype(0:m_step)
      DIMENSION crd_mass_p(m_step,n_mass),crd_mass_g(m_step,n_mass)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      SAVE

      kindmp = 0

!-- set effective beams (CMS)
      IF(iflip.EQ.1) THEN
        CALL set_eff_beams(sprim,ambeam,qeff1,qeff2)
      ELSEIF(iflip.EQ.-1) THEN
        CALL set_eff_beams(sprim,ambeam,qeff2,qeff1)
      ELSE
        WRITE(6,*) 'kinga_ ==> wrong iflip ',iflip
      ENDIF

      CALL set_param(prob_ang,prob_mass,rmas,rgam,
     $               amreg2_mas,amreg2_ang,itype,i_flav)

      smi1=(amdet(1)+amdet(2))**2
      smi2=(amdet(3)+amdet(4))**2
      IF(sprim.LT.(amdet(1)+amdet(2)+amdet(3)+amdet(4))**2) THEN
        fakp = 0d0
        wjac = 0d0
        RETURN
      ENDIF

      IF(mode.EQ.1) THEN
        DO i=1,4
          bq1(i)=dp1(i)+dp2(i)
          bq2(i)=dp3(i)+dp4(i)
          bp1(i)=dp1(i)
          bp2(i)=dp2(i)
          bp3(i)=dp3(i)
          bp4(i)=dp4(i)
        ENDDO
        s1  = dmas2(bq1)
        s2  = dmas2(bq2)
      ENDIF

      CALL mass_2parl_gen(mode,sprim,
     @  smi1,prob_mass,rmas,rgam,amreg2_mas,
     @  smi2,prob_mass,rmas,rgam,amreg2_mas,
     @  s1,s2,sscru,mtype,crd_mass_p,crd_mass_g)

      IF(KINDMP.EQ.1)THEN
        write(6,*)'s1,s2 ',s1,s2
      ENDIF
!------- check if outside phase-space
      IF(s1.LT.smi1 .OR. s2.LT.smi2
     @   .OR. sprim.LT.(dsqrt(s1)+dsqrt(s2))**2 
     @   .OR. sscru .EQ. 0d0) THEN
        fakp=0d0
        wjac=0d0
        RETURN
      ENDIF
!------- generate primary angle and construct 4-vects
      do i=1,4
        pt1(i) = qeff1(i)
        pt2(i) = qeff2(i)
      enddo
      mkrok=0
      call kin_step(mode,amreg2_ang,prob_ang, sprim,s1,s2,
     $    mtype,mkrok,pt1,pt2, bq1,bq2, xccos,crd_ang)
      IF(KINDMP.EQ.1)THEN
        write(6,*)'bq1, bq2 ',costhe,phi
cc        CALL DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
      do i=1,4
        pt1(i) = -qeff1(i) +bq1(i)
        pt2(i) = -pt1(i)
      enddo
!------- generate secondary angles and construct 4-vects
      mkrok=1
      call kin_step(mode,amreg2_ang,prob_ang,s1,amdet(1)**2,amdet(2)**2,
     $     mtype,mkrok,qeff1,pt1, bp1,bp2, xccos1,crd_ang)
      IF(KINDMP.EQ.1)THEN
cc        write(6,*)'bp1, bp2 ',cosde1,phi1
        write(6,*)'bp1, bp2 ',costhe,phi
cc        CALL DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
      mkrok=2
      call kin_step(mode,amreg2_ang,prob_ang,s2,amdet(4)**2,amdet(3)**2,  
     $    mtype,mkrok,qeff2,pt2,bp4,bp3, xccos2,crd_ang)
      IF(KINDMP.EQ.1)THEN
        write(6,*)'CMS-eff (bp3,bp4)',costhe,phi
cc        write(6,*)'CMS-eff (bp3,bp4)',cosde2,phi2
cc        CALL DUMPL(6,bP1,bP2,bP3,bP4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF

      IF(xccos1.EQ.0 .OR. xccos2.EQ.0 .OR. xccos.EQ.0 
     $    .OR. sscru.EQ.0) THEN 
        fakp=0d0
        wjac=0d0
        RETURN
      ENDIF

ccc m.s. 25.04      CALL norm_2parl(crd_mass_p,crd_mass_g,prob_mass,
ccc m.s. 25.04     $      crd_ang,prob_ang,fakp)

      fakp = xccos1*xccos2*xccos*sscru

! lambda factors

      bmain=wlambda(sprim,s1,s2)
      bwm=  wlambda(s1,amdet(1)**2,amdet(2)**2)
      bwp=  wlambda(s2,amdet(3)**2,amdet(4)**2)


      wjac=bmain*bwp*bwm
c      write(6,*) 'PAR fakp,wjac=',fakp,wjac
c      write(6,*) 'PAR crud c1,c2,c,ss',xccos1,xccos2,xccos,sscru

      IF(mode.EQ.0) THEN
        DO i=1,4
          dq1(i)=bq1(i)
          dq2(i)=bq2(i)
          dp1(i)=bp1(i)
          dp2(i)=bp2(i)
          dp3(i)=bp3(i)
          dp4(i)=bp4(i)
        ENDDO
      ENDIF

      END

      SUBROUTINE mass_1_gen
     @  (mode,smi,sma,prob_mass,
     @   rmas,rgam,amr2,sout,imtype,crd_mass0,wt)
!     *******************************************************
! Basic generation of 1-dim ds distribution from smi to sma
! Shape: prob_in/(s+amr2) +\sum^nres prob_bw_i/BW_i + prob_fl
! BW = (sout-rmas(i)^2)^2 + (rmas(i)*rgam(i))^2
! prob_fl    = prob_mass(1)
! prob_in    = prob_mass(2)
! prob_in*ln = prob_mass(3)
! prob_in-s  = prob_mass(4)
! prob_bw(i) = prob_mass(i+4)
! prob_fl +\sum prob_bw(i) + prob_in = 1 assumed !
! crd_mass0(i) = f(i) / \int f(i),  f - i-th branch distrib.
!---------------------
! for mode=0
! OUTPUT: sout  : s variable
!         wt    : wt=1/ (\sum prob_i f_i/F_i); F_i=\int f_i
!---------------------
! for mode=1
! INPUT:  sout  - no generation,  just calculation of weight wt. 
!---------------------
!
! (Re)Written by: M. Skrzypek            date: 6/12/96
!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
      SAVE / matpar / 
      DIMENSION drvec(100)

      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
!      DIMENSION prob_ang(0:m_step,n_ang),crd_ang(0:m_step,n_ang)
      DIMENSION crd_mass0(n_mass)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      DIMENSION alp_ma(n_m_res),alp_mi(n_m_res)
      DIMENSION pr(0:n_mass)
      SAVE

      IF(smi.LT.1d-12 .AND. amr2.LT.1d-13) THEN
        WRITE(6,*)'mass_1_gen==> Expected troubles at s=0: ',smi,amr2
        STOP
      ENDIF

      pr(0)= 0d0
      DO i=1,n_mass
        pr(i)=pr(i-1) +prob_mass(i)
      ENDDO

      DO i=1,n_m_res
        rmrg = rmas(i)*rgam(i)
        alp_ma(i)=atan( (sma-rmas(i)**2)/rmrg )
        alp_mi(i)=atan( (smi-rmas(i)**2)/rmrg )
      ENDDO

      biglog =dlog((sma+amr2)/(smi+amr2))
      biglog2=dlog((sma-smi+amr2)/amr2)
! set strenght of log. singul.
      n_log=1
! setting strenght of power sing.
      n_inv=2
! end setting strenght
      nlo=n_log+1
      ninv=n_inv-1

      sma1=-1/( ninv*(sma+amr2)**ninv )
      smi1=-1/( ninv*(smi+amr2)**ninv )

!     ====================
      IF (mode.EQ.0) THEN
!     ====================
!
 10   CALL varran(drvec,2)
      r1=drvec(1)
      r2=drvec(2)
      IF(r1.LE.pr(1)) THEN      
!-- flat s
        sout=(sma-smi)*r2+smi 
        imtype=1
      ELSEIF(r1.LE.pr(2)) then 
!-- 1/(s+amr2)
        sout=(smi+amr2)*exp(r2*biglog) -amr2
        imtype=2
      ELSEIF(r1.LE.pr(3)) then 
!-- ln(s+amr2/sma)**n_log/(s+amr2)
        ymax= -biglog**nlo/nlo
        ymin=  0d0
        y1=r2*(ymax-ymin)+ymin
        sout=(sma+amr2)*exp( -(-nlo*y1)**(1d0/dble(nlo)) ) -amr2
        imtype=3
      ELSEIF(r1.LE.pr(4)) then 
!-- 1/(sma-s+amr2)
        sout=(    amr2)*exp(r2*biglog2) -amr2
        sout=sma-sout
        if(sout.gt.sma) sout=sma
        imtype=4
      ELSEIF(r1.LE.pr(5)) then 
!-- 1/(s+amr2)**n_inv
        y1=r2*(sma1-smi1) +smi1
        sout=(-1/(ninv*y1))**(1d0/dble(ninv)) -amr2
        imtype=5
      ELSE
!-- resonance 
        DO i=1,n_m_res
          IF(r1.LE.pr(n_mass-n_m_res+i)) then
            alp=alp_mi(i)+r2*(alp_ma(i)-alp_mi(i))
            sout=rmas(i)**2+rmas(i)*rgam(i)*tan(alp)
            imtype=i+n_mass-n_m_res
            GOTO 11
          ENDIF
        ENDDO
        WRITE(6,*) 'mass_1_gen==> Total probability below 1: ',sum
        STOP
      ENDIF
 11   CONTINUE

!     =====
      ENDIF
!     =====

      IF(sout.LT.0d0 ) THEN
        WRITE(6,*)'mass_1_gen==> Negative sout: ',sout
        STOP
      ENDIF

! Normalisation
!-- flat 
      crd_mass0(1) = 1/(sma -smi)
!-- 1/(s+amr2)
      crd_mass0(2) = 1/( (sout+amr2)*biglog )
!-- -log(s+amr2/sma+amr2)^n/(s+amr2)
      crd_mass0(3) = (-dlog((sout+amr2)/(sma+amr2)))**n_log/(sout+amr2) 
     @                 /(biglog**nlo/nlo) 
!-- 1/(sma-s+amr2)
      crd_mass0(4) = 1/( (sma-sout+amr2)*biglog2 )
!-- 1/(s+amr2)**n_inv
      crd_mass0(5) = 1/(sout+amr2)**n_inv /(sma1-smi1) 
!-- resonances
      DO i=1,n_m_res
        rmrg = rmas(i)*rgam(i)
        crd_mass0(n_mass-n_m_res+i) =1/( ((sout-rmas(i)**2)**2+rmrg**2)
     @              *(alp_ma(i)-alp_mi(i))/rmrg )
      ENDDO

      ph_total=0d0
      DO i=1,n_mass
        ph_total = ph_total +prob_mass(i)*crd_mass0(i)
      ENDDO

      wt=1/ph_total

      END

      SUBROUTINE mass_2parl_gen(mode,smax,
     @  smi1,prob_mass1,rmas1,rgam1,amr21,
     @  smi2,prob_mass2,rmas2,rgam2,amr22,
     @  sout1,sout2,wt,mtype,crd_mass_p,crd_mass_g)
!     ***************************************************************
! Generation of ds_1ds_2 distribution of 2 parallel branches 
! within phase space boundaries using weighted (pre-sampled) events
! branches can have different coefficients and limits
!---------------------
! for mode=0
! OUTPUT: sout1, sout2  
!         wt    : wt=1/ (\sum prob_i f_i/F_i); F_i=\int f_i
!---------------------
! for mode=1
! INPUT:  sout1, sout2  - no generation,  just calculation of weight. 
!---------------------
!
! (Re)Written by: M. Skrzypek            date: 6/12/96
!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
      SAVE / matpar / 

      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
!      DIMENSION prob_ang(0:m_step,n_ang),crd_ang(0:m_step,n_ang)
      DIMENSION mtype(0:m_step)
      DIMENSION crd_mass_p(m_step,n_mass),crd_mass_g(m_step,n_mass)
      DIMENSION rmas1(n_m_res),rgam1(n_m_res),prob_mass1(n_mass)
      DIMENSION rmas2(n_m_res),rgam2(n_m_res),prob_mass2(n_mass)
      DIMENSION crd_grd1(n_mass),crd_grd2(n_mass)
      DIMENSION crd_ptt1(n_mass),crd_ptt2(n_mass)

      SAVE


      sxsec1 = smax/4d0
      IF(sxsec1.LT.smi1) sxsec1 = smi1
      sxsec2 = smax/4d0
      IF(sxsec2.LT.smi2) sxsec2 = smi2

! Generation, Normalization

 12   CONTINUE

      CALL mass_1_gen
     @  (mode,smi1,smax,prob_mass1,
     @   rmas1,rgam1,amr21,sout1,imtype1,crd_grd1,wt1_grd)

      CALL mass_1_gen
     @  (mode,smi2,smax,prob_mass2,
     @   rmas2,rgam2,amr22,sout2,imtype2,crd_grd2,wt2_grd)

!-- rejection
      IF(mode.EQ.0 .AND. sout1.GT.sxsec1 .AND. sout2.GT.sxsec2) GOTO 12

      IF( dsqrt(sout1)+dsqrt(sout2) .GT. dsqrt(smax) ) THEN 
!-- check if event outside of phase-space
        wt = 0d0
        RETURN
      ENDIF


! Total normalisation

      CALL mass_1_gen
     @  (1,sxsec1,smax,prob_mass1,
     @   rmas1,rgam1,amr21,sout1,imtype,crd_ptt1,wt1_ptt)

      CALL mass_1_gen
     @  (1,sxsec2,smax,prob_mass2,
     @   rmas2,rgam2,amr22,sout2,imtype,crd_ptt2,wt2_ptt)

      DO i=1,n_mass
        crd_mass_g(1,i)=crd_grd1(i)
        crd_mass_g(2,i)=crd_grd2(i)
        crd_mass_p(1,i)=crd_ptt1(i)
        crd_mass_p(2,i)=crd_ptt2(i)
      ENDDO
      mtype(0)=0
      mtype(1)=imtype1
      mtype(2)=imtype2

      crd1 = 0d0
      crd1_norm = 0d0
      DO i=1,n_mass
        crd1 = crd1 +prob_mass1(i)*crd_grd1(i)
        crd1_norm = crd1_norm +prob_mass1(i)*crd_grd1(i)/crd_ptt1(i)
      ENDDO

      crd2 = 0d0
      crd2_norm = 0d0
      DO i=1,n_mass
        crd2 = crd2 +prob_mass2(i)*crd_grd2(i)
        crd2_norm = crd2_norm +prob_mass2(i)*crd_grd2(i)/crd_ptt2(i)
      ENDDO

      crd_norm=1d0 -crd2_norm*crd1_norm

      wt = crd_norm/(crd1*crd2)

      END


      SUBROUTINE mass_2serl_gen_1(mode,smax,
     @  smi,prob_mass,rmas,rgam,amr2,
     @  sout12,sout123,wt,mtype,crd_mass)
!     ***************************************************************
! ORIGINAL ROUTINE OF v 1.40
! Generation of ds_1ds_2 distribution of 2 serial (nested) branches 
! within phase space boundaries using weighted (pre-sampled) events
! s1 ist the outer variable, s2 the inner one.
! Branches have identical coefficients and limits.
! Limits: smax > s123 > s12 > smi.
!---------------------
! for mode=0
! OUTPUT: sout12, sout123  
!         wt    : wt=1/ (\sum prob_i f_i/F_i); F_i=\int f_i
!---------------------
! for mode=1
! INPUT:  sout12, sout123  - no generation,  just calculation of weight. 
!---------------------
!
! (Re)Written by: M. Skrzypek            date: 6/12/96
!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
      SAVE / matpar / 

      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION mtype(0:m_step),crd_mass(m_step,n_mass)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      DIMENSION crd_mass1(n_mass),crd_mass2(n_mass)
      SAVE

! Generation

      CALL mass_1_gen
     @  (mode,smi,smax,prob_mass,
     @   rmas,rgam,amr2,sout123,imtype123,crd_mass1,wt1_grd)

      CALL mass_1_gen
     @  (mode,smi,smax,prob_mass,
     @   rmas,rgam,amr2,sout12,imtype12,crd_mass2,wt2_grd)


      IF(mode.EQ.0 .AND. sout12.GT.sout123) THEN
        sou=sout12
        sout12=sout123
        sout123=sou
        mtype(0)=0
        mtype(1)=imtype12
        mtype(2)=imtype123
        DO i=1,n_mass
          crd_mass(1,i)=crd_mass2(i)
          crd_mass(2,i)=crd_mass1(i)
        ENDDO
      ELSE
        mtype(0)=0
        mtype(1)=imtype123
        mtype(2)=imtype12
        DO i=1,n_mass
          crd_mass(1,i)=crd_mass1(i)
          crd_mass(2,i)=crd_mass2(i)
        ENDDO
      ENDIF


! Normalisation

      wt = wt1_grd*wt2_grd 
      wt=wt/2               ! to account for ordering

      END

      SUBROUTINE mass_2serl_gen_2(mode,smax,
     @  smi,prob_mass,rmas,rgam,amr2,
     @  sout12,sout123,wt,mtype,crd_mass)
!     ***************************************************************
! SECOND VERSION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Generation of ds_1ds_2 distribution of 2 serial (nested) branches 
! within phase space boundaries using weighted (pre-sampled) events
! s1 ist the outer variable, s2 the inner one.
! Branches have identical coefficients and limits.
! Limits: smax > s123 > s12 > smi.
!---------------------
! for mode=0
! OUTPUT: sout12, sout123  
!         wt    : wt=1/ (\sum prob_i f_i/F_i); F_i=\int f_i
!---------------------
! for mode=1
! INPUT:  sout12, sout123  - no generation,  just calculation of weight. 
!---------------------
!
! (Re)Written by: M. Skrzypek            date: 6/12/96
!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
      SAVE / matpar / 

      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION mtype(0:m_step),crd_mass(m_step,n_mass)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      DIMENSION crd_mass1(n_mass),crd_mass2(n_mass)
      SAVE

! Generation

      CALL mass_1_gen
     @  (mode,smi,smax,prob_mass,
     @   rmas,rgam,amr2,sout123,imtype123,crd_mass1,wt1_grd)

      CALL mass_1_gen
     @  (mode,smi,sout123,prob_mass,
     @   rmas,rgam,amr2,sout12,imtype12,crd_mass2,wt2_grd)


      IF(mode.EQ.0 ) THEN
        mtype(0)=0
        mtype(1)=imtype123
        mtype(2)=imtype12
        DO i=1,n_mass
          crd_mass(1,i)=crd_mass1(i)
          crd_mass(2,i)=crd_mass2(i)
        ENDDO
      ENDIF


! Normalisation

      wt = wt1_grd*wt2_grd 

      END

      SUBROUTINE mass_2serl_gen(mode,smax,
     @  smi,prob_mass,rmas,rgam,amr2,
     @  sout12,sout123,wt,mtype,crd_mass)
!     ***************************************************************
! SECOND VERSION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Generation of ds_1ds_2 distribution of 2 serial (nested) branches 
! within phase space boundaries using weighted (pre-sampled) events
! s1 ist the outer variable, s2 the inner one.
! Branches have identical coefficients and limits.
! Limits: smax > s123 > s12 > smi.
!---------------------
! for mode=0
! OUTPUT: sout12, sout123  
!         wt    : wt=1/ (\sum prob_i f_i/F_i); F_i=\int f_i
!---------------------
! for mode=1
! INPUT:  sout12, sout123  - no generation,  just calculation of weight. 
!---------------------
!
! (Re)Written by: M. Skrzypek            date: 6/12/96
!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
      SAVE / matpar / 

      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION mtype(0:m_step),crd_mass(m_step,n_mass)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      SAVE

c      CALL mass_2serl_gen_2(mode,smax,
c     @  smi,prob_mass,rmas,rgam,amr2,
c     @  sout12,sout123,wt,mtype,crd_mass)

      CALL mass_2serl_gen_1(mode,smax,
     @  smi,prob_mass,rmas,rgam,amr2,
     @  sout12,sout123,wt,mtype,crd_mass)

      END

      SUBROUTINE kin_step(mode,amreg2_ang,prob_ang,s12,s1,s2, 
     $      mtype,mkrok,pinb1,pinb2, pout1,pout2, wt,crd_ang)
*     ************************************************************
! generates and constructs kinematics of a single 'decay'
c INPUT:  
!         mode = 0 generation
!                1 weight calculation based on 4-moms
!         amreg         auxiliary mass for regularization
!         prob_ang(6): 1 - flat
!                      2 - 1/t
!                      3 - 1/u
c         s12    inv mass of 1+2 system
c         s1,s2  squared outgoing masses
c         pinb1,pinb2   reference momenta to span angles on them
c                       in certain reference frame Cref
c         imtype: generated mass channel 
c                 0 - none
c                 1 - 1/s
c                 2 - 1
c                 n+2 - BW(n)
c OUTPUT:
c         pout1(4)  4-momentum of 1 in Cref (INPUT for mode = 1)
c         pout2(4)  4-momentum of 2 in Cref (INPUT for mode = 1)
c         wt        weight
c
c (Re)Written by: M. Skrzypek              date: 1/20/97
c
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION pout1(4),pout2(4)
      DIMENSION pinb1(4),pinb2(4)
      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang),crd_ang(0:m_step,n_ang)
      DIMENSION mtype(0:m_step) !,crd_mass(m_step,n_mass)
!      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
!      real*16 costhe,phi
      SAVE

      CALL cosdec_br(mode,amreg2_ang,mtype,mkrok,prob_ang,
     $  pinb1,pinb2, pout1,pout2,s12,s1,s2,  costhe,phi,wt,crd_ang)
      IF(wt.EQ.0) RETURN
      IF(mode.EQ.0) CALL kin_dec(pinb1,pinb2,costhe,phi,
     $                           s12,s1,s2, pout1,pout2) 

      END

      subroutine kin_dec(pinb1,pinb2,costhe,fi,
     $                           s12,am1sq,am2sq, q1,q2) 
*     ************************************************************
! corresponds to kinett of v.121
c Construction of kinematics for cosdec_br.
c INPUT:  
c         pb1,pb2   reference momenta to span angles on them
c                   in certain reference frame Cref
c         costhe,fi -  production angles 
c         s12 - inv mass of 1+2 system
c         am1sq,am2sq  squared masses of q1 and q2
c OUTPUT:
c         q1(4)        - four-momentum of 1 in Cref
c         q2(4)        - four-momentum of 2 in Cref
c
c (Re)Written by: M. Skrzypek              date: 7/6/96
c
      implicit DOUBLE PRECISION (a-h,o-z)
      save
      dimension q1(4),q2(4)
      dimension b12(4),pb1(4),pb2(4),pinb1(4),pinb2(4)

!      real*16 costhe,fi
!

!!      WRITE(6,*)'kindec: costhe,fi=',costhe,fi

      sinthe =sqrt(max(0d0,((1d0-costhe)*(1d0+costhe))))
!!      if(sinthe.eq.0d0)  write(6,*)'kin_dec==> sinthe=0'
      do i=1,4
        pb1(i)=pinb1(i)
        pb2(i)=pinb2(i)
      enddo

!!      WRITE(6,*)'kindec: pinb1,pinb2=',pb1,pb2

!... q1, q2 in their own rest frame
!... Momentum q1 
      qq=dsqrt( (s12-am1sq-am2sq)**2 -4*am1sq*am2sq )/(2*sqrt(s12))
      q1(1)=qq*sinthe*cos(fi)
      q1(2)=qq*sinthe*sin(fi)
      q1(3)=qq*costhe
      q1(4)=dsqrt(am1sq+q1(1)**2+q1(2)**2+q1(3)**2)
!!... Momentum q2 

!!      WRITE(6,*)'kindec: q1 bef. boost=',q1

      do i=1,4
        b12(i)=pb1(i)+pb2(i)     
      enddo

!... boost pb1 from Cref to b12 rest frame
      call boostv(1,b12,pb1,pb1)
!... rotate q1,q2 to frame parallel to pb1 (in b12 rest fr)
      call rotatv(1,pb1,q1,q1)
!... boost q1, q2 from b12 rest frame to Cref
      call boostv(-1,b12,q1,q1)

!!      WRITE(6,*)'kindec: q1 aft. boost=',q1

      do i=1,3
        q2(i)=b12(i)-q1(i)     
      enddo

!!      WRITE(6,*)'kindec: q2=',q2

!... fine tuning on masses
      q1(4)=dsqrt(am1sq+q1(1)**2+q1(2)**2+q1(3)**2)
      q2(4)=dsqrt(am2sq+q2(1)**2+q2(2)**2+q2(3)**2)
      
      end

      subroutine cosdec_br(mode,amreg2_ang,
     $          mtype,mkrok,prob_ang,pinb1,pinb2,
     $          pout1,pout2,s12,s1,s2,  costhe,phi,wt,crd_ang)
*     *********************************************************
c This is cosdec_br to generate decay angles
! version based fully on invariants !!
c INPUT:  
!    mode     0-generation
!             1-xccos of given cdec
!    prob_ang(mkrok,5): 1 - flat
!                       2 - 1/t
!                       3 - 1/u
!                       4 - ln t/t
!                       5 - ln u/u
!    pout1,pout2   only for mode=1, outgoing momenta,
!                  to construct weight.
c    pinb1,pinb2   reference momenta to span angles on them
c                  in certain reference frame Cref
!    WARNING now both pinb1&2 are incoming !!!
c    s12 - inv masses of 1+2 system
c    s1,s2  squared masses of q1 and q2
c OUTPUT:
c         costhe,phi -  production angles 
c         wt - weight
c
c (Re)Written by: M. Skrzypek              date: 7/6/96
c Last update: 9/19/96 M.S.

      implicit DOUBLE PRECISION (a-h,o-z)
c      implicit real*16 (a-h,o-z)
c      DOUBLE PRECISION amreg2_ang
c      DOUBLE PRECISION prob_ang,pinb1,pinb2
c      DOUBLE PRECISION pout1,pout2,s12,s1,s2 ,wt,crd_ang
c      DOUBLE PRECISION pi,ceuler,drvec
c      DOUBLE PRECISION dmas2
      common / matpar / pi,ceuler     
      dimension drvec(100),pb1(4),pb2(4),b12(4),pinb1(4),pinb2(4)
      dimension pout1(4),pout2(4)
      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang),crd_ang(0:m_step,n_ang)
      DIMENSION mtype(0:m_step) !,crd_mass(m_step,n_mass)
!      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      DIMENSION pr(0:n_ang)
      SAVE

! we will flaten  (BUT NOT CUT) distribution at 
      amflt2 = amreg2_ang

      do i=1,4
        pb1(i)=pinb1(i)
        pb2(i)=pinb2(i)
        b12(i)=pb1(i) +pb2(i) 
      enddo

      t1=dmas2(pb1)
      t2=dmas2(pb2)
      dt=dot(pb1,pb2)
      s=s12

!=========
      x01 = (s12+s1-s2)/2/dsqrt(s12)
      x02 = (s12+s2-s1)/2/dsqrt(s12)
      ex01=x01**2 -s1
      ex02=x02**2 -s2
      IF(ex01.GT.ex02) THEN
        xx= sqrt(max(0d0,ex01))
      ELSE
        xx= sqrt(max(0d0,ex02))
      ENDIF
      IF(xx.EQ.0d0)THEN
        wt=0d0
        write(6,*)'cosdec_br==> xx=0'
        RETURN
      ENDIF

!((((((((((((( replace this with perhaps more stable
!      pp=sqrt( max( 0d0,(dt**2-t1*t2)/s12 ) )
!      IF(pp.EQ.0d0)THEN
!        wt=0d0
!        write(6,*)'cosdec_br  ==>  pp=0'
!        RETURN
!      ENDIF
!      p0=(t1 +dt)/sqrt(s12)
!)))))))))))))) 3/24/97 m.s.
      px01 = (s12+t1-t2)/2/dsqrt(s12)
      px02 = (s12+t2-t1)/2/dsqrt(s12)
      pex01=px01**2 -t1
      pex02=px02**2 -t2
      IF(pex01.LE.0d0 .AND. pex02.LE.0d0)THEN
        wt=0d0
        write(6,*)'cosdec_br  ==>  pp=0'
        write(6,*)'pp2,pex01,pex02',(dt**2-t1*t2)/s12,pex01,pex02
        RETURN
      ENDIF
      IF(pex01.GT.pex02) THEN
        pp= sqrt(max(0d0,pex01))
      ELSE
        pp= sqrt(max(0d0,pex02))
      ENDIF
      p0=px01
!!!!!!!!!!!!!!!

      bb = -xx*pp

!=========
!... first fermion (in b12 rest fr.)
      x01 = sqrt(xx**2+s1)
      aa1 =  x01*p0   -1/2d0*(t1 +s1)
      IF(aa1.lt.-bb)THEN
!        write(6,*)'cosdec_br=>aa1, -bb=',aa1,-bb
        aa1=-bb
      ENDIF
      aa1=aa1+amflt2/2


      ymi1=log(aa1-bb)
      yma1=log(aa1+bb)
      IF(yma1.EQ.ymi1)THEN
        wt=0d0
        write(6,*)'cosdec_br==> ymi1=yma1'
        RETURN
      ENDIF

!      tmima1=((s1-t1)*(s2-t2) +(t2*s1-t1*s2)*(s1-t1-s2+t2)/s12)/4
!      eyma1 =tmima1/(x01*p0 -1/2d0*(t1 +s1) +xx*pp) +amflt2/2d0
!      yma1=log(eyma1)
!      write(6,*)'c_br     =>eyma1,aa1+bb=',eyma1,aa1+bb

!... second fermion (in b12 rest fr.)
      x02 = sqrt(xx**2+s2)
! is this a bug ??? 1.07.97 ms
! 1.07.97 ms      aa2 =  x02*p0   -1/2d0*(t1 +s2) +amflt2/2d0
      aa2 =  x02*p0   -1/2d0*(t1 +s2) 
! is this a bug ??? 1.07.97 ms
      IF(aa2.lt.-bb)THEN
!        write(6,*)'cosdec_br=>aa2, -bb=',aa2,-bb
        aa2=-bb
      ENDIF
      aa2=aa2+amflt2/2

      ymi2=log(aa2-bb)
      yma2=log(aa2+bb)
      IF(yma2.EQ.ymi2)THEN
        wt=0d0
        write(6,*)'cosdec_br==> ymi2=yma2'
        RETURN
      ENDIF

!      tmima2=((s2-t1)*(s1-t2) +(t2*s2-t1*s1)*(s2-t1-s1+t2)/s12)/4
!      eyma2 =tmima2/(x02*p0 -1/2d0*(t1 +s2) +xx*pp) +amflt2/2d0
!      yma2=log(eyma2)

      pr(0)= 0d0
      DO i=1,n_ang
        pr(i)=pr(i-1) +prob_ang(mkrok,i)
      ENDDO

! set singul. strenght
! logarithmic (log t)**n_log/t
      n_log =1   ! original 
      n_log2=2   ! original 
cc      n_log =1   ! !!!!!!!!!!
cc      n_log2=3   ! !!!!!!!!!! 
! power 1/t**n_inv
      n_inv =2      ! original  
ccc      n_inv =3    !!!!!!!!!!!!!!!!!!!!!!!!!!    
! end setting

      nlo =n_log+1
      nlo2=n_log2+1
      ninv=n_inv-1

      tma1=-1/((aa1+bb)**ninv*bb*ninv)
      tmi1=-1/((aa1-bb)**ninv*bb*ninv)
      tma2=-1/((aa2+bb)**ninv*bb*ninv)
      tmi2=-1/((aa2-bb)**ninv*bb*ninv)

      IF(mode.EQ.0) THEN

        call varran(drvec,3)

        r3=drvec(3)
        drv2=drvec(2)
        drv1=drvec(1)

        IF(r3.le.pr(1)) THEN
          costhe= 2*drv1-1  
        ELSEIF(r3.le.pr(2)) THEN
          ymax=yma1/bb
          ymin=ymi1/bb
          y1=drv1*(ymax-ymin)+ymin
          costhe= (exp(bb*y1)-aa1)/bb
        ELSEIF(r3.le.pr(3)) THEN
          ymax=yma2/bb
          ymin=ymi2/bb
          y2=drv1*(ymax-ymin)+ymin
          costhe= -(exp(bb*y2)-aa2)/bb
        ELSEIF(r3.le.pr(4)) THEN
          ymax= -(ymi1-yma1)**nlo/(bb*nlo)
          ymin=  0d0
          y1=drv1*(ymax-ymin)+ymin
          tt=(aa1-bb)* exp( -(-nlo*bb*y1)**(1d0/dble(nlo)) )
          costhe= (tt -aa1)/bb
        ELSEIF(r3.le.pr(5)) THEN
          ymax= -(ymi2-yma2)**nlo/(bb*nlo)
          ymin=  0d0
          y1=drv1*(ymax-ymin)+ymin
          tt=(aa2-bb)* exp( -(-nlo*bb*y1)**(1d0/dble(nlo)) )
          costhe= -(tt -aa2)/bb
        ELSEIF(r3.le.pr(6)) THEN
          ymax= -(ymi1-yma1)**nlo2/(bb*nlo2)
          ymin=  0d0
          y1=drv1*(ymax-ymin)+ymin
          tt=(aa1-bb)* exp( -(-nlo2*bb*y1)**(1d0/dble(nlo2)) )
          costhe= (tt -aa1)/bb
        ELSEIF(r3.le.pr(7)) THEN
          ymax= -(ymi2-yma2)**nlo2/(bb*nlo2)
          ymin=  0d0
          y1=drv1*(ymax-ymin)+ymin
          tt=(aa2-bb)* exp( -(-nlo2*bb*y1)**(1d0/dble(nlo2)) )
          costhe= -(tt -aa2)/bb
        ELSEIF(r3.le.pr(8)) THEN
          y1=drv1*(tma1-tmi1) +tmi1
c          costhe= -(1/(y1*bb)+aa1)/bb
          ttt=(-1/(bb*ninv*y1))**(1d0/dble(ninv))
          costhe= -( -ttt +aa1)/bb
        ELSEIF(r3.le.pr(9)) THEN
          y1=drv1*(tma2-tmi2) +tmi2
c          costhe=  (1/(y1*bb)+aa2)/bb
          ttt=(-1/(bb*ninv*y1))**(1d0/dble(ninv))
          costhe=  ( -ttt +aa2)/bb
        ELSE
          write(6,*)'cosdec_br=> wrong total probability=',pr(7)
          stop
        ENDIF

        phi=2*pi*drv2

      IF(abs(costhe).GT.1d0) THEN
        WRITE(6,*)'TROUBLE in cosdec_br ==> costhe > 1 '
        WRITE(6,*)'cosdec_br: costhe,phi,p0=',costhe,phi,p0
        WRITE(6,*)'pp=',pp
        WRITE(6,*)'pinb1',pinb1
        WRITE(6,*)'pinb2',pinb2
        WRITE(6,*)'b12  ',b12
        WRITE(6,*)'dmas2(pinb1)',dmas2(pinb1),dmas2(pinb2)
        WRITE(6,*)'s12,s12,s1,s2',s12,dmas2(b12),s1,s2

        STOP
      ENDIF

      ENDIF
      IF(mode.EQ.0) THEN
        tpr1 = aa1+bb*costhe
        tpr2 = aa2-bb*costhe
C        write(6,*)'0 tpr1-2 ',tpr1,tpr2,costhe
      ELSEIF(mode.EQ.1) THEN
        do i=1,4
          pb1(i)=pinb1(i)-pout1(i)
          pb2(i)=pinb1(i)-pout2(i)
        enddo
        tpr1 = -dmas2(pb1)/2+amflt2/2d0
        tpr2 = -dmas2(pb2)/2+amflt2/2d0
        IF(tpr1.lt.amflt2/2) tpr1=amflt2/2
        IF(tpr2.lt.amflt2/2) tpr2=amflt2/2
C        write(6,*)'1 tpr1-2 ',tpr1,tpr2
      ENDIF

      crd_ang(mkrok,1) = 1d0 /(4*pi)
      crd_ang(mkrok,2) = 2/tpr1 /((yma1-ymi1)/bb)  /(4*pi)
      crd_ang(mkrok,3) = 2/tpr2 /((yma2-ymi2)/bb)  /(4*pi)
      crd_ang(mkrok,4) = 2*(ymi1-log(tpr1))**n_log/tpr1 
     @                    /((ymi1-yma1)**nlo/(-nlo*bb))  /(4*pi)
      crd_ang(mkrok,5) = 2*(ymi2-log(tpr2))**n_log/tpr2 
     @                    /((ymi2-yma2)**nlo/(-nlo*bb))  /(4*pi)
      crd_ang(mkrok,6) = 2*(ymi1-log(tpr1))**n_log2/tpr1 
     @                    /((ymi1-yma1)**nlo2/(-nlo2*bb))  /(4*pi)
      crd_ang(mkrok,7) = 2*(ymi2-log(tpr2))**n_log2/tpr2 
     @                    /((ymi2-yma2)**nlo2/(-nlo2*bb))  /(4*pi)
      crd_ang(mkrok,8) = 2/tpr1**n_inv /(tma1-tmi1)  /(4*pi)
      crd_ang(mkrok,9) = 2/tpr2**n_inv /(tma2-tmi2)  /(4*pi)

      xccos =0d0
      DO i=1,n_ang
        xccos= xccos +prob_ang(mkrok,i)*crd_ang(mkrok,i) 
      ENDDO

      wt= 1d0/xccos

      end

      SUBROUTINE make_phsp_point
     $      (msdump,label,ambeam,svar,sprim,fak_phsp,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
*     **************************************
* msdump=0 : generates the phase space point
* msdump=1 : point is red from the disk
* fak_phsp  : weight 
*     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER (mm_nrchan=65)
! it was mm_monit
      COMMON /c_phspm/
     $                 prob_mm(mm_nrchan),
     $                 faki_mon(mm_nrchan+3000),
     $                 ikan_mon
      SAVE /c_phspm/
CBB      INCLUDE 'mm_phsp.h'

      DIMENSION
     $      effbeam1(4),effbeam2(4),effp1(4),effp2(4),effp3(4),effp4(4)
      DIMENSION
     $      qeffp1(4),qeffp2(4),qeffp3(4),qeffp4(4)
      DIMENSION drvec(100)

      CALL mm_brancher(sprim,itype,prob_mm)

      IF(msdump .EQ. 1) THEN
         CALL READer(itype,effbeam1,effbeam2,effp1,effp2,effp3,effp4)
         CALL mm_spacegen(1,itype,ambeam,svar,sprim,fak_phsp,
     $                     effp1,effp2,effp3,effp4)
      ELSE
         CALL mm_spacegen(0,itype,ambeam,svar,sprim,fak_phsp,
     $                     qeffp1,qeffp2,qeffp3,qeffp4)
         CALL set_eff_beams(sprim,ambeam,effbeam1,effbeam2)
! permutations in the case of eeee ZZ state
         CALL linear_to_WZ_label(1,label,iwm,iwp,if_z,if_w)
         IF(if_z.EQ.1 .AND. iwm.EQ.6 .AND. iwp.EQ.6) THEN
c extend to all         IF(if_z.EQ.1 .AND. iwm.EQ.iwp) THEN
c masses not equal         IF( (if_z.EQ.1 .AND. (iwm.EQ.6 .OR. iwp.EQ.6)) .OR.
c masses not equal     @       (if_w.EQ.1 .AND. (iwm.EQ.7 .OR. iwp.EQ.7)) ) THEN
           CALL varran(drvec,1)
           IF(drvec(1).LE. .25d0) THEN
             mode=0
           ELSEIF(drvec(1).LE. .5d0) THEN
             mode=1
           ELSEIF(drvec(1).LE. .75d0) THEN
             mode=2
           ELSE
             mode=3
           ENDIF
           CALL perm_em(mode,qeffp1,qeffp2,qeffp3,qeffp4,
     @                    effp1,effp2,effp3,effp4 )
           itype =itype +1000*mode
         ELSE
! no permutations for non-eeee states
           CALL perm_em(0,qeffp1,qeffp2,qeffp3,qeffp4,
     @                    effp1,effp2,effp3,effp4 )
         ENDIF
      ENDIF

      ikan_mon=itype
      faki_mon(itype)=fak_phsp

      END

      SUBROUTINE get_phsp_weight
     $      (label,ambeam,svar,sprim,fak_tot,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
*     **************************************
* calculates jacobians of all channels and does resummation
* fak_tot  : total weight - the only output
*     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER (mm_nrchan=65)
! it was mm_monit
      COMMON /c_phspm/
     $                 prob_mm(mm_nrchan),
     $                 faki_mon(mm_nrchan+3000),
     $                 ikan_mon
      SAVE /c_phspm/
CBB      INCLUDE 'mm_phsp.h'

      DIMENSION
     $      effbeam1(4),effbeam2(4),effp1(4),effp2(4),effp3(4),effp4(4)
      DIMENSION
     $      qeffp1(4),qeffp2(4),qeffp3(4),qeffp4(4)

      fak=0d0
      DO i=1,mm_nrchan
         IF (prob_mm(i) .GT. 0d0) THEN
            IF (i .NE. ikan_mon) 
     $           CALL mm_spacegen(1,i,ambeam,svar,sprim,faki_mon(i),
     $                    effp1,effp2,effp3,effp4)
            IF (faki_mon(i) .GT. 0d0) THEN
               fak=fak+prob_mm(i)/faki_mon(i)
* m.s. 21.03, events with some faki_mon=0 should not be discarded
*            ELSE
*               fak=0d0
*               WRITE(6,*)'mm_phsp==> jacob=0 in chan.',i,' of ',ikan_mon
*               GOTO 150
* m.s. 21.03, events with some faki_mon=0 should not be discarded
            ENDIF
         ENDIF
      ENDDO
* 150  CONTINUE
! permutations in the case of eeee ZZ state
      CALL linear_to_WZ_label(1,label,iwm,iwp,if_z,if_w)
      IF(if_z.EQ.1 .AND. iwm.EQ.6 .AND. iwp.EQ.6) THEN
c extend to all      IF(if_z.EQ.1 .AND. iwm.EQ.iwp) THEN
c masses not equal      IF( (if_z.EQ.1 .AND. (iwm.EQ.6 .OR. iwp.EQ.6)) .OR.
c masses not equal     @    (if_w.EQ.1 .AND. (iwm.EQ.7 .OR. iwp.EQ.7)) ) THEN
       ip_act=ikan_mon/1000
       ikan_mon=mod(ikan_mon,1000)
       DO iper=1,3
        DO i=1,mm_nrchan
          IF (prob_mm(i) .GT. 0d0) THEN
            IF (i.NE.ikan_mon .OR. ip_act.NE.iper) THEN
! do permutations
              CALL perm_em(iper,effp1,effp2,effp3,effp4,
     @                    qeffp1,qeffp2,qeffp3,qeffp4 )
              CALL mm_spacegen(1,i,ambeam,svar,sprim,faki_mon(i),
     $                    qeffp1,qeffp2,qeffp3,qeffp4)
            ELSE
              faki_mon(i)=faki_mon(ikan_mon+ip_act*1000)
            ENDIF
            IF (faki_mon(i) .GT. 0d0) THEN
               fak=fak+prob_mm(i)/faki_mon(i)
            ENDIF
          ENDIF
        ENDDO
       ENDDO
       fak=fak/4d0
      ENDIF
      fak=1d0/fak

      fak_tot=fak

      END

      SUBROUTINE perm_em(mode,qeffp1,qeffp2,qeffp3,qeffp4,
     @                    effp1,effp2,effp3,effp4 )
!     *************************************************
! does very internal permutation for make_phsp_point /eeee state/
! qeffp --> effp
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION
     $      effp1(4),effp2(4),effp3(4),effp4(4)
      DIMENSION
     $      qeffp1(4),qeffp2(4),qeffp3(4),qeffp4(4)
      SAVE

      IF(mode .EQ. 0) THEN
! no permutation
        DO i=1,4
          effp1(i)=qeffp1(i)
          effp2(i)=qeffp2(i)
          effp3(i)=qeffp3(i)
          effp4(i)=qeffp4(i)
        ENDDO
      ELSEIF(mode .EQ. 1) THEN
! 1<->3
        DO i=1,4
          effp1(i)=qeffp3(i)
          effp2(i)=qeffp2(i)
          effp3(i)=qeffp1(i)
          effp4(i)=qeffp4(i)
        ENDDO
        itype=itype+1000
      ELSEIF(mode .EQ. 2) THEN
! 2<->4
        DO i=1,4
          effp1(i)=qeffp1(i)
          effp2(i)=qeffp4(i)
          effp3(i)=qeffp3(i)
          effp4(i)=qeffp2(i)
        ENDDO
        itype=itype+2000
      ELSEIF(mode .EQ. 3) THEN
! 1<->3, 2<->4
        DO i=1,4
          effp1(i)=qeffp3(i)
          effp2(i)=qeffp4(i)
          effp3(i)=qeffp1(i)
          effp4(i)=qeffp2(i)
        ENDDO
      ELSE
        WRITE(6,*)'perm_em=> wrong mode:',mode
        STOP
      ENDIF
      END

      SUBROUTINE pres_monit(mode,wtcrud,wtmod,wtset)
!     *************************************************
! monitors probabilities of different branches
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      SAVE
      PARAMETER (mm_nrchan=65)
! it was mm_monit
      COMMON /c_phspm/
     $                 prob_mm(mm_nrchan),
     $                 faki_mon(mm_nrchan+3000),
     $                 ikan_mon
      SAVE /c_phspm/
CBB      INCLUDE 'mm_phsp.h'

      DIMENSION densit(mm_nrchan),top(mm_nrchan),numb(mm_nrchan)
      DIMENSION densit_jac(mm_nrchan),top_jac(mm_nrchan)
      DIMENSION numb_jac(mm_nrchan)
      DIMENSION wtset(*)      

      DATA init /0/

      IF(init.eq.0)THEN
        init=1
        do i=1,mm_nrchan
          densit(i) = 0d0
          top(i) = 0d0
          numb(i) = 0
        enddo
      ENDIF

      IF(mode.EQ.0) THEN
        IF(wtmod.GT.0d0) THEN

        wtmod4f = wtset(40)
! 10/8/98 double counting of wtset(40)
!        wtt = wtmod*wtmod4f
        wtt = wtmod
! 10/8/98 ms

        densit(ikan_mon) = densit(ikan_mon) +wtt
        numb(ikan_mon) = numb(ikan_mon) +1
        IF(top(ikan_mon).LT.wtt) top(ikan_mon) = wtt 
!.. by jacobians
        imin=1
        DO i=1,mm_nrchan
          IF(faki_mon(imin).eq.0d0 .or. 
     $       faki_mon(i).gt.0d0 .and. faki_mon(i).lt.faki_mon(imin))
     $          imin=i 
        ENDDO
        densit_jac(imin) = densit_jac(imin) +wtt
        numb_jac(imin) = numb_jac(imin) +1
        IF(top_jac(imin).LT.wtt) top_jac(imin) = wtt 
!.. by jacobians
        ENDIF
      ELSEIF(mode.EQ.1) THEN

        dtop=0d0
        dtot=0d0

        do i=1,mm_nrchan
          if(numb(i).gt.0) densit(i) = densit(i)/numb(i)
          dtot = dtot +densit(i)
          if(top(i).gt.dtop) dtop =top(i) 
        enddo

        do i=1,mm_nrchan
          densit(i) = densit(i)/dtot
          if(densit(i).gt.0.02d0) then
        write(6,*) '      prob(',i,')=',densit(i),' !max=',top(i)/dtop
          endif
        enddo

        write(6,*)'all channels'

        do i=1,mm_nrchan
        write(6,*) '      prob(',i,')=',densit(i),' !max=',top(i)/dtop
        enddo
! by jacobians
        dtop=0d0
        dtot=0d0

        do i=1,mm_nrchan
          if(numb_jac(i).gt.0) densit_jac(i)=densit_jac(i)/numb_jac(i)
          dtot = dtot +densit_jac(i)
          if(top_jac(i).gt.dtop) dtop =top_jac(i) 
        enddo

        write(6,*)'by jacobians........'
        do i=1,mm_nrchan
          densit_jac(i) = densit_jac(i)/dtot
          if(densit_jac(i).gt.0.02d0) then
        write(6,*) '      prob(',i,')=',densit_jac(i)
     $            ,' !max=',top_jac(i)/dtop
          endif
        enddo

        write(6,*)'all channels'

        do i=1,mm_nrchan
        write(6,*) '      prob(',i,')=',densit_jac(i)
     $           ,' !max=',top_jac(i)/dtop
        enddo

      ELSE
        write(6,*)'pres_monit=> wrong mode',mode
        stop
      ENDIF

      END


      subroutine mm_dumper(nout2,nout,nevtru
     $                     ,wtovr,wtu,wtmax,wtmod,wtmod4f,iflav)    
*     **************************************************************     
c overweighted events monitoring, MS version
c 
c (Re)Written by: M.Skrzypek        date: 
c Last update:             by:  
c
      implicit DOUBLE PRECISION (a-h,o-z)
      PARAMETER (mm_nrchan=65)
! it was mm_monit
      COMMON /c_phspm/
     $                 prob_mm(mm_nrchan),
     $                 faki_mon(mm_nrchan+3000),
     $                 ikan_mon
      SAVE /c_phspm/
CBB      INCLUDE 'mm_phsp.h'

      COMMON / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot
      COMMON / cms_eff_momdec /
     $      effbeam1(4),effbeam2(4),p1(4),p2(4),p3(4),p4(4)
      SAVE   / momset /,/ cms_eff_momdec /

      dimension q12(4),q13(4),q14(4),q23(4),q24(4),q34(4),qtot(4)
      dimension q123(4),q124(4),q134(4),q234(4)
      dimension iflav(4)

      write(nout,*) '===OVERVEIGHTED EVT. NR: NEVTRU=',NEVTRU,'===='
      write(nout,*) '===generated with channel: ikan=',ikan_mon,'===='
      CALL DUMPL(nout,P1,P2,P3,P4,effbeam1,effbeam2,sphot,nphot)
c      write(nout,*) 'Note: this event should be outside your detector'
c      write(nout,*) 'if not, increase wtmax (?) and/or check manual ?'
c      write(nout,*) 'final state: ',IFLAV
      write(nout,*) 'wtu= wtmod*wtmod4f/wtmax=',wtovr/wtmax
      write(nout,*) 'wtu_max=                 ',wtu,'    ... so far'
      write(nout,*) '-----------'
      write(nout,*) ' wtmod4f=',wtmod4f,' wtmod=',wtmod

      CALL writer(nout2,ikan_mon,effbeam1,effbeam2,p1,p2,p3,p4)

      do k=1,4
        q12(k)=p1(k)+p2(k)-effbeam1(k)
        q13(k)=p1(k)+p3(k)-effbeam1(k)
        q14(k)=p1(k)+p4(k)-effbeam1(k)
        q23(k)=p2(k)+p3(k)-effbeam1(k)
        q24(k)=p2(k)+p4(k)-effbeam1(k)
        q34(k)=p3(k)+p4(k)-effbeam1(k)

        q123(k)=p1(k)+p2(k)+p3(k)-effbeam1(k)
        q124(k)=p1(k)+p2(k)+p4(k)-effbeam1(k)
        q134(k)=p1(k)+p3(k)+p4(k)-effbeam1(k)
        q234(k)=p2(k)+p3(k)+p4(k)-effbeam1(k)

      enddo
      xm12=sqrt(abs(dmas2(q12)))
      xm13=sqrt(abs(dmas2(q13)))
      xm14=sqrt(abs(dmas2(q14)))
      xm23=sqrt(abs(dmas2(q23)))
      xm24=sqrt(abs(dmas2(q24)))
      xm34=sqrt(abs(dmas2(q34)))

      xm123=sqrt(abs(dmas2(q123)))
      xm124=sqrt(abs(dmas2(q124)))
      xm134=sqrt(abs(dmas2(q134)))
      xm234=sqrt(abs(dmas2(q234)))

      write(nout,'(3(A,G14.4))') 't12 =',real(xm12),'t13 =',real(xm13)
     $              ,'t14 =',real(xm14)
      write(nout,'(3(A,G14.4))') 't23 =',real(xm23),'t24 =',real(xm24)
     $              ,'t34 =',real(xm34)
      write(nout,'(4(A,G14.4))')'t123=',real(xm123),'t124=',real(xm124)
     $              ,'t134=',real(xm134),'t234=',real(xm234) 
      do k=1,4

        q123(k)=p4(k)-effbeam1(k)
        q124(k)=p3(k)-effbeam1(k)
        q134(k)=p2(k)-effbeam1(k)
        q234(k)=p1(k)-effbeam1(k)

      enddo

      xm123=sqrt(abs(dmas2(q123)))
      xm124=sqrt(abs(dmas2(q124)))
      xm134=sqrt(abs(dmas2(q134)))
      xm234=sqrt(abs(dmas2(q234)))

      write(nout,'(4(A,G14.4))')'t4  =',real(xm123),'t3  =',real(xm124)
     $         ,'t2  =',real(xm134),'t1  =',real(xm234 )
      do k=1,4
        q12(k)=p1(k)+p2(k)
        q13(k)=p1(k)+p3(k)
        q14(k)=p1(k)+p4(k)
        q23(k)=p2(k)+p3(k)
        q24(k)=p2(k)+p4(k)
        q34(k)=p3(k)+p4(k)

        q123(k)=p1(k)+p2(k)+p3(k)
        q124(k)=p1(k)+p2(k)+p4(k)
        q134(k)=p1(k)+p3(k)+p4(k)
        q234(k)=p2(k)+p3(k)+p4(k)

        qtot(k)=p1(k)+p2(k)+p3(k)+p4(k)
      enddo
      xm12=sqrt(dmas2(q12))
      xm13=sqrt(dmas2(q13))
      xm14=sqrt(dmas2(q14))
      xm23=sqrt(dmas2(q23))
      xm24=sqrt(dmas2(q24))
      xm34=sqrt(dmas2(q34))

      xm123=sqrt(dmas2(q123))
      xm124=sqrt(dmas2(q124))
      xm134=sqrt(dmas2(q134))
      xm234=sqrt(dmas2(q234))

      xmtot=sqrt(dmas2(qtot))
      write(nout,'(3(A,G14.4))') ' m12 =',real(xm12)
     $       ,' m13 =',real(xm13),' m14 =',real(xm14)
      write(nout,'(3(A,G14.4))') ' m23 =',real(xm23)
     $       ,' m24 =',real(xm24),' m34 =',real(xm34)
      write(nout,'(4(A,G14.4))') ' m123=',real(xm123)
     $       ,' m124=',real(xm124),' m134=',real(xm134)
     $                 ,' m234=',real(xm234) 
      write(nout,'(A,G14.4)') ' m1234=',real(xmtot)

      cthe1=cos(angle(effbeam1,p1))
      cthe2=cos(angle(effbeam1,p2))
      cthe3=cos(angle(effbeam1,p3))
      cthe4=cos(angle(effbeam1,p4))
      write(nout,3100)
     @    'decay c1e-,c2-n,c3n,c4e+ ',cthe1,cthe2,cthe3,cthe4

      DO i = 1,mm_nrchan,5
        write(nout,'(A7,I3,A1,I3,A1,5g16.7)')'jacob. ',i,'-',i+4,'=',
     $  real(faki_mon(i)),real(faki_mon(i+1)),real(faki_mon(i+2))
     $               ,real(faki_mon(i+3)),real(faki_mon(i+4))
      ENDDO
       write(nout,*) '        ============================'

!ms        WLAMBD=SPRIM**2+S1**2+S2**2-2*SPRIM*S1-2*SPRIM*S2-2*S1*S2
cc        wlambd=max(0d0,(sprim-s1-s2)**2 -4*s1*s2)
cc        T=-(SPRIM-S1-S2-DSQRT(WLAMBD)*COSTHE)/2
c        write(nout,*) 'channel ',IFLAV(4)
c        write(nout,*) ' wtmod4f=',real(wtmod4f),'wtmod=',real(wtmod)
cc        write(nout,*) ' log t=',real(dlog(-t))
cc        write(nout,*) 'costhe=',real(costhe)
c     @             ,' sqrt(wlambd)/sprim=',real(dsqrt(wlambd)/sprim)
c      write(nout,*)'sqrt s1=',real(sqrt(s1)),'sqrt s2=',real(sqrt(s2))
c     @            ,' sqrt sprim=',real(dsqrt(sprim))
c        write(nout,*)
c     @            '                        ',cthe3,cthe4
 3100 format(a26,4g22.14)   

      end

      subroutine ww_dumper(nout,svar,amel,wtcort)    
*     *******************************************    
c overweighted events monitoring, MS version
c 
c (Re)Written by: M.Skrzypek        date: 
c Last update:             by:  
c
      implicit DOUBLE PRECISION (a-h,o-z)
      PARAMETER (mm_nrchan=65)
! it was mm_monit
      COMMON /c_phspm/
     $                 prob_mm(mm_nrchan),
     $                 faki_mon(mm_nrchan+3000),
     $                 ikan_mon
      SAVE /c_phspm/
CBB      INCLUDE 'mm_phsp.h'

      COMMON / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      COMMON / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot
      SAVE   / momset /,/ momdec /

      dimension q12(4),q13(4),q14(4),q23(4),q24(4),q34(4),qtot(4)
      dimension q123(4),q124(4),q134(4),q234(4)
      dimension effbeam1(4),effbeam2(4)

      CALL set_eff_beams(svar,amel,effbeam1,effbeam2)

      write(nout,*) '================ WW_DUMPER ==================='
      write(nout,*) '--> wtcort = ',wtcort
      write(nout,*) '-----------'
      CALL DUMPL(nout,P1,P2,P3,P4,effbeam1,effbeam2,sphot,nphot)

      do k=1,4
        q12(k)=p1(k)+p2(k)-effbeam1(k)
        q13(k)=p1(k)+p3(k)-effbeam1(k)
        q14(k)=p1(k)+p4(k)-effbeam1(k)
        q23(k)=p2(k)+p3(k)-effbeam1(k)
        q24(k)=p2(k)+p4(k)-effbeam1(k)
        q34(k)=p3(k)+p4(k)-effbeam1(k)

        q123(k)=p1(k)+p2(k)+p3(k)-effbeam1(k)
        q124(k)=p1(k)+p2(k)+p4(k)-effbeam1(k)
        q134(k)=p1(k)+p3(k)+p4(k)-effbeam1(k)
        q234(k)=p2(k)+p3(k)+p4(k)-effbeam1(k)

      enddo
      xm12=sqrt(abs(dmas2(q12)))
      xm13=sqrt(abs(dmas2(q13)))
      xm14=sqrt(abs(dmas2(q14)))
      xm23=sqrt(abs(dmas2(q23)))
      xm24=sqrt(abs(dmas2(q24)))
      xm34=sqrt(abs(dmas2(q34)))

      xm123=sqrt(abs(dmas2(q123)))
      xm124=sqrt(abs(dmas2(q124)))
      xm134=sqrt(abs(dmas2(q134)))
      xm234=sqrt(abs(dmas2(q234)))

      write(nout,'(3(A,G14.4))') 't12 =',real(xm12),'t13 =',real(xm13)
     $              ,'t14 =',real(xm14)
      write(nout,'(3(A,G14.4))') 't23 =',real(xm23),'t24 =',real(xm24)
     $              ,'t34 =',real(xm34)
      write(nout,'(4(A,G14.4))')'t123=',real(xm123),'t124=',real(xm124)
     $              ,'t134=',real(xm134),'t234=',real(xm234) 
      do k=1,4

        q123(k)=p4(k)-effbeam1(k)
        q124(k)=p3(k)-effbeam1(k)
        q134(k)=p2(k)-effbeam1(k)
        q234(k)=p1(k)-effbeam1(k)

      enddo

      xm123=sqrt(abs(dmas2(q123)))
      xm124=sqrt(abs(dmas2(q124)))
      xm134=sqrt(abs(dmas2(q134)))
      xm234=sqrt(abs(dmas2(q234)))

      write(nout,'(4(A,G14.4))')'t4- =',real(xm123),'t3- =',real(xm124)
     $         ,'t2- =',real(xm134),'t1- =',real(xm234 )

      do k=1,4

        q123(k)=p4(k)-effbeam2(k)
        q124(k)=p3(k)-effbeam2(k)
        q134(k)=p2(k)-effbeam2(k)
        q234(k)=p1(k)-effbeam2(k)

      enddo

      xm123=sqrt(abs(dmas2(q123)))
      xm124=sqrt(abs(dmas2(q124)))
      xm134=sqrt(abs(dmas2(q134)))
      xm234=sqrt(abs(dmas2(q234)))

      write(nout,'(4(A,G14.4))')'t4+ =',real(xm123),'t3+ =',real(xm124)
     $         ,'t2+  =',real(xm134),'t1+  =',real(xm234 )

      do k=1,4
        q12(k)=p1(k)+p2(k)
        q13(k)=p1(k)+p3(k)
        q14(k)=p1(k)+p4(k)
        q23(k)=p2(k)+p3(k)
        q24(k)=p2(k)+p4(k)
        q34(k)=p3(k)+p4(k)

        q123(k)=p1(k)+p2(k)+p3(k)
        q124(k)=p1(k)+p2(k)+p4(k)
        q134(k)=p1(k)+p3(k)+p4(k)
        q234(k)=p2(k)+p3(k)+p4(k)

        qtot(k)=p1(k)+p2(k)+p3(k)+p4(k)
      enddo
      xm12=sqrt(dmas2(q12))
      xm13=sqrt(dmas2(q13))
      xm14=sqrt(dmas2(q14))
      xm23=sqrt(dmas2(q23))
      xm24=sqrt(dmas2(q24))
      xm34=sqrt(dmas2(q34))

      xm123=sqrt(dmas2(q123))
      xm124=sqrt(dmas2(q124))
      xm134=sqrt(dmas2(q134))
      xm234=sqrt(dmas2(q234))

      xmtot=sqrt(dmas2(qtot))
      write(nout,'(3(A,G14.4))') ' m12 =',real(xm12)
     $       ,' m13 =',real(xm13),' m14 =',real(xm14)
      write(nout,'(3(A,G14.4))') ' m23 =',real(xm23)
     $       ,' m24 =',real(xm24),' m34 =',real(xm34)
      write(nout,'(4(A,G14.4))') ' m123=',real(xm123)
     $       ,' m124=',real(xm124),' m134=',real(xm134)
     $                 ,' m234=',real(xm234) 
      write(nout,'(A,G14.4)') ' m1234=',real(xmtot)

      cthe1=cos(angle(effbeam1,p1))
      cthe2=cos(angle(effbeam1,p2))
      cthe3=cos(angle(effbeam1,p3))
      cthe4=cos(angle(effbeam1,p4))
      write(nout,3100)
     @    'decay c1e-,c2-n,c3n,c4e+ ',cthe1,cthe2,cthe3,cthe4

       write(nout,*) '        ============================'

 3100 format(a26,4g22.14)   

      end

      SUBROUTINE set_param_WW(prob_ang,prob_mass,rmas,rgam,
     $                    amreg2_mas,amreg2_ang,itype,i_flav)
!     *************************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      SAVE
      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      DIMENSION i_flav(4)
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW 
!      DATA init /0/

!         BR(1)  <== ud
!         BR(2)  <== cd
!         BR(3)  <== us
!         BR(4)  <== cs
!         BR(5)  <== ub
!         BR(6)  <== cb
!         BR(7)  <== e
!         BR(8)  <== mu
!         BR(9)  <== tau

!      IF(init.EQ.0) THEN
!        init=1
! generalities
      ambeam=amel
      amreg2_mas=    ambeam**2/10
      amreg2_ang=    ambeam**3/10
      rmas(1)=amaw
      rmas(2)=amaz
      rgam(1)=gammw
      rgam(2)=gammz

      DO itp=0,m_step
        prob_ang(itp,6)=0d0  !(log t)**2/t
        prob_ang(itp,7)=0d0  !(log u)**2/u
        prob_ang(itp,8)=0d0  !1/t**2
        prob_ang(itp,9)=0d0  !1/u**2
      ENDDO
      prob_mass(5)=0d0

!      ENDIF

!     =======================================================
      IF(abs(i_flav(1)).NE.11 .AND. abs(i_flav(2)).NE.11 .AND. 
     @   abs(i_flav(3)).NE.11 .AND. abs(i_flav(4)).NE.11 ) THEN 
!     ==============
!  1-1 type channels
!     =======================================================
        DO itp=0,m_step
          prob_ang(itp,1)=3/10d0     !flat
          prob_ang(itp,2)=3/10d0     !1/t
          prob_ang(itp,3)=3/10d0     !1/u
          prob_ang(itp,4)=.5d0/10d0  !ln t/t
          prob_ang(itp,5)=.5d0/10d0  !ln u/u
        ENDDO
        IF(itype.eq.10)THEN
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=2/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=7/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=3/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=3/10d0     !flat
            prob_ang(itp,2)=3d0/10d0     !1/t
            prob_ang(itp,3)=3d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ENDIF
          ENDDO        
        ELSEIF(itype.eq.13)THEN
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=3/16d0  !ln s/s
          prob_mass(4)=0/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=8/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=3/10d0     !flat
            prob_ang(itp,2)=3d0/10d0     !1/t
            prob_ang(itp,3)=3d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ENDIF
          ENDDO        
        ELSEIF(itype.eq.38. or. itype.eq.45)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=2d0/10d0
            prob_ang(itp,3)=2d0/10d0
            prob_ang(itp,4)=2d0/10d0
            prob_ang(itp,5)=2d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=2d0/10d0
            prob_ang(itp,3)=2d0/10d0
            prob_ang(itp,4)=2d0/10d0
            prob_ang(itp,5)=2d0/10d0
            ENDIF
          ENDDO        
          prob_mass(1)=4d0/16d0  !flat
          prob_mass(2)=6d0/16d0  !1/s
          prob_mass(3)=2d0/16d0  !ln s/s
          prob_mass(4)=0d0/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=4d0/16d0  !Z
        ELSE
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=3/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=3/16d0  !1/sma-s
          prob_mass(6)=3/16d0  !W
          prob_mass(7)=3/16d0  !Z
        ENDIF
!     =======================================================
      ELSEIF( (abs(i_flav(1)).EQ.11 .AND. abs(i_flav(2)).NE.11 
     @   .AND. abs(i_flav(3)).NE.11 .AND. abs(i_flav(4)).NE.11)
     @      ) THEN 
!     ===============
!  7-1, type channels
!     =======================================================
        DO itp=0,m_step
          prob_ang(itp,1)=3/10d0     !flat
          prob_ang(itp,2)=2d0/10d0     !1/t
          prob_ang(itp,3)=2d0/10d0     !1/u
          prob_ang(itp,4)=1.5d0/10d0  !ln t/t
          prob_ang(itp,5)=1.5d0/10d0  !ln u/u
        ENDDO
        IF(itype.eq.9)THEN
          prob_mass(1)=4/16d0  !flat
          prob_mass(2)=4/16d0  !1/s
          prob_mass(3)=4/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=3/16d0  !Z
        ELSEIF(itype.eq.17)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=3.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=3.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            ENDIF        
          ENDDO
          prob_mass(1)=7d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=1d0/16d0  !ln s/s
          prob_mass(4)=2d0/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=3d0/16d0  !Z
        ELSEIF(itype.eq.18)THEN
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=.5d0/16d0  !1/sma-s
          prob_mass(6)=6d0/16d0  !W
          prob_mass(7)=2.5/16d0  !Z
        ELSEIF(itype.eq.43 .or. itype.eq.55)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=5d0/16d0  !flat
          prob_mass(2)=.5d0/16d0  !1/s
          prob_mass(3)=0d0/16d0  !ln s/s
          prob_mass(4)=1d0/16d0  !1/sma-s
          prob_mass(6)=6d0/16d0  !W
          prob_mass(7)=3.5d0/16d0  !Z
        ELSE
          DO itp=0,m_step
            prob_ang(itp,1)=3/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
          ENDDO
          prob_mass(1)=3/16d0  !flat
          prob_mass(2)=2/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=4/16d0  !1/sma-s
          prob_mass(6)=3/16d0  !W
          prob_mass(7)=2/16d0  !Z
        ENDIF
      ELSEIF( (abs(i_flav(1)).NE.11 .AND. abs(i_flav(2)).NE.11
     @   .AND. abs(i_flav(3)).NE.11 .AND. abs(i_flav(4)).EQ.11)
     @      ) THEN 
!     =======================================================
!  1-7 type channels
        DO itp=0,m_step
          prob_ang(itp,1)=3/10d0     !flat
          prob_ang(itp,2)=2d0/10d0     !1/t
          prob_ang(itp,3)=2d0/10d0     !1/u
          prob_ang(itp,4)=1.5d0/10d0  !ln t/t
          prob_ang(itp,5)=1.5d0/10d0  !ln u/u
        ENDDO
        IF(itype.eq.12)THEN
          prob_mass(1)=4/16d0  !flat
          prob_mass(2)=4/16d0  !1/s
          prob_mass(3)=4/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=3/16d0  !Z
        ELSEIF(itype.eq.34)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t  
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=3.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=0.5d0/10d0
            prob_ang(itp,3)=3.5d0/10d0
            prob_ang(itp,4)=0.5d0/10d0
            prob_ang(itp,5)=3.5d0/10d0
            ENDIF        
          ENDDO
          prob_mass(1)=5d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=1d0/16d0  !ln s/s
          prob_mass(4)=2d0/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=5d0/16d0  !Z
        ELSEIF(itype.eq.33)THEN
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=.5d0/16d0  !1/sma-s
          prob_mass(6)=6d0/16d0  !W
          prob_mass(7)=2.5/16d0  !Z
        ELSEIF(itype.eq.56 .or. itype.eq.44)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=6d0/16d0  !flat
          prob_mass(2)=.5d0/16d0  !1/s
          prob_mass(3)=0d0/16d0  !ln s/s
          prob_mass(4)=1d0/16d0  !1/sma-s
          prob_mass(6)=6d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSE
          DO itp=0,m_step
            prob_ang(itp,1)=3/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=3/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=4/16d0  !1/sma-s
          prob_mass(6)=3/16d0  !W
          prob_mass(7)=2/16d0  !Z
        ENDIF
      ELSEIF(abs(i_flav(1)).EQ.11 .AND. abs(i_flav(2)).NE.11 .AND. 
     @   abs(i_flav(3)).NE.11 .AND. abs(i_flav(4)).EQ.11 ) THEN 
!     =======================================================
!  7-7 type channels
!  22.05.97  re-optimized 10,13,34,38,42,54,61
        DO itp=0,m_step
          prob_ang(itp,1)=3/10d0     !flat
          prob_ang(itp,2)=2d0/10d0     !1/t
          prob_ang(itp,3)=2d0/10d0     !1/u
          prob_ang(itp,4)=1.5d0/10d0  !ln t/t
          prob_ang(itp,5)=1.5d0/10d0  !ln u/u
        ENDDO
        IF(itype.eq.13)THEN
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=.5d0/16d0  !1/s
          prob_mass(3)=3.5d0/16d0  !ln s/s
          prob_mass(4)=8/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=2/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=3d0/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=8d0/10d0
            prob_ang(itp,2)=.5d0/10d0
            prob_ang(itp,3)=.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=8d0/10d0
            prob_ang(itp,2)=.5d0/10d0
            prob_ang(itp,3)=.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ENDIF
          ENDDO        
        ELSEIF(itype.eq.54)THEN
          prob_mass(1)=1.5d0/16d0  !flat
          prob_mass(2)=4d0/16d0  !1/s
          prob_mass(3)=5d0/16d0  !ln s/s
          prob_mass(4)=3d0/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=2.5/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2d0/10d0     !flat
            prob_ang(itp,2)=6.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=5.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ENDIF
          ENDDO        
        ELSEIF(itype.eq.10)THEN
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=3/16d0  !ln s/s
          prob_mass(4)=0/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=8/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2d0/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=6.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=6.5d0/10d0
            prob_ang(itp,3)=.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7d0/10d0
            prob_ang(itp,2)=1d0/10d0
            prob_ang(itp,3)=1d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ENDIF
          ENDDO        
        ELSEIF(itype.eq.57 .or. itype.eq.21 
     $        .or. itype.eq.45)THEN
          DO itp=0,m_step
            prob_ang(itp,1)=3/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
          ENDDO
          prob_mass(1)=5/16d0  !flat
          prob_mass(2)=1/16d0  !1/s
          prob_mass(3)=6/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=3/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2d0/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=6.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=6.5d0/10d0
            prob_ang(itp,3)=.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=1d0/10d0
            prob_ang(itp,3)=1d0/10d0
            prob_ang(itp,4)=0d0/10d0
            prob_ang(itp,5)=6d0/10d0
            ENDIF
          ENDDO        
        ELSEIF(itype.eq.33)THEN
          DO itp=0,m_step
            prob_ang(itp,1)=3/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
          ENDDO
          prob_mass(1)=5/16d0  !flat
          prob_mass(2)=1/16d0  !1/s
          prob_mass(3)=6/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=3/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=6.5d0/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=6.5d0/10d0
            prob_ang(itp,3)=.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1d0/10d0
            prob_ang(itp,3)=1d0/10d0
            prob_ang(itp,4)=0d0/10d0
            prob_ang(itp,5)=2d0/10d0
            ENDIF
          ENDDO        
!!!        ELSEIF(itype.eq.38. or. itype.eq.45)THEN
        ELSEIF(itype.eq.38)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=4.5d0/10d0
            prob_ang(itp,3)=.5d0/10d0
            prob_ang(itp,4)=2.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=2d0/10d0
            prob_ang(itp,3)=2d0/10d0
            prob_ang(itp,4)=0d0/10d0
            prob_ang(itp,5)=0d0/10d0
            ENDIF
          ENDDO        
          prob_mass(1)=4d0/16d0  !flat
          prob_mass(2)=2d0/16d0  !1/s
          prob_mass(3)=0d0/16d0  !ln s/s
          prob_mass(4)=6d0/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=4d0/16d0  !Z
        ELSEIF(itype.eq.9)THEN
          prob_mass(1)=4/16d0  !flat
          prob_mass(2)=4/16d0  !1/s
          prob_mass(3)=4/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=3/16d0  !Z
        ELSEIF(itype.eq.17)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=3.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=3.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            ENDIF        
          ENDDO
          prob_mass(1)=7d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=1d0/16d0  !ln s/s
          prob_mass(4)=2d0/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=3d0/16d0  !Z
        ELSEIF(itype.eq.18)THEN
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=.5d0/16d0  !1/sma-s
          prob_mass(6)=6d0/16d0  !W
          prob_mass(7)=2.5/16d0  !Z
        ELSEIF(itype.eq.43 .or. itype.eq.55)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=6d0/16d0  !flat
          prob_mass(2)=.5d0/16d0  !1/s
          prob_mass(3)=0d0/16d0  !ln s/s
          prob_mass(4)=1d0/16d0  !1/sma-s
          prob_mass(6)=6d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itype.eq.12)THEN
          prob_mass(1)=4/16d0  !flat
          prob_mass(2)=4/16d0  !1/s
          prob_mass(3)=4/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=3/16d0  !Z
        ELSEIF(itype.eq.34)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=6/10d0     !flat
            prob_ang(itp,2)=1.5d0/10d0     !1/t  
            prob_ang(itp,3)=1.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=3.5d0/10d0
            prob_ang(itp,4)=0.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=0.5d0/10d0
            prob_ang(itp,5)=5.5d0/10d0
            ENDIF        
          ENDDO
          prob_mass(1)=7d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=1d0/16d0  !ln s/s
          prob_mass(4)=2d0/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=3d0/16d0  !Z
        ELSEIF(itype.eq.33)THEN
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=.5d0/16d0  !1/sma-s
          prob_mass(6)=6d0/16d0  !W
          prob_mass(7)=2.5/16d0  !Z
        ELSEIF(itype.eq.56 .or. itype.eq.44)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=6d0/16d0  !flat
          prob_mass(2)=.5d0/16d0  !1/s
          prob_mass(3)=0d0/16d0  !ln s/s
          prob_mass(4)=1d0/16d0  !1/sma-s
          prob_mass(6)=6d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itype.eq.61)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=3.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=3.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat !!!!!!!
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=4/16d0  !flat
          prob_mass(2)=1/16d0  !1/s
          prob_mass(3)=0/16d0  !ln s/s
          prob_mass(4)=5/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=6/16d0  !Z
        ELSEIF(itype.eq.42)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=6.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=6.5d0/10d0     !1/u   !!!!!
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat 
            prob_ang(itp,2)=4d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=4/16d0  !flat
          prob_mass(2)=5/16d0  !1/s
          prob_mass(3)=0/16d0  !ln s/s
          prob_mass(4)=5/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=2/16d0  !Z
        ELSE
          DO itp=0,m_step
            prob_ang(itp,1)=3/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
          ENDDO
          prob_mass(1)=3/16d0  !flat
          prob_mass(2)=3/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=3/16d0  !1/sma-s
          prob_mass(6)=3/16d0  !W
          prob_mass(7)=2/16d0  !Z
        ENDIF
      ELSE
!     ====
        WRITE(6,*)'set_param_WW=> not set for i_flav:',i_flav
        STOP
      ENDIF
!     =====
!!!!!!!!!!!!!!!!!!!! short circuit !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!! short circuit !!!!!!!!!!!!!!!!!!!!!!!!
c          DO itp=0,m_step
c            prob_ang(itp,1)=3/10d0     !flat
c            prob_ang(itp,2)=2d0/10d0     !1/t
c            prob_ang(itp,3)=2d0/10d0     !1/u
c            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
c            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
c          ENDDO
c          prob_mass(1)=6/16d0  !flat
c          prob_mass(2)=1/16d0  !1/s
c          prob_mass(3)=6/16d0  !ln s/s
c          prob_mass(4)=1/16d0  !1/sma-s
c          prob_mass(6)=0/16d0  !W
c          prob_mass(7)=1/16d0  !Z
!!!!!!!!!!!!!!!!!!!! short circuit !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!! short circuit !!!!!!!!!!!!!!!!!!!!!!!!
!!! checks on probabilities !!!
      icheck=1
      IF(icheck.EQ.1) THEN
        DO i=0,m_step
          pang=0d0
          DO j=1,n_ang
            pang=pang+prob_ang(i,j)
          ENDDO
          IF(abs(pang-1).gt. 1d-14) THEN
            WRITE(6,*)
     @  'set_param_WW=> wrong ang. prob.: itype,step,sum=',itype,j,pang
            STOP
          ENDIF
        ENDDO
        pang=0d0
        DO i=1,n_mass
          pang=pang+prob_mass(i)
        ENDDO
        IF(abs(pang-1).gt. 1d-14) THEN
          WRITE(6,*)
     @      'set_param_WW=> wrong mass prob.: itype,sum=',itype,pang
          STOP
        ENDIF
      ENDIF

      END
      SUBROUTINE set_param_ZZ(prob_ang,prob_mass,rmas,rgam,
     $                    amreg2_mas,amreg2_ang,itype,i_flav)
!     *************************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      SAVE
      PARAMETER (n_ang=9,n_mass=7,n_m_res=2,m_step=2)
      DIMENSION prob_ang(0:m_step,n_ang)
      DIMENSION rmas(n_m_res),rgam(n_m_res),prob_mass(n_mass)
      DIMENSION i_flav(4)
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW 
!      DATA init /0/

!         BR(1)  <== u
!         BR(2)  <== d
!         BR(3)  <== s
!         BR(4)  <== c
!         BR(5)  <== b
!         BR(6)  <== e
!         BR(7)  <== mu
!         BR(8)  <== tau
!         BR(9)  <== ve
!         BR(10)  <== vmu
!         BR(11)  <== vtau

!      IF(init.EQ.0) THEN
!        init=1
! generalities
      ambeam=amel
      amreg2_mas=    ambeam**2/10
      amreg2_ang=    ambeam**3
      rmas(1)=amaw
      rmas(2)=amaz
      rgam(1)=gammw
      rgam(2)=gammz

      itpe_loc=itype
      IF( (abs(i_flav(1)).NE.11 .AND. abs(i_flav(2)).NE.11
     @   .AND. abs(i_flav(3)).EQ.11 .AND. abs(i_flav(4)).EQ.11)
     @      ) THEN 
        itpe_loc=flip_flop(itype)
      ELSE
        itpe_loc=itype
      ENDIF

      DO itp=0,m_step
        prob_ang(itp,6)=0d0  !log**2t/t
        prob_ang(itp,7)=0d0  !log**2u/u
        prob_ang(itp,8)=0d0  !1/t**2
        prob_ang(itp,9)=0d0  !1/u**2
      ENDDO
      prob_mass(5)=0d0

!      ENDIF

      IF(abs(i_flav(1)).NE.11 .AND. abs(i_flav(2)).NE.11 .AND. 
     @   abs(i_flav(3)).NE.11 .AND. abs(i_flav(4)).NE.11 ) THEN 
!     ==============
!  1-1 type channels
!     =======================================================
        DO itp=0,m_step
          prob_ang(itp,1)=3/10d0     !flat
          prob_ang(itp,2)=3/10d0     !1/t
          prob_ang(itp,3)=3/10d0     !1/u
          prob_ang(itp,4)=.5d0/10d0  !ln t/t
          prob_ang(itp,5)=.5d0/10d0  !ln u/u
        ENDDO
        IF(itpe_loc.eq.8)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=4.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=4.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=8/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=0d0/10d0  !ln t/t
            prob_ang(itp,5)=0d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=5/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=0/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=8/16d0  !Z
        ELSEIF(itpe_loc.eq.11)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=6.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=5.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=5/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=0/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=8/16d0  !Z
        ELSEIF(itpe_loc.eq.10)THEN
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=2/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=5/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=5/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=3/10d0     !flat
            prob_ang(itp,2)=3d0/10d0     !1/t
            prob_ang(itp,3)=3d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ENDIF
          ENDDO        
        ELSEIF(itpe_loc.eq.13)THEN
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=3/16d0  !ln s/s
          prob_mass(4)=0/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=8/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=3/10d0     !flat
            prob_ang(itp,2)=3d0/10d0     !1/t
            prob_ang(itp,3)=3d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=1.5d0/10d0
            prob_ang(itp,4)=.5d0/10d0
            prob_ang(itp,5)=.5d0/10d0
            ENDIF
          ENDDO        
        ELSEIF(itpe_loc.eq.38)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=2d0/10d0
            prob_ang(itp,3)=2d0/10d0
            prob_ang(itp,4)=2d0/10d0
            prob_ang(itp,5)=2d0/10d0
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=2d0/10d0
            prob_ang(itp,3)=2d0/10d0
            prob_ang(itp,4)=2d0/10d0
            prob_ang(itp,5)=2d0/10d0
            ENDIF
          ENDDO        
          prob_mass(1)=4d0/16d0  !flat
          prob_mass(2)=6d0/16d0  !1/s
          prob_mass(3)=2d0/16d0  !ln s/s
          prob_mass(4)=0d0/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=4d0/16d0  !Z
        ELSEIF(itpe_loc.eq.43 .or. itpe_loc.eq.56)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=4.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            prob_ang(itp,8)=1d0/10d0  !1/t**2
            prob_ang(itp,9)=1d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=5d0/16d0  !1/s
          prob_mass(3)=4d0/16d0  !ln s/s
          prob_mass(4)=1d0/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=4d0/16d0  !Z
        ELSEIF(itpe_loc.eq.40. or. itpe_loc.eq.59)THEN
      amreg2_mas=    ambeam**2/10
      amreg2_ang=    ambeam**3*50
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=6d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=8/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=3.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.22 .or. itpe_loc.eq.29)THEN
      amreg2_mas=    ambeam**2/10
      amreg2_ang=    ambeam**3*50
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=6/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=4d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6/10d0     !flat
            prob_ang(itp,2)=1.5d0/10d0     !1/t
            prob_ang(itp,3)=1.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=8/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=3.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.52 .or. itpe_loc.eq.47)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=7.5d0/10d0     !1/u
            prob_ang(itp,4)=0d0/10d0  !ln t/t
            prob_ang(itp,5)=0d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=1.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=5.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=5/16d0  !1/s
          prob_mass(3)=4/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=4d0/16d0  !Z
        ELSEIF(itpe_loc.eq.34 .or. itpe_loc.eq.17)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=5/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=3d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=6.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=6/10d0     !flat
            prob_ang(itp,2)=1.5d0/10d0     !1/t
            prob_ang(itp,3)=1.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=8/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=4d0/16d0  !Z
        ELSEIF(itpe_loc.eq.55 .or. itpe_loc.eq.44)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=6/10d0     !flat
            prob_ang(itp,2)=1.5d0/10d0     !1/t
            prob_ang(itp,3)=1.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=1.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=5.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=5/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=4d0/16d0  !1/s
          prob_mass(3)=4d0/16d0  !ln s/s
          prob_mass(4)=1d0/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=4.5d0/16d0  !Z
        ELSE
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=5/16d0  !1/s
          prob_mass(3)=4/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=4/16d0  !Z
        ENDIF
!     =======================================================
      ELSEIF( (abs(i_flav(1)).EQ.11 .AND. abs(i_flav(2)).EQ.11 
     @   .AND. (abs(i_flav(3)).EQ.14 .OR. abs(i_flav(3)).EQ.16))
     @    .OR.
     @   ((abs(i_flav(1)).EQ.14 .OR. abs(i_flav(1)).EQ.16)
     @   .AND. abs(i_flav(3)).EQ.11 .AND. abs(i_flav(4)).EQ.11)
     @      ) THEN 
!     ===============
!  6-10, type channels
!     =======================================================
        DO itp=0,m_step
          prob_ang(itp,1)=3/10d0     !flat
          prob_ang(itp,2)=2d0/10d0     !1/t
          prob_ang(itp,3)=2d0/10d0     !1/u
          prob_ang(itp,4)=1.5d0/10d0  !ln t/t
          prob_ang(itp,5)=1.5d0/10d0  !ln u/u
        ENDDO
        IF(itpe_loc.eq.9)THEN
          prob_mass(1)=4/16d0  !flat
          prob_mass(2)=4/16d0  !1/s
          prob_mass(3)=3/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=4/16d0  !Z
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=3.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=3.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF        
          ENDDO
        ELSEIF(itpe_loc.eq.50)THEN
! unused
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=4d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=4d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=4/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=4/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=4d0/16d0  !Z
        ELSEIF(itpe_loc.eq.21)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5d0/10d0     !1/t
            prob_ang(itp,3)=0.5d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=3.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=8/16d0  !1/s
          prob_mass(3)=2/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=8d0/16d0  !Z
        ELSEIF(itpe_loc.eq.42.or.itpe_loc.eq.50)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=6.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=4d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=4d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=1.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=7/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=2/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=3.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.60.or.itpe_loc.eq.33)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=4.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=8/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=3.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.57)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=4.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=5.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=4.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=5/16d0  !1/s
          prob_mass(3)=5/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.17)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=1.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=2d0/10d0  !1/t**2
            prob_ang(itp,9)=2d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=1.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            prob_ang(itp,8)=2d0/10d0  !1/t**2
            prob_ang(itp,9)=2d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=1.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            prob_ang(itp,8)=2d0/10d0  !1/t**2
            prob_ang(itp,9)=2d0/10d0  !1/u**2
            ENDIF        
          ENDDO
          prob_mass(1)=7d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=1d0/16d0  !ln s/s
          prob_mass(4)=2d0/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=3d0/16d0  !Z
        ELSEIF(itpe_loc.eq.18)THEN
          prob_mass(1)=2d0/16d0  !flat
          prob_mass(2)=3d0/16d0  !1/s
          prob_mass(3)=3/16d0  !ln s/s
          prob_mass(4)=.5d0/16d0  !1/sma-s
          prob_mass(6)=5d0/16d0  !W
          prob_mass(7)=2.5/16d0  !Z
        ELSEIF(itpe_loc.eq.43)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=2.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=4.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=4.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=5d0/16d0  !flat
          prob_mass(2)=.5d0/16d0  !1/s
          prob_mass(3)=0d0/16d0  !ln s/s
          prob_mass(4)=1d0/16d0  !1/sma-s
          prob_mass(6)=5d0/16d0  !W
          prob_mass(7)=4.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.55 .or. itpe_loc.eq.31)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=6.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=3.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=3.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            ENDIF
          ENDDO
          prob_mass(1)=6d0/16d0  !flat
          prob_mass(2)=.5d0/16d0  !1/s
          prob_mass(3)=0d0/16d0  !ln s/s
          prob_mass(4)=1d0/16d0  !1/sma-s
          prob_mass(6)=6d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.34)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3d0/10d0  !ln u/u
            prob_ang(itp,6)=0d0/10d0  !ln2 t/t
            prob_ang(itp,7)=2d0/10d0  !ln2 u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=0.5d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,6)=0d0/10d0  !ln2 t/t
            prob_ang(itp,7)=0d0/10d0  !ln2 u/u
            prob_ang(itp,8)=4.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=5/10d0     !flat
            prob_ang(itp,2)=1.5d0/10d0     !1/t
            prob_ang(itp,3)=1.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,6)=0d0/10d0  !ln2 t/t
            prob_ang(itp,7)=0d0/10d0  !ln2 u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=9/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=3d0/16d0  !Z
        ELSEIF(itpe_loc.eq.36)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,6)=0d0/10d0  !ln2 t/t
            prob_ang(itp,7)=0d0/10d0  !ln2 u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,6)=5d0/10d0  !ln2 t/t
            prob_ang(itp,7)=0d0/10d0  !ln2 u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=4.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,6)=0d0/10d0  !ln2 t/t
            prob_ang(itp,7)=0d0/10d0  !ln2 u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=9/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=3d0/16d0  !Z
        ELSEIF(itpe_loc.eq.25)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=2.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=4.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=4.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=10/16d0  !1/s
          prob_mass(3)=.5/16d0  !ln s/s
          prob_mass(4)=.5/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=3d0/16d0  !Z
        ELSEIF(itpe_loc.eq.37)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,8)=2.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=2/16d0  !1/s
          prob_mass(3)=8/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.52)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=7.5d0/10d0     !1/u
            prob_ang(itp,4)=0d0/10d0  !ln t/t
            prob_ang(itp,5)=0d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=3.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=5/16d0  !flat
          prob_mass(2)=4/16d0  !1/s
          prob_mass(3)=4/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=0d0/16d0  !W
          prob_mass(7)=2d0/16d0  !Z
        ELSEIF(itpe_loc.eq.22)THEN
      amreg2_mas=    ambeam**2/10
      amreg2_ang=    ambeam**3*50
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=4d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=4d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2d0/10d0  !ln u/u
            prob_ang(itp,8)=0.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=9/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.40)THEN
      amreg2_mas=    ambeam**2/10
      amreg2_ang=    ambeam**3*50
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=6d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=3d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=9/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.45)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=3d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=3d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=6.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=9/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.10)THEN
      amreg2_mas=    ambeam**2/10
      amreg2_ang=    ambeam**3*50
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=3.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=9/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.35
     @         .or.itpe_loc.eq.20)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=4.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=3.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=9/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=2.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.8)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=4.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=4.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=8/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=0d0/10d0  !ln t/t
            prob_ang(itp,5)=0d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=5/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=0/16d0  !1/sma-s
          prob_mass(6)=0/16d0  !W
          prob_mass(7)=8/16d0  !Z
        ELSEIF(itpe_loc.eq.11)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=6.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=5.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=5/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=2/16d0  !1/sma-s
          prob_mass(6)=1/16d0  !W
          prob_mass(7)=5/16d0  !Z
        ELSE
          DO itp=0,m_step
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=5/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=2/16d0  !1/sma-s
          prob_mass(6)=1/16d0  !W
          prob_mass(7)=5/16d0  !Z
        ENDIF
!!!!!!!!!!!!!!!!! FIXED !!!!!!!!!!!!!!!!!!!!!!
        prob_mass(1)=2/16d0  !flat
        prob_mass(2)=5/16d0  !1/s
        prob_mass(3)=0/16d0  !ln s/s
        prob_mass(4)=0/16d0  !1/sma-s
        prob_mass(6)=0d0/16d0  !W
        prob_mass(7)=9d0/16d0  !Z
!!!!!!!!!!!!!!!!! FIXED !!!!!!!!!!!!!!!!!!!!!!
!     =======================================================
      ELSEIF( (abs(i_flav(1)).EQ.11 .AND. abs(i_flav(2)).EQ.11 
     @   .AND. abs(i_flav(3)).NE.11 .AND. abs(i_flav(4)).NE.11)
!     @      ) THEN 
!      ELSEIF( 
     @    .OR.
     @   (abs(i_flav(1)).NE.11 .AND. abs(i_flav(2)).NE.11
     @   .AND. abs(i_flav(3)).EQ.11 .AND. abs(i_flav(4)).EQ.11)
!     @      ) THEN 
!      ELSEIF(
     @    .OR.
     @   (abs(i_flav(1)).EQ.11 .AND. abs(i_flav(2)).EQ.11 .AND. 
     @   abs(i_flav(3)).EQ.11 .AND. abs(i_flav(4)).EQ.11 ) 
     @      ) THEN 
!     ===============
!  6-1, type channels
!     =======================================================
!  6-6, type channels
!     =======================================================
        amreg2_ang=    ambeam**3/50
        amreg2_mas=    ambeam**2/10
        prob_mass(1)=2/16d0  !flat
        prob_mass(2)=8/16d0  !1/s
        prob_mass(3)=1/16d0  !ln s/s
        prob_mass(4)=1/16d0  !1/sma-s
        prob_mass(5)=0/16d0  !1/s**2
        prob_mass(6)=.5d0/16d0  !W
        prob_mass(7)=3.5d0/16d0  !Z
        DO itp=0,m_step
          prob_ang(itp,1)=3/10d0     !flat
          prob_ang(itp,2)=2d0/10d0     !1/t
          prob_ang(itp,3)=2d0/10d0     !1/u
          prob_ang(itp,4)=1.5d0/10d0  !ln t/t
          prob_ang(itp,5)=1.5d0/10d0  !ln u/u
        ENDDO
        IF(itpe_loc.eq.9)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=3.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=3.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=3.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF        
          ENDDO
        ELSEIF(itpe_loc.eq.11)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=3.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=1d0/10d0  !1/t**2
            prob_ang(itp,9)=1d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=4/16d0  !1/s
          prob_mass(3)=.5/16d0  !ln s/s
          prob_mass(4)=.5/16d0  !1/sma-s
          prob_mass(5)=5/16d0  !1/s**2
          prob_mass(6)=.5/16d0  !W
          prob_mass(7)=3.5/16d0  !Z
        ELSEIF(itpe_loc.eq.8)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=5/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=5/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=2.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=2/16d0  !flat
          prob_mass(2)=4/16d0  !1/s
          prob_mass(3)=.5/16d0  !ln s/s
          prob_mass(4)=.5/16d0  !1/sma-s
          prob_mass(5)=5/16d0  !1/s**2
          prob_mass(6)=.5/16d0  !W
          prob_mass(7)=3.5/16d0  !Z
        ELSEIF(itpe_loc.eq.49.or.itpe_loc.eq.46.or.itpe_loc.eq.23)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=5/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.50)THEN
! unused
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=4d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=4d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.21)THEN
c      amreg2_ang=    ambeam**3/10
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=5d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5d0/10d0     !1/t
            prob_ang(itp,3)=0.5d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
          prob_mass(1)=4/16d0  !flat
          prob_mass(2)=6/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(5)=0/16d0  !1/s**2
          prob_mass(6)=.5d0/16d0  !W
          prob_mass(7)=3.5d0/16d0  !Z
        ELSEIF(itpe_loc.eq.42.or.itpe_loc.eq.50)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=6.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=4d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=4d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=1.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.60.or.itpe_loc.eq.33)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=4.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.57)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=4.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=5.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=4.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.17)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=1.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=2d0/10d0  !1/t**2
            prob_ang(itp,9)=2d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1)THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=1.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            prob_ang(itp,8)=2d0/10d0  !1/t**2
            prob_ang(itp,9)=2d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2d0/10d0
            prob_ang(itp,2)=1.5d0/10d0
            prob_ang(itp,3)=0.5d0/10d0
            prob_ang(itp,4)=1.5d0/10d0
            prob_ang(itp,5)=0.5d0/10d0
            prob_ang(itp,8)=2d0/10d0  !1/t**2
            prob_ang(itp,9)=2d0/10d0  !1/u**2
            ENDIF        
          ENDDO
        ELSEIF(itpe_loc.eq.18)THEN
        ELSEIF(itpe_loc.eq.43 .or. itpe_loc.eq.55)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=1d0/10d0  !ln u/u
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.34)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=0.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3d0/10d0  !ln u/u
            prob_ang(itp,6)=0d0/10d0  !ln2 t/t
            prob_ang(itp,7)=2d0/10d0  !ln2 u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=0.5d0/10d0     !1/t
            prob_ang(itp,3)=0.5d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,6)=0d0/10d0  !ln2 t/t
            prob_ang(itp,7)=0d0/10d0  !ln2 u/u
            prob_ang(itp,8)=4.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=2.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,6)=1d0/10d0  !ln2 t/t
            prob_ang(itp,7)=.5d0/10d0  !ln2 u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.36)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,6)=0d0/10d0  !ln2 t/t
            prob_ang(itp,7)=0d0/10d0  !ln2 u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,6)=5d0/10d0  !ln2 t/t
            prob_ang(itp,7)=0d0/10d0  !ln2 u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=4.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,6)=0d0/10d0  !ln2 t/t
            prob_ang(itp,7)=0d0/10d0  !ln2 u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.25)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=2.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=4.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,8)=5d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=4/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=2.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.37)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,8)=2.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.52)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=7.5d0/10d0     !1/u
            prob_ang(itp,4)=0d0/10d0  !ln t/t
            prob_ang(itp,5)=0d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=3.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=3.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=3.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.22)THEN
c      amreg2_ang=    ambeam**3*50
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=4d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=4d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=2d0/10d0  !ln u/u
            prob_ang(itp,8)=0.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.40)THEN
c      amreg2_ang=    ambeam**3*50
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=6d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=3d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=3.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1d0/10d0     !1/t
            prob_ang(itp,3)=1d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.45)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=3d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=3d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=6.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=0.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=7/10d0     !flat
            prob_ang(itp,2)=1.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.10)THEN
c      amreg2_ang=    ambeam**3*50
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=1d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=3.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=.5d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=.5d0/10d0  !ln t/t
            prob_ang(itp,5)=5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSEIF(itpe_loc.eq.35
     @         .or.itpe_loc.eq.20)THEN
          DO itp=0,m_step
            IF(itp.EQ.0) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=4.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ELSEIF(itp.EQ.1) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2.5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=3.5d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=0.5d0/10d0  !1/u**2
            ELSEIF(itp.EQ.2) THEN
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=5d0/10d0     !1/t
            prob_ang(itp,3)=.5d0/10d0     !1/u
            prob_ang(itp,4)=2d0/10d0  !ln t/t
            prob_ang(itp,5)=.5d0/10d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
            ENDIF
          ENDDO
        ELSE
          DO itp=0,m_step
            prob_ang(itp,1)=2/10d0     !flat
            prob_ang(itp,2)=2d0/10d0     !1/t
            prob_ang(itp,3)=2d0/10d0     !1/u
            prob_ang(itp,4)=1.5d0/10d0  !ln t/t
            prob_ang(itp,5)=1.5d0/10d0  !ln u/u
            prob_ang(itp,8)=.5d0/10d0  !1/t**2
            prob_ang(itp,9)=.5d0/10d0  !1/u**2
          ENDDO
        ENDIF
      ELSE
!     ====
        WRITE(6,*)'set_param_ZZ=> not set for i_flav:',i_flav
      ENDIF
!     =====
!!!!!!!!!!!!!!!!!!!! short circuit !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!! short circuit !!!!!!!!!!!!!!!!!!!!!!!!
c          DO itp=0,m_step
c            prob_ang(itp,1)=2/10d0     !flat
c            prob_ang(itp,2)=1d0/10d0     !1/t
c            prob_ang(itp,3)=1d0/10d0     !1/u
c            prob_ang(itp,4)=2.5d0/10d0  !ln t/t
c            prob_ang(itp,5)=2.5d0/10d0  !ln u/u
c            prob_ang(itp,8)=.5d0/10d0  !1/t**2
c            prob_ang(itp,9)=.5d0/10d0  !1/u**2
c          ENDDO
c          prob_mass(1)=2/16d0  !flat
c          prob_mass(2)=2/16d0  !1/s
c          prob_mass(3)=5/16d0  !ln s/s
c          prob_mass(4)=3/16d0  !1/sma-s
c          prob_mass(6)=2/16d0  !W
c          prob_mass(7)=2/16d0  !Z
!!!!!!!!!!!!!!!!!!!! short circuit !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!! short circuit !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!! flat distr. for Mariusz begin !!!!!!!!!!!!!!!!!!!!
        IF(itpe_loc.eq.2)THEN
          DO itp=0,m_step
            prob_ang(itp,1)=8/10d0     !flat
            prob_ang(itp,2)=1/10d0     !1/t
            prob_ang(itp,3)=1/10d0     !1/u
            prob_ang(itp,4)=0d0  !ln t/t
            prob_ang(itp,5)=0d0  !ln u/u
            prob_ang(itp,6)=0d0  !ln t/t
            prob_ang(itp,7)=0d0  !ln u/u
            prob_ang(itp,8)=0d0/10d0  !1/t**2
            prob_ang(itp,9)=0d0/10d0  !1/u**2
          ENDDO
          prob_mass(1)=8/16d0  !flat
          prob_mass(2)=2/16d0  !1/s
          prob_mass(3)=1/16d0  !ln s/s
          prob_mass(4)=1/16d0  !1/sma-s
          prob_mass(5)=0/16d0  !1/s**2
          prob_mass(6)=2d0/16d0  !W
          prob_mass(7)=2d0/16d0  !Z
        ENDIF
!!!!!!!!!!!!! flat distr. for Mariusz end !!!!!!!!!!!!!!!!!!!!
!!! checks on probabilities !!!
      icheck=1
      IF(icheck.EQ.1) THEN
        DO i=0,m_step
          pang=0d0
          DO j=1,n_ang
            pang=pang+prob_ang(i,j)
          ENDDO
          IF(abs(pang-1).gt. 1d-14) THEN
            WRITE(6,*)
     @  'set_param_ZZ=> wrong ang. prob.: itpe_loc,step,sum='
     @    ,itpe_loc,j,pang
            STOP
          ENDIF
        ENDDO
        pang=0d0
        DO i=1,n_mass
          pang=pang+prob_mass(i)
        ENDDO
        IF(abs(pang-1).gt. 1d-14) THEN
          WRITE(6,*)
     @      'set_param_ZZ=> wrong mass prob.: itpe_loc,sum='
     @        ,itpe_loc,pang
          STOP
        ENDIF
      ENDIF

      END
      subroutine mm_spacegen(mode,itype,ambeam,svar,sprim,fakir,
     $                       bp1,bp2,bp3,bp4)
********************************************************
! ================================================================
! mode=0                                                         =
!        generates 4-momenta accordingly to itype of generation, =
!        calculates jacobian (out from 4-momenta)                =
! mode=1                                                         =
!        calculates jacobian (out from 4-momenta)                =
!        for itype generation branch                             =
! ================================================================
      implicit DOUBLE PRECISION (a-h,o-z)
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW 
      COMMON / MATPAR / PI,CEULER       
      COMMON / DECAYS / IFLAV(4), AMDEC(4) 
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      save  / WEKING /,/ WEKIN2 /,/ MATPAR /,/ KeyKey/
      save  / DECAYS /

      dimension amdet(4)
!!! MS new kinematics
!      dimension qeff1(4),qeff2(4)
      dimension bq1(4),bq2(4),bp1(4),bp2(4),bp3(4),bp4(4)
!      dimension pt1(4),pt2(4)
      dimension bq123(4),bq23(4)
      dimension wkbp1(4),wkbp2(4),wkbp3(4),wkbp4(4)
      dimension i_flav(4)
!!! MS new kinematics end
!#############################################################
!    GENERAL INITIALIZATION:                                 #
!    SETTING PRESAMPLER PARAMETERS AND MASSES IN ORDER       #
!    FOR PARTICULAR >ITYPE< SLOT-SPACE                       #
!    most of ITYPES differ by order of 4-vectors only        #
!#############################################################

      IF (ITYPE.EQ.1) THEN
        amx=amaw
        gamx=gammw
        amdet(1)=amdec(1)
        amdet(2)=amdec(2)
        amdet(3)=amdec(3)
        amdet(4)=amdec(4)
      ENDIF

      IF (MODE.EQ.1) THEN 
!##############################################
! INITIALIZATION FOR RECALCULATION MODE:      #
!  ANGLES AND S-i's FROM FOUR VECTORS         #
!##############################################
        IF(itype.EQ.1) THEN
          call invkin(ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $                  amwmn,amwpn,  bp1,bp2,bp3,bp4)
          s1=amwmn**2
          s2=amwpn**2
        ELSE
          do i=1,4
            bq1(i)=bp1(i)+bp2(i)
            bq2(i)=bp3(i)+bp4(i)
            bq23(i)=bp2(i)+bp3(i)
            bq123(i)=bp1(i)+bp2(i)+bp3(i)
          enddo
          s1=dmas2(bq1)
          s2=dmas2(bq2)
          s12=s1
          s23=dmas2(bq23)
          s123=dmas2(bq123)
        ENDIF
      ENDIF

!(((((((((((((((((((((((
      IF(itype.EQ.1)THEN
!(((((((((((((((((((((((

!##############################################
! BASIC PART:                                 #
! MODE=0 GENERATION AND JACOBIAN CALCULATION  #
! MODE=1 JACOBIAN CALCULATION ONLY            #
!##############################################
!... s1,s2 subspace
        CALL RES2GN(mode,SVAR,SPRIM,AMx,GAMx,amdet,S1,S2,SSCRU)

!
! rejection
!m.s. 13.04      if(mode.eq.0.and.sscru.eq.0d0)  then
        if(sscru.eq.0d0)  then
!-- short-out ... 
          fakir=0D0
          return
        endif

! lambda factors, can be moved to res2/3-s or even out.
        x1=s1/sprim
        x2=s2/sprim
        bmain=sqrt( (1-x1-x2)**2 - 4*x1*x2 )
        xwm1=amdet(1)**2/s1
        xwm2=amdet(2)**2/s1
        bwm=sqrt( (1-xwm1-xwm2)**2 - 4*xwm1*xwm2 )
        xwp1=amdet(3)**2/s2
        xwp2=amdet(4)**2/s2
        bwp=sqrt( (1-xwp1-xwp2)**2 - 4*xwp1*xwp2 )
        wjac=bmain*bwp*bwm

!... production angles
        CALL cospro(mode,sprim,s1,s2,ctn,fin,xccos)

! decay angles
        CALL cosdec(mode,sprim,ct1n,fi1n,xccos1)
        CALL cosdec(mode,sprim,ct2n,fi2n,xccos2)

! for the overall jacobian
        fakp = xccos1*xccos2*xccos*sscru

c        write(6,*) 'OLD  fakp,wjac=',fakp,wjac
c        write(6,*) 'OLD crud c1,c2,c,ss',xccos1,xccos2,xccos,sscru
! security check
        if (fakp.eq.0d0 .or. wjac.eq.0d0) then
          write(6,*) 'spacegen: we have troubles;'
          write(6,*) '  fakp,wjac=',fakp,wjac
          write(6,*) 'crud c1,c2,c,ss',xccos1,xccos2,xccos,sscru
          write(6,*) 'itype,mode',itype,mode
          write(6,*) 'amdet',amdet
          write(6,*) 'sq(s1),sq(s2)',sqrt(s1),sqrt(s2)
!!         write(6,*) 1/fakp,1/wjac
!!         stop
        endif
       
        if (mode.eq.0) then 
!##############################################
!  KONSTRUCT FINAL 4-VECTORS etc:             #
!  MODE=0 ONLY                                #
!##############################################
!
          CALL kineww(sprim,ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $           sqrt(s1),sqrt(s2),amdet,bq1,bq2,bp1,bp2,bp3,bp4)

c.. 4momenta for born, in effective CMS, z+ along qeff1
          do i=1,4
            bq1(i)=bp1(i)+bp2(i)
            bq2(i)=bp3(i)+bp4(i)
          enddo
        endif   

!((((((((((!((((((((((
      ENDIF
!((((((((((!((((((((((

!! ++++++++++++++++++++++++++++++++++
!! M.S. New Channels, new kinematics.

      DO i=1,4
        i_flav(i)=iflav(i)
      ENDDO


      IF(itype.GE.2 .AND. itype.LE.7) THEN
!     ========================
! extra slots fixed to parallel without permutation (ie 1234)
        CALL permut_tab(itype,iperm,iflip)
        CALL permut(1,iperm,amdec,amdet,
     $           bp1,bp2,bp3,bp4,wkbp1,wkbp2,wkbp3,wkbp4)
        CALL kinga_2parl(mode,iflip,sprim,ambeam,amdet,i_flav,
     @            itype,wkbp1,wkbp2,wkbp3,wkbp4,fakp,wjac)
        CALL permut(-1,iperm,amdec,amdet,
     $           bp1,bp2,bp3,bp4,wkbp1,wkbp2,wkbp3,wkbp4)
c.. 4momenta for born, in effective CMS, z+ along qeff1
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo

      ELSEIF(itype.GE.8 .AND. itype.LE.13) THEN
!     =======================
! 8-10,11-13 parall. perm.
        CALL permut_tab(itype,iperm,iflip)
        CALL permut(1,iperm,amdec,amdet,
     $           bp1,bp2,bp3,bp4,wkbp1,wkbp2,wkbp3,wkbp4)
        CALL kinga_2parl(mode,iflip,sprim,ambeam,amdet,i_flav,
     @            itype,wkbp1,wkbp2,wkbp3,wkbp4,fakp,wjac)
        CALL permut(-1,iperm,amdec,amdet,
     $           bp1,bp2,bp3,bp4,wkbp1,wkbp2,wkbp3,wkbp4)
c.. 4momenta for born, in effective CMS, z+ along qeff1
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
! checks start
!      if(mode.eq.11 .and. fakp*wjac.ne.0d0) then
!        CALL kinga_2parl(1,iflip,sprim,ambeam,amdet,i_flav,
!     @            itype,bp1,bp2,bp3,bp4,fakp1,wjac1)
!        write(6,*)'jacek ',wjac/wjac1,fakp/fakp1
!      endif
! checks end

      ELSEIF(itype.GE.14 .AND. itype.LE.37) THEN
!     ========================
! 14-25,26-37 serl.1 perm.
        CALL permut_tab(itype,iperm,iflip)
        CALL permut(1,iperm,amdec,amdet,
     $           bp1,bp2,bp3,bp4,wkbp1,wkbp2,wkbp3,wkbp4)
        CALL kinga_2serl(mode,iflip,sprim,ambeam,amdet,i_flav,
     @            itype,wkbp1,wkbp2,wkbp3,wkbp4,fakp,wjac)
        CALL permut(-1,iperm,amdec,amdet,
     $           bp1,bp2,bp3,bp4,wkbp1,wkbp2,wkbp3,wkbp4)
c.. 4momenta for born, in effective CMS, z+ along qeff1
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo

      ELSEIF(itype.GE.38 .AND. itype.LE.61) THEN
!     ========================
! 38-49,50-61 serl.2 perm.
        CALL permut_tab(itype,iperm,iflip)
        CALL permut(1,iperm,amdec,amdet,
     $           bp1,bp2,bp3,bp4,wkbp1,wkbp2,wkbp3,wkbp4)
        CALL kinga_2serl_2(mode,iflip,sprim,ambeam,amdet,i_flav,
     @            itype,wkbp1,wkbp2,wkbp3,wkbp4,fakp,wjac)
        CALL permut(-1,iperm,amdec,amdet,
     $           bp1,bp2,bp3,bp4,wkbp1,wkbp2,wkbp3,wkbp4)
c.. 4momenta for born, in effective CMS, z+ along qeff1
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo

      ENDIF
!     =====

! rejection
      IF(mode.EQ.0 .AND. fakp.EQ.0d0)  THEN
!-- short-cut ... 
         fakir=0D0
         RETURN
      ENDIF

!... WE CALCULATE OVERALL JACOBIAN ...
      fak= 1D0/32D0*fakp*wjac
!...  EN_pi=(2*pi)**4/(2*(2*PI)**3)**(r;r=4) 
      ENPI=(2*PI)**4/(2*(2*PI)**3)**4
      fakir=fak*ENPI

      end


      SUBROUTINE make_phsp_point_mz
     $      (prob_ps_m,msdump,ambeam,svar,sprim,fak_phsp,i_m,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
*     **************************************
* msdump=0 : generates the phase space point
* msdump=1 : point is red from the disk
* fak_phsp  : weight 
*     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)

! two commons, no way ....
      PARAMETER (mm_nrchan=65)
! it was mm_monit
      COMMON /c_phspm/
     $                 prob_mm(mm_nrchan),
     $                 faki_mon(mm_nrchan+3000),
     $                 ikan_mon
      SAVE /c_phspm/
CBB      INCLUDE 'mm_phsp.h'
      PARAMETER (nrch=100)
! it was kanalarz
      COMMON /c_phspz/
     $                prob(nrch),
     $                fak,
     $                faki(nrch),
     $                ikan,
     $                mrchan,
     $                nrchan
      SAVE c_phspz
CBB      INCLUDE 'zz_phsp.h'

      DIMENSION drvec(100)
      DIMENSION
     $      effbeam1(4),effbeam2(4),effp1(4),effp2(4),effp3(4),effp4(4)

      IF(msdump .EQ. 1) THEN
         CALL mm_brancher(sprim,itype,prob_mm)
         CALL brancher(sprim,itype)
         CALL READer(itype,effbeam1,effbeam2,effp1,effp2,effp3,effp4)
         CALL mm_spacegen(1,itype,ambeam,svar,sprim,fak_phsp,
     $                  effp1,effp2,effp3,effp4)
      ELSE
        CALL varran(drvec,1)
        IF(drvec(1) .LE. prob_ps_m) THEN
          i_m=1
        ELSE
          i_m=0
        ENDIF
        IF( i_m .EQ. 1 ) THEN
          CALL mm_brancher(sprim,itype,prob_mm)
          CALL mm_spacegen(0,itype,ambeam,svar,sprim,fak_phsp,
     $                    effp1,effp2,effp3,effp4)
          ikan_mon=itype
          faki_mon(itype)=fak_phsp
        ELSEIF( i_m .EQ. 0 ) THEN
          CALL brancher(sprim,itype)
          CALL spacegen(0,itype,svar,sprim,fak_phsp,
     $                    effp1,effp2,effp3,effp4)
          ikan=itype
          faki(itype)=fak_phsp
        ENDIF
        CALL set_eff_beams(sprim,ambeam,effbeam1,effbeam2)
      ENDIF

      END

      SUBROUTINE get_phsp_weight_mz
     $      (prob_ps_m,ambeam,svar,sprim,fak_tot,i_m,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
*     **************************************
* calculates jacobians of all channels and does resummation
* fak_tot  : total weight - the only output
*     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)

! two commons, no way ....
      PARAMETER (mm_nrchan=65)
! it was mm_monit
      COMMON /c_phspm/
     $                 prob_mm(mm_nrchan),
     $                 faki_mon(mm_nrchan+3000),
     $                 ikan_mon
      SAVE /c_phspm/
CBB      INCLUDE 'mm_phsp.h'
      PARAMETER (nrch=100)
! it was kanalarz
      COMMON /c_phspz/
     $                prob(nrch),
     $                fak,
     $                faki(nrch),
     $                ikan,
     $                mrchan,
     $                nrchan
      SAVE c_phspz
CBB      INCLUDE 'zz_phsp.h'

      DIMENSION
     $      effbeam1(4),effbeam2(4),effp1(4),effp2(4),effp3(4),effp4(4)

      fakm=0d0
      DO i=1,mm_nrchan
         IF (prob_mm(i) .GT. 0d0) THEN
            IF (i .NE. ikan_mon  .OR.  i_m .NE. 1) THEN
               CALL mm_spacegen(1,i,ambeam,svar,sprim,faki_mon(i),
     $                    effp1,effp2,effp3,effp4)
            ENDIF
            IF (faki_mon(i) .GT. 0d0) THEN
               fakm=fakm+prob_mm(i)/faki_mon(i)
            ENDIF
         ENDIF
      ENDDO
*...  
      fakz=0d0
      DO i=1,nrchan
         IF (prob(i) .GT. 0d0) THEN
            IF (i .NE. ikan  .OR.  i_m .NE. 0) THEN
               CALL spacegen(1,i,svar,sprim,faki(i),
     $                    effp1,effp2,effp3,effp4)
            ENDIF
            IF (faki(i) .GT. 0d0) THEN
               fakz=fakz+prob(i)/faki(i)
            ENDIF
         ENDIF
      ENDDO
*...
      fak=1/(prob_ps_m*fakm +(1-prob_ps_m)*fakz)

      fak_tot=fak

      END



      FUNCTION phot_spec_crud(svar,sprim,label)
**********************************************************
! NOTICE 
! This function is used ONLY to fill the prob_crud matrix!!!
! It should not be called directly by any other routines.
! The prob_crud matrix can be used instead!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! this is becoming an unbelievably nasty function... ms.
!
! convention for prob_chan(numb):
! 1-81: WW Wp=1:1-9; 2:10-18..
! 82-202: ZZ Z1=1:82-92; 2:93-103..
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
! the following common is introduced temporarily, to ensure adiabatic
! transition, to be removed later on
      COMMON / DECDAT / AMdumm(20), BR(20)
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
!   -- cuts for selecto
      common /articut/ arbitr,arbitr1,themin,arbitr2
! end commons
      DIMENSION br_crud(20),brz_crud(20)
      SAVE

      DATA init /0/
      SAVE init,factor_z

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(init .EQ. 0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        init=1
! br_crud is the TEMPORARY fundamental DUMMY quantity to construct
! crude probabilities of decay channels, to be ramoved later on.
        DO i=1,20
          br_crud(i)=br(i)
        ENDDO
! stuff from filexp=================start
        Key4f  = MOD(KeyMis,100)/10
        FACWEL=1d0
        FACZEL=1d0
        if(key4f.NE.0)  then
          FACWEL=7d0
          br_crud(7)=br_crud(7)*FACWEL

          FACZEL=48d0!21d0
          IF(arbitr.LT.500d0 .AND.  arbitr.GT.0.1d0) THEN
            faczel=faczel *(.5d0*500d0/arbitr +.5d0)
          ELSEIF(arbitr.LE.0.1d0) THEN
! this is unexploited region, faczel set to an arbitrary number
            faczel=faczel * 2500d0
          ENDIF
        endif

        probw=0
        do k=1,9
          probw=probw+br_crud(k)
        enddo
        do k=1,9
          br_crud(k)=br_crud(k)/probw
        enddo
!
        probw=1
        probh=0.5d0-1.5d0*br_crud(8)
        probh=0.5d0*(1d0-br_crud(7)-br_crud(8)-br_crud(9))

        probz=6d0*br_crud(8)+5d0*probh
        if(key4f.NE.0) probz=(5d0+FACZEL)*br_crud(8)+5d0*probh
        do k=6,11
          brz_crud(k)=br_crud(8)/probz
        enddo
        if(key4f.NE.0)  brz_crud(6)=brz_crud(6)*FACZEL
        do k=1,5
          brz_crud(k)=probh/probz
        enddo
        probz=probz**2
        probw=probw/(probw+probz)
        probz=1d0-probw
! stuff from filexp=================end

        braw=0d0
        braz=0d0
        DO i=1,11
          DO j=1,11
            braw=braw+br_crud(i)*br_crud(j)
            braz=braz+brz_crud(i)*brz_crud(j)
          ENDDO
        ENDDO
        factor_z=braw/braz*(probz/probw)
c        write(6,*)'braw/braz,probw/probz',braw,braz,probw/probz
! end backw. compat
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF(label .GT. 202) THEN
        phot_spec_crud=0d0
        RETURN
      ENDIF

! energy dependence
      phot_spec= tot_born_crud(svar,sprim)
! flavor dependence (this is a routine from decay.f)
      CALL linear_to_WZ_label(1,label,iwm,iwp,if_z,if_w)
c      write(6,*)'1 label,iwm,iwp,if_z,if_w',label,iwm,iwp,if_z,if_w
!-- find the overall W/Z normalisation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!! disabled for now !!!!!!!!!!!!!!!!!!!!!!!!
ccc      goto 11
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
      IF(if_w .EQ. 1) THEN
!-- WW
        phot_spec= phot_spec* br_crud(iwm)*br_crud(iwp)
      ELSEIF(if_z .EQ. 1) THEN
!-- ZZ
        phot_spec= phot_spec* brz_crud(iwm)*brz_crud(iwp)
! factor_z for backw. comp.
        phot_spec= phot_spec *factor_z
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 11   continue
      goto 12
!!!!!!!!!!!!!!!!!! disabled for now !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(if_w .EQ. 1) THEN
!-- WW
        brcm=1
        brcp=1
        IF(iwm.eq.7) brcm=1
        IF(iwp.eq.7) brcp=1 
        phot_spec= phot_spec* brcm *brcp
      ELSEIF(if_z .EQ. 1) THEN
!-- ZZ
        brcm=1
        brcp=1
        IF(iwm.eq.6) brcm=7 
        IF(iwp.eq.6) brcp=7 
        phot_spec= phot_spec* brcm *brcp
      ENDIF
!!!!!!!!!!!!!!!!!! added for now end !!!!!!!!!!!!!!!!!!!!!!!
 12   continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      phot_spec_crud = phot_spec

      END


      FUNCTION tot_born_crud(svar,sprim)
*     ***********************************
* total crude born xsection for photonic spectra
* 
*     ***********************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
      COMMON / phypar / alfinv,gpicob
      COMMON / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf
      COMMON / wekin2 / amaw,gammw,gmu,alphaw
* this COMMON can be everywhere, contains various switches
      COMMON / keykey /  keyrad,keyphy,keytek,keymis,keydwm,keydwp

      keysmp = MOD(keytek,1000)/100

      IF (keysmp .EQ. 0) THEN
! good old CC03
!--     S1-S2 integral over L area
        CALL resspec(svar,sprim,amaw,gammw,prnorm)
        born_crud=prnorm *bornscrud(sprim,2)
      ELSE
! full matr. el.
!--     S1-S2 integral over L area
        CALL resspec(svar,sprim,amaw,gammw,prnorm)
        born_crud=bornscrud(svar,2) *prnorm
* some trick of Zbyszeks
        svarxx=sprim
        IF(sprim .GT. amaz**2+gammz**2) THEN
          xls =log((svar-amaz**2)/gammz**2)
          xlsi=log((sprim-amaz**2)/gammz**2)
          xlsix=xls*(xlsi/xls)**2
          svarxx=amaz**2+gammz**2*exp(xlsix)
        ENDIF
        xsvarx=1/sprim+sprim/((svarxx-amaz**2)**2+(sprim*gammz/amaz)**2)
        svarix=svar
        xsvari=1/svar+svar/((svarix-amaz**2)**2+(svar*gammz/amaz)**2)
        born_crud=born_crud*xsvarx/xsvari
      ENDIF

      tot_born_crud=born_crud

      END

      SUBROUTINE resspec(svar,sprim,rmas,rgam,prnorm)
*     ***************************************************************
*crude FUNCTION for spectrum normalization
*prnorm calculates the value of integral
* ds_1d_s2 w(s_1)w(s_2) over theta crude region (see koralw 1.02) manual.
*its results cancels out in final results.
* this FUNCTION is arbitrary up to the problems with maximum weight and
* algorithm efficiency.
* note: both resonances have the same mass distribution FUNCTION
*         svar    - max sprim
*         sprim   - actual s
*         rmas    - central value of a resonance mass distribution
*         rgam    - width of a resonance
*         prnorm  - value of the integral of crude distr.
*
* written by: m. skrzypek            date: 2/16/95
* last update: 5/07/96                        by: z.was
*
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
      SAVE a,b,winf,wmi,wma,wsqr,wrec,prsqr,prrec
      SAVE uma,umi,uinf,usqr,urec
      SAVE
*
*        WRITE(6,*)'resms2',rmas,rgam
      a=rmas**2
      b=rmas*rgam
* arctg
      winf = 1/b*atan((svar   -a)/b)
      wma  = 1/b*atan((sprim/4d0-a)/b)
      wmi  = 1/b*atan(        -a /b)
* logarithm
      uinf =1/2d0/a*dlog((svar   -a)**2 +b**2)
      uma  =1/2d0/a*dlog((sprim/4d0-a)**2 +b**2)
      umi  =1/2d0/a*dlog(                b**2)
* thetas
      thespr=1d0
      thesvr=1d0
      IF((sprim/4d0) .LT. a) thespr=0d0
      IF( svar       .LT. a) thesvr=0d0
      ulo= thespr*uma +(1d0-thespr)*umi
* normalisations
      wsqr=wma-wmi
      usqr=thespr*(uma-umi)
      prsqr=(wsqr+usqr)**2
      wrec=winf-wma
      urec=thesvr*(uinf -ulo)
      prrec=(wsqr+usqr)*(wrec+urec)
      prnorm=prsqr+2*prrec
      END

      FUNCTION bornscrud(svari,mode)
*     ***********************************
* this routine provides born crude cross section
* mode = 1 : normalized to total(s) (not used i.e. museum ?
*        2 : not normalized
*        difference is only in normalization !!!!?????
*     ***********************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
      COMMON / phypar / alfinv,gpicob
      COMMON / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf
      COMMON / wekin2 / amaw,gammw,gmu,alphaw
      SAVE / weking /,/ wekin2 /
      SAVE
      bss= pi/alfinv**2 /2d0 /sinw2**2
      bbwign=amaw**2/alfinv/pi/sinw2
      thr=1d0/svari/4d0
      IF(svari .GT. 8*amaw**2) THEN
        bs=1d0/svari*(1+4*amaw**2/svari)/2d0*log(svari/amaw**2)
      ELSEIF(svari .GT. 4*amaw**2) THEN
        beta=dsqrt(1-4*amaw**2/svari)
        IF(beta .LT. pi/alfinv) beta=pi/alfinv ! coulomb!
        bsr=1d0/svari*(1+4*amaw**2/svari)/2d0*log(svari/amaw**2)
        bs=bsr*(thr/bsr+beta*(1-thr/bsr))
      ELSE
        bs=thr*svari/4/amaw**2
      ENDIF
      bs=bs*(1+svari/sqrt((svari-amaz**2)**2+(svari*gammz/amaz)**2))/2d0
      bnorto=bss*gpicob*bbwign**2
      IF(mode .EQ. 2) THEN
        bornscrud=bs*4*amaw**2 *2
      ELSE
        WRITE(6,*)'bornsc=> mode <> 2:',mode
        STOP
      ENDIF
***      IF(mode .EQ. 1) bornscrud=bs*4*amaw**2 *2 *bnorto

      END

      SUBROUTINE open_data(disk_file,delimiter,io_number)
*////////////////////////////////////////////////////////////////////////////////
*//                                                                            //
*//   DiskFile  = input file to read                                           //
*//                                                                            //
*//   First data card: Begin://delimiter                                       //
*//   Last  data card: End://delimiter                                         //
*//   First character * defines comment card!                                  //
*//                                                                            //
*////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      CHARACTER*80 disk_file
      CHARACTER*80 delimiter
      CHARACTER*80 line
*
      INTEGER io_number,ninp,ll
*------
! here we open file for reading, if needed this line can be
! commented out and all input taken from one concatenated file 'xxx', 
      ninp = 13
      OPEN(ninp,file=disk_file,status='old')
!cccc      OPEN(ninp,file='xxx',status='old')
! here we open .... END
      REWIND(ninp)
* Search for 'Begin'
      DO ll =1,1000000
         READ(ninp,'(A)',end=201) line
         IF(line .EQ. 'Begin:'//delimiter) THEN
            GOTO 200
         ENDIF
      ENDDO

 201  CONTINUE
      WRITE(6,*)'open_data=> Begin:',delimiter,' not found, STOP'
      STOP

 200  CONTINUE
      io_number=ninp
      END


      SUBROUTINE close_data(disk_file,delimiter,io_number)
*////////////////////////////////////////////////////////////////////////////////
*//                                                                            //
*//   DiskFile  = input file to read                                           //
*//                                                                            //
*//   First data card: Begin://delimiter                                       //
*//   Last  data card: End://delimiter                                         //
*//   First character * defines comment card!                                  //
*//                                                                            //
*////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      CHARACTER*80 disk_file
      CHARACTER*80 delimiter
      CHARACTER*80 line

      INTEGER io_number,ninp,ll
*------
      ninp = io_number
* Check for 'End'
      DO ll =1,1000000
         READ(ninp,'(a)',end=201) line
         IF(line .EQ. 'End:'//delimiter) THEN
            GOTO 200
         ENDIF
      ENDDO

 201  CONTINUE
      WRITE(6,*)'close_data=> End:',delimiter,' not found, STOP'
      STOP

 200  CONTINUE
      CLOSE(ninp)
      END




      SUBROUTINE gifyfs(svar,amel,fyfs)       
C     *********************************       
C YFS formfactor       
C     *********************************       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      SAVE

      KeyNLL = MOD(KeyRad,1000)/100

      alf1  =  1d0/alfinv/pi

      bilg  =  dlog(svar/amel**2)             
      beta  =  2*alf1*(bilg-1)
      IF(KeyNLL .EQ. 0) THEN
         delb  =  beta/4d0
      ELSEIF( KeyNLL .EQ. 1) THEN
         delb  =  beta/4d0 + alf1*( -.5d0  +pi**2/3d0)
      ELSE
         WRITE(6,*) '+++++ STOP in gifyfs, wrong KeyNLL= ',KeyNLL
      ENDIF
      fyfs  =  exp(delb)                  
      END              

      FUNCTION RHOSKO(R)                    
C     ********************                  
C CALLED IN VESK1W        
C PROVIDES V OR K DISTRIBUTION TO BE GENERATED                
C     ********************                  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      PARAMETER (FLEPS = 1D-35)
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI                   
      SAVE
C MAPPING  R => VV CHANGE  TO IMPROVE ON EFFICIENCY
C Note that the replacement below makes program more precise
C and bulet-proof with respect numerical instabilities close to VV=0    
      ALF1   = 1D0/PI/ALFINV
      SVAR   = 4D0*ENE**2
      BILG   = DLOG(SVAR/AMEL**2)           
      BETI   = 2D0*ALF1*(BILG-1D0)          
      X = MAX(R,FLEPS**BETI)                
      BBT = -0.5D0 
cc      write(6,*) amaw,gammw
      CALL CHBIN1(X,BETI,BBT,VVMAX,VV,RJAC)               
C BORN XSECTION           
      SVAR1  = SVAR*(1D0-VV)                 
c ms 11/17/97
      xcrude=get_total_crude(svar1)
c ms      xcrude1=tot_born_crud(svar,svar1)
c ms      IF( abs(xcrude1/xcrude -1) .GT. 0.2d0)
c ms     $ WRITE(6,*)'1 print', xcrude1/xcrude

      DILAT=1D0           
      IF(VV.GT.VVMIN) DILAT=(1D0+1D0/SQRT(1D0-VV))/2D0        
      BETI2  = 2D0*ALF1*BILG                
      DAMEL=1D0           
      IF(VV.GT.VVMIN) DAMEL=BETI2/BETI*(VV/VVMIN)**(BETI2-BETI)
      DISTR= BETI*VV**(BETI-1D0)*DILAT*DAMEL       
      RHOSKO = RJAC*xcrude*DISTR
c      RHOSKO = RJAC*VVRHO(1,SVAR,AMEL,VV,VVMIN)
      END                 

      SUBROUTINE YFSGEN(VV,VMIN,NMAX,WT1,WT2,WT3)               
C     *******************************************               
C======================================================================
C================== Y F S G E N =======================================
C======================================================================
C*********INPUT                 
C VV    = V VARIABLE            
C VMIN  = MINIMUM V VARIABLE (INFRARED CUTOFF)  
C NMAX  = MAXIMUM PHOTON MULTIPLICITY           
C*********OUTPUT                
C WT1  = WEIGHT DUE TO NEGLECTED MASS TERMS     
C WT2  = WEIGHT DUE TO DILATATION OF PHOTON MOMENTA             
C WT3  = ANOTHER DILATATION WEIGHT              
C OTHER OUTPUT RESULTS IN /MOMSET/              
C*****************************                  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)       
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT 
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp   
      SAVE / WEKING /,/ MOMSET /,/ KeyKey /,/ WEKIN2 /
      DIMENSION XPH(100),RR(100)                
      SAVE
C               
C HERE BETI2 MUST BE USED INSTEAD OF BETI (MASS TERM NEGLECTED) 
      BETI2 = 2D0/ALFINV/PI*DLOG(4D0*ENE**2/AMEL**2)            
      AM2=(AMEL/ENE)**2         
      DO 10 I=1,NMAX            
      XPH(I)=0D0                
      DO 10 J=1,4               
   10 SPHOT(I,J)=0D0            
      IF(VV.LE.VMIN) THEN       
C NO PHOTON ABOVE DETECTABILITY THRESHOLD       
         WT1=1D0                
         WT2=1D0                
         WT3=1D0                
         NPHOT=0                
      ELSE      
C ONE OR MORE PHOTONS, GENERATE PHOTON MULTIPLICITY             
C NPHOT = POISSON(WITH AVERAGE = AVERG) + 1     
         AVERG=BETI2*DLOG(VV/VMIN)              
  100    CALL POISSG(AVERG,NMAX,MULTP,RR)       
         NPHOT = MULTP+1        
C This is for tests of program at fixed multiplicity (for adv. users)
! switch off the fixed multiplicity by hand !!!!!!!!!!!!!!!
!         NPHFIX =  MOD(KEYBRM,10000)/1000       
         nphfix = 0
! switch off the fixed multiplicity by hand !!!!!!!!!!!!!!!
         IF(NPHFIX.NE.0.AND.NPHOT.NE.NPHFIX) GOTO 100           
         IF(NPHOT.EQ.1) THEN    
            XPH(1)=VV           
            CALL BREMUL(XPH,AM2,WT1)            
            DJAC0=(1D0+1D0/SQRT(1D0-VV))/2D0    
            WT2  = 1D0/DJAC0    
            WT3  = 1D0          
         ELSE                   
            XPH(1)=VV           
            DO 200 I=2,NPHOT    
  200       XPH(I)=VV*(VMIN/VV)**RR(I-1)        
            CALL BREMUL(XPH,AM2,WT1)            
            CALL RESOLH(VV,EXPY,DJAC)           
            DJAC0=(1D0+1D0/SQRT(1D0-VV))/2D0    
            WT2  = DJAC/DJAC0   
            WT3  = 1D0          
C SCALE DOWN PHOTON ENERGIES AND MOMENTA        
            DO 300 I=1,NPHOT    
            DO 300 K=1,4        
  300       SPHOT(I,K)=SPHOT(I,K)/EXPY          
C CHECK ON LOWER ENERGY CUT-OFF                 
            IF(SPHOT(NPHOT,4).LT.VMIN) WT3 =0D0                 
         ENDIF                  
      ENDIF     
C PHOTON MOMENTA IN GEV UNITS   
      DO 420 J=1,4              
  420 SPHUM(J)=0D0              
      DO 480 I=1,NPHOT          
      DO 480 J=1,4              
      SPHOT(I,J)=SPHOT(I,J)*ENE                 
  480 SPHUM(J)=SPHUM(J)+SPHOT(I,J)              

C DEFINE FERMION MOMENTA        
C..      CALL KINEKR    ! MOVED OUTSIDE YFSGEN           
      END       
      SUBROUTINE RESOLH(VV,EXPY,DJAC)           
C     *******************************           
C THIS SOLVES CONSTRAINT EQUATION ON PHOTON MOMENTA             
C ALSO CALCULATES CORRESPONDING JACOBIAN FACTOR                 
C INPUT:  VV    = COSTRAINT PARAMETER V         
C OUTPUT  EXPY  = RESCALING FACTOR - A SOLUTION OF THE EQUATION 
C         DJAC  = JACOBIAN FACTOR               
C     ************************                  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)       
      DIMENSION PP(4),PK(4)     
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT
      SAVE   / MOMSET /
      SAVE
C               
      DO 210 K=1,4              
      PK(K)=0D0                 
  210 PP(K)=0D0                 
      PP(4)=2D0                 
      DO 215 I=1,NPHOT          
      DO 215 K=1,4              
  215 PK(K)=PK(K)+SPHOT(I,K)    
      PPDPP=PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2                 
      PKDPK=PK(4)**2-PK(3)**2-PK(2)**2-PK(1)**2                 
      PPDPK=PP(4)*PK(4)-PP(3)*PK(3)-PP(2)*PK(2)-PP(1)*PK(1)     
      AA=PPDPP*PKDPK/(PPDPK)**2                 
      EXPY=2D0*PPDPK/PPDPP/VV   
C SOLUTION FOR CONSTRAINT ON PHOTON FOUR MOMENTA                
      EXPY=EXPY*.5D0*(1D0+SQRT(1D0-VV*AA))      
C JACOBIAN FACTOR               
      DJAC=(1D0+1D0/SQRT(1D0-VV*AA))/2D0        
      END       
      SUBROUTINE BREMUL(XPH,AM2,WT)             
C     *****************************             
C PROVIDES PHOTON FOURMOMENTA   
C INPUT  : XPH    = LIST OF PHOTON ENERGIES     
C OUTPUT : SPHOT  = LIST OF PHPTON FOUR-MOMENTA                 
C          WT     = WEIGHT DUE TO MASS TERMS    
C     ************************                  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)       
      COMMON / MATPAR / PI,CEULER     
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT
      SAVE   / MOMSET /
      SAVE
      DIMENSION XPH(*),rn(1),rnumb(1)          

      WT=1D0    
      DO 100 I=1,NPHOT          
      XK=XPH(I)                 
      CALL VARRAN(RN,1)
      CALL ANGBRE(RN(1),AM2,CG,SG,DIST0,DIST1)     
      WTM   =DIST1/DIST0        
      WT    =WT*WTM             
      CALL VARRAN(RNUMB,1)
      PHI=2D0*PI*RNUMB(1)          
      SPHOT(I,1)=XK*SG*COS(PHI)                 
      SPHOT(I,2)=XK*SG*SIN(PHI)                 
      SPHOT(I,3)=XK*CG          
      SPHOT(I,4)=XK             
  100 CONTINUE                  
C======================================================================
C==================END OF YFSGEN=======================================
C======================================================================
      END       

      SUBROUTINE POISSG(AVERG,NMAX,MULT,RR)
C     ************************************** 
C Last corr. Nov. 91              
C This generates photon multipl. NPHOT according to Poisson distr. 
C INPUT:  AVERG = AVERAGE MULTIPLICITY   
C         NMAX  = MAXIMUM MULTIPLICITY   
C OUTPUT: MULT = GENERATED MULTIPLICITY 
C         RR(1:100) LIST OF ORDERED UNIFORM RANDOM NUMBERS, 
C         A BYPRODUCT RESULT, TO BE EVENTUALLY USED FOR SOME FURTHER
C         PURPOSE (I.E.  GENERATION OF PHOTON ENERGIES). 
C     ************************           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION RR(*),rn(1)                    
      COMMON / INOUT  / NINP,NOUT  
      SAVE   / INOUT  /
      SAVE
      DATA NFAIL/0/                      
   50 NN=0                               
      SUM=0D0                            
      DO 100 IT=1,NMAX                   
      CALL VARRAN(RN,1)
      Y= LOG(RN(1))                         
      SUM=SUM+Y                          
      NN=NN+1                            
      RR(NN)=SUM/(-AVERG)                
      IF(SUM.LT.-AVERG) GOTO 130         
  100 CONTINUE                           
      NFAIL=NFAIL+1                      
      IF(NFAIL.GT.100) GOTO 900          
      GOTO 50                            
  130 MULT=NN-1                         
      RETURN                             
  900 WRITE(NOUT,*) ' POISSG: TO SMALL NMAX'
      STOP                               
      END                                

      SUBROUTINE ANGBRE(RN1,AM2,COSTHG,SINTHG,DIST0,DIST1)
C     **************************************************** 
C THIS ROUTINE GENERATES PHOTON ANGULAR DISTRIBUTION 
C IN THE REST FRAME OF THE FERMION PAIR. 
C THE DISTRIBUTION IS TAKEN IN THE INFRARED LIMIT.
C GENERATES WEIGHTED EVENTS              
C INPUT:  AM2 = 4*MASSF**2/S WHERE MASSF IS FERMION MASS
C         AND S IS FERMION PAIR EFFECTIVE MASS.
C OUTPUT: COSTHG, SINTHG, COS AND SIN OF THE PHOTON 
C         ANGLE WITH RESPECT TO FERMIONS DIRECTION 
C         DIST0 = distribution  generated without m**2/(kp)**2 terms
C         DIST1 = distribution  with m**2/(kp)**2 terms 
C     *************************************** 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION RN2(1)
      SAVE
      BETA=SQRT(1.D0-AM2)                
      EPS=AM2/(1.D0+SQRT(1.D0-AM2))      
      DEL1=(2.D0-EPS)*(EPS/(2.D0-EPS))**RN1 
      DEL2=2.D0-DEL1                     
C SYMMETRIZATION                         
      CALL VARRAN(RN2,1)
      IF(RN2(1).LE.0.5D0) THEN              
        A=DEL1                           
        DEL1=DEL2                        
        DEL2=A                           
      ENDIF                              
      DIST0=1D0/DEL1/DEL2                
      DIST1=DIST0-EPS/2.D0*(1D0/DEL1**2+1D0/DEL2**2)
C CALCULATION OF SIN AND COS THETA FROM INTERNAL VARIABLES 
      COSTHG=(1.D0-DEL1)/BETA            
      SINTHG=SQRT(DEL1*DEL2-AM2)/BETA    
      END                                

      FUNCTION BREMKF(KEY,EREL)                  
C     *************************         
C NON-MONTECARLO INTEGRATION OF THE V-DISTRIBUTION            
C GAUSS METHOD, CHANGE OF VARIABLES WITH HELP OF CHBIN1       
C SEE VVDISB              
C KEY= 1,2,3,...FOR VARIOUS DISTRIBUTIONS   
C KEY= 3 FOR MC GENERATION, OTHER FOR TESTS                   
C FOR KEYFIX=1, EXEPTIONALLY, IT PROVIDES INTEGRAND AT VV=VVMAX 
C WITH BORN OMITTED       
C     ************************              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp  
C COMMON KEYDST COMMUNICATES ONLY WITH VVDISB - INTEGRAND FUNCTION 
      COMMON / KEYDST / KEYDIS              
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI                   
      SAVE / KeyKey /,/ KEYDST /,/ WEKING /,/ VVREC  /
      SAVE
      EXTERNAL VVDISB     
      DATA KEYFIX /0/
C       
      KEYDIS=KEY          
      IF(KEYFIX.EQ.0) THEN                  
! ms10/21/97 bornsc is going to be redefined !!!
! ms10/21/97         XBORN  =BORNSC(4D0*ENE**2,2)   
! ms10/21/97         write(6,*)'======bremkf=>xborn;',xborn       
! ms10/21/97         PREC=  XBORN*EREL                  
         PREC=  EREL
! ms10/21/97  
         XA= 0D0          
         XB= 1D0
cc         CALL GAUSJD(VVDISB,XA,XB,PREC,RESULT) ! switched to ADAPTIVE etc
         result =GAUS(VVDISB,XA,XB,PREC)
cc      call DGADAP(XA,XB,VVDISB,PREC,RESULT)
         BREMKF=RESULT          
      ELSE                
         SVAR  = 4D0*ENE**2
         BREMKF= VVRHO(KEYDIS,SVAR,AMEL,VVMAX,VVMIN)
     $          /VVRHO(     9,SVAR,AMEL,VVMAX,VVMIN)          
      ENDIF               
      END                 
      FUNCTION VVDISB(R)                    
C     ******************                    
C INTEGRAND FOR BREMKF    
C MAPPING XX => VV CHANGE  TO IMPROVE ON EFFICIENCY           
C     ************************              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      PARAMETER( FLEPS =1D-35)              
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI                   
      COMMON / KEYDST / KEYDIS              
      SAVE / WEKING /,/ VVREC  /,/ KEYDST /
      SAVE
C       
      KEYD=KEYDIS        
      X = MAX(R,FLEPS**BETI)                
      ALF=  BETI          
      BET=  1D0           
C ...SPECIAL CASES        
C ...Monte Carlo crude distr                
      IF    (KEYD.EQ.1)  THEN               
        BET=  -0.5D0      
C ...YFS exponentiation beta0,1,2 contribs  
      ELSEIF(KEYD.EQ.310)  THEN              
        ALF=  BETI        
      ELSEIF(KEYD.EQ.311)  THEN             
        ALF=  BETI +1     
      ELSEIF(KEYD.EQ.320)  THEN              
        ALF=  BETI        
      ELSEIF(KEYD.EQ.321)  THEN             
        ALF=  BETI +1     
      ELSEIF(KEYD.EQ.322)  THEN             
        ALF=  BETI +2     
C ...Reference distr including dilatation factor DAMEL        
      ELSEIF(KEYD.EQ.12) THEN               
        BET=  -0.5        
      ENDIF               
      CALL CHBIN1(X,ALF,BET,VVMAX,VV,RJAC) 
C BORN XSECTION           
      SVAR   = 4D0*ENE**2
      SVAR1  = SVAR*(1D0-VV)                 
cc      write(6,*)'vvdisb',alf,bet,svar,svar1,vv,ene,vvmax       
c ms 11/17/97
      xcrude=get_total_crude(svar1)
c ms      xcrude1=tot_born_crud(svar,svar1)
c ms      IF( abs(xcrude1/xcrude -1) .GT. 0.2d0)
c ms     $ WRITE(6,*)'2 print', xcrude1/xcrude
      VVDISB = VVRHO(KEYD,SVAR,AMEL,VV,VVMIN) *RJAC*xcrude        
      END            
     
      FUNCTION VVRHO(KEYDIS,SVAR,AMEL,VV,VVMIN) 
C     *****************************************
C-------------------------------------------------------------
C Convention for KEYDIS      
C     KEYDIS   =  1      crude distribution for initial state MC
C     KEYDIS   =  9      reference distr.  of YFS2 CPC paper 
C     KEYDIS   =  50-52  obsolete test distr. for YFS2 CPC paper 
C-------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / INOUT  / NINP,NOUT     
      SAVE   / INOUT  /
      SAVE
C           
      ALF1   = 1D0/PI/ALFINV
      KEYD = KEYDIS           
      BILG   = DLOG(SVAR/AMEL**2)           
      BETI   = 2D0*ALF1*(BILG-1D0)          
C===================================================================
C ---------------------- KEYD = 1 ----------------------------------
C ---- Crude distribution in YFS2 initial state Monte Carlo --------
C ------------------------------------------------------------------
c dilat is related to dilatation jacobian in yfsgen                
c damel is responsible for modification of photon ang. distribution
c see also weight wt=wt1 in   angbre                               
      IF(KEYD.GE.1.AND.KEYD.LT.100) THEN
         DILAT=1D0           
         IF(VV.GT.VVMIN) DILAT=(1D0+1D0/SQRT(1D0-VV))/2D0        
         BETI2  = 2D0*ALF1*BILG                
         DAMEL=1D0           
         IF(VV.GT.VVMIN) DAMEL=BETI2/BETI*(VV/VVMIN)**(BETI2-BETI)
C---------
         IF    (KEYD.EQ.1)  THEN               
            DISTR= BETI*VV**(BETI-1D0)*DILAT*DAMEL       
C ...Reference distribution used in YFS2 paper --------------------
         ELSEIF(KEYD.EQ. 9)  THEN   
            DISTR= BETI*VV**(BETI-1D0)*(1+(1-VV)**2)/2               
C basic reference distribution  xrefer=sigma-ref                
         ELSEIF(KEYD.EQ.50) THEN   
            DISTR= BETI*VV**(BETI-1D0)             
C XREFER TIMES DAMEL            
         ELSEIF(KEYD.EQ.51) THEN   
            DISTR= BETI*VV**(BETI-1D0)*DAMEL       
C XREFER TIMES DILATATION FACTOR DILAT          
         ELSEIF(KEYD.EQ.52) THEN   
            DISTR= BETI*VV**(BETI-1D0)*DILAT
         ENDIF       
      ELSE       
         GOTO 900             
      ENDIF      
      VVRHO = DISTR                
      RETURN    
 900  WRITE(6,*) ' ===--->  WRONG KEYDIS IN VVRHO',KEYD 
      STOP       
      END        

      subroutine brancher(sprim,itype)
!     ******************************************************    
! ###########################################
! sets probablilities of channels, can be   #
! function of sprim and iflav(4)            #
! ###########################################
      implicit DOUBLE PRECISION (a-h,o-z)
      PARAMETER (nrch=100)
! it was kanalarz
      COMMON /c_phspz/
     $                prob(nrch),
     $                fak,
     $                faki(nrch),
     $                ikan,
     $                mrchan,
     $                nrchan
      SAVE c_phspz
CBB      INCLUDE 'zz_phsp.h'

      COMMON / DECAYS / IFLAV(4), AMDEC(4) 
      COMMON / INOUT  / NINP,NOUT 
      COMMON / BXFMTS / BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      save / DECAYS /,/ INOUT  /,/ BXFMTS /   
      DIMENSION drvec(1)
c ms      parameter (NRCH=100)
c ms      DIMENSION PROB(NRCH)
      CHARACTER*5 XI(NRCH)
c ms      common /kanalarz/ fak,fakI(NRCH),ikan,MRCHAN,NRCHAN
c ms      save  /kanalarz/
      logical ifnu,ifnnu,ifnel,ifel
      save
      data istart /0/
      ifnu(k)= (abs(k).eq.12). or.(abs(k).eq.14). or.(abs(k).eq.16)
      ifnnu(k)=(abs(k).ne.12).and.(abs(k).ne.14).and.(abs(k).ne.16)
      ifel(k)= (abs(k).eq.11)
      ifnel(k)=(abs(k).ne.11)
! random choice of generation branch as function of sprime, and final state chosen.
! should be emptied from rn. generation to preserve generation series. 
! you can adopt your choice of the presampling type on sprim and iflav.
! may be one should coonect it with placer? For choice of presampling resonances?
       if (istart.eq.0) then
         MRCHAN=1
         NRCHAN=69
         nrstor=nrchan 
         
         KeySmp = MOD(KeyTek,1000)/100
         XI(1)='  1  ' 
         XI(2)='  2  ' 
         XI(3)='  3  ' 
         XI(4)='  4  ' 
         XI(5)='  5  '   
         XI(6)='  6  '   
         XI(7)='  7  ' 
         XI(8)='  8  ' 
         XI(9)='  9  ' 
         XI(10)=' 10  ' 
         XI(11)=' 11  ' 
         XI(12)=' 12  ' 
         XI(13)=' 13  ' 
         XI(14)=' 14  ' 
         XI(15)=' 15  ' 
         XI(16)=' 16  ' 
         XI(17)=' 17  ' 
         XI(18)=' 18  ' 
         XI(19)=' 19  ' 
         XI(20)=' 20  '
         XI(21)=' 21  '
         XI(22)=' 22  '
         XI(23)=' 23  '
         XI(24)=' 24  '
         XI(25)=' 25  '
         XI(26)=' 26  '
         XI(27)=' 27  '
         XI(28)=' 28  '
         XI(29)=' 29  '
         XI(30)=' 30  '
         XI(31)=' 31  '
         XI(32)=' 32  '
         XI(33)=' 33  '
         XI(34)=' 34  '
         XI(35)=' 35  '
         XI(36)=' 36  '
         XI(37)=' 37  '
         XI(38)=' 38  '
         XI(39)=' 39  '
         XI(40)=' 40  '
         XI(41)=' 41  '
         XI(42)=' 42  '
         XI(43)=' 43  '
         XI(44)=' 44  '
         XI(45)=' 45  '
         XI(46)=' 46  '
         XI(47)=' 47  '
         XI(48)=' 48  '
         XI(49)=' 49  '
         XI(50)=' 50  '
         XI(51)=' 51  '
         XI(52)=' 52  '
         XI(53)=' 53  '
         XI(54)=' 54  '
         XI(55)=' 55  '
         XI(56)=' 56  '
         XI(57)=' 57  '
         XI(58)=' 58  '
         XI(59)=' 59  '
         XI(60)=' 60  '
         XI(61)=' 61  '
         XI(62)=' 62  '
         XI(63)=' 63  '
         XI(64)=' 64  '
         XI(65)=' 65  '
         XI(66)=' 66  '
         XI(67)=' 67  '
         XI(68)=' 68  '
         XI(69)=' 69  '
       endif   
         if (KeySmp.eq.0) then
           prob(1)= 1d0
           DO k=2,nrstor
             prob(k)=.0d0
           ENDDO
           mrchan=1
           nrchan=1
         else
          prob(1)=0.40d0
          prob(2)=0!.20d0     ! zero
          prob(3)=0.80d0    ! (Z+gam)*(Z+gam) + ini pairs ? 
          prob(4)=0!.200d0    ! zero
          prob(5)=0!.20d0    ! zero
          prob(6)=0!.20d0    ! zero
          prob(7)=0.03d0    ! brem 2+3 from 4
          prob(8)=0.03d0    ! brem 2+3 from 1
          prob(9)=0.10d0    ! brem 1+4 from 3
          prob(10)=0.10d0   ! brem 1+4 from 2

          prob(11)=0.20d0  ! multiperif 1 + W-res in 3-4
          prob(12)=0.20d0  ! multiperif 1 + 4  Z-res in 2-3
          prob(13)=0!.050d0 ! out ?
          prob(14)=0.6d0   ! multiperif 1 + 3
          prob(15)=0.1d0   ! multiperif 1 + 4
          prob(16)=0.20d0   ! multiperif 4 + W-res in 3-4
          prob(17)=0.20d0   ! multiperif 4 + 1  Z-res in 3-2
          prob(18)=0!.050d0  ! out ?
          prob(19)=0.6d0    ! multiperif 4 + 2
          prob(20)=0.1d0    ! multiperif 4 + 1
          prob(21)=0.6d0    ! for zz  z(1+2) z(3+4) 
          prob(22)=0.6d0    ! for zz  z(1+4) z(2+3) 

          prob(23)=0.05d0    ! for zz  (1+2)  from 3
          prob(24)=0.05d0    ! for zz  (1+2)  from 4
          prob(25)=0.05d0    ! for zz  (3+4)  from 1 
          prob(26)=0.05d0    ! for zz  (3+4)  from 2 

          prob(27)=0.05d0    ! for zz  (1+4)  from 3
          prob(28)=0.05d0    ! for zz  (1+4)  from 2
          prob(29)=0.05d0    ! for zz  (2+3)  from 1 
          prob(30)=0.05d0    ! for zz  (2+3)  from 4 

          prob(31)=0.1       ! for zz  multiperif 1+3
          prob(32)=0.1       ! for zz  multiperif 1+4
          prob(33)=0.1       ! for zz  multiperif 2+3
          prob(34)=0.1       ! for zz  multiperif 2+4

          prob(35)=0.1       ! for zz  multiperif 3+1
          prob(36)=0.1       ! for zz  multiperif 3+2
          prob(37)=0.1       ! for zz  multiperif 4+1
          prob(38)=0.1       ! for zz  multiperif 4+2

          prob(39)=0.1       ! for zz  rev-multiperif 1+2 + Z (gamma?)
          prob(40)=0.1       ! for zz  rev-multiperif 2+1 + Z (gamma?)
          prob(41)=0.1       ! for zz  rev-multiperif 3+4 + Z (gamma?)
          prob(42)=0.1       ! for zz  rev-multiperif 4+3 + Z (gamma?)

          prob(43)=0.15d0    ! for zz  (1+2)  from 3 (e+
          prob(44)=0.15d0    ! for zz  (1+2)  from 4 (e-
          prob(45)=0.15d0    ! for zz  (3+4)  from 1 (e+
          prob(46)=0.15d0    ! for zz  (3+4)  from 2 (e-

          prob(47)=0.1       ! for zz  multiperif 1+2
          prob(48)=0.1       ! for zz  multiperif 2+1
          prob(49)=0.1       ! for zz  multiperif 3+4
          prob(50)=0.1       ! for zz  multiperif 4+3

          prob(51)=0.35d0    ! for zz  z(3+4) z(1+2) i.e. t at 3 
          prob(52)=0.35d0    ! for zz  z(1+2) z(3+4) i.e. t at 4 
          prob(53)=0.35d0    ! for zz  z(1+2) z(3+4) i.e. t at 1 
          prob(54)=0.35d0    ! for zz  z(3+4) z(1+2) i.e. t at 2 

          prob(55)=0.1       ! for zz  rev-multiperif 1+4 + Z (gamma?)
          prob(56)=0.1       ! for zz  rev-multiperif 4+1 + Z (gamma?)
          prob(57)=0.1       ! for zz  rev-multiperif 3+2 + Z (gamma?)
          prob(58)=0.1       ! for zz  rev-multiperif 2+3 + Z (gamma?)

          prob(59)=0.15d0    ! for zz  (1+4)  from 3 (e+
          prob(60)=0.15d0    ! for zz  (1+4)  from 2 (e-
          prob(61)=0.15d0    ! for zz  (2+3)  from 1 (e+
          prob(62)=0.15d0    ! for zz  (2+3)  from 4 (e-

          prob(63)=0.35d0    ! for zz  z(1+4) z(3+2) i.e. t at 1 
          prob(64)=0.35d0    ! for zz  z(3+2) z(1+4) i.e. t at 4 
          prob(65)=0.35d0    ! for zz  z(3+2) z(1+4) i.e. t at 3 
          prob(66)=0.35d0    ! for zz  z(1+4) z(3+2) i.e. t at 2 

          prob(67)=0.35d0    ! for tests for cc03 ...
          prob(68)=0.35d0    ! for tests for cc03 but zz res...
          prob(69)=0.35d0    ! for tests for cc03 zz flattened...
         endif
!         do k=1,42
!         prob(k)=0
!         enddo
!         do k=55,66
!         prob(k)=0
!         enddo
       if (istart.eq.0) then
         istart=1
        WRITE(NOUT,BXOPE) 
        WRITE(NOUT,BXTXT) '                Window X                '
        WRITE(NOUT,BXTXT) '            Brancher report             '
        if (KeySmp.eq.0)
     $  WRITE(NOUT,BXTXT) ' WARNING: KeySmp =0 Brancher is off !   '
        if (KeySmp.ne.0)
     $  WRITE(NOUT,BXTXT) '          Brancher is on now            '
         DO I=1,NRCHAN       
      WRITE(NOUT,BXL1F) prob(I),' wgt. for branch NR: ',XI(I),'X1'
         ENDDO
        WRITE(NOUT,BXTXT) ' some wgts zeroed in funct of channel   '
        WRITE(NOUT,BXTXT) '                                        '
cc ms        WRITE(NOUT,BXL1F)facwel,'effic factor for W->e-nu',' ','X4'
cc ms        WRITE(NOUT,BXL1F)faczel,'effic factor for Z->e-e ',' ','X5'
        WRITE(NOUT,BXCLO)         
       endif
!#########################################################
!#########################################################
!! here some channels can be switched off as a function of iflav(1..4)
!#########################################################
!#########################################################
!       +++++++++++++++++++++
        if (KeySmp.ne.0) then
!       +++++++++++++++++++++
!! ---  WW channels
        if (iflav(1)+iflav(2).ne.0) then
          nrchan=20
          mrchan=1
          do k=nrchan+1,nrstor
            prob(k)=0
          enddo
          if (iflav(1)+iflav(4).ne.0)                   prob( 3)=0
 
          if (ifnu(iflav(2)).or.(iflav(2).ne.-iflav(3))) prob( 7)=0
          if (ifnu(iflav(2)).or.(iflav(2).ne.-iflav(3))) prob( 8)=0
          if (ifnu(iflav(3)).or.(iflav(1).ne.-iflav(4))) prob( 9)=0
          if (ifnu(iflav(2)).or.(iflav(1).ne.-iflav(4))) prob(10)=0

          if (ifnel(iflav(1)))        then
                                       do k=11,15
                                         prob(k)=0
                                       enddo
                                      endif
          if (ifnu(iflav(3)))         prob(14)=0
 
          if (ifnel(iflav(4)))        then
                                       do k=16,20
                                          prob(k)=0
                                       enddo
                                      endif
          if (ifnu(iflav(2)))         prob(19)=0
 !! --- ZZ channels
        else
          nrchan=nrstor
          mrchan=21 ! <<<
          do k=1,20
            prob(k)=0
          enddo
          if (iflav(1)+iflav(4).ne.0) prob(22)=0
          if (ifnu(iflav(2)).or.ifnu(iflav(3))) then
            prob(23)=0
            prob(24)=0
            prob(25)=0
            prob(26)=0 

            prob(43)=0
            prob(44)=0
            prob(45)=0
            prob(46)=0
            prob(47)=0
            prob(48)=0
            prob(49)=0
            prob(50)=0
          elseif (ifel(iflav(2)).or.ifel(iflav(3))) then
            prob(23)=0
            prob(24)=0
            prob(25)=0
            prob(26)=0
            if (ifnel(iflav(3))) then
              prob(43)=0
              prob(44)=0
              prob(49)=0
              prob(50)=0
            endif
            if (ifnel(iflav(2))) then
              prob(45)=0
              prob(46)=0
              prob(47)=0
              prob(48)=0
            endif
          else
            prob(43)=0
            prob(44)=0
            prob(45)=0
            prob(46)=0
            prob(47)=0
            prob(48)=0
            prob(49)=0
            prob(50)=0
          endif
          if (ifnu(iflav(1)).or.ifnu(iflav(3)).or.
     $       (iflav(2)+iflav(3).ne.0)            ) then
            prob(27)=0
            prob(28)=0
            prob(29)=0
            prob(30)=0
          endif
          if (ifnel(iflav(1)))then
            prob(31)=0
            prob(32)=0
            prob(33)=0
            prob(34)=0
            prob(53)=0
            prob(54)=0
!  ---          prob(39)=0
!    - --      prob(40)=0
          endif
          if (ifnel(iflav(3)))then
            prob(35)=0
            prob(36)=0
            prob(37)=0
            prob(38)=0
            prob(51)=0
            prob(52)=0
!   --        prob(41)=0
!--          prob(42)=0

          endif

!          if ((ifel(iflav(1))).or.(ifel(iflav(3)))) then
!            prob(21)=0
!            prob(22)=0
!          endif
          if ((ifel(iflav(2))).and.(ifel(iflav(3)))) then 
            do k=27,30
              prob(k)=0
            enddo 
          else
            do k=55,66
              prob(k)=0
            enddo 
          endif
        endif
!       +++++++++++++++++++++
        endif
!       +++++++++++++++++++++

!#########################################################
!#########################################################
!#########################################################
!#########################################################

         prtot=0d0
         DO I=MRCHAN,NRCHAN
           prtot=prtot+prob(I)
         ENDDO
         DO I=MRCHAN,NRCHAN
           prob(I)=prob(i)/prtot
         ENDDO
!
        if(Keysmp.eq.0) then
!       ====================
         itype=1
!
        else
!       ====================
!
         CALL varran(drvec,1)
         PROBI=0D0
         DO I=MRCHAN,NRCHAN
          PROBI=PROBI+PROB(I)
          if(drvec(1).lt.probI) THEN
           itype=I
           GOTO 10
          ENDIF
         enddo
        write(*,*) 
     $ 'brancher has problem prob=',prtot
 10     continue
!
        endif
!       =====================
      end 
      subroutine kinchce(IGCL,MODE,AMAW,GAMMW,pr,SPRIM,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension bp1(4),bp2(4),bp3(4),bp4(4)
      IF (IGCL.eq.10) then
        IF(MODE.EQ.0) THEN      
         CALL KINbre(1,pr,SPRIM,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
        elseif(MODE.EQ.1) THEN
         CALL rKINbre(1,pr,SPRIM,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
        else
         write(*,*) 'kinche; wrong mode=',mode
         stop
        endif
      elseIF (IGCL.eq.9) then
        IF(MODE.EQ.0) THEN      
         CALL KINT(1,AMAW,GAMMW,pr,SPRIM,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
        elseif(MODE.EQ.1) THEN
         CALL rKINT(1,AMAW,GAMMW,pr,SPRIM,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
        else
         write(*,*) 'kinche; wrong mode=',mode
         stop
        endif
      elseIF (IGCL.eq.8) then
        IF(MODE.EQ.0) THEN      
         CALL KINT(0,AMAW,GAMMW,pr,SPRIM,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
        elseif(MODE.EQ.1) THEN
         CALL rKINT(0,AMAW,GAMMW,pr,SPRIM,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
        else
         write(*,*) 'kinche; wrong mode=',mode
         stop
        endif
      elseIF (IGCL.eq.7) then
        IF(MODE.EQ.0) THEN      
         CALL KINbre(0,pr,SPRIM,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
        elseif(MODE.EQ.1) THEN
         CALL rKINbre(0,pr,SPRIM,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
        else
         write(*,*) 'kinche; wrong mode=',mode
         stop
        endif
      elseIF (IGCL.lt.7) then
        svar=pr
        call spaceold(mode,igcl,AMAW,GAMMW,svar,sprim,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
      else
       write(*,*) 'kinche; wrong igcl=',igcl 
       stop
      endif
      end

      subroutine spaceold(mode,itype,amx,gamx,svar,sprim,fakir,
     $     bP1,amd1,bP2,amd2,bP3,amd3,bP4,amd4)
********************************************************
! ================================================================
! mode=0                                                         =
!        generates 4-momenta accordingly to itype of generation, =
!        writes them down into / bormom/.                        =
!        calculates jacobian (out from 4-momenta from / bormom/) =
! mode=1                                                         =
!        calculates jacobian (out from 4-momentafrom / bormom/)  =
!        for itype generation branch                             =
! ================================================================
      implicit DOUBLE PRECISION (a-h,o-z)
! we take just amel from this common
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / MATPAR / PI,CEULER
!!! / angles / output SOLELY for external tests (tests102.f) !!!
      COMMON / ANGLES / COSTHE,PHI,COSDE1,PHI1,COSDE2,PHI2

      save  / WEKING /,/ MATPAR /
      save  / ANGLES /
      dimension amdet(4)
      dimension bq1(4),bq2(4),bp1(4),bp2(4),bp3(4),bp4(4)
      common /nevik/ nevtru,ifprint
      save /nevik/
      SAVE
!!!
!     itype=1 WW kinematics with appropriate breit wigners to be set,
!     itype=2    kinematics with flatened breit wigners to be set, obsolete
!     itype=3    kinematics with appropriate breit wigners to be set, ini pairs
!     itype=4-6  kinematics with approp. b-wigners to set, t-chan. Obsolete?
! 
!
!#############################################################
!    GENERAL INITIALIZATION:                                 #
!    SETTING PRESAMPLER PARAMETERS AND MASSES IN ORDER       #
!    FOR PARTICULAR >ITYPE< SLOT-SPACE                       #
!    most of ITYPES differ by order of 4-vectors only        #
!#############################################################
       amdet(1)=amd1
       amdet(2)=amd2
       amdet(3)=amd3
       amdet(4)=amd4


      IF (MODE.EQ.1) THEN 
!##############################################
! INITIALIZATION FOR RECALCULATION MODE:      #
!  ANGLES AND S-i's FROM FOUR VECTORS         #
!##############################################
      if     (itype.le.3 )then
          call invkin(ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $                  amwmn,amwpn,  bp1,bp2,bp3,bp4)
      elseif (itype.le.6 )then
        call invkin(ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $                  amwmn,amwpn,  bp1,bp2,bp3,bp4)
      endif
!
      s1=amwmn**2
      s2=amwpn**2
      ENDIF

!##############################################
! BASIC PART:                                 #
! MODE=0 GENERATION AND JACOBIAN CALCULATION  #
! MODE=1 JACOBIAN CALCULATION ONLY            #
!##############################################
!... s1,s2 subspace
      if (itype.eq.1) then
       CALL RES2GN(mode,SVAR,SPRIM,AMx,GAMx,amdet,S1,S2,SSCRU)
      else
       CALL RES3GN(mode,itype,SVAR,SPRIM,AMx,GAMx,amdet,S1,S2,SSCRU)
      endif
      if (ifprint.eq.1) then
      write(*,*) 'invariants old-t ',s1,s2,sscru
      endif
      if (mode.eq.10.and.itype.eq.3) then
        write(*,*) 'itype=',itype
        write(*,*) 'invariants old-t ',s1,s2,sscru
      endif 
!
! rejection
      if(mode.eq.0.and.sscru.eq.0d0)  then
!-- short-out ... 
         fakir=0D0
         return
      endif

! lambda factors, can be moved to res2/3-s or even out.
      x1=s1/sprim
      x2=s2/sprim
      bmain=sqrt( (1-x1-x2)**2 - 4*x1*x2 )
      xwm1=amdet(1)**2/s1
      xwm2=amdet(2)**2/s1
      bwm=sqrt( (1-xwm1-xwm2)**2 - 4*xwm1*xwm2 )
      xwp1=amdet(3)**2/s2
      xwp2=amdet(4)**2/s2
      bwp=sqrt( (1-xwp1-xwp2)**2 - 4*xwp1*xwp2 )
      wjac=bmain*bwp*bwm

!... production angles
      if (itype.eq.1) then
       CALL cospro(mode,sprim,s1,s2,ctn,fin,xccos)
      else
       CALL cosprozz(mode,sprim,s1,s2,ctn,fin,xccos)
      endif
      if (ifprint.eq.1) then
      write(*,*) 'prod',ctn,xccos
      endif

! decay angles
      if (itype.le.3 )then
       ifl=1
       if (itype.eq.3 ) ifl=0
       CALL cosdecc(mode,ifl,sprim,ct1n,fi1n,xccos1)
       CALL cosdecc(mode,ifl,sprim,ct2n,fi2n,xccos2)

      else
       if (s1.gt.s2) then
        ifak=1
        if (ctn.lt.0d0) ifak=-1

        CALL cosdec_t(mode, 1,svar,sprim,s1,s2,ctn,fin,amel,
     @              amdet(1),amdet(2),ct1n,fi1n,xccos1)
        CALL cosdecc(mode,0,sprim,ct2n,fi2n,xccos2)
       else
 2        ifak=1
        if (ctn.lt.0d0) ifak=-1
        CALL cosdecc(mode,ifak,sprim,ct1n,fi1n,xccos1)
        CALL cosdec_t(mode,-1,svar,sprim,s2,s1,ctn,fin,amel,
     @              amdet(3),amdet(4),ct2n,fi2n,xccos2)
       endif
      endif
      if (ifprint.eq.1) then
      write(*,*) 'dec1 ',ct1n,xccos1
      write(*,*) 'dec2 ',ct2n,xccos2
      endif

!... WE CALCULATE OVERALL JACOBIAN ...
      fak= 1D0/32D0*xccos1*xccos2*xccos*sscru*wjac
!...  EN_pi=(2*pi)**4/(2*(2*PI)**3)**(r;r=4) 
      ENPI=(2*PI)**4/(2*(2*PI)**3)**4
      fakir=fak*ENPI
      if (ifprint.eq.1) then
      write(*,*) 'fakir ',fakir
      endif



      if (mode.eq.0) then 
!##############################################
!  KONSTRUCT FINAL 4-VECTORS etc:             #
!  MODE=0 ONLY                                #
!##############################################
!
      if(itype.le.3) then
       CALL kineww(sprim,ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $           sqrt(s1),sqrt(s2),amdet,bq1,bq2,bp1,bp2,bp3,bp4)
!
      elseif(itype.le.6) then
       CALL kineww(sprim,ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $           sqrt(s1),sqrt(s2),amdet,bq1,bq2,bp1,bp2,bp3,bp4)
      else
       write(6,*) 'spacegen: wrong itype=',itype
       stop
      endif        
      endif   

! security check
      if (fak.eq.0d0) then
       write(*,*) 'spacegen: we have troubles; fakir=',
     $  xccos1,xccos2,xccos,sscru,wjac
       write(*,*) 'please contact Z.Was '
       write(*,*) 'this effect is irrelevant or serious ...'
       write(*,*) 'spacegen: itype=',itype,' mode=',mode
       write(*,*) 'division by zero will be protected'
       write(*,*) 'bp1=',bp1
       write(*,*) 'bp2=',bp2
       write(*,*) 'bp3=',bp3
       write(*,*) 'bp4=',bp4
       write(*,*) amdet
       write(*,*) sqrt(s1),sqrt(s2)
!... WE Re-CALCULATE OVERALL JACOBIAN ...
!... this event was outside phase space, the only justifiable config.
!... is if s1 or s2 is just under threshold defined by electron and neutrino
!... masses
      sscru=1d-40
      fak= 1D0/32D0*xccos1*xccos2*xccos*sscru*wjac
!...  EN_pi=(2*pi)**4/(2*(2*PI)**3)**(r;r=4) 
      ENPI=(2*PI)**4/(2*(2*PI)**3)**4
      fakir=fak*ENPI
      endif

      end

      SUBROUTINE res3gn(mode,itype,svar,sprim,rmas,rgam,amdec,s1,s2,wt)
!     ***************************************************************
! Generation of ds_1ds_2 distribution within phase space boundaries
! using weighted (pre-sampled) events
!---------------------
! note:
! so far generation is within theta_crude and fine tuning is added at the
! end. For non-acceptable events weight is set to zero.
!---------------------
! breit-wigners pre-samplers in both s_1 and s_2 channels are set.
! total volume 'prnorm' ( S(s') defined in formula 31 of koralw 1.02 manual) 
! is calculated including additional W(s_1)*W(s_2) factor 
! (see koralw 1.02 manual). To obtain proper ds_1ds_2 distribution
! weight wt=prnorm/W(s_1)/W(s_2) must be included, and this will help later
! cancelling singularities of matrix element
! 
! note: both resonances have the same mass distribution function
!         svar    - max sprim
!         sprim   - actual s
!         rmas    - central value of a resonance mass distribution
!         rgam    - width of a resonance
! OUTPUT: s1, s2  - svar's of two resonances
!         wt      - weight
! for mode=1
! INPUT:  s1, s2  - no generation,  just calculation of weight. 
!
! Written by: M. Skrzypek            date: 2/16/95
! Last update: 5/5/96                  by: Z. Was
!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / matpar / pi,ceuler
! This common contains parameters of non-established particles such as higgs
! Which need to be used by pre-sampler (to be activated by dipswitch IHIG
      COMMON / WEKIN3 / AMHIG,GAMHIG,IHIG
      SAVE / matpar /,/ WEKIN3 /     
      DOUBLE PRECISION AMDEC(4),amd(4)
      DOUBLE PRECISION drvec(100)
      SAVE
      do k=1,4
       amd(k)=amdec(k)
       if (amd(k).lt.0.0005d0) amd(k)=0.000511d0
      enddo
      POLD=0.3
!      if (itype.eq.3) pold=.7
      proa=0.5
      if (itype.eq.5) proa=0.2d0
      if (itype.eq.6) proa=0.8d0
      ALP2=ATAN((sprim-rmas**2)/rmas/rgam)
      ALP1=ATAN(((amd(1)+amd(2))**2-rmas**2)/rmas/rgam)
      BLP2=ATAN((sprim-rmas**2)/rmas/rgam)
      BLP1=ATAN(((amd(3)+amd(4))**2-rmas**2)/rmas/rgam)
      IF (IHIG.EQ.1) THEN
       CLP2=ATAN((sprim-AMHIG**2)/AMHIG/GAMHIG)
       CLP1=ATAN(((amd(1)+amd(2))**2-AMHIG**2)/AMHIG/GAMHIG)
       DLP2=ATAN((sprim-AMHIG**2)/AMHIG/GAMHIG)
       DLP1=ATAN(((amd(3)+amd(4))**2-AMHIG**2)/AMHIG/GAMHIG)
       PROB1=1D0/3D0
       PROB2=2D0/3D0
       PROB3=2D0/3D0
       PROB4=1D0
      ELSE
       CLP2=ATAN((sprim-rmas**2)/rmas/rgam)
       CLP1=ATAN(((amd(1)+amd(2))**2-rmas**2)/rmas/rgam)
       DLP2=ATAN((sprim-rmas**2)/rmas/rgam)
       DLP1=ATAN(((amd(3)+amd(4))**2-rmas**2)/rmas/rgam)
       PROB1=1D0/2D0
       PROB2=1D0/2D0
       PROB3=1D0/2D0
       PROB4=1D0
      ENDIF
      biglog1=log(sprim/(amd(1)+amd(2))**2)
      biglog2=log(sprim/(amd(3)+amd(4))**2)
!
!     ====================
      if (mode.ne.1) then 
!     ====================
!
 10   call varran(drvec,7)
      r1=drvec(1)
      r2=drvec(2)
      r3=drvec(3)
      r4=drvec(4)
      r5=drvec(5)
      r6=drvec(6)
      r7=drvec(7)
      IF(r6.lt.POLD) THEN
!! ########################################
      if(r3.lt.PROB1) then      
        ALP=ALP1+R1*(ALP2-ALP1)
        s1=rmas**2+rmas*rgam*TAN(ALP)
      elseif(r3.lt.PROB2) then  
        CLP=CLP1+R1*(CLP2-CLP1)
        s1=AMHIG**2+AMHIG*GAMHIG*TAN(DLP)    
      elseif(r3.lt.PROB3) then      
        s1=(sprim-(amd(1)+amd(2))**2)*r1+(amd(1)+amd(2))**2 
      else
        s1=(amd(1)+amd(2))**2*exp(r1*biglog1)
      endif
      if(r4.lt.PROB1) then   
        ALP=BLP1+R2*(BLP2-BLP1)
        s2=rmas**2+rmas*rgam*TAN(ALP)
      elseif(r4.lt.PROB2) then  
        DLP=DLP1+R2*(DLP2-DLP1)
        s2=AMHIG**2+AMHIG*GAMHIG*TAN(DLP) 
      elseif(r4.lt.PROB3) then   
        s2=(sprim-(amd(3)+amd(4))**2)*r2+(amd(3)+amd(4))**2
      else
        s2=(amd(3)+amd(4))**2*exp(r2*biglog2)
      endif
      ELSE ! PNEW !!!
      if(r7.lt.proa) then
      xx=4*(amd(1)+amd(2))**2/sprim
      beta=sqrt(1d0-4*(amd(1)+amd(2))**2/sprim)
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
        u=(log((1D0+beta)/4D0))**2 +xlog*xlog1*r1
        costhe=-1D0/beta*(4D0*EXP(-SQRT(u))-1)
        s1=(sprim-(amd(1)+amd(2))**2)*(1d0-costhe)/2
     $    +(amd(1)+amd(2))**2
!        write(*,*) s1
!        biglog1=log(sprim/(amd(1)+amd(2))**2)
!        s1=(amd(1)+amd(2))**2*exp(r1*biglog1)
        spri1=(sqrt(sprim)-sqrt(s1))**2
        biglo1=log(spri1/(amd(3)+amd(4))**2)
        if(r7.lt.proa/2) then
         s2=(amd(3)+amd(4))**2*exp(r2*biglo1)
         s2=spri1+(amd(3)+amd(4))**2-s2
        else
         s2=(spri1-(amd(3)+amd(4))**2)*r2+(amd(3)+amd(4))**2
        endif

      else
      xx=4*(amd(3)+amd(4))**2/sprim
      beta=sqrt(1d0-4*(amd(3)+amd(4))**2/sprim)
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
        u=(log((1D0+beta)/4D0))**2 +xlog*xlog1*r1
        costhe=-1D0/beta*(4D0*EXP(-SQRT(u))-1)
        s2=(sprim-(amd(3)+amd(4))**2)*(1d0-costhe)/2
     $    +(amd(3)+amd(4))**2

!        biglog2=log(sprim/(amd(3)+amd(4))**2)
!        s2=(amd(3)+amd(4))**2*exp(r1*biglog2)
        spri2=(sqrt(sprim)-sqrt(s2))**2
        biglo2=log(spri2/(amd(1)+amd(2))**2)
        if(r7.gt.proa+(1-proa)/2) then
         s1=(amd(1)+amd(2))**2*exp(r2*biglo2)
         s1=spri2+(amd(1)+amd(2))**2-s1
        else
         s1=(spri2-(amd(1)+amd(2))**2)*r2+(amd(1)+amd(2))**2
        endif

      endif
      ENDIF
!     =====
      endif
!     =====
      ph1c=(sprim-(amdec(1)+amdec(2))**2)
      ph2c=(sprim-(amdec(3)+amdec(4))**2)
!
      PH1a=((s1-rmas**2)**2+(rmas*rgam)**2)/(rmas*rgam)
      PH1a=PH1a*(ALP2-ALP1)
      PH2a=((s2-rmas**2)**2+(rmas*rgam)**2)/(rmas*rgam)
      PH2a=PH2a*(BLP2-BLP1)
!
      IF (IHIG.EQ.1) THEN
       PH1b=((s1-AMHIG**2)**2+(AMHIG*GAMHIG)**2)/(AMHIG*GAMHIG)
       PH1b=PH1b*(CLP2-CLP1)
       PH2b=((s2-AMHIG**2)**2+(AMHIG*GAMHIG)**2)/(AMHIG*GAMHIG)
       PH2b=PH2b*(DLP2-DLP1)
      ELSE
       PH1b=((s1-rmas**2)**2+(rmas*rgam)**2)/(rmas*rgam)
       PH1b=PH1b*(CLP2-CLP1)
       PH2b=((s2-rmas**2)**2+(rmas*rgam)**2)/(rmas*rgam)
       PH2b=PH2b*(DLP2-DLP1)
      ENDIF
!      
      ph1d=s1*biglog1
      ph2d=s2*biglog2
!!!
      ph1=1/( PROB1       /ph1a+(PROB2-PROB1)/ph1b
     $      +(PROB3-PROB2)/ph1c+(PROB4-PROB3)/ph1d)
      ph2=1/( PROB1       /ph2a+(PROB2-PROB1)/ph2b
     $      +(PROB3-PROB2)/ph2c+(PROB4-PROB3)/ph2d)
!      ph1=3/(1d0/ph1a+1d0/ph1b+1d0/ph1c)
!      ph2=3/(1d0/ph2a+1d0/ph2b+1d0/ph2c)
      prn=ph1*ph2

!####################################################
       biglog1=log(sprim/(amd(1)+amd(2))**2)
       ph1cc=s1*biglog1                           ! s1 -->
c to jest zlosliwe swinstwo !! rozklad musi byc 1/E*ln(s1/E**2)
c co powoduje, ze rozbieznosc jest nieco `przydymiona'.
      xx=4*(amd(1)+amd(2))**2/sprim
      beta=sqrt(1d0-4*(amd(1)+amd(2))**2/sprim)
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
      cost=1-2*(s1-(amd(1)+amd(2))**2)/(sprim-(amd(1)+amd(2))**2)
      xccos=1d0/2d0*beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-cost)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-cost))))
       ph1cc=(sprim-(amd(1)+amd(2))**2)/xccos/2
       spri1=(sqrt(sprim)-sqrt(s1))**2
       biglo1=log(spri1/(amd(3)+amd(4))**2)
       ph2cc=1/(0.5/((spri1+(amd(3)+amd(4))**2-s2)*biglo1)
     $         +0.5/(spri1-(amd(3)+amd(4))**2))           !    --> s2 

       biglog2=log(sprim/(amd(3)+amd(4))**2)
       ph1cd=s2*biglog2                           ! s2 --> 
      xx=4*(amd(3)+amd(4))**2/sprim
      beta=sqrt(1d0-4*(amd(3)+amd(4))**2/sprim)
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
      cost=1-2*(s2-(amd(3)+amd(4))**2)/(sprim-(amd(3)+amd(4))**2)
      xccos=1d0/2d0*beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-cost)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-cost))))
       ph1cd=(sprim-(amd(3)+amd(4))**2)/xccos/2

       spri2=(sqrt(sprim)-sqrt(s2))**2
       biglo2=log(spri2/(amd(1)+amd(2))**2)
       ph2cd=1/(0.5/((spri2+(amd(1)+amd(2))**2-s1)*biglo2) 
     $         +0.5/(spri2-(amd(1)+amd(2))**2))           !    --> s1 

       prx=1d0/(proa/(ph1cc*ph2cc)+(1d0-proa)/(ph1cd*ph2cd))
      prnorm=1d0/(pold/prn+(1d0-pold)/prx)
      wt=prnorm
!####################################################
! thresholds
      IF(sqrt(s1)+sqrt(s2).gt.sqrt(sprim)) THEN
        wt=0d0
      ENDIF
!-- check thresholds on decays
      IF(amdec(1)+amdec(2).gt.sqrt(s1)) THEN
        wt=0D0
      ENDIF
      IF(amdec(3)+amdec(4).gt.sqrt(s2)) THEN
        wt=0D0
      ENDIF
      if(mode.eq.1.and.wt.eq.0d0) then
      write(*,*) 'RES3-mode=1 vol=',ph1,ph2
      write(*,*) sqrt(s1),'+',sqrt(s2),'.gt.',sqrt(sprim)
      write(*,*) amdec
      wt=prnorm
      endif
      END

      subroutine invx(s,sprim,ct,fi,ct1,fi1,ct2,fi2,
     $                  amwm,amwp,amdec,  q1,q2,p1,p2,p3,p4)
      implicit DOUBLE PRECISION (a-h,o-z)
! ... this routine is for tests of invkin only. It is nat called at all
      COMMON / cms_eff_momdec /
     $      effbeam1(4),effbeam2(4),bp1(4),bp2(4),bp3(4),bp4(4)

      dimension amdec(4),q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      dimension rp1(4),rp2(4),rp3(4),rp4(4)
c to activate dumps KINDMP=1
      KINDMP=0

      IF(KINDMP.EQ.1) THEN
      write(*,*) '================================================='
      write(*,*) '============     begin     ======================'
      write(*,*) 'we want:  ct2=', ct2,'  fi2=', fi2
      write(*,*) 'we want:  ct1=', ct1,'  fi1=', fi1
      write(*,*) 'we want:   ct=',  ct,'   fi=', fi
      write(*,*) 'we want: amwm=',amwm,' amwp=',amwp
      write(*,*) '-------------------------------------------------'
      endif
!!!
      do k=1,4
      rp1(k)=bp1(k)
      rp2(k)=bp4(k)
      rp3(k)=bp3(k)
      rp4(k)=bp2(k)
      enddo
      call invkintt(ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $                  amwmn,amwpn,  rp1,rp2,rp3,rp4)
!
      IF(KINDMP.EQ.1) THEN
      write(*,*) 'we  get:  ct2=', ct2n,'  fi2=', fi2n
      write(*,*) 'we  get:  ct1=', ct1n,'  fi1=', fi1n
      write(*,*) 'we  get:   ct=',  ctn,'   fi=', fin
      write(*,*) 'we  get: amwm=',amwmn,' amwp=',amwpn
      write(*,*) '============       end     ======================'
      write(*,*) '================================================='
      endif
      end

      SUBROUTINE RKINBRE(ift,ple,
     $ SVAR,DGAMT,PN,amnuta,PIM2,AMP1,PIM1,AMP2,PIPL,AMP3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION  PT(4),PN(4),PAA(4),PIM1(4),PIM2(4),PIPL(4)
      DIMENSION  PR(4),PBST(4)
      common /nevik/ nevtru,ifprint
      save /nevik/
      SAVE
      DATA PI /3.141592653589793238462643D0/
      XLAM(X,Y,Z)=SQRT(ABS((X-Y-Z)**2-4.0D0*Y*Z))


C
C FOUR BODY PHASE SPACE NORMALISED AS IN BJORKEN-DRELL
C D**3 P /2E/(2PI)**3 (2PI)**4 DELTA4(SUM P)
      PHSPAC=1.D0/2**17/PI**8
      amtau=sqrt(svar)
      amro=100
      gamro=50
C
      DO K=1,4
        PAA(K)=PIM1(K)+PIM2(K)+PIPL(K)
        PR(K) =PIM1(K)+PIPL(K)
        pt(k) =PIM1(K)-PIM2(K)+PIPL(K)
      ENDDO
C
C.. MASS OF two
        AMS1=(AMP2+AMP3)**2
        AMS2=(AMTAU-AMNUTA-amp1)**2
        AM2SQ=dmas2(PR)
        AM2 =SQRT(AM2SQ)
!       PHSPAC=PHSPAC*(AMS2-AMS1)
        B=LOG(AMS1)
        A=LOG(AMS2)
        AM2 =SQRT(AM2SQ)
!        PHSPAC=PHSPAC*AM2SQ*(A-B)
C.. MASS OF two
        prob1=.3
        prob2=.3
        PROB3=.0
        PROB4=.4
        AMS1=(AMP2+AMP3)**2
        AMS2=(AMTAU-AMNUTA-amp1)**2
        AM2SQ=dmas2(PR)
        AM2 =SQRT(AM2SQ)
        XJ1=(AMS2-AMS1)
C PHASE SPACE WITH SAMPLING FOR RHO RESONANCE
         B=LOG(AMS1)
         A=LOG(AMS2)
        xj2=AM2SQ*(A-B)

      ALP1=ATAN((AMS1-AMRO**2)/AMRO/GAMRO)
      ALP2=ATAN((AMS2-AMRO**2)/AMRO/GAMRO)
      xj3=((AM2SQ-AMRO**2)**2+(AMRO*GAMRO)**2)/(AMRO*GAMRO)
      xj3=xj3*(ALP2-ALP1)

        n=1
        xj4=am2SQ**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)
!      write(*,*) 't',phspac,xj1,xj2,xj3,xj4,prob1,prob2,prob3,prob4
        PHSPAC=PHSPAC/(PROB1/XJ1+PROB2/XJ2+PROB3/XJ3+PROB4/XJ4)
C.. mass of three
        AMS1=(AMP1+am2)**2
        AMS2=(AMTAU-AMNUTA)**2
        AM3SQ =dmas2(PAA)
        AM3 =SQRT(AM3SQ)
!        PHSPAC=PHSPAC*(AMS2-AMS1)
        B=LOG(AMS1)
        A=LOG(AMS2)
!!        PHSPAC=PHSPAC*AM3SQ*(A-B)
        prbam3=1d0
        AM3SQb =ams2-am3sq+ams1
        PHSPAC=PHSPAC/(prbam3/(AM3SQ*(A-B))+(1-prbam3)/(AM3SQb*(A-B)))
        ENQ1=(AM2SQ-AMP2**2+AMP3**2)/(2*AM2)
        PPPI=SQRT(ABS(ENQ1**2-AMP3**2))
        PHSPAC=PHSPAC*(4*PI)*(2*PPPI/AM2)
C
        PR4=1.D0/(2*AM3)*(AM3**2+AM2**2-AMP1**2)
        PR3= SQRT(ABS(PR4**2-AM2**2))
        PHSPAC=PHSPAC*(2*PR3/AM3)

        PAA4=1.D0/(2*AMTAU)*(AMTAU**2-AMNUTA**2+AM3**2)
        PAA3= SQRT(ABS(PAA(4)**2-AM3**2))
        PHSPAC=PHSPAC*(4*PI)*(2*PAA3/AMTAU)
        pim24=1.D0/(2*AM3)*(AM3**2-AM2**2+AMP1**2)
!... versor of two in three restframe
      PNPAA=dot(pn,paa)
      DO K=1,4
         pt(k) =( (PIM1(K)-PIM2(K)+PIPL(K))
     $           -PAA(K)*(pr4-pim24)/am3   )/2/PR3
         PBST(K)=PN(K)-PAA(K)*PNPAA/am3**2
      ENDDO
        cost=dot(pt,pbst)/sqrt(-dmas2(pbst))
cc ms 03.07.97        if(sqrt(-dmas2(pt)).gt.1d0) cost=cost/sqrt(-dmas2(pt))
        if(sqrt(dabs(-dmas2(pt))).gt.1d0) 
     $       cost=cost/sqrt(dabs(-dmas2(pt)))
! m.s. 8/24/98 beg
        IF(cost.LT.-1d0) cost=-1d0
        IF(cost.GT. 1d0) cost= 1d0
! m.s. 8/24/98 end
        thet=acos(cost)
        costhe=cost
        EPS=(AM2/AM3)**2
        XL1=LOG((2+EPS)/EPS)
        XL0=LOG(EPS)
        etaa=1+eps+cost
        etab=1+eps-cost
        prb=1d0
        PHSPAC=PHSPAC/(prb/(XL1/2*ETAa)+(1d0-prb)/(XL1/2*ETAb))
      PHSPAC=PHSPAC*(4*PI)
!        write(*,*) 'thet inv=',thet,eps,xl1,eta,cost
      cthx=paa(3)/sqrt(paa(1)**2+ paa(2)**2+ paa(3)**2)
      thet=acos(cthx)

        EPS=(AM2/AMtau)**2
        EPS=(AMnuta/Pn(4))**2
        EPS=(AMnuta/AMtau)**2*max((am3/amtau)**2,1d-4)
        EPS=(am3/amtau)**2
        XL1=LOG((2+EPS)/EPS)
        XL0=LOG(EPS)
      eta1=1+eps+cthx
      eta2=1+eps-cthx
      if (ift.eq.0) then
      ff=1d0
      else
      FF=1d0/3d0/1d0+2*ple/3d0/(XL1/2*ETA1)+2*(1d0-ple)/3d0/(XL1/2*ETA2)
      pleft=ple
        prob1=.2
        prob2=Pleft*.4d0
        prob3=(1D0-PLEFT)*.4D0
        prob4=Pleft*.4d0
        prob5=(1D0-PLEFT)*.4D0

      FF=0d0/3d0/1d0+pleft/(XL1/2*ETA1)+(1d0-pleft)/(XL1/2*ETA2)
      beta=sqrt(1d0-eps)
      xx=eps
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
      ct=-cthx

      xccos1=beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))
      ct=cthx
      xccos2=beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))

        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
        n=1
        AM2SQX= CT+2D0-sqrt(1d0-eps)
        xj4=am2SQX**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)/2
        n=1
        AM2SQX=-CT+2D0-sqrt(1d0-eps)
        xj5=am2SQX**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)/2

 !     FF=0d0/3d0/1d0+pleft*xccos1+(1d0-pleft)*xccos2
      FF=PROB1/1d0+PROB2*xccos1+PROB3*xccos2+PROB4/XJ4+PROB5/XJ5
      endif
!      write(*,*) 'inv  ff=',ff,xl1,eta1,eta2,eps,cthx
!      write(*,*) '     thet=',thet,cthx
      PHSPAC=PHSPAC/FF
      DGAMT=PHSPAC
       if (ifprint.eq.1) write(*,*) 'rkinbre dgamt=',dgamt
      END

      SUBROUTINE KINBRE(ift,ple,
     $ SVAR,DGAMT,PN,amnuta,PIM2,AMP1,PIM1,AMP2,PIPL,AMP3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION  PN(4),PAA(4),PIM1(4),PIM2(4),PIPL(4)
      DIMENSION  PR(4)
      REAL*4 RRR(14)
      common /nevik/ nevtru,ifprint
      save /nevik/
      real*4 rrx
      common /erery/ rrx(14)
      save /erery/
      SAVE
      DATA PI /3.141592653589793238462643D0/
      XLAM(X,Y,Z)=SQRT(ABS((X-Y-Z)**2-4.0D0*Y*Z))

C
C FOUR BODY PHASE SPACE NORMALISED AS IN BJORKEN-DRELL
C D**3 P /2E/(2PI)**3 (2PI)**4 DELTA4(SUM P)
      PHSPAC=1.D0/2**17/PI**8
      amtau=sqrt(svar)
C
!      nn=1000000
!      sum=0
!      sum2=0
!      do ll=1,nn
CBB      CALL RANMAR(RRR,14)
       CALL VARRAN(RRR,14)
      do k=1,14
      rrx(k)=rrr(k)
      enddo
C
      amro=100
      gamro=50
C.. MASS OF two
        RR2=RRR(2)
        AMS1=(AMP2+AMP3)**2
        AMS2=(AMTAU-AMNUTA-amp1)**2
!        AM2SQ=AMS1+   RR2*(AMS2-AMS1)
!        AM2 =SQRT(AM2SQ)
!        PHSPAC=PHSPAC*(AMS2-AMS1)
        B=LOG(AMS1)
        A=LOG(AMS2)
        AM2SQ=AMS2*EXP((B-A)*RR2)
        AM2 =SQRT(AM2SQ)
        prob1=.3
        prob2=.3
        PROB3=.0
        PROB4=.4
        RR2=RRR(2)
        AMS1=(AMP2+AMP3)**2      
        AMS2=(AMTAU-AMNUTA-amp1)**2
        ALP1=ATAN((AMS1-AMRO**2)/AMRO/GAMRO)
        ALP2=ATAN((AMS2-AMRO**2)/AMRO/GAMRO)
        IF (RRR(12).LT.PROB1) THEN
         AM2SQ=AMS1+   RR2*(AMS2-AMS1)
         AM2 =SQRT(AM2SQ)
        elseIF (RRR(12).LT.(PROB1+PROB2)) THEN  
         B=LOG(AMS1)
         A=LOG(AMS2)
         AM2SQ=AMS2*EXP((B-A)*RR2)
         AM2 =SQRT(AM2SQ)     
        ELSEIF (RRR(12).LT.(PROB1+PROB2+PROB3)) THEN
         ALP=ALP1+RR2*(ALP2-ALP1)
         AM2SQ=AMRO**2+AMRO*GAMRO*TAN(ALP)
         AM2 =SQRT(AM2SQ)
        ELSE
         n=1
          if(n.eq.1) then
         AM2SQ=AMS1/(1D0-RR2*(1-(ams1/ams2)**n))
          elseif(n.eq.2) then
         AM2SQ=AMS1/sqrt(1D0-RR2*(1-(ams1/ams2)**n))
          else
         AM2SQ=AMS1*(1D0-RR2*(1-(ams1/ams2)**n))**(-1d0/n)
          endif
         AM2 =SQRT(AM2SQ)
         if (am2sq.gt.ams2) WRITE(*,*) 'am2sq',am2sq,ams1,ams2,rr2
         if (am2sq.gt.ams2) stop
         if (am2sq.lt.ams1) WRITE(*,*) 'am2sq',am2sq,ams1,ams2,rr2
         if (am2sq.lt.ams1) stop

        ENDIF
        XJ1=(AMS2-AMS1)
         B=LOG(AMS1)
         A=LOG(AMS2)
        xj2=AM2SQ*(A-B)
        xj3=((AM2SQ-AMRO**2)**2+(AMRO*GAMRO)**2)/(AMRO*GAMRO)
        xj3=xj3*(ALP2-ALP1)
        n=1
        xj4=am2SQ**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)
!        sum=Sum+1d0/(PROB1/XJ1+PROB2/XJ2+PROB3/XJ3+PROB4/XJ4)
!        sum2=Sum2+1d0/(PROB1/XJ1+PROB2/XJ2+PROB3/XJ3+PROB4/XJ4)**2
!        enddo
!        sum=sum/nn
!        sum2=sum2/nn
!        err=sqrt((sum2-sum**2)/nn)
!        write(*,*) sum,'+-',err
!        write(*,*) '28761.2547837270613 +- 0'
!        stop
        PHSPAC=PHSPAC/(PROB1/XJ1+PROB2/XJ2+PROB3/XJ3+PROB4/XJ4)

!        PHSPAC=PHSPAC*AM2SQ*(A-B)
C.. mass of three
        RR1=RRR(1)
        AMS1=(AMP1+am2)**2
        AMS2=(AMTAU-AMNUTA)**2
!        AM3SQ =AMS1+   RR1*(AMS2-AMS1)
!        AM3 =SQRT(AM3SQ)
!        PHSPAC=PHSPAC*(AMS2-AMS1)
        B=LOG(AMS1)
        A=LOG(AMS2)
        prbam3=1d0
        if(rrr(10).lt.prbam3) then
         AM3SQ=AMS2*EXP((B-A)*RR1)
         AM3SQb=ams2-am3sq+ams1
        else
         AM3SQ=AMS2*EXP((B-A)*RR1)
         am3sqb=am3sq
         AM3SQ =ams2-am3sq+ams1
        endif
        AM3 =SQRT(AM3SQ)
        PHSPAC=PHSPAC/(prbam3/(AM3SQ*(A-B))+(1-prbam3)/(AM3SQb*(A-B)))
* two RESTFRAME, DEFINE PIPL AND PIM1
        ENQ1=(AM2SQ-AMP2**2+AMP3**2)/(2*AM2)
        ENQ2=(AM2SQ+AMP2**2-AMP3**2)/(2*AM2)
        PPI=         ENQ1**2-AMP3**2
        PPPI=SQRT(ABS(ENQ1**2-AMP3**2))
        PHSPAC=PHSPAC*(4*PI)*(2*PPPI/AM2)
* PIPL  MOMENTUM IN TWO REST FRAME,RRR(7),RRR(8)
        THET =ACOS(-1.D0+2*RRR(7))
        PHI = 2*PI*RRR(8)
!ms 03.sept.97
c        PIPL(1)=D0
c        PIPL(2)=D0
        PIPL(1)=0D0
        PIPL(2)=0D0
!ms 03.sept.97
        PIPL(3)=PPPI
        PIPL(4)=ENQ1
        CALL ROTPOD(THET,PHI,PIPL)
* PIM1 MOMENTUM IN TWO REST FRAME
        DO 30 I=1,3
 30     PIM1(I)=-PIPL(I)
        PIM1(4)=ENQ2
* three REST FRAME, DEFINE momentum of two
*       two  MOMENTUM
        PR(1)=0
        PR(2)=0
        PR(4)=1.D0/(2*AM3)*(AM3**2+AM2**2-AMP1**2)
        PR(3)= SQRT(ABS(PR(4)**2-AM2**2))
        PPI  =          PR(4)**2-AM2**2
*       PIM2 MOMENTUM
        PIM2(1)=0
        PIM2(2)=0
        PIM2(4)=1.D0/(2*AM3)*(AM3**2-AM2**2+AMP1**2)
        PIM2(3)=-PR(3)
        PHSPAC=PHSPAC*(2*PR(3)/AM3)
* PIPL PIM1 BOOSTED FROM two REST FRAME TO three REST FRAME
      EXE=(PR(4)+PR(3))/AM2
      CALL BOSTd3(EXE,PIPL,PIPL)
      CALL BOSTd3(EXE,PIM1,PIM1)
      rr3=rrr(3)
      rr4=rrr(4)
        EPS=(AM2/AM3)**2
        XL1=LOG((2+EPS)/EPS)
        XL0=LOG(EPS)

        ETA  =EXP(XL1*RR3+XL0)
        prb=1d0
        rrr10=0.1d0 ! branch frozen 
        if (rrr(11).lt.prb) then
         CTHET=-(1+EPS-ETA)
         etaa=eta
         etab=1+eps-CTHET

        else
         CTHET=(1+EPS-ETA)
         etaa=1+eps+CTHET
         etab=eta

        endif
        THET =ACOS(CTHET)
        PHSPAC=PHSPAC/(prb/(XL1/2*ETAa)+(1d0-prb)/(XL1/2*ETAb))

      PHI = 2*PI*RR4
      PHSPAC=PHSPAC*(4*PI)
      CALL ROTPOd(THET,PHI,PIPL)
      CALL ROTPOd(THET,PHI,PIM1)
      CALL ROTPOd(THET,PHI,PIM2)
      CALL ROTPOd(THET,PHI,PR)
!      write(*,*) 'thet gen=',thet,eps,xl1,eta,cthet
C
* NOW TO THE  REST FRAME, DEFINE three AND PN MOMENTA
* three  MOMENTUM
      PAA(1)=0
      PAA(2)=0
      PAA(4)=1.D0/(2*AMTAU)*(AMTAU**2-AMNUTA**2+AM3**2)
      PAA(3)= SQRT(ABS(PAA(4)**2-AM3**2))
      PPI   =          PAA(4)**2-AM3**2
      PHSPAC=PHSPAC*(4*PI)*(2*PAA(3)/AMTAU)
* pn MOMENTUM
      PN(1)=0
      PN(2)=0
      PN(4)=1.D0/(2*AMTAU)*(AMTAU**2+AMNUTA**2-AM3**2)
      PN(3)=-PAA(3)
* Z-AXIS ANTIPARALLEL TO pn MOMENTUM
      EXE=(PAA(4)+PAA(3))/AM3
      CALL BOSTd3(EXE,PIPL,PIPL)
      CALL BOSTd3(EXE,PIM1,PIM1)
      CALL BOSTd3(EXE,PIM2,PIM2)
      CALL BOSTd3(EXE,PR,PR)
      call rotatv(-1,paa,PIPL,PIPL)
      call rotatv(-1,paa,PIM1,PIM1)
      call rotatv(-1,paa,PIM2,PIM2)
      call rotatv(-1,paa,PN,PN)
      call rotatv(-1,paa,pr,pr)
      call rotatv(-1,paa,PAA,PAA)
        EPS=(AM2/AMtau)**2
        EPS=(AMnuta/pn(4))**2
        EPS=(AMnuta/AMtau)**2*max((am3/amtau)**2,1d-4)
        EPS=(am3/amtau)**2
        XL1=LOG((2+EPS)/EPS)
        XL0=LOG(EPS)
      if(ift.eq.0) then
       THET =ACOS(-1.D0+2*RRR(5))
       CTHET=COS(THET)
       PHI = 2*PI*RRR(6)
      else
       IF    (RRR(9).lt.1.d0/3d0) then 
        THET =ACOS(-1.D0+2*RRR(5))
        CTHET=COS(THET)
        PHI = 2*PI*RRR(6)
       elseIF(RRR(9).lt.(1d0+2*ple)/3d0) then
         ETA  =EXP(XL1*RRR(5)+XL0)
         CTHET=-(1+EPS-ETA)
         THET =ACOS(CTHET)
         PHI = 2*PI*RRR(6)
       else
         ETA  =EXP(XL1*RRR(5)+XL0)
         CTHET=(1+EPS-ETA)
         THET =ACOS(CTHET)
         PHI = 2*PI*RRR(6)
       endif
        XL1=LOG((2+EPS)/EPS)
        XL0=LOG(EPS)
        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
        pleft=ple
        prob1=.2
        prob2=Pleft*.4d0
        prob3=(1D0-PLEFT)*.4D0
        prob4=Pleft*.4d0
        prob5=(1D0-PLEFT)*.4D0
      IF    (RRR(9).lt.PROB1) then 
       THET =ACOS(-1.D0+2*RRR(5))
       CTHET=COS(THET)
       PHI = 2*PI*RRR(6)
      elseIF(RRR(9).lt.(PROB1+PROB2)) then
        ETA  =EXP(XL1*RRR(5)+XL0)
        CTHET=-(1+EPS-ETA)
         xx=eps
         beta=sqrt(1d0-eps)
         xlog=-log((1+beta)**2/xx)
         xlog1=-log(16D0/xx)
          u=(log((1D0+beta)/4D0))**2 +xlog*xlog1*rrr(5)
         cthet=-1D0/beta*(4D0*EXP(-SQRT(u))-1)
         CTHET=-cthet
        THET =ACOS(CTHET)
        PHI = 2*PI*RRR(6)
      elseIF    (RRR(9).lt.(PROB1+PROB2+PROB3)) then
        ETA  =EXP(XL1*RRR(5)+XL0)
        CTHET=(1+EPS-ETA)
         xx=eps
         beta=sqrt(1d0-eps)
         xlog=-log((1+beta)**2/xx)
         xlog1=-log(16D0/xx)
          u=(log((1D0+beta)/4D0))**2 +xlog*xlog1*rrr(5)
          cthet=-1D0/beta*(4D0*EXP(-SQRT(u))-1)

        THET =ACOS(CTHET)
        PHI = 2*PI*RRR(6)
      elseIF    (RRR(9).lt.(PROB1+PROB2+PROB3+PROB4)) then
        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
          n=1
          if(n.eq.1) then
         AM2SQX=AMS1/(1D0-RRr(5)*(1-(ams1/ams2)**n))
          elseif(n.eq.2) then
         AM2SQX=AMS1/sqrt(1D0-RRr(5)*(1-(ams1/ams2)**n))
          else
         AM2SQX=AMS1*(1D0-RRr(5)*(1-(ams1/ams2)**n))**(-1d0/n)
          endif
        CTHET=AM2SQX-2D0+sqrt(1d0-eps)
        THET =ACOS(CTHET)
        PHI = 2*PI*RRR(6)
      else
        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
          n=1
          if(n.eq.1) then
         AM2SQX=AMS1/(1D0-RRr(5)*(1-(ams1/ams2)**n))
          elseif(n.eq.2) then
         AM2SQX=AMS1/sqrt(1D0-RRr(5)*(1-(ams1/ams2)**n))
          else
         AM2SQX=AMS1*(1D0-RRr(5)*(1-(ams1/ams2)**n))**(-1d0/n)
          endif
        CTHET=-AM2SQX+2D0-sqrt(1d0-eps)
        THET =ACOS(CTHET)
        PHI = 2*PI*RRR(6)
      endif
      if (cthet**2.gt.1d0) then
       write(*,*) 'station cthet rrr(9); arbit action'
       write(*,*) cthet,rrr(5),rrr(9)
       write(*,*) ams1,ams2,am2sq
       cthet=cthet/cthet**2
        THET =ACOS(CTHET)       
      endif

      endif
      eta1=1+eps+cthet
      eta2=1+eps-cthet
      if (ift.eq.0) then
      ff=1d0
      else
      FF=1d0/3d0/1d0+2*ple/3d0/(XL1/2*ETA1)+2*(1d0-ple)/3d0/(XL1/2*ETA2)
      xx=eps
      beta=sqrt(1d0-eps)
      xx=eps
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
      ct=-cthet

      xccos1=beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))
      ct=cthet
      xccos2=beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))

        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
        n=1
        AM2SQX= CTHET+2D0-sqrt(1d0-eps)
        xj4=am2SQX**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)/2
        n=1
        AM2SQX=-CTHET+2D0-sqrt(1d0-eps)
        xj5=am2SQX**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)/2
       
      FF=PROB1/1d0+PROB2*xccos1+PROB3*xccos2+PROB4/XJ4+PROB5/XJ5

      endif
      PHSPAC=PHSPAC/FF
!      write(*,*) 'prod ff=',ff,xl1,eta1,eta2,eps,cthet
!      write(*,*) 'prod thet=',thet,cthet
      CALL ROTPOd(THET,PHI,PIPL)
      CALL ROTPOd(THET,PHI,PIM1)
      CALL ROTPOd(THET,PHI,PIM2)
      CALL ROTPOd(THET,PHI,PR)
      CALL ROTPOd(THET,PHI,PN)
      CALL ROTPOd(THET,PHI,PAA)
      DGAMT=PHSPAC
      if (ifprint.eq.1) write(*,*) 'kinbre dgamt=',dgamt
      END

      SUBROUTINE RKINT(idoub,amro,gamrox,ple,
     $ SVAR,DGAMT,PN,amnuta,PIM2,AMP1,PIM1,AMP2,PIPL,AMP3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION  PT(4),PN(4),PAA(4),PIM1(4),PIM2(4),PIPL(4)
      DIMENSION  PR(4),PBST(4)
      common /articut/ arbitr,arbitr1,themin,arbitr2
      common /nevik/ nevtru,ifprint
      save /nevik/,/articut/
      SAVE
      DATA PI /3.141592653589793238462643D0/
      XLAM(X,Y,Z)=SQRT(ABS((X-Y-Z)**2-4.0D0*Y*Z))

C
C FOUR BODY PHASE SPACE NORMALISED AS IN BJORKEN-DRELL
C D**3 P /2E/(2PI)**3 (2PI)**4 DELTA4(SUM P)
      PHSPAC=1.D0/2**17/PI**8
      amtau=sqrt(svar)
      ift=0
      if(amnuta.eq.amp2) ift=1
      if(   amnuta.eq.amp1 .and.amp2.eq.amp3
     $   .and.amp1.lt.0.001.and.amp3.ge.amp1) ift=1
      gamro=abs(gamrox)
        if (ifprint.eq.1) write(*,*) 'rkint now ..'
C
      DO K=1,4
        PAA(K)=PIM1(K)+PIM2(K)+PIPL(K)
        PR(K) =PIM1(K)+PIPL(K)
        pt(k) =PIM1(K)-PIM2(K)+PIPL(K)
      ENDDO
C
C.. MASS OF two
      if (gamrox.lt.0d0) then 
        prob1=.2
        prob2=.2!.3
        PROB3=.2
        PROB4=.2!.3
        prob5=.20d0 !this branch was tested negatively must be checked before use
      else
        prob1=.4
        prob2=.0
        PROB3=.4
        PROB4=.0
        prob5=.20d0
      endif

        AMS1=(AMP2+AMP3)**2
        AMS2=(AMTAU-AMNUTA-amp1)**2
        AM2SQ=dmas2(PR)
        AM2 =SQRT(AM2SQ)
        XJ1=(AMS2-AMS1)
C PHASE SPACE WITH SAMPLING FOR RHO RESONANCE
         B=LOG(AMS1)
         A=LOG(AMS2)
        xj2=AM2SQ*(A-B)

      ALP1=ATAN((AMS1-AMRO**2)/AMRO/GAMRO)
      ALP2=ATAN((AMS2-AMRO**2)/AMRO/GAMRO)
      xj3=((AM2SQ-AMRO**2)**2+(AMRO*GAMRO)**2)/(AMRO*GAMRO)
      xj3=xj3*(ALP2-ALP1)
        n=1
        xj4=am2SQ**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)
        arbix=max(1d0,arbitr)
        am2sqx=am2sq+arbix        
        ams1x=ams1+arbix
        ams2x=ams2+arbix
!        n=1
!        xj5=am2SQx**(n+1)*n*(1D0/ams1x**n-1D0/ams2x**n)
         B=LOG(AMS1x)
         A=LOG(AMS2x)
        xj5=AM2SQx*(A-B)
        amrou=sqrt(max(200d0,arbitr))
        gamrou=amrou/4
        amrou=1.5*amrou
 !       amrou=amro
!        gamrou=gamro
        ALP1u=ATAN((AMS1-AMROu**2)/AMROu/GAMROu)
        ALP2u=ATAN((AMS2-AMROu**2)/AMROu/GAMROu)
        xj5=((AM2SQ-AMROu**2)**2+(AMROu*GAMROu)**2)/(AMROu*GAMROu)
        xj5=xj5*(ALP2u-ALP1u)


       PHSPAC=PHSPAC/(PROB1/XJ1+PROB2/XJ2+PROB3/XJ3+PROB4/XJ4+PROB5/XJ5)


C.. mass of three
        AMS1=(AMP1+am2)**2
        AMS2=(AMTAU-AMNUTA)**2
        AM3SQ =dmas2(PAA)
        AM3 =SQRT(AM3SQ)


      if (idoub.ne.1) then
         PROB1=.8d0
         PROB2=0.0d0
         prob3=0.2d0
      elseif (gamrox.lt.0d0) then 
         PROB1=0.4d0
         PROB2=0.4d0
         prob3=0.2d0
      else
         PROB1=.8d0
         PROB2=0d0
         prob3=0.2D0
      endif

         B=LOG(AMS1)
         A=LOG(AMS2)
        XJ1=AM3SQ*(A-B)
        n=1
        xj2=am3SQ**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)
        xj3=ams2-ams1
        PHSPAC=PHSPAC/(PROB1/XJ1+PROB2/XJ2+PROB3/xj3)
!        else
!         B=LOG(AMS1)
!         A=LOG(AMS2)
!         PHSPAC=PHSPAC*AM3SQ*(A-B)
!        endif

        ENQ1=(AM2SQ-AMP2**2+AMP3**2)/(2*AM2)
        PPPI=SQRT(ABS(ENQ1**2-AMP3**2))
        PHSPAC=PHSPAC*(4*PI)*(2*PPPI/AM2)
C
        PR4=1.D0/(2*AM3)*(AM3**2+AM2**2-AMP1**2)
        PR3= SQRT(ABS(PR4**2-AM2**2))
        PHSPAC=PHSPAC*(2*PR3/AM3)
        if (ifprint.eq.1) write(*,*) 'am2-3',am2,am3
        if (ift.eq.1) then
!...     versor of one in two restframe
         PNPAA=dot(pim2,pr)
         prx3=PPPI
         prx4=(AM2SQ-AMP2**2+AMP3**2)/(2*AM2)
         pim14=(AM2SQ+AMP2**2-AMP3**2)/(2*AM2)
         DO K=1,4
          pt(k) =( (PIM1(K)-PIPL(K))
     $            -Pr(K)*(prx4-pim14)/am2   )/2/PRx3
          PBST(K)=Pim2(K)-Pr(K)*PNPAA/am2**2
         ENDDO
         cost=-dot(pt,pbst)/sqrt(-dmas2(pbst))
cc ms 03.07.97         if(sqrt(-dmas2(pt)).gt.1d0) cost=cost/sqrt(-dmas2(pt))
         if(sqrt(dabs(-dmas2(pt))).gt.1d0) 
     $      cost=cost/sqrt(dabs(-dmas2(pt)))
! m.s. 8/24/98 beg
         IF(cost.LT.-1d0) cost=-1d0
         IF(cost.GT. 1d0) cost= 1d0
! m.s. 8/24/98 end
         thet=acos(cost)
         costhe=cost
         cthet=cost
         EPS=(amp3*2*AM2/(AM2**2))**2
         XL1=LOG((2+EPS)/EPS)
         XL0=LOG(EPS)
         eta1=1+eps+cthet
         eta2=1+eps-cthet
         prev=0.3
!         write(*,*) thet, 1/(PREV/(XL1/2*ETA)+(1d0-prev)/1D0)
        PHSPAC=PHSPAC/(PREV/( XL1/2*ETA1)+PREV/(XL1/2*ETA2)
     $                       +(1d0-2*prev)/1D0)
       if (ifprint.eq.1 ) then
       write(*,*) 'R ff2=',1/(XL1/2*ETA1),1/(XL1/2*ETA2),prev
       write(*,*) 'R ',cthet,eps,xl1
       endif
      endif
      

      PHSPAC=PHSPAC*(4*PI)
!        write(*,*) 'thet inv=',thet,eps,xl1,eta,cost


      cthx=pn(3)/sqrt(pn(1)**2+ pn(2)**2+ pn(3)**2)
      thet=acos(cthx)

        EPS=(AMnuta/AMtau)**2*max((am3/amtau)**2,1d-4)
        XL1=LOG((2+EPS)/EPS)
        XL0=LOG(EPS)
      eta1=1+eps+cthx
      eta2=1+eps-cthx
      pleft=ple
!      if(gamrox.lt.0.) pleft=0.5
      if(gamro/amro.gt.0.5) pleft=0.5
      if (gamrox.lt.0d0) then 
        prob1=.2
        prob2=Pleft*.4d0
        prob3=(1D0-PLEFT)*.4D0
        prob4=Pleft*.4d0
        prob5=(1D0-PLEFT)*.4D0
      else
        prob1=.2
        prob2=Pleft*.8d0
        prob3=(1D0-PLEFT)*.8D0
        prob4=0
        prob5=0
      endif
      FF=0d0/3d0/1d0+pleft/(XL1/2*ETA1)+(1d0-pleft)/(XL1/2*ETA2)
      beta=sqrt(1d0-eps)
      xx=eps
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
      ct=-cthx

      xccos1=beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))
      ct=cthx
      xccos2=beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))

        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
        n=1
        AM2SQX= CTHX+2D0-sqrt(1d0-eps)
        xj4=am2SQX**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)/2
        n=1
        AM2SQX=-CTHX+2D0-sqrt(1d0-eps)
        xj5=am2SQX**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)/2

 !     FF=0d0/3d0/1d0+pleft*xccos1+(1d0-pleft)*xccos2
      FF=PROB1/1d0+PROB2*xccos1+PROB3*xccos2+PROB4/XJ4+PROB5/XJ5
       if (ifprint.eq.1 ) then
      write(*,*) 'inv  ff4=',prob1,prob2,prob3,prob4,prob5
      write(*,*) '         ',xccos1,xccos2,xj4,xj5
      write(*,*) '         ',cthx,ams1,ams2
        endif
      PHSPAC=PHSPAC/FF

        PAA4=1.D0/(2*AMTAU)*(AMTAU**2-AMNUTA**2+AM3**2)
        PAA3= SQRT(ABS(PAA(4)**2-AM3**2))
        PHSPAC=PHSPAC*(4*PI)*(2*PAA3/AMTAU)
        pim24=1.D0/(2*AM3)*(AM3**2-AM2**2+AMP1**2)
!... versor of two in three restframe
      PNPAA=dot(pn,paa)
      DO K=1,4
         pt(k) =( (PIM1(K)-PIM2(K)+PIPL(K))
     $           -PAA(K)*(pr4-pim24)/am3   )/2/PR3
         PBST(K)=PN(K)-PAA(K)*PNPAA/am3**2
      ENDDO
        cost=dot(pt,pbst)/sqrt(-dmas2(pbst))
cc ms 03.07.97        if(sqrt(-dmas2(pt)).gt.1d0) cost=cost/sqrt(-dmas2(pt))
        if(sqrt(dabs(-dmas2(pt))).gt.1d0) 
     $     cost=cost/sqrt(dabs(-dmas2(pt)))
        if(abs(cost).gt.1d0) then
        write(*,*) '>>>>>>>>>>>>>>>>>>>>>>>'
        write(*,*) 'cos=',cost
cc ms 03.07.97  write(*,*) dot(pt,pbst),sqrt(-dmas2(pbst)),sqrt(-dmas2(pt))
        write(*,*) dot(pt,pbst),sqrt(dabs(-dmas2(pbst)))
     $      ,sqrt(dabs(-dmas2(pt)))
        write(*,*) '<<<<<<<<<<<<<<<<<<<<<<<'
        cost=cost/cost**2
        endif
        if(idoub.eq.5) cost=-cost
        thet=acos(cost)
        costhe=cost
C-- angles for three:
      t=(amtau**2-am3**2)*(1d0-abs(cthx))

!        EPS=(amp1*2*AM3/(AM3**2-AM2**2+AMP1**2))**2
         EPS=((t+amp1)*2*AM3/(AM3**2-AM2**2+AMP1**2))**2
!         EPS=(amp1*2*AM3/(AM3**2+AMP1**2))**2
         if (eps.ge.1d0) eps=.999d0
        XL1=LOG((2+EPS)/EPS)
        XL0=LOG(EPS)
        eta1=1+eps-cost
        eta2=1+eps-cost
         if(idoub.eq.1) eta2=1+eps+cost
        prev=0.4d0
        if(gamro/amro.gt.0.5) prev=0.6d0
        if(gamrox.lt.0.) prev=0.6d0
        if (amp1.lt.0.3d-3) prev=0
!        if (amp1.lt.0.6d-3) prev=0
        uu=1d0/(PREV/2/(XL1/2*ETA1)+PREV/2/(XL1/2*ETA2)+(1d0-PREV)/1D0)
        PHSPAC=PHSPAC*uu
       if (ifprint.eq.1 ) then
       write(*,*) 'r  ff3  '
       write(*,*) 'r  ff3=',1/(XL1/2*ETA1),prev,cost,idoub
       endif

!      xx=eps
!      beta=sqrt(1d0-eps)
!      xlog=-log((1+beta)**2/xx)
!      xlog1=-log(16D0/xx)
!      ct=cthet
!      if(idoub.eq.1) ct=-cthet

!      xccos=beta/(xlog*xlog1
!     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
!     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))
!         PHSPAC=PHSPAC/(PREV*xccos+(1d0-PREV)/1D0)
!        PHSPAC=PHSPAC/(PREV/(XL1/2*ETA)+(1d0-prev)/1D0)
       if (ifprint.eq.1 ) then
!       write(*,*) 'R ff3=',1/(XL1/2*ETA),prev,cost,idoub
       endif

      DGAMT=PHSPAC
      if (phspac.lt.0d0) then
       write(*,*) 'phspac=',phspac,'ff=',ff
      write(*,*) 'inv  ff=',ff,xl1,eta1,eta2,eps,cthx
      write(*,*) '     thet=',thet,cthx
      write(*,*) 'first angular jacobian=',uu,cost
      write(*,*) 'am2 am3',am2,am3
      endif
      END

      SUBROUTINE KINT(idoub,amro,gamrox,ple,
     $ SVAR,DGAMT,PN,amnuta,PIM2,AMP1,PIM1,AMP2,PIPL,AMP3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION  PN(4),PAA(4),PIM1(4),PIM2(4),PIPL(4)
      DIMENSION  PR(4)
      REAL*4 RRR(14)
      common /articut/ arbitr,arbitr1,themin,arbitr2
      common /nevik/ nevtru,ifprint
      save /nevik/,/articut/
      real*4 rrx
      common /erery/ rrx(14)
      save /erery/
      SAVE
      DATA PI /3.141592653589793238462643D0/
      XLAM(X,Y,Z)=SQRT(ABS((X-Y-Z)**2-4.0D0*Y*Z))
      if (ifprint.eq.1) write(*,*) 'kint now ...'
C
C FOUR BODY PHASE SPACE NORMALISED AS IN BJORKEN-DRELL
C D**3 P /2E/(2PI)**3 (2PI)**4 DELTA4(SUM P)
      PHSPAC=1.D0/2**17/PI**8
      amtau=sqrt(svar)
      ift=0
      if(amnuta.eq.amp2) ift=1
      if(   amnuta.eq.amp1 .and.amp2.eq.amp3
     $   .and.amp1.lt.0.001.and.amp3.ge.amp1) ift=1
      gamro=abs(gamrox)
C
!      sum=0
!      sum2=0
!      nn=1000000
!      do k=1,nn
       CALL VARRAN(RRR,14)
CBB      CALL RANMAR(RRR,14)
        if (ift.gt.100) then
        RRR( 1 )= 0.9478749037   
        RRR( 2 )= 0.6414651871E-03
        RRR( 3 )= 0.2186722755
        RRR( 4 )= 0.6770371199
        RRR( 5 )= 0.9959959984
        RRR( 6 )= 0.8483608961
        RRR( 7 )= 0.2983032465
        RRR( 8 )= 0.4594436884
        RRR( 9 )= 0.3546591997
        RRR( 10 )= 0.7370334864
        RRR( 11 )= 0.5411399007
        RRR( 12 )= 0.5843002200
        RRR( 13 )= 0.5636240840
        RRR( 14 )= 0.1165237427

        RRR( 1 )= 0.9120953083 +.07 
        RRR( 2 )= 0.6044536829E-01  -.00314
        RRR( 3 )= 0.6973654628
        RRR( 4 )= 0.6249293685
        RRR( 5 )= 0.2043003440
        RRR( 6 )= 0.5548739433
        RRR( 7 )= 0.9244202971
        RRR( 8 )= 0.1170529723
        RRR( 9 )= 0.4773632288
        RRR( 10 )= 0.3393656611
        RRR( 11 )= 0.2608218789
        RRR( 12 )= 0.7097090483
        RRR( 13 )= 0.1676200032
        RRR( 14 )= 0.4811552763


        RRR( 1 )= 0.6470682025
        RRR( 2 )= 0.2057774663
        RRR( 3 )= 0.5201567411
        RRR( 4 )= 0.2157996893
        RRR( 5 )= 0.2812561989
        RRR( 6 )= 0.5039139986
        RRR( 7 )= 0.8193333149
        RRR( 8 )= 0.5267652273
        RRR( 9 )= 0.3422190547
        RRR( 10 )= 0.2896287441
        RRR( 11 )= 0.3918313980E-01
        RRR( 12 )= 0.6622866988
        RRR( 13 )= 0.8352186084
        RRR( 14 )= 0.7802855968E-01

        RRR( 1 )= 0.4865604639E-01
        RRR( 2 )= 0.7470643520E-01
        RRR( 3 )= 0.5707561970E-02
        RRR( 4 )= 0.1091821194
        RRR( 5 )= 0.5938494205
        RRR( 6 )= 0.3438313007
        RRR( 7 )= 0.1558269262
        RRR( 8 )= 0.5537773371
        RRR( 9 )= 0.8776282072
        RRR( 10 )= 0.4459511638 /2
        RRR( 11 )= 0.1935519576
        RRR( 12 )= 0.5576208830
        RRR( 13 )= 0.9880648255
        RRR( 14 )= 0.9081103206

!        RRR( 1 )= 0.2664464116
!        RRR( 2 )= 0.4497187138
!        RRR( 3 )= 0.8510571718E-01
!        RRR( 4 )= 0.7872920632
!        RRR( 5 )= 0.5956429243
!        RRR( 6 )= 0.5650028586
!        RRR( 7 )= 0.7602188587
!        RRR( 8 )= 0.9214590788
!        RRR( 9 )= 0.8335509300
!        RRR( 10 )= 0.9940457344
!        RRR( 11 )= 0.2682380676
!        RRR( 12 )= 0.8510574102
!        RRR( 13 )= 0.1608982682
!        RRR( 14 )= 0.4655991197

        iflak=3
        write(*,*) 'walek',ift
        endif
      do k=1,14
      rrx(k)=rrr(k)
      enddo

      if (ifprint.eq.1) then
         write(*,*) '=====>'
         do k=1,14
            write(*,*) '       RRR(',k,')=',RRR(k)
         enddo
      endif
c --- mass of two
      if (ifprint.eq.100) then
        rrr(2)= .9
c --- mass of three
        rrr(1)=.1
C
C-- angles for three:
        rrr(3)=.2
C-- angles for four:
        rrr(5)=.975 
C-- angles for two:
        rrr(7)=.8 
      endif
C.. MASS OF two
      if (gamrox.lt.0d0) then 
        prob1=.2
        prob2=.2!.3
        PROB3=.2
        PROB4=.2!.3
        prob5=0.2d0
      else
        prob1=.4
        prob2=.0
        PROB3=.4
        PROB4=.0
        prob5=0.2d0
      endif
        if (iflak.eq.5) write(*,*) prob1,prob2,prob3,prob4,prob5

        RR2=RRR(2)
        AMS1=(AMP2+AMP3)**2      
        AMS2=(AMTAU-AMNUTA-amp1)**2
        ALP1=ATAN((AMS1-AMRO**2)/AMRO/GAMRO)
        ALP2=ATAN((AMS2-AMRO**2)/AMRO/GAMRO)
        amrou=sqrt(max(200d0,arbitr))
        gamrou=amrou/4
        amrou=1.5*amrou
!        amrou=amro
!        gamrou=gamro
        ALP1u=ATAN((AMS1-AMROu**2)/AMROu/GAMROu)
        ALP2u=ATAN((AMS2-AMROu**2)/AMROu/GAMROu)
        IF (RRR(10).LT.PROB1) THEN
         AM2SQ=AMS1+   RR2*(AMS2-AMS1)
         AM2 =SQRT(AM2SQ)
        elseIF (RRR(10).LT.(PROB1+PROB2)) THEN  
         B=LOG(AMS1)
         A=LOG(AMS2)
         AM2SQ=AMS2*EXP((B-A)*RR2)
         AM2 =SQRT(AM2SQ)     
        ELSEIF (RRR(10).LT.(PROB1+PROB2+PROB3)) THEN

         ALP=ALP1+RR2*(ALP2-ALP1)
         AM2SQ=AMRO**2+AMRO*GAMRO*TAN(ALP)
         AM2 =SQRT(AM2SQ)
        ELSEIF (RRR(10).LT.(PROB1+PROB2+PROB3+prob4)) THEN
         n=1
          if(n.eq.1) then
         AM2SQ=AMS1/(1D0-RR2*(1-(ams1/ams2)**n))
          elseif(n.eq.2) then
         AM2SQ=AMS1/sqrt(1D0-RR2*(1-(ams1/ams2)**n))
          else
         AM2SQ=AMS1*(1D0-RR2*(1-(ams1/ams2)**n))**(-1d0/n)
          endif
         AM2 =SQRT(AM2SQ)
         if (am2sq.gt.ams2) WRITE(*,*) 'am2sqx,err',am2sq,ams1,ams2,rr2
         if (am2sq.gt.ams2) stop
         if (am2sq.lt.ams1) WRITE(*,*) 'am2sqx,err',am2sq,ams1,ams2,rr2
         if (am2sq.lt.ams1) stop

        ELSE
         arbix=max(1d0,arbitr)
         ams1x=ams1+arbix
         ams2x=ams2+arbix
         n=1
          if(n.eq.1) then
         AM2SQx=AMS1x/(1D0-RR2*(1-(ams1x/ams2x)**n))
          elseif(n.eq.2) then
         AM2SQx=AMS1x/sqrt(1D0-RR2*(1-(ams1x/ams2x)**n))
          else
         AM2SQx=AMS1x*(1D0-RR2*(1-(ams1x/ams2x)**n))**(-1d0/n)
          endif
         B=LOG(AMS1x)
         A=LOG(AMS2x)
         AM2SQx=AMS2x*EXP((B-A)*RR2)
         am2sq=am2sqx-arbix
         AM2 =SQRT(AM2SQ)
         if (am2sq.gt.ams2) WRITE(*,*) 'am2sqx,err',am2sq,ams1,ams2,rr2
         if (am2sq.gt.ams2) stop
         if (am2sq.lt.ams1) WRITE(*,*) 'am2sqx,err',am2sq,ams1,ams2,rr2
         if (am2sq.lt.ams1) stop
         ALPu=ALP1u+RR2*(ALP2u-ALP1u)
         AM2SQ=AMROu**2+AMROu*GAMROu*TAN(ALPu)
         AM2 =SQRT(AM2SQ)

        ENDIF
        XJ1=(AMS2-AMS1)
         B=LOG(AMS1)
         A=LOG(AMS2)
        xj2=AM2SQ*(A-B)
        xj3=((AM2SQ-AMRO**2)**2+(AMRO*GAMRO)**2)/(AMRO*GAMRO)
        xj3=xj3*(ALP2-ALP1)
        n=1
        xj4=am2SQ**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)
        arbix=max(1d0,arbitr)
        am2sqx=am2sq+arbix        
        ams1x=ams1+arbix
        ams2x=ams2+arbix
!        n=1
!        xj5=am2SQx**(n+1)*n*(1D0/ams1x**n-1D0/ams2x**n)
         B=LOG(AMS1x)
         A=LOG(AMS2x)
        xj5=AM2SQx*(A-B)

        xj5=((AM2SQ-AMROu**2)**2+(AMROu*GAMROu)**2)/(AMROu*GAMROu)
        xj5=xj5*(ALP2u-ALP1u)

      if (ifprint.eq.1) write(*,*) 'a=',ams1,ams2,am2sq
      if (ifprint.eq.1) write(*,*) 'b=',ams1x,ams2x,am2sqx   
      if (ifprint.eq.1) write(*,*) 'amro-gamro',amro,gamro
!        sum=Sum+1d0/(PROB1/XJ1+PROB2/XJ2+PROB3/XJ3+PROB4/XJ4)
!        sum2=Sum2+1d0/(PROB1/XJ1+PROB2/XJ2+PROB3/XJ3+PROB4/XJ4)**2
!        enddo
!        sum=sum/nn
!        sum2=sum2/nn
!        err=sqrt((sum2-sum**2)/nn)
!        write(*,*) sum,'+-',err
!        write(*,*) '37980.9549446302772 +- 59.65'
!        stop
       PHSPAC=PHSPAC/(PROB1/XJ1+PROB2/XJ2+PROB3/XJ3+PROB4/XJ4+PROB5/XJ5)
        if (ifprint.eq.1) write(*,*) 'am2 ',am2,rr2
        if (ifprint.eq.1) write(*,*) 'xj-ts',xj1,xj2,xj3,xj4,xj5
        if (ifprint.eq.1) 
     $     write(*,*) 'probs',prob1,prob2,prob3,prob4,prob5

C.. mass of three
        RR1=RRR(1)
        AMS1=(AMP1+am2)**2
        AMS2=(AMTAU-AMNUTA)**2
         B=LOG(AMS1)
         A=LOG(AMS2)
      if (idoub.ne.1) then
         PROB1=.8d0
         PROB2=0.0d0
         prob3=0.2d0
      elseif (gamrox.lt.0d0) then 
         PROB1=0.4d0
         PROB2=0.4d0
         prob3=0.2d0
      else
         PROB1=.8d0
         PROB2=0d0
         prob3=0.2D0
      endif

         IF (RRR(12).lt.prob1) then
         AM3SQ=AMS2*EXP((B-A)*RR1)
         elseIF (RRR(12).lt.prob1+prob2) then
         n=1
          if(n.eq.1) then
         AM3SQ=AMS1/(1D0-RR1*(1-(ams1/ams2)**n))
          elseif(n.eq.2) then
         AM3SQ=AMS1/sqrt(1D0-RR1*(1-(ams1/ams2)**n))
          else
         AM3SQ=AMS1*(1D0-RR1*(1-(ams1/ams2)**n))**(-1d0/n)
          endif

         else
            AM3SQ=AMS1+(ams2-ams1)*rr1
         endif
         AM3 =SQRT(AM3SQ)
!         PHSPAC=PHSPAC*AM3SQ*(A-B)
        XJ1=AM3SQ*(A-B)
        n=1
        xj2=am3SQ**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)
        xj3=ams2-ams1
        PHSPAC=PHSPAC/(PROB1/XJ1+PROB2/XJ2+PROB3/xj3)

      if (ifprint.eq.1) write(*,*) 'am3 ',am3,AM3SQ*(A-B),rr1
!        else
!         B=LOG(AMS1)
!         A=LOG(AMS2)
!         AM3SQ=AMS2*EXP((B-A)*RR1)
!         AM3 =SQRT(AM3SQ)
!         PHSPAC=PHSPAC*AM3SQ*(A-B)
!        endif

C-- angles for four:
!      sum=0
!      sum2=0
!      nn=1000000
!      do k=1,nn
!        call varran(rrr,14)
!CBB      CALL RANMAR(RRR,14)

        EPS=(AMnuta/AMtau)**2*max((am3/amtau)**2,1d-4)
        XL1=LOG((2+EPS)/EPS)
        XL0=LOG(EPS)
        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
        pleft=ple
!        if(gamrox.lt.0.) pleft=0.5
      if(gamro/amro.gt.0.5) pleft=0.5
      if (gamrox.lt.0d0) then 
        prob1=.2
        prob2=Pleft*.4d0
        prob3=(1D0-PLEFT)*.4D0
        prob4=Pleft*.4d0
        prob5=(1D0-PLEFT)*.4D0
      else
        prob1=.2
        prob2=Pleft*.8d0
        prob3=(1D0-PLEFT)*.8D0
        prob4=0
        prob5=0
      endif
      IF    (RRR(9).lt.PROB1) then 
       THET =ACOS(-1.D0+2*RRR(5))
       CTHET=COS(THET)
       PHI = 2*PI*RRR(6)
      elseIF(RRR(9).lt.(PROB1+PROB2)) then
        ETA  =EXP(XL1*RRR(5)+XL0)
        CTHET=-(1+EPS-ETA)
         xx=eps
         beta=sqrt(1d0-eps)
         xlog=-log((1+beta)**2/xx)
         xlog1=-log(16D0/xx)
          u=(log((1D0+beta)/4D0))**2 +xlog*xlog1*rrr(5)
         cthet=-1D0/beta*(4D0*EXP(-SQRT(u))-1)
         CTHET=-cthet
        THET =ACOS(CTHET)
        PHI = 2*PI*RRR(6)
      elseIF    (RRR(9).lt.(PROB1+PROB2+PROB3)) then
        ETA  =EXP(XL1*RRR(5)+XL0)
        CTHET=(1+EPS-ETA)
         xx=eps
         beta=sqrt(1d0-eps)
         xlog=-log((1+beta)**2/xx)
         xlog1=-log(16D0/xx)
          u=(log((1D0+beta)/4D0))**2 +xlog*xlog1*rrr(5)
          cthet=-1D0/beta*(4D0*EXP(-SQRT(u))-1)

*!WP (to avoid floating exceptions)
          IF (ABS(cthet).LT.1d0) THEN
            THET =ACOS(CTHET)
          ELSEIF (cthet.LE.-1d0) THEN
             THET =PI
          ELSE
             THET =0d0
          ENDIF
          PHI = 2*PI*RRR(6)
      elseIF    (RRR(9).lt.(PROB1+PROB2+PROB3+PROB4)) then
        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
          n=1
          if(n.eq.1) then
         AM2SQX=AMS1/(1D0-RRr(5)*(1-(ams1/ams2)**n))
          elseif(n.eq.2) then
         AM2SQX=AMS1/sqrt(1D0-RRr(5)*(1-(ams1/ams2)**n))
          else
         AM2SQX=AMS1*(1D0-RRr(5)*(1-(ams1/ams2)**n))**(-1d0/n)
          endif
        CTHET=AM2SQX-2D0+sqrt(1d0-eps)
        THET =ACOS(CTHET)
        PHI = 2*PI*RRR(6)
      else
        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
          n=1
          if(n.eq.1) then
         AM2SQX=AMS1/(1D0-RRr(5)*(1-(ams1/ams2)**n))
          elseif(n.eq.2) then
         AM2SQX=AMS1/sqrt(1D0-RRr(5)*(1-(ams1/ams2)**n))
          else
         AM2SQX=AMS1*(1D0-RRr(5)*(1-(ams1/ams2)**n))**(-1d0/n)
          endif
        CTHET=-AM2SQX+2D0-sqrt(1d0-eps)
        THET =ACOS(CTHET)
        PHI = 2*PI*RRR(6)
      endif
      if (cthet**2.gt.1d0) then
       cthet=cthet/cthet**2
       write(*,*) 'cthet error -- arbi action'
       write(*,*) cthet,rrr(5),rrr(9)
       write(*,*) ams1,ams2,am2sq
        THET =ACOS(CTHET)
      endif
      eta1=1+eps+cthet
      eta2=1+eps-cthet
      xx=eps
      beta=sqrt(1d0-eps)
      xx=eps
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
      ct=-cthet

      xccos1=beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))
      ct=cthet
      xccos2=beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))

        ams1=1-sqrt(1d0-eps)
        ams2=3-sqrt(1d0-eps)
        n=1
        AM2SQX= CTHET+2D0-sqrt(1d0-eps)
        xj4=am2SQX**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)/2
        n=1
        AM2SQX=-CTHET+2D0-sqrt(1d0-eps)
        xj5=am2SQX**(n+1)*n*(1D0/ams1**n-1D0/ams2**n)/2
       
      FF=PROB1/1d0+PROB2*xccos1+PROB3*xccos2+PROB4/XJ4+PROB5/XJ5
!        sum=Sum+1d0/FF
!        sum2=Sum2+1d0/FF**2
!        enddo
!        sum=sum/nn
!        sum2=sum2/nn
!        err=sqrt((sum2-sum**2)/nn)
!        write(*,*) sum,'+-',err
!        write(*,*) '1.00095748299256204 +- 0.14E-01'
!        stop
       if (ifprint.eq.1 ) then

      write(*,*) '     ff4=',prob1,prob2,prob3,prob4,prob5
      write(*,*) '         ',xccos1,xccos2,xj4,xj5
      write(*,*) '         ',cthet,ams1,ams2
       endif

      PHSPAC=PHSPAC/FF 

C-- angles for three:
      t=(amtau**2-am3**2)*(1d0-abs(cthet))
      rr3=rrr(3)
      rr4=rrr(4)
        prev=0.4d0
        if(gamro/amro.gt.0.5) prev=0.6d0
        if(gamrox.lt.0.) prev=0.6d0
        if (amp1.lt.0.3d-3) prev=0
!        if (amp1.lt.0.6d-3) prev=0
        IF(RRR(11).lt.PREV) then
!         EPS=(amp1*2*AM3/(AM3**2-AM2**2+AMP1**2))**2
         EPS=((t+amp1)*2*AM3/(AM3**2-AM2**2+AMP1**2))**2
!         EPS=(amp1*2*AM3/(AM3**2+AMP1**2))**2
         if (eps.ge.1d0) eps=.999d0
         XL1=LOG((2+EPS)/EPS)
         XL0=LOG(EPS)
         ETA  =EXP(XL1*RR3+XL0)
         CTHET=(1+EPS-ETA)
         xx=eps
         beta=sqrt(1d0-eps)
         xlog=-log((1+beta)**2/xx)
         xlog1=-log(16D0/xx)
!          u=(log((1D0+beta)/4D0))**2 +xlog*xlog1*rr3
!          cthet=-1D0/beta*(4D0*EXP(-SQRT(u))-1)

         if(idoub.eq.1.and.RRR(11).lt.PREV/2) CTHET=-cthet
         THET3 =ACOS(CTHET)
       if (ifprint.eq.1 ) then
         write(*,*) 'ff3 aaa',eps,eta,cthet
         write(*,*) '      t',t,amp1,am3,am2,amtau
       endif
        else
         CTHET=-1+2*rr3
         THET3 =ACOS(CTHET)
!         EPS=(amp1*2*AM3/(AM3**2-AM2**2+AMP1**2))**2
         EPS=((t+amp1)*2*AM3/(AM3**2-AM2**2+AMP1**2))**2
!         EPS=(amp1*2*AM3/(AM3**2+AMP1**2))**2
         if (eps.ge.1d0) eps=.999d0
         XL1=LOG((2+EPS)/EPS)
         XL0=LOG(EPS)
         eta=1+eps-cthet
         if(idoub.eq.1) eta=1+eps+cthet
        endif

      xx=eps
      beta=sqrt(1d0-eps)
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
      ct=cthet
      if(idoub.eq.1) ct=-cthet
!      xccos=beta/(xlog*xlog1
!     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-ct)))
!     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-ct))))!!! +1d0/(1+beta*costhe))      
         XL1=LOG((2+EPS)/EPS)
         XL0=LOG(EPS)
         eta1=1+eps-cthet
         eta2=1+eps-cthet
         if(idoub.eq.1) eta2=1+eps+cthet
        xccos1=1D0/(XL1/2*ETA1)
        xccos2=1D0/(XL1/2*ETA2)
        PHSPAC=PHSPAC/(PREV/2*xccos1+PREV/2*xccos2+(1d0-prev)/1D0)
 
!        PHSPAC=PHSPAC/(PREV/(XL1/2*ETA)+(1d0-prev)/1D0)
        PHI3 = 2*PI*RR4
       if (ifprint.eq.1 ) then
       write(*,*) '  ff3,rr3',rr3
       write(*,*) '  ff3=',1/(XL1/2*ETA1),prev,cthet,idoub
       write(*,*) '  3ff=',eps,xccos1,xccos2
       endif

C-- angles for two: 
       if (ift.eq.1) then
        rr3=rrr(7)
        prev=0.3
        IF(RRR(13).lt.PREV) then
         EPS=(amp3*2*AM2/(AM2**2))**2
         XL1=LOG((2+EPS)/EPS)
         XL0=LOG(EPS)
         ETA  =EXP(XL1*RR3+XL0)
         CTHET=-(1+EPS-ETA)
         THET2 =ACOS(CTHET)
        elseIF(RRR(13).lt.2*PREV) then
         EPS=(amp3*2*AM2/(AM2**2))**2
         XL1=LOG((2+EPS)/EPS)
         XL0=LOG(EPS)
         ETA  =EXP(XL1*RR3+XL0)
         CTHET=(1+EPS-ETA)
         THET2 =ACOS(CTHET)
        else
         CTHET=-1+2*rr3
         THET2 =ACOS(CTHET)
        endif
         EPS=(amp3*2*AM2/(AM2**2))**2
         XL1=LOG((2+EPS)/EPS)
         XL0=LOG(EPS)
         eta1=1+eps+cthet
         eta2=1+eps-cthet
!       write(*,*) thet2, 1/(PREV/(XL1/2*ETA)+(1d0-prev)/1D0)
        PHSPAC=PHSPAC/( PREV/(XL1/2*ETA1)+PREV/(XL1/2*ETA2)
     $                +(1d0-2*prev)/1D0)
        PHI2 = 2*PI*RRR(8)
      if (ifprint.eq.1 ) then
      write(*,*) 'ff2=',1/(XL1/2*ETA1),1/(XL1/2*ETA2),prev
      write(*,*) '  ',cos(thet2),rr3,'<== rrr(7)'
      endif
        else
         THET2 =ACOS(-1.D0+2*RRR(7))
         PHI2 = 2*PI*RRR(8)
        PHSPAC=PHSPAC
        endif
C-- construction:
* two RESTFRAME, DEFINE PIPL AND PIM1
        ENQ1=(AM2SQ-AMP2**2+AMP3**2)/(2*AM2)
        ENQ2=(AM2SQ+AMP2**2-AMP3**2)/(2*AM2)
        PPI=         ENQ1**2-AMP3**2
        PPPI=SQRT(ABS(ENQ1**2-AMP3**2))
        PHSPAC=PHSPAC*(4*PI)*(2*PPPI/AM2)
* PIPL  MOMENTUM IN TWO REST FRAME,RRR(7),RRR(8)
!ms 03.sept.97
c        PIPL(1)=D0
c        PIPL(2)=D0
        PIPL(1)=0D0
        PIPL(2)=0D0
!ms 03.sept.97
        PIPL(3)=PPPI
        PIPL(4)=ENQ1
        CALL ROTPOD(THET2,PHI2,PIPL)
* PIM1 MOMENTUM IN TWO REST FRAME
        DO 30 I=1,3
 30     PIM1(I)=-PIPL(I)
        PIM1(4)=ENQ2
* three REST FRAME, DEFINE momentum of two
*       two  MOMENTUM
        PR(1)=0
        PR(2)=0
        PR(4)=1.D0/(2*AM3)*(AM3**2+AM2**2-AMP1**2)
        PR(3)= SQRT(ABS(PR(4)**2-AM2**2))
        PPI  =          PR(4)**2-AM2**2
*       PIM2 MOMENTUM
        PIM2(1)=0
        PIM2(2)=0
        PIM2(4)=1.D0/(2*AM3)*(AM3**2-AM2**2+AMP1**2)
        PIM2(3)=-PR(3)
        PHSPAC=PHSPAC*(2*PR(3)/AM3)
* PIPL PIM1 BOOSTED FROM two REST FRAME TO three REST FRAME
      EXE=(PR(4)+PR(3))/AM2
      CALL BOSTd3(EXE,PIPL,PIPL)
      CALL BOSTd3(EXE,PIM1,PIM1)

      PHSPAC=PHSPAC*(4*PI)
      CALL ROTPOd(THET3,PHI3,PIPL)
      CALL ROTPOd(THET3,PHI3,PIM1)
      CALL ROTPOd(THET3,PHI3,PIM2)
      CALL ROTPOd(THET3,PHI3,PR)
!      write(*,*) 'thet gen=',thet,eps,xl1,eta,cthet
C
* NOW TO THE  REST FRAME, DEFINE three AND PN MOMENTA
* three  MOMENTUM
      PAA(1)=0
      PAA(2)=0
      PAA(4)=1.D0/(2*AMTAU)*(AMTAU**2-AMNUTA**2+AM3**2)
      PAA(3)= SQRT(ABS(PAA(4)**2-AM3**2))
      PPI   =          PAA(4)**2-AM3**2
      PHSPAC=PHSPAC*(4*PI)*(2*PAA(3)/AMTAU)
* pn MOMENTUM
      PN(1)=0
      PN(2)=0
      PN(4)=1.D0/(2*AMTAU)*(AMTAU**2+AMNUTA**2-AM3**2)
      PN(3)=-PAA(3)
* Z-AXIS ANTIPARALLEL TO pn MOMENTUM
      EXE=(PAA(4)+PAA(3))/AM3
      CALL BOSTd3(EXE,PIPL,PIPL)
      CALL BOSTd3(EXE,PIM1,PIM1)
      CALL BOSTd3(EXE,PIM2,PIM2)
      CALL BOSTd3(EXE,PR,PR)
      call rotatv(-1,pn,PIPL,PIPL)
      call rotatv(-1,pn,PIM1,PIM1)
      call rotatv(-1,pn,PIM2,PIM2)
      call rotatv(-1,pn,PAA,PAA)
      call rotatv(-1,pn,pr,pr)
      call rotatv(-1,pn,PN,PN)
!      write(*,*) 'prod ff=',ff,xl1,eta1,eta2,eps,cthet
!      write(*,*) 'prod thet=',thet,cthet
      CALL ROTPOd(THET,PHI,PIPL)
      CALL ROTPOd(THET,PHI,PIM1)
      CALL ROTPOd(THET,PHI,PIM2)
      CALL ROTPOd(THET,PHI,PR)
      CALL ROTPOd(THET,PHI,PN)
      CALL ROTPOd(THET,PHI,PAA)
      DGAMT=PHSPAC
      END

      SUBROUTINE ROTPOd(THET,PHI,PP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
      DIMENSION PP(4)
C
      CALL ROTOD2(THET,PP,PP)
      CALL ROTOD3( PHI,PP,PP)
      RETURN
      END

      SUBROUTINE cosdecc(mode,iflag,svar,cdec,phi,wt)
*     ***************************************
! Crude generation of decay costhe according to a simplified distribution.
!   mode: 0-generation
!         1-xccos of given cdec
!   cdec:  value of generated cosine
!   xccos: value of distribution function
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      common / wekin2 / amaw,gammw,gmu,alphaw   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      DOUBLE PRECISION drvec(100)
      save

      if (iflag.eq.0) then
       KeySpn = 0
      else
       KeySpn = MOD(KeyPhy,10000)/1000
      endif

      IF(keyspn.eq.1) THEN                        !1002=.78
        IF(svar.gt.500**2) THEN                    !502=.4
          delta=0.4d0+ (svar/500**2 -1)/8d0
        ELSEIF(svar.gt.4*amaw**2) THEN             !162=.4
          delta=.4d0
        ELSEIF(svar.gt.4*(amaw-5*gammw)**2) THEN   !142=.78
          delta=.4d0+ (1-svar/(4*amaw**2))*2d0
        ELSEIF(svar.gt.4*(amaw-10*gammw)**2) THEN  !122=40
          delta=.844d0+ (1-svar/(4*(amaw-5*gammw)**2))*100d0
        ELSE
          delta=40d0
        ENDIF

        IF(mode.eq.0)THEN
 11       call varran(drvec,3)
          cdec=2*drvec(1)-1
          xccos=(1+delta+cdec)/(1+delta)
          IF((2+delta)/(1+delta)*drvec(2).gt.xccos) goto 11
          phi =2*pi*drvec(3)
        ELSE
          xccos=(1+delta+cdec)/(1+delta)
        ENDIF
      ELSEIF(keyspn.eq.0) THEN
        IF(mode.eq.0)THEN
          call varran(drvec,3)
          cdec=2*drvec(1)-1
          phi =2*pi*drvec(3)
        ENDIF
        xccos=1D0
      ENDIF
      wt= 4*pi/xccos
      end

      SUBROUTINE cosdec_t(mode,ibeam,svar,sprim,s1,s2,ct,fi,ambeam,
     @                    amfi1,amfi2,   cosu,phi,wt)
*     ***************************************
! Crude generation of decay costhe according to a simplified distribution.
!   mode: 0-generation
!         1-xccos of given cdec
!   cdec:  value of generated cosine
!   xccos: value of distribution function
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      common / wekin2 / amaw,gammw,gmu,alphaw   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      common /nevik/ nevtru,ifprint
      save /nevik/

      DOUBLE PRECISION drvec(100)
      save
C simplified version of this routine.
      if (mode.ne.0) costhe=cosu*ibeam
      xx=s2/svar
      beta=sqrt(1d0-s2/svar)
      xlog=log((1+beta)**2/xx)
      xlog=-log((1+beta)**2/xx)
      xlog1=-log(16D0/xx)
      IF(mode.eq.0) then
 5      continue
        call varran(drvec,3)
       IF( drvec(2).lt.1d0/4d0 ) then
        costhe=-1d0/beta*(xx/(1+beta)*exp(xlog*drvec(1))-1d0)
        u=(log((1D0+beta)/4D0))**2 +xlog*xlog1*drvec(1)
        costhe=-1D0/beta*(4D0*EXP(-SQRT(u))-1)
       elseIF( drvec(2).lt.1d0/2d0 ) then
        costhe=-1d0/beta*(xx/(1+beta)*exp(-xlog*drvec(1))-1d0)
!       elseIF( drvec(2).lt.3d0/4d0 ) then
!        costhe= 1d0/beta*(xx/(1+beta)*exp(-xlog*drvec(1))-1d0)
       else
        costhe=2*drvec(1)-1 !
       endif
       phi=2*pi*drvec(3)
      IF (COSTHE.eq.1d0.or.COSTHE.eq.-1D0) goto 5
      cosu=costhe*ibeam
      endif

      cost=min(1d0,costhe)
      if (ifprint.eq.1) then
       write(*,*) 'cosdec-t',xx,svar,costhe,cosu
      endif
      xccos=1d0/4d0+1d0/4d0/beta/(-xlog)*
     $      (1d0/(xx/(1d0+beta)+beta*(1D0-cost)))
!      xccos=xccos+1d0/3d0/beta/(-xlog)*
!     $      (1d0/(xx/(1d0+beta)+beta*(1D0+cost)))

      xccos=xccos+1d0/8d0*beta/(xlog*xlog1
     $     /log(4d0/(xx/(1d0+beta)+beta*(1D0-cost)))
     $     /(4d0/(xx/(1d0+beta)+beta*(1D0-cost))))!!! +1d0/(1+beta*costhe))
! ms 17.06.96 here was wrong sign.
      wt= 4*pi/xccos/2
!
      end




      subroutine zz_dumper(nout,nevtru
     $                     ,wtovr,wtu,wtmax,wtmod,wtmod4f,iflav)    
*     **************************************************************     
c overweighted events monitoring, ZBW version
c 
c Written by: ZBW,MS        date: 
c Last update:             by:  
c

      implicit DOUBLE PRECISION (a-h,o-z)
      common / momset / QEFF1(4),QEFF2(4),sphum(4),sphot(100,4),nphot
      COMMON / cms_eff_momdec /
     $      effbeam1(4),effbeam2(4),p1(4),p2(4),p3(4),p4(4)
      save   / momset /
      PARAMETER (nrch=100)
! it was kanalarz
      COMMON /c_phspz/
     $                prob(nrch),
     $                fak,
     $                faki(nrch),
     $                ikan,
     $                mrchan,
     $                nrchan
      SAVE c_phspz
CBB      INCLUDE 'zz_phsp.h'

      real*4 rrx
      common /erery/ rrx(14)
      save /erery/

      dimension q12(4),q13(4),q14(4),q23(4),q24(4),q34(4),qtot(4)
      dimension qq1(4),qq2(4),qq3(4),qq4(4),XQ1(4),XQ2(4)
      dimension iflav(4)

      do k=1,4
        q12(k)=p1(k)+p2(k)
        q13(k)=p1(k)+p3(k)
        q14(k)=p1(k)+p4(k)
        q23(k)=p2(k)+p3(k)
        q24(k)=p2(k)+p4(k)
        q34(k)=p3(k)+p4(k)
        qtot(k)=p1(k)+p2(k)+p3(k)+p4(k)
      enddo
      xm12=sqrt(dmas2(q12))
      xm13=sqrt(dmas2(q13))
      xm14=sqrt(dmas2(q14))
      xm23=sqrt(dmas2(q23))
      xm24=sqrt(dmas2(q24))
      xm34=sqrt(dmas2(q34))


      do k=1,4
          qq1(k)=      p2(k)+p3(k)+p4(k)
          qq2(k)=p1(k)      +p3(k)+p4(k)
          qq3(k)=p1(k)+p2(k)      +p4(k)
          qq4(k)=p1(k)+p2(k)+p3(k)
      enddo
      xm1=sqrt(dmas2(qq1))
      xm2=sqrt(dmas2(qq2))
      xm3=sqrt(dmas2(qq3))
      xm4=sqrt(dmas2(qq4))
      xmtot=sqrt(dmas2(qtot))

      DO I=1,4
        XQ1(I)=QEFF1(I)
        XQ2(I)=QEFF2(I)
      ENDDO

!        do k=1,3
!         xq1(k)=xq1(k)*(xq1(4)-0.5*amel**2/xq1(4))/xq1(4)
!         xq2(k)=xq2(k)*(xq2(4)-0.5*amel**2/xq2(4))/xq2(4)
!        enddo
      do k=1,4
          qq1(k)=xq1(k)      -p1(k)
          qq2(k)=xq2(k)      -p2(k)
          qq3(k)=xq1(k)      -p3(k) 
          qq4(k)=xq2(k)      -p4(k)
      enddo
      Ym1=sqrt(-dmas2(qq1))
      Ym2=sqrt(-dmas2(qq2))
      Ym3=sqrt(-dmas2(qq3))
      Ym4=sqrt(-dmas2(qq4))
      i6=6
      write(i6,*) '====OVERVEIGHTED EVT. NR: NEVTRU=',NEVTRU,'====='
      write(i6,*) '===generated with channel: ikan=',ikan,'========'
      write(i6,*) 'Note: this event should be outside your detector'
      write(i6,*) 'if not, increase wtmax (?) and/or check manual ?'
      write(i6,*) 'final state: ',IFLAV
      write(i6,*) 'wtu= wtmod*wtmod4f/wtmax=',wtovr/wtmax,
     $                ' wtmod4f=',wtmod4f
      write(i6,*) 'wtu_max=                 ',wtu,'    ... so far'
      write(i6,*) '-----------'
      write(i6,*) 'e-prim=',xmtot,'  wtmod4f=',wtmod4f
      write(i6,*) 'active entries from',mrchan,'to',nrchan,';fak=',fak
      do k=1,nrchan/5+1
           write(i6,*)  'jacobians(',5*k-4,'--',5*k,')=',
     $      real(faki(5*k-4)),real(faki(5*k-3)),real(faki(5*k-2))
     $     ,real(faki(5*k-1)),real(faki(5*k)) 
      enddo
      write(i6,*) '-----------'
      write(i6,*) ' m12=',xm12,' m13=',xm13,' m14=',xm14
      write(i6,*) ' m23=',xm23,' m24=',xm24,' m34=',xm34
      write(i6,*) '-----------'
      write(i6,*) ' m-1=',xm1,' m-2=',xm2,' m-3=',xm3,' m-4=',xm4
      write(i6,*) '-----------'
      write(i6,*) ' T-1=',Ym1,' T-2=',Ym2,' T-3=',Ym3,' T-4=',Ym4
      write(i6,*) '-----------'
      write(i6,*) 'qtot' ,qtot
      write(i6,*) 'qcms',xq1(1)+xq2(1),xq1(2)+xq2(2),
     $           xq1(3)+xq2(3),xq1(4)+xq2(4)
      write(i6,*) '-----------------------'
      write(i6,*) '  QEFF1 ' ,QEFF1
      write(i6,*) 'M-QEFF1 ' ,dmas2(qeff1),' ',sqrt(dmas2(xq1))
      write(i6,*) '-----------------------'
      write(i6,*) '  QEFF2 ',QEFF2
      write(i6,*) 'M-QEFF2 ' ,dmas2(qeff2),' ',sqrt(dmas2(xq2))
      write(i6,*) '-----------------------'
      write(i6,*) '-----------------------'
      write(i6,*) 'p1= ',p1
      write(i6,*) '-----------'
      write(i6,*) 'p2= ',p2
      write(i6,*) '-----------'
      write(i6,*) 'p3= ', p3
      write(i6,*) '-----------'
      write(i6,*) 'p4= ',p4
      write(i6,*) '-----------'
      call dumpw(i6)
      do k=1,14
        write(i6,*) '       RRR(',k,')=',RRX(k)
      enddo

!      write(*,*) '=====amp4f here is problem==============='
!      write(*,*) '=====one of inv is too small============='
!      do i=1,6
!      do j=1,6
!        write(*,*) 'i=',i,' j=',j,' pp=',pp(i,j)
!      enddo
!      enddo

!--------------------------------------------------------------------

      END

      subroutine zz_dumper_short(nout)    
*     **************************************************************     
c overweighted events monitoring, ZBW version
c 
c Written by: ZBW,MS        date: 
c Last update:             by:  
c
      implicit DOUBLE PRECISION (a-h,o-z)
      PARAMETER (nrch=100)
! it was kanalarz
      COMMON /c_phspz/
     $                prob(nrch),
     $                fak,
     $                faki(nrch),
     $                ikan,
     $                mrchan,
     $                nrchan
      SAVE c_phspz
CBB      INCLUDE 'zz_phsp.h'

      i6=nout
      write(i6,*) 'active entries from',mrchan,'to',nrchan,';'
      do k=1,nrchan/5+1
        write(i6,'(A,I3,A,I3,A,5g16.7)')  
     $  'jac.(',5*k-4,'--',5*k,')=',
     $   real(faki(5*k-4)),real(faki(5*k-3)),real(faki(5*k-2))
     $  ,real(faki(5*k-1)),real(faki(5*k)) 
      enddo
      write(i6,*) '----- END DUMP ------'

      END

      SUBROUTINE z_counter(mode,inumber)
!========================================
! counts the callings, for the moment sends result also to the /nevik/
      IMPLICIT DOUBLE PRECISION (a-h,o-z)

      COMMON /nevik/ nevtru,ifprint
      SAVE /nevik/

      SAVE icount

      IF( mode.EQ.-1 ) THEN
! reset
        icount=0d0
      ELSEIF( mode .EQ. 0 ) THEN
! add
        icount=icount+1
      ELSEIF( mode .EQ. 1 ) THEN
! print only
        inumber=icount
      ELSE
        WRITE(6,*)'z_counter=> wrong mode: ',mode
        STOP
      ENDIF

      inumber=icount
      nevtru=icount

      END
      SUBROUTINE make_phsp_point_z
     $      (msdump,ambeam,svar,sprim,fak_phsp,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
*     **************************************
* msdump=0 : generates the phase space point
* msdump=1 : point is red from the disk
* fak_phsp  : weight 
*     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER (nrch=100)
! it was kanalarz
      COMMON /c_phspz/
     $                prob(nrch),
     $                fak,
     $                faki(nrch),
     $                ikan,
     $                mrchan,
     $                nrchan
      SAVE c_phspz
CBB      INCLUDE 'zz_phsp.h'

      DIMENSION
     $      effbeam1(4),effbeam2(4),effp1(4),effp2(4),effp3(4),effp4(4)

      CALL brancher(sprim,itype)

      CALL spacegen(0,itype,svar,sprim,fak_phsp,
     $                       effp1,effp2,effp3,effp4)
      CALL set_eff_beams(sprim,ambeam,effbeam1,effbeam2)

      ikan=itype
      faki(itype)=fak_phsp

      END

      SUBROUTINE get_phsp_weight_z
     $      (svar,sprim,fak_tot,
     $       effbeam1,effbeam2,effp1,effp2,effp3,effp4)
*     **************************************
* calculates jacobians of all channels and does resummation
* fak_tot  : total weight - the only output
*     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER (nrch=100)
! it was kanalarz
      COMMON /c_phspz/
     $                prob(nrch),
     $                fak,
     $                faki(nrch),
     $                ikan,
     $                mrchan,
     $                nrchan
      SAVE c_phspz
CBB      INCLUDE 'zz_phsp.h'

      DIMENSION
     $      effbeam1(4),effbeam2(4),effp1(4),effp2(4),effp3(4),effp4(4)

      fak=0d0
*      DO i=mrchan,nrchan
      DO i=1,nrchan
         IF (i .NE. ikan .AND. prob(i) .GT. 0d0)
     $        CALL spacegen(1,i,svar,sprim,faki(i),
     $                       effp1,effp2,effp3,effp4)
*      IF (i .EQ. ikan) WRITE(*,*)'fakusie(',i,')=',faki(i)/faki(ikan)
         IF (prob(i) .GT. 0d0) THEN
            fak=fak+prob(i)/faki(i)
         ELSE
            faki(i)=0d0
         ENDIF
         IF(prob(i) .NE. 0d0 .AND. faki(i) .EQ. 0d0) THEN
            WRITE(*,*) 
     $           'karludw: jacobian(i)=0d0  ikan=',ikan,'i=',i
            WRITE(*,*) 
     $           'i from - to',mrchan,'  ',nrchan,'prob(i)',prob(i)
            WRITE(*,*) 'p1=',p1
            WRITE(*,*) 'p2=',p2
            WRITE(*,*) 'p3=',p3
            WRITE(*,*) 'p4=',p4
         ENDIF
      ENDDO

      fak=1d0/fak

      fak_tot=fak

      END



      subroutine spacegen(mode,itype,svar,sprim,fakir,
     $                       bp1,bp2,bp3,bp4)
********************************************************
! ================================================================
! mode=0                                                         =
!        generates 4-momenta accordingly to itype of generation, =
!        calculates jacobian (out from 4-momenta)                =
! mode=1                                                         =
!        calculates jacobian (out from 4-momenta)                =
!        for itype generation branch                             =
! ================================================================
      implicit DOUBLE PRECISION (a-h,o-z)
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW 
      COMMON / MATPAR / PI,CEULER       
      COMMON / DECAYS / IFLAV(4), AMDEC(4) 
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      save  / WEKING /,/ WEKIN2 /,/ MATPAR /,/ KeyKey/
      save  / DECAYS /
c ms      parameter (NRCH=100)
c ms      common /kanalarz/ fak,fakI(NRCH),ikan,MRCHAN,NRCHAN
c ms      save  /kanalarz/

      dimension bq1(4),bq2(4),bp1(4),bp2(4),bp3(4),bp4(4)
      common /nevik/ nevtru,ifprint
      save /nevik/
      SAVE
!
      ifprint=0
!      =====================================
!      ====================================
       IF (ITYPE.GT.62.and.itype.lt.70) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE-62
        igcl=7
      IF     (IRODZ.EQ.1) THEN
       igcl=5
!       if (nevtru.eq.-236) ifprint=1
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP3,amdec(3),bP2,amdec(2))
       ifprint=0
      elseIF (IRODZ.EQ.2) THEN
       igcl=6
!       if (nevtru.eq.-269) ifprint=1
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP3,amdec(3),bP2,amdec(2),bP1,amdec(1),bP4,amdec(4))
       ifprint=0
      elseIF (IRODZ.EQ.3) THEN
       igcl=5
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP3,amdec(3),bP2,amdec(2),bP1,amdec(1),bP4,amdec(4))
      elseIF (IRODZ.EQ.4) THEN
       igcl=6
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP3,amdec(3),bP2,amdec(2))
      elseIF (IRODZ.EQ.5) THEN
       igcl=1
       CALL kinchce(IGCL,MODE,Amaw,gammw,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      elseIF (IRODZ.EQ.6) THEN
       igcl=1
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      elseIF (IRODZ.EQ.7) THEN
       igcl=1
       CALL kinchce(IGCL,MODE,Amaz,amaz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================
!      =====================================
!      =====================================
!      =====================================
       IF (ITYPE.GT.58.and.itype.lt.63) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE-58
        igcl=10
      IF     (IRODZ.EQ.1) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,0.d0,SPRIM,fakir,
     $     bP2,amdec(2),bP3,amdec(3),bP1,amdec(1),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.2) THEN
       if (nevtru.eq.-226) ifprint=1
       CALL kinchce(IGCL,MODE,ADUM,GDUM,1d0,SPRIM,fakir,
     $     bP3,amdec(3),bP2,amdec(2),bP1,amdec(1),bP4,amdec(4))
       ifprint=0
      ELSEIF (IRODZ.EQ.3) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,0d0,SPRIM,fakir,
     $     bP4,amdec(4),bP1,amdec(1),bP2,amdec(2),bP3,amdec(3))
      ELSEIF (IRODZ.EQ.4) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,1d0,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP2,amdec(2),bP3,amdec(3))
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================

!      =====================================
!      =====================================
       IF (ITYPE.GT.54.and.itype.lt.59) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE-54
        igcl=9
      IF     (IRODZ.EQ.1) THEN
       CALL kinchce(IGCL,MODE,AMAZ,-GAMMZ,0D0,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP3,amdec(3),bP2,amdec(2))
      elseIF (IRODZ.EQ.2) THEN
       CALL kinchce(IGCL,MODE,AMAZ,-GAMMZ,1D0,SPRIM,fakir,
     $     bP4,amdec(4),bP1,amdec(1),bP3,amdec(3),bP2,amdec(2))
      ELSEIF (IRODZ.EQ.3) THEN
       if (nevtru.eq.-7) ifprint=1
       CALL kinchce(IGCL,MODE,AMAZ,-GAMMZ,0D0,SPRIM,fakir,
     $     bP3,amdec(3),bP2,amdec(2),bP1,amdec(1),bP4,amdec(4))
       ifprint=0
      ELSEIF (IRODZ.EQ.4) THEN
       if (nevtru.eq.-43026) ifprint=1
       CALL kinchce(IGCL,MODE,AMAZ,-GAMMZ,1D0,SPRIM,fakir,
     $     bP2,amdec(2),bP3,amdec(3),bP1,amdec(1),bP4,amdec(4))
       ifprint=0
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================

!      ====================================
       IF (ITYPE.GT.50.and.itype.lt.55) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE-50
        igcl=7
      IF     (IRODZ.EQ.1) THEN
       igcl=5
!       if (nevtru.eq.-236) ifprint=1
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP3,amdec(3),bP4,amdec(4),bP1,amdec(1),bP2,amdec(2))
       ifprint=0
      elseIF (IRODZ.EQ.2) THEN
       igcl=6
!       if (nevtru.eq.-269) ifprint=1
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
       ifprint=0
      elseIF (IRODZ.EQ.3) THEN
       igcl=5
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      elseIF (IRODZ.EQ.4) THEN
       igcl=6
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP3,amdec(3),bP4,amdec(4),bP1,amdec(1),bP2,amdec(2))
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================

!      =====================================
!      =====================================
       IF (ITYPE.GT.46.and.itype.lt.51) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE-46
        igcl=8
      IF     (IRODZ.EQ.1) THEN
       CALL kinchce(IGCL,MODE,AMAZ,-gammz,0D0,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      elseIF (IRODZ.EQ.2) THEN
       CALL kinchce(IGCL,MODE,AMAZ,-gammz,1D0,SPRIM,fakir,
     $     bP2,amdec(2),bP1,amdec(1),bP3,amdec(3),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.3) THEN
       CALL kinchce(IGCL,MODE,AMAZ,-gammz,0D0,SPRIM,fakir,
     $     bP3,amdec(3),bP4,amdec(4),bP1,amdec(1),bP2,amdec(2))
      ELSEIF (IRODZ.EQ.4) THEN
       CALL kinchce(IGCL,MODE,AMAZ,-gammz,1D0,SPRIM,fakir,
     $     bP4,amdec(4),bP3,amdec(3),bP1,amdec(1),bP2,amdec(2))
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================
       IF (ITYPE.GT.42.and.itype.lt.47) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE-42
        igcl=10
      IF     (IRODZ.EQ.1) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,0.d0,SPRIM,fakir,
     $     bP4,amdec(4),bP3,amdec(3),bP1,amdec(1),bP2,amdec(2))
      ELSEIF (IRODZ.EQ.2) THEN
       if (nevtru.eq.-226) ifprint=1
       CALL kinchce(IGCL,MODE,ADUM,GDUM,1d0,SPRIM,fakir,
     $     bP3,amdec(3),bP4,amdec(4),bP1,amdec(1),bP2,amdec(2))
       ifprint=0
      ELSEIF (IRODZ.EQ.3) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,0d0,SPRIM,fakir,
     $     bP2,amdec(2),bP1,amdec(1),bP3,amdec(3),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.4) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,1d0,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================

!      =====================================
!      =====================================
       IF (ITYPE.GT.38.and.itype.lt.43) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE-38
        igcl=9
      IF     (IRODZ.EQ.1) THEN
       if (nevtru.eq.-3320) ifprint=1
       CALL kinchce(IGCL,MODE,AMAZ,-GAMMZ,0D0,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
       ifprint=0
      elseIF (IRODZ.EQ.2) THEN
       CALL kinchce(IGCL,MODE,AMAZ,-GAMMZ,1D0,SPRIM,fakir,
     $     bP2,amdec(2),bP1,amdec(1),bP3,amdec(3),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.3) THEN
       if (nevtru.eq.-7) ifprint=1
       CALL kinchce(IGCL,MODE,AMAZ,-GAMMZ,0D0,SPRIM,fakir,
     $     bP3,amdec(3),bP4,amdec(4),bP1,amdec(1),bP2,amdec(2))
       ifprint=0
      ELSEIF (IRODZ.EQ.4) THEN
       if (nevtru.eq.-43026) ifprint=1
       CALL kinchce(IGCL,MODE,AMAZ,-GAMMZ,1D0,SPRIM,fakir,
     $     bP4,amdec(4),bP3,amdec(3),bP1,amdec(1),bP2,amdec(2))
       ifprint=0
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================

!      =====================================
!      =====================================
       IF (ITYPE.GT.30.and.itype.lt.39) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE-30
        igcl=8
      IF     (IRODZ.EQ.1) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,0D0,SPRIM,fakir,
     $     bP1,amdec(1),bP3,amdec(3),bP2,amdec(2),bP4,amdec(4))
      elseIF (IRODZ.EQ.2) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,0D0,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP2,amdec(2),bP3,amdec(3))
      ELSEIF (IRODZ.EQ.3) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,1D0,SPRIM,fakir,
     $     bP2,amdec(2),bP3,amdec(3),bP1,amdec(1),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.4) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,1D0,SPRIM,fakir,
     $     bP2,amdec(2),bP4,amdec(4),bP1,amdec(1),bP3,amdec(3))

      ELSEIF (IRODZ.EQ.5) THEN
         if (nevtru.eq.-80  ) ifprint=1
         if (nevtru.eq.-109856  ) write(*,*) nevtru
       CALL kinchce(IGCL,MODE,AMAW,amaw,0D0,SPRIM,fakir,
     $     bP3,amdec(3),bP1,amdec(1),bP4,amdec(4),bP2,amdec(2))
      ELSEIF (IRODZ.EQ.6) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,0D0,SPRIM,fakir,
     $     bP3,amdec(3),bP2,amdec(2),bP4,amdec(4),bP1,amdec(1))
      ELSEIF (IRODZ.EQ.7) THEN
      if (nevtru.eq.-113526) write(*,*) 'nevtru=',nevtru
      if (nevtru.eq.-24) ifprint=1
       CALL kinchce(IGCL,MODE,AMAW,amaw,1D0,SPRIM,fakir,
     $     bP4,amdec(4),bP1,amdec(1),bP3,amdec(3),bP2,amdec(2))
      ELSEIF (IRODZ.EQ.8) THEN
!       if (nevtru.eq.-17170) ifprint=1
       if (nevtru.eq.-9743) ifprint=1
       CALL kinchce(IGCL,MODE,AMAW,amaw,1D0,SPRIM,fakir,
     $     bP4,amdec(4),bP2,amdec(2),bP3,amdec(3),bP1,amdec(1))
       ifprint=0
!      if (mode.eq.0) write(*,*) bp4
!      if (mode.eq.0) write(*,*) bp2
 !     if (mode.eq.0) write(*,*) bp3
!      if (mode.eq.0) write(*,*) bp1
  !    if (mode.eq.0) write(*,*) '==========================='
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================
!      =====================================
!      ====================================
       IF (ITYPE.GT.20.and.itype.lt.31) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE-20
        igcl=7
      IF     (IRODZ.EQ.1) THEN
       igcl=3
       if (nevtru.eq.-8191) ifprint=1
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
       ifprint=0
      elseIF (IRODZ.EQ.2) THEN
       igcl=3
       CALL kinchce(IGCL,MODE,Amaz,gammz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP2,amdec(2),bP3,amdec(3))
      ELSEIF (IRODZ.EQ.3) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP4,amdec(4),bP3,amdec(3),bP1,amdec(1),bP2,amdec(2))
      ELSEIF (IRODZ.EQ.4) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP3,amdec(3),bP4,amdec(4),bP1,amdec(1),bP2,amdec(2))
      ELSEIF (IRODZ.EQ.5) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP2,amdec(2),bP1,amdec(1),bP3,amdec(3),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.6) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.7) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP2,amdec(2),bP3,amdec(3),bP4,amdec(4),bP1,amdec(1))
      ELSEIF (IRODZ.EQ.8) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP3,amdec(3),bP2,amdec(2),bP4,amdec(4),bP1,amdec(1))
      ELSEIF (IRODZ.EQ.9) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP4,amdec(4),bP1,amdec(1),bP2,amdec(2),bP3,amdec(3))
      ELSEIF (IRODZ.EQ.10) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP2,amdec(2),bP3,amdec(3))
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================

!      =====================================
!      =====================================
       IF (ITYPE.GT.10.and.itype.lt.21) THEN
!      =====================================
!      =====================================
        igcl=8
        IRODZ=ITYPE-10
      IF     (IRODZ.EQ.1) THEN
       CALL kinchce(IGCL,MODE,AMAW,GAMMW,0D0,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.2) THEN
       CALL kinchce(IGCL,MODE,AMAZ,GAMMZ,0D0,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP2,amdec(2),bP3,amdec(3))
      ELSEIF (IRODZ.EQ.3) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,0D0,SPRIM,fakir,
     $     bP4,amdec(4),bP3,amdec(3),bP2,amdec(2),bP1,amdec(1))
      ELSEIF (IRODZ.EQ.4) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,0D0,SPRIM,fakir,
     $     bP1,amdec(1),bP3,amdec(3),bP2,amdec(2),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.5) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,0D0,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP2,amdec(2),bP3,amdec(3))

      ELSEIF (IRODZ.EQ.6) THEN
       CALL kinchce(IGCL,MODE,AMAW,GAMMW,1D0,SPRIM,fakir,
     $     bP4,amdec(4),bP3,amdec(3),bP2,amdec(2),bP1,amdec(1))
      ELSEIF (IRODZ.EQ.7) THEN
       CALL kinchce(IGCL,MODE,AMAZ,GAMMZ,1D0,SPRIM,fakir,
     $     bP4,amdec(4),bP1,amdec(1),bP3,amdec(3),bP2,amdec(2))
      ELSEIF (IRODZ.EQ.8) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,1D0,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      ELSEIF (IRODZ.EQ.9) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,1D0,SPRIM,fakir,
     $     bP4,amdec(4),bP2,amdec(2),bP3,amdec(3),bP1,amdec(1))
      ELSEIF (IRODZ.EQ.10) THEN
       CALL kinchce(IGCL,MODE,AMAW,amaw,1D0,SPRIM,fakir,
     $     bP4,amdec(4),bP1,amdec(1),bP3,amdec(3),bP2,amdec(2))

      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif
        RETURN
       ENDIF
!      =====================================
!      =====================================
       IF (ITYPE.GT. 6.and.itype.lt.11) THEN
!      =====================================
!      =====================================
        igcl=7
        IRODZ=ITYPE-6
      IF     (IRODZ.EQ.1) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP2,amdec(2),bP3,amdec(3))
      ELSEIF (IRODZ.EQ.2) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP4,amdec(4),bP1,amdec(1),bP2,amdec(2),bP3,amdec(3))
      ELSEIF (IRODZ.EQ.3) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP2,amdec(2),bP3,amdec(3),bP4,amdec(4),bP1,amdec(1))
      ELSEIF (IRODZ.EQ.4) THEN
       CALL kinchce(IGCL,MODE,ADUM,GDUM,PDUM,SPRIM,fakir,
     $     bP3,amdec(3),bP2,amdec(2),bP4,amdec(4),bP1,amdec(1))
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================
       IF (ITYPE.GT. 0.and.itype.lt. 7) THEN
!      =====================================
!      =====================================
!
        IRODZ=ITYPE
      IF     (IRODZ.EQ.1) THEN
       igcl=1    
       CALL kinchce(IGCL,MODE,Amaw,gammw,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      elseIF (IRODZ.EQ.2) THEN
       igcl=2    
       CALL kinchce(IGCL,MODE,Amaw*5,gammw,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      elseIF (IRODZ.EQ.3) THEN
       igcl=3
       CALL kinchce(IGCL,MODE,amaz,gammz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP3,amdec(3),bP2,amdec(2))
      elseIF (IRODZ.EQ.4) THEN
       igcl=4
       CALL kinchce(IGCL,MODE,amaw,gammw,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP2,amdec(2),bP3,amdec(3),bP4,amdec(4))
      elseIF (IRODZ.EQ.5) THEN
       igcl=4
       CALL kinchce(IGCL,MODE,amaz,gammz,svar,SPRIM,fakir,
     $     bP1,amdec(1),bP4,amdec(4),bP3,amdec(3),bP2,amdec(2))
      elseIF (IRODZ.EQ.6) THEN
       igcl=4
       CALL kinchce(IGCL,MODE,amaz,gammz,svar,SPRIM,fakir,
     $     bP3,amdec(3),bP2,amdec(2),bP1,amdec(1),bP4,amdec(4))
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      if (mode.eq.0) then
      do i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      enddo
      endif 
        RETURN
       ENDIF
!      =====================================
!      =====================================
   
       write(*,*) 'spacegen; wrong ITYPE=',itype
       stop
!!!
      end
C======================================================================
C======================= G L I B K  ===================================
C==================General Library of utilities========================
C===========It is imilar but not identical to HBOOK and HPLOT==========
C======================================================================
C   
C                      Version:    1.10
C              Last correction:    July 1996
C
C
C  Installation remarks: 
C  (1) printing backslash character depends on F77 compilator,
C      user may need to modify definition of BS variable in HPLCAP
C
C  Usage of the program:
C  (1) In most cases names and meanings of programs and their 
C      parameters is the same as in original CERN libraries HBOOK
C  (2) Unlike to original HBOOK and HPLOT, all floating parameters 
C      of the programs are in double precision!
C  (3) GLIBK stores histograms in double precision and always with
C      errors. REAL*8 storage is essential for 10**7 events statistics!
C  (4) Output from GLIBK is a picture recorded as regular a LaTeX file 
C      with frame and curves/histograms, it is easy to change fonts
C      add captions, merge plots, etc. by normal ediding. Finally,
C      picture may be inserted in any place into LaTeX source of the
C      article.
C
C  ********************************************************************
C  *  History of the program:                                         *
C  *  MINI-HBOOK writen by S. Jadach, Rutherford Lab. 1976            *
C  *  Rewritten December 1989 (S.J.)                                  *
C  *  Version with DOUBLE PRECISION ARGUMENTS ONLY!  and SAVE         *
C  *  Subrogram names start with G instead of H letter!               *
C  *  Entries:   Obligatory:  GLIMIT                                  *
C  *             Optional: see table below                            *
C  *  non-user subprograms in brackets                                *
C  ********************************************************************
C    SUBR/FUNC  1 PAR. 2 PAR. 3 PAR. 4 PAR. 5 PAR. 6 PAR.       
C  ====================================================================
*     (GINIT)   ----   ----    ----   ----   ----   ----        
*      GI       INT    INT     ----   ----   ----   ----        
*      GIE      INT    INT     ----   ----   ----   ----        
*      GF1      INT    DBL     DBL    ----   ----   ----        
*      GFILL    INT    DBL     DBL    DBL    ----   ----        
*      GBOOK1   INT    CHR*80  INT    DBL    DBL    ----  
*     (GOPTOU)  INT    INT     INT    INT    INT     INT
* (L.F. GEXIST) INT    -----  ------  ----   ----   ----        
*      GIDOPT   INT    CHR*4   -----  ----   ----   ----        
*      GBFUN1   INT    CHR*80   INT   DBL    DBL  DP-FUNC       
*      GIDOPT   INT    CHR*4   -----  ----   ----   ----        
*      GBOOK2   INT    CHR*80   INT   DBL    DBL     INT   DBL   DBL
*      GISTDO     ---   ----   ----   ----   ----   ----        
*      GOUTPU   INT     ----   ----   ----   ----   ----        
*      GPRINT   INT     ----   ----   ----   ----   ----        
*      GOPERA   INT    CHR*1   INT    INT    DBL    DBL         
*      GINBO1   INT    CHR*8   INT    DBL    DBL    ----        
*      GUNPAK   INT    DBL(*) CHR*(*) INT    ---    ----        
*      GPAK     INT    DBL(*)  ----   ----   ---    ----        
*      GPAKE    INT    DBL(*)  ----   ----   ---    ----       
*      GRANG1   INT    DBL     DBL    ----   ---    ----        
*      GINBO2   INT    INT     DBL    DBL    INT    DBL   DBL      
*      GMAXIM   INT    DBL     ----   ----   ---    ----        
*      GMINIM   INT    DBL     ----   ----   ---    ----        
*      GRESET   INT   CHR*(*)  ----   ----   ---    ----        
*      GDELET   INT     ----   ----   ----   ----   ----        
*      GLIMIT   INT     ----   ----   ----   ----   ----        
*     (COPCH)   CHR*80 CHR*80  ----   ----   ----   ----        
* (F. JADRES)   INT     ----   ----   ----   ----   ----        
*      GRFILE   INT   CHR*(*) CHR*(*) ----   ----   ----        
*      GROUT    INT    INT    CHR*8   ----   ----   ----        
*      GRIN     INT    INT     INT    ----   ----   ----        
*      GREND   CHR*(*) ----    ----   ----   ----   ----        
C  *******************  HPLOT entries ******************
*      GPLINT   INT    ----    ----   ----   ----   ----        
*      GPLCAP   INT    ----    ----   ----   ----   ----        
*      GPLEND   ----   ----    ----   ----   ----   ----        
*      GPLOT    INT    CHR*1   CHR*1   INT   ----   ----        
*     (LFRAM1)  INT      INT     INT  ----   ----   ----        
*     (SAXIX)   INT      DBL     DBL   INT    DBL   ----        
*     (SAXIY)   INT      DBL     DBL   INT    DBL   ----        
*     (PLHIST)  INT      INT     DBL   DBL    INT    INT        
*     (PLHIS2)  INT      INT     DBL   DBL    INT    INT        
*     (PLCIRC)  INT      INT     INT   DBL    DBL    DBL        
*     (APROF)   DBL      INT     DBL  ----   ----   ----        
*      GPLSET   INT      DBL    ----  ----   ----   ----        
*      GPLTIT   INT    CHR*80   ----  ----   ----   ----        
C  *******************  WMONIT entries ******************
*      GMONIT   INT ???
C  *******************************************************************
C                         END OF TABLE        
C  *******************************************************************
*          Map of memory for single histogram
*          ----------------------------------
*  (1-7) Header
*  ist +1   mark      9999999999999
*  ist +2   mark      9d12 + id*10 + 9
*  ist +3   iflag1    9d12 + iflag1*10 +9
*  ist +4   iflag2    9d12 + iflag2*10 +9
*  ist +5   scamin    minimum y-scale
*  ist +6   scamax    maximum y-scale
*  ist +7   jdlast    address of the next histogram 
*                     from previous history of calls (see jadres)
*          ----------------------------------
*              Binning size informations
*          ----------------------------------
*  One dimensional histogram            Two dimensional histog.
*  -------------------------            ----------------------
*  (8-11) Binning information           (8-15) Binning information
*  ist2 +1    NCHX                          ist2 +5   NCHY
*  ist2 +2      XL                          ist2 +6     YL
*  ist2 +3      XU                          ist2 +7     YU
*  ist2 +4   FACTX                          ist2 +8  FACTY
*
*          ----------------------------------
*           All kind of sums except of maxwt
*          ----------------------------------
*  (12-24) Under/over-flow average x    (16-24)
*  ist3 +1   Underflow                     All nine combinations
*  ist3 +2   Normal                        (U,N,O) x (U,N,O)
*  ist3 +3   Overerflow                    sum wt only (no errors)
*  ist3 +4   U  sum w**2
*  ist3 +5   N  sum w**2
*  ist3 +6   O  sum w**2
*  ist3 +7   Sum 1
*  ist3 +8   Sum wt*x
*  ist3 +9   Sum wt*x*x
*  ist3 +10  nevzer    (gmonit)
*  ist3 +11  nevove    (gmonit)
*  ist3 +12  nevacc    (gmonit)
*  ist3 +13  maxwt     (gmonit)
*          ----------------------------------
*           Content of bins including errors
*          ----------------------------------
*  (25 to 24+2*nchx)                     (25 to 24 +nchx*nchy)
*     sum wt and sum wt**2            sum wt only (no errors)
*  ----------------------------------------------------------------

      SUBROUTINE ginit
*     ****************
! First Initialization called from may routines
*     *************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      SAVE   / cglib /
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DATA init /0/
      SAVE init
*
      IF(init .NE. 0) RETURN
      init=1
c this is version version number
      nvrs=111
c default output unit
      nout=16
      lenmax=0
      length=0
      DO i=1,idmx
         DO k=1,3
            index(i,k)=0
         ENDDO
         DO k=1,80
            titlc(i)(k:k)=' '
         ENDDO
      ENDDO
      DO k=1,50000
         b(k)=0d0
      ENDDO
      END

      SUBROUTINE gflush
*     ****************
! FLUSH memory, all histos erased!
*     *************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      SAVE   / cglib /
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/

      CALL ginit
      length=0
      DO i=1,idmx
         DO k=1,3
            index(i,k)=0
         ENDDO
         DO k=1,80
            titlc(i)(k:k)=' '
         ENDDO
      ENDDO
      DO k=1,50000
         b(k)=0d0
      ENDDO
      END

      LOGICAL FUNCTION gexist(id)
!     ***************************
! this function is true when id  exists !!!! 
!     ***************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      gexist = lact .NE. 0
c###  IF(gexist)      write(6,*) 'gexist: does   ID,lact= ',id,lact
c###  IF(.not.gexist) write(6,*) 'gexist: doesnt ID,lact= ',id,lact
      END

      function gi(id,ib)
*     ******************
C getting out bin content
C S.J. 18-Nov. 90
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      save idmem,nch,lact,ist,ist2,ist3
      data idmem / -1256765/
c
      IF(id .EQ. idmem) goto 100
      idmem=id
c some checks, not repeated if id the same as previously
      lact=jadres(id)
      IF(lact .EQ. 0) then
        write(nout,*) ' gi: nonexisting histo id=',id
        write(   6,*) ' gi: nonexisting histo id=',id
        gi= 0d0
        stop
      ENDIF
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
c checking if histo is of proper type
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) then
        write(nout,*) ' gi: 1-dim histos only !!! id=',id
        write(   6,*) ' gi: 1-dim histos only !!! id=',id
        gi= 0d0
        stop
      ENDIF
  100 continue
      nch  = nint(b(ist2+1))
      IF(ib .EQ. 0) then
c underflow
         gi=   b(ist3 +1)
      ELSEIF(ib .GE. 1.and.ib .LE. nch) then
c normal bin
         gi=   b(ist +nbuf+ib)
      ELSEIF(ib .EQ. nch+1) then
c overflow
         gi=   b(ist3 +3)
      ELSE
c abnormal exit
         write(nout,*) ' gi: wrong binning id,ib=',id,ib
         write(   6,*) ' gi: wrong binning id,ib=',id,ib
         gi=0d0
         stop
      ENDIF
      end

      function  gie(id,ib)
*     ********************
c getting out error of the bin
c s.j. 18-nov. 90
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      save idmem,nch,lact,ist,ist2,ist3
      data idmem / -1256765/
c
      IF(id .EQ. idmem) goto 100
      idmem=id
c some checks, not repeated if id the same as previously
      lact=jadres(id)
      IF(lact .EQ. 0) then
        write(nout,*) ' gie: nonexisting histo id=',id
        write(   6,*) ' gie: nonexisting histo id=',id
        gie= 0d0
        stop
      ENDIF
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
c checking if histo is of proper type
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) then
        write(nout,*) ' gie: 1-dim histos only !!! id=',id
        write(   6,*) ' gie: 1-dim histos only !!! id=',id
        gie= 0d0
        stop
      ENDIF
  100 continue
      nch  = b(ist2+1)
      IF(ib .EQ. 0) then
c underflow
         gie=   dsqrt( dabs(b(ist3 +4)))
      ELSEIF(ib .GE. 1.and.ib .LE. nch) then
c...normal bin, error content
         gie=   dsqrt( dabs(b(ist+nbuf+nch+ib)) )
      ELSEIF(ib .EQ. nch+1) then
c overflow
         gie=   dsqrt( dabs(b(ist3 +6)))
      ELSE
c abnormal exit
         write(nout,*) ' gie: wrong binning id, ib=',id,ib
         write(   6,*) ' gie: wrong binning id, ib=',id,ib
         gie=0d0
         stop
      ENDIF
      end

      subroutine gf1(id,x,wtw)
*     ************************
c recommended fast filling 1-dim. histogram
c s.j. 18 nov. 90
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
c exit for non-existig histo
      IF(lact .EQ. 0)  return
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
c one-dim. histo only
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) return
      xx= x
      wt= wtw
      index(lact,3)=index(lact,3)+1
c all entries
      b(ist3 +7)  =b(ist3 +7)   +1
c for average x
      b(ist3 +8)  =b(ist3 +8)  +wt*xx
      b(ist3 +9)  =b(ist3 +9)  +wt*xx*xx
c filling bins
      nchx  =b(ist2 +1)
      xl    =b(ist2 +2)
      xu    =b(ist2 +3)
      factx =b(ist2 +4)
!!!      kx = (xx-xl)*factx+1d0
!!!      IF(kx .LT. 1) then
      IF(xx .LT. xl) then
c underflow
         b(ist3 +1)    = b(ist3 +1)         +wt
         b(ist3 +4)    = b(ist3 +4)         +wt*wt
!!!      ELSEIF(kx .GT. nchx) then
      ELSEIF(xx .GT. xu) then
c overflow
         b(ist3 +3)    = b(ist3 +3)         +wt
         b(ist3 +6)    = b(ist3 +6)         +wt*wt
      ELSE
c normal bin
         kx = (xx-xl)*factx+1d0
         kx = max(kx,1)    !!!   SJ 28.09.96
         kx = min(kx,nchx) !!!   SJ 28.09.96
         b(ist3 +2)    = b(ist3 +2)         +wt
         b(ist +nbuf+kx) = b(ist+nbuf+kx)   +wt
c normal bin error 
         b(ist3 +5)    = b(ist3 +5)         +wt*wt
         b(ist +nbuf+nchx+kx) = b(ist+nbuf+nchx+kx)   +wt**2
      ENDIF
      end

      subroutine gfill(id,x,y,wtw)
*     ****************************
c this routine not finished, 1-dim only!
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0)  return
      ist  = index(lact,2)
c one-dim. histo 
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 1) then
c...one-dim. histogram
        call gf1(id,x,wtw)
        return
      ENDIF
c...two-dim. scattergram, no errors!
      ist2 = ist+7
      ist3 = ist+15
      xx= x
      yy= y
      wt= wtw
      index(lact,3)=index(lact,3)+1
c x-axis
      nchx  =b(ist2 +1)
      xl    =b(ist2 +2)
      factx =b(ist2 +4)
      kx=(xx-xl)*factx+1d0
      lx=2
      IF(kx .LT. 1)     lx=1
      IF(kx .GT. nchx)  lx=3
      l     = ist+34  +lx
      b(l)  = b(l)    +wt
      k     = ist+nbuf2  +kx
      IF(lx .EQ. 2) b(k)  =b(k)  +wt
      k2    = ist+nbuf2  +nchx+kx
      IF(lx .EQ. 2) b(k2) =b(k2) +wt**2
c y-axix
      nchy  =b(ist2 +5)
      yl    =b(ist2 +6)
      facty =b(ist2 +8)
      ky=(yy-yl)*facty+1d0
      ly=2
      IF(ky .LT. 1)    ly=1
      IF(ky .GT. nchy) ly=3
c under/over-flow
      l = ist3  +lx +3*(ly-1)
      b(l) =b(l)+wt
c regular bin
      k = ist+nbuf2 +kx +nchx*(ky-1)
      IF(lx .EQ. 2.and.ly .EQ. 2) b(k)=b(k)+wt
      end

      subroutine gbook1(id,title,nnchx,xxl,xxu)
*     *****************************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      character*80 title
      logical gexist
c
      call ginit
      IF(gexist(id)) goto 900
      ist=length
      lact=jadres(0)
c the case of no free entry in the index
      IF(lact .EQ. 0) goto 901
      index(lact,1)=id
      index(lact,2)=length
      index(lact,3)=0
*----
ccc      write(6,*) 'GBOOK1: ID= ',ID
c -------
      call copch(title,titlc(lact))
      nchx =nnchx
      xl   =xxl
      xu   =xxu
c ---------- title and bin content ----------
      lengt2 = length +2*nchx +nbuf+1
      IF(lengt2 .GE. lenmax) goto 902
      do 10 j=length+1,lengt2+1
  10  b(j) = 0d0
      length=lengt2
c... default flags
      ioplog   = 1
      iopsla   = 1
      ioperb   = 1
      iopsc1   = 1
      iopsc2   = 1
      iflag1   = 
     $ ioplog+10*iopsla+100*ioperb+1000*iopsc1+10000*iopsc2
      ityphi   = 1
      iflag2   = ityphi
C examples of decoding flags 
c      id       = nint(b(ist+2)-9d0-9d12)/10
c      iflag1   = nint(b(ist+3)-9d0-9d12)/10
c      ioplog = mod(iflag1,10)
c      iopsla = mod(iflag1,100)/10
c      ioperb = mod(iflag1,1000)/100
c      iopsc1 = mod(iflag1,10000)/1000
c      iopsc2 = mod(iflag1,100000)/10000
c      iflag2   = nint(b(ist+4)-9d0-9d12)/10
c      ityphi = mod(iflag2,10)
c--------- buffer -----------------
c header
      b(ist +1)  = 9999999999999d0
      b(ist +2)  = 9d12 +     id*10 +9d0
      b(ist +3)  = 9d12 + iflag1*10 +9d0
      b(ist +4)  = 9d12 + iflag2*10 +9d0
c dummy vertical scale
      b(ist +5)  =  -100d0
      b(ist +6)  =   100d0
c pointer used to speed up search of histogram address
      b(ist +7)  =   0d0
c information on binning
      ist2       = ist+7
      b(ist2 +1) = nchx
      b(ist2 +2) = xl
      b(ist2 +3) = xu
      ddx = xu-xl
      IF(ddx .EQ. 0d0) goto 903
      b(ist2 +4) = float(nchx)/ddx
c under/over-flow etc.
      ist3       = ist+11
      do 100  j=1,13
 100  b(ist3 +j)=0d0
c
      RETURN
 900  continue
      write(6   ,*) ' WARNING gbook1: already exists id=  ', id
      write(NOUT,*) ' WARNING gbook1: already exists id=  ', id
      RETURN      
 901  continue
      call gstop1(' gbook1: to many histos !!!!!,     id=  ',id)
 902  continue
      call gstop1(' gbook1: to litle storage!!!!,  lenmax= ',lenmax)
 903  continue
      call gstop1('  gbook1:    xl=xu,               id=   ',id)
      end

      subroutine gstop1(mesage,id)
*     *******************************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save   /gind/
      character*40 mesage

      write(nout,'(a)') 
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      write(nout,'(a,a,i10,a)')  
     $                          '+ ', mesage, id, ' +'
      write(nout,'(a)') 
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      write(6   ,'(a)') 
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      write(6   ,'(a,a,i10,a)')  
     $                          '+ ', mesage, id, ' +'
      write(6   ,'(a)') 
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      stop
      end


      subroutine goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
c     ********************************************************
c decoding option flags
c     **********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/

      lact=jadres(id)
      IF(lact .EQ. 0) return
      ist=index(lact,2)
c decoding flags 
      iflag1   = nint(b(ist+3)-9d0-9d12)/10
      ioplog = mod(iflag1,10)
      iopsla = mod(iflag1,100)/10
      ioperb = mod(iflag1,1000)/100
      iopsc1 = mod(iflag1,10000)/1000
      iopsc2 = mod(iflag1,100000)/10000
      end

      subroutine gidopt(id,ch)
c     ************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      character*4 ch
c
      lact=jadres(id)
      IF(lact .EQ. 0) return
      ist=index(lact,2)
C decoding flags 
      call goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      IF(ch .EQ.       'LOGY'  ) then
c log scale for print
        ioplog = 2 
      ELSEIF(ch .EQ.   'ERRO'  ) then
C errors in printing/plotting
       ioperb  = 2
      ELSEIF(ch .EQ.   'SLAN'  ) then
c slanted line in plotting
       iopsla  = 2
      ELSEIF(ch .EQ.   'YMIN'  ) then
       iopsc1  = 2
      ELSEIF(ch .EQ.   'YMAX'  ) then
       iopsc2  = 2
      ENDIF
c encoding back
      iflag1   = 
     $ ioplog+10*iopsla+100*ioperb+1000*iopsc1+10000*iopsc2
      b(ist+3) = 9d12 + iflag1*10 +9d0
      end


      SUBROUTINE gbfun1(id,title,nchx,xmin,xmax,func)
c     ***********************************************
c ...fills histogram with function func(x)
c     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DIMENSION yy(200)
      EXTERNAL func
      CHARACTER*80 title
      LOGICAL gexist
c
      CALL ginit
      IF(gexist(id)) GOTO 900
 15   xl=xmin
      xu=xmax
      CALL gbook1(id,title,nchx,xl,xu)
c...slanted line in plotting
      CALL gidopt(id,'SLAN')
      IF(nchx .GT. 200) goto 901
      DO 20 ib=1,nchx
      x= xmin +(xmax-xmin)/nchx*(ib-0.5d0)
      yy(ib) = func(x)
   20 CONTINUE
      CALL gpak(id,yy)
      RETURN
 900  WRITE(nout,*) ' +++gbfun1: already exists id=',id
      WRITE(6   ,*) ' +++gbfun1: already exists id=',id      
      CALL gdelet(id)
      GO to 15
 901  WRITE(nout,*) ' +++gbfun1: to many bins'
      END

      SUBROUTINE gbfun2(id,title,nchx,xmin,xmax,func)
c     ***********************************************
c ...fills histogram with function func(x)
c.. three point fit used
c     ***********************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DIMENSION yy(200),yy1(0:200)
      EXTERNAL func
      CHARACTER*80 title
      LOGICAL gexist
!
      CALL ginit
      IF( gexist(id) ) GOTO 900
 15   xl=xmin
      xu=xmax
      CALL gbook1(id,title,nchx,xl,xu)

c...slanted line in plotting
      CALL gidopt(id,'SLAN')
      IF(nchx.gt.200) GOTO 901

      yy1(0) = func(xmin)
      dx=(xmax-xmin)/nchx

      DO ib=1,nchx
         x2= xmin +dx*(ib-0.5d0)
         x3= x2 +dx*0.5d0
         yy(ib)  = func(x2)
         yy1(ib) = func(x3)
c..  simpson 
         yy(ib) = ( yy1(ib-1) +4*yy (ib) +yy1(ib))/6d0
      ENDDO

      CALL gpak(id,yy)
      RETURN
 900  WRITE(nout,*) ' +++gbfun2: already exists id=',id
      WRITE(6   ,*) ' +++gbfun2: already exists id=',id      
      CALL gdelet(id)
      GO TO 15
 901  WRITE(nout,*) ' +++gbfun2: to many bins'
      END



      SUBROUTINE GBOOK2(ID,TITLE,NCHX,XL,XU,NCHY,YL,YU)
*     *************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( IDMX=400,NBUF=24,NBUF2=24)
      COMMON / Cglib / B(50000)
      COMMON /GIND/ NVRS,NOUT,LENMAX,LENGTH,INDEX(IDMX,3),TITLC(IDMX)
      CHARACTER*80 TITLC
      save /cglib/,/gind/
      CHARACTER*80 TITLE
      LOGICAL GEXIST
c
      CALL GINIT
      IF(GEXIST(ID)) GOTO 900
      ist=length
      LACT=JADRES(0)
      IF(LACT .EQ. 0) GOTO 901
      index(LACT,1)=ID
      index(LACT,2)=length
      CALL COPCH(TITLE,TITLC(LACT))
      nnchx=NCHX
      nnchy=NCHY
      LENGT2 = LENGTH  +44+nnchx*nnchy
      IF(LENGT2 .GE. LENMAX) GOTO 902
      DO 10 J=LENGTH+1,LENGT2+1
   10 B(J) = 0D0
      LENGTH=LENGT2
      B(ist+1)=nnchx
      B(ist+2)=XL
      B(ist+3)=XU
      B(ist+4)=float(nnchx)/(b(ist+3)-b(ist+2))
      B(ist+5)=nnchy
      B(ist+6)=YL
      B(ist+7)=YU
      B(ist+8)=float(nnchy)/(b(ist+7)-b(ist+6))
      RETURN
  900 WRITE(NOUT,*) ' GBOOK2: HISTO ALREADY EXISTS!!!! ID=',ID
      RETURN
  901 WRITE(NOUT,*) ' GBOOK2: TO MANY HISTOS !!!!!',LACT
      STOP
  902 WRITE(NOUT,*) ' GBOOK2: TO LITLE STORAGE!!!!',LENMAX
      STOP
      END

      subroutine gistdo
*     *****************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /gind/
      do 10 i=1,idmx
      id=index(i,1)
      IF(id .GT. 0) call gprint(id)
   10 continue
      end

      subroutine goutpu(ilun)
*     ***********************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /gind/
      call ginit
      nout=ilun
      end


      subroutine gprint(id)
*     *********************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
CC..M.S.>>
C      character*1 line(105),lchr(22),lb,lx,li,l0
      character*1 line(0:105),lchr(22),lb,lx,li,l0
CC..M.S.<<
      logical llg
      data lb,lx,li,l0 /' ','X','I','0'/
      data lchr/' ','1','2','3','4','5','6','7','8','9',
     $      'A','B','C','D','E','F','G','H','I','J','K','*'/
      save lb,lx,li,l0,lchr

      lact=jadres(id)
      if(lact.eq.0) goto 900
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11

      call goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      ker    =  ioperb-1
cc..m.s.
      lmx = 57
      lmx = 52
cc..m.s.
      if(ker.eq.1) lmx=54
      nent=index(lact,3)
      if(nent.eq.0) goto 901
      write(nout,1000) id,titlc(lact)
 1000 FORMAT('1',/,1X,I6,10X,A)
c
c one-dim. histo 
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      if(ityphi.ne.1) goto 200
      nchx =   b(ist2 +1)
      xl   =   b(ist2 +2)
      dx   =  (  b(ist2 +3)-b(ist2 +2)  )/float(nchx)
c fixing vertical scale
      istr=ist+nbuf+1
      bmax = b(istr)
      bmin = b(istr)
      do 15 ibn=istr,istr+nchx-1
      bmax = max(bmax,b(ibn))
      bmin = min(bmin,b(ibn))
  15  continue
      if(bmin.eq.bmax) goto 901
      if(iopsc1.eq.2) bmin=b(ist +5)
      if(iopsc2.eq.2) bmax=b(ist +6)
c
      llg=ioplog.eq.2
      if(llg.and.bmin.le.0d0) bmin=bmax/10000.d0
c
      deltb = bmax-bmin
      if(deltb.eq.0d0) goto 902
      fact  = (lmx-1)/deltb
      kzer  = -bmin*fact+1.00001d0
      if(llg) fact=(lmx-1)/(log(bmax)-log(bmin))
      if(llg) kzer=-log(bmin)*fact+1.00001d0
c
      undf = b(ist3 +1)
      ovef = b(ist3 +3)
      avex = 0d0
      sum  = b(ist3 +8)
      if(nent.ne.0) avex = sum/nent
      write(nout,'(4a15      )')  'nent','sum','bmin','bmax'
      write(nout,'(i15,3e15.5)')   nent,  sum,  bmin,  bmax
      write(nout,'(4a15  )')      'undf','ovef','avex'
      write(nout,'(4e15.5)')       undf,  ovef,  avex
c
      if(llg) write(nout,1105)
 1105 format(35x,17hlogarithmic scale)
c
      kzer=max0(kzer,0)
      kzer=min0(kzer,lmx)
      xlow=xl
      do 100 k=1,nchx
c first fill with blanks
CC..M.S.>>
C      do  45 j=1,105
      do  45 j=0,105
CC..M.S.<<
   45 line(j)  =lb
c then fill upper and lower boundry
      line(1)  =li
      line(lmx)=li
      ind=istr+k-1
      bind=b(ind)
      bind= max(bind,bmin)
      bind= min(bind,bmax)
      kros=(bind-bmin)*fact+1.0001d0
      if(llg) kros=log(bind/bmin)*fact+1.0001d0
      k2=max0(kros,kzer)
      k2=min0(lmx,max0(1,k2))
      k1=min0(kros,kzer)
      k1=min0(lmx,max0(1,k1))
      do 50 j=k1,k2
   50 line(j)=lx
      line(kzer)=l0
      z=b(ind)
      if(ker.ne.1) then 
cc..m.s.        write(nout,'(a, f7.4,  a, d12.4,  132a1)') 
        write(nout,'(a, d12.6,  a, d12.6,  132a1)') 
     $             ' ', xlow,' ',     z,' ',(line(i),i=1,lmx)
      else
        er=dsqrt(dabs(b(ind+nchx)))
cc..m.s.        write(nout,'(a,f7.4,  a,d12.4,  a,d12.4, 132a1 )') 
        write(nout,'(a,f8.4,  a,d14.7,  a,d9.2, 132a1 )') 
     $             ' ',xlow,' ',    z,' ',   er,' ',(line(i),i=1,lmx)
      endif
c      if(ker.ne.1) then
c        write(nout,'(a, f7.4,  a, d14.6,  132a1)') 
c     $             ' ', xlow,' ',     z,' ',(line(i),i=1,lmx)
c      else
c        er=dsqrt(dabs(b(ind+nchx)))
c        write(nout,'(a,f7.4,  a,d14.6,  a,d14.6, 132a1 )') 
c     $             ' ',xlow,' ',    z,' ',   er,' ',(line(i),i=1,lmx)
c      endif
      xlow=xlow+dx
  100 continue
      return
C------------- two dimensional requires complete restoration!!!----------------
  200 continue
      nchx=B(ist+1)
      nchy=B(ist+5)
      write(nout,2000) (lx,i=1,nchy)
 2000 format(1h ,10x,2hxx,100a1)
      do 300 kx=1,nchx
      do 250 ky=1,nchy
      k=ist +NBUF2 +kx+nchx*(ky-1)
      N=B(K)+1.99999D0
      n=max0(n,1)
      n=min0(n,22)
      if(DABS(b(k)).lt.1D-20) n=1
      line(ky)=lchr(n)
  250 continue
      line(nchy+1)=lx
      i1=nchy+1
      write(nout,2100) (line(i),i=1,i1)
 2100 format(1h ,10x,1hx,100a1)
  300 continue
      write(nout,2000) (lx,i=1,nchy)
      RETURN
  900 WRITE(NOUT,*) ' +++GPRINT: NONEXISTING HISTO',ID
      WRITE(6   ,*) ' +++GPRINT: NONEXISTING HISTO',ID
      RETURN
 901  WRITE(NOUT,*) ' +++GPRINT: NO ENTRIES  HISTO',ID
      WRITE(   6,*) ' +++GPRINT: NO ENTRIES  HISTO',ID
      RETURN
 902  WRITE(NOUT,*) ' +++GPRINT: wrong plotting limits',ID
      WRITE(   6,*) ' +++GPRINT: wrong plotting limits',ID
      END


      subroutine gopera(ida,chr,idb,idc,coef1,coef2)
*     **********************************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      character*80 title
      character*1  chr
c
      lacta=jadres(ida)
      IF(lacta .EQ. 0) return
      ista  = index(lacta,2)
      ista2 = ista+7
      ncha  = b(ista2+1)
c
      lactb =jadres(idb)
      IF(lactb .EQ. 0) return
      istb  = index(lactb,2)
      istb2 = istb+7
      nchb  = b(istb2+1)
      IF(nchb .NE. ncha) goto 900
c
      lactc=jadres(idc)
      IF(lactc .EQ. 0) then
c ...if nonexistent, histo idc is here defined
        call ginbo1(ida,title,nchx,xl,xu)
        call gbook1(idc,title,nchx,xl,xu)
        lactc = jadres(idc)
        istc  = index(lactc,2)
c...option copied from ida
        b(istc+ 3)= b(ista +3)
      ENDIF
c...one nominal entry recorded
      index(lactc,3) = 1
c
      istc  =  index(lactc,2)
      istc2 =  istc+7
      nchc  =  b(istc2+1)
c
      IF(nchc .NE. ncha) goto 900
      IF(ncha .NE. nchb.or.nchb .NE. nchc) goto 900
      do 30 k=1,ncha
      i1 = ista+nbuf+k
      i2 = istb+nbuf+k
      i3 = istc+nbuf+k
      j1 = ista+nbuf+ncha+k
      j2 = istb+nbuf+ncha+k
      j3 = istc+nbuf+ncha+k
      if    (chr .EQ. '+')   then
        b(i3) =    coef1*b(i1) +    coef2*b(i2)
        b(j3) = coef1**2*b(j1) + coef2**2*b(j2)
      ELSEIF(chr .EQ. '-')   then
        b(i3) = coef1*b(i1) - coef2*b(i2)
        b(j3) = coef1**2*b(j1) + coef2**2*b(j2)
      ELSEIF(chr .EQ. '*')   then
        b(j3) = (coef1*coef2)**2
     $          *(b(j1)*b(i2)**2 + b(j2)*b(i1)**2)
        b(i3) = coef1*b(i1) * coef2*b(i2)
      ELSEIF(chr .EQ. '/')   then
        IF(b(i2) .EQ. 0d0) then
          b(i3) = 0d0
          b(j3) = 0d0
        ELSE
          b(j3) = (coef1/coef2)**2/b(i2)**4
     $          *(b(j1)*b(i2)**2 + b(j2)*b(i1)**2)
          b(i3) = (coef1*b(i1) )/( coef2*b(i2))
        ENDIF
      ELSE
        goto 901
      ENDIF
   30 continue
      return
  900 write(nout,*) '+++++ gopera: non-equal no. bins ',ida,idb,idc
      write(   6,*) '+++++ gopera: non-equal no. bins ',ida,idb,idc
      return
  901 write(nout,*) '+++++ gopera: wrong chr=',chr
      END

      SUBROUTINE ginbo1(id,title,nchx,xl,xu)
!     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      CHARACTER*80 title
!
      lact=jadres(id)
      IF(lact .EQ. 0) THEN
         write(6,*) '+++++ STOP in ginbo1: wrong id=',id
         STOP
      ENDIF
      ist=index(lact,2)
      ist2   = ist+7
      nchx   = b(ist2 +1)
      xl     = b(ist2 +2)
      xu     = b(ist2 +3)
      title  = titlc(lact)
      END

      subroutine gunpak(id,a,chd1,idum)
*     *********************************
c getting out histogram content (and error)
c chd1= 'ERRO' is nonstandard option (unpack errors)
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      character*(*) chd1
      dimension a(*)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist   = index(lact,2)
      ist2  = ist+7
      nch   = b(ist2 +1)
      local = ist +nbuf
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 2) then
        nchy  = b(ist2+5)
        nch   = nch*nchy
        local = ist+ nbuf2
      ENDIF
      do 10 ib=1,nch
      IF(chd1 .NE. 'ERRO') then
c normal bin
        a(ib) = b(local+ib)
      ELSE
c error content
        IF(ityphi .EQ. 2) goto 901
        a(ib) = dsqrt( dabs(b(local+nch+ib) ))
      ENDIF
   10 continue
      return
 900  write(nout,*) '+++gunpak: nonexisting id=',id
      write(6   ,*) '+++gunpak: nonexisting id=',id
      return
 901  write(nout,*) '+++gunpak: no errors, two-dim, id=',id
      end

      subroutine gpak(id,a)
*     *********************
c getting in histogram content
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      dimension  a(*)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist  = index(lact,2)
      ist2 = ist+7
      nch=b(ist2 +1)
      local = ist+nbuf
c 2-dimens histo alowed
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 2) then
        nchy  = b(ist2+5)
        nch   = nch*nchy
        local = ist+nbuf2
      ENDIF
      do 10 ib=1,nch
   10 b(local +ib) = a(ib)
c one nominal entry recorded
      index(lact,3)  = 1
      return
  900 write(nout,*) '+++gpak: nonexisting id=',id
      write(6   ,*) '+++gpak: nonexisting id=',id
      end

      subroutine gpake(id,a)
*     **********************
c getting in error content
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      dimension  a(*)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) goto 901
      ist  = index(lact,2)
      ist2 = ist+7
      nch=b(ist2+1)
c 2-dimens histo NOT alowed
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 2) goto 900
      do 10 ib=1,nch
   10 b(ist+nbuf+nch+ib) = a(ib)**2
      return
  900 write(nout,*) ' +++++ gpake: only for one-dim histos'
      return
  901 write(nout,*) '+++ gpake: nonexisting id=',id
      write(6   ,*) '+++ gpake: nonexisting id=',id
      end


      subroutine grang1(id,ylr,yur)
*     *****************************
c provides y-scale for 1-dim plots
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) return
      ist  = index(lact,2)
      ist2 = ist+7
      nch  = b(ist2 +1)
      yl   = b(ist+nbuf+1)
      yu   = b(ist+nbuf+1)
      do 10 ib=1,nch
      yl = min(yl,b(ist+nbuf+ib))
      yu = max(yu,b(ist+nbuf+ib))
   10 continue
      call goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      IF(iopsc1 .EQ. 2) yl= b( ist +5)
      IF(iopsc2 .EQ. 2) yu= b( ist +6)
      ylr = yl
      yur = yu
      end


      subroutine ginbo2(id,nchx,xl,xu,nchy,yl,yu)
*     *******************************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist  = index(lact,2)
      ist2 = ist+7
      nchx = b(ist2 +1)
      xl   = b(ist2 +2)
      xu   = b(ist2 +3)
      nchy = b(ist2 +5)
      yl   = b(ist2 +6)
      yu   = b(ist2 +7)
      return
  900 write(nout,*) ' +++ginbo2: nonexisting histo id= ',id 
      write(   6,*) ' +++ginbo2: nonexisting histo id= ',id
      end


      subroutine gmaxim(id,wmax)
*     **************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      IF(id .NE. 0) then
        lact=jadres(id)
        IF(lact .EQ. 0) return
        ist= index(lact,2)
        b(ist+6) =wmax
        call gidopt(id,'YMAX')
      ELSE
        do 20 k=1,idmx
        IF(index(k,1) .EQ. 0) goto 20
        ist=index(k,2)
        jd =index(k,1)
        b(ist+6) =wmax
        call gidopt(jd,'YMAX')
   20   continue
      ENDIF
      end

      subroutine gminim(id,wmin)
*     **************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      IF(id .NE. 0) then
        lact=jadres(id)
        IF(lact .EQ. 0) return
        ist =index(lact,2)
        b(ist+5) =wmin
        call gidopt(id,'YMIN')
      ELSE
        do 20 k=1,idmx
        IF(index(k,1) .EQ. 0) goto 20
        ist=index(k,2)
        jd =index(k,1)
        b(ist+5) =wmin
        call gidopt(jd,'YMIN')
   20   continue
      ENDIF
      end

      subroutine greset(id,chd1)
*     **************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      character*(*) chd1
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .LE. 0) return
      ist  =index(lact,2)
      ist2 = ist+7
c 
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 1) then
c one-dim.
        ist3  = ist+11
        nchx  = b(ist2 +1)
        nch   = 2*nchx
        local = ist + nbuf
      ELSEIF(ityphi .EQ. 2) then
c two-dim.
        ist3  = ist+15
        nchx  = b(ist2 +1)
        nchy  = b(ist2 +5)
        nch   = nchx*nchy
        local = ist +nbuf2
      ELSE
         write(nout,*) '+++greset: wrong type id=',id
         write(6   ,*) '+++greset: wrong type id=',id
        return
      ENDIF
c reset miscaelaneous entries and bins
      do 10 j=ist3+1,local +nch
  10  b(j)    = 0d0
c and no. of entries in index
      index(lact,3) = 0
      end

      SUBROUTINE GDELET(ID1)
*     *********************
C Now it should work (stj Nov. 91) but watch out!
C should works for 2-dim histos, please check this!
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      logical gexist
c
      ID=ID1
      IF(id .EQ. 0) GOTO 300
      IF(.not.gexist(id)) GOTO 900
      lact = jadres(id)
      ist  = index(lact,2)
      ist2 = ist+7
*----
c[[[      WRITE(6,*) 'GDELET-ing ID= ',ID
      idec    = nint(b(ist+2)-9d0-9d12)/10
      IF(idec .NE. id) WRITE(6,*) '++++GDELET: ALARM! ID,IDEC= ',ID,IDEC
*----
      nch  = b(ist2 +1)
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 1) THEN
c one-dim.
        nchx  = b(ist2 +1)
        nch   = 2*nchx
c lenght of local histo to be removed
        local = nch+nbuf+1
      ELSEIF(ityphi .EQ. 2) THEN
c two-dim.
        nchx  = b(ist2 +1)
        nchy  = b(ist2 +5)
        nch   = nchx*nchy
c lenght of local histo to be removed
        local = nch+nbuf2+1
      ELSE
         write(nout,*) '+++gdelet: wrong type id=',id
         write(6   ,*) '+++gdelet: wrong type id=',id
        return
      ENDIF
c starting position of next histo in storage b
      next = ist+1 +local
c move down all histos above this one 
      DO 15 k =next,length
      b(k-local)=b(k)
   15 CONTINUE  
c define new end of storage
      length=length-local
c clean free space at the end of storage b
      DO 20 k=length+1, length+local
   20 b(k)=0d0 
c shift adresses of all displaced histos 
      DO 25 l=lact+1,idmx
      IF(index(l,1) .NE. 0) index(l,2)=index(l,2)-local
   25 CONTINUE
c move entries in index down by one and remove id=lact entry
      DO 30 l=lact+1,idmx
      index(l-1,1)=index(l,1)
      index(l-1,2)=index(l,2)
      index(l-1,3)=index(l,3)
      titlc(l-1)=titlc(l)
   30 CONTINUE
c last entry should be always empty
      index(idmx,1)=0
      index(idmx,2)=0
      index(idmx,3)=0 
      do 50 k=1,80
   50 titlc(idmx)(k:k)=' '
      RETURN
C -----------------------------------
C Deleting all histos at once!!!
  300 length=0
      DO 400 i=1,idmx
      DO 340 k=1,3
  340 index(i,k)=0
      DO 350 k=1,80
  350 titlc(i)(k:k)=' '
 400  CONTINUE
      RETURN
C -----------------------------------
 900  CONTINUE
      WRITE(nout,*) ' +++GDELET: nonexisting histo id= ',id 
      WRITE(   6,*) ' +++GDELET: nonexisting histo id= ',id 
      END


      subroutine glimit(lenmx)
*     ************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      call ginit
      IF(lenmx .GE. lenmax) then
         lenmax=lenmx
      ELSE
         call gstop1('glimit: cant decrease storage lenmx  =',lenmx)
      ENDIF
      end

      subroutine copch(ch1,ch2)
*     *************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
* copies character*80 ch1 into ch2 up to a first $ sign
      character*80 ch1,ch2
      logical met
      met = .false.
      do 10 i=1,80
      IF( ch1(i:i) .EQ. '$' .or. met )   then
        ch2(i:i)=' '
        met=.true.
      ELSE
        ch2(i:i)=ch1(i:i)
      ENDIF
  10  continue
      end

      FUNCTION jadre2(id)
*     *********************
*------------------------------------------------
* Good old version -- but it is very very slow!!!
* In the case of 100 histograms or more.
*------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      jadre2=0
      DO 1 i=1,idmx
      IF(index(i,1) .EQ. id) goto 2
    1 CONTINUE
* Nothing found.
      RETURN
* Found: id=0 is also legitimate find!!!
    2 jadre2=i
      END

      FUNCTION jadres(id1)
*     *********************
*--------------------------------------------------------------------
* Educated guess based on past history is used to find quickly
* location of the histogram in the matrix index.
* This is based on observation that subsequent histogram calls 
* are linked into loops (so one can predict easily which histo will
* be called next time).
*--------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DATA iguess,jdlast,idlast /-2141593,-3141593,-3141593/
      SAVE iguess,jdlast,idlast

      id=id1
! --- The case of ID=0 treated separately, it is used to find out
! --- last entry in the index (it is marked with zero)
      IF(id .EQ. 0) THEN
         DO i=1,idmx
            IF(index(i,1) .EQ. 0) goto 4
         ENDDO
         WRITE(6,*) '+++++jadres: STOP index to short'
         STOP
 4       CONTINUE
         jadres = i
         RETURN
      ENDIF

! --- Omit sophistications if lack of initialization
      IF(jdlast .EQ. -3141593) GOTO 10
      IF(iguess .EQ. -2141593) GOTO 10
      IF(iguess .EQ. 0) GOTO 10
      IF(jdlast .EQ. 0) GOTO 10

! --- Try first previous histo (for repeated calls)
      IF(jdlast .LT. 1 .OR. jdlast .GT. idmx) THEN
         WRITE(6,*) '+++++ jadres: jdlast=',jdlast
      ENDIF
      IF(index(jdlast,1) .EQ. id) THEN
         jadres = jdlast
c##   write(6,*) 
c##   $   'found, guess based on previous call to jadres ',jdlast
         GOTO 20
      ENDIF

! --- Try current guess based on previous call
      IF(iguess .LT. 1 .OR. iguess .GT. idmx)  THEN
         WRITE(6,*)'+++++ jadres: iguess=',iguess
      ENDIF
      IF(index(iguess,1) .EQ. id) THEN
         jadres = iguess
c##   write(6,*) 
c##   $   'found, guess on previous calls recorded in b(ist+7)',jdlast
         GOTO 20
      ENDIF

! ================================================
!    Do it HARD WAY, Search all matrix index
! ================================================
 10   CONTINUE
c##   write(6,*) 'trying HARD WAY'
      DO i=1,idmx
         jadres=i
         IF(index(i,1) .EQ. id) GOTO 20
      ENDDO
! -------------------------------------
!     Nothing found: jadres=0
! -------------------------------------
      jadres=0
      RETURN
! =====================================
!     Found: Set new guess for next call
! =====================================
 20   CONTINUE
! --- and store result as a new guess in previous histo 
! --- but only if it existed!!!!
      DO i=1,idmx
         IF(index(i,1) .EQ. 0) GOTO 40
         IF(index(i,1) .EQ. idlast) THEN
            ist=index(i,2)
            IF(ist .GT. 0 .AND. ist .LT. 50000) b(ist +7) = jadres
c##   write(6,*) 'STORED     id=',id
            GOTO 40
         ENDIF 
      ENDDO
 40   CONTINUE
c##   write(6,*)  'found, hard way searching all of index)', jdlast
      iguess = b( index(jadres,2) +7)
      jdlast = jadres
      idlast = id
      END


C--------------------------------------------------------------
C ----------- storing histograms in the disk file -------------
C--------------------------------------------------------------
      subroutine grfile(nhruni,dname,chd2)
c     ***********************************
      implicit double precision (a-h,o-z)
      character*(*) chd2, dname
      common / hruni / nhist
      save /hruni/
      nhist=nhruni
      end

      subroutine grout(idum1,idum2,chdum)
c     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      character*8 chdum
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      common / hruni / nhist
      character*80 titlc
      save /cglib/,/gind/, /hruni/
c
      call ginit
      nouth=nhist
      write(nouth,'(6i10)')   nvrs,nout,lenmax,length
      write(nouth,'(6i10)')   ((index(i,k),k=1,3),i=1,idmx)
      write(nouth,'(a80)')    titlc
      write(nouth,'(3d24.16)') (b(i),i=1,length)
      end


      SUBROUTINE GRIN(IDUM1,IDUM2,IDUM3)
!     **********************************
! New version which has a possibility to 
!            MERGE histograms
! If given ID already exists then it is modified by adding 1000000 !!!!
! Mergigng is done simply by appending new histograms at the 
! very end of the index and bin matrices.
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      COMMON / hruni / nhist
      SAVE /cglib/,/gind/, /hruni/
! Copy of the new index from the disk
      DIMENSION lndex(idmx,3),titld(idmx)
      CHARACTER*80 titld
      LOGICAL gexist

      CALL ginit 
      nouth=nhist
! Read basic params
      READ(nouth,'(6i10)')   nvrs3,nout3,lenma3,lengt3
      IF(length+lengt3 .GE. lenmax) GOTO 900
! Check version
      IF(nvrs .NE. nvrs3) WRITE(nout,*)
     $ '  +++++ warning (grin): histos produced by older version',nvrs3
      IF(nvrs .NE. nvrs3) WRITE(6,*)
     $ '  +++++ warning (grin): histos produced by older version',nvrs3
! Read new index  from the disk
      READ(nouth,'(6i10)')  ((lndex(i,k),k=1,3),i=1,idmx)
      READ(nouth,'(a80)')    titld

      lenold=length
! Append content of new histos AT ONCE  at the end of storage b
      length=length+lengt3
      READ(nouth,'(3d24.16)') (b(i),i=lenold+1,length)

! Append index and titlc with new histos one by one
      lact = jadres(0)
      DO 100 l=1,idmx
      IF(lact .EQ. 0) GOTO 901
      idn= lndex(l,1)
      IF(idn .EQ. 0) GOTO 100
! Identical id's are changed by adding big number = 1000000
 10   CONTINUE
      IF( gexist(idn) ) THEN
         idn = idn +1000000*(idn/iabs(idn))
         GOTO 10 
      ENDIF
      index(lact,1)=idn
      index(lact,2)=lndex(l,2)+lenold
      index(lact,3)=lndex(l,3)
      titlc(lact)  =titld(l)
!
! Still one small correction in the newly appended histo
      istn  = index(lact,2)
      b(istn +2)  = 9d12 +     idn*10 +9d0
!
      lact=lact+1
  100 CONTINUE

!
      RETURN

 900  CONTINUE
      CALL gstop1('++++ grin: to litle space, lenmax=  ',lenmax)
 901  CONTINUE
      CALL gstop1('++++ grin: to many histos, idmx=    ',idmx)
      END




      SUBROUTINE GRIN2(IDUM1,IDUM2,IDUM3)
!     **********************************
! New version which has a possibility to 
!            ADD histograms
! If ID is not existing already then no action is taken
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      COMMON / hruni / nhist
      SAVE /cglib/,/gind/, /hruni/
! Copy of the histos from the disk
      DIMENSION bz(50000)
      DIMENSION indez(idmx,3),titlz(idmx)
      CHARACTER*80 titlz
      LOGICAL gexist

      CALL ginit 
      nouth=nhist
! Read basic params
      READ(nouth,'(6i10)')   nvrsz,noutz,lenmaz,lengtz
! Check version
      IF(nvrs .NE. nvrsz) WRITE(nout,*)
     $ '  +++++ warning (grin2): histos produced by older version',nvrsz
      IF(nvrs .NE. nvrsz) WRITE(6,*)
     $ '  +++++ warning (grin2): histos produced by older version',nvrsz
! Read new index, title and bins from the disk
      READ(nouth,'(6i10)')    ((indez(i,k),k=1,3),i=1,idmx)
      READ(nouth,'(a80)')     titlz
      READ(nouth,'(3d24.16)') (bz(i),i=1,lengtz)

! Add new histos from disk to existing ones one by one
      DO 100 lz=1,idmx
      id= indez(lz,1)
      IF(id .EQ. 0) GOTO 200
      IF(.not.gexist(id)) THEN
        write(6,*) ' Grin2: unmached histo ID=', id, '  Skipped'
        goto 100
      ENDIF
! parameters of existing histo
      lact = jadres(id)
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
      nchx = b(ist2 +1)
! parameters of the histo from the disk
      istz   = indez(lz,2)
      ist2z  = istz+7
      ist3z  = istz+11
      nchxz  = bz(ist2z +1)
      IF(nchx .NE. nchxz) THEN
        write(6,*) ' Grin2: non-equal binning ID=', id, '  Skipped' 
        goto 100
      ENDIF
! Add/Merge all additive entries of the two histos
! No of entries in index
      index(lact,3) = index(lact,3)+indez(lact,3)
! Overflows, underflows etc.
      DO i=1,12
        b(ist3+i)=b(ist3+i) +bz(ist3z+i)
      ENDDO
! Except of this one non-additive entry 
      b(ist3+13)=max(b(ist3+13),b(ist3z+13))
! Regular bin content added now!
      DO i= 1, 2*nchx
        b(ist+nbuf+i)=b(ist+nbuf+i) +bz(istz+nbuf+i)
      ENDDO
  100 CONTINUE
  200 CONTINUE

      END




      subroutine grend(chdum)
c     ***********************
      implicit double precision (a-h,o-z)
      common / hruni / nhist
      save   /hruni/
      character*(*) chdum
      close(nhist)
c======================================================================
c======================end of gbook====================================
c======================================================================
      end

C======================================================================
C======================Mini-GPLOT======================================
C======================================================================
C... Plotting using LATeX
      SUBROUTINE GPLINT(IDUM)
C     ***********************
C ...dummy routine
      END
      SUBROUTINE GPLCAP(IFILE)
C     ***********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / LPLTIT / TITCH,KEYTIT
      CHARACTER*80 TITCH
C Note that backslash definition is varying from one 
C instalation/compiler to another, you have to figure out by yourself 
C how to fill backslash code into BS
      COMMON / BSLASH / BS
      CHARACTER*1 BS,BBS
      save /LPLDAT/, /LPLTIT/, /BSLASH/
C     DATA BBS / 1H\ /
      DATA BBS / '\\' /
      BS = BBS
cc      BS = '\\'
C---
      KEYTIT= 0
      ILINE = 1
      NOUH1=IABS(IFILE)
      NOUH2=NOUH1+1
      WRITE(NOUH1,'(A,A)') BS,'voffset =  1.0cm'
      WRITE(NOUH1,'(A,A)') BS,'hoffset = -1cm'
      WRITE(NOUH1,'(A,A)') BS,'documentstyle[12pt]{article}'
      WRITE(NOUH1,'(A,A)') BS,'textwidth  = 16cm'
      WRITE(NOUH1,'(A,A)') BS,'textheight = 18cm'
      WRITE(NOUH1,'(A,A)') BS,'begin{document}'
      WRITE(NOUH1,'(A)') '  '
      WRITE(NOUH1,'(A)') '  '
      END

      SUBROUTINE GPLEND
C     *****************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      save /LPLDAT/, /BSLASH/
      CHARACTER*1 BS
      WRITE(NOUH1,'(2A)') BS,'end{document}'
      CLOSE(NOUH1)
      END

      SUBROUTINE GPLOT(ID,CH1,CH2,KDUM)
C     *********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(200),YER(200)
      CHARACTER CH1,CH2,CHR
      CHARACTER*80 TITLE
      LOGICAL GEXIST
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/
      DATA CHR /' '/
C return if histo non-existing
      IF(.NOT.GEXIST(ID)) GOTO 900
C ...unpack histogram
      CALL GUNPAK(ID,YY ,'    ',IDUM)
      CALL GUNPAK(ID,YER,'ERRO',IDUM)
      CALL GINBO1(ID,TITLE,NCHX,DXL,DXU)
      XL = DXL
      XU = DXU
      CALL GRANG1(ID,YL,YU)
      KAX=1200
      KAY=1200
      IF(CH1 .EQ. 'S') THEN
C ...superimpose plot
        BACKSPACE(NOUH1)
        BACKSPACE(NOUH1)
      ELSE
C ...new frame only
        CHR=CH1
        CALL LFRAM1(ID,KAX,KAY)
      ENDIF
      WRITE(NOUH1,'(A)')    '%========== next plot (line) =========='
      WRITE(NOUH1,'(A,I6)') '%==== HISTOGRAM ID=',ID
      WRITE(NOUH1,'(A,A70 )') '% ',TITLE
C...cont. line for functions
      call goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      ker = ioperb-1
      IF (iopsla .EQ. 2)  CHR='C'
C...suppress GPLOT assignments
      IF (CH2 .EQ. 'B')   CHR=' '
      IF (CH2 .EQ. '*')   CHR='*'
      IF (CH2 .EQ. 'C')   CHR='C'
C...various types of lines
      IF     (CHR .EQ. ' ') THEN
C...contour line used for histogram
          CALL PLHIST(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
      ELSE IF(CHR .EQ. '*') THEN
C...marks in the midle of the bin
          CALL PLHIS2(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
      ELSE IF(CHR .EQ. 'C') THEN
C...slanted (dotted) line in plotting non-MC functions
          CALL PLCIRC(KAX,KAY,NCHX,YL,YU,YY)
      ENDIF
!------------------------------!
! Ending
!------------------------------!
      WRITE(NOUH1,'(2A)') BS,'end{picture} % close entire picture '
      WRITE(NOUH1,'(2A)') BS,'end{figure}'

      RETURN
  900 WRITE(*,*) ' ++++ GPLOT: NONEXISTIG HISTO ' ,ID
      END

      SUBROUTINE LFRAM1(ID,KAX,KAY)
C     *****************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*80 TITLE
      COMMON / LPLTIT / TITCH,KEYTIT
      CHARACTER*80 TITCH
      DIMENSION TIPSY(20),TIPSX(20)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      DOUBLE PRECISION DXL,DXU
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /LPLTIT/, /BSLASH/
      DATA ICONT/0/

      ICONT=ICONT+1
      CALL GINBO1(ID,TITLE,NCHX,DXL,DXU)
      XL = DXL
      XU = DXU
      CALL GRANG1(ID,YL,YU)

      IF(ICONT .GT. 1) WRITE(NOUH1,'(2A)') BS,'newpage'
!------------------------------!
!           Header
!------------------------------!
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     $%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE(NOUH1,'(A)') '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     $%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE(NOUH1,'(2A)') BS,'begin{figure}[!ht]'
      WRITE(NOUH1,'(2A)') BS,'centering'
!------------------------------!
! General Caption
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'caption{',BS,'small'
      IF(KEYTIT.EQ.0) THEN
        WRITE(NOUH1,'(A)')     TITLE
      ELSE
        WRITE(NOUH1,'(A)')     TITCH
      ENDIF
      WRITE(NOUH1,'(A)') '}'
!------------------------------!
! Frames and labels
!------------------------------!
      WRITE(NOUH1,'(A)') '% =========== big frame, title etc. ======='
      WRITE(NOUH1,'(4A)') BS,'setlength{',BS,'unitlength}{0.1mm}'
      WRITE(NOUH1,'(2A)') BS,'begin{picture}(1600,1500)'
      WRITE(NOUH1,'(4A)') BS,'put(0,0){',BS,'framebox(1600,1500){ }}'
      WRITE(NOUH1,'(A)') '% =========== small frame, labeled axis ==='
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $    BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $    BS,'put(0,0){',BS,'framebox( ',KAX,',',KAY,'){ }}'
      WRITE(NOUH1,'(A)') '% =========== x and y axis ================'
      CALL SAXIX(KAX,XL,XU,NTIPX,TIPSX)
      CALL SAXIY(KAY,YL,YU,NTIPY,TIPSY)
      WRITE(NOUH1,'(3A)') BS,'end{picture}}'
     $                ,'% end of plotting labeled axis'
      END

      SUBROUTINE SAXIX(KAY,YL,YU,NLT,TIPSY)
C     ***************************************
C plotting x-axis with long and short tips
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TIPSY(20)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/

      DY= ABS(YU-YL)
      LY = NINT( LOG10(DY) -0.4999999d0 )
      JY = NINT(DY/10d0**LY)
      DDYL = DY*10d0**(-LY)
      IF( JY .EQ. 1)             DDYL = 10d0**LY*0.25d0
      IF( JY .GE. 2.AND.JY .LE. 3) DDYL = 10d0**LY*0.5d0
      IF( JY .GE. 4.AND.JY .LE. 6) DDYL = 10d0**LY*1.0d0
      IF( JY .GE. 7)             DDYL = 10d0**LY*2.0d0
      WRITE(NOUH1,'(A)') '% .......SAXIX........ '
      WRITE(NOUH1,'(A,I4)') '%  JY= ',JY
C-------
      NLT = INT(DY/DDYL)
      NLT = MAX0(MIN0(NLT,20),1)+1
      YY0L = NINT(YL/DDYL+0.5d0)*DDYL
      DDYS = DDYL/10d0
      YY0S = NINT(YL/DDYS+0.4999999d0)*DDYS
      P0L = KAY*(YY0L-YL)/(YU-YL)
      PDL = KAY*DDYL/(YU-YL)
      P0S = KAY*(YY0S-YL)/(YU-YL)
      PDS = KAY*DDYS/(YU-YL)
      NLT = INT(ABS(YU-YY0L)/DDYL+0.0000001d0)+1
      NTS = INT(ABS(YU-YY0S)/DDYS+0.0000001d0)+1
      DO 41 N=1,NLT
      TIPSY(N) =YY0L+ DDYL*(N-1)
  41  CONTINUE
      WRITE(NOUH1,1000)
     $ BS,'multiput('  ,P0L,  ',0)('  ,PDL,  ',0){'  ,NLT,  '}{',
     $ BS,'line(0,1){25}}',
     $ BS,'multiput('  ,P0S,  ',0)('  ,PDS,  ',0){'  ,NTS,  '}{',
     $ BS,'line(0,1){10}}'
      WRITE(NOUH1,1001)
     $ BS,'multiput('  ,P0L,  ','  ,KAY,  ')('  ,PDL,  ',0){'  ,NLT,
     $ '}{'  ,BS,  'line(0,-1){25}}',
     $ BS,'multiput('  ,P0S,  ','  ,KAY,  ')('  ,PDS,  ',0){'  ,NTS,
     $ '}{'  ,BS,  'line(0,-1){10}}'
 1000 FORMAT(2A,F8.2,A,F8.2,A,I4,3A)
 1001 FORMAT(2A,F8.2,A,I4,A,F8.2,A,I4,3A)
C ...labeling of axis
      SCMX = DMAX1(DABS(YL),DABS(YU))
      LEX  = NINT( LOG10(SCMX) -0.50001)
      DO 45 N=1,NLT
      K = NINT(KAY*(TIPSY(N)-YL)/(YU-YL))
      IF(LEX .LT. 2.AND.LEX .GT. -1) THEN
C ...without exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,A)')
     $ BS,'put(',K,',-25){',BS,'makebox(0,0)[t]{',BS,'large $ ',
     $ TIPSY(N), ' $}}'
      ELSE
C ...with exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,2A,I4,A)')
     $ BS,'put('  ,K,  ',-25){',BS,'makebox(0,0)[t]{',BS,'large $ ',
     $ TIPSY(N)/(10d0**LEX),BS,'cdot 10^{',LEX,'} $}}'
      ENDIF
  45  CONTINUE
      END

      SUBROUTINE SAXIY(KAY,YL,YU,NLT,TIPSY)
C     ***************************************
C plotting y-axis with long and short tips
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TIPSY(20)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/

      DY= ABS(YU-YL)
      LY = NINT( LOG10(DY) -0.49999999d0 )
      JY = NINT(DY/10d0**LY)
      DDYL = DY*10d0**(-LY)
      IF( JY .EQ. 1)             DDYL = 10d0**LY*0.25d0
      IF( JY .GE. 2.AND.JY .LE. 3) DDYL = 10d0**LY*0.5d0
      IF( JY .GE. 4.AND.JY .LE. 6) DDYL = 10d0**LY*1.0d0
      IF( JY .GE. 7)             DDYL = 10d0**LY*2.0d0
      WRITE(NOUH1,'(A)') '% .......SAXIY........ '
      WRITE(NOUH1,'(A,I4)') '%  JY= ',JY
C-------
      NLT = INT(DY/DDYL)
      NLT = MAX0(MIN0(NLT,20),1)+1
      YY0L = NINT(YL/DDYL+0.4999999d0)*DDYL
      DDYS = DDYL/10d0
      YY0S = NINT(YL/DDYS+0.5d0)*DDYS
      P0L = KAY*(YY0L-YL)/(YU-YL)
      PDL = KAY*DDYL/(YU-YL)
      P0S = KAY*(YY0S-YL)/(YU-YL)
      PDS = KAY*DDYS/(YU-YL)
      NLT= INT(ABS(YU-YY0L)/DDYL+0.0000001d0) +1
      NTS= INT(ABS(YU-YY0S)/DDYS+0.0000001d0) +1
      DO 41 N=1,NLT
      TIPSY(N) =YY0L+ DDYL*(N-1)
  41  CONTINUE
C plotting tics on vertical axis
      WRITE(NOUH1,1000)
     $ BS,'multiput(0,'  ,P0L,  ')(0,'  ,PDL  ,'){'  ,NLT,  '}{',
     $ BS,'line(1,0){25}}',
     $ BS,'multiput(0,'  ,P0S,  ')(0,'  ,PDS,  '){'  ,NTS,  '}{',
     $ BS,'line(1,0){10}}'
      WRITE(NOUH1,1001)
     $ BS,'multiput('  ,KAY,  ','  ,P0L,  ')(0,'  ,PDL,  '){'  ,NLT,
     $ '}{',BS,'line(-1,0){25}}',
     $ BS,'multiput('  ,KAY,  ','  ,P0S,  ')(0,'  ,PDS,  '){'  ,NTS,
     $ '}{',BS,'line(-1,0){10}}'
 1000 FORMAT(2A,F8.2,A,F8.2,A,I4,3A)
 1001 FORMAT(2A,I4,A,F8.2,A,F8.2,A,I4,3A)
C ...Zero line if necessary
      Z0L = KAY*(-YL)/(YU-YL)
      IF(Z0L .GT. 0D0.AND.Z0L .LT. FLOAT(KAY))
     $      WRITE(NOUH1,'(2A,F8.2,3A,I4,A)')
     $       BS,'put(0,'  ,Z0L,  '){',BS,'line(1,0){'  ,KAY,  '}}'
C ...labeling of axis
      SCMX = DMAX1(DABS(YL),DABS(YU))
      LEX  = NINT( LOG10(SCMX) -0.50001d0)
      DO 45 N=1,NLT
      K = NINT(KAY*(TIPSY(N)-YL)/(YU-YL))
      IF(LEX .LT. 2.AND.LEX .GT. -1) THEN
C ...without exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,A)')
     $  BS,'put(-25,'  ,K,  '){',BS,'makebox(0,0)[r]{',
     $  BS,'large $ '  ,TIPSY(N),  ' $}}'
      ELSE
C ...with exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,2A,I4,A)')
     $ BS,'put(-25,'  ,K,  '){',BS,'makebox(0,0)[r]{',
     $ BS,'large $ '
     $ ,TIPSY(N)/(10d0**LEX),  BS,'cdot 10^{'  ,LEX,  '} $}}'
      ENDIF
  45  CONTINUE
      END
      SUBROUTINE PLHIST(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
C     ************************************************
C plotting contour line for histogram
C     ***********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),YER(*)
      CHARACTER*80 FMT1
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $  BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
C...various types of line
      IF(ILINE .EQ. 1) THEN
         WRITE(NOUH1,'(2A)') BS,'thicklines '
      ELSE
         WRITE(NOUH1,'(2A)') BS,'thinlines '
      ENDIF
C...short macros for vertical/horizontal straight lines
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'x}[3]{',BS,'put(#1,#2){',
     $ BS,'line(1,0){#3}}}'
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'y}[3]{',BS,'put(#1,#2){',
     $ BS,'line(0,1){#3}}}'
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'z}[3]{',BS,'put(#1,#2){',
     $ BS,'line(0,-1){#3}}}'
C   error bars
      WRITE(NOUH1,'(8A)')
     $   BS,'newcommand{',BS,'e}[3]{',
     $   BS,'put(#1,#2){',BS,'line(0,1){#3}}}'
      IX0=0
      IY0=0
      DO 100 IB=1,NCHX
      IX1 = NINT(KAX*(IB-0.00001)/NCHX)
      IY1 = NINT(KAY*(YY(IB)-YL)/(YU-YL))
      IDY = IY1-IY0
      IDX = IX1-IX0
      FMT1 = '(2(2A,I4,A,I4,A,I4,A))'
      IF( IDY .GE. 0) THEN  
         IF(IY1 .GE. 0.AND.IY1 .LE. KAY)
     $   WRITE(NOUH1,FMT1) BS,'y{',IX0,'}{',IY0,'}{',IDY,'}',
     $                     BS,'x{',IX0,'}{',IY1,'}{',IDX,'}'
      ELSE
         IF(IY1 .GE. 0.AND.IY1 .LE. KAY)
     $   WRITE(NOUH1,FMT1) BS,'z{',IX0,'}{',IY0,'}{',-IDY,'}',
     $                     BS,'x{',IX0,'}{',IY1,'}{',IDX,'}'
      ENDIF
      IX0=IX1
      IY0=IY1
      IF(KER .EQ. 1) THEN
        IX2  = NINT(KAX*(IB-0.5000d0)/NCHX)
        IERR = NINT(KAY*((YY(IB)-YER(IB))-YL)/(YU-YL))
        IE = NINT(KAY*YER(IB)/(YU-YL))
        IF(IY1 .GE. 0.AND.IY1 .LE. KAY.and.abs(ierr) .LE. 9999
     $     .and.2*ie .LE. 9999) WRITE(NOUH1,8000) BS,IX2,IERR,IE*2
      ENDIF
 100  CONTINUE
8000  FORMAT(4(A1,2He{,I4,2H}{,I5,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(3A)') BS,'end{picture}}',
     $       ' % end of plotting histogram'
C change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 2) ILINE=1
      END
      SUBROUTINE PLHIS2(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
C     ************************************************
C marks in the midle of the bin
C     **********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),YER(*)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/

      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $ BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
C...various types of mark
      IRAD1= 6
      IRAD2=10
      IF(ILINE .EQ. 1) THEN
C   small filled circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 2) THEN
C   small open circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 3) THEN
C   big filled circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD2,'}}}'
      ELSEIF(ILINE .EQ. 4) THEN
C   big open circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD2,'}}}'
C Other symbols
      ELSEIF(ILINE .EQ. 5) THEN
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'diamond$}}}'
      ELSE
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'star$}}}'
      ENDIF
C   error bars
      WRITE(NOUH1,'(8A)')
     $   BS,'newcommand{',BS,'E}[3]{',
     $   BS,'put(#1,#2){',BS,'line(0,1){#3}}}'
      DO 100 IB=1,NCHX
      IX1 = NINT(KAX*(IB-0.5000d0)/NCHX)
      IY1 = NINT(KAY*(YY(IB)-YL)/(YU-YL))
      IF(IY1 .GE. 0.AND.IY1 .LE. KAY) WRITE(NOUH1,7000) BS,IX1,IY1
      IF(KER .EQ. 1) THEN
        IERR = NINT(KAY*((YY(IB)-YER(IB))-YL)/(YU-YL))
        IE   = NINT(KAY*YER(IB)/(YU-YL))
        IF(IY1 .GE. 0.AND.IY1 .LE. KAY.and.abs(ierr) .LE. 9999
     $       .and.2*ie .LE. 9999) WRITE(NOUH1,8000) BS,IX1,IERR,IE*2
      ENDIF
 100  CONTINUE
7000  FORMAT(4(A1,2HR{,I4,2H}{,I4,1H}:1X ))
8000  FORMAT(4(A1,2HE{,I4,2H}{,I5,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(3A)') BS,'end{picture}}',
     $    ' % end of plotting histogram'
C change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 6) ILINE=1
      END
      SUBROUTINE PLCIRC(KAX,KAY,NCHX,YL,YU,YY)
C     ****************************************
C plots equidistant points, four-point interpolation,
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),IX(3000),IY(3000)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/
      SAVE DS

C ...various types of line
C ...distance between points is DS, radius of a point is IRAD
      IRAD2=6
      IRAD1=3
C .............
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $  BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
      IF(ILINE .EQ. 1) THEN
C   small filled circle
       DS = 10
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 2) THEN
C   small open circle
       DS = 10
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 3) THEN
C   big filled circle
       DS = 20
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD2,'}}}'
      ELSEIF(ILINE .EQ. 4) THEN
C   big open circle
       DS = 20
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD2,'}}}'
C Other symbols
      ELSEIF(ILINE .EQ. 5) THEN
       DS = 20
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'diamond$}}}'
      ELSE
       DS = 20
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'star$}}}'
      ENDIF
      FACY = KAY/(YU-YL)
C plot first point
      AI  = 0.
      AJ  = (APROF( (AI/KAX)*NCHX+0.5d0, NCHX, YY) -YL)*FACY
      IPNT =1
      IX(IPNT) = INT(AI)
      IY(IPNT) = INT(AJ)
      DX =  DS
      AI0 = AI
      AJ0 = AJ
C plot next points
      DO 100 IPOIN=2,3000
C iteration to get (approximately) equal distance among ploted points
      DO  50 ITER=1,3
      AI  = AI0+DX
      AJ  = (APROF( (AI/KAX)*NCHX+0.5d0, NCHX, YY) -YL)*FACY
      DX  = DX *DS/SQRT(DX**2 + (AJ-AJ0)**2)
  50  CONTINUE
      IF(INT(AJ) .GE. 0.AND.INT(AJ) .LE. KAY.AND.INT(AI) .LE. KAX) THEN
         IPNT = IPNT+1
         IX(IPNT) = INT(AI)
         IY(IPNT) = INT(AJ)
      ENDIF
      AI0 = AI
      AJ0 = AJ
      IF(INT(AI) .GT. KAX) GOTO 101
 100  CONTINUE
 101  CONTINUE
      WRITE(NOUH1,7000) (BS,IX(I),IY(I), I=1,IPNT)
7000  FORMAT(4(A1,2HR{,I4,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(2A)') BS,'end{picture}} % end of plotting line'
C change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 2) ILINE=1
      END
      FUNCTION APROF(PX,NCH,YY)
C     *************************
C PX is a continuous extension of the index in array YY
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*)
      X=PX
      IF(X .LT. 0.0.OR.X .GT. FLOAT(NCH+1)) THEN
        APROF= -1E-20
        RETURN
      ENDIF
      IP=INT(X)
      IF(IP .LT. 2)     IP=2
      IF(IP .GT. NCH-2) IP=NCH-2
      P=X-IP
      APROF = -(1./6.)*P*(P-1)*(P-2)  *YY(IP-1)
     $        +(1./2.)*(P*P-1)*(P-2)  *YY(IP  )
     $        -(1./2.)*P*(P+1)*(P-2)  *YY(IP+1)
     $        +(1./6.)*P*(P*P-1)      *YY(IP+2)
      END
      SUBROUTINE GPLSET(CH,XX)
*     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      save / LPLDAT /
      CHARACTER*4 CH
      KTY=NINT(XX)
      IF(CH .EQ. 'DMOD') THEN
        ILINE=KTY
      ENDIF
      END
      SUBROUTINE GPLTIT(TITLE)
*     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*80 TITLE
      COMMON / LPLTIT / TITCH,KEYTIT
      CHARACTER*80 TITCH
      save / LPLTIT /
      KEYTIT=1
      DO 50 K=1,80
   50 TITCH(K:K)=' '
      CALL COPCH(TITLE,TITCH)
      END



      SUBROUTINE gpltab(Npl,idl,capt,fmt,nch1,incr,npag)
C     ******************************************************
! Tables in TeX, up to 5 columns
C     ******************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE 
!------- parameters
      DIMENSION    idl(5)
      CHARACTER*16 capt(6)
      CHARACTER*8  fmt(3)
!======= commons of glibk
      COMMON / LPLDAT / nouh1,nouh2,iline
      COMMON / LPLTIT / titch,keytit
      CHARACTER*80 titch
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/
!======= other
      LOGICAL gexist
      DIMENSION yyy(200),yer(200),bi(200,5),er(200,5)
      CHARACTER*80 title
      CHARACTER*1 Cn(5)
      DATA Cn /'1','2','3','4','5'/
!----------

! return if histo non-existing or to many columns
      IF(.NOT.GEXIST(ID)) GOTO 900
      IF(Npl .GT. 5 )     GOTO 901
!
! npack histograms
      CALL ginbo1( idl(1),title,nchx,dxl,dxu)
      xl = dxl
      xu = dxu
      DO n=1,Npl
        CALL gunpak( idl(n),yyy ,'    ',idum)
        CALL gunpak( idl(n),yer ,'ERRO',idum)
        DO k=1,nchx
           bi(k,n)=yyy(k)
           er(k,n)=yer(k)
        ENDDO
      ENDDO
!------------------------------!
!           Header
!------------------------------!
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') '% ========================================='
      WRITE(NOUH1,'(A)') '% ============= begin table ==============='
      WRITE(NOUH1,'(2A)') BS,'begin{table}[!ht]'
      WRITE(NOUH1,'(2A)') BS,'centering'
!------------------------------!
! Central Caption
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'caption{',BS,'small'
      IF(KEYTIT.EQ.0) THEN
        WRITE(NOUH1,'(A)')     TITLE
      ELSE
        WRITE(NOUH1,'(A)')     TITCH
      ENDIF
      WRITE(NOUH1,'(A)') '}'
!------------------------------!
! Tabular header
!------------------------------!
      WRITE(NOUH1,'(20A)') BS,'begin{tabular}
     $ {|',  ('|c',j=1,Npl+1),  '||}'
!
      WRITE(NOUH1,'(4A)') BS,'hline',BS,'hline'
!------------------------------!
! Captions in columns
!------------------------------!
      WRITE(NOUH1,'(2A)') capt(1),('&',capt(j+1),j=1,Npl)
!
      WRITE(NOUH1,'(2A)') BS,BS
      WRITE(NOUH1,'(2A)') BS,'hline'
!----------------------------------------!
! Table content
! Note that by default RIGHT EDGE of bin is printed, as necessary for
! cumulative distributions, this can be changed with SLAN option
!----------------------------------------!
      CALL goptou(idl(1),ioplog,iopsla,ioperb,iopsc1,iopsc2)
      DO k=nch1,nchx,incr
        xi= dxl + (dxu-dxl)*k/(1d0*nchx)
        IF(iopsla.eq.2) xi= dxl + (dxu-dxl)*(k-0.5d0)/(1d0*nchx)
        IF(ioperb.eq.2) THEN
        WRITE(NOUH1,
     $  '(A,'//fmt(1)//'
     $     ,      '//Cn(Npl)//'(A,'//fmt(2)//',A,A,'//fmt(3)//'),  A)')
     $   '$', xi, ('$ & $', bi(k,j), BS, 'pm', er(k,j), j=1,Npl), '$'
        WRITE(NOUH1,'(2A)') BS,BS
        ELSE
        WRITE(NOUH1,
     $  '(A,'//fmt(1)//'
     $     ,      '//Cn(Npl)//'(A,'//fmt(2)//'),  A)')
     $   '$', xi, ('$ & $', bi(k,j), j=1,Npl), '$'
        WRITE(NOUH1,'(2A)') BS,BS
        ENDIF
      ENDDO
!------------------------------!
! Ending
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'hline',BS,'hline'
      WRITE(NOUH1,'(2A)') BS,'end{tabular}'
      WRITE(NOUH1,'(2A)') BS,'end{table}'
      WRITE(NOUH1,'(A)') '% ============= end   table ==============='
      WRITE(NOUH1,'(A)') '% ========================================='
      IF(npag .NE. 0) WRITE(NOUH1,'(2A)') BS,'newpage'

      RETURN
  900 WRITE(*,*) ' ++++ gpltab: NONEXISTIG HISTO ' ,ID
      RETURN
 901  WRITE(*,*) ' ++++ gpltab: TO MANY COLUMNS  ' ,Npl
      END


      subroutine gmonit_old(mode,id,wt,wtmax,rn)
c     **************************************
c Utility program for monitoring m.c. rejection weights.
c ---------------------------------------------------------
C It is backward compatible with WMONIT except:
c  (1) for id=-1 one  should call as follows:
c      call(-1,id,0d0,1d0,1d0) or skip initialisation completely!
c  (2) maximum absolute weight is looked for,
c  (3) gprint(-id) prints weight distribution, net profit!
c  (4) no restriction id<100 any more!
c ---------------------------------------------------------
c wt is weight, wtmax is maximum weight and rn is random number.
c IF(mode .EQ. -1) then
c          initalization if entry id, 
c        - wtmax is maximum weight used for couting overweighted
c          other arguments are ignored
c ELSEIF(mode .EQ. 0) then
c          summing up weights etc. for a given event for entry id
c        - wt is current weight.
c        - wtmax is maximum weight used for couting overweighted
c          events with wt>wtmax.
c        - rn is random number used in rejection, it is used to
c          count no. of accepted (rn < wt/wtmax) and rejected
c          (wt > wt/wtmax) events,
c          if ro rejection then put rn=0d0.
c ELSEIF(mode .EQ. 1) then
c          in this mode wmonit repports on accumulated statistics
c          and the information is stored in common /cmonit/
c        - averwt= average weight wt counting all event
c        - errela= relative error of averwt
c        - nevtot= total nimber of accounted events
c        - nevacc= no. of accepted events (rn < wt/wtmax)
c        - nevneg= no. of events with negative weight (wt < 0)
c        - nevzer= no. of events with zero weight (wt = 0d0)
c        - nevove= no. of overweghted events (wt > wtmax)
c          and if you do not want to use cmonit then the value
c          the value of averwt is assigned to wt,
c          the value of errela is assigned to wtmax and
c          the value of wtmax  is assigned to rn in this mode.
c ELSEIF(mode .EQ. 2) then
c          all information defined for entry id defined above
c          for mode=2 is just printed of unit nout
c ENDIF
c note that output repport (mode=1,2) is done dynamically just for a
c given entry id only and it may be repeated many times for one id and
c for various id's as well.
c     ************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
c special gmonit common
      common / cmonit/ averwt,errela,nevtot,nevacc,nevneg,nevove,nevzer
      save / cglib /,/gind/, /cmonit/
c
      idg = -id
      IF(id .LE. 0) then
           write(nout,*) ' =====> Gmonit: wrong id= ',id
           write(   6,*) ' =====> Gmonit: wrong id= ',id
           stop
      ENDIF
      IF(mode .EQ. -1) then
c     *******************
           nbin = nint(dabs(rn))
           IF(nbin .GT. 100) nbin =100 
           IF(nbin .EQ. 0)   nbin =1
           xl   =  wt
           xu   =  wtmax
           IF(xu .LE. xl) then
             xl = 0d0
             xu = 1d0
           ENDIF
           lact=jadres(idg)
           IF(lact .EQ. 0) then
              call gbook1(idg,' gmonit $',nbin,xl,xu)
           ELSE
              write(nout,*) ' WARNING gmonit: exists, id= ',id
              write(   6,*) ' WARNING gmonit: exists, id= ',id
           ENDIF
      ELSEIF(mode .EQ. 0) then
c     **********************
           lact=jadres(idg)
           IF(lact .EQ. 0) then
              write(nout,*) ' *****> Gmonit: uinitialized, id= ',id
              write(   6,*) ' *****> Gmonit: uinitialized, id= ',id
              call gbook1(idg,' gmonit $',1,0d0,1d0)
! ms 7/2/96 >>
              lact=jadres(idg)
! ms 7/2/96 <<
           ENDIF
c     standard entries
           call gf1(idg,wt,1d0)
c     additional goodies
           ist  = index(lact,2)
           ist2 = ist+7
           ist3 = ist+11
c    maximum weight -- maximum by absolute value but keeping sign
           b(ist3+13)    = max( dabs(b(ist3+13)) ,dabs(wt))
           IF(wt .NE. 0d0) b(ist3+13)=b(ist3+13) *wt/dabs(wt)
c    nevzer,nevove,nevacc
           IF(wt .EQ. 0d0)        b(ist3+10) =b(ist3+10) +1d0
           IF(wt .GT. wtmax)      b(ist3+11) =b(ist3+11) +1d0
           IF(rn*wtmax .LE. wt)   b(ist3+12) =b(ist3+12) +1d0
      ELSEIF(mode .GE. 1.or.mode .LE. 3) then
c     ***********************************
           lact=jadres(idg)
           IF(lact .EQ. 0) then
              write(nout,*) ' +++++++++ STOP in  wmonit ++++++++++++++'
              write(   6,*) ' +++++++++ STOP in  wmonit ++++++++++++++'
              write(nout,*) ' lack of initialization, id=',id
              write(   6,*) ' lack of initialization, id=',id
              STOP
           ENDIF
           ist    = index(lact,2)
           ist2   = ist+7
           ist3   = ist+11
           ntot   = nint(b(ist3 +7))
           swt    =      b(ist3 +8)
           sswt   =      b(ist3 +9)
           IF(ntot .LE. 0 .or. swt  .EQ.  0d0 )  then
              averwt=0d0
              errela=0d0
           ELSE
              averwt=swt/float(ntot)
              errela=sqrt(abs(sswt/swt**2-1d0/float(ntot)))
           ENDIF
! output through commons
           nevtot = ntot
           nevacc = b(ist3 +12)
           nevneg = b(ist3  +1)
           nevzer = b(ist3 +10)
           nevove = b(ist3 +11)
           wwmax  = b(ist3 +13)
!  output through parameters
           wt     = averwt
           wtmax  = errela
           rn     = wwmax
c  no printout for mode > 1
c  ************************
           IF(mode .EQ. 1) return
           write(nout,1003) id, averwt, errela, wwmax
           write(nout,1004) nevtot,nevacc,nevneg,nevove,nevzer
           IF(mode .EQ. 2) return
           call gprint(idg)
      ELSE
c     ****
           write(nout,*) ' =====wmonit: wrong mode',mode
           write(   6,*) ' =====wmonit: wrong mode',mode
           stop
      ENDIF
c     *****
 1003 format(
     $  ' =======================gmonit========================'
     $/,'   id           averwt         errela            wwmax'
     $/,    i5,           e17.7,         f15.9,           e17.7)
 1004 format(
     $  ' -----------------------------------------------------------'
     $/,'      nevtot      nevacc      nevneg      nevove      nevzer'
     $/,   5i12)
      end


      SUBROUTINE gmonit(mode,id,par1,par2,par3)
!     *****************************************
! Utility program for monitoring m.c. rejection weights.
! ---------------------------------------------------------
! It is backward compatible with WMONIT except:
!  (1) for id=-1 one  should call as follows:
!      gmonit(-1,id,0d0,1d0,1d0) or skip initialisation completely!
!  (2) maximum absolute weight is looked for,
!  (3) gprint(-id) prints weight distribution, net profit!
!  (4) no restriction id<100 any more!
! ---------------------------------------------------------
! wt is weight, wtmax is maximum weight and rn is random number.
! IF(mode .EQ. -1) then
!          initalization if entry id, 
!        - wtmax is maximum weight used for couting overweighted
!          other arguments are ignored
! ELSEIF(mode .EQ. 0) then
!          summing up weights etc. for a given event for entry id
!        - wt is current weight.
!        - wtmax is maximum weight used for couting overweighted
!          events with wt>wtmax.
!        - rn is random number used in rejection, it is used to
!          count no. of accepted (rn < wt/wtmax) and rejected
!          (wt > wt/wtmax) events,
!          if ro rejection then put rn=0d0.
! ELSEIF(mode .EQ. 1) THEN
!          in this mode wmonit repports on accumulated statistics
!          and the information is stored in COMMON /cmonit/
!        - averwt= average weight wt counting all event
!        - errela= relative error of averwt
!        - nevtot= total number of accounted events
!        - nevacc= no. of accepted events (rn < wt/wtmax)
!        - nevneg= no. of events with negative weight (wt < 0)
!        - nevzer= no. of events with zero weight (wt = 0d0)
!        - nevove= no. of overweghted events (wt > wtmax)
!          and if you do not want to use cmonit then the value
!          the value of averwt is assigned to wt,
!          the value of errela is assigned to wtmax and
!          the value of wtmax  is assigned to rn in this mode.
! ELSEIF(mode .EQ. 2) THEN
!          all information defined for entry id defined above
!          for mode=2 is just printed of unit nout
! ENDIF
! note that output repport (mode=1,2) is done dynamically just for a
! given entry id only and it may be repeated many times for one id and
! for various id's as well.
!     ************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE
!
      idg = -id
      IF(id .LE. 0) THEN
           WRITE(nout,*) ' =====> Gmonit: wrong id= ',id
           WRITE(   6,*) ' =====> Gmonit: wrong id= ',id
           STOP
      ENDIF
      IF(mode .EQ. -1) THEN
!     *******************
           nbin = nint(dabs(par3))
           IF(nbin .GT. 100) nbin =100 
           IF(nbin .EQ. 0)   nbin =1
           xl   =  par1
           xu   =  par2
           IF(xu .LE. xl) THEN
             xl = 0d0
             xu = 1d0
           ENDIF
           lact=jadres(idg)
           IF(lact .EQ. 0) THEN
              CALL gbook1(idg,' gmonit $',nbin,xl,xu)
           ELSE
              WRITE(nout,*) ' WARNING gmonit: exists, id= ',id
              WRITE(   6,*) ' WARNING gmonit: exists, id= ',id
           ENDIF
      ELSEIF(mode .EQ. 0) THEN
!     **********************
           lact=jadres(idg)
           IF(lact .EQ. 0) THEN
              WRITE(nout,*) ' *****> Gmonit: uninitialized, id= ',id
              WRITE(   6,*) ' *****> Gmonit: uninitialized, id= ',id
              CALL gbook1(idg,' gmonit $',1,0d0,1d0)
              lact=jadres(idg)
           ENDIF
           wt   =par1
           wtmax=par2
           rn   =par3
!     standard entries
           CALL gf1(idg,wt,1d0)
!     additional goodies
           ist  = index(lact,2)
           ist2 = ist+7
           ist3 = ist+11
!    maximum weight -- maximum by absolute value but keeping sign
           b(ist3+13)    = max( dabs(b(ist3+13)) ,dabs(wt))
           IF(wt .NE. 0d0) b(ist3+13)=b(ist3+13) *wt/dabs(wt)
!    nevzer,nevove,nevacc
           IF(wt .EQ. 0d0)        b(ist3+10) =b(ist3+10) +1d0
           IF(wt .GT. wtmax)      b(ist3+11) =b(ist3+11) +1d0
           IF(rn*wtmax .LE. wt)   b(ist3+12) =b(ist3+12) +1d0
      ELSEIF(mode .GE. 1 .OR. mode .LE. 10) THEN
!     *************************************
           lact=jadres(idg)
           IF(lact .EQ. 0) THEN
              WRITE(nout,*) ' +++++++++ STOP in  wmonit ++++++++++++++'
              WRITE(   6,*) ' +++++++++ STOP in  wmonit ++++++++++++++'
              WRITE(nout,*) ' lack of initialization, id=',id
              WRITE(   6,*) ' lack of initialization, id=',id
              STOP
           ENDIF
           ist    = index(lact,2)
           ist2   = ist+7
           ist3   = ist+11
           ntot   = nint(b(ist3 +7))
           swt    =      b(ist3 +8)
           sswt   =      b(ist3 +9)
           IF(ntot .LE. 0 .or. swt  .EQ.  0d0 )  THEN
              averwt=0d0
              errela=0d0
           ELSE
              averwt=swt/float(ntot)
              errela=sqrt(abs(sswt/swt**2-1d0/float(ntot)))
           ENDIF
           nevacc = b(ist3 +12)
           nevneg = b(ist3  +1)
           nevove = b(ist3 +11)
           nevzer = b(ist3 +10)
           wwmax  = b(ist3 +13)
           nevtot = ntot
!  output through parameters
           par1   = averwt
           par2   = errela
           par3   = nevtot
           IF(mode .EQ. 2) THEN
              par1   = nevacc
              par2   = nevneg
              par3   = nevove
           ELSEIF(mode .EQ. 3) THEN
c m.s 2/27/98 knox.              par1   = nevneg
              par1   = nevzer
c m.s 2/27/98 knox.
              par2   = wwmax
           ENDIF
!  no printout for mode > 1
!  ************************
           IF(mode .LE. 9) RETURN
           WRITE(nout,1003) id, averwt, errela, wwmax
           WRITE(nout,1004) nevtot,nevacc,nevneg,nevove,nevzer
           IF(mode .EQ. 2) RETURN
           CALL gprint(idg)
      ELSE
!     ****
           WRITE(nout,*) ' =====wmonit: wrong mode',mode
           WRITE(   6,*) ' =====wmonit: wrong mode',mode
           STOP
      ENDIF
!     *****
 1003 format(
     $  ' =======================gmonit========================'
     $/,'   id           averwt         errela            wwmax'
     $/,    i5,           e17.7,         f15.9,           e17.7)
 1004 format(
     $  ' -----------------------------------------------------------'
     $/,'      nevtot      nevacc      nevneg      nevove      nevzer'
     $/,   5i12)
      END
C  !!!! LOGBOOK of corrections since 24 Nov 91 !!!!!
C  
C * line in MARRAN to long ( in printout of ijkl)
C * CHBIN2 replaced by CHBIN1
C  !!!!!!!!!!!!!!

C Library of utilities for YFS and BHLUMI programs
C version 1.0 November 91
      SUBROUTINE CHBIN3(R,ALF,BET,X,XPRIM,DJAC)
C     *****************************************
C Written: Dec. 1991
C This routine mapps variable R into X, XPRIM=1-X.
C To be employed in the integration (either ordinary or Monte Carlo)
C of any distributions resambling the binomial distribution 
C             x**(alf-1)*(1-x)**(bet-1).
C with 1> alf,bet > 0. Variables R and X are  in (0,1) range.
C Djac is the Jacobian factor d(x)/d(r).
C Mapping is such that 1/djac is very close to
C binomial distribution x**(alf-1)*(1-x)**(bet-1).
C WARNING: 
C Mapping may fail very close to R=0 and R=1. User is recommended 
C to assure that: fleps**alf < R < 1-fleps**bet, 
C where fleps = 1.d-30.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / INOUT  / NINP,NOUT
      SAVE   / INOUT  /
C
      IF( ALF.LE.1D-10 .OR. ALF .GT. 3D0 ) GOTO 900
      IF( BET.LE.1D-10 .OR. BET .GT. 3D0 ) GOTO 900
      X0=(1D0-ALF)/(2D0-ALF-BET)
      X0= MIN( MAX(X0, 0.001D0), 0.999D0)
      Q1=       X0**ALF            *BET*(1D0-X0)**(BET-1D0)
      Q2=       ALF*X0**(ALF-1D0)  *((1D0-X0)**BET)
      P1= Q1/(Q1+Q2)
      IF( R.LE.P1 ) THEN
         X    =  X0*(R/P1)**(1D0/ALF)
         XPRIM=  1D0-X
         DIST =  ALF* X**(ALF-1D0)  *BET*(1D0-X0)**(BET-1D0)
ccc      write(6,*) '3A:x,x1=',x,xprim
      ELSE
         XPRIM=  (1-X0)*((1D0-R)/(1D0-P1))**(1D0/BET)
         X    =  1D0- XPRIM
         DIST =  ALF*X0**(ALF-1D0)  *BET*XPRIM**(BET-1D0)
ccc      write(6,*) '3B:x,x1=',x,xprim
      ENDIF
      DJAC    =  (Q1+Q2)/DIST
      RETURN
  900 WRITE(NOUT,*) ' ++++ STOP IN CHBIN3: wrong parameters'
      WRITE(   6,*) ' ++++ STOP IN CHBIN3: wrong parameters'
      STOP
      END

      SUBROUTINE CHBIN1(R,ALF,BET,XMAX,X,DJAC)
C     ****************************************
C     last correction Dec. 91
c this mapps variable r into x.
c to be employed in the integration (either ordinary or monte carlo)
c of distributions resambling
c the binomial distribution x**(alf-1)*(1-x)**(bet-1)
c with alf > 0 and  bet arbitrary.
c variable r is in (0,1) range and x is within (0,xmax) range.
c djac is jacobian factor d(x)/d(r).
c mapping is such that 1/djac is very close to
c binomial distribution x**(alf-1)*(1-x)**(bet-1).
c WARNING: mapping may fail very close to R=0. Practically, one is
c recommended to obey: fleps**alf < r, where fleps = 1.d-30.
c Problems may also arise for very small xmax ( below 1.d-12 ).
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / INOUT  / NINP,NOUT
      save   / INOUT  / 
C
      IF( ALF.LE.0D0 ) GOTO 900
      X0=(ALF-1D0)/(ALF+BET-2D0)
      IF(X0.GT.XMAX) X0=XMAX
      X0= MAX(X0, 0D0)
      Q1= 1D0/ALF*X0**ALF  *(1D0-X0)**(BET-1D0)
      Q2= X0**(ALF-1D0) /BET*((1D0-X0)**BET-(1D0-XMAX)**BET)
      P1= Q1/(Q1+Q2)
      IF( R.LE.P1 ) THEN
         X=X0*(R/P1)**(1D0/ALF)
         DIST= X**(ALF-1D0)*(1D0-X0)**(BET-1D0)
      ELSE
         R1= (1D0-R)/(1D0-P1)
         X = (1D0-XMAX)**BET + ((1D0-X0)**BET-(1D0-XMAX)**BET)*R1
         X = 1D0 - X**(1D0/BET)
         DIST= X0**(ALF-1D0)*(1D0-X)**(BET-1D0)
      ENDIF
      DJAC=(Q1+Q2)/DIST
      RETURN
  900 WRITE(NOUT,*) ' ========= STOP IN CHBIN1: WRONG PARAMS'
      STOP
      END


      SUBROUTINE VESK1W(MMODE,FUNSKO,PAR1,PAR2,PAR3) 
C     **********************************************
C======================================================================
C======================================================================
C===================== V E S K 1 W ====================================
C==================S. JADACH  SEPTEMBER 1985=========================== 
C==================S. JADACH  November  1991=========================== 
C======================================================================
C ONE DIMENSIONAL MONTE CARLO  SAMPLER. 
C Vesrion with weighted events! 
C DOUBLE PRECISION  FUNCTION FUNSKO IS THE DISTRIBUTION TO BE GENERATED.
C JLIM1 IS THE NUMBER OF ENTRIES IN THE EQUIDISTANT LATICE WHICH
C IS FORMED IN THE FIRST STAGE AND JLIM2 IS THE TOTAL MAXIMUM 
C NUMBER OF ENTRIES IN THE LATICE, NOTE THAT DIMENSIONS OF
C MATRICES IN /CESK8A/ SHOULD BE AT LEAST JLIM2+1 . 
C FOR MILD FUNSKO JLIM2=128 IS ENOUGH.   
C TO CREATE AN INDEPENDENT VERSION REPLACE /ESK8A/=>/ESK8B/. 
C     ********************************** 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      save
      COMMON / CESK1W / XX(1025),YY(1025),ZINT(1025),ZSUM,JMAX 
      COMMON / INOUT  / NINP,NOUT  
      DIMENSION RNUM(1)
      EXTERNAL FUNSKO
      DATA JLIM1,JLIM2/16,257/           
      DATA IWARM/0/                      
C                                        
      MODE=MMODE                         
      IF(MODE.EQ.-1) THEN                
C     ===================                
C INITIALISATION PART, SEE VINSKO FOR MORE COMMENTS
      INIRAN=1                           
      IWARM=1                            
      WT=0.                              
      SWT=0.                             
      SSWT=0.                            
      NEVS=0                             
C INITIALISATION PART, SAMPLING DISTRIBUTION FUNSKO
C AND FILLING MATRICES XX,YY,ZINT ETC.   
      JMAX=1                             
      XX(1)=0.                           
      XX(2)=1.                           
      YY(1)=FUNSKO(XX(1))                
      YY(2)=FUNSKO(XX(2))                
      IF(YY(1).LT.0.0.OR.YY(2).LT.0.0) GO TO 999 
      ZINT(1)=.5D0*(YY(2)+YY(1))*(XX(2)-XX(1))  
C
      JDIV=1                             
      DO 200 K=1,JLIM2-1                 
      IF(JMAX.LT.JLIM1) THEN             
C...    NOTE THAT DESK1W INCREMENTS JMAX=JMAX+1 IN EVERY CALL 
        CALL DESK1W(FUNSKO,JDIV)                
        JDIV=JDIV+2                      
        IF(JDIV.GT.JMAX) JDIV=1          
      ELSE                               
        JDIV=1                           
        ZMX=ZINT(1)                      
        DO 180 J=1,JMAX                  
        IF(ZMX.LT.ZINT(J)) THEN          
          ZMX=ZINT(J)                    
          JDIV=J                         
        ENDIF                            
  180   CONTINUE                         
        CALL DESK1W(FUNSKO,JDIV)                
      ENDIF                              
  200 CONTINUE                           
C                                        
C...  FINAL ADMINISTRATION, NORMALIZING ZINT ETC. 
      ZSUM1=0.                           
      ZSUM =0.                           
      DO 220 J=1,JMAX                    
      ZSUM1=ZSUM1+ZINT(J)                
      YMAX= MAX( YY(J+1),YY(J))          
      ZINT(J)=YMAX*(XX(J+1)-XX(J))       
  220 ZSUM=ZSUM+ZINT(J)                  
      SUM=0.                             
      DO 240 J=1,JMAX                    
      SUM=SUM+ZINT(J)                    
  240 ZINT(J)=SUM/ZSUM                   
C====>>>
C Crude x-section estimate
ccc      CINTEG=ZSUM 
ccc      ERRINT=0D0 
      PAR1=  ZSUM                      
      PAR2=  ZSUM
      PAR3=  ZSUM                      
C===<<<                                 
      ELSE IF(MODE.EQ.0) THEN            
C     =======================            
C GENERATION PART                        
      IF(IWARM.EQ.0) GOTO 901            
ccc  222 CONTINUE                           
ccc      IF( (WT-1D0).GT.1D-10) THEN                  
ccc        WT=WT-1.D0                       
ccc      ELSE                               
        CALL VARRAN(RNUM,1)
        RNUMB=RNUM(1)
        DO 215 J=1,JMAX                  
        JSTOP=J                          
  215   IF(ZINT(J).GT.RNUMB) GOTO 216    
  216   CONTINUE                         
        IF(JSTOP.EQ.1) THEN              
          D=RNUMB/ZINT(1)                
        ELSE                             
          D =(RNUMB-ZINT(JSTOP-1))/(ZINT(JSTOP)-ZINT(JSTOP-1))
        ENDIF                            
        X=XX(JSTOP)*(1.D0 -D )+XX(JSTOP+1)*D   
        FN=FUNSKO(X)                     
        IF(FN.LT.0.D0) GOTO 999            
        YYMAX=MAX(YY(JSTOP+1),YY(JSTOP)) 
        WT=FN/YYMAX                      
        NEVS=NEVS+1                      
        SWT=SWT+WT                       
        SSWT=SSWT+WT*WT                  
ccc      ENDIF                              
ccc      CALL VARRAN(RNUMB,1)
ccc      IF(RNUMB.GT.WT) GOTO 222           
      PAR1=  X                           
      PAR2=  FN
      PAR3=  WT                          
C                                        
      ELSE IF(MODE.EQ.1) THEN            
C     =======================            
C FINAL STATISTICS                       
C STJ 24.OCT.89                          
      CINTEG=0D0                         
      ERRINT=0D0                         
      IF(NEVS.GT.0) CINTEG=ZSUM*SWT/FLOAT(NEVS) 
      IF(NEVS.GT.0) ERRINT=SQRT(SSWT/SWT**2-1.D0/FLOAT(NEVS)) 
      PAR1=  CINTEG                      
      PAR2=  ERRINT
      PAR3=  ZSUM                      
C--
      ELSE                               
C     ====                               
      GOTO  902                          
      ENDIF                              
C     =====                              
C                                        
      RETURN                             
 901  WRITE(NOUT,9010)                   
 9010 FORMAT(' **** STOP IN VESK8A, LACK OF INITIALISATION') 
      STOP                               
 902  WRITE(NOUT,9020)                   
 9020 FORMAT(' **** STOP IN VESK8A, WRONG MODE ') 
      STOP                               
 999  WRITE(NOUT,9990)                   
 9990 FORMAT(' **** STOP IN VESK8A, NEGATIVE VALUE OF FUNSKO ') 
      STOP                               
      END                                
      SUBROUTINE DESK1W(FUNSKO,JDIV)            
C     ******************************            
C THIS ROUTINE BELONGS TO VESK8A PACKAGE 
C IT SUDIVIDES INTO TWO EQUAL PARTS THE INTERVAL 
C (XX(JDIV),XX(JDIV+1))  IN THE LATICE   
C     ***********************            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      save
      COMMON / CESK1W / XX(1025),YY(1025),ZINT(1025),ZSUM,JMAX 
      COMMON / INOUT  / NINP,NOUT  
      EXTERNAL FUNSKO
C                                        
      XNEW=.5D0*(XX(JDIV) +XX(JDIV+1))   
      DO 100 J=JMAX,JDIV,-1              
      XX(J+2)  =XX(J+1)                  
      YY(J+2)  =YY(J+1)                  
  100 ZINT(J+1)=ZINT(J)                  
      XX(JDIV+1)= XNEW                   
      YY(JDIV+1)= FUNSKO(XNEW)           
      IF(YY(JDIV+1).LT.0.) GOTO 999      
      ZINT(JDIV)  =.5D0*(YY(JDIV+1)+YY(JDIV)  )*(XX(JDIV+1)-XX(JDIV)  ) 
      ZINT(JDIV+1)=.5D0*(YY(JDIV+2)+YY(JDIV+1))*(XX(JDIV+2)-XX(JDIV+1)) 
      JMAX=JMAX+1                        
      RETURN                             
  999 WRITE(NOUT,9000)                   
 9000 FORMAT(' **** STOP IN DESK1W, NEGATIVE VALUE OF FUNSKO ')
      STOP                               
      END                                


      SUBROUTINE VESK2W(MODE,FUNSKO,X,Y,WT)
C     *************************************
C=======================================================================
C=======================================================================
C=======================================================================
C===============TWO DIMENSIONAL SAMPLER VESK2W==========================
C=======================================================================
C=======================================================================
C=======================================================================
C                         VESK2W                                       C
C  GENERAL PURPOSE ROUTINE TO GENERATE AN ARBITRARY TWO DIMENSIONAL    C
C  DISTRIBUTION SUPPLIED BY USER IN A FORM OF FUNCTION FUNSKO(X,Y)     C
C                 WRITTEN NOVEMBER 1985                                C
C                    BY S. JADACH                                      C
C                 LAST UPDATE:  07.NOV.1990                            C
C                 version with weighted event....                      C
C======================================================================C
C VESKO2 GENERATES TWO DIMENSIONAL DISTRIBUTION DEFINED BY ARBITRARY
C FUNCTION FUNSKO(X,Y) WHERE X,Y BELONG  TO (0,1) RANGE.
C THE METHOD CONSISTS IN DIVIDING UNIT PLAQUET INTO CELLS USING
C SORT OF 'LIFE-GAME' METHOD IN WHICH THE DIVISION OF A CELLS IS MADE
C (DURING INITIALISATION) ALWAYS FOR THIS CELL WHICH CONTAINS
C A MAXIMUM VALUE OF THE INTEGRAL OVER FUNSKO IN THE CELL.
C RESULTING CELLS CONTAIN (USUALLY UP TO FACTOR TWO) EQUAL INTERGRAL
C VALUE. THE GENERATION CONSISTS IN CHOOSING RANDOMLY  A CELL
C ACCORDING TO ITS CONTENT AND THEN IN GENERATING X,Y WITHIN THE CELL.
C REJECTION METHOD IS APPLIED AT THE END OF THE PROCEDURE IN ORDER TO
C ASSURE THAT X,Y ARE DISTRIBUTED PRECISELY ACCORDING TO FUNSKO(X,Y)
C                    PARAMETERS
C -/ MODE = -1 INITIALISATION, NO (X,Y) GENERATED, CALL VESKO2(-1,D1,D2)
C    HAS TO BE MADE PRIOR  TO GENERATING FIRST (X,Y) PAIR
C -/ MODE =  0 GENERATION OF (X,Y) PAIR BY CALL VESKO2(0,X,Y)
C -/ MODE =  1 CALL VESKO2(1,VALINT,ERRINT) MAY BE DONE AFTER LAST
C    (X,Y) WAS GENERATED IN ORDER TO OBTAIN THE VALUE OF THE INTEGRAL
C    VALINT AND ITS ERROR ERRINT, INTEGRAL IS CALCULATED USING AVERAGE
C    WEIGHTS ENCOUTERED DURING GENERATION PHASE
C -/ X,Y  IF MODE=-1 THE THEY ARE DUMMY
C         IF MODE= 0 THE RESULT OF RANDOM GENERATION ACCORDING TO
C                    FUNCTION FUNSKO, X AND Y BELONG TO (0,1)
C         IF MODE= 1 X= VALUE OF INTEGRAL AND Y=ERROR (RELATIVE)
C                    WT = crude x-section
C ------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      save
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      DOUBLE PRECISION DRVEC(100)
      EXTERNAL FUNSKO
      DATA IWARM/77/

      IF(MODE) 100,200,300
C...  INITIALISATION PART, SEE VINSKO FOR MORE COMMENTS
  100 CALL VINSKW(FUNSKO)
      IWARM=0
      WT=0D0
      WTMAX = 1D0
      WTMXX = WTMAX
      NEVOV=0
      SWT=0D0
      SSWT=0D0
      NEVS=0
C(((((((((((((
C     CALL HBOOK1(1, 16H WT-VESKO2     $,75,0.0D0,1.5D0)
C     CALL HMINIM(1,0)
C     CALL HBOOK2(2,16H X-Y VESKO2    $, 64,0,1, 32,0,1,0)
C     CALL HSCALE(2)
C))))))))))))
      RETURN
C...
  200 CONTINUE
C...  GENERATION PART
      IF(IWARM.EQ.77) GO TO 980
cc    IF(WT.GT.WTMAX) THEN
cc      write(6,*) ' vesko2: ev. overweighted, dont worry, wt=',wt
cc      WT=WT-WTMAX
cc      NEVOV=NEVOV+1
cc    ELSE
        CALL VARRAN(DRVEC,3)
        R = DRVEC(1)
        DO 215 J=1,JMAX
        JSTOP=J
  215   IF(ZINT(J).GT.R) GOTO 216
  216   CONTINUE
        XR=XX(JSTOP,1)+DX(JSTOP,1)*DRVEC(2)
        YR=XX(JSTOP,2)+DX(JSTOP,2)*DRVEC(3)
        FN=FUNSKO(XR,YR)
        IF(FN.LT.0.) GOTO 999
        YYMAX=YYMX(JSTOP)
        WT=FN/YYMAX
        WTMXX = MAX(WTMXX,WT)
cc      IF(NEVS.LE.(4*JLIM2).AND.WT.GT.WTMAX) THEN
cc         WTMAX=WT*1.1D0
cc         WRITE(6,*) ' VESKO2: NEVS, new WTMAX= ',NEVS,WTMAX
cc      ENDIF
        NEVS=NEVS+1
        SWT=SWT+WT
        SSWT=SSWT+WT*WT
C((((((((((
C       CALL HFILL(1,WT,0D0,1D0)
C))))))))))
ccc   ENDIF
CCC    CALL VARRAN(DRVEC,1)
ccc    RN=DRVEC(1)
ccc   IF(WTMAX*RN.GT.WT) GOTO 200
      X=XR
      Y=YR
C((((((((((
C     CALL HFILL(2,XR,YR)
C))))))))))
      RETURN
C...
  300 CONTINUE
C THIS IS THE VALUE OF THE INTEGRAL
      CINTEG=ZSUM*SWT/NEVS
C AND ITS ERROR
      ERRINT=SQRT(SSWT/SWT**2-1D0/NEVS)
      X=CINTEG
      Y=ERRINT
      WT=ZSUM
C((((((((((
C     CALL HPRINT(1)
C     CALL HDELET(1)
C     CALL HPRINT(2)
C     CALL HDELET(2)
      PRINT 7000,NEVS,NEVOV,WTMAX,WTMXX
 7000 FORMAT(' VESK2W: NEVS,NEVOV,WTMAX,WTMXX= ',2I7,2F7.3)
C))))))))))
      RETURN
  980 WRITE(NOUT,9002)
 9002 FORMAT(' **** STOP IN VESK2W, LACK OF INITIALISATION   ')
      STOP
  999 WRITE(NOUT,9004)
 9004 FORMAT(' **** STOP IN VESK2W, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END

      SUBROUTINE VINSKW(FUNSKO)
C     *************************
C THIS ROUTINE BELONGS TO VESKO2 PACKAGE
C JLIM1 IS THE NUMBER OF CELLS, DIVISION OF THE UNIT PLAQUE INTO CELLS
C IS MADE IN THE FIRST STAGE.    JLIM2 IS THE TOTAL MAXIMUM
C NUMBER OF CELLS, NOTE THAT DIMENSIONS OF
C MATRICES IN /VESKOA/ SHOULD BE AT LEAST JLIM2
C     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      save
C ------------------------------------------------------------
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      EXTERNAL FUNSKO

C...  INITIALISATION PART, SAMPLING DISTRIBUTION FUNSKO
C...  AND FILLING MATRICES XX,YY,ZINT ETC.
      JMAX=1
      XX(1,1)=0D0
      XX(1,2)=0D0
      DX(1,1)=1D0
      DX(1,2)=1D0
      LEV(1)=1
      SUM=0D0
      DO 150 I=1,2
      DO 150 K=1,2
C... THIS IS NOT ELEGANT BUT SIMPLE
      YY(1,I,K)=FUNSKO(XX(1,1)+(I-1.)*DX(1,1),XX(1,2)+(K-1.)*DX(1,2))
      IF(YY(1,I,K).LT.0.0) GO TO 999
  150 SUM=SUM+YY(1,I,K)
      ZINT(1)=SUM*DX(1,1)*DX(1,2)/4D0

      JDIV=1
      DO 200 KK=1,JLIM2-1
      IF(JMAX.LT.JLIM1) THEN
C...    NOTE THAT DIVSKW INCREMENTS JMAX=JMAX+1 IN EVERY CALL
        CALL DIVSKW(JDIV,FUNSKO)
C(((((((((((
c      IF(JMAX.EQ.JLIM1) THEN
c      PRINT 9900,JMAX,(LEV(I),I=1,JMAX)
c 9900 FORMAT(///,' JMAX...  LEV LEV LEV LEV LEV',I10,/(24I5))
c      PRINT 9901,((XX(JD,I),I=1,2),JD=1,JMAX)
c 9901 FORMAT('  XX XX XX XX XX XX XX  ',/(10E12.5))
c      PRINT 9902,((DX(JD,I),I=1,2),JD=1,JMAX)
c 9902 FORMAT('  DX  DX DX DX DX DX ',/(10E12.5))
c      PRINT 9903,(((YY(JD,I,K),I=1,2),K=1,2),JD=1,JMAX)
c 9903 FORMAT('  YY  YY YY YY YY YY ',/(8E15.5))
c      PRINT 9904,(ZINT(I),I=1,JMAX)
c 9904 FORMAT('   ZINT ZINT ZINT ZINT ',/(10E12.5))
c      ENDIF
C))))))))))))
        JDIV=JDIV+2
        IF(JDIV.GT.JMAX) JDIV=1
      ELSE
        JDIV=1
        ZMX=ZINT(1)
        DO 180 J=1,JMAX
        IF(ZMX.LT.ZINT(J)) THEN
          ZMX=ZINT(J)
          JDIV=J
        ENDIF
  180   CONTINUE
        CALL DIVSKW(JDIV,FUNSKO)
      ENDIF
  200 CONTINUE

C(((((((((((
c      JPRN=64
c      PRINT 9910,JMAX,(LEV(I),I=1,JMAX)
c 9910 FORMAT(/,' JMAX...  LEV LEV LEV LEV LEV',I10,/(24I5))
c      IF(JMAX.LE.JPRN) PRINT 9911,((XX(JD,I),I=1,2),JD=1,JMAX)
c 9911 FORMAT('  XX XX XX XX XX XX XX  ',/(10E12.5))
c      IF(JMAX.LE.JPRN) PRINT 9912,((DX(JD,I),I=1,2),JD=1,JMAX)
c 9912 FORMAT('  DX  DX DX DX DX DX ',/(10E12.5))
c      IF(JMAX.LE.JPRN) PRINT 9913,(((YY(JD,I,K),I=1,2),K=1,2),JD=1,JMAX)
c 9913 FORMAT('  YY  YY YY YY YY YY ',/(8E15.5))
c      IF(JMAX.LE.JPRN) PRINT 9914,(ZINT(I),I=1,JMAX)
c 9914 FORMAT('   ZINT ZINT ZINT ZINT ',/(10E12.5))
C     DO 902 J=1,JMAX
C     Z=1D0*J-.5D0
C 902 CALL HFILL(202,Z,ZINT(J))
C))))))))))))
C...  FINAL ADMINISTRATION, NORMALIZING ZINT ETC.
      ZSUM1=0D0
      ZSUM =0D0
      DO 260 J=1,JMAX
      ZSUM1=ZSUM1+ZINT(J)
      YMAX= 0D0
      DO 250 I=1,2
      DO 250 K=1,2
  250 YMAX= MAX(YMAX,YY(J,I,K))
      YYMX(J)=YMAX
      ZINT(J)=YMAX*DX(J,1)*DX(J,2)
  260 ZSUM=ZSUM+ZINT(J)
C((((((((
      ZR=ZSUM1/ZSUM
      PRINT 7000,ZR
 7000 FORMAT(' /////// ZSUM1/ZSUM= ',F20.8)
C)))))))))
      SUM=0D0
      DO 240 J=1,JMAX
      SUM=SUM+ZINT(J)
  240 ZINT(J)=SUM/ZSUM
C(((((((((((
c     JPRN=64
c     PRINT 9932,JMAX
c9932 FORMAT(/'=====JMAX ZINT ZINT ZINT  ',I10)
c     IF(JMAX.LE.JPRN) PRINT 9935,(ZINT(I),I=1,JMAX)
c9935            FORMAT(10E12.5)
C     DO 901 J=2,JMAX
C 901 CALL HFILL(201,(ZINT(J)-ZINT(J-1))*JMAX)
C     CALL HFILL(201,ZINT(1)*JMAX)
C))))))))))))
      RETURN
  999 WRITE(NOUT,9000)
 9000 FORMAT(' **** STOP IN VINSKW, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END

      SUBROUTINE DIVSKW(JD,FUNSKO)
C     ****************************
C THIS ROUTINE BELONGS TO VESKO2 PACKAGE
C IT SUBDIVIDES ONE CELL (NO. JD) INTO TWO EQUAL SIZE CELLS
C     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      save
C ------------------------------------------------------------
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      EXTERNAL FUNSKO

C...  MOOVE TO MAKE A HOLE FOR A NEW ENTRY (ONE ADDITIONAL CELL)
      DO 100 J=JMAX,JD,-1
      ZINT(J+1)=ZINT(J)
      LEV(J+1)=LEV(J)
      DO 100 I=1,2
      XX(J+1,I)  =XX(J,I)
      DX(J+1,I)  =DX(J,I)
      DO 100 K=1,2
  100 YY(J+1,I,K)  =YY(J,I,K)
C...  CREATE TWO NEW CELLS AND STORE THEM
      LL= MOD(LEV(JD),2)+1
      DX(JD,LL)=DX(JD,LL)/2D0
      DX(JD+1,LL)=DX(JD+1,LL)/2D0
      XX(JD+1,LL)=XX(JD,LL)+DX(JD,LL)
      IF(LL.EQ.1) THEN
        DO 150 I=1,2
C... THIS IS NOT ELEGANT, PROBABLY COULD BE DONE BETTER
        YY(JD,2,I)=FUNSKO(XX(JD,1)+DX(JD,1),XX(JD,2)+(I-1.)*DX(JD,2))
  150   YY(JD+1,1,I)=YY(JD,2,I)
      ELSE
        DO 152 I=1,2
        YY(JD,I,2)=FUNSKO(XX(JD,1)+(I-1.)*DX(JD,1),XX(JD,2)+DX(JD,2))
  152   YY(JD+1,I,1)=YY(JD,I,2)
      ENDIF
C...  ESTIMATE THE INTEGRALS OVER NEW CELLS RESULTING FROM DIVISION
      DO 220 JDV=JD,JD+1
      LEV(JDV)=LEV(JDV)+1
      SUM=0D0
      DO 210 I=1,2
      DO 210 K=1,2
      IF(YY(JDV,I,K).LT.0.D0) GO TO 999
  210 SUM=SUM+YY(JDV,I,K)
  220 ZINT(JDV) =SUM*DX(JDV,1)*DX(JDV,2)/4D0
      JMAX=JMAX+1
      RETURN
  999 WRITE(NOUT,9000)
 9000 FORMAT(' **** STOP IN DIVSKW, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END


      SUBROUTINE GAUSJD(FUN,AA,BB,EEPS,RESULT)
C     ****************************************
C Gauss integration by S. Jadach, Oct. 90.
C This is NON-ADAPTIVE (!!!!) UNOPTIMIZED (!!!) integration subprogram.
C     *************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION WG(12),XX(12)
      COMMON / INOUT  / NINP,NOUT
      EXTERNAL FUN
      save /inout/,wg,xx,ITERMX 
      DATA WG
     $/0.101228536290376D0, 0.222381034453374D0, 0.313706645877887D0,
     $ 0.362683783378362D0, 0.027152459411754D0, 0.062253523938648D0,
     $ 0.095158511682493D0, 0.124628971255534D0, 0.149595988816577D0,
     $ 0.169156519395003D0, 0.182603415044924D0, 0.189450610455069D0/
      DATA XX
     $/0.960289856497536D0, 0.796666477413627D0, 0.525532409916329D0,
     $ 0.183434642495650D0, 0.989400934991650D0, 0.944575023073233D0,
     $ 0.865631202387832D0, 0.755404408355003D0, 0.617876244402644D0,
     $ 0.458016777657227D0, 0.281603550779259D0, 0.095012509837637D0/
      DATA ITERMX / 15/
      EPS=ABS(EEPS)
      A=AA
      B=BB
      NDIVI=1
C iteration over subdivisions terminated by precision requirement
      DO 400 ITER=1,ITERMX
      CALK8  =0D0
      CALK16 =0D0
C sum over DELTA subintegrals
      DO 200 K = 1,NDIVI
      DELTA = (B-A)/NDIVI
      X1    =  A + (K-1)*DELTA
      X2    =  X1+ DELTA
      XMIDLE= 0.5D0*(X2+X1)
      RANGE = 0.5D0*(X2-X1)
      SUM8 =0D0
      SUM16=0D0
C 8- and 12-point   Gauss integration over single DELTA subinterval
      DO 100 I=1,12
      XPLUS= XMIDLE+RANGE*XX(I)
      XMINU= XMIDLE-RANGE*XX(I)
      FPLUS=FUN(XPLUS)
      FMINU=FUN(XMINU)
      IF(I.LE.4) THEN
          SUM8 =SUM8  +(FPLUS+FMINU)*WG(I)/2D0
      ELSE
          SUM16=SUM16 +(FPLUS+FMINU)*WG(I)/2D0
      ENDIF
  100 CONTINUE
      CALK8 = CALK8 + SUM8 *(X2-X1)
      CALK16= CALK16+ SUM16*(X2-X1)
  200 CONTINUE
      ERABS = ABS(CALK16-CALK8)
      ERELA = 0D0
      IF(CALK16.NE.0D0) ERELA= ERABS/ABS(CALK16)
c     write(6,*) 'gausjd: CALK8,CALK16=',ITER,CALK8,CALK16,ERELA
C precision check to terminate integration
      IF(EEPS.GT.0D0) THEN
        IF(ERABS.LT. EPS) GOTO 800
      ELSE
        IF(ERELA.LT. EPS) GOTO 800
      ENDIF
  400 NDIVI=NDIVI*2
      WRITE(NOUT,*) ' +++++ GAUSJD:  REQUIRED PRECISION TO HIGH!'
      WRITE(NOUT,*) ' +++++ GAUSJD:  ITER,ERELA=',ITER,ERELA
  800 RESULT= CALK16
      END


      SUBROUTINE WMONIT(MODE,ID,WT,WTMAX,RN)
C     **************************************
C last correction 19 sept. 89
C Utility program for monitoring M.C. rejection weights.
C ID is weight idendifier, maximum IDMX (defined below).
C WT IS WEIGHT, WTMAX IS MAXIMUM WEIGHT AND RN IS RANDOM NUMBER.
C IF(MODE.EQ.-1) THEN
C          INITALIZATION IF ENTRY ID, OTHER ARGUMENTS ARE IGNORED
C ELSEIF(MODE.EQ.0) THEN
C          SUMMING UP WEIGHTS ETC. FOR A GIVEN EVENT FOR ENTRY ID
C        - WT IS CURRENT WEIGHT.
C        - WTMAX IS MAXIMUM WEIGHT USED FOR COUTING OVERWEIGHTED
C          EVENTS WITH WT>WTMAX.
C        - RN IS RANDOM NUMBER USED IN REJECTION, IT IS USED TO
C          COUNT NO. OF ACCEPTED (RN<WT/WTMAX) AND REJECTED
C          (WT>WT/WTMAX) EVENTS,
C          IF RO REJECTION THEN PUT RN=0D0.
C ELSEIF(MODE.EQ.1) THEN
C          IN THIS MODE WMONIT REPPORTS ON ACCUMULATED STATISTICS
C          AND THE INFORMATION IS STORED IN COMMON /CMONIT/
C        - AVERWT= AVERAGE WEIGHT WT COUNTING ALL EVENT
C        - ERRELA= RELATIVE ERROR OF AVERWT
C        - NEVTOT= TOTAL NIMBER OF ACCOUNTED EVENTS
C        - NEVACC= NO. OF ACCEPTED EVENTS (RN<WT\WTMAX)
C        - NEVNEG= NO. OF EVENTS WITH NEGATIVE WEIGHT (WT<0)
C        - NEVZER= NO. OF EVENTS WITH ZERO WEIGHT (WT.EQ.0D0)
C        - NEVOVE= NO. OF OVERWEGHTED EVENTS (WT>WTMAX)
C          AND IF YOU DO NOT WANT TO USE CMONIT THEN THE VALUE
C          The value of AVERWT is assigned to WT,
C          the value of ERRELA is assigned to WTMAX and
C          the value of WTMAX  is assigned to RN in this mode.
C ELSEIF(MODEE.EQ.2) THEN
C          ALL INFORMATION DEFINED FOR ENTRY ID DEFINED ABOVE
C          FOR MODE=2 IS JUST PRINTED OF UNIT NOUT
C ENDIF
C NOTE THAT OUTPUT REPPORT (MODE=1,2) IS DONE DYNAMICALLY JUST FOR A
C GIVEN ENTRY ID ONLY AND IT MAY BE REPEATED MANY TIMES FOR ONE ID AND
C FOR VARIOUS ID'S AS WELL.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save
      PARAMETER(IDMX=100)
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / INOUT  / NINP,NOUT
      INTEGER NTOT(IDMX),NACC(IDMX),NNEG(IDMX),NOVE(IDMX),NZER(IDMX)
      DIMENSION SWT(IDMX),SSWT(IDMX),WWMX(IDMX)
      DATA NTOT /IDMX* -1/  SWT /IDMX*   0D0/
      DATA SSWT /IDMX*0D0/ WWMX /IDMX*-1D-20/
C
      IF(ID.LE.0.OR.ID.GT.IDMX) THEN
           WRITE(NOUT,*) ' =====WMONIT: WRONG ID',ID
           STOP
      ENDIF
      IF(MODE.EQ.-1) THEN
           NTOT(ID)=0
           NACC(ID)=0
           NNEG(ID)=0
           NZER(ID)=0
           NOVE(ID)=0
           SWT(ID)   =0D0
           SSWT(ID)  =0D0
           WWMX(ID)  = -1D-20
      ELSEIF(MODE.EQ.0) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           NTOT(ID)=NTOT(ID)+1
           SWT(ID)=SWT(ID)+WT
           SSWT(ID)=SSWT(ID)+WT**2
           WWMX(ID)= MAX(WWMX(ID),WT)
           IF(WT.EQ.0D0)   NZER(ID)=NZER(ID)+1
           IF(WT.LT.0D0)   NNEG(ID)=NNEG(ID)+1
           IF(WT.GT.WTMAX)      NOVE(ID)=NOVE(ID)+1
           IF(RN*WTMAX.LE.WT)   NACC(ID)=NACC(ID)+1
      ELSEIF(MODE.EQ.1) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
           ENDIF
           NEVTOT=NTOT(ID)
           NEVACC=NACC(ID)
           NEVNEG=NNEG(ID)
           NEVZER=NZER(ID)
           NEVOVE=NOVE(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSEIF(MODE.EQ.2) THEN
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
              WWMAX=WWMX(ID)
           ENDIF
           WRITE(NOUT,1003) ID, AVERWT, ERRELA, WWMAX
           WRITE(NOUT,1004) NTOT(ID),NACC(ID),NNEG(ID),NOVE(ID),NZER(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSE
           WRITE(NOUT,*) ' =====WMONIT: WRONG MODE',MODE
           STOP
      ENDIF
 1003 FORMAT(
     $  ' =======================WMONIT========================'
     $/,'   ID           AVERWT         ERRELA            WWMAX'
     $/,    I5,           E17.7,         F15.9,           E17.7)
 1004 FORMAT(
     $  ' -----------------------------------------------------------'
     $/,'      NEVTOT      NEVACC      NEVNEG      NEVOVE      NEVZER'
     $/,   5I12)
      END

      SUBROUTINE WMONI2(MODE,ID,WT,WTMAX,RN)
C     **************************************
C -------------- SECOND COPY OF WMONIT ----------------
C last correction 19 sept. 89
C Utility program for monitoring M.C. rejection weights.
C ID is weight idendifier, maximum IDMX (defined below).
C WT IS WEIGHT, WTMAX IS MAXIMUM WEIGHT AND RN IS RANDOM NUMBER.
C IF(MODE.EQ.-1) THEN
C          INITALIZATION IF ENTRY ID, OTHER ARGUMENTS ARE IGNORED
C ELSEIF(MODE.EQ.0) THEN
C          SUMMING UP WEIGHTS ETC. FOR A GIVEN EVENT FOR ENTRY ID
C        - WT IS CURRENT WEIGHT.
C        - WTMAX IS MAXIMUM WEIGHT USED FOR COUTING OVERWEIGHTED
C          EVENTS WITH WT>WTMAX.
C        - RN IS RANDOM NUMBER USED IN REJECTION, IT IS USED TO
C          COUNT NO. OF ACCEPTED (RN<WT/WTMAX) AND REJECTED
C          (WT>WT/WTMAX) EVENTS,
C          IF RO REJECTION THEN PUT RN=0D0.
C ELSEIF(MODE.EQ.1) THEN
C          IN THIS MODE WMONIT REPPORTS ON ACCUMULATED STATISTICS
C          AND THE INFORMATION IS STORED IN COMMON /CMONIT/
C        - AVERWT= AVERAGE WEIGHT WT COUNTING ALL EVENT
C        - ERRELA= RELATIVE ERROR OF AVERWT
C        - NEVTOT= TOTAL NIMBER OF ACCOUNTED EVENTS
C        - NEVACC= NO. OF ACCEPTED EVENTS (RN<WT\WTMAX)
C        - NEVNEG= NO. OF EVENTS WITH NEGATIVE WEIGHT (WT<0)
C        - NEVZER= NO. OF EVENTS WITH ZERO WEIGHT (WT.EQ.0D0)
C        - NEVOVE= NO. OF OVERWEGHTED EVENTS (WT>WTMAX)
C          AND IF YOU DO NOT WANT TO USE CMONIT THEN THE VALUE
C          The value of AVERWT is assigned to WT,
C          the value of ERRELA is assigned to WTMAX and
C          the value of WTMAX  is assigned to RN in this mode.
C ELSEIF(MODEE.EQ.2) THEN
C          ALL INFORMATION DEFINED FOR ENTRY ID DEFINED ABOVE
C          FOR MODE=2 IS JUST PRINTED OF UNIT NOUT
C ENDIF
C NOTE THAT OUTPUT REPPORT (MODE=1,2) IS DONE DYNAMICALLY JUST FOR A
C GIVEN ENTRY ID ONLY AND IT MAY BE REPEATED MANY TIMES FOR ONE ID AND
C FOR VARIOUS ID'S AS WELL.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save
      PARAMETER(IDMX=100)
      COMMON / CMONI2/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / INOUT  / NINP,NOUT
      INTEGER NTOT(IDMX),NACC(IDMX),NNEG(IDMX),NOVE(IDMX),NZER(IDMX)
      DIMENSION SWT(IDMX),SSWT(IDMX),WWMX(IDMX)
      DATA NTOT /IDMX* -1/  SWT /IDMX*   0D0/
      DATA SSWT /IDMX*0D0/ WWMX /IDMX*-1D-20/
C
      IF(ID.LE.0.OR.ID.GT.IDMX) THEN
           WRITE(NOUT,*) ' =====WMONI2: WRONG ID',ID
           STOP
      ENDIF
      IF(MODE.EQ.-1) THEN
           NTOT(ID)=0
           NACC(ID)=0
           NNEG(ID)=0
           NZER(ID)=0
           NOVE(ID)=0
           SWT(ID)   =0D0
           SSWT(ID)  =0D0
           WWMX(ID)  = -1D-20
      ELSEIF(MODE.EQ.0) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           NTOT(ID)=NTOT(ID)+1
           SWT(ID)=SWT(ID)+WT
           SSWT(ID)=SSWT(ID)+WT**2
           WWMX(ID)= MAX(WWMX(ID),WT)
           IF(WT.EQ.0D0)   NZER(ID)=NZER(ID)+1
           IF(WT.LT.0D0)   NNEG(ID)=NNEG(ID)+1
           IF(WT.GT.WTMAX)      NOVE(ID)=NOVE(ID)+1
           IF(RN*WTMAX.LE.WT)   NACC(ID)=NACC(ID)+1
      ELSEIF(MODE.EQ.1) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONI2: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
           ENDIF
           NEVTOT=NTOT(ID)
           NEVACC=NACC(ID)
           NEVNEG=NNEG(ID)
           NEVZER=NZER(ID)
           NEVOVE=NOVE(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSEIF(MODE.EQ.2) THEN
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
              WWMAX=WWMX(ID)
           ENDIF
           WRITE(NOUT,1003) ID, AVERWT, ERRELA, WWMAX
           WRITE(NOUT,1004) NTOT(ID),NACC(ID),NNEG(ID),NOVE(ID),NZER(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSE
           WRITE(NOUT,*) ' =====WMONI2: WRONG MODE',MODE
           STOP
      ENDIF
 1003 FORMAT(
     $  ' =======================WMONI2========================'
     $/,'   ID           AVERWT         ERRELA            WWMAX'
     $/,    I5,           E17.7,         F15.9,           E17.7)
 1004 FORMAT(
     $  ' -----------------------------------------------------------'
     $/,'      NEVTOT      NEVACC      NEVNEG      NEVOVE      NEVZER'
     $/,   5I12)
      END

      FUNCTION GAUS(F,A,B,EEPS)  
C     *************************   
C THIS IS ITERATIVE INTEGRATION PROCEDURE                             
C ORIGINATES  PROBABLY FROM CERN LIBRARY                              
C IT SUBDIVIDES INEGRATION RANGE UNTIL REQUIRED PRECISION IS REACHED  
C PRECISION IS A DIFFERENCE FROM 8 AND 16 POINT GAUSS ITEGR. RESULT   
C EEPS POSITIVE TREATED AS ABSOLUTE PRECISION                         
C EEPS NEGATIVE TREATED AS RELATIVE PRECISION                         
C     *************************              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      DIMENSION W(12),X(12)        
      COMMON / INOUT  / NINP,NOUT      
      EXTERNAL F                             
      DATA CONST /1.0D-19/     
      save     / INOUT/, CONST, W, X
      DATA W                       
     1/0.10122 85362 90376d0, 0.22238 10344 53374d0, 
     1 0.31370 66458 77887d0, 
     2 0.36268 37833 78362d0, 0.02715 24594 11754d0, 
     2 0.06225 35239 38648d0, 
     3 0.09515 85116 82493d0, 0.12462 89712 55534d0, 
     3 0.14959 59888 16577d0, 
     4 0.16915 65193 95003d0, 0.18260 34150 44924d0, 
     4 0.18945 06104 55069d0/ 
      DATA X                       
     1/0.96028 98564 97536d0, 0.79666 64774 13627d0, 
     1 0.52553 24099 16329d0, 
     2 0.18343 46424 95650d0, 0.98940 09349 91650d0, 
     1 0.94457 50230 73233d0, 
     3 0.86563 12023 87832d0, 0.75540 44083 55003d0, 
     1 0.61787 62444 02644d0, 
     4 0.45801 67776 57227d0, 0.28160 35507 79259d0, 
     1 0.09501 25098 37637d0/ 
      EPS=ABS(EEPS)                
      DELTA=CONST*ABS(A-B)         
      GAUS=0D0                     
      AA=A                         
    5 Y=B-AA                       
      IF(ABS(Y) .LE. DELTA) RETURN 
    2 BB=AA+Y                      
      C1=0.5D0*(AA+BB)             
      C2=C1-AA                     
      S8=0D0                       
      S16=0D0                      
      DO 1 I=1,4                   
      U=X(I)*C2                    
    1 S8=S8+W(I)*(F(C1+U)+F(C1-U)) 
      DO 3 I=5,12                  
      U=X(I)*C2                    
    3 S16=S16+W(I)*(F(C1+U)+F(C1-U))                                  
      S8=S8*C2                     
      S16=S16*C2                   
      IF(EEPS.LT.0D0) THEN         
        IF(ABS(S16-S8) .GT. EPS*ABS(S16)) GO TO 4                     
      ELSE             
        IF(ABS(S16-S8) .GT. EPS) GO TO 4                  
      ENDIF            
      GAUS=GAUS+S16    
      AA=BB            
      GO TO 5          
    4 Y=0.5D0*Y        
      IF(ABS(Y) .GT. DELTA) GOTO 2                        
      WRITE(NOUT,7)                          
      GAUS=0D0                
      RETURN                  
    7 FORMAT(1X,36HGAUS  ... TOO HIGH ACCURACY REQUIRED)         
      END                     

      FUNCTION GAUS2(F,A,B,EEPS)  
C     *************************   
C THIS IS ITERATIVE INTEGRATION PROCEDURE                             
C ORIGINATES  PROBABLY FROM CERN LIBRARY                              
C IT SUBDIVIDES INEGRATION RANGE UNTIL REQUIRED PRECISION IS REACHED  
C PRECISION IS A DIFFERENCE FROM 8 AND 16 POINT GAUSS ITEGR. RESULT   
C EEPS POSITIVE TREATED AS ABSOLUTE PRECISION                         
C EEPS NEGATIVE TREATED AS RELATIVE PRECISION                         
C     *************************              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      DIMENSION W(12),X(12)        
      COMMON / INOUT  / NINP,NOUT      
      EXTERNAL F                             
      DATA CONST /1.0D-19/     
      save     / INOUT/, CONST, W, X
      DATA W                       
     1/0.10122 85362 90376d0, 0.22238 10344 53374d0, 
     1 0.31370 66458 77887d0, 
     2 0.36268 37833 78362d0, 0.02715 24594 11754d0, 
     2 0.06225 35239 38648d0, 
     3 0.09515 85116 82493d0, 0.12462 89712 55534d0, 
     3 0.14959 59888 16577d0, 
     4 0.16915 65193 95003d0, 0.18260 34150 44924d0, 
     4 0.18945 06104 55069d0/ 
      DATA X                       
     1/0.96028 98564 97536d0, 0.79666 64774 13627d0, 
     1 0.52553 24099 16329d0, 
     2 0.18343 46424 95650d0, 0.98940 09349 91650d0, 
     1 0.94457 50230 73233d0, 
     3 0.86563 12023 87832d0, 0.75540 44083 55003d0, 
     1 0.61787 62444 02644d0, 
     4 0.45801 67776 57227d0, 0.28160 35507 79259d0, 
     1 0.09501 25098 37637d0/ 
      EPS=ABS(EEPS)                
      DELTA=CONST*ABS(A-B)         
      GAUS2=0D0                     
      AA=A                         
    5 Y=B-AA                       
      IF(ABS(Y) .LE. DELTA) RETURN 
    2 BB=AA+Y                      
      C1=0.5D0*(AA+BB)             
      C2=C1-AA                     
      S8=0D0                       
      S16=0D0                      
      DO 1 I=1,4                   
      U=X(I)*C2                    
    1 S8=S8+W(I)*(F(C1+U)+F(C1-U)) 
      DO 3 I=5,12                  
      U=X(I)*C2                    
    3 S16=S16+W(I)*(F(C1+U)+F(C1-U))                                  
      S8=S8*C2                     
      S16=S16*C2                   
      IF(EEPS.LT.0D0) THEN         
        IF(ABS(S16-S8) .GT. EPS*ABS(S16)) GO TO 4                     
      ELSE             
        IF(ABS(S16-S8) .GT. EPS) GO TO 4                  
      ENDIF            
      GAUS2=GAUS2+S16    
      AA=BB            
      GO TO 5          
    4 Y=0.5D0*Y        
      IF(ABS(Y) .GT. DELTA) GOTO 2                        
      WRITE(NOUT,7)                          
      GAUS2=0D0                
      RETURN                  
    7 FORMAT(1X,36HGAUS2 ... TOO HIGH ACCURACY REQUIRED)         
      END                     

      FUNCTION GAUS3(F,A,B,EEPS)  
C     *************************   
C THIS IS ITERATIVE INTEGRATION PROCEDURE                             
C ORIGINATES  PROBABLY FROM CERN LIBRARY                              
C IT SUBDIVIDES INEGRATION RANGE UNTIL REQUIRED PRECISION IS REACHED  
C PRECISION IS A DIFFERENCE FROM 8 AND 16 POINT GAUSS ITEGR. RESULT   
C EEPS POSITIVE TREATED AS ABSOLUTE PRECISION                         
C EEPS NEGATIVE TREATED AS RELATIVE PRECISION                         
C     *************************              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      DIMENSION W(12),X(12)        
      COMMON / INOUT  / NINP,NOUT      
      EXTERNAL F                             
      DATA CONST /1.0D-19/     
      save     / INOUT/, CONST, W, X
      DATA W                       
     1/0.10122 85362 90376d0, 0.22238 10344 53374d0, 
     1 0.31370 66458 77887d0, 
     2 0.36268 37833 78362d0, 0.02715 24594 11754d0, 
     2 0.06225 35239 38648d0, 
     3 0.09515 85116 82493d0, 0.12462 89712 55534d0, 
     3 0.14959 59888 16577d0, 
     4 0.16915 65193 95003d0, 0.18260 34150 44924d0, 
     4 0.18945 06104 55069d0/ 
      DATA X                       
     1/0.96028 98564 97536d0, 0.79666 64774 13627d0, 
     1 0.52553 24099 16329d0, 
     2 0.18343 46424 95650d0, 0.98940 09349 91650d0, 
     1 0.94457 50230 73233d0, 
     3 0.86563 12023 87832d0, 0.75540 44083 55003d0, 
     1 0.61787 62444 02644d0, 
     4 0.45801 67776 57227d0, 0.28160 35507 79259d0, 
     1 0.09501 25098 37637d0/ 
      EPS=ABS(EEPS)                
      DELTA=CONST*ABS(A-B)         
      GAUS3=0D0                     
      AA=A                         
    5 Y=B-AA                       
      IF(ABS(Y) .LE. DELTA) RETURN 
    2 BB=AA+Y                      
      C1=0.5D0*(AA+BB)             
      C2=C1-AA                     
      S8=0D0                       
      S16=0D0                      
      DO 1 I=1,4                   
      U=X(I)*C2                    
    1 S8=S8+W(I)*(F(C1+U)+F(C1-U)) 
      DO 3 I=5,12                  
      U=X(I)*C2                    
    3 S16=S16+W(I)*(F(C1+U)+F(C1-U))                                  
      S8=S8*C2                     
      S16=S16*C2                   
      IF(EEPS.LT.0D0) THEN         
        IF(ABS(S16-S8) .GT. EPS*ABS(S16)) GO TO 4                     
      ELSE             
        IF(ABS(S16-S8) .GT. EPS) GO TO 4                  
      ENDIF            
      GAUS3=GAUS3+S16    
      AA=BB            
      GO TO 5          
    4 Y=0.5D0*Y        
      IF(ABS(Y) .GT. DELTA) GOTO 2                        
      WRITE(NOUT,7)                          
      GAUS3=0D0                
      RETURN                  
    7 FORMAT(1X,36HGAUS3 ... TOO HIGH ACCURACY REQUIRED)         
      END                     

C  CORRECTIONS ST. JADACH   (STJ)
C    DOUBLE PRECISION,
C    THIS PROGRAM IS NOT REALY ABLE TO FIND INTEGRAL
C    WITH RELATIVE PRECISION, EPS IS NOW ABSOLUTE ERROR (INPUT ONLY!!)
C.......................................................................
C
C   PURPOSE           - INTEGRATE A FUNCTION F(X)
C   METHOD            - ADAPTIVE GAUSSIAN
C   USAGE             - CALL GADAP(A0,B0,F,EPS,SUM)
C   PARAMETERS  A0    - LOWER LIMIT (INPUT,REAL)
C               B0    - UPPER LIMIT (INPUT,REAL)
C               F     - FUNCTION F(X) TO BE INTEGRATED. MUST BE
C                       SUPPLIED BY THE USER. (INPUT,REAL FUNCTION)
C               EPS   - DESIRED RELATIVE ACCURACY. IF SUM IS SMALL EPS
C                       WILL BE ABSOLUTE ACCURACY INSTEAD. (INPUT,REAL)
C               SUM   - CALCULATED VALUE FOR THE INTEGRAL (OUTPUT,REAL)
C   PRECISION         - DOUBLE
C   REQ'D PROG'S      - F
C   AUTHOR            - THOMAS JOHANSSON, LDC,1973
C   REFERENCE(S)      - THE AUSTRALIAN COMPUTER JOURNAL,3 P.126 AUG. -71
C
C.......................................................................
      SUBROUTINE DGADAP(A0,B0,F,EPS1,SUM)
*     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON/GADAP1/ NUM,IFU
      EXTERNAL F
      DIMENSION A(300),B(300),F1(300),F2(300),F3(300),S(300),N(300)
    1 FORMAT(16H GADAP:I TOO BIG)
      DSUM(F1F,F2F,F3F,AA,BB)=5D0/18D0*(BB-AA)*(F1F+1.6D0*F2F+F3F)

      EPS=EPS1
      IF(EPS.LT.1D-15) EPS=1D-15
      RED=1.3D0
      L=1
      I=1
      SUM=0D0
      C=SQRT(15D0)/5D0
      A(1)=A0
      B(1)=B0
      F1(1)=F(0.5D0*(1D0+C)*A0+0.5D0*(1D0-C)*B0)
      F2(1)=F(0.5D0*(A0+B0))
      F3(1)=F(0.5D0*(1D0-C)*A0+0.5D0*(1D0+C)*B0)
      IFU=3
      S(1)=  DSUM(F1(1),F2(1),F3(1),A0,B0)
  100 CONTINUE
      L=L+1
      N(L)=3
      EPS=EPS*RED
      A(I+1)=A(I)+C*(B(I)-A(I))
      B(I+1)=B(I)
      A(I+2)=A(I)+B(I)-A(I+1)
      B(I+2)=A(I+1)
      A(I+3)=A(I)
      B(I+3)=A(I+2)
      W1=A(I)+(B(I)-A(I))/5D0
      U2=2D0*W1-(A(I)+A(I+2))/2D0
      F1(I+1)=F(A(I)+B(I)-W1)
      F2(I+1)=F3(I)
      F3(I+1)=F(B(I)-A(I+2)+W1)
      F1(I+2)=F(U2)
      F2(I+2)=F2(I)
      F3(I+2)=F(B(I+2)+A(I+2)-U2)
      F1(I+3)=F(A(I)+A(I+2)-W1)
      F2(I+3)=F1(I)
      F3(I+3)=F(W1)
      IFU=IFU+6
      IF(IFU.GT.5000) GOTO 130
      S(I+1)=  DSUM(F1(I+1),F2(I+1),F3(I+1),A(I+1),B(I+1))
      S(I+2)=  DSUM(F1(I+2),F2(I+2),F3(I+2),A(I+2),B(I+2))
      S(I+3)=  DSUM(F1(I+3),F2(I+3),F3(I+3),A(I+3),B(I+3))
      SS=S(I+1)+S(I+2)+S(I+3)
      I=I+3
      IF(I.GT.300)GOTO 120
      SOLD=S(I-3)
*STJ  IF(ABS(SOLD-SS).GT.EPS*(1D0+ABS(SS))/2D0) GOTO 100
      IF(ABS(SOLD-SS).GT.EPS/2D0) GOTO 100
      SUM=SUM+SS
      I=I-4
      N(L)=0
      L=L-1
  110 CONTINUE
      IF(L.EQ.1) GOTO 130
      N(L)=N(L)-1
      EPS=EPS/RED
      IF(N(L).NE.0) GOTO 100
      I=I-1
      L=L-1
      GOTO 110
  120 WRITE(6,1)
 130  CONTINUE
      END

C  CORRECTIONS ST. JADACH   (STJ)
C    DOUBLE PRECISION,
C    THIS PROGRAM IS NOT REALY ABLE TO FIND INTEGRAL
C    WITH RELATIVE PRECISION, EPS IS NOW ABSOLUTE ERROR (INPUT ONLY!!)
C.......................................................................
C
C   PURPOSE           - INTEGRATE A FUNCTION F(X)
C   METHOD            - ADAPTIVE GAUSSIAN
C   USAGE             - CALL GADAP(A0,B0,F,EPS,SUM)
C   PARAMETERS  A0    - LOWER LIMIT (INPUT,REAL)
C               B0    - UPPER LIMIT (INPUT,REAL)
C               F     - FUNCTION F(X) TO BE INTEGRATED. MUST BE
C                       SUPPLIED BY THE USER. (INPUT,REAL FUNCTION)
C               EPS   - DESIRED RELATIVE ACCURACY. IF SUM IS SMALL EPS
C                       WILL BE ABSOLUTE ACCURACY INSTEAD. (INPUT,REAL)
C               SUM   - CALCULATED VALUE FOR THE INTEGRAL (OUTPUT,REAL)
C   PRECISION         - DOUBLE
C   REQ'D PROG'S      - F
C   AUTHOR            - THOMAS JOHANSSON, LDC,1973
C   REFERENCE(S)      - THE AUSTRALIAN COMPUTER JOURNAL,3 P.126 AUG. -71
C
C.......................................................................
      SUBROUTINE DGADA2(A0,B0,F,EPS1,SUM)
*     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON/GADAP1/ NUM,IFU
      EXTERNAL F
      DIMENSION A(300),B(300),F1(300),F2(300),F3(300),S(300),N(300)
    1 FORMAT(16H GADAP:I TOO BIG)
      DSUM(F1F,F2F,F3F,AA,BB)=5D0/18D0*(BB-AA)*(F1F+1.6D0*F2F+F3F)

      EPS=EPS1
      IF(EPS.LT.1D-15) EPS=1D-15
      RED=1.3D0
      L=1
      I=1
      SUM=0D0
      C=SQRT(15D0)/5D0
      A(1)=A0
      B(1)=B0
      F1(1)=F(0.5D0*(1D0+C)*A0+0.5D0*(1D0-C)*B0)
      F2(1)=F(0.5D0*(A0+B0))
      F3(1)=F(0.5D0*(1D0-C)*A0+0.5D0*(1D0+C)*B0)
      IFU=3
      S(1)=  DSUM(F1(1),F2(1),F3(1),A0,B0)
  100 CONTINUE
      L=L+1
      N(L)=3
      EPS=EPS*RED
      A(I+1)=A(I)+C*(B(I)-A(I))
      B(I+1)=B(I)
      A(I+2)=A(I)+B(I)-A(I+1)
      B(I+2)=A(I+1)
      A(I+3)=A(I)
      B(I+3)=A(I+2)
      W1=A(I)+(B(I)-A(I))/5D0
      U2=2D0*W1-(A(I)+A(I+2))/2D0
      F1(I+1)=F(A(I)+B(I)-W1)
      F2(I+1)=F3(I)
      F3(I+1)=F(B(I)-A(I+2)+W1)
      F1(I+2)=F(U2)
      F2(I+2)=F2(I)
      F3(I+2)=F(B(I+2)+A(I+2)-U2)
      F1(I+3)=F(A(I)+A(I+2)-W1)
      F2(I+3)=F1(I)
      F3(I+3)=F(W1)
      IFU=IFU+6
      IF(IFU.GT.5000) GOTO 130
      S(I+1)=  DSUM(F1(I+1),F2(I+1),F3(I+1),A(I+1),B(I+1))
      S(I+2)=  DSUM(F1(I+2),F2(I+2),F3(I+2),A(I+2),B(I+2))
      S(I+3)=  DSUM(F1(I+3),F2(I+3),F3(I+3),A(I+3),B(I+3))
      SS=S(I+1)+S(I+2)+S(I+3)
      I=I+3
      IF(I.GT.300)GOTO 120
      SOLD=S(I-3)
*STJ  IF(ABS(SOLD-SS).GT.EPS*(1D0+ABS(SS))/2D0) GOTO 100
      IF(ABS(SOLD-SS).GT.EPS/2D0) GOTO 100
      SUM=SUM+SS
      I=I-4
      N(L)=0
      L=L-1
  110 CONTINUE
      IF(L.EQ.1) GOTO 130
      N(L)=N(L)-1
      EPS=EPS/RED
      IF(N(L).NE.0) GOTO 100
      I=I-1
      L=L-1
      GOTO 110
  120 WRITE(6,1)
 130  CONTINUE
      END

C  CORRECTIONS ST. JADACH   (STJ)
C    DOUBLE PRECISION,
C    THIS PROGRAM IS NOT REALY ABLE TO FIND INTEGRAL
C    WITH RELATIVE PRECISION, EPS IS NOW ABSOLUTE ERROR (INPUT ONLY!!)
C.......................................................................
C
C   PURPOSE           - INTEGRATE A FUNCTION F(X)
C   METHOD            - ADAPTIVE GAUSSIAN
C   USAGE             - CALL GADAP(A0,B0,F,EPS,SUM)
C   PARAMETERS  A0    - LOWER LIMIT (INPUT,REAL)
C               B0    - UPPER LIMIT (INPUT,REAL)
C               F     - FUNCTION F(X) TO BE INTEGRATED. MUST BE
C                       SUPPLIED BY THE USER. (INPUT,REAL FUNCTION)
C               EPS   - DESIRED RELATIVE ACCURACY. IF SUM IS SMALL EPS
C                       WILL BE ABSOLUTE ACCURACY INSTEAD. (INPUT,REAL)
C               SUM   - CALCULATED VALUE FOR THE INTEGRAL (OUTPUT,REAL)
C   PRECISION         - DOUBLE
C   REQ'D PROG'S      - F
C   AUTHOR            - THOMAS JOHANSSON, LDC,1973
C   REFERENCE(S)      - THE AUSTRALIAN COMPUTER JOURNAL,3 P.126 AUG. -71
C
C.......................................................................
      SUBROUTINE DGADA3(A0,B0,F,EPS1,SUM)
*     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON/GADAP1/ NUM,IFU
      EXTERNAL F
      DIMENSION A(300),B(300),F1(300),F2(300),F3(300),S(300),N(300)
    1 FORMAT(16H GADAP:I TOO BIG)
      DSUM(F1F,F2F,F3F,AA,BB)=5D0/18D0*(BB-AA)*(F1F+1.6D0*F2F+F3F)

      EPS=EPS1
      IF(EPS.LT.1D-15) EPS=1D-15
      RED=1.3D0
      L=1
      I=1
      SUM=0D0
      C=SQRT(15D0)/5D0
      A(1)=A0
      B(1)=B0
      F1(1)=F(0.5D0*(1D0+C)*A0+0.5D0*(1D0-C)*B0)
      F2(1)=F(0.5D0*(A0+B0))
      F3(1)=F(0.5D0*(1D0-C)*A0+0.5D0*(1D0+C)*B0)
      IFU=3
      S(1)=  DSUM(F1(1),F2(1),F3(1),A0,B0)
  100 CONTINUE
      L=L+1
      N(L)=3
      EPS=EPS*RED
      A(I+1)=A(I)+C*(B(I)-A(I))
      B(I+1)=B(I)
      A(I+2)=A(I)+B(I)-A(I+1)
      B(I+2)=A(I+1)
      A(I+3)=A(I)
      B(I+3)=A(I+2)
      W1=A(I)+(B(I)-A(I))/5D0
      U2=2D0*W1-(A(I)+A(I+2))/2D0
      F1(I+1)=F(A(I)+B(I)-W1)
      F2(I+1)=F3(I)
      F3(I+1)=F(B(I)-A(I+2)+W1)
      F1(I+2)=F(U2)
      F2(I+2)=F2(I)
      F3(I+2)=F(B(I+2)+A(I+2)-U2)
      F1(I+3)=F(A(I)+A(I+2)-W1)
      F2(I+3)=F1(I)
      F3(I+3)=F(W1)
      IFU=IFU+6
      IF(IFU.GT.5000) GOTO 130
      S(I+1)=  DSUM(F1(I+1),F2(I+1),F3(I+1),A(I+1),B(I+1))
      S(I+2)=  DSUM(F1(I+2),F2(I+2),F3(I+2),A(I+2),B(I+2))
      S(I+3)=  DSUM(F1(I+3),F2(I+3),F3(I+3),A(I+3),B(I+3))
      SS=S(I+1)+S(I+2)+S(I+3)
      I=I+3
      IF(I.GT.300)GOTO 120
      SOLD=S(I-3)
*STJ  IF(ABS(SOLD-SS).GT.EPS*(1D0+ABS(SS))/2D0) GOTO 100
      IF(ABS(SOLD-SS).GT.EPS/2D0) GOTO 100
      SUM=SUM+SS
      I=I-4
      N(L)=0
      L=L-1
  110 CONTINUE
      IF(L.EQ.1) GOTO 130
      N(L)=N(L)-1
      EPS=EPS/RED
      IF(N(L).NE.0) GOTO 100
      I=I-1
      L=L-1
      GOTO 110
  120 WRITE(6,1)
 130  CONTINUE
      END


      DOUBLE PRECISION FUNCTION DILOGX(X)
C-------------------------------------------- REMARKS ---------------
C DILOGARITHM FUNCTION: DILOG(X)=INT( -LN(1-Z)/Z ) , 0 < Z < X .
C THIS IS THE CERNLIB VERSION.
C--------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Z=-1.644934066848226D0
      IF(X .LT.-1.D0) GO TO 1
      IF(X .LE. 0.5D0) GO TO 2
      IF(X .EQ. 1.D0) GO TO 3
      IF(X .LE. 2.D0) GO TO 4
      Z=3.289868133696453D0
    1 T=1.D0/X
      S=-0.5D0
      Z=Z-0.5D0*DLOG(DABS(X))**2
      GO TO 5
    2 T=X
      S=0.5D0
      Z=0.D0
      GO TO 5
    3 DILOGX=1.644934066848226D0
      RETURN
    4 T=1.D0-X
      S=-0.5D0
      Z=1.644934066848226D0-DLOG(X)*DLOG(DABS(T))
    5 Y=2.666666666666667D0*T+0.666666666666667D0
      B=      0.000000000000001D0
      A=Y*B  +0.000000000000004D0
      B=Y*A-B+0.000000000000011D0
      A=Y*B-A+0.000000000000037D0
      B=Y*A-B+0.000000000000121D0
      A=Y*B-A+0.000000000000398D0
      B=Y*A-B+0.000000000001312D0
      A=Y*B-A+0.000000000004342D0
      B=Y*A-B+0.000000000014437D0
      A=Y*B-A+0.000000000048274D0
      B=Y*A-B+0.000000000162421D0
      A=Y*B-A+0.000000000550291D0
      B=Y*A-B+0.000000001879117D0
      A=Y*B-A+0.000000006474338D0
      B=Y*A-B+0.000000022536705D0
      A=Y*B-A+0.000000079387055D0
      B=Y*A-B+0.000000283575385D0
      A=Y*B-A+0.000001029904264D0
      B=Y*A-B+0.000003816329463D0
      A=Y*B-A+0.000014496300557D0
      B=Y*A-B+0.000056817822718D0
      A=Y*B-A+0.000232002196094D0
      B=Y*A-B+0.001001627496164D0
      A=Y*B-A+0.004686361959447D0
      B=Y*A-B+0.024879322924228D0
      A=Y*B-A+0.166073032927855D0
      A=Y*A-B+1.935064300869969D0
      DILOGX=S*T*(A-B)+Z
      END

      DOUBLE PRECISION FUNCTION DILOGY(X)
C-------------------------------------------- REMARKS ---------------
C DILOGARITHM FUNCTION: DILOG(X)=INT( -LN(1-Z)/Z ) , 0 < Z < X .
C THIS IS THE CERNLIB VERSION.
C--------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Z=-1.644934066848226D0
      IF(X .LT.-1.D0) GO TO 1
      IF(X .LE. 0.5D0) GO TO 2
      IF(X .EQ. 1.D0) GO TO 3
      IF(X .LE. 2.D0) GO TO 4
      Z=3.289868133696453D0
    1 T=1.D0/X
      S=-0.5D0
      Z=Z-0.5D0*DLOG(DABS(X))**2
      GO TO 5
    2 T=X
      S=0.5D0
      Z=0.D0
      GO TO 5
    3 DILOGY=1.644934066848226D0
      RETURN
    4 T=1.D0-X
      S=-0.5D0
      Z=1.644934066848226D0-DLOG(X)*DLOG(DABS(T))
    5 Y=2.666666666666667D0*T+0.666666666666667D0
      B=      0.000000000000001D0
      A=Y*B  +0.000000000000004D0
      B=Y*A-B+0.000000000000011D0
      A=Y*B-A+0.000000000000037D0
      B=Y*A-B+0.000000000000121D0
      A=Y*B-A+0.000000000000398D0
      B=Y*A-B+0.000000000001312D0
      A=Y*B-A+0.000000000004342D0
      B=Y*A-B+0.000000000014437D0
      A=Y*B-A+0.000000000048274D0
      B=Y*A-B+0.000000000162421D0
      A=Y*B-A+0.000000000550291D0
      B=Y*A-B+0.000000001879117D0
      A=Y*B-A+0.000000006474338D0
      B=Y*A-B+0.000000022536705D0
      A=Y*B-A+0.000000079387055D0
      B=Y*A-B+0.000000283575385D0
      A=Y*B-A+0.000001029904264D0
      B=Y*A-B+0.000003816329463D0
      A=Y*B-A+0.000014496300557D0
      B=Y*A-B+0.000056817822718D0
      A=Y*B-A+0.000232002196094D0
      B=Y*A-B+0.001001627496164D0
      A=Y*B-A+0.004686361959447D0
      B=Y*A-B+0.024879322924228D0
      A=Y*B-A+0.166073032927855D0
      A=Y*A-B+1.935064300869969D0
      DILOGY=S*T*(A-B)+Z
      END


      DOUBLE PRECISION FUNCTION DPGAMM(Z)
C     **********************************
C Double precision Gamma function
      DOUBLE PRECISION Z,Z1,X,X1,X2,D1,D2,S1,S2,S3,PI,C(20),CONST
      save C,PI,CONST
      DATA C( 1) / 8.3333333333333333333333333332D-02/
      DATA C( 2) /-2.7777777777777777777777777777D-03/
      DATA C( 3) / 7.9365079365079365079365079364D-04/
      DATA C( 4) /-5.9523809523809523809523809523D-04/
      DATA C( 5) / 8.4175084175084175084175084175D-04/
      DATA C( 6) /-1.9175269175269175269175269175D-03/
      DATA C( 7) / 6.4102564102564102564102564102D-03/
      DATA C( 8) /-2.9550653594771241830065359477D-02/
      DATA C( 9) / 1.7964437236883057316493849001D-01/
      DATA C(10) /-1.3924322169059011164274322169D+00/
      DATA C(11) / 1.3402864044168391994478951001D+01/
      DATA C(12) /-1.5684828462600201730636513245D+02/
      DATA C(13) / 2.1931033333333333333333333333D+03/
      DATA C(14) /-3.6108771253724989357173265219D+04/
      DATA C(15) / 6.9147226885131306710839525077D+05/
      DATA C(16) /-1.5238221539407416192283364959D+07/
      DATA C(17) / 3.8290075139141414141414141414D+08/
      DATA C(18) /-1.0882266035784391089015149165D+10/
      DATA C(19) / 3.4732028376500225225225225224D+11/
      DATA C(20) /-1.2369602142269274454251710349D+13/
      DATA PI    / 3.1415926535897932384626433832D+00/
      DATA CONST / 9.1893853320467274178032973641D-01/
      IF(Z.GT.5.75D 1)                                     GOTO  6666
      NN = Z
      IF (Z  -  DBLE(FLOAT(NN)))                 3,1,3
    1 IF (Z    .LE.    0.D 0)                    GOTO 6667
      DPGAMM = 1.D 0
      IF (Z    .LE.    2.D 0)                    RETURN
      Z1 = Z
    2 Z1 = Z1  -  1.D 0
      DPGAMM = DPGAMM * Z1
      IF (Z1  -  2.D 0)                          61,61,2
    3 IF (DABS(Z)    .LT.    1.D-29)             GOTO 60
      IF (Z    .LT.    0.D 0)                    GOTO 4
      X  = Z
      KK = 1
      GOTO 10
    4 X  = 1.D 0  -  Z
      KK = 2
   10 X1 = X
      IF (X    .GT.    19.D 0)                   GOTO 13
      D1 = X
   11 X1 = X1  +  1.D 0
      IF (X1    .GE.    19.D 0)                  GOTO 12
      D1 = D1 * X1
      GOTO 11
   12 S3 = -DLOG(D1)
      GOTO 14
   13 S3 = 0.D 0
   14 D1 = X1 * X1
      S1 = (X1  -  5.D-1) * DLOG(X1)  -  X1  +  CONST
      DO 20                  K=1,20
      S2 = S1  +  C(K)/X1
      IF (DABS(S2  -  S1)    .LT.    1.D-28)     GOTO 21
      X1 = X1 * D1
   20 S1 = S2
   21 S3 = S3  +  S2
      GOTO (50,22),    KK
   22 D2 = DABS(Z  -  NN)
      D1 = D2 * PI
      IF (D1    .LT.    1.D-15)                  GOTO 31
   30 X2 =  DLOG(PI/DSIN(D1))  -  S3
      GOTO 40
   31 X2 = -DLOG(D2)
   40 MM = DABS(Z)
      IF(X2      .GT.      1.74D2)                         GO TO 6666
      DPGAMM = DEXP(X2)
      IF (MM    .NE.    (MM/2) * 2)              RETURN
      DPGAMM = -DPGAMM
      RETURN
   50 IF(S3      .GT.      1.74D2)                         GO TO 6666
      DPGAMM = DEXP(S3)
      RETURN
 6666 PRINT *, 2000
      RETURN
 6667 PRINT *, 2001
      RETURN
   60 DPGAMM = 0.D 0
      IF(DABS(Z)   .LT.   1.D-77)   RETURN
      DPGAMM = 1.D 0/Z
   61 RETURN
 2000 FORMAT (/////, 2X, 32HDPGAMM ..... ARGUMENT TOO LARGE., /////)
 2001 FORMAT (/////, 2X, 32HDPGAMM ..... ARGUMENT IS A POLE., /////)
      END




C=======================================================================
C=======================================================================
C=======================================================================
C==Received: by dxmint.cern.ch (cernvax) (5.57/3.14)
C== id AA13405; Wed, 23 Jan 91 17:19:06 +0100
C==Message-Id: <9101231619.AA13405@dxmint.cern.ch>
C==Received: by cernapo; Wed, 23 Jan 91 17:23:40 +0100
C==Received: by apojames.cern.ch; Wed, 23 Jan 91 17:05:23 CET
C==Date: Wed, 23 Jan 91 17:05:23 CET
C==From: james@cernapo.cern.ch (Frederick James)
C==To: jadach@cernvm
C==Subject: Random generators
C==
C==      PROGRAM PSEUDORAN
C==C  CPC # ABTK                                           CPC # ABTK
C==C         Pseudorandom generator demonstration (test case)
C==      DIMENSION RVEC(1000)
C==      DIMENSION VERI(5), ISD25(25)
C==C
C==C
C==C   ................................................
C==      WRITE(6,'(20X,A)') 'DEMONSTRATION OF PSEUDORANDOM GENERATORS'
C==      WRITE(6,'(20X,A)') 'MACHINE/SYSTEM: date:'
C==      WRITE(6,'(/20X,A/)') 'INITIALIZATION AND TEST OF PORTABILITY'
C==C   ................................................
C==C
C==C                   initialization and verification  RANMAR
C==        DO 40 I9= 1, 20
C==   40   CALL RANMAR(RVEC,1000)
C==      CALL RANMAR(RVEC,5)
C==      DO 41 I= 1 ,5
C==   41 VERI(I) = (4096.*RVEC(I))*(4096.)
C==      WRITE(6,'(A,5F12.1/)') '  RANMAR 20001  ',VERI
C==C
C==C                   initialization and verification  RANECU
C==      CALL RANECU(RVEC,1000)
C==      CALL RANECU(VERI,5)
C==      DO 52 I= 1 ,5
C==   52 VERI(I) = 4096.*(4096.*VERI(I))
C==      WRITE(6,'(A,5F12.1/)') '  RANECU 1001   ',VERI
C==C
C==C                   initialization and verification  RCARRY
C==      CALL RCARRY(RVEC,1000)
C==      CALL RCARRY(VERI,5)
C==      DO 62 I= 1 ,5
C==   62 VERI(I) = 4096.*(4096.*VERI(I))
C==      WRITE(6,'(A,5F12.1/)') '  RCARRY 1001   ',VERI
C==C
C==      WRITE(6,'(//20X,A/)') 'TEST OF REPEATABILITY'
C==C  .................................................
C==C                  verify restarting      RANMAR
C==      WRITE(6,'(/A)') '   THE NEXT LINE SHOULD BE REPEATED:'
C==      CALL RMARUT(IMAR1,IMAR2,IMAR3)
C==      CALL RANMAR(RVEC,777)
C==      CALL RANMAR(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANMAR 1 ',VERI
C==      CALL RMARIN(IMAR1,IMAR2,IMAR3)
C==      CALL RANMAR(RVEC,777)
C==      CALL RANMAR(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANMAR 2 ',VERI
C==C
C==C                  verify restarting      RANECU
C==      WRITE(6,'(/A)') '   THE NEXT LINE SHOULD BE REPEATED:'
C==      CALL RECUUT(IS1,IS2)
C==      CALL RANECU(RVEC,777)
C==      CALL RANECU(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANECU 1 ',VERI
C==      CALL RECUIN(IS1,IS2)
C==      CALL RANECU(RVEC,777)
C==      CALL RANECU(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANECU 2 ',VERI
C==C
C==C                  verify restarting      RCARRY
C==      WRITE(6,'(/A)') '   THE NEXT LINE SHOULD BE REPEATED:'
C==      CALL RCARUT(ISD25)
C==      CALL RCARRY(RVEC,777)
C==      CALL RCARRY(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RCARRY 1 ',VERI
C==      CALL RCARIN(ISD25)
C==      CALL RCARRY(RVEC,777)
C==      CALL RCARRY(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RCARRY 2 ',VERI
C==C
C==      STOP
C==      END
C=======================================================================
C=======================================================================
C=======================================================================
      SUBROUTINE MARRAN(RVEC,LENV)
C =======================S. JADACH===================================
C == This commes from F. James, The name of RANMAR is changed to   ==
C == MARRAN in order to avoid interference with the version        ==
C == already in use and the public library version (if present).   ==
C ==      THIS IS THE ONLY MODIFICATION !!!!                       ==
C ========================S. JADACH==================================
C Universal random number generator proposed by Marsaglia and Zaman
C in report FSU-SCRI-87-50
C        modified by F. James, 1988 and 1989, to generate a vector
C        of pseudorandom numbers RVEC of length LENV, and to put in
C        the COMMON block everything needed to specify currrent state,
C        and to add input and output entry points MARINI, MAROUT.
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for RANMAR:                                  ++
C!!!      CALL RANMAR (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL MARINI(I1,N1,N2)   initializes the generator from one ++
C!!!                   32-bit integer I1, and number counts N1,N2    ++
C!!!                  (for initializing, set N1=N2=0, but to restart ++
C!!!                    a previously generated sequence, use values  ++
C!!!                    output by MAROUT)                            ++
C!!!      CALL MAROUT(I1,N1,N2)   outputs the value of the original  ++
C!!!                  seed and the two number counts, to be used     ++
C!!!                  for restarting by initializing to I1 and       ++
C!!!                  skipping N2*100000000+N1 numbers.              ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(*)
      PARAMETER (MODCNS=1000000000)
C!!!  COMMON/RASET1/U(97),C,I97,J97
      DIMENSION     U(97)
      SAVE          U    ,C,I97,J97
      SAVE CD, CM, TWOM24, NTOT, NTOT2, IJKL
      DATA NTOT,NTOT2,IJKL/-1,0,0/
C
      IF (NTOT .GE. 0)  GO TO 50
C
C        Default initialization. User has called RANMAR without MARINI.
      IJKL = 54217137
      NTOT = 0
      NTOT2 = 0
      KALLED = 0
      GO TO 1
C
      ENTRY      MARINI(IJKLIN, NTOTIN,NTOT2N)
C         Initializing routine for RANMAR, may be called before
C         generating pseudorandom numbers with RANMAR. The input
C         values should be in the ranges:  0<=IJKLIN<=900 OOO OOO
C                                          0<=NTOTIN<=999 999 999
C                                          0<=NTOT2N<<999 999 999!
C To get the standard values in Marsaglia's paper, IJKLIN=54217137
C                                            NTOTIN,NTOT2N=0
      IJKL = IJKLIN
      NTOT = MAX(NTOTIN,0)
      NTOT2= MAX(NTOT2N,0)
      KALLED = 1
C          always come here to initialize
    1 CONTINUE
      IJ = IJKL/30082
      KL = IJKL - 30082*IJ
      I = MOD(IJ/177, 177) + 2
      J = MOD(IJ, 177)     + 2
      K = MOD(KL/169, 178) + 1
      L = MOD(KL, 169)
      WRITE(6,'(A,5I10)')
     $' MARran INITIALIZED: IJ,KL,IJKL,NTOT,NTOT2=',IJ,KL,IJKL,NTOT,NTOT2
      DO 2 II= 1, 97
      S = 0.
      T = .5
      DO 3 JJ= 1, 24
         M = MOD(MOD(I*J,179)*K, 179)
         I = J
         J = K
         K = M
         L = MOD(53*L+1, 169)
         IF (MOD(L*M,64) .GE. 32)  S = S+T
    3    T = 0.5*T
    2 U(II) = S
      TWOM24 = 1.0
      DO 4 I24= 1, 24
    4 TWOM24 = 0.5*TWOM24
      C  =   362436.*TWOM24
      CD =  7654321.*TWOM24
      CM = 16777213.*TWOM24
      I97 = 97
      J97 = 33
C       Complete initialization by skipping
C            (NTOT2*MODCNS + NTOT) random numbers
      DO 45 LOOP2= 1, NTOT2+1
      NOW = MODCNS
      IF (LOOP2 .EQ. NTOT2+1)  NOW=NTOT
      IF (NOW .GT. 0)  THEN
        WRITE(6,'(A,I15)') ' MARINI SKIPPING OVER ',NOW
       DO 40 IDUM = 1, NTOT
       UNI = U(I97)-U(J97)
       IF (UNI .LT. 0.)  UNI=UNI+1.
       U(I97) = UNI
       I97 = I97-1
       IF (I97 .EQ. 0)  I97=97
       J97 = J97-1
       IF (J97 .EQ. 0)  J97=97
       C = C - CD
       IF (C .LT. 0.)  C=C+CM
   40  CONTINUE
      ENDIF
   45 CONTINUE
      IF (KALLED .EQ. 1)  RETURN
C
C          Normal entry to generate LENV random numbers
   50 CONTINUE
      DO 100 IVEC= 1, LENV
      UNI = U(I97)-U(J97)
      IF (UNI .LT. 0.)  UNI=UNI+1.
      U(I97) = UNI
      I97 = I97-1
      IF (I97 .EQ. 0)  I97=97
      J97 = J97-1
      IF (J97 .EQ. 0)  J97=97
      C = C - CD
      IF (C .LT. 0.)  C=C+CM
      UNI = UNI-C
      IF (UNI .LT. 0.) UNI=UNI+1.
      RVEC(IVEC) = UNI
C             Replace exact zeros by uniform distr. *2**-24
         IF (UNI .EQ. 0.)  THEN
         ZUNI = TWOM24*U(2)
C             An exact zero here is very unlikely, but let's be safe.
         IF (ZUNI .EQ. 0.) ZUNI= TWOM24*TWOM24
         RVEC(IVEC) = ZUNI
         ENDIF
  100 CONTINUE
      NTOT = NTOT + LENV
         IF (NTOT .GE. MODCNS)  THEN
         NTOT2 = NTOT2 + 1
         NTOT = NTOT - MODCNS
         ENDIF
      RETURN
C           Entry to output current status
      ENTRY MAROUT(IJKLUT,NTOTUT,NTOT2T)
      IJKLUT = IJKL
      NTOTUT = NTOT
      NTOT2T = NTOT2
      RETURN
      END
      SUBROUTINE CARRAN(RVEC,LENV)
C         Add-and-carry random number generator proposed by
C         Marsaglia and Zaman in SIAM J. Scientific and Statistical
C             Computing, to appear probably 1990.
C         modified with enhanced initialization by F. James, 1990
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for CARRAN:                                  ++
C!!!      CALL CARRAN (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL CARINI(INT)     initializes the generator from one    ++
C!!!                   32-bit integer INT                            ++
C!!!      CALL CARRES(IVEC)    restarts the generator from vector    ++
C!!!                   IVEC of 25 32-bit integers (see CAROUT)       ++
C!!!      CALL CAROUT(IVEC)    outputs the current values of the 25  ++
C!!!                 32-bit integer seeds, to be used for restarting ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(LENV)
      DIMENSION SEEDS(24), ISEEDS(24), ISDEXT(25)
      PARAMETER (TWOP12=4096.)
      PARAMETER (ITWO24=2**24, ICONS=2147483563)
      SAVE NOTYET, I24, J24, CARRY, SEEDS, TWOM24
      LOGICAL NOTYET
      DATA NOTYET/.TRUE./
      DATA I24,J24,CARRY/24,10,0./
C
C              Default Initialization by Multiplicative Congruential
      IF (NOTYET) THEN
         NOTYET = .FALSE.
         JSEED = 314159265
         WRITE(6,'(A,I12)') ' CARRAN DEFAULT INITIALIZATION: ',JSEED
            TWOM24 = 1.
         DO 25 I= 1, 24
            TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
   25    CONTINUE
         DO 50 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
   50    CONTINUE
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .LT. SEEDS(14)) CARRY = TWOM24
      ENDIF
C
C          The Generator proper: "Subtract-with-borrow",
C          as proposed by Marsaglia and Zaman,
C          Florida State University, March, 1989
C
      DO 100 IVEC= 1, LENV
      UNI = SEEDS(I24) - SEEDS(J24) - CARRY
      IF (UNI .LT. 0.)  THEN
         UNI = UNI + 1.0
         CARRY = TWOM24
      ELSE
         CARRY = 0.
      ENDIF
      SEEDS(I24) = UNI
      I24 = I24 - 1
      IF (I24 .EQ. 0)  I24 = 24
      J24 = J24 - 1
      IF (J24 .EQ. 0)  J24 = 24
      RVEC(IVEC) = UNI
  100 CONTINUE
      RETURN
C           Entry to input and float integer seeds from previous run
      ENTRY CARRES(ISDEXT)
         TWOM24 = 1.
         DO 195 I= 1, 24
  195    TWOM24 = TWOM24 * 0.5
      WRITE(6,'(A)') ' FULL INITIALIZATION OF CARRAN WITH 25 INTEGERS:'
      WRITE(6,'(5X,5I12)') ISDEXT
      DO 200 I= 1, 24
      SEEDS(I) = REAL(ISDEXT(I))*TWOM24
  200 CONTINUE
      CARRY = REAL(MOD(ISDEXT(25),10))*TWOM24
      ISD = ISDEXT(25)/10
      I24 = MOD(ISD,100)
      ISD = ISD/100
      J24 = ISD
      RETURN
C                    Entry to ouput seeds as integers
      ENTRY CAROUT(ISDEXT)
      DO 300 I= 1, 24
         ISDEXT(I) = INT(SEEDS(I)*TWOP12*TWOP12)
  300 CONTINUE
      ICARRY = 0
      IF (CARRY .GT. 0.)  ICARRY = 1
      ISDEXT(25) = 1000*J24 + 10*I24 + ICARRY
      RETURN
C                    Entry to initialize from one integer
      ENTRY CARINI(INSEED)
      JSEED = INSEED
      WRITE(6,'(A,I12)') ' CARRAN INITIALIZED FROM SEED ',INSEED
C      TWOM24 = 1.
         DO 325 I= 1, 24
           TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
  325    CONTINUE
         DO 350 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
  350    CONTINUE
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .LT. SEEDS(14)) CARRY = TWOM24
      RETURN
      END

      SUBROUTINE ECURAN(RVEC,LEN)
C         Random number generator given by L'Ecuyer in
C            Comm. ACM Vol 31, p.742, 1988
C            modified by F. James to return a vector of numbers
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for ECURAN:                                  ++
C!!!      CALL ECURAN (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL ECUINI(I1,I2)    initializes the generator from two   ++
C!!!                   32-bit integers I1 and I2                     ++
C!!!      CALL ECUOUT(I1,I2)    outputs the current values of the    ++
C!!!                   two integer seeds, to be used for restarting  ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(*)
      SAVE ISEED1,ISEED2
      DATA ISEED1,ISEED2 /12345,67890/
C
      DO 100 I= 1, LEN
      K = ISEED1/53668
      ISEED1 = 40014*(ISEED1 - K*53668) - K*12211
      IF (ISEED1 .LT. 0) ISEED1=ISEED1+2147483563
C
      K = ISEED2/52774
      ISEED2 = 40692*(ISEED2 - K*52774) - K* 3791
      IF (ISEED2 .LT. 0) ISEED2=ISEED2+2147483399
C
      IZ = ISEED1 - ISEED2
      IF (IZ .LT. 1)  IZ = IZ + 2147483562
C
      RVEC(I) = REAL(IZ) * 4.656613E-10
  100 CONTINUE
      RETURN
C
      ENTRY ECUINI(IS1,IS2)
      ISEED1 = IS1
      ISEED2 = IS2
      RETURN
C
      ENTRY ECUOUT(IS1,IS2)
      IS1 = ISEED1
      IS2 = ISEED2
      RETURN
      END

      SUBROUTINE VARRAN(DRVEC,LENGT)
C     ***************************
C Switchable random number generator
C Translation to double precision
C     ***************************
      COMMON / RANPAR / KEYRND
      save   / RANPAR /
      DOUBLE PRECISION DRVEC(*)
      DIMENSION RVEC(1000)
      IF(LENGT.LT.1.OR.LENGT.GT.1000) GOTO 901
   10 CONTINUE
      IF(KEYRND.EQ.1) THEN
         CALL RANMAR(RVEC,LENGT)
CBB         CALL MARRAN(RVEC,LENGT)
      ELSEIF(KEYRND.EQ.2) THEN
         CALL ECURAN(RVEC,LENGT)
      ELSEIF(KEYRND.EQ.3) THEN
         CALL CARRAN(RVEC,LENGT)
      ELSE
         GOTO 902
      ENDIF
C random numbers 0 and 1 not accepted
      DO 30 I=1,LENGT
      IF(RVEC(I).LE.0E0.OR.RVEC(I).GE.1E0) THEN
        WRITE(6,*) ' +++++ VARRAN: RVEC=',RVEC(I)
        GOTO 10
      ENDIF
      DRVEC(I)=RVEC(I)
   30 CONTINUE
      RETURN
  901 WRITE(6,*) ' +++++ STOP IN VARRAN: LENGT=',LENGT
      STOP
  902 WRITE(6,*) ' +++++ STOP IN VARRAN: WRONG KEYRND',KEYRND
      STOP
      END

      SUBROUTINE BOSTDQ(MODE,QQ,PP,R)        
C     *******************************        
C BOOST ALONG ARBITRARY AXIS (BY RONALD KLEISS).
C P BOOSTED INTO R  FROM ACTUAL FRAME TO REST FRAME OF Q  
C FORTH (MODE = 1) OR BACK (MODE = -1).      
C Q MUST BE A TIMELIKE, P MAY BE ARBITRARY.  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      PARAMETER ( NOUT =6 )                         
      DIMENSION QQ(*),PP(*),R(*)             
      DIMENSION Q(4),P(4)                    

      DO 10 K=1,4                            
      P(K)=PP(K)                             
   10 Q(K)=QQ(K)                             
      AMQ =DSQRT(Q(4)**2-Q(1)**2-Q(2)**2-Q(3)**2) 
      IF    (MODE.EQ.-1) THEN                
         R(4) = (P(1)*Q(1)+P(2)*Q(2)+P(3)*Q(3)+P(4)*Q(4))/AMQ 
         FAC  = (R(4)+P(4))/(Q(4)+AMQ)       
      ELSEIF(MODE.EQ. 1) THEN                
         R(4) =(-P(1)*Q(1)-P(2)*Q(2)-P(3)*Q(3)+P(4)*Q(4))/AMQ  
         FAC  =-(R(4)+P(4))/(Q(4)+AMQ)       
      ELSE                                   
         WRITE(NOUT,*) ' ++++++++ WRONG MODE IN BOOST3 ' 
         STOP                                
      ENDIF                                  
      R(1)=P(1)+FAC*Q(1)                     
      R(2)=P(2)+FAC*Q(2)                     
      R(3)=P(3)+FAC*Q(3)                     
      END                                    


C BOOST ALONG X AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
      SUBROUTINE BOSTD1(EXE,PVEC,QVEC)
C     ********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION PVEC(4),QVEC(4),RVEC(4)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(1)
      RMI=RVEC(4)-RVEC(1)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(2)=RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(1)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      END

C BOOST ALONG Z AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
      SUBROUTINE BXSTD3(EXE,PVEC,QVEC)
C     ********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION PVEC(4),QVEC(4),RVEC(4)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(3)
      RMI=RVEC(4)-RVEC(3)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(1)=RVEC(1)
      QVEC(2)=RVEC(2)
      QVEC(3)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      END

      SUBROUTINE RXTOD1(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)=RVEC(1)
      QVEC(2)= CS*RVEC(2)-SN*RVEC(3)
      QVEC(3)= SN*RVEC(2)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END

      SUBROUTINE RXTOD2(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)+SN*RVEC(3)
      QVEC(2)=RVEC(2)
      QVEC(3)=-SN*RVEC(1)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END

      SUBROUTINE RXTOD3(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)-SN*RVEC(2)
      QVEC(2)= SN*RVEC(1)+CS*RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(4)=RVEC(4)
      END

      FUNCTION ANGFIX(X,Y)
C     *******************
* CALCULATES ANGLE IN (0,2*PI) RANGE OUT OF X-Y
*     ***********************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA PI /3.1415926535897932D0/

      IF(ABS(Y).LT.ABS(X)) THEN
        THE=ATAN(ABS(Y/X))
        IF(X.LE.0D0) THE=PI-THE
      ELSE
        THE=ACOS(X/SQRT(X**2+Y**2))
      ENDIF
      IF(Y.LT.0D0) THE=2D0*PI-THE
      ANGFIX=THE
      END

      SUBROUTINE DUMPT(NUNIT,WORD,PP)        
C     *******************************        
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)               
      CHARACTER*8 WORD                       
      DOUBLE PRECISION PP(4)                           
      AMS=PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2  
      IF(AMS.GT.0.0) AMS=SQRT(AMS)           
      WRITE(NUNIT,'(1X,A8,5(1X,F13.8))') WORD,(PP(I),I=1,4),AMS 
C====================================================================== 
C================END OF YFSLIB========================================= 
C====================================================================== 
      END 
c......==================== BORN ==========================
      function wwborn_massless(bp1,bp2,bp3,bp4,keyacc_lcl)
*     *******************************************
c This function provides a value of a differential born cross section
c for W+W- pair production and decay in e+e- scattering. 
c INPUT: sprim   - CMS-eff energy squared (in GeV**2)
c        costhe - cosine of the W- polar angle in the CMS
c                 of the incoming e+e- with z-axis pointing 
c                 in the e- direction
c Written by: Wieslaw Placzek            date: 20.07.1994
c Last update: 27.07.1994                by: W.P.
c
!
! wwborn is massless strictly speaking, but it works for massive 
! 4vects as well and the buffor routine wwborn_massive is in fact
! redundant, m.s. 
!
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      common / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf 
      common / wekin2 / amaw,gammw,gmu,alphaw   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
!  br - for normalisation
      COMMON / decdat / amafin(20), br(20)
! polarization amplitudes for WW production for left and right 
! handed electrons
      DOUBLE COMPLEX awwel(3,3),awwer(3,3)
! polarization amplitudes for W-decays
      DOUBLE COMPLEX adwm(3),adwp(3)
! auxilliary
      DOUBLE COMPLEX amwwr, amwwl,BWEXA1,BWEXA2
      dimension bq1(4),bq2(4),bp1(4),bp2(4),bp3(4),bp4(4)
      save   / weking /,/ wekin2 /,/ matpar / 
      save   / decdat /,/ keykey /
      save

      KeySpn = MOD(KeyPhy,10000)/1000
      KeyRed = MOD(KeyPhy,100000)/10000
c      KeyAcc = MOD(KeyMis,1000)/100
      KeyAcc = keyacc_lcl
      KeyWu  = MOD(KeyPhy,1000000)/100000
      Key4f  = MOD(KeyMis,100)/10

! convert linear labels to KoralW.13x convention (mode=1)
! these routines come from the decay.f package !!! 
      CALL store_label(1,label)
      CALL linear_to_WZ_label(1,label,icwm,icwp,ifznow,ifwnow)

      IF(ifwnow.EQ.0) THEN
!-- for ZZ Born is set to 0
!--     except for MIX doubly CKM-non-diagonal, ms 10/7/98
!--     in this case to get cross-section we cheat by 
!--     re-setting icwm & icwp from ZZ to WW values
        IF(icwm.EQ.1.AND.icwp.EQ.4 .OR. icwm.EQ.4.AND.icwp.EQ.1) THEN
!         cdcd
          icwm=2
          icwp=2
        ELSEIF(icwm.EQ.2.AND.icwp.EQ.3 .OR. icwm.EQ.3.AND.icwp.EQ.2)THEN 
!         usus
          icwm=3
          icwp=3
        ELSEIF(icwm.EQ.2.AND.icwp.EQ.5 .OR. icwm.EQ.5.AND.icwp.EQ.2)THEN 
!         ubub
          icwm=5
          icwp=5
        ELSEIF(icwm.EQ.4.AND.icwp.EQ.5 .OR. icwm.EQ.5.AND.icwp.EQ.4)THEN 
!         cbcb
          icwm=6
          icwp=6
        ELSE
!         all others
          wwborn_massless = 0
          RETURN
        ENDIF
      ENDIF

      IF(key4f.EQ.2) THEN
!-- Born suppressed (in case of external Matr el.)
!-- ( to restore ZZ, remember to fix br. ratios - br(icwm).. )
        wwborn_massless=1d0
        IF(ifwnow.EQ.1) THEN
!-- to have non-diag CKM zeroed if requested
          brel=br(7)  
          dfwmwp=br(icwm)*br(icwp)/brel**2
          wwborn_massless=wwborn_massless*dfwmwp
        ENDIF
        RETURN
      ENDIF

      wwborn_massless = 0  

      DO i=1,4
        bq1(i)=bp1(i)+bp2(i)
        bq2(i)=bp3(i)+bp4(i)
      ENDDO
!.. cms-eff mass
      sprim=(bp1(4)+bp2(4)+bp3(4)+bp4(4))**2
!.. resonance masses
      s1=dmas2(bq1)
      s2=dmas2(bq2)
!.. cos theta
      qq=dsqrt(bq1(1)**2+bq1(2)**2+bq1(3)**2)
      costhe=bq1(3)/qq

!! here was a bug A. Vallasi 30.06.96
!ms      wlambd=abs(sprim**2+s1**2+s2**2-2*sprim*s1-2*sprim*s2-2*s1*s2)
      wlambd=max(0d0,abs((sprim-s1-s2)**2 -4*s1*s2))
!      tvar=-(sprim-s1-s2-dsqrt(wlambd)*costhe)/2

      tvar=-1d0/2d0*(dsqrt(wlambd)*(1d0-costhe)
     $              +4d0*s1*s2/(sprim-s1-s2+dsqrt(wlambd)) ) 
!... Amplitudes for WW production
      IF(keyacc.EQ.0) THEN 
        call wwprod(sprim,tvar,bq1,bq2,awwel,awwer) 
      ELSE
        call wwamgc(sprim,tvar,bq1,bq2,awwel,awwer) 
      ENDIF
!... Amplitudes for W-decays
      call wdecay(bq1,bp1,bp2,adwm)  
      call wdecay(bq2,bp3,bp4,adwp) 
!... Exact W-propagators:
      IF(KeyWu.EQ.0) THEN
        BWEXA1=1D0/dcmplx((S1-AMAW**2),(S1/AMAW*GAMMW))
        BWEXA2=1D0/dcmplx((S2-AMAW**2),(S2/AMAW*GAMMW))
      ELSEIF(KeyWu.EQ.1) THEN
        BWEXA1=1D0/dcmplx((S1-AMAW**2),(AMAW*GAMMW))
        BWEXA2=1D0/dcmplx((S2-AMAW**2),(AMAW*GAMMW))
      ELSEIF(KeyWu.EQ.2) THEN
        BWEXA1=1D0/dcmplx((S1-AMAW**2),0D0)
        BWEXA2=1D0/dcmplx((S2-AMAW**2),0D0)
      ELSE
        WRITE(6,*)'BWIGN==> Wrong KeyWu=',keywu
        STOP
      ENDIF


!... Polarization amplitudes for WW production and decay
      if(keyspn.eq.0)then
        xmatr=0
        do 10 l2=1,3
        do 10 l1=1,3

          amwwl=awwel(l1,l2)*adwm(l1)*adwp(l2)*BWEXA1*BWEXA2 !ms,zw
          amwwr=awwer(l1,l2)*adwm(l1)*adwp(l2)*BWEXA1*BWEXA2 !ms,zw
          xmatr=xmatr + amwwl*dconjg(amwwl) !ms
          xmatr=xmatr + amwwr*dconjg(amwwr) !ms
 10     continue
      elseif(keyspn.eq.1)then
        xmatr=0
        amwwl=(0,0)
        amwwr=(0,0)
        do 20 l2=1,3
        do 20 l1=1,3
          amwwl=amwwl+awwel(l1,l2)*adwm(l1)*adwp(l2)*BWEXA1*BWEXA2 !ms,zw
          amwwr=amwwr+awwer(l1,l2)*adwm(l1)*adwp(l2)*BWEXA1*BWEXA2 !ms,zw
 20     continue
        xmatr=xmatr + amwwl*dconjg(amwwl) !ms
        xmatr=xmatr + amwwr*dconjg(amwwr) !ms
      endif
      fkin=1D0
      wwborn_massless=fkin*xmatr 
! include normalisation due to branching ratios (various WW channels)
! 10/7/98 ms      IF(ifwnow.EQ.1) THEN
        brel=br(7)  
        dfwmwp=br(icwm)*br(icwp)/brel**2
        wwborn_massless=wwborn_massless*dfwmwp
! 10/7/98 ms      ENDIF
! include spin average 1/4
      wwborn_massless=wwborn_massless/4d0

      end

      subroutine wwprod(s,t,q1,q2,awwel,awwer)
*     ****************************************
c This routine calculates polarization amplitudes for the process
c e+e- --> W+W-, for on-shell W's in Born approximation. Calculation
c is done in the CMS of e+e- with z-axis pointing along e- direction. 
c It is based on the paper: 
c K. Kolodziej, M. Zralek, Phys. Rev. D43 (1991) 43;
c INPUT: s   - center mass energy squared (in GeV**2)
c        t   - transfer (in GeV**2)
c        q1(4) - four-momentum of W- 
c        q2(4) - four-momentum of W+
c OUTPUT: awwel(3,3) - complex array containing polarization amplitudes 
c                     for left-handed electron 
c                     {M_0(-,+,l1,l2) in eq. (31)}
c         awwel(3,3) - complex array containing polarization amplitudes 
c                     for right-handed electron 
c                     {M_0(+,-,l1,l2) in eq. (31)}
c
c Written by: Wieslaw Placzek            date: 01.07.1994
c Last update: 02.08.1994                by: W.P.
c
      implicit DOUBLE PRECISION (a-h,o-z)
      DOUBLE PRECISION q1(4),q2(4)
      DOUBLE COMPLEX awwel(3,3),awwer(3,3)
      common / matpar / pi,ceuler     
      common / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf 
      common / wekin2 / amaw,gammw,gmu,alphaw   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! polarization vectors of W-bosons, eq. (9)
      DOUBLE PRECISION eps1(4,3),eps2(4,3)
      DOUBLE PRECISION e1(4),e2(4),p1(4),pmq(4)
      DOUBLE COMPLEX f1e1,f1e2,f1q2,fkz3
      data init /0/
      save init,zetl,etal,etar
      save
!
!...initialization
!--------------------------------------------------------------------- 
      if (init.eq.0) then
        init=1 
        KeyZet = MOD(KeyPhy,1000)/100
!... electroweak coefficient factors, eq. (30)
        zetl=0.5d0/sinw2
        etal=1-zetl
        etar=1
      endif  
!... calculation
!=====================================================================
!... four-momentum of the incoming electron in CMS 
!    (+z axis along the electron beam)
      ecm=dsqrt(s)   
      p1(1)=0d0
      p1(2)=0d0
      p1(3)=ecm/2d0
      p1(4)=p1(3)
!... calculation of polarization vectors of W-bosons, eq. (9)
      call polvec(q1,eps1)
      call polvec(q2,eps2)
!... calculation of the polarization amplitudes, eq. (31)
!    {note: we use different normalization!}
!    (note: electric charge squared equals: 4*pi*alphaw}
      wsp=-4*pi*alphaw*ecm
      do 10 k=1,4
 10   pmq(k)=p1(k)-q1(k)
      do 20 l2=1,3
      do 20 l1=1,3
        do 25 k=1,4
          e1(k)=eps1(k,l1)
 25       e2(k)=eps2(k,l2)
        f1e1=dcmplx(e1(1),-e1(2))
        f1e2=dcmplx(e2(1),-e2(2))
        f1q2=dcmplx(q2(1),-q2(2))
        e1e2=prodm(e1,e2)
        e1q2=prodm(e1,q2)
        e2q1=prodm(e2,q1) 
        if(keyzet.eq.0)then
        awwel(l1,l2)=( 2*(1/s - etal/dcmplx(s-amaz**2,s/amaz*gammz))*
     &                   (e1q2*f1e2 - e1e2*f1q2 - e2q1*f1e1) + 
     &                 zetl/t*fkz3(e2,pmq,e1) ) *wsp   
        awwer(l1,l2)= -2*(1/s - etar/dcmplx(s-amaz**2,s/amaz*gammz))*
     &                   (e1q2*conjg(f1e2) - e1e2*conjg(f1q2) -
     &                    e2q1*conjg(f1e1)) *wsp
        elseif(keyzet.eq.1)then
        awwel(l1,l2)=( 2*(1/s - etal/dcmplx(s-amaz**2,amaz*gammz))*
     &                   (e1q2*f1e2 - e1e2*f1q2 - e2q1*f1e1) + 
     &                 zetl/t*fkz3(e2,pmq,e1) ) *wsp   
        awwer(l1,l2)= -2*(1/s - etar/dcmplx(s-amaz**2,amaz*gammz))*
     &                   (e1q2*conjg(f1e2) - e1e2*conjg(f1q2) -
     &                    e2q1*conjg(f1e1)) *wsp
        elseif(keyzet.eq.2)then
        awwel(l1,l2)=( 2*(1/s - etal/(s-amaz**2))*
     &                   (e1q2*f1e2 - e1e2*f1q2 - e2q1*f1e1) + 
     &                 zetl/t*fkz3(e2,pmq,e1) ) *wsp   
        awwer(l1,l2)= -2*(1/s - etar/(s-amaz**2))*
     &                   (e1q2*conjg(f1e2) - e1e2*conjg(f1q2) -
     &                    e2q1*conjg(f1e1)) *wsp
        else
          write(6,*)'wrong KEYZET:',keyzet
        endif
 20   continue
      end  

      function prodm(p,q)
*     *******************
c Scalar product of the four-vectors p and q in Minkowski space;
c note: p_0=p(4), q_0=q(4)
c 
      implicit DOUBLE PRECISION (a-h,o-z)
      DOUBLE PRECISION p(4),q(4)
      prodm=p(4)*q(4)-p(3)*q(3)-p(2)*q(2)-p(1)*q(1)
      end

      subroutine polvec(q,eps)
*     **************************
c Calculation of polarization vectors of a vector boson 
c in the rectangular basis, see eq. (9); see also K. Hagiwara 
c and D. Zeppenfeld, Nucl. Phys. B274 (1986) 1, eq. (3.47).
c     INPUT:  q(4)     - four-momentum of the vector boson
c     OUTPUT: eps(4,3) - three polarization four-vector
c
c Written by: Wieslaw Placzek            date: 01.07.1994
c Last update: 27.07.1994                by: W.P.
c
      implicit DOUBLE PRECISION (a-h,o-z)
      DOUBLE PRECISION q(4),eps(4,3)   
      save
!
      qt2=q(1)**2+q(2)**2
      qt =sqrt(qt2)
      aq2=qt2+q(3)**2
      aq =sqrt(aq2)
      am =sqrt(q(4)**2-aq2) 
      do 10 l=1,3
      do 10 k=1,4
   10 eps(k,l)=0
      if(aq.lt.1d-10) then
        eps(1,1)=1
        eps(2,2)=1
        if (am.gt.1d-10) eps(3,3)=1 
      elseif (qt2.lt.1d-10) then
        eps(1,1)=q(3)/aq
        eps(2,2)=q(3)/aq
        if (am.gt.1d-10) then
          eps(3,3)=q(4)/am/aq*q(3)
          eps(4,3)=aq/am
        endif
      else
        ws1=1/aq/qt
        eps(1,1)= ws1*q(1)*q(3)
        eps(2,1)= ws1*q(2)*q(3)
        eps(3,1)=-ws1*qt2
        eps(4,1)= 0
        ws2=1/qt 
        eps(1,2)=-ws2*q(2)
        eps(2,2)= ws2*q(1)
        eps(3,2)= 0
        eps(4,2)= 0
        if (am.gt.1d-10) then
          ws3=q(4)/am/aq 
          eps(1,3)=ws3*q(1)
          eps(2,3)=ws3*q(2)
          eps(3,3)=ws3*q(3)
          eps(4,3)=ws3*aq2/q(4)
        endif
      endif  
      end

      function fkz3(a,b,c)
*     *******************************
c Function F_3 of four-vectors contracted with Dirac matrices; 
c see eq. (19)
c
c Written by: Wieslaw Placzek            date: 01.07.1994
c
      DOUBLE PRECISION a(4),b(4),c(4)
      DOUBLE COMPLEX fkz3
      fkz3=(a(4) +a(3)) * ((b(4) -b(3))*dcmplx(c(1),-c(2)) -
     &                     dcmplx(b(1),-b(2))*(c(4) -c(3)))+ 
     &     dcmplx(a(1),-a(2)) * ((b(4) +b(3))*(c(4) -c(3)) -
     &                     dcmplx(b(1), b(2))*dcmplx(c(1),-c(2)))
      end        

      subroutine wdecay(q,p1,p2,adw)
*     ******************************
c This routine calculates polarization amplitudes for W decays 
c into massless fermions. It is based on the paper: 
c K. Hagiwara et al., Nucl. Phys. B282 (1987) 253; see Appendix C.
c No CKM-mixing matrix elements incuded here.
c INPUT: q(4)        - four-momentum of W  
c        p1(4),p2(4) - four-momenta of decay products
c OUTPUT: adw(3) - complex array containing W decay amplitudes 
c                   {M(lambda,sigma_1,sigma_2) in eq. (C.16)}
c
c Written by: Wieslaw Placzek            date: 20.07.1994
c Last update: 02.08.1994                by: W.P.
c
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler
      common / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf 
      common / wekin2 / amaw,gammw,gmu,alphaw   
      save / matpar /, / weking /, / wekin2 /
      DOUBLE PRECISION q(4),p1(4),p2(4)
      DOUBLE COMPLEX adw(3)
      DOUBLE PRECISION eps(4,3),e(4)
      DOUBLE COMPLEX sfunhz
!
!... calculation of polarization vectors of W in rectangular basis
      call polvec(q,eps)
!... calculation of the W decay amplitudes
      do 10 l=1,3
        do 15 k=1,4
 15       e(k)=eps(k,l)
        adw(l)=  sqrt(4*pi*alphaw) /sqrt(2*sinw2)
     $        *2*sqrt(p1(4)*p2(4)) *sfunhz(p1,e,p2)
 10   continue
      end

      function sfunhz(p1,a,p2)
*     ********************************** *
c Spinorial string S(pi,a,pf) for massless spinors chi(pi), chi(pf);
c a(4) - given four-vector, 
c see K. Hagiwara et al., Nucl. Phys. B282 (1987) 253; Appendix C. 
c
c Written by: Wieslaw Placzek            date: 20.07.1994
c Last update: 01.08.1994                by: W.P.
c
      implicit DOUBLE PRECISION (a-h,o-z)
      DOUBLE PRECISION a(4),p1(4),p2(4)
      DOUBLE COMPLEX z1p,z2m,zam,zap
      DOUBLE COMPLEX sfunhz
!
      x1p=p1(3)+p1(4)
      x2p=p2(3)+p2(4)
      z1p=dcmplx(p1(1), p1(2))
      z2m=dcmplx(p2(1),-p2(2))
      xam=a(4)-a(3)
      xap=a(4)+a(3)
      zam=dcmplx(a(1),-a(2))
      zap=dcmplx(a(1), a(2))
      if (x1p.gt.1d-20 .and. x2p.gt.1d-20) then
        fac=0.5/sqrt(p1(4)*p2(4)*x1p*x2p)
        sfunhz=fac *( x1p*(x2p*xam-z2m*zap) + z1p*(z2m*xap-x2p*zam) )
      elseif (x1p.gt.1d-20) then
        sfunhz= (x1p*zap - z1p*xap)/sqrt(2*p1(4)*x1p)
      elseif (x2p.gt.1d-20) then
        sfunhz= (z2m*xap - x2p*zam)/sqrt(2*p2(4)*x2p)
      else
        sfunhz=xap
      endif
      end

!======================================================================
!============= Born Version with Anomalous Couplings ==================
!======================================================================

      subroutine WWamgc(s,t,q1,q2,awwel,awwer)
*     ****************************************
!----------------------------------------------------------------------!
! This routine calculates polarization amplitudes for the process:     !
!              e-(p1) e+(p2) ---> W-(q1) W+(q2)                        !
! Calculation  is done in the CMS of e+e- with z-axis pointing along   !
! the e- direction. These amplitudes include general type three boson  !
! coupling constant as given in the paper:                             !
!     K. Hagiwara, R.D. Peccei, D. Zeppenfeld and K. Hikasa,           !
!                 Nucl. Phys. B282 (1987) 253.                         !
! The same formalism as in the subroutine wwprod (where only the SM    !
! coupling constant are included) is used here.                        !
! INPUT: s   - center mass energy squared (in GeV**2)                  !
!        t   - transfer momentum squared (in GeV**2)                   !
!        q1(4) - four-momentum of W-                                   !
!        q2(4) - four-momentum of W+                                   !
! OUTPUT: awwel(3,3) - complex array containing polarization           !
!                      amplitudes for left-handed electron,            !
!                      M_0(-,+,l1,l2);                                 !
!         awwel(3,3) - complex array containing polarization           ! 
!                      amplitudes for right-handed electron,           ! 
!                      M_0(+,-,l1,l2).                                 !
! Note: Before first use of this routine general type coupling         !
!       constant have to be set up in the routine setacc.              !
!----------------------------------------------------------------------! 
! Written by: Wieslaw Placzek                 Knoxville, November 1995 !
! Last update: 16.11.1995            by: W.P.                          !
!----------------------------------------------------------------------!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DOUBLE COMPLEX zi
      PARAMETER ( zi = (0d0,1d0) )
      DOUBLE PRECISION q1(4),q2(4)
      DOUBLE COMPLEX awwel(3,3),awwer(3,3)
      COMMON / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf 
      COMMON / wekin2 / amaw,gammw,gmu,alphaw 
      common / matpar / pi,ceuler  
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      SAVE / weking /, / wekin2 /, / KeyKey /, / matpar /
! Polarization vectors of W-bosons          
      DOUBLE PRECISION eps1(4,3),eps2(4,3)
      DOUBLE PRECISION e1(4),e2(4),p1(4),pmq(4)
! General type 3-boson coupling constants
      DOUBLE COMPLEX GamV(4,2)
      DOUBLE COMPLEX dZ,Gagm,Gagp,GaZm,GaZp,fkz3
      DATA init /0/
      SAVE init,KeyZet,zetl,etal,etar
!--------------------------------------------------------------------- 
! Initialization ...
      IF (init.eq.0) THEN
        init=1 
        KeyZet = MOD(KeyPhy,1000)/100
! Electroweak coefficient factors
        zetl=0.5/sinw2
        etal=1-zetl
        etar=1
! Set up anomaluos couplings constants
!!        CALL setacc ! MOVED TO FILEXP
      ENDIF  
!--------------------------------------------------------------------- 
! Calculation ...
! Options for Z-boson width
! a) running Z-width
      IF (KeyZet.eq.0) THEN
        dZ = DCMPLX(s-amaz**2,s/amaz*gammz)
! b) constant Z-width
      ELSEIF (KeyZet.eq.1) THEN
        dZ = DCMPLX(s-amaz**2,amaz*gammz)
! c) zero Z-width
      ELSEIF (KeyZet.eq.2) THEN
        dZ = s-amaz**2
      ELSE
         WRITE(6,*)'>>> Wrong KeyZet:',KeyZet
      ENDIF
! Four-momentum of the incoming electron in CMS (+z axis along e-)
      p1(1) = 0
      p1(2) = 0
      p1(3) = SQRT(s)/2
      p1(4) = p1(3)
! Calculation of W polarization vectors
      CALL polvec(q1,eps1)
      CALL polvec(q2,eps2)
      DO k = 1,4
        pmq(k) = p1(k) - q1(k)
      ENDDO
      wsp =-4*pi*alphaw*SQRT(s)
! Calculation of the polarization amplitudes
      DO l2 = 1,3
        DO l1 = 1,3
          DO k = 1,4
            e1(k)=eps1(k,l1)
            e2(k)=eps2(k,l2)
          ENDDO
! Calculation of the 3-boson couplings
          CALL WWVgcc(s,amaw,q1,e1,q2,e2,GamV)
! WWgamma vertex
          Gagm = GamV(1,1) - zi*GamV(2,1)
          Gagp = GamV(1,1) + zi*GamV(2,1)          
! WWZ vertex
          GaZm = GamV(1,2) - zi*GamV(2,2)
          GaZp = GamV(1,2) + zi*GamV(2,2) 
! Polarization amplitudes         
          awwel(l1,l2) = wsp*( Gagm/s - GaZm*etal/dZ 
     &                       + zetl/t *fkz3(e2,pmq,e1) )
          awwer(l1,l2) = wsp*( Gagp/s - GaZp*etar/dZ )
        ENDDO
      ENDDO
      END

      subroutine WWVgcc(s,amW,q1,eps1,q2,eps2,GamV)
*     *********************************************
!----------------------------------------------------------------------!
! This routine calculates four-vector Gamma_V^mu including general     !
! type 3-boson WWV couplings (V=gamma,Z) as defined in the paper:      !
!     K. Hagiwara, R.D. Peccei, D. Zeppenfeld and K. Hikasa,           !
!                 Nucl. Phys. B282 (1987) 253.                         !
! INPUT: s       - center mass energy squared (in GeV**2)              !
!        amW     - W-boson mass                                        !
!        q1(4)   - four-momentum of W-                                 !
!        eps1(4) - polarizarion vector of W-                           !
!        q2(4)   - four-momentum of W+                                 !
!        eps1(4) - polarizarion vector of W+                           !
! OUTPUT: GamV(4,2) - 2 complex number four-vectors Gamma_V^mu:        !
!                     GamV(4,1) for WWgamma vertex,                    !
!                     GamV(4,2) for WWZ vertex.                        !
!----------------------------------------------------------------------! 
! Written by: Wieslaw Placzek                 Knoxville, November 1995 !
! Last update: 15.11.1995            by: W.P.                          !
!----------------------------------------------------------------------!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DOUBLE COMPLEX zi
      PARAMETER ( zi = (0d0,1d0) )
      DOUBLE PRECISION q1(4),eps1(4),q2(4),eps2(4)
      DOUBLE COMPLEX GamV(4,2)
      COMMON / ancoco / g1(2),kap(2),lam(2),g4(2),g5(2),kapt(2),lamt(2)
      DOUBLE COMPLEX g1,kap,lam,g4,g5,kapt,lamt
      SAVE  / ancoco /
! 7 form factors f_i^V
      DOUBLE COMPLEX f1(2),f2(2),f3(2),f4(2),f5(2),f6(2),f7(2)
      DOUBLE PRECISION P(4),Q(4),Peps(4),Qeps(4)
!
      amW2 = amW**2
! Set up form factors
      DO i = 1,2
         f1(i) = g1(i) + s/(2*amW2) *lam(i)
         f2(i) = lam(i)
         f3(i) = g1(i) + kap(i) + lam(i)
         f4(i) = g4(i)
         f5(i) = g5(i)
         f6(i) = kapt(i) - lamt(i)
         f7(i) = -0.5*lamt(i)
      ENDDO
! Sum and difference of W's 4-momenta
      DO k = 1,4
         P(k) = q1(k) + q2(k)
         Q(k) = q1(k) - q2(k)
      ENDDO
! Scalar products of various 4-vectors
      e1e2 = prodm(eps1,eps2)
      Pe1  = prodm(P,eps1)
      Pe2  = prodm(P,eps2)
! Coefficients for f5 and f6 
      CALL epsabc(P,eps1,eps2,Peps)
      CALL epsabc(Q,eps1,eps2,Qeps)
! Coefficient for f7
      PQeps = prodm(P,Qeps)
! Calculate Gamma_V^mu
      DO i = 1,2
        DO k = 1,4
          GamV(k,i) = ( f1(i)*e1e2 - f2(i)/amW2*Pe1*Pe2 
     &                - f7(i)/amW2*PQeps )*Q(k)
     &              + (-f3(i) + zi*f4(i) )*Pe2*eps1(k)   
     &              + ( f3(i) + zi*f4(i) )*Pe1*eps2(k)   
     &              + zi*f5(i)*Qeps(k) - f6(i)*Peps(k)
        ENDDO
      ENDDO
      END

      subroutine epsabc(a,b,c,q)
*     **************************
!----------------------------------------------------------------------!
! This routine calculates four-vector q according to the formula:      !
!                                                                      !
!  q^mu = epsilon^{mu,alpha,beta,gamma} a_alpha b_beta c_gamma,        !
!                                                                      !
! where epsilon is a totally antisymmetric tensor in Bjorken & Drell   !
! convention, and a, b, c are four-vectors.                            !
!----------------------------------------------------------------------! 
! Written by: Wieslaw Placzek                 Knoxville, November 1995 !
! Last update: 16.11.1995            by: W.P.                          !
!----------------------------------------------------------------------!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DOUBLE PRECISION a(4),b(4),c(4),q(4)
!
      b1c2 = b(1)*c(2) - b(2)*c(1)
      b1c4 = b(1)*c(4) - b(4)*c(1)
      b2c3 = b(2)*c(3) - b(3)*c(2)
      b2c4 = b(2)*c(4) - b(4)*c(2)
      b3c1 = b(3)*c(1) - b(1)*c(3)
      b3c4 = b(3)*c(4) - b(4)*c(3)
! 4-vector q^mu
      q(1) = a(2)*b3c4 - a(3)*b2c4 + a(4)*b2c3
      q(2) =-a(1)*b3c4 + a(3)*b1c4 + a(4)*b3c1
      q(3) = a(1)*b2c4 - a(2)*b1c4 + a(4)*b1c2  
      q(4) = a(1)*b2c3 + a(2)*b3c1 + a(3)*b1c2
      END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! UNUSED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine setacc_UNUSED
*     *****************
!----------------------------------------------------------------------!
! Setting up the anomalous couplings constants as given in the paper:  !
!     K. Hagiwara, R.D. Peccei, D. Zeppenfeld and K. Hikasa,           !
!                 Nucl. Phys. B282 (1987) 253.                         !
! Note: This subroutine has to be called prior first use of the        !
! subroutine WWamgc.                                                   !
! The variables used in this routine correspond to the following       !
! contants defined in the above paper:                                 !
!           constant name     corresponding variable                   ! 
!                g_1^V                g1(2)                            !
!                kappa_V              kap(2)                           !
!                lambda_V             lam(2)                           !
!                g_4^V                g4(2)                            !
!                g_5^V                g5(2)                            !
!                kappa-tilde_V        kapt(2)                          !
!                lambda-tilde_V       lamt(2)                          ! 
!----------------------------------------------------------------------! 
! Written by: Wieslaw Placzek                 Knoxville, November 1995 !
! Last update: 14.11.1995            by: W.P.                          !
!----------------------------------------------------------------------!
      COMMON / ancoco / g1(2),kap(2),lam(2),g4(2),g5(2),kapt(2),lamt(2)
      DOUBLE COMPLEX g1,kap,lam,g4,g5,kapt,lamt
      SAVE  / ancoco /
! Set up the constants (within SM: g1=kap=1, lam=g4=g5=kapt=lamt=0):
! 1) for WWgamma vertex
      g1(1)   = (1.0, 0.0)
      kap(1)  = (1.0, 0.0)
      lam(1)  = (0.0, 0.0)
      g4(1)   = (0.0, 0.0)
      g5(1)   = (0.0, 0.0)
      kapt(1) = (0.0, 0.0)
      lamt(1) = (0.0, 0.0)
! 2) for WWZ vertex
      g1(2)   = (1.0, 0.0)
      kap(2)  = (1.0, 0.0)
      lam(2)  = (0.0, 0.0)
      g4(2)   = (0.0, 0.0)
      g5(2)   = (0.0, 0.0)
      kapt(2) = (0.0, 0.0)
      lamt(2) = (0.0, 0.0)
      END

      subroutine kinold_unused(bp1,bp2,bp3,bp4,
     $  s1,s2,costhe,phi,cosde1,phi1,cosde2,phi2)
!  zw 17.06.96 wrong arguments were used   ctn,fin,ct1n,fi1n,ct2n,fi2n)
! this routine sets back principal angular variables for matrix element.
! matrix element is not calculated so far from born-like 4-momenta, 
! but from these angles. 
      implicit DOUBLE PRECISION (a-h,o-z)
      dimension bp1(4),bp2(4),bp3(4),bp4(4)

      call invkin(ctn,fin,ct1n,fi1n,ct2n,fi2n,
     $                  amwmn,amwpn,  bp1,bp2,bp3,bp4)
      s1=amwmn**2
      s2=amwpn**2
      costhe=ctn
      phi=fin
      cosde1=ct1n
! zw 17.06.96 next line was phi2.
      phi1=fi1n
      cosde2=ct2n
      phi2=fi2n

      end

      function bornex(s)
*     *********************************
! exact, on-shell born(s)
      implicit DOUBLE PRECISION (a-h,o-z)
      common / matpar / pi,ceuler     
      common / phypar / alfinv,gpicob     
      common / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf 
      common / wekin2 / amaw,gammw,gmu,alphaw   
      save   / weking /,/ wekin2 /
      save
   
      IF(s.le.4*amaw**2) THEN
        bornex=0d0
        return
      ENDIF
       
      ams= amaw**2/s
      bet=sqrt(1-4*ams)
      cc1=amaz**2*(1-2*sinw2)/(s-amaz**2)
      cc2=amaz**4*(8*sinw2**2-4*sinw2+1)*bet**2/(s-amaz**2)**2/48d0

      sigma=  (1+2*ams+2*ams**2)/bet*log((1+bet)/(1-bet)) -5d0/4d0
     @       +cc1*( 2*(ams**2+2*ams)/bet*log((1+bet)/(1-bet))
     @             -1/ams/12d0 -5d0/3d0 -ams )
     @       +cc2*( 1/ams**2 +20/ams +12)

      bornex=pi/alfinv**2*bet/2d0/sinw2**2/s *gpicob
      end


      FUNCTION BORNKD_unused(ep1,ep2,ep3,ep4)
*******************************************************
! ep-i are massive 4momenta of produced fermions in cms-eff
! buffor routine, in order to include additional effects in born
! (QCD, Coulomb) as well as total normalisation
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      common / phypar / alfinv,gpicob     

      dimension ep1(4),ep2(4),ep3(4),ep4(4)
      dimension bq1(4),bq2(4)

!      born=wwborn_massive(ep1,ep2,ep3,ep4)
! wwborn is massless strictly speaking, but it works for massive 
! 4vects as well and the buffor routine wwborn_massive is in fact
! redundant, m.s. 

      born=wwborn(ep1,ep2,ep3,ep4,keyac)

! invariants for normalisation
      DO i=1,4
        bq1(i)=ep1(i)+ep2(i)
        bq2(i)=ep3(i)+ep4(i)
      ENDDO

cc      s1=dmas2(bq1)
cc      s2=dmas2(bq2)
      sprim=(bq1(4)+bq2(4))**2

C now include born level flux factor1/2s', spin average factor 1/4
      BORN=1D0/(2D0*SPRIM)*(1D0/4D0)*BORN
! picobarns
      born=born*gpicob
!-- Coulomb corr.
c moved to karlud 11/7      cc=CulMC(sprim,s1,s2)
c moved to karlud 11/7      BORN=BORN*CC
!-- Naive QCD to be added here.....

      bornkd=born
      END 


      FUNCTION WWBORN(ep1,ep2,ep3,ep4,keyacc_lcl)
*********************************************************
! ep-i are 4momenta of produced fermions in cms-eff
! this routine converts massive 4vects into massless ones 
! and calls the massless WWBORN
! also sets born to 1 on request (key4f=2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      SAVE /keykey/           

      dimension ep1(4),ep2(4),ep3(4),ep4(4)
      dimension boq1(4),boq2(4),bop1(4),bop2(4),bop3(4),bop4(4)
      dimension amdec0(4)
      dimension qeff1(4),qeff2(4)
      dimension bq1(4),bq2(4)

      KeyRed = MOD(KeyPhy,100000)/10000
      Key4f  = MOD(KeyMis,100)/10
      idebug=0

      DO i=1,4
        bq1(i)=ep1(i)+ep2(i)
        bq2(i)=ep3(i)+ep4(i)
      ENDDO

      IF(key4f.EQ.2) THEN
!     1=================1
!-- Born suppressed (in case of external Matr el.)
        IF(idebug.eq.1) THEN
          write(6,*)'born_massive suppressed, keyred=',keyred
        ENDIF
        born=1d0
      ELSE
!     1=================1
!-- standard matrix element squared
        IF(idebug.eq.1) THEN
          write(6,*)'born_massive dumps before, keyred=',keyred
          write(6,*)'born_massive dumps before, sq(sprim)=',sqrt(sprim)
          call dumpl(6,eP1,eP2,eP3,eP4,QEFF1,QEFF2,SPHOT,nphot)
        ENDIF
c.. make decay products massless
        IF(KeyRed.eq.0) THEN
!       2==================2
c.. sophisticated fermion mass reduction to 0
! re-construct angles
          call invkin(costhe,phi,cosde1,phi1,cosde2,phi2,
     $                  amwmn,amwpn,  ep1,ep2,ep3,ep4)

! make sure it is OK
          qq=dsqrt(bq1(1)**2+bq1(2)**2+bq1(3)**2)
          ctn=bq1(3)/qq
          IF(abs(ctn/costhe-1) .GT. 1d-14) THEN
            WRITE(6,*)'cosinusy ',ctn/costhe
          ENDIF
          sprim=(bq1(4)+bq2(4))**2
          
          DO i=1,4
            amdec0(i)=0d0
          ENDDO
! build again 4vects with 0 masses
          call kineww(sprim,costhe,phi,cosde1,phi1,cosde2,phi2,
     $    amwmn,amwpn,amdec0,boq1,boq2,bop1,bop2,bop3,bop4)
        ELSEIF(KeyRed.eq.1) THEN
!       2======================2
c.. brute force fermion mass reduction to 0, no 4 mom conserv.
          bp1mod=ep1(4)/dsqrt(ep1(1)**2+ep1(2)**2+ep1(3)**2)
          bp2mod=ep2(4)/dsqrt(ep2(1)**2+ep2(2)**2+ep2(3)**2)
          bp3mod=ep3(4)/dsqrt(ep3(1)**2+ep3(2)**2+ep3(3)**2)
          bp4mod=ep4(4)/dsqrt(ep4(1)**2+ep4(2)**2+ep4(3)**2)
          do i=1,3
            bop1(i)=ep1(i)*bp1mod !*(1-1d-15)
            bop2(i)=ep2(i)*bp2mod !*(1-1d-15)
            bop3(i)=ep3(i)*bp3mod !*(1-1d-15)
            bop4(i)=ep4(i)*bp4mod !*(1-1d-15)
          enddo
          bop1(4)=ep1(4)
          bop2(4)=ep2(4)
          bop3(4)=ep3(4)
          bop4(4)=ep4(4)
        ELSEIF(KeyRed.eq.2) THEN
!       2======================2
c.. NO reduction at all...
          DO i=1,4
            bop1(i)=ep1(i)
            bop2(i)=ep2(i)
            bop3(i)=ep3(i)
            bop4(i)=ep4(i)
          ENDDO
        ELSE
!       2================2
          write(6,*)'born_massive==>wrong KEYRED:',keyred
          stop
        ENDIF
!       2================2
        IF(idebug.eq.1) THEN
          write(6,*)'born_massive dumps after'
          call dumpl(6,boP1,boP2,boP3,boP4,QEFF1,QEFF2,SPHOT,nphot)
          write(6,*)'masses',sqrt(dmas2(bop1)),sqrt(dmas2(bop2)),
     $                       sqrt(dmas2(bop3)),sqrt(dmas2(bop4))
        ENDIF
        BORN= WWBORN_massless(bop1,bop2,bop3,bop4,keyacc_lcl)
      ENDIF
!     1=================1

      wwborn=born
      END 



      FUNCTION culmc(s,s1,s2)
!     **************************
! Coulomb effect from Fadin, Khoze, Martin, Stirling, dtp/95/64
! first order, eq. 9

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  

      COMMON / MATPAR / pi,ceuler     
      COMMON / PHYPAR / alfinv,gpicob     
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp

      KeyCul = MOD(KeyRad,10000)/1000

      if(keycul.eq.0) then
        culmc = 1d0
      elseif(keycul.eq.1) then
        pp = 1/(4*s) *( s**2 -2*s*(s1+s2) +(s1-s2)**2 )
        ppp= 1/(4*s) *((s-s1-s2)**2-4d0*s1*s2)
        if(pp.lt.0.) pp=ppp
        p  = dsqrt(pp)
        en = (s-4*amaw**2)/(4*amaw)
        ddee = dsqrt(en**2+gammw**2)
        p1 = dsqrt( amaw/2d0 *( ddee -en ) )
        p2 = dsqrt( amaw/2d0 *( ddee +en ) )
        dabskap2 = amaw *ddee
        drekap  =  p1
        dimkap  = -p2

        ff = 1 +sqrt(s)/(4*p*alfinv) 
     $           *( pi -2*datan( (dabskap2 -pp)/(2*p*drekap) ) )   

        culmc = ff -1  ! <========!!!!!!!!!
        culmc = ff
      else
        write(6,*) ' culMC==> wrong keycul=',keycul
        stop
      endif
      end

      SUBROUTINE eexx_wt_cor(wtcrud,svar,amel,iflav,vvmin,wtcort)
*     ***********************************************************   
*===================================================================*
*             NOTE: This is a DUMMY ROUTINE !!!                     *
* The real one is still under development and can be obtained from  *
* the authors only on a special request.                            *
*===================================================================*
!-------------------------------------------------------------------!
! The t-channel radiation correction weight for eexx final states   !
! with hard high PT radiative photons.                              !
! INPUT: wtcrud - "crude" weight                                    !
!        svar  - CMS energy squared                                 !
!        amel  - electron mass                                      !
!        iflav - final state particles flavours (PDG)               !
!        vvmin   - soft photon cut-off in CMS (min: E_gamma/E_beam) !
! OUTPUT: wtcort - correction weight                                !
!-------------------------------------------------------------------!
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot
      COMMON / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      COMMON / cms_eff_momdec /
     $      effb1(4),effb2(4),p1e(4),p2e(4),p3e(4),p4e(4)
      SAVE / momset /, / momdec /, / cms_eff_momdec /

      DIMENSION iflav(4)

*! DUMMY DUMMY DUMMY ... 

*! Total correction weight 
      wtcort = 1d0
      END
      SUBROUTINE betar(alfinv,wtborn,svar,amel,nphot,sphot,wtset)
!     *****************************************************************
! This routine defines weights for ISR QED matrix element up to O(alf3)
! to be implemented on top of basic distribution from karlud    
!     ***************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z) 
      PARAMETER( pi=3.1415926535897932d0)
      DIMENSION sphot(100,4),wtset(100)
      SAVE 

      DIMENSION bt10(100),bt11(100)
      DIMENSION bt20(100,100),bt21(100,100)
! ------------------ Inline functions ------------------
! Elements of single bremss. distribution
      chi(x)= (1d0+(1d0-x)**2)/2d0
      xni(x)= x*(-1+x/2d0 )
!
      DO i = 1,100
        wtset(i) = 0d0
      ENDDO
      IF (wtborn.EQ.0d0) RETURN

      beta00=0d0
      beta01=0d0
      beta02=0d0
      beta03=0d0

      beta10=0d0
      beta11=0d0
      beta12=0d0

      beta20=0d0
      beta21=0d0

      beta30=0d0

      gami=  2d0/alfinv/pi*(dlog(svar/amel**2)-1) 
!-- beta0
      beta00 = 1d0
      beta01 = 1d0 + gami/2d0     
      beta02 = 1d0 + gami/2d0 + gami**2/8d0 
      beta03 = 1d0 + gami/2d0 + gami**2/8d0 +gami**3/48d0

!-- beta1
!-- Contributions from beta1 
      ene=sqrt(svar/4d0)
      DO  jph=1,nphot
      CALL d_isr1(ene,gami,sphot,jph,dis10,dis11,dis12)
         vv=sphot(jph,4)/ene
         zz=1-vv
*     O(alf1) tree
         bt10(jph) = dis10 -beta00
         beta10 = beta10 +bt10(jph)
*     O(alf2) one loop
         bt11(jph) = dis11 -beta01
         beta11 = beta11 +bt11(jph)
*     O(alf3) two loop
         bt12   = dis12 -beta02
         beta12 = beta12 +bt12
      ENDDO

!-- beta2
      DO j2=2,nphot
         DO j1=1,j2-1
            dis20 = 0d0         !initialization
            dis21 = 0d0         !initialization
            CALL d_isr2(ene,gami,sphot,j1,j2,dis20,dis21)
            CALL d_isr2(ene,gami,sphot,j2,j1,dis20,dis21)
* O(alf2) Tree level
            bt20(j1,j2)  = dis20 -bt10(j1) -bt10(j2) -beta00
            beta20 = beta20 +bt20(j1,j2)  
* O(alf3) One loop level
            bt21(j1,j2)  = dis21 -bt11(j1) -bt11(j2) -beta01
            beta21 = beta21 +bt21(j1,j2)  
         ENDDO
      ENDDO

!-- beta3
* O(alf3) Tree level
         DO j3 = 3,nphot
            DO j2 = 2,j3-1
               DO j1 = 1,j2-1
                  dis30 = 0d0   !initialization
*     Sum over 6 fragmentation trees
                  CALL d_isr3(ene,gami,sphot,j1,j2,j3,dis30)
                  CALL d_isr3(ene,gami,sphot,j2,j1,j3,dis30)
                  CALL d_isr3(ene,gami,sphot,j1,j3,j2,dis30)
                  CALL d_isr3(ene,gami,sphot,j2,j3,j1,dis30)
                  CALL d_isr3(ene,gami,sphot,j3,j1,j2,dis30)
                  CALL d_isr3(ene,gami,sphot,j3,j2,j1,dis30)
                  bt30 = dis30
     $                 -beta00
     $                 -bt10(j1) -bt10(j2) -bt10(j3)
     $                 -bt20(j1,j2) -bt20(j1,j3) -bt20(j2,j3)
                  beta30 = beta30+bt30
               ENDDO
         ENDDO
      ENDDO

      wtx0=beta00
      wtx1=beta01 +beta10
      wtx2=beta02 +beta11 + beta20
      wtx3=beta03 +beta12 + beta21+ beta30
* Totals
      wtset(1) =wtborn*wtx0
      wtset(2) =wtborn*wtx1
      wtset(3) =wtborn*wtx2
      wtset(4) =wtborn*wtx3
* Betas
      wtset(10)=wtborn*beta00 ! O(alf0)
      wtset(11)=wtborn*beta01 !   O(alf1)
      wtset(12)=wtborn*beta10 !   O(alf1)
      wtset(13)=wtborn*beta02 ! O(alf2)
      wtset(14)=wtborn*beta11 ! O(alf2)
      wtset(15)=wtborn*beta20 ! O(alf2)
      wtset(16)=wtborn*beta03 !   O(alf3)
      wtset(17)=wtborn*beta12 !   O(alf3)
      wtset(18)=wtborn*beta21 !   O(alf3)
      wtset(19)=wtborn*beta30 !   O(alf3)

      END

      SUBROUTINE d_isr1(ene,gami,sphot,j1,dis10,dis11,dis12)
*     ******************************************************
*     ***********************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION sphot(100,4)
* inline functions
      chi2(a,b)=  0.5d0*   ((1d0-a)**2+(1d0-b)**2)
*
      a1 = (sphot(j1,4)-sphot(j1,3))/ene/2d0
      b1 = (sphot(j1,4)+sphot(j1,3))/ene/2d0
      zz = (1d0-a1)*(1d0-b1)
      IF(zz  .LE. 0d0) WRITE(*,*) '!!!! zz=',zz
      dels1 =  gami/2d0 -gami/4d0*dlog(zz)
      dels2 =  gami**2/8d0
     $        -gami**2/8d0  *dlog(zz)
     $        +gami**2/24d0 *dlog(zz)**2
* Exact O(alf1) matrix element for the hardest photon jhard
      dis10  = chi2(a1,b1)
      dis11  = dis10*(1+dels1)
      dis12  = dis10*(1+dels1+dels2)
      END


      SUBROUTINE d_isr2(ene,gami,sphot,j1,j2,dis20,dis21)
*     **************************************************
* dis20,dis21 has to be initialized in the calling program
*     ***********************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION sphot(100,4)
* inline functions
      chi2(a,b)=  0.5d0* ((1d0-a)**2+(1d0-b)**2)
*
      ntree = 2                 ! for 2 ISR fragmentation trees
      y1 = (sphot(j1,4)-sphot(j1,3))/ene/2d0
      z1 = (sphot(j1,4)+sphot(j1,3))/ene/2d0
      y2 = (sphot(j2,4)-sphot(j2,3))/ene/2d0
      z2 = (sphot(j2,4)+sphot(j2,3))/ene/2d0
      a1 = y1
      b1 = z1
      a2 = y2/(1d0-y1)
      b2 = z2/(1d0-z1)
      d20 = chi2(a1,b1)*chi2(a2,b2)/ntree
      zz1 =  (1-y1)*(1-z1)
      z1z2= (1-y1-y2)*(1-z1-z2)
* soft limit to d_isr1 OK! for 2 trees we get 3 terms gami/6d0*dlog(zz)
      delvir1 = gami/2d0 -gami/6d0*dlog(zz1) -gami/6d0*dlog(z1z2)
      dis20 = dis20 +d20
      dis21 = dis21 +d20*(1+delvir1)

      IF(z1  .le.0d0) WRITE(*,*) '!!!! z1=',z1
      IF(z1z2.le.0d0) WRITE(*,*) '!!!! z1z2=',z1z2

      END


      SUBROUTINE d_isr3(ene,gami,sphot,j1,j2,j3,dis30)
*     ************************************************
*     dis30 has to be initialized in the calling program
*     ***********************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DIMENSION sphot(100,4)
* inline functions
      chi2(a,b)=  0.5d0*   ((1d0-a)**2+(1d0-b)**2)
*
      ntree = 6                 ! for 6 ISR fragmentation trees
      y1 = (sphot(j1,4)-sphot(j1,3))/ene/2d0
      z1 = (sphot(j1,4)+sphot(j1,3))/ene/2d0
      y2 = (sphot(j2,4)-sphot(j2,3))/ene/2d0
      z2 = (sphot(j2,4)+sphot(j2,3))/ene/2d0
      y3 = (sphot(j3,4)-sphot(j3,3))/ene/2d0
      z3 = (sphot(j3,4)+sphot(j3,3))/ene/2d0
      a1 = y1
      b1 = z1
      a2 = y2/(1d0-y1)
      b2 = z2/(1d0-z1)
      a3 = y3/(1d0-y2-y1)
      b3 = z3/(1d0-z2-z1)

      d30= chi2(a1,b1) *chi2(a2,b2) *chi2(a3,b3)/ntree

      dis30= dis30 +d30

      IF(a2  .GT. 1d0) WRITE(*,*) '!!!! a2=',a2
      IF(b2  .GT. 1d0) WRITE(*,*) '!!!! b2=',b2
      IF(a3  .GT. 1d0) WRITE(*,*) '!!!! a3=',a3
      IF(b3  .GT. 1d0) WRITE(*,*) '!!!! b3=',b3

      END

**************************************************************
! !!!! OBSOLETE !!!!! !!!! OBSOLETE !!!!! !!!! OBSOLETE !!!!
! !!!! OBSOLETE !!!!! !!!! OBSOLETE !!!!! !!!! OBSOLETE !!!!
**************************************************************
      SUBROUTINE betax(alfinv,wtborn,svar,amel,nphot,sphot,wtset)
!     *****************************************************************
! O(alf2)LL weights for the beta0, beta1 and beta2,
! to be implemented on top of basic distribution from karlud    
!     ***************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      PARAMETER( pi=3.1415926535897932d0)
      DIMENSION sphot(100,4),wtset(100)
      SAVE 
! ------------------ Inline functions ------------------
! Elements of single bremss. distribution
      chi(x)= (1d0+(1d0-x)**2)/2d0
      xni(x)= x*(-1+x/2d0 )

      beta00=0d0
      beta01=0d0
      beta02=0d0
      beta10=0d0
      beta11=0d0
      beta20=0d0

      gami=  2d0/alfinv/pi*(dlog(svar/amel**2)-1) 
!-- beta0
      beta00 = 1d0
      beta01 = 1d0 + gami/2d0     
      beta02 = 1d0 + gami/2d0 + gami**2/8d0 

!-- beta1
!-- Contributions from beta1            
      ene=sqrt(svar/4d0)    
      DO  jph=1,nphot 
         vv=sphot(jph,4)/ene            
         b10 = xni(vv)
         b11 = xni(vv)*(1d0 +gami/2d0)
     $        -chi(vv)*(gami/4d0)*dlog(1d0-vv)
         beta10  =  beta10 +b10             
         beta11  =  beta11 +b11             
      ENDDO

!-- beta2
      DO i=1,nphot
         DO j=i+1,nphot
            v1=sphot(i,4)/ene
            v2=sphot(j,4)/ene
            v1st = v1/(1-v2)
            v2st = v2/(1-v1)
            IF ( sphot(i,3)*sphot(j,3) .LT. 0d0) THEN
!           OPPOSITE directions two photons
               dis2= chi(v1)*chi(v2)
            ELSE
!           SAME directions two photons
               dis2= 0.5d0*( chi(v1)*chi(v2st) + chi(v1st)*chi(v2) )
            ENDIF
            beta1i = xni(v1)
            beta1j = xni(v2)
            bt20  = dis2 -beta1i -beta1j -beta00
            beta20 = beta20 +bt20  
         ENDDO
      ENDDO

      wtx0=beta00
      wtx1=beta01 +beta10
      wtx2=beta02 +beta11 + beta20
* Totals OLD version
      wtset(5) =wtborn*wtx0
      wtset(6) =wtborn*wtx1
      wtset(7) =wtborn*wtx2

      END


      SUBROUTINE AMPINW(XPAR,NPAR)
c **********************
C Initialization of the external 4fermion matrix el. codes
C For the moment only GRACE will be interfaced
c **********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION  XPAR ( *),NPAR ( *)   
      DIMENSION  XPARY(100),NPARY(100)   
      COMMON / INOUT  / NINP,NOUT 
      COMMON / BXFMTS / BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      Key4f  = MOD(KeyMis,100)/10
      if (key4f.ne.0) then
        WRITE(NOUT,BXOPE) 
        WRITE(NOUT,BXTXT) '      Window H used only by  Grace 2.0  '
        WRITE(NOUT,BXTXT) '          Higgs  boson parameters       '
        WRITE(NOUT,BXL1F)xpar(11),'xpar(11)= higgs mass ','amh','H1'
        WRITE(NOUT,BXL1F)xpar(12),'xpar(12)= higgs width','agh','H2'
        WRITE(NOUT,BXTXT) '                                        '
        WRITE(NOUT,BXCLO)
      endif
      DO I=1,100
        XPARY(I)=XPAR(I)
        NPARY(I)=NPAR(I)
      ENDDO

      CALL AMPINI(XPARY,NPARY)
      END
C
      SUBROUTINE AMPEXT(WTMOD4F,WT4F )
! *****************************************************
! external 4fermion matrix elements calculations
! BUFFOR routine
! OUTPUTS
!   wtmod4f        - principal weight for rejection
!   wt4f(9)        - auxiliary weights wector
! *****************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT
      COMMON / MOMDEC / Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
      COMMON / DECAYS / IFLAV(4), AMDEC(4)
!!!      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON / cms_eff_MOMDEC / 
     $      effbeam1(4),effbeam2(4),effP1(4),effP2(4),effP3(4),effP4(4)
      save   /cms_eff_momdec/   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      SAVE   / DECAYS /,/ MOMDEC /,/ KeyKey/
      DIMENSION XQ1(4),XQ2(4),XP1(4),XP2(4),XP3(4),XP4(4)      
      DIMENSION WT4F(9),WT(9)
      I1 = 11
      I2 =-11
      IF1=IFLAV(1)
      IF2=IFLAV(2)
      IF3=IFLAV(3)
      IF4=IFLAV(4)

      key_cms_eff = 1

      IF(key_cms_eff.EQ.0) THEN
! Ext. Matr. el. calculated in CMS 
! function elmatr will break down in this case !!!!!
       DO I=1,4
          XQ1(I)=QEFF1(I)
          XQ2(I)=QEFF2(I)
          XP1(I)=P1(I)
          XP2(I)=P2(I)
          XP3(I)=P3(I)
          XP4(I)=P4(I)
        ENDDO
      ELSE
! Ext. Matr. el. calculated in EFFECTIVE CMS 
        DO I=1,4
          XQ1(I)=EFFbeam1(I)
          XQ2(I)=EFFbeam2(I)
          XP1(I)=effP1(I)
          XP2(I)=effP2(I)
          XP3(I)=effP3(I)
          XP4(I)=effP4(I)
        ENDDO
      ENDIF

      CALL AMP4F(XQ1,I1,XQ2,I2,XP1,IF1,XP2,IF2,XP3,IF3,XP4,IF4
     $          , WTMOD,WT )
C
      DO I4F=1,9
        WT4F(I4F) = WT(I4F)
      ENDDO

      WTMOD4F = WTMOD
      END


      FUNCTION ELMATRunsd(imode,p1,p2,p3,p4,iflav1,iflav2,iflav3,iflav4)
!     ***************************************************************
!  plain matrix el. as of koralw BUT with modified normalisation 
!  according to the IMODE parameter
!  imode = 1  : GRACE
! NOTE,
! p-i momenta MUST be in their CMS frame with e- beam into z+ direction

      implicit double precision (a-h,o-z)   

      DIMENSION P1(4),P2(4),P3(4),P4(4)

      elmatr=wwborn(p1,p2,p3,p4,keyac)
ccc      elmatr=wwborn_massive(p1,p2,p3,p4)
ccc      elmatr=bornkd(p1,p2,p3,p4)

      end

      SUBROUTINE MASOW(SIN2W,GPICB,AMAF)
C     **********************************
C buffor routine: transmits to the external library 
C information from KORALW commons. 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / PHYPAR / ALFINV,GPICOB  
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON / DECDAT / AMAFIN(20), BR(20)
      SAVE   / WEKIN2 /,/ DECDAT /
      DIMENSION AMAF(20)
      SIN2W=SINW2
      GPICB = GPICOB
      DO I=1,20
       AMAF(I)=AMAFIN(I)
      ENDDO
      END

      SUBROUTINE 
     $   KWPAR2(XAMAW,XAMAZ,XGAMMW,XGAMMZ,XSINW2)
************************************************************************
! buffor routine, like MASOW
************************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!************ KORALW stuff ******************
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF  
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! TAUOLA, PHOTOS and JETSET overall switches
      COMMON / LIBRA  / JAK1,JAK2,ITDKRC,IFPHOT,IFHADM,IFHADP

      COMMON / DECDAT / AMAFIN(20), BR(20)
      COMMON / INOUT  / NINP,NOUT     
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB  

!************ end KORALW stuff ******************

      XAMAW = AMAW
      XAMAZ = AMAZ
      XGAMMW = GAMMW
      XGAMMZ = GAMMZ
      XSINW2 = SINW2

      END
      SUBROUTINE TOHEP
      DOUBLE PRECISION  QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4)
      DOUBLE PRECISION Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
      INTEGER NPHOT
      COMMON / MOMSET / QEFF1,QEFF2,SPHUM,SPHOT,NPHOT
      COMMON / MOMDEC / Q1,Q2,P1,P2,P3,P4
      DOUBLE PRECISION ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2
      INTEGER IDE,IDF
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON /TAUPOS/ NP1,NP2                
      COMMON /PHOACT/ IFPHOT
      COMMON /TRALID/ idtra
      INTEGER IFLAV(4)
      REAL*4    POL0(4),POL1(4),POL2(4)
      DOUBLE PRECISION  AMDEC(4)
      COMMON / DECAYS / IFLAV, AMDEC
C
      REAL*4 APH(4),XPB1(4),XPB2(4),AQF1(4),AQF2(4),XAMD(4),AM
      REAL*4 XP1(4),XP2(4),XP3(4),XP4(4)

! to switch tau polarization OFF in taus from W- (W+) decays set 
!     DATA  POL1 /0.0,0.0,0.0,0.0/
!     DATA  POL2 /0.0,0.0,0.0,0.0/
! note that taus from Z decays are unpolarized, 10/7/98

      DATA  POL0 /0.0,0.0, 0.0,0.0/
      DATA  POL1 /0.0,0.0,-1.0,0.0/
      DATA  POL2 /0.0,0.0,-1.0,0.0/


      SAVE
C
      DO K=1,4
       AQF1(K)=Q1(K)
       AQF2(K)=Q2(K)
       XP1(K) =P1(K) 
       XP2(K) =P2(K) 
       XP3(K) =P3(K) 
       XP4(K) =P4(K) 
C
       XAMD(K)=AMDEC(K)
      ENDDO

C initial state (1,2)
      AM=AMEL
! e- (0 0 +1 1)
      XPB1(1) = 0d0
      XPB1(2) = 0d0
      XPB1(3) = dsqrt(ene**2 -am**2) !e- (00+11)
      XPB1(4) = ene
! e+ (0 0 -1 1)
      XPB2(1) = 0d0
      XPB2(2) = 0d0
      XPB2(3) =-xpb1(3) !e+ (00-11)
      XPB2(4) = ene

      CALL FILHEP(1,3, 11,0,0,0,0,XPB1,AM,.TRUE.)
      CALL FILHEP(2,3,-11,0,0,0,0,XPB2,AM,.TRUE.)
C primary final state W-W+ (3,4)
      IF (IFLAV(1).eq.-iflav(2)) then
       idwm= 23
       idwp= 23
      else
       idwm=-24
       idwp= 24       
      endif
      AM=sqrt(Q1(4)**2-Q1(3)**2-Q1(2)**2-Q1(1)**2)
      CALL FILHEP(3,2,idwm,1,2,0,0,AQF1,AM,.TRUE.)
      AM=sqrt(Q2(4)**2-Q2(3)**2-Q2(2)**2-Q2(1)**2)
      CALL FILHEP(4,2,idwp,1,2,0,0,AQF2,AM,.TRUE.)
C radiative photons (5 ... 4+NPHOT) (PDG-code for gamma is 22)
      IF(NPHOT.NE.0) THEN
        IP=0
        DO I=1,NPHOT
          DO J=1,4
            APH(J)=SPHOT(I,J)
          END DO
          IF (APH(4).GT.0.0) THEN
            IP=IP+1
            CALL FILHEP(4+IP,1,22,1,2,0,0,APH,0.0,.TRUE.)
          END IF
       END DO
      END IF
C decay products W- (5,6)+NPHOT
      CALL FILHEP(0,1,IFLAV(1),3,3,0,0,XP1,XAMD(1),.TRUE.)
      CALL FILHEP(0,1,IFLAV(2),3,3,0,0,XP2,XAMD(2),.TRUE.)
C decay products W+ (7,8)+NPHOT
      CALL FILHEP(0,1,IFLAV(3),4,4,0,0,XP3,XAMD(3),.TRUE.)
      CALL FILHEP(0,1,IFLAV(4),4,4,0,0,XP4,XAMD(4),.TRUE.)
C

C tau decays:
      if (abs(iflav(1)).eq.15.and.abs(iflav(2)).eq.16) then
          KTO=2
          idtra=1
!          write(*,*) 'id=',idtra
          NP2=5+NPHOT
          CALL DEXAY(KTO,POL2)
 !         write(*,*) '==='
      endif
      if (abs(iflav(4)).eq.15.and.abs(iflav(3)).eq.16) then
          KTO=1
          idtra=4
          NP1=8+NPHOT
          CALL DEXAY(KTO,POL1)
      endif
      if (abs(iflav(1)).eq.15.and.abs(iflav(2)).eq.15) then
          KTO=2
          idtra=1
          NP2=5+NPHOT
          CALL DEXAY(KTO,POL0)
          KTO=1
          idtra=2
          NP1=6+NPHOT
          CALL DEXAY(KTO,POL0)
      endif
      if (abs(iflav(4)).eq.15.and.abs(iflav(3)).eq.15) then
          KTO=2
          idtra=3
          NP2=7+NPHOT
          CALL DEXAY(KTO,POL0)
          KTO=1
          idtra=4
          NP1=8+NPHOT
          CALL DEXAY(KTO,POL0)
      endif
C radiate photons for  leptonic W decays.
        IF (IFPHOT.EQ.1) THEN
          if (abs(iflav(1)).gt.10) CALL PHOTOS(3)
          if (abs(iflav(3)).gt.10) CALL PHOTOS(4)
        ENDIF
      END 
C      SUBROUTINE tohad(ifhadm,ifhadp,PReco)
C! This is modified koralw/interfaces/lundface
C! * S. Jadach Jan. 1997
C! * yet modified by W. Placzek, 7 Apr. 1998
C! It implements some kind of simple color reconnection.
C! It is activated when the input parameter PReco > 0.
C! PReco - colour reconnection probability (to be supplied be the user).
C      IMPLICIT NONE
C      INTEGER ifhadm,ifhadp
C! Colour Re-Connection probability
C      DOUBLE PRECISION PReco
C
C      INTEGER iflav(4)
C      DOUBLE PRECISION   amdec(4)
C      COMMON / decays / iflav, amdec
C
C! This common can be everywhere, contains various switches
C      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
C      INTEGER            KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
C
C      REAL*4 PHEP,VHEP
C      INTEGER NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
C      PARAMETER (NMXHEP=2000)
C      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
C     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C      INTEGER ijoin(2)
C      INTEGER id1,id2,ireco,key4f
C      REAL*4 sqrs1,sqrs2
C      DOUBLE PRECISION drvec(1)
C      SAVE
C!
C! lets first go to LUND and later hadronize
C!
C      CALL luhepc(2)
C      DO  id1=5,nhep
C         IF (idhep(id1).NE.22) THEN
C            GOTO 8
C         ENDIF
C      ENDDO
C 8    CONTINUE
C      DO  id2=id1+2,nhep
C         IF (idhep(id2).NE.22) THEN
C            GOTO 18
C         ENDIF
C      ENDDO
C 18   CONTINUE
C!--------------------------
C      IF (ifhadm .NE. 0 .OR. ifhadp .NE. 0) THEN
C         ireco=0
C         Key4f  = MOD(KeyMis,100)/10
C         IF (key4f.ne.0) THEN 
C! routine spdetx can make a random choice of two colour recombination
C! pattern either WW or ZZ like. 
C! It is necessary only for uudd ccss f.s.
C            IF ( (abs(iflav(1)).eq.1).AND.(abs(iflav(4)).eq.1)
C     $           .AND.(abs(iflav(2)).eq.2).and.(abs(iflav(3)).eq.2) 
C     $           )  CALL spdetx(ireco)
C            IF ( (abs(iflav(1)).eq.3).AND.(abs(iflav(4)).eq.3)
C     $           .AND.(abs(iflav(2)).eq.4).and.(abs(iflav(3)).eq.4) 
C     $           )  CALL spdetx(ireco)
C         ENDIF
C!-------------------------Colour REconnection START
C! Primitive ansatz, S.J. 29Dec96
C! Corrected by W. Placzek, 7Apr98
C! WP: Do "colour reconnection" only if 4 quarks in the final state!
C         IF (abs(iflav(1)).LT.10 .AND. abs(iflav(2)).LT.10 .AND.
C     $       abs(iflav(3)).LT.10 .AND. abs(iflav(4)).LT.10) THEN
C
C            CALL varran(drvec,1)
C            IF( drvec(1) .LT. PReco ) THEN
C               IF(ireco.eq.0) THEN
C                  ireco=1
C               ELSE
C                  ireco=0
C               ENDIF
C            ENDIF
C
C         ENDIF
C!-------------------------Colour REconnection END
C         IF(ireco .EQ. 0) then
C            IF (abs(iflav(1)) .LT. 10) then
C               ijoin(1) = ID1
C               ijoin(2) = ID1+1 
C               call lujoin(2,ijoin)
C            ENDIF
C            IF (ABS(iflav(3)) .LT. 10) THEN
C               ijoin(1) = ID2
C               ijoin(2) = ID2+1 
C               CALL lujoin(2,ijoin)
C            ENDIF
C!     That's it, shower and hadronize now
C            sqrs1=phep(5,jmohep(1,id1))
C            IF (ABS(iflav(1)).LT.10) CALL lushow(id1,id1+1,sqrs1)
C            sqrs2=phep(5,jmohep(1,id2))
C            IF (ABS(iflav(3)).LT.10) CALL lushow(id2,id2+1,sqrs2)
C            CALL luexec
C         ELSE
C            ijoin(1) = id1
C            ijoin(2) = id2+1 
C            CALL lujoin(2,ijoin)
C            ijoin(1) = id1+1
C            ijoin(2) = id2 
C            CALL lujoin(2,ijoin)
C!     That's it, shower and hadronize now  
C            sqrs1=(phep(4,id1)+phep(4,id2+1))**2
C     $           -(phep(3,id1)+phep(3,id2+1))**2
C     $           -(phep(2,id1)+phep(2,id2+1))**2
C     $           -(phep(1,id1)+phep(1,id2+1))**2
C            sqrs1=sqrt(abs(sqrs1))
C            CALL lushow(id1,id2+1,sqrs1)
C            sqrs2=(phep(4,id1+1)+phep(4,id2))**2
C     $           -(phep(3,id1+1)+phep(3,id2))**2
C     $           -(phep(2,id1+1)+phep(2,id2))**2
C     $           -(phep(1,id1+1)+phep(1,id2))**2
C            sqrs2=sqrt(abs(sqrs2))
C            CALL lushow(id1+1,id2,sqrs2)
C            CALL luexec
C         ENDIF 
C      ENDIF
C      END











      SUBROUTINE INIETC(jakk1,jakk2,itd,ifpho)
      COMMON / IDFC  / IDFF
      COMMON / TAURAD / XK0DEC,ITDKRC
      DOUBLE PRECISION            XK0DEC
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON /PHOACT/ IFPHOT
      SAVE
C KTO=1 will denote tau+, thus :: IDFF=-15
          IDFF=-15
C XK0 for tau decays.
          XK0DEC=0.01
C radiative correction switch in tau --> e (mu) decays !
          ITDKRC=itd
C switches of tau+ tau- decay modes !!
          JAK1=jakk1
          JAK2=jakk2
C photos activation switch
          IFPHOT=IFPHO
      end

      SUBROUTINE TRALO4(KTOS,PHOI,PHOF,AM)
!! Corrected 11.10.96 (ZW) tralor for KORALW.
!! better treatment is to  cascade from tau rest-frame through W
!! restframe down to LAB. 
      COMMON / MOMDEC / Q1,Q2,P1,P2,P3,P4
      COMMON /TRALID/ idtra
      double precision Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
      double precision PIN(4),POUT(4),PBST(4),PBS1(4),QQ(4),PI
      double precision THET,PHI
      REAL*4 PHOI(4),PHOF(4)
      SAVE
      DATA PI /3.141592653589793238462643D0/
      AM=SQRT(ABS
     $   (PHOI(4)**2-PHOI(3)**2-PHOI(2)**2-PHOI(1)**2))
      DO K=1,4
       PIN(K)=PHOI(K)
      ENDDO
!      write(*,*) idtra
      IF    (idtra.EQ.1) THEN
        DO K=1,4
         PBST(K)=P1(K)
         QQ(K)=Q1(K)
        ENDDO
      ELSEIF(idtra.EQ.2) THEN
        DO K=1,4
         PBST(K)=P2(K)
         QQ(K)=Q1(K)
        ENDDO
      ELSEIF(idtra.EQ.3) THEN
        DO K=1,4
         PBST(K)=P3(K)
         QQ(K)=Q2(K)
        ENDDO
      ELSE
        DO K=1,4
         PBST(K)=P4(K)
         QQ(K)=Q2(K)
        ENDDO
      ENDIF
C for tau- spin-axis is antiparallel to 4-momentum. 
       IF(KTOS.EQ.1) CALL ROTOD2(PI,PIN,PIN)       

        CALL BOSTDQ(1,QQ,PBST,PBST)
        PBS1(4)=PBST(4)
        PBS1(3)=SQRT(PBST(3)**2+PBST(2)**2+PBST(1)**2)
        PBS1(2)=0D0
        PBS1(1)=0D0 
        CALL BOSTDQ(-1,PBS1,PIN,POUT)
        THET=ACOS(PBST(3)/SQRT(PBST(3)**2+PBST(2)**2+PBST(1)**2))
        PHI=0D0
        PHI=ACOS(PBST(1)/SQRT(PBST(2)**2+PBST(1)**2))
        IF(PBST(2).LT.0D0) PHI=2*PI-PHI
        CALL ROTPOX(THET,PHI,POUT)
        CALL BOSTDQ(-1,QQ,POUT,POUT)
      DO K=1,4
       PHOF(K)=POUT(K)
      ENDDO
      END
      SUBROUTINE CHOICE(MNUM,RR,ICHAN,PROB1,PROB2,PROB3,
     $            AMRX,GAMRX,AMRA,GAMRA,AMRB,GAMRB)
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      AMROP=1.1
      GAMROP=0.36
      AMOM=.782
      GAMOM=0.0084
C     XXXXA CORRESPOND TO S2 CHANNEL !
      IF(MNUM.EQ.0) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =AMA1
       GAMRX=GAMA1
       AMRA =AMRO
       GAMRA=GAMRO
       AMRB =AMRO
       GAMRB=GAMRO
      ELSEIF(MNUM.EQ.1) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.57
       GAMRX=0.9
       AMRB =AMKST
       GAMRB=GAMKST
       AMRA =AMRO
       GAMRA=GAMRO
      ELSEIF(MNUM.EQ.2) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.57
       GAMRX=0.9
       AMRB =AMKST
       GAMRB=GAMKST
       AMRA =AMRO
       GAMRA=GAMRO
      ELSEIF(MNUM.EQ.3) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.27
       GAMRX=0.3
       AMRA =AMKST
       GAMRA=GAMKST
       AMRB =AMKST
       GAMRB=GAMKST
      ELSEIF(MNUM.EQ.4) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.27
       GAMRX=0.3
       AMRA =AMKST
       GAMRA=GAMKST
       AMRB =AMKST
       GAMRB=GAMKST
      ELSEIF(MNUM.EQ.5) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.27
       GAMRX=0.3
       AMRA =AMKST
       GAMRA=GAMKST
       AMRB =AMRO
       GAMRB=GAMRO
      ELSEIF(MNUM.EQ.6) THEN
       PROB1=0.4
       PROB2=0.4
       AMRX =1.27
       GAMRX=0.3
       AMRA =AMRO
       GAMRA=GAMRO
       AMRB =AMKST
       GAMRB=GAMKST
      ELSEIF(MNUM.EQ.7) THEN
       PROB1=0.0
       PROB2=1.0
       AMRX =1.27
       GAMRX=0.9
       AMRA =AMRO
       GAMRA=GAMRO
       AMRB =AMRO
       GAMRB=GAMRO
      ELSEIF(MNUM.EQ.8) THEN
       PROB1=0.0
       PROB2=1.0
       AMRX =AMROP
       GAMRX=GAMROP
       AMRB =AMOM
       GAMRB=GAMOM
       AMRA =AMRO
       GAMRA=GAMRO
      ELSEIF(MNUM.EQ.101) THEN
       PROB1=.35
       PROB2=.35
       AMRX =1.2
       GAMRX=.46
       AMRB =AMOM
       GAMRB=GAMOM
       AMRA =AMOM
       GAMRA=GAMOM
      ELSEIF(MNUM.EQ.102) THEN
       PROB1=0.0
       PROB2=0.0
       AMRX =1.4
       GAMRX=.6
       AMRB =AMOM
       GAMRB=GAMOM
       AMRA =AMOM
       GAMRA=GAMOM
      ELSE
       PROB1=0.0
       PROB2=0.0
       AMRX =AMA1
       GAMRX=GAMA1
       AMRA =AMRO
       GAMRA=GAMRO
       AMRB =AMRO
       GAMRB=GAMRO
      ENDIF
C
      IF    (RR.LE.PROB1) THEN
       ICHAN=1
      ELSEIF(RR.LE.(PROB1+PROB2)) THEN
       ICHAN=2
        AX   =AMRA
        GX   =GAMRA
        AMRA =AMRB
        GAMRA=GAMRB
        AMRB =AX
        GAMRB=GX
        PX   =PROB1
        PROB1=PROB2
        PROB2=PX
      ELSE
       ICHAN=3
      ENDIF
C
      PROB3=1.0-PROB1-PROB2
      END

      SUBROUTINE INIPHX(XK00)
C ----------------------------------------------------------------------
C     INITIALISATION OF PARAMETERS
C     USED IN QED and/or GSW ROUTINES
C ----------------------------------------------------------------------
      COMMON / QEDPRM /ALFINV,ALFPI,XK0
      REAL*8           ALFINV,ALFPI,XK0
      REAL*8 PI8,XK00
C
      PI8    = 4.D0*DATAN(1.D0)
      ALFINV = 137.03604D0
      ALFPI  = 1D0/(ALFINV*PI8)
C---->      XK0=XK00
      END
      SUBROUTINE INITDK
C ----------------------------------------------------------------------
C     INITIALISATION OF TAU DECAY PARAMETERS  and routines
C
C     called by : KORALZ
C ----------------------------------------------------------------------
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
      PARAMETER (NMODE=15,NM1=0,NM2=1,NM3=8,NM4=2,NM5=1,NM6=3)
      COMMON / TAUDCD /IDFFIN(9,NMODE),MULPIK(NMODE)
     &                ,NAMES
      CHARACTER NAMES(NMODE)*31
      REAL*4 PI,pol1(4)
C
C LIST OF BRANCHING RATIOS
CAM normalised to e nu nutau channel
CAM                  enu   munu   pinu  rhonu   A1nu   Knu    K*nu   pi'
CAM   DATA JLIST  /    1,     2,     3,     4,     5,     6,     7,
CAM               /0.1779,0.1731,0.1106,0.2530,0.1811,0.0072,0.0139
CAM   DATA GAMPRT / 1.000,0.9732,0.6217,1.4221,1.0180,0.0405,0.0781
CAM   DATA GAMPRT /1.000,0.9676,0.6154,1.3503,1.0225,0.0368,O.O758
CAM
C
C    conventions of particles names
c
cam  mode (JAK)                     8                     9
CAM  channel          pi- pi- pi0 pi+              3pi0 pi-
cam  particle code  -1,-1, 2, 1, 0, 0,     2, 2, 2,-1, 0, 0,
CAM  BR relative to electron    .2414,                .0601,
c
*                                  10                    11
*    1                     3pi+- 2pi0                 5pi+-
*    1              -1,-1, 1, 2, 2, 0,    -1,-1,-1, 1, 1, 0,
*    1                          .0281,                .0045,

*                                  12                    13
*    2                      5pi+- pi0            3pi+- 3pi0
*    2              -1,-1,-1, 1, 1, 2,    -1,-1, 1, 2, 2, 2,
*    2                          .0010,                .0062,

*                                  14                    15
*    3                      K- pi- K+             K0 pi- KB
*    3              -3,-1, 3, 0, 0, 0,     4,-1,-4, 0, 0, 0,
*    3                          .0096,                .0169,

*                                  16                    17
*    4                      K- pi0 K0               2pi0 K-
*    4              -3, 2, 4, 0, 0, 0,     2, 2,-3, 0, 0, 0,
*    4                          .0056,                .0045,

*                                  18                    19
*    5                     K- pi- pi+            pi- KB pi0
*    5              -3,-1, 1, 0, 0, 0,    -1,-4, 2, 0, 0, 0,
*    5                          .0219,                .0180,

*                                  20                    21
*    6                    eta pi- pi0         pi- pi0 gamma
*    6               9,-1, 2, 0, 0, 0,    -1, 2, 8, 0, 0, 0,
*    6                          .0096,                .0088,

*                                  22   /
*    7                          K- K0   /
*    7                          -3, 4   /
*    7                          .0146   /

C
      DIMENSION NOPIK(6,NMODE),NPIK(NMODE)
CAM   outgoing multiplicity and flavors of multi-pion /multi-K modes
      DATA   NPIK  /                4,                    4,
     1                              5,                    5,
     2                              6,                    6,
     3                              3,                    3,
     4                              3,                    3,
     5                              3,                    3,
     6                              3,                    3,
     7                              2   /
      DATA  NOPIK / -1,-1, 2, 1, 0, 0,     2, 2, 2,-1, 0, 0,
     1              -1,-1, 1, 2, 2, 0,    -1,-1,-1, 1, 1, 0,
     2              -1,-1,-1, 1, 1, 2,    -1,-1, 1, 2, 2, 2,
     3              -3,-1, 3, 0, 0, 0,     4,-1,-4, 0, 0, 0,
     4              -3, 2, 4, 0, 0, 0,     2, 2,-3, 0, 0, 0,
     5              -3,-1, 1, 0, 0, 0,    -1,-4, 2, 0, 0, 0,
     6               9,-1, 2, 0, 0, 0,    -1, 2, 8, 0, 0, 0,
     7              -3, 4, 0, 0, 0, 0   /
C LIST OF BRANCHING RATIOS
      NCHAN = NMODE + 7
      DO 1 I = 1,30
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I
        IF(I.EQ. 1) GAMPRT(I) = 1.0000
        IF(I.EQ. 2) GAMPRT(I) =  .9732
        IF(I.EQ. 3) GAMPRT(I) =  .6217
        IF(I.EQ. 4) GAMPRT(I) = 1.4221
        IF(I.EQ. 5) GAMPRT(I) = 1.0180
        IF(I.EQ. 6) GAMPRT(I) =  .0405
        IF(I.EQ. 7) GAMPRT(I) =  .0781
        IF(I.EQ. 8) GAMPRT(I) =  .2414
        IF(I.EQ. 9) GAMPRT(I) =  .0601
        IF(I.EQ.10) GAMPRT(I) =  .0281
        IF(I.EQ.11) GAMPRT(I) =  .0045
        IF(I.EQ.12) GAMPRT(I) =  .0010
        IF(I.EQ.13) GAMPRT(I) =  .0062
        IF(I.EQ.14) GAMPRT(I) =  .0096
        IF(I.EQ.15) GAMPRT(I) =  .0169
        IF(I.EQ.16) GAMPRT(I) =  .0056
        IF(I.EQ.17) GAMPRT(I) =  .0045
        IF(I.EQ.18) GAMPRT(I) =  .0219
        IF(I.EQ.19) GAMPRT(I) =  .0180
        IF(I.EQ.20) GAMPRT(I) =  .0096
        IF(I.EQ.21) GAMPRT(I) =  .0088
        IF(I.EQ.22) GAMPRT(I) =  .0146
        IF(I.EQ. 8) NAMES(I-7)='  TAU-  --> 2PI-   PI0   PI+   '
        IF(I.EQ. 9) NAMES(I-7)='  TAU-  --> 3PI0         PI-   '
        IF(I.EQ.10) NAMES(I-7)='  TAU-  --> 2PI-   PI+  2PI0   '
        IF(I.EQ.11) NAMES(I-7)='  TAU-  --> 3PI-  2PI+         '
        IF(I.EQ.12) NAMES(I-7)='  TAU-  --> 3PI-  2PI+   PI0   '
        IF(I.EQ.13) NAMES(I-7)='  TAU-  --> 2PI-   PI+  3PI0   '
        IF(I.EQ.14) NAMES(I-7)='  TAU-  -->  K-  PI-   K+      '
        IF(I.EQ.15) NAMES(I-7)='  TAU-  -->  K0  PI-  K0B      '
        IF(I.EQ.16) NAMES(I-7)='  TAU-  -->  K-  PI0   K0      '
        IF(I.EQ.17) NAMES(I-7)='  TAU-  --> PI0  PI0   K-      '
        IF(I.EQ.18) NAMES(I-7)='  TAU-  -->  K-  PI-  PI+      '
        IF(I.EQ.19) NAMES(I-7)='  TAU-  --> PI-  K0B  PI0      '
        IF(I.EQ.20) NAMES(I-7)='  TAU-  --> ETA  PI-  PI0      '
        IF(I.EQ.21) NAMES(I-7)='  TAU-  --> PI-  PI0  GAM      '
        IF(I.EQ.22) NAMES(I-7)='  TAU-  -->  K-  K0            '
      ELSE
        JLIST(I) = 0
        GAMPRT(I) = 0.
      ENDIF
   1  CONTINUE
      DO I=1,NMODE
        MULPIK(I)=NPIK(I)
        DO J=1,MULPIK(I)
         IDFFIN(J,I)=NOPIK(J,I)
        ENDDO
      ENDDO
C
C
c --- coefficients to fix ratio of:
c --- (a1 -> 3pi+-)/(a1 -> pi+-2pi0) matrix elements (massless lim.)
c --- probability of k0 to be ks
c --- probability of k0b to be ks
c --- ratio of coefficients for k*--> k0 pi-
c --- all coefficients should be in the range (0.0,1.0)
c --- their meaning is probability of the first choice only if one
c --- neglects mass-phase space effects
      BRA1=0.5
      BRK0=0.5
      BRK0B=0.5
      BRKS=0.6667
C
C --- remaining constants
      PI =4.*ATAN(1.)
      GFERMI = 1.16637E-5
      CCABIB = 0.975
      GV     = 1.0
      GA     =-1.0
C ZW 13.04.89 HERE WAS AN ERROR
      SCABIB = SQRT(1.-CCABIB**2)
      GAMEL  = GFERMI**2*AMTAU**5/(192*PI**3)
C
      CALL DEXAY(-1,pol1)
C
      RETURN
      END
      FUNCTION DCDMAS(IDENT)
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
c
      IF      (IDENT.EQ. 1) THEN
        APKMAS=AMPI
      ELSEIF  (IDENT.EQ.-1) THEN
        APKMAS=AMPI
      ELSEIF  (IDENT.EQ. 2) THEN
        APKMAS=AMPIZ
      ELSEIF  (IDENT.EQ.-2) THEN
        APKMAS=AMPIZ
      ELSEIF  (IDENT.EQ. 3) THEN
        APKMAS=AMK
      ELSEIF  (IDENT.EQ.-3) THEN
        APKMAS=AMK
      ELSEIF  (IDENT.EQ. 4) THEN
        APKMAS=AMKZ
      ELSEIF  (IDENT.EQ.-4) THEN
        APKMAS=AMKZ
      ELSEIF  (IDENT.EQ. 8) THEN
        APKMAS=0.0001
      ELSEIF  (IDENT.EQ. 9) THEN
        APKMAS=0.5488
      ELSE
        PRINT *, 'STOP IN APKMAS, WRONG IDENT=',IDENT
        STOP
      ENDIF
      DCDMAS=APKMAS
      END
      FUNCTION LUNPIK(ID,ISGN)
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
c
      IDENT=ID*ISGN
      IF      (IDENT.EQ. 1) THEN
        IPKDEF= 211
      ELSEIF  (IDENT.EQ.-1) THEN
        IPKDEF=-211
      ELSEIF  (IDENT.EQ. 2) THEN
        IPKDEF= 111
      ELSEIF  (IDENT.EQ.-2) THEN
        IPKDEF= 111
      ELSEIF  (IDENT.EQ. 3) THEN
        IPKDEF= 321
      ELSEIF  (IDENT.EQ.-3) THEN
        IPKDEF=-321
      ELSEIF  (IDENT.EQ. 4) THEN
C
C K0 --> K0_LONG (IS 130) / K0_SHORT (IS 310) = 1/1
         CALL VARRAN(XIO,1)
CBB        CALL RANMAR(XIO,1)
        IF (XIO.GT.BRK0) THEN
          IPKDEF= 130
        ELSE
          IPKDEF= 310
        ENDIF
      ELSEIF  (IDENT.EQ.-4) THEN
C
C K0B--> K0_LONG (IS 130) / K0_SHORT (IS 310) = 1/1
        CALL VARRAN(XIO,1)
CBB        CALL RANMAR(XIO,1)
        IF (XIO.GT.BRK0B) THEN
          IPKDEF= 130
        ELSE
          IPKDEF= 310
        ENDIF
      ELSEIF  (IDENT.EQ. 8) THEN
        IPKDEF= 22
      ELSEIF  (IDENT.EQ.-8) THEN
        IPKDEF= 22
      ELSEIF  (IDENT.EQ. 9) THEN
        IPKDEF= 221
      ELSEIF  (IDENT.EQ.-9) THEN
        IPKDEF= 221
      ELSE
        PRINT *, 'STOP IN IPKDEF, WRONG IDENT=',IDENT
        STOP
      ENDIF
      LUNPIK=IPKDEF
      END
      SUBROUTINE INIMAS
C ----------------------------------------------------------------------
C     INITIALISATION OF MASSES
C
C     called by : KORALZ
C ----------------------------------------------------------------------
      COMMON / IDPART/ IA1
CCC      PARAMETER (L1MST=200, L1PAR=200)
CCC      PARAMETER (L2PAR=500, L2PARF=2000 )
CCC      PARAMETER (LJNPAR=4000)
CCC      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
CCC      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
CCC      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
CCC     &                KFDP(L2PARF,5)
CCC      COMMON /LUDAT4/ CHAF(L2PAR)
CCC      CHARACTER*8 CHAF
CCC      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
C IN-COMING / OUT-GOING  FERMION MASSES
      AMTAU  = 1.7771
      AMEL   = 0.0005111
      AMNUE  = 0.0
      AMMU   = 0.105659
      AMNUMU = 0.00
C
C MASSES USED IN TAU DECAYS
      AMPIZ  = 0.134964
      AMPI   = 0.139568
      AMRO   = 0.7714
      GAMRO  = 0.153
cam   AMRO   = 0.773
cam   GAMRO  = 0.145
      AMA1   = 1.251! PMAS(LUCOMP(ia1),1)       ! AMA1   = 1.251
      GAMA1  = 0.599! PMAS(LUCOMP(ia1),2)       ! GAMA1  = 0.599
      print *,'INIMAS a1 mass= ',ama1,gama1
      AMK    = 0.493667
      AMKZ   = 0.49772
      AMKST  = 0.8921
      GAMKST = 0.0513
C
      RETURN
      END

c================= SEMIANALYTICAL FORMULAS ===========

      Function e1wan(e)
c **********************
C this function calculates  mass spectrum GeV**-1 of the
C single W. For initialization korwan should be called first.
c **********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      s1=e**2
      e1wan=s1wan(s1)*2*e
      END


      Function s1wan(s1)
c **********************
C this function calculates invariant mass**2 spectrum of the
C single W. For initialization korwan should be called first.
c **********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! The next 4 commons are input data for KORWAN
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF  
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW    
      COMMON / DECDAT / amdumm(20), br(20)
! This coomon can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! This common sends rtesults back to KORWAN from its subprograms
      COMMON / GOUP / totfac,beti,xsmut0,dels,db
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT /
     $ XAMEL,XSVAR,XAMAZ,XGAMMZ,XAMAW,XGAMMW,XSINW2,XALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
! This PHOTONIC common sends internal parameters from KORWAN 
! to its subprograms
      COMMON / KORINT / vmin,vmax
      SAVE
! This common sends down stuff necessary for KORSAN extensions
! and reinitialisation of KORWAN 
      Common /wansan/ sx1,sx2,kmode,initre

      EXTERNAL yfspho
      KEYMOD=KEYPHO
      KeyPre= KeyAcc
      IF (KMODE.NE.-2.AND.KMODE.NE.-1.AND.KMODE.NE.5) THEN
      WRITE(6,*) 'KORWAN not initialized lets stop in S1WAN'
      STOP
      ENDIF 
      kmode=-1
      sx1=s1
      KeyZet = MOD(KeyPhy,1000)/100
      KeyNLL = MOD(KeyRad,1000)/100

      KeyZon = MOD(KeyMis,10000)/1000
      KeyWon = MOD(KeyMis,100000)/10000
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(KeyWon.EQ.1 .AND. KeyZon.EQ.0) THEN
        brel=br(7)  
        IF ( keydwm .NE. 0) THEN
          dfwm=br (keydwm)/brel
        ELSE
          dfwm=1d0/brel
        ENDIF
        IF ( keydwp .NE. 0) THEN
          dfwp=br (keydwp)/brel
        ELSE
          dfwp=1d0/brel
        ENDIF
      ELSE
        dfwm=0d0
        dfwp=0d0
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF(KEYMOD.LT.0) THEN   ! diff. distrib.
        xsmut0=xsmuta(xsvar)
        akor = yfspho(vmin)
        IF(KEYPRE.EQ.1) THEN
          errabs= 1d-5*dfwm*dfwp     ! test    (time:   1.0)  
        ELSEIF(KEYPRE.EQ.2) THEN
          errabs= 1d-6*dfwm*dfwp     ! normal  (time: * 2.5)  
        ELSEIF(KEYPRE.EQ.3) THEN
          errabs= 1d-7*dfwm*dfwp     ! high    (time: * 5.5)   
        ENDIF
      ELSEIF(KEYMOD.EQ.0) THEN   ! born
        akor = xsmuta(xsvar)
        IF(KEYPRE.EQ.1) THEN
          errabs= 1d-5*dfwm*dfwp     ! test    (time:   1.0)  
        ELSEIF(KEYPRE.EQ.2) THEN
          errabs= 1d-6*dfwm*dfwp     ! normal  (time: * 2.5)  
        ELSEIF(KEYPRE.EQ.3) THEN
          errabs= 1d-7*dfwm*dfwp     ! high    (time: * 5.5)   
        ENDIF
      ELSE  
        IF(KEYPRE.EQ.1) THEN
          eeps= 1d-5*dfwm*dfwp   ! test    (time:    1.0)
          eeps= 1d-5*dfwm*dfwp   ! test    (time:    1.0)
          errabs= 3d-4*dfwm*dfwp    
        ELSEIF(KEYPRE.EQ.2) THEN
          eeps= 5d-6*dfwm*dfwp   ! normal  (time: *  4.5) 
          errabs= 1d-5*dfwm*dfwp   
        ELSEIF(KEYPRE.EQ.3) THEN
          eeps= 1d-6*dfwm*dfwp   ! high    (time: * 40.0)
          errabs= 1d-6*dfwm*dfwp    
        ELSEIF(KEYPRE.EQ.4) THEN
          eeps= 1d-7*dfwm*dfwp   ! super-high    (time: * 200)
          errabs= 1d-7*dfwm*dfwp 
        ENDIF

        vmiorg=vmin
        vmaorg=vmax

        ndiv=4
        dv=(vmaorg-vmiorg)/dble(ndiv)
        akor=0d0
        vvma=vmiorg

        DO 100 ncurr=1,ndiv

        vvmi=vvma
        vvma=vvmi+dv
        if(vvma.ge.vmaorg) vvma=vmaorg
cc        xsmut1=xsmuta(xsvar)
cc        xsmut0=xsmut1
        vmin=vvmi
        vmax=vvma

cc        CALL gausjd(yfspho,vvmi,vvma,eeps,res)
cc        CALL dgada3(vvmi,vvma,yfspho,eeps,res)
        res = gaus3(yfspho,vvmi,vvma,eeps)  
cc        xcr=res +totfac/beti*(vvma**beti-vvmi**beti)*xsmut1*(db+dels)
        xcr=res +totfac/beti*(vvma**beti-vvmi**beti)*xsmut0*(db+dels)
        akor=akor+xcr

        if(vvma.ge.vmaorg) goto 200
 100    continue
 200    continue
      ENDIF
      vmax=vmaorg
      vmin=vmiorg
      xsect=akor
      s1wan= xsect
      end

      Function testna(s1)
c ***********************
C this is temporary routine to be erased
c ***********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      common /glupi/ ss1
      COMMON / WEKING / ENE,AMAx,GAMMx,AMEx,AMFIN,XK0,SINxW2,IDE,IDF
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
  
      external testpod
      ss1=s1
      umi=0D0
      uma=4D0*ENE**2
      aa=0d0
      bb=4D0*ENE**2

      gm =amaw*gammw
      am2=amaw**2
      umi=1d0/gm* ( ATAN((aa-am2)/gm) )
      uma=1d0/gm* ( ATAN((bb-am2)/gm) ) 
      eeps=1d-8
      call dgadap(umi,uma,testpod,eeps,res)
      testna=res
      end

      function testpod(s2)
c ************************
C this is temporary routine to be erased
c ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL

      common /glupi/ ss1 

      gmwm=gammw*amaw
      gmwm2=gmwm**2
      amw2=amaw**2
 
      ss2=GMWM*TAN(GMWM*s2)+AMW2
      gm =amaw*gammw
      am2=amaw**2
      testpod=s1s2wan(ss1,ss2)
     $       *((ss2-am2)**2+gm**2)
      end

      Function s1s2wan(s1,s2)
c **********************
C this function calculates invariant mass**2 double spectrum of the
C two W-s. For initialization korwan should be called first.
c **********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! The next 4 commons are input data for KORWAN
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF  
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW    
      COMMON / DECDAT / amdumm(20), br(20)
! This coomon can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! This common sends rtesults back to KORWAN from its subprograms
      COMMON / GOUP / totfac,beti,xsmut0,dels,db
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $ XAMEL,XSVAR,XAMAZ,XGAMMZ,XAMAW,XGAMMW,XSINW2,XALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
! This PHOTONIC common sends internal parameters from KORWAN 
! to its subprograms
      COMMON / KORINT / vmin,vmax
! This common sends down stuff necessary for KORSAN extensions
! and reinitialisation of KORWAN 
      Common /wansan/ sx1,sx2,kmode,initre
      SAVE
      EXTERNAL yfspho
      KEYMOD=KEYPHO
      KeyPre= KeyAcc
      IF (KMODE.NE.-2.AND.KMODE.NE.-1.AND.KMODE.NE.5) THEN
      WRITE(6,*) 'KORWAN not initialized lets stop in S1S2WAN'
      STOP
      ENDIF 
      kmode=-2
      sx1=s1
      sx2=s2
      KeyZet = MOD(KeyPhy,1000)/100
      KeyNLL = MOD(KeyRad,1000)/100

      KeyZon = MOD(KeyMis,10000)/1000
      KeyWon = MOD(KeyMis,100000)/10000
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(KeyWon.EQ.1 .AND. KeyZon.EQ.0) THEN
        brel=br(7)  
        IF ( keydwm .NE. 0) THEN
          dfwm=br (keydwm)/brel
        ELSE
          dfwm=1d0/brel
        ENDIF
        IF ( keydwp .NE. 0) THEN
          dfwp=br (keydwp)/brel
        ELSE
          dfwp=1d0/brel
        ENDIF
      ELSE
        dfwm=0d0
        dfwp=0d0
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF(KEYMOD.LT.0) THEN   ! diff. distrib.
        xsmut0=xsmuta(xsvar)
        akor = yfspho(vmin)
        IF(KEYPRE.EQ.1) THEN
          errabs= 1d-5*dfwm*dfwp     ! test    (time:   1.0)  
        ELSEIF(KEYPRE.EQ.2) THEN
          errabs= 1d-6*dfwm*dfwp     ! normal  (time: * 2.5)  
        ELSEIF(KEYPRE.EQ.3) THEN
          errabs= 1d-7*dfwm*dfwp     ! high    (time: * 5.5)   
        ENDIF
      ELSEIF(KEYMOD.EQ.0) THEN   ! born
        akor = xsmuta(xsvar)
        IF(KEYPRE.EQ.1) THEN
          errabs= 1d-5*dfwm*dfwp     ! test    (time:   1.0)  
        ELSEIF(KEYPRE.EQ.2) THEN
          errabs= 1d-6*dfwm*dfwp     ! normal  (time: * 2.5)  
        ELSEIF(KEYPRE.EQ.3) THEN
          errabs= 1d-7*dfwm*dfwp     ! high    (time: * 5.5)   
        ENDIF
      ELSE  
        IF(KEYPRE.EQ.1) THEN
          eeps= 1d-5*dfwm*dfwp   ! test    (time:    1.0)
          eeps= 1d-5*dfwm*dfwp   ! test    (time:    1.0)
          errabs= 3d-4*dfwm*dfwp    
        ELSEIF(KEYPRE.EQ.2) THEN
          eeps= 5d-6*dfwm*dfwp   ! normal  (time: *  4.5) 
          errabs= 1d-5*dfwm*dfwp   
        ELSEIF(KEYPRE.EQ.3) THEN
          eeps= 1d-6*dfwm*dfwp   ! high    (time: * 40.0)
          errabs= 1d-6*dfwm*dfwp    
        ELSEIF(KEYPRE.EQ.4) THEN
          eeps= 1d-7*dfwm*dfwp   ! super-high    (time: * 200)
          errabs= 1d-7*dfwm*dfwp 
        ENDIF

        vmiorg=vmin
        vmaorg=vmax

        ndiv=4
        dv=(vmaorg-vmiorg)/dble(ndiv)
        akor=0d0
        vvma=vmiorg

        DO 100 ncurr=1,ndiv

        vvmi=vvma
        vvma=vvmi+dv
        if(vvma.ge.vmaorg) vvma=vmaorg
cc        xsmut1=xsmuta(xsvar)
cc        xsmut0=xsmut1
        vmin=vvmi
        vmax=vvma

cc        CALL gausjd(yfspho,vvmi,vvma,eeps,res)
cc        CALL dgada3(vvmi,vvma,yfspho,eeps,res)
        res = gaus3(yfspho,vvmi,vvma,eeps)  
cc        xcr=res +totfac/beti*(vvma**beti-vvmi**beti)*xsmut1*(db+dels)
        xcr=res +totfac/beti*(vvma**beti-vvmi**beti)*xsmut0*(db+dels)
        akor=akor+xcr

        if(vvma.ge.vmaorg) goto 200
 100    continue
 200    continue
      ENDIF
      vmax=vmaorg
      vmin=vmiorg
      xsect=akor

      s1s2wan= xsect
      end


c================= SEMIANALYTICAL FORMULAS ===========
c================= SEMIANALYTICAL FORMULAS ===========
      SUBROUTINE korwan(svar,vvmin,vvmax,keymod,keypre,xsect,errabs)
C     **************************************************************
! KORWAN is a semianalytical routine calculating total xsection 
! with and without bremsstrahlung. Based on formula by Muta et.al. 
! and LL third order exponentiated structure functions.
!---------------------------------------------------------------
!  INPUTS:
!    svar   = CMS energy squared [GeV]
!    vvmin  = minimal photon energy (in most cases should be set to 0d0)
!    vvmax  = maximal photon energy
!    keymod = type of ISR corrections requested
!      general
!          0 ....Born               
!        300 ....Zero   Order, YFS style
!        301 ....First  Order, YFS style
!        302 ....Second Order, YFS style
!        303 ....Third  Order, YFS style
!        502 ....Second Order, Gribov-Kuraev-Fadin style
!      technical tests on various components of KORALW matr. el.
!        310 ....First  Order Beta0
!        311 ....First  Order Beta1
!        320 ....Second Order Beta0
!        321 ....Second Order Beta1
!        322 ....Second Order Beta2
!    keypre = precision key
!        1,2,3   ..for Born
!        1,2,3,4 ..for bremsstrahlung
!    other inputs are sent from KORALW input routine via commons
!    see KORALW for further explanations
!      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF  
!      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW    
!      COMMON / DECDAT / amdumm(20), br(20)
!      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
!      COMMON / MATPAR / pi,ceuler     
!      COMMON / PHYPAR / alfinv,gpicob     
!---------------------------------------------------------------
!  OUTPUT:
!    xsect  = cross-section in picobarns
!    errabs = absolute error in pbarns
!---------------------------------------------------------------

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! The next 4 commons are input data for KORWAN
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF  
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW    
      COMMON / DECDAT / amdumm(20), br(20)
! This coomon can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! This common sends rtesults back to KORWAN from its subprograms
      COMMON / GOUP / totfac,beti,xsmut0,dels,db
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $ XAMEL,XSVAR,XAMAZ,XGAMMZ,XAMAW,XGAMMW,XSINW2,XALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
! This PHOTONIC common sends internal parameters from KORWAN 
! to its subprograms
      COMMON / KORINT / vmin,vmax
! This common sends down stuff necessary for KORSAN extensions 
! and reinitialisation of KORWAN 
      Common /wansan/ sx1,sx2,kmode,initre
 
      SAVE
      EXTERNAL yfspho

      KeyAcc = MOD(KeyMis,1000)/100
      IF(KeyAcc.NE.0) THEN
        WRITE(6,*)
     $     'KORWAN==> Sorry, no anomalous couplings yet...'
        xsect=0d0 
        errabs=0d0
        RETURN  
      ENDIF
      Key4f  = MOD(KeyMis,100)/10
      KeySmp = MOD(KeyTek,1000)/100 
      IF(Key4f.NE.0) THEN
        WRITE(6,*)
     $     'KORWAN==> Sorry, KORWAN works for CC03 graphs only!'
        xsect=0d0 
        errabs=0d0
        RETURN  
      ENDIF
      KeyZon = MOD(KeyMis,10000)/1000
      KeyWon = MOD(KeyMis,100000)/10000
      IF(KeyWon.NE.1 .OR. KeyZon.NE.0) THEN
        WRITE(6,*)
     $     'KORWAN==> Sorry, KORWAN works for WW final states only!'
        xsect=0d0 
        errabs=0d0
        RETURN  
      ENDIF

      kmode  = 5
      initre = 1

! mass average if requested
      IF(SVAR.LT.0D0) THEN
        kmode = 11
        SVAR =-SVAR
        WRITE(6,*)'KORWAN WARNING ::'
        WRITE(6,*)'KORWAN WARNING :: MASS AVERAGE BEING CALCULATED'
        WRITE(6,*)'KORWAN WARNING ::      it takes for ever,'
        WRITE(6,*)'KORWAN WARNING ::  have a cup of tea and relax'
        WRITE(6,*)'KORWAN WARNING ::'
      ENDIF

      XAMEL=AMEL
      XSVAR=SVAR
      XAMAZ=AMAZ
      XGAMMZ=GAMMZ
      XAMAW=AMAW
      XGAMMW=GAMMW
      XSINW2=SINW2
      XALPHAW=ALPHAW

      VMIN=VVMIN
      VMAX=VVMAX

      KeyAcc = KeyPre
      KeyZet = MOD(KeyPhy,1000)/100
      KeyNLL = MOD(KeyRad,1000)/100
      KeyCul = MOD(KeyRad,10000)/1000

      KeyPho = mod(KeyMod,10000)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(KeyWon.EQ.1 .AND. KeyZon.EQ.0) THEN
        brel=br(7)  
        IF ( keydwm .NE. 0) THEN
          dfwm=br (keydwm)/brel
        ELSE
          dfwm=1d0/brel
        ENDIF
        IF ( keydwp .NE. 0) THEN
          dfwp=br (keydwp)/brel
        ELSE
          dfwp=1d0/brel
        ENDIF
      ELSE
        dfwm=0d0
        dfwp=0d0
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF(KEYpho.LT.0) THEN   ! diff. distrib.
        IF(KEYPRE.EQ.1) THEN
          errabs= 1d-5*dfwm*dfwp     ! test    (time:   1.0)  
        ELSEIF(KEYPRE.EQ.2) THEN
          errabs= 1d-6*dfwm*dfwp     ! normal  (time: * 2.5)  
        ELSEIF(KEYPRE.EQ.3) THEN
          errabs= 1d-7*dfwm*dfwp     ! high    (time: * 5.5)   
        ENDIF
      ELSEIF(KEYpho.EQ.0) THEN   ! born
        IF(KEYPRE.EQ.1) THEN
          errabs= 1d-5*dfwm*dfwp     ! test    (time:   1.0)  
        ELSEIF(KEYPRE.EQ.2) THEN
          errabs= 1d-6*dfwm*dfwp     ! normal  (time: * 2.5)  
        ELSEIF(KEYPRE.EQ.3) THEN
          errabs= 1d-7*dfwm*dfwp     ! high    (time: * 5.5)   
        ENDIF
      ELSE  
        IF(KEYPRE.EQ.1) THEN
          eeps= 1d-5*dfwm*dfwp   ! test    (time:    1.0)
          errabs= 3d-5*dfwm*dfwp    
        ELSEIF(KEYPRE.EQ.2) THEN
          eeps= 5d-6*dfwm*dfwp   ! normal  (time: *  4.5) 
          errabs= 1d-5*dfwm*dfwp   
        ELSEIF(KEYPRE.EQ.3) THEN
          eeps= 1d-6*dfwm*dfwp   ! high    (time: * 40.0)
          errabs= 1d-6*dfwm*dfwp    
        ELSEIF(KEYPRE.EQ.4) THEN
          eeps= 1d-7*dfwm*dfwp   ! super-high    (time: * 200)
          errabs= 1d-7*dfwm*dfwp 
        ENDIF
      ENDIF

      if(iabs(keymod).gt.10000) then   ! initialization only
        return     
      endif

      IF(KEYpho.LT.0) THEN   ! diff. distrib.
        akor = yfspho(vmin)
      ELSEIF(KEYpho.EQ.0) THEN   ! born
        akor = xsmuta(svar)
      ELSE  
        ndiv=4
        dv=(vvmax-vvmin)/dble(ndiv)
        akor=0d0
        vvma=vvmin

        DO 100 ncurr=1,ndiv

        vvmi=vvma
        vvma=vvmi+dv
        if(vvma.ge.vvmax) vvma=vvmax

        VMIN=VVMI
        VMAX=VVMA

c        CALL gausjd(yfspho,vvmi,vvma,eeps,res)
        CALL dgada3(vvmi,vvma,yfspho,eeps,res)
c        res = gaus3(yfspho,vvmi,vvma,eeps)  
        xcr=res +totfac/beti*(vvma**beti-vvmi**beti)*xsmut0*(db+dels)
        akor=akor+xcr

        if(vvma.ge.vvmax) goto 200
 100    continue
 200    continue

        vmin=vvmin
        vmax=vvmax

      ENDIF
      vmin=vvmin
      vmax=vvmax
 5    xsect=akor
      END


      function vvyfs(vvar)
C     *******************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! This common sends down stuff necessary for KORSAN extensions
! and reinitialisation of KORWAN 
      Common /wansan/ sx1,sx2,kmode,initre
      xsect = yfspho(vvar)
      if(kmode.eq.21)then
        xsect = xsect
      elseif(kmode.eq.22)then
        xsect = xsect*vvar
      elseif(kmode.eq.23)then
        xsect = xsect*vvar**2
      elseif(kmode.eq.24)then
        xsect = xsect*vvar**3
      else
        write(6,*)'vvyfs==> Wrong mode:',kmode
        stop
      endif
      vvyfs=xsect
      end

      FUNCTION xsmuta(ssact)
!     **********************
! integrated total xsect from Muta et.al.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
! This common sends internal parameters from XSMUTA to its subprograms
      COMMON / XMUINT / ssvar
! This common sends down stuff necessary for KORSAN extensions
! and reinitialisation of KORWAN 
      Common /wansan/ sx1,sx2,kmode,initre
      EXTERNAL d1muta
      SAVE

      ssvar=ssact
      aa=0d0
      bb=ssact

! change of vars
      gm =amaw*gammw
      am2=amaw**2
      umi=1d0/gm* ( ATAN((aa-am2)/gm) )
      uma=1d0/gm* ( ATAN((bb-am2)/gm) ) 

      IF(KEYACC.EQ.1) THEN
        eeps= 1d-6*dfwm*dfwp   ! test  
      ELSEIF(KEYACC.EQ.2) THEN
        eeps= 1d-8*dfwm*dfwp   ! normal
      ELSEIF(KEYACC.EQ.3.or.KEYACC.EQ.4) THEN
        eeps= 1d-9*dfwm*dfwp   ! high
      ENDIF
       IF (KMODE.EQ.-2) THEN
         xsmuta=f2muta(sx1,sx2) 
       ELSEIF (KMODE.EQ.-1) THEN
         xsmuta=f1muta(sx1)
       ELSE
c         xsmuta=gaus(d1muta,umi,uma,eeps)  
         CALL dgada2(umi,uma,d1muta,eeps,xsmuta)
       ENDIF
      END

      FUNCTION f2muta(xs1,xs2)
!     *******************
! one dimensional diff. xsect from muta et.al.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! This common sends internal parameters from XSMUTA to its subprograms
      COMMON / XMUINT / ssvar
! This common sends internal parameters from D1MUTA to its subprograms
      COMMON / D1MUIN / s1
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
      EXTERNAL D2MUTA
      SAVE

      IF (XS1.LT.SVAR) THEN
      GM =AMAW*GAMMW
      AM2=AMAW**2
      S1=xs1
      s2=xs2
! change of vars
      U=1D0/GM* ( ATAN((s2-AM2)/GM) )

      result=d2muta(u)
      ELSE
      result=0d0
      ENDIF

      f2muta =result
     $          /((s2-am2)**2+gm**2)  ! JACOBIAN

      END

      FUNCTION f1muta(xs1)
!     *******************
! one dimensional diff. xsect from muta et.al.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! This common sends internal parameters from XSMUTA to its subprograms
      COMMON / XMUINT / ssvar
! This common sends internal parameters from D1MUTA to its subprograms
      COMMON / D1MUIN / s1
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
      EXTERNAL D2MUTA
      SAVE

      IF (XS1.LT.SSVAR) THEN
      GM =AMAW*GAMMW
      AM2=AMAW**2
      S1=xs1
      AA=0D0
      BB=(SQRT(SSVAR)-SQRT(S1))**2
! change of vars
      UMI=1D0/GM* ( ATAN((AA-AM2)/GM) )
      UMA=1D0/GM* ( ATAN((BB-AM2)/GM) ) 
! change of vars

      IF(KEYACC.EQ.1) THEN
        eeps= 1d-9*dfwm*dfwp   ! test  
      ELSEIF(KEYACC.EQ.2) THEN
        eeps= 1d-11*dfwm*dfwp   ! normal 
      ELSEIF(KEYACC.EQ.3.or.KEYACC.EQ.4) THEN
        eeps= 1d-13*dfwm*dfwp   ! high, long run
      ENDIF

      call dgadap(umi,uma,d2muta,eeps,result)
      ELSE
      result=0d0
      ENDIF

      f1muta =result
     $          *((s1-am2)**2+gm**2)  ! JACOBIAN
      f1muta =result
      END

      FUNCTION d1muta(u)
!     *******************
! one dimensional diff. xsect from muta et.al.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! This common sends internal parameters from XSMUTA to its subprograms
      COMMON / XMUINT / ssvar
! This common sends internal parameters from D1MUTA to its subprograms
      COMMON / D1MUIN / s1
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
      EXTERNAL D2MUTA
      SAVE


      GM =AMAW*GAMMW
      AM2=AMAW**2
      S1=GM*TAN(GM*U)+AM2
      AA=0D0
      BB=(SQRT(SSVAR)-SQRT(S1))**2
! change of vars
      UMI=1D0/GM* ( ATAN((AA-AM2)/GM) )
      UMA=1D0/GM* ( ATAN((BB-AM2)/GM) ) 
! change of vars

      IF(KEYACC.EQ.1) THEN
        eeps= 1d-9*dfwm*dfwp   ! test  
      ELSEIF(KEYACC.EQ.2) THEN
        eeps= 1d-11*dfwm*dfwp   ! normal 
      ELSEIF(KEYACC.EQ.3.or.KEYACC.EQ.4) THEN
        eeps= 1d-13*dfwm*dfwp   ! high, long run
      ENDIF

      call dgadap(umi,uma,d2muta,eeps,result)
c      result=gaus(d2muta,umi,uma,eeps)  

      d1muta =result
     $          *((s1-am2)**2+gm**2)  ! JACOBIAN

      END


      FUNCTION d2muta(u)
C     *******************
C TWO DIMENSIONAL DIFF. XSECT FROM MUTA et.al.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / MATPAR / pi,ceuler     
      COMMON / PHYPAR / alfinv,gpicob     
! This common sends internal parameters from XSMUTA to its subprograms
      COMMON / XMUINT / ssvar
! This common sends internal parameters from D1MUTA to its subprograms
      COMMON / D1MUIN / s1
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
! This common sends down stuff necessary for KORSAN extensions
! and reinitialisation of KORWAN 
      Common /wansan/ sx1,sx2,kmode,initre

      SAVE
      data init / 1 /

      IF( init.eq.1 .or. ssvar.ne.sold .or. initre.eq.1) THEN
        sold   = ssvar
        init   = 0
        initre = 0

        e2 = 4*pi*alphaw
        g2 = e2/sinw2
        g2e2=g2*e2/2d0
        g4=g2**2/8d0
        a=1-4*sinw2
        b=-1
        a2b2=a**2+b**2
        amb=a-b

        gmw =gammw/amaw
        gmwm=gammw*amaw
        gmwm2=gmwm**2
        amw2=amaw**2

        gennor=(g2/(4*pi)/12d0)**2*gpicob/pi**2/(64*pi)
     $            *(-2d0)/ssvar**2*dfwm*dfwp

        IF( keyzet .eq. 0) THEN
          bwz=(ssvar-amaz**2)**2 +(ssvar/amaz*gammz)**2
        ELSEIF( keyzet .EQ. 1) THEN
          bwz=(ssvar-amaz**2)**2 +(amaz*gammz)**2
        ELSEIF( keyzet .EQ. 2) THEN
          bwz=(ssvar-amaz**2)**2 
        ELSE
          write(6,*)'d2muta==> wrong keyzet:',keyzet
        ENDIF
        bwzre=(ssvar-amaz**2)

        gaa = e2**2/ssvar**2
        gzz=g4/2d0 *a2b2/bwz
        gaz=g2e2   *a*bwzre/bwz/ssvar 
        gnn=g4 
        gnz=-g4     *amb*bwzre/bwz
        gna=-g2e2   /ssvar

        cngg1=(gaa+gzz+gaz)*gennor
        cngg2=(gnn)*gennor
        cngg3=(gnz+gna)*gennor
      ENDIF

C.. change of vars
      S2=GMWM*TAN(GMWM*U)+AMW2
C.. change of vars

      S1S2=S1*S2      
      S1pS2=S1+S2       
      SmS=ssvar-S1pS2

! Ad hoc correction S.J.
      IF(s2 .LT. 0d0 ) THEN
        d2muta=0d0
c[[[[[
c        write(6,*) 'd2muta <2> u,s2=',u,s2
c        write(6,*) 'd2muta <2> GMWM,AMW2=',GMWM,AMW2
c]]]]]
        RETURN
      ENDIF


      IF( (sqrt(ssvar)-sqrt(s1)-sqrt(s2)).le.0d0) THEN
        d2muta=0d0
        RETURN
c        wlambd=0d0
c        sqrwla=0d0
c        flog=0d0
      ELSE
!        WLAMBD=(SmS)**2 -4*S1S2
        WLAMBD=MAX(0d0,(SmS)**2 -4*S1S2)
c        IF(WLAMBD.LT.0D0) WRITE(6,*)'WLAMBD=',WLAMBD
c        IF(WLAMBD.LT.0D0) WRITE(6,*)sqrt(s2),sqrt(ssvar)-sqrt(s1)
        SQRWLA=DSQRT(WLAMBD)
        IF(s1s2.gt.0d0.and.SmS.GT.SQRWLA) THEN
          FLOG= S1S2*DLOG ((SmS-SQRWLA) / (SmS+SQRWLA)) 
        ELSE
          FLOG= 0D0
        ENDIF
      ENDIF

      GG1= -WLAMBD*SQRWLA* ( WLAMBD/6D0 +2*(ssvar*(S1pS2)+  S1S2) )
      GG2= -SQRWLA* ( WLAMBD/6D0 +2*(ssvar*(S1pS2)-4*S1S2) )
     &     +4*(SmS)*FLOG
      GG3= -SQRWLA* ( 
     &               WLAMBD/6D0*(ssvar+11*S1pS2)
     &         +2*(S1**2 +3*S1S2 +S2**2)*ssvar -2*(S1**3 +S2**3) 
     &               )
     &    -4*(ssvar*(S1pS2)+S1S2)*FLOG


c from Manel Martinez
c      GG3MM= -SQRWLA*(ssvar-S1-S2)* ( WLAMBD/6D0
c     &              +2*(S1*S2+ssvar*S1+ssvar*S2)
c     &                           )
c     &    -4*S1*S2*(ssvar*(S1+S2)+S1*S2)*FLOG

      D2MUTA=( CNGG1*GG1 +CNGG2*GG2 +CNGG3*GG3) 
     &        /((S1-AMW2)**2+(S1*GMW)**2)  ! breit-wigner
     &        /((S2-AMW2)**2+(S2*GMW)**2)  ! breit-wigner
     &        *((S2-AMW2)**2+GMWM2)     ! JACOBIAN
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!  average mass   
      if(kmode.eq.11) then
        d2muta=d2muta*( sqrt(s1)+sqrt(s2)-2*amaw )
      elseif(kmode.eq.12) then
        d2muta=d2muta*( sqrt(s1)+sqrt(s2)-2*amaw )**2
      elseif(kmode.eq.13) then
        d2muta=d2muta*( sqrt(s1)+sqrt(s2)-2*amaw )**3
      elseif(kmode.eq.14) then
        d2muta=d2muta*( sqrt(s1)+sqrt(s2)-2*amaw )**4
      endif
!!!!!!!  average mass   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Coulomb on top 
      cul1=culsan(ssvar,s1,s2)
      d2muta = d2muta*cul1

      END


      FUNCTION xsbrem(svar)
c     ccccccccccccccccccccccc
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / PHODIS / keypho
      SAVE

      KEYMOD=KEYPHO
      KEYPRE=2
      
      VVMI=0D0
      XSTOT=0D0
      DO vvma=0.25d0,1.001d0,.25d0
         call korwan(svar,vvmi,vvma,keymod,keypre,res,errabs)
         VVMI=VVMA
         xstot=xstot+res
      ENDDO
      xsbrem=xstot
      END

      FUNCTION yfspho(vv)
!     ************************************************
! for negative KEYPHO vv*D\otimes D*xsmuta is returned
! = differential distribution d sigma/ d log v
!     ************************************************
!          0 ....Born               
!        300 ....Zero   Order, YFS style
!        301 ....First  Order, YFS style
!        302 ....Second Order, YFS style
!        303 ....Third  Order, YFS style
!        502 ....Second Order, Gribov-Kuraev-Fadin style
!      technical tests on various components of KORALW matr. el.
!        310 ....First  Order Beta0
!        311 ....First  Order Beta1
!        320 ....Second Order Beta0
!        321 ....Second Order Beta1
!        322 ....Second Order Beta2
!     ************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  

      COMMON / GOUP / totfac,beti,xsmut0,dels,db
      COMMON / MATPAR / pi,ceuler     
      COMMON / PHYPAR / alfinv,gpicob     
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL
! This PHOTONIC common sends internal parameters from KORWAN 
! to its subprograms
      COMMON / KORINT / vmin,vmax
      Common /wansan/ sx1,sx2,kmode,initre
      SAVE
      data init / 1 /

      IF( init.eq.1 .or. svar.ne.sold .or. vmin.ne.vmiold 
     &   .or. keyNLL.ne.knlold .or. initre.eq.1) THEN
        init=0
        sold=svar
        vmiold=vmin
        knlold=keyNLL
        alf1   = 1d0/pi/alfinv
        bilg   = dlog(svar/amel**2)
        beti   = 2d0*alf1*(bilg-1d0)
        IF(    KeyNLL .EQ. 0) THEN
           delb = 1/4d0*beti  
        ELSEIF(KeyNLL .EQ. 1) THEN
           delb = 1/4d0*beti  +alf1*(-0.5d0  +pi**2/3d0)        
        ELSE
           WRITE(6,*) '++++ STOP in yfspho, KeyNLL= ',KeyNLL
           STOP
        ENDIF
        gamfac = exp(-ceuler*beti)/dpgamm(1d0+beti)          
        totfac = beti*gamfac*exp(delb)
        xsmut0 = xsmuta(svar*(1-vmin))
      ENDIF

      sprim=svar*(1-vv)
      IF( vv .GT. 1d0) THEN
        write(6,*)'WARNING YFSPHO==> 1-vv<0 ',1-vv,', vv set to 1'
        yfspho=0d0
        RETURN
!        vv=1d0
!        sprim=0d0
      ENDIF
      IF( vv .LT. 0d0) THEN
        write(6,*)'WARNING YFSPHO==> 1-vv>1 ',1-vv,', vv set to 0'
        yfspho=0d0
        RETURN
!        vv=0d0
!        sprim=svar
      ENDIF
      born = xsmuta(sprim)


      IF(KEYPHO.GE.0) THEN     
        keypht = keypho
        yfs  = totfac*vv**(beti-1d0) 
      ELSE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! WATCH OUT:  1/VV removed for negative KEYPHO !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        yfs  = totfac*vv**beti 
        xsmut0 = 0d0
        keypht = -keypho
      ENDIF
!---------------------------------------------------------------
! ....Zero   Order               
      IF(KEYPHT  .EQ.300)  THEN 
        db = 1d0        
        dels = 0d0           
        delh = -beti/4d0 *log(1-vv)          
!---------------------------------------------------------------
! ....First  Order               
      ELSEIF(KEYPHT  .EQ.301)  THEN           
        db = 1d0        
        dels = beti/2d0        
        delh = vv*(-1 +vv/2d0)            
     $      -beti/2d0*vv**2 - beti/4d0*(1-vv)**2*log(1-vv)           
!---------------------------------------------------------------
!-- Second COMPLETE, YFS style (from yfs3mod.f)
      ELSEIF(KEYPHT  .EQ.302)  THEN           
        db = 1d0
        dels = beti/2d0  +1/8d0*beti**2
        delh = vv*(-1 +vv/2d0) 
     $   +0.5d0*beti*(-0.25d0*(4d0-6d0*vv+3d0*vv**2)*dlog(1d0-vv) -vv)
!---------------------------------------------------------------
!-- Third COMPLETE, YFS style 
      ELSEIF(KEYPHT  .EQ.303)  THEN           
        db = 1d0
        ds1= beti/2d0 
        ds2=            1/8d0*beti**2 
        ds3=                           1/48d0*beti**3

        dh0=   vv*(-1 +vv/2d0)
        dh1= beti*( -(1d0+3d0*(1d0-vv)**2)/8d0*dlog(1d0-vv)
     @                 -.25d0*vv**2   )
        dh2= beti**2*( 0
     @                 +(3d0*vv-2d0)*vv/16*dlog(1d0-vv)
     @                 +(8d0-14d0*vv+7d0*vv**2)/96*dlog(1d0-vv)**2
     @                 +vv**2/8d0
     @                 +(2d0-vv)*vv/8*dilogy(vv)  
     @               )
        dels = ds1 +ds2 +ds3 
        delh = dh0*(1+ds1+ds2) +dh1*(1+ds1) +dh2
!---------------------------------------------------------------
!-- First  Order Beta0 
      ELSEIF(KEYPHT  .EQ.310)  THEN 
        db = 1d0        
        dels = beti/2d0        
        delh = -beti/4d0 *log(1-vv)   
!---------------------------------------------------------------
!-- First  Order Beta1
      ELSEIF(KEYPHT  .EQ.311)  THEN           
        db = 0d0        
        dels = 0d0        
        delh =    
     $        vv*(-1d0+vv/2d0/(1+beti))*(1-0.5*beti*log(1-vv))
!---------------------------------------------------------------
!-- Second Beta0 
      ELSEIF( KEYPHT .EQ. 320) THEN       
         db = 1d0        
         dels = beti/2d0  +beti**2/8d0             
         delh = -beti/4d0 *log(1-vv)   
!---------------------------------------------------------------
!-- Second  Order Beta1
      ELSEIF(KEYPHT  .EQ.321)  THEN           
        db = 0d0        
        dels = 0d0        
        delh = vv*(-1+vv/2d0)         
     $      -beti*vv/2 -beti*vv**2/4 +beti/8*(-2+6*vv-3*vv**2)*log(1-vv)
!---------------------------------------------------------------
!-- Second  Beta2 
      ELSEIF( KEYPHT .EQ. 322)  THEN     
        db = 0d0        
        dels = 0d0        
        delh =    beti*  vv**2/4d0  
!---------------------------------------------------------------
!-- First COMPLETE, Gribov-Kuraev-Fadin style
!---This is added in artificial manner but numerically should be OK
      ELSEIF(KEYPHT  .EQ.501)  THEN
        totfac = beti
        dels   = 0d0
        db = gamfac*exp( 3/4d0 *beti)
        z=1-vv
        delh= -1/2d0*(1d0+z)
        delh=delh/vv**(beti-1d0)
!---------------------------------------------------------------
!-- Second COMPLETE, Gribov-Kuraev-Fadin style
!---This is added in artificial manner but numerically should be OK
      ELSEIF(KEYPHT  .EQ.502)  THEN
        totfac = beti
        dels   = 0d0
        db = gamfac*exp( 3/4d0 *beti)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       DELH=  -ALF1*(1D0+Z)*(BILG-1D0)    
!    $      +ALF1**2*( -(1D0+Z*Z)/VV     *DLOG(Z)                
!    $              +(1D0+Z)*(0.5D0*DLOG(Z)-2D0*DLOG(VV))        
!    $              -2.5D0-0.5D0*Z)*BILG**2    
!       DISTR=  BETI*VV**(BETI-1D0)*( 1D0+DELVS) +DELH          
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        z=1-vv
        delh= -1/2d0*(1d0+z)
     $        +1/4d0*beti*( -(1d0+z*z)/vv     *dlog(z)                
     $                +(1d0+z)*(0.5d0*dlog(z)-2d0*dlog(vv))        
     $                 -2.5d0-0.5d0*z)
        delh=delh/vv**(beti-1d0)
      ELSE
        write(6,* ) '+++++ STOP in YFSPHO, wrong KEYPHO=',KEYPHT
      ENDIF        

      yfspho= yfs*(  (db+dels+delh)*born -(db+dels)*xsmut0  )
!----------------------------------------------------------------   
      END

      FUNCTION culsan2(s,s1,s2)
!     **************************
! Coulomb effect from Fadin, Khoze, Martin, Stirling, dtp/95/64
! first order, eq. 9

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / MATPAR / pi,ceuler     
      COMMON / PHYPAR / alfinv,gpicob     
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL

      p=sqrt( 1/(4*s)*( s**2 -2*s*(s1+s2) +(s1-s2)**2 ) )
      e=(s-4*amaw**2)/(4*amaw)
      sqeg= sqrt(e**2+gammw**2)

      rek =sqrt( amaw/2*( sqeg -e ) )
      abk2 =amaw*sqeg

      culsan2=1+sqrt(s)/alfinv/4/p *(pi-
     $          2*datan( (abk2 -p**2)/2/p/rek ) )
      end

      FUNCTION culsan(s,s1,s2)
!     **************************
! Coulomb effect from Fadin, Khoze, Martin, Stirling, dtp/95/64
! first order, eq. 9

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  

      COMMON / MATPAR / pi,ceuler     
      COMMON / PHYPAR / alfinv,gpicob     
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KEYPHO,KEYCUL

      if(keycul.eq.0) then
        culsan = 1d0
      elseif(keycul.eq.1) then
        pp = 1/(4*s) *( s**2 -2*s*(s1+s2) +(s1-s2)**2 )
        p  = dsqrt(pp)
        en = (s-4*amaw**2)/(4*amaw)
        ddee = dsqrt(en**2+gammw**2)
        p1 = dsqrt( amaw/2d0 *( ddee -en ) )
        p2 = dsqrt( amaw/2d0 *( ddee +en ) )
        dabskap2 = amaw *ddee
        drekap  =  p1
        dimkap  = -p2

        ff = 1 +sqrt(s)/(4*p*alfinv) 
     $           *( pi -2*datan( (dabskap2 -pp)/(2*p*drekap) ) )   

        culsan = ff -1  ! <========!!!!!!!!!
        culsan = ff
      else
        write(6,*) ' culSAN==> wrong keycul=',keycul
        stop
      endif
      end

c================= END SEMIANALYTICAL FORMULAS ===========
c================= END SEMIANALYTICAL FORMULAS ===========
c================= END SEMIANALYTICAL FORMULAS ===========


      SUBROUTINE MAVRG(SVAR,KEYPHO,kaccbre,dmavrg,dmerr)
C     *************************************************
! does average mass calculation, ie
! < sqrt(s1)+sqrt(s2) -2amaw > / sigma_tot
C     *************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / 
     $   AMEL,SsVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW,DFWM,DFWP
     $ ,KEYZET,KEYACC,KEYNLL,KkEYPHO,KEYCUL
      SAVE

      CALL KORWAN(SVAR,0D0,1D0, KEYPHO,KACCBRE,XSECT,ERRABS)

      CALL KORWAN(-SVAR,0D0,1D0, KEYPHO,KACCBRE,dmSECT,Eps)
      dmavrg = dmSECT/XSECT/sqrt(svar)
      dmerr  = ( (eps*xsect) +(errabs*dmsect) )
     $                             /xsect**2 / sqrt(svar)
      END

      SUBROUTINE MLOSS(SVAR,KEYPHO,eeps,VVLOSS,VVERR)
C     *************************************************
! does average mass loss calculation, ie.
! sqrt(s)/2 < v > / sigma_tot
C     *************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      SAVE
c .. communicates with vvyfs
      common / vlos / ssvar,keyph,kaccborn
      external vxyfs
      ssvar = svar
      keyph = keypho
      kaccborn = 3

      kaccbre = 4
      CALL KORWAN(SVAR,0D0,1D0, KEYPHO,KACCBRE,XSECT,ERRABS)

      eps=eeps*2/sqrt(svar)*xsect
      CALL KORWAN(SVAR,0D0,1D0, 0,kaccborn,XS,ER)
      if(eps.lt.0.1d0*er) eps=0.1d0*er

      call dgada3(0d0,1d0,vxyfs,eps,vvsect)
      VVLOSS = SQRT(SVAR)/2D0 *VVSECT/XSECT
      VVERR  = SQRT(SVAR)/2D0 *sqrt( (eps*xsect)**2 
     $                     +(errabs*vvsect)**2 
     $                             )/xsect**2
      END


      function vxyfs(vvar)
C     *******************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      common / vlos / svar,keypho,keyacc
      save
      CALL KORWAN(SVAR,VVAR,VVDUM,-KEYPHO,KEYACC,XSECT,ERRABS)
      vxyfs = xsect
      end

