*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                     Pseudo-CLASS  BornV                                  //
*//                                                                          //
*//  Purpose:                                                                //
*//  Provide Born angular distribution and integrated x-section              //
*//  as a function of s.                                                     //
*//                                                                          //
*//  NOTES:                                                                  //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
*
      SUBROUTINE BornV_Initialize(xpar_input)
*//////////////////////////////////////////////////////////////////////////////
*//                    Class initializator                                   //
*// Notes:                                                                   //
*// This initializator should be called before any other routine of the class//
*// It defines (mostly static) class members using input from xpar matrix    //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BXformat.h'
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  xpar_input(*)
*------------------------------------------------------------------------------
      DOUBLE PRECISION    amuon
      INTEGER             k,j,kxpa,KF
      DOUBLE PRECISION    vvmax
*------------------------------------------------------------------------------
      m_QCDcor = 0d0
      DO k=1,m_poinQ
         m_QCDcorR(k)=0d0
      ENDDO
      m_CMSene = xpar_input( 1)         ! Central value of CMS energy, do not change!
      m_XXXene = m_CMSene               ! Just initialization only
      m_KFini = xpar_input( 400)        ! KFcode of beam, POSITIVE!!!
*                      <<<  ff-pair spectrum >>>
      m_vvmin  = xpar_input(16)         ! minimum v, infrared cut
      vvmax    = xpar_input(17)         ! maximum v
      amuon  = 0.1056583d0
      m_vvmax  = min(vvmax, 1d0-(2*amuon/m_CMSene)**2)
      m_HadMin = xpar_input(51)         ! minimum hadronization mass
*                        <<< Basic QED >>>
      m_AlfInv = xpar_input(30)         ! Alpha_QED at Thomson limit
      m_alfpi  = 1d0/m_pi/m_AlfInv
*                  <<< Electroweak parameters >>>
      m_Gmu    = xpar_input(32)         ! Fermi constant
      m_MZ     = xpar_input(502)        ! Z mass [GeV]
      m_amh    = xpar_input(805)        ! Higgs mass, Input for Dizet
      m_amtop  = xpar_input(806)        ! Top mass,   Input for Dizet
* Note that gammz and swsq will be redefined in the case of EW corrs. are on
      m_swsq   = xpar_input(503)        ! Electroweak mixing angle
      m_gammz  = xpar_input(504)        ! Z width
      m_MW     = xpar_input(505)        ! W mass [GeV]
      m_GammW  = xpar_input(506)        ! W width[GeV]

*               <<< Static Table of ALL fermion parameters >>>
      DO j=1,20
         m_IsGenerated(j) = xpar_input(400+j)   ! Generation flag
         kxpa = 500+10*j
         m_KFferm(j)= xpar_input(kxpa+1)        ! fermion flavour code
         m_NCf(j)   = xpar_input(kxpa+2)        ! number of colours
         m_Qf(j)    = xpar_input(kxpa+3)/3d0    ! electric charge
         m_T3f(j)   = xpar_input(kxpa+4)/2d0    ! isospin, L-hand component
         m_helic(j) = xpar_input(kxpa+5)        ! helicity, polarization
         m_amferm(j)= xpar_input(kxpa+6)        ! fermion mass
         m_AuxPar(j)= xpar_input(kxpa+8)        ! auxiliary parameter
      ENDDO
*                       <<< Test switches >>>
      m_KeyElw = xpar_input(12)         ! ElectroWeak library on/off
      m_KeyZet = xpar_input(501)        ! Z-boson on/off
      m_KeyWtm = xpar_input(26)         ! Photon emission without mass terms
*                       <<<  Other        >>>
      m_KeyQCD = xpar_input(53)         ! QCD FSR
      m_KeyINT = xpar_input(27)         ! This is realy copy from KK2f
      m_Xenph  = xpar_input(40)         ! This is realy copy from KK2f
      IF(m_KeyINT .EQ. 0)  m_Xenph  = 1D0
*                       <<< Miscelaneous >>>
      m_gnanob = xpar_input(31)         ! GeV^(-2) to nanobarns
*
      m_out    = xpar_input(4)
*
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '  BornV  Initializator                '
      WRITE(m_out,bxl1f) m_MZ    ,   'Z mass     [GeV]   ','amz   ','a1'
      WRITE(m_out,bxl1f) m_amh   ,   'Higgs mass [GeV]   ','amh   ','a2'
      WRITE(m_out,bxl1f) m_amtop ,   'Top mass   [GeV]   ','amtop ','a3'
      WRITE(m_out,bxl1f) m_gammz,    'Z width    [GeV]   ','gammz ','a4'
      WRITE(m_out,bxl1f) m_swsq,     'sin(theta_w)**2    ','sinw2 ','a5'
      WRITE(m_out,bxl1f) m_AlfInv,   '1/alfa_QED  at  Q=0','AlfInv','a6'
      WRITE(m_out,bxl1f) m_HadMin,   'MassCut light qqbar','HadMin','a6'
      WRITE(m_out,bxl1i) m_KFini ,   'KF code of beam    ','KFini ','a7'
      WRITE(m_out,bxl1g) vvmax,      'Input vvmax        ','vvmax ','a8'
      WRITE(m_out,bxl1g) m_vvmax,    'reduced vvmax in MC','vvmax ','a9'
      WRITE(m_out,bxtxt) 'Test switches:                         '
      WRITE(m_out,bxl1i) m_KeyElw,   'Electroweak lib.   ','KeyElw','10'
      WRITE(m_out,bxl1i) m_KeyZet,   'Z on/off   switch  ','KeyZet','11'
      WRITE(m_out,bxl1i) m_KeyWtm,   'mass terms on/off  ','KeyWtm','12'
      WRITE(m_out,bxclo)
      IF( m_KeyElw .NE. 0 ) CALL BornV_StartEW(xpar_input)
      END


      SUBROUTINE BornV_ReBin1(RR,alf,bet,xmax,x,djac)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//  This mapps variable r into x.                                           //
*//  to be employed in the integration (either ordinary or Monte Carlo)      //
*//  of distributions resambling                                             //
*//  the binomial distribution x**(alf-1)*(1-x)**(bet-1)                     //
*//  with alf > 0 and  bet arbitrary.                                        //
*//  variable r is in (0,1) range and x is within (0,xmax) range.            //
*//  djac is jacobian factor d(x)/d(r).                                      //
*//  mapping is such that 1/djac is very CLOSE to                            //
*//  binomial distribution x**(alf-1)*(1-x)**(bet-1).                        //
*//  WARNING: mapping may fail very CLOSE to r=0. Practically, one is        //
*//  recommended to obey: fleps**alf < r, where fleps = 1d-100.              //
*//  problems may also arise for very small xmax ( below 1.d-12 ).           //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION RR,R,alf,bet,xmax,x,djac
      DOUBLE PRECISION x0,dist,r1,p1,q1,q2
*------------------------------------------------------------------------------
      IF( alf .LE. 0d0 ) GOTO 900
      R = MAX(RR,m_fleps**alf)
      x0=(alf-1d0)/(alf+bet-2d0)
      IF(x0 .GT. xmax) x0=xmax
      x0= max(x0, 0d0)
      q1= 1d0/alf *x0**alf  *(1d0-x0)**(bet-1d0)
      q2= x0**(alf-1d0)     *1d0/bet *( (1d0-x0)**bet -(1d0-xmax)**bet )
      p1= q1/(q1+q2)
      IF( r .LE. p1 ) THEN
         x     = x0*(r/p1)**(1d0/alf)
         dist  = x**(alf-1d0)  *(1d0-x0)**(bet-1d0)
      ELSE
         r1    = (1d0-r)/(1d0-p1)
         x     = (1d0-xmax)**bet + ((1d0-x0)**bet-(1d0-xmax)**bet)*r1
         x     = 1d0 - x**(1d0/bet)
         dist  = x0**(alf-1d0) *(1d0-x)**(bet-1d0)
      ENDIF
      djac=(q1+q2)/dist
      RETURN
  900 WRITE(*,*) ' ========= STOP in BornV_ReBin1: wrong params'
      STOP
      END

      SUBROUTINE BornV_ReBin1a(RR,alf,bet,xmax,x,djac)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   the same as BornV_ReBin1 but pole approximation used                   //
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION RR,R,alf,bet,xmax,x,djac
      DOUBLE PRECISION x0,dist,r1,p1,q1,q2
*------------------------------------------------------------------------------
      IF( alf .LE. 0d0 ) GOTO 900
      R = MAX(RR,m_fleps**alf)
      x0=(alf-1d0)/(alf+bet-2d0)
      IF(x0 .GT. xmax) x0=xmax
      x0= max(x0, 0d0)
      q1= 1d0/alf *x0**alf
      q2= 1d0/bet *( (1d0-x0)**bet -(1d0-xmax)**bet )
      p1= q1/(q1+q2)
      IF( r .LE. p1 ) THEN
         x     = x0*(r/p1)**(1d0/alf)
         dist  = x**(alf-1d0)
      ELSE
         r1    = (1d0-r)/(1d0-p1)
         x     = (1d0-xmax)**bet + ((1d0-x0)**bet-(1d0-xmax)**bet)*r1
         x     = 1d0 - x**(1d0/bet)
         dist  = (1d0-x)**(bet-1d0)
      ENDIF
      djac=(q1+q2)/dist
      RETURN
  900 WRITE(*,*) ' ========= STOP in BornV_ReBin1: wrong params'
      STOP
      END

      SUBROUTINE BornV_ReBin2(RR,alf,bet,x,xm1,djac)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//  This mapps variable r into x. xm1=1-x kept because of rounding errors   //
*//  The same as BornV_ReBin1, but xmax=1 and bet>0                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION RR,R,alf,bet,x,xm1,djac
      DOUBLE PRECISION x0,dist,r1,p1,q1,q2
*------------------------------------------------------------------------------
      IF( alf .LE. 0d0 ) GOTO 900
      IF( bet .LE. 0d0 ) GOTO 900
      R = MAX(RR,m_fleps**alf)
      x0=(alf-1d0)/(alf+bet-2d0)
      IF( (x0 .GT. 1d0) .OR. (x0 .LT. 0d0) ) GOTO 900
      x0= max(x0, 0d0)
      q1= 1d0/alf *x0**alf  *(1d0-x0)**(bet-1d0)
      q2= x0**(alf-1d0)     *1d0/bet *(1d0-x0)**bet
      p1= q1/(q1+q2)
      IF( r .LE. p1 ) THEN
         x    = x0*(r/p1)**(1d0/alf)
         dist = x**(alf-1d0)  *(1d0-x0)**(bet-1d0)
         xm1  = 1d0-x
      ELSE
         r1   = (1d0-r)/(1d0-p1)
         r1   = MAX(r1,m_fleps**bet)
         xm1  =(1d0-x0) *r1**(1d0/bet)
         dist = x0**(alf-1d0) *xm1**(bet-1d0)
         x    = 1d0-xm1
      ENDIF
      djac=(q1+q2)/dist
      RETURN
  900 WRITE(*,*) ' ========= STOP in BornV_ReBin2: wrong params'
      STOP
      END

      SUBROUTINE BornV_ReBin2a(RR,alf,bet,x,xm1,djac)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//  This mapps variable r into x. xm1=1-x kept because of rounding errors   //
*//  The same as BornV_ReBin2, but xmax=1 and bet>0                          //
*//  and pole approximation is used for crude/simplified distribution.       //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION RR,R,alf,bet,x,xm1,djac
      DOUBLE PRECISION x0,dist,r1,p1,q1,q2
*------------------------------------------------------------------------------
      IF( alf .LE. 0d0 ) GOTO 900
      IF( bet .LE. 0d0 ) GOTO 900
      R = MAX(RR,m_fleps**alf)
      x0=(alf-1d0)/(alf+bet-2d0)
      IF( (x0 .GT. 1d0) .OR. (x0 .LT. 0d0) ) GOTO 900
      x0= max(x0, 0d0)
      q1= 1d0/alf *x0**alf
      q2= 1d0/bet *(1d0-x0)**bet
      p1= q1/(q1+q2)
      IF( r .LE. p1 ) THEN
         x    = x0*(r/p1)**(1d0/alf)
         dist = x**(alf-1d0)
         xm1  = 1d0-x
      ELSE
         r1   = (1d0-r)/(1d0-p1)
         r1   = MAX(r1,m_fleps**bet)
         xm1  = (1d0-x0) *r1**(1d0/bet)
         dist = xm1**(bet-1d0)
         x    = 1d0-xm1
      ENDIF
      djac=(q1+q2)/dist
      RETURN
  900 WRITE(*,*) ' ========= STOP in BornV_ReBin2a: wrong params'
      STOP
      END

      DOUBLE PRECISION FUNCTION BornV_RhoFoamC(xarg)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Integrand for FoamC in 3-dim mode for beamstrahlung                        //
*//                                                                              //
*//   Remember that BornV_Crude and BornV_MakeRho use hidden input  m_XXXene!!   //
*//   BornV_Crude is in R-units (poitnlike xsection at  sqrt(s)=m_XXXene!        //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  xarg(10)
      DOUBLE PRECISION  R,r1,r2
      DOUBLE PRECISION  Power,Jacob,sf12
      DOUBLE PRECISION  Rho,BornV_Crude,IRC_circee
      DOUBLE PRECISION  z1, z2, XX, RhoISR, gamiCR, gami, beta, GamBig, alpha, alpha2
      DOUBLE PRECISION  Rjac0, Rjac1, Rjac2
      DOUBLE PRECISION  zbms, zisr, y1,y2, ybms,yisr, xbms,xisr
      DOUBLE PRECISION  Par(0:3)
      INTEGER Option
      INTEGER           Icont
      DATA              Icont/0/
      Icont = Icont+1
*//////////////////////////////////////////////////////////////////////////////////////
*//  gamiCR, alpha, beta are dummy parameters in variable transformations
*//  they can be varied by 25% or so and weight distribution will be exactly the same!
*//  grid will absorb their variations.
      CALL IRC_GetParamee (Par) ! dee(z) = Par(1) *z1**Par(2)  *(1-z1)**Par(3), z1=1-x1
****  IF(Icont.LE.1) WRITE(*,*) ' Par(i)= ', Par(0), Par(1),Par(2),Par(3)
      alpha =  0.40d0           ! beamsstrahl: x1**(alpha-1), alpha manualy adjusted
      alpha =  Par(3)+1d0
      beta  = -0.50d0           ! ISR crude is as (1-vv)**(-1.5)=(1-vv)**(beta-1)
*//////////////////////////////////////////////////////////////////////////////////////
      R    = xarg(1)
      r1   = xarg(2)
      r2   = xarg(3)
      Rho  = 1d0
      Option = 1                ! Option = 1, sophisticated, a litle bit better
      Option = 2                ! Option = 2, simple, good enough
      IF( Option .EQ. 1 ) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//   R --> XX,   ZZ=1-XX=(1-vv)*(1-x1)= total loss factor, ISR and beamsstrahlung
         CALL BornV_MakeGami(m_CMSene,gamiCR,gami)          ! make gamiCR at CMSene
         IF( gamiCR .LE. 0d0 ) GOTO 800
         GamBig = gami+2d0*alpha                            ! total singularity at XX=0
         CALL BornV_ReBin1a(R,GamBig,beta,m_vvmax,XX,RJac0) ! Mapping  R => XX=1-ZZ
         Rho = Rho *RJac0
*//   r1 --> m_vv
         alpha2 = 2d0*alpha
         CALL BornV_ReBin2a(r1, gami, alpha2, yisr, ybms, RJac1) ! Mapping  r1 => m_vv
         xisr = yisr *XX
         xbms = ybms *XX/(1d0-yisr*XX)
         Rho  = Rho  *XX/(1d0-yisr*XX) *RJac1
         zisr = 1d0-xisr
         zbms = 1d0-xbms
         m_vv = xisr
*//   r2 --> m_x2
         CALL BornV_ReBin2a(r2, alpha, alpha, y1, y2, RJac2) ! Mapping  r2 => m_x2
         m_x1 =   y1*xbms
         m_x2 =   y2*xbms/(1d0-y1*xbms)
         Rho  = Rho *xbms/(1d0-y1*xbms) *RJac2 
*//////////////////////////////////////////////////////////////////////////////////////
*//   simplified analytical importance sampling transformations
      ELSEIF( Option .EQ. 2 ) THEN
         CALL BornV_MakeGami(m_CMSene,gamiCR,gami)           ! make gamiCR at CMSene
         IF( gamiCR .LE. 0d0 ) GOTO 800
         m_vv  = R**(1d0/gamiCR)*m_vvmax
         Rho   = Rho* m_vv/R/gamiCR*m_vvmax
         m_x1  = r1**(1d0/alpha)                             ! Mapping  r1 => x1
         Rho   = Rho   *m_x1/r1/alpha
         m_x2  = r2**(1d0/alpha)                             ! Mapping  r2 => x2
         Rho = Rho   *m_x2/r2/alpha
         IF( (1d0-m_vv)*(1d0-m_x1)*(1d0-m_x2) .LT. (1d0-m_vvmax) ) GOTO 800
      ENDIF
      z1 = 1d0-m_x1
      z2 = 1d0-m_x2
*//////////////////////////////////////////////////////////////////////////////////////
*//   Calculate ISR crude structure function (the same as in Karlud)
      m_XXXene =  m_CMSene*SQRT(z1*z2)                   ! hidden input for BornV_Crude
      CALL BornV_MakeISR(RhoISR)                         !<-- uses m_XXXene and m_vv
      Rho = Rho *RhoISR
*//////////////////////////////////////////////////////////////////////////////////////
*//   Beamsstrahlung structure function, singular as m_x1**(alpha-1)
      IF( (z1.EQ.1d0) .OR. (z2.EQ.1d0) ) THEN ! rounding errors may cause problems
         SF12 = 0d0
      ELSE
***      SF12 = IRC_circee( z1, z2 )
         SF12 = Par(1) *m_x1**Par(3) *z1**Par(2)   *Par(1) *m_x2**Par(3) *z2**Par(2)
***      SF12 = Par(1) *m_x1**Par(3)               *Par(1) *m_x2**Par(3)    ! Truncated
      ENDIF
      Rho = Rho *SF12

* Born Xsection at s' = m_XXXene**2 *(1-vv)
      BornV_RhoFoamC = Rho*BornV_Crude(m_vv)/(1d0-m_vv)
      RETURN
 800  CONTINUE
      BornV_RhoFoamC =0d0
      RETURN
 900  CONTINUE
      WRITE(*,*) ' STOP in BornV_RhoFoamC, m_x1 = ', m_x1
      WRITE(*,*) ' XX, m_vv= ', XX, m_vv
      STOP
      END

      DOUBLE PRECISION FUNCTION BornV_RhoFoamB(xarg)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Integrand for FoamB in 2-dim mode for beamstrahlung                        //
*//                                                                              //
*//   Remember that BornV_Crude and BornV_MakeRho use hidden input  m_XXXene!!   //
*//   BornV_Crude is in R-units (poitnlike xsection at  sqrt(s)=m_XXXene!        //
*//                                                                              //
*//                                                                              //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  xarg(10)
      DOUBLE PRECISION  R,r1
      DOUBLE PRECISION  Rho,BornV_Crude,IRC_circee
      DOUBLE PRECISION  gamiCR, gami, beta, RJac0, RJac1, RJacS
      DOUBLE PRECISION  alpha,  alpha2,  eps,  eps2
      DOUBLE PRECISION  GamBig, RhoISR, SF1, XX, yisr, ybms, z1, aa
      DOUBLE PRECISION  anor, xnor
      DOUBLE PRECISION  Par(0:3)
      INTEGER Option
      INTEGER           Icont
      DATA              Icont/0/
      Icont = Icont+1
*//////////////////////////////////////////////////////////////////////////////////////
*//  gamiCR, alpha, beta are dummy parameters in variable transformations
*//  they can be varied by 25% or so and weight distribution will be exactly the same!
*//  grid will absorb their variations.
      CALL IRC_GetParamee (Par) ! dee(z) = Par(1) *z1**Par(2)  *(1-z1)**Par(3), z1=1-x1
****  IF(Icont.LE.1) WRITE(*,*) ' Par(i)= ', Par(0), Par(1),Par(2),Par(3)
      alpha =  0.40d0           ! beamsstrahl: x1**(alpha-1), alpha manualy adjusted
      alpha =  Par(3)+1d0
      beta  = -0.50d0           ! ISR crude is as (1-vv)**(-1.5)=(1-vv)**(beta-1)
*//////////////////////////////////////////////////////////////////////////////////////
      R    = xarg(1)
      r1   = xarg(2)
      m_x2 = 0d0
      Rho  = 1d0
      Option = 3                ! Option = 3 for tests of normalization only
      Option = 1                ! Option = 1 sophisticated
      Option = 2                ! Option = 2 simple, good enough
      IF( Option .EQ. 1 ) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//   (over)complicated analytical importance sampling transformations
*//   R --> XX,   ZZ=1-XX=(1-vv)*(1-x1)= total loss factor, ISR and beamsstrahlung
         CALL BornV_MakeGami(m_CMSene,gamiCR,gami)          ! make gamiCR at CMSene
         IF( gamiCR .LE. 0d0 ) GOTO 800
         GamBig = gami+alpha                                ! total singularity at XX=0
         CALL BornV_ReBin1a(R,GamBig,beta,m_vvmax,XX,RJac0) ! Mapping  R => XX
         Rho = Rho *RJac0
*//   r1 --> m_vv
         IF( gamiCR .LE. 0d0 ) GOTO 800
         CALL BornV_ReBin2a(r1, gami, alpha, yisr, ybms, RJac1) ! Mapping  r1 => m_vv
         m_vv =  yisr* XX
         m_x1 =  ybms* XX/(1d0-yisr*XX)
         Rho  =  Rho  *XX/(1d0-yisr*XX) *RJac1
         IF( m_x1 .GE. 1d0) GOTO 900
*//////////////////////////////////////////////////////////////////////////////////////
*//   simplified analytical importance sampling transformations
      ELSEIF( Option .EQ. 2 ) THEN
         CALL BornV_MakeGami(m_CMSene,gamiCR,gami)           ! make gamiCR at CMSene
         IF( gamiCR .LE. 0d0 ) GOTO 800
         m_vv  = R**(1d0/gamiCR)*m_vvmax
         Rho   = Rho* m_vv/R/gamiCR*m_vvmax
         m_x1  = r1**(1d0/alpha)                             ! Mapping  r1 => x1
         Rho   = Rho *m_x1/r1/alpha
         IF( (1d0-m_vv)*(1d0-m_x1) .LT. (1d0-m_vvmax) ) GOTO 800
      ELSEIF( Option .EQ. 3 ) THEN
*//   primitive test options, usefull for checking normalization
         m_vv = R*m_vvmax
         Rho = Rho *m_vvmax
         m_x1  = r1
         IF( (1d0-m_vv)*(1d0-m_x1) .LT. (1d0-m_vvmax) ) GOTO 800
      ENDIF
*//////////////////////////////////////////////////////////////////////////////////////
*//   Calculate ISR crude structure function (the same as in Karlud)
      m_XXXene =  m_CMSene*SQRT(1d0-m_x1)              ! hidden input for BornV_Crude
      CALL BornV_MakeISR(RhoISR)                       !<-- uses m_XXXene and m_vv
      Rho = Rho *RhoISR
*//////////////////////////////////////////////////////////////////////////////////////
*//   Beamsstrahlung structure function, singular as m_x1**(alpha-1)
      z1 = 1d0-m_x1
      IF( (z1.EQ.1d0) .OR. (m_x1.EQ.0d0) ) THEN ! rounding errors may cause problems
         SF1 = 0d0
      ELSE
*****    SF1 = 2d0 *IRC_circee( z1, 1d0 )   ! factor 2 due to implicit symmetrization x1<-->x2
*****    SF1 = 2d0 *Par(0) *Par(1) *m_x1**Par(3)               ! truncated
         SF1 = 2d0 *Par(0) *Par(1) *m_x1**Par(3) *z1**Par(2)   ! the same as circee
      ENDIF
      Rho = Rho *SF1
*//////////////////////////////////////////////////////////////////////////////////////
*//   Born Xsection at s' =m_XXXene**2 *(1-vv) =m_CMSene**2 *(1-XX)
      BornV_RhoFoamB = Rho* BornV_Crude(m_vv)/(1d0-m_vv)
      RETURN
 800  CONTINUE
      BornV_RhoFoamB =0d0
      RETURN
 900  CONTINUE
      WRITE(*,*) ' STOP in BornV_RhoFoamB, m_x1 = ', m_x1
      WRITE(*,*) ' XX, m_vv= ', XX, m_vv
      STOP
      END                       ! BornV_RhoFoamB


      DOUBLE PRECISION FUNCTION BornV_RhoFoamA(xarg)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Integrand for FoamA in 1-dim mode beamstrahlung off/on                     //
*//   !!! DEFINES m_vv !!!!                                                      //
*//                                                                              //
*//   Remember that BornV_Crude and BornV_MakeRho use hidden input  m_XXXene!!   //
*//   BornV_Crude is in R-units (poitnlike xsection at  sqrt(s)=m_XXXene!        //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  xarg(10)
      DOUBLE PRECISION  R
      DOUBLE PRECISION  Rho,BornV_Crude
      DOUBLE PRECISION  gamiCR, gami, beta, RJac
      DOUBLE PRECISION  IRC_circee
*----------------------------------------
      R  = xarg(1)
      m_x1 = 0d0
      m_x2 = 0d0
*-----------------------------------------------
      m_XXXene =  m_CMSene        ! hidden input for BornV_Crude
      CALL BornV_MakeGami(m_XXXene,gamiCR,gami)
      IF( gamiCR .LE. 0d0 ) GOTO 800
*     Mapping  r => vv change  to improve on efficiency
      m_vv  = R**(1d0/gamiCR)*m_vvmax
      RJac  = m_vv/R/gamiCR*m_vvmax
      CALL BornV_MakeISR(Rho)                  !<-- uses m_XXXene and m_vv
      Rho = Rho*RJac
*----------------------------------------
      Rho = Rho *IRC_circee(1d0,1d0)           !<-- implicit factor from circee 
*----------------------------------------
* Born Xsection at s' = m_XXXene**2 *(1-vv)
      IF(m_KeyZet .EQ. -2) THEN   ! Artificial constant x-section for test runs
         BornV_RhoFoamA = Rho* BornV_Crude(0d0)
      ELSE                        ! 1/(1-vv) because BornV_Crude is in R-units
         BornV_RhoFoamA = Rho* BornV_Crude(m_vv)/(1d0-m_vv)
      ENDIF
      RETURN
 800  CONTINUE
      BornV_RhoFoamA =0d0
      END

      DOUBLE PRECISION  FUNCTION BornV_RhoVesko1(R)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Integrand of Vesko, dSigma/dV(V)*Jacobians function of R                   //
*//   !!!! DEFINES m_vv !!!!                                                     //
*//                                                                              //
*//   Remember that BornV_Crude and BornV_MakeRho use hidden input  m_XXXene!!   //
*//   BornV_Crude is in R-units (poitnlike xsection at  sqrt(s)=m_XXXene!        //
*//                                                                              //
*//   In  the case of beamsstrahlung additional normalization                    //
*//   factor circee(1d0,1d0) is added in BStra_Initialize                        //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION   R,Rho, BornV_Crude
      DOUBLE PRECISION   gamiCR, gami, beta, RJac
*-----------------------------------------------
      m_XXXene =  m_CMSene                 ! hidden input for BornV_Crude
*-----------------------------------------------
      CALL BornV_MakeGami(m_XXXene,gamiCR,gami)
      IF( gamiCR .LE. 0d0 ) GOTO 800
*     Mapping  r => vv change  to improve on efficiency
      beta = -0.5d0
      CALL BornV_ReBin1(R,gamiCR,beta,m_vvmax,m_vv,RJac)
      CALL BornV_MakeISR(Rho)              ! uses m_XXXene and m_vv
      Rho = Rho*RJac
*-----------------------------------------------
* Translate R into m_vv and get QED (crude) density Rho
***** CALL BornV_MakeRho(R,Rho)
*-----------------------------------------------
* Born Xsection at s' = m_XXXene**2 *(1-vv)
      IF(m_KeyZet .EQ. -2) THEN   ! Artificial constant x-section for test runs
         BornV_RhoVesko1 = Rho* BornV_Crude(0d0)
      ELSE                        ! 1/(1-vv) because BornV_Crude is in R-units
         BornV_RhoVesko1 = Rho* BornV_Crude(m_vv)/(1d0-m_vv)
      ENDIF
      RETURN
 800  CONTINUE
      BornV_RhoVesko1 =0d0
      END

      SUBROUTINE BornV_MakeGami(CMSene,gamiCR,gami)
*//////////////////////////////////////////////////////////////////////////////
*//   Crude Gami as a function of CMSene                                     //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  CMSene, gamiCR, gami
      DOUBLE PRECISION  amel, svar, am2, beta
      INTEGER           KFbeam
*---------------------------------
      KFbeam = 11           ! KF=11 is electron
      amel   = m_amferm(KFbeam)
      am2  = (2d0*amel/CMSene)**2
      IF( am2 .GT. 1d0 ) GOTO 800
      beta = SQRT(1d0-am2)
      gami    = 2d0*m_alfpi *( DLOG((1+beta)**2/am2) -1d0)
      gamiCR  = 2d0*m_alfpi *  DLOG((1+beta)**2/am2)
      gamiCR  = gamiCR *m_Xenph         !!! enhancement of crude photon multiplicity
      IF(m_KeyWtm .EQ. 1) gamiCR=gami   !!! new, for very special tests
*-------------
      RETURN
 800  CONTINUE
      gamiCR = 0d0
      gami   = 0d0
      END

      SUBROUTINE BornV_MakeISR(Rho)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   This procedure is tightly related to ISR photon generation in Karlud   //
*//   It calculates Rho(m_vv, m_XXXene) QED crude Structure Function         //
*//                                                                          //
*//   m_AvMult is later used in KarLud_YFSini                                //
*//   m_YFSkon ,m_YFS_IR are later used in GPS_Make  and QED3_Make           //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION   Rho
      DOUBLE PRECISION   gami,  gamiCR,  BornV_Crude
      DOUBLE PRECISION   xBorn, DilJac0, beta, VoluMC
*-----------------------------
      CALL BornV_MakeGami(m_XXXene,gamiCR,gami)
      IF(m_vv .GT. m_vvmin) THEN
         DilJac0   = (1d0+1d0/SQRT(1d0-m_vv))/2d0
         m_AvMult  = gamiCR*DLOG(m_vv/m_vvmin)
         VoluMC    = gamiCR/m_vv *EXP( m_AvMult )    !!! Phase space Volume CRUDE
         m_YFS_IR  = -gami*DLOG(1d0/m_vvmin)         !!! IR part of YFS formfactor
         Rho       = VoluMC *EXP(m_YFS_IR)
      ELSE
         DilJac0   = 1d0
         m_AvMult  = 0d0
         VoluMC    = 1d0
* IMPORTANT:     The integral over Rho(v<vvmin) = YFS_IR = EXP(-gami*LOG(1/vvmin))
         m_YFS_IR  = -gami*DLOG(1d0/m_vvmin)         !!! IR part of YFS formfactor
         Rho       = 1d0/m_vv *gami*m_vv**gami
      ENDIF
      Rho =  Rho * DilJac0
* YFS formfactor, finite part, YFS_form_Factor = EXP(YFS_IR + YFSkon)
* YFSkon is delegated/exported to QED3 and GPS (not used here).
      m_YFSkon =  EXP(1/4d0 *gami + m_alfpi*( -.5d0  +m_pi**2/3d0) )
      m_YFS_IR =  EXP(m_YFS_IR)
      END

      SUBROUTINE BornV_MakeRho(R,Rho)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   This function is tightly related to ISR photon generation in Karlud    //
*//   It calculates Rho(vv) QED crude Structure Function at XXXene           //
*//   Translates R into m_vv                                                 //
*//                                                                          //
*//   m_vv is used in Karlud and other places                                //
*//   m_AvMult is later used in KarLud_YFSini                                //
*//   m_YFSkon ,m_YFS_IR are later used in GPS_Make  and QED3_Make           //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION   R,Rho
      DOUBLE PRECISION   gami,  gamiCR,  BornV_Crude
      DOUBLE PRECISION   xBorn, DilJac0, beta, RJac,  VoluMC
*-----------------------------
      CALL BornV_MakeGami(m_XXXene,gamiCR,gami)
      IF( gamiCR .LE. 0d0 ) GOTO 800
*     Mapping  r => vv change  to improve on efficiency
      beta = -0.5d0
      CALL BornV_ReBin1(R,gamiCR,beta,m_vvmax,m_vv,RJac)
      IF(m_vv .GT. m_vvmin) THEN
         DilJac0   = (1d0+1d0/SQRT(1d0-m_vv))/2d0
         m_AvMult  = gamiCR*DLOG(m_vv/m_vvmin)
         VoluMC    = gamiCR/m_vv *EXP( m_AvMult )    !!! Phase space Volume CRUDE
         m_YFS_IR  = -gami*DLOG(1d0/m_vvmin)         !!! IR part of YFS formfactor
         Rho       = VoluMC *EXP(m_YFS_IR)
      ELSE
         DilJac0   = 1d0
         m_AvMult  = 0d0
         VoluMC    = 1d0
* IMPORTANT:     The integral over Rho(v<vvmin) = YFS_IR = EXP(-gami*LOG(1/vvmin))
         m_YFS_IR  = -gami*DLOG(1d0/m_vvmin)         !!! IR part of YFS formfactor
         Rho       = 1d0/m_vv *gami*m_vv**gami
      ENDIF
      Rho =  Rho * DilJac0*RJac
* YFS formfactor, finite part, YFS_form_Factor = EXP(YFS_IR + YFSkon)
* YFSkon is delegated/exported to QED3 and GPS (not used here).
      m_YFSkon =  EXP(1/4d0 *gami + m_alfpi*( -.5d0  +m_pi**2/3d0) )
      m_YFS_IR =  EXP(m_YFS_IR)
      RETURN
 800  CONTINUE
      Rho  = 0d0
      m_vv = 0d0
      END


      DOUBLE PRECISION  FUNCTION BornV_Crude(vv)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*// This routine calculates CRUDE total Born cross section  SUMMED OVER KF.   //
*// It exploits the fact that born x. section = a + b*c + d*c**2              //
*// Hidden input is m_XXXene                                                  //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
*
      INTEGER KFf
      DOUBLE PRECISION   vv, svar1
      DOUBLE PRECISION   BornV_Differential
      DOUBLE PRECISION   Born, sum
* for brancher
      DOUBLE PRECISION   WMList(200),XsList(200)
      INTEGER j, KFlist(200), Nbranch
*-----------------------------------------------------------------------
      svar1  = (1-vv)*m_XXXene**2
* get from brancher list of KF's and of enhancement factors
      CALL MBrA_GetKFlist(Nbranch,KFlist)
      CALL MBrA_GetWMList(Nbranch,WMList)
      sum = 0d0
      DO j=1,Nbranch
         Born =0d0
         KFf=KFlist(j)
*///////////////////////////////////////////////////////////////////////////////
         Born= BornV_Differential( 0, Kff, svar1, 0.d0, 0.d0,0.d0, 0.d0,0.d0 )
*///////////////////////////////////////////////////////////////////////////////
* For light quarks u,d,s, special cut on mass (just in case)
         IF( (ABS(KFf) .GE. 1) .AND. (ABS(KFf) .LE. 3)) THEN
            IF( svar1 .LE. m_HadMin**2) Born=0d0
         ENDIF
* The amplification factor WM goes into crude normalization
* It is countered later on by the weight from MBrA_GenKF
         sum = sum +Born*WMList(j)
         XsList(j) = Born              !<---  WtMax=WMList(j) NOT included!!!
      ENDDO
* send back to bracher xsections for generation of KF
      CALL MBrA_SetXSList(XsList)
*------
      BornV_Crude =sum                 !<---  WtMax=WMList(j) IS included!!!
      END

      DOUBLE PRECISION  FUNCTION BornV_Differential(Mode,KFf,svar,CosThe,eps1,eps2,ta,tb)
*///////////////////////////////////////////////////////////////////////////////
*// Mode=0 it is CRUDE version for pure Born, no spin, no EW corrs.           //
*//                                                                           //
*// Mode=1 full result with electroweak corrs. spin etc. added.               //
*//        used in QED3 and all kind of tests                                 //
*//                                                                           //
*// Mode=3 for tests of pretabulation, GSW(s,theta) has to be provided from   //
*//        outside with help of BornV_SetGSW                                  //
*//                                                                           //
*// Note that in the test mode KeyEwl=0 and Mode=1 we use BornV_Simple        //
*// which perhaps will have to be changed in future besause lack of spin eff. //
*// At this stage however we are bound to use it because the KeyZet etc.      //
*// are implemented only in BornV_Simple and not in BornV_Dizet.              //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE

      INTEGER Mode
      DOUBLE PRECISION   svar,CosThe,eps1,eps2,ta,tb
      INTEGER KFf
      INCLUDE 'BornV.h'
      SAVE
      DOUBLE PRECISION   Born
      DOUBLE PRECISION   BornV_Dizet, BornV_Simple
*-----------------------------------------------------------------------------
      IF(     Mode .EQ. 1 ) THEN
         IF( m_KeyElw .EQ. 0 ) THEN
            Born= BornV_Simple( m_KFini,KFf,svar,CosThe)
         ELSE
*           Linear interpolation from tables, only for Mode=1 
            CALL BornV_InterpoGSW( ABS(KFf),  svar, CosThe)
            Born= BornV_Dizet( 1,m_KFini,KFf, svar, CosThe, eps1,eps2,ta,tb)
         ENDIF
      ELSEIF( Mode .EQ. 3 ) THEN
*           For test of pretabulation, BornV_SetGSW has to be invoked prior
            Born= BornV_Dizet( 1,m_KFini,KFf, svar, CosThe, eps1,eps2,ta,tb)
      ELSEIF( Mode .EQ. 0 ) THEN
         Born= BornV_Simple( m_KFini,KFf,svar,CosThe)
*        Another potential possibility, with a different threshold behavior is:
*        Born= BornV_Dizet( 0,m_KFini,KFf,svar,CosThe,0d0,0d0,0d0,0d0)
      ELSE
         WRITE(*,*) 'STOP in BornV_Differential: Mode =',Mode
         STOP
      ENDIF
      BornV_Differential = Born
      END


      DOUBLE PRECISION  FUNCTION BornV_Simple(KFi,KFf,svar,costhe)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*// This routine provides unsophisticated Born differential cross section     //
*// at the crude x-section level, with Z and gamma s-chanel exchange.         //
*// Note that it uses m_swsq from tables (Dizet) and not from xpar(503)       //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
*
      INTEGER KFi,KFf
      DOUBLE PRECISION   svar,costhe
*
      DOUBLE PRECISION   ss,T3e,Qe,deno,Ve,Ae
      DOUBLE PRECISION   ye,yf,xf,rechi,xe,amx2
      DOUBLE PRECISION   thresh,ff0,ff1,chi2
      DOUBLE PRECISION   t3f,amfin,af,vf,born,sum,qf
      DOUBLE PRECISION   BWD,Coef
      DOUBLE COMPLEX     PropW,WVPi
      INTEGER NCF
*----------------------
      ss = svar
* Z and gamma couplings to beams (electrons)
      T3e = m_T3f(KFi)  ! isospin, L-hand component
      Qe  = m_Qf( KFi)  ! electric charge

      deno= 4d0*sqrt(m_swsq*(1d0-m_swsq))
      Ve  = (2*T3e -4*Qe*m_swsq)/deno
      Ae  =  2*T3e              /deno

      NCf   = m_NCf(KFf)        ! number of colours
      T3f   = m_T3f(KFf)        ! isospin, L-hand component
      Qf    = m_Qf( KFf)        ! electric charge
      deno  = 4d0*sqrt(m_swsq*(1d0-m_swsq))
      Vf    = (2*T3f -4*Qf*m_swsq)/deno
      Af    =  2*T3f              /deno
* Switch off Z or gamma
      IF(m_KeyZet .EQ. 0) THEN
         Ve=0d0
         Ae=0d0
      ENDIF
      IF(m_KeyZet .EQ. 9) THEN
         Qe=0d0
         Qf=0d0
      ENDIF
c[[   BWD = (ss-m_MZ**2)**2 + (m_gammz*m_MZ)**2   !!! <--! fixed width
      BWD = (ss-m_MZ**2)**2 + (m_gammz*ss/m_MZ)**2
      chi2 = ss**2          /BWD
      rechi=(ss-m_MZ**2)*ss /BWD
      xe= Ve**2 +Ae**2
      xf= Vf**2 +Af**2
      ye= 2*Ve*Ae
      yf= 2*Vf*Af
      ff0= qe**2*qf**2 +2*rechi*qe*qf*Ve*Vf +chi2*xe*xf
      ff1=             +2*rechi*qe*qf*Ae*Af +chi2*ye*yf
      Born    = (1d0+ costhe**2)*ff0 +2d0*costhe*ff1
      IF(ABS(KFf).EQ.12) THEN
* Electron neutrino, extremely approximate, actually matters costhe=0 for spectrum ....
* rest is basically irrelevant/wrong
        Coef  =1.d0/2.d0/m_swsq
        Born    = Born+0.25*(0.75D0*(1d0-costhe)**2+0.75*(1d0+costhe)**2)*Coef**2*ss**2/(m_MZ**2*(1-m_swsq))**2
      ENDIF
*     Colour factor
      Born = NCf*Born
      IF( ABS(costhe) .GT. 1d0) WRITE(*,*) '----> BornV: costhe=',costhe
* This is a bit crude method of introducing threshold behaviour
* cos(theta) depencence incorrect!!!
      amfin = m_amferm(KFf)     ! mass
      IF(    svar .LE.  4d0*amfin**2) THEN
         thresh=0d0
      ELSEIF(svar .LE. 16d0*amfin**2) THEN
         amx2=4d0*amfin**2/svar
         thresh=sqrt(1d0-amx2)*(1d0+amx2/2d0)
      ELSE
         thresh=1d0
      ENDIF
      Born= Born*thresh
      BornV_Simple = Born
      END

      DOUBLE PRECISION  FUNCTION BornV_Integrated(KFfin,svar)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   !!!!!!!!!!!!!! USED ONLY IN SEMIANALYTICAL programs!!!!!!!!!!           //
*//                                                                           //
*// This routine calculates total Born cross section.                         //
*// It is NOT used in MC any more                                             //
*//                                                                           //
*// It exploits the fact that born x. section = a + b*c + d*c**2              //
*//                                                                           //
*// For KFfin = 0 we sum over all alowed flavours, otherwise,                 //
*// for KFfin.NE.0 we calculate xsect for the actual value of m_KFfin         //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
*
      INTEGER KFfin
      DOUBLE PRECISION   svar
      DOUBLE PRECISION   BornV_Differential
      DOUBLE PRECISION   Born,Sum
      INTEGER KFf
*-----------------------------------------------------------------------
* Selective Inclusive/Exclusive Loop over all final fermions
      Sum = 0d0
      DO KFf=1,20
         Born =0d0
         IF( m_IsGenerated(KFf) .NE.  0) THEN
            IF((KFfin .EQ. 0  )  .OR. ! Inclusive
     $         (KFfin .EQ. KFf)) THEN ! Exclusive
               Born= BornV_Differential( 0,Kff,svar, 0.d0, 0.d0,0.d0, 0.d0,0.d0 )
            ENDIF
         ENDIF
         Sum = Sum +Born
      ENDDO
      BornV_Integrated =Sum
      END



      DOUBLE PRECISION FUNCTION BornV_Dizet(Mode,KFi,KFf,svar,CosThe,eps1,eps2,ta,tb)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Calculates differential born cross section.                            //
*//   For Mode=0 pure Born and for Mode=1 electroweak corrs. are added.      //
*//   KFi,KFf can be also negative for antiparticle, in this case it is      //
*//   important to produce tables with correct input KFini, KFfin !!!        //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER          Mode,KFi,KFf
      DOUBLE PRECISION svar,CosThe,eps1,eps2,ta,tb
*-------------------------------------------------------------------------------------
      DOUBLE PRECISION  pi
      PARAMETER( pi =3.141592653589793238462643d0 )
      INTEGER    nneut
      PARAMETER( nneut = 1)
*-------------------------------------------------------------------------------------
      DOUBLE PRECISION 
     $  xupgi(2),                   ! Left/Right coupling gamma initial
     $  xupzi(2),                   ! Left/Right coupling Z     initial
     $  xupgf(2),                   ! Left/Right coupling gamma final
     $  xupzf(2)                    ! Left/Right coupling Z     final
*-------------------------------------------------------------------------------------
      DOUBLE PRECISION 
     $  t3e,                        ! Left izospin initial
     $  qe,                         ! Charge       initial
     $  t3f,                        ! Left izospin final
     $  qf                          ! Charge       final
      INTEGER
     $  kolor                       ! Color final fermion
*-------------------------------------------------------------------------------------
      DOUBLE COMPLEX   aborn(2,2),aphot(2,2),azett(2,2)
      DOUBLE COMPLEX   xupzfp(2),xupzip(2)
      DOUBLE COMPLEX   abornm(2,2),aphotm(2,2),azettm(2,2)
      DOUBLE COMPLEX   propa,propz
      DOUBLE COMPLEX   xr,xi,propw,aw(2,2)
      DOUBLE COMPLEX   xupf,xupi,xff(4),xfem,xfota,xrho,xke,xkf,xkef
      DOUBLE COMPLEX   xthing,xve,xvf,xvef,DelW
*
      INTEGER          j,i,ivini,kdumm,kff0,mode0,ivfin
      DOUBLE PRECISION xm2,xp2,xmw,regulm,regula,del1,xef,del0,factom,factor,thresh
      DOUBLE PRECISION xm3,helit,polar2,polar1,helic,born,amin,aizor,xgw,cost0,svar0
      DOUBLE PRECISION qem,qfm,xf,xe,beta,amfin,aizol,sinthe,xcoup
      DOUBLE PRECISION RSQV,RSQA
      DOUBLE PRECISION t,s
*-------------------------------------------------------------------------------------
      INTEGER    icont
      DATA       icont /0/
*/////////////////////////////////////////////////////////////////////////////
* Translation table KF-->IV
      INTEGER IV(-16:16)
      DATA IV / -1, -2, -1, -2, -1, -2, 4*0, -3, -4, -3, -4, -3, -4,  0,  
     $           4,  3,  4,  3,  4,  3, 4*0,  2,  1,  2,  1,  2,  1    /
*/////////////////////////////////////////////////////////////////////////////
      DATA xi/(0.d0,1.d0)/,xr/(1.d0,0.d0)/
      DATA xgw/2.5d0/
* To be sure that initialization starts properly
      DATA Mode0,svar0,cost0,KFf0 /-155,-155.d0,-156.d0,-99/
*-------------------------------------------------------------------------------------

*////////////////////////////////////////////////////////////////////////
*//    Save CPU for the same svar, CosThe  and varying spins ta, tb    //
*////////////////////////////////////////////////////////////////////////
      IF (Mode.NE.Mode0 .OR. svar.NE.svar0 .OR. CosThe.NE.cost0 
     $                  .OR. KFf.NE.KFf0  ) THEN
         Mode0  = Mode
         svar0  = svar
         cost0  = CosThe
         KFf0   = KFf
*////////////////////////////////////////////////////////////////////////
*//               Coupling constants                                   //
*////////////////////////////////////////////////////////////////////////
c[[[[[[[[[[[[[[[[[[[[[[!!!!!!!!!!!!!!!!!!!!!!
c      icont=icont+1
c      IF(icont.LE.200) THEN
c         write(*,*) '|||||||||||||||||||||BornV|||||||||||||||||||||||||||||||||||||'
c         write(*,'(a,8g22.14)') 'sqrt(svar),costhe=',sqrt(svar),costhe
c         write(*,'(a,8g22.14)') 'QCDcor=',m_QCDcor
c      ENDIF
c]]]]]]]]]]]]]]]]]]]]]]!!!!!!!!!!!!!!!!!!!!!!
         amin  = m_amferm(ABS(KFi))
         IVini = IV(KFi)
         CALL BornV_givizo( IVini, 1,aizor,qe,kdumm)
         CALL BornV_givizo( IVini,-1,aizol,qe,kdumm)
         xupgi(1)=qe
         xupgi(2)=qe
         t3e    = aizol+aizor
         xupzi(1)=(aizor-qe*m_swsq)/sqrt(m_swsq*(1-m_swsq))
         xupzi(2)=(aizol-qe*m_swsq)/sqrt(m_swsq*(1-m_swsq))
*
         amfin = m_amferm(ABS( KFf ))
         IVfin = IV( KFf )
         CALL BornV_givizo( IVfin, 1,aizor,qf,kolor)
         CALL BornV_givizo( IVfin,-1,aizol,qf,kolor)
         xupgf(1)=qf
         xupgf(2)=qf
         t3f    =  aizol+aizor
         xupzf(1)=(aizor -qf*m_swsq)/sqrt(m_swsq*(1-m_swsq))
         xupzf(2)=(aizol -qf*m_swsq)/sqrt(m_swsq*(1-m_swsq))
*
         sinthe = sqrt(1.d0-CosThe**2)
         beta   = SQRT(MAX(0d0,1d0-4d0*amfin**2/svar))
c[[[[[!!!!!!!!!!!!!!!!!!!!!!!
ccc         beta=1d0
c]]]]]!!!!!!!!!!!!!!!!!!!!!!!

* Multiply axial coupling by beta factor.
         xupzfp(1)= 0.5d0*(xupzf(1)+xupzf(2))+0.5*beta*(xupzf(1)-xupzf(2))
         xupzfp(2)= 0.5d0*(xupzf(1)+xupzf(2))-0.5*beta*(xupzf(1)-xupzf(2))
         xupzip(1)= 0.5d0*(xupzi(1)+xupzi(2))     +0.5*(xupzi(1)-xupzi(2))
         xupzip(2)= 0.5d0*(xupzi(1)+xupzi(2))     -0.5*(xupzi(1)-xupzi(2))
* Final state vector coupling
         xupf     = 0.5d0*(xupzf(1)+xupzf(2))
         xupi     = 0.5d0*(xupzi(1)+xupzi(2))
         xthing   = 0d0
*////////////////////////////////////////////////////////////////////////
*                          Propagators                                 //
*////////////////////////////////////////////////////////////////////////
         IF (Mode .EQ. 0 ) THEN
            propa =1d0/svar
            propz =1d0/dcmplx( svar -m_MZ**2, svar/m_MZ *m_gammz )
            RSQV=1d0
            RSQA=1d0
         ELSE
* Multiply axial coupling by beta factor. 
* Add formfactors initialisation of s-dependent electro-weak form factors and
* photonic vacuum polarisation 
* (electro-weak box contributions left out here, they depend on acos)
            CALL BornV_GetQCDcor2(KFf,RSQV,RSQA)
            xff(1)=m_GSW(1)
            xff(2)=m_GSW(2)
            xff(3)=m_GSW(3)
            xff(4)=m_GSW(4)
***         xffa  =UNDEFINED !!!!
            xfem  =m_GSW(6)
            xfota =m_GSW(7)
*-------------------------------------------------
            xrho =xff(1)
            xke  =xff(2)
            xkf  =xff(3)
            xkef =xff(4)
            qfm =dabs(qf)
            qem =dabs(qe)
            xe   =  1.d0 -4.d0*m_swsq*qem
            xf   =  1.d0 -4.d0*m_swsq*qfm
            xef  = -1.d0 +xe +xf +16.d0*qem*qfm*m_swsq*m_swsq ! xef=xe*xf !!!
            xve  =  1.d0 -4.d0*m_swsq*qem*xke
            xvf  =  1.d0 -4.d0*m_swsq*qfm*xkf
            xvef = -1.d0 +xve +xvf +16.d0*qem*qfm*m_swsq*m_swsq*xkef
* Multiply axial  coupling by beta factor.
* Multiply vector coupling by form-factor.
* Multiply final vector by RSQV and final axial by RSQA (QCD corrections)
            xupgf(1)=xupgf(1)*RSQV
            xupgf(2)=xupgf(2)*RSQV
            xupzfp(1)=0.5d0*(xupzf(1)+xupzf(2))*xvf/xf*RSQV  +0.5*(xupzf(1)-xupzf(2))*beta*RSQA !
            xupzfp(2)=0.5d0*(xupzf(1)+xupzf(2))*xvf/xf*RSQV  -0.5*(xupzf(1)-xupzf(2))*beta*RSQA !
            xupzip(1)=0.5d0*(xupzi(1)+xupzi(2))*xve/xe  +0.5*(xupzi(1)-xupzi(2)) !
            xupzip(2)=0.5d0*(xupzi(1)+xupzi(2))*xve/xe  -0.5*(xupzi(1)-xupzi(2)) !
* Final state vector coupling
            xupf     =0.5d0*(xupzf(1)+xupzf(2))*xvf/xf*RSQV
* Double vector formfactor thing
            xthing=0.25d0*(xupzf(1)+xupzf(2))*(xupzi(1)+xupzi(2))*(xvef/xef-xvf*xve/xe/xf)*RSQV !
            propa =1d0/svar/(2d0-xfem)
            propz =1d0/dcmplx(svar-m_MZ**2,svar/m_MZ*m_gammz)
* Replace Born normalization of Z propagator by the better one
            del1 =m_Gmu *m_MZ**2 *m_AlfInv/(DSQRT(2.d0)*8.d0*pi)
            del0 =1.d0/(m_swsq*(1.d0-m_swsq))/16.d0
            propz = propz*del1/del0*xrho
c[[[[[[[[[[[[[
c      IF(icont.LE.20) THEN
c         write(*,'(a,5g22.14)') '   propa= ', propa
c         write(*,'(a,5g22.14)') '   propz= ', propz
c         write(*,'(a,5g22.14)') '   xrho = ', xrho
c         write(*,'(a,5g22.14)') '   xke  = ', xke
c         write(*,'(a,5g22.14)') '   xkf  = ', xkf
c         WRITE(*,'(a,5g22.14)') '   xkef = ', xkef/(xke*xkf)
c         write(*,'(a,5g22.14)') '    swsq= ', m_swsq
c      ENDIF
c]]]]]]]]]]]]]
         ENDIF ! (Mode .EQ. 0)
*////////////////////////////////////////////////////////////////////////
*//             Additional Spin amplitudes in neutrino case            //
*////////////////////////////////////////////////////////////////////////
         DO i=1,2
            DO j=1,2
               aw(i,j)=(0.d0,0.d0)
            ENDDO
         ENDDO
         IF (iabs(IVfin) .EQ. 1.and.abs(kff).eq.12) THEN
            IF(Mode .EQ. 0) THEN
               xmw=m_MZ*dsqrt(1d0-m_swsq)
               xmw=m_MW         ! new!!!
               xcoup=1.d0/2.d0/m_swsq
               IF (IVini .LT. 0) THEN
                  aw(1,1)= -DCMPLX(xcoup*(1.d0+CosThe))/xmw/xmw
               ELSE
                  aw(2,2)= -DCMPLX(xcoup*(1.d0+CosThe))/xmw/xmw
               ENDIF
            ELSE
               t= -svar*(1.d0-CosThe)/2d0
               s= svar
cc               xmw=m_MZ*dsqrt(1d0-m_swsq)
cc               xmw=m_MW         ! new!!!
cc               xgw=0d0          ! new!!!
cc               xp2=(svar*(1.d0-CosThe)/2.+xmw*xmw)**2+(xmw*xgw)**2
cc               propw=dcmplx(-(svar*(1.d0-CosThe)/2+xmw*xmw)/xp2)
cc               propw=propw-xi*dcmplx(xmw*xgw/xp2)
               propw=DCMPLX( 1d0/(t-m_MW**2) )
               xcoup=1.d0/2.d0/m_swsq
               del1 =m_Gmu *m_MW**2 *m_AlfInv/(DSQRT(2d0)*pi) ! corrected
               del0 =1.d0/m_swsq/2.d0
               propw=propw*DCMPLX(del1/del0)
c[[[[
cccc           DelW= 1D0/m_AlfInv/m_pi/2*(-3D0/2*LOG(s/m_MW**2)+1D0/2*(LOG(-t/s))**2-m_pi**2/6+2D0)
cccc           ROW=ROW+AL1PI/2*(-3D0/2*ALSMW+1D0/2*(LOG(-TT/S))**2   ! modified
cccc        &                   -2D0/3*PI2+2D0)                      !   row           
c]]]]
               DelW=+( +3d0/2d0*LOG(m_MW**2/amin**2) +0.5d0*(LOG(-t/s))**2 )
     $              -( +3d0/2d0*LOG(s/amin**2) +4d0/6d0*m_pi**2 -2d0) ! (b) true F_1 s-chanel
cccc $              -( +3d0/2d0*LOG(s/amin**2) +1d0/6d0*m_pi**2 -2d0) ! (a) from Zbyszek&Tord
               DelW=DelW *0.5d0/(m_AlfInv*m_pi)
               propw=propw*(m_GSW(5) +DelW)
               IF (IVini .LT. 0) THEN
                  aw(1,1)= propw*dcmplx(xcoup*(1.d0+CosThe)) !orig. 1+CosThe
               ELSE
                  aw(2,2)= propw*dcmplx(xcoup*(1.d0+CosThe)) !orig. 1+CosThe
               ENDIF
            ENDIF
         ENDIF
*////////////////////////////////////////////////////////////////////////
*//             Spin amplitudes   Z+gamma case                         //
*////////////////////////////////////////////////////////////////////////
         DO i=1,2
            DO j=1,2
               regula= (3-2*i)*(3-2*j) + CosThe
               regulm=-(3-2*i)*(3-2*j) * sinthe *2.d0*amfin/sqrt(svar)
               aphot(i,j)=propa*(xupgi(i) *xupgf(j)*regula)
               azett(i,j)=propz*(xupzip(i)*xupzfp(j)+xthing)*regula
               aborn(i,j)=aphot(i,j)+azett(i,j)+aw(i,j)
               aphotm(i,j)= propa*dcmplx(0d0,1d0)  *xupgi(i)*xupgf(j)    *regulm !
               azettm(i,j)= propz*dcmplx(0d0,1d0)*(xupzip(i)*xupf+xthing)*regulm !
               abornm(i,j)=aphotm(i,j)+azettm(i,j)
c[[[[[[[[[[[[[
c               IF(icont.LE.20) THEN
c                  write(*,'(a,2i5,5g22.14)') 'amplit= ',i,j, 
c     $                 propa*xupgi(i) *xupgf(j) + propz*(xupzip(i)*xupzfp(j)+xthing)
c               ENDIF
c]]]]]]]]]]]]]
            ENDDO
         ENDDO
      ENDIF
*////////////////////////////////////////////////////////////////////////
*//    Saving CPU trick ENDs here                                      //
*////////////////////////////////////////////////////////////////////////
*////////////////////////////////////////////////////////////////////////
*//           Differential X-section out of spin amplituds             //
*//  Helicity conservation explicitly obeyed:                          //
*//  Only diagonal elements of the spin density matrices.              //
*//  (Only longitudinal polarizations)                                 //
*////////////////////////////////////////////////////////////////////////
      polar1 =  (eps1)
      polar2 = (-eps2)
      Born   =  0d0
      DO i=1,2
         helic= 3-2*i
         DO j=1,2
            helit=3-2*j
            factor=kolor*(1d0+helic*polar1)*(1d0-helic*polar2)/4d0
            factom=factor*(1+helit*ta)*(1-helit*tb)
            factor=factor*(1+helit*ta)*(1+helit*tb)
            IF(iabs(IVfin) .NE. 1) THEN
*     Normal case
*     (mass terms included in Born. is it better ??????)
               Born=Born+cdabs(aborn(i,j))**2*factor
               IF (Mode .NE. 0) THEN
                  Born=Born+CDABS(abornm(i,j))**2*factom
               ENDIF
            ELSE
*     Neutrino case
               xm2=cdabs( aborn(i,j))**2 !  +(nneut-1)*cdabs(azett(i,j))**2
               xm3=cdabs(abornm(i,j))**2 ! +(nneut-1)*cdabs(azettm(i,j))**2
               Born=Born+(xm2+xm3)*factor
            ENDIF
         ENDDO
      ENDDO
* phase space threshold factor, and multiply by svar**2 to get R-units!
      IF (svar .GT. 4d0*amfin**2) THEN
         thresh=sqrt(1-4d0*amfin**2/svar)
         Born = Born*svar**2*thresh
      ELSE
         Born=0.d0
      ENDIF
      BornV_Dizet = Born
      END


      SUBROUTINE BornV_InterpoGSW(KFf,svar,CosThe)
*//////////////////////////////////////////////////////////////////////////
*//
*//  Calculates GSW formfactors from tables using linear interpolation
*//
*//////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION   svar,CosThe
      DOUBLE COMPLEX     xff(4),xfem,xfota
      INTEGER            kk,i,j, KFf
      DOUBLE PRECISION   x,y,h,hy,ww
*----------------------------------------------------------------------------
      ww = SQRT(svar)
      m_WminZ = m_MZ-m_WdelZ
      m_WmaxZ = m_MZ+m_WdelZ

      IF(  (ww.GE.m_WminZ) .AND. (ww.LE.m_WmaxZ)    ) THEN
* LEP1 near Z0 resonance 
         x= (ww-m_WminZ)/(m_WmaxZ-m_WminZ)
         i= MIN( INT(m_poin2*x)+1, m_poin2)
         h= x*m_poin2-DFLOAT(i-1)
         IF(m_poTh2.EQ.0) THEN
            DO kk=1,m_poinG
               m_GSW(kk) = m_czz( i,1,kk,KFf)*(1-h) +m_czz(i+1,1,kk,KFf)*h !
            ENDDO
         ELSE
            y= (1d0+CosThe)/2d0
            j= MIN( INT(m_poTh2*y)+1, m_poTh2)
            hy= y*m_poTh2-DFLOAT(j-1)
            DO kk=1,m_poinG
               m_GSW(kk)=m_czz( i,j,  kk,KFf)*(1-h)*(1d0-hy) +m_czz(i+1,j,  kk,KFf)*h*(1d0-hy) !
     $                  +m_czz( i,j+1,kk,KFf)*(1-h)*hy       +m_czz(i+1,j+1,kk,KFf)*h*hy !
            ENDDO
         ENDIF
         DO kk=1,m_poinQ
            m_QCDcorR(kk)=m_szz( i,kk,KFf)*(1-h) +m_szz(i+1,kk,KFf)*h
         ENDDO
      ELSEIF(  (ww.GE.m_WminLEP1) .AND. (ww.LE.m_WmaxLEP1) ) THEN
* LEP1 outside Z0 and low energies
         x= LOG( ww/m_WminLEP1) / LOG(m_WmaxLEP1/m_WminLEP1)
         i= MIN( INT(m_poin1*x)+1, m_poin1)
         h= x*m_poin1-DFLOAT(i-1)
         DO kk=1,m_poinG
            m_GSW(kk)    =m_cyy( i,kk,KFf)*(1-h) +m_cyy(i+1,kk,KFf)*h
         ENDDO
         DO kk=1,m_poinQ
            m_QCDcorR(kk)=m_syy( i,kk,KFf)*(1-h) +m_syy(i+1,kk,KFf)*h
         ENDDO
      ELSEIF(  (ww.GE.m_WmaxLEP1) .AND. (ww.LE.m_WmaxLEP2) ) THEN
* in the LEP2 region
         x= (ww-m_WmaxLEP1)/(m_WmaxLEP2-m_WmaxLEP1)
         i= MIN( INT(m_poin3*x)+1, m_poin3) 
         h= x*m_poin3-DFLOAT(i-1)
         y= (1d0+CosThe)/2d0
         j= MIN( INT(m_poTh3*y)+1, m_poTh3)
         hy= y*m_poTh3-DFLOAT(j-1)
* EW complex form-factors
         DO  kk=1,m_poinG
            m_GSW(kk)=
     $          m_ctt(i,j  , kk,KFf)*(1-h) *(1d0-hy)  +m_ctt(i+1,  j, kk,KFf) *h*(1d0-hy) !
     $         +m_ctt(i,j+1, kk,KFf)*(1-h) *hy        +m_ctt(i+1,j+1, kk,KFf) *h*hy       !
         ENDDO
* QCD correction
         DO kk=1,m_poinQ
            m_QCDcorR(kk)=m_stt( i,kk,KFf)*(1-h) +m_stt(i+1,kk,KFf)*h
         ENDDO
      ELSEIF(  (ww.GE.m_WmaxLEP2) .AND. (ww.LE.m_WmaxNLC) ) THEN
* in the NLC region
         x= (ww-m_WmaxLEP2)/(m_WmaxNLC-m_WmaxLEP2)
         i= MIN( INT(m_poin4*x)+1, m_poin4)
         h= x*m_poin4-DFLOAT(i-1)
         y= (1d0+CosThe)/2d0
         j= MIN( INT(m_poTh4*y)+1, m_poTh4)
         hy= y*m_poTh4-DFLOAT(j-1)
* EW complex form-factors
         DO  kk=1,m_poinG
            m_GSW(kk)=
     $         m_clc(i,  j, kk,KFf) *(1-h) *(1d0-hy)  +m_clc(i+1,  j, kk,KFf) *h *(1d0-hy)  !
     $        +m_clc(i,j+1, kk,KFf) *(1-h) *hy        +m_clc(i+1,j+1, kk,KFf) *h *hy        !
         ENDDO
* QCD correction
         DO kk=1,m_poinQ
            m_QCDcorR(kk)=m_slc( i,kk,KFf)*(1-h) +m_slc(i+1,kk,KFf)*h
         ENDDO
      ELSE
         PRINT *,'STOP in BornV_InterpoGSW: s out of predefined range, ww=', ww
         STOP
      ENDIF
      m_QCDcor = m_QCDcorR(1)-1d0 ! <--- obsolete!!!
      END                        !BornV_GetGSW


      SUBROUTINE BornV_givizo(idferm,ihelic,sizo3,charge,kolor)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Provides electric charge and weak izospin of a family fermion where      //
*// idferm =           1,        2,        3,         4,                     //
*// denotes:    neutrino,   lepton,       up,      down   (quark)            //
*// negative idferm=-1,-2,-3,-4, denotes corresponding antiparticle          //
*// ihelic =     +1,  -1   denotes  right and left handednes ( chirality)    //
*// sizo3 is third projection of weak izospin (plus minus half)              //
*// and charge is electric charge in units of electron charge                //
*// kolor is a qcd colour, 1 for lepton, 3 for quarks                        //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER          idferm,ihelic,kolor
      DOUBLE PRECISION sizo3,charge
*
      INTEGER          lepqua,iupdow,ic,ih,idtype
*------------------------------------------------------------------------------
      IF(idferm  .EQ.  0  .OR.  iabs(idferm)  .GT.  4) GOTO 901
      IF(iabs(ihelic)  .NE.  1)                GOTO 901
      ih  =ihelic
      idtype =iabs(idferm)
      ic  =idferm/idtype
      lepqua=INT(idtype*0.4999999d0)
      iupdow=idtype-2*lepqua-1
      charge  =(-iupdow+2d0/3d0*lepqua)*ic
      sizo3   =0.25d0*(ic-ih)*(1-2*iupdow)
      kolor=1+2*lepqua
* note that conventionaly z0 coupling is
* xoupz=(sizo3-charge*swsq)/sqrt(swsq*(1-swsq))
      RETURN
 901  print *,' STOP in BornV_givizo: wrong params.'
      STOP
      END


      DOUBLE PRECISION  FUNCTION BornV_Sig0nb(CMSene)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//  provides pointlike muon x-section in nanobarns                          //
*//  for normalization purpose                                               //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION   pi
      PARAMETER (pi =3.1415926535897932d0)
      DOUBLE PRECISION  CMSene
*---------------------------------
      BornV_Sig0nb =  4d0*pi/(m_AlfInv**2*3d0*CMSene**2)*m_gnanob
      END ! BornV_Sig0nb

*//////////////////////////////////////////////////////////////////////////////
*//                  Getters and setters                                     //
*//////////////////////////////////////////////////////////////////////////////

      SUBROUTINE BornV_GetParticle(KFferm, mass, Qf, T3f, NCf)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KFferm, NCf
      DOUBLE PRECISION   mass, Qf, T3f
      INTEGER KF
      KF = ABS(KFferm)
      mass  = m_amferm( KF)
      Qf    = m_Qf(     KF)
      T3f   = m_T3f(    KF)
      NCf   = m_NCf(    KF)
      END                       ! BornV_GetParticle

      SUBROUTINE BornV_GetIsGenerated(KFferm,IsGenerated)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KFferm, IsGenerated
      IsGenerated   = m_IsGenerated(KFferm)
      END                       ! BornV_GetIsGenerated

      SUBROUTINE BornV_SetKF(KFferm)
*//////////////////////////////////////////////////////////////////////////////
*//   set just one fermion                                                   //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KFferm,k
      DO k=1,20
         m_IsGenerated(k)=0
      ENDDO
      m_IsGenerated(KFferm) =1d0
      END                       ! BornV_GetIsGenerated

      DOUBLE PRECISION  FUNCTION BornV_GetMass(KFferm)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KFferm
      BornV_GetMass = m_amferm(ABS(KFferm))
      END ! BornV_GetMass

      DOUBLE PRECISION  FUNCTION BornV_GetAuxPar(KFferm)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KFferm
*------
      BornV_GetAuxPar = m_AuxPar(ABS(KFferm))
      END ! BornV_GetAuxPar

      DOUBLE PRECISION  FUNCTION BornV_GetCharge(KFferm)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KFferm
*------
      BornV_GetCharge = m_Qf(ABS(KFferm))
      END ! BornV_GetCharge

      INTEGER FUNCTION BornV_GetColor(KFferm)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KFferm
*------
      BornV_GetColor = m_NCf(ABS(KFferm))
      END ! BornV_GetColor


      SUBROUTINE BornV_SetKeyQCD(KeyQCD)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KeyQCD
      m_KeyQCD = KeyQCD
      END

      SUBROUTINE BornV_GetKeyQCD(KeyQCD)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KeyQCD
      KeyQCD = m_KeyQCD
      END

      SUBROUTINE BornV_SetKeyElw(KeyElw)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KeyElw
      m_KeyElw = KeyElw
      END

      SUBROUTINE BornV_GetKeyElw(KeyElw)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KeyElw
      KeyElw = m_KeyElw
      END

      SUBROUTINE BornV_GetKeyZet(KeyZet)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KeyZet
*
      KeyZet = m_KeyZet
      END

      SUBROUTINE BornV_SetKeyZet(KeyZet)
*//////////////////////////////////////////////////////////////////////////////
*//   for tests only                                                         //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER KeyZet
*
      m_KeyZet = KeyZet
      END

      SUBROUTINE BornV_SetCMSene(CMSene)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  CMSene
*
      m_CMSene = CMSene
      END

      SUBROUTINE BornV_SetMZ(MZ)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  MZ
*------------------
      m_MZ = MZ
      END

      SUBROUTINE BornV_GetMZ(MZ)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  MZ
*------------------
      MZ = m_MZ
      END

      SUBROUTINE BornV_GetGammZ(GammZ)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  GammZ
*------------------
      GammZ = m_GammZ
      END

      SUBROUTINE BornV_GetGmu(Gmu)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  Gmu
*------------------
      Gmu = m_Gmu
      END

      SUBROUTINE BornV_GetSwsq(Swsq)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  Swsq
*------------------
      Swsq = m_swsq
      END

      SUBROUTINE BornV_GetAlfInv(AlfInv)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  AlfInv
*------------------
      AlfInv = m_AlfInv
      END

      SUBROUTINE BornV_GetAvMult(AvMult)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION    AvMult
*------------------
      AvMult = m_AvMult
      END

      SUBROUTINE BornV_GetYFSkon(YFSkon)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Get finite part of YFS form-factor                                     //
*//   Used in QED3.f                                                         //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION    YFSkon
*------------------
      YFSkon = m_YFSkon
      END

      SUBROUTINE BornV_GetYFS_IR(YFS_IR)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Get IR (cut-off dependend) part of ISR YFS form-factor                 //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION    YFS_IR
*------------------
      YFS_IR = m_YFS_IR

      END

      SUBROUTINE BornV_GetVV(vv)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  vv
*
      vv = m_vv
      END ! BornV_GetVV

      SUBROUTINE BornV_GetVXX(vv,x1,x2)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION  vv,x1,x2
*
      vv = m_vv
      x1 = m_x1
      x2 = m_x2
      END ! BornV_GetVXX

      SUBROUTINE BornV_GetGSW(GSW)
*//////////////////////////////////////////////////////////////////////////
*//
*//  Exports  GSW formfactors, called in GPS/CEEX
*//  BornV_InterpoGSW has to be called before
*//
*//////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE COMPLEX    GSW(*)
      INTEGER      k
*     ---------------------------------------------------------------------
      DO k=1,m_poinG
         GSW(k) = m_GSW(k)
      ENDDO
      END

      SUBROUTINE BornV_SetGSW(GSW)
*//////////////////////////////////////////////////////////////////////////
*//
*//  For special tests of pretabulation GSW values can be set form outside
*//
*//////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE COMPLEX    GSW(*)
      INTEGER      k
*     ---------------------------------------------------------------------
      DO k=1,m_poinG
          m_GSW(k) =GSW(k)
      ENDDO
      END

      SUBROUTINE BornV_GetQCDcor2(KFf,RSQV,RSQA)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Get QCD correction factors for vercor and axial couplings              //
*//   QED corrections has to be removed from the Dizet QCD formfactor.       //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      INTEGER   KFf
      DOUBLE PRECISION   RSQV,RSQA, QEDcor
      INTEGER            KeyFSR

*     Standard case KeyQCD=1:
*     QCD included through K-factor of Dizet,
*     FSR O(alpha) QED corr. from Dizet is excluded (but alpha*alpha_s is included!)
      IF( ABS(KFf) .LE. 6 ) THEN
         QEDcor = m_Qf(KFf)**2 *3d0/4d0 *m_alfpi
         RSQV = SQRT(m_QCDcorR(1) -QEDcor)        ! Quarks
         RSQA = SQRT(m_QCDcorR(2) -QEDcor)
      ELSE
         RSQV = 1d0             ! Leptons
         RSQA = 1d0
      ENDIF
      IF( m_KeyQCD .EQ. 0) THEN
*     Special KeyQCD=0: for excluding QCD correction whatsoever
         RSQV = 1d0
         RSQA = 1d0
      ENDIF
*
      CALL KK2f_GetKeyFSR(KeyFSR)
      IF( (m_KeyQCD .EQ. 2) .AND. (KeyFSR .EQ .0) ) THEN
*     Special KeyQCD=2: for including QED FSR correction "by-hand"
         IF( ABS(KFf) .LE. 6 ) THEN
            RSQV = SQRT(m_QCDcorR(1)) ! Quarks, QEDcor provided by Dizet
            RSQA = SQRT(m_QCDcorR(2))
         ELSE
            QEDcor = m_Qf(KFf)**2 *3d0/4d0 *m_alfpi
            RSQV = SQRT(1d0+QEDcor) ! Leptons, explicit QEDcor
            RSQA = SQRT(1d0+QEDcor)
         ENDIF
      ENDIF
      END


      SUBROUTINE BornV_GetQCDcorR(QCDcorR)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Get QCD correction factor, provided by Dizet                           //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION    QCDcorR(m_poinQ)
      INTEGER i
      DO i=1,m_poinQ
         QCDcorR(i) = m_QCDcorR(i)
      ENDDO
      END

      SUBROUTINE BornV_GetQCDcor(QCDcor)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Get QCD correction factor, provided by Dizet                           //
*//   !!!!! Obsolete !!!!!!!!!!!!!!
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION    QCDcor
      QCDcor = m_QCDcor
      END

      SUBROUTINE BornV_SetQCDcor(QCDcor)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Set QCD correction factor, for tests of pretabulation                  //
*//   !!!!! Obsolete !!!!!!!!!!!!!!
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION    QCDcor
      m_QCDcor    = QCDcor
      END

      SUBROUTINE BornV_GetMW(MW)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Get Mass of W
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION    MW
      MW = m_MW
      END

      SUBROUTINE BornV_GetGammW(GammW)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Get width of W
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BornV.h'
      DOUBLE PRECISION    GammW
      GammW = m_GammW
      END

*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                      End of CLASS  BornV                                 //
*//////////////////////////////////////////////////////////////////////////////

*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                     Pseudo-CLASS  Vesk1                                  //
*//                                                                          //
*//   Purpose: generate one-dimensional arbitrary distribution rho(x)        //
*//   where x is in the range (0,1).                                         //
*//   Now Denser grid!!!!                                                    //
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
*
*
*
      SUBROUTINE Vesk1_Initialize(funsko,crude)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Vesk1.h'
*
      DOUBLE PRECISION  funsko, crude
      EXTERNAL funsko
*
      DOUBLE PRECISION      drvec(10)
      DOUBLE PRECISION      wt,sum,zmx,ymax
      INTEGER    j,k,jdiv
      INTEGER    iniran
*-----------------------------------------------------------------------------
* initialisation part, see vinsko for more comments
      wt   = 0.d0
      swt  = 0.d0
      sswt = 0.d0
      nevs=  -911119
* initialisation part, sampling distribution funsko
* and filling matrices xx,yy,zint etc.
      jmax =1
      xx(1)=0.d0
      xx(2)=1.d0
      yy(1)=funsko(xx(1))
      yy(2)=funsko(xx(2))
      IF(yy(1) .LT. 0.d0 .OR. yy(2) .LT. 0.d0) go to 999
      zint(1)=.5d0*(yy(2)+yy(1))*(xx(2)-xx(1))
*
      jdiv=1
      DO k=1,jlim2-1
         IF(jmax .LT. jlim1) THEN
*  note that Vesk1_Divide increments jmax=jmax+1 in every CALL
            CALL Vesk1_Divide(funsko,jdiv)
            jdiv=jdiv+2
            IF(jdiv .GT. jmax) jdiv=1
         ELSE
            jdiv=1
            zmx=zint(1)
            DO j=1,jmax
               IF(zmx .LT. zint(j)) THEN
                  zmx=zint(j)
                  jdiv=j
               ENDIF
            ENDDO
            CALL Vesk1_Divide(funsko,jdiv)
         ENDIF
      ENDDO
*     
*...  final administration, normalizing zint etc.
      zsum =0.d0
      DO j=1,jmax
         ymax= max( yy(j+1),yy(j))
         zint(j)=ymax*(xx(j+1)-xx(j))
         zsum=zsum+zint(j)
      ENDDO
      sum=0.
      DO j=1,jmax
         sum=sum+zint(j)
         zint(j)=sum/zsum
      ENDDO
      crude = zsum
      RETURN
*
 999  WRITE(*,'(a)') ' **** STOP in vesk01, negative value of funsko '
      STOP
      END       ! Vesk1_Initialize


      SUBROUTINE Vesk1_Divide(funsko,jdiv)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// this routine belongs to vesko1 package                                   //
*// it sudivides into two equal parts the interval                           //
*// (xx(jdiv),xx(jdiv+1))  in the 1-dim. latice                              //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Vesk1.h'
*
      DOUBLE PRECISION  funsko
      EXTERNAL          funsko
      DOUBLE PRECISION  xnew
      INTEGER           j,jdiv
*----------------------------------------------------------------------------
      xnew=.5d0*(xx(jdiv) +xx(jdiv+1))
      DO j=jmax,jdiv,-1
         xx(j+2)  =xx(j+1)
         yy(j+2)  =yy(j+1)
         zint(j+1)=zint(j)
      ENDDO
      xx(jdiv+1)= xnew
      yy(jdiv+1)= funsko(xnew)
      IF(yy(jdiv+1) .LT. 0.) GOTO 999
      zint(jdiv)  =.5d0*(yy(jdiv+1)+yy(jdiv)  )*(xx(jdiv+1)-xx(jdiv)  )
      zint(jdiv+1)=.5d0*(yy(jdiv+2)+yy(jdiv+1))*(xx(jdiv+2)-xx(jdiv+1))
      jmax=jmax+1
      RETURN
 999  CONTINUE
      WRITE(*,'(a)') ' *** STOP Vesk1_Divide, negative value of funsko '
      STOP
      END


      SUBROUTINE Vesk1_Make(funsko,x,fn,wt)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Vesk1.h'
*
      DOUBLE PRECISION  funsko
      EXTERNAL funsko
*
      DOUBLE PRECISION      fn,x,d,yymax,wt,rnumb
      REAL                  rvec(10)
      INTEGER    j,jstop,iwarm
      INTEGER    klower,krange,kurrent
*
      DATA iwarm /0/

* check if initialization was done (not trivial in old poor fortran)
      IF(iwarm .EQ. 0) THEN
         iwarm=1
         IF(nevs .EQ.  -911119 ) THEN
            nevs = 0
         ELSE
            GOTO 901
         ENDIF
      ENDIF
      
      CALL PseuMar_MakeVec(rvec,1)
      rnumb = rvec(1)
****----------------------------------------------
**   Old simple search method
****----------------------------------------------
**      DO j=1,jmax
**         jstop=j
**         IF(zint(j) .GT. rnumb) GOTO 216
**      ENDDO
** 216  CONTINUE
*====================================================
**  Equivalent faster Weierstrass-type search method
*====================================================
      klower   = 0
      krange   = jmax
 330  CONTINUE
      krange   = krange/2
      kurrent  = klower +krange
      IF(kurrent.EQ.0) GOTO 350
      IF(zint(kurrent) .LE. rnumb) THEN
         klower = kurrent
      ENDIF
      IF(krange.EQ.0) GOTO 350
      GOTO 330
 350  CONTINUE
      jstop = kurrent+1
*====================================================
      IF(jstop .EQ. 1) THEN
         d=rnumb/zint(1)
      ELSE
         d =(rnumb-zint(jstop-1))/(zint(jstop)-zint(jstop-1))
      ENDIF
      x=xx(jstop)*(1.d0 -d )+xx(jstop+1)*d
      fn=funsko(x)
      IF(fn .LT. 0.d0) GOTO 999
      yymax=max(yy(jstop+1),yy(jstop))
      wt=fn/yymax
      nevs=nevs+1
      swt=swt+wt
      sswt=sswt+wt*wt
*
      RETURN
 901  WRITE(*,'(a)') ' **** STOP in vesko1, lack of initialisation'
      STOP
 999  WRITE(*,'(a)') ' **** STOP in vesk01, negative value of funsko '
      STOP
      END       ! Vesk1_Make



      SUBROUTINE Vesk1_Finalize(cinteg,errint,zcrude)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Vesk1.h'
*
      DOUBLE PRECISION     cinteg,errint,zcrude
*-----------------------------------------------------------------------------
      cinteg =0d0
      errint =0d0
      zcrude =0d0
      IF(nevs .GT. 0) cinteg=zsum*swt/float(nevs)
      IF(nevs .GT. 0) errint=sqrt(sswt/swt**2-1.d0/float(nevs))
      IF(nevs .GT. 0) zcrude=  zsum
      END       ! Vesk1_Finalize
*
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                      End of CLASS  Vesk1                                 //
*//////////////////////////////////////////////////////////////////////////////
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//          Pseudoclass BStra                                                       //
*//                                                                                  //
*//                                                                                  //
*//   Foam is now the basic MC sampler for beamstrahlung and ISR.                    //
*//   Notes: BStra has internal rejection procedure,                                 //
*//   consequently, normalization is determined by average weight, which is monitored//
*//   by MBrB                                                                        //
*//////////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE BStra_Initialize(KeyGrid,Xcrude)
*//////////////////////////////////////////////////////////////////////////////////////
*//   Initialization phase                                                           //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BStra.h'
      INCLUDE 'BXformat.h'
      INTEGER            KeyGrid
      DOUBLE PRECISION   XCrude
      INTEGER  k,j
      DOUBLE PRECISION   XsCru(10), WMList(10)
      DOUBLE PRECISION   XsectA, XsectB,  XsectC
      DOUBLE PRECISION   ErrelA,  ErrelB,  ErrelC
      DOUBLE PRECISION   BornV_RhoFoamA, BornV_RhoFoamB, BornV_RhoFoamC
      EXTERNAL           BornV_RhoFoamA, BornV_RhoFoamB, BornV_RhoFoamC
      DOUBLE PRECISION   XXXene, vvmax
      INTEGER            nCallsA,  nCallsB,  nCallsC
      INTEGER            IterMaxA, IterMaxB, IterMaxC,  Idyfs,  IdBra, Nbin
*-------------------------------
      m_out     = 16
      m_Nevgen  =  0
*//////////////////////////////////////////////////////////////////////////////////////
*// Mode=-1 creation+dump, =+1 reading, =0 creation without dump (default)           //
*//////////////////////////////////////////////////////////////////////////////////////
      m_ModeA  =   KeyGrid
      m_ModeB  =   KeyGrid
      m_ModeC  =   KeyGrid
*
      CALL KarLud_GetXXXene(XXXene)
*
      IF(m_ModeA.EQ.2) WMList(1) = 1.00d0
      IF(m_ModeB.EQ.2) WMList(2) = 1.00d0
      IF(m_ModeC.EQ.2) WMList(3) = 1.00d0
*
      CALL KarLud_Getvvmax(vvmax)
*//////////////////////////////////////////////////////////////////////////////////////
      XsectA   = 1d-100
      XsectB   = 1d-100
      XsectC   = 1d-100
      ErrelA   = 1d0
      ErrelB   = 1d0
      ErrelC   = 1d0
*//////////////////////////////////////////////////////////////////////////////////////
*//   1-dimensional case                                                             //
*//////////////////////////////////////////////////////////////////////////////////////
      WRITE(*,*) '*****************************************************************'
      WRITE(*,*) '****** BE PATIENT FoamA CREATING GRID FOR BEAMSTRAHLUNG *********'
      WRITE(*,*) '*****************************************************************'
      CALL FoamA_SetKdim(       1) ! No of dimensions<5
      CALL FoamA_SetnBuf(    1000) ! Length of buffer<5000,  =Maximum No. of cells
      CALL FoamA_SetnSampl(  1000) ! No. of MC sampling inside single cell, default=100
      CALL FoamA_SetnBin(       4) ! No of bins for edge explorations
      CALL FoamA_SetEvPerBin(  25) ! No. of equiv. MC events per bin
      CALL FoamA_SetOptEdge(    0) ! OptEdge excludes vertices
      CALL FoamA_SetOptRanIni(  0) ! No internal initialization of rand.num.gen
      CALL FoamA_SetOptRanLux( -1) ! Ranmar choosen
      CALL FoamA_SetChat(       1) ! printout level =0,1,2
      CALL FoamA_Initialize(BornV_RhoFoamA)
*//////////////////////////////////////////////////////////////////////////////////////
*//   2-dimensional case                                                             //
*//////////////////////////////////////////////////////////////////////////////////////
      WRITE(*,*) '*****************************************************************'
      WRITE(*,*) '****** BE PATIENT FoamB CREATING GRID FOR BEAMSTRAHLUNG *********'
      WRITE(*,*) '*****************************************************************'
      CALL FoamB_SetKdim(       2) ! No of dimensions<5
      CALL FoamB_SetnBuf(    2000) ! Length of buffer<5000,  =Maximum No. of cells
      CALL FoamB_SetnSampl(  2000) ! No. of MC sampling inside single cell, default=100
      CALL FoamB_SetnBin(       4) ! No of bins for edge explorations
      CALL FoamB_SetEvPerBin(  25) ! No. of equiv. MC events per bin
      CALL FoamB_SetOptEdge(    0) ! OptEdge excludes vertices
      CALL FoamB_SetOptRanIni(  0) ! No internal initialization of rand.num.gen
      CALL FoamB_SetOptRanLux( -1) ! Ranmar choosen
      CALL FoamB_SetChat(       1) ! printout level =0,1,2
      CALL FoamB_Initialize(BornV_RhoFoamB)
*     Debug plotting; nbuf=250 is maximum for ploting cell boundries
      OPEN(23,FILE='./map.tex')
      CALL FoamB_PltBegin(23)
      CALL FoamB_PltVert
      CALL FoamB_PltCell        ! nbuf<250
      CALL FoamB_PltEnd
*//////////////////////////////////////////////////////////////////////////////////////
*//   3-dimensional case                                                             //
*//////////////////////////////////////////////////////////////////////////////////////
      WRITE(*,*) '*****************************************************************'
      WRITE(*,*) '****** BE PATIENT FoamC CREATING GRID FOR BEAMSTRAHLUNG *********'
      WRITE(*,*) '*****************************************************************'
      CALL FoamC_SetKdim(       3) ! No. of dimensions<5
      CALL FoamC_SetnBuf(    5000) ! Length of buffer<5000,  =Maximum No. of cells
      CALL FoamC_SetnSampl(  5000) ! No. of MC sampling inside single cell, default=100
      CALL FoamC_SetnBin(       4) ! No of bins for edge explorations
      CALL FoamC_SetEvPerBin(  25) ! No. of equiv. MC events per bin
      CALL FoamC_SetOptEdge(    0) ! OptEdge excludes vertices
      CALL FoamC_SetOptRanIni(  0) ! No internal initialization of rand.num.gen
      CALL FoamC_SetOptRanLux( -1) ! Ranmar choosen
      CALL FoamC_SetChat(       1) ! printout level =0,1,2
      CALL FoamC_Initialize(BornV_RhoFoamC)
*//////////////////////////////////////////////////////////////////////////////////////
*//   The best Vegas Integral estimators from initialization (grid building) phase   //
*//////////////////////////////////////////////////////////////////////////////////////
      CALL FoamA_GetTotPrim( XsectA) ! Integral estimate from Initialization
      ErrelA  = ErrelA/XsectA
      CALL FoamB_GetTotPrim( XsectB) ! Integral estimate from Initialization
      ErrelB  = ErrelB/XsectB
      CALL FoamC_GetTotPrim( XsectC) ! Integral estimate from Initialization
      ErrelC  = ErrelC/XsectC

      m_XGridB =  XsectA+XsectB+XsectC ! is it really used ?????
      m_EGridB =  DSQRT( (ErrelA*XsectA)**2 +(ErrelB*XsectB)**2 +(ErrelC*XsectC)**2)
      m_EGridB =  m_EGridB/m_XGridB
*
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '  BStra  Initializator                '
      WRITE(m_out,bxtxt) '  Grid initialization finished        '
      WRITE(m_out,bxl1g) XsectA ,    'XsectA  1-dimen.  ','XsectA','**'
      WRITE(m_out,bxl1g) XsectB ,    'XsectB  2-dimen.  ','XsectB','**'
      WRITE(m_out,bxl1g) XsectC ,    'XsectC  3-dimen.  ','XsectC','**'
      WRITE(m_out,bxl1f) ErrelA  ,   'ErrelA  1-dimen.  ','ErrelA','**'
      WRITE(m_out,bxl1f) ErrelB  ,   'ErrelB  2-dimen.  ','ErrelB','**'
      WRITE(m_out,bxl1f) ErrelC  ,   'ErrelC  3-dimen.  ','ErrelC','**'
      WRITE(m_out,bxl1g) m_XGridB,   'XGridB  total.    ','XGridB','**'
      WRITE(m_out,bxl1f) m_EGridB,   'EGridB, rel. total','EGridB','**'
      WRITE(m_out,bxclo)
*//////////////////////////////////////////////////////////////////////////////////////
*//     Calculate Crude Xcru(i), i=1,2,3,  branch per branch                         //
*//////////////////////////////////////////////////////////////////////////////////////
      CALL FoamA_GetTotPrim(XsCru(1)) ! get crude normalization for MC
      CALL FoamB_GetTotPrim(XsCru(2)) ! get crude normalization for MC
      CALL FoamC_GetTotPrim(XsCru(3)) ! get crude normalization for MC
*//////////////////////////////////////////////////////////////////////////////////////
*//   Initialization of MBrB, the own copy of a brancher                             //
*//////////////////////////////////////////////////////////////////////////////////////
      CALL KK2f_GetIdyfs(Idyfs)
      IdBra = Idyfs+200
      CALL MBrB_Initialize(m_out,IdBra,50, 1d0, 'MBrB: Bstra main weight$')
      Nbin  = 500
      CALL MBrB_AddBranch(1, Nbin, WMList(1), 'MBrB: next branch A  !!! $')
      CALL MBrB_AddBranch(2, Nbin, WMList(2), 'MBrB: next branch B  !!! $')
      CALL MBrB_AddBranch(3, Nbin, WMList(3), 'MBrB: next branch C  !!! $')
      CALL MBrB_SetXSList(XsCru)
      CALL MBrB_GetXCrude(m_XCrude)
*//////////////////////////////////////////////////////////////////////////////////////
*// Because in Bstra we have internal rejection loop we send to Karlud and KK2f      //
*// the best estimator of integral we have at this moment                            // 
*// It will be used for histogram normalization (sometimes)                          // 
*// Note that xsection from KK2f_finalize uses m_XCrude*<wt)> or m_XGridB            //
*//////////////////////////////////////////////////////////////////////////////////////
      XCrude   = m_XGridB
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '  BStra  Initializator, PreGeneration '
      WRITE(m_out,bxl1g) XsCru(1) ,   'XsCru(1)  1-dimen.  ','XsCru(1) ','**'
      WRITE(m_out,bxl1g) XsCru(2) ,   'XsCru(2)  2-dimen.  ','XsCru(2) ','**'
      WRITE(m_out,bxl1g) XsCru(3) ,   'XsCru(3)  3-dimen.  ','XsCru(3) ','**'
      WRITE(m_out,bxl1f) WMlist(1) ,  'WMlist(1) 1-dimen.  ','WMlist(1)','**'
      WRITE(m_out,bxl1f) WMlist(2) ,  'WMlist(2) 2-dimen.  ','WMlist(2)','**'
      WRITE(m_out,bxl1f) WMlist(3) ,  'WMlist(3) 3-dimen.  ','WMlist(3)','**'
      WRITE(m_out,bxl1g) m_XCrude ,   'XCrude   total      ','XCrude   ','**'
      WRITE(m_out,bxclo)
      END

      SUBROUTINE BStra_Make(vv, x1, x2, MCwt)
*//////////////////////////////////////////////////////////////////////////////////////
*//   Genearete set of 3 ISR variables for beamsstrahlung ISR                        //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BStra.h'
      DOUBLE PRECISION  vv, x1, x2, MCwt, x, Wt_KF
      DOUBLE PRECISION  BornV_RhoFoamA, BornV_RhoFoamB, BornV_RhoFoamC
      EXTERNAL          BornV_RhoFoamA, BornV_RhoFoamB, BornV_RhoFoamC
      REAL              Qrand(10)        ! for PseuMar
      INTEGER           Itype
      DOUBLE PRECISION  rand
*-------------------------------------------------------------------------------
 100  CONTINUE
      m_Nevgen  =  m_Nevgen +1
      CALL MBrB_GenKF(Itype, Wt_KF)
      IF(     Itype .EQ. 1 ) THEN
         CALL FoamA_MakeEvent(BornV_RhoFoamA) ! generate MC event
         CALL FoamA_GetMCwt(  MCwt) ! get MC weight
      ELSEIF( Itype .EQ. 2 ) THEN
         CALL FoamB_MakeEvent(BornV_RhoFoamB) ! generate MC event
         CALL FoamB_GetMCwt(  MCwt) ! get MC weight
      ELSEIF( Itype .EQ. 3 ) THEN
         CALL FoamC_MakeEvent(BornV_RhoFoamC) ! generate MC event
         CALL FoamC_GetMCwt(  MCwt) ! get MC weight
      ELSE
         WRITE(m_out,*) '+++++ STOP in BStra_Make '
         WRITE(    *,*) '+++++ STOP in BStra_Make '
         STOP 
      ENDIF
      CALL BornV_GetVXX(vv,x1,x2)
* random swap, necessary because FoamB integrand is asymmetric
      CALL PseuMar_MakeVec(Qrand,2)
      IF( Qrand(1) .LT. 0.5d0 ) THEN
         x  = x1
         x1 = x2
         x2 = x
      ENDIF
      MCwt = MCwt *Wt_KF
* Rejection
      rand = Qrand(2)
      CALL MBrB_Fill(MCwt   ,rand)
      IF(rand .GT. MCwt) GOTO 100
      MCwt = 1d0
      END                       ! BStra_Make

      SUBROUTINE BStra_GetXCrude(XCrude)
*//////////////////////////////////////////////////////////////////////////////////////
*//   Get TRUE crude integraml                                                       //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BStra.h'
      DOUBLE PRECISION   XCrude
      XCrude   = m_XCrude
      END                       ! BStra_GetXCrude

      SUBROUTINE BStra_Finalize(Integ,Errel)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Provides Crude integral at the end of MC generation based on <wt>      //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BStra.h'
      INCLUDE 'BXformat.h'
      DOUBLE PRECISION     Integ,Errel
      DOUBLE PRECISION     IntegMC,ErrelMC
      DOUBLE PRECISION     AverWt, WtSup
*-----------------------------------------------------------------------------
      CALL MBrB_MgetAve(AverWt, ErRelMC, WtSup)
      IntegMC= m_XCrude*AverWt
      Integ  = IntegMC
      ErRel  = ErRelMC
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '  BStra  Finalize MC results     '
      WRITE(m_out,bxl1g) IntegMC,   'MC integral   ','IntegMC','**'
      WRITE(m_out,bxl1f) ErRelMC,   'relat. error  ','ErRelMC','**'
      WRITE(m_out,bxl1f) WtSup,     'maximum wt    ','WtSup  ','**'
      WRITE(m_out,bxtxt) '  From grid building (initializ.)'  
      WRITE(m_out,bxl1g) m_XGridB,   'XGridB  total.    ','XGridB','**'
      WRITE(m_out,bxl1f) m_EGridB,   'EGridB, rel. total','EGridB','**'
      WRITE(m_out,bxclo)
* Print more on the main weight
      CALL MBrB_Print0
* Print even more on the weight in each branch!
      CALL MBrB_Print1
      END       ! BStra_Finalize

      SUBROUTINE BStra_GetXGridB(XGridB,EGridB)
*//////////////////////////////////////////////////////////////////////////////////////
*//   Get TRUE crude integram                                                        //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BStra.h'
      DOUBLE PRECISION     XGridB,EGridB
      XGridB   = m_XGridB
      EGridB   = m_EGridB
      END                       ! BStra_GetXGridB

      SUBROUTINE BStra_GetIntegMC(IntegMC,ErRelMC)
*//////////////////////////////////////////////////////////////////////////////////////
*//   Get TRUE Monte Carlo run Integral and errors                                   //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BStra.h'
      DOUBLE PRECISION     IntegMC,ErRelMC
      DOUBLE PRECISION     AverWt,WtSup
      CALL MBrB_MgetAve(AverWt, ErRelMC, WtSup)
      IntegMC= m_XCrude*AverWt
      END                       ! BStra_GetIntegMC

      SUBROUTINE BStra_GetAveWt(AveWt,RatWt)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BStra.h'
      DOUBLE PRECISION     AveWt,RatWt
      DOUBLE PRECISION     AverWt, ErRela, WtSup
*-----------------------------------------------------------------------------
      CALL MBrB_MgetAve(AverWt, ErRela, WtSup)
      AveWt = AverWt
      RatWt = AverWt/WtSup
      END
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//          END of Pseudoclass BStra                                                //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
*////////////////////////////////////////////////////////////////////////////////////
*//                                                                                //
*//            Customization by S. Jadach, April 1999                              //
*//                                                                                //
*//     Prefix IRC_ added to all subrogram names                                   //
*//     Name /circom/ replaced with /c_IRC/                                        //
*//                                                                                //
*//                                                                                //
*//                                                                                //
*////////////////////////////////////////////////////////////////////////////////////

c circe.f -- canonical beam spectra for linear collider physics
c   Copyright (C) 1996,1997 by Thorsten.Ohl@Physik.TH-Darmstadt.de
c
c   Circe is free software; you can redistribute it and/or modify it
c   under the terms of the GNU General Public License as published by
c   the Free Software Foundation; either version 2, or (at your option)
c   any later version.
c
c   Circe is distributed in the hope that it will be useful, but
c   WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with this program; if not, write to the Free Software
c   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

      DOUBLE PRECISION FUNCTION IRC_circe (x1, x2, p1, p2)
      IMPLICIT NONE
      DOUBLE PRECISION x1, x2
      INTEGER p1, p2
      DOUBLE PRECISION IRC_circee, IRC_circeg, IRC_circgg
      INTEGER electr, positr, photon
      PARAMETER (electr =  11)
      PARAMETER (positr = -11)
      PARAMETER (photon =  22)
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      IF (magic  .NE.  magic0) THEN
         CALL IRC_circes (-1d0, -1d0, -1d0, -1, -1, -1, -1)
      ENDIF
      IRC_circe = -1.0
      IF (ABS(p1)  .EQ.  electr) THEN
         IF (ABS(p2)  .EQ.  electr) THEN
            IRC_circe = IRC_circee (x1, x2)
         ELSEIF (p2  .EQ.  photon) THEN
            IRC_circe = IRC_circeg (x1, x2)
         ENDIF
      ELSEIF (p1  .EQ.  photon) THEN
         IF (ABS(p2)  .EQ.  electr) THEN
            IRC_circe = IRC_circeg (x2, x1)
         ELSEIF (p2  .EQ.  photon) THEN
            IRC_circe = IRC_circgg (x1, x2)
         ENDIF
      ENDIF
      END
      SUBROUTINE IRC_circes (xx1m, xx2m, xroots, xacc, xver, xrev, xchat)
      IMPLICIT NONE
      DOUBLE PRECISION xx1m, xx2m, xroots
      INTEGER xacc, xver, xrev, xchat
      INTEGER sband, tesla, xband
      PARAMETER (sband  =  1, tesla  =  2, xband  =  3)
      INTEGER sbndee, teslee, xbndee
      PARAMETER (sbndee =  4, teslee =  5, xbndee =  6)
      INTEGER nacc
      PARAMETER (nacc = 6)
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      CHARACTER*60 msgbuf
      CHARACTER*6 accnam(nacc)
      INTEGER zver
      SAVE zver
      INTEGER ver34
      INTEGER gev350, gev500, gev800, tev1, tev16
      PARAMETER (gev350 = 1, gev500 = 2, gev800 = 3,
     $           tev1 =   4, tev16  = 5)
      INTEGER a1negy, a1nrev
      PARAMETER (a1negy = 5, a1nrev = 5)
      INTEGER i
      REAL xa1lum(a1negy,nacc,0:a1nrev)
      REAL xa1(0:7,a1negy,nacc,0:a1nrev)
      INTEGER a3negy, a3nrev
      PARAMETER (a3negy = 5, a3nrev = 5)
      REAL xa3lum(a3negy,nacc,0:a3nrev)
      REAL xa3(0:7,a3negy,nacc,0:a3nrev)
      INTEGER a5negy, a5nrev
      PARAMETER (a5negy = 5, a5nrev = 1)
      REAL xa5lum(a5negy,nacc,0:a5nrev)
      REAL xa5(0:7,a5negy,nacc,0:a5nrev)
      DATA accnam(sband)  /'sband'/
      DATA accnam(tesla)  /'tesla'/
      DATA accnam(xband)  /'xband'/
      DATA accnam(sbndee) /'sbndee'/
      DATA accnam(teslee) /'teslee'/
      DATA accnam(xbndee) /'xbndee'/
      DATA zver / -1 /
      DATA xa1lum(gev500,sband,1) /  5.212299e+01 /
      DATA (xa1(i,gev500,sband,1),i=0,7) /
     $    .39192e+00,   .66026e+00,   .11828e+02,  -.62543e+00, 
     $    .52292e+00,  -.69245e+00,   .14983e+02,   .65421e+00 /
      DATA xa1lum(gev500,tesla,1) /  6.066178e+01 /
      DATA (xa1(i,gev500,tesla,1),i=0,7) /
     $    .30196e+00,   .12249e+01,   .21423e+02,  -.57848e+00, 
     $    .68766e+00,  -.69788e+00,   .23121e+02,   .78399e+00 /
      DATA xa1lum(gev500,xband,1) /  5.884699e+01 /
      DATA (xa1(i,gev500,xband,1),i=0,7) /
     $    .48594e+00,   .52435e+00,   .83585e+01,  -.61347e+00, 
     $    .30703e+00,  -.68804e+00,   .84109e+01,   .44312e+00 /
      DATA xa1lum(tev1,sband,1)   /  1.534650e+02 /
      DATA (xa1(i,tev1,sband,1),i=0,7) /
     $    .24399e+00,   .87464e+00,   .66751e+01,  -.56808e+00, 
     $    .59295e+00,  -.68921e+00,   .94232e+01,   .83351e+00 /
      DATA xa1lum(tev1,tesla,1)   /  1.253381e+03 /
      DATA (xa1(i,tev1,tesla,1),i=0,7) /
     $    .39843e+00,   .70097e+00,   .11602e+02,  -.61061e+00, 
     $    .40737e+00,  -.69319e+00,   .14800e+02,   .51382e+00 /
      DATA xa1lum(tev1,xband,1)   /  1.901783e+02 /
      DATA (xa1(i,tev1,xband,1),i=0,7) /
     $    .32211e+00,   .61798e+00,   .28298e+01,  -.54644e+00, 
     $    .45674e+00,  -.67301e+00,   .41703e+01,   .74536e+00 /
      DATA (xa1lum(gev350,i,1),i=1,nacc) / nacc*-1d0 /
      DATA (xa1lum(gev800,i,1),i=1,nacc) / nacc*-1d0 /
      DATA (xa1lum(gev500,i,1),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(tev1,i,1),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(tev16,i,1),i=1,nacc) / 6*-1d0 /
      DATA xa1lum(gev500,sband,2) /   .31057e+02 /
      DATA (xa1(i,gev500,sband,2),i=0,7) /
     $    .38504e+00,   .79723e+00,   .14191e+02,  -.60456e+00, 
     $    .53411e+00,  -.68873e+00,   .15105e+02,   .65151e+00 /
      DATA xa1lum(tev1,sband,2) /   .24297e+03 /
      DATA (xa1(i,tev1,sband,2),i=0,7) /
     $    .24374e+00,   .89466e+00,   .70242e+01,  -.56754e+00, 
     $    .60910e+00,  -.68682e+00,   .96083e+01,   .83985e+00 /
      DATA xa1lum(gev350,tesla,2) /   .73369e+02 /
      DATA (xa1(i,gev350,tesla,2),i=0,7) /
     $    .36083e+00,   .12819e+01,   .37880e+02,  -.59492e+00, 
     $    .69109e+00,  -.69379e+00,   .40061e+02,   .65036e+00 /
      DATA xa1lum(gev500,tesla,2) /   .10493e+03 /
      DATA (xa1(i,gev500,tesla,2),i=0,7) /
     $    .29569e+00,   .11854e+01,   .21282e+02,  -.58553e+00, 
     $    .71341e+00,  -.69279e+00,   .24061e+02,   .77709e+00 /
      DATA xa1lum(gev800,tesla,2) /   .28010e+03 /
      DATA (xa1(i,gev800,tesla,2),i=0,7) /
     $    .22745e+00,   .11265e+01,   .10483e+02,  -.55711e+00, 
     $    .69579e+00,  -.69068e+00,   .13093e+02,   .89605e+00 /
      DATA xa1lum(tev1,tesla,2) /   .10992e+03 /
      DATA (xa1(i,tev1,tesla,2),i=0,7) /
     $    .40969e+00,   .66105e+00,   .11972e+02,  -.62041e+00, 
     $    .40463e+00,  -.69354e+00,   .14669e+02,   .51281e+00 /
      DATA xa1lum(gev500,xband,2) /   .35689e+02 /
      DATA (xa1(i,gev500,xband,2),i=0,7) /
     $    .48960e+00,   .46815e+00,   .75249e+01,  -.62769e+00, 
     $    .30341e+00,  -.68754e+00,   .85545e+01,   .43453e+00 /
      DATA xa1lum(tev1,xband,2) /   .11724e+03 /
      DATA (xa1(i,tev1,xband,2),i=0,7) /
     $    .31939e+00,   .62415e+00,   .30763e+01,  -.55314e+00, 
     $    .45634e+00,  -.67089e+00,   .41529e+01,   .73807e+00 /
      DATA xa1lum(gev350,sband,2) / -1d0 /
      DATA xa1lum(gev350,xband,2) / -1d0 /
      DATA xa1lum(gev800,sband,2) / -1d0 /
      DATA xa1lum(gev800,xband,2) / -1d0 /
      DATA (xa1lum(gev350,i,2),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(gev500,i,2),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(gev800,i,2),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(tev1,i,2),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(tev16,i,2),i=1,nacc) / 6*-1d0 /
      DATA xa1lum(gev500,sband, 3) /   .31469e+02 /
      DATA (xa1(i,gev500,sband, 3),i=0,7) /
     $    .38299e+00,   .72035e+00,   .12618e+02,  -.61611e+00, 
     $    .51971e+00,  -.68960e+00,   .15066e+02,   .63784e+00 /
      DATA xa1lum(tev1,  sband, 3) /   .24566e+03 /
      DATA (xa1(i,tev1,  sband, 3),i=0,7) /
     $    .24013e+00,   .95763e+00,   .69085e+01,  -.55151e+00, 
     $    .59497e+00,  -.68622e+00,   .94494e+01,   .82158e+00 /
      DATA xa1lum(gev350,tesla, 3) /   .74700e+02 /
      DATA (xa1(i,gev350,tesla, 3),i=0,7) /
     $    .34689e+00,   .12484e+01,   .33720e+02,  -.59523e+00, 
     $    .66266e+00,  -.69524e+00,   .38488e+02,   .63775e+00 /
      DATA xa1lum(gev500,tesla, 3) /   .10608e+03 /
      DATA (xa1(i,gev500,tesla, 3),i=0,7) /
     $    .28282e+00,   .11700e+01,   .19258e+02,  -.58390e+00, 
     $    .68777e+00,  -.69402e+00,   .23638e+02,   .75929e+00 /
      DATA xa1lum(gev800,tesla, 3) /   .28911e+03 /
      DATA (xa1(i,gev800,tesla, 3),i=0,7) /
     $    .21018e+00,   .12039e+01,   .96763e+01,  -.54024e+00, 
     $    .67220e+00,  -.69083e+00,   .12733e+02,   .87355e+00 /
      DATA xa1lum(tev1,  tesla, 3) /   .10936e+03 /
      DATA (xa1(i,tev1,  tesla, 3),i=0,7) /
     $    .41040e+00,   .68099e+00,   .11610e+02,  -.61237e+00, 
     $    .40155e+00,  -.69073e+00,   .14698e+02,   .49989e+00 /
      DATA xa1lum(gev500,xband, 3) /   .36145e+02 /
      DATA (xa1(i,gev500,xband, 3),i=0,7) /
     $    .51285e+00,   .45812e+00,   .75135e+01,  -.62247e+00, 
     $    .30444e+00,  -.68530e+00,   .85519e+01,   .43062e+00 /
      DATA xa1lum(tev1,  xband, 3) /   .11799e+03 /
      DATA (xa1(i,tev1,  xband, 3),i=0,7) /
     $    .31241e+00,   .61241e+00,   .29938e+01,  -.55848e+00, 
     $    .44801e+00,  -.67116e+00,   .41119e+01,   .72753e+00 /
      DATA xa1lum(gev350,sband,3) / -1d0 /
      DATA xa1lum(gev350,xband,3) / -1d0 /
      DATA xa1lum(gev800,sband,3) / -1d0 /
      DATA xa1lum(gev800,xband,3) / -1d0 /
      DATA (xa1lum(gev350,i,3),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(gev500,i,3),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(gev800,i,3),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(tev1,i,3),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(tev16,i,3),i=1,nacc) / 6*-1d0 /
      DATA xa1lum(gev500,sband, 4) /   .31528e+02 /
      DATA (xa1(i,gev500,sband, 4),i=0,7) /
     $    .38169e+00,   .73949e+00,   .12543e+02,  -.61112e+00, 
     $    .51256e+00,  -.69009e+00,   .14892e+02,   .63314e+00 /
      DATA xa1lum(tev1,  sband, 4) /   .24613e+03 /
      DATA (xa1(i,tev1,  sband, 4),i=0,7) /
     $    .24256e+00,   .94117e+00,   .66775e+01,  -.55160e+00, 
     $    .57484e+00,  -.68891e+00,   .92271e+01,   .81162e+00 /
      DATA xa1lum(gev350,tesla, 4) /   .74549e+02 /
      DATA (xa1(i,gev350,tesla, 4),i=0,7) /
     $    .34120e+00,   .12230e+01,   .32932e+02,  -.59850e+00, 
     $    .65947e+00,  -.69574e+00,   .38116e+02,   .63879e+00 /
      DATA xa1lum(gev500,tesla, 4) /   .10668e+03 /
      DATA (xa1(i,gev500,tesla, 4),i=0,7) /
     $    .28082e+00,   .11074e+01,   .18399e+02,  -.59118e+00, 
     $    .68880e+00,  -.69375e+00,   .23463e+02,   .76073e+00 /
      DATA xa1lum(gev800,tesla, 4) /   .29006e+03 /
      DATA (xa1(i,gev800,tesla, 4),i=0,7) /
     $    .21272e+00,   .11443e+01,   .92564e+01,  -.54657e+00, 
     $    .66799e+00,  -.69137e+00,   .12498e+02,   .87571e+00 /
      DATA xa1lum(tev1,  tesla, 4) /   .11009e+03 /
      DATA (xa1(i,tev1,  tesla, 4),i=0,7) /
     $    .41058e+00,   .64745e+00,   .11271e+02,  -.61996e+00, 
     $    .39801e+00,  -.69150e+00,   .14560e+02,   .49924e+00 /
      DATA xa1lum(gev500,xband, 4) /   .36179e+02 /
      DATA (xa1(i,gev500,xband, 4),i=0,7) /
     $    .51155e+00,   .43313e+00,   .70446e+01,  -.63003e+00, 
     $    .29449e+00,  -.68747e+00,   .83489e+01,   .42458e+00 /
      DATA xa1lum(tev1,  xband, 4) /   .11748e+03 /
      DATA (xa1(i,tev1,  xband, 4),i=0,7) /
     $    .32917e+00,   .54322e+00,   .28493e+01,  -.57959e+00, 
     $    .39266e+00,  -.68217e+00,   .38475e+01,   .68478e+00 /
      DATA xa1lum(gev350,sband,4) / -1d0 /
      DATA xa1lum(gev350,xband,4) / -1d0 /
      DATA xa1lum(gev800,sband,4) / -1d0 /
      DATA xa1lum(gev800,xband,4) / -1d0 /
      DATA (xa1lum(gev350,i,4),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(gev500,i,4),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(gev800,i,4),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(tev1,i,4),i=sbndee,nacc) / 3*-1d0 /
      DATA (xa1lum(tev16,i,4),i=1,nacc) / 6*-1d0 /
      DATA xa1lum(gev350,sband, 5) /  0.21897e+02 /
      DATA (xa1(i,gev350,sband, 5),i=0,7) /
     $   0.57183e+00,  0.53877e+00,  0.19422e+02, -0.63064e+00, 
     $   0.49112e+00, -0.69109e+00,  0.24331e+02,  0.52718e+00 /
      DATA xa1lum(gev500,sband, 5) /  0.31383e+02 /
      DATA (xa1(i,gev500,sband, 5),i=0,7) /
     $   0.51882e+00,  0.49915e+00,  0.11153e+02, -0.63017e+00, 
     $   0.50217e+00, -0.69113e+00,  0.14935e+02,  0.62373e+00 /
      DATA xa1lum(gev800,sband, 5) /  0.95091e+02 /
      DATA (xa1(i,gev800,sband, 5),i=0,7) /
     $   0.47137e+00,  0.46150e+00,  0.56562e+01, -0.61758e+00, 
     $   0.46863e+00, -0.68897e+00,  0.85876e+01,  0.67577e+00 /
      DATA xa1lum(tev1,sband, 5) /  0.11900e+03 /
      DATA (xa1(i,tev1,sband, 5),i=0,7) /
     $   0.43956e+00,  0.45471e+00,  0.42170e+01, -0.61180e+00, 
     $   0.48711e+00, -0.68696e+00,  0.67145e+01,  0.74551e+00 /
      DATA xa1lum(tev16,sband, 5) /  0.11900e+03 /
      DATA (xa1(i,tev16,sband, 5),i=0,7) /
     $   0.43956e+00,  0.45471e+00,  0.42170e+01, -0.61180e+00, 
     $   0.48711e+00, -0.68696e+00,  0.67145e+01,  0.74551e+00 /
      DATA xa1lum(gev350,tesla, 5) /  0.97452e+02 /
      DATA (xa1(i,gev350,tesla, 5),i=0,7) /
     $   0.39071e+00,  0.84996e+00,  0.17614e+02, -0.60609e+00, 
     $   0.73920e+00, -0.69490e+00,  0.28940e+02,  0.77286e+00 /
      DATA xa1lum(gev500,tesla, 5) /  0.10625e+03 /
      DATA (xa1(i,gev500,tesla, 5),i=0,7) /
     $   0.42770e+00,  0.71457e+00,  0.15284e+02, -0.61664e+00, 
     $   0.68166e+00, -0.69208e+00,  0.24165e+02,  0.73806e+00 /
      DATA xa1lum(gev800,tesla, 5) /  0.17086e+03 /
      DATA (xa1(i,gev800,tesla, 5),i=0,7) /
     $   0.36025e+00,  0.69118e+00,  0.76221e+01, -0.59440e+00, 
     $   0.71269e+00, -0.69077e+00,  0.13117e+02,  0.91780e+00 /
      DATA xa1lum(tev1,tesla, 5) /  0.21433e+03 /
      DATA (xa1(i,tev1,tesla, 5),i=0,7) /
     $   0.33145e+00,  0.67075e+00,  0.55438e+01, -0.58468e+00, 
     $   0.72503e+00, -0.69084e+00,  0.99992e+01,  0.10112e+01 /
      DATA xa1lum(tev16,tesla, 5) /  0.34086e+03 /
      DATA (xa1(i,tev16,tesla, 5),i=0,7) /
     $   0.49058e+00,  0.42609e+00,  0.50550e+01, -0.61867e+00, 
     $   0.39225e+00, -0.68916e+00,  0.75514e+01,  0.58754e+00 /
      DATA xa1lum(gev350,xband, 5) /  0.31901e+02 /
      DATA (xa1(i,gev350,xband, 5),i=0,7) /
     $   0.65349e+00,  0.31752e+00,  0.94342e+01, -0.64291e+00, 
     $   0.30364e+00, -0.68989e+00,  0.11446e+02,  0.40486e+00 /
      DATA xa1lum(gev500,xband, 5) /  0.36386e+02 /
      DATA (xa1(i,gev500,xband, 5),i=0,7) /
     $   0.65132e+00,  0.28728e+00,  0.69853e+01, -0.64440e+00, 
     $   0.28736e+00, -0.68758e+00,  0.83227e+01,  0.41492e+00 /
      DATA xa1lum(gev800,xband, 5) /  0.10854e+03 /
      DATA (xa1(i,gev800,xband, 5),i=0,7) /
     $   0.49478e+00,  0.36221e+00,  0.30116e+01, -0.61548e+00, 
     $   0.39890e+00, -0.68418e+00,  0.45183e+01,  0.67243e+00 /
      DATA xa1lum(tev1,xband, 5) /  0.11899e+03 /
      DATA (xa1(i,tev1,xband, 5),i=0,7) /
     $   0.49992e+00,  0.34299e+00,  0.26184e+01, -0.61584e+00, 
     $   0.38450e+00, -0.68342e+00,  0.38589e+01,  0.67408e+00 /
      DATA xa1lum(tev16,xband, 5) /  0.13675e+03 /
      DATA (xa1(i,tev16,xband, 5),i=0,7) /
     $   0.50580e+00,  0.30760e+00,  0.18339e+01, -0.61421e+00, 
     $   0.35233e+00, -0.68315e+00,  0.26708e+01,  0.67918e+00 /
      DATA xa1lum(gev500,sbndee, 0) /   .92914e+01 /
      DATA (xa1(i,gev500,sbndee, 0),i=0,7) /
     $    .34866e+00,   .78710e+00,   .10304e+02,  -.59464e+00, 
     $    .40234e+00,  -.69741e+00,   .20645e+02,   .47274e+00 /
      DATA xa1lum(tev1,  sbndee, 0) /   .45586e+02 /
      DATA (xa1(i,tev1,  sbndee, 0),i=0,7) /
     $    .21084e+00,   .99168e+00,   .54407e+01,  -.52851e+00, 
     $    .47493e+00,  -.69595e+00,   .12480e+02,   .64027e+00 /
      DATA xa1lum(gev350,teslee, 0) /   .15175e+02 /
      DATA (xa1(i,gev350,teslee, 0),i=0,7) /
     $    .33093e+00,   .11137e+01,   .25275e+02,  -.59942e+00, 
     $    .49623e+00,  -.70403e+00,   .60188e+02,   .44637e+00 /
      DATA xa1lum(gev500,teslee, 0) /   .21622e+02 /
      DATA (xa1(i,gev500,teslee, 0),i=0,7) /
     $    .27175e+00,   .10697e+01,   .14858e+02,  -.58418e+00, 
     $    .50824e+00,  -.70387e+00,   .36129e+02,   .53002e+00 /
      DATA xa1lum(gev800,teslee, 0) /   .43979e+02 /
      DATA (xa1(i,gev800,teslee, 0),i=0,7) /
     $    .22994e+00,   .10129e+01,   .81905e+01,  -.55751e+00, 
     $    .46551e+00,  -.70461e+00,   .19394e+02,   .58387e+00 /
      DATA xa1lum(tev1,  teslee, 0) /   .25465e+02 /
      DATA (xa1(i,tev1,  teslee, 0),i=0,7) /
     $    .37294e+00,   .67522e+00,   .87504e+01,  -.60576e+00, 
     $    .35095e+00,  -.69821e+00,   .18526e+02,   .42784e+00 /
      DATA xa1lum(gev500,xbndee, 0) /   .13970e+02 /
      DATA (xa1(i,gev500,xbndee, 0),i=0,7) /
     $    .47296e+00,   .46800e+00,   .58897e+01,  -.61689e+00, 
     $    .27181e+00,  -.68923e+00,   .10087e+02,   .37462e+00 /
      DATA xa1lum(tev1,  xbndee, 0) /   .41056e+02 /
      DATA (xa1(i,tev1,  xbndee, 0),i=0,7) /
     $    .27965e+00,   .74816e+00,   .27415e+01,  -.50491e+00, 
     $    .38320e+00,  -.67945e+00,   .47506e+01,   .62218e+00 /
      DATA xa1lum(gev350,sbndee,0) / -1d0 /
      DATA xa1lum(gev350,xbndee,0) / -1d0 /
      DATA xa1lum(gev800,sbndee,0) / -1d0 /
      DATA xa1lum(gev800,xbndee,0) / -1d0 /
      DATA xa1lum(gev500,sband, 0) /   .31528e+02 /
      DATA (xa1(i,gev500,sband, 0),i=0,7) /
     $    .38169e+00,   .73949e+00,   .12543e+02,  -.61112e+00, 
     $    .51256e+00,  -.69009e+00,   .14892e+02,   .63314e+00 /
      DATA xa1lum(tev1,  sband, 0) /   .24613e+03 /
      DATA (xa1(i,tev1,  sband, 0),i=0,7) /
     $    .24256e+00,   .94117e+00,   .66775e+01,  -.55160e+00, 
     $    .57484e+00,  -.68891e+00,   .92271e+01,   .81162e+00 /
      DATA xa1lum(gev350,tesla, 0) /   .74549e+02 /
      DATA (xa1(i,gev350,tesla, 0),i=0,7) /
     $    .34120e+00,   .12230e+01,   .32932e+02,  -.59850e+00, 
     $    .65947e+00,  -.69574e+00,   .38116e+02,   .63879e+00 /
      DATA xa1lum(gev500,tesla, 0) /   .10668e+03 /
      DATA (xa1(i,gev500,tesla, 0),i=0,7) /
     $    .28082e+00,   .11074e+01,   .18399e+02,  -.59118e+00, 
     $    .68880e+00,  -.69375e+00,   .23463e+02,   .76073e+00 /
      DATA xa1lum(gev800,tesla, 0) /   .29006e+03 /
      DATA (xa1(i,gev800,tesla, 0),i=0,7) /
     $    .21272e+00,   .11443e+01,   .92564e+01,  -.54657e+00, 
     $    .66799e+00,  -.69137e+00,   .12498e+02,   .87571e+00 /
      DATA xa1lum(tev1,  tesla, 0) /   .11009e+03 /
      DATA (xa1(i,tev1,  tesla, 0),i=0,7) /
     $    .41058e+00,   .64745e+00,   .11271e+02,  -.61996e+00, 
     $    .39801e+00,  -.69150e+00,   .14560e+02,   .49924e+00 /
      DATA xa1lum(gev500,xband, 0) /   .36179e+02 /
      DATA (xa1(i,gev500,xband, 0),i=0,7) /
     $    .51155e+00,   .43313e+00,   .70446e+01,  -.63003e+00, 
     $    .29449e+00,  -.68747e+00,   .83489e+01,   .42458e+00 /
      DATA xa1lum(tev1,  xband, 0) /   .11748e+03 /
      DATA (xa1(i,tev1,  xband, 0),i=0,7) /
     $    .32917e+00,   .54322e+00,   .28493e+01,  -.57959e+00, 
     $    .39266e+00,  -.68217e+00,   .38475e+01,   .68478e+00 /
      DATA xa1lum(gev350,sband,0) / -1d0 /
      DATA xa1lum(gev350,xband,0) / -1d0 /
      DATA xa1lum(gev800,sband,0) / -1d0 /
      DATA xa1lum(gev800,xband,0) / -1d0 /
      DATA xa3lum(gev800,tesla, 3) /   .17196e+03 /
      DATA (xa3(i,gev800,tesla, 3),i=0,7) /
     $    .21633e+00,   .11333e+01,   .95928e+01,  -.55095e+00, 
     $    .73044e+00,  -.69101e+00,   .12868e+02,   .94737e+00 /
      DATA xa3lum(gev800,tesla, 4) /   .16408e+03 /
      DATA (xa3(i,gev800,tesla, 4),i=0,7) /
     $    .41828e+00,   .72418e+00,   .14137e+02,  -.61189e+00, 
     $    .36697e+00,  -.69205e+00,   .17713e+02,   .43583e+00 /
      DATA xa3lum(gev350,tesla, 5) /  0.66447e+02 /
      DATA (xa3(i,gev350,tesla, 5),i=0,7) /
     $   0.69418e+00,  0.50553e+00,  0.48430e+02, -0.63911e+00, 
     $   0.34074e+00, -0.69533e+00,  0.55502e+02,  0.29397e+00 /
      DATA xa3lum(gev500,tesla, 5) /  0.95241e+02 /
      DATA (xa3(i,gev500,tesla, 5),i=0,7) /
     $   0.64882e+00,  0.45462e+00,  0.27103e+02, -0.64535e+00, 
     $   0.35101e+00, -0.69467e+00,  0.33658e+02,  0.35024e+00 /
      DATA xa3lum(gev800,tesla, 5) /  0.16974e+03 /
      DATA (xa3(i,gev800,tesla, 5),i=0,7) /
     $   0.58706e+00,  0.43771e+00,  0.13422e+02, -0.63804e+00, 
     $   0.35541e+00, -0.69467e+00,  0.17528e+02,  0.43051e+00 /
      DATA xa3lum(tev1,tesla, 5) /  0.21222e+03 /
      DATA (xa3(i,tev1,tesla, 5),i=0,7) /
     $   0.55525e+00,  0.42577e+00,  0.96341e+01, -0.63587e+00, 
     $   0.36448e+00, -0.69365e+00,  0.13161e+02,  0.47715e+00 /
      DATA xa3lum(tev16,tesla, 5) /  0.34086e+03 /
      DATA (xa3(i,tev16,tesla, 5),i=0,7) /
     $   0.49058e+00,  0.42609e+00,  0.50550e+01, -0.61867e+00, 
     $   0.39225e+00, -0.68916e+00,  0.75514e+01,  0.58754e+00 /
      DATA xa3lum(gev350,tesla, 0) /  0.66447e+02 /
      DATA (xa3(i,gev350,tesla, 0),i=0,7) /
     $   0.69418e+00,  0.50553e+00,  0.48430e+02, -0.63911e+00, 
     $   0.34074e+00, -0.69533e+00,  0.55502e+02,  0.29397e+00 /
      DATA xa3lum(gev500,tesla, 0) /  0.95241e+02 /
      DATA (xa3(i,gev500,tesla, 0),i=0,7) /
     $   0.64882e+00,  0.45462e+00,  0.27103e+02, -0.64535e+00, 
     $   0.35101e+00, -0.69467e+00,  0.33658e+02,  0.35024e+00 /
      DATA xa3lum(gev800,tesla, 0) /  0.16974e+03 /
      DATA (xa3(i,gev800,tesla, 0),i=0,7) /
     $   0.58706e+00,  0.43771e+00,  0.13422e+02, -0.63804e+00, 
     $   0.35541e+00, -0.69467e+00,  0.17528e+02,  0.43051e+00 /
      DATA xa3lum(tev1,tesla, 0) /  0.21222e+03 /
      DATA (xa3(i,tev1,tesla, 0),i=0,7) /
     $   0.55525e+00,  0.42577e+00,  0.96341e+01, -0.63587e+00, 
     $   0.36448e+00, -0.69365e+00,  0.13161e+02,  0.47715e+00 /
      DATA xa3lum(tev16,tesla, 0) /  0.34086e+03 /
      DATA (xa3(i,tev16,tesla, 0),i=0,7) /
     $   0.49058e+00,  0.42609e+00,  0.50550e+01, -0.61867e+00, 
     $   0.39225e+00, -0.68916e+00,  0.75514e+01,  0.58754e+00 /
      DATA xa5lum(gev350,tesla, 1) /  -1.0 /
      DATA xa5lum(gev500,tesla, 1) /  0.33980e+03 /
      DATA (xa5(i,gev500,tesla, 1),i=0,7) /
     $   0.49808e+00,  0.54613e+00,  0.12287e+02, -0.62756e+00, 
     $   0.42817e+00, -0.69120e+00,  0.17067e+02,  0.51143e+00 /
      DATA xa5lum(gev800,tesla, 1) /  0.35936e+03 /
      DATA (xa5(i,gev800,tesla, 1),i=0,7) /
     $   0.58751e+00,  0.43128e+00,  0.13324e+02, -0.64006e+00, 
     $   0.30682e+00, -0.69235e+00,  0.16815e+02,  0.37078e+00 /
      DATA xa5lum(tev1,  tesla, 1) /  -1.0 /
      DATA xa5lum(tev16, tesla, 1) /  -1.0 /
      DATA xa5lum(gev350,tesla, 0) /  -1.0 /
      DATA xa5lum(gev500,tesla, 0) /  0.33980e+03 /
      DATA (xa5(i,gev500,tesla, 0),i=0,7) /
     $   0.49808e+00,  0.54613e+00,  0.12287e+02, -0.62756e+00, 
     $   0.42817e+00, -0.69120e+00,  0.17067e+02,  0.51143e+00 /
      DATA xa5lum(gev800,tesla, 0) /  0.35936e+03 /
      DATA (xa5(i,gev800,tesla, 0),i=0,7) /
     $   0.58751e+00,  0.43128e+00,  0.13324e+02, -0.64006e+00, 
     $   0.30682e+00, -0.69235e+00,  0.16815e+02,  0.37078e+00 /
      DATA xa5lum(tev1,  tesla, 0) /  -1.0 /
      DATA xa5lum(tev16, tesla, 0) /  -1.0 /

      IF (magic  .NE.  1904 06 16) THEN
         magic = 1904 06 16
      x1m = 0d0
      x2m = 0d0
      roots = 500d0
      acc = tesla
      ver = 0
      rev = 0
      chat = 1
      IF (xchat  .NE.  0) THEN
         CALL IRC_circem ('message', 'starting up ...')
         CALL IRC_circem ('message',
     $        '$id: circe.nw,v 1.28 1998/05/05 10:37:32 ohl exp $')
      ENDIF
      ENDIF
      IF ((xchat  .GE.  0)  .AND.  (xchat  .NE.  chat)) THEN
         chat = xchat
         IF (chat  .GE.  1) THEN
            WRITE (msgbuf, 1000) 'chat', chat
 1000       FORMAT ('updating `', a, ''' to ', i2)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ELSE
         IF (chat  .GE.  2) THEN
            WRITE (msgbuf, 1100) 'chat', chat
 1100       FORMAT ('keeping `', a, ''' at ', i2)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ENDIF
      IF ((xx1m  .GE.  0d0)  .AND.  (xx1m  .NE.  x1m)) THEN
         x1m = xx1m
         IF (chat  .GE.  1) THEN
            WRITE (msgbuf, 1001) 'x1min', x1m
 1001       FORMAT ('updating `', a, ''' to ', e12.4)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ELSE
         IF (chat  .GE.  2) THEN
            WRITE (msgbuf, 1101) 'x1min', x1m
 1101       FORMAT ('keeping `', a, ''' at ', e12.4)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ENDIF
      IF ((xx2m  .GE.  0d0)  .AND.  (xx2m  .NE.  x2m)) THEN
         x2m = xx2m
         IF (chat  .GE.  1) THEN
            WRITE (msgbuf, 1001) 'x2min', x2m
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ELSE
         IF (chat  .GE.  2) THEN
            WRITE (msgbuf, 1101) 'x2min', x2m
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ENDIF
      IF ((xroots  .GE.  0d0)  .AND. (xroots  .NE.  roots)) THEN
         roots = xroots
         IF (chat  .GE.  1) THEN
            WRITE (msgbuf, 1002) 'roots', roots
 1002       FORMAT ('updating `', a, ''' to ', f6.1)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ELSE
         IF (chat  .GE.  2) THEN
            WRITE (msgbuf, 1102) 'roots', roots
 1102       FORMAT ('keeping `', a, ''' at ', f6.1)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ENDIF
      IF ((xacc  .GE.  0)  .AND. (xacc  .NE.  acc)) THEN
         IF ((xacc  .GE.  1)  .AND.  (xacc  .LE.  nacc)) THEN
            acc = xacc
            IF (chat  .GE.  1) THEN
               WRITE (msgbuf, 1003) 'acc', accnam(acc)
 1003          FORMAT ('updating `', a, ''' to ', a)
               CALL IRC_circem ('message', msgbuf)
            ENDIF
         ELSE
            WRITE (msgbuf, 1203) xacc
 1203       FORMAT ('invalid `acc'': ', i8)
            CALL IRC_circem ('error', msgbuf)
            WRITE (msgbuf, 1103) 'acc', accnam(acc)
 1103       FORMAT ('keeping `', a, ''' at ', a)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ELSE
         IF (chat  .GE.  2) THEN
            WRITE (msgbuf, 1003) 'acc', accnam(acc)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ENDIF
      IF ((acc  .EQ.  sbndee)  .OR.  (acc  .EQ.  teslee)
     $     .OR.  (acc  .EQ.  xbndee)) THEN
      CALL IRC_circem ('warning', '***********************************')
      CALL IRC_circem ('warning', '* the accelerator PARAMETERs have *')
      CALL IRC_circem ('warning', '* not been ENDorsed for use in    *')
      CALL IRC_circem ('warning', '* an e-e- collider yet!!!         *')
      CALL IRC_circem ('warning', '***********************************')
      ENDIF
      IF ((xver  .GE.  0)  .AND.  (xver  .NE.  zver)) THEN
         ver = xver
         zver = xver
         IF (chat  .GE.  1) THEN
            WRITE (msgbuf, 1000) 'ver', ver
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ELSE
         IF (chat  .GE.  2) THEN
            WRITE (msgbuf, 1100) 'ver', ver
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ENDIF
      IF ((xrev  .GE.  0)  .AND. (xrev  .NE.  rev)) THEN
         rev = xrev
         IF (chat  .GE.  1) THEN
            WRITE (msgbuf, 1004) 'rev', rev
 1004       FORMAT ('updating `', a, ''' to ', i8)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ELSE
         IF (chat  .GE.  2) THEN
            WRITE (msgbuf, 1104) 'rev', rev
 1104       FORMAT ('keeping `', a, ''' at ', i8)
            CALL IRC_circem ('message', msgbuf)
         ENDIF
      ENDIF
      ver34 = 0
      IF ((ver  .EQ.  1)  .OR.  (ver  .EQ.  0)) THEN
      IF (rev  .EQ.  0) THEN
         r = 0
      CALL IRC_circem ('warning', '*************************************')
      CALL IRC_circem ('warning', '* this release is not official yet, *')
      CALL IRC_circem ('warning', '* DO not use it in publications!    *')
      CALL IRC_circem ('warning', '*************************************')
      ELSEIF (rev  .GE.  1997 04 17) THEN
         r = 5
      ELSEIF (rev  .GE.  1996 09 02) THEN
         r = 4
      ELSEIF (rev  .GE.  1996 07 29) THEN
         r = 3
      ELSEIF (rev  .GE.  1996 07 11) THEN
         r = 2
      ELSEIF (rev  .GE.  1996 04 01) THEN
         r = 1
      ELSEIF (rev  .LT.  1996 04 01) THEN
         CALL IRC_circem ('error',
     $        'no revision of version 1 before 96/04/01 available')
         CALL IRC_circem ('message', 'falling back to default')
         r = 1
      ENDIF
      IF (chat  .GE.  2) THEN
         WRITE (msgbuf, 2000) rev, r
 2000    FORMAT ('mapping date ', i8, ' to revision index ', i2)
         CALL IRC_circem ('message', msgbuf)
      ENDIF
      IF (roots  .EQ.  350d0) THEN
         e = gev350
      ELSEIF ((roots  .GE.  340d0)  .AND.  (roots  .LE.  370d0)) THEN
         WRITE (msgbuf, 2001) roots, 350d0
         CALL IRC_circem ('message', msgbuf)
         e = gev350
      ELSEIF (roots  .EQ.  500d0) THEN
         e = gev500
      ELSEIF ((roots  .GE.  480d0)  .AND.  (roots  .LE.  520d0)) THEN
         WRITE (msgbuf, 2001) roots, 500d0
         CALL IRC_circem ('message', msgbuf)
         e = gev500
      ELSEIF (roots  .EQ.  800d0) THEN
         e = gev800
      ELSEIF ((roots  .GE.  750d0)  .AND.  (roots  .LE.  850d0)) THEN
         WRITE (msgbuf, 2001) roots, 800d0
         CALL IRC_circem ('message', msgbuf)
         e = gev800
      ELSEIF (roots  .EQ.  1000d0) THEN
         e = tev1
      ELSEIF ((roots  .GE.  900d0)  .AND.  (roots  .LE.  1100d0)) THEN
         WRITE (msgbuf, 2001) roots, 1000d0
         CALL IRC_circem ('message', msgbuf)
         e = tev1
      ELSEIF (roots  .EQ.  1600d0) THEN
         e = tev16
      ELSEIF ((roots  .GE.  1500d0)  .AND.  (roots  .LE.  1700d0)) THEN
         WRITE (msgbuf, 2001) roots, 1600d0
         CALL IRC_circem ('message', msgbuf)
         e = tev16
      ELSE
         CALL IRC_circem ('error',
     $        'only roots = 350, 500, 800, 1000 and 1600gev available')
         CALL IRC_circem ('message', 'falling back to 500gev')
         e = gev500
      ENDIF
      IF (xa1lum(e,acc,r)  .LT.  0d0) THEN
         WRITE (msgbuf, 2002) roots, accnam(acc), r
         CALL IRC_circem ('error', msgbuf)
         CALL IRC_circem ('message', 'falling back to 500gev')
         e = gev500
      ENDIF
      IF (chat  .GE.  2) THEN
         WRITE (msgbuf, 2003) roots, e
         CALL IRC_circem ('message', msgbuf)
      ENDIF
      lumi = xa1lum (e,acc,r)
      DO 10 i = 0, 7
         a1(i) = xa1(i,e,acc,r)
 10   CONTINUE
      ELSEIF ((ver  .EQ.  3)  .OR.  (ver  .EQ.  4)) THEN
         ver34 = ver
         ver = 1
      IF (rev  .EQ.  0) THEN
         r = 0
      CALL IRC_circem ('warning', '*************************************')
      CALL IRC_circem ('warning', '* this release is not official yet, *')
      CALL IRC_circem ('warning', '* DO not use it in publications!    *')
      CALL IRC_circem ('warning', '*************************************')
      ELSEIF (rev  .GE.  1997 04 17) THEN
         r = 5
         IF (ver34  .EQ.  3) THEN
            CALL IRC_circem ('warning', 'version 3 retired after 97/04/17')
            CALL IRC_circem ('message', 'falling back to version 4')
         ENDIF
      ELSEIF (rev  .GE.  1996 10 22) THEN
         r = ver34
         IF ((roots  .NE.  800d0)  .OR.  (acc  .NE.  tesla)) THEN
            CALL IRC_circem ('error', 'versions 3 and 4 before 97/04/17')
            CALL IRC_circem ('error', 'apply to tesla at 800 gev only')
            CALL IRC_circem ('message', 'falling back to tesla at 800gev')
            acc = tesla
            e = gev800
         ENDIF
      ELSEIF (rev  .LT.  1996 10 22) THEN
         CALL IRC_circem ('error',
     $     'no revision of versions 3 and 4 available before 96/10/22')
         CALL IRC_circem ('message', 'falling back to default')
         r = 5
      ENDIF
      IF (chat  .GE.  2) THEN
         WRITE (msgbuf, 2000) rev, r
         CALL IRC_circem ('message', msgbuf)
      ENDIF
      IF (roots  .EQ.  350d0) THEN
         e = gev350
      ELSEIF ((roots  .GE.  340d0)  .AND.  (roots  .LE.  370d0)) THEN
         WRITE (msgbuf, 2001) roots, 350d0
         CALL IRC_circem ('message', msgbuf)
         e = gev350
      ELSEIF (roots  .EQ.  500d0) THEN
         e = gev500
      ELSEIF ((roots  .GE.  480d0)  .AND.  (roots  .LE.  520d0)) THEN
         WRITE (msgbuf, 2001) roots, 500d0
         CALL IRC_circem ('message', msgbuf)
         e = gev500
      ELSEIF (roots  .EQ.  800d0) THEN
         e = gev800
      ELSEIF ((roots  .GE.  750d0)  .AND.  (roots  .LE.  850d0)) THEN
         WRITE (msgbuf, 2001) roots, 800d0
         CALL IRC_circem ('message', msgbuf)
         e = gev800
      ELSEIF (roots  .EQ.  1000d0) THEN
         e = tev1
      ELSEIF ((roots  .GE.  900d0)  .AND.  (roots  .LE.  1100d0)) THEN
         WRITE (msgbuf, 2001) roots, 1000d0
         CALL IRC_circem ('message', msgbuf)
         e = tev1
      ELSEIF (roots  .EQ.  1600d0) THEN
         e = tev16
      ELSEIF ((roots  .GE.  1500d0)  .AND.  (roots  .LE.  1700d0)) THEN
         WRITE (msgbuf, 2001) roots, 1600d0
         CALL IRC_circem ('message', msgbuf)
         e = tev16
      ELSE
         CALL IRC_circem ('error',
     $        'only roots = 350, 500, 800, 1000 and 1600gev available')
         CALL IRC_circem ('message', 'falling back to 500gev')
         e = gev500
      ENDIF
      IF (xa3lum(e,acc,r)  .LT.  0d0) THEN
         WRITE (msgbuf, 2002) roots, accnam(acc), r
         CALL IRC_circem ('error', msgbuf)
         CALL IRC_circem ('message', 'falling back to 500gev')
         e = gev500
      ENDIF
      IF (chat  .GE.  2) THEN
         WRITE (msgbuf, 2003) roots, e
         CALL IRC_circem ('message', msgbuf)
      ENDIF
      lumi = xa3lum (e,acc,r)
      DO 20 i = 0, 7
         a1(i) = xa3(i,e,acc,r)
 20   CONTINUE
      ELSEIF (ver  .EQ.  5) THEN
         ver = 1
      IF (rev  .EQ.  0) THEN
         r = 0
      CALL IRC_circem ('warning', '*************************************')
      CALL IRC_circem ('warning', '* this release is not official yet, *')
      CALL IRC_circem ('warning', '* DO not use it in publications!    *')
      CALL IRC_circem ('warning', '*************************************')
      ELSEIF (rev  .GE.  1998 05 05) THEN
         r = 1
      ELSEIF (rev  .LT.  1998 05 05) THEN
         CALL IRC_circem ('error',
     $     'no revision of version 5 available before 98/05/05')
         CALL IRC_circem ('message', 'falling back to default')
         r = 1
      ENDIF
      IF (chat  .GE.  2) THEN
         WRITE (msgbuf, 2000) rev, r
         CALL IRC_circem ('message', msgbuf)
      ENDIF
      IF (acc  .NE.  tesla) THEN
         CALL IRC_circem ('error', 'versions 5 applies to tesla only')
         acc = tesla
      END IF
      IF (roots  .EQ.  350d0) THEN
         e = gev350
      ELSEIF ((roots  .GE.  340d0)  .AND.  (roots  .LE.  370d0)) THEN
         WRITE (msgbuf, 2001) roots, 350d0
         CALL IRC_circem ('message', msgbuf)
         e = gev350
      ELSEIF (roots  .EQ.  500d0) THEN
         e = gev500
      ELSEIF ((roots  .GE.  480d0)  .AND.  (roots  .LE.  520d0)) THEN
         WRITE (msgbuf, 2001) roots, 500d0
         CALL IRC_circem ('message', msgbuf)
         e = gev500
      ELSEIF (roots  .EQ.  800d0) THEN
         e = gev800
      ELSEIF ((roots  .GE.  750d0)  .AND.  (roots  .LE.  850d0)) THEN
         WRITE (msgbuf, 2001) roots, 800d0
         CALL IRC_circem ('message', msgbuf)
         e = gev800
      ELSEIF (roots  .EQ.  1000d0) THEN
         e = tev1
      ELSEIF ((roots  .GE.  900d0)  .AND.  (roots  .LE.  1100d0)) THEN
         WRITE (msgbuf, 2001) roots, 1000d0
         CALL IRC_circem ('message', msgbuf)
         e = tev1
      ELSEIF (roots  .EQ.  1600d0) THEN
         e = tev16
      ELSEIF ((roots  .GE.  1500d0)  .AND.  (roots  .LE.  1700d0)) THEN
         WRITE (msgbuf, 2001) roots, 1600d0
         CALL IRC_circem ('message', msgbuf)
         e = tev16
      ELSE
         CALL IRC_circem ('error',
     $        'only roots = 350, 500, 800, 1000 and 1600gev available')
         CALL IRC_circem ('message', 'falling back to 500gev')
         e = gev500
      ENDIF
      IF (xa5lum(e,acc,r)  .LT.  0d0) THEN
         WRITE (msgbuf, 2002) roots, accnam(acc), r
         CALL IRC_circem ('error', msgbuf)
         CALL IRC_circem ('message', 'falling back to 500gev')
         e = gev500
      ENDIF
      IF (chat  .GE.  2) THEN
         WRITE (msgbuf, 2003) roots, e
         CALL IRC_circem ('message', msgbuf)
      ENDIF
      lumi = xa5lum (e,acc,r)
      DO 30 i = 0, 7
         a1(i) = xa5(i,e,acc,r)
 30   CONTINUE
      ELSEIF (ver  .EQ.  2) THEN
      CALL IRC_circem ('panic', '*********************************')
      CALL IRC_circem ('panic', '* version 2 has been retired,   *')
      CALL IRC_circem ('panic', '* please use version 1 instead! *')
      CALL IRC_circem ('panic', '*********************************')
      RETURN
      ELSEIF (ver  .GT.  5) THEN
         CALL IRC_circem ('panic', 'versions >5 not available yet')
         RETURN
      ELSE
         CALL IRC_circem ('panic', 'version must be positive')
         RETURN
      ENDIF
 2001 FORMAT ('treating energy ', f6.1, 'gev as ',  f6.1, 'gev')
 2002 FORMAT ('energy ', f6.1, ' not available for ', a6,
     $        ' in revison ', i2)
 2003 FORMAT ('mapping energy ', f6.1, ' to energy index ', i2)
      END
      SUBROUTINE IRC_circel (l)
      IMPLICIT NONE
      DOUBLE PRECISION l
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      l = lumi
      END
      DOUBLE PRECISION FUNCTION IRC_circee (x1, x2)
      IMPLICIT NONE
      DOUBLE PRECISION x1, x2
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      DOUBLE PRECISION d1, d2
      IF (magic  .NE.  magic0) THEN
         CALL IRC_circes (-1d0, -1d0, -1d0, -1, -1, -1, -1)
      ENDIF
      IRC_circee = -1.0
      IF ((ver  .EQ.  1)  .OR.  (ver  .EQ.  0)) THEN
      IF (x1  .EQ.  1d0) THEN
         d1 = a1(0)
      ELSEIF (x1  .LT.  1d0  .AND.  x1  .GT.  0d0) THEN
         d1 = a1(1) * x1**a1(2) * (1d0 - x1)**a1(3)
      ELSEIF (x1  .EQ.  -1d0) THEN
         d1 = 1d0 - a1(0)
      ELSE
         d1 = 0d0
      ENDIF
      IF (x2  .EQ.  1d0) THEN
         d2 = a1(0)
      ELSEIF (x2  .LT.  1d0  .AND.  x2  .GT.  0d0) THEN
         d2 = a1(1) * x2**a1(2) * (1d0 - x2)**a1(3)
      ELSEIF (x2  .EQ.  -1d0) THEN
         d2 = 1d0 - a1(0)
      ELSE
         d2 = 0d0
      ENDIF
      IRC_circee = d1 * d2
      ELSEIF (ver  .EQ.  2) THEN
      CALL IRC_circem ('panic', '*********************************')
      CALL IRC_circem ('panic', '* version 2 has been retired,   *')
      CALL IRC_circem ('panic', '* please use version 1 instead! *')
      CALL IRC_circem ('panic', '*********************************')
      RETURN
      ELSEIF (ver  .GT.  5) THEN
         CALL IRC_circem ('panic', 'versions >5 not available yet')
         RETURN
      ELSE
         CALL IRC_circem ('panic', 'version must be positive')
         RETURN
      ENDIF
      END
      DOUBLE PRECISION FUNCTION IRC_circeg (x1, x2)
      IMPLICIT NONE
      DOUBLE PRECISION x1, x2
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      DOUBLE PRECISION d1, d2
      IF (magic  .NE.  magic0) THEN
         CALL IRC_circes (-1d0, -1d0, -1d0, -1, -1, -1, -1)
      ENDIF
      IRC_circeg = -1.0
      IF ((ver  .EQ.  1)  .OR.  (ver  .EQ.  0)) THEN
      IF (x1  .EQ.  1d0) THEN
         d1 = a1(0)
      ELSEIF (x1  .LT.  1d0  .AND.  x1  .GT.  0d0) THEN
         d1 = a1(1) * x1**a1(2) * (1d0 - x1)**a1(3)
      ELSEIF (x1  .EQ.  -1d0) THEN
         d1 = 1d0 - a1(0)
      ELSE
         d1 = 0d0
      ENDIF
      IF (x2  .LT.  1d0  .AND.  x2  .GT.  0d0) THEN
         d2 = a1(4) * x2**a1(5) * (1d0 - x2)**a1(6)
      ELSEIF (x2  .EQ.  -1d0) THEN
         d2 = a1(7)
      ELSE
         d2 = 0d0
      ENDIF
      IRC_circeg = d1 * d2
      ELSEIF (ver  .EQ.  2) THEN
      CALL IRC_circem ('panic', '*********************************')
      CALL IRC_circem ('panic', '* version 2 has been retired,   *')
      CALL IRC_circem ('panic', '* please use version 1 instead! *')
      CALL IRC_circem ('panic', '*********************************')
      RETURN
      ELSEIF (ver  .GT.  5) THEN
         CALL IRC_circem ('panic', 'versions >5 not available yet')
         RETURN
      ELSE
         CALL IRC_circem ('panic', 'version must be positive')
         RETURN
      ENDIF
      END
      DOUBLE PRECISION FUNCTION IRC_circgg (x1, x2)
      IMPLICIT NONE
      DOUBLE PRECISION x1, x2
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      DOUBLE PRECISION d1, d2
      IF (magic  .NE.  magic0) THEN
         CALL IRC_circes (-1d0, -1d0, -1d0, -1, -1, -1, -1)
      ENDIF
      IRC_circgg = -1.0
      IF ((ver  .EQ.  1)  .OR.  (ver  .EQ.  0)) THEN
      IF (x1  .LT.  1d0  .AND.  x1  .GT.  0d0) THEN
         d1 = a1(4) * x1**a1(5) * (1d0 - x1)**a1(6)
      ELSEIF (x1  .EQ.  -1d0) THEN
         d1 = a1(7)
      ELSE
         d1 = 0d0
      ENDIF
      IF (x2  .LT.  1d0  .AND.  x2  .GT.  0d0) THEN
         d2 = a1(4) * x2**a1(5) * (1d0 - x2)**a1(6)
      ELSEIF (x2  .EQ.  -1d0) THEN
         d2 = a1(7)
      ELSE
         d2 = 0d0
      ENDIF
      IRC_circgg = d1 * d2
      ELSEIF (ver  .EQ.  2) THEN
      CALL IRC_circem ('panic', '*********************************')
      CALL IRC_circem ('panic', '* version 2 has been retired,   *')
      CALL IRC_circem ('panic', '* please use version 1 instead! *')
      CALL IRC_circem ('panic', '*********************************')
      RETURN
      ELSEIF (ver  .GT.  5) THEN
         CALL IRC_circem ('panic', 'versions >5 not available yet')
         RETURN
      ELSE
         CALL IRC_circem ('panic', 'version must be positive')
         RETURN
      ENDIF
      END
      SUBROUTINE IRC_girce (x1, x2, p1, p2, rng)
      IMPLICIT NONE
      DOUBLE PRECISION x1, x2
      INTEGER p1, p2
      EXTERNAL rng
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      DOUBLE PRECISION u, w, IRC_circgg
      INTEGER electr, positr, photon
      PARAMETER (electr =  11)
      PARAMETER (positr = -11)
      PARAMETER (photon =  22)
      IF (magic  .NE.  magic0) THEN
         CALL IRC_circes (-1d0, -1d0, -1d0, -1, -1, -1, -1)
      ENDIF
 99   CONTINUE
      w = 1d0 / (1d0 + IRC_circgg (-1d0, -1d0))
      CALL rng (u)
      IF (u*u  .LE.  w) THEN
         p1 = positr
      ELSE
         p1 = photon
      ENDIF
      CALL rng (u)
      IF (u*u  .LE.  w) THEN
         p2 = electr
      ELSE
         p2 = photon
      ENDIF
      IF (ABS(p1)  .EQ.  electr) THEN
         IF (ABS(p2)  .EQ.  electr) THEN
            CALL IRC_gircee (x1, x2, rng)
         ELSEIF (p2  .EQ.  photon) THEN
            CALL IRC_girceg (x1, x2, rng)
         ENDIF
      ELSEIF (p1  .EQ.  photon) THEN
         IF (ABS(p2)  .EQ.  electr) THEN
            CALL IRC_girceg (x2, x1, rng)
         ELSEIF (p2  .EQ.  photon) THEN
            CALL IRC_gircgg (x1, x2, rng)
         ENDIF
      ENDIF
      IF ((x1  .LT.  x1m)  .OR.  (x2  .LT.  x2m)) GOTO 99
      END
      SUBROUTINE IRC_gircee (x1, x2, rng)
      IMPLICIT NONE
      DOUBLE PRECISION x1, x2
      EXTERNAL rng
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      DOUBLE PRECISION u, IRC_girceb
      IF (magic  .NE.  magic0) THEN
         CALL IRC_circes (-1d0, -1d0, -1d0, -1, -1, -1, -1)
      ENDIF
      IF ((ver  .EQ.  1)  .OR.  (ver  .EQ.  0)) THEN
      CALL rng (u)
      IF (u  .LE.  a1(0)) THEN
         x1 = 1d0
      ELSE
         x1 = 1d0 - IRC_girceb (0d0, 1d0-x1m, a1(3)+1d0, a1(2)+1d0, rng)
      ENDIF
      CALL rng (u)
      IF (u  .LE.  a1(0)) THEN
         x2 = 1d0
      ELSE
         x2 = 1d0 - IRC_girceb (0d0, 1d0-x2m, a1(3)+1d0, a1(2)+1d0, rng)
      ENDIF
      ELSEIF (ver  .EQ.  2) THEN
      CALL IRC_circem ('panic', '*********************************')
      CALL IRC_circem ('panic', '* version 2 has been retired,   *')
      CALL IRC_circem ('panic', '* please use version 1 instead! *')
      CALL IRC_circem ('panic', '*********************************')
      RETURN
      ELSEIF (ver  .GT.  5) THEN
         CALL IRC_circem ('panic', 'versions >5 not available yet')
         RETURN
      ELSE
         CALL IRC_circem ('panic', 'version must be positive')
         RETURN
      ENDIF
      END
      SUBROUTINE IRC_girceg (x1, x2, rng)
      IMPLICIT NONE
      DOUBLE PRECISION x1, x2
      EXTERNAL rng
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      DOUBLE PRECISION u, IRC_girceb
      IF (magic  .NE.  magic0) THEN
         CALL IRC_circes (-1d0, -1d0, -1d0, -1, -1, -1, -1)
      ENDIF
      IF ((ver  .EQ.  1)  .OR.  (ver  .EQ.  0)) THEN
      CALL rng (u)
      IF (u  .LE.  a1(0)) THEN
         x1 = 1d0
      ELSE
         x1 = 1d0 - IRC_girceb (0d0, 1d0-x1m, a1(3)+1d0, a1(2)+1d0, rng)
      ENDIF
      x2 = IRC_girceb (x2m, 1d0, a1(5)+1d0, a1(6)+1d0, rng)
      ELSEIF (ver  .EQ.  2) THEN
      CALL IRC_circem ('panic', '*********************************')
      CALL IRC_circem ('panic', '* version 2 has been retired,   *')
      CALL IRC_circem ('panic', '* please use version 1 instead! *')
      CALL IRC_circem ('panic', '*********************************')
      RETURN
      ELSEIF (ver  .GT.  5) THEN
         CALL IRC_circem ('panic', 'versions >5 not available yet')
         RETURN
      ELSE
         CALL IRC_circem ('panic', 'version must be positive')
         RETURN
      ENDIF
      END
      SUBROUTINE IRC_gircgg (x1, x2, rng)
      IMPLICIT NONE
      DOUBLE PRECISION x1, x2
      EXTERNAL rng
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      DOUBLE PRECISION IRC_girceb
      IF (magic  .NE.  magic0) THEN
         CALL IRC_circes (-1d0, -1d0, -1d0, -1, -1, -1, -1)
      ENDIF
      IF ((ver  .EQ.  1)  .OR.  (ver  .EQ.  0)) THEN
      x1 = IRC_girceb (x1m, 1d0, a1(5)+1d0, a1(6)+1d0, rng)
      x2 = IRC_girceb (x2m, 1d0, a1(5)+1d0, a1(6)+1d0, rng)
      ELSEIF (ver  .EQ.  2) THEN
      CALL IRC_circem ('panic', '*********************************')
      CALL IRC_circem ('panic', '* version 2 has been retired,   *')
      CALL IRC_circem ('panic', '* please use version 1 instead! *')
      CALL IRC_circem ('panic', '*********************************')
      RETURN
      ELSEIF (ver  .GT.  5) THEN
         CALL IRC_circem ('panic', 'versions >5 not available yet')
         RETURN
      ELSE
         CALL IRC_circem ('panic', 'version must be positive')
         RETURN
      ENDIF
      END
      DOUBLE PRECISION FUNCTION IRC_girceb (xmin, xmax, a, b, rng)
      IMPLICIT NONE
      DOUBLE PRECISION xmin, xmax, a, b
      EXTERNAL rng
      DOUBLE PRECISION t, p, u, umin, umax, x, w
      IF ((a  .GT.  1d0)  .OR.  (b  .LT.  1d0)) THEN
         IRC_girceb = -1d0
         CALL IRC_circem ('error', 'beta-distribution expects a<=1<=b')
         RETURN
      ENDIF
      t = (1d0 - a) / (b + 1d0 - a)
      p = b*t / (b*t + a * (1d0 - t)**b)
      IF (xmin  .LE.  0d0) THEN
         umin = 0d0
      ELSEIF (xmin  .LT.  t) THEN
         umin = p * (xmin/t)**a
      ELSEIF (xmin  .EQ.  t) THEN
         umin = p
      ELSEIF (xmin  .LT.  1d0) THEN
         umin = 1d0 - (1d0 - p) * ((1d0 - xmin)/(1d0 - t))**b
      ELSE
         umin = 1d0
      ENDIF
      IF (xmax  .GE.  1d0) THEN
         umax = 1d0
      ELSEIF (xmax  .GT.  t) THEN
         umax = 1d0 - (1d0 - p) * ((1d0 - xmax)/(1d0 - t))**b
      ELSEIF (xmax  .EQ.  t) THEN
         umax = p
      ELSEIF (xmax  .GT.  0d0) THEN
         umax = p * (xmax/t)**a
      ELSE
         umax = 0d0
      ENDIF
      IF (umax  .LT.  umin) THEN
         IRC_girceb = -1d0
         RETURN
      ENDIF
 10   CONTINUE
      CALL rng (u)
      u = umin + (umax - umin) * u
      IF (u  .LE.  p) THEN
         x = t * (u/p)**(1d0/a)
         w = (1d0 - x)**(b-1d0)
      ELSE
         x = 1d0 - (1d0 - t) * ((1d0 - u)/(1d0 - p))**(1d0/b)
         w = (x/t)**(a-1d0)
      ENDIF
         CALL rng (u)
      IF (w  .LE.  u) GOTO 10
      IRC_girceb = x
      END
      SUBROUTINE IRC_circem (errlvl, errmsg)
      IMPLICIT NONE
      CHARACTER*(*) errlvl, errmsg
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      INTEGER errcnt
      SAVE errcnt
      DATA errcnt /0/
      IF (errlvl  .EQ.  'message') THEN
         print *, 'circe:message: ', errmsg
      ELSEIF (errlvl  .EQ.  'warning') THEN
         IF (errcnt  .LT.  100) THEN
            errcnt = errcnt + 1
            print *, 'circe:warning: ', errmsg
         ELSEIF (errcnt  .EQ.  100) THEN
            errcnt = errcnt + 1
            print *, 'circe:message: more than 100 messages' 
            print *, 'circe:message: turning warnings off' 
         ENDIF
      ELSEIF (errlvl  .EQ.  'error') THEN
         IF (errcnt  .LT.  200) THEN
            errcnt = errcnt + 1
            print *, 'circe:error:   ', errmsg
         ELSEIF (errcnt  .EQ.  200) THEN
            errcnt = errcnt + 1
            print *, 'circe:message: more than 200 messages' 
            print *, 'circe:message: turning error messages off' 
         ENDIF
      ELSEIF (errlvl  .EQ.  'panic') THEN
         IF (errcnt  .LT.  300) THEN
            errcnt = errcnt + 1
            print *, 'circe:panic:   ', errmsg
         ELSEIF (errcnt  .EQ.  300) THEN
            errcnt = errcnt + 1
            print *, 'circe:message: more than 300 messages' 
            print *, 'circe:message: turning panic messages off' 
         ENDIF
      ELSE
         print *, 'circe:panic:    invalid error code ', errlvl
      ENDIF
      END


      SUBROUTINE IRC_GetParamee (Paramee)
*////////////////////////////////////////////////////////////////////////////////////
*//                                                                                //
*//   This is clone of circee  by S.J.                                             //
*//   Instead of function it provides 3 constants to build the function            //
*//                                                                                //
*////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION Paramee(0:3)
*
      DOUBLE PRECISION x1, x2
      INTEGER magic0
      PARAMETER (magic0 = 1904 06 16)
      DOUBLE PRECISION x1m, x2m, roots
      COMMON /c_IRC/ x1m, x2m, roots
      DOUBLE PRECISION lumi
      COMMON /c_IRC/ lumi
      DOUBLE PRECISION a1(0:7)
      COMMON /c_IRC/ a1
      INTEGER acc, ver, rev, chat
      COMMON /c_IRC/ acc, ver, rev, chat
      INTEGER magic
      COMMON /c_IRC/ magic
      INTEGER e, r
      COMMON /c_IRC/ e, r
      SAVE /c_IRC/
      DOUBLE PRECISION d1, d2
      IF (magic  .NE.  magic0) THEN
         CALL IRC_circes (-1d0, -1d0, -1d0, -1, -1, -1, -1)
      ENDIF
      IF ((ver  .EQ.  1)  .OR.  (ver  .EQ.  0)) THEN
*(((((((((((((
******   d2 = a1(1) * x2**a1(2) * (1d0 - x2)**a1(3)
         Paramee(0) = a1(0)     ! Normalization of delta part
         Paramee(1) = a1(1)     ! Normalization of beta part
         Paramee(2) = a1(2)     ! Power at x=0, big one
         Paramee(3) = a1(3)     ! Power at x=1, small one
*))))))))))))))
      ELSEIF (ver  .EQ.  2) THEN
      CALL IRC_circem ('panic', '*********************************')
      CALL IRC_circem ('panic', '* version 2 has been retired,   *')
      CALL IRC_circem ('panic', '* please use version 1 instead! *')
      CALL IRC_circem ('panic', '*********************************')
      RETURN
      ELSEIF (ver  .GT.  5) THEN
         CALL IRC_circem ('panic', 'versions >5 not available yet')
         RETURN
      ELSE
         CALL IRC_circem ('panic', 'version must be positive')
         RETURN
      ENDIF
      END

*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                Pseudo-CLASS  MBrA (Multi-Brancher)                       //
*//                                                                          //
*//   Purpose: Manage general multi-branched Monte Carlo algorithm           //
*//                                                                          //
*//   MBrA is capable to:                                                    //
*//    -register arbitrary number of branches                                //
*//    -keep track of elaborate MC statistics for each branch                //
*//    -generate randomly branch index                                       //
*//    -prowide useful averages and counts at the end of the run             //
*//    -print out statistics at the ned of the run                           //
*//   It is using tools from GLK library for weight monitoring               //
*//                                                                          //
*//  Xslist(i) = list of crude xsections without WTmax used in GenKF,        //
*//  Probablitity of branch(i) = XsList(i)*WTmax(i)*Normalization            //
*//                                                                          //
*//  Entries:                                                                //
*//      Global initialization:                                              //
*//  MBrA_Initialize(Nout, m_idMBrA, Nbin, WTmax, Title)                     //
*//  MBrA_AddBranch(KF,Nbin,WTmax,Title)    add branch i with name KF        //
*//      Event generation:                                                   //
*//  MBrA_SetXSList(XsList)                 set list of Xs Crude, see above  //
*//  MBrA_GenKF(KF,Wt_KF)                   generate KF                      //
*//  MBrA_GetKF(KF)                         get generated KF                 //
*//  MBrA_Fill(Wt,Rn)                       fill statistics                  //
*//      Global finalization:                                                //
*//  MBrA_MgetAve(AverWt, ErRela, WtSup)    get average total weight         //
*//  MBrA_Print0                            small final printout             //
*//  MBrA_Print1                            detailed final printout          //
*//      Tools:                                                              //
*//  MBrA_GetWMList(Ntot,WMList)            get list of WMlist(i)=WTmax(i)   //
*//  MBrA_GetKFlist(Ntot,KFlist)            get list of KF names             //
*//  MBrA_Stoper(mesage,id)                 miscelaneous                     //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
*
      SUBROUTINE MBrA_Initialize(Nout,idMBR,Nbin,WTmax,Title)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Basic initialization:                                                  //
*//   Nout  = output unit number                                             //
*//   idMBR = pointer for histograming                                       //
*//   Nbin  = number of bins for main weight                                 //
*//   WTmax = maximum weight for main weight                                 //
*//   Title = title of the branch system/main weight                         //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER idMBR,Nout,Nbin
      CHARACTER*(*) Title
      DOUBLE PRECISION  WTmax
      INCLUDE 'MBrA.h'
      SAVE
      INTEGER i
*---------------------------------
      m_out   = Nout
      m_idMBR = idMBR
      m_Ntot  = 0
      DO i=1,m_MaxBra
         m_KFlist(i) =0
         m_XSlist(i) =0d0
         m_WMList(i) =0d0
      ENDDO
      CALL GLK_Mbook(m_idMBR,Title, Nbin, WTmax)
      END

      SUBROUTINE MBrA_AddBranch(KF,Nbin,WTmax,Title)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Register one branch in the system                                      //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER KF,Nbin
      CHARACTER*(*) Title
      DOUBLE PRECISION  WTmax
      INCLUDE 'MBrA.h'
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      DO i=1,m_Ntot
         IF(KF .EQ. m_KFlist(i)) CALL MBrA_Stoper(
     $        'MBrA_AddBranch: KF code already defined KF=',KF)
      ENDDO
*
      m_Ntot=m_Ntot+1
      IF(m_Ntot .GT. m_MaxBra) CALL MBrA_Stoper(
     $     'MBrA_AddBranch: Too many branches, MaxBra= ',m_MaxBra)
*-----------------------------------------------------------------------
      m_KFlist(m_Ntot)=KF
      m_WMList( m_Ntot)=WTmax
      CALL GLK_Mbook(m_idMBR+m_Ntot,Title, Nbin, 1d0)
      END                       ! MBrA_AddBranch

      SUBROUTINE MBrA_GetKFlist(Ntot,KFlist)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER Ntot, KFlist(*)
      INCLUDE 'MBrA.h'
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      Ntot = m_Ntot
      DO i=1,m_Ntot
         KFlist(i) = m_KFlist(i)
      ENDDO
      END

      SUBROUTINE MBrA_GetWMList(Ntot,WMList)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER Ntot
      DOUBLE PRECISION   WMList(*)
      INCLUDE 'MBrA.h'
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      Ntot = m_Ntot
      DO i=1,m_Ntot
         WMList(i) = m_WMList(i)
      ENDDO
      END

      SUBROUTINE MBrA_SetXSList(XsList)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'MBrA.h'
      DOUBLE PRECISION   XsList(*)
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      DO i=1,m_Ntot
         m_Xslist(i) = XsList(i)
      ENDDO
      END


      SUBROUTINE MBrA_GenKF(KF,Wt_KF)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Generate KF (and branch ID)                                             //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'MBrA.h'
      SAVE
      INTEGER  KF,ID
      DOUBLE PRECISION    Wt_KF
      DOUBLE PRECISION    cumuKF(m_MaxBra)
      DOUBLE PRECISION    sum,rnumb
      REAL                rvec(10)
      INTEGER  i
*-----------------------------------------------------------------------
      Wt_KF = 1d0
      CALL PseuMar_MakeVec(rvec,1)
      rnumb = rvec(1)
*
      sum=0d0
      DO i=1,m_Ntot
         sum = sum +m_XsList(i)*m_WMList(i)
         cumuKF(i)= sum
      ENDDO
*
      IF(sum .EQ. 0d0 ) GOTO 900
      DO i=1,m_Ntot
         cumuKF(i)=cumuKF(i)/sum
      ENDDO
*
      DO i=1,m_Ntot
         IF(rnumb .LE. cumuKF(i)) THEN
            KF = m_KFList(i)
            ID = i
            GOTO 500
         ENDIF
      ENDDO
      CALL MBrA_Stoper('MBrA_GenKF: unable to define KF !!! ',-1)
*-----------------------------------------------------------------------
 500  CONTINUE
      Wt_KF = 1d0/m_WMList(ID)  ! compensating weight
      m_KFlast = KF             ! memorize generated KFcode
      m_IDlast = ID             ! memorize generated ID
      RETURN
*
 900  CONTINUE
      WRITE(*,*) '+++ MBrA_GenKF: STOP, sum=0'
      STOP
      END ! MBrA_GenKF

      SUBROUTINE MBrA_GetKF(KF)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER ID,KF
      INCLUDE 'MBrA.h'
      SAVE
*-----------------------------------------------------------------------
      KF = m_KFlast
      END

      SUBROUTINE MBrA_Fill(Wt,Rn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  Wt,Rn
      INCLUDE 'MBrA.h'
      SAVE
*-----------------------------------------------------------------------
      CALL GLK_Mfill(m_idMBR, Wt, Rn)
      CALL GLK_Mfill(m_idMBR +m_IDlast, Wt,   Rn)
      END

      SUBROUTINE MBrA_GetXCrude(XCrude)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   get total crude integral, for normalization purpose                    //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'MBrA.h'
      DOUBLE PRECISION   XCrude
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      XCrude=0d0
      DO i=1,m_Ntot
         XCrude= XCrude + m_Xslist(i)*m_WMlist(i)
      ENDDO
      END

      SUBROUTINE MBrA_MgetAve(AverWt, ErRela, WtSup)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      INCLUDE 'MBrA.h'
      DOUBLE PRECISION          AverWt, ErRela, WtSup
      CALL GLK_MgetAve(m_idMBR, AverWt, ErRela, WtSup)
      END

      SUBROUTINE MBrA_Print0
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BXformat.h'
      INCLUDE 'MBrA.h'
      SAVE
      DOUBLE PRECISION    AveWt, ERela, WTsup, AvUnd, AvOve
      INTEGER  Ntot,Nacc,Nneg,Nove,Nzer
      DOUBLE PRECISION    ROverf, RUnder
      INTEGER  i
*-----------------------------------------------------------------------
      CALL GLK_Mprint( m_idMBR)
      CALL GLK_MgetAll(m_idMBR,
     $     AveWt, ERela, WtSup, AvUnd, AvOve,
     $     Ntot, Nacc, Nneg, Nove, Nzer)
      
      ROverf = AvOve/AveWt
      RUnder = AvUnd/AveWt

      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) ' MBrA: report on the main Weight '
      WRITE(m_out,bxl1i) Ntot,      'no of raw events   ','Ntot  ',' b1'
      WRITE(m_out,bxl1i) Nacc,      'accepted    events ','Nacc  ',' b2'
      WRITE(m_out,bxl1i) Nneg,      'wt<0        events ','Nneg  ',' b3'
      WRITE(m_out,bxl1i) Nove,      'wt>WTmax    events ','Nove  ',' b4'
      WRITE(m_out,bxl1f) WTsup ,    'WTsup, largest WT  ','WTsup ',' b5'
      WRITE(m_out,bxl1f) AvOve ,    '<Wt-WtMax>  Overfl.','AvOve ',' b6'
      WRITE(m_out,bxl1f) AvUnd ,    '<Wt> for Wt<0      ','AvUnd ',' b7'
      WRITE(m_out,bxl1f) ROverf,    'AvOve/<Wt>,WT>WtMax','ROverf',' b8'
      WRITE(m_out,bxl1f) RUnder,    'AvUnd/<Wt>,Wt<0    ','RUnder',' b9'
      WRITE(m_out,bxclo)
      END

      SUBROUTINE MBrA_Print1
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BXformat.h'
      INCLUDE 'MBrA.h'
      SAVE
      DOUBLE PRECISION  AveWt, ERela, WTsup, AvUnd, AvOve
      INTEGER  Ntot,Nacc,Nneg,Nove,Nzer
      INTEGER  i, KF
      DOUBLE PRECISION  Overf, Under
*-----------------------------------------------------------------------
      
***      DO i=1,m_Ntot
***         CALL GLK_Mprint( m_idMBR+i)
***      ENDDO

      WRITE(m_out,'(a)') ' '
      WRITE(m_out,'(2a)') '=============',
     $ '========================================================================================'
      WRITE(m_out,'(a)') '            MBrA:    Detailed statistics for all branches    '
      WRITE(m_out,'(2a)') '=============',
     $ '========================================================================================'

      WRITE(m_out,'(a4, 2a10,a10,2a10,2a11,3a7)') 
     $     'KF',
     $     'AveWt', 'ERela', 'WtSup', 'Wt<0', 'Wt>Wmax',
     $     'Ntot', 'Nacc', 'Nneg', 'Nove', 'Nzer'
*--------- chanel by chanel
      DO i= 1,m_Ntot
         KF=m_KFList(i)
         CALL GLK_MgetAll(m_idMBR+i,
     $        AveWt, ERela, WtSup, AvUnd, AvOve,
     $        Ntot, Nacc, Nneg, Nove, Nzer)
         Under = AvUnd/AveWt
         Overf = AvOve/AveWt
         WRITE(m_out,'(I4,2f10.6,g10.4,2f10.6,2i11,3i7)')
     $        KF,
     $        AveWt, ERela, WtSup, Under, Overf,
     $        Ntot, Nacc, Nneg, Nove, Nzer
      ENDDO
*-------- all chanels
      CALL GLK_MgetAll(m_idMBR,
     $     AveWt, ERela, WtSup, AvUnd, AvOve,
     $     Ntot, Nacc, Nneg, Nove, Nzer)
      Under = AvUnd/AveWt
      Overf = AvOve/AveWt
      WRITE(m_out,'(a4,2f10.6,g10.4,2f10.6,2i11,3i7)')
     $     'All:',
     $     AveWt, ERela, WtSup, Under, Overf,
     $     Ntot, Nacc, Nneg, Nove, Nzer

      WRITE(m_out,'(2a)')  '=============',
     $ '========================================================================================'

      END



      SUBROUTINE MBrA_Stoper(mesage,id)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'MBrA.h'
      SAVE
      CHARACTER*(*) mesage
      INTEGER id
*-----------------------------
      WRITE(m_out,'(a)')'++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(m_out,'(a,a,i10,a)') '+++ ',   mesage, id,    ' +++'
      WRITE(m_out,'(a)') '+++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(6    ,'(a)') '+++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(6    ,'(a,a,i10,a)') '+++ ',   mesage, id,    ' +++'
      WRITE(6    ,'(a)') '+++++++++++++++++++++++++++++++++++++++++++++'
      STOP
      END

*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                      End of CLASS  MBrA                                  //
*//////////////////////////////////////////////////////////////////////////////
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                Pseudo-CLASS  MBrB (Multi-Brancher)                       //
*//                                                                          //
*//   Purpose: Manage general multi-branched Monte Carlo algorithm           //
*//                                                                          //
*//   MBrB is capable to:                                                    //
*//    -register arbitrary number of branches                                //
*//    -keep track of elaborate MC statistics for each branch                //
*//    -generate randomly branch index                                       //
*//    -prowide useful averages and counts at the end of the run             //
*//    -print out statistics at the ned of the run                           //
*//   It is using tools from GLK library for weight monitoring               //
*//                                                                          //
*//  Xslist(i) = list of crude xsections without WTmax used in GenKF,        //
*//  Probablitity of branch(i) = XsList(i)*WTmax(i)*Normalization            //
*//                                                                          //
*//  Entries:                                                                //
*//      Global initialization:                                              //
*//  MBrB_Initialize(Nout, m_idMBrA, Nbin, WTmax, Title)                     //
*//  MBrB_AddBranch(KF,Nbin,WTmax,Title)    add branch i with name KF        //
*//      Event generation:                                                   //
*//  MBrB_SetXSList(XsList)                 set list of Xs Crude, see above  //
*//  MBrB_GenKF(KF,Wt_KF)                   generate KF                      //
*//  MBrB_GetKF(KF)                         get generated KF                 //
*//  MBrB_Fill(Wt,Rn)                       fill statistics                  //
*//      Global finalization:                                                //
*//  MBrB_MgetAve(AverWt, ErRela, WtSup)    get average total weight         //
*//  MBrB_Print0                            small final printout             //
*//  MBrB_Print1                            detailed final printout          //
*//      Tools:                                                              //
*//  MBrB_GetWMList(Ntot,WMList)            get list of WMlist(i)=WTmax(i)   //
*//  MBrB_GetKFlist(Ntot,KFlist)            get list of KF names             //
*//  MBrB_Stoper(mesage,id)                 miscelaneous                     //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
*
      SUBROUTINE MBrB_Initialize(Nout,idMBR,Nbin,WTmax,Title)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Basic initialization:                                                  //
*//   Nout  = output unit number                                             //
*//   idMBR = pointer for histograming                                       //
*//   Nbin  = number of bins for main weight                                 //
*//   WTmax = maximum weight for main weight                                 //
*//   Title = title of the branch system/main weight                         //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER idMBR,Nout,Nbin
      CHARACTER*(*) Title
      DOUBLE PRECISION  WTmax
      INCLUDE 'MBrB.h'
      SAVE
      INTEGER i
*---------------------------------
      m_out   = Nout
      m_idMBR = idMBR
      m_Ntot  = 0
      DO i=1,m_MaxBra
         m_KFlist(i) =0
         m_XSlist(i) =0d0
         m_WMList(i) =0d0
      ENDDO
      CALL GLK_Mbook(m_idMBR,Title, Nbin, WTmax)
      END

      SUBROUTINE MBrB_AddBranch(KF,Nbin,WTmax,Title)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Register one branch in the system                                      //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER KF,Nbin
      CHARACTER*(*) Title
      DOUBLE PRECISION  WTmax
      INCLUDE 'MBrB.h'
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      DO i=1,m_Ntot
         IF(KF .EQ. m_KFlist(i)) CALL MBrB_Stoper('MBrB_AddBranch: KF code already defined KF=',KF)
      ENDDO
*
      m_Ntot=m_Ntot+1
      IF(m_Ntot .GT. m_MaxBra) CALL MBrB_Stoper(
     $     'MBrB_AddBranch: Too many branches, MaxBra= ',m_MaxBra)
*-----------------------------------------------------------------------
      m_KFlist(m_Ntot)=KF
      m_WMList( m_Ntot)=WTmax
      CALL GLK_Mbook(m_idMBR+m_Ntot,Title, Nbin, 50d0)
      END                       ! MBrB_AddBranch

      SUBROUTINE MBrB_GetKFlist(Ntot,KFlist)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER Ntot, KFlist(*)
      INCLUDE 'MBrB.h'
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      Ntot = m_Ntot
      DO i=1,m_Ntot
         KFlist(i) = m_KFlist(i)
      ENDDO
      END

      SUBROUTINE MBrB_GetWMList(Ntot,WMList)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER Ntot
      DOUBLE PRECISION   WMList(*)
      INCLUDE 'MBrB.h'
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      Ntot = m_Ntot
      DO i=1,m_Ntot
         WMList(i) = m_WMList(i)
      ENDDO
      END

      SUBROUTINE MBrB_SetXSList(XsList)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'MBrB.h'
      DOUBLE PRECISION   XsList(*)
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      DO i=1,m_Ntot
         m_Xslist(i) = XsList(i)
      ENDDO
      END


      SUBROUTINE MBrB_GenKF(KF,Wt_KF)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Generate KF (and branch ID)                                             //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'MBrB.h'
      SAVE
      INTEGER  KF,ID
      DOUBLE PRECISION    Wt_KF
      DOUBLE PRECISION    cumuKF(m_MaxBra)
      DOUBLE PRECISION    sum,rnumb
      REAL                rvec(10)
      INTEGER  i
*-----------------------------------------------------------------------
      Wt_KF = 1d0
      CALL PseuMar_MakeVec(rvec,1)
      rnumb = rvec(1)
*
      sum=0d0
      DO i=1,m_Ntot
         sum = sum +m_XsList(i)*m_WMList(i)
         cumuKF(i)= sum
      ENDDO
*
      IF(sum .EQ. 0d0 ) GOTO 900
      DO i=1,m_Ntot
         cumuKF(i)=cumuKF(i)/sum
      ENDDO
*
      DO i=1,m_Ntot
         IF(rnumb .LE. cumuKF(i)) THEN
            KF = m_KFList(i)
            ID = i
            GOTO 500
         ENDIF
      ENDDO
      CALL MBrB_Stoper('MBrA_GenKF: unable to define KF !!! ',-1)
*-----------------------------------------------------------------------
 500  CONTINUE
      Wt_KF = 1d0/m_WMList(ID)  ! compensating weight
      m_KFlast = KF             ! memorize generated KFcode
      m_IDlast = ID             ! memorize generated ID
      RETURN
*
 900  CONTINUE
      WRITE(*,*) '+++ MBrB_GenKF: STOP, sum=0'
      STOP
      END ! MBrB_GenKF

      SUBROUTINE MBrB_GetKF(KF)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER ID,KF
      INCLUDE 'MBrB.h'
      SAVE
*-----------------------------------------------------------------------
      KF = m_KFlast
      END

      SUBROUTINE MBrB_Fill(Wt,Rn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  Wt,Rn
      INCLUDE 'MBrB.h'
      SAVE
*-----------------------------------------------------------------------
      CALL GLK_Mfill(m_idMBR, Wt, Rn)
      CALL GLK_Mfill(m_idMBR +m_IDlast, Wt,   Rn)
      END

      SUBROUTINE MBrB_GetXCrude(XCrude)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   get total crude integral, for normalization purpose                    //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'MBrB.h'
      DOUBLE PRECISION   XCrude
      SAVE
      INTEGER i
*-----------------------------------------------------------------------
      XCrude=0d0
      DO i=1,m_Ntot
         XCrude= XCrude + m_Xslist(i)*m_WMlist(i)
      ENDDO
      END

      SUBROUTINE MBrB_MgetAve(AverWt, ErRela, WtSup)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      INCLUDE 'MBrB.h'
      DOUBLE PRECISION          AverWt, ErRela, WtSup
      CALL GLK_MgetAve(m_idMBR, AverWt, ErRela, WtSup)
      END

      SUBROUTINE MBrB_Print0
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BXformat.h'
      INCLUDE 'MBrB.h'
      SAVE
      DOUBLE PRECISION    AveWt, ERela, WTsup, AvUnd, AvOve
      INTEGER  Ntot,Nacc,Nneg,Nove,Nzer
      DOUBLE PRECISION    ROverf, RUnder
      INTEGER  i
*-----------------------------------------------------------------------
      CALL GLK_Mprint( m_idMBR)
      CALL GLK_MgetAll(m_idMBR,
     $     AveWt, ERela, WtSup, AvUnd, AvOve,
     $     Ntot, Nacc, Nneg, Nove, Nzer)
      
      ROverf = AvOve/AveWt
      RUnder = AvUnd/AveWt

      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) ' MBrB: report on the main Weight '
      WRITE(m_out,bxl1i) Ntot,      'no of raw events   ','Ntot  ',' b1'
      WRITE(m_out,bxl1i) Nacc,      'accepted    events ','Nacc  ',' b2'
      WRITE(m_out,bxl1i) Nneg,      'wt<0        events ','Nneg  ',' b3'
      WRITE(m_out,bxl1i) Nove,      'wt>WTmax    events ','Nove  ',' b4'
      WRITE(m_out,bxl1f) WTsup ,    'WTsup, largest WT  ','WTsup ',' b5'
      WRITE(m_out,bxl1f) AvOve ,    '<Wt-WtMax>  Overfl.','AvOve ',' b6'
      WRITE(m_out,bxl1f) AvUnd ,    '<Wt> for Wt<0      ','AvUnd ',' b7'
      WRITE(m_out,bxl1f) ROverf,    'AvOve/<Wt>,WT>WtMax','ROverf',' b8'
      WRITE(m_out,bxl1f) RUnder,    'AvUnd/<Wt>,Wt<0    ','RUnder',' b9'
      WRITE(m_out,bxclo)
      END

      SUBROUTINE MBrB_Print1
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BXformat.h'
      INCLUDE 'MBrB.h'
      SAVE
      DOUBLE PRECISION  AveWt, ERela, WTsup, AvUnd, AvOve
      INTEGER  Ntot,Nacc,Nneg,Nove,Nzer
      INTEGER  i, KF
      DOUBLE PRECISION  Overf, Under
*-----------------------------------------------------------------------
      
***      DO i=1,m_Ntot
***         CALL GLK_Mprint( m_idMBR+i)
***      ENDDO

      WRITE(m_out,'(a)') ' '
      WRITE(m_out,'(2a)') '=============',
     $ '========================================================================================'
      WRITE(m_out,'(a)') '            MBrB:    Detailed statistics for all branches    '
      WRITE(m_out,'(2a)') '=============',
     $ '========================================================================================'

      WRITE(m_out,'(a4, 2a10,a10,2a10,2a11,3a7)') 
     $     'KF',
     $     'AveWt', 'ERela', 'WtSup', 'Wt<0', 'Wt>Wmax',
     $     'Ntot', 'Nacc', 'Nneg', 'Nove', 'Nzer'
*--------- chanel by chanel
      DO i= 1,m_Ntot
         KF=m_KFList(i)
         CALL GLK_MgetAll(m_idMBR+i,
     $        AveWt, ERela, WtSup, AvUnd, AvOve,
     $        Ntot, Nacc, Nneg, Nove, Nzer)
         Under = AvUnd/AveWt
         Overf = AvOve/AveWt
         WRITE(m_out,'(I4,2f10.6,g10.4,2f10.6,2i11,3i7)')
     $        KF,
     $        AveWt, ERela, WtSup, Under, Overf,
     $        Ntot, Nacc, Nneg, Nove, Nzer
      ENDDO
*-------- all chanels
      CALL GLK_MgetAll(m_idMBR,
     $     AveWt, ERela, WtSup, AvUnd, AvOve,
     $     Ntot, Nacc, Nneg, Nove, Nzer)
      Under = AvUnd/AveWt
      Overf = AvOve/AveWt
      WRITE(m_out,'(a4,2f10.6,g10.4,2f10.6,2i11,3i7)')
     $     'All:',
     $     AveWt, ERela, WtSup, Under, Overf,
     $     Ntot, Nacc, Nneg, Nove, Nzer

      WRITE(m_out,'(2a)')  '=============',
     $ '========================================================================================'

      DO i= 1,m_Ntot
***      CALL GLK_Print(   -(m_idMBR+i))
         CALL MBrB_WtLimit(-(m_idMBR+i))
      ENDDO

      END

      SUBROUTINE MBrB_Stoper(mesage,id)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'MBrB.h'
      SAVE
      CHARACTER*(*) mesage
      INTEGER id
*-----------------------------
      WRITE(m_out,'(a)')'++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(m_out,'(a,a,i10,a)') '+++ ',   mesage, id,    ' +++'
      WRITE(m_out,'(a)') '+++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(6    ,'(a)') '+++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(6    ,'(a,a,i10,a)') '+++ ',   mesage, id,    ' +++'
      WRITE(6    ,'(a)') '+++++++++++++++++++++++++++++++++++++++++++++'
      STOP
      END

      SUBROUTINE MBrB_WtLimit(id)
*//////////////////////////////////////////////////////////////////////////////////
*// calculates wtmax for which overflow integral is below epsilon
*// the precision of the result is limited by beam size and statistics
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'MBrB.h'
      CHARACTER*80 title
      INTEGER           id,nchx
      DOUBLE PRECISION  xl,xu, Bin
      DOUBLE PRECISION  GLK_hi,sum,sumWt,WtLimit,tail,eps,AveWt,Wt
      INTEGER           ib,ibX
*     ------------------------------------------
      CALL GLK_hinbo1(id,title,nchx,xl,xu)
      eps=1d-4
      sum   = 0d0
      sumWt = 0d0
      DO ib=1,nchx
         Bin = GLK_hi(id,ib)
         Wt = xl+(ib-0.5d0)*(xu-xl)/nchx
         sum   = sum   +Bin
         sumWt = sumWt +Bin*Wt
      ENDDO
      AveWt = sumWt/sum

      DO ibX=nchx,1,-1
         WtLimit =xl+(ibX-0.5d0)*(xu-xl)/nchx
         tail=0d0
         DO ib=ibX,nchx
            Bin = GLK_hi(id,ib)
            Wt = xl+(ib-0.5d0)*(xu-xl)/nchx
            tail=tail+Bin*(Wt-WtLimit) ! deficit of xsection because Wt->WtLimit
         ENDDO
         IF(tail/sum .GT. eps) GOTO 100
      ENDDO
 100  CONTINUE
      WRITE(m_out,*) '-------------------------------------------------------------------------------'
      WRITE(m_out,*) '-->FindWtLimit: content, <Wt>,  WtLimit= ',sum,AveWt,WtLimit
      WRITE(m_out,*) '-->FindWtLimit: EFFICIENCY <Wt>/WtLimit= ',AveWt/WtLimit
      WRITE(    6,*) '-------------------------------------------------------------------------------'
      WRITE(    6,*) '-->FindWtLimit: content, <Wt>,  WtLimit= ',sum,AveWt,WtLimit
      WRITE(    6,*) '-->FindWtLimit: EFFICIENCY <Wt>/WtLimit= ',AveWt/WtLimit
      END                       !!!WtLimit

*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                      End of CLASS  MBrB                                  //
*//////////////////////////////////////////////////////////////////////////////
*///////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                       //
*//             Foam Version 2.02                                                         //
*//             November 2000                                                             //
*//                                                                                       //
*//  N-dimensional general purpose Monte Carlo sampler                                    //
*//  with the self-adapting simplical and hyper-cubical grid                              //
*//                                                                                       //
*//             Author:   Stanislaw JADACH                                                //
*//             Address:  INP Cracow, DESY-Zeuthen                                        //
*//             Email:    S.Jadach@cern.ch, S.Jadach@ifj.edu.pl                           //
*//             HomePage: http://home.cern.ch/jadach/                                     //
*//                                                                                       //
*//  First version 1.00 written by S.J. in May 1999 during visit in DESY                  //
*//        version 2.00 written by S.J. in Aug 2000 during visit in DESY-Zeuthen          //
*///////////////////////////////////////////////////////////////////////////////////////////


*////////////////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                                    //
*//          Pseudoclass Foam                                                                          //
*//                                                                                                    //
*//                                                                                                    //
*//                      Initialization of the grid                                                    //
*//  FoamA_PreInitialize                 : Pre-initialization, set all default values (constructor?)   //
*//  FoamA_Initialize(FunW)              : Initialization of the grid etc.                             //
*//  FoamA_InitVertices                  : Initializes first vertices of the basic cube                //
*//  FoamA_InitCells                     : Initializes first n-factorial cells inside original cube    //
*//  FoamA_DefCell                       : Create new (daughter) cell and append at end of the buffer  //
*//  FoamA_Explore(iCell,funW)           : Short MC sampling in iCell, determine <wt>, wtMax etc.      //
*//  FoamA_RanDiscr(Driv,nTot,Power,iRand) : Random choice of cell division direction                  //
*//  FoamA_MakeAlpha(Alpha)              : auxiliary procedure for FoamA_Explore                       //
*//  FoamA_MakeLambda(Lambda)            : auxiliary procedure for FoamA_Explore                       //
*//  FoamA_Determinant(R,Det)            : determinant of matrix R                                     //
*//  FoamA_Det2Lapl(R,i1,i2)             : Laplace formula for 1-dim. determinant                      //
*//  FoamA_Det3Lapl(R,i1,i2,i3)          : Laplace formula for 2-dim. determinant                      //
*//  FoamA_Det4Lapl(R,i1,i2,i3,i4)       : Laplace formula for 3-dim. determinant                      //
*//  FoamA_Det5Lapl(R,i1,i2,i3,i4,i5)    : Laplace formula for 4-dim. determinant                      //
*//  FoamA_Grow(funW)              : grow cells until buffer is full                                   //
*//  FoamA_PeekMax(iCell)          : choose randomly one cell, used also in MC generation              //
*//  FoamA_PeekRand1(iCell)        : Generates randomly one (active) cell pointer iCell                //
*//  FoamA_Divide(iCell,funW,RC)   :Divide iCell into two daughters; iCell retained, taged as inactive //
*//  FoamA_ActUpda                 : Creates list of active cells (pointers)                           //
*//                     Generation                                                                     //
*//  FoamA_MakeEvent(Density)      : Generates point/vector Xrand with the weight MCwt                 //
*//  FoamA_CellGener(Density)      : Cooses randomly one of active cells                               //
*//  FoamA_GetMCvector(MCvector)   : Provides point/vector MCvector generated by  MakeEvent            //
*//  FoamA_GetMCwt(MCwt)           : Provides MCwt, MC weight calculated by MakeEvent                  //
*//  FoamA_MCgenerate(funW,X,MCwt) : Alternative entry, Generates point X with the weight MCwt         //
*//                     Finalization                                                                   //
*//  FoamA_Finalize(MCresult,MCerror)    : Calculates integral and its error after (only from) MC run  //
*//                     Other Getters and Setters                                                      //
*//  MCellA_GetTotPrim(TotPrim)    :Provides Primary integral used in MC generation                    //
*//  FoamA_SetnDim(nDim)           :Sets no. of dimensions simplices   (to be called before Initialize)//
*//  FoamA_SetkDim(kDim)           :Sets no. of dimensions hypercubics (to be called before Initialize)//
*//  FoamA_GetnDim(nDim)           :Provides nDim, miscelaneous, for tests                             //
*//  FoamA_SetnBuf(nBuf)           :Sets nBuf, working area in buffer                                  //
*//  FoamA_SetnBin(nBin)           :Sets nBin, no of bins in histo in exploration <nBinMax=256         //
*//  FoamA_SetOut(Out)             :Sets output unit number                                            //
*//  FoamA_SetChat(Chat)           :Sets chat level; Chat=0,1,2 chat level in output, Chat=1 normal    //
*//  FoamA_SetnSampl(nSampl)       :Sets nSampl; No of MC sampling before dividing cell                //
*//  FoamA_SetOptDrive(OptDrive)   :Sets OptDrive; type of Drive =0,1,2 for True,Sigma,WtMax           //
*//  FoamA_SetOptPeek              :Sets type of method in cell division                               //
*//  FoamA_SetOptEdge(OptEdge)     :Sets OptEdge; decides whether vertices are included in the sampling//
*//  FoamA_SetOptRanIni(OptRanIni) :Sets OptRanIni=0,1, for rand. numb. initialization inside/outside  //
*//  FoamA_SetOptRanLux(OptRanLux) :Sets OptRanLux=-1,0,1,2,3 raand.numb.gen. level                    //
*//  FoamA_GetnCalls(nCalls)       :Get total no of function calls                                     //
*//                    Debugging and miscelaneous                                                      //
*//  FoamA_Check(mout,level)       :Checks all pointers (after comression) debuging!                   //
*//  FoamA_BufPrint(mout)          :Prints all cells, debugging                                        //
*//  FoamA_BufActPrint(mout)       :Prints all active cells, debugging                                 //
*//  FoamA_VertPrint(mout)         :Prints all vertices,  debugging                                    //
*//  FoamA_PltBegin                :Ploting 2-dim. cells and vertices                                  //
*//  FoamA_PltVert(mout)           :Ploting 2-dim. cells and vertices                                  //
*//  FoamA_PltCell(mout)           :Ploting 2-dim. cells and vertices                                  //
*//  FoamA_PltEnd                  :Ploting 2-dim. cells and vertices                                  //
*//                                                                                                    //
*////////////////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                                    //
*//  Input parameters:                                                                                 //
*//    nDim     number of dimensions for simplices, for the moment nDim=1-5 in this version,           //
*//             n>5 requires re-writing FoamA_Determinant,                                             //
*//    kDim     number of dimensions for hypercubics                                                   //
*//    Dimen    total number of dimensions = nDim +kDim                                                //
*//             for n=1 alternatively Foam1A may be used, could be factor 2 faster!                    //
*//    nBuf     Actual dynamic lenth of the buffer m_nBuf<m_nBufMax. For strongly peaked distribution  //
*//             nBuf should be as large as possible, this will increase CPU time in initialization     //
*//             MC generation is weakly affected by increasing nBuf                                    //
*//    nSampl   No of sampling when dividing cell, nSampl=10-100 is OK, further increase improves      //
*//             costs CPU time and apparently does not increase grid efficiency too much.              //
*//             This should be checked however for every new distribution.                             //
*//    OptDrive Type of Drive =0,1,2 for TrueInt,Sigma,WtMax,  Drive=WtMax is the best if we aim       //
*//             at rejection leading to wt=1 events. If not then Drive=Sigma iswiser choice leading    //
*//             to save of CPU time.  Simplistic Drive=Sigma is correct but not recommeneded (ineffic.)//
*//    OptEdge  decides whether vertices are included in the sampling. Default  OptEdge=1 causes that  //
*//             vertices at the edge of simplex cells are included always in MC exploration            //
*//             of the cell. In the case of density distrib. with weak integrable singularities        //
*//             at the edges it may be not possible and OptEdge=0 may help.                            //
*//    OptOrd decides whether the integration domain in simplical subspace is unit cube or simplex.    //
*//             It is active only for  nDim=2,3,4. For OptOrd=0 (default) the initial domain is unit   //
*//             hypercubic split into nDim! simplices in the initialization phase.                     //
*//             If m_OptOrd=1 then initial domain is simplex and the integration variables providede   //
*//             by FoamA_GetMCvector(V) are ordered: V(1)<V(2)<...<V(nDim)<1.                          //
*//             Hyp-cubical variables  0<V(nDim+1), V(nDim+2),...,V(nDim+kDim)<1 are never ordered.    //
*//    EvPerBin Limit on effective number of eevents per bin in exploration phase. This option saves   //
*//             considerably CPU time.      =0 option is desabled, recommended value is >20.           //
*//    Out      Miscelaneous. Output unit number.                                                      //
*//    Chat     Miscelaneous. Chat=0,1,2 chat level in output, Chat=1 normal level.                    //
*//                                                                                                    //
*//                                                                                                    //
*////////////////////////////////////////////////////////////////////////////////////////////////////////
*//  Terminology:                                                                                      //
*//    "Active cells" are these which did not divide and are eligible for division                     //
*//  Remarks:                                                                                          //
*//    List of active cells is not realy necessary, but let us keep it for possible                    //
*//    future developements or tests.                                                                  //
*////////////////////////////////////////////////////////////////////////////////////////////////////////


      SUBROUTINE FoamA_PreInitialize     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Presets certain defaults for switches and other and regualtory parameters.     //
*//   They Can be reset with setters                                                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER i,j
*
      INTEGER   m_Magic        ! Magic cookie, to avoid multiple initialization
      DATA      m_Magic /378231178/
      SAVE      m_Magic
*     -------------------------------------------------
      IF(m_Magic .EQ. 378231178 ) THEN
         m_MagicInit = m_Magic
         m_Magic     = 0
      ENDIF
      IF(m_MagicInit .NE. 378231178 ) RETURN
      WRITE(m_out,*) '===============FoamA_PreInitialize=============='
      m_MagicInit= 0
      m_nDim     = 0                 ! dimension siplices
      m_kDim     = 0                 ! dimension hypercubics
      m_Dimen    = m_nDim+m_kDim     ! dimension total
      m_nBuf     = 1000              ! Actual dynamic lenth of the buffer m_nBuf<m_nBufMax
      m_nBin     = 8                 ! No of bins in cell exploration/division  <nBinMax  
      m_nSampl   = 200               ! No of sampling when dividing cell
      m_OptDrive = 2                 ! type of Drive =0,1,2 for TrueVol,Sigma,WtMax
      m_OptEdge  = 0                 ! decides whether vertices are included in the sampling
      m_OptPeek  = 0                 ! type of Peek =0,1 for maximum, random
      m_Out      = 6                 ! Output unit
      m_Chat     = 1                 ! Chat=0,1,2 chat level in output, Chat=1 normal level
      m_OptOrd   = 0                 ! Entire integr domain is hyp-cubic if =0 and simplex if =1
      m_EvPerBin = 25                ! Upper limit on eevents/bin in exploration, =0 disabled 
      m_OptRanIni= 1                 ! Rand.num. generator initialized (=1) inside PianA
      m_OptRanLux= 3                 ! Rand.num. generator level (-1 for PseuMar)
*
      m_LastCe   = 0                 ! length of dynamical buffor for cells
      m_LastVe   = 0                 ! length of dynamical buffor for vertices
* Clean up list of simplical vertices
      DO i=1,m_vMax
         DO j=1,m_nDim
            m_VerX(i,j) = 0d0        ! vertices
         ENDDO
      ENDDO
* Clean up hypercubic parameters
      DO i=1,m_nBufMax
         DO j=1,m_KdiMax
            m_CeVer1(i,j)=0d0        ! hcubic position
            m_CeVer2(i,j)=0d0        ! hcubic size
         ENDDO
      ENDDO
*
      m_nCalls   = 0                 ! No of function calls
*
      END

      SUBROUTINE FoamA_Flush        !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////*//                                                                                  //
*//   Use this before re-initialization of the Foam                                  //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      m_MagicInit = 378231178   ! Reset Magic cookie, alowing initialization
      WRITE(m_out,*) '===============FoamA_Flush=============='
      CALL FoamA_PreInitialize
      END

      SUBROUTINE FoamA_Initialize(FunW)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Basic initialization, create "foam of cells"                                   //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            i,j,k,iCell,kCell
      DOUBLE PRECISION   TotPrim
*---------------------------------------------
      CALL FoamA_PreInitialize
* User may prefers to initialize r.n.gen. by himsef, the initialization below can be inhibited 
      IF( m_OptRanIni.EQ. 1 ) THEN
         IF(m_OptRanLux.EQ.-1) THEN
            CALL PseuMar_Initialize(54217137, 0, 0) ! Initialization of random number generator
         ELSEIF(m_OptRanLux.GE.0 .AND. m_OptRanLux.LE.4 ) THEN
            CALL RLUXGO( m_OptRanLux, 54217137,0,0) ! Initialization of random number generator
         ELSE
            WRITE(*,    *) ' ### STOP in FoamA_Initialize, wrong OptRanLux =',m_OptRanLux !
            WRITE(m_Out,*) ' ### STOP in FoamA_Initialize, wrong OptRanLux =',m_OptRanLux !
         ENDIF
      ENDIF
      IF( (m_nBuf .GT. m_nBufMax) .OR. (m_nBuf .LE. 0) ) THEN
         WRITE(*,    *) ' ### STOP in FoamA_Initialize, wrong m_nBuf =',m_nBuf !
         WRITE(m_Out,*) ' ### STOP in FoamA_Initialize, wrong m_nBuf =',m_nBuf !
         STOP
      ENDIF
      IF( (m_nBin .GT. m_nBinMax) .OR. (m_nBin .LE. 0) ) THEN
         WRITE(*,    *) ' ### STOP in PianA_Initialize, wrong m_nBin =',m_nBin !
         WRITE(m_Out,*) ' ### STOP in PianA_Initialize, wrong m_nBin =',m_nBin !
         STOP
      ENDIF
      IF( m_Dimen .LE. 0 ) THEN
         WRITE(*,    *) ' ### STOP in FoamA_Initialize, wrong m_Dimen =',m_Dimen !
         WRITE(m_Out,*) ' ### STOP in FoamA_Initialize, wrong m_Dimen =',m_Dimen !
         STOP
      ENDIF
* First  cells are the (nDim)! simplices from division of the basic unit cube
      CALL FoamA_InitVertices
      CALL FoamA_InitCells(funW)

***** CALL FoamA_VertPrint(6)   ! debug
***** CALL FoamA_BufPrint(6)    ! debug

      CALL  FoamA_Grow(funW)
* Update list of active cells, only for internal tests
      CALL FoamA_ActUpda
      CALL FoamA_Check(6,0)     ! Check if the liked list is OK
* --------------------------------------------------------------------------------------------
      IF( m_Chat.GE.1) THEN
         WRITE(m_Out,'( 3(a,i4),2(a,g15.8) )') 
     $      'Foam_Initialize:  No. Cells=',m_LastCe,'  Active=',m_LastAc, '  No. Vertices=' ,m_LastVe, !
     $      '  True Integ.=',m_CeIntg(1),'  Driver Integ.=',m_CeDriv(1)  !
      ENDIF
      IF( m_Chat.EQ.2) THEN
         CALL FoamA_BufPrint(    m_Out)
         CALL FoamA_BufActPrint( m_Out)
         CALL FoamA_VertPrint(   m_Out)
      ENDIF
* Initializations for M.C. generation
      m_Drive  = m_CeDriv(1)  ! M.C. generation Drive value of integral
      m_SumWt  = 0d0          ! M.C. generation sum of Wt
      m_SumWt2 = 0d0          ! M.C. generation sum of Wt**2
      m_NevGen  = 0d0         ! M.C. generation sum of 1d0
      m_WtMax  = -1d99        ! M.C. generation maximum wt
      m_WtMin  =  1d99        ! M.C. generation minimum wt
      m_VolTot = m_CeIntg(1)  ! Estimate of integral tot. without error
      m_MCresult = m_VolTot   ! M.C. generation Final value of ITEGRAL, temporary asignment
      m_MCerror  = m_VolTot   ! M.C. generation Final walue of ERROR  , temporary asignment
*
*((((((((((((################################################################################
*((((((((((((################################################################################
c      CALL FoamA_VertPrint(  6)
c      CALL FoamA_BufPrint(   6)
c      CALL FoamA_BufActPrint(6)
c      CALL FoamA_Check(      6,0)
c      WRITE(*,*) ' ########### STOP ########### developement not finished @@@@' !
c      STOP
*))))))))))))################################################################################
*))))))))))))################################################################################
*
* Cumulative Primary for MC generation, see FoamA_CellGener
      CALL FoamA_ActUpda
      TotPrim = 0d0
      m_CePrCu(0)= 0d0
      DO kCell = 1,m_LastAc
         iCell  = m_ActC(kCell)
         TotPrim = TotPrim +m_CePrim( iCell )
         m_CePrCu(kCell)=TotPrim
      ENDDO
* ----------------------------------------------------
      IF( m_Chat.GE.1) WRITE(m_Out,'( 2(a,g15.8) )')
     $     'Foam_Initialize:  TotPrimary =',TotPrim,'   True/Primary =', m_CeIntg(1)/TotPrim
      END                       ! FoamA_Initialize

      SUBROUTINE FoamA_InitVertices    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   In siplical subspace initialize 2^nDim vertices at corners of basic hyper-cube //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER       iVe,i,j,l,digit( m_NdiMax)
*
      IF( m_OptOrd .EQ.0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*// Here we have all vertices of the initial unit hypercube
*// like (0000),(1000),(0100),(1100),(0010)...(1111)
         m_LastVe = 2**m_nDim
         DO i=1,m_nDim
            digit(i)=0
         ENDDO
         DO iVe=1,m_LastVe
*****    WRITE(*,'(a,i5,a,10i5)') 'iVe=', iVe, '  digit=',(digit(j),j=1,m_nDim)
            DO i=1,m_nDim
               m_VerX(iVe,i) = digit(i) ! Simplical vertex positions
            ENDDO
            digit(1)=digit(1)+1         ! Basic increment
            DO i=1,m_nDim-1
               IF(digit(i).EQ.2) THEN   ! Overflow goes to higher digits
                  digit(i)=0
                  digit(i+1)=digit(i+1)+1
               ENDIF
            ENDDO
         ENDDO
      ELSE
*//////////////////////////////////////////////////////////////////////////////////////
*// Here we have only vertices of the SINGLE  INITIAL SIMPLEX 
*// like (0000),(1000),(1110),(1111)
         m_LastVe = m_nDim+1
         DO iVe=1,m_LastVe
            DO i=1,m_nDim
               m_VerX(iVe,i) =0
               IF(i.LT.iVe) m_VerX(iVe,i) =1  ! Simplical vertex positions
            ENDDO
*****       WRITE(*,'(a,i5,a,10f5.0)') 'iVe=', iVe, '  m_VerX=',(m_VerX(iVe,i),i=1,m_nDim)!
         ENDDO
      ENDIF
      END                       !! FoamA_InitVertices

      SUBROUTINE FoamA_InitCells(funW)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Initialize first n-factorial cells inside original cube                        //
*//   MC exploration done for all newly defined cells                                //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            perm(m_NdiMax),mask,iCeNew,iCell
      INTEGER            iCe,i,j,k,iVe,Vert(m_NdiMax),digit(m_NdiMax),factorial,NoMC !
      DOUBLE PRECISION   HcPosi(m_KdiMax),   HcSize(m_KdiMax)
*     -----------------------------------------------------------------------------
      DO j=1,m_kDim
         HcPosi(j)=0d0
         HcSize(j)=1d0
      ENDDO
      DO iVe=1,m_nDim+1
         Vert(iVe) =0
      ENDDO
      IF( m_nDim .EQ. 0 ) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//      Only hypercubics, no simplices                                              //
*//////////////////////////////////////////////////////////////////////////////////////
*        ROOT cell ACTIVE, no daughters
         NoMC= m_nSampl
*        -------------------- Stat,Pare, Dau1, Dau2, MCsampl,VertList, Position,Size,   iCeNew)
         CALL FoamA_DefCell(     1,  -1,   -1,   -1,    NoMC,   Vert, HcPosi,  HcSize, iCeNew) !
         CALL FoamA_Explore(iCeNew,funW) ! Initial MC sampling
      ELSEIF( m_nDim .EQ. 1 ) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//      Simplical algorithm in one dimension                                        //
*//////////////////////////////////////////////////////////////////////////////////////
         Vert(1) =1
         Vert(2) =2
*        ROOT cell ACTIVE, no daughters
*        -------------------- Stat,Pare, Dau1, Dau2,  MCsampl,VertList, Position,Size,   iCeNew
         CALL FoamA_DefCell(     1,  -1,   -1,   -1, m_nSampl,    Vert, HcPosi,  HcSize, iCeNew) !
         CALL FoamA_Explore(iCeNew,funW) ! Initial MC sampling
      ELSE
         IF( m_OptOrd .EQ.0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//      (nDim)! Simplices in more two and more dimensions                           //
*//////////////////////////////////////////////////////////////////////////////////////
            factorial=1
            DO k = 1,m_nDim
               factorial = factorial*k
            ENDDO
*           ROOT cell INACTIVE, (nDim)! daugter cells defined below
*           ----------------- Stat,Pare, Dau1,        Dau2,  MCsampl, VertList, Position,Size,   iCeNew)
            CALL FoamA_DefCell( -1,  -1,    2, factorial+1, m_nSampl,     Vert, HcPosi,  HcSize, iCeNew) !
* ================================================================
* START OF LOOP OVER permutations of (1,2,3,...,m_nDim)
            iCe=0
            DO i=1,m_nDim
               perm(i)=m_nDim
            ENDDO
 300        CONTINUE
            perm(1)=perm(1)-1
            DO k = 1,m_nDim-1
               IF(perm(k).EQ.0) THEN
                  perm(k)=m_nDim
                  perm(k+1)=perm(k+1)-1
               ENDIF
            ENDDO
            Mask=1
            DO i=1,m_nDim
               DO j=i+1,m_nDim
                  IF( perm(i).EQ.perm(j) ) Mask=0
               ENDDO
            ENDDO
            IF(Mask.EQ.1) THEN
*           At this point NEW PERMUTATION is found !!!
               iCe=iCe+1
               DO iVe=1,m_nDim+1
*              This digit represents single BASIC SIMPLEX, other obtained by permuting dimensions
                  DO k=1,m_nDim
                     digit(k)=0
                     IF(k.LT.iVe) digit(k)=1
                  ENDDO
*                 Translation from "binary" digit to serial pointer of a given vertex
                  j=0
                  DO k=1,m_nDim
                     j=j+  digit(perm(k)) *2**(k-1)
                  ENDDO
                  Vert(iVe)=j+1
               ENDDO
*****    WRITE(*,*) '########>>>>>>> iCe =',iCe, 'permut= ',(perm(i),i=1,m_nDim)
*****    WRITE(*,*) '########>>>>>>> Vert= ',(vert(i),i=1,m_nDim+1)
*              Define ACTIVE simplical cell
*              ----------------- Stat,Pare,Dau1,Dau2,  MCsampl,VertList, Position,Size,   iCeNew
               CALL FoamA_DefCell(  1,   1,  -1,  -1, m_nSampl,    Vert, HcPosi,  HcSize, iCeNew) !
*              ---------------------------------------------------------------------
               IF( iCe.EQ.factorial) GOTO 100 ! Last permutation found
            ENDIF
            GOTO 300            ! END OF LOOP over permutations
* ================================================================
 100        CONTINUE
            DO iCell = 2,m_LastCe
               CALL FoamA_Explore(iCell,funW) ! Initial MC sampling, necessarily in a separate loop
            ENDDO
         ELSE
*//////////////////////////////////////////////////////////////////////////////////////
*//      Single initial simplex in two or more dimension (ordered integr. variables) //
*//////////////////////////////////////////////////////////////////////////////////////
            DO k = 1,m_nDim+1
               Vert(k) =k
            ENDDO
*           ROOT cell ACTIVE, no daughters
*           -------------------- Stat,Pare, Dau1, Dau2,  MCsampl, VertList, Position,Size,    iCeNew)
            CALL FoamA_DefCell(     1,  -1,   -1,   -1, m_nSampl,     Vert, HcPosi,  HcSize,  iCeNew) !
            CALL FoamA_Explore(iCeNew,funW) ! Initial MC sampling
         ENDIF  ! m_OptOrd
      ENDIF  ! m_nDim
      END                       !!!FoamA_InitCells

      SUBROUTINE FoamA_DefCell(Stat,Pare,Dau1,Dau2,NoMC,Vertex,HcPosi,HcSize,iCeNew)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Create new (daughter) cell and append it at the very end of the buffer         //
*//   iCeNew is pointer of the new cell                                              //
*//   Note clever trick: volume of this daughter is assigned initialy half volume    //
*//   of the parent, if parent exists.                                               //
*//   In Explore this value is used to update all parents such that                  //
*//   in the entrire tree parents have volume being sum of all daughter volumes.     //
*//   This summation discipline is useful for MC generation of an active cell by     //
*//   going randomly from top to bottom of the tree.                                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            Stat,Pare,Dau1,Dau2,iCeNew,k,NoMC
      INTEGER            Vertex(m_NdiMax+1)
      DOUBLE PRECISION   HcPosi(m_KdiMax), HcSize(m_KdiMax)
*     ------------------------------------------------------------------
      IF( m_LastCe .EQ. m_nBuf) THEN
         WRITE(*,*) ' STOP in FoamA_DefCell: something wrong with m_nBuf=',m_nBuf !
         STOP
      ENDIF
      m_LastCe = m_LastCe+1
      iCeNew   = m_LastCe
      m_CeStat(iCeNew)= Stat                    ! status code, =0 inactive, =1 active
      m_CePare(iCeNew)= Pare                    ! parent cell pointer
      m_CeDau1(iCeNew)= Dau1                    ! daughter1 cell pointer
      m_CeDau2(iCeNew)= Dau2                    ! daughter2 cell pointer
      m_CeSamp(iCeNew)= NoMC                    ! No of MC events in exploration
      m_CeBest(iCeNew)= -1                      ! pointer for planning division of the cell
      m_CeXave(iCeNew)= 0.5d0                   ! factor for division
*  simplical subspace: vertex list
      DO k=1,m_NdiMax+1
         m_CeVert(iCeNew,k)= Vertex(k)
      ENDDO
*  hypercubical subspace: position and size
      DO k=1,m_kDim
         m_CeVer1(iCeNew,k) = HcPosi(k)
         m_CeVer2(iCeNew,k) = HcSize(k)
      ENDDO
*
      IF(Pare.NE.-1) THEN
         m_CeIntg(iCeNew)= m_CeIntg(Pare)/2d0   ! integr. half of parent
         m_CeDriv(iCeNew)= m_CeDriv(Pare)/2d0   ! integr. half of parent
      ELSE
         m_CeIntg(iCeNew)= 0d0
         m_CeDriv(iCeNew)= 0d0
      ENDIF
      m_CeVolu(iCeNew)= 0d0                     ! cartesian Volume   
      END                       ! FoamA_DefCell


      SUBROUTINE FoamA_Explore(iCell,funW)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Explore newly defined cell with help of special short MC sampling              //
*//   As a result, estimetes of true and Drive volume will be defined                //
*//   Average and dispersion of the weight distribution will be found along each     //
*//   edge and the best edge (minimum dispersion) is memorized for future use.       //
*//   Axerage x for eventual future cell division is also defined.                   //
*//   Recorded are aso minimum and maximu weight etc.                                //
*//   The volume estimate in all (inactive) parent cells is updated                  //
*//   Note that links to parents and initial volume = 1/2 parent has to be           //
*//   already defined prior to calling this routine.                                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            iCell
* Simplical subspace
      DOUBLE PRECISION   Vec(m_NdiMax+1,m_NdiMax)
      DOUBLE PRECISION   Xre(m_NdiMax,  m_NdiMax), Yre(m_NdiMax,  m_NdiMax) !
      DOUBLE PRECISION   Lambda(m_NdiMax),VolPart(m_NdiMax+1)
* Hyper-Cubic subspace
      DOUBLE PRECISION   Alpha( m_KdiMax)
      INTEGER            digit( m_KdiMax)
* Total space
      DOUBLE PRECISION   Vrand( m_DimMax)
* Edge sampling working space, matrices
      DOUBLE PRECISION   Xdivi(m_NdiviMax)
      DOUBLE PRECISION   Histo(m_NdiviMax,m_nBinMax),Xsu1(m_NdiviMax),Xsu2(m_NdiviMax) !
*--------
      INTEGER            loop,i,j,k,parent,iv,jv,kv,nEdges,kBest,iBin,nDivi,NevEff !
      DOUBLE PRECISION   x,x1,x2,Wt,Vsum,xBest,yBest
      DOUBLE PRECISION   Det,VolOld, DriOld, XrSum,Factorial
      DOUBLE PRECISION   Dx,DxHc,DxSp,DxPartial,NoMC
      DOUBLE PRECISION   funW
      EXTERNAL           funW
*-----------------------------------------------------------------------
* memorize old values, will be needed for correcting parent cells
      VolOld = m_CeIntg(iCell)
      DriOld = m_CeDriv(iCell)
*
      Factorial=1
      DO i=1,m_nDim
         Factorial=Factorial*i
      ENDDO
      DxSp=1d0          ! Cartesian volume of the simplex
      DxHc=1d0          ! Cartesian volume of the hypercube
*
      IF( m_nDim.GT.0) THEN
         DO iv=1,m_nDim+1
            DO j=1,m_nDim
               Vec(iv,j) = m_VerX( m_CeVert(iCell,iv) ,j) ! decode vertex vectors
            ENDDO
         ENDDO
         DO iv=1,m_nDim
            DO j=1,m_nDim
               Xre(iv,j) = Vec(iv,j)-Vec(m_nDim+1,j) ! relative to last vertex
            ENDDO
         ENDDO
         CALL FoamA_Determinant(Xre,Det)
         IF(Det.EQ.0d0) WRITE(*,*) '!!!!! WARNING: Determinant=',Det
         DxSp = DxSp*ABS(Det)/Factorial       ! Simplical Cartesian volume of the Cell
      ENDIF
      IF( m_kDim.GT.0) THEN
         DO i=1,m_kDim
            DxHc = DxHc*m_CeVer2(iCell,i) ! Product of sizes in hypercubical subspace
         ENDDO
      ENDIF
      Dx=DxSp*DxHc              ! Total cartesian volume
      m_CeVolu(iCell)  = Dx
c[[[[[[[[[[[[[[[
c      DO iv=1,m_nDim
c          WRITE(*,'(a,9f10.5)') '### Xre=',(Xre(iv,j),j=1,m_nDim)
c      ENDDO
c      WRITE(*,'(a,f12.6)') 'FoamA_Explore: Cartesian volume Dx =',Dx
c]]]]]]]]]]]]]]]

*/////////////////////////////////////////////////////
*//    Exploratory MC sampling to probe the cell    //
*/////////////////////////////////////////////////////
      m_CeSum(iCell,1) =  0
      m_CeSum(iCell,2) =  0
      m_CeSum(iCell,3) =  0
      m_CeSum(iCell,4) =  1d90  ! wtmin
      m_CeSum(iCell,5) = -1d90  ! wtmax
      DO k=1,m_NdiviMax
         Xsu1(k)=0d0
         Xsu2(k)=0d0
         DO iBin=1,m_nBin
            Histo(k,iBin)=0d0
         ENDDO
      ENDDO
*///////////////////////////////////////////////////////////////////////////////////
*     Additional scan over vertices in order to improve max/min weights
*     Note that this option adds 2**m_kDim of the function calls, limited to 1000!
      IF( m_OptEdge .EQ. 1 ) THEN
         DO k=1,m_kDim
            digit(k)=0          ! initialize first partitions
         ENDDO
         DO loop=1,1000         ! start of loop over partitions
            DO iv=1,m_nDim+1
               DO j=1,m_nDim
                  Vrand(j) = m_VerX( m_CeVert(iCell,iv) ,j) ! simplical subspace
               ENDDO
               DO j=1,m_kDim
                  Vrand(m_nDim+j) = m_CeVer1(iCell,j) + digit(j)*m_CeVer2(iCell,j) ! h-cubical subspace
               ENDDO
               Wt =funW(Vrand)*Dx ! weight average normalised to integral over the cell
               m_CeSum(iCell,4) = MIN( m_CeSum(iCell,4),Wt) ! minium weight
               m_CeSum(iCell,5) = MAX( m_CeSum(iCell,5),Wt) ! maximu weight
            ENDDO
            digit(1)=digit(1)+1
            DO k=1,m_kDim-1
               IF(digit(k).EQ.2) THEN
                  digit(k)=0
                  digit(k+1)=digit(k+1)+1
               ENDIF
            ENDDO
            IF( m_kDim .EQ.0 )      GOTO 122
            IF( digit(m_kDim).EQ.2) GOTO 122
         ENDDO                  ! end of loop over partitions
 122     CONTINUE
      ENDIF
*///////////////////////////////////////////////////////////////////////////////////
*     generate randomly/uniformly vector Vrand inside this simplex&hypercube
      DO i=1,m_CeSamp(iCell)
         CALL FoamA_MakeLambda(Lambda)
         CALL FoamA_MakeAlpha( Alpha)
         DO j=1,m_nDim
            Vrand(j) = Vec(m_nDim+1,j)
            DO iv=1,m_nDim
               Vrand(j) = Vrand(j) +Lambda(iv)*Xre(iv,j) ! simplical subspace
            ENDDO
         ENDDO
         DO j=1,m_kDim
            Vrand(m_nDim+j) = m_CeVer1(iCell,j) +Alpha(j)*m_CeVer2(iCell,j) ! hypcubic subspace
         ENDDO
****     WRITE(*,'(a,6f12.6)') ' Lambda    =',(Lambda(k),k=1,m_nDim)
****     WRITE(*,'(a,6f12.6)') ' Alpha     =',(Alpha(k), k=1,m_kDim)
****     WRITE(*,'(a,6f12.6)') ' Vrand     =',(Vrand(k), k=1,m_Dimen)
*///////////////////////////////////////////////////////////////////////////////////
* Projecting on all simplex edges, preparatory step, partial volumes etc.
         IF( m_nDim .GT. 0 ) THEN
            Vsum=0d0
            DO jv=1,m_nDim+1    ! vertex jv will be replaced with Vrand
               kv=0             ! kv numbers all verices axcept jv
               DO iv=1,m_nDim+1
                  IF(iv.NE.jv) THEN
                     kv=kv+1
                     DO j=1,m_nDim
                        Yre(kv,j) = Vec(iv,j)-Vrand(j) ! All vertices except jv relative to Vrand
                     ENDDO
                  ENDIF
               ENDDO
               CALL FoamA_Determinant(Yre,DxPartial)
               VolPart(jv) = ABS(DxPartial)/Factorial
               Vsum=Vsum + VolPart(jv)
            ENDDO
            IF( ABS(Vsum-DxSp)/DxSp .GT. 1d-5) GOTO 950 ! X-check
         ENDIF
*------------------------------------------------------------------------------------
* IMPORTANT! Two Loops below determine the indexing of edges (simplex and hypercube)
*------------------------------------------------------------------------------------
         nEdges=0
         DO jv=1,m_nDim+1
            DO iv=jv+1,m_nDim+1
               nEdges=nEdges+1
               Xdivi(nEdges) = VolPart(jv) / ( VolPart(jv)+VolPart(iv) )
            ENDDO
         ENDDO
         DO j=1,m_kDim
            nEdges=nEdges+1
            Xdivi(nEdges) = Alpha(j)
         ENDDO
*///////////////////////////////////////////////////////////////////////////////////
         Wt =funW(Vrand)*Dx  ! principal weight normalised to integral over the cell
*------------------------------------------------------------------------------------
         m_nCalls = m_nCalls+1
         m_CeSum(iCell,1) = m_CeSum(iCell,1)+ Wt         ! sum of weights
         m_CeSum(iCell,2) = m_CeSum(iCell,2)+ Wt*Wt      ! sum of weights squared
         m_CeSum(iCell,3) = m_CeSum(iCell,3)+ 1d0        ! sum of 1
         m_CeSum(iCell,4) = MIN( m_CeSum(iCell,4),Wt)    ! minium weight
         m_CeSum(iCell,5) = MAX( m_CeSum(iCell,5),Wt)    ! maximu weight
* Search for the best edge candidate for future cell division, prepare MC material
         Ndivi  = (m_nDim+1)*m_nDim/2 +m_kDim
         IF(nEdges.NE.nDivi) GOTO 970
         DO k=1,nDivi
            Xsu1(k)=Xsu1(k) +Xdivi(k)*Wt                 ! averages for all Xdivi
            Xsu2(k)=Xsu2(k) +Xdivi(k)**2*Wt
            iBin = INT(Xdivi(k)*m_nBin)+1d0
            iBin = MIN(MAX(iBin,0),m_nBin)
            Histo(k,iBin) = Histo(k,iBin)+Wt             ! fill histo for each edge
c[[[[[[[[[[[[ debug [[[[[[[[[[[[ debug [[[[[[[[[[[[
c            IF(iCell.EQ.4) THEN
c               CALL GLK_Fil1(1200+k, Xdivi(k),Wt)
c            ENDIF
c]]]]]]]]]]]] debug ]]]]]]]]]]]] debug ]]]]]]]]]]]]
         ENDDO
         IF( m_EvPerBin.GT.0 ) THEN
            NevEff = m_CeSum(iCell,1)**2/m_CeSum(iCell,2)
            IF( NevEff .GE. m_nBin*m_EvPerBin) GOTO 222 !
         ENDIF
      ENDDO
 222  CONTINUE
*///////////////////////////////////////////////////////
*//   End of Special Short MC sampling to probe cell  //
*///////////////////////////////////////////////////////


*//////////////////////////////////////////////////////////////////////////////////////
*//  Determine the best edge candidate for future cell division, 
*//  using MC  material in Histo,Xsu1,Xsu2
*//  kBest,xBest,yBest are the output results
      CALL FoamA_Carver(iCell,Histo,Xsu1,Xsu2,kBest,xBest,yBest)
*//////////////////////////////////////////////////////////////////////////////////////
      NoMC =m_CeSum(iCell,3)
      IF( m_CeSum(iCell,1) .LT.0d0) GOTO 920
      m_CeXave(iCell)  = xBest                      ! best lambda for future division
      m_CeBest(iCell)  = kBest                      ! best edge for future division
      m_CeIntg(iCell)  = m_CeSum(iCell,1)/NoMC      ! estimator of the true integral
*!!!!!!!!!  DrivE is for the Foam build-up (not for MC generation)    !!!!!!!!!!
*!!!!!!!!!  PRIMARY is for MC generation (not for the Foam build-up ) !!!!!!!!!!
      IF(     m_OptDrive.EQ.0 ) THEN
         m_CePrim(iCell) = m_CeIntg(iCell)                   ! True integral, MC generation
         m_CeDriv(iCell) = m_CeIntg(iCell)                   ! True integral, Foam build-up
      ELSEIF( m_OptDrive.EQ.1 ) THEN
         m_CePrim(iCell) = DSQRT(m_CeSum(iCell,2)/NoMC)      ! Sqrt( <w>**2 + sigma**2 )= Sqrt(<w**2>)
         m_CeDriv(iCell) = DSQRT(m_CeSum(iCell,2)/NoMC -m_CeIntg(iCell)**2) ! sigma =Sqrt(<w**2>-<w>**2)
      ELSEIF( m_OptDrive.EQ.2 ) THEN
         m_CePrim(iCell) = m_CeSum(iCell,5)                  ! wtmax    , MC generation
         m_CeDriv(iCell) = m_CeSum(iCell,5) -m_CeIntg(iCell) ! wtmax-<w>, Foam build-up
      ELSE
         WRITE(m_out,*) ' ++++ STOP in FoamA_Explore, wrong m_OptDrive =',m_OptDrive !
         WRITE(    *,*) ' ++++ STOP in FoamA_Explore, wrong m_OptDrive =',m_OptDrive !
         STOP
      ENDIF
* correct volume and Drive in all parent cells to the top of the tree
      parent = m_CePare(iCell)
      DO i = 1,m_nBuf
         IF( parent .EQ. -1 ) GOTO 100 ! Exit if no parent exists
         m_CeIntg(parent)  = m_CeIntg(parent)  +( m_CeIntg(iCell)  -VolOld) !
         m_CeDriv(parent)  = m_CeDriv(parent)  +( m_CeDriv(iCell)  -DriOld) !
         parent=m_CePare(parent)
      ENDDO
 100  CONTINUE
      RETURN
 920  WRITE(*,*) ' ### STOP in FoamA_Explore: something wrong with integrand ' !
      STOP
 950  WRITE(*,*) ' ### STOP in FoamA_Explore: something wrong with volume calculation ' !
      WRITE(*,*) ' Vsum,DxSp= ',Vsum,DxSp
      STOP
 960  WRITE(*,*) ' ### STOP in FoamA_Explore: something wrong with best pair pointer =',kBest !
      STOP
 970  WRITE(*,*) ' ### STOP in FoamA_Explore: something wrong with Ndivi =',nDivi !
      STOP
      END                       ! FoamA_Explore



      SUBROUTINE FoamA_Carver(iCell,Histo,Xsu1,Xsu2,kBest,xBest,yBest)  !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*// Determine the best edge candidate for future cell division, using MC  material   //
*// kBest is the best edge found, xBest and yBest are the best values of lambda      //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            iCell,kBest
      DOUBLE PRECISION   xBest,yBest,zBest
      DOUBLE PRECISION   Histo(m_NdiviMax,m_nBinMax), Xsu1(m_NdiviMax), Xsu2(m_NdiviMax) !
      DOUBLE PRECISION   Bins(m_nBinMax)
      INTEGER            i,j,k, iBin, nDivi
      INTEGER            iUp,iLow, jUp,jLow, kDivi, jv,iv, jDivi
      DOUBLE PRECISION   SumWt,BinMax,This, Carve, yLevel
      DOUBLE PRECISION   CarvOne,CarvTwo,CarvMax
*
      kBest =1
      xBest =0.5d0
      yBest =1d0
      SumWt  = m_CeSum(iCell,1)
      IF( SumWt .NE. 0d0) THEN
         CarvMax = -1d150
         nDivi  = (m_nDim+1)*m_nDim/2 +m_kDim
         DO kDivi=1,nDivi
            BinMax  = -1d150
            DO iBin=1,m_nBin    ! Unload histo and Maximum bin
               Bins(iBin) = Histo(kDivi,iBin)
               BinMax = MAX( BinMax, Bins(iBin)) ! Maximum content/bin
            ENDDO
            CarvTwo = 0d0
            DO iBin=1,m_nBin
               Bins(iBin) = Bins(iBin)/BinMax ! Normalize to the highest bin
               CarvTwo = CarvTwo + (1d0-Bins(iBin))/m_nBin ! Total carve
            ENDDO
* Find maximum 'rectangular carve' in betwen the two bins (jLow,...,jUp)
            jLow =1
            jUp  =m_nBin
            CarvOne = -1d150
            yLevel  = -1d150
            DO iBin=1,m_nBin
               This = Bins(iBin)
               iLow = iBin
               DO j=iBin,1,-1   ! walk to the left and find first bin > current
                  IF(This .LT. Bins(j) ) GOTO 100
                  iLow = j
               ENDDO
 100           CONTINUE
               iUp  = iBin
               DO j=iBin,m_nBin ! walk to the right and find first bin > current
                  IF(This .LT. Bins(j) ) GOTO 200
                  iUp  = j
               ENDDO
 200           CONTINUE
               Carve = (iUp-iLow+1)*(1d0-This)/m_nBin
               IF( Carve .GT. CarvOne) THEN
                  CarvOne = Carve
                  jLow = iLow
                  jUp  = iUp
                  yLevel = This
               ENDIF
            ENDDO               ! end-loop over histogram bins 
*************************************************************
**               IF( CarvOne .GT. CarvMax) THEN
**                  CarvMax   = CarvOne
*************************************************************
            IF( CarvTwo .GT. CarvMax) THEN
               CarvMax   = CarvTwo
               kBest = kDivi    ! Best edge
               xBest = (jLow-1d0)/m_nBin
               yBest = (jUp*1d0)/m_nBin
               IF(jLow .EQ. 1 )     xBest = yBest
               IF(jUp  .EQ. m_nBin) yBest = xBest
* division ratio in units of 1/m_nBin, testing
               jDivi = jLow-1
               IF(jLow .EQ. 1 )     jDivi=jUp
**************************************************************
* The improvement below does not seem to matter at all
*                  IF( 0.5d0*(xBest+yBest) .LT. 0.5d0) THEN
*                     zBest =xBest
*                     xBest =yBest
*                     yBest =zBest
*                  ENDIF
**************************************************************
            ENDIF
*[[[[[[[[[[[[ debug [[[[[[[[[[[[ debug [[[[[[[[[[[[[[[[[[
*               WRITE(*,'(a,4i5,f10.5)') '==iCell,kDivi,jLow,jUp,CarvMax= ',iCell,kDivi,jLow,jUp,CarvOne !!
*               IF(iCell.EQ.2 ) THEN
*                  CALL GLK_Pak(  1200+kDivi,Bins)
*                  DO iBin = 1,m_nBin
*                     Bins(iBin) = 1d0
*                  ENDDO
*                  DO iBin=jLow,jUp
*                     Bins(iBin) = yLevel
*                  ENDDO
*                  CALL GLK_Pak(  1400+kDivi,Bins)
*               ENDIF
c]]]]]]]]]]]]debug ]]]]]]]]]]]] debug ]]]]]]]]]]]]]]]]]]]
         ENDDO                  ! end-loop over nDivi
      ENDIF
*[[[[[[[[[[[[ debug [[[[[[[[[[[[ debug [[[[[[[[[[[[
*      WRITE(*,'(a,i5)')        ' FoamA_Carver , iCell = ', iCell
*      WRITE(*,'(a,i5,3f10.5)') ' kBest,xBest,yBest= ', kBest, xBest,yBest!!
*]]]]]]]]]]]] debug ]]]]]]]]]]]] debug ]]]]]]]]]]]]
      END                       ! FoamA_Carver


      SUBROUTINE FoamA_RanDiscr(Driv,nTot,Power,iRand)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   PRESENTLY UNUSED !!!                                                           //
*//   Generates iRand in (1,nTot) acconding to discrete un-normalized probab. Driv   //
*//   Power is normaly =1, can be useful for special purposes                        //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      DOUBLE PRECISION   Driv(*),Power
      INTEGER            nTot,iRand
      INTEGER            i
      DOUBLE PRECISION   random,sum,Total
      REAL               Qrand(10)        ! from PseuMar
*
      Total   = 0d0
      DO i= 1,nTot
         Total = Total +Driv( i)**Power
      ENDDO
      IF(Total .EQ. 0d0) GOTO 990
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,1)
      ELSE
         CALL RanLux(Qrand,1)
      ENDIF
      random = Qrand(1)
      iRand  = -1
      sum   = 0d0
      DO i= 1,nTot
         iRand  = i
         sum = sum +Driv( i)**Power
         IF( random .LT. sum/Total ) GOTO 100
      ENDDO
      IF(iRand .EQ. -1) GOTO 990
 100  CONTINUE
      RETURN
 990  WRITE(*,*) ' ### STOP in FoamA_RanDiscr, something went wrong !!!!'
      STOP
      END


      SUBROUTINE FoamA_MakeAlpha(Alpha)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                      //
*//   Provides random vector Alpha, each component in (0,1) range                        //
*//                                                                                      //
*//////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   Alpha(m_KdiMax), y
      REAL               Qrand( m_KdiMax)        ! from PseuMar
      INTEGER            i,k
*     --------------------------------------------------------
      IF( m_kDim.LT.0 .OR. m_kDim.GT.m_KdiMax) THEN
         WRITE(*,*) 'STOP in FoamA_MakeAlpha: m_kDim=',m_kDim
         STOP
      ENDIF
      IF(m_kDim.LE.0) RETURN
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,m_kDim)
      ELSE
         CALL RanLux(Qrand,m_kDim)
      ENDIF
      DO k =1,m_kDim
         Alpha(k)=Qrand(k)
      ENDDO
      END

      SUBROUTINE FoamA_MakeLambda(Lambda)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                      //
*//   Provides random vector Lambda such that Sum Lamba(i) = 1, with uniform probab.     //
*//   This  vector is used to populate uniformly the interior of a simplex.              //
*//   The method is: generate point inside cube, order components (maping into simplex)  //
*//   and take differences of Lambda(i+1) - Lambda(i)                                    //
*//                                                                                      //
*//////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            i,k
      DOUBLE PRECISION   Lambda(m_NdiMax), y
      REAL               Qrand( m_NdiMax)        ! from PseuMar
      REAL               x
*     --------------------------------------------------------
      IF(m_nDim.LE.0) RETURN
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,m_nDim)
      ELSE
         CALL RanLux(Qrand,m_nDim)
      ENDIF
* ordering vector components (maping into simplex)
      DO i =m_nDim,1,-1
         DO k =2,i
            IF( Qrand(k).LT.Qrand(k-1)) THEN
               x            = Qrand(k)
               Qrand(k)    = Qrand(k-1)
               Qrand(k-1)  = x
            ENDIF
         ENDDO
      ENDDO
* Sum of lambdas should equal one
      Lambda(1)=Qrand(1)
      DO k =2,m_nDim
         Lambda(k)=Qrand(k)-Qrand(k-1)
      ENDDO
      END                       ! MakeLambda


      SUBROUTINE FoamA_Determinant(R,Det)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Calculates determinant of matrix R                                             //
*//   Use of Laplace formula should be perhaps replaced with something faster        //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det
      DOUBLE PRECISION   FoamA_Det2Lapl, FoamA_Det3Lapl,FoamA_Det4Lapl,FoamA_Det5Lapl !
*     -------------------------------------------------
      IF(        m_nDim .EQ. 1) THEN
         Det= R(1,1)
      ELSEIF(    m_nDim .EQ. 2) THEN
         Det= FoamA_Det2Lapl(R, 1,2)
      ELSEIF(    m_nDim .EQ. 3) THEN
         Det= FoamA_Det3Lapl(R, 1,2,3)
      ELSEIF(    m_nDim .EQ. 4) THEN
         Det= FoamA_Det4Lapl(R, 1,2,3,4)
      ELSEIF(    m_nDim .EQ. 5) THEN
         Det= FoamA_Det5Lapl(R, 1,2,3,4,5)
      ELSE
         WRITE(*,*) '####FoamA_Determinant: STOP, m_nDim =',m_nDim
         STOP
      ENDIF
      END                       ! FoamA_Determinant

      DOUBLE PRECISION FUNCTION FoamA_Det2Lapl(R,i1,i2)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det
      INTEGER  i1,i2
*     ------------------------------------------------------------
      FoamA_Det2Lapl= R(1,i1)*R(2,i2) - R(1,i2)*R(2,i1)
      END


      DOUBLE PRECISION FUNCTION FoamA_Det3Lapl(R,i1,i2,i3)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   Determinat Laplace method                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det,FoamA_Det2Lapl
      INTEGER  i1,i2,i3
*     ------------------------------------------------------------
      FoamA_Det3Lapl=+R(3,i1) *FoamA_Det2Lapl(R,i2,i3)
     $               -R(3,i2) *FoamA_Det2Lapl(R,i1,i3)
     $               +R(3,i3) *FoamA_Det2Lapl(R,i1,i2)
      END

      DOUBLE PRECISION FUNCTION FoamA_Det4Lapl(R,i1,i2,i3,i4)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   Determinat Laplace method                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det,FoamA_Det3Lapl
      INTEGER  i1,i2,i3,i4
*     ------------------------------------------------------------
      FoamA_Det4Lapl=-R(4,i1) *FoamA_Det3Lapl(R,i2,i3,i4)
     $               +R(4,i2) *FoamA_Det3Lapl(R,i1,i3,i4)
     $               -R(4,i3) *FoamA_Det3Lapl(R,i1,i2,i4)
     $               +R(4,i4) *FoamA_Det3Lapl(R,i1,i2,i3)
      END

      DOUBLE PRECISION FUNCTION FoamA_Det5Lapl(R,i1,i2,i3,i4,i5)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   Determinat Laplace method                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax), Det, FoamA_Det4Lapl
      INTEGER  i1,i2,i3,i4,i5
*     ------------------------------------------------------------
      FoamA_Det5Lapl=+R(5,i1) *FoamA_Det4Lapl(R,i2,i3,i4,i5)
     $               -R(5,i2) *FoamA_Det4Lapl(R,i1,i3,i4,i5)
     $               +R(5,i3) *FoamA_Det4Lapl(R,i1,i2,i4,i5)
     $               -R(5,i4) *FoamA_Det4Lapl(R,i1,i2,i3,i5)
     $               +R(5,i5) *FoamA_Det4Lapl(R,i1,i2,i3,i4)
      END


      SUBROUTINE FoamA_Grow(funW)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Grow new cells by division                                                     //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            i,RC,iCell
*---------------------------------------------
* Final division
      DO i=1,100000
         IF(  m_OptPeek .EQ. 0 ) THEN
            CALL FoamA_PeekMax(  iCell)       ! choose cell with maximum Drive
         ELSE
            CALL FoamA_PeekRand1(iCell)       ! randomly choose one cell
         ENDIF
         CALL FoamA_Divide( iCell,funW,RC)    ! and divide it into two
c[[[[
c         CALL FoamA_BufPrint(    m_Out)
c         CALL FoamA_VertPrint(  6)
c]]]]
         IF(RC.EQ.-1) GOTO 300
      ENDDO
 300  CONTINUE
      WRITE(16,*) '######################### FoamA_Grow #####################'
*****[[[[[[ debug
***** CALL FoamA_BufPrint(    6)
***** CALL FoamA_BufActPrint( 6)
***** CALL FoamA_VertPrint(   6)
*****]]]]]]
      CALL FoamA_Check(6,0)
      END                       ! FoamA_Grow

      SUBROUTINE FoamA_PeekMax(iCell)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Create list of active cells (pointers) using DrivE                             //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER   iCell
      INTEGER   i
      DOUBLE PRECISION  DrivMax, Driv
*     ---------------------------------------------------
      iCell = 0
      DrivMax = -1d150
      DO i = 1,m_LastCe
         IF( m_CeStat(i).EQ.1 ) THEN
            Driv=  ABS(m_CeDriv(i))
            IF(Driv .GT. DrivMax) THEN
               DrivMax = Driv
               iCell = i
            ENDIF
         ENDIF
      ENDDO
****  WRITE(*,*) '###>>> FoamA_PeekMax: iCell=',iCell
      IF(iCell.EQ.0) THEN
         WRITE(*,*) '### STOP in FoamA_PeekMax: not found iCell=', iCell
         STOP
      ENDIF
      END                       ! FoamA_PeekMax


      SUBROUTINE FoamA_PeekRand1(iCell)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//       Peek up randomly TREE-WISE the pointer iCell of an active cell             //
*//                        Using DrivE                                               //
*// We walk randomly from top of tree downwards until we find active cell m_CeStat=1 //
*// At each step one of daugters cells is choosen randomly according                 //
*// to their volume estimates.                                                       //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            iCell
      INTEGER            kCell,i,Dau1,Dau2,iDau
      DOUBLE PRECISION   random,p1,volu1,volu2,volu,TotDri,sum
      REAL               Qrand(10)        ! from PseuMar
*     ----------------------------------------------------------------
* first cell is special because it has nDim-factorial daughters, istead of just 2
      iCell=1
      IF(m_CeStat(iCell).EQ.1) RETURN ! In case of cubics firs cell may be active
      kCell = 1
      Dau1  = m_CeDau1(kCell)
      Dau2  = m_CeDau2(kCell)
      TotDri   = 0d0
      DO iCell= Dau1,Dau2
         TotDri = TotDri+m_CeDriv( iCell )
      ENDDO
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,1)
      ELSE
         CALL RanLux(Qrand,1)
      ENDIF
      random = Qrand(1)
      iDau  = -1
      sum   = 0d0
      DO iCell= Dau1,Dau2
         iDau  = iCell
         sum = sum+m_CeDriv( iCell )
         IF( random .LT. sum/TotDri ) GOTO 100
      ENDDO
      IF(iDau.EQ.-1) GOTO 990
 100  kCell=iDau
****[[[[[[[[[[[[[[[[
****  WRITE(*,*) '%%%%%%%%%%%%%%%%%%%%%% FoamA_PeekRand1: top daughter =',kCell
****]]]]]]]]]]]]]]]]
      IF( m_CeStat( kCell ) .EQ. 1 ) GOTO 300
* now the other standard cells with 2 daughters
      DO i=1,10000000
         IF( m_CeStat( kCell ) .EQ. 1 ) GOTO 300
         volu1= m_CeDriv( m_CeDau1(kCell) )
         volu2= m_CeDriv( m_CeDau2(kCell) )
         p1 = volu1/(volu1+volu2)
         IF(m_OptRanLux.EQ.-1) THEN
            CALL PseuMar_MakeVec(Qrand,1)
         ELSE
            CALL RanLux(Qrand,1)
         ENDIF
         random = Qrand(1)
         IF( random .LT. p1 ) THEN
            kCell = m_CeDau1(kCell)
         ELSE
            kCell = m_CeDau2(kCell)
         ENDIF
****[[[[[[[[[[[[[[[[[[[[
****  WRITE(*,*) '%%%%%%%%%%%%%%%%%%%%%% FoamA_PeekRand1: normal daughter =',kCell
****]]]]]]]]]]]]]]]]]]]]]
      ENDDO
      GOTO 990
 300  CONTINUE
      iCell=kCell
****[[[[[[[[[[[[[[[[[[[[
****  WRITE(*,*) '%%%%%%%%%%%%%%%%%%%%%% FoamA_PeekRand1: choosen cell =',kCell
****]]]]]]]]]]]]]]]]]]]]]
      RETURN
 990  WRITE(*,*) ' ### STOP in FoamA_PeekRand1, something went wrong !!!!'
      STOP
      END                       !!! FoamA_PeekRand1


      SUBROUTINE FoamA_Divide(iCell,funW,RC)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Divide cell iCell into two daughter cells                                      //
*//   The iCell is retained and taged as inactive, daughter cells are appended       //
*//   at the end of the buffer.                                                      //
*//   New vertex is added to list of vertice.                                        //
*//   List of active cells is updated, iCell remooved, two daughters added           //
*//   and their properties set with help of MC sampling (FoamA_Explore)              //
*//   Return Code RC=-1 of buffer limit is reached,  m_LastCe=m_nBuf                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            iCell,RC
      INTEGER            Dau1, Dau2, kVer1(m_NdiMax+1), kVer2(m_NdiMax+1),p1,p2 !
      DOUBLE PRECISION   HcPosi1(m_KdiMax),   HcSize1(m_KdiMax)
      DOUBLE PRECISION   HcPosi2(m_KdiMax),   HcSize2(m_KdiMax)
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            Old1,Old2,j,k,jv,iv,nEdges,kBest,kDivi,NoMC
      DOUBLE PRECISION   Xave
      INTEGER            nDivi,nDivi0
*--------------------------------------------------------------------------------------
      RC = 0
      IF( m_LastCe+2 .GT. m_nBuf) GOTO 990 !! abort if no space in buffer
* reset cell as inactive
      m_CeStat(iCell) = 0
*------------------------------------------------------------------------------------
* Define lists of vertices for daughters, initially inherited...
      IF(m_nDim .GT. 0) THEN
         DO jv=1,m_nDim+1
            kVer1(jv) = m_CeVert(iCell,jv)
            kVer2(jv) = m_CeVert(iCell,jv)
         ENDDO
      ENDIF
* Position and size initially also inherited
      DO j=1,m_kDim
         HcPosi1(j)=m_CeVer1(iCell,j)
         HcSize1(j)=m_CeVer2(iCell,j)
         HcPosi2(j)=m_CeVer1(iCell,j)
         HcSize2(j)=m_CeVer2(iCell,j)
      ENDDO
*--------------------------------------------------------------------------------------
      nDivi  = (m_nDim+1)*m_nDim/2 +m_kDim
      nDivi0 = (m_nDim+1)*m_nDim/2
      kBest  = m_CeBest(iCell)
      IF( kBest.LE.nDivi0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//   The best division in simplical subspace
*//////////////////////////////////////////////////////////////////////////////////////
         m_LastVe=m_LastVe+1    ! add new vertex to the list
         IF(m_LastVe.GT.m_vMax) GOTO 980
         Xave  = m_CeXave(iCell)
         nEdges=0
         DO jv=1,m_nDim+1
            DO iv=jv+1,m_nDim+1
               nEdges=nEdges+1
               IF( nEdges.EQ.kBest) THEN
                  p1 =  m_CeVert(iCell,jv)
                  p2 =  m_CeVert(iCell,iv)
                  DO j=1,m_nDim
                     m_VerX(m_LastVe,j) = Xave*m_VerX(p1,j) + (1d0-Xave)*m_VerX(p2,j) ! New Vertex
                  ENDDO
                  Old1=jv
                  Old2=iv
                  GOTO 100
               ENDIF
            ENDDO
         ENDDO
 100     CONTINUE
         kVer1(Old1)=m_LastVe   ! one old vertex replaced by new one
         kVer2(Old2)=m_LastVe   ! one old vertex replaced by new one
      ELSE
*//////////////////////////////////////////////////////////////////////////////////////
*//   The best division in hypercubical subspace
*//   Position and size inherited except one direction k=kDivi
*//////////////////////////////////////////////////////////////////////////////////////
         Xave  = m_CeXave(iCell)
         kDivi = kBest-nDivi0
         HcPosi1(kDivi)=      m_CeVer1(iCell,kDivi)        ! (1) Position unchanged
         HcSize1(kDivi)= Xave*m_CeVer2(iCell,kDivi)        ! (1) Size reduced by Xave
         HcPosi2(kDivi)= HcPosi1(kDivi) +HcSize1(kDivi)    ! (2) Position shifted
         HcSize2(kDivi)= (1d0-Xave)*m_CeVer2(iCell,kDivi)  ! (2) Size reduced by (1-Xave)
      ENDIF
* define two daughter cells (active)
      NoMC= m_nSampl
* ------------------------ Stat, Pare, Dau1,Dau2, MCsampl, VertList, Position,Size,  iCeNew
      CALL FoamA_DefCell(  1, iCell,  -1,  -1,    NoMC,   kVer1,  HcPosi1, HcSize1, Dau1) !
      CALL FoamA_DefCell(  1, iCell,  -1,  -1,    NoMC,   kVer2,  HcPosi2, HcSize2, Dau2) !
      m_CeDau1(iCell) = Dau1
      m_CeDau2(iCell) = Dau2
      CALL FoamA_Explore(Dau1,funW)
      CALL FoamA_Explore(Dau2,funW)
* Update list of active cells, only for internal tests
      CALL FoamA_ActUpda
      RETURN
 990  RC=-1                     !!buffer limit is reached,  m_LastCe=m_nBuf
      RETURN
 980  WRITE(*,*) ' ### STOP in FoamA_Divide: too short list of vertices '
      STOP
      END                       ! FoamA_Divide



      SUBROUTINE FoamA_ActUpda     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Create list of active cells (pointers)                                         //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER   iCell,Dau1,Dau2
      INTEGER   i
*     ---------------------------------------------------
      m_LastAc=0
      DO iCell = 1,m_LastCe
         IF( m_CeStat(iCell).EQ.1 ) THEN
            m_LastAc=m_LastAc+1
            IF(m_LastAc .EQ. m_cMax) GOTO 950
            m_ActC(m_LastAc) = iCell
         ENDIF
      ENDDO
      RETURN
 900  WRITE(*,*) '### STOP in FoamA_ActUpda: not found iCell=', iCell
      STOP
 950  WRITE(*,*) '### STOP in FoamA_ActUpda: list of active cells too short'
      STOP
      END                       ! FoamA_ActUpda


      SUBROUTINE FoamA_CellGener(iCell)     !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//       Peek up randomly pointer iCell of an active cell according to PRIMARY      //
*//       Straightforward way, using list of active pointes made by ActUpda          //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            iCell
      INTEGER            kCell,i,Dau1,Dau2,iDau
      DOUBLE PRECISION   random,x1,TotPrim
      INTEGER            klower,kuper,krange,kurrent,DipSwitch
      REAL               Qrand(10)        ! from PseuMar
      INTEGER   iCont
      DATA      iCont/0/
      iCont = iCont+1
*     ----------------------------------------------------------------
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,1)
      ELSE
         CALL RanLux(Qrand,1)
      ENDIF
      random = Qrand(1)
*     ---------------------------------------------------
      TotPrim = m_CePrCu(m_LastAc)
      x1 = TotPrim *random
      DipSwitch=0               ! =0 is faater
      IF( DipSwitch.EQ.1) THEN
*-------- primitive method --------
         DO kCell = 1,m_LastAc
            iCell = m_ActC(kCell)
            IF( m_CePrCu(kCell) .GE. x1 ) GOTO 800
         ENDDO
         WRITE(*,*) '### STOP1 in FoamA_CellGener: something wrong' !
         STOP
 800     CONTINUE
      ELSEIF( DipSwitch.EQ.0) THEN
*-------- a bit more sophisticated/faster method ------
         klower   = 0
         kuper    = m_LastAc
         DO i=1,m_LastAc
            krange   = (kuper-klower+1)/2
            kurrent  = klower +krange
            IF( x1 .LE. m_CePrCu(kurrent) ) THEN
               kuper = kurrent
            ELSE
               klower = kurrent
            ENDIF
            IF(kuper-klower.LE.1) GOTO 850
         ENDDO
         WRITE(*,*) ' STOP in FoamA_CellGener'
         STOP
 850     CONTINUE
         iCell = m_ActC(kuper)
         kCell = kuper
      ELSE
         iCell = m_ActC(1)      ! nonsense for tests
      ENDIF
***** IF(iCont.LE.10) WRITE(*,*) 'LastAc,kCell,iCell = ',m_LastAc,kCell,iCell !! debug
      END                       ! FoamA_CellGener


      SUBROUTINE FoamA_MakeEvent(funW)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Generates point/vector Xrand with the weight MCwt                              //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            iCell,i,j,iv
      DOUBLE PRECISION   Wt,x1,x2,Dx,MCwt
      DOUBLE PRECISION   Alpha( m_KdiMax) ! Hyper-Cubic subspace
      DOUBLE PRECISION   Lambda(m_NdiMax) ! Simplical subspace
*     -----------------------------------------------------------------
*     choose randomly one cell
      CALL FoamA_CellGener( iCell)
*     generate randomly/uniformly vector Vrand inside this simplex
      CALL FoamA_MakeAlpha( Alpha)
      CALL FoamA_MakeLambda(Lambda)
      DO j=1,m_nDim
         m_MCvector(j) = m_VerX( m_CeVert(iCell,m_nDim+1) ,j)
         DO iv=1,m_nDim
            m_MCvector(j) = m_MCvector(j) 
     $           +Lambda(iv)*( m_VerX( m_CeVert(iCell,iv) ,j) -m_VerX( m_CeVert(iCell,m_nDim+1) ,j) ) !
         ENDDO
      ENDDO
      DO j=1,m_kDim
         m_MCvector(m_nDim+j) = m_CeVer1(iCell,j) +Alpha(j)*m_CeVer2(iCell,j) ! hypcubic subspace
      ENDDO
      Dx = m_CeVolu(iCell)      ! Cartesian volume of the Cell
*[[[[[[[[[[[[
***** CALL FoamA_TestNewConcept(iCell)
*]]]]]]]]]]]]
* weight average normalised to PRIMARY integral over the cell
      MCwt =funW(m_MCvector)*Dx/m_CePrim(iCell) ! PRIMARY controls normalization
      m_nCalls =  m_nCalls+1
      m_MCwt   =  MCwt
* accumulation of statistics for the main MC weight
      m_SumWt  =  m_SumWt  +MCwt         ! sum of Wt
      m_SumWt2 =  m_SumWt2 +MCWt*MCwt    ! sum of Wt**2
      m_NevGen =  m_NevGen +1d0          ! sum of 1d0
      m_WtMax  =  MAX(m_WtMax,MCwt)      ! maximum wt
      m_WtMin  =  MIN(m_WtMin,MCwt)      ! minimum wt
* update also weight sums in the cell,
* note weights here are normalized absolutely, eg. to the value of the integral
      Wt = MCwt*m_CePrim(iCell)
      m_CeSum(iCell,1) = m_CeSum(iCell,1)+ Wt      ! sum of weights
      m_CeSum(iCell,2) = m_CeSum(iCell,2)+ Wt*Wt   ! sum of weights squared
      m_CeSum(iCell,3) = m_CeSum(iCell,3)+ 1d0     ! sum of 1
      m_CeSum(iCell,4) = MIN( m_CeSum(iCell,4),Wt) ! minium weight
      m_CeSum(iCell,5) = MAX( m_CeSum(iCell,5),Wt) ! maximu weight
      END                       ! FoamA_MakeEvent

      SUBROUTINE FoamA_GetMCvector(MCvector)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION  MCvector(*)
      INTEGER           k
*-----------------------
      DO k=1,m_Dimen
         MCvector(k)    = m_MCvector(k)
      ENDDO
      END

      SUBROUTINE FoamA_GetMCwt(MCwt)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION  MCwt
*-----------------------
      MCwt    = m_MCwt
      END

      SUBROUTINE FoamA_MCgenerate(funW,MCvector,MCwt)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Obsolete                                                                       //
*//   Generates point/vector MCvector with the weight MCwt                           //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION   MCvector(*),MCwt
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            j
*     ---------------------------------------------------------------
      CALL FoamA_MakeEvent(funW)
      MCwt = m_MCwt
      DO j=1,m_Dimen
         MCvector(j) =m_MCvector(j)
      ENDDO
      END                       !!FoamA_MCgenerate


      SUBROUTINE FoamA_Finalize(MCresult,MCerror)    !# Finalization
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   After MC run is completed it calculates integral and its error         //
*//   Also prints some information/statistics on the MC run                  //
*//   Remember that PRIMARY controls normalization of the integration        //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
*
      DOUBLE PRECISION     MCresult,MCerror,MCerelat
      DOUBLE PRECISION     Vtot,Verr,VerRela,TotPrim
      INTEGER              kCell,iCell
*-----------------------------------------------------------------------------
      MCresult =0d0
      MCerelat =1d0
      TotPrim = 0d0
      DO kCell = 1,m_LastAc
         iCell  = m_ActC(kCell)
         TotPrim = TotPrim +m_CePrim( iCell )
      ENDDO
      IF(m_NevGen .GT. 0) THEN
         MCresult = TotPrim *m_SumWt/m_NevGen
         IF(m_SumWt.NE.0d0)  MCerelat  = m_SumWt2/m_SumWt**2 -1d0/m_NevGen !
         IF(MCerelat.GT.0d0) MCerelat =  SQRT( ABS(MCerelat) ) !
         IF(MCerelat.LT.0d0) MCerelat = -SQRT( ABS(MCerelat) ) !
      ENDIF
      MCerror = MCresult*MCerelat
* some test printouts
      WRITE(m_Out,'(3a)') '||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||' !
      WRITE(m_Out,'(3a)') '||||||||||||||||||||||||||||||', ' FoamA_Finalize ', !
     $                    '||||||||||||||||||||||||||||||'
      WRITE(m_Out,'(a,2g18.9,f11.7)')               'MCresult, MCerror, Errela= ',MCresult,MCerror,MCerelat !
      WRITE(m_Out,'(a,2f11.5)')                     'Minimum maximum weight   = ',m_WtMin,m_WtMax !
      IF(m_NevGen .GT. 0) WRITE(m_Out,'(a,2f11.5)') 'Average wt SumWt/NevGen  = ',m_SumWt/m_NevGen !
      WRITE(m_Out,'(a,i15)')                        'Total of function calls  = ',m_nCalls !
      WRITE(m_Out,'(a,2i15)')                       'Number of cells&vertices = ',m_LastCe,m_LastVe !
      WRITE(m_Out,'(a,f12.1,2g18.9)')               'NevGen,SumWt,m_SumWt2    = ',m_NevGen,m_SumWt,m_SumWt2 !
      END       ! FoamA_Finalize



      SUBROUTINE FoamA_GetTotPrim(TotPrim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Total integral from cell statistics, including initialization + MC generation  //
*//   It can be invoked just after initialization or after MC generation             //
*//   Note that this estimate is distorted slightly if vertices are included in      //
*//   the exploration of the cells.                                                  //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      INCLUDE 'FoamA.h'
      DOUBLE PRECISION  TotPrim,Sum
      INTEGER   kCell,  iCell      
      Sum = 0d0
      DO kCell = 1,m_LastAc
         iCell  = m_ActC(kCell)
         Sum = Sum +m_CePrim( iCell )
      ENDDO
      TotPrim=Sum
      END


      SUBROUTINE FoamA_SetnDim(nDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      nDim
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_nDim = nDim
      m_Dimen=m_nDim+m_kDim
      END                       !!! FoamA_SetnDim

      SUBROUTINE FoamA_SetkDim(kDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      kDim
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_kDim = kDim
      m_Dimen=m_nDim+m_kDim
      END                       !!! FoamA_SetnDim

      SUBROUTINE FoamA_GetnDim(nDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      nDim
*     -------------------------------------------------
      nDim = m_nDim
      END                       !!! FoamA_SetnDim

      SUBROUTINE FoamA_GetkDim(kDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      kDim
*     -------------------------------------------------
      kDim = m_kDim
      END                       !!! FoamA_SetkDim

      SUBROUTINE FoamA_GetnCalls(nCalls)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      nCalls
*     -------------------------------------------------
      nCalls = m_nCalls
      END                       !!! FoamA_SetnCalls

      SUBROUTINE FoamA_SetnBuf(nBuf)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      nBuf
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_nBuf = nBuf
      END                       !!! FoamA_SetnBuf

      SUBROUTINE FoamA_SetnBin(nBin)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      nBin
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_nBin = nBin
      END                       !!! FoamA_SetnBin

      SUBROUTINE FoamA_SetOut(Out)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      Out
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_Out = Out
      END                       !!! FoamA_SetOut

      SUBROUTINE FoamA_SetChat(Chat)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      Chat
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_Chat = Chat
      END                       !!! FoamA_SetChat

      SUBROUTINE FoamA_SetnSampl(nSampl)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      nSampl
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_nSampl = nSampl
      END                       !!! FoamA_SetnSampl

      SUBROUTINE FoamA_SetOptDrive(OptDrive)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      OptDrive
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_OptDrive = OptDrive
      END                       !!! FoamA_SetOptDrive

      SUBROUTINE FoamA_SetEvPerBin(EvPerBin)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      EvPerBin
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_EvPerBin = EvPerBin
      END                       !!! FoamA_SetEvPerBin

      SUBROUTINE FoamA_SetOptPeek(OptPeek)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      OptPeek
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_OptPeek = OptPeek
      END                       !!! FoamA_SetOptPeek

      SUBROUTINE FoamA_SetOptEdge(OptEdge)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      OptEdge
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_OptEdge = OptEdge
      END                       !!! FoamA_SetOptEdge

      SUBROUTINE FoamA_SetOptOrd(OptOrd)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      OptOrd
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_OptOrd = OptOrd
      END                       !!! FoamA_SetOptOrd


      SUBROUTINE FoamA_SetOptRanIni(OptRanIni)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      OptRanIni
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_OptRanIni = OptRanIni
      END                       !!! FoamA_SetOptRanIni

      SUBROUTINE FoamA_SetOptRanLux(OptRanLux)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER      OptRanLux
*     -------------------------------------------------
      CALL FoamA_PreInitialize
      m_OptRanLux = OptRanLux
      END                       !!! FoamA_SetOptRanLux

      SUBROUTINE FoamA_Check(mout,level)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//  Checks all pointers, It is is a usefull autodiagnostic.                         //
*//                                                                                  //
*//  level=0, no printout, failures causes STOP                                      //
*//  level=1, printout, failures lead to WARNINGS only                               //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER       mout,level
      INTEGER       nFailures, iCell, Dau1,Dau2, Pare, NoRefs(m_vMax), iVe,n !
      INTEGER       NoEmpty, iError
*     ---------------------------------------------------------
      nFailures=0
      iError   =0
      IF(level.EQ.1) WRITE(mout,*)
     $'//////////////////////////////////////// FoamA_Checks /////////////////////////////////////////////' !
      DO iCell = 1,m_LastCe
         Dau1 = m_CeDau1(iCell)
         Dau2 = m_CeDau2(iCell)
         Pare = m_CePare(iCell)
* checking on parents
         IF(iCell.GT.1) THEN
            IF(Pare.GT.m_LastCe) THEN
               iError   =1
               nFailures = nFailures+1
               IF(level.EQ.1) WRITE(mout,*) '##### iCell= ',iCell,' Parent out of range = ',Pare !
            ENDIF
         ENDIF
         IF(iCell.GT.1) THEN
            IF(  (Pare.NE.1) .AND. (m_CeDau1(Pare).NE.iCell) .AND. (m_CeDau2(Pare).NE.iCell)  ) THEN !
               iError   =2
               nFailures = nFailures+1
               IF(level.EQ.1) 
     $              WRITE(mout,*) '##### iCell= ',iCell,' Parent not pointing to this daughter Pare= ',Pare !
            ENDIF
         ENDIF
* checking on daughters
         IF( Dau1 .GT. m_LastCe ) THEN
            iError   =3
            nFailures = nFailures+1
            IF(level.EQ.1) WRITE(mout,*) '##### iCell= ',iCell,' First  Daugter out of range Dau1= ',Dau1 !
         ENDIF
         IF( Dau2 .GT.m_LastCe ) THEN
            iError   =4
            nFailures = nFailures+1
            IF(level.EQ.1) WRITE(mout,*) '##### iCell= ',iCell,' Second Daugter out of range Dau2= ',Dau2 !
         ENDIF
         IF( Dau1.GE.1 .AND. Dau1.LE. m_LastCe) THEN
            IF( m_CePare(Dau1).NE.iCell ) THEN
               iError   =5
               nFailures = nFailures+1
               IF(level.EQ.1) 
     $              WRITE(mout,*) '##### iCell= ',iCell,' First  Daugter not pointing to parent Dau1= ',Dau1 !
            ENDIF
         ENDIF
         IF( Dau2.GE.1 .AND. Dau2.LE. m_LastCe) THEN
            IF( m_CePare(Dau2).NE.iCell ) THEN
               iError   =6
               nFailures = nFailures+1
               IF(level.EQ.1) 
     $              WRITE(mout,*) '##### iCell= ',iCell,' Second Daugter not pointing to parent Dau2= ',Dau2 !
            ENDIF
         ENDIF 
      ENDDO
* check on vertices
      DO iVe = 1, m_LastVe
         NoRefs(iVe)=0
      ENDDO
      DO iVe = 1, m_LastVe
         DO iCell = 1, m_LastCe
            DO n=1,m_nDim+1
               IF( iVe .EQ. m_CeVert(iCell,n) ) NoRefs(iVe) =1
            ENDDO
         ENDDO
      ENDDO
      DO iVe = 1, m_LastVe
         IF(NoRefs(iVe).EQ.0 .AND.  level.EQ.1) WRITE(mout,*) '***** Vertex no. ',iVe, '  NOT referenced!' !
      ENDDO
* Check for empty cells
      NoEmpty = 0d0
      DO iCell = 1,m_LastCe
         IF( m_CeStat(iCell).EQ.1 ) THEN
            IF( m_CeDriv(iCell) .EQ. 0d0) NoEmpty = NoEmpty +1
         ENDIF
      ENDDO
      IF( NoEmpty.GT.0) THEN
         WRITE(mout,*) '++++++++++ FoamA_Check: !!! WARNING!!!! Empty Cells found NoEmpty= ',NoEmpty !
         WRITE(   *,*) '++++++++++ FoamA_Check: !!! WARNING!!!! Empty Cells found NoEmpty= ',NoEmpty !
      ENDIF
* summary
      IF(level.EQ.1) WRITE(mout,*) '++++++++++ FoamA_Check has found total ', nFailures, ' failures ' !
      IF(level.EQ.1) WRITE(mout,*)
     $'///////////////////////////////////////////////////////////////////////////////////////////////////' !
      IF(level.EQ.0 .AND. nFailures.GT.0 ) THEN
         WRITE(mout,*) '++++++++++ STOP in FoamA_Check, found total ', nFailures, ' failures ' !
         WRITE(mout,*) '++++++++++ iErro= ',iError
         STOP
      ENDIF
      END                       ! FoamA_Check



      SUBROUTINE FoamA_BufPrint(mout)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   all cells                                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            iCell,mout,active,j
*     ----------------------------------------------------------------
      active  =0
      WRITE(mout,'(3a)') '==============================================', ' ALL-CELLS ',
     $                   '=============================================='
      WRITE(mout,'(3a)') 
     $     ' iCell  Stat  Pare  Dau1  Dau2 kBest       Xave     Volume      Drive  TrueInteg  Ver1  Ver2  ...'
      DO iCell = 1, m_LastCe
         WRITE(mout,'(6i6,4f11.5,20i6)')
     $        iCell, m_CeStat(iCell),  m_CePare(iCell), m_CeDau1(iCell), m_CeDau2(iCell), !
     $        m_CeBest(iCell),                          ! pointer to best division
     $        m_CeXave(iCell),                          ! factor for Best division 
     $        m_CeVolu(iCell),                          ! Cartesian Volume
     $        m_CeDriv(iCell),                          ! Drive 
     $        m_CeIntg(iCell),                          ! TrueInteg
     $        (m_CeVert(iCell,j), j=1,m_nDim+1)         ! vertices
         IF(m_kDim .GT. 0 ) THEN
            WRITE(mout,'(a,(50g11.6))')
     $           '       HypCubs Posit&Size: ',
     $           (m_CeVer1(iCell,j), j=1,m_kDim), ! position
     $           (m_CeVer2(iCell,j), j=1,m_kDim)  ! size
         ENDIF
         IF(m_CeStat(iCell).EQ.1) active  = active +1
      ENDDO
      WRITE(mout,*) ' All cells: ',m_LastCe, ' Active: ', active
      END                       !! FoamA_BufPrint



      SUBROUTINE FoamA_BufActPrint(mout)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   Active cells only                                                              //
*//   Side=1 indicates that this cell is "side leaf" sticking out of main branch     //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            iCell,mout,active,Side,Pare,j
      DOUBLE PRECISION   DriRat,IntgSum,DrivSum,Fact,WtMin,WtMax
      DOUBLE PRECISION   AveWt, Sigma, Nentry
*     ----------------------------------------------------------------
      WRITE(mout,'(3a)') '==================================================', ' ACTIVE CELLS ',
     $                   '=================================================='
      IntgSum =0d0
      DrivSum =0d0
      WtMin   =  1d60
      WtMax   = -1d60
      active  =0
      WRITE(mout,'(2a)') ' iCell Stat Pare Dau1 Dau2',
     $   '     WtMin      WtMax        <w>  sigma/<w>     Volume      Drive    TrueInt   Ver1  Ver2 ...' !
      DO iCell = 1, m_LastCe
         IF(m_CeStat(iCell).EQ.1) THEN
            Nentry = m_CeSum(iCell,3)
            AveWt  = m_CeSum(iCell,1)/m_CeSum(iCell,3)
            Sigma  = DSQRT(  ABS(m_CeSum(iCell,2)/Nentry - AveWt**2))
            IF(AveWt.NE.0d0) WtMin = Min( WtMin, m_CeSum(iCell,4)/AveWt)
            IF(AveWt.NE.0d0) WtMax = Max( WtMax, m_CeSum(iCell,5)/AveWt)
            IF(AveWt.NE.0d0) Sigma = Sigma/AveWt
            WRITE(mout,'(5i5, 7f11.5 ,10i5)') 
     $           iCell, m_CeStat(iCell),  m_CePare(iCell),  m_CeDau1(iCell), m_CeDau2(iCell), !
     $           m_CeSum(iCell,4),       ! minWt/AveWt
     $           m_CeSum(iCell,5),       ! maxWt/AveWt
     $           m_CeIntg(iCell) /(m_CeDriv(iCell)+1d-100), ! average weight
     $           Sigma/(AveWt+1d-100),   ! sigma/AveWt
     $           m_CeVolu(iCell),        ! Cartesian volume
     $           m_CeDriv(iCell),        ! Driver Integral
     $           m_CeIntg(iCell),        ! True   Integral
     $           (m_CeVert(iCell,j), j=1,m_nDim+1) ! vertices
            IF(m_kDim .GT. 0 ) THEN
               WRITE(mout,'(a,(50g11.6))')
     $              '      HypCubs Posit&Size: ',
     $              (m_CeVer1(iCell,j), j=1,m_kDim), ! position
     $              (m_CeVer2(iCell,j), j=1,m_kDim)  ! size
            ENDIF
            IntgSum = IntgSum +m_CeIntg(iCell)
            DrivSum = DrivSum +m_CeDriv(iCell)
            active  = active +1
         ENDIF
      ENDDO
      WRITE(mout,'(a,i6,a,2i6)') 'All cells: ',m_LastCe, '      Active: ', active, m_LastAc !
      WRITE(mout,'(a,2f12.5)')   'Minimum and Maximum Weight/<Wt>       = ',WtMin,WtMax !
      WRITE(mout,'(a,2g20.13)')  'Total True Integral in active cells   = ', IntgSum, m_CeIntg(1) !
      WRITE(mout,'(a,2g20.13)')  'Total Driver Integral in active cells = ', DrivSum, m_CeDriv(1) !
      WRITE(mout,'(a,f12.5)')    'True/Drive = ', IntgSum/DrivSum
      END                       !! FoamA_BufActPrint


      SUBROUTINE FoamA_VertPrint(mout)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   all vertices                                                                   //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            mout, iCell, iVe, NoRefs(m_vMax), NoRefsAc(m_vMax), k,j
*     ----------------------------------------------------------------
      DO iVe = 1, m_LastVe
         NoRefs(iVe)=0
      ENDDO
      DO iVe = 1, m_LastVe
         DO iCell = 1, m_LastCe
            DO k=1,m_nDim+1
               IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefs(iVe) =NoRefs(iVe) +1 !
               IF(m_CeStat(iCell) .EQ. 1) THEN
                  IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefsAc(iVe) =NoRefsAc(iVe) +1 !
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      WRITE(mout,*) '=====================ALL VERTICES===================================' !
      WRITE(mout,*) ' iVert   NoRefs  NoRefsAc       Vertex     Componets    ' !
      DO iVe = 1, m_LastVe
         WRITE(mout,'(i6,2i10,5f17.10)') iVe,NoRefs(iVe),NoRefsAc(iVe), (m_VerX(iVe,j),j=1,m_nDim) !
      ENDDO
      END                       !! VertPrint



      SUBROUTINE FoamA_PltBegin(ltx)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Initialization, write header of TeX file                                       //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER   ltx
*---------------------------------------------------
      m_ltx  = ABS(ltx)
      IF(ltx .GT. 0 ) THEN
         WRITE(m_ltx,'(2A)') '\\newpage'
      ELSE
*------------------------------!
*           Header
*------------------------------!
         WRITE(m_ltx,'(A)') '\\documentclass[12pt]{article}'
         WRITE(m_ltx,'(A)') '\\usepackage{color}' !<-for colors!!!
         WRITE(m_ltx,'(A)') '\\usepackage{epic}' !<-for extended ploting
         WRITE(m_ltx,'(A)') '\\textwidth  = 16cm'
         WRITE(m_ltx,'(A)') '\\textheight = 18cm'
         WRITE(m_ltx,'(A)') '\\pagestyle{empty}'
         WRITE(m_ltx,'(A)') '\\begin{document}'
         WRITE(m_ltx,'(A)') '  '
         WRITE(m_ltx,'(A)') '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' !
         WRITE(m_ltx,'(A)') '\\begin{figure}[!ht]'
         WRITE(m_ltx,'(A)') '\\centering'
*------------------------------!
* Frames and labels
*------------------------------!
         WRITE(m_ltx,'(A)') '% =========== big frame, title etc. ======='
         WRITE(m_ltx,'(A)') '\\setlength{\\unitlength}{0.1mm}'
         WRITE(m_ltx,'(A)') '\\begin{picture}(1600,1600)'
         WRITE(m_ltx,'(A)') '\\put(0,0){\\framebox(1600,1600){ }}'
      ENDIF
      END

      SUBROUTINE FoamA_PltVert     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Plot all vertices                                                              //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            iCell, iVe, NoRefs(m_vMax), NoRefsAc(m_vMax), k,j
*------------------------------------------------------------------------------
*     Mark plots for plots
      CHARACTER*62 star,diamond,circle,ring,times,disc,plus,box,dot
      PARAMETER (diamond ='\\makebox(0,0){\\Large $\\diamond$}')
      PARAMETER (star    ='\\makebox(0,0){\\Large $\\star$}')
      PARAMETER (circle  ='\\circle{30}')
      PARAMETER (ring    ='\\circle{20}')
      PARAMETER (times   ='\\makebox(0,0){\\Large $\\times$}')
      PARAMETER (disc    ='\\circle*{20}')
      PARAMETER (plus    ='\\makebox(0,0){\\Large $+$}')
      PARAMETER (box     ='\\makebox(0,0){\\Large $\\Box$}') !!! does not work???
      PARAMETER (dot     ='\\circle*{10}')
*------------------------------------------------------------------------------
      CHARACTER*62  chmark
      INTEGER       kx,ky
*---------------------------------------------------------------------------------------------
      IF(m_nDim.EQ.2) THEN
* Count references of vertices
         DO iVe = 1, m_LastVe
            NoRefs(iVe)=0
         ENDDO
         DO iVe = 1, m_LastVe
            DO iCell = 1, m_LastCe
               DO k=1,m_nDim+1
                  IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefs(iVe) =NoRefs(iVe) +1 !
                  IF(m_CeStat(iCell) .EQ. 1) THEN
                     IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefsAc(iVe) =NoRefsAc(iVe) +1 !
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
*---------------------------------------------------------------------------------------------
* Begin frame
         WRITE(m_ltx,'(A)') '% =========== Vertices Vertices ==========='
         WRITE(m_ltx,'(A)') '\\put(0,0){\\begin{picture}( 1600,1600)' !
         WRITE(m_ltx,'(A)') '\\put(0,0){\\framebox( 1600,1600){ }}' !
* Plotting symbol
         IF(m_LastVe.LE.250) THEN
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VD}[2]{\\put(#1,#2){',disc,'}}' !
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VS}[2]{\\put(#1,#2){',star,'}}' !
         ELSE
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VD}[2]{\\put(#1,#2){',dot,'}}' !
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VS}[2]{\\put(#1,#2){',dot,'}}' !
         ENDIF
         WRITE(m_ltx,'(10A)') 
     $        '\\newcommand{\\VN}[3]{\\put(#1,#2){\\makebox(0,0)[b]{\\hbox{\\color{red}\\scriptsize #3}}}}' !
         DO iVe = 1, m_LastVe
            kx = m_VerX(iVe,1)*1600
            ky = m_VerX(iVe,2)*1600
*****         WRITE(*,*) NoRefs(iVe),NoRefsAc(iVe)
            IF( NoRefsAc(iVe).LE.2 ) THEN
               WRITE(m_ltx,'(A,I5,A,I5,A)') '\\VD{',kx,'}{',ky,'}'
            ELSE
               WRITE(m_ltx,'(A,I5,A,I5,A)') '\\VS{',kx,'}{',ky,'}'
            ENDIF            
            IF(m_LastVe.LE.250) WRITE(   m_ltx,'(A,I5,A,I5,A,I5,A)') '\\VN{',kx-8,'}{',ky+12,'}{',iVe,'}' !
         ENDDO
* Close frame
         WRITE(m_ltx,'(A)') '\\end{picture}}'
         WRITE(m_ltx,'(A)') '% ============== End Vertices ==========='
      ENDIF
*---------------------------------------------------------------------------------------------
      END                       !! VertPrint


      SUBROUTINE FoamA_PltCell     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Plot all simplectic cells                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
      INTEGER            iCell,active,j
      INTEGER            iV1,iV2,iV3
      INTEGER            kx1,ky1,kx2,ky2,kx3,ky3,kx,ky,lx,ly
*---------------------------------------------------------------------------------------------
*                        Rectangular 2-dim FOAM
*---------------------------------------------------------------------------------------------
      IF(m_kDim.EQ.2) THEN
         WRITE(m_ltx,'(A)') '% =========== Vertices Vertices ==========='
         WRITE(m_ltx,'(A)') '\\put(0,0){\\begin{picture}( 1600,1600)' !
         DO iCell = 2, m_LastCe
            IF(m_CeStat(iCell).EQ.1) THEN ! Only active cells
               kx = m_CeVer1(iCell,1)*1600
               ky = m_CeVer1(iCell,2)*1600
               lx = m_CeVer2(iCell,1)*1600
               ly = m_CeVer2(iCell,2)*1600
               kx1 = kx+lx/2
               ky1 = ky+ly/2
*     cell rectangle
               WRITE(m_ltx,'(A,I5,A,I5,A,I5,A,I5,A)') 
     $              '\\put(',kx,',',ky,'){\\color{black}\\framebox(',lx,',',ly,'){ }}' !
***               WRITE(m_ltx,'(A,I5,A,I5,A,I5,A,I5,A)') 
***     $              '\\put(',kx,',',ky,'){\\color{black}\\dashbox{7}(',lx,',',ly,'){ }}' !
*     cell number
               IF(m_LastCe.LE.250) WRITE(m_ltx,'(A,I4,A,I4,A,I4,A)') 
     $            '\\put(',kx1,',',ky1,'){\\makebox(0,0)[b]{\\hbox{\\small\\color{magenta}\\scriptsize ',iCell,' }}}' !
            ENDIF
         ENDDO
* Close frame
         WRITE(m_ltx,'(A)') '\\end{picture}}'
         WRITE(m_ltx,'(A)') '% ============== End Vertices ==========='
      ENDIF
*---------------------------------------------------------------------------------------------
*                        Triangulare 2-dim FOAM
*---------------------------------------------------------------------------------------------
      IF(m_nDim.EQ.2) THEN
         WRITE(m_ltx,'(A)') '% =========== Vertices Vertices ==========='
         WRITE(m_ltx,'(A)') '\\put(0,0){\\begin{picture}( 1600,1600)' !
         DO iCell = 2, m_LastCe
            iV1=m_CeVert(iCell,1)
            iV2=m_CeVert(iCell,2)
            iV3=m_CeVert(iCell,3)
            kx1 = m_VerX(iV1,1)*1600
            ky1 = m_VerX(iV1,2)*1600
            kx2 = m_VerX(iV2,1)*1600
            ky2 = m_VerX(iV2,2)*1600
            kx3 = m_VerX(iV3,1)*1600
            ky3 = m_VerX(iV3,2)*1600
            kx= (kx1+kx2+kx3)/3
            ky= (ky1+ky2+ky3)/3
            IF(m_CeStat(iCell).EQ.1) THEN
***         WRITE(*,*) iCell,iV1,iV2,iV3
               WRITE(m_ltx,'(A,I4,A,I4,A,I4,A,I4,A)') '\\drawline(',kx1,',',ky1,')(',kx2,',',ky2,')' !
               WRITE(m_ltx,'(A,I4,A,I4,A,I4,A,I4,A)') '\\drawline(',kx2,',',ky2,')(',kx3,',',ky3,')' !
               WRITE(m_ltx,'(A,I4,A,I4,A,I4,A,I4,A)') '\\drawline(',kx3,',',ky3,')(',kx1,',',ky1,')' !
               IF(m_LastCe.LE.250) WRITE(m_ltx,'(A,I4,A,I4,A,I4,A)') 
     $              '\\put(',kx,',',ky,'){\\makebox(0,0)[b]{\\hbox{\\color{magenta}\\scriptsize ',iCell,' }}}' !
            ENDIF
         ENDDO
* Close frame
         WRITE(m_ltx,'(A)') '\\end{picture}}'
         WRITE(m_ltx,'(A)') '% ============== End Vertices ==========='
      ENDIF
      END                       !! FoamA_BufPrint

      SUBROUTINE FoamA_PltMisc     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   Miscelaneous                                                                   //
*//   Plot of frame for 2-dim void testing function                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
*
      WRITE(m_ltx,'(A)') '\\put(80,80){\\color{blue}\\dashbox{7}( 1440,1440){ }}' ! 5% edge band
      END                       !! FoamA_PltMisc

      SUBROUTINE FoamA_PltEnd     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Close Tex file with plot                                                       //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamA.h'
*
      WRITE(m_ltx,'(A)') '\\end{picture}'
      WRITE(m_ltx,'(A)') '\\end{figure}'
      WRITE(m_ltx,'(A)') '\\end{document}'
      CLOSE(m_ltx)
      END

*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
*///////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                       //
*//             Foam Version 2.02                                                         //
*//             November 2000                                                             //
*//                                                                                       //
*//  N-dimensional general purpose Monte Carlo sampler                                    //
*//  with the self-adapting simplical and hyper-cubical grid                              //
*//                                                                                       //
*//             Author:   Stanislaw JADACH                                                //
*//             Address:  INP Cracow, DESY-Zeuthen                                        //
*//             Email:    S.Jadach@cern.ch, S.Jadach@ifj.edu.pl                           //
*//             HomePage: http://home.cern.ch/jadach/                                     //
*//                                                                                       //
*//  First version 1.00 written by S.J. in May 1999 during visit in DESY                  //
*//        version 2.00 written by S.J. in Aug 2000 during visit in DESY-Zeuthen          //
*///////////////////////////////////////////////////////////////////////////////////////////


*////////////////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                                    //
*//          Pseudoclass Foam                                                                          //
*//                                                                                                    //
*//                                                                                                    //
*//                      Initialization of the grid                                                    //
*//  FoamB_PreInitialize                 : Pre-initialization, set all default values (constructor?)   //
*//  FoamB_Initialize(FunW)              : Initialization of the grid etc.                             //
*//  FoamB_InitVertices                  : Initializes first vertices of the basic cube                //
*//  FoamB_InitCells                     : Initializes first n-factorial cells inside original cube    //
*//  FoamB_DefCell                       : Create new (daughter) cell and append at end of the buffer  //
*//  FoamB_Explore(iCell,funW)           : Short MC sampling in iCell, determine <wt>, wtMax etc.      //
*//  FoamB_RanDiscr(Driv,nTot,Power,iRand) : Random choice of cell division direction                  //
*//  FoamB_MakeAlpha(Alpha)              : auxiliary procedure for FoamB_Explore                       //
*//  FoamB_MakeLambda(Lambda)            : auxiliary procedure for FoamB_Explore                       //
*//  FoamB_Determinant(R,Det)            : determinant of matrix R                                     //
*//  FoamB_Det2Lapl(R,i1,i2)             : Laplace formula for 1-dim. determinant                      //
*//  FoamB_Det3Lapl(R,i1,i2,i3)          : Laplace formula for 2-dim. determinant                      //
*//  FoamB_Det4Lapl(R,i1,i2,i3,i4)       : Laplace formula for 3-dim. determinant                      //
*//  FoamB_Det5Lapl(R,i1,i2,i3,i4,i5)    : Laplace formula for 4-dim. determinant                      //
*//  FoamB_Grow(funW)              : grow cells until buffer is full                                   //
*//  FoamB_PeekMax(iCell)          : choose randomly one cell, used also in MC generation              //
*//  FoamB_PeekRand1(iCell)        : Generates randomly one (active) cell pointer iCell                //
*//  FoamB_Divide(iCell,funW,RC)   :Divide iCell into two daughters; iCell retained, taged as inactive //
*//  FoamB_ActUpda                 : Creates list of active cells (pointers)                           //
*//                     Generation                                                                     //
*//  FoamB_MakeEvent(Density)      : Generates point/vector Xrand with the weight MCwt                 //
*//  FoamB_CellGener(Density)      : Cooses randomly one of active cells                               //
*//  FoamB_GetMCvector(MCvector)   : Provides point/vector MCvector generated by  MakeEvent            //
*//  FoamB_GetMCwt(MCwt)           : Provides MCwt, MC weight calculated by MakeEvent                  //
*//  FoamB_MCgenerate(funW,X,MCwt) : Alternative entry, Generates point X with the weight MCwt         //
*//                     Finalization                                                                   //
*//  FoamB_Finalize(MCresult,MCerror)    : Calculates integral and its error after (only from) MC run  //
*//                     Other Getters and Setters                                                      //
*//  MCellA_GetTotPrim(TotPrim)    :Provides Primary integral used in MC generation                    //
*//  FoamB_SetnDim(nDim)           :Sets no. of dimensions simplices   (to be called before Initialize)//
*//  FoamB_SetkDim(kDim)           :Sets no. of dimensions hypercubics (to be called before Initialize)//
*//  FoamB_GetnDim(nDim)           :Provides nDim, miscelaneous, for tests                             //
*//  FoamB_SetnBuf(nBuf)           :Sets nBuf, working area in buffer                                  //
*//  FoamB_SetnBin(nBin)           :Sets nBin, no of bins in histo in exploration <nBinMax=256         //
*//  FoamB_SetOut(Out)             :Sets output unit number                                            //
*//  FoamB_SetChat(Chat)           :Sets chat level; Chat=0,1,2 chat level in output, Chat=1 normal    //
*//  FoamB_SetnSampl(nSampl)       :Sets nSampl; No of MC sampling before dividing cell                //
*//  FoamB_SetOptDrive(OptDrive)   :Sets OptDrive; type of Drive =0,1,2 for True,Sigma,WtMax           //
*//  FoamB_SetOptPeek              :Sets type of method in cell division                               //
*//  FoamB_SetOptEdge(OptEdge)     :Sets OptEdge; decides whether vertices are included in the sampling//
*//  FoamB_SetOptRanIni(OptRanIni) :Sets OptRanIni=0,1, for rand. numb. initialization inside/outside  //
*//  FoamB_SetOptRanLux(OptRanLux) :Sets OptRanLux=-1,0,1,2,3 raand.numb.gen. level                    //
*//  FoamB_GetnCalls(nCalls)       :Get total no of function calls                                     //
*//                    Debugging and miscelaneous                                                      //
*//  FoamB_Check(mout,level)       :Checks all pointers (after comression) debuging!                   //
*//  FoamB_BufPrint(mout)          :Prints all cells, debugging                                        //
*//  FoamB_BufActPrint(mout)       :Prints all active cells, debugging                                 //
*//  FoamB_VertPrint(mout)         :Prints all vertices,  debugging                                    //
*//  FoamB_PltBegin                :Ploting 2-dim. cells and vertices                                  //
*//  FoamB_PltVert(mout)           :Ploting 2-dim. cells and vertices                                  //
*//  FoamB_PltCell(mout)           :Ploting 2-dim. cells and vertices                                  //
*//  FoamB_PltEnd                  :Ploting 2-dim. cells and vertices                                  //
*//                                                                                                    //
*////////////////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                                    //
*//  Input parameters:                                                                                 //
*//    nDim     number of dimensions for simplices, for the moment nDim=1-5 in this version,           //
*//             n>5 requires re-writing FoamB_Determinant,                                             //
*//    kDim     number of dimensions for hypercubics                                                   //
*//    Dimen    total number of dimensions = nDim +kDim                                                //
*//             for n=1 alternatively Foam1A may be used, could be factor 2 faster!                    //
*//    nBuf     Actual dynamic lenth of the buffer m_nBuf<m_nBufMax. For strongly peaked distribution  //
*//             nBuf should be as large as possible, this will increase CPU time in initialization     //
*//             MC generation is weakly affected by increasing nBuf                                    //
*//    nSampl   No of sampling when dividing cell, nSampl=10-100 is OK, further increase improves      //
*//             costs CPU time and apparently does not increase grid efficiency too much.              //
*//             This should be checked however for every new distribution.                             //
*//    OptDrive Type of Drive =0,1,2 for TrueInt,Sigma,WtMax,  Drive=WtMax is the best if we aim       //
*//             at rejection leading to wt=1 events. If not then Drive=Sigma iswiser choice leading    //
*//             to save of CPU time.  Simplistic Drive=Sigma is correct but not recommeneded (ineffic.)//
*//    OptEdge  decides whether vertices are included in the sampling. Default  OptEdge=1 causes that  //
*//             vertices at the edge of simplex cells are included always in MC exploration            //
*//             of the cell. In the case of density distrib. with weak integrable singularities        //
*//             at the edges it may be not possible and OptEdge=0 may help.                            //
*//    OptOrd decides whether the integration domain in simplical subspace is unit cube or simplex.    //
*//             It is active only for  nDim=2,3,4. For OptOrd=0 (default) the initial domain is unit   //
*//             hypercubic split into nDim! simplices in the initialization phase.                     //
*//             If m_OptOrd=1 then initial domain is simplex and the integration variables providede   //
*//             by FoamB_GetMCvector(V) are ordered: V(1)<V(2)<...<V(nDim)<1.                          //
*//             Hyp-cubical variables  0<V(nDim+1), V(nDim+2),...,V(nDim+kDim)<1 are never ordered.    //
*//    EvPerBin Limit on effective number of eevents per bin in exploration phase. This option saves   //
*//             considerably CPU time.      =0 option is desabled, recommended value is >20.           //
*//    Out      Miscelaneous. Output unit number.                                                      //
*//    Chat     Miscelaneous. Chat=0,1,2 chat level in output, Chat=1 normal level.                    //
*//                                                                                                    //
*//                                                                                                    //
*////////////////////////////////////////////////////////////////////////////////////////////////////////
*//  Terminology:                                                                                      //
*//    "Active cells" are these which did not divide and are eligible for division                     //
*//  Remarks:                                                                                          //
*//    List of active cells is not realy necessary, but let us keep it for possible                    //
*//    future developements or tests.                                                                  //
*////////////////////////////////////////////////////////////////////////////////////////////////////////


      SUBROUTINE FoamB_PreInitialize     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Presets certain defaults for switches and other and regualtory parameters.     //
*//   They Can be reset with setters                                                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER i,j
*
      INTEGER   m_Magic        ! Magic cookie, to avoid multiple initialization
      DATA      m_Magic /378231178/
      SAVE      m_Magic
*     -------------------------------------------------
      IF(m_Magic .EQ. 378231178 ) THEN
         m_MagicInit = m_Magic
         m_Magic     = 0
      ENDIF
      IF(m_MagicInit .NE. 378231178 ) RETURN
      WRITE(m_out,*) '===============FoamB_PreInitialize=============='
      m_MagicInit= 0
      m_nDim     = 0                 ! dimension siplices
      m_kDim     = 0                 ! dimension hypercubics
      m_Dimen    = m_nDim+m_kDim     ! dimension total
      m_nBuf     = 1000              ! Actual dynamic lenth of the buffer m_nBuf<m_nBufMax
      m_nBin     = 8                 ! No of bins in cell exploration/division  <nBinMax  
      m_nSampl   = 200               ! No of sampling when dividing cell
      m_OptDrive = 2                 ! type of Drive =0,1,2 for TrueVol,Sigma,WtMax
      m_OptEdge  = 0                 ! decides whether vertices are included in the sampling
      m_OptPeek  = 0                 ! type of Peek =0,1 for maximum, random
      m_Out      = 6                 ! Output unit
      m_Chat     = 1                 ! Chat=0,1,2 chat level in output, Chat=1 normal level
      m_OptOrd   = 0                 ! Entire integr domain is hyp-cubic if =0 and simplex if =1
      m_EvPerBin = 25                ! Upper limit on eevents/bin in exploration, =0 disabled 
      m_OptRanIni= 1                 ! Rand.num. generator initialized (=1) inside PianA
      m_OptRanLux= 3                 ! Rand.num. generator level (-1 for PseuMar)
*
      m_LastCe   = 0                 ! length of dynamical buffor for cells
      m_LastVe   = 0                 ! length of dynamical buffor for vertices
* Clean up list of simplical vertices
      DO i=1,m_vMax
         DO j=1,m_nDim
            m_VerX(i,j) = 0d0        ! vertices
         ENDDO
      ENDDO
* Clean up hypercubic parameters
      DO i=1,m_nBufMax
         DO j=1,m_KdiMax
            m_CeVer1(i,j)=0d0        ! hcubic position
            m_CeVer2(i,j)=0d0        ! hcubic size
         ENDDO
      ENDDO
*
      m_nCalls   = 0                 ! No of function calls
*
      END

      SUBROUTINE FoamB_Flush        !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////*//                                                                                  //
*//   Use this before re-initialization of the Foam                                  //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      m_MagicInit = 378231178   ! Reset Magic cookie, alowing initialization
      WRITE(m_out,*) '===============FoamB_Flush=============='
      CALL FoamB_PreInitialize
      END

      SUBROUTINE FoamB_Initialize(FunW)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Basic initialization, create "foam of cells"                                   //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            i,j,k,iCell,kCell
      DOUBLE PRECISION   TotPrim
*---------------------------------------------
      CALL FoamB_PreInitialize
* User may prefers to initialize r.n.gen. by himsef, the initialization below can be inhibited 
      IF( m_OptRanIni.EQ. 1 ) THEN
         IF(m_OptRanLux.EQ.-1) THEN
            CALL PseuMar_Initialize(54217137, 0, 0) ! Initialization of random number generator
         ELSEIF(m_OptRanLux.GE.0 .AND. m_OptRanLux.LE.4 ) THEN
            CALL RLUXGO( m_OptRanLux, 54217137,0,0) ! Initialization of random number generator
         ELSE
            WRITE(*,    *) ' ### STOP in FoamB_Initialize, wrong OptRanLux =',m_OptRanLux !
            WRITE(m_Out,*) ' ### STOP in FoamB_Initialize, wrong OptRanLux =',m_OptRanLux !
         ENDIF
      ENDIF
      IF( (m_nBuf .GT. m_nBufMax) .OR. (m_nBuf .LE. 0) ) THEN
         WRITE(*,    *) ' ### STOP in FoamB_Initialize, wrong m_nBuf =',m_nBuf !
         WRITE(m_Out,*) ' ### STOP in FoamB_Initialize, wrong m_nBuf =',m_nBuf !
         STOP
      ENDIF
      IF( (m_nBin .GT. m_nBinMax) .OR. (m_nBin .LE. 0) ) THEN
         WRITE(*,    *) ' ### STOP in PianA_Initialize, wrong m_nBin =',m_nBin !
         WRITE(m_Out,*) ' ### STOP in PianA_Initialize, wrong m_nBin =',m_nBin !
         STOP
      ENDIF
      IF( m_Dimen .LE. 0 ) THEN
         WRITE(*,    *) ' ### STOP in FoamB_Initialize, wrong m_Dimen =',m_Dimen !
         WRITE(m_Out,*) ' ### STOP in FoamB_Initialize, wrong m_Dimen =',m_Dimen !
         STOP
      ENDIF
* First  cells are the (nDim)! simplices from division of the basic unit cube
      CALL FoamB_InitVertices
      CALL FoamB_InitCells(funW)

***** CALL FoamB_VertPrint(6)   ! debug
***** CALL FoamB_BufPrint(6)    ! debug

      CALL  FoamB_Grow(funW)
* Update list of active cells, only for internal tests
      CALL FoamB_ActUpda
      CALL FoamB_Check(6,0)     ! Check if the liked list is OK
* --------------------------------------------------------------------------------------------
      IF( m_Chat.GE.1) THEN
         WRITE(m_Out,'( 3(a,i4),2(a,g15.8) )') 
     $      'Foam_Initialize:  No. Cells=',m_LastCe,'  Active=',m_LastAc, '  No. Vertices=' ,m_LastVe, !
     $      '  True Integ.=',m_CeIntg(1),'  Driver Integ.=',m_CeDriv(1)  !
      ENDIF
      IF( m_Chat.EQ.2) THEN
         CALL FoamB_BufPrint(    m_Out)
         CALL FoamB_BufActPrint( m_Out)
         CALL FoamB_VertPrint(   m_Out)
      ENDIF
* Initializations for M.C. generation
      m_Drive  = m_CeDriv(1)  ! M.C. generation Drive value of integral
      m_SumWt  = 0d0          ! M.C. generation sum of Wt
      m_SumWt2 = 0d0          ! M.C. generation sum of Wt**2
      m_NevGen  = 0d0         ! M.C. generation sum of 1d0
      m_WtMax  = -1d99        ! M.C. generation maximum wt
      m_WtMin  =  1d99        ! M.C. generation minimum wt
      m_VolTot = m_CeIntg(1)  ! Estimate of integral tot. without error
      m_MCresult = m_VolTot   ! M.C. generation Final value of ITEGRAL, temporary asignment
      m_MCerror  = m_VolTot   ! M.C. generation Final walue of ERROR  , temporary asignment
*
*((((((((((((################################################################################
*((((((((((((################################################################################
c      CALL FoamB_VertPrint(  6)
c      CALL FoamB_BufPrint(   6)
c      CALL FoamB_BufActPrint(6)
c      CALL FoamB_Check(      6,0)
c      WRITE(*,*) ' ########### STOP ########### developement not finished @@@@' !
c      STOP
*))))))))))))################################################################################
*))))))))))))################################################################################
*
* Cumulative Primary for MC generation, see FoamB_CellGener
      CALL FoamB_ActUpda
      TotPrim = 0d0
      m_CePrCu(0)= 0d0
      DO kCell = 1,m_LastAc
         iCell  = m_ActC(kCell)
         TotPrim = TotPrim +m_CePrim( iCell )
         m_CePrCu(kCell)=TotPrim
      ENDDO
* ----------------------------------------------------
      IF( m_Chat.GE.1) WRITE(m_Out,'( 2(a,g15.8) )')
     $     'Foam_Initialize:  TotPrimary =',TotPrim,'   True/Primary =', m_CeIntg(1)/TotPrim
      END                       ! FoamB_Initialize

      SUBROUTINE FoamB_InitVertices    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   In siplical subspace initialize 2^nDim vertices at corners of basic hyper-cube //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER       iVe,i,j,l,digit( m_NdiMax)
*
      IF( m_OptOrd .EQ.0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*// Here we have all vertices of the initial unit hypercube
*// like (0000),(1000),(0100),(1100),(0010)...(1111)
         m_LastVe = 2**m_nDim
         DO i=1,m_nDim
            digit(i)=0
         ENDDO
         DO iVe=1,m_LastVe
*****    WRITE(*,'(a,i5,a,10i5)') 'iVe=', iVe, '  digit=',(digit(j),j=1,m_nDim)
            DO i=1,m_nDim
               m_VerX(iVe,i) = digit(i) ! Simplical vertex positions
            ENDDO
            digit(1)=digit(1)+1         ! Basic increment
            DO i=1,m_nDim-1
               IF(digit(i).EQ.2) THEN   ! Overflow goes to higher digits
                  digit(i)=0
                  digit(i+1)=digit(i+1)+1
               ENDIF
            ENDDO
         ENDDO
      ELSE
*//////////////////////////////////////////////////////////////////////////////////////
*// Here we have only vertices of the SINGLE  INITIAL SIMPLEX 
*// like (0000),(1000),(1110),(1111)
         m_LastVe = m_nDim+1
         DO iVe=1,m_LastVe
            DO i=1,m_nDim
               m_VerX(iVe,i) =0
               IF(i.LT.iVe) m_VerX(iVe,i) =1  ! Simplical vertex positions
            ENDDO
*****       WRITE(*,'(a,i5,a,10f5.0)') 'iVe=', iVe, '  m_VerX=',(m_VerX(iVe,i),i=1,m_nDim)!
         ENDDO
      ENDIF
      END                       !! FoamB_InitVertices

      SUBROUTINE FoamB_InitCells(funW)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Initialize first n-factorial cells inside original cube                        //
*//   MC exploration done for all newly defined cells                                //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            perm(m_NdiMax),mask,iCeNew,iCell
      INTEGER            iCe,i,j,k,iVe,Vert(m_NdiMax),digit(m_NdiMax),factorial,NoMC !
      DOUBLE PRECISION   HcPosi(m_KdiMax),   HcSize(m_KdiMax)
*     -----------------------------------------------------------------------------
      DO j=1,m_kDim
         HcPosi(j)=0d0
         HcSize(j)=1d0
      ENDDO
      DO iVe=1,m_nDim+1
         Vert(iVe) =0
      ENDDO
      IF( m_nDim .EQ. 0 ) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//      Only hypercubics, no simplices                                              //
*//////////////////////////////////////////////////////////////////////////////////////
*        ROOT cell ACTIVE, no daughters
         NoMC= m_nSampl
*        -------------------- Stat,Pare, Dau1, Dau2, MCsampl,VertList, Position,Size,   iCeNew)
         CALL FoamB_DefCell(     1,  -1,   -1,   -1,    NoMC,   Vert, HcPosi,  HcSize, iCeNew) !
         CALL FoamB_Explore(iCeNew,funW) ! Initial MC sampling
      ELSEIF( m_nDim .EQ. 1 ) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//      Simplical algorithm in one dimension                                        //
*//////////////////////////////////////////////////////////////////////////////////////
         Vert(1) =1
         Vert(2) =2
*        ROOT cell ACTIVE, no daughters
*        -------------------- Stat,Pare, Dau1, Dau2,  MCsampl,VertList, Position,Size,   iCeNew
         CALL FoamB_DefCell(     1,  -1,   -1,   -1, m_nSampl,    Vert, HcPosi,  HcSize, iCeNew) !
         CALL FoamB_Explore(iCeNew,funW) ! Initial MC sampling
      ELSE
         IF( m_OptOrd .EQ.0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//      (nDim)! Simplices in more two and more dimensions                           //
*//////////////////////////////////////////////////////////////////////////////////////
            factorial=1
            DO k = 1,m_nDim
               factorial = factorial*k
            ENDDO
*           ROOT cell INACTIVE, (nDim)! daugter cells defined below
*           ----------------- Stat,Pare, Dau1,        Dau2,  MCsampl, VertList, Position,Size,   iCeNew)
            CALL FoamB_DefCell( -1,  -1,    2, factorial+1, m_nSampl,     Vert, HcPosi,  HcSize, iCeNew) !
* ================================================================
* START OF LOOP OVER permutations of (1,2,3,...,m_nDim)
            iCe=0
            DO i=1,m_nDim
               perm(i)=m_nDim
            ENDDO
 300        CONTINUE
            perm(1)=perm(1)-1
            DO k = 1,m_nDim-1
               IF(perm(k).EQ.0) THEN
                  perm(k)=m_nDim
                  perm(k+1)=perm(k+1)-1
               ENDIF
            ENDDO
            Mask=1
            DO i=1,m_nDim
               DO j=i+1,m_nDim
                  IF( perm(i).EQ.perm(j) ) Mask=0
               ENDDO
            ENDDO
            IF(Mask.EQ.1) THEN
*           At this point NEW PERMUTATION is found !!!
               iCe=iCe+1
               DO iVe=1,m_nDim+1
*              This digit represents single BASIC SIMPLEX, other obtained by permuting dimensions
                  DO k=1,m_nDim
                     digit(k)=0
                     IF(k.LT.iVe) digit(k)=1
                  ENDDO
*                 Translation from "binary" digit to serial pointer of a given vertex
                  j=0
                  DO k=1,m_nDim
                     j=j+  digit(perm(k)) *2**(k-1)
                  ENDDO
                  Vert(iVe)=j+1
               ENDDO
*****    WRITE(*,*) '########>>>>>>> iCe =',iCe, 'permut= ',(perm(i),i=1,m_nDim)
*****    WRITE(*,*) '########>>>>>>> Vert= ',(vert(i),i=1,m_nDim+1)
*              Define ACTIVE simplical cell
*              ----------------- Stat,Pare,Dau1,Dau2,  MCsampl,VertList, Position,Size,   iCeNew
               CALL FoamB_DefCell(  1,   1,  -1,  -1, m_nSampl,    Vert, HcPosi,  HcSize, iCeNew) !
*              ---------------------------------------------------------------------
               IF( iCe.EQ.factorial) GOTO 100 ! Last permutation found
            ENDIF
            GOTO 300            ! END OF LOOP over permutations
* ================================================================
 100        CONTINUE
            DO iCell = 2,m_LastCe
               CALL FoamB_Explore(iCell,funW) ! Initial MC sampling, necessarily in a separate loop
            ENDDO
         ELSE
*//////////////////////////////////////////////////////////////////////////////////////
*//      Single initial simplex in two or more dimension (ordered integr. variables) //
*//////////////////////////////////////////////////////////////////////////////////////
            DO k = 1,m_nDim+1
               Vert(k) =k
            ENDDO
*           ROOT cell ACTIVE, no daughters
*           -------------------- Stat,Pare, Dau1, Dau2,  MCsampl, VertList, Position,Size,    iCeNew)
            CALL FoamB_DefCell(     1,  -1,   -1,   -1, m_nSampl,     Vert, HcPosi,  HcSize,  iCeNew) !
            CALL FoamB_Explore(iCeNew,funW) ! Initial MC sampling
         ENDIF  ! m_OptOrd
      ENDIF  ! m_nDim
      END                       !!!FoamB_InitCells

      SUBROUTINE FoamB_DefCell(Stat,Pare,Dau1,Dau2,NoMC,Vertex,HcPosi,HcSize,iCeNew)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Create new (daughter) cell and append it at the very end of the buffer         //
*//   iCeNew is pointer of the new cell                                              //
*//   Note clever trick: volume of this daughter is assigned initialy half volume    //
*//   of the parent, if parent exists.                                               //
*//   In Explore this value is used to update all parents such that                  //
*//   in the entrire tree parents have volume being sum of all daughter volumes.     //
*//   This summation discipline is useful for MC generation of an active cell by     //
*//   going randomly from top to bottom of the tree.                                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            Stat,Pare,Dau1,Dau2,iCeNew,k,NoMC
      INTEGER            Vertex(m_NdiMax+1)
      DOUBLE PRECISION   HcPosi(m_KdiMax), HcSize(m_KdiMax)
*     ------------------------------------------------------------------
      IF( m_LastCe .EQ. m_nBuf) THEN
         WRITE(*,*) ' STOP in FoamB_DefCell: something wrong with m_nBuf=',m_nBuf !
         STOP
      ENDIF
      m_LastCe = m_LastCe+1
      iCeNew   = m_LastCe
      m_CeStat(iCeNew)= Stat                    ! status code, =0 inactive, =1 active
      m_CePare(iCeNew)= Pare                    ! parent cell pointer
      m_CeDau1(iCeNew)= Dau1                    ! daughter1 cell pointer
      m_CeDau2(iCeNew)= Dau2                    ! daughter2 cell pointer
      m_CeSamp(iCeNew)= NoMC                    ! No of MC events in exploration
      m_CeBest(iCeNew)= -1                      ! pointer for planning division of the cell
      m_CeXave(iCeNew)= 0.5d0                   ! factor for division
*  simplical subspace: vertex list
      DO k=1,m_NdiMax+1
         m_CeVert(iCeNew,k)= Vertex(k)
      ENDDO
*  hypercubical subspace: position and size
      DO k=1,m_kDim
         m_CeVer1(iCeNew,k) = HcPosi(k)
         m_CeVer2(iCeNew,k) = HcSize(k)
      ENDDO
*
      IF(Pare.NE.-1) THEN
         m_CeIntg(iCeNew)= m_CeIntg(Pare)/2d0   ! integr. half of parent
         m_CeDriv(iCeNew)= m_CeDriv(Pare)/2d0   ! integr. half of parent
      ELSE
         m_CeIntg(iCeNew)= 0d0
         m_CeDriv(iCeNew)= 0d0
      ENDIF
      m_CeVolu(iCeNew)= 0d0                     ! cartesian Volume   
      END                       ! FoamB_DefCell


      SUBROUTINE FoamB_Explore(iCell,funW)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Explore newly defined cell with help of special short MC sampling              //
*//   As a result, estimetes of true and Drive volume will be defined                //
*//   Average and dispersion of the weight distribution will be found along each     //
*//   edge and the best edge (minimum dispersion) is memorized for future use.       //
*//   Axerage x for eventual future cell division is also defined.                   //
*//   Recorded are aso minimum and maximu weight etc.                                //
*//   The volume estimate in all (inactive) parent cells is updated                  //
*//   Note that links to parents and initial volume = 1/2 parent has to be           //
*//   already defined prior to calling this routine.                                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            iCell
* Simplical subspace
      DOUBLE PRECISION   Vec(m_NdiMax+1,m_NdiMax)
      DOUBLE PRECISION   Xre(m_NdiMax,  m_NdiMax), Yre(m_NdiMax,  m_NdiMax) !
      DOUBLE PRECISION   Lambda(m_NdiMax),VolPart(m_NdiMax+1)
* Hyper-Cubic subspace
      DOUBLE PRECISION   Alpha( m_KdiMax)
      INTEGER            digit( m_KdiMax)
* Total space
      DOUBLE PRECISION   Vrand( m_DimMax)
* Edge sampling working space, matrices
      DOUBLE PRECISION   Xdivi(m_NdiviMax)
      DOUBLE PRECISION   Histo(m_NdiviMax,m_nBinMax),Xsu1(m_NdiviMax),Xsu2(m_NdiviMax) !
*--------
      INTEGER            loop,i,j,k,parent,iv,jv,kv,nEdges,kBest,iBin,nDivi,NevEff !
      DOUBLE PRECISION   x,x1,x2,Wt,Vsum,xBest,yBest
      DOUBLE PRECISION   Det,VolOld, DriOld, XrSum,Factorial
      DOUBLE PRECISION   Dx,DxHc,DxSp,DxPartial,NoMC
      DOUBLE PRECISION   funW
      EXTERNAL           funW
*-----------------------------------------------------------------------
* memorize old values, will be needed for correcting parent cells
      VolOld = m_CeIntg(iCell)
      DriOld = m_CeDriv(iCell)
*
      Factorial=1
      DO i=1,m_nDim
         Factorial=Factorial*i
      ENDDO
      DxSp=1d0          ! Cartesian volume of the simplex
      DxHc=1d0          ! Cartesian volume of the hypercube
*
      IF( m_nDim.GT.0) THEN
         DO iv=1,m_nDim+1
            DO j=1,m_nDim
               Vec(iv,j) = m_VerX( m_CeVert(iCell,iv) ,j) ! decode vertex vectors
            ENDDO
         ENDDO
         DO iv=1,m_nDim
            DO j=1,m_nDim
               Xre(iv,j) = Vec(iv,j)-Vec(m_nDim+1,j) ! relative to last vertex
            ENDDO
         ENDDO
         CALL FoamB_Determinant(Xre,Det)
         IF(Det.EQ.0d0) WRITE(*,*) '!!!!! WARNING: Determinant=',Det
         DxSp = DxSp*ABS(Det)/Factorial       ! Simplical Cartesian volume of the Cell
      ENDIF
      IF( m_kDim.GT.0) THEN
         DO i=1,m_kDim
            DxHc = DxHc*m_CeVer2(iCell,i) ! Product of sizes in hypercubical subspace
         ENDDO
      ENDIF
      Dx=DxSp*DxHc              ! Total cartesian volume
      m_CeVolu(iCell)  = Dx
c[[[[[[[[[[[[[[[
c      DO iv=1,m_nDim
c          WRITE(*,'(a,9f10.5)') '### Xre=',(Xre(iv,j),j=1,m_nDim)
c      ENDDO
c      WRITE(*,'(a,f12.6)') 'FoamB_Explore: Cartesian volume Dx =',Dx
c]]]]]]]]]]]]]]]

*/////////////////////////////////////////////////////
*//    Exploratory MC sampling to probe the cell    //
*/////////////////////////////////////////////////////
      m_CeSum(iCell,1) =  0
      m_CeSum(iCell,2) =  0
      m_CeSum(iCell,3) =  0
      m_CeSum(iCell,4) =  1d90  ! wtmin
      m_CeSum(iCell,5) = -1d90  ! wtmax
      DO k=1,m_NdiviMax
         Xsu1(k)=0d0
         Xsu2(k)=0d0
         DO iBin=1,m_nBin
            Histo(k,iBin)=0d0
         ENDDO
      ENDDO
*///////////////////////////////////////////////////////////////////////////////////
*     Additional scan over vertices in order to improve max/min weights
*     Note that this option adds 2**m_kDim of the function calls, limited to 1000!
      IF( m_OptEdge .EQ. 1 ) THEN
         DO k=1,m_kDim
            digit(k)=0          ! initialize first partitions
         ENDDO
         DO loop=1,1000         ! start of loop over partitions
            DO iv=1,m_nDim+1
               DO j=1,m_nDim
                  Vrand(j) = m_VerX( m_CeVert(iCell,iv) ,j) ! simplical subspace
               ENDDO
               DO j=1,m_kDim
                  Vrand(m_nDim+j) = m_CeVer1(iCell,j) + digit(j)*m_CeVer2(iCell,j) ! h-cubical subspace
               ENDDO
               Wt =funW(Vrand)*Dx ! weight average normalised to integral over the cell
               m_CeSum(iCell,4) = MIN( m_CeSum(iCell,4),Wt) ! minium weight
               m_CeSum(iCell,5) = MAX( m_CeSum(iCell,5),Wt) ! maximu weight
            ENDDO
            digit(1)=digit(1)+1
            DO k=1,m_kDim-1
               IF(digit(k).EQ.2) THEN
                  digit(k)=0
                  digit(k+1)=digit(k+1)+1
               ENDIF
            ENDDO
            IF( m_kDim .EQ.0 )      GOTO 122
            IF( digit(m_kDim).EQ.2) GOTO 122
         ENDDO                  ! end of loop over partitions
 122     CONTINUE
      ENDIF
*///////////////////////////////////////////////////////////////////////////////////
*     generate randomly/uniformly vector Vrand inside this simplex&hypercube
      DO i=1,m_CeSamp(iCell)
         CALL FoamB_MakeLambda(Lambda)
         CALL FoamB_MakeAlpha( Alpha)
         DO j=1,m_nDim
            Vrand(j) = Vec(m_nDim+1,j)
            DO iv=1,m_nDim
               Vrand(j) = Vrand(j) +Lambda(iv)*Xre(iv,j) ! simplical subspace
            ENDDO
         ENDDO
         DO j=1,m_kDim
            Vrand(m_nDim+j) = m_CeVer1(iCell,j) +Alpha(j)*m_CeVer2(iCell,j) ! hypcubic subspace
         ENDDO
****     WRITE(*,'(a,6f12.6)') ' Lambda    =',(Lambda(k),k=1,m_nDim)
****     WRITE(*,'(a,6f12.6)') ' Alpha     =',(Alpha(k), k=1,m_kDim)
****     WRITE(*,'(a,6f12.6)') ' Vrand     =',(Vrand(k), k=1,m_Dimen)
*///////////////////////////////////////////////////////////////////////////////////
* Projecting on all simplex edges, preparatory step, partial volumes etc.
         IF( m_nDim .GT. 0 ) THEN
            Vsum=0d0
            DO jv=1,m_nDim+1    ! vertex jv will be replaced with Vrand
               kv=0             ! kv numbers all verices axcept jv
               DO iv=1,m_nDim+1
                  IF(iv.NE.jv) THEN
                     kv=kv+1
                     DO j=1,m_nDim
                        Yre(kv,j) = Vec(iv,j)-Vrand(j) ! All vertices except jv relative to Vrand
                     ENDDO
                  ENDIF
               ENDDO
               CALL FoamB_Determinant(Yre,DxPartial)
               VolPart(jv) = ABS(DxPartial)/Factorial
               Vsum=Vsum + VolPart(jv)
            ENDDO
            IF( ABS(Vsum-DxSp)/DxSp .GT. 1d-5) GOTO 950 ! X-check
         ENDIF
*------------------------------------------------------------------------------------
* IMPORTANT! Two Loops below determine the indexing of edges (simplex and hypercube)
*------------------------------------------------------------------------------------
         nEdges=0
         DO jv=1,m_nDim+1
            DO iv=jv+1,m_nDim+1
               nEdges=nEdges+1
               Xdivi(nEdges) = VolPart(jv) / ( VolPart(jv)+VolPart(iv) )
            ENDDO
         ENDDO
         DO j=1,m_kDim
            nEdges=nEdges+1
            Xdivi(nEdges) = Alpha(j)
         ENDDO
*///////////////////////////////////////////////////////////////////////////////////
         Wt =funW(Vrand)*Dx  ! principal weight normalised to integral over the cell
*------------------------------------------------------------------------------------
         m_nCalls = m_nCalls+1
         m_CeSum(iCell,1) = m_CeSum(iCell,1)+ Wt         ! sum of weights
         m_CeSum(iCell,2) = m_CeSum(iCell,2)+ Wt*Wt      ! sum of weights squared
         m_CeSum(iCell,3) = m_CeSum(iCell,3)+ 1d0        ! sum of 1
         m_CeSum(iCell,4) = MIN( m_CeSum(iCell,4),Wt)    ! minium weight
         m_CeSum(iCell,5) = MAX( m_CeSum(iCell,5),Wt)    ! maximu weight
* Search for the best edge candidate for future cell division, prepare MC material
         Ndivi  = (m_nDim+1)*m_nDim/2 +m_kDim
         IF(nEdges.NE.nDivi) GOTO 970
         DO k=1,nDivi
            Xsu1(k)=Xsu1(k) +Xdivi(k)*Wt                 ! averages for all Xdivi
            Xsu2(k)=Xsu2(k) +Xdivi(k)**2*Wt
            iBin = INT(Xdivi(k)*m_nBin)+1d0
            iBin = MIN(MAX(iBin,0),m_nBin)
            Histo(k,iBin) = Histo(k,iBin)+Wt             ! fill histo for each edge
c[[[[[[[[[[[[ debug [[[[[[[[[[[[ debug [[[[[[[[[[[[
c            IF(iCell.EQ.4) THEN
c               CALL GLK_Fil1(1200+k, Xdivi(k),Wt)
c            ENDIF
c]]]]]]]]]]]] debug ]]]]]]]]]]]] debug ]]]]]]]]]]]]
         ENDDO
         IF( m_EvPerBin.GT.0 ) THEN
            NevEff = m_CeSum(iCell,1)**2/m_CeSum(iCell,2)
            IF( NevEff .GE. m_nBin*m_EvPerBin) GOTO 222 !
         ENDIF
      ENDDO
 222  CONTINUE
*///////////////////////////////////////////////////////
*//   End of Special Short MC sampling to probe cell  //
*///////////////////////////////////////////////////////


*//////////////////////////////////////////////////////////////////////////////////////
*//  Determine the best edge candidate for future cell division, 
*//  using MC  material in Histo,Xsu1,Xsu2
*//  kBest,xBest,yBest are the output results
      CALL FoamB_Carver(iCell,Histo,Xsu1,Xsu2,kBest,xBest,yBest)
*//////////////////////////////////////////////////////////////////////////////////////
      NoMC =m_CeSum(iCell,3)
      IF( m_CeSum(iCell,1) .LT.0d0) GOTO 920
      m_CeXave(iCell)  = xBest                      ! best lambda for future division
      m_CeBest(iCell)  = kBest                      ! best edge for future division
      m_CeIntg(iCell)  = m_CeSum(iCell,1)/NoMC      ! estimator of the true integral
*!!!!!!!!!  DrivE is for the Foam build-up (not for MC generation)    !!!!!!!!!!
*!!!!!!!!!  PRIMARY is for MC generation (not for the Foam build-up ) !!!!!!!!!!
      IF(     m_OptDrive.EQ.0 ) THEN
         m_CePrim(iCell) = m_CeIntg(iCell)                   ! True integral, MC generation
         m_CeDriv(iCell) = m_CeIntg(iCell)                   ! True integral, Foam build-up
      ELSEIF( m_OptDrive.EQ.1 ) THEN
         m_CePrim(iCell) = DSQRT(m_CeSum(iCell,2)/NoMC)      ! Sqrt( <w>**2 + sigma**2 )= Sqrt(<w**2>)
         m_CeDriv(iCell) = DSQRT(m_CeSum(iCell,2)/NoMC -m_CeIntg(iCell)**2) ! sigma =Sqrt(<w**2>-<w>**2)
      ELSEIF( m_OptDrive.EQ.2 ) THEN
         m_CePrim(iCell) = m_CeSum(iCell,5)                  ! wtmax    , MC generation
         m_CeDriv(iCell) = m_CeSum(iCell,5) -m_CeIntg(iCell) ! wtmax-<w>, Foam build-up
      ELSE
         WRITE(m_out,*) ' ++++ STOP in FoamB_Explore, wrong m_OptDrive =',m_OptDrive !
         WRITE(    *,*) ' ++++ STOP in FoamB_Explore, wrong m_OptDrive =',m_OptDrive !
         STOP
      ENDIF
* correct volume and Drive in all parent cells to the top of the tree
      parent = m_CePare(iCell)
      DO i = 1,m_nBuf
         IF( parent .EQ. -1 ) GOTO 100 ! Exit if no parent exists
         m_CeIntg(parent)  = m_CeIntg(parent)  +( m_CeIntg(iCell)  -VolOld) !
         m_CeDriv(parent)  = m_CeDriv(parent)  +( m_CeDriv(iCell)  -DriOld) !
         parent=m_CePare(parent)
      ENDDO
 100  CONTINUE
      RETURN
 920  WRITE(*,*) ' ### STOP in FoamB_Explore: something wrong with integrand ' !
      STOP
 950  WRITE(*,*) ' ### STOP in FoamB_Explore: something wrong with volume calculation ' !
      WRITE(*,*) ' Vsum,DxSp= ',Vsum,DxSp
      STOP
 960  WRITE(*,*) ' ### STOP in FoamB_Explore: something wrong with best pair pointer =',kBest !
      STOP
 970  WRITE(*,*) ' ### STOP in FoamB_Explore: something wrong with Ndivi =',nDivi !
      STOP
      END                       ! FoamB_Explore



      SUBROUTINE FoamB_Carver(iCell,Histo,Xsu1,Xsu2,kBest,xBest,yBest)  !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*// Determine the best edge candidate for future cell division, using MC  material   //
*// kBest is the best edge found, xBest and yBest are the best values of lambda      //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            iCell,kBest
      DOUBLE PRECISION   xBest,yBest,zBest
      DOUBLE PRECISION   Histo(m_NdiviMax,m_nBinMax), Xsu1(m_NdiviMax), Xsu2(m_NdiviMax) !
      DOUBLE PRECISION   Bins(m_nBinMax)
      INTEGER            i,j,k, iBin, nDivi
      INTEGER            iUp,iLow, jUp,jLow, kDivi, jv,iv, jDivi
      DOUBLE PRECISION   SumWt,BinMax,This, Carve, yLevel
      DOUBLE PRECISION   CarvOne,CarvTwo,CarvMax
*
      kBest =1
      xBest =0.5d0
      yBest =1d0
      SumWt  = m_CeSum(iCell,1)
      IF( SumWt .NE. 0d0) THEN
         CarvMax = -1d150
         nDivi  = (m_nDim+1)*m_nDim/2 +m_kDim
         DO kDivi=1,nDivi
            BinMax  = -1d150
            DO iBin=1,m_nBin    ! Unload histo and Maximum bin
               Bins(iBin) = Histo(kDivi,iBin)
               BinMax = MAX( BinMax, Bins(iBin)) ! Maximum content/bin
            ENDDO
            CarvTwo = 0d0
            DO iBin=1,m_nBin
               Bins(iBin) = Bins(iBin)/BinMax ! Normalize to the highest bin
               CarvTwo = CarvTwo + (1d0-Bins(iBin))/m_nBin ! Total carve
            ENDDO
* Find maximum 'rectangular carve' in betwen the two bins (jLow,...,jUp)
            jLow =1
            jUp  =m_nBin
            CarvOne = -1d150
            yLevel  = -1d150
            DO iBin=1,m_nBin
               This = Bins(iBin)
               iLow = iBin
               DO j=iBin,1,-1   ! walk to the left and find first bin > current
                  IF(This .LT. Bins(j) ) GOTO 100
                  iLow = j
               ENDDO
 100           CONTINUE
               iUp  = iBin
               DO j=iBin,m_nBin ! walk to the right and find first bin > current
                  IF(This .LT. Bins(j) ) GOTO 200
                  iUp  = j
               ENDDO
 200           CONTINUE
               Carve = (iUp-iLow+1)*(1d0-This)/m_nBin
               IF( Carve .GT. CarvOne) THEN
                  CarvOne = Carve
                  jLow = iLow
                  jUp  = iUp
                  yLevel = This
               ENDIF
            ENDDO               ! end-loop over histogram bins 
*************************************************************
**               IF( CarvOne .GT. CarvMax) THEN
**                  CarvMax   = CarvOne
*************************************************************
            IF( CarvTwo .GT. CarvMax) THEN
               CarvMax   = CarvTwo
               kBest = kDivi    ! Best edge
               xBest = (jLow-1d0)/m_nBin
               yBest = (jUp*1d0)/m_nBin
               IF(jLow .EQ. 1 )     xBest = yBest
               IF(jUp  .EQ. m_nBin) yBest = xBest
* division ratio in units of 1/m_nBin, testing
               jDivi = jLow-1
               IF(jLow .EQ. 1 )     jDivi=jUp
**************************************************************
* The improvement below does not seem to matter at all
*                  IF( 0.5d0*(xBest+yBest) .LT. 0.5d0) THEN
*                     zBest =xBest
*                     xBest =yBest
*                     yBest =zBest
*                  ENDIF
**************************************************************
            ENDIF
*[[[[[[[[[[[[ debug [[[[[[[[[[[[ debug [[[[[[[[[[[[[[[[[[
*               WRITE(*,'(a,4i5,f10.5)') '==iCell,kDivi,jLow,jUp,CarvMax= ',iCell,kDivi,jLow,jUp,CarvOne !!
*               IF(iCell.EQ.2 ) THEN
*                  CALL GLK_Pak(  1200+kDivi,Bins)
*                  DO iBin = 1,m_nBin
*                     Bins(iBin) = 1d0
*                  ENDDO
*                  DO iBin=jLow,jUp
*                     Bins(iBin) = yLevel
*                  ENDDO
*                  CALL GLK_Pak(  1400+kDivi,Bins)
*               ENDIF
c]]]]]]]]]]]]debug ]]]]]]]]]]]] debug ]]]]]]]]]]]]]]]]]]]
         ENDDO                  ! end-loop over nDivi
      ENDIF
*[[[[[[[[[[[[ debug [[[[[[[[[[[[ debug [[[[[[[[[[[[
*      WRITE(*,'(a,i5)')        ' FoamB_Carver , iCell = ', iCell
*      WRITE(*,'(a,i5,3f10.5)') ' kBest,xBest,yBest= ', kBest, xBest,yBest!!
*]]]]]]]]]]]] debug ]]]]]]]]]]]] debug ]]]]]]]]]]]]
      END                       ! FoamB_Carver


      SUBROUTINE FoamB_RanDiscr(Driv,nTot,Power,iRand)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   PRESENTLY UNUSED !!!                                                           //
*//   Generates iRand in (1,nTot) acconding to discrete un-normalized probab. Driv   //
*//   Power is normaly =1, can be useful for special purposes                        //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      DOUBLE PRECISION   Driv(*),Power
      INTEGER            nTot,iRand
      INTEGER            i
      DOUBLE PRECISION   random,sum,Total
      REAL               Qrand(10)        ! from PseuMar
*
      Total   = 0d0
      DO i= 1,nTot
         Total = Total +Driv( i)**Power
      ENDDO
      IF(Total .EQ. 0d0) GOTO 990
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,1)
      ELSE
         CALL RanLux(Qrand,1)
      ENDIF
      random = Qrand(1)
      iRand  = -1
      sum   = 0d0
      DO i= 1,nTot
         iRand  = i
         sum = sum +Driv( i)**Power
         IF( random .LT. sum/Total ) GOTO 100
      ENDDO
      IF(iRand .EQ. -1) GOTO 990
 100  CONTINUE
      RETURN
 990  WRITE(*,*) ' ### STOP in FoamB_RanDiscr, something went wrong !!!!'
      STOP
      END


      SUBROUTINE FoamB_MakeAlpha(Alpha)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                      //
*//   Provides random vector Alpha, each component in (0,1) range                        //
*//                                                                                      //
*//////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   Alpha(m_KdiMax), y
      REAL               Qrand( m_KdiMax)        ! from PseuMar
      INTEGER            i,k
*     --------------------------------------------------------
      IF( m_kDim.LT.0 .OR. m_kDim.GT.m_KdiMax) THEN
         WRITE(*,*) 'STOP in FoamB_MakeAlpha: m_kDim=',m_kDim
         STOP
      ENDIF
      IF(m_kDim.LE.0) RETURN
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,m_kDim)
      ELSE
         CALL RanLux(Qrand,m_kDim)
      ENDIF
      DO k =1,m_kDim
         Alpha(k)=Qrand(k)
      ENDDO
      END

      SUBROUTINE FoamB_MakeLambda(Lambda)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                      //
*//   Provides random vector Lambda such that Sum Lamba(i) = 1, with uniform probab.     //
*//   This  vector is used to populate uniformly the interior of a simplex.              //
*//   The method is: generate point inside cube, order components (maping into simplex)  //
*//   and take differences of Lambda(i+1) - Lambda(i)                                    //
*//                                                                                      //
*//////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            i,k
      DOUBLE PRECISION   Lambda(m_NdiMax), y
      REAL               Qrand( m_NdiMax)        ! from PseuMar
      REAL               x
*     --------------------------------------------------------
      IF(m_nDim.LE.0) RETURN
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,m_nDim)
      ELSE
         CALL RanLux(Qrand,m_nDim)
      ENDIF
* ordering vector components (maping into simplex)
      DO i =m_nDim,1,-1
         DO k =2,i
            IF( Qrand(k).LT.Qrand(k-1)) THEN
               x            = Qrand(k)
               Qrand(k)    = Qrand(k-1)
               Qrand(k-1)  = x
            ENDIF
         ENDDO
      ENDDO
* Sum of lambdas should equal one
      Lambda(1)=Qrand(1)
      DO k =2,m_nDim
         Lambda(k)=Qrand(k)-Qrand(k-1)
      ENDDO
      END                       ! MakeLambda


      SUBROUTINE FoamB_Determinant(R,Det)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Calculates determinant of matrix R                                             //
*//   Use of Laplace formula should be perhaps replaced with something faster        //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det
      DOUBLE PRECISION   FoamB_Det2Lapl, FoamB_Det3Lapl,FoamB_Det4Lapl,FoamB_Det5Lapl !
*     -------------------------------------------------
      IF(        m_nDim .EQ. 1) THEN
         Det= R(1,1)
      ELSEIF(    m_nDim .EQ. 2) THEN
         Det= FoamB_Det2Lapl(R, 1,2)
      ELSEIF(    m_nDim .EQ. 3) THEN
         Det= FoamB_Det3Lapl(R, 1,2,3)
      ELSEIF(    m_nDim .EQ. 4) THEN
         Det= FoamB_Det4Lapl(R, 1,2,3,4)
      ELSEIF(    m_nDim .EQ. 5) THEN
         Det= FoamB_Det5Lapl(R, 1,2,3,4,5)
      ELSE
         WRITE(*,*) '####FoamB_Determinant: STOP, m_nDim =',m_nDim
         STOP
      ENDIF
      END                       ! FoamB_Determinant

      DOUBLE PRECISION FUNCTION FoamB_Det2Lapl(R,i1,i2)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det
      INTEGER  i1,i2
*     ------------------------------------------------------------
      FoamB_Det2Lapl= R(1,i1)*R(2,i2) - R(1,i2)*R(2,i1)
      END


      DOUBLE PRECISION FUNCTION FoamB_Det3Lapl(R,i1,i2,i3)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   Determinat Laplace method                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det,FoamB_Det2Lapl
      INTEGER  i1,i2,i3
*     ------------------------------------------------------------
      FoamB_Det3Lapl=+R(3,i1) *FoamB_Det2Lapl(R,i2,i3)
     $               -R(3,i2) *FoamB_Det2Lapl(R,i1,i3)
     $               +R(3,i3) *FoamB_Det2Lapl(R,i1,i2)
      END

      DOUBLE PRECISION FUNCTION FoamB_Det4Lapl(R,i1,i2,i3,i4)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   Determinat Laplace method                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det,FoamB_Det3Lapl
      INTEGER  i1,i2,i3,i4
*     ------------------------------------------------------------
      FoamB_Det4Lapl=-R(4,i1) *FoamB_Det3Lapl(R,i2,i3,i4)
     $               +R(4,i2) *FoamB_Det3Lapl(R,i1,i3,i4)
     $               -R(4,i3) *FoamB_Det3Lapl(R,i1,i2,i4)
     $               +R(4,i4) *FoamB_Det3Lapl(R,i1,i2,i3)
      END

      DOUBLE PRECISION FUNCTION FoamB_Det5Lapl(R,i1,i2,i3,i4,i5)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   Determinat Laplace method                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax), Det, FoamB_Det4Lapl
      INTEGER  i1,i2,i3,i4,i5
*     ------------------------------------------------------------
      FoamB_Det5Lapl=+R(5,i1) *FoamB_Det4Lapl(R,i2,i3,i4,i5)
     $               -R(5,i2) *FoamB_Det4Lapl(R,i1,i3,i4,i5)
     $               +R(5,i3) *FoamB_Det4Lapl(R,i1,i2,i4,i5)
     $               -R(5,i4) *FoamB_Det4Lapl(R,i1,i2,i3,i5)
     $               +R(5,i5) *FoamB_Det4Lapl(R,i1,i2,i3,i4)
      END


      SUBROUTINE FoamB_Grow(funW)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Grow new cells by division                                                     //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            i,RC,iCell
*---------------------------------------------
* Final division
      DO i=1,100000
         IF(  m_OptPeek .EQ. 0 ) THEN
            CALL FoamB_PeekMax(  iCell)       ! choose cell with maximum Drive
         ELSE
            CALL FoamB_PeekRand1(iCell)       ! randomly choose one cell
         ENDIF
         CALL FoamB_Divide( iCell,funW,RC)    ! and divide it into two
c[[[[
c         CALL FoamB_BufPrint(    m_Out)
c         CALL FoamB_VertPrint(  6)
c]]]]
         IF(RC.EQ.-1) GOTO 300
      ENDDO
 300  CONTINUE
      WRITE(16,*) '######################### FoamB_Grow #####################'
*****[[[[[[ debug
***** CALL FoamB_BufPrint(    6)
***** CALL FoamB_BufActPrint( 6)
***** CALL FoamB_VertPrint(   6)
*****]]]]]]
      CALL FoamB_Check(6,0)
      END                       ! FoamB_Grow

      SUBROUTINE FoamB_PeekMax(iCell)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Create list of active cells (pointers) using DrivE                             //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER   iCell
      INTEGER   i
      DOUBLE PRECISION  DrivMax, Driv
*     ---------------------------------------------------
      iCell = 0
      DrivMax = -1d150
      DO i = 1,m_LastCe
         IF( m_CeStat(i).EQ.1 ) THEN
            Driv=  ABS(m_CeDriv(i))
            IF(Driv .GT. DrivMax) THEN
               DrivMax = Driv
               iCell = i
            ENDIF
         ENDIF
      ENDDO
****  WRITE(*,*) '###>>> FoamB_PeekMax: iCell=',iCell
      IF(iCell.EQ.0) THEN
         WRITE(*,*) '### STOP in FoamB_PeekMax: not found iCell=', iCell
         STOP
      ENDIF
      END                       ! FoamB_PeekMax


      SUBROUTINE FoamB_PeekRand1(iCell)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//       Peek up randomly TREE-WISE the pointer iCell of an active cell             //
*//                        Using DrivE                                               //
*// We walk randomly from top of tree downwards until we find active cell m_CeStat=1 //
*// At each step one of daugters cells is choosen randomly according                 //
*// to their volume estimates.                                                       //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            iCell
      INTEGER            kCell,i,Dau1,Dau2,iDau
      DOUBLE PRECISION   random,p1,volu1,volu2,volu,TotDri,sum
      REAL               Qrand(10)        ! from PseuMar
*     ----------------------------------------------------------------
* first cell is special because it has nDim-factorial daughters, istead of just 2
      iCell=1
      IF(m_CeStat(iCell).EQ.1) RETURN ! In case of cubics firs cell may be active
      kCell = 1
      Dau1  = m_CeDau1(kCell)
      Dau2  = m_CeDau2(kCell)
      TotDri   = 0d0
      DO iCell= Dau1,Dau2
         TotDri = TotDri+m_CeDriv( iCell )
      ENDDO
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,1)
      ELSE
         CALL RanLux(Qrand,1)
      ENDIF
      random = Qrand(1)
      iDau  = -1
      sum   = 0d0
      DO iCell= Dau1,Dau2
         iDau  = iCell
         sum = sum+m_CeDriv( iCell )
         IF( random .LT. sum/TotDri ) GOTO 100
      ENDDO
      IF(iDau.EQ.-1) GOTO 990
 100  kCell=iDau
****[[[[[[[[[[[[[[[[
****  WRITE(*,*) '%%%%%%%%%%%%%%%%%%%%%% FoamB_PeekRand1: top daughter =',kCell
****]]]]]]]]]]]]]]]]
      IF( m_CeStat( kCell ) .EQ. 1 ) GOTO 300
* now the other standard cells with 2 daughters
      DO i=1,10000000
         IF( m_CeStat( kCell ) .EQ. 1 ) GOTO 300
         volu1= m_CeDriv( m_CeDau1(kCell) )
         volu2= m_CeDriv( m_CeDau2(kCell) )
         p1 = volu1/(volu1+volu2)
         IF(m_OptRanLux.EQ.-1) THEN
            CALL PseuMar_MakeVec(Qrand,1)
         ELSE
            CALL RanLux(Qrand,1)
         ENDIF
         random = Qrand(1)
         IF( random .LT. p1 ) THEN
            kCell = m_CeDau1(kCell)
         ELSE
            kCell = m_CeDau2(kCell)
         ENDIF
****[[[[[[[[[[[[[[[[[[[[
****  WRITE(*,*) '%%%%%%%%%%%%%%%%%%%%%% FoamB_PeekRand1: normal daughter =',kCell
****]]]]]]]]]]]]]]]]]]]]]
      ENDDO
      GOTO 990
 300  CONTINUE
      iCell=kCell
****[[[[[[[[[[[[[[[[[[[[
****  WRITE(*,*) '%%%%%%%%%%%%%%%%%%%%%% FoamB_PeekRand1: choosen cell =',kCell
****]]]]]]]]]]]]]]]]]]]]]
      RETURN
 990  WRITE(*,*) ' ### STOP in FoamB_PeekRand1, something went wrong !!!!'
      STOP
      END                       !!! FoamB_PeekRand1


      SUBROUTINE FoamB_Divide(iCell,funW,RC)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Divide cell iCell into two daughter cells                                      //
*//   The iCell is retained and taged as inactive, daughter cells are appended       //
*//   at the end of the buffer.                                                      //
*//   New vertex is added to list of vertice.                                        //
*//   List of active cells is updated, iCell remooved, two daughters added           //
*//   and their properties set with help of MC sampling (FoamB_Explore)              //
*//   Return Code RC=-1 of buffer limit is reached,  m_LastCe=m_nBuf                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            iCell,RC
      INTEGER            Dau1, Dau2, kVer1(m_NdiMax+1), kVer2(m_NdiMax+1),p1,p2 !
      DOUBLE PRECISION   HcPosi1(m_KdiMax),   HcSize1(m_KdiMax)
      DOUBLE PRECISION   HcPosi2(m_KdiMax),   HcSize2(m_KdiMax)
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            Old1,Old2,j,k,jv,iv,nEdges,kBest,kDivi,NoMC
      DOUBLE PRECISION   Xave
      INTEGER            nDivi,nDivi0
*--------------------------------------------------------------------------------------
      RC = 0
      IF( m_LastCe+2 .GT. m_nBuf) GOTO 990 !! abort if no space in buffer
* reset cell as inactive
      m_CeStat(iCell) = 0
*------------------------------------------------------------------------------------
* Define lists of vertices for daughters, initially inherited...
      IF(m_nDim .GT. 0) THEN
         DO jv=1,m_nDim+1
            kVer1(jv) = m_CeVert(iCell,jv)
            kVer2(jv) = m_CeVert(iCell,jv)
         ENDDO
      ENDIF
* Position and size initially also inherited
      DO j=1,m_kDim
         HcPosi1(j)=m_CeVer1(iCell,j)
         HcSize1(j)=m_CeVer2(iCell,j)
         HcPosi2(j)=m_CeVer1(iCell,j)
         HcSize2(j)=m_CeVer2(iCell,j)
      ENDDO
*--------------------------------------------------------------------------------------
      nDivi  = (m_nDim+1)*m_nDim/2 +m_kDim
      nDivi0 = (m_nDim+1)*m_nDim/2
      kBest  = m_CeBest(iCell)
      IF( kBest.LE.nDivi0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//   The best division in simplical subspace
*//////////////////////////////////////////////////////////////////////////////////////
         m_LastVe=m_LastVe+1    ! add new vertex to the list
         IF(m_LastVe.GT.m_vMax) GOTO 980
         Xave  = m_CeXave(iCell)
         nEdges=0
         DO jv=1,m_nDim+1
            DO iv=jv+1,m_nDim+1
               nEdges=nEdges+1
               IF( nEdges.EQ.kBest) THEN
                  p1 =  m_CeVert(iCell,jv)
                  p2 =  m_CeVert(iCell,iv)
                  DO j=1,m_nDim
                     m_VerX(m_LastVe,j) = Xave*m_VerX(p1,j) + (1d0-Xave)*m_VerX(p2,j) ! New Vertex
                  ENDDO
                  Old1=jv
                  Old2=iv
                  GOTO 100
               ENDIF
            ENDDO
         ENDDO
 100     CONTINUE
         kVer1(Old1)=m_LastVe   ! one old vertex replaced by new one
         kVer2(Old2)=m_LastVe   ! one old vertex replaced by new one
      ELSE
*//////////////////////////////////////////////////////////////////////////////////////
*//   The best division in hypercubical subspace
*//   Position and size inherited except one direction k=kDivi
*//////////////////////////////////////////////////////////////////////////////////////
         Xave  = m_CeXave(iCell)
         kDivi = kBest-nDivi0
         HcPosi1(kDivi)=      m_CeVer1(iCell,kDivi)        ! (1) Position unchanged
         HcSize1(kDivi)= Xave*m_CeVer2(iCell,kDivi)        ! (1) Size reduced by Xave
         HcPosi2(kDivi)= HcPosi1(kDivi) +HcSize1(kDivi)    ! (2) Position shifted
         HcSize2(kDivi)= (1d0-Xave)*m_CeVer2(iCell,kDivi)  ! (2) Size reduced by (1-Xave)
      ENDIF
* define two daughter cells (active)
      NoMC= m_nSampl
* ------------------------ Stat, Pare, Dau1,Dau2, MCsampl, VertList, Position,Size,  iCeNew
      CALL FoamB_DefCell(  1, iCell,  -1,  -1,    NoMC,   kVer1,  HcPosi1, HcSize1, Dau1) !
      CALL FoamB_DefCell(  1, iCell,  -1,  -1,    NoMC,   kVer2,  HcPosi2, HcSize2, Dau2) !
      m_CeDau1(iCell) = Dau1
      m_CeDau2(iCell) = Dau2
      CALL FoamB_Explore(Dau1,funW)
      CALL FoamB_Explore(Dau2,funW)
* Update list of active cells, only for internal tests
      CALL FoamB_ActUpda
      RETURN
 990  RC=-1                     !!buffer limit is reached,  m_LastCe=m_nBuf
      RETURN
 980  WRITE(*,*) ' ### STOP in FoamB_Divide: too short list of vertices '
      STOP
      END                       ! FoamB_Divide



      SUBROUTINE FoamB_ActUpda     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Create list of active cells (pointers)                                         //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER   iCell,Dau1,Dau2
      INTEGER   i
*     ---------------------------------------------------
      m_LastAc=0
      DO iCell = 1,m_LastCe
         IF( m_CeStat(iCell).EQ.1 ) THEN
            m_LastAc=m_LastAc+1
            IF(m_LastAc .EQ. m_cMax) GOTO 950
            m_ActC(m_LastAc) = iCell
         ENDIF
      ENDDO
      RETURN
 900  WRITE(*,*) '### STOP in FoamB_ActUpda: not found iCell=', iCell
      STOP
 950  WRITE(*,*) '### STOP in FoamB_ActUpda: list of active cells too short'
      STOP
      END                       ! FoamB_ActUpda


      SUBROUTINE FoamB_CellGener(iCell)     !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//       Peek up randomly pointer iCell of an active cell according to PRIMARY      //
*//       Straightforward way, using list of active pointes made by ActUpda          //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            iCell
      INTEGER            kCell,i,Dau1,Dau2,iDau
      DOUBLE PRECISION   random,x1,TotPrim
      INTEGER            klower,kuper,krange,kurrent,DipSwitch
      REAL               Qrand(10)        ! from PseuMar
      INTEGER   iCont
      DATA      iCont/0/
      iCont = iCont+1
*     ----------------------------------------------------------------
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,1)
      ELSE
         CALL RanLux(Qrand,1)
      ENDIF
      random = Qrand(1)
*     ---------------------------------------------------
      TotPrim = m_CePrCu(m_LastAc)
      x1 = TotPrim *random
      DipSwitch=0               ! =0 is faater
      IF( DipSwitch.EQ.1) THEN
*-------- primitive method --------
         DO kCell = 1,m_LastAc
            iCell = m_ActC(kCell)
            IF( m_CePrCu(kCell) .GE. x1 ) GOTO 800
         ENDDO
         WRITE(*,*) '### STOP1 in FoamB_CellGener: something wrong' !
         STOP
 800     CONTINUE
      ELSEIF( DipSwitch.EQ.0) THEN
*-------- a bit more sophisticated/faster method ------
         klower   = 0
         kuper    = m_LastAc
         DO i=1,m_LastAc
            krange   = (kuper-klower+1)/2
            kurrent  = klower +krange
            IF( x1 .LE. m_CePrCu(kurrent) ) THEN
               kuper = kurrent
            ELSE
               klower = kurrent
            ENDIF
            IF(kuper-klower.LE.1) GOTO 850
         ENDDO
         WRITE(*,*) ' STOP in FoamB_CellGener'
         STOP
 850     CONTINUE
         iCell = m_ActC(kuper)
         kCell = kuper
      ELSE
         iCell = m_ActC(1)      ! nonsense for tests
      ENDIF
***** IF(iCont.LE.10) WRITE(*,*) 'LastAc,kCell,iCell = ',m_LastAc,kCell,iCell !! debug
      END                       ! FoamB_CellGener


      SUBROUTINE FoamB_MakeEvent(funW)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Generates point/vector Xrand with the weight MCwt                              //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            iCell,i,j,iv
      DOUBLE PRECISION   Wt,x1,x2,Dx,MCwt
      DOUBLE PRECISION   Alpha( m_KdiMax) ! Hyper-Cubic subspace
      DOUBLE PRECISION   Lambda(m_NdiMax) ! Simplical subspace
*     -----------------------------------------------------------------
*     choose randomly one cell
      CALL FoamB_CellGener( iCell)
*     generate randomly/uniformly vector Vrand inside this simplex
      CALL FoamB_MakeAlpha( Alpha)
      CALL FoamB_MakeLambda(Lambda)
      DO j=1,m_nDim
         m_MCvector(j) = m_VerX( m_CeVert(iCell,m_nDim+1) ,j)
         DO iv=1,m_nDim
            m_MCvector(j) = m_MCvector(j) 
     $           +Lambda(iv)*( m_VerX( m_CeVert(iCell,iv) ,j) -m_VerX( m_CeVert(iCell,m_nDim+1) ,j) ) !
         ENDDO
      ENDDO
      DO j=1,m_kDim
         m_MCvector(m_nDim+j) = m_CeVer1(iCell,j) +Alpha(j)*m_CeVer2(iCell,j) ! hypcubic subspace
      ENDDO
      Dx = m_CeVolu(iCell)      ! Cartesian volume of the Cell
*[[[[[[[[[[[[
***** CALL FoamB_TestNewConcept(iCell)
*]]]]]]]]]]]]
* weight average normalised to PRIMARY integral over the cell
      MCwt =funW(m_MCvector)*Dx/m_CePrim(iCell) ! PRIMARY controls normalization
      m_nCalls =  m_nCalls+1
      m_MCwt   =  MCwt
* accumulation of statistics for the main MC weight
      m_SumWt  =  m_SumWt  +MCwt         ! sum of Wt
      m_SumWt2 =  m_SumWt2 +MCWt*MCwt    ! sum of Wt**2
      m_NevGen =  m_NevGen +1d0          ! sum of 1d0
      m_WtMax  =  MAX(m_WtMax,MCwt)      ! maximum wt
      m_WtMin  =  MIN(m_WtMin,MCwt)      ! minimum wt
* update also weight sums in the cell,
* note weights here are normalized absolutely, eg. to the value of the integral
      Wt = MCwt*m_CePrim(iCell)
      m_CeSum(iCell,1) = m_CeSum(iCell,1)+ Wt      ! sum of weights
      m_CeSum(iCell,2) = m_CeSum(iCell,2)+ Wt*Wt   ! sum of weights squared
      m_CeSum(iCell,3) = m_CeSum(iCell,3)+ 1d0     ! sum of 1
      m_CeSum(iCell,4) = MIN( m_CeSum(iCell,4),Wt) ! minium weight
      m_CeSum(iCell,5) = MAX( m_CeSum(iCell,5),Wt) ! maximu weight
      END                       ! FoamB_MakeEvent

      SUBROUTINE FoamB_GetMCvector(MCvector)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION  MCvector(*)
      INTEGER           k
*-----------------------
      DO k=1,m_Dimen
         MCvector(k)    = m_MCvector(k)
      ENDDO
      END

      SUBROUTINE FoamB_GetMCwt(MCwt)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION  MCwt
*-----------------------
      MCwt    = m_MCwt
      END

      SUBROUTINE FoamB_MCgenerate(funW,MCvector,MCwt)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Obsolete                                                                       //
*//   Generates point/vector MCvector with the weight MCwt                           //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION   MCvector(*),MCwt
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            j
*     ---------------------------------------------------------------
      CALL FoamB_MakeEvent(funW)
      MCwt = m_MCwt
      DO j=1,m_Dimen
         MCvector(j) =m_MCvector(j)
      ENDDO
      END                       !!FoamB_MCgenerate


      SUBROUTINE FoamB_Finalize(MCresult,MCerror)    !# Finalization
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   After MC run is completed it calculates integral and its error         //
*//   Also prints some information/statistics on the MC run                  //
*//   Remember that PRIMARY controls normalization of the integration        //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
*
      DOUBLE PRECISION     MCresult,MCerror,MCerelat
      DOUBLE PRECISION     Vtot,Verr,VerRela,TotPrim
      INTEGER              kCell,iCell
*-----------------------------------------------------------------------------
      MCresult =0d0
      MCerelat =1d0
      TotPrim = 0d0
      DO kCell = 1,m_LastAc
         iCell  = m_ActC(kCell)
         TotPrim = TotPrim +m_CePrim( iCell )
      ENDDO
      IF(m_NevGen .GT. 0) THEN
         MCresult = TotPrim *m_SumWt/m_NevGen
         IF(m_SumWt.NE.0d0)  MCerelat  = m_SumWt2/m_SumWt**2 -1d0/m_NevGen !
         IF(MCerelat.GT.0d0) MCerelat =  SQRT( ABS(MCerelat) ) !
         IF(MCerelat.LT.0d0) MCerelat = -SQRT( ABS(MCerelat) ) !
      ENDIF
      MCerror = MCresult*MCerelat
* some test printouts
      WRITE(m_Out,'(3a)') '||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||' !
      WRITE(m_Out,'(3a)') '||||||||||||||||||||||||||||||', ' FoamB_Finalize ', !
     $                    '||||||||||||||||||||||||||||||'
      WRITE(m_Out,'(a,2g18.9,f11.7)')               'MCresult, MCerror, Errela= ',MCresult,MCerror,MCerelat !
      WRITE(m_Out,'(a,2f11.5)')                     'Minimum maximum weight   = ',m_WtMin,m_WtMax !
      IF(m_NevGen .GT. 0) WRITE(m_Out,'(a,2f11.5)') 'Average wt SumWt/NevGen  = ',m_SumWt/m_NevGen !
      WRITE(m_Out,'(a,i15)')                        'Total of function calls  = ',m_nCalls !
      WRITE(m_Out,'(a,2i15)')                       'Number of cells&vertices = ',m_LastCe,m_LastVe !
      WRITE(m_Out,'(a,f12.1,2g18.9)')               'NevGen,SumWt,m_SumWt2    = ',m_NevGen,m_SumWt,m_SumWt2 !
      END       ! FoamB_Finalize



      SUBROUTINE FoamB_GetTotPrim(TotPrim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Total integral from cell statistics, including initialization + MC generation  //
*//   It can be invoked just after initialization or after MC generation             //
*//   Note that this estimate is distorted slightly if vertices are included in      //
*//   the exploration of the cells.                                                  //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      INCLUDE 'FoamB.h'
      DOUBLE PRECISION  TotPrim,Sum
      INTEGER   kCell,  iCell      
      Sum = 0d0
      DO kCell = 1,m_LastAc
         iCell  = m_ActC(kCell)
         Sum = Sum +m_CePrim( iCell )
      ENDDO
      TotPrim=Sum
      END


      SUBROUTINE FoamB_SetnDim(nDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      nDim
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_nDim = nDim
      m_Dimen=m_nDim+m_kDim
      END                       !!! FoamB_SetnDim

      SUBROUTINE FoamB_SetkDim(kDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      kDim
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_kDim = kDim
      m_Dimen=m_nDim+m_kDim
      END                       !!! FoamB_SetnDim

      SUBROUTINE FoamB_GetnDim(nDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      nDim
*     -------------------------------------------------
      nDim = m_nDim
      END                       !!! FoamB_SetnDim

      SUBROUTINE FoamB_GetkDim(kDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      kDim
*     -------------------------------------------------
      kDim = m_kDim
      END                       !!! FoamB_SetkDim

      SUBROUTINE FoamB_GetnCalls(nCalls)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      nCalls
*     -------------------------------------------------
      nCalls = m_nCalls
      END                       !!! FoamB_SetnCalls

      SUBROUTINE FoamB_SetnBuf(nBuf)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      nBuf
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_nBuf = nBuf
      END                       !!! FoamB_SetnBuf

      SUBROUTINE FoamB_SetnBin(nBin)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      nBin
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_nBin = nBin
      END                       !!! FoamB_SetnBin

      SUBROUTINE FoamB_SetOut(Out)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      Out
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_Out = Out
      END                       !!! FoamB_SetOut

      SUBROUTINE FoamB_SetChat(Chat)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      Chat
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_Chat = Chat
      END                       !!! FoamB_SetChat

      SUBROUTINE FoamB_SetnSampl(nSampl)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      nSampl
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_nSampl = nSampl
      END                       !!! FoamB_SetnSampl

      SUBROUTINE FoamB_SetOptDrive(OptDrive)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      OptDrive
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_OptDrive = OptDrive
      END                       !!! FoamB_SetOptDrive

      SUBROUTINE FoamB_SetEvPerBin(EvPerBin)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      EvPerBin
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_EvPerBin = EvPerBin
      END                       !!! FoamB_SetEvPerBin

      SUBROUTINE FoamB_SetOptPeek(OptPeek)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      OptPeek
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_OptPeek = OptPeek
      END                       !!! FoamB_SetOptPeek

      SUBROUTINE FoamB_SetOptEdge(OptEdge)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      OptEdge
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_OptEdge = OptEdge
      END                       !!! FoamB_SetOptEdge

      SUBROUTINE FoamB_SetOptOrd(OptOrd)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      OptOrd
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_OptOrd = OptOrd
      END                       !!! FoamB_SetOptOrd


      SUBROUTINE FoamB_SetOptRanIni(OptRanIni)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      OptRanIni
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_OptRanIni = OptRanIni
      END                       !!! FoamB_SetOptRanIni

      SUBROUTINE FoamB_SetOptRanLux(OptRanLux)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER      OptRanLux
*     -------------------------------------------------
      CALL FoamB_PreInitialize
      m_OptRanLux = OptRanLux
      END                       !!! FoamB_SetOptRanLux

      SUBROUTINE FoamB_Check(mout,level)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//  Checks all pointers, It is is a usefull autodiagnostic.                         //
*//                                                                                  //
*//  level=0, no printout, failures causes STOP                                      //
*//  level=1, printout, failures lead to WARNINGS only                               //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER       mout,level
      INTEGER       nFailures, iCell, Dau1,Dau2, Pare, NoRefs(m_vMax), iVe,n !
      INTEGER       NoEmpty, iError
*     ---------------------------------------------------------
      nFailures=0
      iError   =0
      IF(level.EQ.1) WRITE(mout,*)
     $'//////////////////////////////////////// FoamB_Checks /////////////////////////////////////////////' !
      DO iCell = 1,m_LastCe
         Dau1 = m_CeDau1(iCell)
         Dau2 = m_CeDau2(iCell)
         Pare = m_CePare(iCell)
* checking on parents
         IF(iCell.GT.1) THEN
            IF(Pare.GT.m_LastCe) THEN
               iError   =1
               nFailures = nFailures+1
               IF(level.EQ.1) WRITE(mout,*) '##### iCell= ',iCell,' Parent out of range = ',Pare !
            ENDIF
         ENDIF
         IF(iCell.GT.1) THEN
            IF(  (Pare.NE.1) .AND. (m_CeDau1(Pare).NE.iCell) .AND. (m_CeDau2(Pare).NE.iCell)  ) THEN !
               iError   =2
               nFailures = nFailures+1
               IF(level.EQ.1) 
     $              WRITE(mout,*) '##### iCell= ',iCell,' Parent not pointing to this daughter Pare= ',Pare !
            ENDIF
         ENDIF
* checking on daughters
         IF( Dau1 .GT. m_LastCe ) THEN
            iError   =3
            nFailures = nFailures+1
            IF(level.EQ.1) WRITE(mout,*) '##### iCell= ',iCell,' First  Daugter out of range Dau1= ',Dau1 !
         ENDIF
         IF( Dau2 .GT.m_LastCe ) THEN
            iError   =4
            nFailures = nFailures+1
            IF(level.EQ.1) WRITE(mout,*) '##### iCell= ',iCell,' Second Daugter out of range Dau2= ',Dau2 !
         ENDIF
         IF( Dau1.GE.1 .AND. Dau1.LE. m_LastCe) THEN
            IF( m_CePare(Dau1).NE.iCell ) THEN
               iError   =5
               nFailures = nFailures+1
               IF(level.EQ.1) 
     $              WRITE(mout,*) '##### iCell= ',iCell,' First  Daugter not pointing to parent Dau1= ',Dau1 !
            ENDIF
         ENDIF
         IF( Dau2.GE.1 .AND. Dau2.LE. m_LastCe) THEN
            IF( m_CePare(Dau2).NE.iCell ) THEN
               iError   =6
               nFailures = nFailures+1
               IF(level.EQ.1) 
     $              WRITE(mout,*) '##### iCell= ',iCell,' Second Daugter not pointing to parent Dau2= ',Dau2 !
            ENDIF
         ENDIF 
      ENDDO
* check on vertices
      DO iVe = 1, m_LastVe
         NoRefs(iVe)=0
      ENDDO
      DO iVe = 1, m_LastVe
         DO iCell = 1, m_LastCe
            DO n=1,m_nDim+1
               IF( iVe .EQ. m_CeVert(iCell,n) ) NoRefs(iVe) =1
            ENDDO
         ENDDO
      ENDDO
      DO iVe = 1, m_LastVe
         IF(NoRefs(iVe).EQ.0 .AND.  level.EQ.1) WRITE(mout,*) '***** Vertex no. ',iVe, '  NOT referenced!' !
      ENDDO
* Check for empty cells
      NoEmpty = 0d0
      DO iCell = 1,m_LastCe
         IF( m_CeStat(iCell).EQ.1 ) THEN
            IF( m_CeDriv(iCell) .EQ. 0d0) NoEmpty = NoEmpty +1
         ENDIF
      ENDDO
      IF( NoEmpty.GT.0) THEN
         WRITE(mout,*) '++++++++++ FoamB_Check: !!! WARNING!!!! Empty Cells found NoEmpty= ',NoEmpty !
         WRITE(   *,*) '++++++++++ FoamB_Check: !!! WARNING!!!! Empty Cells found NoEmpty= ',NoEmpty !
      ENDIF
* summary
      IF(level.EQ.1) WRITE(mout,*) '++++++++++ FoamB_Check has found total ', nFailures, ' failures ' !
      IF(level.EQ.1) WRITE(mout,*)
     $'///////////////////////////////////////////////////////////////////////////////////////////////////' !
      IF(level.EQ.0 .AND. nFailures.GT.0 ) THEN
         WRITE(mout,*) '++++++++++ STOP in FoamB_Check, found total ', nFailures, ' failures ' !
         WRITE(mout,*) '++++++++++ iErro= ',iError
         STOP
      ENDIF
      END                       ! FoamB_Check



      SUBROUTINE FoamB_BufPrint(mout)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   all cells                                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            iCell,mout,active,j
*     ----------------------------------------------------------------
      active  =0
      WRITE(mout,'(3a)') '==============================================', ' ALL-CELLS ',
     $                   '=============================================='
      WRITE(mout,'(3a)') 
     $     ' iCell  Stat  Pare  Dau1  Dau2 kBest       Xave     Volume      Drive  TrueInteg  Ver1  Ver2  ...'
      DO iCell = 1, m_LastCe
         WRITE(mout,'(6i6,4f11.5,20i6)')
     $        iCell, m_CeStat(iCell),  m_CePare(iCell), m_CeDau1(iCell), m_CeDau2(iCell), !
     $        m_CeBest(iCell),                          ! pointer to best division
     $        m_CeXave(iCell),                          ! factor for Best division 
     $        m_CeVolu(iCell),                          ! Cartesian Volume
     $        m_CeDriv(iCell),                          ! Drive 
     $        m_CeIntg(iCell),                          ! TrueInteg
     $        (m_CeVert(iCell,j), j=1,m_nDim+1)         ! vertices
         IF(m_kDim .GT. 0 ) THEN
            WRITE(mout,'(a,(50g11.6))')
     $           '       HypCubs Posit&Size: ',
     $           (m_CeVer1(iCell,j), j=1,m_kDim), ! position
     $           (m_CeVer2(iCell,j), j=1,m_kDim)  ! size
         ENDIF
         IF(m_CeStat(iCell).EQ.1) active  = active +1
      ENDDO
      WRITE(mout,*) ' All cells: ',m_LastCe, ' Active: ', active
      END                       !! FoamB_BufPrint



      SUBROUTINE FoamB_BufActPrint(mout)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   Active cells only                                                              //
*//   Side=1 indicates that this cell is "side leaf" sticking out of main branch     //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            iCell,mout,active,Side,Pare,j
      DOUBLE PRECISION   DriRat,IntgSum,DrivSum,Fact,WtMin,WtMax
      DOUBLE PRECISION   AveWt, Sigma, Nentry
*     ----------------------------------------------------------------
      WRITE(mout,'(3a)') '==================================================', ' ACTIVE CELLS ',
     $                   '=================================================='
      IntgSum =0d0
      DrivSum =0d0
      WtMin   =  1d60
      WtMax   = -1d60
      active  =0
      WRITE(mout,'(2a)') ' iCell Stat Pare Dau1 Dau2',
     $   '     WtMin      WtMax        <w>  sigma/<w>     Volume      Drive    TrueInt   Ver1  Ver2 ...' !
      DO iCell = 1, m_LastCe
         IF(m_CeStat(iCell).EQ.1) THEN
            Nentry = m_CeSum(iCell,3)
            AveWt  = m_CeSum(iCell,1)/m_CeSum(iCell,3)
            Sigma  = DSQRT(  ABS(m_CeSum(iCell,2)/Nentry - AveWt**2))
            IF(AveWt.NE.0d0) WtMin = Min( WtMin, m_CeSum(iCell,4)/AveWt)
            IF(AveWt.NE.0d0) WtMax = Max( WtMax, m_CeSum(iCell,5)/AveWt)
            IF(AveWt.NE.0d0) Sigma = Sigma/AveWt
            WRITE(mout,'(5i5, 7f11.5 ,10i5)') 
     $           iCell, m_CeStat(iCell),  m_CePare(iCell),  m_CeDau1(iCell), m_CeDau2(iCell), !
     $           m_CeSum(iCell,4),       ! minWt/AveWt
     $           m_CeSum(iCell,5),       ! maxWt/AveWt
     $           m_CeIntg(iCell) /(m_CeDriv(iCell)+1d-100), ! average weight
     $           Sigma/(AveWt+1d-100),   ! sigma/AveWt
     $           m_CeVolu(iCell),        ! Cartesian volume
     $           m_CeDriv(iCell),        ! Driver Integral
     $           m_CeIntg(iCell),        ! True   Integral
     $           (m_CeVert(iCell,j), j=1,m_nDim+1) ! vertices
            IF(m_kDim .GT. 0 ) THEN
               WRITE(mout,'(a,(50g11.6))')
     $              '      HypCubs Posit&Size: ',
     $              (m_CeVer1(iCell,j), j=1,m_kDim), ! position
     $              (m_CeVer2(iCell,j), j=1,m_kDim)  ! size
            ENDIF
            IntgSum = IntgSum +m_CeIntg(iCell)
            DrivSum = DrivSum +m_CeDriv(iCell)
            active  = active +1
         ENDIF
      ENDDO
      WRITE(mout,'(a,i6,a,2i6)') 'All cells: ',m_LastCe, '      Active: ', active, m_LastAc !
      WRITE(mout,'(a,2f12.5)')   'Minimum and Maximum Weight/<Wt>       = ',WtMin,WtMax !
      WRITE(mout,'(a,2g20.13)')  'Total True Integral in active cells   = ', IntgSum, m_CeIntg(1) !
      WRITE(mout,'(a,2g20.13)')  'Total Driver Integral in active cells = ', DrivSum, m_CeDriv(1) !
      WRITE(mout,'(a,f12.5)')    'True/Drive = ', IntgSum/DrivSum
      END                       !! FoamB_BufActPrint


      SUBROUTINE FoamB_VertPrint(mout)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   all vertices                                                                   //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            mout, iCell, iVe, NoRefs(m_vMax), NoRefsAc(m_vMax), k,j
*     ----------------------------------------------------------------
      DO iVe = 1, m_LastVe
         NoRefs(iVe)=0
      ENDDO
      DO iVe = 1, m_LastVe
         DO iCell = 1, m_LastCe
            DO k=1,m_nDim+1
               IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefs(iVe) =NoRefs(iVe) +1 !
               IF(m_CeStat(iCell) .EQ. 1) THEN
                  IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefsAc(iVe) =NoRefsAc(iVe) +1 !
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      WRITE(mout,*) '=====================ALL VERTICES===================================' !
      WRITE(mout,*) ' iVert   NoRefs  NoRefsAc       Vertex     Componets    ' !
      DO iVe = 1, m_LastVe
         WRITE(mout,'(i6,2i10,5f17.10)') iVe,NoRefs(iVe),NoRefsAc(iVe), (m_VerX(iVe,j),j=1,m_nDim) !
      ENDDO
      END                       !! VertPrint



      SUBROUTINE FoamB_PltBegin(ltx)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Initialization, write header of TeX file                                       //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER   ltx
*---------------------------------------------------
      m_ltx  = ABS(ltx)
      IF(ltx .GT. 0 ) THEN
         WRITE(m_ltx,'(2A)') '\\newpage'
      ELSE
*------------------------------!
*           Header
*------------------------------!
         WRITE(m_ltx,'(A)') '\\documentclass[12pt]{article}'
         WRITE(m_ltx,'(A)') '\\usepackage{color}' !<-for colors!!!
         WRITE(m_ltx,'(A)') '\\usepackage{epic}' !<-for extended ploting
         WRITE(m_ltx,'(A)') '\\textwidth  = 16cm'
         WRITE(m_ltx,'(A)') '\\textheight = 18cm'
         WRITE(m_ltx,'(A)') '\\pagestyle{empty}'
         WRITE(m_ltx,'(A)') '\\begin{document}'
         WRITE(m_ltx,'(A)') '  '
         WRITE(m_ltx,'(A)') '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' !
         WRITE(m_ltx,'(A)') '\\begin{figure}[!ht]'
         WRITE(m_ltx,'(A)') '\\centering'
*------------------------------!
* Frames and labels
*------------------------------!
         WRITE(m_ltx,'(A)') '% =========== big frame, title etc. ======='
         WRITE(m_ltx,'(A)') '\\setlength{\\unitlength}{0.1mm}'
         WRITE(m_ltx,'(A)') '\\begin{picture}(1600,1600)'
         WRITE(m_ltx,'(A)') '\\put(0,0){\\framebox(1600,1600){ }}'
      ENDIF
      END

      SUBROUTINE FoamB_PltVert     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Plot all vertices                                                              //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            iCell, iVe, NoRefs(m_vMax), NoRefsAc(m_vMax), k,j
*------------------------------------------------------------------------------
*     Mark plots for plots
      CHARACTER*62 star,diamond,circle,ring,times,disc,plus,box,dot
      PARAMETER (diamond ='\\makebox(0,0){\\Large $\\diamond$}')
      PARAMETER (star    ='\\makebox(0,0){\\Large $\\star$}')
      PARAMETER (circle  ='\\circle{30}')
      PARAMETER (ring    ='\\circle{20}')
      PARAMETER (times   ='\\makebox(0,0){\\Large $\\times$}')
      PARAMETER (disc    ='\\circle*{20}')
      PARAMETER (plus    ='\\makebox(0,0){\\Large $+$}')
      PARAMETER (box     ='\\makebox(0,0){\\Large $\\Box$}') !!! does not work???
      PARAMETER (dot     ='\\circle*{10}')
*------------------------------------------------------------------------------
      CHARACTER*62  chmark
      INTEGER       kx,ky
*---------------------------------------------------------------------------------------------
      IF(m_nDim.EQ.2) THEN
* Count references of vertices
         DO iVe = 1, m_LastVe
            NoRefs(iVe)=0
         ENDDO
         DO iVe = 1, m_LastVe
            DO iCell = 1, m_LastCe
               DO k=1,m_nDim+1
                  IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefs(iVe) =NoRefs(iVe) +1 !
                  IF(m_CeStat(iCell) .EQ. 1) THEN
                     IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefsAc(iVe) =NoRefsAc(iVe) +1 !
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
*---------------------------------------------------------------------------------------------
* Begin frame
         WRITE(m_ltx,'(A)') '% =========== Vertices Vertices ==========='
         WRITE(m_ltx,'(A)') '\\put(0,0){\\begin{picture}( 1600,1600)' !
         WRITE(m_ltx,'(A)') '\\put(0,0){\\framebox( 1600,1600){ }}' !
* Plotting symbol
         IF(m_LastVe.LE.250) THEN
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VD}[2]{\\put(#1,#2){',disc,'}}' !
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VS}[2]{\\put(#1,#2){',star,'}}' !
         ELSE
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VD}[2]{\\put(#1,#2){',dot,'}}' !
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VS}[2]{\\put(#1,#2){',dot,'}}' !
         ENDIF
         WRITE(m_ltx,'(10A)') 
     $        '\\newcommand{\\VN}[3]{\\put(#1,#2){\\makebox(0,0)[b]{\\hbox{\\color{red}\\scriptsize #3}}}}' !
         DO iVe = 1, m_LastVe
            kx = m_VerX(iVe,1)*1600
            ky = m_VerX(iVe,2)*1600
*****         WRITE(*,*) NoRefs(iVe),NoRefsAc(iVe)
            IF( NoRefsAc(iVe).LE.2 ) THEN
               WRITE(m_ltx,'(A,I5,A,I5,A)') '\\VD{',kx,'}{',ky,'}'
            ELSE
               WRITE(m_ltx,'(A,I5,A,I5,A)') '\\VS{',kx,'}{',ky,'}'
            ENDIF            
            IF(m_LastVe.LE.250) WRITE(   m_ltx,'(A,I5,A,I5,A,I5,A)') '\\VN{',kx-8,'}{',ky+12,'}{',iVe,'}' !
         ENDDO
* Close frame
         WRITE(m_ltx,'(A)') '\\end{picture}}'
         WRITE(m_ltx,'(A)') '% ============== End Vertices ==========='
      ENDIF
*---------------------------------------------------------------------------------------------
      END                       !! VertPrint


      SUBROUTINE FoamB_PltCell     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Plot all simplectic cells                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
      INTEGER            iCell,active,j
      INTEGER            iV1,iV2,iV3
      INTEGER            kx1,ky1,kx2,ky2,kx3,ky3,kx,ky,lx,ly
*---------------------------------------------------------------------------------------------
*                        Rectangular 2-dim FOAM
*---------------------------------------------------------------------------------------------
      IF(m_kDim.EQ.2) THEN
         WRITE(m_ltx,'(A)') '% =========== Vertices Vertices ==========='
         WRITE(m_ltx,'(A)') '\\put(0,0){\\begin{picture}( 1600,1600)' !
         DO iCell = 2, m_LastCe
            IF(m_CeStat(iCell).EQ.1) THEN ! Only active cells
               kx = m_CeVer1(iCell,1)*1600
               ky = m_CeVer1(iCell,2)*1600
               lx = m_CeVer2(iCell,1)*1600
               ly = m_CeVer2(iCell,2)*1600
               kx1 = kx+lx/2
               ky1 = ky+ly/2
*     cell rectangle
               WRITE(m_ltx,'(A,I5,A,I5,A,I5,A,I5,A)') 
     $              '\\put(',kx,',',ky,'){\\color{black}\\framebox(',lx,',',ly,'){ }}' !
***               WRITE(m_ltx,'(A,I5,A,I5,A,I5,A,I5,A)') 
***     $              '\\put(',kx,',',ky,'){\\color{black}\\dashbox{7}(',lx,',',ly,'){ }}' !
*     cell number
               IF(m_LastCe.LE.250) WRITE(m_ltx,'(A,I4,A,I4,A,I4,A)') 
     $            '\\put(',kx1,',',ky1,'){\\makebox(0,0)[b]{\\hbox{\\small\\color{magenta}\\scriptsize ',iCell,' }}}' !
            ENDIF
         ENDDO
* Close frame
         WRITE(m_ltx,'(A)') '\\end{picture}}'
         WRITE(m_ltx,'(A)') '% ============== End Vertices ==========='
      ENDIF
*---------------------------------------------------------------------------------------------
*                        Triangulare 2-dim FOAM
*---------------------------------------------------------------------------------------------
      IF(m_nDim.EQ.2) THEN
         WRITE(m_ltx,'(A)') '% =========== Vertices Vertices ==========='
         WRITE(m_ltx,'(A)') '\\put(0,0){\\begin{picture}( 1600,1600)' !
         DO iCell = 2, m_LastCe
            iV1=m_CeVert(iCell,1)
            iV2=m_CeVert(iCell,2)
            iV3=m_CeVert(iCell,3)
            kx1 = m_VerX(iV1,1)*1600
            ky1 = m_VerX(iV1,2)*1600
            kx2 = m_VerX(iV2,1)*1600
            ky2 = m_VerX(iV2,2)*1600
            kx3 = m_VerX(iV3,1)*1600
            ky3 = m_VerX(iV3,2)*1600
            kx= (kx1+kx2+kx3)/3
            ky= (ky1+ky2+ky3)/3
            IF(m_CeStat(iCell).EQ.1) THEN
***         WRITE(*,*) iCell,iV1,iV2,iV3
               WRITE(m_ltx,'(A,I4,A,I4,A,I4,A,I4,A)') '\\drawline(',kx1,',',ky1,')(',kx2,',',ky2,')' !
               WRITE(m_ltx,'(A,I4,A,I4,A,I4,A,I4,A)') '\\drawline(',kx2,',',ky2,')(',kx3,',',ky3,')' !
               WRITE(m_ltx,'(A,I4,A,I4,A,I4,A,I4,A)') '\\drawline(',kx3,',',ky3,')(',kx1,',',ky1,')' !
               IF(m_LastCe.LE.250) WRITE(m_ltx,'(A,I4,A,I4,A,I4,A)') 
     $              '\\put(',kx,',',ky,'){\\makebox(0,0)[b]{\\hbox{\\color{magenta}\\scriptsize ',iCell,' }}}' !
            ENDIF
         ENDDO
* Close frame
         WRITE(m_ltx,'(A)') '\\end{picture}}'
         WRITE(m_ltx,'(A)') '% ============== End Vertices ==========='
      ENDIF
      END                       !! FoamB_BufPrint

      SUBROUTINE FoamB_PltMisc     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   Miscelaneous                                                                   //
*//   Plot of frame for 2-dim void testing function                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
*
      WRITE(m_ltx,'(A)') '\\put(80,80){\\color{blue}\\dashbox{7}( 1440,1440){ }}' ! 5% edge band
      END                       !! FoamB_PltMisc

      SUBROUTINE FoamB_PltEnd     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Close Tex file with plot                                                       //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamB.h'
*
      WRITE(m_ltx,'(A)') '\\end{picture}'
      WRITE(m_ltx,'(A)') '\\end{figure}'
      WRITE(m_ltx,'(A)') '\\end{document}'
      CLOSE(m_ltx)
      END

*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
*///////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                       //
*//             Foam Version 2.02                                                         //
*//             November 2000                                                             //
*//                                                                                       //
*//  N-dimensional general purpose Monte Carlo sampler                                    //
*//  with the self-adapting simplical and hyper-cubical grid                              //
*//                                                                                       //
*//             Author:   Stanislaw JADACH                                                //
*//             Address:  INP Cracow, DESY-Zeuthen                                        //
*//             Email:    S.Jadach@cern.ch, S.Jadach@ifj.edu.pl                           //
*//             HomePage: http://home.cern.ch/jadach/                                     //
*//                                                                                       //
*//  First version 1.00 written by S.J. in May 1999 during visit in DESY                  //
*//        version 2.00 written by S.J. in Aug 2000 during visit in DESY-Zeuthen          //
*///////////////////////////////////////////////////////////////////////////////////////////


*////////////////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                                    //
*//          Pseudoclass Foam                                                                          //
*//                                                                                                    //
*//                                                                                                    //
*//                      Initialization of the grid                                                    //
*//  FoamC_PreInitialize                 : Pre-initialization, set all default values (constructor?)   //
*//  FoamC_Initialize(FunW)              : Initialization of the grid etc.                             //
*//  FoamC_InitVertices                  : Initializes first vertices of the basic cube                //
*//  FoamC_InitCells                     : Initializes first n-factorial cells inside original cube    //
*//  FoamC_DefCell                       : Create new (daughter) cell and append at end of the buffer  //
*//  FoamC_Explore(iCell,funW)           : Short MC sampling in iCell, determine <wt>, wtMax etc.      //
*//  FoamC_RanDiscr(Driv,nTot,Power,iRand) : Random choice of cell division direction                  //
*//  FoamC_MakeAlpha(Alpha)              : auxiliary procedure for FoamC_Explore                       //
*//  FoamC_MakeLambda(Lambda)            : auxiliary procedure for FoamC_Explore                       //
*//  FoamC_Determinant(R,Det)            : determinant of matrix R                                     //
*//  FoamC_Det2Lapl(R,i1,i2)             : Laplace formula for 1-dim. determinant                      //
*//  FoamC_Det3Lapl(R,i1,i2,i3)          : Laplace formula for 2-dim. determinant                      //
*//  FoamC_Det4Lapl(R,i1,i2,i3,i4)       : Laplace formula for 3-dim. determinant                      //
*//  FoamC_Det5Lapl(R,i1,i2,i3,i4,i5)    : Laplace formula for 4-dim. determinant                      //
*//  FoamC_Grow(funW)              : grow cells until buffer is full                                   //
*//  FoamC_PeekMax(iCell)          : choose randomly one cell, used also in MC generation              //
*//  FoamC_PeekRand1(iCell)        : Generates randomly one (active) cell pointer iCell                //
*//  FoamC_Divide(iCell,funW,RC)   :Divide iCell into two daughters; iCell retained, taged as inactive //
*//  FoamC_ActUpda                 : Creates list of active cells (pointers)                           //
*//                     Generation                                                                     //
*//  FoamC_MakeEvent(Density)      : Generates point/vector Xrand with the weight MCwt                 //
*//  FoamC_CellGener(Density)      : Cooses randomly one of active cells                               //
*//  FoamC_GetMCvector(MCvector)   : Provides point/vector MCvector generated by  MakeEvent            //
*//  FoamC_GetMCwt(MCwt)           : Provides MCwt, MC weight calculated by MakeEvent                  //
*//  FoamC_MCgenerate(funW,X,MCwt) : Alternative entry, Generates point X with the weight MCwt         //
*//                     Finalization                                                                   //
*//  FoamC_Finalize(MCresult,MCerror)    : Calculates integral and its error after (only from) MC run  //
*//                     Other Getters and Setters                                                      //
*//  MCellA_GetTotPrim(TotPrim)    :Provides Primary integral used in MC generation                    //
*//  FoamC_SetnDim(nDim)           :Sets no. of dimensions simplices   (to be called before Initialize)//
*//  FoamC_SetkDim(kDim)           :Sets no. of dimensions hypercubics (to be called before Initialize)//
*//  FoamC_GetnDim(nDim)           :Provides nDim, miscelaneous, for tests                             //
*//  FoamC_SetnBuf(nBuf)           :Sets nBuf, working area in buffer                                  //
*//  FoamC_SetnBin(nBin)           :Sets nBin, no of bins in histo in exploration <nBinMax=256         //
*//  FoamC_SetOut(Out)             :Sets output unit number                                            //
*//  FoamC_SetChat(Chat)           :Sets chat level; Chat=0,1,2 chat level in output, Chat=1 normal    //
*//  FoamC_SetnSampl(nSampl)       :Sets nSampl; No of MC sampling before dividing cell                //
*//  FoamC_SetOptDrive(OptDrive)   :Sets OptDrive; type of Drive =0,1,2 for True,Sigma,WtMax           //
*//  FoamC_SetOptPeek              :Sets type of method in cell division                               //
*//  FoamC_SetOptEdge(OptEdge)     :Sets OptEdge; decides whether vertices are included in the sampling//
*//  FoamC_SetOptRanIni(OptRanIni) :Sets OptRanIni=0,1, for rand. numb. initialization inside/outside  //
*//  FoamC_SetOptRanLux(OptRanLux) :Sets OptRanLux=-1,0,1,2,3 raand.numb.gen. level                    //
*//  FoamC_GetnCalls(nCalls)       :Get total no of function calls                                     //
*//                    Debugging and miscelaneous                                                      //
*//  FoamC_Check(mout,level)       :Checks all pointers (after comression) debuging!                   //
*//  FoamC_BufPrint(mout)          :Prints all cells, debugging                                        //
*//  FoamC_BufActPrint(mout)       :Prints all active cells, debugging                                 //
*//  FoamC_VertPrint(mout)         :Prints all vertices,  debugging                                    //
*//  FoamC_PltBegin                :Ploting 2-dim. cells and vertices                                  //
*//  FoamC_PltVert(mout)           :Ploting 2-dim. cells and vertices                                  //
*//  FoamC_PltCell(mout)           :Ploting 2-dim. cells and vertices                                  //
*//  FoamC_PltEnd                  :Ploting 2-dim. cells and vertices                                  //
*//                                                                                                    //
*////////////////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                                    //
*//  Input parameters:                                                                                 //
*//    nDim     number of dimensions for simplices, for the moment nDim=1-5 in this version,           //
*//             n>5 requires re-writing FoamC_Determinant,                                             //
*//    kDim     number of dimensions for hypercubics                                                   //
*//    Dimen    total number of dimensions = nDim +kDim                                                //
*//             for n=1 alternatively Foam1A may be used, could be factor 2 faster!                    //
*//    nBuf     Actual dynamic lenth of the buffer m_nBuf<m_nBufMax. For strongly peaked distribution  //
*//             nBuf should be as large as possible, this will increase CPU time in initialization     //
*//             MC generation is weakly affected by increasing nBuf                                    //
*//    nSampl   No of sampling when dividing cell, nSampl=10-100 is OK, further increase improves      //
*//             costs CPU time and apparently does not increase grid efficiency too much.              //
*//             This should be checked however for every new distribution.                             //
*//    OptDrive Type of Drive =0,1,2 for TrueInt,Sigma,WtMax,  Drive=WtMax is the best if we aim       //
*//             at rejection leading to wt=1 events. If not then Drive=Sigma iswiser choice leading    //
*//             to save of CPU time.  Simplistic Drive=Sigma is correct but not recommeneded (ineffic.)//
*//    OptEdge  decides whether vertices are included in the sampling. Default  OptEdge=1 causes that  //
*//             vertices at the edge of simplex cells are included always in MC exploration            //
*//             of the cell. In the case of density distrib. with weak integrable singularities        //
*//             at the edges it may be not possible and OptEdge=0 may help.                            //
*//    OptOrd decides whether the integration domain in simplical subspace is unit cube or simplex.    //
*//             It is active only for  nDim=2,3,4. For OptOrd=0 (default) the initial domain is unit   //
*//             hypercubic split into nDim! simplices in the initialization phase.                     //
*//             If m_OptOrd=1 then initial domain is simplex and the integration variables providede   //
*//             by FoamC_GetMCvector(V) are ordered: V(1)<V(2)<...<V(nDim)<1.                          //
*//             Hyp-cubical variables  0<V(nDim+1), V(nDim+2),...,V(nDim+kDim)<1 are never ordered.    //
*//    EvPerBin Limit on effective number of eevents per bin in exploration phase. This option saves   //
*//             considerably CPU time.      =0 option is desabled, recommended value is >20.           //
*//    Out      Miscelaneous. Output unit number.                                                      //
*//    Chat     Miscelaneous. Chat=0,1,2 chat level in output, Chat=1 normal level.                    //
*//                                                                                                    //
*//                                                                                                    //
*////////////////////////////////////////////////////////////////////////////////////////////////////////
*//  Terminology:                                                                                      //
*//    "Active cells" are these which did not divide and are eligible for division                     //
*//  Remarks:                                                                                          //
*//    List of active cells is not realy necessary, but let us keep it for possible                    //
*//    future developements or tests.                                                                  //
*////////////////////////////////////////////////////////////////////////////////////////////////////////


      SUBROUTINE FoamC_PreInitialize     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Presets certain defaults for switches and other and regualtory parameters.     //
*//   They Can be reset with setters                                                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER i,j
*
      INTEGER   m_Magic        ! Magic cookie, to avoid multiple initialization
      DATA      m_Magic /378231178/
      SAVE      m_Magic
*     -------------------------------------------------
      IF(m_Magic .EQ. 378231178 ) THEN
         m_MagicInit = m_Magic
         m_Magic     = 0
      ENDIF
      IF(m_MagicInit .NE. 378231178 ) RETURN
      WRITE(m_out,*) '===============FoamC_PreInitialize=============='
      m_MagicInit= 0
      m_nDim     = 0                 ! dimension siplices
      m_kDim     = 0                 ! dimension hypercubics
      m_Dimen    = m_nDim+m_kDim     ! dimension total
      m_nBuf     = 1000              ! Actual dynamic lenth of the buffer m_nBuf<m_nBufMax
      m_nBin     = 8                 ! No of bins in cell exploration/division  <nBinMax  
      m_nSampl   = 200               ! No of sampling when dividing cell
      m_OptDrive = 2                 ! type of Drive =0,1,2 for TrueVol,Sigma,WtMax
      m_OptEdge  = 0                 ! decides whether vertices are included in the sampling
      m_OptPeek  = 0                 ! type of Peek =0,1 for maximum, random
      m_Out      = 6                 ! Output unit
      m_Chat     = 1                 ! Chat=0,1,2 chat level in output, Chat=1 normal level
      m_OptOrd   = 0                 ! Entire integr domain is hyp-cubic if =0 and simplex if =1
      m_EvPerBin = 25                ! Upper limit on eevents/bin in exploration, =0 disabled 
      m_OptRanIni= 1                 ! Rand.num. generator initialized (=1) inside PianA
      m_OptRanLux= 3                 ! Rand.num. generator level (-1 for PseuMar)
*
      m_LastCe   = 0                 ! length of dynamical buffor for cells
      m_LastVe   = 0                 ! length of dynamical buffor for vertices
* Clean up list of simplical vertices
      DO i=1,m_vMax
         DO j=1,m_nDim
            m_VerX(i,j) = 0d0        ! vertices
         ENDDO
      ENDDO
* Clean up hypercubic parameters
      DO i=1,m_nBufMax
         DO j=1,m_KdiMax
            m_CeVer1(i,j)=0d0        ! hcubic position
            m_CeVer2(i,j)=0d0        ! hcubic size
         ENDDO
      ENDDO
*
      m_nCalls   = 0                 ! No of function calls
*
      END

      SUBROUTINE FoamC_Flush        !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////*//                                                                                  //
*//   Use this before re-initialization of the Foam                                  //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      m_MagicInit = 378231178   ! Reset Magic cookie, alowing initialization
      WRITE(m_out,*) '===============FoamC_Flush=============='
      CALL FoamC_PreInitialize
      END

      SUBROUTINE FoamC_Initialize(FunW)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Basic initialization, create "foam of cells"                                   //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            i,j,k,iCell,kCell
      DOUBLE PRECISION   TotPrim
*---------------------------------------------
      CALL FoamC_PreInitialize
* User may prefers to initialize r.n.gen. by himsef, the initialization below can be inhibited 
      IF( m_OptRanIni.EQ. 1 ) THEN
         IF(m_OptRanLux.EQ.-1) THEN
            CALL PseuMar_Initialize(54217137, 0, 0) ! Initialization of random number generator
         ELSEIF(m_OptRanLux.GE.0 .AND. m_OptRanLux.LE.4 ) THEN
            CALL RLUXGO( m_OptRanLux, 54217137,0,0) ! Initialization of random number generator
         ELSE
            WRITE(*,    *) ' ### STOP in FoamC_Initialize, wrong OptRanLux =',m_OptRanLux !
            WRITE(m_Out,*) ' ### STOP in FoamC_Initialize, wrong OptRanLux =',m_OptRanLux !
         ENDIF
      ENDIF
      IF( (m_nBuf .GT. m_nBufMax) .OR. (m_nBuf .LE. 0) ) THEN
         WRITE(*,    *) ' ### STOP in FoamC_Initialize, wrong m_nBuf =',m_nBuf !
         WRITE(m_Out,*) ' ### STOP in FoamC_Initialize, wrong m_nBuf =',m_nBuf !
         STOP
      ENDIF
      IF( (m_nBin .GT. m_nBinMax) .OR. (m_nBin .LE. 0) ) THEN
         WRITE(*,    *) ' ### STOP in PianA_Initialize, wrong m_nBin =',m_nBin !
         WRITE(m_Out,*) ' ### STOP in PianA_Initialize, wrong m_nBin =',m_nBin !
         STOP
      ENDIF
      IF( m_Dimen .LE. 0 ) THEN
         WRITE(*,    *) ' ### STOP in FoamC_Initialize, wrong m_Dimen =',m_Dimen !
         WRITE(m_Out,*) ' ### STOP in FoamC_Initialize, wrong m_Dimen =',m_Dimen !
         STOP
      ENDIF
* First  cells are the (nDim)! simplices from division of the basic unit cube
      CALL FoamC_InitVertices
      CALL FoamC_InitCells(funW)

***** CALL FoamC_VertPrint(6)   ! debug
***** CALL FoamC_BufPrint(6)    ! debug

      CALL  FoamC_Grow(funW)
* Update list of active cells, only for internal tests
      CALL FoamC_ActUpda
      CALL FoamC_Check(6,0)     ! Check if the liked list is OK
* --------------------------------------------------------------------------------------------
      IF( m_Chat.GE.1) THEN
         WRITE(m_Out,'( 3(a,i4),2(a,g15.8) )') 
     $      'Foam_Initialize:  No. Cells=',m_LastCe,'  Active=',m_LastAc, '  No. Vertices=' ,m_LastVe, !
     $      '  True Integ.=',m_CeIntg(1),'  Driver Integ.=',m_CeDriv(1)  !
      ENDIF
      IF( m_Chat.EQ.2) THEN
         CALL FoamC_BufPrint(    m_Out)
         CALL FoamC_BufActPrint( m_Out)
         CALL FoamC_VertPrint(   m_Out)
      ENDIF
* Initializations for M.C. generation
      m_Drive  = m_CeDriv(1)  ! M.C. generation Drive value of integral
      m_SumWt  = 0d0          ! M.C. generation sum of Wt
      m_SumWt2 = 0d0          ! M.C. generation sum of Wt**2
      m_NevGen  = 0d0         ! M.C. generation sum of 1d0
      m_WtMax  = -1d99        ! M.C. generation maximum wt
      m_WtMin  =  1d99        ! M.C. generation minimum wt
      m_VolTot = m_CeIntg(1)  ! Estimate of integral tot. without error
      m_MCresult = m_VolTot   ! M.C. generation Final value of ITEGRAL, temporary asignment
      m_MCerror  = m_VolTot   ! M.C. generation Final walue of ERROR  , temporary asignment
*
*((((((((((((################################################################################
*((((((((((((################################################################################
c      CALL FoamC_VertPrint(  6)
c      CALL FoamC_BufPrint(   6)
c      CALL FoamC_BufActPrint(6)
c      CALL FoamC_Check(      6,0)
c      WRITE(*,*) ' ########### STOP ########### developement not finished @@@@' !
c      STOP
*))))))))))))################################################################################
*))))))))))))################################################################################
*
* Cumulative Primary for MC generation, see FoamC_CellGener
      CALL FoamC_ActUpda
      TotPrim = 0d0
      m_CePrCu(0)= 0d0
      DO kCell = 1,m_LastAc
         iCell  = m_ActC(kCell)
         TotPrim = TotPrim +m_CePrim( iCell )
         m_CePrCu(kCell)=TotPrim
      ENDDO
* ----------------------------------------------------
      IF( m_Chat.GE.1) WRITE(m_Out,'( 2(a,g15.8) )')
     $     'Foam_Initialize:  TotPrimary =',TotPrim,'   True/Primary =', m_CeIntg(1)/TotPrim
      END                       ! FoamC_Initialize

      SUBROUTINE FoamC_InitVertices    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   In siplical subspace initialize 2^nDim vertices at corners of basic hyper-cube //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER       iVe,i,j,l,digit( m_NdiMax)
*
      IF( m_OptOrd .EQ.0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*// Here we have all vertices of the initial unit hypercube
*// like (0000),(1000),(0100),(1100),(0010)...(1111)
         m_LastVe = 2**m_nDim
         DO i=1,m_nDim
            digit(i)=0
         ENDDO
         DO iVe=1,m_LastVe
*****    WRITE(*,'(a,i5,a,10i5)') 'iVe=', iVe, '  digit=',(digit(j),j=1,m_nDim)
            DO i=1,m_nDim
               m_VerX(iVe,i) = digit(i) ! Simplical vertex positions
            ENDDO
            digit(1)=digit(1)+1         ! Basic increment
            DO i=1,m_nDim-1
               IF(digit(i).EQ.2) THEN   ! Overflow goes to higher digits
                  digit(i)=0
                  digit(i+1)=digit(i+1)+1
               ENDIF
            ENDDO
         ENDDO
      ELSE
*//////////////////////////////////////////////////////////////////////////////////////
*// Here we have only vertices of the SINGLE  INITIAL SIMPLEX 
*// like (0000),(1000),(1110),(1111)
         m_LastVe = m_nDim+1
         DO iVe=1,m_LastVe
            DO i=1,m_nDim
               m_VerX(iVe,i) =0
               IF(i.LT.iVe) m_VerX(iVe,i) =1  ! Simplical vertex positions
            ENDDO
*****       WRITE(*,'(a,i5,a,10f5.0)') 'iVe=', iVe, '  m_VerX=',(m_VerX(iVe,i),i=1,m_nDim)!
         ENDDO
      ENDIF
      END                       !! FoamC_InitVertices

      SUBROUTINE FoamC_InitCells(funW)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Initialize first n-factorial cells inside original cube                        //
*//   MC exploration done for all newly defined cells                                //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            perm(m_NdiMax),mask,iCeNew,iCell
      INTEGER            iCe,i,j,k,iVe,Vert(m_NdiMax),digit(m_NdiMax),factorial,NoMC !
      DOUBLE PRECISION   HcPosi(m_KdiMax),   HcSize(m_KdiMax)
*     -----------------------------------------------------------------------------
      DO j=1,m_kDim
         HcPosi(j)=0d0
         HcSize(j)=1d0
      ENDDO
      DO iVe=1,m_nDim+1
         Vert(iVe) =0
      ENDDO
      IF( m_nDim .EQ. 0 ) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//      Only hypercubics, no simplices                                              //
*//////////////////////////////////////////////////////////////////////////////////////
*        ROOT cell ACTIVE, no daughters
         NoMC= m_nSampl
*        -------------------- Stat,Pare, Dau1, Dau2, MCsampl,VertList, Position,Size,   iCeNew)
         CALL FoamC_DefCell(     1,  -1,   -1,   -1,    NoMC,   Vert, HcPosi,  HcSize, iCeNew) !
         CALL FoamC_Explore(iCeNew,funW) ! Initial MC sampling
      ELSEIF( m_nDim .EQ. 1 ) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//      Simplical algorithm in one dimension                                        //
*//////////////////////////////////////////////////////////////////////////////////////
         Vert(1) =1
         Vert(2) =2
*        ROOT cell ACTIVE, no daughters
*        -------------------- Stat,Pare, Dau1, Dau2,  MCsampl,VertList, Position,Size,   iCeNew
         CALL FoamC_DefCell(     1,  -1,   -1,   -1, m_nSampl,    Vert, HcPosi,  HcSize, iCeNew) !
         CALL FoamC_Explore(iCeNew,funW) ! Initial MC sampling
      ELSE
         IF( m_OptOrd .EQ.0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//      (nDim)! Simplices in more two and more dimensions                           //
*//////////////////////////////////////////////////////////////////////////////////////
            factorial=1
            DO k = 1,m_nDim
               factorial = factorial*k
            ENDDO
*           ROOT cell INACTIVE, (nDim)! daugter cells defined below
*           ----------------- Stat,Pare, Dau1,        Dau2,  MCsampl, VertList, Position,Size,   iCeNew)
            CALL FoamC_DefCell( -1,  -1,    2, factorial+1, m_nSampl,     Vert, HcPosi,  HcSize, iCeNew) !
* ================================================================
* START OF LOOP OVER permutations of (1,2,3,...,m_nDim)
            iCe=0
            DO i=1,m_nDim
               perm(i)=m_nDim
            ENDDO
 300        CONTINUE
            perm(1)=perm(1)-1
            DO k = 1,m_nDim-1
               IF(perm(k).EQ.0) THEN
                  perm(k)=m_nDim
                  perm(k+1)=perm(k+1)-1
               ENDIF
            ENDDO
            Mask=1
            DO i=1,m_nDim
               DO j=i+1,m_nDim
                  IF( perm(i).EQ.perm(j) ) Mask=0
               ENDDO
            ENDDO
            IF(Mask.EQ.1) THEN
*           At this point NEW PERMUTATION is found !!!
               iCe=iCe+1
               DO iVe=1,m_nDim+1
*              This digit represents single BASIC SIMPLEX, other obtained by permuting dimensions
                  DO k=1,m_nDim
                     digit(k)=0
                     IF(k.LT.iVe) digit(k)=1
                  ENDDO
*                 Translation from "binary" digit to serial pointer of a given vertex
                  j=0
                  DO k=1,m_nDim
                     j=j+  digit(perm(k)) *2**(k-1)
                  ENDDO
                  Vert(iVe)=j+1
               ENDDO
*****    WRITE(*,*) '########>>>>>>> iCe =',iCe, 'permut= ',(perm(i),i=1,m_nDim)
*****    WRITE(*,*) '########>>>>>>> Vert= ',(vert(i),i=1,m_nDim+1)
*              Define ACTIVE simplical cell
*              ----------------- Stat,Pare,Dau1,Dau2,  MCsampl,VertList, Position,Size,   iCeNew
               CALL FoamC_DefCell(  1,   1,  -1,  -1, m_nSampl,    Vert, HcPosi,  HcSize, iCeNew) !
*              ---------------------------------------------------------------------
               IF( iCe.EQ.factorial) GOTO 100 ! Last permutation found
            ENDIF
            GOTO 300            ! END OF LOOP over permutations
* ================================================================
 100        CONTINUE
            DO iCell = 2,m_LastCe
               CALL FoamC_Explore(iCell,funW) ! Initial MC sampling, necessarily in a separate loop
            ENDDO
         ELSE
*//////////////////////////////////////////////////////////////////////////////////////
*//      Single initial simplex in two or more dimension (ordered integr. variables) //
*//////////////////////////////////////////////////////////////////////////////////////
            DO k = 1,m_nDim+1
               Vert(k) =k
            ENDDO
*           ROOT cell ACTIVE, no daughters
*           -------------------- Stat,Pare, Dau1, Dau2,  MCsampl, VertList, Position,Size,    iCeNew)
            CALL FoamC_DefCell(     1,  -1,   -1,   -1, m_nSampl,     Vert, HcPosi,  HcSize,  iCeNew) !
            CALL FoamC_Explore(iCeNew,funW) ! Initial MC sampling
         ENDIF  ! m_OptOrd
      ENDIF  ! m_nDim
      END                       !!!FoamC_InitCells

      SUBROUTINE FoamC_DefCell(Stat,Pare,Dau1,Dau2,NoMC,Vertex,HcPosi,HcSize,iCeNew)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Create new (daughter) cell and append it at the very end of the buffer         //
*//   iCeNew is pointer of the new cell                                              //
*//   Note clever trick: volume of this daughter is assigned initialy half volume    //
*//   of the parent, if parent exists.                                               //
*//   In Explore this value is used to update all parents such that                  //
*//   in the entrire tree parents have volume being sum of all daughter volumes.     //
*//   This summation discipline is useful for MC generation of an active cell by     //
*//   going randomly from top to bottom of the tree.                                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            Stat,Pare,Dau1,Dau2,iCeNew,k,NoMC
      INTEGER            Vertex(m_NdiMax+1)
      DOUBLE PRECISION   HcPosi(m_KdiMax), HcSize(m_KdiMax)
*     ------------------------------------------------------------------
      IF( m_LastCe .EQ. m_nBuf) THEN
         WRITE(*,*) ' STOP in FoamC_DefCell: something wrong with m_nBuf=',m_nBuf !
         STOP
      ENDIF
      m_LastCe = m_LastCe+1
      iCeNew   = m_LastCe
      m_CeStat(iCeNew)= Stat                    ! status code, =0 inactive, =1 active
      m_CePare(iCeNew)= Pare                    ! parent cell pointer
      m_CeDau1(iCeNew)= Dau1                    ! daughter1 cell pointer
      m_CeDau2(iCeNew)= Dau2                    ! daughter2 cell pointer
      m_CeSamp(iCeNew)= NoMC                    ! No of MC events in exploration
      m_CeBest(iCeNew)= -1                      ! pointer for planning division of the cell
      m_CeXave(iCeNew)= 0.5d0                   ! factor for division
*  simplical subspace: vertex list
      DO k=1,m_NdiMax+1
         m_CeVert(iCeNew,k)= Vertex(k)
      ENDDO
*  hypercubical subspace: position and size
      DO k=1,m_kDim
         m_CeVer1(iCeNew,k) = HcPosi(k)
         m_CeVer2(iCeNew,k) = HcSize(k)
      ENDDO
*
      IF(Pare.NE.-1) THEN
         m_CeIntg(iCeNew)= m_CeIntg(Pare)/2d0   ! integr. half of parent
         m_CeDriv(iCeNew)= m_CeDriv(Pare)/2d0   ! integr. half of parent
      ELSE
         m_CeIntg(iCeNew)= 0d0
         m_CeDriv(iCeNew)= 0d0
      ENDIF
      m_CeVolu(iCeNew)= 0d0                     ! cartesian Volume   
      END                       ! FoamC_DefCell


      SUBROUTINE FoamC_Explore(iCell,funW)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Explore newly defined cell with help of special short MC sampling              //
*//   As a result, estimetes of true and Drive volume will be defined                //
*//   Average and dispersion of the weight distribution will be found along each     //
*//   edge and the best edge (minimum dispersion) is memorized for future use.       //
*//   Axerage x for eventual future cell division is also defined.                   //
*//   Recorded are aso minimum and maximu weight etc.                                //
*//   The volume estimate in all (inactive) parent cells is updated                  //
*//   Note that links to parents and initial volume = 1/2 parent has to be           //
*//   already defined prior to calling this routine.                                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            iCell
* Simplical subspace
      DOUBLE PRECISION   Vec(m_NdiMax+1,m_NdiMax)
      DOUBLE PRECISION   Xre(m_NdiMax,  m_NdiMax), Yre(m_NdiMax,  m_NdiMax) !
      DOUBLE PRECISION   Lambda(m_NdiMax),VolPart(m_NdiMax+1)
* Hyper-Cubic subspace
      DOUBLE PRECISION   Alpha( m_KdiMax)
      INTEGER            digit( m_KdiMax)
* Total space
      DOUBLE PRECISION   Vrand( m_DimMax)
* Edge sampling working space, matrices
      DOUBLE PRECISION   Xdivi(m_NdiviMax)
      DOUBLE PRECISION   Histo(m_NdiviMax,m_nBinMax),Xsu1(m_NdiviMax),Xsu2(m_NdiviMax) !
*--------
      INTEGER            loop,i,j,k,parent,iv,jv,kv,nEdges,kBest,iBin,nDivi,NevEff !
      DOUBLE PRECISION   x,x1,x2,Wt,Vsum,xBest,yBest
      DOUBLE PRECISION   Det,VolOld, DriOld, XrSum,Factorial
      DOUBLE PRECISION   Dx,DxHc,DxSp,DxPartial,NoMC
      DOUBLE PRECISION   funW
      EXTERNAL           funW
*-----------------------------------------------------------------------
* memorize old values, will be needed for correcting parent cells
      VolOld = m_CeIntg(iCell)
      DriOld = m_CeDriv(iCell)
*
      Factorial=1
      DO i=1,m_nDim
         Factorial=Factorial*i
      ENDDO
      DxSp=1d0          ! Cartesian volume of the simplex
      DxHc=1d0          ! Cartesian volume of the hypercube
*
      IF( m_nDim.GT.0) THEN
         DO iv=1,m_nDim+1
            DO j=1,m_nDim
               Vec(iv,j) = m_VerX( m_CeVert(iCell,iv) ,j) ! decode vertex vectors
            ENDDO
         ENDDO
         DO iv=1,m_nDim
            DO j=1,m_nDim
               Xre(iv,j) = Vec(iv,j)-Vec(m_nDim+1,j) ! relative to last vertex
            ENDDO
         ENDDO
         CALL FoamC_Determinant(Xre,Det)
         IF(Det.EQ.0d0) WRITE(*,*) '!!!!! WARNING: Determinant=',Det
         DxSp = DxSp*ABS(Det)/Factorial       ! Simplical Cartesian volume of the Cell
      ENDIF
      IF( m_kDim.GT.0) THEN
         DO i=1,m_kDim
            DxHc = DxHc*m_CeVer2(iCell,i) ! Product of sizes in hypercubical subspace
         ENDDO
      ENDIF
      Dx=DxSp*DxHc              ! Total cartesian volume
      m_CeVolu(iCell)  = Dx
c[[[[[[[[[[[[[[[
c      DO iv=1,m_nDim
c          WRITE(*,'(a,9f10.5)') '### Xre=',(Xre(iv,j),j=1,m_nDim)
c      ENDDO
c      WRITE(*,'(a,f12.6)') 'FoamC_Explore: Cartesian volume Dx =',Dx
c]]]]]]]]]]]]]]]

*/////////////////////////////////////////////////////
*//    Exploratory MC sampling to probe the cell    //
*/////////////////////////////////////////////////////
      m_CeSum(iCell,1) =  0
      m_CeSum(iCell,2) =  0
      m_CeSum(iCell,3) =  0
      m_CeSum(iCell,4) =  1d90  ! wtmin
      m_CeSum(iCell,5) = -1d90  ! wtmax
      DO k=1,m_NdiviMax
         Xsu1(k)=0d0
         Xsu2(k)=0d0
         DO iBin=1,m_nBin
            Histo(k,iBin)=0d0
         ENDDO
      ENDDO
*///////////////////////////////////////////////////////////////////////////////////
*     Additional scan over vertices in order to improve max/min weights
*     Note that this option adds 2**m_kDim of the function calls, limited to 1000!
      IF( m_OptEdge .EQ. 1 ) THEN
         DO k=1,m_kDim
            digit(k)=0          ! initialize first partitions
         ENDDO
         DO loop=1,1000         ! start of loop over partitions
            DO iv=1,m_nDim+1
               DO j=1,m_nDim
                  Vrand(j) = m_VerX( m_CeVert(iCell,iv) ,j) ! simplical subspace
               ENDDO
               DO j=1,m_kDim
                  Vrand(m_nDim+j) = m_CeVer1(iCell,j) + digit(j)*m_CeVer2(iCell,j) ! h-cubical subspace
               ENDDO
               Wt =funW(Vrand)*Dx ! weight average normalised to integral over the cell
               m_CeSum(iCell,4) = MIN( m_CeSum(iCell,4),Wt) ! minium weight
               m_CeSum(iCell,5) = MAX( m_CeSum(iCell,5),Wt) ! maximu weight
            ENDDO
            digit(1)=digit(1)+1
            DO k=1,m_kDim-1
               IF(digit(k).EQ.2) THEN
                  digit(k)=0
                  digit(k+1)=digit(k+1)+1
               ENDIF
            ENDDO
            IF( m_kDim .EQ.0 )      GOTO 122
            IF( digit(m_kDim).EQ.2) GOTO 122
         ENDDO                  ! end of loop over partitions
 122     CONTINUE
      ENDIF
*///////////////////////////////////////////////////////////////////////////////////
*     generate randomly/uniformly vector Vrand inside this simplex&hypercube
      DO i=1,m_CeSamp(iCell)
         CALL FoamC_MakeLambda(Lambda)
         CALL FoamC_MakeAlpha( Alpha)
         DO j=1,m_nDim
            Vrand(j) = Vec(m_nDim+1,j)
            DO iv=1,m_nDim
               Vrand(j) = Vrand(j) +Lambda(iv)*Xre(iv,j) ! simplical subspace
            ENDDO
         ENDDO
         DO j=1,m_kDim
            Vrand(m_nDim+j) = m_CeVer1(iCell,j) +Alpha(j)*m_CeVer2(iCell,j) ! hypcubic subspace
         ENDDO
****     WRITE(*,'(a,6f12.6)') ' Lambda    =',(Lambda(k),k=1,m_nDim)
****     WRITE(*,'(a,6f12.6)') ' Alpha     =',(Alpha(k), k=1,m_kDim)
****     WRITE(*,'(a,6f12.6)') ' Vrand     =',(Vrand(k), k=1,m_Dimen)
*///////////////////////////////////////////////////////////////////////////////////
* Projecting on all simplex edges, preparatory step, partial volumes etc.
         IF( m_nDim .GT. 0 ) THEN
            Vsum=0d0
            DO jv=1,m_nDim+1    ! vertex jv will be replaced with Vrand
               kv=0             ! kv numbers all verices axcept jv
               DO iv=1,m_nDim+1
                  IF(iv.NE.jv) THEN
                     kv=kv+1
                     DO j=1,m_nDim
                        Yre(kv,j) = Vec(iv,j)-Vrand(j) ! All vertices except jv relative to Vrand
                     ENDDO
                  ENDIF
               ENDDO
               CALL FoamC_Determinant(Yre,DxPartial)
               VolPart(jv) = ABS(DxPartial)/Factorial
               Vsum=Vsum + VolPart(jv)
            ENDDO
            IF( ABS(Vsum-DxSp)/DxSp .GT. 1d-5) GOTO 950 ! X-check
         ENDIF
*------------------------------------------------------------------------------------
* IMPORTANT! Two Loops below determine the indexing of edges (simplex and hypercube)
*------------------------------------------------------------------------------------
         nEdges=0
         DO jv=1,m_nDim+1
            DO iv=jv+1,m_nDim+1
               nEdges=nEdges+1
               Xdivi(nEdges) = VolPart(jv) / ( VolPart(jv)+VolPart(iv) )
            ENDDO
         ENDDO
         DO j=1,m_kDim
            nEdges=nEdges+1
            Xdivi(nEdges) = Alpha(j)
         ENDDO
*///////////////////////////////////////////////////////////////////////////////////
         Wt =funW(Vrand)*Dx  ! principal weight normalised to integral over the cell
*------------------------------------------------------------------------------------
         m_nCalls = m_nCalls+1
         m_CeSum(iCell,1) = m_CeSum(iCell,1)+ Wt         ! sum of weights
         m_CeSum(iCell,2) = m_CeSum(iCell,2)+ Wt*Wt      ! sum of weights squared
         m_CeSum(iCell,3) = m_CeSum(iCell,3)+ 1d0        ! sum of 1
         m_CeSum(iCell,4) = MIN( m_CeSum(iCell,4),Wt)    ! minium weight
         m_CeSum(iCell,5) = MAX( m_CeSum(iCell,5),Wt)    ! maximu weight
* Search for the best edge candidate for future cell division, prepare MC material
         Ndivi  = (m_nDim+1)*m_nDim/2 +m_kDim
         IF(nEdges.NE.nDivi) GOTO 970
         DO k=1,nDivi
            Xsu1(k)=Xsu1(k) +Xdivi(k)*Wt                 ! averages for all Xdivi
            Xsu2(k)=Xsu2(k) +Xdivi(k)**2*Wt
            iBin = INT(Xdivi(k)*m_nBin)+1d0
            iBin = MIN(MAX(iBin,0),m_nBin)
            Histo(k,iBin) = Histo(k,iBin)+Wt             ! fill histo for each edge
c[[[[[[[[[[[[ debug [[[[[[[[[[[[ debug [[[[[[[[[[[[
c            IF(iCell.EQ.4) THEN
c               CALL GLK_Fil1(1200+k, Xdivi(k),Wt)
c            ENDIF
c]]]]]]]]]]]] debug ]]]]]]]]]]]] debug ]]]]]]]]]]]]
         ENDDO
         IF( m_EvPerBin.GT.0 ) THEN
            NevEff = m_CeSum(iCell,1)**2/m_CeSum(iCell,2)
            IF( NevEff .GE. m_nBin*m_EvPerBin) GOTO 222 !
         ENDIF
      ENDDO
 222  CONTINUE
*///////////////////////////////////////////////////////
*//   End of Special Short MC sampling to probe cell  //
*///////////////////////////////////////////////////////


*//////////////////////////////////////////////////////////////////////////////////////
*//  Determine the best edge candidate for future cell division, 
*//  using MC  material in Histo,Xsu1,Xsu2
*//  kBest,xBest,yBest are the output results
      CALL FoamC_Carver(iCell,Histo,Xsu1,Xsu2,kBest,xBest,yBest)
*//////////////////////////////////////////////////////////////////////////////////////
      NoMC =m_CeSum(iCell,3)
      IF( m_CeSum(iCell,1) .LT.0d0) GOTO 920
      m_CeXave(iCell)  = xBest                      ! best lambda for future division
      m_CeBest(iCell)  = kBest                      ! best edge for future division
      m_CeIntg(iCell)  = m_CeSum(iCell,1)/NoMC      ! estimator of the true integral
*!!!!!!!!!  DrivE is for the Foam build-up (not for MC generation)    !!!!!!!!!!
*!!!!!!!!!  PRIMARY is for MC generation (not for the Foam build-up ) !!!!!!!!!!
      IF(     m_OptDrive.EQ.0 ) THEN
         m_CePrim(iCell) = m_CeIntg(iCell)                   ! True integral, MC generation
         m_CeDriv(iCell) = m_CeIntg(iCell)                   ! True integral, Foam build-up
      ELSEIF( m_OptDrive.EQ.1 ) THEN
         m_CePrim(iCell) = DSQRT(m_CeSum(iCell,2)/NoMC)      ! Sqrt( <w>**2 + sigma**2 )= Sqrt(<w**2>)
         m_CeDriv(iCell) = DSQRT(m_CeSum(iCell,2)/NoMC -m_CeIntg(iCell)**2) ! sigma =Sqrt(<w**2>-<w>**2)
      ELSEIF( m_OptDrive.EQ.2 ) THEN
         m_CePrim(iCell) = m_CeSum(iCell,5)                  ! wtmax    , MC generation
         m_CeDriv(iCell) = m_CeSum(iCell,5) -m_CeIntg(iCell) ! wtmax-<w>, Foam build-up
      ELSE
         WRITE(m_out,*) ' ++++ STOP in FoamC_Explore, wrong m_OptDrive =',m_OptDrive !
         WRITE(    *,*) ' ++++ STOP in FoamC_Explore, wrong m_OptDrive =',m_OptDrive !
         STOP
      ENDIF
* correct volume and Drive in all parent cells to the top of the tree
      parent = m_CePare(iCell)
      DO i = 1,m_nBuf
         IF( parent .EQ. -1 ) GOTO 100 ! Exit if no parent exists
         m_CeIntg(parent)  = m_CeIntg(parent)  +( m_CeIntg(iCell)  -VolOld) !
         m_CeDriv(parent)  = m_CeDriv(parent)  +( m_CeDriv(iCell)  -DriOld) !
         parent=m_CePare(parent)
      ENDDO
 100  CONTINUE
      RETURN
 920  WRITE(*,*) ' ### STOP in FoamC_Explore: something wrong with integrand ' !
      STOP
 950  WRITE(*,*) ' ### STOP in FoamC_Explore: something wrong with volume calculation ' !
      WRITE(*,*) ' Vsum,DxSp= ',Vsum,DxSp
      STOP
 960  WRITE(*,*) ' ### STOP in FoamC_Explore: something wrong with best pair pointer =',kBest !
      STOP
 970  WRITE(*,*) ' ### STOP in FoamC_Explore: something wrong with Ndivi =',nDivi !
      STOP
      END                       ! FoamC_Explore



      SUBROUTINE FoamC_Carver(iCell,Histo,Xsu1,Xsu2,kBest,xBest,yBest)  !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*// Determine the best edge candidate for future cell division, using MC  material   //
*// kBest is the best edge found, xBest and yBest are the best values of lambda      //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            iCell,kBest
      DOUBLE PRECISION   xBest,yBest,zBest
      DOUBLE PRECISION   Histo(m_NdiviMax,m_nBinMax), Xsu1(m_NdiviMax), Xsu2(m_NdiviMax) !
      DOUBLE PRECISION   Bins(m_nBinMax)
      INTEGER            i,j,k, iBin, nDivi
      INTEGER            iUp,iLow, jUp,jLow, kDivi, jv,iv, jDivi
      DOUBLE PRECISION   SumWt,BinMax,This, Carve, yLevel
      DOUBLE PRECISION   CarvOne,CarvTwo,CarvMax
*
      kBest =1
      xBest =0.5d0
      yBest =1d0
      SumWt  = m_CeSum(iCell,1)
      IF( SumWt .NE. 0d0) THEN
         CarvMax = -1d150
         nDivi  = (m_nDim+1)*m_nDim/2 +m_kDim
         DO kDivi=1,nDivi
            BinMax  = -1d150
            DO iBin=1,m_nBin    ! Unload histo and Maximum bin
               Bins(iBin) = Histo(kDivi,iBin)
               BinMax = MAX( BinMax, Bins(iBin)) ! Maximum content/bin
            ENDDO
            CarvTwo = 0d0
            DO iBin=1,m_nBin
               Bins(iBin) = Bins(iBin)/BinMax ! Normalize to the highest bin
               CarvTwo = CarvTwo + (1d0-Bins(iBin))/m_nBin ! Total carve
            ENDDO
* Find maximum 'rectangular carve' in betwen the two bins (jLow,...,jUp)
            jLow =1
            jUp  =m_nBin
            CarvOne = -1d150
            yLevel  = -1d150
            DO iBin=1,m_nBin
               This = Bins(iBin)
               iLow = iBin
               DO j=iBin,1,-1   ! walk to the left and find first bin > current
                  IF(This .LT. Bins(j) ) GOTO 100
                  iLow = j
               ENDDO
 100           CONTINUE
               iUp  = iBin
               DO j=iBin,m_nBin ! walk to the right and find first bin > current
                  IF(This .LT. Bins(j) ) GOTO 200
                  iUp  = j
               ENDDO
 200           CONTINUE
               Carve = (iUp-iLow+1)*(1d0-This)/m_nBin
               IF( Carve .GT. CarvOne) THEN
                  CarvOne = Carve
                  jLow = iLow
                  jUp  = iUp
                  yLevel = This
               ENDIF
            ENDDO               ! end-loop over histogram bins 
*************************************************************
**               IF( CarvOne .GT. CarvMax) THEN
**                  CarvMax   = CarvOne
*************************************************************
            IF( CarvTwo .GT. CarvMax) THEN
               CarvMax   = CarvTwo
               kBest = kDivi    ! Best edge
               xBest = (jLow-1d0)/m_nBin
               yBest = (jUp*1d0)/m_nBin
               IF(jLow .EQ. 1 )     xBest = yBest
               IF(jUp  .EQ. m_nBin) yBest = xBest
* division ratio in units of 1/m_nBin, testing
               jDivi = jLow-1
               IF(jLow .EQ. 1 )     jDivi=jUp
**************************************************************
* The improvement below does not seem to matter at all
*                  IF( 0.5d0*(xBest+yBest) .LT. 0.5d0) THEN
*                     zBest =xBest
*                     xBest =yBest
*                     yBest =zBest
*                  ENDIF
**************************************************************
            ENDIF
*[[[[[[[[[[[[ debug [[[[[[[[[[[[ debug [[[[[[[[[[[[[[[[[[
*               WRITE(*,'(a,4i5,f10.5)') '==iCell,kDivi,jLow,jUp,CarvMax= ',iCell,kDivi,jLow,jUp,CarvOne !!
*               IF(iCell.EQ.2 ) THEN
*                  CALL GLK_Pak(  1200+kDivi,Bins)
*                  DO iBin = 1,m_nBin
*                     Bins(iBin) = 1d0
*                  ENDDO
*                  DO iBin=jLow,jUp
*                     Bins(iBin) = yLevel
*                  ENDDO
*                  CALL GLK_Pak(  1400+kDivi,Bins)
*               ENDIF
c]]]]]]]]]]]]debug ]]]]]]]]]]]] debug ]]]]]]]]]]]]]]]]]]]
         ENDDO                  ! end-loop over nDivi
      ENDIF
*[[[[[[[[[[[[ debug [[[[[[[[[[[[ debug [[[[[[[[[[[[
*      WRITE(*,'(a,i5)')        ' FoamC_Carver , iCell = ', iCell
*      WRITE(*,'(a,i5,3f10.5)') ' kBest,xBest,yBest= ', kBest, xBest,yBest!!
*]]]]]]]]]]]] debug ]]]]]]]]]]]] debug ]]]]]]]]]]]]
      END                       ! FoamC_Carver


      SUBROUTINE FoamC_RanDiscr(Driv,nTot,Power,iRand)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   PRESENTLY UNUSED !!!                                                           //
*//   Generates iRand in (1,nTot) acconding to discrete un-normalized probab. Driv   //
*//   Power is normaly =1, can be useful for special purposes                        //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      DOUBLE PRECISION   Driv(*),Power
      INTEGER            nTot,iRand
      INTEGER            i
      DOUBLE PRECISION   random,sum,Total
      REAL               Qrand(10)        ! from PseuMar
*
      Total   = 0d0
      DO i= 1,nTot
         Total = Total +Driv( i)**Power
      ENDDO
      IF(Total .EQ. 0d0) GOTO 990
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,1)
      ELSE
         CALL RanLux(Qrand,1)
      ENDIF
      random = Qrand(1)
      iRand  = -1
      sum   = 0d0
      DO i= 1,nTot
         iRand  = i
         sum = sum +Driv( i)**Power
         IF( random .LT. sum/Total ) GOTO 100
      ENDDO
      IF(iRand .EQ. -1) GOTO 990
 100  CONTINUE
      RETURN
 990  WRITE(*,*) ' ### STOP in FoamC_RanDiscr, something went wrong !!!!'
      STOP
      END


      SUBROUTINE FoamC_MakeAlpha(Alpha)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                      //
*//   Provides random vector Alpha, each component in (0,1) range                        //
*//                                                                                      //
*//////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   Alpha(m_KdiMax), y
      REAL               Qrand( m_KdiMax)        ! from PseuMar
      INTEGER            i,k
*     --------------------------------------------------------
      IF( m_kDim.LT.0 .OR. m_kDim.GT.m_KdiMax) THEN
         WRITE(*,*) 'STOP in FoamC_MakeAlpha: m_kDim=',m_kDim
         STOP
      ENDIF
      IF(m_kDim.LE.0) RETURN
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,m_kDim)
      ELSE
         CALL RanLux(Qrand,m_kDim)
      ENDIF
      DO k =1,m_kDim
         Alpha(k)=Qrand(k)
      ENDDO
      END

      SUBROUTINE FoamC_MakeLambda(Lambda)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                      //
*//   Provides random vector Lambda such that Sum Lamba(i) = 1, with uniform probab.     //
*//   This  vector is used to populate uniformly the interior of a simplex.              //
*//   The method is: generate point inside cube, order components (maping into simplex)  //
*//   and take differences of Lambda(i+1) - Lambda(i)                                    //
*//                                                                                      //
*//////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            i,k
      DOUBLE PRECISION   Lambda(m_NdiMax), y
      REAL               Qrand( m_NdiMax)        ! from PseuMar
      REAL               x
*     --------------------------------------------------------
      IF(m_nDim.LE.0) RETURN
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,m_nDim)
      ELSE
         CALL RanLux(Qrand,m_nDim)
      ENDIF
* ordering vector components (maping into simplex)
      DO i =m_nDim,1,-1
         DO k =2,i
            IF( Qrand(k).LT.Qrand(k-1)) THEN
               x            = Qrand(k)
               Qrand(k)    = Qrand(k-1)
               Qrand(k-1)  = x
            ENDIF
         ENDDO
      ENDDO
* Sum of lambdas should equal one
      Lambda(1)=Qrand(1)
      DO k =2,m_nDim
         Lambda(k)=Qrand(k)-Qrand(k-1)
      ENDDO
      END                       ! MakeLambda


      SUBROUTINE FoamC_Determinant(R,Det)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Calculates determinant of matrix R                                             //
*//   Use of Laplace formula should be perhaps replaced with something faster        //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det
      DOUBLE PRECISION   FoamC_Det2Lapl, FoamC_Det3Lapl,FoamC_Det4Lapl,FoamC_Det5Lapl !
*     -------------------------------------------------
      IF(        m_nDim .EQ. 1) THEN
         Det= R(1,1)
      ELSEIF(    m_nDim .EQ. 2) THEN
         Det= FoamC_Det2Lapl(R, 1,2)
      ELSEIF(    m_nDim .EQ. 3) THEN
         Det= FoamC_Det3Lapl(R, 1,2,3)
      ELSEIF(    m_nDim .EQ. 4) THEN
         Det= FoamC_Det4Lapl(R, 1,2,3,4)
      ELSEIF(    m_nDim .EQ. 5) THEN
         Det= FoamC_Det5Lapl(R, 1,2,3,4,5)
      ELSE
         WRITE(*,*) '####FoamC_Determinant: STOP, m_nDim =',m_nDim
         STOP
      ENDIF
      END                       ! FoamC_Determinant

      DOUBLE PRECISION FUNCTION FoamC_Det2Lapl(R,i1,i2)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det
      INTEGER  i1,i2
*     ------------------------------------------------------------
      FoamC_Det2Lapl= R(1,i1)*R(2,i2) - R(1,i2)*R(2,i1)
      END


      DOUBLE PRECISION FUNCTION FoamC_Det3Lapl(R,i1,i2,i3)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   Determinat Laplace method                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det,FoamC_Det2Lapl
      INTEGER  i1,i2,i3
*     ------------------------------------------------------------
      FoamC_Det3Lapl=+R(3,i1) *FoamC_Det2Lapl(R,i2,i3)
     $               -R(3,i2) *FoamC_Det2Lapl(R,i1,i3)
     $               +R(3,i3) *FoamC_Det2Lapl(R,i1,i2)
      END

      DOUBLE PRECISION FUNCTION FoamC_Det4Lapl(R,i1,i2,i3,i4)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   Determinat Laplace method                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax),Det,FoamC_Det3Lapl
      INTEGER  i1,i2,i3,i4
*     ------------------------------------------------------------
      FoamC_Det4Lapl=-R(4,i1) *FoamC_Det3Lapl(R,i2,i3,i4)
     $               +R(4,i2) *FoamC_Det3Lapl(R,i1,i3,i4)
     $               -R(4,i3) *FoamC_Det3Lapl(R,i1,i2,i4)
     $               +R(4,i4) *FoamC_Det3Lapl(R,i1,i2,i3)
      END

      DOUBLE PRECISION FUNCTION FoamC_Det5Lapl(R,i1,i2,i3,i4,i5)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//   Determinat Laplace method                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   R(m_NdiMax,m_NdiMax), Det, FoamC_Det4Lapl
      INTEGER  i1,i2,i3,i4,i5
*     ------------------------------------------------------------
      FoamC_Det5Lapl=+R(5,i1) *FoamC_Det4Lapl(R,i2,i3,i4,i5)
     $               -R(5,i2) *FoamC_Det4Lapl(R,i1,i3,i4,i5)
     $               +R(5,i3) *FoamC_Det4Lapl(R,i1,i2,i4,i5)
     $               -R(5,i4) *FoamC_Det4Lapl(R,i1,i2,i3,i5)
     $               +R(5,i5) *FoamC_Det4Lapl(R,i1,i2,i3,i4)
      END


      SUBROUTINE FoamC_Grow(funW)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Grow new cells by division                                                     //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            i,RC,iCell
*---------------------------------------------
* Final division
      DO i=1,100000
         IF(  m_OptPeek .EQ. 0 ) THEN
            CALL FoamC_PeekMax(  iCell)       ! choose cell with maximum Drive
         ELSE
            CALL FoamC_PeekRand1(iCell)       ! randomly choose one cell
         ENDIF
         CALL FoamC_Divide( iCell,funW,RC)    ! and divide it into two
c[[[[
c         CALL FoamC_BufPrint(    m_Out)
c         CALL FoamC_VertPrint(  6)
c]]]]
         IF(RC.EQ.-1) GOTO 300
      ENDDO
 300  CONTINUE
      WRITE(16,*) '######################### FoamC_Grow #####################'
*****[[[[[[ debug
***** CALL FoamC_BufPrint(    6)
***** CALL FoamC_BufActPrint( 6)
***** CALL FoamC_VertPrint(   6)
*****]]]]]]
      CALL FoamC_Check(6,0)
      END                       ! FoamC_Grow

      SUBROUTINE FoamC_PeekMax(iCell)    !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Create list of active cells (pointers) using DrivE                             //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER   iCell
      INTEGER   i
      DOUBLE PRECISION  DrivMax, Driv
*     ---------------------------------------------------
      iCell = 0
      DrivMax = -1d150
      DO i = 1,m_LastCe
         IF( m_CeStat(i).EQ.1 ) THEN
            Driv=  ABS(m_CeDriv(i))
            IF(Driv .GT. DrivMax) THEN
               DrivMax = Driv
               iCell = i
            ENDIF
         ENDIF
      ENDDO
****  WRITE(*,*) '###>>> FoamC_PeekMax: iCell=',iCell
      IF(iCell.EQ.0) THEN
         WRITE(*,*) '### STOP in FoamC_PeekMax: not found iCell=', iCell
         STOP
      ENDIF
      END                       ! FoamC_PeekMax


      SUBROUTINE FoamC_PeekRand1(iCell)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//       Peek up randomly TREE-WISE the pointer iCell of an active cell             //
*//                        Using DrivE                                               //
*// We walk randomly from top of tree downwards until we find active cell m_CeStat=1 //
*// At each step one of daugters cells is choosen randomly according                 //
*// to their volume estimates.                                                       //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            iCell
      INTEGER            kCell,i,Dau1,Dau2,iDau
      DOUBLE PRECISION   random,p1,volu1,volu2,volu,TotDri,sum
      REAL               Qrand(10)        ! from PseuMar
*     ----------------------------------------------------------------
* first cell is special because it has nDim-factorial daughters, istead of just 2
      iCell=1
      IF(m_CeStat(iCell).EQ.1) RETURN ! In case of cubics firs cell may be active
      kCell = 1
      Dau1  = m_CeDau1(kCell)
      Dau2  = m_CeDau2(kCell)
      TotDri   = 0d0
      DO iCell= Dau1,Dau2
         TotDri = TotDri+m_CeDriv( iCell )
      ENDDO
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,1)
      ELSE
         CALL RanLux(Qrand,1)
      ENDIF
      random = Qrand(1)
      iDau  = -1
      sum   = 0d0
      DO iCell= Dau1,Dau2
         iDau  = iCell
         sum = sum+m_CeDriv( iCell )
         IF( random .LT. sum/TotDri ) GOTO 100
      ENDDO
      IF(iDau.EQ.-1) GOTO 990
 100  kCell=iDau
****[[[[[[[[[[[[[[[[
****  WRITE(*,*) '%%%%%%%%%%%%%%%%%%%%%% FoamC_PeekRand1: top daughter =',kCell
****]]]]]]]]]]]]]]]]
      IF( m_CeStat( kCell ) .EQ. 1 ) GOTO 300
* now the other standard cells with 2 daughters
      DO i=1,10000000
         IF( m_CeStat( kCell ) .EQ. 1 ) GOTO 300
         volu1= m_CeDriv( m_CeDau1(kCell) )
         volu2= m_CeDriv( m_CeDau2(kCell) )
         p1 = volu1/(volu1+volu2)
         IF(m_OptRanLux.EQ.-1) THEN
            CALL PseuMar_MakeVec(Qrand,1)
         ELSE
            CALL RanLux(Qrand,1)
         ENDIF
         random = Qrand(1)
         IF( random .LT. p1 ) THEN
            kCell = m_CeDau1(kCell)
         ELSE
            kCell = m_CeDau2(kCell)
         ENDIF
****[[[[[[[[[[[[[[[[[[[[
****  WRITE(*,*) '%%%%%%%%%%%%%%%%%%%%%% FoamC_PeekRand1: normal daughter =',kCell
****]]]]]]]]]]]]]]]]]]]]]
      ENDDO
      GOTO 990
 300  CONTINUE
      iCell=kCell
****[[[[[[[[[[[[[[[[[[[[
****  WRITE(*,*) '%%%%%%%%%%%%%%%%%%%%%% FoamC_PeekRand1: choosen cell =',kCell
****]]]]]]]]]]]]]]]]]]]]]
      RETURN
 990  WRITE(*,*) ' ### STOP in FoamC_PeekRand1, something went wrong !!!!'
      STOP
      END                       !!! FoamC_PeekRand1


      SUBROUTINE FoamC_Divide(iCell,funW,RC)     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Divide cell iCell into two daughter cells                                      //
*//   The iCell is retained and taged as inactive, daughter cells are appended       //
*//   at the end of the buffer.                                                      //
*//   New vertex is added to list of vertice.                                        //
*//   List of active cells is updated, iCell remooved, two daughters added           //
*//   and their properties set with help of MC sampling (FoamC_Explore)              //
*//   Return Code RC=-1 of buffer limit is reached,  m_LastCe=m_nBuf                 //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            iCell,RC
      INTEGER            Dau1, Dau2, kVer1(m_NdiMax+1), kVer2(m_NdiMax+1),p1,p2 !
      DOUBLE PRECISION   HcPosi1(m_KdiMax),   HcSize1(m_KdiMax)
      DOUBLE PRECISION   HcPosi2(m_KdiMax),   HcSize2(m_KdiMax)
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            Old1,Old2,j,k,jv,iv,nEdges,kBest,kDivi,NoMC
      DOUBLE PRECISION   Xave
      INTEGER            nDivi,nDivi0
*--------------------------------------------------------------------------------------
      RC = 0
      IF( m_LastCe+2 .GT. m_nBuf) GOTO 990 !! abort if no space in buffer
* reset cell as inactive
      m_CeStat(iCell) = 0
*------------------------------------------------------------------------------------
* Define lists of vertices for daughters, initially inherited...
      IF(m_nDim .GT. 0) THEN
         DO jv=1,m_nDim+1
            kVer1(jv) = m_CeVert(iCell,jv)
            kVer2(jv) = m_CeVert(iCell,jv)
         ENDDO
      ENDIF
* Position and size initially also inherited
      DO j=1,m_kDim
         HcPosi1(j)=m_CeVer1(iCell,j)
         HcSize1(j)=m_CeVer2(iCell,j)
         HcPosi2(j)=m_CeVer1(iCell,j)
         HcSize2(j)=m_CeVer2(iCell,j)
      ENDDO
*--------------------------------------------------------------------------------------
      nDivi  = (m_nDim+1)*m_nDim/2 +m_kDim
      nDivi0 = (m_nDim+1)*m_nDim/2
      kBest  = m_CeBest(iCell)
      IF( kBest.LE.nDivi0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//   The best division in simplical subspace
*//////////////////////////////////////////////////////////////////////////////////////
         m_LastVe=m_LastVe+1    ! add new vertex to the list
         IF(m_LastVe.GT.m_vMax) GOTO 980
         Xave  = m_CeXave(iCell)
         nEdges=0
         DO jv=1,m_nDim+1
            DO iv=jv+1,m_nDim+1
               nEdges=nEdges+1
               IF( nEdges.EQ.kBest) THEN
                  p1 =  m_CeVert(iCell,jv)
                  p2 =  m_CeVert(iCell,iv)
                  DO j=1,m_nDim
                     m_VerX(m_LastVe,j) = Xave*m_VerX(p1,j) + (1d0-Xave)*m_VerX(p2,j) ! New Vertex
                  ENDDO
                  Old1=jv
                  Old2=iv
                  GOTO 100
               ENDIF
            ENDDO
         ENDDO
 100     CONTINUE
         kVer1(Old1)=m_LastVe   ! one old vertex replaced by new one
         kVer2(Old2)=m_LastVe   ! one old vertex replaced by new one
      ELSE
*//////////////////////////////////////////////////////////////////////////////////////
*//   The best division in hypercubical subspace
*//   Position and size inherited except one direction k=kDivi
*//////////////////////////////////////////////////////////////////////////////////////
         Xave  = m_CeXave(iCell)
         kDivi = kBest-nDivi0
         HcPosi1(kDivi)=      m_CeVer1(iCell,kDivi)        ! (1) Position unchanged
         HcSize1(kDivi)= Xave*m_CeVer2(iCell,kDivi)        ! (1) Size reduced by Xave
         HcPosi2(kDivi)= HcPosi1(kDivi) +HcSize1(kDivi)    ! (2) Position shifted
         HcSize2(kDivi)= (1d0-Xave)*m_CeVer2(iCell,kDivi)  ! (2) Size reduced by (1-Xave)
      ENDIF
* define two daughter cells (active)
      NoMC= m_nSampl
* ------------------------ Stat, Pare, Dau1,Dau2, MCsampl, VertList, Position,Size,  iCeNew
      CALL FoamC_DefCell(  1, iCell,  -1,  -1,    NoMC,   kVer1,  HcPosi1, HcSize1, Dau1) !
      CALL FoamC_DefCell(  1, iCell,  -1,  -1,    NoMC,   kVer2,  HcPosi2, HcSize2, Dau2) !
      m_CeDau1(iCell) = Dau1
      m_CeDau2(iCell) = Dau2
      CALL FoamC_Explore(Dau1,funW)
      CALL FoamC_Explore(Dau2,funW)
* Update list of active cells, only for internal tests
      CALL FoamC_ActUpda
      RETURN
 990  RC=-1                     !!buffer limit is reached,  m_LastCe=m_nBuf
      RETURN
 980  WRITE(*,*) ' ### STOP in FoamC_Divide: too short list of vertices '
      STOP
      END                       ! FoamC_Divide



      SUBROUTINE FoamC_ActUpda     !# Initialization
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Create list of active cells (pointers)                                         //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER   iCell,Dau1,Dau2
      INTEGER   i
*     ---------------------------------------------------
      m_LastAc=0
      DO iCell = 1,m_LastCe
         IF( m_CeStat(iCell).EQ.1 ) THEN
            m_LastAc=m_LastAc+1
            IF(m_LastAc .EQ. m_cMax) GOTO 950
            m_ActC(m_LastAc) = iCell
         ENDIF
      ENDDO
      RETURN
 900  WRITE(*,*) '### STOP in FoamC_ActUpda: not found iCell=', iCell
      STOP
 950  WRITE(*,*) '### STOP in FoamC_ActUpda: list of active cells too short'
      STOP
      END                       ! FoamC_ActUpda


      SUBROUTINE FoamC_CellGener(iCell)     !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//       Peek up randomly pointer iCell of an active cell according to PRIMARY      //
*//       Straightforward way, using list of active pointes made by ActUpda          //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            iCell
      INTEGER            kCell,i,Dau1,Dau2,iDau
      DOUBLE PRECISION   random,x1,TotPrim
      INTEGER            klower,kuper,krange,kurrent,DipSwitch
      REAL               Qrand(10)        ! from PseuMar
      INTEGER   iCont
      DATA      iCont/0/
      iCont = iCont+1
*     ----------------------------------------------------------------
      IF(m_OptRanLux.EQ.-1) THEN
         CALL PseuMar_MakeVec(Qrand,1)
      ELSE
         CALL RanLux(Qrand,1)
      ENDIF
      random = Qrand(1)
*     ---------------------------------------------------
      TotPrim = m_CePrCu(m_LastAc)
      x1 = TotPrim *random
      DipSwitch=0               ! =0 is faater
      IF( DipSwitch.EQ.1) THEN
*-------- primitive method --------
         DO kCell = 1,m_LastAc
            iCell = m_ActC(kCell)
            IF( m_CePrCu(kCell) .GE. x1 ) GOTO 800
         ENDDO
         WRITE(*,*) '### STOP1 in FoamC_CellGener: something wrong' !
         STOP
 800     CONTINUE
      ELSEIF( DipSwitch.EQ.0) THEN
*-------- a bit more sophisticated/faster method ------
         klower   = 0
         kuper    = m_LastAc
         DO i=1,m_LastAc
            krange   = (kuper-klower+1)/2
            kurrent  = klower +krange
            IF( x1 .LE. m_CePrCu(kurrent) ) THEN
               kuper = kurrent
            ELSE
               klower = kurrent
            ENDIF
            IF(kuper-klower.LE.1) GOTO 850
         ENDDO
         WRITE(*,*) ' STOP in FoamC_CellGener'
         STOP
 850     CONTINUE
         iCell = m_ActC(kuper)
         kCell = kuper
      ELSE
         iCell = m_ActC(1)      ! nonsense for tests
      ENDIF
***** IF(iCont.LE.10) WRITE(*,*) 'LastAc,kCell,iCell = ',m_LastAc,kCell,iCell !! debug
      END                       ! FoamC_CellGener


      SUBROUTINE FoamC_MakeEvent(funW)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Generates point/vector Xrand with the weight MCwt                              //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            iCell,i,j,iv
      DOUBLE PRECISION   Wt,x1,x2,Dx,MCwt
      DOUBLE PRECISION   Alpha( m_KdiMax) ! Hyper-Cubic subspace
      DOUBLE PRECISION   Lambda(m_NdiMax) ! Simplical subspace
*     -----------------------------------------------------------------
*     choose randomly one cell
      CALL FoamC_CellGener( iCell)
*     generate randomly/uniformly vector Vrand inside this simplex
      CALL FoamC_MakeAlpha( Alpha)
      CALL FoamC_MakeLambda(Lambda)
      DO j=1,m_nDim
         m_MCvector(j) = m_VerX( m_CeVert(iCell,m_nDim+1) ,j)
         DO iv=1,m_nDim
            m_MCvector(j) = m_MCvector(j) 
     $           +Lambda(iv)*( m_VerX( m_CeVert(iCell,iv) ,j) -m_VerX( m_CeVert(iCell,m_nDim+1) ,j) ) !
         ENDDO
      ENDDO
      DO j=1,m_kDim
         m_MCvector(m_nDim+j) = m_CeVer1(iCell,j) +Alpha(j)*m_CeVer2(iCell,j) ! hypcubic subspace
      ENDDO
      Dx = m_CeVolu(iCell)      ! Cartesian volume of the Cell
*[[[[[[[[[[[[
***** CALL FoamC_TestNewConcept(iCell)
*]]]]]]]]]]]]
* weight average normalised to PRIMARY integral over the cell
      MCwt =funW(m_MCvector)*Dx/m_CePrim(iCell) ! PRIMARY controls normalization
      m_nCalls =  m_nCalls+1
      m_MCwt   =  MCwt
* accumulation of statistics for the main MC weight
      m_SumWt  =  m_SumWt  +MCwt         ! sum of Wt
      m_SumWt2 =  m_SumWt2 +MCWt*MCwt    ! sum of Wt**2
      m_NevGen =  m_NevGen +1d0          ! sum of 1d0
      m_WtMax  =  MAX(m_WtMax,MCwt)      ! maximum wt
      m_WtMin  =  MIN(m_WtMin,MCwt)      ! minimum wt
* update also weight sums in the cell,
* note weights here are normalized absolutely, eg. to the value of the integral
      Wt = MCwt*m_CePrim(iCell)
      m_CeSum(iCell,1) = m_CeSum(iCell,1)+ Wt      ! sum of weights
      m_CeSum(iCell,2) = m_CeSum(iCell,2)+ Wt*Wt   ! sum of weights squared
      m_CeSum(iCell,3) = m_CeSum(iCell,3)+ 1d0     ! sum of 1
      m_CeSum(iCell,4) = MIN( m_CeSum(iCell,4),Wt) ! minium weight
      m_CeSum(iCell,5) = MAX( m_CeSum(iCell,5),Wt) ! maximu weight
      END                       ! FoamC_MakeEvent

      SUBROUTINE FoamC_GetMCvector(MCvector)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION  MCvector(*)
      INTEGER           k
*-----------------------
      DO k=1,m_Dimen
         MCvector(k)    = m_MCvector(k)
      ENDDO
      END

      SUBROUTINE FoamC_GetMCwt(MCwt)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION  MCwt
*-----------------------
      MCwt    = m_MCwt
      END

      SUBROUTINE FoamC_MCgenerate(funW,MCvector,MCwt)    !# Generation
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Obsolete                                                                       //
*//   Generates point/vector MCvector with the weight MCwt                           //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION   MCvector(*),MCwt
      DOUBLE PRECISION   funW
      EXTERNAL           funW
      INTEGER            j
*     ---------------------------------------------------------------
      CALL FoamC_MakeEvent(funW)
      MCwt = m_MCwt
      DO j=1,m_Dimen
         MCvector(j) =m_MCvector(j)
      ENDDO
      END                       !!FoamC_MCgenerate


      SUBROUTINE FoamC_Finalize(MCresult,MCerror)    !# Finalization
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   After MC run is completed it calculates integral and its error         //
*//   Also prints some information/statistics on the MC run                  //
*//   Remember that PRIMARY controls normalization of the integration        //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
*
      DOUBLE PRECISION     MCresult,MCerror,MCerelat
      DOUBLE PRECISION     Vtot,Verr,VerRela,TotPrim
      INTEGER              kCell,iCell
*-----------------------------------------------------------------------------
      MCresult =0d0
      MCerelat =1d0
      TotPrim = 0d0
      DO kCell = 1,m_LastAc
         iCell  = m_ActC(kCell)
         TotPrim = TotPrim +m_CePrim( iCell )
      ENDDO
      IF(m_NevGen .GT. 0) THEN
         MCresult = TotPrim *m_SumWt/m_NevGen
         IF(m_SumWt.NE.0d0)  MCerelat  = m_SumWt2/m_SumWt**2 -1d0/m_NevGen !
         IF(MCerelat.GT.0d0) MCerelat =  SQRT( ABS(MCerelat) ) !
         IF(MCerelat.LT.0d0) MCerelat = -SQRT( ABS(MCerelat) ) !
      ENDIF
      MCerror = MCresult*MCerelat
* some test printouts
      WRITE(m_Out,'(3a)') '||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||' !
      WRITE(m_Out,'(3a)') '||||||||||||||||||||||||||||||', ' FoamC_Finalize ', !
     $                    '||||||||||||||||||||||||||||||'
      WRITE(m_Out,'(a,2g18.9,f11.7)')               'MCresult, MCerror, Errela= ',MCresult,MCerror,MCerelat !
      WRITE(m_Out,'(a,2f11.5)')                     'Minimum maximum weight   = ',m_WtMin,m_WtMax !
      IF(m_NevGen .GT. 0) WRITE(m_Out,'(a,2f11.5)') 'Average wt SumWt/NevGen  = ',m_SumWt/m_NevGen !
      WRITE(m_Out,'(a,i15)')                        'Total of function calls  = ',m_nCalls !
      WRITE(m_Out,'(a,2i15)')                       'Number of cells&vertices = ',m_LastCe,m_LastVe !
      WRITE(m_Out,'(a,f12.1,2g18.9)')               'NevGen,SumWt,m_SumWt2    = ',m_NevGen,m_SumWt,m_SumWt2 !
      END       ! FoamC_Finalize



      SUBROUTINE FoamC_GetTotPrim(TotPrim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//   Total integral from cell statistics, including initialization + MC generation  //
*//   It can be invoked just after initialization or after MC generation             //
*//   Note that this estimate is distorted slightly if vertices are included in      //
*//   the exploration of the cells.                                                  //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      INCLUDE 'FoamC.h'
      DOUBLE PRECISION  TotPrim,Sum
      INTEGER   kCell,  iCell      
      Sum = 0d0
      DO kCell = 1,m_LastAc
         iCell  = m_ActC(kCell)
         Sum = Sum +m_CePrim( iCell )
      ENDDO
      TotPrim=Sum
      END


      SUBROUTINE FoamC_SetnDim(nDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      nDim
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_nDim = nDim
      m_Dimen=m_nDim+m_kDim
      END                       !!! FoamC_SetnDim

      SUBROUTINE FoamC_SetkDim(kDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      kDim
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_kDim = kDim
      m_Dimen=m_nDim+m_kDim
      END                       !!! FoamC_SetnDim

      SUBROUTINE FoamC_GetnDim(nDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      nDim
*     -------------------------------------------------
      nDim = m_nDim
      END                       !!! FoamC_SetnDim

      SUBROUTINE FoamC_GetkDim(kDim)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      kDim
*     -------------------------------------------------
      kDim = m_kDim
      END                       !!! FoamC_SetkDim

      SUBROUTINE FoamC_GetnCalls(nCalls)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      nCalls
*     -------------------------------------------------
      nCalls = m_nCalls
      END                       !!! FoamC_SetnCalls

      SUBROUTINE FoamC_SetnBuf(nBuf)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      nBuf
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_nBuf = nBuf
      END                       !!! FoamC_SetnBuf

      SUBROUTINE FoamC_SetnBin(nBin)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      nBin
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_nBin = nBin
      END                       !!! FoamC_SetnBin

      SUBROUTINE FoamC_SetOut(Out)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      Out
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_Out = Out
      END                       !!! FoamC_SetOut

      SUBROUTINE FoamC_SetChat(Chat)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      Chat
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_Chat = Chat
      END                       !!! FoamC_SetChat

      SUBROUTINE FoamC_SetnSampl(nSampl)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      nSampl
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_nSampl = nSampl
      END                       !!! FoamC_SetnSampl

      SUBROUTINE FoamC_SetOptDrive(OptDrive)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      OptDrive
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_OptDrive = OptDrive
      END                       !!! FoamC_SetOptDrive

      SUBROUTINE FoamC_SetEvPerBin(EvPerBin)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      EvPerBin
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_EvPerBin = EvPerBin
      END                       !!! FoamC_SetEvPerBin

      SUBROUTINE FoamC_SetOptPeek(OptPeek)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      OptPeek
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_OptPeek = OptPeek
      END                       !!! FoamC_SetOptPeek

      SUBROUTINE FoamC_SetOptEdge(OptEdge)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      OptEdge
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_OptEdge = OptEdge
      END                       !!! FoamC_SetOptEdge

      SUBROUTINE FoamC_SetOptOrd(OptOrd)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      OptOrd
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_OptOrd = OptOrd
      END                       !!! FoamC_SetOptOrd


      SUBROUTINE FoamC_SetOptRanIni(OptRanIni)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      OptRanIni
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_OptRanIni = OptRanIni
      END                       !!! FoamC_SetOptRanIni

      SUBROUTINE FoamC_SetOptRanLux(OptRanLux)     !# Getters and Setters
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER      OptRanLux
*     -------------------------------------------------
      CALL FoamC_PreInitialize
      m_OptRanLux = OptRanLux
      END                       !!! FoamC_SetOptRanLux

      SUBROUTINE FoamC_Check(mout,level)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//  Checks all pointers, It is is a usefull autodiagnostic.                         //
*//                                                                                  //
*//  level=0, no printout, failures causes STOP                                      //
*//  level=1, printout, failures lead to WARNINGS only                               //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER       mout,level
      INTEGER       nFailures, iCell, Dau1,Dau2, Pare, NoRefs(m_vMax), iVe,n !
      INTEGER       NoEmpty, iError
*     ---------------------------------------------------------
      nFailures=0
      iError   =0
      IF(level.EQ.1) WRITE(mout,*)
     $'//////////////////////////////////////// FoamC_Checks /////////////////////////////////////////////' !
      DO iCell = 1,m_LastCe
         Dau1 = m_CeDau1(iCell)
         Dau2 = m_CeDau2(iCell)
         Pare = m_CePare(iCell)
* checking on parents
         IF(iCell.GT.1) THEN
            IF(Pare.GT.m_LastCe) THEN
               iError   =1
               nFailures = nFailures+1
               IF(level.EQ.1) WRITE(mout,*) '##### iCell= ',iCell,' Parent out of range = ',Pare !
            ENDIF
         ENDIF
         IF(iCell.GT.1) THEN
            IF(  (Pare.NE.1) .AND. (m_CeDau1(Pare).NE.iCell) .AND. (m_CeDau2(Pare).NE.iCell)  ) THEN !
               iError   =2
               nFailures = nFailures+1
               IF(level.EQ.1) 
     $              WRITE(mout,*) '##### iCell= ',iCell,' Parent not pointing to this daughter Pare= ',Pare !
            ENDIF
         ENDIF
* checking on daughters
         IF( Dau1 .GT. m_LastCe ) THEN
            iError   =3
            nFailures = nFailures+1
            IF(level.EQ.1) WRITE(mout,*) '##### iCell= ',iCell,' First  Daugter out of range Dau1= ',Dau1 !
         ENDIF
         IF( Dau2 .GT.m_LastCe ) THEN
            iError   =4
            nFailures = nFailures+1
            IF(level.EQ.1) WRITE(mout,*) '##### iCell= ',iCell,' Second Daugter out of range Dau2= ',Dau2 !
         ENDIF
         IF( Dau1.GE.1 .AND. Dau1.LE. m_LastCe) THEN
            IF( m_CePare(Dau1).NE.iCell ) THEN
               iError   =5
               nFailures = nFailures+1
               IF(level.EQ.1) 
     $              WRITE(mout,*) '##### iCell= ',iCell,' First  Daugter not pointing to parent Dau1= ',Dau1 !
            ENDIF
         ENDIF
         IF( Dau2.GE.1 .AND. Dau2.LE. m_LastCe) THEN
            IF( m_CePare(Dau2).NE.iCell ) THEN
               iError   =6
               nFailures = nFailures+1
               IF(level.EQ.1) 
     $              WRITE(mout,*) '##### iCell= ',iCell,' Second Daugter not pointing to parent Dau2= ',Dau2 !
            ENDIF
         ENDIF 
      ENDDO
* check on vertices
      DO iVe = 1, m_LastVe
         NoRefs(iVe)=0
      ENDDO
      DO iVe = 1, m_LastVe
         DO iCell = 1, m_LastCe
            DO n=1,m_nDim+1
               IF( iVe .EQ. m_CeVert(iCell,n) ) NoRefs(iVe) =1
            ENDDO
         ENDDO
      ENDDO
      DO iVe = 1, m_LastVe
         IF(NoRefs(iVe).EQ.0 .AND.  level.EQ.1) WRITE(mout,*) '***** Vertex no. ',iVe, '  NOT referenced!' !
      ENDDO
* Check for empty cells
      NoEmpty = 0d0
      DO iCell = 1,m_LastCe
         IF( m_CeStat(iCell).EQ.1 ) THEN
            IF( m_CeDriv(iCell) .EQ. 0d0) NoEmpty = NoEmpty +1
         ENDIF
      ENDDO
      IF( NoEmpty.GT.0) THEN
         WRITE(mout,*) '++++++++++ FoamC_Check: !!! WARNING!!!! Empty Cells found NoEmpty= ',NoEmpty !
         WRITE(   *,*) '++++++++++ FoamC_Check: !!! WARNING!!!! Empty Cells found NoEmpty= ',NoEmpty !
      ENDIF
* summary
      IF(level.EQ.1) WRITE(mout,*) '++++++++++ FoamC_Check has found total ', nFailures, ' failures ' !
      IF(level.EQ.1) WRITE(mout,*)
     $'///////////////////////////////////////////////////////////////////////////////////////////////////' !
      IF(level.EQ.0 .AND. nFailures.GT.0 ) THEN
         WRITE(mout,*) '++++++++++ STOP in FoamC_Check, found total ', nFailures, ' failures ' !
         WRITE(mout,*) '++++++++++ iErro= ',iError
         STOP
      ENDIF
      END                       ! FoamC_Check



      SUBROUTINE FoamC_BufPrint(mout)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   all cells                                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            iCell,mout,active,j
*     ----------------------------------------------------------------
      active  =0
      WRITE(mout,'(3a)') '==============================================', ' ALL-CELLS ',
     $                   '=============================================='
      WRITE(mout,'(3a)') 
     $     ' iCell  Stat  Pare  Dau1  Dau2 kBest       Xave     Volume      Drive  TrueInteg  Ver1  Ver2  ...'
      DO iCell = 1, m_LastCe
         WRITE(mout,'(6i6,4f11.5,20i6)')
     $        iCell, m_CeStat(iCell),  m_CePare(iCell), m_CeDau1(iCell), m_CeDau2(iCell), !
     $        m_CeBest(iCell),                          ! pointer to best division
     $        m_CeXave(iCell),                          ! factor for Best division 
     $        m_CeVolu(iCell),                          ! Cartesian Volume
     $        m_CeDriv(iCell),                          ! Drive 
     $        m_CeIntg(iCell),                          ! TrueInteg
     $        (m_CeVert(iCell,j), j=1,m_nDim+1)         ! vertices
         IF(m_kDim .GT. 0 ) THEN
            WRITE(mout,'(a,(50g11.6))')
     $           '       HypCubs Posit&Size: ',
     $           (m_CeVer1(iCell,j), j=1,m_kDim), ! position
     $           (m_CeVer2(iCell,j), j=1,m_kDim)  ! size
         ENDIF
         IF(m_CeStat(iCell).EQ.1) active  = active +1
      ENDDO
      WRITE(mout,*) ' All cells: ',m_LastCe, ' Active: ', active
      END                       !! FoamC_BufPrint



      SUBROUTINE FoamC_BufActPrint(mout)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   Active cells only                                                              //
*//   Side=1 indicates that this cell is "side leaf" sticking out of main branch     //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            iCell,mout,active,Side,Pare,j
      DOUBLE PRECISION   DriRat,IntgSum,DrivSum,Fact,WtMin,WtMax
      DOUBLE PRECISION   AveWt, Sigma, Nentry
*     ----------------------------------------------------------------
      WRITE(mout,'(3a)') '==================================================', ' ACTIVE CELLS ',
     $                   '=================================================='
      IntgSum =0d0
      DrivSum =0d0
      WtMin   =  1d60
      WtMax   = -1d60
      active  =0
      WRITE(mout,'(2a)') ' iCell Stat Pare Dau1 Dau2',
     $   '     WtMin      WtMax        <w>  sigma/<w>     Volume      Drive    TrueInt   Ver1  Ver2 ...' !
      DO iCell = 1, m_LastCe
         IF(m_CeStat(iCell).EQ.1) THEN
            Nentry = m_CeSum(iCell,3)
            AveWt  = m_CeSum(iCell,1)/m_CeSum(iCell,3)
            Sigma  = DSQRT(  ABS(m_CeSum(iCell,2)/Nentry - AveWt**2))
            IF(AveWt.NE.0d0) WtMin = Min( WtMin, m_CeSum(iCell,4)/AveWt)
            IF(AveWt.NE.0d0) WtMax = Max( WtMax, m_CeSum(iCell,5)/AveWt)
            IF(AveWt.NE.0d0) Sigma = Sigma/AveWt
            WRITE(mout,'(5i5, 7f11.5 ,10i5)') 
     $           iCell, m_CeStat(iCell),  m_CePare(iCell),  m_CeDau1(iCell), m_CeDau2(iCell), !
     $           m_CeSum(iCell,4),       ! minWt/AveWt
     $           m_CeSum(iCell,5),       ! maxWt/AveWt
     $           m_CeIntg(iCell) /(m_CeDriv(iCell)+1d-100), ! average weight
     $           Sigma/(AveWt+1d-100),   ! sigma/AveWt
     $           m_CeVolu(iCell),        ! Cartesian volume
     $           m_CeDriv(iCell),        ! Driver Integral
     $           m_CeIntg(iCell),        ! True   Integral
     $           (m_CeVert(iCell,j), j=1,m_nDim+1) ! vertices
            IF(m_kDim .GT. 0 ) THEN
               WRITE(mout,'(a,(50g11.6))')
     $              '      HypCubs Posit&Size: ',
     $              (m_CeVer1(iCell,j), j=1,m_kDim), ! position
     $              (m_CeVer2(iCell,j), j=1,m_kDim)  ! size
            ENDIF
            IntgSum = IntgSum +m_CeIntg(iCell)
            DrivSum = DrivSum +m_CeDriv(iCell)
            active  = active +1
         ENDIF
      ENDDO
      WRITE(mout,'(a,i6,a,2i6)') 'All cells: ',m_LastCe, '      Active: ', active, m_LastAc !
      WRITE(mout,'(a,2f12.5)')   'Minimum and Maximum Weight/<Wt>       = ',WtMin,WtMax !
      WRITE(mout,'(a,2g20.13)')  'Total True Integral in active cells   = ', IntgSum, m_CeIntg(1) !
      WRITE(mout,'(a,2g20.13)')  'Total Driver Integral in active cells = ', DrivSum, m_CeDriv(1) !
      WRITE(mout,'(a,f12.5)')    'True/Drive = ', IntgSum/DrivSum
      END                       !! FoamC_BufActPrint


      SUBROUTINE FoamC_VertPrint(mout)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   all vertices                                                                   //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            mout, iCell, iVe, NoRefs(m_vMax), NoRefsAc(m_vMax), k,j
*     ----------------------------------------------------------------
      DO iVe = 1, m_LastVe
         NoRefs(iVe)=0
      ENDDO
      DO iVe = 1, m_LastVe
         DO iCell = 1, m_LastCe
            DO k=1,m_nDim+1
               IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefs(iVe) =NoRefs(iVe) +1 !
               IF(m_CeStat(iCell) .EQ. 1) THEN
                  IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefsAc(iVe) =NoRefsAc(iVe) +1 !
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      WRITE(mout,*) '=====================ALL VERTICES===================================' !
      WRITE(mout,*) ' iVert   NoRefs  NoRefsAc       Vertex     Componets    ' !
      DO iVe = 1, m_LastVe
         WRITE(mout,'(i6,2i10,5f17.10)') iVe,NoRefs(iVe),NoRefsAc(iVe), (m_VerX(iVe,j),j=1,m_nDim) !
      ENDDO
      END                       !! VertPrint



      SUBROUTINE FoamC_PltBegin(ltx)     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Initialization, write header of TeX file                                       //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER   ltx
*---------------------------------------------------
      m_ltx  = ABS(ltx)
      IF(ltx .GT. 0 ) THEN
         WRITE(m_ltx,'(2A)') '\\newpage'
      ELSE
*------------------------------!
*           Header
*------------------------------!
         WRITE(m_ltx,'(A)') '\\documentclass[12pt]{article}'
         WRITE(m_ltx,'(A)') '\\usepackage{color}' !<-for colors!!!
         WRITE(m_ltx,'(A)') '\\usepackage{epic}' !<-for extended ploting
         WRITE(m_ltx,'(A)') '\\textwidth  = 16cm'
         WRITE(m_ltx,'(A)') '\\textheight = 18cm'
         WRITE(m_ltx,'(A)') '\\pagestyle{empty}'
         WRITE(m_ltx,'(A)') '\\begin{document}'
         WRITE(m_ltx,'(A)') '  '
         WRITE(m_ltx,'(A)') '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' !
         WRITE(m_ltx,'(A)') '\\begin{figure}[!ht]'
         WRITE(m_ltx,'(A)') '\\centering'
*------------------------------!
* Frames and labels
*------------------------------!
         WRITE(m_ltx,'(A)') '% =========== big frame, title etc. ======='
         WRITE(m_ltx,'(A)') '\\setlength{\\unitlength}{0.1mm}'
         WRITE(m_ltx,'(A)') '\\begin{picture}(1600,1600)'
         WRITE(m_ltx,'(A)') '\\put(0,0){\\framebox(1600,1600){ }}'
      ENDIF
      END

      SUBROUTINE FoamC_PltVert     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Plot all vertices                                                              //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            iCell, iVe, NoRefs(m_vMax), NoRefsAc(m_vMax), k,j
*------------------------------------------------------------------------------
*     Mark plots for plots
      CHARACTER*62 star,diamond,circle,ring,times,disc,plus,box,dot
      PARAMETER (diamond ='\\makebox(0,0){\\Large $\\diamond$}')
      PARAMETER (star    ='\\makebox(0,0){\\Large $\\star$}')
      PARAMETER (circle  ='\\circle{30}')
      PARAMETER (ring    ='\\circle{20}')
      PARAMETER (times   ='\\makebox(0,0){\\Large $\\times$}')
      PARAMETER (disc    ='\\circle*{20}')
      PARAMETER (plus    ='\\makebox(0,0){\\Large $+$}')
      PARAMETER (box     ='\\makebox(0,0){\\Large $\\Box$}') !!! does not work???
      PARAMETER (dot     ='\\circle*{10}')
*------------------------------------------------------------------------------
      CHARACTER*62  chmark
      INTEGER       kx,ky
*---------------------------------------------------------------------------------------------
      IF(m_nDim.EQ.2) THEN
* Count references of vertices
         DO iVe = 1, m_LastVe
            NoRefs(iVe)=0
         ENDDO
         DO iVe = 1, m_LastVe
            DO iCell = 1, m_LastCe
               DO k=1,m_nDim+1
                  IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefs(iVe) =NoRefs(iVe) +1 !
                  IF(m_CeStat(iCell) .EQ. 1) THEN
                     IF( iVe .EQ. m_CeVert(iCell,k) ) NoRefsAc(iVe) =NoRefsAc(iVe) +1 !
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
*---------------------------------------------------------------------------------------------
* Begin frame
         WRITE(m_ltx,'(A)') '% =========== Vertices Vertices ==========='
         WRITE(m_ltx,'(A)') '\\put(0,0){\\begin{picture}( 1600,1600)' !
         WRITE(m_ltx,'(A)') '\\put(0,0){\\framebox( 1600,1600){ }}' !
* Plotting symbol
         IF(m_LastVe.LE.250) THEN
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VD}[2]{\\put(#1,#2){',disc,'}}' !
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VS}[2]{\\put(#1,#2){',star,'}}' !
         ELSE
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VD}[2]{\\put(#1,#2){',dot,'}}' !
            WRITE(m_ltx,'(10A)') '\\newcommand{\\VS}[2]{\\put(#1,#2){',dot,'}}' !
         ENDIF
         WRITE(m_ltx,'(10A)') 
     $        '\\newcommand{\\VN}[3]{\\put(#1,#2){\\makebox(0,0)[b]{\\hbox{\\color{red}\\scriptsize #3}}}}' !
         DO iVe = 1, m_LastVe
            kx = m_VerX(iVe,1)*1600
            ky = m_VerX(iVe,2)*1600
*****         WRITE(*,*) NoRefs(iVe),NoRefsAc(iVe)
            IF( NoRefsAc(iVe).LE.2 ) THEN
               WRITE(m_ltx,'(A,I5,A,I5,A)') '\\VD{',kx,'}{',ky,'}'
            ELSE
               WRITE(m_ltx,'(A,I5,A,I5,A)') '\\VS{',kx,'}{',ky,'}'
            ENDIF            
            IF(m_LastVe.LE.250) WRITE(   m_ltx,'(A,I5,A,I5,A,I5,A)') '\\VN{',kx-8,'}{',ky+12,'}{',iVe,'}' !
         ENDDO
* Close frame
         WRITE(m_ltx,'(A)') '\\end{picture}}'
         WRITE(m_ltx,'(A)') '% ============== End Vertices ==========='
      ENDIF
*---------------------------------------------------------------------------------------------
      END                       !! VertPrint


      SUBROUTINE FoamC_PltCell     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Plot all simplectic cells                                                      //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
      INTEGER            iCell,active,j
      INTEGER            iV1,iV2,iV3
      INTEGER            kx1,ky1,kx2,ky2,kx3,ky3,kx,ky,lx,ly
*---------------------------------------------------------------------------------------------
*                        Rectangular 2-dim FOAM
*---------------------------------------------------------------------------------------------
      IF(m_kDim.EQ.2) THEN
         WRITE(m_ltx,'(A)') '% =========== Vertices Vertices ==========='
         WRITE(m_ltx,'(A)') '\\put(0,0){\\begin{picture}( 1600,1600)' !
         DO iCell = 2, m_LastCe
            IF(m_CeStat(iCell).EQ.1) THEN ! Only active cells
               kx = m_CeVer1(iCell,1)*1600
               ky = m_CeVer1(iCell,2)*1600
               lx = m_CeVer2(iCell,1)*1600
               ly = m_CeVer2(iCell,2)*1600
               kx1 = kx+lx/2
               ky1 = ky+ly/2
*     cell rectangle
               WRITE(m_ltx,'(A,I5,A,I5,A,I5,A,I5,A)') 
     $              '\\put(',kx,',',ky,'){\\color{black}\\framebox(',lx,',',ly,'){ }}' !
***               WRITE(m_ltx,'(A,I5,A,I5,A,I5,A,I5,A)') 
***     $              '\\put(',kx,',',ky,'){\\color{black}\\dashbox{7}(',lx,',',ly,'){ }}' !
*     cell number
               IF(m_LastCe.LE.250) WRITE(m_ltx,'(A,I4,A,I4,A,I4,A)') 
     $            '\\put(',kx1,',',ky1,'){\\makebox(0,0)[b]{\\hbox{\\small\\color{magenta}\\scriptsize ',iCell,' }}}' !
            ENDIF
         ENDDO
* Close frame
         WRITE(m_ltx,'(A)') '\\end{picture}}'
         WRITE(m_ltx,'(A)') '% ============== End Vertices ==========='
      ENDIF
*---------------------------------------------------------------------------------------------
*                        Triangulare 2-dim FOAM
*---------------------------------------------------------------------------------------------
      IF(m_nDim.EQ.2) THEN
         WRITE(m_ltx,'(A)') '% =========== Vertices Vertices ==========='
         WRITE(m_ltx,'(A)') '\\put(0,0){\\begin{picture}( 1600,1600)' !
         DO iCell = 2, m_LastCe
            iV1=m_CeVert(iCell,1)
            iV2=m_CeVert(iCell,2)
            iV3=m_CeVert(iCell,3)
            kx1 = m_VerX(iV1,1)*1600
            ky1 = m_VerX(iV1,2)*1600
            kx2 = m_VerX(iV2,1)*1600
            ky2 = m_VerX(iV2,2)*1600
            kx3 = m_VerX(iV3,1)*1600
            ky3 = m_VerX(iV3,2)*1600
            kx= (kx1+kx2+kx3)/3
            ky= (ky1+ky2+ky3)/3
            IF(m_CeStat(iCell).EQ.1) THEN
***         WRITE(*,*) iCell,iV1,iV2,iV3
               WRITE(m_ltx,'(A,I4,A,I4,A,I4,A,I4,A)') '\\drawline(',kx1,',',ky1,')(',kx2,',',ky2,')' !
               WRITE(m_ltx,'(A,I4,A,I4,A,I4,A,I4,A)') '\\drawline(',kx2,',',ky2,')(',kx3,',',ky3,')' !
               WRITE(m_ltx,'(A,I4,A,I4,A,I4,A,I4,A)') '\\drawline(',kx3,',',ky3,')(',kx1,',',ky1,')' !
               IF(m_LastCe.LE.250) WRITE(m_ltx,'(A,I4,A,I4,A,I4,A)') 
     $              '\\put(',kx,',',ky,'){\\makebox(0,0)[b]{\\hbox{\\color{magenta}\\scriptsize ',iCell,' }}}' !
            ENDIF
         ENDDO
* Close frame
         WRITE(m_ltx,'(A)') '\\end{picture}}'
         WRITE(m_ltx,'(A)') '% ============== End Vertices ==========='
      ENDIF
      END                       !! FoamC_BufPrint

      SUBROUTINE FoamC_PltMisc     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   Miscelaneous                                                                   //
*//   Plot of frame for 2-dim void testing function                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
*
      WRITE(m_ltx,'(A)') '\\put(80,80){\\color{blue}\\dashbox{7}( 1440,1440){ }}' ! 5% edge band
      END                       !! FoamC_PltMisc

      SUBROUTINE FoamC_PltEnd     !# Miscelaneous and debug
*//////////////////////////////////////////////////////////////////////////////////////
*//   DEBUG                                                                          //
*//   Close Tex file with plot                                                       //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FoamC.h'
*
      WRITE(m_ltx,'(A)') '\\end{picture}'
      WRITE(m_ltx,'(A)') '\\end{figure}'
      WRITE(m_ltx,'(A)') '\\end{document}'
      CLOSE(m_ltx)
      END

*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
*
* $Id: ranlux.f,v 1.1.1.1 2000/10/26 19:49:41 jadach Exp $
*
* $Log: ranlux.f,v $
* Revision 1.1.1.1  2000/10/26 19:49:41  jadach
* starting version
*
* Revision 1.1.1.1  1996/04/01 15:02:55  mclareni
* Mathlib gen
*
*
Cgs #include "gen/pilot.h"
Cgs #include "pilot.h"
      SUBROUTINE RANLUX(RVEC,LENV)
C         Subtract-and-borrow random number generator proposed by
C         Marsaglia and Zaman, implemented by F. James with the name
C         RCARRY in 1991, and later improved by Martin Luescher
C         in 1993 to produce "Luxury Pseudorandom Numbers".
C     Fortran 77 coded by F. James, 1993
C
C   LUXURY LEVELS.
C   ------ ------      The available luxury levels are:
C
C  level 0  (p=24): equivalent to the original RCARRY of Marsaglia
C           and Zaman, very long period, but fails many tests.
C  level 1  (p=48): considerable improvement in quality over level 0,
C           now passes the gap test, but still fails spectral test.
C  level 2  (p=97): passes all known tests, but theoretically still
C           defective.
C  level 3  (p=223): DEFAULT VALUE.  Any theoretically possible
C           correlations have very small chance of being observed.
C  level 4  (p=389): highest possible luxury, all 24 bits chaotic.
C
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for RANLUX:                                  ++
C!!!      CALL RANLUX (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero (not included) and one (also not incl.). ++
C!!!      CALL RLUXGO(LUX,INT,K1,K2) initializes the generator from  ++
C!!!               one 32-bit integer INT and sets Luxury Level LUX  ++
C!!!               which is integer between zero and MAXLEV, or if   ++
C!!!               LUX .GT. 24, it sets p=LUX directly.  K1 and K2   ++
C!!!               should be set to zero unless restarting at a break++ 
C!!!               point given by output of RLUXAT (see RLUXAT).     ++
C!!!      CALL RLUXAT(LUX,INT,K1,K2) gets the values of four integers++
C!!!               which can be used to restart the RANLUX generator ++
C!!!               at the current point by calling RLUXGO.  K1 and K2++
C!!!               specify how many numbers were generated since the ++
C!!!               initialization with LUX and INT.  The restarting  ++
C!!!               skips over  K1+K2*E9   numbers, so it can be long.++
C!!!   A more efficient but less convenient way of restarting is by: ++
C!!!      CALL RLUXIN(ISVEC)    restarts the generator from vector   ++
C!!!                   ISVEC of 25 32-bit integers (see RLUXUT)      ++
C!!!      CALL RLUXUT(ISVEC)    outputs the current values of the 25 ++
C!!!                 32-bit integer seeds, to be used for restarting ++
C!!!      ISVEC must be dimensioned 25 in the calling program        ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(LENV)
      DIMENSION SEEDS(24), ISEEDS(24), ISDEXT(25)
      PARAMETER (MAXLEV=4, LXDFLT=3)
      DIMENSION NDSKIP(0:MAXLEV)
      DIMENSION NEXT(24)
      PARAMETER (TWOP12=4096., IGIGA=1000000000,JSDFLT=314159265)
      PARAMETER (ITWO24=2**24, ICONS=2147483563)
      SAVE NOTYET, I24, J24, CARRY, SEEDS, TWOM24, TWOM12, LUXLEV
      SAVE NSKIP, NDSKIP, IN24, NEXT, KOUNT, MKOUNT, INSEED
      INTEGER LUXLEV
      LOGICAL NOTYET
      DATA NOTYET, LUXLEV, IN24, KOUNT, MKOUNT /.TRUE., LXDFLT, 0,0,0/
      DATA I24,J24,CARRY/24,10,0./
C                               default
C  Luxury Level   0     1     2   *3*    4
      DATA NDSKIP/0,   24,   73,  199,  365 /
Corresponds to p=24    48    97   223   389
C     time factor 1     2     3     6    10   on slow workstation
C                 1    1.5    2     3     5   on fast mainframe
C
C  NOTYET is .TRUE. if no initialization has been performed yet.
C              Default Initialization by Multiplicative Congruential
      IF (NOTYET) THEN
         NOTYET = .FALSE.
         JSEED = JSDFLT  
         INSEED = JSEED
         WRITE(6,'(A,I12)') ' RANLUX DEFAULT INITIALIZATION: ',JSEED
         LUXLEV = LXDFLT
         NSKIP = NDSKIP(LUXLEV)
         LP = NSKIP + 24
         IN24 = 0
         KOUNT = 0
         MKOUNT = 0
         WRITE(6,'(A,I2,A,I4)')  ' RANLUX DEFAULT LUXURY LEVEL =  ',
     +        LUXLEV,'      p =',LP
            TWOM24 = 1.
         DO 25 I= 1, 24
            TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
   25    CONTINUE
         TWOM12 = TWOM24 * 4096.
         DO 50 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
         NEXT(I) = I-1
   50    CONTINUE
         NEXT(1) = 24
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .EQ. 0.) CARRY = TWOM24
      ENDIF
C
C          The Generator proper: "Subtract-with-borrow",
C          as proposed by Marsaglia and Zaman,
C          Florida State University, March, 1989
C
      DO 100 IVEC= 1, LENV
      UNI = SEEDS(J24) - SEEDS(I24) - CARRY 
      IF (UNI .LT. 0.)  THEN
         UNI = UNI + 1.0
         CARRY = TWOM24
      ELSE
         CARRY = 0.
      ENDIF
      SEEDS(I24) = UNI
      I24 = NEXT(I24)
      J24 = NEXT(J24)
      RVEC(IVEC) = UNI
C  small numbers (with less than 12 "significant" bits) are "padded".
      IF (UNI .LT. TWOM12)  THEN
         RVEC(IVEC) = RVEC(IVEC) + TWOM24*SEEDS(J24)
C        and zero is forbidden in case someone takes a logarithm
         IF (RVEC(IVEC) .EQ. 0.)  RVEC(IVEC) = TWOM24*TWOM24
      ENDIF
C        Skipping to luxury.  As proposed by Martin Luscher.
      IN24 = IN24 + 1
      IF (IN24 .EQ. 24)  THEN
         IN24 = 0
         KOUNT = KOUNT + NSKIP
         DO 90 ISK= 1, NSKIP
         UNI = SEEDS(J24) - SEEDS(I24) - CARRY
         IF (UNI .LT. 0.)  THEN
            UNI = UNI + 1.0
            CARRY = TWOM24
         ELSE
            CARRY = 0.
         ENDIF
         SEEDS(I24) = UNI
         I24 = NEXT(I24)
         J24 = NEXT(J24)
   90    CONTINUE
      ENDIF
  100 CONTINUE
      KOUNT = KOUNT + LENV
      IF (KOUNT .GE. IGIGA)  THEN
         MKOUNT = MKOUNT + 1
         KOUNT = KOUNT - IGIGA
      ENDIF
      RETURN
C
C           Entry to input and float integer seeds from previous run
      ENTRY RLUXIN(ISDEXT)
         NOTYET = .FALSE.
         TWOM24 = 1.
         DO 195 I= 1, 24
         NEXT(I) = I-1
  195    TWOM24 = TWOM24 * 0.5
         NEXT(1) = 24
         TWOM12 = TWOM24 * 4096.
      WRITE(6,'(A)') ' FULL INITIALIZATION OF RANLUX WITH 25 INTEGERS:'
      WRITE(6,'(5X,5I12)') ISDEXT
      DO 200 I= 1, 24
      SEEDS(I) = REAL(ISDEXT(I))*TWOM24
  200 CONTINUE
      CARRY = 0.
      IF (ISDEXT(25) .LT. 0)  CARRY = TWOM24
      ISD = IABS(ISDEXT(25))
      I24 = MOD(ISD,100)
      ISD = ISD/100
      J24 = MOD(ISD,100)
      ISD = ISD/100
      IN24 = MOD(ISD,100)
      ISD = ISD/100
      LUXLEV = ISD
        IF (LUXLEV .LE. MAXLEV) THEN
          NSKIP = NDSKIP(LUXLEV)
          WRITE (6,'(A,I2)') ' RANLUX LUXURY LEVEL SET BY RLUXIN TO: ',
     +                         LUXLEV
        ELSE  IF (LUXLEV .GE. 24) THEN
          NSKIP = LUXLEV - 24
          WRITE (6,'(A,I5)') ' RANLUX P-VALUE SET BY RLUXIN TO:',LUXLEV
        ELSE
          NSKIP = NDSKIP(MAXLEV)
          WRITE (6,'(A,I5)') ' RANLUX ILLEGAL LUXURY RLUXIN: ',LUXLEV
          LUXLEV = MAXLEV
        ENDIF
      INSEED = -1
      RETURN
C
C                    Entry to ouput seeds as integers
      ENTRY RLUXUT(ISDEXT)
      DO 300 I= 1, 24
         ISDEXT(I) = INT(SEEDS(I)*TWOP12*TWOP12)
  300 CONTINUE
      ISDEXT(25) = I24 + 100*J24 + 10000*IN24 + 1000000*LUXLEV
      IF (CARRY .GT. 0.)  ISDEXT(25) = -ISDEXT(25)
      RETURN
C
C                    Entry to output the "convenient" restart point
      ENTRY RLUXAT(LOUT,INOUT,K1,K2)
      LOUT = LUXLEV
      INOUT = INSEED
      K1 = KOUNT
      K2 = MKOUNT
      RETURN
C
C                    Entry to initialize from one or three integers
      ENTRY RLUXGO(LUX,INS,K1,K2)
         IF (LUX .LT. 0) THEN
            LUXLEV = LXDFLT
         ELSE IF (LUX .LE. MAXLEV) THEN
            LUXLEV = LUX
         ELSE IF (LUX .LT. 24 .OR. LUX .GT. 2000) THEN
            LUXLEV = MAXLEV
            WRITE (6,'(A,I7)') ' RANLUX ILLEGAL LUXURY RLUXGO: ',LUX
         ELSE
            LUXLEV = LUX
            DO 310 ILX= 0, MAXLEV
              IF (LUX .EQ. NDSKIP(ILX)+24)  LUXLEV = ILX
  310       CONTINUE
         ENDIF
      IF (LUXLEV .LE. MAXLEV)  THEN
         NSKIP = NDSKIP(LUXLEV)
         WRITE(6,'(A,I2,A,I4)') ' RANLUX LUXURY LEVEL SET BY RLUXGO :',
     +        LUXLEV,'     P=', NSKIP+24
      ELSE
          NSKIP = LUXLEV - 24
          WRITE (6,'(A,I5)') ' RANLUX P-VALUE SET BY RLUXGO TO:',LUXLEV
      ENDIF
      IN24 = 0
      IF (INS .LT. 0)  WRITE (6,'(A)')   
     +   ' Illegal initialization by RLUXGO, negative input seed'
      IF (INS .GT. 0)  THEN
        JSEED = INS
        WRITE(6,'(A,3I12)') ' RANLUX INITIALIZED BY RLUXGO FROM SEEDS',
     +      JSEED, K1,K2
      ELSE
        JSEED = JSDFLT
        WRITE(6,'(A)')' RANLUX INITIALIZED BY RLUXGO FROM DEFAULT SEED'
      ENDIF
      INSEED = JSEED
      NOTYET = .FALSE.
      TWOM24 = 1.
         DO 325 I= 1, 24
           TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
  325    CONTINUE
      TWOM12 = TWOM24 * 4096.
         DO 350 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
         NEXT(I) = I-1
  350    CONTINUE
      NEXT(1) = 24
      I24 = 24
      J24 = 10
      CARRY = 0.
      IF (SEEDS(24) .EQ. 0.) CARRY = TWOM24
C        If restarting at a break point, skip K1 + IGIGA*K2
C        Note that this is the number of numbers delivered to
C        the user PLUS the number skipped (if luxury .GT. 0).
      KOUNT = K1
      MKOUNT = K2
      IF (K1+K2 .NE. 0)  THEN
        DO 500 IOUTER= 1, K2+1
          INNER = IGIGA
          IF (IOUTER .EQ. K2+1)  INNER = K1
          DO 450 ISK= 1, INNER
            UNI = SEEDS(J24) - SEEDS(I24) - CARRY 
            IF (UNI .LT. 0.)  THEN
               UNI = UNI + 1.0
               CARRY = TWOM24
            ELSE
               CARRY = 0.
            ENDIF
            SEEDS(I24) = UNI
            I24 = NEXT(I24)
            J24 = NEXT(J24)
  450     CONTINUE
  500   CONTINUE
C         Get the right value of IN24 by direct calculation
        IN24 = MOD(KOUNT, NSKIP+24)
        IF (MKOUNT .GT. 0)  THEN
           IZIP = MOD(IGIGA, NSKIP+24)
           IZIP2 = MKOUNT*IZIP + IN24
           IN24 = MOD(IZIP2, NSKIP+24)
        ENDIF
C       Now IN24 had better be between zero and 23 inclusive
        IF (IN24 .GT. 23) THEN
           WRITE (6,'(A/A,3I11,A,I5)')  
     +    '  Error in RESTARTING with RLUXGO:','  The values', INS,
     +     K1, K2, ' cannot occur at luxury level', LUXLEV
           IN24 = 0
        ENDIF
      ENDIF
      RETURN
      END
