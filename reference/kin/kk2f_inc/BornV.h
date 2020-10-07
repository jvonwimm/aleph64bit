*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                     Pseudo-CLASS  BornV                                  //
*//                                                                          //
*//  Purpose:                                                                //
*//  Provide Born angular distribution and integrated x-section              //
*//  as a function of s.                                                     //
*//                                                                          //
*//  NOTES:                                                                  //
*//  How to eliminate nneut? This will come in a natural way                 //
*//  when neutrino type is generated.                                        //
*//  NB. Zbyszek says that weight dispersion might worsen!                   //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
*
*  Class members:
*
*//////////////////////////////////////////////////////////////////////////////
      DOUBLE PRECISION   m_pi
      PARAMETER         (m_pi =3.1415926535897932d0)
      DOUBLE PRECISION   m_fleps
****  PARAMETER (m_fleps = 1d-35)  ! original
****  PARAMETER (m_fleps = 1d-45)  ! enough???
      PARAMETER (m_fleps = 1d-100)  ! enough!!!
****      PARAMETER (m_fleps = 1d-200)  ! enough!!!
*//////////////////////////////////////////////////////////////////////////////
*//       Energy limits in the EW grid, w=sqrt(s) in GeV units.              //
*//////////////////////////////////////////////////////////////////////////////
      DOUBLE PRECISION  m_WminZ, m_WmaxZ                             ! Z pole interval
      DOUBLE PRECISION  m_WminLEP1,          m_WmaxLEP1              ! LEP1 basic interval
      PARAMETER(        m_WminLEP1=0.010d0,  m_WmaxLEP1=120.001d0 )  ! LEP1 basic interval
      DOUBLE PRECISION  m_WmaxLEP2                                   ! LEP2 interval
      PARAMETER(        m_WmaxLEP2  =240.001d0 )                     ! LEP2 interval
      DOUBLE PRECISION  m_WmaxNLC                                    ! NLC interval
      PARAMETER(        m_WmaxNLC  =1040.001d0 )                     ! NLC interval
      INTEGER           m_poin1, m_poin2, m_poin3, m_poin4, m_poinT
*//////////////////////////////////////////////////////////////////////////////
* 340-point grid, only 80pt for NLC to be improved/tested in future
      PARAMETER( m_poin1 = 120 ) ! wide energy range   (m_WminLEP1,m_WmaxLEP1)
      PARAMETER( m_poin2 =  20 ) ! near z narrow range (amz +- 2*gammz)
      PARAMETER( m_poin3 = 120 ) ! high energy range   (m_WmaxLEP1,m_WmaxLEP2)
      PARAMETER( m_poin4 =  80 ) ! NLC energy range    (m_WmaxLEP2,m_WmaxNLC)
      PARAMETER( m_poinT =  14 ) ! cost(heta) grid
*//////////////////////////////////////////////////////////////////////////////
* EW formfactors, all flavours!!
      DOUBLE COMPLEX     m_cyy,     m_czz,     m_ctt,    m_clc   ! Electroweak FFactors
      DOUBLE PRECISION   m_syy,     m_szz,     m_stt,    m_slc   ! QCD corr.
      DOUBLE COMPLEX     m_GSW
      DOUBLE PRECISION   m_QCDcor
*//////////////////////////////////////////////////////////////////////////////
* EW parameters
      DOUBLE PRECISION   m_Gmu
      DOUBLE PRECISION   m_MZ,      m_amh,     m_amtop
      DOUBLE PRECISION   m_swsq,    m_gammz,   m_amw,    m_gammw
*
      DOUBLE PRECISION   m_CMSene,  m_XXXene,  m_HadMin, m_vvmin,  m_vvmax
      DOUBLE PRECISION   m_AvMult,  m_YFSkon,  m_YFS_IR, m_alfinv, m_alfpi, m_Xenph
      DOUBLE PRECISION   m_vv,      m_x1,      m_x2
      DOUBLE PRECISION   m_Qf,      m_T3f,     m_helic,  m_amferm, m_auxpar
      DOUBLE PRECISION   m_gnanob
      INTEGER            m_IsGenerated, m_KFferm, m_NCf
      INTEGER            m_KFini,       m_KeyINT
      INTEGER            m_KeyElw,      m_KeyZet, m_KeyWtm
      INTEGER            m_out

      COMMON /c_BornV/
* Tables of EW formfactors
     $  m_cyy(m_poin1+1,7,16),           ! formfactor, table
     $  m_czz(m_poin2+1,7,16),           ! formfactor, table
     $  m_ctt(m_poin3+1,m_poinT+1,7,16), ! formfactor, table
     $  m_clc(m_poin4+1,m_poinT+1,7,16), ! formfactor, table
     $  m_syy(m_poin1+1,16),             ! QCD correction, table
     $  m_szz(m_poin2+1,16),             ! QCD correction, table
     $  m_stt(m_poin3+1,m_poinT+1,16),   ! QCD correction, table
     $  m_slc(m_poin3+1,m_poinT+1,16),   ! QCD correction, table
     $  m_GSW(100),                      ! form-factors,   at the actual energy/angle
     $  m_QCDcor,                        ! QCD correction, at the actual energy/angle
*
     $  m_CMSene,                       ! Initial value of CMS energy
     $  m_XXXene,                       ! CMS energy after beamsstrahlung or beam spread
* -------------------- EVENT --------------------------
     $  m_x1,                           ! 1-z1 = x1 for first  beam(strahlung)
     $  m_x2,                           ! 1-z2 = x2 for second beam(strahlung)
     $  m_vv,                           ! v = 1-s'/s
     $  m_AvMult,                       ! Average photon multiplicity CRude at given v
     $  m_YFSkon,                       ! YFS formfactor finite part
     $  m_YFS_IR,                       ! YFS formfactor IR part
* -----------------------------------------------------
     $  m_vvmin,                        ! minimum v, infrared cut
     $  m_vvmax,                        ! maximum v
     $  m_HadMin,                       ! minimum hadronization mass [GeV]
* Basic QED
     $  m_alfinv,                       ! 1/alphaQED, Thomson limit
     $  m_alfpi,                        ! alphaQED/pi
     $  m_Xenph,                        ! Enhancement factor for Crude photon multiplicity
* EW parameters
     $  m_MZ,                           ! Z mass
     $  m_amh,                          ! Higgs mass
     $  m_amtop,                        ! Top mass
     $  m_swsq,                         ! sin(thetaW)**2
     $  m_gammz,                        ! Z width
     $  m_amw,                          ! W mass
     $  m_gammw,                        ! W width
     $  m_Gmu,                          ! Fermi constant (from muon decay)
* Table of fermion paramerets, quarks (1->6) and leptons (11->16)
     $  m_KFferm(20),                   ! fermion KFcode (1->6) and (11->16)
     $  m_NCf(20),                      ! number of colours
     $  m_Qf(20),                       ! electric charge
     $  m_T3f(20),                      ! isospin, L-hand component
     $  m_helic(20),                    ! helicity or polarization
     $  m_amferm(20),                   ! fermion mass
     $  m_auxpar(20),                   ! auxiliary parameter
     $  m_IsGenerated(20),              ! Generation flag, only for SAN !!! 
* Normalization
     $  m_gnanob,                       ! GeV^(-2) to nanobarns
* Initial/final fermion types
     $  m_KFini,                        ! KF code of beam
* Test switches
     $  m_KeyINT,                       ! ISR/FSR INTereference switch
     $  m_KeyElw,                       ! Type of Electrowak Library
     $  m_KeyZet,                       ! Z-boson on/off
     $  m_KeyWtm,                       ! Photon emission without mass terms
     $  m_out                           ! output unit for printouts
      SAVE /c_BornV/
*
* Formats for writing tables onto disk file.
      CHARACTER*80  m_fmt0, m_fmt1, m_fmt2
      PARAMETER (
     $  m_fmt0 ='(4g20.13)',                      ! Mz,Mt,Mh etc.
     $  m_fmt1 ='( a,  i4,  f10.5, i4,  f10.5 )', ! header
     $  m_fmt2 ='(6g13.7)'   )                    ! complex formfactors

*
*  Class procedures:
*
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                      End of CLASS  BornV                                 //
*//////////////////////////////////////////////////////////////////////////////
