*////////////////////////////////////////////////////////////////////////////////
*//                                                                            //
*//   ======================================================================   //
*//   ======================================================================   //
*//   ===========================KORALW=====================================   //
*//   ======================WW pair production==============================   //
*//   =================initial state exponentiation=========================   //
*//   ======================================================================   //
*//                                                                            //
*////////////////////////////////////////////////////////////////////////////////
*     Author list:
*          S. Jadach      (Stanislaw.Jadach@cern.ch)
*          W. Placzek     (Wieslaw.Placzek@cern.ch)
*          M. Skrzypek    (Maciej.Skrzypek@cern.ch)
*          B.F.L. Ward    (bflw@slac.stanford.edu)
*          Z. Was         (Zbigniew.Was@cern.ch)
*////////////////////////////////////////////////////////////////////////////////
*
      INTEGER     m_Npar
      REAL(16)    m_Xpar
      INTEGER     m_IdGen,     idyfs
      REAL(16)    m_Xcrude, m_svar,    m_alpha_s,   Preco,    Wtu
      REAL(16)    m_XSecMC, m_XErrMC
      REAL(16)    m_XSecNR, m_XErrNR, m_tCosMin
      INTEGER     m_KeyWgt, m_KeySmp,  m_KeyIsr,  m_Key4f,  m_KeyAcc
      INTEGER     m_NevTru, IevAcc,    NevTot,    m_KeyBra
      INTEGER     m_i_4f,   m_i_beta
      INTEGER     m_i_prnt, m_i_read_wt, m_i_prwt 
      INTEGER     m_i_writ_wt, m_i_writ_4v, m_i_pres, m_i_sw4f
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
     $  m_tCosMin,               ! t-ch-like bremss emulation cut-off
     $  Preco,                   ! Should go to lower level??
     $  m_Xcrude,                ! Crude x-section
     $  Wtu,                     ! Should go to lower level??
     $  m_IdGen,                 ! THIS generator ident
     $  idyfs,                   ! Used/Defined in lower levels?
     $  m_KeyWgt,                ! Weighted events
     $  m_KeySmp,                ! Type of sampler
     $  m_KeyIsr,                ! ISR on/off
     $  m_KeyBra,                ! W branching ratios
     $  m_Key4f,                 ! Matrix element
     $  m_KeyAcc,                ! Anomalous couplings
     $  IevAcc,                  ! Counter of accepted events
     $  m_NevTru,                ! Counter of MC events broadcost to user
     $  NevTot,                  ! Probably the same as m_NevTru, to be eliminated???
     $  m_i_4f,                  ! Monitoring
     $  m_i_beta,                ! Monitoring
     $  m_i_prnt,                ! wtover printouts 
     $  m_i_read_wt,             ! suboption fori_disk=2
     $  m_i_writ_wt,             ! suboption fori_disk=2
     $  m_i_writ_4v,             ! suboption fori_disk=2
     $  m_i_prwt,                ! principal ISR weight
     $  m_i_pres,                ! presampler probabilities monitor
     $  m_i_sw4f                 ! 4f matrix el suboptions
*
      SAVE    /c_KW/
*////////////////////////////////////////////////////////////////////////////////
*//                                                                            //
*//                                                                            //
*//                                                                            //
*//                                                                            //
*////////////////////////////////////////////////////////////////////////////////
