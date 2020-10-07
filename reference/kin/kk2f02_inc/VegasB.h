*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//          Pseudoclass VegasB                                                      //
*//                                                                                  //
*//   Performs n-dimensional Monte Carlo integration.                                //
*//      - by G.P. LEPAGE   Sept 1976/(rev)Apr 1978                                  //
*//      - Algorithm described in J.Comp.Phys. 27, 192 (1978)                        //
*//      - Customized by S. Jadach (1999)                                            //
*//                                                                                  //
*//                                                                                  //
*//                                                                                  //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////

      INTEGER           m_nBinMax,        m_nDimMax
***   PARAMETER (       m_nBinMax = 500,  m_nDimMax=10)
      PARAMETER (       m_nBinMax = 100,  m_nDimMax=10)
      DOUBLE PRECISION  m_alpha,          m_one
      PARAMETER (       m_alpha = 1.5d0,  m_one = 1d0 )

      DOUBLE PRECISION  m_xl,       m_xu,      m_xi,      m_di,      m_Cumulus
      DOUBLE PRECISION  m_MCvector
      DOUBLE PRECISION  m_Accur,    m_si,      m_si2,     m_swgt,    m_schi
      DOUBLE PRECISION  m_IntCrude, m_MCwt
      INTEGER           m_nDim,     m_nCall,   m_IterMax, m_PriLev,  m_NevGen
      INTEGER           m_nBinReq,  m_nBinOld, m_Iter,    m_Flag,    m_nBin
      INTEGER           m_kTotal,   m_nGroup,  m_nSeries
      DOUBLE PRECISION  m_Integral, m_Errabs,  m_Chi2per, m_sumWt,   m_sumWt2

      COMMON /c_VegasB/   
     $    m_xl(m_nDimMax),                ! Lower bound (input)
     $    m_xu(m_nDimMax),                ! Upper bound (input)
     $    m_xi(m_nBinMax,m_nDimMax),      ! Grid matrix, final version (output)
     $    m_di(m_nBinMax,m_nDimMax),      ! Sums of weights
     $    m_Cumulus(m_nBinMax,m_nDimMax), ! Cumulatives for generation
     $    m_MCvector(m_nDimMax),          ! vector generated in MC (post)generation
     $    m_MCwt,                         ! weight generated in MC (post)generation
     $    m_Accur,                   ! Accuracy, relative (input)
     $    m_si,                      ! statistics
     $    m_si2,                     ! statistics
     $    m_swgt,                    ! statistics
     $    m_schi,                    ! statistics chi-squared
     $    m_Integral,                ! Intergrand estimate (output)
     $    m_Errabs,                  ! absolute error est. (output)
     $    m_Chi2per,                 ! Chi2 per iteration  (output) should < 1
     $    m_IntCrude,                ! Crude Integral for MC (post)Generation
     $    m_sumWt,                   ! sum of wt   for MC (post)Generation
     $    m_sumWt2,                  ! sum of wt^2 for MC (post)Generation
     $    m_nDim,                    ! Number of dimensions in the problem (input)
     $    m_nBinReq,                 ! nBin requested
     $    m_nBinOld,                 ! Memorized variable nBin
     $    m_nBin,                    ! Number of bin id grid in each dimension
     $    m_nCall,                   ! Total Number of MC trials (input)
     $    m_kTotal,                  ! grand total per single dimension =nBin*nGroup
     $    m_nGroup,                  ! multiplicity of the group in one bin (nBin)
     $    m_nSeries,                 ! number of MC points in single hipercube
     $    m_IterMax,                 ! Maximum number of iterations (input)
     $    m_Iter,                    ! Iteration counter
     $    m_PriLev,                  ! Printout level (input)
     $    m_NevGen,                  ! No of (post)generated events
     $    m_Flag                     ! Flag
      SAVE /c_VegasB/   
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
