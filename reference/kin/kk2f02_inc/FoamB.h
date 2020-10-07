*///////////////////////////////////////////////////////////////////////////////////////
*//                                                                                   //
*//          Pseudoclass FoamB                                                        //
*//                                                                                   //
*///////////////////////////////////////////////////////////////////////////////////////
*
      INTEGER        m_NdiMax
      PARAMETER  (   m_NdiMax   = 5 )    ! maximum dimension
      INTEGER        m_nBufMax
      PARAMETER  (   m_nBufMax = 5000 )  ! maximum buffer length for all cells
      INTEGER        m_vMax
      PARAMETER  (   m_vMax    = 5000  ) ! maximum number of vertices
      INTEGER        m_cMax
      PARAMETER  (   m_cMax    = 5000 )  ! maximum number of (active) Cells
      INTEGER        m_sMax
      PARAMETER  (   m_sMax    = 5 )     ! statistics of the weight
      INTEGER        m_NpairMax
      PARAMETER  (   m_NpairMax  = m_NdiMax*(m_NdiMax+1)/2 ) ! no. of pairs of vertices
*
      INTEGER            m_CeStat,  m_CePare,   m_CeDau1,  m_CeDau2, m_CeVert, m_CeBest
      DOUBLE PRECISION   m_CeIntg,  m_CeSum,    m_CeCrud,  m_CeXave, m_CeVolu
      INTEGER            m_LastCe,  m_LastAc,   m_nBuf,    m_LastVe
      INTEGER            m_ActC,    m_Iterat
      DOUBLE PRECISION   m_VerX,    m_VolTot,   m_KillFac,  m_BetaPower
      INTEGER            m_Ndim,    m_Chat,     m_Out,     m_nSampl, m_Ncalls
      INTEGER            m_OptPeek, m_OptCrude, m_OptEdge, m_OptBeta
      DOUBLE PRECISION   m_Crude,   m_SumWt,    m_SumWt2,  m_NevGen,    m_WtMax,   m_WtMin
      DOUBLE PRECISION   m_MCresult,   m_MCerror,   m_MCwt,   m_MCvector
      INTEGER            m_Ltx
*
      COMMON /c_FoamB/   
     $ m_CeStat(m_nBufMax),           ! Cell member: status=0 inactive, =1 active
     $ m_CePare(m_nBufMax),           ! Cell member: parent cell pointer
     $ m_CeDau1(m_nBufMax),           ! Cell member: daughter1 cell pointer
     $ m_CeDau2(m_nBufMax),           ! Cell member: daughter2 cell pointer
     $ m_CeVert(m_nBufMax,m_NdiMax+1),! Cell member: vertex pointers
     $ m_CeIntg(m_nBufMax),           ! Cell member: integral estimator
     $ m_CeCrud(m_nBufMax),           ! Cell member: Crude integral estimate
     $ m_CeVolu(m_nBufMax),           ! Cell member: Cartesian volume
     $ m_CeXave(m_nBufMax),           ! Cell member: Average best X
     $ m_CeBest(m_nBufMax),           ! Cell member: Best pair of vertices, pointer
     $ m_CeSum( m_nBufMax,m_sMax),    ! Cell member: weight summaries
     $ m_VerX(  m_vMax, m_NdiMax), ! List of all VERTEX positions
     $ m_ActC(m_cMax),             ! List of all pointers to ACTIVE cells
     $ m_VolTot,                   ! Estimate of Volume total, without error
     $ m_Crude,             ! M.C. generation Crude value of integral
     $ m_SumWt,             ! M.C. generation sum of Wt
     $ m_SumWt2,            ! M.C. generation sum of Wt**2
     $ m_NevGen,            ! M.C. generation sum of 1d0
     $ m_WtMax,             ! M.C. generation maximum wt
     $ m_WtMin,             ! M.C. generation minimum wt
     $ m_MCresult,          ! M.C. generation Final value of ITEGRAL
     $ m_MCerror,           ! M.C. generation Final walue of ERROR
     $ m_MCwt,              ! M.C. generation current event weight
     $ m_MCvector(m_NdiMax),! M.C. generated vector
     $ m_KillFac,           ! Threshold factor for collapse of cells
     $ m_Ndim,              ! dimension of the problem
     $ m_nBuf,              ! Actual dynamic lenth of the buffer m_nBuf<m_nBufMax
     $ m_LastVe,            ! Last vertex
     $ m_LastAc,            ! Last active cell
     $ m_LastCe,            ! Last cell in buffer 
     $ m_nSampl,            ! No. of sampling when dividing cell
     $ m_Iterat,            ! No. of iterations in consolidation process
     $ m_Ncalls,            ! No. of function calls, total
     $ m_BetaPower,         ! Power in case of m_OptBeta  = 2
     $ m_OptBeta,           ! type of choice of edge in the division of the cell
     $ m_OptPeek,           ! type of Peek =0,1 for maximum, random
     $ m_OptCrude,          ! type of Crude =0,1,2 for TrueVol,Sigma,WtMax
     $ m_OptEdge,           ! decides whether vertices are included in the sampling
     $ m_Chat,              ! Chat level in output, Chat=1 normal level
     $ m_Ltx,               ! Latex Output unit, for debug
     $ m_Out                ! Output unit
*
      SAVE /c_FoamB/
*
      INTEGER   m_Magic
      DATA      m_Magic /378231178/
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
