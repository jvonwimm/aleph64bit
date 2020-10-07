C
C! Common for ALPHA Energy Flow Objects matching routines
C
      INTEGER MXMC,MXEF,MXMTCH
      PARAMETER( MXMC   = 200 )
      PARAMETER( MXEF   = 200 )
      PARAMETER( MXMTCH = 30 )
      INTEGER LEVT                ! Last event treated
     +       ,NMCIN               ! Number of MC input tracks
     +       ,NEFIN               ! Number of EF input tracks
     +       ,ITMCIN(MXMC)        ! Track numbers of MC input tracks
     +       ,ITEFIN(MXEF)        ! Track numbers of EF input tracks
     +       ,NMCEF(MXMC)         ! Number of MC-EF matches for each MC
     +       ,NEFMC(MXEF)         ! Number of EF-MC matches for each EF
     +       ,ITMCEF(MXMC,MXMTCH) ! Track numbers of each MC-EF match
     +       ,ITEFMC(MXEF,MXMTCH) ! Track numbers of each EF-MC match
      REAL    SMCEF(MXMC,MXMTCH)  ! Matching quantity for each MC-EF match
     +       ,SEFMC(MXEF,MXMTCH)  ! Matching quantity for each EF-MC match
      COMMON / MATCH / LEVT,NMCIN,NEFIN,ITMCIN,ITEFIN
     +  ,NMCEF,NEFMC,ITMCEF,ITEFMC,SMCEF,SEFMC
C
      INTEGER ICDEBU,ICWARN
      COMMON/ ICONT / ICDEBU,ICWARN
      LOGICAL FCDEBU,FCWARN
      COMMON/ FCONT / FCDEBU,FCWARN
C
C-- Some real constants
C
      REAL CTAMH,CTAMS
      PARAMETER( CTAMH = 0.995   )
      PARAMETER( CTAMS = 0.995   )
