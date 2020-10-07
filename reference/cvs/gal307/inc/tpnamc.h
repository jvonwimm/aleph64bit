*CD tpnamc
      PARAMETER (LTPTE=12, LBTE=1000*LTPTE, LBTEX=LBTE/2)
      PARAMETER (NWPHT=6 , LBHT=1000*NWPHT, LBHTX=LBHT/2)
      PARAMETER (NWPHE=1 , LBHE=1000*NWPHE, LBHEX=LBHE/2)
      COMMON /TPNAMC/ NATPTE,NATPHT,NATPHE,NATTHT,NATTHE,NATPCO,NATPCH,
     &                 NATCRL
     &               ,JDTWRK,JDSORT,JDWORK
#if defined(DOC)
     Name indices for TPC BOS banks
      JDTWRK = 'TWRK' working bank index (TPCOOR)
      JDSORT = working bank index (TPHSRT)
      JDWORK = working bank index (TPHSRT)
#endif
