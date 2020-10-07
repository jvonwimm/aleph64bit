      COMMON /ABRCOM/ BATCH,INIT,CLOSE1,CLOSE2,FWFILM
     &               ,IUNDAT(2),IUTDAT(5),IUNSEL,IUNSE2,IUTSEL
     &               ,MASKA,MASKW
     &               ,WLIST,TLIST
      LOGICAL BATCH, INIT, CLOSE1, CLOSE2, FWFILM
      CHARACTER*1   TLIST, WLIST
C
#if defined(DOC)
      BATCH           is false when running in INTEractive mode
      INIT            is true  when setting up ABRREC
      CLOSE1          is true when EDIR has to be written
      CLOSE2          is true when data file has to be written
      FWFILM          is true when a FILM card has to be written
      IUNDAT          input data file logical unit
      IUTDAT          output data file logical unit
      IUNSEL          input EDIR file logical unit
      IUNSE2          not used
      IUTSEL          output EDIR file logical unit
      MASKA           30 bits mask with all bit set
      MASKW           30 bits write class word
      WLIST           if not ' ' write this list out
      TLIST           input list 'E' or 'R'
#endif
