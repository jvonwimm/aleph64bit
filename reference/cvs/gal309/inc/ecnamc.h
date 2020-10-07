*CD ecnamc
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C
#if defined(DOC)
      LPS1, LPS2         IDPSIG working bank miniheader
      NAE...            name-index of bank 'E...'
                        the index of the bank 'E...' with the lowest number
                        is stored in IW(NAE...)
      IDPSIG            index of the working bank 'PSIG'
      IDEWTM            index of working bank 'EWTM'
      IDETTM            index of working bank 'ETTM'
#endif
