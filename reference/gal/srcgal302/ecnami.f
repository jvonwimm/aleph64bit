      SUBROUTINE ECNAMI
C --------------------------------------------------------
C - F.Ranjard - 860422
C - define formats and name-indices of EC BOS banks
C   set working bank indices to 0
C - Called from   ECIRUN                       from ECAL lib
C - Calls         NAMIND, BKFMT                from BOS77.hlb
C -----------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C
C ------------------------------------------------------------
C
C define name-index
C
       NAETHT = NAMIND('ETHT')
       NAEWHT = NAMIND('EWHT')
       NAESHI = NAMIND('ESHI')
       NAEWHI = NAMIND('EWHI')
       NAETDI = NAMIND('ETDI')
       NAEWDI = NAMIND('EWDI')
       NAEWHE = NAMIND('EWHE')
       NAETTR = NAMIND('ETTR')
       NAEWTR = NAMIND('EWTR')
       NAENDI = NAMIND ('ENDI')
C
C define formats
C
       CALL BKFMT ('EWHT','(I)')
       CALL BKFMT ('ETHT','(I)')
       CALL BKFMT ('ESHI','(I)')
       CALL BKFMT ('EWHI','(I)')
       CALL BKFMT ('ETDI','(I)')
       CALL BKFMT ('EWDI','(I)')
       CALL BKFMT ('EWHE','(I)')
       CALL BKFMT ('ETTR','(I)')
       CALL BKFMT ('EWTR','(I)')
C
C set index of working banks to 0
C
       IDPSIG = 0
       IDEWTM = 0
       IDETTM = 0
       RETURN
       END
