      SUBROUTINE EHIBOS
C.----------------------------------------------------------------------
C Y.Karyotakis M.Rumpf
C! ECAL bos banks init
C  - Called by ECHIT
C  - Calls     BKFMT,BLIST
C.______________________________________________________________________
      SAVE
      PARAMETER (NTTD = 1000)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LSTCK=3,LPHI=384,LTHET=228)
      PARAMETER (LWPLA=45,LMODU=12,LCOMP=3)
      PARAMETER (NTHSG=12,NPHSG=24)
      COMMON/EWIRES/ IEWHTS(LWPLA+1,LCOMP*LMODU)
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C
      PARAMETER(JESHPT=1,JESHTI=2,JESHDE=3,LESHIA=5)
      PARAMETER(JEWHPT=1,JEWHMI=2,JEWHDE=3,LEWHIA=47)
C -------------------------------------------------------------------
C
C   Define BOS banks for EC signals
C   Drop them if they exist and then book them again
C
      IDRP = IW(NAESHI)
      IF (IDRP.NE.0) CALL BDROP (IW,'ESHIEWHI')
C
      CALL ALBOS('ESHI',0,NTTD*LESHIA+LMHLEN,KESHI,IGARB)
      IW ( KESHI+LMHCOL) = LESHIA
C
      CALL ALBOS('EWHI',0,NTTD*LEWHIA+LMHLEN,KEWHI,IGARB)
      IW ( KEWHI+LMHCOL ) = LEWHIA
C
      CALL BLIST(IW,'E+','ESHI')
      CALL BLIST(IW,'T+','EWHIETHTEWHT')
C
      END
