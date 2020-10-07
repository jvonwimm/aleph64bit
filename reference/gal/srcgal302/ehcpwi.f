      SUBROUTINE EHCPWI
C.----------------------------------------------------------------------
C Y.Karyotakis March 86
C! Fill Wire bank EWHT
C  - Called by ECASIG
C  - Calls ALBOS,BKFMT,BLIST
C.----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
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
C -------------------------------------------------------------------
C
C - If hit bank does not exist return
C
      IF (IW(NAETHT) .EQ. 0) RETURN
C
C        Find number of fired modules
C
      NMODF = 0
      DO 1 I = 1,LMODU * LCOMP
         IF( IEWHTS(1,I) .NE. 0 ) NMODF = NMODF + 1
    1 CONTINUE
C
C        Book EWHT bank with the right number of words
C
            CALL ALBOS('EWHT',0,NMODF*(LWPLA+1)+LMHLEN,KTHW,IGARB)
            CALL BLIST(IW,'E+','EWHT')
C
            IW( KTHW + LMHCOL ) = LWPLA + 1
            IW( KTHW + LMHROW ) = NMODF
C
C        Fill EWHT bank from IEWHTS table
C
      KTHW = KTHW + LMHLEN
      ICW  = 1
      DO 2 I = 1,LMODU * LCOMP
         IF( IEWHTS(1,I) .NE. 0 ) THEN
              DO 3 J=1,LWPLA + 1
               IW(KTHW+ICW+J-1) = IEWHTS(1+J-1,I)
    3         CONTINUE
              ICW = ICW + LWPLA+1
         ENDIF
    2 CONTINUE
C
 980  RETURN
      END
