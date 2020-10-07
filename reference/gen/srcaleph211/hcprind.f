      SUBROUTINE HCPRIND (ICODE)
C --------------------------------------------
CKEY PRINT HCAL PRINT / USER
C!    decode HLWD and HWDI and print them
C!
C!       Author      : F.Ranjard - 911121
C!
C        Input       :       ICODE  / I   = 1 print HLWD
C                                           2       HWDI
C                                           3       both
C!       Input bank  :       HWDI and HLWD
C!
C!      -Called by : user
C!      -Calls     : MVBITS  from CERNLIB
C-------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JHLWWA=1,JHLWHP=2,LHLWDA=2)
      PARAMETER(JHWHTA=1,LHWHTA=1)
      PARAMETER(JHWDCA=1,LHWDIA=1)
      PARAMETER (LHIT=3, LDIG=5)
      DIMENSION ITUPR(LDIG),LCLUP(LDIG),ILAPR(LDIG),MODPR(LDIG),
     +          YHIT(LHIT)
      DATA NAHLWD /0/
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C ----------------------------------------------
C
      IF (NAHLWD.EQ.0) THEN
         NAHLWD = NAMIND ('HLWD')
         NAHWDI = NAMIND ('HWDI')
      ENDIF
C
      LOUTIO = IW(6)
      IF(ICODE.NE.2 .AND. IW(NAHLWD).GT.0) THEN
         JHLWD = IW(NAHLWD)
         WRITE(IW(6),500) LROWS(JHLWD)
         NHLWD = LROWS(JHLWD)
         KHLWD = JHLWD + LMHLEN
         DO 30 I=1,LROWS(JHLWD),LHIT
            JMAX = MIN (LHIT,NHLWD-I+1)
            DO 20 J=1,JMAX
               N       = IW(KHLWD+1)
               ITUPR(J)= IBITS (N,0,8)
               ILAPR(J)= IBITS (N,16,8)
               MODPR(J)= IBITS (N,24,8)
               YHIT (J)= RW(KHLWD+2)
   20       KHLWD = KHLWD + LHLWDA
            WRITE (LOUTIO,510) I,I+LHIT-1,(MODPR(J),ILAPR(J),
     +      ITUPR(J),YHIT(J),J=1,JMAX)
   30    CONTINUE
      ENDIF
  500 FORMAT (/1X,'+++HCPRIND+++ HLWD hit position along the tube ',
     +         I5/ 8X,3(4X,'  Mo  La  Tu  position '))
  510 FORMAT (1X,I3,'-',I3,3(4X,3I4,F10.2,1X))
C
      IF(ICODE.GE.2 .AND. IW(NAHWDI).GT.0) THEN
         JHWDI = IW(NAHWDI)
         NHWDI = LROWS(JHWDI)
         WRITE(LOUTIO,600) NHWDI
         KHWDI = JHWDI + LMHLEN
         DO 50 I=1,NHWDI,LDIG
            JMAX = MIN (LDIG,NHWDI-I+1)
            DO 40 J=1,JMAX
               N = IW(KHWDI+1)
               ITUPR(J)= IBITS (N,0,8)
               LCLUP(J)= IBITS (N,8,8)
               ILAPR(J)= IBITS (N,16,8)
               MODPR(J)= IBITS (N,24,8)
   40       KHWDI = KHWDI + LHWDIA
            WRITE (LOUTIO,610) I,I+LDIG-1,(MODPR(J),ILAPR(J),ITUPR(J),
     &      LCLUP(J),J=1,JMAX)
   50    CONTINUE
      ENDIF
      RETURN
  600 FORMAT (/1X,'+++HCPRIND+++ HWDI McTubeDigitising ',I5/ 8X,5(4X,
     +'  Mo  La  Tu  Cl'))
  610 FORMAT (1X,I3,'-',I3,5(4X,4I4))
      END
