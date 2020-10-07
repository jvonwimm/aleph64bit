       SUBROUTINE GVERCR(IPECO,RAD,IER)
C======================================================================
C! Calculate cluster coordinate
C! Author   :- MN Minard             27-JAN-1993
C      Input
C-     IPECO / I = PECO row number
C      Output
C-     RAD   / R = Radius of shower
C      IER   / I = error code
C ====================================================================
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPESKS=1,JPESER=2,JPESED=3,JPESET=4,JPESPE=5,LPESTA=5)
      PARAMETER(JETDTL=1,JETDS1=2,JETDS2=3,JETDS3=4,LETDIA=4)
       DIMENSION XPOINT(4)
       DATA NAPEST, NAETDI , NAPYER /3*0/
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
       IF (NAPEST.EQ.0) THEN
         NAPEST = NAMIND('PEST')
         NAETDI = NAMIND('ETDI')
         NAPYER = NAMIND('PYER')
       ENDIF
       KPEST = IW(NAPEST)
       NPEST = 0
       IF (KPEST.NE.0) NPEST = LROWS(KPEST)
       KETDI = IW(NAETDI)
       NETDI = 0
       IF (KETDI.NE.0) NETDI = LROWS(KETDI)
       KPYER = IW(NAPYER)
       NCELL = 0
       E1 = 0.
       E2 = 0.
       E3 = 0.
       ENS = 0.
       DO IPEST = 1, NPEST
         IF ( ITABL(KPEST,IPEST,JPESPE).EQ.IPECO)NCELL=NCELL + 1
         IF ( ITABL(KPEST,IPEST,JPESPE).EQ.IPECO.AND.
     &        ITABL(KPEST,IPEST,JPESET).GT.0)THEN
C
C-       Look if corresponding ETDI has energy
C
           JETDI = ITABL(KPEST,IPEST,JPESET)
           K = ITABL(KPEST,IPEST,JPESKS)
           EN = FLOAT(ITABL(KETDI,JETDI,K+1))/1000000.
           IADDS = ITABL(KETDI,JETDI,1)
           ITET = IBITS(IADDS,16,8)
           IPHI = IBITS(IADDS,2,9)
           IF (ITET.GT.0.AND.ITET.LT.229) THEN
              IF(IPHI.GT.0.AND.IPHI.LT.384) THEN
                CALL ESRBC('ALEPH',ITET,IPHI,K,XPOINT)
                E1 = E1 + EN*XPOINT(1)
                E2 = E2 + EN*XPOINT(2)
                E3 = E3 + EN*XPOINT(3)
                ENS = ENS + EN
              ENDIF
            ENDIF
         ENDIF
       ENDDO
       IER = 0
       IF ( ENS.GT.0) THEN
         EX1 = E1/ENS
         EX2 = E2/ENS
         EX3 = E3/ENS
         RAD = SQRT(EX1**2+EX2**2+EX3**2)
       ELSE
        IER = 1
       ENDIF
       RETURN
       END
