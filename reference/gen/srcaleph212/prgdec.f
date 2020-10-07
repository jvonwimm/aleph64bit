      SUBROUTINE PRGDEC
C ----------------------------------------------------------------
C! Print decay  bank GDEC in readable format
C! B. Bloch-Devaux     870812
C!
C!   No arguments
C!   refers to Banks GDEC (if it exits   ) and PART
C!
C --------------------------------------------------------------------
      SAVE
       EXTERNAL NAMIND
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      CHARACTER*4 CTABL , CHAINT
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
C - Lth CHAR*4 element of the NRBOSth row of the bank with index ID
      CTABL(ID,NRBOS,L) = CHAINT(ITABL(ID,NRBOS,L))
C
C --------------------------------------------------------------------
C
      LOUT = IW(6)
      NAMP = NAMIND('GDEC')
      NAPAR = NAMIND('PART')
      JPART=IW(NAPAR)
      IF (NAMP.EQ.0) THEN
        WRITE (LOUT,'(''/1X,+++PRGDEC+++ NO GDEC   bank - RETURN'')')
        RETURN
      ENDIF
      WRITE ( LOUT,1010 )
 1010  FORMAT(/1X,'+++PRGDEC+++  Decay modes  '/T9,
     & 'Aleph #',8X,'Branching ratio(%)  ',19X,'decay products',
     2 /)
C
      JPAR = IW(NAMP)
      NPAR=LROWS (JPAR)
      J=JPAR+LMHLEN
      IF (NPAR.GT.0) THEN
         DO 10 I=1,NPAR
           WRITE (LOUT,1011) IW(J+1),(CTABL(JPART,IW(J+1),K),K=2,4)
           DO 20 IM=1,6
           BR=RTABL(JPAR,I,IM+1)
           IF (BR.GT.0.) THEN
                 ID=ITABL(JPAR,I,IM+7)
                 N1=ID/10000
                 N2=MOD(ID/100,100)
                 N3=MOD(ID,100)
              IF (N1.GT.0) THEN
                 WRITE (LOUT,1013) BR,ID,(CTABL(JPART,N3,II),II=2,4),
     &          (CTABL(JPART,N2,II),II=2,4),(CTABL(JPART,N1,II),II=2,4)
              ELSE
                 WRITE (LOUT,1013) BR,ID,
     &          (CTABL(JPART,N3,II),II=2,4),(CTABL(JPART,N2,II),II=2,4)
              ENDIF
           ENDIF
 20        CONTINUE
            J=J+LCOLS(JPAR)
 10      CONTINUE
      ENDIF
C
      RETURN
 1011  FORMAT(T4,I6,2X,3A4)
 1013  FORMAT(T24,F10.4,15X,I8,3(5X,3A4))
      END
