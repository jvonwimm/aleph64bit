      SUBROUTINE PRPART
C ----------------------------------------------------------------
C! Print particle bank PART in readable format
C! B. Bloch-Devaux     861024  mods  873103
C!
C!   No arguments
C!   refers to Banks PART and KLIN if it exits
C!
      SAVE
       EXTERNAL NAMIND
       CHARACTER*4 CHAINT, NAME(3)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPARGN=1,JPARNA=2,JPARCO=5,JPARMA=6,JPARCH=7,JPARLT=8,
     +          JPARMW=9,JPARAN=10,LPARTA=10)
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
C
C --------------------------------------------------------------------
C
      LOUT = IW(6)
      JPART = IW(NAMIND('PART'))
      JKLIN = IW(NAMIND('KLIN'))
      IF (JPART.EQ.0) THEN
        WRITE (LOUT,'(/1X,''+++PRPART+++ NO PART   bank - RETURN'')')
        RETURN
      ENDIF
      WRITE ( LOUT,1010 )
 1010  FORMAT(/1X,'+++PRPART+++  Particle bank'/T3,
     & 'Aleph #',2X,'Geant #',6X,'Name', 9X,'trck.type',8X,
     1 'Mass',3X,'charge',2X,'lif.time',2X,'M.width',3X,'anti#',3X,
     2 'generat#'/)
C
      NPART = LROWS (JPART)
      KPART = JPART+LMHLEN
      DO 10 I=1,NPART
         ICOR=0
         IF (JKLIN.GT.0) ICOR=ITABL(JKLIN,I,1)
         IF (LCOLS(JPART) .GE. JPARAN) THEN
            WIDTH = RTABL(JPART,I,JPARMW)
            IANTI = ITABL(JPART,I,JPARAN)
         ELSE
            WIDTH = 0.
            IANTI = 0
         ENDIF
         DO 9 J=1,3
            NAME(J) = CHAINT (IW(KPART+1+J))
 9       CONTINUE
         WRITE (LOUT,1011) I,IW(KPART+1),NAME,IW(KPART+5)
     &                      ,(RW(KPART+K),K=6,8),WIDTH,IANTI,ICOR
         KPART = KPART + LCOLS(JPART)
 10   CONTINUE
C
      RETURN
 1011 FORMAT(T4,I6,2X,I5,5X,3A4,5X,I4,6X,F12.6,F4.1,E12.3,F10.6,3X,I5
     &         ,3X,I8)
      END
