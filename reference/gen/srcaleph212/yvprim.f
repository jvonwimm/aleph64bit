      SUBROUTINE YVPRIM(KPOI,PRIV,SIPRI)
C-------------------------------------------
C! Finds the mean value and the cov mat. of the primary
CKEY YV0 PRIMARY VERTEX / USER
C   author:    M.A.Ciocci 21/2/90
C   modified:  M.A.Ciocci 20/1/93 Included the out diagonal
C                                 terms in the covariance
Cmatrix of the primary vertex
C            input:
C                   kpoi/i     pointer to the jsum bank
C            output:
C                   priv(3)/r  coordinates of the primary vertex
C                   sipri(3,3)/r covariance matrix of the primary
C                                vertex.
C
C     Banks:
C             jsum
C-------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JJSUNT=1,JJSUNV=2,JJSUNZ=3,JJSUNL=4,JJSUNB=5,JJSUVT=6,
     +          JJSUVV=7,JJSUVZ=8,JJSUVL=9,JJSUVB=10,JJSUTT=11,
     +          JJSUTZ=12,JJSUTB=13,JJSULI=14,JJSUIZ=15,JJSULO=16,
     +          JJSULZ=17,JJSUXV=18,JJSUYV=19,JJSUZV=20,JJSUXS=21,
     +          JJSUYS=22,JJSUZS=23,JJSUKB=24,JJSUKW=25,JJSUTN=26,
     +          JJSUTS=27,JJSUTV=28,JJSUA0=29,JJSUA1=30,JJSUA2=31,
     +          JJSUA3=32,JJSUA4=33,JJSUA5=34,JJSUA6=35,JJSUA7=36,
     +          JJSUA8=37,JJSUA9=38,JJSUB0=39,JJSUB1=40,JJSUB2=41,
     +          JJSUB3=42,JJSUB4=43,JJSUB5=44,JJSUB6=45,JJSUB7=46,
     +          JJSUB8=47,JJSUB9=48,LJSUMA=48)
            INTEGER KPOI
            REAL PRIV(3),SIPRI(3,3),PRIV2(3)
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
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
C+ FINDS THE NUMBER OF EVENT WITH PRIMARY VERTEX RECONSTRUCTED
C
         INEVV=ITABL(KPOI,1,JJSUNV)
C
C+  CALCULATES THE MEAN VALUE OF THE PRIMARY VERTEX FOR RUN
C
         PRIV(1)=RTABL(KPOI,1,JJSUXV)/INEVV
         PRIV(2)=RTABL(KPOI,1,JJSUYV)/INEVV
         PRIV(3)=RTABL(KPOI,1,JJSUZV)/INEVV
C
C+  CALCULATES THE MEAN VALUE OF THE PRIMARY VERTEX SQUARE
C
         PRIV2(1)=RTABL(KPOI,1,JJSUXS)/INEVV
         PRIV2(2)=RTABL(KPOI,1,JJSUYS)/INEVV
         PRIV2(3)=RTABL(KPOI,1,JJSUZS)/INEVV
C
C+   AND FINALLY CALCULATES THE SIGMA ON THE MEAN VALUE OF PRIMARY
C+   VERTEX FOR RUN
C
        DO 1 J=1,3
          DO 2 K=1,3
           IF(J.EQ.K)THEN
              SIPRI(J,K)=(PRIV2(J)-PRIV(J)**2)*INEVV/(INEVV-1)
           ELSE
           SIPRI(J,K)=0
           ENDIF
  2       CONTINUE
  1     CONTINUE
        RETURN
        END
