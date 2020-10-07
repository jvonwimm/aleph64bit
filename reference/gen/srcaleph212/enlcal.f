      SUBROUTINE ENLCAL(CLAS7,CLAS8)
C----------------------------------------------------------------------
CKEY EDIR DEF CLASS7->8
C! Luminosity from LCAL.
C-
C   Input  : None
C   Output : CLAS7 = Class 7 logical flag
C            CLAS8 = Class 8 logical flag
C-
C   Called by   : SELEVT
C   Calls  : None
C   Input banks : LUPA
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JLUPGB=1,JLUPHV=2,JLUPT1=3,JLUPL2=4,JLUPTE=5,JLUPLO=6,
     +          JLUPAM=7,JLUPMD=8,JLUPEF=9,JLUPEC=10,JLUPEW=12,
     +          JLUPXC=14,JLUPYC=16,JLUPTC=18,JLUPPC=20,JLUPAX=22,
     +          JLUPAY=24,JLUPXX=26,JLUPYY=28,JLUPXY=30,JLUPXA=32,
     +          JLUPYA=34,JLUPXD=36,JLUPYD=38,JLUPAD=40,JLUPIT=42,
     +          JLUPCT=44,JLUPTT=46,JLUPPT=48,JLUPXT=50,JLUPYT=52,
     +          JLUPE1=54,JLUPE2=56,LLUPAA=57)
C --
      LOGICAL CLAS7,CLAS8
C --
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
C --
      ELCALA = 0.
      ELCALB = 0.
      CLAS7 = .FALSE.
      CLAS8 = .FALSE.
C --
      KLUPA=IW(NAMIND('LUPA'))
      IF(KLUPA.LE.0) GOTO 999
C --
C   Loop on LCAL clusters and get separately the energy
C   of the LCAL sides A and B
C --
      NLCTW = LROWS(KLUPA)
      DO 40 NLT = 1,NLCTW
        ELCALA = ELCALA+RTABL(KLUPA,NLT,JLUPEC)
        ELCALB = ELCALB+RTABL(KLUPA,NLT,JLUPEC+1)
   40 CONTINUE
C --
      IF(ELCALA.GT.15 .AND. ELCALB.GT.15.) CLAS7 = .TRUE.
      IF(ELCALA.GT.15 .OR. ELCALB.GT.15.)  CLAS8 = .TRUE.
C --
  999 RETURN
      END
