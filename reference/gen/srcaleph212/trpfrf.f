      SUBROUTINE TRPFRF(I,PP1,IRF)
C-----------------------------------------------------------------------
CKEY EDIR TRACKS 4-MOMENTUM
C! Charged tracks momentum.
C-
C   Input  : I   = PFRF track number
C   Output : PP1 = PX, PY, PZ and PTOT*charge
C            IRF = Return code
C                   0 : success
C                  -1 : overflow problem in pt value
C-
C   Called by   : LEPTO,TRUSLU
C   Calls  : None
C   Input banks : PFRF
C-
C                                Author: J.C.Brient     date : 29/6/89
C-----------------------------------------------------------------------
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
      PARAMETER(JPFRIR=1,JPFRTL=2,JPFRP0=3,JPFRD0=4,JPFRZ0=5,JPFRAL=6,
     +          JPFREO=7,JPFREM=13,JPFRC2=28,JPFRNO=29,LPFRFA=29)
C --
      PARAMETER ( COSPT = 0.3 , BFIL = 1.5)
      DIMENSION PP1(4)
      LOGICAL DEBUG
      DATA DEBUG/.FALSE./
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
      IRF   = 0
      JPFRF = IW(NAMIND('PFRF'))
      IF(JPFRF.LE.0) GOTO 999
C --
      RT    = 100. * RTABL(JPFRF,I,JPFRIR)
      IF(ABS(RT).LT.0.000001) GO TO 999
      IF(ABS(RT).GT.1000000.) GO TO 999
      FPHI  = RTABL(JPFRF,I,JPFRP0)
      PXY = COSPT * BFIL/RT
      IF(PXY .GT. 0.) THEN
         CH = -1.
      ELSE
         CH = 1.
         PXY = - PXY
      ENDIF
      PP1(1)   =  PXY * COS(FPHI)
      PP1(2)   =  PXY * SIN(FPHI)
      PP1(3)   =  PXY * RTABL(JPFRF,I,JPFRTL)
      PP = VMOD(PP1,3)
      PP1(4)   = CH * PP
C --
      RETURN
  999 CONTINUE
      IRF = -1
      RETURN
      END
