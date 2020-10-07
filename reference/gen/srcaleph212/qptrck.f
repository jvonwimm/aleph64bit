      SUBROUTINE QPTRCK(NT,PZ,PTOT)
C----------------------------------------------------------------------
CKEY EDIR TRACK MOMENTUM
C! Get TPC Track momentum.
C-
C   Input  : NT   =  Track number
C   Output : PZ   =  Track Z momentum component
C            PTOT =  Track total momentum
C-
C   Called by   : MUELID,TRKHAD
C   Calls  : None
C   Input banks : PFRT
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
      PARAMETER(JPFRIR=1,JPFRTL=2,JPFRP0=3,JPFRD0=4,JPFRZ0=5,JPFRAL=6,
     +          JPFREO=7,JPFREM=13,JPFRC2=28,JPFRNO=29,LPFRFA=29)
C --
      PARAMETER(AAAKEC=29.9792458*1.E-5)
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
C   Standard momentum calculation from magnetic field,
C   inverse radius and the tangent of the deep angle.
C --
      PTOT = 0.
      QMFLD = ALFIEL(QMFLD)
      QMFLDC=QMFLD*AAAKEC
C --
      KPFRF = IW(NAMIND('PFRF'))
      IF(KPFRF.LE.0) GOTO 999
C --
      RINR = RTABL(KPFRF,NT,JPFRIR)
      IF(RINR.EQ.0.) GOTO 999
      PS = QMFLDC / RINR
      IF (PS .GT. 0.)  THEN
        PT = PS
      ELSE
        PT = - PS
      ENDIF
      TL = RTABL(KPFRF,NT,JPFRTL)
      PZ = PT * TL
      PTOT = SQRT (PT**2 + PZ**2)
C --
  999 RETURN
      END
