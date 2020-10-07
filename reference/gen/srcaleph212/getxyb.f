      SUBROUTINE GETXYB(IRUN,IFOUN,IFL,XYZ,DXYZ,OFS,VLUM)
C --------------------------------------------------------------
C! Gets the beam position per run from bank 'RXYZ'
C - J.Boucrot 15-Dec-1991
CKEY ALEF LFIL BEAM
C
C - Input     : IRUN     / INTE   = run number
C - Output arguments :
C         IFOUN    / INTE   = 0 if no information found for run IRUN
C                           = 1 if information found
C All arguments described below are defined only if IFOUN = 1 :
C         IFL      / INTE   = flag to tell how the beampos was obtained
C                           = -1 no beam position ( not enough events )
C                           =  0 position obtained with ITC/TPC only
C                           =  1 position obtained using the VDET
C         XYZ(3)   / REAL   = mean beam position :  X,Y,Z coordinates
C         DXYZ(3)  / REAL   = errors on X,Y,Z
C         OFS      / REAL   = offset ( mean D0 )
C         VLUM     / REAL   = VDET lumi for this run
C ----------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JRXYRN=1,JRXYNV=2,JRXYFL=3,JRXYBX=4,JRXYBY=5,JRXYBZ=6,
     +          JRXYEX=7,JRXYEY=8,JRXYEZ=9,JRXYLU=10,JRXYOF=11,
     +          LRXYZA=11)
      INTEGER ALGTRO
      REAL XYZ(*),DXYZ(*)
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
C ----------------------------------------------------------------
      IFOUN = 0
      JRXYZ = ALGTRO('RXYZ',IRUN,JRXYRN,JROW)
      IF (JRXYZ.NE.0.AND.JROW.GT.0) THEN
         JRXYZ = IABS(JRXYZ)
         KRXYZ = KROW(JRXYZ,JROW)
         IFL = IW(KRXYZ+JRXYFL)
         CALL UCOPY(RW(KRXYZ+JRXYBX),XYZ(1),3)
         CALL UCOPY(RW(KRXYZ+JRXYEX),DXYZ(1),3)
         VLUM = RW(KRXYZ+JRXYLU)
         OFS  = RW(KRXYZ+JRXYOF)
         IFOUN=1
      ENDIF
 999  RETURN
      END
