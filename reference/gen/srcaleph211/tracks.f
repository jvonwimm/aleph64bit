        SUBROUTINE TRACKS (NTRK,CLAS5,CLAS6)
C----------------------------------------------------------------------
CKEY EDIR DEF CLASS5->6
C! Tracks and TPC hits, classes 5 and 6 code.
C-
C   Input  : None
C   Output : NTRK  = Number of TPC tracks with Nhits > 4, |D0| <  5 cm
C                                                     and |Z0| < 20 cm
C            CLAS5 = Class 5 logical flag
C            CLAS6 = Class 6 logical flag
C-
C   Called by   : SELEVT
C   Calls  : None
C   Input banks : PFRF,PFRT
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
      PARAMETER(JPFRNV=1,JPFRNI=2,JPFRNE=3,JPFRNT=4,JPFRNR=5,LPFRTA=5)
C --
      LOGICAL CLAS5,CLAS6
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
      NTRK = 0
      CLAS5 = .FALSE.
      CLAS6 = .FALSE.
C --
      KPFRF = IW(NAMIND('PFRF'))
      KPFRT = IW(NAMIND('PFRT'))
      IF(KPFRF.LE.0 .OR. KPFRT.LE.0) GOTO 999
C --
C   Loop on charged tracks
C --
      NTRACK = LROWS(KPFRF)
      DO 10 ITK = 1,NTRACK
        D0 = ABS(RTABL(KPFRF,ITK,JPFRD0))
        Z0 = ABS(RTABL(KPFRF,ITK,JPFRZ0))
        NH = ITABL(KPFRT,ITK,JPFRNT)
        IF(NH.LT.4 .OR. D0.GT.5. .OR. Z0 .GT.20.) GO TO 10
        NTRK = NTRK+1
   10 CONTINUE
C --
      IF(NTRK.GE.1 .AND. NTRK.LE.7) CLAS5 = .TRUE.
      IF(NTRK.GE.8) CLAS6 = .TRUE.
C --
  999 RETURN
      END
