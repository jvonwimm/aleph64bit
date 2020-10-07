      INTEGER FUNCTION ALKEVH (NRDN,NTRK,NVRTX,IDPRO,WEIT)
C ------------------------------------------------------------------
C - F.Ranjard - 870401
C! Create kine event header KEVH
C - Input :  NRDN   = 1st random number used(3)
C            NTRK   = # of input tracks
C            NVRTX  = # of input vertices
C            IDPRO  = process identification
C            WEIT   = weght of the event
C - Output : ALKEVH = KEVH bank index
C                     0 means not enough space to book the bank
C
C   --------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JKEVRN=1,JKEVNT=2,JKEVNV=3,JKEVPI=4,JKEVWT=5,JKEVSR=6,
     +          JKEVTR=7,LKEVHA=7)
      INTEGER NRDN(*)
      DATA IFI /0/
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
C ----------------------------------------------------------------------
C - at 1st entry, get the random generator type
      IF (IFI .EQ. 0) THEN
         IFI = 1
         CALL ALSEED (IRGTY,ISD1,ISD2)
      ENDIF
C - Book the 'KEVH' bank
      CALL AUBOS ('KEVH',0,LMHLEN+LKEVHA,JKEVH,IGARB)
      IF (JKEVH.EQ.0) GOTO 999
      IW(JKEVH+LMHCOL) = LKEVHA
      IW(JKEVH+LMHROW) = 1
      CALL BKFMT ('KEVH','6I,F,2I')
C
C - Fill 'KEVH'
      KKEVH = JKEVH + LMHLEN
      IW(KKEVH+JKEVRN) = NRDN(1)
      IW(KKEVH+JKEVNT) = NTRK
      IW(KKEVH+JKEVNV) = NVRTX
      IW(KKEVH+JKEVPI) = IDPRO
C
      RW(KKEVH+JKEVWT) = WEIT
C
      IF (IRGTY .GE. 2) IW(KKEVH+JKEVSR) = NRDN(2)
      IF (IRGTY .EQ. 3) IW(KKEVH+JKEVTR) = NRDN(3)
C
 999  CONTINUE
      ALKEVH = JKEVH
      END
