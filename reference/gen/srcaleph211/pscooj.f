      SUBROUTINE PSCOOJ(LIST,IER)
C----------------------------------------------------------------------
C! Convert POT bank PSCO into JULIA bank SCOO
C!
C!    Author:     H. Meinhard       26-May-1989
C!
C!    Input:      - LIST      /C    BOS event list
C!                                  If LIST(2:2) .eq. '-' drop POT bk
C!    Output:     - IER       /I    = 0  successful
C!                                  = 1  input bank does not exist or
C!                                       is empty
C!                                  = 2  not enough space
C!                                  = -1 ok, but garbage collection
C----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPSCSI=1,JPSCLA=2,JPSCSC=3,JPSCTT=4,LPSCOA=5)
      PARAMETER(JSCOSI=1,JSCOLA=2,JSCOSC=3,JSCOTT=4,LSCOOA=5)
      CHARACTER LIST*(*),PLIST*4,JLIST*4
      LOGICAL FIRST
      EXTERNAL NAMIND
      DATA FIRST/.TRUE./
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
C----------------------------------------------------------------------
      IF (FIRST) THEN
        NPSCO = NAMIND('PSCO')
        NSCOO = NAMIND('SCOO')
        CALL BKFMT('SCOO','2I,(3I,2F)')
        FIRST = .FALSE.
      ENDIF
C
      IER = 1
      JPSCO = IW(NPSCO)
      IF (JPSCO .LE. 0)                                     GOTO 999
      NPROW = LROWS(JPSCO)
      IF (NPROW .EQ. 0)                                     GOTO 999
C
      CALL AUBOS('SCOO',0,LMHLEN+NPROW*LSCOOA,JSCOO,IER)
      IF (IER .EQ. 2)                                       GOTO 999
      JLIST = 'SCOO'
      JPSCO = IW(NPSCO)
      IW(JSCOO+LMHCOL) = LSCOOA
      IW(JSCOO+LMHROW) = NPROW
C
      DO 300 IPROW = 1, NPROW
        KSCOO = KROW(JSCOO,IPROW)
        IW(KSCOO+JSCOSI) = ITABL(JPSCO,IPROW,JPSCSI) + 1
        IW(KSCOO+JSCOLA) = ITABL(JPSCO,IPROW,JPSCLA)
        IW(KSCOO+JSCOSC) = ITABL(JPSCO,IPROW,JPSCSC) + 1
        RW(KSCOO+JSCOTT) = REAL(ITABL(JPSCO,IPROW,JPSCTT))/100000.
        RW(KSCOO+JSCOTT+1) = REAL(ITABL(JPSCO,IPROW,JPSCTT+1))/100000.
  300 CONTINUE
C
      PLIST = 'PSCO'
C! add JLIST to S-list, drop PLIST if required
      IF (LNBLNK(LIST).EQ.2) THEN
         IF (LIST(2:2).EQ.'-' .AND. LNBLNK(PLIST).GE.4) THEN
            CALL BDROP (IW,PLIST)
            CALL BLIST (IW,LIST,PLIST(1:LNBLNK(PLIST)))
         ENDIF
      ENDIF
      CALL BLIST (IW,'S+',JLIST(1:LNBLNK(JLIST)))
C
C
      IF (IER .EQ. 1) IER = -1
C
  999 CONTINUE
      RETURN
      END
