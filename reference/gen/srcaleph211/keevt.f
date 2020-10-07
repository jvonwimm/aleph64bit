      SUBROUTINE KEEVT (IEVT,ISTAT,NVX,NTRK,WEIT,IDPRO,ECMS,LWRT)
C -----------------------------------------------------------------
C - F.Ranjard - 870505
C! KINGAL end of event
CKEY KINE KINGAL FILL BANK /  INTERNAL
C   if it is a good event fill the EVEH and KEVH banks and write
C   the event on a file if required . Then reset the BOS array (drop
C   the event list and garbage collect)
C
C - structure: SUBROUTINE program
C              User Entry Name: KEEVT
C              External references: ALEVEH/ALKEVH(ALEPHLIB)
C                                   NAMIND/BWRITE/BLIST/BDROP/BGARB(BOS7
C                                   RDMOUT(CERNLIB)
C              Comdecks referenced: BCS, BMACRO
C
C - usage   : CALL KEEVT (IEVT,ISTAT,NVX,NTRK,WEIT,IDPRO,ECMS,LWRT)
C - input   : IEVT   = event#
C             ISTAT  = status word ( = 1 means OK)
C                      the event is written out only if ISTAT=1
C             NVX    = # of vertices in the event
C             NTRK   = # of tracks in the event
C             IDPRO  = event process identification
C             WEIT   = event weight
C             ECMS   = beam energy in center of mass
C             LWRT   = output logical unit ( 0 means do not write)
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER ALEVEH,ALKEVH,NRDN(3)
      DATA NARUN/0/
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
C ------------------------------------------------------------------
C
C - Get RUNH name-index
      IF (NARUN.EQ.0) NARUN = NAMIND ('RUNH')
C
      IF (ISTAT .EQ. 1) THEN
C     Compress KINE and VERT banks
      CALL KIBPRS ('VERTKINE')
C     Fill EVEH and KEVH banks
         IEXP = IW (IW(NARUN) + 1)
         IRUN = IW (IW(NARUN) + 2)
         IPRO = IW (IW(NARUN) + 3)
         JEVEH = ALEVEH (IEVT,IEXP,IRUN,ECMS,IPRO,ISTAT)
         IF (JEVEH .EQ. 0) GOTO 100
C
         CALL RDMOUT (NRDN)
         JKEVH = ALKEVH (NRDN,NTRK,NVX,IDPRO,WEIT)
         IF (JKEVH .EQ. 0) GOTO 100
C
C     Write event if required
         IF (LWRT .NE. 0) THEN
            IW(1) = 0
            CALL BWRITE (IW,LWRT,'C')
            CALL BLIST (IW,'C=','0')
            IW(1) = 1
            CALL BWRITE (IW,LWRT,'E')
         ENDIF
      ENDIF
C
C - Drop 'E' list and garbage collection
C
 100  CONTINUE
      CALL BDROP (IW,'E')
      CALL BGARB (IW)
C
      END
