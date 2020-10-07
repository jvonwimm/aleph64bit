      INTEGER FUNCTION ALRLEP (IELEP,BLEP,IPERI,IFILL,IPOL)
C -------------------------------------------------------------------
CKEY ALEF LEP RLEP
C - B.Bloch-Devaux 891106
C! Build data run header RLEP
C - Input  :  IELEP   = LEP energy in Mev
C             BLEP    = Beam type (character*4)
C             IPERI   = Lep operation period number
C             IFILL   = Lep fill number
C             IPOL    = Polarisation code
C - Output :  ALRLEP  = RLEP bank index
C                       0 means not enough space to book the bank
C  ---------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JRLELE=1,JRLELB=2,JRLELD=3,JRLELF=4,JRLELP=5,LRLEPA=5)
      CHARACTER*4   BLEP
      EXTERNAL INTCHA
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
C --------------------------------------------------------------------
C - Book the bank 'RLEP'
      CALL AUBOS ('RLEP',0,LMHLEN+LRLEPA,JRLEP,IGARB)
      IF (JRLEP.EQ.0) GOTO 999
      IW(JRLEP+LMHCOL) = LRLEPA
      IW(JRLEP+LMHROW) = 1
      CALL BKFMT ('RLEP','3I,A,3I')
C
C - fill the  row
      KRLEP = JRLEP + LMHLEN
      IW(KRLEP+JRLELE) = IELEP
      IW(KRLEP+JRLELB) = INTCHA(BLEP)
      IW(KRLEP+JRLELD) = IPERI
      IW(KRLEP+JRLELF) = IFILL
      IW(KRLEP+JRLELP) = IPOL
C
 999  CONTINUE
      ALRLEP = JRLEP
      END
