      SUBROUTINE SISIDI
C.----------------------------------------------------------------
C  B.Bloch-Devaux  October 91 - January 93
C! SCAL : Format digitizings apllying zero suppresion scheme
C     - Called by SIDIGI
C     - Calls     BDROP,BLIST                    from BOS lib
C                 ALBOS                          from ALEPHlib
C.----------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/SINAMC/NASIHT,NASIHI,NASIDI,NASIX2,NASIXA,NASIFO
      COMMON/SICONST/ISIFCT,ISIZCT,SIMPCT,SIPGEV
C Following parameters are filled from Data Base banks SFTH,SZTH,SRCO
C ISIFCT  : Zero suppression threshold on SIFO data   ( ADC count )
C ISIZCT  : Zero suppression threshold on SIDI data   ( Mev       )
C SIMPCT  : Conversion factor from signal to SIFO ADC ( Mev/ADC count )
C SIPGEV  : enegy deposit per GEV for tracking in Silicium( Gev-1)
      PARAMETER(JSIDAD=1,JSIDE1=2,JSIDE2=3,JSIDE3=4,LSIDIA=4)
      PARAMETER(JSIHAD=1,JSIHE1=2,JSIHE2=3,JSIHE3=4,LSIHTA=4)
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
C-----------------------------------------------------------
C - Drop existing banks
      CALL BDROP (IW,'SIDI')
C - Find length of pad hit bank
      JSIHT = IW(NASIHT)
      IF (JSIHT.LE.0) GO TO 999
      IF (LROWS(JSIHT).LE.0) GO TO 999
C - Create new banks
      LEN = LROWS(JSIHT)*LSIDIA+LMHLEN
      CALL ALBOS('SIDI',0,LEN,JSIDI,IGARB)
      IW(JSIDI+LMHCOL) = LSIDIA
      IW(JSIDI+LMHROW) = 0
      IF ( IGARB.GT.0) JSIHT = IW(NASIHT)
C - Add to the 'E' list
      CALL BLIST(IW,'E+','SIDI')
C
C - Fill SIDI bank
C   copy info from SIHT into SIDI  if at least one of the pads of the
C   triplet is above Zero suppession threshold
C
      DO 10 IP = 1,LROWS(JSIHT)
         KHIT = KROW(JSIHT,IP)
         KDIG = KNEXT(JSIDI)
         IF( (IW(KHIT+JSIHE1).GT.ISIZCT).OR.(IW(KHIT+JSIHE2).GT.ISIZCT)
     $   .OR.     (IW(KHIT+JSIHE3).GT.ISIZCT)) THEN
            IW(KDIG+JSIDAD) = IW(KHIT+JSIHAD)
            IW(KDIG+JSIDE1) = IW(KHIT+JSIHE1)
            IW(KDIG+JSIDE2) = IW(KHIT+JSIHE2)
            IW(KDIG+JSIDE3) = IW(KHIT+JSIHE3)
            IW(JSIDI+LMHROW) = IW(JSIDI+LMHROW) + 1
         ENDIF
 10   CONTINUE
C
 999  RETURN
      END
