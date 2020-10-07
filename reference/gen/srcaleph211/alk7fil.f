       SUBROUTINE ALK7FIL (TAPE,IRET)
C ------------------------------------------------------------------
C - F.Ranjard - 920114
CKEY ALEF RUN TAPE / USER
C! set the FILI CARD for a given K7
C - Input   : TAPE       / A  = tape number i.e. AA1234_99
C - Output  : IRET       / I  = return code
C                               =0  Ok
C                               =1  no K7
C                               =2  no space to create FILI card
C -------------------------------------------------------------------
      CHARACTER*(*) TAPE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER ACARD1
      CHARACTER STRING*68, TYPE*4, LABEL*3, CART*12
C -------------------------------------------------------------------
      IER = 0
      IND = ACARD1(' ')
      IND = NDROP ('FILI',0)
      CART = TAPE
      IF (CART(1:1) .EQ. ' ') THEN
         IER = 1
         GOTO 999
      ENDIF
      IND = NBANK ('FILI',0,17)
      IF (IND.EQ.0) THEN
         IER = 2
      ELSE
C
         LC = LNBLNK(CART)
         IF (LC.EQ.6) CART = CART(1:6)//'.1'
         JU = INDEX (CART,'_')
         IF (JU.GT.0) CART(JU:JU) = '.'
         JP = INDEX(CART(8:),'.')
         IF (JP.EQ.0) THEN
            LABEL = '.SL'
            IF (CART(1:2) .EQ.'AA')LABEL = '.AL'
            IF (CART(1:2) .EQ.'AR')LABEL = '.AL'
            IF (CART(1:2) .EQ.'AS')LABEL = '.AL'
            IF (CART(1:2) .EQ.'AX')LABEL = '.NL'
            LC = LNBLNK(CART)
            CART = CART(1:LC)//LABEL
         ENDIF
         LC = LNBLNK(CART)
         STRING = 'ALDATA | EPIO | CART '//CART(1:LC)
         CALL ALINST (STRING,IW(IND+1),NWRDS)
         IND = NBANK('FILI',0,NWRDS)
      ENDIF
C
 999  CONTINUE
      END
