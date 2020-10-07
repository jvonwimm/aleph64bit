      SUBROUTINE SITRIN
C***********************************************************************
C                                                                      *
C   Author   :- B. Bloch-Devaux      30-OCT-1991                       *
C                                                                      *
C! Init the trigger constants for SICAL                                *
C                                                                      *
C   Inputs        :  SRCO bank,SITC bank                               *
C   Output        :  SITRCO common                                     *
C                                                                      *
C***********************************************************************
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*4 KEY,CHAINT
      REAL SIFOXA,SIDIFO
      INTEGER ISITHR,ISIDIS
      COMMON/SITRCO/ISITHR(4),ISIDIS(2),SIFOXA,SIDIFO
C
C     ISITHR(4) : thresholds in ADC counts for very low , low ,
C                 high , very high triggers on odd/even sums
C     ISIDIS(2) : pattern of disabled segments in sical A and B
C     SIFOXA    : # if SIXA ADC count per SIFO ADC count
C     SIDIFO    : # of SIFO ADC count per Mev
      PARAMETER(JSITTT=1,JSITDO=2,JSITDE=3,LSITCA=3)
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
C     Get the conversion factor from SIFO ADC count to SIXA ADC count
         SIFOXA = 0.228
         SIDIFO = 0.00913
         ISITHR(1) =  9
         ISITHR(2) = 12
         ISITHR(3) = 22
         ISITHR(4) = 41
         ISIDIS(1) = 0
         ISIDIS(2) = 0
         NASIFO =  NAMIND ('SIFO')
         JSRCO = IW ( NAMIND('SRCO'))
         IF ( JSRCO.GT.0 ) THEN
            DO  10 I= 1,LROWS(JSRCO)
               KEY = CHAINT(ITABL(JSRCO,I,1))
               IF (KEY.EQ.'SIXA') SIFOXA = 0.001*ITABL(JSRCO,I,2)
               IF (KEY.EQ.'SIFO') SIDIFO = 0.00001*ITABL(JSRCO,I,2)
 10         CONTINUE
         ENDIF
         NASITC =  NAMIND ('SITC')
         KSITC  = IW(NASITC)
         IF ( KSITC.GT.0 ) THEN
         ITH = ITABL(KSITC,1,JSITTT)
         DO 100 I = 1,4
            ISITHR(I) = IBITS(ITH,8*(I-1),8)
 100     CONTINUE
         ISIDIS(1) = ITABL(KSITC,1,JSITDO)
         ISIDIS(2) = ITABL(KSITC,1,JSITDE)
         ENDIF
         WRITE(IW(6),110) SIDIFO,SIFOXA,ISITHR,ISIDIS
  110 FORMAT(/2X,'SICAL Trigger conditions :  '
     5    /16X,'Conversion SIFO ADC per SIDI Mev:', F8.4,'     '
     5    /16X,'Conversion SIXA ADC per SIFO ADC:', F8.4,'     '
     2    /16X,'Thresholds in SIXA ADC counts : ',I8  ,' Very low',
     3    /16X,'                                ',I8  ,' Low      '
     4    /16X,'                                ',I8  ,' High     '
     4    /16X,'                                ',I8  ,' Very high'
     5    /16X,'Disable sector words odd/even  :',2I8,'     '
     7                )
      RETURN
      END
