      SUBROUTINE SIXAPR
C***********************************************************************
C                                                                      *
C   Author   :- B. Bloch-Devaux      30-OCT-1991                       *
C                                                                      *
C! Print the content of SIXA trigger ADC's in readable format          *
C                                                                      *
C   Inputs        :  SIXA bank                                         *
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
      DIMENSION SIADC(4)
      CHARACTER*1 NAM(2)
      REAL SIFOXA,SIDIFO
      INTEGER ISITHR,ISIDIS
      COMMON/SITRCO/ISITHR(4),ISIDIS(2),SIFOXA,SIDIFO
C
C     ISITHR(4) : thresholds in ADC counts for very low , low ,
C                 high , very high triggers on odd/even sums
C     ISIDIS(2) : pattern of disabled segments in sical A and B
C     SIFOXA    : # if SIXA ADC count per SIFO ADC count
C     SIDIFO    : # of SIFO ADC count per Mev
      DATA NAM/'A','B'/
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
      NASIXA = NAMIND('SIXA')
      KSIXA = IW(NASIXA)
      IF (KSIXA.GT.0) THEN
        WRITE(IW(6),1000)
        DO 100 IEND= 1,2
           WRITE(IW(6),1001) NAM(IEND)
           DO 200 ISECT= 1,8
             DO 300 IPTY= 1,2
               DO 400 IROAD = 1,2
              IBT = 16*(IROAD-1) + 8*(IEND-1) + ISECT
              JPNTR=KROW(KSIXA,(IEND-1)*8+ISECT)+IPTY
              IADC = IBITS(IW(JPNTR),16*(IROAD-1),16)
              SIADC(2*(IPTY-1)+IROAD) = IADC/(SIFOXA*SIDIFO)
 400    CONTINUE
 300    CONTINUE
        IF ( MAX(SIADC(1),SIADC(2),SIADC(3),SIADC(4)).GT.0)
     $  WRITE(IW(6),1002) ISECT,SIADC
 200    CONTINUE
 100    CONTINUE
      ENDIF
 1000 FORMAT(//,' +++SIXAPR+++ Sical  TriggerSignals  (MeV)',//,
     * '                    odd  layers        even  layers    ',/,
     * '                   road1  road2      road1    road2   ',/)
 1001 FORMAT(' Endcap ',A,'    : ')
 1002 FORMAT(' Sector ',I5,4F10.1,/)
      RETURN
      END
