      SUBROUTINE SIDISC(IEND,IPTY,IPATT)
C***********************************************************************
C                                                                      *
C   Author   :- Joe Boudreau          30-OCT-1991                      *
C                                                                      *
C!  Discriminate the SIXA bank for endcap IEND odd or IEND even.       *
C!  PARITY=1  means odd , PARITY=2 means even                          *
C                                                                      *
C   Inputs        : INTEGER IEND   = 1 if endcap A, = 2 if endcap B    *
C                   INTEGER IPTY   = 1 if odd, = 2 if even             *
C   Outputs       : INTEGER IPATT  bit 0-3 pattern after discrimination*
C                                  against threshholds 1-4             *
C                                                                      *
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
C
      COMMON/X1NAMC/NAXTBN,NAXTCN,NAXTDI,NAXTEB,NAXSGE,NAXSHI,
     &              NAXSSC,NAX1AD,NAX1SC,NAX1TH,NASIXA,NASIX2,NASIFO,
     &              NAX1RG,NAX1IP,NAX1TV
C
      LOGICAL BTEST
      INTEGER IEND,IPTY,IPATT
      INTEGER IROAD,ISECT,IBT,IADC,JPNTR,KSIXA,I
      REAL SIFOXA,SIDIFO
      INTEGER ISITHR,ISIDIS
      COMMON/SITRCO/ISITHR(4),ISIDIS(2),SIFOXA,SIDIFO
C
C     ISITHR(4) : thresholds in ADC counts for very low , low ,
C                 high , very high triggers on odd/even sums
C     ISIDIS(2) : pattern of disabled segments in sical A and B
C     SIFOXA    : # if SIXA ADC count per SIFO ADC count
C     SIDIFO    : # of SIFO ADC count per Mev
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
C
C  Loop over every sector and every road within this endcap and parity,
C  reading the energy deposit from the SIXA bank . If any sector has
C  an  energy deposit above threshold, then set a a bit in IPATT
C  corresponding to that threshold.
C
      IPATT = 0
      KSIXA = IW(NASIXA)
      IF (KSIXA.GT.0) THEN
        DO 200 IROAD = 1,2
          DO 300 ISECT= 1,8
            IBT = 16*(IROAD-1) + 8*(IEND-1) + ISECT -1
            IF (.NOT.BTEST(ISIDIS(IPTY),IBT)) THEN
              JPNTR=KROW(KSIXA,(IEND-1)*8+ISECT)+IPTY
              IADC = IBITS(IW(JPNTR),16*(IROAD-1),16)
              DO 400 I = 1,4
                IF (IADC.GT.ISITHR(I)) IPATT=IBSET(IPATT,I-1)
 400          CONTINUE
            ENDIF
 300      CONTINUE
 200    CONTINUE
      ENDIF
      RETURN
      END
