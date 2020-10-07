      SUBROUTINE SIXAMK
C*********************************************************************
C                                                                    *
C   Author   :- Joe Boudreau          30-OCT-1991                    *
C                                                                    *
C! Build the SIXA bank for Sical trigger                             *
C                                                                    *
C   Inputs        :  SIFO bank                                       *
C   Outputs       :  SIXA bank                                       *
C                                                                    *
C*********************************************************************
C
      SAVE
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
      REAL SIFOXA,SIDIFO
      INTEGER ISITHR,ISIDIS
      COMMON/SITRCO/ISITHR(4),ISIDIS(2),SIFOXA,SIDIFO
C
C     ISITHR(4) : thresholds in ADC counts for very low , low ,
C                 high , very high triggers on odd/even sums
C     ISIDIS(2) : pattern of disabled segments in sical A and B
C     SIFOXA    : # if SIXA ADC count per SIFO ADC count
C     SIDIFO    : # of SIFO ADC count per Mev
      PARAMETER(JSIXAO=1,JSIXAE=2,LSIXAA=2)
      PARAMETER(JSIFAD=1,JSIFA1=2,JSIFA2=3,JSIFA3=4,LSIFOA=4)
C
      PARAMETER ( IFUL = 4095)
      INTEGER KSIXA,KSIFO,ITRIP,KSIXB
      INTEGER JPNT,IADDR,I,IAND,MIN,ISHFT,IMD,IST,IPH,IRD,IRT
      INTEGER ISEG,IESEG,IPTY,IFE,KPNT
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
      KSIFO = IW(NASIFO)
      IF (KSIFO.GT.0) THEN
C
C     Create the SIXA bank with fixed length , auxilliary bank SIXB
C
         LEN = LMHLEN + 16*LSIXAA
         CALL AUBOS('SIXA',0,LEN,KSIXA,IGARB)
         IF (KSIXA.LE.0) RETURN
         IW(KSIXA + LMHCOL) = LSIXAA
         IW(KSIXA + LMHROW) = 16
         LEN = LMHLEN + 32*LSIXAA
         CALL AUBOS('SIXB',0,LEN,KSIXB,IGARB)
         IF (KSIXB.LE.0) RETURN
         IW(KSIXB + LMHCOL) = LSIXAA*2
         IW(KSIXB + LMHROW) = 16
C
C   Loop over each row in the SIFO bank and each plane in the triplet
C   For each Amplex, find the two trigger segments which contain that
C   Amplex.Increment the energy in the SIXA bank by the amount of each
C   fast-or signal.
         DO 100 ITRIP = 1,LROWS(KSIFO)
            IADDR = ITABL(KSIFO,ITRIP,JSIFAD)
            DO 200 I = 1,3
                CALL SIDCOD(IADDR,I-1,IMD,IST,IPH,IRD)
                IF (IMD.LT.0.OR.IST.LT.0.OR.IPH.LT.0.OR.IRD.LT.0) RETURN
                IPTY = MOD(IST+1,2)+1
                DO 300 IRT = 1,2
                   ISEG=MOD((IPH-1)+2*(IRT-1),32)/4+8*(IMD-1)+1
                   JPNT = KROW(KSIXB,ISEG)+IRT+2*(IPTY-1)
                   IW(JPNT) = IW(JPNT)+ ITABL(KSIFO,ITRIP,JSIFA1+I-1)
 300            CONTINUE
 200        CONTINUE
 100     CONTINUE
C
C Now apply the conversion factor to the total signal  in both roads of
C each segment and fill SIXA in a compact format
         DO 400  ISEG = 1,16
         DO 400  IPTY = 1,2
         DO 400  IRT  = 1,2
            JPNT = KROW(KSIXB,ISEG)+IRT+2*(IPTY-1)
            IW(JPNT) = SIFOXA*IW(JPNT)
            IW(JPNT) = MIN(IW(JPNT),IFUL)
            KPNT = KROW(KSIXA,ISEG)+IPTY
            IESEG  = IW(JPNT)
            CALL MVBITS(IESEG,0,16,IW(KPNT),16*(IRT-1))
 400     CONTINUE
      ENDIF
C
      CALL BDROP(IW,'SIXB')
      RETURN
      END
