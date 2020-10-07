      SUBROUTINE BKRHAL(PRNAM,IPVER,ICVER,NATOU,IFAIL)
C-----------------------------------------------------------------------
C! Enlarges and fills the bank RHAH (  Run Header Analysis History )
C! according to the input arguments
CKEY BOOK RUN HEADER / INTERNAL
C Author     J. Boucrot     25-Sep-1988   modified 02-Feb-1989
C Auxiliary to BKRHAW                          from ALEPHLIB
C NOT User-callable
C Calls AUBOS,ALVERS,ADBVER                    from ALEPHLIB
C Input arguments :
C    PRNAM  : Name of the production program ( JULIA or ALPHA )
C    IPVER  : Program version number
C    ICVER  : Correction set version number
C    NATOU  : Code for the output written by the calling program:
C              2 = RAW           3 = POT           4 = DST
C              5 = MiniDST       6 = MicroDST      7 = NanoDST
C Output argument :  IFAIL = 0 if all OK ,  =1 if no room for RHAH
C Modified: P. Comas  8-JUN-1994, Store computer type in last word of RH
C                                 after a call to ALMACH from ALEPHLIB.
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JRHAPN=1,JRHAPD=3,JRHAPH=4,JRHAPV=5,JRHAAV=6,JRHADV=7,
     +          JRHADD=8,JRHANI=9,JRHANO=10,JRHACV=11,JRHANU=12,
     +          LRHAHA=12)
      COMMON / NAMRHA / NARHAH,NARHOH,NARUNH,NAAJOB,
     +                  NAKJOB,NAKRUN
        EXTERNAL NAMIND,INTCHA
        CHARACTER*8 PRNAM,COMPU
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
C-----------------------------------------------------------------------
C Check if RHAH already exists ; if not, create it ; if yes , enlarge it
        IFAIL=1
        JRHAH=IW(NARHAH)
        IF (JRHAH.EQ.0) THEN
           LENRH=LMHLEN+LRHAHA
           NROWS=1
        ELSE
           NROWS=LROWS(JRHAH)+1
           LENRH=LMHLEN+NROWS*LCOLS(JRHAH)
        ENDIF
        CALL AUBOS('RHAH',0,LENRH,JRHAH,IGARB)
        IF (IGARB.EQ.2) GO TO 999
        IW(JRHAH+LMHCOL)=LRHAHA
        IW(JRHAH+LMHROW)=NROWS
C
C Fill the last row with the required entities  :
C
        KRHAH=KROW(JRHAH,NROWS)
        IW(KRHAH+JRHAPN)=INTCHA(PRNAM(1:4))
        IW(KRHAH+JRHAPN+1)=INTCHA(PRNAM(5:8))
C Day , time , program version :
        CALL DATIME(IDAY,ITIM)
        IW(KRHAH+JRHAPD)=IDAY
        IW(KRHAH+JRHAPH)=ITIM
        IW(KRHAH+JRHAPV)=IPVER
        IW(KRHAH+JRHACV)=ICVER
C Computer type:
        CALL ALMACH(COMPU)
        IW(KRHAH+JRHANU)=INTCHA(COMPU(1:4))
C Alephlib version , Database version and date :
        ISAV=IW(6)
        IW(6)=0
        CALL ALVERS(ALVER)
        CALL ADBVER(IDBVE,IDBDA)
        IW(6)=ISAV
C
        IW(KRHAH+JRHAAV)=NINT(10.*ALVER)
        IW(KRHAH+JRHADV)=IDBVE
        IW(KRHAH+JRHADD)=IDBDA
C Nature of input is taken from previous step , if any :
        NATIN=0
        IF (NROWS.GT.1)  NATIN=ITABL(JRHAH,NROWS-1,JRHANO)
        IW(KRHAH+JRHANI)=NATIN
        IW(KRHAH+JRHANO)=NATOU
        IFAIL=0
C
 999   RETURN
       END
