      SUBROUTINE ALVSN(ITYP,IPV,IAV,IYR)
C-----------------------------------------------------------------------
C! Return data type and version numbers of ALEPH programs from RHAH.
CKEY ALEF DATA TYPE / USER
C Author: S.Haywood        22-DEC-1992
C Input banks:      RHAH, ASIM
C Output arguments: ITYP    = data type (as read in)
C                             1=Kingal; 2=Raw; 3=Pot; 4=DST; 5=Mini;
C                             6=Micro; 7=Nano
C                   IPV(10) = version numbers of ALEPH programs.
C                             1: Kingal
C                             2: Galeph
C                             3: Julia
C                             4: Redpot
C                             5: Mini
C                             6: Nano
C                             7: Alpha
C                   IAV(10) = corresponding ALEPHLIB version.
C                   IYR     = year when data taken (89,90,91,92,....)
C                             or to which MC corresponds
C
C This is a more robust version of ALDTYP, however it only uses RHAH and
C is intended for use in Alpha.
C To be effective, it should be called before the RHAH bank is modified
C in case of event output. For a given run, information is saved to
C protect against this situation.
C In case of repeated entries, the last entry is taken.
C
C Whether the data is DATA or MC can be determined from checking whether
C IPV(1) (the Kingal code) is 0 or not.
C-----------------------------------------------------------------------
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
      PARAMETER (JASIYM=1,LASIMA=1)
      SAVE LRUN,ITYPS,IPVS,IAVS,IYRS,IDATE
      CHARACTER * 4 CHAINT,NAME,PNAME(10)
      DIMENSION IPV(10),IPVS(10),IAV(10),IAVS(10)
      DATA PNAME / 'KING','GALE','JULI','REDP','MINI','NANO','ALPH',
     &          3 * '   ' /
      DATA LRUN, ITYPS, IPVS, IAVS, IYRS / -999, 0, 10 * 0, 10 * 0, 0 /
      DATA IDATE / 0 /
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
C++   Check if new run, if so, pick up version numbers.
C
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.NE.LRUN .AND. IRUN.GT.0) THEN
         LRUN = IRUN
C
C++      Look at RHAH bank for program versions.
C
         KRHAH = IW(NAMIND('RHAH'))
         IF (KRHAH.GT.0) THEN
            NRHAH = LROWS(KRHAH)
            ITYPS = ITABL(KRHAH,NRHAH,JRHANO)
            DO 10 I=1,NRHAH
               NAME = CHAINT(ITABL(KRHAH,I,JRHAPN))
               JVSP = ITABL(KRHAH,I,JRHAPV)
               JVSA = ITABL(KRHAH,I,JRHAAV)
               DO 20 J=1,10
               IF (NAME.EQ.PNAME(J)) THEN
                  IPVS(J) = JVSP
                  IAVS(J) = JVSA
               ENDIF
   20          CONTINUE
               IF (NAME.EQ.'ON L') IDATE = ITABL(KRHAH,I,JRHAPD)
   10       CONTINUE
         ENDIF
C
C++      Determine corresponding year.
C++      For MC: use ASIM.
C++      For data: use run number upto 91, and RHAH after.
C
         IYRS = 0
         IF (IRUN.LT.2000) THEN
            KASIM = IW(NAMIND('ASIM'))
            IF (KASIM.GT.0) IYRS = ITABL(KASIM,1,JASIYM) / 100
         ELSE
            IF (IRUN.LT.14000) THEN
               IF (IRUN.GE. 4000) IYRS = 89
               IF (IRUN.GE. 6000) IYRS = 90
               IF (IRUN.GE.10000) IYRS = 91
            ELSE
               IYRS = IDATE / 10000
            ENDIF
         ENDIF
      ENDIF
C
C++   Fill return arguments.
C
      ITYP = ITYPS
      DO 100 J=1,10
         IPV(J) = IPVS(J)
         IAV(J) = IAVS(J)
  100 CONTINUE
      IYR = IYRS
C
      RETURN
      END
