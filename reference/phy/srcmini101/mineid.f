      SUBROUTINE MINEID
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill electron ID bank DEID for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C     Modify: Agnieszka Jacholkowska   24-Oct-94
C
C     Input  : EIDT,DTRA banks
C     Output : DEID bank
C
C     Called by MINDST
C
C     Electron information is only interesting for particles with
C     energies greater than about 2 GeV.
C     Therefore, a conservative momentum cut is made.
C     The DTRA track bank is used for this - if not present, it can be
C     created from PFRF by calling MINTRA.
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
      PARAMETER(JEIDIF=1,JEIDR1=2,JEIDR2=3,JEIDR3=4,JEIDR4=5,JEIDR5=6,
     +          JEIDR6=7,JEIDR7=8,JEIDEC=9,JEIDIP=10,JEIDE1=11,
     +          JEIDE2=12,JEIDE3=13,JEIDFR=14,JEIDPE=15,LEIDTA=15)
      PARAMETER(JDTRCH=1,JDTRP0=2,JDTRTH=3,JDTRPH=4,JDTRD0=5,JDTRZ0=6,
     +          JDTRER=7,JDTRTF=12,JDTRHO=13,JDTRHM=14,JDTRVB=15,
     +          JDTRQF=16,JDTREA=17,JDTRVI=27,LDTRAA=27)
      PARAMETER(JDEIR2=1,JDEIR3=2,JDEIQF=3,JDEIDE=4,JDEIDT=5,LDEIDA=5)
C
      LOGICAL FIRST
      DATA CUTP / 1. /
      SAVE FIRST,KUTP
      DATA FIRST,KUTP / .TRUE.,0 /
C
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
      IF(FIRST) THEN
         KUTP = NINT(EFACTM * CUTP)
         FIRST = .FALSE.
      ENDIF
C
C++   Unpack EIDT from PEID if necessary.
C
      KEIDT = NLINK('EIDT',0)
      IF(KEIDT.LE.0) THEN
         KPEID = NLINK('PEID',0)
         IF(KPEID.LE.0) RETURN
         CALL PEIDTJ('  ',IER)
         KEIDT = NLINK('EIDT',0)
      ENDIF
C
C++   Identify the maximum number of DEID entries.
C
      IF(KEIDT.GT.0) THEN
         NEIDT = LROWS(KEIDT)
      ELSE
         NEIDT = 0
      ENDIF
      IF(NEIDT.LE.0) RETURN
      NDEID = NEIDT
C
      KDTRA = IW(NAMIND('DTRA'))
      IF(KDTRA.LE.0) RETURN
      NDTRA = LROWS(KDTRA)
      IF(NDTRA.LE.0) RETURN
C
C++   Create the DEID bank.
C
      LEN = LMHLEN + LDEIDA * NDEID
      CALL AUBOS('DEID',0,LEN, KDEID,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINEID: Cannot create DEID bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KEIDT = NLINK('EIDT',0)
         KDTRA = IW(NAMIND('DTRA'))
      ENDIF
      IW(KDEID+LMHCOL) = LDEIDA
      IW(KDEID+LMHROW) = NDEID
C
C++   Electron identification: R2 and R3.
C++   Quality flag (see EIDT) is also added.
C
      IDEID = 0
      DO 100 I=1,NEIDT
         IDTRA = ITABL(KEIDT,I,JEIDFR)
         IF(IDTRA.LE.0 .OR. IDTRA.GT.NDTRA) GOTO 100
         IF(ITABL(KDTRA,IDTRA,JDTRP0).LT.KUTP) GOTO 100
         IDEID = IDEID + 1
         IEST2 = NINT(100.*RTABL(KEIDT,I,JEIDR2))
         IEST3 = NINT(100.*RTABL(KEIDT,I,JEIDR3))
         IF (IEST2.LT.100*1000) THEN
            IW(KROW(KDEID,IDEID)+JDEIR2) = MIN(MAX(IEST2,-1023),+1023)
         ELSE
            IW(KROW(KDEID,IDEID)+JDEIR2) = +2048
         ENDIF
         IF (IEST3.LT.100*1000) THEN
            IW(KROW(KDEID,IDEID)+JDEIR3) = MIN(MAX(IEST3,-1023),+1023)
         ELSE
            IW(KROW(KDEID,IDEID)+JDEIR3) = +1024
         ENDIF
         IW(KROW(KDEID,IDEID)+JDEIQF) = ITABL(KEIDT,I,JEIDIF)
         IW(KROW(KDEID,IDEID)+JDEIDE) = ITABL(KEIDT,I,JEIDPE)
         IW(KROW(KDEID,IDEID)+JDEIDT) = IDTRA
  100 CONTINUE
C
C++   Compress bank to required size and add the bank to the Mini list.
C
      NDEID = IDEID
      IF(NDEID.GT.0) THEN
         LEN = LMHLEN + LDEIDA * NDEID
         CALL AUBOS('DEID',0,LEN, KDEID,IGARB)
         IW(KDEID+LMHROW) = NDEID
         CALL MINLIS('DEID')
      ELSE
         CALL BDROP(IW,'DEID')
      ENDIF
C
      RETURN
      END
