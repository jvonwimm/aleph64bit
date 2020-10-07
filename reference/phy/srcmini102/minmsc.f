      SUBROUTINE MINMSC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill  bank DMSC for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    30-Sep-94
C
C     Input  : FRFT bank
C     Output : DMSC bank (multiple scattering angle)
C
C     Called by MINDST
C
C     FRFT is  used after call to MINTRA
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
C
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JPFRIR=1,JPFRTL=2,JPFRP0=3,JPFRD0=4,JPFRZ0=5,JPFRAL=6,
     +          JPFREO=7,JPFREM=13,JPFRC2=28,JPFRNO=29,LPFRFA=29)
      PARAMETER(JDMSC1=1,LDMSCA=1)
C
      INTEGER GTSTUP
      LOGICAL FIRST,XFRID,PACK
      SAVE INDXE,INDXA,C1,HC2,UNDFL,FIRST,NR
      DATA FIRST / .TRUE. /
C
C!    set of intrinsic functions to handle BOS banks
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
C
C++   Initialisation.
C
      IF(FIRST) THEN
C
C++      Determine which FRFT bank to use. Default is with Vdet.
C++      If FRF0 is given, no swap is made in ALPHA and FRFT/0 exists.
C++      For pre-1991 (data and MC), force FRFT/0. Since the FRFT/2 bank
C++      is unusable, drop it to be sure. FRFT/0 will appear as FRFT/3.
C
         IF(NLINK('FRF0',0).LE.0) THEN
            NR = 2
         ELSE
            NR = 0
         ENDIF
         CALL ABRUEV(IRUN,IEVT)
         IF (GTSTUP('VD',IRUN).LE.2) KFRFT = NDROP('FRFT',2)
         FIRST = .FALSE.
      ENDIF
C
C++   Ensure FRFT bank is unpacked from PFRF.
C
      KFRFT = IW(NAMIND('FRFT'))
      IF(KFRFT.LE.0) CALL FPTOJ('  ',IER)
      KFRFT = NLINK('FRFT',NR)
      IF(KFRFT.LE.0) KFRFT = IW(NAMIND('FRFT'))
C
C++   Identify the number of tracks.
C++   If no tracks, return without creating DMSC bank.
C
      IF(KFRFT.LE.0) RETURN
      NFRFT = LROWS(KFRFT)
      IF(NFRFT.LE.0) RETURN
      NDMSC = NFRFT
      IR = IW(KFRFT-2)
c!!   IRM = IR
      IRM = 0
      IF(IR.EQ.3) IRM = 0
      KPFRF = NLINK('PFRF',IRM)
C
C++   Create the DMSC bank.
C
      LEN = LMHLEN + LDMSCA * NDMSC
      CALL AUBOS('DMSC',IRM,LEN, KDMSC,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINMSC: Cannot create DMSC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KFRFT = NLINK('FRFT',IR)
      ENDIF
      IW(KDMSC+LMHCOL) = LDMSCA
      IW(KDMSC+LMHROW) = NDMSC
c     WRITE(IW(6),'('' MINMSC: DMSC bank lifted'')')
C
C++   Loop over the FRFT bank and fill the DMSC bank.
C
      DO 100 I=1,NFRFT
C
C++      Track parameters.
C
         IW(KROW(KDMSC,I)+JDMSC1) = NINT(AFACTM * RTABL(KFRFT,I,JFRFAL))
         I1   = IW(KROW(KDMSC,I)+JDMSC1)
c        PRINT * ,'DMSC', I1, IRM, NFRFT, NDMSC

  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DMSC')
C
      RETURN
      END
