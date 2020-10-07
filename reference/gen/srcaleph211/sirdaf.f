      SUBROUTINE SIRDAF(LUNDAF,IRUN,IOK)
C.---------------------------------------------------------------------
CKEY SCALDES READ DAF BANKS / USER
C     B.BLOCH       October 91
C! SCALDES interface with DAF
C  Fill SCALDES commons from direct access file.
C   Input :
C          LUNDAF Logical Unit for DAF
C          IRUN   Run number
C   Output:
C          IOK   (0 = OK   , -1 = ERROR)
C   Called by USER program
C   Calls: ALGTDB                    from ALEPHLIB
C.---------------------------------------------------------------------
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! global SCAL data base HAC parameters
      PARAMETER(JSALDX=1,JSALDY=2,JSALDZ=3,JSALGX=4,LSALGA=4)
      PARAMETER(JSIGNM=1,JSIGNR=2,JSIGNP=3,JSIGNZ=4,JSIGRN=5,JSIGRX=6,
     +          JSIGPS=7,JSIGZ0=10,JSIGZW=11,JSIGZ1=12,JSIGZ2=13,
     +          JSIGZF=14,JSIGZB=15,JSIGOV=16,JSIGRF=17,JSIGRL=18,
     +          LSIGOA=18)
      PARAMETER(JSINMN=1,JSINTP=3,JSINXR=5,LSINTA=20)
      PARAMETER(JSTYSN=1,JSTYMN=2,JSTYPT=4,LSTYPA=4)
      PARAMETER(JSIPPN=1,JSIPXX=2,JSIPYY=3,LSIPOA=3)
      COMMON/SIGECO/NMODSI,NRBNSI,NPBNSI,NZBNSI,RMINSI(2),RMAXSI(2),
     $              Z0SNSI(2),ZWIDSI,ZWRFSI,ZWRLSI,ZWFRSI,ZWFLSI,
     $              ZWBKSI,ZWLASI,OVLPSI,DPOSSI(3,2),GAPXSI(2),
     $              PHSHFT(3,2),RADSTP,PHISTP,ISINUM(12,2)
      COMMON/SINALI/NASINT,NASIPO
C---------------------------------------------------------------------
C- First fill SCAL Bos banks from direct access file
C
      CHARACTER PLNA * 8
      CHARACTER * 4 CHAINT
      INTEGER LUNDAF,IRUN,IOK
      INTEGER ALGTDB,NAMIND,GTSTUP
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
C- Read Banks for SCAL from d/a file into memory  according to setup
C  when needed
      IOK = 0
C
      ISTP = GTSTUP('SI',IRUN)
      INDEC = ALGTDB(LUNDAF,'SIGO',IRUN)
      IF(INDEC .EQ. 0) GO TO 998
      INDEC = ALGTDB(LUNDAF,'SALGSTYPSINT',ISTP)
      IF(INDEC .EQ. 0) GO TO 998
      NASINT= NAMIND('SINT')

C
C Now fill SCAL commons from banks
C
C  ... Bank SIGO
C
      JSIGO = IW(NAMIND('SIGO'))
      IF (JSIGO.LE.0)                 GO TO 998
      NMODSI = ITABL(JSIGO,1,JSIGNM)
      NRBNSI = ITABL(JSIGO,1,JSIGNR)
      NPBNSI = ITABL(JSIGO,1,JSIGNP)
      NZBNSI = ITABL(JSIGO,1,JSIGNZ)
      RMINSI(1) = RTABL(JSIGO,1,JSIGRN)
      RMAXSI(1) = RTABL(JSIGO,1,JSIGRX)
      Z0SNSI(1) = RTABL(JSIGO,1,JSIGZ0)
      ZWIDSI = RTABL(JSIGO,1,JSIGZW)
      ZWFRSI = RTABL(JSIGO,1,JSIGZF)
      ZWBKSI = RTABL(JSIGO,1,JSIGZB)
      ZWFLSI = RTABL(JSIGO,1,JSIGZ1)
      ZWLASI = RTABL(JSIGO,1,JSIGZ2)
      ZWRLSI = RTABL(JSIGO,1,JSIGRL)
      ZWRFSI = RTABL(JSIGO,1,JSIGRF)
      OVLPSI = RTABL(JSIGO,1,JSIGOV)
      PHSHFT(1,1) = RTABL(JSIGO,1,JSIGPS)  *DEGRA
      PHSHFT(2,1) = RTABL(JSIGO,1,JSIGPS+1)*DEGRA
      PHSHFT(3,1) = RTABL(JSIGO,1,JSIGPS+2)*DEGRA
      RMAXSI(2) = RMAXSI(1)
      RMINSI(2) = RMINSI(1)
      Z0SNSI(2) = Z0SNSI(1)
      PHSHFT(1,2) = PHSHFT(1,1)
      PHSHFT(2,2) = PHSHFT(2,1)
      PHSHFT(3,2) = PHSHFT(3,1)
C  ... Compute derived quantities
      RADSTP =  (RMAXSI(1)- RMINSI(1))/FLOAT(NRBNSI)
      PHISTP =  TWOPI/FLOAT(NPBNSI)
C
C  ... Bank SALG
C
      JSALI = IW(NAMIND('SALG'))
      IF (JSALI.LE.0)   GO TO 998
      DO 10 IMD = 1,2
C ... Get Alignment survey  correction
         DPOSSI(1,IMD) = RTABL(JSALI,IMD,JSALDX)
         DPOSSI(2,IMD) = RTABL(JSALI,IMD,JSALDY)
         DPOSSI(3,IMD) = RTABL(JSALI,IMD,JSALDZ)
         GAPXSI(IMD)   = 0.5*RTABL(JSALI,IMD,JSALGX)
  10  CONTINUE
C
C  ... Bank STYP , SINT
C
      JSTYP = IW(NAMIND('STYP'))
      IF (JSTYP.LE.0)   GO TO 998
      JSINT = IW(NASINT)
      IF (JSINT.LE.0)   GO TO 998
      DO 11 IMD = 1,2
      DO 11 IZ = 1,12
         IROW = (IMD-1)*12 + IZ
         PLNA(5:8) = CHAINT(ITABL(JSTYP,IROW,JSTYMN+1))
         ISINUM(IZ,IMD) = 0
         DO 20 I = 1, LROWS(JSINT)
            IF(PLNA(5:8).EQ.CHAINT(ITABL(JSINT,I,JSINMN+1)))THEN
               ISINUM(IZ,IMD) = I
               GO TO 12
            ENDIF
  20     CONTINUE
  12     IF ( ISINUM(IZ,IMD).EQ.0) GO TO 998
  11  CONTINUE
C
  999 CONTINUE
      RETURN
  998 CONTINUE
      IOK  = -1
      END
