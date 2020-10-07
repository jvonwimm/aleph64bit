      SUBROUTINE MINFRF
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill FRFT bank from DTRA.
C
C     Author: Stephen Haywood      03-Apr-90
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
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
      PARAMETER(JDTRCH=1,JDTRP0=2,JDTRTH=3,JDTRPH=4,JDTRD0=5,JDTRZ0=6,
     +          JDTRER=7,JDTRTF=12,JDTRHO=13,JDTRHM=14,JDTRVB=15,
     +          JDTRQF=16,JDTREA=17,JDTRVI=27,LDTRAA=27)
      INTEGER JFRFIR,JFRFTL,JFRFP0,JFRFD0,JFRFZ0,JFRFAL,JFRFEM,JFRFC2,
     +          JFRFDF,JFRFNO,LFRFTA
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER (CFACT=CLGHT/100000.)
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
      COMMON / SCALMO / ISCP93,SCPF93
C
      LOGICAL FIRST,NOEM,NEWEM
      DIMENSION CMPK(15),INDXE(5),INDXA(10)
      SAVE INDXE,INDXA,C1,HC2,UNDFL,FIRST,NOEM
      DATA INDXE / 1,  3,    6,      10,            15 /
      DATA INDXA /   2,  4,5,  7,8,9,   11,12,13,14    /
      DATA C1,HC2, UNDFL / 0.1,2500., -20. /
      DATA FIRST,NOEM / .TRUE.,.TRUE. /
      DATA EPSIL  / 0.000001 /
C
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+LMHCOL)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+LMHROW)
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
C++   Statement function used to ensure minimum entry for track errors.
C++   Zero errors must be avoided when the error matrix is inverted.
C++   Likewise, correlation coefficients of +1 or -1 give singularities.
C
      ETABL(ID,NRBOS,L) = AMAX1(FLOAT(ITABL(ID,NRBOS,L)),0.5)
      CTABL(ID,NRBOS,L) = AMIN1(AMAX1(FLOAT(ITABL(ID,NRBOS,L)),-99.5),
     &  99.5)
C
C++  Determine whether should fill error-matrix.
C
      IF(FIRST) THEN
         IF(NLINK('NOEM',0).GT.0) THEN
            NOEM = .TRUE.
         ELSE
            NOEM = .FALSE.
         ENDIF
         FIRST = .FALSE.
      ENDIF
C
C++   Determine the bank number (not from DTRA/100).
C
      KDTRA = IW(NAMIND('DTRA'))
      NR = IW(KDTRA-2)
      IF (NR.EQ.3) NR = 0
C
C++   Pick up links.
C
      KDTRA = NLINK('DTRA',100)
      IF (KDTRA.LE.0) THEN
         CALL MINUPD('DTRA')
         KDTRA = NLINK('DTRA',100)
         IF (KDTRA.LE.0) RETURN
      ENDIF
C
C++   Determine whether we have the old or new style error matrix.
C
      IF(MINGTV(DUM).GE.61) THEN
         NEWEM = .TRUE.
      ELSE
         NEWEM = .FALSE.
      ENDIF
C
C++   Create FRFT bank.
C
      NFRFT = LROWS(KDTRA)
      IF(NFRFT.LE.0) RETURN
      LEN = LMHLEN + LFRFTA * NFRFT
      CALL AUBOS('FRFT',NR,LEN, KFRFT,IGARB)
      CALL BLIST(IW,'S+','FRFT')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTRA = NLINK('DTRA',100)
      ENDIF
      IW(KFRFT+LMHCOL) = LFRFTA
      IW(KFRFT+LMHROW) = NFRFT
C
C++   Get the magnetic field - use ALEPHLIB routine.
C++   The -ve sign corresponds to the definition of 1/R.
C++   To obtain momenta, this is multiplied by speed of light.
C
      BFACT = - ALFIEL(DUMMY) * CFACT
      IF(BFACT.EQ.0.) BFACT = - 15. * CFACT
C
C++   Fill FRFT bank.
C
      DO 100 I=1,NFRFT
         P = FLOAT(ITABL(KDTRA,I,JDTRP0)) / EFACTM
         IF (ABS(P).LT.EPSIL)  P = EPSIL
         THETA = FLOAT(ITABL(KDTRA,I,JDTRTH)) / AFACTM
         SECL = 1. / SIN(THETA)
         TANL = TAN( PIBY2-THETA )
         RHO = BFACT * SECL / (FLOAT(ITABL(KDTRA,I,JDTRCH)) * P)
C Scale track curvature for 1993 MINIs written with wrong BFIELD :
         IF (ISCP93.EQ.1) RHO = RHO/SCPF93
         RW(KROW(KFRFT,I)+JFRFIR) = RHO
         RW(KROW(KFRFT,I)+JFRFTL) = TANL
         RW(KROW(KFRFT,I)+JFRFP0) = FLOAT(ITABL(KDTRA,I,JDTRPH))/AFACTM
         RW(KROW(KFRFT,I)+JFRFD0) = FLOAT(ITABL(KDTRA,I,JDTRD0))/DFACTM
         RW(KROW(KFRFT,I)+JFRFZ0) = FLOAT(ITABL(KDTRA,I,JDTRZ0))/DFACTM
C
C++      Old form of error matrix.
C
         IF(.NOT.NEWEM .AND. .NOT.NOEM) THEN
         SQR = ( ETABL(KDTRA,I,JDTRER+0)/EFACTM /P )**2
     &       - ( ETABL(KDTRA,I,JDTRER+1)/AFACTM *TANL )**2
         IF(SQR.LT.0.) SQR = 0.
         EM00 = SQR * RHO**2
         EM02 = ( SECL**2*ETABL(KDTRA,I,JDTRER+1)/AFACTM )**2
         EM05 = ( ETABL(KDTRA,I,JDTRER+2)/AFACTM )**2
         EM09 = ( ETABL(KDTRA,I,JDTRER+3)/DFACTM )**2
         EM14 = ( ETABL(KDTRA,I,JDTRER+4)/DFACTM )**2
         EM03 = CTABL(KDTRA,I,JDTREA+0)/100. * SQRT(EM00 * EM05)
         EM06 = CTABL(KDTRA,I,JDTREA+1)/100. * SQRT(EM00 * EM09)
         EM08 = CTABL(KDTRA,I,JDTREA+2)/100. * SQRT(EM05 * EM09)
         EM11 = CTABL(KDTRA,I,JDTREA+3)/100. * SQRT(EM02 * EM14)
         RW(KROW(KFRFT,I)+JFRFEM+ 0) = EM00
         RW(KROW(KFRFT,I)+JFRFEM+ 2) = EM02
         RW(KROW(KFRFT,I)+JFRFEM+ 5) = EM05
         RW(KROW(KFRFT,I)+JFRFEM+ 9) = EM09
         RW(KROW(KFRFT,I)+JFRFEM+14) = EM14
         RW(KROW(KFRFT,I)+JFRFEM+ 3) = EM03
         RW(KROW(KFRFT,I)+JFRFEM+ 6) = EM06
         RW(KROW(KFRFT,I)+JFRFEM+ 8) = EM08
         RW(KROW(KFRFT,I)+JFRFEM+11) = EM11
         ENDIF
C
C++      New form of error matrix.
C
         IF(NEWEM .AND. .NOT.NOEM) THEN
            DO J=1,5
               IEVAL = ITABL(KDTRA,I,JDTRER+J-1)
               ARG = AMIN1(AMAX1(FLOAT(IEVAL)*C1,-40.),+40.)
               CMPK(INDXE(J)) = EXP(ARG)
            ENDDO
            DO J=1,10
               IANGL = ITABL(KDTRA,I,JDTREA+J-1)
               CMPK(INDXA(J)) = (FLOAT(IANGL)-HC2) / HC2
            ENDDO
            KADDR = KROW(KFRFT,I) + JFRFEM
            CALL FUPKCM(CMPK,RW(KADDR))
         ENDIF
C
C++      Fill the chi-squared.
C++      We have the Chisq/DoF, so we have to estimate DoF.
C++      This may be wrong, but when Chisq/DoF is reobtained, it will
C++      be correct.
C++      The recipe used is to assume that TPC and Vdet contribute 2
C++      per hit to DoF, while ITC only contributes 1.
C++      There are 5 constraints, except for ITC only, when there are 3.
C++      This may differ slightly from actual DoF from Kalmen Filter.
C
         IH = ITABL(KDTRA,I,JDTRHO)
         IF (NR.EQ.2) THEN
            NMVD = MINHIT(IH,1)
         ELSE
            NMVD = 0
         ENDIF
         NITC = MINHIT(IH,2)
         NTPC = MINHIT(IH,3)
         NCONS = 5
         IF(NMVD+NTPC.EQ.0) NCONS = 3
         NDEG = 2*NMVD + NITC + 2*NTPC - NCONS
         CHIN = FLOAT(ITABL(KDTRA,I,JDTRTF)) / 10.
         RW(KROW(KFRFT,I)+JFRFC2) = FLOAT(NDEG) * CHIN
         IW(KROW(KFRFT,I)+JFRFDF) = NDEG
  100 CONTINUE
C
      RETURN
      END
