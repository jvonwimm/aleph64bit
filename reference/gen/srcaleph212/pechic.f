      SUBROUTINE PECHIC (IER)
C--------------------------------------------------------------------
C! Builds the relation between CAL objects and MC tracks for the ECAL
C! and the LCAL
C D. Cinabro - 890500
C
C Input:  PECO,PEST,ETDI,ESHI,PLSD,LSHI,FPOI
C
C Output: PEMH
C         IER  = 0  OK
C                -1 LCAL input bank missing
C                -2 ECAL input bank missing
C                -3 not enough space : output bank missing
C--------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPESKS=1,JPESER=2,JPESED=3,JPESET=4,JPESPE=5,LPESTA=5)
      PARAMETER(JETDTL=1,JETDS1=2,JETDS2=3,JETDS3=4,LETDIA=4)
      PARAMETER(JFPOIP=1,JFPOIS=2,LFPOIA=2)
      PARAMETER(JPLSAD=1,JPLSE1=2,JPLSE2=3,JPLSE3=4,JPLSPE=5,LPLSDA=5)
      PARAMETER(JPEMCO=1,JPEMTN=2,JPEMSE=3,LPEMHA=3)
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
      PARAMETER(JPEOER=1,JPEOE1=2,JPEOE2=3,JPEOEC=4,JPEOKD=5,JPEOR1=6,
     +          JPEOR2=7,LPEOBA=7)
      PARAMETER(JESHPT=1,JESHTI=2,JESHDE=3,LESHIA=5)
      PARAMETER(JLSHPT=1,JLSHTI=2,JLSHDE=3,LLSHIA=5)
C
      COMMON /BANK/ JWRK1,JWRK2
      DIMENSION ADC(3)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
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
      IER = 0
C
      IF (FIRST) THEN
         FIRST = .FALSE.
         JWRK1 = 0
         JWRK2 = 0
         NPECO = NAMIND('PECO')
         NPEOB = NAMIND('PEOB')
         NPEST = NAMIND('PEST')
         NETDI = NAMIND('ETDI')
         NESHI = NAMIND('ESHI')
         NFPOI = NAMIND('FPOI')
         NPLSD = NAMIND('PLSD')
         NLSHI = NAMIND('LSHI')
         CALL BKFMT('PEMH','I')
C
C      The LCAL ADC to MeV conversion
C
         ADC(1) = 32.0
         ADC(2) = 32.0
         ADC(3) = 64.2231685
      ENDIF
C
C PEST,ETDI,ESHI and FPOI must be there to continue
C
      KPEST = IW(NPEST)
      KETDI = IW(NETDI)
      KFPOI = IW(NFPOI)
      KESHI = IW(NESHI)
      IF (KESHI.LE.0.OR.KPEST.LE.0.OR.KETDI.LE.0.OR.KFPOI.LE.0) GOTO 902
      KPECO = IW(NPECO)
      KPEOB = IW(NPEOB)
C
C One of these two must be here to continue
C
      IF (KPECO.LE.0.AND.KPEOB.LE.0) GOTO 902
C
C Book PEMH with a long initial length
C
      IF (KPECO.GT.0) THEN
         NEOB = LROWS(KPECO)
      ELSEIF (KPECO.LE.0) THEN
         NEOB = LROWS(KPEOB)
      ENDIF
      NCOLS = LPEMHA
      LPEMH = NEOB*5*NCOLS + LMHLEN
      CALL AUBOS('PEMH',0,LPEMH,KPEMH,IGARB)
      IF (IGARB.EQ.2) GO TO 903
      IW(KPEMH+LMHCOL) = NCOLS
C
C Update the bank indexes
C
      KPEST = IW(NPEST)
      KETDI = IW(NETDI)
      KFPOI = IW(NFPOI)
      KESHI = IW(NESHI)
      KPECO = IW(NPECO)
      KPEOB = IW(NPEOB)
      KPLSD = IW(NPLSD)
      KLSHI = IW(NLSHI)
C
C Build the relation between ESHI and ETDI to avoid the long loop
C over ESHI.  Need two work banks. The first is parallel to ETDI and
C has the number of related ESHI rows and an offset into the second.
C The second has the list of ESHI rows related to ETDI.
C
      NPOI = LROWS(KETDI)
      LEN = NPOI*2 + LMHLEN
      CALL WBANK(IW,JWRK1,LEN,*903)
      IW(JWRK1 + LMHCOL) = 2
      IW(JWRK1 + LMHROW) = NPOI
      IW(JWRK1-3) = INTCHA ('WPOI')
C
      NRES = LROWS(KESHI)
      LEN = NRES + LMHLEN
      CALL WBANK(IW,JWRK2,LEN,*903)
      IW(JWRK2 + LMHCOL) = 1
      IW(JWRK2 + LMHROW) = NRES
      IW(JWRK2-3) = INTCHA ('WLIS')
C
C Loop on ETDI
C
      NTOT = 0
      DO 1 I = 1,NPOI
       NCON = 0
C
C Get the address
C
       IADD = ITABL(KETDI,I,JETDTL)
C
C Loop on ESHI and look for matching addresses
C
       DO 2 J = 1,NRES
        IADM = ITABL(KESHI,J,JESHTI)
        IF (IADM.NE.IADD) GOTO 2
C
C They match so fill work2 and count matches in NCON
C
        NCON = NCON + 1
        IW(KROW(JWRK2,NTOT+NCON)+1) = J
    2  CONTINUE
C
C Now fill in work2 with the number of matches and offset and update
C the offset
C
       IW(KROW(JWRK1,I)+1) = NCON
       IW(KROW(JWRK1,I)+2) = NTOT
       NTOT = NTOT + NCON
    1 CONTINUE
C
C Loop on PECO or PEOB
C
      NEST = LROWS(KPEST)
      NESH = LROWS(KESHI)
      NREF = 0
      JSAV = 0
      DO 10 I = 1,NEOB
        NMAT = 0
C
C Is this an LCAL cluster
C
       IF (KPECO.GT.0) THEN
          IRC = ITABL(KPECO,I,JPECKD)
       ELSEIF (KPECO.LE.0) THEN
          IRC = ITABL(KPEOB,I,JPEOKD)
       ENDIF
       IF (IRC.NE.192) THEN
c
C Here not an LCAL cluster.  Loop on PEST
C
        JEST = JSAV
  20    JEST = JEST + 1
         IF (JEST.GT.NEST) GOTO 9
C
C Is this story related to this object, PEST is ordered by object
C
         IREL = ITABL(KPEST,JEST,JPESPE)
         IF (IREL.NE.I) THEN
            JSAV = JEST - 1
            GOTO 9
         ENDIF
C
C It is so get the ETDI row and save the level
C
         ILEV = ITABL(KPEST,JEST,JPESKS)
         IET = ITABL(KPEST,JEST,JPESET)
         IF (IET.EQ.0) GOTO 20
C
C Loop over the related ESHI rows stored in the work banks
C
        NLP = ITABL(JWRK1,IET,1)
        IOFF = ITABL(JWRK1,IET,2)
        DO 30 K = 1,NLP
         IESH = ITABL(JWRK2,IOFF+K,1)
C
C Is there any energy at this level?
C
         IEN = NINT(FLOAT(ITABL(KESHI,IESH,JESHDE+ILEV-1))/1000.)
         IF (IEN.EQ.0) GOTO 30
C
C Check that this MC track has not already been related.
C
         IMC = ITABL(KFPOI,ITABL(KESHI,IESH,JESHPT),JFPOIP)
         IF (NMAT.NE.0) THEN
          DO 40 L = 1,NMAT
           IMCT = ITABL(KPEMH,NREF+L,JPEMTN)
           IF (IMC.EQ.IMCT) THEN
C
C This MC track has already been related to this object so simply
C accumulate the energy.
C
            IW(KROW(KPEMH,NREF+L)+JPEMSE) =
     &                       ITABL(KPEMH,NREF+L,JPEMSE) + IEN
            GOTO 30
           ENDIF
   40     CONTINUE
         ENDIF
C
C First time this MC track has been related so fill in
C the row of PEMH
C
         NMAT = NMAT + 1
         IW(KROW(KPEMH,NREF+NMAT)+JPEMCO) = I
         IW(KROW(KPEMH,NREF+NMAT)+JPEMTN) = IMC
         IW(KROW(KPEMH,NREF+NMAT)+JPEMSE) = IEN
   30    CONTINUE
        GOTO 20
C
C An LCAL cluster
C
       ELSEIF (IRC.EQ.192) THEN
C
C LSHI and PLSD had better be there
C
        IF (KLSHI.LE.0.OR.KPLSD.LE.0) GOTO 900
C
C Loop on PLSD
C
        DO 50 J = 1,LROWS(KPLSD)
C
C Is this LCAL tower related to this PECO
C
         IREL = ITABL(KPLSD,J,JPLSPE)
         IF (IREL.NE.I) GOTO 50
C
C It is related so get the address
C
         IADD = ITABL(KPLSD,J,JPLSAD)
C
C Loop on LSHI
C
         DO 60 K = 1,LROWS(KLSHI)
C
C Get the address
C
          IADM = ITABL(KLSHI,K,JLSHTI)
          IF (IADM.NE.IADD) GOTO 60
C
C Check that this MC track has not already been related.
C
          IEN = 0
          IMC = ITABL(KFPOI,ITABL(KLSHI,K,JLSHPT),JFPOIP)
          IF (NMAT.NE.0) THEN
           DO 70 L = 1,NMAT
            IMCT = ITABL(KPEMH,NREF+L,JPEMTN)
            IF (IMC.EQ.IMCT) THEN
C
C This MC track has already been related to this object so simply
C accumulate the energy.
C
             DO 71 M = 1,3
   71      IEN = NINT(ADC(M)*FLOAT(ITABL(KLSHI,K,JLSHDE+M-1)))+IEN
             IW(KROW(KPEMH,NREF+L)+JPEMSE) = IEN
             GOTO 60
            ENDIF
   70      CONTINUE
          ENDIF
C
C First time this MC track has been related so fill in
C the row of PEMH
C
          NMAT = NMAT + 1
          IW(KROW(KPEMH,NREF+NMAT)+JPEMCO) = I
          IW(KROW(KPEMH,NREF+NMAT)+JPEMTN) = IMC
          DO 72 M = 1,3
   72      IEN = IEN + NINT(ADC(M)*FLOAT(ITABL(KLSHI,K,JLSHDE+M-1)))
          IW(KROW(KPEMH,NREF+NMAT)+JPEMSE) = IEN
C
   60    CONTINUE
   50   CONTINUE
       ENDIF
C
C Keep track of how many rows in PEMH have been filled
C
    9  NREF = NREF + NMAT
   10 CONTINUE
      GOTO 901
C
C Compress PEMH, drop the work banks and handle errors properly
C
C - LCAL input bank missing
  900 IER = -1
C
  901 IW(KPEMH+LMHROW) = NREF
      GOTO 999
C
C - ECAL input  bank missing
  902 IER = -2
      GOTO 999
C
C - not enough space to book a bank
  903 IER = -3
C
C - drop and compress
  999 CONTINUE
      CALL AUBPRS('PEMH')
      CALL WDROP(IW,JWRK1)
      CALL WDROP(IW,JWRK2)
      END
