      SUBROUTINE ITRPP(IWIRS,TIMES,IRPBT,IZPBT,ISPBT,IRPHM)
C.
C...ITRPP  1.20  920213  13:22                        R.Beuselinck
C.
C!  Simulate the ITC R-phi and R-phi-Z trigger processors.
C.
C.  Calling arguments:
C.  IWIRS - Index of hit wires. (data part of work bank).      (INPUT)
C.  TIMES - Expanded time differences for 3D trigger.          (INPUT)
C.  IRPBT - 3 words containing the 72 R-phi trigger bits.     (OUTPUT)
C.  IZPBT - 3 words containing the 72 R-phi-Z trigger bits.   (OUTPUT)
C.  ISPBT - 1 word containing the 24 'special' trigger bits.  (OUTPUT)
C.  IRPHM - Array containing R-phi mask results for lev 3.    (OUTPUT)
C.
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON/ITEXPC/CLOKIT,CLGOIT,TSTRIT,TBINIT,PLENIT(8),EXPFIT(8)
C
      PARAMETER (LIHIT=8, MIHIT=500, LITWP=3)
      PARAMETER (LITFN=6, LITFP=2)
      COMMON /ITNAMC/NAIHIT,NAIDIG,NAITTR,NAIDHR,NAIXBW,
     + JDITWP,JDITFN,JDITFP,JDITNW,JDITAB,JDITDC,JDIZSC
C
      PARAMETER (LITWBK = 7)
      INTEGER JDITWB(LITWBK)
      EQUIVALENCE (JDITWB(1),JDITWP)
C
      INTEGER ITRPMN,ITZPMN,ITMSKT,ITZMSK,ITIB16,ITBINS
      COMMON/ITTRGC/ITRPMN,ITZPMN,ITMSKT(0:255),ITZMSK(0:255),
     +              ITIB16,ITBINS(2,8)
C
      COMMON/ITWIRC/RWIRIT(8),NWIRIT(8),IWIRIT(8),PHWRIT(8),CELHIT(8),
     +              CELWIT(8),WZMXIT
C
      PARAMETER(JIXBWN=1,LIXBWA=1)
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      PARAMETER (LPSCT = 48, LPSEC=6, LPMSK=LPSCT*LPSEC)
      INTEGER IWIRS(960), IRPBT(3), IZPBT(3), IBTT(8), NSEG(LPSCT)
      INTEGER NHIT(8,0:145), ITRGR(LPMSK), ITRGZ(LPMSK), NW(8)
      INTEGER IMSK(0:192), ISPBT
      INTEGER IRPHM(*)
      REAL    TMEXP(8,0:145), TIMES(3,*), TMZER(8*146)
      EQUIVALENCE (TMEXP(1,0), TMZER(1))
      CHARACTER*90 PATRN
      LOGICAL DEBTR, BTEST, THETOK, RESLT
      EXTERNAL RNDM
      SAVE NW, IBTT
      DATA NW/4*96, 4*144/
      DATA IBTT/1,2,4,8,16,32,64,128/
      PARAMETER (LP4PL=4*96, LP4PL1=LP4PL+1)
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
C--  Statement function for theta bin check.
C--
      THETOK(IB1,IB2) = (IB2.GE.ITBINS(1,IB1)) .AND.
     +                  (IB2.LE.ITBINS(2,IB1))
C
      DEBTR = FDEBJO .AND. (ICITJO(2).NE.0)
C
C--  Set random clock start time w.r.t. the time of this event.
C--
      CLGOIT = CLOKIT*RNDM(0)
C
C--  Setup wire hit array for R-PHI mask routine and the associated
C--  expanded time differences for the 3D trigger.
C--
      CALL VZERO(NHIT,8*146)
      DO 5 I=1,8*146
        TMZER(I) = 0.
    5 CONTINUE
      IWABS = 0
      NWH = 0
C
C--  Set the bad wires from IXBW to be always ON for the trigger.
C--
      IXBW = IW(NAIXBW)
      K = KROW(IXBW,1) + JIXBWN
      DO 8 I=1,LROWS(IXBW)
        IDUD = IW(K)
        IF (IDUD .LE. LP4PL) THEN
          LAY = (IDUD-1)/96 + 1
        ELSE
          LAY = (IDUD-LP4PL1)/144 + 5
        ENDIF
        NN  = IDUD - IWIRIT(LAY)
        NHIT(LAY,NN) = 1
        K = K + LIXBWA
    8 CONTINUE
C
      DO 20 I=1,8
        DO 10 J=1,NW(I)
          IWABS = IWABS + 1
          IF (IWIRS(IWABS).GT.0) THEN
            NWH = NWH + 1
            NHIT(I,J) = 1
            TMEXP(I,J) = TIMES(1,NWH)
          ENDIF
   10   CONTINUE
        NHIT(I,0)        = NHIT(I,NW(I))
        NHIT(I,NW(I)+1)  = NHIT(I,1)
        TMEXP(I,0)       = TMEXP(I,NW(I))
        TMEXP(I,NW(I)+1) = TMEXP(I,1)
   20 CONTINUE
C
      IF (DEBTR) WRITE(LOUTIO,1000) ((NHIT(I,J),J=1,96),I=1,4),
     +                              ((NHIT(I,J),J=1,144),I=5,8)
C
C--  Test the LPMSK R-phi masks.  Count the hits in each and fill
C--  ITRGR.  Simulate the 3D trigger by comparing the corresponding time
C--  differences and fill ITRGZ with the theta bin number for masks
C--  forming a satisfactory 3D trigger.
C--
      CALL ITPHIM(NHIT,TMEXP,ITRGR,ITRGZ)
C
C------------------ Evaluate the basic R-phi trigger. ------------------
C
C--  Fill the LPSCT R-phi sectors.  LPSEC masks are or-ed per sector.
C--  Within each sector two pairs of masks are combined as in the
C--  14-bit ram input: 1+2, 3, 4+5, 6.  This reduces the number of
C--  distinguishable masks down to 192.  The 192 bit results are stored
C--  for output in the IXRP bank which is used in the level 3 tracking.
C--
      CALL VZERO(NSEG,LPSCT)
      CALL VZERO(IMSK,193)
      IBT = 0
      IBW = 1
      IBR = 1
      IRPHM(IBW) = 0
      DO 40 I=1,LPSCT
        DO 30 J=1,LPSEC
          IJ = (I-1)*LPSEC + J
          IF (J.EQ.1.OR.J.EQ.4) THEN
            RESLT = ITMSKT(ITRGR(IJ))+ITMSKT(ITRGR(IJ+1)).NE.0
          ELSE IF (J.EQ.2.OR.J.EQ.5) THEN
            GO TO 30
          ELSE
            RESLT = ITMSKT(ITRGR(IJ)).EQ.1
          ENDIF
          IF (RESLT) THEN
            NSEG(I) = 1
            IRPHM(IBW) = IBSET(IRPHM(IBW),IBT)
            IMSK(IBR) = 1
          ENDIF
          IBR = IBR + 1
          IBT = IBT + 1
          IF (IBT.EQ.32) THEN
            IBT = 0
            IBW = IBW + 1
            IRPHM(IBW) = 0
          ENDIF
   30   CONTINUE
   40 CONTINUE
      IMSK(0) = IMSK(192)
C
C--  Fill the Special Trigger bits.
C--
      CALL ITRSB(NHIT,IMSK,ISPBT)
C
C--  Fill the correlation bits to provide the final trigger output.
C--
      IRPBT(1) = 0
      IRPBT(2) = 0
      IRPBT(3) = 0
      NMEM = 255
      CALL ITMSK1(NSEG, ITIB16, LBUS)
      CALL ITMSK2(LBUS, NMEM, IRPBT)
C
C--  Debug printout of trigger results.
C--
      IF (DEBTR) THEN
        WRITE(LOUTIO,1001) ITRGR
        WRITE(LOUTIO,1002) NSEG
        PATRN = '00000000  00000000  00'
        J = 1
        DO 50 I=1,18
          IF (I.EQ.9.OR.I.EQ.17) J = J + 2
          IF (BTEST(LBUS,I-1)) PATRN(J:J) = '1'
          J = J + 1
   50   CONTINUE
        WRITE(LOUTIO,1003) PATRN(1:22)
        PATRN = '  00000000  00000000  00000000  00000000  00000000'//
     +          '  00000000  00000000  00000000  00000000'
        J = 1
        K = 1
        DO 60 I=1,72
          IF (MOD(I,8).EQ.1) J = J + 2
          IF (I.EQ.33.OR.I.EQ.65) K = K + 1
          II = I - 1
          IF (I.GT.32) II = II - 32
          IF (I.GT.64) II = II - 32
          IF (BTEST(IRPBT(K),II)) PATRN(J:J) = '1'
          J = J + 1
   60   CONTINUE
        WRITE(LOUTIO,1004) PATRN
C
        IBT = 1
        IBW = 1
        J = 1
        PATRN = '00000000000000000000000000000000'//
     +          '00000000000000000000000000000000'
        WRITE(LOUTIO,1007)
        DO 65 I=1,192
          IF (BTEST(IRPHM(IBW),IBT-1)) PATRN(J:J) = '1'
          IBT = IBT + 1
          IF (IBT.GT.32) THEN
            IBT = 1
            IBW = IBW + 1
            IF (MOD(IBW,2).NE.0) THEN
            WRITE(LOUTIO,1008) PATRN(1:8)//'  '//
     +      PATRN(9:16)//'  '//PATRN(17:24)//'  '//PATRN(25:32)//
     +      '  '//PATRN(33:40)//'  '//PATRN(41:48)//'  '//PATRN(49:56)//
     +      '  '//PATRN(57:64)
            PATRN = '00000000000000000000000000000000'//
     +              '00000000000000000000000000000000'
            J = 0
            ENDIF
          ENDIF
          J = J + 1
   65   CONTINUE
C
        PATRN = '000000000000000000000000'
        DO 66 I=1,24
          IF(BTEST(ISPBT,I-1)) PATRN(I:I) = '1'
   66   CONTINUE
        WRITE(LOUTIO,1009) PATRN(1:24)
      ENDIF
C
C------------------- Evaluate the R-phi-Z trigger. ---------------------
C
      IF (DEBTR) WRITE(LOUTIO,1006) ITRGZ
C
      IZPBT(1) = 0
      IZPBT(2) = 0
      IZPBT(3) = 0
C
C--  Loop over the 8 calorimeter theta bins.
C--
      DO 90 ITHET=1,8
        CALL VZERO(NSEG,LPSCT)
        DO 80 I=1,LPSCT
          DO 70 J=1,LPSEC
            IJ = (I-1)*LPSEC + J
            IF (THETOK(ITHET,ITRGZ(IJ))) NSEG(I) = 1
   70     CONTINUE
   80   CONTINUE
C
C--  Fire the appropriate correlation bits for the current theta bin.
C--  This is controlled by setting the appropriate mask bit in the
C--  8 bit pattern cross-matched with the 18 bus-line bits.
C--
        NMEM = IBTT(ITHET)
        CALL ITMSK1(NSEG, ITIB16, LBUS)
        CALL ITMSK2(LBUS, NMEM, IZPBT)
   90 CONTINUE
C
C--  Debug printout for the R-phi-Z trigger final result.
C--
      IF (DEBTR) THEN
        PATRN = '  00000000  00000000  00000000  00000000  00000000'//
     +          '  00000000  00000000  00000000  00000000'
        J = 1
        K = 1
        DO 100 I=1,72
          IF (MOD(I,8).EQ.1) J = J + 2
          IF (I.EQ.33.OR.I.EQ.65) K = K + 1
          II = I - 1
          IF (I.GT.32) II = II - 32
          IF (I.GT.64) II = II - 32
          IF (BTEST(IZPBT(K),II)) PATRN(J:J) = '1'
          J = J + 1
  100   CONTINUE
        WRITE(LOUTIO,1005) PATRN
      ENDIF
 1000 FORMAT('0Wire hit flags (NHIT):'/4(/12(2X,8I1))/8(/9(2X,8I1)))
 1001 FORMAT('0R-Phi Trigger mask results (8-bit address):'/(30I4))
 1002 FORMAT('0ITC 48-phi sectors (NSEG):',6(2X,8I1))
 1003 FORMAT('0ITC 18 bus-line bits  = ',2X,A)
 1004 FORMAT('0Final 72 R-phi   trigger bits = ',A)
 1005 FORMAT('0Final 72 R-phi-Z trigger bits = ',A)
 1006 FORMAT('0R-Phi-Z Trigger mask results (theta bin no.):'/(30I4))
 1007 FORMAT('0192 ITC R-phi mask results (IXRP bank):')
 1008 FORMAT(1X,A)
 1009 FORMAT('0Final 24 Special trigger bits = ',A)
      END
