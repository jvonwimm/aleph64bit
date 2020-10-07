      SUBROUTINE ITRGPA
C.
C...ITRGPA  1.10  920213  10:33                        R.Beuselinck.
C.
C!  Set up the ITC trigger 256 bit address scheme for the allowed
C.  hit combinations.
C.
C.  Each of the addresses (0 to 255) corresponds to an 8 bit pattern
C.  representing the layer which received a hit for the trigger mask
C.  being considered.  The addresses point to an array which contains
C.  a 1 for a valid layer combination or a 0 for an invalid one.
C.
C.  The aim of this routine is to setup the 256 zero or one values
C.  according to the minimum trigger criteria:
C.    1) a minimum number of hit layers is required. (ITRPMN,ITZPMN)
C.    2) a minimum number of hits in layers 1-4
C.    3) a minimum number of hits in layers 5-8
C.
C-----------------------------------------------------------------------
      SAVE
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON /ITSPEC/ITB2BS,ITHCLU,ITTCLU,ITRTHR,ITHTHR(8),ITBTHR(8)
      INTEGER ITB2BS,ITRTHR,ITHTHR,ITBTHR
      LOGICAL ITHCLU,ITTCLU
C
      INTEGER ITRPMN,ITZPMN,ITMSKT,ITZMSK,ITIB16,ITBINS
      COMMON/ITTRGC/ITRPMN,ITZPMN,ITMSKT(0:255),ITZMSK(0:255),
     +              ITIB16,ITBINS(2,8)
C
      PARAMETER(JIXCIH=1,JIXCOH=2,JIXCMH=3,JIXCTT=4,JIXCBB=5,JIXCTC=6,
     +          JIXCHC=7,JIXCPI=8,JIXCHT=9,JIXCBV=17,JIXCFM=25,
     +          LIXCRA=25)
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      INTEGER MSKT(0:255), IND, NAMIND, IB16, IBSET
      INTEGER I, J, NB
      INTEGER SIDEF(0:15), TBINS(2,8)
      EQUIVALENCE (MSKT(0),ITMSKT(0)), (ITIB16,IB16)
      LOGICAL BTEST, DEBTR
      EXTERNAL NAMIND
      DATA TBINS/1, 80, 53, 98, 85, 118, 104, 133,
     +     123, 152, 138, 171, 158, 203, 176, 256/
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
      DEBTR = ICITJO(2).NE.0
C
C--  Clear the valid address list.
C--
      CALL VZERO(ITMSKT,256)
      CALL VZERO(ITZMSK,256)
C
C--  Obtain the trigger conditions from the IXCR bank.
C--
      IND   = IW(NAMIND('IXCR'))
      NIXCR = IW(IND-2)
      JROW  = KROW(IND, 1)
C
C--  Set minimum no. of layers hit in mask for: layers 1-4, 5-8, 1-8.
C--
      NORL1  = IW(JROW+JIXCIH)
      NORL2  = IW(JROW+JIXCOH)
      ITRPMN = IW(JROW+JIXCMH)
      ITZPMN = ITRPMN
C
C--  Set Track threshold, Back-to-back span, Track clust., Hit clust.
C--
      ITRTHR = IW(JROW+JIXCTT)
      ITB2BS = IW(JROW+JIXCBB)
      ITTCLU = IW(JROW+JIXCTC).EQ.1
      ITHCLU = IW(JROW+JIXCHC).EQ.1
C
C--  Set Trigger processor used, Hit/Background thresholds.
C--
      IF (IW(JROW+JIXCPI).EQ.0) THEN
        ICITJO(3) = 1
      ELSE
        ICITJO(3) = 0
      ENDIF
      CALL UCOPY(IW(JROW+JIXCHT),ITHTHR,8)
      CALL UCOPY(IW(JROW+JIXCBV),ITBTHR,8)
C
C--  Set fanout mode for correlation with calorimeters.
C--  The only currently implemented fanout mode has all 16 side-bits
C--  switched on.
C--
      MODE = IW(JROW+JIXCFM)
      IF (MODE.NE.1)
     +  CALL ALTELL('ITRGPA: CTS fanout mode in IXCR not known',
     +  0,'STOP')
      IB16 = 0
      IF (MODE.EQ.1) THEN
        DO 10 I=0,15
          IB16 = IBSET(IB16,I)
          SIDEF(I) = 1
   10   CONTINUE
      ENDIF
C
C--  For old MC: 1989-1991 side-bits 3 & 12 are off.
C--
      IF (NIXCR.EQ.1) THEN
        SIDEF(3)  = 0
        SIDEF(12) = 0
        IB16 = IBCLR(IB16,3)
        IB16 = IBCLR(IB16,12)
      ENDIF
C
C--  Now compute the allowed bit patterns for each mask.
C--
      DO 30 I=0,255
        NB = 0
        NB1 = 0
        NB2 = 0
        DO 20 J=0,7
          IF (BTEST(I,J)) THEN
            NB = NB + 1
            IF (J.LT.4) THEN
              NB1 = NB1 + 1
            ELSE
              NB2 = NB2 + 1
            ENDIF
          ENDIF
   20   CONTINUE
        IF (NB.GE.ITRPMN) ITMSKT(I) = 1
        IF (NB.GE.ITZPMN) ITZMSK(I) = 1
        IF (NB1.LT.NORL1.OR.NB2.LT.NORL2) THEN
          ITMSKT(I) = 0
          ITZMSK(I) = 0
        ENDIF
   30 CONTINUE
C
C--  Get the lower and upper ITC theta bin numbers to match to each
C--  Calorimeter theta bin. (Used for SSP - which doesn't exist yet).
C--
      CALL UCOPY(TBINS,ITBINS,16)
C
  999 IF (DEBTR) THEN
        WRITE(LOUTIO,1000) NIXCR,ITRPMN,NORL1,NORL2,SIDEF
        IF (ICITJO(3).EQ.1) WRITE(LOUTIO,1001) ITMSKT
        IF (ICITJO(3).EQ.0) WRITE(LOUTIO,1002) ITZMSK
        WRITE(LOUTIO,1003) 2*ITB2BS+1,ITHCLU,ITTCLU,ITRTHR,ITHTHR,ITBTHR
      ENDIF
 1000 FORMAT(//30X,' ITC Trigger Parameters:  from IXCR',I3//
     + ' Minimum number of hit layers = ',I2,5X,
     + ' Minimum number of hits in layers 1-4 and 5-8  = ',I2,I4//
     + ' Software-loaded bits = ',4(2X,4I1))
 1001 FORMAT(' Trigger processor in use is R-phi Processor'//
     +' Valid R-phi trigger mask lookup memory:'/(8(2X,8I1)))
 1002 FORMAT(' Trigger processor in use is Space Point Processor'//
     +' Valid R-phi-Z trigger mask lookup memory:'/(8(2X,8I1)))
 1003 FORMAT(//' ITC Special Trigger Parameters:'/
     +' Back-to-back Trigger :  1 against ',I1/
     +'       Hit clustering : ',L2/
     +'     Track clustering : ',L2/
     +'      Track threshold : > ',I2/
     +'        Hit threshold : > ',I2,7I3/
     +' Background threshold : > ',I2,7I3)
      END
