      SUBROUTINE MUSMFR
C
C***********************************************************************
C
C T.Wang -851216
C
C! deal with track segments produced by a track element
C       in a simple way :
C         (1) check if a part of the segment is cross a spacer and
C             if so then devide it into two pieces or throw it away;
C         (2) check if the length of the segment is greater than
C             SGLSMU;
C         (3) make a random decision of producing a streamer with
C             firing efficiency PSCRMU.
C
C       Output:
C          fire flag in BOS bank, in some case more track segments
C          are added
C
C       Called by MUSTRM
C       Calls ALBOS                     in this .HLB
C
C***********************************************************************
C
      SAVE
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
C
      PARAMETER (JMUSFF= 1,JMUSPL= 2,JMUSET= 3,JMUSTU= 4,JMUSXI= 5,
     +           JMUSYI= 6,JMUSZI= 7,JMUSXO= 8,JMUSYO= 9,JMUSZO=10,
     +           JMUSXY=11,JMUSXZ=12,JMUSYZ=13,LMUSTA=13)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
C! The general constants to create MU signals
      COMMON/MUGNCN/WDEIMU,WDTBMU,OFTBMU,WDATMU,HTATMU,HFHTMU,
     *              SGLSMU,PSCRMU,PTSPMU,DSSPMU,
     *              PTXSMU,PTYSMU,SNSXMU(4),SNSYMU(4)
      LOGICAL LS1,LS2
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
C       Link to BOS bank JDMUST
C
      IF (JDMUST .EQ. 0) RETURN
      IF (LROWS(JDMUST) .EQ. 0) RETURN
      NTBSG = LROWS(JDMUST)
C
C       At the return point of this routine, NSTRM includes the
C       streamers formed by new pieces of segment
C
      NSTRM = NTBSG
C
C       KOLD -- pointer to the current existing segment
C       KNEW -- pointer to the current newly-build segment
C
      KOLD = JDMUST + LMHLEN
      KNEW = KNEXT(JDMUST)
C
      DO 30 ITBSG=1,NTBSG
C
C     Get information about segment from JDMUST, notice Y2 has been
C     greater than Y1 already
C
         X1 = RW(KOLD + JMUSXI)
         Y1 = RW(KOLD + JMUSYI)
         Z1 = RW(KOLD + JMUSZI)
         X2 = RW(KOLD + JMUSXO)
         Y2 = RW(KOLD + JMUSYO)
         Z2 = RW(KOLD + JMUSZO)
         TXY = RW(KOLD + JMUSXY)
         TYZ = RW(KOLD + JMUSYZ)
C
C       Starting point is behind the N1-th spacer and finishing
C       point is behind the N2-th spacer, LS1 and LS2 indicate if
C       these two points are inside the spacers.
C
         N1 = Y1/PTSPMU
         N2 = Y2/PTSPMU
         LS1 = (Y1 - N1*PTSPMU) .GT. DSSPMU
         LS2 = (Y2 - N2*PTSPMU) .GT. DSSPMU
C
C       Y1 and Y2 at the same range
C       three cases:
C         (1) Both points inside spacer, give up;
C         (2) Y2 inside spacer, change X2,Y2 and Z2;
C         (3) Both points outside spacer, nochange.
C
         IF( N1 .EQ. N2 )THEN
            IF( LS2 )THEN
               IF( LS1 ) THEN
                  GOTO 20
               ELSE
                  Y2 = N1*PTSPMU + DSSPMU
                  X2 = X1 + (Y2-Y1)*TXY
                  Z2 = Z1 + (Y2-Y1)/TYZ
               ENDIF
            ENDIF
C
C       Check length and random decision
C
            XL = SQRT( (X1-X2)** 2+ (Y1-Y2)** 2+ (Z1-Z2)** 2)
            IF( XL .LT. SGLSMU )GOTO 20
            R = RNDM(0)
            IF( R .GT. PSCRMU )GOTO 20
            IW(KOLD + JMUSFF) = 1
            RW(KOLD + JMUSYO) = Y2
C
C       Y1 and Y2 are not in same region
C       General case -- decide YN1 and YN2 by spacer position;
C       The first piece -- if LS1, first piece is given up;
C                     otherwise, YN1 no change;
C       The last piece -- if LS1, YN2 change;
C                    otherwise, YN2 no change.
C
         ELSE
            IPLN = IW(KOLD + JMUSPL)
            IEIT = IW(KOLD + JMUSET)
            ITUB = IW(KOLD + JMUSTU)
            DO 10 N=N1,N2
               IF( N .EQ. N1 )THEN
                  IF( LS1 )GOTO 10
                  XN1 = X1
                  YN1 = Y1
                  ZN1 = Z1
               ELSE
                  YN1 = N*PTSPMU
                  XN1 = X1 + (YN1 - Y1)*TXY
                  ZN1 = Z1 + (YN1 - Y1)/TYZ
               ENDIF
               IF( N .EQ. N2 .AND. .NOT. LS2 )THEN
                  XN2 = X2
                  YN2 = Y2
                  ZN2 = Z2
               ELSE
                  YN2 = N*PTSPMU + DSSPMU
                  XN2 = X1 + (YN2 - Y1)*TXY
                  ZN2 = Z1 + (YN2 - Y1)/TYZ
               ENDIF
C
C       For pieces crossing two spacers, no need to check the length
C
               IF( (N.EQ.N1) .OR. (N.EQ.N2) )THEN
                  XL = SQRT( (XN1-XN2)** 2+ (YN1-YN2)** 2+ (ZN1-ZN2)**
     +            2)
                  IF( XL .LT. SGLSMU )GOTO 10
               ENDIF
               R = RNDM(0)
               IF( R .GT. PSCRMU )GOTO 10
               IF (LFRWRD(JDMUST) .LT. LCOLS(JDMUST)) THEN
                  NWORD = IW(JDMUST) + 16*LMUSTA
                  CALL WBANK (IW,JDMUST,NWORD,*40)
                  KOLD = KROW(JDMUST,ITBSG)
                  KNEW = KNEXT(JDMUST)
               ENDIF
               IW(JDMUST+2) = IW(JDMUST+2) + 1
               IW(KNEW + JMUSFF) = 1
               IW(KNEW + JMUSPL) = IPLN
               IW(KNEW + JMUSET) = IEIT
               IW(KNEW + JMUSTU) = ITUB
               RW(KNEW + JMUSYI) = YN1
               RW(KNEW + JMUSYO) = YN2
               KNEW = KNEW + LMUSTA
   10       CONTINUE
         ENDIF
   20    CONTINUE
         KOLD = KOLD + LMUSTA
   30 CONTINUE
C
      RETURN
C
   40 CONTINUE
      CALL ALTELL ('MUSMFR: not enough space for JDMUST ',1,'NEXT')
      RETURN
      END
