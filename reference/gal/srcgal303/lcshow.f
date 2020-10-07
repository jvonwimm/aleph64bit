      SUBROUTINE LCSHOW
C--------------------------------------------------------------
C! Shower parametrisation in LCal
C. - J.Dines Hansen & P.Hansen - 860417
C.                               modified by F.Ranjard - 890317
C.                               modified by P.Hansen -  940701
C. - Sets GEANT stop flag
C.   ********************New for 94*****************************
C. - Use longitudinal parametrization:
C.     dN/dS   = (ETOT/ECRTLC)*B*(B*S)**(A-1)*exp(-B*S)/gamma(A)
C. - The P-terms are from fit to Lcal data,
C. -  the DP-terms from Ecal test beam fit:
C.      A/B    = PAR1LC + DPR1LC*alog(E)
C.      1/B    = PAR2LC + DPR2LC*alog(E)    for E>0.1GeV
C.      A/B    = PAR3LC + DPR3LC*alog(E)
C.      1/B    = PAR4LC + DPR4LC*alog(E)    for E<0.1GeV
C. - Use lateral parametrization:           R in cm
C.     d(dN/dS)/dR = (dN/dS)*exp(-R/R0)/R0
C.         R0      = PAR5LC + DPR5LC*S*B/A
C. - PAR1LC is fluctuated by the relative amount PAR6LC
C. - PAR2LC is fluctuated by the relative amount PAR8LC
C. - R0     is fluctuated by the relative amount PAR7LC
C.**************************************************************
C.
C. - Called by  LCHIT                            from this .HLB
C. - Calls      LCSTRT, LCFRAL, LCXYPA, CAHIST   from this .HLB
C.              RNDM                             from   CERNLIB
C.              POISSN                           from    GENSEC
C -----------------------------------------------
      SAVE
      DIMENSION XIN(3),DXIN(3),XOUT(3)
      DIMENSION XYZ(3),XYAV(3)
      EXTERNAL RNDM
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /LCCOMC/ ADCOLC,    COHNLC,    DPR1LC,    DPR2LC,
     *                DPR3LC,    DPR4LC,    DPR5LC,    ECRTLC,
     *                ECUTLC,    EELALC,    GVARLC,    LCADCO,
     *                LCBHTR,    LCHBOK,    LCNLAY(3), LCNWPL,
     *                LCMATE(2), LCPRNT,    LCSTRH(3), CHTOE(3),
     *                PAR1LC,    PAR2LC,    PAR3LC,    PAR4LC,
     *                PAR5LC,    PAR6LC,    PAR7LC,    PAR8LC,
     *                RADLLC(2), SNOILC(3), SCONLC,    SSAMLC,
     *                SSTPLC(3), TNOILC(3), WNOILC(3),
     *                ZMATLC(2), ZREFLC(3), ZSTPLC(3), Z123LC(3),
     *                XYZOLC(3,2),DWIRLC
      COMMON /LCNAMC/ NALTHT, NALTDI, NALWHT, NALWDI, NALTTR, NALWTR,
     *                NALWHI, NALSHI, NALCWS, NALCAL, NALLAY, NALMTY,
     *                NALALI, NALSCO, NALSLO, NALWRG, NALCEL, NALCSH,
     *                NALDRE, NALCCA, NALCPG, NALSHO
C - user stop particle flag
      PARAMETER (NOMOR=3)
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      LOGICAL FTINO, FMUON, FELEC, FHADC
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
C -------------------------------------------------------------
C - Stop GEANT tracking for electron/positron
      ITRKEL(9)=NOMOR
C
C - Electron enters the volume or is created in the volume
      IF (ITRKEL(8).NE.1)                        GOTO 999
C
C - Transform trackelement into local system
      CALL LCFRAL(IFB,MODU,XIN,XOUT,DXIN)
C
C - Total energy of track element
      ETOT = TRKELE(8)
C
C - Very small energies are absorbed before entering the gas
      IF(ETOT.LT.ECRTLC)                         GOTO 999
C
C - Add the track energy to run summary bank
      KWS  = IW(NALCWS)
      IF(KWS.NE.0) THEN
         IW(KWS+LMHLEN+4+IFB) = IW(KWS+LMHLEN+4+IFB)+INT(ETOT*1000.)
      ENDIF
C
C - Find storey and layer no. where shower starts
      IF(MODU.LE.2) THEN
        ZABS = -XIN(3)
      ELSE
        ZABS = XIN(3)
      ENDIF
      CALL LCSTRT(ZABS,ISTMN,LAYER,LMIN,ZWIRE)
C
C - Move starting point to preceeding wire-plane
      LAYER = LAYER - 1
      DL = (ZWIRE-ZABS)/ABS(DXIN(3))
      XIN(1) = XIN(1)+DXIN(1)*DL
      XIN(2) = XIN(2)+DXIN(2)*DL
      XIN(3) = XIN(3)+DXIN(3)*DL
C
C - Compute longidutinal shower parameters
      IF(ETOT.GT.0.1) THEN
        E1  = PAR1LC + DPR1LC*ALOG(ETOT)
        E2  = PAR2LC + DPR2LC*ALOG(ETOT)
      ELSE
        EN  = AMAX1(ETOT,0.03)
        E1  = PAR3LC + DPR3LC*ALOG(EN)
        E2  = PAR4LC + DPR4LC*ALOG(EN)
      ENDIF
c
c fluctuate A/B and 1/B
      CALL RANNOR(RN1,RN2)
      IF(ABS(RN1).LT.4.) E1 = E1*(1.+PAR6LC*RN1)
      CALL RANNOR(RN3,RN4)
      IF(ABS(RN3).LT.4.) E2 = E2*(1.+PAR8LC*RN3)
      A  = E1/E2
      B  = 1./E2
C
C - Compute normalization
      ZFIR = AMAX1(ZABS,0.)-
     &       FLOAT(INT(AMAX1(ZABS,0.)/ZSTPLC(1)))*ZSTPLC(1)
      SNORM = SSTPLC(2)*ETOT*(B**A)/(ECRTLC*GAMMA(A))
      DL    = 0.
      S     = -ZFIR*SSTPLC(1)/ZSTPLC(1)
      KLDWP = IW(NAMIND('LDWP'))
      KLDST = IW(NAMIND('LDST'))
C
C - Step through storeyes
      DO 300 ISTOR = ISTMN,3
         LMAX  = LCNLAY(ISTOR)
         SSTEP = SSTPLC(ISTOR)
         STEPL = ZSTPLC(ISTOR)/ABS(DXIN(3))
C
C - Step through layers
       DO 400 LAY = LMIN,LMAX
         LAYER = LAYER + 1
         S     = S  + SSTEP
         DL    = DL + STEPL
C - Number of hits in the layer
         AVHIT = SNORM*S**(A-1.)*EXP(-B*S)
         CALL POISSN(AVHIT,NHIT,IER)
         IF (IER .NE. 0)                         GOTO 998
         IF (NHIT.EQ. 0)                         GOTO 400
         NWHIT = 0
C - Coordinates of shower centroid
         XYAV(1) = XIN(1) + DL*DXIN(1)
         XYAV(2) = XIN(2) + DL*DXIN(2)
         XYAV(3) = XIN(3) + DL*DXIN(3)
C - Compute radial parameter
         R0    = PAR5LC + DPR5LC*S*B/A
C - Same fluctuation in all planes
         R0    = R0*(1.+RN2*PAR7LC)
C - Loop over hits
         DO 500 IHIT = 1,NHIT
C - Choose x,y coordinates of hit
            R    = -R0*ALOG(RNDM(DUMMY))
            PHI  = RNDM(DUMMY)*TWOPI
            XYZ(1) = XYAV(1) + R*COS(PHI)
            XYZ(2) = XYAV(2) + R*SIN(PHI)
            XYZ(3) = XYAV(3)
C - Find tower address of hit
            CALL LCXYPA(MODU,LAYER,XYZ,IPAD)
C - If inside the acceptance
            IF(IPAD.EQ.0)                        GOTO 500
C - Update number of hits in this plane (if alive)
            IFALI = 1
            IF(KLDWP.GT.0) THEN
              DO 510 I=1,LROWS(KLDWP)
                IF(ITABL(KLDWP,I,4).EQ.(MODU-1)*64+LAYER) THEN
                  IFALI = 0
                  GOTO 500
                ENDIF
  510         CONTINUE
            ENDIF
            CALL POISSN(16.,N16,IER)
            NWHIT = NWHIT + N16
            CALL CAHIST(NALSHI,IPAD,ISTOR,N16)
  500    CONTINUE
C - Add the hits to the wire-plane history bank
         IF (NWHIT .GT. 0) CALL CAHIST(NALWHI,MODU,LAYER,NWHIT)
  400 CONTINUE
      LMIN = 1
  300 CONTINUE
      GOTO 999
  998 WRITE(LOUTIO,9999) IER
  999 RETURN
 9999 FORMAT(' +++ LCSHOW +++ error in call to POISSN',I10)
      END
