      SUBROUTINE SIZINI
C.----------------------------------------------------------------
C  B.Bloch-Devaux   January 93
C! SCAL : Get zero suppression scheme and conversion constants
C     - Called by SIIRUN
C     - Banks SRCO , SFTH , SZTH
C
C.----------------------------------------------------------------
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
      COMMON/SICONST/ISIFCT,ISIZCT,SIMPCT,SIPGEV
      CHARACTER*4 KEY,CHAINT
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
C---------------------------------------------------------------------
C Default values for quantities in SICONST
C Zero suppression threshold on SIDI data   ( Mev       )
      ISIZCT = 20.
C Zero suppression threshold on SIFO data   ( ADC count )
      ISIFCT = 2
C Conversion factor from signal to SIFO ADC ( Mev/ADC count )
      SIMPCT = 109.53
C this value is a guess for 500 Kev tracking cut
      SIPGEV = 0.0042
      JSRCO = IW ( NAMIND('SRCO'))
      IF ( JSRCO.GT.0 ) THEN
         DO  10 I= 1,LROWS(JSRCO)
            KEY = CHAINT(ITABL(JSRCO,I,1))
            IF (KEY.EQ.'SIFO') SIMPCT = 1.E05/ITABL(JSRCO,I,2)
            IF (KEY.EQ.'PERG') SIPGEV = 10./ITABL(JSRCO,I,2)
 10      CONTINUE
      ENDIF
      JSZTH = IW ( NAMIND('SZTH'))
      IF ( JSZTH.GT.0 )  ISIZCT = IFIX(RTABL(JSZTH,1,2))
      JSFTH = IW ( NAMIND('SFTH'))
      IF ( JSFTH.GT.0 )  ISIFCT = ITABL(JSFTH,1,2)
      IF (IPRIJO(9).GT.0) WRITE(IW(6),100) ISIZCT,ISIFCT,SIMPCT
  100 FORMAT(/2X,'SICAL Analog signals and digitization constants :  '
     2    /16X,'Zero supp. threshold  for SIDI= ',I8  ,' Mev'
     3    /16X,'Zero supp. threshold  for SIFO= ',I8  ,' ADC count'
     5    /16X,'Conversion from signal to SIFO= ',F8.4,' Mev/ADC count'
     7                )
      RETURN
      END
