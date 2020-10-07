      SUBROUTINE ASCEVE
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328                 modified - 861009
C! Close the current event
C.   do analysis
C.   save event on tape
C.   drop BOS banks and release space
C. - called by    ASPRUN                      from this .HLB
C. - calls        USCEVE, ASEVST, ASWRTP      from this .HLB
C.                RDMOUT                      from KERNLIB
C---------------------------------------------------------------
      SAVE
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
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
      PARAMETER(JAFIAR=1,JAFIAZ=2,JAFIMF=3,JAFIBE=4,LAFIDA=4)
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
      CHARACTER*4 NAME,NLIST
      DATA IFI /0/
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
C ----------------------------------------------------------------------
C
C - Call USER routine
      CALL USCEVE
C
C - Build event statistics bank 'ASEV'
      CALL ASEVST
C
C - Update AFID beam energy with EVEH beam energy at 1st entry
      IF (IFI .EQ. 0) THEN
        IF(IW(NAMIND('RLEP')) .EQ. 0) THEN
          JAFID = IW(NAMIND('AFID'))
          IF (JAFID .GT. 0) THEN
            JEVEH = IW(NAEVEH)
            IF (JEVEH .GT. 0) THEN
              RW(JAFID+LMHLEN+JAFIBE) = REAL(IW(JEVEH+JEVETE))*1.E-6
            ENDIF
          ENDIF
        ENDIF
        IFI = 1
      ENDIF
C
C Produce friendly Fxxx DST if asked for.
      IF (FXXXJO) THEN
        CALL ASFXXX
        IF (FDEBJO .AND. IPRIJO(16).EQ.1) CALL PRFKIN
      ENDIF
C
C - Save event onto unit # LSAVIO if required
C
      IF (MSAVJO.NE.0) CALL ASWRTP ('E')
C
      IF (FDEBJO .AND. IPRIJO(14).EQ.1) CALL AUBLIS ('E')
C
C
C - Save last random number which will be the root of the next evt.
      CALL RDMOUT (NRNDJO(1))
      CALL VZERO (IRNDJO,LPRO*LRND)
C
C - Debug
C
      IF (FDEBJO .OR. KERRJO.NE.0) THEN
        CALL TIMAD (TIMEJO(6))
        TIMEJO(4) = TIMEJO(4) + TIMEJO(6)
        WRITE (LOUTIO,801) NEVTJO,TIMEJO(4),TIMEJO(6)
  801   FORMAT (/1X,'+++ASCEVE+++ event# ',I5,'  total time spent = '
     &             ,F9.3,' sec , during digitization + trigger = '
     &             ,F9.3,' sec')
        TIMEJO(4) = 0.
      ENDIF
C
      RETURN
      END
