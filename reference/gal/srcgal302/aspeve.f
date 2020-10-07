      SUBROUTINE ASPEVE
C --------------------------------------------------------------------
C. - F.RANJARD - 850328                modified - 890210
C! Process current event # NEVTJO
C. - Execute each processor in turn if required :
C.     KINE, TRAC(incl. hits), ASIG, DIGI, TRIG, RDST
C. - Drop the 'T'list at the end : it contents banks which do not go
C.   onto the output.
C. - called by    ASPRUN                                  from this .HLB
C. - Calls        ASKINE, ASTRAC, ASASIG, ASDIGI          from this .HLB
C.                ASTRIG
C.                BDROP                                   from BOS77.lib
C.
C -----------------------------------------------------
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
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
C
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
      DATA MKVOL /200/
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
C - Execute modules in turn if required
C     set origin vertex volume name
      JKVOL = IW(NAKVOL)
      IF (JKVOL.EQ.0) THEN
          CALL ALBOS ('KVOL',0,LMHLEN+MKVOL*LKVOLA,JKVOL,IGARB)
          IW(JKVOL+LMHCOL) = LKVOLA
          IW(JKVOL+LMHROW) = 0
      ENDIF
C
C     event generator
      IF (IPROJO(1).NE.0) THEN
         CALL ASKINE
      ENDIF
C
C     fill origin vertex volume name if it is not done
         JKVOL = IW(NAKVOL)
         IF (LROWS(JKVOL) .EQ. 0) THEN
            KKVOL = KNEXT(JKVOL)
            DO 1 I=1,NIVXKI
            IW(KKVOL+JKVOVN) = INTCHA ('    ')
            IW(KKVOL+JKVOVM) = INTCHA ('    ')
 1          KKVOL = KKVOL + LCOLS(JKVOL)
            IW(JKVOL+LMHROW) = NIVXKI
         ENDIF
C
C     tracking + hits ===> analog signals in tracking devices
      IF (IPROJO(2).NE.0) THEN
C     transfert KINE BOS banks to KINE GEANT banks
         CALL ASTGEA
         CALL ASTRAC
      ENDIF
C
C     saturation effects in calorimeters ===> analog signals
      IF (IPROJO(3).NE.0) THEN
         CALL ASASIG
      ENDIF

C
C     digitizations in all devices ===> digits
      IF (IPROJO(4).NE.0) CALL ASDIGI
C
C     apply trigger conditions ===> trigger output
      IF (IPROJO(5).NE.0) CALL ASTRIG
C
 999  CONTINUE
      CALL BDROP (IW,'T')
      RETURN
      END
