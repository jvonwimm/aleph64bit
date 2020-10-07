      SUBROUTINE ASWSUM
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328    modified - 920131
C! Print statistic for this run
C. - Called from    ASCJOB                            from this .HLB
C. - Calls          TIMAX                             from KERNLIB
C.                  GFTITL                            from GEANT3 pam
C.                  BOSTA                             from BOS77.hlb
C.
C -----------------------------------------------------
      SAVE
C - GALEPH   30.2   950512  18:17:44
      PARAMETER (GALVER=30.2)
      PARAMETER (CORVER=0.0)
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
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C
      PARAMETER (KGWBK=69000,KGWRK=5200)
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))
C
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT
C
      CHARACTER*40 TITL(LST2)
      PARAMETER (LPAC=3)
      CHARACTER*8 TPAC(LPAC)
      CHARACTER*2 TNAM(LGEO)
      CHARACTER*4 TGET, TSAV, TJET, TLUND*7
      CHARACTER*4 CHAINT,CTABL
      CHARACTER*6 TGEN(3)
C
      DATA TGEN /'RNDM  ', 'RANECU', 'RANMAR'/
      DATA TLUND /'GUDSCBT'/
      DATA TPAC /'GHEISHA ','TATINA  ','CASCADE '/
      DATA TITL /'TOTAL NUMBER OF TRACKS PER EVENT        '
     1          ,'TOTAL NUMBER OF "PRIMARIES" PER EVENT   '
     2          ,'MAXIMUM STACK SIZE PER EVENT            '/
C
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
C - Lth character element of the NRBOSth row of the bank with index ID
      CTABL(ID,NRBOS,L) = CHAINT(ITABL(ID,NRBOS,L))
C ----------------------------------------------------------------------
C
C - get time elapsed since beginning of job
      CALL TIMAX (TIMEJO(3))
C
C - get number of triggers processed and time per trigger
      TIME = 0.
      IF(NEVTJO.GT.0) TIME = (TIMEJO(3)-TIMEJO(1)-TIMEJO(5))/NEVTJO
C
C - print title
      WRITE(LOUTIO,801) TITLJO,VERSJO,CORVER,JDATJO,JTIMJO
  801 FORMAT(1H1,//10X,A40,2X,'VERSION',F7.2,2X,'CORRECTIONS',F7.2,3X,
     &            'DATE',I8,2X,'TIME',I5//)
      WRITE (LOUTIO,'(3X,''ALEPHLIB '',T15,'':'',F7.2)') AVERJO
      WRITE (LOUTIO,'(3X,''TPCSIM   '',T15,'':'',F7.2)') TVERJO
      WRITE (LOUTIO,'(3X,''GEANT    '',T15,'':'',F7.2)') GVERSN*100.
C
      WRITE (LOUTIO,'(3X,''ADBSCONS '',T15,'':'',I5,I9/)')IDAFJO,
     &       IDCHJO
C
C - print summary of data cards used for this run
      IF(MGETJO.EQ.0) THEN
        TGET = 'none'
      ELSE
        TGET = 'all'
      ENDIF
      IF(MSAVJO.EQ.0) THEN
        TSAV = 'none'
      ELSE
        TSAV = 'all'
      ENDIF
      WRITE(LOUTIO,810) TGET, TSAV
      WRITE(LOUTIO,816) (TPROJO(J),J=1,MPROJO)
  810 FORMAT(3X,'STEERING   : READ     ',A4,4X,'SAVE    ',A4)
  816 FORMAT(3X,'             PROCESS  ',12A8)
C
      IF (MGETJO.NE.0) THEN
        WRITE (LOUTIO,811)
      ELSE
        IF (TKINJO.EQ.'USER') THEN
          WRITE (LOUTIO,806) BKINJO
        ELSE IF (TKINJO.EQ.'LUND') THEN
          WRITE(LOUTIO,812) BKINJO(4)
        ELSE IF (TKINJO.EQ.'JET') THEN
          WRITE(LOUTIO,813) (BKINJO(J),J=4,6)
        ELSE IF (TKINJO.EQ.'PART') THEN
          ITYP=NINT(BKINJO(4))
          JPART = IW(NAPART)
          WRITE(LOUTIO,814) CTABL(JPART,ITYP,2),CTABL(JPART,ITYP,3),
     &                        CTABL(JPART,ITYP,4),(BKINJO(J),J=5,8)
        ELSE IF (TKINJO.EQ.'SJET') THEN
          ITYP = INT (ABS (BKINJO(4))-500.) + 1
          TJET = TLUND(ITYP:ITYP)//'   '
          IF (BKINJO(4) .LT. 0.) TJET = TLUND(ITYP:ITYP)//'bar'
          WRITE (LOUTIO,822) TJET,(BKINJO(J),J=5,8)
        ENDIF
      ENDIF
  806 FORMAT(/3X,'KINEMATICS : USER parameters ',8F10.4)
  811 FORMAT(/3X,'KINEMATICS : was read in ')
  812 FORMAT(/3X,'KINEMATICS : LUND events (beam energy =',F6.1,' Gev)')
  813 FORMAT(/3X,'KINEMATICS : LUND single jet (beam energy ='
     1       ,F6.1,' Gev',3X,'theta =',F6.3,3X,'phi =',F6.3,')')
  814 FORMAT(/3X,'KINEMATICS : single ',3A4,' (momentum range =',2F8.3
     1       ,' Gev',3X,'cos(theta) range =',2F6.3,')'  )
  822 FORMAT (/3X,'KINEMATICS : LUND single jet ',A4,' (energy range =',
     &         2F8.3,' Gev',3X,'cos(theta) range =',2F6.3,')' )
C
      WRITE(LOUTIO,815) (TCUTJO(J),J=1,5)
  815 FORMAT(/3X,'TRACKING   : particles with energy below threshold '
     1                ,'are stopped'
     2                /16X,'electron cut          =',F8.4,' Gev'
     3                /16X,'gamma cut             =',F8.4,' Gev'
     5                /16X,'charged hadron cut    =',F8.4,' Gev'
     4                /16X,'neutral hadron cut    =',F8.4,' Gev'
     6                /16X,'muon cut              =',F8.4,' Gev'
     7                )
      CALL ALSEED (IRGEN,ISD1,ISD2)
      IF (IRGEN .EQ. 3) THEN
        WRITE (LOUTIO,817) TGEN(IRGEN),ISD1,ISD2
      ELSE
        WRITE (LOUTIO,817) TGEN(IRGEN)
      ENDIF
  817 FORMAT(/3X,'RANDOM NUMBER GENERATOR is ',A6:' with lab. seeds ',
     &        2I7)
      IHPAC = MOD (IPACJO,10)
      IF (IHPAC.GT.0 .AND. IHPAC.LE.LPAC) THEN
        WRITE (LOUTIO,818) TPAC(IHPAC)
  818   FORMAT(/3X,'HADRONIC  package is ',A8)
      ELSE
        WRITE(LOUTIO,'(''NO HADRONIC package'')')
      ENDIF
      ITPAC = MOD (IPACJO/10,10)
      IF (ITPAC.EQ.1) WRITE (LOUTIO,'(3X,''FAST tracking'')')
      IRPAC = IPACJO/100
      IF (IRPAC.EQ.1) WRITE (LOUTIO,'(3X,''RDST in use'')')
C
      WRITE(LOUTIO,819) ALFIEL
  819 FORMAT(/3X,'MAGNETIC FIELD : nominal value = ',F6.3,' Kgauss')
C
      WRITE(LOUTIO,820) ALRMAX,ALZMAX
  820 FORMAT(/3X,'GEOMETRY   : fiducial volume is RMAX = ',F8.1,' cm',
     1        2X,'and ZMAX = ',F8.1,' cm')
      M=0
      DO 2 L=1,LGEO
        IF(IGEOJO(L).GT.0) THEN
          M=M+1
          TNAM(M)=TGEOJO(L)
        ENDIF
    2 CONTINUE
      WRITE(LOUTIO,821) (TNAM(L),L=1,M)
  821 FORMAT(16X,'components present in the setup are : ',15(A2,2X))
C
C - print statistics for this run
      WRITE(LOUTIO,800) IRUNJO
  800 FORMAT(/3X,'RUN NUMBER',T25,I10)
      WRITE(LOUTIO,802) NEVTJO
  802 FORMAT(3X,'EVENTS ',T25,I10)
      WRITE(LOUTIO,803) TIMEJO(5),TIME
  803 FORMAT(3X,'TIME SPENT',T25,'before 1st event',F8.3,' sec',T50
     1         ,'per event ',F8.3,' sec')
      WRITE(LOUTIO,805) NRNDJO
  805 FORMAT(3X,'LAST RANDOM NUMBER ',T25,3I15)
C
      IF (IPROJO(2).NE.0) THEN
C - print pseudo-histograms on number of tracks / event
        DO 1 I=1,LST2
          WRITE(LOUTIO,807) TITL(I),NSTAJO(LBIN+1,I),LBIN*MBINJO(I)
     1                    ,(NSTAJO(LBIN+L,I),L=2,3)
          WRITE(LOUTIO,808) (N*MBINJO(I),N=1,LBIN),
     &                      (NSTAJO(N,I),N=1,LBIN)
    1   CONTINUE
  807   FORMAT(/3X,A40/3X,'# of events with zero track = ',I3,
     1            5X,'with more than ',I4,' tracks = ',I4,
     2            5X,'max. # of tracks = ',I5)
  808   FORMAT(20I6)
      ENDIF
C
C - print error codes
      WRITE(LOUTIO,804) (N,N=1,LERR),NERRJO
  804 FORMAT(/3X,'ERROR CODES  (definitions in ASCRUN : 804 format)'/
     &        3X,'1. - not enough space to book BOS bank (ALBOS)',
     &        T58,'2. - MUON error : array too small (MUDGTZ)'/
     &        3X,'3. - HCAL error (HCAL module)',
     &        T58,'4. - ECAL error : missing banks (ECAL module)'/
     &        3X,'5. - ITC  error : bank too small (ITDAQ)',
     &        T58,'6. - event did not terminate (ASEVEH)'/
     &        3X,'7. - VDET error',
     &        T58,'8. - not used'/
     &        3X,'9. - LCAL error : wrong data',
     &        T58,'10.- User reject at KINE process'/
     &        3X,'11.- CALO error : missing banks (CALO module)'/
     +        3X,20I5/3X,20I5)
C
C - print BOS statistics
      CALL BOSTA
C
C - print ZEBRA statistics
      CALL MZEND
C
      RETURN
      END
