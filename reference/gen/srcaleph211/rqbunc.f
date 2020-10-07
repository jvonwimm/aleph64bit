      SUBROUTINE RQBUNC(IBUN,INBU,NWAG,IQUA)
C---------------------------------------------------------------------
CKEY ALEF BUNCH
C - P.Comas, 28-APR-95
C! Read the bunch number to be used for reconstruction from EVEH.
C! The routine provides:
C!      1 The wagon identification
C!      2 The interbunch distance from LZZT
C!      3 The number of wagons per train from LZZT
C!      4 The quality of the wagon identification
C!
C!  Input  : EVEH, LZZT banks
C!  Output : IBUN
C!           Wagon identification in the bunch
C!              0         no measure
C!              1,2,3,4
C!
C!           INBU
C!           Interbunch distance in ns
C!
C!           NWAG
C!           Number of wagons per train
C!
C!           IQUA
C!           Quality flag of the wagon identification
C!              0         no information, bunch number is zero
C!              1         ambiguous, inconsistent
C!              2         probable
C!              3         sure, no discussion
C!
C---------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
      INTEGER JLZZDP,JLZZHP,JLZZRT,JLZZVN,JLZZFL,JLZZRU,JLZZFU,JLZZMT,
     +          JLZZMO,JLZZMS,JLZZPN,JLZZBV,JLZZTU,JLZZTM,JLZZBP,JLZZBM,
     +          JLZZBS,JLZZHT,JLZZHL,JLZZFT,JLZZEF,JLZZNM,JLZZNV,JLZZFI,
     +          JLZZGR,JLZZMM,JLZZMP,JLZZET,JLZZEH,JLZZIT,JLZZCE,JLZZCP,
     +          JLZZTE,JLZZTP,JLZZSE,JLZZSP,JLZZSC,JLZZLT,JLZZLF,JLZZLE,
     +          JLZZLP,JLZZLN,JLZZLS,JLZZLB,JLZZBT,JLZZBL,JLZZGL,JLZZGT,
     +          JLZZGA,JLZZGE,JLZZAL,JLZZAE,JLZZSA,JLZZSR,JLZZSL,JLZZSI,
     +          JLZZCT,JLZZCL,JLZZST,JLZZSS,LLZZTA
      PARAMETER(JLZZDP=1,JLZZHP=2,JLZZRT=3,JLZZVN=4,JLZZFL=5,JLZZRU=6,
     +          JLZZFU=7,JLZZMT=8,JLZZMO=9,JLZZMS=10,JLZZPN=11,
     +          JLZZBV=12,JLZZTU=13,JLZZTM=14,JLZZBP=15,JLZZBM=16,
     +          JLZZBS=17,JLZZHT=18,JLZZHL=19,JLZZFT=20,JLZZEF=21,
     +          JLZZNM=22,JLZZNV=23,JLZZFI=29,JLZZGR=30,JLZZMM=31,
     +          JLZZMP=32,JLZZET=66,JLZZEH=67,JLZZIT=131,JLZZCE=132,
     +          JLZZCP=148,JLZZTE=164,JLZZTP=168,JLZZSE=172,JLZZSP=174,
     +          JLZZSC=176,JLZZLT=177,JLZZLF=185,JLZZLE=193,JLZZLP=194,
     +          JLZZLN=195,JLZZLS=196,JLZZLB=197,JLZZBT=198,JLZZBL=199,
     +          JLZZGL=231,JLZZGT=247,JLZZGA=248,JLZZGE=249,JLZZAL=250,
     +          JLZZAE=254,JLZZSA=258,JLZZSR=259,JLZZSL=260,JLZZSI=264,
     +          JLZZCT=268,JLZZCL=269,JLZZST=281,JLZZSS=285,LLZZTA=300)
C
      LOGICAL BUNCH
      SAVE IBUNS,INBUS,NWAGS,IQUAS,NLZZT,NEVEH,BUNCH
      SAVE NWADEF,INBDEF
      DATA IEOLD,IROLD/2*0/
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
C---------------------------------------------------------------------

      IF (IROLD.EQ.0) THEN
         NLZZT = NAMIND('LZZT')
         NEVEH = NAMIND('EVEH')
C      set defaults used when LZZT bank is missing
         NWADEF = 4
         INBDEF = 247
      ENDIF
C
      CALL ABRUEV (IRUN,IEVT)
C
C - Once per run ===========================================
C
      IF (IRUN.NE.IROLD) THEN
         IROLD = IRUN
         BUNCH = .TRUE.
C
C - IF BUNCH get NWAGS number of wagons per train AND
C      INBUS interbunch distance in ns
C
C - NO bunch trains for MC and data < '95 except Nov 94 test
         IF (IRUN.LT.35000) THEN
           IF ((IRUN.NE.29989).AND.
     .         (IRUN.NE.29990).AND.
     .         (IRUN.NE.29991).AND.
     .         (IRUN.NE.30038).AND.
     .         (IRUN.NE.30042).AND.
     .         (IRUN.NE.30043).AND.
     .         (IRUN.NE.30044).AND.
     .         (IRUN.NE.30045).AND.
     .         (IRUN.NE.30046).AND.
     .         (IRUN.NE.30060).AND.
     .         (IRUN.NE.30248).AND.
     .         (IRUN.NE.30249).AND.
     .         (IRUN.NE.30250).AND.
     .         (IRUN.NE.30251).AND.
     .         (IRUN.NE.30252).AND.
     .         (IRUN.NE.30253).AND.
     .         (IRUN.NE.30254).AND.
     .         (IRUN.NE.30255).AND.
     .         (IRUN.NE.30380).AND.
     .         (IRUN.NE.30381) ) THEN
              BUNCH = .FALSE.
           ELSE
C          Run of the bunch trains test of November 1994
              INBUS = INBDEF
              NWAGS = NWADEF
           ENDIF
C
         ELSE
C
C - Bunch trains in data => '95
C   get NWAGS and INBUS from LEP info
C
            KLZZT=IW(NLZZT)
            IF (KLZZT.LE.0) THEN
               IF (IW(6).GT.0) WRITE (IW(6),*)
     &                     ' RQBUNC: Bank LZZT not available:',
     &                     ' assuming 4 bunches in a train'
                INBUS = INBDEF
                NWAGS = NWADEF
            ELSE IF (IW(KLZZT+LMHLEN+JLZZBP).NE.
     .                       IW(KLZZT+LMHLEN+JLZZBM)) THEN
                IF(IW(6).GT.0) WRITE(IW(6),*)
     &            ' RQBUNC',-2,'Electron bunches different than ',
     &            ' positron bunches: assuming 4 bunches in a train'
                INBUS = INBDEF
                NWAGS = NWADEF
            ELSE
                INBUS = NINT (IW(KLZZT+LMHLEN+JLZZBS) * 2.84)
                NWAGS = IW(KLZZT+LMHLEN+JLZZBP)
                IF (INBUS.EQ.0) INBUS = INBDEF
            ENDIF
         ENDIF
C
      ENDIF
C
C - Once per event ==================================================
C
      IF (IEVT.NE.IEOLD) THEN
         IEOLD = IEVT
         IF (BUNCH) THEN
C        bunch trains get bunch number and quality flag
            KEVEH = IW(NEVEH)
            KBUNCH = IW(KEVEH+JEVEM4)
            IF (IRUN.LT.35000) THEN
               IBUNS = IBITS (KBUNCH,0,3)
               IQUAS = 3
C        tagging of level 3 was not working at the beginning of 95
C        so set IBUN=1, IQUA=3 for runs in 4x1 configuration and
C        leave as before those in 4x2 configuration
            ELSE IF ( (IRUN.GE.35000).AND.(IRUN.LT.35718).AND.
     .         (IRUN.NE.35600).AND.
     .         (IRUN.NE.35601).AND.
     .         (IRUN.NE.35602).AND.
     .         (IRUN.NE.35603).AND.
     .         (IRUN.NE.35604).AND.
     .         (IRUN.NE.35609).AND.
     .         (IRUN.NE.35610).AND.
     .         (IRUN.NE.35611).AND.
     .         (IRUN.NE.35612).AND.
     .         (IRUN.NE.35613).AND.
     .         (IRUN.NE.35662) ) THEN
               IBUNS = 1
               IQUAS = 3
            ELSE
               IBUNS = IBITS (KBUNCH,27,3)
               IQUAS = IBITS (KBUNCH,30,2)
            ENDIF
         ELSE
C        NO bunch trains
            IBUNS = 1
            INBUS = 0
            NWAGS = 1
            IQUAS = 3
         ENDIF
      ENDIF
C
C - end ===========================================================
C
999   IBUN=IBUNS
      IQUA=IQUAS
      INBU=INBUS
      NWAG=NWAGS

      RETURN

      END
