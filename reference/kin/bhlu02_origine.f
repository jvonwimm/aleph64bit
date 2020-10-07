
C-----------------------------------------------------------------------
C  A  L  E  P  H   I  N  S  T  A  L  L  A  T  I  O  N    N  O  T  E  S |
C                                                                      |
C    original code : BHLUMI from S.Jadach et al.                       |
C    trasmitted by : B.Pietrzyk , January 1992                         |
C    modifications to the code ( description,author,date)              |
C                                                                      |
C    W A R N I N G  ! ! ! !  COMPILE ON VAX WITH OPTION /G_FLOAT       |
C                                                                      |
C       B. Bloch January 1992                                          |
C    1. Change random number generator : interface VARRAN to RNDM in   |
C       case RANMAR is selected .As we use RNDM as a server of RANMAR  |
C       from the KIN disk, it is equivalent and cleaner!               |
C    2. Change names of routines TRIGAS0 and TRIGAS1 to TRIGA0(1)      |
C    3. Comment out the whole routine MARRAN which has entry points    |
C       RMARIN and RMARUT which may bring confusion with ours          |
C    4. Unplug the hardwired values of NINP,NOUT,NOUT2 in BHLUMI and   |
C       use the values from the COMMON which are initialised in a      |
C       coherent way in the interface...                               |
C    5. In BHLUM2  book and fill histos with single precision arguments|
C       If filled with double prec args , the behaviour on Vax is crazy|
C       B. Bloch September 1992                                        |
C    6. Introduce definition of Maximum weight used in BHLUM2 through  |
C       XPAR( 7) which is still free...                                |
C       As a consequence , subroutines BHLDE1 and BHLDE2 are modified  |
C       to give an input value (2.8D0) to XPAR(7).                     |
C-----------------------------------------------------------------------
      PROGRAM MAIN
C     ************
C
C   BBBBBBB    BBB   BBB  BBB      BBB  BBB  BBB     BBB   BBB
C   BBB  BBB   BBB   BBB  BBB      BBB  BBB  BBBB   BBBB   BBB
C   BBB  BBB   BBB   BBB  BBB      BBB  BBB  BBBBB BBBBB   BBB
C   BBBBBB     BBBBBBBBB  BBB      BBB  BBB  BBB BBB BBB   BBB
C   BBBBBBBBB  BBBBBBBBB  BBB      BBB  BBB  BBB  B  BBB   BBB
C   BBB  BBBB  BBB   BBB  BBB  BB  BBB  BBB  BBB     BBB   BBB
C   BBBBBBBBB  BBB   BBB  BBBBBBB  BBB  BBB  BBB     BBB   BBB
C   BBBBBBBB   BBB   BBB  BBBBBBB   BBBBBB   BBB     BBB   BBB
C
C
C======================================================================
C======================================================================
C================             B H L U M I            ==================
C======================================================================
C================ MONTE CARLO FOR SMALL ANGLE BHABHA ==================
C================            VERSION 2.01            ==================
C=====================      SEPTEMBER 1991     ========================
C======================================================================
C======================================================================
C========================     AUTHORS      ============================
C=====   S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was   =========
C======================================================================
C======================================================================
C
C The complete description of the usage of the program can be found
C in the Long-write-up of BHLUMI 2.01, CERN preprint TH-6xxx,
C (ref. [1]) to be submitted to Computer Physics Communications.
C A lot of usefull information may be found in comments in the source
C code, see routines BHLUM2, LUMLOG, OLDBIS.
C
C This deck contains two demostration programs:
C   (0)  BHLDE1, BHLDE2
C which start below and the M.C. generator BHLUMI which includes
C three independend subgererators:
C   (1)  BHLUM2
C    Multiphoton generator with Yennie-Frautschi-Suura first
C    order exponentiation based on refs. [1,2,3]
C   (2)  LUMLOG
C    Leading-Logarithmic generator with collinear emission of photons,
C    QED matrix element up to third order described in ref. [4]
C   (3)  OLDBIS
C    Modified version of the first order clasical generator OLDBAB,
C    see refs. [5] and [6]
C
C The program is complete and does not require any external libraries.
C
C                  IMPORTANT NOTE
C                  --------------
C The user is kindly requested to cite at least refs. [1-3]
C and any other ones from the above list if his study depends strongly
C on the particular subgenerator.
C
C [1] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
C     TH-6230, sept. 1991
C [2] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
C     TH-6118, June 1991, Phys. Lett. in print.
C [3] S. Jadach and B.F.L. Ward,
C     Phys. Rev. D40 (1989) 3582.
C [4] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was
C     Phys. Lett. B260 (1991) 173, TH-5995.
C [5] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was
C     Phys. Lett. B253 (1991) 469, TH-5888.
C [6] F. Berends and R. Kleiss Nucl. Phys. B228 (1983) 537.
C
C------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C histograms in blank common
C=====((((
      COMMON / PAWC / HISTO(20000)
C     COMMON /      / HISTO(20000)
C=====))))
C Input/output files
      COMMON / INOUT  / NINP,NOUT,NOUT2
C Initialization of histograming package --
C here we use double precision BHBOOK-like histograming/plotting
C package GLIBD written by S. Jadach, (1990) unpublished.
C In the C.P.C. version all references to GLIBD commented out.
C See strings ===(((  ===))) marking these parts of code.
C
C=====((((
      CALL HLIMIT(20000)
      CALL HOUTPU(16)
C=====))))
      NINP =15
      NOUT =16
      NOUT2=16
CCC   OPEN(NOUT)
CCC   OPEN(NINP,STATUS='OLD')
C     ***********
C Simplistic demo
      CALL BHLDE1
C Full scale demo: loop over three subgenerators
      DO 10 IGEN=1,3
      CALL BHLDE2
   10 CONTINUE
C     ***********
      END
 
      SUBROUTINE  BHLDE1
C     ******************
C Simplistic demo for beginner
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      DOUBLE PRECISION  P1,P2,Q1,Q2,PHOT,XPAR(100)
      DOUBLE PRECISION  THETP,THETQ
      INTEGER NPAR(100)
 
      NOUT=16
      WRITE(NOUT,*) '   '
      WRITE(NOUT,*) '=============================================='
      WRITE(NOUT,*) '==========*********************==============='
      WRITE(NOUT,*) '==========***  B H L D E 1  ***==============='
      WRITE(NOUT,*) '==========*********************==============='
      WRITE(NOUT,*) '=============================================='
      WRITE(NOUT,*) '   '
C book histograms
C=====((((
      CALL HBOOK1(100,'THETA DISTRIBUTION (DEGREES) $',60,0D0,12D0)
C=====))))
      NPAR(1)= 3001
      NPAR(2)=    1
      XPAR(1)=   92D0
      XPAR(2)=   1.9D0
      XPAR(3)=   64.D0
      XPAR(4)=   1D-4
      XPAR(7)= 2.8D0
      CALL BHLUMI(-1,XPAR,NPAR)
      DO 500 IEVENT = 1,10000
      CALL BHLUMI( 0,XPAR,NPAR)
C printing four-momenta of first events
      IF(IEVENT.LE.5) CALL DUMPS(16)
C filling histograms
      THETP= DATAN(ABS(DSQRT(P2(1)**2+P2(2)**2)/P2(3)))
      THETQ= DATAN(ABS(DSQRT(Q2(1)**2+Q2(2)**2)/Q2(3)))
      THETP= THETP*180D0/3.1415926535897932D0
      THETQ= THETQ*180D0/3.1415926535897932D0
C=====((((
      CALL HF1(100,THETP,1D0)
      CALL HF1(100,THETQ,1D0)
C=====))))
  500 CONTINUE
      CALL BHLUMI( 2,XPAR,NPAR)
C Printing histograms
C N.B. Model weight distribution ID=9001 is filled in BHLUM2
C=====((((  ====>>>>
      CALL HPRINT(100)
      CALL HPRINT(9001)
C=====))))
      END
 
      SUBROUTINE BHLDE2
C     *****************
C Tests/calculations specific to TH-6118 paper
C ------------------------------------------------
C                BOKER1
C This for any type of generator BHLUM2, OLDBIS, LUMLOG.
C Using these results, in the later analysis,
C the inter-generator differences are calculated.
C Pure t-channel and no vacuum polarization !!!
C Calculated: integrated x-sections, litle discrete set for one
C energy-cut PLUS correspondig histograms on energy-cut dependence
C                BOKER2
C Exclusively OLDBIS related tests. No histograms!
C Calculated: effects due to up-down interference for realistic trigger
C                BOKER3
C Tests related exclusively to LUMLOG
C Histograming plus discrete set of cross-sections.
C Calculated: LL cross sections in various perturbative orders
C (with and without exponentiation) and their differences
C depending on energy cut. Light fermion pair contribution.
C                BOKER4
C BHLUM2 related tests:
C Histograms + discrete set.
C Calculated:
C (a) Total QED correction, depending on z_min
C (b) Subleading correction
C (c) Test on phase space completness specific for BHLUMI generator
C -----------------------------------------------
C     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0 )
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI
      DIMENSION NPAR(100),XPAR(100)
      CHARACTER*80 TESTIT
      LOGICAL KONIEC
C
      WRITE(NOUT,*) '   '
      WRITE(NOUT,*) '   '
      WRITE(NOUT,*) '=============================================='
      WRITE(NOUT,*) '==========*********************==============='
      WRITE(NOUT,*) '==========***  B H L D E 2  ***==============='
      WRITE(NOUT,*) '==========*********************==============='
      WRITE(NOUT,*) '=============================================='
      WRITE(NOUT,*) '   '
C======================================
 3000 FORMAT(A80)
 3001 FORMAT(8I2)
 3002 FORMAT(I10)
 3003 FORMAT(F10.0)
      READ( NINP,3000) TESTIT
      WRITE(NOUT,3000) TESTIT
      READ( NINP,3001) KAT1,KAT2,KAT3,KAT4,KAT5,KAT6
      READ( NINP,3002) NEVT,KEYOPT,KEYRAD,KEYTRI
      READ( NINP,3003) CMSENE,TMING,RAXIG,VMAXG,XK0
      READ( NINP,3003) TMINW,RAXIW,TMINN,RAXIN,VMAXE
C======================================
C control output
      WRITE(NOUT,'(6A6/6I6)')
     $ 'KAT1','KAT2','KAT3','KAT4','KAT5','KAT6',
     $  KAT1 , KAT2 , KAT3 , KAT4 , KAT5 , KAT6
      WRITE(NOUT,'(4A12/4I12)')
     $  'NEVT','KEYRAD','KEYOPT','KEYTRI',
     $   NEVT,  KEYRAD , KEYOPT , KEYTRI
      WRITE(NOUT,'(5A12/5F12.6)')
     $ 'CMSENE','TMING','RAXIG','VMAXG','XK0',
     $  CMSENE , TMING , RAXIG , VMAXG , XK0
      WRITE(NOUT,'(5A12/5F12.6)')
     $  'TMINW','RAXIW','TMINN','RAXIN','VMAXE',
     $   TMINW , RAXIW , TMINN , RAXIN , VMAXE
      TMAXG  = ACOS(1-(1-COS(TMING*PI/180))*RAXIG)*180/PI
      TMAXW  = ACOS(1-(1-COS(TMINW*PI/180))*RAXIW)*180/PI
      TMAXN  = ACOS(1-(1-COS(TMINN*PI/180))*RAXIN)*180/PI
      WRITE(NOUT,'(6A12/6F12.6)')
     $  'TMING','TMAXG','TMINW','TMAXW','TMINN','TMAXN',
     $   TMING , TMAXG , TMINW , TMAXW , TMINN , TMAXN
C======================================
 
C Leading-log limiting values for the transfer
      TRMINL =CMSENE**2*VMAXE*(1D0-COS(TMING*PI/180))/2D0
      TRMAXL =CMSENE**2*      (1D0-COS(TMAXG*PI/180))/2D0
      TRMIN = 0.80D0*TRMINL
      TRMAX = 2.00D0*TRMAXL
      EPSCM = XK0
 
      KEYGEN = MOD(KEYOPT,10000)/1000
      IF(KEYGEN.EQ.3) THEN
C input data for --- BHLUM2 ---
       NPAR(1)=KEYOPT
       NPAR(2)=KEYRAD
       XPAR(1)=CMSENE
       XPAR(2)=TRMIN
       XPAR(3)=TRMAX
       XPAR(4)=EPSCM
      ELSEIF(KEYGEN.EQ.2) THEN
C input data for --- LUMLOG ---
       NPAR(1) = KEYOPT
       NPAR(2) = KEYRAD
       XPAR(1) = CMSENE
       XPAR(2) = TMING
       XPAR(3) = TMAXG
       XPAR(4) = XK0
       XPAR(5) = VMAXG
      ELSEIF(KEYGEN.EQ.1) THEN
C input data for --- OLDBIS ---
       NPAR(1)= KEYOPT
       NPAR(2)= KEYRAD
       XPAR(1)= CMSENE
       XPAR(2)= TMING
       XPAR(3)= TMAXG
       XPAR(4)= XK0
       XPAR(5)= VMAXG
       XPAR(6)= 0D0
       XPAR(7)= 2.8D0
      ELSE
       WRITE(6,*) '++++ WRONG KEYOPT=',KEYOPT
      STOP
      ENDIF
 
      CALL BHLUMI(  -1,XPAR,NPAR)
*     ==================================
      IF(KAT1.EQ.1) CALL BOKER1(-1)
      IF(KAT2.EQ.1) CALL BOKER2(-1)
      IF(KAT3.EQ.1) CALL BOKER3(-1)
      IF(KAT4.EQ.1) CALL BOKER4(-1)
*     *************************
 
      write(6,'(F10.2,A)') NEVT/1.E6,' Mega-events requested'
      write(6,*)  ' =======> generation starts...'
C by hand initialization of RANMAR subgenerator, may be omitted!
      CALL RMARIN(54217137,0,0)
      IEV=0
      DO 100 II =1,10000
      DO 100 III=1,10000000
      IEV=IEV+1
      IF(MOD(IEV,200000).EQ.1) WRITE(16,*)  'IEV= ',IEV
      IF(MOD(IEV, 20000).EQ.1) WRITE( 6,*)  'IEV= ',IEV
      CALL BHLUMI(   0,XPAR,NPAR)
*     ============================================
      IF(IEV.LE. 4) CALL DUMPS( 6)
      IF(IEV.LE. 4) CALL DUMPS(16)
      IF(KAT1.EQ.1) CALL BOKER1( 0)
      IF(KAT2.EQ.1) CALL BOKER2( 0)
      IF(KAT3.EQ.1) CALL BOKER3( 0)
      IF(KAT4.EQ.1) CALL BOKER4( 0)
*     *************************
C----------- EXIT FROM THE MAIN LOOP -------------
      IF(KONIEC(IEV,NEVT,LIMTIM)) GOTO 103
  100 CONTINUE
  103 CONTINUE
      write(6,*)  ' =======> generation finished'
      CALL BHLUMI(   2,XPAR,NPAR)
*     ============================================
      IF(KAT1.EQ.1) CALL BOKER1( 1)
      IF(KAT2.EQ.1) CALL BOKER2( 1)
      IF(KAT3.EQ.1) CALL BOKER3( 1)
      IF(KAT4.EQ.1) CALL BOKER4( 1)
C ------------WRITING HISTOS ON THE DISK ------------------------
C=====((((
C     NOUTH=10
C     OPEN(NOUTH)
C     CALL HRFILE(NOUTH,' ','N')
C     CALL HROUT( 0,ICY,' ')
C     CALL HREND(DNAME)
C=====)))
C ------------THE END OF HISTO WRITING -------------------------
      END
 
      LOGICAL FUNCTION KONIEC(IEV,NEVLIM,LIMTIM)
C     ******************************************
C This function signals that end of CPU time comes soon (in batch)
      TLIMIT=LIMTIM
C THIS IS SLACVM TIMER
CCC   CALL LEFT1A(TIMLFT)
C THIS IS CERNVM TIMER
CCC   CALL TIMEL(TIMLFT)
C APOLLO NO TIMER at all
      TIMLFT = 999999999.
      KONIEC = (IEV.GE.NEVLIM).OR.(TIMLFT.LT.TLIMIT)
      END
 
      SUBROUTINE BOKER1(MODE)
C     ***********************
C This for any type of generator BHLUM2, OLDBIS, LUMLOG.
C Using these results, in the later analysis,
C the inter-generator differences are calculated.
C Pure t-channel and no vacuum polarization !!!
C Calculated: integrated x-sections, litle discrete set for one
C energy-cut PLUS correspondig histograms on energy-cut dependence
*     ***********************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0 )
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
      LOGICAL LWIDE,LNARR,LMIX1,LMIX2
      DIMENSION NPAR(100),XPAR(100)
 
      IF(MODE.EQ.-1) THEN
*     *******************
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) 'BOKER1 start initialisation....  '
C
      TMAXG  = ACOS(1-(1-COS(TMING*PI/180))*RAXIG)*180/PI
      TMAXW  = ACOS(1-(1-COS(TMINW*PI/180))*RAXIW)*180/PI
      TMAXN  = ACOS(1-(1-COS(TMINN*PI/180))*RAXIN)*180/PI
      NPHI=6
      XMIN=VMAXE
      TH1W=TMINW*PI/180
      TH2W=TMAXW*PI/180
      TH1N=TMINN*PI/180
      TH2N=TMAXN*PI/180
      IDA=50
      DO 10 K=0,2
      CALL WMONI2(-1,IDA+K,DUMM1,DUMM2,DUMM3)
   10 CONTINUE
      BORWID  = BORNB(CMSENE,TH1W,TH2W)
      BORNAR  = BORNB(CMSENE,TH1N,TH2N)
C.... initialize histos
      NBIV =   40
      VMAX =  1D0
      VMIN =  0D0
C....   O(alf1)exp
      KEYGEN = MOD(KEYOPT,10000)/1000
      JDA = 2300  +10000*KEYGEN
C=====((((
C     DO 20 K=1,3
C     CALL HBOOK1(JDA+K,' O(alf1)exp      $',NBIV,VMIN,VMAX)
C  20 CONTINUE
C=====))))
      NEVGEN=0
      WRITE(NOUT,BXL1F) TH1W  ,     'theta_min wide     ','TH1W  ','  '
      WRITE(NOUT,BXL1F) TH2W  ,     'theta_max wide     ','TH2W  ','  '
      WRITE(NOUT,BXL1F) TH1N  ,     'theta_min narrow   ','TH1N  ','  '
      WRITE(NOUT,BXL1F) TH2N  ,     'theta_max narrow   ','TH2N  ','  '
      WRITE(NOUT,BXL1I) NPHI,       'No of phi-sectors  ','NPHI  ','  '
      WRITE(NOUT,BXL1F) BORWID,     'Born Wide    [nb]  ','BORWID','  '
      WRITE(NOUT,BXTXT) '  TH.6118 Tab.2a line.1          '
      WRITE(NOUT,BXL1F) BORNAR,     'Born Narrow  [nb]  ','BORNAR','  '
      WRITE(NOUT,BXTXT) '  .... BOKER1 end initialization '
      WRITE(NOUT,BXCLO)
      ELSEIF(MODE.EQ.0) THEN
*     **********************
      NEVGEN=NEVGEN+1
      WTCRUD = WTCRU1*WTCRU2
      LWIDE  = .FALSE.
      LNARR  = .FALSE.
      LMIX1  = .FALSE.
      LMIX2  = .FALSE.
      IF(WTCRUD.NE.0D0 ) THEN
C Two triggers
       IF(KEYTRI.EQ.0) THEN
       CALL TRIGA0(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
       ELSE
       CALL TRIGA1(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
       ENDIF
       LWIDE= XWIDE.GT.XMIN
       LNARR= XNARR.GT.XMIN
       LMIX1= XMIX1.GT.XMIN
       LMIX2= XMIX2.GT.XMIN
      ENDIF
C No trigger
      WTNOTR =  WTMOD
C With trigger number one
      WTWIDE = 0D0
      WTNARR = 0D0
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LWIDE) WTWIDE=WTNOTR
      IF(LNARR) WTNARR=WTNOTR
C no factor 1/2 due to symmetrizadion!!!
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
      CALL WMONI2( 0,IDA   ,WTWIDE,1D0,0D0)
      CALL WMONI2( 0,IDA+1 ,WTNARR,1D0,0D0)
C symmetrisation on forward/backward to improve a bit on statistics
C and to avoid symmetrisation inside OLDBIS!!!!
      CALL WMONI2( 0,IDA+2 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDA+2 ,WTMIX2,1D0,0D0)
C....    HISTOGRAMS
C....   O(alf1)norm/exp
C=====((((
C     CALL HF1(JDA+1, 1D0-XWIDE ,WTNOTR)
C     CALL HF1(JDA+2, 1D0-XNARR ,WTNOTR)
C     CALL HF1(JDA+3, 1D0-XMIX1 ,WTNOTR/2D0)
C     CALL HF1(JDA+3, 1D0-XMIX2 ,WTNOTR/2D0)
C=====))))
      ELSEIF(MODE.EQ.1) THEN
*     ***********************
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '============ BOKER1 ============='
      CALL BHLUMI(   1,XPAR,NPAR)
      NEVT  = NPAR(20)
      XCRU  = XPAR(20)
C....various types of the cuts
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'Integrated x-sect. various cuts  '
      WRITE(NOUT,BXTXT) '      Any sub-generator          '
      WRITE(NOUT,BXTXT) '      PURE BREMSSTRAHLUNG        '
      WRITE(NOUT,BXTXT) 'th.6118 fig.2a, middle bin       '
      WRITE(NOUT,BXTXT) 'th.6118 fig.3,  middle bin       '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// wide /////         '
      CALL PRICUT(IDA  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '      ///// narrow /////         '
      CALL PRICUT(IDA+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      WRITE(NOUT,BXTXT) 'th.6118 Tab.2a line.2, for BHLUM2'
      WRITE(NOUT,BXTXT) 'th.6118 Tab.2b line.2 (!!!)      '
      CALL PRICUT(IDA+2,XCRU,BORNAR)
      WRITE(NOUT,BXCLO)
C renormalize special histos (cut-off dependence)
      DO 401 K=1,3
      CALL CUMHI3(JDA+K,NBIV,NEVT,XCRU)
  401 CONTINUE
      ENDIF
*     *****
      END
 
      SUBROUTINE BOKER2(MODE)
*     ***********************
C Exclusively OLDBIS related tests
C No histograms!
C Calculated: effects due to up-down interference for realistic trigger
*     ***********************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0 )
      CHARACTER*80     BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      LOGICAL LWIDE,LNARR,LMIX1,LMIX2
      DIMENSION NPAR(100),XPAR(100)
      IF(MODE.EQ.-1) THEN
*     *******************
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) 'BOKER2 start initialisation....  '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'OLDBIS related tests...          '
      WRITE(NOUT,BXTXT) '*********************************'
      TMAXG  = ACOS(1-(1-COS(TMING*PI/180))*RAXIG)*180/PI
      TMAXW  = ACOS(1-(1-COS(TMINW*PI/180))*RAXIW)*180/PI
      TMAXN  = ACOS(1-(1-COS(TMINN*PI/180))*RAXIN)*180/PI
      NPHI=6
      XMIN=VMAXE
      TH1W=TMINW*PI/180
      TH2W=TMAXW*PI/180
      TH1N=TMINN*PI/180
      TH2N=TMAXN*PI/180
      IDF=10
      IDS=15
      DO 10 K=0,4
      CALL WMONI2(-1,IDF+K,DUMM1,DUMM2,DUMM3)
      CALL WMONI2(-1,IDS+K,DUMM1,DUMM2,DUMM3)
   10 CONTINUE
      BORWID  = BORNB(CMSENE,TH1W,TH2W)
      BORNAR  = BORNB(CMSENE,TH1N,TH2N)
      WRITE(NOUT,BXL1F) TH1W  ,     'theta_min wide     ','TH1W  ','  '
      WRITE(NOUT,BXL1F) TH2W  ,     'theta_max wide     ','TH2W  ','  '
      WRITE(NOUT,BXL1F) TH1N  ,     'theta_min narrow   ','TH1N  ','  '
      WRITE(NOUT,BXL1F) TH2N  ,     'theta_max narrow   ','TH2N  ','  '
      WRITE(NOUT,BXL1I) NPHI,       'No of phi-sectors  ','NPHI  ','  '
      WRITE(NOUT,BXL1F) BORWID,     'Born Wide    [nb]  ','BORWID','  '
      WRITE(NOUT,BXL1F) BORNAR,     'Born Narrow  [nb]  ','BORNAR','  '
      WRITE(NOUT,BXCLO)
      ELSEIF(MODE.EQ.0) THEN
*     **********************
      NEVGEN=NEVGEN+1
      WTCRUD = WTCRU1*WTCRU2
      LWIDE  = .FALSE.
      LNARR  = .FALSE.
      LMIX1  = .FALSE.
      LMIX2  = .FALSE.
      IF(WTCRUD.NE.0D0 ) THEN
C Two triggers
       IF(KEYTRI.EQ.0) THEN
       CALL TRIGA0(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
       ELSE
       CALL TRIGA1(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
       ENDIF
       LWIDE= XWIDE.GT.XMIN
       LNARR= XNARR.GT.XMIN
       LMIX1= XMIX1.GT.XMIN
       LMIX2= XMIX2.GT.XMIN
      ENDIF
C Non-interference x-section
C No trigger
      WTNOTR = WTCRU1*WTCRU2*WTSET(11)
C With trigger number one
      WTWIDE = 0D0
      WTNARR = 0D0
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LWIDE) WTWIDE=WTNOTR
      IF(LNARR) WTNARR=WTNOTR
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
C.... X-sections
      CALL WMONI2( 0,IDF   ,WTWIDE,1D0,0D0)
      CALL WMONI2( 0,IDF+1 ,WTNARR,1D0,0D0)
      CALL WMONI2( 0,IDF+2 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDF+2 ,WTMIX2,1D0,0D0)
C Interference contribs.
C No trigger
      WTNOTR = WTCRU1*WTCRU2*(WTSET(12)-WTSET(11))
C With trigger number one
      WTWIDE = 0D0
      WTNARR = 0D0
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LWIDE) WTWIDE=WTNOTR
      IF(LNARR) WTNARR=WTNOTR
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
C.... X-sections
      CALL WMONI2( 0,IDS   ,WTWIDE,1D0,0D0)
      CALL WMONI2( 0,IDS+1 ,WTNARR,1D0,0D0)
      CALL WMONI2( 0,IDS+2 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDS+2 ,WTMIX2,1D0,0D0)
 
      ELSEIF(MODE.EQ.1) THEN
*     ***********************
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '============ BOKER2 ============='
      WRITE(NOUT,BXTXT) '      OLDBIS related tests       '
      CALL BHLUMI(   1,XPAR,NPAR)
      NEVT  = NPAR(20)
      XCRU  = XPAR(20)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'Integrated x-sect. no interf.    '
      WRITE(NOUT,BXTXT) 'th.5888 tab.1, third column      '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// wide /////         '
      CALL PRICUT(IDF  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '      ///// narrow /////         '
      CALL PRICUT(IDF+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      CALL PRICUT(IDF+2,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '      UP-DOWN Interference       '
      WRITE(NOUT,BXTXT) 'th.5888 tab.1, forth column      '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// wide  /////        '
      CALL PRIDIF(IDS  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '       ///// narrow /////        '
      CALL PRIDIF(IDS+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      WRITE(NOUT,BXTXT) 'th.6118 tab.2b line.5            '
      CALL PRIDIF(IDS+2,XCRU,BORNAR)
      WRITE(NOUT,BXCLO)
      ENDIF
*     *****
      END
 
      SUBROUTINE BOKER3(MODE)
*     ***********************
C Tests related exclusively to LUMLOG
C Histograming plus discrete set of cross-sections.
C Calculated: LL cross sections in various perturbative orders
C (with and without exponentiation) and their differences
C depending on energy cut. Light fermion pair contribution.
*     ***********************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0 )
      CHARACTER*80    BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      LOGICAL LWIDE,LNARR,LMIX1,LMIX2
      DIMENSION NPAR(100),XPAR(100)
 
      IF(MODE.EQ.-1) THEN
*     *******************
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) 'BOKER3 start initialisation....  '
C
      TMAXG  = ACOS(1-(1-COS(TMING*PI/180))*RAXIG)*180/PI
      TMAXW  = ACOS(1-(1-COS(TMINW*PI/180))*RAXIW)*180/PI
      TMAXN  = ACOS(1-(1-COS(TMINN*PI/180))*RAXIN)*180/PI
      NPHI=6
      XMIN=VMAXE
      TH1W=TMINW*PI/180
      TH2W=TMAXW*PI/180
      TH1N=TMINN*PI/180
      TH2N=TMAXN*PI/180
      BORWID  = BORNB(CMSENE,TH1W,TH2W)
      BORNAR  = BORNB(CMSENE,TH1N,TH2N)
 
      IDR=5
      IDA=10
      IDD=20
      IDE=25
      IDP=30
      DO 10 K=0,4
      CALL WMONI2(-1,IDR+K,DUMM1,DUMM2,DUMM3)
      CALL WMONI2(-1,IDA+K,DUMM1,DUMM2,DUMM3)
      CALL WMONI2(-1,IDD+K,DUMM1,DUMM2,DUMM3)
      CALL WMONI2(-1,IDE+K,DUMM1,DUMM2,DUMM3)
      CALL WMONI2(-1,IDP+K,DUMM1,DUMM2,DUMM3)
   10 CONTINUE
C.... initialize histos
      NBIV =   40
      VMAX =  1D0
      VMIN =  0D0
C... JDE  O(alf3)exp. - O(alf1)norm.
C... JDN  O(alf1)norm.
      KEYGEN = MOD(KEYOPT,10000)/1000
      JDE = 2320  +10000*KEYGEN
      JDN = 2350  +10000*KEYGEN
C=====((((
C     DO 20 K=1,3
C     CALL HBOOK1(JDE+K,' alf3 - alf1, LUMLOG  $',NBIV,VMIN,VMAX)
C     CALL HBOOK1(JDN+K,' alf1       , LUMLOG  $',NBIV,VMIN,VMAX)
C  20 CONTINUE
C=====))))
      NEVGEN=0
      WRITE(NOUT,BXL1F) TH1W  ,     'theta_min wide     ','TH1W  ','  '
      WRITE(NOUT,BXL1F) TH2W  ,     'theta_max wide     ','TH2W  ','  '
      WRITE(NOUT,BXL1F) TH1N  ,     'theta_min narrow   ','TH1N  ','  '
      WRITE(NOUT,BXL1F) TH2N  ,     'theta_max narrow   ','TH2N  ','  '
      WRITE(NOUT,BXL1I) NPHI,       'No of phi-sectors  ','NPHI  ','  '
      WRITE(NOUT,BXL1F) BORWID,     'Born Wide    [nb]  ','BORWID','  '
      WRITE(NOUT,BXL1F) BORNAR,     'Born Narrow  [nb]  ','BORNAR','  '
      WRITE(NOUT,BXTXT) '  .... BOKER3 end initialization '
      WRITE(NOUT,BXCLO)
      ELSEIF(MODE.EQ.0) THEN
*     **********************
      NEVGEN=NEVGEN+1
      WTCRUD= WTCRU1*WTCRU2
      LWIDE  = .FALSE.
      LNARR  = .FALSE.
      LMIX1  = .FALSE.
      LMIX2  = .FALSE.
      IF(WTCRUD.NE.0D0 ) THEN
C Two triggers
       IF(KEYTRI.EQ.0) THEN
       CALL TRIGA0(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
       ELSE
       CALL TRIGA1(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
       ENDIF
       LWIDE= XWIDE.GT.XMIN
       LNARR= XNARR.GT.XMIN
       LMIX1= XMIX1.GT.XMIN
       LMIX2= XMIX2.GT.XMIN
      ENDIF
C for the moment energy cut is inside trigger
C...--------!!!!! BORN !!!!!!!!!
C Here calculation of Born is for controll only
C But for general exp. trigger this may be useful!
      WTWIDE=0D0
      IF(LWIDE) WTWIDE=WTCRUD*WTSET(11)
      CALL WMONI2( 0,IDR  ,WTWIDE,1D0,0D0)
      WTNARR=0D0
      IF(LNARR) WTNARR=WTCRUD*WTSET(11)
      CALL WMONI2( 0,IDR+1,WTNARR,1D0,0D0)
      WDMIXD=0D0
      IF(LMIX1) WDMIX1=WTCRUD*WTSET(11)
      IF(LMIX2) WDMIX2=WTCRUD*WTSET(11)
      CALL WMONI2( 0,IDR+3,WDMIX1,1D0,0D0)
      CALL WMONI2( 0,IDR+3,WDMIX2,1D0,0D0)
C... third order exponentiated
      WTWIDE=0D0
      IF(LWIDE) WTWIDE=WTCRUD*WTSET(4)
      CALL WMONI2( 0,IDA   ,WTWIDE,1D0,0D0)
      WTNARR=0D0
      IF(LNARR) WTNARR=WTCRUD*WTSET(4)
      CALL WMONI2( 0,IDA+1,WTNARR,1D0,0D0)
      WTMIX1=0D0
      WTMIX2=0D0
      IF(LMIX1) WTMIX1=WTCRUD*WTSET(4)
      IF(LMIX2) WTMIX2=WTCRUD*WTSET(4)
      CALL WMONI2( 0,IDA+2,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDA+2,WTMIX2,1D0,0D0)
C....differences between orders:  O(alf3-alf1)  exp.
      WDWIDE=0D0
      IF(LWIDE) WDWIDE=WTCRUD*(WTSET(4)-WTSET(2))
      CALL WMONI2( 0,IDD  ,WDWIDE,1D0,0D0)
      WDNARR=0D0
      IF(LNARR) WDNARR=WTCRUD*(WTSET(4)-WTSET(2))
      CALL WMONI2( 0,IDD+1,WDNARR,1D0,0D0)
      WDMIX1=0D0
      WDMIX2=0D0
      IF(LMIX1) WDMIX1=WTCRUD*(WTSET(4)-WTSET(2))
      IF(LMIX2) WDMIX2=WTCRUD*(WTSET(4)-WTSET(2))
      CALL WMONI2( 0,IDD+2,WDMIX1,1D0,0D0)
      CALL WMONI2( 0,IDD+2,WDMIX2,1D0,0D0)
C....differences between orders:  O(alf3)exp. -O(alf1)not.exp.
      WDWIDE=0D0
      IF(LWIDE) WDWIDE=WTCRUD*(WTSET(4)-WTSET(12))
      CALL WMONI2( 0,IDE  ,WDWIDE,1D0,0D0)
      WDNARR=0D0
      IF(LNARR) WDNARR=WTCRUD*(WTSET(4)-WTSET(12))
      CALL WMONI2( 0,IDE+1,WDNARR,1D0,0D0)
      WDMIXD=0D0
      IF(LMIX1) WDMIX1=WTCRUD*(WTSET(4)-WTSET(12))
      IF(LMIX2) WDMIX2=WTCRUD*(WTSET(4)-WTSET(12))
      CALL WMONI2( 0,IDE+2,WDMIX1,1D0,0D0)
      CALL WMONI2( 0,IDE+2,WDMIX2,1D0,0D0)
C....differences between orders:  O(alf3)exp. -O(alf2)not.exp.
      WDMIXD=0D0
      IF(LMIX1) WDMIX1=WTCRUD*(WTSET(4)-WTSET(13))
      IF(LMIX2) WDMIX2=WTCRUD*(WTSET(4)-WTSET(13))
      CALL WMONI2( 0,IDE+3,WDMIX1,1D0,0D0)
      CALL WMONI2( 0,IDE+3,WDMIX2,1D0,0D0)
C....pairs
      WDWIDE=0D0
      IF(LWIDE) WDWIDE=WTCRUD*(WTSET(5)-WTSET(4))
      CALL WMONI2( 0,IDP  ,WDWIDE,1D0,0D0)
      WDNARR=0D0
      IF(LNARR) WDNARR=WTCRUD*(WTSET(5)-WTSET(4))
      CALL WMONI2( 0,IDP+1,WDNARR,1D0,0D0)
      WDMIX1=0D0
      WDMIX2=0D0
      IF(LMIX1) WDMIX1=WTCRUD*(WTSET(5)-WTSET(4))
      IF(LMIX2) WDMIX2=WTCRUD*(WTSET(5)-WTSET(4))
      CALL WMONI2( 0,IDP+2,WDMIX1,1D0,0D0)
      CALL WMONI2( 0,IDP+2,WDMIX2,1D0,0D0)
C....    HISTOGRAMS
C....   O(alf3)exp. - O(alf1)norm.
C=====((((
C     WTACT = WTCRUD*(WTSET(4)-WTSET(12))
C     CALL HF1(JDE+1, 1D0-XWIDE ,WTACT)
C     CALL HF1(JDE+2, 1D0-XNARR ,WTACT)
C     CALL HF1(JDE+3, 1D0-XMIX1 ,WTACT/2D0)
C     CALL HF1(JDE+3, 1D0-XMIX2 ,WTACT/2D0)
CC...   O(alf1)norm.
C     WTACT = WTCRUD*WTSET(12)
C     CALL HF1(JDN+1, 1D0-XWIDE ,WTACT)
C     CALL HF1(JDN+2, 1D0-XNARR ,WTACT)
C     CALL HF1(JDN+3, 1D0-XMIX1 ,WTACT/2D0)
C     CALL HF1(JDN+3, 1D0-XMIX2 ,WTACT/2D0)
C=====))))
      ELSEIF(MODE.EQ.1) THEN
*     ***********************
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '============ BOKER3 ============='
      WRITE(NOUT,BXTXT) 'LUMLOG corrections and  x-sects. '
      CALL BHLUMI(   1,XPAR,NPAR)
      NEVT  = NPAR(20)
      XCRU  = XPAR(20)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '          BORN,     O(alf0)      '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// wide /////         '
      CALL PRICUT(IDR  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '      ///// narrow /////         '
      CALL PRICUT(IDR+1,XCRU,BORNAR)
CC    WRITE(NOUT,BXTXT) '       ////// mixed /////        '
CC    CALL PRICUT(IDR+2,XCRU,BORNAR)
C....various types of the cuts
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '      O(alf3) exp. various cuts  '
      WRITE(NOUT,BXTXT) 'th.5995 fig.2, middle bin        '
      WRITE(NOUT,BXTXT) 'th.5995 tab.2, third column     '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// wide /////         '
      CALL PRICUT(IDA  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '      ///// narrow /////         '
      CALL PRICUT(IDA+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      CALL PRICUT(IDA+2,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       O(alf3)-O(alf1) exp.      '
      WRITE(NOUT,BXTXT) 'th.5995 fig.4, middle bin        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ---- WIDE -----           '
      CALL PRIDIF(IDD  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '       ---- NARROW ---           '
      CALL PRIDIF(IDD+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ---- MIXED  ---           '
      CALL PRIDIF(IDD+2,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  O(alf3)exp-O(alf1)not.exp.     '
      WRITE(NOUT,BXTXT) 'th.5995 tab.2,  forth column     '
      WRITE(NOUT,BXTXT) 'th.5995 fig.3,  middle bin       '
      WRITE(NOUT,BXTXT) 'th.6118 fig.2a, middle bin       '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ---- WIDE -----           '
      CALL PRIDIF(IDE  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '       ---- NARROW ---           '
      CALL PRIDIF(IDE+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ---- MIXED  ---           '
      WRITE(NOUT,BXTXT) 'th.6118 tab.2b line.2 (component)'
      CALL PRIDIF(IDE+2,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  O(alf3)exp-O(alf2)not.exp.     '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ---- MIXED  ---           '
      WRITE(NOUT,BXTXT) 'th.6118 tab.2b line.4            '
      CALL PRIDIF(IDE+3,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  O(alf3)pairs-O(alf3)nopairs    '
      WRITE(NOUT,BXTXT) '*********************************'
CC    WRITE(NOUT,BXTXT) '       ---- WIDE -----           '
CC    CALL PRIDIF(IDP  ,XCRU,BORWID)
CC    WRITE(NOUT,BXTXT) '       ---- NARROW ---           '
CC    CALL PRIDIF(IDP+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ---- MIXED  ---           '
      WRITE(NOUT,BXTXT) 'th.6118 tab.2b line.6            '
      CALL PRIDIF(IDP+2,XCRU,BORNAR)
      WRITE(NOUT,BXCLO)
C renormalize special histos (cut-off dependence)
      DO 401 K=1,3
      CALL CUMHI3(JDE+K,NBIV,NEVT,XCRU)
      CALL CUMHI3(JDN+K,NBIV,NEVT,XCRU)
  401 CONTINUE
      ENDIF
*     *****
      END
 
      SUBROUTINE BOKER4(MODE)
C     ***********************
C BHLUM2 related tests:
C Histograms + discrete set.
C Calculated:
C (a) Total QED correction, depending on z_min
C (b) Subleading correction
C (c) Test on phase space completness specific for BHLUMI generator
C Warning! this routine is spying on BHLUMI through /TRANSR/
C     ***********************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0 )
      CHARACTER*80   BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI
C THIS IS COMMON BLOCK FROM INSIDE GENERATOR !!!!!!!
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
C !!!!!!!
      LOGICAL LWIDE,LNARR,LMIX1,LMIX2
      DIMENSION NPAR(100),XPAR(100)
 
      IF(MODE.EQ.-1) THEN
*     *******************
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) 'BOKER4 start initialisation....  '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'BHLUM2 related tests...          '
      WRITE(NOUT,BXTXT) 'total the best correction.       '
      WRITE(NOUT,BXTXT) 'test of phase space completness  '
      WRITE(NOUT,BXTXT) 'transfer dist. before/after trig.'
      WRITE(NOUT,BXTXT) '*********************************'
C
      TMAXG  = ACOS(1-(1-COS(TMING*PI/180))*RAXIG)*180/PI
      TMAXW  = ACOS(1-(1-COS(TMINW*PI/180))*RAXIW)*180/PI
      TMAXN  = ACOS(1-(1-COS(TMINN*PI/180))*RAXIN)*180/PI
      NPHI=6
      XMIN=VMAXE
      TH1W=TMINW*PI/180
      TH2W=TMAXW*PI/180
      TH1N=TMINN*PI/180
      TH2N=TMAXN*PI/180
      IDT=40
      IDB=55
      IDV=70
      IDZ=75
      IDM=80
      DO 10 K=0,4
      CALL WMONI2(-1,IDT+K,DUMM1,DUMM2,DUMM3)
      CALL WMONI2(-1,IDB+K,DUMM1,DUMM2,DUMM3)
      CALL WMONI2(-1,IDV+K,DUMM1,DUMM2,DUMM3)
      CALL WMONI2(-1,IDZ+K,DUMM1,DUMM2,DUMM3)
      CALL WMONI2(-1,IDM+K,DUMM1,DUMM2,DUMM3)
   10 CONTINUE
      BORWID  = BORNB(CMSENE,TH1W,TH2W)
      BORNAR  = BORNB(CMSENE,TH1N,TH2N)
C.... initialize histos
      NBIV =   40
      VMAX =  1D0
      VMIN =  0D0
C....   O(alf1)exp total, vac_pol. and Z included
C....   O(alf1)exp sub-leading, vac_pol. and Z excluded
      KEYGEN = MOD(KEYOPT,10000)/1000
      JDT = 2000  +10000*KEYGEN
      JDB = 2400  +10000*KEYGEN
C=====((((
C     DO 20 K=1,3
C     CALL HBOOK1(JDT+K,' O(alf1)exp all  $',NBIV,VMIN,VMAX)
C     CALL HBOOK1(JDB+K,' O(alf1)exp subl $',NBIV,VMIN,VMAX)
C  20 CONTINUE
C=====))))
C.....
C Born nad  Leading-log limiting values for the transfer
      TRMINB =CMSENE**2*      (1D0-COS(TMING*PI/180))/2D0
      TRMINL =CMSENE**2*VMAXE*(1D0-COS(TMING*PI/180))/2D0
      TRMAXL =CMSENE**2*      (1D0-COS(TMAXG*PI/180))/2D0
C transfer distribution with/without trigger
      KEYGEN = MOD(KEYOPT,10000)/1000
      JDH  =  400  +10000*KEYGEN
      NBIN =  100
      UMIN =  0D0
      UMAX =  TRMAXL*1.25D0
C=====((((
C     CALL HBOOK1(JDH+0,'tran, all   $',NBIN,UMIN,UMAX)
C     CALL HBOOK1(JDH+1,'tran, WIDE  $',NBIN,UMIN,UMAX)
C     CALL HBOOK1(JDH+2,'tran, MIXD  $',NBIN,UMIN,UMAX)
C=====))))
C weight after triggen N-W
      WMIN = -0.5D0
      WMAX =  4.5D0
      WTMAX = 2.5D0
C=====((((
C     CALL HBOOK1(JDH+5,'WTmod MIXD  $',NBIN,WMIN,WMAX)
C=====))))
      REPIB = REPI(-TRMINB)
      REPI1 = REPI(-TRMINL)
      REPI2 = REPI(-TRMAXL)
      NEVGEN=0
      WRITE(NOUT,BXL1F) TH1W  ,     'theta_min wide     ','TH1W  ','  '
      WRITE(NOUT,BXL1F) TH2W  ,     'theta_max wide     ','TH2W  ','  '
      WRITE(NOUT,BXL1F) TH1N  ,     'theta_min narrow   ','TH1N  ','  '
      WRITE(NOUT,BXL1F) TH2N  ,     'theta_max narrow   ','TH2N  ','  '
      WRITE(NOUT,BXL1I) NPHI,       'No of phi-sectors  ','NPHI  ','  '
      WRITE(NOUT,BXL1F) BORWID,     'Born Wide    [nb]  ','BORWID','  '
      WRITE(NOUT,BXL1F) BORNAR,     'Born Narrow  [nb]  ','BORNAR','  '
      WRITE(NOUT,BXL1F) TRMINB,     'trans_min Born     ','TRMINB','  '
      WRITE(NOUT,BXL1F) TRMINL,     'trans_min L-Log    ','TRMINL','  '
      WRITE(NOUT,BXL1F) TRMAXL,     'trans_max L-Log    ','TRMAXL','  '
      WRITE(NOUT,BXL1F) REPIB ,     'RePi(tran_min_Born)','REPIB ','  '
      WRITE(NOUT,BXL1F) REPI1 ,     'RePi(tran_LL_min)  ','REPI1 ','  '
      WRITE(NOUT,BXL1F) REPI2 ,     'RePi(tran_LL_max)  ','REPI2 ','  '
      WRITE(NOUT,BXTXT) '  .... BOKER4 end initialization '
      WRITE(NOUT,BXCLO)
      ELSEIF(MODE.EQ.0) THEN
*     **********************
      NEVGEN=NEVGEN+1
      WTCRUD = WTCRU1*WTCRU2
      LWIDE  = .FALSE.
      LNARR  = .FALSE.
      LMIX1  = .FALSE.
      LMIX2  = .FALSE.
      IF(WTCRUD.NE.0D0 ) THEN
C Two triggers
       IF(KEYTRI.EQ.0) THEN
       CALL TRIGA0(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
       ELSE
       CALL TRIGA1(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
       ENDIF
       LWIDE= XWIDE.GT.XMIN
       LNARR= XNARR.GT.XMIN
       LMIX1= XMIX1.GT.XMIN
       LMIX2= XMIX2.GT.XMIN
      ENDIF
C BHLUM2 total... with vac_pol and Z
C No trigger
      WTNOTR = WTCRU1*WTCRU2*WTSET(12)
C With trigger number one
      WTWIDE = 0D0
      WTNARR = 0D0
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LWIDE) WTWIDE=WTNOTR
      IF(LNARR) WTNARR=WTNOTR
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
C.... X-sections
      CALL WMONI2( 0,IDT   ,WTWIDE,1D0,0D0)
      CALL WMONI2( 0,IDT+1 ,WTNARR,1D0,0D0)
      CALL WMONI2( 0,IDT+2 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDT+2 ,WTMIX2,1D0,0D0)
C....    HISTOGRAMS
C=====((((
C     CALL HF1(JDT+1, 1D0-XWIDE ,WTNOTR)
C     CALL HF1(JDT+2, 1D0-XNARR ,WTNOTR)
C     CALL HF1(JDT+3, 1D0-XMIX1 ,WTNOTR/2D0)
C     CALL HF1(JDT+3, 1D0-XMIX2 ,WTNOTR/2D0)
C=====))))
C BHLUM2 subleading...... pure bremss. no va_pol. Z
C No trigger
      WTNOTR = WTCRU1*WTCRU2*(WTSET(52)-WTSET(62))
C With trigger number one
      WTWIDE = 0D0
      WTNARR = 0D0
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LWIDE) WTWIDE=WTNOTR
      IF(LNARR) WTNARR=WTNOTR
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
C.... X-sections
      CALL WMONI2( 0,IDB   ,WTWIDE,1D0,0D0)
      CALL WMONI2( 0,IDB+1 ,WTNARR,1D0,0D0)
      CALL WMONI2( 0,IDB+2 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDB+2 ,WTMIX2,1D0,0D0)
C.... HISTOGRAMS
C=====((((
C     CALL HF1(JDB+1, 1D0-XWIDE ,WTNOTR)
C     CALL HF1(JDB+2, 1D0-XNARR ,WTNOTR)
C     CALL HF1(JDB+3, 1D0-XMIX1 ,WTNOTR/2D0)
C     CALL HF1(JDB+3, 1D0-XMIX2 ,WTNOTR/2D0)
C=====))))
C......................................................C
C.... vacuum polarization correction                   C
C......................................................C
      WTNOTR =  WTCRU1*WTCRU2*WTSET(20)
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
      CALL WMONI2( 0,IDV+3 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDV+3 ,WTMIX2,1D0,0D0)
      WTNOTR =  WTCRU1*WTCRU2*WTSET(21)
      WTWIDE = 0D0
      WTNARR = 0D0
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LWIDE) WTWIDE=WTNOTR
      IF(LNARR) WTNARR=WTNOTR
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
      CALL WMONI2( 0,IDV   ,WTWIDE,1D0,0D0)
      CALL WMONI2( 0,IDV+1 ,WTNARR,1D0,0D0)
      CALL WMONI2( 0,IDV+2 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDV+2 ,WTMIX2,1D0,0D0)
C......................................................C
C....    Z-exchange correction                         C
C......................................................C
      WTNOTR =  WTCRU1*WTCRU2*WTSET(22)
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
      CALL WMONI2( 0,IDZ+3 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDZ+3 ,WTMIX2,1D0,0D0)
      WTNOTR =  WTCRU1*WTCRU2*WTSET(23)
      WTWIDE = 0D0
      WTNARR = 0D0
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LWIDE) WTWIDE=WTNOTR
      IF(LNARR) WTNARR=WTNOTR
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
      CALL WMONI2( 0,IDZ   ,WTWIDE,1D0,0D0)
      CALL WMONI2( 0,IDZ+1 ,WTNARR,1D0,0D0)
      CALL WMONI2( 0,IDZ+2 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDZ+2 ,WTMIX2,1D0,0D0)
C......................................................C
C....  S-channel-gamma exchange correction             C
C......................................................C
      WTNOTR =  WTCRU1*WTCRU2*WTSET(25)
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
      CALL WMONI2( 0,IDZ+4 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDZ+4 ,WTMIX2,1D0,0D0)
C......................................................C
C....  beta_0 cortribution                             C
C......................................................C
      WTNOTR =  WTCRU1*WTCRU2*WTSET(26)
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
      CALL WMONI2( 0,IDM+0 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDM+0 ,WTMIX2,1D0,0D0)
C......................................................C
C....  beta_1 cortribution                             C
C......................................................C
      WTNOTR =  WTCRU1*WTCRU2*WTSET(27)
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
      CALL WMONI2( 0,IDM+1 ,WTMIX1,1D0,0D0)
      CALL WMONI2( 0,IDM+1 ,WTMIX2,1D0,0D0)
C......................................................
C.... test on minimum transfer in MC generation       C
C......................................................
      WTNOTR =  WTCRU1*WTCRU2*WTSET(52)
      WTWIDE = 0D0
      WTNARR = 0D0
      WTMIX1 = 0D0
      WTMIX2 = 0D0
      IF(LWIDE) WTWIDE=WTNOTR
      IF(LNARR) WTNARR=WTNOTR
      IF(LMIX1) WTMIX1=WTNOTR
      IF(LMIX2) WTMIX2=WTNOTR
C=====((((
C     CALL HF1(JDH+0, TRAN, WTNOTR)
C     CALL HF1(JDH+1, TRAN, WTWIDE)
C     CALL HF1(JDH+2, TRAN, WTMIX1/2D0)
C     CALL HF1(JDH+2, TRAN, WTMIX2/2D0)
CCCC weight distribution
C     CALL HF1(JDH+5, WTMIX1,0.5D0)
C     CALL HF1(JDH+5, WTMIX2,0.5D0)
C=====))))
C x-section in WT>WTMAX events
      WTOVE1 = 0D0
      WTOVE2 = 0D0
      IF(WTMIX1.GT.WTMAX) WTOVE1= WTMIX1-WTMAX
      IF(WTMIX2.GT.WTMAX) WTOVE2= WTMIX2-WTMAX
      CALL WMONI2( 0,IDM+2 ,WTOVE1,1D0,0D0)
      CALL WMONI2( 0,IDM+2 ,WTOVE2,1D0,0D0)
 
      ELSEIF(MODE.EQ.1) THEN
*     ***********************
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '============ BOKER4 ============='
      WRITE(NOUT,BXTXT) '      BHLUM2 related tests       '
      WRITE(NOUT,BXTXT) 'Various corr. Table 2 in TH.6118 '
      CALL BHLUMI(   1,XPAR,NPAR)
      NEVT  = NPAR(20)
      XCRU  = XPAR(20)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'Integrated x-sect. total best    '
      WRITE(NOUT,BXTXT) 'th.6118 fig.1, middle bin        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// wide /////         '
      CALL PRICUT(IDT  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '      ///// narrow /////         '
      CALL PRICUT(IDT+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      WRITE(NOUT,BXTXT) 'th.6118 tab.2a line.6            '
      CALL PRICUT(IDT+2,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'Integrated x-sect. SUBLEADING    '
      WRITE(NOUT,BXTXT) 'th.6118 fig.3, middle bin        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// wide /////         '
      CALL PRIDIF(IDB  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '      ///// narrow /////         '
      CALL PRIDIF(IDB+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      CALL PRIDIF(IDB+2,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'Integrated x-sect. Vac_Pol       '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// wide /////         '
      CALL PRIDIF(IDV  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '      ///// narrow /////         '
      CALL PRIDIF(IDV+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      CALL PRIDIF(IDV+2,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      WRITE(NOUT,BXTXT) 'th.6118 tab.2a line.3            '
      CALL PRICUT(IDV+3,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'Integrated x-sect. Z contr.      '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// wide /////         '
      CALL PRIDIF(IDZ  ,XCRU,BORWID)
      WRITE(NOUT,BXTXT) '       ///// narrow /////        '
      CALL PRIDIF(IDZ+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      CALL PRIDIF(IDZ+2,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      WRITE(NOUT,BXTXT) 'th.6118 tab.2a line.4            '
      CALL PRICUT(IDZ+3,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'Integrated x-sect. s_Cor.        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      WRITE(NOUT,BXTXT) 'th.6118 tab.2a line.5            '
      CALL PRIDIF(IDZ+4,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'Beta_0, Beta_1 contr.            '
      WRITE(NOUT,BXTXT) 'th.6118 tab.2b indirectly        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      CALL PRIDIF(IDM+0,XCRU,BORNAR)
      CALL PRIDIF(IDM+1,XCRU,BORNAR)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'WT>WTMAX contr. important x-check'
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '       ////// mixed /////        '
      CALL PRIDIF(IDM+2,XCRU,BORNAR)
      WRITE(NOUT,BXCLO)
C renormalize normal histos
C=====((((
C     DO 401 K=1,3
C 401 CALL CUMHI3(JDT+K,NBIV,NEVT,XCRU)
C     DO 402 K=1,3
C 402 CALL CUMHI3(JDB+K,NBIV,NEVT,XCRU)
C     FACT  = NBIN/(NEVT*ABS(UMAX-UMIN)) *XCRU
C     DO 405 K=0,2
C     CALL RENHIE(JDH+K, FACT ,NBIN)
C     CALL HPRINT(JDH+K)
C 405 CONTINUE
C     FACT  = NBIN/(NEVT*ABS(WMAX-WMIN))
C     CALL HPRINT(JDH+5)
C     CALL RENHIE(JDH+5, FACT ,NBIN)
C     CALL HPRINT(JDH+5)
C=====))))
      ENDIF
*     *****
      END
 
      SUBROUTINE TRIGA0(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,
     $                 XMIX1,XMIX2)
C     **********************************************************
C Idealized simple trigger on BARE/DRESSED final electrons.
C It is made entirely on P2, Q2 momenta --
C for BHLUMI and OLDBIS they are normal "bare" electrons
C while for LUMLOG they are by definition "dressed".
C This trigger is used in TH.5888 and TH.9555.
C Input:  TH1N,TH2N,TH1W,TH2W  theta limits of the Narrow/Wide cones
C         NPHI        number of phi-sectors (dummy)
C Output: XWIDE,XNARR,XMIX1,XMIX2 = 0 or V=1-S'/S, depending
C         whether theta's fall into corresponding enery range
C         RANGE = N-N, W-W, N-W, W-N or does not.
C This looks a bit like puting right hand to left pocket but
C it is done in order to be in compatibility with more complicated
C realistic calorimetric triggers,  see TRIGAS1.
C What is important to remember is that to accept an event we shall
C require 0 X>1-V_min in the routine calling TRIGAS0.
C The condition X>1-V_min is in fact the logical multiplication
C (V<V_min) .AND. (theta's in the given RANGE) i.e. definition
C of our present trigger!
C N.B. This trigger is identical to trigger in CUTCUT in
C version of LUMLOG distributed in January 91.
C     ******************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      LOGICAL LFORW,LFORN,LBACW,LBACN
C wide/narrow sectors forward
      THET=ANGFI(P2(3),DSQRT(P2(1)**2+P2(2)**2))
      LFORW= THET.GT.TH1W.AND.THET.LT.TH2W
      LFORN= THET.GT.TH1N.AND.THET.LT.TH2N
C wide/narrow sectors backward
      THET=ANGFI(-Q2(3),DSQRT(Q2(1)**2+Q2(2)**2))
      LBACW= THET.GT.TH1W.AND.THET.LT.TH2W
      LBACN= THET.GT.TH1N.AND.THET.LT.TH2N
C ....
      SV1 = (P2(4)+Q2(4))**2 -(P2(3)+Q2(3))**2
     $     -(P2(2)+Q2(2))**2 -(P2(1)+Q2(1))**2
      SV  = (P1(4)+Q1(4))**2 -(P1(3)+Q1(3))**2
     $     -(P1(2)+Q1(2))**2 -(P1(1)+Q1(1))**2
      V = 1 -SV1/SV
C ....
      XWIDE= 0D0
      XNARR= 0D0
      XMIX1= 0D0
      XMIX2= 0D0
      IF(LFORW.AND.LBACW) XWIDE= 1D0-V
      IF(LFORN.AND.LBACN) XNARR= 1D0-V
      IF(LFORW.AND.LBACN) XMIX1= 1D0-V
      IF(LFORN.AND.LBACW) XMIX2= 1D0-V
      END
 
 
      SUBROUTINE TRIGA1(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,
     $                    XMIX1,XMIX2)
C     **********************************************************
C Idealized exper. CALORIMETRIC trigger on dressed final electrons.
C Electrons and photons not distinguished!
C It is described in detail in TH.6118.
C Input:  TH1N,TH2N,TH1W,TH2W  theta limits of the Narrow/Wide cone
C         NPHI                 number of phi-sectors
C Output: XWIDE,XNARR,XMIX1,XMIX2, each variable X parametrizes
C         enery "cought" in the corresponding N-N, W-W, N-W, W-N
C         angular range, X>X_min is the energy cut contition
C     ******************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0 )
      PARAMETER( NMX1 = 20)
      LOGICAL LANGW,LANGN,LPHI
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      DIMENSION PC(100,4)
      DIMENSION PCW1(NMX1,4),PCN1(NMX1,4)
      DIMENSION PCW2(NMX1,4),PCN2(NMX1,4)
      DATA ICONT /0/
C Beam energy
      ENE = P1(4)
C Final electrons and photons not distinguished
      DO 10 K=1,4
      PC(1,K)=P2(K)
   10 PC(2,K)=Q2(K)
      DO 20 I=1,NPHOT
      DO 20 K=1,4
   20 PC(2+I,K)=PHOT(I,K)
      NP = NPHOT+2
      DO 40 I=1,NMX1
      DO 40 K=1,4
      PCW1(I,K)=0D0
      PCN1(I,K)=0D0
      PCW2(I,K)=0D0
   40 PCN2(I,K)=0D0
C
C Collecting energies in calorimeter sectors
C
      DO 100 I=1,NP
C wide/narrow sectors forward
      THET=ANGFI(PC(I,3),DSQRT(PC(I,1)**2+PC(I,2)**2))
      PHI =ANGFI(PC(I,1),PC(I,2))
      LANGW= THET.GT.TH1W.AND.THET.LT.TH2W
      LANGN= THET.GT.TH1N.AND.THET.LT.TH2N
      DO 60 JPHI=1,NPHI
      PHI1 = (JPHI-1)*(2D0*PI/NPHI)
      PHI2 =    JPHI *(2D0*PI/NPHI)
      LPHI= PHI .GT.PHI1.AND. PHI.LT.PHI2
      IF(LANGW.AND.LPHI) THEN
        DO 50 K=1,4
   50   PCW1(JPHI,K)=PCW1(JPHI,K)+ PC(I,K)
      ENDIF
      IF(LANGN.AND.LPHI) THEN
        DO 51 K=1,4
   51   PCN1(JPHI,K)=PCN1(JPHI,K)+ PC(I,K)
      ENDIF
   60 CONTINUE
C wide/narrow sectors backward
      THET=ANGFI(-PC(I,3),DSQRT(PC(I,1)**2+PC(I,2)**2))
      PHI =ANGFI(-PC(I,1),-PC(I,2))
      LANGW= THET.GT.TH1W.AND.THET.LT.TH2W
      LANGN= THET.GT.TH1N.AND.THET.LT.TH2N
      DO 80 JPHI=1,NPHI
      PHI1 = (JPHI-1)*(2D0*PI/NPHI)
      PHI2 =    JPHI *(2D0*PI/NPHI)
      LPHI= PHI .GT.PHI1.AND. PHI.LT.PHI2
      IF(LANGW.AND.LPHI) THEN
        DO 70 K=1,4
   70   PCW2(JPHI,K)=PCW2(JPHI,K)+ PC(I,K)
      ENDIF
      IF(LANGN.AND.LPHI) THEN
        DO 71 K=1,4
   71   PCN2(JPHI,K)=PCN2(JPHI,K)+ PC(I,K)
      ENDIF
   80 CONTINUE
  100 CONTINUE
 
C at least one coincidences in a pair of opposite calorimetric blocks
      XWIDE= 0D0
      XNARR= 0D0
      XMIX1= 0D0
      XMIX2= 0D0
      EMIN = XMIN*ENE
      DO 150 JPHI=1,NPHI
C minimum of energies deposed in the opposite blocks,
C we will require energies in opposite block above certain minimum
      EWW   = DMIN1(PCW1(JPHI,4),PCW2(JPHI,4))
      ENN   = DMIN1(PCN1(JPHI,4),PCN2(JPHI,4))
      EWN   = DMIN1(PCW1(JPHI,4),PCN2(JPHI,4))
      ENW   = DMIN1(PCN1(JPHI,4),PCW2(JPHI,4))
C maximum over pairs of blocks,
C we will ask at least one pair of blocks excited above certain min.
      XWIDE = DMAX1(XWIDE,EWW/ENE)
      XNARR = DMAX1(XNARR,ENN/ENE)
      XMIX1 = DMAX1(XMIX1,EWN/ENE)
      XMIX2 = DMAX1(XMIX2,ENW/ENE)
  150 CONTINUE
      END
 
      SUBROUTINE DUMPC(NOUT,PC,NMX,NP)
C     ********************************
C For testing TRIGAS1 trigger routine.
C PRINTS OUT FOUR MOMENTA IN MATRIX PC(NMX,4)
C ON THE OUTPUT UNIT NOUT
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PC(NMX,4),SUM(4)
      WRITE(NOUT,*) '---------------------DUMPC--------------------'
      DO 100 I=1,NP
  100 WRITE(NOUT,3100) 'PC ',(PC(I,K),K=1,4)
      DO 200 K=1,4
  200 SUM(K)=0D0
      DO 210 I=1,NP
      DO 210 K=1,4
  210 SUM(K)=SUM(K)+PC(I,K)
      WRITE(NOUT,3100) 'SUM',(SUM(K),K=1,4)
 3100 FORMAT(1X,A3,1X,5F18.13)
      END
 
 
      SUBROUTINE RENHIE(ID,FACT,NB)
C     ****************************
C watch out!!! single precision here
C errors taken into account
C     INTRODUCES HISTOGRAM NORMALISATION
      DOUBLE PRECISION FACT
      DIMENSION X(400),XE(400)
      FAC=FACT
C=====((((
C     CALL HUNPAK(ID, X,'    ',1)
C     CALL HUNPAK(ID,XE,'ERRO',1)
C     CALL HRESET(ID,' ')
C=====))))
      SUM=0.
      DO 10 I=1,NB
   10 SUM=SUM+X(I)
      IF(SUM.EQ.0.) RETURN
      IF(FAC.EQ.0.) FAC=1./SUM
      DO 20 I=1,NB
      X(I) = X(I)*FAC
      XE(I)=XE(I)*FAC
   20 CONTINUE
C=====((((
C     CALL HPAK( ID,X )
C     CALL HPAKE(ID,XE)
C=====))))
      END
 
      SUBROUTINE CUMHI3(ID,NB,NEVT,XCRU)
C     **********************************
C cumulates histogram content starting from underflow
*     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*4 X(400),ER(100)
C=====((((
C     SWT  = HI (ID,0)
C     SSWT = HIE(ID,0)**2
C     DO 100 I=1,NB
C     SWT   = SWT + HI (ID,I)
C     SSWT  = SSWT+ HIE(ID,I)**2
C     XSEC  = SWT*(XCRU/NEVT)
C note NEVT in error calc. is for ethe ntire sample related
C to XCRU !!!! including zero weight events !
C     ERREL = SQRT(ABS(SSWT/SWT**2-1D0/FLOAT(NEVT)))
C     X(I)  = XSEC
C     ER(I) = XSEC*ERREL
C100  CONTINUE
C     CALL HPAK (ID  ,X)
C     CALL HPAKE(ID  ,ER)
C     CALL HIDOPT(ID,'ERRO')
C     CALL HPRINT(ID)
C=====((((
      END
 
      SUBROUTINE PRICUT(ID,XCRU,BORN)
C     ***********************************
C printing averages/xsections
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      CHARACTER*80      BXL1F,BXL2F
      BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)'
      BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)'
      CALL WMONI2( 1,ID,AWT,DWT,DUMM)
      XS = XCRU*AWT
      DS = XCRU*AWT*DWT
      WRITE(NOUT,BXL2F) XS,DS,  'xsec. total        ','XS    ','  '
      WRITE(NOUT,BXL1F) DS/XS,  'rel. error         ','DS/XS ','  '
      RXS = XCRU*AWT      /BORN -1
      RDS = XCRU*AWT*DWT  /BORN
      WRITE(NOUT,BXL2F) RXS,RDS,'O(alfi)/Born -1    ','RXS   ','  '
      END
 
      SUBROUTINE PRIDIF(ID,XCRU,BORN)
C     ***********************************
C printing averages/xsections
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      CHARACTER*80      BXL1F,BXL2F
      BXL1F =  '(1X,1H*,F17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2F =  '(1X,1H*,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      CALL WMONI2(1,ID,AWT,DWT,DUMM3)
      RXS = XCRU*AWT        /BORN
      RDS = XCRU*AWT*DWT    /BORN
      WRITE(NOUT,BXL2F) RXS,RDS,'O(alfi-alfj)/Born  ','RXS   ','  '
C======================================================================
C=====================BHLUMI 2.01======================================
C================THE END OF DEMONSTRATION DECK ========================
C======================================================================
      END
 
 
 
      SUBROUTINE BHLUMI(MODE,XPAR,NPAR)
*     *********************************
C
C   BBBBBBB    BBB   BBB  BBB      BBB  BBB  BBB     BBB   BBB
C   BBB  BBB   BBB   BBB  BBB      BBB  BBB  BBBB   BBBB   BBB
C   BBB  BBB   BBB   BBB  BBB      BBB  BBB  BBBBB BBBBB   BBB
C   BBBBBB     BBBBBBBBB  BBB      BBB  BBB  BBB BBB BBB   BBB
C   BBBBBBBBB  BBBBBBBBB  BBB      BBB  BBB  BBB  B  BBB   BBB
C   BBB  BBBB  BBB   BBB  BBB  BB  BBB  BBB  BBB     BBB   BBB
C   BBBBBBBBB  BBB   BBB  BBBBBBB  BBB  BBB  BBB     BBB   BBB
C   BBBBBBBB   BBB   BBB  BBBBBBB   BBBBBB   BBB     BBB   BBB
C
C
C======================================================================
C======================================================================
C======================================================================
C===============             B H L U M I            ===================
C======================================================================
C======================================================================
C=============== MONTE CARLO FOR SMALL ANGLE BHABHA ===================
C===============            VERSION 2.01            ===================
C======================================================================
C====================      SEPTEMBER 1991     =========================
C======================================================================
C======================================================================
C=======================     AUTHORS      =============================
C====   S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was   ==========
C======================================================================
C======================================================================
C
C The complete description of the usage of the program can be found
C in the Long-write-up of BHLUMI 2.01, CERN preprint TH-6230,(ref. [1])
C to be submitted to Computer Physics Communications.
C
C The program contains three subgenerators:
C   (1)  BHLUM2
C    Multiphoton generator with Yennie-Frautschi-Suura first
C    order exponentiation based on refs. [1,2,3]
C   (2)  LUMLOG
C    Leading-Logarithmic generator with collinear emission of photons,
C    QED matrix element up to third order described in ref. [4]
C   (3)  OLDBIS
C    Modified version of the first order clasical generator OLDBAB,
C    see refs. [5] and [6]
C
C [1] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
C     TH-6230, sept. 1991
C [2] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
C     TH-6118, June 1991, Phys. Lett. in print.
C [3] S. Jadach and B.F.L. Ward,
C     Phys. Rev. D40 (1989) 3582.
C [4] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was
C     Phys. Lett. B260 (1991) 173, TH-5995.
C [5] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was
C     Phys. Lett. B253 (1991) 469, TH-5888.
C [6] F. Berends and R. Kleiss Nucl. Phys. B228 (1983) 537.
C
C                  IMPORTANT NOTE
C                  --------------
C The user is kindly requested to cite at least refs. [1-3]
C and any other ones from the above list if his study depends strongly
C on the particular subgenerator.
C
C----------------------------------------------------------------------
C                        INPUT and OUTPUT
C----------------------------------------------------------------------
C All input and output goes through parameters in
C                 CALL BHLUMI(MODE,XPAR,NPAR)
C and through /MOMSET/ and /WGTALL/ common blocks.
C In the following we shall  briefly indicate the meaning of the
C above parameters/variables.
C
C IF( MODE =-1 ) THEN
C ===================
C Initialisation is performed, all input parameters are transfered
C through XPAR and NPAR.
C The meaning of the entries in XPAR and NPAR depends on the type of
C the subgenerator: see tables in the source of BHLUM2, OLDBIS, LUMLOG
C and tables in the Long-write-up. In the following table we indicate
C parameters which have the same meaning for all three subgenerators
C----------------------------------------------------------------------
C  Entry    Variable   Meaning
C----------------------------------------------------------------------
C  NPAR( 1)  KEYOPT =1000*KEYGEN +10*KEYWGT +KEYRND
C                   General option switch
C            KEYGEN =3 for this sub-generator
C            KEYRND =1,2 type of random number generator RANMAR,RANECU
C            KEYWGT =0,1 for constant, variable weight WTM
C  NPAR( 2)  KEYRAD =KEYPIA is QED option switch defining WTM weight
C                   see tables in BHLUM2, LUMLOG and OLDBIS
C  XPAR( 1)  CMSENE Total center mass energy [GeV]
C  XPAR( 2)         see tables in BHLUM2, LUMLOG and OLDBIS
C  XPAR( 3)         see tables in BHLUM2, LUMLOG and OLDBIS
C  XPAR( 4)         see tables in BHLUM2, LUMLOG and OLDBIS
C  XPAR( 5)         see tables in BHLUM2, LUMLOG and OLDBIS
C  XPAR( 6)         see tables in BHLUM2, LUMLOG and OLDBIS
C----------------------------------------------------------------------
C
C ELSE IF( MODE = 0 ) THEN
C ========================
C Generation of the single Monte Carlo event
C The four momenta of the final state electron positron and photon
C are encoded in
C      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
C where P1 and Q1 are four-momenta of positron and elecron beams,
C P2 and Q2 are four-momenta of outgoing positron and electron,
C PHOT(100,4) contains list of photon four-momenta
C and NPHOT is the number of real photons in PHOT.
C For weighted events it may be profitable to use "paralel weights"
C from
C      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
C where WTMOD is the principal model weight and another useful weights
C representing some interesting version of the QED matrix element
C can be constructed as WT= WTCRU1*WTCRU2*WTSET(J).
C Which J is alowed and what version of the matrix element it
C represents depends on the type of subgenerator (BHLUM2,OLDBIS
C ,LUMLOG) and may be found in corresponding Tables of this source
C code  and in Long-write-up.
C
C ELSE IF( MODE = 1 ) THEN
C ========================
C The total cross section corresponding to generated series of event,
C i.e. resulting from MC integrartion is calculated and stored in XPAR
C and NPAR.
C In the table below we describe their most essential entries.
C For describtion of the other entries see tables in the source code
C of the subgenerators BHLUM2, LUMLOG and OLDBIS and in Long-write-up.
C----------------------------------------------------------------------
C  Entry    Variable   Meaning
C----------------------------------------------------------------------
C  NPAR(10)  NEVGEN  Number of generated MC events
C  NPAR(20)  NEVGEN  Number of generated MC events
C  XPAR(10)    XSEC  Total x-section [nb]
C  XPAR(11)   RXSEC  The relative (statistical) error of XSEC
C  XPAR(20)          Crude total MC x-section [nb] which is necessary
C                    for rescaling histograms in run
C                    with weighted events.
C  XPAR(21)          =0, error of XPAR(20) is zero
C----------------------------------------------------------------------
C For constant weight option KEYWGT=0 (convevience in rescaling histos)
C we put XPAR(20,21)=XPAR(10,11) !
C For MODE=1 program is called upon many times in the process of
C rescaling histograms and therefore no output is printed.
C
C ELSE IF( MODE = 2 ) THEN
C ========================
C Only in this MODE=2 in addition to filling XPAR and NPAR
C (as for MODE=1)
C the values of various x-sections are printed on the standard
C output file.
C
C ENDIF
C ====
C     ****************************************
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*80    BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      DIMENSION  XPAR(*),NPAR(*)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / BHPAR3 / KEYRAD,KEYOPT
 
      IF(MODE.EQ.-1) THEN
*     ===================
CBB   NINP= 15
CBB   NOUT= 16
CBB   NOUT2=16
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '   '
      WRITE(NOUT,BXTXT) 'BBB   B  B  B    B   B  B    B  B'
      WRITE(NOUT,BXTXT) 'B  B  B  B  B    B   B  BB  BB  B'
      WRITE(NOUT,BXTXT) 'BBB   BBBB  B    B   B  B BB B  B'
      WRITE(NOUT,BXTXT) 'B  B  B  B  B    B   B  B    B  B'
      WRITE(NOUT,BXTXT) 'BBB   B  B  BBBB  BBB   B    B  B'
      WRITE(NOUT,BXTXT) '   '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '*   BHLUMI version 2.01         *'
      WRITE(NOUT,BXTXT) '*   September      1991         *'
      WRITE(NOUT,BXTXT) '*         AUTHORS               *'
      WRITE(NOUT,BXTXT) '* S. Jadach, E. Richter-Was     *'
      WRITE(NOUT,BXTXT) '*    B.F.L. Ward, Z. Was        *'
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXCLO)
C
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) 'This program is based on papers '
      WRITE(NOUT,BXTXT) '--------------------------------'
      WRITE(NOUT,BXTXT) 'Long-write-up, sept.91 TH-6230  '
      WRITE(NOUT,BXTXT) 'P.L. (1991) in print,  TH-6118. '
      WRITE(NOUT,BXTXT) 'Phys. Rev. D40 (1989) 3582.     '
      WRITE(NOUT,BXTXT) 'P.L. B260 (1991) 173,  TH-5995. '
      WRITE(NOUT,BXTXT) 'P.L. B253 (1991) 469,  TH-5888. '
      WRITE(NOUT,BXTXT) 'Nucl. Phys. B228 (1983) 537.    '
      WRITE(NOUT,BXCLO)
C
      NEVG=0
      KEYOPT=NPAR(1)
      KEYGEN = MOD(KEYOPT,10000)/1000
      IF(KEYGEN.EQ.3) CALL BHLUM2(  -1,XPAR,NPAR)
      IF(KEYGEN.EQ.2) CALL LUMLOG(  -1,XPAR,NPAR)
      IF(KEYGEN.EQ.1) CALL OLDBIS(  -1,XPAR,NPAR)
 
      ELSEIF(MODE.EQ.0) THEN
*     ======================
      NEVG=NEVG+1
      IF(KEYGEN.EQ.3) CALL BHLUM2(   0,XPAR,NPAR)
      IF(KEYGEN.EQ.2) CALL LUMLOG(   0,XPAR,NPAR)
      IF(KEYGEN.EQ.1) CALL OLDBIS(   0,XPAR,NPAR)
C clean final state common blocks if necessary (safety reason)
      CALL BHCLEN
 
      ELSE
*     ====
      IF(KEYGEN.EQ.3) CALL BHLUM2(MODE,XPAR,NPAR)
      IF(KEYGEN.EQ.2) CALL LUMLOG(MODE,XPAR,NPAR)
      IF(KEYGEN.EQ.1) CALL OLDBIS(MODE,XPAR,NPAR)
      ENDIF
*     =====
      END
 
      SUBROUTINE BHCLEN
C     *****************
C This routine prevents user from using zero weight events
C and parellel weights when they should not be used!
C     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
 
C Parallel weights should not be used for constant weight events.
      IF(WTMOD.EQ.1D0) THEN
       DO 30 I=1,100
   30  WTSET(I)=0D0
C Clean final state momenta for events outside phase space
      ELSEIF(WTCRU1*WTCRU2 .EQ.0D0 )  THEN
       DO 10 K=1,4
       P2(K)=0D0
   10  Q2(K)=0D0
       NPHOT=0
       DO 15 J=1,100
       DO 15 K=1,4
   15  PHOT(J,K)=0D0
      ENDIF
      END
 
      SUBROUTINE BHLUM2(MODE,XPAR,NPAR)
C     *********************************
C
C           **************************************************
C           *       **********************************       *
C           *       *      *******************       *       *
C           *       *      *                 *       *       *
C           *       *      *   B H L U M 2   *       *       *
C           *       *      *                 *       *       *
C           *       *      *******************       *       *
C           *       **********************************       *
C           **************************************************
C
C=======================     AUTHORS      =============================
C====   S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was   ==========
C====================      SEPTEMBER 1991     =========================
C
C Main subprogram in MC multiphoton t-channel generator BHLUMI 2.01
C It is multiphoton generator with Yennie-Frautschi-Suura first
C order exponentiation based on refs. [1,2,3]
C According to ref. [2] this has OVERALL PRECISION 0.25%, see ref [2]
C for the validity range of the above statement.
C [1] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
C     TH-6230, sept. 1991
C [2] S. Jadach, E. Richter-Was, B.F.L. Ward  and Z. Was,
C     TH-6118, June 1991, Phys. Lett. in print.
C [3] S. Jadach and B.F.L. Ward,
C     Phys. Rev. D40 (1989) 3582.
C
C----------------------------------------------------------------------
C                 INPUT and OUTPUT of BHLUM2
C----------------------------------------------------------------------
C All input and output goes through parameters in
C                 CALL BHLUM2(MODE,XPAR,NPAR)
C and through /MOMSET/ and /WGTALL/ common blocks.
C In the following we shall  briefly indicate the meaning of the
C above parameters/variables.
C
C IF( MODE =-1 ) THEN
C ===================
C Initialisation is performed, all input parameters are transfered
C through XPAR and NPAR.
C In the following table we indicate the meaning of NPAR, XPAR
C entries for LUMLOG subgenerator in the initialization mode.
C      Table,           Input parameters of BHLUM2
C----------------------------------------------------------------------
C  Entry    Variable   Meaning
C----------------------------------------------------------------------
C  NPAR( 1)  KEYOPT =1000*KEYGEN +10*KEYWGT +KEYRND
C                    General option switch
C            KEYGEN =3 for this sub-generator
C            KEYRND =1,2 type of random number generator RANMAR,RANECU
C            KEYWGT =0,1 for constant, variable weight WTM
C  NPAR( 2)  KEYRAD =KEYPIA is QED option switch defining WTM weight
C            KEYPIA =0,1 photon vacuum polarization switched off, on
C  XPAR( 1)  CMSENE Total center mass energy [GeV]
C  XPAR( 2)   TRMIN Minimum transfer (positive) [GeV**2]
C  XPAR( 3)   TRMAX Maximum transfer (positive) [GeV**2]
C  XPAR( 4)   EPSCM Dimensionless infrared cut on CMS energy of soft
C                   photons, ( E_phot > CMSENE*EPSCM/2 )
C----------------------------------------------------------------------
C
C ELSE IF( MODE = 0 ) THEN
C ========================
C Generation of the single Monte Carlo event.
C The four momenta of the final state electron, positron and photon
C and of real photons are encoded in
C      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
C where P1 and Q1 are four-momenta of positron and elecron beams.
C P2 and Q2 are four-momenta of outgoing positron and electron.
C The list PHOT(100,4) four-momenta contains
C NPHOT four-momenta of real the photons, all in GeV units.
C The principal weight WTM of the event is placed in
C      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
C It is often of interest to use "paralel weights" from WTSET.
C The event weight is constructed then as WT= WTCRU1*WTCRU2*WTSET(J).
C Which J is alowed and what version of the QED matrix element
C it represents is summarized in the table below.
C (Note that using "parallel weights" makes only sense for calculation
C with variable weights, KEYWGT=1.)
C N.B. principal weight WTM= WTCRU1*WTCRU2*WTSET(2).
C              Table of WTSETS entries for BHLUM2
C----------------------------------------------------------------------
C  Entry      Type of QED calculation
C----------------------------------------------------------------------
C  WTSET( 1)  = WTSET(11)
C  WTSET( 2)  = WTSET(12)           (principal weight)
C             ---------------------------------------------------------
C             QED order   Vacuum pol.   Z-exchange    s-chan.gamma exch
C             ---------------------------------------------------------
C  WTSET(11)  Zero-th      Yes             Yes            Yes
C  WTSET(12)  First        Yes             Yes            Yes
C  WTSET(51)  Zero          No              No             No
C  WTSET(52)  First         No              No             No
C                           ----- Miscelanous ----
C  WTSET(20)  First        Yes              No             No
C  WTSET(21)  = WTSET(20)-WTSET(52)
C  WTSET(22)  First        Yes             Yes             No
C  WTSET(23)  = WTSET(22)-WTSET(20)
C  WTSET(24)  First        Yes             Yes            Yes
C  WTSET(25)  = WTSET(24)-WTSET(22)
C             ---------------------------------------------------------
C  WTSET(26)  Beta_0 component in WTSET(20)
C  WTSET(27)  Beta_1 component in WTSET(20),
C             i.e. WTSET(20)=WTSET(26)+WTSET(27)
C----------------------------------------------------------------------
C
C ELSE IF( MODE = 1 ) THEN
C ========================
C The total cross section corresponding to generated series of event,
C i.e. resulting from MC integrartion is calculated and stored in XPAR
C and NPAR, see table below.
C----------------------------------------------------------------------
C  Entry    Variable   Meaning
C----------------------------------------------------------------------
C  NPAR(10)  NEVGEN  Number of generated MC events
C  NPAR(20)  NEVGEN  Number of generated MC events
C  XPAR(10)   XMCNB  Total x-section [nb]
C  XPAR(11)    EREL  The relative error of XPAR(10)
C  XPAR(12)     XMC  Total x-section [GEV**-2]
C  XPAR(20)  SIG0NB  Crude total MC x-section [nb] which is necessary
C                    for rescaling histograms in run with
C                    weighted events.
C  XPAR(21)          =0, error of XPAR(20) is zero
C  XPAR(20)    SIG0  Crude x-sectio as XPAR(20) but in [GeV**-2]
C----------------------------------------------------------------------
C For constant weight option KEYWGT=0 (convevience in rescaling histos)
C we put XPAR(20,21,22)=XPAR(10,11,12) !
C For MODE=1 program is called upon many times in the process of
C rescaling histograms, therefore, there is no output printed
C in this mode.
C
C ELSE IF( MODE = 2 ) THEN
C ========================
C Only in this MODE=2 in addition to filling XPAR and NPAR as for
C MODE=1 the values of various x-sections are printed on standard
C output file.
C
C ENDIF
C ====
C
C modifications HISTORY (stj)
C 12 jan. 91 - kicking out the interference started
C 21 jan. 91 - writing model2 new matrix element
c 23 feb. 91 - kicking out original model completely
C 12 Feb. 91 - changing random generators
c 20 March 91 - removal of photons etc.
c  5 April new trigger
C     *********************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      PARAMETER( ALFPI=  1D0/PI/ALFINV ,ALFA=1D0/ALFINV)
      PARAMETER( GNANOB=389.385D-30*1.D33 )
      CHARACTER*80   BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      DIMENSION  XPAR(*), NPAR(*)
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
* CMONIT COMMUNICATES WITH WMONIT
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / MOMSET / PX1(4),QX1(4),PX2(4),QX2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      DIMENSION MARKP(100)
      DOUBLE PRECISION DRVEC(100)
 
      IF(MODE.EQ.-1) THEN
*     ===================
      CALL FILBH2(XPAR,NPAR)
      SVAR=CMSENE**2
CBB   WTMAX= 2.8D0     
      WTMAX = XPAR(7)
C-- maximum trnsfer for photon angular distributions
C-- TRMX2=svar is a safe choice, for low  angles
C-- (thmin,thmax << 10mrad)
C-- it can be lowered to speed up generation (but with great care).
      TRMX2 = TRMAX
C (over)conservative and safe choice is TRMX2=SVAR
      TRMX2 = SVAR
      IF(TRMX2.GT.SVAR) TRMX2=SVAR
      EMIN = CMSENE/2D0*EPSCM
* WEIGHT MONITORING INITIALIZATION
      IDA=0
      DO 11 I=IDA+1,IDA+20
  11  CALL WMONIT(  -1, I,DUMM1,DUMM2,DUMM3)
      IDC = 90
      DO 12 I=IDC+1,IDC+9
  12  CALL WMONIT(  -1, I,DUMM1,DUMM2,DUMM3)
C=====(((  ===>>>
CBB  set limits of histos to single prec instead of double prec...
      CALL HBOOK1(9001,' BHLUM2, WEIGHT DISTRIBUTION ',40,-1.,7.,0.)
      CALL HBOOK1(9002,' BHLUM2, WTLL WEIGHT         ',40,-1.,7.,0.)
C=====)))
      KEYWGT = MOD(KEYOPT,100)/10
      IEVENT=0
      NEVGEN=0
      ELSEIF(MODE.EQ.0) THEN
*     ======================
      NEVGEN = NEVGEN+1
  200 CONTINUE
      IEVENT=IEVENT+1
      WT1=0D0
      WT2=0D0
      WT3=0D0
      WTKIN=0D0
      WTMSS=0D0
      CALL VARRAN(DRVEC,2)
C--------- generate t-channel transfer (true one) ---------
      RN1 = DRVEC(1)
      TRAN= 1D0 / (  RN1/TRMIN   +(1D0-RN1)/TRMAX )
C--------------------  Photon generation ------------------
      CALL MLTIBR(TRAN,TRMX2,AMEL,DEL,
     $   NPHOT1,PHOT1,PHSU1,AL1,BE1,TRANP,AMSP,MK1,WKP,WTM1)
      CALL MLTIBR(TRAN,TRMX2,AMEL,DEL,
     $   NPHOT2,PHOT2,PHSU2,AL2,BE2,TRANQ,AMSQ,MK2,WKQ,WTM2)
      IF(WKP*WKQ.EQ.0D0) GOTO 140
C-- construct fermions, transform photons and fermions to CMS frame
      CALL KINO4(SVAR,TRAN,AMEL,AMSP,AMSQ,WT3)
      CALL WMONIT(   0,IDA+3,  WT3, 1D0,5D-4)
      IF(WT3.EQ.0D0) GOTO 140
C-- beyond this point only events conserving four-momentum
      WTKIN=1D0
C-- manipulations on mass weights, removal of soft photons
      CALL PIATEK(CMSENE,TRMX2,AMEL,EMIN,DEL,
     $      NPHOT1,P1,P2,PHOT1,PHSU1,WTM1,WT1,WTMR1,WCTA1,WCTB1)
      CALL PIATEK(CMSENE,TRMX2,AMEL,EMIN,DEL,
     $      NPHOT2,Q1,Q2,PHOT2,PHSU2,WTM2,WT2,WTMR2,WCTA2,WCTB2)
C-- remooving photons < epsCM form the record
      CALL REMPHO(EMIN,NPHOT1,PHOT1,AL1,BE1,WTM1,MK1)
      CALL REMPHO(EMIN,NPHOT2,PHOT2,AL2,BE2,WTM2,MK2)
      WTM1T2 = WTMR1*WTMR2
      WT1= WTMR1*WKP
      WT2= WTMR2*WKQ
C---------- monitoring control weights
      CALL WMONIT(   0,IDC+1,       WCTA1, 1D0,5D-4)
      CALL WMONIT(   0,IDC+2,       WCTA2, 1D0,5D-4)
      CALL WMONIT(   0,IDC+3, WCTA1*WCTA2, 1D0,5D-4)
      CALL WMONIT(   0,IDC+4,       WCTB1, 1D0,5D-4)
      CALL WMONIT(   0,IDC+5,       WCTB2, 1D0,5D-4)
      CALL WMONIT(   0,IDC+6, WCTB1*WCTB2, 1D0,5D-4)
C----------- FORMFACTOR
      FPHS  = EXP( 4D0*ALFPI* LOG(TRMX2/AMEL**2)* LOG(1D0/DEL ) )
      PDEL = DEL*BCUD(P1,P2,PHSU1)
      QDEL = DEL*BCUD(Q1,Q2,PHSU2)
      FYFS3= EXP( ALFPI*(
     $  -2D0*(DLOG(TRANP/AMEL**2)-1)*DLOG(1D0/PDEL)
     $  -2D0*(DLOG(TRANQ/AMEL**2)-1)*DLOG(1D0/QDEL)
     $  +0.5D0*DLOG(TRANQ/AMEL**2)  -1D0
     $  +0.5D0*DLOG(TRANP/AMEL**2)  -1D0
     $    ))
      WT4 = FPHS * FYFS3
      CALL WMONIT(   0,IDA+4,WT4,  1D0,5D-4)
C---------------------- MODEL ----------------------------------
      CALL MODEL1(1)
      WT5 = WTSET(2)
C------------------- TOTAL ----- WEIGHT ------------------------
  140 CONTINUE
      WT  = WT1*WT2*WT3*   WT4   *WT5
      WTLL= WT1*WT2*WT3*   WT4   *WTSET(62)
C-- monitoring model weight
      CALL WMONIT(   0,IDA+1, WTM1T2,  2D0,5D-4)
      CALL WMONIT(   0,IDA+20,WT,WTMAX,RN)
      CALL WMONIT(   0,IDA+19,WT-WTLL,WTMAX,RN)
      WTOVR=MAX(0D0,WT-WTMAX)
      CALL WMONIT(   0,IDA+18,  WTOVR,0D0,0D0)
C=====((( ===>>>
CBB use single prec variables as args of HF1
      CALL HF1( 9001, REAL(WT),1.0)
      CALL HF1( 9002, REAL(WTLL),1.0)
C=====)))
C ...Rejection according to principal weight
      IF(KEYWGT.EQ.0) THEN
C ...unweihgted events with WT=1
            RN = DRVEC(2)
            IF(WT.LT.RN*WTMAX) GOTO 200
            WTMOD=1.D0
C ...WTCRU1,2  weights are RESET to zero for control
            WTCRU1=0D0
            WTCRU2=0D0
         ELSE
C ...weighted events
            WTCRU1=WT1*WT2
            WTCRU2=WT3*WT4
            WTMOD  = WTCRU1*WTCRU2*WT5
         ENDIF
C-- merge photons/fermion into one common block
      CALL MERGIK
      ELSE
*     ====
      NPAR(10)= NEVGEN
      NPAR(20)= NEVGEN
      SIG0= 4D0*PI*ALFA**2 *(1D0/TRMIN-1D0/TRMAX)
      SIG0NB=SIG0*GNANOB
      CALL WMONIT(   1,IDA+20,AWTOT,DWTOT,WWMX )
      XSMC   = SIG0  *AVERWT
      XSMCNB = SIG0NB*AVERWT
      EREL   = ERRELA
      ERMC   = XSMCNB*EREL
      XPAR(10) = XSMCNB
      XPAR(11) = EREL
      XPAR(12) = XSMC
      IF(KEYWGT.EQ.0) THEN
C ...WT=1  events, normal option...
         XPAR(20)=XSMCNB
         XPAR(21)=EREL
         XPAR(22)=XSMC
      ELSE
C ...Weighted events, additional information on x-sections
         XPAR(20)= SIG0NB
         XPAR(21)= 0D0
         XPAR(22)= SIG0
      ENDIF
* printout only for MODE=2
      IF(MODE.EQ.1) RETURN
C=====(((
C      CALL HPRINT(9001)
C      CALL HPRINT(9002)
C=====)))
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHLUM2:        WINDOW A        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL1I) NEVGEN,     ' Accepted total    ','NEVGEN','A1'
      WRITE(NOUT,BXL1I) IEVENT,     ' Raw prior reject. ','IEVENT','A2'
      WRITE(NOUT,BXL2G) XSMCNB,ERMC,' Xsec M.C. [nb]    ','XSECMC','A3'
      WRITE(NOUT,BXL1F) EREL,       ' relat. error      ','ERELMC','A4'
      WRITE(NOUT,BXL2F) AWTOT,DWTOT,' weight  M.C.      ','AWT   ','A5'
      WRITE(NOUT,BXL1I) NEVNEG,     ' WT<0              ','NEVNEG','A6'
      WRITE(NOUT,BXL1I) NEVOVE,     ' WT>WTMAX          ','NEVOVE','A7'
      WRITE(NOUT,BXL1F) WTMAX ,     ' Maximum WT        ','WWMX  ','A8'
      WRITE(NOUT,BXCLO)
* PRINT ADDITIONAL INFOS
*------------------------------------------------------------
      CALL WMONIT(   1,IDA+1, AWT1 ,DWT1 ,DUMM3)
      CALL WMONIT(   1,IDA+2, AWT2 ,DWT2 ,DUMM3)
      CALL WMONIT(   1,IDA+3, AWT3 ,DWT3 ,DUMM3)
      CALL WMONIT(   1,IDA+4, AWT4 ,DWT4 ,DUMM3)
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHLUM2:        WINDOW B        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL2F) AWT1,DWT1  ,'WT1*WT2*T/TP*T/TQ  ','      ','B1'
      WRITE(NOUT,BXL2F) AWT3,DWT3  ,'WT3 from KINO4     ','      ','B2'
      WRITE(NOUT,BXL2F) AWT4,DWT4  ,'YFS formfac        ','WT4   ','B4'
      WRITE(NOUT,BXL2F) AWTOT,DWTOT,'TOTAL              ','      ','B5'
      CALL WMONIT(   1,IDA+19, AWT19 ,RWT19 ,DUMM3)
      DWT19=AWT19*RWT19
      WRITE(NOUT,BXL2F) AWT19,DWT19,'WT-WTLL            ','WT5   ','B6'
      CALL WMONIT(   1,IDA+18, AWT18 ,RWT18 ,DUMM3)
      XWT18 = AWT18/AWTOT
      DWT18 = XWT18*RWT18
      WRITE(NOUT,BXL2F) XWT18,DWT18,'xsec/xtot: WT>WTMAX','WT5   ','B7'
      WRITE(NOUT,BXCLO)
* ---------------------------------------------------------------
      CALL WMONIT( 1,IDC+1,AWT1,DWT1,DUMM3)
      CALL WMONIT( 1,IDC+2,AWT2,DWT2,DUMM3)
      CALL WMONIT( 1,IDC+3,AWT3,DWT3,DUMM3)
      CALL WMONIT( 1,IDC+4,AWT4,DWT4,DUMM3)
      CALL WMONIT( 1,IDC+5,AWT5,DWT5,DUMM3)
      CALL WMONIT( 1,IDC+6,AWT6,DWT6,DUMM3)
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '            WINDOW C             '
      WRITE(NOUT,BXTXT) 'Built-in average control weights.'
      WRITE(NOUT,BXTXT) 'Should equal one +- statist. err.'
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL2F) AWT1,DWT1  ,'<WCTA1>            ','      ','C1'
      WRITE(NOUT,BXL2F) AWT2,DWT2  ,'<WCTA2>            ','      ','C2'
      WRITE(NOUT,BXL2F) AWT3,DWT3  ,'<WCTA1*WCTA2>      ','      ','C3'
      WRITE(NOUT,BXL2F) AWT4,DWT4  ,'<WCTB1>            ','      ','C4'
      WRITE(NOUT,BXL2F) AWT5,DWT5  ,'<WCTB2>            ','      ','C5'
      WRITE(NOUT,BXL2F) AWT6,DWT6  ,'<WCTB1*WCTB2>      ','      ','C6'
      WRITE(NOUT,BXCLO)
      ENDIF
*     =====
      END
 
      SUBROUTINE FILBH2(XPAR,NPAR)
*     ****************************
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  XPAR(*), NPAR(*)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      CHARACTER*80   BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR1 / DEL,EPSCM,THMIN,XMIVIS
C Communicates with VARRAN
      COMMON / RANPAR / KEYRND
      COMMON / INOUT  / NINP,NOUT,NOUT2
C--
      CMSENE=XPAR(1)
      TRMIN =XPAR(2)
      TRMAX =XPAR(3)
      EPSCM =XPAR(4)
      KEYOPT=NPAR(1)
      KEYRAD=NPAR(2)
      KEYRND = MOD(KEYOPT,10)
      KEYWGT = MOD(KEYOPT,100)/10
      KEYPIA = MOD(KEYRAD,10)
C--
      AMEL  =  0.5111D-3
      SVAR  = CMSENE**2
      XIMIN = TRMIN/SVAR
      XIMAX = TRMAX/SVAR
C+++      DEL   = EPSCM*0.01D0
      DEL   = EPSCM*XIMIN
      IF(TRMAX.GT.SVAR) TRMAX=SVAR
      REPI1 = REPI(-TRMIN)
      REPI2 = REPI(-TRMAX)
C--
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '  BHLUM2: INPUT PARAMETRES       '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL1I) KEYOPT,     ' options   switch  ','KEYOPT','N1'
      WRITE(NOUT,BXL1I) KEYRND,     ' VARRAN    switch  ','KEYRND','  '
      WRITE(NOUT,BXL1I) KEYWGT,     ' weighting switch  ','KEYWGT','  '
      WRITE(NOUT,BXL1I) KEYRAD,     ' radiation switch  ','KEYRAD','N2'
      WRITE(NOUT,BXL1I) KEYPIA,     ' vac_pol   switch  ','KEYPIA','  '
      WRITE(NOUT,BXL1F) CMSENE,     ' CMS enegry  [GeV] ','CMSENE','X1'
      WRITE(NOUT,BXL1G) SVAR  ,     ' CMSENE^2  [GeV^2] ','SVAR  ','  '
      WRITE(NOUT,BXL1G) TRMIN ,     ' trasf_min [GeV^2] ','TRMIN ','X2'
      WRITE(NOUT,BXL1G) TRMAX ,     ' trasf_min [GeV^2] ','TRMAX ','X3'
      WRITE(NOUT,BXL1G) XIMIN ,     ' xi_min=TRMIN/SVAR ','XIMIN ','  '
      WRITE(NOUT,BXL1G) XIMAX ,     ' xi_max=TRMAX/SVAR ','XIMAX ','  '
      THMIN  = ACOS(1-2*XIMIN)*1000D0
      THMAX  = ACOS(1-2*XIMAX)*1000D0
      WRITE(NOUT,BXL1F) THMIN ,     ' theta_min  [mrad] ','THMIN ','  '
      WRITE(NOUT,BXL1F) THMAX ,     ' theta_max  [mrad] ','THMAX ','  '
      THMIN  = ACOS(1-2*XIMIN)*180/PI
      THMAX  = ACOS(1-2*XIMAX)*180/PI
      WRITE(NOUT,BXL1F) THMIN ,     ' theta_min   [deg] ','THMIN ','  '
      WRITE(NOUT,BXL1F) THMAX ,     ' theta_max   [deg] ','THMAX ','  '
      WRITE(NOUT,BXL1F) EPSCM ,     ' eps_CM infr. cut  ','EPSCM ','X4'
      WRITE(NOUT,BXL1F) DEL   ,     ' delta  infr. cut  ','DEL   ','  '
      WRITE(NOUT,BXL1F) REPI1 ,     ' RePi(transf_min)  ','      ','  '
      WRITE(NOUT,BXL1F) REPI2 ,     ' RePi(transf_max)  ','      ','  '
      WRITE(NOUT,BXCLO)
      END
 
      SUBROUTINE REMPHO(EMIN,NPHOT,PHOT,ALF,BET,WTM,MK)
*     *************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PHOT(100,4),ALF(50),BET(50),WTM(50),MK(50)
      NPH=NPHOT
      DO 100 J=NPHOT,1,-1
      IF(PHOT(J,4).LT.EMIN) THEN
         DO 60 I=J+1,NPH
         ALF(I-1)=ALF(I)
         BET(I-1)=BET(I)
         WTM(I-1)=WTM(I)
         MK( I-1)=MK( I)
         DO 60 K=1,4
   60    PHOT(I-1,K)=PHOT(I,K)
         NPH=NPH-1
      ENDIF
  100 CONTINUE
      NPHOT=NPH
      END
 
      SUBROUTINE PIATEK(CMSENE,TRMAX,AMEL,EMIN,DELTA,
     $         NPHOT,P1,P2,PHOT,PHSU,WMAT,WTAL,WTMRE,WCTR1,WCTR2)
C     ***************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932D0,ALFINV=137.03604D0)
      PARAMETER(ALF1=1/ALFINV/PI )
      DIMENSION P1(4),P2(4),WMAT(50),PHOT(100,4),PHSU(4)
      DATA ICONT /0/
      ICONT=ICONT+1
C Calculate mass weight for all photos and separately for
C photons with energy below/above EMIN
      EPSCM = 2*EMIN/CMSENE
      WTAL = 1D0
      WTM1 = 1D0
      WTM2 = 1D0
      WTEPSP=1D0
      DO 100 I=1,NPHOT
      WTAL = WTAL*WMAT(I)
      IF(WTAL.LT.1D-15) WTAL =0D0
      IF(PHOT(I,4).LT.EMIN) THEN
        WTM1 = WTM1*WMAT(I)
        IF(WTM1.LT.1D-15) WTM1=0D0
        WTEPSP = 0D0
      ELSE
        WTM2=WTM2*WMAT(I)
        IF(WTM2.LT.1D-15) WTM2=0D0
      ENDIF
  100 CONTINUE
C Control weight for delta-->epsilon rejection
      DELT1 = DELTA*BCUD(P1,P2,PHSU)
      CALL WFORM(TRMAX,P1,P2,AMEL,DELT1,EMIN,PDYFS)
      WCTR1 = WTEPSP*PDYFS
C control weight for photons below EMIN remooval
      TRANP = 2D0*(P1(4)*P2(4)-P1(3)*P2(3)-P1(2)*P2(2)-P1(1)*P2(1))
      EPS1  =  SQRT(EMIN**2/P1(4)/P2(4))
      DELB2 = -2*ALF1*(DLOG(TRMAX/TRANP)+1) *DLOG(EPS1/DELT1)
      WCTR2 = WTM1*EXP(-DELB2)
C In the case of remooval the new mass weight is this
      WTMRE = WTM2*EXP(DELB2)
      END
 
      SUBROUTINE WFORM(TRMAX,Q1,Q2,AMF,DELTA,EMIN,DYFS)
C     *************************************************
C For tests only.
C Yennie-Frautschi-Suura Formfactors for the single fermion pair
C This is for crude distribition before mass wights
C The triangle effect included (pi**2/6)
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(PI=3.1415926535897932D0,ALFINV=137.03604D0)
      PARAMETER(ALF1=1/ALFINV/PI )
      DIMENSION Q1(4),Q2(4)
C ...Momenta q1,q2 should be in CMS
      Q1Q2  = Q1(4)*Q2(4)-Q1(3)*Q2(3)-Q1(2)*Q2(2)-Q1(1)*Q2(1)
      E1 = Q1(4)
      E2 = Q2(4)
      BETF2 = 2*ALF1* DLOG(TRMAX /AMF**2)
      DELB  = BETF2*DLOG(EMIN/SQRT(E1*E2)/DELTA)
      EP    = E1+E2
      EM    = E1-E2
      DL    = SQRT( 2*Q1Q2 +EM**2 )
C Note that approximately REMN= +(1./6.)*PI**2 for t-channel
      REMN  = PI**2/2
     $        -0.50*DLOG(E1/E2)**2
     $        -0.25*DLOG((DL+EM)**2/(4*E1*E2))**2
     $        -0.25*DLOG((DL-EM)**2/(4*E1*E2))**2
     $        - DILOG((DL+EP)/(DL+EM)) -DILOG((DL-EP)/(DL-EM))
     $        - DILOG((DL-EP)/(DL+EM)) -DILOG((DL+EP)/(DL-EM))
C This (alf/pi)*pi**2/6 is related to replacing (y+z)>epsilon
C by max(y,z)>epsilon.   (Rejection delta=> epsilon over-estimated)
      TRIANG = -PI**2/6D0
      DYFS   = EXP( DELB +ALF1*REMN +ALF1*TRIANG)
      END
 
      FUNCTION BCUD(P1,P2,SF)
*     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P1(4),P2(4),SF(4)
      XPP  = P2(4)*P1(4)-P2(3)*P1(3)-P2(2)*P1(2)-P2(1)*P1(1)
      XPR  = P1(4)*(P2(4)+SF(4)) - P1(3)*(P2(3)+SF(3))
     $     - P1(2)*(P2(2)+SF(2)) - P1(1)*(P2(1)+SF(1))
      BCUD= XPR/XPP
      END
 
      SUBROUTINE MLTIBR(TRAN,TRMAX,AMEL,DEL,
     $      NPH,PHOT,PHSU,ALF1,BET1,TRANP,AMSP,MK,WT1,WTM)
*     ****************************************************
* THIS PROVIDES MOMENTA OF PHOTONS IN A FERMION PROPER FRAME
C INPUT : TRAN    = PRINCIPAL T-CHANNEL TRANSFER     (GEV**2)
C         TRMAX   = max. transf. (>0) for angular phot. dist. [GEV**2]
C         AMEL    = ELECTRON ENERGY         (GEV)
C         DEL     = LOW ENERGY PHOTON LIMIT   (DIMENSIONLESS)
C OUTPUT: NPH     = PHOTON MULTIPLICITY
C         PHOT    = LIST OF PHOTON FOUR-MOMENTA
C         PHSU    = SUM OF PHOTON MOMENTA
C         ALF1,BET1   = SUDAKOV VARIABLES
C         TRANP   = (P2-P1)**2
C         AMSP    = (P2+PHSU)**2
C         MK      = MARKED PHOTONS
C         WT1     = TRANP/TRAN is Jacobian, =0 outside ph.sp.
C         WTM     = LIST OF MASS WEIGHTS
*     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (PI=3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER (NMAX=20)
      DIMENSION PHOT(100,4),PHSU(4),PH(4)
      DIMENSION MK(50),WTM(50)
      DIMENSION ALF(50),BET(50),ALF1(50),BET1(50),Y(50),Z(50)
      DIMENSION RR(100),P2(4)
      DOUBLE PRECISION DRVEC(100)
      DATA ICONT /0/
      ICONT=ICONT+1
 
      DELS  = AMEL**2/TRMAX
      BILGS = LOG(TRMAX/AMEL**2)
      DELL  = LOG(DEL)
      WT1    = 1D0
      DO  11 I=1,50
      DO  11 K=1,4
   11 PHOT(I,K)=0D0
      AVERG=2D0/(PI*ALFINV)*BILGS*LOG(1D0/DEL)
      CALL POISSG(AVERG,NMAX,NPH,RR)
      IF(NPH.GT.NMAX) GOTO 900
* NO PHOTON
      DO 45 K=1,4
   45 PHSU(K)=0D0
      IF(NPH.EQ.0) THEN
        TRANP=TRAN
      ELSE
* ONE OR MORE PHOTONS
   50   CALL VARRAN(DRVEC,NPH)
        BSUM=0D0
        DO 80 I=1,NPH
* WE DEFINE R=LOG(MAX(YGR,ZET))
        R=DELL*RR(I)
* PHOTONS CLOSE TO LOWER INFRARED BOUNDRY ARE MARKED FOR TESTS
        MK(I)=0
        IF(EXP(R).LT.DEL*3D0) MK(I)=1
        T= 2D0*DRVEC(I)
        IF(T.GT.1D0) THEN
           YGR=R
           ZET=R-(T-1D0)*BILGS
        ELSE
           ZET=R
           YGR=R-T*BILGS
        ENDIF
        YGR=EXP(YGR)
        ZET=EXP(ZET)
        Y(I)=YGR
        Z(I)=ZET
* DEFINE ALPHA AND BETA (PRIM)
        ALF1(I)=YGR-ZET*DELS
        BET1(I)=ZET-YGR*DELS
        IF(ALF1(I).LE.0D0.OR.BET1(I).LE.0D0) GOTO 50
   80   BSUM=BSUM+BET1(I)
        IF(BSUM.GT.1D0) GOTO 900
* RESCALE ALPHA AND BETA
        CALL VARRAN(DRVEC,NPH)
        DO 90 I=1,NPH
        ALF(I)=ALF1(I)/(1D0-BSUM)
   90   BET(I)=BET1(I)/(1D0-BSUM)
* DEFINE PHOTON FOUR MOMENTA IN SQRT(TRANP)/2 UNITS
        DO 100 I=1,NPH
        PHOT(I,4)= ALF(I)+BET(I)
        PHOT(I,3)=-ALF(I)+BET(I)
        R1 = DRVEC(I)
        PHI=2D0*PI*R1
        PTRAN=2D0*DSQRT(DABS(ALF(I)*BET(I)))
        PHOT(I,1)=PTRAN*COS(PHI)
        PHOT(I,2)=PTRAN*SIN(PHI)
        DO 100 K=1,4
  100   PHSU(K)=PHSU(K)+PHOT(I,K)
* DEFINE FACTOR FOR RESCALING PHOTON MOMENTA
        XMK2=PHSU(4)**2-PHSU(3)**2-PHSU(2)**2-PHSU(1)**2
        YY2=1D0/(1D0+PHSU(3)-.25D0*XMK2)
* YY2 NEGATIVE WHEN OUTSIDE PHASE SPACE (ABS(T)>ABS(S))
        IF(YY2.LE.0D0) GOTO 900
        TRANP=TRAN*YY2
        ENER =SQRT(TRANP)/2D0
* RESCALE ALL PHOTON MOMENTA
        DO 120 K=1,4
        PHSU(K)=PHSU(K)*ENER
        DO 120 I=1,NPH
  120   PHOT(I,K)=PHOT(I,K)*ENER
* THIS ROTATION MAKES PHSU(2)=0
        PSIT=ANGFI(PHSU(1),PHSU(2))
        CALL ROTOD3(-PSIT, PHSU,PHSU)
        DO 140 I=1,NPH
        DO 135 K=1,4
  135   PH(K)=PHOT(I,K)
        CALL ROTOD3(-PSIT, PH,PH)
        DO 140 K=1,4
  140   PHOT(I,K)=PH(K)
      ENDIF
c+++      IF(TRANP.EQ.0D0) GO TO 900
      IF(TRANP.LE. 4*AMEL**2) GO TO 900
      BETEL=SQRT(1D0-4D0*AMEL**2/(TRANP+4D0*AMEL**2))
      P2(3)=SQRT(TRANP)/2D0
      P2(4)=SQRT(P2(3)*P2(3)+AMEL**2)
      P2(2)=0D0
      P2(1)=0D0
      AMSP=(P2(4)+PHSU(4))**2-(P2(3)+PHSU(3))**2
     $    -(P2(2)+PHSU(2))**2-(P2(1)+PHSU(1))**2
* AND THE WEIGHT FINALLY
      WT1 = TRANP/TRAN
      DELT=AMEL**2/TRANP
      DO 200 I=1,NPH
* GENERATED DISTRIBUTION
C here some numerical regularization
      DIST0 = 1D0/((ALF1(I)+DELS*BET1(I))*(BET1(I)+DELS*ALF1(I)))
      YGR=ALF1(I)+DELT*BET1(I)
      ZET=BET1(I)+DELT*ALF1(I)
* DESIRED DISTRIBUTION = SOFT FACTOR
      DIST1 = ALF1(I)*BET1(I)/(YGR*ZET)**2
      WTM(I)= DIST1/DIST0
  200 CONTINUE
      RETURN
C EVENT OUTSIDE PHASE SPACE
  900 WT1   =0D0
      TRANP =0D0
      NPHOT =-1
      END
 
      SUBROUTINE POISSG(AVERG,NMAX,MULT ,RR)
C     **************************************
* DIFFERS FROM THAT IN EXPAND DEC. 87
* THIS GENERATES PHOTON MULTIPLICITY ACCORDING TO POISSON DISTRIBUTION
* INPUT:  AVERG = AVERAGE MULTIPLICITY
*         NMAX  = MAXIMUM MULTIPLICITY
* OUTPUT: MULT  = GENERATED MULTIPLICITY
*         RR(1:100) LIST OF ORDERED UNIFORM RANDOM NUMBERS,
*         A BYPRODUCT RESULT, TO BE EVENTUALLY USED FOR SOME FURTHER
*         PURPOSE (I.E.  GENERATION OF PHOTON ENERGIES).
*     **************************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 RR(100)
      DOUBLE PRECISION DRVEC(100)
      DATA NFAIL/0/
   50 NN=0
      IF(NMAX.GT.100) GOTO 900
      CALL VARRAN(DRVEC,NMAX)
      SUM=0D0
      DO 100 IT=1,NMAX
      RN = DRVEC(IT)
      Y= LOG(RN)
      SUM=SUM+Y
      NN=NN+1
      IF(SUM.LT.-AVERG) GOTO 130
      RR(NN)=SUM/(-AVERG)
  100 CONTINUE
      NFAIL=NFAIL+1
      IF(NFAIL.GT.100) GOTO 900
      GOTO 50
  130 MULT =NN-1
      RETURN
  900 WRITE(6,*) ' POISSG: TO SMALL OR TO BIG NMAX',NMAX
      STOP
      END
 
* KINEMATICS, CNSTRUCTION OF MOMENTA IN CMS
      SUBROUTINE KINO4(SVAR,TRAN,AMEL,AMSP,AMSQ,WTKK)
*     ************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      DOUBLE PRECISION DRVEC(100)
      REAL*8 PH(4)
      DIMENSION SUM(4),QCM(4)
      DATA PI /3.1415926535897932D0/
      BTEL=DSQRT(1D0-4D0*AMEL**2/SVAR)
      WTKK=1D0
      CALL VARRAN(DRVEC,3)
      PSI1= 2D0*PI*DRVEC(1)
      PSI2= 2D0*PI*DRVEC(2)
      PHI = 2D0*PI*DRVEC(3)
* UPPER VERTEX: TRANSF. FROM P2-P1 PROPER FRAME
      CALL KLIPER(TRANP,AMEL,PHSU1,P2,TH1,EXT1,EXB1)
* LOWER VERTEX: TRASCF. FROM Q2-Q1 PROPER FRAME
      CALL KLIPER(TRANQ,AMEL,PHSU2,Q2,TH2,EXT2,EXB2)
* DEFINE P1, Q1  IN CENTRAL QMS
      P1(3)= -(TRAN+AMSP-AMEL**2)/SQRT(TRAN)/2D0
      Q1(3)=  (TRAN+AMSQ-AMEL**2)/SQRT(TRAN)/2D0
      RPQK=(Q1(3)+P1(3))/DSQRT(SVAR)
C     PX2=SVAR*(SVAR+4D0*P1(3)*Q1(3))/((Q1(3)+P1(3))**2+SVAR)/4D0
C     PX2=PX2-AMEL**2
      GPQK= P1(3)-Q1(3)
      PX2=(BTEL**2*SVAR*(1D0+RPQK*RPQK)-GPQK*GPQK)/(1D0+RPQK*RPQK)/4D0
      IF(PX2.LE.0D0)  GOTO 900
      PX=SQRT(PX2)
      P1(2)=  0D0
      Q1(2)=  0D0
      P1(1)=  -PX
      Q1(1)=   PX
      P1(4)=  SQRT(P1(1)**2+P1(2)**2+P1(3)**2+AMEL**2)
      Q1(4)=  SQRT(Q1(1)**2+Q1(2)**2+Q1(3)**2+AMEL**2)
* CORRECTING FOR ELECTRON MASS
C     BETP = SQRT(1D0-(AMEL/P1(4))**2)
C     BETQ = SQRT(1D0-(AMEL/Q1(4))**2)
C     DO 7 K=1,3
C     P1(K)=BETP* P1(K)
C   7 Q1(K)=BETQ* Q1(K)
      EXW1=SQRT((P1(4)+P1(1))/(P1(4)-P1(1)))
      EXW2=SQRT((Q1(4)+Q1(1))/(Q1(4)-Q1(1)))
* CONSTRUCT MOMENTUM TRANSFER Q IN CMS
      QCM(4)=(AMSP-AMSQ)/SQRT(SVAR)/2D0
      QMOD=SQRT(TRAN+QCM(4)**2)
      QCM(3)=(-TRAN-AMSP/2D0-AMSQ/2D0+AMEL**2)/SQRT(SVAR-4D0*AMEL**2)
      QCM(2)=0D0
      QCM(1)=SQRT(QMOD**2-QCM(3)**2)
      FIF =ANGFI(QCM(3),QCM(1))
      EXE2=SQRT((QMOD+QCM(4))/(QMOD-QCM(4)))
 
* FINAL SET OF TRANSFORMATIONS FROM QMSP AND QMSQ TO CMS
* FIRST BRANCH
      CALL  PTRAL(TH1,EXT1,EXB1,PSI1,EXW1,EXE2,FIF,PHI,P2)
      IF(NPHOT1.NE.0) THEN
       CALL PTRAL(TH1,EXT1,EXB1,PSI1,EXW1,EXE2,FIF,PHI,PHSU1)
       DO 20 I=1,NPHOT1
       DO 15 K=1,4
   15  PH(K)=PHOT1(I,K)
       CALL PTRAL(TH1,EXT1,EXB1,PSI1,EXW1,EXE2,FIF,PHI,PH)
       DO 16 K=1,4
   16  PHOT1(I,K)=PH(K)
   20  CONTINUE
      ENDIF
* SECOND BRANCH
      CALL  QTRAL(TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI,Q2)
      IF(NPHOT2.NE.0) THEN
       CALL QTRAL(TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI,PHSU2)
       DO 30 I=1,NPHOT2
       DO 25 K=1,4
   25  PH(K)=PHOT2(I,K)
       CALL QTRAL(TH2,EXT2,EXB2,PSI2,EXW2,EXE2,FIF,PHI,PH)
       DO 26 K=1,4
   26  PHOT2(I,K)=PH(K)
   30  CONTINUE
      ENDIF
* BEAMS P1 AND Q1
      CALL BOSTD3(EXE2,P1,P1)
      CALL ROTOD2( FIF,P1,P1)
      CALL BOSTD3(EXE2,Q1,Q1)
      CALL ROTOD2( FIF,Q1,Q1)
      RETURN
* EVENT OUTSIDE PHASE SPACE
  900 WTKK=0D0
      END
 
      SUBROUTINE MERGIK
*     *****************
      IMPLICIT REAL*8(A-H,O-Z)
* TRANSFER MOMENTA AND MARK INTO PROPER COMMON
* PHOTONS ORDERED ACCORDING TO CMS ENERGY (THE HARDEST IN FIRST
* POSITION)
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / MOMSET / PX1(4),QX1(4),PX2(4),QX2(4),PHOT(100,4),NPHOT
      COMMON / MARPKP / MARKP(100)
      NPHOT=NPHOT1+NPHOT2
      I1=1
      I2=1
      DO 207 I=1,NPHOT
      IF(PHOT1(I1,4).GT.PHOT2(I2,4)) THEN
         DO 205 K=1,4
  205    PHOT( I,K)=PHOT1(I1,K)
         MARKP(I)  =  MK1(I1)
         I1=I1+1
      ELSE
         DO 206 K=1,4
  206    PHOT( I,K)=PHOT2(I2,K)
         MARKP(I)  =  MK2(I2)
         I2=I2+1
      ENDIF
  207 CONTINUE
      DO 300 K=1,4
      PX1(K)=P1(K)
      PX2(K)=P2(K)
      QX1(K)=Q1(K)
      QX2(K)=Q2(K)
  300 CONTINUE
      END
 
 
      SUBROUTINE PTRAL(TH,EXT,EXB,PSI,EXW,EXE,FIF,PHI,P)
*     **************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 P(4)
      CALL ROTOD2( -TH, P, P)
      CALL BOSTD3( EXT, P, P)
      CALL BOSTD1( EXB, P, P)
      CALL ROTOD3( PSI, P, P)
      CALL BOSTD1( EXW, P, P)
      CALL BOSTD3( EXE, P, P)
      CALL ROTOD2( FIF, P, P)
      CALL ROTOD3( PHI, P, P)
      END
 
      SUBROUTINE QTRAL(TH,EXT,EXB,PSI,EXW,EXE,FIF,PHI,P)
*     **************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 P(4)
      DATA PI /3.1415926535897932D0/
      CALL ROTOD2( -TH, P, P)
      CALL BOSTD3( EXT, P, P)
      CALL BOSTD1( EXB, P, P)
      CALL ROTOD3( PSI, P, P)
      CALL ROTOD2(  PI, P, P)
      CALL BOSTD1( EXW, P, P)
      CALL BOSTD3( EXE, P, P)
      CALL ROTOD2( FIF, P, P)
      CALL ROTOD3( PHI, P, P)
      END
 
 
* DEALS WITH LORENTZ TRANS. FROM QQ1 TO QQ FRAME
* WHERE QQ1=P2-P1, QQ=P2+PHSUM-P1, TRANP=QQ1**2, P1**2=P2**2=AMEL**2
* INPUT: TRANP,AMEL,PHSUM
* OUTPUT: P2,TH,EXT,EXB,PHSUM
* HERE, TH, EXT, EXB ARE TRANSFORMATION PARAMS.
      SUBROUTINE KLIPER(TRANP,AMEL,PHSUM,P2,TH,EXT,EXB)
*     **************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PHSUM(4),P2(4)
      REAL*8 P1(4),QQ1(4)
 
      BETEL=SQRT(1D0-4D0*AMEL**2/(TRANP+4D0*AMEL**2))
* NO PHOTON
      IF(PHSUM(4).EQ.0D0) THEN
        P2(3)= SQRT(TRANP)/2D0
        P2(4)= SQRT(P2(3)*P2(3)+AMEL**2)
        P2(2)=0D0
        P2(1)=0D0
        TH =0D0
        EXT=1D0
        EXB=1D0
      ELSE
* ONE PHOTON OR MORE
        ENER1=SQRT(TRANP)/2D0
 
        P1(1)=0D0
        P1(2)=0D0
        P1(3)=-ENER1
        P1(4)= SQRT(P1(3)*P1(3)+AMEL**2)
 
        P2(1)=0D0
        P2(2)=0D0
        P2(3)= ENER1
        P2(4)= SQRT(P2(3)*P2(3)+AMEL**2)
 
        DO 33 I=1,4
  33    QQ1(I)=P2(I)+PHSUM(I)-P1(I)
 
* ROTATION 2 PUTS QQ1 PARALEL TO AXIS 3
        TH  =ANGFI(QQ1(3),QQ1(1))
        CALL ROTOD2(-TH ,QQ1,QQ1)
        CALL ROTOD2(-TH ,P1,P1)
* BOOST 3 PUTS QQ1(4)=0
        EXT = SQRT((QQ1(3)-QQ1(4))/(QQ1(3)+QQ1(4)))
        CALL BOSTD3( EXT ,QQ1,QQ1)
        CALL BOSTD3( EXT , P1, P1)
        EXB = SQRT((P1(4)-P1(1))/(P1(4)+P1(1)))
CC Testing obsolete appendix
CC BOOST 1 PUTS P1 ANTIPARALLEL TO AXIS 3
CC      CALL ROTOD2( -TH , P2, P2)
CC      CALL BOSTD3( EXT , P2, P2)
CC      CALL BOSTD1( EXB , P2, P2)
      ENDIF
      END
 
 
C==================================================================
C=====================NEW MODEL====================================
C==================================================================
      SUBROUTINE MODEL1(MODE)
*     ***********************************
C written:     21 jan. 91 (S.Jadach)
C last update:  5 MAY. 91 (S.J.)
C New model weight without the up-down interference
C WT0,1,2 are zero,first,second order exponentiation weights
* The normalisation of distributions is without (1/s)**2 factor
* and without phase space jacobians (like tp*tq/t**2)
* i.e. just matrix element squared
* in beta's S-tilde*d3k/k0 divided out as usual.
*     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER( ALFA= 1D0/ALFINV, ALF1 = 1D0/PI/ALFINV)
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
      DIMENSION PH(4),RP(4),RQ(4)
      LOGICAL WARUNK
      DATA ICONT/0/
      SDOT(X1,Y1,Z1,E1,X2,Y2,Z2,E2)
     $            = (E1+E2)**2-(X1+X2)**2-(Y1+Y2)**2-(Z1+Z2)**2
      TDOT(X1,Y1,Z1,E1,X2,Y2,Z2,E2)
     $            = (E1-E2)**2-(X1-X2)**2-(Y1-Y2)**2-(Z1-Z2)**2
C.........................................
      DO 10 K=1,4
      RP(K)= P2(K)+PHSU1(K)
   10 RQ(K)= Q2(K)+PHSU2(K)
      TR = TDOT(P1(1),P1(2),P1(3),P1(4) ,RP(1),RP(2),RP(3),RP(4))
      T  = TDOT(P1(1),P1(2),P1(3),P1(4) ,P2(1),P2(2),P2(3),P2(4))
      T1 = TDOT(Q1(1),Q1(2),Q1(3),Q1(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      S  = SDOT(P1(1),P1(2),P1(3),P1(4) ,Q1(1),Q1(2),Q1(3),Q1(4))
      S1 = SDOT(P2(1),P2(2),P2(3),P2(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      U  = TDOT(P1(1),P1(2),P1(3),P1(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      U1 = TDOT(P2(1),P2(2),P2(3),P2(4) ,Q1(1),Q1(2),Q1(3),Q1(4))
C.........................................
C...  Crude MC distribution (S-tilde factors omitted)
      CRUDE  = S**2/TR**2
C.........................................
C...  Contribution from beta0
      BILG   = DLOG(ABS(TR)/AMEL**2)
      BET    = 2D0*ALF1 *( BILG-1D0 )
C... regular photonic corr. to beta0
      DEL0   = BET
C... vacuum polarization factor
      VPFAC  = 1D0/(1D0+REPI(TR))**2
      VPFACS = 1D0/(1D0+REPI( S))**2
C... Z correction
      AMZ  = 91.161
      GAMZ = 2.534
      SINW2= 0.2306
      GA = -1/(4*DSQRT(SINW2*(1-SINW2)))
      GV = GA*(1-4*SINW2)
      DELZ =(TR/S)*(1+TR/S)**3 *2*S**2/(S**2+(S+TR)**2)*
     $ (GV**2+GA**2)*S*(S-AMZ**2)/((S-AMZ**2)**2+(S/AMZ*GAMZ)**2)
C... Z-gamma intrf. has one power of photon and one of Z coup_cons.
      DELZ = DELZ*VPFACS/VPFAC
C... s-channel gamma correction
      DELS   =  TR/S +1.5D0*(TR/S)**2
c........................
      DIS0   = (S**2+U**2+S1**2+U1**2)/4D0 /(T*T1)
      BT00   =  DIS0
      SUM00  =  BT00    /CRUDE
C...  Leading-Log version ........
      ZETA   = DABS(TRAN)/S
      DISLL0 = 0.5D0*(1 + (1-ZETA)**2)*S**2 /(T*T1)
      BTLL00 =  DISLL0
      SULL00 =  BTLL00    /CRUDE
C.........................................
C...  Contributions from beta1 upper line
      SULL11  =  0D0
      XPP  = P2(4)*P1(4)-P2(3)*P1(3)-P2(2)*P1(2)-P2(1)*P1(1)
      XPR  = P1(4)*RP(4)-P1(3)*RP(3)-P1(2)*RP(2)-P1(1)*RP(1)
      DELT = AMEL**2/(2*XPP)
      SUM11U=0D0
      DO 150 I=1,NPHOT1
C...  Numerically safe variables
      A   = AL1(I)
      B   = BE1(I)
      Y   = A +B*DELT
      Z   = B +A*DELT
      P1K = XPR*Z
      P2K = XPR*Y
C...  soft factor
      WMS0 = A*B/(Y*Z)
      SFC  = 2D0*XPP/(P1K*P2K)*WMS0
C...  one photon bremss. distribution
      WM = A*B/(Y*Z) +DELT*(Y**2+Z**2)/((1-Y)**2+(1-Z)**2)*(Y/Z+Z/Y)
      DIS1 = (S**2 +U**2 +S1**2+U1**2)
     $       /4D0/(-T1*P1K*P2K)   *WM
C...  beta1
      BT11U   = DIS1/SFC-BT00
      SUM11U  = SUM11U  + BT11U /CRUDE
C...  Leading-Log version ........
C Note that in DIS1/SFC we get 1/(T*T1) which after multiplication
C by jacobians (T/TRAN)(T1/TRAN) results in 1/TRAN**2, LL is OK!
      ZETA = DABS(TRAN)/(S*(1-A))
      XX  = MAX(A,B)
      DISLL1 = S**2 *(1 + (1-ZETA)**2) * (1 +(1-XX)**2 )
     $       /4D0/(-T1*P1K*P2K)  *WMS0
      BTLL11   = DISLL1/SFC-BTLL00
      SULL11  = SULL11  + BTLL11 /CRUDE
  150 CONTINUE
C.........................................
C...  Contributions from beta1 lower line
      XQQ  = Q2(4)*Q1(4)-Q2(3)*Q1(3)-Q2(2)*Q1(2)-Q2(1)*Q1(1)
      XQR  = Q1(4)*RQ(4)-Q1(3)*RQ(3)-Q1(2)*RQ(2)-Q1(1)*RQ(1)
      DELT = AMEL**2/(2*XQQ)
      SUM11L=0D0
      DO 250 I=1,NPHOT2
C...  numerically safe variables
      A   = AL2(I)
      B   = BE2(I)
      Y   = A+B*DELT
      Z   = B+A*DELT
      Q1K = XQR*Z
      Q2K = XQR*Y
C...  soft factor
      WMS0 = A*B/(Y*Z)
      SFC  = 2D0*XQQ/(Q1K*Q2K)*WMS0
C...  one photon bremss. distribution
      WM = A*B/(Y*Z) +DELT*(Y**2+Z**2)/((1-Y)**2+(1-Z)**2)*(Y/Z+Z/Y)
      DIS1 = (S**2 +U**2 +S1**2+U1**2)
     $       /4D0/(-T *Q1K*Q2K)   *WM
C...  beta1 contrib.
      BT11L   = DIS1/SFC-BT00
      SUM11L  = SUM11L  + BT11L /CRUDE
C...  Leading-Log version ........
      ZETA = DABS(TRAN)/(S*(1-A))
      XX  = MAX(A,B)
      DISLL1 = S**2 *(1 + (1-ZETA)**2) * (1 +(1-XX)**2 )
     $       /4D0/(-T*Q1K*Q2K)  *WMS0
      BTLL11   = DISLL1/SFC-BTLL00
      SULL11   = SULL11  + BTLL11 /CRUDE
  250 CONTINUE
c-----------------------
C the best and most complete
C-----------------------
C zero order
      WTSET( 11) = SUM00*(1+DELZ+DELS)
C first order
      SUM01      = SUM00*(1D0 + DEL0+DELZ+DELS)
      WTSET( 12) = (SUM01 + SUM11U + SUM11L)*VPFAC
C second order not implemented
      WTSET( 13) = 0D0
C-----------------------
C model weight pure bremss. only, no vac.pol, no Z
C zero order
      WTSET( 51) = SUM00
C first order
      SUM01 = SUM00*(1D0 + DEL0)
      WTSET( 52) = (SUM01 + SUM11U + SUM11L)
C second order not implemented
      WTSET( 53) = 0D0
C-----------------------
C Leading-Log version, pure bremss. only
      WTSET( 61) = SULL00
      SULL01 = SULL00*(1D0 + DEL0)
      WTSET( 62) = (SULL01 + SULL11)
      WTSET( 63) = (0D0            )
C------------------------------------------
C Miscelanous, for tests in TH-6118
C Vacuum polarization effect ONN/OFF
      SUM01      = SUM00*(1D0 + DEL0 )
      WTSET( 20) = (SUM01 + SUM11U + SUM11L)* VPFAC
      WTSET( 21) = (SUM01 + SUM11U + SUM11L)* (VPFAC-1)
C Z-exchange  ON/OFF
      SUM01      = SUM00*(1D0 +DEL0 +DELZ  )
      WTSET( 22) = (SUM01 + SUM11U + SUM11L)*VPFAC
      WTSET( 23) = WTSET(22)-WTSET(20)
C s-schannel exchange
      SUM01      = SUM00*(1D0 +DEL0 +DELZ +DELS )
      WTSET( 24) = (SUM01 + SUM11U + SUM11L)*VPFAC
      WTSET( 25) = WTSET(24)-WTSET(22)
C beta_0,1 contribs.
      WTSET( 26) = (SUM01                  )*VPFAC
      WTSET( 27) = (      + SUM11U + SUM11L)*VPFAC
C---------------------------------------------------
C Model weight, normaly the best...
C-----------------------
      WTSET(1) = WTSET(11)
      WTSET(2) = WTSET(12)
      WTSET(3) = WTSET(13)
      KEYPIA = MOD(KEYRAD,10)
C pure bremsstr. as an option:
      IF(KEYPIA.EQ.0) THEN
      WTSET(1) = WTSET(51)
      WTSET(2) = WTSET(52)
      WTSET(3) = WTSET(53)
      ENDIF
      END
 
      SUBROUTINE MODEL2(MODE,WT0,WT1,WT2)
*     ***********************************
C !!! THIS ROUTINE IS NOT USED ANY-MORE !!! KEPT FOR THE RECORD ONLY
C written:     21 jan. 91 (S.Jadach)
C last update: 22 feb. 91 (S.J.)
C New model weight without the up-down interference
C WT0,1,2 are zero,first,second order exponentiation weights
* The normalisation of distributions is without (1/s)**2 factor
* and without phase space jacobians (like tp*tq/t**2)
* i.e. just matrix element squared
* in beta's S-tilde*d3k/k0 divided out as usual.
*     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER( ALFA= 1D0/ALFINV, ALF1 = 1D0/PI/ALFINV)
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
      COMMON / MOMS1  / TRANP,P1(4),P2(4),PHOT1(100,4),PHSU1(4),NPHOT1
      COMMON / MOMS2  / TRANQ,Q1(4),Q2(4),PHOT2(100,4),PHSU2(4),NPHOT2
      COMMON / MOMZ1  / AL1(50),BE1(50),WTM1(50),MK1(50)
      COMMON / MOMZ2  / AL2(50),BE2(50),WTM2(50),MK2(50)
      DIMENSION PH(4),RP(4),RQ(4)
      DATA ICONT/0/
      SDOT(X1,Y1,Z1,E1,X2,Y2,Z2,E2)
     $            = (E1+E2)**2-(X1+X2)**2-(Y1+Y2)**2-(Z1+Z2)**2
      TDOT(X1,Y1,Z1,E1,X2,Y2,Z2,E2)
     $            = (E1-E2)**2-(X1-X2)**2-(Y1-Y2)**2-(Z1-Z2)**2
      ICONT = ICONT+1
 
      DO 10 K=1,4
      RP(K)= P2(K)+PHSU1(K)
   10 RQ(K)= Q2(K)+PHSU2(K)
      TR = TDOT(P1(1),P1(2),P1(3),P1(4) ,RP(1),RP(2),RP(3),RP(4))
      T  = TDOT(P1(1),P1(2),P1(3),P1(4) ,P2(1),P2(2),P2(3),P2(4))
      T1 = TDOT(Q1(1),Q1(2),Q1(3),Q1(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      S  = SDOT(P1(1),P1(2),P1(3),P1(4) ,Q1(1),Q1(2),Q1(3),Q1(4))
      S1 = SDOT(P2(1),P2(2),P2(3),P2(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      U  = TDOT(P1(1),P1(2),P1(3),P1(4) ,Q2(1),Q2(2),Q2(3),Q2(4))
      U1 = TDOT(P2(1),P2(2),P2(3),P2(4) ,Q1(1),Q1(2),Q1(3),Q1(4))
c[[[[[
      IF(MODE.EQ.2) THEN
       WRITE(6,*)'==============================================='
       WRITE(6,*)'==============REPETITION======================='
       WRITE(6,*)'==============================================='
       WRITE(6,*)'///// NPHOT1,2=',NPHOT1,NPHOT2
       WRITE(6,*)'TR/TRAN,T/TRANP,T1/TRANQ',
     $            TR/TRAN,T/TRANP,T1/TRANQ
       WRITE(6,*)'TRANP/TR,TRANQ/TR,TRANP/amel**2,TRANQ/amel**2',
     $            TRANP/TR,TRANQ/TR,TRANP/amel**2,TRANQ/amel**2
      ENDIF
C]]]]
C...  Crude MC distribution (S-tilde factors omitted)
      CRUDE  = S**2/TR**2
C...  Contribution from beta0
      CALL GVIRT0(TR,DELB)
      DIS0= (S**2+U**2+S1**2+U1**2)/4D0 /(T*T1)
      BETA00 =  DIS0
      BETA01 =  DIS0*(1D0+DELB)
      SUM00  =  BETA00    /CRUDE
      SUM01  =  BETA01    /CRUDE
C...  Contributions from beta1 upper line
      XPP  = P2(4)*P1(4)-P2(3)*P1(3)-P2(2)*P1(2)-P2(1)*P1(1)
      XPR  = P1(4)*RP(4)-P1(3)*RP(3)-P1(2)*RP(2)-P1(1)*RP(1)
      DELT = AMEL**2/(2*XPP)
      SUM11U=0D0
      DO 150 I=1,NPHOT1
CC--------Clasic matrix element -------------
CC    DO 120 K=1,4
CC120 PH(K) = PHOT1(I,K)
CC    P1K = P1(4)*PH(4)-P1(3)*PH(3)-P1(2)*PH(2)-P1(1)*PH(1)
CC    P2K = P2(4)*PH(4)-P2(3)*PH(3)-P2(2)*PH(2)-P2(1)*PH(1)
CC    DIS1U =( (S**2 +U**2 ) *(1 -2*AMEL**2/(-TR)*P1K/P2K)
CC   $        +(S1**2+U1**2) *(1 -2*AMEL**2/(-TR)*P2K/P1K) )
CC   $        /4D0/(-T1*P1K*P2K)
CC-- From four-momenta, numericaly instable
CC    ALPRI = (P2K-P1K*DELT)/XPR
CC    BEPRI = (P1K-P2K*DELT)/XPR
CC    YGR = ALPRI +BEPRI*DELT
CC    ZET = BEPRI +ALPRI*DELT
CC-------------------------------------------
C...  Numerically safe variables
      A   = AL1(I)
      B   = BE1(I)
      Y   = A +B*DELT
      Z   = B +A*DELT
      P1K = XPR*Z
      P2K = XPR*Y
C...  soft factor
CZZZZZ      WMS0 = 1 - DELT *( Y/Z + Z/Y )
      WMS0 = A*B/(Y*Z)
      SFC  = 2D0*XPP/(P1K*P2K)*WMS0
C...  one photon bremss. distribution
CZZZ      WM= 1 -2D0*DELT*(1-Y)*(1-Z) /((1-Y)**2+(1-Z)**2)*(Y/Z+Z/Y)
      WM = A*B/(Y*Z) +DELT*(Y**2+Z**2)/((1-Y)**2+(1-Z)**2)*(Y/Z+Z/Y)
      DIS1 = (S**2 +U**2 +S1**2+U1**2)
     $       /4D0/(-T1*P1K*P2K)   *WM
C...  beta1
      BT11U   = DIS1/SFC-BETA00
      SUM11U  = SUM11U  + BT11U /CRUDE
C[[[[[[
      IF(MODE.EQ.2) THEN
        WRITE(6,*)  'BT11U/CRUDE',BT11U/CRUDE
        WRITE(6,*)  'I---------UPER LINE----------------------I'
        WRITE(6,*)  'phot1(i,4)      :::::>', phot1(i,4)
c        WRITE(6,*)  'APRIM/AL1(I)     :::::>', ALPRI/AL1(I),ALPRI
c        WRITE(6,*)  'BPRIM/AL1(I)        ::>', BEPRI/BE1(I),BEPRI
        WRITE(6,*)  'Y,Z,WTM1(I)     +++++>',Y,Z,WTM1(I)
        WRITE(6,*)  'AL1,BE1(I)      +++++>',AL1(I),BE1(I)
        WRITE(6,*)  'WM,DIS1         +++++>',WM,DIS1
        WRITE(6,*)  'WMS0,SFS         ----->',WMS0,SFC
        WRITE(6,*)  'I----------------------------------------I'
      ENDIF
C]]]]]]
  150 CONTINUE
C.........................................
C...  Contributions from beta1 lower line
C.........................................
      XQQ  = Q2(4)*Q1(4)-Q2(3)*Q1(3)-Q2(2)*Q1(2)-Q2(1)*Q1(1)
      XQR  = Q1(4)*RQ(4)-Q1(3)*RQ(3)-Q1(2)*RQ(2)-Q1(1)*RQ(1)
      DELT = AMEL**2/(2*XQQ)
      SUM11L=0D0
      DO 250 I=1,NPHOT2
CC--------Clasic matrix element -------------
CC    DO 220 K=1,4
CC220 PH(K) = PHOT2(I,K)
CC    Q1K  = Q1(4)*PH(4)-Q1(3)*PH(3)-Q1(2)*PH(2)-Q1(1)*PH(1)
CC    Q2K  = Q2(4)*PH(4)-Q2(3)*PH(3)-Q2(2)*PH(2)-Q2(1)*PH(1)
CC    DIS1L=( (S**2 +U1**2) *(1 -2*AMEL**2/(-TR)*Q1K/Q2K)
CC   $       +(S1**2+U**2 ) *(1 -2*AMEL**2/(-TR)*Q2K/Q1K)
CC   $      /4D0/(-T*Q1K*Q2K)
CC------------------------------------------
C...  numerically safe variables
      A   = AL2(I)
      B   = BE2(I)
      Y   = A+B*DELT
      Z   = B+A*DELT
      Q1K = XQR*Z
      Q2K = XQR*Y
C...  soft factor
CZZZZZ      WMS0 = 1 - DELT *( Y/Z + Z/Y )
      WMS0 = A*B/(Y*Z)
      SFC  = 2D0*XQQ/(Q1K*Q2K)*WMS0
C...  one photon bremss. distribution
CZZZZZ      WM= 1 -2D0*DELT*(1-Y)*(1-Z) /((1-Y)**2+(1-Z)**2)*(Y/Z+Z/Y)
      WM = A*B/(Y*Z) +DELT*(Y**2+Z**2)/((1-Y)**2+(1-Z)**2)*(Y/Z+Z/Y)
      DIS1 = (S**2 +U**2 +S1**2+U1**2)
     $       /4D0/(-T *Q1K*Q2K)   *WM
C...  beta1 contrib.
      BT11L   = DIS1/SFC-BETA00
      SUM11L  = SUM11L  + BT11L /CRUDE
C[[[[[[
      IF(MODE.EQ.2) THEN
        WRITE(6,*)  'BT11U/CRUDE',BT11U/CRUDE
        WRITE(6,*)  'I--------lower LINE----------------------I'
        WRITE(6,*)  'phot2(i,4)      :::::>', phot2(i,4)
c        WRITE(6,*)  'APRIM/AL1(I)     :::::>', ALPRI/AL1(I),ALPRI
c        WRITE(6,*)  'BPRIM/AL1(I)        ::>', BEPRI/BE1(I),BEPRI
        WRITE(6,*)  'Y,Z,WTM1(I)     +++++>',Y,Z,WTM2(I)
        WRITE(6,*)  'AL2,BE2(I)      +++++>',AL2(I),BE2(I)
        WRITE(6,*)  'WM,DIS1         +++++>',WM,DIS1
        WRITE(6,*)  'WMS0,SFS         ----->',WMS0,SFC
        WRITE(6,*)  'I----------------------------------------I'
      ENDIF
C]]]]]]
  250 CONTINUE
      WT0 = (SUM00         )
      WT1 = (SUM01 + SUM11U + SUM11L)
      WT2 = (0D0  )
C[[[[[[
      IF(MODE.EQ.2) THEN
       WRITE(6,*) 'WT0,WT1=',WT0,WT1
       WRITE(6,*) 'tran,tranp,tranq', tran,tranp,tranq
      ENDIF
C]]]]]]
      END
 
      SUBROUTINE GVIRT0(T,CORB)
*     ************************
C CALLED IN MODEL2 WHICH IS NOT USED ANY-MORE!!!!
C written:     12 jan. 91 (S.Jadach)
C last update: 11 feb. 91 (SJ)
C Virtual correction to beta0, no up-down interference
*     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER( ALF1 = 1D0/PI/ALFINV)
      PARAMETER( EPSCM= 1D-3)
      COMMON / BHPAR2 / CMSENE,AMEL
      COMMON / BHPAR3 / KEYRAD,KEYOPT
* EPSCM IS HERE A DUMMY PARAMETER BECAUSE LOG(EPSCM) DROPS OUT
      BILG   = DLOG(ABS(T)/AMEL**2)
      BET    = 2D0*ALF1 *( BILG-1D0 )
      COR1   = 2*BET*DLOG(EPSCM)  +1.5D0*BET -ALF1
      COR2   = -2*REPI(T)
      COR    = COR1+COR2
      BPB    = 2*ALF1*( LOG(EPSCM**2)*(BILG-1D0)
     $             +0.5D0*BILG -1D0 -PI**2/6D0 +0.5D0*PI**2  )
c older (numericaly identical) formula (stj)
      CORB1   = COR - BPB
c note that.... (sj)
cc    BPB    = 2*(REBPB(T,EPSCM,AMEL) +0.5D0*ALF1*PI**2)
C Now the real chnge: pi**2 kicked out!!! see also formfactor
C/////CORB   = BET  -ALF1*2D0/3D0*PI**2 +COR2
      KEYPIA = MOD(KEYRAD,10)
      IF(KEYPIA.EQ.0) THEN
        CORB   = BET
      ELSE
        CORB   = BET  +COR2
      ENDIF
      END
 
 
      FUNCTION REPI(S)
C-------------------------------------------- REMARKS ---------------
C VACUUM POLARIZATION IN QED. THE LEPTONIC CONTRIBUTION IS AN ANALY
C EXPRESSION INVOLVING THE LEPTON MASS; THE HADRONIC CONTRIBUTION IS
C A DISPERSION INTEGRAL OVER THE KNOWN HADRONIC CROSS SECTION. THE
C RESULT USED HERE IS A PARAMETRIZATION GIVEN BY
C H.BURKHARDT, TASSO NOTE 192(1981).
C updated see H.Burkhardt et al. Pol. at Lep CERN 88-06 VOL I
C lepton masses now fully taken into account, H.Burkhardt June 89
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 BETA
      REAL*8 M(3)
C
      DATA A1,B1,C1/   0.0   ,   0.00835,  1.0   /
      DATA A2,B2,C2/   0.0   ,   0.00238,  3.927 /
      DATA A3,B3,C3/ 0.00165 ,   0.00300,  1.0   /
      DATA A4,B4,C4/ 0.00221 ,   0.00293,  1.0   /
C
      DATA M/0.51099906D-3,0.10565839D0,1.7841D0/
C
C     for leptons use F,P functions see Burgers, Hollik etc.
C     F(s,m1,m2) for m1=m2 depends only on beta = sqrt((1-4m**2)/s)
      FSYM(BETA)=2.D0+BETA*LOG( (BETA-1.D0)/(BETA+1.D0) )
      P(S,XM,BETA)=1.D0/3.D0-(1.D0+2.D0*XM**2/S) * FSYM(BETA)
C     asymptotic formula for high energies (real part)
      PASYM(S,XM)=-5.D0/3.D0 - LOG (ABS(XM**2/S))
C
C---------------------------------- init and  CHECK FOR S VALUES ----
      DATA I/0/
      IF(I.EQ.0) THEN
        I=1
        AL3PI=1./ (3.D0 * 137.0359895D0 * 3.141592653589793D0)
        IF(S.GT.0.D0.AND.S.LT.100.D0)
     .  WRITE(6,'(3H0S=,F6.3,7H GEV**2,/,
     .    46H VACUUM POLARIZATION MAY BE BADLY APPROXIMATED)')
      ENDIF
C-------------------------------------------- LEPTONIC PART ---------
      REPI=0.D0
C     loop over leptons
      DO 1 I=1,3
        IF(ABS(S).GT.1.D3*M(I)**2) THEN
C         asymptotic formula for s,t >> m**2
          REPI=REPI-PASYM(S,M(I))
        ELSE
          BETA=1.D0-4.D0*M(I)**2/S
          BETA=SQRT(BETA)
          REPI=REPI-P(S,M(I),BETA)
        ENDIF
    1 CONTINUE
      REPI=AL3PI*REPI
C-------------------------------------------- HADRONIC PART ---------
      X=DABS(S)
      IF(X.LT.0.3**2) THEN
        REPI=REPI- (A1+B1*LOG(1.+C1*X))
      ELSEIF(X.LT.3.**2) THEN
        REPI=REPI- (A2+B2*LOG(1.+C2*X))
      ELSEIF(X.LT.100.**2) THEN
        REPI=REPI- (A3+B3*LOG(1.+C3*X))
      ELSE
        REPI=REPI- (A4+B4*LOG(1.+C4*X))
      ENDIF
      END
 
      SUBROUTINE DUMPS(NOUT)
*     **********************
* THIS PRINTS OUT FOUR MOMENTA OF PHOTONS
* ON OUTPUT UNIT NOUT
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      REAL*8 SUM(4)
      WRITE(NOUT,*) '=====================DUMPS===================='
      WRITE(NOUT,3100) ' P2',(P2(K),K=1,4)
      WRITE(NOUT,3100) ' Q2',(Q2(K),K=1,4)
      DO 100 I=1,NPHOT
  100 WRITE(NOUT,3100) 'PHO',(PHOT(I,K),K=1,4)
      DO 200 K=1,4
  200 SUM(K)=P2(K)+Q2(K)
      DO 210 I=1,NPHOT
      DO 210 K=1,4
  210 SUM(K)=SUM(K)+PHOT(I,K)
      WRITE(NOUT,3100) 'SUM',(SUM(K),K=1,4)
 3100 FORMAT(1X,A3,1X,5F18.13)
      END
 
      SUBROUTINE DUMPT(NUNIT,WORD,PP)
C     *******************************
C 15 Jan 90 (SJ)
C this originates from yfs3lib
C prints four momentum PP and its effective mass
C more precisely:  sign(PP.PP)*sqrt(PP.PP)
C     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8 WORD
      REAL*8 PP(4)
      AMS=PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2
      IF(AMS.GT.0.0) AMS=SQRT(AMS)
      WRITE(NUNIT,'(1X,A8,5(1X,F13.8))') WORD,(PP(I),I=1,4),AMS
      END
 
      SUBROUTINE DUMPR(NUNIT,WORD,PP,QQ)
C     **********************************
C 15 Jan 90 (SJ)
C prints twice dot-products of two four momentum PP and QQ
C more precisely:   2*PP.QQ  and  (PP+QQ).(PP+QQ)
C     ************************
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8 WORD
      REAL*8 PP(4),QQ(4)
      DOT1=2*(PP(4)*QQ(4)-PP(3)*QQ(3)-PP(2)*QQ(2)-PP(1)*QQ(1))
      DOT2=(PP(4)+QQ(4))**2-(PP(3)+QQ(3))**2
     $    -(PP(2)+QQ(2))**2-(PP(1)+QQ(1))**2
      WRITE(NUNIT,'(1X,A8,5(1X,F20.10))') WORD,DOT1,DOT2
      END
 
      SUBROUTINE GIBEA(CMSENE,AMEL,P1,P2)
C     ***********************************
C 15 Jan 90 (SJ)
C this originates from yfs302
C GIVEN CMS ENERGY (CMSENE) DEFINES BEAM MOMENTA IN CMS
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P1(*),P2(*)
      BETEL=SQRT(1D0-4D0*AMEL**2/CMSENE**2)
      P1(1)=  0D0
      P1(2)=  0D0
      P1(3)=  CMSENE/2D0*BETEL
      P1(4)=  CMSENE/2D0
      P2(1)=  0D0
      P2(2)=  0D0
      P2(3)= -CMSENE/2D0*BETEL
      P2(4)=  CMSENE/2D0
C----------------------------------------------------------------C
C                      The end of BHLUM2                         C
C----------------------------------------------------------------C
      END
 
 
 
      SUBROUTINE OLDBIS(MODE,XPAR,NPAR)
C     *********************************
C
C           **************************************************
C           *       **********************************       *
C           *       *      *******************       *       *
C           *       *      *                 *       *       *
C           *       *      *   O L D B I S   *       *       *
C           *       *      *                 *       *       *
C           *       *      *******************       *       *
C           *       **********************************       *
C           **************************************************
C
C======================================================================
C
C OLDBIS is an improved version of OLDBAB Monte Carlo of
C F. Berends and R. Kleiss Nucl. Phys. B228 (1983) 537.
C
C OLDBIS was extensively used and partly described in
C Phys. Lett. B253 (1991) 173, TH-5888.
C
C OLDBIS represents the true QED first order calculation
C for small angle Bhabha ( < 10degr.) to within  0.02% TECHNICAL
C precision, see Phys. Lett. B253 (1991) 173  for more details.
C
C For those who are too young to rememeber:
C OLDBAB was the standard and unique Monte Carlo for luminosity
C calculations in all PETRA and PEP experiments!
C
C In the following we anwer or commentent on the following questions:
C   (1) What are essential advantages of OLDBIS with respect to OLDBAB
C   (2) What corrections were done with respect to the original source
C   (3) How to use the program?
C======================================================================
C
C Answer to question (1) :
C ========================
C
C The most important advantage of OLDBIS with respect to OLDBAB is that
C OLDBIS has well established TECHMICAL precision of 0.02% in a very
C wide range of cut-off parameters,
C see Phys. Lett. B253(1991)173,TH-5888.
C
C OLDBAB calculates first order QED correction to small angle Bhabha.
C The basic and burning question in the beginning of 1990 was:
C to what TECHNICAL precision this
C first order correction from OLDBAB (and other programs like BABAMC)
C represent the true and unique QED answer!
C One could only guess that it is about 1%  but no firm statement with
C solid justification could be found at the time
C in any published papers on the above guess!
C (N.B. 1% precision was enough for PETRA/PEP.)
C In Phys. Lett. B253(1991)173, we have proved that, after some
C modifications (transforming OLDBAB into OLDBIS), the fantastic
C TECHNICAL precision 0.02% was reached! To this precision OLDBIS
C represents the TRUE first order QED calculation in the
C low angle Bhabha.
C
C Answer to question (2) :
C ========================
C
C We have started with source code of OLDBAB taken from RADCOR package
C installed on CERN public disk by R. Kleiss.
C The algorithm and most of source code in present OLDBIS is identical
C to that of OLDBAB.
C New corrections are of two types:
C (a) cosmetic ones which concern mainly output/input formats, choice
C of random number generator etc.
C (b) essential ones which modify
C algorithm, implementation of QED matrix element and usage of program.
C
C Full list of corrections/modifications:
C
C (a) Midifications of input/output and other of the cosmetic type:
C -----------------------------------------------------------------
C
C (==( 000 )==)
C The histograming routines are removed.
C
C (==( 001 )==)
C The input/output structure was aligned with other programs like
C BHLUMI, KORALZ, KORALB, LESKOF etc.
C In the CALL OLDBIS(MODE,XPAR,NPAR) initialisation, generation
C and postgeneration phases are determined by MODE= -1,0,1 parameter.
C
C (==( 002 )==)
C All input is read through XPAR and NPAR in the obligatory
C initialization for MODE=-1 and immetiately printed out.
C
C (==( 003 )==)
C Each generated event (for MODE=0) is encoded in /MOMBAB/.
C
C (==( 004 )==)
C Calling OLDBIS with MODE=1 or 2 provides
C cross-section resulting from Monte Carlo integration (necessary
C for histogram normalization).
C For MODE=1 no output is printed.
C
C (==( 005 )==)
C For MODE=2 in addition to information encoded in XPAR and NPAR
C certain output is printed.
C Note that routine BABINF printing output in the same format
C as in original OLDBAB is kept active.
C
C (==( 006 )==)
C All weight accounting is not done "by hand" but rather using
C special routine WMONIT from KORALZ.
C
C (==( 007 )==)
C The modern random number generator VARRAN replaces the original one.
C
C (==( 008 )==)
C Output four-momenta QP,QM,QK are now in GeV units.
C
C (b) Modifications of the algorithm and QED matrix element:
C ----------------------------------------------------------
C
C (==( 100 )==)
C Detailed insight into MC algorithm of OLDBAB reveals that variable
C SIGS is a dummy variable i.e. none of the calculated x-sections
C and distributions depends (within stat. err.) on SIGS.
C For this to be true SIGS has to be positive, however!
C For very small k0 SIGS becomes negative and the results from
C the OLDBAB do not represent true first order QED anymore.
C We have corrected this i.e. defined SIGS which is always positive.
C
C (==( 101 )==)
C The above corr. (100) is still not enough to avoid problems at the
C very small k0=XK0 limit which has to be taken in order to check that
C the program represent first order QED at the 0.02% technical
C precision level. One has to alow negative weights i.e. introduction
C of weighted events is necessary. This option is implemented
C and the program provides weighted events for switch KEYWGT=1.
C The weight WTM is located in /MOMBAB/ and later transfered
C to /WGTALL/.  We recommend to use OLDBIS with weighted events.
C
C (==( 102 )==)
C The essential observation made in Phys. Lett. B253 (1991) 173,TH-5888
C and expoited also in other papers (TH-5995,TH-6118) is that certain
C class of anoying QED corrections, so called up-down interferences,
C is completely unimportant at low angles ( <10 degr.).
C To obtain this result we had to rewrite completely the QED soft and
C hard matrix element in OLDBAB.
C New routine VIRSEL calculating soft and virtual corrections is added.
C The user has at his disposal four types of matrix element with
C various components switched on/off.
C Each of them is represented by the separate model weight XWT(10:13).
C Only one of them
C                      XWT(10+KEYSIN)
C is choosen as a model weight
C for eventual rejection, where KEYSIN=0,1,2,3 is the input parameter:
C               Table 1, WTSET entries for OLDBIS
C----------------------------------------------------------------------
C  Entry        Corrections present/absent in matrix element
C----------------------------------------------------------------------
C               up-down int.   vac.pol.   Z-exch.  s-chan.phot.
C               -------------------------------------------------------
C  XWT(10)        yes          yes         yes        yes
C  XWT(11)         no           no          no         no
C  XWT(12)        yes           no          no         no   principal)
C  XWT(13)        yes           no          no        yes
C  -------------------------------------------------------------------
C The backward compatibility is kept, XWT(10) represents original
C OLDBAB matrix element (with new vacuum polarization and Z-width).
C The difference XWT(12)-XWT(11) accounts for pure up-down interferece.
C
C (==( 103 )==)
C The archaic vacuum polarization subprogram REPI (for KEYSIN=0)
C is replaced by the modern version of the same name.
C
C (==( 104 )==)
C The Z-gamma inerference correction includes now Z width
C (for KEYSIN=0).
C
C (==( 105 )==)
C No immediate rejection for events in which one fermion is in
C theta-trigger but another one (due to photon emission) is out.
C Such an event goes through but has zero weight.
C
C (==( 106 )==)
C Symmetrisation QP <=> QM can be supressed by setting KEYMIR switch
C KEYMIR = 0. This option was usefull in TH-5888 comparisons
C with semianalytical calculations.
C
C Answer to question (3) :
C ========================
C
C The complete description of the usage of the program can be found
C in Long-write-up of BHLUMI 2.01, CERN preprint TH-6230.
C Here we only summarize on Input/Output parameters in
C                 CALL BHLUMI(MODE,XPAR,NPAR)
C The user may use directly OLDBIS as well
C                 CALL OLDBIS(MODE,XPAR,NPAR)
C
C IF( MODE =-1 ) THEN
C ===================
C
C Initialisation is performed, all input parameters are transferred
C through XPAR and NPAR, see table below:
C     Table 2, Input parameters of OLDBIS
C----------------------------------------------------------------------
C  Entry    Variable   Meaning
C----------------------------------------------------------------------
C  NPAR( 1)  KEYOPT =1000*KEYGEN +10*KEYWGT +KEYRND
C                   General option switch
C            KEYGEN =3 for this sub-generator
C            KEYRND =1,2 type of random number generator RANMAR,RANECU
C            KEYWGT =0,1 for constant, variable weight WTM
C  NPAR( 2)  KEYRAD =10*KEYMIR +KEYSIN, QED switch defining WTM weight
C                   The meaning of KEYSIN is summarized in Table 1,
C            KEYMIR =0 photon emitted only from both fermions
C            KEYMIR =1 photon emitted only from QM fermion line (tests)
C  XPAR( 1)  CMSENE Total center mass energy [GeV]
C  XPAR( 2)   THMIN Minimum theta angle for electron [degr]
C  XPAR( 3)   THMAX Maximum theta angle for electron [degr]
C  XPAR( 4)     XK0 k0 parameter, infrared cut on real photon energy
C                   is k0*CMSENE/2, recomended range 0.000001<k0<0.001
C  XPAR( 5)   XKMAX maximum real photon energy is XKMAX*CMSENE/2
C  XPAR( 6)   XKMIN minimum real photon energy is XKMIN*CMSENE/2,
C                   one should normaly set XKMAX=1, XKMIN=0.
C----------------------------------------------------------------------
C
C ELSE IF( MODE = 0 ) THEN
C ========================
C
C Generation of the single Monte Carlo event
C The four momenta of the final state electron positron and photon
C are primarily encoded in
C      COMMON / MOMBAB / QP(4),QM(4),QK(4),WTM,XWT(20)
C where QP and QM are four-momenta of positron and electron and QK
C is four-momentum of the photon (from time to time QK is zero).
C They are in GeV units and the z-axis points in the direction of
C the positron beam.
C WTM is the main model weight selected according to KEYSIN switch.
C All four possible model weights with various contributions on/off
C (see table 1) are encoded in XWT. For KEYWGT=0 we have WTM=1
C and XWT should not be used!
C Note that content of /MOMBAB/ is copied to standard multiphoton
C common blocks /MOMSET/ and /WGTALL/ of BHLUMI as well.
C
C
C ELSE IF( MODE = 1 ) THEN
C ========================
C
C The total cross section corresponding to generated series of event,
C i.e. resulting from MC integrartion is calculated and stored in XPAR,
C together with a lot of auxiliary information, see table 3 below.
C This impressive set of output was nesessary for TH-5888 calculations.
C                         Table 3
C----------------------------------------------------------------------
C  Entry    Variable   Meaning
C----------------------------------------------------------------------
C  NPAR(10)  NEVGEN  Number of generated MC events
C  NPAR(20)  NEVGEN  Number of generated MC events
C  XPAR( 9)    SIG0  Born x-section  [nb]
C  XPAR(10)    XST2  Total x-section [nb]
C  XPAR(11)   RXST2  The relative error of XST2
C  XPAR(12)     XSS  Soft (k<k0)  x-section [nb]
C  XPAR(13)    RXSS  The relative error of XSS
C  XPAR(14)     XSH  Hard (k<k0)  x-section [nb]
C  XPAR(15)    RXSH  The relative error of XSH
C  XPAR(20)    SIGS+SIGH  Crude total MC x-section [nb] which
C                    is necessary for rescaling histograms in
C                    run with weighted events.
C  XPAR(21)          =0, error of XPAR(20) is zero
C  XPAR(22)    SIGS  Crude soft MC x-section [nb]
C  XPAR(23)          =0, error of XPAR(22) is zero
C  XPAR(24)    SIGH  Crude hard MC x-section [nb]
C  XPAR(25)          =0, error of XPAR(24) is zero
C----------------------------------------------------------------------
C For constant weight option KEYWGT=0 (convevience in rescaling histos)
C we put XPAR(20,21,22)=XPAR(10,11,12) !
C For MODE=1 program is called upon many times in the process of
C rescaling histograms there is no output printed.
C
C ELSE IF( MODE = 2 ) THEN
C ========================
C
C Only in this MODE=2 in addition to filling XPAR and NPAR as
C for MODE=1 the values of various x-sections are printed on
C standard output file.
C
C ENDIF
C ====
C
C
C
C======================================================================
C======================================================================
C History of corrections for the record (stj)
C you may skip reading this.
C======================================================================
C===== Series of corrections from Jan. 91 to June 91 ==================
C======================================================================
C===== VERSION OF OLDBAB USED IN TH.5888/90 BENCHMARK =================
C===== DILOG and WMONIT moved to library ==============================
C===== REPI replaced with modern version ==============================
C===== Adjusted KEYRAD and KEYPOT/KEYWGT ==============================
C===== Outout XPAR(11) is relative error ==============================
C===== KINBIS translates MOMBAB into MOMSET ===========================
C======================================================================
C===== Series of corrections from Jan. 90 to Dec. 90 ==================
C======================================================================
C  (0) IMODE=-1,0,1,2   initialization, production, output
C  (1) new random number generator
C  (2) QP,QM,PK moved to /MOMBAB/
C  (3) QP,QM,PK in GeV units
C  (4) negative weights in soft part treated properly
C  (5) possibility of weighted events, including negative
C  (6) reorganized QED matrix element with up-down interf. isolated
C======================================================================
C======================================================================
C
C        Here starts "original" source code
C
C------------------------------------------- REMARKS ---------------
C
C
C SIMULATION OF RADIATIVE BHABHA SCATTERING IN 2ND & 3RD ORDER Q.E.D.
C WRITTEN BY R.KLEISS (LEIDEN & DESY) OCTOBER '82
C
C
C********************************************************************
C
C A DETAILED WRITEUP, TOGETHER WITH PHYSICS INTERPRETATION
C CAN BE FOUND IN  :
C         F.A. BERENDS AND R. KLEISS , NUCL. PHYS. B228(1983)537 .
C IF YOU USE THIS PROGRAM, PLEASE MAKE REFERENCE TO THE ABOVE PAPER !
C
C********************************************************************
C
C
C BEFORE CALL OF FIRST EVENT, COMMON 'PARM01' MUST BE FILLED WITH :
C EBEAM = BEAM ENERGY (IN GEV) ;
C THMIN = MINIMUM SCATTERING ANGLE OF ELECTRON/POSITRON (IN DEGREES)
C THMAX = MAXIMUM SCATTERING ANGLE OF ELECTRON/POSITRON (IN DEGREES)
C XKMIN = MINIMUM BREMSSTRAHLUNG ENERGY (IN UNITS OF EBEAM) ;
C XKMAX = MAXIMUM BREMSSTRAHLUNG ENERGY (IN UNITS OF EBEAM) .
C XKMIN=0,XKMAX=1 ARE ALLOWED. THMIN=0,THMAX=180 ARE NOT ALLOWED.
C IF XKMIN < 0.01 IT IS ASSUMED TO BE 0.
C QP(I) = FOUR MOMENTUM OF OUTGOING POSITRON (I=1,2,3,4) ;
C QM(I) = FOUR MOMENTUM OF OUTGOING ELECTRON (I=1,2,3,4) ;
C QK(I) = FOUR MOMENTUM OF PHOTON            (I=1,2,3,4) .
C FOUR MOMENTA ARE IN UNITS OF EBEAM. THE INCOMING MOMENTA ARE:
C POSITRON (0,0,1,1) , ELECTRON (0,0,-1,1) .
C--------------------------------------------------------------------
C LATEST UPDATE: APRIL 5,1983.
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI= 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER(GNANOB =  389385D0)
CCCCCCCCC                       (==( 002 )==)
      DIMENSION XPAR(*),NPAR(*)
CCCCCCCCC                       (==( 003 )==)
      COMMON / MOMBAB / QP(4),QM(4),QK(4),WTM,XWT(20)
      COMMON / PARM01 / EBEAM,THMIN,THMAX,XKMIN,XKMAX,CMIN,CMAX
      COMMON / PARM02 / SIG0,SIGS,SIGH,SIGT,WT
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
C Communicates with VARRAN      (==( 007 )==)
      COMMON / RANPAR / KEYRND
      DOUBLE PRECISION DRVEC(100)
      DIMENSION WT(18)
CCCCCCCCC                       (==( 001 )==)
C     ===================
      IF(MODE.EQ.-1) THEN
C     ===================
C-------------------------------------------- INITIALIZATION --------
C ...BX-formats for nice and flexible outbuts
      BXOPE =  '(//1X,15(5H*****)    )'
      BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'
      BXL1I =  '(1X,1H*,I17,                 16X, A20,A12,A7, 1X,1H*)'
      BXL1F =  '(1X,1H*,F17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2F =  '(1X,1H*,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXL1G =  '(1X,1H*,G17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2G =  '(1X,1H*,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXCLO =  '(1X,15(5H*****)/   )'
      CALL WMONIT(-1,70,DUMM1,DUMM2,DUMM3)
      CALL WMONIT(-1,71,DUMM1,DUMM2,DUMM3)
      CALL WMONIT(-1,72,DUMM1,DUMM2,DUMM3)
      CALL WMONIT(-1,41,DUMM1,DUMM2,DUMM3)
      CALL WMONIT(-1,42,DUMM1,DUMM2,DUMM3)
      CALL WMONIT(-1,51,DUMM1,DUMM2,DUMM3)
      CALL WMONIT(-1,52,DUMM1,DUMM2,DUMM3)
CCCCCCCCC                       (==( 002 )==)
      KEYOPT=NPAR(1)
      KEYRAD=NPAR(2)
      CMSENE=XPAR(1)
      THMIN =XPAR(2)
      THMAX =XPAR(3)
      XK0   =XPAR(4)
      XKMAX =XPAR(5)
      XKMIN =XPAR(6)
      EBEAM =CMSENE/2
      KEYRND = MOD(KEYOPT,10)
      KEYWGT = MOD(KEYOPT,100)/10
      KEYSIN = MOD(KEYRAD,10)
      KEYMIR = MOD(KEYRAD,100)/10
CCCCCCCCC                       (==( 002 )==)
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '     **********************      '
      WRITE(NOUT,BXTXT) '     **    O L D B I S   **      '
      WRITE(NOUT,BXTXT) '     **********************      '
      WRITE(NOUT,BXTXT) ' O(alpha) Monte Carlo Program    '
      WRITE(NOUT,BXTXT) ' for the small-angle Bhabha scat.'
      WRITE(NOUT,BXTXT) '         --------------          '
      WRITE(NOUT,BXTXT) '         Important Note          '
      WRITE(NOUT,BXTXT) '         --------------          '
      WRITE(NOUT,BXTXT) ' This is an improved version     '
      WRITE(NOUT,BXTXT) ' of the OLDBAB M.C. program of   '
      WRITE(NOUT,BXTXT) ' F.A. BERENDS AND R. KLEISS      '
      WRITE(NOUT,BXTXT) ' [1] NUCL. PHYS. B228 (1983) 537 '
      WRITE(NOUT,BXTXT) ' Changes were done by S. Jadach, '
      WRITE(NOUT,BXTXT) ' E. Richter-Was and other        '
      WRITE(NOUT,BXTXT) ' authors of the paper            '
      WRITE(NOUT,BXTXT) ' [2] Phys. Lett. B253 (1991) 173 '
      WRITE(NOUT,BXTXT) ' All modifications are desribed  '
      WRITE(NOUT,BXTXT) ' in detail in the source code.   '
      WRITE(NOUT,BXTXT) ' PLEASE CITE references [1,2] !  '
      WRITE(NOUT,BXCLO)
C
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '* This program is part of       *'
      WRITE(NOUT,BXTXT) '*   BHLUMI version 2.01         *'
      WRITE(NOUT,BXTXT) '*   September      1991         *'
      WRITE(NOUT,BXTXT) '*         AUTHORS               *'
      WRITE(NOUT,BXTXT) '* S. Jadach, E. Richter-Was     *'
      WRITE(NOUT,BXTXT) '*    B.F.L. Ward, Z. Was        *'
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXCLO)
C
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '         OLDBIS input            '
      WRITE(NOUT,BXL1I) KEYOPT,     ' option    switch  ','KEYOPT','N1'
      WRITE(NOUT,BXL1I) KEYRND,     ' VARRAN    switch  ','KEYRND','  '
      WRITE(NOUT,BXL1I) KEYWGT,     ' weighting switch  ','KEYWGT','  '
      WRITE(NOUT,BXL1I) KEYRAD,     ' radiation switch  ','KEYRAD','N2'
      WRITE(NOUT,BXL1I) KEYSIN,     ' interf. s-chan.   ','KEYSIN','  '
      WRITE(NOUT,BXL1I) KEYMIR,     ' QM emiss.only/test','KEYMIR','  '
      WRITE(NOUT,BXL1F) CMSENE,     ' CMSENE            ','CMSENE','X1'
      WRITE(NOUT,BXL1F) THMIN ,     ' THMIN             ','THMIN ','X2'
      WRITE(NOUT,BXL1F) THMAX ,     ' THMAX             ','THMAX ','X3'
      WRITE(NOUT,BXL1F) XK0   ,     ' XK0               ','XK0   ','X4'
      WRITE(NOUT,BXL1F) XKMAX ,     ' XKMAX             ','XKMAX ','X5'
      WRITE(NOUT,BXL1F) XKMIN ,     ' XKMIN             ','XKMIN ','X6'
      WRITE(NOUT,BXCLO)
      NEVGEN=0
      WMAX=2.D0
      DO 1 I=1,18
    1 WT(I)=0.
      WT(13)=WMAX
      WT(17)=WMAX
      AMEL  = 0.511D-3
      XM2=(AMEL/EBEAM)**2
      EM2=1.D0+.5D0*XM2
      XL2=DLOG(2.D0/XM2)
      TWOPI= 2D0*PI
      CMIN=DCOS(THMAX*TWOPI/360.D0)
      CMAX=DCOS(THMIN*TWOPI/360.D0)
      DMIN=1.D0/(1.D0-CMIN)
      DMAX=1.D0/(1.D0-CMAX)
      XLCMIN=XL2+DLOG(1.D0-CMIN)
      XSOFT=1.D0
      IF(XKMIN.LT.XK0) GOTO 2
      XSOFT=0.D0
      XK0=XKMIN
    2 XKRAT=XKMAX/XK0
      ALFA  =  1D0/ALFINV
      SIGBS =  8D0*PI*ALFA**2/(2D0*EBEAM)**2 *GNANOB
      SIG0  =  SIGBS/16D0  *(
     $ 16*(DMAX-DMIN) + 16*DLOG(DMIN/DMAX) + 9*(CMAX-CMIN)
     $ + (CMAX**2-CMIN**2) + 1.D0/3.D0*(CMAX**3-CMIN**3)         )
      SIGH =  SIGBS * (ALFA/PI)  *4D0 *DLOG(XKRAT)*
     $ ((XL2+1.D0+DLOG(1.D0-CMAX))*DMAX - (1.D0+XLCMIN)*DMIN )
CC[[[[[[[[[[[[   (==( 100 )==)
C     SIGS=0.D0
C     DO 3 I=1,100
C     R= FLOAT(I-1)/99.D0
C     C=1.-1./(R*DMAX+(1.-R)*DMIN)
C     IF(KEYSIN.EQ.0) THEN
C       CALL VIRSOF(EBEAM,XK0,C,BORN,CORR)
C     ELSE
C       CALL VIRSEL(KEYSIN,EBEAM,XK0,C,BORN,CORR)
C     ENDIF
C&&&3 SIGS=SIGS+BORN*(1.+CORR)*(1.-C)**2
C   3 SIGS=SIGS+BORN*(1.D0     )*(1.D0-C)**2
C the above assures full indep. of xsect. on k0, stj, sept. 89.
C     WRITE(NOUT,*) ' /////CORR IN SIGS EXCLUDED/////'
C     SIGS=SIGS*TWOPI*(DMAX-DMIN)/100.D0*XSOFT
CC]]]]]]]]]]]]
C SIGS is a dummy parameter
C convention: sigma_sof_crude = sigBS/(1-c)**2 (stj)
      SIGS = SIGBS *(DMAX-DMIN)
      SIGT=SIGS+SIGH
      YSOFT=SIGS/SIGT
      ZSOFT=0.D0
      IF(SIGS.NE.0.D0)  ZSOFT=(DMAX-DMIN)/SIGS*TWOPI
      DTOT=SIGT/SIG0-1.
      WRITE(NOUT, 4) XM2   ,EM2   ,XL2   ,TWOPI ,CMIN  ,CMAX
     .       ,DMIN  ,DMAX  ,XLCMIN,XK0   ,XSOFT ,XKRAT
     .       ,WMAX  ,SIG0  ,SIGH  ,SIGS  ,SIGT  ,YSOFT
     .       ,ZSOFT ,DTOT
    4 FORMAT('0',50('=')/,
     . ' INITIALIZATION FOR BHABHA SCATTERING'/,6(' ',4D15.6/))
      WRITE(NOUT,5)EBEAM,THMIN,THMAX,XK0,XKMAX,SIG0,SIGS,SIGH,SIGT,DTOT
    5 FORMAT(
     . '                        BEAM ENERGY =',F15.6,' GEV'/,
     . '           MINIMUM SCATTERING ANGLE =',F15.6,' DEGREES'/,
     . '           MAXIMUM SCATTERING ANGLE =',F15.6,' DEGREES'/,
     . ' MINIMUM HARD BREMSSTRAHLUNG ENERGY =',F15.6/,
     . ' MAXIMUM HARD BREMSSTRAHLUNG ENERGY =',F15.6/,
     . '         LOWEST ORDER CROSS SECTION =',D15.6,' NB'/,
     . ' APPROX. CROSS SECTION IN SOFT PART =',D15.6,' NB'/,
     . ' APPROX. CROSS SECTION IN HARD PART =',D15.6,' NB'/,
     . '        APPROX. CROSS SECTION TOTAL =',D15.6,' NB'/,
     . '           APPROX. TOTAL CORRECTION =',F15.6)
C     ======================
      ELSEIF(MODE.EQ.0) THEN
C     ======================
      NEVGEN=NEVGEN+1
    6 CONTINUE
      WTM=1D0
      WTK=1D0
C-------------------------------------------- CHOOSE HARD OR SOFT ---
CCCCCCCCC                       (==( 007 )==)
      CALL VARRAN(DRVEC,1)
      IF(DRVEC(1).LT.YSOFT) GOTO 11
C-------------------------------------------- HARD PHOTON PART ------
      WT(1)=WT(1)+1.D0
C-------------------------------------------- GENERATE K VALUE ------
      CALL VARRAN(DRVEC,1)
      XK=XK0*XKRAT**DRVEC(1)
C-------------------------------------------- GENERATE C VALUE ------
    7 CONTINUE
      CALL VARRAN(DRVEC,2)
      C=1.D0-1.D0/(DMIN+DRVEC(1)*(DMAX-DMIN))
      R=DRVEC(2)*XLCMIN/(XL2+DLOG(1.D0-C))
      WT(2)=WT(2)+1.D0
      IF(R.GT.1.D0) GOTO 7
      CM=2.D0*(1.D0-C)
      SC=DSQRT(1.D0-C*C)
C-------------------------------------------- GENERATE FI VALUE -----
      FI=R*TWOPI
C-------------------------------------------- GENERATE U VALUE ------
      D=XM2/CM
      CALL VARRAN(DRVEC,1)
      R=-1.D0+2.D0*DRVEC(1)
      V=(D/(1.D0+D))**DABS(R)
      U=((1.D0+D)*V-D)/(1.D0+V)
      E2=U*(1.D0-U)*CM
      EV=DSQRT(1.D0-E2)
      E2=XM2+E2
      IF(R.LT.0.D0) U=1.D0-U
C-------------------------------------------- GENERATE C1 VALUE -----
      CALL VARRAN(DRVEC,2)
      R=DRVEC(1)
      VC=2.D0*E2*(1.D0-R)/(E2+2.D0*EV*(EM2+EV)*R)
      C1=1.D0-VC
      SC1=DSQRT(VC*(2.D0-VC))
C-------------------------------------------- GENERATE F1 VALUE -----
      F1=TWOPI*DRVEC(2)
      CF1=DCOS(F1)
      SF1=DSIN(F1)
C-------------------------------------------- CONSTRUCT QK DIRECTION
      UC=-1.D0+U-U*C
      QK1=(UC*SC1*CF1-U*SC*C1)/EV
      QK2=SC1*SF1
      QK3=(U*SC*SC1*CF1+UC*C1)/EV
      CG=C*QK3+SC*QK1
C-------------------------------------------- REJECT CT VALUES ------
      XKM=1.D0-XK
      X=2.D0*XKM/(2.D0-XK+XK*CG)
      XT=2.D0-X-XK
      CT=(X*C+XK*QK3)/XT
CCCCC               (==( 109 )=)
      IF(CT.LT.CMIN.OR.CT.GT.CMAX) WTK=0D0
      WT(3)=WT(3)+ WTK
C-------------------------------------------- CALCULATE WEIGHT ------
      S =4.D0
      S1=4.D0*XKM
      T =-2.D0*X *(1.D0-C )
      T1=-2.D0*XT*(1.D0-CT)
      U =-2.D0*XT*(1.D0+CT)
      U1=-2.D0*X *(1.D0+C )
      X1=XK*(EM2-QK3)
      X2=XK*(EM2+QK3)
      DY=.5D0*XM2*XK/XKM
      Y1=2.D0*(1.D0-XT)+DY
      Y2=2.D0*(1.D0-X )+DY
CC[[[[[[[[[[[[         (==(102)==)
*.....KEYSIN=0   original OLDBAB
*.....KEYSIN=1   T NONINTERFERENCE CHANNEL
*.....KEYSIN=2   T CHANNEL WITH INTERF.
*.....KEYSIN=3 S+T CHANNEL
      WTH30=(S*S1*(S*S+S1*S1)+T*T1*(T*T+T1*T1)+U*U1*(U*U+U1*U1))
     .  /(4.*S**3*S1)
     .  *(1.D0-(S*Y1*Y2+S1*X1*X2+U*X2*Y1+U1*X1*Y2)
     .         /(T*X2*Y2+T1*X1*Y1))
     .  *(1.D0-XM2*XK/(1.D0+XKM*XKM)
     .        *(XKM/X1+XKM/X2+1.D0/Y1+1.D0/Y2))
      XWT(10)=  WTK*WTH30
      XWT(13)=  WTK*WTH30
C Here t-channel only and no interferences
      XTCH= (S**2 +U**2 )/(-X1*Y1*T1)*(1+2*XM2*X1/Y1/T1)
     $     +(S1**2+U1**2)/(-X1*Y1*T1)*(1+2*XM2*Y1/X1/T1)
     $     +(S**2 +U1**2)/(-X2*Y2*T )*(1+2*XM2*X2/Y2/T )
     $     +(S1**2+U**2 )/(-X2*Y2*T )*(1+2*XM2*Y2/X2/T )
      XES2= 4*S**2*(-1/T1/X1/Y1 -1/T/X2/Y2)
      WTH1 = XTCH/XES2
      XWT(11)=  WTK*WTH1
C t-chanel only PLUS interferences
      XTCH= (S**2 +U**2 )/(-X1*Y1*T1)*(1+2*XM2*X1/Y1/T1)
     $     +(S1**2+U1**2)/(-X1*Y1*T1)*(1+2*XM2*Y1/X1/T1)
     $     +(S**2 +U1**2)/(-X2*Y2*T )*(1+2*XM2*X2/Y2/T )
     $     +(S1**2+U**2 )/(-X2*Y2*T )*(1+2*XM2*Y2/X2/T )
      XTCHI= XTCH
     $  +1/(T*T1)*(S**2+U**2+S1**2+U1**2)
     $     *( S/X1/X2 +S1/Y1/Y2 +U/X1/Y2 +U1/X2/Y1)
      XES2= 4*S**2*(-1/T1/X1/Y1 -1/T/X2/Y2)
      WTH2 = XTCHI/XES2
      XWT(12)=  WTK*WTH2
      IF(KEYSIN.GT.3.OR.KEYSIN.LT.0) THEN
       WRITE(NOUT,*) ' ++++ WRONG KEYSIN ',KEYSIN
       STOP
      ENDIF
      WTM   =  XWT(10+KEYSIN)
      XWT(2)=  WTM
CC]]]]]]]]]]]]
C-------------------------------------------- CONSTRUCT MOMENTA -----
      CFI=DCOS(FI)
      SFI=DSIN(FI)
      QK(4)=XK
      QK(3)=XK*QK3
      QK(2)=XK*(QK2*CFI-QK1*SFI)
      QK(1)=XK*(QK2*SFI+QK1*CFI)
      QP(4)=X
      QP(3)=X*C
      QP(2)=-X*SC*SFI
      QP(1)=X*SC*CFI
      DO 8 I=1,4
    8 QM(I)=-QP(I)-QK(I)
      QM(4)=2.D0+QM(4)
C-------------------------------------------- REJECT W VALUES -------
      WT(4)=WT(4)+WTM
      WT(5)=WT(5)+WTM*WTM
      IF(WTM.LT.0.D0)   WT(11)=WT(11)+1.D0
      IF(WTM.GT.WMAX)   WT(12)=WT(12)+1.D0
      IF(WTM.LT.WT(13)) WT(13)=WTM
      IF(WTM.GT.WT(14)) WT(14)=WTM
      CALL VARRAN(DRVEC,1)
      RN=DRVEC(1)
C principal weight
      CALL WMONIT(0,72,WTM,WMAX,RN)
      CALL WMONIT(0,70,WTM,WMAX,RN)
C auxiliary weights
      CALL WMONIT(0,51,WTH1*WTK,WMAX,RN)
      CALL WMONIT(0,52,WTH2*WTK,WMAX,RN)
C ...
      IF(KEYWGT.EQ.0.AND.RN*WMAX.GT.WTM) GOTO 6
      IF(KEYWGT.EQ.0) WTM=1D0
      WT(6)=WT(6)+1.D0
C-------------------------------------------- REFLECTION POINT ------
      CALL VARRAN(DRVEC,1)
      RN=DRVEC(1)
CCCCCCCCCC         (==( 106 )==)
C Reflection QP <=> QM can be optionally suspended
      IF(KEYMIR.EQ.1) RN=1
      IF(RN.GT.0.5D0) GOTO 10
      DO 9 I=1,3
      QK(I)=-QK(I)
      QPI=QP(I)
      QP(I)=-QM(I)
    9 QM(I)=-QPI
      QPI=QP(4)
      QP(4)=QM(4)
      QM(4)=QPI
   10 CONTINUE
CCCCCCCCCC      (==( 008 )==)
      DO 50 I=1,4
      QP(I)=EBEAM*QP(I)
      QM(I)=EBEAM*QM(I)
   50 QK(I)=EBEAM*QK(I)
      CALL KINBIS
      RETURN
C-------------------------------------------- END OF HARD PART ------
C-------------------------------------------- SOFT PHOTON PART ------
   11 CONTINUE
      WT(7)=WT(7)+1.D0
C-------------------------------------------- GENERATE C VALUE ------
      CALL VARRAN(DRVEC,1)
      C=1.D0-1.D0/(DMIN+DRVEC(1)*(DMAX-DMIN))
C-------------------------------------------- CALCULATE WEIGHT ------
CC[[[[[[[[[       (==(102)==)
      CALL VIRSOF(EBEAM,XK0,C,BORN,CORR)
      XWT(10)=BORN*(1.D0+CORR)*(1.D0-C)**2*ZSOFT
      DO 52 K=1,3
      CALL VIRSEL(K,EBEAM,XK0,C,WBORN,CORR)
      XWT(10+K) =WBORN*(1.D0+CORR)*(1.D0-C)**2
   52 CONTINUE
      WTM   =  XWT(10+KEYSIN)
      XWT(2)=  WTM
C]]]]]]]]]]
      WT(8)=WT(8)+WTM
      WT(9)=WT(9)+WTM*WTM
      IF(WTM.LT.0.D0)   WT(15)=WT(15)+1.D0
      IF(WTM.GT.WMAX)   WT(16)=WT(16)+1.D0
      IF(WTM.LT.WT(17)) WT(17)=WTM
      IF(WTM.GT.WT(18)) WT(18)=WTM
C-------------------------------------------- REJECT W VALUES -------
      CALL VARRAN(DRVEC,1)
      RN=DRVEC(1)
C principal weight
      CALL WMONIT(0,71,WTM,WMAX,RN)
      CALL WMONIT(0,70,WTM,WMAX,RN)
C auxiliary weights
      CALL VIRSEL(1,EBEAM,XK0,C,WBORN,CORR)
      WTM1=WBORN*(1.D0+CORR)*(1.D0-C)**2
      CALL VIRSEL(KEYSIN,EBEAM,XK0,C,WBORN,CORR)
      WTM2=WBORN*(1.D0+CORR)*(1.D0-C)**2
      CALL WMONIT(0,41,WTM1,WMAX,RN)
      CALL WMONIT(0,42,WTM2,WMAX,RN)
C  (==( 101 )==)
      IF(KEYWGT.EQ.0.AND.RN*WMAX.GT.WTM) GOTO 6
      IF(KEYWGT.EQ.0) WTM=1D0
      WT(10)=WT(10)+1.D0
C-------------------------------------------- GENERATE FI VALUE -----
      FI=R*TWOPI
C-------------------------------------------- CONSTRUCT MOMENTA -----
      CFI=DCOS(FI)
      SFI=DSIN(FI)
      SC=DSQRT(1.D0-C*C)
      QK(4)=0.D0
      QK(3)=0.D0
      QK(2)=0.D0
      QK(1)=0.D0
      QP(4)=1.D0
      QP(3)=C
      QP(2)=SC*SFI
      QP(1)=SC*CFI
      DO 12 I=1,3
   12 QM(I)=-QP(I)
      QM(4)=1.D0
      DO 15 I=1,4
      QP(I)=EBEAM*QP(I)
   15 QM(I)=EBEAM*QM(I)
      CALL KINBIS
C     ===================================
      ELSEIF(MODE.EQ.1.OR.MODE.EQ.2) THEN
C     ===================================
CCCCCCCCC                       (==( 004 )==)
      NPAR(10)= NEVGEN
      NPAR(20)= NEVGEN
      CALL WMONIT(1,70,AWT70,DWT70,DUMM3)
      CALL WMONIT(1,71,AWT71,DWT71,DUMM3)
      CALL WMONIT(1,72,AWT72,DWT72,DUMM3)
      XST    =  SIGT*AWT70
      DXST   =  XST *DWT70
      XSS    =  SIGS*AWT71
      DXSS   =  XSS *DWT71
      XSH    =  SIGH*AWT72
      DXSH   =  XSH *DWT72
      XST2   =  XSS+XSH
      DXST2  =  SQRT(DXSS**2 +DXSH**2)
      XPAR( 9)= SIG0
C for unweighted events, WTM=1D0
      XPAR(10)=  XST2
      XPAR(11)= DXST2/XST2
      XPAR(12)=  XSS
      XPAR(13)= DXSS
      XPAR(14)=  XSH
      XPAR(15)= DXSH
C for WEIGHTED events
      XPAR(20)= SIGS+SIGH
      XPAR(21)= 0D0
      XPAR(22)= SIGS
      XPAR(23)= 0D0
      XPAR(24)= SIGH
      XPAR(25)= 0D0
C for unweighted events, WTM=1D0
      IF(KEYWGT.EQ.0) THEN
        DO 313 I=20,25
  313   XPAR(I)=XPAR(I-10)
      ENDIF
C ...
      CALL WMONIT(1,41,AWT41,DWT41,DUMM3)
      CALL WMONIT(1,51,AWT51,DWT51,DUMM3)
      XSA    =  SIGS*AWT41
      DXSA   =  XSA *DWT41
      XAH    =  SIGH*AWT51
      DXAH   =  XSH *DWT51
      XSA1   =  XSA+XAH
      DXSA1  =  SQRT(DXSA**2 +DXAH**2)
      XPAR(41)=  XSA1
      XPAR(51)= DXSA1
      XPAR(60)=  XSA
      XPAR(70)= DXSA
      XPAR(61)=  XAH
      XPAR(71)= DXAH
      CALL WMONIT(1,42,AWT42,DWT42,DUMM3)
      CALL WMONIT(1,52,AWT52,DWT52,DUMM3)
      XSA    =  SIGS*AWT42
      DXSA   =  XSA *DWT42
      XAH    =  SIGH*AWT52
      DXAH   =  XAH *DWT52
      XSA2   =  XSA+XAH
      DXSA2  =  SQRT(DXSA**2 +DXAH**2)
      XPAR(42)=  XSA2
      XPAR(52)= DXSA2
      IF(MODE.EQ.1) RETURN
C     ====================
CCCCCCC        (==( 005 )==)
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '        OUTPUT FROM              '
      WRITE(NOUT,BXTXT) '  OLDBIS:        WINDOW A        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '   X.sect. in [nb] units         '
      WRITE(NOUT,BXL1I) NEVGEN,     ' generated events  ','NEVGEN','A0'
      WRITE(NOUT,BXL2F) XSS ,DXSS  ,'Xsec.      soft    ','      ','A1'
      WRITE(NOUT,BXL2F) XSH ,DXSH  ,'Xsec.      hard    ','      ','A2'
      WRITE(NOUT,BXL2F) XST ,DXST  ,'Xsec. straight     ',' total','A3'
      WRITE(NOUT,BXL2F) XST2,DXST2 ,'Xsec. clever       ',' total','A4'
      WRITE(NOUT,BXTXT) '   More on weights etc...        '
      WRITE(NOUT,BXL1F) SIGS ,      'crude Xs. soft     ','      ','A5'
      WRITE(NOUT,BXL1F) SIGH ,      'crude Xs. hard     ','      ','A6'
      DWT71=DWT71*AWT71
      DWT72=DWT72*AWT72
      WRITE(NOUT,BXL2F) AWT71,DWT71,'aver. wt. soft     ','      ','A7'
      WRITE(NOUT,BXL2F) AWT72,DWT72,'aver. wt. hard     ','      ','A8'
      WRITE(NOUT,BXL1F) SIG0 ,      'Born  Xs.          ','      ','A9'
      WRITE(NOUT,BXCLO)
C ...
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '  OLDBIS:        WINDOW B        '
      WRITE(NOUT,BXTXT) '     auxiliary information       '
      WRITE(NOUT,BXL2F) XSA1,DXSA1 ,'KSIN=1, no interf. ',' total','B1'
      WRITE(NOUT,BXL2F) XSA2,DXSA2 ,'KSIN=2, with interf',' total','B2'
      WRITE(NOUT,BXCLO)
 
C old output routine---------
      CALL BABINF
      ENDIF
      END
      SUBROUTINE BABINF
C-------------------------------------------- REMARKS ---------------
C INFO ROUTINE TO BE CALLED AFTER 'BHABHA' HAS RUN EVENTS. IT CALCULA
C THE EXACT CROSS SECTION CORRESPONDING TO THE GENERATED EVENT SAMPLE
C AND SOME STATISTICS ON THE GENERATION.
C WT( 1) = NO.OF STARTS IN HARD-PHOTON PART;
C WT( 2) = NO. OF TRIALS INSIDE C-GENERATION W.R.P. LOOP;
C WT( 3) = NO. OF HARD PHOTON TRIALS SURVIVING C & CT CUTS;
C WT( 4) = SUM OF HARD PHOTON WEIGHTS;
C WT( 5) = SUM OF SQUARED HARD PHOTON WEIGHTS;
C WT( 6) = NO. OF ACCEPTED HARD PHOTON EVENTS;
C WT( 7) = NO. OF STARTS OF SOFT-PHOTON PART;
C WT( 8) = SUM OF SOFT PHOTON WEIGHTS;
C WT( 9) = SUM OF SQUARED SOFT PHOTON WEIGHTS;
C WT(10) = NO. OF ACCEPTED SOFT PHOTON EVENTS;
C WT(11) = NO. OF HARD EVENTS WITH W < 0;
C WT(12) = NO. OF HARD EVENTS WITH W > WMAX;
C WT(13) = MINIMUM GENERATED WEIGHT IN HARD PART;
C WT(14) = MAXIMUM GENERATED WEIGHT IN HARD PART;
C WT(15) = NO. OF SOFT EVENTS WITH W < 0;
C WT(16) = NO. OF SOFT EVENTS WITH W > WMAX;
C WT(17) = MINIMUM GENERATED WEIGHT IN SOFT PART;
C WT(18) = MAXIMUM GENERATED WEIGHT IN HARD PART.
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / PARM02 / SIG0,SIGS,SIGH,SIGT,WT
      DIMENSION WT(18)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      DATA DH/0.D0/,DHE/0.D0/,DDHE/0.D0/,EC/0.D0/
      DATA ECT/0.D0/,EWH/0.D0/,DS/0.D0/,DSE/0.D0/
      DATA DDSE/0.D0/,EWS/0.D0/,DT/0.D0/,DTE/0.D0/,DDTE/0.D0/
      IF(WT(1).EQ.0.D0) GOTO 1
      SIGHE=SIGH*WT(4)/WT(1)
      DSIGHE=SIGH/WT(1)*DSQRT(WT(5)-WT(4)**2/WT(1))
      DH  =SIGH/SIG0
      DHE =SIGHE/SIG0
      DDHE=DSIGHE/SIG0
      EC=WT(1)/WT(2)
      ECT=WT(3)/WT(1)
      EWH=WT(6)/WT(3)
    1 CONTINUE
      IF(WT(7).EQ.0.D0) GOTO 2
      SIGSE=SIGS*WT(8)/WT(7)
      DSIGSE=SIGS/WT(7)*DSQRT(WT(9)-WT(8)**2/WT(7))
      DS  =SIGS/SIG0
      DSE =SIGSE/SIG0
      DDSE=DSIGSE/SIG0
      EWS=WT(10)/WT(7)
    2 CONTINUE
      DT  =SIGT/SIG0
      DTE =DHE+DSE
C&&&& DDTE=DDHE+DDSE
      DDTE=SQRT(DDHE**2+DDSE**2)
      SIGTOT=DTE*SIG0
      SIGSFT=DSE*SIG0
      SIGHRD=DHE*SIG0
C-----
      WRITE(NOUT,3) SIG0,SIGH,SIGS,SIGT,(WT(I),I=1,18)
    3 FORMAT(1H0,90(1H-)/,' BHABHA SAMPLE STATISTICS'/,(4D15.6))
      WRITE(NOUT,1004) SIG0,DH,DHE,DDHE,EC,ECT,EWH,DS,DSE,DDSE
      WRITE(NOUT,1005) EWS,DT,DTE,DDTE,SIGTOT,SIGSFT,SIGHRD
 1004 FORMAT(
     . '          LOWEST ORDER CROSS SECTION =',D15.6,' NB = UNIT'/,
     . '   APPROXIMATED HARD PHOTON XSECTION =',F15.6/,
     . '          EXACT HARD PHOTON XSECTION =',F15.6/,
     . '                         UNCERTAINTY =',F15.6/,
     . ' W.R.P EFFICIENCY IN INTERNAL C LOOP =',F15.6/,
     . '   "      "      OF C/CT RESTRICTION =',F15.6/,
     . '   "      "         FOR HARD WEIGHTS =',F15.6/,
     . '   APPROXIMATED SOFT PHOTON XSECTION =',F15.6/,
     . '          EXACT SOFT PHOTON XSECTION =',F15.6/,
     . '                         UNCERTAINTY =',F15.6)
 1005 FORMAT(
     . '  W.R.P. EFFICIENCY FOR SOFT WEIGHTS =',F15.6/,
     . '    APPROXIMATED TOTAL CROSS SECTION =',F15.6/,
     . '           EXACT TOTAL CROSS SECTION =',F15.6/,
     . '                         UNCERTAINTY =',F15.6/,
     . ' ----------> TOTAL CROSS SECTION =====',D15.6,' NB'/,
     . ' ---------->  SOFT CROSS SECTION =====',D15.6,' NB'/,
     . ' ---------->  HARD CROSS SECTION =====',D15.6,' NB')
      WRITE(NOUT,5) (WT(I),I=11,18)
    5 FORMAT('0 GENERATED WEIGHTS:'/,
     . '          < 0 IN HARD PART =',F15.6/,
     . '       > WMAX IN HARD PART =',F15.6/,
     . '      MINIMUM IN HARD PART =',F15.6/,
     . '      MAXIMUM IN HARD PART =',F15.6/,
     . '          < 0 IN SOFT PART =',F15.6/,
     . '       > WMAX IN SOFT PART =',F15.6/,
     . '      MINIMUM IN SOFT PART =',F15.6/,
     . '      MAXIMUM IN SOFT PART =',F15.6)
      END
      SUBROUTINE VIRSEL (KEY,EBEAM,XK0,COST,WBORN,CORR)
C     ************************************************
C new routine, not present in OLDBAB     (==(102)==)
C......COMPACT FORMULA FOR CROSS SECTION IN BHABHA PROCESS
C......BORN + SOFT +VIRTUAL CORRECTIONS... ONLY QED VERSION
C......MOST OF THE FORMULAS FROM
C......BOHM,DENNIER,HOLLIK,NUCL.PHYS.B304(1988),687
C......COMPLETED E. RICHTER-WAS. APRIL 1990.........
*.....AMGAM IS DUMMY PARAMETR
*.....KEY=1   T NONINTERFERENCE CHANNEL
*.....KEY=2   T CHANNEL
*.....KEY=3 S+T CHANNEL
*     ********************
      IMPLICIT REAL*8(A-H,O-Z)
      DATA PI,ALFINV /3.1415926535897932D0, 137.03604D0/
      DATA AMGAM,AMEL /1D0, 0.511D-3/
 
      ALFA=1D0/ALFINV
      ALFPI=ALFA/PI
      AKMAX=XK0*EBEAM
      S=4D0*EBEAM**2
      T=-1/2D0*S*(1D0-COST)
      U=-1/2D0*S*(1D0+COST)
      DLT=DLOG(-T/AMEL**2)
      DLS=DLOG( S/AMEL**2)
      DLU=DLOG(-U/AMEL**2)
      DLTG=DLOG(-T/AMGAM**2)
      DLSG=DLOG( S/AMGAM**2)
      DLKG=DLOG(2D0*AKMAX/AMGAM)
      DLKGM=DLOG(AMEL**2*AKMAX**2/AMGAM**2/EBEAM**2)
      DLUS=DLOG(-U/S)
      DLTS=DLOG(-T/S)
      DLTU=DLOG(T/U)
C........S + T CHANNEL...............
      IF(KEY.EQ.3) THEN
C....BORN LEVEL
      XT1=U/T
      XT3=S/T
      XS1=U/S
      XS2=T/S
      XT=XT1**2+XT3**2
      XS=XS1**2+XS2**2
      XST=2D0*XS1*XT1
      BORN=XT+XS+XST
C....VIRTUAL CORRECTIONS
      VIRT =ALFPI*(
     $      -1D0+PI*PI/12D0+1/4D0*DLT+1/4D0*DLT**2
     $      +1/2D0*DLTG*(1D0-DLT))
      DVIRT=4D0*XT*VIRT
      VIRS =ALFPI*(
     $      -1D0+PI*PI/3D0+1/4D0*DLS+1/4D0*DLS**2
     $      +1/2D0*DLSG*(1D0-DLS))
      DVIRS=4D0*XS*VIRS
      DVIRST  =2D0*(VIRT+VIRS)*XST
C.....BOX CORRECTIONS
      BOXS1= 2D0*ALFPI*(-DLTU*DLSG
     $      +S/2D0/(S+T)*DLTS-S*(S+2D0*T)/4D0/(S+T)**2*DLTS**2)
      BOXS2= 2D0*ALFPI*(-DLTU*DLSG
     $      -S/2D0/(S+U)*DLUS+S*(S+2D0*U)/4D0/(S+U)**2*DLUS**2)
      BOXT1= 2D0*ALFPI*(+DLUS*DLTG
     $      -T/2D0/(T+S)*DLTS-T*(T+2D0*S)/4D0/(T+S)**2*DLTS**2)
      BOXT3= 2D0*ALFPI*(+DLUS*DLTG
     $ +T/2D0/(T+U)*DLTU+T*(T+2D0*U)/4D0/(T+U)**2*(DLTU**2+PI*PI))
      DBOXT=BOXT1*XT1**2+BOXT3*XT3**2
      DBOXS=BOXS1*XS1**2+BOXS2*XS2**2
      DBOXST=1/2D0*(BOXS1+BOXT1)*XST
C.....SOFT CORRECTIONS
      XS=2D0*ALFPI*(
     $      (DLS-1D0)*DLKGM+1/2D0*DLS**2-PI*PI/3D0       )
      XT=2D0*ALFPI*(
     $      (DLT-1D0)*DLKGM+1/2D0*DLT**2
     $      -1/2D0*DLTS**2+PI*PI/6D0
     $      -2D0*DILOG(1D0+2D0*EBEAM/DSQRT(-T))
     $      -2D0*DILOG(1D0-2D0*EBEAM/DSQRT(-T))   )
      XU=2D0*ALFPI*(
     $      (DLU-1D0)*DLKGM+1/2D0*DLU**2
     $      -1/2D0*DLUS**2+PI*PI/6D0
     $      -2D0*DILOG(1D0+2D0*EBEAM/DSQRT(-U))
     $      -2D0*DILOG(1D0-2D0*EBEAM/DSQRT(-U))   )
      SOFT=XS+XT-XU
C.....ADDED TOGETHER
      CORS=DVIRS+DBOXS
      CORT=DVIRT+DBOXT
      CORST=DVIRST+DBOXST
      CORSOFT=BORN*SOFT
      BORNCOR=BORN+CORSOFT+CORS+CORT+CORST
      CORR=(BORNCOR-BORN)/BORN
      ENDIF
C.....end of...S + T CHANNEL...end of............
C........ T CHANNEL...............
      IF(KEY.EQ.2) THEN
C....BORN LEVEL
      XT1=U/T
      XT3=S/T
      XT=XT1**2+XT3**2
      BORN=XT
C....VIRTUAL CORRECTIONS
      VIRT =ALFPI*(
     $      -1D0+PI*PI/12D0+1/4D0*DLT+1/4D0*DLT**2
     $      +1/2D0*DLTG*(1D0-DLT))
      DVIRT=4D0*XT*VIRT
C.....BOX CORRECTIONS
      BOXT1= 2D0*ALFPI*(+DLUS*DLTG
     $      -T/2D0/(T+S)*DLTS-T*(T+2D0*S)/4D0/(T+S)**2*DLTS**2)
      BOXT3= 2D0*ALFPI*(+DLUS*DLTG
     $ +T/2D0/(T+U)*DLTU+T*(T+2D0*U)/4D0/(T+U)**2*(DLTU**2+PI*PI))
      DBOXT=BOXT1*XT1**2+BOXT3*XT3**2
C.....SOFT CORRECTIONS
      XS=2D0*ALFPI*(
     $      (DLS-1D0)*DLKGM+1/2D0*DLS**2-PI*PI/3D0       )
      XT=2D0*ALFPI*(
     $      (DLT-1D0)*DLKGM+1/2D0*DLT**2
     $      -1/2D0*DLTS**2+PI*PI/6D0
     $      -2D0*DILOG(1D0+2D0*EBEAM/DSQRT(-T))
     $      -2D0*DILOG(1D0-2D0*EBEAM/DSQRT(-T))   )
      XU=2D0*ALFPI*(
     $      (DLU-1D0)*DLKGM+1/2D0*DLU**2
     $      -1/2D0*DLUS**2+PI*PI/6D0
     $      -2D0*DILOG(1D0+2D0*EBEAM/DSQRT(-U))
     $      -2D0*DILOG(1D0-2D0*EBEAM/DSQRT(-U))   )
      SOFT=XS+XT-XU
C.....ADDED TOGETHER
      CORT=DVIRT+DBOXT
      CORSOFT=BORN*SOFT
      BORNCOR=BORN+CORSOFT+CORT
      CORR=(BORNCOR-BORN)/BORN
      ENDIF
C....end of....T  CHANNEL........end of........
C........ T non interference CHANNEL...............
      IF(KEY.EQ.1) THEN
C....BORN LEVEL
      XT1=U/T
      XT3=S/T
      XT=XT1**2+XT3**2
      BORN=XT
C....VIRTUAL CORRECTIONS
      VIRT =ALFPI*(
     $      -1D0+PI*PI/12D0+1/4D0*DLT+1/4D0*DLT**2
     $      +1/2D0*DLTG*(1D0-DLT))
C.....SOFT CORRECTIONS
      SOFT=2D0*ALFPI*(
     $      (DLT-1D0)*DLKGM+1/2D0*DLT**2
     $      -1/2D0*DLTS**2+PI*PI/6D0
     $      -2D0*DILOG(1D0+2D0*EBEAM/DSQRT(-T))
     $      -2D0*DILOG(1D0-2D0*EBEAM/DSQRT(-T))   )
C.....ADDED TOGETHER
      CORR=SOFT+4D0*VIRT
      ENDIF
C....end of....T non interference  CHANNEL........end of........
      WBORN = BORN/8D0
      END
 
      SUBROUTINE VIRSOF(EB,XK0,X,BORN,CORR)
C-------------------------------------------- REMARKS ---------------
C BHABHA SCATTERING DIFFERENTIAL CROSS SECTION WITH CORRECTIONS:
C 1) SELF-ENERGY DIAGRAMS FOR IN- AND OUTGOING LEPTONS;
C 2) VERTEX CORRECTION DIAGRAMS;
C 3) BOX DIAGRAMS (TWO-PHOTON EXCHANGE);
C 4) SOFT BREMSSTRAHLUNG (PHOTON ENERGY < XK0*EBEAM );
C 5) VACUUM POLARIZATION (PHOTON SELF-ENERGY DIAGRAMS);
C 6) INTERFERENCE BETWEEN PHOTON AND Z0 EXCHANGE GRAPHS.
C FORMULA AND CONVENTIONS TAKEN FROM:
C F.A.BERENDS ET AL, NUCL.PHYS.B68(1974)541.
C EB   = BEAM ENERGY IN GEV;
C XK0  = CUTOFF ON SOFT BREMSSTRAHLUNG ENERGY;
C X    = COSINE OF POLAR SCATTERING ANGLE OF POSITRON;
C BORN = LOWEST-ORDER DIFFERENTIAL CROSS SECTION IN NANOBARN;
C CORR = TOTAL OF CORRECTIONS GIVEN ABOVE.
C ASSUMED VALUES: 90 GEV FOR THE Z0 MASS, .23 FOR SIN**2(TH).
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / INOUT  / NINP,NOUT,NOUT2
C-------------------------------------------- LOWEST ORDER ----------
      X2=X*X
      X3=X2*X
      X4=X3*X
      XNUM=9.D0+6.D0*X2+X4
      BORN=1.295958D0/EB**2*XNUM/(1.-X)**2
C-------------------------------------------- CORRECTIONS (NO VAC.POL
      U=2.D0*DLOG(3.9139D03*EB)
      V=U+DLOG(1.+X)-0.6931472D0
      W=U+DLOG(1.-X)-0.6931472D0
      CORR=2.3228D-03*(
     .-4.D0*(1.D0-U+V-W)*DLOG(XK0) - 6.5797D0 + U*U - V*V + W*W
     .+ 2.*DILOG((1.D0+X)/2.D0) - 2.*DILOG((1.D0-X)/2.D0)
     .+ ( ( 1.-12.*X+12.*X2 -4.*X3 +3.*X4)*U
     .   -( 5. -7.*X +3.*X2    -X3       )*V
     .   +(31. +5.*X +9.*X2 +3.*X3       )*W
     .   +( 3. +7.*X -5.*X2 -3.*X3 -2.*X4)*U*U*.5
     .   +( 3. -3.*X    +X2    -X3       )*V*V
     .   -( 9. +7.*X+11.*X2 +5.*X3       )*W*W*.5
     .   -( 2.    -X           -X3       )*U*V*X*2.
     .   -(21. +3.*X +9.*X2 -3.*X3 +2.*X4)*U*W
     .   +( 6. +5.*X +4.*X2    +X3       )*V*W*2.
     .   -(36.      +24.*X2        +4.*X4)
     .   +(18.-15.*X+12.*X2 -3.*X3 +4.*X4)*3.2899)/XNUM)
C<<<<<--------------------------------------- VACUUM POLARIZATION ---
C[[[[[       (==( 103 )=)
      CORR=CORR+
     .(6.*X -6.*X2 +2.*X3 -2.*X4)*REPI(4.*EB*EB)/XNUM+
     .(-18.-6.*X-6.*X2-2.*X3)*REPI(-2.*EB*EB*(1.-X))/XNUM
C<<<<---------------------------------------- INTERFERENCE WITH Z0 --
C&&&  CORR=CORR + XSWEAK(EB,9.D01,.23D0,X)/BORN
CCCC  CORR=CORR + XSWEAK(EB,92D0 ,0.2288D0,X)/BORN
CCCCCCCCCCC (==( 104 )==)
CCCC  Note outdated Z mass, width and SINW2 here !!!!
      CORR=CORR + YYWEAK(EB,92D0,2.4533D0,0.2288D0,X)
      Y=BORN*(1.+CORR)*1000.D0
CCC   IF(CORR.LT.-1.D0) WRITE(NOUT,1) CORR,EB,XK0,X
CCC 1 FORMAT(' ***VIRSOF WARNING : ',4D15.6)
      END
CCCCCCCCCCCCCCCC (==( 104 )==)
C slightly improved version of XSWEAK with finite width of Z resonance
CCCCCCCCCCCCCCCC
C BHABHA CROSS SECTION DUE TO INTERFERENCE OF THE PHOTON AND Z0 GRAPH
C EB  = BEAM ENERGY (GEV)
C XMZ0= Z0 MASS (GEV)
C GAMZ0= Z0 WIDTH (GEV)
C SIN2= SIN(WEAK MIXING ANGLE)**2
C X   = COSINE OF POLAR SCATTERING ANGLE OF POSITRON
      FUNCTION YYWEAK(EB,XMZ0,GAMZ0,SIN2,X)
*     *************************************
      IMPLICIT REAL*8(A-H,O-Z)
      XM=1.-X
      A2=1./(16.*SIN2*(1.-SIN2))
      VP=((1.-4.*SIN2)**2+1.)*A2
      VM=VP-2.*A2
      SS=4*EB**2
      TT= SS*(1-X)/2D0
C (==( 104 )==)
CC    XS=SS/(SS-XMZ0**2 )
      XS=SS*(SS-XMZ0**2)/((SS-XMZ0**2)**2+(XMZ0*GAMZ0)**2)
      XT=TT/(TT+XMZ0**2 )
      XP=(1.+X)**2
CC    FACT=5.183833D0/EB**2
CC    BORN=1.295958D0/EB**2*XNUM/(1.-X)**2
      XNUM=9.D0+6.D0*X**2+X**4
      FACT= 4D0/XNUM*XM**2
      YYWEAK=FACT*( XS*( VP*XP + VM*XM**2 )/4.
     $            + XT*( VP*XP + VM*4.    )/(XM*XM)
     $        -(XS+XT)*( VP*XP            )/(2.*XM) )
      END
C==================== not used anymore...
      FUNCTION XSWEAK(EB,XMZ0,SIN2,X)
C-------------------------------------------- REMARKS ---------------
C BHABHA CROSS SECTION DUE TO INTERFERENCE OF THE PHOTON AND Z0 GRAPH
C EB  = BEAM ENERGY (GEV)
C XMZ0= Z0 MASS (GEV)
C SIN2= SIN(WEAK MIXING ANGLE)**2
C X   = COSINE OF POLAR SCATTERING ANGLE OF POSITRON
C THE COUPLING CONSTANTS OF THE ELECTRONS TO THE Z0 ARE CALCULATED
C ACCORDING TO THE STANDARD SU(2)*U(1) MODEL, USING SIN2. THE MASS
C XMZ0 IS TREATED AS AN ADDITIONAL, INDEPENDENT PARAMETER.
C NEITHER THE PURE Z0 CHANNEL, NOR THE EFFECTS OF A NONZERO Z0 WIDTH
C ARE TAKEN INTO ACCOUNT ---> THIS ROUTINE IS NOT GOOD FOR LEP/SLC.
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DATA INIT /0/
      IF(INIT.NE.0) GOTO 1
      INIT=1
C-------------------------------------------- INITIALIZATION --------
      A2=1./(16.*SIN2*(1.-SIN2))
      VP=((1.-4.*SIN2)**2+1.)*A2
      VM=VP-2.*A2
      CHIQ=(XMZ0/EB)**2/2.
      XS=1./(1.-CHIQ/2.)
      FACT=5.183833D0/EB**2
C--------------------------------------------------------------------
    1 XM=1.-X
      XP=(1.+X)**2
      XT=1./(1.+CHIQ/XM)
      XSWEAK=FACT*( XS*( VP*XP + VM*XM**2 )/4.
     .            + XT*( VP*XP + VM*4.    )/(XM*XM)
     .        -(XS+XT)*( VP*XP            )/(2.*XM) )
      RETURN
      END
 
      SUBROUTINE KINBIS
C     *****************
C TRANSTATES /MOMBAB/ INTO /MOMSET/
C     ************************
      IMPLICIT REAL*8(A-H,O-Z)
C from OLDBIS
      COMMON / PARM01 / EBEAM,THMIN,THMAX,XKMIN,XKMAX,CMIN,CMAX
      COMMON / MOMBAB / QP(4),QM(4),QK(4),WTM,XWT(20)
C to BHLUMI
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
      AMEL  =  0.5111D-3
C beams
      P1(4)= EBEAM
      P1(3)= DSQRT(EBEAM**2-AMEL**2)
      P1(2)= 0D0
      P1(1)= 0D0
C...
      Q1(4)= EBEAM
      Q1(3)=-DSQRT(EBEAM**2-AMEL**2)
      Q1(2)= 0D0
      Q1(1)= 0D0
C final electrons
      DO 10 K=1,4
      P2(K)=QP(K)
      Q2(K)=QM(K)
   10 CONTINUE
C photon, if present
      NPHOT=0
      IF(QK(4).NE.0D0) THEN
       NPHOT=1
       DO 20 K=1,4
   20  PHOT(1,K)=QK(K)
      ENDIF
C weights
      WTCRU1=1D0
      WTCRU2=1D0
      WTMOD    = WTM
      DO 70 K=1,20
   70 WTSET(K) = XWT(K)
C----------------------------------------------------------------C
C                      The end of OLDBIS                         C
C----------------------------------------------------------------C
      END
 
 
 
      SUBROUTINE LUMLOG(MODE,XPAR,NPAR)
C     *********************************
C===================================================================
C===================================================================
C===BBB=======BBB==BBB===BBB===BBB===BBB=======BBBBBB=====BBBBBBB===
C===BBB=======BBB==BBB===BBB===BBB===BBB======BBBBBBBB===BBBBBBBBB==
C===BBB=======BBB==BBB===BBB=B=BBB===BBB======BBB==BBB===BBB========
C===BBB=======BBB==BBB===BBBBBBBBB===BBB======BBB==BBB===BBB==BBBB==
C===BBBBBBBB==BBBBBBBB===BBBB=BBBB===BBBBBBB==BBB==BBB===BBB===BBB==
C===BBBBBBBB===BBBBBB====BBB===BBB===BBBBBBB===BBBBBB=====BBBBBB====
C===================================================================
C
C           **************************************************
C           *       **********************************       *
C           *       *      *******************       *       *
C           *       *      *                 *       *       *
C           *       *      *   L U M L O G   *       *       *
C           *       *      *                 *       *       *
C           *       *      *******************       *       *
C           *       **********************************       *
C           **************************************************
C
C----------------------------------------------------------------C
C                                                                C
C                       LUMLOG                                   C
C                                                                C
C          COLLLINEAR LEADING-LOG MONTE CARLO FOR                C
C               LOW-ANGLE BHABHA SCATTERING                      C
C                     NOVEMBER 1990                              C
C                 last update  5 feb. 91 (pairs)                 C
C                 last update 14 feb. 91 born in robol6          C
C                                     bug in modelu              C
C                 last update  8 apr. 91 cosmetics (sj)          C
C                 last update 26 aug. 91 cosmetics (sj)          C
C         AUTHORS:                                               C
C         S. Jadach, E. Richter-Was, Z. Was, B.F.L. Ward         C
C                                                                C
C The user is kindly requested to cite preprint TH.5995 (1991)   C
C Phys. Lett. B260 (1991) 173 of the same authors.               C
C (Note that TH.5888 is now Phys. Lett. B253 (1991) 469).        C
C----------------------------------------------------------------C
C
C Note that LUMLOG contains originally two MC program: BHALOG which
C is present here and MULTILOG which is not inluded here.
C The series of comparisons between these two programs (TH-5995)
C has leaded the 0.02% technical precision estimate for LUMLOG.
C
C----------------------------------------------------------------------
C                 INPUT and OUTPUT of LUMLOG
C----------------------------------------------------------------------
C All input and output goes through parameters in
C                 CALL LUMLOG(MODE,XPAR,NPAR)
C and through /MOMSET/ and /WGTALL/ common blocks.
C In the following we shall  briefly indicate the meaning of the
C above parameters/variables.
C
C IF( MODE =-1 ) THEN
C ===================
C Initialisation is performed, all input parameters are transfered
C through XPAR and NPAR.
C In the following table we indicate the meaning of NPAR, XPAR
C entries for LUMLOG subgenerator in the initialization mode.
C         Table        Input parameters of LUMLOG
C----------------------------------------------------------------------
C  Entry    Variable   Meaning
C----------------------------------------------------------------------
C  NPAR( 1)  KEYOPT =1000*KEYGEN +10*KEYWGT +KEYRND
C                   General option switch
C            KEYGEN =2 for this sub-generator
C            KEYRND =1,2 type of random number generator RANMAR,RANECU
C            KEYWGT =1 for variable weight WTM
C            KEYWGT =0 for  WTM=1events is NOT IMPLEMENTED!!!
C  NPAR( 2)  KEYRAD =KEYBLO +10*KEYTES, QED option switch
C            KEYTES =  2    normal str. function
C            KEYTES =  1    test str. functions (1-z)**(-1/2)
C            KEYBLO =  3    LLog  ln(s'*xi_dot/me**2)  -1)
C            KEYBLO =  4    LLog  ln(s*xiA/me**2)
C  XPAR( 1)  CMSENE Total center mass energy [GeV]
C  XPAR( 2)   TMINL Theta_minimum [degr]
C  XPAR( 3)   TMAXL Theta_maximum [degr]
C  XPAR( 4)     XK0 Dimensionless infrared cut-off (on real soft
C                   photons) relevant for un-exponentiated case.
C                   Range 0.000001<XKO<0.0001 recommeded.
C  XPAR( 5)   XKMAX Determines minimum effective mass s' of the
C                   final state electron-positron, s'>s*(1-XKMAX).
C                   XKMAX=1 is alowed and recommended.
C----------------------------------------------------------------------
C
C ELSE IF( MODE = 0 ) THEN
C ========================
C Generation of the single Monte Carlo event.
C Only variable weight events are produced (!)
C (the user may turn them, if he wishes, into WT=1 events by himself).
C The four momenta of the final state electron, positron and photon
C are encoded in
C      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
C where P1 and Q1 are four-momenta of positron and elecron beams.
C P2 and Q2 are four-momenta of outgoing positron and electron.
C                       IMPORTANT
C                       ---------
C Remember that P2 and Q2 from LUMLOG do not represent normal "bare"
C electron and positron but "dressed" ones, i.e. they sum four-momenta
C of electron and ALL photons collinear with them
C (as calorimeter does).
C (N.B. Collinearity relation extends to photons with transverse
C momentum up to characteristic LL scale pT_max = sqrt(Q**2)).
C                       ---------
C As a result, the list PHOT(100,4) of photon four-momenta is empty
C and NPHOT=0 (the number of real photons in PHOT).
C The principal weight WTM of the event is placed in
C      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
C It is usually of interest to use "paralel weights" from WTSET.
C The event weight is constructed then as WT= WTCRU1*WTCRU2*WTSET(J).
C Which J is alowed and what version of the QED matrix element
C it represents is summarized in the table below.
C N.B. principal weight WTM= WTCRU1*WTCRU2*WTSET(4).
C All calculation in LUMLOG are Leading-Log initial state bremss. type.
C       Table of WTSET entries for LUMLOG
C----------------------------------------------------------------------
C  Entry      Type of the used electron (non-singl) structure function
C----------------------------------------------------------------------
C             QED order   Exponentiation       pairs
C             ---------------------------------------------------------
C  WTSET( 1)  Zero-th     exponentiated        No
C  WTSET( 2)  First       exponentiated        No
C  WTSET( 3)  Second      exponentiated        No
C  WTSET( 4)  Third       exponentiated        No   (principal weight)
C  WTSET( 5)  Third       exponentiated       Yes
C  WTSET(11)  Zero-th     not exponentiated    No
C  WTSET(12)  First       not exponentiated    No
C  WTSET(13)  Second      not exponentiated    No
C  --------------------------------------------------------------------
C
C ELSE IF( MODE = 1 ) THEN
C ========================
C The total cross section corresponding to generated series of event,
C i.e. resulting from MC integrartion is calculated and stored in XPAR
C and NPAR, see table below.
C  --------------------------------------------------------------------
C  Entry    Variable   Meaning
C  --------------------------------------------------------------------
C  NPAR(20)  NEVGEN  Number of generated MC events
C  XPAR(20)    XCRU  Crude total MC x-section [nb] which is necessary
C                    for rescaling histograms
C                    in run with weighted events.
C  XPAR(21)          =0, error of XPAR(20), it is identicaly zero
C  XPAR(22)    BORN  Born x-cextion [nb]
C  XPAR(25)    SIG0  Miscelanous
C  --------------------------------------------------------------------
C For MODE=1 program is called upon many times in the process of
C rescaling histograms, therefore, there is no output printed in
C this mode.
C
C ELSE IF( MODE = 2 ) THEN
C ========================
C
C Only in this MODE=2 in addition to filling XPAR and NPAR as
C for MODE=1 the values of various x-sections are printed on
C the standard output file.
C
C ENDIF
C ====
C
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XPAR(*),NPAR(*)
C...
      CALL BHALOG(MODE,XPAR,NPAR)
      END
 
      SUBROUTINE BHALOG(MODE,XPAR,NPAR)
C     ***********************************
C----------------------------------------------------------------C
C            BHALOG is part of LUMLOG library                    C
C----------------------------------------------------------------C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(PI     =  3.1415926535897932D0)
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(GNANOB =  389385D0)
      PARAMETER(AMEL   =  0.511D-3)
      PARAMETER(X0     =  1D-5)
      DIMENSION XPAR(*),NPAR(*)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,KEYTES
      COMMON / UTIBLO / X1,X2,XI1,XI2,XI
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
      COMMON / RANPAR / KEYRND
      LOGICAL LCONE1,LCONE2,LCONE,LENERG
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      DOUBLE PRECISION DRVEC(100)
      EXTERNAL FUNSKI
 
      IF( MODE.EQ.-1) THEN
C     ********************
C ...BX-formats for nice and flexible outputs
      BXOPE =  '(//1X,15(5H*****)    )'
      BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'
      BXL1I =  '(1X,1H*,I17,                 16X, A20,A12,A7, 1X,1H*)'
      BXL1F =  '(1X,1H*,F17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2F =  '(1X,1H*,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXL1G =  '(1X,1H*,G17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2G =  '(1X,1H*,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXCLO =  '(1X,15(5H*****)/   )'
C .........
      CMSENE = XPAR(1)
      TMINL  = XPAR(2)
      TMAXL  = XPAR(3)
      XK0    = XPAR(4)
      XKMAX  = XPAR(5)
      KEYOPT = NPAR(1)
      KEYRAD = NPAR(2)
C
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '*     *****************         *'
      WRITE(NOUT,BXTXT) '*     ***   LUMLOG  ***         *'
      WRITE(NOUT,BXTXT) '*     *****************         *'
      WRITE(NOUT,BXTXT) '*(P.L. B260 (1991) 173, TH-5995)*'
      WRITE(NOUT,BXTXT) '* This program is now part of   *'
      WRITE(NOUT,BXTXT) '*   BHLUMI version 2.01         *'
      WRITE(NOUT,BXTXT) '*   September      1991         *'
      WRITE(NOUT,BXTXT) '*         AUTHORS               *'
      WRITE(NOUT,BXTXT) '* S. Jadach, E. Richter-Was     *'
      WRITE(NOUT,BXTXT) '*    B.F.L. Ward, Z. Was        *'
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXCLO)
C
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '   ===== LUMLOG/BHALOG  ======   '
      WRITE(NOUT,BXTXT) '   initialization starts....     '
c .........
      KEYBLO = MOD(KEYRAD,10)
      KEYTES = MOD(KEYRAD,100)/10
      KEYRND = MOD(KEYOPT,10)
      KEYWGT = MOD(KEYOPT,100)/10
      IF(TMAXL.GE. 180D0) TMAXL=179.9999999D0
      XIA    = (1-COS(TMINL*PI/180))/2
      XIB    = (1-COS(TMAXL*PI/180))/2
      RAXI   = XIB/XIA
      DSIG0 = 4D0*ALFA**2*PI/CMSENE**2*GNANOB
C  ETA is a dummy parameter present only in crude str. funct.
      ETA  = ALF1*DLOG((CMSENE/AMEL)**2*XIA)
C for callibration test
      IF(KEYTES.EQ.1)  ETA  = 0.5D0
      ZMIN = AMEL/CMSENE
      IF(XKMAX.LT.1D0) ZMIN = 1D0-XKMAX
C ........
      BORNC = DSIG0*(1/XIA-1/XIB)
      BORN  = DSIG0*FOBOR(XIA,XIB)
 
      WRITE(NOUT,BXL1I) KEYOPT,     ' options   switch  ','KEYOPT','N1'
      WRITE(NOUT,BXL1I) KEYRND,     ' VARRAN    switch  ','KEYRND','  '
      WRITE(NOUT,BXL1I) KEYWGT,     ' NOT IMPLEMENTED   ','KEYWGT','  '
      WRITE(NOUT,BXL1I) KEYRAD,     ' radiation switch  ','KEYRAD','N2'
      WRITE(NOUT,BXL1I) KEYTES,     ' KEYTES            ','KEYTES','  '
      WRITE(NOUT,BXL1I) KEYBLO,     ' KEYBLO            ','KEYBLO','  '
      WRITE(NOUT,BXL1I) KEYRND,     ' KEYRND            ','KEYRND','  '
      WRITE(NOUT,BXL1F) CMSENE,     ' CMSENE [GeV]      ','CMSENE','X1'
      WRITE(NOUT,BXL1F) TMINL ,     ' TMIN  [degr.]     ','TMIN  ','X2'
      WRITE(NOUT,BXL1F) TMAXL ,     ' TMAX  [degr.]     ','TMAX  ','X3'
      WRITE(NOUT,BXL1F) XK0   ,     ' xk0 cut (no exp.) ','XK0   ','X4'
      WRITE(NOUT,BXL1F) XKMAX ,     ' minimum sprim/s   ','XKMAX ','X5'
      WRITE(NOUT,BXL1F) RAXI  ,     ' RAXI              ','RAXI  ','  '
      WRITE(NOUT,BXL1F) XIA   ,     ' XIA=(1-cosTMINL)/2','XIA   ','  '
      WRITE(NOUT,BXL1F) XIB   ,     ' XIB=(1-cosTMAXL)/2','XIB   ','  '
      WRITE(NOUT,BXL1F) ETA   ,     ' ETA               ','      ','  '
      WRITE(NOUT,BXL1F) ZMIN  ,     ' ZMIN              ','      ','  '
      WRITE(NOUT,BXL1F) BORNC ,     ' Born crude        ','BORNC ','  '
      WRITE(NOUT,BXL1F) BORN  ,     ' Born exact        ','BORNX ','  '
c ..........
      CALL VESK2W(-1,FUNSKI,DUM1,DUM2,WT)
      DO 12 K=1,5
      CALL WMONIT(-1,   K,DUMM1,DUMM2,DUMM3)
      CALL WMONIT(-1,10+K,DUMM1,DUMM2,DUMM3)
   12 CONTINUE
      CALL MODELU(-1)
      NEVGEN=0
      WRITE(NOUT,BXTXT) '  end of initialization          '
      WRITE(NOUT,BXCLO)
C     ***********************
      ELSEIF( MODE.EQ.0) THEN
C     ***********************
      NEVGEN=NEVGEN+1
C==============================================================
C==========================CRUDE DISTR.========================
C generation of (x1,x2)
      CALL VESK2W(0,FUNSKI,DUM1,DUM2,WTVES)
      XMAX=  1-ZMIN
      XX1=0D0
      XX2=0D0
      IF(TT1.GT.X0**ETA) XX1 = XMAX*DEXP(1/ETA*DLOG(TT1))
      IF(TT2.GT.X0**ETA) XX2 = XMAX*DEXP(1/ETA*DLOG(TT2))
      X1 = XX1
      X2 = XX2
      Z1 = 1D0-XX1
      Z2 = 1D0-XX2
      CALL VARRAN(DRVEC,1)
      RN=DRVEC(1)
      IF(Z1.LE.Z2) THEN
C generation of xi2 according to 1/xi**2 distribution
         XI2 =  XIA*XIB/(XIB*(1D0-RN)+XIA*RN)
C calculate the other xi1
         XI  =  XI2*Z2/(Z1+XI2*(Z2-Z1))
         XI1 =  XI *Z2/(Z1*(1-XI) +Z2*XI)
C for callibration test
         IF(KEYTES.EQ.1) XI1 = XI2*(Z2/Z1)**2
      ELSE
C generation of xi1 according to 1/xi**2 distribution
         XI1 =  XIA*XIB/(XIB*(1D0-RN)+XIA*RN)
C calculate the other xi2
         XI  =  XI1*Z1/(Z2+XI1*(Z1-Z2))
         XI2 =  XI *Z1/(Z2*(1-XI) +Z1*XI)
C for callibration test
         IF(KEYTES.EQ.1) XI2 = XI1*(Z1/Z2)**2
      ENDIF
C symmetric angular trigger
      LCONE1 = XI1.GT.XIA.AND.XI1.LT.XIB
      LCONE2 = XI2.GT.XIA.AND.XI2.LT.XIB
      LCONE  = LCONE1.AND.LCONE2
      LENERG = Z1*Z2 .GT. ZMIN
      WTRIG=0D0
CC No cut on z1*z2 = 1-v any more (stj sept. 91)
      IF(LCONE) WTRIG=1D0
CCC (stj. 16-sept)
CCC   IF(LCONE.AND.LENERG)      WTRIG =1D0
CCC   IF(KEYTES.EQ.1.AND.LCONE) WTRIG =1D0
      WTCRU1 = WTVES
      WTCRU2 = WTRIG
      WTCRUD = WTCRU1*WTCRU2
C calculate four-momenta for accepted events
CCC (stj, 16-sept)
CCC   IF(WTCRUD.NE.0D0) CALL KINOLT
      IF( LCONE ) CALL KINOLT
C==============================================================
C=======================MODEL DISTR.===========================
C Born Factor
      WTBOR = (1+(1-XI)**2)/2D0
C Two types of the big-log definition
      IF(KEYBLO.EQ.3) THEN
        SPRIM = CMSENE**2*Z1*Z2
        BETA  = ALF1*(DLOG(SPRIM*XI/AMEL**2) -1)
      ELSEIF(KEYBLO.EQ.4) THEN
        BETA  = ALF1* DLOG((CMSENE/AMEL)**2*XIA)
c[[[[[        BETA  = ALF1* DLOG((CMSENE/AMEL)**2*XIB)
      ELSE
        WRITE(6,*) ' WRONG KEYBLO=', KEYBLO
      ENDIF
C .....  in order to assure numerical stability
C .....  factors x**(beta-1) and x**(eta-1) are divided off by hand,
C .....  here and in in STRUFU.
C .....  (Similar procedure in FUNSKI is independent.)
C===  DDCRU =  ETA*XX1**(ETA-1D0)*(1D0+(1D0-XX1)**2)/2D0
C=== $        *ETA*XX2**(ETA-1D0)*(1D0+(1D0-XX2)**2)/2D0
      DDCRU =  ETA*               (1D0+(1D0-XX1)**2)/2D0
     $        *ETA*               (1D0+(1D0-XX2)**2)/2D0
     $        *XMAX**( ETA-BETA)*TT1**(1-BETA/ETA)
     $        *XMAX**( ETA-BETA)*TT2**(1-BETA/ETA)
C zero order
      DDMZR     =  STRUFU(310,BETA,XX1)*STRUFU(310,BETA,XX2)
      WTSET( 1) =  DDMZR/DDCRU*WTBOR
      CALL WMONIT( 0, 1,WTCRUD*WTSET( 1),1D0,0D0)
C first order
      DDMO1     =  STRUFU(311,BETA,XX1)*STRUFU(311,BETA,XX2)
      WTSET( 2) =  DDMO1/DDCRU*WTBOR
      CALL WMONIT( 0, 2,WTCRUD*WTSET( 2),1D0,0D0)
      CALL WMONIT( 0,12,WTCRUD*(WTSET(2)-WTSET(1)),1D0,0D0)
C second order
      DDMO2     =  STRUFU(312,BETA,XX1)*STRUFU(312,BETA,XX2)
      WTSET( 3) =  DDMO2/DDCRU*WTBOR
      CALL WMONIT( 0, 3,WTCRUD*WTSET( 3),1D0,0D0)
      CALL WMONIT( 0,13,WTCRUD*(WTSET(3)-WTSET(2)),1D0,0D0)
C third order
      DDMO3     =  STRUFU(313,BETA,XX1)*STRUFU(313,BETA,XX2)
      WTSET( 4) =  DDMO3/DDCRU*WTBOR
      CALL WMONIT( 0, 4,WTCRUD*WTSET( 4),1D0,0D0)
      CALL WMONIT( 0,14,WTCRUD*(WTSET(4)-WTSET(3)),1D0,0D0)
C third order + pairs
      BETR = -3D0*DLOG(1-BETA/3D0)
      DDCRR =  ETA*               (1D0+(1D0-XX1)**2)/2D0
     $        *ETA*               (1D0+(1D0-XX2)**2)/2D0
     $        *XMAX**( ETA-BETR)*TT1**(1-BETR/ETA)
     $        *XMAX**( ETA-BETR)*TT2**(1-BETR/ETA)
      DDMO4     =  STRUFU(313,BETR,XX1)*STRUFU(313,BETR,XX2)
      WTSET( 5) =  DDMO4/DDCRR*WTBOR
      CALL WMONIT( 0, 5,WTCRUD*WTSET( 5),1D0,0D0)
      CALL WMONIT( 0,15,WTCRUD*(WTSET(5)-WTSET(4)),1D0,0D0)
C this is principal weight (third order no pairs)
      WTMOD     =      WTCRUD*WTSET( 4)
C unexponentiated weights in separate routine
      CALL MODELU(0)
C=============================================================
C for calibration test
      IF(KEYTES.EQ.1)  WTMOD=WTCRUD
C     ********
      ELSE
C     ********
C final printout
      NPAR(10)= NEVGEN
      NPAR(20)= NEVGEN
      CALL VESK2W(1,FUNSKI,AWT,EREL,ZCRUD)
      DWT  = AWT*EREL
      XCRU = ZCRUD *BORNC
C ....... xsections in concecutive orders......
      CALL WMONIT( 1, 1,AWT01,DWT01,DUMM)
      XS01 = XCRU*AWT01
      DS01 = XCRU*AWT01*DWT01
      CALL WMONIT( 1, 2,AWT02,DWT02,DUMM)
      XS02 = XCRU*AWT02
      DS02 = XCRU*AWT02*DWT02
      CALL WMONIT( 1, 3,AWT03,DWT03,DUMM)
      XS03 = XCRU*AWT03
      DS03 = XCRU*AWT03*DWT03
      CALL WMONIT( 1, 4,AWT04,DWT04,DUMM)
      XS04 = XCRU*AWT04
      DS04 = XCRU*AWT04*DWT04
C ....... the differences between orders......
      CALL WMONIT( 1,12,AWT12,DWT12,DUMM)
      RXS12 = XCRU*AWT12        /BORN
      RDS12 = XCRU*AWT12*DWT12  /BORN
      CALL WMONIT( 1,13,AWT13,DWT13,DUMM)
      RXS13 = XCRU*AWT13        /BORN
      RDS13 = XCRU*AWT13*DWT13  /BORN
      CALL WMONIT( 1,14,AWT14,DWT14,DUMM)
      RXS14 = XCRU*AWT14        /BORN
      RDS14 = XCRU*AWT14*DWT14  /BORN
C ... and pairs: third order + pairs, pairs only
      CALL WMONIT( 1, 5,AWT05,DWT05,DUMM)
      XS05 = XCRU*AWT05
      DS05 = XCRU*AWT05*DWT05
      CALL WMONIT( 1,15,AWT15,DWT15,DUMM)
      RXS15 = XCRU*AWT15        /BORN
      RDS15 = XCRU*AWT15*DWT15  /BORN
C .......
      XPAR(10)= XS04
      XPAR(11)= DS04/XS04
C for WEIGHTED events
      XPAR(20)= XCRU
      XPAR(21)= 0D0
      XPAR(22)= BORN
C auxiliary information
      XPAR(25)= DSIG0
      IF(MODE.EQ.1) RETURN
C     ====================
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '        OUTPUT FROM              '
      WRITE(NOUT,BXTXT) '  LUMLOG/BHALOG: WINDOW A        '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) '   X.sect. in [nb] units         '
      WRITE(NOUT,BXTXT) '   for total generated sample    '
      WRITE(NOUT,BXL1I) NEVGEN,     'generated events   ','NEVGEN','A1'
      WRITE(NOUT,BXL2F) AWT ,DWT   ,'vesko2 int. estim. ','AWT   ','A2'
      WRITE(NOUT,BXL1F) XCRU  ,     'crude xsec (vesko2)','XCRU  ','A3'
      WRITE(NOUT,BXL1F) BORN  ,     'Born xsection      ','BORN  ','A4'
      WRITE(NOUT,BXTXT) '        ---  O(alf0)exp ---      '
      WRITE(NOUT,BXL2F) XS01,DS01,  'xsec. total        ','XS01  ','A5'
      WRITE(NOUT,BXL1F) DS01/XS01,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS01/BORN-1,'O(alf0)/Born-1     ','      ','  '
      WRITE(NOUT,BXTXT) '        ---  O(alf1)exp ---      '
      WRITE(NOUT,BXL2F) XS02,DS02,  'xsec. total        ','XS02  ','A6'
      WRITE(NOUT,BXL1F) DS02/XS02,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS02/BORN-1,'O(alf1)/Born-1     ','      ','  '
      WRITE(NOUT,BXL2F) RXS12,RDS12,'O(alf1-alf0)/Born  ','RXS12 ','  '
      WRITE(NOUT,BXTXT) '        ---  O(alf2)exp ---      '
      WRITE(NOUT,BXL2F) XS03,DS03,  'xsec. total        ','XS03  ','A7'
      WRITE(NOUT,BXL1F) DS03/XS03,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS03/BORN-1,'O(alf2)/Born-1     ','      ','  '
      WRITE(NOUT,BXL2F) RXS13,RDS13,'O(alf2-alf1)/Born  ','RXS13 ','  '
      WRITE(NOUT,BXTXT) '        ---  O(alf3)exp ---      '
      WRITE(NOUT,BXL2F) XS04,DS04,  'xsec. total        ','XS04  ','A8'
      WRITE(NOUT,BXL1F) DS04/XS04,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS04/BORN-1,'O(alf3)/Born-1     ','      ','  '
      WRITE(NOUT,BXL2F) RXS14,RDS14,'O(alf3-alf2)/Born  ','RXS14 ','  '
      WRITE(NOUT,BXTXT) '        ---  O(alf3)exp ---      '
      WRITE(NOUT,BXTXT) '        ---  plus pairs ---      '
      WRITE(NOUT,BXL2F) XS05,DS05,  'xsec. total        ','XS05  ','A9'
      WRITE(NOUT,BXL1F) DS05/XS05,  'rel. error         ','      ','  '
      WRITE(NOUT,BXL1F) XS05/BORN-1,'O(alf3+prs)/Born-1 ','      ','  '
      WRITE(NOUT,BXL2F) RXS15,RDS15,'pairs/Born         ','RXS15 ','  '
      WRITE(NOUT,BXCLO)
      ENDIF
C     ********
      END
 
 
      SUBROUTINE MODELU(MODE)
C     ***********************
C  model weight for second order LL unexponentiated
C  note infrared cut k0 is present as usual
C  (stj) 14 febr 91,
C  correction of bug xk0**eta instead of (xk0/xmax)**eta
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(PI     =  3.1415926535897932D0)
      PARAMETER(ALFA   =  1D0/137.03604D0,   ALF1=  ALFA/PI)
      PARAMETER(GNANOB =  389385D0)
      PARAMETER(AMEL   =  0.511D-3)
      PARAMETER(X0     =  1D-5)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,KEYTES
      COMMON / UTIBLO / X1,X2,XI1,XI2,XI
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
 
      IF( MODE.EQ.-1) THEN
C     ********************
      KEYBLO = MOD(KEYRAD,10)
      IGEN = 0
      ELSEIF( MODE.EQ.0) THEN
C     ***********************
      IGEN = IGEN+1
      XMAX=  1-ZMIN
      T0 = (XK0/XMAX)**ETA
      XX1=0D0
      IF(TT1.GT.T0) XX1 = XMAX*DEXP(1/ETA*DLOG(TT1))
      XX2=0D0
      IF(TT2.GT.T0) XX2 = XMAX*DEXP(1/ETA*DLOG(TT2))
      Z1 = 1D0-XX1
      Z2 = 1D0-XX2
C Born Factor
      WTBOR = (1+(1-XI)**2)/2D0
C Two types of the big-log definition
      IF(KEYBLO.EQ.3) THEN
        SPRIM = CMSENE**2*Z1*Z2
        BETA  = ALF1*(DLOG(SPRIM*XI/AMEL**2) -1)
      ELSEIF(KEYBLO.EQ.4) THEN
        BETA  = ALF1* DLOG((CMSENE/AMEL)**2*XIA)
c[[[[[        BETA  = ALF1* DLOG((CMSENE/AMEL)**2*XIB)
      ELSE
        WRITE(6,*) ' WRONG KEYBLO=', KEYBLO
      ENDIF
C   WTSET(11)   Born
C   WTSET(12)   O(alf1)
C
      WTSET(11) =0D0
      WTSET(12) =0D0
      WTSET(13) =0D0
C soft region x1<k0, x2<k0
CCCCCCCCC      IF(TT1.LT.XK0**ETA.AND.TT2.LT.XK0**ETA) THEN
      IF(TT1.LT.T0.AND.TT2.LT.T0) THEN
         WTSET(11) = 1D0/XK0**(2*ETA) * WTBOR
         WTSET(12) = (1D0+ 2D0*BETA*DLOG(XK0)+ 3D0/2D0*BETA)
     $        /XK0**(2*ETA) * WTBOR
         WTSET(13) =
     $  ( 1D0+4D0*BETA/4D0*(3/2D0+2D0*DLOG(XK0))
     $    +    (BETA/2D0)**2 *(3/2D0+2D0*DLOG(XK0))**2
     $    +2D0*(BETA/2D0)**2 *(9/8D0+2D0*(DLOG(XK0))**2
     $           +3D0*DLOG(XK0)    -PI**2/3D0))
     $        /XK0**(2*ETA) * WTBOR
      ENDIF
C single bremss. one soft photon (virt.) one hard
CCCC      IF(TT1.LT.XK0**ETA.AND.TT2.GT.XK0**ETA) THEN
      IF(TT1.LT.T0.AND.TT2.GT.T0) THEN
         WTSET(12) =
     $     BETA/( XK0**ETA *ETA *XX2**ETA)  * WTBOR
         WTSET(13) =
     $      BETA *(1D0+BETA/2D0*( 3/2D0+2D0*DLOG(XK0)
     $                 +2D0*DLOG(XX2)-DLOG(1D0-XX2)+3/2D0)
     $     +BETA/2D0*((2D0-XX2)/2D0*DLOG(1D0-XX2)-XX2)*XX2 )
     $    /( XK0**ETA *ETA *XX2**ETA)  * WTBOR
      ENDIF
CCCC      IF(TT1.GT.XK0**ETA.AND.TT2.LT.XK0**ETA) THEN
      IF(TT1.GT.T0.AND.TT2.LT.T0) THEN
         WTSET(12) =
     $     BETA/( XK0**ETA *ETA *XX1**ETA)  * WTBOR
         WTSET(13) =
     $      BETA *(1D0+BETA/2D0*( 3/2D0+2D0*DLOG(XK0)
     $                 +2D0*DLOG(XX1)-DLOG(1D0-XX1)+3/2D0)
     $     +BETA/2D0*((2D0-XX1)/2D0*DLOG(1D0-XX1)-XX1)*XX1 )
     $    /( XK0**ETA *ETA *XX1**ETA)  * WTBOR
      ENDIF
C two hardreal  photons above k0
CCCC      IF(TT1.GT.XK0**ETA.AND.TT2.GT.XK0**ETA) THEN
      IF(TT1.GT.T0.AND.TT2.GT.T0) THEN
         WTSET(13) =
     $     BETA**2/( ETA**2 *XX1**ETA*XX2**ETA)  * WTBOR
      ENDIF
c[[[      IF(IGEN.LT.50) WRITE(6,*) ' WTSET(...)',(WTSET(I),I=11,13)
C     *****
      ENDIF
C     *****
      END
 
      SUBROUTINE KINOLT
C     *****************
C construction of four-momenta
C They are stored in /MOMSET/ which replaces /UTILUS/
*     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(PI     =  3.1415926535897932D0)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / PARBLO / CMSENE,TMINL,RAXI,XK0,KEYRAD
      COMMON / UTIBLO / X1,X2,XI1,XI2,XI
C obsolete common block of LUMLOG type
CCC      COMMON / UTILUS / P1(4),Q1(4),P2(4),Q2(4)
C final state fermions in BHLUMI output format
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      DOUBLE PRECISION DRVEC(100)
      DIMENSION SUM(4)
      DATA ICONT /0/
      ICONT = ICONT+1
      ENE = CMSENE/2D0
C...beams
      P1(4)=  ENE
      P1(3)=  ENE
      P1(2)=  0D0
      P1(1)=  0D0
      Q1(4)=  ENE
      Q1(3)= -ENE
      Q1(2)=  0D0
      Q1(1)=  0D0
C...explicit photons absent
      NPHOT= 0
C...outgoing dressed (calorimetric) electrons
      Z1   = 1D0-X1
      Z2   = 1D0-X2
      CDOT = 1D0-2D0*XI
      SDOT = DSQRT(DABS(4D0*XI*(1D0-XI)))
      XPT  = 2D0*DSQRT(Z1*Z2)*SDOT
      CALL VARRAN(DRVEC,1)
      PHI  = 2D0*PI*DRVEC(1)
C...first electron
      P2(4) = 0.5D0*ENE *( Z1+Z2   +CDOT*(Z1-Z2))
      P2(3) = 0.5D0*ENE *( CDOT*(Z1+Z2) +Z1-Z2)
      P2(2) = 0.5D0*ENE * XPT*DSIN(PHI)
      P2(1) = 0.5D0*ENE * XPT*DCOS(PHI)
C...second electron
      Q2(4) = 0.5D0*ENE *( Z1+Z2   -CDOT*(Z1-Z2))
      Q2(3) = 0.5D0*ENE *( -CDOT*(Z1+Z2) +Z1-Z2)
      Q2(2) =-0.5D0*ENE * XPT*DSIN(PHI)
      Q2(1) =-0.5D0*ENE * XPT*DCOS(PHI)
      END
 
      FUNCTION FUNSKI(T1,T2)
C     ************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C FLEPS is the smallest floating on a given instalation
      PARAMETER(X0=1D-5)
cc===      PARAMETER(FLEPS= 1D-300)
      COMMON / PARFUN / ETA,ZMIN,XIA,XIB,TT1,TT2,KEYTES
 
      TT1=T1
      TT2=T2
      XMAX = 1D0-ZMIN
cc===      SMALL = FLEPS**ETA
cc===      IF(SMALL.GT.1D-4)
cc===     $   WRITE(6,*) ' ++++ FUNSKO: warning FLEPS**ETA=',SMALL
cc===      IF(T1.LT.SMALL.OR.T2.LT.SMALL) GOTO 900
      XX1=0D0
      XX2=0D0
      IF(T1.GT.X0**ETA) XX1 = XMAX*DEXP(1/ETA*DLOG(T1))
      IF(T2.GT.X0**ETA) XX2 = XMAX*DEXP(1/ETA*DLOG(T2))
C Jacobian factor due to change of variables t=>x
C eta*x**(eta-1) multiplied  (numerical stability)  see DD1*DD2
      RJAC = XMAX**(2D0*ETA)
C=== $     /(ETA*XX1**(ETA-1D0))/(ETA*XX2**(ETA-1D0))
C anticipated angular trigger
      Z1 = 1-XX1
      Z2 = 1-XX2
      SLOPE = Z1/Z2
      SRAXI = SQRT(XIB/XIA)
      IF(SLOPE.GT.  SRAXI) GOTO 900
      IF(SLOPE.LT.1/SRAXI) GOTO 900
C Ordinary flux-factor
      FLUX = 1D0/(Z1*Z2)
C Jacobian due to xi=>xi1 or xi=>xi2 change
      IF( Z1.LT.Z2) THEN
        DJAC= Z1/Z2
      ELSE
        DJAC= Z2/Z1
      ENDIF
C Crude structure functions
C eta*x**(eta-1) divided out (numerical stability)  see RJAC
C===  DD1 = ETA*XX1**(ETA-1D0)*(1D0+(1D0-XX1)**2)/2D0
C===  DD2 = ETA*XX2**(ETA-1D0)*(1D0+(1D0-XX2)**2)/2D0
      DD1 =                    (1D0+(1D0-XX1)**2)/2D0
      DD2 =                    (1D0+(1D0-XX2)**2)/2D0
C Test distributions
      IF(KEYTES.EQ.1) THEN
C===    DD1 = ETA*XX1**(ETA-1D0)
C===    DD2 = ETA*XX2**(ETA-1D0)
        DD1 = 1D0
        DD2 = 1D0
      ENDIF
C below no trace of eta*x**(eta-1) any more!
      FUNSKI = FLUX*RJAC*DJAC*DD1*DD2
      RETURN
  900 FUNSKI = 0D0
      END
 
      FUNCTION STRUFU(KEYD,BETA,VV)
C-------------------------------------------------------------------C
C This originates from XSTFIG
C non-singlet structure functions, factor x**(beta-1) removed!!!
C-------------------------------------------------------------------C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (DZ2=1.6449340668482264D0,DZ3=1.2020569031595943D0)
      PARAMETER(PI= 3.1415926535897932D0, ALFINV=137.03604D0)
      PARAMETER(CEULER =0.57721566D0)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      BETI=BETA
      X=VV
      Z=1D0-VV
C ....Zero Order without subleading terms
      IF(KEYD  .EQ.310)  THEN
C===     DISTR=BETI*X**(BETI-1D0)
         DISTR=BETI
     @      *EXP(BETI*(.75D0-CEULER))/DPGAMM(1D0+BETI)
C ....First Order without subleading terms
      ELSEIF(KEYD  .EQ.311)  THEN
C===     DISTR=BETI*X**(BETI-1D0)
         DISTR=BETI
     @      *EXP(BETI*(.75D0-CEULER))/DPGAMM(1D0+BETI)
     @      *(1D0 -.5D0*(1D0-(1D0-X)**2) )
C ....Second Order without subleading terms
      ELSEIF(KEYD  .EQ.312)  THEN
C===     DISTR=BETI*X**(BETI-1D0)
         DISTR=BETI
     @      *EXP(BETI*(.75D0-CEULER))/DPGAMM(1D0+BETI)
     @      *(1D0 -.5D0*(1D0-(1D0-X)**2)
     @       +BETI*( -(1D0+3D0*(1D0-X)**2)/8D0*LOG(1D0-X) -.25D0*X**2)
     @       )
C ....Third Order without subleading terms
      ELSEIF(KEYD  .EQ.313)  THEN
C===     DISTR=BETI*X**(BETI-1D0)
C redefine beta in the case of fermion pairs,
C (this is: LL nonsinglet, electron pair only)
         DISTR=BETI
     @      *EXP(BETI*(.75D0-CEULER))/DPGAMM(1D0+BETI)
     @      *(1D0 -.5D0*(1D0-(1D0-X)**2)
     @       +BETI*( -(1D0+3D0*(1D0-X)**2)/8D0*LOG(1D0-X) -.25D0*X**2)
     @          +BETI**2*(
     @                +(3D0*X-2D0)*X/16*LOG(1D0-X)
     @                 +(8D0-14D0*X+7D0*X**2)/96*LOG(1D0-X)**2
     @                 +X**2/8D0
     @                  +(2D0-X)*X/8*DILOG(X)
     @                    )  )
      ELSE
            GOTO 900
      ENDIF
      STRUFU=DISTR
      RETURN
 900  WRITE(NOUT,*) ' ===--->  WRONG KEYDIS IN STRUFU'
      STOP
      END
 
      FUNCTION BORNB(CMSENE,THA,THB)
C     *******************************
C BORN XSECTION pure t-channel
C THA,THB are in radians, CMSENE in GEV units
C result in nanobarns
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0, ALFINV = 137.03604D0)
      PARAMETER( ALFPI=  1D0/PI/ALFINV ,ALFA=1D0/ALFINV)
      PARAMETER( GNANOB=389.385D-30*1.D33 )
      EXTERNAL BORXI
      XIA= (1D0-DCOS(THA))/2D0
      XIB= (1D0-DCOS(THB))/2D0
      DSIG0 = 4D0*ALFA**2*PI/CMSENE**2*GNANOB
      CALL GAUSJD(BORXI,XIA,XIB,-1D-6,RESULT)
      BORNB= RESULT *DSIG0
      END
      FUNCTION BORXI(XI)
C     ******************
C Integrand of BORNB
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      BORXI=1D0/XI**2*(1D0+(1D0-XI)**2)/2D0
      END
 
      FUNCTION FOBOR(XIA,XIB)
C     ***********************
C BORN XSECTION for pure t-channel
C XI=(1-cos(theta))/2
C result in DSIG0 units where
C DSIG0 = 4D0*ALFA**2*PI/CMSENE**2
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL BORNXF
      CALL GAUSJD(BORNXF,XIA,XIB,-1D-6,RESULT)
      FOBOR= RESULT
      END
      FUNCTION BORNXF(XI)
C     ******************
C Integrand for FOBOR
C     ***********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      BORNXF=1D0/XI**2*(1D0+(1D0-XI)**2)/2D0
C----------------------------------------------------------------C
C                  The end of LUMLOG                             C
C----------------------------------------------------------------C
      END
 
 
 
C           **************************************************
C           *       **********************************       *
C           *       *      *******************       *       *
C           *       *      *                 *       *       *
C           *       *      *   B H L L I B   *       *       *
C           *       *      *                 *       *       *
C           *       *      *******************       *       *
C           *       **********************************       *
C           **************************************************
C
CBB   SUBROUTINE MARRAN(RVEC,LENV)
C =======================S. JADACH===================================
C == This commes from F. James, The name of RANMAR is changed to   ==
C == MARRAN in order to avoid interference with the version        ==
C == already in use and the public library version (if present).   ==
C ==      THIS IS THE ONLY MODIFICATION !!!!                       ==
C ========================S. JADACH==================================
C Universal random number generator proposed by Marsaglia and Zaman
C in report FSU-SCRI-87-50
C        modified by F. James, 1988 and 1989, to generate a vector
C        of pseudorandom numbers RVEC of length LENV, and to put in
C        the COMMON block everything needed to specify currrent state,
C        and to add input and output entry points RMARIN, RMARUT.
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for RANMAR:                                  ++
C!!!      CALL RANMAR (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL RMARIN(I1,N1,N2)   initializes the generator from one ++
C!!!                   32-bit integer I1, and number counts N1,N2    ++
C!!!                  (for initializing, set N1=N2=0, but to restart ++
C!!!                    a previously generated sequence, use values  ++
C!!!                    output by RMARUT)                            ++
C!!!      CALL RMARUT(I1,N1,N2)   outputs the value of the original  ++
C!!!                  seed and the two number counts, to be used     ++
C!!!                  for restarting by initializing to I1 and       ++
C!!!                  skipping N2*100000000+N1 numbers.              ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CBB   DIMENSION RVEC(*)
CBB   COMMON/RASET1/U(97),C,I97,J97
CBB   PARAMETER (MODCNS=1000000000)
CBB   SAVE CD, CM, TWOM24, NTOT, NTOT2, IJKL
CBB   DATA NTOT,NTOT2,IJKL/-1,0,0/
C
CBB   IF (NTOT .GE. 0)  GO TO 50
C
C        Default initialization. User has called RANMAR without RMARIN.
CBB   IJKL = 54217137
CBB   NTOT = 0
CBB   NTOT2 = 0
CBB   KALLED = 0
CBB   GO TO 1
C
CBB   ENTRY      RMARIN(IJKLIN, NTOTIN,NTOT2N)
C         Initializing routine for RANMAR, may be called before
C         generating pseudorandom numbers with RANMAR. The input
C         values should be in the ranges:  0<=IJKLIN<=900 OOO OOO
C                                          0<=NTOTIN<=999 999 999
C                                          0<=NTOT2N<<999 999 999!
C To get the standard values in Marsaglia's paper, IJKLIN=54217137
C                                            NTOTIN,NTOT2N=0
CBB   IJKL = IJKLIN
CBB   NTOT = MAX(NTOTIN,0)
CBB   NTOT2= MAX(NTOT2N,0)
CBB   KALLED = 1
C          always come here to initialize
CBB 1 CONTINUE
CBB   IJ = IJKL/30082
CBB   KL = IJKL - 30082*IJ
CBB   I = MOD(IJ/177, 177) + 2
CBB   J = MOD(IJ, 177)     + 2
CBB   K = MOD(KL/169, 178) + 1
CBB   L = MOD(KL, 169)
CBB   WRITE(6,'(A,5I10)')
CBB  $' MARran INITIALIZED:IJ,KL,IJKL,NTOT,NTOT2=',IJ,KL,IJKL,NTOT,NTOT2
CCCCBB   PRINT '(A,4I10)', '   I,J,K,L= ',I,J,K,L
CBB   DO 2 II= 1, 97
CBB   S = 0.
CBB   T = .5
CBB   DO 3 JJ= 1, 24
CBB      M = MOD(MOD(I*J,179)*K, 179)
CBB      I = J
CBB      J = K
CBB      K = M
CBB      L = MOD(53*L+1, 169)
CBB      IF (MOD(L*M,64) .GE. 32)  S = S+T
CBB 3    T = 0.5*T
CBB 2 U(II) = S
CBB   TWOM24 = 1.0
CBB   DO 4 I24= 1, 24
CBB 4 TWOM24 = 0.5*TWOM24
CBB   C  =   362436.*TWOM24
CBB   CD =  7654321.*TWOM24
CBB   CM = 16777213.*TWOM24
CBB   I97 = 97
CBB   J97 = 33
CCBB    Complete initialization by skipping
CCBB         (NTOT2*MODCNS + NTOT) random numbers
CBB   DO 45 LOOP2= 1, NTOT2+1
CBB   NOW = MODCNS
CBB   IF (LOOP2 .EQ. NTOT2+1)  NOW=NTOT
CBB   IF (NOW .GT. 0)  THEN
CBB     WRITE(6,'(A,I15)') ' RMARIN SKIPPING OVER ',NOW
CBB    DO 40 IDUM = 1, NTOT
CBB    UNI = U(I97)-U(J97)
CBB    IF (UNI .LT. 0.)  UNI=UNI+1.
CBB    U(I97) = UNI
CBB    I97 = I97-1
CBB    IF (I97 .EQ. 0)  I97=97
CBB    J97 = J97-1
CBB    IF (J97 .EQ. 0)  J97=97
CBB    C = C - CD
CBB    IF (C .LT. 0.)  C=C+CM
CBB40  CONTINUECBBCBB
CBB   ENDIF
CBB45 CONTINUECBB
CBB   IF (KALLED .EQ. 1)  RETURN
CCBB
CCBB       Normal entry to generate LENV random numbers
CBB50 CONTINUECBB
CBB   DO 100 IVEC= 1, LENV
CBB   UNI = U(I97)-U(J97)
CBB   IF (UNI .LT. 0.)  UNI=UNI+1.
CBB   U(I97) = UNI
CBB   I97 = I97-1
CBB   IF (I97 .EQ. 0)  I97=97
CBB   J97 = J97-1
CBB   IF (J97 .EQ. 0)  J97=97
CBB   C = C - CD
CBB   IF (C .LT. 0.)  C=C+CM
CBB   UNI = UNI-C
CBB   IF (UNI .LT. 0.) UNI=UNI+1.
CBB   RVEC(IVEC) = UNI
CCBB          Replace exact zeros by uniform distr. *2**-24
CBB      IF (UNI .EQ. 0.)  THEN
CBB      ZUNI = TWOM24*U(2)
CCBB          An exact zero here is very unlikely, but let's be safe.
CBB      IF (ZUNI .EQ. 0.) ZUNI= TWOM24*TWOM24
CBB      RVEC(IVEC) = ZUNI
CBB      ENDIF
CBB100 CONTINUE
CBB   NTOT = NTOT + LENV
CBB      IF (NTOT .GE. MODCNS)  THEN
CBB      NTOT2 = NTOT2 + 1
CBB      NTOT = NTOT - MODCNS
CBB      ENDIF
CBB   RETURN
CCBB        Entry to output current status
CBB   ENTRY RMARUT(IJKLUT,NTOTUT,NTOT2T)
CBB   IJKLUT = IJKL
CBB   NTOTUT = NTOT
CBB   NTOT2T = NTOT2
CBB   RETURN
CBB   END
 
      SUBROUTINE RCARRY(RVEC,LENV)
C         Add-and-carry random number generator proposed by
C         Marsaglia and Zaman in SIAM J. Scientific and Statistical
C             Computing, to appear probably 1990.
C         modified with enhanced initialization by F. James, 1990
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for RCARRY:                                  ++
C!!!      CALL RCARRY (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL RCARGO(INT)     initializes the generator from one    ++
C!!!                   32-bit integer INT                            ++
C!!!      CALL RCARIN(IVEC)    restarts the generator from vector    ++
C!!!                   IVEC of 25 32-bit integers (see RCARUT)       ++
C!!!      CALL RCARUT(IVEC)    outputs the current values of the 25  ++
C!!!                 32-bit integer seeds, to be used for restarting ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(LENV)
      DIMENSION SEEDS(24), ISEEDS(24), ISDEXT(25)
      PARAMETER (TWOP12=4096.)
      PARAMETER (ITWO24=2**24, ICONS=2147483563)
      SAVE NOTYET, I24, J24, CARRY, SEEDS, TWOM24
      LOGICAL NOTYET
      DATA NOTYET/.TRUE./
      DATA I24,J24,CARRY/24,10,0./
C
C              Default Initialization by Multiplicative Congruential
      IF (NOTYET) THEN
         NOTYET = .FALSE.
         JSEED = 314159265
         WRITE(6,'(A,I12)') ' RCARRY DEFAULT INITIALIZATION: ',JSEED
            TWOM24 = 1.
         DO 25 I= 1, 24
            TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
   25    CONTINUE
         DO 50 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
   50    CONTINUE
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .LT. SEEDS(14)) CARRY = TWOM24
      ENDIF
C
C          The Generator proper: "Subtract-with-borrow",
C          as proposed by Marsaglia and Zaman,
C          Florida State University, March, 1989
C
      DO 100 IVEC= 1, LENV
      UNI = SEEDS(I24) - SEEDS(J24) - CARRY
      IF (UNI .LT. 0.)  THEN
         UNI = UNI + 1.0
         CARRY = TWOM24
      ELSE
         CARRY = 0.
      ENDIF
      SEEDS(I24) = UNI
      I24 = I24 - 1
      IF (I24 .EQ. 0)  I24 = 24
      J24 = J24 - 1
      IF (J24 .EQ. 0)  J24 = 24
      RVEC(IVEC) = UNI
  100 CONTINUE
      RETURN
C           Entry to input and float integer seeds from previous run
      ENTRY RCARIN(ISDEXT)
         TWOM24 = 1.
         DO 195 I= 1, 24
  195    TWOM24 = TWOM24 * 0.5
      WRITE(6,'(A)') ' FULL INITIALIZATION OF RCARRY WITH 25 INTEGERS:'
      WRITE(6,'(5X,5I12)') ISDEXT
      DO 200 I= 1, 24
      SEEDS(I) = REAL(ISDEXT(I))*TWOM24
  200 CONTINUE
      CARRY = REAL(MOD(ISDEXT(25),10))*TWOM24
      ISD = ISDEXT(25)/10
      I24 = MOD(ISD,100)
      ISD = ISD/100
      J24 = ISD
      RETURN
C                    Entry to ouput seeds as integers
      ENTRY RCARUT(ISDEXT)
      DO 300 I= 1, 24
         ISDEXT(I) = INT(SEEDS(I)*TWOP12*TWOP12)
  300 CONTINUE
      ICARRY = 0
      IF (CARRY .GT. 0.)  ICARRY = 1
      ISDEXT(25) = 1000*J24 + 10*I24 + ICARRY
      RETURN
C                    Entry to initialize from one integer
      ENTRY RCARGO(INSEED)
      JSEED = INSEED
      WRITE(6,'(A,I12)') ' RCARRY INITIALIZED FROM SEED ',INSEED
C      TWOM24 = 1.
         DO 325 I= 1, 24
           TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
  325    CONTINUE
         DO 350 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
  350    CONTINUE
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .LT. SEEDS(14)) CARRY = TWOM24
      RETURN
      END
 
      SUBROUTINE RANECU(RVEC,LEN)
C         Random number generator given by L'Ecuyer in
C            Comm. ACM Vol 31, p.742, 1988
C            modified by F. James to return a vector of numbers
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for RANECU:                                  ++
C!!!      CALL RANECU (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL RECUIN(I1,I2)    initializes the generator from two   ++
C!!!                   32-bit integers I1 and I2                     ++
C!!!      CALL RECUUT(I1,I2)    outputs the current values of the    ++
C!!!                   two integer seeds, to be used for restarting  ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(*)
      SAVE ISEED1,ISEED2
      DATA ISEED1,ISEED2 /12345,67890/
C
      DO 100 I= 1, LEN
      K = ISEED1/53668
      ISEED1 = 40014*(ISEED1 - K*53668) - K*12211
      IF (ISEED1 .LT. 0) ISEED1=ISEED1+2147483563
C
      K = ISEED2/52774
      ISEED2 = 40692*(ISEED2 - K*52774) - K* 3791
      IF (ISEED2 .LT. 0) ISEED2=ISEED2+2147483399
C
      IZ = ISEED1 - ISEED2
      IF (IZ .LT. 1)  IZ = IZ + 2147483562
C
      RVEC(I) = REAL(IZ) * 4.656613E-10
  100 CONTINUE
      RETURN
C
      ENTRY RECUIN(IS1,IS2)
      ISEED1 = IS1
      ISEED2 = IS2
      RETURN
C
      ENTRY RECUUT(IS1,IS2)
      IS1 = ISEED1
      IS2 = ISEED2
      RETURN
      END
 
      SUBROUTINE VARRAN(DRVEC,LEN)
C     ***************************
C Switchable random number generator
C Translation to double precision
C     ***************************
      COMMON / RANPAR / KEYRND
      DOUBLE PRECISION DRVEC(*)
      DIMENSION RVEC(1000)
      IF(LEN.LT.1.OR.LEN.GT.1000) GOTO 901
   10 CONTINUE
      IF(KEYRND.EQ.1) THEN
CBB      CALL MARRAN(RVEC,LEN)
CBB      GO TO INTERFACE TO RANMAR THROUGH ALEPH RNDM
         DO 20 I = 1,LEN
 20         RVEC(I) = RNDM(DUM)
CBB  END OF MOD
      ELSEIF(KEYRND.EQ.2) THEN
         CALL RANECU(RVEC,LEN)
      ELSE
         GOTO 902
      ENDIF
C random numbers 0 and 1 not accepted
      DO 30 I=1,LEN
      IF(RVEC(I).LE.0E0.OR.RVEC(I).GE.1E0) THEN
        WRITE(6,*) ' +++++ VARRAN: RVEC=',RVEC(I)
        GOTO 10
      ENDIF
      DRVEC(I)=RVEC(I)
   30 CONTINUE
      RETURN
  901 WRITE(6,*) ' +++++ STOP IN VARRAN: LEN=',LEN
      STOP
  902 WRITE(6,*) ' +++++ STOP IN VARRAN: WRONG KEYRND',KEYRND
      STOP
      END
 
C BOOST ALONG X AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
      SUBROUTINE BOSTD1(EXE,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(1)
      RMI=RVEC(4)-RVEC(1)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(2)=RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(1)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      END
 
C BOOST ALONG Z AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
      SUBROUTINE BOSTD3(EXE,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(3)
      RMI=RVEC(4)-RVEC(3)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(1)=RVEC(1)
      QVEC(2)=RVEC(2)
      QVEC(3)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      END
 
      SUBROUTINE ROTOD1(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)=RVEC(1)
      QVEC(2)= CS*RVEC(2)-SN*RVEC(3)
      QVEC(3)= SN*RVEC(2)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END
 
      SUBROUTINE ROTOD2(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)+SN*RVEC(3)
      QVEC(2)=RVEC(2)
      QVEC(3)=-SN*RVEC(1)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END
 
      SUBROUTINE ROTOD3(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)-SN*RVEC(2)
      QVEC(2)= SN*RVEC(1)+CS*RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(4)=RVEC(4)
      END
 
      FUNCTION ANGFI(X,Y)
C     *******************
* CALCULATES ANGLE IN (0,2*PI) RANGE OUT OF X-Y
*     ***********************
      IMPLICIT REAL*8(A-H,O-Z)
      DATA PI /3.1415926535897932D0/
 
      IF(ABS(Y).LT.ABS(X)) THEN
        THE=ATAN(ABS(Y/X))
        IF(X.LE.0D0) THE=PI-THE
      ELSE
        THE=ACOS(X/SQRT(X**2+Y**2))
      ENDIF
      IF(Y.LT.0D0) THE=2D0*PI-THE
      ANGFI=THE
      END
 
 
      DOUBLE PRECISION FUNCTION DILOG(X)
C-------------------------------------------- REMARKS ---------------
C DILOGARITHM FUNCTION: DILOG(X)=INT( -LN(1-Z)/Z ) , 0 < Z < X .
C THIS IS THE CERNLIB VERSION.
C--------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Z=-1.644934066848226D0
      IF(X .LT.-1.D0) GO TO 1
      IF(X .LE. 0.5D0) GO TO 2
      IF(X .EQ. 1.D0) GO TO 3
      IF(X .LE. 2.D0) GO TO 4
      Z=3.289868133696453D0
    1 T=1.D0/X
      S=-0.5D0
      Z=Z-0.5D0*DLOG(DABS(X))**2
      GO TO 5
    2 T=X
      S=0.5D0
      Z=0.D0
      GO TO 5
    3 DILOG=1.644934066848226D0
      RETURN
    4 T=1.D0-X
      S=-0.5D0
      Z=1.644934066848226D0-DLOG(X)*DLOG(DABS(T))
    5 Y=2.666666666666667D0*T+0.666666666666667D0
      B=      0.000000000000001D0
      A=Y*B  +0.000000000000004D0
      B=Y*A-B+0.000000000000011D0
      A=Y*B-A+0.000000000000037D0
      B=Y*A-B+0.000000000000121D0
      A=Y*B-A+0.000000000000398D0
      B=Y*A-B+0.000000000001312D0
      A=Y*B-A+0.000000000004342D0
      B=Y*A-B+0.000000000014437D0
      A=Y*B-A+0.000000000048274D0
      B=Y*A-B+0.000000000162421D0
      A=Y*B-A+0.000000000550291D0
      B=Y*A-B+0.000000001879117D0
      A=Y*B-A+0.000000006474338D0
      B=Y*A-B+0.000000022536705D0
      A=Y*B-A+0.000000079387055D0
      B=Y*A-B+0.000000283575385D0
      A=Y*B-A+0.000001029904264D0
      B=Y*A-B+0.000003816329463D0
      A=Y*B-A+0.000014496300557D0
      B=Y*A-B+0.000056817822718D0
      A=Y*B-A+0.000232002196094D0
      B=Y*A-B+0.001001627496164D0
      A=Y*B-A+0.004686361959447D0
      B=Y*A-B+0.024879322924228D0
      A=Y*B-A+0.166073032927855D0
      A=Y*A-B+1.935064300869969D0
      DILOG=S*T*(A-B)+Z
      END
 
 
      SUBROUTINE GAUSJD(FUN,AA,BB,EEPS,RESULT)
C     ****************************************
C Gauss integration by S. Jadach, Oct. 90.
C This is NON-ADAPTIVE (!!!!) UNOPTIMIZED (!!!) integration subprogram.
C     *************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION WG(12),XX(12)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      EXTERNAL FUN
      DATA WG
     $/0.101228536290376D0, 0.222381034453374D0, 0.313706645877887D0,
     $ 0.362683783378362D0, 0.027152459411754D0, 0.062253523938648D0,
     $ 0.095158511682493D0, 0.124628971255534D0, 0.149595988816577D0,
     $ 0.169156519395003D0, 0.182603415044924D0, 0.189450610455069D0/
      DATA XX
     $/0.960289856497536D0, 0.796666477413627D0, 0.525532409916329D0,
     $ 0.183434642495650D0, 0.989400934991650D0, 0.944575023073233D0,
     $ 0.865631202387832D0, 0.755404408355003D0, 0.617876244402644D0,
     $ 0.458016777657227D0, 0.281603550779259D0, 0.095012509837637D0/
      DATA ITERMX / 15/
      EPS=ABS(EEPS)
      A=AA
      B=BB
      NDIVI=1
C iteration over subdivisions terminated by precision requirement
      DO 400 ITER=1,ITERMX
      CALK8  =0D0
      CALK16 =0D0
C sum over DELTA subintegrals
      DO 200 K = 1,NDIVI
      DELTA = (B-A)/NDIVI
      X1    =  A + (K-1)*DELTA
      X2    =  X1+ DELTA
      XMIDLE= 0.5D0*(X2+X1)
      RANGE = 0.5D0*(X2-X1)
      SUM8 =0D0
      SUM16=0D0
C 8- and 12-point   Gauss integration over single DELTA subinterval
      DO 100 I=1,12
      XPLUS= XMIDLE+RANGE*XX(I)
      XMINU= XMIDLE-RANGE*XX(I)
      FPLUS=FUN(XPLUS)
      FMINU=FUN(XMINU)
      IF(I.LE.4) THEN
          SUM8 =SUM8  +(FPLUS+FMINU)*WG(I)/2D0
      ELSE
          SUM16=SUM16 +(FPLUS+FMINU)*WG(I)/2D0
      ENDIF
  100 CONTINUE
      CALK8 = CALK8 + SUM8 *(X2-X1)
      CALK16= CALK16+ SUM16*(X2-X1)
  200 CONTINUE
      ERABS = ABS(CALK16-CALK8)
      ERELA = 0D0
      IF(CALK16.NE.0D0) ERELA= ERABS/ABS(CALK16)
c     write(6,*) 'gausjd: CALK8,CALK16=',ITER,CALK8,CALK16,ERELA
C precision check to terminate integration
      IF(EEPS.GT.0D0) THEN
        IF(ERABS.LT. EPS) GOTO 800
      ELSE
        IF(ERELA.LT. EPS) GOTO 800
      ENDIF
  400 NDIVI=NDIVI*2
      WRITE(NOUT,*) ' +++++ GAUSJD:  REQUIRED PRECISION TO HIGH!'
      WRITE(NOUT,*) ' +++++ GAUSJD:  ITER,ERELA=',ITER,ERELA
  800 RESULT= CALK16
      END
 
 
      DOUBLE PRECISION FUNCTION DPGAMM(Z)
C     **********************************
C Double precision Gamma function
      DOUBLE PRECISION Z,Z1,X,X1,X2,X3,D1,D2,D3,S1,S2,S3,PI,C(20),CONST
      DATA C( 1) / 8.3333333333333333333333333332D-02/
      DATA C( 2) /-2.7777777777777777777777777777D-03/
      DATA C( 3) / 7.9365079365079365079365079364D-04/
      DATA C( 4) /-5.9523809523809523809523809523D-04/
      DATA C( 5) / 8.4175084175084175084175084175D-04/
      DATA C( 6) /-1.9175269175269175269175269175D-03/
      DATA C( 7) / 6.4102564102564102564102564102D-03/
      DATA C( 8) /-2.9550653594771241830065359477D-02/
      DATA C( 9) / 1.7964437236883057316493849001D-01/
      DATA C(10) /-1.3924322169059011164274322169D+00/
      DATA C(11) / 1.3402864044168391994478951001D+01/
      DATA C(12) /-1.5684828462600201730636513245D+02/
      DATA C(13) / 2.1931033333333333333333333333D+03/
      DATA C(14) /-3.6108771253724989357173265219D+04/
      DATA C(15) / 6.9147226885131306710839525077D+05/
      DATA C(16) /-1.5238221539407416192283364959D+07/
      DATA C(17) / 3.8290075139141414141414141414D+08/
      DATA C(18) /-1.0882266035784391089015149165D+10/
      DATA C(19) / 3.4732028376500225225225225224D+11/
      DATA C(20) /-1.2369602142269274454251710349D+13/
      DATA PI    / 3.1415926535897932384626433832D+00/
      DATA CONST / 9.1893853320467274178032973641D-01/
      IF(Z.GT.5.75D 1)                                     GOTO  6666
      NN = Z
      IF (Z  -  DBLE(FLOAT(NN)))                 3,1,3
    1 IF (Z    .LE.    0.D 0)                    GOTO 6667
      DPGAMM = 1.D 0
      IF (Z    .LE.    2.D 0)                    RETURN
      Z1 = Z
    2 Z1 = Z1  -  1.D 0
      DPGAMM = DPGAMM * Z1
      IF (Z1  -  2.D 0)                          61,61,2
    3 IF (DABS(Z)    .LT.    1.D-29)             GOTO 60
      IF (Z    .LT.    0.D 0)                    GOTO 4
      X  = Z
      KK = 1
      GOTO 10
    4 X  = 1.D 0  -  Z
      KK = 2
   10 X1 = X
      IF (X    .GT.    19.D 0)                   GOTO 13
      D1 = X
   11 X1 = X1  +  1.D 0
      IF (X1    .GE.    19.D 0)                  GOTO 12
      D1 = D1 * X1
      GOTO 11
   12 S3 = -DLOG(D1)
      GOTO 14
   13 S3 = 0.D 0
   14 D1 = X1 * X1
      S1 = (X1  -  5.D-1) * DLOG(X1)  -  X1  +  CONST
      DO 20                  K=1,20
      S2 = S1  +  C(K)/X1
      IF (DABS(S2  -  S1)    .LT.    1.D-28)     GOTO 21
      X1 = X1 * D1
   20 S1 = S2
   21 S3 = S3  +  S2
      GOTO (50,22),    KK
   22 D2 = DABS(Z  -  NN)
      D1 = D2 * PI
      IF (D1    .LT.    1.D-15)                  GOTO 31
   30 X2 =  DLOG(PI/DSIN(D1))  -  S3
      GOTO 40
   31 X2 = -DLOG(D2)
   40 MM = DABS(Z)
      IF(X2      .GT.      1.74D2)                         GO TO 6666
      DPGAMM = DEXP(X2)
      IF (MM    .NE.    (MM/2) * 2)              RETURN
      DPGAMM = -DPGAMM
      RETURN
   50 IF(S3      .GT.      1.74D2)                         GO TO 6666
      DPGAMM = DEXP(S3)
      RETURN
 6666 PRINT *, 2000
      RETURN
 6667 PRINT *, 2001
      RETURN
   60 DPGAMM = 0.D 0
      IF(DABS(Z)   .LT.   1.D-77)   RETURN
      DPGAMM = 1.D 0/Z
   61 RETURN
 2000 FORMAT (/////, 2X, 32HDPGAMM ..... ARGUMENT TOO LARGE., /////)
 2001 FORMAT (/////, 2X, 32HDPGAMM ..... ARGUMENT IS A POLE., /////)
      END
 
      SUBROUTINE VESK2W(MODE,FUNSKO,X,Y,WT)
C     *************************************
C======================================================================
C======================================================================
C======================================================================
C==============TWO DIMENSIONAL SAMPLER VESK2W==========================
C======================================================================
C======================================================================
C======================================================================
C                         VESK2W                                      C
C  GENERAL PURPOSE ROUTINE TO GENERATE AN ARBITRARY TWO DIMENSIONAL   C
C  DISTRIBUTION SUPPLIED BY USER IN A FORM OF FUNCTION FUNSKO(X,Y)    C
C                 WRITTEN NOVEMBER 1985                               C
C                    BY S. JADACH                                     C
C                 LAST UPDATE:  07.NOV.1990                           C
C                 version with weighted event....                     C
C=====================================================================C
C VESKO2 GENERATES TWO DIMENSIONAL DISTRIBUTION DEFINED BY ARBITRARY
C FUNCTION FUNSKO(X,Y) WHERE X,Y BELONG  TO (0,1) RANGE.
C THE METHOD CONSISTS IN DIVIDING UNIT PLAQUET INTO CELLS USING
C SORT OF 'LIFE-GAME' METHOD IN WHICH THE DIVISION OF A CELLS IS MADE
C (DURING INITIALISATION) ALWAYS FOR THIS CELL WHICH CONTAINS
C A MAXIMUM VALUE OF THE INTEGRAL OVER FUNSKO IN THE CELL.
C RESULTING CELLS CONTAIN (USUALLY UP TO FACTOR TWO) EQUAL INTERGRAL
C VALUE. THE GENERATION CONSISTS IN CHOOSING RANDOMLY  A CELL
C ACCORDING TO ITS CONTENT AND THEN IN GENERATING X,Y WITHIN THE CELL.
C REJECTION METHOD IS APPLIED AT THE END OF THE PROCEDURE IN ORDER TO
C ASSURE THAT X,Y ARE DISTRIBUTED PRECISELY ACCORDING TO FUNSKO(X,Y)
C                    PARAMETERS
C -/ MODE = -1 INITIALISATION, NO (X,Y) GENERATED,
C              CALL VESKO2(-1,D1,D2)
C    HAS TO BE MADE PRIOR  TO GENERATING FIRST (X,Y) PAIR
C -/ MODE =  0 GENERATION OF (X,Y) PAIR BY CALL VESKO2(0,X,Y)
C -/ MODE =  1 CALL VESKO2(1,VALINT,ERRINT) MAY BE DONE AFTER LAST
C    (X,Y) WAS GENERATED IN ORDER TO OBTAIN THE VALUE OF THE INTEGRAL
C    VALINT AND ITS ERROR ERRINT, INTEGRAL IS CALCULATED USING AVERAGE
C    WEIGHTS ENCOUTERED DURING GENERATION PHASE
C -/ X,Y  IF MODE=-1 THE THEY ARE DUMMY
C         IF MODE= 0 THE RESULT OF RANDOM GENERATION ACCORDING TO
C                    FUNCTION FUNSKO, X AND Y BELONG TO (0,1)
C         IF MODE= 1 X= VALUE OF INTEGRAL AND Y=ERROR (RELATIVE)
C                    WT = crude x-section
C ------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      DOUBLE PRECISION DRVEC(100)
      EXTERNAL FUNSKO
      DATA IWARM/77/
 
      IF(MODE) 100,200,300
C...  INITIALISATION PART, SEE VINSKO FOR MORE COMMENTS
  100 CALL VINSKW(FUNSKO)
      IWARM=0
      WT=0D0
      WTMAX = 1D0
      WTMXX = WTMAX
      NEVOV=0
      SWT=0D0
      SSWT=0D0
      NEVS=0
C(((((((((((((
C     CALL HBOOK1(1, 16H WT-VESKO2     $,75,0.0D0,1.5D0)
C     CALL HMINIM(1,0)
C     CALL HBOOK2(2,16H X-Y VESKO2    $, 64,0,1, 32,0,1,0)
C     CALL HSCALE(2)
C))))))))))))
      RETURN
C...
  200 CONTINUE
C...  GENERATION PART
      IF(IWARM.EQ.77) GO TO 980
cc    IF(WT.GT.WTMAX) THEN
cc      write(6,*) ' vesko2: ev. overweighted, dont worry, wt=',wt
cc      WT=WT-WTMAX
cc      NEVOV=NEVOV+1
cc    ELSE
        CALL VARRAN(DRVEC,3)
        R = DRVEC(1)
        DO 215 J=1,JMAX
        JSTOP=J
  215   IF(ZINT(J).GT.R) GOTO 216
  216   CONTINUE
        XR=XX(JSTOP,1)+DX(JSTOP,1)*DRVEC(2)
        YR=XX(JSTOP,2)+DX(JSTOP,2)*DRVEC(3)
        FN=FUNSKO(XR,YR)
        IF(FN.LT.0.) GOTO 999
        YYMAX=YYMX(JSTOP)
        WT=FN/YYMAX
        WTMXX = MAX(WTMXX,WT)
cc      IF(NEVS.LE.(4*JLIM2).AND.WT.GT.WTMAX) THEN
cc         WTMAX=WT*1.1D0
cc         WRITE(6,*) ' VESKO2: NEVS, new WTMAX= ',NEVS,WTMAX
cc      ENDIF
        NEVS=NEVS+1
        SWT=SWT+WT
        SSWT=SSWT+WT*WT
C((((((((((
C       CALL HFILL(1,WT,0D0,1D0)
C))))))))))
ccc   ENDIF
CCC    CALL VARRAN(DRVEC,1)
ccc    RN=DRVEC(1)
ccc   IF(WTMAX*RN.GT.WT) GOTO 200
      X=XR
      Y=YR
C((((((((((
C     CALL HFILL(2,XR,YR)
C))))))))))
      RETURN
C...
  300 CONTINUE
C THIS IS THE VALUE OF THE INTEGRAL
      CINTEG=ZSUM*SWT/NEVS
C AND ITS ERROR
      ERRINT=SQRT(SSWT/SWT**2-1D0/NEVS)
      X=CINTEG
      Y=ERRINT
      WT=ZSUM
C((((((((((
C     CALL HPRINT(1)
C     CALL HDELET(1)
C     CALL HPRINT(2)
C     CALL HDELET(2)
      PRINT 7000,NEVS,NEVOV,WTMAX,WTMXX
 7000 FORMAT(' VESK2W: NEVS,NEVOV,WTMAX,WTMXX= ',2I7,2F7.3)
C))))))))))
      RETURN
  980 WRITE(NOUT,9002)
 9002 FORMAT(' **** STOP IN VESK2W, LACK OF INITIALISATION   ')
      STOP
  999 WRITE(NOUT,9004)
 9004 FORMAT(' **** STOP IN VESK2W, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END
 
      SUBROUTINE VINSKW(FUNSKO)
C     *************************
C THIS ROUTINE BELONGS TO VESKO2 PACKAGE
C JLIM1 IS THE NUMBER OF CELLS, DIVISION OF THE UNIT PLAQUE INTO CELLS
C IS MADE IN THE FIRST STAGE.    JLIM2 IS THE TOTAL MAXIMUM
C NUMBER OF CELLS, NOTE THAT DIMENSIONS OF
C MATRICES IN /VESKOA/ SHOULD BE AT LEAST JLIM2
C     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C ------------------------------------------------------------
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      EXTERNAL FUNSKO
 
C...  INITIALISATION PART, SAMPLING DISTRIBUTION FUNSKO
C...  AND FILLING MATRICES XX,YY,ZINT ETC.
      JMAX=1
      XX(1,1)=0D0
      XX(1,2)=0D0
      DX(1,1)=1D0
      DX(1,2)=1D0
      LEV(1)=1
      SUM=0D0
      DO 150 I=1,2
      DO 150 K=1,2
C... THIS IS NOT ELEGANT BUT SIMPLE
      YY(1,I,K)=FUNSKO(XX(1,1)+(I-1.)*DX(1,1),XX(1,2)+(K-1.)*DX(1,2))
      IF(YY(1,I,K).LT.0.0) GO TO 999
  150 SUM=SUM+YY(1,I,K)
      ZINT(1)=SUM*DX(1,1)*DX(1,2)/4D0
 
      JDIV=1
      DO 200 KK=1,JLIM2-1
      IF(JMAX.LT.JLIM1) THEN
C...    NOTE THAT DIVSKW INCREMENTS JMAX=JMAX+1 IN EVERY CALL
        CALL DIVSKW(JDIV,FUNSKO)
C(((((((((((
c      IF(JMAX.EQ.JLIM1) THEN
c      PRINT 9900,JMAX,(LEV(I),I=1,JMAX)
c 9900 FORMAT(///,' JMAX...  LEV LEV LEV LEV LEV',I10,/(24I5))
c      PRINT 9901,((XX(JD,I),I=1,2),JD=1,JMAX)
c 9901 FORMAT('  XX XX XX XX XX XX XX  ',/(10E12.5))
c      PRINT 9902,((DX(JD,I),I=1,2),JD=1,JMAX)
c 9902 FORMAT('  DX  DX DX DX DX DX ',/(10E12.5))
c      PRINT 9903,(((YY(JD,I,K),I=1,2),K=1,2),JD=1,JMAX)
c 9903 FORMAT('  YY  YY YY YY YY YY ',/(8E15.5))
c      PRINT 9904,(ZINT(I),I=1,JMAX)
c 9904 FORMAT('   ZINT ZINT ZINT ZINT ',/(10E12.5))
c      ENDIF
C))))))))))))
        JDIV=JDIV+2
        IF(JDIV.GT.JMAX) JDIV=1
      ELSE
        JDIV=1
        ZMX=ZINT(1)
        DO 180 J=1,JMAX
        IF(ZMX.LT.ZINT(J)) THEN
          ZMX=ZINT(J)
          JDIV=J
        ENDIF
  180   CONTINUE
        CALL DIVSKW(JDIV,FUNSKO)
      ENDIF
  200 CONTINUE
 
C(((((((((((
c      JPRN=64
c      PRINT 9910,JMAX,(LEV(I),I=1,JMAX)
c 9910 FORMAT(/,' JMAX...  LEV LEV LEV LEV LEV',I10,/(24I5))
c      IF(JMAX.LE.JPRN) PRINT 9911,((XX(JD,I),I=1,2),JD=1,JMAX)
c 9911 FORMAT('  XX XX XX XX XX XX XX  ',/(10E12.5))
c      IF(JMAX.LE.JPRN) PRINT 9912,((DX(JD,I),I=1,2),JD=1,JMAX)
c 9912 FORMAT('  DX  DX DX DX DX DX ',/(10E12.5))
c      IF(JMAX.LE.JPRN) PRINT 9913,
C     $   (((YY(JD,I,K),I=1,2),K=1,2),JD=1,JMAX)
c 9913 FORMAT('  YY  YY YY YY YY YY ',/(8E15.5))
c      IF(JMAX.LE.JPRN) PRINT 9914,(ZINT(I),I=1,JMAX)
c 9914 FORMAT('   ZINT ZINT ZINT ZINT ',/(10E12.5))
C     DO 902 J=1,JMAX
C     Z=1D0*J-.5D0
C 902 CALL HFILL(202,Z,ZINT(J))
C))))))))))))
C...  FINAL ADMINISTRATION, NORMALIZING ZINT ETC.
      ZSUM1=0D0
      ZSUM =0D0
      DO 260 J=1,JMAX
      ZSUM1=ZSUM1+ZINT(J)
      YMAX= 0D0
      DO 250 I=1,2
      DO 250 K=1,2
  250 YMAX= MAX(YMAX,YY(J,I,K))
      YYMX(J)=YMAX
      ZINT(J)=YMAX*DX(J,1)*DX(J,2)
  260 ZSUM=ZSUM+ZINT(J)
C((((((((
      ZR=ZSUM1/ZSUM
      PRINT 7000,ZR
 7000 FORMAT(' /////// ZSUM1/ZSUM= ',F20.8)
C)))))))))
      SUM=0D0
      DO 240 J=1,JMAX
      SUM=SUM+ZINT(J)
  240 ZINT(J)=SUM/ZSUM
C(((((((((((
c     JPRN=64
c     PRINT 9932,JMAX
c9932 FORMAT(/'=====JMAX ZINT ZINT ZINT  ',I10)
c     IF(JMAX.LE.JPRN) PRINT 9935,(ZINT(I),I=1,JMAX)
c9935            FORMAT(10E12.5)
C     DO 901 J=2,JMAX
C 901 CALL HFILL(201,(ZINT(J)-ZINT(J-1))*JMAX)
C     CALL HFILL(201,ZINT(1)*JMAX)
C))))))))))))
      RETURN
  999 WRITE(NOUT,9000)
 9000 FORMAT(' **** STOP IN VINSKW, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END
 
      SUBROUTINE DIVSKW(JD,FUNSKO)
C     ****************************
C THIS ROUTINE BELONGS TO VESKO2 PACKAGE
C IT SUBDIVIDES ONE CELL (NO. JD) INTO TWO EQUAL SIZE CELLS
C     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C ------------------------------------------------------------
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      EXTERNAL FUNSKO
 
C...  MOOVE TO MAKE A HOLE FOR A NEW ENTRY (ONE ADDITIONAL CELL)
      DO 100 J=JMAX,JD,-1
      ZINT(J+1)=ZINT(J)
      LEV(J+1)=LEV(J)
      DO 100 I=1,2
      XX(J+1,I)  =XX(J,I)
      DX(J+1,I)  =DX(J,I)
      DO 100 K=1,2
  100 YY(J+1,I,K)  =YY(J,I,K)
C...  CREATE TWO NEW CELLS AND STORE THEM
      LL= MOD(LEV(JD),2)+1
      DX(JD,LL)=DX(JD,LL)/2D0
      DX(JD+1,LL)=DX(JD+1,LL)/2D0
      XX(JD+1,LL)=XX(JD,LL)+DX(JD,LL)
      IF(LL.EQ.1) THEN
        DO 150 I=1,2
C... THIS IS NOT ELEGANT, PROBABLY COULD BE DONE BETTER
        YY(JD,2,I)=FUNSKO(XX(JD,1)+DX(JD,1),XX(JD,2)+(I-1.)*DX(JD,2))
  150   YY(JD+1,1,I)=YY(JD,2,I)
      ELSE
        DO 152 I=1,2
        YY(JD,I,2)=FUNSKO(XX(JD,1)+(I-1.)*DX(JD,1),XX(JD,2)+DX(JD,2))
  152   YY(JD+1,I,1)=YY(JD,I,2)
      ENDIF
C...  ESTIMATE THE INTEGRALS OVER NEW CELLS RESULTING FROM DIVISION
      DO 220 JDV=JD,JD+1
      LEV(JDV)=LEV(JDV)+1
      SUM=0D0
      DO 210 I=1,2
      DO 210 K=1,2
      IF(YY(JDV,I,K).LT.0.D0) GO TO 999
  210 SUM=SUM+YY(JDV,I,K)
  220 ZINT(JDV) =SUM*DX(JDV,1)*DX(JDV,2)/4D0
      JMAX=JMAX+1
      RETURN
  999 WRITE(NOUT,9000)
 9000 FORMAT(' **** STOP IN DIVSKW, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END
 
 
      SUBROUTINE WMONIT(MODE,ID,WT,WTMAX,RN)
C     **************************************
C last correction 19 sept. 89
C Utility program for monitoring M.C. rejection weights.
C ID is weight idendifier, maximum IDMX (defined below).
C WT IS WEIGHT, WTMAX IS MAXIMUM WEIGHT AND RN IS RANDOM NUMBER.
C IF(MODE.EQ.-1) THEN
C          INITALIZATION IF ENTRY ID, OTHER ARGUMENTS ARE IGNORED
C ELSEIF(MODE.EQ.0) THEN
C          SUMMING UP WEIGHTS ETC. FOR A GIVEN EVENT FOR ENTRY ID
C        - WT IS CURRENT WEIGHT.
C        - WTMAX IS MAXIMUM WEIGHT USED FOR COUTING OVERWEIGHTED
C          EVENTS WITH WT>WTMAX.
C        - RN IS RANDOM NUMBER USED IN REJECTION, IT IS USED TO
C          COUNT NO. OF ACCEPTED (RN<WT/WTMAX) AND REJECTED
C          (WT>WT/WTMAX) EVENTS,
C          IF RO REJECTION THEN PUT RN=0D0.
C ELSEIF(MODE.EQ.1) THEN
C          IN THIS MODE WMONIT REPPORTS ON ACCUMULATED STATISTICS
C          AND THE INFORMATION IS STORED IN COMMON /CMONIT/
C        - AVERWT= AVERAGE WEIGHT WT COUNTING ALL EVENT
C        - ERRELA= RELATIVE ERROR OF AVERWT
C        - NEVTOT= TOTAL NIMBER OF ACCOUNTED EVENTS
C        - NEVACC= NO. OF ACCEPTED EVENTS (RN<WT\WTMAX)
C        - NEVNEG= NO. OF EVENTS WITH NEGATIVE WEIGHT (WT<0)
C        - NEVZER= NO. OF EVENTS WITH ZERO WEIGHT (WT.EQ.0D0)
C        - NEVOVE= NO. OF OVERWEGHTED EVENTS (WT>WTMAX)
C          AND IF YOU DO NOT WANT TO USE CMONIT THEN THE VALUE
C          The value of AVERWT is assigned to WT,
C          the value of ERRELA is assigned to WTMAX and
C          the value of WTMAX  is assigned to RN in this mode.
C ELSEIF(MODEE.EQ.2) THEN
C          ALL INFORMATION DEFINED FOR ENTRY ID DEFINED ABOVE
C          FOR MODE=2 IS JUST PRINTED OF UNIT NOUT
C ENDIF
C NOTE THAT OUTPUT REPPORT (MODE=1,2) IS DONE DYNAMICALLY JUST FOR A
C GIVEN ENTRY ID ONLY AND IT MAY BE REPEATED MANY TIMES FOR ONE ID AND
C FOR VARIOUS ID'S AS WELL.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(IDMX=100)
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / INOUT  / NINP,NOUT,NOUT2
      INTEGER NTOT(IDMX),NACC(IDMX),NNEG(IDMX),NOVE(IDMX),NZER(IDMX)
      DIMENSION SWT(IDMX),SSWT(IDMX),WWMX(IDMX)
      DATA NTOT /IDMX* -1/  SWT /IDMX*   0D0/
      DATA SSWT /IDMX*0D0/ WWMX /IDMX*-1D-20/
C
      IF(ID.LE.0.OR.ID.GT.IDMX) THEN
           WRITE(NOUT,*) ' =====WMONIT: WRONG ID',ID
           STOP
      ENDIF
      IF(MODE.EQ.-1) THEN
           NTOT(ID)=0
           NACC(ID)=0
           NNEG(ID)=0
           NZER(ID)=0
           NOVE(ID)=0
           SWT(ID)   =0D0
           SSWT(ID)  =0D0
           WWMX(ID)  = -1D-20
      ELSEIF(MODE.EQ.0) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           NTOT(ID)=NTOT(ID)+1
           SWT(ID)=SWT(ID)+WT
           SSWT(ID)=SSWT(ID)+WT**2
           WWMX(ID)= MAX(WWMX(ID),WT)
           IF(WT.EQ.0D0)   NZER(ID)=NZER(ID)+1
           IF(WT.LT.0D0)   NNEG(ID)=NNEG(ID)+1
           IF(WT.GT.WTMAX)      NOVE(ID)=NOVE(ID)+1
           IF(RN*WTMAX.LE.WT)   NACC(ID)=NACC(ID)+1
      ELSEIF(MODE.EQ.1) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
           ENDIF
           NEVTOT=NTOT(ID)
           NEVACC=NACC(ID)
           NEVNEG=NNEG(ID)
           NEVZER=NZER(ID)
           NEVOVE=NOVE(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSEIF(MODE.EQ.2) THEN
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
              WWMAX=WWMX(ID)
           ENDIF
           WRITE(NOUT,1003) ID, AVERWT, ERRELA, WWMAX
           WRITE(NOUT,1004) NTOT(ID),NACC(ID),NNEG(ID),NOVE(ID),NZER(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSE
           WRITE(NOUT,*) ' =====WMONIT: WRONG MODE',MODE
           STOP
      ENDIF
 1003 FORMAT(
     $  ' =======================WMONIT========================'
     $/,'   ID           AVERWT         ERRELA            WWMAX'
     $/,    I5,           E17.7,         F15.9,           E17.7)
 1004 FORMAT(
     $  ' -----------------------------------------------------------'
     $/,'      NEVTOT      NEVACC      NEVNEG      NEVOVE      NEVZER'
     $/,   5I12)
      END
 
      SUBROUTINE WMONI2(MODE,ID,WT,WTMAX,RN)
C     **************************************
C -------------- SECOND COPY OF WMONIT ----------------
C last correction 19 sept. 89
C Utility program for monitoring M.C. rejection weights.
C ID is weight idendifier, maximum IDMX (defined below).
C WT IS WEIGHT, WTMAX IS MAXIMUM WEIGHT AND RN IS RANDOM NUMBER.
C IF(MODE.EQ.-1) THEN
C          INITALIZATION IF ENTRY ID, OTHER ARGUMENTS ARE IGNORED
C ELSEIF(MODE.EQ.0) THEN
C          SUMMING UP WEIGHTS ETC. FOR A GIVEN EVENT FOR ENTRY ID
C        - WT IS CURRENT WEIGHT.
C        - WTMAX IS MAXIMUM WEIGHT USED FOR COUTING OVERWEIGHTED
C          EVENTS WITH WT>WTMAX.
C        - RN IS RANDOM NUMBER USED IN REJECTION, IT IS USED TO
C          COUNT NO. OF ACCEPTED (RN<WT/WTMAX) AND REJECTED
C          (WT>WT/WTMAX) EVENTS,
C          IF RO REJECTION THEN PUT RN=0D0.
C ELSEIF(MODE.EQ.1) THEN
C          IN THIS MODE WMONIT REPPORTS ON ACCUMULATED STATISTICS
C          AND THE INFORMATION IS STORED IN COMMON /CMONIT/
C        - AVERWT= AVERAGE WEIGHT WT COUNTING ALL EVENT
C        - ERRELA= RELATIVE ERROR OF AVERWT
C        - NEVTOT= TOTAL NIMBER OF ACCOUNTED EVENTS
C        - NEVACC= NO. OF ACCEPTED EVENTS (RN<WT\WTMAX)
C        - NEVNEG= NO. OF EVENTS WITH NEGATIVE WEIGHT (WT<0)
C        - NEVZER= NO. OF EVENTS WITH ZERO WEIGHT (WT.EQ.0D0)
C        - NEVOVE= NO. OF OVERWEGHTED EVENTS (WT>WTMAX)
C          AND IF YOU DO NOT WANT TO USE CMONIT THEN THE VALUE
C          The value of AVERWT is assigned to WT,
C          the value of ERRELA is assigned to WTMAX and
C          the value of WTMAX  is assigned to RN in this mode.
C ELSEIF(MODEE.EQ.2) THEN
C          ALL INFORMATION DEFINED FOR ENTRY ID DEFINED ABOVE
C          FOR MODE=2 IS JUST PRINTED OF UNIT NOUT
C ENDIF
C NOTE THAT OUTPUT REPPORT (MODE=1,2) IS DONE DYNAMICALLY JUST FOR A
C GIVEN ENTRY ID ONLY AND IT MAY BE REPEATED MANY TIMES FOR ONE ID AND
C FOR VARIOUS ID'S AS WELL.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(IDMX=100)
      COMMON / CMONI2/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / INOUT  / NINP,NOUT,NOUT2
      INTEGER NTOT(IDMX),NACC(IDMX),NNEG(IDMX),NOVE(IDMX),NZER(IDMX)
      DIMENSION SWT(IDMX),SSWT(IDMX),WWMX(IDMX)
      DATA NTOT /IDMX* -1/  SWT /IDMX*   0D0/
      DATA SSWT /IDMX*0D0/ WWMX /IDMX*-1D-20/
C
      IF(ID.LE.0.OR.ID.GT.IDMX) THEN
           WRITE(NOUT,*) ' =====WMONI2: WRONG ID',ID
           STOP
      ENDIF
      IF(MODE.EQ.-1) THEN
           NTOT(ID)=0
           NACC(ID)=0
           NNEG(ID)=0
           NZER(ID)=0
           NOVE(ID)=0
           SWT(ID)   =0D0
           SSWT(ID)  =0D0
           WWMX(ID)  = -1D-20
      ELSEIF(MODE.EQ.0) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           NTOT(ID)=NTOT(ID)+1
           SWT(ID)=SWT(ID)+WT
           SSWT(ID)=SSWT(ID)+WT**2
           WWMX(ID)= MAX(WWMX(ID),WT)
           IF(WT.EQ.0D0)   NZER(ID)=NZER(ID)+1
           IF(WT.LT.0D0)   NNEG(ID)=NNEG(ID)+1
           IF(WT.GT.WTMAX)      NOVE(ID)=NOVE(ID)+1
           IF(RN*WTMAX.LE.WT)   NACC(ID)=NACC(ID)+1
      ELSEIF(MODE.EQ.1) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONI2: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
           ENDIF
           NEVTOT=NTOT(ID)
           NEVACC=NACC(ID)
           NEVNEG=NNEG(ID)
           NEVZER=NZER(ID)
           NEVOVE=NOVE(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSEIF(MODE.EQ.2) THEN
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
              WWMAX=WWMX(ID)
           ENDIF
           WRITE(NOUT,1003) ID, AVERWT, ERRELA, WWMAX
           WRITE(NOUT,1004) NTOT(ID),NACC(ID),NNEG(ID),NOVE(ID),NZER(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSE
           WRITE(NOUT,*) ' =====WMONI2: WRONG MODE',MODE
           STOP
      ENDIF
 1003 FORMAT(
     $  ' =======================WMONI2========================'
     $/,'   ID           AVERWT         ERRELA            WWMAX'
     $/,    I5,           E17.7,         F15.9,           E17.7)
 1004 FORMAT(
     $  ' -----------------------------------------------------------'
     $/,'      NEVTOT      NEVACC      NEVNEG      NEVOVE      NEVZER'
     $/,   5I12)
      END
