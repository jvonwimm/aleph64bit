C --------------------------------------------------------------------          
C EEGG01 : original version transmitted by P. Gay June 1988                     
C                                                                               
C G. Bonneaud June, 8, 1988                                                     
C- Modification by B. Bloch-Devaux on june 17, 1988                             
C 1)Function SPENCE has value -66.D36 instead of -66.D66                        
C  so that it can compile on VAX. It was assumed to be initialised              
C  to a 'big' negative value........                                            
C 2) Also the size of the code was reduced by almost 15% by removing            
C    blank lines.......                                                         
C J. von Wimmersperg 2019
C  1) Use Ranmar instead of Rndgen
C  2) Report cross section fraction for overweighted events
C  3) extra cut option CEGMAX in EGamma config with hard rad corr.
C  4) additional minor cuts to avoid large weights in hard rad. corr
C     for etron and gamma configs (avoid ~ back-to-back gammas) 
C--------------------------------------------------------------------          
C  Original program : TEEGG7                                                    
C  Author : D.Karlen                                                            
C                                                                               
C                                                                               
C                                                                               
C          P. GAY (Clermont fd 5/88)                                            
C   modifications :                                                             
C     -  The FORTRAN statement INCLUDE (TEEGG7) is remplaced by its             
C             explicit meaning.                                                 
C     -  To book the different value of the weight:                             
C             new code lines in the routines TEEGGS,T3BODY and T4BODY           
C                                                                               
C                                                                               
C-----------------------------------------------------------------------        
C Event generation main program (called from TEEGGX) for stand alone.           
      SUBROUTINE TEEGGS(TITLE)                                                  
      IMPLICIT LOGICAL*1(A-Z)                                                   
      INTEGER*4 PLUN                                                            
      PARAMETER (PLUN=6)                                                        
      INTEGER*4 NEV                                                             
      COMMON/PARCOM/NEV                                                         
      INTEGER*4 I                                                               
      CHARACTER*(*)TITLE                                                        
      EXTERNAL TEEGGL                                                           
      LOGICAL  TEEGGL                                                           
C Write out the title for the run                                               
      WRITE(PLUN,'(''1'')')                                                     
      WRITE(PLUN,*)'Title: ',TITLE                                              
      WRITE(PLUN,*)' '                                                          
C Check that the parameters are valid.                                          
      IF(TEEGGL(PLUN))THEN                                                      
C    modif                                                                      
C    init the histogramma                                                       
C                                                                               
      CALL HDELET(0)                                                            
C                                                                               
      CALL HBOOK1(4001,' Weight WGHT T3BODY  $',50,0.D0,WGHTMX)                 
      CALL HBOOK1(4002,' Weight WGHT T4BODY  $',50,0.D0,WGHTMX)                 
C                                                                               
      CALL HBOOK1(4003,' Weight WGHT1 T3BODY  $',50,0.D0,WGHT1M)                
      CALL HBOOK1(4004,' Weight WGHT1 T4BODY  $',50,0.D0,WGHT1M)                
C                                                                               
C Here is the Main loop.                                                        
C ---------------------                                                         
         DO 1 I=1,NEV                                                           
C Call the generating routine. (Generates one event).                           
            CALL TEEGG7                                                         
C Call the user supplied event analysis routine.                                
C           CALL EVANAL                                                         
 1       CONTINUE                                                               
C Calculate the total cross section etc.                                        
         CALL TEEGGC                                                            
C Print out the summary header                                                  
         CALL TEEGGP(PLUN)                                                      
C Call the user supplied print routine.                                         
C        CALL PRINT(PLUN)                                                       
C    modif                                                                      
C    print the histogramma                                                      
C                                                                               
      CALL HOUTPU(PLUN)                                                         
      CALL HISTDO(0)                                                            
C                                                                               
      ENDIF                                                                     
C All done for this set of parameters. Return to TEEGGX                         
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C Random number routine: put N random numbers in common RND                     
      SUBROUTINE RNDGEN(N)                                                      
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
      INTEGER*4 IR(1),N,NR,JSEED                                                   
      NR=MIN(NRNDMX,N)                                                          
      SEED=NXSEED                                                               
C Fill array RND with NR random numbers.                                        
      CALL ATRAN7(RND(1),NR)                                                    
C Find the seed that continues the sequence.                                    
      CALL ITRAN7(IR,1)                                                         
      NXSEED=IR(1)                                                                 
      RETURN                                                                    
      ENTRY RNDIN(JSEED)                                                        
C TRAN7 requires a positive seed to work properly.                              
      CALL TRAN7A(ABS(JSEED))                                                   
      NXSEED=JSEED                                                              
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C Interface to TRAND routine                                                     
C                                                                               
      SUBROUTINE ATRAN7(ARRAY,NUM)                                              
      REAL*4 ARRAY(NUM)                                                           
      INTEGER*4 IARRAY(NUM)                                                       
      DO 1 I=1,NUM                                                              
 1    ARRAY(I)=TRAND(IX)                                                         
      RETURN                                                                    
      ENTRY ITRAN7(IARRAY,NUM)                                                  
      DO 2 I=1,NUM                                                              
      DUM=TRAND(IX)                                                              
 2    IARRAY(I)=IX                                                              
      RETURN                                                                    
      ENTRY TRAN7A(IY)                                                          
      IX=IY                                                                     
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C This is the code for the RAN7 random number generator:                        
C                                                                               
C Reference : P.A.W. Lewis, A.S. Goodman, and J.M. Miller,                      
C             "Pseudo-Random Number Generator for the System/360",              
C             IBM Syst. J. 8,2 (1969) 136-146                                   
C                                                                               
                  FUNCTION TRAND(IX)                                             
C PORTABLE RANDOM NUMBER GENERATOR USING MULTIPLICATIVE                         
C CONGRUENTIAL METHOD, MULTIPLIER=16807, MODULUS=2**31-1.                       
C NOTE - IX MUST NOT BE CHANGED BETWEEN CALLS.                                  
C                                                                               
C SOME COMPILERS, E.G. THE HP3000, REQUIRE THE FOLLOWING                        
C DECLARATION TO BE INTEGER*4                                                   
      INTEGER A,P,IX,B15,B16,XHI,XALO,LEFTLO,FHI,K                              
C                                                                               
C CONSTANTS   7**5,     2**15,     2**16,      2**31-1                          
      DATA A/16807/,B15/32768/,B16/65536/,P/2147483647/                         
C                                                                               
C GET 15 HIGH-ORDER BITS OF IX                                                  
      XHI = IX / B16                                                            
C GET 16 LOW-ORDER BITS OF IX AND FORM LOW-ORDER PRODUCT                        
      XALO = (IX - XHI * B16) * A                                               
C GET 15 HIGH-ORDER BITS OF LOW-ORDER PRODUCT                                   
      LEFTLO = XALO / B16                                                       
C FORM THE 31 HIGHEST BITS OF THE FULL PRODUCT                                  
      FHI = XHI * A + LEFTLO                                                    
C GET OVERFLOW PAST 31-ST BIT OF FULL PRODUCT                                   
      K = FHI / B15                                                             
C ASSEMBLE ALL PARTS, PRESUBTRACT P (THE PARENS ARE ESSENTIAL)                  
      IX = (((XALO-LEFTLO*B16)-P) + (FHI-K*B15) * B16) + K                      
C ADD P BACK IN IF NECESSARY                                                    
      IF (IX .LT. 0) IX = IX + P                                                
C MULTIPLY BY 1/(2**31-1)                                                       
      TRAND = FLOAT(IX) * 4.656612875E-10                                        
      RETURN                                                                    
      END                                                                       
C -------------------------TEEGG CONTROL FILE---------------------------        
C This file controls the execution of TEEGG:                                    
C  - Change any parameters as desired. (see TEEGG TEX)                          
C  - To call the event generator,  CALL TEEGGS('Title')                         
C  - To re-initialize the random number generator with seed in ISEED,           
C    CALL RNDIN(ISEED).                                                         
C-----------------------------------------------------------------------        
      SUBROUTINE TEEGGX                                                         
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      INTEGER*4 I,NEV                                                           
      COMMON/PARCOM/NEV                                                         
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
      ISEED= 123454321                                                          
      CALL RNDIN(ISEED)                                                         
C-----------------------------                                                  
C A test e-gamma configuration                                                  
C-----------------------------                                                  
      RADCOR=NONE                                                               
      CONFIG=EGAMMA                                                             
      EB=49.                                                                    
      TEVETO=30.D-3                                                             
      TGMIN= 15. * PI/180.                                                      
      TEMIN= 15. * PI/180.                                                      
      EGMIN=5.0D0                                                               
      EEMIN=5.0D0                                                               
      NEV=100                                                                   
      WGHTMX=0.5                                                                
      CALL TEEGGS(' A test of lowest order e-gamma configuration')              
      NEV=50                                                                    
      CUTOFF=0.01D0                                                             
      RADCOR=SOFT                                                               
      CALL TEEGGS(' A test of e-gamma configuration with soft corr.')           
      NEV=10                                                                    
      WGHTMX=2.D0                                                               
      RADCOR=HARD                                                               
      CALL TEEGGS(' A test of e-gamma configuration - (e-e-g-g)')               
      RETURN                                                                    
      END                                                                       
C                                                                               
C     -----------                                                               
C     Version 7.1                                                               
C     -----------                                                               
C                                                                               
C     Dean Karlen                                                               
C     Stanford Linear Accelerator Center                                        
C     June 1987                                                                 
C                                                                               
C Changes since Version 7.0:                                                    
C                                                                               
C o form of 3rd order EPA cross section put in terms of invariants              
C o add Martinez/Miquel matrix element for e-e-g-g (very CPU intensive)         
C o allow generation of weighted events (useful for looking at                  
C   distributions and calculating cross sections for many acceptances).         
C o new configuration: GAMMAE, 4th order single photon with a (soft)            
C   electron in the acceptance.                                                 
C o new MTRXGG: HEEGG, a hybrid of EPADC and BEEGG                              
C                                                                               
C **********************************************************************        
C *                                                                    *        
C * TEEGG - A monte carlo event generator to simulate the process,     *        
C *                                                                    *        
C *          +  -      +  -                                            *        
C *         e  e  --> e  e  gamma (gamma)    for t-channel dominated   *        
C *                                          configurations.           *        
C *                                                                    *        
C * Reference: D. Karlen, Nucl. Phys. B289 (1987) 23                   *        
C *                                                                    *        
C * > The three different configurations with their input parameters:  *        
C *    1) e-gamma (Both an electron and gamma are 'seen'):             *        
C *       TEVETO - maximum theta of the 'missing' electron             *        
C *       TEMIN  - minimum theta of the 'seen' electron  |TEMIN and/or *        
C *       TGMIN  - minimum theta of the 'seen' gamma     | TGMIN>TEVETO*        
C *       EEMIN  - minimum energy of the 'seen' electron               *        
C *       EGMIN  - minimum energy of the 'seen' gamma                  *        
C *    2) electron (Only the electron is 'seen'):                      *        
C *       TEVETO - maximum theta of the 'missing' electron             *        
C *       TEMIN  - minimum theta of the 'seen' electron   |TEMIN>TEVETO*        
C *       TGVETO - maximum theta of the 'missing' gamma(s)|TEMIN>TGVETO*        
C *       EEMIN  - minimum energy of the 'seen' electron               *        
C *    3) gamma (Only the gamma is 'seen'):                            *        
C *       TEVETO - maximum theta of the 'missing' electrons            *        
C *       TGMIN  - minimum theta of the 'seen' gamma       TGMIN>TEVETO*        
C *       EGMIN  - minimum energy of the 'seen' gamma                  *        
C *                                                                    *        
C * > The fourth order process is divided into two regions:            *        
C *    1) SOFT PART - second gamma has energy < CUTOFF (in CM frame)   *        
C *                 - only one photon is generated                     *        
C *    2) HARD PART - second gamma has energy > CUTOFF (in CM frame)   *        
C *   The two regions must be generated separately.                    *        
C *                                                                    *        
C **********************************************************************        
C This file contains the following :                                            
C User callable routines:                                                       
C     TEEGGI,TEEGGL,TEEGG7,TEEGGC,TEEGGP,TEEGGA                                 
C Internal routines:                                                            
C     T3BODY,T4BODY,FQPEG,DMORK,SPENCE,TBOORT,BEEGGC,BCOLL,BEEGGM,TPRD          
C     INMART,MEEGGC                                                             
C Routines supplied by Martinez/Miquel:                                         
C     ELEMAT,AMTOTM,AMPLIM,ZMART,SPININ                                         
C How to Use This Program With Another Monte Carlo System.                      
C --------------------------------------------------------                      
C                                                                               
C 1) Initialization                                                             
C    --------------                                                             
C  - Call TEEGGI to set the defaults for all the parameters.                    
C  - modify parameters as desired                                               
C  - Initialize the random number by: CALL RNDIN(ISEED)                         
C  - Call the logical function, TEEGGL(OLUN). This checks the validity          
C    of the parameters and calculates some constants to be used later.          
C    Returns .TRUE. if parameters are valid; .FALSE. if not. Use                
C    OLUN to specify the logical unit number for error messages to be           
C    printed. TEEGGL must be called before any new generation of events.        
C  > Input parameters are found in common TINPAR                                
C  > Constants calculated by TEEGGL are in common TCONST                        
C                                                                               
C 2) Generation                                                                 
C    ----------                                                                 
C  - Call TEEGG7 to generate one event.                                         
C  > Event information (4-vectors) are found in common TEVENT                   
C                                                                               
C 3) End of generation                                                          
C    -----------------                                                          
C  - Call TEEGGC to calculate efficiencies and total cross section from         
C    the last event sample generated.                                           
C  - Call TEEGGP(OLUN) to print out a summary header of the parameters          
C    and results from the last event generation (on unit OLUN).                 
C  > Summary information filled by TEEGGC is found in common TSMMRY             
C                                                                               
C Routines That Must be Provided (site dependant):                              
C ------------------------------------------------                              
C                                                                               
C SUBROUTINE RNDGEN(NRND)                                                       
C  - fill the REAL*4 array, RND(i) (i=1,NRND) (in COMMON TRND) with             
C    pseudo random numbers equidistributed between 0 and 1.                     
C  - optionally modify the varaibles in common TRND as follows:                 
C    - fill SEED with the seed required to generate the current set of          
C      random numbers                                                           
C    - fill NXSEED with the seed required to generate the next set of           
C      random numbers                                                           
C                                                                               
C SUBROUTINE RNDIN(JSEED)                                                       
C  - initialize random number generator with INTEGER*4 seed JSEED               
C  - optionally:                                                                
C    - fill NXSEED with JSEED. (Seed required to generate the next set          
C      of random numbers)                                                       
C                                                                               
C Unweighted vs. weighted events                                                
C ------------------------------                                                
C                                                                               
C An unweighted event sample is the easiest to use, as it represents the        
C results from a hypothetical experiment.                                       
C                                                                               
C If an integrated cross section with a more complicated acceptance than        
C can be specified with the input parameters is desired (eg. a pt cut),         
C greater efficiency can be obtained using weighted events.                     
C This form of generation is specified by setting UNWGHT=.FALSE.                
C The routine, TEEGGA, can be used in this integration, as follows.             
C After an event is generated, call TEEGGA(var1,var2,sig,ersig) if it           
C passes the complicated acceptance. The variables, var1 and var2               
C are REAL*8 variables that are used to accumulate the total weights and        
C should be set to 0 before event generation. By using other pairs of           
C variables, many acceptances can be calculated at once. After the event        
C generation, and after TEEGGC is called, call TEEGGA once again and the        
C total cross section and error is returned in sig and ersig.                   
C The use of weighted events in histogramming is more efficient also.           
C The variable WGHT in common TEVENT specifies the event weight.                
C-----------------------------------------------------------------------        
C SUBROUTINE TEEGGI : Sets default values for the parameters.                   
C-----------------------------------------------------------------------        
      SUBROUTINE TEEGGI                                                         
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
C.....EB = energy of e beam (in GeV)                                            
      EB = 14.5 D0                                                              
C.....RADCOR= HARD  generate e+ e- gamma gamma                                  
C           = SOFT  e+ e- gamma(with soft brem and virtual corrections)         
C           = NONE  e+ e- gamma(according to lowest order diagrams only)        
      RADCOR= NONE                                                              
C.....CONFIG= EGAMMA  then generate according to e-gamma configuration          
C           = ETRON   then generate according to single electron config.        
C           = GAMMA   then generate according to single gamma config.           
C           = GAMMAE  then generate accrdng to sngl gamma/soft e config.        
      CONFIG= EGAMMA                                                            
C.....MATRIX = BK    then use Berends & Kleiss matrix element                   
C            = BKM2  then use    "        "      "    " with m**2/t term        
C            = TCHAN then use t channel only matrix element (/w m term)         
C            = EPA   then use matrix element from EPA (for testing)             
      MATRIX = BKM2                                                             
C.....MTRXGG = EPADC then use EPA with double compton (RADCOR=HARD only)        
C            = BEEGG then use Berends et al. e-e-g-g  (RADCOR=HARD only)        
C            = MEEGG then use Martinez/Miquel e-e-g-g (RADCOR=HARD only)        
C            = HEEGG then use EPA for low Q**2, BEEGG otherwise ("   " )        
      MTRXGG = EPADC                                                            
C.....TEVETO = maximum theta of 'missing' e's in final state (in radians        
      TEVETO = 100.D-3                                                          
C.....TEMIN = minimum angle between the 'seen' electron and beam line           
C             (used for e-gamma & etron configurations)     (in radians)        
      TEMIN = ACOS(.75D0)                                                       
C.....TGMIN = minimum angle between the 1st gamma and the beam line             
C             (used for the e-gamma & gamma configurations) (in radians)        
      TGMIN = ACOS(.75D0)                                                       
C.....TGVETO = maximum angle between the 'missing' 1st gamma & beam line        
C             (only used for etron configuration)           (in radians)        
C             (also used in for the gamma configuration to veto 2nd g)          
      TGVETO = 50.D-3                                                           
C.....EEMIN = minimum energy of the 'observed' electron (in GeV)                
C             (used for the e-gamma & electron configurations)                  
      EEMIN = 2.00 D0                                                           
C.....EGMIN = minimum energy of the 'observed' 1st photon (in GeV)              
C             (used for the e-gamma & gamma configurations)                     
      EGMIN = 2.00 D0                                                           
C.....PEGMIN= minimum separation of e and gamma in phi (in radians)             
C             (used for egamma configuration with hard rad. correction)         
      PEGMIN= PI/4.D0
* jvw
*     CEGMAX = maximum cosine of angle between e an gamma
*             (used for egamma configuration with hard rad. correction)
      CEGMAX=0.95                                                           
C.....EEVETO= energy of electron required to act as a veto (in GeV)             
C             (used for gamma configuration with hard rad. correction)          
      EEVETO= 0.0 D0                                                            
C.....EGVETO= energy of gamma required to act as a veto (in GeV)                
C             (used for etron&gamma configs with hard rad. correction)          
      EGVETO= 0.0 D0                                                            
C.....PHVETO= separation of two particles in phi reqd. to act as a veto         
C             (used for e-g separation in etron config. and g-gs separ.         
C             in gamma config... all for hard rod. correction only)             
      PHVETO= PI/4.D0                                                           
C.....CUTOFF = CM cutoff energy for radiative correction (in GeV)               
      CUTOFF = 0.25 D0                                                          
C.....WGHT1M = maximum weight for generation of QP0 & cos(theta QP)             
      WGHT1M = 1.001 D0                                                         
C.....WGHTMX = maximum weight of the trial events                               
      WGHTMX = 1.00 D0                                                          
C.....EPS = arbitrary small parameter, used to stabilize weights.               
C           It determines the relative sampling of ks^pbc vs. ks^0; if          
C           large weights occur due to very hard 2nd photon,decrease EPS        
      EPS = 0.01 D0                                                             
C.....ISEED = initial seed                                                      
      ISEED = 123456789                                                         
C.....UNWGHT = logical variable, specifies if unweighted events are reqd        
C            = TRUE  Then events are unweighted (each event weight=1)           
C            = FALSE Then events are weighted(event weight given by WGHT        
      UNWGHT = .TRUE.                                                           
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C LOGICAL FUNCTION TEEGGL: Checks parameters and calculates constants.          
C-----------------------------------------------------------------------        
      FUNCTION TEEGGL(OLUN)                                                     
C Returns .TRUE. if okay ; .FALSE. if parameters are invalid.                   
C OLUN is the unit number to write out any error messages.                      
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
      LOGICAL TEEGGL                                                            
      INTEGER*4 OLUN                                                            
      REAL*8 SINTST,QP0MAX,MINEE,MINEG,MINQP,ECMMIN,SINT1,SINT2,TKMIN           
     >,      PBCMIN                                                             
      EXTERNAL FQPEG                                                            
      REAL*8 FQPEG                                                              
      TEEGGL=.TRUE.                                                             
C Run through various parameters, and check for validity                        
      IF(EB .LT. 0.1 D0 )THEN                                                   
         TEEGGL=.FALSE.                                                         
         WRITE(OLUN,*)'EB is too small. EB must be greater than .1 GeV'         
      ENDIF                                                                     
      IF(MAX(MAX(MAX(TEVETO,TEMIN),TGMIN),TGVETO) .GT. PI/2.D0)THEN             
         TEEGGL=.FALSE.                                                         
         WRITE(OLUN,*)'All ANGLE parameters must be less than pi/2'             
      ENDIF                                                                     
      IF(MIN(MIN(MIN(TEVETO,TEMIN),TGMIN),TGVETO) .LE.  0.D0)THEN               
         TEEGGL=.FALSE.                                                         
         WRITE(OLUN,*)'All ANGLE parameters must be greater than 0'             
      ENDIF                                                                     
      IF(CUTOFF .LE.  0.D0 .AND. RADCOR.NE.NONE)THEN                            
         TEEGGL=.FALSE.                                                         
         WRITE(OLUN,*)'CUTOFF must be greater than 0'                           
      ENDIF                                                                     
      IF(EEMIN.LT.1.D-4*EB .AND. CONFIG.EQ.ETRON)THEN                           
         TEEGGL=.FALSE.                                                         
         WRITE(OLUN,*)'EEMIN must be at least EB/10000. '                       
      ENDIF                                                                     
      IF(EGMIN.LT.1.D-4*EB .AND. CONFIG.NE.ETRON)THEN                           
         TEEGGL=.FALSE.                                                         
         WRITE(OLUN,*)'EGMIN must be at least EB/10000. '                       
      ENDIF                                                                     
      IF(CONFIG.EQ.EGAMMA)THEN                                                  
         IF(TEVETO.GE.TEMIN .AND. TEVETO.GE.TGMIN)THEN                          
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'TEMIN or TGMIN must be greater than TEVETO'           
         ENDIF                                                                  
      ELSE IF(CONFIG.EQ.GAMMA)THEN                                              
         IF(TEVETO  .GE. TGMIN)THEN                                             
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'TEVETO must be less than TGMIN'                       
         ENDIF                                                                  
      ELSE IF(CONFIG.EQ.GAMMAE)THEN                                             
         IF(RADCOR.NE.HARD)THEN                                                 
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'GAMMAE only valid for RADCOR = HARD'                  
         ENDIF                                                                  
         IF(TEVETO  .GE. TGMIN)THEN                                             
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'TEVETO must be less than TGMIN'                       
         ENDIF                                                                  
         IF(TGVETO  .GE. TGMIN)THEN                                             
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'TGVETO must be less than TGMIN'                       
         ENDIF                                                                  
         IF(TGVETO  .GT. TEVETO)THEN                                            
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'TGVETO must be less than or equal to TEVETO'          
         ENDIF                                                                  
         IF(EEVETO .LT. M)THEN                                                  
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'EEVETO must be greater than m_e'                      
         ENDIF                                                                  
      ELSE IF(CONFIG.EQ.ETRON)THEN                                              
         IF(TEVETO.GE.TEMIN)THEN                                                
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'TEVETO must be less than TEMIN'                       
         ENDIF                                                                  
C The following is made for convenience. If necessary it can be removed,        
C but then theta-gamma generation must allow +z side final state.               
         IF(TGVETO.GE.TEMIN)THEN                                                
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'TGVETO must be less than TEMIN'                       
         ENDIF                                                                  
      ELSE                                                                      
         TEEGGL=.FALSE.                                                         
         WRITE(OLUN,*)'Invalid configuration. Choose EGAMMA, GAMMA, ',          
     >                ' or ETRON.'                                              
      ENDIF                                                                     
      IF(RADCOR.NE.HARD .AND. RADCOR.NE.SOFT .AND. RADCOR.NE.NONE)THEN          
         TEEGGL=.FALSE.                                                         
         WRITE(OLUN,*)'Invalid RADCOR. Choose HARD, SOFT,or NONE.'              
      ENDIF                                                                     
C Check that MATRIX element is consistent with RADCOR                           
      IF(RADCOR.NE.HARD)THEN                                                    
         IF(MATRIX.NE.BK    .AND. MATRIX.NE.BKM2 .AND.                          
     >      MATRIX.NE.TCHAN .AND. MATRIX.NE.EPA       )THEN                     
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)'Invalid MATRIX. Choose BK, BKM2, TCHAN,or EPA'        
         ENDIF                                                                  
      ELSE                                                                      
         IF(MTRXGG.NE.EPADC .AND. MTRXGG.NE.BEEGG .AND.                         
     >      MTRXGG.NE.MEEGG .AND. MTRXGG.NE.HEEGG      )THEN                    
            TEEGGL=.FALSE.                                                      
            WRITE(OLUN,*)                                                       
     >      'Invalid MTRXGG. Choose EPADC, BEEGG, MEEGG, or HEEGG'              
         ENDIF                                                                  
         IF(MTRXGG.EQ.BEEGG)THEN                                                
          IF(CONFIG.EQ.GAMMA)                                                   
     >    WRITE(OLUN,*)'MTRXGG=BEEGG may not valid be for CONFIG=GAMMA'         
          IF(CONFIG.EQ.ETRON)                                                   
     >    WRITE(OLUN,*)'MTRXGG=BEEGG may not valid be for CONFIG=ETRON'         
          IF(CONFIG.EQ.GAMMAE)                                                  
     >    WRITE(OLUN,*)'MTRXGG=BEEGG may not valid be for CONFIG=GAMMAE'        
         ENDIF                                                                  
      ENDIF                                                                     
      IF(RADCOR.EQ.HARD .AND. EPS.LE.0.D0)THEN                                  
         TEEGGL=.FALSE.                                                         
         WRITE(OLUN,*)'EPS must be greater than 0.'                             
      ENDIF                                                                     
C If the parameters look good, continue on...                                   
      IF(TEEGGL)THEN                                                            
C Keep track of the initial seed                                                
         BSEED=NXSEED                                                           
C Initialize some variables.                                                    
         NTRIAL=0                                                               
         NPASSQ=0                                                               
         NACC  =0                                                               
         W1MAX =0.D0                                                            
         WMAX  =0.D0                                                            
         WMINSF=1.D20                                                           
         Q2W2MX=0.D0                                                            
         SUMW1 =0.D0                                                            
         SUMW12=0.D0                                                            
         SUMWGT=0.D0
* jvw
         SUMWGTO=0.D0
*                                                            
         SUMW2 =0.D0                                                            
         ASOFTC=1.D0                                                            
         CONVER=0.D0                                                            
         P(13)=0.D0                                                             
         P(14)=0.D0                                                             
         P(15)=0.D0                                                             
         P(16)=0.D0                                                             
C Some useful constants                                                         
         S=4.D0*EB**2                                                           
         SQRTS=2.D0*EB                                                          
         EBP = SQRT(EB**2 -M**2)                                                
         EPSLON=2.D0*M**2/S                                                     
         CDELT=COS(TEVETO)                                                      
C CDELT1 = 1 - COS(TEVETO)                                                      
         CDELT1 = 2.D0 * SIN(TEVETO/2.D0)**2                                    
C CTGVT1 = 1 - COS(TGVETO)                                                      
         CTGVT1 = 2.D0 * SIN(TGVETO/2.D0)**2                                    
         ACTEM = ABS(COS(TEMIN))                                                
         ACTK  = ABS(COS(TGMIN))                                                
C Calculate constants required for generation of the phase space vars           
C First for the soft generation.                                                
         IF(RADCOR.EQ.NONE .OR. RADCOR.EQ.SOFT)THEN                             
C #1 - QP0                                                                      
C FQPMAX = EB - QP0max                                                          
C Note that this can be a small number compared to EB, so care must be          
C taken.                                                                        
C Also, a check is made to see if the config is kinematically allowed           
            IF(CONFIG.EQ.EGAMMA)THEN                                            
               FQPMAX = FQPEG(TEMIN,TGMIN,EEMIN,EGMIN,TEVETO,EB)                
            ELSE IF(CONFIG.EQ.GAMMA)THEN                                        
               FQPMAX = EB*( EGMIN*SIN((TGMIN-TEVETO)/2.D0)**2 /                
     >                       (EB-EGMIN*COS((TGMIN-TEVETO)/2.D0)**2) )           
               SINTST = EGMIN*SIN(TGMIN-TEVETO)/(EB+FQPMAX-EGMIN)               
               IF(SINTST.GT.SIN(2.D0*TEVETO))FQPMAX=-1.                         
            ELSE IF(CONFIG.EQ.ETRON)THEN                                        
               FQPMAX = SQRTS*EEMIN*SIN((TEMIN-TEVETO)/2.D0)**2 /               
     >                  (SQRTS - EEMIN*(1.D0+COS(TEMIN-TEVETO)))                
               SINTST = EEMIN*SIN(TEMIN-TEVETO)/(EB+FQPMAX-EEMIN)               
               IF(SINTST.GT.SIN(TGVETO+TEVETO))FQPMAX=-1.                       
            ENDIF                                                               
C Here we check if the choice is kinematically impossible.                      
            IF(FQPMAX.LT.0.D0)THEN                                              
               WRITE(OLUN,*)'Sorry, your choice of parameters is',              
     >                      ' kinematically impossible!'                        
               TEEGGL=.FALSE.                                                   
            ELSE                                                                
               QP0MAX=EB-FQPMAX                                                 
               QP0MIN=10.*M                                                     
               ZMAX=QP0MAX/FQPMAX                                               
               LOGZ0M=LOG(1.D0 + CDELT1/EPSLON*ZMAX**2)                         
C #2 - COS(thetaQP)                                                             
C #3 - COS(thetaK)                                                              
               IF(CONFIG.EQ.EGAMMA .OR. CONFIG.EQ.ETRON)THEN                    
C Check if kinematics give a stronger limit on theta-gamma than given           
                  SINT1=EEMIN * SIN(TEMIN+TEVETO) *                             
     >                  ( EB - EEMIN*SIN((TEMIN+TEVETO)/2.D0)**2 )              
     >              /   ( EB**2 - EEMIN*(SQRTS-EEMIN) *                         
     >                          SIN((TEMIN+TEVETO)/2.D0)**2 )                   
                  SINT2=EEMIN * SIN(TEMIN-TEVETO) *                             
     >                  ( EB - EEMIN*COS((TEMIN-TEVETO)/2.D0)**2 )              
     >              /   ( EB**2 - EEMIN*(SQRTS-EEMIN) *                         
     >                          COS((TEMIN-TEVETO)/2.D0)**2 )                   
                  TKMIN=ASIN(MIN(SINT1,SINT2))-TEVETO                           
                  IF(CONFIG.EQ.ETRON )TKMIN=MAX(0.D0,TKMIN)                     
                  IF(CONFIG.EQ.EGAMMA)TKMIN=MAX(TGMIN,TKMIN)                    
                  CTGM1M = 2.D0 * SIN(TKMIN/2.D0)**2 + EPSLON                   
                  IF(CONFIG.EQ.ETRON)                                           
     >               FACT3=(2.D0*SIN(TGVETO/2.D0)**2 + EPSLON)/CTGM1M           
                  IF(CONFIG.EQ.EGAMMA)                                          
     >               FACT3=(1.D0+COS(TKMIN) + EPSLON)/CTGM1M                    
               ELSE                                                             
                  CTGM1M = 2.D0 * SIN(TGMIN/2.D0)**2 + EPSLON                   
                  FACT3=(1.D0+COS(TGMIN)+EPSLON)/CTGM1M                         
               ENDIF                                                            
C #4 - PHI K                                                                    
C #5 - PHI QP                                                                   
            ENDIF                                                               
C Now for the hard generation                                                   
         ELSE IF(RADCOR.EQ.HARD)THEN                                            
C #1 - QP0                                                                      
C FQPMAX = EB - QP0max                                                          
            IF(CONFIG.EQ.EGAMMA)THEN                                            
               FQPMAX = MIN( ( EB*EGMIN*SIN((TGMIN+TEVETO)/2.D0)**2             
     >                        +EB*EEMIN*SIN((TEMIN-TEVETO)/2.D0)**2             
     >                        -EGMIN*EEMIN*SIN((TGMIN+TEMIN)/2.D0)**2 )         
     >                      /( EB - EGMIN*COS(TGMIN+TEVETO/2.D0)**2             
     >                            - EEMIN*COS(TEMIN-TEVETO/2.D0)**2 )  ,        
     >                       ( EB*EEMIN*SIN((TEMIN+TEVETO)/2.D0)**2             
     >                        +EB*EGMIN*SIN((TGMIN-TEVETO)/2.D0)**2             
     >                        -EEMIN*EGMIN*SIN((TEMIN+TGMIN)/2.D0)**2 )         
     >                      /( EB - EEMIN*COS(TEMIN+TEVETO/2.D0)**2             
     >                            - EGMIN*COS(TGMIN-TEVETO/2.D0)**2 )  )        
            ELSE IF(CONFIG.EQ.GAMMA .OR. CONFIG.EQ.GAMMAE)THEN                  
               FQPMAX = EB*( EGMIN*SIN((TGMIN-TEVETO)/2.D0)**2 /                
     >                       (EB-EGMIN*COS((TGMIN-TEVETO)/2.D0)**2) )           
               SINTST = EGMIN*SIN(TGMIN-TEVETO)/(EB+FQPMAX-EGMIN)               
               IF(SINTST.GT.SIN(2.D0*TEVETO))FQPMAX=-1.                         
            ELSE IF(CONFIG.EQ.ETRON)THEN                                        
               FQPMAX = SQRTS*EEMIN*SIN((TEMIN-TEVETO)/2.D0)**2 /               
     >                  (SQRTS - EEMIN*(1.D0+COS(TEMIN-TEVETO)))                
               SINTST = EEMIN*SIN(TEMIN-TEVETO)/(EB+FQPMAX-EEMIN)               
               IF(SINTST.GT.SIN(TGVETO+TEVETO))FQPMAX=-1.                       
            ENDIF                                                               
C Here we check if the choice is kinematically impossible.                      
            IF(FQPMAX.LT.0.D0)THEN                                              
               WRITE(OLUN,*)'Sorry, your choice of parameters is',              
     >                      ' kinematically impossible!'                        
               TEEGGL=.FALSE.                                                   
            ELSE                                                                
               QP0MAX=EB-FQPMAX                                                 
               QP0MIN=10.*M                                                     
               ZMAX=QP0MAX/FQPMAX                                               
               PBCMIN= SQRT( (S+M**2)*FQPMAX/EB - 2.D0*M**2)/2.D0               
               LOGZ0M=LOG(1.D0 + CDELT1/EPSLON*ZMAX**2)                         
               IF(CUTOFF.GE.PBCMIN*(1.D0+EPS))THEN                              
                  WRITE(OLUN,*)'Your choice of CUTOFF is too large.'            
                  TEEGGL=.FALSE.                                                
               ELSE                                                             
                  LOGRSM=LOG( (PBCMIN*(1.D0+EPS) - CUTOFF)/(EPS*CUTOFF))        
               ENDIF                                                            
C #2 - COS(thetaQP)                                                             
C #3 - COS(thetaK)                                                              
               IF(CONFIG.EQ.ETRON .OR. CONFIG.EQ.GAMMAE)THEN                    
                  CTGM1M = EPSLON                                               
                  FACT3=(2.D0*SIN(TGVETO/2.D0)**2 + EPSLON)/CTGM1M              
               ELSE                                                             
                  CTGM1M = 2.D0 * SIN(TGMIN/2.D0)**2 + EPSLON                   
                  FACT3=(1.D0+COS(TGMIN)+EPSLON)/CTGM1M                         
               ENDIF                                                            
C #4 - PHI K                                                                    
C #5 - PHI QP                                                                   
C #6 - KS                                                                       
C #7 - Theta KS                                                                 
               FACT7=(1.D0+2.D0/EPSLON)                                         
C #8 - Phi KS                                                                   
C Additional initialization is required for the Martinez/Miquel ME.             
               IF(MTRXGG.EQ.MEEGG)CALL INMART                                   
            ENDIF                                                               
         ENDIF                                                                  
C Now finished with the constant calculations. Check few more parameters        
         IF(TEEGGL)THEN                                                         
C Let the user know if he's working inefficiently                               
            IF(CDELT1 .LT. 10.D0 * 2.D0*M**2/QP0MIN/SQRTS)                      
     >         WRITE(OLUN,*)'Warning: The choice of TEVETO is so '              
     >,        'small as to cause this program to be inefficient'               
            IF(FQPMAX .LT. 10.D0 * 2.D0*M**2/SQRTS)                             
     >         WRITE(OLUN,*)'Warning: TEMIN or TGMIN '                          
     >,       'so close to TEVETO causes this program to be inefficient'        
C Here a check on the minimum energy of the missing e or gamma is made.         
           IF(CONFIG.EQ.GAMMA .OR. CONFIG.EQ.GAMMAE)THEN                        
               MINQP = SQRTS/(1.D0+ (SIN(2.D0*TEVETO)+SIN(TGMIN-TEVETO))        
     >                 /SIN(TGMIN+TEVETO))                                      
               MINEE = (SQRTS-MINQP)*SIN(TGMIN-TEVETO) /                        
     >                 ( SIN(2.D0*TEVETO) + SIN(TGMIN-TEVETO) )                 
               IF(MINEE.LT. 1.D-3 * EB)                                         
     >            WRITE(OLUN,*)'SEVERE Warning: TGMIN'                          
     >,           ' may be too close to TEVETO to trust the results!!!'         
            ELSE IF(CONFIG.EQ.ETRON)THEN                                        
               MINQP =SQRTS/(1.D0+(SIN(TGVETO+TEVETO)+SIN(TEMIN-TEVETO))        
     >                 /SIN(TEMIN+TGVETO))                                      
               MINEG = (SQRTS-MINQP)*SIN(TEMIN-TEVETO) /                        
     >                 ( SIN(TGVETO+TEVETO) + SIN(TEMIN-TEVETO) )               
               IF(MINEG.LT. 1.D-3 * EB)                                         
     >            WRITE(OLUN,*)'SEVERE Warning: TEMIN'                          
     >,           ' may be too close to TEVETO to trust the results!!!'         
            ENDIF                                                               
C Here check that CUTOFF is much less than the minimum total CM energy          
            IF(RADCOR.EQ.SOFT.OR.RADCOR.EQ.HARD)THEN                            
               ECMMIN=2.D0*SQRT(EB*FQPMAX)                                      
               IF(CUTOFF.GT.ECMMIN/2.)THEN                                      
                  WRITE(OLUN,*)'SEVERE Warning: CUTOFF is much '                
     >,           'too large for the chosen set of parameters.'                 
                  WRITE(OLUN,*)'Do not trust the results.'                      
                  WRITE(OLUN,800)ECMMIN/10.D0                                   
               ELSE IF(CUTOFF.GT.ECMMIN/10.)THEN                                
                  WRITE(OLUN,*)'Warning: CUTOFF may be too large'               
     >,           ' for the chosen set of parameters.'                          
                  WRITE(OLUN,*)'The results are not guaranteed.'                
                  WRITE(OLUN,800)ECMMIN/10.D0                                   
               ENDIF                                                            
 800           FORMAT(' Suggest choosing CUTOFF= ',E8.2,' GeV or less'/)        
C Calculate the approximate soft correction here.(to stabilize wght)            
C 3.5 seems to work better than 4                                               
               ASOFTC = 1.D0 + 3.5D0 * ALPHA/PI * LOG(ECMMIN/M)                 
     >                                          * LOG(CUTOFF/ECMMIN)            
            ENDIF                                                               
         ENDIF                                                                  
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C SUBROUTINE TEEGG7: Calls an event generator to generate one event.            
C-----------------------------------------------------------------------        
      SUBROUTINE TEEGG7                                                         
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
      IF(RADCOR.EQ.NONE .OR. RADCOR.EQ.SOFT)THEN                                
         CALL T3BODY               
      ELSE IF(RADCOR.EQ.HARD)THEN
         CALL T4BODY            
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C SUBROUTINE TEEGGC: Calculates the efficiency and total cross section.         
C-----------------------------------------------------------------------        
      SUBROUTINE TEEGGC                                                         
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
      REAL*8 CONST                                                              
      EFFIC =REAL(NACC)/DBLE(NTRIAL)                                            
      IF(RADCOR.EQ.NONE .OR. RADCOR.EQ.SOFT)THEN                                
         CONST = 2.D0*ALPHA3*2.D0/S*LOGZ0M*ZMAX*SUMW1/NTRIAL                    
     >           * LOG(FACT3) * ASOFTC                                          
C Factor of 2 is from e+ and e- symmetrization:                                 
         CONVER= CONST/NPASSQ * PBARN *2.D0                                     
      ELSE IF(RADCOR.EQ.HARD)THEN                                               
         CONST = ALPHA4/PI*2.D0/S*LOGZ0M*LOGRSM*ZMAX*SUMW1/NTRIAL               
     >           * LOG(FACT3) * 2.D0*LOG(FACT7)                                 
C Factor of 2 is from k and ks symmetrization:                                  
         CONVER= CONST/NPASSQ * PBARN *2.D0                                     
      ENDIF                                                                     
      SIGE  = CONVER*SUMWGT                                                     
      ERSIGE= SIGE*SQRT( SUMW2/SUMWGT**2    + SUMW12/SUMW1**2                   
     >                  - 1.D0/DBLE(NPASSQ) -  1.D0 /DBLE(NTRIAL) )
* jvw
      SIGEO = CONVER*SUMWGTO             
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C SUBROUTINE TEEGGP: Prints out a summary of input params, total x-sect.        
C-----------------------------------------------------------------------        
      SUBROUTINE TEEGGP(OLUN)                                                   
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
      INTEGER*4 OLUN                                                            
      LOGICAL      LWWARN                                                       
      CHARACTER*4  DESCOR                                                       
      CHARACTER*7  DESCON                                                       
      CHARACTER*13 WARN,WARN1                                                   
      CHARACTER*30 DESCME                                                       
      IF(CONFIG.EQ.EGAMMA)THEN                                                  
         DESCON='e-gamma'                                                       
      ELSE IF(CONFIG.EQ.GAMMA)THEN                                              
         DESCON='gamma  '                                                       
      ELSE IF(CONFIG.EQ.ETRON)THEN                                              
         DESCON='etron  '                                                       
      ELSE IF(CONFIG.EQ.GAMMAE)THEN                                             
         DESCON='gamma e'                                                       
      ENDIF                                                                     
      IF(RADCOR.NE.HARD)THEN                                                    
         IF(MATRIX.EQ.BK)THEN                                                   
            DESCME='Berends & Kleiss             '                              
         ELSE IF(MATRIX.EQ.BKM2)THEN                                            
            DESCME='Berends & Kleiss with m term '                              
         ELSE IF(MATRIX.EQ.TCHAN)THEN                                           
            DESCME='t channel only (inc. m term) '                              
         ELSE IF(MATRIX.EQ.EPA)THEN                                             
            DESCME='Equivalent photon approx.    '                              
         ENDIF                                                                  
      ELSE                                                                      
         IF(MTRXGG.EQ.EPADC)THEN                                                
            DESCME='EPA with double compton      '                              
         ELSE IF(MTRXGG.EQ.BEEGG)THEN                                           
            DESCME='Berends et al e-e-gamma-gamma'                              
         ELSE IF(MTRXGG.EQ.MEEGG)THEN                                           
            DESCME='Martinez/Miquel e-e-g-g      '                              
         ELSE IF(MTRXGG.EQ.HEEGG)THEN                                           
            DESCME='EPA/Berends et al hybrid eegg'                              
         ENDIF                                                                  
      ENDIF                                                                     
      IF(RADCOR.EQ.NONE)THEN                                                    
         DESCOR='none'                                                          
      ELSE IF(RADCOR.EQ.SOFT)THEN                                               
         DESCOR='soft'                                                          
      ELSE IF(RADCOR.EQ.HARD)THEN                                               
         DESCOR='hard'                                                          
      ENDIF                                                                     
      LWWARN=.FALSE.                                                            
      IF(W1MAX.GT.WGHT1M)THEN                                                   
         WARN1='** Warning **'                                                  
         LWWARN=.TRUE.                                                          
      ELSE                                                                      
         WARN1='             '                                                  
      ENDIF                                                                     
      IF(WMAX.GT.WGHTMX.AND.UNWGHT)THEN                                         
         WARN='** Warning ** overweighted xsec fraction' 
         LWWARN=.TRUE.                                                          
      ELSE                                                                      
         WARN='                                        '                                                   
      ENDIF                                                                     
      IF(RADCOR.EQ.SOFT .AND. WMINSF.LT.0.D0)THEN                               
         WRITE(OLUN,120)                                                        
 120     FORMAT(' ** Warning ** The choice of CUTOFF is too small,'             
     >,         ' causing the soft correction',/,' weight to be < 0.',/         
     >,         ' Increase CUTOFF, and try again.',/)                           
      ENDIF                                                                     
      IF(LWWARN)THEN                                                            
         WRITE(OLUN,121)                                                        
 121     FORMAT(' ** Warning ** The choice of a maximum weight is too'          
     >,         ' small (see below).',/,' Increase as necessary.',/)            
      ENDIF                                                                     
      IF(( (RADCOR.EQ.HARD.AND.MTRXGG.EQ.EPADC) .OR. RADCOR.EQ.SOFT .OR.        
     >     (RADCOR.EQ.NONE.AND.MATRIX.EQ.EPA)) .AND. Q2W2MX.GE..1D0)THEN        
         WRITE(OLUN,122)Q2W2MX                                                  
 122     FORMAT(' ** Warning ** The equivalent photon approximation'            
     >,         ' may be invalid for the event',/,' sample generated:',/        
     >,         ' Q**2/W**2 << 1 is not always true. max(Q**2/W**2)='           
     >,         F7.3,/)                                                         
      ENDIF                                                                     
*jvw      WRITE(OLUN,100)DESCOR,DESCME,DESCON,BSEED,NXSEED,EB                       
      WRITE(OLUN,100)DESCOR,DESCME,DESCON,EB
      IF(RADCOR.NE.NONE)WRITE(OLUN,101)CUTOFF                                   
      WRITE(OLUN,102)TEVETO                                                     
      IF(CONFIG.EQ.EGAMMA)THEN                                                  
         WRITE(OLUN,103)TEMIN,TGMIN,EEMIN,EGMIN                                 
      ELSE IF(CONFIG.EQ.GAMMA.AND.RADCOR.NE.HARD)THEN                           
         WRITE(OLUN,104)TGMIN,EGMIN                                             
      ELSE IF(CONFIG.EQ.GAMMA.AND.RADCOR.EQ.HARD)THEN                           
         WRITE(OLUN,105)TGMIN,TGVETO,EGMIN                                      
      ELSE IF(CONFIG.EQ.GAMMAE.AND.RADCOR.EQ.HARD)THEN                          
         WRITE(OLUN,105)TGMIN,TGVETO,EGMIN                                      
      ELSE IF(CONFIG.EQ.ETRON)THEN                                              
         WRITE(OLUN,106)TEMIN,TGVETO,EEMIN                                      
      ENDIF                                                                     
      IF(RADCOR.EQ.HARD)THEN                                                    
         IF(CONFIG.EQ.EGAMMA)WRITE(OLUN,107)PEGMIN                              
         IF(CONFIG.EQ.EGAMMA)WRITE(OLUN,117)CEGMAX
         IF(CONFIG.NE.EGAMMA)WRITE(OLUN,108)PHVETO                              
         IF(CONFIG.EQ.GAMMA .AND.EEVETO.NE.0.D0)WRITE(OLUN,109)EEVETO           
         IF(CONFIG.EQ.GAMMAE.AND.EEVETO.NE.0.D0)WRITE(OLUN,109)EEVETO           
         IF(CONFIG.NE.EGAMMA.AND.EGVETO.NE.0.D0)WRITE(OLUN,110)EGVETO           
         IF(EPS.NE.0.01D0)WRITE(OLUN,111)EPS                                    
      ENDIF                                                                     
      WRITE(OLUN,112)WGHT1M,W1MAX,WARN1                                         
      IF(UNWGHT)THEN                                                            
         WRITE(OLUN,113)WGHTMX,WMAX,WARN          
         WRITE(OLUN,114)NACC,NTRIAL,EFFIC,SIGE,ERSIGE,SIGEO/SIGE*100
      ELSE                                                                      
         WRITE(OLUN,115)WMAX                                                    
         WRITE(OLUN,116)NACC,NTRIAL,EFFIC,SIGE,ERSIGE                           
      ENDIF                                                                     
 100  FORMAT(' '                                                                
     >,/,    ' TEEGG (7.1) - an e e gamma (gamma) event generator.'             
     >,                                   ' Rad. correction: ',A4               
     >,/,    ' Matrix element: ',A30,'      Configuration: ',A7                 
c     >,/,/,  ' Initial seed =',I12,' next seed =',I12                           
     >,/,    ' Parameter: EB     = ',F7.3,' GeV')                               
 101  FORMAT(' Parameter: CUTOFF = ',F7.5,' GeV')                               
 102  FORMAT(' Parameter: TEVETO = ',F7.5,' rad')                               
 103  FORMAT(' Parameter: TEMIN  = ',F7.5,' rad'                                
     >,/,    ' Parameter: TGMIN  = ',F7.5,' rad'                                
     >,/,    ' Parameter: EEMIN  = ',F7.3,' GeV'                                
     >,/,    ' Parameter: EGMIN  = ',F7.3,' GeV')                               
 104  FORMAT(' Parameter: TGMIN  = ',F7.5,' rad'                                
     >,/,    ' Parameter: EGMIN  = ',F7.3,' GeV')                               
 105  FORMAT(' Parameter: TGMIN  = ',F7.5,' rad'                                
     >,/,    ' Parameter: TGVETO = ',F7.5,' rad'                                
     >,/,    ' Parameter: EGMIN  = ',F7.3,' GeV')                               
 106  FORMAT(' Parameter: TEMIN  = ',F7.5,' rad'                                
     >,/,    ' Parameter: TGVETO = ',F7.5,' rad'                                
     >,/,    ' Parameter: EEMIN  = ',F7.3,' GeV')                               
 107  FORMAT(' Parameter: PEGMIN = ',F7.5,' rad')
 117  FORMAT(' Parameter: CEGMAX = ',F7.5,'    ')
 108  FORMAT(' Parameter: PHVETO = ',F7.5,' rad')                               
 109  FORMAT(' Parameter: EEVETO = ',F7.4,' GeV')                               
 110  FORMAT(' Parameter: EGVETO = ',F7.4,' GeV')                               
 111  FORMAT(' Parameter: EPS    = ',F14.12,'    ')                               
 112  FORMAT(' Parameter: WGHT1M = ',F7.3,' ; Observed maximum ',F10.3           
     >,                                   ' ',A13)                              
 113  FORMAT(' Parameter: WGHTMX = ',F7.3,' ; Observed maximum ',F10.3           
     >,                                   ' ',A13)                              
 114  FORMAT(' No. of events generated=',I7,','                                 
     >,      ' No. of attempts=',I8,','                                         
     >,      ' Efficiency=',F8.6                                                
     >,/,    ' Total cross section =',E12.6,' +/- ',E12.6,' pb',/
     >,/,    ' Overweighted xsec fraction =',F10.5,'%')            
 115  FORMAT(' Weighted events generated   ; Observed maximum ',F10.3)           
 116  FORMAT(' No. of weighted events =',I7,','                                 
     >,      ' No. of attempts=',I8,','                                         
     >,      ' Efficiency=',F8.6                                                
     >,/,    ' Total cross section =',E12.6,' +/- ',E12.6,' pb',/,/)            
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C SUBROUTINE TEEGGA: Calculate cross section for user defined Acceptance        
C-----------------------------------------------------------------------        
      SUBROUTINE TEEGGA(SUMWT,SUMWT2,XSCT,ERXSCT)                               
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 SUMWT,SUMWT2,XSCT,ERXSCT                                           
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
C If CONVER  has not been calculated, accumulate statistics                     
      IF(CONVER.EQ.0.D0)THEN                                                    
         IF(UNWGHT)THEN                                                         
            SUMWT  = SUMWT + 1.D0                                               
            SUMWT2 = 0.D0                                                       
         ELSE                                                                   
            SUMWT  = SUMWT  + WGHT                                              
            SUMWT2 = SUMWT2 + WGHT**2                                           
         ENDIF                                                                  
C If CONVER has been calculated, calculate the cross section, provided          
C There were some events in the acceptance                                      
      ELSE IF(SUMWT.GT.0.D0)THEN                                                
         IF(UNWGHT)THEN                                                         
            XSCT = SUMWT/DBLE(NACC) * SIGE                                      
            ERXSCT = XSCT * SQRT(1.D0/SUMWT - 1.D0/DBLE(NTRIAL))                
         ELSE                                                                   
            XSCT  = CONVER*SUMWT                                                
            ERXSCT= XSCT*SQRT(SUMWT2/SUMWT**2     + SUMW12/SUMW1**2             
     >                        - 1.D0/DBLE(NPASSQ) -  1.D0 /DBLE(NTRIAL))        
         ENDIF                                                                  
C If no events were in the acceptance, xsct=0                                   
      ELSE                                                                      
         XSCT = 0.D0                                                            
         ERXSCT = 0.D0                                                          
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C SUBROUTINE T3BODY: Generates an e-e-gamma event                               
C-----------------------------------------------------------------------        
      SUBROUTINE T3BODY                                                         
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED
* jvw
      REAL RNDM,dummy
*                                  
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
C Variables                                                                     
      REAL*8 Z,Z0,FQPP,FQP,QP0,QPP,WGHT1                                        
     >,      COSQP1,COSK1M,COSK1,PHIK,PHIP,W2,SINQP,SINK,COSKP1                 
     >,      K0,QM0,QMP,TA,COSQM1,F1,ABK,WBK,WM,TM,TPM,TRIX,D5L                 
     >,      FQM,COSKM1,DSIGA                                                   
     >,      SC,PBC,EBC,BETA1,BETA,GAMA,KCK,ATERM                               
     >,      KXPKC,KYPKC,KZPKC                                                  
     >,      XITERM,SINTR,COSTR,KXCKC,KYCKC,KZCKC,SINKC2,BETAM1,COSKC1          
     >,      EMORK,TMORK,YMORK,UMORK,DEMORK,DELTAS                              
      INTEGER*4 IMISS,IOBSV                                                     
C Externals                                                                     
      REAL*8   DMORK                                                            
      EXTERNAL DMORK                                                            
C The starting point for a new trial                                            
C ----------------------------------                                            
 1    CONTINUE                                                                  
C #1 Generate QP0                                                               
C Do until trial QP0 COSQP1 is taken                                            
C Increment the trial event counter here.                                       
         NTRIAL=NTRIAL+1                                                        
C Generate all random numbers needed here.                                      
*jvw         CALL RNDGEN(8)                                                         
*jvw         Z=RND(1)*ZMAX                                                          
          Z=DBLE(RNDM(dummy))*ZMAX
C FQPP=EB-QP ; FQP=EB-QP0                                                       
         FQPP=EB/(1.D0+Z)                                                       
         FQP =FQPP - EPSLON*EB*(1.D0+Z)/Z                                       
         QP0=EB-FQP                                                             
         QPP=SQRT(QP0**2-M**2)                                                  
         Z0 = QP0/FQP                                                           
C Require QP0 > QP0min                                                          
         IF(QP0.LT.QP0MIN)GOTO 1                                                
C Assign a weight for this trial QP0                                            
         WGHT1=(FQPP/FQP)**2 * LOG(  Z0**2*CDELT1/EPSLON+1.D0)/                 
     >                         LOGZ0M                                           
C Keep track of all weights, to calculate the total cross section.              
      CALL HFILL(9003,real(WGHT1),0.,1.)                                              
         SUMW1 = SUMW1 + WGHT1                                                  
         SUMW12= SUMW12+ WGHT1**2                                               
      IF(WGHT1.LT.DBLE(RNDM(dummy))*WGHT1M)GOTO 1                                          
C A q+0 has been accepted. Keep track of number that pass qp generation.        
      NPASSQ = NPASSQ + 1                                                       
C #2 Generate COS(theta QP0)                                                    
C COSQP1 = 1 - COS(theta QP0)                                                   
      COSQP1 = EPSLON/Z0**2*
     &          ( (Z0**2/EPSLON*CDELT1+1.D0)**DBLE(RNDM(dummy)) -1.D0)         
C #3 COS(theta K)                                                               
C COSK1M = 1 + COS(theta K) + epsilon                                           
      COSK1M=CTGM1M*FACT3**DBLE(RNDM(dummy))                                               
      COSK1 = COSK1M - EPSLON                                                   
C #4 phi K  note that Kx=Ksin(theta_k)cos(phi_k + phi_qp) etc.                  
      PHIK=TWOPI*DBLE(RNDM(dummy))                                    
C #5 phi QP                                                                     
      PHIP=TWOPI*DBLE(RNDM(dummy))                                              
C Derive the rest of the quantities that determine the exact x-section          
C W2 = Invariant mass **2                                                       
      W2 = 2.D0*SQRTS*FQP + M**2                                                
      SINQP = SQRT(COSQP1*(2.D0 - COSQP1))                                      
      SINK  = SQRT(COSK1*(2.D0 - COSK1))                                        
C COSKP1 = 1 + cos(theta gamma-e+)                                              
      COSKP1= (COSK1+COSQP1) - COSK1*COSQP1 + SINK*SINQP*COS(PHIK)              
      Y2 = (W2 - M**2)/2.D0                                                     
      K0 = SQRTS*FQP / (2.D0*FQP + QPP*COSKP1 + (QP0-QPP))                      
      QM0 = SQRTS - K0 - QP0                                                    
      QMP = SQRT(QM0**2-M**2)                                                   
C Carefully calculate t.If QP0 is large enough, expand in powers of m**2        
      IF(QP0.GE.100.D0*M)THEN                                                   
         T   = -2.D0*EBP*QPP*COSQP1 -M**2*FQP**2/EB/QP0                         
      ELSE                                                                      
         T   = -2.D0*EBP*QPP*COSQP1 + 2.D0*M**2 - 2.D0*(EB*QP0-EBP*QPP)         
      ENDIF                                                                     
C Also calculate the approximate t, to be used to evaluate DSIGA                
      TA  = -2.D0*EB*QP0*(COSQP1 + EPSLON/Z0**2)                                
C COSQM1 = 1 + cos(theta e-)                                                    
      COSQM1 = (M**2 + W2 - T - 2.D0*EBP*K0*COSK1 -2.D0*K0*(EB-EBP)             
     >          -2.D0*(EB*QM0-EBP*QMP))/(2.D0*EBP*QMP)                          
C Require that the electron is in the right place                               
C and that 'seen' particles have enough energy                                  
      IF(CONFIG.EQ.EGAMMA)THEN                                                  
         IF(ABS(COSQM1-1.D0).GT.ACTEM)GOTO 1                                    
         IF(QM0.LT.EEMIN .OR. K0.LT.EGMIN)GOTO 1                                
      ELSE IF(CONFIG.EQ.ETRON)THEN                                              
         IF(ABS(COSQM1-1.D0).GT.ACTEM)GOTO 1                                    
         IF(QM0.LT.EEMIN)GOTO 1                                                 
      ELSE IF(CONFIG.EQ.GAMMA)THEN                                              
         IF(K0.LT.EGMIN)GOTO 1                                                  
         IF(COSQM1.GT.CDELT1)GOTO 1                                             
      ENDIF                                                                     
      TP  = 2*M**2 - 2.D0*EBP*QMP*COSQM1 - 2.D0*(EB*QM0-EBP*QMP)                
      SP  = S - 2.D0 * SQRTS * K0                                               
      U   = 2.D0 * M**2 - 2.D0*(EB*QM0+EBP*QMP) + 2.D0*EBP*QMP*COSQM1           
      UP  = 2.D0 * M**2 - 2.D0*(EB*QP0+EBP*QPP) + 2.D0*EBP*QPP*COSQP1           
      X1  = (EB+EBP)*K0 - EBP*K0*COSK1                                          
      X2  = (EB-EBP)*K0 + EBP*K0*COSK1                                          
      Y1  = (QP0+QPP)*K0 - QPP*K0*COSKP1                                        
C Calculate the exact cross section.                                            
      F1 = ALPHA3/PI**2 /S                                                      
C Calculate some M terms                                                        
      WM = M**2*(S-SP)/(S**2+SP**2)*(SP/X1+SP/X2+S/Y1+S/Y2)                     
      TM = -8.D0*M**2/T**2*(X2/Y2+Y2/X2)                                        
      TPM= -8.D0*M**2/TP**2*(X1/Y1+Y1/X1)                                       
C Decide which matrix element to use                                            
C This is the Berends Kleiss terms only                                         
      IF(MATRIX .EQ. BK )THEN                                                   
         ABK = (  S*SP*(S**2+SP**2) + T*TP*(T**2+TP**2)                         
     >          + U*UP*(U**2+UP**2)   ) / (S*SP*T*TP)                           
         WBK =  S/X1/X2 + SP/Y1/Y2 - T/X1/Y1 - TP/X2/Y2                         
     >        + U/X1/Y2 + UP/X2/Y1                                              
         TRIX= ABK * WBK * (1.D0-WM)                                            
C This is the Berends Kleiss with me**2 term                                    
      ELSE IF(MATRIX .EQ. BKM2)THEN                                             
         TRIX = (  S*SP*(S**2+SP**2) + T*TP*(T**2+TP**2)                        
     >           + U*UP*(U**2+UP**2)   ) / (S*SP*T*TP)                          
     >         *(S/X1/X2+SP/Y1/Y2-T/X1/Y1-TP/X2/Y2+U/X1/Y2+UP/X2/Y1)            
     >         *(1.D0-WM)                                                       
     >         + TM + TPM                                                       
C This is the t-channel term with me**2 term. This can be used to judge         
C the size of interference terms.                                               
      ELSE IF(MATRIX .EQ. TCHAN)THEN                                            
         TRIX=                                                                  
     >     -(S**2+SP**2+U**2+UP**2)/T/X2/Y2 + TM                                
C This is the EPA matrix element (for testing only)                             
      ELSE IF(MATRIX .EQ. EPA)THEN                                              
         TRIX= -4.D0*(X2**2 + Y2**2)                                            
     >              *((S**2 + (S-W2)**2)/W2**2 + 2.D0*M**2/T)                   
     >              /T/X2/Y2                                                    
      ELSE                                                                      
         TRIX= 1.D9                                                             
      ENDIF                                                                     
      D5L = QP0 * K0 * K0 / Y2 / 8.D0                                           
      DSIGE = F1 * TRIX * D5L                                                   
C Calculate approximate cross section.                                          
C Here the approximate sigma is symmetrized.                                    
      FQM=EB-QM0                                                                
      COSKM1=Y2/QM0/K0                                                          
      DSIGA =-2.D0*ALPHA3*S/PI**2 *                                             
     >        (  1.D0/X2/Y2/TA*(SQRTS-QP0*(2.D0-COSKP1))/FQP                    
     >         + 1.D0/X1/Y1/TP*(SQRTS-QM0*(2.D0-COSKM1))/FQM )                  
     >        * D5L * ASOFTC                                                    
C If soft corrections are requested, modify the weight.                         
      WGHTSF=1.D0                                                               
      IF(RADCOR.EQ.SOFT)THEN                                                    
         SC = W2                                                                
         PBC= SQRT(W2-2.D0*M**2)/2.D0                                           
         EBC= SQRT(PBC**2+M**2)                                                 
C Boost the photon's angles to the gamma-e center of mass.                      
C (allowing for transverse momentum, as done in the 4 body generator)           
C Beta = -qp/(sqrts-qp0)                                                        
         BETA1 = (2.D0*FQP + .5D0*M**2/QP0)/(EB + FQP)                          
         BETA  = 1.D0-BETA1                                                     
         GAMA  = (SQRTS - QP0)/SQRT(SC)                                         
C KXPKC = kx'/k' etc.  KCK = k'/k = k''/k                                       
         KCK   = GAMA*(BETA*SINK*COS(PHIK)*SINQP                                
     >                  + BETA1*(1.D0 - COSK1 - COSQP1 + COSK1*COSQP1)          
     >                  + COSK1 + COSQP1 - COSK1*COSQP1)                        
         ATERM = SINK*COS(PHIK)*SINQP - COSK1*COSQP1 +COSK1+COSQP1-BETA1        
         KXPKC =(SINK*COS(PHIK) +BETA*SINQP+(GAMA-1.D0)*ATERM*SINQP)/KCK        
         KYPKC =(SINK*SIN(PHIK))/KCK                                            
         KZPKC =(COSK1 + BETA1*COSQP1 - BETA1 - COSQP1                          
     >           + (GAMA - 1.D0)*ATERM*(1.D0-COSQP1))/KCK                       
C KXCKC= kx''/k'' etc. this is the frame where kinematics a solved.             
         XITERM = (GAMA-1.D0)*COSQP1 - GAMA*(BETA1 + BETA*EPSLON)               
         SINTR  = (1.D0+XITERM)*SINQP /                                         
     >             SQRT(2.D0*(1.D0 + XITERM)*COSQP1 + XITERM**2)                
         COSTR  = SIGN(SQRT(1.D0-SINTR**2),COSQP1-XITERM*(1.D0-COSQP1))         
         KXCKC = KXPKC*COSTR + KZPKC*SINTR                                      
         KYCKC = KYPKC                                                          
         KZCKC = KZPKC*COSTR - KXPKC*SINTR                                      
         SINKC2= KXCKC**2 + KYCKC**2                                            
C BETAM1 = 1 - beta(e-) in cm frame                                             
         BETAM1=(M**2/2.D0/PBC**2 - M**4/8.D0/PBC**4)*PBC/EBC                   
         COSKC1=SINKC2/(1.D0-KZCKC)                                             
         EMORK=SQRT(SC)/M                                                       
         TMORK=.5D0*(BETAM1 + COSKC1 -BETAM1*COSKC1)                            
         YMORK=.5D0*LOG(.5D0* EMORK**2 * (1.D0 - KZCKC))                        
         UMORK=TMORK+1.D0/TMORK                                                 
         DEMORK=CUTOFF/M                                                        
         DELTAS=DMORK(TMORK,YMORK,UMORK,EMORK,DEMORK)                           
         WGHTSF=(1.D0+DELTAS)                                                   
         WMINSF=MIN(WMINSF,WGHTSF)                                              
      ENDIF                                                                     
C The weight of this trial event is,                                            
      WGHT = DSIGE/DSIGA * WGHTSF                                               
      WMAX = MAX(WMAX,WGHT)                                                     
      W1MAX= MAX(W1MAX,WGHT1)                                                   
C Keep track of quantities for the total cross section calculation.             
      CALL HFILL(9001,real(WGHT),0.,1.)                                               
      SUMWGT = SUMWGT + WGHT
* jvw
      if(wght.gt.wghtmx) SUMWGTO = SUMWGTO + WGHT
*                                                    
      SUMW2  = SUMW2  + WGHT**2                                                 
      IF(UNWGHT.AND.WGHT.LT.DBLE(RNDM(dummy))*WGHTMX)GOTO 1                                
C An event has been accepted at this point                                      
C ----------------------------------------                                      
      NACC=NACC+1                                                               
C Calculate the 4 vectors. Decide whether to interchange or not here.           
      IF(DBLE(RNDM(dummy)).LT..5)THEN                                                      
         IMISS=0                                                                
         IOBSV=4                                                                
         RSIGN=+1.D0                                                            
      ELSE                                                                      
         IMISS=4                                                                
         IOBSV=0                                                                
         RSIGN=-1.D0                                                            
      ENDIF                                                                     
C 'missing'                                                                     
      P(IMISS+1)=QPP*SINQP*COS(PHIP)  * RSIGN                                   
      P(IMISS+2)=QPP*SINQP*SIN(PHIP)  * RSIGN                                   
      P(IMISS+3)=QPP*(1.D0-COSQP1)    * RSIGN                                   
      P(IMISS+4)=QP0                                                            
C Gamma                                                                         
      P(9) =K0*SINK*COS(PHIK+PHIP)    * RSIGN                                   
      P(10)=K0*SINK*SIN(PHIK+PHIP)    * RSIGN                                   
      P(11)=K0*(COSK1-1.D0)           * RSIGN                                   
      P(12)=K0                                                                  
C 'observed'                                                                    
      P(IOBSV+1)=-P(IMISS+1)-P(9)                                               
      P(IOBSV+2)=-P(IMISS+2)-P(10)                                              
      P(IOBSV+3)=QMP*(COSQM1-1.D0)    * RSIGN                                   
      P(IOBSV+4)=QM0                                                            
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C SUBROUTINE T4BODY: Generates an e-e-gamma-gamma event.                        
C-----------------------------------------------------------------------        
      SUBROUTINE T4BODY                                                         
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
* jvw
      REAL RNDM,dummy
* 
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
C Variables                                                                     
      REAL*8 ZP,Z0,FQPP,FQP,QP0,WGHT1,RS,W2,SQRTSC,SC,PBC,EBC                   
     >,      COSQP1,SINQP,COSK1M,COSK1,SINK,PHIK,PHIP                           
     >,      BETA1,BETA,GAMA,KCK,ATERM,KXPKC,KYPKC,KZPKC                        
     >,      XITERM,SINTR,COSTR,KXCKC,KYCKC,KZCKC,SINKC,PHIKC                   
     >,      KS,CSKS1P,SINKSP,CSKS1Q,PHIKSP,KC,QM0C,QMPC                        
     >,      CSKS1K,SINKSK,PHIKSK,KYZCKC,CPKSK,SPKSK,KSXKS,KSYKS,KSZKS          
     >,      QPP0,QM0,QMP0,K0,KSL,COSQM,COSQM1,COSKSL,CSKS1L,CSK1CQ             
     >,      DELTQ,PHIEL,PHIKL,PHIKSL                                           
     >,      KAP1,KAP2,KAP3,KAP1P,KAP2P,KAP3P,A,B,C,X,Z,UA,UB,RO,XTERM          
     >,      DSIGA,TE,BPMK1,BPMK2,BQMK1,BQMK2,BK1K2                             
     >,      S1,S2,T1,T2,U1,U2,PPK1K2,PMK1K2,QPK1K2,QMK1K2                      
     >,      K1P,K1M,K2P,K2M,QPP,QPM,QMP,QMM                                    
      REAL*8 QMC4(4),GAMC4(4),GAMSC4(4),QPL4(4),QML4(4)                         
     >,      GAML4(4),GAMSL4(4)
      INTEGER*4 IMISS,IOBSV                                                     
      LOGICAL LKSAXZ,LPASS,LSWAP                                                
      LOGICAL LAE,LAG,LAGS,LAEG,LAEGS,LRE,LRG,LRGS,LREG,LREGS,LRGGS             
      REAL*8 HQ2MIN                                                             
      PARAMETER (HQ2MIN = 1.D2 * M**2)                                          
C Externals                                                                     
      REAL*8   TPRD,BEEGGC,MEEGGC                                               
      EXTERNAL TPRD,BEEGGC,MEEGGC                                               
* jvw
      REAL*8 COSELKL,COSPLKL,COSELKSL,COSPLKSL,COSKLKSL,COSELEL
      REAL*8 PHISEP,PHI1,PHI2                                                   
      PHISEP(PHI1,PHI2)=MIN(ABS(PHI1 - PHI2) , TWOPI - ABS(PHI1 - PHI2))        
C The starting point for a new trial                                            
C ----------------------------------                                            
 1    CONTINUE                                                                  
C #1 Generate QP0                                                               
C Do until trial QP0 COSQP1 is taken                                            
C Increment the trial event counter here. The calculation of the total          
C cross section depends on this quantity.                                       
         NTRIAL=NTRIAL+1                                                        
C Generate all random numbers needed here.                                      
*jvw         CALL RNDGEN(12)                                                        
*jvw         ZP=RND(1)*ZMAX
         ZP=DBLE(RNDM(dummy))*ZMAX                                                         
C FQPP=EB-QP ; FQP=EB-QP0                                                       
         FQPP=EB/(1.D0+ZP)                                                      
         FQP =FQPP - EPSLON*EB*(1.D0+ZP)/ZP                                     
         QP0=EB-FQP                                                             
         Z0 = QP0/FQP                                                           
C Require QP0 > QP0min                                                          
         IF(QP0.LT.QP0MIN)GOTO 1                                                
C Some handy 'constants'                                                        
         W2 = 2.D0*SQRTS*FQP + M**2                                             
         PBC= SQRT(W2-2.D0*M**2)/2.D0                                           
         EBC= SQRT(PBC**2+M**2)                                                 
         SQRTSC=SQRT(W2)                                                        
         SC = W2                                                                
C Assign a weight for this trial QP0                                            
         RS=(PBC*(1.D0+EPS)-CUTOFF)/(EPS*CUTOFF)                                
         WGHT1=(FQPP/FQP)**2 * LOG(1.D0+CDELT1/EPSLON*Z0**2)*LOG(RS)/           
     >                       ( LOGZ0M * LOGRSM )                                
C Keep track of all weights, to calculate the total cross section.              
      CALL HFILL(9004,real(WGHT1),0.,1.)                                              
         SUMW1 = SUMW1 + WGHT1                                                  
         SUMW12= SUMW12+ WGHT1**2                                               
      IF(WGHT1.LT.DBLE(RNDM(dummy))*WGHT1M)GOTO 1                                          
C A q+0 has been accepted. Keep track of number that pass qp generation         
      NPASSQ = NPASSQ + 1                                                       
C #2 Generate COS(theta QP0)                                                    
C COSQP1 = 1 - COS(theta QP0)                                                   
      COSQP1 = EPSLON/Z0**2*
     &         ( (Z0**2/EPSLON*CDELT1+1.D0)**DBLE(RNDM(dummy)) -1.D0)         
      SINQP  = SQRT(COSQP1*(2.D0 - COSQP1))                                     
C #3 COS(theta K)                                                               
C COSK1M = 1 + COS(theta K) + epsilon                                           
      COSK1M=CTGM1M*FACT3**DBLE(RNDM(dummy))                  
      COSK1 = COSK1M - EPSLON                                                   
      SINK  = SQRT(COSK1*(2.D0 - COSK1))                                        
C #4 phi K  note that Kx=Ksin(theta_k)cos(phi_k + phi_qp) etc.                  
      PHIK=TWOPI*DBLE(RNDM(dummy))                                    
C #5 phi QP                                                                     
      PHIP=TWOPI*DBLE(RNDM(dummy))                                              
C Now boost the first photon's angles to the gamma-e center of mass.            
C Beta = -qp/(sqrts-qp0)                                                        
      BETA1 = (2.D0*FQP + .5D0*M**2/QP0)/(EB + FQP)                             
      BETA  = 1.D0-BETA1
      GAMA  = (SQRTS - QP0)/SQRT(SC)                                            
C KXPKC = kx'/k' etc.  KCK = k'/k = k''/k                                       
      KCK   = GAMA*(BETA*SINK*COS(PHIK)*SINQP                                   
     >               + BETA1*(1.D0 - COSK1 - COSQP1 + COSK1*COSQP1)             
     >               + COSK1 + COSQP1 - COSK1*COSQP1)                           
      ATERM = SINK*COS(PHIK)*SINQP - COSK1*COSQP1 + COSK1+COSQP1-BETA1          
      KXPKC =(SINK*COS(PHIK) + BETA*SINQP + (GAMA-1.D0)*ATERM*SINQP)/KCK        
      KYPKC =(SINK*SIN(PHIK))/KCK                                               
      KZPKC =(COSK1 + BETA1*COSQP1 - BETA1 - COSQP1                             
     >        + (GAMA - 1.D0)*ATERM*(1.D0-COSQP1))/KCK                          
C KXCKC= kx''/k'' etc. this is the frame where kinematics a solved.             
      XITERM = (GAMA-1.D0)*COSQP1 - GAMA*(BETA1 + BETA*EPSLON)                  
      SINTR  = (1.D0+XITERM)*SINQP /                                            
     >          SQRT(2.D0*(1.D0 + XITERM)*COSQP1 + XITERM**2)                   
      COSTR  = SIGN(SQRT(1.D0-SINTR**2),COSQP1-XITERM*(1.D0-COSQP1))            
      KXCKC = KXPKC*COSTR + KZPKC*SINTR                                         
      KYCKC = KYPKC                                                             
      KZCKC = KZPKC*COSTR - KXPKC*SINTR                                         
      SINKC = SQRT(KXCKC**2 + KYCKC**2)                                         
      PHIKC = ATAN2(KYCKC,KXCKC)                                                
C Now add another photon according to the approximate cross section.            
C #6 KS                                                                         
      KS = PBC * (1.D0+EPS)/(1.D0+EPS*RS**DBLE(RNDM(dummy)))                               
C #7 theta KS                                                                   
C Determine if the generating axis is the -z axis or the QM0 direction          
      IF(DBLE(RNDM(dummy)).GE..5)THEN                                                      
         LKSAXZ=.TRUE.                                                          
      ELSE                                                                      
         LKSAXZ=.FALSE.                                                         
      ENDIF                                                                     
C CSKS1 =1-COS(theta-ks) (about PM or -K direction)                             
      IF(LKSAXZ)THEN                                                            
         CSKS1P=EPSLON*FACT7**DBLE(RNDM(dummy))-EPSLON                                     
         if(csks1p.gt.2.d0) then
           write(6,*)'warning csks1p.gt.2.d0',csks1p
           csks1p=2.d0
         endif
         if(csks1p.lt.0.d0) then
           write(6,*)'warning csks1p.lt.0.d0',csks1p
           csks1p=0.d0
         endif
         SINKSP=DSQRT(CSKS1P*(2.D0 - CSKS1P))
      ELSE                                                                      
         CSKS1K=EPSLON*FACT7**DBLE(RNDM(dummy))-EPSLON
         if(csks1k.gt.2.d0) then
           write(6,*)'warning csks1k.gt.2.d0',csks1k
           csks1k=2.d0
         endif
         if(csks1k.lt.0.d0) then
           write(6,*)'warning csks1k.lt.0.d0',csks1k
           csks1k=0.d0
         endif
         SINKSK=DSQRT(CSKS1K*(2.D0 - CSKS1K))
      ENDIF                                                                     
C #8 phi KS (about PM or -K direction)                                          
      IF(LKSAXZ)THEN                                                            
         PHIKSP=TWOPI*DBLE(RNDM(dummy))                                 
      ELSE                                                                      
         PHIKSK=TWOPI*DBLE(RNDM(dummy))                                         
      ENDIF                                                                     
C Check that the configuration is even possible.                                
      IF(KS.GT.PBC)GOTO 1                                                       
C Solve the event kinematics                                                    
C --------------------------                                                    
C Solve for the 1st photon energy and for the electron. First for the           
C case where the 2nd photon is generated about the -z axis                      
      IF(LKSAXZ)THEN                                                            
         KC = ( SQRTSC/2.D0*(SQRTSC-2.D0*KS) - M**2/2.D0 ) /                    
     >        ( SQRTSC - KS*(1.0D0 + KZCKC - CSKS1P+(1.D0-KZCKC)*CSKS1P         
     >                       - SINKC*SINKSP*COS(PHIKC-PHIKSP) )  )              
         QM0C = SQRTSC - KC - KS                                                
         QMPC = SQRT(QM0C**2-M**2)                                              
         CSKS1Q=( (QMPC+KS)**2 - KC**2 ) / (2.D0*QMPC*KS)                       
         CSKS1K=( QM0C**2 - M**2 - (KC-KS)**2 ) / (2.D0*KC*KS)                  
         if(csks1k.gt.2.d0) then
           write(6,*)'warning csks1k.gt.2.d0',csks1k
           csks1k=2.d0
         endif
         if(csks1k.lt.0.d0) then
           write(6,*)'warning csks1k.lt.0.d0',csks1k
           csks1k=0.d0
         endif
         SINKSK=DSQRT(CSKS1K*(2.D0 - CSKS1K))                                   
         GAMC4(1) = KC*KXCKC                                                    
         GAMC4(2) = KC*KYCKC                                                    
         GAMC4(3) = KC*KZCKC                                                    
         GAMC4(4) = KC                                                          
         GAMSC4(1) = KS*SINKSP*COS(PHIKSP)                                      
         GAMSC4(2) = KS*SINKSP*SIN(PHIKSP)                                      
         GAMSC4(3) = KS*(CSKS1P-1.D0)                                           
         GAMSC4(4) = KS                                                         
         QMC4(1) = -GAMC4(1)-GAMSC4(1)                                          
         QMC4(2) = -GAMC4(2)-GAMSC4(2)                                          
         QMC4(3) = -GAMC4(3)-GAMSC4(3)                                          
         QMC4(4) = QM0C                                                         
      ELSE                                                                      
C Now for the case where the 2nd photon is generated about the -k axis          
C First the energies of the participants can be found from CSKS1K               
         KC = (.5D0*(SC-M**2) - SQRTSC*KS)/(SQRTSC - KS*(2.0D0-CSKS1K))         
         QM0C = SQRTSC - KC - KS                                                
         QMPC = SQRT(QM0C**2 - M**2)                                            
C KSXKS = Ks_x / Ks etc.                                                        
         KYZCKC=SQRT(KYCKC**2 + KZCKC**2)                                       
         CPKSK = COS(PHIKSK)                                                    
         SPKSK = SIN(PHIKSK)                                                    
         KSXKS =   KYZCKC*SINKSK*CPKSK         - KXCKC*(1.D0-CSKS1K)            
         KSYKS = - KXCKC*KYCKC/KYZCKC * SINKSK*CPKSK                            
     >           + KZCKC/KYZCKC * SINKSK*SPKSK - KYCKC*(1.D0-CSKS1K)            
         KSZKS = - KXCKC*KZCKC/KYZCKC * SINKSK*CPKSK                            
     >           - KYCKC/KYZCKC * SINKSK*SPKSK - KZCKC*(1.D0-CSKS1K)            
C Calculate the 4-vectors in the CM frame.                                      
         GAMC4(1) = KC*KXCKC                                                    
         GAMC4(2) = KC*KYCKC                                                    
         GAMC4(3) = KC*KZCKC                                                    
         GAMC4(4) = KC                                                          
         GAMSC4(1) = KS*KSXKS                                                   
         GAMSC4(2) = KS*KSYKS                                                   
         GAMSC4(3) = KS*KSZKS                                                   
         GAMSC4(4) = KS                                                         
         QMC4(1) = -GAMC4(1)-GAMSC4(1)                                          
         QMC4(2) = -GAMC4(2)-GAMSC4(2)                                          
         QMC4(3) = -GAMC4(3)-GAMSC4(3)                                          
         QMC4(4) = QM0C                                                         
         CSKS1P = 1.D0 + GAMSC4(3)/KS                                           
         SINKSP = SQRT(CSKS1P*(2.D0-CSKS1P))                                    
         CSKS1Q=( (QMPC+KS)**2 - KC**2 ) / (2.D0*QMPC*KS)                       
      ENDIF                                                                     
C Finished with the kinematics......                                            
C Require that the first photon be above the cutoff too.                        
      IF(KC.LT.CUTOFF)GOTO 1                                                    
C Rotate and boost back to lab system, then rotate for phip                     
      CALL TBOORT(QMC4,SINTR,COSTR,GAMA,BETA,SINQP,COSQP1,PHIP,QML4)            
      CALL TBOORT(GAMC4,SINTR,COSTR,GAMA,BETA,SINQP,COSQP1,PHIP,GAML4)          
      CALL TBOORT(GAMSC4,SINTR,COSTR,GAMA,BETA,SINQP,COSQP1,PHIP,GAMSL4)        
      QPP0    = SQRT(QP0**2-M**2)                                               
      QPL4(1) = QPP0*SINQP*COS(PHIP)                                            
      QPL4(2) = QPP0*SINQP*SIN(PHIP)                                            
      QPL4(3) = QPP0*(1.D0-COSQP1)                                              
      QPL4(4) = QP0                                                             
      QM0 = QML4(4)                                                             
      QMP0 = SQRT(QM0**2-M**2)                                                  
      K0  = GAML4(4)                                                            
      KSL = GAMSL4(4)                                                           
      COSQM=QML4(3)/QMP0                                                        
C COSQM1= 1+COS(theta q-) in lab system (can be small)                          
      IF(QML4(3).LT.0.D0)THEN                                                   
         COSQM1 = ( QML4(1)**2 + QML4(2)**2 )                                   
     >            /QMP0**2 /(1.D0-COSQM)                                        
      ELSE                                                                      
         COSQM1 = 1.D0+COSQM                                                    
      ENDIF                                                                     
      COSKSL=GAMSL4(3)/GAMSL4(4)                                                
C CSKS1L= 1+COS(theta ks) in lab system (can be small)                          
      IF(GAMSL4(3).LT.0.D0)THEN                                                 
         CSKS1L = ( GAMSL4(1)**2 + GAMSL4(2)**2 )                               
     >            /GAMSL4(4)**2 /(1.D0-COSKSL)                                  
      ELSE                                                                      
         CSKS1L = 1.D0+COSKSL                                                   
      ENDIF                                                                     
C Check if the event passes for the chosen configuration.                       
      PHIEL =ATAN2(QML4(2),QML4(1))                                             
      PHIKL =ATAN2(GAML4(2),GAML4(1))                                           
      PHIKSL=ATAN2(GAMSL4(2),GAMSL4(1))                                         
* jvw angular separation vars
      COSELKL=(QML4(1)*GAML4(1)+QML4(2)*GAML4(2)+QML4(3)*GAML4(3))/
     &         SQRT(QML4(1)**2+QML4(2)**2+QML4(3)**2)/GAML4(4)
      COSPLKL=(QPL4(1)*GAML4(1)+QPL4(2)*GAML4(2)+QPL4(3)*GAML4(3))/
     &         SQRT(QPL4(1)**2+QPL4(2)**2+QPL4(3)**2)/GAML4(4)
      COSELKSL=(QML4(1)*GAMSL4(1)+QML4(2)*GAMSL4(2)+QML4(3)*GAMSL4(3))/
     &            SQRT(QML4(1)**2+QML4(2)**2+QML4(3)**2)/GAMSL4(4)
      COSPLKSL=(QPL4(1)*GAMSL4(1)+QPL4(2)*GAMSL4(2)+QPL4(3)*GAMSL4(3))/
     &            SQRT(QPL4(1)**2+QPL4(2)**2+QPL4(3)**2)/GAMSL4(4)
      COSKLKSL=
     & (GAML4(1)*GAMSL4(1)+GAML4(2)*GAMSL4(2)+GAML4(3)*GAMSL4(3))/
     &  GAML4(4)/GAMSL4(4)
      COSELEL=(QML4(1)*QPL4(1)+QML4(2)*QPL4(2)+QML4(3)*QPL4(3))/
     &         SQRT(QML4(1)**2+QML4(2)**2+QML4(3)**2)/
     &         SQRT(QPL4(1)**2+QPL4(2)**2+QPL4(3)**2)
*
C Acceptance criteria                                                           
      LAE  = QM0.GE.EEMIN .AND. ABS(COSQM).LE.ACTEM                             
      LAG  =  K0.GE.EGMIN                                                       
      LAGS = KSL.GE.EGMIN .AND. ABS(COSKSL).LE.ACTK                             
      LAEG = PHISEP(PHIEL,PHIKL).GE.PEGMIN .AND. COSELKL.LE.CEGMAX
      LAEGS= PHISEP(PHIEL,PHIKSL).GE.PEGMIN .AND. COSELKSL.LE.CEGMAX            
C Rejection criteria                                                            
      LRE  = QM0.GE.EEVETO .AND. COSQM1.GE.CDELT1                               
      LRG  =  K0.GE.EGVETO .AND. COSK1.GE.CTGVT1                                
      LRGS = KSL.GE.EGVETO .AND.                                                
     >       MIN((1.D0+COSKSL),(1.D0-COSKSL)).GE.CTGVT1                         
      LREG = PHISEP(PHIEL,PHIKL)  .GE. PHVETO                                   
      LREGS= PHISEP(PHIEL,PHIKSL) .GE. PHVETO                                   
      LRGGS= PHISEP(PHIKL,PHIKSL) .GE. PHVETO                                   
C Now decide if event passes all the criteria: LPASS                            
C and if the (k,ks) swapped event could have been generated: LSWAP              
      LPASS=.FALSE.                                                             
      LSWAP=.FALSE.                                                             
      IF(CONFIG.EQ.EGAMMA)THEN
       if(LAE) then
* jvw if both photons satisfy fiducial and energy criteria they must also each satisfy angular sep cuts
        if((LAG.and.LAEG).and.(LAGS.and.LAEGS)) LPASS=.TRUE.
* otherwise test the relevant photon for separation cuts 
        if(.not.(LAG).and.(LAGS.and.LAEGS)) LPASS=.TRUE.
        if(.not.(LAGS).and.(LAG.and.LAEG)) LPASS=.TRUE.
       endif
*         LPASS = LAE .AND. ((LAG.AND.LAEG) .OR. (LAGS.AND.LAEGS))               
         LSWAP = ABS(COSKSL).LE.ACTK                                            
      ELSE IF(CONFIG.EQ.ETRON)THEN                                              
         LPASS = LAE.AND. .NOT.(LRG.AND.LREG).AND. .NOT.(LRGS.AND.LREGS)        
* jvw additional check for rare back-to-back topology to avoid large weights
* this mainly occurs for COSKSKSL<0
         IF(COSKLKSL.lt.-0.9) LPASS=.FALSE.
*
         LSWAP = (1.D0+COSKSL) .LT. CTGVT1
      ELSE IF(CONFIG.EQ.GAMMA)THEN                                              
         LPASS = (LAG .AND. .NOT.LRE .AND. .NOT.(LRGS.AND.LRGGS) ) .OR.         
     >           (LAGS.AND. .NOT.LRE .AND. .NOT.(LRG .AND.LRGGS) )
* jvw additional check for rare back-to-back topology to avoid large weights
         IF(COSKLKSL.lt.-0.9.and.abs(GAML4(3)/GAML4(4)).gt.0.96) THEN
           LPASS=.FALSE.
         ENDIF
*             
         LSWAP = ABS(COSKSL).LE.ACTK                                            
      ELSE IF(CONFIG.EQ.GAMMAE)THEN                                             
         LPASS = (LAGS.AND. .NOT.LRE .AND. COSQM1.GE.CDELT1)                    
* jvw cut this sliver of phase space to avoid large weights
         if(COSELKL+COSPLKL+COSELKSL+COSPLKSL-COSELEL+QML4(4).lt.-4)then
          LPASS=.FALSE.
         endif
         LSWAP = .FALSE.                                                        
      ENDIF                                                                     
      IF(.NOT.LPASS)GOTO 1                                                      
C Derive some quantities needed for calculating the event weight                
      CSK1CQ=( (QMPC+KC)**2 - KS**2 ) / (2.D0*QMPC*KC)                          
      IF(M/QM0C.LT..01D0)THEN                                                   
         DELTQ=QM0C*( 1.D0/2.D0 *(M/QM0C)**2 +  1.D0/8.D0 * (M/QM0C)**4         
     >              + 3.D0/48.D0*(M/QM0C)**6 + 15.D0/384.D0*(M/QM0C)**8)        
      ELSE                                                                      
         DELTQ=QM0C-QMPC                                                        
      ENDIF                                                                     
C These invariants are calculated in the gamma-e CM                             
      KAP1 = (PBC*KS*CSKS1P + M**2/2.D0 * KS/PBC)         / M**2                
      KAP2 = (PBC*KC*(1.D0+KZCKC) + M**2/2.D0 * KC/PBC)   / M**2                
      KAP3 = (-SC+M**2)/2.D0                              / M**2                
      KAP1P= (-QMPC*KS*CSKS1Q - DELTQ*KS)                 / M**2                
      KAP2P= (-QMPC*KC*CSK1CQ - DELTQ*KC)                 / M**2                
      KAP3P= (QM0C-QMC4(3))*PBC                           / M**2                
      A = 1.D0/KAP1  + 1.D0/KAP2  + 1.D0/KAP3                                   
      B = 1.D0/KAP1P + 1.D0/KAP2P + 1.D0/KAP3P                                  
      C = 1.D0/KAP1/KAP1P + 1.D0/KAP2/KAP2P + 1.D0/KAP3/KAP3P                   
      X = KAP1  + KAP2  + KAP3                                                  
      Z = KAP1*KAP1P + KAP2*KAP2P + KAP3*KAP3P                                  
      UA= KAP1  * KAP2  * KAP3                                                  
      UB= KAP1P * KAP2P * KAP3P                                                 
      RO= KAP1/KAP1P+KAP1P/KAP1 + KAP2/KAP2P+KAP2P/KAP2 +                       
     >    KAP3/KAP3P+KAP3P/KAP3                                                 
CMMR   XTERM =  2.D0*(A*B-C)*( (A+B)*(X+2.D0) - (A*B-C) - 8.D0 )                
CMMR  >       - 2.D0*X*(A**2+B**2) - 8.D0*C                                     
CMMR  >       + 4.D0*X/UA/UB*                                                   
CMMR  >               ( (UA+UB)*(X+1.D0)-(A*UA+B*UB)*(2.D0+Z*(1.D0-X)/X)        
CMMR  >                + X**2*(1.D0-Z) + 2.D0*Z )                               
CMMR  >       - 2.D0*RO*( A*B + C*(1.D0-X) )                                    
C                                                                               
C  A MORE STABLE WAY OF COMPUTING XTERM                                         
C                M. Martinez (Nov.1987)                                         
C                                                                               
       XTERM = 2.D0*(A*B-C)*( (A+B)*(X+2.D0) - (A*B-C) - 8.D0 )                 
     >       - 2.D0*X*(A**2+B**2 - C*RO )                                       
     >       - 8.D0*C                                                           
     >       + 4.D0*X*                                                          
     >         ( (1/UA+1/UB)*(X+1.D0)-(A/UB+B/UA)*(2.D0+Z*(1.D0-X)/X)           
     >                + X/UA*X/UB*(1.D0-Z) + 2.D0/UA*Z/UB )                     
     >       - 2.D0*RO*( A*B + C )                                              
C Calculate the 'exact' cross section                                           
C It is not symmetrized about + <--> - .                                        
C Here is the slightly approximated t                                           
      T   = -SQRTS*QP0*COSQP1 - M**2*FQP**2/EB/QP0                              
C Here is the more exact t:                                                     
      TE  = -2.D0*EBP*QPP0*COSQP1 -M**2*FQP**2/EB/QP0                           
      DSIGE = (ALPHA/PI)**4 * XTERM/M**2 /S /(-TE) *                            
     >        ( (S**2 + (S-SC)**2)/SC**2 + 2.D0*M**2/TE )                       
C If the Berends et al. eegg calculation is requested:                          
      IF(MTRXGG.EQ.BEEGG .OR. (MTRXGG.EQ.HEEGG .AND. -T.GE.HQ2MIN))THEN         
C Invariant products of photons and the electron.                               
C Work with the LAB 4 vectors only.                                             
C The CM does not have a physical value for p-                                  
C and was only developed for EPA methods.                                       
         BPMK1 = EB*GAML4(4) + EBP*GAML4(3)                                     
         BPMK2 = EB*GAMSL4(4) + EBP*GAMSL4(3)                                   
         BQMK1 = TPRD(QML4,GAML4)                                               
         BQMK2 = -KAP1P*M**2                                                    
         BK1K2 = TPRD(GAML4,GAMSL4)                                             
C Calculate the BK invariants:                                                  
         U = 2.D0*M**2 - SQRTS*QM0 + 2.D0*EBP*QMP0*COSQM                        
         SP= S - 2.D0*SQRTS*(K0+KSL) + 2.D0*BK1K2                               
         TP= 2.D0*M**2 - SQRTS*QM0 - 2.D0*EBP*QMP0*COSQM                        
         UP= 2.D0*M**2 - SQRTS*QP0                                              
     >                 - 2.D0*EBP*QPP0*(1.D0-COSQP1)                            
         S1= 4.D0*EB*(EB-K0)                                                    
         S2= 4.D0*EB*(EB-KSL)                                                   
         T1= TE- 2.D0*(BQMK1-BPMK1+BK1K2)                                       
         T2= TE- 2.D0*(BQMK2-BPMK2+BK1K2)                                       
         U1= U - SQRTS*K0*(2.D0-COSK1) - M**2*K0/EB*(COSK1-1.D0)                
     >       + 2.D0*BQMK1                                                       
         U2= U - SQRTS*KSL*(1.D0-COSKSL) - M**2*KSL/EB*COSKSL                   
     >       + 2.D0*BQMK2                                                       
         PPK1K2 = SP+UP+TP - 3.D0*M**2                                          
         PMK1K2 = SP+U +TE - 3.D0*M**2                                          
         QPK1K2 = S +U +TP - 3.D0*M**2                                          
         QMK1K2 = S +UP+TE - 3.D0*M**2                                          
         K1P = GAML4(4)+GAML4(3)                                                
         K1M = GAML4(4)-GAML4(3)                                                
         K2P = GAMSL4(4)+GAMSL4(3)                                              
         K2M = GAMSL4(4)-GAMSL4(3)                                              
         QPP = QPL4(4)+QPL4(3)                                                  
         QPM = QP0*COSQP1 - (QPP0-QP0)*(1.D0-COSQP1)                            
         QMP = QML4(4) + QML4(3)                                                
         QMM = QML4(4) - QML4(3)                                                
         DSIGE=BEEGGC(M,EB,QPL4,QML4,GAML4,GAMSL4                               
     >,               S1,S2,TE,T1,T2,U1,U2,PPK1K2,PMK1K2,QPK1K2,QMK1K2          
     >,               K1P,K1M,K2P,K2M,QPP,QPM,QMP,QMM                           
     >,               BPMK1,BPMK2,BQMK1,BQMK2,BK1K2)                            
      ELSE IF(MTRXGG.EQ.MEEGG)THEN                                              
         DSIGE=MEEGGC(QPL4,QML4,GAML4,GAMSL4)                                   
      ENDIF                                                                     
C Calculate approximate cross section.                                          
C Here the approximate sigma is symmetrized for k <--> ks                       
      DSIGA= 2.D0*(ALPHA/PI)**4 * QP0 / COSK1M / FQP**2 / (-T)                  
     >           *( 1.D0/(CSKS1P+EPSLON) + 1.D0/(CSKS1K+EPSLON) )               
     >           /(1.D0 - KS/PBC + EPS) / KS**2 / K0**2                         
     >           *(-M**2*KAP2P) * QP0/(QP0**2-M**2) * (1.D0 + EPS)              
      IF(LSWAP)THEN                                                             
        DSIGA= DSIGA                                                           
     >       +2.D0*(ALPHA/PI)**4 * QP0 / (CSKS1L+EPSLON) / FQP**2 / (-T)        
     >       *( 1.D0/(1.D0+KZCKC+EPSLON) + 1.D0/(CSKS1K+EPSLON) )               
     >       /(1.D0 - KC/PBC + EPS) / KC**2 / KSL**2                            
     >       *(-M**2*KAP1P) * QP0/(QP0**2-M**2) * (1.D0 + EPS)
      ENDIF                                                                     
C The weight of this trial event is, 
      WGHT = DSIGE/DSIGA                                                        
      WMAX = MAX(WMAX,WGHT)                                                     
      W1MAX= MAX(W1MAX,WGHT1)                                                   
C Keep track of quantities for the total cross section calculation.             
      CALL HFILL(9002,real(WGHT),0.,1.)                                               
      SUMWGT = SUMWGT + WGHT
* jvw
      if(wght.gt.wghtmx) SUMWGTO = SUMWGTO + WGHT
* 
      SUMW2  = SUMW2  + WGHT**2                                                 
      IF(UNWGHT.AND.WGHT.LT.DBLE(RNDM(dummy))*WGHTMX)GOTO 1                               

C An event has been accepted at this point                                      
C ----------------------------------------                                      
      NACC=NACC+1                                                               
C Look to see if the EPA is valid for the event sample                          
      Q2W2MX=MAX(Q2W2MX,-T/W2)                                                  
C Calculate the 4 vectors. Decide whether to interchange or not here.           
      IF(DBLE(RNDM(dummy)).LT..5)THEN                                                     
         IMISS=0                                                                
         IOBSV=4                                                                
         RSIGN=+1.D0                                                            
      ELSE                                                                      
         IMISS=4                                                                
         IOBSV=0                                                                
         RSIGN=-1.D0                                                            
      ENDIF                                                                     
      P(IMISS+1)=QPL4(1) * RSIGN                                                
      P(IMISS+2)=QPL4(2) * RSIGN                                                
      P(IMISS+3)=QPL4(3) * RSIGN                                                
      P(IMISS+4)=QPL4(4)                                                        
      P(IOBSV+1)=QML4(1) * RSIGN                                                
      P(IOBSV+2)=QML4(2) * RSIGN                                                
      P(IOBSV+3)=QML4(3) * RSIGN                                                
      P(IOBSV+4)=QML4(4)                                                        
      P(9) =GAML4(1) * RSIGN                                                    
      P(10)=GAML4(2) * RSIGN                                                    
      P(11)=GAML4(3) * RSIGN                                                    
      P(12)=GAML4(4)                                                            
      P(13)=GAMSL4(1) * RSIGN                                                   
      P(14)=GAMSL4(2) * RSIGN                                                   
      P(15)=GAMSL4(3) * RSIGN                                                   
      P(16)=GAMSL4(4)
c      IF(WGHT.gt.WGHTMX) then
*       write(26,*)WGHT,dsiga,dsige,P
c      ENDIF                                                         
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C DOUBLE PRECISION FUNCTION FQPEG:                                              
C Returns the minimum Eb-qp0, given the detector acceptance (e-gamma).          
C Returns -1 if configuration is kinematically impossible.                      
C-----------------------------------------------------------------------        
      DOUBLE PRECISION FUNCTION FQPEG(TEMIN,TGMIN,EEMIN,EGMIN,D,EB)             
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 TEMIN,TGMIN,EEMIN,EGMIN,D,EB                                       
      REAL*8 SQRTS,DELT,FQPTST,EETST,EGTST,SINED,SINGD,CON1                     
      REAL*8 SIGND(2)/1.D0,-1.D0/                                               
      INTEGER*4 I,J                                                             
      SQRTS=2.*EB                                                               
      FQPEG=2.*EB                                                               
C Go through each endpoint given by two of (TEMIN,TGMIN,EEMIN,EGMIN)            
C with TEVETO= DELT and -DELT                                                   
C 1) TEMIN,TGMIN                                                                
      DO 1 I=1,2                                                                
         DELT=D*SIGND(I)                                                        
         FQPTST = EB*TAN((TEMIN+DELT)/2.D0)*TAN((TGMIN-DELT)/2.D0)              
         EETST = (EB+FQPTST)*SIN(TGMIN-DELT)/                                   
     >           ( SIN(TEMIN+DELT)+SIN(TGMIN-DELT) )                            
         EGTST = (EB+FQPTST-EETST)                                              
         IF(EETST.GE.EEMIN .AND. EGTST.GE.EGMIN .AND.                           
     >      FQPTST.GE.0.D0 )FQPEG=MIN(FQPEG,FQPTST)                             
 1    CONTINUE                                                                  
C 2) EGMIN,TGMIN                                                                
      DO 2 I=1,2                                                                
         DELT=D*SIGND(I)                                                        
         FQPTST =  EB * EGMIN * SIN( (TGMIN-DELT)/2.D0 )**2/                    
     >            (EB - EGMIN * COS( (TGMIN-DELT)/2.D0 )**2)                    
         EETST = EB+FQPTST-EGMIN                                                
         SINED = EGMIN/EETST * SIN(TGMIN-DELT)                                  
         IF(EETST.GE.EEMIN .AND. SINED .GE. SIN(TEMIN+DELT) .AND.               
     >      FQPTST.GE.0.D0 .AND. SINED .LE. 1.D0)FQPEG=MIN(FQPEG,FQPTST)        
 2    CONTINUE                                                                  
C 3) EEMIN,TEMIN                                                                
      DO 3 I=1,2                                                                
         DELT=D*SIGND(I)                                                        
         FQPTST =  EB * EEMIN * SIN( (TEMIN+DELT)/2.D0 )**2/                    
     >            (EB - EEMIN * COS( (TEMIN+DELT)/2.D0 )**2)                    
         EGTST = EB+FQPTST-EEMIN                                                
         SINGD = EEMIN/EGTST * SIN(TEMIN+DELT)                                  
         IF(EGTST.GE.EGMIN .AND. SINGD .GE. SIN(TGMIN-DELT) .AND.               
     >      FQPTST.GE.0.D0 .AND. SINGD .LE. 1.D0)FQPEG=MIN(FQPEG,FQPTST)        
 3    CONTINUE                                                                  
C 4) EGMIN,TEMIN                                                                
      DO 4 I=1,2                                                                
         DELT=D*SIGND(I)                                                        
         CON1= (EGMIN/2.D0)**2 - EB*(EB-EGMIN)*TAN((TEMIN+DELT)/2.D0)**2        
         IF(CON1.GE.0.D0)THEN                                                   
            DO 41 J=1,2                                                         
               FQPTST = EGMIN/2.D0 + SIGND(J)*SQRT(CON1)                        
               EETST=EB+FQPTST-EGMIN                                            
               SINGD = EETST/EGMIN * SIN (TEMIN+DELT)                           
               IF(EETST.GE.EEMIN .AND. SINGD .GE. SIN(TGMIN-DELT) .AND.         
     >           FQPTST.GE.0.D0  .AND. SINGD .LE. 1.D0                          
     >           )FQPEG=MIN(FQPEG,FQPTST)                                       
 41         CONTINUE                                                            
         ENDIF                                                                  
 4    CONTINUE                                                                  
C 5) EEMIN,TGMIN                                                                
      DO 5 I=1,2                                                                
         DELT=D*SIGND(I)                                                        
         CON1= (EEMIN/2.D0)**2 - EB*(EB-EEMIN)*TAN((TGMIN-DELT)/2.D0)**2        
         IF(CON1.GE.0.D0)THEN                                                   
            DO 51 J=1,2                                                         
               FQPTST = EEMIN/2.D0 + SIGND(J)*SQRT(CON1)                        
               EGTST=EB+FQPTST-EEMIN                                            
               SINED = EGTST/EEMIN * SIN (TGMIN-DELT)                           
               IF(EGTST.GE.EGMIN .AND. SINED .GE. SIN(TEMIN+DELT) .AND.         
     >           FQPTST.GE.0.D0  .AND. SINED .LE. 1.D0                          
     >           )FQPEG=MIN(FQPEG,FQPTST)                                       
 51         CONTINUE                                                            
         ENDIF                                                                  
 5    CONTINUE                                                                  
C 6) EEMIN,EGMIN                                                                
      DO 6 I=1,2                                                                
         DELT=D*SIGND(I)                                                        
         FQPTST = EEMIN + EGMIN - EB                                            
         SINED = (EGMIN**2-(EB-FQPTST-EEMIN)**2)/4.D0/(EB-FQPTST)/EEMIN         
         SINGD = (EEMIN**2-(EB-FQPTST-EGMIN)**2)/4.D0/(EB-FQPTST)/EGMIN         
         IF(SINED.GE.SIN((TEMIN+DELT)/2.D0)**2 .AND. SINED.LE. 1.D0.AND.        
     >      SINGD.GE.SIN((TGMIN-DELT)/2.D0)**2 .AND. SINGD.LE. 1.D0             
     >     )FQPEG=MIN(FQPEG,FQPTST)                                             
 6    CONTINUE                                                                  
      IF(FQPEG.GT.EB)FQPEG=-1.D0                                                
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C DOUBLE PRECISION FUNCTION DMORK: Calculates the soft and virtual corr.        
C This routine contains the corrections to the misprints as communicated        
C by K.J. Mork. Reference: Phys. Rev. A4 (1971) 917.                            
C-----------------------------------------------------------------------        
      DOUBLE PRECISION FUNCTION DMORK(T,Y,U,E,DE)                               
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 PI,ALPHA                                                           
      PARAMETER (PI=3.14159265358979D0 , ALPHA=1.D0/137.036D0)                  
      REAL*8 T,Y,U,E,DE,TERM(10),SUM                                            
      INTEGER*4 I                                                               
      REAL*8 SPENCE                                                             
      EXTERNAL SPENCE                                                           
      TERM(1) = 2.D0*(1.D0-2.D0*Y)*U*LOG(2.D0*DE)                               
      TERM(2) = PI**2/6.D0*(4.D0-3.D0*T-1.D0/T-2.D0/E**4/T**3)                  
      TERM(3) = 4.D0*(2.D0-U)*Y**2                                              
      TERM(4) = -4.D0*Y+1.5D0*U+2.D0/E**2/T**2                                  
      TERM(5) = 4.D0*(1.D0-.5D0/T)*LOG(E)**2                                    
      TERM(6) = (2.D0*T+1.D0/T-2.D0+2.D0/E**4/T**3)*SPENCE(1.D0-E**2*T)         
      TERM(7) = (2.D0-5.D0*T-2.D0/T+4.D0*Y*(2.D0/T+T-2.D0))*LOG(E)              
      TERM(8) = -.5D0*U*LOG(1-T)**2                                             
      TERM(9) = -U*SPENCE(T)                                                    
      TERM(10)= (1.D0-2.D0/T-2.D0/E**2/T**2-.5D0*E**2/(1.D0-E**2*T)             
     >           +4.D0*Y*(T-1.D0+.5D0/T))*LOG(E**2*T)                           
      SUM=0.D0                                                                  
      DO 1 I=1,10                                                               
 1    SUM=SUM+TERM(I)                                                           
      DMORK=-ALPHA/PI/U * SUM                                                   
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C DOUBLE PRECISION FUNCTION SPENCE: Calculates the Spence function.(X<1)        
C-----------------------------------------------------------------------        
      DOUBLE PRECISION FUNCTION SPENCE(X)                                       
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 X,SUM,PI,PREC                                                      
      INTEGER*4 I,NTERMS                                                        
      PARAMETER (PI=3.14159265358979D0 , PREC=0.0001D0)                         
C The series used, depends on what X is...                                      
      IF(X.GT.1.D0)THEN                                                         
CBB      SPENCE=-66.D66                                                         
         SPENCE=-66.D36                                                         
      ELSE IF(X.EQ.1.D0)THEN                                                    
         SPENCE=PI**2/6.D0                                                      
      ELSE IF(X.GT.0.5D0)THEN                                                   
         SUM=0.D0                                                               
         NTERMS=1 + LOG(PREC/2.D0)/LOG(1.D0-X)                                  
         DO 1 I=1,MIN(100,NTERMS)                                               
 1       SUM=SUM + (1.D0-X)**I/I**2                                             
         SPENCE=PI**2/6.D0 - LOG(X)*LOG(1.D0-X)-SUM                             
      ELSE IF(X.EQ.0.D0)THEN                                                    
         SPENCE=0.D0                                                            
      ELSE IF(X.GT.-1.D0)THEN                                                   
         SUM=0.D0                                                               
         NTERMS=1 + LOG(ABS(X+X**2/4.D0)*PREC)/LOG(ABS(X))                      
         DO 2 I=1,MIN(100,NTERMS)                                               
 2       SUM=SUM + X**I/I**2                                                    
         SPENCE=SUM                                                             
      ELSE IF(X.EQ.-1.D0)THEN                                                   
         SPENCE=-PI**2/12.D0                                                    
      ELSE                                                                      
         SUM=0.D0                                                               
         NTERMS=1 + LOG(PREC)/LOG(ABS(1.D0/X))                                  
         DO 3 I=1,MIN(100,NTERMS)                                               
 3       SUM=SUM + (1.D0/X)**I/I**2                                             
         SPENCE=-(SUM+PI**2/6.D0+.5D0*LOG(-X)**2)                               
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C SUBROUTINE TBOORT: Boosts 4-momentum from g-e center of mass to lab.          
C-----------------------------------------------------------------------        
      SUBROUTINE TBOORT(IN,SINTR,COSTR,GAMA,BETA,SINQP,COSQP1,PHIP,OUT)         
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 IN(4),SINTR,COSTR,GAMA,BETA,SINQP,COSQP1,PHIP,OUT(4)               
      REAL*8 INROT(4),ROTBOO(4),CONST,COSQP                                     
      INROT(1)=IN(1)*COSTR - IN(3)*SINTR                                        
      INROT(2)=IN(2)                                                            
      INROT(3)=IN(3)*COSTR + IN(1)*SINTR                                        
      INROT(4)=IN(4)                                                            
      COSQP = 1.D0-COSQP1                                                       
      CONST = (GAMA-1.D0)*INROT(1)*SINQP + (GAMA-1.D0)*INROT(3)*COSQP           
     >        - GAMA*BETA*INROT(4)                                              
      ROTBOO(1) = INROT(1) + CONST*SINQP                                        
      ROTBOO(2) = INROT(2)                                                      
      ROTBOO(3) = INROT(3) + CONST*COSQP                                        
      ROTBOO(4) = GAMA * (INROT(4) - BETA*INROT(1)*SINQP                        
     >                              -BETA*INROT(3)*COSQP)                       
      OUT(1) = ROTBOO(1)*COS(PHIP) - ROTBOO(2)*SIN(PHIP)                        
      OUT(2) = ROTBOO(2)*COS(PHIP) + ROTBOO(1)*SIN(PHIP)                        
      OUT(3) = ROTBOO(3)                                                        
      OUT(4) = ROTBOO(4)                                                        
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C DOUBLE PRECISION FUNCTION BEEGGC: Calculates the matrix element for,          
C     +  _      +  _                                                            
C    e  e  --> e  e  gamma gamma                                                
C                                                                               
C using the equations supplied by Berends et al. Nucl.Phys.B264(1986)265        
C-----------------------------------------------------------------------        
      DOUBLE PRECISION FUNCTION BEEGGC(M,EB,QP,QM,K1,K2                         
     >,            S1,S2,T,T1,T2,U1,U2,PPK1K2,PMK1K2,QPK1K2,QMK1K2              
     >,            K1P,K1M,K2P,K2M,QPP,QPM,QMP,QMM                              
     >,            BPMK1,BPMK2,BQMK1,BQMK2,BK1K2)                               
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,PI                                                           
      PARAMETER                                                                 
     >(           ALPHA = 1.D0/137.036D0                                        
     >,           PI    = 3.14159265358979D0                                    
     >)                                                                         
*      REAL*8 TA,TP,SP,U,UP                                                      
*      COMMON/TEVQUA/TA,TP,SP,U,UP
      REAL*8 TA,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/TA,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                                                
C Input parameters                                                              
      REAL*8 M,EB,PP(4),PM(4),QP(4),QM(4),K1(4),K2(4)                           
     >,            S1,S2,T,T1,T2,U1,U2,PPK1K2,PMK1K2,QPK1K2,QMK1K2              
     >,            K1P,K1M,K2P,K2M,QPP,QPM,QMP,QMM                              
     >,            BPMK1,BPMK2,BQMK1,BQMK2,BK1K2                                
C Real quantities                                                               
      REAL*8 S,QPK1,QPK2,PPK1,PPK2                                              
     >,      V1Q(4),V1P(4),V1QV1P(4),V2Q(4),V2P(4),V2QV2P(4)                    
     >,      ABSQM,COLCUT,CQMK2,CPMK2,SP2,TP2,UP2,KPP2M,KPP2P                   
     >,      TERM1,TERM2,TERM3                                                  
C Externals                                                                     
      REAL*8   TPRD,BEEGGM,BCOLL                                                
      EXTERNAL TPRD,BEEGGM,BCOLL                                                
      PP(1)=0.D0                                                                
      PP(2)=0.D0                                                                
      PP(3)=SQRT(EB**2 - M**2)                                                  
      PP(4)=EB                                                                  
      PM(1)=0.D0                                                                
      PM(2)=0.D0                                                                
      PM(3)=-SQRT(EB**2 - M**2)                                                 
      PM(4)=EB                                                                  
      S = 4.D0*EB**2                                                            
      QPK1 = TPRD(QP,K1)                                                        
      PPK1 = TPRD(PP,K1)                                                        
      V1Q(1)=QP(1)/QPK1 - QM(1)/BQMK1                                           
      V1Q(2)=QP(2)/QPK1 - QM(2)/BQMK1                                           
      V1Q(3)=QP(3)/QPK1 - QM(3)/BQMK1                                           
      V1Q(4)=QP(4)/QPK1 - QM(4)/BQMK1                                           
      V1P(1)=PP(1)/PPK1 - PM(1)/BPMK1                                           
      V1P(2)=PP(2)/PPK1 - PM(2)/BPMK1                                           
      V1P(3)=PP(3)/PPK1 - PM(3)/BPMK1                                           
      V1P(4)=PP(4)/PPK1 - PM(4)/BPMK1                                           
      V1QV1P(1) = V1Q(1) - V1P(1)                                               
      V1QV1P(2) = V1Q(2) - V1P(2)                                               
      V1QV1P(3) = V1Q(3) - V1P(3)                                               
      V1QV1P(4) = V1Q(4) - V1P(4)                                               
      QPK2 = TPRD(QP,K2)                                                        
      PPK2 = TPRD(PP,K2)                                                        
      V2Q(1)=QP(1)/QPK2 - QM(1)/BQMK2                                           
      V2Q(2)=QP(2)/QPK2 - QM(2)/BQMK2                                           
      V2Q(3)=QP(3)/QPK2 - QM(3)/BQMK2                                           
      V2Q(4)=QP(4)/QPK2 - QM(4)/BQMK2                                           
      V2P(1)=PP(1)/PPK2 - PM(1)/BPMK2                                           
      V2P(2)=PP(2)/PPK2 - PM(2)/BPMK2                                           
      V2P(3)=PP(3)/PPK2 - PM(3)/BPMK2                                           
      V2P(4)=PP(4)/PPK2 - PM(4)/BPMK2                                           
      V2QV2P(1) = V2Q(1) - V2P(1)                                               
      V2QV2P(2) = V2Q(2) - V2P(2)                                               
      V2QV2P(3) = V2Q(3) - V2P(3)                                               
      V2QV2P(4) = V2Q(4) - V2P(4)                                               
      ABSQM = SQRT(QM(4)**2-M**2)                                               
      COLCUT=SQRT(M*EB)                                                         
C Check if a collinear cross section should be used...                          
C if K2 is collinear with both p- and q-, choose the smaller coll               
      CPMK2=BCOLL(PM,K2)                                                        
      CQMK2=BCOLL(QM,K2)                                                        
      IF(MAX(CPMK2,CQMK2).LT.COLCUT)THEN                                        
         IF(CPMK2.LT.CQMK2)THEN                                                 
            CQMK2=COLCUT*10.D0                                                  
         ELSE                                                                   
            CPMK2=COLCUT*10.D0                                                  
         ENDIF                                                                  
      ENDIF                                                                     
      IF(CQMK2.LT.COLCUT)THEN                                                   
         SP2 = S1                                                               
         TP2 = T1                                                               
         UP2 = U1                                                               
         KPP2M = (BQMK2 - K2(4)*(QM(4)-ABSQM))/ABSQM                            
         KPP2P = 2.D0*K2(4)-KPP2M                                               
         BEEGGC = -ALPHA**4/2.D0/PI**4/S                                        
     >             * TPRD(V1QV1P,V1QV1P)                                        
     >             * QM(4)*KPP2M/(2.D0*QM(4)+KPP2P)/KPP2P/BQMK2**2              
     >             * ( 4.D0*QM(4)**2 + (2.D0*QM(4)+KPP2P)**2                    
     >                + M**2*KPP2P**3/4.D0/QM(4)**2/KPP2M )                     
     >             * ( S*SP2*(S**2+SP2**2) + T*TP2*(T**2+TP2**2)                
     >                +U2*UP*(U2**2+UP**2) )                                    
     >             / (S*SP2*T*TP2)                                              
      ELSE IF(CPMK2.LT.COLCUT)THEN                                              
         SP2 = S1                                                               
         TP2 = T1                                                               
         UP2 = U1                                                               
         IF(K2P.LT.1.D-20) K2P=1.D-20
         BEEGGC = -ALPHA**4/2.D0/PI**4/S                                        
     >             * TPRD(V1QV1P,V1QV1P)                                        
     >             * EB*K2P/(2.D0*EB-K2M)/K2M/BPMK2**2                          
     >             * ( S + (2.D0*EB-K2M)**2 + M**2*K2M**3/S/K2P )               
     >             * ( S2*SP*(S2**2+SP**2) + T*TP2*(T**2+TP2**2)                
     >                +U*UP2*(U**2+UP2**2) )                                    
     >             / (S2*SP*T*TP2)                                              
      ELSE                                                                      
         TERM1 =ALPHA**4/4.D0/PI**4/S                                           
     >          * TPRD(V1QV1P,V1QV1P) * TPRD(V2QV2P,V2QV2P)                     
     >          *(S*SP*(S**2+SP**2)+T*TP*(T**2+TP**2)+U*UP*(U**2+UP**2))        
     >          /(S*SP*T*TP)                                                    
         TERM2 =  4.D0*ALPHA**4 / PI**4 /S                                      
     >             * BEEGGM(M,EB,PP,PM,QP,QM,K1,K2                              
     >,               S1,S2,T1,T2,U1,U2,PPK1K2,PMK1K2,QPK1K2,QMK1K2             
     >,               K1P,K1M,K2P,K2M,QPP,QPM,QMP,QMM)                          
         TERM3 =  4.D0*ALPHA**4 / PI**4 /S                                      
     >             * BEEGGM(M,EB,PP,PM,QP,QM,K2,K1                              
     >,               S2,S1,T2,T1,U2,U1,PPK1K2,PMK1K2,QPK1K2,QMK1K2             
     >,               K2P,K2M,K1P,K1M,QPP,QPM,QMP,QMM)                          
         BEEGGC=TERM1+TERM2+TERM3                                               
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C DOUBLE PRECISION FUNCTION BCOLL: Determines if photon is collinear            
C with a fermion. Does not need to be very accurate.                            
C-----------------------------------------------------------------------        
      DOUBLE PRECISION FUNCTION BCOLL(P,K)                                      
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 P(4),K(4)                                                          
      REAL*8 COST,SINT                                                          
      BCOLL=999.D0                                                              
      COST=(P(1)*K(1)+P(2)*K(2)+P(3)*K(3))                                      
     >     /DSQRT(P(1)**2+P(2)**2+P(3)**2)/K(4)
      IF(COST**2.GT.1.00001D0) THEN
        WRITE(6,*)'COST**2 TOO LARGE',COST**2
      ENDIF
      SINT=DSQRT(DABS(1.D0-COST**2))
      IF(COST.GT.0.D0)BCOLL=K(4)*SINT                                           
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C DOUBLE PRECISION FUNCTION BEEGGM: Calculates the squared matrix elemnt        
C-----------------------------------------------------------------------        
      DOUBLE PRECISION FUNCTION BEEGGM(M,EB,PP,PM,QP,QM,K1,K2                   
     >,            S1,S2,T1,T2,U1,U2,PPK1K2,PMK1K2,QPK1K2,QMK1K2                
     >,            K1P,K1M,K2P,K2M,QPP,QPM,QMP,QMM)                             
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      COMPLEX*16 I                                                              
      PARAMETER( I = (0.D0,1.D0) )                                              
*      REAL*8 T,TP,SP,U,UP                                                       
*      COMMON/TEVQUA/T,TP,SP,U,UP                                                
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF 
C Input parameters                                                              
      REAL*8 M,EB,PP(4),PM(4),QP(4),QM(4),K1(4),K2(4)                           
     >,            S1,S2,T1,T2,U1,U2,PPK1K2,PMK1K2,QPK1K2,QMK1K2                
     >,            K1P,K1M,K2P,K2M,QPP,QPM,QMP,QMM                              
      INTEGER*4 J                                                               
C Real quantities                                                               
      REAL*8 ECM,S                                                              
C Complex quantities                                                            
      COMPLEX*16 QPT,QMT,K1T,K2T,WP,WM,W1,W2                                    
     >,          A,A1,B,B1,C,C1,C11,C111,D,D1                                   
     >,          M1,M1TERM(12),M2,M2TERM(8),M3,M3TERM(8)                        
C Externals                                                                     
      REAL*8 TPRD                                                               
      EXTERNAL TPRD                                                             
      ECM=2.D0*EB                                                               
      S=ECM**2                                                                  
      QPT = QP(1) + I*QP(2)                                                     
      QMT = QM(1) + I*QM(2)                                                     
      K1T = K1(1) + I*K1(2)                                                     
      K2T = K2(1) + I*K2(2)                                                     
      WP = QPT/QPP                                                              
      WM = QMT/QMP                                                              
      W1 = K1T/K1P                                                              
      W2 = K2T/K2P                                                              
      A = QPP * CONJG(WP) + K1P * CONJG(W1)                                     
      A1= QMP * CONJG(WM) + K1P * CONJG(W1)                                     
      B = ECM - K1M + CONJG(K1T)*WP + K1T*CONJG(WM) - K1P*WP*CONJG(WM)          
      B1= ECM - K1M + CONJG(K1T)*WM + K1T*CONJG(WP) - K1P*WM*CONJG(WP)          
      C   = ECM - K1M + CONJG(K1T)*WP                                           
      C1  = ECM - K1M + CONJG(K1T)*WM                                           
      C11 = ECM - K2M + CONJG(K2T)*WP                                           
      C111= ECM - K2M + CONJG(K2T)*WM                                           
      D = (ECM-K2P)*CONJG(WM) + CONJG(K2T)                                      
      D1= (ECM-K2P)*CONJG(WP) + CONJG(K2T)                                      
      M1TERM(1) = SQRT(QPP/QMP) / (S1*K1P*K2P) * C/CONJG(W1)                    
     >            * C/(WP-W2)/(WM-W2)                                           
      M1TERM(2) = SQRT(QMP/QPP) / (S2*K1P*K2P) * D/W2                           
     >            * D/CONJG(WP-W1)/CONJG(WM-W1)                                 
      M1TERM(3) = ECM / SQRT(QPP*QMP) / (S*K1P*K2P) * C/(WP-W2)/(WM-W2)         
     >            * D/CONJG(WP-W1)/CONJG(WM-W1)                                 
      M1TERM(4) = SQRT(QPP*QMP) / ECM / (SP*K1P*K2P) * C/CONJG(W1)*D/W2         
      M1TERM(5) = ECM / SQRT(QPP*QMP) / (T1*K1P*K2P)                            
     >            * A/CONJG(W1)/CONJG(WP-W1) * A/(WM-W2)                        
      M1TERM(6) = SQRT(QPP*QMP) / ECM / (T2*K1P*K2P)                            
     >            * B/CONJG(WM-W1) * B/W2/(WP-W2)                               
      M1TERM(7) =-SQRT(QPP/QMP) / (T*K1P*K2P) * A/(WM-W2)                       
     >            * B/CONJG(WM-W1)                                              
      M1TERM(8) =-SQRT(QMP/QPP) / (TP*K1P*K2P)                                  
     >            * A/CONJG(W1)/CONJG(WP-W1) *B/W2/(WP-W2)                      
      M1TERM(9) =-SQRT(QPP*QMP) / PPK1K2 * (1.D0/SP + 1.D0/TP)                  
     >            * CONJG(W2)/CONJG(W1)/W2 * ( B + (WP-W1)*D )                  
      M1TERM(10)= SQRT(QPP*QMP) / PMK1K2 * (1.D0/SP + 1.D0/T )                  
     >            *( CONJG(W2)*B + CONJG(WM-W2)*C )                             
      M1TERM(11)=-SQRT(QMP/QPP) * ECM/QPK1K2 * (1.D0/S + 1.D0/TP)               
     >            * (WP-W1)/CONJG(WP-W1)/(WP-W2)                                
     >            *( CONJG(W2)*D + CONJG(W2-WM)*A )                             
      M1TERM(12)= SQRT(QPP/QMP) * ECM/QMK1K2 * (1.D0/S + 1.D0/T)                
     >            * CONJG(WM-W2)/CONJG(WM-W1)/(WM-W2)                           
     >            *( C + (W1-WP)*A )                                            
      M1 = (0.D0,0.D0)                                                          
      DO 1 J=1,12                                                               
 1    M1 = M1 + M1TERM(J)                                                       
      M2TERM(1) =-SQRT(QMP/QPP) / (S1*K1P*K2P) * C1/CONJG(W1)                   
     >            * C1/(WM-W2)/(WP-W2)                                          
      M2TERM(2) =-SQRT(QPP/QMP) / (S2*K1P*K2P) * D1/W2                          
     >            * D1/CONJG(WM-W1)/CONJG(WP-W1)                                
      M2TERM(3) = ECM / SQRT(QPP*QMP) / (S*K1P*K2P) * C1/(WM-W2)/(WP-W2)        
     >            * D1/CONJG(WM-W1)/CONJG(WP-W1)                                
      M2TERM(4) = SQRT(QPP*QMP)/ECM  / (SP*K1P*K2P) * C1/CONJG(W1)*D1/W2        
      M2TERM(5) =-SQRT(QPP*QMP) / PPK1K2 * (1.D0/SP)                            
     >            * CONJG(W2)/CONJG(W1)/W2 * ( B1 + (WM-W1)*D1 )                
      M2TERM(6) = SQRT(QPP*QMP) / PMK1K2 * (1.D0/SP)                            
     >            *( CONJG(W2)*B1 + CONJG(WP-W2)*C1)                            
      M2TERM(7) =-SQRT(QPP/QMP) * ECM/QMK1K2 * (1.D0/S)                         
     >            * (WM-W1)/CONJG(WM-W1)/(WM-W2)                                
     >            *( CONJG(W2)*D1 + CONJG(W2-WP)*A1 )                           
      M2TERM(8) = SQRT(QMP/QPP) * ECM/QPK1K2 * (1.D0/S)                         
     >            * CONJG(WP-W2)/CONJG(WP-W1)/(WP-W2)                           
     >            *( C1 + (W1-WM)*A1 )                                          
      M2 = (0.D0,0.D0)                                                          
      DO 2 J=1,8                                                                
 2    M2 = M2 + M2TERM(J)                                                       
      M3TERM(1) =-SQRT(QPP/QMP) / (T2*K1P*K2P)                                  
     >            * CONJG(D1)/CONJG(WM-W1) * CONJG(D1)/W2/(WP-W2)               
      M3TERM(2) =-SQRT(QMP/QPP) / (T1*K1P*K2P)                                  
     >            * C111/(WM-W2) * C111/CONJG(W1)/CONJG(WP-W1)                  
      M3TERM(3) = SQRT(QPP/QMP) / (T*K1P*K2P)                                   
     >            * CONJG(D1)/CONJG(WM-W1) * C111/(WM-W2)                       
      M3TERM(4) = SQRT(QMP/QPP) / (TP*K1P*K2P)                                  
     >            * CONJG(D1)/W2/(WP-W2) * C111/CONJG(W1)/CONJG(WP-W1)          
      M3TERM(5) = SQRT(QPP*QMP) / PPK1K2 * (1.D0/TP)                            
     >            * CONJG(W2)/CONJG(W1)/W2                                      
     >            * ( (WP-W1)*CONJG(D) + (W1-WM)*CONJG(D1) )                    
      M3TERM(6) =-SQRT(QPP*QMP) / PMK1K2 * (1.D0/T)                             
     >            *( (W1-WP)*C111 + (WM-W1)*C11 )                               
      M3TERM(7) = SQRT(QMP/QPP) * ECM/QPK1K2 * (1.D0/TP)                        
     >            * (WP-W1)/(WP-W2)/CONJG(WP-W1)                                
     >            *( C111 + CONJG(W2)*CONJG(D) )                                
      M3TERM(8) =-SQRT(QPP/QMP) * ECM/QMK1K2 * (1.D0/T)                         
     >            * (WM-W1)/CONJG(WM-W1)/(WM-W2)                                
     >            *( C11 +  CONJG(W2)*CONJG(D1) )                               
      M3 = (0.D0,0.D0)                                                          
      DO 3 J=1,8                                                                
 3    M3 = M3 + M3TERM(J)                                                       
      BEEGGM = ABS(M1)**2 + ABS(M2)**2 + ABS(M3)**2                             
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C DOUBLE PRECISION FUNCTION TPRD: Calculates the invariant product.             
C-----------------------------------------------------------------------        
      DOUBLE PRECISION FUNCTION TPRD(A,B)                                       
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 A(4),B(4)                                                          
      TPRD = A(4)*B(4) - A(1)*B(1) - A(2)*B(2) - A(3)*B(3)                      
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C SUBROUTINE INMART: Initialization of some constants of Martinez/Miquel        
C-----------------------------------------------------------------------        
      SUBROUTINE INMART                                                         
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
      REAL*8 PIMART,DR,SR2,PB,ALF,ALF2,ALDPI,ME,ME2                             
      COMMON /MCONST/PIMART,DR,SR2,PB                                           
      COMMON /MARQED/  ALF,ALF2,ALDPI                                           
      COMMON /MLEPT1/ME,ME2                                                     
      PIMART = PI                                                               
      SR2    = SQRT(2.D0)                                                       
      ALF    = ALPHA                                                            
      ALF2   = ALPHA**2                                                         
      ALDPI  = ALPHA/PI                                                         
      ME     = M                                                                
      ME2    = M**2                                                             
      RETURN                                                                    
      END                                                                       
C-----------------------------------------------------------------------        
C DOUBLE PRECISION FUNCTION MEEGGC: Interface to Martinez/Miquel program        
C-----------------------------------------------------------------------        
      DOUBLE PRECISION FUNCTION MEEGGC(QPL4,QML4,GAML4,GAMSL4)                  
      IMPLICIT LOGICAL*1 (A-Z)                                                  
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                               
      PARAMETER                                                                 
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4        
     >,          M     = 0.5110034 D-3                                          
     >,          PBARN = .389386 D9                                             
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI                   
     >)                                                                         
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE                   
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG                     
      PARAMETER                                                                 
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31              
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32              
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33              
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34              
     >)                                                                         
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                          
      LOGICAL*4     UNWGHT                                                      
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN                    
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS                      
     >,             WGHT1M,WGHTMX                                               
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT                  
      INTEGER*4 NRNDMX                                                          
      PARAMETER (NRNDMX=20)                                                     
      REAL*4 RND(NRNDMX)                                                        
      INTEGER*4 SEED,NXSEED,BSEED                                               
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                         
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      COMMON/TCONST/                                                            
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK                  
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC         
      REAL*8 P(16),RSIGN                                                        
      COMMON/TEVENT/P,RSIGN                                                     
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                         
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                  
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                         
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                                   
      INTEGER*4 NTRIAL,NPASSQ,NACC                                              
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                  
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER                            
     >,             NTRIAL,NPASSQ,NACC                                          
      REAL*8 QPL4(4),QML4(4),GAML4(4),GAMSL4(4)                                 
      REAL*8 ANS                                                                
      COMMON / MOMENZ / P1,P2,P3,P4,P5,P6                                       
      REAL*8 P1(5),P2(5),P3(5),P4(5),P5(5),P6(5)                                
      INTEGER*4 I                                                               
      P1(1)=0.D0                                                                
      P1(2)=0.D0                                                                
      P1(3)=-EBP                                                                
      P1(4)=EB                                                                  
      P2(1)=0.D0                                                                
      P2(2)=0.D0                                                                
      P2(3)=EBP                                                                 
      P2(4)=EB                                                                  
      DO 3 I=1,4                                                                
 3    P3(I)=QML4(I)                                                             
      DO 4 I=1,4                                                                
 4    P4(I)=QPL4(I)                                                             
      DO 5 I=1,4                                                                
 5    P5(I)=GAML4(I)                                                            
      DO 6 I=1,4                                                                
 6    P6(I)=GAMSL4(I)                                                           
      P1(5)=M                                                                   
      P2(5)=M                                                                   
      P3(5)=M                                                                   
      P4(5)=M                                                                   
      P5(5)=0.D0                                                                
      P6(5)=0.D0                                                                
      CALL ELEMAT(ANS)                                                          
      MEEGGC=ANS/(TWOPI**8)/2./S *2.                                            
      RETURN                                                                    
      END                                                                       
C ----------------------------------------------------------------------        
C MARTINEZ/MIQUEL SUBROUTINES:                                                  
C The routines below were provided by Martinez & Miquel.                        
C Reference: preprint UAB-LFAE 87-01 (Barcelona)                                
C ----------------------------------------------------------------------        
C The following external references have had their names changed to             
C protect the innocent (more obscure names so that conflicts are                
C less likely).                                                                 
C Original subprogram name        New name                                      
C ------------------------        --------                                      
C ELEMAT                           (same)                                       
C AMTOT                            AMTOTM                                       
C AMPLI                            AMPLIM                                       
C Z                                ZMART                                        
C SPININ                           (same)                                       
C Original common block name      New name                                      
C --------------------------      --------                                      
C PRODUX                           (same)                                       
C CONST                            MCONST                                       
C QED                              MARQED                                       
C LEPT1                            MLEPT1                                       
C MOMENZ                           (same)                                       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC               
C                                                                               
C    THIS ROUTINE CALCULATES THE MATRIX ELEMENT SQUARED                         
C    FOR THE PROCESS                                                            
C    E-(P1) + E+(P2) ----> E-(P3) + E+(P4) + G(P5) + G(P6)                      
C    THE OUTPUT CONTAINS ALL FACTORS RELATED WITH MAT. ELEMENT                  
C    BUT NOT CONVERSION TO PB.                                                  
C                                                                               
C    (THIS HELICITY AMPLITUDES METHOD IS DESCRIBED, FOR INSTANCE,               
C     IN DESY 86-062 AND 86-114 REPORTS)                                        
C                                                                               
C                          M.MARTINEZ & R.MIQUEL   BARCELONA-87                 
C                                                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC               
      SUBROUTINE ELEMAT(WT)                                                     
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      DIMENSION NCONF1(6,16),NCONF2(6,16),NCONF3(6,16),NCONF4(6,16)             
      DATA NCONF1/                                                              
     . 1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1,-1,                                    
     . 1, 1, 1, 1,-1,-1,   1, 1, 1, 1,-1, 1,                                    
     . 1, 1,-1,-1, 1, 1,   1, 1,-1,-1, 1,-1,                                    
     . 1, 1,-1,-1,-1,-1,   1, 1,-1,-1,-1, 1,                                    
     .-1,-1,-1,-1, 1, 1,  -1,-1,-1,-1, 1,-1,                                    
     .-1,-1,-1,-1,-1,-1,  -1,-1,-1,-1,-1, 1,                                    
     .-1,-1, 1, 1, 1, 1,  -1,-1, 1, 1, 1,-1,                                    
     .-1,-1, 1, 1,-1,-1,  -1,-1, 1, 1,-1, 1/                                    
      DATA NCONF2/                                                              
     . 1, 1, 1,-1, 1, 1,   1, 1, 1,-1, 1,-1,                                    
     . 1, 1, 1,-1,-1,-1,   1, 1, 1,-1,-1, 1,                                    
     . 1, 1,-1, 1, 1, 1,   1, 1,-1, 1, 1,-1,                                    
     . 1, 1,-1, 1,-1,-1,   1, 1,-1, 1,-1, 1,                                    
     .-1,-1,-1, 1, 1, 1,  -1,-1,-1, 1, 1,-1,                                    
     .-1,-1,-1, 1,-1,-1,  -1,-1,-1, 1,-1, 1,                                    
     .-1,-1, 1,-1, 1, 1,  -1,-1, 1,-1, 1,-1,                                    
     .-1,-1, 1,-1,-1,-1,  -1,-1, 1,-1,-1, 1/                                    
      DATA NCONF3/                                                              
     . 1,-1, 1, 1, 1, 1,   1,-1, 1, 1, 1,-1,                                    
     . 1,-1, 1, 1,-1,-1,   1,-1, 1, 1,-1, 1,                                    
     . 1,-1,-1,-1, 1, 1,   1,-1,-1,-1, 1,-1,                                    
     . 1,-1,-1,-1,-1,-1,   1,-1,-1,-1,-1, 1,                                    
     .-1, 1,-1,-1, 1, 1,  -1, 1,-1,-1, 1,-1,                                    
     .-1, 1,-1,-1,-1,-1,  -1, 1,-1,-1,-1, 1,                                    
     .-1, 1, 1, 1, 1, 1,  -1, 1, 1, 1, 1,-1,                                    
     .-1, 1, 1, 1,-1,-1,  -1, 1, 1, 1,-1, 1/                                    
      DATA NCONF4/                                                              
     . 1,-1, 1,-1, 1, 1,   1,-1, 1,-1, 1,-1,                                    
     . 1,-1, 1,-1,-1,-1,   1,-1, 1,-1,-1, 1,                                    
     . 1,-1,-1, 1, 1, 1,   1,-1,-1, 1, 1,-1,                                    
     . 1,-1,-1, 1,-1,-1,   1,-1,-1, 1,-1, 1,                                    
     .-1, 1,-1, 1, 1, 1,  -1, 1,-1, 1, 1,-1,                                    
     .-1, 1,-1, 1,-1,-1,  -1, 1,-1, 1,-1, 1,                                    
     .-1, 1, 1,-1, 1, 1,  -1, 1, 1,-1, 1,-1,                                    
     .-1, 1, 1,-1,-1,-1,  -1, 1, 1,-1,-1, 1/                                    
C                                                                               
      NSPIN = 16                                                                
      CALL SPININ(0)                                                            
C                                                                               
      WT = 0.D0                                                                 
      DO 100 I=1,NSPIN                                                          
      PCONF = AMTOTM(NCONF1(1,I))                                               
      WT  = WT + PCONF                                                          
 100  CONTINUE                                                                  
      DO 200 I=1,NSPIN                                                          
      PCONF = AMTOTM(NCONF2(1,I))                                               
      WT  = WT + PCONF                                                          
 200  CONTINUE                                                                  
      DO 300 I=1,NSPIN                                                          
      PCONF = AMTOTM(NCONF3(1,I))                                               
      WT  = WT + PCONF                                                          
 300  CONTINUE                                                                  
      DO 400 I=1,NSPIN                                                          
      PCONF = AMTOTM(NCONF4(1,I))                                               
      WT  = WT + PCONF                                                          
 400  CONTINUE                                                                  
C                                                                               
C   FACTOR 8 STANDS FOR AVERAGE OVER INITIAL POLS AND PHOT SYMM FACTOR          
      WT = WT/8.D0                                                              
      RETURN                                                                    
      END                                                                       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC              
C                                                                               
C    THIS FUNCTION ADDS THE CONTRIBUTION OF ALL THE POLARIZATION                
C    CONFIGURATIONS BY THE ADEQUATE PERMUTATIONS OF THE ONE                     
C    CALCULATED IN AMPLI                                                        
C                                                                               
C                           M.MARTINEZ & R.MIQUEL  BARCELONA-87                 
C                                                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC              
      FUNCTION AMTOTM(L)                                                        
      IMPLICIT REAL*8(A-H,M,O-Z)                                                
      COMPLEX*16 AMT,AMPLIM,SP,SM                                               
      DIMENSION SP(6,6),SM(6,6),D(6,6),E(6),U(6),B(6)                           
      DIMENSION L(6)                                                            
      COMMON / PRODUX / SP,SM,U,E,D                                             
      COMMON /MCONST/ PI,DR,SR2,PB                                              
C                                                                               
C  SR2 IS THE SQUARED ROOT OF 2                                                 
      COMMON /MARQED/ALF,ALF2,ALDPI                                             
      DATA INIT/0/                                                              
      IF(INIT.NE.0)GO TO 100                                                    
      INIT=1                                                                    
C                                                                               
C  OVERALL FACTOR                                                               
      AMFAC = (4.D0*PI*ALF)**4                                                  
100   CONTINUE                                                                  
C                                                                               
      AMT =    AMPLIM (1,L(1),2,L(2),3,L(3),4,L(4),5, L(5),6, L(6),1)           
     .+        AMPLIM (4,L(4),3,L(3),2,L(2),1,L(1),6, L(6),5, L(5),1)           
     .+ DCONJG(AMPLIM (3,L(3),4,L(4),1,L(1),2,L(2),6,-L(6),5,-L(5),1)           
     .    +    AMPLIM (2,L(2),1,L(1),4,L(4),3,L(3),5,-L(5),6,-L(6),1))          
     .+        AMPLIM (1,L(1),2,L(2),3,L(3),4,L(4),5, L(5),6, L(6),2)           
     .+        AMPLIM (4,L(4),3,L(3),2,L(2),1,L(1),6, L(6),5, L(5),2)           
     .+        AMPLIM (1,L(1),2,L(2),3,L(3),4,L(4),5, L(5),6, L(6),3)           
     .+ DCONJG(AMPLIM (3,L(3),4,L(4),1,L(1),2,L(2),5,-L(5),6,-L(6),3))          
     .+        AMPLIM (1,L(1),2,L(2),3,L(3),4,L(4),5, L(5),6, L(6),4)           
     .+ DCONJG(AMPLIM (3,L(3),4,L(4),1,L(1),2,L(2),5,-L(5),6,-L(6),4))          
C                                                                               
      AMT =    AMT                                                              
     .+        AMPLIM (1,L(1),2,L(2),3,L(3),4,L(4),6, L(6),5, L(5),1)           
     .+        AMPLIM (4,L(4),3,L(3),2,L(2),1,L(1),5, L(5),6, L(6),1)           
     .+ DCONJG(AMPLIM (3,L(3),4,L(4),1,L(1),2,L(2),5,-L(5),6,-L(6),1)           
     .    +    AMPLIM (2,L(2),1,L(1),4,L(4),3,L(3),6,-L(6),5,-L(5),1))          
     .+        AMPLIM (1,L(1),2,L(2),3,L(3),4,L(4),6, L(6),5, L(5),2)           
     .+        AMPLIM (4,L(4),3,L(3),2,L(2),1,L(1),5, L(5),6, L(6),2)           
     .+        AMPLIM (1,L(1),2,L(2),3,L(3),4,L(4),6, L(6),5, L(5),3)           
     .+ DCONJG(AMPLIM (3,L(3),4,L(4),1,L(1),2,L(2),6,-L(6),5,-L(5),3))          
     .+        AMPLIM (1,L(1),2,L(2),3,L(3),4,L(4),6, L(6),5, L(5),4)           
     .+ DCONJG(AMPLIM (3,L(3),4,L(4),1,L(1),2,L(2),6,-L(6),5,-L(5),4))          
C                                                                               
      AMTOTM =  AMFAC*AMT*DCONJG(AMT)                                           
      RETURN                                                                    
      END                                                                       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC               
C                                                                               
C    THIS FUNCTION CALCULATES THE AMPLITUDE FOR THE                             
C    GAMMA IN T-CHANNEL FOR THE PROCESS                                         
C    E-(P1) + E+(P2) ----> E-(P3) + E+(P4) + G(P5) + G(P6)                      
C    WITH BREMSS. FROM THE SAME                  E+ OR E-  WITH IND=1           
C    WITH BREMSS. FROM DIFFERENT                 E+ OR E-  WITH IND=2           
C    WITH BREMSS. FROM BOTH INITIAL OR FINAL     E+ &  E-  WITH IND=3           
C    WITH BREMSS. FROM ONE INITIAL AND ONE FINAL E+ &  E-  WITH IND=4           
C                                                                               
C                            M.MARTINEZ & R.MIQUEL   BARCELONA-87               
C                                                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC               
      FUNCTION AMPLIM(P1,L1,P2,L2,P3,L3,P4,L4,P5,L5,P6,L6,IND)                  
      IMPLICIT REAL*8(A-H,M,O-Z)                                                
      COMPLEX*16 ZMART,SP,SM                                                    
      COMPLEX*16 AMPLIM,ZREP1,ZREP5                                             
      INTEGER P,P1,P2,P3,P4,P5,P6                                               
      COMMON / PRODUX / SP,SM,U,E,D                                             
      COMMON /MCONST / PI,DR,SR2,PB                                             
      COMMON /MLEPT1/ME,ME2                                                     
C                                                                               
C  ME IS THE ELECTRON MASS                                                      
C  ME2 = ME**2                                                                  
      DIMENSION SP(6,6),SM(6,6),D(6,6),E(6),U(6),B(6)                           
C                                                                               
C  INITIALIZATION                                                               
      DATA B/1.D0, 1.D0,-1.D0,-1.D0,-1.D0,-1.D0/                                
      DATA P/3/                                                                 
C                                                                               
      AMPLIM = (0.D0,0.D0)                                                      
C                                                                               
C  NORMALIZATION FACTORS FOR THE PHOTON POLARIZATION VECTOR                     
      ZN5=1.D0/(SR2 * CDABS(SP(P,P5)) )                                         
      ZN6=1.D0/(SR2 * CDABS(SP(P,P6)) )                                         
C                                                                               
C   LOOP OVER REPEATED INDEX "IL" AND  "ILP"                                    
      DO 100 I1=1,2                                                             
      ILP   = 2*I1 - 3                                                          
      ZREP5 = ZMART(P5,ILP,P1,L1,P,L5,P5,L5)                                    
      ZREP1 = ZMART(P1,ILP,P1,L1,P,L5,P5,L5)                                    
      DO 100 I2=1,2                                                             
      IL    = 2*I2 - 3                                                          
      IF     (IND.EQ.1) THEN                                                    
      AMPLIM = AMPLIM +                                                         
     .   ZMART(P2,L2,P4,L4,P3,L3,P1,IL)             * B(P1) *                   
     .     (ZMART(P1,IL,P5,ILP,P,L6,P6,L6)  * ZREP5 * B(P5)                     
     .     +ZMART(P1,IL,P1,ILP,P,L6,P6,L6)  * ZREP1 * B(P1))                    
     . + ZMART(P2,L2,P4,L4,P3,L3,P5,IL)             * B(P5) *                   
     .     (ZMART(P5,IL,P5,ILP,P,L6,P6,L6)  * ZREP5 * B(P5)                     
     .     +ZMART(P5,IL,P1,ILP,P,L6,P6,L6)  * ZREP1 * B(P1))                    
     . + ZMART(P2,L2,P4,L4,P3,L3,P6,IL)             * B(P6) *                   
     .     (ZMART(P6,IL,P5,ILP,P,L6,P6,L6)  * ZREP5 * B(P5)                     
     .     +ZMART(P6,IL,P1,ILP,P,L6,P6,L6)  * ZREP1 * B(P1))                    
C                                                                               
      ELSE IF(IND.EQ.2) THEN                                                    
      AMPLIM  = AMPLIM +                                                        
     .   ZMART(P3,L3,P6,IL,P,L6,P6,L6)              * B(P6) *                   
     .     (ZMART(P2,L2,P4,L4,P6,IL,P1,ILP) * ZREP1 * B(P1)                     
     .    + ZMART(P2,L2,P4,L4,P6,IL,P5,ILP) * ZREP5 * B(P5))                    
     . + ZMART(P3,L3,P3,IL,P,L6,P6,L6)              * B(P3) *                   
     .     (ZMART(P2,L2,P4,L4,P3,IL,P1,ILP) * ZREP1 * B(P1)                     
     .    + ZMART(P2,L2,P4,L4,P3,IL,P5,ILP) * ZREP5 * B(P5))                    
C                                                                               
      ELSE IF(IND.EQ.3) THEN                                                    
      AMPLIM  = AMPLIM +                                                        
     .   ZMART(P2,L2,P6,IL,P,L6,P6,L6)              * B(P6) *                   
     .     (ZMART(P3,L3,P1,ILP,P6,IL,P4,L4) * ZREP1 * B(P1)                     
     .    + ZMART(P3,L3,P5,ILP,P6,IL,P4,L4) * ZREP5 * B(P5))                    
     . + ZMART(P2,L2,P2,IL,P,L6,P6,L6)              * B(P2) *                   
     .     (ZMART(P3,L3,P1,ILP,P2,IL,P4,L4) * ZREP1 * B(P1)                     
     .    + ZMART(P3,L3,P5,ILP,P2,IL,P4,L4) * ZREP5 * B(P5))                    
C                                                                               
      ELSE IF(IND.EQ.4) THEN                                                    
      AMPLIM  = AMPLIM +                                                        
     .   ZMART(P6,IL,P4,L4,P,L6,P6,L6)              * B(P6) *                   
     .     (ZMART(P3,L3,P1,ILP,P2,L2,P6,IL) * ZREP1 * B(P1)                     
     .    + ZMART(P3,L3,P5,ILP,P2,L2,P6,IL) * ZREP5 * B(P5))                    
     . + ZMART(P4,IL,P4,L4,P,L6,P6,L6)              * B(P4) *                   
     .     (ZMART(P3,L3,P1,ILP,P2,L2,P4,IL) * ZREP1 * B(P1)                     
     .    + ZMART(P3,L3,P5,ILP,P2,L2,P4,IL) * ZREP5 * B(P5))                    
      ENDIF                                                                     
 100  CONTINUE                                                                  
C                                                                               
C   PROPAGATORS                                                                 
      PROP1 = B(P2)*B(P4)*D(P2,P4) + 2.D0*ME2                                   
      PROP2 = B(P1)*B(P5)*D(P1,P5)                                              
      PROP3 = B(P1)*B(P5)*D(P1,P5) +                                            
     .        B(P1)*B(P6)*D(P1,P6) +                                            
     .        B(P6)*B(P5)*D(P6,P5)                                              
      PROP4 = B(P3)*B(P6)*D(P3,P6)                                              
      PROP5 = B(P1)*B(P5)*D(P1,P5) +                                            
     .        B(P1)*B(P3)*D(P1,P3) +                                            
     .        B(P3)*B(P5)*D(P3,P5) + 2.D0*ME2                                   
      PROP6 = B(P2)*B(P6)*D(P2,P6)                                              
      PROP7 = B(P4)*B(P6)*D(P4,P6)                                              
C                                                                               
      IF (IND.EQ.1) AMPLIM = AMPLIM*ZN5*ZN6/PROP1/PROP2/PROP3                   
      IF (IND.EQ.2) AMPLIM = AMPLIM*ZN5*ZN6/PROP1/PROP2/PROP4*(-1.D0)           
      IF (IND.EQ.3) AMPLIM = AMPLIM*ZN5*ZN6/PROP2/PROP5/PROP6*(-1.D0)           
      IF (IND.EQ.4) AMPLIM = AMPLIM*ZN5*ZN6/PROP2/PROP5/PROP7                   
C                                                                               
      RETURN                                                                    
      END                                                                       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
C                                                                               
C    COMPUTATION OF THE BASIC QUANTITIES                                        
C    NEEDED FOR THE HELICITY AMPLITUDES EVALUATION                              
C      INPUT   ==> VECTORS P1,P2,P3,P4,P5,P6    ( COMMON /MOMENZ/ )             
C          FORMAT: (PX,PY,PZ,E,M)                                               
C      OUTPUT  ==> BASIC QUANTITIES SP,SM,U,E,D ( COMMON /PRODUX/ )             
C                  ( SP --> S+  / SM --> S- )                                   
C                                                                               
C                              C.MANA & M.MARTINEZ   DESY-86                    
C                                                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
      SUBROUTINE SPININ(INF)                                                    
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMPLEX*16 SP,SM                                                          
      COMMON / MOMENZ / P1,P2,P3,P4,P5,P6                                       
      COMMON / PRODUX / SP,SM,U,E,D                                             
      DIMENSION P1(5),P2(5),P3(5),P4(5),P5(5),P6(5)                             
      DIMENSION Q(5,6),SP(6,6),SM(6,6),D(6,6)                                   
      DIMENSION E(6),U(6)                                                       
      EQUIVALENCE ( P1(1) , Q(1,1) )                                            
C                                                                               
      DO 1 I=1,6                                                                
      U(I) = DSQRT( 2.*( Q(4,I) - Q(1,I) )  )                                   
      E(I) = Q(5,I)/U(I)                                                        
    1 CONTINUE                                                                  
      DO 2 I=1,6                                                                
      DO 2 J=I,6                                                                
      SP(I,J)= DCMPLX( Q(2,I) , Q(3,I) ) * U(J)/U(I)                            
     .        -DCMPLX( Q(2,J) , Q(3,J) ) * U(I)/U(J)                            
      SP(J,I)=-SP(I,J)                                                          
      SM(I,J)=-DCONJG( SP(I,J) )                                                
      SM(J,I)=-SM(I,J)                                                          
      D(I,J) = SP(I,J)*SM(J,I) + (E(I)*U(J))**2 + (E(J)*U(I))**2                
      D(J,I) = D(I,J)                                                           
    2 CONTINUE                                                                  
C                                                                               
      IF(INF.LT.1) RETURN                                                       
      WRITE(6,100)                                                              
  100 FORMAT(' ',40(1H-),' SPININ INF  ',40(1H-))                               
      WRITE(6,101) (P1(I),P2(I),P3(I),P4(I),P5(I),P6(I),I=1,5)                  
  101 FORMAT('0INPUT (PX ,PY ,PZ ,E ,M ) ',/,(6G15.6))                          
      WRITE(6,102) (U(I),E(I),I=1,6)                                            
  102 FORMAT('0VECTORS U(I) AND E(I)',/,(2G15.6))                               
      WRITE(6,104) ((SP(I,J),J=1,6),I=1,6)                                      
  104 FORMAT('0MATRIX SP(I,J)',/,(6('  ',2G10.3)))                              
      WRITE(6,105) ((SM(I,J),J=1,6),I=1,6)                                      
  105 FORMAT('0MATRIX SM(I,J)',/,(6('  ',2G10.3)))                              
      WRITE(6,107) ((D(I,J),J=1,6),I=1,6)                                       
  107 FORMAT('0MATRIX D(I,J)',/,(6('  ',G15.6)))                                
      RETURN                                                                    
      END                                                                       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           
C                                                                               
C    CALCULATION OF ALL THE Z FUNCTIONS FOR GAMMA EXCHANGE                      
C                                                                               
C                               C.MANA & M.MARTINEZ   DESY-86                   
C                                                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           
      FUNCTION ZMART(P1,L1,P2,L2,P3,L3,P4,L4)                                   
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMPLEX*16 ZMART,SP,SM                                                    
      INTEGER P1,P2,P3,P4,P5,P6                                                 
      DIMENSION SP(6,6),SM(6,6),D(6,6),E(6),U(6)                                
      COMMON / PRODUX / SP,SM,U,E,D                                             
      LZ=9-4*L1-2*L2-L3-(L4+1)/2                                                
      GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),LZ                           
    1 ZMART= -2.D0*( SP(P1,P3)*SM(P2,P4)                                        
     .        - U(P1)*U(P2)*E(P3)*E(P4)                                         
     .        - U(P3)*U(P4)*E(P1)*E(P2) )                                       
      GOTO 17                                                                   
    2 ZMART= -2.D0*U(P2)*(    SP(P1,P3)*E(P4)                                   
     .                 - SP(P1,P4)*E(P3) )                                      
      GOTO 17                                                                   
    3 ZMART= -2.D0*U(P1)*(    SM(P2,P3)*E(P4)                                   
     .                 - SM(P2,P4)*E(P3) )                                      
      GOTO 17                                                                   
    4 ZMART= -2.D0*( SP(P1,P4)*SM(P2,P3)                                        
     .        - U(P1)*U(P2)*E(P3)*E(P4)                                         
     .        - U(P3)*U(P4)*E(P1)*E(P2) )                                       
      GOTO 17                                                                   
    5 ZMART= -2.D0*U(P4)*(    SP(P3,P1)*E(P2)                                   
     .                 - SP(P3,P2)*E(P1) )                                      
      GOTO 17                                                                   
    6 ZMART=(0.D0,0.D0)                                                         
      GOTO 17                                                                   
    7 ZMART=  2.D0*( E(P2)*U(P1) - E(P1)*U(P2) )                                
     .       *( E(P4)*U(P3) - E(P3)*U(P4) )                                     
      GOTO 17                                                                   
    8 ZMART=  2.D0*U(P3)*(    SP(P1,P4)*E(P2)                                   
     .                 - SP(P2,P4)*E(P1) )                                      
      GOTO 17                                                                   
    9 ZMART=  2.D0*U(P3)*(    SM(P1,P4)*E(P2)                                   
     .                 - SM(P2,P4)*E(P1) )                                      
      GOTO 17                                                                   
   10 ZMART=  2.D0*( E(P2)*U(P1) - E(P1)*U(P2) )                                
     .       *( E(P4)*U(P3) - E(P3)*U(P4) )                                     
      GOTO 17                                                                   
   11 ZMART=(0.D0,0.D0)                                                         
      GOTO 17                                                                   
   12 ZMART=  2.D0*U(P4)*(    SM(P1,P3)*E(P2)                                   
     .                 - SM(P2,P3)*E(P1) )                                      
      GOTO 17                                                                   
   13 ZMART= -2.D0*( SP(P2,P3)*SM(P1,P4)                                        
     .        - U(P1)*U(P2)*E(P3)*E(P4)                                         
     .        - U(P3)*U(P4)*E(P1)*E(P2) )                                       
      GOTO 17                                                                   
   14 ZMART= -2.D0*U(P1)*(    SP(P2,P3)*E(P4)                                   
     .                 - SP(P2,P4)*E(P3) )                                      
      GOTO 17                                                                   
   15 ZMART= -2.D0*U(P2)*(    SM(P1,P3)*E(P4)                                   
     .                 - SM(P1,P4)*E(P3) )                                      
      GOTO 17                                                                   
   16 ZMART= -2.D0*( SP(P2,P4)*SM(P1,P3)                                        
     .        - U(P1)*U(P2)*E(P3)*E(P4)                                         
     .        - U(P3)*U(P4)*E(P1)*E(P2) )                                       
   17 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
