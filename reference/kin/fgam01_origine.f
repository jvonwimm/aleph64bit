C ---------------------------------------------------------------------         
C EEGGGG : original version transmitted by P. Janot November, 18/1987.          
C          ALEPH code name FGAMxx (xx = 01,... is the version's number)         
C  -------------------------------------------------------------------          
C  *      Monte-Carlo program  for e+e- --> 4 photons                *          
C  *                                                                 *          
C  *      Author        :      P. JANOT          VERSION: 19/10/86   *          
C  *                           L.A.L.                                *          
C  *                           Universite Paris XI                   *          
C  *                           91400 Orsay - FRANCE                  *          
C  *                                                                 *          
C  *      Questions? / Problems : Contact P. Janot                   *          
C  *                         tel: 1 6446 8395 (France)               *          
C  *                                                                 *          
C  *      References : - CELLO note O-086 / November 1986.           *          
C  *                   - Berends and Kleiss. Nucl.Phys. B239,395(84) *          
C  *                                                                 *          
C  -------------------------------------------------------------------          
C                                                                               
C       *************************************************************           
C       *                                                           *           
C       *          W     R     I     T     E     U     P            *           
C       *                                                           *           
C       *************************************************************           
C                                                                               
C                                                                               
C                     EEGGGG & PJGGGG is a Monte-Carlo to simulate the          
C   e+ e-  -----> 4 photons process. It is based on the matrix element          
C   given by Berends,Kleiss et.al.. This matrix element has been com=           
C   puted in the Me = 0 limite, therefore it cannot be used when the            
C   propagators take values of the order of Me**2. A test is carried            
C   out by the Monte Carlo routine and a warning is printed when the            
C   minimal values of the propagators are not greater than 100*Me2.             
C   As an example,when the beam energy is 20 GeV,the photon energies            
C   must be larger than 20 MeV and their polar angle must satisfy               
C   1 < theta < 179 degrees.                                                    
C                                                                               
C                                                                               
C        1. DETERMINATION OF PHASE SPACE LIMITS.                                
C           ===================================                                 
C                                                                               
C       The routine allows to generate photons only in the restricted           
C   phase space which corresponds to a given detector acceptance.               
C   Therefore besides EBEAM, one has to enter :                                 
C         * The minimal energy of the photons, XMIN = Ei/EBEAM                  
C         * The polar angles limits TETMIN & TETMAX (in degrees)                
C               (TETmin < theta < TETmax)                                       
C         * The minimal acolinearity angle between two photons of any           
C         pairs : DELMIN (in degrees)                                           
C                                                                               
C *** Notice that XMIN and TETMIN should not be zero,while DELMIN can           
C   be given the value zero since the cross section has no singula=             
C   rity associated to this variable.                                           
C                                                                               
C       The routine also allows to distinguish between an end cap               
C   region and a barrel region. One may require that 3 out the 4                
C   photons be detected in the barrel defined by :                              
C            GRTMIN < theta < GRTMAX (in degrees)                               
C   togetherwith a 4th photon in the (TETMIN,TETMAX) range which                
C   includes the endcaps.                                                       
C                                                                               
C                                                                               
C        2. CHOICE OF THE INTEGRATION ROUTINE.                                  
C           =================================                                   
C                                                                               
C          2.1 By-pass of VEGAS                                                 
C              ----------------                                                 
C       The Monte Carlo routine uses a subroutine of G.P. LEPAGE                
C   (VEGAS) for the n-dimensionnal phase space integration.                     
C   Since the use of VEGAS might be at the origin of minor difficul=            
C   ties, it is possible to suppress this routine by just setting               
C   IVEG to 0 (IVEG = 0) in the main programm.                                  
C                                                                               
C          2.2 Use of VEGAS  (IVEG = 1)                                         
C              -----------------------                                          
C        What VEGAS does is to sample with an increased frequency               
C   the phase space regions where the weight varies more rapidly.               
C   In order to do so, it constructs a 'grid' in the multi-dimen=               
C   sionnal phase space which is then used to perform a non-uniform             
C   sampling of the differential cross section.                                 
C        It follows that the grid has to be prepared. This in turn              
C   requires a number of iterations (ITST), and for each these                  
C   iterations the number of sampling (IPOINT) has to be defined.               
C   A good set of values fot these 2 parameters are :                           
C           ITST   = 6 (number of preliminary iterations)                       
C           IPOINT = 20000 (number of sampling)                                 
C      (and IITT   = ITST  :total number of iterations)                         
C   They imply less than 2 mn of CPU time (IBM 3090)                            
C        The grid thus obtained is then written by UNIT 8. Later,it             
C   can be read with UNIT 7, while setting to 0 the number of                   
C   preliminary iterations (ITST=0) and choosing some value for the             
C   total number of iterations (IITT = 6, as an example)                        
C                                                                               
C          2.3 Generation of events of equal weight.                            
C              ------------------------------------                             
C        One may also generate events of weight one by using the VEGAS          
C   grid. In order to do so, one just has to give a non zero value to           
C   WMAX (maximum value of weight) in the main program. This value is           
C   computed during each run and is given in each output.                       
C                                                                               
C          2.4 Analysis.                                                        
C              --------                                                         
C         For each generated event, the kinematic variables are found           
C   in :                                                                        
C          COMMON / PHOTON / PH(4,4),WEIGHT,WMAX                                
C         PH(4,4) regroups the four photons 4-momenta:                          
C         *PH(4,I) is the energy of the Ith photon                              
C         *[PH(1,I),PH(2,I),PH(3,I)] is its momentum                            
C         The weight of the event is :                                          
C         * WEIGHT if WMAX = 0.                                                 
C         * 1.     if WMAX # 0.                                                 
C                                                                               
C         The analys program must be :                                          
C            SUBROUTINE ANALYS(IOK)                                             
C    when returning, the integer IOK should have the value :                    
C         * 1 for accepted events                                               
C         * 0 for rejected events                                               
C   -------------------------------------------------------------------         
      SUBROUTINE OLMAIN                                                         
C ----     Main program                                                         
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      EXTERNAL GGGG                                                             
      REAL*8 GGGG,X(10),ACC                                                       
      COMMON / PHOTON / PH(4,4),WEIGHT,WMAX                                     
      COMMON / DETECT / EBEAM,XMIN,TETMIN,TETMAX,GRTMIN,GRTMAX,DELMIN           
      COMMON / ALEA   / ALEA                                                    
      COMMON / EVENTS / ITOT,IBEF,ICUT,IGEN,IANA,IOK                            
      COMMON / SIGMA  / SIGMA,ERROR                                             
      COMMON / RANDOM / X                                                       
      EBEAM  = 65.0D0                                                           
      TETMIN =  1.00D0                                                          
      GRTMIN =  1.00D0                                                          
      TETMAX = 180.D0 - TETMIN                                                  
      GRTMAX = 180.D0 - GRTMIN                                                  
      XMIN   = 03.D-3                                                           
      DELMIN = 00.00D0                                                          
                                                                                
      ALEA   = 5.83D0                                                           
      NTOT   = 20000                                                            
                                                                                
      IVEG = 1                                                                  
      ITST = 0                                                                  
      IITT = 10                                                                 
      IPOINT = 40000                                                            
      WMAX = 0.2                                                                
C     WMAX = 0D-2                                                               
                                                                                
      CALL BEGIN(IVEG,ITST)                                                     
      CALL BOOK                                                                 
C                                                                               
C  ----  Main loop                                                              
100   CONTINUE                                                                  
      IF(IVEG.EQ.0) GOTO 90                                                     
      IF(ITST.EQ.0) GOTO 91                                                     
      CALL VEGAS (GGGG,IPOINT,IITT,ITST)                                        
      CALL RSAVE                                                                
      GOTO 200                                                                  
                                                                                
91    CONTINUE                                                                  
      CALL RREST                                                                
      CALL VEGAS2(GGGG,IPOINT,IITT,ITST)                                        
      CALL RSAVE                                                                
      GOTO 200                                                                  
                                                                                
90    CONTINUE                                                                  
      CALL TIMEL(TILEFT)                                                        
      IF(TILEFT.LT. 10.) GOTO 200                                               
      DO 92 IX=1,7                                                              
         X(IX) = RNDM(IX*IGEN*ALEA)                                             
92    CONTINUE                                                                  
      WEIGHT = GGGG(X)                                                          
      IF(IOK.EQ.0) GOTO 100                                                     
C ---  Analysis and final cuts                                                  
      WGH    = WEIGHT                                                           
      CALL WGHONE(WEIGHT,WGH,WMAX)                                              
      IF(WGH.NE.0.D0)                 CALL ANALYS(IOK)                          
      CALL CROSS(WEIGHT,WGH,WMAX)                                               
      IF(IANA.GE.NTOT) GOTO 200                                                 
      IF(IOK.EQ.0) GOTO 100                                                     
C     CALL HFILL(1,DLOG10(WEIGHT),1.)                                           
C     CALL REMPL(WEIGHT,0)                                                      
C ---  Calculate the cross-section                                              
C     CALL CROSS(WEIGHT,WEIGHT,0.D0)                                            
      GOTO 100                                                                  
                                                                                
200   CONTINUE                                                                  
      CALL FINISH(IVEG)                                                         
      STOP 13                                                                   
      END                                                                       
                                                                                
      SUBROUTINE BOOK                                                           
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      CALL HBOOK1(10000,'LOG(WEIGHT)',50  ,-10.,5.,0.)                          
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE ANALYS(IOK)                                                    
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      COMMON / PHOTON / PH(4,4),WEIGHT,WMAX                                     
C     IF(WEIGHT.GT.WMAX) PRINT 1 ,PH                                            
C     IF(WMAX.NE.0.D0) WRITE(10) PH                                             
      WRITE(10,1) ph                                                            
 1    FORMAT(4(1X,4(D14.6,2X)/)//)                                              
      IOK=1                                                                     
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE BEGIN(IVEG,ITST)                                               
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      REAL S1,S2,S3,S4                                                          
C     COMMON / / HMEMOR(10000)                                                  
      COMMON / DETECT / EBEAM,XMIN,TETMIN,TETMAX,GRTMIN,GRTMAX,DELMIN           
      COMMON / ENERGY / SBEAM,SQS,EMIN,MA2MIN,MA2MAX,MB2MIN,MB2MAX              
      COMMON / LIMITE / D12MAX,TAMMIN,TBPMIN,TAMMAX,TBPMAX                      
     *                 ,T1MMIN,T1MMAX,T1PMIN,T1PMAX,D12MIN                      
      COMMON / BEAM   / PMINUS(4),PPLUS(4)                                      
      COMMON / ANGLES / CMIN,CMAX,CMIN2,CMAX2,ALPH                              
      COMMON / FLUX   / FLUX                                                    
      COMMON / USEFUL / PI,DEGRAD                                               
      COMMON / ALEA   / ALEA                                                    
      COMMON / EVENTS / ITOT,IBEF,ICUT,IGEN,IANA,IOK                            
      COMMON / PRINT / IPRINT                                                   
      COMMON / RESULT / S1,S2,S3,S4                                             
      COMMON / SIGMA  / SIGMA,ERROR                                             
      COMMON / WNEW / WNEW,WMAX1,wmax2,IWERR(1000)                              
      COMMON / PHOTON / PH(4,4),WEIGHT ,WMAX                                    
      IPRINT = 0                                                                
      ITOT   = 0                                                                
      IBEF   = 0                                                                
      ICUT   = 0                                                                
      IGEN   = 0                                                                
      IANA   = 0                                                                
      SIGMA  = 0.D0                                                             
      ERROR  = 0.D0                                                             
      WNEW   = 0.D0                                                             
      CALL VZERO(IWERR(1),1000)                                                 
      S1     = 0.D0                                                             
      S2     = 0.D0                                                             
      S3     = 0.D0                                                             
      S4     = 0.D0                                                             
C     IF(IVEG.EQ.0.OR.ITST.NE.0) WMAX = 0.D0                                    
      WMAX1  = WMAX                                                             
                                                                                
      PI     =  DATAN(1.D0)*4.D0                                                
      DEGRAD =  PI/180.D0                                                       
      RADDEG =  1.D0/DEGRAD                                                     
      CMIN   =  DCOS(TETMIN*DEGRAD)                                             
      CMAX   =  DCOS(TETMAX*DEGRAD)                                             
      CMIN2 = DCOS(GRTMIN*DEGRAD)                                               
      CMAX2 = DCOS(GRTMAX*DEGRAD)                                               
      ALPH = DELMIN*DEGRAD                                                      
      SMIN   =  1.D0 - CMIN                                                     
      SMAX   =  1.D0 + CMAX                                                     
      SBEAM  =  4.D0 * EBEAM**2                                                 
      SQS    =  2. * EBEAM                                                      
      EMIN   =  XMIN * EBEAM                                                    
      MA2MAX =  SBEAM * (1.D0-XMIN)**2                                          
      D12MAX =  DMAX1(-SBEAM*XMIN*(1.-XMIN),                                    
     *     -SBEAM*XMIN*(1.-CMIN2-XMIN*(1.-DCOS(2.*GRTMIN*DEGRAD))/2.))          
      D12MAX =  DMAX1(D12MAX,                                                   
     *     -SBEAM*XMIN*(1.+CMAX2-XMIN*(1.-DCOS(2.*GRTMAX*DEGRAD))/2.))          
      D12MAX =  DMAX1(D12MAX,                                                   
     *     -SBEAM*XMIN/2.*(2.-CMIN-CMIN2                                        
     *                     -XMIN*(1.-DCOS(DEGRAD*(TETMIN+GRTMIN)))))            
      D12MAX =  DMAX1(D12MAX,                                                   
     *     -SBEAM*XMIN/2.*(2.+CMAX+CMAX2                                        
     *                     -XMIN*(1.-DCOS(DEGRAD*(TETMAX+GRTMAX)))))            
      D12MAX = DMAX1(D12MAX,-SBEAM*((1.+CMAX2)/2.-                              
     *                              (1.-DCOS(2.*GRTMAX*DEGRAD))/8.))            
      D12MAX = DMAX1(D12MAX,-SBEAM*((1.-CMIN2)/2.-                              
     *                              (1.-DCOS(2.*GRTMIN*DEGRAD))/8.))            
      D12MAX = DMAX1(D12MAX,-SBEAM*((2.+CMAX2+CMAX)/4.-                         
     *                      (1.-DCOS((TETMAX+GRTMAX)*DEGRAD))/8.))              
      D12MAX = DMAX1(D12MAX,-SBEAM*((2.-CMIN2-CMIN)/4.-                         
     *                      (1.-DCOS((TETMIN+GRTMIN)*DEGRAD))/8.))              
      EENDCP = SQS*CMIN2/CMIN/(1.D0+CMIN2/CMIN)                                 
      D12MIN = -SQS*(EBEAM*(1+CMIN2)+EMIN*(1+CMIN))                             
     *           +2.D0*EMIN*EBEAM*(1.D0-DCOS((GRTMIN+TETMIN)*DEGRAD))           
      D12MIN = DMIN1(D12MIN,                                                    
     *         -SQS*(EMIN*(1+CMIN2)+EENDCP*(1+CMIN))                            
     *           +2.D0*EMIN*EENDCP*(1.D0-DCOS((GRTMIN+TETMIN)*DEGRAD)))         
      TAMMAX = -SQS*EMIN*(2.D0+CMAX+CMAX2)                                      
      TAMMIN = -SBEAM - TAMMAX                                                  
      TBPMAX = -SQS*EMIN*(2.D0-CMIN-CMIN2)                                      
      TBPMIN = -SBEAM - TBPMAX                                                  
      T1MMIN = -SQS*EBEAM*(1.D0+CMIN)                                           
      T1MMAX = -SQS*EMIN*(1.D0+CMAX)                                            
      T1PMIN = -SQS*EBEAM*(1.D0-CMAX)                                           
      T1PMAX = -SQS*EMIN*(1.D0-CMIN)                                            
      D12MIN = T1MMIN                                                           
      NTOT   =  0                                                               
      PMINUS(4) =  EBEAM                                                        
      PMINUS(3) = -EBEAM                                                        
      PMINUS(2) =  0.D0                                                         
      PMINUS(1) =  0.D0                                                         
      PPLUS (4) =  EBEAM                                                        
      PPLUS (3) = +EBEAM                                                        
      PPLUS (2) =  0.D0                                                         
      PPLUS (1) =  0.D0                                                         
                                                                                
      PRINT 99,EBEAM,XMIN,EMIN,TETMIN,TETMAX,CMAX,CMIN,                         
     *                         GRTMIN,GRTMAX,CMAX2,CMIN2                        
99    FORMAT(1X,' ------- MONTE CARLO E+E- ----> 4 PHOTONS ------'//            
     *       1X,'    THE INITIAL CONDTIONS ARE :                 '//            
     *       1X,'       BEAM ENERGY          :  ',D10.4,' GEV'/                 
     *       1X,'       LOWEST PHOTON ENERGY :  ',D10.4,' EBEAM'/               
     *       1X,'                             ( ',D10.4,' GEV )'/               
     *       1X,'     FOR THE 4 PHOTONS :'/                                     
     *       1X,'       POLAR ANGLES BETWEEN :  ',D10.4,' DEGREES'/             
     *       1X,'                           AND ',D10.4,' DEGREES'/             
     *       1X,'       ( COSINE BETWEEN     :  ',D10.4,' AND '/                
     *       1X,'                               ',D10.4,' )'/                   
     *       1X,'     FOR THREE PHOTONS :'/                                     
     *       1X,'       POLAR ANGLES BETWEEN :  ',D10.4,' DEGREES'/             
     *       1X,'                           AND ',D10.4,' DEGREES'/             
     *       1X,'       ( COSINE BETWEEN     :  ',D10.4,' AND '/                
     *       1X,'                               ',D10.4,' )'///)                
                                                                                
      ALPHA  =  1./137.0360411D0                                                
      ALPHA2 =  20.73530702D0                                                   
      FLUX   =  4.D0 * ALPHA**2 * ALPHA2 / ((4.D0*PI)**4*SBEAM**2)              
      XME2   =  (.511D-3)**2                                                    
      IRUN   =  1                                                               
      TEST1  =  -T1MMAX/XME2                                                    
      TEST2  =  -T1PMAX/XME2                                                    
      TEST3  =  -D12MAX/XME2                                                    
                                                                                
      IF(TEST1.LT.100..OR.TEST2.LT.100..OR.TEST3.LT.100.) PRINT 219             
219   FORMAT(1X,'***************************************************'/          
     *       1X,'***                                             ***'/          
     *       1X,'***       W    A    R    N    I    N    G       ***'/          
     *       1X,'***                                             ***'/          
     *       1X,'***                                             ***'/          
     *       1X,'***   YOUR INITIAL CONDITIONS ARE TOO HARD:     ***'/          
     *       1X,'***   THE MATRIX ELEMENT HAS BEEN CALCULATED    ***'/          
     *       1X,'***  WITH THE MASSLESS ELECTRON APPROXIMATION.  ***'/          
     *       1X,'***  THIS APPROXIMATION BECOMES WRONG IN THIS   ***'/          
     *       1X,'***  CASE, THE FOLLOWING RESULTS TOO.           ***'/          
     *       1X,'***                                             ***'/          
     *       1X,'***************************************************'//)        
                                                                                
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
      FUNCTION GGGG(X)                                                          
      IMPLICIT REAL*8(A-H,J-Z)   
      REAL*4 RNDM                                                
      dimension X(10)                                                           
      EXTERNAL MATRIX                                                           
      COMMON / ALEA   / ALEA                                                    
      COMMON / FLUX   / FLUX                                                    
      COMMON / PHOTON / K1(4),K2(4),K3(4),K4(4),WEIGHT,WMAX                     
      COMMON / PRINT / IPRINT                                                   
      COMMON / EVENTS / ITOT,IBEF,ICUT,IGEN,IANA,IOK                            
      COMMON / SIGMA  / SIGMA,ERROR                                             
      COMMON / TEST / DJTEST                                                    
C ---                                                                           
C ---   The Program generate the eight variables :                              
C ---                                                                           
C ---    The physical variables are :                                           
C --- p- ___________________ k1    1)Propagator 1 : t1_=(p_-k1)**2              
C ---             !1               2)Propagator 2 : t4+=(p+-k4)**2              
C ---             !_________ k2    3)Propagator 3 : d12=(p_-k1-k2)**2           
C ---             !3               4)Inv.Mass 1-2 : ta_= t1_ + t2_              
C ---             !_________ k3    5)Inv.Mass 3-4 : tb+= t3+ + t4+              
C ---             !2               6),7)& 8) 3 azimuthal angles:                
C --- P+ _________!_________ k4      * Phi1 in R(1,2)                           
C ---                                * Phi4 in R(3,4)                           
C ---                                * Phia in Lab (pa=k1+k2)                   
C ---                                                                           
C ---                                                                           
C ---    The random variables x1,...,x8 are defined below                       
C ---                                                                           
C ---       dx1 = d(d12)/d12/(s+d12)                                            
C ---       dx2 = d(ta_)/ta_/(s+ta_)                                            
C ---       dx3 = d(tb+)/tb+/(s+tb+)                                            
C ---       dx4 = d(t1_)/t1_/(s+t1_)                                            
C ---       dx5 = d(t4+)/t4+/(s+t4+)                                            
C ---       dx6 = d(phi1)                                                       
C ---       dx7 = d(phi4)                                                       
C ---       dx8 = d(phia)                                                       
C ---                                                                           
C ---    Finaly, x9 determinates the permutation i1,i2,i3,i4 of 1,2,3,4         
C ---                                                                           
      IOK    = 0                                                                
      GGGG = 0.                                                                 
      DO 1 IX = 8,9                                                             
          X(IX) = RNDM(IX*IGEN)                                            
 1    CONTINUE                                                                  
C ---     1) Generate the eight quantities described above                      
      ITOT = ITOT + 1                                                           
      CALL GENER(X)                                                             
      IF(IOK.EQ.0) RETURN                                                       
C ---                                                                           
C ---     2) Calculate the 4 four-vectors k1,k2,k3,k4 and all                   
C ---   the useful kinematics quantities                                        
      IBEF = IBEF+1                                                             
      CALL PICK4                                                                
      IF(IOK.EQ.0) RETURN                                                       
      ICUT = ICUT+1                                                             
C ---                                                                           
C----     3) Calculate all the Jacobians                                        
      CALL DJACOB(DJDY)                                                         
      DJDY = DJDY * DJTEST                                                      
C ---                                                                           
C ---     4) Calculate the exact matrix element squared  and                    
C ---        the sum of the approximate amplitudes squared                      
      CALL MATRIX(EXACT,APPROX)                                                 
      RATIO  = EXACT/APPROX                                                     
C ---                                                                           
C ---     5) Calculate the weight of the set of 4-vectors                       
      WGHT = DJDY * RATIO  * FLUX                                               
C ---                                                                           
C ---     6) Permutation choice of (1,2,3,4)                                    
      CALL PERMUT(X)                                                            
C ---                                                                           
      GGGG = WGHT                                                               
      IOK  = 1                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE GENER(X)                                                       
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      dimension X(10)                                                                
      COMMON / DETECT / EBEAM,XMIN,TETMIN,TETMAX,GRTMIN,GRTMAX,DELMIN           
      COMMON / ENERGY / SBEAM,SQS,EMIN,MA2MIN,MA2MAX,MB2MIN,MB2MAX              
      COMMON / ANGLES / CMIN,CMAX,CMIN2,CMAX2,ALPH                              
      COMMON / FLUX   / FLUX                                                    
      COMMON / USEFUL / PI,DEGRAD                                               
      COMMON / INVAR  / MA2,MB2                                                 
      COMMON / TETFI  / D12,T1M,T4P,FA,F1,F4                                    
      COMMON / PART23 / T2M                                                     
      COMMON / PRINT  / IPRINT                                                  
      COMMON / EVENTS / ITOT,IBEF,ICUT,IGEN,IANA,IOK                            
      COMMON / LIMITE / D12MAX,TAMMIN,TBPMIN,TAMMAX,TBPMAX                      
     *                 ,T1MMIN,T1MMAX,T1PMIN,T1PMAX,D12MIN                      
      COMMON / DJDXNN / DJDX(8)                                                 
      COMMON / TEST / DJTEST                                                    
      IOK = 0                                                                   
C ----------------------------------------- GENERATE D12 VALUE -------          
      D12    = HAS(X(1),D12MIN,D12MAX,3,SBEAM,DJDX(1))                          
      IF(IPRINT.GE.8) PRINT 144,D12MIN,D12MAX,D12                               
144   FORMAT(1X,'D12MIN/MAX/D12 : ',3(D12.6,2X))                                
C ----------------------------------------- GENERATE MA2 VALUE -------          
      MA2MX  = DMIN1(MA2MAX,D12-TAMMIN)                                         
      MA2MN  = DMAX1(0.D0,D12-TAMMAX)                                           
C --- Avoid events out of phase space                                           
      IF(MA2MX.LT.MA2MN) RETURN                                                 
      UMIN = DMIN1(8.D-1,1.D-1*(1.D0-CMIN))                                     
      U   = HAS(X(2),UMIN,1.D0,1,0.D0,DJTES2)                                   
      DJTEST = DJTES2 * U / (1.D0-UMIN)                                         
      Y2 = (1.D0-U)/(1.D0-UMIN)                                                 
      MA2 = D12-HAS(Y2,D12-MA2MX,D12-MA2MN,3,SBEAM,DJDX(2))                     
      IF(IPRINT.GE.8) PRINT 145,MA2MN,MA2MX,MA2                                 
145   FORMAT(1X,'MA2MIN/MAX/MA2 : ',3(D12.6,2X))                                
C ----------------------------------------- GENERATE MB2 VALUE -------          
      EAMIN  = DSQRT(MA2)                                                       
      EAMAX  = DMIN1((SBEAM+MA2)/(2.D0*SQS),SQS-2.D0*EMIN)                      
      TAM    = D12-MA2                                                          
      TBPMN  = DMAX1(TBPMIN,-SBEAM+TAM+2.D0*SQS*EAMIN)                          
      TBPMX  = DMIN1(TBPMAX,-SBEAM+TAM+2.D0*SQS*EAMAX)                          
      MB2MAX = DMIN1(SBEAM+MA2-2.D0*SQS*EAMIN,MA2MAX)                           
      MB2MAX = DMIN1(MB2MAX,D12-TBPMN)                                          
      MB2MAX = DMIN1(MB2MAX,D12*(TAM+SBEAM)/TAM)                                
      MB2MIN = DMAX1(0.D0,D12-TBPMX)                                            
C --- Avoid events out of phase space                                           
      IF(MB2MAX.LT.MB2MIN) RETURN                                               
      YMIN = DMIN1(2.D-1,50.D0*(1.D0-CMIN))                                     
      YMAX = DMAX1(9.D-1,1.D0-25.D0*(1.D0-CMIN))                                
      Y3 = HAS(X(3),YMIN,YMAX,3,-1.D0,DJTES3)                                   
      DJTEST = DJTEST / (YMAX-YMIN) * Y3 * (Y3-1.D0) * DJTES3                   
      Y3 = (Y3-YMIN)/(YMAX-YMIN)                                                
      MB2=D12-HAS(Y3,D12-MB2MAX,D12-MB2MIN,3,SBEAM,DJDX(3))                     
      IF(IPRINT.GE.8) PRINT 146,MB2MIN,MB2MAX,MB2                               
146   FORMAT(1X,'MB2MIN/MAX/MB2 : ',3(D12.6,2X))                                
C ----------------------------------------- GENERATE T1_ VALUE -------          
      EA    = (SBEAM-MB2+MA2)/(2.D0*SQS)                                        
      PMA   = DSQRT(EA**2-MA2)                                                  
      CA    = (-EA + (MA2-D12)/SQS)/PMA                                         
      BETAZ = PMA*CA/EA                                                         
      T1MMN = -SQS*EA*(1.D0+BETAZ)                                              
      E1MAX = DMIN1(EBEAM,-TAM/(SQS*(1.D0+CMAX)))                               
      E1MAX = DMIN1(E1MAX,EA-EMIN)                                              
      E1MIN = DMAX1(EMIN,EA-E1MAX)                                              
      E1MIN = DMAX1(E1MIN,MA2/(4.D0*E1MAX))                                     
      E1MAX = DMIN1(E1MAX,EA-E1MIN)                                             
      T1MMX = -SQS*E1MIN*(1.+CMAX)                                              
      T1MMN = DMAX1(-SQS*E1MAX*(1.+CMIN),T1MMN)                                 
      T1MMN = DMAX1(T1MMN,TAM-T1MMX)                                            
      T1MMX = DMIN1(T1MMX,TAM-T1MMN)                                            
C --- Avoid events out of phase space                                           
      IF(T1MMN.GT.T1MMX) RETURN                                                 
      T1M   = HAS(X(4),T1MMN,T1MMX,3,SBEAM,DJDX(4))                             
      T2M   = TAM - T1M                                                         
      IF(IPRINT.GE.8) PRINT 147,T1MMN,T1MMX,T1M,T2M                             
147   FORMAT(1X,'T1MMIN/MAX/T1M/T2M : ',4(D12.6,2X))                            
C ----------------------------------------- GENERATE T4+ VALUE -------          
      TBP   = D12-MB2                                                           
      EB    = SQS-EA                                                            
      BETAZ = -PMA*CA/EB                                                        
      T4PMN = -SQS*EB*(1.D0-BETAZ)                                              
      E4MAX = DMIN1(EBEAM,-TBP/(SQS*(1.D0-CMIN)))                               
      E4MAX = DMIN1(E4MAX,EB-EMIN)                                              
      E4MIN = DMAX1(EMIN,EB-E4MAX)                                              
      E4MIN = DMAX1(E4MIN,MB2/(4.D0*E4MAX))                                     
      E4MAX = DMIN1(E4MAX,EB-E4MIN)                                             
      T4PMX = -SQS*E4MIN*(1.-CMIN)                                              
      T4PMN = DMAX1(-SQS*E4MAX*(1.-CMAX),T4PMN)                                 
      T4PMN = DMAX1(T4PMN,TBP-T4PMX)                                            
      T4PMX = DMIN1(T4PMX,TBP-T4PMN)                                            
C --- Avoid events out of phase space                                           
      IF(T4PMN.GT.T4PMX) RETURN                                                 
      T4P   = HAS(X(5),T4PMN,T4PMX,3,SBEAM,DJDX(5))                             
      T3P   = TBP - T4P                                                         
      IF(IPRINT.GE.8) PRINT 148,T4PMN,T4PMX,T4P,T3P                             
148   FORMAT(1X,'T4PMIN/MAX/T4P/T3P : ',4(D12.6,2X))                            
C ----------------------------------------- GENERATE  F1 VALUE -------          
      F1    = HAS (X(6),0.D0,2.D0*PI,0,0.,DJDX(6))                              
C ----------------------------------------- GENERATE  F4 VALUE -------          
      F4    = HAS (X(7),0.D0,2.D0*PI,0,0.,DJDX(7))                              
C ----------------------------------------- GENERATE  FA VALUE -------          
      FA     = HAS(X(8),0.D0,2.D0*PI,0,0.,DJDX(8))                              
      IOK = 1                                                                   
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE PICK4                                                          
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      DIMENSION PA(4),PB(4),T(4,4)                                              
      COMMON / BEAM   / PMINUS(4),PPLUS(4)                                      
      COMMON / PHOTON / K1(4),K2(4),K3(4),K4(4),WEIGHT,WMAX                     
      COMMON / DETECT / EBEAM,XMIN,TETMIN,TETMAX,GRTMIN,GRTMAX,DELMIN           
      COMMON / ENERGY / SBEAM,SQS,EMIN,MA2MIN,MA2MAX,MB2MIN,MB2MAX              
      COMMON / ANGLES / CMIN,CMAX,CMIN2,CMAX2,ALPH                              
      COMMON / FLUX   / FLUX                                                    
      COMMON / USEFUL / PI,DEGRAD                                               
      COMMON / INVAR  / MA2,MB2                                                 
      COMMON / TETFI  / D12,T1M,T4P,FA,F1,F4                                    
      COMMON / PRINT / IPRINT                                                   
      COMMON / EVENTS / ITOT,IBEF,ICUT,IGEN,IANA,IOK                            
      COMMON / VAR    / TM(4),TP(4),D(4,4),M(4,4)                               
      DIMENSION COST(4),PP(4),PM(4)                                             
      IOK = 0                                                                   
      NREP = 0                                                                  
      DO 128 IVECT=1,4                                                          
         PM(IVECT)=PMINUS(IVECT)                                                
         PP(IVECT)=PPLUS (IVECT)                                                
128   CONTINUE                                                                  
                                                                                
C ----  Determination of pa & pb in LAB                                         
      EA     = (SBEAM-MB2+MA2)/(2.D0*SQS)                                       
      PMA    = DSQRT(EA**2-MA2)                                                 
      CA     = (-EA + (MA2-D12)/SQS)/PMA                                        
      IF(IPRINT.GE.9) PRINT 145,EA,PMA,CA                                       
145   FORMAT(1X,'EA/PMA/CA : ',3(D12.6,2X))                                     
      SA     = DSQRT(1.D0-CA**2)                                                
      PA(4)  = EA                                                               
      PA(3)  = PMA * CA                                                         
      PA(2)  = PMA * SA                                                         
      PA(1)  = 0.D0                                                             
      PB(4)  = SQS-EA                                                           
      PB(3)  = -PMA * CA                                                        
      PB(2)  = -PMA * SA                                                        
      PB(1)  = 0.D0                                                             
                                                                                
                                                                                
C ---- Determination of Cos1 in Ra                                              
      BETAZA = PA(3)/PA(4)                                                      
      BETAYA = PA(2)/PA(4)                                                      
      BETAXA = PA(1)/PA(4)                                                      
      CALL LORENZ(BETAXA,BETAYA,BETAZA,PM)                                      
      C1     = T1M/(DSQRT(MA2)*PM(4))+1.D0                                      
      S1     = DSQRT(1.D0-C1**2)                                                
                                                                                
C ---- Determination of Cos4 in Rb                                              
      BETAZB = PB(3)/PB(4)                                                      
      BETAYB = PB(2)/PB(4)                                                      
      BETAXB = PB(1)/PB(4)                                                      
      CALL LORENZ(BETAXB,BETAYB,BETAZB,PP)                                      
      C4     = T4P/(DSQRT(MB2)*PP(4))+1.D0                                      
      S4     = DSQRT(1.D0-C4**2)                                                
                                                                                
C ---- Determination of k1 & k2 in Ra                                           
      K1(4)  = DSQRT(MA2)/2.D0                                                  
      K1(3)  = K1(4) * C1                                                       
      K1(2)  = K1(4) * S1 * DSIN(F1)                                            
      K1(1)  = K1(4) * S1 * DCOS(F1)                                            
      K2(4)  = K1(4)                                                            
      K2(3)  =-K1(3)                                                            
      K2(2)  =-K1(2)                                                            
      K2(1)  =-K1(1)                                                            
      CALL ROTX(PM(3)/PM(4),PM(2)/PM(4),K1)                                     
      CALL ROTX(PM(3)/PM(4),PM(2)/PM(4),K2)                                     
                                                                                
C ---- Determination of k3 & k4 in Rb                                           
      K4(4)  = DSQRT(MB2)/2.D0                                                  
      K4(3)  = K4(4) * C4                                                       
      K4(2)  = K4(4) * S4 * DSIN(F4)                                            
      K4(1)  = K4(4) * S4 * DCOS(F4)                                            
      K3(4)  = K4(4)                                                            
      K3(3)  =-K4(3)                                                            
      K3(2)  =-K4(2)                                                            
      K3(1)  =-K4(1)                                                            
      CALL ROTX(PP(3)/PP(4),PP(2)/PP(4),K3)                                     
      CALL ROTX(PP(3)/PP(4),PP(2)/PP(4),K4)                                     
                                                                                
C ---- re-writing of k1,..,k4 in the LAB                                        
      CALL LORENZ(-BETAXA,-BETAYA,-BETAZA,K1)                                   
      CALL LORENZ(-BETAXA,-BETAYA,-BETAZA,K2)                                   
      CALL LORENZ(-BETAXB,-BETAYB,-BETAZB,K3)                                   
      CALL LORENZ(-BETAXB,-BETAYB,-BETAZB,K4)                                   
                                                                                
      ETOT = K1(4)+K2(4)+K3(4)+K4(4)                                            
      PTOTZ= K1(3)+K2(3)+K3(3)+K4(3)                                            
      PTOTY= K1(2)+K2(2)+K3(2)+K4(2)                                            
      PTOTX= K1(1)+K2(1)+K3(1)+K4(1)                                            
      IF(IPRINT.GE.9) PRINT 2,K1,K2,K3,K4,ETOT,PTOTX,PTOTY,PTOTZ                
2     FORMAT(4(1X,4(D14.8,2X)/)/1X,'   ETOT : ',D14.8/                          
     *       1X,'PTOT : ',3(D14.8,2X))                                          
C ---- Avoid events out of detector                                             
      IF(K1(4).LT.EMIN.OR.                                                      
     *   K2(4).LT.EMIN.OR.                                                      
     *   K3(4).LT.EMIN.OR.                                                      
     *   K4(4).LT.EMIN) RETURN                                                  
                                                                                
      COST(1) = K1(3)/K1(4)                                                     
      COST(2) = K2(3)/K2(4)                                                     
      COST(3) = K3(3)/K3(4)                                                     
      COST(4) = K4(3)/K4(4)                                                     
      IF(IPRINT.GE.9) PRINT 368,COST                                            
368   FORMAT(1X,'COST(I) : ',4(D12.6,2X))                                       
      DO 22 IPH=1,4                                                             
         IF(COST(IPH).GT.CMIN.OR.COST(IPH).LT.CMAX) RETURN                      
22    CONTINUE                                                                  
                                                                                
      IBAR = 0                                                                  
      DO 23 IPH=1,4                                                             
      IF(COST(IPH).LT.CMIN2.AND.COST(IPH).GT.CMAX2) IBAR = IBAR+1               
23    CONTINUE                                                                  
      IF(IBAR.LT.3) RETURN                                                      
                                                                                
      CFA  = DCOS(FA)                                                           
      SFA  = DSIN(FA)                                                           
      CALL ROTZ(CFA,SFA,K1)                                                     
      CALL ROTZ(CFA,SFA,K2)                                                     
      CALL ROTZ(CFA,SFA,K3)                                                     
      CALL ROTZ(CFA,SFA,K4)                                                     
                                                                                
      M(1,2)=2.D0*PROSCA(K1,K2)                                                 
      M(1,3)=2.D0*PROSCA(K1,K3)                                                 
      M(1,4)=2.D0*PROSCA(K1,K4)                                                 
      M(2,3)=2.D0*PROSCA(K2,K3)                                                 
      M(2,4)=2.D0*PROSCA(K2,K4)                                                 
      M(3,4)=2.D0*PROSCA(K3,K4)                                                 
      T(1,2)=ACOS(1.D0-M(1,2)/(2.D0*K1(4)*K2(4)))                             
      T(1,3)=ACOS(1.D0-M(1,3)/(2.D0*K1(4)*K3(4)))                             
      T(1,4)=ACOS(1.D0-M(1,4)/(2.D0*K1(4)*K4(4)))                             
      T(2,3)=ACOS(1.D0-M(2,3)/(2.D0*K2(4)*K3(4)))                             
      T(2,4)=ACOS(1.D0-M(2,4)/(2.D0*K2(4)*K4(4)))                             
      T(3,4)=ACOS(1.D0-M(3,4)/(2.D0*K3(4)*K4(4)))                             
                                                                                
      DO 129 I1=1,3                                                             
         DO 229 I2=I1,3                                                         
            IF(T(I1,I2+1).LT.ALPH) RETURN                                       
229      CONTINUE                                                               
129   CONTINUE                                                                  
                                                                                
C ------ Determination of tm and d                                              
      TM(1)=-2.D0*PROSCA(PMINUS,K1)                                             
      TM(2)=-2.D0*PROSCA(PMINUS,K2)                                             
      TM(3)=-2.D0*PROSCA(PMINUS,K3)                                             
      TM(4)=-2.D0*PROSCA(PMINUS,K4)                                             
      TP(1)=-2.D0*PROSCA(PPLUS ,K1)                                             
      TP(2)=-2.D0*PROSCA(PPLUS ,K2)                                             
      TP(3)=-2.D0*PROSCA(PPLUS ,K3)                                             
      TP(4)=-2.D0*PROSCA(PPLUS ,K4)                                             
                                                                                
      DO 192 ID=1,4                                                             
         D(ID,ID)=0.D0                                                          
         M(ID,ID)=0.D0                                                          
         DO 193 IE=ID,4                                                         
            IF(ID.EQ.IE) GOTO 193                                               
            D(ID,IE)=TM(ID)+TM(IE)+M(ID,IE)                                     
            D(IE,ID)=D(ID,IE)                                                   
            M(IE,ID)=M(ID,IE)                                                   
193      CONTINUE                                                               
192   CONTINUE                                                                  
                                                                                
      IF(IPRINT.GE.8) PRINT 151,M,TM,TP,D                                       
151   FORMAT(1X,'MASSES : '/4(1X,4(D12.6,2X)/)/                                 
     *       1X,'TM : '/1X,4(D12.6,2X)/                                         
     *       1X,'TP : '/1X,4(D12.6,2X)/                                         
     *       1X,'D  : '/4(1X,4(D12.6,2X)/))                                     
      IOK = 1                                                                   
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
      SUBROUTINE LORENZ(BETAX,BETAY,BETAZ,P)                                    
C ---- This routine is a Lorentz transformation of the 4-vector p               
C ---- from R1 to R2 . Beta(x,y,z) is R2's velocity/ R1.                        
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      DIMENSION P(4)                                                            
      BETA2 = BETAX**2 + BETAY**2 + BETAZ**2                                    
      GAMMA = 1.D0/DSQRT(1.D0-BETA2)                                            
      ONE   = BETAX*P(1) + BETAY*P(2) + BETAZ*P(3)                              
      TWO   = (GAMMA-1.D0)*ONE/BETA2- GAMMA*P(4)                                
      P(1)  = P(1) + BETAX*TWO                                                  
      P(2)  = P(2) + BETAY*TWO                                                  
      P(3)  = P(3) + BETAZ*TWO                                                  
      P(4)  = GAMMA*(-ONE+P(4))                                                 
      RETURN                                                                    
      END                                                                       
                                                                                
      DOUBLE PRECISION FUNCTION HAS(X,XMIN,XMAX,IEX,LIM,DJDX)                   
      IMPLICIT REAL*8(A-H,J-Z)
      HAS = 0.                                                  
      IF(IEX.NE.0) GOTO 1                                                       
      HAS  = XMIN + (XMAX - XMIN) * X                                           
      DJDX = XMAX - XMIN                                                        
      RETURN                                                                    
1     CONTINUE                                                                  
      IF(IEX.NE.1) GOTO 2                                                       
      HAS  = XMIN * (XMAX / XMIN) ** X                                          
      DJDX = DLOG(XMAX/XMIN)                                                    
      RETURN                                                                    
2     CONTINUE                                                                  
      IF(IEX.NE.2) GOTO 3                                                       
      HAS  = 1.D0 / (1.D0/XMIN + (1.D0/XMAX - 1.D0/XMIN) * X)                   
      DJDX = 1.D0/XMIN - 1.D0/XMAX                                              
      RETURN                                                                    
3     CONTINUE                                                                  
      IF(IEX.NE.3) RETURN                                                       
      AUXMIN = XMIN/(LIM+XMIN)                                                  
      AUXMAX = XMAX/(LIM+XMAX)                                                  
      AUX    = AUXMIN*(AUXMAX/AUXMIN)**X                                        
      HAS    = LIM*AUX/(1.D0-AUX)                                               
      DJDX   = 1.D0/LIM*DLOG(AUXMAX/AUXMIN)                                     
      RETURN                                                                    
      END                                                                       
                                                                                
      DOUBLE PRECISION FUNCTION HAS2(X,X1MIN,X1MAX,                             
     *                                 X2MIN,X2MAX,DJDX)                        
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      DJDX1= X1MAX - X1MIN                                                      
      DJDX2= X2MAX - X2MIN                                                      
C     PRINT 9,DJDX1,DJDX2                                                       
C9    FORMAT(1X,'DJDX1/2 : ',2(D12.6,2X))                                       
      DJDX = DJDX1 + DJDX2                                                      
      IF(X.LT.DJDX1/DJDX) HAS2 =         X1MIN  + DJDX * X                      
      IF(X.GT.DJDX1/DJDX) HAS2 = (-DJDX1+X2MIN) + DJDX * X                      
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE DJACOB(DJDY)                                                   
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      COMMON / ENERGY / SBEAM,SQS,EMIN,MA2MIN,MA2MAX,MB2MIN,MB2MAX              
      COMMON / LIMITE / D12MAX,TAMMIN,TBPMIN,TAMMAX,TBPMAX                      
     *                 ,T1MMIN,T1MMAX,T1PMIN,T1PMAX,D12MIN                      
      COMMON / INVAR  / MA2,MB2                                                 
      COMMON / USEFUL / PI,DEGRAD                                               
      COMMON / DJDXNN / DJDX(8)                                                 
      COMMON / TETFI  / D12,T1M,T4P,FA,F1,F4                                    
      DJDY = 1.D0                                                               
      DO 1 I=1,8                                                                
         DJDY = DJDY*DJDX(I)                                                    
1     CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE MATRIX(EXACT,APPROX)                                           
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      COMPLEX*16 KP(4),KM(4),KT(4),KS(4),Z(4,4),ZS(4,4),MSQUAC                  
      EXTERNAL FMAT                                                             
      COMMON / ENERGY / SBEAM,SQS,EMIN,MA2MIN,MA2MAX,MB2MIN,MB2MAX              
      COMMON / USEFUL / PI,DEGRAD                                               
      COMMON / PHOTON / PH(4,4),WEIGHT,WMAX                                     
      COMMON / PRINT  / IPRINT                                                  
      COMMON / VAR    / TM(4),TP(4),D(4,4),M(4,4)                               
      COMMON / COMPLX / KT,KS,Z,ZS,KP,KM                                        
                                                                                
C --- Calculates the exact matrix element squared                               
C ---                                                                           
C ---   PH(i,j) : 4-momentum of the jth photon Kj                               
C ---        (i=4 : energy / i=1,2,3 : 3-momentum)                              
C ---   TM(j)  = (P_ - Kj)      ** 2 }                                          
C ---   TP(j)  = (P+ - Kj)      ** 2 }---> the 3 propagators                    
C ---   D(i,j) = (P_ - Ki - Kj) ** 2 }                                          
C ---                                                                           
      DO 1 IPHOT1 = 1,4                                                         
          KP(IPHOT1) = DCMPLX(-TM(IPHOT1)/SQS,0.D0)                             
          KM(IPHOT1) = DCMPLX(-TP(IPHOT1)/SQS,0.D0)                             
          KT(IPHOT1) = DCMPLX(PH(1,IPHOT1),PH(2,IPHOT1))                        
          KS(IPHOT1) = DCMPLX(PH(1,IPHOT1),-PH(2,IPHOT1))                       
1     CONTINUE                                                                  
      DO 11 IPHOT1 = 1,4                                                        
          DO 2 IPHOT2 = 1,4                                                     
              Z(IPHOT1,IPHOT2) = KP(IPHOT1)*KM(IPHOT2)-                         
     *                           KS(IPHOT1)*KT(IPHOT2)                          
              ZS(IPHOT1,IPHOT2)= DCONJG(Z(IPHOT1,IPHOT2))                       
2         CONTINUE                                                              
11    CONTINUE                                                                  
      IF(IPRINT.GE.10) PRINT 100,KP,KM,KT,KS,Z,ZS                               
100   FORMAT(1X,'KP:'/4(1X,2(D14.8,2X)/)                                        
     *      ,1X,'KM:'/4(1X,2(D14.8,2X)/)                                        
     *      ,1X,'KT:'/4(1X,2(D14.8,2X)/)                                        
     *      ,1X,'KS:'/4(1X,2(D14.8,2X)/)                                        
     *      ,1X,'Z :'/16(1X,2(D14.8,2X)/)                                       
     *      ,1X,'ZS:'/16(1X,2(D14.8,2X)/))                                      
                                                                                
      MSQUAC = (0.D0,0.D0)                                                      
      DO 3 ISQUAR=1,4                                                           
          MSQUAC = MSQUAC +                                                     
     *           4.D0*(KP(ISQUAR)**2+KM(ISQUAR)**2)*                            
     *                 KP(ISQUAR)*KM(ISQUAR)                                    
3     CONTINUE                                                                  
      EXACT  = DREAL(MSQUAC) + 4.D0/SBEAM*(                                     
     *         FMAT(1,2,3,4)+                                                   
     *         FMAT(1,3,2,4)+                                                   
     *         FMAT(1,4,2,3)+                                                   
     *         FMAT(2,3,1,4)+                                                   
     *         FMAT(2,4,1,3)+                                                   
     *         FMAT(3,4,1,2))                                                   
      EXACT  = EXACT /(TM(1)*TM(2)*TM(3)*TM(4)*TP(1)*TP(2)*TP(3)*TP(4))         
      EXACT  = EXACT  * SBEAM**4                                                
C ---                                                                           
C --- Calculates the sum of the approximate amplitudes squared                  
C ---                                                                           
      APPROX = 0.D0                                                             
      DO 635 IM=1,4                                                             
         DO 636 IP=1,4                                                          
            IF(IP.EQ.IM) GOTO 636                                               
            DO 637 ID=1,4                                                       
               IF(ID.EQ.IM.OR.ID.EQ.IP) GOTO 637                                
               DO 638 IB=1,4                                                    
                 IF(IB.EQ.IM.OR.IB.EQ.IP.OR.IB.EQ.ID) GOTO 638                  
                 APPROX = APPROX +                                              
     *              1.D0/(TM(IM) * TP(IP) * D(IM,ID))                           
     *             *1.D0/(SBEAM + D(IM,ID))                                     
     *             *1.D0/(SBEAM + TM(IM) + TM(ID))                              
     *             *1.D0/(SBEAM + TP(IP) + TP(IB))                              
     *             *1.D0/(SBEAM + TM(IM))                                       
     *             *1.D0/(SBEAM + TP(IP))                                       
638            CONTINUE                                                         
637         CONTINUE                                                            
636      CONTINUE                                                               
635   CONTINUE                                                                  
                                                                                
      IF(IPRINT.GE.8) PRINT 110,EXACT,APPROX                                    
110   FORMAT(1X,'EXACT MATRIX ELEMENT SQUARED  : ',D14.8/                       
     *       1X,'SUM OF THE APPROX. AMPLITUDES : ',D14.8)                       
      RETURN                                                                    
      END                                                                       
                                                                                
      DOUBLE PRECISION FUNCTION FMAT(I1,I2,I3,I4)                               
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      COMPLEX*16 KP(4),KM(4),KT(4),KS(4),Z(4,4),ZS(4,4),FINT                    
      COMMON / ENERGY / SBEAM,SQS,EMIN,MA2MIN,MA2MAX,MB2MIN,MB2MAX              
      COMMON / PRINT  / IPRINT                                                  
      COMMON / VAR    / TM(4),TP(4),D(4,4),M(4,4)                               
      COMMON / COMPLX / KT,KS,Z,ZS,KP,KM                                        
      IF(IPRINT.GE.10.AND.I1.EQ.1.AND.I2.EQ.2.AND.I3.EQ.3.AND.I4.EQ.4)          
     *    PRINT 100,KP,KM,KT,KS,Z,Z                                             
100   FORMAT(1X,'KP:'/4(1X,2(D14.8,2X)/)                                        
     *      ,1X,'KM:'/4(1X,2(D14.8,2X)/)                                        
     *      ,1X,'KT:'/4(1X,2(D14.8,2X)/)                                        
     *      ,1X,'KS:'/4(1X,2(D14.8,2X)/)                                        
     *      ,1X,'Z :'/16(1X,2(D14.8,2X)/)                                       
     *      ,1X,'ZS:'/16(1X,2(D14.8,2X)/))                                      
      FINT  = (KT(I1)+KT(I2))*D(I1,I2) +                                        
     *         KT(I4)/D(I1,I3)*(                                                
     *         SQS*KP(I2)*KS(I3)*KT(I1) + Z(I2,I1)*ZS(I1,I3))+                  
     *         KT(I3)/D(I1,I4)*(                                                
     *         SQS*KP(I2)*KS(I4)*KT(I1) + Z(I2,I1)*ZS(I1,I4))+                  
     *         KT(I4)/D(I2,I3)*(                                                
     *         SQS*KP(I1)*KS(I3)*KT(I2) + Z(I1,I2)*ZS(I2,I3))+                  
     *         KT(I3)/D(I2,I4)*(                                                
     *         SQS*KP(I1)*KS(I4)*KT(I2) + Z(I1,I2)*ZS(I2,I4))                   
      FMAT  = CDABS(FINT)**2                                                    
      RETURN                                                                    
      END                                                                       
                                                                                
      DOUBLE PRECISION FUNCTION PROSCA(P1,P2)                                   
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      DIMENSION P1(4),P2(4)                                                     
      PROSCA = P1(4)*P2(4)                                                      
      DO 1 I=1,3                                                                
          PROSCA=PROSCA-P1(I)*P2(I)                                             
1     CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE ROTX(C,S,P)                                                    
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      DIMENSION P(4)                                                            
      PY   = C*P(2) + S*P(3)                                                    
      PZ   =-S*P(2) + C*P(3)                                                    
      P(2) = PY                                                                 
      P(3) = PZ                                                                 
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE ROTZ(C,S,P)                                                    
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      DIMENSION P(4)                                                            
      PX   = C*P(1) + S*P(2)                                                    
      PY   =-S*P(1) + C*P(2)                                                    
      P(1) = PX                                                                 
      P(2) = PY                                                                 
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE PERMUT(X)                                                      
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      COMMON / PHOTON / PH(4,4),WEIGHT,WMAX                                     
      COMMON / VAR    / TM(4),TP(4),D(4,4),M(4,4)                               
      DIMENSION RH(4,4),DN(4,4),MN(4,4),TMN(4),TPN(4)                           
      INTEGER N(4,24)                                                           
      dimension X(10)                                                                
      DATA N/1,2,3,4,1,2,4,3,                                                   
     *       1,3,2,4,1,3,4,2,                                                   
     *       1,4,2,3,1,4,3,2,                                                   
     *       2,1,3,4,2,1,4,3,                                                   
     *       2,3,1,4,2,3,4,1,                                                   
     *       2,4,1,3,2,4,3,1,                                                   
     *       3,1,2,4,3,1,4,2,                                                   
     *       3,2,1,4,3,2,4,1,                                                   
     *       3,4,1,2,3,4,2,1,                                                   
     *       4,1,2,3,4,1,3,2,                                                   
     *       4,2,1,3,4,2,3,1,                                                   
     *       4,3,1,2,4,3,2,1/                                                   
      IPERM = X(9)*24 + 1                                                       
      DO 1 IPHOT1 = 1,4                                                         
          IPHNW1 = N(IPHOT1,IPERM)                                              
          DO 2 IVECT = 1,4                                                      
             RH(IVECT,IPHOT1) = PH(IVECT,IPHNW1)                                
2         CONTINUE                                                              
          TMN(IPHOT1) = TM(IPHNW1)                                              
          TPN(IPHOT1) = TP(IPHNW1)                                              
          DO 3 IPHOT2 =1,4                                                      
             IPHNW2 = N(IPHOT2,IPERM)                                           
             DN(IPHOT1,IPHOT2) = D(IPHNW1,IPHNW2)                               
             MN(IPHOT1,IPHOT2) = M(IPHNW1,IPHNW2)                               
3         CONTINUE                                                              
1     CONTINUE                                                                  
      DO 11 IPHOT1 = 1,4                                                        
          DO 12 IVECT = 1,4                                                     
            PH(IVECT,IPHOT1)=RH(IVECT,IPHOT1)                                   
12        CONTINUE                                                              
          TM(IPHOT1) = TMN(IPHOT1)                                              
          TP(IPHOT1) = TPN(IPHOT1)                                              
          DO 13 IPHOT2 =1,4                                                     
             D(IPHOT1,IPHOT2) = DN(IPHOT1,IPHOT2)                               
             M(IPHOT1,IPHOT2) = MN(IPHOT1,IPHOT2)                               
13        CONTINUE                                                              
11    CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE RSAVE                                                          
      COMMON/BVEG2/NDO,IT,SI,SI2,SWGT,SCHI,XI(50,10),SCALLS,                    
     1                    SIMY,SI2MY,SWGTMY,D(50,10),DI(50,10)                  
      WRITE(8) NDO,IT,SI,SI2,SWGT,SIMY,SI2MY,SWGTMY,SCHI,XI,DI                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE RREST                                                          
      COMMON/BVEG2/NDO,IT,SI,SI2,SWGT,SCHI,XI(50,10),SCALLS,                    
     1                    SIMY,SI2MY,SWGTMY,D(50,10),DI(50,10)                  
      COMMON / RESULT / S1,S2,AVGIMY,SDMY                                       
      READ(7) NDO,IT,SI,SI2,SWGT,SIMY,SI2MY,SWGTMY,SCHI,XI,DI                   
      READ(7) AVGIMY,SDMY                                                       
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE WGHONE(WEIGHT,WGH,WMAX)                                        
      IMPLICIT REAL*8(A-H,J-Z)
      REAL*4 rndm                                                  
      COMMON / EVENTS / ITOT,IBEF,ICUT,IGEN,IANA,IOK                            
C     IF(WEIGHT.GT.0.D0) CALL HFILL(1,DLOG10(WEIGHT),1.)                        
      IF(WMAX.EQ.0.D0) RETURN                                                   
C     IF(WGH.GT.WMAX) WMAX = WGH                                                
      IF(WGH/WMAX.LT.RNDM(WGH)) WGH = 0.D0                                      
      IF(WGH.NE.0.D0) IGEN = IGEN + 1                                           
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE CROSS(WEIGHT,WGH,WMAX)                                         
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      COMMON / EVENTS / ITOT,IBEF,ICUT,IGEN,IANA,IOK                            
      COMMON / SIGMA / SIGMA,ERROR                                              
      COMMON / WNEW / WNEW,WMAX1,wmax2,IWERR(1000)                              
      COMMON / CONDIT / ICOND                                                   
      ICOND = 1                                                                 
      IF(IOK.EQ.0.AND.WMAX.EQ.0.D0) ICOND = 0                                   
      IF(ICOND.EQ.0.OR.WEIGHT.EQ.0.D0) RETURN                                   
      ERROR  = ERROR + WEIGHT**2                                                
      SIGMA  = SIGMA + WEIGHT                                                   
      IF(WMAX.EQ.0.D0.OR.(IOK.NE.0.AND.WGH.NE.0.D0)) IANA = IANA + 1            
      IF(WEIGHT.GT.WNEW.AND.IOK.NE.0.AND.WGH.NE.0.D0) WNEW = WEIGHT             
      IF(WEIGHT.GT.WMAX2.AND.IOK.NE.0.AND.WGH.NE.0.D0) WMAX2 = WEIGHT           
      IF(WMAX.EQ.0.D0.AND.ICUT.LT.1000) WMAX1 = WNEW                            
      IF(WMAX.EQ.0.D0.AND.ICUT.LT.1000) RETURN                                  
      I = IDINT(10.*WEIGHT/WMAX1)+1                                             
      IF(I.GT.1000) RETURN                                                      
C     DO 1 I=1,100                                                              
C         IF(WEIGHT.GT.WMAX1*I/10..AND.WEIGHT.LT.WMAX1*(I+1.)/10.)              
             IWERR(I) = IWERR(I)+1                                              
C1    CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE FINISH(IVEG)                                                   
      IMPLICIT REAL*8(A-H,J-Z)                                                  
      REAL S1,S2,S3,S4                                                          
      COMMON / EVENTS / ITOT,IBEF,ICUT,IGEN,IANA,IOK                            
      COMMON / SIGMA  / SIGMA,ERROR                                             
      COMMON / RESULT / S1,S2,S3,S4                                             
      COMMON / PHOTON / PH(4,4),WEIGHT,WMAX                                     
      COMMON / WNEW / WNEW,WMAX1,wmax2,IWERR(1000)                              
      SIGTO = SIGMA/ITOT*1000.D0                                                
      DSIG  = DSQRT((ERROR*1.D6/ITOT - SIGTO**2)/ITOT)                          
      IF(WMAX.EQ.0.D0.OR.IANA.EQ.IGEN) GOTO 4                                   
      IF(IVEG.EQ.0) GOTO 4                                                      
      IN3  = (S3/S4)**2                                                         
      S3   = (S1*IANA**2/IGEN + S3*IN3)/(IANA+IN3)                              
      S4   = S3/SQRT(FLOAT(IANA)+FLOAT(IN3))                                    
4     CONTINUE                                                                  
      IF(IANA.EQ.IGEN) S3=S1                                                    
      IF(IANA.EQ.IGEN) S4=S2                                                    
      IF(IVEG.NE.0) WRITE(8) S3,S4                                              
      S1 = S1*1000.                                                             
      S2 = S2*1000.                                                             
      S3 = S3*1000.                                                             
      S4 = S4*1000.                                                             
      IWMAX = ITOT/40000+1                                                      
      ISEAR = 0                                                                 
      DO 100 I=1,1000                                                           
         ISEAR = ISEAR+IWERR(1001-I)                                            
         IF(ISEAR.LT.IWMAX) GOTO 100                                            
         IRIGHT = 1001-I                                                        
         GOTO 101                                                               
100   CONTINUE                                                                  
101   CONTINUE                                                                  
      WNEW1 = WMAX1*IRIGHT/10.D0                                                
      IF(WNEW1.EQ.0.D0.OR.WNEW1.GT.WNEW) WNEW1 = WNEW                           
      PRINT 3,WNEW1                                                             
3     FORMAT(1X,'**** TO GET UNWEIGHTED EVENTS YOU NEED THIS VALUE :'//         
     *       1X,'     ************************************'/                    
     *       1X,'     *                                  *'/                    
     *       1X,'     *     WMAX    =    ', D9.2,'       *'/                    
     *       1X,'     *                                  *'/                    
     *       1X,'     ************************************'//)                  
      PRINT 2,ITOT,ICUT,IGEN,IANA,S1,S2,S3,S4,SIGTO,DSIG                        
2     FORMAT(1X,'NUMBER OF TRIALS                        : ',I10/               
     *       1X,'NUMBER OF GENERATED EVENTS              : ',I10/               
     *       1X,'NUMBER OF UNWEIGHTED EVENTS               '/                   
     *       1X,'   ( 0 MEANS THAT EVENTS ARE WEIGHTED)  : ',I10/               
     *       1X,'FINAL NUMBER OF EVENTS (AFTER ANALYSIS) : ',I10//              
     *       1X,70('*')//1X,'TOTAL CROSS SECTION : '/                           
     *       1X,'    1) WITH VEGAS ACCUMULATED RESULTS BEFORE CUTS:'/           
     *       20X,E14.8,' PB',' +/- ',E14.8,' PB'//                              
     *       1X,'    2) WITH VEGAS ACCUMULATED RESULTS AFTER CUTS :'/           
     *       20X,E14.8,' PB',' +/- ',E14.8,' PB'//                              
     *       1X,'    3) DURING THE LAST RUN :'/                                 
     *       20X,D14.8,' PB',' +/- ',D14.8,' PB'//)                             
      IF(WMAX.EQ.0.D0.OR.IANA.EQ.IGEN) GOTO 9                                   
      SIGTO = SIGTO * IANA/IGEN                                                 
      DSIG  = DSIG * IANA/IGEN                                                  
      S1    = S1 * IANA/IGEN                                                    
      S2    = S2 * IANA/IGEN                                                    
      DSIG  = SIGTO /DSQRT(IANA+1.D0)                                           
      S2    = S1    /SQRT(IGEN+1.)                                              
      PRINT 5,SIGTO,DSIG,S1,S2                                                  
5     FORMAT(1X,'    4) DURING THE LAST RUN AFTER CUTS:'/                       
     *       20X,D14.8,' PB',' +/- ',D14.8,' PB'//                              
     *       1X,'    5) DURING THE LAST RUN AFTER CUTS (VEGAS):'/               
     *       20X,E14.8,' PB',' +/- ',E14.8,' PB'//)                             
9     CONTINUE                                                                  
      PRINT 7                                                                   
7     FORMAT(1X,70('*')//)                                                      
      CALL HIDOPT(0,'1EVL')                                                     
      CALL HIDOPT(0,'BLAC')                                                     
C     CALL HISTDO                                                               
      RETURN                                                                    
      END                                                                       
C                                                                               
C *******************************************************************           
C                                                                               
C    E N D    O F   T H E   M O N T E - C A R L O   P R O G R A M               
C                                                                               
C *******************************************************************           
C                                                                               
      SUBROUTINE VEGAS(FXN,NCALL,ITMX,ITST)                                     
C                                                                               
C  SUBROUTINE PERFORMS N-DIMENSIONAL MONTE CARLO INTEG*N                        
C    - BY G.P.  LEPAGE  SEPT 1976/ (REV) APR 1978                               
C                                                                               
C      WARNING : THIS ROUTINE HAS BEEN MODIFIED TO BE USED                      
C    WITH THE MONTE CARLO PROGRAM : E+E- --> 4 PHOTONS                          
C                                                                               
                                                                                
      REAL*8 PH,WEIGHT,WGH,WMAX                                                 
      COMMON / PHOTON / PH(4,4),WEIGHT,WMAX                                     
      COMMON / EVENTS / ITOT,IBEF,ICUT,IGEN,IANA,IOK                            
      COMMON / CONDIT / ICOND                                                   
      COMMON/BVEG2/NDO,IT,SI,SI2,SWGT,SCHI,XI(50,10),SCALLS,                    
     1                    SIMY,SI2MY,SWGTMY,D(50,10),DI(50,10)                  
       DIMENSION XIN(50),R(50),DX(10),IA(10),KG(10),DT(10)                      
      DIMENSION XL(10),XU(10),QRAN(10)
      REAL *8 FXN
      REAL *8 X                                   
      COMMON / RANDOM / X(10)                                                       
      COMMON/RESULT/S1,S2,AVGIMY,SDMY                                           
      DATA XL,XU/10*0.,10*1./                                                   
      DATA NDMX/50/,ALPH/1.5/,ONE/1./,MDS/1/                                    
      DATA NDIM/7/                                                              
C                                                                               
      NDO=1                                                                     
      DO 1 J=1,NDIM                                                             
                                                                                
1      XI(1,J)=ONE                                                              
C                                                                               
      ENTRY VEGAS1                                                              
C      - INITIALIZES CUMMULATIVE VARIABLES,BUT NOT GRID                         
      IT=0                                                                      
      SI=0.                                                                     
      SI2=SI                                                                    
      SWGT=SI                                                                   
      SCHI=SI                                                                   
      SCALLS=SI                                                                 
      SIMY=0.                                                                   
      SI2MY=SIMY                                                                
      SWGTMY=SIMY                                                               
      SCHIMY=SIMY                                                               
C                                                                               
      ENTRY VEGAS2(FXN,NCALL,ITMX,ITST)                                         
C       NO INITIALIZATION                                                       
      ITBEF = IT                                                                
      ND=NDMX                                                                   
      NG=1                                                                      
      IF(MDS.EQ.0) GO TO 2                                                      
      NG=(NCALL/2.)**(1./NDIM)                                                  
      MDS=1                                                                     
      IF((2*NG-NDMX).LT.0) GO TO 2                                              
      MDS=-1                                                                    
      NPG=NG/NDMX+1                                                             
      ND=NG/NPG                                                                 
      NG=NPG*ND                                                                 
2     K=NG**NDIM                                                                
      NPG=NCALL/K                                                               
      IF(NPG.LT.2) NPG=2                                                        
      CALLS=NPG*K                                                               
      DXG=ONE/NG                                                                
      DV2G=DXG**(2*NDIM)/NPG/NPG/(NPG-ONE)                                      
      XND=ND                                                                    
      NDM=ND-1                                                                  
      DXG=DXG*XND                                                               
      XJAC=ONE                                                                  
      DO 3 J=1,NDIM                                                             
      DX(J)=XU(J)-XL(J)                                                         
3     XJAC=XJAC*DX(J)                                                           
C                                                                               
C  REBIN PRESERVING BIN DENSITY                                                 
C                                                                               
      IF(ND.EQ.NDO) GO TO 8                                                     
      RC=NDO/XND                                                                
      DO 7 J=1,NDIM                                                             
      K=0                                                                       
      XN=0.                                                                     
      DR=XN                                                                     
      I=K                                                                       
4     K=K+1                                                                     
      DR=DR+ONE                                                                 
      XO=XN                                                                     
      XN=XI(K,J)                                                                
5     IF(RC.GT.DR) GO TO 4                                                      
      I=I+1                                                                     
      DR=DR-RC                                                                  
      XIN(I)=XN-(XN-XO)*DR                                                      
      IF(I.LT.NDM) GO TO 5                                                      
      DO 6  I=1,NDM                                                             
6     XI(I,J)=XIN(I)                                                            
7     XI(ND,J)=ONE                                                              
      NDO=ND                                                                    
8     CONTINUE                                                                  
      ENTRY VEGAS3(FXN,NCALL,ITMX,ITST)                                         
C     - MAIN INTEGRATION LOOP                                                   
9      IT=IT+1                                                                  
      TI=0.                                                                     
      TSI=TI                                                                    
      TIMY=0.                                                                   
      TSIMY=TIMY                                                                
      DO 10 J=1,NDIM                                                            
      KG(J)=1                                                                   
      DO 10 I=1,ND                                                              
      D(I,J)=TI                                                                 
10    DI(I,J)=TI                                                                
C                                                                               
11     FB=0.                                                                    
      F2B=FB                                                                    
      FBMY=0.                                                                   
      F2BMY=FBMY                                                                
      K=0                                                                       
12    K=K+1                                                                     
      DO 121 J=1,NDIM                                                           
121   QRAN(J)=RANF(0)                                                           
      WGT=XJAC                                                                  
      DO 15 J=1,NDIM                                                            
      XN=(KG(J)-QRAN(J))*DXG+ONE                                                
      IA(J)=XN                                                                  
      IAJ=IA(J)                                                                 
      IAJ1=IAJ-1                                                                
      IF(IAJ.GT.1) GO TO 13                                                     
      XO=XI(IAJ,J)                                                              
      RC=(XN-IAJ)*XO                                                            
      GO TO 14                                                                  
13    XO=XI(IAJ,J)-XI(IAJ1,J)                                                   
      RC=XI(IAJ1,J)+(XN-IAJ)*XO                                                 
14    X(J)=XL(J)+RC*DX(J)                                                       
15    WGT=WGT*XO*XND                                                            
C                                                                               
      F=FXN(X)*WGT                                                              
C **********   ANALYSIS     AND     FINAL    CUTS   ****************            
      WEIGHT = F                                                                
      WGH    = WEIGHT                                                           
      IF(IT.GT.ITST) CALL WGHONE(WEIGHT,WGH,WMAX)                               
      IF(WMAX.NE.0.D0.AND.IT.LE.ITST) GOTO 354                                  
C     IF(WGH.NE.0.D0.OR.WMAX.EQ.0.D0) CALL ANALYS(IOK)                          
      IF(WGH.NE.0.D0)                 CALL ANALYS(IOK)                          
354   CONTINUE                                                                  
      CALL CROSS(WEIGHT,WGH,WMAX)                                               
      F = WEIGHT                                                                
C ******************************************************************            
      F1=F/CALLS                                                                
      W=WGT/CALLS                                                               
      F2=F*F                                                                    
      FB=FB+F                                                                   
      F2B=F2B+F2                                                                
C **** WARNING ****                                                             
      IF(WMAX.NE.0.D0) GOTO 355                                                 
      FBMY=FBMY+F*ICOND                                                         
      F2BMY=F2BMY+F2*ICOND                                                      
355   CONTINUE                                                                  
C *****************                                                             
      DO 16 J=1,NDIM                                                            
      IAJ=IA(J)                                                                 
      DI(IAJ,J)=DI(IAJ,J)+F/CALLS                                               
16    IF(MDS.GE.0)  D(IAJ,J)=D(IAJ,J)+F2                                        
      IF(K.LT.NPG) GO TO 12                                                     
C                                                                               
      F2B=F2B*NPG                                                               
      F2B=SQRT(F2B)                                                             
      F2B=(F2B-FB)*(F2B+FB)                                                     
      TI=TI+FB                                                                  
      TSI=TSI+F2B                                                               
C **** WARNING ****                                                             
      IF(WMAX.NE.0.D0) GOTO 356                                                 
      F2BMY=F2BMY*NPG                                                           
      F2BMY=SQRT(F2BMY)                                                         
      F2BMY=(F2BMY-FBMY)*(F2BMY+FBMY)                                           
      TIMY=TIMY+FBMY                                                            
      TSIMY=TSIMY+F2BMY                                                         
356   CONTINUE                                                                  
C *****************                                                             
      IF(MDS.GE.0) GO TO 18                                                     
      DO 17 J=1,NDIM                                                            
      IAJ=IA(J)                                                                 
17    D(IAJ,J)=D(IAJ,J)+F2B                                                     
18    K=NDIM                                                                    
19     KG(K)=MOD(KG(K),NG)+1                                                    
      IF(KG(K).NE.1) GO TO 11                                                   
      K=K-1                                                                     
      IF(K.GT.0) GO TO 19                                                       
C                                                                               
C  FINAL RESULTS FOR THIS ITERATION                                             
C                                                                               
      TI=TI/CALLS                                                               
      TSI=TSI*DV2G                                                              
      TI2=TI*TI                                                                 
      WGT=TI2/TSI                                                               
      SI=SI+TI*WGT                                                              
      SI2=SI2+TI2                                                               
      SWGT=SWGT+WGT                                                             
      SCHI=SCHI+TI2*WGT                                                         
      SCALLS=SCALLS+CALLS                                                       
      AVGI=SI/SWGT                                                              
      SD=SWGT*IT/SI2                                                            
       CHI2A=0.                                                                 
       IF(IT.GT.1)CHI2A=SD*(SCHI/SWGT-AVGI*AVGI)/(IT-1)                         
      SD=ONE/SD                                                                 
      SD=SQRT(SD)                                                               
C **** WARNING ****                                                             
      IF(WMAX.NE.0.D0) GOTO 357                                                 
      TIMY=TIMY/CALLS                                                           
      TSIMY=TSIMY*DV2G                                                          
      TI2MY=TIMY*TIMY                                                           
      WGTMY=TI2MY/TSIMY                                                         
      SIMY=SIMY+TIMY*WGTMY                                                      
      SI2MY=SI2MY+TI2MY                                                         
      SWGTMY=SWGTMY+WGTMY                                                       
      AVGIMY=SIMY/SWGTMY                                                        
      SDMY=SWGTMY*IT/SI2MY                                                      
      SDMY=ONE/SDMY                                                             
      SDMY=SQRT(SDMY)                                                           
357   CONTINUE                                                                  
C *****************                                                             
C                                                                               
C   REFINE GRID                                                                 
C                                                                               
21    S1=AVGI                                                                   
      S2=SD                                                                     
      S3=TI                                                                     
      S4=TSI                                                                    
      DO 23 J=1,NDIM                                                            
      XO=D(1,J)                                                                 
      XN=D(2,J)                                                                 
      D(1,J)=(XO+XN)/2.                                                         
      DT(J)=D(1,J)                                                              
      DO 22 I=2,NDM                                                             
      D(I,J)=XO+XN                                                              
      XO=XN                                                                     
      XN=D(I+1,J)                                                               
      D(I,J)=(D(I,J)+XN)/3.                                                     
22    DT(J)=DT(J)+D(I,J)                                                        
      D(ND,J)=(XN+XO)/2.                                                        
23    DT(J)=DT(J)+D(ND,J)                                                       
C                                                                               
      DO 28 J=1,NDIM                                                            
      RC=0.                                                                     
      DO 24 I=1,ND                                                              
      R(I)=0.                                                                   
      IF(D(I,J).LE.0.) GO TO 24                                                 
      XO=DT(J)/D(I,J)                                                           
      R(I)=((XO-ONE)/XO/ALOG(XO))**ALPH                                         
24    RC=RC+R(I)                                                                
      RC=RC/XND                                                                 
      K=0                                                                       
      XN=0.                                                                     
      DR=XN                                                                     
      I=K                                                                       
25    K=K+1                                                                     
      DR=DR+R(K)                                                                
      XO=XN                                                                     
      XN=XI(K,J)                                                                
26    IF(RC.GT.DR) GO TO 25                                                     
      I=I+1                                                                     
      DR=DR-RC                                                                  
      XIN(I)=XN-(XN-XO)*DR/R(K)                                                 
      IF(I.LT.NDM) GO TO 26                                                     
      DO 27 I=1,NDM                                                             
27     XI(I,J)=XIN(I)                                                           
28    XI(ND,J)=ONE                                                              
C                                                                               
      IF(IT.LT.(ITMX+ITBEF)) GO TO 9                                            
      S1=AVGI                                                                   
      S2=SD                                                                     
      S3=CHI2A                                                                  
      RETURN                                                                    
      END                                                                       
