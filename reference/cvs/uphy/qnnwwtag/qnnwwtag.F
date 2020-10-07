C------------------------------------------------------------------------------C
      SUBROUTINE QNNWTAG(IPRESEL,IPLOT,IENERGY,OUTNN,IERNNT)
C------------------------------------------------------------------------------C
C
C     QQQ   N   N N   N W     W TTTTT  A    GGG
C    Q   Q  NN  N NN  N W     W   T   A A  G                Version 1.3
C    Q  QQ  N N N N N N W  W  W   T  A   A G  GG
C     QQQQ  N  NN N  NN W W W W   T  AAAAA G   G          11th March 1997
C         Q N   N N   N WW   WW   T  A   A  GGG
C    -------------------------------------------
C
C
C    Last Modified:    01/07/97 
C
C
C
C           In case of problems, please contact :   guichene@frcpn11.in2p3.fr
C
C
C           If you want more informations, please go to the Web at :
C
C         http://alephwww.cern.ch/~edgecock/ww_jets/tools/qnnwtag.html  
C
C
C    The WW group of Clermont-Ferrand : 
C    ----------------------------------
C    (IN2P3 - CNRS - Blaise Pascal University - France) 
C                   Z.  Ajaltouni                      P.   Henrard
C                   G.  Chazelle                       J.   Jousset
C                   A.  Falvard                        F.   Podlyski
C                   C.  Guicheney                      J.M. Rossignol
C
C
C
C This package allows to tag WW events with an artificial neural network (NN) 
C in the e+e- --> W+W- --> 4quarks final state at Lep2. The discrimination is 
C done on the NN output variable. 
C The architecture of the subroutine is :
C             ---> preselection of each event with cuts on some variables
C             ---> computation of the variables used as inputs of the NN
C             ---> computation of the NN output
C For more details with preselection and learning of the NN, read the abstract
C concerning the version of QNNWTAG in use.
C
C
C Description of parameters of the QNNWTAG routine :
C ==================================================
C
C                   Call QNNWTAG(IPRESEL,IPLOT,IENERGY,OUTNN,IERNNT)
C
C
C Inputs :
C ========
C
C          * IPRESEL : = 1 The preselection @ "IENERGY" Gev value is triggered
C            -------   = 0 NOT preselection
C                          !!! W A R N I N G !!!  : the learning of the NN is
C                                                   done WITH the preselection       
C                                                        ----
C
C          * IPLOT : Flag to print (= 1) or not (= 0) some histogramms
C            -----  
C                    ID = 1001 --> 1000+x : distribution of the x NN inputs.
C                       The x value is provided with the version of QNNWTAG 
C                       For example : @ 161Gev (version 1.0) : x = 17 
C
C                    ID = 701 : output of th NN 
C
C
C          * IENERGY : Containing the value of the running energy analysis.
C            -------  
C                      For the values : at the present time, the NN 
C                      learning @ 161 Gev and @ 172 GeV are available. 
C
C
C
C Outputs :
C =========
C
C          * OUTNN : Neural network output [-1., 1.]
C            -----   OUTNN = 9999. means that the output has not been 
C                    computed for the event. Check the corresponding
C                    IERNNT value. 
C
C          * IERNNT : Error flag
C            ------   = 0 : OK
C                     = 1 : parameters (IPRESEL, IPLOT) are not available
C                     = 2 : preselection cuts not satisfied
C                     = 3 : error in computing the preselection variables  
C                     = 4 : error in computing the NN inputs
C                     = 5 : error in computing the NN output
C                     = 6 : learning for the IENERGY desired is not available
C
C
C
C------------------------------------------------------------------------------C
C
C
C
C  Revision history: 
C   ======>    Version 1.0 - 18th December 1996    <======
C
C                    * Initial version used @ IENERGY = 161 Gev
C                      This package can be used with IENERGY = 172 Gev too.
C                      When running @ 172 Gev, keep in mind that the NN
C                      learning is optimized for 161 Gev.
C                                    
C                    * The NN is optimized for 161 Gev and its 
C                      architecture is (17,15,1) : 
C                        17 = inputs (corresponding variables are described
C                                     below)
C                        15 = hidden neurons
C                         1 = output
C                    * The preselection uses 5 cuts :
C                           1)  EM < 50 Gev   : Missing Energy
C                           2)  95 > OEF > 45 : Number of Energy Flow objects 
C                           3)  NJET = 4      : events forced in 4 jets 
C                           4)  SPH > 0.2     : Sphericity
C                           5)  THR < 0.95    : Thrust
C 
C 
C
C   More informations about the 17 NN inputs :
C   ------------------------------------------
C      The 17 variables used for the NN learning are :
C            1) Fox Wolfram Moment : H2
C            2) Fox Wolfram Moment : H3
C            3) Fox Wolfram Moment : H4
C            4) Missing mass : XM
C            5) Transverse Momentum of all charged tracks : PTCH
C            6) Transverse Momentum of Jet2 : XT2
C            7) Invariant mass for a dijet combination (Jet3 <-> Jet4) : XA34
C            8) Invariant mass for a dijet combination (Jet2 <-> Jet4) : XA24 
C            9) Invariant mass for a dijet combination (Jet1 <-> Jet4) : XA14
C           10) Number of objects for jet3 : OBJ3
C           11) Number of objects for jet4 : OBJ4
C           12) Energy of Jet1 : EJ1
C           13) Asymmetry between Jet1 and Jet4 : ASY1
C           14) Asymmetry between Jet3 and Jet4 : ASY6
C           15) Angle between Jet1 and Jet2 : ANG1
C           16) Angle between Jet1 and Jet3 : ANG2
C           17) Angle between Jet2 and Jet3 : ANG4 
C
C
C
C   ======>    Version 1.1 - 30th January 1997    <=====
C
C                    * Version used @ IENERGY = 161 GeV and 172 GeV
C                      The NN learning used are optimized for each energy.
C                          -----------
C                      The preselection, the NN architecture (17,15,1)  
C                      and the NN inputs are the same. 
C                                    
C
C
C   ======>    Version 1.2 - 03th February 1997    <=====
C
C                    * Version used @ IENERGY = 161 GeV and 172 GeV
C                      The NN learning and the preselection used are 
C                                              ------------
C                      optimized for each energy.
C                      The NN architecture (17,15,1) and the 17 NN inputs
C                      are the same.
C
C                    * The preselection @ 172 GeV uses 6 cuts :
C                           1)  EM < 40 Gev
C                           2)  OEF > 45
C                           3)  NJ005 >= 4
C                           4)  NJET = 4
C                           5)  IFTJT >= 2
C                           6)  RELMAX < 0.9 
C                                    
C
C
C   ======>    Version 1.3 - 11th March 1997    <=====
C
C                    * Version used @ IENERGY = 161 GeV and 172 GeV
C                      The preselection, the NN architecture and the 
C                                            ---------------
C                      NN learning used are optimized for each energy.
C                    
C                    * @ 161 GeV : the NN architecture (17,15,1) and the 
C                      17 NN inputs are the inputs of versions 1.0 -> 1.2.
C                    
C                    * @ 172 GeV : the NN architecture (21,17,1) and the 
C                      21 NN inputs are new and optimized for this energy.
C 
C                    * !!! WARNING !!! : for this version, you must include 
C                      in your CARDS the QIPBTAG calibration cards.
C                      You can get an example of these cards on the Web @ :
C                http://alephwww.cern.ch/~edgecock/ww_jets/tools/qnnwtag.html  
C
C 
C
C   More informations about the 21 NN inputs :
C   ------------------------------------------
C      The 21 variables used for the NN learning @ 172 Gev are :
C            1) Aplanarity : APLA
C            2) Fox Wolfram Moment : H0
C            3) Fox Wolfram Moment : H2
C            4) Fox Wolfram Moment : H3
C            5) Fox Wolfram Moment : H4
C            6) Energy Flow : EF
C            7) Oblatness : OBL
C            8) Momentum of all charged tracks : PCH
C            9) Transverse Momentum of Jet1 : XT1
C           10) Number of Objects for Jet1 : OBJ1
C           11) Number of Objects for Jet4 : OBJ4
C           12) Sum of Cosinus between Jets : ANGTOT 
C           13) The most energetic charged track of Jet1 : ETM1 
C           14) Maximum energy carried by one object of Jet1 : RT1
C           15) Maximum energy carried by one object of Jet2 : RT2
C           16) Maximum energy carried by one object of Jet3 : RT3
C           17) Maximum of minimum Invariant mass for dijet combination : XMSTOT
C           18) Maximum Invariant Mass : XAMAX
C           19) Variable of QIPBTAG : SOMPROB
C           20) Variable of QIPBTAG : SPROB
C           21) Variable of QIPBTAG : SPBEVT
C
C
C
C
C------------------------------------------------------------------------------C
C
#include "qcde.h"
C 
#include "qnnwini.h"     
C
      COMMON/NN161/NEURON161(3),WPOIDS161(2,30,30),A161(30),B161(30),
     & XMIN161(30),XMAX161(30),SOMOY161(30),SOM161(30),NIVMAX161                                        
C
      COMMON/NN172/NEURON172(3),WPOIDS172(2,30,30),A172(30),B172(30),
     & XMIN172(30),XMAX172(30),SOMOY172(30),SOM172(30),NIVMAX172                                        
C
      COMMON/TABJET/JJMAX(4),QEM(4),TAX(6),TT(4),TTB(4),TL(4)
C
      INTEGER IENERGY,IPRESEL,IERNNT,IPLOT
      REAL OUTNN
C
#include "qmacro.h"
C
C
C.....Initialize Error Flag : IERNNT
C     ------------------------------
      IERNNT = 0
C
C.....Initialize NN Output : OUTNNT
C     -----------------------------
      OUTNN = -9999.
C
C.....Clusterisation of Energy flow objects
C     -------------------------------------
      CALL QJOPTR('EF',' ')      
C
C
C.....Initialization of histogramms
C     -----------------------------
      IF (.NOT.IFLAG1) THEN
         IFLAG1 = .TRUE.    
         IF (IENERGY.EQ.161) CALL HISTOINI161(IPLOT)
         IF (IENERGY.EQ.172) CALL HISTOINI172(IPLOT)
C
C........Output file in unit 19
C        ----------------------         
         CALL OUTVERS(IPRESEL,IPLOT,IENERGY) 
C
      ENDIF       
C
C
C
C.....Test of QNNWTAG inputs validity
C     -------------------------------
      CALL TESTVALID(IPRESEL,IPLOT,IENERGY,IERNNT)
      IF (IERNNT.NE.0) RETURN
C
C
C*******************************************************************************
C                                                                              *     
C  Version 1.3 :  Preselection and NN optimized for IENERGY = 161 GeV          *
C  -------------                          and       IENERGY = 172 GeV          *
C                                                                              *
C*******************************************************************************
C
C
C
C.........Preselection 
C         ============ 
          IF (IPRESEL.EQ.1) THEN 
             IF (IENERGY.EQ.161) CALL PRESEL161(IENERGY,IERNNT)
             IF (IENERGY.EQ.172) CALL PRESEL172(IENERGY,IERNNT)
          ENDIF
          IF (IERNNT.NE.0) RETURN
C
C
C
C.........Compute the Neural Network Inputs
C         =================================
C         @ IENERGY = 161 GeV 
C         -------------------
          IF (IENERGY.EQ.161)
     &       CALL NNINPUT161(XINPUT,IPRESEL,IPLOT,IENERGY,IERNNT)
C
C         @ IENERGY =  172 GeV 
C         --------------------
          IF (IENERGY.EQ.172) 
     &       CALL NNINPUT172(XINPUT,IPRESEL,IPLOT,IENERGY,IERNNT)
C
          IF (IERNNT.NE.0) RETURN
C
C
C
C.........NN Initialization    
C         =================
          IF (.NOT.IFLAG2) THEN
             IFLAG2 = .TRUE.   
             CALL RESINI(IENERGY) 
          ENDIF        
C
C
C.........Compute NN Output
C         =================
          CALL RESNEUR(IENERGY,XINPUT,OUTNN)
C
C
C.........Distribution of NN ouput
C         ========================
          IF (OUTNN.EQ.-9999.) RETURN
          IF (IPLOT.EQ.1) THEN
             CALL HFILL (701 , REAL(OUTNN) ,0.,1.)
          ENDIF

C
C
C
C
C*******************************************************************************
C                                                                              *     
C                             End  of  Version 1.3                             *
C                             --------------------                             * 
C                                                                              *
C*******************************************************************************
C
C
C
C
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine  QNNWTAG                                                                       
C=====================================================================C
C
C
C
C
C
C
C
C
C
C=====================================================================C         
      LOGICAL FUNCTION PODTRCK(ITK)                                             
C     This function returns : .TRUE. for "good" track                       
C                             .FALSE. if not
C                                                  
C     A "good" track must satisfy cuts on these criteria :             
C                QP,   QDB,  QZB,  KFRTNT,  QCOS                                                              
C---------------------------------------------------------------------C         
#include "qcde.h"                                             
#include "qmacro.h"                                           
C                                                                               
C.....Values of cuts                                                     
      DATA D0MAX,Z0MAX,MINHIT,COSMAX/ 3. , 7., 4 , 0.95 /                       
      DATA PMIN/0.20/                                                           
C                                                                               
      PODTRCK = .FALSE.                                                         
C.....Criterion of selection a "good" track                               
      IF (QP(ITK).LT.PMIN)         RETURN                                       
      IF (KFRTNT(ITK).LT.MINHIT)   RETURN                                       
      IF (ABS(QCT(ITK)).GE.COSMAX) RETURN                                       
      IF (ABS(QZB(ITK)).GE.Z0MAX)  RETURN                                       
      IF (ABS(QDB(ITK)).GE.D0MAX)  RETURN                                       
      PODTRCK = .TRUE.                                                          
C                                                                               
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  FUNCTION PODTRCK                                                                       
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE TESTVALID(IPRESEL,IPLOT,IENERGY,IERNNT)   
C     Test of QNNWTAG inputs validity                    
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h" 
C
#include "qmacro.h"
C
C.....Test of IPRESEL :
C     -----------------
      IF (IPRESEL.NE.0.AND.IPRESEL.NE.1) IERNNT=1      
C
C.....Test of IPLOT :
C     ---------------
      IF (IPLOT.NE.0.AND.IPLOT.NE.1) IERNNT=1 
C
C.....Test of IENERGY :
C     -----------------
      IF (IENERGY.NE.161.AND.IENERGY.NE.172) IERNNT=6       
C
C                                                                               
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine TESTVALID                                                                       
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE PRESEL161(IENERGY,IERNNT)  
C     Preselection @ 161 GeV
C     Input  : IENERGY
C     Output : IERNNT                   
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h" 
C
      COMMON/TABJET/JJMAX(4),QEM(4),TAX(6),TT(4),TTB(4),TL(4)
C
#include "qmacro.h"
C
C
C
C
C............Cut number 1 : EM < 50 Gev
C                           (Missing Energy < 50 gev)
C
C            Compute the Missing Energy
             CALL QJMISS(PMISS,'MISS-VECTOR',KRECO,0)                                  
             IMISS = KPDIR('MISS-VECTOR',KRECO) 
             IF (IMISS.NE.0) THEN                                       
                EM=QE(IMISS)                                                                                                                  
             ENDIF
             IF (IMISS.EQ.0) THEN
                IERNNT=3
                RETURN
             ENDIF   
C 
C            Cut on the Missing Energy
             IF (EM.GE.50.00) THEN
                IERNNT=2 
                RETURN
             ENDIF     
C        
C............End of Cut 1
C
C
C
C............Cut number 2 : 95 > OEF > 45
C                           (95 > Number of Energy Flow Objects > 45)
C
C            Compute the total value of energy flow EFL
             EFL= 0.                                                                   
             DO 71 JP= KFEFT,KLEFT                                                     
                EFL=EFL+QE(JP)                                                      
71           CONTINUE  
C      
C            Cut on EFL L  
             IF (EFL.GT.500.00) THEN
                IERNNT=2 
                RETURN
             ENDIF
C
C            Compute the total energy of charged tracks ECHA
             ECHA= 0.                                                                  
             DO 771 JCH=KFCHT,KLCHT                                                    
                ECHA=ECHA+QE(JCH)                                                   
771          CONTINUE  
C
C            Cut on ECHA
             IF (ECHA.GT.300.00) THEN
                IERNNT=2
                RETURN 
             ENDIF
C                                                                               
C            Number of Energy Flow Objects OEF                                     
             OEF=FLOAT(KNEFT)                                                          
C 
C            Cut on OEF
             IF (OEF.LE.45.00.OR.OEF.GE.95.00) THEN
                IERNNT=2 
                RETURN
             ENDIF     
C        
C............End of Cut 2
C
C
C
C
C
C............Cut number 3 : NJET = 4
C                           (The event is forced in 4 jets) 
C 
C            Clusterisation with Jade algorithm to force in 4 jets                                                                                           
             CALL CLUSJET4(IENERGY,NJET,'Jetdir')
C  
C            Cut on NJET
             IF (NJET.NE.4) THEN
                IERNNT=2 
                RETURN
             ENDIF
C        
C............End of Cut 3
C
C
C
C
C............Cut number 4 : THR < 0.95
C                           (Thrust < 0.95)
C
             CALL QJTHRU(THR,'thrust-axis',KRECO)     
C
C                                                                               
C
C            Cut on THRUST
             IF (THR.GE.0.95) THEN
                IERNNT=2 
                RETURN
             ENDIF
C        
C............End of Cut 4
C
C
C
C
C............Cut number 5 : SPH > 0.2
C                           (Sphericity > 0.2)
C
             CALL QJSPHE(SPH,'SPHERIAX',KRECO)     
C
C                                                                               
C
C            Cut on SPH
             IF (SPH.LE.0.2) THEN
                IERNNT=2 
                RETURN
             ENDIF
C        
C............End of Cut 5
C
C
C
C
C                                                                               
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine PRESEL161                                                                       
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE PRESEL172(IENERGY,IERNNT)  
C     Preselection @ 172 GeV
C     Input  : IENERGY
C     Output : IERNNT                   
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h" 
C
      COMMON/TABJET/JJMAX(4),QEM(4),TAX(6),TT(4),TTB(4),TL(4)
C
#include "qmacro.h"
C
C
C
C
C
C............Cut number 1 : EM < 40 Gev
C                           (Missing Energy < 40 gev)
C
C            Compute the Missing Energy
             CALL QJMISS(PMISS,'MISS-VECTOR',KRECO,0)                                  
             IMISS = KPDIR('MISS-VECTOR',KRECO) 
             IF (IMISS.NE.0) THEN                                       
                EM=QE(IMISS)                                                                                                                  
             ENDIF
             IF (IMISS.EQ.0) THEN
                IERNNT=3
                RETURN
             ENDIF   
C 
C            Cut on the Missing Energy
             IF (EM.GE.40.00) THEN
                IERNNT=2 
                RETURN
             ENDIF     
C        
C............End of Cut 1
C
C
C
C............Cut number 2 : OEF > 45
C                           (Number of Energy Flow Objects > 45)
C                                                                               
C            Number of Energy Flow Objects OEF                                     
             OEF=FLOAT(KNEFT)                                                          
C 
C            Cut on OEF
             IF (OEF.LE.45.00) THEN
                IERNNT=2 
                RETURN
             ENDIF     
C        
C............End of Cut 2
C
C
C
C............Cut number 3 : NJ005 >= 4
C                           (At least 4 jets required using the Jade 
C                            clusterisation algorithm with Ycut=0.005)
C
C            Clusterisation with Ycut : Y005 = 0.005
             Y005=0.005
             EVIS=0.00  
             CALL QJMMCL(NJ005,'N005',KRECO,Y005,EVIS)
C  
C            Cut on NJ005
             IF (NJ005.LT.4) THEN
                IERNNT=2 
                RETURN
             ENDIF
C        
C............End of Cut 3
C
C
C
C............Cut number 4 : NJET = 4
C                           (The event is forced in 4 jets) 
C 
C            Clusterisation with Jade algorithm to force in 4 jets                                                                                           
             CALL CLUSJET4(IENERGY,NJET,'Jetdir')
C  
C            Cut on NJET
             IF (NJET.NE.4) THEN
                IERNNT=2 
                RETURN
             ENDIF
C        
C............End of Cut 4
C
C
C
C............Cut number 5 : IFTJT >= 2
C                           (At least 2 "good" tracks in a jet)
C                                                                               
C            Classification of jets : from the most energetic to the less
C                                     energetic one 
             CALL QEJJ4(NJET,'Jetdir')  
C                                                                              
C                                                                               
C           Number of charged objects per jets    
            CALL OBC14B
C
            NTJ = 0 
            NTJB = 0
            DO K = 1,4
               IF (TT(K).EQ.0.0) NTJ = NTJ + 1
               IF (TTB(K).EQ.0.0) NTJB = NTJB + 1
            END DO    
C
C           One jet has 0 charged track                         
            IF (NTJ.GT.0) IFTJT = 0
C           All jets have at least one charged track   
            IF (NTJ.EQ.0) IFTJT = 1
C           All jets have one good charged track 
            IF (NTJB.EQ.0) IFTJT = 2
C
C
C
C            Cut on IFTJT
             IF (IFTJT.LT.2) THEN
                IERNNT=2 
                RETURN
             ENDIF
C        
C............End of Cut 5
C
C
C
C............Cut number 6 : RELMAX < 0.9
C                           (The maximum electromagnetic energy of jets)
C
C                                                                                                                 
C            Jet energy                                                         
             EJ1=QEM(1)                                                            
             EJ2=QEM(2)                                                            
             EJ3=QEM(3)                                                            
             EJ4=QEM(4)                                                            
C  
C            Electromagnetic jet energy
             EL1 = 0.00
             EL2 = 0.00
             EL3 = 0.00
             EL4 = 0.00
             ELMIN = 100.00
             ELMAX = -100.00
C
             DO ITE = KFEFT,KLEFT
                IF (XEFO(ITE)) THEN
                   IF (KEFOTY(ITE).EQ.4) THEN
                      IF (QE(ITE).GT.ELMAX) ELMAX = QE(ITE)
                      IF (QE(ITE).LT.ELMIN) ELMIN = QE(ITE)
C
                      IF (XSAME(ITE,JJMAX(1))) EL1 = EL1 + QE(ITE)
C
                      IF (XSAME(ITE,JJMAX(2))) EL2 = EL2 + QE(ITE)
C
                      IF (XSAME(ITE,JJMAX(3))) EL3 = EL3 + QE(ITE)
C
                      IF (XSAME(ITE,JJMAX(4))) EL4 = EL4 + QE(ITE)
C
                   ENDIF
                ENDIF        
             END DO
C
             REL1 = -1.00
             REL2 = -1.00
             REL3 = -1.00
             REL4 = -1.00
             IF (EJ1.NE.0.00) REL1 = EL1/EJ1
             IF (EJ2.NE.0.00) REL2 = EL2/EJ2
             IF (EJ3.NE.0.00) REL3 = EL3/EJ3
             IF (EJ4.NE.0.00) REL4 = EL4/EJ4
C                                                                               
             TL(1) = REL1
             TL(2) = REL2
             TL(3) = REL3
             TL(4) = REL4
             RELMAX = -100.00
             RELMIN = 100.00
             DO L=1,4
                IF (TL(L).GT.RELMAX) RELMAX = TL(L)
                IF (TL(L).LT.RELMIN) RELMIN = TL(L)
             END DO
C
C 
C
C            Cut on RELMAX
             IF (RELMAX.GE.0.9) THEN
                IERNNT=2 
                RETURN
             ENDIF
C        
C............End of Cut 6
C
C
C
C                                                                               
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine PRESEL172                                                                       
C=====================================================================C
C
C
C
C
C=====================================================================C         
      SUBROUTINE CLUSJET4(IENERGY,NJET,DIRJET)  
C     Clusterisation with Jade algorithm to force in 4 jets  
C     Outputs : NJET, DIRJET                    
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h" 
C
      CHARACTER*2 SHEME
      CHARACTER*6 VERSN
      CHARACTER*7 DIRJET
C      
#include "qmacro.h"
C
C
C.....Clusterisation with Jade algorithm and at first Ycut = 0.02
      EVIS=0.                                                                
      YCUT=0.02 
C      
      IF (IENERGY.EQ.161) THEN 
         SHEME = 'E'
         VERSN = 'JADE' 
      ENDIF                                                               
C      
      IF (IENERGY.EQ.172) THEN 
         SHEME = 'P'
         VERSN = 'DURHAM' 
      ENDIF 
C      
      CALL QGJMMC(NJET,DIRJET,KRECO,YCUT,EVIS,SHEME,VERSN)
C
C
C.....At this step, 4 jets are required                                             
      IF (NJET.EQ.4) GOTO 9                                                     
C 
C.....For the remaining events, we iterate on the Ycut value to
C     force them in 4 jets              
      IF (NJET.NE.4) THEN                                      
         ICOMPT=0                                                                     
 11      CONTINUE 
C                                                             
C........Iterate on the YCUT value                          
         YJET=FLOAT(NJET)                                                       
         YCUT=(YJET-4.)/2000.+YCUT                                              
         CALL QGJMMC(NJET,DIRJET,KRECO,YCUT,EVIS,SHEME,VERSN)                                                              
         ICOMPT = ICOMPT+1                                                      
         IF (ICOMPT.EQ.50) GOTO 8                                               
         IF (NJET.NE.4) GOTO 11                                                 
      ENDIF                                                                     
C                                                                               
 9    CONTINUE                                                                  
C
 8    CONTINUE 
C  
C
C
C       
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine CLUSJET4                                                                         
C=====================================================================C
C
C
C 
C=====================================================================C         
      SUBROUTINE QEJJ4(NJET,DIRJET)  
C     Clusterisation with Jade algorithm to force in 4 jets  
C     Inputs : NJET, 'Jetdir'                     
C     Output  : JJMAX(1->4) : jet number 
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h" 
C
C
      COMMON/TABJET/JJMAX(4),QEM(4),TAX(6),TT(4),TTB(4),TL(4)
C
C
      CHARACTER*7 DIRJET
C
#include "qmacro.h"
C
C
C.....Initialisation of QEM :                    
      DO I=1,4
         QEM(I) = -10.
      ENDDO                                                               
C                                                                               
C.....Classification of jets : from the most energetic to the less
C                              energetic one                                                                                          
      JJ = KPDIR(DIRJET,KRECO)                                                 
      DO 2 J = 1,NJET                                                           
         IF (J.GT.1) JJ = KFOLLO(JJ)                                            
         IF (QE(JJ).GT.QEM(1)) THEN                                               
            QEM(1) = QE(JJ)                                                       
            JJMAX(1) = JJ                                                                                                 
         ENDIF                                                                 
 2    CONTINUE                                                                  
C                                                                               
      JJ = KPDIR(DIRJET,KRECO)                                                 
      DO 3 J = 1,NJET                                                           
         IF (J.GT.1) JJ = KFOLLO(JJ)                                            
         IF (JJ.EQ.JJMAX(1)) GOTO 3                                               
         IF (QE(JJ).GT.QEM(2)) THEN                                               
            QEM(2) = QE(JJ)                                                                                                      
            JJMAX(2) = JJ                                                         
         ENDIF                                                                  
 3    CONTINUE                                                                  
C                                                                               
      JJ = KPDIR(DIRJET,KRECO)                                                 
      DO 4 J = 1,NJET                                                           
         IF (J.GT.1) JJ = KFOLLO(JJ)                                            
         IF ((JJ.EQ.JJMAX(1)).OR.(JJ.EQ.JJMAX(2))) GOTO 4                           
         IF (QE(JJ).GT.QEM(3)) THEN                                               
            QEM(3) = QE(JJ)                                                                                                       
            JJMAX(3) = JJ                                                         
         ENDIF                                                                  
 4    CONTINUE                                                                  
C                                                                               
      JJ = KPDIR(DIRJET,KRECO)                                                 
      DO 5 J=1,NJET                                                             
         IF (J.GT.1) JJ = KFOLLO(JJ)                                            
         IF ((JJ.EQ.JJMAX(1)).OR.(JJ.EQ.JJMAX(2)).OR.
     &       (JJ.EQ.JJMAX(3))) GOTO 5         
         IF (QE(JJ).GT.QEM(4)) THEN                                               
            QEM(4) = QE(JJ)                                                                                                      
            JJMAX(4) = JJ                                                         
         ENDIF                                                                  
 5    CONTINUE                                                                  
C                          
C
C       
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine QEJJ4                                                                         
C=====================================================================C
C
C
C 
C=====================================================================C         
      SUBROUTINE OBC14B  
C     Number of charged objects per jets   
C     Input  : JJMAX(1->4) : jet number  
C     Outputs: TT(1->4) 
C              TTB(1->4)              
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h" 
C
      COMMON/TABJET/JJMAX(4),QEM(4),TAX(6),TT(4),TTB(4),TL(4)
C
      LOGICAL PODTRCK
C
#include "qmacro.h"
C
C
C.....Number of charged objects per jets                                          
      ICOJ1 = 0                                                                 
      ICOJ2 = 0                                                                 
      ICOJ3 = 0                                                                 
      ICOJ4 = 0                                                                 
C                              
      ICOJ1B = 0                                                                 
      ICOJ2B = 0                                                                 
      ICOJ3B = 0                                                                 
      ICOJ4B = 0                                                                 
C                                   
      DO 81 ITK = KFCHT,KLCHT
         ET = 0.00                                                   
         IF (XSAME(JJMAX(1),ITK)) THEN
            ICOJ1 = ICOJ1 + 1       
            IF (PODTRCK(ITK)) ICOJ1B=ICOJ1B+1
         ENDIF                        
         IF (XSAME(JJMAX(2),ITK)) THEN
            ICOJ2 = ICOJ2 + 1       
            IF (PODTRCK(ITK)) ICOJ2B=ICOJ2B+1
         ENDIF                        
         IF (XSAME(JJMAX(3),ITK)) THEN 
            ICOJ3 = ICOJ3 + 1 
            IF (PODTRCK(ITK)) ICOJ3B=ICOJ3B+1
         ENDIF                              
         IF (XSAME(JJMAX(4),ITK)) THEN
            ICOJ4 = ICOJ4 + 1       
            IF (PODTRCK(ITK)) ICOJ4B=ICOJ4B+1
         ENDIF                        
C                    
 81   CONTINUE                                                                  
C                                                                               
      OBC1 = FLOAT(ICOJ1)                                                       
      OBC2 = FLOAT(ICOJ2)                                                       
      OBC3 = FLOAT(ICOJ3)                                                       
      OBC4 = FLOAT(ICOJ4)                                                           
C                                                                              
      OBC1B = FLOAT(ICOJ1B)            
      OBC2B = FLOAT(ICOJ2B)    
      OBC3B = FLOAT(ICOJ3B)    
      OBC4B = FLOAT(ICOJ4B)        
C
      TT(1) = OBC1
      TT(2) = OBC2
      TT(3) = OBC3
      TT(4) = OBC4
C                            
      TTB(1) = OBC1B
      TTB(2) = OBC2B
      TTB(3) = OBC3B
      TTB(4) = OBC4B
C                          
C
C       
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine OBC14B                                                                        
C=====================================================================C
C
C
C 
C=====================================================================C         
      SUBROUTINE XADIJET(IERRX)  
C     Number of charged objects per jets   
C     Input  : JJMAX(1->4) : jet number  
C     Outputs: TAX(1->6) : dijet combinations 
C              IERR : Error flag              
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h" 
C
C
      COMMON/TABJET/JJMAX(4),QEM(4),TAX(6),TT(4),TTB(4),TL(4)
C
#include "qmacro.h"
C
C                          
      IERRX = 0
C.....Compute invariant masses for both dijet combination
      TJOU12 = (QE(JJMAX(1))+QE(JJMAX(2)))**2
     &        -(QX(JJMAX(1))+QX(JJMAX(2)))**2            
     &        -(QY(JJMAX(1))+QY(JJMAX(2)))**2
     &        -(QZ(JJMAX(1))+QZ(JJMAX(2)))**2            
      TJOU34 = (QE(JJMAX(3))+QE(JJMAX(4)))**2
     &        -(QX(JJMAX(3))+QX(JJMAX(4)))**2            
     &        -(QY(JJMAX(3))+QY(JJMAX(4)))**2
     &        -(QZ(JJMAX(3))+QZ(JJMAX(4)))**2            
C                                                                               
      TJOU13 = (QE(JJMAX(1))+QE(JJMAX(3)))**2
     &        -(QX(JJMAX(1))+QX(JJMAX(3)))**2            
     &        -(QY(JJMAX(1))+QY(JJMAX(3)))**2
     &        -(QZ(JJMAX(1))+QZ(JJMAX(3)))**2            
      TJOU24 = (QE(JJMAX(2))+QE(JJMAX(4)))**2
     &        -(QX(JJMAX(2))+QX(JJMAX(4)))**2            
     &        -(QY(JJMAX(2))+QY(JJMAX(4)))**2
     &        -(QZ(JJMAX(2))+QZ(JJMAX(4)))**2            
C                                                                               
      TJOU14 = (QE(JJMAX(1))+QE(JJMAX(4)))**2
     &        -(QX(JJMAX(1))+QX(JJMAX(4)))**2            
     &        -(QY(JJMAX(1))+QY(JJMAX(4)))**2
     &        -(QZ(JJMAX(1))+QZ(JJMAX(4)))**2            
      TJOU23 = (QE(JJMAX(2))+QE(JJMAX(3)))**2
     &        -(QX(JJMAX(2))+QX(JJMAX(3)))**2            
     &        -(QY(JJMAX(2))+QY(JJMAX(3)))**2
     &        -(QZ(JJMAX(2))+QZ(JJMAX(3)))**2            
C                                 
      IF (TJOU12.GE.0.0) THEN   
         XA12 = SQRT(TJOU12)    
      ELSE
         IERRX=1
         RETURN
      ENDIF    
      IF (TJOU34.GE.0.0) THEN                                               
         XA34 = SQRT(TJOU34)    
      ELSE
         IERRX=1
         RETURN
      ENDIF
C
      IF (TJOU13.GE.0.0) THEN                                                   
         XA13 = SQRT(TJOU13)    
      ELSE
         IERRX=1
         RETURN
      ENDIF
      IF (TJOU24.GE.0.0) THEN                                                   
         XA24 = SQRT(TJOU24) 
      ELSE
         IERRX=1
         RETURN
      ENDIF                                                   
C                                                                               
      IF (TJOU14.GE.0.0) THEN
         XA14 = SQRT(TJOU14) 
      ELSE 
         IERRX=1
         RETURN
      ENDIF
      IF (TJOU23.GE.0.0) THEN                    
         XA23 = SQRT(TJOU23) 
      ELSE 
         IERRX=1
         RETURN
      ENDIF
C
C
      TAX(1)=XA12
      TAX(2)=XA34
      TAX(3)=XA13
      TAX(4)=XA24
      TAX(5)=XA14
      TAX(6)=XA23
C
C       
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine XADIJET                                                                         
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE HISTOINI161(IPLOT)  
C     Initialization of histogramms @ 161 GeV                       
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h"      
C       
#include "qmacro.h"
C
C
C.....Distribution of the 17 NN inputs
C      --------------------------------
      CALL QBOOK1(1001,'H2  ',50,0.,1.,0.)
      CALL QBOOK1(1002,'H3  ',50,0.,1.,0.)
      CALL QBOOK1(1003,'H4  ',50,0.,1.,0.)
      CALL QBOOK1(1004,'XM  ',50,0.,50.,0.)      
      CALL QBOOK1(1005,'PTCH',50,0.,80.,0.)
      CALL QBOOK1(1006,'XT2 ',50,0.,60.,0.)
      CALL QBOOK1(1007,'XA34',50,0.,100.,0.)
      CALL QBOOK1(1008,'XA24',50,0.,100.,0.)
      CALL QBOOK1(1009,'XA14',50,0.,100.,0.)
      CALL QBOOK1(1010,'OBJ3',50,0.,50.,0.)
      CALL QBOOK1(1011,'OBJ4',50,0.,50.,0.)
      CALL QBOOK1(1012,'EJ1 ',50,20.,100.,0.)
      CALL QBOOK1(1013,'ASY1',50,0.,1.,0.)  
      CALL QBOOK1(1014,'ASY6',50,0.,1.,0.)
      CALL QBOOK1(1015,'ANG1',50,-1.,1.,0.)
      CALL QBOOK1(1016,'ANG2',50,-1.,1.,0.)
      CALL QBOOK1(1017,'ANG4',50,-1.,1.,0.)
C
C
C
C
C.....Distribution of the NN output
C     -----------------------------
      CALL QBOOK1 (701 , 'NN output' ,50,-1.,1.,0.) 
C
C
C       
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine HISTOINI161                                                                        
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE HISTOINI172(IPLOT)  
C     Initialization of histogramms @ 172 GeV                       
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h"      
C       
#include "qmacro.h"
C
C
C.....Distribution of the 21 NN inputs
C      --------------------------------
      CALL QBOOK1(1001,'APLA   ',50,0.,1.,0.)
      CALL QBOOK1(1002,'H0     ',50,0.,1.,0.)
      CALL QBOOK1(1003,'H2     ',50,0.,1.,0.)
      CALL QBOOK1(1004,'H3     ',50,0.,1.,0.)
      CALL QBOOK1(1005,'H4     ',50,0.,1.,0.)
      CALL QBOOK1(1006,'EF     ',50,100.,200.,0.)
      CALL QBOOK1(1007,'OBL    ',50,0.,1.,0.)
      CALL QBOOK1(1008,'PCH    ',50,0.,500.,0.)
      CALL QBOOK1(1009,'XT1    ',50,0.,150.,0.)
      CALL QBOOK1(1010,'OBJ1   ',50,0.,50.,0.)
      CALL QBOOK1(1011,'OBJ4   ',50,0.,50.,0.)
      CALL QBOOK1(1012,'ANGTOT ',50,-2.,0.,0.)
      CALL QBOOK1(1013,'ETM1   ',50,0.,100.,0.)
      CALL QBOOK1(1014,'RT1    ',50,0.,1.,0.)
      CALL QBOOK1(1015,'RT2    ',50,0.,1.,0.)
      CALL QBOOK1(1016,'RT3    ',50,0.,1.,0.)
      CALL QBOOK1(1017,'XMSTOT ',50,0.,100.,0.)
      CALL QBOOK1(1018,'XAMAX  ',50,0.,200.,0.)
      CALL QBOOK1(1019,'SOMPROB',50,-10.,0.,0.)  
      CALL QBOOK1(1020,'SPROB  ',50,0.,10.,0.)
      CALL QBOOK1(1021,'SPBEVT ',50,0.,1.,0.)
C
C
C
C
C
C.....Distribution of the NN output
C     -----------------------------
      CALL QBOOK1 (701 , 'NN output' ,50,-1.,1.,0.) 
C
C
C       
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine HISTOINI172                                                                        
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE NNINPUT161(XINPUT,IPRESEL,IPLOT,IENERGY,IERNNT)  
C     Compute NN inputs @ 161 GeV                                                                 
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h"  
C
      COMMON/TABJET/JJMAX(4),QEM(4),TAX(6),TT(4),TTB(4),TL(4)
C
      DIMENSION FOXWOL(5),XINPUT(30)
C       
#include "qmacro.h"
C
C
C We use a 3-layered feed forward Neural Network trained by back propagation of 
C the errors. Its architecture is 17, 15, 1 neurons in, respectively, the
C first, second and last layer.
C
C
C
C............Compute the Neural Network inputs : 17 variables
C
C
C............Variables number 1, 2 and 3 : Fox Wolfram Moments : H2, H3, H4
             CALL QJFOXW(FOXWOL,KRECO)                                                                                                               
             H2 = FOXWOL(3)                                                              
             H3 = FOXWOL(4)                                                              
             H4 = FOXWOL(5)                 
C              
C
C............Variable number 4 : Missing Mass : XM
             CALL QJMISS(PMISS,'MISS-VECTOR',KRECO,0)                                  
             IMISS = KPDIR('MISS-VECTOR',KRECO) 
             IF (IMISS.NE.0) THEN                                                                                              
                XM = QM(IMISS) 
             ELSE 
                IERNNT=4 
                RETURN                                                             
             ENDIF  
C
C
C
C............Variable number 5 : Transverse Momentum of all charged tracks : PTCH
             PXCHT=0.                                                                 
             PYCHT=0.                                                                                                                        
             DO 772 JCH=KFCHT,KLCHT                                                   
                PXCHT=PXCHT+QX(JCH)                                                 
                PYCHT=PYCHT+QY(JCH)                                                                                         
 772         CONTINUE                   
             PTCH = SQRT((PXCHT)**2+(PYCHT)**2)    
C
C
C
C...............Variables number : 
C                6 - Transverse Momentum of Jet 2 : XT2  
C            7,8,9 - Invariant masses for 3 dijet combinations : XA34,XA24,XA14
C            10,11 - Number of objects for jets 3 and 4 : OBJ3,OBJ4 
C            13,14 - Asymetries : ASY1,ASY6
C         15,16,17 - Angles between Jets : ANG1,ANG2,ANG4
C
C
C
C...............Clusterisation with Jade algorithm to force in 4 jets                                                                                           
                CALL CLUSJET4(IENERGY,NJET,'Jetdir')
C  
C               Cut on NJET
                IF (NJET.NE.4) THEN
                   IERNNT=4 
                   RETURN
                ENDIF
C
C
C
C        
C                                                    
C............Classification of jets : from the most energetic to the less
C                                     energetic one 
             CALL QEJJ4(NJET,'Jetdir')  
C                                                          
C                                                                               
C                                     
C                                                                                                                 
C............Compute invariant masses for both dijet combination
             CALL XADIJET(IERRX)
C     
C
C            Error flag
             IF (IERRX.EQ.1) THEN
                IERNNT=4 
                RETURN
             ENDIF
C
C
             XT2 = QPT(JJMAX(2))
             XA34 = TAX(2)
             XA24 = TAX(4)
             XA14 = TAX(5)
C
C
C............Number of objects for jets 3 and 4 : OBJ3, OBJ4                                                                                                  
             ICOJE3=0
             ICOJE4=0                                                                                                                         
             DO 31 J = KFEFT,KLEFT                                                                                
                IF(XSAME(JJMAX(3),J)) ICOJE3 = ICOJE3 + 1                                
                IF(XSAME(JJMAX(4),J)) ICOJE4 = ICOJE4 + 1                                             
 31          CONTINUE                                                                                                                                                                         
             OBJ3=FLOAT(ICOJE3)                                                        
             OBJ4=FLOAT(ICOJE4)                                                       
C............Asymetries : ASY1 and ASY6
             ASY1 = ABS((QP(JJMAX(1))-QP(JJMAX(4)))/
     &                  (QP(JJMAX(1))+QP(JJMAX(4))))
             ASY6 = ABS((QP(JJMAX(3))-QP(JJMAX(4)))/
     &                  (QP(JJMAX(3))+QP(JJMAX(4))))
C
C
C............Angles between Jets : ANG1, ANG2, ANG4
             ANG1 = QCOSA(JJMAX(1),JJMAX(2))                                                
             ANG2 = QCOSA(JJMAX(1),JJMAX(3))                                                                                     
             ANG4 = QCOSA(JJMAX(2),JJMAX(3))                                               
C              
C
C............Variable number 12 : Energy of Jet 1 : EJ1
             EJ1 = QE(JJMAX(1))
C
C
C
C............Put these 17 variables into NN inputs
             XINPUT(1) = H2
             XINPUT(2) = H3
             XINPUT(3) = H4
             XINPUT(4) = XM
             XINPUT(5) = PTCH
             XINPUT(6) = XT2
             XINPUT(7) = XA34
             XINPUT(8) = XA24
             XINPUT(9) = XA14
             XINPUT(10) = OBJ3
             XINPUT(11) = OBJ4
             XINPUT(12) = EJ1
             XINPUT(13) = ASY1
             XINPUT(14) = ASY6
             XINPUT(15) = ANG1
             XINPUT(16) = ANG2
             XINPUT(17) = ANG4
C
C
C
C............Distribution of NN inputs
C     
             IF (IPLOT.EQ.1) THEN
                CALL HFILL (1000 +  1, XINPUT(1) ,0.,1.)
                CALL HFILL (1000 +  2, XINPUT(2) ,0.,1.)
                CALL HFILL (1000 +  3, XINPUT(3) ,0.,1.)
                CALL HFILL (1000 +  4, XINPUT(4) ,0.,1.)
                CALL HFILL (1000 +  5, XINPUT(5) ,0.,1.)
                CALL HFILL (1000 +  6, XINPUT(6) ,0.,1.)
                CALL HFILL (1000 +  7, XINPUT(7) ,0.,1.)
                CALL HFILL (1000 +  8, XINPUT(8) ,0.,1.)
                CALL HFILL (1000 +  9, XINPUT(9) ,0.,1.)
                CALL HFILL (1000 + 10, XINPUT(10) ,0.,1.)
                CALL HFILL (1000 + 11, XINPUT(11) ,0.,1.)
                CALL HFILL (1000 + 12, XINPUT(12) ,0.,1.)
                CALL HFILL (1000 + 13, XINPUT(13) ,0.,1.)
                CALL HFILL (1000 + 14, XINPUT(14) ,0.,1.)
                CALL HFILL (1000 + 15, XINPUT(15) ,0.,1.)
                CALL HFILL (1000 + 16, XINPUT(16) ,0.,1.)
                CALL HFILL (1000 + 17, XINPUT(17) ,0.,1.)
             ENDIF
C
C
C       
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine NNINPUT161                                                                     
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE NNINPUT172(XINPUT,IPRESEL,IPLOT,IENERGY,IERNNT)  
C     Compute NN inputs @ 172 GeV                                                                 
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h"  
C
      COMMON/TABJET/JJMAX(4),QEM(4),TAX(6),TT(4),TTB(4),TL(4)
C
      DIMENSION FOXWOL(5),EIGVAL(3),XINPUT(30)
C       
#include "qmacro.h"
C
C
C We use a 3-layered feed forward Neural Network trained by back propagation of 
C the errors. Its architecture is 21, 17, 1 neurons in, respectively, the
C first, second and last layer.
C
C
C
C............Compute the Neural Network inputs : 21 variables
C
C
C............Variable number 1 : Aplanarity : APLA
             CALL QJEIG(EIGVAL,'EIGENVEC',KRECO)
             APLA = 1.5*EIGVAL(3)
C
C
C............Variables number 2,3,4 and 5 : Fox Wolfram Moments : H0,H2,H3,H4
             CALL QJFOXW(FOXWOL,KRECO)                                                                                                               
             H0 = FOXWOL(1)      
             H2 = FOXWOL(3)                                                              
             H3 = FOXWOL(4)                                                              
             H4 = FOXWOL(5)           
C              
C
C............Variable number 6 : Energy Flow : EF
             EF = 0.
             EFL = 0. 
             DO 71 JP=KFEFT,KLEFT
                EFL = EFL+QE(JP)                                                                                     
 71          CONTINUE
             EF = EFL  
C              
C
C............Variable number 7 : Oblatness : OBL
             ICLASS=KRECO
             IFTRK=KFEFT
             ILTRK=KLEFT
             CALL OBLCAL(ICLASS,KERREUR,IFTRK,ILTRK,OBL)
C     
C            Error flag
             IF (KERREUR.EQ.1) THEN
                IERNNT=4  
                RETURN
             ENDIF
C
C
C
C............Variable number 8 : Momentum of all charged tracks : PCH
             PXCHT=0.                                                                 
             PYCHT=0.
             PZCHT=0.                                                                                                                        
             DO 772 JCH=KFCHT,KLCHT                                                   
                PXCHT=PXCHT+QX(JCH)                                                 
                PYCHT=PYCHT+QY(JCH)   
                PZCHT=PZCHT+QZ(JCH)                                                                                         
 772         CONTINUE                   
             PCH = SQRT((PXCHT)**2+(PYCHT)**2+(PZCHT)**2)
C
C
             IF (IPRESEL.EQ.0) THEN
C
C...............Clusterisation with Jade algorithm to force in 4 jets                                                                                           
                CALL CLUSJET4(IENERGY,NJET,'Jetdir')
C  
C               Cut on NJET
                IF (NJET.NE.4) THEN
                   IERNNT=4 
                   RETURN
                ENDIF
C        
C                                                    
C...............Classification of jets : from the most energetic to the less
C                                        energetic one 
                CALL QEJJ4(NJET,'Jetdir')  
C                                                          
C                                                                               
C
             ENDIF     ! of (IPRESEL.EQ.0)
C
C                                     
C                                                                                                                 
C............Compute invariant masses for both dijet combination
             CALL XADIJET(IERRX)
C     
C            Error flag
             IF (IERRX.EQ.1) THEN
                IERNNT=4  
                RETURN
             ENDIF
C
C
C............Variable number 9 : Transverse Momentum of Jet1 : XT1
             XT1 = QPT(JJMAX(1))
C
C
C
C............Variables number 10, 11 : 
C            Number of objects for jets 1 and 4 : OBJ1, OBJ4                                                                                                  
             ICOJE1=0
             ICOJE4=0                                                                                                                         
             DO 31 J = KFEFT,KLEFT                                                                                
                IF(XSAME(JJMAX(1),J)) ICOJE1 = ICOJE1 + 1                                
                IF(XSAME(JJMAX(4),J)) ICOJE4 = ICOJE4 + 1                                             
 31          CONTINUE                                                                                                                                                                         
             OBJ1=FLOAT(ICOJE1)                                                        
             OBJ4=FLOAT(ICOJE4)
C
C
C............Variable number 12 :
C            Sum of Cosinus between Jets : ANGTOT
             ANG1 = QCOSA(JJMAX(1),JJMAX(2))                                                
             ANG2 = QCOSA(JJMAX(1),JJMAX(3)) 
             ANG3 = QCOSA(JJMAX(1),JJMAX(4))                                                                                     
             ANG4 = QCOSA(JJMAX(2),JJMAX(3))
             ANG5 = QCOSA(JJMAX(2),JJMAX(4))
             ANG6 = QCOSA(JJMAX(3),JJMAX(4))
             ANGTOT = ANG1+ANG2+ANG3+ANG4+ANG5+ANG6                                              
C
C
C............Variable number 13 :
C            The most energetic charged track of Jet1 : ETM1 
             ETMM1=-10.00
             DO 812 ITK=KFCHT,KLCHT
                ET=0.00
                IF (XSAME(JJMAX(1),ITK)) THEN
                   ET=QE(ITK)
                   IF (ET.GT.ETMM1) THEN
                      ETMM1=ET
                   ENDIF
                 ENDIF
 812         CONTINUE
             ETM1 = ETMM1 
C     
C            Error flag
             IF (ETM1.EQ.-10.00) THEN
                IERNNT=4  
                RETURN
             ENDIF
C
C
C
C............Variables number 14, 15, 16 :
C            Maximum fraction of jet energy carried by one object :
C            RT1, RT2, RT3
             ET1 = -1.
             ET2 = -1. 
             ET3 = -1.
             ET4 = -1.                        
             DO 912 ITE = KFEFT,KLEFT
                ET = QE(ITE)                                
                IF (XSAME(JJMAX(1),ITE)) THEN      
                   IF (ET.GT.ET1) ET1=ET
                ENDIF                        
                IF (XSAME(JJMAX(2),ITE)) THEN
                   IF (ET.GT.ET2) ET2=ET
                ENDIF                        
                IF (XSAME(JJMAX(3),ITE)) THEN 
                   IF (ET.GT.ET3) ET3=ET
                ENDIF                              
                IF (XSAME(JJMAX(4),ITE)) THEN
                   IF (ET.GT.ET4) ET4=ET
                ENDIF                   
 912         CONTINUE
C
             RT1 = 1.
             RT2 = 1.
             RT3=  1.
             RT4 = 1.
             IF (QE(JJMAX(1)).NE.0.00) RT1 = ET1/QE(JJMAX(1))      
             IF (QE(JJMAX(2)).NE.0.00) RT2 = ET2/QE(JJMAX(2))      
             IF (QE(JJMAX(3)).NE.0.00) RT3 = ET3/QE(JJMAX(3))   
C     
C            Error flag
             IF (RT1.EQ.1.OR.RT2.EQ.1.OR.RT3.EQ.1.) THEN
                IERNNT=4  
                RETURN
             ENDIF
C
C
C
C............Variable number 17 :
C            Maximum of minimum Invariant mass for dijet combination :
C            XMSTOT
             IF (TAX(1).GT.TAX(2)) THEN
                XMS1 = TAX(2)
             ELSE
                XMS1 = TAX(1)
             ENDIF
             IF (TAX(3).GT.TAX(4)) THEN
                XMS2 = TAX(4)
             ELSE
                XMS2 = TAX(3)
             ENDIF
             IF (TAX(5).GT.TAX(6)) THEN
                XMS3 = TAX(6)
             ELSE
                XMS3 = TAX(5)
             ENDIF
C
             IF (XMS1.GT.XMS2) XMSTOT = XMS1
             IF (XMS3.GT.XMSTOT) XMSTOT = XMS3   
C
C
C
C............Variable number 18 : Maximum Invariant Mass : XAMAX
             XAMAX = -100.00
             DO L=1,6
                IF (TAX(L).GT.XAMAX) XAMAX = TAX(L)
             ENDDO
C     
C            Error flag
             IF (XAMAX.EQ.-100.00) THEN
                IERNNT=4  
                RETURN
             ENDIF
C
C
C
C............Variables number 19, 20, 21 :
C            QIPBTAG Variables : SOMPROB, SPROB, SPBEVT
C
             CALL PODQBTG2(JETQIB2,SOMPROB2,SPROB2,SPBEVT2,IERT)
C     
C            Error flag
             IF (IERT.NE.0) THEN
                IERNNT=4  
                RETURN
             ENDIF
C
             SOMPROB = SOMPROB2
             SPROB = SPROB2                               
             SPBEVT = SPBEVT2             
C     
C
C
C
C............Put these 21 variables into NN inputs
             XINPUT(1) = APLA
             XINPUT(2) = H0
             XINPUT(3) = H2
             XINPUT(4) = H3
             XINPUT(5) = H4
             XINPUT(6) = EF
             XINPUT(7) = OBL
             XINPUT(8) = PCH
             XINPUT(9) = XT1
             XINPUT(10) = OBJ1
             XINPUT(11) = OBJ4
             XINPUT(12) = ANGTOT
             XINPUT(13) = ETM1
             XINPUT(14) = RT1
             XINPUT(15) = RT2
             XINPUT(16) = RT3
             XINPUT(17) = XMSTOT
             XINPUT(18) = XAMAX
             XINPUT(19) = SOMPROB
             XINPUT(20) = SPROB
             XINPUT(21) = SPBEVT
C
C
C
C............Distribution of NN inputs
C     
             IF (IPLOT.EQ.1) THEN
                CALL HFILL (1000 +  1, XINPUT(1) ,0.,1.)
                CALL HFILL (1000 +  2, XINPUT(2) ,0.,1.)
                CALL HFILL (1000 +  3, XINPUT(3) ,0.,1.)
                CALL HFILL (1000 +  4, XINPUT(4) ,0.,1.)
                CALL HFILL (1000 +  5, XINPUT(5) ,0.,1.)
                CALL HFILL (1000 +  6, XINPUT(6) ,0.,1.)
                CALL HFILL (1000 +  7, XINPUT(7) ,0.,1.)
                CALL HFILL (1000 +  8, XINPUT(8) ,0.,1.)
                CALL HFILL (1000 +  9, XINPUT(9) ,0.,1.)
                CALL HFILL (1000 + 10, XINPUT(10) ,0.,1.)
                CALL HFILL (1000 + 11, XINPUT(11) ,0.,1.)
                CALL HFILL (1000 + 12, XINPUT(12) ,0.,1.)
                CALL HFILL (1000 + 13, XINPUT(13) ,0.,1.)
                CALL HFILL (1000 + 14, XINPUT(14) ,0.,1.)
                CALL HFILL (1000 + 15, XINPUT(15) ,0.,1.)
                CALL HFILL (1000 + 16, XINPUT(16) ,0.,1.)
                CALL HFILL (1000 + 17, XINPUT(17) ,0.,1.)
                CALL HFILL (1000 + 18, XINPUT(18) ,0.,1.)
                CALL HFILL (1000 + 19, XINPUT(19) ,0.,1.)
                CALL HFILL (1000 + 20, XINPUT(20) ,0.,1.)
                CALL HFILL (1000 + 21, XINPUT(21) ,0.,1.)
             ENDIF
C
C
C       
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  subroutine NNINPUT172                                                                     
C=====================================================================C
C
C
C 
C=====================================================================C         
      SUBROUTINE PODQBTG2(JETQIB2,SOMPROB2,SPROB2,SPBEVT2,IERT)  
C     QIPBTAG Variables  
C
C     Outputs: JETQIB, SOMPROB, SPROB, SPBEVT
C              IERT : Error flag              
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h" 
C
      DIMENSION PRJET(40),ITRK(40),IFRF2(400),PRTRK(400),PRHEMI(2)
C
#include "qmacro.h"
C
C                          
      IERT = 0
C
      IF (XMCEV) THEN
         CALL QIPBTAG(IRET,NTRK,NJET,ITRK,IFRF2,PRTRK,PRJET, 
     &                PRHEMI,PREVT)
      ELSE
         IF (XVDEOK(DUMMY).AND.XGETBP.AND.QVCHIF(KFREV).GT.0.) THEN
            CALL QIPBTAG(IRET,NTRK,NJET,ITRK,IFRF2,PRTRK,PRJET, 
     &                   PRHEMI,PREVT)
         ELSE
            IRET=1
         ENDIF                      
      ENDIF
C
      JETQIB2=NJET
C
      SOMPROB2 = 0.00
      SPROB2 = 0.00                               
      IF (NJET.GE.1) THEN                             
         DO I=1,NJET                                                        
           IF (PRJET(I).GT.0.0) SOMPROB2=SOMPROB2+LOG10(PRJET(I))            
           IF (PRJET(I).GT.0.0) SPROB2=SPROB2+PRJET(I)            
         END DO
      ENDIF
      SPBEVT2 = PREVT
C                                                                               
C                                                                               
      RETURN                                                                    
      END
C---------------------------------------------------------------------C    
C                    End  of  subroutine PODQBTG2                                                                   
C=====================================================================C
C
C 
C
C=====================================================================C
      SUBROUTINE OBLCAL(ICLASS,KERREUR,IFTRK,ILTRK,OBL)                         
C     Variable OBLATNESS  
C
C     Inputs :
C          ICLASS      : KRECO (KMONTE)
C          IFTRK,ILTRK : for example : KFEFT,KLEFT
C     
C     Outputs :
C          KERREUR     : flag=0 Ok; flag=1 Not ok
C          OBL         : Oblatness
C
C---------------------------------------------------------------------C                      
#include "qcde.h"  
C                                           
      DIMENSION EIGVAL(3)                                                       
C
#include "qmacro.h"                                           
C
      KERREUR=0                                                                 
C                                                                               
      CALL QJEIG(EIGVAL,'TOTO',ICLASS)                                          
      I1=KPDIR('TOTO',ICLASS)                                                   
      I2=KFOLLO(I1)                                                             
      I3=KFOLLO(I2)                                                             
C                                                                               
      ET=0.                                                                     
      FMA=0.                                                                    
      FMI=0.                                                                    
C                                                                               
      IF ((QP(I2).LT.1.E-12).OR.(QP(I3).LT.1.E-12)) THEN                         
         KERREUR=1                                                          
         GOTO 100                                                           
      ENDIF                                                                    
C                                                                               
      DO 1 ITK=IFTRK,ILTRK                                                   
           IF(XLOCK(ITK))GOTO 1                                            
           ET=ET+QE(ITK)                                                        
           FMA=FMA+ABS((QX(ITK)*QX(I2)+QY(ITK)*QY(I2)+QZ(ITK)*QZ(I2))*          
     +     QE(ITK)/(QP(I2)*QP(ITK)))                                            
           FMI=FMI+ABS((QX(ITK)*QX(I3)+QY(ITK)*QY(I3)+QZ(ITK)*QZ(I3))*          
     +     QE(ITK)/(QP(I3)*QP(ITK)))                                            
   1  CONTINUE                                                                  
C                                                                               
      IF (ET.LT.E-12) THEN                                                 
         KERREUR=1                                                       
         GOTO 100                                                       
      ENDIF                                                                
C                                                                               
      OBL=(FMA-FMI)/ET                                                  
      OBL=ABS(OBL)                                                       
C
  100 CONTINUE 
C                                                                 
      RETURN                                                                    
      END    
C---------------------------------------------------------------------C    
C                    End  of  subroutine OBLCAL                                                                    
C=====================================================================C
C
C
C
C
C=====================================================================C         
      SUBROUTINE RESINI(IENERGY)  
C     Neural Network Initialization                                                                  
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h"  
C
      COMMON/NN161/NEURON161(3),WPOIDS161(2,30,30),A161(30),B161(30),
     & XMIN161(30),XMAX161(30),SOMOY161(30),SOM161(30),NIVMAX161  
C
      COMMON/NN172/NEURON172(3),WPOIDS172(2,30,30),A172(30),B172(30),
     & XMIN172(30),XMAX172(30),SOMOY172(30),SOM172(30),NIVMAX172                                     
C
#include "qmacro.h"                                           
C                         
C
C                                                      
C.....Initialization of NN parameters and NN weights : 
C     ------------------------------------------------  
C
      IF (IENERGY.EQ.161) THEN 
         CALL RESINI161
C
C........Compute parameters for normalization : -1. < NN inputs < 1.
         DO 447 K=1,NEURON161(1)                                                      
            A161(K)=2.00/(XMAX161(K)-XMIN161(K))                                                 
            B161(K)=(XMAX161(K)+XMIN161(K))/(XMIN161(K)-XMAX161(K))                        
 447     CONTINUE 
C 
      ENDIF
C                                                                               
C
      IF (IENERGY.EQ.172) THEN 
         CALL RESINI172
C
C........Compute parameters for normalization : -1. < NN inputs < 1.
         DO 448 K=1,NEURON172(1)                                                      
            A172(K)=2.00/(XMAX172(K)-XMIN172(K))                                                 
            B172(K)=(XMAX172(K)+XMIN172(K))/(XMIN172(K)-XMAX172(K))                        
 448     CONTINUE 
C 
      ENDIF
C     
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  Subroutine  RESINI                                                                      
C=====================================================================C
C
C
C
C=====================================================================C                                                
      SUBROUTINE RESNEUR(IENERGY,XINPUT,OUTNN)  
C     This subroutine returns : OUTNN = NN output                       
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h"  
C
      COMMON/NN161/NEURON161(3),WPOIDS161(2,30,30),A161(30),B161(30),
     & XMIN161(30),XMAX161(30),SOMOY161(30),SOM161(30),NIVMAX161                                        
C
      COMMON/NN172/NEURON172(3),WPOIDS172(2,30,30),A172(30),B172(30),
     & XMIN172(30),XMAX172(30),SOMOY172(30),SOM172(30),NIVMAX172                                     
C
      REAL OUTNN
C
      DIMENSION XINPUT(30)                             
      DIMENSION X(3,30),Y(3,30)                                               
      DIMENSION XINPUT2(30)                                 
C                                           
#include "qmacro.h"  
C   
C                                                                                  
C.....Normalization of NN inputs between -1. and 1. 
C     ---------------------------------------------                                                                         
      IF (IENERGY.EQ.161) THEN 
         DO 2291 K=1,NEURON161(1)+1                                                    
            IF (K.EQ.NEURON161(1)+1) THEN                                             
               XINPUT2(K)=1.E+00                                                                                                                       
            ELSE 
               XINPUT(K)=(XINPUT(K)-SOMOY161(K))/SQRT(SOM161(K))                                                                 
               IF (XINPUT(K).GT.XMAX161(K)) XINPUT(K)=XMAX161(K)                         
               IF (XINPUT(K).LT.XMIN161(K)) XINPUT(K)=XMIN161(K)                         
               XINPUT2(K)=A161(K)*XINPUT(K)+B161(K)                                                                 
            ENDIF                                                                  
 2291    CONTINUE                                                                  
C 
C                                                                              
C..........Computation of NN                                                      
C          =================                                                      
C          Input layer                                                             
C          -----------                                                             
           DO 1291 IM=1,NEURON161(1)+1                                             
              Y(1,IM)=XINPUT2(IM)                                
 1291      CONTINUE                                                             
C                                                                               
C          Hidden(s) and output layers                                                       
C          ---------------------------                                              
           DO 1491 NIV=1,NIVMAX161-1                                                
               DO 1491 J=1,NEURON161(NIV+1)                                         
                  X(NIV+1,J)=0.E+00                                             
                  DO 3491 K=1,NEURON161(NIV)+1                                      
                  X(NIV+1,J)=X(NIV+1,J)+Y(NIV,K)*WPOIDS161(NIV,K,J)             
 3491             CONTINUE                                                                                                                         
C                                                                                                                    
                  Y(NIV+1,J)=(1.E+00-EXP(-X(NIV+1,J)))/                
     &                       (1.E+00+EXP(-X(NIV+1,J)))                                                                           
C                                                                 
                  IF (J.EQ.NEURON161(NIV+1)) THEN                                  
                     Y(NIV+1,J+1)=1.E+00                                        
                  ENDIF                                                                                                               
C                                                                               
 1491      CONTINUE                                                             
C                    
C                            
C..........NN output                                                       
C          ========= 
           OUTNN = Y(NIVMAX161,1)                                                          
C
C
           ENDIF     !  from (IENERGY.EQ.161)
C                                                                               
C   
C                                                                                  
C.....Normalization of NN inputs between -1. and 1. 
C     ---------------------------------------------                                                                         
      IF (IENERGY.EQ.172) THEN 
         DO 2292 K=1,NEURON172(1)+1                                                    
            IF (K.EQ.NEURON172(1)+1) THEN                                             
               XINPUT2(K)=1.E+00                                                                                                                       
            ELSE 
               XINPUT(K)=(XINPUT(K)-SOMOY172(K))/SQRT(SOM172(K))                                                                 
               IF (XINPUT(K).GT.XMAX172(K)) XINPUT(K)=XMAX172(K)                         
               IF (XINPUT(K).LT.XMIN172(K)) XINPUT(K)=XMIN172(K)                         
               XINPUT2(K)=A172(K)*XINPUT(K)+B172(K)                                                                 
            ENDIF                                                                  
 2292    CONTINUE                                                                  
C 
C                                                                              
C..........Computation of NN                                                      
C          =================                                                      
C          Input layer                                                             
C          -----------                                                             
           DO 1292 IM=1,NEURON172(1)+1                                             
              Y(1,IM)=XINPUT2(IM)                                
 1292      CONTINUE                                                             
C                                                                               
C          Hidden(s) and output layers                                                       
C          ---------------------------                                              
           DO 1492 NIV=1,NIVMAX172-1                                                
               DO 1492 J=1,NEURON172(NIV+1)                                         
                  X(NIV+1,J)=0.E+00                                             
                  DO 3492 K=1,NEURON172(NIV)+1                                      
                  X(NIV+1,J)=X(NIV+1,J)+Y(NIV,K)*WPOIDS172(NIV,K,J)             
 3492             CONTINUE                                                                                                                         
C                                                                                                                    
                  Y(NIV+1,J)=(1.E+00-EXP(-X(NIV+1,J)))/                
     &                       (1.E+00+EXP(-X(NIV+1,J)))                                                                           
C                                                                 
                  IF (J.EQ.NEURON172(NIV+1)) THEN                                  
                     Y(NIV+1,J+1)=1.E+00                                        
                  ENDIF                                                                                                               
C                                                                               
 1492       CONTINUE                                                             
C                    
C                            
C..........NN output                                                       
C          ========= 
           OUTNN = Y(NIVMAX172,1)                                                          
C
C
           ENDIF     !  from (IENERGY.EQ.172)
C                                                                                                                                                                                                                                                                                                                                            
C                                                                               
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  Subroutine  RESNEUR                                                                      
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE RESINI161  
C     Version 1.0 : Neural Network Initialization @ 161 Gev                                                                 
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h"  
C  
      COMMON/NN161/NEURON161(3),WPOIDS161(2,30,30),A161(30),B161(30),
     & XMIN161(30),XMAX161(30),SOMOY161(30),SOM161(30),NIVMAX161                                         
C
#include "weights161.h"  
C
#include "qmacro.h" 
C                                                                               
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  Subroutine  RESINI161                                                                      
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE RESINI172  
C     Version 1.1 : Neural Network Initialization @ 172 Gev                                                                 
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h"  
C  
      COMMON/NN172/NEURON172(3),WPOIDS172(2,30,30),A172(30),B172(30),
     & XMIN172(30),XMAX172(30),SOMOY172(30),SOM172(30),NIVMAX172                                         
C
#include "weights172v21.h"  
C
#include "qmacro.h" 
C                                                                               
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  Subroutine  RESINI172                                                                      
C=====================================================================C
C
C
C
C=====================================================================C         
      SUBROUTINE OUTVERS(IPRESEL,IPLOT,IENERGY)  
C                                                                    
C                                                                                   
C---------------------------------------------------------------------C         
#include "qcde.h"  
C
#include "qmacro.h" 
C                                                                               
C
       WRITE(19,1211)'     QQQ   N   N N   N W     W TTTTT  A    GGG'
 1211  FORMAT(A46)
       
       WRITE(19,1212)'    Q   Q  NN  N NN  N W     W   T   A A  G   ',
     & '             Version 1.3'
 1212  FORMAT(A46,A24) 
       WRITE(19,1213)'    Q  QQ  N N N N N N W  W  W   T  A   A G  GG'
 1213  FORMAT(A47)
       WRITE(19,1214)'     QQQQ  N  NN N  NN W W W W   T  AAAAA G   G',
     & '          11th March 1997'
 1214  FORMAT(A47,A25)      
       WRITE(19,1211)'         Q N   N N   N WW   WW   T  A   A  GGG '
       WRITE(19,1211)'    -------------------------------------------'
C
C
       WRITE(19,*) 
       WRITE(19,1215)'    Last Modified:    01/07/97'  
 1215  FORMAT(A31) 
C
       WRITE(19,*) 
       WRITE(19,1216) 'You use QNNWTAG Version 1.3 with',
     & ' these Input parameters :'
 1216  FORMAT(A32,A25)
       WRITE(19,*) 
       WRITE(19,1217)' -->  IPRESEL = ',IPRESEL
       WRITE(19,1217)' -->  IPLOT   = ',IPLOT
       WRITE(19,1217)' -->  IENERGY = ',IENERGY
 1217  FORMAT(A16,I3) 
C       
       WRITE(19,*) 
       WRITE(19,*) 
       WRITE(19,1218)'For more informations about this version,',
     & ' please read the Web pages at :'
 1218  FORMAT(A41,A31)
       WRITE(19,1219)'    http://alephwww.cern.ch/~edgecock/',
     & 'ww_jets/tools/qnnwtag.html'         
 1219  FORMAT(A38,A26)
C
C
      RETURN                                                                    
      END 
C---------------------------------------------------------------------C    
C                    End  of  Subroutine  OUTVERS                                                                      
C=====================================================================C











