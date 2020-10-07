      SUBROUTINE QMININC(IER)
C
CKEY INIT CARDS /INTERNAL
C-----------------------------------------------------------------------
C! Data cards default initialisation for official MINI making
C  Called from QMCARD
C     Author: J. Boucrot       23-Sep-98
C
C Output argument :
C  IER = 0 if all OK
C      = 1 if no room to book banks
C
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
C-----------------------------------------------------------------------
      IER=0
C Do-nothing if 'MINP' card not present :
      IF (IW(NAMIND('MINP')).LE.0) GO TO 999
C Drop all existing MINI-production-related banks :
      CALL BDROP(IW,'MINIEFLJQFNDCOMPQLID')
      CALL BDROP(IW,'LSHDLSETLSECLSCVLSMTLSMCLSJT')
      CALL QWMESS('_QMININC_ Setting up MINI production cards')
C
C Initialise the MINI card :
C
      CALL AUBOS('MINI',0,0,JMINI,IGARB)
      IF (IGARB.EQ.2) GO TO 900
C
C Initialise data card for Energy Flow :
C EFLJ                  ! ENFLW and Jets                                        
C
      CALL AUBOS('EFLJ',0,0,JEFLJ,IGARB)
      IF (IGARB.EQ.2) GO TO 900
C
C Initialise data card for QFNDIP :
C QFND                  ! call QFNDIP
C
      CALL AUBOS('QFND',0,0,JQFND,IGARB)
      IF (IGARB.EQ.2) GO TO 900
C
C Initialise the Unpacking for MINIs (PTPX bank) :
C UNPK  'AL SO'
C
      CALL BKFMT('UNPK','2A')
      CALL AUBOS('UNPK',0,2,JUNPK,IGARB)
      IF (IGARB.EQ.2) GO TO 900   
      IW(JUNPK+1)=INTCHA('AL S')
      IW(JUNPK+2)=INTCHA('O   ')
C
C Initialise data compression card :                                           
C COMP 'INTE' 'NOFM'                                                             
C
      CALL BKFMT('COMP','2A')
      CALL AUBOS('COMP',0,2,JCOMP,IGARB)
      IF (IGARB.EQ.2) GO TO 900
      IW(JCOMP+1)=INTCHA('INTE')
      IW(JCOMP+2)=INTCHA('NOFM')
C
C Initialise data card to trigger the execution of QSELEP :
C QLID                                                                            
C
      CALL AUBOS('QLID',0,0,JQLID,IGARB)
      IF (IGARB.EQ.2) GO TO 900
C
C --------- Initialise data cards for QSELEP selection cuts --------
C These cuts are different from the default ones of ALPHA ----------
C  
C Initialise data card LSHD for QSELEP :
C-- *  #tracks e_frac #tpc  d0   z0   cos(theta_max)                             
C LSHD    5     0.10    4  2.0  10.0    0.95                                      
C
      CALL BKFMT('LSHD','I,F,I,3F')
      LLSHD = 6
      CALL AUBOS('LSHD',0,LLSHD,JLSHD,IGARB)
      IF (IGARB.EQ.2) GO TO 900
      IW(JLSHD+1) = 5
      RW(JLSHD+2) = 0.10
      IW(JLSHD+3) = 4
      RW(JLSHD+4) = 2.0
      RW(JLSHD+5) = 10.0
      RW(JLSHD+6) = 0.95
C
C Initialise data card LSET for QSELEP :
C
C--   User card for track selection electron candidates                         
C--                                                                             
C-- *   #tpc  d0   z0   cos(the_max)  p   p_t                                    
C LSET    5  2.0  10.0    0.95       2.0  0.0                                     
C
      CALL BKFMT('LSET','I,5F')
      LLSET = 6
      CALL AUBOS('LSET',0,LLSET,JLSET,IGARB)
      IF (IGARB.EQ.2) GO TO 900
      IW(JLSET+1) = 5
      RW(JLSET+2) = 2.0
      RW(JLSET+3) = 10.0
      RW(JLSET+4) = 0.95
      RW(JLSET+5) = 2.0
      RW(JLSET+6) = 0.0
C
C Initialise data card LSEC for QSELEP :
C--                                                                             
C--   User card electron identification cuts                                    
C--                                                                             
C-- *   rt_min rt_max rl_min rl_max #wires ri_min ri_max                         
C LSEC   -3.0   999.0  -3.0   3.0      0    -999.  999.                           
C
      CALL BKFMT('LSEC','4F,I,2F')
      LLSEC = 7
      CALL AUBOS('LSEC',0,LLSEC,JLSEC,IGARB)
      IF (IGARB.EQ.2) GO TO 900
      RW(JLSEC+1) = -3.0
      RW(JLSEC+2) = 999.0
      RW(JLSEC+3) = -3.0
      RW(JLSEC+4) = 3.0
      IW(JLSEC+5) = 0
      RW(JLSEC+6) = -999.
      RW(JLSEC+7) = 999.
C
C Initialise data card LSCV for QSELEP :
C--                                                                             
C--   User card rejection of photon conversions and dalitz decays               
C--                                                                             
C-- *  XY_max Z_max  M_max   W_conv(1=write,0=reject)                            
C LSCV   1.0    1.0   0.020   1                                                   
C--                                                                             
      CALL BKFMT('LSCV','3F,I')
      LLSCV = 4
      CALL AUBOS('LSCV',0,LLSCV,JLSCV,IGARB)
      IF (IGARB.EQ.2) GO TO 900
      RW(JLSCV+1) = 1.0
      RW(JLSCV+2) = 1.0
      RW(JLSCV+3) = 0.020
      IW(JLSCV+4) = 1
C
C Initialise data cards LSMT for QSELEP :
C--                                                                             
C--   User card for track selection muon candidates                             
C--                                                                             
C-- *  #TPC  d0   z0   cos(the_max)  p   p_t                                     
C LSMT    5  2.0  10.0    0.95       3.0  0.0                                     
C
      CALL BKFMT('LSMT','I,5F')
      LLSMT = 6
      CALL AUBOS('LSMT',0,LLSMT,JLSMT,IGARB)
      IF (IGARB.EQ.2) GO TO 900
      IW(JLSMT+1) = 5
      RW(JLSMT+2) = 2.0      
      RW(JLSMT+3) = 10.0
      RW(JLSMT+4) = 0.95
      RW(JLSMT+5) = 3.0 
      RW(JLSMT+6) = 0.0
C
C Initialise data card LSMC for QSELEP :
C
C--   User card muon identification cuts                                        
C--                                                                             
C-- *  QMUIDO return flags accepted                                             
C LSMC   13  14                                                                   
C--                                                                             
      LLSMC = 2
      CALL AUBOS('LSMC',0,LLSMC,JLSMC,IGARB)
      IF (IGARB.EQ.2) GO TO 900
      IW(JLSMC+1) = 13
      IW(JLSMC+2) = 14
C
C Initilalise data card LSJT for QSELEP :
C--                                                                             
C--   User card jet cuts                                                        
C--                                                                             
C-- *  #objects/jet  Max P(lepton)/E(jet) #jets   Jet Ycut  Jrej                 
C--                                                       1=keep,0=cut          
C LSJT      5                 0.9            2     0.004       1                  
C
      CALL BKFMT('LSJT','I,F,I,F,I')
      LLSJT = 5
      CALL AUBOS('LSJT',0,LLSJT,JLSJT,IGARB)
      IF (IGARB.EQ.2) GO TO 900
      IW(JLSJT+1) = 5
      RW(JLSJT+2) = 0.9 
      IW(JLSJT+3) = 2
      RW(JLSJT+4) = 0.004
      IW(JLSJT+5) = 1
C
      GO TO  999
C
C No space to book banks :
C
 900  IER=1
C
 999  RETURN
      END
#endif
