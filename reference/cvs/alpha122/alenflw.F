      SUBROUTINE QUEFLO                                                         
C----------------------------------------------------------------------
CKEY PACK FILL EFLOW /USER
C! User routine called before QFEFLO
C  to call the energy flow when ALEPH_JULIA flag is ON
C                                                       
C - P.Janot
C----------------------------------------------------------------------         
#ifndef DOC
#include "bcs.h"
C----------------------------------------------------------------------         
      IVERS=0                                                                   
      JALPV=NLINK('ALPV',0)                                                     
      IF (JALPV.EQ.0) GO TO 100                                                 
      IVERS=IW(JALPV+1)                                                         
C                                                                               
C Do nothing if ALPHA version < 115. Will lead to unpredictable                 
C results with ENFLW...                                                         
C                                                                               
      IF ( ivers .LT. 115 ) GO TO 100                                           
C                                                                               
C Only for ALPHA version 115-116-117-118 :                                      
C                                                                               
      IF ( ivers .LE. 118 ) THEN                                                
C                                                                               
C  Create PGPC # 0 if needed                                                    
C                                                                               
        ipgpc  = NLINK('PGPC',0)                                                
        IF ( ipgpc .LE. 0 ) THEN                                                
           CALL gapgpc                                                          
           CALL BLIST(iw,'S+','PGPC')                                           
        ENDIF                                                                   
C                                                                               
C Only for ALPHA version 120 and after :                                        
C                                                                               
      ELSE                                                                      
C                                                                               
C  Create PGAC # 0 if needed                                                    
C                                                                               
        ipgac  = NLINK('PGAC',0)                                                
        IF ( ipgac .LE. 0 ) THEN                                                
           CALL gapgac                                                          
           CALL BLIST(iw,'S+','PGAC')                                           
        ENDIF                                                                   
C                                                                               
      ENDIF                                                                     
C                                                                               
 100  CALL creflw(irejet)                                                       
C
  999 RETURN                                                                    
      END                                                                       
#endif
      SUBROUTINE QUPEC1                                                         
C----------------------------------------------------------------------         
CKEY FILL EFLOW /USER
C!  Create bank PECO/1 if requested                                             
C                                                                               
C P. Janot -- 9 June 1993                                                       
C----------------------------------------------------------------------         
#ifndef DOC
#include "bcs.h"
C----------------------------------------------------------------------         
      IVERS=0                                                                   
      JALPV=NLINK('ALPV',0)                                                     
      IF (JALPV.EQ.0) GO TO 999                                                 
      IVERS=IW(JALPV+1)                                                         
      IF (IVERS.LT.115) GO TO 999                                               
C Only for ALPHA version 115 and after :                                        
C                                                                               
C  Create PECO # 1 if needed                                                    
C                                                                               
      ipeco1 = NLINK('PECO',1)                                                  
      IF ( ipeco1 .LE. 0 ) THEN                                                 
         CALL GAPECO(IER)                                                       
         CALL BLIST(IW,'S+','PECO')                                             
      ENDIF                                                                     
C                                                                               
  999 RETURN
      END                                                                       
#endif
      SUBROUTINE QMUNEW                                                         
C----------------------------------------------------------------------         
C!  Initialize the muon identification                                          
C----------------------------------------------------------------------         
#ifndef DOC
      CALL MUNEWR                                                               
  999 RETURN             
      END                                                                       
#endif
      SUBROUTINE QUMUPR                                                         
C----------------------------------------------------------------------         
C!  Prepare the muon identification                                             
C----------------------------------------------------------------------         
#ifndef DOC
C...Swap between FRFT #0 an #3 ALWAYS to perform the muon extrap with           
C...FRFT #2 as in JULIA                                                         
      IND=NSWAP('FRFT',0,'FRFT',3)                                              
C                                                                               
      CALL MUREDO(IER)                                                          
C                                                                               
C...Restore the original situation                                              
      IND=NSWAP('FRFT',3,'FRFT',0)                                              
C                                                                               
  999 RETURN
      END
#endif
                                                                       