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
