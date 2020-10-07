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
                                                                       