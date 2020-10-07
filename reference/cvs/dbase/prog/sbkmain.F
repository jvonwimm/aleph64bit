#ifndef UNIX
      PROGRAM SBKMAIN                                                  
C----------------------------------------------------------------------
C! SBANK main Fortran program                                          
C!                                                                     
C!    Author:     H. Meinhard       11-Jan-1993                        
C!    modified:   F.Ranjard         17-Aug-1998
C                 introduce SBKMAIN in CVS repository for non UNIX
C----------------------------------------------------------------------
C Decode the command line options                                      
      CALL SBKDEC                                                      
C Call the SBANK code                                                  
      CALL PRSBANK                                                     
C That's it                                                            
      END                                                              
#endif
