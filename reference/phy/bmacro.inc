*CD BMACRO        set of intrinsic functions to handle BOS banks                
C - # of words/row in bank with index ID                                        
      LCOLS(ID) = IW(ID+LMHCOL)                                                 
C - # of rows in bank with index ID                                             
      LROWS(ID) = IW(ID+LMHROW)                                                 
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
