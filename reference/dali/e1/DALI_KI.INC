C*HE 01/22/88 14:03:04 S        
C
C_CG  Updated 11-May-89 C.Grab
C_CG     Name changed to DALI_KI for IBM compatibility
C
*CD KMACRO  
C - index of the next vertex/track to be stored in KINE/VERT
C   bank known by its index JVK 
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3) 
C - # of vertices/tracks which could be stored in KINE/VERT 
C   bank known by its index JVK 
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))   
C - index of the 1st parameter of KINE/VERT bank known by its   
C   index JVK   
      KPARVK(JVK) = JVK + IW(JVK+1) 
C - index of 1st vertex/track # contained into the list of  
C   bank KINE/VERT known by its index JVK   
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2) 
C - charge of ALEPH particle# JPA   
      CHARGE(JPA) = RTABL(IW(NAPAR),JPA,7)  
C - mass of ALEPH particle# JPA 
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)  
C - time of life of ALEPH particle# JPA 
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)  
C - # of vertices on a track known by its BOS index JVK /   
C   # of outgoing tracks of a vertex known by its BOS index JVK 
      NOFVK(JVK)  = IW(JVK+3)   
C - Particle type of a track known by its BOS index JVK 
      KINTYP(JVK) = IW(KPARVK(JVK)+5)   
C - incoming track # of a vertex known by its BOS index JVK 
      INPTRK(JVK) = IW(KPARVK(JVK)+5)   
C - origin vertex # of a track known by its BOS index JVK   
      INPVRT(JVK) = IW(KLISVK(JVK)+1)   
C - momentum of a track known by its BOS index JVK  
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2   
     &                   +RW(KPARVK(JVK)+3)**2) 
C - energy of a track known by its BOS index JVK
      ENERVK(JVK) = RW(KPARVK(JVK)+4)   
C - time of flight of the icoming particle to the vertex known by its   
C   BOS index JVK   
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)   
C   
