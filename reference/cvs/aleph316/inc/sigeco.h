      COMMON/SIGECO/NMODSI,NRBNSI,NPBNSI,NZBNSI,RMINSI(2),RMAXSI(2),
     $              Z0SNSI(2),ZWIDSI,ZWRFSI,ZWRLSI,ZWFRSI,ZWFLSI,
     $              ZWBKSI,ZWLASI,OVLPSI,DPOSSI(3,2),GAPXSI(2),
     $              PHSHFT(3,2),RADSTP,PHISTP,ISINUM(12,2)
#if defined(DOC)
C    NMODSI  Number of modules
C    NRBNSI  Number of radial bins per module
C    NPBNSI  Number of Phi bins per module
C    NZBNSI  Number of z layers per module
C    RMINSI  Minimum radius of sensitive area(Mod number)
C    RMAXSI  Maximum radius of sensitive area(Mod number)
C    Z0SNSI  Z position of first sensitive layer (Mod number)
C    ZWIDSI  Z width of a sensitive layer
C    ZWFRSI  Z width in front of fisrt module
C    ZWFLSI  Z width in front of first layer
C    ZWLASI  Z width of last modul (#12)
C    ZWBKSI  Z width of back material
C    OVLPSI  Oerlap between Si crystals material
C    ZWRFSI  Number of radiation length before first layer
C    ZWRLSI  Number of radiation length per layer
C    PHSHFT  PHI shifts of subsequent z planes in a triplet (Mod number)
C    RADSTP  Radial width of a pad
C    PHISTP  Phi width of a pad
C    DPOSSI(3,2)    Offset of each SICAL module for x,y,z ( for z , it applies
C                   to the absolute Z value )
C    GAPXSI(2)      Half opening of the two half modules on each side along x
C    ISINUM(12,2)   Numbering of the 12 plates used in the stack on each side
#endif
