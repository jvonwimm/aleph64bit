*CD mug1pr
C! The parameters needed by the geometry routine AGMUCH
      PARAMETER (NMBIN = 12, NMBOU = 12 )
      PARAMETER (NMMIN = 10, NMMOU =  9, NMMA = NMMIN+NMMOU, IMMBT =  9)
      PARAMETER (NMCIN = 4, NMCOU = 4, NMCA = NMCIN+NMCOU)
      PARAMETER (NMMBI = NMMA+NMMIN, NMCBI = NMCA+NMCIN)
      COMMON /MUG1PR/   MMADPR(12,4)
C
#if defined(DOC)
        NMBIN -- # of modules in the inner layer of MU barrel
        NMBOU -- # of modules in the outer layer of MU barrel
        NMMIN -- # of modules in the inner layer of MU middle angle
        NMMOU -- # of modules in the outer layer of MU middle angle
        IMMBT -- order number of the bottom module in the inner layer
                 of MU middle angle
        NMCIN -- # of modules in the inner layer of MU endcap
        NMCOU -- # of modules in the outer layer of MU ebdcap
#endif
