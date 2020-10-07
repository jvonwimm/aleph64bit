      COMMON /VTKREC/  NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL
     +                ,NARCVD
      INTEGER NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL,NARCVD
#if defined(DOC)
C! internal to VDTRACK package
        NLAYER      number of layer for the current track
        NULINK      number of linked clusters in U direction
        NWLINK      number of linked clusters in U direction
        IGRBMX      maximum value for IGARB
        IERVTR      return flag for VTRFIT
                    = 0  OK
                      4  not enough space for work bank  (VTRFIT)
                      5  not enough space for named bank (VTRFIT)
                      6  missing some named banks        (VTRFIT)
                     11  not enough space for named bank (VTFILL)
                     12                                  (VTFILL)
                     14                                  (VTFILL)
#endif
