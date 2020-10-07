*CD kincom
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
C
#if defined(DOC)

                    KINEmatic definition
      IPROKI            process identification
      ECMSKI            beam energy in the center of mass
      IDEVKI            event process identification
      ISTAKI            status word
      WEITKI            event weight
      NOTRKI            NOtracking marker word
      NITRKI            # of input tracks
      NIVXKI            # of input vertices
#endif
