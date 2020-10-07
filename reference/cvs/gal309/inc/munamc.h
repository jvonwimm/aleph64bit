*CD munamc
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
C
#if defined(DOC)
        NAxxxx - name-index of bank 'xxxx'
        NHTIN -- the initial length of 'MUHT' in # of signals
        INCHT -- the increment as 'MUHT' is enlarged
                 ( in # of signals )
        NAVTD -- average # of tracks per digit
#endif
