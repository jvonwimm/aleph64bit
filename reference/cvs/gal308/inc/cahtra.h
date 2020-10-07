*CD  cahtra
      COMMON /CAHTRA/ HCTRKM (27,2 ),HCAXL0,HTIN01,HTIX01,HSHGRA,HSHGAB
#if defined(DOC)
         HCTRKM (27,2) : entrance and outgoing track parameter in a
                         volume
         HCAXL0        : track length in volume
         HTIN01        : absorption length crossed before volume entrance
         HTIX01        : radiation length crossed before volume entrance
         HSHGRA        : average radiation length in volume
         HSHGAB        : average absorption length in volume
#if defined(DOC)
#endif
#endif
