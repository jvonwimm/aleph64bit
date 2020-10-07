*CD capano
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
#if defined(DOC)
        LTYPEL : geantino electron particle #
        LTYPME : geantino meson particle #
        LTYPBA : geantino baryon particle #
        LTYPAN : geantino antibaryon particle #
        LTYPGA : geantino photon particle #
        NATGER : Nature de la gerbe (elec=1, phot=2, had=3)
        METHCO : No param. = 1 , param. = 2
        TOTNRJ : Energie totale.
        TINOX0 : total rad length seen by the current geantino
        TINOL0 : total abs. length seen by the current geantino
        EMGNRJ : Fraction e.m.
        EMALFA : Alpha e.m. parameter.
        EMBETA : Beta e.m. parameter.
        EMALM1 : Alpha - 1.
        EMAEXP : Radial e.m. parameter = 1. / ( n - 2 )
        EMFACT : R0 / Z
        EMUDP0 : Depth shower size dependance,constant term.
        EMUDP2 : Depth shower size dependance, z**2  term.
        HADNRJ : Fraction hadronique.
        HALPHA : Alpha parameter of hadronic shower
        HABETA : Beta parameter of hdronic shower
        HADRAY : Radius of hadronic shower
        HAPUIS : Transversal dependance of hadronic shower
        HGAMMA : Free parameter
        SMAXLR : Radiation length tracked so far
        SMAXLA : Absorption length tracked so far
        EMNAME : Last volume name
#endif
