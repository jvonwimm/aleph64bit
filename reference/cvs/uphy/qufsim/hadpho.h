      PARAMETER (maxclu=200)
      COMMON / hadpho / vph(4,maxclu), vhn(4,maxclu), vclu(4,maxclu),
     .                  echa(maxclu), iclu(maxclu),
     .                  nclu, nph, nhn, nch
      COMMON / extrap / execal(maxclu,3), exhcal(maxclu,3),
     .                  iandx(maxclu)
      DIMENSION mobj(maxclu),moclu(maxclu,maxclu),nmoclu(maxclu),
     &          fclu(maxclu,maxclu),
     &          mobjp(maxclu),moclup(maxclu,maxclu),nmoclup(maxclu),
     &          fclup(maxclu,maxclu)
      common/mcmoth/mobj,moclu,nmoclu,fclu
      common/mcmothp/mobjp,moclup,nmoclup,fclup
C
