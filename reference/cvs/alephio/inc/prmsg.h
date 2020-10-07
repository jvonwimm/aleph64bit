C! print MSG
#if defined(ARDEB)
      WRITE (IW(6),*) ' PRMSG: ',SUBR,' LUN = ',LUN,
     &                ' MSG= ',MSG(1:LNBLNK(MSG))
#endif
