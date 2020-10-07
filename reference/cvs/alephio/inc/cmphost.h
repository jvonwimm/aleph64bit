C! define the HOST machine
#if defined(ALEPH_DEC) || defined(ALEPH_LINUX)
      HOST = 'VAX'
#else
#if defined(IBM)
      HOST = 'IBM'
#else
#if defined(APOLLO) || defined(ALEPH_HP) || defined(RS6K) || defined(ALEPH_SGI)
      HOST = 'APOL'
#endif
#endif
#endif
