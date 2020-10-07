C! to go from upper case to lower case
#if defined(ALEPH_DEC) || defined(ALEPH_LINUX)
      TOLOWER = 32
#else
#if defined(IBM)
      TOLOWER = -1073741824
#else
#if defined(APOLLO) || defined(ALEPH_HP) || defined(RS6K) || defined(ALEPH_SGI)
      TOLOWER = 536870912
#endif
#endif
#endif
