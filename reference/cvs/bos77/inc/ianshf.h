#if defined(APOLLO)
      J= AND(K,15)
      K=RSHFT(K, 4)
#else
      J=IAND(K,15)
      K=ISHFT(K,-4)
#endif
