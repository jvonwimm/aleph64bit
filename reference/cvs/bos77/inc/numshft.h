#if defined(APOLLO)
            NUMB=IOR(LSHFT(NUMB,4),N)
#else
            NUMB=IOR(ISHFT(NUMB,4),N)
#endif
