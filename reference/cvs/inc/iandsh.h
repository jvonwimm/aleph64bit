*CD iandsh
#if defined(APOLLO)
         I1= AND(RSHFT(IIW, 16),65535)
         I2= AND(      IIW     ,65535)
#else
         I1=IAND(ISHFT(IIW,-16),65535)
         I2=IAND(      IIW     ,65535)
#endif
