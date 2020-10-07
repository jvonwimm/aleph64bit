C!     set of functions to decode Lcal tower addresses
      LCMOD(IXX) = (IXX-1)/512 + 1
      LCROW(IXX) = MOD(IXX-1,512)/16 + 1
      LCCOL(IXX) = MOD(IXX-1,16) + 1
#if defined(DOC)
C LCMOD  : Module number
C LCROW  : Row number
C LCCOL  : Column number
C
#endif
