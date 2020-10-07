      PARAMETER (ITRYIT=3,ITLYIT=8)
      COMMON/ITRKER/CTR0IT(ITRYIT),SIGNUM(ITRYIT),
     +              CCRVIT(ITLYIT,ITLYIT,ITLYIT)
#if defined(DOC)
C! Cuts (for links and branches) used in ITC tracking
C
C ITRYIT         : Max. no. of track search types
C ITLYIT         : Max. no. of ITC layers
C
C CTR0IT(i)      : Cut on 2*r0 for search type i
C SIGNUM(i)      : No. of standard deviations error allowed, when
C                   comparing link curvatures
C CCRVIT(l,m,n)  : Error on the difference in curvatures between links
C                   running from layers l to m and m to n respectively.
C
#endif
