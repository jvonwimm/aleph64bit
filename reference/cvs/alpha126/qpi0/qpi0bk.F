      SUBROUTINE QPI0BK
CKEY QPI0DO / USER
C-----------------------------------------------------------------------
C!  BOOK Some control histograms filled by qpi0do
C    Author   :  J-P Lees              15-Oct-1992
C-----------------------------------------------------------------------
#ifndef DOC
      COMMON/HIQPI0/IHIPI0
      IHIPI0=1
      CALL HBOOK2(9001,'MASS [<GG>] VS E?[<GG>] <ALL>',
     +            30,0.,30.,100,0.,1.,0.)
      CALL HBOOK2(9002,'MASS [<GG>] VS E?[<GG>] <TYPE> 1',
     +            30,0.,30.,100,0.,1.,0.)
      CALL HBOOK2(9003,'MASS [<GG>] VS E?[<GG>] <TYPE> 2',
     +            30,0.,30.,100,0.,1.,0.)
      CALL HBOOK2(9004,'MASS [<GG>] VS E?[<GG>] <TYPE> 1',
     +            30,0.,30.,100,0.,1.,0.)
      CALL HBOOK2(9005,'MASS [<GG>] VS E?[<GG>] <TYPE> 2',
     +            30,0.,30.,100,0.,1.,0.)

      RETURN
      END
#endif
