C! ALREAD variable declarations
C - variables set  by *CA STBFORMA
      CHARACTER     BFORMA*7, BFORM2*3
C - variables set  by *CA GTCHUNIT
      CHARACTER     CHUNIT*2, DSNAME*10
C - variables set  by *CA GTVSN
      CHARACTER     CHMED*4, CHTAPE*16, CHOPT*80
C - input arguments
      CHARACTER*(*) ANAME, ATYPE, FDEVI
C - local variables
      CHARACTER     FTYP3*3, MSG*120, SUBR*8
      CHARACTER     CHRECL*5, CHDEF*120
      CHARACTER     FNAME*80
C - external functions
C - internal commom block to the package KERNLIB M432
      COMMON /SLATE/ ND432,NE432,NF432,NG432,DUM432(36)
