      INTEGER MAXIND
      PARAMETER (MAXIND = 12*4+15*4)
      INTEGER IINDEX(2,4,15),IINDXY(MAXIND),NWAFER(MAXIND)
      INTEGER ILAYND(MAXIND),IPHIND(MAXIND),IWAFND(MAXIND)
      COMMON/VINDEX/IINDEX,IINDXY,NWAFER,ILAYND,IPHIND,IWAFND
#if defined(DOC)
C
C!  Common block for standard wafer indexes
C
C
C  IINDEX = Unique consecutive integer for each physical wafer
C  NWAFER = Bank number (vaenwa output) given IINDEX
C  IINDXY = Bank number for r-phi side, compresses wafers 1+2,3+4
C  ILAYND = Layer of this IINDEX
C  IPHIND = phi   of this IINDEX
C  IWAFND = wafer of this IINDEX
C
#endif
