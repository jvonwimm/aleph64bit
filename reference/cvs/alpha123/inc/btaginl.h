C
C  Inline functions for QIPBTAG routines.  As these include the
C  variable declarations, they should be the first inline comdeck
C  included
C
      INTEGER IPTT,IVEW,ITYP,IFLG
      INTEGER TTYPE,TVIEW,TVDPAT,TPACK
      LOGICAL TWOHIT,INNERHIT,ONEHIT,TWORPHIT,TWOZHIT,RPHIT,ZHIT
      LOGICAL THREED,RPHI,RZ
C
C  These select tracks based on vdet hit pattern
C
      ONEHIT(IPTT) = (IAND(IPTT,3).GT.0.AND.
     &                IAND(ISHFT(IPTT,-4),3).GT.0).OR.
     &               (IAND(ISHFT(IPTT,-2),3).GT.0.AND.
     &                IAND(ISHFT(IPTT,-6),3).GT.0)
      INNERHIT(IPTT) = (IAND(IPTT,3).GT.0.AND.
     &                IAND(ISHFT(IPTT,-4),3).GT.0)
      TWOHIT(IPTT) = IAND(IPTT,3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-2),3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-4),3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-6),3).GT.0
      RPHIT(IPTT) = IAND(IPTT,3).GT.0 .OR.
     &               IAND(ISHFT(IPTT,-2),3).GT.0
      ZHIT(IPTT) = IAND(ISHFT(IPTT,-4),3).GT.0 .OR.
     &               IAND(ISHFT(IPTT,-6),3).GT.0
      TWORPHIT(IPTT) = IAND(IPTT,3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-2),3).GT.0
      TWOZHIT(IPTT) = IAND(ISHFT(IPTT,-4),3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-6),3).GT.0
C
C  Split and pack the track flag fields
C
      TTYPE(IFLG) = IAND(IFLG,15)
      TVIEW(IFLG) = IAND(ISHFT(IFLG,-4),3)
      TVDPAT(IFLG) = IAND(ISHFT(IFLG,-6),255)
      TPACK(ITYP,IVEW,IPTT) = ITYP+ISHFT(IVEW,4)+ISHFT(IPTT,6)
