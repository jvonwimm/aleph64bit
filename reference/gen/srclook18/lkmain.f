C*HE 06/12/95 17:30:26 C,D
C*DK LKMAIN
C*DF UNIX
      PROGRAM LKMAIN
C ----------------------------------------------------------
C! program to look at banks
C     Author : A.McSporran         VAX version
C     Author : G. Bagliesi 890307  IBM version
C
C     Modified by D.Harvatis          11-AUG-89
C                 G.Bagliesi          21-SET-89
C ----------------------------------------------------------
C*IF .NOT.DOC
      PARAMETER (LBNAM=3000, LBCS=500000)
      COMMON /BCS/ IW(LBCS)
C*CA LKEVT
      COMMON/LKEVT/IRUNTR,IEVTR,IEVE,IVOULU,IRUNV
C*CC LKEVT
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
C*CA LKLAST
      COMMON/LKLAST/LBNKNM,LBNKNR,LROWNM,LPRNUM
      CHARACTER*4 LBNKNM
      INTEGER LBNKNR,LROWNM,LPRNUM
C*CC LKLAST
C*CA VERSION
C! date and version number
      PARAMETER (IVERS=19, IDATE=950808)
C*CC VERSION
C ---------------------------------------------------------
C*IF .NOT.DOC
C     initialize time on vax, do-nothing routine in batch and on
C     other machines
      CALL TIMEST (1000000.)
C     terminal output unit
      LOUT = 6
      CALL LKSTOUT (6)
      IW(6) = 0
      CALL ALVERS (ALVER)
      WRITE(LOUT,8000)IVERS ,IDATE, ALVER
 8000 FORMAT (/5X,'LOOK program version ',I2, ' released on ',I6
     &        ,' linked with ALEPHLIB ',F4.1)
C
      CALL BNAMES(LBNAM)
      CALL BOS(IW,LBCS)
C
C     BOS output unit
      IW(6) = LOUT
C     number of messages to be printed
      IW(7)=10000
C     number of banks to be printed
      IW(8)=10000
C     terminal logical unit
      LTERM = 5
C     data card logical unit
      LCARD = 7
      IW(5) = LCARD
C     output data card file (CARD 72 col.)
      LFILE(1) = 11
C     print file unit
      LFILE(2) = 12
C     output file (EPIO or NATIVE)
      LFILE(3) = 13
C     BOS format unit
      LUFMT = 8
C     data base unit
      LDBAS = JUNIDB(0)
C*IF IBM
C*EI
C
      CALL LKINIT
      WRITE(LOUT,*) '[Type "HELP" for command list]'
      LBNKNM=' '
      IVOULU=1
      CALL LKLOOK
      END
