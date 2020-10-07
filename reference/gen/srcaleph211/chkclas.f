      LOGICAL FUNCTION CHKCLAS (KCLASW)
C----------------------------------------------------------------------
C - F.Ranjard - 930309          from M.Talby
CKEY EDIR CLASS REVH /USER
C!  Get the class word from REVH bank or SELEVT subroutine.
C-  and check if it contains a selected class.
C   Called by   : USER
C   Calls  : ALPHARD and EDIR packages
C - Output - KCLASW  / I = class word
C          - CHKCLAS / L = .FALSE. if the class word does not
C                          contains one of the selected bits
C-
C----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JREVDS=1,JREVFE=2,JREVNE=4,JREVSB=6,JREVTI=7,JREVRB=8,
     +          JREVEC=10,LREVHA=10)
C --
      LOGICAL BSELEC
      DATA NREVH /0/
C --
C ----------------------------------------------------------------------
C --
      IF (NREVH.EQ.0) THEN
        NREVH = NAMIND('REVH')
      ENDIF
C
      KREVH = IW (NREVH)
      KCLASW = 0
C
      IF(KREVH.GT.0 .AND. IW(KREVH+LMHCOL).GE.JREVEC) THEN
C      get write class word from REVH bank
         KCLASW = IW(KREVH+LMHLEN+JREVEC)
      ELSE
C      build the EDIR  class word
         CALL ALCLASW (KCLASW)
      ENDIF
C
C -- check class word
C
      CALL ABGTRCL (MASKR)
      CHKCLAS = BSELEC (KCLASW,MASKR)
C
C -- set write class word
C
      CALL ABSTCL (KCLASW)
C
      END
