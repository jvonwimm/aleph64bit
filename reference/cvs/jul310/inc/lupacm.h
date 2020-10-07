C!          LUPA common block
      PARAMETER (NM=4, NR=30, NC=16)
      INTEGER ISCALU,ISTOLU
      PARAMETER (NMET=32)
      REAL ACPTLU
      COMMON /LUPACM/ISCALU(NM,NR,NC),ISTOLU(NM,NR,NC),ACPTLU(NMET)
#if defined(DOC)
C ISCALUijk : LCAL storey map
C ISTOLUijk : LCAL storey map
C ACPTLUi   : array containing number of previously accepted events
#endif
