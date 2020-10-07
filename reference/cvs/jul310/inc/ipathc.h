      PARAMETER (MXNLIP=8,MXPTIP=25)
      COMMON/IPATHC/NPTHIP,LPTHIP,LLNKIP(MXNLIP,MXPTIP),
     +IWORST(MXNLIP,MXPTIP),IESAVE
#if defined(DOC)
C! Path common used in ITC tracking
C
C MXNLIP      : Max. no. of links on path
C MXPTIP      : Max. no. of paths (in tree)
C
C NPTHIP      : No. paths
C LPTHIP      : Length of paths
C LLNKIP(j,i) : List of links on path i,  j=1,LPTHIP
C IWORST(j,i) : Probable j'th worst coordinate on path i
C IESAVE      : Error flag : set to 1 by subroutine ISAVE, if not all
C               links could be stored.
#endif
