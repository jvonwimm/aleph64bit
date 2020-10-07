      COMMON/ITTIME/TPREDA,TTRECI,TTCTPC,TTRECF,TTCTRK,TMATCH,TTRMCA,
     +TNSECT,TSRCH,TTLNK1,TTREE1,TFIND1,TFITP1,
     +TTLNK2,TTREE2,TFIND2,TFITP2
     + ,IFITTI
      LOGICAL IFITTI
#if defined(DOC)
C!
C! CPU time per event in ITC modules
C!
C! TPREDA = Average CPU time per event used by IPREDA
C! TTRECI = Average CPU time per event used by ITRECI
C! TTCTPC = Average CPU time per event used by ITCTPC
C! TTRECF = Average CPU time per event used by ITRECF
C! TTCTRK = Average CPU time per event used by ITCTRK
C! TMATCH = Average CPU time per event used by IMATCH
C! TTRMCA = Average CPU time per event used by ITRMCA
C! TNSECT = Average CPU time per event used by INSECT
C! TSRCH  = Average CPU time per event used by ISRCH
C! TTLNK1 = Average CPU time per event used by ITLNK1
C! TTREE1 = Average CPU time per event used by ITREE
C!            (when called from subroutine IGICHA)
C! TFIND1 = Average CPU time per event used by IFIND1
C! TFITP1 = Average CPU time per event used by IFITP
C!            (when called from subroutine IGICHA)
C! TTLNK2 = Average CPU time per event used by ITLNK2
C! TTREE2 = Average CPU time per event used by ITREE
C!            (when called from subroutine ITRAK)
C! TFIND2 = Average CPU time per event used by IFIND2
C! TFITP2 = Average CPU time per event used by IFITP
C!            (when called from subroutine IFIND2)
#endif
