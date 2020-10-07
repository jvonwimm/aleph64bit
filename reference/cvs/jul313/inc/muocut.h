C! Parameters for Muon reconstruction
      COMMON/MUOCUT/PCUTMA,PCUTHA,ACCLAY(2),ACCANG,
     + ROADWD,TETASS,NEXNFO,NLAS10,NMINFP,WCLUMX
C
#if defined(DOC)
C
C    PCUTMA    = cutoff momentum for track association to mu-ch
C    PCUTHA    = cutoff momentum for track association to Hcal
C    ACCLAY(1) = cutoff on the ratio (obs dist)/(expect dist) for Int la
C    ACCLAY(2) = cutoff on the ratio (obs dist)/(expect dist) for Ext la
C    ACCANG    = cutoff on the ratio (obs exit angl)e/(expected exit ang
C    ROADWD    = road width in Hcal (in sigma mult scatt units)
C    NEXNFO    = cutoff on (nb of expected planes - nb of fired planes)
C    TETASS    = minimum angle to associate a muon track to an Hcal towe
C    NLAS10    = cutoff on the number of fired planes among last 10
C    NMINFP    = cutoff on the minimum number of fired planes
C    WCLUMX    = max width of a tubes cluster
#endif
