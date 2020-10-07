*CD sistat
      PARAMETER ( NSIST = 15)
      DOUBLE PRECISION ESICOU
      COMMON/SISTAT/NSIPRT,NSICOU(NSIST),ESICOU(NSIST)
#if defined(DOC)
C!      Statistics related to SICAL detector
      NSIPRT    : print flag
      NSICOU(1) : Number of events entering SICAL
      NSICOU(2) : Number of events entering SICAL END A
      NSICOU(3) : Number of events entering SICAL END B
      NSICOU(4) : Number of pad hit per event     END A + B
      NSICOU(5) :
      ESICOU(1) : Energy per event entering SICAL        GEANT trackel
      ESICOU(2) : Energy per event entering SICAL END A  GEANT trackel
      ESICOU(3) : Energy per event entering SICAL END B  GEANT trackel
      ESICOU(4) : Energy per event entering SICAL        parametrized
      ESICOU(5) : Energy per event entering SICAL END A  parametrized
      ESICOU(6) : Energy per event entering SICAL END B  parametrized
      ESICOU(7) : Energy per event lost from SICAL when  parametrized
      ESICOU(8) : Energy per event lost from SICAL END A parametrized
      ESICOU(9) : Energy per event lost from SICAL END B parametrized
      ESICOU(10): Energy squared per event entering SICAL
#endif
