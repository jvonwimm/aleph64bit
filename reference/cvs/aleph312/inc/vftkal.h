C! VDET UFTKAL geometry constants
      INTEGER KVDL, KFACE, KBPIP
      PARAMETER (KVDL=2, KFACE=15, KBPIP=2)
      INTEGER KDIVPV,KBPSTP
      REAL VKRF,VKUWID,VKZWID,VKPHIF,VKPHIN,UKRVAC,UKSVAC,UKSVD
      REAL UKRITC,UKSITC,UKRTPC,UKSTPC,UKSPITC,UKSPTPC
      REAL UKZICA, UKRIICA, UKROICA, UKSICA
      REAL UKZOCA, UKRIOCA, UKROOCA, UKSOCA
      COMMON /VFTKAL/ KDIVPV(KVDL),VKRF(KFACE,KVDL),VKUWID(KFACE,KVDL),
     &                VKZWID(KFACE,KVDL),VKPHIF(KFACE,KVDL),
     &                VKPHIN(KFACE,KVDL),KBPSTP,
     &                UKRVAC(KBPIP),UKSVAC(KBPIP),UKSVD(KBPIP),
     &                UKRITC,UKSITC,UKRTPC,UKSTPC,UKSPITC,UKSPTPC,
     &                UKZICA(KBPIP),UKZOCA(KBPIP),UKRIICA(KBPIP),
     &                UKRIOCA(KBPIP),UKROICA(KBPIP),UKROOCA(KBPIP),
     &                UKSICA(KBPIP),UKSOCA(KBPIP)
#if defined(DOC)
      KVDL       : number of VDET layers L=1,KVDL
      KFACE      : maximum number of faces I=1,KFACE
      KBPIP      : number of different beam pipe or VDET K=1,KBPIP
      KDIVPV(L)  : number of faces in layer L
      VKRF(I,L)  : radius of the center of the wafer
      VKUWID(I,L): r-phi dimension of a face
      VKZWID(I,L): zed dimension of a face
      VKPHIF(I,L): azimuthal angle of the wafer center
      VKPHIN(I,L): azimutal angle of the perpendicular
      KBPSTP     : beam pipe setup code
      UKRVAC(K)  : mean beam pipe radius
      UKSVAC(K)  : beam pipe total rad length
      UKSVD(K)   : total rad length per wafer
      UKRITC     : inner radius ITC
      UKSITC     : total rad length inner ITC wall
      UKRTPC     : a radius between ITC and TPC walls
      UKSTPC     : total rad length in ITC/TPC wall
      UKSPITC    : rad length per cm in ITC gas
      UKSPTPC    : rad length per cm in TPC gas
      UKZICA(K)  : abs(Z) of disc for inner VDET endcap
      UKZOCA(K)  : abs(Z) of disc for outer VDET endcap
      UKRIICA(K) : inner radius of inner VDET endcap disc
      UKRIOCA(K) : inner radius of outer VDET endcap disc
      UKROICA(K) : outer radius of inner VDET endcap disc
      UKROOCA(K) : outer radius of outer VDET endcap disc
      UKSICA(K)  : rad length of inner VDET endcap disc
      UKSOCA(K)  : rad length of outer VDET endcap disc
#endif
