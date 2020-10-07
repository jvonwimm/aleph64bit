      PARAMETER (LNTCNT=10,LTSTAT=14)
      PARAMETER (LCPRED=1,LCPADS=2,LCWIRE=3)
      PARAMETER (LCMAGN=4,LCTPCO=5,LCTRKF=6,LCTRFT=7,LCTRWR=8)
      PARAMETER (LCELOS=9,LCPDEX=10)
      PARAMETER (LPULTS=1,LCLUTS=2,LTSPTS=3,LTSCTS=4,LTWPTS=5)
      PARAMETER (LTPCTS=6,LTCHTS=7,LTGFTS=8,LTWTTS=9,LTELTS=10)
      PARAMETER (LTPCOV=11,LTPCTW=12,LTPCTL=13,LTPLTS=14)
      COMMON /TPCSTA/ ICNTER(LNTCNT),ITSTAT(LTSTAT)
#if defined(DOC)
C! Event and statistics counters for TPC reconstruction
C
C   ICNTER =  event counter for TPC reconstruction
C   LNTCNT =  length of event counter array
C   ITSTAT =  counters for keeping TPC statistics
C   LTSTAT =  number of TPC items for which statistics are accumulated
C
C   LCPRED =  index in ICNTER for prepare data
C   LCPADS =  index in ICNTER for analyze pad data
C   LCWIRE =  index in ICNTER for analyze wire data
C   LCMAGN =  index in ICNTER for HALL probe
C   LCTPCO =  index in ICNTER for calculate coordinates
C   LCTRKF =  index in ICNTER for find tracks
C   LCTRFT =  index in ICNTER for fit tracks
C   LCTRWR =  index in ICNTER for track-wire association
C   LCELOS =  index in ICNTER for calculate track wire energy loss
C   LCPDEX =  index in ICNTER for calculate track pad energy loss
C
C   LPULTS =  index in ITSTAT for number of pulses
C   LCLUTS =  index in ITSTAT for number of clusters
C   LTSCTS =  index in ITSTAT for number of subclusters
C   LTSPTS =  index in ITSTAT for number of subpulses
C   LTWPTS =  index in ITSTAT for number of wire pulses
C   LTPCTS =  index in ITSTAT for number of coordinates
C   LTCHTS =  index in ITSTAT for number of chains
C   LTGFTS =  index in ITSTAT for number of fitted tracks
C   LTWTTS =  index in ITSTAT for number of wires pulses on tracks
C   LTELTS =  index in ITSTAT for number of tracks with wire dE/dX calculated
C   LTPCOV =  index in ITSTAT for number of coords removed from TCAL
C   LTPCTW =  index in ITSTAT for number of twin coordinates used
C   LTPCTL =  index in ITSTAT for number of linked TGFT tracks
C   LTPLTS =  index in ITSTAT for number of tracks with pad dE/dx calculated
C
C-----------------------------------------------------------------------
#endif
