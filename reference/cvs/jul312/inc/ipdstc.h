      PARAMETER (NSTAIP=11,LSTAIP=8,MSTAIP=3)
      COMMON/IPDSTC/ISTAIP(NSTAIP),ILSTIP(LSTAIP,MSTAIP)
#if defined(DOC)
C! ITC Prepare Data Statistics
C
C NSTAIP         : Array dimension.
C LSTAIP         : Array dimension (no. of ITC layers)
C MSTAIP         : Array dimension
C
C ISTAIP(i)      : Statistics array:
C                : 1 = # events processed through IPREDA
C                : 2 = # events with ITDI (digitisings) bank
C                : 3 = # events with empty ITDI bank.
C
C                : 4 = # events with non empty ITDI bank
C                : 5 = Sum of no. of digis. per event.
C                : 6 = Max. no. of digits. in any one event
C                : 7 = Min. no. of digits. in any one event
C
C                : 8 = # of evnts with ITCO bank
C                : 9 = Sum of no. of coords per event
C                : 10 = Max. no. of coords in any one event
C                : 11 = Min. no. of coords in any one event
C
C ILSTIP(j,i)    : Statistics array (per layer - j)
C                : 1 = Sum of no. of digis. in layer j
C                : 2 = Max. no. of digis in layer j
C                : 3 = Min. no. of digis in layer j
C
#endif
