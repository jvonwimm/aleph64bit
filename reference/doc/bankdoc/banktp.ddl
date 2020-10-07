 SUBSCHEMA TpcONLBanks
 : 'Description of TPC online banks'
 
 AUTHOR   'J. Hilgart, A. Lusiani'
 REVIEWER 'W. Wiedenmann, G.Ganis'
 VERSION  '4.1'
 DATE     '10/06/94'
 
 DEFINE ESET
 
 
 TERR:   'TPC online error information. (ONL)
          Obsolete as of 1992 run. Replaced by TE1R.
          The data words are present only in case an error has been detected.
          Otherwise only the mini-header is present.  Warning, on much of our
          data the miniheader is not correct and should not be used.\
          Number of columns\
          Number of rows'
           STATIC
       = (TppNumber     = INTE [1,72]       :'Fastbus address of TPP causing the error',
          ReadoutMask   = INTE [0,*]        :'mask of errors on the readout level',
          BankMask      = INTE [0,*]        :'mask for BOS bank format errors',
          TruncLen      = INTE [0,*]        :'amount of data truncated, if truncation occurred',
          TpdMask       = INTE [0,*]        :'mask of TPDs the TPP should read',
          TpdError      = INTE [0,*]        :'mask of TPDs that gave errors');
 
 
 TE1R:   'TPC online error information. (ONL)
          Valid as of 1992 run.
          Obsolete as of 1994 run. Replaced by TOER.
          NR= Event Builder FASTBUS primary address.
          The data words are present only in case an error has been detected.
          Otherwise only the mini-header is present.\
          Number of columns\
          Number of rows'
            STATIC
       = (TppNumber     = INTE [1,72]       :'sector number (+36 for wire TPPs)',
          ReadoutMask   = INTE [0,*]        :'mask of errors on the readout level',
          BankMask      = INTE [0,*]        :'mask for BOS bank format errors');
 
 TOER:   'Tpc Online ERror information. (ONL)
          Valid as of 1994 run. 
          NR= Sector number.
          Present only in case an error has been detected.\
          Number of columns\
          Number of rows'
                STATIC
       = (ReadOutmask     = INTE [0,*]      :'mask of errors on the readout level',
          TruncatedMask   = INTE [0,*]      :"mask of truncated TPD's",
          NumberTruncated = INTE [0,*]      :'number of truncated bytes',
          InvalidHit      = INTE [0,*]      :'mask of TPDs with invalid hits',
          FastBuserrors   = INTE [0,*]      :'mask of TPDs with fatal FB errors',
          InvalidPointers = INTE [0,*]      :'mask of TPDs with invalid pointers');

 
 TINF:   'TPC online readout information. (ONL)
          Obsolete as of 1992 run, replaced by TANF/TWNF, TARE/TWRE.
          NR=(1:36) for pads.
          NR=(37:72) for wires.'
           STATIC
       = (EventCount     = INTE [1,*]       :'internal event # counted by the TPPDAQ',
          EventLength    = INTE [0,*]       :'event length in bytes',
          ReadBank       = INTE [0,*]       :'TPD Read bank ( current bank read-out )',
          WriteBank      = INTE [0,*]       :'TPD Write bank ( to be digitized next )',
          StartAddress   = INTE [0,*]       :'CSR_10 content : event buffer start address',
          PadWire        = INTE [0,*]       :'pad/wires flag .... not very useful indeed ... say : reserved',
          ReadTime       = INTE [0,*]       :'read-out time for this event',
          ReadMask       = INTE [0,*]       :'TPD read-out mask',
          NohitMask      = INTE [0,*]       :'mask of TPDs which had no hits',
          FailMask       = INTE [0,*]       :'mask of TPDs where read-out failed',
          OriginalLength = INTE [0,*]       :'original length of truncated event ( = 0 if not truncated )');
 
 
 TANF:   'TPC Additional iNFo, produced by the pad TPP. (ONL)
          Introduced for the 1992 run.
          NR= TPC sector number (1:36).'
           STATIC
       = (FsmMask        = INTE             :'mask to flag relevant FSM info',
          FsmStatus      = INTE             :'TPP finite state machine status',
          TpdMask        = INTE             :'mask of TPDs in the read-out',
          TpdNoHit       = INTE             :'mask of TPDs with no hit',
          TpdOvflow      = INTE             :'mask of TPDs not read due to lack of space',
          Reserved       = INTE             :'reserved',
          TpdInvalid     = INTE             :'mask of TPDs producing invalid data',
          TpdFailed      = INTE             :'mask of TPDs which failed to be read-out',
          TruncWc        = INTE             :'number of truncated BOS words',
          CpuTime        = INTE             :'TPP processing time in microseconds');
 
 
 TWNF:   'TPC Wire additional iNFo, produced by the wire TPP if used. (ONL)
          Introduced for the 1992 run.
          NR= TPC sector number (1:36).'
           STATIC
       = (FsmMask        = INTE             :'mask to flag relevant FSM info',
          FsmStatus      = INTE             :'TPP finite state machine status',
          TpdMask        = INTE             :'mask of TPDs in the read-out',
          TpdNoHit       = INTE             :'mask of TPDs with no hit',
          TpdOvflow      = INTE             :'mask of TPDs not read due to lack of space',
          Reserved       = INTE             :'reserved',
          TpdInvalid     = INTE             :'mask of TPDs producing invalid data',
          TpdFailed      = INTE             :'mask of TPDs which failed to be read-out',
          TruncWc        = INTE             :'number of truncated BOS words',
          CpuTime        = INTE             :'TPP processing time in microseconds');
 
 
 TARE:   'TPC Additional info REduced, produced by the pad TPP. (ONL)
          Introduced for the 1992 run.
          NR= TPC sector number (1:36).'
           STATIC
       = (FsmMask        = INTE             :'mask to flag relevant FSM info',
          FsmStatus      = INTE             :'TPP finite state machine status');
 
 
 TWRE:   'TPC Wire additional info REduced, by the wire TPP if used. (ONL)
          Introduced for the 1992 run.
          NR= TPC sector number (1:36).'
           STATIC
       = (FsmMask        = INTE             :'mask to flag relevant FSM info',
          FsmStatus      = INTE             :'TPP finite state machine status');
 
 
 END ESET
 
 END SUBSCHEMA
 
 
 SUBSCHEMA TpcRAWBanks
 : 'Description of TPC raw data banks'
 
 AUTHOR   'A. Lusiani'
 REVIEWER 'W. Wiedenmann'
 VERSION  '5.1'
 DATE     '10/05/94'
 
 DEFINE ESET
 
 TSLE :      'Tpc Smart reduced wire pulse LEngth (NR = sector number).
              Replaces TRLE as of 1992 run.
              This bank is parallel to the TSIR bank, having
              one entry (NOT one word!) for each wire pulse. (RAW)'
           STATIC
      = (PulseLength  = INTE   : 'Packed word of 8 entries, each
                                    entry corresponding to one hit.
                                    For non-reduced hits (see the flag
                                    in each word of TSIR), the content
                                    of this nibble (4 bits) is not defined.
                                    For reduced hits it contains the number
                                    of samples used in the calculation
                                    of the charge estimator of the pulse.');
 
 TSDI
      :      'Tpc Smart reduced wire DIgits (NR = sector number).
              Replaces TRDI as of 1992 run.
              Digitizations are in the same order as the hits
              in bank TSIR.  Use TSIR to obtain the order of hits
              and the number of digitizations in each hit. (RAW)'
 
           STATIC
      = (Sample       = INTE   : 'Packed word of 4 samples (digitizations).\
                                  Units are FADC counts from 0 to 255.
                                  The first sample of the 32 bit word is in
                                  bits 24:31, the second in bits 16:23, the
                                  third in bits 8:15, and the fourth in bits
                                  0:7')
      ;
 
 TSIR
      :      'Tpc Smart reduced wIRe hits in ROC format (NR = sector number).
              Replaces TRIR as of 1992 run.
              For nonreduced hits, digitizations are to be found
              in the bank TSDI of the same NR, with the digitizations
              arranged in the same order as these hits.
              If the digitizations are missing in TSDI an appropriate flag is set
              in the nonreduced wire hit entry. (RAW)'
 
           STATIC
      = (WireHit      = INTE   : 'wire hit..\
                                  -- if reduced:\
                                     bits: 0 -12 : peak time * 16\
                                           13    : reduce flag = 1\
                                           14-23 : total charge / 2 (ADC counts)\
                                           24-31 : wire number\
                                   -- if not reduced:\
                                      bits: 0 - 8 : t0 [0:511] time of the 1st bucket \
                                            9 -12 : free\
                                            13    : reduce flag = 0\
                                            14    : if nonzero: too long pulse, no sample in TSDI\
                                            15    : if nonzero: no sample available in TSDI\
                                            16-23 : # of buckets\
                                            24-31 : wire number ')
      ;
 
 TRLE :      'Tpc Reduced pulse LEngth (NR = sector number).
              This bank is parallel to the TRIR bank, having
              one entry (NOT one word!) for each wire pulse 
              This bank has been replaced by TSLE. (RAW)'
           STATIC
      = (PulseLength  = INTE   : 'Packed word of 8 entries, each
                                  entry corresponding to one hit.
                                  For non-reduced hits (see the flag
                                  in each word of TRIR), the content
                                  of this nibble (4 bits) is not defined.
                                  For reduced hits it contains the number
                                  of samples used in the calculation
                                  of the charge estimator of the pulse.');
 
 TRDI
      :      'Tpc Reduced wire DIgits (NR = sector number).
              Digitizations are in the same order as the hits
              in bank TRIR.  Use TRIR to obtain the order of hits
              and the number of digitizations in each hit. This
              bank has been replaced by TSDI. (RAW)'
           STATIC
      = (Sample       = INTE   : 'Packed word of 4 samples (digitizations).\
                                  Units are FADC counts from 0 to 255.
                                  The first sample of the 32 bit word is in
                                  bits 24:31, the second in bits 16:23, the
                                  third in bits 8:15, and the fourth in bits
                                  0:7')
      ;
 
 TRIR
      :      'Tpc Reduced wIRe hits  in TPP format (NR = sector number).
              For nonreduced hits, digitizations are to be found
              in the bank TRDI of the same NR, with the digitizations
              arranged in the same order as these hits.
              This bank has been replaced by TSIR (RAW)'
           STATIC
      = (WireHit      = INTE   : 'wire hit..\
                                  -- if reduced:\
                                     bits: 0 -12 : peak time * 16\
                                           13    : reduce flag = 1\
                                           14-23 : total charge / 2 (ADC counts)\
                                           24-31 : wire number\
                                   -- if not reduced:\
                                      bits: 0 - 8 : t0 [0:511] time of the 1st bucket \
                                            9 -12 : free\
                                            13    : reduce flag = 0\
                                            14-15 : free\
                                            16-23 : # of buckets\
                                            24-31 : wire number ')
      ;
 
 TPAD
      :      'Tpc PAD hits in ROC pad format (NR = sector number).
              The digitizations are to be found in the bank TPDI of
              the same NR and are arranged in the same order as these hits.
              (RAW)'
           STATIC
 
      = (RowHeader    = INTE   : 'row header :\
                                  bits: 0 - 3 : row # in sector\
                                        4 -31 : set to 0',
         WordCount    = INTE   : 'word count for rowlist = # of hits in row',
         FirstPadHit  = INTE   : 'first pad hit..\
                                  bits: 0 - 8 : t0 [0:511] time of first bucket of this hit\
                                        9 -14 : TPD channel #\
                                        16-23 : # of buckets \
                                        24-31 : pad number in row',
         SecondPadHi  = INTE   : 'second pad hit. . .\
                                  and so forth to the final pad hit
                                  of this padrow.  Then one starts again
                                  with the RowHeader of the next padrow.')
      ;
 
 TWDI
      :      'Tpc Wire DIgits (NR = sector number).
              Digitizations are in the same order as the hits
              in bank TWIR.  Use TWIR to obtain the order of hits
              and the number of digitizations in each hit. 
              This bank is not normally found on raw data tapes. See
              TSIR and TSDI for TPP reduced raw data.
              (RAW)'
           STATIC
 
      = (Sample       = INTE   : 'Packed word of 4 samples (digitizations).\
                                  Units are FADC counts from 0 to 255.
                                  The first sample of the 32 bit word is in
                                  bits 24:31, the second in bits 16:23, the
                                  third in bits 8:15, and the fourth in bits
                                  0:7')
      ;
 
 TPDI
      :      'Tpc Pad DIgits (NR = Sector Number)
              Digitizations are in the same order as the hits
              in bank TPAD.  Use TPAD to obtain the order of hits
              and the number of digitizations in each hit.
              (RAW)'
           STATIC
 
      = (SampleByte   = INTE   : 'Packed word of 4 samples (digitizations).
                                  Units are FADC counts from 0 to 255.
                                  The first sample of the 32 bit word is in
                                  bits 24:31, the second in bits 16:23, the
                                  third in bits 8:15, and the fourth in bits
                                  0:7')
       ;
 
 TWIR
      :      'Tpc WIRe hits  in TPP format (NR = sector number).
              This bank is not normally found on raw data tapes.
              See banks TSIR and TSDI for TPP reduced data.
              Digitizations are to be found
              in the bank TWDI of the same NR, with the digitizations
              arranged in the same order as these hits.
              (RAW)'
           STATIC
      = (WireHit      = INTE   : 'wire hit..\
                                      bits: 0 - 8 : t0 [0:511] time of the 1st bucket \
                                            9 -15 : free\
                                            16-23 : # of buckets\
                                            24-31 : wire number ')
      ;
 
 TISL
      :      'Tpc ISLands (NR = sector number)
              This is reconstructed from raw data online
              in the TPP or else offline in JULIA. In fact,
              it has never been implimented in the TPP,
              and likely never will be (there being no
              real advantage in doing so).'
           STATIC
 
      = ( RowHeader   = INTE   : 'bits  0-15   row number\
                                  bits 16-31   # clusters in row\
                                               or FFFF if tpp timed out\
                                               in this row',
          RowLength   = INTE   : '# clusters + # pulses in this row',
          ClusterHead = INTE   : 'bits  0-15   # pulses in cluster\
                                  bit  31      ordering flag',
          TpadOffset  = INTE   : 'full offset to TPAD for pulse\
                                      etc. for n pulses\
                                  and etc. for all hit rows')
          ;
 
 
 
 END ESET
 
 END SUBSCHEMA

 
SUBSCHEMA TpcSLOWBanks
 : 'Description of TPC slow control banks'
 
 AUTHOR   'W. Tejessy'
 REVIEWER 'M. Kasemann'
 VERSION  '2.0'
 DATE     '11/6/90'
 
 DEFINE ESET
 
  
TSCI
      :      'Tpc Slow Control Interleaved information giving status 
              of apparatus at regular intervals as sequential set of
              vectors for various hardware subassemblies. The order of
              hardware indices is not defined.\
              Number of information units per row\
              Number of rows = 1'
            STATIC
 
      = (Hardware1    = INTE            : 'hardware index 1 = Fastbus',
         Entries1     = INTE            : 'No. of entries for index 1',
         FastBus      = INTE            : 'E1 (=6) words of status bits for\
                                              fastbus crates',
         Hardware2    = INTE            : 'hardware index 2 = preamp. voltages',
         Entries2     = INTE            : 'No. of entries for index 2',
         PreampVolt   = INTE            : '36 * 2 words preamplifier voltages\
                                                  [voltage * 100]',
         Hardware3    = INTE            : 'hardware index 3 = gating voltages',
         Entries3     = INTE            : 'No. of entries for index 3',
         GatingVolt   = INTE            : '36  * 3 words gating voltage\
                                                  [voltage * 100]', 
         Hardware4    = INTE            : 'hardware index 4= HV curr. and volt',
         Entries4     = INTE            : 'No. of entries for index 4',
         HighVolt     = INTE            : '36  * 2 words: HV current\
                                                  wire potential', 
         Hardware5    = INTE            : 'hardware index 5 = sector Temper.',
         Entries5     = INTE            : 'No. of entries for index 5',
         SectorTemp   = INTE            : '36 * 5 words temperature\
                                                  [degrees * 10]', 
         Hardware6    = INTE            : 'hardware index 6 = gating voltages',
         Entries6     = INTE            : 'No. of entries for index 6',
         NN           = INTE            : '    spare for\
                                                  future use', 
         Hardware7    = INTE            : 'hardware index 7 = gas parameters',
         Entries7     = INTE            : 'No. of entries for index 7',
         GasPressure  = INTE            : '6 words various gas pressures\
                                                  [millibar * 10]',
         GasTemper    = INTE            : '6 words various gas temperatures\
                                                  [degrees * 10]', 
         GasMiscellan = INTE            : '4 words miscellaneous parameters\
                                                  [units * 10]',
         GasStatus    = INTE            : '2 words gas alarm status\
                                                  bitwise warnings & alarms',
         Hardware8    = INTE            : 'hardware index 8= Fe monit. chamber',
         Entries8     = INTE            : 'No. of entries for index 8',
         RUnMode      = INTE            : 'Run mode',
         FailMode     = INTE            : 'Failure mode',
         RunMinutes   = INTE            : 'Run minutes',
         Fe55peak     = INTE            : 'Fe55 peak * 100',
         GasAmplific  = INTE            : 'Gas amplification * 100',
         DeltaGasamp  = INTE            : 'Delta gas amplification',
         WirePotent   = INTE            : 'Wire potential * 100',
         CorrPotent   = INTE            : 'corr. potential * 100',
         TpcPressure  = INTE            : 'actual pressure in gas system',
         TpcTemperat  = INTE            : 'actual gas temper. at D4')
      ;
 
TSSR
      :      'Tpc Slow control SoR information giving status 
              of apparatus at start of run as sequential set of
              vectors for various hardware subassemblies. The order of
              hardware indices is not defined.\
              Number of information units per row\
              Number of rows = 1'
            STATIC
 
      = (Hardware1    = INTE            : 'hardware index 1 = Fastbus',
         Entries1     = INTE            : 'No. of entries for index 1',
         FastBus      = INTE            : 'E1 (=6) words of status bits for\
                                              fastbus crates',
         Hardware2    = INTE            : 'hardware index 2 = preamp. voltages',
         Entries2     = INTE            : 'No. of entries for index 2',
         PreampVolt   = INTE            : '36 * 2 words preamplifier voltages\
                                                  [voltage * 100]',
         Hardware3    = INTE            : 'hardware index 3 = gating voltages',
         Entries3     = INTE            : 'No. of entries for index 3',
         GatingVolt   = INTE            : '36  * 3 words gating voltage\
                                                  [voltage * 100]', 
         Hardware4    = INTE            : 'hardware index 4= HV curr. and volt',
         Entries4     = INTE            : 'No. of entries for index 4',
         HighVolt     = INTE            : '36  * 2 words HV current &\
                                                  wire potential', 
         Hardware5    = INTE            : 'hardware index 5 = sector Temper.',
         Entries5     = INTE            : 'No. of entries for index 5',
         SectorTemp   = INTE            : '36 * 5 words temperature\
                                                  [degrees * 10]', 
         Hardware6    = INTE            : 'hardware index 6 = gating voltages',
         Entries6     = INTE            : 'No. of entries for index 6',
         NN           = INTE            : '    spare for\
                                                  future use', 
         Hardware7    = INTE            : 'hardware index 7 = gas parameters',
         Entries7     = INTE            : 'No. of entries for index 7',
         GasPressure  = INTE            : '6 words various gas pressures\
                                                  [millibar * 10]',
         GasTemper    = INTE            : '6 words various gas temperatures\
                                                  [degrees * 10]', 
         GasMiscellan = INTE            : '4 words miscellaneous parameters\
                                                  [units * 10]',
         GasStatus    = INTE            : '2 words gas alarm status\
                                                  bitwise warnings & alarms',
         Hardware8    = INTE            : 'hardware index 8= Fe monit. chamber',
         Entries8     = INTE            : 'No. of entries for index 8',
         RUnMode      = INTE            : 'Run mode',
         FailMode     = INTE            : 'Failure mode',
         RunMinutes   = INTE            : 'Run minutes',
         Fe55peak     = INTE            : 'Fe55 peak * 100',
         GasAmplific  = INTE            : 'Gas amplification * 100',
         DeltaGasamp  = INTE            : 'Delta gas amplification',
         WirePotent   = INTE            : 'Wire potential * 100',
         CorrPotent   = INTE            : 'corr. potential * 100',
         TpcPressure  = INTE            : 'actual pressure in gas system',
         TpcTemperat  = INTE            : 'actual gas temper. at D4')
      ;
 
 TSCE
      :      'Tpc Slow Control Error information giving the hardware
              vectors of those subassemblies that show 
              warnings/alarms, in same format as TSCI banks\
              Number of information units per vector per row\
              Number of rows = 1'
             STATIC
 
      = (HardwareI    = INTE            : 'hardware index I (having alarms)',
         EntriesI     = INTE            : 'No. of entries for index I',
         AlarmsI      = INTE            : 'EI words of information for\
                                              subassembly with alarms',
         HardwareJ    = INTE            : 'hardware index J (having alarms)',
         EntriesJ     = INTE            : 'No. of entries for index J',
         AlarmsJ      = INTE            : 'EJ words of information for\
                                              subassembly with alarms;...etc.')
      ;
 
 TSEE
      :      'Tpc Slow Control Error End information signalling
              the return to normal conditions\
              Number of information units per vector per row\
              Number of rows = 1'
             STATIC
 
      = (HardwareI    = INTE            : 'hardware index I back to normal',
         EntriesI     = INTE            : 'No. of entries for index I',
         AlarmsI      = INTE            : 'EI words of information for\
                                              subassembly with alarms')
      ;
 
 NEVE
      :      'Date & time of writing of all banks of this record into the
              CDF buffer. Used in conjunction with TPC slow control records.
              NR=0; no. of rows and columns information suppressed.'
             STATIC
 
      = (DAte         = INTE            : 'Date of record Xfer to CDF buffer;\
                                        Form yyyymmdd (yymmdd before 89.12.11)',
         TIme         = INTE            : 'Time of such Xfer in form hhmmss')
      ;
 
 TNMR
      :      'Tpc Nuclear Magnetic Resonance meaurement of the magnetic
              field at the endcaps\
              Number of information units per row\
              Number of rows = 1'
             STATIC
 
      = (DAte         = INTE            : 'Date of reading [yyyymmdd]',
         TIme         = INTE            : 'Time of reading [hhmmss]',
         UPdate       = INTE            : 'Update rate [hhmmss]',
         BfieldA      = INTE            : 'Magnetic Field Side A [Gauss]',
         BfieldB      = INTE            : 'Magnetic Field Side B [Gauss]')
      ;
 
 END ESET
 
END SUBSCHEMA
 
 SUBSCHEMA TpcGALBanks 
 : 'Description of TPC simulation banks'
 
 AUTHOR   'A. Putzer'
 REVIEWER 'R. Johnson,M.Mermikides,F.Ranjard'
 VERSION  '1.6'
 DATE     '10/07/92'
 
 DEFINE ESET
 
 TPHT
      :      'Tpc Pad HiT bank (NR=0).
              This is MC truth information.  These should
              be sorted in order of increasing ChannelId.\
              Number of words/hit \
              Number of hits '
           STATIC
 
      = (KinematicTr  = INTE               : 'Kinematic track number',
         ChannelId    = INTE               : '100000*padrow number +\
                                              1000*sector number + pad number',
         Phi          = REAL               : 'phi',
         ZV           = REAL               : 'z',
         DphiDr       = REAL               : 'dphi/dr',
         DZdr         = REAL               : 'dZ/dr')
      ;
 
 
 TTHT
      :      'Tpc Trigger pads HiT bank
              This is MC truth info.\
              Number of words/hit\
              Number of hits '
         STATIC
      = (KinematicTr  = INTE               : 'Kinematic track number',
         ChannelId    = INTE               : 'channel ID, see below',
         Phi          = REAL               : 'phi',
         ZV           = REAL               : 'z',
         DphiDr       = REAL               : 'dphi/dr',
         DZd          = REAL               : 'dz/dr')
      ;
 
TPCH
      :      'Tpc Pad Coordinates ref to monte carlo pad Hit \
              Number of words/coord.\
              Number of coordinates'
           STATIC
 
      = (IndexofHit   = INTE [1,*] : 'Row number of hit in TPHT MC bank',
         NumberHits   = INTE [1,*] : 'Number of MC hits in the cluster')
      ;
 
 
 TRCL
     :    'Tpc Reference from CLusters to mc hits (NR=sector slot).
           The row numbers correspond one-to-one with TCLU.\
           Number of words per cluster\
           Number of clusters'
         STATIC
        = (NumberHits   = INTE [0,*]       : 'Number of Monte Carlo hits
                                              matching this cluster',
           OffsetHit    = INTE [0,*]       : 'Offset of first hit in
                                              the bank TMCL')
        ;
 
 
 TMCL
     :    'Tpc Monte carlo-CLuster pointers.
           Use the TRCL bank to index into this bank.\
           Number of words per MC hit\
           Number of MC hits'
         STATIC
       = (PoinTer       = INTE [1,*]       : 'Pointer into TPHT bank')
        ;
 
 TGMA 
      :      'Tpc Geometric track to Monte carlo Association
              Rows correspond to rows in TGFT or FRFT bank \
              Number of words per track\
              Number of TPC tracks '
           STATIC 
      = (NumberMc     = INTE [0,*]         : 'Number of MC tracks associated',
         OffsetMc     = INTE [0,*]         : 'Offset to first MC track in TMTL',
         NumCoords    = INTE [1,*]         : 'Number of coordinates on the track.\
                                              We consider only those coordinates used\
                                              in the track fit (1st half loop)',
         NumAssoc     = INTE [0,*]         : 'Number of coordinates associated with a MC track')
      ;
 
 TMTL
      :      'Tpc association Monte carlo Track List.
              Use TGMA to index into this bank \
              Number of words/associated MC track \
              Number of associated MC tracks '
           STATIC
      = (McTrack      = INTE [1,*]         : 'MC track number in FKIN',
         NumHits      = INTE [1,*]         : 'Number of hits shared with this MC track',
         ChiSquared   = REAL [0.00,*]      : 'Chisquared of helix parameters')
      ;
 
 
 
 TPHE
      :      'Tpc Pad Hit to track Element reference \
              Number of words/hit \
              Number of hits'
           STATIC
 
      = (IndexTrackel = INTE            : 'Track element number')
      ;
 
 TTHE
      :      'Tpc Trigger pad Hit to track Element reference \
              Number of words/hit \
              Number of hits'
           STATIC
 
      = (IndexTrackel = INTE             : 'Track element number')
      ;
 
 TPTE
      :      'TPc Track Element
              This is MC truth info \
              Number of words/track element \
              Number of track element .'
           STATIC
 
      = (KinematicTr  = INTE               : 'kinematic track number',
         XAtstartoft  = REAL               : 'x at start of track-element',
         YAtstartoft  = REAL               : 'y at start of track-element',
         ZAtstartoft  = REAL               : 'z at start of track-element',
         DXdsatstart  = REAL               : 'dx/ds at start of track-element',
         DYdsatstart  = REAL               : 'dy/ds at start of track-element',
         DZdsatstart  = REAL               : 'dz/ds at start of track-element',
         PV           = REAL               : 'p',
         LeNgth       = REAL               : 'length',
         TimeofFligh  = REAL               : 'time of flight to start point',
         ParticleMas  = REAL               : 'particle mass',
         CHarge       = REAL               : 'charge (can be 0.)')
       ;
 
                                                                                
 END ESET
 
 END SUBSCHEMA

 
 SUBSCHEMA TpcJULBanks
 : 'Description of TPC reconstruction banks'
 
 AUTHOR   'D. Schlatter','M. Mermikides','R. Johnson'
 REVIEWER 'I.Tomalin,D.Casper'
 VERSION  '5.0'
 DATE     '04/03/97'
 
 DEFINE ESET
 
 T2XS    
     :    'Tpc dE/dX Segment for Overlapping Tracks (NR=0).
           One track pair segment per sector crossed\
           Number of words per pair segment \
           Number of pair segments'
        STATIC
       = (SegmentId     = INTE [1,*]       : 'Sector slot number',
          TruncatedMean = REAL [0.000,*]   : 'Truncated mean of dE/dx measurements',
          TrackLength   = REAL [0.0,500.0] : 'Useful length of track for dE/dx',
          NumberSamples = INTE [1,500]     : 'Number of samples used for dE/dx',
          AverageDrift  = REAL [0.0,220.0] : 'Average drift length of samples',
          Tracknumber1  = INTE [1,*]       : 'Pointer to first track entry in TGFT',
          Tracknumber2  = INTE [1,*]       : 'Pointer to second track entry in TGFT')
       ;
  
 TWOL
      :      'Tpc Tracks that share wire hits (NR=0) \
              Number of words/row\
              Number of tracks pairs'
         STATIC
 
      = (Tracknumber1 = INTE [1,*]      : 'Pointer to first track entry in TGFT',
         Tracknumber2 = INTE [1,*]      : 'Pointer to second track entry in TGFT')
      ;
 
 TCLU
      :      'Tpc-pad CLUster (NR = Sector number) \
              Number of words per cluster \
              Number of clusters in sector'
           STATIC
 
      = (OffsetPad    = INTE [0,*]         : 'offset of first pad-pulse\
                                              in cluster into TPUL',
         NumberPulses = INTE [1,6144]      : 'number of pad-pulses in cluster',
         FirstBucket  = INTE [1,512]       : 'min. bucket number',
         LastBucket   = INTE [1,512]       : 'max. bucket number',
         FirstPad     = INTE [1,150]       : 'min. pad number',
         LastPad      = INTE [1,150]       : 'max. pad number',
         PadrowNumber = INTE [1,12]        : 'padrow number in TPC (in sector frame)')
      ;
 
 TLRL
      :      'Tpc cLuster Row List (NR= Sector number) \
              Number of words/row\
              Number of rows in sector'
         STATIC
 
      = (NumberCluste = INTE [0,6144]      : 'number of clusters in this row',
         OffsetCluste = INTE [0,*]         : 'offset of first cluster of row in TCLU')
      ;
 
 TSRL
      :      'Tpc Subcluster Row List (NR= Sector number) \
              Number of words/row\
              Number of rows in sector'
         STATIC
 
     = ( NumberSub    = INTE [0,6144]      : 'number of subclusters in this row',
         OffsetSub    = INTE [0,*]         : 'offset of first subcluster of row into TSCL')
      ;
 
 TREP
      :      'Tpc error REPorting\
              number of words/error code\
              number of error codes reported'
           STATIC
 
      = (MessageCode   = INTE [1,*] :  'Code for the error message\
                                       (unique within the subroutine)',
         NumberCalls   = INTE [1,*] :  'Number of times that this\
                                       error was encountered',
         Byte1         = INTE [*,*] :  'First byte of the subroutine\
                                       name where error was encountered',
         Byte2         = INTE [*,*] :  'Second byte of subroutine name')
       ;
 
 TCRL
      :      'Tpc-Coordinates Row List (NR=0)\
              Number of words/ padrow\
              Number of padrows'
           STATIC
 
      = (OffsetCoord  = INTE [0,*]         : 'offset of first coordinate\
                                              in row into TPCO',
         NumberCoord  = INTE [0,*]         : 'number of coordinates in row',
         N1           = INTE [0,*]         : 'number of coordinates with z>0')
      ;
 
 TBCO
     :       'Tpc Bad COordinate list (NR=0); These are derived from
              either TCLU or TSCL, depending on where along the
              analysis chain the coordinate failed a cut.\
              Number of words per coordinate \
              Number of coordinates'
           STATIC
        = (INdex       = INTE [101001,2136150] : '100000*padrow number +\
                                                 1000*sector number + pad number',
           RValue      = REAL [30.00,180.]     : 'radius [cm]',
           PHi         = REAL [0.00,6.3]       : 'phi    [radians]',
           ZValue      = REAL [-220.00,220.]   : 'z      [cm]',
           RphiWidth   = INTE [1,255]          : 'number of pads in cluster',
           ZWidth      = INTE [1,255]          : 'length of cluster in buckets',
           SubCluster  = INTE [1,*]            : 'subcluster number in TSCL if >0\
                                                  -(cluster number) in TCLU if <0')
      ;
 
 TPCO
      :      'Tpc Pad Coordinates in global system.
              NR=1: raw coordinates.
              NR=0: final coordinates, after
              removal of overlapping coordinates from
              track candidates and addition of twin coordinates.
              The coordinates are
              sorted in order of increasing Index.\
              Number of words/coord.\
              Number of coordinates'
           STATIC
 
      = (Index        = INTE [101001,2136150] : '100000*padrow number +\
                                                 1000*sector number + pad number',
         RValue       = REAL [30.000,180.]    : 'r      [cm]',
         Phi          = REAL [0.000,6.3]      : 'phi    [rad]',
         ZValue       = REAL [-216.00,216.]   : 'z      [cm]',
         SigRphi      = REAL [0.0000,*]       : 'sigma**2 of R*Phi',
         SigZ         = REAL [0.0000,*]       : 'sigma**2 of Z',
         OriginFlag   = INTE [0,5]            : 'Origin flag:\           
                                                 0=can not remember\
                                                 1=TCOOR\
                                                 2=TWINCO\
                                                 3=TSACOR\
                                                 4=TCOORT\
                                                 5=TPCOOR Fake Coordinate',
         TrackNumber  = INTE [*,*]            : 'track number\
                                                 = 0 if coordinate not used,\
                                                 = -(track number) if associated\
                                                   but not used in fit\
                                                   Warning: which track bank this refers
                                                   to depends on what stage of the program
                                                   is being executed.  At the end it refers
                                                   to FRFT.',
         ClusterNumbe = INTE [1,*]            : 'sub-cluster number (per sector)',
         IT           = INTE [0,*]            : 'work space',
         RawRphi      = REAL [-55.,55.]       : 'Uncorrected r*phi in the sector
                                                 coordinate system',
         RawZ         = REAL [0.,220.]        : 'Uncorrected z in the sector
                                                 coordinate system')
         ;
 
 TPRL
      :      'TPc Row List, containing offsets and counters (NR=sector number)\
              Number of words/row\
              Number of sector padrows'
           STATIC
 
      = (NumberPulses =  INTE [0,6144]  : 'number of pulses /sector-row',
         OffsetPulse  =  INTE [0,*]     : 'offset for 1. pulse in row into TPAD',
         NumberSample =  INTE [0,76800] : 'total number of samples / row',
         OffsetSample =  INTE [0,*]     : 'offset for 1. sample of first pulse')
      ;
 
 TPUL
      :      'Tpc pad-PULses (NR = Sector number) \
              Number of words/ pulse\
              Number of pulses'
           STATIC
 
      = (OffsetPulse  = INTE [0,*]         : 'offset of pulse into TPAD (in words)',
         OffsetSample = INTE [*,*]         : 'offset of 1st sample into TPDI (in BYTES)',
         NumberSample = INTE [1,512]       : 'number of samples in pulse',
         ClustPointer = INTE [1,*]         : 'pointer to cluster in TCLU')
      ;
 
 TWTB
     :       'Tpc Wire-Track Association (NR=0).
              Use TWIT and TWAT to index into this bank.\
              Number of words per wire pulse \
              Number of associated wire pulses'
           STATIC
      = (WireId       = INTE [1001,*]        : 'Sector number*65536 + pointer into TWRR',
         ChargeEst    = REAL [1.,*]          : 'charge estimate (calibration corrected)',
         SampleLength = REAL [0.20,*]        : 'length of dE/dx sample in cm',
         zPositioN    = REAL [0.00,220.00]   : 'z position of hit in sector frame (drift length),
                                                based on the track extrapolation.  You will have
                                                to look back to TWRR to get the measured wire pulse time.',
         RPosition    = REAL [0.00,180.00]   : 'Radial position of hit in sector frame')
      ;
 
 TWIT :      'Number of TPC wire hits associated with each track in TGFT.
              To be used in conjuction with TWAT and TWTB.
              Rows in this bank correspond with rows in TGFT.
              A good hit is one which matches a single track.
              A bad hit is one which matches two or more hits,
              though by default all hits are dropped which match more than 
              two tracks or which match even one secondary arc of
              a track.\
              Number of words per track\
              Number of tracks'
           STATIC
      = (OffSet    = INTE [0,*]   : 'Pointer to the row in TWAT just before
                                     the first hit for this track.',
         NumGood   = INTE [0,*]   : 'number of good wire hits',
         NumBad    = INTE [0,*]   : 'number of bad wire hits');
 
 TWAT :       'Pointers from TPC tracks to wire hits.
              To be used in conjuction with TWIT and TWTB.
              Use TWIT to point into this bank.\
              Number of words per pointer\
              Number of pointers'
           STATIC
      = (PoinTer   = INTE [0,*]   : 'Pointer to wire hit in TWTB.
                                     All hits for a given track are
                                     consecutive, with the bad hits
                                     following the good hits.');
 
 TWTA
     :       'Tpc Wire-Track Association (NR=0).
              This bank is obsolete.  Use TWTB and TWIT instead.\
              Number of words per wire pulse \
              Number of associated wire pulses'
           STATIC
      = (WireId       = INTE [1001,*]        : 'QW*100000 + sector # *1000 + wire number \
                                                QW is a quality word, where 0 means good',
         ChargeEst    = REAL [1.,*]          : 'charge estimate (calibration corrected)',
         SampleLength = REAL [0.20,*]        : 'length of dE/dx sample in cm',
         ZResidual    = REAL [-512.00,512.]  : 'z residual from fitted track in cm',
         zPositioN    = REAL [0.00,220.00]   : 'z position of hit in sector frame (drift length)',
         RPosition    = REAL [0.00,180.00]   : 'Radial position of hit in sector frame',
         TrackNumber  = INTE [1,*]           : 'pointer to associated track in TGFT')
      ;
 
 TWRR
      :      'Tpc WiRe pulses in Reduced format (NR = sector number).\
              Number of words per wire pulse \
              Number of wire pulses'
           STATIC
      = (WireHit      = INTE               : 'wire hit..\
                                              bits: 0 -12 : peak time * 16\
                                                    13    : quality flag (1=good)\
                                                    14-23 : total charge / 2\
                                                    24-31 : wire number')
      ;
 
 TWRF:     'Tpc Wire ReFerence (NR=sector number).
            This bank is only produced by JULIA if the
            card TOPT "TWPU" is included in the job.
            It is used by various debugging dump
            routines to give a reference between
            TWRR and TSIR/TSDI.  This bank is
            parallel to TWRR.\
            Number of words per reduced wire pulse (=1)\
            Number of reduced wire pulses'
         STATIC
      = (RefWord   = INTE  : 'Packed word:\
                              bits: 0-15: pointer to TSIR hit

                                   16-31: offset into TSDI of first digitization');
 TEXS    
     :    'Tpc dE/dX Segment (NR=0) 
           One track segment per sector crossed\
           Number of words per segment \
           Number of segments'
        STATIC
       = (SegmentId     = INTE [1,*]       : 'Sector slot number',
          TruncatedMean = REAL [0.000,*]   : 'Truncated mean of dE/dx measurements',
          TrackLength   = REAL [0.0,500.0] : 'Useful length of track for dE/dx',
          NumberSamples = INTE [1,500]     : 'Number of samples used for dE/dx',
          AverageDrift  = REAL [0.0,220.0] : 'Average drift length of samples',
          TrackNumber   = INTE [1,*]       : 'Pointer to track entry in TGFT',
          SatFlag       = INTE [0,1]       : '1= >40% saturated hits.\
                                              0= <40% saturated hits')
       ;
 
 TLNK
      :      'Tpc chain LiNK table (NR=0)\
              Number of words/chain\
              Number of chains'
           STATIC
 
      = (UPlink       = INTE [0,*]        : 'Upstream chain number',
         DowNlink     = INTE [0,*]        : 'Downstream chain number',
         ToPlink      = INTE [0,*]        : 'Number of chain connected to top\
                                             part (from opposite semicircle)',
         BottoMlink   = INTE [0,*]        : 'Number of chain connected to lower\
                                             part (from opposite semicircle)')
      ;
 
 TWPU
      :      'Tpc Wire-PUlses  (NR = Sector number).
              This bank is obsolete and no longer produced by JULIA.
              It has been replaced by TWRF, which uses far less memory.\
              Number of words/ pulse\
              Number of pulses'
           STATIC
 
      = (OffsetPulse  = INTE [0,*]         : 'offset of pulse into TWIR',
         OffsetSample = INTE [0,*]         : 'offset of first sample\
                                              in this pulse into TWDI',
         NumberSample = INTE [1,512]       : 'number of samples in pulse',
         WireNumber   = INTE [1,300]       : 'sector wire number for this pulse',
         ChargeEstim  = INTE [1,130560]    : 'charge estimate ( sum of pulse heights )',
         TimeEstimate = REAL [0.0,512.]    : 'time estimate',
         TwtaPointer  = INTE [-1,*]        : 'pointer to TWTB')
      ;
 
 T1FT  :     'Tpc First geometrical track FiT (NR=1), before
              joining broken tracks in TFLNK2\
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (InverseRadi  = REAL [*,*]         : 'Inverse radius of curvature in x-y projection\
                                              signed +ve if track bends counterclockwise\
                                                     -ve if tracks bends clockwise',
         TanLambda    = REAL  [*,*]        : 'tangent of dip angle',
         Phi0         = REAL  [0.,6.3]     : 'Phi at closest point of approach to line x=y=0',
         D0           = REAL  [-180.,180.] : 'Closest distance of approach to line x=y=0\
                                              in the x-y plane (impact parameter)\
                                             (signed +ve if particle has a positive angular\
                                              momemtum around the origin, -ve otherwise)',
         Z0           = REAL  [-220.,220.] : 'Z-coordinate at D0',
         EcovMat(15)  = REAL  [*,*]        : 'Triangular covariance matrix stored in the order:\
                                                      1  2  4  7 11\
                                                         3  5  8 12\
                                                            6  9 13\
                                                              10 14\
                                                                 15',
         ChisquareD   = REAL [0.,*]          : 'Chisquare of helix fit',
         numDegFree   = INTE [0,53]          : 'Number of degrees of freedom',
         TCal         = INTE [1,*]           : 'Pointer to track candidate from which
                                                this track derives',
         TGft         = INTE [1,*]           : 'Pointer to the track in TGFT.  More than
                                                one T1FT track can point to one TGFT track.')
      ;
 
 T1TL  :    'Tpc Geometrical Track point List.  NR=0.
             This is parallel to the T1FT bank.\
             Number of words per track \
             Number of Tpc geometry tracks '
           STATIC
      = (IOff      = INTE [0,10000]   : ' offset in TGCL',
         N1arc     = INTE [3,21]      : ' numb. coord. in first arc',
         Nrest     = INTE [0,1000]    : ' numb. coord. in following spirals')
      ;
  
 T1CL  :    'Tpc Geometrical track Coordinate List. NR=0.
             Use T1TL to index into this bank.\
             Number of words per coordinate \
             Number of coordinates associated with tracks '
           STATIC
      = (ITpco     = INTE [1,10000]  : 'coordinate number in TPCO')
      ;  
 
 TGFT :     'Tpc Geometrical track FiT (NR=1)\
             Number of words/track\
             Number of tracks'
           STATIC
      = (InverseRadi  = REAL [*,*]         : 'Inverse radius of curvature in x-y projection\
                                              signed +ve if track bends counterclockwise\
                                                     -ve if tracks bends clockwise',
         TanLambda    = REAL  [*,*]        : 'tangent of dip angle',
         Phi0         = REAL  [0.,6.3]     : 'Phi at closest point of approach to line x=y=0',
         D0           = REAL  [-180.,180.] : 'Closest distance of approach to line x=y=0\
                                              in the x-y plane (impact parameter)\
                                             (signed +ve if particle has a positive angular\
                                              momemtum around the origin, -ve otherwise)',
         Z0           = REAL  [-220.,220.] : 'Z-coordinate at D0',
         EcovMat(15)  = REAL  [*,*]        : 'Triangular covariance matrix stored in the order:\
                                                      1  2  4  7 11\
                                                         3  5  8 12\
                                                            6  9 13\
                                                              10 14\
                                                                 15',
         ChisquareD   = REAL [0.,*]          : 'Chisquare of helix fit',
         numDegFree   = INTE [0,53]          : 'Number of degrees of freedom',
         TC           = INTE [1,*]           : 'Pointer to fitted track in the FRFT bank')
      ;
 
 TELS
     :       'Tpc track Energy LoSs (NR=0) \
              Number of words per track \
              Number of tracks with dE/dx'
           STATIC                                                                      
       = (NumberseGments= INTE [1,10]      : 'Number of segments in TEXS',
          SegmentOffset = INTE [0,*]       : 'Offset to first segment in TEXS',
          TrackNumber   = INTE [0,*]       : 'Index of track in TGFT')
         ;
 
 TSCL
     :       'Tpc Sub-CLuster (NR = Sector number) \
              Number of words per sub-cluster \
              Number of sub-clusters in sector'
           STATIC
 
     = (NumSubpulses   = INTE [2,150]      : 'Number of subpulses from TSPU',
        tsPuOffset     = INTE [2,*]        : 'Offset into TSPU of 1st subpulse',
        FirstPad       = INTE [1,*]        : 'Pad number of first subpulse',
        NumCoord       = INTE [0,*]        : 'Number of coordinates or
                                              TCOALG error code if <0',
        tpCoOffset     = INTE [0,*]        : 'Offset to TPCO of first coordinate',
        TotalCharge    = INTE [0,*]        : 'Total subcluster charge',
        AlgWord        = INTE [0,*]        : 'Q and t algorithm choice.\
                                              1000*Qalg + Talg\
                                              Qalg:  0= no valid estimator;
                                                     1= parabola algorithm;
                                                     2= sum of all samples\
                                              Talg:  0= no valid estimator;        
                                                     1= parabola algorithm;
                                                     2= threshold algorithm;
                                                     3= weighted mean',
        QUality        = INTE [0,*]        : 'Subcluster quality flag.\
                                              QU > 99 means subcluster is
                                              not useable for finding a single
                                              coordinate.\
                                              By decimal digits:\
                                              1:   0=no overlap in $z$ and 1=at
                                                   least one neighbor in $z$\
                                              2:   0=isolated in r*phi and
                                                   1=neighbor on at least 1 side\
                                              3:   number of peaks in z minus 1\
                                              4:   number of minima in r*phi\
                                              5:   1=adjacent in r*phi to large pulse\
                                              6:   1=contains subpulses with too
                                                   many saturated samples\
                                              7:   1=clusters hanging off TPC end',
        SigmaPulse     = REAL [0.0,*]      : 'RMS width of cluster in units of pulses',
        SigmaBucket    = REAL [0.0,*]      : 'RMS length of cluster in units of buckets',
        CoRrelation    = REAL [*,*]        : 'Correlation coefficient between
                                              SigmaPulse and SigmaBucket, defined as
                                              rho= (sigma12)**2/sigma1/sigma2 (dimensionless)',
        CLuster        = INTE [1,*]        : 'Pointer to TCLU cluster')
     ;
 
 TSPU
     :       'Tpc Sub-PUlse (NR = Sector number) \
              Number of words per sub-pulse \
              Number of sub-pulses in sector'
           STATIC
                                                                      
     = (TotalCharge    = INTE [1,*]         : 'Sum of charge between thresholds T1 and T2',
        PeakCharge     = REAL [1.0,*]       : 'Height of the peak',
        PeakTime       = REAL [0.0,512.]    : 'Time of subpulse peak',
        InitialTime    = REAL [0.0,512.]    : 'Time of leading edge',
        Time1          = REAL [0.0,512.]    : 'Threshold crossing 1',
        Time2          = REAL [0.0,512.]    : 'Threshold crossing 2',
        QualityFlag    = INTE [0,*]         : 'Subpulse quality\
                                              By decimal digit:\
                                              1:  number of peaks\
                                              4:  0=isolated in $z$ and
                                                  1=overlap with leading or
                                                  trailing neighbor\
                                              5:  1 if adjacent in r*phi to a large
                                                  amount of pulse height from 
                                                  a pulse not included in the subcluster\
                                              6:  1 if too many samples are saturated and
                                                  0 otherwise',
        PulseNumber    = INTE [1,*]         : 'Index into TPUL',
        SubCluster     = INTE [0,*]         : 'Index into TSCL')
      ;
 
 TCHA
      :      'Tpc CHAin (track candidate)
              Used internally by track-finder (NR=0)\
              Number of words/chain \
              Number of chains'
           STATIC
 
      = (InvRadofcurv = REAL [0.,*]        : 'Curvature in x-y projection (signed)',
         TangentLambda= REAL [*,*]         : 'Tangent of dip angle',
         Phi0         = REAL [0,6.3]       : 'Phi at closest point of approach to origin',
         D0           = REAL [-180.,180.]  : 'Closest distance of approach in x-y plane (signed)',
         Z0           = REAL [-220.,220.]  : 'Z-coordinate at D0',
         LengthofCha  = REAL [24.,550.]    : 'Length of chain in x-y projection',
         Chisq1       = REAL [0.,*]        : 'Chisq/degrees of freedom of circle (x-y) fit',
         Chisq2       = REAL [0.,*]        : 'Chisq/degrees of freedom of line (s-z) fit',
         ERRmat(15)   = REAL [0.,*]        : 'Inverse of fit covariance matrix in symmetric storage.',
         PadrowIndex  = INTE [15,2097152]  : 'Bit pattern corresponding to padrows hit,\
                                              positions 1-21 refer to rows 1-21',
         OFfset       = INTE [0,*]         : 'Offset for first coord in bank TCTC',
         NoofCoords   = INTE [4,21]        : 'Number of coords in chain')
      ;
 
 TCTC
      :      'Tpc Chain To Coordinates relation\
              Number of words/coord\
              Number of used coordinates'
           STATIC
 
      = (CoordRef     = INTE [0,*]        : 'Row number of coordinate in TPCO bank')
      ;
 
 TCAL  :     'Tpc CAndidate List.
              NR=1:  original output from TFCAND.
              NR=0:  after removing coordinates from overlapping
              tracks and insertion of twin coordinates.
              Note that the coordinates are ordered according to
              distance along the helix, in the direction of
              flight of the particle.\
              Number of words per track candidate \
              Number of TPC track candidates '
             STATIC
 
      = (Ioff         = INTE [0,10000]   : 'first coordinate in TTCC',
         Nfirst       = INTE [3,21]      : 'numb. of coord. in first arc',
         Nrest        = INTE [0,10000]   : 'numb. of coord. following')
      ;
 
 TARC  :    'Tpc ARCs (NR=0).  This bank contains those extra half loops
             of TPC tracks which are not contained in the fit bank
             TGFT.  Only a rough helix fit is done.\
             Number of words per arc\
             Number of arcs'
          STATIC
      = (InvRadofcurv = REAL [0.,*]        : 'Curvature in x-y projection (signed)',
         TangentLambda= REAL [*,*]         : 'Tangent of dip angle',
         Phi0         = REAL [0,6.3]       : 'Phi at closest point of approach to origin',
         D0           = REAL [-180.,180.]  : 'Closest distance of approach in x-y plane (signed)',
         Z0           = REAL [-220.,220.]  : 'Z-coordinate at D0',
         Chisq1       = REAL [0.,*]        : 'Chisq/degrees of freedom of circle (x-y) fit',
         Chisq2       = REAL [0.,*]        : 'Chisq/degrees of freedom of line (s-z) fit',
         ERRmat(5)    = REAL [0.,*]        : 'Diagonal terms of inverse of error
                                              matrix of helix.  1/SQRT(ERRmat(i)) is the
                                              error on the ith parameter',
         PadrowIndex  = INTE [15,2097152]  : 'Bit pattern corresponding to padrows hit,\
                                              positions 1-21 refer to rows 1-21',
         OFfset       = INTE [0,*]         : 'Offset for first coord in bank TATC',
         NoofCoords   = INTE [4,21]        : 'Number of coords in arc',
         ArcNumber    = INTE [*,*]         : 'Number of the arc of the track.  The first
                                              arc is the TGFT segment.  Each arc is a half turn.
                                              The sign is positive if the particle is going radially
                                              outward in this arc; negative otherwise.',
         TrackNumber  = INTE [1,*]         : 'TCAL (not TGFT!) track to which this
                                              arc is associated');
 
 TATC
      :      'Tpc Arc To Coordinates relation\
              Number of words/coord\
              Number of used coordinates'
           STATIC
      = (CoordRef     = INTE [0,*]        : 'Row number of coordinate in TPCO bank')
      ;
 
 TTCC  :     'Tpc Track Candidate Coordinate list.
              NR=1:  original output from TFCAND.
              NR=0:  after removing coordinates from overlapping
              tracks and insertion of twin coordinates.\         
              Number of words per coordinate \
              Number of pad coordinates associated with track candidates'
           STATIC
 
      = (Itpco        = INTE [1,10000]   : 'coordinate number in TPCO')
      ;
 
 TGTL  :    'Tpc Geometrical Track point List.
             NR=1: tracks found during TPC reconstruction.
             NR=2: tracks found by extrapolating from ITC back
             into the TPC.
             Rows for NR=1 correspond with rows in TGFT bank.\
             Number of words per track \
             Number of Tpc geometry tracks '
           STATIC
      = (IOff      = INTE [0,10000]   : ' offset in TGCL',
         N1arc     = INTE [3,21]      : ' numb. coord. in first arc',
         Nrest     = INTE [0,1000]    : ' numb. coord. in following spirals')
      ;
  
 TGCL  :    'Tpc Geometrical track Coordinate List. NR=1,2.
             Use TGTL with same NR to index into this bank.\
             Number of words per coordinate \
             Number of coordinates associated with tracks '
           STATIC
      = (ITpco     = INTE [1,10000]  : 'coordinate number in TPCO')
      ;  
  
 THRP  :     'Tpd tHReshold Pointers (NR = sector number) \
              Number of words per padrow \
              Number of padrows'
           STATIC               
 
      = (PoinTer  = INTE [0,*]         : 'Offset of first entry in
                                          list of bad pad channels 
                                          for this sector padrow in
                                          the bank THPL',
         NumBer   = INTE [0,*]         : 'Number of bad pad channels
                                            for this sector padrow')
      ;
 
 THPL  :     'Tpd tHreshold bad Pad List (NR=0); entries are
              sorted first by sector slot, then by padrow, 
              then by pad number. \
              Number of words per bad pad \
              Number of bad pads'
           STATIC
      
      = (PadNumber = INTE [1,200]      : 'Number of bad pad in TPC padrow',
         THreshold = INTE [0,255]      : 'Abnormal TPD threshold')
      ;
 
 TCPL :    'Tpc workspace for cluster finding.
             Used in the routine TISLND.  NR=0.'
           STATIC
       = (WorkSpace     = INTE [*,*]        : 'Workspace for cluster finding')
         ;
 
 TVVD :      'Tpc drift velocity measured in VDET (NR=run number).\
              Number of columns \
              Number of rows=1 '
           STATIC
 
      = (VZ           = REAL         : 'TPC drift velocity in z',
         ERror        = REAL [0.0,*] : 'Error on drift velocity',
         EXpected     = INTE [0,*]   : 'Expected no. of VDET coords',
         NUmber       = INTE [0,*]   : 'Observed no. of VDET coords',
         Chi2         = REAL [0.0,*] : 'Chi2 of drift velocity fit',
         Clearance    = REAL [0.0,*] : 'No. of sigma from window edge')
      ;

 TPLS
     :    'Tpc track Pad Energy LoSs (NR=0) \
           Number of words per track \
           Number of tracks with pad dE/dx'
        STATIC
       = (NumberseGments= INTE [1,10]      : 'Number of segments in TPXS',
          SegmentOffset = INTE [0,*]       : 'Offset to first segment in TPXS',
          TrackNumber   = INTE [0,*]       : 'Index of track in TGFT')
         ;
 
 TPXS
     :    'Tpc Pad de/dX Segment (NR=0)
           One track segment per sector crossed\
           Number of words per segment \
           Number of segments'
        STATIC
       = (SegmentId     = INTE [1,*]       : 'Sector slot number',
          TruncatedMean = REAL [0.000,*]   : 'Truncated mean of dE/dx measurements',
          TrackLength   = REAL [0.0,500.0] : 'Useful length of track for dE/dx',
          NumberSamples = REAL [0.2,21.0]  : 'Number of samples used for dE/dx',
          AverageDrift  = REAL [0.0,220.0] : 'Average drift length of samples',
          TrackNumber   = INTE [1,*]       : 'Pointer to track entry in TGFT')
       ;
 
 TPDX
     :    'Tpc Pad de/dX Samples  (NR = track).
           One entry for each pad sample on a track\
           Number of words per pad samples \
           Number of associated pad samples'
        STATIC
      = (INdex          = INTE [101001,2136150]: '100000*padrow number +
                                                  1000*sector number + pad number',
         NPads          = INTE [1,5]           : 'Number of pads in coordinate',
         NWires         = INTE [1,20]          : 'Number of wires correlated
                                                  with coordinate',
         TotalCharge    = REAL [0.,*]          : 'Charge in sample',
         SampleLength   = REAL [0.20,*]        : 'length of dE/dx sample in cm',
         RawZposition   = REAL [0.,220.]       : 'z position in sector frame (cm)',
         cosineCrossAng = REAL [-1.,1.]        : 'Cosine of pad crossing angle',
         DeltaRphi      = REAL [0.,7.]         : 'Distance from nearest pad center in cm')
      ;
 
 TWZZ
     :    'Tpc Wire Z coordinates (NR=0) 
           One row per pad coordinate
           Rows in this bank are indexed by
           a field in the Origin Flag word
           of TPCO and the Twin Flag word
           of PCOI\
           Number of words per coordinate \
           Number of coordinates'
        STATIC
       = (NWires    = INTE [0,32]: 'Number of wires used in coordinate',
          Chi2      = REAL [0.,*]: 'Chi^2 of straight line fit',
          DeltaZ    = REAL [*,*] : 'Sector frame Z distance (wire-pad) in cm',
          SigmaZ    = REAL [0.,*]: 'Wire Z coordinate error squared',
          PUll      = REAL [*,*] : 'Number of sigmas wire coordinate is from
                                    pad')
       ;
 
 TRIK
      :      'TRack Extrapolation from Kalman filter \
              Number of words/track\
              Number of tracks in event'
           STATIC

      = (X3(3)        = REAL [-300.,300.]  : 'X(3) coordinate (cm, in the MRS)\
                                              at ecal entry (extrapolated)',
         P3(3)        = REAL [-1.,1.]      : 'direction cosines of momentum',
         PTotal       = REAL [0,*]         : 'Momentum',
         CHarge       = REAL -1.|0.|+1.    : 'Charge',
         SigmaRphi    = REAL [0,*]         : 'Uncertainty on extrapolation.\
                                              May be zero if filter failed.',
         SigmaZ       = REAL [0,*]         : 'Uncertainty on extrapolation. \
                                              May be zero if filter failed.')
      ;

 TWHT
      :      'TPC Wire HiTs for z reconstruction
              (NR = TGFT track number)\
              Number of words/hit\
              Number of hits on track'
           STATIC

      = (
         SectorNum    = INTE [1,36] : 'Sector number of hit',
         XWire        = REAL [0.,200.] : 'X coordinate in local frame',
         ZWire        = REAL [0.,200.] : 'Z coordinate in local frame',
         LWire        = INTE [1,*] :  'Length of wire pulse in buckets',
         QWire        = INTE [1,*] :  'Charge of wire pulse')
      ;

 TWZA
     :    'Tpc Wire Z Auxiliary data (NR=TWZZ row) 
           One row per pad subpulse associated with
           wire coordinate.\
           Number of words per subpulse \
           Number of pads in TPCO coord'
        STATIC
       = (DeltaT    = REAL [*,*] : 'Wire-Pad difference (buckets)',

          Moment1   = REAL [*,*] : 'First moment of pad pulse with
                                    respect to average of leading and
                                    trailing edges (buckets)',
          TanLambda = REAL [*,*] : 'Dip tangent angle of track')
       ;


 END ESET
 
 END SUBSCHEMA

 
SUBSCHEMA TpcPOTBanks
 : 'Description of TPC production output banks'
 
 AUTHOR   'D. Schlatter','R. Johnson','D. Cinabro'
 REVIEWER 'D. Casper'
 VERSION  '2.0'
 DATE     '07/11/95'
 
 DEFINE ESET
 
 PT2X
      :       'Production output Tpc track dE/dX for overlapping tracks (NR=0)\
               Number of words per track pair segment\
               Number of track pair segments'
           STATIC
 
       = (Slot         = INTE [1,36]          : 'Sector slot number for track segment',
          TruncMean    = INTE [1,65000]       : 'Truncated mean in units of minimum ionizing/1000',
          UsefulLength = INTE [1,255]         : 'Useful length of track in units of 0.5cm',
          NumSamples   = INTE [1,255]         : 'Number of useable samples on track, after truncation',
          AveDrift     = INTE [1,255]         : 'Average drift length of track in units of 1cm',
          Tracknumber1 = INTE [1,255]         : 'First track number in the FRFT or TGFT bank',
          Tracknumber2 = INTE [1,255]         : 'Second track number in the FRFT or TGFT bank')
        ;
 
  PCOI 
       :     'Supplemental TPC coordinate information (NR=0).
              This banks is parallel to the PTNC bank but may contain only
              entries for used coordinates.  Note that PTNC is
              not necessarily parallel to TPCO if TPCO has been sorted by
              row number (sort option in the routine PTPCOJ).\
              Number of words/coordinate\
              Number of coordinates'
           STATIC
        = (NumPads     = INTE [1,50]       : 'Number of pads in the subcluster',
           PulseHeight = INTE [1,2000]     : 'Total pulse height in units of 10 ADC counts',
           TwinFlag    = INTE [0,1]        : '1= coordinate calculated from overlapping signals\
                                              0= isolated coordinate',
           NumHalf     = INTE [0,2]        : 'Number of halfpads in the subcluster',
           PulseLength = INTE [1,50]       : 'Average of the number of time buckets in the subcluster')
         ;
 
 PTMA 
      :      'Production output Tpc track to Monte carlo Association.
              Rows correspond to rows in TGFT or FRFT bank \
              Number of words per track\
              Number of TPC tracks '
           STATIC 
      = (NumberMc     = INTE [0,4]         : 'Number of MC tracks associated',
         NumAssoc     = INTE [0,21]        : 'Number of coordinates associated with a MC track')
      ;
 
 PTML
      :      'Production output Tpc association Monte carlo track List.
              Use PTMA to index into this bank \
              Number of words/associated MC track \
              Number of associated MC tracks '
           STATIC
      = (McTrack      = INTE [1,*]         : 'MC track number in KINE',
         NumHits      = INTE [1,*]         : 'Number of hits shared with this MC track')
      ;
 
 
 PTST :      'Tpc reconstruction STaTus, per event\
              Number of words per sector\
              Number of sectors'
            STATIC
      = (SectorPresence = INTE [0,1]    : 'Flag = 1 if this sector
                                           was present on input',
         TruncatedWires = INTE [0,1]    : 'Flag = 0 if this sector
                                           was truncated in wires',
         TruncatedPads  = INTE [0,1]    : 'Flag = 0 if this sector
                                           was truncated in pads',
         NumberTrunc    = INTE [0,*]    : 'Number of bytes truncated',
         TpdError       = INTE [0,*]    : 'DAQ error bit mask.  Equals
                                           zero if there is no error'); 
 
 PTNC
      :      'Production output Tpc pad Coordinates (NR=0).  The coordinates
              here are in the TPC sector reference frame and contain no    
              drift or alignment corrections.  This bank replaces the         
              older version named PTCO, which contained corrected
              coordinates.\
              Number of words/coord.\
              Number of coordinates'
           STATIC                                       
      = (SLot         = INTE [1,36]           : 'Sector slot number',
         SectorRow    = INTE [1,12]           : 'Padrow number in sector system',
         RPhisector   = INTE [-32000,32000]   : 'R*Phi in sector system in units of 16 microns',
         ZValue       = INTE [0,65000]        : 'z in sector system in units of 40 microns',
         SigrPhi      = INTE [0,255]          : 'sigma of R*Phi in units of 40 microns',
         SigZ         = INTE [0,255]          : 'sigma of Z in units of 80 microns')
      ;                                                 
 
 
 PTCO
      :      'Production output Tpc pad Coordinates (NR=0).  Note that here
              the coordinates are transformed from the global system to the
              "sector system" by a simple transformation assuming no alignment
              corrections.  NOTE: this bank is obsolete; see instead PTNC. (DROP)\
              Number of words/coord.\
              Number of coordinates'
           STATIC                                       
      = (SLot         = INTE [1,36]           : 'Sector slot number',
         SectorRow    = INTE [1,12]           : 'Padrow number in sector system',
         PSector      = INTE [-32000,32000]   : 'Phi in sector system in units of 10 micro-radians',
         ZValue       = INTE [0,65000]        : 'z in sector system in units of 40 microns',
         SigrPhi      = INTE [0,255]          : 'sigma of R*Phi in units of 40 microns',
         SigZ         = INTE [0,255]          : 'sigma of Z in units of 80 microns',
         DelR         = INTE [-127,127]       : 'Deviation in radius from ideal padrow radius in units
                                                 of 20 microns')
      ;                                                 
 
 
 PTBC
      :      'Production output Tpc Bad Coordinates (NR=0)\
              Number of words/coord.\
              Number of coordinates'
           STATIC
 
      = (SLot         = INTE [1,36]           : 'Sector slot number',
         SectorRow    = INTE [1,12]           : 'Padrow number in sector system',     
         PSector      = INTE [-32000,32000]   : 'Phi in sector system in units of 10 micro-radians',
         ZValue       = INTE [0,65000]        : 'z in sector system in units of 40 microns',
         NumPads      = INTE [1,150]          : 'number of pads in the cluster',
         NumBuckets   = INTE [1,255]          : 'number of buckets in the cluster')
       ;
        
 
 PTEX
      :       'Production output Tpc track dE/dX  (NR=0)\
               Number of words per track segment\
               Number of track segments'
           STATIC
 
       = (Slot         = INTE [1,36]          : 'Sector slot number for track segment',
          TruncMean    = INTE [1,65000]       : 'Truncated mean in units of minimum ionizing/1000',
          UsefulLength = INTE [1,255]         : 'Useful length of track in units of 0.5cm',
          NumSamples   = INTE [1,255]         : 'Number of useable samples on track, after truncation',
          AveDrift     = INTE [1,255]         : 'Average drift length of track in units of 1cm',
          TrackNumber  = INTE [1,255]         : 'Track number in the FRFT or TGFT bank',
          SatFlag      = INTE [0,1]           : '1= >40% saturated hits.\
                                                 0= <40% saturated hits')
        ;
 
 
PTUN
     :         'Production output Tpc UNits for packing data (NR=0)\
                Number of columns\
                Number of rows'
 
        = (ValRng(2)   = INTE [1,*]        : 'Validity range',
           RSector     = REAL [0.,*]       : 'Units for radius in PTCO',
           PhiSector   = REAL [0.,*]       : 'Units for phi in bank PTCO',
           ZSector     = REAL [0.,*]       : 'Units for z in bank PTNC',
           SigRphi     = REAL [0.,*]       : 'Units for sigma(r*phi) in bank PTCO',
           SigZ        = REAL [0.,*]       : 'Units for sigma(z) in bank PTCO',
           PhiBad      = REAL [0.,*]       : 'Units for phi in bank PTBC',
           ZBad        = REAL [0.,*]       : 'Units for z in bank PTBC',
           TruncMean   = REAL [0.,*]       : 'Units for truncated mean in PTEX',
           TrackLength = REAL [0.,*]       : 'Units for track length in PTEX',
           AvgDrift    = REAL [0.,*]       : 'Units for average drift in PTEX',
           RPhi        = REAL [0.,*]       : 'Units for r*phi in bank PTNC')
         ;
 
PTPX
      :       'Production output Tpc track pad dE/dX  (NR=0)\
               Number of words per track segment\
               Number of track segments'
           STATIC
 
       = (Slot         = INTE [1,36]          : 'Sector slot number for track segment',
          TruncMean    = INTE [1,65000]       : 'Truncated mean in units of minimum ionizing/1000',
          UsefulLength = INTE [1,255]         : 'Useful length of track in units of 0.5cm',
          NumSamples   = INTE [1,210]         : 'Number of useable samples on track, after truncation',
          AveDrift     = INTE [1,255]         : 'Average drift length of track in units of 1cm',
          TrackNumber  = INTE [1,255]         : 'Track number in the FRFT or TGFT bank')
        ;
 

 END ESET
 
 END SUBSCHEMA
