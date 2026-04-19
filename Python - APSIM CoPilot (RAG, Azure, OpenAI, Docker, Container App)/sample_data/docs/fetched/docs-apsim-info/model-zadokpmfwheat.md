# ZadokPMFWheat documentation

Source URL: https://docs.apsim.info/model/ZadokPMFWheat
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:54:38.047275+00:00

1 ZadokPMFWheat
This model calculates a Zadok growth stage value based upon the current phenological growth stage within the model.
The model uses information regarding germination, emergence, leaf appearance and tiller appearance for early growth stages (Zadok stages 0 to 30).
The model then uses simulated phenological growth stages for Zadok stages 30 to 100.
1.1 List of growth phases
Growth Phase
Descriptipon
Germinating
ZadokStage = 5 x FractionThroughPhase
Emerging
ZadokStage = 5 + 5 x FractionThroughPhase
Vegetative
ZadokStage = 10 + Structure.LeafTipsAppeared
Reproductive
ZadokStage is interpolated from values of stage number using the following table
1.2 List of growth stages
Growth Stage
Stage Name
ZadokStage
5
Pseudostem
30
5.99
Third node detectable
34
6
Flag leaf ligule just visible
39
7
Heading (hEar half emerged)
55
8
Flowering (Anthesis half-way)
65
9
Kernel water ripe
71
10
Hard dough
87
11
Ripening
90
