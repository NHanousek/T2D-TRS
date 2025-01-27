# T2D-TRS
 A clean version of the tidal range scheme model for Telemac, available to those interested. Originally developed by [Bin Guo](https://www.researchgate.net/profile/Bin-Guo-32), under the supervision and guidance of [Reza Ahmadian](https://www.researchgate.net/profile/Reza-Ahmadian-2) and [Roger Falconer](https://www.researchgate.net/profile/Roger-Falconer).

## Publications

- N. Hanousek; *Numerical modelling of tidal energy devices and structures as part of net zero*, Ph.D. Thesis, Cardiff University (2023) [URI](https://orca.cardiff.ac.uk/id/eprint/167131)
- E. Roome, P. Robins, R. Ahmadian, M. Austen, N. Hanousek, B. Gup, S. Neill; *Assessing hydrodynamic impacts of tidal range energy impoundments in UK coastal waters*, Renewable Energy (2024) [DOI](https://doi.org/10.1016/j.renene.2024.121601)

##  Example models:

- Simple lagoon
  - Basic single scheme model runnning in fixed operation
- Simple twin lagoons
  - Simple model containing twin schemes, each runnning in fixed operation
- West Somerset Fixed
  - [West Somerset Lagoon](https://tidalengineering.co.uk/west-somerset-lagoon/location-and-scheme-description/) model in fixed operation
- West Somerset Flexible Pumped
  - [West Somerset Lagoon](https://tidalengineering.co.uk/west-somerset-lagoon/location-and-scheme-description/) model in flexible operation

## Guide to Operation

Key input data information

### MODES:

| N | Description | Start condition |
|:-:|------------|-----------|
| 0 | Initial sluice mode | Time = 0|
| 1 | High water holding |  HD >= H_START|
| 2 | Ebb generation |      HD <= H_END|
| 3 | Ebb sluicing |        HD ~= 0.0|
|-1| Ebb pumping |         WL_IN <= PUMP_TARG|
| 4 | Low water holding  |  HD >= H_START|
| 5 | Flood generation |    HD <= H_END|
| 6 | Flood sluicing |      HD ~= 0.0|
|-2 | FLood pumping |      WL_IN <= PUMP_TARG|


### Culvert File

|Key      |Old use                      |New Use|
|---------|-----------------------------|-----------------------------|
|I1      |First culvert point           |Upstream point|
|I2      |Second culvert point          |Downstream point|
|CE1     |Head loss coeff as inlet p1   |Ebb head loss coeff generate/sluice|
|CE2     |Head loss coedd as inlet p2   |Flood head loss coeff generate/sluice|
|CS1     |Head loss coeff outlet p1     |Ebb head loss coeff pumping|
|CS2     |Head loss coeff outlet p2     |Flood head loss coeff pumping|
|LRG     |Width of the culvert          |Width of sluice gate, turbine diameter|
|HAUT1   |Height of construction p1     |Minimum height for safe operation at p1|
|CLP     |Flow control type             |Culvert type: 4 = control, 5 = Turbine, 6 = Sluice, [0,1,2,3] as before|
|LBUS    |Linear head loss in culvert   |Turbine Cd when sluicing|
|Z1      |Level of inlet/outlet at p1   |Turbine draft tube area at p1|
|Z2      |Level of inlet/outlet at p2   |Turbine draft tube area at p2|
|CV      |Loss coeff due to valve       |Weighting for control points|
|C56     |Constant to determ flow type  |Parrallel sluicing for turbine: 0 = FALSE, 1 = TRUE|
|CV5     |Correction for type 5 flow    |Turbine type: 1 = Turb+Pump, 2 = Turb Only, 3 = Pump Only|
|C5      |Correction for type 5 flow    |Number of turbines at node|
|CT      |Loss coeff for trash screen   |Tidal Range Scheme Number|
|HAUT2   |Height of construction p2     |Minimum height for safe operation at p2|
|FRIC    |Manning Strickler coeff       |Friction of the sluice gate sides?|
|LENGTH  |Length of the culvert         |Length of sluice gate|
|CIRC    |Shape: 1=circle, 0=Rect       |Circular or rectangular sluice gate|
|D1      |Angle of pipe-bottom p1       |SAME/Unused|
|D2      |Angle of pipe-bottom p2       |SAME/Unused|
|A1      |Angle of pipe-x_axis p1       |SAME/Unused|
|A2      |Angle of pipe-x_axis p2       |SAME/Unused|
|AA      |1: Auto angles, 2: User       |1: Auto individual, 2: User, 3: Mean-type|

