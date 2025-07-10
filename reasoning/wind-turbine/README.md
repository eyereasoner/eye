# 🔧 Wind Turbine Predictive Maintenance — N3 Logic

This project demonstrates a simple yet powerful use of [Notation3 (N3)](https://www.w3.org/TeamSubmission/n3/) logic for **predictive maintenance of wind turbines**, using a set of threshold-based rules to identify anomalies and recommend maintenance actions.

## 📂 Files

* `wind-turbine.n3` — Main rules and facts (see example below)

## 📘 Description

We model sensor readings from a wind turbine and define rules for detecting:

* High vibration
* High temperature
* Gearbox degradation

The system infers maintenance requirements based on detected anomalies.

## 📥 Input Facts

```n3
:turbine1 :vibration "0.42"^^xsd:decimal.
:turbine1 :temperature "78"^^xsd:decimal.
:turbine1 :rpm "1650"^^xsd:decimal.
:turbine1 :gearboxStatus :degraded.

:highVibrationThreshold :value "0.35"^^xsd:decimal.
:highTemperatureThreshold :value "75"^^xsd:decimal.
:criticalRPM :value "1800"^^xsd:decimal.
```

## 📐 Rules

### High Vibration

```n3
{ ?T :vibration ?V.
  :highVibrationThreshold :value ?VT.
  ?V math:greaterThan ?VT. }
=>
{ ?T :hasAnomaly :highVibration. }.
```

### High Temperature

```n3
{ ?T :temperature ?Temp.
  :highTemperatureThreshold :value ?TT.
  ?Temp math:greaterThan ?TT. }
=>
{ ?T :hasAnomaly :highTemperature. }.
```

### Combined Anomalies → Urgent Inspection

```n3
{ ?T :hasAnomaly :highVibration.
  ?T :hasAnomaly :highTemperature. }
=>
{ ?T :requires :urgentInspection. }.
```

### Gearbox Degradation → Maintenance

```n3
{ ?T :gearboxStatus :degraded. }
=>
{ ?T :requires :gearboxMaintenance. }.
```

## 🔍 Queries

### Anomaly Detection

```n3
{ ?T :hasAnomaly ?Anomaly. } =^ { ?T :hasAnomaly ?Anomaly. }.
```

### Required Actions

```n3
{ ?T :requires ?Action. } =^ { ?T :requires ?Action. }.
```

## ✅ Output (Inferred)

Given the input, the reasoner (e.g. [EYE](https://github.com/eyereasoner/eye)) infers:

```n3
:turbine1 :hasAnomaly :highVibration.
:turbine1 :hasAnomaly :highTemperature.
:turbine1 :requires :urgentInspection.
:turbine1 :requires :gearboxMaintenance.
```

