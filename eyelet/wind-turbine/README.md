# 🌀 Wind Turbine Predictive Maintenance — N3 Logic

This N3 logic example demonstrates how to detect anomalies and trigger predictive maintenance actions for a wind turbine based on sensor input and threshold-based rules.

It uses [Notation3 Logic (N3)](https://en.wikipedia.org/wiki/Notation3) expressed in RDF Turtle syntax, designed to run with reasoners like [EYE](https://github.com/eyereasoner/eye).

---

## 📊 Input Data

We define sensor readings for a turbine:

```turtle
:turbine1 :vibration 0.42 .          # mm/s
:turbine1 :temperature 78 .          # °C
:turbine1 :rpm 1650 .
:turbine1 :gearboxStatus :degraded .
````

And safe operational thresholds:

```turtle
:highVibrationThreshold :value 0.35 .
:highTemperatureThreshold :value 75 .
:criticalRPM :value 1800 .
```

---

## 🔧 Rules

### 🟡 Vibration Anomaly Detection

If the turbine's vibration exceeds the threshold:

```turtle
:turbine1 :vibration ?V .
:highVibrationThreshold :value ?VT .
?V math:greaterThan ?VT .
=>
:turbine1 :hasAnomaly :highVibration .
```

### 🔴 Temperature Anomaly Detection

```turtle
:turbine1 :temperature ?T .
:highTemperatureThreshold :value ?TT .
?T math:greaterThan ?TT .
=>
:turbine1 :hasAnomaly :highTemperature .
```

### 🚨 Combined Anomaly → Urgent Maintenance

```turtle
:turbine1 :hasAnomaly :highVibration .
:turbine1 :hasAnomaly :highTemperature .
=>
:turbine1 :requires :urgentInspection .
```

### ⚙️ Gearbox Degradation → Maintenance

```turtle
:turbine1 :gearboxStatus :degraded .
=>
:turbine1 :requires :gearboxMaintenance .
```

---

## ❓ Queries

### 🔍 Which anomalies are detected?

```turtle
:turbine1 :hasAnomaly ?Anomaly .
```

### 🛠 What actions are required?

```turtle
:turbine1 :requires ?Action .
```

---

## ✅ Example Inference Result

```turtle
:turbine1 :hasAnomaly :highVibration .
:turbine1 :hasAnomaly :highTemperature .
:turbine1 :requires :urgentInspection .
:turbine1 :requires :gearboxMaintenance .
```

---

## 📚 Reference

This model illustrates a simplified version of a predictive maintenance system for wind turbines using rule-based reasoning. It can be extended with time-series trend analysis, condition monitoring, or statistical learning methods.

Inspired by EYE reasoning and Semantic Web rule modeling.

