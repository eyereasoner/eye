import Std
open Std

--──────────────────────────
--  Basic domain definitions
--──────────────────────────

inductive City where
  | gent | brugge | kortrijk | oostende
  deriving Repr, DecidableEq, BEq

instance : ToString City where
  toString
    | .gent     => "gent"
    | .brugge   => "brugge"
    | .kortrijk => "kortrijk"
    | .oostende => "oostende"

structure Step where
  mp       : String
  origin   : City
  dest     : City
  action   : String
  duration : Float
  cost     : Float
  belief   : Float
  comfort  : Float
  deriving Repr

def descriptions : List Step :=
[ { mp := "map_be", origin := .gent,     dest := .brugge,
    action := "drive_gent_brugge",
    duration := 1500.0, cost := 0.006, belief := 0.96, comfort := 0.99 },
  { mp := "map_be", origin := .gent,     dest := .kortrijk,
    action := "drive_gent_kortrijk",
    duration := 1600.0, cost := 0.007, belief := 0.96, comfort := 0.99 },
  { mp := "map_be", origin := .kortrijk, dest := .brugge,
    action := "drive_kortrijk_brugge",
    duration := 1600.0, cost := 0.007, belief := 0.96, comfort := 0.99 },
  { mp := "map_be", origin := .brugge,   dest := .oostende,
    action := "drive_brugge_oostende",
    duration :=  900.0, cost := 0.004, belief := 0.98, comfort := 1.00 } ]

structure Limits where
  maxDuration   : Float
  maxCost       : Float
  minBelief     : Float
  minComfort    : Float
  maxStagecount : Nat
  deriving Repr

structure Solution where
  path     : List String
  duration : Float
  cost     : Float
  belief   : Float
  comfort  : Float
  deriving Repr

partial def stagecount : List String → Nat
| []            => 1
| [_]           => 1
| m₁ :: m₂ :: t =>
  if m₁ = m₂ then
    stagecount (m₂ :: t)
  else
    stagecount (m₂ :: t) + 1

partial def findpaths
    (mapsSoFar : List String)
    (current   : City)
    (goal      : City)
    (pathSoFar : List String)
    (dur cost belief comfort : Float)
    (lim       : Limits)
    : List Solution := by
  if current == goal then
    exact [⟨pathSoFar, dur, cost, belief, comfort⟩]
  else
    exact descriptions.foldl (init := []) fun acc step =>
      if step.origin == current then
        let maps' := mapsSoFar ++ [step.mp]
        let admissible :=
          (decide (stagecount maps' ≤ lim.maxStagecount)) &&
          (decide (dur     + step.duration ≤ lim.maxDuration)) &&
          (decide (cost    + step.cost     ≤ lim.maxCost))     &&
          (decide (belief  * step.belief   ≥ lim.minBelief))   &&
          (decide (comfort * step.comfort  ≥ lim.minComfort))
        if admissible then
          let deeper :=
            findpaths maps' step.dest goal
              (pathSoFar ++ [step.action])
              (dur + step.duration)
              (cost + step.cost)
              (belief * step.belief)
              (comfort * step.comfort)
              lim
          acc ++ deeper
        else
          acc
      else
        acc

def findpath (start goal : City) (lim : Limits) : List Solution :=
  findpaths [] start goal [] 0.0 0.0 1.0 1.0 lim

def exampleLimits : Limits :=
{ maxDuration   := 5000.0
, maxCost       :=    5.0
, minBelief     :=    0.2
, minComfort    :=    0.4
, maxStagecount :=    1 }

def main : IO Unit := do
  let sols := findpath .gent .oostende exampleLimits
  IO.println (repr sols)
