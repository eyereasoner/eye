# Answer • Reason • Check — *ARC*

ARC is a framework that transforms a combination of data, logic, and a question into a self-contained
program that not only gives an answer but also explains its reasoning and verifies its correctness.
Built on the [P3 method](https://eyereasoner.github.io/eye/arc/p3.html), ARC embodies a “triad of trust”
— Answer, Reason, Check — ensuring that every result is transparent, auditable, and reproducible.

## Examples and Test Cases

<!-- Inline SVG icon sprite (safe, self-contained) -->
<svg aria-hidden="true" focusable="false" width="0" height="0" style="position:absolute;left:-9999px;overflow:hidden">
  <defs>
    <!-- Generic math badge -->
    <symbol id="ico-math" viewBox="0 0 24 24">
      <rect x="2" y="2" width="20" height="20" rx="4" fill="#f2f2f2" stroke="#999"/>
      <path d="M6 8h6M9 5v6M14.5 6.5l3 3M17.5 6.5l-3 3M6 16h12M6 19h12" stroke="#333" stroke-width="1.5" fill="none" stroke-linecap="round"/>
    </symbol>

    <!-- Geometry (circles/triangles) -->
    <symbol id="ico-geom" viewBox="0 0 24 24">
      <rect x="2" y="2" width="20" height="20" rx="4" fill="#eef7ff" stroke="#7aa7d7"/>
      <circle cx="9" cy="9" r="4" fill="none" stroke="#2b6cb0"/>
      <path d="M6 18l6-8 6 8z" fill="none" stroke="#2b6cb0"/>
    </symbol>

    <!-- Health/clinical -->
    <symbol id="ico-health" viewBox="0 0 24 24">
      <rect x="2" y="2" width="20" height="20" rx="4" fill="#fff5f5" stroke="#f5a6a6"/>
      <path d="M6 12h4l1.2-3 1.6 6 1.2-3H18" stroke="#c53030" fill="none" stroke-linecap="round"/>
      <path d="M12 6v3M10.5 7.5h3" stroke="#c53030" stroke-linecap="round"/>
    </symbol>

    <!-- Graphs/networks -->
    <symbol id="ico-graph" viewBox="0 0 24 24">
      <rect x="2" y="2" width="20" height="20" rx="4" fill="#f4f9f4" stroke="#9ad1a5"/>
      <circle cx="7" cy="8" r="2" fill="#2f855a"/>
      <circle cx="16" cy="7" r="2" fill="#2f855a"/>
      <circle cx="10" cy="16" r="2" fill="#2f855a"/>
      <circle cx="18" cy="16" r="2" fill="#2f855a"/>
      <path d="M9 9l5-1M8 10l2 5M12 16h4" stroke="#2f855a" fill="none"/>
    </symbol>

    <!-- Logic / reasoning -->
    <symbol id="ico-logic" viewBox="0 0 24 24">
      <rect x="2" y="2" width="20" height="20" rx="4" fill="#fbf7ff" stroke="#c2a9f2"/>
      <path d="M6 8h6l2 3-2 3H6z" fill="none" stroke="#6b46c1"/>
      <circle cx="17" cy="12" r="2.5" fill="none" stroke="#6b46c1"/>
      <path d="M12 12h2.5" stroke="#6b46c1"/>
    </symbol>

    <!-- Puzzles / grids -->
    <symbol id="ico-puzzle" viewBox="0 0 24 24">
      <rect x="2" y="2" width="20" height="20" rx="4" fill="#fffbea" stroke="#f2d48b"/>
      <path d="M8 4v16M16 4v16M4 8h16M4 16h16" stroke="#b7791f"/>
      <circle cx="8" cy="8" r="1" fill="#b7791f"/>
      <rect x="12.5" y="12.5" width="3" height="3" fill="#b7791f"/>
    </symbol>

    <!-- Number theory / primes -->
    <symbol id="ico-number" viewBox="0 0 24 24">
      <rect x="2" y="2" width="20" height="20" rx="4" fill="#eefaff" stroke="#9ad0e6"/>
      <path d="M7 17c3-8 7-8 10 0" stroke="#1a73e8" fill="none"/>
      <circle cx="9" cy="9" r="1" fill="#1a73e8"/>
      <circle cx="13" cy="9" r="1" fill="#1a73e8"/>
      <circle cx="17" cy="9" r="1" fill="#1a73e8"/>
    </symbol>

    <!-- Date/time -->
    <symbol id="ico-date" viewBox="0 0 24 24">
      <rect x="3" y="5" width="18" height="16" rx="3" fill="#f0f7ff" stroke="#9bbfee"/>
      <path d="M7 5V3M17 5V3M3 9h18" stroke="#3b82f6"/>
      <rect x="7" y="12" width="3" height="3" fill="#3b82f6"/>
    </symbol>

    <!-- Control/engineering -->
    <symbol id="ico-control" viewBox="0 0 24 24">
      <rect x="2" y="2" width="20" height="20" rx="4" fill="#f2fffb" stroke="#8dd9c3"/>
      <path d="M6 15c3 0 3-6 6-6s3 6 6 6" stroke="#0f766e" fill="none"/>
      <circle cx="12" cy="9" r="1.5" fill="#0f766e"/>
    </symbol>

    <!-- Code / automata -->
    <symbol id="ico-code" viewBox="0 0 24 24">
      <rect x="2" y="2" width="20" height="20" rx="4" fill="#f6f6ff" stroke="#b3b3ff"/>
      <path d="M9 8l-3 4 3 4M15 8l3 4-3 4" stroke="#4444dd" fill="none" stroke-linecap="round"/>
    </symbol>
  </defs>
</svg>

<style>
  .case-grid {display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:12px;margin:8px 0 2rem}
  .case {display:flex;align-items:flex-start;gap:10px;padding:10px;border:1px solid #e7e7e7;border-radius:10px;background:#fafafa}
  .case .ico {width:26px;height:26px;flex:0 0 26px}
  .case .body {line-height:1.35}
  .case .title {font-weight:600;margin:0 0 2px}
  .case .desc {margin:0;color:#444}
</style>

<div class="case-grid">

<div class="case">
  <svg class="ico"><use href="#ico-math"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/ackermann.html">A₂ (Ackermann via hyper-operations)</a></div>
    <p class="desc">Compute A₂ with exact hyper-ops; print small, expand huge safely.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/apollonian_gasket.html">Apollonian gasket</a></div>
    <p class="desc">Exact tangent-circle packing via Descartes’ theorem + complex centers.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-health"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/auroracare.html">AuroraCare</a></div>
    <p class="desc">Purpose-based medical data exchange (ODRL/DPV) with Answer/Reason/Check.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-graph"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/bike_trip.html">Bike Trip Planning</a></div>
    <p class="desc">Route priorities from hazards, preferences, and declarative JSON rules.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-health"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/bmi.html">BMI</a></div>
    <p class="desc">Compute BMI categories with explainable thresholds and sanity checks.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-control"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/building_performance.html">Building Performance</a></div>
    <p class="desc">Reason about energy/comfort metrics and verify rule-based outcomes.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-health"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/clinical_care_planning.html">Clinical Care Planning</a></div>
    <p class="desc">Derive care plans from observations, guidelines, and policy constraints.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-number"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/collatz.html">Collatz (3n+1)</a></div>
    <p class="desc">Generate trajectories and check invariants for the Collatz map.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-math"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/combinatorics.html">Combinatorics</a></div>
    <p class="desc">Count, choose, and permute with proofs of identities where feasible.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-logic"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/complex_identities.html">Complex identities — explanatory proofs</a></div>
    <p class="desc">Symbolic steps for complex-number equalities with auditable reasoning.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-control"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/control_system.html">Control System</a></div>
    <p class="desc">Model simple feedback loops and verify stability/response conditions.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-number"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/cryptarithm.html">Cryptarithm</a></div>
    <p class="desc">Solve letter-to-digit puzzles with constraint checks on carry/uniqueness.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/delfour.html">Delfour</a></div>
    <p class="desc">Geometric reasoning demo (curvatures/packing) inspired by Delfour’s work.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/descartes_circles.html">Descartes’ circle theorem</a></div>
    <p class="desc">Compute the fourth tangent circle from three using curvature relations.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-date"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/easter.html">Easter (Computus)</a></div>
    <p class="desc">Derive Easter dates from calendrical rules with verifiable steps.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-graph"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/eco_route.html">Eco-Route</a></div>
    <p class="desc">Pick lower-emission routes by fusing traffic, grade, and policy goals.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/euler_characteristic.html">Euler’s characteristic</a></div>
    <p class="desc">Compute χ = V−E+F for meshes; sanity-check against topology rules.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-logic"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/family_logic.html">Family logic</a></div>
    <p class="desc">Infer kinship from base relations (parent, spouse) with consistency checks.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/fibonacci_spiral.html">Fibonacci golden spiral</a></div>
    <p class="desc">Draw the spiral from Fibonacci rectangles and verify ratios.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-number"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/fibonacci_fast_doubling.html">Fibonacci via Fast Doubling</a></div>
    <p class="desc">Compute big Fₙ with fast-doubling recurrences and proof-style checks.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/ford_circles.html">Ford circles</a></div>
    <p class="desc">Place circles at rationals; verify tangency and Farey-sequence links.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-health"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/gps_clinical_bench.html">GPS Clinical Bench</a></div>
    <p class="desc">Benchmark clinical decisions with transparent rules and audit trails.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-graph"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/graph_french_cities.html">Graph — French cities</a></div>
    <p class="desc">Shortest paths and connectivity over a city graph with proofs.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-health"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/grass_seed.html">Grass seed — molecular germination</a></div>
    <p class="desc">Model germination states and transitions with rule checks.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-math"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/group_theory.html">Group Theory</a></div>
    <p class="desc">Verify closure, identity, inverses, and associativity on examples.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-health"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/health_info_processing.html">Health Information Processing</a></div>
    <p class="desc">Transform clinical payloads with typed rules and validation.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-logic"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/insight_economy.html">Infinite Game of Insight Economy</a></div>
    <p class="desc">Toy economy where insight generation/consumption follows explicit rules.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-puzzle"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/kakuro.html">Kakuro (Cross Sums)</a></div>
    <p class="desc">Fill grid sums with unique digits using constraint propagation.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-puzzle"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/kenken.html">KenKen</a></div>
    <p class="desc">Latin-square + cage arithmetic solved with explainable deductions.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-graph"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/lee.html">Lee</a></div>
    <p class="desc">Maze routing with Lee’s algorithm; trace optimal wavefront paths.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-code"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/library_path.html">Library &amp; Path</a></div>
    <p class="desc">Resolve resource links and dependencies with path-finding checks.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-code"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/lldm.html">LLDM</a></div>
    <p class="desc">Logical lifecycle/data management with policy-aware transformations.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-logic"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/math_what_why.html">Mathematics — the WHAT &amp; the WHY</a></div>
    <p class="desc">Pair results with reasons: each computation carries its proof sketch.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-math"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/matrix_basics.html">Matrix basics</a></div>
    <p class="desc">Do matrix ops (add/mul/inv) with dimension and property checks.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-puzzle"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/n_queens.html">N-Queens</a></div>
    <p class="desc">Place N queens without attacks; verify constraints per row/diag.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-puzzle"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/nonogram.html">Nonogram (Picross)</a></div>
    <p class="desc">Fill grid cells to match run hints using logical deductions.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/pentagon_pentagram.html">Pentagon &amp; pentagram — golden ratio</a></div>
    <p class="desc">Construct φ-relations in pentagons and star polygons with proofs.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-number"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/pi_chudnovsky.html">π (Chudnovsky)</a></div>
    <p class="desc">High-precision π via Chudnovsky series with error-bound checks.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/picks_theorem.html">Pick’s Theorem</a></div>
    <p class="desc">Area = I + B/2 − 1 for lattice polygons; verify interior/boundary counts.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-number"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/polynomial_roots_dk.html">Polynomial roots (Durand–Kerner)</a></div>
    <p class="desc">Find all roots simultaneously; prove convergence on typical cases.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-number"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/primes.html">Primes</a></div>
    <p class="desc">Generate/test primes; log certs (e.g., trial factors or proofs) as checks.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/pythagorean.html">Pythagorean Theorem</a></div>
    <p class="desc">Compute legs/hypotenuse and confirm with algebraic or area proofs.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-code"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/rest_path.html">REST-path</a></div>
    <p class="desc">Explain link-following over REST resources; verify pre/post conditions.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-math"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/roots_of_unity.html">Roots of Unity</a></div>
    <p class="desc">Place complex nth roots on the unit circle; check sum/products.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-puzzle"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/skyscrapers.html">Skyscrapers</a></div>
    <p class="desc">Deduce building heights from sightlines with constraint logic.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-logic"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/socrates.html">Socrates</a></div>
    <p class="desc">Classic syllogisms (All men are mortal…) with explicit inference traces.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-geom"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/square_17_triangles.html">Square tiled by 17 right triangles</a></div>
    <p class="desc">Dissect a square into 17 right triangles; verify tiling constraints.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-puzzle"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/sudoku.html">Sudoku</a></div>
    <p class="desc">Explain each step of solving a 9×9 with row/col/box checks.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-code"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/turing_machine.html">Turing Machine</a></div>
    <p class="desc">Run tapes with explicit transitions; verify halting and tape contents.</p>
  </div>
</div>

<div class="case">
  <svg class="ico"><use href="#ico-control"/></svg>
  <div class="body">
    <div class="title"><a href="https://eyereasoner.github.io/eye/arc/etc/wind_turbine_maintenance.html">Wind-Turbine Maintenance</a></div>
    <p class="desc">Plan maintenance from telemetry and policies with auditable outcomes.</p>
  </div>
</div>

</div><!-- /.case-grid -->

