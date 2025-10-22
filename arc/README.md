# Answer • Reason • Check — *ARC*

ARC is a framework that transforms a combination of data, logic, and a question into a self-contained
program that not only gives an answer but also explains its reasoning and verifies its correctness.
Built on the [P3 method](p3.md), ARC embodies a “triad of trust” — Answer, Reason, Check — ensuring that
every result is transparent, auditable, and reproducible. By uniting generative AI’s creativity with the
rigor of symbolic reasoning, ARC produces explainable and verifiable outputs that can be confidently used
in real-world applications.

## Examples and Test Cases

<style>
  /* lightweight, inline so it works in GitHub Pages */
  .case-grid {display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:12px;margin:8px 0 2rem}
  .case {display:flex;align-items:flex-start;gap:10px;padding:10px;border:1px solid #e7e7e7;border-radius:10px;background:#fafafa}
  .case .ico {font-size:22px;line-height:1.1;width:26px;text-align:center;flex:0 0 26px}
  .case .body {line-height:1.35}
  .case .title {font-weight:600;margin:0 0 2px}
  .case .desc {margin:0;color:#444}
</style>

<div class="case-grid">

<div class="case">
  <div class="ico">🧮</div>
  <div class="body">
    <div class="title"><a href="./etc/ackermann.html">A₂ (Ackermann via hyper-operations)</a></div>
    <p class="desc">Compute A₂ with exact hyper-ops; print small, expand huge safely.</p>
  </div>
</div>

<div class="case">
  <div class="ico">⚪️</div>
  <div class="body">
    <div class="title"><a href="./etc/apollonian_gasket.html">Apollonian gasket</a></div>
    <p class="desc">Exact tangent-circle packing via Descartes’ theorem + complex centers.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🏥</div>
  <div class="body">
    <div class="title"><a href="./etc/auroracare.html">AuroraCare</a></div>
    <p class="desc">Purpose-based medical data exchange (ODRL/DPV) with Answer/Reason/Check.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🚲</div>
  <div class="body">
    <div class="title"><a href="./etc/bike_trip.html">Bike Trip Planning</a></div>
    <p class="desc">Route priorities from hazards, preferences, and declarative JSON rules.</p>
  </div>
</div>

<div class="case">
  <div class="ico">⚖️</div>
  <div class="body">
    <div class="title"><a href="./etc/bmi.html">BMI</a></div>
    <p class="desc">Compute BMI categories with explainable thresholds and sanity checks.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🏗️</div>
  <div class="body">
    <div class="title"><a href="./etc/building_performance.html">Building Performance</a></div>
    <p class="desc">Reason about energy/comfort metrics and verify rule-based outcomes.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🩺</div>
  <div class="body">
    <div class="title"><a href="./etc/clinical_care_planning.html">Clinical Care Planning</a></div>
    <p class="desc">Derive care plans from observations, guidelines, and policy constraints.</p>
  </div>
</div>

<div class="case">
  <div class="ico">↗️</div>
  <div class="body">
    <div class="title"><a href="./etc/collatz.html">Collatz (3n+1)</a></div>
    <p class="desc">Generate trajectories and check invariants for the Collatz map.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🧩</div>
  <div class="body">
    <div class="title"><a href="./etc/combinatorics.html">Combinatorics</a></div>
    <p class="desc">Count, choose, and permute with proofs of identities where feasible.</p>
  </div>
</div>

<div class="case">
  <div class="ico">∴</div>
  <div class="body">
    <div class="title"><a href="./etc/complex_identities.html">Complex identities — explanatory proofs</a></div>
    <p class="desc">Symbolic steps for complex-number equalities with auditable reasoning.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🎛️</div>
  <div class="body">
    <div class="title"><a href="./etc/control_system.html">Control System</a></div>
    <p class="desc">Model simple feedback loops and verify stability/response conditions.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🔢</div>
  <div class="body">
    <div class="title"><a href="./etc/cryptarithm.html">Cryptarithm</a></div>
    <p class="desc">Solve letter-to-digit puzzles with constraint checks on carry/uniqueness.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🧭</div>
  <div class="body">
    <div class="title"><a href="./etc/delfour.html">Delfour</a></div>
    <p class="desc">Geometric reasoning demo (curvatures/packing) inspired by Delfour’s work.</p>
  </div>
</div>

<div class="case">
  <div class="ico">⭕️</div>
  <div class="body">
    <div class="title"><a href="./etc/descartes_circles.html">Descartes’ circle theorem</a></div>
    <p class="desc">Compute the fourth tangent circle from three using curvature relations.</p>
  </div>
</div>

<div class="case">
  <div class="ico">📅</div>
  <div class="body">
    <div class="title"><a href="./etc/easter.html">Easter (Computus)</a></div>
    <p class="desc">Derive Easter dates from calendrical rules with verifiable steps.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🌱</div>
  <div class="body">
    <div class="title"><a href="./etc/eco_route.html">Eco-Route</a></div>
    <p class="desc">Pick lower-emission routes by fusing traffic, grade, and policy goals.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🧷</div>
  <div class="body">
    <div class="title"><a href="./etc/euler_characteristic.html">Euler’s characteristic</a></div>
    <p class="desc">Compute χ = V−E+F for meshes; sanity-check against topology rules.</p>
  </div>
</div>

<div class="case">
  <div class="ico">👪</div>
  <div class="body">
    <div class="title"><a href="./etc/family_logic.html">Family logic</a></div>
    <p class="desc">Infer kinship from base relations (parent, spouse) with consistency checks.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🌀</div>
  <div class="body">
    <div class="title"><a href="./etc/fibonacci_spiral.html">Fibonacci golden spiral</a></div>
    <p class="desc">Draw the spiral from Fibonacci rectangles and verify ratios.</p>
  </div>
</div>

<div class="case">
  <div class="ico">⚡</div>
  <div class="body">
    <div class="title"><a href="./etc/fibonacci_fast_doubling.html">Fibonacci via Fast Doubling</a></div>
    <p class="desc">Compute big Fₙ with fast-doubling recurrences and proof-style checks.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🟣</div>
  <div class="body">
    <div class="title"><a href="./etc/ford_circles.html">Ford circles</a></div>
    <p class="desc">Place circles at rationals; verify tangency and Farey-sequence links.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🧪</div>
  <div class="body">
    <div class="title"><a href="./etc/gps_clinical_bench.html">GPS Clinical Bench</a></div>
    <p class="desc">Benchmark clinical decisions with transparent rules and audit trails.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🗺️</div>
  <div class="body">
    <div class="title"><a href="./etc/graph_french_cities.html">Graph — French cities</a></div>
    <p class="desc">Shortest paths and connectivity over a city graph with proofs.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🌾</div>
  <div class="body">
    <div class="title"><a href="./etc/grass_seed.html">Grass seed — molecular germination</a></div>
    <p class="desc">Model germination states and transitions with rule checks.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🔗</div>
  <div class="body">
    <div class="title"><a href="./etc/group_theory.html">Group Theory</a></div>
    <p class="desc">Verify closure, identity, inverses, and associativity on examples.</p>
  </div>
</div>

<div class="case">
  <div class="ico">📈</div>
  <div class="body">
    <div class="title"><a href="./etc/health_info_processing.html">Health Information Processing</a></div>
    <p class="desc">Transform clinical payloads with typed rules and validation.</p>
  </div>
</div>

<div class="case">
  <div class="ico">♾️</div>
  <div class="body">
    <div class="title"><a href="./etc/insight_economy.html">Infinite Game of Insight Economy</a></div>
    <p class="desc">Toy economy where insight generation/consumption follows explicit rules.</p>
  </div>
</div>

<div class="case">
  <div class="ico">➕</div>
  <div class="body">
    <div class="title"><a href="./etc/kakuro.html">Kakuro (Cross Sums)</a></div>
    <p class="desc">Fill grid sums with unique digits using constraint propagation.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🧮</div>
  <div class="body">
    <div class="title"><a href="./etc/kenken.html">KenKen</a></div>
    <p class="desc">Latin-square + cage arithmetic solved with explainable deductions.</p>
  </div>
</div>

<div class="case">
  <div class="ico">📐</div>
  <div class="body">
    <div class="title"><a href="./etc/lee.html">Lee</a></div>
    <p class="desc">Maze routing with Lee’s algorithm; trace optimal wavefront paths.</p>
  </div>
</div>

<div class="case">
  <div class="ico">📚</div>
  <div class="body">
    <div class="title"><a href="./etc/library_path.html">Library &amp; Path</a></div>
    <p class="desc">Resolve resource links and dependencies with path-finding checks.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🧪</div>
  <div class="body">
    <div class="title"><a href="./etc/lldm.html">LLDM</a></div>
    <p class="desc">Logical lifecycle/data management with policy-aware transformations.</p>
  </div>
</div>

<div class="case">
  <div class="ico">❓</div>
  <div class="body">
    <div class="title"><a href="./etc/math_what_why.html">Mathematics — the WHAT &amp; the WHY</a></div>
    <p class="desc">Pair results with reasons: each computation carries its proof sketch.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🧊</div>
  <div class="body">
    <div class="title"><a href="./etc/matrix_basics.html">Matrix basics</a></div>
    <p class="desc">Do matrix ops (add/mul/inv) with dimension and property checks.</p>
  </div>
</div>

<div class="case">
  <div class="ico">👑</div>
  <div class="body">
    <div class="title"><a href="./etc/n_queens.html">N-Queens</a></div>
    <p class="desc">Place N queens without attacks; verify constraints per row/diag.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🖼️</div>
  <div class="body">
    <div class="title"><a href="./etc/nonogram.html">Nonogram (Picross)</a></div>
    <p class="desc">Fill grid cells to match run hints using logical deductions.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🟨⭐</div>
  <div class="body">
    <div class="title"><a href="./etc/pentagon_pentagram.html">Pentagon &amp; pentagram — golden ratio</a></div>
    <p class="desc">Construct φ-relations in pentagons and star polygons with proofs.</p>
  </div>
</div>

<div class="case">
  <div class="ico">π</div>
  <div class="body">
    <div class="title"><a href="./etc/pi_chudnovsky.html">π (Chudnovsky)</a></div>
    <p class="desc">High-precision π via Chudnovsky series with error-bound checks.</p>
  </div>
</div>

<div class="case">
  <div class="ico">📏</div>
  <div class="body">
    <div class="title"><a href="./etc/picks_theorem.html">Pick’s Theorem</a></div>
    <p class="desc">Area = I + B/2 − 1 for lattice polygons; verify interior/boundary counts.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🌱</div>
  <div class="body">
    <div class="title"><a href="./etc/polynomial_roots_dk.html">Polynomial roots (Durand–Kerner)</a></div>
    <p class="desc">Find all roots simultaneously; prove convergence on typical cases.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🔍</div>
  <div class="body">
    <div class="title"><a href="./etc/primes.html">Primes</a></div>
    <p class="desc">Generate/test primes; log certs (e.g., trial factors or proofs) as checks.</p>
  </div>
</div>

<div class="case">
  <div class="ico">📐</div>
  <div class="body">
    <div class="title"><a href="./etc/pythagorean.html">Pythagorean Theorem</a></div>
    <p class="desc">Compute legs/hypotenuse and confirm with algebraic or area proofs.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🌐</div>
  <div class="body">
    <div class="title"><a href="./etc/rest_path.html">REST-path</a></div>
    <p class="desc">Explain link-following over REST resources; verify pre/post conditions.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🛞</div>
  <div class="body">
    <div class="title"><a href="./etc/roots_of_unity.html">Roots of Unity</a></div>
    <p class="desc">Place complex nth roots on the unit circle; check sum/products.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🏙️</div>
  <div class="body">
    <div class="title"><a href="./etc/skyscrapers.html">Skyscrapers</a></div>
    <p class="desc">Deduce building heights from sightlines with constraint logic.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🗣️</div>
  <div class="body">
    <div class="title"><a href="./etc/socrates.html">Socrates</a></div>
    <p class="desc">Classic syllogisms (All men are mortal…) with explicit inference traces.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🔺</div>
  <div class="body">
    <div class="title"><a href="./etc/square_17_triangles.html">Square tiled by 17 right triangles</a></div>
    <p class="desc">Dissect a square into 17 right triangles; verify tiling constraints.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🧩</div>
  <div class="body">
    <div class="title"><a href="./etc/sudoku.html">Sudoku</a></div>
    <p class="desc">Explain each step of solving a 9×9 with row/col/box checks.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🧠</div>
  <div class="body">
    <div class="title"><a href="./etc/turing_machine.html">Turing Machine</a></div>
    <p class="desc">Run tapes with explicit transitions; verify halting and tape contents.</p>
  </div>
</div>

<div class="case">
  <div class="ico">🌬️</div>
  <div class="body">
    <div class="title"><a href="./etc/wind_turbine_maintenance.html">Wind-Turbine Maintenance</a></div>
    <p class="desc">Plan maintenance from telemetry and policies with auditable outcomes.</p>
  </div>
</div>

</div><!-- /.case-grid -->

