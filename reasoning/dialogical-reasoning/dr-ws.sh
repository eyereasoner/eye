#!/bin/bash
websocketd --port 8000 swipl -x /opt/eye/lib/eye.pvm -- --quiet --nope https://w3c.github.io/N3/files/state%20transitions/socrates/socrates.n3 --multi-query
