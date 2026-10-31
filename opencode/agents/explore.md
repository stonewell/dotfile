---
description: 'LOCAL: read-only repository search, code navigation, and locating relevant
  files.'
mode: subagent
model: relay-gateway/agy/gemini-3.8-flash-medium
permissions:
- action: edit
  resource: '*'
  effect: deny
- action: bash
  resource: '*'
  effect: deny
- action: shell
  resource: '*'
  effect: deny
- action: subagent
  resource: '*'
  effect: deny
---

