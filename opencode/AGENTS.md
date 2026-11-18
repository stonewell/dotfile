# Global agent instructions

## Relay provider selection (applies to every agent that launches subagents)

Before launching any subagent, probe the relay once per session using the execute tool's fetch on `http://127.0.0.1:8765/v1/models`. Any HTTP response — including 401 or 404 — means the relay is available; a network error such as ECONNREFUSED or a timeout means it is unavailable. Cache the result for the rest of the session and do not probe again before every dispatch.

- Relay available: launch every subagent under its `-relay` name — `pro-relay`, `flash-relay`, `explore-relay`, `tests-relay`, `review-relay`.
- Relay unavailable: launch only the plain names — `pro`, `flash`, `explore`, `tests`, `review`, which run on DeepSeek models. Never route a plain-name task through relay-gateway or any other local endpoint.
- `local` has no relay variant: always keep the plain name `local`, and never route an on-device task to a relay or cloud model.
- If a relay dispatch fails with a model-unavailable or provider error, treat the relay as unavailable for the rest of the session, say so in your next report, and re-dispatch the same task to the plain-name agent with any partial findings instead of redoing finished work.
- Report relay availability only together with the probe result; never claim it without evidence.
- Launch subagents in the background, never sleep, poll, or block waiting for them, and give each one a self-contained brief: paths it owns, acceptance criteria, and validation commands.

## Delegation limits

- Never launch another instance of your own tier, and never exceed two concurrently running child tasks.
- Writers must own disjoint files. Run tests or review only after their prerequisites finish.

## Subagent dispatch integrity (mandatory)

Every `subagent` call must be verifiable from its own tool result. A dispatch is not
"launched" until a `sessionID` comes back.

- **New dispatch**: record the returned `sessionID` verbatim. If the result contains no
  `sessionID` (or it does not start with `ses`), the dispatch did not happen — do not
  describe it to the user as launched, and do not assume work is in progress.
- **Continuation** (passing `sessionID`): the id must come from an actual earlier tool
  result in this conversation. Never reconstruct, shorten, or copy an id out of a
  summary/checkpoint without first confirming it appears in a real result. If it cannot be
  confirmed, start a new session instead.
- **Before re-dispatching to an existing child**, confirm that child still exists: its id
  was returned by a real dispatch AND its last reported state was not a terminal failure
  (`error`, timeout, provider failure). A failed child is not a reusable session — start a
  new one and pass along any partial findings rather than retrying the dead id.
- **Never batch-claim**: if that turn contains no `subagent` call, say so plainly rather
  than listing tasks as "sent".
- When a dispatch fails (timeout / provider error), report it in the same turn, state which
  session died, and either split the brief smaller or hand the work to another tier — do
  not silently drop it.
