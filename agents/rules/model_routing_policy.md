# Global Model Routing & Subagent Execution Policy

> [!IMPORTANT]
> This policy serves as an explicit standing user instruction for `invoke_subagent` `Model` selection, primary-agent role separation, and high-efficiency model routing (overriding the default `inherit` fallback). It directly aligns with the global configuration in `~/.gemini/config/rules/model_routing_policy.md`.

## 1. Dual-Persona Operating Matrix (By Starting Model Tier)

### Heavy Reasoning Primary (Claude & Gemini Pro) -- Thinking, Planning & Review Only
- When the primary agent starts on a heavy model (**Claude** or **Gemini Pro**), it must **only** perform high-level cognition:
  - Strategic planning, architectural design, code/plan review, system auditing, debugging diagnosis, and subagent orchestration.
  - Creating plan/review artifacts in the brain artifact directory is permitted.
- **Zero Direct Workspace File Edits**: All real workspace file modifications (`replace_file_content`, `write_to_file` on repository code) must be delegated to a subagent (`TypeName="self"`) using a light model (`Model="flash"` or `Model="flash_lite"`), passing the exact specification or diff to apply.

### Fast / Medium Primary (Gemini Flash / Flash-Medium) -- Agile Orchestrator & Dispatcher
- When the primary agent starts on **Flash-Medium / Flash**, it acts as the **Agile Orchestrator**:
  - Handles rapid triage, user interaction, problem boundary definition, direct lightweight searches/checks, and synthesis.
- **Mandatory Escalation for Deep Cognition**: Do **not** attempt complex mathematical derivations, multi-module architectural refactoring, or deep research directly in the Flash primary thread. Instead, **immediately spawn a `Model="pro"` subagent** (or `Model="flash"` if budget is strictly exhausted) to generate the in-depth blueprint or audit.
- **Strict Ban on `Model="inherit"` for Heavy Tasks**: Because inheriting from a Flash primary passes Flash down, **never use `Model="inherit"`** for deep research, math modeling, architecture, or forensic audits; always explicitly specify `Model="pro"`.
- **Precision Execution Delegation**: Delegate planned file diffs and test executions to `Model="flash"` subagents (`TypeName="self"`) to keep context lean and costs minimal.

---

## 2. Model Tier Routing Matrix (`Model` parameter in `invoke_subagent`)

- **`Model="pro"` (On-Demand Deep Brain -- Explicit Specification Required)**:
  - Mandatory for: Deep research, mathematical modeling, statistical derivations, multi-stage architectural plans, forensic audits, and complex analytical evaluations.
  - Prioritize newer/powerful Pro models whenever budget/quota allows; fall back to `Model="flash"` only when budget or quota is strictly constrained.
- **`Model="flash"` (Execution Hands & Agile Exploration -- Default for Delegation)**:
  - Use for executing planned file edits, applying diffs, running test suites (`pytest`, test scripts, linters), syntax verification, codebase searching, reading/summarizing logs or config files, and standard documentation lookups.
  - Also serves as budget-constrained fallback for deep research.
- **`Model="flash_lite"` (Trivial Single-Step Tasks)**:
  - Use for ultra-simple single-step queries, trivial one-line/boilerplate edits, formatting, or quick status checks.

---

## 3. Flash-Medium Pareto Workflow: "Flash Orchestrator + Pro Brain + Flash Hands"

1. **Triage & Scoping (Flash Primary)**: Fast requirements analysis, clarification with user, and task decomposition.
2. **Deep Brain Blueprint (`Model="pro"` Subagent)**: Complex reasoning, formal derivations, and architectural specifications.
3. **Execution & Testing (`Model="flash"` Subagent)**: Precise code application and test verification.
4. **Synthesis & Verification (Flash Primary)**: Audits results against the Pro blueprint and delivers concise, verified outputs to the user.
