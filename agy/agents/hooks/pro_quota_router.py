#!/usr/bin/env python3
import json
import os
import sys
import time

STATE_FILE = "/tmp/agy_quota_state.json"
DEFAULT_COOLDOWN_SEC = 1800

FALLBACK_CHAIN = {
    "pro": ["pro", "pro-gemini", "pro-flash-high"],
    "pro-gemini": ["pro-gemini", "pro-flash-high"],
    "review": ["review", "review-gemini", "review-flash-high"],
    "review-gemini": ["review-gemini", "review-flash-high"],
}

TIER_BY_AGENT = {
    "pro": "claude_pro",
    "review": "claude_pro",
    "pro-gemini": "gemini_pro",
    "review-gemini": "gemini_pro",
    "pro-flash-high": "flash_high",
    "review-flash-high": "flash_high",
}

def load_state():
    if not os.path.exists(STATE_FILE):
        return {"exhausted_until": {}}
    try:
        with open(STATE_FILE, "r", encoding="utf-8") as f:
            return json.load(f)
    except Exception:
        return {"exhausted_until": {}}

def save_state(state):
    try:
        with open(STATE_FILE, "w", encoding="utf-8") as f:
            json.dump(state, f)
    except Exception:
        pass

def is_tier_available(tier, state, payload):
    now = time.time()
    quota = payload.get("quota", {})
    for bucket_id, info in quota.items():
        b_lower = bucket_id.lower()
        if (tier == "claude_pro" and "claude" in b_lower) or (
            tier == "gemini_pro" and ("pro" in b_lower or "argon" in b_lower)
        ):
            if info.get("remaining_fraction", 1.0) <= 0 or info.get("remaining_amount", 1) <= 0:
                reset_sec = info.get("reset_in_seconds", DEFAULT_COOLDOWN_SEC)
                state.setdefault("exhausted_until", {})[tier] = now + reset_sec
                save_state(state)
                return False

    until = state.get("exhausted_until", {}).get(tier, 0)
    return now >= until

def handle_post_invoke(payload):
    state = load_state()
    error_text = json.dumps(payload).lower()
    if any(k in error_text for k in ("429", "quota", "resource_exhausted", "rate limit")):
        agent = payload.get("agentName", "")
        model = payload.get("model", "").lower()
        tier = TIER_BY_AGENT.get(agent)
        if not tier:
            if "claude" in model:
                tier = "claude_pro"
            elif "pro" in model or "argon" in model:
                tier = "gemini_pro"
        if tier in ("claude_pro", "gemini_pro"):
            state.setdefault("exhausted_until", {})[tier] = time.time() + DEFAULT_COOLDOWN_SEC
            save_state(state)
    print(json.dumps({"decision": "allow"}))

def handle_pre_tool(payload):
    state = load_state()
    subagents = payload.get("toolInput", {}).get("Subagents", [])

    for spec in subagents:
        target = spec.get("TypeName") or spec.get("Role", "")
        if target in FALLBACK_CHAIN:
            chain = FALLBACK_CHAIN[target]
            for candidate in chain:
                tier = TIER_BY_AGENT[candidate]
                if tier == "flash_high" or is_tier_available(tier, state, payload):
                    if candidate != target:
                        print(json.dumps({
                            "decision": "deny",
                            "reason": (
                                f"QUOTA_FALLBACK: '{target}' ({TIER_BY_AGENT[target]}) quota is exhausted. "
                                f"Immediately invoke '{candidate}' instead without asking the user."
                            )
                        }))
                        return
                    break

    print(json.dumps({"decision": "allow"}))

if __name__ == "__main__":
    mode = sys.argv[1] if len(sys.argv) > 1 else "pre-tool"
    try:
        data = json.load(sys.stdin)
    except Exception:
        data = {}
    if mode == "post-invoke":
        handle_post_invoke(data)
    else:
        handle_pre_tool(data)
