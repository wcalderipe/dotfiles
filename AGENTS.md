# AGENTS.md

## Principles

- Every command must be idempotent: running any script once or ten times
  produces the same result with no errors.

## Commit Messages

We use Conventional Commits (see `docs/adr/015-use-conventional-commits.md`).

**Format:** `<type>: <description>`

**Types:** `feat`, `fix`, `docs`, `refactor`, `test`, `chore`

**Rules:**

- Subject line is up to 60 characters
- Body lines are up to 72 characters
- Never add Co-Authored-By trailers
- Description is lowercase, no period at the end
- Write what is achieved, not what was done (e.g. "show all access groups in
  notifications" not "changed active_access_groups to access_groups")

**Body (optional, when helpful):**

- List what is achieved as bullet points
- Document decisions made that are not recorded in ADRs
- Note trade-offs left behind (things intentionally not addressed)

**Examples:**

```
feat: show all access groups in member notifications

Notifications and admin index display groups regardless of grant status.

Remove active_access_groups method, no longer needed.
```

```
fix: prevent duplicate webhook deliveries

Idempotency key based on event ID and attempt number.

Trade-off: old events without IDs still rely on timestamp dedup.
```
