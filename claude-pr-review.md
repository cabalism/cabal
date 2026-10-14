# Claude PR reviews for haskell/cabal: feasibility, cost and setup

Written 2026-10-14. Includes a sample review of the `exact-pp-leana` branch (exact-print parser with comments) run with Claude Fable 5.1 at medium effort, and an estimate of what reviewing every cabal PR with Fable would cost.

## Sample review: exact-pp-leana

Branch at commit 48cb2a661, 15 commits, 27 files, 1,486 insertions and 102 deletions against the merge base with master.

### Regressions against master (confirmed by running the parser)

1. **Comment before the `{` of a brace-style field fails to parse.** `readFields "foo:\n-- c\n{\n bar\n}\n"` now gives a parse error at line 3 ("expecting indentation of at least 1"), while the same input without the comment parses, as it did on master. The same regression exists inside braces sections via `fieldInlineOrBraces`. Fix: consume `many tokComment` before `openBrace` in both brace branches, as `elementInLayoutContext` already does for sections. `Cabal-syntax/src/Distribution/Fields/Parser.hs:435` and `:450`.
2. **Trailing comment after a section name inside a braces block is a hard parse error.** `readFields "x { library -- c\n {\n  foo: bar\n }\n}\n"` fails with `unexpected comment "-- c"; expecting ":", section parameter or "{"`. The layout branch at line 367 handles this; the non-layout branch `elementInNonLayoutContext` does `many sectionArg; openBrace` with nothing in between. `Cabal-syntax/src/Distribution/Fields/Parser.hs:386`.

### Comment API behaviour (confirmed)

3. **Comment-only file loses all comments.** `readFieldsWithComments "-- only\n" == Right []`. A `cabal.project` that is nothing but a commented-out `packages:` line round-trips to empty. `cabalStyleFile` discards the comments when there are no fields even though `elements` already carries them to that point. `Cabal-syntax/src/Distribution/Fields/Parser.hs:269`.
4. **One comment list per annotation cannot say before or after.** `readFieldsWithComments "library\n  foo: bar\n-- trailing\n\nexecutable e\n  main-is: M.hs\n"` attaches `Comment "-- trailing" (Position 3 1)` to the `FieldLine "bar"` inside `library`, though it sits at column 1 directly above `executable`. An empty `Section` likewise merges `commentsAfterSectionArgs` and `innerCmts` (line 371). The changelog promises "preceding and succeeding comments" but the type cannot distinguish them. A `WithComments { before, after }` record, or attaching between-element comments to the following element, fixes this at the type level. `Cabal-syntax/src/Distribution/Fields/Parser.hs:302`.
5. **Debug trace returns the wrong binding.** `traceShow t $ return tok` under `CABAL_PARSEC_DEBUG` returns the top-level `tok` helper, not the token `t`. Should be `return t` or `traceShow t (return ())`. `Cabal-syntax/src/Distribution/Fields/Lexer.x:234`.

### Consistency, API and cleanup (plausible, not verified end to end)

6. **Beginning-of-line and in-section comments differ in text and column.** BOL comments keep leading whitespace in the `Comment` ByteString and sit at column 1 (golden files show `Comment "    -- foo" (Position 13 1)`), while in-section comments start at `--`. A printer re-emitting comments at position plus text will double-indent one kind. Trim the `$spacetab*` prefix and advance the position in the bol rules, or document the asymmetry. `Cabal-syntax/src/Distribution/Fields/Lexer.x:86`.
7. **New public API has no `@since`, and `Comment` argument order is inverted.** `Comment`, `WithComments`, `Distribution.Fields.Field.Lens`, `readFieldsWithComments`, `readFieldsWithComments'` and the newly exported `formatError` carry no `@since`, and `Comment` puts the ByteString before the annotation unlike `Name`, `FieldLine` and `SecArgStr`. `Cabal-syntax/src/Distribution/Fields/Field.hs:50`.
8. **Grammar comment is self-recursive.** `Elements ::= Elements* '\n'?` should read `Element* '\n'?`. `Cabal-syntax/src/Distribution/Fields/Parser.hs:217`.
9. **`readFieldTest` and `commentTest` are near-duplicates** split across four `#ifdef MIN_VERSION_tree_diff` blocks, differing only in the projection applied to the parsed fields. One `fieldGolden :: ToExpr a => ([Field (WithComments Position)] -> a) -> FilePath -> TestTree` covers both. `Cabal-tests/tests/ParserTests.hs:138`.
10. **`appendCommentsField` rebuilds nested spines when there are no comments.** `elements` calls it for every element even with empty `postCmts`. Short-circuit on `null postCmts`, and likewise in `prependCommentsFields`. `Cabal-syntax/src/Distribution/Fields/Parser.hs:332`.

## Can cabal use Fable for PR reviews?

Yes. Two documented routes exist, billed differently.

**Route 1: the `anthropics/claude-code-action` GitHub Action.** A workflow file plus an API key secret. Billing is pay-per-token API pricing from an Anthropic Console organisation. The docs say a shared org repo should use an API key rather than a subscription OAuth token, because an OAuth token is tied to the subscription of the person who ran `claude setup-token`, and the terms say Pro and Max limits assume ordinary individual usage. The model is chosen with `--model claude-fable-5-1` in the action's `claude_args`. Fable 5.1 requires 30-day data retention, which a standard (non-ZDR) Console org has. No subscription is required for this route. Fable 5.1 needs Claude Code v2.1.257 or later, so pin or check the action version.

**Route 2: the hosted Code Review product.** No workflow file. An org owner enables it at claude.ai admin settings and installs the Claude GitHub App. It runs on Anthropic infrastructure and posts inline comments, a summary and a neutral check run. It requires a **Team or Enterprise plan**, and reviews are billed as usage credits on top of seats, averaging $15 to $25 per review per Anthropic's docs. Owners can set a monthly spend cap. The docs do not show a model setting, so whether Fable is selectable there is unconfirmed.

**`/code-review ultra` (ultrareview)** is a third, separate thing: a paid multi-agent cloud review launched from a developer's own Claude Code session, roughly $5 to $25 per run after three free runs on Pro or Max. It can post findings to a PR as a comment from the developer's account. It is not a CI reviewer.

**The catch for a public repo.** GitHub withholds secrets from workflows triggered by fork PRs, so the Action can only auto-review PRs from branches in haskell/cabal itself. Over July to September 2026 that was 151 of 270 PRs; the other 119 (44%) came from forks. For fork PRs a maintainer with write access comments `@claude review` to trigger a run via the `issue_comment` event. The hosted product has the same rule: fork PRs only review on a maintainer's top-level comment.

## Cost estimate

### Inputs

| Measure | Value |
|---|---|
| PRs opened, July 2026 | 98 |
| PRs opened, August 2026 | 87 |
| PRs opened, September 2026 | 85 |
| PRs opened, 1 to 14 October 2026 | 43 |
| Share from forks, Jul to Sep | 44% (119 of 270) |
| Median PR size (lines changed) | 57 |
| 90th percentile PR size | 440 |
| Mean PR size | 324 |
| This branch | 1,586 lines, 27 files, 157 KB diff |

### Pricing per million tokens (platform.claude.com pricing page, 2026-10-14)

| Model | Input | 5m cache write | Cache read | Output |
|---|---|---|---|---|
| Claude Fable 5.1 (`claude-fable-5-1`) | $10 | $12.50 | $0.25 | $50 |
| Claude Opus 5.5 (`claude-opus-5-5`) | $4 | $5 | $0.20 | $20 |
| Claude Sonnet 5.5 (`claude-sonnet-5-5`) | $2 | $2.50 | $0.10 | $10 |

### Estimates

Most of a review's tokens are cache reads of the growing context, so cost is dominated by output and thinking tokens plus the initial cache write of the files the reviewer reads.

| Scenario | Fable 5.1 | Opus 5.5 | Sonnet 5.5 |
|---|---|---|---|
| Median PR, one review | $1 to $2 | $0.50 to $1 | $0.25 to $0.50 |
| 90th-percentile PR | $4 to $6 | $2 to $3 | $1 to $1.50 |
| This branch | $8 to $15 | $4 to $6 | $2 to $3 |
| Monthly, 90 PRs, review on open only | $200 to $300 | $100 to $150 | $50 to $75 |
| Monthly, re-review on every push | $500 to $1,000 | $250 to $400 | $120 to $200 |
| Monthly, hosted Code Review product | $1,350 to $2,250 plus Team seats | n/a | n/a |

The per-review figures for the Action are a model of a Claude Code review session, not measurements. They sit below Anthropic's own $15 to $25 average for the hosted product, which presumably runs more passes. GitHub Actions minutes are free on public repos. The right first step is to run the Action on 10 or 20 PRs under a workspace spend cap and read the actual numbers from the Console usage page.

## Setup for Route 1 (GitHub Action with API key)

1. **Console org.** Create a pay-as-you-go Anthropic Console organisation under a project account, make a workspace for CI with a monthly spend limit, and create an API key scoped to it. Add it to haskell/cabal as the `ANTHROPIC_API_KEY` secret. Leave data retention at the standard setting; Fable is not available under zero data retention.
2. **GitHub App.** A repo admin runs `/install-github-app` from Claude Code (needs the `gh` CLI and admin access), or installs the Claude GitHub App manually.
3. **Workflow.** Copy the automated PR review example from the GitHub Actions docs page (https://code.claude.com/docs/en/github-actions). It uses the `code-review@claude-code-plugins` plugin with `prompt: "/code-review:code-review --comment owner/repo/pull/N"` and the `plugin_marketplaces` and `plugins` inputs. Add the model flag and the inline-comment tool:

   ```yaml
   claude_args: '--model claude-fable-5-1 --allowedTools "mcp__github_inline_comment__create_inline_comment"'
   ```

   Trigger on `pull_request` with types `opened, synchronize, ready_for_review, reopened`, permissions `contents: read`, `pull-requests: read`, `issues: read`, `id-token: write`, and `actions/checkout` with `fetch-depth: 1`. Drop `synchronize` to pay per PR rather than per push. Add an `if:` guard that `github.event.pull_request.head.repo.full_name == github.repository` so fork PRs skip cleanly instead of failing on the missing secret.
4. **Fork PRs.** Keep `issue_comment` as a second trigger so a maintainer's `@claude review` comment reviews a fork PR; the commenter must have write access. Do not enable `allowed_non_write_users` on a public repo; the docs mark it as a significant security risk. Prefer an explicit `allowed_bots` list over `'*'`. Avoid `pull_request_target` unless the untrusted PR head is never checked out before the action runs. The action restores `.claude/`, `.mcp.json` and `CLAUDE.md` from the base branch on PR runs.
5. **Review policy.** The plugin reads a `REVIEW.md` or `CLAUDE.md` for project-specific guidance, which is where things like "ignore `make style` reformatting churn" would go.

## Open questions for Anthropic

- Does the hosted Code Review product let an org pick Fable 5.1, or is the model fixed?
- Does the hosted product work on public repos? The docs page does not say.
- Is a subscription OAuth token permitted in CI on a shared org repo? The docs steer shared use to API keys but do not state a rule.
- Is there any credit programme for open-source projects?

## Sources

- https://code.claude.com/docs/en/github-actions
- https://github.com/anthropics/claude-code-action
- https://github.com/anthropics/claude-code-action/blob/main/docs/security.md
- https://code.claude.com/docs/en/code-review
- https://code.claude.com/docs/en/ultrareview
- https://code.claude.com/docs/en/authentication
- https://code.claude.com/docs/en/legal-and-compliance
- https://code.claude.com/docs/en/model-config
- https://platform.claude.com/docs/en/about-claude/pricing
- https://platform.claude.com/docs/en/models/fable-5-1/overview
- https://platform.claude.com/docs/en/manage-claude/api-and-data-retention
- https://code.claude.com/docs/en/zero-data-retention
