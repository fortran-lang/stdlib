#!/usr/bin/env python3
"""Check that a pull request carries no AI attribution.

Whoever opens a pull request owns it: they are responsible for every change in
it, whatever tools they used to write it, and authorship belongs to people. So
the commits and the pull-request text carry no co-author trailer, generated-with
footer, product name, link or address that credits an AI coding tool, and the
commits are authored by people. Human co-authors stay welcome.

Surfaces checked:

  * every commit in the pull request (merges excluded): the message and the
    author's name and address;
  * the pull-request title and body;
  * the pull-request branch name (its first segment).

The workflow hands everything over through the environment, never on a command
line, so text from the pull request is never parsed by a shell:

  ATTRIBUTION_BASE, ATTRIBUTION_HEAD   commit SHAs of the pull request
  ATTRIBUTION_BASE_REF                 base branch name (commits already on
                                       origin/<ref> are not checked)
  ATTRIBUTION_PR_TITLE, ATTRIBUTION_PR_BODY, ATTRIBUTION_PR_BRANCH

Run locally without them to check the current branch before pushing; the range
is then <merge-base with master>..HEAD and the title and body are skipped:

  python3 .github/check_ai_attribution.py
  python3 .github/check_ai_attribution.py --selftest

Each line is checked on its own, and the first pattern that hits it reports
it. Paths that only name a tool's configuration file (CLAUDE.md, AGENTS.md,
.github/copilot-instructions.md, .cursor/, ...) are masked out first: naming a
file is not attribution.
"""

from __future__ import annotations

import os
import re
import subprocess
import sys

BASE_ENV = "ATTRIBUTION_BASE"
HEAD_ENV = "ATTRIBUTION_HEAD"
BASE_REF_ENV = "ATTRIBUTION_BASE_REF"
TITLE_ENV = "ATTRIBUTION_PR_TITLE"
BODY_ENV = "ATTRIBUTION_PR_BODY"
BRANCH_ENV = "ATTRIBUTION_PR_BRANCH"

RULE = ("whoever opens a pull request owns it: no AI attribution in commits or "
        "pull-request text, and only people as authors and co-authors")

I = re.IGNORECASE

#: Configuration files and folders of AI coding tools, named by path.
TOOL_PATH_RE = re.compile(
    r"(?:[\w.-]*/)*"
    r"(?:\.(?:claude|cursor|codex|gemini|copilot|windsurf|junie|aider[\w.-]*)(?![\w-])(?:/[\w./*+-]*)?"
    r"|(?:claude|agents|gemini|copilot-instructions)\.md\b"
    r"|\.cursorrules\b|\.windsurfrules\b)", I)

#: An identity (name and address) that belongs to an AI tool or its bot account.
AI_IDENTITY_RE = re.compile(
    r"\b(?:claude|anthropic|copilot|chatgpt|openai|codex|gemini|cursor|aider|grok"
    r"|windsurf|codeium|devin[\s_-]*ai|devin-ai-integration|google-labs-jules|jules\[bot\])\b"
    r"|\bcopilot-swe-agent\b", I)

#: `Co-authored-by:` at the start of a line, i.e. a trailer.
COAUTHOR_RE = re.compile(r"^\s*co-authored-by\s*:\s*(?P<who>.*)$", I)

#: Wording that credits someone for the work.
ATTRIB_RE = re.compile(
    r"\b(?:(?:generated|created|(?:co-?)?authored|(?:co-?)?written|produced|assisted"
    r"|powered|made|drafted|coded|done|helped)\s+(?:with|by|using|via|in)"
    r"|with\s+(?:the\s+)?help\s+(?:of|from)|thanks\s+to|using|used|via)\b", I)

#: AI tools and the generic terms, for use next to an attribution phrase.
AI_TOKEN_RE = re.compile(
    r"\b(?:ai|llms?|claude|anthropic|copilot|chatgpt|openai|codex|gemini|grok|aider"
    r"|windsurf|codeium|cursor[\s_-]*agent|devin[\s_-]*ai)\b", I)

#: The generated-with footer opens with a robot emoji.
ROBOT_RE = re.compile("\U0001f916")

#: Links to AI tools, including the session links some of them append.
AI_URL_RE = re.compile(
    r"\b(?:claude\.(?:ai|com)|(?:[\w-]+\.)*anthropic\.com|chatgpt\.com|(?:[\w-]+\.)*openai\.com"
    r"|gemini\.google\.com|(?:[\w-]+\.)*devin\.ai|cursor\.(?:com|sh)|jules\.google(?:\.com)?"
    r"|aider\.chat|copilot\.microsoft\.com|gh\.io/copilot[\w-]*|github\.com/(?:features/)?copilot"
    r"|windsurf\.com|codeium\.com)(?:/\S*)?", I)

#: Addresses of AI vendors and of AI bot accounts on GitHub.
AI_EMAIL_RE = re.compile(
    r"@(?:anthropic|openai|cursor)\.com\b|@aider\.chat\b"
    r"|\b(?:\d+\+)?(?:copilot|claude|codex|cursor|devin-ai-integration|google-labs-jules"
    r"|chatgpt-codex-connector|gemini-code-assist)(?:\[bot\])?@users\.noreply\.github\.com", I)

#: Product and model names, which in a commit message or pull-request text can
#: only be attribution.
AI_PRODUCT_RE = re.compile(
    r"\b(?:claude[\s_-]*(?:code|fable|opus|sonnet|haiku|ai)|(?:github[\s_-]*)?copilot|chatgpt"
    r"|gpt-?\d(?:\.\d+)?(?:o|-?turbo|-?mini|-?codex)?|openai[\s_-]*codex"
    r"|codex[\s_-]*(?:cli|cloud|agent)|gemini[\s_-]*(?:cli|code[\s_-]*assist|pro|flash|ultra|\d(?:\.\d+)?)"
    r"|cursor[\s_-]*(?:agent|ai|composer)|devin[\s_-]*ai|aider[\s_-]*chat|amazon[\s_-]*q[\s_-]*developer"
    r"|windsurf|codeium|google[\s_-]*jules)\b", I)

#: Branch prefixes that name the tool that opened the branch.
AI_BRANCH_RE = re.compile(
    r"^(?:claude|copilot|codex|cursor|devin|jules|gemini|aider|openai|chatgpt|windsurf)$", I)


def line_finding(line: str) -> str | None:
    """The first pattern that hits `line`, or None."""
    line = TOOL_PATH_RE.sub(" ", line)
    trailer = COAUTHOR_RE.match(line)
    if trailer is not None and AI_IDENTITY_RE.search(trailer.group("who")):
        return "co-author trailer crediting an AI tool"
    verb = ATTRIB_RE.search(line)
    if verb is not None:
        token = AI_TOKEN_RE.search(line)
        if token is not None:
            return "'%s' next to '%s'" % (verb.group(0), token.group(0))
        if ROBOT_RE.search(line):
            return "robot-emoji '%s' line" % verb.group(0)
    for regex, what in ((AI_URL_RE, "link"), (AI_EMAIL_RE, "address"),
                        (AI_PRODUCT_RE, "product name")):
        hit = regex.search(line)
        if hit is not None:
            return "AI tool %s '%s'" % (what, hit.group(0))
    return None


def check_text(text: str) -> list[tuple[int, str]]:
    """(line number, finding) for every line of `text` that credits an AI tool."""
    found = []
    for lineno, line in enumerate(text.splitlines(), start=1):
        finding = line_finding(line)
        if finding is not None:
            found.append((lineno, finding))
    return found


def check_author(identity: str) -> str | None:
    """A finding when a commit's author is an AI tool account."""
    if AI_IDENTITY_RE.search(identity):
        return "commit authored by an AI tool account (%s)" % identity
    return None


def check_branch(branch: str) -> str | None:
    """A finding when the branch's first segment names an AI tool."""
    if "/" in branch and AI_BRANCH_RE.match(branch.split("/", 1)[0]):
        return "branch '%s' is named after an AI tool" % branch
    return None


def git(*args: str) -> str | None:
    proc = subprocess.run(("git",) + args, stdout=subprocess.PIPE,
                          stderr=subprocess.DEVNULL, universal_newlines=True,
                          encoding="utf-8", errors="replace")
    return proc.stdout if proc.returncode == 0 else None


def is_pull_request() -> bool:
    return os.environ.get("GITHUB_EVENT_NAME", "").startswith("pull_request")


def commits() -> list[tuple[str, str, str]]:
    """(sha, author, message) of every non-merge commit in the pull request."""
    head = os.environ.get(HEAD_ENV) or "HEAD"
    exclude = []
    base = os.environ.get(BASE_ENV)
    if base:
        exclude.append(base)
        ref = os.environ.get(BASE_REF_ENV)
        if ref and git("rev-parse", "--verify", "--quiet", "refs/remotes/origin/" + ref):
            exclude.append("refs/remotes/origin/" + ref)
    elif is_pull_request():
        # On a pull request an unchecked commit range would be a hole in the check.
        sys.exit("error: %s is not set: the workflow must pass the pull-request base" % BASE_ENV)
    else:
        for ref in ("upstream/master", "origin/master", "master"):
            out = git("merge-base", ref, head)
            if out and out.strip():
                exclude.append(out.strip())
                break
    if not exclude:
        print("note: no base commit found; commit messages not checked")
        return []
    out = git("log", "--no-merges", "--format=%H%x1f%an <%ae>%x1f%B%x1e", head, "--not", *exclude)
    if out is None:
        if is_pull_request():
            sys.exit("error: cannot read the commits of the pull request; "
                     "the checkout needs the full history (fetch-depth: 0)")
        print("note: cannot read the commit range; commit messages not checked")
        return []
    result = []
    for record in out.split("\x1e"):
        fields = record.strip("\r\n").split("\x1f", 2)
        if len(fields) == 3:
            result.append((fields[0].strip(), fields[1].strip(), fields[2]))
    return result


def report(where: str, lineno: int | None, finding: str) -> None:
    at = where if lineno is None else "%s, line %d" % (where, lineno)
    print("::error title=AI attribution::%s: %s" % (at, finding))


def main() -> int:
    findings = 0
    scanned = []

    found_commits = commits()
    scanned.append("%d commit(s)" % len(found_commits))
    for sha, author, message in found_commits:
        where = "commit %s" % sha[:9]
        finding = check_author(author)
        if finding:
            report(where, None, finding)
            findings += 1
        for lineno, finding in check_text(message):
            report(where, lineno, finding)
            findings += 1

    for env, label in ((TITLE_ENV, "pull-request title"), (BODY_ENV, "pull-request body")):
        text = os.environ.get(env)
        if text is None:
            continue
        scanned.append(label)
        for lineno, finding in check_text(text):
            report(label, lineno, finding)
            findings += 1

    branch = os.environ.get(BRANCH_ENV)
    if branch is None and not is_pull_request():
        out = git("rev-parse", "--abbrev-ref", "HEAD")
        branch = out.strip() if out and out.strip() != "HEAD" else None
    if branch:
        scanned.append("branch name")
        finding = check_branch(branch)
        if finding:
            report("branch", None, finding)
            findings += 1

    print("Checked %s." % ", ".join(scanned))
    if findings:
        print("%d finding(s): %s. Reword the commits (amend or rebase, then "
              "force-push) or edit the pull-request text." % (findings, RULE))
        return 1
    print("No AI attribution found.")
    return 0


# Each case: (label, text, 1-based lines expected to be flagged).
SELFTEST_TEXT = [
    ("trailer naming Claude",
     "fix: guard the port\n\nCo-Authored-By: Claude <noreply@anthropic.com>\n", [3]),
    ("trailer naming a Claude model",
     "fix: guard the port\n\nCo-Authored-By: Claude Opus 5 <noreply@anthropic.com>\n", [3]),
    ("trailer naming the Copilot bot",
     "Co-authored-by: copilot-swe-agent[bot] <198982749+Copilot@users.noreply.github.com>\n", [1]),
    ("trailer naming Copilot", "Co-authored-by: Copilot <copilot@github.com>\n", [1]),
    ("trailer naming the Cursor agent", "Co-authored-by: Cursor Agent <cursoragent@cursor.com>\n", [1]),
    ("trailer naming aider", "Co-authored-by: aider (gpt-5) <noreply@aider.chat>\n", [1]),
    ("trailer naming Codex", "Co-authored-by: Codex <codex@openai.com>\n", [1]),
    ("trailer naming Gemini", "Co-authored-by: Gemini <gemini-code-assist[bot]@users.noreply.github.com>\n", [1]),
    ("trailer naming Devin",
     "Co-authored-by: Devin AI <158243242+devin-ai-integration[bot]@users.noreply.github.com>\n", [1]),
    ("indented trailer", "  co-authored-by: Claude <noreply@anthropic.com>\n", [1]),
    ("generated-with footer",
     "\U0001f916 Generated with [Claude Code](https://claude.com/claude-code)\n", [1]),
    ("robot footer with no tool named", "\U0001f916 Generated with an assistant\n", [1]),
    ("written-with wording", "Tests written with Copilot, reviewed by hand\n", [1]),
    ("tool before the verb", "Claude wrote this; produced with care\n", [1]),
    ("help-from wording", "Ported with help from ChatGPT\n", [1]),
    ("generic AI wording", "This change was generated by AI.\n", [1]),
    ("LLM wording", "Drafted with an LLM and checked by hand\n", [1]),
    ("product name", "perf: rewrite the sweep (Claude Code session)\n", [1]),
    ("Copilot on its own", "Addressed the Copilot review comments\n", [1]),
    ("GitHub Copilot", "Suggested by GitHub Copilot\n", [1]),
    ("Gemini CLI", "found with Gemini CLI\n", [1]),
    ("model name", "benchmarked against GPT-5 output\n", [1]),
    ("Codex task link", "https://chatgpt.com/codex/tasks/task_e_0123456789\n", [1]),
    ("Devin session link", "Link to Devin run: https://app.devin.ai/sessions/0123abcd\n", [1]),
    ("Cursor link", "[Open in Cursor](https://cursor.com/background-agent?bcId=bc-1)\n", [1]),
    ("Claude link", "see https://claude.ai/code for the transcript\n", [1]),
    ("vendor address", "questions to noreply@anthropic.com\n", [1]),
    ("whole Claude footer block",
     "fix: guard the port\n\n"
     "\U0001f916 Generated with [Claude Code](https://claude.com/claude-code)\n\n"
     "Co-Authored-By: Claude <noreply@anthropic.com>\n", [3, 5]),
    # Clean text.
    ("human co-author",
     "fix: guard the port\n\nCo-authored-by: Jane Doe <jane@example.com>\n", []),
    ("human co-author with a noreply address",
     "Co-authored-by: Jane Doe <12345+janedoe@users.noreply.github.com>\n", []),
    ("human co-author who shares a tool's name",
     "Co-authored-by: Devin Kees <6895754+djkees@users.noreply.github.com>\n", []),
    ("sign-off", "Signed-off-by: Jane Doe <jane@example.com>\n", []),
    ("plain commit message",
     "feat: build targets in parallel with OpenMP\n\n"
     "Compile, link and archive commands now start through posix_spawn.\n", []),
    ("trailer named mid-line", "- no Co-Authored-By trailer may credit a tool\n", []),
    ("rule subject", "ci: check pull requests for AI attribution\n", []),
    ("tool config by path", "chore: ignore CLAUDE.md, AGENTS.md and .claude/\n", []),
    ("Copilot instructions by path", "docs: move .github/copilot-instructions.md\n", []),
    ("Cursor rules by path", "chore: drop .cursor/rules and .cursorrules\n", []),
    ("tool config with a directory", "docs: restate the guardrails in doc/CLAUDE.md\n", []),
    ("verb with no tool", "fix: files created by fpm new keep their case\n", []),
    ("using with no tool", "build: link using the archiver from FPM_AR\n", []),
    ("cursor as a word", "fix: restore the cursor after the progress bar\n", []),
    ("vendor-shaped word", "docs: anthropogenic emissions are out of scope\n", []),
    ("AIX", "fix: build on AIX using xlf\n", []),
    ("Gemini3D", "test: build gemini3d with the openmp feature\n", []),
    ("tool name with no verb", "Thanks Claude for the review!\n", []),
]

SELFTEST_AUTHORS = [
    ("Copilot bot", "copilot-swe-agent[bot] <198982749+Copilot@users.noreply.github.com>", True),
    ("Devin bot", "devin-ai-integration[bot] <158243242+devin-ai-integration[bot]@users.noreply.github.com>", True),
    ("Claude", "Claude <noreply@anthropic.com>", True),
    ("version-bump bot", "github-actions[bot] <41898282+github-actions[bot]@users.noreply.github.com>", False),
    ("dependabot", "dependabot[bot] <49699333+dependabot[bot]@users.noreply.github.com>", False),
    ("a person", "Jane Doe <jane@example.com>", False),
    ("a person who shares a tool's name", "Devin Kees <6895754+djkees@users.noreply.github.com>", False),
]

SELFTEST_BRANCHES = [
    ("claude/fix-the-seam", True), ("Copilot/fix-1234", True), ("codex/add-tests", True),
    ("cursor/bc-1234", True), ("fix/claude-md-typo", False), ("ci/no-ai-attribution", False),
    ("main", False), ("claude-notes", False),
]


def selftest() -> int:
    failures = 0
    for label, text, expected in SELFTEST_TEXT:
        got = [lineno for lineno, _ in check_text(text)]
        if got != expected:
            print("FAIL %s: flagged lines %s, expected %s" % (label, got, expected))
            failures += 1
    for label, identity, expected in SELFTEST_AUTHORS:
        if bool(check_author(identity)) != expected:
            print("FAIL author %s: expected %s" % (label, "flagged" if expected else "clean"))
            failures += 1
    for branch, expected in SELFTEST_BRANCHES:
        if bool(check_branch(branch)) != expected:
            print("FAIL branch %s: expected %s" % (branch, "flagged" if expected else "clean"))
            failures += 1
    total = len(SELFTEST_TEXT) + len(SELFTEST_AUTHORS) + len(SELFTEST_BRANCHES)
    print("selftest: %d/%d cases pass" % (total - failures, total))
    return 1 if failures else 0


if __name__ == "__main__":
    if sys.argv[1:] == ["--selftest"]:
        sys.exit(selftest())
    if sys.argv[1:]:
        sys.exit(__doc__)
    sys.exit(main())
