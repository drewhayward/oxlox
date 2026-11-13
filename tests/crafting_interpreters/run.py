#!/usr/bin/env python3
"""Run the upstream clox conformance suite against a command-line interpreter.

Expectation parsing and validation are ported from Robert Nystrom's test.dart.
See LICENSE and upstream_test.dart in this directory for attribution/reference.
"""

import argparse
from dataclasses import dataclass, field
import json
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
FIXTURES = Path(__file__).resolve().parent / "test"
EXPECTED_OUTPUT = re.compile(r"// expect: ?(.*)")
EXPECTED_ERROR = re.compile(r"// (Error.*)")
ERROR_LINE = re.compile(r"// \[((java|c) )?line (\d+)\] (Error.*)")
RUNTIME_ERROR = re.compile(r"// expect runtime error: (.+)")
SYNTAX_ERROR = re.compile(r"\[.*line (\d+)\] (Error.+)")
STACK_TRACE = re.compile(r"\[line (\d+)\]")


def lines(text):
    """Match Dart's LineSplitter, including CRLF and a missing final newline."""
    result = text.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    if result[-1] == "":
        result.pop()
    return result


@dataclass
class Expectations:
    output: list = field(default_factory=list)
    errors: set = field(default_factory=set)
    runtime_error: str = None
    runtime_line: int = 0
    exit_code: int = 0


def parse(source):
    expected = Expectations()
    for number, line in enumerate(lines(source), 1):
        if "// nontest" in line:
            return None
        match = EXPECTED_OUTPUT.search(line)
        if match:
            expected.output.append((number, match[1]))
            continue
        match = EXPECTED_ERROR.search(line)
        if match:
            expected.errors.add(f"[{number}] {match[1]}")
            expected.exit_code = 65
            continue
        match = ERROR_LINE.search(line)
        if match:
            # oxlox follows clox, including its parser recovery expectations.
            if match[2] in (None, "c"):
                expected.errors.add(f"[{match[3]}] {match[4]}")
                expected.exit_code = 65
            continue
        match = RUNTIME_ERROR.search(line)
        if match:
            expected.runtime_error = match[1]
            expected.runtime_line = number
            expected.exit_code = 70
    if expected.errors and expected.runtime_error is not None:
        raise ValueError("Cannot expect both compile and runtime errors")
    return expected


def validate(expected, result):
    failures = []
    stderr = lines(result.stderr)
    if expected.runtime_error is not None:
        if len(stderr) < 2:
            failures.append(f"Expected runtime error {expected.runtime_error!r} and stack trace; got {stderr!r}")
        else:
            if stderr[0] != expected.runtime_error:
                failures.append(f"Expected runtime error {expected.runtime_error!r}; got {stderr[0]!r}")
            trace = next((STACK_TRACE.search(line) for line in stderr[1:]
                          if STACK_TRACE.search(line)), None)
            if trace is None:
                failures.append(f"Expected stack trace; got {stderr[1:11]!r}")
            elif int(trace[1]) != expected.runtime_line:
                failures.append(f"Expected runtime error on line {expected.runtime_line}; got line {trace[1]}")
    else:
        found = set()
        unexpected = []
        for line in stderr:
            match = SYNTAX_ERROR.search(line)
            error = f"[{match[1]}] {match[2]}" if match else None
            if error in expected.errors:
                found.add(error)
            elif line:
                unexpected.append(line)
        for line in unexpected[:10]:
            failures.append(f"Unexpected stderr: {line}")
        if len(unexpected) > 10:
            failures.append(f"... {len(unexpected) - 10} more stderr lines")
        for error in sorted(expected.errors - found):
            failures.append(f"Missing expected error: {error}")

    if result.returncode != expected.exit_code:
        failures.append(f"Expected exit code {expected.exit_code}; got {result.returncode}")

    output = lines(result.stdout)
    # The upstream runner tolerates one trailing empty output line.
    if output and output[-1] == "":
        output.pop()
    for index, actual in enumerate(output):
        if index >= len(expected.output):
            failures.append(f"Unexpected stdout: {actual!r}")
        else:
            line, wanted = expected.output[index]
            if actual != wanted:
                failures.append(f"Line {line}: expected stdout {wanted!r}; got {actual!r}")
    for line, wanted in expected.output[len(output):]:
        failures.append(f"Line {line}: missing stdout {wanted!r}")
    return failures


def discover(prefix):
    selected = []
    skipped = 0
    for path in sorted(FIXTURES.rglob("*.lox")):
        relative = path.relative_to(FIXTURES)
        if not relative.as_posix().startswith(prefix):
            continue
        # These exclusions match the full upstream clox suite, not oxlox's
        # current feature set. Keep unsupported language features as failures.
        if relative.parts[0] in ("benchmark", "scanning", "expressions"):
            skipped += 1
            continue
        expected = parse(path.read_text(encoding="utf-8"))
        if expected is None:
            skipped += 1
        else:
            selected.append((path, expected))
    return selected, skipped


def build_interpreter():
    result = subprocess.run(
        ["cargo", "build", "--bin", "oxlox", "--message-format=json"],
        cwd=ROOT, stdout=subprocess.PIPE, text=True, check=True,
    )
    for line in result.stdout.splitlines():
        message = json.loads(line)
        if message.get("reason") == "compiler-message":
            rendered = message["message"].get("rendered")
            if rendered:
                print(rendered, end="", file=sys.stderr)
        if (message.get("reason") == "compiler-artifact"
                and message["target"]["name"] == "oxlox"
                and message.get("executable")):
            return Path(message["executable"])
    raise ValueError("Cargo did not report an oxlox executable")


def run_test(interpreter, path, expected, timeout):
    try:
        result = subprocess.run(
            [str(interpreter), str(path)], cwd=ROOT,
            capture_output=True, text=True, encoding="utf-8", errors="replace",
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return [f"Timed out after {timeout:g}s"]
    return validate(expected, result)


def positive_seconds(value):
    seconds = float(value)
    if not 0 < seconds < float("inf"):
        raise argparse.ArgumentTypeError("timeout must be a finite positive number")
    return seconds


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("prefix", nargs="?", default="", help="fixture path prefix, e.g. operator/ or string/literals.lox")
    parser.add_argument("--interpreter", type=Path, help="existing executable (default: build oxlox with Cargo)")
    parser.add_argument("--timeout", type=positive_seconds, default=5.0, help="seconds per test (default: 5)")
    parser.add_argument("--list", action="store_true", help="list selected tests without building or running")
    parser.add_argument("--quiet", action="store_true", help="print only the summary")
    args = parser.parse_args(argv)
    try:
        selected, skipped = discover(args.prefix)
        if not selected:
            parser.error(f"no runnable tests match {args.prefix!r} ({skipped} excluded fixtures)")
        if args.list:
            for path, _ in selected:
                print(path.relative_to(FIXTURES).as_posix())
            return 0
        interpreter = args.interpreter.resolve() if args.interpreter else build_interpreter()
        passed = failed = 0
        for path, expected in selected:
            failures = run_test(interpreter, path, expected, args.timeout)
            if failures:
                failed += 1
                if not args.quiet:
                    print(f"FAIL {path.relative_to(FIXTURES).as_posix()}")
                    for failure in failures[:12]:
                        print(f"  {failure}")
                    if len(failures) > 12:
                        print(f"  ... {len(failures) - 12} more failures")
            else:
                passed += 1
        print(f"{passed} passed; {failed} failed; {skipped} skipped.")
        return 1 if failed else 0
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"Runner error: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
