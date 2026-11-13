"""Self-tests for the conformance runner, independent of oxlox behavior."""

import argparse
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

import run as runner


class RunnerTests(unittest.TestCase):
    def validate(self, source, stdout="", stderr="", code=0):
        result = subprocess.CompletedProcess([], code, stdout, stderr)
        return runner.validate(runner.parse(source), result)

    def test_stdout_matches_in_order_and_handles_crlf(self):
        source = "print 1; // expect: 1\nprint 2; // expect: 2"
        self.assertEqual([], self.validate(source, "1\r\n2\r\n"))
        self.assertTrue(self.validate(source, "2\n1\n"))
        self.assertTrue(self.validate(source, "1\n"))
        self.assertTrue(self.validate(source, "1\n2\n3\n"))

    def test_empty_expectation_and_upstream_trailing_blank_tolerance(self):
        self.assertEqual([(1, "")], runner.parse("print \"\"; // expect:").output)
        self.assertEqual([], self.validate("// expect: ok", "ok\n\n"))
        self.assertTrue(self.validate("// expect: ok", "ok\n\n\n"))

    def test_normal_errors_use_the_comment_line(self):
        source = "\nvar ; // Error at ';': Expect variable name."
        stderr = "[line 2] Error at ';': Expect variable name.\n"
        self.assertEqual([], self.validate(source, stderr=stderr, code=65))
        self.assertTrue(self.validate(source, stderr=stderr, code=0))
        self.assertTrue(self.validate(source, code=65))
        self.assertTrue(self.validate(source, stderr=stderr + "surprise\n", code=65))
        self.assertTrue(self.validate(source, stderr=stderr.replace("2", "1"), code=65))

    def test_explicit_lines_and_language_specific_errors(self):
        source = "\n".join([
            "// [line 7] Error shared",
            "// [c line 8] Error clox only",
            "// [java line 9] Error jlox only",
        ])
        self.assertEqual({"[7] Error shared", "[8] Error clox only"}, runner.parse(source).errors)
        self.assertEqual([], self.validate(source, stderr="[line 8] Error clox only\n[line 7] Error shared\n", code=65))
        self.assertEqual(0, runner.parse("// [java line 9] Error ignored").exit_code)

    def test_runtime_message_and_first_stack_frame(self):
        source = "// expect: before\n1 + nil; // expect runtime error: Operands must be numbers."
        stderr = "Operands must be numbers.\n[line 2] in script\n"
        self.assertEqual([], self.validate(source, "before\n", stderr, 70))
        self.assertTrue(self.validate(source, "before\n", stderr, 65))
        self.assertTrue(self.validate(source, "before\n", stderr.replace("numbers", "strings"), 70))
        self.assertTrue(self.validate(source, "before\n", stderr.replace("line 2", "line 1"), 70))
        self.assertTrue(self.validate(source, "before\n", "Operands must be numbers.\n", 70))
        self.assertTrue(self.validate(source, "before\n", "Operands must be numbers.\nno trace\n", 70))

    def test_unexpected_errors_and_panics_fail(self):
        self.assertTrue(self.validate("", stderr="thread 'main' panicked\n", code=101))
        self.assertTrue(self.validate("", stderr="[line 1] Error unexpected\n", code=65))
        self.assertTrue(self.validate("", stdout="== disassembly ==\n"))

    def test_non_tests_and_inconsistent_expectations(self):
        self.assertIsNone(runner.parse("// nontest\n// expect: ignored"))
        with self.assertRaises(ValueError):
            runner.parse("// Error bad syntax\n// expect runtime error: bad value")

    def test_discovery_matches_upstream_clox_exclusions(self):
        selected, skipped = runner.discover("")
        self.assertEqual(246, len(selected))
        self.assertEqual(19, skipped)
        self.assertTrue(all(path.relative_to(runner.FIXTURES).parts[0]
                            not in ("benchmark", "scanning", "expressions")
                            for path, _ in selected))
        selected, _ = runner.discover("string/literals.lox")
        self.assertEqual(1, len(selected))
        self.assertEqual([], runner.discover("nonexistent/")[0])

    def test_subprocess_execution_captures_streams_and_status(self):
        with tempfile.TemporaryDirectory() as directory:
            script = Path(directory) / "fake.py"
            script.write_text("import sys\nprint('ok')\nprint('[line 2] Error fake', file=sys.stderr)\nsys.exit(65)\n")
            expected = runner.parse("// expect: ok\n// Error fake")
            self.assertEqual([], runner.run_test(sys.executable, script, expected, 5))

    def test_hanging_interpreter_is_killed(self):
        with tempfile.TemporaryDirectory() as directory:
            script = Path(directory) / "hang.py"
            script.write_text("import time\ntime.sleep(60)\n")
            failures = runner.run_test(sys.executable, script, runner.parse(""), 0.1)
            self.assertEqual(["Timed out after 0.1s"], failures)

    def test_timeout_must_be_positive_and_finite(self):
        for invalid in ("0", "-1", "nan", "inf"):
            with self.assertRaises(argparse.ArgumentTypeError):
                runner.positive_seconds(invalid)


if __name__ == "__main__":
    unittest.main()
