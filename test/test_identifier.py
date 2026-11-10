from dataclasses import dataclass


class MissingArtifacts(RuntimeError):
    """Raised by a testsuite when a testcase is run without its build artifacts."""
    pass


@dataclass(frozen=True, order=True)
class TestIdentifier:
    """A testcase: (suite_name, test_name, mode).

    Serialized as ``suite_name::test_name::mode``. ``::`` separates the three
    values: the first ``::`` ends the suite name, the last ``::`` starts the
    mode, and everything between them is the test name (which may contain
    ``/``). A suite with a single mode has an empty mode (``asm::simple``).
    """
    suite_name: str
    test_name: str
    mode: str

    def __str__(self):
        return f"{self.suite_name}::{self.test_name}::{self.mode}"

    @classmethod
    def from_str(cls, s: str) -> "TestIdentifier":
        suite_name, rest = s.split("::", 1)
        if "::" in rest:
            test_name, mode = rest.rsplit("::", 1)
        else:
            test_name, mode = rest, ""
        return cls(suite_name, test_name, mode)

    @property
    def path(self) -> str:
        # Filesystem-safe form for workdir subpaths: "::" -> "/".
        return str(self).replace("::", "/")
