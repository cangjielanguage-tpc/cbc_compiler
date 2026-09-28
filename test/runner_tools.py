import os
import shutil
import tempfile


def patched(name):
    return f'{name}-patched'


def dotll(name):
    return f'{name}.ll'


def dotll_diff(name):
    return f'{name}.ll_diff'


def dotbc(name):
    return f'{name}.bc'


def dotcj(name):
    return f'{name}.cj'


def dotcjaot(name):
    return f'{name}.aot.cj'


def dotpdba(name):
    return f'{name}.pdba'


def dotcbc(name):
    return f'{name}.cbc'


def dotactual(name):
    return f'{name}.actual'


def dotexpected(name):
    return f'{name}.expected'


def dotasm(name):
    return f'{name}.asm'


def dotchir(name):
    return f'{name}.chir'


def java_cmd():
    """Resolves the java executable path via JAVA_HOME or system PATH."""
    java_home = os.getenv('JAVA_HOME')
    if java_home:
        return os.path.join(java_home, 'bin', 'java')
    java_exec = shutil.which('java')
    if java_exec:
        return java_exec
    raise EnvironmentError('JAVA_HOME environment variable is not set and java is not found in PATH')


def java_perf_flags():
    """JVM flags that make short-lived compiler JVM runs faster.

    - C1-only JIT: C2 compilation of the compiler itself is a large share of CPU
      in short runs and pays off only for long-running JVMs.
    - AppCDS: the compiler loads ~8.5k classes per run; sharing the parsed class
      metadata via an archive cuts startup and classloading time roughly in half.
      The archive is (re)created automatically when missing/stale.
    """
    flags = [
        '-XX:TieredStopAtLevel=1',
        '-XX:+AutoCreateSharedArchive',
        '-XX:SharedArchiveFile=' + os.path.join(tempfile.gettempdir(), 'cbc-compiler.jsa'),
    ]
    # Allow switching JIT policy for experiments (e.g. CBC_RUNNER_FULLJIT=1)
    if os.environ.get('CBC_RUNNER_FULLJIT'):
        flags = [f for f in flags if f != '-XX:TieredStopAtLevel=1']
    return flags


def diff(actual, expected):
    return ['diff', '-u', expected, actual]
