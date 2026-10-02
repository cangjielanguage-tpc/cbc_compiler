import os
import shutil


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


def dotobfdotmap(name):
    return f'{name}.obf.map'

def java_cds_archive():
    return '/tmp/cbc-compiler.jsa'


def java_cmd():
    """Resolves the java executable path via JAVA_HOME or system PATH."""
    java_home = os.getenv('JAVA_HOME')
    if java_home:
        base = os.path.join(java_home, 'bin', 'java')
    else:
        java_exec = shutil.which('java')
        if java_exec:
            base = java_exec
        else:
            raise EnvironmentError('JAVA_HOME environment variable is not set and java is not found in PATH')

    java_version = _get_java_major_version(base)
    args = [base]
    if java_version and java_version >= 21:
        args.extend([
            '-XX:TieredStopAtLevel=1',
            '-XX:+AutoCreateSharedArchive',
            f'-XX:SharedArchiveFile={java_cds_archive()}',
        ])
    return args


def _get_java_major_version(java_exec):
    """Get the major version of a Java executable, or None on failure."""
    try:
        import subprocess
        result = subprocess.run(
            [java_exec, '-version'],
            capture_output=True,
            text=True,
            timeout=10,
        )
        # Output format: java version "11.0.x" or openjdk version "21.x.x"
        output = result.stderr
        import re
        match = re.search(r'version"?(?:\s+"(\d+))', output)
        if match:
            return int(match.group(1))
    except Exception:
        pass
    return None


def diff(actual, expected):
    return ['diff', '-u', expected, actual]
