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


def java_cmd():
    """Resolves the java executable path via JAVA_HOME or system PATH."""
    java_home = os.getenv('JAVA_HOME')
    if java_home:
        return os.path.join(java_home, 'bin', 'java')
    java_exec = shutil.which('java')
    if java_exec:
        return java_exec
    raise EnvironmentError('JAVA_HOME environment variable is not set and java is not found in PATH')


def diff(actual, expected):
    return ['diff', '-u', expected, actual]
