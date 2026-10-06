import os
import shutil
import tempfile
import time
import atexit


# -----------------------------------------------------------------------------
# Per-command timing instrumentation (comp-time investigation).
#
# Timings are always collected (cheap); they are printed at the end of the run.
# Commands are classified by the executable name: cjc, cbc-compiler.jar (java),
# cbc-asm.jar (java), launcher, diff, other. Totals per class + build/run
# chunk totals + grand total are reported.
# -----------------------------------------------------------------------------


class _Timings:
    def __init__(self):
        self.lock = __import__('asyncio').Lock()
        self.per_command = {}          # class -> [total_seconds, count]
        self.build_total = 0.0
        self.build_count = 0
        self.run_total = 0.0
        self.run_count = 0
        self.other_total = 0.0         # diff etc
        self.t0 = time.monotonic()

    async def add(self, cmd_class: str, seconds: float, chunk: str):
        async with self.lock:
            slot = self.per_command.setdefault(cmd_class, [0.0, 0])
            slot[0] += seconds
            slot[1] += 1
            if chunk == 'build':
                self.build_total += seconds
                self.build_count += 1
            elif chunk == 'run':
                self.run_total += seconds
                self.run_count += 1
            else:
                self.other_total += seconds

    def report(self) -> str:
        total = time.monotonic() - self.t0
        lines = ['[Timings] command classes (wall time, sequential-equivalent):']
        for cls, (secs, cnt) in sorted(self.per_command.items(), key=lambda kv: -kv[1][0]):
            lines.append(f'  {cls:<28} {secs:8.2f} s  ({cnt} calls)')
        if self.other_total > 0:
            lines.append(f'  {"diff/other":<28} {self.other_total:8.2f} s')
        lines.append(f'  build chunk total: {self.build_total:8.2f} s ({self.build_count} commands)')
        lines.append(f'  run chunk total:   {self.run_total:8.2f} s ({self.run_count} commands)')
        lines.append(f'  measured commands: {self.build_total + self.run_total + self.other_total:8.2f} s')
        lines.append(f'  wall total (incl. python overhead): {total:8.2f} s')
        return '\n'.join(lines)


Timings = _Timings()


def classify_cmd(cmd) -> str:
    """Classify a command list into a reporting bucket by executable."""
    if not cmd:
        return 'other'
    exe = os.path.basename(str(cmd[0]))
    if exe == 'cjc':
        return 'cjc'
    if exe in ('launcher',):  # run chunk
        return 'launcher'
    if exe == 'diff':
        return 'diff'
    if exe == 'java' or exe.endswith('java'):
        for a in cmd[1:]:
            base = os.path.basename(str(a))
            if 'cbc-compiler.jar' in base:
                return 'cbc-compiler (java)'
            if 'cbc-asm.jar' in base:
                return 'cbc-asm (java)'
        return 'java (unclassified)'
    if exe in ('bash', 'sh', 'tool.sh') and len(cmd) > 1:
        # tool.sh wrapper: classify by inner command
        return classify_cmd(cmd[1:])
    return exe



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

def dotjsa(name):
    return f'{name}.jsa'


def java_cmd(cds_archive):
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
    if cds_archive and java_version and java_version >= 21:
        args.extend([
            '-XX:TieredStopAtLevel=1',
            '-XX:+AutoCreateSharedArchive',
            f'-XX:SharedArchiveFile={cds_archive}',
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
