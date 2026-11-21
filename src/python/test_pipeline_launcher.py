"""Exercise the public launchers without running experiments or requiring GPUs."""

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

from src.python.get_sktime_models import get_sktime_models


ROOT = Path(__file__).resolve().parents[2]
PHASES = [
    "src/r/00_main_new.R",
    "src.python.run_tsc_experiment",
    "src.python.run_mantis_experiment",
    "src.python.run_tsc_chronos2",
    "src/r/sensitivity/99_sensitivity_run_all.R",
    "src/r/paper/run_all.R",
]
FAKE_COMMAND = r'''
import os
from pathlib import Path
import sys
from types import SimpleNamespace as NS

if Path(sys.argv[0]).name == "Rscript":
    target = sys.argv[1]
elif "-m" in sys.argv:
    target = sys.argv[sys.argv.index("-m") + 1]
else:
    code = sys.argv[sys.argv.index("-c") + 1]
    if "import glob" in code:
        if os.environ["TEST_LIBS"] == "1":
            environment = sys.argv[sys.argv.index("-n") + 1]
            print("/fake/" + environment + ("/bin" if "*/bin" in code else "/lib"))
    else:
        backend = os.environ["TEST_BACKEND"]
        sys.modules["torch"] = NS(
            __version__="test", version=NS(cuda="test"),
            cuda=NS(is_available=lambda: backend == "cuda",
                    get_device_name=lambda _: "test CUDA GPU"),
            backends=NS(mps=NS(is_available=lambda: backend == "mps")),
        )
        sys.modules["tensorflow"] = NS(
            __version__="test", test=NS(is_built_with_cuda=lambda: backend == "cuda"),
            config=NS(list_physical_devices=lambda _: ["test GPU"]),
        )
        exec(code)
    sys.exit(0)
with open("executed.txt", "a") as trace:
    trace.write(target + "\n")
'''


class PipelineLauncherTests(unittest.TestCase):
    def test_legacy_chronos_factory_is_not_available(self):
        with self.assertRaisesRegex(ValueError, "Unknown model_id"):
            get_sktime_models(model_id="CHRONOS")

    def run_pipeline(self, script, args=(), backend="mps", libraries=False):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name in ("run_paper_results.sh", "run_paper_results_from_step2.sh"):
                shutil.copyfile(ROOT / name, root / name)
            bin_dir = root / "bin"
            bin_dir.mkdir()
            for name in ("conda", "Rscript"):
                executable = bin_dir / name
                executable.write_text(f"#!{sys.executable}\n" + FAKE_COMMAND)
                executable.chmod(0o755)
            result = subprocess.run(
                ["bash", str(root / script), *args],
                cwd=bin_dir,
                env={**os.environ, "PATH": f"{bin_dir}:{os.environ['PATH']}",
                     "TEST_BACKEND": backend, "TEST_LIBS": str(int(libraries))},
                capture_output=True, text=True, timeout=30,
            )
            trace = root / "executed.txt"
            executed = trace.read_text().splitlines() if trace.exists() else []
            timings = list((root / "results").glob("runtime_*.tsv"))
            timing = timings[0].read_text() if timings else ""
            return result, executed, timing

    def test_gpu_backends_and_resume_use_the_same_pipeline(self):
        for backend in ("mps", "cuda"):
            for libraries in (False, True):
                for script, args, start in (
                    ("run_paper_results.sh", (), 0),
                    ("run_paper_results.sh", ("--from-step2",), 1),
                    ("run_paper_results_from_step2.sh", (), 1),
                ):
                    with self.subTest(backend=backend, libraries=libraries,
                                      script=script, args=args):
                        result, executed, timing = self.run_pipeline(
                            script, args, backend, libraries,
                        )
                        self.assertEqual(result.returncode, 0, result.stderr)
                        self.assertEqual(executed, PHASES[start:])
                        self.assertEqual("[1/6] R pipeline" in timing, start == 0)
                        self.assertIn("[6/6] Paper tables and figures", timing)
                        self.assertIn("CUDA" if backend == "cuda" else "MPS", result.stdout)

    def test_no_gpu_and_invalid_arguments_stop_before_foundation_work(self):
        result, executed, _ = self.run_pipeline(
            "run_paper_results.sh", ("--from-step2",), backend="cpu",
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(executed, PHASES[1:2])
        self.assertIn("cannot access a GPU", result.stderr)
        result, executed, _ = self.run_pipeline(
            "run_paper_results_from_step2.sh", ("--invalid",),
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(executed, [])


if __name__ == "__main__":
    unittest.main()
