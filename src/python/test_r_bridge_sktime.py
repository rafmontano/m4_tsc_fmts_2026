import unittest

import numpy as np

from src.python.r_bridge_sktime import get_model_split


class ModelSplitTest(unittest.TestCase):
    def test_runner_model_names_use_capped_split_and_preserve_test_rows(self):
        split_index = {
            "uncap": {"train": [1, 3, 5, 8], "test": [2, 7]},
            "rocket": {"train": [3, 8], "test": [2, 7]},
            "inceptiontime": {"train": [1, 5], "test": [2, 7]},
        }
        for model, expected in [
            ("ROCKET", [2, 7]),
            ("rocket", [2, 7]),
            ("InceptionTime", [0, 4]),
            ("RotF", [0, 2, 4, 7]),
        ]:
            with self.subTest(model=model):
                result = get_model_split(split_index, model)
                np.testing.assert_array_equal(result["train"], expected)
                np.testing.assert_array_equal(result["test"], [1, 6])


if __name__ == "__main__":
    unittest.main()
