"""Small deterministic 1,280-D decoder contract; no GPU or fixture download."""

from __future__ import annotations

import csv
import gzip
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import struct
import unittest


MODULE = Path(__file__).resolve().parents[2] / "inst/python/cellvit_partition_features.py"
SPEC = importlib.util.spec_from_file_location("cellvit_partition_features", MODULE)
assert SPEC and SPEC.loader
decoder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(decoder)


class FeatureMatrixTest(unittest.TestCase):
    def bundle(self):
        columns = [f"feature_{i:04d}" for i in range(1280)]
        matrix = bytearray()
        index = io.StringIO(newline="")
        writer = csv.writer(index)
        writer.writerow(["cell_id", "source_id", "x", "y", "row_sha256"])
        wanted = {}
        for row in range(3):
            meta = [f"cell-{row}", f"local-{row}", str(row + 1), str(row + 2)]
            values = [row + column / 2048 for column in range(1280)]
            packed = struct.pack("<1280f", *values)
            matrix.extend(packed)
            binding = json.dumps(meta, ensure_ascii=False, separators=(",", ":")).encode()
            writer.writerow([*meta, hashlib.sha256(binding + b"\0" + packed).hexdigest()])
            if row != 1:
                wanted[row] = {"cell_id": meta[0], "source_id": f"shard:{meta[1]}",
                               "x": row + 101.0, "y": row + 202.0}
        compressed_matrix = gzip.compress(bytes(matrix))
        compressed_index = gzip.compress(index.getvalue().encode())
        manifest = {"schema": "cellvit-float32-feature-matrix", "version": 1,
                    "dtype": "float32-le", "feature_count": 1280, "row_count": 3,
                    "columns": ["cell_id", "source_id", "x", "y", *columns],
                    "source": {"sha256": "a" * 64},
                    "matrix": {"sha256": hashlib.sha256(compressed_matrix).hexdigest()},
                    "index": {"sha256": hashlib.sha256(compressed_index).hexdigest()}}
        bundle = {decoder.MATRIX: compressed_matrix, decoder.ROW_INDEX: compressed_index,
                  decoder.MANIFEST: json.dumps(manifest).encode()}
        return columns, wanted, bundle

    def test_selected_rows_are_exact_float32_and_keep_requested_identity(self):
        columns, wanted, bundle = self.bundle()
        result = decoder.rows_from_bundle({"source_csv_sha256": "a" * 64}, bundle,
                                          columns, wanted, {"id": "shard", "cell_count": 3},
                                          [100, 200, 200, 300])
        self.assertEqual(list(result), ["cell-0", "cell-2"])
        self.assertEqual(len(result["cell-0"]), 1280)
        self.assertEqual(result["cell-2"][1279], struct.unpack("<f", struct.pack("<f", 2 + 1279 / 2048))[0])

    def test_row_identity_digest_rejects_changed_vector(self):
        columns, wanted, bundle = self.bundle()
        damaged = bytearray(gzip.decompress(bundle[decoder.MATRIX]))
        damaged[0] ^= 1
        bundle[decoder.MATRIX] = gzip.compress(bytes(damaged))
        manifest = json.loads(bundle[decoder.MANIFEST])
        manifest["matrix"]["sha256"] = hashlib.sha256(bundle[decoder.MATRIX]).hexdigest()
        bundle[decoder.MANIFEST] = json.dumps(manifest).encode()
        with self.assertRaisesRegex(ValueError, "row digest"):
            decoder.rows_from_bundle({"source_csv_sha256": "a" * 64}, bundle,
                                     columns, wanted, {"id": "shard", "cell_count": 3},
                                     [100, 200, 200, 300])


if __name__ == "__main__":
    unittest.main()
