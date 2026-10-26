#!/usr/bin/env python3
"""Read only explicitly selected CellViT float32 rows from an immutable v3 index.

The browser never calls this helper. The R prediction path invokes it for at
most one bounded reviewed-cell batch. Each source partition is checked before
its small matrix is decoded; no whole-slide feature table is materialized.
"""
from __future__ import annotations

import argparse
import csv
import gzip
import hashlib
import io
import json
import math
from pathlib import Path
import sqlite3
import struct
import tarfile

MAX_REQUEST = 2000
MAX_PARTITION_BYTES = 256 * 1024 * 1024
MAX_SOURCE_BYTES = 8 * 1024 * 1024
MATRIX = "cellvit_feature_matrix/features.f32.gz"
ROW_INDEX = "cellvit_feature_matrix/row_index.csv.gz"
MANIFEST = "cellvit_feature_matrix/manifest.json"
MEMBERS = (MATRIX, ROW_INDEX, MANIFEST)


def require(condition, message):
    if not condition:
        raise ValueError(message)


def sha256_bytes(data):
    return hashlib.sha256(data).hexdigest()


def digest(path):
    hash_value = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            hash_value.update(block)
    return hash_value.hexdigest()


def read_json(path):
    path = Path(path)
    require(path.is_file() and path.stat().st_size <= MAX_SOURCE_BYTES,
            "Missing or oversized CellViT source manifest")
    value = json.loads(path.read_text(encoding="utf-8"))
    require(isinstance(value, dict), "CellViT source must be a JSON object")
    return value


def within(root, relative):
    require(isinstance(relative, str) and relative and not Path(relative).is_absolute(),
            "CellViT storage reference must be relative")
    path = (root / relative).resolve()
    require(path.is_relative_to(root.resolve()), "CellViT storage reference escapes its project")
    return path


def checked_reference(root, reference, member_name):
    require(isinstance(reference, dict) and type(reference.get("bytes")) is int and
            0 <= reference["bytes"] and
            isinstance(reference.get("sha256"), str) and len(reference["sha256"]) == 64,
            "CellViT matrix reference has no size/hash")
    require(reference["bytes"] <= MAX_PARTITION_BYTES,
            "CellViT matrix member exceeds the bounded partition budget")
    storage = reference.get("storage")
    if storage == "file":
        path = within(root, reference.get("path"))
        require(path.is_file() and path.stat().st_size == reference["bytes"] and
                digest(path) == reference["sha256"],
                "CellViT matrix file differs from the manifest")
        return path.read_bytes()
    require(storage == "archive-member" and reference.get("member") == member_name,
            "CellViT matrix archive member differs from the manifest")
    return None


def checked_bundle(root, entry):
    storage = entry.get("feature_storage")
    require(isinstance(storage, dict) and
            storage.get("schema") == "cellphenotyper-cellvit-feature-storage-v1",
            "CellViT partition has no versioned feature storage")
    references = storage.get("references")
    require(isinstance(references, dict) and set(references) >= set(MEMBERS),
            "CellViT partition feature members are incomplete")
    data = {}
    archived = [name for name in MEMBERS if references[name].get("storage") == "archive-member"]
    if archived:
        require(len(archived) == len(MEMBERS), "Mixed archive/direct matrix members are unsupported")
        names = {references[name].get("archive") for name in MEMBERS}
        require(len(names) == 1, "CellViT matrix members belong to different archives")
        archive_path = within(root, next(iter(names)))
        declared = entry.get("lossless_archive")
        require(isinstance(declared, dict) and declared.get("path") == str(archive_path.relative_to(root)) and
                archive_path.is_file() and digest(archive_path) == declared.get("sha256"),
                "CellViT matrix archive differs from its immutable source")
        with tarfile.open(archive_path, "r:gz") as archive:
            for name in MEMBERS:
                ref = references[name]
                checked_reference(root, ref, name)
                item = archive.getmember(name)
                require(item.isfile() and item.size == ref["bytes"],
                        "CellViT archived matrix member size differs")
                stream = archive.extractfile(item)
                require(stream is not None, "CellViT archived matrix member is unreadable")
                with stream:
                    data[name] = stream.read(MAX_PARTITION_BYTES + 1)
                require(len(data[name]) == ref["bytes"] and
                        sha256_bytes(data[name]) == ref["sha256"],
                        "CellViT archived matrix member hash differs")
    else:
        for name in MEMBERS:
            data[name] = checked_reference(root, references[name], name)
    return storage, data


def bounded_gzip(data, maximum, label):
    with gzip.GzipFile(fileobj=io.BytesIO(data)) as stream:
        decoded = stream.read(maximum + 1)
        require(len(decoded) <= maximum, f"CellViT {label} exceeds its decoded byte budget")
        require(not stream.read(1), f"CellViT {label} has excess decoded content")
    return decoded


def rows_from_bundle(storage, bundle, feature_columns, wanted, partition, input_xyxy):
    manifest = json.loads(bundle[MANIFEST].decode("utf-8"))
    feature_count = len(feature_columns)
    require(manifest.get("schema") == "cellvit-float32-feature-matrix" and
            manifest.get("version") == 1 and manifest.get("dtype") == "float32-le" and
            manifest.get("feature_count") == feature_count and
            manifest.get("columns") == ["cell_id", "source_id", "x", "y", *feature_columns] and
            manifest.get("row_count") == partition.get("cell_count") and
            manifest.get("source", {}).get("sha256") == storage.get("source_csv_sha256") and
            manifest.get("matrix", {}).get("sha256") == sha256_bytes(bundle[MATRIX]) and
            manifest.get("index", {}).get("sha256") == sha256_bytes(bundle[ROW_INDEX]),
            "CellViT matrix schema, columns or source digest differs")
    row_count = manifest["row_count"]
    require(type(row_count) is int and row_count >= 0 and
            row_count * feature_count * 4 <= MAX_PARTITION_BYTES,
            "CellViT matrix exceeds the bounded partition budget")
    matrix = bounded_gzip(bundle[MATRIX], row_count * feature_count * 4, "matrix")
    require(len(matrix) == row_count * feature_count * 4,
            "CellViT matrix byte count differs from its declared dimensions")
    row_index = bounded_gzip(bundle[ROW_INDEX], MAX_PARTITION_BYTES, "row index")
    with io.StringIO(row_index.decode("utf-8"), newline="") as stream:
        index = csv.reader(stream)
        require(next(index) == ["cell_id", "source_id", "x", "y", "row_sha256"],
                "CellViT matrix row-index header differs")
        results = {}
        for ordinal, row in enumerate(index):
            require(ordinal < row_count and len(row) == 5,
                    "CellViT matrix row index exceeds its declared shape")
            if ordinal not in wanted:
                continue
            meta = row[:4]
            packed = matrix[ordinal * feature_count * 4:(ordinal + 1) * feature_count * 4]
            binding = json.dumps(meta, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
            require(sha256_bytes(binding + b"\0" + packed) == row[4],
                    "CellViT matrix row digest differs from immutable identity")
            indexed = wanted[ordinal]
            require(meta[0] == indexed["cell_id"] and
                    f"{partition['id']}:{meta[1]}" == indexed["source_id"] and
                    (float(meta[2]) + input_xyxy[0], float(meta[3]) + input_xyxy[1]) ==
                    (indexed["x"], indexed["y"]),
                    "CellViT matrix row ID or coordinates differ from the global index")
            values = struct.unpack("<" + "f" * feature_count, packed)
            require(all(map(math.isfinite, values)), "CellViT matrix has nonfinite features")
            results[meta[0]] = values
        require(ordinal + 1 == row_count if row_count else True,
                "CellViT matrix row-index population differs")
    require(len(results) == len(wanted), "Selected CellViT feature rows are missing")
    return results


def decode(index_path, request):
    root = index_path.parent.resolve()
    require(digest(index_path) == request.get("index_sha256"),
            "CellViT index changed since its project was opened")
    source = within(root, request.get("partition_manifest_path"))
    require(digest(source) == request.get("partition_manifest_sha256"),
            "CellViT partition manifest changed since project reopen")
    manifest = read_json(source)
    require(manifest.get("schema") == "cellphenotyper-cellvit-shards-v1" and
            isinstance(manifest.get("partitions"), list) and
            len(manifest["partitions"]) == manifest.get("partition_count"),
            "CellViT partition manifest has an unsupported shape")
    ids = request.get("cell_ids")
    require(isinstance(ids, list) and 0 < len(ids) <= MAX_REQUEST and
            all(isinstance(value, str) and value for value in ids) and
            len(set(ids)) == len(ids), "Select one bounded unique cell-ID batch")
    uri = index_path.resolve().as_uri() + "?mode=ro"
    with sqlite3.connect(uri, uri=True) as connection:
        connection.row_factory = sqlite3.Row
        connection.execute("PRAGMA query_only=ON")
        connection.execute("PRAGMA trusted_schema=OFF")
        require(not connection.execute(
            "SELECT 1 FROM sqlite_master WHERE type IN ('view','trigger') LIMIT 1").fetchone(),
            "CellViT index contains an unsupported executable SQLite object")
        metadata = {key: json.loads(value) for key, value in
                    connection.execute("SELECT key,value FROM metadata")}
        require(metadata.get("schema") == "cellphenotyper-cell-index" and
                metadata.get("schema_version") == 3 and
                metadata.get("feature_storage") == "partition-row-reference-v1" and
                metadata.get("classification_sha256") == request["partition_manifest_sha256"] and
                metadata.get("cell_count") == manifest.get("cell_count") and
                metadata.get("image") == manifest.get("image"),
                "CellViT index metadata differs from the complete source")
        columns = metadata["feature_set"]["feature_columns"]
        require(columns == request.get("feature_columns") and
                len(columns) == metadata["feature_set"]["descriptor"]["feature_count"],
                "CellViT feature columns differ from the selected representation")
        query = ("SELECT c.cell_id,c.source_id,c.x,c.y,f.values_json FROM cells c "
                 "JOIN features f ON f.row_id=c.row_id WHERE c.cell_id IN (" +
                 ",".join("?" for _ in ids) + ")")
        selected = connection.execute(query, ids).fetchall()
        require(len(selected) == len(ids), "One or more selected CellViT IDs are absent")
    entries = {entry["id"]: entry for entry in manifest["partitions"]}
    require(len(entries) == len(manifest["partitions"]),
            "CellViT partition IDs are duplicated")
    grouped = {}
    for row in selected:
        locator = json.loads(row["values_json"])
        require(isinstance(locator, dict) and set(locator) == {"partition_id", "row"} and
                locator["partition_id"] in entries and type(locator["row"]) is int and
                0 <= locator["row"] < entries[locator["partition_id"]]["cell_count"],
                "CellViT feature locator is invalid")
        grouped.setdefault(locator["partition_id"], {})[locator["row"]] = dict(row)
    results = {}
    for partition_id, wanted in grouped.items():
        entry = entries[partition_id]
        storage, bundle = checked_bundle(root, entry)
        results.update(rows_from_bundle(storage, bundle, columns, wanted, entry, entry["input_xyxy"]))
    require(set(results) == set(ids), "CellViT selected row identities differ from request")
    return {"schema": "wsitools-cellvit-selected-features-v1",
            "cell_ids": ids, "feature_columns": columns,
            "rows": [list(results[cell_id]) for cell_id in ids]}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--index", type=Path, required=True)
    parser.add_argument("--request", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    request = read_json(args.request)
    result = decode(args.index.resolve(), request)
    with args.output.open("x", encoding="utf-8") as output:
        json.dump(result, output, separators=(",", ":"), allow_nan=False)
        output.write("\n")


if __name__ == "__main__":
    main()
