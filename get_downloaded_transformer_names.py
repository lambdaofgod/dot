import os
import json
from pathlib import Path as P
import urllib
from functools import wraps
from types import GeneratorType
import pandas as pd
import numpy as np


pd.set_option("display.max_colwidth", 500)
pd.set_option("display.max_rows", 500)


def maybe_get_model_name(conf):
    if "url" in conf.keys():
        url = conf["url"]
        model_file_path = urllib.parse.urlparse(url).path
        model_name = model_file_path.split(os.sep)[1:3]
        return "/".join(model_name)


def get_size(file_path, unit):
    file_size = os.path.getsize(file_path)
    exponents_map = {'bytes': 0, 'kb': 1, 'mb': 2, 'gb': 3}
    if unit not in exponents_map:
        raise ValueError("Must select from \
        ['bytes', 'kb', 'mb', 'gb']")
    else:
        size = file_size / 1024 ** exponents_map[unit]
        return round(size, 3)


def get_transformers_model_names(cache_dir, unit):
    cache_dir_path = P(cache_dir).expanduser()
    print(cache_dir_path)
    config_files = P(cache_dir_path).rglob("*.json")
    for fname in config_files:
        with open(fname) as f:
            conf = json.load(f)
            maybe_model_name = maybe_get_model_name(conf)
            model_fname = P(str(fname).replace(".json", ""))
            try:
                model_size = get_size(model_fname, unit=unit) 
            except:
                model_size = np.nan
            if maybe_model_name:
                yield {"model_name": maybe_model_name, "path": model_fname.relative_to(cache_dir_path), f"model_size[{unit}]": model_size}


def get_downloaded_models_metadata(cache_dir="~/.cache/huggingface/transformers", unit="gb"):
    model_metadata = pd.DataFrame.from_records(get_transformers_model_names(cache_dir, unit))
    return model_metadata.dropna().sort_values(by=f"model_size[{unit}]", ascending=False).drop_duplicates(["model_name"])

if __name__ == "__main__":
    print(get_downloaded_models_metadata())
