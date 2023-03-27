import os
import json
from pathlib import Path as P
import urllib
from functools import wraps
from types import GeneratorType


def listify(func):
    """decorator for making generator functions return a list instead"""

    @wraps(func)
    def new_func(*args, **kwargs):
        r = func(*args, **kwargs)
        return list(r)

    return new_func


def maybe_get_model_name(conf):
    if "url" in conf.keys():
        url = conf["url"]
        if "json" in url:
            model_file_path = urllib.parse.urlparse(url).path
            model_name = model_file_path.split(os.sep)[1:3]
            print("/".join(model_name))


@listify
def get_transformers_model_names(cache_dir="~/.cache/huggingface/transformers"):
    cache_dir_path = P(cache_dir).expanduser()
    print(cache_dir_path)
    config_files = P(cache_dir_path).rglob("*.json")
    for fname in config_files:
        with open(fname) as f:
            conf = json.load(f)
            maybe_model_name = maybe_get_model_name(conf)
            if maybe_model_name:
                yield maybe_model_name


if __name__ == "__main__":
    get_transformers_model_names()
