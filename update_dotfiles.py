import fire
import shutil
import subprocess
import os
from pathlib import Path as P


class DoomSettings:

    emacs_dir = P("~/.emacs.d").expanduser()
    doom_config_dir = P("~/.doom.d").expanduser()
    repo_config_dir = "dot.doom.d"

    @classmethod
    def get_doom_bin(cls):
        return cls.emacs_dir / "bin" / "doom"


class Main:
    class doom:
        """
        TODO:
        update local/repo config
        """

        @staticmethod
        def update_local(
            doom_config_dir=DoomSettings.doom_config_dir,
            repo_config_dir=DoomSettings.repo_config_dir,
            backup_dir="~/.bkp.doom.d",
        ):
            """
            backup files from `doom_config_dir` to `backup_dir`
            copy files from `repo_config_dir` to `doom_config_dir`
            """
            if P(backup_dir).exists():
                shutil.rmtree(backup_dir)
            shutil.copytree(doom_config_dir, backup_dir)
            for config_file in P(repo_config_dir).rglob("*.el"):
                shutil.copy(config_file, doom_config_dir)

        @staticmethod
        def update_repo(
            doom_config_dir=DoomSettings.doom_config_dir,
            repo_config_dir=DoomSettings.repo_config_dir,
            backup_dir="bkp.dot.doom.d",
        ):
            """
            backup files from `doom_config_dir` to `backup_dir`
            copy files from `repo_config_dir` to `doom_config_dir`
            """
            if P(backup_dir).exists():
                shutil.rmtree(backup_dir)
            shutil.copytree(repo_config_dir, backup_dir)
            for config_file in P(doom_config_dir).rglob("*.el"):
                shutil.copy(config_file, repo_config_dir)

        @staticmethod
        def run(doom_command):
            """
            run doom install/sync
            """
            assert doom_command in ["install", "sync"], "unsupported command"
            subprocess.check_call([DoomSettings.get_doom_bin(), doom_command])


if __name__ == "__main__":
    fire.Fire(Main)
