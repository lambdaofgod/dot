import git
import jinja2
import fire


def main(repo_name):
    repo = git.Repo(".")
    user_name = repo.config_reader().get("user", "name")
    user_email = repo.config_reader().get("user", "email")

    env = jinja2.Environment(loader=jinja2.FileSystemLoader("templates"))

    template = env.get_template("setup.py")
    print(template.render(repo_name=repo_name, user_name=user_name, user_email=user_email))


if __name__ == "__main__":
    fire.Fire(main)
