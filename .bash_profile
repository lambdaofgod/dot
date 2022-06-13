parse_git_branch() {
  if [ -n "$(git rev-parse --git-dir 2> /dev/null)" ]; then
    echo "($(git rev-parse --abbrev-ref HEAD))"
  fi
}

get_git_basename() {
  if [ -n "$(git rev-parse --git-dir 2> /dev/null)" ]; then
  echo "$(basename -- $(git rev-parse --show-toplevel))"
  fi
}

# environment customization
export PS1="\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\w\[\033[01;34m\][\$(get_git_basename)]\$(parse_git_branch)\[\033[00m\]\$ "
