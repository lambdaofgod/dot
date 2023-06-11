from_name="$USER@$HOSTNAME"
git add -f .doom.d/*el
git add -f .doom.d/util/*el
git commit -a -m "changes from $from_name: $(git status -s | grep org | tr '\n' ' ')"
git push
