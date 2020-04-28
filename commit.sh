cd C:\\Users\\CONNO\\OneDrive\\Documents\\GitHub\\COVID\\COVID.Rproj
git add --all
timestamp() {
  date +"at %H:%M:%S on %m/%d/%Y"
}
git commit -am "Regular auto-commit $(timestamp)"
git push origin master