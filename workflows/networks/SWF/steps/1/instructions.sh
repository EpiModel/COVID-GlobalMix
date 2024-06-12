. /projects/epimodel/spack/share/spack/setup-env.sh
spack unload -a
spack load r@4.3.2
CUR_BRANCH=$(git rev-parse --abbrev-ref HEAD)
if [[ "$CUR_BRANCH" != "main" ]]; then
echo 'The git branch is not `main`.)
Exiting' 1>&2
exit 1
fi
git pull
Rscript -e "renv::init(bare = TRUE)"
Rscript -e "renv::restore()"
