---
date: 2023-12-31
tags: haskell programming
---

# Generating Standalone Haddock Docs for a Multi-package Haskell Project

So here's a thing I recently wanted to do. I am working on a personal Haskell project, and as the
project grew big, I split it up into multiple packages. Something like `project-core`, `project-backend`,
`project-frontend` and such. After writing some thousands of lines of code, I decided to put the
project documentation online. Now, this is not the kind of project that I'd publish as a bunch of
libraries on [Hackage](https://hackage.haskell.org/). Nevertheless, I wanted to publish the API docs and hyperlinked source code
online as a self-contained website. Well, turns out, this is a really hard to do with Haskell projects.

Haskell has [Haddock](http://www.haskell.org/haddock/), its documentation generation tool. It generates
the API docs and sources fine, but not for a self-contained website. You can run `cabal haddock-project` in the `--local` mode,
in which all the generated docs will link to other packages' local docs only. Or you can run it in
the `--hackage` mode, in which the
docs will link to other packages' docs on Hackage. In either case, all the hyperlinked source code
link only to other packages' local source code. I didn't want this.

What I wanted was to generate API docs and source code that linked to local ones only for the
packages in my project, and to the Hackage ones for all my project's dependency libraries. This just doesn't
seem possible with Haddock.

So I did what I had to. I summoned my old self from when I used to work as a Perl[^fn3] programmer
(my first job decades ago!), and I wrote
some regexes[^fn1] and a Bash script[^fn2]. It took some experimentation, but I finally got it working. Here it is
in its full glory:

[^fn3]: [Ahmm](https://www.xkcd.com/224).
[^fn1]: Thankfully, generated HTML files are uniform enough for regex substitutions.
[^fn2]: [Shellcheck](https://www.shellcheck.net/) is an absolute life saver when writing Bash scripts.

```bash
#!/usr/bin/env bash
set -euo pipefail

if [ $# -ne 2 ]; then
    echo "Usage: $0 <title> <output>"
    exit 1
fi

TITLE=$1
OUTPUT=$2

OS=$(uname -s)

if [ "$OS" = "Darwin" ]
then
    OS="osx"
elif [ "$OS" = "Linux" ]
then
    OS="linux"
else
    printf "OS not supported: %s\n" "$OS" >&2
    exit 1
fi

ARCH=$(uname -m)
GHC_VERSION=$(ghc --version | awk '{print $8}')
PKG_NAME=$(cat ./*.cabal | grep name | head -1 | awk '{print $2}')
PKG_VERSION=$(cat ./*.cabal | grep version | grep -v cabal | awk '{print $2}' | head -1)
BUILD_PATH="dist-newstyle/build/${ARCH}-${OS}/ghc-${GHC_VERSION}/${PKG_NAME}-${PKG_VERSION}"
DOC_PATH="${BUILD_PATH}/l/*/doc/html/${PKG_NAME}"

temp_files=()

cleanup() {
  for file in "${temp_files[@]}"; do
        rm "$file"
  done
}
trap cleanup EXIT INT TERM

bold_print() {
  echo -e "\033[1m$1\033[0m"
}

# build project
bold_print "Building project"
cabal build --enable-documentation

# generate docs
bold_print "Generating docs"
mkdir -p "${OUTPUT}"
cabal haddock --haddock-html \
    --haddock-quickjump \
    --haddock-hyperlink-source \
    --haddock-option="--use-index=../doc-index.html" \
    --haddock-option="--use-contents=../index.html" \
    --haddock-option="--base-url=.." \
    all
cp -f -r ${DOC_PATH}/ "${OUTPUT}"

# generate index
bold_print "Generating index"

CMD_FILE1=$(mktemp)
temp_files+=("$CMD_FILE1")

echo "set -euo pipefail" > "${CMD_FILE1}"
echo -n "haddock -t \"${TITLE}\" -o \"${OUTPUT}\" --quickjump --gen-index --gen-contents " >> "${CMD_FILE1}"
find ${DOC_PATH}/${PKG_NAME}.haddock -print0 | xargs -0 -I {} echo -n "--read-interface=${PKG_NAME},{} " >> "${CMD_FILE1}"
bash "${CMD_FILE1}"

# fix links in hyperlinked source and docs
GEN_SRC_PATH="${OUTPUT}/${PKG_NAME}/src/*.html"
GEN_DOC_PATH="${OUTPUT}/${PKG_NAME}/*.html"

bold_print "Fixing links to project subpackages"
# fix links to source files for project subpackages
perl -i -pe "s|file:[^\"]*?/${BUILD_PATH}/l/.*?/doc/html/${PKG_NAME}/src/||g" ${GEN_SRC_PATH}
# fix links to doc files for project subpackages
perl -i -pe "s|href=\"../${PKG_NAME}/|href=\"|g" ${GEN_DOC_PATH}

bold_print "Fixing links to project dependencies"
# fix links to doc files for project dependency libraries
perl -i -pe "s|href=\"../([^\"]+)/([^\"]+)|href=\"https://hackage.haskell.org/package/\$1/docs/\$2|g" ${GEN_DOC_PATH}

bold_print "Fixing links to libraries in Nix store"
# fix links to source files for GHC libraries stored in the Nix store
perl -i -pe "s|file:///nix/store/[\d\w]*-ghc-${GHC_VERSION}-doc/share/doc/ghc/html/libraries/([^\"]*)/src/([^\"]*)|https://hackage.haskell.org/package/\$1/docs/src/\$2|g" ${GEN_SRC_PATH}
# fix links to doc files for GHC libraries stored in the Nix store
perl -i -pe "s|file:///nix/store/[\d\w]*-ghc-${GHC_VERSION}-doc/share/doc/ghc/html/libraries/([^\"]*)/src|https://hackage.haskell.org/package/\$1/docs/src/|g" ${GEN_DOC_PATH}

# fix links to source files for Hackage libraries stored in the Nix store
perl -i -pe "s|file:///nix/store/[^\"]+-doc/share/doc/([^\"]+)/html/src/([^\"]+)|https://hackage.haskell.org/package/\$1/docs/src/\$2|g" ${GEN_SRC_PATH}
# fix links to doc files for Hackage libraries stored in the Nix store
perl -i -pe "s|file:///nix/store/[^\"]+-doc/share/doc/([^\"]+)/html/src|https://hackage.haskell.org/package/\$1/docs/src|g" ${GEN_DOC_PATH}

# generate Cabal package short ID to package name mapping
bold_print "Generating Cabal package short ID to package name mapping"
SHORT_IDS_FILE=$(mktemp)
temp_files+=("$SHORT_IDS_FILE")

grep -h -o -P "href=\"file://[^\"]+/\.cabal/store/ghc-${GHC_VERSION}/.+?/" ${GEN_SRC_PATH} ${GEN_DOC_PATH} | \
  sort -u | grep -h -o -P "\.cabal/store/ghc-${GHC_VERSION}/.+?/" | cut -d '/' -f 4 > "${SHORT_IDS_FILE}"

PKG_NAMES_FILE=$(mktemp)
temp_files+=("$PKG_NAMES_FILE")

grep -h -o -P "href=\"file://[^\"]+/\.cabal/store/ghc-${GHC_VERSION}/.+?/" ${GEN_SRC_PATH} ${GEN_DOC_PATH} | sort -u | \
  cut -c 14- | xargs -I {} cat {}cabal-hash.txt | grep "pkgid:" | cut -d ' ' -f 2 > "${PKG_NAMES_FILE}"

CMD_FILE2=$(mktemp)
temp_files+=("$CMD_FILE2")

echo "set -euo pipefail" > "${CMD_FILE2}"
paste -d " " "${SHORT_IDS_FILE}" "${PKG_NAMES_FILE}" | awk "{print \"perl -i -pe \\\"s|\"\$1\"|\"\$2\"|g\\\" ${GEN_SRC_PATH} ${GEN_DOC_PATH}\"}" >> "${CMD_FILE2}"
bash "${CMD_FILE2}"

# fix links to doc and source files of libraries stored in the Cabal store
bold_print "Fixing links to libraries in Cabal store"
perl -i -pe "s|href=\"file://[^\"]+/\.cabal/store/ghc-${GHC_VERSION}/(.+?)/share/doc/html/([^\"]+)|href=\"https://hackage.haskell.org/package/\$1/docs/src/\$2|g" ${GEN_DOC_PATH}
perl -i -pe "s|href=\"file://[^\"]+/\.cabal/store/ghc-${GHC_VERSION}/(.+?)/share/doc/html/src|href=\"https://hackage.haskell.org/package/\$1/docs/src/|g" ${GEN_SRC_PATH}

rm "${OUTPUT}/${PKG_NAME}/${PKG_NAME}.haddock"
bold_print "Done"
```

You can invoke this script in your project's root directory like so:

```
bash standalone-haddock.sh "My wonderful project" docs
```

It will generate correctly linked and self-contained docs in the `docs` directory. Now you
can publish this directory on Github pages, or wherever you want.

I hope this script is useful to you. If you know better ways of accomplishing this task, please
let me know.

You can like, share or comment on this post on [Mastodon](https://fantastic.earth/@abnv/111675766745919140){:class="mastodon-link"}.
