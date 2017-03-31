#BN util

## Install

```
devtools::install_bitbucket("bnoperator/bnutil")
```
# git

https://bitbucket.org/bnoperator/pgcran

```
git add -A && git commit -m "Cube added" && git push
git tag -a 2.1 -m "Cube added" && git push --tags
```

# Publish a package on pamagene R repository


```
bntools::deployGitPackage('https://bitbucket.org/bnoperator/bnutil.git', '2.1')
```

# Test bnutil

```
devtools::test()
```
