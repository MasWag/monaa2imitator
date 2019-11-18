monaa2imitator
==============

[![GitHub Actions Badge](https://github.com/MasWag/monaa2imitator/workflows/hspec/badge.svg)](https://github.com/MasWag/monaa2imitator/actions?query=workflow%3Ahspec)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](./LICENSE)
[![Docker Hub](https://img.shields.io/docker/cloud/build/maswag/monaa2imitator)](https://hub.docker.com/repository/docker/maswag/monaa2imitator)

Translate [MONAA](https://github.com/MasWag/monaa)'s dot language to the modeling language of [IMITATOR](https://www.imitator.fr).

Usage
-----

### Synopsis

```bash
stack run < [MONAA TA].dot > [IMIATOR model].imi
docker run -i maswag/monaa2imitator < [MONAA TA].dot > [IMIATOR model].imi
```

### Example

```bash
stack run < ./example/small.dot > small.imi
docker run -i maswag/monaa2imitator < ./example/small.dot > small.imi
```

Requirement
===========

- stack
