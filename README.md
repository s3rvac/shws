SHWS - Simple Haskell Web Server
================================

SHWS is a simple web server written in Haskell.

Features
--------

* handles HTTP/1.0 and HTTP/1.1 (persistent) connections and requests
* concurrent (can handle multiple connections at the same time)
* configurable via a configuration file
* logging
* client connection timeout

Limitations
-----------

* only a basic subset of HTTP/1.0 and HTTP/1.1 is implemented (for example,
  only GET and HEAD methods are implemented)
* large files transfer can cause memory exhaustion

Requirements
============

* ghc (6.8) with the following additional packages and modules:
  * html (1.0.1.1)
  * http (3001.0.4)
  * hunit (1.2.0.0) - optional
  * network (2.1.0.0)
  * parallel (1.0.0.0)
  * regex (0.93.1)
  * stream (0.2.2)
  * time (1.1.2.0)
* haddock (0.8) for generating documentation - optional

Compilation
===========

* simply run `make` in the project's root directory
* to compile the project tester, run `make test` instead

Synopsis
========

```
./shws -f|--config-file CONFIG_FILE_PATH

CONF_FILE_PATH - path to the configuration file (e.g. shws.cfg)
```

Usage examples
==============

```
./shws -f ./default.cfg
```

Tested on
=========

Debian 5.0, kernel 2.6.28, x86_64

Author
======

Petr Zemek <s3rvac@gmail.com>, 2009

License
=======

Copyright (C) 2009 Petr Zemek <s3rvac@gmail.com>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
USA.
