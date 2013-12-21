===========
microparser
===========
----------------------------------------------------
a hacky dorty C++/JUCE helper to create LUCE classes
----------------------------------------------------

    Copyright |copy| 2012-2013, Peersuasive Technologies

    Licenced under the GPLv3 terms (http://www.gnu.org/licenses/gpl-3.0.html)


Preamble
========

    This script may parse a ``JUCE`` header class and output a ``LUCE`` binding
    class, though it may not.

    Most of the time, it will.

    This is just a helper script I maintain sporadically according to my own
    needs whithout any kind of roadmap.

    I wrote the base code some times ago to get a general understanding on
    PEG/LPEG, so I'd rather discourage anyone to use it for any other purpose
    than practicing, but feel free to use and modify it as you wish anyway :)

    Someday, as ``LUCE`` grows, I'll rewrite it from scratch.

Requirements
============

* `lua 5.1 <http://www.lua.org/download.html>`_ / `luajit 2.0.X <http://luajit.org/download.html>`_ (untested with 5.2)

* `LPEG 12.0 <http://www.inf.puc-rio.br/~roberto/lpeg/#download>`_

* `Luce <https://github.com/peersuasive/luce>`_, obviously

* GCC or CLANG (for preprocessing)


Usage
=====

    By default, the script creates a direct binding to a ``JUCE`` class but it
    can also generate a standalone wrapper to the ``JUCE`` class instead (see
    ``LComponent``, for instance).

with preprocessing (recommended)
--------------------------------

    ::
    
        create.sh <input> [class name] [as wrapper class] [output to screen]


.. note::

    environment variables ``CFLAGS`` and/or ``CXXFLAGS`` containing some ``JUCE`` definitions can be used
    by the preprocessor



without preprocessing
---------------------

    ::

        lua[jit] luce_binding.lua <input> [class name] [as wrapper class] [output to screen]



.. |copy|   unicode:: U+000A9 .. COPYRIGHT SIGN

.. vim:syntax=rst:filetype=rst:spelllang=en
