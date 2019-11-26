Syntax
======

Radon is a whitespace sensitive transpiled language mainly targetting C. 

Hello World
===========

.. code-block:: c

  main(argc: Int32, argv: Ptr<String>): Int32 =
    Console::printf("Hello %s!", "World")
    return 0

Functions
=========

Modules
=======

Modules namespace content under a specific name, begining with the `module` keyword and referenced via `::` for example

.. code-block:: c

  module Greeter =
    hello(who: String): Void = Console::printf("Hey %s", who);
  
  # Things outside module scope belong in a "Global" module.
  
  Greeter::hello("You")

Enums
=====

Structs
=======

Tagged Unions
=============

Type aliases
============

Importing/Aliasing
==================

Loops
=====

Control Flow
============

Pattern Matching
================

Inline Language Blocks
======================

Documentation Tags
==================

