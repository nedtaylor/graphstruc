Installation
============

graphstruc can be built using either the Fortran Package Manager (fpm) or CMake.

Requirements
------------

* **Compiler**: GCC gfortran 14.1.0 or later
* **Build tool**: fpm or CMake 3.17.5+
* **Dependencies**: coreutils (fetched automatically)

Using fpm (Recommended)
-----------------------

The simplest way to use graphstruc is as a dependency in your fpm project.

As a Dependency
~~~~~~~~~~~~~~~

Add to your project's ``fpm.toml``:

.. code-block:: toml

   [dependencies]
   graphstruc = { git = "https://github.com/nedtaylor/graphstruc.git" }

Then use in your code:

.. code-block:: fortran

   use graphstruc, only: graph_type

fpm will automatically fetch and build graphstruc and its dependencies.

Direct Installation
~~~~~~~~~~~~~~~~~~~

To build graphstruc directly:

.. code-block:: bash

   git clone https://github.com/nedtaylor/graphstruc.git
   cd graphstruc
   fpm build

Run the test suite:

.. code-block:: bash

   fpm test

Run the example programme:

.. code-block:: bash

   fpm run --example

Using CMake
-----------

For projects using CMake:

.. code-block:: bash

   git clone https://github.com/nedtaylor/graphstruc.git
   cd graphstruc
   mkdir build && cd build
   cmake ..
   make

Run tests:

.. code-block:: bash

   make test

Verifying Installation
----------------------

Test your installation with this simple programme:

.. code-block:: fortran

   program test
     use graphstruc, only: graph_type
     implicit none
     
     type(graph_type) :: g
     
     call g%add_vertex(feature=[1.0])
     write(*,*) 'Success! Vertices:', g%num_vertices
     call g%clear()
   end program

Compile and run:

.. code-block:: bash

   fpm run test
