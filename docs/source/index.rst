graphstruc
==========

**graphstruc** is a Fortran library providing efficient implementations of graph data structures and algorithms.

The library offers both dense and sparse graph representations with support for:

* Directed and undirected graphs
* Weighted edges
* Vertex and edge features
* Self-loops
* Compressed Sparse Row (CSR) format for efficient sparse storage

Quick Example
-------------

.. code-block:: fortran

   use graphstruc, only: graph_type
   type(graph_type) :: graph
   
   call graph%add_vertex(feature=[1.0, 2.0])
   call graph%add_edge(index=[1, 2], weight=1.5)
   call graph%convert_to_sparse()

.. toctree::
   :maxdepth: 1
   :caption: Contents
   
   installation
   quickstart
   user_guide/graphs
   user_guide/vertices
   user_guide/edges
   user_guide/operations
   user_guide/sparse_graphs
   Fortran API <api>

Indices
=======

* :ref:`genindex`
* :ref:`search`
