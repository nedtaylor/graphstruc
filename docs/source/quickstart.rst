Quick Start
===========

This guide demonstrates the essential features of graphstruc to get you started quickly.

Basic Usage
-----------

Creating a simple undirected graph with vertices and edges:

.. code-block:: fortran

   use graphstruc, only: graph_type
   type(graph_type) :: graph
   
   ! Add vertices with 2D feature vectors
   call graph%add_vertex(feature=[1.0, 2.0])
   call graph%add_vertex(feature=[3.0, 4.0])
   
   ! Connect vertices with a weighted edge
   call graph%add_edge(index=[1, 2], weight=1.5)
   
   ! Query graph properties
   write(*,*) graph%num_vertices, graph%num_edges
   
   ! Clean up when finished
   call graph%clear()

The first vertex you add determines the feature vector size for all subsequent vertices.

Directed Graphs
---------------

For directed graphs, set the ``directed`` property and use negative indices for the target vertex:

.. code-block:: fortran

   graph%directed = .true.
   
   ! Create directed edge from vertex 1 to vertex 2
   call graph%add_edge(index=[1, -2])  ! 1 → 2
   
   ! Alternatively, use the directed parameter
   call graph%add_edge(index=[1, 2], directed=.true.)

This convention allows you to have both directed and undirected edges in the same graph.

Sparse Storage
--------------

For large or sparse graphs, convert to Compressed Sparse Row (CSR) format for better memory efficiency:

.. code-block:: fortran

   ! Build graph in default dense format
   do i = 1, 1000
     call graph%add_vertex()
   end do
   
   ! Add edges
   do i = 1, 5000
     call graph%add_edge(index=[src(i), tgt(i)])
   end do
   
   ! Convert to sparse format (O(V+E) memory instead of O(V²))
   call graph%convert_to_sparse()

Sparse format is particularly beneficial when the number of edges is much smaller than V².

Common Operations
-----------------

Removing vertices and edges:

.. code-block:: fortran

   ! Remove multiple vertices (and their connected edges)
   call graph%remove_vertices([2, 4])
   
   ! Remove specific edges
   call graph%remove_edges([1, 3])

Working with self-loops:

.. code-block:: fortran

   ! Add self-loops to all vertices
   call graph%add_self_loops()
   
   ! Add to specific vertices with custom weight
   call graph%add_self_loops(indices=[1, 3, 5], weight=2.0)

Calculating vertex degrees:

.. code-block:: fortran

   ! Calculate degree for all vertices
   call graph%calculate_degree()
   
   ! Access individual vertex degrees
   write(*,*) 'Vertex 1 degree:', graph%vertex(1)%degree

Copying graphs:

.. code-block:: fortran

   ! Create a duplicate graph
   call graph2%copy(graph1)
   
   ! Copy and convert format simultaneously
   call graph2%copy(graph1, sparse=.true.)

Next Steps
----------

For more detailed information:

* :doc:`user_guide/graphs` - Graph types and storage formats
* :doc:`user_guide/vertices` - Vertex operations and features
* :doc:`user_guide/edges` - Edge operations and weights
* :doc:`api` - Complete API reference
