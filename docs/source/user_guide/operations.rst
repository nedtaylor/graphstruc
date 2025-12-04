Operations
==========

This page summarises the key operations available in graphstruc.

Graph Management
----------------

Clearing
~~~~~~~~

Remove all data and reset the graph to an empty state:

.. code-block:: fortran

   call graph%clear()

This deallocates all arrays (vertices, edges, adjacency matrix) and resets all properties to their defaults. Use this when you're finished with a graph to free memory.

Copying
~~~~~~~

Create a duplicate of a graph:

.. code-block:: fortran

   type(graph_type) :: graph1, graph2
   
   ! Exact copy maintaining format
   call graph2%copy(graph1)
   
   ! Copy and convert to sparse
   call graph2%copy(graph1, sparse=.true.)
   
   ! Copy and convert to dense
   call graph2%copy(graph1, sparse=.false.)

The copy operation duplicates all vertices, edges, and properties.

Format Conversion
~~~~~~~~~~~~~~~~~

Convert between dense and sparse storage formats:

.. code-block:: fortran

   ! Convert to sparse (CSR) format
   call graph%convert_to_sparse()
   
   ! Convert to dense (adjacency matrix) format
   call graph%convert_to_dense()

You can convert at any time without losing data. See :doc:`sparse_graphs` for more details.

Vertex Operations
-----------------

Adding Vertices
~~~~~~~~~~~~~~~

.. code-block:: fortran

   ! Add single vertex with features
   call graph%add_vertex(feature=[1.0, 2.0])
   
   ! Add with custom ID
   call graph%add_vertex(feature=[1.0, 2.0], id=100)

Pre-allocating Vertices
~~~~~~~~~~~~~~~~~~~~~~~

For efficiency when adding many vertices:

.. code-block:: fortran

   ! Allocate space for 1000 vertices with 10 features
   call graph%set_num_vertices(1000, num_vertex_features=10)
   
   ! Then populate
   do i = 1, 1000
     graph%vertex(i)%feature = data(:,i)
   end do

Removing Vertices
~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   ! Remove one or more vertices
   call graph%remove_vertices([2, 5, 7])

This also removes all edges connected to these vertices and renumbers the remaining vertices.

Edge Operations
---------------

Adding Edges
~~~~~~~~~~~~

.. code-block:: fortran

   ! Simple edge
   call graph%add_edge(index=[1, 2])
   
   ! Weighted edge
   call graph%add_edge(index=[1, 2], weight=1.5)
   
   ! Edge with features
   call graph%add_edge(index=[1, 2], weight=1.5, feature=[0.1, 0.2])

Batch Edge Addition
~~~~~~~~~~~~~~~~~~~

Connect one vertex to many:

.. code-block:: fortran

   ! Connect vertex 1 to vertices 2, 3, 4
   call graph%set_edges(1, [2, 3, 4])

Removing Edges
~~~~~~~~~~~~~~

.. code-block:: fortran

   ! Remove one or more edges
   call graph%remove_edges([1, 3])
   
   ! For batch removal, defer adjacency updates
   do i = 1, n
     call graph%remove_edges([i], update_adjacency=.false.)
   end do
   call graph%generate_adjacency()

Self-Loops
----------

Adding Self-Loops
~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   ! Add to all vertices
   call graph%add_self_loops()
   
   ! Add to specific vertices with custom weight
   call graph%add_self_loops(indices=[1, 3, 5], weight=2.0)

Removing Self-Loops
~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   ! Remove from all vertices
   call graph%remove_self_loops()
   
   ! Remove from specific vertices
   call graph%remove_self_loops(indices=[2, 4])

Degree Calculation
------------------

Calculate vertex degrees:

.. code-block:: fortran

   ! Calculate for all vertices
   call graph%calculate_degree()
   
   ! Access individual degrees
   write(*,*) 'Vertex', i, 'degree:', graph%vertex(i)%degree
   
   ! Find high-degree vertices
   do i = 1, graph%num_vertices
     if (graph%vertex(i)%degree > 10) then
       write(*,*) 'Hub vertex:', i
     end if
   end do

For undirected graphs, degree is the total number of connections. For directed graphs, it's the number of outgoing edges.

Adjacency Matrix
----------------

Manually regenerate the adjacency matrix:

.. code-block:: fortran

   call graph%generate_adjacency()

This is normally called automatically when edges are added or removed, but you may need it after batch operations with ``update_adjacency=.false.``.
