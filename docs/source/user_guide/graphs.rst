Graphs
======

The ``graph_type`` is the main data structure in graphstruc, representing a graph with vertices and edges.

Graph Type
----------

Key properties of a graph:

.. code-block:: fortran

   type(graph_type) :: graph
   
   graph%directed             ! .true. for directed, .false. for undirected
   graph%num_vertices         ! Number of vertices (read-only)
   graph%num_edges            ! Number of edges (read-only)
   graph%storage_format       ! 'dense' or 'sparse'
   graph%num_vertex_features  ! Feature vector size for vertices
   graph%num_edge_features    ! Feature vector size for edges

.. rubric:: :h3style:`Directed vs Undirected Graphs`

Set the graph type when creating your graph:

.. code-block:: fortran

   ! Undirected graph (default)
   graph%directed = .false.
   
   ! Directed graph
   graph%directed = .true.

In undirected graphs, edges connect vertices bidirectionally. In directed graphs, edges have a specific source and target.

Storage Formats
---------------

graphstruc supports two storage formats, each optimised for different use cases.

.. rubric:: :h3style:`Dense Format`

The default format uses an adjacency matrix:

* **Memory**: O(V²) where V is the number of vertices
* **Edge lookup**: O(1) - very fast
* **Best for**: Small graphs (< 1000 vertices) or dense graphs with many edges

.. code-block:: fortran

   ! Dense format is the default
   graph%storage_format = 'dense'

.. rubric:: :h3style:`Sparse Format (CSR)`

Compressed Sparse Row (CSR) format for memory efficiency:

* **Memory**: O(V + E) where E is the number of edges
* **Edge lookup**: O(degree) - slower than dense
* **Best for**: Large graphs (> 1000 vertices) or sparse graphs where E << V²

.. code-block:: fortran

   ! Convert to sparse format
   call graph%convert_to_sparse()

You can convert between formats at any time:

.. code-block:: fortran

   call graph%convert_to_sparse()  ! Dense → Sparse
   call graph%convert_to_dense()   ! Sparse → Dense

Accessing Graph Data
--------------------

.. rubric:: :h3style:`Dense Format`

Access the adjacency matrix directly:

.. code-block:: fortran

   real(real32), allocatable :: adj(:,:)
   adj = graph%adjacency
   
   ! Check if vertices i and j are connected
   if (graph%adjacency(i, j) /= 0.0) then
     write(*,*) 'Vertices connected with weight:', graph%adjacency(i, j)
   end if

.. rubric:: :h3style:`Sparse Format`

Use CSR arrays to find neighbours:

.. code-block:: fortran

   ! Get all neighbours of vertex i
   do j = graph%row_ptr(i), graph%row_ptr(i+1)-1
     neighbour = graph%col_ind(j)
     write(*,*) 'Vertex', i, 'connects to', neighbour
   end do

Graph Operations
----------------

.. rubric:: :h3style:`Clearing`

Remove all data and reset the graph:

.. code-block:: fortran

   call graph%clear()

This deallocates all arrays and resets properties to defaults.

.. rubric:: :h3style:`Copying`

Duplicate a graph:

.. code-block:: fortran

   type(graph_type) :: graph1, graph2
   
   ! Exact copy
   call graph2%copy(graph1)
   
   ! Copy and convert format
   call graph2%copy(graph1, sparse=.true.)

.. rubric:: :h3style:`Degree Calculation`

Calculate and update vertex degrees:

.. code-block:: fortran

   call graph%calculate_degree()
   
   ! Then access degrees
   do i = 1, graph%num_vertices
     write(*,*) 'Vertex', i, 'degree:', graph%vertex(i)%degree
   end do

For undirected graphs, degree is the number of connections. For directed graphs, it's the number of outgoing edges.

.. rubric:: :h3style:`Adjacency Matrix`

Manually regenerate the adjacency matrix from edges:

.. code-block:: fortran

   call graph%generate_adjacency()

This is normally called automatically when edges are added or removed.
