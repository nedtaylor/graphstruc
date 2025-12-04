Edges
=====

Edges (or links) connect vertices in a graph, representing relationships or connections between entities.

Edge Type
---------

Each edge has the following properties:

.. code-block:: fortran

   type(edge_type) :: e
   e%id           ! Unique identifier (integer)
   e%index(2)     ! [source, target] vertex indices
   e%weight       ! Edge weight (real32, default: 1.0)
   e%feature(:)   ! Feature vector (real32, allocatable)

Adding Edges
------------

Basic Edges
~~~~~~~~~~~

Connect two vertices:

.. code-block:: fortran

   ! Simple undirected edge between vertices 1 and 2
   call graph%add_edge(index=[1, 2])

Directed Edges
~~~~~~~~~~~~~~

For directed graphs, use negative indices or the ``directed`` parameter:

.. code-block:: fortran

   graph%directed = .true.
   
   ! Method 1: Negative target index (from 1 to 2)
   call graph%add_edge(index=[1, -2])  ! 1 → 2
   
   ! Method 2: Use directed parameter
   call graph%add_edge(index=[1, 2], directed=.true.)

The negative index convention allows you to mix directed and undirected edges in the same graph.

Weighted Edges
~~~~~~~~~~~~~~

Add edges with custom weights:

.. code-block:: fortran

   ! Edge with weight 2.5
   call graph%add_edge(index=[1, 2], weight=2.5)
   
   ! Default weight is 1.0
   call graph%add_edge(index=[2, 3])  ! weight = 1.0

Edge Features
~~~~~~~~~~~~~

Edges can have feature vectors:

.. code-block:: fortran

   ! Add edge with features
   call graph%add_edge(index=[1, 2], weight=1.5, feature=[0.1, 0.2])

**Important**: Like vertices, the first edge sets the feature dimension for all subsequent edges.

Batch Operations
~~~~~~~~~~~~~~~~

Connect one vertex to multiple vertices efficiently:

.. code-block:: fortran

   ! Connect vertex 1 to vertices 2, 3, 4 (undirected)
   call graph%set_edges(1, [2, 3, 4])
   
   ! Same but directed (1 → 2, 1 → 3, 1 → 4)
   call graph%set_edges(1, [-2, -3, -4])

Accessing Edges
---------------

By Index
~~~~~~~~

Access edge properties:

.. code-block:: fortran

   ! Get edge endpoints
   write(*,*) 'Source:', graph%edge(3)%index(1)
   write(*,*) 'Target:', graph%edge(3)%index(2)
   
   ! Get weight and features
   write(*,*) 'Weight:', graph%edge(3)%weight
   write(*,*) 'Features:', graph%edge(3)%feature

Iterating All Edges
~~~~~~~~~~~~~~~~~~~

Loop through all edges:

.. code-block:: fortran

   do i = 1, graph%num_edges
     write(*,'(A,I0,A,I0,A,I0,A,F6.2)') &
       'Edge ', i, ': ', graph%edge(i)%index(1), &
       ' -> ', graph%edge(i)%index(2), &
       ' weight: ', graph%edge(i)%weight
   end do

Modifying Edges
---------------

Updating Properties
~~~~~~~~~~~~~~~~~~~

Modify edge weights and features:

.. code-block:: fortran

   ! Update weight
   graph%edge(3)%weight = 5.0
   
   ! Update features
   graph%edge(3)%feature = [0.5, 0.6]
   
   ! Update individual feature components
   graph%edge(3)%feature(1) = 0.8

Removing Edges
~~~~~~~~~~~~~~

Remove one or more edges:

.. code-block:: fortran

   ! Remove single edge
   call graph%remove_edges([3])
   
   ! Remove multiple edges
   call graph%remove_edges([1, 4, 7])

**Effects**:

* Updates vertex degrees
* Updates adjacency matrix
* Remaining edges are renumbered

Batch Removal
~~~~~~~~~~~~~

For efficiency when removing many edges, defer adjacency updates:

.. code-block:: fortran

   ! Remove many edges without updating adjacency each time
   do i = 1, n_to_remove
     call graph%remove_edges([edge_list(i)], update_adjacency=.false.)
   end do
   
   ! Update adjacency once at the end
   call graph%generate_adjacency()

Self-Loops
----------

A self-loop is an edge that connects a vertex to itself.

Adding Self-Loops
~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   ! Add to all vertices
   call graph%add_self_loops()
   
   ! Add to specific vertices
   call graph%add_self_loops(indices=[1, 3, 5])
   
   ! With custom weight
   call graph%add_self_loops(indices=[1, 3, 5], weight=2.0)

Removing Self-Loops
~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   ! Remove from all vertices
   call graph%remove_self_loops()
   
   ! Remove from specific vertices
   call graph%remove_self_loops(indices=[2, 4])

Edge Features
-------------

Edge features can represent:

* Relationship strength or type
* Distance or cost
* Capacity or flow
* Temporal information
* Any numerical property of the connection

The feature size is determined by the first edge added and applies to all edges in the graph.
