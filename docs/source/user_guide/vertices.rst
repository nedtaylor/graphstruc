Vertices
========

Vertices (or nodes) are the fundamental elements of a graph, representing entities or points in your network.

Vertex Type
-----------

Each vertex has the following properties:

.. code-block:: fortran

   type(vertex_type) :: v
   v%id           ! Unique identifier (integer)
   v%degree       ! Number of connections (integer)
   v%feature(:)   ! Feature vector (real32, allocatable)

Adding Vertices
---------------

Basic Addition
~~~~~~~~~~~~~~

Add a vertex without features:

.. code-block:: fortran

   call graph%add_vertex()

This creates a vertex with no features and an automatically assigned ID.

With Features
~~~~~~~~~~~~~

Add a vertex with a feature vector:

.. code-block:: fortran

   ! Add vertex with 3D features
   call graph%add_vertex(feature=[1.0, 2.0, 3.0])

**Important**: The first vertex you add sets the feature dimension for all subsequent vertices. All vertices must have the same feature size:

.. code-block:: fortran

   call graph%add_vertex(feature=[1.0, 2.0])  ! Sets feature size to 2
   call graph%add_vertex(feature=[3.0, 4.0])  ! OK - same size
   call graph%add_vertex(feature=[5.0])        ! ERROR - wrong size

With Custom ID
~~~~~~~~~~~~~~

Specify a custom identifier:

.. code-block:: fortran

   call graph%add_vertex(feature=[1.0, 2.0], id=100)

Batch Addition
~~~~~~~~~~~~~~

For efficiency when adding many vertices, pre-allocate space:

.. code-block:: fortran

   ! Allocate space for 1000 vertices with 10 features each
   call graph%set_num_vertices(1000, num_vertex_features=10)
   
   ! Then populate vertex data
   do i = 1, 1000
     graph%vertex(i)%feature = compute_features(i)
     graph%vertex(i)%id = i
   end do

Accessing Vertices
------------------

By Index
~~~~~~~~

Access vertices using array notation:

.. code-block:: fortran

   ! Get vertex properties
   write(*,*) 'ID:', graph%vertex(3)%id
   write(*,*) 'Degree:', graph%vertex(3)%degree
   write(*,*) 'Features:', graph%vertex(3)%feature

Iterating All Vertices
~~~~~~~~~~~~~~~~~~~~~~

Loop through all vertices:

.. code-block:: fortran

   do i = 1, graph%num_vertices
     write(*,*) 'Vertex', i
     write(*,*) '  ID:', graph%vertex(i)%id
     write(*,*) '  Degree:', graph%vertex(i)%degree
     write(*,*) '  Features:', graph%vertex(i)%feature
   end do

Modifying Vertices
------------------

Updating Features
~~~~~~~~~~~~~~~~~

Modify vertex feature vectors:

.. code-block:: fortran

   ! Replace entire feature vector
   graph%vertex(5)%feature = [10.0, 20.0, 30.0]
   
   ! Modify individual components
   graph%vertex(5)%feature(2) = 25.0

Removing Vertices
~~~~~~~~~~~~~~~~~

Remove one or more vertices:

.. code-block:: fortran

   ! Remove single vertex
   call graph%remove_vertices([3])
   
   ! Remove multiple vertices
   call graph%remove_vertices([2, 5, 7])

**Important effects**:

* All edges connected to removed vertices are also removed
* Remaining vertices are renumbered sequentially
* The adjacency matrix is automatically updated

Vertex Degrees
--------------

The degree of a vertex is the number of edges connected to it:

* **Undirected graphs**: Total number of connections
* **Directed graphs**: Number of outgoing edges

Calculate degrees:

.. code-block:: fortran

   ! Calculate for all vertices
   call graph%calculate_degree()
   
   ! Access individual degrees
   write(*,*) 'Vertex 1 has', graph%vertex(1)%degree, 'connections'

Vertex Features
---------------

Feature vectors can represent any numerical properties of vertices, such as:

* Node attributes in a network
* Coordinates in space
* Embedding vectors
* Physical properties

The feature size is determined by the first vertex added and applies to all vertices in the graph.
