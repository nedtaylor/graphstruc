Sparse Graphs
=============

For large or sparse graphs, the sparse storage format provides significant memory savings whilst maintaining efficient access patterns.

CSR Format
----------

Compressed Sparse Row (CSR) is an efficient format for storing sparse matrices. Instead of storing an entire V×V adjacency matrix, CSR stores only the non-zero elements.

**Structure**:

* ``adj_ia(:)``: Row pointer array of size V+1 indicating the start of each vertex's edge list
* ``adj_ja(:,:)``: 2D array of size (2, E) storing edge information:
  
  * ``adj_ja(1, :)``: Indices of connecting vertices (column indices)
  * ``adj_ja(2, :)``: Indices of the edges connecting the vertices

**Memory usage**: O(V + 2E) instead of O(V²) for dense format

**How it works**: For vertex i, its edges are stored in ``adj_ja(:, adj_ia(i):adj_ia(i+1)-1)``. This gives O(degree) access to a vertex's neighbours rather than O(V) scanning.

**Example**: For a graph with 10,000 vertices and 50,000 edges:

* Dense: 10,000 × 10,000 = 100,000,000 values (400 MB for real32)
* Sparse: 10,001 + 2×50,000 = 110,001 values (440 KB) - over 900× smaller!

Format Conversion
-----------------

Convert between formats at any time:

.. code-block:: fortran

   ! Dense → Sparse
   call graph%convert_to_sparse()
   
   ! Sparse → Dense
   call graph%convert_to_dense()
   
   ! Check current format
   if (graph%storage_format == 'sparse') then
     write(*,*) 'Using sparse storage'
   else
     write(*,*) 'Using dense storage'
   end if

The conversion preserves all graph data - vertices, edges, weights, and features. You can convert as many times as needed without losing information.

Accessing Neighbours
--------------------

In sparse format, use the CSR arrays to find neighbours:

.. code-block:: fortran

   ! Get all neighbours of vertex i
   integer :: j, start_idx, end_idx, neighbour, edge_idx
   
   start_idx = graph%adj_ia(i)
   end_idx = graph%adj_ia(i+1) - 1
   
   do j = start_idx, end_idx
     neighbour = graph%adj_ja(1, j)
     edge_idx = graph%adj_ja(2, j)
     
     write(*,*) 'Vertex', i, 'connects to vertex', neighbour
     write(*,*) '  via edge', edge_idx, 'with weight', graph%edge(edge_idx)%weight
   end do

Count neighbours:

.. code-block:: fortran

   ! Number of neighbours (degree)
   num_neighbours = graph%adj_ia(i+1) - graph%adj_ia(i)
   write(*,*) 'Vertex', i, 'has', num_neighbours, 'neighbours'

When to Use Sparse Format
--------------------------

Choose sparse format when:

**Large graphs**:

* V > 1000 vertices
* Memory becomes a constraint with dense format

**Sparse graphs**:

* E << V² (far fewer edges than possible)
* Typical for real-world networks (social networks, web graphs, molecular structures)

**Memory-constrained environments**:

* Limited RAM
* Need to fit multiple graphs in memory simultaneously

When to Use Dense Format
------------------------

Choose dense format when:

**Small graphs**:

* V < 1000 vertices
* Memory overhead is negligible

**Dense graphs**:

* E ≈ V² (many edges)
* Little benefit from sparse storage

**Fast lookups needed**:

* Frequent edge existence queries
* O(1) adjacency lookup vs O(degree) in sparse

Complete Example
----------------

Building and querying a large sparse graph:

.. code-block:: fortran

   program sparse_example
     use graphstruc, only: graph_type
     implicit none
     
     type(graph_type) :: graph
     integer :: i, j, start_idx, end_idx, neighbour
     integer :: num_neighbours
     real :: sparsity
     
     ! Build large graph
     graph%directed = .false.
     call graph%set_num_vertices(10000)
     
     ! Add edges (sparse connectivity pattern)
     do i = 1, 50000
       call graph%add_edge(index=[src(i), tgt(i)])
     end do
     
     ! Calculate sparsity
     sparsity = real(graph%num_edges) / real(graph%num_vertices**2)
     
     write(*,*) 'Graph statistics:'
     write(*,*) '  Vertices:', graph%num_vertices
     write(*,*) '  Edges:', graph%num_edges
     write(*,'(A,F8.6)') '  Sparsity:', sparsity
     
     ! Convert to sparse format
     call graph%convert_to_sparse()
     write(*,*) '  Format:', trim(graph%storage_format)
     
     ! Query neighbours of vertex 100
     write(*,*) ''
     write(*,*) 'Neighbours of vertex 100:'
     
     start_idx = graph%adj_ia(100)
     end_idx = graph%adj_ia(101) - 1
     num_neighbours = end_idx - start_idx + 1
     
     do j = start_idx, end_idx
       neighbour = graph%adj_ja(1, j)
       write(*,*) '  Vertex', neighbour
     end do
     
     write(*,*) 'Total:', num_neighbours, 'neighbours'
     
     ! Calculate all vertex degrees
     call graph%calculate_degree()
     
     ! Find high-degree vertices
     write(*,*) ''
     write(*,*) 'High-degree vertices (degree > 10):'
     do i = 1, graph%num_vertices
       if (graph%vertex(i)%degree > 10) then
         write(*,*) '  Vertex', i, 'degree:', graph%vertex(i)%degree
       end if
     end do
     
     call graph%clear()
   end program sparse_example

Performance Considerations
--------------------------

**Sparse format advantages**:

* **Memory efficiency**: Dramatically lower memory usage for sparse graphs
* **Cache locality**: Better cache performance when iterating through neighbours
* **Scalability**: Enables working with very large graphs (millions of vertices)
* **Iteration speed**: Faster to iterate through actual neighbours vs scanning entire rows

**Sparse format trade-offs**:

* **Edge queries**: Slower to check if a specific edge exists - O(degree) vs O(1)
* **Access patterns**: Slightly more complex code for neighbour access
* **Conversion cost**: One-time overhead to convert (though typically negligible)
* **Random access**: Less efficient for random edge lookups

**Performance tips**:

1. Build graphs in dense format, convert once to sparse after construction
2. Use sparse for iteration-heavy algorithms (breadth-first search, shortest paths)
3. Use dense for algorithms requiring frequent edge existence queries
4. Consider graph sparsity: if E > 0.1×V², dense may be competitive

Sparse vs Dense Comparison
---------------------------

+---------------------------+----------------------+----------------------+
| Operation                 | Dense                | Sparse               |
+===========================+======================+======================+
| Memory usage              | O(V²)                | O(V + E)             |
+---------------------------+----------------------+----------------------+
| Check edge exists         | O(1)                 | O(degree)            |
+---------------------------+----------------------+----------------------+
| Get all neighbours        | O(V)                 | O(degree)            |
+---------------------------+----------------------+----------------------+
| Add vertex                | O(V)                 | O(1)                 |
+---------------------------+----------------------+----------------------+
| Add edge                  | O(1)                 | O(1)                 |
+---------------------------+----------------------+----------------------+
| Iterate all edges         | O(V²)                | O(E)                 |
+---------------------------+----------------------+----------------------+

For most real-world graphs where E << V², sparse format provides significant advantages.

See Also
--------

* :doc:`graphs` - General graph operations
* :doc:`operations` - Converting between formats
