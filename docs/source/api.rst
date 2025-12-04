
API Reference
=============

The full Fortran API documentation is generated using FORD (Fortran Automatic Documentation).

You can browse the complete API documentation here:

.. raw:: html

   <div style="margin: 20px 0;">
      <a href="_static/ford/index.html" target="_blank" class="btn btn-primary">
         📚 Open FORD API Documentation
      </a>
   </div>

The API documentation includes:

* **Module Reference**: Detailed documentation of all modules
* **Type Definitions**: Documentation of derived types like ``graph_type``
* **Procedures**: All public procedures and their interfaces
* **Source Code**: Annotated source code with cross-references
* **Call Graphs**: Visual representation of procedure dependencies

Key Components
--------------

The following are some of the key derived types and their main methods documented in the API:


* ``graph_type``: The main graph data structure

  * ``add_vertex()``: Method to add a vertex
  * ``add_edge()``: Method to add an edge
  * ``remove_vertices()``: Method to remove vertices
  * ``remove_edges()``: Method to remove edges
  * ``calculate_degree()``: Method to calculate vertex degrees
  * ``generate_adjacency()``: Method to generate adjacency matrix
  * ``convert_to_sparse()``: Method to convert to sparse format
  * ``convert_to_dense()``: Method to convert to dense format
  * ``copy()``: Method to copy the graph
  * ``clear()``: Method to clear the graph data
  
* ``vertex_type``: Represents a vertex in the graph

  * ``feature``: Property for vertex features
  * ``id``: Property for vertex identifier

* ``edge_type``: Represents an edge in the graph

  * ``weight``: Property for edge weight
  * ``feature``: Property for edge features
  * ``directed``: Property indicating if the edge is directed
  * ``id``: Property for edge identifier

For complete details and source code, please refer to the FORD documentation linked above.
