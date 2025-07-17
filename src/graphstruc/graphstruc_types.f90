module graphstruc_types
  !! This module contains the graph constructs.
  !!
  !! The module provides a derived type implementation for graph structures.
  !! Sparse storage implemented using Compressed Sparse Row (CSR) format.
  !! Sparse adjacency implemented by Artan Qerushi.
  use graphstruc_kinds, only: real32
  implicit none

  private

  public :: vertex_type, edge_type, graph_type


!!!-----------------------------------------------------------------------------
!!! graph vertex type
!!!-----------------------------------------------------------------------------
  type :: vertex_type
     !! Type implementing the vertex structure.
     integer :: degree = 0
     !! Degree of the vertex.
     integer :: id = -1
     !! Optional identifer for the vertex.
     real(real32), dimension(:), allocatable :: feature
     !! Feature vector of the vertex.
  end type vertex_type

  type :: edge_type
     !! Type implementing the edge structure.
     !!
     !! The edge connects two vertices in the graph.
     !! Vertex indices are stored in the index array.
     !! For directed graphs, index(1) is the source vertex, index(2) is the
     !! target vertex
     !! For undirected graphs, the order of the indices does not matter.
     integer, dimension(2) :: index
     !! Vertex indices of the edge.
     !! For directed graphs, index(2) is -ve if the edge is directed.
     !! Both indices are +ve for bidirectional edges.
     real(real32) :: weight = 1._real32
     !! Weight of the edge.
     integer :: id = -1
     !! Optional identifer for the edge.
     real(real32), dimension(:), allocatable :: feature
     !! Feature vector of the edge.
  end type edge_type

  type :: graph_type
     !! Type implementing the graph structure.
     !!
     !! The graph structure contains the vertices and edges of the graph.
     logical :: directed = .false.
     !! Boolean whether the graph is directed.
     logical :: is_sparse = .false.
     !! Boolean whether the graph is sparse
     integer :: num_vertices = 0, num_edges = 0
     !! Number of vertices and edges in the graph.
     integer :: num_vertex_features = 0, num_edge_features = 0
     !! Number of features for vertices and edges.
     character(len=:), allocatable :: name
     !! Name of the graph.
     integer, dimension(:,:), allocatable :: adjacency
     !! Adjacency matrix of the graph, when the graph isn't sparse.
     !!
     !! The adjacency matrix is a 2D array of integers.
     !! The value of the element (i,j) is the index of the edge connecting
     !! vertex i to vertex j (directed).
     !! If no edge exists, the value is 0.
     integer, dimension(:), allocatable :: adj_ia
     integer, dimension(:,:), allocatable :: adj_ja

     real(real32), dimension(:), allocatable :: edge_weights
     !! Weights of the edges.
     !! Adjacency matrix of the graph, when the graph is sparse and not directed,
     !! in Compressed Sparse Row (CSR) format.
     !!
     !! The first array, adj_ia, is known as the row pointer array;
     !! the second array, adj_ja, is known as the column index array.
     !! For example, the nodes connected to node 1 through an edge,
     !! are adj_ja(adj_ia(1)) to adj_ja(adj_ia(2)-1), the nodes connected
     !! to node 2 through an edge are, adj_ja(adj_ia(2)) to adj_ja(adj_ia(3)-1)
     !! and so on. In a calculation, to loop over the nodes connected to node i
     !! through an edge, we use:
     !! do i = adj_ia(i), adj_ia(i+1) - 1
     !!   adj_ja(1,i) ... node connected to node i through an edge
     !!   adj_ja(2,i) ... edge connecting node i to node adj_ja(1,i)
     !! end do
     real(real32), dimension(:,:), allocatable :: vertex_features
     !! Feature vectors of the vertices.
     real(real32), dimension(:,:), allocatable :: edge_features
     !! Feature vectors of the edges.
     type(vertex_type), dimension(:), allocatable :: vertex
     !! Array of vertices in the graph.
     type(edge_type), dimension(:), allocatable :: edge
     !! Array of edges in the graph.
   contains
     procedure, pass(this) :: add_vertex
     !! Procedure to add a vertex to the graph.
     procedure, pass(this) :: add_edge
     !! Procedure to add an edge to the graph.
     procedure, pass(this) :: set_num_vertices
     !! Procedure to set the number of vertices in the graph.
     procedure, pass(this) :: set_num_edges
     !! Procedure to set the number of edges in the graph.
     procedure, pass(this) :: set_edges
     !! Procedure to set the edges of the graph.
     procedure, pass(this) :: remove_vertices
     !! Procedure to remove a vertex from the graph.
     procedure, pass(this) :: remove_edges
     !! Procedure to remove an edge from the graph.
     procedure, pass(this) :: calculate_degree
     !! Procedure to calculate the degree of the vertices.
     procedure, pass(this) :: generate_adjacency
     !! Procedure to generate the adjacency matrix.
     procedure, pass(this) :: convert_to_sparse
     !! Procedure to convert the graph to a sparse representation.
     procedure, pass(this) :: convert_to_dense
     !! Procedure to convert the graph to a dense representation.
     procedure, pass(this) :: copy
     !! Procedure to copy the graph.
  end type graph_type

  interface vertex_type
     module function vertex_type_init(feature, id) &
          result(output)
       !! Interface for initialising a vertex.
       implicit none

       ! Arguments
       real(real32), dimension(:), intent(in) :: feature
       !! Feature vector of the vertex.
       integer, intent(in), optional :: id
       !! Identifier of the vertex.
       type(vertex_type) :: output
       !! Initialised vertex.
     end function vertex_type_init
  end interface vertex_type

  interface edge_type
     module function edge_type_init(index, weight, feature, directed) &
          result(output)
       !! Interface for initialising an edge.
       implicit none

       ! Arguments
       integer, dimension(2), intent(in) :: index
       !! Vertex indices of the edge.
       real(real32), intent(in), optional :: weight
       !! Weight of the edge.
       real(real32), dimension(:), intent(in), optional :: feature
       !! Feature vector of the edge.
       logical, intent(in), optional :: directed
       !! Boolean whether the edge is directed. Default is False.
       type(edge_type) :: output
       !! Initialised edge.
     end function edge_type_init
  end interface edge_type

  interface graph_type
     module function graph_type_init(vertex, edge, name, directed, is_sparse) &
          result(output)
       !! Interface for initialising a graph.
       implicit none

       ! Arguments
       type(vertex_type), dimension(:), intent(in), optional :: vertex
       !! Vertices in the graph.
       type(edge_type), dimension(:), intent(in), optional :: edge
       !! Edges in the graph.
       character(len=*), intent(in), optional :: name
       !! Name of the graph.
       logical, intent(in), optional :: directed
       !! Boolean whether the graph is directed. Default is False.
       logical, intent(in), optional :: is_sparse
       !! Boolean whether the graph is sparse. Default is False.
       type(graph_type) :: output
       !! Initialised graph.
     end function graph_type_init
  end interface graph_type

  interface
     module subroutine add_vertex(this, vertex, feature, id, update_adjacency)
       !! Interface for adding a vertex to the graph.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
       type(vertex_type), intent(in), optional :: vertex
       !! Vertex to be added.
       real(real32), dimension(:), intent(in), optional :: feature
       !! Feature vector of the vertex.
       integer, intent(in), optional :: id
       !! Identifier of the vertex.
       logical, intent(in), optional :: update_adjacency
       !! Boolean whether to update the adjacency matrix. Default is True.
     end subroutine add_vertex

     module subroutine add_edge( &
         this, edge, index, weight, feature, directed, id, update_adjacency &
     )
       !! Interface for adding an edge to the graph.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
       type(edge_type), intent(in), optional :: edge
       !! Edge to be added.
       integer, dimension(2), optional, intent(in) :: index
       !! Vertex indices of the edge.
       real(real32), intent(in), optional :: weight
       !! Weight of the edge.
       real(real32), dimension(:), intent(in), optional :: feature
       !! Feature vector of the edge.
       logical, intent(in), optional :: directed
       !! Boolean whether the edge is directed. Default is False.
       integer, intent(in), optional :: id
       !! Identifier of the vertex.
       logical, intent(in), optional :: update_adjacency
       !! Boolean whether to update the adjacency matrix. Default is True.
     end subroutine add_edge

     module subroutine set_num_vertices(this, num_vertices, num_vertex_features)
       !! Interface for setting the number of vertices of the graph.
       !!
       !! This will deallocate the existing vertices and edges
       !! and set the number of vertices.
       !! New vertices will be allocated but not initialised.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
       integer, intent(in) :: num_vertices
       !! Number of vertices in the graph.
       integer, intent(in), optional :: num_vertex_features
       !! Number of features for the vertices. Default is 0.
     end subroutine set_num_vertices

     module subroutine set_num_edges(this, num_edges, num_edge_features)
       !! Interface for setting the number of edges of the graph.
       !!
       !! This will deallocate the existing edges and set the number of edges.
       !! New edges will be allocated but not initialised.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
       integer, intent(in) :: num_edges
       !! Number of edges in the graph.
       integer, intent(in), optional :: num_edge_features
       !! Number of features for the edges. Default is 0.
     end subroutine set_num_edges

     module subroutine set_edges(this, vertex_index, connected_indices)
       !! Interface for setting the edges of the graph.
       !!
       !! Negative indicies in connected_indices define directional edge from
       !! vertex_index to -connected_index.
       !! Positive indicies in connected_indices define undirected edge between
       !! vertex_index and connected_index.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
       integer, intent(in) :: vertex_index
       !! Index of the vertex.
       integer, dimension(:), intent(in) :: connected_indices
       !! Indices of the connected vertices.
     end subroutine set_edges

     module subroutine remove_vertices(this, indices)
       !! Interface for removing vertices from the graph.
       !!
       !! This will deallocate the vertex and edges connected to the vertices.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
       integer, dimension(:), intent(in) :: indices
       !! Indices of the vertices to be removed.
     end subroutine remove_vertices

     module subroutine remove_edges(this, indices, update_adjacency)
       !! Interface for removing edges from the graph.
       !!
       !! This will deallocate the edges.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
       integer, dimension(:), intent(in) :: indices
       !! Indices of the edges to be removed.
       logical, intent(in), optional :: update_adjacency
       !! Boolean whether to update the adjacency matrix. Default is True.
     end subroutine remove_edges

     module subroutine calculate_degree(this)
       !! Interface for calculating the degree of the vertices.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
     end subroutine calculate_degree

     module subroutine generate_adjacency(this, index_list)
       !! Interface for generating the adjacency matrix.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
       integer, dimension(:,:), intent(in), optional :: index_list
       !! List of indices to be used for the adjacency matrix.
     end subroutine generate_adjacency

     module subroutine convert_to_sparse(this)
       !! Interface for converting the graph to a sparse representation.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
     end subroutine convert_to_sparse

     module subroutine convert_to_dense(this)
       !! Interface for converting the graph to a dense representation.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
     end subroutine convert_to_dense

     module subroutine copy(this, source, sparse)
       !! Interface for copying the graph.
       implicit none
       class(graph_type), intent(inout) :: this
       !! Parent. Instance of the graph structure.
       class(graph_type), intent(in) :: source
       !! Source graph to be copied.
       logical, intent(in), optional :: sparse
       !! Boolean whether to copy the graph as sparse. Default is False.
     end subroutine copy
  end interface

end module graphstruc_types
