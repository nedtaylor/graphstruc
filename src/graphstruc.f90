module graphstruc
  !! This module contains the graph constructs.
  !! 
  !! The module provides a derived type implementation for graph structures.
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
     real(real32), dimension(:), allocatable :: feature
     !! Feature vector of the edge.
  end type edge_type

  type :: graph_type
     !! Type implementing the graph structure.
     !!
     !! The graph structure contains the vertices and edges of the graph.
     logical :: directed = .false.
     !! Boolean whether the graph is directed.
     integer :: num_vertices, num_edges
     !! Number of vertices and edges in the graph.
     integer :: num_vertex_features, num_edge_features
     !! Number of features for vertices and edges.
     character(len=128) :: name
     !! Name of the graph.
     integer, dimension(:,:), allocatable :: adjacency
     !! Adjacency matrix of the graph.
     !!
     !! The adjacency matrix is a 2D array of integers.
     !! The value of the element (i,j) is the index of the edge connecting
     !! vertex i to vertex j (directed).
     !! If no edge exists, the value is 0.
     type(vertex_type), dimension(:), allocatable :: vertex
     !! Array of vertices in the graph.
     type(edge_type), dimension(:), allocatable :: edge
     !! Array of edges in the graph.
   contains
     procedure, pass(this) :: add_vertex
     !! Procedure to add a vertex to the graph.
     procedure, pass(this) :: add_edge
     !! Procedure to add an edge to the graph.
     procedure, pass(this) :: set_edges
     !! Procedure to set the edges of the graph.
     procedure, pass(this) :: calculate_degree
     !! Procedure to calculate the degree of the vertices.
     procedure, pass(this) :: generate_adjacency
     !! Procedure to generate the adjacency matrix.
  end type graph_type

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
    module function graph_type_init(vertex, edge, name, directed) &
         result(output)
      !! Interface for initialising a graph.
      implicit none

      ! Arguments
      type(vertex_type), dimension(:), intent(in) :: vertex
      !! Vertices in the graph.
      type(edge_type), dimension(:), intent(in) :: edge
      !! Edges in the graph.
      character(len=128), intent(in), optional :: name
      !! Name of the graph.
      logical, intent(in), optional :: directed
      !! Boolean whether the graph is directed. Default is False.
      type(graph_type) :: output
      !! Initialised graph.
    end function graph_type_init
  end interface graph_type

  interface
    module subroutine add_vertex(this, vertex, feature)
      !! Interface for adding a vertex to the graph.
      implicit none
      class(graph_type), intent(inout) :: this
      !! Parent. Instance of the graph structure.
      type(vertex_type), intent(in), optional :: vertex
      !! Vertex to be added.
      real(real32), dimension(:), intent(in), optional :: feature
      !! Feature vector of the vertex.
    end subroutine add_vertex

    module subroutine add_edge(this, edge, index, weight, feature, directed)
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
    end subroutine add_edge

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

    module subroutine calculate_degree(this)
      !! Interface for calculating the degree of the vertices.
      implicit none
      class(graph_type), intent(inout) :: this
      !! Parent. Instance of the graph structure.
    end subroutine calculate_degree

    module subroutine generate_adjacency(this)
      !! Interface for generating the adjacency matrix.
      implicit none
      class(graph_type), intent(inout) :: this
      !! Parent. Instance of the graph structure.
    end subroutine generate_adjacency
  end interface

end module graphstruc