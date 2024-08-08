submodule(graphstruc) graphstruc_submodule
  !! This submodule contains the implementation of procedures outlined in the
  !! graphstruc module.
  implicit none  


contains
  
  module function edge_type_init(index, weight, feature, directed) &
       result(output)
    !! Initialise an edge.
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
    output%index = index
    if(present(directed))then
       if(directed) output%index(2) = -index(2)
    end if
    if(present(weight)) output%weight = weight
    if(present(feature)) output%feature = feature
  end function edge_type_init


  module function graph_type_init(vertices, edges, name, directed) &
       result(output)
    !! Interface for initialising a graph.
    implicit none

    ! Arguments
    type(vertex_type), dimension(:), intent(in) :: vertices
    !! Vertices in the graph.
    type(edge_type), dimension(:), intent(in) :: edges
    !! Edges in the graph.
    character(len=128), intent(in), optional :: name
    !! Name of the graph.
    logical, intent(in), optional :: directed
    !! Boolean whether the graph is directed. Default is False.
    type(graph_type) :: output
    !! Initialised graph.

    output%num_vertices = size(vertices, dim=1)
    output%num_edges = size(edges, dim=1)
    output%directed = .false.
    if(present(directed)) output%directed = directed
    if(present(name)) output%name = name
    output%vertex = vertices
    output%edge = edges
    call output%generate_adjacency()
    call output%calculate_degree()
  end function graph_type_init


  module subroutine calculate_degree(this)
    !! Calculate the degree of the vertices in the graph.
    implicit none
    
    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.

    ! Local variables
    integer :: i, j
    !! Loop indices.

    this%vertex(:)%degree = 0
    do i = 1, this%num_vertices
      do j = 1, this%num_vertices, 1
        if(this%adjacency(i,j) .gt. 0) &
             this%vertex(i)%degree = this%vertex(i)%degree + 1
      end do
    end do
  end subroutine calculate_degree


  module subroutine generate_adjacency(this)
    !! Generate the adjacency matrix of the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer :: i, j, k
    !! Loop indices.

    allocate(this%adjacency(this%num_vertices, this%num_vertices))
    this%adjacency = 0
    do k = 1, this%num_edges
      i = this%edge(k)%index(1)
      j = this%edge(k)%index(2)
      if(this%directed.and.j.lt.0) then
        this%adjacency(i,abs(j)) = k
      else
        this%adjacency(i,j) = k
        this%adjacency(j,i) = k
      end if
    end do
  end subroutine generate_adjacency

end submodule graphstruc_submodule