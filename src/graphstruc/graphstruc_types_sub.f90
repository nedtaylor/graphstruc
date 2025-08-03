submodule(graphstruc_types) graphstruc_types_submodule
  !! This submodule contains the implementation of procedures outlined in the
  !! graphstruc module.
  !!
  !! Sparse adjacency implemented by Artan Qerushi.
  implicit none


contains

  module function vertex_type_init(feature, id) &
       result(output)
    !! Initialise a vertex.
    implicit none

    ! Arguments
    real(real32), dimension(:), intent(in) :: feature
    !! Feature vector of the vertex.
    integer, intent(in), optional :: id
    !! Identifier of the vertex.
    type(vertex_type) :: output
    !! Initialised vertex.

    output%feature = feature
    if(present(id)) output%id = id
  end function vertex_type_init


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
       if(directed) output%index(2) = -abs(index(2))
    end if
    if(present(weight)) output%weight = weight
    if(present(feature)) output%feature = feature
  end function edge_type_init


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

    ! Local variables
    integer :: i
    !! Loop index.
    integer :: id
    !! Identifier of the vertex or edge.

    output%is_sparse = .false.
    output%directed = .false.
    if(present(is_sparse)) output%is_sparse = is_sparse
    if(present(directed)) output%directed = directed
    if(present(name)) output%name = name
    if(present(vertex))then
       output%num_vertices = size(vertex, dim=1)
       output%num_vertex_features = size(vertex(1)%feature, dim=1)
       do i = 1, output%num_vertices
          if(size(vertex(i)%feature, dim=1) .ne. output%num_vertex_features)then
             write(0,*) 'ERROR: Number of vertex features do not match'
             stop "Exiting..."
          end if
       end do
       allocate(output%vertex(output%num_vertices))
       output%vertex = vertex
       id = 1
       do i = 1, output%num_vertices
          if(output%vertex(i)%id .eq. -1)then
             do while (any(output%vertex(:)%id .eq. id))
                id = id + 1
             end do
             output%vertex(i)%id = id
          end if
       end do
       if(present(edge))then
          output%num_edges = size(edge, dim=1)
          output%num_edge_features = size(edge(1)%feature, dim=1)
          do i = 1, output%num_edges
             if(size(edge(i)%feature, dim=1) .ne. output%num_edge_features)then
                write(0,*) 'ERROR: Number of edge indices do not match'
                stop "Exiting..."
             end if
          end do
       end if
       allocate(output%edge(output%num_edges))
       output%edge = edge
       do i = 1, output%num_edges
          if(output%edge(i)%id .eq. -1)then
             do while (any(output%edge(:)%id .eq. id))
                id = id + 1
             end do
             output%edge(i)%id = id
          end if
       end do
       call output%generate_adjacency()
       call output%calculate_degree()
    elseif(present(edge))then
       write(0,*) 'ERROR: Edges are present without vertices'
       stop "Exiting..."
    else
       output%num_vertices = 0
       output%num_vertex_features = 0
       output%num_edges = 0
       output%num_edge_features = 0
    end if
  end function graph_type_init


  module subroutine add_vertex(this, vertex, feature, id, update_adjacency)
    !! Add a vertex to the graph.
    implicit none

    ! Arguments
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

    ! Local variables
    type(vertex_type) :: vertex_
    !! Initialised vertex.
    real(real32), dimension(:,:), allocatable :: vertex_features
    !! Feature vectors of the vertices.
    logical :: update_adjacency_
    !! Boolean whether to update the adjacency matrix.


    if(present(vertex).and.present(feature))then
       write(0,*) 'ERROR: Both vertex and feature are present where only one &
            &should be defined'
       stop "Exiting..."
    elseif(.not.present(vertex).and..not.present(feature))then
       write(0,*) 'ERROR: Neither vertex nor feature are present'
       stop "Exiting..."
    end if
    if(present(vertex)) vertex_ = vertex
    if(present(feature)) vertex_%feature = feature

    if(.not.allocated(vertex_%feature))then
       allocate( &
            vertex_%feature(this%num_vertex_features), &
            source = 0.0_real32&
       )
    else if(this%num_vertex_features.eq.0)then
       this%num_vertex_features = size(vertex_%feature, dim=1)
    else if(size(vertex_%feature, dim=1).ne.this%num_vertex_features)then
       write(0,*) 'ERROR: Number of vertex features do not match'
       stop "Exiting..."
    end if

    if(present(id)) vertex_%id = id


    this%num_vertices = this%num_vertices + 1
    if(this%is_sparse)then
       allocate(vertex_features(this%num_vertex_features,this%num_vertices))
       vertex_features(:,1:this%num_vertices-1) = this%vertex_features
       vertex_features(:,this%num_vertices) = vertex_%feature
       if(allocated(this%vertex_features)) deallocate(this%vertex_features)
       call move_alloc(vertex_features, this%vertex_features)
    else
       if(.not.allocated(this%vertex)) allocate(this%vertex(0))
       this%vertex = [this%vertex, vertex_]
    end if
    update_adjacency_ = .true.
    if(present(update_adjacency)) update_adjacency_ = update_adjacency
    if(update_adjacency_) call this%generate_adjacency()
  end subroutine add_vertex


  module subroutine add_edge( &
       this, edge, index, weight, feature, directed, id, update_adjacency &
  )
    !! Add an edge to the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    type(edge_type), intent(in), optional :: edge
    !! Edge to be added.
    integer, dimension(2), intent(in), optional :: index
    !! Vertex indices of the edge.
    real(real32), intent(in), optional :: weight
    !! Weight of the edge.
    real(real32), dimension(:), intent(in), optional :: feature
    !! Feature vector of the edge.
    logical, intent(in), optional :: directed
    !! Boolean whether the edge is directed. Default is False.
    integer, intent(in), optional :: id
    !! Identifier of the edge.
    logical, intent(in), optional :: update_adjacency
    !! Boolean whether to update the adjacency matrix. Default is True.

    ! Local variables
    class(edge_type), allocatable :: edge_
    !! Initialised edge.
    real(real32) :: weight_
    !! Weight of the edge.
    logical :: directed_
    real(real32), dimension(:,:), allocatable :: edge_features
    !! Feature vectors of the edges.
    logical :: update_adjacency_
    !! Boolean whether to update the adjacency matrix.


    directed_ = .false.
    if(present(edge).and.( &
         present(index)   .or. &
         present(weight)  .or. &
         present(directed).or. &
         present(feature) &
    ))then
       write(0,*) 'ERROR: Both edge and parameters are present where only one &
            &should be defined'
       stop "Exiting..."
    elseif(.not.present(edge)  .and. &
         .not.present(index)   .and. &
         .not.present(weight)  .and. &
         .not.present(directed).and. &
         .not.present(feature) &
    )then
       write(0,*) 'ERROR: Neither edge nor parameters are present'
       stop "Exiting..."
    end if

    if(present(edge))then
       edge_ = edge
    else
       if(.not.present(index))then
          write(0,*) 'ERROR: Index is not present'
          stop "Exiting..."
       else
          weight_ = 0._real32
          if(present(weight)) weight_ = weight
          if(present(directed))then
             directed_ = directed
          else
             if(any(index .lt. 0)) directed_ = .true.
          end if
          if(directed_.and.(directed_.neqv.this%directed))then
             write(0,*) 'ERROR: Edge direction does not match graph direction'
             stop "Exiting..."
          end if
          if(present(feature)) then
             edge_ = edge_type_init(index, weight_, feature, directed_)
          else
             edge_ = edge_type_init(index, weight_, directed=directed_)
          end if
       end if
    end if

    if(.not.allocated(edge_%feature))then
       allocate( &
            edge_%feature(this%num_edge_features), &
            source = 0.0_real32&
       )
    else if(this%num_edge_features.eq.0)then
       this%num_edge_features = size(edge_%feature, dim=1)
    else if(size(edge_%feature, dim=1).ne.this%num_edge_features)then
       write(0,*) 'ERROR: Number of edge features do not match'
       stop "Exiting..."
    end if

    if(present(id)) edge_%id = id

    this%num_edges = this%num_edges + 1
    if(this%is_sparse)then
       allocate(edge_features(this%num_edge_features,this%num_edges))
       edge_features(:,1:this%num_edges-1) = this%edge_features
       edge_features(:,this%num_edges) = edge_%feature
       if(allocated(this%edge_features)) deallocate(this%edge_features)
       call move_alloc(edge_features, this%edge_features)
    else
       if(.not.allocated(this%edge)) allocate(this%edge(0))
       this%edge = [this%edge, edge_]
    end if

    update_adjacency_ = .true.
    if(present(update_adjacency)) update_adjacency_ = update_adjacency
    if(update_adjacency_)then
       call this%generate_adjacency()
       if(edge_%index(1).gt.0)then
          this%vertex(edge_%index(1))%degree = this%vertex(edge_%index(1))%degree + 1
       end if
       if(.not.directed_) &
            this%vertex(abs(edge_%index(2)))%degree = &
                 this%vertex(abs(edge_%index(2)))%degree + 1
    end if

  end subroutine add_edge


  module subroutine set_num_vertices(this, num_vertices, num_vertex_features)
    !! Set the number of vertices of the graph.
    !!
    !! This will deallocate the existing vertices and edges
    !! and set the number of vertices.
    !! New vertices will be allocated but not initialised.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, intent(in) :: num_vertices
    !! Number of vertices in the graph.
    integer, intent(in), optional :: num_vertex_features

    if(allocated(this%vertex)) deallocate(this%vertex)
    if(allocated(this%edge)) deallocate(this%edge)
    if(allocated(this%adj_ia)) deallocate(this%adj_ia)
    if(allocated(this%adj_ja)) deallocate(this%adj_ja)
    if(allocated(this%adjacency)) deallocate(this%adjacency)
    this%num_vertices = num_vertices
    this%num_edges = 0
    if(present(num_vertex_features)) &
         this%num_vertex_features = num_vertex_features
    if(this%is_sparse)then
       allocate(this%vertex_features(this%num_vertex_features, this%num_vertices))
    else
       allocate(this%vertex(num_vertices))
    end if
  end subroutine set_num_vertices


  module subroutine set_num_edges(this, num_edges, num_edge_features)
    !! Set the number of edges of the graph.
    !!
    !! This will deallocate the existing edges and set the number of edges.
    !! New edges will be allocated but not initialised.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, intent(in) :: num_edges
    !! Number of edges in the graph.
    integer, intent(in), optional :: num_edge_features
    !! Number of edge features.

    if(allocated(this%edge)) deallocate(this%edge)
    if(allocated(this%adj_ia)) deallocate(this%adj_ia)
    if(allocated(this%adj_ja)) deallocate(this%adj_ja)
    if(allocated(this%adjacency)) deallocate(this%adjacency)
    this%num_edges = num_edges
    if(present(num_edge_features)) &
         this%num_edge_features = num_edge_features
    if(this%is_sparse)then
       allocate(this%edge_features(this%num_edge_features, this%num_edges))
       allocate(this%edge_weights(this%num_edges))
    else
       allocate(this%edge(num_edges))
    end if

  end subroutine set_num_edges


  module subroutine set_edges(this, vertex_index, connected_indices)
    !! Add edge connections between vertices of the graph.
    implicit none
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, intent(in) :: vertex_index
    !! Index of the vertex.
    integer, dimension(:), intent(in) :: connected_indices
    !! Indices of the connected vertices.

    ! Local variables
    integer :: i
    !! Loop index.
    logical :: directed
    !! Boolean whether the edge is directed.


    do i = 1, size(connected_indices, dim=1)
       directed = .false.
       if(connected_indices(i).lt.0) directed = .true.
       call this%add_edge( &
            index=[vertex_index, connected_indices(i)], &
            directed=directed &
       )
    end do
  end subroutine set_edges


  module subroutine remove_vertices(this, indices)
    !! Remove vertices from the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, dimension(:), intent(in) :: indices
    !! Indices of the vertices to be removed.

    ! Local variables
    integer :: i, j, k
    !! Loop indices.
    integer, dimension(size(indices, dim=1)) :: vertex_indices
    !! Indices of the vertices to be removed.
    integer, dimension(:), allocatable :: edge_indices
    !! Indices of the edges to be removed.

    allocate(edge_indices(0))
    do i = 1, size(indices, dim=1)
       do j = 1, this%num_edges
          if( &
               any(this%edge(j)%index .eq. indices(i)) .or. &
               any(this%edge(j)%index .eq. -indices(i)) &
          ) &
          edge_indices = [edge_indices, j]
       end do
    end do
    if(size(edge_indices, dim=1) .gt. 0) &
         call this%remove_edges(edge_indices, update_adjacency=.false.)

    vertex_indices = indices
    do i = 1, size(indices, dim=1)
       k = maxval(vertex_indices, dim = 1)
       this%vertex = [ &
            this%vertex(1:k-1:1), &
            this%vertex(k+1:this%num_vertices:1) &
       ]
       this%num_vertices = this%num_vertices - 1
       vertex_indices(maxloc(vertex_indices, dim = 1)) = 0
       do j = 1, this%num_edges
          if(this%edge(j)%index(1).gt.k)then
             this%edge(j)%index(1) = this%edge(j)%index(1) - 1
          elseif(this%edge(j)%index(1).lt.-k)then
             this%edge(j)%index(1) = this%edge(j)%index(1) + 1
          end if
          if(this%edge(j)%index(2).gt.k)then
             this%edge(j)%index(2) = this%edge(j)%index(2) - 1
          elseif(this%edge(j)%index(2).lt.-k)then
             this%edge(j)%index(2) = this%edge(j)%index(2) + 1
          end if
       end do
    end do
    call this%generate_adjacency()
  end subroutine remove_vertices


  module subroutine remove_edges(this, indices, update_adjacency)
    !! Remove edges from the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, dimension(:), intent(in) :: indices
    !! Indices of the edges to be removed.
    logical, intent(in), optional :: update_adjacency
    !! Boolean whether to update the adjacency matrix. Default is True.

    ! Local variables
    integer :: i, k
    !! Loop indices.
    logical :: update_adjacency_ = .true.
    !! Boolean whether to update the adjacency matrix.
    integer, dimension(size(indices, dim=1)) :: edge_indices
    !! Indices of the vertices to be removed.

    if(present(update_adjacency)) update_adjacency_ = update_adjacency

    edge_indices = indices
    do i = 1, size(indices, dim=1)
       k = maxval(edge_indices, dim = 1)
       this%vertex(this%edge(k)%index(1))%degree = &
            this%vertex(this%edge(k)%index(1))%degree - 1
       if(.not.this%directed)then
          this%vertex(abs(this%edge(k)%index(2)))%degree = &
               this%vertex(abs(this%edge(k)%index(2)))%degree - 1
       else if(this%edge(k)%index(2).gt.0)then
          this%vertex(this%edge(k)%index(2))%degree = &
               this%vertex(this%edge(k)%index(2))%degree - 1
       end if
       this%edge = [ &
            this%edge(1:k-1:1), &
            this%edge(k+1:this%num_edges:1) &
       ]
       this%num_edges = this%num_edges - 1
       edge_indices(maxloc(edge_indices, dim = 1)) = 0
    end do
    if(update_adjacency_) call this%generate_adjacency()
  end subroutine remove_edges

  module subroutine add_self_loops(this, indices, weight, features)
    !! Interface for adding self-loops to the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, dimension(:), intent(in), optional :: indices
    !! Indices of the vertices to which self-loops are added.
    real(real32), intent(in), optional :: weight
    !! Weight of the self-loop. Default is 1.0.
    real(real32), dimension(:), intent(in), optional :: features
    !! Feature vector of the self-loop. Default is empty.

    ! Local variables
    integer :: i
    !! Loop index.
    real(real32) :: weight_
    !! Weight of the self-loop.
    real(real32), dimension(:), allocatable :: features_
    !! Feature vector of the self-loop.

    if(.not.allocated(this%vertex)) then
       write(0,*) 'ERROR: Graph has no vertices to add self-loops'
       stop "Exiting..."
    end if
    if(present(features))then
       if(size(features, dim=1) .ne. 1 .and. &
            size(features, dim=1) .ne. this%num_edge_features &
       )then
         write(0,*) 'ERROR: Feature vector size does not match edge features'
         stop "Exiting..."
       end if
       features_ = features
    else
       allocate(features_(this%num_edge_features), source = 0._real32)
    end if

    weight_ = 1._real32
    if(present(weight)) weight_ = weight

    if(present(indices))then
       do i = 1, size(indices, dim=1)
          if(indices(i) .gt. 0 .and. indices(i) .le. this%num_vertices)then
             !! check if self-loop is already present
             if(this%is_sparse.and.allocated(this%adj_ja))then
                if( any( &
                     this%adj_ja(1, &
                          this%adj_ia(indices(i)):this%adj_ia(indices(i)+1)-1 &
                     ) .eq. indices(i) &
                ) ) then
                   write(0,*) 'Self-loop already exists for vertex', indices(i)
                   cycle
                end if
             elseif(allocated(this%adjacency))then
                if(this%adjacency(indices(i), indices(i)) .ne. 0) then
                   write(0,*) 'Self-loop already exists for vertex', indices(i)
                   cycle
                end if
             end if
             call this%add_edge( &
                  index=[indices(i), indices(i)], &
                  weight=weight_, &
                  feature=features_, &
                  update_adjacency=.false. &
             )
          else
             write(0,*) 'ERROR: Index out of bounds for self-loop'
             stop "Exiting..."
          end if
       end do
    else
       do i = 1, this%num_vertices
          if(this%is_sparse.and.allocated(this%adj_ja))then
             if(any(this%adj_ja(1,this%adj_ia(i):this%adj_ia(i+1)-1) .eq. i)) then
                write(0,*) 'Self-loop already exists for vertex', i
                cycle
             end if
          elseif(allocated(this%adjacency))then
             if(this%adjacency(i, i) .ne. 0) then
                write(0,*) 'Self-loop already exists for vertex', indices(i)
                cycle
             end if
          end if
          call this%add_edge( &
               index=[i, i], &
               weight=weight_, &
               feature=features_, &
               update_adjacency=.false. &
          )
       end do
    end if
    if((this%is_sparse.and.allocated(this%adj_ja)).or.allocated(this%adjacency))then
       call this%generate_adjacency()
    end if
    this%has_self_loops = .true.

  end subroutine add_self_loops

  module subroutine remove_self_loops(this, indices)
    !! Remove self-loops from the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, dimension(:), intent(in), optional :: indices
    !! Indices of the vertices from which self-loops are removed.

    ! Local variables
      integer :: i, j
      !! Loop indices.

      if(this%num_edges .eq. 0) then
         write(0,*) 'No edges to remove self-loops from'
         return
      end if

      do i = this%num_edges, 1, -1
         if(this%edge(i)%index(1) .eq. this%edge(i)%index(2)) then
            if(present(indices))then
               if(all(this%edge(i)%index(1) .ne. indices)) then
                  cycle
               end if
            end if
            ! Remove self-loop edge
            call this%remove_edges([i], update_adjacency=.false.)
         end if
      end do
      this%has_self_loops = .false.

  end subroutine remove_self_loops


  module subroutine calculate_degree(this)
    !! Calculate the degree of the vertices in the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.

    ! Local variables
    integer :: i, j
    !! Loop indices.

    if ( .not. this%is_sparse )then  ! graph is not sparse
       this%vertex(:)%degree = 0
       do i = 1, this%num_vertices
          do j = 1, this%num_vertices, 1
             if(this%adjacency(i,j) .gt. 0) &
                  this%vertex(i)%degree = this%vertex(i)%degree + 1
          end do
       end do
    else if ( this%is_sparse .and. ( .not. this%directed ) )then  ! graph is sparse and not directed
       this%vertex(:)%degree = 0
       do i = 1, this%num_vertices
          this%vertex(i)%degree = this%adj_ia(i+1) - this%adj_ia(i)
       end do
    else  ! graph is sparse and directed
       write(0,*) 'ERROR: Case of sparse and directed graph NOT IMPLEMENTED!'
       stop "Exiting..."
    end if

  end subroutine calculate_degree


  module subroutine generate_adjacency(this, index_list)
    !! Generate the adjacency matrix of the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, dimension(:,:), intent(in), optional :: index_list
    !! List of indices to be used for the adjacency matrix.

    integer :: i, j, k, i1, i2
    !! Loop indices.
    integer, dimension(3,this%num_vertices+2*this%num_edges) :: coo
    !! sparse adjacency in coordinate format (coo)
    integer, dimension(3) :: temp
    !! temporary array

    if( .not. this%is_sparse )then  ! graph is not sparse; allocate and fill array adjacency.
       if(allocated(this%adjacency)) deallocate(this%adjacency)
       allocate(this%adjacency(this%num_vertices, this%num_vertices))
       this%adjacency = 0
       do k = 1, this%num_edges
          i = this%edge(k)%index(1)
          j = this%edge(k)%index(2)
          if(i.eq.0.or.j.eq.0) cycle  ! skip edges with zero index
          if(this%directed.and.j.lt.0) then
             this%adjacency(i,abs(j)) = k
          else
             this%adjacency(i,abs(j)) = k
             this%adjacency(abs(j),i) = k
          end if
       end do
    else if( this%is_sparse .and. ( .not. this%directed ) )then  ! graph is sparse and not directed; allocate and fill arrays adj_ia and adj_ja.
       if(allocated(this%adj_ia)) deallocate(this%adj_ia)
       if(allocated(this%adj_ja)) deallocate(this%adj_ja)
       allocate(this%adj_ia(this%num_vertices+1))
       if(this%has_self_loops)then
          allocate(this%adj_ja(2,2*this%num_edges - this%num_vertices))
       else
          allocate(this%adj_ja(2,2*this%num_edges))
       end if
       ! Step 1: edgelist to coo array.
       !  do i = 1, this%num_vertices  ! vertex self-loops
       !     coo(1,i) = i
       !     coo(2,i) = i
       !     coo(3,i) = 0
       !  end do
       if(present(index_list)) then
          do i = 1, size(index_list, dim=2)
             coo(1,i+this%num_vertices) = index_list(1,i)
             coo(2,i+this%num_vertices) = index_list(2,i)
             coo(3,i+this%num_vertices) = i
          end do
          do i = 1, size(index_list, dim=2)
             coo(1,i+this%num_vertices+this%num_edges) = index_list(2,i)
             coo(2,i+this%num_vertices+this%num_edges) = index_list(1,i)
             coo(3,i+this%num_vertices+this%num_edges) = i
          end do
       else
          do i = 1, this%num_edges     ! first pass over edges (1,2)
             coo(1,i+this%num_vertices) = this%edge(i)%index(1)
             coo(2,i+this%num_vertices) = this%edge(i)%index(2)
             coo(3,i+this%num_vertices) = i
          end do
          do i = 1, this%num_edges     ! second pass over edges (2,1)
             if(this%edge(i)%index(1).eq.this%edge(i)%index(2)) cycle ! skip self-loops
             coo(1,i+this%num_vertices+this%num_edges) = this%edge(i)%index(2)
             coo(2,i+this%num_vertices+this%num_edges) = this%edge(i)%index(1)
             coo(3,i+this%num_vertices+this%num_edges) = i
          end do
       end if
       ! Step 2: sort coo array.
       do i2 = this%num_vertices+2*this%num_edges, 2, -1
          do i1 = 1, i2 - 1
             if ( ( coo(1,i1) .gt. ( coo(1,i2) ) ) .or. &
                  ( ( coo(1,i1) .eq. coo(1,i2) ) .and. ( coo(2,i1) .gt. coo(2,i2)  ) ) ) then
                temp(1:3) = coo(1:3,i1)
                coo(1:3,i1) = coo(1:3,i2)
                coo(1:3,i2) = temp(1:3)
             end if
          end do
       end do
       ! Step 3: sorted coo array to adj_ia and adj_ja arrays.
       this%adj_ia(1) = 1
       do i = 1, this%num_vertices+2*this%num_edges
          this%adj_ja(1,i) = coo(2,i)
          this%adj_ja(2,i) = coo(3,i)
          this%adj_ia(coo(1,i)+1) = i + 1
       end do
    else  ! graph is sparse and directed
       write(0,*) 'ERROR: Case of sparse and directed graph NOT IMPLEMENTED!'
       stop "Exiting..."
    end if

  end subroutine generate_adjacency


  module subroutine convert_to_sparse(this)
    !! Convert the graph to a sparse representation.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.

    ! Local variables
    integer :: v, e, idx
    !! Loop indices.

    if(this%is_sparse) return

    this%is_sparse = .true.

    if(allocated(this%adjacency)) deallocate(this%adjacency)
    if(allocated(this%vertex_features)) deallocate(this%vertex_features)
    if(allocated(this%edge_features)) deallocate(this%edge_features)
    if(allocated(this%edge_weights)) deallocate(this%edge_weights)

    allocate(this%vertex_features(this%num_vertex_features, this%num_vertices))
    allocate(this%edge_features(this%num_edge_features, this%num_edges))
    allocate(this%edge_weights(this%num_edges))

    do v = 1, this%num_vertices
       idx = this%vertex(v)%id
       if(idx.eq.-1) idx = v
       this%vertex_features(:,idx) = this%vertex(v)%feature
    end do

    do e = 1, this%num_edges
       idx = this%edge(e)%id
       if(idx.eq.-1) idx = e
       this%edge_features(:,e) = this%edge(e)%feature
       this%edge_weights(e) = this%edge(e)%weight
    end do

    deallocate(this%vertex)

    allocate(this%adj_ia(this%num_vertices+1))
    allocate(this%adj_ja(2,this%num_vertices+2*this%num_edges))
    this%adj_ia(1) = 1
    this%adj_ja(1,:) = 0
    this%adj_ja(2,:) = 0
    do v = 1, this%num_vertices
       this%adj_ia(v+1) = this%adj_ia(v)
       do e = 1, this%num_edges
          if(this%directed.and.this%edge(e)%index(1).eq.v)then
             this%adj_ja(1,this%adj_ia(v+1)) = this%edge(e)%index(2)
             this%adj_ja(2,this%adj_ia(v+1)) = e
             this%adj_ia(v+1) = this%adj_ia(v+1) + 1
          elseif(.not.this%directed.and.this%edge(e)%index(1).eq.v)then
             this%adj_ja(1,this%adj_ia(v+1)) = this%edge(e)%index(2)
             this%adj_ja(2,this%adj_ia(v+1)) = e
             this%adj_ia(v+1) = this%adj_ia(v+1) + 1
          elseif(.not.this%directed.and.this%edge(e)%index(2).eq.v)then
             this%adj_ja(1,this%adj_ia(v+1)) = this%edge(e)%index(1)
             this%adj_ja(2,this%adj_ia(v+1)) = e
             this%adj_ia(v+1) = this%adj_ia(v+1) + 1
          end if
       end do
    end do
    if(allocated(this%adjacency)) deallocate(this%adjacency)

  end subroutine convert_to_sparse

  module subroutine convert_to_dense(this)
    !! Convert the graph to a dense representation.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.

    ! Local variables
    integer :: v, e, i, j, idx
    !! Loop indices.

    if(.not.this%is_sparse) return

    this%is_sparse = .false.

    if(allocated(this%vertex_features)) deallocate(this%vertex_features)
    if(allocated(this%edge_features)) deallocate(this%edge_features)
    if(allocated(this%edge_weights)) deallocate(this%edge_weights)

    allocate(this%vertex(this%num_vertices))
    allocate(this%edge(this%num_edges))

    do v = 1, this%num_vertices
       idx = this%vertex(v)%id
       if(idx.eq.-1) idx = v
       this%vertex(v)%feature = this%vertex_features(:,idx)
       this%vertex(v)%id = v
    end do

    do e = 1, this%num_edges
       idx = this%edge(e)%id
       if(idx.eq.-1) idx = e
       this%edge(e)%feature = this%edge_features(:,idx)
       this%edge(e)%weight = this%edge_weights(idx)
       this%edge(e)%id = e
    end do

    if(allocated(this%adjacency)) deallocate(this%adjacency)
    allocate(this%adjacency(this%num_vertices, this%num_vertices))
    this%adjacency = 0
    do i = 1, size(this%adj_ia, dim=1)-1
       do j = this%adj_ia(i), this%adj_ia(i+1)-1
          this%adjacency(i,this%adj_ja(1,j)) = this%adj_ja(2,j)
       end do
    end do

    deallocate(this%vertex_features)
    deallocate(this%edge_features)
    deallocate(this%edge_weights)
    deallocate(this%adj_ia)
    deallocate(this%adj_ja)

  end subroutine convert_to_dense


  module subroutine copy(this, source, sparse)
    !! Copy the graph structure.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    class(graph_type), intent(in) :: source
    !! Source graph to copy from.
    logical, intent(in), optional :: sparse
    !! Boolean whether to copy the graph as sparse. Default is False.

    this%num_vertices = source%num_vertices
    this%num_edges = source%num_edges
    this%num_vertex_features = source%num_vertex_features
    this%num_edge_features = source%num_edge_features
    this%is_sparse = source%is_sparse
    this%directed = source%directed
    this%name = source%name
    if(this%is_sparse)then
       this%adj_ia = source%adj_ia
       this%adj_ja = source%adj_ja
       this%vertex_features = source%vertex_features
       this%edge_features = source%edge_features
       this%edge_weights = source%edge_weights
    else
       allocate(this%vertex(this%num_vertices))
       this%vertex = source%vertex
       allocate(this%edge(this%num_edges))
       this%edge = source%edge
       this%adjacency = source%adjacency
    end if
    if(present(sparse))then
       if(sparse.and..not.this%is_sparse)then
          call this%convert_to_sparse()
       elseif(.not.sparse.and.this%is_sparse)then
          call this%convert_to_dense()
       end if
    end if
  end subroutine copy

end submodule graphstruc_types_submodule
