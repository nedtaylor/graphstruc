module graphstruc
  !! This is the top-level module for the graphstruc Fortran library.
  use coreutils, only: real32
  use graphstruc_types, only: vertex_type, edge_type, graph_type
  implicit none

  private

  public :: real32
  public :: vertex_type, edge_type, graph_type

end module graphstruc
