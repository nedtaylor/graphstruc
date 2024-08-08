module graphstruc_kinds
  !! This module contains the kinds used in the program.
  !!
  !! This module enables the loading of portable real kind precision.
  implicit none
  integer, parameter, public :: real32 = Selected_real_kind(6,37)
end module graphstruc_kinds
