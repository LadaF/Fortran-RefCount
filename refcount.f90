module ref_type
  use iso_fortran_env
  
  implicit none
    
  type nil
  end type

  type refptr
    integer :: ref_count = 0
    class(*), allocatable :: data
  contains
    final :: refptr_finalize
  end type

  type :: ref
!     private
    type(refptr),pointer :: ptr => null()
    integer :: weak = 0 !1.. this is a weak reference, -1.. make weak references to this
  contains
    procedure :: assign_star
    generic :: assignment(=) => assign_star
    procedure :: set_temp
    procedure :: val_real32
    procedure :: val_real64
    procedure :: val_int32
    procedure :: val_int64
    procedure :: val_char
    procedure :: val_logical
    generic :: value => val_real32, val_real64, &
                        val_int32, val_int64, &
                        val_char, val_logical
    procedure :: pointer => ref_pointer
    final :: ref_finalize
  end type
  
  interface ref
    module procedure ref_init
    module procedure ref_init_1
  end interface
  

contains
    
    
  function ref_init(obj) result(res)
    class(*),intent(in) :: obj
    type(ref) :: res

    allocate(res%ptr)

    select type (obj)
      type is (ref)
        stop "Passing ref not allowed."
    end select
    allocate(res%ptr%data, source = obj)
  end function
  function ref_init_1(obj) result(res)
    class(*),intent(in) :: obj(:)
    type(ref) :: res

    allocate(res%ptr)

    select type (obj)
      type is (ref)
        stop "Passing ref to ref not allowed."
    end select
    allocate(res%ptr%data, source = obj(1))
  end function

  
  subroutine assign_star(out,in)
    class(*),intent(in) :: in
    class(ref),intent(inout) :: out
    
    if (associated(out%ptr).and.out%weak<=0) then
      out%ptr%ref_count = out%ptr%ref_count - 1
      if (out%ptr%ref_count<=0) then
        deallocate(out%ptr)
      end if
    end if
    
    select type (in)
      class is (ref)
        if (associated(in%ptr)) then
          out%ptr => in%ptr
          if (in%weak>=0) then
            out%ptr%ref_count = out%ptr%ref_count + 1
            out%weak = 0
          else
            out%weak = 1
          end if
        else
          out%ptr => null()
        end if
      class default
        if (associated(out%ptr)) nullify(out%ptr)
        allocate(out%ptr)
        allocate(out%ptr%data, source=in)
        out%ptr%ref_count = out%ptr%ref_count + 1
    end select

  end subroutine
  
  
  subroutine set_temp(self)
    class(ref), intent(inout) :: self
    if (associated(self%ptr).and.self%weak<=0) &
      self%ptr%ref_count = self%ptr%ref_count - 1
  end subroutine
  
  
  recursive subroutine ref_finalize(self)
    type(ref),intent(inout) :: self
    
    if (associated(self%ptr).and.self%weak<=0) then
      self%ptr%ref_count = self%ptr%ref_count - 1
      if (self%ptr%ref_count<=0) then
        deallocate(self%ptr)
      end if
    end if
  end subroutine
  
  recursive subroutine refptr_finalize(self)
    type(refptr),intent(inout) :: self

    if (allocated(self%data)) deallocate(self%data)
  end subroutine
  
  
  
#define FULL_T SHORT_T
#define RES_T SHORT_T

#define SHORT_T real(real32)
#define NAME_T val_real32
#include "value-inc.f90"
#undef SHORT_T
#undef NAME_T
  
#define SHORT_T real(real64)
#define NAME_T val_real64
#include "value-inc.f90"
#undef SHORT_T
#undef NAME_T
  
#define SHORT_T integer(int32)
#define NAME_T val_int32
#include "value-inc.f90"
#undef SHORT_T
#undef NAME_T
  
#define SHORT_T integer(int64)
#define NAME_T val_int64
#include "value-inc.f90"
#undef SHORT_T
#undef NAME_T
  
#define SHORT_T logical
#define NAME_T val_logical
#include "value-inc.f90"
#undef SHORT_T
#undef NAME_T

#define SHORT_T character(*)
#undef RES_T
#define RES_T character(:), allocatable
#define NAME_T val_char
#include "value-inc.f90"
#undef SHORT_T
#undef FULL_T
#undef NAME_T
  
!   
  function ref_pointer(self) result(res)
    class(*), pointer             :: res
    class(ref), target, intent(in) :: self
    
    if (associated(self%ptr)) then
      res => self%ptr%data
    else
      res => null()
    end if
  end function
    
end module ref_type




#ifdef TEST
module cons_type

  use ref_type
  
  type :: cons
    type(ref) :: car, cdr
  end type
  
  interface cons
    module procedure cons_init
  end interface
  
  contains

  
  function cons_init(a, b) result(res)
    class(*),intent(in) :: a, b
    type(cons) :: res
    
    res%car = a
    res%cdr = b
  end function
  
    
  recursive subroutine set_car(r, a)
    type(ref),intent(inout) :: r
    class(*),intent(in) :: a
    select type(x=>r%ptr%data)
      type is (cons)
        x%car = a
      class default
        stop "set-car! expects pair"
    end select
  end subroutine
  
  recursive subroutine set_cdr(r, a)
    type(ref),intent(inout) :: r
    class(*),intent(in) :: a
    select type(x=>r%ptr%data)
      type is (cons)
        x%cdr = a
      class default
        stop "set-cdr! expects pair"
    end select
  end subroutine
  
end module

program test
  use ref_type
  use cons_type

  
  implicit none
  
  call main
  
contains

  subroutine main
    type(ref) :: a,b,c,d
    
    class(*), pointer :: p

    a = 3.14
    print *, "a:", a%value(0.0)
    
    
    a = 42
    b = f(f(f(f(a))))
    a = f(f(b))
    b = a
    b = f(a)
    
    print *, "a:", a%value(0), "b:", b%value(0)
    
    c = cons(a, b)
    d = c
    
    p => d%pointer()
    select type(p)
      type is (cons)
        print *, "d: (",p%car%value(0),",",p%cdr%value(0),")"
    end select
    
    
    print *, "reference count of a:", a%ptr%ref_count

  end subroutine
    
  recursive function f(a) result(res)
    type(ref) a,res
    res = a
#ifdef __GFORTRAN__
    !HACK gfortran still does not finalize function results :(
    call res%set_temp
#endif
  end function

end program
#endif
