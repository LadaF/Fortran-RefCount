  function NAME_T(self, mold, ierr) result(res)
    RES_T                         :: res
    class(ref),        intent(in)  :: self
    FULL_T,            intent(in)  :: mold
    integer, optional, intent(out) :: ierr
    
    if (present(ierr)) ierr = 0

    if (associated(self%ptr)) then
      select type (data=>self%ptr%data)
        type is (SHORT_T)
          res = data
        class default
          call error(2)
      end select
    else
      call error(1)
    end if
  contains
    subroutine error(n)
      integer, intent(in) :: n
      if (present(ierr)) then
        ierr = n
      else
        write(*,*) "Error at", __FILE__,__LINE__
        write(*,*) "Error getting type FULL_T, code", n
        stop
      end if
    end subroutine
  end function
  