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
        if (n==2) then
          call backtrace
          stop "Error in select type for &
          &FULL_T."
        else if (n==1) then
          stop "Error, self%ptr not associated."
        end if
      end if
    end subroutine
  end function
  
