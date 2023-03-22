# Fortran-RefCount
Simple reference-counting-based smart pointer class for Fortran

Usage (see example code provided for details):


    type(ref) :: a,b,c,d, e

    a = "abc"
    print *, "a:", a
    
    
    a = 42
    b = f(f(f(f(a))))
    a = f(f(b))
    b = a
    b = f(a)
    
    print *, "a:", a, "b:", b
    
    c = cons(a, b)
    d = c
    
    p => d%pointer()
    select type(p)
      type is (cons)
        print *, "d: (",p%car,",",p%cdr,")"
    end select
    
    print *, "d:",d
    
    e = cons(c, d)


Tested using:

              gfortran -cpp -DTEST -fcheck=all -frecursive refcount.f90

or

              ifort -cpp -DTEST -check refcount.f90

