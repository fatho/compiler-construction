begin

    proc foo(val x, res y) is
      call baz(x, y);
      y := 10;
    end
    
    proc bar(val x, res y) is
      call baz(x, y);
    end
    
    proc baz(val x, res y) is
      y := 2 * x;
    end
    
    
    call foo(ai, ao);
    
    call bar(bi, bo);
    
    ai := 42;
    bi := 42;
end