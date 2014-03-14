<?php

class Inner {
    
    protected $var;
    
    function __construct ()
    {
        $this->var = 42;
    }
    
    function foo ()
    {
        return $this->var;
    }
}

class Outer {
    function __construct ()
    {
        $this->inner = new Inner ();
    }
    
    function foo ()
    {
        return $this->inner->foo ();
    }
}

$x = new Outer ();
$a = $x->foo();

?>