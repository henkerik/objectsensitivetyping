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

class P {
    function __construct ()
    {
        $this->inner = new Inner ();
    }
    
    function foo ()
    {
        return $this->inner->foo ();
    }
}

class Child extends P {
    function __construct ()
    {
        parent::__construct();
        $this->bar = true;
    }
}

$x = new Child ();
$a = $x->foo();
$b = $x->bar;

?>