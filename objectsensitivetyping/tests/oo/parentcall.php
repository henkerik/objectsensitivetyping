<?php

class X {
    function foo ()
    {
        return true;
    }
}

class Y extends X {    
    public $var;
    
    function foo ()
    {
        $this->var = parent::foo ();
        
        return true;
    }
}

$y = new Y ();

$a = $y->foo();
$b = $y->var;

?>