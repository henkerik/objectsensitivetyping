<?php

class X {
    protected $var;

    public function foo ()
    {
        return $this->var;
    }
}

class Y extends X {
    public function __construct ()
    {
        $this->var = true;
    }
    
    public function foo ()
    {
        return parent::foo();
    }
}

$y = new Y ();
$a = $y->foo ();
$b = $a;


?>