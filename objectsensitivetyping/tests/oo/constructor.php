<?php

class X {
    public function __construct ()
    {
        $this->foo = 42;
    }
}

class Y extends X {}

$x = new X();
$a = $x->foo;

$y = new Y();
$b = $y->foo;

?>