<?php

$a = 1;

class Y {
    public function __construct ()
    {
        $this->foo = 42;
    }
}

class X {
    public function __construct ()
    {
        $y = new Y();
        $this->foo = $y->foo;
    }
}

$x = new X();
$a = $x->foo;

?>