<?php

class X {
    protected $var;
    
    public function __construct ()
    {
        $this->var = true;
    }
    
    public function get ()
    {
        return $this->var;
    }
    
    public function set ($value)
    {
        $this->var = $value;
    }
}

$x = new X();
$a = $x->get();
$x->set (1);
$b = $x->get();

?>