<?php

class X {

    protected function __construct () 
    {
        $this->foo = 42;
    }
    
    protected static $instance;
    
    public static function getInstance ()
    {
        if (self::$instance == null) {
            self::$instance = new X ();
        }
        
        return self::$instance;
    }

    public function foo ()
    {
        return $this->foo;
    }
}

for ($i = 0; $i < 2; $i++) {
    $x = X::getInstance ();
    $a = $x->foo();
}

?>