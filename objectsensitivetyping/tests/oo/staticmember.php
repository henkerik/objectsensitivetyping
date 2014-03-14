<?php

class X {
    public static $foo;
    
    public static function foo ()
    {
        self::$foo = 1;
    }
    
    public static function bar ()
    {
        $a = self::$foo;
        
        self::$foo = true;
    }
}

X::foo();
$a = X::$foo;
X::bar();
$b = X::$foo;

?>