<?php

class X {
    const FOO = true;
}

interface Y {
    const BAR = 10;
    
    function foo ();
}

$a = X::FOO;
$b = Y::BAR;

?>