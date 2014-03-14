<?php

function foo ($x = array('foo'=>'bar')) 
{
    return $x;
}

$a = foo ();
$x = $a['foo'];
var_dump ($x);

?>