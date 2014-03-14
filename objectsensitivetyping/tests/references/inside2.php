<?php

$r->foo = "text";
$x =& $r->foo;
unset ($x);
$l = $r;
$l->foo = true;

$a = $r->foo;


?>