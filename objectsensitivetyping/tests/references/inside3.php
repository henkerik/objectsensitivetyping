<?php

$r->foo = "text";
$x =& $r->foo;
$l = $r;
unset ($x);
$l->foo = true;

$a = $r->foo;


?>