<?php

$r->foo = "text";
$x =& $r->foo;
$l = $r;
$l->foo = true;

$a = $r->foo;


?>