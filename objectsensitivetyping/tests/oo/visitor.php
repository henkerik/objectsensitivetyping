<?php

class Foo {

	public $x = 42;
	public $y;

	function __construct($bar)
	{
		$this->y = $bar->g ($this);
	}
}

class Bar {
	function g ($foo) 
	{
		return $foo->x;
	}
}

$x = new Foo(new Bar());
$y = $x->y;

?>