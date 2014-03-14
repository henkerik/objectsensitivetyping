<?php

class Foo {

	public function foo ($b)
	{
		try {
			if ($b) throw new Exception ();

			return 42;
		} catch (Exception $e) {
			return "hello";
		}
	}
}

$f = new Foo();
$a = $f->foo (true);
$b = $f->foo (false);

?>