<?php

class ChildException extends Exception {

	public function __construct($sMessage) 
	{
		parent::__construct($sMessage);
	}
	
}

$e = new ChildException("message");

?>