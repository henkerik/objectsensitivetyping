<?php

abstract class Filter {

	protected $context;	

	abstract public function execute (FilterChain $filterChain, Action $action, Request $request);

	/**
	 * Returns a reference to the context
	 *
	 * @return Context
	 */
	protected function getContext ()
	{
		return $this->context;
	}

	public function initialize (Context $context)
	{
		$this->context = $context;
	}
}

?>