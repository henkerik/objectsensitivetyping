<?php

/**
 * ClassLoader promotes louse coupling between serveral Loaders
 * 
 */
class ClassLoader {
	
	protected static $instance;

	protected $loaders;
	
	private function ClassLoader () 
	{			
		$this->loaders = new ArrayList ();
	}
	
	public static function getInstance ()
	{
		if (self::$instance == null) 
			self::$instance = new ClassLoader ();
		
		return self::$instance;
	}
	
	public function load ($class)
	{	
		foreach ($this->loaders as $loader) 
		{
			if ($loader->loadable ($class))
				$loader->load ($class);
		}
	}
	
	public function addLoader ($loader)
	{
		$this->loaders->add ($loader);
	}
}

?>