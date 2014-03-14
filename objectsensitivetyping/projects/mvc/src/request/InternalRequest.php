<?php

require_once 'mvc/request/Request.php';
require_once 'mvc/request/WebRequest.php';

class InternalRequest extends Request {

	public function __construct ($moduleName, $actionName)
	{
		parent::__construct ();
        
        $this->setRequestMethod(WebRequest::GET);
		
		$this->moduleName = $moduleName;
		$this->actionName = $actionName;
	}

}

?>