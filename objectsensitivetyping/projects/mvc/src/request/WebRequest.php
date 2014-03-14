<?php

require_once 'collection/ArrayMap.php';
require_once 'mvc/request/Request.php';
require_once 'mvc/toolkit/Toolkit.php';

class WebRequest extends Request {

    protected $requestMethod;

    const POST = 1;

    const GET = 2;

    const ALL = 3;

    function __construct ()
    {
		parent::__construct ();
		
        // Get the request method
        switch ($_SERVER['REQUEST_METHOD']) {
            case 'GET':
                $this->setRequestMethod (WebRequest::GET);
                break;

            case 'POST':
                $this->setRequestMethod (WebRequest::POST);
                break;

            default:
                throw new MvcException ('The given HTTP request method is not supported.');
                break;
        }


        // Store the POST and the GET parameters
        foreach ($_POST as $key => $value) {
        	$value = (empty($value) && !is_numeric($value)) ? null : Toolkit::stripslashes ($value);
            $this->setAttribute ($key, $value);
        }

        foreach ($_GET as $key => $value) {
            $value = (empty($value) && !is_numeric ($value)) ? null : Toolkit::stripslashes ($value);
            $this->setAttribute ($key, $value);
        }


        // Store the files
        foreach ($_FILES as $key => $value) {
            $this->setAttribute ($key, $value);
        }
        
        
        
        if ($this->hasAttribute ('module')) {
            $this->moduleName = $this->getAttribute ('module');
        } else {
            $this->moduleName = WEBAPP_DEFAULT_MODULE;
        }
        
        if ($this->hasAttribute ('action')) {
            $this->actionName = $this->getAttribute ('action');
        } else {
            $this->actionName = WEBAPP_DEFAULT_ACTION;
        }

    }  
}

?>