<?php


require_once 'mvc/filter/FilterChain.php';
require_once 'mvc/exception/FileNotFoundException.php';

class Controller {

    /**
     * @var FilterChain
     */
    protected $filterChain = null;

    /**
     * @var Context
     */
    protected $context = null;
  
    public function initialize ($context)
    {
        // Save the context
        $this->context = $context;

        // This sets the default filterchain...

        // Make a filterchain
        $this->filterChain = new FilterChain ();
        $this->filterChain->initialize ($context);
    }
    
    public function setFilterChain(FilterChain $filterChain)
    {
        $this->filterChain = $filterChain;   
    }

    protected function getContext ()
    {
        return $this->context;
    }

    /**
     * 
     * @throws MvcException
     */
    public function execute (Request $request)
    {
        // Get the filterChain and execute it.
        $this->filterChain->reset ();
        
		try {
			$action = $this->getAction ($request->getModuleName (), $request->getActionName());
		} catch (FileNotFoundException $e) {
		    try {
		    	//print_r($_SERVER); HE: Ik neem aan dat dit debug code was :)
		    	//print_r($request);
		        $action = $this->getAction (WEBAPP_FILENOTFOUND_MODULE, WEBAPP_FILENOTFOUND_ACTION);
			} catch (FileNotFoundException $ex) {
			
				throw new MvcException ($ex->getMessage ());
			}
		}
        
        $this->filterChain->execute ($action, $request);

		return $action->getPresentation ();
    }


	/**
	 * 
	 * @throws FileNotFoundException
	 * @return Profile
	 */
    function getDecorator ($decoratorName)
    {
        // Look if this profile file exists
        if (is_readable ($file = (NXT_DECORATOR_DIR . $decoratorName . 'Decorator/views/' . $decoratorName . 'Decorator.php'))) {
            include_once ($file);
        } else {
			$message = sprintf('Controller.getDecorator(): The decorator "%s" 
				doesn\'t exist.', $file);
			
			throw new FileNotFoundException ($message);
        }

        $className = $decoratorName . 'Decorator';

        $decorator = new $className(null, null, null);
        $decorator->initialize ($this->getContext());

        return $decorator;
    }

    /**
     * 
     * 
     * @throws FileNotFoundException
     * @return Action
     */
    protected function getAction ($moduleName, $actionName)
    {
        
        
        // Look if the file exists
        $file = NXT_MODULES_DIR . $moduleName . '/' . $actionName . '/' . $actionName . 'Action.php';

        //echo $file; exit;
        
        if (is_readable ($file)) {
            include_once ($file);
        } else {
			
			$message = sprintf('NXT.Controller.Controller.getAction(): The 
				action "%s" in the module "%s" doesn\'t exist.', $actionName, $moduleName);
				

			throw new FileNotFoundException ($message);
        }

                
        $className = $actionName . 'Action';

        // Make an instance of the action class and return it
        $action = new $className($moduleName, $actionName);
        $action->initialize ($this->getContext());
        
        
        return $action;

    }

    /**
     * 
     * @throws FileNotFoundException
     * @return View
     */
    public function getView ($moduleName, $actionName, $viewName)
    {
        // Kijk of de view file leesbaar is
        $file = NXT_MODULES_DIR . $moduleName . '/' . $actionName . '/views/' . $actionName . $viewName . 'View.php';

        if (is_readable ($file)) {
            include_once($file);
        } else {
			$message = sprintf ('NXT.Controller.Controller.getView(): The view 
				"%s" in the module "%s" doesn\'t exist.' . $file, ($actionName . $viewName), $moduleName);
			
			throw new FileNotFoundException ($message);
        }
        
        $className = $actionName . $viewName . 'View';

        $view = new $className($moduleName, $actionName, $viewName);
        $view->initialize ($this->getContext());

        return $view;
    }
    

}


?>