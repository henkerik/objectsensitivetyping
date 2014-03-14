<?php

require_once 'mvc/filter/Filter.php';

class ExecutionFilter extends Filter {

	/**
	 * Executes action and the view. This is the last filter in the chain
	 *
	 * @param FilterChain $filterChain
	 * @param Action $action
	 * @param Request $request
	 */
    public function execute (FilterChain $filterChain, Action $action, Request $request)
    {


    	// Look if the request method is right
		if ($action->getRequestMethod () & $request->getRequestMethod ()) {
     		
            if (!$action->validate ($request)) {

                // handle error
                $viewName = $action->handleError ($request);

            } else {

                // No validation errors found. Fire the execute method
                $viewName = $action->execute($request);

            }

        } else {
            // getDefaultView
            $viewName = $action->defaultView($request);
        }

       // echo $viewName; exit;

        /*
            ---------------------------------------------------
            View
            ---------------------------------------------------
        */

        if ($viewName != View::NONE) {
            $moduleName = $action->getModuleName ();
            $actionName = $action->getActionName ();
            
            // Get view
			$controller = $this->getContext()->getController ();
			$view = $controller->getView ($moduleName, $actionName, $viewName);
			
			// Execute view
			$view->execute ($request);
			
			// Render view
            $action->setPresentation ($view->render ($request));
        }
    }
}

?>