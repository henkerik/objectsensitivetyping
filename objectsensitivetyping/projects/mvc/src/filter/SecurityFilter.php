<?php

require_once 'mvc/filter/Filter.php';
require_once 'mvc/request/InternalRequest.php';

class SecurityFilter extends Filter {

	public function execute (FilterChain $filterChain, Action $action, Request $request)
    {
        $user = $this->getContext ()->getUser ();

        if ($action->isSecure () && !$user->isAuthenticated () && WEBAPP_AUTHENTICATION_USE == true) {

			$controller = $this->getContext()->getController ();            
            $action->setPresentation ($controller->execute (new InternalRequest (WEBAPP_AUTHENTICATION_MODULE, WEBAPP_AUTHENTICATION_ACTION)));

        } else {

            // Continue with this action
        	$filterChain->execute($action, $request);

        }
    }
}

?>