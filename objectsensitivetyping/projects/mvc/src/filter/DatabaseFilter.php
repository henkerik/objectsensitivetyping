<?php

class DatabaseFilter extends Filter {

    protected $initialized = false;

    /**
     * Initializes a database connnection. 
     *
     * @param Filterchain $filterChain
     * @param Action $action
     * @param Request $request
     * 
     */
    public function execute (FilterChain $filterChain, Action $action, Request $request)
    {
        // Look if there is a database connection
        if (!$this->initialized) {
            
            $databasePool = DatabasePool::getInstance();
            $database = new MysqlDatabase();
            $database->connect(WEBAPP_DATABASE_HOST, WEBAPP_DATABASE_USERNAME, WEBAPP_DATABASE_PASSWORD, WEBAPP_DATABASE_DATABASE);
            $databasePool->register($database, 'default');
            
            $this->initialized = true;
        }

        $filterChain->execute ($action, $request);

    }
}

?>