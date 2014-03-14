module Analysis.Resource where
    
import Control.DeepSeq

-- IMPORTANT: If one adds a new resource, also add this resource to the anyResource function in Analysis.Lattice.Resource

data Resource = BZ 
              | CURL
              | CURLMulti
              | DBA
              | FTP
              | GD
              | GDFont -- Created with imagepsloadfont
              | IMAP
              | File         -- Created with fopen, tmpfile, fsockopen
              | ProcessFile  -- Created with popen
              | Directory
              | Context  -- created with stream_context_create
              | Socket   -- Created with socket_create
              | Process  -- Created with proc_open
              | Zip      -- Created with zip_open
              | ZipEntry -- Created with zip_read
              deriving (Show,Eq,Ord)
                            
instance NFData Resource where
    rnf BZ          = ()
    rnf CURL        = ()
    rnf CURLMulti   = ()
    rnf DBA         = ()
    rnf FTP         = ()
    rnf GD          = ()
    rnf GDFont      = ()
    rnf IMAP        = ()
    rnf File        = ()
    rnf ProcessFile = ()
    rnf Directory   = ()
    rnf Context     = ()
    rnf Socket      = ()
    rnf Process     = ()
    rnf Zip         = ()
    rnf ZipEntry    = ()