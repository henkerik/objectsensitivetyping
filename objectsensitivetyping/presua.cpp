/*
 * phc -- the open source PHP compiler
 * See doc/license/README.license for licensing information
 *
 * Remove all the phc.unparser attributes from the IR
 */

#include <AST.h>
#include <AST_visitor.h>
#include <pass_manager/Plugin_pass.h>

using namespace AST;

class PreSUA : public virtual AST::Visitor, virtual public GC_obj
{
	void post_node (AST::Node* in)
	{
	    if (in->attrs->is_true("phc.unparser.is_doubly_quoted")) {
	        //cout << "Saving doubly quotes" << endl;
	        
            in->attrs->set_true ("uu.is_doubly_quoted");
        }
        
        if (in->attrs->is_true("phc.unparser.needs_user_brackets")) {
//            cout << "Saving need user brackets" << endl;
            
            in->attrs->set_true ("uu.needs_user_brackets");
        }
    }
    
    void post_string(AST::STRING* in)
    {
        //cout << "Post String..." << endl;
        
        if (dynamic_cast<String*> (in->attrs->get ("phc.unparser.source_rep")) != NULL) {
            in->attrs->set ("uu.source_rep", in->attrs->get ("phc.unparser.source_rep"));
            in->attrs->set_true("uu.has_source_rep");
        }
	}
};



extern "C" void load (Pass_manager* pm, Plugin_pass* pass)
{
        pm->add_after_named_pass (pass, new String ("decomment"));
}

extern "C" void run_ast (AST::PHP_script* in, Pass_manager* pm, String* option)
{
        PreSUA v;
        in->visit(&v);
}