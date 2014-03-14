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

class PostSUA : public virtual AST::Visitor, virtual public GC_obj
{
	void post_node (AST::Node* in)
	{
	    if (in->attrs->is_true("uu.is_doubly_quoted")) {
	        //cout << "Restoring doubly qoutes" << endl;
            in->attrs->set_true ("phc.unparser.is_doubly_quoted");
        }
        
        //if (in->attrs)    
        if (in->attrs->is_true("uu.has_source_rep")) {
            in->attrs->set ("phc.unparser.source_rep", in->attrs->get ("uu.source_rep"));
        }
        
        if (in->attrs->is_true ("uu.needs_user_brackets")) {
            in->attrs->set_true ("phc.unparser.needs_user_brackets");
        }
        //}
	}
};



extern "C" void load (Pass_manager* pm, Plugin_pass* pass)
{
        pm->add_after_named_pass (pass, new String ("sua"));
}

extern "C" void run_ast (AST::PHP_script* in, Pass_manager* pm, String* option)
{
        PostSUA v;
        in->visit(&v);
}